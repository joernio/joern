use anyhow::{anyhow, Context, Result};
use serde_json::{json, Map, Value};
use std::collections::HashSet;
use std::path::Path;
use tree_sitter::{Node, Parser};

pub fn parse_go_source(path: &Path, source: &str) -> Result<Value> {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_go::LANGUAGE.into())
        .context("loading tree-sitter-go grammar")?;
    let tree = parser
        .parse(source, None)
        .ok_or_else(|| anyhow!("tree-sitter returned no parse tree"))?;
    let root = tree.root_node();
    if root.has_error() {
        if let Some(point) = first_error_position(root) {
            let location = format!(" at {}:{}", point.row + 1, point.column + 1);
            return Err(anyhow!("parse error{location}"));
        }
    }
    let declared_idents = collect_declared_identifiers(root, source);
    let type_idents = collect_type_identifiers(root, source);
    let imported_idents = collect_imported_identifiers(root, source);
    let mut mapper = Mapper::new(
        path.to_string_lossy().into_owned(),
        source,
        declared_idents,
        type_idents,
        imported_idents,
    );
    Ok(mapper.map_file(root))
}

fn first_error_position(node: Node<'_>) -> Option<tree_sitter::Point> {
    if node.is_error() || node.is_missing() {
        return Some(node.start_position());
    }
    for index in 0..node.child_count() {
        if let Some(child) = node.child(index) {
            if child.has_error() {
                if let Some(point) = first_error_position(child) {
                    return Some(point);
                }
            }
        }
    }
    None
}

struct Mapper<'a> {
    filename: String,
    source: &'a str,
    next_id: u64,
    declared_idents: HashSet<String>,
    type_idents: HashSet<String>,
    imported_idents: HashSet<String>,
    local_decl_scopes: Vec<HashSet<String>>,
    plain_composite_keys: Vec<bool>,
}

impl<'a> Mapper<'a> {
    fn new(
        filename: String,
        source: &'a str,
        declared_idents: HashSet<String>,
        type_idents: HashSet<String>,
        imported_idents: HashSet<String>,
    ) -> Self {
        Self {
            filename,
            source,
            next_id: 1,
            declared_idents,
            type_idents,
            imported_idents,
            local_decl_scopes: Vec::new(),
            plain_composite_keys: Vec::new(),
        }
    }

    fn map_file(&mut self, node: Node<'a>) -> Value {
        let mut decls = Vec::new();
        let mut imports = Vec::new();
        let mut package_name = self.ident_from_text("main", node);
        let mut package_node = None;
        let mut last_decl_end_byte = None;

        for child in self.named_children(node) {
            match child.kind() {
                "package_clause" => {
                    package_node = Some(child);
                    if let Some(name) = self
                        .named_children(child)
                        .into_iter()
                        .find(|n| is_identifier_kind(n.kind()))
                    {
                        package_name = self.map_ident(name);
                    }
                }
                "import_declaration" => {
                    last_decl_end_byte = Some(child.end_byte());
                    let decl = self.map_gen_decl(child, "import");
                    let import_specs = decl
                        .get("Specs")
                        .and_then(Value::as_array)
                        .cloned()
                        .unwrap_or_default();
                    for spec in &import_specs {
                        imports.push(self.import_ref(spec));
                    }
                    decls.push(Value::Object(decl));
                }
                "type_declaration" => {
                    last_decl_end_byte = Some(child.end_byte());
                    decls.push(Value::Object(self.map_gen_decl(child, "type")));
                }
                "var_declaration" => {
                    last_decl_end_byte = Some(child.end_byte());
                    decls.push(Value::Object(self.map_gen_decl(child, "var")));
                }
                "const_declaration" => {
                    last_decl_end_byte = Some(child.end_byte());
                    decls.push(Value::Object(self.map_gen_decl(child, "const")));
                }
                "function_declaration" => {
                    last_decl_end_byte = Some(child.end_byte());
                    decls.push(self.map_function_decl(child, false));
                }
                "method_declaration" => {
                    last_decl_end_byte = Some(child.end_byte());
                    decls.push(self.map_function_decl(child, true));
                }
                _ => {}
            }
        }

        let mut obj = self.base_obj("ast.File", node);
        if let Some(package_node) = package_node {
            self.apply_span(
                &mut obj,
                package_node.start_byte(),
                last_decl_end_byte.unwrap_or_else(|| package_node.end_byte()),
            );
        }
        insert(&mut obj, "Name", package_name);
        insert(
            &mut obj,
            "Decls",
            if decls.is_empty() {
                Value::Null
            } else {
                Value::Array(decls)
            },
        );
        if imports.is_empty() {
            insert(&mut obj, "Imports", Value::Null);
        } else {
            insert(&mut obj, "Imports", Value::Array(imports));
        }
        insert(&mut obj, "Comments", Value::Null);
        insert(&mut obj, "Unresolved", Value::Array(Vec::new()));
        insert(&mut obj, "GoVersion", Value::String(self.go_version()));
        insert(&mut obj, "FileStart", json!(legacy_pos(node.start_byte())));
        insert(&mut obj, "FileEnd", json!(legacy_pos(node.end_byte())));
        insert(
            &mut obj,
            "Package",
            json!(package_node
                .map(|node| legacy_pos(node.start_byte()))
                .unwrap_or_else(|| self.keyword_position(node, "package").unwrap_or(0))),
        );
        insert(&mut obj, "Scope", Value::Null);
        Value::Object(obj)
    }

    fn map_gen_decl(&mut self, node: Node<'a>, tok: &str) -> Map<String, Value> {
        let spec_kind = match tok {
            "import" => "import_spec",
            "type" => "type_spec",
            "const" => "const_spec",
            _ => "var_spec",
        };
        let specs = self
            .declaration_specs(node, spec_kind)
            .into_iter()
            .map(|child| match spec_kind {
                "import_spec" => self.map_import_spec(child),
                "type_spec" => self.map_type_spec(child),
                _ => self.map_value_spec(child),
            })
            .collect();

        let mut obj = self.base_obj("ast.GenDecl", node);
        let (lparen, rparen) = self.gen_decl_paren_positions(node, tok);
        insert(&mut obj, "Tok", Value::String(tok.into()));
        insert(&mut obj, "TokPos", json!(legacy_pos(node.start_byte())));
        insert(&mut obj, "Lparen", json!(lparen));
        insert(&mut obj, "Rparen", json!(rparen));
        insert(&mut obj, "Specs", Value::Array(specs));
        obj
    }

    fn declaration_specs(&self, node: Node<'a>, spec_kind: &str) -> Vec<Node<'a>> {
        let mut specs = Vec::new();
        for child in self.named_children(node) {
            if child.kind() == spec_kind {
                specs.push(child);
            } else if child.kind().ends_with("_spec_list") {
                specs.extend(
                    self.named_children(child)
                        .into_iter()
                        .filter(|nested| nested.kind() == spec_kind),
                );
            }
        }
        specs
    }

    fn map_import_spec(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ImportSpec", node);
        insert(&mut obj, "EndPos", json!(0));
        if let Some(path) = self
            .named_children(node)
            .into_iter()
            .find(|child| is_string_literal_kind(child.kind()))
        {
            insert(&mut obj, "Path", self.map_basic_lit(path));
        }
        if let Some(name) = self.named_children(node).into_iter().find(|child| {
            child.kind() == "package_identifier" || child.kind() == "dot" || child.kind() == "_"
        }) {
            insert(&mut obj, "Name", self.map_ident(name));
        }
        Value::Object(obj)
    }

    fn map_type_spec(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.TypeSpec", node);
        if let Some(name) = self
            .field_child(node, "name")
            .or_else(|| self.first_identifier(node))
        {
            insert(&mut obj, "Name", self.map_decl_ident(name));
        }
        let mut pushed_type_param_scope = false;
        if let Some(type_params) = self.field_child(node, "type_parameters") {
            self.local_decl_scopes
                .push(self.declared_names_in(type_params));
            pushed_type_param_scope = true;
            insert(&mut obj, "TypeParams", self.map_field_list(type_params));
        }
        if let Some(type_node) = self.field_child(node, "type").or_else(|| {
            self.named_children(node)
                .into_iter()
                .rev()
                .find(|n| !is_identifier_kind(n.kind()))
        }) {
            insert(&mut obj, "Type", self.map_type_or_expr(type_node));
        }
        if pushed_type_param_scope {
            self.local_decl_scopes.pop();
        }
        insert(&mut obj, "Assign", json!(0));
        Value::Object(obj)
    }

    fn map_value_spec(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ValueSpec", node);
        let names = self.field_children(node, "name");
        let values = self.flatten_expr_list(self.field_children(node, "value"));
        let type_node = self.field_child(node, "type");

        let fallback_names = if names.is_empty() {
            self.named_children(node)
                .into_iter()
                .take_while(|n| is_identifier_kind(n.kind()))
                .collect()
        } else {
            names
        };
        let scope_names = fallback_names.clone();

        insert(
            &mut obj,
            "Names",
            Value::Array(
                fallback_names
                    .into_iter()
                    .map(|n| self.map_decl_ident(n))
                    .collect(),
            ),
        );
        if let Some(type_node) = type_node {
            insert(&mut obj, "Type", self.map_type_or_expr(type_node));
        }
        if !values.is_empty() {
            insert(
                &mut obj,
                "Values",
                Value::Array(
                    values
                        .into_iter()
                        .map(|n| self.map_type_or_expr(n))
                        .collect(),
                ),
            );
        } else {
            insert(&mut obj, "Values", Value::Null);
        }
        self.add_local_declared_nodes(&scope_names);
        Value::Object(obj)
    }

    fn map_function_decl(&mut self, node: Node<'a>, is_method: bool) -> Value {
        let mut obj = self.base_obj("ast.FuncDecl", node);
        if let Some(name) = self
            .field_child(node, "name")
            .or_else(|| self.first_identifier(node))
        {
            let mapped_name = if is_method || self.text(name).trim() == "init" {
                self.map_ident(name)
            } else {
                self.map_decl_ident(name)
            };
            insert(&mut obj, "Name", mapped_name);
        }
        insert(&mut obj, "Type", self.map_func_type(node));
        if is_method {
            if let Some(receiver) = self.field_child(node, "receiver") {
                let mut recv = self.map_field_list(receiver);
                remove_obj_from_index_fields(&mut recv);
                insert(&mut obj, "Recv", recv);
            }
        }
        if let Some(body) = self
            .field_child(node, "body")
            .or_else(|| self.child_kind(node, "block"))
        {
            let scope_names = self.callable_scope_names(node, is_method);
            self.local_decl_scopes.push(scope_names);
            insert(&mut obj, "Body", self.map_block_stmt(body));
            self.local_decl_scopes.pop();
        }
        Value::Object(obj)
    }

    fn map_func_type(&mut self, node: Node<'a>) -> Value {
        let target = self.child_kind(node, "function_type").unwrap_or(node);
        let mut obj = self.base_obj("ast.FuncType", target);
        let (span_start, span_end) = self.func_type_span(node, target);
        self.apply_span(&mut obj, span_start, span_end);
        insert(
            &mut obj,
            "Func",
            json!(self
                .direct_keyword_position(node, &["func"])
                .or_else(|| self.direct_keyword_position(target, &["func"]))
                .unwrap_or(0)),
        );
        let mut pushed_type_param_scope = false;
        if let Some(type_params) = self.field_child(node, "type_parameters") {
            self.local_decl_scopes
                .push(self.declared_names_in(type_params));
            pushed_type_param_scope = true;
            insert(&mut obj, "TypeParams", self.map_field_list(type_params));
        }
        if let Some(params) = self
            .field_child(node, "parameters")
            .or_else(|| self.child_kind(target, "parameter_list"))
        {
            insert(&mut obj, "Params", self.map_field_list(params));
        } else {
            insert(&mut obj, "Params", self.empty_field_list(target));
        }
        if let Some(result) = self.field_child(node, "result") {
            insert(&mut obj, "Results", self.map_result_list(result));
        }
        if pushed_type_param_scope {
            self.local_decl_scopes.pop();
        }
        Value::Object(obj)
    }

    fn map_result_list(&mut self, node: Node<'a>) -> Value {
        if node.kind() == "parameter_list" {
            self.map_field_list(node)
        } else {
            let mut list = self.base_obj("ast.FieldList", node);
            let mut field = self.base_obj("ast.Field", node);
            insert(&mut field, "Names", Value::Null);
            insert(&mut field, "Type", self.map_type_or_expr(node));
            insert(&mut list, "Opening", json!(0));
            insert(&mut list, "Closing", json!(0));
            insert(&mut list, "List", Value::Array(vec![Value::Object(field)]));
            Value::Object(list)
        }
    }

    fn map_field_list(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.FieldList", node);
        let (opening, closing) = self.delimiter_positions(node);
        if opening > 0 && closing > 0 {
            self.apply_span(&mut obj, opening - 1, closing);
        }
        insert(&mut obj, "Opening", json!(opening));
        insert(&mut obj, "Closing", json!(closing));
        let fields: Vec<Value> = self
            .named_children(node)
            .into_iter()
            .filter_map(|child| match child.kind() {
                "parameter_declaration"
                | "field_declaration"
                | "method_elem"
                | "constraint_elem"
                | "type_parameter_declaration" => Some(self.map_field(child)),
                "type_elem" => Some(self.map_embedded_field(child)),
                "variadic_parameter_declaration" => Some(self.map_field(child)),
                _ if is_type_kind(child.kind()) || is_identifier_kind(child.kind()) => {
                    Some(self.map_embedded_field(child))
                }
                _ => None,
            })
            .collect();
        if fields.is_empty() {
            insert(&mut obj, "List", Value::Null);
        } else {
            insert(&mut obj, "List", Value::Array(fields));
        }
        Value::Object(obj)
    }

    fn empty_field_list(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.FieldList", node);
        insert(&mut obj, "Opening", json!(0));
        insert(&mut obj, "Closing", json!(0));
        insert(&mut obj, "List", Value::Array(Vec::new()));
        Value::Object(obj)
    }

    fn map_field(&mut self, node: Node<'a>) -> Value {
        if node.kind() == "method_elem" {
            return self.map_method_elem_field(node);
        }
        let mut obj = self.base_obj("ast.Field", node);
        let names = self.field_children(node, "name");
        if !names.is_empty() {
            insert(
                &mut obj,
                "Names",
                Value::Array(
                    names
                        .into_iter()
                        .map(|name| self.map_decl_ident(name))
                        .collect(),
                ),
            );
        } else {
            insert(&mut obj, "Names", Value::Null);
        }
        if let Some(type_node) = self.field_child(node, "type").or_else(|| {
            self.named_children(node)
                .into_iter()
                .rev()
                .find(|n| !is_identifier_kind(n.kind()))
        }) {
            let mapped_type = if node.kind() == "variadic_parameter_declaration" {
                self.map_ellipsis_type(node, type_node)
            } else {
                self.map_type_or_expr(type_node)
            };
            insert(&mut obj, "Type", mapped_type);
        }
        if let Some(tag) = self.field_child(node, "tag") {
            insert(&mut obj, "Tag", self.map_basic_lit(tag));
        }
        Value::Object(obj)
    }

    fn map_method_elem_field(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.Field", node);
        if let Some(name) = self.field_child(node, "name") {
            insert(
                &mut obj,
                "Names",
                Value::Array(vec![self.map_decl_ident(name)]),
            );
        } else {
            insert(&mut obj, "Names", Value::Null);
        }
        insert(&mut obj, "Type", self.map_func_type(node));
        Value::Object(obj)
    }

    fn map_embedded_field(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.Field", node);
        insert(&mut obj, "Names", Value::Null);
        insert(&mut obj, "Type", self.map_type_or_expr(node));
        Value::Object(obj)
    }

    fn map_block_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.BlockStmt", node);
        self.insert_block_braces(&mut obj, node);
        let statement_parent = self.child_kind(node, "statement_list").unwrap_or(node);
        self.local_decl_scopes.push(HashSet::new());
        let list = self
            .named_children(statement_parent)
            .into_iter()
            .filter_map(|child| self.map_statement(child))
            .collect::<Vec<_>>();
        self.local_decl_scopes.pop();
        insert(
            &mut obj,
            "List",
            if list.is_empty() {
                Value::Null
            } else {
                Value::Array(list)
            },
        );
        Value::Object(obj)
    }

    fn map_statement(&mut self, node: Node<'a>) -> Option<Value> {
        match node.kind() {
            "block" => Some(self.map_block_stmt(node)),
            "statement_list" => Some(self.map_block_stmt(node)),
            "declaration" => self
                .named_children(node)
                .into_iter()
                .next()
                .map(|decl| self.map_decl_stmt(node, decl)),
            "type_declaration" | "var_declaration" | "const_declaration" => {
                Some(self.map_decl_stmt(node, node))
            }
            "short_var_declaration" | "assignment_statement" => Some(self.map_assign_stmt(node)),
            "expression_statement" => self.named_children(node).into_iter().next().map(|expr| {
                let value = self.map_type_or_expr(expr);
                self.wrap_one("ast.ExprStmt", node, "X", value)
            }),
            "return_statement" => Some(self.map_return_stmt(node)),
            "if_statement" => Some(self.map_if_stmt(node)),
            "for_statement" => Some(self.map_for_stmt(node)),
            "range_clause" => Some(self.map_range_stmt(node, None, None)),
            "switch_statement" | "expression_switch_statement" => {
                Some(self.map_switch_stmt(node, false))
            }
            "type_switch_statement" => Some(self.map_switch_stmt(node, true)),
            "select_statement" => Some(self.map_select_stmt(node)),
            "communication_case" | "default_case" => Some(self.map_comm_clause(node)),
            "expression_case" => Some(self.map_case_clause(node)),
            "go_statement" => self.map_call_statement("ast.GoStmt", node),
            "defer_statement" => self.map_call_statement("ast.DeferStmt", node),
            "send_statement" => Some(self.map_send_stmt(node)),
            "receive_statement" => Some(self.map_receive_stmt(node)),
            "inc_statement" => Some(self.map_inc_dec_stmt(node, "++")),
            "dec_statement" => Some(self.map_inc_dec_stmt(node, "--")),
            "branch_statement"
            | "break_statement"
            | "continue_statement"
            | "goto_statement"
            | "fallthrough_statement" => Some(self.map_branch_stmt(node)),
            "labeled_statement" => Some(self.map_labeled_stmt(node)),
            _ if is_expression_kind(node.kind()) => {
                let value = self.map_type_or_expr(node);
                Some(self.wrap_one("ast.ExprStmt", node, "X", value))
            }
            _ => None,
        }
    }

    fn map_decl_stmt(&mut self, node: Node<'a>, decl: Node<'a>) -> Value {
        let value = match decl.kind() {
            "type_declaration" => Value::Object(self.map_gen_decl(decl, "type")),
            "var_declaration" => Value::Object(self.map_gen_decl(decl, "var")),
            "const_declaration" => Value::Object(self.map_gen_decl(decl, "const")),
            _ => self.unknown(decl),
        };
        if decl.kind() == "type_declaration" {
            let mut names = HashSet::new();
            collect_top_level_spec_identifiers(decl, "type_spec", self.source, &mut names);
            self.add_local_declared_names(names);
        }
        self.wrap_one("ast.DeclStmt", node, "Decl", value)
    }

    fn map_assign_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.AssignStmt", node);
        let lhs = self.flatten_expr_list(self.field_children(node, "left"));
        let rhs = self.flatten_expr_list(self.field_children(node, "right"));
        let named = self.named_children(node);
        let is_short_var = node.kind() == "short_var_declaration";
        let lhs_for_scope = lhs.clone();
        let op = self.operator_token(node).unwrap_or_else(|| {
            if is_short_var {
                ":=".into()
            } else {
                "=".into()
            }
        });
        insert(&mut obj, "TokPos", json!(self.operator_position(node, &op)));
        insert(&mut obj, "Tok", Value::String(op));
        insert(
            &mut obj,
            "Lhs",
            Value::Array(if lhs.is_empty() {
                named
                    .first()
                    .map(|n| self.map_type_or_expr(*n))
                    .into_iter()
                    .collect::<Vec<_>>()
            } else if is_short_var {
                lhs.into_iter()
                    .map(|n| {
                        if is_identifier_kind(n.kind()) {
                            self.map_decl_ident(n)
                        } else {
                            self.map_type_or_expr(n)
                        }
                    })
                    .collect()
            } else {
                lhs.into_iter().map(|n| self.map_type_or_expr(n)).collect()
            }),
        );
        insert(
            &mut obj,
            "Rhs",
            Value::Array(if rhs.is_empty() {
                named
                    .into_iter()
                    .skip(1)
                    .map(|n| self.map_type_or_expr(n))
                    .collect()
            } else {
                rhs.into_iter().map(|n| self.map_type_or_expr(n)).collect()
            }),
        );
        if is_short_var {
            self.add_local_declared_nodes(&lhs_for_scope);
        }
        Value::Object(obj)
    }

    fn map_return_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ReturnStmt", node);
        let result_nodes = self.flatten_expr_list(self.named_children(node));
        let results = result_nodes
            .into_iter()
            .map(|n| self.map_type_or_expr(n))
            .collect::<Vec<_>>();
        insert(&mut obj, "Return", json!(legacy_pos(node.start_byte())));
        insert(
            &mut obj,
            "Results",
            if results.is_empty() {
                Value::Null
            } else {
                Value::Array(results)
            },
        );
        Value::Object(obj)
    }

    fn map_if_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.IfStmt", node);
        self.local_decl_scopes.push(HashSet::new());
        insert(&mut obj, "If", json!(legacy_pos(node.start_byte())));
        if let Some(init) = self.field_child(node, "initializer") {
            if let Some(init) = self.map_statement(init) {
                insert(&mut obj, "Init", init);
            }
        }
        if let Some(cond) = self.field_child(node, "condition") {
            insert(&mut obj, "Cond", self.map_type_or_expr(cond));
        }
        if let Some(body) = self
            .field_child(node, "consequence")
            .or_else(|| self.child_kind(node, "block"))
        {
            insert(&mut obj, "Body", self.map_block_stmt(body));
        }
        if let Some(alt) = self.field_child(node, "alternative") {
            let else_value = if alt.kind() == "block" {
                self.map_block_stmt(alt)
            } else {
                self.map_statement(alt).unwrap_or_else(|| self.unknown(alt))
            };
            insert(&mut obj, "Else", else_value);
        }
        self.local_decl_scopes.pop();
        Value::Object(obj)
    }

    fn map_for_stmt(&mut self, node: Node<'a>) -> Value {
        if let Some(range) = self.child_kind(node, "range_clause") {
            return self.map_range_stmt(range, self.child_kind(node, "block"), Some(node));
        }
        let mut obj = self.base_obj("ast.ForStmt", node);
        self.local_decl_scopes.push(HashSet::new());
        insert(
            &mut obj,
            "For",
            json!(self.keyword_position(node, "for").unwrap_or(0)),
        );
        if let Some(for_clause) = self.child_kind(node, "for_clause") {
            if let Some(init) = self.field_child(for_clause, "initializer") {
                if let Some(init) = self.map_statement(init) {
                    insert(&mut obj, "Init", init);
                }
            }
            if let Some(cond) = self.field_child(for_clause, "condition") {
                insert(&mut obj, "Cond", self.map_type_or_expr(cond));
            }
            if let Some(update) = self.field_child(for_clause, "update") {
                if let Some(update) = self.map_statement(update) {
                    insert(&mut obj, "Post", update);
                }
            }
        } else if let Some(cond) = self
            .named_children(node)
            .into_iter()
            .find(|child| child.kind() != "block")
        {
            insert(&mut obj, "Cond", self.map_type_or_expr(cond));
        }
        if let Some(body) = self
            .field_child(node, "body")
            .or_else(|| self.child_kind(node, "block"))
        {
            insert(&mut obj, "Body", self.map_block_stmt(body));
        }
        self.local_decl_scopes.pop();
        Value::Object(obj)
    }

    fn map_range_stmt(
        &mut self,
        node: Node<'a>,
        body: Option<Node<'a>>,
        for_node: Option<Node<'a>>,
    ) -> Value {
        let span_node = for_node.unwrap_or(node);
        let mut obj = self.base_obj("ast.RangeStmt", span_node);
        self.local_decl_scopes.push(HashSet::new());
        insert(
            &mut obj,
            "For",
            json!(for_node
                .and_then(|node| self.keyword_position(node, "for"))
                .unwrap_or(0)),
        );
        insert(
            &mut obj,
            "Range",
            json!(self.keyword_position(node, "range").unwrap_or(0)),
        );
        let left = self.flatten_expr_list(self.field_children(node, "left"));
        let right = self
            .field_child(node, "right")
            .or_else(|| self.named_children(node).into_iter().last());
        let right_value = right.map(|right| self.map_type_or_expr(right));
        let left_values: Vec<Value> = left.iter().map(|n| self.map_type_or_expr(*n)).collect();
        let range_decl = if left_values.is_empty() {
            None
        } else {
            let mut decl = self.base_obj("ast.AssignStmt", node);
            let decl_op = self.operator_token(node).unwrap_or_else(|| ":=".into());
            insert(
                &mut decl,
                "TokPos",
                json!(self.operator_position(node, &decl_op)),
            );
            insert(&mut decl, "Tok", Value::String(decl_op));
            insert(&mut decl, "Lhs", Value::Array(left_values.clone()));
            if let Some(value) = right_value.clone() {
                insert(&mut decl, "Rhs", Value::Array(vec![value]));
            }
            Some(Value::Object(decl))
        };

        if let Some(key) = left_values.first() {
            insert(
                &mut obj,
                "Key",
                with_decl_object(key.clone(), range_decl.as_ref()),
            );
        }
        if let Some(value) = left_values.get(1) {
            insert(
                &mut obj,
                "Value",
                self.reference_stub(value)
                    .unwrap_or_else(|| with_decl_object(value.clone(), range_decl.as_ref())),
            );
        }
        if let Some(right_value) = right_value {
            let x = if left_values.is_empty() {
                right_value
            } else {
                self.reference_stub(&right_value).unwrap_or(right_value)
            };
            insert(&mut obj, "X", x);
        }
        let op = if left_values.is_empty() {
            "ILLEGAL".into()
        } else {
            self.operator_token(node).unwrap_or_else(|| ":=".into())
        };
        insert(&mut obj, "TokPos", json!(self.operator_position(node, &op)));
        if op == ":=" {
            self.add_local_declared_nodes(&left);
        }
        insert(&mut obj, "Tok", Value::String(op));
        if let Some(body) = body {
            insert(&mut obj, "Body", self.map_block_stmt(body));
        }
        self.local_decl_scopes.pop();
        Value::Object(obj)
    }

    fn map_switch_stmt(&mut self, node: Node<'a>, type_switch: bool) -> Value {
        let mut obj = self.base_obj(
            if type_switch {
                "ast.TypeSwitchStmt"
            } else {
                "ast.SwitchStmt"
            },
            node,
        );
        self.local_decl_scopes.push(HashSet::new());
        insert(
            &mut obj,
            "Switch",
            json!(self.keyword_position(node, "switch").unwrap_or(0)),
        );
        if type_switch {
            if let Some(value) = self.field_child(node, "value") {
                insert(&mut obj, "Assign", self.map_type_switch_assign(node, value));
            }
            for alias in self.field_children(node, "alias") {
                self.add_local_declared_nodes(&[alias]);
            }
        } else if let Some(value) = self
            .field_child(node, "value")
            .or_else(|| self.field_child(node, "condition"))
        {
            insert(&mut obj, "Tag", self.map_type_or_expr(value));
        }
        let cases = self
            .named_children(node)
            .into_iter()
            .filter_map(|child| match child.kind() {
                "expression_case" => Some(self.map_case_clause(child)),
                "type_case" => Some(self.map_case_clause(child)),
                "default_case" => Some(self.map_case_clause(child)),
                _ => None,
            })
            .collect();
        let mut body = self.base_obj("ast.BlockStmt", node);
        let (lbrace, rbrace) = self.insert_block_braces(&mut body, node);
        self.apply_brace_span(&mut body, lbrace, rbrace);
        insert(&mut body, "List", Value::Array(cases));
        insert(&mut obj, "Body", Value::Object(body));
        self.local_decl_scopes.pop();
        Value::Object(obj)
    }

    fn map_type_switch_assign(&mut self, node: Node<'a>, value: Node<'a>) -> Value {
        let (_, condition_end) = self.type_switch_condition_span(node, None, value);
        let type_assert = self.map_type_switch_assert(node, value, condition_end);
        if let Some(alias) = self.field_child(node, "alias") {
            let (condition_start, condition_end) =
                self.type_switch_condition_span(node, Some(alias), value);
            let lhs = vec![self.map_decl_ident(alias)];
            let mut obj = self.base_obj("ast.AssignStmt", node);
            self.apply_span(&mut obj, condition_start, condition_end);
            let op = self.operator_token(node).unwrap_or_else(|| ":=".into());
            insert(&mut obj, "TokPos", json!(self.operator_position(node, &op)));
            insert(&mut obj, "Tok", Value::String(op));
            insert(&mut obj, "Lhs", Value::Array(lhs));
            insert(&mut obj, "Rhs", Value::Array(vec![type_assert]));
            Value::Object(obj)
        } else {
            type_assert
        }
    }

    fn map_type_switch_assert(
        &mut self,
        source: Node<'a>,
        value: Node<'a>,
        end_byte: usize,
    ) -> Value {
        let mut obj = self.base_obj("ast.TypeAssertExpr", value);
        self.apply_span(&mut obj, value.start_byte(), end_byte);
        let (lparen, rparen) = self.type_assert_paren_positions_in(source.start_byte(), end_byte);
        insert(&mut obj, "Lparen", json!(lparen));
        insert(&mut obj, "Rparen", json!(rparen));
        insert(&mut obj, "X", self.map_type_or_expr(value));
        Value::Object(obj)
    }

    fn map_case_clause(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CaseClause", node);
        insert(&mut obj, "Case", json!(self.clause_keyword_position(node)));
        insert(
            &mut obj,
            "Colon",
            json!(self.keyword_position(node, ":").unwrap_or(0)),
        );
        let value_field = if node.kind() == "type_case" {
            "type"
        } else {
            "value"
        };
        let values = self.flatten_expr_list(self.field_children(node, value_field));
        let value_nodes = values.clone();
        if !values.is_empty() {
            insert(
                &mut obj,
                "List",
                Value::Array(
                    values
                        .into_iter()
                        .map(|n| self.map_type_or_expr(n))
                        .collect(),
                ),
            );
        } else {
            insert(&mut obj, "List", Value::Null);
        }
        let body_nodes: Vec<_> = self
            .flatten_statement_lists(self.named_children(node))
            .into_iter()
            .filter(|n| {
                !matches!(n.kind(), "expression_list" | "type_list")
                    && !value_nodes
                        .iter()
                        .any(|value| same_tree_sitter_node(*n, *value))
            })
            .collect();
        self.apply_span(
            &mut obj,
            node.start_byte(),
            self.clause_end_byte(node, &body_nodes),
        );
        let body = body_nodes
            .into_iter()
            .filter_map(|n| self.map_statement(n))
            .collect::<Vec<_>>();
        insert(
            &mut obj,
            "Body",
            if body.is_empty() {
                Value::Null
            } else {
                Value::Array(body)
            },
        );
        Value::Object(obj)
    }

    fn map_select_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.SelectStmt", node);
        insert(
            &mut obj,
            "Select",
            json!(self.keyword_position(node, "select").unwrap_or(0)),
        );
        let clauses = self
            .descendants_named(node)
            .into_iter()
            .filter(|n| n.kind() == "communication_case" || n.kind() == "default_case")
            .map(|n| self.map_comm_clause(n))
            .collect();
        let mut body = self.base_obj("ast.BlockStmt", node);
        let (lbrace, rbrace) = self.insert_block_braces(&mut body, node);
        self.apply_brace_span(&mut body, lbrace, rbrace);
        insert(&mut body, "List", Value::Array(clauses));
        insert(&mut obj, "Body", Value::Object(body));
        Value::Object(obj)
    }

    fn map_comm_clause(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CommClause", node);
        insert(&mut obj, "Case", json!(self.clause_keyword_position(node)));
        insert(
            &mut obj,
            "Colon",
            json!(self.keyword_position(node, ":").unwrap_or(0)),
        );
        let comm_node = self.field_child(node, "communication").or_else(|| {
            self.named_children(node).into_iter().find(|n| {
                matches!(
                    n.kind(),
                    "send_statement"
                        | "receive_statement"
                        | "assignment_statement"
                        | "short_var_declaration"
                )
            })
        });
        if let Some(comm) = comm_node {
            if comm.kind() == "receive_statement" {
                if let Some(expr) = self.named_children(comm).into_iter().next() {
                    let value = self.map_type_or_expr(expr);
                    insert(
                        &mut obj,
                        "Comm",
                        self.wrap_one("ast.ExprStmt", comm, "X", value),
                    );
                }
            } else if let Some(stmt) = self.map_statement(comm) {
                insert(&mut obj, "Comm", stmt);
            }
        }
        let body_nodes: Vec<_> = self
            .flatten_statement_lists(self.named_children(node))
            .into_iter()
            .filter(|n| {
                comm_node
                    .map(|comm| !same_tree_sitter_node(*n, comm))
                    .unwrap_or(true)
            })
            .collect();
        self.apply_span(
            &mut obj,
            node.start_byte(),
            self.clause_end_byte(node, &body_nodes),
        );
        let body: Vec<_> = body_nodes
            .into_iter()
            .filter_map(|n| self.map_statement(n))
            .collect();
        insert(
            &mut obj,
            "Body",
            if body.is_empty() {
                Value::Null
            } else {
                Value::Array(body)
            },
        );
        Value::Object(obj)
    }

    fn map_call_statement(&mut self, kind: &str, node: Node<'a>) -> Option<Value> {
        self.descendants_named(node)
            .into_iter()
            .find(|n| n.kind() == "call_expression")
            .map(|call| {
                let value = self.map_call_expr(call);
                let mut obj = self.base_obj(kind, node);
                match kind {
                    "ast.DeferStmt" => {
                        insert(&mut obj, "Defer", json!(legacy_pos(node.start_byte())))
                    }
                    "ast.GoStmt" => insert(&mut obj, "Go", json!(legacy_pos(node.start_byte()))),
                    _ => {}
                }
                insert(&mut obj, "Call", value);
                Value::Object(obj)
            })
    }

    fn map_send_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.SendStmt", node);
        insert(&mut obj, "Arrow", json!(self.operator_position(node, "<-")));
        if let Some(chan) = self
            .field_child(node, "channel")
            .or_else(|| self.named_children(node).into_iter().next())
        {
            insert(&mut obj, "Chan", self.map_type_or_expr(chan));
        }
        if let Some(value) = self
            .field_child(node, "value")
            .or_else(|| self.named_children(node).into_iter().nth(1))
        {
            insert(&mut obj, "Value", self.map_type_or_expr(value));
        }
        Value::Object(obj)
    }

    fn map_receive_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.UnaryExpr", node);
        if let Some(right) = self
            .field_child(node, "right")
            .or_else(|| self.named_children(node).into_iter().last())
        {
            insert(&mut obj, "X", self.map_type_or_expr(right));
        }
        insert(&mut obj, "Op", Value::String("<-".into()));
        Value::Object(obj)
    }

    fn map_inc_dec_stmt(&mut self, node: Node<'a>, tok: &str) -> Value {
        let mut obj = self.base_obj("ast.IncDecStmt", node);
        if let Some(x) = self.named_children(node).into_iter().next() {
            insert(&mut obj, "X", self.map_type_or_expr(x));
        }
        insert(&mut obj, "TokPos", json!(self.operator_position(node, tok)));
        insert(&mut obj, "Tok", Value::String(tok.into()));
        Value::Object(obj)
    }

    fn map_branch_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.BranchStmt", node);
        let tok = self
            .text(node)
            .split_whitespace()
            .next()
            .unwrap_or("")
            .to_string();
        insert(&mut obj, "TokPos", json!(legacy_pos(node.start_byte())));
        insert(&mut obj, "Tok", Value::String(tok));
        if let Some(label) = self.first_identifier(node) {
            insert(&mut obj, "Label", self.map_decl_ident(label));
        }
        Value::Object(obj)
    }

    fn map_labeled_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.LabeledStmt", node);
        if let Some(label) = self.first_identifier(node) {
            insert(&mut obj, "Label", self.map_decl_ident(label));
        }
        insert(
            &mut obj,
            "Colon",
            json!(self.keyword_position(node, ":").unwrap_or(0)),
        );
        if let Some(stmt) = self
            .named_children(node)
            .into_iter()
            .find_map(|n| self.map_statement(n))
        {
            insert(&mut obj, "Stmt", stmt);
        }
        Value::Object(obj)
    }

    fn map_type_or_expr(&mut self, node: Node<'a>) -> Value {
        match node.kind() {
            kind if is_identifier_kind(kind) => self.map_ident_ref(node),
            kind if is_string_literal_kind(kind)
                || matches!(
                    kind,
                    "int_literal" | "float_literal" | "imaginary_literal" | "rune_literal"
                ) =>
            {
                self.map_basic_lit(node)
            }
            "binary_expression" => self.map_binary_expr(node),
            "unary_expression" if self.operator_token(node).as_deref() == Some("*") => {
                self.wrap_child_type("ast.StarExpr", node, "X")
            }
            "unary_expression" => self.map_unary_expr(node),
            "parenthesized_expression" | "parenthesized_type" => self.map_paren_expr(node),
            "selector_expression" | "qualified_type" => self.map_selector_expr(node),
            "call_expression" => self.map_call_expr(node),
            "type_conversion_expression" => self.map_type_conversion_expr(node),
            "index_expression" => self.map_index_expr(node),
            "slice_expression" => self.map_slice_expr(node),
            "type_instantiation_expression" | "generic_type" => self.map_type_instantiation(node),
            "type_assertion_expression" => self.map_type_assert_expr(node),
            "pointer_type" => self.wrap_child_type("ast.StarExpr", node, "X"),
            "array_type" | "slice_type" | "implicit_length_array_type" => self.map_array_type(node),
            "map_type" => self.map_map_type(node),
            "channel_type" => self.map_chan_type(node),
            "struct_type" => self.map_struct_type(node),
            "interface_type" => self.map_interface_type(node),
            "function_type" => self.map_func_type(node),
            "func_literal" | "function_literal" => self.map_func_lit(node),
            "composite_literal" => self.map_composite_lit(node),
            "literal_value" => self.map_literal_value(node),
            "type_elem" | "type_constraint" => self.map_type_union(node),
            "keyed_element" | "literal_element" | "expression_list" => {
                self.map_key_value_or_element(node)
            }
            _ => {
                if let Some(child) = self.named_children(node).into_iter().next() {
                    self.map_type_or_expr(child)
                } else {
                    self.unknown(node)
                }
            }
        }
    }

    fn map_binary_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.BinaryExpr", node);
        let children = self.named_children(node);
        if let Some(left) = self
            .field_child(node, "left")
            .or_else(|| children.first().copied())
        {
            insert(&mut obj, "X", self.map_type_or_expr(left));
        }
        if let Some(right) = self
            .field_child(node, "right")
            .or_else(|| children.get(1).copied())
        {
            insert(&mut obj, "Y", self.map_type_or_expr(right));
        }
        let op = self.operator_token(node).unwrap_or_default();
        insert(&mut obj, "OpPos", json!(self.operator_position(node, &op)));
        insert(&mut obj, "Op", Value::String(op));
        Value::Object(obj)
    }

    fn map_unary_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.UnaryExpr", node);
        if let Some(child) = self
            .named_children(node)
            .into_iter()
            .find(|child| child.kind() != "comment")
        {
            insert(&mut obj, "X", self.map_type_or_expr(child));
        }
        let op = self.operator_token(node).unwrap_or_default();
        insert(&mut obj, "OpPos", json!(self.operator_position(node, &op)));
        insert(&mut obj, "Op", Value::String(op));
        Value::Object(obj)
    }

    fn map_paren_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ParenExpr", node);
        let (lparen, rparen) = self.paren_positions(node);
        insert(&mut obj, "Lparen", json!(lparen));
        insert(&mut obj, "Rparen", json!(rparen));
        if let Some(child) = self
            .named_children(node)
            .into_iter()
            .find(|child| child.kind() != "comment")
        {
            insert(&mut obj, "X", self.map_type_or_expr(child));
        }
        Value::Object(obj)
    }

    fn map_selector_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.SelectorExpr", node);
        let children = self.named_children(node);
        if let Some(x) = self
            .field_child(node, "operand")
            .or_else(|| children.first().copied())
        {
            let receiver = if is_identifier_kind(x.kind()) {
                let name = self.text(x).trim().to_string();
                if self.imported_idents.contains(&name) && !self.is_locally_declared(&name) {
                    self.map_ident(x)
                } else {
                    self.map_type_or_expr(x)
                }
            } else {
                self.map_type_or_expr(x)
            };
            insert(&mut obj, "X", receiver);
        }
        if let Some(sel) = self
            .field_child(node, "field")
            .or_else(|| children.last().copied())
        {
            insert(&mut obj, "Sel", self.map_ident(sel));
        }
        Value::Object(obj)
    }

    fn map_call_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CallExpr", node);
        if let Some(function) = self
            .field_child(node, "function")
            .or_else(|| self.named_children(node).into_iter().next())
        {
            let fun = if let Some(type_args) = self.field_child(node, "type_arguments") {
                self.map_index_like(function, type_args, node)
            } else {
                self.map_type_or_expr(function)
            };
            insert(&mut obj, "Fun", fun);
        }
        let arg_list = self.child_kind(node, "argument_list");
        let args: Vec<Value> = arg_list
            .map(|arg_list| {
                self.named_children(arg_list)
                    .into_iter()
                    .map(|arg| self.map_type_or_expr(arg))
                    .collect()
            })
            .unwrap_or_default();
        let (lparen, rparen) = self.call_paren_positions(node, arg_list);
        insert(&mut obj, "Lparen", json!(lparen));
        insert(&mut obj, "Rparen", json!(rparen));
        insert(
            &mut obj,
            "Ellipsis",
            json!(arg_list
                .and_then(|arg_list| self.ellipsis_position(arg_list))
                .unwrap_or(0)),
        );
        if args.is_empty() {
            insert(&mut obj, "Args", Value::Null);
        } else {
            insert(&mut obj, "Args", Value::Array(args));
        }
        Value::Object(obj)
    }

    fn map_type_conversion_expr(&mut self, node: Node<'a>) -> Value {
        let typ = self.field_child(node, "type");
        let operand = self.field_child(node, "operand");
        if let (Some(typ), Some(operand)) = (typ, operand) {
            if typ.start_byte() == node.start_byte()
                && self.text(typ).trim_start().starts_with("*(")
            {
                return self.map_deref_type_conversion_expr(node, typ, operand);
            }
        }

        let mut obj = self.base_obj("ast.CallExpr", node);
        if let Some(typ) = typ {
            insert(&mut obj, "Fun", self.map_type_or_expr(typ));
        }
        let (lparen, rparen) = operand
            .map(|operand| self.conversion_paren_positions(node, operand))
            .unwrap_or_else(|| self.call_paren_positions(node, None));
        insert(&mut obj, "Lparen", json!(lparen));
        insert(&mut obj, "Rparen", json!(rparen));
        insert(&mut obj, "Ellipsis", json!(0));
        if let Some(operand) = operand {
            insert(
                &mut obj,
                "Args",
                Value::Array(vec![self.map_type_or_expr(operand)]),
            );
        } else {
            insert(&mut obj, "Args", Value::Null);
        }
        Value::Object(obj)
    }

    fn map_deref_type_conversion_expr(
        &mut self,
        node: Node<'a>,
        typ: Node<'a>,
        operand: Node<'a>,
    ) -> Value {
        let mut star = self.base_obj("ast.StarExpr", node);
        insert(&mut star, "Star", json!(legacy_pos(node.start_byte())));

        let mut call = self.base_obj("ast.CallExpr", node);
        self.apply_span(&mut call, node.start_byte() + 1, node.end_byte());
        let fun = self
            .named_children(typ)
            .into_iter()
            .next()
            .map(|inner| self.map_type_or_expr(inner))
            .unwrap_or_else(|| self.map_type_or_expr(typ));
        insert(&mut call, "Fun", fun);
        let (lparen, rparen) = self.conversion_paren_positions(node, operand);
        insert(&mut call, "Lparen", json!(lparen));
        insert(&mut call, "Rparen", json!(rparen));
        insert(&mut call, "Ellipsis", json!(0));
        insert(
            &mut call,
            "Args",
            Value::Array(vec![self.map_type_or_expr(operand)]),
        );
        insert(&mut star, "X", Value::Object(call));
        Value::Object(star)
    }

    fn map_index_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.IndexExpr", node);
        let children = self.named_children(node);
        if let Some(x) = self
            .field_child(node, "operand")
            .or_else(|| children.first().copied())
        {
            insert(&mut obj, "X", self.map_type_or_expr(x));
        }
        if let Some(index) = self
            .field_child(node, "index")
            .or_else(|| children.get(1).copied())
        {
            let (lbrack, rbrack) = self.index_bracket_positions(node, index);
            insert(&mut obj, "Lbrack", json!(lbrack));
            insert(&mut obj, "Rbrack", json!(rbrack));
            insert(&mut obj, "Index", self.map_type_or_expr(index));
        } else {
            let (lbrack, rbrack) = self.bracket_positions(node);
            insert(&mut obj, "Lbrack", json!(lbrack));
            insert(&mut obj, "Rbrack", json!(rbrack));
        }
        Value::Object(obj)
    }

    fn map_slice_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.SliceExpr", node);
        let (lbrack, rbrack) = self.bracket_positions(node);
        insert(&mut obj, "Lbrack", json!(lbrack));
        insert(&mut obj, "Rbrack", json!(rbrack));
        let children = self.named_children(node);
        let x = self
            .field_child(node, "operand")
            .or_else(|| children.first().copied());
        if let Some(x) = x {
            insert(&mut obj, "X", self.map_type_or_expr(x));
        }

        let mut bounds = children.into_iter().filter(|child| {
            x.map(|x| child.start_byte() != x.start_byte() || child.end_byte() != x.end_byte())
                .unwrap_or(true)
        });
        let content = self.bracket_contents(node).unwrap_or("").to_string();
        let segments = content.split(':').collect::<Vec<_>>();
        insert(&mut obj, "Slice3", json!(segments.len() >= 3));
        for (field, segment) in ["Low", "High", "Max"].iter().zip(segments.iter()) {
            if !segment.trim().is_empty() {
                if let Some(bound) = bounds.next() {
                    insert(&mut obj, field, self.map_type_or_expr(bound));
                }
            }
        }
        Value::Object(obj)
    }

    fn map_type_instantiation(&mut self, node: Node<'a>) -> Value {
        let type_node = self
            .field_child(node, "type")
            .or_else(|| self.named_children(node).into_iter().next());
        let args = self
            .field_child(node, "type_arguments")
            .or_else(|| self.child_kind(node, "type_arguments"))
            .or_else(|| self.named_children(node).into_iter().nth(1));
        match (type_node, args) {
            (Some(type_node), Some(args)) => self.map_index_like(type_node, args, node),
            (Some(type_node), None) => self.map_type_or_expr(type_node),
            _ => self.unknown(node),
        }
    }

    fn map_index_like(&mut self, x: Node<'a>, args: Node<'a>, whole: Node<'a>) -> Value {
        let arg_nodes = self.flatten_expr_list(vec![args]);
        let mut obj = self.base_obj(
            if arg_nodes.len() > 1 {
                "ast.IndexListExpr"
            } else {
                "ast.IndexExpr"
            },
            whole,
        );
        let (lbrack, rbrack) = self.bracket_positions(whole);
        insert(&mut obj, "Lbrack", json!(lbrack));
        insert(&mut obj, "Rbrack", json!(rbrack));
        insert(&mut obj, "X", self.map_type_or_expr(x));
        if arg_nodes.len() > 1 {
            insert(
                &mut obj,
                "Indices",
                Value::Array(
                    arg_nodes
                        .into_iter()
                        .map(|arg| self.map_type_or_expr(arg))
                        .collect(),
                ),
            );
        } else if let Some(index) = arg_nodes.first() {
            insert(&mut obj, "Index", self.map_type_or_expr(*index));
        }
        Value::Object(obj)
    }

    fn map_type_assert_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.TypeAssertExpr", node);
        let (lparen, rparen) = self.type_assert_paren_positions(node);
        insert(&mut obj, "Lparen", json!(lparen));
        insert(&mut obj, "Rparen", json!(rparen));
        let children = self.named_children(node);
        if let Some(x) = self
            .field_child(node, "operand")
            .or_else(|| children.first().copied())
        {
            insert(&mut obj, "X", self.map_type_or_expr(x));
        }
        if let Some(typ) = self
            .field_child(node, "type")
            .or_else(|| children.get(1).copied())
        {
            insert(&mut obj, "Type", self.map_type_or_expr(typ));
        }
        Value::Object(obj)
    }

    fn map_array_type(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ArrayType", node);
        insert(&mut obj, "Lbrack", json!(self.lbrack_position(node)));
        if let Some(length) = self.field_child(node, "length") {
            insert(&mut obj, "Len", self.map_type_or_expr(length));
        }
        if let Some(elt) = self
            .field_child(node, "element")
            .or_else(|| self.named_children(node).into_iter().last())
        {
            insert(&mut obj, "Elt", self.map_type_or_expr(elt));
        }
        Value::Object(obj)
    }

    fn map_ellipsis_type(&mut self, node: Node<'a>, elt: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.Ellipsis", node);
        let ellipsis = self.ellipsis_position(node).unwrap_or(0);
        if ellipsis > 0 {
            self.apply_span(&mut obj, ellipsis - 1, elt.end_byte());
        }
        insert(&mut obj, "Ellipsis", json!(ellipsis));
        insert(&mut obj, "Elt", self.map_type_or_expr(elt));
        Value::Object(obj)
    }

    fn map_map_type(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.MapType", node);
        insert(
            &mut obj,
            "Map",
            json!(self.keyword_position(node, "map").unwrap_or(0)),
        );
        let children = self.named_children(node);
        if let Some(key) = self
            .field_child(node, "key")
            .or_else(|| children.first().copied())
        {
            insert(&mut obj, "Key", self.map_type_or_expr(key));
        }
        if let Some(value) = self
            .field_child(node, "value")
            .or_else(|| children.get(1).copied())
        {
            insert(&mut obj, "Value", self.map_type_or_expr(value));
        }
        Value::Object(obj)
    }

    fn map_chan_type(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ChanType", node);
        let chan = self.keyword_position(node, "chan").unwrap_or(0);
        let arrow = self.operator_position(node, "<-");
        let (begin, dir) = if arrow > 0 && (chan == 0 || arrow < chan) {
            (arrow, 2)
        } else if arrow > 0 {
            (chan, 1)
        } else {
            (chan, 3)
        };
        insert(&mut obj, "Begin", json!(begin));
        insert(&mut obj, "Arrow", json!(arrow));
        insert(&mut obj, "Dir", json!(dir));
        if let Some(value) = self
            .field_child(node, "value")
            .or_else(|| self.named_children(node).into_iter().last())
        {
            insert(&mut obj, "Value", self.map_type_or_expr(value));
        }
        Value::Object(obj)
    }

    fn map_struct_type(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.StructType", node);
        insert(
            &mut obj,
            "Struct",
            json!(self.keyword_position(node, "struct").unwrap_or(0)),
        );
        insert(&mut obj, "Incomplete", Value::Bool(false));
        if let Some(fields) = self.child_kind(node, "field_declaration_list") {
            insert(&mut obj, "Fields", self.map_field_list(fields));
        } else {
            insert(&mut obj, "Fields", self.empty_field_list(node));
        }
        Value::Object(obj)
    }

    fn map_interface_type(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.InterfaceType", node);
        insert(
            &mut obj,
            "Interface",
            json!(self.keyword_position(node, "interface").unwrap_or(0)),
        );
        insert(&mut obj, "Incomplete", Value::Bool(false));
        insert(&mut obj, "Methods", self.map_field_list(node));
        Value::Object(obj)
    }

    fn map_func_lit(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.FuncLit", node);
        insert(&mut obj, "Type", self.map_func_type(node));
        if let Some(body) = self
            .field_child(node, "body")
            .or_else(|| self.child_kind(node, "block"))
        {
            self.local_decl_scopes
                .push(self.callable_scope_names(node, false));
            insert(&mut obj, "Body", self.map_block_stmt(body));
            self.local_decl_scopes.pop();
        }
        Value::Object(obj)
    }

    fn map_composite_lit(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CompositeLit", node);
        insert(&mut obj, "Incomplete", Value::Bool(false));
        let typ = self
            .field_child(node, "type")
            .or_else(|| self.named_children(node).into_iter().next());
        let plain_keys =
            typ.is_some_and(|typ| typ.kind() != "literal_value" && typ.kind() != "map_type");
        if let Some(typ) = typ {
            if typ.kind() != "literal_value" {
                insert(&mut obj, "Type", self.map_type_or_expr(typ));
            }
        }
        if let Some(values) = self.child_kind(node, "literal_value") {
            self.insert_block_braces(&mut obj, values);
            self.plain_composite_keys.push(plain_keys);
            insert(&mut obj, "Elts", self.literal_elements(values));
            self.plain_composite_keys.pop();
        } else {
            self.insert_block_braces(&mut obj, node);
        }
        Value::Object(obj)
    }

    fn map_literal_value(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CompositeLit", node);
        insert(&mut obj, "Incomplete", Value::Bool(false));
        self.insert_block_braces(&mut obj, node);
        insert(&mut obj, "Elts", self.literal_elements(node));
        Value::Object(obj)
    }

    fn literal_elements(&mut self, node: Node<'a>) -> Value {
        let elements: Vec<_> = self
            .named_children(node)
            .into_iter()
            .filter(|child| child.kind() != "comment")
            .map(|child| self.map_type_or_expr(child))
            .collect();
        if elements.is_empty() {
            Value::Null
        } else {
            Value::Array(elements)
        }
    }

    fn map_key_value_or_element(&mut self, node: Node<'a>) -> Value {
        let raw_children = self.named_children(node);
        let children = if matches!(node.kind(), "expression_list" | "type_arguments") {
            self.flatten_expr_list(raw_children)
        } else {
            raw_children
        };
        if children.len() >= 2 {
            let mut obj = self.base_obj("ast.KeyValueExpr", node);
            insert(
                &mut obj,
                "Colon",
                json!(self.keyword_position(node, ":").unwrap_or(0)),
            );
            let key = if self.plain_composite_keys.last().copied().unwrap_or(false) {
                let key_node = self
                    .named_children(children[0])
                    .into_iter()
                    .next()
                    .unwrap_or(children[0]);
                if is_identifier_kind(key_node.kind()) {
                    let name = self.text(key_node).trim().to_string();
                    if self.is_locally_declared(&name)
                        || self.type_idents.contains(&name)
                        || self.declared_idents.contains(&name)
                    {
                        self.map_type_or_expr(key_node)
                    } else {
                        self.map_ident(key_node)
                    }
                } else {
                    self.map_type_or_expr(children[0])
                }
            } else if children[0].kind() == "field_identifier" {
                self.map_ident(children[0])
            } else {
                self.map_type_or_expr(children[0])
            };
            insert(&mut obj, "Key", key);
            insert(&mut obj, "Value", self.map_type_or_expr(children[1]));
            Value::Object(obj)
        } else if let Some(child) = children.first() {
            self.map_type_or_expr(*child)
        } else {
            self.unknown(node)
        }
    }

    fn map_type_union(&mut self, node: Node<'a>) -> Value {
        let children = self.flatten_expr_list(self.named_children(node));
        let mut iter = children.into_iter();
        let Some(first) = iter.next() else {
            return self.unknown(node);
        };
        iter.fold(self.map_type_or_expr(first), |left, right| {
            let mut obj = self.base_obj("ast.BinaryExpr", node);
            insert(&mut obj, "X", left);
            insert(&mut obj, "Y", self.map_type_or_expr(right));
            insert(&mut obj, "OpPos", json!(self.operator_position(node, "|")));
            insert(&mut obj, "Op", Value::String("|".into()));
            Value::Object(obj)
        })
    }

    fn map_ident(&mut self, node: Node<'a>) -> Value {
        self.ident_from_text(self.text(node).trim(), node)
    }

    fn map_decl_ident(&mut self, node: Node<'a>) -> Value {
        let mut ident = self.map_ident(node);
        if let Value::Object(obj) = &mut ident {
            insert(obj, "Obj", Value::Object(Map::new()));
        }
        ident
    }

    fn map_ident_ref(&mut self, node: Node<'a>) -> Value {
        let text = self.text(node);
        let trimmed = text.trim();
        let mut ident = self.ident_from_text(trimmed, node);
        let is_local = self.is_locally_declared(trimmed);
        let is_declared = self.declared_idents.contains(trimmed);
        if is_local || (is_declared && !is_builtin_identifier(trimmed)) {
            if let Value::Object(obj) = &mut ident {
                insert(obj, "Obj", Value::Object(Map::new()));
            }
        }
        ident
    }

    fn ident_from_text(&mut self, text: &str, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.Ident", node);
        insert(&mut obj, "Name", Value::String(text.into()));
        insert(&mut obj, "NamePos", json!(legacy_pos(node.start_byte())));
        Value::Object(obj)
    }

    fn map_basic_lit(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.BasicLit", node);
        insert(
            &mut obj,
            "Kind",
            Value::String(literal_kind(node.kind()).into()),
        );
        insert(&mut obj, "Value", Value::String(self.text(node)));
        insert(&mut obj, "ValuePos", json!(legacy_pos(node.start_byte())));
        Value::Object(obj)
    }

    fn wrap_child_type(&mut self, kind: &str, node: Node<'a>, field: &str) -> Value {
        let child = self
            .field_child(node, "type")
            .or_else(|| self.field_child(node, "element"))
            .or_else(|| self.named_children(node).into_iter().next());
        let mut obj = self.base_obj(kind, node);
        if kind == "ast.StarExpr" {
            insert(
                &mut obj,
                "Star",
                json!(self.keyword_position(node, "*").unwrap_or(0)),
            );
        }
        if let Some(child) = child {
            insert(&mut obj, field, self.map_type_or_expr(child));
        }
        Value::Object(obj)
    }

    fn wrap_one(&mut self, kind: &str, node: Node<'a>, field: &str, value: Value) -> Value {
        let mut obj = self.base_obj(kind, node);
        insert(&mut obj, field, value);
        Value::Object(obj)
    }

    fn unknown(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.Unknown", node);
        insert(&mut obj, "Kind", Value::String(node.kind().into()));
        Value::Object(obj)
    }

    fn base_obj(&mut self, node_type: &str, node: Node<'a>) -> Map<String, Value> {
        let id = self.next_id;
        self.next_id += 1;
        let mut obj = Map::new();
        insert(&mut obj, "node_type", Value::String(node_type.into()));
        insert(&mut obj, "node_id", json!(id));
        if node_type == "ast.File" {
            insert(
                &mut obj,
                "node_filename",
                Value::String(self.filename.clone()),
            );
        }
        insert(
            &mut obj,
            "node_line_no",
            json!(node.start_position().row + 1),
        );
        insert(
            &mut obj,
            "node_col_no",
            json!(node.start_position().column + 1),
        );
        insert(
            &mut obj,
            "node_line_no_end",
            json!(node.end_position().row + 1),
        );
        insert(
            &mut obj,
            "node_col_no_end",
            json!(node.end_position().column + 1),
        );
        obj
    }

    fn insert_block_braces(&self, obj: &mut Map<String, Value>, node: Node<'a>) -> (usize, usize) {
        let bytes = self.source.as_bytes();
        let start = node.start_byte();
        let end = node.end_byte().min(bytes.len());
        let Some(slice) = bytes.get(start..end) else {
            insert(obj, "Lbrace", json!(0));
            insert(obj, "Rbrace", json!(0));
            return (0, 0);
        };

        let lbrace = slice
            .iter()
            .position(|byte| *byte == b'{')
            .map(|offset| start + offset + 1)
            .unwrap_or(0);
        let rbrace = slice
            .iter()
            .rposition(|byte| *byte == b'}')
            .map(|offset| start + offset + 1)
            .unwrap_or(0);
        insert(obj, "Lbrace", json!(lbrace));
        insert(obj, "Rbrace", json!(rbrace));
        (lbrace, rbrace)
    }

    fn apply_brace_span(&self, obj: &mut Map<String, Value>, lbrace: usize, rbrace: usize) {
        if lbrace > 0 && rbrace > 0 {
            self.apply_span(obj, lbrace - 1, rbrace);
        }
    }

    fn paren_positions(&self, node: Node<'a>) -> (usize, usize) {
        let Some(text) = self.source.get(node.byte_range()) else {
            return (0, 0);
        };
        let lparen = text
            .find('(')
            .map(|offset| legacy_pos(node.start_byte() + offset))
            .unwrap_or(0);
        let rparen = text
            .rfind(')')
            .map(|offset| legacy_pos(node.start_byte() + offset))
            .unwrap_or(0);
        (lparen, rparen)
    }

    fn gen_decl_paren_positions(&self, node: Node<'a>, keyword: &str) -> (usize, usize) {
        let Some(text) = self.source.get(node.byte_range()) else {
            return (0, 0);
        };
        let Some(keyword_offset) = text.find(keyword) else {
            return (0, 0);
        };
        let after_keyword = keyword_offset + keyword.len();
        let Some(group_offset) = text[after_keyword..]
            .char_indices()
            .find_map(|(offset, ch)| (!ch.is_whitespace()).then_some(after_keyword + offset))
        else {
            return (0, 0);
        };
        if text.as_bytes().get(group_offset) != Some(&b'(') {
            return (0, 0);
        }
        let lparen = legacy_pos(node.start_byte() + group_offset);
        let rparen = text
            .rfind(')')
            .map(|offset| legacy_pos(node.start_byte() + offset))
            .unwrap_or(0);
        (lparen, rparen)
    }

    fn call_paren_positions(&self, node: Node<'a>, arg_list: Option<Node<'a>>) -> (usize, usize) {
        let (mut lparen, mut rparen) = arg_list
            .map(|arg_list| self.paren_positions(arg_list))
            .unwrap_or_else(|| self.paren_positions(node));
        if lparen == 0 {
            if let Some(arg_list) = arg_list {
                let before_args = arg_list.start_byte().saturating_sub(1);
                if self.source.as_bytes().get(before_args) == Some(&b'(') {
                    lparen = legacy_pos(before_args);
                }
            }
        }
        if rparen == 0 {
            if let Some(arg_list) = arg_list {
                let last_byte = arg_list.end_byte().saturating_sub(1);
                if self.source.as_bytes().get(last_byte) == Some(&b')') {
                    rparen = legacy_pos(last_byte);
                }
            }
        }
        (lparen, rparen)
    }

    fn conversion_paren_positions(&self, node: Node<'a>, operand: Node<'a>) -> (usize, usize) {
        let bytes = self.source.as_bytes();
        let before_operand = operand.start_byte().saturating_sub(1);
        let lparen = if bytes.get(before_operand) == Some(&b'(') {
            legacy_pos(before_operand)
        } else {
            self.call_paren_positions(node, None).0
        };
        let last_byte = node.end_byte().saturating_sub(1);
        let rparen = if bytes.get(last_byte) == Some(&b')') {
            legacy_pos(last_byte)
        } else {
            self.call_paren_positions(node, None).1
        };
        (lparen, rparen)
    }

    fn type_assert_paren_positions(&self, node: Node<'a>) -> (usize, usize) {
        self.type_assert_paren_positions_in(node.start_byte(), node.end_byte())
    }

    fn type_assert_paren_positions_in(&self, start: usize, end: usize) -> (usize, usize) {
        let Some(text) = self.source.get(start..end.min(self.source.len())) else {
            return (0, 0);
        };
        let lparen = text
            .find(".(")
            .map(|offset| start + offset + 2)
            .unwrap_or(0);
        let rparen = text
            .rfind(')')
            .map(|offset| legacy_pos(start + offset))
            .unwrap_or(0);
        (lparen, rparen)
    }

    fn import_ref(&mut self, spec: &Value) -> Value {
        let mut obj = Map::new();
        insert(
            &mut obj,
            "node_type",
            Value::String("ast.ImportSpec".into()),
        );
        insert(&mut obj, "node_id", json!(self.next_id));
        self.next_id += 1;
        if let Value::Object(spec_obj) = spec {
            if let Some(id) = spec_obj.get("node_id") {
                insert(&mut obj, "node_reference_id", id.clone());
            }
            for key in [
                "node_line_no",
                "node_col_no",
                "node_line_no_end",
                "node_col_no_end",
            ] {
                if let Some(value) = spec_obj.get(key) {
                    insert(&mut obj, key, value.clone());
                }
            }
        }
        Value::Object(obj)
    }

    fn reference_stub(&mut self, value: &Value) -> Option<Value> {
        let Value::Object(value_obj) = value else {
            return None;
        };
        let mut obj = Map::new();
        for key in [
            "node_type",
            "node_line_no",
            "node_col_no",
            "node_line_no_end",
            "node_col_no_end",
        ] {
            if let Some(value) = value_obj.get(key) {
                insert(&mut obj, key, value.clone());
            }
        }
        insert(&mut obj, "node_id", json!(self.next_id));
        self.next_id += 1;
        if let Some(id) = value_obj.get("node_id") {
            insert(&mut obj, "node_reference_id", id.clone());
        }
        Some(Value::Object(obj))
    }

    fn delimiter_positions(&self, node: Node<'a>) -> (usize, usize) {
        let bytes = self.source.as_bytes();
        let start = node.start_byte();
        let end = node.end_byte().min(bytes.len());
        let Some(slice) = bytes.get(start..end) else {
            return (0, 0);
        };
        let opening = slice
            .iter()
            .position(|byte| matches!(*byte, b'(' | b'[' | b'{'))
            .map(|offset| start + offset + 1)
            .unwrap_or(0);
        let closing = slice
            .iter()
            .rposition(|byte| matches!(*byte, b')' | b']' | b'}'))
            .map(|offset| start + offset + 1)
            .unwrap_or(0);
        (opening, closing)
    }

    fn lbrack_position(&self, node: Node<'a>) -> usize {
        let bytes = self.source.as_bytes();
        let start = node.start_byte();
        let end = node.end_byte().min(bytes.len());
        bytes
            .get(start..end)
            .and_then(|slice| slice.iter().position(|byte| *byte == b'['))
            .map(|offset| start + offset + 1)
            .unwrap_or(0)
    }

    fn bracket_positions(&self, node: Node<'a>) -> (usize, usize) {
        let bytes = self.source.as_bytes();
        let start = node.start_byte();
        let end = node.end_byte().min(bytes.len());
        let Some(slice) = bytes.get(start..end) else {
            return (0, 0);
        };
        let lbrack = slice
            .iter()
            .position(|byte| *byte == b'[')
            .map(|offset| start + offset + 1)
            .unwrap_or(0);
        let rbrack = slice
            .iter()
            .rposition(|byte| *byte == b']')
            .map(|offset| start + offset + 1)
            .unwrap_or(0);
        (lbrack, rbrack)
    }

    fn index_bracket_positions(&self, node: Node<'a>, index: Node<'a>) -> (usize, usize) {
        let bytes = self.source.as_bytes();
        let start = node.start_byte();
        let end = node.end_byte().min(bytes.len());
        let lbrack = bytes
            .get(start..index.start_byte().min(bytes.len()))
            .and_then(|slice| slice.iter().rposition(|byte| *byte == b'['))
            .map(|offset| start + offset + 1)
            .unwrap_or(0);
        let rbrack = bytes
            .get(index.end_byte().min(bytes.len())..end)
            .and_then(|slice| slice.iter().position(|byte| *byte == b']'))
            .map(|offset| index.end_byte() + offset + 1)
            .unwrap_or(0);
        if lbrack > 0 || rbrack > 0 {
            (lbrack, rbrack)
        } else {
            self.bracket_positions(node)
        }
    }

    fn bracket_contents(&self, node: Node<'a>) -> Option<&str> {
        let text = self.source.get(node.byte_range())?;
        let open = text.find('[')?;
        let close = text.rfind(']')?;
        text.get(open + 1..close)
    }

    fn ellipsis_position(&self, node: Node<'a>) -> Option<usize> {
        self.direct_ellipsis_position(node).or_else(|| {
            self.named_children(node)
                .into_iter()
                .filter(|child| child.kind() == "variadic_argument")
                .find_map(|child| self.direct_ellipsis_position(child))
        })
    }

    fn direct_ellipsis_position(&self, node: Node<'a>) -> Option<usize> {
        (0..node.child_count()).find_map(|idx| {
            node.child(idx).and_then(|child| {
                (!child.is_named() && child.kind() == "...").then(|| legacy_pos(child.start_byte()))
            })
        })
    }

    fn keyword_position(&self, node: Node<'a>, keyword: &str) -> Option<usize> {
        let text = self.source.get(node.byte_range())?;
        text.find(keyword)
            .map(|offset| legacy_pos(node.start_byte() + offset))
    }

    fn clause_keyword_position(&self, node: Node<'a>) -> usize {
        self.direct_keyword_position(node, &["case", "default"])
            .or_else(|| self.keyword_position(node, "case"))
            .or_else(|| self.keyword_position(node, "default"))
            .unwrap_or(0)
    }

    fn direct_keyword_position(&self, node: Node<'a>, keywords: &[&str]) -> Option<usize> {
        (0..node.child_count()).find_map(|idx| {
            node.child(idx).and_then(|child| {
                (!child.is_named() && keywords.contains(&child.kind()))
                    .then(|| legacy_pos(child.start_byte()))
            })
        })
    }

    fn clause_end_byte(&self, node: Node<'a>, body_nodes: &[Node<'a>]) -> usize {
        body_nodes
            .last()
            .map(|body| body.end_byte())
            .or_else(|| self.keyword_byte(node, ":").map(|colon| colon + 1))
            .unwrap_or_else(|| node.end_byte())
    }

    fn keyword_byte(&self, node: Node<'a>, keyword: &str) -> Option<usize> {
        let text = self.source.get(node.byte_range())?;
        text.find(keyword).map(|offset| node.start_byte() + offset)
    }

    fn func_type_span(&self, node: Node<'a>, target: Node<'a>) -> (usize, usize) {
        let start = if node.kind() == "method_elem" {
            self.field_child(node, "parameters")
                .map(|params| params.start_byte())
                .unwrap_or_else(|| target.start_byte())
        } else {
            self.keyword_byte(node, "func")
                .unwrap_or_else(|| target.start_byte())
        };
        let end = self
            .field_child(node, "body")
            .or_else(|| self.child_kind(node, "block"))
            .map(|body| self.trim_ascii_whitespace_end(body.start_byte()))
            .unwrap_or_else(|| target.end_byte());
        (start, end)
    }

    fn trim_ascii_whitespace_end(&self, end_byte: usize) -> usize {
        let bytes = self.source.as_bytes();
        let mut end = end_byte.min(bytes.len());
        while end > 0 && bytes[end - 1].is_ascii_whitespace() {
            end -= 1;
        }
        end
    }

    fn apply_span(&self, obj: &mut Map<String, Value>, start_byte: usize, end_byte: usize) {
        let (start_row, start_col) = byte_to_line_col(self.source, start_byte);
        let (end_row, end_col) = byte_to_line_col(self.source, end_byte);
        insert(obj, "node_line_no", json!(start_row + 1));
        insert(obj, "node_col_no", json!(start_col + 1));
        insert(obj, "node_line_no_end", json!(end_row + 1));
        insert(obj, "node_col_no_end", json!(end_col + 1));
    }

    fn type_switch_condition_span(
        &self,
        node: Node<'a>,
        alias: Option<Node<'a>>,
        value: Node<'a>,
    ) -> (usize, usize) {
        let start = alias
            .map(|n| n.start_byte())
            .unwrap_or_else(|| value.start_byte());
        let node_text = self.text(node);
        let relative_end = node_text
            .find('{')
            .map(|idx| {
                let before_brace = &node_text[..idx];
                before_brace.trim_end().len()
            })
            .unwrap_or_else(|| value.end_byte() - node.start_byte());
        (start, node.start_byte() + relative_end)
    }

    fn named_children(&self, node: Node<'a>) -> Vec<Node<'a>> {
        let mut cursor = node.walk();
        node.named_children(&mut cursor).collect()
    }

    fn descendants_named(&self, node: Node<'a>) -> Vec<Node<'a>> {
        let mut out = Vec::new();
        self.collect_named(node, &mut out);
        out
    }

    fn collect_named(&self, node: Node<'a>, out: &mut Vec<Node<'a>>) {
        for child in self.named_children(node) {
            out.push(child);
            self.collect_named(child, out);
        }
    }

    fn field_child(&self, node: Node<'a>, field: &str) -> Option<Node<'a>> {
        node.child_by_field_name(field).filter(Node::is_named)
    }

    fn field_children(&self, node: Node<'a>, field: &str) -> Vec<Node<'a>> {
        (0..node.child_count())
            .filter_map(|idx| {
                if node.field_name_for_child(idx as u32) == Some(field) {
                    node.child(idx).filter(Node::is_named)
                } else {
                    None
                }
            })
            .collect()
    }

    fn child_kind(&self, node: Node<'a>, kind: &str) -> Option<Node<'a>> {
        self.named_children(node)
            .into_iter()
            .find(|child| child.kind() == kind)
    }

    fn first_identifier(&self, node: Node<'a>) -> Option<Node<'a>> {
        self.named_children(node)
            .into_iter()
            .find(|child| is_identifier_kind(child.kind()))
    }

    fn flatten_expr_list(&self, nodes: Vec<Node<'a>>) -> Vec<Node<'a>> {
        nodes
            .into_iter()
            .flat_map(|node| match node.kind() {
                "expression_list" | "type_arguments" | "type_elem" | "type_constraint" => {
                    self.named_children(node)
                }
                _ => vec![node],
            })
            .collect()
    }

    fn flatten_statement_lists(&self, nodes: Vec<Node<'a>>) -> Vec<Node<'a>> {
        nodes
            .into_iter()
            .flat_map(|node| {
                if node.kind() == "statement_list" {
                    self.named_children(node)
                } else {
                    vec![node]
                }
            })
            .collect()
    }

    fn operator_token(&self, node: Node<'a>) -> Option<String> {
        const OPS: &[&str] = &[
            "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "&^", "+=", "-=", "*=", "/=", "%=",
            "&=", "|=", "^=", "<<=", ">>=", "&^=", "&&", "||", "<-", "==", "!=", "<", "<=", ">",
            ">=", "=", ":=", "!",
        ];
        (0..node.child_count()).find_map(|idx| {
            node.child(idx).and_then(|child| {
                if !child.is_named() && OPS.contains(&child.kind()) {
                    Some(child.kind().to_string())
                } else {
                    None
                }
            })
        })
    }

    fn operator_position(&self, node: Node<'a>, op: &str) -> usize {
        if op.is_empty() {
            return 0;
        }
        if let Some(position) = (0..node.child_count()).find_map(|idx| {
            node.child(idx).and_then(|child| {
                (!child.is_named() && child.kind() == op).then(|| legacy_pos(child.start_byte()))
            })
        }) {
            return position;
        }
        self.source
            .get(node.byte_range())
            .and_then(|text| text.find(op))
            .map(|offset| legacy_pos(node.start_byte() + offset))
            .unwrap_or(0)
    }

    fn callable_scope_names(&self, node: Node<'a>, include_receiver: bool) -> HashSet<String> {
        let mut names = HashSet::new();
        if let Some(type_params) = self.field_child(node, "type_parameters") {
            collect_declared_identifiers_inner(type_params, self.source, &mut names);
        }
        if include_receiver {
            if let Some(receiver) = self.field_child(node, "receiver") {
                collect_declared_identifiers_inner(receiver, self.source, &mut names);
            }
        }
        if let Some(parameters) = self.field_child(node, "parameters") {
            collect_declared_identifiers_inner(parameters, self.source, &mut names);
        }
        if let Some(result) = self.field_child(node, "result") {
            collect_declared_identifiers_inner(result, self.source, &mut names);
        }
        names
    }

    fn declared_names_in(&self, node: Node<'a>) -> HashSet<String> {
        let mut names = HashSet::new();
        collect_declared_identifiers_inner(node, self.source, &mut names);
        names
    }

    fn add_local_declared_nodes(&mut self, nodes: &[Node<'a>]) {
        let Some(scope) = self.local_decl_scopes.last_mut() else {
            return;
        };
        let mut names = HashSet::new();
        for node in nodes {
            collect_identifier_descendants(*node, self.source, &mut names);
        }
        scope.extend(names);
    }

    fn add_local_declared_names(&mut self, names: HashSet<String>) {
        let Some(scope) = self.local_decl_scopes.last_mut() else {
            return;
        };
        scope.extend(names);
    }

    fn is_locally_declared(&self, name: &str) -> bool {
        self.local_decl_scopes
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    fn text(&self, node: Node<'a>) -> String {
        self.source
            .get(node.byte_range())
            .unwrap_or_default()
            .to_string()
    }

    fn go_version(&self) -> String {
        for line in self.source.lines() {
            let trimmed = line.trim_start();
            if trimmed.is_empty() {
                continue;
            }
            if !trimmed.starts_with("//") {
                break;
            }
            let Some(expr) = trimmed.strip_prefix("//go:build") else {
                continue;
            };
            if let Some(version) = expr
                .split(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '.'))
                .find(|part| part.starts_with("go1.") && !expr.contains(&format!("!{part}")))
            {
                return version.to_string();
            }
        }
        String::new()
    }
}

fn insert(obj: &mut Map<String, Value>, key: &str, value: Value) {
    obj.insert(key.into(), value);
}

fn same_tree_sitter_node(left: Node<'_>, right: Node<'_>) -> bool {
    left.kind() == right.kind()
        && left.start_byte() == right.start_byte()
        && left.end_byte() == right.end_byte()
}

fn legacy_pos(byte_offset: usize) -> usize {
    byte_offset + 1
}

fn collect_imported_identifiers(root: Node<'_>, source: &str) -> HashSet<String> {
    let mut imported = HashSet::new();
    collect_imported_identifiers_inner(root, source, &mut imported);
    imported
}

fn collect_type_identifiers(root: Node<'_>, source: &str) -> HashSet<String> {
    let mut types = HashSet::new();
    collect_type_identifiers_inner(root, source, &mut types);
    types
}

fn collect_type_identifiers_inner(node: Node<'_>, source: &str, types: &mut HashSet<String>) {
    if node.kind() == "type_spec" {
        for ident in field_children_for(node, "name") {
            collect_declared_identifier(ident, source, types);
        }
    }

    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        collect_type_identifiers_inner(child, source, types);
    }
}

fn collect_imported_identifiers_inner(
    node: Node<'_>,
    source: &str,
    imported: &mut HashSet<String>,
) {
    if node.kind() == "import_spec" {
        if let Some(alias) = field_children_for(node, "name").into_iter().next() {
            let name = source.get(alias.byte_range()).unwrap_or_default().trim();
            if !matches!(name, "" | "." | "_") {
                imported.insert(name.to_string());
            }
            return;
        }
        let path = {
            let mut cursor = node.walk();
            let path = node
                .named_children(&mut cursor)
                .find(|child| is_string_literal_kind(child.kind()));
            path
        };
        if let Some(path) = path {
            let raw = source.get(path.byte_range()).unwrap_or_default().trim();
            let unquoted = raw.trim_matches('"').trim_matches('`');
            if let Some(name) = unquoted.rsplit('/').next().filter(|name| !name.is_empty()) {
                imported.insert(name.to_string());
            }
        }
    }

    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        collect_imported_identifiers_inner(child, source, imported);
    }
}

fn collect_declared_identifiers(root: Node<'_>, source: &str) -> HashSet<String> {
    let mut declared = HashSet::new();
    let mut cursor = root.walk();
    for child in root.named_children(&mut cursor) {
        match child.kind() {
            "var_declaration" => {
                collect_top_level_spec_identifiers(child, "var_spec", source, &mut declared)
            }
            "const_declaration" => {
                collect_top_level_spec_identifiers(child, "const_spec", source, &mut declared)
            }
            "type_declaration" => {
                collect_top_level_spec_identifiers(child, "type_spec", source, &mut declared)
            }
            "function_declaration" => {
                for ident in field_children_for(child, "name") {
                    collect_declared_identifier(ident, source, &mut declared);
                }
            }
            _ => {}
        }
    }
    declared
}

fn collect_top_level_spec_identifiers(
    node: Node<'_>,
    spec_kind: &str,
    source: &str,
    declared: &mut HashSet<String>,
) {
    let mut stack = vec![node];
    while let Some(current) = stack.pop() {
        if current.kind() == spec_kind {
            for ident in field_children_for(current, "name") {
                collect_declared_identifier(ident, source, declared);
            }
        }
        let mut cursor = current.walk();
        for child in current.named_children(&mut cursor) {
            stack.push(child);
        }
    }
}

fn collect_declared_identifiers_inner(
    node: Node<'_>,
    source: &str,
    declared: &mut HashSet<String>,
) {
    match node.kind() {
        "var_spec"
        | "const_spec"
        | "type_spec"
        | "function_declaration"
        | "parameter_declaration"
        | "variadic_parameter_declaration"
        | "type_parameter_declaration" => {
            for ident in field_children_for(node, "name") {
                collect_declared_identifier(ident, source, declared);
            }
        }
        "short_var_declaration" => {
            if let Some(left) = node.child_by_field_name("left").filter(Node::is_named) {
                collect_identifier_descendants(left, source, declared);
            }
        }
        "range_clause" => {
            if let Some(left) = node.child_by_field_name("left").filter(Node::is_named) {
                collect_identifier_descendants(left, source, declared);
            }
        }
        "type_switch_statement" => {
            for alias in field_children_for(node, "alias") {
                collect_identifier_descendants(alias, source, declared);
            }
        }
        _ => {}
    }

    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        collect_declared_identifiers_inner(child, source, declared);
    }
}

fn collect_identifier_descendants(node: Node<'_>, source: &str, declared: &mut HashSet<String>) {
    collect_declared_identifier(node, source, declared);
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        collect_identifier_descendants(child, source, declared);
    }
}

fn collect_declared_identifier(node: Node<'_>, source: &str, declared: &mut HashSet<String>) {
    if is_identifier_kind(node.kind()) {
        let text = source.get(node.byte_range()).unwrap_or_default().trim();
        if !matches!(text, "" | "_" | "nil" | "true" | "false") {
            declared.insert(text.to_string());
        }
    }
}

fn field_children_for<'a>(node: Node<'a>, field: &str) -> Vec<Node<'a>> {
    (0..node.child_count())
        .filter_map(|idx| {
            if node.field_name_for_child(idx as u32) == Some(field) {
                node.child(idx).filter(Node::is_named)
            } else {
                None
            }
        })
        .collect()
}

fn byte_to_line_col(source: &str, byte: usize) -> (usize, usize) {
    let mut row = 0;
    let mut col = 0;
    for b in source.as_bytes().iter().take(byte.min(source.len())) {
        if *b == b'\n' {
            row += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (row, col)
}

fn with_decl_object(mut value: Value, decl: Option<&Value>) -> Value {
    if let (Value::Object(obj), Some(decl)) = (&mut value, decl) {
        let mut ident_obj = Map::new();
        ident_obj.insert("Decl".into(), decl.clone());
        obj.insert("Obj".into(), Value::Object(ident_obj));
    }
    value
}

fn remove_obj_from_index_fields(value: &mut Value) {
    match value {
        Value::Object(obj) => {
            if let Some(Value::Object(index_obj)) = obj.get_mut("Index") {
                index_obj.remove("Obj");
            }
            for value in obj.values_mut() {
                remove_obj_from_index_fields(value);
            }
        }
        Value::Array(values) => {
            for value in values {
                remove_obj_from_index_fields(value);
            }
        }
        _ => {}
    }
}

fn literal_kind(kind: &str) -> &'static str {
    match kind {
        "int_literal" => "INT",
        "float_literal" => "FLOAT",
        "imaginary_literal" => "IMAG",
        "rune_literal" => "CHAR",
        _ => "STRING",
    }
}

fn is_identifier_kind(kind: &str) -> bool {
    matches!(
        kind,
        "identifier"
            | "package_identifier"
            | "type_identifier"
            | "field_identifier"
            | "label_name"
            | "iota"
            | "nil"
            | "true"
            | "false"
    )
}

fn is_builtin_identifier(name: &str) -> bool {
    matches!(
        name,
        "append"
            | "cap"
            | "clear"
            | "close"
            | "complex"
            | "copy"
            | "delete"
            | "imag"
            | "len"
            | "make"
            | "max"
            | "min"
            | "new"
            | "panic"
            | "print"
            | "println"
            | "real"
            | "recover"
    )
}

fn is_string_literal_kind(kind: &str) -> bool {
    matches!(kind, "interpreted_string_literal" | "raw_string_literal")
}

fn is_type_kind(kind: &str) -> bool {
    matches!(
        kind,
        "pointer_type"
            | "array_type"
            | "slice_type"
            | "implicit_length_array_type"
            | "map_type"
            | "channel_type"
            | "struct_type"
            | "interface_type"
            | "function_type"
            | "qualified_type"
    ) || is_identifier_kind(kind)
}

fn is_expression_kind(kind: &str) -> bool {
    matches!(
        kind,
        "identifier"
            | "selector_expression"
            | "call_expression"
            | "index_expression"
            | "slice_expression"
            | "binary_expression"
            | "unary_expression"
            | "parenthesized_expression"
            | "type_assertion_expression"
            | "composite_literal"
            | "func_literal"
            | "function_literal"
            | "interpreted_string_literal"
            | "raw_string_literal"
            | "int_literal"
            | "float_literal"
            | "imaginary_literal"
            | "rune_literal"
    )
}
