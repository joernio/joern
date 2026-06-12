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
    let declared_idents = collect_declared_identifiers(root, source);
    let mut mapper = Mapper::new(path.to_string_lossy().into_owned(), source, declared_idents);
    Ok(mapper.map_file(root))
}

struct Mapper<'a> {
    filename: String,
    source: &'a str,
    next_id: u64,
    declared_idents: HashSet<String>,
}

impl<'a> Mapper<'a> {
    fn new(filename: String, source: &'a str, declared_idents: HashSet<String>) -> Self {
        Self {
            filename,
            source,
            next_id: 1,
            declared_idents,
        }
    }

    fn map_file(&mut self, node: Node<'a>) -> Value {
        let mut decls = Vec::new();
        let mut imports = Vec::new();
        let mut package_name = self.ident_from_text("main", node);

        for child in self.named_children(node) {
            match child.kind() {
                "package_clause" => {
                    if let Some(name) = self
                        .named_children(child)
                        .into_iter()
                        .find(|n| is_identifier_kind(n.kind()))
                    {
                        package_name = self.map_ident(name);
                    }
                }
                "import_declaration" => {
                    let decl = self.map_gen_decl(child, "import");
                    if let Some(specs) = decl.get("Specs").and_then(Value::as_array) {
                        imports.extend(specs.iter().cloned());
                    }
                    decls.push(Value::Object(decl));
                }
                "type_declaration" => decls.push(Value::Object(self.map_gen_decl(child, "type"))),
                "var_declaration" => decls.push(Value::Object(self.map_gen_decl(child, "var"))),
                "const_declaration" => decls.push(Value::Object(self.map_gen_decl(child, "const"))),
                "function_declaration" => decls.push(self.map_function_decl(child, false)),
                "method_declaration" => decls.push(self.map_function_decl(child, true)),
                _ => {}
            }
        }

        let mut obj = self.base_obj("ast.File", node);
        insert(&mut obj, "Name", package_name);
        insert(&mut obj, "Decls", Value::Array(decls));
        insert(&mut obj, "Imports", Value::Array(imports));
        insert(&mut obj, "Comments", Value::Array(Vec::new()));
        insert(&mut obj, "Unresolved", Value::Array(Vec::new()));
        insert(&mut obj, "GoVersion", Value::String(String::new()));
        insert(&mut obj, "FileStart", json!(node.start_byte()));
        insert(&mut obj, "FileEnd", json!(node.end_byte()));
        insert(&mut obj, "Package", json!(node.start_byte()));
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
            .descendants_named(node)
            .into_iter()
            .filter(|child| child.kind() == spec_kind)
            .map(|child| match spec_kind {
                "import_spec" => self.map_import_spec(child),
                "type_spec" => self.map_type_spec(child),
                _ => self.map_value_spec(child),
            })
            .collect();

        let mut obj = self.base_obj("ast.GenDecl", node);
        insert(&mut obj, "Tok", Value::String(tok.into()));
        insert(&mut obj, "TokPos", json!(node.start_byte()));
        insert(&mut obj, "Lparen", json!(0));
        insert(&mut obj, "Rparen", json!(0));
        insert(&mut obj, "Specs", Value::Array(specs));
        obj
    }

    fn map_import_spec(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ImportSpec", node);
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
            insert(&mut obj, "Name", self.map_ident(name));
        }
        if let Some(type_params) = self.field_child(node, "type_parameters") {
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

        insert(
            &mut obj,
            "Names",
            Value::Array(
                fallback_names
                    .into_iter()
                    .map(|n| self.map_ident(n))
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
        }
        Value::Object(obj)
    }

    fn map_function_decl(&mut self, node: Node<'a>, is_method: bool) -> Value {
        let mut obj = self.base_obj("ast.FuncDecl", node);
        if let Some(name) = self
            .field_child(node, "name")
            .or_else(|| self.first_identifier(node))
        {
            insert(&mut obj, "Name", self.map_ident(name));
        }
        insert(&mut obj, "Type", self.map_func_type(node));
        if is_method {
            if let Some(receiver) = self.field_child(node, "receiver") {
                insert(&mut obj, "Recv", self.map_field_list(receiver));
            }
        }
        if let Some(body) = self
            .field_child(node, "body")
            .or_else(|| self.child_kind(node, "block"))
        {
            insert(&mut obj, "Body", self.map_block_stmt(body));
        }
        Value::Object(obj)
    }

    fn map_func_type(&mut self, node: Node<'a>) -> Value {
        let target = self.child_kind(node, "function_type").unwrap_or(node);
        let mut obj = self.base_obj("ast.FuncType", target);
        if let Some(type_params) = self.field_child(node, "type_parameters") {
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
        Value::Object(obj)
    }

    fn map_result_list(&mut self, node: Node<'a>) -> Value {
        if node.kind() == "parameter_list" {
            self.map_field_list(node)
        } else {
            let mut list = self.base_obj("ast.FieldList", node);
            let mut field = self.base_obj("ast.Field", node);
            insert(&mut field, "Type", self.map_type_or_expr(node));
            insert(&mut list, "List", Value::Array(vec![Value::Object(field)]));
            Value::Object(list)
        }
    }

    fn map_field_list(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.FieldList", node);
        let fields = self
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
        insert(&mut obj, "List", Value::Array(fields));
        Value::Object(obj)
    }

    fn empty_field_list(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.FieldList", node);
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
                Value::Array(names.into_iter().map(|name| self.map_ident(name)).collect()),
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
            insert(&mut obj, "Names", Value::Array(vec![self.map_ident(name)]));
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
        let statement_parent = self.child_kind(node, "statement_list").unwrap_or(node);
        let list = self
            .named_children(statement_parent)
            .into_iter()
            .filter_map(|child| self.map_statement(child))
            .collect();
        insert(&mut obj, "List", Value::Array(list));
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
            "range_clause" => Some(self.map_range_stmt(node, None)),
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
        self.wrap_one("ast.DeclStmt", node, "Decl", value)
    }

    fn map_assign_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.AssignStmt", node);
        let lhs = self.flatten_expr_list(self.field_children(node, "left"));
        let rhs = self.flatten_expr_list(self.field_children(node, "right"));
        let named = self.named_children(node);
        let op = self.operator_token(node).unwrap_or_else(|| {
            if node.kind() == "short_var_declaration" {
                ":=".into()
            } else {
                "=".into()
            }
        });
        insert(&mut obj, "Tok", Value::String(op));
        insert(
            &mut obj,
            "Lhs",
            Value::Array(if lhs.is_empty() {
                named
                    .first()
                    .map(|n| self.map_type_or_expr(*n))
                    .into_iter()
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
        Value::Object(obj)
    }

    fn map_return_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ReturnStmt", node);
        let result_nodes = self.flatten_expr_list(self.named_children(node));
        let results = result_nodes
            .into_iter()
            .map(|n| self.map_type_or_expr(n))
            .collect();
        insert(&mut obj, "Results", Value::Array(results));
        Value::Object(obj)
    }

    fn map_if_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.IfStmt", node);
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
        Value::Object(obj)
    }

    fn map_for_stmt(&mut self, node: Node<'a>) -> Value {
        if let Some(range) = self.child_kind(node, "range_clause") {
            return self.map_range_stmt(range, self.child_kind(node, "block"));
        }
        let mut obj = self.base_obj("ast.ForStmt", node);
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
        Value::Object(obj)
    }

    fn map_range_stmt(&mut self, node: Node<'a>, body: Option<Node<'a>>) -> Value {
        let mut obj = self.base_obj("ast.RangeStmt", node);
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
            insert(
                &mut decl,
                "Tok",
                Value::String(self.operator_token(node).unwrap_or_else(|| ":=".into())),
            );
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
                with_decl_object(value.clone(), range_decl.as_ref()),
            );
        }
        if let Some(right_value) = right_value {
            insert(&mut obj, "X", right_value);
        }
        insert(
            &mut obj,
            "Tok",
            Value::String(self.operator_token(node).unwrap_or_else(|| ":=".into())),
        );
        if let Some(body) = body {
            insert(&mut obj, "Body", self.map_block_stmt(body));
        }
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
        if type_switch {
            if let Some(value) = self.field_child(node, "value") {
                insert(&mut obj, "Assign", self.map_type_switch_assign(node, value));
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
        insert(&mut body, "List", Value::Array(cases));
        insert(&mut obj, "Body", Value::Object(body));
        Value::Object(obj)
    }

    fn map_type_switch_assign(&mut self, node: Node<'a>, value: Node<'a>) -> Value {
        let (_, condition_end) = self.type_switch_condition_span(node, None, value);
        let type_assert = self.map_type_switch_assert(value, condition_end);
        if let Some(alias) = self.field_child(node, "alias") {
            let (condition_start, condition_end) =
                self.type_switch_condition_span(node, Some(alias), value);
            let lhs = self
                .flatten_expr_list(vec![alias])
                .into_iter()
                .map(|n| self.map_type_or_expr(n))
                .collect();
            let mut obj = self.base_obj("ast.AssignStmt", node);
            self.apply_span(&mut obj, condition_start, condition_end);
            insert(
                &mut obj,
                "Tok",
                Value::String(self.operator_token(node).unwrap_or_else(|| ":=".into())),
            );
            insert(&mut obj, "Lhs", Value::Array(lhs));
            insert(&mut obj, "Rhs", Value::Array(vec![type_assert]));
            Value::Object(obj)
        } else {
            type_assert
        }
    }

    fn map_type_switch_assert(&mut self, value: Node<'a>, end_byte: usize) -> Value {
        let mut obj = self.base_obj("ast.TypeAssertExpr", value);
        self.apply_span(&mut obj, value.start_byte(), end_byte);
        insert(&mut obj, "X", self.map_type_or_expr(value));
        insert(&mut obj, "Type", Value::Null);
        Value::Object(obj)
    }

    fn map_case_clause(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CaseClause", node);
        let value_field = if node.kind() == "type_case" {
            "type"
        } else {
            "value"
        };
        let values = self.flatten_expr_list(self.field_children(node, value_field));
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
        let body_nodes = self.flatten_statement_lists(self.named_children(node));
        let body = body_nodes
            .into_iter()
            .filter(|n| !matches!(n.kind(), "expression_list" | "type_list"))
            .filter_map(|n| self.map_statement(n))
            .collect();
        insert(&mut obj, "Body", Value::Array(body));
        Value::Object(obj)
    }

    fn map_select_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.SelectStmt", node);
        let clauses = self
            .descendants_named(node)
            .into_iter()
            .filter(|n| n.kind() == "communication_case" || n.kind() == "default_case")
            .map(|n| self.map_comm_clause(n))
            .collect();
        let mut body = self.base_obj("ast.BlockStmt", node);
        insert(&mut body, "List", Value::Array(clauses));
        insert(&mut obj, "Body", Value::Object(body));
        Value::Object(obj)
    }

    fn map_comm_clause(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CommClause", node);
        if let Some(comm) = self.field_child(node, "communication").or_else(|| {
            self.named_children(node).into_iter().find(|n| {
                matches!(
                    n.kind(),
                    "send_statement"
                        | "receive_statement"
                        | "assignment_statement"
                        | "short_var_declaration"
                )
            })
        }) {
            if let Some(stmt) = self.map_statement(comm) {
                insert(&mut obj, "Comm", stmt);
            }
        }
        let body_nodes = self.flatten_statement_lists(self.named_children(node));
        let body = body_nodes
            .into_iter()
            .filter_map(|n| self.map_statement(n))
            .collect();
        insert(&mut obj, "Body", Value::Array(body));
        Value::Object(obj)
    }

    fn map_call_statement(&mut self, kind: &str, node: Node<'a>) -> Option<Value> {
        self.descendants_named(node)
            .into_iter()
            .find(|n| n.kind() == "call_expression")
            .map(|call| {
                let value = self.map_call_expr(call);
                self.wrap_one(kind, node, "Call", value)
            })
    }

    fn map_send_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.SendStmt", node);
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
        insert(&mut obj, "Tok", Value::String(tok));
        if let Some(label) = self.first_identifier(node) {
            insert(&mut obj, "Label", self.map_ident(label));
        }
        Value::Object(obj)
    }

    fn map_labeled_stmt(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.LabeledStmt", node);
        if let Some(label) = self.first_identifier(node) {
            insert(&mut obj, "Label", self.map_ident(label));
        }
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
            "index_expression" | "slice_expression" => self.map_index_expr(node),
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
        insert(
            &mut obj,
            "Op",
            Value::String(self.operator_token(node).unwrap_or_default()),
        );
        Value::Object(obj)
    }

    fn map_unary_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.UnaryExpr", node);
        if let Some(child) = self.named_children(node).into_iter().next() {
            insert(&mut obj, "X", self.map_type_or_expr(child));
        }
        insert(
            &mut obj,
            "Op",
            Value::String(self.operator_token(node).unwrap_or_default()),
        );
        Value::Object(obj)
    }

    fn map_paren_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.ParenExpr", node);
        if let Some(child) = self.named_children(node).into_iter().next() {
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
            insert(&mut obj, "X", self.map_type_or_expr(x));
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
        let args = self
            .child_kind(node, "argument_list")
            .map(|arg_list| {
                self.named_children(arg_list)
                    .into_iter()
                    .map(|arg| self.map_type_or_expr(arg))
                    .collect()
            })
            .unwrap_or_default();
        insert(&mut obj, "Args", Value::Array(args));
        Value::Object(obj)
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
            insert(&mut obj, "Index", self.map_type_or_expr(index));
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
        let mut obj = self.base_obj("ast.IndexExpr", whole);
        insert(&mut obj, "X", self.map_type_or_expr(x));
        let arg_nodes = self.flatten_expr_list(vec![args]);
        if let Some(index) = arg_nodes.first() {
            insert(&mut obj, "Index", self.map_type_or_expr(*index));
        }
        Value::Object(obj)
    }

    fn map_type_assert_expr(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.TypeAssertExpr", node);
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
        insert(&mut obj, "Elt", self.map_type_or_expr(elt));
        Value::Object(obj)
    }

    fn map_map_type(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.MapType", node);
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
        if let Some(fields) = self.child_kind(node, "field_declaration_list") {
            insert(&mut obj, "Fields", self.map_field_list(fields));
        } else {
            insert(&mut obj, "Fields", self.empty_field_list(node));
        }
        Value::Object(obj)
    }

    fn map_interface_type(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.InterfaceType", node);
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
            insert(&mut obj, "Body", self.map_block_stmt(body));
        }
        Value::Object(obj)
    }

    fn map_composite_lit(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CompositeLit", node);
        if let Some(typ) = self
            .field_child(node, "type")
            .or_else(|| self.named_children(node).into_iter().next())
        {
            if typ.kind() != "literal_value" {
                insert(&mut obj, "Type", self.map_type_or_expr(typ));
            }
        }
        if let Some(values) = self.child_kind(node, "literal_value") {
            insert(&mut obj, "Elts", self.literal_elements(values));
        }
        Value::Object(obj)
    }

    fn map_literal_value(&mut self, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.CompositeLit", node);
        insert(&mut obj, "Elts", self.literal_elements(node));
        Value::Object(obj)
    }

    fn literal_elements(&mut self, node: Node<'a>) -> Value {
        Value::Array(
            self.named_children(node)
                .into_iter()
                .map(|child| self.map_type_or_expr(child))
                .collect(),
        )
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
            insert(&mut obj, "Key", self.map_type_or_expr(children[0]));
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
            insert(&mut obj, "Op", Value::String("|".into()));
            Value::Object(obj)
        })
    }

    fn map_ident(&mut self, node: Node<'a>) -> Value {
        self.ident_from_text(self.text(node).trim(), node)
    }

    fn map_ident_ref(&mut self, node: Node<'a>) -> Value {
        let text = self.text(node);
        let trimmed = text.trim();
        let mut ident = self.ident_from_text(trimmed, node);
        if self.declared_idents.contains(trimmed) {
            if let Value::Object(obj) = &mut ident {
                insert(obj, "Obj", Value::Object(Map::new()));
            }
        }
        ident
    }

    fn ident_from_text(&mut self, text: &str, node: Node<'a>) -> Value {
        let mut obj = self.base_obj("ast.Ident", node);
        insert(&mut obj, "Name", Value::String(text.into()));
        insert(&mut obj, "NamePos", json!(node.start_byte()));
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
        insert(&mut obj, "ValuePos", json!(node.start_byte()));
        Value::Object(obj)
    }

    fn wrap_child_type(&mut self, kind: &str, node: Node<'a>, field: &str) -> Value {
        let child = self
            .field_child(node, "type")
            .or_else(|| self.field_child(node, "element"))
            .or_else(|| self.named_children(node).into_iter().next());
        let mut obj = self.base_obj(kind, node);
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
        insert(
            &mut obj,
            "node_filename",
            Value::String(self.filename.clone()),
        );
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

    fn text(&self, node: Node<'a>) -> String {
        self.source
            .get(node.byte_range())
            .unwrap_or_default()
            .to_string()
    }
}

fn insert(obj: &mut Map<String, Value>, key: &str, value: Value) {
    obj.insert(key.into(), value);
}

fn collect_declared_identifiers(root: Node<'_>, source: &str) -> HashSet<String> {
    let mut declared = HashSet::new();
    collect_declared_identifiers_inner(root, source, &mut declared);
    declared
}

fn collect_declared_identifiers_inner(
    node: Node<'_>,
    source: &str,
    declared: &mut HashSet<String>,
) {
    match node.kind() {
        "var_spec" | "const_spec" | "parameter_declaration" | "variadic_parameter_declaration" => {
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
            for field in ["key", "value"] {
                if let Some(ident) = node.child_by_field_name(field).filter(Node::is_named) {
                    collect_declared_identifier(ident, source, declared);
                }
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
            | "nil"
            | "true"
            | "false"
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
