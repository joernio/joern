grammar Function;
import ModuleLex, Common;

@parser::members {
   public void preProcSkipToEnd() {
        Stack<Object> CurlyStack = new Stack<>();
        Object o = new Object();
        int t = _input.LA(1);

        while(t != EOF && !(CurlyStack.empty() && t == PRE_ENDIF)){

            if(t == PRE_IF) CurlyStack.push(o);
            else if(t == PRE_ENDIF) CurlyStack.pop();

            consume();
            t = _input.LA(1);
        }
        if(t != EOF) consume();
   }
}

statements: (pre_opener
            | pre_closer
            | pre_else {preProcSkipToEnd(); }
            | statement)*;

statement: opening_curly
         | closing_curly
         | block_starter
         | jump_statement
         | label 
         | simple_decl
         | expr_statement
         | water
        ;

pre_opener: PRE_IF;
pre_else: PRE_ELSE;
pre_closer: PRE_ENDIF;
opening_curly: '{';
closing_curly: '}';
                
block_starter: selection_or_iteration;

selection_or_iteration: TRY                      #Try_statement
                      | CATCH '(' (param_type | ELLIPSIS) ')' #Catch_statement
                      | IF '(' condition ')'     #If_statement
                      | ELSE                     #Else_statement
                      | SWITCH '(' condition ')' #Switch_statement
                      | FOR '(' (for_init_statement | ';') condition? ';'  expr? ')' #For_statement
                      | DO                          #Do_statement
                      | WHILE '(' condition ')'     #While_statement
;

// Don't know why, but: introducing this unused rule results
// in a performance boost.

do_statement1: DO statement WHILE '(' expr ')';

for_init_statement : simple_decl
                   | expr ';'
                   ;

jump_statement: BREAK ';'		#breakStatement
              | CONTINUE ';' 		#continueStatement
              | GOTO identifier ';'	#gotoStatement
              | RETURN expr? ';'	#returnStatement
              | THROW expr?  ';'	#throwStatement
              ;

label: CASE? (identifier | number | CHAR ) ':' ;

expr_statement: expr? ';';

condition: expr
	 | type_name declarator '=' assign_expr;


// Copied from FineSimpleDecl.g4

init_declarator: declarator '(' expr? ')' #initDeclWithCall
               | declarator '=' initializer #initDeclWithAssign
               | declarator #initDeclSimple
               ;

declarator: ptrs? identifier template_args? type_suffix? |
            ptrs? '(' func_ptrs identifier ')' type_suffix;

type_suffix : ('[' conditional_expression? ']') | param_type_list;

// Copied from SimpleDecl.g4

simple_decl : (TYPEDEF?) var_decl;

var_decl : class_def init_declarator_list? #declByClass
         | template_decl* type_name init_declarator_list #declByType
         ;

init_declarator_list: init_declarator (',' init_declarator)* ';';

initializer: assign_expr
           |'{' initializer_list '}'
;

initializer_list: initializer (',' initializer)*;

// Parameters

param_decl_specifiers : (AUTO | REGISTER)? type_name;

// this is a bit misleading. We're just allowing access_specifiers
// here because C programs can use 'public', 'protected' or 'private'
// as variable names.

parameter_name: identifier;

param_type_list: '(' VOID ')'
               | '(' (param_type (',' param_type)*)? ')';

param_type: param_decl_specifiers param_type_id;
param_type_id: (ptrs | rvalue_ref)? ('(' param_type_id ')' | parameter_name?) type_suffix?;
