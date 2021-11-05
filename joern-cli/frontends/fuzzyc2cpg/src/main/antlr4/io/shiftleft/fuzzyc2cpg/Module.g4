grammar Module;

import ModuleLex, Common;

/*
    Copyright (C) 2013 Fabian 'fabs' Yamaguchi <fabs@phenoelit.de>
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

code : (function_decl | function_def | simple_decl | using_directive | water)*;

using_directive: USING NAMESPACE identifier ';';

function_decl: ('extern'? | template_decl*) return_type? function_name function_param_list ctor_list? ';';

function_def: template_decl* return_type? function_name function_param_list ctor_list? compound_statement;

return_type : (function_decl_specifiers* type_name) ptr_operator*;

function_param_list : '(' parameter_decl_clause? ')' CV_QUALIFIER* exception_specification?;

parameter_decl_clause: (parameter_decl (',' parameter_decl)*) (',' '...')?
                     | VOID;
parameter_ptrs: ptrs | rvalue_ref;
parameter_decl: param_decl_specifiers parameter_id |
                param_decl_specifiers parameter_ptrs?;
parameter_id: parameter_ptrs? ('(' parameter_id ')' | parameter_name) type_suffix? default_parameter_value?;
default_parameter_value: '=' expr;

compound_statement: OPENING_CURLY { skipToEndOfObject(); };

ctor_list: ':'  ctor_initializer (',' ctor_initializer)*;
ctor_initializer:  initializer_id ctor_expr;
initializer_id : '::'? identifier;
ctor_expr:  '(' expr? ')';

function_name: '(' function_name ')' | identifier | OPERATOR operator;

exception_specification : THROW '(' type_id_list ')';
type_id_list: no_brackets* ('(' type_id_list ')' no_brackets*)*;



// The following two contain 'water'-rules for expressions

init_declarator : declarator (('(' expr? ')') | ('=' assign_expr_w_))?;
declarator: ptrs? identifier template_args? type_suffix? |
            ptrs? '(' func_ptrs identifier ')' type_suffix;

type_suffix : ('[' constant_expr_w_ ']') | param_type_list;

// water rules for expressions

assign_expr_w_: assign_water*
        (('{' assign_expr_w__l2 '}' | '(' assign_expr_w__l2 ')' | '[' assign_expr_w__l2 ']')
             assign_water*)*;

assign_expr_w__l2: assign_water_l2* (('{' assign_expr_w__l2 '}' | '(' assign_expr_w__l2 ')' | '[' assign_expr_w__l2 ']')
             assign_water_l2*)*;

constant_expr_w_: no_squares* ('[' constant_expr_w_ ']' no_squares*)*;

simple_decl : storage_class_specifier* var_decl;

storage_class_specifier: (EXTERN | TYPEDEF);

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
param_type_id: ptrs? ('(' param_type_id ')' | parameter_name?) type_suffix?;
