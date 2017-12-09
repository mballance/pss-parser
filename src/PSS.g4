/****************************************************************************
 * 
 ****************************************************************************/

grammar PSS;
	
model : 
	portable_stimulus_description* 
	;

portable_stimulus_description : 
	package_body_item 
	;
	
package_declaration:
	'package' name=package_identifier '{'
		package_body_item*
	'}'	(';')?
;	

package_body_item:
	abstract_action_declaration 	|
	struct_declaration 				|
	enum_declaration 				|
	coverspec_declaration 			|
	import_method_decl				|
	import_class_decl				|
	import_method_qualifiers		|
	export_action					|
	typedef_declaration 			|
	bins_declaration 				|
	import_stmt 					|
	extend_stmt						|
	package_declaration				|
	component_declaration
;

extend_stmt:
		(
			('extend' ext_type='action' type_identifier '{'
				action_body_item*
				'}' (';')?
			) | 
			('extend' ext_type='struct' type_identifier '{'
				struct_body_item*
				'}' (';')?
			) |
			('extend' ext_type='enum' type_identifier '{'
				(enum_item (',' enum_item)*)?
				'}' (';')?
			) |
			('extend' ext_type='component' type_identifier '{'
				component_body_item*
				'}' (';')?
			)
		)
;

import_stmt:
	'import' package_import_pattern ';'
;

package_import_pattern:
	type_identifier ('::' '*')?
;

/****************************************************************************
 * H1: Action Declarations
 ****************************************************************************/
action_declaration:
	'action' action_identifier (action_super_spec)? 
	'{'
		action_body_item*
	'}' (';')?
;


abstract_action_declaration :
	'abstract' 'action' action_identifier (action_super_spec)?
	'{'
		action_body_item*
	'}' (';')?
;

action_super_spec:
	':' type_identifier
;

action_body_item:
	activity_declaration			|
	overrides_declaration  		|	
	constraint_declaration 		|
	action_field_declaration	|
	bins_declaration   			|
	symbol_declaration			|
	coverspec_declaration		|
	exec_block_stmt
;

activity_declaration: 'activity' '{' activity_stmt* '}' (';')? ;

action_field_declaration:
	action_field_modifier? action_data_declaration
;

action_field_modifier:
	'rand'|io_direction|'lock'|'share'|'action'	
;

io_direction:
	'input' | 'output'
;

/****************************************************************************
 * H2: Exec Blocks
 ****************************************************************************/
exec_block_stmt:
	exec_block |
	target_code_exec_block |
	target_file_exec_block
	;
	
exec_block:
	'exec' exec_kind_identifier '{' exec_body_stmt* '}'
;

exec_kind_identifier:
	'pre_solve' |
	'post_solve' |
	'body' |
	'header' |
	'declaration' |
	'run_start' |
	'run_end' |
	'init'
;	

target_code_exec_block:
	'exec' exec_kind_identifier language_identifier '=' string ';'
;

target_file_exec_block:
	'exec' 'file' filename_string '=' string ';'
;

assign_op:
	'=' | '+=' | '-=' | '<<=' | '>>=' | '|=' | '&='
;

exec_body_stmt:
	expression (assign_op expression)? ';'
;

/****************************************************************************
 * H1: Struct Declarations
 ****************************************************************************/
struct_declaration: struct_type identifier (struct_super_spec)? '{'
		struct_body_item*
	'}' (';')?
;

struct_type:
	(img='struct' | img='buffer' | img='stream' | img='state' | (img='resource' ('[' constant_expression ']')?))
;

struct_super_spec : ':' type_identifier
;

//struct_qualifier:
//	 ('memory' | 'stream' | 'state' | ('resource' ('[' constant_expression ']')?))
//;

struct_body_item:
	constraint_declaration		|
	struct_field_declaration	|
	typedef_declaration			|
	bins_declaration			|
	coverspec_declaration		|
	exec_block_stmt
;

struct_field_declaration:
	struct_field_modifier? data_declaration
;

struct_field_modifier:
	'rand'
;

/****************************************************************************
 * H1: Procedural Interface
 ****************************************************************************/
import_method_decl:
	'import' method_prototype ';'
;

method_prototype:
// Namespace
//	method_return_type method_identifier method_parameter_list_prototype
	method_return_type method_identifier method_parameter_list_prototype
;

method_parameter_list_prototype: 
	'('
		(
			method_parameter (',' method_parameter)*
		)?
	')'
;

method_parameter_list: 
	'('
	(
		expression (',' expression)*
	)?
	')'
;

// Method qualifiers
import_method_qualifiers:
	import_method_phase_qualifiers |
	import_method_target_template
	;

import_method_phase_qualifiers:
	'import' import_function_qualifiers type_identifier ';'
;

import_method_target_template:
	'import' language_identifier method_prototype '=' string ';'
;

import_function_qualifiers:
	(method_qualifiers (language_identifier)?) |
	language_identifier
;

method_qualifiers: 
	('target'|'solve')
;

method_return_type:
	'void'|data_type
;

method_parameter:
	method_parameter_dir? data_type identifier
;

method_parameter_dir:
	('input'|'output'|'inout')
;

/****************************************************************************
 * H2: Import Class Declaration
 ****************************************************************************/
import_class_decl:
	'import' 'class' import_class_identifier (import_class_extends)? '{'
		import_class_method_decl*
	'}' (';')?
	;
	
import_class_method_decl:
	method_prototype ';'
;

import_class_extends:
	':' type_identifier (',' type_identifier)*
;

/****************************************************************************
 * H2: Export Action
 ****************************************************************************/
export_action:
//	(attributes)?
	'export' (method_qualifiers)? action_type_identifier method_parameter_list_prototype ';'
;

/****************************************************************************
 * H1: Component Declaration
 ****************************************************************************/
component_declaration:
//	(attributes)?
	'component' component_identifier (component_super_spec)? '{'
	component_body_item*
	'}' (';')?
;

component_super_spec :
	':' type_identifier
;

component_body_item:
	overrides_declaration			|
	component_field_declaration		|
	action_declaration 				|
	object_bind_stmt				|
//	inline_type_object_declaration	|
	exec_block						|
	package_body_item
;

component_field_declaration:
	component_data_declaration |
	component_pool_declaration
;

component_data_declaration:
	data_declaration
;

component_pool_declaration:
	'pool' ('[' expression ']')? data_declaration
;

component_field_modifier:
	('pool')
;

//inline_type_object_declaration:
//	'pool' struct_qualifier 'struct' identifier (':' [struct_declaration|struct_identifier])? '{'
//		struct_body_item*
//	'}' (';')?
//;

object_bind_stmt:
	'bind' hierarchical_id object_bind_item_or_list ';'
;

object_bind_item_or_list:
	component_path | '{' component_path (',' component_path)* '}'
;

component_path:
	 (component_identifier ('.' component_path_elem)*) |
	'*'
; 

component_path_elem:
	component_action_identifier|'*'
;

/********************************************************************
 * H1: Activity-Graph Statements
 ********************************************************************/
activity_stmt: 
	activity_if_else_stmt		
	| activity_repeat_stmt 			//*	
	| activity_constraint_stmt			
	| activity_foreach_stmt				
	| activity_action_traversal_stmt //*
	| activity_sequence_block_stmt	//*
	| activity_select_stmt			//*	
	| activity_parallel_stmt		//*
	| activity_schedule_stmt				
	| activity_bind_stmt					
//	| graph_method_call_stmt
;

/*
graph_method_call_stmt:
	(variable_ref assign_op)? method_function_call ';'
;
*/

activity_bind_stmt:
	'bind' hierarchical_id activity_bind_item_or_list ';'
;

activity_bind_item_or_list:
	hierarchical_id | ('{' hierarchical_id (',' hierarchical_id)* '}')
;

activity_if_else_stmt:
	'if' '(' expression ')' activity_stmt 
	('else' activity_stmt)?
;

activity_select_stmt:
	'select' '{'
		activity_labeled_stmt 
		activity_labeled_stmt
		activity_labeled_stmt*
	'}'
;

// TODO: allow action array elements to be traversed
activity_action_traversal_stmt:
	 (
		(variable_ref inline_with_constraint?
		) |
		(is_do='do' type_identifier inline_with_constraint?
		)
	)
	';'
;

inline_with_constraint:
	 (
		('with' '{' constraint_body_item* '}') | 
		('with' constant_expression)
	)
;


activity_parallel_stmt:
	 'parallel' '{'
		activity_labeled_stmt*
	'}' (';')?
;

activity_schedule_stmt:
	 'schedule' '{'
		activity_labeled_stmt*
	'}' (';')?
;

//graph_select_production:
//	(identifier /*(':=' graph_select_weight)?*/ ':')? activity_stmt 
//;
//
//graph_select_weight:
//	number |
//	identifier |
//	'(' expression ')'
//;

activity_labeled_stmt:
	(identifier ':')? activity_stmt
;

activity_repeat_stmt:
	 (
		('repeat' is_while='while' '(' expression ')' activity_sequence_block_stmt) |
		('repeat' '(' (loop_var=identifier ':')? expression ')' activity_sequence_block_stmt) |
		('repeat' activity_sequence_block_stmt (is_do_while='while' '(' expression ')' ';')?)
		)
;

activity_constraint_stmt:
	 (
		('constraint' ('{' constraint_body_item* '}' )) | 
		('constraint' (single_stmt_constraint))
	)
;

activity_foreach_stmt:
	'foreach' '(' expression ')' activity_sequence_block_stmt
;

activity_sequence_block_stmt:
	('sequence')? '{'  activity_stmt* '}'
;

symbol_declaration:
	'symbol' identifier ('(' symbol_paramlist ')')? '='
		activity_stmt 
;

symbol_param:
	(data_type) identifier
;

symbol_paramlist:
	 (symbol_param (',' symbol_param)*)?
;

/********************************************************************
 * H1: Overrides
 ********************************************************************/
overrides_declaration:
	 'override' '{' override_stmt* '}'
;

override_stmt:
	type_override | instance_override
;

type_override:
	'type' identifier 'with' type_identifier ';'
;

instance_override:
	'instance' hierarchical_id 'with' identifier ';'
;

/********************************************************************
 * H1: Data Declarations
 ********************************************************************/
data_declaration:
	data_type data_instantiation (',' data_instantiation)* ';' 
;

action_data_declaration :
	action_data_type data_instantiation (',' data_instantiation)* ';' 
;

data_instantiation:
	identifier ('(' coverspec_portmap_list ')')? array_dim? ('=' constant_expression)?
;

array_dim:
//	 '[' (constant_expression (':' constant_expression)?)? ']'
	 '[' constant_expression ']'
;

coverspec_portmap_list:
	 (
		// Name-mapped port binding
		(coverspec_portmap (',' coverspec_portmap)*) |
		// Positional port binding
		(hierarchical_id (',' hierarchical_id)*)
	)?
;

coverspec_portmap:
	'.' identifier '(' hierarchical_id ')'
;

/********************************************************************
 * H1: Data Types
 ********************************************************************/
 
data_type:
	scalar_data_type |
	user_defined_datatype
;

/**
 * BNF: action_data_type ::= scalar_data_type | user_defined_datatype | action_type
 */
action_data_type:
	scalar_data_type |
	user_defined_datatype
;


scalar_data_type:
	chandle_type 	|
	integer_type 	|
	string_type  	|
	bool_type
;

enum_declaration:
  	'enum' enum_identifier '{' 
  		(enum_item (',' enum_item)*)?
  		'}' (';')?
  ;
  
bool_type:
	 'bool'
;

chandle_type:
	 'chandle'
;

enum_item:
	identifier ('=' constant_expression)?
;

user_defined_datatype:
	type_identifier
;

typedef_declaration:
 	'typedef' data_type type_identifier ';' 
;

string_type: 'string';  

integer_type:
	integer_atom_type ('[' 
		expression
		(
			(is_width=':' expression) | 
			(is_range1=',' open_range_value (',' open_range_value)*) | 
			(is_range='..' expression (',' open_range_value)*)
		)?
		']'
	)? 
;

integer_atom_type:
	'int'|'bit'
;

open_range_list:
	open_range_value (',' open_range_value)*
;

open_range_value:
	expression ('..' expression)?
;

/********************************************************************
 * H1: Constraints
 ********************************************************************/
constraint_declaration:
	 
	(
		((is_dynamic='dynamic')? 'constraint' identifier '{' constraint_body_item* '}') |
		('constraint' '{' constraint_body_item* '}') | 
		('constraint' single_stmt_constraint)
	)
;

constraint_body_item:
	expression_constraint_item |
	foreach_constraint_item |
	if_constraint_item		|
	unique_constraint_item
;

/**
 * BNF: expression_constraint_item ::= 
 	expression implicand_constraint_item
 	| expression <kw>;</kw>
 */
expression_constraint_item:
	expression (implicand_constraint_item|';')
;

implicand_constraint_item:
	'->' constraint_set
;

single_stmt_constraint:
	expression_constraint_item |
	unique_constraint_item
;

if_constraint_item:
	'if' '(' expression ')' constraint_set ('else' constraint_set )? 
;

foreach_constraint_item:
	'foreach' '(' expression ')' constraint_set
;

constraint_set:
	constraint_body_item | 
	constraint_block
;

constraint_block:
	 '{' constraint_body_item* '}'
;

unique_constraint_item:
	'unique' '{' hierarchical_id (',' hierarchical_id)* '}' ';'
;

/********************************************************************
 * H1: Bins 
 ********************************************************************/
 bins_declaration:
 	'bins' identifier (variable_identifier)? bin_specification ';'
 ;
 
 bin_specification:
 	bin_specifier (bin_specifier)* (bin_wildcard)?
 ;
 
 bin_specifier:
 	explicit_bin_value |
 	explicit_bin_range |
 	bin_range_divide   |
 	bin_range_size
 ;
 
 explicit_bin_value:
 	'[' constant ']'
 ;
 
 explicit_bin_range:
 	'[' constant '..' constant ']'
 ;
 
 bin_range_divide:
 	explicit_bin_range '/' constant
 ;
 
 bin_range_size:
 	explicit_bin_range ':' constant
 ;
 
 bin_wildcard:
 	'[' '*' ']'
 ;
 
/********************************************************************
 * H1: Coverspec
 ********************************************************************/

coverspec_declaration:
	'coverspec' identifier '(' coverspec_port (',' coverspec_port)* ')' '{'
		coverspec_body_item*
	'}' (';')?
;

coverspec_port:
	data_type identifier
;

coverspec_body_item:
	coverspec_option		|
	coverspec_coverpoint	|
	coverspec_cross			|
//	coverspec_paths			|
	constraint_declaration
;

coverspec_option:
	'option' '.' identifier '=' constant_expression ';'
;

coverspec_coverpoint: (
		(coverpoint_identifier ':' 'coverpoint' coverpoint_target_identifier 
			'{' coverspec_coverpoint_body_item* '}' (';')?) |
		(coverpoint_identifier ':' 'coverpoint' coverpoint_target_identifier ';')
	)
;

coverspec_coverpoint_body_item:
	coverspec_option			|
	coverspec_coverpoint_binspec
;

coverspec_coverpoint_binspec: (
		('bins' identifier bin_specification ';') | 
		('bins' identifier hierarchical_id ';')
	)
;

coverspec_cross_body_item:
	coverspec_option
;

coverspec_cross: (
	(ID ':' 'cross' coverpoint_identifier (',' coverpoint_identifier)*
			'{' coverspec_cross_body_item* '}' (';')? ) |
	(ID ':' 'cross' coverpoint_identifier (',' coverpoint_identifier)* ';')
	)
;

/********************************************************************
 * H1: Expressions
 ********************************************************************/

constant_expression: expression;

expression:
	unary_op lhs=expression								|
	lhs=expression exp_op rhs=expression 				|
	lhs=expression mul_div_mod_op rhs=expression 		|
	lhs=expression add_sub_op rhs=expression			|
	lhs=expression shift_op rhs=expression				|
    lhs=expression logical_inequality_op rhs=expression	|
    lhs=expression eq_neq_op rhs=expression				|
    lhs=expression binary_and_op rhs=expression			|
    lhs=expression binary_xor_op rhs=expression			|
    lhs=expression binary_or_op rhs=expression			|
    lhs=expression logical_and_op rhs=expression		|
    lhs=expression logical_or_op rhs=expression			|
	primary
	;

//condition_expr :
//	cond=logical_or_expr ( '?' true_expr=logical_or_expr ':' false_expr=logical_or_expr)*
//	; 
//
//logical_or_expr :
//	lhs=logical_and_expr ( '||' logical_and_expr)*
//;
logical_or_op : '||';
//
//logical_and_expr :
//	binary_or_expr ( '&&' binary_or_expr)*	
//;
logical_and_op : '&&';
//
//binary_or_expr :
//	binary_xor_expr ( '|' binary_xor_expr)*
//;
binary_or_op : '|';
//
//binary_xor_expr :
//	binary_and_expr ( '^' binary_and_expr)*
//;
binary_xor_op : '^';
//
//binary_and_expr :
//	logical_equality_expr ( '&' logical_equality_expr)*
//;
binary_and_op : '&';
//
//logical_equality_expr :
//	logical_inequality_expr ( eq_neq_op logical_inequality_expr)*
//;
//
//logical_inequality_expr : 
//		binary_shift_expr ( logical_inequality_rhs)*
//;
//
//logical_inequality_rhs :
//	inequality_expr_term | inside_expr_term
//;
//
//inequality_expr_term :
//	logical_inequality_op binary_shift_expr
//;
//
//inside_expr_term :
//	'inside' '[' open_range_list ']'
//;
//
logical_inequality_op:
	'<'|'<='|'>'|'>='
;
//
//binary_shift_expr :
//	binary_add_sub_expr ( shift_op binary_add_sub_expr)*
//;
//
//binary_add_sub_expr :
//	binary_mul_div_mod_expr ( add_sub_op binary_mul_div_mod_expr)*
//;
//
//binary_mul_div_mod_expr :
//	binary_exp_expr ( mul_div_mod_op binary_exp_expr)*
//;
//
//binary_exp_expr :
//	unary_expr ( '**' unary_expr)*
//;

unary_op: '+' | '-' | '!' | '~' | '&' | '|' | '^';

exp_op: '**';

//
eq_neq_op: '==' | '!=';
//
shift_op: '<<' | '>>';
//
add_sub_op: '+' | '-';
//
mul_div_mod_op: '*' | '/' | '%';
//
//unary_expr :
//	(unary_op)? primary
//;

primary: //; :
	number 					
	| bool_literal			
	| paren_expr
	| string_literal
	| variable_ref_path
	| method_function_call
	;

method_function_call:
	method_call		|
	function_call
;

method_call:
	method_hierarchical_id method_parameter_list
;

method_hierarchical_id :
	identifier '.' identifier ('.' identifier)*
;

function_call:
	function_id method_parameter_list	
;

function_id:
// Namespace
//	ID ('::' ID ('::' ID)?)?
	type_identifier
;

paren_expr:
	'(' expression ')'
;

// TODO: Mantis
variable_ref_path:
	variable_ref ('.' variable_ref)*
;

variable_ref:
	identifier ('[' expression (':' expression)? ']')?
//	hierarchical_id ('[' expression (':' expression)? ']')?
;



/********************************************************************
 * H1: Identifiers and Literals
 ********************************************************************/
action_identifier: identifier;
struct_identifier: identifier;
component_identifier: identifier;
component_action_identifier: identifier;
coverpoint_identifier : identifier;
enum_identifier: identifier;
import_class_identifier: identifier;
language_identifier: identifier;
method_identifier: identifier;
variable_identifier: identifier;
constant: number | identifier;

coverpoint_target_identifier : hierarchical_id;
parameter_identifier : identifier;

identifier: ID | ESCAPED_ID;

filename_string: DOUBLE_QUOTED_STRING;

// Namespace
package_identifier: type_identifier ;

action_type_identifier: type_identifier;

type_identifier : ID ('::' ID)* ;

hierarchical_id:
	identifier ('.' identifier)*
;

bool_literal:
	'true'|'false'
;

/********************************************************************
 * H1: Numbers
 ********************************************************************/
number:
	based_hex_number 		|
	based_dec_number		|
	based_bin_number		|
	based_oct_number		|
	dec_number			|
	oct_number			|
	hex_number
;

based_hex_number: DEC_LITERAL? BASED_HEX_LITERAL;
BASED_HEX_LITERAL: '\'' ('s'|'S')? ('h'|'H') ('0'..'9'|'a'..'f'|'A'..'F') ('0'..'9'|'a'..'f'|'A'..'F'|'_')*;

based_dec_number: DEC_LITERAL? BASED_DEC_LITERAL;
BASED_DEC_LITERAL: '\'' ('s'|'S')? ('d'|'D') ('0'..'9') ('0'..'9'|'_')*;

dec_number: DEC_LITERAL;
DEC_LITERAL: ('1'..'9') ('0'..'9'|'_')*;

based_bin_number: DEC_LITERAL? BASED_BIN_LITERAL;
BASED_BIN_LITERAL: '\'' ('s'|'S')? ('b'|'B') (('0'..'1') ('0'..'1'|'_')*);

based_oct_number: DEC_LITERAL? BASED_OCT_LITERAL;
BASED_OCT_LITERAL: '\'' ('s'|'S')? ('o'|'O') (('0'..'7') ('0'..'7'|'_')*);


oct_number: OCT_LITERAL;
OCT_LITERAL: '0' ('0'..'7')*;

hex_number: HEX_LITERAL;
HEX_LITERAL: '0x' ('0'..'9'|'a'..'f'|'A'..'F') ('0'..'9'|'a'..'f'|'A'..'F'|'_')*;


/********************************************************************
 * H1: Comments
 ********************************************************************/

WS : [ \t\n\r]+ -> channel (HIDDEN) ;
 
/**
 * BNF: SL_COMMENT ::= <kw>//</kw>\n 
 */
SL_COMMENT 	: '//' .*? '\r'? '\n' -> channel (HIDDEN) ;

/*
 * BNF: ML_COMMENT ::= <kw>/*</kw><kw>*\057</kw>
 */
ML_COMMENT	: '/*' .*? '*/' -> channel (HIDDEN) ;

string: DOUBLE_QUOTED_STRING|TRIPLE_DOUBLE_QUOTED_STRING;

string_literal: value=string;

DOUBLE_QUOTED_STRING	: '"' (~ [\n\r])* '"' ;

/**
 * BNF: TRIPLE_DOUBLE_QUOTED_STRING ::= <kw>"""</kw><kw>"""</kw>
 */
TRIPLE_DOUBLE_QUOTED_STRING:
			'"""' ('\u0009'..'\u000d' '\u0020'..'\u007e')* '"""'
		; 

ID : [a-zA-Z_] [a-zA-Z0-9_]* ;

ESCAPED_ID : '\\' ('\u0021'..'\u007E')+ ~ [ \r\t\n]* ;





