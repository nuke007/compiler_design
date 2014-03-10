%{
	#include<stdio.h>
	#include<string.h>
	#include<stdlib.h>
	extern int line_num;
	struct symbol_table
	{
		int line;
		char id[20];
		char type[100];
		int isarray;
		int size;
		char initvalue[1000];
	}st[1000],dm[100];
	int en = 0,dmn=0,eflag=0;
	int isarray = 0,size = -1;
	int sgn=-1,ls=-1,icfd=-1,ss=-1;
	
	//char 1
	//short 2
	//int 3
	//long 4
	//float 5
	//double 6
	//signed 7
	//unsigned 8
%}
%token AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN 
%token FLOAT FOR GOTO IF INT LONG REGISTER RETURN SHORT SIGNED SIZEOF STATIC 
%token STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE PLUS MINUS THREE_DOT
%token DIV MULT MOD EQUAL LESS GREATER NOT B_AND B_OR B_XOR B_NOT INCREMENT DOT
%token DECREMENT PLUS_EQUAL MINUS_EQUAL MULT_EQUAL DIV_EQUAL MOD_EQUAL EQUAL_EQUAL 
%token LESS_EQUAL GREATER_EQUAL NOT_EQUAL AND_EQUAL OR_EQUAL XOR_EQUAL AND OR TERNARY
%token LSHIFT RSHIFT LSHIFT_EQUAL RSHIFT_EQUAL LINK OPENING_CURLY_BRACES CLOSING_CURLY_BRACES 
%token OPENING_PARANTHESES CLOSING_PARANTHESES OPENING_SQUARE_BRACKET CLOSING_SQUARE_BRACKET 
%token SEMICOLON COLON COMMA IDENTIFIER NUMBER REAL_NUMBER EXP_NUMBER CHAR_CONSTANT STRING_CONSTANT

%union{
		int ival;
		char cval[1000];
	   }
%type <cval> direct_declarator IDENTIFIER declarator NUMBER CHAR_CONSTANT REAL_NUMBER EXP_NUMBER
constant primary_expression postfix_expression unary_expression cast_expression multiplicative_expression additive_expression shift_expression relational_expression equality_expression AND_expression exclusive_OR_expression inclusive_OR_expression logical_AND_expression logical_OR_expression conditional_expression assignment_expression initializer constant_expression initializer_list 

%%
translation_unit:external_declaration {printf("\tReduced : translation_unit -> external_declaration\n");}
				|translation_unit external_declaration {printf("\tReduced : translation_unit -> translation_unit external_declaration\n");}
				;

external_declaration:function_definition {printf("\tReduced : external_declaration -> function_definition\n");}
					|declaration {printf("\tReduced : external_declaration -> declaration\n");}
					;

function_definition:declaration_specifiers declarator declaration_list compound_statement {printf("\tReduced : function_definition -> declaration_specifiers declarator declaration_list compound_statement\n");}
				   |declarator declaration_list compound_statement {printf("\tReduced : function_definition -> declarator declaration_list compound_statement\n");}
				   |declaration_specifiers declarator compound_statement {printf("\tReduced : function_definition -> declaration_specifiers declarator compound_statement\n");}
				   |declarator compound_statement {printf("\tReduced : function_definition -> declarator compound_statement\n");}
				   ;

declaration:declaration_specifiers init_declarator_list SEMICOLON {
																	int l=0,pen=en,i;
	char dtype[101]="                                                                                                    ";
	char a1[2][10]={"signed ","unsigned "};
	char a2[2][10]={"short ","long "};
	char a3[4][10]={"char ","int ","float ","double "};
	char a4[5][10]={"auto ","register ","static ","extern ","typedef "};
	if((icfd==0||icfd==2)&&ls!=-1) yyerror();
	else if(icfd==3&&(ls==0||ls==2)) yyerror();
	else if(sgn!=-1&&(icfd==2||icfd==3)) yyerror();
	if(ss!=-1)
	{
		strcpy(dtype,a4[ss]);
		l+=strlen(a4[ss]);
	}
	if(sgn!=-1)
	{
		strcpy(dtype,a1[sgn]);
		l+=strlen(a1[sgn]);
	}
	if(ls!=-1)
	{
		if(ls==2)
		{
			strcpy(dtype+l,a2[1]);
			l+=strlen(a2[1]);
			strcpy(dtype+l,a2[1]);
			l+=strlen(a2[1]);
		}
		else
		{
			strcpy(dtype+l,a2[ls]);
			l+=strlen(a2[ls]);
		}
	}
	if(icfd!=-1)
	{
		strcpy(dtype+l,a3[icfd]);
		l+=strlen(a3[icfd]);
	}
	dtype[l]='\0';
	sgn=ls=icfd=ss=-1;
																	for(i=0;i<dmn;++i)
																	{
																		if(!idpresent(dm[i].id))
																		{
																			st[pen].line = line_num;
																			strcpy(st[pen].id,dm[i].id);
																			strcpy(st[pen].type,dtype);
																			st[pen].isarray = dm[i].isarray;
																			st[pen].size = dm[i].size;
																			strcpy(st[pen].initvalue,dm[i].initvalue);
																			++pen;
																		}
																	}
																	dmn=0;
																	en=pen;
																	printf("\tReduced : declaration -> declaration_specifiers init_declarator_list SEMICOLON\n");}
		   |declaration_specifiers SEMICOLON { printf("\tReduced : declaration -> declaration_specifiers SEMICOLON\n"); }
		   ;

declaration_list:declaration {printf("\tReduced : declaration_list -> declaration\n");}
				|declaration_list declaration {printf("\tReduced : declaration_list -> declaration_list declaration\n");}
				;

declaration_specifiers:storage_class_specifier declaration_specifiers {printf("\tReduced : declaration_specifiers -> storage_class_specifier declaration_specifiers\n");}
					  |storage_class_specifier {printf("\tReduced : declaration_specifiers -> storage_class_specifier\n");}
					  |type_specifier declaration_specifiers { printf("\tReduced : declaration_specifiers -> type_specifier declaration_specifiers\n");}
					  |type_specifier { printf("\tReduced : declaration_specifiers -> type_specifier\n");}
					  |type_qualifier declaration_specifiers {printf("\tReduced : declaration_specifiers -> type_qualifier declaration_specifiers\n");}
					  |type_qualifier {printf("\tReduced : declaration_specifiers -> type_qualifier\n");}
					  ;

storage_class_specifier:AUTO {
								if(ss==-1)ss=0;
								else yyerror();
								printf("\tReduced : storage_class_specifier -> AUTO\n");}
					   |REGISTER {
									if(ss==-1)ss=1;
									else yyerror();
									printf("\tReduced : storage_class_specifier -> REGISTER\n");}
					   |STATIC {
									if(ss==-1)ss=2;
									else yyerror();
									printf("\tReduced : storage_class_specifier -> STATIC\n");}
					   |EXTERN {
									if(ss==-1)ss=3;
									else yyerror();
									printf("\tReduced : storage_class_specifier -> EXTERN\n");}
					   |TYPEDEF {
									if(ss==-1)ss=4;
									else yyerror();
									printf("\tReduced : storage_class_specifier -> TYPEDEF\n");}
					   ;

type_specifier:VOID { printf("\tReduced : type_specifier -> VOID\n");}
			  |CHAR { 
						if(icfd==-1)icfd = 0;
						else yyerror();
						printf("\tReduced : type_specifier -> CHAR\n"); }
			  |SHORT { 
						if(ls==-1)ls=0;
						else yyerror();
						printf("\tReduced : type_specifier -> SHORT\n"); }
			  |INT { 
					 if(icfd==-1)icfd=1;
					 else yyerror();
					 printf("\tReduced : type_specifier -> INT\n");}
			  |LONG { 
						if(ls==-1)ls=1;
						else if(ls==1)ls=2;
						else yyerror();
						printf("\tReduced : type_specifier -> LONG\n"); }
			  |FLOAT { 
						if(icfd==-1)icfd=2;
						else yyerror();
						printf("\tReduced : type_specifier -> FLOAT\n"); }
			  |DOUBLE { 
						if(icfd==-1)icfd=3;
						else yyerror();
						printf("\tReduced : type_specifier -> DOUBLE\n"); }
			  |SIGNED { 
						if(sgn==-1)sgn=0;
						else yyerror();
						printf("\tReduced : type_specifier -> SIGNED\n"); }
			  |UNSIGNED { 
						if(sgn==-1)sgn=1;
						else yyerror();
						printf("\tReduced : type_specifier -> UNSIGNED\n"); }
			  |struct_or_union_specifier {printf("\tReduced : type_specifier -> struct_or_union_specifier\n");}
			  |enum_specifier {printf("\tReduced : type_specifier -> enum_specifier\n");}
			  ;

type_qualifier:CONST {printf("\tReduced : type_qualifier -> CONST\n");}
			  |VOLATILE {printf("\tReduced : type_qualifier -> VOLATILE\n");}
			  ;

struct_or_union_specifier:struct_or_union IDENTIFIER OPENING_CURLY_BRACES struct_declaration_list CLOSING_CURLY_BRACES {printf("\tReduced : struct_or_union_specifier -> struct_or_union IDENTIFIER OPENING_CURLY_BRACES struct_declaration_list CLOSING_CURLY_BRACES\n");}
						 |struct_or_union OPENING_CURLY_BRACES struct_declaration_list CLOSING_CURLY_BRACES {printf("\tReduced : struct_or_union_specifier -> struct_or_union OPENING_CURLY_BRACES struct_declaration_list CLOSING_CURLY_BRACES\n");}
						 |struct_or_union IDENTIFIER {printf("\tReduced : struct_or_union_specifier -> struct_or_union IDENTIFIER\n");}
						 ;
struct_or_union:STRUCT {printf("\tReduced : struct_or_union -> STRUCT\n");}
			   |UNION {printf("\tReduced : struct_or_union -> UNION\n");}
			   ;
			   
struct_declaration_list:struct_declaration {printf("\tReduced : struct_declaration_list -> struct_declaration\n");}
					   |struct_declaration_list struct_declaration {printf("\tReduced : struct_declaration_list -> struct_declaration_list struct_declaration\n");}
					   ;

init_declarator_list:init_declarator { printf("\tReduced : init_declarator_list -> init_declarator\n");}
					|init_declarator_list COMMA init_declarator {printf("\tReduced : init_declarator_list -> init_declarator_list COMMA init_declarator\n");}
					;

init_declarator:declarator { 
								strcpy(dm[dmn].id,$1);
								memset(dm[dmn].initvalue,0,sizeof(dm[dmn].initvalue));
								if(isarray)
								{
									if(dm[dmn].size==0) dm[dmn].size = size;
								}
								else dm[dmn].size=-1;
								dm[dmn].isarray = isarray;
								isarray = 0;
								size = -1;
								++dmn; 
								printf("\tReduced : init_declarator -> declarator\n");}
			   |declarator EQUAL initializer {
												strcpy(dm[dmn].id,$1);
												strcpy(dm[dmn].initvalue,$3);
												if(isarray)
												{
													if(dm[dmn].size==0) dm[dmn].size = size;
												}
												else dm[dmn].size = -1;
												dm[dmn].isarray = isarray;
												isarray = 0;
												size = -1;
												++dmn;
												printf("\tReduced : init_declarator -> declarator EQUAL initializer\n");}
			   ;
			   
struct_declaration:specifier_qualifier_list struct_declarator_list SEMICOLON {printf("\tReduced : struct_declaration -> specifier_qualifier_list struct_declarator_list SEMICOLON\n");}
				  ;

specifier_qualifier_list:type_specifier specifier_qualifier_list {printf("\tReduced : specifier_qualifier_list -> type_specifier specifier_qualifier_list\n");}
						|type_specifier {printf("\tReduced : specifier_qualifier_list -> type_specifier\n");}
						|type_qualifier specifier_qualifier_list {printf("\tReduced : specifier_qualifier_list -> type_qualifier specifier_qualifier_list\n");}
						|type_qualifier {printf("\tReduced : specifier_qualifier_list -> type_qualifier\n");}
						;

struct_declarator_list:struct_declarator {printf("\tReduced : struct_declarator_list -> struct_declarator\n");}
					  |struct_declarator_list COMMA struct_declarator {printf("\tReduced : struct_declarator_list -> struct_declarator_list COMMA struct_declarator\n");}

struct_declarator:declarator {printf("\tReduced : struct_declarator -> declarator\n");}
				 |declarator COLON constant_expression {printf("\tReduced : struct_declarator -> declarator COLON constant_expression\n");}
				 |COLON constant_expression {printf("\tReduced : struct_declarator -> COLON constant_expression\n");}
				 ;

enum_specifier:ENUM IDENTIFIER OPENING_CURLY_BRACES enumerator_list CLOSING_CURLY_BRACES {printf("\tReduced : enum_specifier -> ENUM IDENTIFIER OPENING_CURLY_BRACES enumerator_list CLOSING_CURLY_BRACES\n");}
			  |ENUM OPENING_CURLY_BRACES enumerator_list CLOSING_CURLY_BRACES {printf("\tReduced : enum_specifier -> ENUM OPENING_CURLY_BRACES enumerator_list CLOSING_CURLY_BRACES\n");}
			  |ENUM IDENTIFIER {printf("\tReduced : enum_specifier -> ENUM IDENTIFIER\n");}
			  ;

enumerator_list:enumerator {printf("\tReduced : enumerator_list -> enumerator\n");}
			   |enumerator_list COMMA enumerator {printf("\tReduced : enumerator_list -> enumerator_list COMMA enumerator\n");}
			   ;

enumerator:IDENTIFIER {printf("\tReduced : enumerator -> IDENTIFIER\n");}
		  |IDENTIFIER EQUAL constant_expression {printf("\tReduced : enumerator -> IDENTIFIER EQUAL constant_expression\n");}
		  ;

declarator:pointer direct_declarator { printf("\tReduced : declarator -> pointer direct_declarator\n");}
		  |direct_declarator { strcpy($$,$1); printf("\tReduced : declarator -> direct_declarator\n");}
		  ;

direct_declarator:IDENTIFIER { strcpy($$,$1); printf("\tReduced : direct_declarator -> IDENTIFIER\n");}
				 |OPENING_PARANTHESES declarator CLOSING_PARANTHESES {printf("\tReduced : direct_declarator -> OPENING_PARANTHESES declarator CLOSING_PARANTHESES\n");}
				 |direct_declarator OPENING_SQUARE_BRACKET constant_expression CLOSING_SQUARE_BRACKET {
																										dm[dmn].size = atoi($3);
																										isarray = 1;
																										printf("\tReduced : direct_declarator -> direct_declarator OPENING_SQUARE_BRACKET constant_expression CLOSING_SQUARE_BRACKET\n");
																										}
				 |direct_declarator OPENING_SQUARE_BRACKET CLOSING_SQUARE_BRACKET { 
																					isarray = 1;
																					size = 0;
																					dm[dmn].size = 0; 
																					printf("\tReduced : direct_declarator -> direct_declarator OPENING_SQUARE_BRACKET CLOSING_SQUARE_BRACKET\n");}
				 |direct_declarator OPENING_PARANTHESES parameter_type_list CLOSING_PARANTHESES { sgn=ls=icfd=ss=-1; printf("\tReduced : direct_declarator -> direct_declarator OPENING_PARANTHESES parameter_type_list CLOSING_PARANTHESES\n");}
				 |direct_declarator OPENING_PARANTHESES identifier_list CLOSING_PARANTHESES { sgn=ls=icfd=ss=-1; printf("\tReduced : direct_declarator -> direct_declarator OPENING_PARANTHESES identifier_list CLOSING_PARANTHESES\n");}
				 |direct_declarator OPENING_PARANTHESES CLOSING_PARANTHESES { sgn=ls=icfd=ss=-1; printf("\tReduced : direct_declarator -> direct_declarator OPENING_PARANTHESES CLOSING_PARANTHESES\n");}
				 ;

pointer:MULT type_qualifier_list {printf("\tReduced : pointer -> MULT type_qualifier_list\n");}
	   |MULT {printf("\tReduced : pointer -> MULT\n");}
	   |MULT type_qualifier_list pointer {printf("\tReduced : pointer -> MULT type_qualifier_list pointer\n");}
	   |MULT pointer {printf("\tReduced : pointer -> MULT pointer\n");}
	   ;

type_qualifier_list:type_qualifier {printf("\tReduced : type_qualifier_list -> type_qualifier\n");}
				   |type_qualifier_list type_qualifier {printf("\tReduced : type_qualifier_list -> type_qualifier_list type_qualifier\n");}
				   ;

parameter_type_list:parameter_list {printf("\tReduced : parameter_type_list -> parameter_list\n");}
				   |parameter_list COMMA THREE_DOT {printf("\tReduced : parameter_type_list -> parameter_list COMMA THREE_DOT\n");}
				   ;

parameter_list:parameter_declaration {printf("\tReduced : parameter_list -> parameter_declaration\n");}
			  |parameter_list COMMA parameter_declaration {printf("\tReduced : parameter_list -> parameter_list COMMA parameter_declaration\n");}
			  ;

parameter_declaration:declaration_specifiers declarator {printf("\tReduced : parameter_declaration -> declaration_specifiers declarator\n");}
					 |declaration_specifiers abstract_declarator {printf("\tReduced : parameter_declaration -> declaration_specifiers abstract_declarator\n");}
					 |declaration_specifiers {printf("\tReduced : parameter_declaration -> declaration_specifiers\n");}
					 ;

identifier_list:IDENTIFIER {printf("\tReduced : identifier_list -> IDENTIFIER\n");}
			   |identifier_list COMMA IDENTIFIER {printf("\tReduced : identifier_list -> identifier_list COMMA IDENTIFIER\n");}
			   ;

initializer:assignment_expression { strcpy($$,$1); printf("\tReduced : initializer -> assignment_expression\n");}
		   |OPENING_CURLY_BRACES initializer_list CLOSING_CURLY_BRACES { strcpy($$,$2); printf("\tReduced : initializer -> OPENING_CURLY_BRACES initializer_list CLOSING_CURLY_BRACES\n");}
		   |OPENING_CURLY_BRACES initializer_list COMMA CLOSING_CURLY_BRACES { strcpy($$,$2); printf("\tReduced : initializer -> OPENING_CURLY_BRACES initializer_list COMMA CLOSING_CURLY_BRACES\n");}
		   ;

initializer_list:initializer { strcpy($$,$1); ++size; printf("\tReduced : initializer_list -> initializer\n");}
				|initializer_list COMMA initializer {   
														int l;
														++size;
														memset($$,' ',sizeof($$));
														l = strlen($1);
														strcpy($1+l," ");
														++l;
														strcpy($1+l,$3);
														l+=strlen($3);
														$1[l]='\0';
														strcpy($$,$1);
														printf("\tReduced : initializer_list -> initializer_list COMMA initializer\n"); }
				;

type_name:specifier_qualifier_list abstract_declarator {printf("\tReduced : type_name -> specifier_qualifier_list abstract_declarator\n");}
		 |specifier_qualifier_list {printf("\tReduced : type_name -> specifier_qualifier_list\n");}
		 ;

abstract_declarator:pointer {printf("\tReduced : abstract_declarator ->  pointer\n");}
				   |pointer direct_abstract_declarator {printf("\tReduced : abstract_declarator ->  pointer direct_abstract_declarator\n");}
				   |direct_abstract_declarator {printf("\tReduced : abstract_declarator -> direct_abstract_declarator\n");}
				   ;

direct_abstract_declarator:OPENING_PARANTHESES abstract_declarator CLOSING_PARANTHESES {printf("\tReduced : direct_abstract_declarator -> OPENING_PARANTHESES abstract_declarator CLOSING_PARANTHESES\n");}
						  |direct_abstract_declarator OPENING_SQUARE_BRACKET constant_expression CLOSING_SQUARE_BRACKET {printf("\tReduced : direct_abstract_declarator -> direct_abstract_declarator OPENING_SQUARE_BRACKET constant_expression CLOSING_SQUARE_BRACKET\n");}
						  |OPENING_SQUARE_BRACKET constant_expression CLOSING_SQUARE_BRACKET {printf("\tReduced : direct_abstract_declarator -> OPENING_SQUARE_BRACKET constant_expression CLOSING_SQUARE_BRACKET\n");}
						  |direct_abstract_declarator OPENING_SQUARE_BRACKET CLOSING_SQUARE_BRACKET {printf("\tReduced : direct_abstract_declarator -> direct_abstract_declarator OPENING_SQUARE_BRACKET CLOSING_SQUARE_BRACKET\n");}
						  |OPENING_SQUARE_BRACKET CLOSING_SQUARE_BRACKET {printf("\tReduced : direct_abstract_declarator -> OPENING_SQUARE_BRACKET CLOSING_SQUARE_BRACKET\n");}
						  |direct_abstract_declarator OPENING_PARANTHESES parameter_type_list CLOSING_PARANTHESES {printf("\tReduced : direct_abstract_declarator -> direct_abstract_declarator OPENING_PARANTHESES parameter_type_list CLOSING_PARANTHESES\n");}
						  |OPENING_PARANTHESES parameter_type_list CLOSING_PARANTHESES {printf("\tReduced : direct_abstract_declarator -> OPENING_PARANTHESES parameter_type_list CLOSING_PARANTHESES\n");}
						  |direct_abstract_declarator OPENING_PARANTHESES CLOSING_PARANTHESES {printf("\tReduced : direct_abstract_declarator -> direct_abstract_declarator OPENING_PARANTHESES CLOSING_PARANTHESES\n");}
						  |OPENING_PARANTHESES CLOSING_PARANTHESES {printf("\tReduced : direct_abstract_declarator -> OPENING_PARANTHESES CLOSING_PARANTHESES\n");}
						  ;


statement:labeled_statement {printf("\tReduced : statement -> labeled_statement\n");}
		 |expression_statement {printf("\tReduced : statement -> expression_statement\n");}
		 |compound_statement {printf("\tReduced : statement -> compound_statement\n");}
		 |selection_statement {printf("\tReduced : statement -> selection_statement\n");}
		 |iteration_statement {printf("\tReduced : statement -> iteration_statement\n");}
		 |jump_statement {printf("\tReduced : statement -> jump_statement\n");}
		 ;

labeled_statement:IDENTIFIER COLON statement {printf("\tReduced : labeled_statement -> IDENTIFIER COLON statement\n");}
				 |CASE constant_expression COLON statement {printf("\tReduced : labeled_statement -> CASE constant_expression COLON statement\n");}
				 |DEFAULT COLON statement {printf("\tReduced : labeled_statement -> DEFAULT COLON statement\n");}
				 ;

expression_statement:expression SEMICOLON {printf("\tReduced : expression_statement -> expression SEMICOLON\n");}
					|SEMICOLON {printf("\tReduced : expression_statement -> SEMICOLON\n");}
					;

compound_statement:OPENING_CURLY_BRACES declaration_list statement_list CLOSING_CURLY_BRACES {printf("\tReduced : compound_statement -> OPENING_CURLY_BRACES declaration_list statement_list CLOSING_CURLY_BRACES\n");}
				  |OPENING_CURLY_BRACES declaration_list CLOSING_CURLY_BRACES {printf("\tReduced : compound_statement -> OPENING_CURLY_BRACES declaration_list CLOSING_CURLY_BRACES\n");}
				  |OPENING_CURLY_BRACES statement_list CLOSING_CURLY_BRACES {printf("\tReduced : compound_statement -> OPENING_CURLY_BRACES statement_list CLOSING_CURLY_BRACES\n");}
				  |OPENING_CURLY_BRACES CLOSING_CURLY_BRACES {printf("\tReduced : compound_statement -> OPENING_CURLY_BRACES CLOSING_CURLY_BRACES\n");}
				  ;

statement_list:statement {printf("\tReduced : statement_list -> statement\n");}
			  |statement_list statement {printf("\tReduced : statement_list -> statement_list statement\n");}
			  ;

selection_statement:IF OPENING_PARANTHESES expression CLOSING_PARANTHESES statement {printf("\tReduced : selection_statement -> IF OPENING_PARANTHESES expression CLOSING_PARANTHESES statement\n");}
				   |IF OPENING_PARANTHESES expression CLOSING_PARANTHESES statement ELSE statement {printf("\tReduced : selection_statement -> IF OPENING_PARANTHESES expression CLOSING_PARANTHESES statement ELSE statement\n");}
				   |SWITCH OPENING_PARANTHESES expression CLOSING_PARANTHESES statement {printf("\tReduced : selection_statement -> SWITCH OPENING_PARANTHESES expression CLOSING_PARANTHESES statement\n");}
				   ;

iteration_statement:WHILE OPENING_PARANTHESES expression CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> WHILE OPENING_PARANTHESES expression CLOSING_PARANTHESES statement\n");}
				   |DO statement WHILE OPENING_PARANTHESES expression CLOSING_PARANTHESES SEMICOLON {printf("\tReduced : iteration_statement -> DO statement WHILE OPENING_PARANTHESES expression CLOSING_PARANTHESES SEMICOLON\n");}
				   |FOR OPENING_PARANTHESES expression SEMICOLON expression SEMICOLON expression CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> FOR OPENING_PARANTHESES expression SEMICOLON expression SEMICOLON expression CLOSING_PARANTHESES statement\n");}
				   |FOR OPENING_PARANTHESES expression SEMICOLON expression SEMICOLON CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> FOR OPENING_PARANTHESES expression SEMICOLON expression SEMICOLON CLOSING_PARANTHESES statement\n");}
				   |FOR OPENING_PARANTHESES expression SEMICOLON SEMICOLON expression CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> FOR OPENING_PARANTHESES expression SEMICOLON SEMICOLON expression CLOSING_PARANTHESES statement\n");}
				   |FOR OPENING_PARANTHESES expression SEMICOLON SEMICOLON CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> FOR OPENING_PARANTHESES expression SEMICOLON SEMICOLON CLOSING_PARANTHESES statement\n");}
				   |FOR OPENING_PARANTHESES SEMICOLON expression SEMICOLON expression CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> FOR OPENING_PARANTHESES SEMICOLON expression SEMICOLON expression CLOSING_PARANTHESES statement\n");}
				   |FOR OPENING_PARANTHESES SEMICOLON expression SEMICOLON CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> FOR OPENING_PARANTHESES SEMICOLON expression SEMICOLON CLOSING_PARANTHESES statement\n");}
				   |FOR OPENING_PARANTHESES SEMICOLON SEMICOLON expression CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> FOR OPENING_PARANTHESES SEMICOLON SEMICOLON expression CLOSING_PARANTHESES statement\n");}
				   |FOR OPENING_PARANTHESES SEMICOLON SEMICOLON CLOSING_PARANTHESES statement {printf("\tReduced : iteration_statement -> FOR OPENING_PARANTHESES SEMICOLON SEMICOLON CLOSING_PARANTHESES statement\n");}
				   ;
				   
jump_statement:GOTO IDENTIFIER SEMICOLON {printf("\tReduced : jump_statement -> GOTO IDENTIFIER SEMICOLON\n");}
			  |CONTINUE SEMICOLON {printf("\tReduced : jump_statement -> CONTINUE SEMICOLON\n");}
			  |BREAK SEMICOLON {printf("\tReduced : jump_statement -> BREAK SEMICOLON\n");}
			  |RETURN expression SEMICOLON {printf("\tReduced : jump_statement -> RETURN expression SEMICOLON\n");}
			  |RETURN SEMICOLON {printf("\tReduced : jump_statement -> RETURN SEMICOLON\n");}
			  ;

expression:assignment_expression {printf("\tReduced : expression -> assignment_expression\n");}
		  |expression COMMA assignment_expression {printf("\tReduced : expression -> expression COMMA assignment_expression\n");}
		  ;

assignment_expression:conditional_expression {strcpy($$,$1); printf("\tReduced : assignment_expression -> conditional_expression\n");}
					 |unary_expression assignment_operator assignment_expression {printf("\tReduced : assignment_expression -> unary_expression assignment_operator assignment_expression\n");}
					 ;

assignment_operator:EQUAL {printf("\tReduced : assignment_operator -> EQUAL\n");}
				   |MULT_EQUAL {printf("\tReduced : assignment_operator -> MULT_EQUAL\n");}
				   |DIV_EQUAL {printf("\tReduced : assignment_operator -> DIV_EQUAL\n");}
				   |MOD_EQUAL {printf("\tReduced : assignment_operator -> MOD_EQUAL\n");}
				   |PLUS_EQUAL {printf("\tReduced : assignment_operator -> PLUS_EQUAL\n");}
				   |MINUS_EQUAL {printf("\tReduced : assignment_operator -> MINUS_EQUAL\n");}
				   |LSHIFT_EQUAL {printf("\tReduced : assignment_operator -> LSHIFT_EQUAL\n");}
				   |RSHIFT_EQUAL {printf("\tReduced : assignment_operator -> RSHIFT_EQUAL\n");}
				   |AND_EQUAL {printf("\tReduced : assignment_operator -> AND_EQUAL\n");}
				   |XOR_EQUAL {printf("\tReduced : assignment_operator -> XOR_EQUAL\n");}
				   |OR_EQUAL {printf("\tReduced : assignment_operator -> OR_EQUAL\n");}
				   ;

conditional_expression:logical_OR_expression { strcpy($$,$1); printf("\tReduced : conditional_expression -> logical_OR_expression\n");}
					  |logical_OR_expression TERNARY expression COLON conditional_expression {printf("\tReduced : conditional_expression -> logical_OR_expression TERNARY expression COLON conditional_expression\n");}
					  ;
					  
constant_expression:conditional_expression { strcpy($$,$1); printf("\tReduced : constant_expression -> conditional_expression\n");}
				   ;
				   
logical_OR_expression:logical_AND_expression { strcpy($$,$1); printf("\tReduced : logical_OR_expression -> logical_AND_expression\n");}
					 |logical_OR_expression OR logical_AND_expression {printf("\tReduced : logical_OR_expression -> logical_OR_expression OR logical_AND_expression\n");}
					 ;

logical_AND_expression:inclusive_OR_expression { strcpy($$,$1); printf("\tReduced : logical_AND_expression -> inclusive_OR_expression\n");}
					  |logical_AND_expression AND inclusive_OR_expression {printf("\tReduced : logical_AND_expression -> logical_AND_expression AND inclusive_OR_expression\n");}
					  ;
					  
inclusive_OR_expression:exclusive_OR_expression {strcpy($$,$1); printf("\tReduced : inclusive_OR_expression -> exclusive_OR_expression\n");}
					   |inclusive_OR_expression B_OR exclusive_OR_expression {printf("\tReduced : inclusive_OR_expression -> inclusive_OR_expression B_OR exclusive_OR_expression\n");}
					   ;

exclusive_OR_expression:AND_expression { strcpy($$,$1); printf("\tReduced : exclusive_OR_expression -> AND_expression\n");}
					   |exclusive_OR_expression B_XOR AND_expression {printf("\tReduced : exclusive_OR_expression -> exclusive_OR_expression B_XOR AND_expression\n");}
					   ;
					   
AND_expression:equality_expression {strcpy($$,$1); printf("\tReduced : AND_expression -> equality_expression\n");}
			  |AND_expression B_AND equality_expression {printf("\tReduced : AND_expression -> AND_expression B_AND equality_expression\n");}

equality_expression:relational_expression { strcpy($$,$1); printf("\tReduced : equality_expression -> relational_expression\n");}
				   |equality_expression EQUAL_EQUAL relational_expression {printf("\tReduced : equality_expression -> equality_expression EQUAL_EQUAL relational_expression\n");}
				   |equality_expression NOT_EQUAL relational_expression {printf("\tReduced : equality_expression -> equality_expression NOT_EQUAL relational_expression\n");}
				   ;

relational_expression:shift_expression { strcpy($$,$1); printf("\tReduced : relational_expression -> shift_expression\n");}
					 |relational_expression LESS shift_expression {printf("\tReduced : relational_expression -> relational_expression LESS shift_expression\n");}
					 |relational_expression GREATER shift_expression {printf("\tReduced : relational_expression -> relational_expression GREATER shift_expression\n");}
					 |relational_expression LESS_EQUAL shift_expression {printf("\tReduced : relational_expression -> relational_expression LESS_EQUAL shift_expression\n");}
					 |relational_expression GREATER_EQUAL shift_expression {printf("\tReduced : relational_expression -> relational_expression GREATER_EQUAL shift_expression\n");}
					 ;

shift_expression:additive_expression { strcpy($$,$1); printf("\tReduced : shift_expression -> additive_expression\n");}
			    |shift_expression LSHIFT additive_expression {printf("\tReduced : shift_expression -> shift_expression LSHIFT additive_expression\n");}
				|shift_expression RSHIFT additive_expression {printf("\tReduced : shift_expression -> shift_expression RSHIFT additive_expression\n");}
				;
				
additive_expression:multiplicative_expression { strcpy($$,$1); printf("\tReduced : additive_expression -> multiplicative_expression\n");}
				   |additive_expression PLUS multiplicative_expression {printf("\tReduced : additive_expression -> additive_expression PLUS multiplicative_expression\n");}
				   |additive_expression MINUS multiplicative_expression {printf("\tReduced : additive_expression -> additive_expression MINUS multiplicative_expression\n");}
				   ;
				   
multiplicative_expression:cast_expression { strcpy($$,$1); printf("\tReduced : multiplicative_expression -> cast_expression\n");}
						 |multiplicative_expression MULT cast_expression {printf("\tReduced : multiplicative_expression -> multiplicative_expression MULT cast_expression\n");}
						 |multiplicative_expression DIV cast_expression {printf("\tReduced : multiplicative_expression -> multiplicative_expression DIV cast_expression\n");}
						 |multiplicative_expression MOD cast_expression {printf("\tReduced : multiplicative_expression -> multiplicative_expression MOD cast_expression\n");}
						 ;
						 
cast_expression:unary_expression {strcpy($$,$1); printf("\tReduced : cast_expression -> unary_expression\n");}
			   |OPENING_PARANTHESES type_name CLOSING_PARANTHESES cast_expression {printf("\tReduced : cast_expression -> OPENING_PARANTHESES type_name CLOSING_PARANTHESES cast_expression\n");}
			   ;
			   
unary_expression:postfix_expression { strcpy($$,$1); printf("\tReduced : unary_expression -> postfix_expression\n");}
				|INCREMENT unary_expression {printf("\tReduced : unary_expression -> INCREMENT unary_expression\n");}
				|DECREMENT unary_expression {printf("\tReduced : unary_expression -> DECREMENT unary_expression\n");}
				|unary_operator cast_expression {printf("\tReduced : unary_expression -> unary_operator cast_expression\n");}
				|SIZEOF unary_expression {printf("\tReduced : unary_expression -> SIZEOF unary_expression\n");}
				|SIZEOF OPENING_PARANTHESES type_name CLOSING_PARANTHESES {printf("\tReduced : unary_expression -> SIZEOF OPENING_PARANTHESES type_name CLOSING_PARANTHESES\n");}
				;
				
unary_operator:B_AND {printf("\tReduced : unary_operator -> B_AND\n");}
			  |MULT {printf("\tReduced : unary_operator -> MULT\n");}
			  |PLUS {printf("\tReduced : unary_operator -> PLUS\n");}
			  |MINUS {printf("\tReduced : unary_operator -> MINUS\n");}
			  |B_NOT {printf("\tReduced : unary_operator -> B_NOT\n");}
			  |NOT {printf("\tReduced : unary_operator -> NOT\n");}
			  ;

postfix_expression:primary_expression { strcpy($$,$1); printf("\tReduced : postfix_expression -> primary_expression\n");}
				  |postfix_expression OPENING_SQUARE_BRACKET expression CLOSING_SQUARE_BRACKET {printf("\tReduced : postfix_expression -> postfix_expression OPENING_SQUARE_BRACKET expression CLOSING_SQUARE_BRACKET\n");}
				  |postfix_expression OPENING_PARANTHESES argument_expression_list CLOSING_PARANTHESES {printf("\tReduced : postfix_expression -> postfix_expression OPENING_PARANTHESES argument_expression_list CLOSING_PARANTHESES\n");}
				  |postfix_expression OPENING_PARANTHESES CLOSING_PARANTHESES {printf("\tReduced : postfix_expression -> postfix_expression OPENING_PARANTHESES CLOSING_PARANTHESES\n");}
				  |postfix_expression DOT IDENTIFIER {printf("\tReduced : postfix_expression -> postfix_expression DOT IDENTIFIER\n");}
				  |postfix_expression LINK IDENTIFIER {printf("\tReduced : postfix_expression -> postfix_expression LINK IDENTIFIER\n");}
				  |postfix_expression INCREMENT {printf("\tReduced : postfix_expression -> postfix_expression INCREMENT\n");}
				  |postfix_expression DECREMENT {printf("\tReduced : postfix_expression -> postfix_expression DECREMENT\n");}
				  ;

primary_expression:IDENTIFIER {printf("\tReduced : primary_expression -> IDENTIFIER\n");}
				  |constant { strcpy($$,$1); printf("\tReduced : primary_expression -> constant\n");}
				  |STRING_CONSTANT {printf("\tReduced : primary_expression -> STRING_CONSTANT\n");}
				  |OPENING_PARANTHESES expression CLOSING_PARANTHESES {printf("\tReduced : primary_expression -> OPENING_PARANTHESES expression CLOSING_PARANTHESES\n");}
				  ;
				  
argument_expression_list:assignment_expression {printf("\tReduced : argument_expression_list -> assignment_expression\n");}
						|argument_expression_list COMMA assignment_expression {printf("\tReduced : argument_expression_list -> argument_expression_list COMMA assignment_expression\n");}
						;
						
constant:NUMBER { strcpy($$,$1); printf("\tReduced : constant -> NUMBER\n");}
		|CHAR_CONSTANT { strcpy($$,$1); printf("\tReduced : constant -> CHAR_CONSTANT\n");}
		|REAL_NUMBER { strcpy($$,$1); printf("\tReduced : constant -> REAL_NUMBER\n");}
		|EXP_NUMBER { strcpy($$,$1); printf("\tReduced : constant -> EXP_NUMBER\n");}
		;
		
%%

#include "lex.yy.c"
int yyerror()
{
	printf("error...\n");
	eflag = 1;
	exit(0);
}

int idpresent( char *id)
{
	int i;
	for(i=0;i<en;++i)
	{
		if(strcmp(st[i].id,id)==0)
		{
			printf("Duplicate ids ");
			yyerror();
		}
	}
	return 0;
}
int main(int argc,char *argv[])
{
		int i; 
        if(argc<2)
        {
                printf("Error detected due to file name missing\n");
                return -1;
        }
        yyin = fopen(argv[1],"r");
        yyparse();
		if(!eflag)
		{
		printf("______________________________________________________________________________________________________________________\n");
		printf("Constructing symbol table.....\n");
		printf("\tLine\tID   type   is_array\tsize\tinit_values(s)\n");
		for(i=0;i<en;++i)
		{
			printf("\t%d\t%s\t%s\t%d\t%d\t",st[i].line , st[i].type ,st[i].id, st[i].isarray , st[i].size);
			if(strcmp(st[i].initvalue,"")==0) printf("----\n");
			else printf("%s\n",st[i].initvalue);
		}
		}
        fclose(yyin);
        return 0;
}