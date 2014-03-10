%{
    #include<stdio.h>
	#include<string.h>
    int line_num = 1;
%}
charset [a-zA-Z_]
allchar {charset}[a-zA-Z0-9_]*
digit [0-9]
number {digit}+
real {digit}+"."{digit}*
exp {digit}+("."{digit}*)?("e"|"E")("+"|"-")?{digit}*
char_consta "'"[^\'\\]"'"
char_constb "'\\"[abfrntv0\'\\]"'"
str_part (([^\"])*)
str_const (\"){str_part}(("\\\"")*{str_part})*(\")

%%
"auto" { printf("%d:\tAUTO\n",line_num); return AUTO; }
"break" { printf("%d:\tBREAK\n",line_num); return BREAK; }
"case" { printf("%d:\tCASE\n",line_num); return CASE;}
"char" { printf("%d:\tCHAR\n",line_num); return CHAR;}
"const" { printf("%d:\tCONST\n",line_num); return CONST;}
"continue" { printf("%d:\tCONTINUE\n",line_num); return CONTINUE; }
"default" { printf("%d:\tDEFAULT\n",line_num); return DEFAULT; }
"do" { printf("%d:\tDO\n",line_num); return DO;}
"double" { printf("%d:\tDOUBLE\n",line_num); return DOUBLE; } 
"else" { printf("%d:\tELSE\n",line_num); return ELSE;}
"enum" { printf("%d:\tENUM\n",line_num); return ENUM;}
"extern" { printf("%d:\tEXTERN\n",line_num); return EXTERN;}
"float" { printf("%d:\tFLOAT\n",line_num); return FLOAT;}
"for" { printf("%d:\tFOR\n",line_num); return FOR;}
"goto" { printf("%d:\tGOTO\n",line_num); return GOTO;}
"if" { printf("%d:\tIF\n",line_num); return IF;}
"int" { printf("%d:\tINT\n",line_num); return INT;}
"long" { printf("%d:\tLONG\n",line_num); return LONG;}
"register" { printf("%d:\tREGISTER\n",line_num); return REGISTER;}
"return" { printf("%d:\tRETURN\n",line_num); return RETURN;}
"short" { printf("%d:\tSHORT\n",line_num); return SHORT;}
"signed" { printf("%d:\tSIGNED\n",line_num); return SIGNED;}
"sizeof" { printf("%d:\tSIZEOF\n",line_num); return SIZEOF;}
"static" { printf("%d:\tSTATIC\n",line_num); return STATIC;}
"struct" { printf("%d:\tSTRUCT\n",line_num); return STRUCT;}
"switch" { printf("%d:\tSWITCH\n",line_num); return SWITCH;}
"typedef" { printf("%d:\tTYPEDEF\n",line_num); return TYPEDEF;}
"union" { printf("%d:\tUNION\n",line_num); return UNION;}
"unsigned" { printf("%d:\tUNSIGNED\n",line_num); return UNSIGNED;}
"void" { printf("%d:\tVOID\n",line_num); return VOID;}
"volatile" { printf("%d:\tVOLATILE\n",line_num); return VOLATILE;}
"while" { printf("%d:\tWHILE\n",line_num); return WHILE;}

"+" { printf("%d:\tPLUS\n",line_num); return PLUS;}
"-" { printf("%d:\tMINUS\n",line_num); return MINUS;}
"/" { printf("%d:\tDIV\n",line_num); return DIV;}
"*" { printf("%d:\tMULT\n",line_num); return MULT;}
"%" { printf("%d:\tMOD\n",line_num); return MOD;}
"=" { printf("%d:\tEQUAL\n",line_num); return EQUAL;}
"<" { printf("%d:\tLESS\n",line_num); return LESS;}
">" { printf("%d:\tGREATER\n",line_num); return GREATER;}
"!" { printf("%d:\tNOT\n",line_num); return NOT;}
"&" { printf("%d:\tB_AND\n",line_num); return B_AND;}
"|" { printf("%d:\tB_OR\n",line_num); return B_OR;}
"^" { printf("%d:\tB_XOR\n",line_num); return B_XOR;}
"~" { printf("%d:\tB_NOT\n",line_num); return B_NOT;}
"++" { printf("%d:\tINCREMENT\n",line_num); return INCREMENT;}
"--" { printf("%d:\tDECREMENT\n",line_num); return DECREMENT;}
"+=" { printf("%d:\tPLUS_EQUAL\n",line_num); return PLUS_EQUAL;}
"-=" { printf("%d:\tMINUS_EQUAL\n",line_num); return MINUS_EQUAL;}
"*=" { printf("%d:\tMULT_EQUAL\n",line_num); return MULT_EQUAL;}
"/=" { printf("%d:\tDIV_EQUAL\n",line_num); return DIV_EQUAL;}
"%=" { printf("%d:\tMOD_EQUAL\n",line_num); return MOD_EQUAL;}
"==" { printf("%d:\tEQUAL_EQUAL\n",line_num); return EQUAL_EQUAL;}
"<=" { printf("%d:\tLESS_EQUAL\n",line_num); return LESS_EQUAL;}
">=" { printf("%d:\tGREATER_EQUAL\n",line_num); return GREATER_EQUAL;}
"!=" { printf("%d:\tNOT_EQUAL\n",line_num); return NOT_EQUAL;}
"&=" { printf("%d:\tAND_EQUAL\n",line_num); return AND_EQUAL;}
"|=" { printf("%d:\tOR_EQUAL\n",line_num); return OR_EQUAL;}
"^=" { printf("%d:\tXOR_EQUAL\n",line_num); return XOR_EQUAL;}
"&&" { printf("%d:\tAND\n",line_num); return AND;}
"||" { printf("%d:\tOR\n",line_num); return OR;}
"<<" { printf("%d:\tLSHIFT\n",line_num); return LSHIFT;}
">>" { printf("%d:\tRSHIFT\n",line_num); return RSHIFT;}
"<<=" { printf("%d:\tLSHIFT_EQUAL\n",line_num); return LSHIFT_EQUAL;}
">>=" { printf("%d:\tRSHIFT_EQUAL\n",line_num); return RSHIFT_EQUAL;}
"->" { printf("%d:\tLINK\n",line_num); return LINK;}

"{" { printf("%d:\tOPENING_CURLY_BRACES\n",line_num); return OPENING_CURLY_BRACES;} 
"}" { printf("%d:\tCLOSING_CURLY_BRACES\n",line_num); return CLOSING_CURLY_BRACES;}
"(" { printf("%d:\tOPENING_PARANTHESES\n",line_num); return OPENING_PARANTHESES;}
")" { printf("%d:\tCLOSING_PARANTHESES\n",line_num); return CLOSING_PARANTHESES;}
"[" { printf("%d:\tOPENING_SQUARE_BRACKET\n",line_num); return OPENING_SQUARE_BRACKET;}
"]" { printf("%d:\tCLOSING_SQUARE_BRACKET\n",line_num); return CLOSING_SQUARE_BRACKET;}
";" { printf("%d:\tSEMICOLON\n",line_num); return SEMICOLON;}
":" { printf("%d:\tCOLON\n",line_num); return COLON;}
"," { printf("%d:\tCOMMA\n",line_num); return COMMA;}
"?" { printf("%d:\tTERNARY\n",line_num); return TERNARY;}
"." { printf("%d:\tDOT\n",line_num); return DOT;}
"..." { printf("%d:\tTHREE_DOT\n",line_num); return THREE_DOT;}


{allchar} {
			printf("%d:\tIDENTIFIER\t%s\n",line_num,yytext);
			strcpy(yylval.cval,yytext);
			return IDENTIFIER; } 
{number} {
			printf("%d:\tNUMBER\t%s\n",line_num,yytext); 
			strcpy(yylval.cval,yytext);
			return NUMBER; }
{real} {
			printf("%d:\tREAL_NUMBER\t%s\n",line_num,yytext);
			strcpy(yylval.cval,yytext);
			return REAL_NUMBER; }
{exp} {
		printf("%d:\tEXP_NUMBER\t%s\n",line_num,yytext);
		strcpy(yylval.cval,yytext);
		return EXP_NUMBER; }
{char_consta} {
				printf("%d:\tCHAR_CONSTANT\t%s\n",line_num,yytext); 
				strcpy(yylval.cval,yytext);
				return CHAR_CONSTANT; }
{char_constb} {
				printf("%d:\tCHAR_CONSTANT\t%s\n",line_num,yytext);
				strcpy(yylval.cval,yytext);
				return CHAR_CONSTANT; } 
{str_const} {
				printf("%d:\tSTRING_CONSTANT\t%s\n",line_num,yytext);
				strcpy(yylval.cval,yytext);
				return STRING_CONSTANT; }

\n { line_num++;}
(\t|" ") {}
. { printf("%d:\tError: Token not recognised\n"); }
%%

int yywrap()
{
return -1;
}
