grammar ExampleGrammar;

/*
 * Lexer Rules
 */

// Keywords

INT: 'int';

BOOL: 'bool';

PRINT: 'print';

IF: 'if';

ELSE: 'else';

WHILE: 'while';


// Separators / Punctuators

LBRACE: '{';

RBRACE: '}';

LPAR: '(';

RPAR: ')';

SEMICOLON: ';';


// Operators

ASSIGN: '=';

NOT: 'not';

AND: 'and';

OR: 'or';

ADD: '+';

SUBTRACT: '-';

MULTIPLY: '*';

DIVIDE: '/';

GREATER: '>';

GREATER_EQUAL: '>=';

LESS: '<';

LESS_EQUAL: '<=';

EQUAL: '==';

DIFFERENT: '!=';


// Literals

TRUE: 'True';

FALSE: 'False';

fragment DIGIT: [0-9];

NUMBER: DIGIT+;


// Identifiers

fragment LETTER: [a-zA-Z];

IDENTIFIER: LETTER (LETTER | DIGIT)*;


// Comments -> ignored

COMMENT: ('/*'(.*?)'*/'|'//'.*?'\r'?('\n'|EOF)) -> skip;


// Whitespaces -> ignored

NEWLINE: '\r'?'\n'  -> skip ;

WS: [ \t]+ -> skip ;


/*
 * Parser Rules
 */


// Programs

program: scope;

scope: declareStatement* statements;

statements: statement*;


// Statements

statement: assignStatement | printStatement | ifStatement | whileStatement;

declareStatement: type IDENTIFIER SEMICOLON;

assignStatement: IDENTIFIER ASSIGN (expression | LBRACE scope expression RBRACE) SEMICOLON;

printStatement: PRINT expression SEMICOLON;

ifStatement: IF LPAR expression RPAR LBRACE if=statements RBRACE ELSE LBRACE else=statements RBRACE;

whileStatement: WHILE LPAR expression RPAR LBRACE statements RBRACE;


// Types

type: INT | BOOL;


// Expressions

expression: disjunction;

disjunction: conjunction (OR conjunction)+ | passthrough=conjunction;

conjunction: inversion (AND inversion)+ | passthrough=inversion;

inversion: NOT inversion | comparison;

comparison: left=comparison (LESS | GREATER | EQUAL | DIFFERENT | GREATER_EQUAL | LESS_EQUAL) right=sum | passthrough=sum;

sum: left=sum (ADD | SUBTRACT) right=product | passthrough=product;

product: left=product (MULTIPLY | DIVIDE) right=factor | passthrough=factor;

factor: (ADD | SUBTRACT) atom | atom;

atom: IDENTIFIER | NUMBER | TRUE | FALSE | LPAR expression RPAR;
