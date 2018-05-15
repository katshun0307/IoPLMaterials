%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ COMMENT

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl(x, e) } 

Expr :
    e=IfExpr { e } (* if expression *)
  | e=LTExpr { e } (* less than *)
  | e=ORExpr { e } (* boolean expression *)  
  | e=LETExpr { e } (* let expression *)

LTExpr : (* less than expression *)
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr : (* addition *)
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr : (* multiplication *)
    l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }

ORExpr : (* or *)
    l=ANDExpr OR r=ANDExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr : (* and *)
    l=ANDExpr AND r=AExpr { BinOp (And, l, r) }
  | e=AExpr { e }

AExpr : (* integer, boolean, variable(id), expression_with_parenthesis *)
    i=INTV { ILit i } 
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LETExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp(x, e1, e2) }
