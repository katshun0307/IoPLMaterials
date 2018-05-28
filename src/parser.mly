%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN DFUN

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
  | e=FUNExpr { e } (* static function expression *)
  | e=DFUNExpr { e } (* dynamic function expression *)

(* if expression *)
IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

(* let expression *)
LETExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp(x, e1, e2) }

(* number expressions *)
LTExpr : (* less than expression *)
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr : (* addition *)
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  (* | LPAREN PLUS RPAREN s=PExpr t=MExpr { BinOp (Plus, s, t) } *)
  | e=MExpr { e }
 
MExpr : (* multiplication *)
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr : (* function application *)
    e1=AppExpr e2=AExpr { AppExp(e1, e2) }
  | e=AExpr { e }

(* static function expression *)
FUNExpr : (* fun x1 ... -> expr *)
    FUN b=FunBottomExpr { b }

FunBottomExpr : (* ....xn-1 xn -> expr *)
    x=ID RARROW e=Expr { FunExp(x, e) }
  | x=ID b=FunBottomExpr { FunExp (x, b) }

(* dynamic function expression *)
DFUNExpr : (* dfun x1 ... -> expr *)
    DFUN b=DFunBottomExpr { b }

DFunBottomExpr : (* ....xn-1 xn -> expr *)
    x=ID RARROW e=Expr { DFunExp(x, e) }
  | x=ID b=FunBottomExpr { DFunExp (x, b) }

(* logical expressions *)
ORExpr : (* or *)
    l=ANDExpr OR r=ANDExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr : (* and *)
    l=ANDExpr AND r=AExpr { BinOp (And, l, r) }
  | e=AExpr { e }

(* most basic expressions *)
AExpr : (* integer, boolean, variable(id), expression_with_parenthesis *)
    i=INTV { ILit i } 
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
