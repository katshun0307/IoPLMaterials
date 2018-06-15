%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ LETAND
%token RARROW FUN DFUN REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e } (* expressions *)
  | LET x=ID EQ e=Expr SEMISEMI { Decl(x, e) } (* declaration *)
  | LET x=ID EQ e1=Expr l2=toplevel { DeclList(Decl(x, e1), l2) } (* 3.3.2: let x = 1 let y = 1;; OK *)
  | LET x=ID EQ e1=Expr LETAND l2=CLOSEDDECLBOTTOMExpr { ClosedDeclList(ClosedDecl(x, e1):: l2) } (* to use let x = 100 and y = x in x+y *)
  | LET x=ID EQ e1=Expr IN e2=Expr LETAND l2=CLOSEDDECLBOTTOMExpr { ClosedDeclList(ClosedLetExp(x, e1, e2):: l2) } (* to use let x = 100 and y = x in x+y *)
  (* TODO: ex) let x a b = a + b *)
  /* | LET x=ID b=LETFUNBOTTOMExpr { Decl(x, b) } */

(* closed declarations *)
CLOSEDDECLBOTTOMExpr :
  (* 3.3.4 (ex) let x = 1 and y = x in x + y;; *) 
  | x=ID EQ e1=Expr LETAND l2=CLOSEDDECLBOTTOMExpr { ClosedDecl(x, e1):: l2 }
  | x=ID EQ e1=Expr IN e2=Expr LETAND l2=CLOSEDDECLBOTTOMExpr { ClosedLetExp(x, e1, e2):: l2 }
  | x=ID EQ e1=Expr SEMISEMI { ClosedDecl(x, e1)::[] }
  | x=ID EQ e1=Expr IN e2=Expr SEMISEMI { ClosedLetExp(x, e1, e2)::[] }

Expr :
    e=IfExpr { e } (* if expression *)
  | e=LTExpr { e } (* arithmatic expression *)
  | e=ORExpr { e } (* boolean expression *)  
  | e=LETExpr { e } (* let expression *)
  | e=FUNExpr { e } (* static function expression *)
  | e=DFUNExpr { e } (* dynamic function expression *)
  | e=BinExpr { e } (* binary expressions *) 
  (* | e=MULTDECLExpr { e } *)

(* if expression *)
IfExpr :
  | IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

(* let expression *)
LETExpr :
  | LET x=ID EQ e1=Expr IN e2=Expr { LetExp(x, e1, e2) }


(* MULTDECLExpr : 
  LET x=ID EQ e1=Expr { DeclList(Decl(x, e1), DeclListEnd(End)) }
  | LET x=ID EQ e1=Expr l2=MULTDECLExpr { DeclList(Decl(x, e1), l2) } *)

(* arithmatic expressions *)
LTExpr : (* less than expression *)
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr : (* addition *)
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }
 
MExpr : (* multiplication *)
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr : (* function application *)
    e1=AppExpr e2=AExpr { AppExp(e1, e2) }
  | e1=BinExpr e2=AExpr { AppExp(e1, e2) } (* binary expressions as function *)
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
    l=ORExpr OR r=ANDExpr { BinOp (Or, l, r) }
    /* l=ANDExpr OR r=ORExpr { BinOp (Or, l, r) } */
  | e=ANDExpr { e }

ANDExpr : (* and *)
    l=ANDExpr AND r=AExpr { BinOp (And, l, r) } 
    /* l=AExpr AND r=ANDExpr { BinOp (And, l, r) } */
  | e=AExpr { e } 

BinExpr : (* binary expression *)
  |  LPAREN PLUS RPAREN { FunExp("a", FunExp("b", BinOp (Plus,  Var "a", Var "b"))) }
  |  LPAREN MULT RPAREN { FunExp("a", FunExp("b", BinOp (Mult,  Var "a", Var "b"))) }
  |  LPAREN LT RPAREN   { FunExp("a", FunExp("b", BinOp (Lt,    Var "a", Var "b"))) }
  |  LPAREN AND RPAREN  { FunExp("a", FunExp("b", BinOp (And,   Var "a", Var "b"))) }
  |  LPAREN OR RPAREN   { FunExp("a", FunExp("b", BinOp (Or,    Var "a", Var "b"))) }

(* most basic expressions *)
AExpr : (* integer, boolean, variable(id), expression_with_parenthesis *)
    i=INTV { ILit i } 
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
