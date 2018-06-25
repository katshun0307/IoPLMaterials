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
  (* TODO: ex) let x a b = a + b *)
  | LET f=ID b=LETFUNExpr { Decl(f, b) } (* declaration *)
  | LET REC f=ID EQ FUN para=ID RARROW e=Expr SEMISEMI { RecDecl(f, para, e) }
  | LET REC f=ID para=ID EQ e=Expr SEMISEMI { RecDecl(f, para, e) } (* recursive declaration 2 *)
  | LET x=ID EQ e1=Expr l2=DECLLISTBOTTOMExpr { DeclList((x, e1):: l2) } (* 3.3.2: let x = 1 let y = 1;; OK *)
  | LET x=ID EQ e1=Expr LETAND l2=CLOSEDDECLBOTTOMExpr { ClosedDeclList(ClosedDecl(x, e1):: l2) } (* 3.3.4: let x = 100 and y = x in x+y *)

(* continuous declarations *)
DECLLISTBOTTOMExpr : 
  | LET x=ID EQ e=Expr l2=DECLLISTBOTTOMExpr { (x, e) :: l2 }
  | LET x=ID EQ e=Expr SEMISEMI { (x, e) :: [] }

(* closed declarations *)
CLOSEDDECLBOTTOMExpr :
  (* 3.3.4 (ex) let x = 1 and y = x;; *) 
  | x=ID EQ e1=Expr LETAND l2=CLOSEDDECLBOTTOMExpr { ClosedDecl(x, e1):: l2 }
  | x=ID EQ e1=Expr SEMISEMI { ClosedDecl(x, e1)::[] }

(* let function declarations *)
LETFUNExpr : 
  | para=LETFUNPARAExpr e=Expr SEMISEMI { FunExp(para, e) }

LETFUNPARAExpr : 
  | x=ID l=LETFUNPARAExpr { x :: l }
  | x=ID EQ { x :: [] }

Expr :
    e=IfExpr { e } (* if expression *)
  | e=LTExpr { e } (* arithmatic expression *)
  | e=ORExpr { e } (* boolean expression *)
  | e=LETExpr { e } (* let expression *)
  | e=LETRECExpr { e } (* recurisve let expression *)
  | e=FUNExpr { e } (* static function expression *)
  | e=DFUNExpr { e } (* dynamic function expression *)
  | e=BinExpr { e } (* binary expressions *)

(* if expression *)
IfExpr :
  | IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

(* let expression *)
LETExpr : (* TODO: let f x y = x + y and y s t = s * t in in f 5 3 + y 5 7;; *)
  | LET e1=MULTILETExpr IN e2=Expr { MultiLetExp(e1, e2) } (* simple value declarations *)

LETRECExpr : 
  | LET REC f=ID EQ FUN para=ID RARROW e1=Expr IN e2=Expr { LetRecExp(f, para, e1, e2) }
  | LET REC f=ID para=ID EQ e1=Expr IN e2=Expr { LetRecExp(f, para, e1 ,e2) }

(* multiple declarations for let expression *)
MULTILETExpr : 
  /* | x=ID EQ e=Expr LETAND l=MULTILETExpr { (x, e) :: l } */
  /* | x=ID EQ e=Expr { (x, e) :: [] } */
  | x=ID EQ e=Expr LETAND l=MULTILETExpr { (x, e) :: l }
  | f=ID params=LETFUNPARAExpr e=Expr LETAND l=MULTILETExpr { (f, FunExp(params, e)) :: l }
  | x=ID EQ e=Expr { (x, e) :: [] }
  | f=ID params=LETFUNPARAExpr e=Expr { (f, FunExp(params, e)) :: [] }

/* LETEXPDECLExpr : (* <f x y x = x + y * z> / <x = 2> *)
  | x=ID EQ e=Expr { (x, e) } (* for simple declarations *)
  | f=ID params=LETFUNPARAExpr e=Expr { (f, FunExp(params, e)) } (* for function declarations using let *) */

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

(* logical expressions *)
ORExpr : (* or *)
    /* l=ORExpr OR r=ANDExpr { LogicOp (Or, l, r) } */
    l=ANDExpr OR r=ORExpr { LogicOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr : (* and *)
    /* l=ANDExpr AND r=AExpr { LogicOp (And, l, r) }  */
    l=AExpr AND r=ANDExpr { LogicOp (And, l, r) }
  | e=AppExpr { e }

AppExpr : (* function application *)
    e1=AppExpr e2=AExpr { AppExp(e1, e2) }
  | e1=AppExpr e2=BinExpr { AppExp(e1, e2) }
  /* | e1=Expr e2=AExpr { AppExp(e1, e2) } */
  /* | e1=Expr e2=BinExpr { AppExp(e1, e2) } */
  /* | e1=AppExpr e2=Expr { AppExp(e1, e2) } */
  | e=BinExpr { e }
  | e=AExpr { e }

BinExpr : (* binary expression *)
  |  LPAREN PLUS RPAREN { FunExp(["a" ; "b"], BinOp (Plus,  Var "a", Var "b")) }
  |  LPAREN MULT RPAREN { FunExp(["a" ; "b"], BinOp (Mult,  Var "a", Var "b")) }
  |  LPAREN LT RPAREN   { FunExp(["a" ; "b"], BinOp (Lt,    Var "a", Var "b")) }
  |  LPAREN AND RPAREN  { FunExp(["a" ; "b"], LogicOp (And,  Var "a", Var "b")) }
  |  LPAREN OR RPAREN   { FunExp(["a" ; "b"], LogicOp (Or,   Var "a", Var "b")) }

(* static function expression *)
/* FUNExpr : (* fun x1 ... -> expr *)
    FUN b=FunBottomExpr { b } */

/* FunBottomExpr : (* ....xn-1 xn -> expr *)
    x=ID RARROW e=Expr { FunExp(x, e) }
  | x=ID b=FunBottomExpr { FunExp (x, b) } */

FUNExpr : (* store ids as list *)
  FUN params=FUNPARAExpr e=Expr { FunExp(params, e) }

FUNPARAExpr : 
  | x=ID l=FUNPARAExpr { x :: l }
  | x=ID RARROW { x :: [] }

(* dynamic function expression *)
DFUNExpr : (* dfun x1 ... -> expr *)
    DFUN b=DFunBottomExpr { b }

DFunBottomExpr : (* ....xn-1 xn -> expr *)
    x=ID RARROW e=Expr { DFunExp(x, e) }
  | x=ID b=DFunBottomExpr { DFunExp (x, b) }

(* most basic expressions *)
AExpr : (* integer, boolean, variable(id), expression_with_parenthesis *)
    i=INTV { ILit i } 
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
