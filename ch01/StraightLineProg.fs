module AppelCh01

open Microsoft.FSharp.Collections

type id = string

type binop = Plus | Minus | Times | Div

type stm =  
      | CompoundStm of stm * stm
      | AssignStm of id * expr
      | PrintStm of expr list
and expr =  
      | IdExp of id
      | NumExp of int
      | OpExp of expr * binop * expr
      | EseqExp of stm * expr

let putVal (env:Map<string, int>) idName v = Map.add idName v env
let getVal env idName = 
    if Map.containsKey idName env 
    then Map.find idName env 
    else failwith (Printf.sprintf "The variable %s is not defined" idName) 

let rec evalStm env stm =
    match stm with
    | CompoundStm(stm1, stm2) ->
        let (env', res) = evalStm env stm1
        evalStm env' stm2
    | AssignStm(idName, expr) ->
        let (env', v) = evalExpr env expr
        let env'' = putVal env idName v
        (env'', v)
    | PrintStm(xs) ->
        evalPrint env xs

and evalPrint env exprs = 
    let evalPrintSingle env expr =
        let (env', v) = evalExpr env expr
        printfn "%d" v
        (env', v)
    match exprs with
    | [] -> (env, 0)
    | expr::exprs' ->
            let (env', v) = evalPrintSingle env expr
            evalPrint env' exprs'

and evalExpr env expr =
    match expr with
    | IdExp(idName) -> (env, getVal env idName)
    | NumExp(v) -> (env, v)
    | OpExp(expr1, op, expr2) ->
        let (env', v1) = evalExpr env expr1
        let (env'', v2) = evalExpr env' expr2
        let vRes = match op with
                   | Plus -> v1 + v2
                   | Minus -> v1 - v2
                   | Times -> v1 * v2
                   | Div -> v1 / v2
        (env'', vRes)
    | EseqExp(stm1, expr) -> 
        let (env', v) = evalStm env stm1
        evalExpr env' expr
        

let assignStm1 = AssignStm("hello", NumExp(10)) 
let assignStm2 = AssignStm("mello", NumExp(20))
let printStm2 = PrintStm([EseqExp(CompoundStm(assignStm1, assignStm2), IdExp("mello"))]) 

let prog1 = 
    CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStm(AssignStm("b",
                     EseqExp(PrintStm[IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                        OpExp(NumExp 10, Times, IdExp "a"))),
        PrintStm [IdExp "b"]))

let testEvalStm stm =
    evalStm (Map.empty : Map<string, int>) stm
