module TigerLexerTest

open System
open Microsoft.FSharp.Text

let x = " 32 if then else ident1 ident2 "

let lexbuf = Lexing.LexBuffer<char>.FromString(x)
while not lexbuf.IsPastEndOfStream do
    printfn "%A" (TigerLexer.tokenize lexbuf)

Console.ReadKey(true) |> ignore
