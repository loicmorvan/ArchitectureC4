module Tokenizer

open System.Text.RegularExpressions
open System.Linq

type Token =
    | Word of string
    | Operator of string
    | Text of string
    | Unknown

let words = "(?<word>\w+)"
let operators = "(?<operator>[{}=])"
let texts = "\"(?<text>[^\"]*)\""
let regex = new Regex($"{words}|{operators}|{texts}")

let tokenFromMatch (m: Match) =
    let theMatch = (m.Groups
        |> Seq.skip 1
        |> Seq.filter (fun x -> x.Success)).FirstOrDefault()
    match theMatch.Name with
    | "word" -> Word theMatch.Value
    | "operator" -> Operator theMatch.Value
    | "text" -> Text theMatch.Value
    | _ -> Unknown

let tokenize input =
    regex.Matches input 
    |> Seq.cast<Match>
    |> Seq.map tokenFromMatch
    |> Seq.toList
