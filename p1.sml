(*
Name: April Gauthreaux and Sadie Sanders
Date: 01/17/2025
Description: Project 1 - ML Mini Parser
*)

open TextIO;

(*String.tokens;*)

datatype token = EQ | PL | MI | TI | DI | ID of string;

(* FLOW
1. read line
2. parse tokens in line
3. check for incorrect tokens ie. @ # $ % ^
    - if any, return false
    - else, add to token array
4. write to output file
*)

(*reads each line of a file (april)*)
fun readLines instream = 
    case TextIO.inputLine instream of 
        SOME line => line :: readLines instream
        | NONE => [];

(*returns a string list of file (sadie)*)   
fun parse (input) = 
let
    val instream = TextIO.openIn input;
    val text = readLines instream;
    val _ = TextIO.closeIn instream;
    fun tokenise nil = nil
    |   tokenise (x:: xs) = [(String.tokens (fn c => c = #"\n") x)] @ tokenise(xs)
in
    tokenise(text)
end;
parse("input");

