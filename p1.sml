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

fun readLines instream = 
    case TextIO.inputLine instream of 
        SOME line => line :: readLines instream
        | NONE => [];

fun parse (input, output) = 
    val instream =TextIO.openIn input;
    val lines = readLines instream
    val _ = TextIO.closeIn instream;
