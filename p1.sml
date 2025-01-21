(*
Name: April Gauthreaux and Sadie Sanders
Date: 01/17/2025
Description: Project 1 - ML Mini Parser
*)

(*String.tokens;*)

datatype token = EQ | PL | MI | TI | DI | ID of string;

fun readLines instream = 
    case TextIO.inputLine instream of 
        SOME line => line :: readLines instream
        | NONE => [];

fun parse input, output = 
    val instream =TextIO.openIn input
    val lines = readLines instream
    val _ = TextIO.closeIn instream;
