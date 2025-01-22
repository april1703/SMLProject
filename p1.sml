(*
Name: April Gauthreaux and Sadie Sanders
Date: 01/17/2025
Description: Project 1 - ML Mini Parser
*)

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


(*returns a string list of file (sadie)*)   
fun getinput (input) = 
let 
    fun readLines instream = 
    (* read file to each newline, return string list list *)
    case TextIO.inputLine instream of 
        SOME line => line :: readLines instream
        | NONE => [];

    (* push exploded string list list into function one string list at a time -sadie*)
    fun explodeStringLists ( _, nil ) = nil
    |   explodeStringLists (f, (x :: xs)) = implode(f(explode(x))) :: explodeStringLists(f, xs);

    (* potential checking function partially worked on *)
    (* 
        if (ord(x)>=97 andalso ord(x)<=122) orelse (ord(x)>=65 andalso ord(x) <=90) orelse (ord(x)>=48 andalso ord(x) <= 57)
        then true
        else false;
    *)

    (* add whitespace before and after valid special symbols -sadie *)
    fun addwhitespace nil = nil
    |   addwhitespace (#"=" :: xs) = [#" ", #"=", #" "] @ addwhitespace(xs)
    |   addwhitespace (#"+" :: xs) = [#" ", #"+", #" "] @ addwhitespace(xs)
    |   addwhitespace (#"-" :: xs) = [#" ", #"-", #" "] @ addwhitespace(xs)
    |   addwhitespace (#"*" :: xs) = [#" ", #"*", #" "] @ addwhitespace(xs)
    |   addwhitespace (#"/" :: xs) = [#" ", #"/", #" "] @ addwhitespace(xs)
    |   addwhitespace (x :: xs) = x :: addwhitespace(xs);

    (* tokenise the separate strings -sadie*)
    fun removewhitespace nil = nil
    |   removewhitespace ( x:: xs) = 
        [(String.tokens (fn c => c = #" ") (String.substring(x, 0, String.size(x)-1)))] @ removewhitespace(xs);

    (* VARIABLES *)
    val instream = TextIO.openIn input;
    val text = readLines instream;
    val _ = TextIO.closeIn instream;
in
    (* 1. read file (done in variable step so file can be closed properly) *)

    removewhitespace(explodeStringLists(addwhitespace, text)) (* 2. tokenise the string lists *)

    (* 3. parse through the tokens, replace when necessary *)

    (* 4. write to output file *)

end;
getinput("input");