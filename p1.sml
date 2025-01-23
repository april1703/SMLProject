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
    (* HELPER FOR STEP 1 *)

    (* read file to each newline, return string list list *)
    case TextIO.inputLine instream of 
        SOME line => (String.substring(line, 0, String.size(line)-1)) :: readLines instream
        | NONE => [];
    
    (* MAIN STEP 1 *)
    val instream = TextIO.openIn input;
    val text = readLines instream;
    val _ = TextIO.closeIn instream;
    

    (* HELPER FOR STEP 2 *)

    (* push exploded string list list into function one string list at a time -sadie*)
    fun explodeAndFunction ( _, nil ) = nil
    |   explodeAndFunction (f, (x :: xs)) = implode(f(explode(x))) :: explodeAndFunction(f, xs);

    (* check each char for invalid characters *)
    fun checkForInvalid nil = [#"T"]
    |   checkForInvalid (#" " :: xs) = checkForInvalid(xs)
    |   checkForInvalid (#"=" :: xs) = checkForInvalid(xs)
    |   checkForInvalid (#"+" :: xs) = checkForInvalid(xs)
    |   checkForInvalid (#"-" :: xs) = checkForInvalid(xs)
    |   checkForInvalid (#"*" :: xs) = checkForInvalid(xs)
    |   checkForInvalid (#"/" :: xs) = checkForInvalid(xs)
    |   checkForInvalid (x :: xs) = (* basically, if letter or number*)
            if (ord(x)>=97 andalso ord(x)<=122) orelse (ord(x)>=65 andalso ord(x) <=90) orelse (ord(x)>=48 andalso ord(x) <= 57)
            then checkForInvalid(xs)
            else [#"F"];

    (* add whitespace before and after valid special symbols -sadie *)
    fun addwhitespace nil = nil
    |   addwhitespace (#"=" :: xs) = [#" ", #"=", #" "] @ addwhitespace(xs)
    |   addwhitespace (#"+" :: xs) = [#" ", #"+", #" "] @ addwhitespace(xs)
    |   addwhitespace (#"-" :: xs) = [#" ", #"-", #" "] @ addwhitespace(xs)
    |   addwhitespace (#"*" :: xs) = [#" ", #"*", #" "] @ addwhitespace(xs)
    |   addwhitespace (#"/" :: xs) = [#" ", #"/", #" "] @ addwhitespace(xs)
    |   addwhitespace (x :: xs) = x :: addwhitespace(xs);

    (* tokeise the separate strings -sadie *)
    fun removewhitespace nil = nil
    |   removewhitespace ( x:: xs) = 
        [(String.tokens (fn c => c = #" ") x)] @ removewhitespace(xs);

    (* MAIN STEP 2 *)
    fun step2 filelist =
        explodeAndFunction(checkForInvalid, filelist);        


in
    (* MAIN FUNCTION *)

    (* 1. read file (done in variables so file can be closed properly) *)

    (* 2. tokenise file, throw error if invalid character*)
    step2(text)
    (* 3. parse through the tokens, replace when necessary *)

    (* 4. write to output file *)

end;
getinput("./SMLProject/input");