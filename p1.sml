(*
Names: April Gauthreaux and Sadie Sanders
Emails: ajg044@email.latech.edu, sas111@email.latech.edu
Date: 01/17/2025
Course: CSC 330 002
Quarter: Winter 2025 
Project 1 - ML Mini Parser
*)

datatype token = EQ | PL | MI | TI | DI | ID of string;

fun parse (input, output) = 
let 
    fun readLines instream = 
    (* HELPER FOR STEP 1 *)
    (* read file to each newline, return string list list -april *)
    case TextIO.inputLine instream of 
        SOME line => (String.substring(line, 0, String.size(line)-1)) :: readLines instream
        | NONE => [];
    
    (* MAIN STEP 1 *)
    val instream = TextIO.openIn(input);
    val outstream = TextIO.openOut(output);
    val text = readLines(instream);

    (* HELPER FOR STEP 2 *)
    (* reuseable function: push exploded string list list into function one string list at a time -sadie*)
    fun explodeAndFunction ( _, nil ) = nil
    |   explodeAndFunction (f, (x :: xs)) = implode(f(explode(x))) :: explodeAndFunction(f, xs);

    (* mini step: check if file contains only valid characters *)
    (* check each char for invalid characters -sadie*)
    fun checkInvCHAR nil = [#"T"]
    |   checkInvCHAR (#" " :: xs) = checkInvCHAR(xs)
    |   checkInvCHAR (#"=" :: xs) = checkInvCHAR(xs)
    |   checkInvCHAR (#"+" :: xs) = checkInvCHAR(xs)
    |   checkInvCHAR (#"-" :: xs) = checkInvCHAR(xs)
    |   checkInvCHAR (#"*" :: xs) = checkInvCHAR(xs)
    |   checkInvCHAR (#"/" :: xs) = checkInvCHAR(xs)
    |   checkInvCHAR (x :: xs) = (* basically, if letter or number*)
            if (ord(x)>=97 andalso ord(x)<=122) orelse (ord(x)>=65 andalso ord(x) <=90) orelse (ord(x)>=48 andalso ord(x) <= 57)
            then checkInvCHAR(xs)
            else [#"F"];
    
    (* parse input from checkInvCHAR (turn into true/false) -sadie*)
    fun checkInvBOOL nil = true
    |   checkInvBOOL ("F" :: _) = false
    |   checkInvBOOL (_ :: xs) = checkInvBOOL(xs);

    (* mini step: prep the file lines for tokenization *)
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
        if checkInvBOOL(explodeAndFunction(checkInvCHAR, filelist))
        then removewhitespace(explodeAndFunction(addwhitespace, filelist))
        else (print("Parse Error\n"); TextIO.stdOut; []);
    val step2list = step2(text);


    (* HELPER FOR STEP 3 *)
    (*Converting the strings into token datatype - april *)
    fun tokenise str = 
        case str of "=" => EQ
        | "+" => PL
        | "-" => MI
        | "*" => TI
        | "/" => DI
        | _ => ID str;
    
    (* MAIN STEP 3 - april*)
    (*Iterates over the lists to tokenise each string in list*)
    fun step3 inputLists = List.map(fn singularList => List.map tokenise singularList) inputLists;
    val step3list = step3(step2list);

    (* HELPER FOR STEP 4 - april*)
    fun innerLists [] = ()
    | innerLists [y] =  (* Handle the last token without adding a trailing comma *)
        (case y of
            EQ => TextIO.output(outstream, "EQ")
            | PL => TextIO.output(outstream, "PL")
            | MI => TextIO.output(outstream, "MI")
            | TI => TextIO.output(outstream, "TI")
            | DI => TextIO.output(outstream, "DI")
            | ID str => TextIO.output(outstream, "ID " ^ "\"" ^ str ^ "\""))
    | innerLists (y :: ys) =  (* Add a comma after the token *)
        (case y of
            EQ => TextIO.output(outstream, "EQ, ")
            | PL => TextIO.output(outstream, "PL, ")
            | MI => TextIO.output(outstream, "MI, ")
            | TI => TextIO.output(outstream, "TI, ")
            | DI => TextIO.output(outstream, "DI, ")
            | ID str => TextIO.output(outstream, "ID " ^ "\"" ^ str ^ "\", ");
            innerLists(ys));

    (* MAIN STEP 4 *)
    fun writeToOut [] = TextIO.output(outstream, "[]")
    |   writeToOut (x :: nil) = 
            (TextIO.output(outstream, "[");
            innerLists(x);
            TextIO.output(outstream, "]"))
    | writeToOut (x :: xs) =
      (TextIO.output(outstream, "[");
       innerLists x;
       TextIO.output(outstream, "]\n");
       writeToOut xs);
    
in
    (* MAIN FUNCTION *)
    (* 1. read file (done in variables so file can be closed properly) *)
    (* 2. tokenise file, throw error if invalid character*)
    (* 3. change datatype to token *)
    (* 4. write to output file *)
    writeToOut(step3list);
    TextIO.closeOut(outstream);
    step3list
end;
