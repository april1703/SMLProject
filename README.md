In a file named p1.sml, declare the following datatype:

datatype token = EQ | PL | MI | TI | DI | ID of string;
 
Then write an ML function with name/type:

parse : string * string -> token list list
 
that inputs the name of an input text file and the name of an output text. 
The input file will contain a sequence of characters representing a (not necessarily proper) assignment statement per line of text. 
Your function will parse each line of the text file and output a list of tokens lists that classify each type of word that was read in each line the file. 
In addition, your function will also write the results to the output file, where each line has the corresponding token list in the format specified below.

The following table shows all possible valid words and their corresponding tokens.

 

Word                                       Token

=                                              EQ

+                                              PL

-                                               MI

*                                               TI

/                                               DI

<letter combo>                      ID <letter combo>

For example, if the input text file is named input.txt and contains:

profit = Revenue - cost
Hello world +
 
Then your function should work as follows by the user in the terminal:

- parse("input.txt","output.txt");
val it = [[ID "profit",EQ,ID "Revenue",MI,ID "cost"],[ID "Hello",ID "world",PL]] : token list list
 
And the following should be written to the output file output.txt:

[ID "profit",EQ,ID "Revenue",MI,ID "cost"]
[ID "Hello",ID "world",PL]

The words may be separated by one or more whitespace characters, which shall be ignored. 
For example, if the input text file contains:

profit=Revenue-cost
Hello world+

Then the results should be the same as the previous example. 
The given text file need not contain a valid statements (as in the second line of the example above). 
Your parser's job is to correctly identify the tokens, even if the program would not successfully compile at a later stage. 
Also, any identifiers must have intervening whitespace among themselves to distinguish them. 
For example, "hello world" should be read as two tokens while "helloworld" should be read as one token.

Also, any other words that are not in the table above shall be considered invalid. 
The following are examples that are considered invalid for parsing:

Hello 3 world +
profit = Revenue & cost

For parsing an invalid file, your function should return an empty list and write an empty list to the output file. 
You should also display the message "Parse Error" to the terminal.
