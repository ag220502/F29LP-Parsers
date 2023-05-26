# Questions About CW 

This is a document with questions I got about the coursework. 
To be fair/that everyone has the same information, questions and answers that are not specific to one project are published here: 

- What does "that is not already a keyword" mean for identifiers? - See the slide on tokens in the Lexical Analysis slides:
A keyword is any word representing the structure of a program; so it is all lexemes reserved for this - which of course need separate tokens later on.
- Can I distinguish ids and function names in the lexer? - No, you can't. The parser will be responsible for distinguishing them. 
- The lexer doesn't output what I expect. - Ensure that it uses your up-to-date version of the file. As described in the file, you have to **restart** the kernel. (And not just re-run the code part.)
- How do I best approach parsing a complicated non-terminal? - See the lab: Start with building a suitable grammar, and then parse strictly according to this grammar.
- Is my parser for X correct? - Try testing your parser for different cases: What should be accepted, what should not be? (See examples in the lab and lecture on how to test your parser!) Feel free to approach me at any time to ask whether a certain program is valid.
- For the definition of leading zeroes, see the following Wikipedia article: https://en.wikipedia.org/wiki/Leading_zero

Please feel free to contact me in case any other questions appear!
