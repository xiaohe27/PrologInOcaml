PrologInOcaml
=============

This is a project for implementing Prolog in Ocaml.

Basically, it is an interpreter for Prolog programs.

Participants: He Xiao and Shijiao Yuwen


#Install
sh make.sh


If the install finished successfully, then there will be four executables
generated: play.exe, debug.exe, readFile.exe and play_all.exe.

##play.exe: 
This is an interactive environment where users can input Prolog
program by hand; and when a complete program (clauses with a query) is detected,
the first result will be printed to the console.

##debug.exe:
Similar to play.exe, but you can get more debugging information in the output
in addition to the result.

##readFile.exe:
Same internal algorithm used as the above two programs, but it can read the input
from the specified path.
```./readFile.exe <path-to-your-prolog-source-code>```


##play_all.exe:
Play_all.exe uses backtracking algorithm to gather all the results, and it is the 
application which we mainly tested. It is an interactive interpreter, and users 
can interact with it in the similar way as they do for running play.exe. 
If multiple results available, users can input a ';' followed by a '\n' (enter key),
then the next result will be provided if there exists some.



#Test
The main test cases we used can be found in Test/prolog folder.


