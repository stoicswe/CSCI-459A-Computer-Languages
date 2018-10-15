Project 1 of Programming Languages

This project involved generating unique bracelets and outputing the number of them
given the number of beads and colors.

The haskell version generates bracelet types wehereas the rest of the programs
just generates the number of bracelets according to the rule: b! / ((b-c)! * c!),
where b is the number of beads and c the number of colors.


Compiling:

The only languuages I got to compile was the haskell and c versions, the rest were run in
the respective language's environments:

PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1> ls


    Directory: C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1


Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----        9/19/2018  10:31 AM         388744 a.exe
-a----        9/20/2018   8:40 PM            785 beads.c
-a----        9/20/2018   8:38 PM        7011072 beads.exe
-a----        9/20/2018   8:38 PM           2129 beads.hi
-a----        9/20/2018   8:36 PM           3296 beads.hs
-a----        9/20/2018   8:38 PM          17588 beads.o
-a----        9/21/2018   7:36 PM            624 beads.pl
-a----        9/20/2018   8:40 PM            384 beads.py
-a----        9/20/2018  11:22 PM            630 beads.rkt
-a----        9/11/2018  10:40 AM            110 factorialTest.py
-a----        9/21/2018   7:56 PM           2454 README.md


PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1> ghc ./beads.hs
PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1> gcc ./beads.c
PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1>


Here is an abreviation of the times I had with each language:

Haskell
-------
Haskell was the language I chose to fully implement. I throughly enjoyed the very
unique and different ideas/syntax of Haskell versus the ususal C-styled syntax I
had grown accustumned to. Overall, the language was a bit confusing, when not having
guidance. Once I have had guidance with working on the project, I found the language
much more understandable. I believe this might be my favorite of the languages we are
covering in this course so far.

OUTPUT:

colors: 2
beads: 4

PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1> ./beads.exe 2 4
Generating bracelets...
The number of bracelets generated is:
6
PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1>

C
------
C is definately not C#. I miss many of the nice things C# has to offer (like memory
management). I do like there is the familiar structure of programming and, overall,
it was a somewhat easy language to learn the basics to without much help from outside
sources.

OUTPUT:

colors: 2
beads: 4

PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1> .\a.exe 4 2
Number of Unique Bracelets: 6.000000
PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1>

Python
------
Python was super easy to work with, as I am usually using it in machine learning and
computational statistics. I dont particularily like python, becuase of its arbitruary
var types, unlike Haskell and C. I wish that python had a strict type system.

OUTPUT:

colors: 2
beads: 4

PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1> python .\beads.py
 4 2
Number of unique bracelets: 6
PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1>

Racket
------
When I first looked at this code, I thought it was going to be a nightmare to work on.
SO MANY PARANTHESIS!!!! I dont know why the paranthesis,  they could have used something
easier to undersand like brackets...but whatever. Anyway, after looking at some good racket
code, I was able to figure it out enough for what I needed to code. This langauge isnt so bad
once you get used to it.

OUTPUT:

colors: 2
beads: 4

PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1> racket .\beads.rk
t 4 2
The number of bracelets with the given parameters is 6
PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1>

Prolog
-----
Yea....about that.... This language, I imagine, once I understand it, it would be cool to write
in, but the logic that went into making this language just doesnt make sense to me right now.
I've tried to understand it better...and I havent been able to find good documentation or books
on it. I do want to trash talk the language constantly, but honestly...I cannot really do that 
until I have a better argurment. There's a lot of strange ways in which the language acts when
it runs and the error messages are not helpfun when debugging it.

OUTPUT:

colors: 2
beads: 4

PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1> swipl -q -t main
-s .\beads.pl -- 4 2
'Number of bracelets: '6
PS C:\Users\taran\Documents\Houghton College\Programming Languages\Homework 1>