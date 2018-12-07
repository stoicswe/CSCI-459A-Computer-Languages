# LLParse

This project was cretaed for the homework assignment #3 for programming languages. Unfortunately, due to debugging issues and complications, I was unable to create the interpretor for the abstract syntax tree. But, the application can create an abstract syntax tree.

## Difficulties

Some of the difficulties encoutnered involved understanding the concepts of how a programming langauge is broken down and using the productions to build an abstract syntax tree. This difficult concept I had gotten over mostly by the help of Ryan, but overall I had spent time just running the code and observing its outputs.

## Output

The example output is as follows:

taranoshi@lpt:~/Documents/Houghton College/CSCI-459A-Computer-Languages/Homework 3$ ghci ./LLParse.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling LLParse          ( LLParse.hs, interpreted )
Ok, modules loaded: LLParse.
*LLParse> printResult $ parse (parseTable extendedCalcGrammar) primes
P
`- SL
   +- S
   |  +- read
   |  `- n
   `- SL
      +- S
      |  +- cp
      |  +- :=
      |  `- E
      |     +- T
      |     |  +- F
      |     |  |  `- 2
      |     |  `- FT
      |     `- TT
      `- SL
         +- S
         |  +- while
         |  +- C
         |  |  +- E
         |  |  |  +- T
         |  |  |  |  +- F
         |  |  |  |  |  `- n
         |  |  |  |  `- FT
         |  |  |  `- TT
         |  |  +- rn
         |  |  |  `- >
         |  |  `- E
         |  |     +- T
         |  |     |  +- F
         |  |     |  |  `- 0
         |  |     |  `- FT
         |  |     `- TT
         |  +- SL
         |  |  +- S
         |  |  |  +- found
         |  |  |  +- :=
         |  |  |  `- E
         |  |  |     +- T
         |  |  |     |  +- F
         |  |  |     |  |  `- 0
         |  |  |     |  `- FT
         |  |  |     `- TT
         |  |  `- SL
         |  |     +- S
         |  |     |  +- cf1
         |  |     |  +- :=
         |  |     |  `- E
         |  |     |     +- T
         |  |     |     |  +- F
         |  |     |     |  |  `- 2
         |  |     |     |  `- FT
         |  |     |     `- TT
         |  |     `- SL
         |  |        +- S
         |  |        |  +- cf1s
         |  |        |  +- :=
         |  |        |  `- E
         |  |        |     +- T
         |  |        |     |  +- F
         |  |        |     |  |  `- cf1
         |  |        |     |  `- FT
         |  |        |     |     +- mo
         |  |        |     |     |  `- *
         |  |        |     |     +- F
         |  |        |     |     |  `- cf1
         |  |        |     |     `- FT
         |  |        |     `- TT
         |  |        `- SL
         |  |           +- S
         |  |           |  +- while
         |  |           |  +- C
         |  |           |  |  +- E
         |  |           |  |  |  +- T
         |  |           |  |  |  |  +- F
         |  |           |  |  |  |  |  `- cf1s
         |  |           |  |  |  |  `- FT
         |  |           |  |  |  `- TT
         |  |           |  |  +- rn
         |  |           |  |  |  `- <=
         |  |           |  |  `- E
         |  |           |  |     +- T
         |  |           |  |     |  +- F
         |  |           |  |     |  |  `- cp
         |  |           |  |     |  `- FT
         |  |           |  |     `- TT
         |  |           |  +- SL
         |  |           |  |  +- S
         |  |           |  |  |  +- cf2
         |  |           |  |  |  +- :=
         |  |           |  |  |  `- E
         |  |           |  |  |     +- T
         |  |           |  |  |     |  +- F
         |  |           |  |  |     |  |  `- 2
         |  |           |  |  |     |  `- FT
         |  |           |  |  |     `- TT
         |  |           |  |  `- SL
         |  |           |  |     +- S
         |  |           |  |     |  +- pr
         |  |           |  |     |  +- :=
         |  |           |  |     |  `- E
         |  |           |  |     |     +- T
         |  |           |  |     |     |  +- F
         |  |           |  |     |     |  |  `- cf1
         |  |           |  |     |     |  `- FT
         |  |           |  |     |     |     +- mo
         |  |           |  |     |     |     |  `- *
         |  |           |  |     |     |     +- F
         |  |           |  |     |     |     |  `- cf2
         |  |           |  |     |     |     `- FT
         |  |           |  |     |     `- TT
         |  |           |  |     `- SL
         |  |           |  |        +- S
         |  |           |  |        |  +- while
         |  |           |  |        |  +- C
         |  |           |  |        |  |  +- E
         |  |           |  |        |  |  |  +- T
         |  |           |  |        |  |  |  |  +- F
         |  |           |  |        |  |  |  |  |  `- pr
         |  |           |  |        |  |  |  |  `- FT
         |  |           |  |        |  |  |  `- TT
         |  |           |  |        |  |  +- rn
         |  |           |  |        |  |  |  `- <=
         |  |           |  |        |  |  `- E
         |  |           |  |        |  |     +- T
         |  |           |  |        |  |     |  +- F
         |  |           |  |        |  |     |  |  `- cp
         |  |           |  |        |  |     |  `- FT
         |  |           |  |        |  |     `- TT
         |  |           |  |        |  +- SL
         |  |           |  |        |  |  +- S
         |  |           |  |        |  |  |  +- if
         |  |           |  |        |  |  |  +- C
         |  |           |  |        |  |  |  |  +- E
         |  |           |  |        |  |  |  |  |  +- T
         |  |           |  |        |  |  |  |  |  |  +- F
         |  |           |  |        |  |  |  |  |  |  |  `- pr
         |  |           |  |        |  |  |  |  |  |  `- FT
         |  |           |  |        |  |  |  |  |  `- TT
         |  |           |  |        |  |  |  |  +- rn
         |  |           |  |        |  |  |  |  |  `- ==
         |  |           |  |        |  |  |  |  `- E
         |  |           |  |        |  |  |  |     +- T
         |  |           |  |        |  |  |  |     |  +- F
         |  |           |  |        |  |  |  |     |  |  `- cp
         |  |           |  |        |  |  |  |     |  `- FT
         |  |           |  |        |  |  |  |     `- TT
         |  |           |  |        |  |  |  +- SL
         |  |           |  |        |  |  |  |  +- S
         |  |           |  |        |  |  |  |  |  +- found
         |  |           |  |        |  |  |  |  |  +- :=
         |  |           |  |        |  |  |  |  |  `- E
         |  |           |  |        |  |  |  |  |     +- T
         |  |           |  |        |  |  |  |  |     |  +- F
         |  |           |  |        |  |  |  |  |     |  |  `- 1
         |  |           |  |        |  |  |  |  |     |  `- FT
         |  |           |  |        |  |  |  |  |     `- TT
         |  |           |  |        |  |  |  |  `- SL
         |  |           |  |        |  |  |  `- end
         |  |           |  |        |  |  `- SL
         |  |           |  |        |  |     +- S
         |  |           |  |        |  |     |  +- cf2
         |  |           |  |        |  |     |  +- :=
         |  |           |  |        |  |     |  `- E
         |  |           |  |        |  |     |     +- T
         |  |           |  |        |  |     |     |  +- F
         |  |           |  |        |  |     |     |  |  `- cf2
         |  |           |  |        |  |     |     |  `- FT
         |  |           |  |        |  |     |     `- TT
         |  |           |  |        |  |     |        +- ao
         |  |           |  |        |  |     |        |  `- +
         |  |           |  |        |  |     |        +- T
         |  |           |  |        |  |     |        |  +- F
         |  |           |  |        |  |     |        |  |  `- 1
         |  |           |  |        |  |     |        |  `- FT
         |  |           |  |        |  |     |        `- TT
         |  |           |  |        |  |     `- SL
         |  |           |  |        |  |        +- S
         |  |           |  |        |  |        |  +- pr
         |  |           |  |        |  |        |  +- :=
         |  |           |  |        |  |        |  `- E
         |  |           |  |        |  |        |     +- T
         |  |           |  |        |  |        |     |  +- F
         |  |           |  |        |  |        |     |  |  `- cf1
         |  |           |  |        |  |        |     |  `- FT
         |  |           |  |        |  |        |     |     +- mo
         |  |           |  |        |  |        |     |     |  `- *
         |  |           |  |        |  |        |     |     +- F
         |  |           |  |        |  |        |     |     |  `- cf2
         |  |           |  |        |  |        |     |     `- FT
         |  |           |  |        |  |        |     `- TT
         |  |           |  |        |  |        `- SL
         |  |           |  |        |  `- end
         |  |           |  |        `- SL
         |  |           |  |           +- S
         |  |           |  |           |  +- cf1
         |  |           |  |           |  +- :=
         |  |           |  |           |  `- E
         |  |           |  |           |     +- T
         |  |           |  |           |     |  +- F
         |  |           |  |           |     |  |  `- cf1
         |  |           |  |           |     |  `- FT
         |  |           |  |           |     `- TT
         |  |           |  |           |        +- ao
         |  |           |  |           |        |  `- +
         |  |           |  |           |        +- T
         |  |           |  |           |        |  +- F
         |  |           |  |           |        |  |  `- 1
         |  |           |  |           |        |  `- FT
         |  |           |  |           |        `- TT
         |  |           |  |           `- SL
         |  |           |  |              +- S
         |  |           |  |              |  +- cf1s
         |  |           |  |              |  +- :=
         |  |           |  |              |  `- E
         |  |           |  |              |     +- T
         |  |           |  |              |     |  +- F
         |  |           |  |              |     |  |  `- cf1
         |  |           |  |              |     |  `- FT
         |  |           |  |              |     |     +- mo
         |  |           |  |              |     |     |  `- *
         |  |           |  |              |     |     +- F
         |  |           |  |              |     |     |  `- cf1
         |  |           |  |              |     |     `- FT
         |  |           |  |              |     `- TT
         |  |           |  |              `- SL
         |  |           |  `- end
         |  |           `- SL
         |  |              +- S
         |  |              |  +- if
         |  |              |  +- C
         |  |              |  |  +- E
         |  |              |  |  |  +- T
         |  |              |  |  |  |  +- F
         |  |              |  |  |  |  |  `- found
         |  |              |  |  |  |  `- FT
         |  |              |  |  |  `- TT
         |  |              |  |  +- rn
         |  |              |  |  |  `- ==
         |  |              |  |  `- E
         |  |              |  |     +- T
         |  |              |  |     |  +- F
         |  |              |  |     |  |  `- 0
         |  |              |  |     |  `- FT
         |  |              |  |     `- TT
         |  |              |  +- SL
         |  |              |  |  +- S
         |  |              |  |  |  +- write
         |  |              |  |  |  `- E
         |  |              |  |  |     +- T
         |  |              |  |  |     |  +- F
         |  |              |  |  |     |  |  `- cp
         |  |              |  |  |     |  `- FT
         |  |              |  |  |     `- TT
         |  |              |  |  `- SL
         |  |              |  |     +- S
         |  |              |  |     |  +- n
         |  |              |  |     |  +- :=
         |  |              |  |     |  `- E
         |  |              |  |     |     +- T
         |  |              |  |     |     |  +- F
         |  |              |  |     |     |  |  `- n
         |  |              |  |     |     |  `- FT
         |  |              |  |     |     `- TT
         |  |              |  |     |        +- ao
         |  |              |  |     |        |  `- -
         |  |              |  |     |        +- T
         |  |              |  |     |        |  +- F
         |  |              |  |     |        |  |  `- 1
         |  |              |  |     |        |  `- FT
         |  |              |  |     |        `- TT
         |  |              |  |     `- SL
         |  |              |  `- end
         |  |              `- SL
         |  |                 +- S
         |  |                 |  +- cp
         |  |                 |  +- :=
         |  |                 |  `- E
         |  |                 |     +- T
         |  |                 |     |  +- F
         |  |                 |     |  |  `- cp
         |  |                 |     |  `- FT
         |  |                 |     `- TT
         |  |                 |        +- ao
         |  |                 |        |  `- +
         |  |                 |        +- T
         |  |                 |        |  +- F
         |  |                 |        |  |  `- 1
         |  |                 |        |  `- FT
         |  |                 |        `- TT
         |  |                 `- SL
         |  `- end
         `- SL

*LLParse>
*LLParse>