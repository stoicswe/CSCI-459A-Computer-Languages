Author: Nathaniel Bunch
Submission Date: 12/6/2018

Description of Project: This project was designed to teach the use of lambda claculus expressions.

This project contains my solution to the problem that was given to us. It solves all the example
programs according to sepcifications. The primary difficulty was debugging the lambda code.
The hardest one to debug was definately the lists, as there was many cases in which the list
could not work, as there was many lambda expression dependencies. Also, with the horrific syntax
of python added to the difficulty to match the different object wrappers for the lambda expressions.

----------------------------------------------------------------------------------------------------

Test cases and results:

>>> evalHighLevel(fibE(4))
5 s z
(230, 5 s z)
>>> evalHighLevel(factE(4))
24 s z
(88, 24 s z)
>>> evalHighLevel(sumRE([1,2,3,4]))
10 s z
(155, 10 s z)
>>> evalHighLevel(primesE(3))
3 s z
(351, 3 s z)
>>> evalHighLevel(primesE(4))
3 s z
(3255, 3 s z)
>>> evalHighLevel(plusNegE(-1, 3))
2 s z pos
(124, 2 s z pos)
>>> evalHighLevel(plusNegE(-3, 1))
2 s z neg
(124, 2 s z neg)
>>> evalHighLevel(plusNegE(1, 1))
2 s z pos
(92, 2 s z pos)
>>> evalHighLevel(plusNegE(-1, -3))
4 s z neg
(92, 4 s z neg)
>>> 
