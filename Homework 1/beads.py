import sys
import numpy as np
import random

def factorial(n):
    r = 1
    for i in range(1, n+1):
        r *= i
    return r

BC = int(sys.argv[1])
CC = int(sys.argv[2])
COM = factorial(BC) // (factorial(abs(BC-CC)) * factorial(CC))
if COM == 0:
    COM = factorial(CC) // (factorial(abs(CC-BC)) * factorial(BC))
print("Number of unique bracelets: {0}".format(COM))