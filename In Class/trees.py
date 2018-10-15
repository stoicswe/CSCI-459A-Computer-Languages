# start with n = 1
# build n = 2 from it =, by adding a single node in each of the available phases
# When n = N, count the size of the set.

import sys

def reprNode(self):
    if self == None:
        return '()'
    return '(' + reprNode(self.l) + reprNode(self.r) + ')'

class Node:
    def __init__(self, l, r):
        self.l = l
        self.r = r
    
    def __repr__(self):
        return reprNode(self)
    
    def __eq__(self, other):
        if self is None:
            return other is None #no stirring of the nones
        if other is None:
            return False
        return str(self) == str(other)
    
    def __hash__(self):
        return hash(str(self))
        
    
    def plus_one(self):
        if self.l == None:
            yield Node(one, self.r)
        else:
            for t in self.l.plus_one():
               yield Node(t, self.r)
        if self.r == None:
            yield Node(self.l, one)
        else:
            for t in self.r.plus_one():
                yield Node(self.l, t)

one = Node(None, None)

def build(trees):
    for t in trees:
        for tp1 in t.plus_one():
            yield tp1

if len(sys.argv) < 1:
    print("No arguments given.")
else:
    print('The arguments are:')
    i = 0
    for a in sys.argv[1:]:
        print(str(i) + ": " + a)
        i += 1
    
    N = int(sys.argv[1])
    print('N = ' + str(N))

    trees = set([one])
    for n in range(2,N+1):
        trees = set(build(trees))
    print("Tree Count:")
    print(len(trees))
    #n = 1
    #trees = one.plus_one()
    #print(set(build(trees)))