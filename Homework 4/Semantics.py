# ------------------------------------------------
# A lambda calculus interpreter.

# ------------------------------------------------
# Singleton: https://stackoverflow.com/questions/6760685/creating-a-singleton-in-python
class Singleton(type):
    _instances = {}
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super(Singleton, cls).__call__(*args, **kwargs)
        return cls._instances[cls]

# ------------------------------------------------
# Show
class Show:
    def showPrec(self, d):
        pass

    def __repr__(self): return self.showPrec(0)

def showParen(b, s):
    if b:
        return "({})".format(s)
    return s

# ------------------------------------------------
# Names
class Name:
    def __init__(self, s, i):
        self.s = s
        self.i = i

    def __repr__(self):
        if self.i == 0:
            return self.s
        return "{}{}".format(self.s, self.i)

    def __eq__(self,o):
        return (self.__class__ == o.__class__ and 
                self.s == o.s and self.i == o.i)

    def __hash__(self):
        return hash(self.s) ^ hash(self.i)

    def next(self, ns):
        n = self
        while (n in ns):
            n = Name(n.s, n.i+1)
        return n, ns.union({n})


def name(n): return Name(n,0)

class HasVars:
    def freeVars(self):
        return set()
    def allVars(self):
        return set()

# ------------------------------------------------
# Expressions
class E(Show,HasVars):
    pass

class Lam(E):
    def __init__(self, n, t, e):
        self.n = n
        self.t = t
        self.e = e

    def freeVars(self): return self.e.freeVars().difference({self.n})
    def allVars(self): return self.e.allVars()
    
    def showPrec(self, d):
        if self.t.isany():
            return showParen(d >= 1, "\\{}. {}".format(self.n, self.e.showPrec(0)))    
        return showParen(d >= 1, "\\{}:{}. {}".format(self.n, self.t, self.e.showPrec(0)))

class App(E):
    def __init__(self, f, a):
        self.f = f
        self.a = a

    def freeVars(self): return self.f.freeVars().union(self.a.freeVars())
    def allVars(self): return self.f.allVars().union(self.a.allVars())

    def showPrec(self, d):
        if isinstance(self.f, App):
            return showParen(d >= 1, "{} {}".format(self.f.showPrec(0), self.a.showPrec(1)))
        else:
            return showParen(d >= 1, "{} {}".format(self.f.showPrec(1), self.a.showPrec(1)))

class Var(E):
    def __init__(self, n):
        self.n = n

    def freeVars(self): return { self.n }
    def allVars(self): return { self.n }
    
    def showPrec(self, d): return str(self.n)

class MV(E):
    def __init__(self, n):
        self.n = n

    def showPrec(self, d): return self.n

def var(n): return Var(name(n))

#################################
# TODO: Add new expression representations here!

class ENum(E):
    def __init__(self, n):
        self.n = n
    # Not sure if this is correct or not
    def showPrec(self, d): return str(self.n)

class EList(E):
    def __init__(self, es):
        self.es = es
    def showPrec(self, d):
        return showParen(d > 0, str(sumREE(self.es)))

class EPair(E):
    def __init__(self, e1, e2):
        self.e1 = e1
        self.e2 = e2
    def showPrec(self, d):
        return showParen(d >= 0, "PAIR {} {}".format(self.e1.showPrec(1), self.e2.showPrec(1)))

class __ENil(E, metaclass=Singleton):
    def __init__(self):
        pass
    def showPrec(self, d):
        return "NIL"
ENil = __ENil()

class ELeft(E):
    def __init__(self, e):
        self.e = e
    def showPerc(self):
        return str(self.e)

class ERight(E):
    def __init__(self, e):
        self.e = e
    def showPerc(self):
        return str(self.e)
#   ...

# ------------------------------------------------
# Types
class T(Show, HasVars):
    def isany(self): return False

class TFun(T):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def freeVars(self):
        return self.a.freeVars().union(self.b.freeVars())
    
    def showPrec(self, d):
        return showParen(d >= 1, "{} -> {}".format(self.a.showPrec(1), self.b.showPrec(0)))

class TPair(T):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def freeVars(self):
        return self.a.freeVars().union(self.b.freeVars())
    
    def showPrec(self, d):
        return showParen(d >= 1, "{} -> {}".format(self.a.showPrec(1), self.b.showPrec(0)))

class TVar(T):
    def __init__(self, n):
        self.n = n

    def freeVars(self): return { self.n }

    def showPrec(self, d): return str(self.n)

    def isany(self): return str(self.n) == "any" 

def tvar(n): return TVar(name(n))

class __TBool(T,metaclass=Singleton):
    pass
TBool = __TBool()

class __TNum(T,metaclass=Singleton):
    pass
TNum = __TNum()

class __TUnit(T,metaclass=Singleton):
    pass
TUnit = __TUnit()

# ------------------------------------------------
# Parsing

class ParseError:
    def __init__(self, message):
        self.message = message
    def __repr__(self):
        return "Error: " + self.message

def cons(x, xs):
    yield x
    for x in xs:
        yield x

def fail(m):
    return ParseError(m)

def stop(r):
    return isinstance(r, ParseError)

def emap(f, r):
    if stop(r): return r
    return f(r)

class Parser:
    def __init__(self, s):
        self.s = s
        self.i = 0

    def next(self):
        if self.i == len(self.s):
            return fail("Unexpected end of input.")
        c = self.s[self.i]
        self.i = self.i + 1
        return c

    def peek(self):
        if self.i == len(self.s):
            return '\0'
        return self.s[self.i]

    def matchChar(self, c, m):
        if not c(self.peek()):
            return fail("Expected " + m + " character.")
        return self.next()

    def char(self, c):
        return self.matchChar(lambda x: x == c, c)

    def eof(self, r):
        if stop(r): return r
        if self.i == len(self.s): return r
        return fail("Expected end of input.")

    def lower(self): return self.matchChar(str.islower, "lowercase")
    def upper(self): return self.matchChar(str.isupper, "uppercase")
    def digit(self): return self.matchChar(str.isdigit, "digit")
    
    def many(self, p):
        while True:
            r = p(self)
            if stop(r): return
            yield r

    def many1(self, p):
        r = p(self)
        if stop(r): return r
        return cons(r, self.many(p))

    def manyStr(self, p):
        return emap(''.join, self.many(p))

    def many1Str(self, p):
        return emap(''.join, self.many1(p))

    def oneOf(self, ps):
        for p in ps:
            r = p(self)
            if not stop(r): return r
        return ParseError("No choices matched.")
    
    def oneOfChar(self, cs):
        for c in cs:
            r = self.char(c)
            if not stop(r): return r
        return ParseError("No choices of {} matched.".format(cs))
    
    def seq(self, ps):
        for p in ps:
            r = p(self)
            if stop(r): return r
            yield r

    def ident(self):
        l = self.many1Str(Parser.lower)
        if stop(l): return l
        n = self.manyStr(Parser.digit)
        if stop(n): return n
        if len(n) == 0:
            return name(l)
        return Name(l, int(n))

    def whitespace(self):
        return self.oneOfChar(" \t\n")

    def metaVar(self):
        n = self.many1Str(Parser.upper)
        if stop(n):
            n = self.many1Str(Parser.digit)
            if stop(n): return n
        return MV(n)

    def expr(self):
        list(self.many(Parser.whitespace))
        e = self.lam()
        if stop(e): return self.app()
        return e

    def exprApp(self):
        r = self.oneOf([Parser.lam, Parser.paren, Parser.exprVar, Parser.metaVar])
        if not stop(r): list(self.many(Parser.whitespace))
        return r
    
    def lam(self):
        r = self.oneOfChar("\\λ")
        if stop(r): return r
        x = self.ident()
        if stop(x): return x
        r = self.char(".")
        if stop(r): return r
        b = self.expr()
        if stop(b): return b
        return Lam(x, tvar("any"), b)

    def paren(self):
        r = self.char('(')
        if stop(r): return r
        e = self.expr()
        if stop(e): return e
        list(self.many(Parser.whitespace))
        r = self.char(')')
        if stop(r): return r
        return e

    def app(self):
        f = self.exprApp()
        if stop(f): return f
        es = list(self.many(Parser.exprApp))
        if stop(es): return es
        a = f
        for e in es:
            a = App(a,e)
        return a

    def exprVar(self):
        return emap(Var, self.ident())

    def typeT(self):
        list(self.many(Parser.whitespace))
        t = self.typeParen()
        if stop(t): t = self.typeBase()
        if stop(t): return t
        list(self.many(Parser.whitespace))
        r = self.typeArrow(t)
        if stop(r): return t
        return r

    def typeParen(self):
        r = self.char('(')
        if stop(r): return r
        t = self.typeT()
        if stop(t): return t
        list(self.many(Parser.whitespace))
        r = self.char(')')
        if stop(r): return r
        return t

    def typeBase(self):
        n = self.ident()
        if stop(n): return n
        list(self.many(Parser.whitespace))
        if n == name("pair"):
            a = self.typeT()
            if stop(a): return a
            b = self.typeT()
            if stop(b): return b
            return TPair(a,b)

        def baseType(n):
            if n == name("num"):  return TNum
            if n == name("bool"): return TBool
            if n == name("unit"): return TUnit
            return TVar(n)

        return baseType(n)
        
    def typeArrow(self, a):
        r = self.char("-")
        if stop(r): return r
        r = self.char(">")
        if stop(r): return r
        b = self.typeT()
        if stop(b): return b
        return TFun(a, b)

def parseExpr(s):
    p = Parser(s)
    return p.eof(p.expr())
    
def parseType(s):
    p = Parser(s)
    return p.eof(p.typeT())


# ------------------------------------------------
# Semantics

# Capture avoiding substitution.
def subE(x, s, b):
    fvs = s.freeVars()
    vs0 = fvs.union(b.allVars())
    def sub(vs, e):
        if isinstance(e, Var):
            if e.n == x:
                return s
            return e
        elif isinstance(e, Lam):
            if e.n == x:
                return e
            elif e.n in fvs:
                v1,vs1 = e.n.next(vs)
                e1 = subE(e.n, Var(v1), e.e)
                return Lam(v1, e.t, sub(vs1, e1))
            else:
                return Lam(e.n, e.t, sub(vs, e.e))
        elif isinstance(e, App):
            return App(sub(vs, e.f), sub(vs, e.a))
        return e
    return sub(vs0, b)

# Substitution rule
def sub(e):
    if isinstance(e, App) and isinstance(e.f, Lam):
        return subE(e.f.n, e.a, e.f.e)
    return None

# Argument rule
def arg(s, e):
    if isinstance(e, App):
        r = s(e.a)
        if r is not None:
            return App(e.f, r)
    return None

# Function rule
def func(s, e):
    if isinstance(e, App):
        r = s(e.f)
        if r is not None:
            return App(r, e.a)
    return None

# Lambda body rule (not used).
def body(s, e):
    if isinstance(e, Lam):
        r = s(e.b)
        if r is not None:
            return Lam(e.n, e.t, r)
    return None

# Meta-variable expansion.
#  You will make your own version of this function that includes higher-level
#  semantics.
def mvExpand(e):
    if isinstance(e, App) and isinstance(e.f, MV) and e.f.n in metaVars:
        i = metaVars[e.f.n]
        return App(i, e.a)
    return None

# Apply rules.
#  `step` will apply rules by first attempting to apply an argument
#  to a lambda expression.  If the expression is not in the right form
#  for that try expanding a meta variable that is applied to an argument.
#  Otherwise, step into the function side and try again, or the argument
#  side and try again.
def stepExpand(e):
    r = sub(e)
    if r is not None: return r
    r = mvExpand(e)
    if r is not None: return r
    r = func(stepExpand, e)
    if r is not None: return r
    return arg(stepExpand, e)

# Evaluate with 
# Apply the given `step` function `s` 
def evalStepsWith(s,e):
    n = 0
    print(e)
    while True:
        next = s(e)
        if next is None or next == e: return (n, e) 
        n = n + 1
        print(next)
        e = next

# Evaluate without printing intermediate
# steps.
def evalWith(s,e):
    n = 0
    while True:
        next = s(e)
        if next is None or next == e:
            print(e)
            return (n, e)
        n = n + 1
        e = next

def evalStepsExpand(e):
    return evalStepsWith(stepExpand, e)

def evalExpand(e):
    return evalWith(stepExpand, e)

######################################################
# YOUR MAIN MODIFICATIONS HAPPEN HERE

# Add entries to this as support is added for each meta-variable.
highLevelMetaVars = set(["NIL", "Z", "0", "1", "2", "3", "4", "5", "PRED", "SUCC", "SUB", "PLUS", "MULT", "POW", "LEQ", "ISZERO", "PAIR", "FIRST", "SECOND", "ISNIL"])
# set(["Z", "0", "1", "2", "3", "4", "PRED"])

# Meta-variable expansion.
def mvHighLevel(e):
    if isinstance(e, App) and isinstance(e.f, App) and isinstance(e.f.f, App):
        if isinstance(e.f.f.f, MV) and e.f.f.f.n == "EITHER":
            if isinstance(e.a, ELeft):
                return App(e.f.f.a, e.a.e)
            if isinstance(e.a, ERight):
                return App(e.f.a, e.a.e)


    if isinstance(e, App) and isinstance(e.f, App):
        if isinstance(e.f.f, MV) and e.f.f.n == "SUB":
            if isinstance(e.a, ENum) and isinstance(e.f.a, ENum):
                if (e.f.a.n - e.a.n) < 0:
                    return ENum(0)
                return ENum(e.f.a.n - e.a.n)
        if isinstance(e.f.f, MV) and e.f.f.n == "PLUS":
            if isinstance(e.a, ENum) and isinstance(e.f.a, ENum):
                return ENum(e.a.n + e.f.a.n)
        if isinstance(e.f.f, MV) and e.f.f.n == "MULT":
            if isinstance(e.a, ENum) and isinstance(e.f.a, ENum):
                return ENum(e.a.n * e.f.a.n)
        if isinstance(e.f.f, MV) and e.f.f.n == "POW":
            if isinstance(e.a, ENum) and isinstance(e.f.a, ENum):
                return ENum(e.f.a.n ** e.a.n)
        if isinstance(e.f.f, MV) and e.f.f.n == "LEQ":
            if isinstance(e.a, ENum) and isinstance(e.f.a, ENum):
                if e.f.a.n <= e.a.n:
                    return MV("TRUE")
                return MV("FALSE")
        if isinstance(e.f.f, MV) and e.f.f.n == "PAIR":
            return EPair(e.f.a, e.a)
        
    if isinstance(e, App) and isinstance(e.f, MV) and e.f.n in metaVars:
        # Fallback on meta-variable expansion if we do not have high
        # level semantics for this meta variable yet.
        if not e.f.n in highLevelMetaVars: return mvExpand(e)

        # Here we know that `e.f.n` is a known meta variable and it is
        # applied to `e.a`.  Instead of replacing it with its expansion,
        # apply high-level semantics:

        if e.f.n == "PRED": #...
        #   # If the argument is not in the correct form first, just return `None`:
            if isinstance(e.a, ENum):
              return ENum(e.a.n - 1) # Perform the high-level computation ...
            return None
        if e.f.n == "SUCC": #...
        #   # If the argument is not in the correct form first, just return `None`:
            if isinstance(e.a, ENum):
              return ENum(e.a.n + 1) # Perform the high-level computation ...
            return None
        if e.f.n == "ISZERO":
            if isinstance(e.a, ENum):
                if e.a.n == 0:
                    return MV("TRUE")
                return MV("FALSE")
        if e.f.n == "FIRST":
            if isinstance(e.a, EPair):
                return e.a.e1
        if e.f.n == "SECOND":
            if isinstance(e.a, EPair):
                return e.a.e2
        if e.f.n == "ISNIL":
            if e.a is ENil:
                return MV("TRUE")
            if isinstance(e.a, App):
                return None
            if isinstance(e.a, MV):
                return None
            if isinstance(e.a, Var):
                return None
            return MV("FALSE")
        #
        # Define sub, mult, plus, leq, isZero
        #
        
        return None
    elif isinstance(e, MV):
        # Here we have a meta-variable that we may or may not want to convert
        # to a new representation.  For instance, we may have a number '5'.
        # We can convert that to a new kind of expression `ENum`:
        if e.n == 'Z':
           return ENum(0)
        if e.n == '0':
           return ENum(0)
        if e.n == '1':
           return ENum(1)
        if e.n == '2':
           return ENum(2)
        if e.n == '3':
           return ENum(3)
        if e.n == '4':
           return ENum(4)
        if e.n == '5':
           return ENum(5)
        if e.n == 'NIL':
            return ENil
        return None
    return None

def stepHighLevel(e):
    r = sub(e)
    if r is not None: return r
    r = mvHighLevel(e)
    if r is not None: return r
    r = func(stepHighLevel, e)
    if r is not None: return r
    return arg(stepHighLevel, e)

def evalStepsHighLevel(e):
    return evalStepsWith(stepHighLevel, e)

def evalHighLevel(e):
    return evalWith(stepHighLevel, e)

# Meta-variable definitions.
a = tvar("a")
b = tvar("b")
c = tvar("c")

exprs = {
    'F':      ("λx. λy. y",     TBool),
    'T':      ("λx. λy. x",     TBool),
    'FALSE':  ("λx. λy. y",     TBool),
    'TRUE':   ("λx. λy. x",     TBool),
    'AND':    ("λp. λq. p q p", TFun(TBool, TBool)),
    'OR':     ("λp. λq. p p q", TFun(TBool, TBool)),
    'NOT':    ("λp. p F T",     TFun(TBool, TBool)),
    
    'Z':      ("λf. λx. x",                   TNum),
    '0':      ("λf. λx. x",                   TNum),
    '1':      ("λf. λx. f x",                 TNum),
    '2':      ("λf. λx. f (f x)",             TNum),
    '3':      ("λf. λx. f (f (f x))",         TNum),
    '4':      ("λf. λx. f (f (f (f x)))",     TNum),
    '5':      ("λf. λx. f (f (f (f (f x))))", TNum),

    'SUCC':   ("λn. λf. λx. f (n f x)",                           TFun(TNum, TNum)),
    'PLUS':   ("λm. λn. λf. λx. m f (n f x)",                     TFun(TNum, TFun(TNum, TNum))),
    'MULT':   ("λm. λn. λf. m (n f)",                             TFun(TNum, TFun(TNum, TNum))),
    'POW':    ("λb. λe. e b",                                     TFun(TNum, TFun(TNum, TNum))),
    'PRED':   ("λn. λf. λx. n (λg. λh. h (g f)) (λu. x) (λu. u)", TFun(TNum, TNum)),
    'SUB':    ("λm. λn. n PRED m",                                TFun(TNum, TNum)),
    'ISZERO': ("λn. n (λx. F) T",                                 TFun(TNum, TBool)),
    'LEQ':    ("λm. λn. ISZERO (SUB m n)",                        TFun(TNum, TFun(TNum, TBool))),

    'Y':      ("λg. (λx. g (x x)) (λx. g (x x))",                 TFun((TFun(a, a)),a)),
    'IF':     ("λp. λa. λb. p a b",                               TFun(TBool, TFun(a, TFun(a, a)))),
    'I':      ("λx. x",                                           TFun(a, a)),
    'K':      ("λx. λy. x",                                       TFun(a, TFun(b, a))),
    'S':      ("λx. λy. λz. x z (y z)",                           TFun((TFun(a, TFun(b, c))),TFun((TFun(a, b)), TFun(a, c)))),
            
    'PAIR':   ("λx. λy. λf. f x y",                               TFun(a, TFun(b, TPair(a,b)))),
    'FIRST':  ("λp. p T",                                         TFun(TPair(a,b),a)),
    'SECOND': ("λp. p F",                                         TFun(TPair(a,b),b)),
    'NIL':    ("F",                                               TUnit),
    'CONS':   ("PAIR",                                            TUnit),
    'HEAD':   ("FIRST",                                           TUnit),
    'TAIL':   ("SECOND",                                          TUnit),
    'ISNIL':  ("λl. l (λh. λt. λd. FALSE) TRUE",                  TUnit),
    'LEFT' :  ("PAIR FALSE",                                      TUnit),
    'RIGHT':  ("PAIR TRUE",                                       TUnit),
    'EITHER': ("λl . λr. λe. IF (FIRST e) (r SECOND e) (l SECOND e)",           TUnit),
}
metaVars = {k: parseExpr(e) for k,(e,t) in exprs.items()}

# Example programs
fibStr = """Y (λr. λn.
                (IF (ISZERO n)
                        (SUCC Z)
                        (IF (ISZERO (PRED n))
                        (SUCC Z)
                        (PLUS (r (PRED n))
                                (r (PRED (PRED n)))))))"""
def fibE(n):
    e = fibStr + " {} s z".format(n)
    return parseExpr(e)

factStr = """Y (λr. λn. IF (ISZERO n)
                             (SUCC Z)
                             (MULT n (r (PRED n))))"""
def factE(n):
    e = factStr + " {} s z".format(n)
    return parseExpr(e)

sumStr = """Y (λsum. λl.
                IF (ISNIL l)
                    Z
                    (PLUS (HEAD l)
                        (sum (TAIL l))
                ))"""

def sumC(l):
    e = sumStr + " ({}) s z".format(l)
    return parseExpr(e)

def sumRE(l):
    e = sumStr + " ({}) s z".format(sumREE(l))
    return parseExpr(e)

def sumREE(ns):
    e = MV("NIL")
    for n in reversed(ns):
        e = App(App(MV("CONS"), ENum(n)), e)
    return e

"""def sumREE(ns):
    e = MV("NIL")
    for n in ns:
        e = App(App(MV("CONS"), ENum(n)), e)
    return e"""

primesStr = """
    (λmap. λminus. λlast. λnprimes. last
        (   (Y (λeuler. λm. λps.
                    IF (NOT (LEQ (MULT (HEAD ps) (HEAD ps)) m))
                        ps
                        (CONS (HEAD ps) (euler m (minus (TAIL ps)
                                                    (map (MULT (HEAD ps)) ps))))
                )
            )
            nprimes
            (   (Y (λto. λn. λm.
                        IF (LEQ n m)
                            (CONS n (to (SUCC n) m))
                            NIL
                    )
                )
                2
                nprimes
            )
        )
    )
    (Y (λmap. λf. λl.
            IF (ISNIL l)
                NIL
                (CONS (f (HEAD l)) (map f (TAIL l)))
        )
    )
    (Y (λminus. λa. λb.
            IF (OR (ISNIL a) (ISNIL b)) a
                (IF (LEQ (SUCC (HEAD a)) (HEAD b))
                    (CONS (HEAD a) (minus (TAIL a) b))
                    (IF (LEQ (HEAD a) (HEAD b))
                        (minus (TAIL a) (TAIL b))
                        (minus a (TAIL b))
                    )
                )
        )
    )
    (Y (λlast. λl.
          IF (ISNIL (TAIL l))
             (HEAD l)
             (last (TAIL l))
       )
    )
    {} s z"""

def primesE(n):
    e = primesStr.format(n)
    return parseExpr(e)

def signedNumber(x):
    if x < 0:
        return "LEFT " + str(-x)
    return "RIGHT " + str(x)

# Program to add two "signed" numbers.
plusNegStr = """
    (λplus. EITHER (λn. n s z neg) (λp. p s z pos) (plus ({}) ({})))
    (λa. λb.
        EITHER (λna.
            EITHER (λnb. LEFT (PLUS na nb))
                   (λpb. IF (LEQ na pb)
                            (RIGHT (SUB pb na))
                            (LEFT  (SUB na pb)))
                   b)
               (λpa.
            EITHER (λnb. IF (LEQ nb pa)
                            (RIGHT (SUB pa nb))
                            (LEFT  (SUB nb pa)))
                   (λpb. RIGHT (PLUS pa pb))
                   b)
            a)
    """

def plusNegE(a, b):
    e = plusNegStr.format(signedNumber(a), signedNumber(b))
    return parseExpr(e)
