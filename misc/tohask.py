# -*- encoding: utf-8 -*-
import tables

print """{-# LANGUAGE GADTs #-}
module LR (parse) where
import Rep
import LRRep
import Tokenize (Token(..), Pos)
import Control.Monad.State

type Tokens = [(Token, Pos, Pos)]
type Parser a = State Tokens a

peek :: Parser Token 
peek = do
    list <- get
    case list of
        [] -> pure TEnd
        ((x,p,q):xs) -> pure x

shift :: Parser ()
shift = modify f where f [] = []
                       f (x:xs) = xs"""



print """
data Status a where"""

for index, items in enumerate(tables.priors):
    if index == 0:
        print "    S{} :: Status Empty".format(index) 
    else:
        root = "a"
        for item in items:
            if item.startswith('"'):
                root = "(Ckeyword {})".format(root)
            else:
                root = "(C{} {})".format(item, root)
        print "    S{} :: Status {}".format(index, root) 

print """
parse :: Tokens -> (Maybe [Statement], Tokens)
parse = runState (run S0 SEmpty)

run :: Status a -> a -> Parser (Maybe [Statement])
run s stack = do
    c <- peek
    run' s stack c

run' :: Status a -> a -> Token -> Parser (Maybe [Statement])"""

def generate_reduction(index, arg):
    lhs, count = tables.rstate[arg]
    if count == 0:
        red = "reduction{}".format(arg)
        rule = "goto{} S{} (S{} stack S{} {})".format(lhs.title(), index, lhs, index, red)
        return rule
    else:
        to_remove = tables.priors[index][-count:]
    top = 'stack'
    args = []
    k = 0
    for item in to_remove:
        if item.startswith('"'):
            top = "(Skeyword {} {})".format(top, ("s" if top == "stack" else "_"))
        else:
            top = "(S{} {} {} x{})".format(item, top, ("s" if top == "stack" else "_"), k)
            args.append("x{}".format(k))
            k += 1
    red =  "(reduction{} {})".format(arg, " ".join(args))
    if lhs is None:
        rule = "case stack of\n    {} -> pure {}".format(top, red)
    else:
        rule = "case stack of\n    {} ->\n        goto{} s (S{} stack s {})".format(top, lhs.title(), lhs, red)
    return rule

gotos = {}
for index, table in enumerate(tables.state):
    for sym, (act, arg) in table.iteritems():
        if sym is None:
            assert act == 1
            rule = generate_reduction(index, arg)
            print "run' S{} stack TEnd = {}".format(index, rule)
        elif sym.startswith('"'):
            if act == 0:
                rule = "shift >> run S{} (Skeyword stack S{})".format(arg, index)
            else:
                rule = generate_reduction(index, arg)
            print "run' S{} stack (TKeyword {}) = {}".format(index, sym.encode('utf-8'), rule)
        elif sym in tables.lexemes:
            if act == 0:
                rule = "shift >> run S{} (S{} stack S{} arg)".format(arg, sym, index)
            else:
                rule = generate_reduction(index, arg)
            print "run' S{} stack (T{} arg) = {}".format(index, sym.title(), rule)
        else:
            if sym not in gotos:
                gotos[sym] = {}
            gotos[sym][index] = (act, arg)

for name, table in gotos.iteritems():
    print "\ngoto{} :: Status a -> (C{} a) -> Parser (Maybe [Statement])".format(name.title(), name)
    for index, (act, arg) in sorted(table.items()):
        assert act == 0
        print "goto{} S{} = run S{}".format(name.title(), index, arg)
    print "goto{} _ = \_ -> pure Nothing".format(name.title())


print """
data Empty = SEmpty
data Ckeyword a = Skeyword a (Status a)
data Cinteger a = Sinteger a (Status a) Integer
data Cstring a = Sstring a (Status a) String
data Cid a = Sid a (Status a) String
data Creal a = Sreal a (Status a) String
"""
for name in gotos:
    print "data C{} a".format(name).ljust(21), "= S{} a (Status a) T{}".format(name, name)

#for kw in tables.lexemes:
#    print kwo



#data Token = TInteger Integer
#           | TString String
#           | TKeyword String
#           | TId String
#           | TReal String
#           | TEnd
#           deriving (Show, Eq)

