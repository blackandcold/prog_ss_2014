__author__ = 'Johannes'


import sys
from lexerparser import MyLexer
from lexerparser import MyParser
from pprint import pprint
from ASTInterpreter import ASTInterpreter
# Test it out
data = '''
test a b - "$out$"{
    out = exec "IF $a$ GEQ $b$ (ECHO 1) ELSE (ECHO 0)";
    head tail = split "/" "a/s/d/f";
}

maxnum a b c - "$max$" {
    head tail = split "/" "a/s/d/f";
    ab = test "$a$" "$b$";
    bc = exec "IF $b$ GEQ $c$ (ECHO 1) ELSE (ECHO 0)";
    ab == "0" : bc == "0" : max = "$c$";
    ab == "0" : bc == "1" : max = "$b$";
    ab == "1" : bc == "1" : max = "$a$";
    ab == "1" : bc == "0" : ac = exec "IF $a$ GEQ $c$ (ECHO 1) ELSE (ECHO 0)";
    ac == "0" : max = "$c$";
    ac == "1" : max = "$a$";
    finally : max = "error";
}

out = maxnum "2" "6" "7";
outn = maxnum "4" "2" "3";
outm = maxnum "2" "2" "2";
'''

# Build the lexer and try it out
m = MyLexer()
m.build()           # Build the lexer
m.test(data)     # Test it

# Initialize Parser
p = MyParser()

# Test Parser and Lexer to get AST
ast = p.parse(data)
pprint(ast)


# now parse AST and fill structures
astInterpreter = ASTInterpreter()
astInterpreter.interpreteAST(ast)

