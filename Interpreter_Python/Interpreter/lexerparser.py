# ------------------------------------------------------------
# lexerparser.py
#
# tokenizer for guarded command syntax
# and parser for commands
# ------------------------------------------------------------
import lexer
import context
import gparser

# Test it out
data = '''
maxnum a b c - "$max$" {
    ab = exec "test $a$ -le $b$";
    bc = exec "test $b$ -le $c$";
    ab == "0" : bc == "0" : max = "$c$";
    ab == "0" : bc == "1" : max = "$b$";
    ab == "1" : bc == "1" : max = "$a$";
    ab == "1" : bc == "0" : ac = exec "test $a$ -le $c$";
    ac == "0" : max = "$c$";
    ac == "1" : max = "$a$";
    finally : max = "error";
}
'''

gLex = lexer.GuardedLexer(data)
gLex.build()           # Build the lexer
tokens = gLex.lexit()     # Test it

print tokens

gPars = gparser.GuardParser(tokens)

outprocedurelist = gPars.parseLexerArray()

#gContext = context.GuardContext(outprocedurelist)



