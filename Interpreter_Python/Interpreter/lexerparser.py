# ------------------------------------------------------------
# lexerparser.py
#
# tokenizer for guarded command syntax
# and parser for commands
# ------------------------------------------------------------


# Test it out
data = '''
test a b - "$out$"{
    out = exec "IF $a$ GEQ $b$ (ECHO 1) ELSE (ECHO 0)"
}

maxnum a b c - "$max$" {
    ab = test "$a$" "$b$" ;
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

reserved = {'finally': 'TKFinally',
            'exec': 'TKExec',
            'split': 'TKSplit',
}
tokens = [
             #'TKProcedure',
             'TKEquals',
             'TKNotEquals',
             'TKColon',
             'TKOpenBracket',
             'TKCloseBracket',
             'TKMinus',
             'TKAssignment',
             'TKSemicolon',
             'TKName',
             'TKString',] + list(reserved.values())

# Regular expression rules for simple tokens
t_TKSemicolon = r'\;'
t_TKColon = r'\:'
t_TKOpenBracket = r'\{'
t_TKCloseBracket = r'\}'


#def t_TKPrecedure(t):
#    r'([a-zA-Z].*?)\s?-\s?("[^\n]+")\s?\{\s*(.+?;)\s*\}'
#    return t


#def t_TKProcCall(t):
#    r'(?<=\n)([a-zA-Z].*?) = (?:([a-zA-Z]\w*?) +(".+?");)'
#    return t

#def t_TKVariable(self, t):
#    r'(?<=\$)[A-Za-z]\w*(?=\$)'
#    return t

def t_TKName(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'TKName')
    return t


def t_TKEquals(t):
    r'\s?==\s?'
    return t


def t_TKNotEquals(t):
    r'\s?!=\s?'
    return t


def t_TKMinus(t):
    r'\s?-\s?'
    return t


def t_TKString(t):
    r'(?<=\s)\".*?\"(?<!\s)'
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_TKAssignment(t):
    r'\s?=\s?'
    return t

# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'

# Error handling rule
def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

#import lexer
import PLY.lex as lex

# Build the lexer
lex.lex()
lexer = lex.lex()
lexer.input(data)

while True:
             tok = lexer.token()
             if not tok: break
             print tok


######### TODO Parsing rules

# dictionary of names of variable
varnames = { }
procname = { }

def p_program(p):
    ''' program : procedure
                | assignment
    '''

def p_procedure(p):
    ''' procedure : TKName valuelist TKMinus valuelist TKOpenBracket guardedcommand TKCloseBracket'''
    pass

def p_valuelist(p):
    ''' valuelist : valuelist TKString
                  | TKString
    '''
    pass

def p_guardedcommand(p):
    ''' guardedcommand : guard TKColon guardedcommand
                       | guard TKColon command TKSemicolon
    '''

def p_guard(p):
    ''' guard : TKName
            | TKName TKEquals TKString
            | TKName TKNotEquals TKString
            | TKName TKFinally
    '''
    # TODO check bedingungen
    pass

def p_command(p):
    ''' command : assignment
                | TKName TKAssignment TKExec valuelist
                | TKName TKAssignment TKSplit valuelist
                | TKName TKAssignment valuelist
    '''
    pass

def p_assignment(p):
    '''assignment : TKName TKAssignment valuelist TKSemicolon
                  | TKName TKAssignment TKName valuelist TKSemicolon'''
    # TODO assign TKname value of list
    pass

def p_error(p):
    print "Syntax error at '%s'" % p.value


import PLY.yacc as yacc
yparser = yacc.yacc()

yparser.parse(data)
