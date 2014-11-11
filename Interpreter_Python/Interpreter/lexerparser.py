__author__ = 'Johannes'
# ------------------------------------------------------------
# lexerparser.py
#
# tokenizer for guarded command syntax
# and parser for commands
# ------------------------------------------------------------


#import lexer
import PLY.lex as lex

class MyLexer:
    def __init__(self):
        self.lexer = lex.lex(module=self)
    reserved = {'finally': 'TKFinally',
                'exec': 'TKExec',
                'split': 'TKSplit',
    }
    tokens = [
                 'TKEquals',
                 'TKNotEquals',
                 'TKColon',
                 'TKOpenBracket',
                 'TKCloseBracket',
                 'TKMinus',
                 'TKAssignment',
                 'TKSemicolon',
                 'TKName',
                 'TKString'] + list(reserved.values())

    # Regular expression rules for simple tokens
    t_TKSemicolon = r'\;'
    t_TKColon = r'\:'
    t_TKOpenBracket = r'\{'
    t_TKCloseBracket = r'\}'
    t_TKMinus = r'\-'

    def t_TKName(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = self.reserved.get(t.value, 'TKName')
        return t

    def t_TKEquals(self, t):
        r'\s?==\s?'
        return t

    def t_COMMENT(self, t):
        r'\#.*'
        pass
        # No return value. Token discarded

    def t_TKNotEquals(self, t):
        r'\s?!=\s?'
        return t

    def t_TKString(self, t):
        r'(?<=\s)\".*?\"(?<!\s)'
        return t

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    def t_TKAssignment(self, t):
        r'\s?=\s?'
        return t

    # A string containing ignored characters (spaces and tabs)
    t_ignore = ' \t'

    # Error handling rule
    def t_error(self, t):
        print "Illegal character '%s'" % t.value[0]
        t.lexer.skip(1)

    # Build the lexer
    def build(self, **kwargs):
        self.lexer = lex.lex(module=self, **kwargs)

    # Test it output
    def test(self,data):
        self.lexer.input(data)
        while True:
             tok = self.lexer.token()
             if not tok: break
             print tok


import PLY.yacc as yacc

######### Parsing rules
class MyParser:
    def __init__(self):
        self.lexer = MyLexer()
        self.tokens = self.lexer.tokens
        self.parser = yacc.yacc(module=self, debug=True)

    def p_program(self, p):
        ''' program : program procedure
                    | procedure'''
        if (len(p) == 3):
             p[0] = p[1] + [p[2]]
        elif (len(p) == 2):
            p[0] = [p[1]]

    def p_procedure(self, p):
        '''procedure : valuelist TKAssignment TKName parameterlist TKSemicolon
                     | TKName valuelist TKMinus parameterlist TKOpenBracket guardedcommandlist TKCloseBracket'''
        if (len(p) == 6):
            p[0] = ('proccall', (p[1], p[2], p[3], p[4]))
        elif (len(p) == 8):
            p[0] = ('procscript', (p[1], p[2], p[3], p[4], p[5], p[6], p[7]))

    def p_guardedcommandlist(self, p):
        ''' guardedcommandlist : guardedcommandlist guardedcommand
                               | guardedcommand
        '''
        if (len(p) == 2):
            p[0] = p[1]
        elif (len(p) == 3):
            p[0] = p[1] + p[2]

    def p_guardedcommand(self, p):
        '''guardedcommand : guard TKColon guardedcommand
                          | guard TKColon command
                          | command TKSemicolon
        '''
        if (len(p) == 3):
            p[0] = [p[1]]
        elif (len(p) == 4):
            p[0] = [p[1]] + p[3]

    #  TODO Double return valies
    def p_command(self, p):
        '''command : valuelist TKAssignment TKName parameterlist
                   | valuelist TKAssignment TKExec parameterlist
                   | valuelist TKAssignment TKSplit parameterlist
                   | valuelist TKAssignment TKString
        '''
        if len(p) == 5:
            p[0] = ('commandexecsplitproc', p[1], p[2], p[3], p[4])
        elif len(p) == 4:
            p[0] = ('commandassignment', p[1], p[2], p[3])

    def p_guard(self, p):
        '''guard : TKName TKEquals TKString
                 | TKName TKNotEquals TKString
                 | TKFinally
        '''
        if len(p) == 4:
            p[0] = ('compairguard', p[1], p[2], p[3])
        elif len(p) == 2:
            p[0] = ('finallyguard', p[1])

    def p_valuelist(self, p):
        ''' valuelist : valuelist TKName
                      | TKName
        '''
        if (len(p) == 2):
            p[0] = [p[1]]
        elif (len(p) == 3):
            p[0] = p[1]+[p[2]]

    def p_parameterlist(self, p):
        ''' parameterlist : parameterlist TKString
                          | TKString
        '''
        if (len(p) == 2):
            p[0] = [p[1]]
        if (len(p) == 3):
            p[0] = p[1]+[p[2]]

    def p_error(self, p):
        print "Syntax error at '%s'" % p.value

    def parse(self, data):
        if data:
            return self.parser.parse(data, self.lexer.lexer, 0, 0, None)
        else:
            return []

