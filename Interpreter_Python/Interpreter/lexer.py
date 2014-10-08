import PLY.lex as lex

class GuardedLexer:
    def __init__(self, inputdata):
        self.lexer = None
        self.data = inputdata
        # List of token names.   This is always required

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
        'TKStringTerminal',
        'TKName',
        'TKString',
        'TKVariable',
        'TKDollar'] + list(reserved.values())

    # Regular expression rules for simple tokens
    t_TKSemicolon = r'\;'
    t_TKColon = r'\:'
    t_TKOpenBracket = r'\{'
    t_TKCloseBracket = r'\}'
    t_TKDollar = r'\$'
    t_TKStringTerminal = r'\"'

    def t_TKVariable(self, t):
        r'(?<=\$)[A-Za-z]\w*(?=\$)'
        return t

    def t_TKName(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = self.reserved.get(t.value, 'TKName')
        return t

    def t_TKEquals(self, t):
        r'\s?==\s?'
        return t

    def t_TKNotEquals(self, t):
        r'\s?!=\s?'
        return t

    def t_TKMinus(self, t):
        r'\s?-\s?'
        return t

    def t_TKString(self, t):
        r'(\")[A-Za-z0-9](\")'
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

    def lexit(self):
        tokenlist = []
        self.lexer.input(self.data)
        while True:
            tok = self.lexer.token()
            if not tok:
                break      # No more input
            tokenlist.append(tok)
            #print tok
        return tokenlist

