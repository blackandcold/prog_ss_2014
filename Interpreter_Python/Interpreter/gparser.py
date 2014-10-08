class GuardParser:
    def __init__(self, lexertokenlist):
        self.tokenlist = lexertokenlist
        self.kv_tokens = []
        self.procedurelist = []

        # convert lexer result to something more useful
        for token in self.tokenlist:
            self.kv_tokens.append([token.type, token.value, token.lineno, token.lexpos])

    # parse array to get eleemts of the program and check validity
    def parseLexerArray(self):
        print self.kv_tokens
        # TODO parsing magic

        return self.procedurelist
