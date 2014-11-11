__author__ = 'Johannes'

import threadedclasses

# runs procedures that have been called in a thread

class ThreadedCodeExecutor:
    def __init__(self):
        self.procedures = None
        self.procedureCalls = None
        self.contexts = []
        self.globalContext = {}
        self.threadedGuardedCommands = []
        self.threadedProcedures = []

    def setProcedures(self, procedures):
        self.procedures = procedures

    def getProcedures(self):
        return self.procedures

    def setProcedureCalls(self, procedureCalls):
        self.procedureCalls = procedureCalls

    def start(self):
        print "\n######################## starting ThreadedCodeExec ########################\n"
        for procedureCall in self.procedureCalls:
            procedureCall.setCallingContext(self.globalContext)
            self.startNewProcedure(procedureCall)

    def finish(self):
        print "\n######################## ended ThreadedCodeExec ########################\n"
        for threadedProc in self.threadedProcedures:
            threadedProc.join()
        print self.globalContext

    def startNewProcedure(self, procedureCall):
        if procedureCall.getProcedureName() in self.procedures:
            #print dict(zip(self.procedures[procedureCall.getProcedureName()].getInputArgs(), procedureCall.getParameters()))

            threadedProc = threadedclasses.ThreadedProcedure(
                self.procedures[procedureCall.getProcedureName()],
                dict(zip(self.procedures[procedureCall.getProcedureName()].getInputArgs(), procedureCall.getParameters())),
                procedureCall.getCallingContext(),
                procedureCall.getOutputVars(),
                self)
            self.threadedProcedures.append(threadedProc)
            threadedProc.start()
        else:
            raise Exception('Procedure %s does not exist' % procedureCall.getProcedureName())

