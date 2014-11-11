__author__ = 'Johannes'
from abc import ABCMeta, abstractmethod


class AbstractCommand:
    def __init__(self):
        pass

    __metaclass__ = ABCMeta

    @abstractmethod
    def execute(self, threadscope):
        pass

    @abstractmethod
    def getVars(self):
        pass

    @abstractmethod
    def getArgs(self):
        pass

    def __str__(self):
        toString = "COMMAND ## " \
                   + " CommandType " + type(self).__name__ \
                   + " Variables: " + ', '.join(self.getVars()) \
                   + " Arguments: " + ', '.join(self.getArgs()) \
                   + " ##"
        return toString


class ExecCommand(AbstractCommand):
    def execute(self, threadscope):
        self.threadscope = threadscope
        print "executing Exec Command"

    def getArgs(self):
        return self.args

    def getVars(self):
        return self.vars

    def __init__(self, vars, args):
        self.vars = vars
        self.args = args
        self.threadscope = None


class SplitCommand(AbstractCommand):
    def execute(self, threadscope):
        self.threadscope = threadscope
        print "executing Split Command"

    def getArgs(self):
        return self.args

    def getVars(self):
        return self.vars

    def __init__(self, vars, args):
        self.vars = vars
        self.args = args
        self.threadscope = None


class ProcCommand(AbstractCommand):
    def execute(self, threadscope):
        self.threadscope = threadscope
        print "executing Proc Command"

    def getArgs(self):
        return self.args

    def getVars(self):
        return self.vars

    def __init__(self, name, vars, args):
        self.vars = vars
        self.args = args
        self.name = name
        self.threadscope = None


class AssignmentCommand(AbstractCommand):
    def execute(self, threadscope):
        self.threadscope = threadscope
        print "executing Assignment Command"

    def getArgs(self):
        return self.args

    def getVars(self):
        return self.vars

    def __init__(self, vars, args):
        self.vars = vars
        self.args = args
        self.threadscope = None


class SingleGuard:
    BOUND = 0
    EQUAL = 1
    NOT_EQUAL = 2
    FINALLY = 3

    def __init__(self, operation, variable=None, value=None):
        self.opertion = operation
        self.variable = variable
        self.value = value

    def getOperation(self):
        return self.opertion

    def getVariable(self):
        return self.variable

    def getValue(self):
        return self.value

    def setOperation(self, operation):
        self.opertion = operation

    def setVar(self, variable):
        self.variable = variable

    def setValue(self, value):
        self.value = value

    def __str__(self):
        toString = " GUARD "
        return toString


class GuardedCommand:

    def __init__(self, guards, command):
        self.guards = guards
        self.command = command

    def getGuards(self):
        return self.guards

    def getCommand(self):
        return self.command

    def __str__(self):
        toString = "GUARDED COMMAND ## "
        for guard in self.guards:
            toString += " -- " + guard.__str__() + " -- "
        toString += " Cmd " + self.command.__str__()
        return toString


class Procedure:
    def __init__(self, name, inputArgs, results, outVars):
        self.name = name
        self.inputArgs = inputArgs
        self.outputVars = outVars
        self.results = results
        self.guardedCommands = []

    def getNumberOfParameters(self):
        return len(self.inputArgs)

    def getNumberOfResults(self):
        return len(self.a_results)

    def getGuardedCommands(self):
        return self.guardedCommands

    def getInputArgs(self):
        return self.inputArgs

    def getOutputVars(self):
        return self.outputVars

    def getResults(self):
        return self.results

    def getName(self):
        return self.name

    def setGuardedCommands(self, gc):
        self.guardedCommands = gc

    def setInputArgs(self, input):
        self.inputArgs = input

    def setOutputVars(self, outputVars):
        self.outputVars = outputVars

    def setResults(self, results):
        self.results = results

    def __str__(self):
        toString = "\nPROCEDURE ## " + self.name + ' ' + ','.join(self.getInputArgs()) + ' ' + ','.join(self.getOutputVars()) + ""
        return toString


class ProcedureCall:
    def __init__(self, procedureName, parameters, output=None):
        self.procedureName = procedureName
        self.parameters = parameters
        self.output = output
        self.callingContext = None

    def getProcedureName(self):
        return self.procedureName

    def getParameters(self):
        return self.parameters

    def getOutputVars(self):
        return self.output

    def getCallingContext(self):
        return self.callingContext

    def setProcedureName(self, procedureName):
        self.procedureName = procedureName

    def setParameters(self, parameters):
        self.parameters = parameters

    def setOutputVars(self, output):
        self.output = output

    def setCallingContext(self, callingContext):
        self.callingContext = callingContext

    def __str__(self):
        toString = 'ProcCall ' + self.getProcedureName() + ' ' + ', '.join(self.getParameters()) + ' Ret: ' + ', '.join(self.getOutputVars())

        return toString