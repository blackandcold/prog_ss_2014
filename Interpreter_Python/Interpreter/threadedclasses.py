import re
import subprocess

__author__ = 'Johannes'

import time
from itertools import izip
from threading import Thread
import structureclasses


class ThreadedProcedure(Thread):
    def __init__(self, procedure, context, callingContext, outputVars, threadedCodeExecutor):
        Thread.__init__(self)
        self.threadedCodeExecutor = threadedCodeExecutor
        self.context = context
        self.callingContext = callingContext
        self.procedure = procedure
        self.finished = False
        self.outputVars = outputVars
        self.threadedGuardedCommands = []
        for guardedCommand in self.procedure.getGuardedCommands():
            #print guardedCommand
            #print self.context
            #print self.outputVars
            self.threadedGuardedCommands.append(ThreadedGuardedCommand(self, guardedCommand))

        #print self.context
        #print self.callingContext
        #print self.outputVars

    def run(self):
        #  call commands
        for threadedGuardedCommand in self.threadedGuardedCommands:
            threadedGuardedCommand.start()

        #  wait for finish
        while not self.finished:
            # check for output vars rewritten to our context
            varsSatisfied = True
            for outputVar in self.procedure.getOutputVars():
                #print outputVar
                if not outputVar in self.context:
                    varsSatisfied = False

            if varsSatisfied:
                for resultvar, result in izip(self.procedure.getOutputVars(), self.context):
                    self.callingContext[self.outputVars[0]] = self.context[resultvar]

                # for contextVars in self.context:
                #     print "\n contextVar: " + contextVars
                self.finished = True

            time.sleep(0.01)

        for threadedGuardedCommand in self.threadedGuardedCommands:
            threadedGuardedCommand.join()

    def getThreadedCodeExecutor(self):
        return self.threadedCodeExecutor

    def getThreadedGuardedCommands(self):
        return self.threadedGuardedCommands

    def getContext(self):
        return self.context

    def setContext(self, context):
        self.context = context

    def setCallingContext(self, callingContext):
        self.callingContext = callingContext

    def isFinished(self):
        return self.finished


class ThreadedGuardedCommand(Thread):
    def __init__(self, threadedProcedure, guardedCommand):
        Thread.__init__(self)
        self.threadedProcedure = threadedProcedure  # Parent Procedure
        self.guardedCommand = guardedCommand  # Command to invoke
        self.context = threadedProcedure.getContext()  # context to write results to
        self.finished = False  # indicator if finished

    def run(self):
        while not self.finished:
            if self.isCommandExecutable():
                self.runCommand()
                self.finished = True
            if self.threadedProcedure.isFinished():
                self.finished = True

    def isFinished(self):
        return self.finished

    def setFinished(self, finished):
        self.finished = finished

    def getGuardedCommand(self):
        return self.guardedCommand

    def areGuardsSatisfied(self):
        # check if guards are satisfied, bound or finally
        guardsSatisfied = True

        for guard in self.guardedCommand.getGuards():
            if guard.getOperation() == structureclasses.SingleGuard.BOUND:
                guardsSatisfied = False
            elif guard.getOperation == structureclasses.SingleGuard.EQUAL:
                if self.threadedProcedure.getContext()[guard.getVariable()] != guard.getValue():
                    guardsSatisfied = False
                    self.finished = True
                elif guard.getVariable not in self.threadedProcedure.getContext():
                    guardsSatisfied = False
            elif guard.getOperation == structureclasses.SingleGuard.NOT_EQUAL:
                if self.threadedProcedure.getContext()[guard.getVariable()] == guard.getValue():
                    guardsSatisfied = False
                    self.finished = True
                elif guard.getVariable not in self.threadedProcedure.getContext():
                    guardsSatisfied = False
            elif guard.getOperation == structureclasses.SingleGuard.FINALLY:
                # go through all commands and check guards
                for threadedGuardedCommand in self.threadedProcedure.getThreadedGuardedCommands():
                    # check all commands if they are not yet finshed
                    if not threadedGuardedCommand.isFinished():
                        # check if the not yet finished still have guards
                        if len(threadedGuardedCommand.getGuardedCommand().getGuards()) > 0:
                            # check if the not yet finished command has a non finally guard
                            if threadedGuardedCommand.getGuardedCommand().getGuards()[0].getOperation() != structureclasses.SingleGuard.FINALLY:
                                guardsSatisfied = False
            return guardsSatisfied

    def isCommandExecutable(self):
        # all vars in scope present
        for var in self.guardedCommand.getCommand().getArgs():
            if var not in self.context:
                return False

        # when all guards are satisfied
        if not self.areGuardsSatisfied():
            return False

        # all out vars unbound
        for var in self.guardedCommand.getCommand().getVars():
            if var in self.context:
                self.finished = True
                # case 1 var is bound => we finished
                    # for
                    # case 2 check other commands for occurence of var, set if not finished yes
                self.findAndSetUnboundVars()
                return False
        return True

    def findAndSetUnboundVars(self):
        # go through all commands
        for var in self.guardedCommand.getCommand().getVars():
            # assume the var is not satisfiable
            isVarSatisfiable = False
            # go through all commands that are not finished and check if they have our bound var
            for threadedGuardedCommand in self.threadedProcedure.getThreadedGuardedCommands():
                if not threadedGuardedCommand.isFinished() and var in threadedGuardedCommand.getGuardedCommand().getCommand().getVars():
                    # there is a assignment for the var
                    isVarSatisfiable = True
            if not isVarSatisfiable:
                # go through all commands in this procedure
                for guardedCommand2 in self.threadedProcedure.getThreadedGuardedCommands():
                    # check if not finished yet
                    if not guardedCommand2.isFinished():
                        # get all guards
                        for commandGuard in guardedCommand2.getGuardedCommand().getGuards():
                            # if guard has the same var, set finished
                            if commandGuard.getVar() == var:
                                guardedCommand2.setFinished(True)

    def runCommand(self):
        # check type of command and execute
        cmdType = type(self.guardedCommand.getCommand())
        if cmdType == structureclasses.AssignmentCommand:
            self.execAssignment()
        elif cmdType == structureclasses.ExecCommand:
            self.execExec()
        elif cmdType == structureclasses.ProcCommand:
            self.execProc()
        elif cmdType == structureclasses.SplitCommand:
            self.execSplit()

    def execProc(self):
        name = self.guardedCommand.getCommand().getName()
        vars = self.guardedCommand.getCommand().getVars()
        args = self.guardedCommand.getCommand().getArgs()

        argsReplaced = []
        for var in args:
            if '$' in var:
                varName = var.strip('$')
                argsReplaced.append(re.sub(r'\$'+varName+'\$', self.context[varName], var))
            else:
                argsReplaced.append(var)

        # fetch procedure
        procedure = self.threadedProcedure.getThreadedCodeExecutor().getProcedures()[name]
        newProcedure = ThreadedProcedure(
            procedure,
            dict(zip(procedure.getInputArgs(),argsReplaced)),
            self.context,
            vars,
            self.threadedProcedure.getThreadedCodeExecutor())
        newProcedure.start()
        newProcedure.join()

    def execSplit(self):
        vars = self.gc.getCommand().getVars()
        args = self.gc.getCommand().getArgs()
        argsReplaced = []
        for var in args:
            if '$' in var:
                varName = var.strip('$')
                argsReplaced.append(re.sub(r'\$'+varName+'\$', self.context[varName], var))
            else:
                argsReplaced.append(var)

        # print argsReplaced
        # now split
        outvars = argsReplaced[1].split(argsReplaced[2], 1)
        self.context[vars[0]] = outvars[0]
        self.context[vars[1]] = outvars[1]

    def execExec(self):
        vars = self.guardedCommand.getVars()
        args = self.guardedCommand.getArgs()
        input = args[0]
        # replace vars in string to execute
        for var in args[1:]:
            input = re.sub(r'\$'+var+'\$', self.context[var], input)

        #print 'Console Command: ' + input

        try:
            output = subprocess.check_output(input, shell=True)
            if len(vars) == 2:
                self.context[vars[1]] = output
            else:
                self.context[vars[0]] = output
        except subprocess.CalledProcessError, e:
            output = e.output
            returnCode = str(e.returncode)
            if len(vars) == 2:
                self.context[vars[0]] = returnCode
                self.context[vars[1]] = output
            else:
                self.context[vars[0]] = output

    def execAssignment(self):
        var = self.guardedCommand.getCommand().getVars()[0]
        arg = self.guardedCommand.getCommand().getArgs()[0]
        self.context[var] = self.context[arg]

