__author__ = 'Johannes'

import structureclasses
import re
from ThreadedCodeExecutor import ThreadedCodeExecutor
from pprint import pprint


class ASTInterpreter:
    def __init__(self):
        self.procedures = {}
        self.procedureCalls = []
        self.threadedCodeExecutor = ThreadedCodeExecutor()

    def interpreteAST(self, ast):
        self.buildClassStructure(ast)

        self.threadedCodeExecutor.setProcedures(self.procedures)
        self.threadedCodeExecutor.setProcedureCalls(self.procedureCalls)

        self.threadedCodeExecutor.start()
        self.threadedCodeExecutor.finish()

    def buildClassStructure(self, ast):
        for toplevel in ast:
            #print toplevel
            if toplevel[0] == 'procscript':
                outVars = []
                resultnames = []
                for outvar in toplevel[1][3]:
                    resultnames.append(outvar.strip('"'))
                    outVars.append(re.findall('(?<=\$)[A-Za-z]\w*(?=\$)', outvar.strip('"'))[0])
                #print outVars
                #print resultnames

                self.procedures[toplevel[1][0]] = structureclasses.Procedure(toplevel[1][0], toplevel[1][1], resultnames, outVars)
                print self.procedures[toplevel[1][0]]

                procGuards = []

                command = None
                guards = []
                # TODO procedure body
                for procline in toplevel[1][5]:
                    #print procline
                    if procline[0] == 'commandassignment':  # assignment
                            args = self.clearArgs([procline[3]])
                            vars = procline[1]
                            if len(vars) != 1:
                                raise Exception("Return parameter count not matching assignment command!")
                            if len(args) < 1:
                                raise Exception("Input parameter count not matching assignment command!")
                            command = structureclasses.AssignmentCommand(vars, args)

                    if procline[0] == 'commandexecsplitproc':
                        #print procline
                        if procline[3] == 'exec':
                            args = [procline[4][0]] + re.findall('(?<=\$)[A-Za-z]\w*(?=\$)', procline[4][0].strip('"'))
                            vars = procline[1]
                            if len(vars) != 1:
                                raise Exception("Return parameter count not matching exec command!")
                            if len(args) > 3 or len(args) < 2:
                                raise Exception("Input parameter count not matching exec command!")
                            command = structureclasses.ExecCommand(vars, args)

                        elif procline[3] == 'split':
                            args = self.clearArgs(procline[4])
                            vars = procline[1]
                            if len(vars) != 2:
                                raise Exception("Return parameter count not matching split command!")
                            if len(args) != 2:
                                raise Exception("Input parameter count not matching split command!")
                            command = structureclasses.SplitCommand(vars, args)

                        else:  # procedure
                            args = self.clearArgs(procline[4])
                            vars = procline[1]
                            if len(vars) != 1:
                                raise Exception("Return parameter count not matching proc command!")
                            if len(args) > 2 or len(args) < 1:
                                raise Exception("Input parameter count not matching proc command!")
                            command = structureclasses.ProcCommand(procline[3], vars, args)

                    elif procline[0] == 'compairguard':
                        if procline[2] == '== ':
                            guards.append(structureclasses.SingleGuard(structureclasses.SingleGuard.EQUAL, procline[1], procline[3].strip('"')))
                        if procline[2] == '!= ':
                            guards.append(structureclasses.SingleGuard(structureclasses.SingleGuard.NOT_EQUAL, procline[1], procline[3].strip('"')))

                    elif procline[0] == 'finallyguard':
                        guards.append(structureclasses.SingleGuard(structureclasses.SingleGuard.FINALLY))

                    if command != None:
                        procGuards.append(structureclasses.GuardedCommand(guards, command))
                        guards = []
                        command = None

                self.procedures[toplevel[1][0]].setGuardedCommands(procGuards)
                pprint(self.procedures[toplevel[1][0]].getGuardedCommands())

            if toplevel[0] == 'proccall':
                #print toplevel[1]
                #print toplevel[1][2]
                #print toplevel[1][0]
                args = self.clearArgs(toplevel[1][3])
                procedureCall = structureclasses.ProcedureCall(toplevel[1][2], args, toplevel[1][0])
                self.procedureCalls.append(procedureCall)



        pprint(self.procedures)

    def clearArgs(self, argArray):
        out = []
        for arg in argArray:
            arg = arg.strip('"')
            arg = arg.strip('$')
            out.append(arg)
        return out