import re
import subprocess

__author__ = 'Johannes'


# output = ''
# try:
#     output = subprocess.check_output("IF 3 GEQ 2 (ECHO 1) ELSE (ECHO 0)", shell=True)
# except subprocess.CalledProcessError, e:
#     output = e.output
#     returnCode = str(e.returncode)
#     print 'ReturnCode: ' + returnCode + '\n'
#
# print 'Output: ' + output + '\n'
#
# output = ''
# try:
#     output = subprocess.check_output("IF 2 GEQ 3 (ECHO 1) ELSE (ECHO 0)", shell=True)
# except subprocess.CalledProcessError, e:
#     output = e.output
#     returnCode = str(e.returncode)
#     print 'ReturnCode: ' + returnCode + '\n'
#
# print 'Output: ' + output + '\n'

# context = {'a': '3', 'b': '4'}
# args = ['IF $b$ GEQ $a$ (ECHO 1) ELSE (ECHO 0)', 'a', 'b']
# input = args[0]
# for var in args[1:]:
#     print r'\$'+var+'\$'
#     input = re.sub(r'\$'+var+'\$', context[var], input)
#
# print input

#GUARDED COMMAND ##  Cmd COMMAND ##  CommandType SplitCommand Variables: head, tail Arguments: /, a/s/d/f ##
context = {'a': '/', 'c': '3', 'b': 'f/g/h'}
args = ['$a$', '$b$']
argsReplaced = []
for var in args:
    if '$' in var:
        varName = var.strip('$')
        argsReplaced.append(re.sub(r'\$'+varName+'\$', context[varName], var))
    else:
        argsReplaced.append(var)

print argsReplaced

outvars = argsReplaced[1].split(argsReplaced[0], 1)
print outvars