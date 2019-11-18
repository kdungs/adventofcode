import sys

def execute(code, registers=None):
    if registers is None:
        registers = {'a': 0, 'b': 0}
    program = dict(enumerate(code.split('\n')[:-1]))
    l = 0
    while l < len(program):
        line = program[l]
        l += 1
        instruction = line[:3]
        register = line[4]
        if instruction == 'hlf':
            registers[register] /= 2
        elif instruction == 'tpl':
            registers[register] *= 3
        elif instruction == 'inc':
            registers[register] += 1
        elif instruction == 'jmp':
            l += int(line[4:]) - 1
        elif instruction == 'jie':
            if registers[register] % 2 == 0:
                l += int(line[7:]) - 1
        elif instruction == 'jio':
            if registers[register] == 1:
                l += int(line[7:]) - 1
        else:
            assert(False)
    return registers

with open(sys.argv[1]) as f:
    code = f.read()

print(execute(code))
print(execute(code, registers={'a': 1, 'b': 0}))
