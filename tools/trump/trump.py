from z3 import *
from argparse import ArgumentParser
from pta import Substitution, Term
from string import whitespace

# sexp manipulation tools
def filter_comments(string):
    output = ""
    for line in string.split("\n"):
        output += line.split(";")[0] + "\n"
    return output

def parse_sexp(string):
    output = [[]]
    buffer = ''
    in_string = False

    for char in string:
        # start sexp
        if char == "(" and not in_string:
            output.append([])
        # close an sexp, put into one context up
        elif char == ")" and not in_string:
            if buffer:
                output[-1].append(buffer)
                buffer = ''
            temp = output.pop()
            output[-1].append(temp)
        # handle tokens within an sexp
        elif char in whitespace and not in_string:
            if buffer:
                output[-1].append(buffer)
                buffer = ''
        # open or close strings, as appropriate
        elif char == "\"": in_string = not in_string
        # otherwise, just update the buffer
        else: buffer += char

    return output[0][0]

def clean_string(string):
    return "(" + filter_comments(string) + ")"

# only for basic boolean operations and LIA
SYMBOLS = {
    "="         : lambda a, b: a == b,
    'ite'		: If,
	'and'		: And,
	'or'		: Or,
	'not'		: Not,
	'xor'		: Xor,
    '=>'		: Implies,
    '+'			: lambda a, b: a + b,
	'-'			: lambda a, b: a - b,
	'*'			: lambda a, b: a * b,
    '<='		: lambda a, b: a <= b,
	'>='		: lambda a, b: a >= b,
	'>'			: lambda a, b: a > b,
	'<'			: lambda a, b: a < b,
    'abs_bound' : lambda a, b, c: Or(a - b <= c, -c <= a - b),
    'x'         : Int('x'),
    'y'         : Int('y')
}

# convert string to a constant value
def interpret_constant(value):
    try:
        return int(value)
    except:
        pass
    # maybe it's boolean
    if value == "true":
        return True
    elif value == "false":
        return False
    # otherwise we don't know
    else:
        raise Exception("{} not a valid constant".format(value))

# we need to convert terms to z3 objects
def evaluate(term):
    def valuation(t):
        # if it's not a term, push it through
        if not isinstance(t, Term):
            return lambda: t
        # otherwise, handle the leaf case
        if t.is_leaf():
            # find it in the symbol table
            if t.value in SYMBOLS.keys():
                evaluated = SYMBOLS[t.value]
            # or conver it to a constant/strip of Term constructor
            else:
                try:
                    evaluated = interpret_constant(t.value)
                except Exception as e:
                    evaluated = t.value # catch-all, hope nothing breaks
            return lambda: evaluated
        # now evaluating functions
        else:
            return SYMBOLS[t.value]
    return term.cata(valuation)

# and now we can worry about interpreting added functions
def interpret_function(inputs, out_sort, sexp):
    input_vars, input_sorts = zip(*inputs)
    term = Term(sexp)
    def f(*args):
        sub = Substitution(zip(input_vars, args))
        return evaluate(term @ sub)
    return f

# now we need to actually do the sensitivity check
def check_sensitivity(name, sensitivity=1):
    formula = ("=>",
        ("abs_bound",
            "x",
            "y",
            1
        ),
        ("abs_bound",
            (name, "x"),
            (name, "y"),
            sensitivity
        )
    )
    term = Term(formula)
    return ForAll([SYMBOLS["x"], SYMBOLS["y"]], evaluate(term))

if __name__ == "__main__":
    parser = ArgumentParser("Trump")
    parser.add_argument("file")
    filename = parser.parse_args().file
    # load the data
    with open(filename, "r") as f:
        cmds = parse_sexp(clean_string(f.read()))
    # and do the work
    for command in cmds:
        inst, *args = command
        if inst == "define-fun":
            name, inputs, out_sort, sexp = args
            SYMBOLS[name] = interpret_function(inputs, out_sort, sexp)
        if inst == "check-sensitivity":
            name = args[0]
            # first, see if there's any sensitivity that works
            c = Int("c")
            f = check_sensitivity(name, c)
            s = Solver()
            s.add(Exists([c], f))
            is_sensitive = (s.check() == sat)

            if is_sensitive:
                c = 0
                while True:
                    c += 1
                    f = check_sensitivity(name, c)
                    s = Solver()
                    s.add(f)
                    if s.check() == sat:
                        print("SENSITIVITY: {}".format(c))
                        exit(0)

            else:
                print("NOT SENSITIVE")
                exit(0)
