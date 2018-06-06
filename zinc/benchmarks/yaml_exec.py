from argparse import ArgumentParser
from ruamel.yaml import YAML
import subprocess
import time
import itertools
    
def split_output(lines, prefixes):
    output = []
    for _, prefix in prefixes:
        line = list(filter(lambda l: l.startswith(prefix), lines))[0]
        output.append(line.split(prefix)[-1])
    return output

# this function forms the core of the operation
def time_it(cmd, timeout, prefixes):
    try:
        start_time = time.time()
        # output_lines = subprocess.run(" ".join(cmd),
        output_lines = subprocess.run(cmd,
            # shell=True, 
            timeout=timeout, 
            stdout=subprocess.PIPE,
            universal_newlines=True).stdout.split("\n")
        t = time.time() - start_time
        return t, split_output(output_lines, prefixes)
    except subprocess.TimeoutExpired as e:
        return timeout, []

# given a benchmark, we need to be able to extract all the commands to run and time
def extract_commands(bm):
    # initialize the command - main proc here is to build a list of lists and take the product
    prod = [[bm["command"]]]
    # but we'll also maintain a list of indices/keys for when we make a choice
    choices = []
    # so loop, keeping track of the index a thing gets put into prod
    try:
        arguments = bm["arguments"]
    except:
        arguments = []
    for cmd in arguments:
        # check to see if its a param
        if "parameter" in cmd.keys():
            param = cmd["parameter"]
            prod.append([str(param)])
        # check to see if it's a flag
        if "flag" in cmd.keys():
            flag = cmd["flag"]
            prod.append(["-{}".format(flag)])
            # and if there are values to be passed to the flag
            if "values" in cmd.keys():
                # grab the values
                values = cmd["values"]
                prod.append(str(v) for v in values)
                # check and see if we actually have some choice here
                if len(values) > 1:
                    choices.append( (flag, len(prod) - 1) )
            # tack it on there
    if "filters" in bm.keys():
        prefixes = [(cmd["id"], cmd["prefix"]) for cmd in bm["filters"]]
    else: prefixes = []
    return itertools.product(*prod), choices, prefixes

if __name__ == "__main__":
    # construct the yaml parser
    yaml = YAML()

    # load the cmd line arguments
    parser = ArgumentParser(description="Executes benchmarks in YAML form.")
    parser.add_argument("input")

    args = parser.parse_args()

    # load the input file and parse as a yaml doc
    benchmark = None
    with open(args.input, "r") as f:
        benchmark = yaml.load(f.read())

    # get the commands and timeout
    cmds, choices, prefixes = extract_commands(benchmark)
    timeout = int(benchmark["timeout"])

    # print out our keys
    print("\t".join([p[0] for p in choices] + ["time"] + [p[0] for p in prefixes]))

    # now run them all
    for cmd in cmds:
        dur, output = time_it(cmd, timeout, prefixes)
        print("\t".join([cmd[p[1]] for p in choices] + ["{}".format(dur)] + output))
