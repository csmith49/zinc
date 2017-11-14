from argparse import ArgumentParser
from ruamel.yaml import YAML
import subprocess
import time
import itertools
    
# this function forms the core of the operation
def time_it(cmd, timeout):
    try:
        start_time = time.time()
        subprocess.run(" ".join(cmd), shell=True, timeout=timeout, stdout=subprocess.DEVNULL)
        return time.time() - start_time
    except subprocess.TimeoutExpired as e:
        return timeout

# given a benchmark, we need to be able to extract all the commands to run and time
def extract_commands(bm):
    # initialize the command - main proc here is to build a list of lists and take the product
    prod = [[bm["command"]]]
    # but we'll also maintain a list of indices/keys for when we make a choice
    choices = []
    # so loop, keeping track of the index a thing gets put into prod
    for cmd in bm["arguments"]:
        # check to see if its a param
        if "parameter" in cmd.keys():
            prod.append([cmd["parameter"]])
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
    return itertools.product(*prod), choices

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
    cmds, choices = extract_commands(benchmark)
    timeout = int(benchmark["timeout"])

    # print out our keys
    print("\t".join([p[0] for p in choices] + ["time"]))

    # now run them all
    for cmd in cmds:
        dur = time_it(cmd, timeout)
        print("\t".join([cmd[p[1]] for p in choices] + ["{}".format(dur)]))