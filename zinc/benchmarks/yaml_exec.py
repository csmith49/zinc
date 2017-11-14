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
    prod = [[bm["command"]]]
    for cmd in bm["arguments"]:
        # check to see if its a param
        if "parameter" in cmd.keys():
            prod.append([cmd["parameter"]])
        # check to see if it's a flag
        if "flag" in cmd.keys():
            flag = cmd["flag"]
            args = []
            # and if there are values to be passed to the flag
            if "values" in cmd.keys():
                values = cmd["values"]
                args = ["-{} {}".format(flag, v) for v in values]
            else: args = ["-{}".format(flag)]
            # now put it all together, and don't forget to hyphenate the flag
            prod.append(args)
    return itertools.product(*prod)

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
    cmds = extract_commands(benchmark)
    timeout = int(benchmark["timeout"])

    # now run them all
    for cmd in cmds:
        print(" ".join(cmd))
        dur = time_it(cmd, timeout)
        print("{}".format(dur))