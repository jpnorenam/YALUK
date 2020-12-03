import os, sys, glob
import signal
import argparse
import subprocess
import numpy as np
import pandas as pd

def parse_args():
    """Parse command line arguments"""

    parser = argparse.ArgumentParser(
             description="Yaluk subprocess to run several cases")
    parser._action_groups.pop()
    required = parser.add_argument_group('required arguments')
    required.add_argument('-d',
                          dest="workdir",
                          help="path to directory of the .lis results files",
                          required=True)
    args = parser.parse_args()
    return args

def read_lis(file, case_name):
    # Get nodes order
    bash_cmd = """cat {} | sed -n -e '/Step/,/Variable maxima/p' | sed -e '$ d' | \
                perl -nE 'say /N.*/g' | tr -s '\n' | tr -s '\t' | tr -s ' ' | tr '\n' ' '""".format(file)
    process = subprocess.Popen(bash_cmd, stdout=subprocess.PIPE, shell=True)
    output, error = process.communicate()
    nodes = np.array(output.decode("utf-8").split(), dtype=str)

    # Get variable maxima
    bash_cmd = """cat {} | sed -n -e '/Variable maxima/,/Times of maxima/p' | sed -e '$ d' | \
                sed -e 's/[^0-9.E \-]*//g' -e  's/ \+/ /g' | tr -s ' ' | tr '\n' ' '""".format(file)
    process = subprocess.Popen(bash_cmd, stdout=subprocess.PIPE, shell=True)
    output, error = process.communicate()
    max_var = np.array(output.decode("utf-8").split(), dtype=float)

    # Get variable minima
    bash_cmd = """cat {} | sed -n -e '/Variable minima/,/Times of minima/p' | sed -e '$ d' | \
                sed -e 's/[^0-9.E \-]*//g' -e  's/ \+/ /g' | tr -s ' ' | tr '\n' ' '""".format(file)
    process = subprocess.Popen(bash_cmd, stdout=subprocess.PIPE, shell=True)
    output, error = process.communicate()
    max_var = np.vstack([max_var, np.array(output.decode("utf-8").split(), dtype=float)])

    # Compare the abs and select max
    max_var = np.amax(abs(max_var), axis=0)

    nodes_filled = np.append(nodes, ['fill{}'.format(x) for x in range(len(max_var)-len(nodes))])# Solve this

    return pd.DataFrame([max_var.tolist()], columns=nodes_filled.tolist(), index=[case_name])

def progress_bar(iteration, total, prefix = '', suffix = '', decimals = 1, length = 100, fill = '█', printEnd = "\r"):
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print('\r%s |%s| %s%% %s' % (prefix, bar, percent, suffix), end = printEnd)
    if iteration == total: 
        print()

if __name__ == '__main__':
    args = parse_args()

    if not os.path.isdir(args.workdir):
        print("The directory doesn't exists, goodbye!")
        sys.exit(0)

    res_file = '{}/results.csv'.format(args.workdir)

    append_df = False
    if os.path.isfile(res_file):
        df = pd.read_csv(res_file)
        df = df.set_index(df.columns[0])
        append_df = True

    def signal_handler(sig, frame):
        df.to_csv(res_file)
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)

    lis_files = glob.glob('{}/*.lis'.format(args.workdir))

    init = True
    lis_count = 0
    for file in lis_files:
        case_name = file.split('/')[-1][:-4]

        if not append_df and init:
            df = read_lis(file, case_name)
            init = False
        
        elif case_name not in df:
            try:
                df = df.append(read_lis(file, case_name))
            except:
                print("[yaluk results] ERROR: skipping {}.lis, processing format failed.".format(case_name))

        lis_count += 1
        progress_bar(lis_count, len(lis_files), prefix = '[yaluk results {}] progress:'.format(case_name), suffix = 'completed', length = 50)

        if not lis_count % 25:
            df.to_csv(res_file)

    df.to_csv(res_file)
