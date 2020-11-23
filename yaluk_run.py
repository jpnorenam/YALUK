import multiprocessing
import argparse, sys
import os, shutil
import subprocess
import threading
import pandas
import time

def parse_args():
    """Parse command line arguments"""

    parser = argparse.ArgumentParser(
             description="Yaluk subprocess to run several cases")
    parser._action_groups.pop()
    required = parser.add_argument_group('required arguments')
    required.add_argument('-n',
                          dest="case_prefix",
                          help="case name prefix of the .atp files",
                          required=True)
    required.add_argument('-w',
                          dest="workdir",
                          help="location of the .atp & .ini files and the CaseFile folder",
                          required=True)
    required.add_argument('-j',
                          dest="n_threads",
                          help="number of threads",
                          required=True, type=int,
                          choices=range(1, multiprocessing.cpu_count()))
    required.add_argument('-b',
                          dest="build",
                          help="build source before running the subprocess [y/n]",
                          required=True)
    required.add_argument('-a',
                          dest="libatp",
                          help="libatp source directory",
                          required=True)
    required.add_argument('-y',
                          dest="libyaluk",
                          help="libyaluk source directory",
                          required=True)
    args = parser.parse_args()

    args.build = True if args.build == 'y'else False

    return args

def build_libyaluk(src):
    os.chdir(src)
    bash_cmd = "make"
    process = subprocess.Popen(bash_cmd.split(), stdout=subprocess.PIPE)
    output, error = process.communicate()

def build_libatp(src, pwd):
    os.chdir(src)
    bash_cmd = """chmod 755 vardim vardimn; dos2unix vardimn; 
               ./vardimn listsize.ylk; make; cp startup {}""".format(pwd)
    process = subprocess.Popen(bash_cmd, stdout=subprocess.PIPE, shell=True)
    output, error = process.communicate()
    
class ThreadPool(object):
    def __init__(self):
        super(ThreadPool, self).__init__()
        self.active = []
        self.lock = threading.Lock()
    def makeActive(self, name):
        with self.lock:
            self.active.append(name)
    def makeInactive(self, name):
        with self.lock:
            self.active.remove(name)
    def numActive(self):
        with self.lock:
            return len(self.active)

def t_fun(pool, semaphore, curr_params, tpbig, case):
    name = threading.currentThread().getName()
    pool.makeActive(name)
    print(os.get_cwd(), "./{}/corr_00001.txt".format(name))
    with open("./{}/corr_00001.txt".format(name), "r+") as f:
        f_string = ""
        for val in curr_params:
            f_string += "{}\n".format(val)
        f.write(f_string)
    for n in range(3):
        bash_cmd = "{} BOTH {}.{}.atp s -r".format(tpbig, case, n)
        process = subprocess.Popen(bash_cmd.split(), stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        #process = subprocess.run(bash_cmd.split(), stdout=subprocess.PIPE, capture_output=True, text=True, input=name)
        time.sleep(5)
        output, error = process.communicate(input=bytes(name, 'utf-8')) 
        with semaphore:
            # Escribo resultados
            # debug copy .pl4
            time.sleep(0.1)
    # Erase .dat
    pool.makeInactive(name)


if __name__ == '__main__':
    args = parse_args()
    
    if (args.build):
        build_libyaluk(args.libyaluk)
        build_libatp(args.libatp, args.workdir)

    os.chdir(args.workdir)

    tpbig = "{}/tpbig".format(args.libatp)

    # Create a directory with the case files for each thread
    for n in range(args.n_threads):
        if not os.path.isdir("./CaseFiles{}".format(n)):
            shutil.copytree("./CaseFiles", "./CaseFiles{}".format(n))

    # Read current params dataframe
    curr_df = pandas.read_csv("{}.csv".format(args.case_prefix), sep=';')

    i = 0 # Stroke current params dataframe entry
    pool = ThreadPool()
    semaphore = threading.Semaphore(3) # Thread safety when writing results
    while(i < len(curr_df.index)):
        curr_params = curr_df.iloc[i].values
        print(i, curr_params)
        print(pool.active)
        if pool.numActive() < args.n_threads:
            for j in range(args.n_threads):
                t_name = "CaseFiles{}".format(j)
                if not t_name in pool.active:
                    break
            t = threading.Thread(target=t_fun, name=t_name,
                                 args=(pool, semaphore, curr_params, tpbig, args.case_prefix))
            t.start()
            i+=1
        else:
            time.sleep(0.5)

    # Clean up
    for n in range(args.n_threads):
        if os.path.isdir("./CaseFiles{}".format(n)):
            shutil.rmtree("./CaseFiles{}".format(n))