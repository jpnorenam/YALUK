import multiprocessing
import argparse, sys
import os, glob, shutil
import subprocess
import threading
import signal
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

    args.build = True if args.build == 'y' else False

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
    start = time.time()
    name = threading.currentThread().getName()
    pool.makeActive(name)
    with open("./{}/corr_00001.txt".format(name), "w") as f:
        f_string = ""
        curr_params = curr_params.split(';')
        for val in curr_params[1:]:
            f_string += "{}\n".format(val)
        f.write(f_string)
    print("[Thread {}] Running stroke No. {}.".format(name, curr_params[0]))
    for n in range(2):
        # Renaming file atp to get a .lis of the stroke
        try:
            shutil.copyfile("./{}.{}.atp".format(case, n),
                        "./{}.{}.s{}.atp".format(case, n,curr_params[0]))
        except:
            print("* Could not copy atp file *stroke:{} case:{}*".format(curr_params[0], n))

        bash_cmd = "{} DISK {}.{}.s{}.atp s -r".format(tpbig, case, n,curr_params[0])
        process = subprocess.Popen(bash_cmd.split(), stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        output, error = process.communicate(input=bytes(name, 'utf-8'))
        try:
            shutil.copyfile("./{}/{}.{}.s{}.pl4".format(name, case, n,curr_params[0]),
                        "./results/stroke{}.{}.pl4".format(curr_params[0], n))
            try:
                os.remove("./{}/{}.{}.s{}.pl4".format(name, case, n,curr_params[0]))
            except:
                print("Couldn't remove pl4 files *stroke:{} case:{}*".format(curr_params[0], n))
        except:
            print("* No pl4 file *stroke:{} case:{}*".format(curr_params[0], n))
        
        # Extraer datos del .LIS
        try:
            shutil.copyfile("./{}.{}.s{}.lis".format(case, n,curr_params[0]),
                        "./results/stroke{}.{}.lis".format(curr_params[0], n))
            try:
                
                os.remove("./{}.{}.s{}.lis".format(case, n,curr_params[0]))
                os.remove("./{}.{}.s{}.atp".format(case, n,curr_params[0]))
                os.remove("./{}.{}.s{}.dbg".format(case, n,curr_params[0]))
                os.remove("./{}.{}.s{}.pl4 ".format(case, n,curr_params[0]))
                print("Removing simulation files *Stroke:{}.Case{} *".format(curr_params[0], n))
            except:
                print("Couldn't remove simulation files *./\'{}.{}.s{}.pl4 \'*".format(curr_params[0], n))
        except:
            print("* No lis file *stroke:{} case:{}*".format(curr_params[0], n))
        
        
        with semaphore:
            # Escribir en un csv unificado (hacerlo thread safe!)
            time.sleep(0.1)
    end = time.time()
    try:
        print("[Thread {}] Completed stroke No. {} in {} s.".format(name, curr_params[0], end - start))
        os.remove("./{}/status_file.ylk".format(name))
        pool.makeInactive(name)
    except:
        print("Couldn't remove ./{}/status_file.ylk Case:{} \'*".format(name,n))

def signal_handler(sig, frame):
    print("Cleaning up the mess...")
    # Erase folders
    for n in range(args.n_threads):
        if os.path.isdir("./CaseFiles{}".format(n)):
            shutil.rmtree("./CaseFiles{}".format(n))
    print("Goodbye!")
    sys.exit(0)
    
if __name__ == '__main__':
    signal.signal(signal.SIGINT, signal_handler)
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
            
    # If results folder doesn't exist, then create it.   
    if not os.path.isdir("./results"): 
        os.makedirs("./results")

    # Read current params dataframe
    with open("{}.csv".format(args.case_prefix), 'r') as db:
        db.readline()
        eof = False # Stroke current params dataframe entry
        pool = ThreadPool()
        semaphore = threading.Semaphore(3) # Thread safety when writing results
        busy = 0
        while(not eof):
            if pool.numActive() < args.n_threads:
                busy = 0
                curr_params = db.readline()
                if not curr_params:
                    eof = True
                for j in range(args.n_threads):
                    t_name = "CaseFiles{}".format(j)
                    if not t_name in pool.active:
                        break
                t = threading.Thread(target=t_fun, name=t_name,
                                     args=(pool, semaphore, curr_params, tpbig, args.case_prefix))
                t.start()             
            else:
                if (busy == 0):
                    print("[All threads] busy", end = '')
                    busy += 1
                if (busy > 5):
                    print(".")
                    busy = 0
                else:
                    print(".", end = '')
                    busy += 1
                time.sleep(2)
         
        # Clean up
        for n in range(args.n_threads):
            if os.path.isdir("./CaseFiles{}".format(n)):
                shutil.rmtree("./CaseFiles{}".format(n))
