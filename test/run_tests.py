"""
>>> python3 run_tests.py > junk.out
"""
import os
import sys
import subprocess
from typing import List

from mystran_f06_reader import read_f06, NotLinearStatics

IS_WINDOWS = sys.platform == 'win32'

def get_files_of_type(dirname: str, extension: str='.txt',
                      max_size: float=100., case_sensitive: bool=False) -> List[str]:
    """
    Gets the list of all the files with a given extension in the specified directory

    Parameters
    ----------
    dirname : str
        the directory name
    extension : str; default='.txt'
        list of filetypes to get
    max_size : float; default=100.0
        size in MB for max file size
    case_sensitive : bool; default=False
        should files be case sensitive

    Returns
    -------
    files : List[str]
        list of all the files with a given extension in the specified directory

    """
    filenames2 = []  # type: List[str]
    if not os.path.exists(dirname):
        return filenames2

    filenames = os.listdir(dirname)

    for filenamei in filenames:
        filename = os.path.join(dirname, filenamei)

        if os.path.isdir(filename):
            filenames2 += get_files_of_type(
                filename,
                extension=extension, max_size=max_size,
                case_sensitive=case_sensitive)
        elif (os.path.isfile(filename) and os.path.getsize(filename) / 1048576. <= max_size):
            ext = os.path.splitext(filenamei)[1]
            #print(f'***ext={ext!r}')
            if ext == extension or (not case_sensitive and ext.lower() == extension):
                #print('A')
                pass
            else:
                A = ext == extension
                B = ext.lower() == extension
                #print(f'skipping {filename}; A={A}; B={B}; case_sensitive={case_sensitive}; extension_expected={extension!r}')
                continue
            filenames2.append(filename)
    return filenames2


def call_mystran(mystran_exe: str, bdf_filename: str) -> int:
    assert os.path.exists(bdf_filename), bdf_filename
    call_args = [mystran_exe, str(bdf_filename)] # + keywords_list
    return_code = None
    #if run:
    base = os.path.splitext(bdf_filename)[0]
    log_filename = base + '.log'
    with open(log_filename, 'w') as log_file:
        return_code = subprocess.call(call_args, stdout=log_file, stderr=log_file)
    return return_code

def run_jobs(test_dirname: str):
    """
    >>> python3 run_tests.py > junk.out
    """
    mystran_exe = os.path.join('..', 'Binaries', 'mystran')
    if IS_WINDOWS:
        mystran_exe = 'mystran'
        mystran_exe += '.exe'
    assert os.path.exists(mystran_exe), mystran_exe
    print(f'{mystran_exe!r} exists')

    #files = get_files_of_type(dirname, extension='.op2', max_size=100.,
                              #case_sensitive=False)
    #print(files)

    bdf_filenames = get_files_of_type(
        test_dirname, extension='.dat', max_size=100.,
        case_sensitive=False)
    for bdf_filename in bdf_filenames:
        #if ('Archive' in bdf_filename
            #or 'Combined' in bdf_filename
            #):
            #continue

        root = os.path.splitext(bdf_filename)[0]
        f06_filename = root + '.f06'
        op2_filename = root + '.op2'

        #if 'Static' in bdf_filename:
            #continue
        #if 'Eigen' in bdf_filename:
            #continue

        #if 'BUCK' not in bdf_filename:
            #continue

        print(bdf_filename)
        return_code = call_mystran(mystran_exe, bdf_filename)
        print(return_code)

        if not os.path.exists(op2_filename):
            print(f'****missing {op2_filename}')

        #asdf

    if 0:
        files = get_files_of_type(test_dirname, extension='.ans', max_size=100.,
                                  case_sensitive=False)
        for f06_filename in files:
            #if 'Static' in f06_filename:
                #continue
            #if 'Eigen' in f06_filename:
                #continue
            #if 'Combined' in f06_filename:
                #continue

            #try:
            print(f06_filename)
            read_f06(f06_filename)
            #except NotLinearStatics:
                #print(f'*{f06_filename}')
    x = 1

def check_jobs(test_dirname):
    from pyNastran.op2.op2 import read_op2
    bdf_filenames = get_files_of_type(test_dirname, extension='.dat', max_size=100.,
                                      case_sensitive=False)
    bdf_filenames += get_files_of_type(test_dirname, extension='.bdf', max_size=100.,
                              case_sensitive=False)
    #files = get_files_of_type(test_dirname, extension='.op2', max_size=100.,
                              #case_sensitive=False)
    ntotal = 0
    npassed = 0
    nfailures = 0
    nmissing = 0

    ntotal_static = 0
    ntotal_eigen = 0
    nstatic = 0
    neigen = 0
    for bdf_filename in bdf_filenames:
        if 'Combined' in bdf_filename:
            continue
        root = os.path.splitext(bdf_filename)[0]
        base = os.path.basename(bdf_filename)[0]
        f06_filename = root + '.f06'
        op2_filename = root + '.op2'
        if not os.path.exists(op2_filename):
            print(f'****missing {op2_filename}')
            nmissing += 1
            ntotal += 1
            continue

        try:
            read_op2(op2_filename, debug=None)
            print(op2_filename)
            npassed += 1
            if base.startswith('S'):  # static, buckling
                nstatic += 1
            if base.startswith('E'):  # eigenvalue/modal
                neigen += 1
        except Exception as e:
            print(f'*{op2_filename}')
            print(e)
            nfailures += 1

        if base.startswith('S'):  # static, buckling
            ntotal_static += 1
            #try:
                #read_f06(f06_filename)
            #except:
                #print(f'*{f06_filename}')
        if base.startswith('E'):  # eigenvalue/modal
            ntotal_eigen += 1
        ntotal += 1
        sys.stdout.flush()
        sys.stderr.flush()
    print(f'{npassed}/{ntotal}; nmissing={nmissing}; nfailed={nfailures}')
    print(f'Static: {nstatic}/{ntotal_static}')
    print(f'Eigen:  {neigen}/{ntotal_eigen}')

def main():
    test_dirname = os.path.join('../../examples/Benchmark_11_29_2021/DAT')
    if IS_WINDOWS:
        run_jobs(test_dirname)
        check_jobs(test_dirname)
    else:
        run_jobs(test_dirname)

if __name__ == '__main__':
    #read_f06(r'D:\hd\mystran_november\mystran-op2-displacement-merge2\test\test_runs\Separated\Static\Banded\CurrentRuns\RODLOAD-MPC-FOR-SING-SB.f06')
    main()

