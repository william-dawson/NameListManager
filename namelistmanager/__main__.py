'''Generates code and documentation for a Fortran namelist.

Usage:
    namelistmanager input.xml doc_path mod_path read_path
    input_list.xml : an xml document describing the input.
    doc_path : path to where the documentation should be placed.
    module_path : path to where the module files should be placed.
    read_path : path to where the reader source code should be placed.
    language : what language to use (optional, default="en").
    mpi : 1 if you want mpi support (optional, default="0").
'''
from sys import argv
from .__init__ import Parse


def main():
    # Parse input
    if len(argv) < 5:
        print(__doc__)
        raise Exception("Incorrect input parameters.")

    fname = argv[1]
    doc_path = argv[2]
    mod_path = argv[3]
    read_path = argv[4]

    # Optional Parameters
    if len(argv) > 5:
        language = argv[5]
    else:
        language = None
    if len(argv) > 6:
        mpi = int(argv[6])
    else:
        mpi = None

    Parse(fname, doc_path, mod_path, read_path, language, mpi)


if __name__ == "__main__":
    main()
