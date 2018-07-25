'''
Generates code and documentation for a Fortran namelist.

Usage:
    python namelistmanager input.xml doc_path mod_path read_path
    input_list.xml : an xml document describing the input.
    doc_path : path to where the documentation should be placed.
    module_path : path to where the module files should be placed.
    read_path : path to where the reader source code should be placed.

'''
from sys import argv
from __init__ import Parse

if __name__ == "__main__":
    if len(argv) < 5:
        raise Exception("Incorrect input parameters.")
    fname = argv[1]
    doc_path = argv[2]
    mod_path = argv[3]
    read_path = argv[4]

    Parse(fname, doc_path, mod_path, read_path)
