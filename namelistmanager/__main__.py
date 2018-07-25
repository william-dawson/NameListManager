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
from documentation import create_doc
from module import create_mod
from readers import create_reader
from common import create_common
import xml.etree.ElementTree as ET

if __name__ == "__main__":
    if len(argv) < 5:
        raise Exception("Incorrect input parameters.")
    fname = argv[1]
    doc_path = argv[2]
    mod_path = argv[3]
    read_path = argv[4]

    # Read in the input file
    try:
        tree = ET.parse(fname)
    except:
        print("File could not be loaded", fname)
        quit()

    root = tree.getroot()
    for module in root:
        create_doc(module, doc_path)
        create_mod(module, mod_path)
        create_reader(module, read_path)
    create_common(mod_path)
