'''
The driver routine for generating the documents.


'''
from .documentation import create_doc
from .modules import create_mod
from .readers import create_reader
from .common import create_common
from lxml import etree
from pkg_resources import resource_filename

def Parse(fname, doc_path, mod_path, read_path):
    '''
    Parse an xml file and generate all the code.

    fname: the name of the xml file.
    doc_path: path to the documentation to generate.
    mod_path: path to where to generate modules.
    read_path: path to where to generate input readers.
    '''
    # Read in the input file
    try:
        tree = etree.parse(fname)
        root = tree.getroot()
    except:
        print("File could not be loaded", fname)
        quit()

    # Validate the input file
    schema_file = resource_filename('namelistmanager', 'data/namelist.xsd')
    xmlschema = etree.parse(schema_file)
    xmlschema = etree.XMLSchema(xmlschema)
    xmlschema.assertValid(tree)

    # Optional parameters
    if "lang" in root.attrib.keys():
        language = root.attrib["lang"]
    else:
        language = "en"

    if "mpi" in root.attrib.keys():
        mpi = (root.attrib["mpi"].lower() ==
               "true" or root.attrib["mpi"] == "1")
    else:
        mpi = False

    # Run the generators
    for module in root:
        create_doc(module, doc_path, language)
        create_mod(module, mod_path, language)
        create_reader(module, read_path)
    create_common(mod_path, mpi)
