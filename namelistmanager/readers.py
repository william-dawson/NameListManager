'''
This file contains the procedures needed to generate the reader subroutines
associated with each namelist entry.
'''

from .helpers import off, offcom, offlabel, offcont

def create_reader(module, output_path):
    '''Create the reader file associated with a given module.

    module: a tree of elements associated with that module.
    output_path: path to where we should put the output file.
    '''
    mod_name = module.attrib["name"]
    file_name = output_path + "/" + mod_name + "_reader.f90"

    try:
        ofile = open(file_name, "w")
    except:
        print("Couldn't open file:", file_name)
        quit()

    # Header
    txt = "Reads in the data associated with " + mod_name.upper() + "Module"
    ofile.write("!" + offcom + txt + "\n")
    ofile.write(off + "SUBROUTINE " + mod_name.upper() + "Reader")
    ofile.write("(fname)\n")
    ofile.write("!\n")

    # Use Statements
    ofile.write(off + "USE IOCommonModule, ONLY : HandleError\n")
    ofile.write(off + "USE " + mod_name.upper() + "InputModule, ONLY : &\n")
    entry_list = variable_list(module)
    entry_list = ", ".join(entry_list)
    write_list(ofile, entry_list, offcont)
    ofile.write("!\n")

    # Implicit None Part
    ofile.write(off + "IMPLICIT NONE\n")
    ofile.write("!\n")

    # Input variables
    ofile.write(off + "CHARACTER(len=*), INTENT(IN) :: fname\n")
    ofile.write("!\n")

    # Helper Variables
    ofile.write(off + "INTEGER, PARAMETER :: IO = 16\n")
    ofile.write("!\n")

    # The namelist
    ofile.write(off + "NAMELIST /"+mod_name.upper()+"/ &\n")
    write_list(ofile, entry_list, offcont)
    ofile.write("!\n")

    # First we set the default values.
    default_values(ofile, module.find("element_list"))

    # Open the file
    ofile.write("!"+offcom+"Open the input file\n")
    txt = "OPEN(UNIT=IO, FILE=fname, STATUS='OLD', " + \
          "ACCESS='SEQUENTIAL', ERR=100)\n"
    ofile.write(off+txt)
    ofile.write("!\n")

    # Read the namelist
    ofile.write("!"+offcom+"Read The Name List\n")
    ofile.write(off+"READ(IO, "+mod_name.upper()+", ERR=100)\n")
    ofile.write("!\n")

    # Close The Input File
    ofile.write("!"+offcom+"Cleanup\n")
    ofile.write(off+"CLOSE(IO)\n")
    ofile.write(off+"GO TO 200\n")
    ofile.write("!\n")

    # Error Handling At The End
    ofile.write("!"+offcom+"Error Handling\n")
    ofile.write(offlabel+"100 CONTINUE\n")
    ofile.write(off+"CALL HandleError(fname)\n")

    # Footer
    ofile.write(offlabel+"200 CONTINUE\n")
    ofile.write("!\n")
    ofile.write(off + "END SUBROUTINE " + mod_name.upper() + "Reader\n")

    # Cleanup
    ofile.close()


def variable_list(module):
    '''
    Gets a list of variables associated with this module.

    module: the module to process.
    '''
    member_list = []
    for member in module.find("element_list"):
        member_list.append(member.attrib["name"].upper())
    return member_list


def write_list(ofile, text, off):
    '''A subroutine that writes out a list of variables with the correct
    format.

    ofile: output stream.
    text: list to write
    off: how far to indent
    '''

    # Make sure it prints in chunks of 72 - offset characters
    start = 0
    while (start < len(text)):
        end = start + 72 - len(off)
        if end > len(text):
            end = len(text)
        else:
            while(text[end - 1] != " " and end > start):
                end -= 1
        ofile.write(off + " &   " + text[start:end])
        start = end
        if (start < len(text)):
            ofile.write("&\n")
        else:
            ofile.write("\n")

def default_values(ofile, module):
    '''Write code to fill in all the default values of variables.

    ofile: file stream to write to.
    module: module to write.
    '''
    ofile.write("!"+offcom+"Fill in the default values.\n")
    for element in module:
        ofile.write(off+element.attrib["name"] + " = ")
        if element.find("datatype").text != "string":
            ofile.write(element.find("default").text)
        else:
            ofile.write("\""+element.find("default").text+"\"")
        ofile.write("\n")
    ofile.write("!\n")
