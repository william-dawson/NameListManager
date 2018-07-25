'''
This file contains the procedures needed to generate the module files associated
with each namelist entry.
'''

from .helpers import find_description

def create_mod(module, output_path, language):
    '''Create the module file associated with a given module.

    module: a tree of elements associated with that module.
    output_path: path to where we should put the output file.
    '''

    mod_name = module.attrib["name"]
    file_name = output_path + "/" + mod_name + "_input_module.f90"

    try:
        ofile = open(file_name, "w")
    except:
        print("Couldn't open file:", file_name)
        quit()

    # Header
    off = " "*6
    offcom = " "*5
    write_comment(ofile, module.find("description_list"), offcom, language)
    ofile.write(off+"MODULE "+mod_name.upper()+"InputModule"+"\n")
    ofile.write("!\n")
    ofile.write(off + "USE IOCommonModule, ONLY : instr_len\n")
    ofile.write("!\n")
    ofile.write(off+"IMPLICIT NONE\n")
    ofile.write("!\n")

    # Members
    for member in module.find("element_list"):
        # Write the description
        write_comment(ofile, member.find("description_list"), offcom, language)
        # Write Variable
        datatype = member.find("datatype").text.upper()
        if datatype == "STRING":
            datatype = "CHARACTER(LEN=instr_len)"
        name = member.attrib["name"]
        ofile.write(off+datatype+" :: "+name+"\n")

    # Footer
    ofile.write("!\n")
    ofile.write(off+"END MODULE")

    # Cleanup
    ofile.close()

def write_comment(ofile, description_list, off, language):
    '''A subroutine that writes out a comment with the correct format.

    ofile: output stream.
    description_list: description list to write
    off: the offset for comments between the ! and the text.
    language: language to write in.
    '''
    description = find_description(description_list, language)
    text = description.text.lstrip().rstrip()

    # Remove the whitespace associated with new lines.
    start_idx = text.find("\n")
    while(start_idx != -1):
        substr = "\n"
        for end_idx in range(start_idx+1, len(text)):
            if text[end_idx] != " ":
                break
            else:
                substr += " "
        text = text.replace(substr, " ")
        start_idx = text.find("\n")

    # Make sure it prints in chunks of 72 - offset characters
    start = 0
    while (start < len(text)):
        end = start + 72 - len(off)
        if end > len(text):
            end = len(text)
        else:
            while(text[end-1] != " " and end > start):
                end -= 1
        ofile.write("!"+off+text[start:end]+"\n")
        start = end
