'''
This file contains the procedures needed to generate documentation from the
xml file.
'''


def create_doc(module, output_path):
    '''
    Create the documentation associated with a given module.

    module: a tree of elements associated with that module.
    output_path: path to where we should put the output file.
    '''
    mod_name = module.attrib["name"]
    file_name = output_path + "/" + mod_name + ".rst"

    try:
        ofile = open(file_name, "w")
    except:
        print("Couldn't open file:", file_name)
        quit()

    head = mod_name.upper() + " Module"
    ofile.write(head + "\n")
    ofile.write("=" * len(head) + "\n\n")
    write_description(ofile, module.find("description").text)
    ofile.write("\n")

    for member in module:
        if member.tag == "description":
            continue
        mem_name = member.attrib["name"]
        head = mem_name.upper()
        ofile.write(head + "\n")
        ofile.write("-" * len(head) + "\n")
        write_description(ofile, member.find("description").text)
        ofile.write("  - Default: " + member.find("default").text + "\n")
        ofile.write("  - DataType: " + member.find("datatype").text + "\n")
        ofile.write("\n")

    # Cleanup
    ofile.close()


def write_description(ofile, description):
    '''
    A subroutine that writes out a description with the correct formatting.

    ofile: output stream.
    description: text to write
    '''

    text = description.lstrip().rstrip()

    # Remove the whitespace associated with new lines.
    start_idx = text.find("\n")
    while(start_idx != -1):
        substr = "\n"
        for end_idx in range(start_idx + 1, len(text)):
            if text[end_idx] != " ":
                break
            else:
                substr += " "
        text = text.replace(substr, " ")
        start_idx = text.find("\n")

    # Make sure it prints in chunks of 79 characters
    start = 0
    while (start < len(text)):
        end = start + 79
        if end > len(text):
            end = len(text)
        else:
            while(text[end - 1] != " " and end > start):
                end -= 1
        ofile.write(text[start:end] + "\n")
        start = end
