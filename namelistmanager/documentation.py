'''This file contains the procedures needed to generate documentation from
the xml file.
'''

from .helpers import find_description


def create_doc(module, output_path, language):
    '''Create the documentation associated with a given module.

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
    write_description(ofile, module.find("description_list"), language)
    ofile.write("\n")

    for member in module.find("element_list"):
        mem_name = member.attrib["name"]
        head = mem_name.upper()
        ofile.write(head + "\n")
        ofile.write("-" * len(head) + "\n")
        write_description(ofile, member.find("description_list"), language)
        ofile.write("\n")
        try:
            ofile.write("- Default: " + member.find("default").text + "\n")
        except:
            ofile.write("- Default: \"\"\n")
        ofile.write("- DataType: " + member.find("datatype").text + "\n")
        for valid_list in member.findall("valid_list"):
            ofile.write("- Valid Values:\n\n")
            for valid in valid_list.findall("valid"):
                valid_value = valid.attrib["name"]
                ofile.write("  - " + valid_value + " : " +
                            valid.text.lstrip().rstrip() + "\n")
        ofile.write("\n")

    # Cleanup
    ofile.close()


def write_description(ofile, description_list, language):
    '''A subroutine that writes out a description with the correct format.

    ofile: output stream.
    description_list: description list.
    language: language to write in.
    '''
    description = find_description(description_list, language)
    text = description.text.lstrip().rstrip()

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
