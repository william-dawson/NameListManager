'''Some helpers common to the different modules in this package.
'''

# Global variables
off = " " * 6
offcom = " " * 5
offcont = " " * 4
offlabel = " " * 2

def find_description(description_list, language):
    '''Find a description in the desired language.
    '''
    found = False
    for description in description_list:
        if description.attrib["lang"] == language:
            found = True
            break
    if not found and language != "en":
        return find_description(description_list, "en")
    elif not found:
        return description_list.find("description")
    else:
        return description
