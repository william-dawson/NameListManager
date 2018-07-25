'''Some helpers common to the different modules in this package.
'''


def find_description(description_list, language):
    '''Find a description in the desired language.
    '''
    found = False
    for description in description_list:
        if description.attrib["language"] == language:
            found = True
            break
    if not found and language != "en":
        return find_description(description_list, "en")
    elif not found:
        return description_list.find("description")
    else:
        return description
