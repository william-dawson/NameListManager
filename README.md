# NameListManager

This program automatically generates code and documentation for Fortran
programs. To use it, you first write an xml file which describes all the
input parameters. An example input file is available in `example/input.xml` .
This xml file describes all the parameters, as well as their data types
and purposes. Then, you run this program on the xml file, and it automatically
generates:

-   Restructered text documenting the parameters, including default values.
-   Module files to store all the input variables.
-   Reader subroutines that read in data from a namelist specifying input values.

You run the program as:

> python generate.py input.xml doc_path mod_path read_path

You can run the examples using the following command:

> python generate.py example/input.xml example/doc example/module example/readers

The documentation, module files, and readers will then be available in the
example directory.

## XML File Details
