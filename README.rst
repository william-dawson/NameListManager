Name List Manager
=================

This program automatically generates code and documentation for Fortran
programs that manage their input using namelists. To use it, you first write an
xml file which describes all the input parameters. An example input file is
available in `example/input.xml` . This xml file describes all the parameters,
as well as their data types and purposes. Then, you run this program on the
xml file, and it automatically generates:

-   Restructered text documenting the parameters, including default values.
-   Module files to store all the input variables.
-   Reader subroutines that read in data from a namelist specifying input
    values.

You install the program as::

  pip install .

And then run it like::

  namelistmanager input.xml doc_path mod_path read_path

You can run the examples using the following command::

  namelistmanager example/input.xml example/doc example/module example/readers

The documentation, module files, and readers will then be available in the
example directory. The documentation files are restructed text, which means
you could easily add them to a Sphinx project. There is also a sample
driver program and makefile available in the `examples` directory, so you
can test out the code.

Requirements
------------

This package requires Python 3.0 and up. It also depends on the lxml package,
which pip will automatically install.

XML File Details
----------------

The XML is used to describe the different input variables available in the
namelist. To begin your xml file, wrap everything in the input tag::

  <input name="project_name" lang="en" mpi="false">
  ...
  </input>

Inside `<input>` you can place different groups. For each group, a module
will be created to store the data of that group, and a reader for that group
as well. Each group is identified by the name attribute::

  <group name="matrixmultiplication">
  </group>

Inside each group, you need to provide a list of descriptions of that group::

  <description_list>
    <description xml:lang="en">
      The parameters for performing matrix multiplication.
    </description>
    <description xml:lang="ja">
      説明
    </description>
  </description_list>

Next you specify the elements of that group. The variable name associated with
that element is based on the name attribute. Each element also requires a
description of that variable. You also need to specify the data type and
the default value::

  <element_list>
    <element name="block_size">
      <description_list>
        <description xml:lang="en">Size of blocks.</description>
      </description_list>
      <datatype>integer</datatype>
      <default>64</default>
    </element>
  </element_list>

You can optionally add a description of what type of values are valid::

  <valid_list>
    <valid name="1">Block Size 1</valid>
    <valid name="2">Block Size 2</valid>
    <valid name="4">Block Size 4</valid>
  </valid_list>

Any XML file you create can be validated against the XML schema stored in
`namelistmanager/namelist.xsd` .

Additional Options
------------------

The default language can be specified using the lang attribute in the
input element. You can also specify whether to generate MPI safe input handling.
This mainly affects how error handling is performed.
