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

You run the program as::

  python namelistmanager input.xml doc_path mod_path read_path

You can run the examples using the following command::

  python namelistmanager example/input.xml example/doc example/module example/readers

The documentation, module files, and readers will then be available in the
example directory.

Requirements
------------

This package requires the lxml python package, which can be installed with
pip::

  pip install lxml

XML File Details
----------------

The XML is used to describe the different input variables available in the
namelist. To begin your xml file, wrap everything in the input tag::

  <input>
  ...
  </input>

Inside `<input>` you can place different groups. For each group, a module
will be created to store the data of that group, and a reader for that group
as well. Each group is identified by the name attribute::

  <group name="groupa">
  </group>

Inside each group, you need to provide a list of descriptions of that group::

  <description_list>
    <description language="en">
      Description.
    </description>
    <description language="ja">
      説明
    </description>
  </description_list>

Next you specify the elements of that group. The variable name associated with
that element is based on the name attribute. Each element also requires a
description of that variable. You also need to specify the data type and
the default value::

  <element_list>
    <element name="">
      <description_list>
        <description language="en"></description>
      </description_list>
      <datatype></datatype>
      <default></default>
    </element>
  </element_list>

Any XML file you create can be validated against the XML schema stored in
`namelistmanager/namelist.xsd` .

Additional Options
------------------

There are a few more options you might want to specify.
