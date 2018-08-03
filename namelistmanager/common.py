'''This file contains the code needed to generate the common module used to
support the readers.
'''

from .helpers import off, offcom

def create_common(output_path, mpi):
    '''Creates the common module.

    output_path: pass the path to the modules.
    '''
    file_name = output_path + "/io_common_module.f90"

    try:
        ofile = open(file_name, "w")
    except:
        print("Couldn't open file:", file_name)
        quit()

    # Header
    ofile.write("!" + offcom + "Common information to support the readers\n")
    ofile.write(off + "MODULE IOCommonModule\n")
    ofile.write("!\n")
    ofile.write(off + "IMPLICIT NONE\n")
    ofile.write("!\n")

    # Common members
    ofile.write(off + "INTEGER, PARAMETER :: instr_len = 20\n")
    ofile.write("!\n")

    # Subroutines
    ofile.write(off + "CONTAINS\n")

    # Error Handler
    ofile.write("!\n")
    write_error_handler(ofile, mpi)

    # String Helper
    ofile.write("!\n")
    write_to_upper(ofile)

    # Footer
    ofile.write("!\n")
    ofile.write(off + "END MODULE")

    # Cleanup
    ofile.close()

def write_error_handler(ofile, mpi):
    '''This subroutine will write the sourcecode for the error handler.

    ofile: file to write to.
    mpi: 1 if you want to use mpi, 0 otherwise.
    '''
    from pkg_resources import resource_filename

    if mpi:
        ename = "ErrorHandlerMPI.f90"
    else:
        ename = "ErrorHandler.f90"
    code_file = resource_filename('namelistmanager', 'data/'+ename)

    with open(code_file, "r") as ifile:
        for line in ifile:
            ofile.write(line)

def write_to_upper(ofile):
    '''This subroutine will write the helper that converts strings to upper
    case.

    ofile: file to write to.
    '''
    from pkg_resources import resource_filename

    code_file = resource_filename('namelistmanager', 'data/ToUpper.f90')

    with open(code_file, "r") as ifile:
        for line in ifile:
            ofile.write(line)
