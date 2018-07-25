'''This file contains the code needed to generate the common module used to
support the readers.
'''


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
    off = " " * 6
    offcom = " " * 5
    # write_comment(ofile, module.find("description").text, offcom)
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
    ofile.write("!\n")

    # Error Handler
    ofile.write("!" + offcom + "If there is an I/O error, call this routine.\n")
    ofile.write(off + "SUBROUTINE HandleError(fname)\n")
    ofile.write("!\n")
    ofile.write(off + "CHARACTER(len=*), INTENT(IN) :: fname\n")
    ofile.write("!\n")
    ofile.write(off + "WRITE(*,*) \"Problem with file \", fname\n")
    ofile.write(off + "CALL EXIT(-1)\n")
    ofile.write("!\n")
    ofile.write(off + "END SUBROUTINE HandleError\n")

    # Footer
    ofile.write("!\n")
    ofile.write(off + "END MODULE")

    # Cleanup
    ofile.close()
