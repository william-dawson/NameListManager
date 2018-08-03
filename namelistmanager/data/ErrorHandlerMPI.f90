!
!     If there is an I/O error, call this routine.
      SUBROUTINE HandleError(fname)
!
      CHARACTER(len=*), INTENT(IN) :: fname
      INTEGER :: ierr
!
      WRITE(*,*) "Problem with file ", fname
      CALL MPI_Abort(ierr, -1)
!
      END SUBROUTINE HandleError
