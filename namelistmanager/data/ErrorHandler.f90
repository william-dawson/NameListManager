!
!     If there is an I/O error, call this routine.
      SUBROUTINE HandleError(fname)
!
      CHARACTER(len=*), INTENT(IN) :: fname
!
      WRITE(*,*) "Problem with file ", fname
      CALL EXIT(-1)
!
      END SUBROUTINE HandleError
