!
!     Convert a string to upper case.
      SUBROUTINE ToUpper(string, strlen)
!
      CHARACTER(len=*), INTENT(INOUT) :: string
      INTEGER, INTENT(IN) :: strlen
!
      INTEGER :: II
      INTEGER :: offset
!
      offset = ICHAR('A') - ICHAR('a')
      DO II = 1, strlen
         IF (string(II:II) .GT. 'a' .AND. string(II:II) .LE. 'z') THEN
            string(II:II) = CHAR(ICHAR(string(II:II)) + offset)
         END IF
      END DO
!
      END SUBROUTINE ToUpper
