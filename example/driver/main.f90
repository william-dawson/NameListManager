!     Driver program showing the input reader in action.
      PROGRAM IOCommonModule
!
      USE SCFInputModule, ONLY : max_iterations
!
      IMPLICIT NONE
!
      CALL SCFReader("input.nml")
!
      WRITE(*,*) "Max iterations:", max_iterations
!
      END PROGRAM
