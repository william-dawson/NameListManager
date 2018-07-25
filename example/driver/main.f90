!     Driver program showing the input reader in action.
      PROGRAM IOCommonModule
!
      USE SCFInputModule, ONLY : max_iterations, coulomb, exchange, direct
!
      IMPLICIT NONE
!
      CALL SCFReader(.TRUE., "input.nml")
!
      WRITE(*,"(A I4)") "Max iterations was specified: ", max_iterations
      WRITE(*,"(A A)") "Coulomb was specified: ", coulomb
      WRITE(*,"(A A)") "Exchange is default: ", exchange
      WRITE(*,"(A L1)") "Direct is default: ", direct
!
      END PROGRAM
