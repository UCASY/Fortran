PROGRAM UCASY
  IMPLICIT NONE
  INTEGER :: I, TIME, HOURS, MINUTES
  TIME = "9:15"
  I = INDEX(C,':')
  READ(C(:I-1),*) HOURS    ! String to integer
  READ(C(I+1:),*) MINUTES  ! String to integer
  WRITE(*,*) HOURS,MINUTES
  STOP
END PROGRAM