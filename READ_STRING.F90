PROGRAM UCASY
  IMPLICIT NONE
  INTEGER :: I, TIME, HOURS, MINUTES
  TIME = "9:15"
  I = INDEX(TIME,':')
  READ(TIME(:I-1),*) HOURS    ! String to integer
  READ(TIME(I+1:),*) MINUTES  ! String to integer
  WRITE(*,*) HOURS,MINUTES
  STOP
END PROGRAM
