PROGRAM UCASY
  IMPLICIT NONE
  INTEGER,PARAMETER :: NCITY=9
  INTEGER           :: I,ERROR, AQI
  CHARACTER(LEN=10) :: DATE, LEVEL
  CHARACTER(LEN=55) :: FLNM,FLNM1
  CHARACTER(LEN=10) :: CITY(NCITY) = &
          (/'baicheng ','baishan  ', &
 	          'changchun','jilin    ', &
 	          'liaoyuan ','siping   ', &
 	          'songyuan ','tonghua  ', &
 	          'yanbian  '/)
  DO I=1,NCITY
   FLNM='air_'//TRIM(CITY(I))//'_2019.csv'
   FLNM1='output_'//TRIM(CITY(I))//'_2019.csv'
   OPEN(10,FILE=FLNM1)
   OPEN(11,FILE=FLNM)
   DO
   READ(11,*,IOSTAT=ERROR) DATE, LEVEL, AQI
   IF(ERROR/=0) EXIT
   IF (AQI<=100) THEN
   WRITE(10,*)  DATE,',',LEVEL,',',AQI
   END IF
   END DO
  END DO

 END PROGRAM UCASY

