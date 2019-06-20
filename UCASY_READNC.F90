      PROGRAM READ_NC_UCASY
      IMPLICIT NONE
      INCLUDE "netcdf.inc"
      INTEGER              :: LONDIM, LATDIM, TMDIM, I_DIM, J_DIM, K_DIM
      INTEGER              :: NCID, XLONGID, XLATID
      INTEGER              :: M, ASTAT
      INTEGER              :: ATTR_LEN, NGATTS
      REAL,ALLOCATABLE     :: LONGITUDE(:,:,:), LATITUDE(:,:,:)
      REAL                 :: LON1, LAT1
      CHARACTER(LEN=255)   :: MESSAGE
      CHARACTER(LEN=255)   :: FILESPEC='wrfout_d01_2016-11-03_00:00:00'
      CHARACTER(LEN=255)   :: ATTR_NAME

      TYPE                 :: GLB_ATT
       INTEGER             :: LEN
       INTEGER             :: TYPE
       INTEGER,POINTER     :: ATTR_INT(:)   
       INTEGER(1),POINTER  :: ATTR_BYTE(:)
       INTEGER(2),POINTER  :: ATTR_SHORT(:)
       REAL,POINTER        :: ATTR_REAL(:)
       REAL(8),POINTER     :: ATTR_DBL(:)
       CHARACTER(LEN=132)  :: NAME   
       CHARACTER(LEN=256)  :: ATTR_CHAR         
      END TYPE GLB_ATT
  
      TYPE(GLB_ATT),POINTER:: ATTRS(:)
    
!---------------------------------------------------------------------
!   open wrf input file
!---------------------------------------------------------------------
      MESSAGE = 'wrf_file: Failed to open ' // TRIM(FILESPEC)
      CALL HANDLE_NCERR( NF_OPEN( TRIM(FILESPEC), NF_NOCLOBBER, NCID ), MESSAGE )       
!---------------------------------------------------------------------
!   get wrf diMesions
!---------------------------------------------------------------------
      MESSAGE = 'Failed to get lon dimension id'
      CALL HANDLE_NCERR( NF_INQ_DIMID( NCID, 'west_east', LONDIM ), MESSAGE )
      MESSAGE = 'Failed to get lon dimension'
      CALL HANDLE_NCERR( NF_INQ_DIMLEN( NCID, LONDIM, I_DIM ), MESSAGE )
       WRITE(*,*) 'lon dimension length is : ',I_DIM
      MESSAGE = 'Failed to get lat dimension id'
      CALL HANDLE_NCERR( NF_INQ_DIMID( NCID, 'south_north', LATDIM ), MESSAGE )
      MESSAGE = 'Failed to get lat dimension'
      CALL HANDLE_NCERR( NF_INQ_DIMLEN( NCID, LATDIM, J_DIM ), MESSAGE )
       WRITE(*,*) 'lat dimension length is : ',J_DIM
      MESSAGE = 'Failed to get time dimension id'
      CALL HANDLE_NCERR( NF_INQ_DIMID( NCID, 'Time', TMDIM ), MESSAGE )
      MESSAGE = 'Failed to get time dimension'
      CALL HANDLE_NCERR( NF_INQ_DIMLEN( NCID, TMDIM, K_DIM ), MESSAGE )
       WRITE(*,*) 'time dimension length is : ',K_DIM          
!---------------------------------------------------------------------
!   get wrf variables
!---------------------------------------------------------------------
      MESSAGE = 'wrf_file: Failed to get XLONG variable id'
      CALL HANDLE_NCERR( NF_INQ_VARID( NCID, 'XLONG', XLONGID ), MESSAGE )
     
      ALLOCATE( LONGITUDE(I_DIM,J_DIM,K_DIM),STAT=ASTAT )
      IF ( ASTAT /= 0 ) THEN
       WRITE(*,*) 'wrf_file: failed to allocate longitude variable; error = ',ASTAT
       STOP 'Alloc error'
      END IF
	  
      MESSAGE = 'wrf_file: Failed to read XLONG variable'
      CALL HANDLE_NCERR( NF_GET_VAR_REAL( NCID, XLONGID, LONGITUDE ), MESSAGE )      
      LON1 = LONGITUDE (1,1,1)    
	  WRITE(*,*) 'longitude of (1,1) point is ',LON1
      MESSAGE = 'wrf_file: Failed to get XLAT variable id'
      CALL HANDLE_NCERR( NF_INQ_VARID( NCID, 'XLAT', XLATID ), MESSAGE )
     
	  ALLOCATE( LATITUDE(I_DIM,J_DIM,K_DIM),STAT=ASTAT )
      IF ( ASTAT /= 0 ) THEN
       WRITE(*,*) 'wrf_file: failed to allocate latitude variable; error = ',ASTAT
       STOP 'Alloc error'
      END IF 
    
      MESSAGE = 'wrf_file: Failed to read XLAT variable'
      CALL HANDLE_NCERR( NF_GET_VAR_REAL( NCID, XLATID, LATITUDE ), MESSAGE )
      LAT1 = LATITUDE(1,1,1)
      WRITE(*,*) 'latitude of (1,1) point is ',LAT1
      DEALLOCATE( LONGITUDE )
      DEALLOCATE( LATITUDE  )

!---------------------------------------------------------------------
!   get global ATTR count
!---------------------------------------------------------------------
      MESSAGE = 'glb_attr: Failed to get glb attr count'
      CALL HANDLE_NCERR( NF_INQ_NATTS( NCID, NGATTS ), MESSAGE ) 
      WRITE(*,*) 'global attributes totally about ',NGATTS     
!---------------------------------------------------------------------
!   allocate variables
!---------------------------------------------------------------------
      IF ( NGATTS > 0 ) THEN
      ALLOCATE( ATTRS(NGATTS),STAT=ASTAT )
      IF ( ASTAT /= 0 ) THEN
       WRITE(*,*) 'glb_attr: failed to allocate type glb_att'
       STOP 'Alloc err'
      END IF 
      ATTRS(:)%NAME = ' '
      END IF
!---------------------------------------------------------------------
!   loop over glb attributes
!---------------------------------------------------------------------
      DO M = 1, NGATTS
       WRITE(MESSAGE,*) 'glb_attr: Failed to get glb attr # ',M,' name'
      CALL HANDLE_NCERR( NF_INQ_ATTNAME( NCID, NF_GLOBAL, M, ATTR_NAME ), MESSAGE )       
      ATTRS(M)%NAME = ATTR_NAME 
      WRITE(MESSAGE,*) 'glb_attr: Failed to get glb attr # ',M,' type,len'
      CALL HANDLE_NCERR( NF_INQ_ATT( NCID, NF_GLOBAL, TRIM(ATTR_NAME), ATTRS(M)%TYPE, ATTR_LEN ), MESSAGE )       
      ATTRS(M)%LEN = ATTR_LEN
      WRITE(*,*) ATTRS(M)%LEN
      MESSAGE = 'glb_attr: Failed to get ' // TRIM(ATTR_NAME)
      SELECT CASE( ATTRS(M)%TYPE )
       CASE( NF_BYTE )
         ALLOCATE( ATTRS(M)%ATTR_BYTE(ATTR_LEN),STAT=ASTAT )
         IF ( ASTAT /= 0 ) THEN
           WRITE(*,*) 'glb_attr: failed to allocate attr_byte'
           STOP 'Alloc err'
         END IF 
         CALL HANDLE_NCERR( NF_GET_ATT_INT1( NCID, NF_GLOBAL, TRIM(ATTR_NAME), ATTRS(M)%ATTR_BYTE ), MESSAGE )       
       CASE( NF_CHAR )
         ATTRS(M)%ATTR_CHAR = ' '
         CALL HANDLE_NCERR( NF_GET_ATT_TEXT( NCID, NF_GLOBAL, TRIM(ATTR_NAME), ATTRS(M)%ATTR_CHAR ), MESSAGE )       
       CASE( NF_SHORT )
         ALLOCATE( ATTRS(M)%ATTR_SHORT(ATTR_LEN),STAT=ASTAT )
         IF ( ASTAT /= 0 ) THEN
           WRITE(*,*) 'glb_ATTR: failed to allocate attr_short'
           STOP 'Alloc err'
         END IF
         CALL HANDLE_NCERR( NF_GET_ATT_INT2( NCID, NF_GLOBAL, TRIM(ATTR_NAME), ATTRS(M)%ATTR_SHORT ), MESSAGE )       
       CASE( NF_INT )
         ALLOCATE( ATTRS(M)%ATTR_INT(ATTR_LEN),STAT=ASTAT )
         IF ( ASTAT /= 0 ) THEN
           WRITE(*,*) 'glb_attr: failed to allocate attr_int'
           STOP 'Alloc err'
         END IF
         CALL HANDLE_NCERR( NF_GET_ATT_INT( NCID, NF_GLOBAL, TRIM(ATTR_NAME), ATTRS(M)%ATTR_INT ), MESSAGE )       
       CASE( NF_FLOAT )
         ALLOCATE( ATTRS(M)%ATTR_REAL(ATTR_LEN),STAT=ASTAT )
         IF ( ASTAT /= 0 ) THEN
           WRITE(*,*) 'glb_attr: failed to allocate attr_real'
           STOP 'Alloc err'
         END IF
         CALL HANDLE_NCERR( NF_GET_ATT_REAL( NCID, NF_GLOBAL, TRIM(ATTR_NAME), ATTRS(M)%ATTR_REAL ), MESSAGE )       
       CASE( NF_DOUBLE )
         ALLOCATE( ATTRS(M)%ATTR_DBL(ATTR_LEN),STAT=ASTAT )
         IF ( ASTAT /= 0 ) THEN
           WRITE(*,*) 'glb_attr: failed to allocate attr_dbl'
           STOP 'Alloc err'
         END IF
         CALL HANDLE_NCERR( NF_GET_ATT_DOUBLE( NCID, NF_GLOBAL, TRIM(ATTR_NAME), ATTRS(M)%ATTR_DBL ), MESSAGE )       
      END SELECT
      END DO

!---------------------------------------------------------------------
!   close wrf file
!---------------------------------------------------------------------
      MESSAGE = 'wrf_file: Failed to close ' // TRIM(FILESPEC)
      CALL HANDLE_NCERR( NF_CLOSE( NCID ), MESSAGE )       

      END PROGRAM READ_NC_UCASY

      SUBROUTINE HANDLE_NCERR( RET, MES )
      IMPLICIT NONE
      INCLUDE 'netcdf.inc'
!---------------------------------------------------------------------
!	... netcdf error handling routine
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
      INTEGER, INTENT(IN) :: RET
      CHARACTER(LEN=*), INTENT(IN):: MES

      IF ( RET /= NF_NOERR ) THEN
      WRITE(*,*) 'handle_ncerr: ',TRIM(MES)
      WRITE(*,*) NF_STRERROR( RET )
      STOP 'netcdf error'
      END IF

      END SUBROUTINE HANDLE_NCERR
