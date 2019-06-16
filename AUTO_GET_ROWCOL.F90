!This code comes from fcode.cn
Module DFile_Mod
  Implicit None
  
contains 

  Integer Function GetDataN( cStr )
    Character( Len = * ) , Intent( IN ) :: cStr
    Integer :: i
    Logical :: bIsSeparator , bIsQuote
    GetDataN = 0
    bIsSeparator = .TRUE.
    bIsQuote = .FALSE.
    Do i = 1 , Len_Trim( cStr )
      Select Case( cStr(i:i) )
      Case( '"' , "'" ) !// 如果遇到引号
        If ( .Not.bIsQuote ) GetDataN = GetDataN + 1  !//如果不在引号中，则增加一个数据
        bIsQuote = .Not.bIsQuote !// 引号结束或开始
        bIsSeparator = .FALSE.
      Case( " " , "," , char(9) ) !// 如果遇到分隔符
        If ( .Not.bIsQuote ) then  !// 分隔符如果不在引号中
          bIsSeparator = .TRUE.
        End If
      Case Default      
        If ( bIsSeparator ) then
          GetDataN = GetDataN + 1
        End If
        bIsSeparator = .FALSE.
      End Select
    End Do
  End Function GetDataN
  
  Function f_numbervars(vars) result(numvars)
    character(len=*), intent(in) :: vars
    integer :: numvars
    character(len=len(vars)) :: tmpvars
    character(len=256) :: tmpvar
    tmpvars = trim(adjustl(vars))
    numvars = 0
    do while (len_trim(tmpvars) > 0)
      read(tmpvars, *) tmpvar
      numvars = numvars + 1
      tmpvars = tmpvars(index(tmpvars, trim(tmpvar))+len_trim(tmpvar):)
    end do
  End Function f_numbervars
  
  Integer Function GetFileN( iFileUnit )
  !// 此函数应在打开文件后立即调用。调用后读取位置返回文件起始位置
    Implicit None
    Integer , Intent( IN ) :: iFileUnit
    character( Len = 1 ) :: cDummy
    integer :: ierr
    GetFileN = 0
    Rewind( iFileUnit )
    Do
      Read( iFileUnit , * , ioStat = ierr ) cDummy
      If( ierr /= 0 ) Exit
      GetFileN = GetFileN + 1
    End Do
    Rewind( iFileUnit )
  End Function GetFileN 

End Module DFile_Mod

Program www_fcode_cn
  use DFile_Mod
  Implicit None
  Character( Len = 512 ) :: cLine
  integer :: nRow , nCol , i
  Open( 12 , File = 'in.txt' )
  nRow = GetFileN( 12 )
  write( * , * ) '文件共',nRow,'行！'
  Do i = 1 , nRow
    read( 12 , '(a512)' ) cLine
    nCol = GetDataN( cLine )
    write( * , * ) i,'行有',nCol,'个数据'
  End Do
  Close( 12 )
End Program www_fcode_cn