/***************************************************************************

      Программа : MON
      Модуль    : TOOLS ( Вспомогательные функции )
      Файл      : TOOLS.PRG
      Автор     : Залманов А.З.

      Off-line Монитор оператора обслуживания физических лиц ИАС "Забота"

***************************************************************************/

#include "common.ch"
#include "clippext.ch"

#include "tools.ch"
#include "fileio.ch"

funcDef dayOfWeek with dayNo:NU local dayNames := ;
        { "воскресенье", "понедельник", "вторник",;
          "среда", "четверг", "пятница", "суббота" }
  default dayNo to dow( date())
  assert( dayNo > 0 .and. dayNo < 8 )
return dayNames[ dayNo ]

funcDef monthName with monthNo:NU local monthNames := ;
        { "Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", ;
          "Июль", "Август", "Сентябрь", "Октябрь", "Hоябрь", "Декабрь"}
  default monthNo to month( date() )
  assert( monthNo > 0 .and. monthNo < 13 )
return monthNames[ monthNo ]

funcDef div with devide:N, devider:N
return round( devide / devider, 0 )

funcDef addSlash with dirName:C
  dirName := allTrim( dirName )
  if right( dirName, 1) != "\" then;
   dirName := stuff( dirName, len( dirName ) + 1, 0, "\" )
return dirName

funcDef openFile with fileName:C, fileMode:NU local handle
  default fileMode to FO_READ
  if !file( fileName )
    messageB( "Отсутствует файл " + fileName )
    return 0
  endIf
  handle := fopen( fileName, fileMode )
  if handle < 1
    messageB( "Ошибка открытия файла " + fileName + " с кодом " + str( ferror()))
    return handle
  endIf
return handle

funcDef isArchive with  fName:C;
                  local fAttr, bitStr

  fAttr  := fileAttr( fName )
  if fAttr == 0 then return .F.
  bitStr := bitToC( fAttr, "ADVSHR" )
return "A" $ bitStr

funcDef setArchive with  fName:C
return setFAttr( fName ) == 0

funcDef clearArchive with  fName:C;
                     local fAttr
  fAttr := fileAttr( fName )
  if fAttr < FA_ARCHIVE then return -1
return setFAttr( fName, fAttr - FA_ARCHIVE )

funcDef fileSize with nHandle:N local savePos, rV
  savePos := FILE_POS( nHandle )
  rV      := fseek( nHandle, 0, FS_END )
  fseek( nHandle, savePos, FS_SET )
return rV

funcDef fwriteln with handle:N, string:CU
  if handle < 1 then return .F.
  default string to ""
return fwrite( handle, string + chr( 13 ) + chr( 10 )) == ( len( string ) + 2 )

funcDef getFNameExt with fName:C
return getFName( fName ) + "." + getFExt( fName )

funcDef freadln with  handle:N, string:CU;
                local buffer, _filePos, charsRead, eolPos
  buffer := space( MAX_STR_LEN )
  if handle == 0 then return .F.
  _filePos := FILE_POS( handle )
  charsRead := fread( handle, @buffer, MAX_STR_LEN )
  if ( eolPos := at( EOL, subStr( buffer, 1, charsRead ))) == 0
    string := buffer
  else
    string := subStr( buffer, 1, eolPos - 1 )
    fseek( handle, _filePos + eolPos + 1, FS_SET )
  endIf
return charsRead != 0

funcDef print with string:C
  set console off
  set printer on
  ?? string
  set printer off
  set console on
return nil

funcDef println with string:CU
  default string to " "
  print( string + EOL )
return nil

funcDef aSearch with arr:A, value:anyType local i
  for i := 1 to len( arr ) do;
   if valType( arr[ i ] ) == valType( value ) .and.;
      arr[ i ] == value then return i
return 0

funcDef aSearchTrim with arr:A, value:C local i
  for i := 1 to len( arr ) do;
   if allTrim( arr[ i ]) == allTrim( value ) then return i
return 0

funcDef getString with  name:C, number:N;
                  local i, handle := openFile( name ), s
  if handle < 1 then return ""
  if number < 1 then return ""
  for i := 1 to number do freadln( handle, @s )
  fclose( handle )
return s

funcDef freadbuf with  name:C, linesSkip:NU;
                 local strbuf, handle := openFile( name ), i
  default linesSkip to 0
  if handle < 1 then return ""
  for i := 1 to linesSkip do freadln( handle, strbuf )
  strbuf := freadstr( handle, 32768 )
  fclose( handle )
return strbuf

funcDef runEXE with  command:C;
               local useXMS, useEMS, useUMB, screen, video
  screen := saveScreen( ,,, )
  UseXMS := swpUseXMS( .F. )
  UseEMS := swpUseEMS( .F. )
  UseUMB := swpUseUMB( .F. )
  video  := swpVidMDE( .T. )
  swpRunCMD( command, 0, "", "")
  swpVidMDE( video )
  swpUseUMB( UseUMB)
  swpUseEMS( UseEMS)
  swpUseXMS( UseXMS)
  restScreen( ,,,, screen)
  setBlink( .F. )
  setCursor( 0 )
return nil

funcDef printFile with  fileName:C;
                  local handle
  if !checkPrinter() then return .F.
  if !file( fileName ) then return .F.
  prn( memoRead( fileName ))
  prnFormFeed()
return .T.

funcDef prnError with oE:O local rV
  rV := oE: canRetry .and. alarm({ "Принтер не готов!" },;
                                 { "Продолжить печать",;
                                   "Закончить печать" }) != 2
  if !rV then break oE
return rV

funcDef filePrint with fName:C, outputDev:CU local handle, s, bError, oE
  default outputDev to "PRN"
  handle := fopen( fName )
  if handle < 1 then return .F.
  set( _SET_PRINTFILE, outputDev, .T. )
  bError := errorBlock({| oE | prnError( oE )})
  begin sequence
    while freadln( handle, @s ) do println( s )
  recover using oE
//    OK( "П Е Ч А Т Ь    П P Е P В А H А" )
  endSequence
  errorBlock( bError )
  set( _SET_PRINTFILE, "" )
  fclose( handle )
return .T.

funcDef stripCRLF with  inStr:C;
                  local outStr := "", i, c
  for i := 1 to len( inStr )
    c := subStr( inStr, i, 1 )
    if !( c $ chr( 13 ) + chr( 10 ) + chr( 9 ) + chr( 255 )) then outStr += c
  next
return outStr

funcDef pathCheck with cPath:C local rV := .T., cDir, i
   if .NOT.empty( cPath)
      for i=1 to subDirs( cPath)
         cDir := extrDir( cPath, i)
         if .NOT.isDir( cDir)
            if dirMake( cDir) != 0
               rV := .F.
               exit
            endIf
         endIf
      endFor
   endIf
return rV

static funcDef subDirs with cPath:C local rV := 1, i
  for i=1 to len( cPath)
    if substr( cPath, i, 1) == "\" .or. substr( cPath, i, 1) == "/"
       rV++
    endIf
  next
return rV

static funcDef extrDir with cPath:C, limit:N local pos, i, cnt := 1
   for i=1 to len( cPath)
      if substr( cPath, i, 1) == "\" .or. substr( cPath, i, 1) == "/"
         cnt++
      endIf
      if cnt > limit
         pos := i - 1
         exit
      endIf
   endFor
   if empty( pos)
     pos := len( cPath)
     if right( cPath, 1 ) $ "\/" .and. len( cPath ) > 3 then pos--
   endIf
return substr( cPath, 1, pos)

procDef centerStartUp with cStr:C, bHelpHandler:ACBU  ;
   local savedColors, savedY1, savedX1, mN, y1, dx, x1
   savedColors := setColor()
   savedY1     := row()
   savedX1     := col()
   mN          := wStAMesLen()
   y1          := 1
   dx          := maxCol() - 1
   x1          := 0
   windowAltStartUp( @y1, @x1, 1, dx, setMesColor(), 0, bHelpHandler)
   @ y1+1, x1+2 SAY padC( cStr, dx - 3 )
   wStAMesPush( seconds())
   setColor( savedColors)
   setPos( savedY1, savedX1)
return

procDef centerExit with timeSlice:NU local actTime := wStAMesPop()
   DEFAULT timeSlice TO 0.5
   timeSlice = timeSlice - seconds() + actTime        // Сколько еще ждать
   if timeSlice > 0.1 then helpKey( timeSlice)        // Еще не дождались
   windowExit()
return

procDef moveFile with source:C, dest:C
  __copyFile( source, dest )
  ferase( source )
return 

funcDef sumMin with arr:A local i, min
  min := arr[ 1 ]
  for i := 2 to len( arr ) do;
    if arr[ i ] < min then min := arr[ i ]
return min

funcDef freshOrder with oB:O, alias:C local nRec
   nRec := ( alias ) -> ( recNo())
   oB: refreshAll()
   oB: forceStable()
   if ( nRec != ( alias ) -> ( lastRec()) + 1 )
     while (( alias ) -> ( recNo()) != nRec .and.;
           !( alias ) -> ( bof()))
       select ( alias )
       oB: up()
       oB: forceStable()
     endDo
   endIf
return nil

funcDef zStr with nInt:N, nDig:N
return padL( nStr( int( nInt )), nDig, "0" )

procDef fKeysDraw with fKeys:A local i, label, cColor
  for i := 1 to 10
    label := ""
    if len( fKeys ) >= i .and. !empty( fKeys[ i ][ FK_NORMAL ][ FK_TITLE ])
      label  := padR( fKeys[ i ][ FK_NORMAL ][ FK_TITLE ], 6 )
      cColor := hiliteColor( setBrwColor())
    else
      label  := space( 6 )
      cColor := normalColor( setBrwColor())
    endIf
    @ maxRow(), ( i - 1 ) * 8 + 2 say label color cColor
  next
return

funcDef fRound with nVal:N local nFraq, nInt
  round( nVal, 0 )
  nVal  /= 100
  nInt  := int( nVal )
  nFraq := fraq( nVal )
  if nFraq > 0.5 then nInt += 1
return ( nInt * 100 )

funcDef baseSort with dbInfo:A, cField:C local tmpFile, backupFile
  tmpFile    := getFPath( dbInfo[ 1 ]) + "\" +;
                getFName( getTmpName( getFPath( dbInfo[ 1 ]))) + ".DBF"
  backupFile := getFPath( dbInfo[ 1 ]) + "\" +;
                getFName( dbInfo[ 1 ]) + ".$$$"
  if !baseMXOpen( dbInfo ) then return .F.
  messageStartup( "Выполняется сортировка..." )
  &( dbInfo[ 2 ]) -> ( __dbSort( tmpFile, { cField }))
  messageExit( MESSAGE_DELAY )
  baseMClose( dbInfo )
  if file( backupFile ) then ferase( backupFile )
  if frename( dbInfo[ 1 ], backupFile ) != 0 then return .F.
  if frename( tmpFile, dbInfo[ 1 ])     != 0 then return .F.
  baseIndex( dbInfo )
return .T.

func DummyEStr; return eStr( 1 )

funcDef errMsg with oErr:O, cMsg:CAU
  default cMsg to ""
  if isCHARACTER( cMsg )
    messageB( cMsg )
  else
    OK( cMsg )
  endIf
  break oErr
return .F.

funcDef rStr with nVal:N, nWidth:NU
  default nWidth to 6
return str( nVal, nWidth, 2 )

procDef appendBase with  cDestName:C, cSourceName:C;
                   local i, nPos, lSOpened, lDOpened
  lSOpened := .F.
  lDOpened := .F.
  if !IS_OPEN( cSourceName )
    use ( cSourceName ) new exclusive alias A_SRC_BASE
    cSourceName := "A_SRC_BASE"
    lSOpened    := .T.
  endIf
  if !IS_OPEN( cDestName )
    if !file( cDestName ) then;
     dbCreate( cDestName, ( cSourceName ) -> ( dbStruct()))
    use ( cDestName ) new exclusive alias A_DST_BASE
    cDestName := "A_DST_BASE"
    lDOpened  := .T.
  endIf
  nPos := ( cSourceName ) -> ( recNo())
  operStartup( "Идет перезапись данных", 1, ( cSourceName ) -> ( recCount()))
  ( cSourceName ) -> ( netGoTop())
  do while !( cSourceName ) -> ( eof())
    ( cDestName ) -> ( dbAppend())
    for i := 1 to ( cSourceName ) -> ( fCount()) do;
     ( cDestName ) -> ( fieldPut( i, ( cSourceName ) -> ( fieldGet( i ))))
    operProc(( cSourceName ) -> ( recNo()))
    exit if !( cSourceName ) -> ( netSkipDown())
  endDo
  ( cSourceName ) -> ( dbGoTo( nPos ))
  operExit()
  if lSOpened
    close ( cSourceName )
  endIf
  if lDOpened
    close ( cDestName )
  endIf
return

funcDef dbTempOpen with dbInfo:A
  tempCreate( dbInfo )
  baseMXOpen( dbInfo )
return dbInfo[ 1 ]

procDef dbTempClose with dbInfo:A
  baseMClose( dbInfo )
  tempErase( dbInfo )
return

procDef tempErase with dbInfo:A local i
  for i = 1 to len( dbInfo[ 4 ]) do ferase( dbInfo[ 4 ][ i ][ 1 ])
  ferase( dbInfo[ 1 ])
return

funcDef salStr with nSum:N, nWidth:NU local rV
  rV := mStr( abs( nSum ))
  if nSum < 0 
    rV += " ДБ"
  else
    rV += " КР"
  endIf
  if nWidth != nil then rV := padL( allTrim( rV ), nWidth )
return rV

funcDef shortName with fName:C, name:C, sName:C local rV
  if empty( fName ) .and. empty( name ) .and. empty( sName ) then return ""
  rV := allTrim( fName ) + " " + left( allTrim ( name ), 1 ) + "."
  if !empty( sName ) then rV += left( allTrim ( sName ), 1 ) + "."
return rV

funcDef truncStr with cStr:C, maxLen:N
  cStr := allTrim( cStr )
  if len( cStr ) > maxLen then cStr := left( cStr, maxLen - 3 ) + "..."
return cStr

funcDef truncStrR with cStr:C, maxLen:N
return padR( truncStr( cStr, maxLen ), maxLen )

funcDef truncStrL with cStr:C, maxLen:N
return padL( truncStr( cStr, maxLen ), maxLen )

funcDef stuff2 with sourceStr:C, startPos:N, width:N, insStr:C local rV
return left( sourceStr, startPos - 1 ) + insStr +;
       right( sourceStr, len( sourceStr ) - startPos - width + 1 )

funcDef changeFileExt with cFile:C, cExt:C 
return getFPath( cFile ) + "\" + getFName( cFile ) + cExt

funcDef renameFileExt with cFile:C, cExt:C local tmpName
  tmpName := changeFileExt( cFile, cExt ) 
  if file( tmpName ) then ferase( tmpName )
return frename( cFile, tmpName )

funcDef copyFileExt with cFile:C, cExt:C local tmpName
  tmpName := changeFileExt( cFile, cExt ) 
  __copyFile( cFile, tmpName )
return file( tmpName )

procDef baseZap with dbInfo:A local i
  ( dbInfo[ 2 ]) -> ( __dbZap())
  ( dbInfo[ 2 ]) -> ( ordCondSet())
  ( dbInfo[ 2 ]) -> ( ordListRebuild())

//  for i := 1 to len( dbInfo[ 3 ]) 
//    index on ( dbInfo[ 4 ][ i ][ 2 ]) to ( dbInfo[ 4 ][ i ][ 1 ])
//  next
return

funcDef cStr with nSum:N
return allTrim( mStr( nSum ))

