/***************************************************************************

      �ணࠬ�� : MON
      �����    : DCGN ( P���७�� ������� ���⮢ �-�� ����� )
      ����      : DCGN.PRG
      ����     : �������� �.�.

      Off-line ������ ������ ���㦨����� 䨧��᪨� ��� ��� "�����"

***************************************************************************/

#include "common.ch"
#include "clippext.ch"

#include "tools.ch"
#include "fileio.ch"

#define DCGN_TEMP_FILE "$$TEMP$$.$$$"

static dcgnHandle := 0

funcDef printLongStr with  str:C, onPages:LU local i, j, s
  default onPages to .F.
  if empty( str ) then return .F.
  if !onPages
    print( str )
    return .T.
  endIf
  repeat
    i := at( EOP, str )
    j := dialog( "��⠢�� ��࠭���...",;
                 { "�த������ �����",;
                   "�ய����� ��࠭���",;
                   "�������� �����" })
    do case
      case j == 1
        if !checkPrinter() then return .F.
        print( if( i == 0, str, left( str, i )))
        str := subStr( str, i + 1, 64000 )
      case j == 2
        str := subStr( str, i + 1, 64000 )
      case j == 3 .or. j == 0
        return .F.
    end
    statProc()
  until i == 0
  println( EOP )
return .T.

funcDef dcgnPrintDoc with onPages:LU local arr, i, copies := 1, j
  default onPages to .F.
  arr := dcgnDocGet()
  if !getUp( 10, 25, {{ "������⢮ �����: ", block( copies ), "@99" }});
   then return .F.
  statStartUp( "���� �����..." )
  for j := 1 to copies
    if isArray( arr )
      for i := 1 to len( arr )
        statProc()
        printLongStr( arr, onPages )
      next
    else
      statProc()
      printLongStr( arr, onPages )
    endIf
  next
  statExit()
return .T.

funcDef dcgnPrint with onPages:LU, outputDev:CU
return dcgnPrintFile( DCGN_TEMP_FILE, onPages, outputDev )

funcDef dcgnPrintFile with  fileName:C, onPages:LU, outputDev:CU;
                      local copies := 1, i, s, nHandle, fSize, bError, oE
  default onPages to .F.
  if !getUp( 10, 25, {{ "������⢮ �����: ", block( copies ), "@99" }});
   then return .F.
  if !empty( outputDev )
    for i := 1 to copies do filePrint( fileName, outputDev )
    return .T.
  endIf
  nHandle := fopen( fileName )
  fSize   := fileSize( nHandle )
  if nHandle < 1 then return .F.
  statStartUp( "���� �����..." )
  for i := 1 to copies
    fseek( nHandle, 0, FS_SET )
    if dialog( "��⠢�� ��࠭���...",;
               { "�த������ �����",;
                "�������� �����" }) != 1
      fclose( nHandle )
      statExit()
      return .T.
    endIf
    do while freadln( nHandle, @s )
      if onPages .and. !empty( at( EOP, s ))
        println( s )
        if FILE_POS( nHandle ) >= fSize .or.;
           dialog( "��⠢�� ��࠭���...",;
                   { "�த������ �����",;
                     "�������� �����" }) != 1
          fclose( nHandle )
          statExit()
          return .T.
        endIf
        if !freadln( nHandle, @s )
          fclose( nHandle )
          statExit()
          return .T.
        endIf
      endIf
      bError := errorBlock({| oE | prnError( oE )})
      begin sequence
        println( s )
      recover using oE
        errorBlock( bError )
        fclose( nHandle )
        statExit()
        return .F.
      endSequence
      errorBlock( bError )
      statProc()
    endDo
  next
  fclose( nHandle )
  statExit()
return .T.

funcDef dcgnGo local copies := 1
  if !getUp( 10, 25, {{ "������⢮ �����: ", block( copies ), "@99" }});
   then return .F.
return doPrint( memoRead( DCGN_TEMP_FILE ), copies )

funcDef dcgnOpenName with cName:C
return fcreate( cName, FC_NORMAL )

funcDef dcgnOpen
  dcgnHandle := fcreate( DCGN_TEMP_FILE, FC_NORMAL )
  if dcgnHandle < 1 then dcgnHandle := 0
return dcgnHandle != 0

funcDef dcgnPut
  if empty( dcgnHandle )
    messageB( "dcgnPut() - �맮� �� ���樠����樨!" )
    return .F.
  endIf
return dcgnPutHandle( dcgnHandle )

funcDef dcgnPutHandle with nHandle:N local aDoc, i
  aDoc := dcgnDocGet()
  if isARRAY( aDoc )
    for i := 1 to len( aDoc ) do fwrite( nHandle, aDoc[ i ])
  else
    fwrite( nHandle, aDoc )
  endIf
return .T.

funcDef dcgnCloseHandle with nHandle:N
return fclose( nHandle )

funcDef dcgnClose
  if empty( dcgnHandle )
    messageB( "dcgnClose() - �맮� �� ���樠����樨!" )
    return .F.
  endIf
  fclose( dcgnHandle )
  dcgnHandle := 0
return .T.

funcdef dcgnEraseName with cFile:C
return ferase( cFile ) == 0

funcDef dcgnErase
return ferase( DCGN_TEMP_FILE ) == 0

funcDef dcgnShowFile with cFile:C
return viewFile( cFile )

funcDef dcgnShow
return dcgnShowFile( DCGN_TEMP_FILE )

funcDef dcgnExtract with tplName:C local rV, cS
  cS := extractStrOpen( tplName )
  if empty( cS )
    return ""
  endIf
  rV := getTmpName( ".\" ) + ".TPL"
  memoWrit( rV, cS )
return rV

funcDef viewFile with fName:C
  text_prn( fName )
return nil
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
