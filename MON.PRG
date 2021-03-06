* =========================================================================
*   
*     �ணࠬ�� : MON.EXE
*     ����      : MON.PRG ( ROOT )
*     ����     : ���⫨� �.�.
*
*     ���ᠭ��  : �᭮���� �ணࠬ��
*
* -------------------------------------------------------------------------
#include "clippext.ch"
#include "common.ch"

#include "bases.ch"

#define MON_VERSION_NUMBER	"30"
//#define MON_RELEASE_NUMBER	"0.2" 
#define MON_RELEASE_NUMBER	"0.14"
#define MON_VERSION_DATE 	"04.08.2005"
#define MON_VERSION 		MON_VERSION_NUMBER + "." + MON_RELEASE_NUMBER
#define MON_README_FILE		"README." + MON_VERSION_NUMBER
#define MON_MESSAGE_FILE	"INFO_MES.TXT"
#define MON_EXE_NAME		"MON.EXE"
#define MON_LOG_FILE		"MON_USR.LOG"

#define CM_NORMAL 0
#define CM_HELP   5

static cScreen, runMode, paramDate,;
	lOdClose // Add by TMV  15-01-2002

request DBFNTX

procDef main with  cArg1:CU, cArg2:CU, cArg3:CU, cArg4:CU, cArg5:CU,;
                   cArg6:CU, cArg7:CU, cArg8:CU;
             local aItems, aTitle, localKey, pos, cnt, bStartup, dDate

// ��� ������������ ������ ��������� ��᫥ ��ࠢ�� ������ � 䨫���

  prepSettings()
  cScreen := saveScreen() 
  runMode := iParseCmdLine( cArg1, cArg2, cArg3, cArg4, cArg5, cArg6, cArg7, cArg8 )
  do case
    case runMode == CM_HELP
      dispHelp()
      quit
    case runMode == CM_NORMAL
      if !initialize() .and. !pbt() then startupMsg()
  endCase 
  cnt := confCnt()
  if cnt = 0 then cnt := 50
  dosIdle()
  noAskIndex( .T.)
  getDonExeName( ".\printpp.exe")
  getDonName( ".\kwitpp.dat")
  pcPath( confPathExe())
  setKey( 281, {|| strStw()})
  errPath( confPathOD())
  monInit()
  moveCredDoc()
  if !pbt() .and. !fast()
    readmeFile( MON_README_FILE  )
    readmeFile( MON_MESSAGE_FILE )
  endIf  
  logFileName( MON_LOG_FILE )
  aItems := {                               ;
		  { " ������� ", "����� � �����⠬�",;
          {{ " ������� ", .T. , {|| cliView()}}}},;
        { " ����� ", "����� ����묨 � 䨫�����", ;
          {{ "��।�� ������ �� 䨫���", .T., {|| odClose( setCurDate(), cScreen)}},;
           { "�ਥ� ������ � 䨫����",    .T., {|| odOpen(  setCurDate())}};
			 }},;
        { " ����ன�� ", "��������� ��ࠬ��஢ ��⥬�",;
          {{ " ���䨣���� ",            {|| .T. }, {|| confChange() }}}, ;
           };
		  }
  aTitle :=  { "", " � � � � � � � � � � � �   � � � � � � � � � �   � � � ",;
               "", '(c) 2002, ��� "������"',                   ;
               "", "����� " + MON_VERSION + " �� " + MON_VERSION_DATE,  "" }
  if pbt() .or. initialize()
    bStartup := {|| denisPBT()}
  else
    bStartup := {|| startupCode()}
  endIf       
  if dateParam()
    dDate := paramDate
  else
    dDate := nil
  endIf
  if workAlone()        // Add by TMV  1-11-2001
  	aadd(aItems[2,3], {"�ਥ� ������ �� ��", .T., {|| impAccFromFond()}})
  endif
  mainProg( aItems, aTitle, bStartup, {|| exitCode()},;
                                      {|| recoverCode()},, userParam(), pwdParam(), dDate )
  if pbt() then restScreen(,,,, cScreen )
return

procDef startupMsg
  ?
  ? '������ ���㦨����� 䨧��᪨� ��� ��� "�����". ����� ' + MON_VERSION +;
		' �� ' + MON_VERSION_DATE
  ? replicate( chr(196), 79 )
  ? version() + " ������: CHAR: " + allTrim( str( Memory( 0 ))) +;
                         " MAX: " + allTrim( str( Memory( 1 ))) +;
                         " RUN: " + allTrim( str( Memory( 2 )))
  ?
  ?
return

funcDef dispHelp()
  cls
  startupMsg()
  ? "�ᯮ�짮�����:"
  ?
  ? "     MON [</|-><�������>] [</|-><��ࠬ���>=<���祭��>]"
  ?
  ? "�������:"
  ? "     H[elp ]   - �뤠� ������ �ࠢ�筮� ���ଠ樨"
  ?
  ? '��ࠬ����:'
  ? "     I[nit]    - ����� ⮫쪮 ��� ���樠����樨 �����"
  ? "     N[omodem] - ����� ��� �஢�ન 䠪� ������ ���"
  ? "     F[ast]    - ����� ��� �஢�ન 䠩��� ���� ������"
  ? "     P[BT]     - ����� ��� �஢�ન 䠪� ������ ���"
  ? "                 � ��� �஢�ન 䠩��� ���� ������"
  ? "     S         - ����� �⠭樨 ���� ���㬥�⮢ ��� �믫�� �� ����"
  ? '     D         - ��� ࠡ��� � �ଠ� DD/MM/[YY]YY'
  ? '     U         - ��� ���짮��⥫�'
  ? '     W         - ��஫� ���짮��⥫�'
  ?
return nil

static funcDef parseArg with cArg:CU local cSign, cValue
  public inStat

  default cArg to ""
  cArg   := allTrim( cArg )
  cSign  := upper( subStr( cArg, 2, 1 ))
  cValue := allTrim( subStr( cArg, 4, 255 ))
  if subStr( cArg, 1, 3 ) == "PBT" then cSign := "P"
  if cSign == "S"
     if subStr( cArg, 3, 1 ) == ":"
        cValue := subStr( cArg, 4, 2 )
     else
        cValue := subStr( cArg, 3, 2 )
     endIf
     if len( cValue ) > 2
        errOK( "������ ������ ����� �⠭樨 ����!" )
        return .F.
     endIf
     if !empty( cValue ) then cValue := padL( cValue, 2, "0" )
     confConfName( cValue )
     thisStation( cValue )
     inStat := cValue
  endIf
  if cSign == "P"
     confPBTKassa( cValue )
     pbt( .T. )
  endIf
  if cSign == "I" then initialize( .T. )
  if cSign == "N" then silent( .T. )
  if cSign == "F" then fast( .T. )
  if cSign == "D"
     paramDate := ctod( cValue )
     dateParam( .T. )
  endIf
  if cSign == "U"
     userParam( allTrim( cValue ))
     userName( userParam())
  endIf
  if cSign == "W"
     pwdParam( allTrim( cValue ))
     userPassword( pwdParam())
  endIf
return .T.

func iParseCmdLine( cArg1, cArg2, cArg3, cArg4, cArg5, cArg6, cArg7, cArg8 )
local cSign, cDigit
  default cArg1 to ""
  default cArg2 to ""
  default cArg3 to ""
  default cArg4 to ""
  default cArg5 to ""
  default cArg6 to ""
  default cArg7 to ""
  default cArg8 to ""
  parseArg( cArg1 )
  parseArg( cArg2 )
  parseArg( cArg3 )
  parseArg( cArg4 )
  parseArg( cArg5 )
  parseArg( cArg6 )
  parseArg( cArg7 )
  parseArg( cArg8 )
  cArg1 := subStr( allTrim( Upper( cArg1 )), 2, 10 )
  do case
    case cArg1 == "?" .or. cArg1 == "H" .or. cArg1 == "HELP"
      return CM_HELP
    otherwise
      return CM_NORMAL
  endCase
return CM_NORMAL

procDef prepSettings
  dbSetDriver( "DBFNTX")
  readInsert( .T.)
  set century    on
  set confirm    on
  set date       to German
  set epoch      to 1971
  set deleted    off
  set scoreboard off
  set unique     off
  set wrap       on
return

procDef startupCode ;
        local old := setFastOpen( .F.)
  if initialize()
    messageStartup( '���樠������ ����� ���㦨����� �����⮢ ��� "�����"...' )
  endIf
  if !fast() then basesCheck()
  if !dateParam()
    getUp( 0, 0, {{ "������ ���� ࠡ��� ", funBlock( setCurDate )}})
  endIf
  if !silent() .and. !odOpen( setCurDate())
    errLog( "����� ࠡ���� - ���� �� �����!")
    initialize(.T.)
  endif
  isDebug( confPrintTo() == 2 )
  setFastOpen( old)
  tempData()
  fillProvs()
  if initialize()
    exitCode()
    messageExit()
    restScreen(,,,, cScreen )
    quit
  endIf
return

procDef exitCode local i:=2

   basesAllClose()

return

procDef recoverCode
   basesAllClose()
   prepSettings()
   monInit()
   tempData()
return

procDef denisPBT
  if initialize()
    startupCode()
  else
    if !dateParam() .and. modOpen()
      MODEM ->( dbSetOrder( 2))
      MODEM ->( dbGoBottom())
      setCurDate( MODEM ->DATE)
      modClose()
    endIf
    tempData()
    fillProvs()
  endIf
return 


* NEW OBJECT FILE
//
// �᫨ ���� 䠩� � ��������ﬨ, � ������
//
funcDef readMeFile with fileName:C local i, file2
//
// �᫨ ���� 䠩� � ��������ﬨ
//
if file( ".\" + fileName)
   //
   // �p�ᬮ�p�� 䠩� � ������ﬨ
   //
   editMemo( memoRead( ".\" + fileName), .F.)
   //
   // p�ᯥ�⠥�
   //
   if yesNO( "������ 䠩� ?")
      if CLPrintFile( ".\" + fileName)
         file2 := "_" + subStr( fileName, 2, len( fileName))
         if file( file2) then fErase( file2)
         if fRename( ".\" + fileName, ".\" + file2)  == - 1
            if dialog( { "���������� ��p���������� 䠩� " + fileName,;
               "��� �訡�� : " + str( fError())                     ,;
               "�������� ������ ��� ��� � ᮮ��� �"          ,;
               "��㦡� ᮯ஢�������"},{ "������� 䠩�", "�� 㤠����" }) = 1
               fErase( ".\" + fileName)
            endif 
         endif
      endif
   else
      cls; quit
   endif
endif

return NIL

// func clPrintFile; return 0

* NEW OBJECT FILE
funcDef od_Gather local aArr := Array( FCount()), i
for i := 1 to Len( aArr) do aArr[ i] := FieldGet( i)
return aArr

* NEW OBJECT FILE
funcDef od_Scatter with aArr:A local i, nLen := min( Len( aArr), FCount())
for i := 1 to nLen do if !isNIL( aArr[ i]) then FieldPut( i, aArr[ i])
return nil

funcDef netTime
return 0

// mxPrintFile
//
// ��ᯥ�⪠ 䠩��. ��ࠬ��� xFile - ��� ��� 䠩��, ��� ��� 奭��.
//
Function CLPrintFile( xFile )
   LOCAL cBuf := space( 1024 )
   LOCAL nRead
   LOCAL rV := .F.

   if CLPrnReady()
      if ValType( xFile ) == "C"
         if ( xFile := FOpen( xFile ) ) == -1
            errLog( { "�訡�� ������ 䠩��", alltrim( xFile ) }   )
            return rV
        endif
      endif

      set device to print
      set printer to
      SetPrc( 0, 0 )

      while .t.
         if ( nRead := FRead( xFile, @cBuf, 1024 ) ) > 0
            DevOut( left( cBuf, nRead ) )
         endif

         if nRead != 1024    ;   exit    ;   endif
      enddo
      FClose( xFile)
      set device to screen
      rV := .T.

   endif
return rV

// mxPrnReady
//
// �஢�ઠ ��⮢���� �ਭ��
//
Function CLPrnReady( cFile )
    LOCAL lReady := .t.
    LOCAL nRetry := 5
    LOCAL nRet
    LOCAL cColor := SetColor()
    MEMVAR  GetList

    if empty( cFile )   ; cFile := ""   ;   endif
    cFile := PadR( cFile, 50 )

	if !IsPrinter()

		while nRetry > 0 .and. !IsPrinter() .and. lReady

            nRet := dialog( { "�訡�� �ਭ��","�ਭ�� �� ��⮢","" },;
              { "  �����  ", "  ��室  " }  )

            lReady := .f.

            do case
                case nRet == 1
                    lReady := .t.
                otherwise

                endcase
			nRetry--

		enddo

        SetColor( cColor )

        if nRet == 2
            set printer to ( cFile )
            return .t.

        else
            set printer to

        endif

		if lReady .and. !( lReady := IsPrinter() )

			errLog( { "        �訡�� �p���p�       "      ,;
                                 "       ����� �⬥�����      ", ""  ,;
                                 "        ������� �ਭ��      " }  )

		endif

	endif

return lReady

func isNewPlan
return confIsNewPlan()

func tempfilOtd2()
return confOtdCode()

autofunction setValue thisStation init ""


funcDef tempData
if !file( tempInfo()[ 1])
   if confXOpen()
      if baseCreate( tempInfo())
         if baseMXOpen( tempInfo())
            tempGetComment( "CREDDOC",  nStr( CONFIG ->credDoc  ))
            tempGetComment( "ZABDOC",   nStr( CONFIG ->zabDoc   ))
            tempGetComment( "ZABPDOC",  nStr( CONFIG ->zabPDoc  ))
            tempGetComment( "APPDOC",   nStr( CONFIG ->appDoc   ))
            tempGetComment( "TRANSDOC", nStr( CONFIG ->transDoc ))
            baseMClose( tempInfo())
         endif
      endif
      confClose()
   endif
endif
return NIL

func fPasp;     return NIL
func fAdres;    return NIL
func ff_i_o;    return NIL
func infoTrust; return NIL
func _runExe;   return NIL

function printtemp(fname,lstil,lp1,lp2)

local fh := -1 ,flen, s, ln:=1, li:=1, lk:=57, i:=0,;
      le, lpb:=.F.

begin sequence
set printer on

if lp1 != NIL .and. lp2 != NIL
   lpb := .T.
endif

do while !isprinter()
   if !yesno("�ਭ�� �� ��⮢.������� ?") then break
enddo
if (fh := fopen(fname,66)) <= 0
    ok({"�� ���� ������ 䠩� "+ fname ,;
        "�訡�� DOS "+str(ferror(),4)})
    break
endif
flen := fseek(fh,0,2)
fseek(fh,0,0)
set console off
?chr(27)
if lstil != NIL .and. lstil = 1
   ?chr(18)
else
   ?chr(15)                && �������
endif
?
if lpb then fseek(fh,lp1,0)
do while fseek(fh,0,1) < flen
   if lpb .and. (fseek(fh,0,1) >= lp2) then exit
   s:=freadln(fh)
   ?? s
enddo
?
?
?chr(27)
?chr(18)
end sequence
if fh > 0 then fclose(fh)
set printer off
set console on

return 0

function viewtemps(lname, lfl, n)
  text_prn( lName )
return 0

funcDef moveCredDoc local pathOD, confValue, tempValue, hHandle
   if empty( tempGetComment( "NEW_CRED"))
      if ( hHandle := flagOpen( confPathOD())) <> - 1
         if empty( tempGetComment( "NEW_CRED"))
            if tempOpen()
               tempValue := tmpGet( "CREDDOC")
               tempValue := nStr( val( tempValue) + 15000)
               tmpSet( "CREDDOC", tempValue)
               tempGetComment( "NEW_CRED", "YES")
               tempClose()
            endif
         endif
         flagClose( hHandle)
      endif
   endif
return NIL

funcDef monVerDate local cVersion, cFileName, aFileDesc
  cVersion  := MON_VERSION_DATE
  cFileName := MON_EXE_NAME
  if file( cFileName )  
    aFileDesc := getMaskInfo( cFileName )
    if len( aFileDesc ) >= 3 then cVersion := myDtoC( aFileDesc[ 3 ])
  endIf
return cVersion

autofunction setValue silent     init .F.
autofunction setValue pbt        init .F.
autofunction setValue initialize init .F.
autofunction setValue fast       init .F.

autofunction setValue dateParam  init .F.
autofunction setValue userParam  init ""
autofunction setValue pwdParam   init ""

autofunction setValue monVerNum init MON_VERSION_NUMBER
autofunction setValue tempInfo  init TEMP_SPEC
autofunction setValue nodesInfo init NODES_SPEC
autofunction setValue infoInfo  init INFO_SPEC

// ��� ������������ ������ ��������� ��᫥ ��ࠢ�� ������ � 䨫���
autofunction setValue chkOdClose init .F.  // Add by TMV  15-01-2002
//
funcDef getFName with fileName:C local name := "", pos1 := 0, pos2 := 0
   name := fileName
   pos1 := rat( "\", name)       // ������ ��᫥����� ᨬ���� "\"
   pos2 := rat( ".", name)       // ������ ��᫥����� ᨬ���� "."
   //
   // ������ �ਬ��� :
   //
   //    ..\BASES\CONFIG.DBF                 ( �窨 ��। ᨬ����� "\" )
   //    ..\BASES\CONFIG                     ( �窠 ����, � ���७�� ��� )
   //    G:\USER.DIR\CLIRING\FL_D\ARHKLI.DBF ( ���७�� � ����� ��४�ਨ )
   //
   if pos2 > pos1                      // ���砫� ��०�� ���७��
      name := left( name, pos2 - 1)
   endIf
   if pos1 > 0                         // ������ ��०�� ����
      name := substr( name, pos1 + 1)
   endIf
return allTrim( name)
