*=============================================================================
*     
*                  �ணࠬ��  : ������ �⤥�����
*                  ����       : basemodm.prg
*                  ����      : ���⫨� �.�.
*
*                  �����祭�� : ����� ���ଠ樥� � 䨫�����
*
*-----------------------------------------------------------------------------
//11-08-2000 �� �� �⮩ ��� � ⥪�� - �஢�ન ��� �孮����� �ਥ��
//           ����窨 �� ��.���

*
* 20-01-2003 TMV
* ������� ����������� ������ ��१���� ⠡��� WISC �� �ਥ�� ������
*  �� 䨫����. �-�� modReceive() � modCopyFile()
*
* 20.08.2005 OVD
* cleaTexBas() - ���⪠ �孮�����᪨� ���
*

#include "clippExt.ch"
#include "mon.ch"
#include "common.ch"
#include "inkey.ch"

#define F_FIRST_LETTER    1
#define F_OTD             2
#define F_DATE            3
#define F_FLOW            4

#define inBoxLog          ".\inbox.log"

#define DAY_OPEN              "010"
#define DAY_OPEN_AGAIN        "011"

#define DAY_CLOSE             "020"
#define DAY_REPEAT            "030"

#define DAY_TRY_OPEN_CLOSED   "099"

* NEW OBJECT FILE

//
// ����஢���� ����� 䠩�� � ����ᨬ��� �� ����
//
funcDef codeName with arr:A ;
        local firstLetter, theOTD, dDate, theFlow   ,;
              fileName, aMonths, aDays, theDay      ,;
              theMonth, theYear, i

firstLetter := arr[ F_FIRST_LETTER]
theOTD      := arr[ F_OTD]
dDate       := arr[ F_DATE]
theFlow     := arr[ F_FLOW]

theDay   := Day(   dDate)
theMonth := Month( dDate)
theYear  := Year(  dDate)

aMonths  := { "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c" }

aDays    := { }
for i := 65 to  90 do aAdd( aDays, Chr( i))
// for i := 97 to 101 do aAdd( aDays, Chr( i))
for i := 48 to 53 do aAdd( aDays, Chr( i))

theYear := right( nStr( theYear), 1)

fileName := ".\" + firstLetter + nStr( theFlow) + theOTD + aDays[ theDay] + ;
            aMonths[ theMonth]  + theYear + ".DBF"

return fileName


funcDef decodeKrupDate with str:C ;
  local curD, aDays, aMonths, D, M, Y, ptr, i

  aDays    := { }
  aMonths := { "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c" }
  for i := 49 to 57 do aAdd( aDays, Chr( i))
  for i := 65 to 90 do aAdd( aDays, Chr( i))
  D    := left( str, 1); M := subStr( str, 2, 1); Y := subStr( str, 3, 1)
  D    := nStr( aScan( aDays,   {|x| upper( x) == upper( D )}))
  M    := nStr( aScan( aMonths, {|x| upper( x) == upper( M)}))
  // Y    := nStr( aScan( aMonths, {|x| upper( x) == upper( Y)}))
  Y := val( Y)
  Y    := nStr( 2000 + Y)
  curD := CtoD( D + "." + M + "." + Y)

return curD

funcDef enCodeKrupDate with curD:D ;
  local str, aDays, aMonths, D, M, Y, ptr, i
  if empty( curD) then return ""
  aDays    := { }
  aMonths := { "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c" }
  for i := 49 to 57 do aAdd( aDays, Chr( i))
  for i := 65 to 90 do aAdd( aDays, Chr( i))
  D    := Day( curD); M := Month( curD); Y := Year( curD) - 2000
  str  := aDays[ D] + aMonths[ M] + nStr( Y)
return str


//
// �� ����� 䠩�� ����⠭���� ���� ��� �ନ஢���� �⤥����� � ��⮪
//
funcDef decodeFileName with fileName:C ;
        local firstLetter, theOTD, dDate, theFlow ,;
              aMonths, aDays, theDay, theMonth    ,;
              theYear, i, arr
   
fileName := getFName( fileName)

firstLetter := left( fileName, 1)
theOTD      := subStr( fileName, 3, 3)

theDay      := subStr( fileName, 6, 1)
theMonth    := subStr( fileName, 7, 1)
theYear     := subStr( fileName, 8, 1)
theFlow     := subStr( fileName, 2, 1)

aMonths     := { "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c" }

aDays := { }
// for i := 65 to  90 do aAdd( aDays, Chr( i))
// for i := 97 to 101 do aAdd( aDays, Chr( i))
for i := 49 to 57 do aAdd( aDays, Chr( i))
for i := 65 to 90 do aAdd( aDays, Chr( i))


theDay   := nStr( aScan( aDays,   {|x| upper( x) == upper( theDay  )}))
theMonth := nStr( aScan( aMonths, {|x| upper( x) == upper( theMonth)}))
theYear  := nStr( 1990 + val( theYear))

dDate    := CtoD( theDay + "." + theMonth + "." + theYear)

arr := { firstLetter, theOTD, dDate, theFlow}

return arr

* NEW OBJECT FILE

//
// ���ᠭ�� ���� ࠡ��� ����
//
func modInfo
static dbInfo := { ".\modem.dbf", "MODEM",;
                   {{ "dateWork", "D",  8, 0}  ,;
                    { "typeOf",   "C",  3, 0}  ,;
                    { "date",     "D",  8, 0}  ,;
                    { "time",     "C",  8, 0}  ,;
                    { "owner",    "C",  8, 0}  ,;
                    { "flow",     "N",  1, 0}} ,;
                   {{, "typeOf + DtoS( dateWork)"} ,;
                    {, "DtoS( dateWork) + typeOf"}}, 0}
return dbInfo

funcDef modPath with newPath:CU
return basePath( modInfo(), newPath)

funcDef modOpen
return baseMOpen( modInfo())

funcDef modXOpen
return baseMXOpen( modInfo())


funcDef modClose
return baseMClose( modInfo())

//
// ������ ����樨 � ��⮪��
//
funcDef modWrite with theDate:D, theType:C local rV

if ( rV := MODEM ->( mAppend() ))

   MODEM ->dateWork := theDate
   MODEM ->typeOf   := theType
   MODEM ->date     := setCurDate()
   MODEM ->time     := time()
   MODEM ->owner    := userName() //nNetWhoAmI()

   MODEM ->( netUnLock())
endif

return rV


funcDef modSendProt with protInfo:A, fileName:C ;
   local i, info, s := select(), curA, key, crc, rV, EncodedDate, fileInfo

   key  := getCrcKey()
   info := aClone( protInfo)
   info[ 1] := fileName
   info[ 2] := "PROT"
   info[ 4] := {}
   curA     := protInfo[ 2]
   crc      := 0
   EncodedDate := Encode( setCurDate(), key)
   baseCreate( info)
   /*
   if baseMXOpen( protInfo)
      operStartUp( "�������� ����� ������", i := 1, ( curA) ->( lastRec()))
      while ( curA) ->( !eof())
        if ( setCurDate() - ( curA) ->wDate) > 30
           ( curA) ->( mDelete())
        else
           skip 1 alias ( curA)
        endif
        operProc( i++)
      enddo
      operExit()
      baseMClose( protInfo)
   endif       
   */
   if basesMOpen( { protInfo, info})
      rV  := CheckCrc( protInfo)
      crc := 0
      operStartUp( "��ࠢ�� ��⮪����", i := 1, ( curA) ->( lastRec()))
      while ( curA) ->( !Eof())           
        if ( curA) ->wDate == EncodedDate
           PROT ->( dbAppend())
           if !netErr()
              PROT ->( od_Scatter( ( curA) ->( od_Gather())))
              crc += PROT ->( CrcRec())
              PROT ->( netUnLock())
           endif
        endif
        skip 1 alias ( curA)
        operProc( i++)
      enddo
      PROT ->( dbAppend())
      PROT ->userName := Encode( userName(),   key)
      PROT ->wDate    := Encode( setCurDate(), key)
      PROT ->wTime    := Encode( time(),       key)       
      if PROT ->( fieldPos( "fileName")) > 0
         PROT ->fileName := Encode( "CRC:" + nStr( rV), key)
      else
         PROT ->str   := Encode( "CRC:" + nStr( rV), key)
         PROT ->stack := Encode( "CRC:" + nStr( rV), key)
      endif
      crc += PROT ->( CrcRec())
      PROT ->( netUnLock())
      operExit()
      basesMClose({ protInfo, info})
      writeCrc( info, crc, key)
   endif
   select (S)
return .T.

//
// ���뫪� ��� �� 䨫���
//
funcDef modSend with theDate:D ;
                local rV, ODPath, modemPath, fileName,;
                      arr, flow, i, theOTD, tmp, str ,;
                      arr2 := { }, tmpDate, pCode

messageStartUp( "��ࠢ�� ������ �� 䨫���. ��������� ")
ODPath    := confPathOD()
modemPath := confPathOutBox()
theOTD    := padL( confOTDNumb(), 3, "0")
pCode     := setUpValue( "pcod")
theOtd    := pCode + right( theOtd, 2)

//
// �஢�ઠ ��। ��ࠢ���
//
tmpDate := theDate

if !infoCheck()
   messageExit( 0 )
   repeat
     if yesNo({ "���ଠ樮���� �ࠢ�� ᮤ�ন� ������������ ����!",;
                "��३� � ०�� ���������� �ࠢ��?" }) 
       infoEdit()
     else
       errLog({ "���ଠ樮���� �ࠢ�� ᮤ�ন� ������������ ����.",;
                "��ࠢ�� ������ �⬥����." })
       return .F.
     endIf
   until infoCheck()
endif

if !modCheck( @tmpDate)
   messageExit( 0)
   errLog( { "��। �����⨥� ��� �㦭� 㤠���� ��� ��ࠡ���� ��" ,;
            "�थ� ���������� �� ���� " + DtoC( tmpDate) })
   return .F.
endif
if !sumsCheck()
   messageExit( 0)
   errLog( "�⪠� �� ������ ��-�� �訡�� � SUMS", .F.)
   return .F.
endif

if !accCheck()
   messageExit( 0)
   errLog( "�⪠� �� ������ ��-�� �訡�� � ACCOUNTS", .F.)
   return .F.
endif


rV        := .T.
//
// ��।���� ����� ��⮪�
//
// flow := modFlow( theDate)
flow := 0
if flow = -1

   errLog( "���� " + DtoC( theDate) + " �� ����� !")
   return .F.
endif

// if i <= 9
if .T.
   // flow := i
   //
   // ����뫠�� �����⮢  // Mike !!!!!!!!!!!!!!!!!!!!!!! 
   // ������� 䫠� ����뫠�� ���� ���������
   // ��� ⮫쪮 ��������� ����� !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   arr := {{ "c", cliInfo()[   1], .T.} ,;  // �������
           { "a", accInfo()[   1], .T.} ,;  // ���
           { "s", sumInfo()[   1], .F.} ,;  // �஢����
           { "k", kasInfo()[   1], .F.} ,;  // ���⮪ �� ����
           { "t", trustInfo()[ 1], .T.} ,;  // ����७����
           { "p", provInfo()[  1], .F.} ,;  // ��㣨
           { "r", assInfo()[   1], .F.} ,;  // ����⨬���
           { "n", contInfo()[  1], .F.} ,;  // ����ࠪ�
           { "v", prov2Info()[ 1], .F.} ,;  // ���⠢騪�
           { "i", infoInfo()[  1], .F.} ,;  // ���ଠ樮��� �ࠢ��
           { "g", logInfo()[   1], .F.} ,;  // ��⮪��� �ਥ�� ������
           { "e", errInfo()[   1], .F.} }   // ��⮪��� �訡���� ᮮ�饭��
   operStartUp( "��ࠢ�� ������ �� 䨫���", 1, len( arr))
   logWrite( getMaskInfo( newFPath( "printz.dbf", confPathOD())),  90 )
   logWrite( getMaskInfo( newFPath( "printz.dbt", confPathOD())),  91 )
   logWrite( logDummy( "VERSION:" + monVerNum()), 98 )
   logWrite( getMaskInfo( newFPath( "mon.exe",    confPathExe())), 99 )
 
   for i := 1 to len( arr)
       fileName := codeName( { arr[ i, 1], theOTD, theDate, flow})
       fileName := newFPath( fileName, modemPath)
       if .not. arr[i, 3] // Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          do case
             case arr[ i, 1] = "g" do modSendProt( logInfo(), fileName)
             case arr[ i, 1] = "e" do modSendProt( errInfo(), fileName)
             otherwise             
                if !file( arr[ i, 2])
                   errLog( { "�� ��।�� 䠩��� �� ������ 䠩� " + arr[ i, 2],;
                             "", "������� �� 䨫���!"})
                   close all; setColor( "w/n"); cls; quit
                endif   
                COPY FILE ( arr[ i, 2]) TO ( fileName)
                if i == 3 then modSendSums( fileName)
             endcase
       else
          // Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          do case
             case  arr[i, 1] == "c" do MikeBaseSend( cliInfo(), fileName)
             case  arr[i, 1] == "a" do MikeBaseSend( accInfo(), fileName)
             case  arr[i, 1] == "t" do MikeBaseSend( trustInfo(), fileName)
          endcase
          // Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       endif
       if !file( fileName)
          errLog( "�訡�� �� ����஢����/�ନ஢���� 䠩�� " + arr[ i, 2]) //Mike !!!!!!!!!
          aAdd( arr2, fileName)
          rV := .F.
          exit
       else
          tmp  := strTran( arr[ i, 2], ".DBF", ".DBT")
          fileName := strTran( fileName, ".DBF", ".DBT")
          if file( tmp)
             COPY FILE ( tmp) TO ( fileName)
             if !file( fileName)
                errLog( "�訡�� �� ����஢���� 䠩�� " + arr[ i, 2])
                aAdd( arr2, fileName)
                rV := .F.
                exit
             endif
          endif
       endif
       operProc( i)
   next                              
   operExit()
   //
   // 㤠�塞 䠩��
   //
   for i := 1 to len( arr2)
       if file( arr2[ i]) then fErase( arr2[ i])
   next
   fileName := codeName( { "w", theOTD, theDate, flow})
   if rV .AND. len( arr2) == 0 .AND. sendWisc( fileName)
      fileName := newFExt( codeName( { "f", theOTD, theDate, flow}), "ZIP")
      str := "pkzip.exe " + newFPath( fileName,  confPathOutBox()) + ;
             " "          + newFPath( ".\*.db?", confPathOutBox()) + " >nul"
      runExe( str)
      runExe( newFPath( ".\send.bat", confPathExe()) + " " + padL( confOtdNumb(), 3, "0"))
   endif

////////////////  Add By TMV 24-09-2001  ////  Begin
//	if lCloseDay then SendToYouSelf()
////////////////  Add By TMV 24-09-2001  ////  End
else
  errLog( "� �祭�� ��� �� ����� ���� ����� 10 ᥠ�ᮢ �裡 !")
  rV := .F.
endif

messageExit( 0)
clear typeAHead
// keyboard chr( 0)

return rV

//
// 䠩� 㦥 ᪮��஢�� � OutBox
//
funcDef modSendSums with fileName:C local rV := .T., s := select(), i
  use ( fileName) alias TMP new
  operStartUp( "�����⮢�� �஢���� ", i := 1, TMP ->( lastRec()))
  while TMP ->( !eof())
    if TMP ->typeOf == "9100" then TMP ->( clearRec())
    skip 1 alias TMP
    operProc( i++)
  enddo
  operExit()
  close TMP
  select ( s)
return rV


//
// �஢�ઠ ��� ��। ��ࠢ���
//
funcDef modCheck with theDate:D local rV := .T., s := select()
//
// �஢�ਬ ���� �� �थ� ���������� ���������
//
if sumOpen()
   SUMS ->( dbSetOrder( 3))
   if SUMS ->( dbSeek( ORD_33_DATE, .T.))
      rV := SUMS ->typeOf <> ORD_33_DATE
      theDate := SUMS ->dDate
   else
      if SUMS ->( dbSeek( ORD_904_DATE, .T.))
         rV := SUMS ->typeOf <> ORD_904_DATE
         theDate := SUMS ->dDate
      endif
   endif
   sumClose()
endif
select ( S)

return rV


//
// ��।�� ������� �஬� ���譨� �믨᮪
//
static funcDef sendWisc with fileName:C local rV := .F., tmp, i

tmp     := aClone( wiscInfo())
tmp[ 1] := fileName
tmp[ 2] := "TMP"
tmp[ 4] := { }
basePath( tmp, confPathOutBox())
if baseCreate( tmp)
   if basesMOpen( { tmp, wiscInfo()})
      WISC ->( dbSetOrder( 2))
      WISC ->( dbGoTop()) 
      operStartUp( "��।�� ������� �����⮢", i := 1, WISC ->( lastRec()) )
      while WISC ->( !eof())
         if WISC ->type <> WS_HISTORY .AND. WISC ->type <> "2000"
            TMP ->( dbAppend())
            TMP ->( od_Scatter( WISC ->( od_Gather())  ))
            if !empty( TMP ->ID) .AND. Left (WISC->type, 1) == '9' then ;
               TMP ->ID := ""
         endif
         skip 1 alias WISC
         operProc( i++)
      enddo
      operExit()
      rV := .T.
      basesMClose( { tmp, wiscInfo() })
   endif
endif

return rV

funcDef inBoxLogWrite with Str:AC local i
   if valType( Str) = "C"
      logStr( logStr() + endl() + Str)
   else
      for i := 1 to len( Str) do logStr( logStr() + endl() + Str[ i])
   endif
return eFileWrite( Str, ".\inbox.log")
autofunction setValue logStr init ""

funcDef getMaskFile with Mask:C, errN:NU local arr, fileName := ""
   errN := 0
   arr  := Directory( Mask, "")
   do case
      case len( arr) = 0 do errN := -1  // ��� 䠩��
      case len( arr) > 1 do errN := -2  // ᫨誮� ����� 䠩���
       otherwise         do fileName := arr[ 1, 1]
   endcase
return fileName

funcDef getMaskInfo with Mask:C, errN:NU local arr
   errN := 0
   arr  := Directory( Mask, "")
   do case
      case len( arr) = 0 
           errN := -1
           arr := logDummy( getFName( Mask) + "." + getFExt( Mask))
      case len( arr) > 1 do errN := -2
      otherwise          do arr  := arr[ 1]
   endcase
return arr

//
// �ਥ� ������ � 䨫����
//
funcDef modReceive with theDate:D ;
   local rV, inBoxPath, basesPath, tempPath, monInfo, arrFiles, ;
         useMask, canReceive, errN, i, s, fileHandle, lShowLog, ;
         opened := Select( "TICKETS") > 0, rT:=.F.

rV       := .F.
lShowLog := .T.
inBoxPath := confPathInbox()
basesPath := confPathOD()
tempPath  := confPathTemp()
if ( fileHandle := flagOpen( basesPath)) <> - 1
   logStr( "")
   // runExe( newFPath( ".\receive.bat", confPathExe()))
   //
   // �஢�ਬ ����稥 䠩���
   //
	if workAlone()
		inBoxLogWrite(;
     		{ "",;
       	"������ ���⪮� �� " + DtoC( setCurDate()) + " : " + time(),;
       	"-----------------------------------------------------"})
		SendToYouSelf()
	endif
   inBoxLogWrite(;
     { "", "", "",;
       "�ਥ� ������ � 䨫���� " + DtoC( setCurDate()) + " : " + time(),;
       "-----------------------------------------------------"})
   if file( newFPath( "clients.dbf", inBoxPath))
      canReceive := .T.
      useMask    := .F.
   else
      getMaskFile( newFPath( "c0??????.dbf", inBoxPath), @errN)
      canReceive := errN = 0
      useMask    := .T.
   endif
                      
   // if file( newFPath( "clients.dbf", inBoxPath))
   if canReceive
      inBoxLogWrite( "���� �ਥ� 䠩���")
// Setup.dbf ��७�ᥭ �� 5 ������⮢ �����
// by TMV  20-01-2003
      arrFiles := {;
         {"clients.dbf"  , "c0", nil},;
         { "accounts.dbf", "a0", nil},;
         { "sump.dbf"    , "p0", nil},;
         { "sums.dbf"    , "s0", nil},;
         { "setUp.dbf"   , "e0", nil},;
         { "vid.dbf"     , "v0", .T.},;
         { "wisc.dbf"    , "w0", nil},;
         { "wisc.dbt"    , "w0", nil},;
         { "kassa.dbf"   , "k0", nil},;
         { "trust.dbf"   , "t0", .T.},;
         { "kr_tprou.dbf", "u0", nil},;
         { "provider.dbf", "j0", nil},;
         { "assort.dbf"  , "r0", nil},;
         { "prov2.dbf"   , "b0", nil},;
         { "contract.dbf", "n0", nil},;
         { "contract.dbt", "n0", nil},;
         { "district.dbf", "d0", .T.},;
         { "raion.kmn"   , ""  , .T.},;
         { "adv_home.pic", "a1", .T.},;
         { "adv_pens.pic", "a2", .T.},;
         { "spr_info.dbf", "g0", .T.},;
         { "oper.dbf"    , "o0", .T.},;
         { "nodes.dbf"   , "f0", .T.},;
         { "wiscR.dbf"   , "wr", .T.},;
         { "info.dbf"    , "if", .T.},;
         { "orginfo.dbf",  "o1", .T.}}  //11-08-2000

      rV := .T.
      if !file( logInfo()[ 1]) then baseCreate( logInfo())
      logWrite( logDummy( "OPEN"), 0)
      for i := 1 to len( arrFiles)
        if arrFiles[ i, 1] <> "wisc.dbt" ;
		  		.Or. isWiskOverWrite()  // Add by TMV  20-01-2003
           if !modCopyFile( arrFiles[ i], useMask .AND. !empty( arrFiles[i,2]),;
               basesPath, inboxPath, tempPath)
               rV := .F.
               exit
           endif
        endif
      next
      logWrite( logDummy( "CLOSE"), if( rV, 0, -1))
      if rV
         noAskIndex( .T.)
         monInfo := aClone( monInfo())
         aAdd( monInfo, prov2Info())
         aAdd( monInfo, provInfo() )
         aAdd( monInfo, vidInfo()  )
         aAdd( monInfo, infoInfo()  )
         aAdd( monInfo, ticInfo() )

//    ���⨬ ⠡���� �죮��� �஥����� ���㬥�⮢
         if opened then ticClose()
         if !opened then rT := ticXOpen()
         if opened .OR. rT
           zap
         endif
         if !opened then ticClose()
         ticIndex( )

         basesIndex( monInfo)
         noAskIndex( .F.)

// ���⪠ �孮�����᪨� ���
         if !cleaTexBas( logInfo(), errInfo() )
            inBoxLogWrite( "�訡�� �� ���⪥ �孮�����᪨� ���!")
            errLog( "�訡�� �� ���⪥ �孮�����᪨� ���!")
         endif
      else
         inBoxLogWrite( "�訡�� �� ����஢���� 䠩���!")
         errLog( "�訡�� �� ����஢���� 䠩���!")
      endif
   else
     inBoxLogWrite( "�� ������ 䠩� CLIENTS.DBF")
     errLog( "�� ��諨 䠩�� � 䨫���� !")
     lShowLog := .F.
   endif
   inBoxLogWrite( { "�ਥ� ������ " + if( rV, "", "�� ") + "�ᯥ譮 �����襭 � " + time(),;
                 "============================================"} )
   if lShowLog then EditMemo( logStr(), .F.)
   logStr( "")
   if rV
      OK( "�ਥ� ������ �ᯥ譮 �����襭!")
   else
      errLog( "����� �ਭ��� �� 㤠����!")
   endif
   flagClose( fileHandle)
endif
clear typeahead
keyboard chr( K_ENTER)
inkey()
return rV
//
//  ���⪠ �孮�����᪨� ���
//
funcDef cleaTexBas with logInfo:A, errInfo:A ;
    local rV := .F., rT := .F., openLog := Select( "modemlog") > 0, ;
    openErr := Select( "errlog") > 0, Ddate := setCurDate(), i, ;
    modA, errA, rV1 := .F., rV2 := .F.

    modA := logInfo[ 2]
    errA := errInfo[ 2]

    if openLog then logClose()
    if !openLog then rT := logXOpen()
    if openLog .OR. rT
       operStartUp( "���⪠ ����  MODEMLOG.DBF", i := 1, ( modA) ->( lastRec()))
       MODEMLOG ->( dbGoTop( ))
       do whil MODEMLOG ->( !Eof())
          if MODEMLOG ->wdate <= Ddate - 30
             if MODEMLOG ->( mRLock())
                Delete
                MODEMLOG ->( netUnLock())
             endif
          endif
          skip 1 alias MODEMLOG
          operProc( i++)
       enddo
       pack
       rV1 := .T.
       statExit()
    endif
    inBoxLogWrite( "��饭� ����  MODEMLOG.DBF")
    if !openLog then logClose()

    if openErr then errClose()
    if !openErr then rT := errXOpen()
    if openErr .OR. rT
       operStartUp( "���⪠ ����  ERRLOG.DBF", i := 1, ( errA) ->( lastRec()))
       ERRLOG ->( dbGoTop( ))
       do whil ERRLOG ->( !Eof())
          if ERRLOG ->wdate <= Ddate - 30
             if ERRLOG ->( mRLock())
                Delete
                ERRLOG ->( netUnLock())
             endif
          endif
          skip 1 alias ERRLOG
          operProc( i++)
       enddo
       pack
       rV2 := .T.
       statExit()
    endif
    inBoxLogWrite( "��饭� ����  ERRLOG.DBF")
    if !openErr then errClose()

    if file( ".\prot.txt"  )
      if Day( setCurDate()) == 18
         inBoxLogWrite( "������ ��⮪��  PROT.TXT!")
         erase ( ".\prot.txt")
      endif
    endif
    rV := rV1 .AND. rV2
return rV
//
// ��९��뢠�� 䠩�
//
static funcDef modCopyFile ;
   with arr:A, useMask:L, basesPath:C, inBoxPath:C, tempPath:C ;
   local rV := .F.,tempFile, basesFile, inBoxFile, Str,;
         fileName, canSkip, errN, MaskName, fileInfo, fError, useWisc := .F.

canSkip  := arr[ 3] = .T.
fileName := arr[ 1]
useWisc  := upper( fileName) == "WISC.DBF"
if useMask
   MaskName := arr[ 2] + "??????" + "." + getFExt( arr[ 1])
   useWisc  := Upper( arr[ 2]) == "W0"
   fileName := getMaskFile( newFPath( MaskName, inboxPath), @errN)
   if errN = -2 // ᫨誮� ����� 䠩���
      inboxLogWrite( ;
       Str := { "���誮� ����� 䠩��� � ��᪮� " + MaskName,;
         "������� �� 䨫���!" })
      errLog( Str)
      logWrite( logDummy( MaskName), errN)
      return .F.
   endif
   if errN = -1 // �� ������ 䠩�
      logWrite( logDummy( MaskName), errN)
      if canSkip 
         inBoxLogWrite( Str := "���� " + MaskName + " �� ������, �� ��� ����� �ய�����")
         errLog( Str, .F.)
         return .T.
      else
         errLog( Str := { "���� " + MaskName + " �� ������ ", "������� �� 䨫��� !!!"})
         inBoxLogWrite( Str)
         return .F.
      endif
   endif
endif

if useMask
   tempFile  := newFPath( arr[ 1], tempPath)
   basesFile := newFPath( arr[ 1], basesPath)
   inBoxFile := newFPath( fileName, inBoxPath)
else
  tempFile  := newFPath( fileName, tempPath)
  basesFile := newFPath( fileName, basesPath)
  inBoxFile := newFPath( fileName, inBoxPath)
endif

if file( tempFile) then fErase( tempFile)
if !file( inboxFile) .AND. canSkip
   inBoxLogWrite( "���� " + inBoxFile + " �� ������, �� ��� ����� �ய�����")
   logWrite( logDummy( fileName), -1)
   return .T.
endif
Str := "���� " + inBoxFile + " ᪮��஢�� "
fileInfo := getMaskInfo( inboxFile)
if !isWiskOverWrite() .And.;  // Add by TMV  20-01-2003
		useWisc
  	rV := recCopyWisc( fileName, inboxPath)
   return rV
endif
if if( file( basesFile), fRename( basesFile, tempFile) == 0, .T.)
   fError   := 0
   fileInfo := getMaskInfo( inboxFile)
   if fRename( inBoxFile, basesFile) == 0
      rV := .T.
   else
      errLog( Str := { "�訡�� ����஢���� 䠩�� " + inBoxFile,;
           "��� �訡�� : " + Str( fError := fError()), "", "���������� ������� �� 䨫���" })
   endif
   logWrite( fileInfo, fError)
else
   errLog( Str := { "�訡�� ����஢���� 䠩�� " + basesPath,;
      "��� �訡�� : " + Str( fError := fError()), "", "���������� ������� �� 䨫���" })
   logWrite( fileInfo, fError)
endif
inBoxLogWrite( Str)
return rV

static funcDef recNewWisc local i, dbInfo, rV := .F., name1, name2
dbInfo := aClone( wiscInfo())
dbInfo[ 1] := newFPath( "new_Wisc.DBF", confPathOD())
dbInfo[ 2] := "new_Wisc"
dbInfo[ 4] := {}

if baseCreate( dbInfo)
   if basesXOpen( { dbInfo, wiscInfo()})
      operStartUp( "�������� ����� ������", i := 1, WISC ->( lastRec()))
      WISC ->( netGoTop())
      while WISC ->( !eof())
        if WISC ->type == "9999"
           new_Wisc ->( dbAppend())
           new_Wisc ->schet  := WISC ->schet
           new_Wisc ->type   := WISC ->type
           new_Wisc ->buffer := WISC ->buffer
           // new_Wisc ->( dbSkip( 0))
        endif
        skip 1 alias WISC
        operProc( i++)
      enddo
      operExit()
      new_wisc ->( dbCommit())
      basesMClose( { dbInfo, wiscInfo()})
      base2Delete( wiscInfo())
      if fRename( dbInfo[ 1], wiscInfo()[ 1]) == 0
         name1 := left( dbInfo[ 1],     len( dbInfo[ 1])     - 1) + "T"
         name2 := left( wiscInfo()[ 1], len( wiscInfo()[ 1]) - 1) + "T"
         if fRename( name1, name2) == 0
            rV := baseIndex( wiscInfo())
         endif
      endif
   endif
endif
return rV

static funcDef recCopyWisc with fileName:C, inboxPath:C ;
   local rV := .T., s, info, i


info := aClone( wiscInfo())
info[ 1] := fileName
info[ 2] := "NEW_WISC"
info[ 4] := {}
basePath( info, inboxPath)
s        := Select()
if recNewWisc()
endif
if basesXOpen( { wiscInfo(), info})
            /*
   operStartUp( "�������� ����� ������", i := 1, WISC ->( lastRec()))
   WISC ->( dbSetOrder( 0))
   WISC ->( netGoTop())
   while WISC ->( !eof())
     if WISC ->type <> "9999"
        WISC ->( dbDelete())
     endif
     skip 1 alias WISC
     operProc( i++)
   enddo
   operExit()
   messageStartUp( "��������...")
   WISC ->( __dbPack())
   messageExit( 0)
              */

   operStartUp( "��७�� ������ � �믨᪥", i := 1, NEW_WISC ->( lastRec()))
   WISC ->( dbSetOrder( 1))
   WISC ->( netGoTop())
   while NEW_WISC ->( !Eof())
     if WISC ->type <> "9999"
        rV := wiscAddRecord()
     else
        if WISC ->( dbSeek( NEW_WISC ->schet + NEW_WISC ->type))
           rV := .T.
        else
           rV := wiscAddRecord()
        endif
     endif
     if rV 
        WISC ->schet  := NEW_WISC ->schet
        WISC ->type   := NEW_WISC ->type
        WISC ->ID     := NEW_WISC ->ID
        WISC ->buffer := NEW_WISC ->buffer
        WISC ->( dbSkip( 0))
     else
        errLog( "���������� �������� ������ #" + nStr( NEW_WISC ->( recNo())))
     endif
     skip 1 alias NEW_WISC
     operProc( i++)
   enddo
   operExit()
   basesMClose( { info, wiscInfo()})
   base2Delete( info)
else
   errLog( "�⪠� ������ �� �ਥ�� �믨᮪ � 䨫����", .F.)
endif
select (s)
return rV

//
// ��१����� �஢����
//
funcDef modCopySums with basesPath:C, inBoxPath:C, tempPath:C ;
                    local rV := .F., i, fileName := "sums.dbf"

COPY FILE ( newFPath( fileName, basesPath)) TO ( newFPath( fileName, tempPath))

if baseMXOpen( sumInfo())
   operStartUp( "�����⮢�� �஢����", i := 1, SUMS ->( lastRec()) )
   while SUMS ->( !eof())
     if SUMS ->typeOf <> ORD_33_DATE then SUMS ->( dbDelete())
     if SUMS ->typeOf == "9100"      then SUMS ->( dbDelete())
     skip 1 alias SUMS 
     operProc( i++)
   enddo
   messageStartUp( "�������� 䠩�� �஢����. ��������...")
   PACK
   messageExit( 0)
   operExit()
   baseMClose( sumInfo())
   rV := .T.
endif

return rV

* NEW OBJECT FILE
//
// ����⨥ ����樮����� ���
//
funcDef odOpen with theDate:D local rV := .T., tmpDate, n := 1

messageStartUp( "�஢�ઠ ��⮢���� ࠡ���")
if modOpen()

   //
   // � ���� ���� ����� �� �।��騥 ���� ?
   //
   if MODEM ->( emptyRec())
      if (rV := modReceive( theDate))
          rV := modWrite( theDate, DAY_OPEN)
      endif
   else
      //
      // �饬 ᥠ�� ������� ��� �� ��� ����
      //
      if MODEM ->( !netSeek( DAY_CLOSE + "19971231", .T.))
         modWrite( CtoD( "31/12/1997"), DAY_CLOSE)
      endif

      if MODEM ->( netSeek( DAY_CLOSE + DtoS( theDate), .T. ))
         errLog( { "������� ��⮩ " + DtoC( theDate) + " �����" ,;
                  "���� 㦥 ������ !"})
//  Add by TMV 1-11-2001
			MODEM ->( dbSetOrder( 2))
			MODEM ->( dbSeek( DtoS( theDate ), .T.))
			do while MODEM ->dateWork == theDate
				MODEM -> (dbskip())
			enddo
         tmpDate = MODEM ->dateWork
			MODEM ->( dbSetOrder( 1))
      	if !MODEM ->( netSeek( DAY_CLOSE + DtoS( tmpDate), .T. ))
	else
           rV := .F.
	endif
//  Add by TMV 1-11-2001  // End
         modWrite( theDate, DAY_TRY_OPEN_CLOSED)
      else
        //
        // ��᫥ �⮣� ��� ���� �������
        //
        if MODEM ->typeOf == DAY_CLOSE .AND. MODEM ->dateWork > theDate
           errLog( { "������� ��⮩ " + DtoC( theDate) + " �����" ,;
                    "��᫥ �⮩ ���� ���� ������� ��� !"})
           modWrite( MODEM ->dateWork, DAY_TRY_OPEN_CLOSED)
           rV := .F.
        else
          //
          // ��� ���� 㦥 �����
          //
          if MODEM ->( netSeek( DAY_OPEN + DtoS( theDate), .T. ))
             rV := modWrite( theDate, DAY_OPEN_AGAIN)
          else
             //
             // ��᫥ �⮣� ��� 㦥 ���� ������
             //
             if MODEM ->typeOf == DAY_OPEN .AND. MODEM ->dateWork > theDate
//  Replaced by TMV  19-11-2001
				    errLog( { "������� ��⮩ " + DtoC( theDate) + " �����" ,;
								 "��᫥ �⮩ ���� ���� ������ ��� !"})
					 modWrite( theDate, DAY_TRY_OPEN_CLOSED)
					 rV := .F.
//                rV := modWrite( theDate, DAY_OPEN)
             else
                //
                // �।��騩 ���� ������ ?
                //
                MODEM ->( baseSavePos())
                MODEM ->( dbSetOrder( 2))
                MODEM ->( dbSeek( DtoS( theDate - 1), .T.))
                if MODEM -> dateWork >( theDate - 1) then MODEM ->( dbSkip( -1))
                if MODEM ->( Eof()) then MODEM ->( dbSkip( -1))
                if MODEM -> ( BOF())
                   //
                   // ���।� ��� ���
                   //
                   if modReceive( theDate) then rV := modWrite( theDate, DAY_OPEN)
                else
                   MODEM ->( dbSetOrder( 1))
                   //
                   // �᫨ �।��騩 ���� ������ - ��஥� ���
                   tmpDate := MODEM ->dateWork
                   if MODEM ->( netSeek( DAY_CLOSE + DtoS( tmpDate)))
                      if modReceive( theDate) then rV := modWrite( theDate, DAY_OPEN)
                   else
						 	 n = dialog({'�������� !',;
											 '�� ��ࠫ� ����, �⫨�� �� ����⮣� ᥩ��.',;
											 '�᫨ �� �訡���� - �롥�� <�த������ ࠡ��� � ��஬ ���>,',;
							 				 '���� �롥�� <������ ���� ����>.'},;
											{'�த������ ࠡ��� � ��஬ ���',;
											 '������ ���� ����'})
							 if n == 2
							 	 errLog( { "����� �㦭� ������� ���� � ��ࠢ��� ����� �� 䨫���" ,;
                               "��⥬ ������� �ணࠬ�� �� ࠧ !" })
                      	 if modSend( theDate) then modWrite( tmpDate, DAY_CLOSE)
							 else
							 	 errLog( {"������� �ணࠬ�� �� ࠧ � 㪠��� �ࠢ����� ���� !" })
							 endif
                    	 rV := .F.
                   endif
                endif
                MODEM ->( baseRestPos())
             endif
          endif
        endif
      endif
   endif
   modClose()
endif
messageExit( 0)

return rV

funcDef begDate with type:C, date:D local rV := CtoD( "")

MODEM ->( dbSeek( type + DtoS( date - 1), .T.))
if type == MODEM ->typeOf .AND. MODEM ->dateWork <= date
   rV := MODEM ->dateWork
else
   MODEM ->( netSkipUp())
   if MODEM ->typeOf == type
      rV := MODEM ->dateWork
   endif
endif

return rV

//
// �����⨥ ����樮����� ���
//
funcDef odClose with theDate:D, cScr:C local rV := .T., n := 1

if !isOrwDayClose()  //11-08-2000
  if dialog({"�������� !!!",;
             "�� ������ ���� �� ���⥦�� ��. ���",;
             "�᫨ �� �� �믮���� ��� ��楤���",;
             "� ���� ���, �ᯫ�� �㤥� ��⪮�",;
             "      ���㬠�� ��襭쪮        "},;
            {"��ࢠ�� ��ࠢ�� ������", "�த������"}) != 2
    return .F.
  endif
endif

if modOpen()
   if MODEM ->( netSeek( DAY_CLOSE + DtoS( theDate) ))
		if dialog({'�������� !',;
					  '����� �� 㦥 ��ࠢ����.',;
					  '��ࠢ��� ����୮ ?'},;
					  {'  � �  ',' � � � '}	) == 1
      	if modSend( theDate) then rV := modWrite( theDate, DAY_REPEAT)
		endif
   else
// Add by TMV  16-10-2001
		if (n:=dialog({"�������� !!!",;
             	"        �� ��� ������� ����樮��� ����?      ",;
					"  ��᫥ ������� �஢����� ����樨 �� ��� ����  ",;
					"               �㤥� ���������� !                 "},;
            	{"��ࠢ��� � ������� ����",;
					 "�⬥��"})) == 1
// Add by TMV  16-10-2001  --  End
      	if modSend( theDate)     // Edited by TMV  16-10-2001
				rV := modWrite( theDate, DAY_CLOSE)
			endIf
		endif
   endif
   modClose()
endif

return rV
 
//
// ����� ��⮪�
//
funcDef modFlow with theDate:D local rV := .F., opened, theFlow := -1, s

s := select()
opened := select( "modem") >0
if !opened then rV := modOpen()

MODEM ->( baseSavePos())
if opened .OR. rV

   MODEM ->( dbSetOrder( 1))
   if MODEM ->( netSeek( DAY_OPEN + DtoS( theDate)))
      if MODEM ->( mRLock())
         MODEM ->flow := MODEM ->flow + 1
         theFlow      := MODEM ->flow
         MODEM ->( netUnLock())
      endif
   endif
   if !opened then modClose()
   select ( s)
endif
MODEM ->( baseRestPos())

return theFlow

static funcDef fNetRename with oldFile:C, newFile:C local rV 
  __copyFile( oldFile, newFile)
  rV := fErase( oldFile)
return rV



func wiscAddRecord 
local rV := .F.

if empty( WISC ->schet) .AND. WISC ->( !Eof())
   if WISC ->( rLock())
      rV := .T.
   else
      WISC ->( dbAppend())
      if !netErr()
         rV := .T.
      else
         rV := WISC ->( mAppend())
      endif
   endif
else
   WISC ->( dbAppend())
   if !netErr()
      rV := .T.
   else
      rV := WISC ->( mAppend())
   endif
endif

return rV

funcDef base2Delete with dbInfo:A ;
   local dbName, ntxInfo, ntxName, i, rV := .F.
   basePrep( dbInfo)
   dbName  := dbInfo[ 1]
   ntxInfo := dbInfo[ 4]
   if fErase( dbName) = -1
      errOK( { "�� ���� 㤠���� ���� ������",         ;
               dbName + ",",                          ;
               "��� DOS-�訡�� - " + nStr( fError()) })
   else
      dbName := subStr( dbName, 1, len( dbName) - 1) + "T"
      if file( dbName) .AND. fErase( dbName) = -1
         errOK( { "�� ���� 㤠���� ���� ������",         ;
                  dbName + ",",                          ;
                  "��� DOS-�訡�� - " + nStr( fError()) })

      else 
         rV := .T.
         for i=1 to len( ntxInfo)
            ntxName := ntxInfo[ i][ 1]
            if fErase( ntxName) = -1
               errOK( { "�� ���� 㤠���� ������� 䠩�",      ;
                        ntxName + ",",                         ;
                        "��� DOS-�訡�� - " + nStr( fError()) })
               rV := .F.
               exit
            endIf
         endFor
      endIf
   endif
return rV

//
// �஢�ઠ ����� �����⮢
//
funcDef accCheck local rV := .T., i, curCode := "", curRec := 0
if basesMOpen( { cliInfo(), accInfo(), sumInfo()})
  operStartUp( "�஢�ઠ ��⮢", i := 1, ACC ->( lastRec()))
  ACC ->( dbGoTop())
  while ACC ->( !Eof())
    if ACC ->lEdited .AND. empty( ACC ->code)
       OK( { "������ ��� " + ACC ->Schet + " ��� ���� ������. ",;
          "���⮪ �� ���� " + mStr( sumRest( ACC ->schet, setCurDate())),;
          "�������� �롥�� ������, ���஬� �ਭ������� ���." })
       if viewOwner( @curCode, @curRec) .AND. !empty( curCode)
         if ACC ->( mRLock())
           ACC ->code := curCode 
           ACC ->( netUnLock())
         endif
       else
         rV := .F.
         exit
       endif
    endif
    skip 1 alias ACC
    operProc( i++, .F.)
  enddo
  operExit()
  basesMClose( { cliInfo(), accInfo(), sumInfo()})
endif
return rV

// ����஢���� ������ � InBox ��� "�ਥ��" ������ �� 業��
procDef SendToYouSelf local aFile, i, sPath, sOd, oldSel := select(),;
	sSource, sDist, sTmp, mnSumma:=0
private sFile
aFile := { ;
			 'Accounts.dbf',;
			 'Assort.dbf',;
			 'Clients.dbf',;
			 'Contract.dbf', 'Contract.dbt',;
			 'District.dbf',;
			 'Kassa.dbf',;
			 'Kr_tprou.dbf',;
			 'Oper.dbf',;
			 'Provider.dbf',;
			 'Prov2.dbf',;
			 'Setup.dbf',;
			 'Sump.dbf',;
			 'Sums.dbf',;
			 'Trust.dbf',;
			 'Vid.dbf',;
			 'Wisc.dbf', 'Wisc.dbt',;
			 'Wisctype.dbf';
			 }
sPath = confPathInBox()
sTmp = confPathTemp()
sOd = confPathOD()
if left(sPath, 1) == '.' then sPath = Strtran( alltrim( sPath ), '.', Diskname() + ':\' + Curdir(), 1,1)
if left(sOd, 1) == '.' then sOd = Strtran( alltrim( sOd ), '.', Diskname() + ':\' + Curdir(), 1,1)
if left(sTmp, 1) == '.' then sTmp = Strtran( alltrim( sTmp ), '.', Diskname() + ':\' + Curdir(), 1,1)
if right(sPath, 1) != '\' then sPath += '\'
if right(sOd, 1) != '\' then sOd += '\'
if right(sTmp, 1) != '\' then sTmp += '\'

//  ����஢���� 䠩��� � Temp
messageStartUp("���࠭���� ���� � TEMP")
inBoxLogWrite( "���࠭���� ���譥� ���� � TEMP " + Time())
for i=1 to len(aFile)
	sSource = sOd + aFile[i]
	sDist = sTmp + aFile[i]
	if file(sSource)
		COPY FILE ( sSource) TO ( sDist)
		inBoxLogWrite( "���� " + sSource + " ᪮��஢��. " + Time())
	endif
next
messageExit(0)
//  ���⪠ ���� ��� ������ ���
if basesMExcOpen( monInfo())
	operStartUp( "������ ���⪮� �� ��⠬", i := 1, ACC ->( lastRec()))
	ACC->(dbgotop())
	do while !ACC->(eof())
		replace ACC->LEdited with .F., ACC->Paym_Date with Ctod(''),;
			ACC->rest with sumRest(ACC ->schet, setCurDate())*100
		ACC->(dbskip())
		operProc( i++, .F.)
	enddo
	operExit()
   inBoxLogWrite( "�����⠭� ���⪨ �� ��⠬ " + Time())
	messageStartUp("���⪠ ����")
	sFile=sPath + aFile[1]  //  ACCOUNTS
	sFile=sPath + 'Sums.dbf'
	Select SUMS
	zap       //  ����� �� �஢����
	if kasOpen()
   	if KASSA ->(dbSeek( '999'))
			mnSumma = KASSA ->summa
			if KASSA ->( mRLock())
				KASSA ->obK := 0; KASSA ->obD := 0
				KASSA ->( netUnLock())
			endif
			if KASSA ->(dbSeek( '998'))
				if KASSA ->( mRLock())
					KASSA ->summa := mnSumma  //; KASSA ->obK := 0; KASSA ->obD := 0
					KASSA ->( netUnLock())
				endif
			endif
		endif
   	kasClose()
   	select (oldSel)
	endif
	basesMClose( monInfo())
   inBoxLogWrite( "��饭 ॥��� ����権 " + Time())
	messageExit(0)

//  ����஢���� ��饭��� 䠩��� � InBox
	messageStartUp("���࠭���� ���� � InBox")
	for i=1 to len(aFile)
		sSource = sOd + aFile[i]
		sDist = sPath + aFile[i]
		if file(sSource) then COPY FILE ( sSource) TO ( sDist)
	next
   inBoxLogWrite( "���� ����� ��९�ᠭ� � InBox " + Time())
	messageExit(0)
else
	messageB("�������� �訡�� �� �����⮢�� ���ଠ樨.")
endif
return