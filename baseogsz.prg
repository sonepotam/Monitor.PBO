
*============================================================================
*                                                                           
*                     �ணࠬ��  : MON.EXE
*                     ����       : baseOGSZ.prg
*                     ����      : ���⫨� �.�.
*
*                     �����祭�� : ���㦨����� ���
*
*----------------------------------------------------------------------------
#include "clippExt.ch"
#include "common.ch"
#include "inkey.ch"
#include "getExit.ch"
#include "gsz.ch"

* NEW OBJECT FILE

funcDef gszBuyGSZ local rV := .T.
if !gszCheckPasp() then return .F.
if contLock( .T., 6)
   rV := gszEdit( .T., .F.)
   contUnLock( !rV)
endif
return NIL

funcDef gszEditSchet ;
  local rV := .F., gets, aFrom1, aFrom2, ptrSch1, ptrSch2, rec1, rec2,;
        oldSch1, oldSch2, sch1, sch2, arr

sch1 := sch2 := ""
rec1 := rec2 := 0
getIncomSchet( CLIENTS ->code, @sch1, @sch2, @rec1, @rec2)
oldSch1   := sch1; oldSch2   := sch2
gets := {{ "������஢���� ��⮢                              "        },;
         { "���������������������������������������������������"        },;
         { "�������� ������権 : " + cliFullName()                      },;
         { "��� ��� ���᫥��� �㯮����� ��室� : ", block( sch1),,,,    ;
                      {|g| changeSchet( g, @rec1)                      }},;
         { "��� ��� ����襭�� ������権         : ", block( sch2),,,,    ;
                      {|g| changeSchet( g, @rec2)                      }} }
if getUp( 5, 5, gets)
  if oldSch1 <> sch1 .OR. oldSch2 <> sch2
    if WISC ->( rV := mAppend())
      WISC ->schet  := CONTRACT ->docNumb
      WISC ->type   := "952 "
      WISC ->buffer := sch1 + ";" + sch2
      WISC ->( netUnLock())
      arr := { }
      if oldSch1 <> sch1
       aAdd( arr, padR( "��� ��� ����. �㯮�. ��室�", 30) + " " + oldSch1 + " " + sch1)
      endif
      if oldSch2 <> sch2
       aAdd( arr, padR( "��� ��� ����襭�� ������権  ", 30) + " " + oldSch2 + " " + sch2)
      endif
      gszIzmPrint( arr)
    endif
  endif
endif
return rV


funcDef gszEdit with newContract:L, useRequest:LU ;
   local rV, gets, curOwner, ownerRec, aCode, aName, aBuf, ptr1, ptr2,;
         ptr3, cnt1, cnt2, cnt3, tot1, tot2, tot3, tot, aCnt, totPtr ,;
         ptrPaym, aPaym, ptrSch1, aFrom1, aFrom2, rec1, rec2, sch1   ,;
         sch2, s1, s2, s3, ownerCode, mustSaveOpen, aOrder, ro, sim  ,;
         nDoc, nazn, ptrWisc, aWisc, aDay, ptrProl, yesNo, month1    ,;
         year1, month2, year2, firstDate, nextDate, oldEsc, toBuffer ,;
         buf2, mustSchet, oldSch1, oldSch2, makeTrans, naznNal, useDogovor,;
         ordMem,;
			intDoc  // Add by TMV 10-02-2004

useDogovor := confUseGSZDogovor()
DEFAULT useRequest TO .T.
yesNo   := { "����", "��� "}
ptrProl := 2
if !newContract
   // messageB( "������஢���� ����饭� !!!")
   gszEditSchet()
   return .F.
endif

if DtoS( setCurDate()) <> setupWDate()
   messageB( "��� ���ଠ樨 � ���� !!!")
   return .F.
endif

if empty( confGSZIncom())
   messageB( "��� ��� ��室�� SETUP -> GSZIN. ������� �� 䨫��� !")
   return .F.
endif

if empty( confGSZS2())
   messageB( "��� ��� ��室�� SETUP -> GSZS2. ������� �� 䨫��� !")
   return .F.
endif


assGetInfo( GSZ_CODE_2, @aCode, @aName, @aBuf)
//messageB( nStr( len( aCode)))


aAdd( aCode, NIL); aIns( aCode, 1)
aAdd( aName, NIL); aIns( aName, 1)
aAdd( aBuf,  NIL); aIns( aBuf,  1)
aCode[ 1] := Space(  4)
aName[ 1] := Space( 30)
aBuf[  1] := Space( 80)
if len( aCode) <= 1
   messageB( "��� ���ଠ樨 �� ��������� !!!")
   return .F.
endif

for ptr1 := 1 to len( aName) do aName[ ptr1] := left( aName[ ptr1], 30)
if !useRequest
   ptr1 := 2
   while ptr1 <= len( aName)
     if gszBuy( aBuf[ ptr1]) > 0
        ptr1++
     else
        aDel( aName, ptr1); aSize( aName, len( aName) - 1)
        aDel( aCode, ptr1); aSize( aCode, len( aCode) - 1)
        aDel( aBuf,  ptr1); aSize( aBuf,  len( aBuf ) - 1)
     endif
   enddo
endif

ptr1 := ptr2 := ptr3 := 1
cnt1 := cnt2 := cnt3 := 0
tot1 := tot2 := tot3 := 0.00
tot  := 0.00
nDoc := 0

aPaym    := { "�������筮", "�����묨", "�� ����"}
curOwner := padR( cliFullName(), 35)
ownerRec := CLIENTS ->( recNo())

getAFrom( @aFrom1, @aFrom2, useRequest)
if len( aFrom1) > 1
   ptrPaym := 1
   ptrSch1 := 2
else
   ptrPaym := 2
   ptrSch1 := 1
endif
aCnt    := { 4, 7, 10}
totPtr  := 12
sch1    := sch2 := ""
rec1    := rec2 := 0

getIncomSchet( CLIENTS ->code, @sch1, @sch2, @rec1, @rec2)
mustSchet := !empty( sch1) .AND. !empty( sch2)
oldSch1   := sch1; oldSch2   := sch2

// Add by TMV 10-02-2004  And removed for edit PC_POS
//intDoc := gszGetSale( CLIENTS ->code )
//if !empty(intDoc)
//endif
// Add by TMV 10-02-2004 -- End

//Replaced by TMV 10-02-2004
gets := {{ "���㯪� ������権 ���㤠��⢥����� ���客��� �����"        },;
;//gets := {{ "���㯪� ������権 ���㤠��⢥����� ���ॣ�⥫쭮�� ����� �����-�����࣠"},;
         { "���������������������������������������������������"        },;
         { "�������� ������権 :", block( curOwner),,,                   ,;
              {|g| changeOwner( g, @ownerRec, @rec1, @rec2, useRequest)}},;
         { "*" + chr( 25)                                               },;
         { "���㬥��           :", {|| getCurDocum( ownerRec)},,{|| .F.}},;
         { "------------------------------------------------------"     },;
         { "���㯪� ������権"                                          },;
         { "����/���                 ������⢮             �⮨�����" }}
aAdd( gets,;
         { "", block( ptr1), aName,{|g| s1 := g:varGet, .t.},             ;
       {|| chkName( s1, aName, aCode, aBuf, 3, aCnt,totPtr, useRequest)}} )
aAdd( gets, { "*" + chr( 25)                                               })
aAdd( gets,;
         { "*        ", block( cnt1), "999999",,                             ;
          {|g| chkSum( g, aCode, aName, aBuf, aCnt, totPtr, useRequest)}} )
aAdd( gets,;
         { "*      ", block( tot1),"99999999.99",{|| .F.}               })

aAdd( gets,;
         { "", block( ptr2), aName,{|g| s2 := g:varGet,!useRequest .AND. !useDogovor},      ;
       {|| chkName( s2, aName, aCode, aBuf, 6, aCnt,totPtr, useRequest)}} )
aAdd( gets, { "*" + chr( 25)                                               })
aAdd( gets,;
       { "*        ", block( cnt2), "999999",{|| !useRequest .AND. !useDogovor},          ;
          {|g| chkSum( g, aCode, aName, aBuf, aCnt, totPtr, useRequest)}} )
aAdd( gets, { "*      ", block( tot2),"99999999.99",{|| .F.}               })

aAdd( gets,;
         { "", block( ptr3), aName,{|g| s3 := g:varGet,!useRequest .AND. !useDogovor},      ;
       {|| chkName( s3, aName, aCode, aBuf, 9, aCnt,totPtr, useRequest)}} )
aAdd( gets,{ "*" + chr( 25)                                               })
aAdd( gets,;
         { "*        ", block( cnt3), "999999",{|| !useRequest .AND. !useDogovor},          ;
          {|g| chkSum( g, aCode, aName, aBuf, aCnt, totPtr, useRequest)}} )
aAdd( gets,;
         { "*      ", block( tot3),"99999999.99",{|| .F.}               })

aAdd( gets,;
         { "�⮣�                                           : "          ,;
                                     block( tot ),"99999999.99",{|| .F.}} )
aAdd( gets,;
         { "------------------------------------------------------"     } )
aAdd( gets,;
         { "�����      : ", block( ptrPaym), aPaym, {|g| !useRequest}   ,;
                                                     {|g| chkPaym( g)}  } )
aAdd( gets,;
         { "*" + chr( 25)                                               })
aAdd( gets,;
         { "��� ������ : ", block( ptrSch1), aFrom1,,                    ;
                    {|g| chkFrom(g, aFrom1, aFrom2, totPtr, useRequest)}} )
aAdd( gets,;
         { "*" + chr( 25)                                               })
aAdd( gets,;
         { "" }                                                          )
aAdd( gets,;
         { "��� ��� ���᫥��� �㯮����� ��室� : ", block( sch1),,,,    ;
                      {|g| changeSchet( g, @rec1)                      }} )
aAdd( gets,;
         { "*" + chr( 25)                                               })
aAdd( gets,;
         { "��� ��� ����襭�� ������権         : ", block( sch2),,,,    ;
                      {|g| changeSchet( g, @rec2)                      }} )
aAdd( gets,;
         { "*" + chr( 25)                                               })
aAdd( gets,;
         { "------------------------------------------------------"     })
aAdd( gets,;
         { "���⥫�騪         :" + cliFullName()                       })
aAdd( gets,;
         { "���㬥��           :" + cliDocum()                          })
ptrWisc := ADay := 0
if useRequest
   firstDate:= firstDay( addMonth( setCurDate(), 1))
   month1   := month( firstDate)
   year1    := year(   firstDate)

   nextDate := firstDay( addMonth( firstDate, 1))
   month2   := month( nextDate)
   year2    := year(  nextDate)
   ADay     := Day( setCurDate())
   aWisc    := { "���㯠�� ������樨 ���� ࠧ � �����     ",;
                 "��������஢����                        ",;
                 "����� ������権 � ���� ࠧ��饭�� �਩" }
   ptrWisc  := 1
   gets[ 3] := { "�������� ������権 :", {|| curOwner},,{|| .F.}}
   aSize( gets, len( gets) - 2)
   aAdd( gets, { "����祭�� :", block( ptrWisc), aWisc,,{|g| chkAWisc( g, aWisc)}})
   aAdd( gets, { "* �᫮ ", block( ADay), "99", {|g| chkGszDay(g, aWisc,17)} })
   aAdd( gets, { "��砫�       :", block(month1), aClone( aMonths()) })
   aAdd( gets, { "*" + chr( 25)                                      })
   aAdd( gets, { "*", block( year1), "9999",, {|g| appFirstDate(g,firstDate,19)}})
   aAdd( gets, { "*�."                                          })
   aAdd( gets, { "����砭��    :", block( month2), aMonths()    })
   aAdd( gets, { "*" + chr( 25)                                 })
   aAdd( gets, { "*", block( year2), "9999",,{|g|appLastDate(g,firstDate,19,21)}})
   aAdd( gets, { "*�."                                          })
   aAdd( gets, { "�஫������  :", block( ptrProl), yesNo,{||.F.}})
endif
 
useDec( .F.)
if rV := getUp( gets) .AND. !empty( sch1) .AND. !empty( sch2) .AND. ;
   ptrWisc <> 2

   do case
      case ptrPaym <= 1 do nDoc := ordIncMem( setCurDate())
      case ptrPaym == 2 do nDoc := ordIncIn(  setCurDate())
   endcase   


   //
   // �஢�ઠ ����⮢
   //
   if !useRequest
      cnt1 := - kasLimitSumma( - cnt1, aCode[ ptr1])
      cnt2 := - kasLimitSumma( - cnt2, aCode[ ptr2])
      cnt3 := - kasLimitSumma( - cnt3, aCode[ ptr3])
      if ( cnt1 + cnt2 + cnt3 ) = 0 then return .F.
   endif
   tot1 := gszBuy( aBuf[ ptr1]) * cnt1
   tot2 := gszBuy( aBuf[ ptr2]) * cnt2
   tot3 := gszBuy( aBuf[ ptr3]) * cnt3
   tot  := tot1 + tot2 + tot3

   if ptrWisc == 3 then ADay := 0
   naznNal := "����� ���� ���,"
   if cnt1 >0 then naznNal += rTrim( aName[ptr1]) + " - " + nStr(cnt1) + " ��,"
   if cnt2 >0 then naznNal += rTrim( aName[ptr2]) + " - " + nStr(cnt2) + " ��,"
   if cnt3 >0 then naznNal += rTrim( aName[ptr3]) + " - " + nStr(cnt3) + " ��,"
   if right( naznNal, 1) == "," then naznNal := left( naznNal, len(naznNal) - 1)

   CLIENTS ->( baseSavePos())
   CLIENTS ->( dbGoTo( ownerRec))
   ownerCode := CLIENTS ->code
   CLIENTS ->( baseRestPos())

   CONTRACT ->code3     := "73"
   CONTRACT ->code      := ownerCode
   CONTRACT ->intDoc    := confOGSZDoc()
   CONTRACT ->debetAcc  := if( ptrPaym == 1, aFrom2[ ptrSch1, 2], "")
   CONTRACT ->creditAcc := ""
   CONTRACT ->docNumb   := getOGSZDocNumb()
   CONTRACT ->summa     := tot * 100

   if useRequest
      CONTRACT ->dateBeg  := packDate( 1, month1, year1)
      CONTRACT ->dateEnd  := packDate( 1, month2, year2)
      CONTRACT ->buffer   := left( CONTRACT ->buffer, 2)  + ;
           if( ptrProl <= 1, "Y", "N") + padL( ptrWisc, 2, "0") + ;
           Str( ADay, 2)
   else
      CONTRACT ->dateBeg   := setCurDate()
      CONTRACT ->dateEnd   := setCurDate()
   endif
   CONTRACT ->dateCur   := setCurDate()
   CONTRACT ->mBuffer   := CLIENTS ->code + "," + ownerCode         + ";" + ;
     aCode[ ptr1] + "," + nStr( cnt1) + "," + Str( gszBuy( aBuf[ ptr1]),15,2)+ "," + ;
     DtoC( gszDateKef( aBuf[ ptr1]))  + "," + gszBufSeria( aBuf[ ptr1]) + ";" + ;
     aCode[ ptr2] + "," + nStr( cnt2) + "," + Str( gszBuy( aBuf[ ptr2]),15,2)+ "," + ;
     DtoC( gszDateKef( aBuf[ ptr2]))  + "," + gszBufSeria( aBuf[ ptr2]) + ";" + ;
     aCode[ ptr3] + "," + nStr( cnt3) + "," + Str( gszBuy( aBuf[ ptr3]),15,2)+ "," + ;
     DtoC( gszDateKef( aBuf[ ptr3]))  + "," + gszBufSeria( aBuf[ ptr3]) + ";" + ;
                          Str( tot, 15, 2 ) + ";" + sch1 + "," + sch2 + ";" + nStr( nDoc) + ";"

   if CLIENTS ->code <> ownerCode .OR. !useRequest
      OK( { "��ଫ��� ���㯪� ��� � ����஬ ������� " + CONTRACT ->intDoc   ,;
            "������ �㤥� ����� � ���������� ����祭��� �������� ������権",;
            curOwner })
   endif
   //
   // �㬬� �믫�祭� �� ����७����
   //
   toBuffer := CONTRACT ->intDoc
   buf2     := ""
   if len( aFrom2[ ptrSch1]) > 2 .AND. !useRequest
      TRUST ->( baseSavePos())
      TRUST ->( dbGoTo( aFrom2[ ptrSch1, 3]))
      toBuffer += ";DOV:" + TRUST ->number
      buf2     := "DOV:" + TRUST ->number
      if trustRLock()
         trustLastSum( setCurDate(), tot)
         trustUnLock()
      endif
   endif

   if gszPrint( newContract)
      //
      // ���室�� � �஢�����
      //
      makeTrans := .F.
      if !useRequest
         do case
            case ptrPaym <= 1 // ������

                 aOrder := memWisc( aFrom2[ ptrSch1, 2], tot,;
                           "�� ���㤠��⢥��� ᡥॣ�⥫�� ������ ������樨 �����-�����࣠. ��� �� ����������.")

                 //aOrder := memWisc2( aFrom2[ ptrSch1, 2], confGSZIncom(),;
                 //   confBankName(), tot, confBankName() + " ��� N " + confGSZS2() ,;
                 //   "�� ���㤠��⢥��� ᡥॣ�⥫�� ������ ������樨 �����-�����࣠. ��� �� ����������."           ,;
                 //   "����祭��-������ N " + CONTRACT ->intDoc, setCurDate())
                 //aOrder := memWisc3( aFrom2[ ptrSch1, 2], confBankName(),;
                 //   tot, confBankName() + " ��� N " + confGSZS2()           ,;
                 //   "�� ���㤠��⢥��� ᡥॣ�⥫�� ������ ������樨 �����-�����࣠." + endl() + "��� �� ����������."           ,;
                 //   "����祭��-������ N " + CONTRACT ->intDoc, setCurDate())
                 printAOrder( aOrder)

                 //nDoc := ordIncMem( setCurDate())
                 if sumRest( aFrom2[ ptrSch1, 2], setCurDate()) >= tot
                    if !( makeTrans := sumWrite( aFrom2[ ptrSch1, 2], - tot,;
                          setCurDate(), GSZ_BUY_BEZNAL, nDoc,, toBuffer))
                       errLog( "�஢���� �� �������. ������� �� 䨫��� !!!")
                    endif
                 else
                    errLog( { "�� �஢���� �맮��� ��᭮� ᠫ줮 �� ���� !!!",;
                              "�஢���� �� ᤥ���� !!!"})
                 endif

            case ptrPaym == 2 // �����
                 operGetInfo( GSZ_BUY_NAL, @ro, @sim, @nazn)
                 //nDoc   := ordIncIn( setCurDate())
                 useZabota( .T.)
                 aOrder := vznos_nal( nDoc, setCurDate(), cliFullName(),;
                    confGSZIncom(), tot,;
                    confBankName()+','+ rTrim(confFilial()), naznNal,;
                    confKassa(), sim,,, .T.)
                 useZabota( .F.)
                 printAOrder( aOrder)
                 if !( makeTrans := sumWrite( confGSZIncom(), tot, setCurDate(),;
                       GSZ_BUY_NAL, nDoc,, toBuffer))
                    errLog( "�஢���� �� �������. ������� �� 䨫��� !!!")
                 endif
            case ptrPaym == 3 // ����
                 sorry()
         endcase
      endif
      if mustSchet
         gszMustSchet( CONTRACT ->docNumb, oldSch1, oldSch2, sch1, sch2)
      endif

      //
      // ���������� ���������
      //
      if ( makeTrans .OR. useRequest ) .AND. WISC ->( mAppend())
         WISC ->schet  := CONTRACT ->intDoc
         WISC ->type   := if( useRequest, GSZ_WISC_WISC, GSZ_WISC)
         WISC ->ID     := if( useRequest, "", nStr( SUMS ->( recNo())))
         WISC ->buffer := ;
            CLIENTS ->code  + "," + ownerCode + "," + CONTRACT->docNumb+ "," + nStr( nDoc)+ ";" + ;
            aCode[ ptr1]    + "," + nStr( cnt1) + "," + Str( gszBuy( aBuf[ ptr1]), 15, 2) + "," + ;
            safeDtoC( gszDateKef( aBuf[ ptr1]))  + "," + gszBufSeria( aBuf[ ptr1]) + ";" + ;
            aCode[ ptr2]    + "," + nStr( cnt2) + "," + Str( gszBuy( aBuf[ ptr2]), 15, 2) + "," + ;
            safeDtoC( gszDateKef( aBuf[ ptr2]))  + "," + gszBufSeria( aBuf[ ptr3]) + ";" + ;
            aCode[ ptr3]    + "," + nStr( cnt3) + "," + Str( gszBuy( aBuf[ ptr3]), 15, 2) + "," + ;
            safeDtoC( gszDateKef( aBuf[ ptr3]))  + "," + gszBufSeria( aBuf[ ptr3]) + ";" + ;
            Str( tot, 15, 2 )     + ";" + sch1  + "," + sch2                              + "," + ;
            if( ptrPaym == 1, aFrom2[ ptrSch1, 2], "") + ";" + ; // �᫨ ������ - � ���, ���� ����
            DtoC( CONTRACT ->dateBeg)                 + ";" + ;
            DtoC( CONTRACT ->dateEnd) + ";" + buf2    + ";" + ;
            if( ptrProl <= 1, "1", "0")               + ";" + ;
            padL( ptrWisc, 2, "0") + ";" +  Str( ADay, 2)
         WISC ->( netUnLock())
      else
         if !makeTrans
            limDeleteSumma( - cnt1, aCode[ ptr1])
            limDeleteSumma( - cnt2, aCode[ ptr2])
            limDeleteSumma( - cnt3, aCode[ ptr3])
            rV := .F.
         endif
         messageB( "��������� �� ��ଫ��� !!!")
      endif
   else
      if !useRequest
         limDeleteSumma( - cnt1, aCode[ ptr1])
         limDeleteSumma( - cnt2, aCode[ ptr2])
         limDeleteSumma( - cnt3, aCode[ ptr3])
         rV := .F.
      endif
   endif
endif
return rV

funcDef safeDtoC with d1:D
return if( empty( d1), "", DtoC( d1))


funcDef chkAWisc with g:O, aWisc:A
if allTrim( g:varGet) == allTrim( aWisc[ 2])
   messageB( "��������஢���� ���� ������㯭� !!!")
   return .F.
endif
return .T.

funcDef gszMustSchet ;
  with intDoc:C, oldSch1:C, oldSch2:C, sch1:C, sch2:C ;
  local rV := .T.
if oldSch1 <> sch1 .OR. oldSch2 <> sch2
   if WISC ->( !dbSeek( padR( intDoc, 20) + GSZ_MUST_SCHET))
      rV := WISC ->( mAppend())
      WISC ->schet  := intDoc
      WISC ->type   := GSZ_MUST_SCHET
   else
      rV := WISC ->( mRLock())
   endif
   if rV
      WISC ->buffer := sch1 + ";" + sch2
      WISC ->( netUnLock())
   endif
endif
return rV

funcDef getIncomSchet ;
  with clientCode:C, sch1:C, sch2:C, rec1:N, rec2:N ;
  local arr

sch1 := sch2 := Space( 20)
rec1 := rec2 := 0
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 1))
if CONTRACT ->( dbSeek( clientCode + GSZ_CODE_2))
   WISC ->( baseSavePos())
   if WISC ->( dbSeek( padR( CONTRACT ->docNumb, 20) + GSZ_MUST_SCHET))
      arr := CtoA( WISC ->buffer, ";")
      if ACC ->( dbSeek( arr[ 1])) then sch1 := arr[ 1];rec1 := ACC ->( recNo())
      if ACC ->( dbSeek( arr[ 2])) then sch2 := arr[ 2];rec2 := ACC ->( recNo())
   else
      arr := CtoA( CONTRACT ->mBuffer, ";")
      if len( arr) >= 6
         arr  := CtoA( arr[ 6], ",")
         ACC ->( baseSavePos())
         ACC ->( dbSetOrder( 1))
         if ACC ->( dbSeek( arr[1])) then sch1 := arr[1];rec1:= ACC ->( recNo())
         if ACC ->( dbSeek( arr[2])) then sch2 := arr[2];rec2:= ACC ->( recNo())
         ACC ->( baseRestPos())
      endif
   endif
   WISC ->( baseRestPos())
endif
CONTRACT ->( baseRestPos())
return .T.


funcDef gszSaleGSZ ;
   local rV, gets, aName, aCode, aBuf, curOwner, curStr, arr, ptr1, cnt1,;
         ptr2, cnt2, ptr3, cnt3, tot1, tot2, tot3, tot, i, aCnt, totPtr ,;
         seria, aLimit, aLimCodes, intDoc, s1, s2, s3, canIntDoc, docName,;
         useDogovor, hasanDate, cnt2Print


useDogovor := confUseGSZDogovor() .AND. !confUseAgent()
docName    := if( useDogovor, "GS12B", "GS12A")
cnt2Print  := if( useDogovor, 1, 2)

assGetInfo( GSZ_CODE_2, @aCode, @aName, @aBuf)
aAdd( aCode, NIL); aIns( aCode, 1)
aAdd( aName, NIL); aIns( aName, 1)
aAdd( aBuf,  NIL); aIns( aBuf,  1)
aCode[ 1] := Space(  4)
aName[ 1] := Space( 30)
aBuf[  1] := Space( 80)
if len( aCode) <= 1
   messageB( "��� ���ଠ樨 �� ��������� !!!")
   return .F.
endif
if DtoS( setCurDate()) <> setupWDate()
   messageB( "��� ���ଠ樨 � ���� !!!")
   return .F.
endif

for ptr1 := 1 to len( aName) do aName[ ptr1] := left( aName[ ptr1], 30)

/* 28.10.99
ptr1 := 2
while ptr1 <= len( aName)
  if gszSale( aBuf[ ptr1], 1) > 0
     ptr1++
  else
     aDel( aName, ptr1); aSize( aName, len( aName) - 1)
     aDel( aCode, ptr1); aSize( aCode, len( aCode) - 1)
     aDel( aBuf,  ptr1); aSize( aBuf,  len( aBuf ) - 1)
  endif
enddo
*/

cnt1 := cnt2 := cnt3 := 0
ptr1 := ptr2 := ptr3 := 1
tot1 := tot2 := tot3 := tot := 0

aLimit    := {}
aLimCodes := {}
aCnt      := { 5, 8, 11}
totPtr    := 13
intDoc    := gszGetSale( CLIENTS ->code, aCode, @aLimit, @aLimCodes)
gszCorrectWisc( intDoc, @aLimit, @aLimCodes)
if empty( intDoc)
   messageB( "� ������� ������ ��� ��� ���� !!!")
   canIntDoc := .T.
   return .F.  // ���������஢��� �����, �⮡� ।���஢��� intDoc
else
   canIntDoc := .F.
endif

curOwner := padR( cliFullName(), 35)
gets := {{ "�த��� ������権 ���㤠��⢥����� ���客��� �����"        },;
         { "���������������������������������������������������"        },;
         { "�������� ������権 :", {|| curOwner  },,{||.F.}             },;
         { "���㬥��           :", {|| cliDocum()},,{||.F.}             },;
         { "��� ����          :", block( intDoc),, {||canIntDoc}       },;
         { "------------------------------------------------------"     },;
         { "�த��� ������権"                                          },;
         { "����/���                 ������⢮             �⮨�����" },;
         { "", block( ptr1), aName,{|g| s1 := g:varGet, .T.}                            ,;
       {|| chkName( s1, aName, aCode, aBuf, 4, aCnt,totPtr, .F.)}},;
         { "*           ", block( cnt1), "999999",,                             ;
           {|g| chkDecSum( g, aLimit, aLimCodes, aCode, aName, aBuf, aCnt, totPtr, 1)}},;
         { "*         ", block( tot1),"99999999.99",{|| .F.}               },;
         { "", block( ptr2), aName,{|g| s2 := g:varGet, !useDogovor}                             ,;
       {|| chkName( s2, aName, aCode, aBuf, 7, aCnt,totPtr, .F.)}},;
         { "*           ", block( cnt2), "999999",{||!useDogovor},                             ;
           {|g| chkDecSum( g, aLimit, aLimCodes, aCode, aName, aBuf, aCnt, totPtr, 2)}},;
         { "*         ", block( tot2),"99999999.99",{|| .F.}               },;
         { "", block( ptr3), aName,{|g| s3 := g:varGet, !useDogovor}                             ,;
       {|| chkName( s3, aName, aCode, aBuf, 10, aCnt,totPtr, .F.)}},;
         { "*           ", block( cnt3), "999999",{|| !useDogovor},                             ;
           {|g| chkDecSum( g, aLimit, aLimCodes, aCode, aName, aBuf, aCnt, totPtr, 3)}},;
         { "*         ", block( tot3),"99999999.99",{|| .F.}               },;
         { "�⮣�                                              : ",;
                                       block( tot ),"99999999.99",{|| .F.}}  }
useDec( .T.)
if ( rV := getUp( 2, 2, gets) )
   useDec( .F.)
   if WISC ->( mAppend())
      WISC ->schet  := intDoc
      WISC ->type   := GSZ_SALE_WISC
      WISC ->buffer := intDoc                                   + ";" + ;
       aCode[ ptr1] + "," + nStr( cnt1) + "," + Str( gszSale( aBuf[ ptr1], cnt1), 15, 2) ;
           + "," + safeDtoC( gszDateSale( aBuf[ ptr1], cnt1)) + ";" + ;
       aCode[ ptr2] + "," + nStr( cnt2) + "," + Str( gszSale( aBuf[ ptr2], cnt2), 15, 2) ;
           + "," + safeDtoC( gszDateSale( aBuf[ ptr2], cnt2)) + ";" + ;
       aCode[ ptr3] + "," + nStr( cnt3) + "," + Str( gszSale( aBuf[ ptr3], cnt3), 15, 2) ;
           + "," + safeDtoC( gszDateSale( aBuf[ ptr3], cnt3)) + ";" + ;
           nStr( tot )  + ";"
      WISC ->( netUnLock())
   else
      messageB( "��������� �� ��ଫ��� !!!")
   endif

   if cnt1 > 0
      seria := {}
      hasanDate := if( empty( gszDateSale( aBuf[ ptr1], cnt1)),;
        gszDateKef( aBuf[ ptr1]), gszDateSale( aBuf[ ptr1], cnt1))
      if useDogovor
         getSeria22Dog( @seria, aName[ ptr1], gszNominal( aBuf[ ptr1]), cnt1,;
         gszSale( aBuf[ ptr1], cnt1), gszDateSale( aBuf[ ptr1], cnt1),;
         gszDateKef( aBuf[ ptr1]), gszBufSeria( aBuf[ ptr1]), 78)
      else
         getSeria2( @seria, aName[ ptr1], gszNominal( aBuf[ ptr1]), cnt1,;
         gszSale( aBuf[ ptr1], cnt1), gszDateSale( aBuf[ ptr1], cnt1),;
         gszDateKef( aBuf[ ptr1]), gszBufSeria( aBuf[ ptr1]), 78)
         addStr( seria, "- �� �㬬� " + getS( tot1), 78)
      endif
      servDummy( {{ "@uprfilial",  getUprFilial()      },;
                  { "@uprdover",   getUprDover()       },;
                  { "@doggszname", getDoverName()      },;
                  { "@seria",      seria               },;
                  { "@summa",      nStr( tot1)         },;
                  { "@hasandate",  hasanDate           },;
                  { "@rsumma",     rSumma(tot1)        },;
                  { "@depo",       intDoc              },;
                  { "@intdoc",     confOGSZDoc()       }})
      printOpen( docName, cnt2Print)
   endif

   if cnt2 > 0
      seria := {}
      getSeria2( @seria, aName[ ptr2], gszNominal( aBuf[ ptr2]), cnt2,;
         gszSale( aBuf[ ptr2], cnt2), gszDateSale( aBuf[ ptr2], cnt2),;
         gszDateKef( aBuf[ ptr2]), gszBufSeria( aBuf[ ptr2]), 78)
      addStr( seria, "- �� �㬬� " + getS( tot2), 78)
      servDummy( {{ "@seria", seria}, { "@depo", intDoc}})
      printOpen( "GS12A")
   endif

   if cnt3 > 0
      seria := {}
      getSeria2( @seria, aName[ ptr3], gszNominal( aBuf[ ptr3]), cnt3,;
         gszSale( aBuf[ ptr3], cnt3), gszDateSale( aBuf[ ptr3], cnt3),;
         gszDateKef( aBuf[ ptr3]), gszBufSeria( aBuf[ ptr3]), 78)
      addStr( seria, "- �� �㬬� " + getS( tot3), 78)
      servDummy( {{ "@seria", seria}, { "@depo", intDoc}})
      printOpen( "GS12A")
   endif

   servDummy( {})
endif
useDec( .F.)
return rV

static funcDef gszGetSale ;
   with code:C, aCode:A, aLimit:A, aLimCodes:A ;
   local arr, curCode, ptr, cnt, curStr, ptr2, intDoc := "", i

CONTRACT ->( baseSavePos())
aLimit    := {}
aLimCodes := {}
CONTRACT ->( dbSetOrder( 1))
CONTRACT ->( dbSeek( code + GSZ_CODE_2))
intDoc := CONTRACT ->docNumb
/*
while CONTRACT ->code == code .AND. CONTRACT ->code2 == GSZ_CODE_2 .AND. ;
      CONTRACT ->( !eof())
  if CONTRACT ->status == 1 .AND. !empty( CONTRACT ->buffer)
     intDoc := CONTRACT ->intDoc
     curStr := CtoA( CONTRACT ->mBuffer, ";")
     for i  := 2 to 4
         arr  := CtoA( curStr[ i], ",")
         ptr  := aScan( aCode, {|x| allTrim( arr[ 1]) == allTrim( x )})
         cnt  := val( arr[ 2])
         ptr2 := aScan( aLimCodes,{|x| allTrim( x) == allTrim( aCode[ ptr])})
         if ptr2 = 0
            aAdd( aLimCodes, arr[ 1])
            aAdd( aLimit, 0); ptr2 := len( aLimit)
         endif
         aLimit[ ptr2] += cnt
     next
  endif
  skip 1 alias CONTRACT
enddo
*/
CONTRACT ->( baseRestPos())
return intDoc

static funcDef chkGszDay with g:O, aWisc:A, ptr:N ;
  local rV := .T.

if allTrim( M ->getList[ ptr]:varGet) == allTrim( aWisc[ 3])
   rV := .F.
endif
return rV


static funcDef gszCorrectWisc ;
   with intDoc:C, aLimit:A, aLimCodes:A ;
   local arr, curStr, i, intD, cnt, code, ptr

intD := padR( intDoc, 20)
if wiscSeek( intD, GSZ_SALE_WISC)
   while WISC ->schet == intD .AND. WISC ->type == GSZ_SALE_WISC .AND. ;
         WISC ->( !eof())
     for i := 2 to 4
       curStr := CtoA(  WISC ->buffer, ";")
       cnt    := val(   CtoA( curStr[ i], ",")[ 2])
       code   := CtoA(  curStr[ i], ",")[ 1]
       ptr    := aScan( aLimCodes, {|x| allTrim(x) == allTrim(code)})
       if ptr > 0
          aLimit[ ptr] -= cnt
          aLimit[ ptr] := max( 0, aLimit[ ptr])
       endif
     next
     skip 1 alias WISC
   enddo
endif

return NIL


static funcDef getOGSZDocNumb ;
  local rV, recNo, code
CONTRACT ->( dbSkip( 0))
rV    := CONTRACT ->docNumb
recNo := CONTRACT ->( recNo())
code  := CONTRACT ->code
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 1))
if CONTRACT ->( dbSeek( code + GSZ_CODE_2))
   while CONTRACT ->code  == code .AND. CONTRACT ->code2 == GSZ_CODE_2 .AND. ;
         CONTRACT ->( !eof())
     if CONTRACT ->( recNo()) <> recNo
        rV := CONTRACT ->docNumb
        exit
     endif
     skip 1 alias CONTRACT
   enddo
endif
CONTRACT ->( baseRestPos())
return if( empty( rV),  subStr( CONTRACT ->intDoc, 3), rV)


static funcDef chkPaym with g:O
if allTrim( g:varGet) == "�� ����"
   messageB( "���⥦� �� ���� ���� �� �ਭ������� !!!")
   return .F.
endif
if len( M ->getList) > 13
   if !empty( M ->getList[14]:varGet) .AND. allTrim(g:varGet) <> "�������筮"
      M ->getList[14]:varPut( Space( len( M ->getList[14]:varGet)))
      M ->getList[14]:display()
   endif
endif
return .T.

static funcDef chkFrom with g:O, aFrom1:A, aFrom2:A, totPtr:N, useRequest:L ;
   local ptr, rest, curS

if empty( allTrim( g:varGet)) .AND. ;
   allTrim( M ->getList[ totPtr + 1]: varGet) == "�������筮" .AND. ;
   ( lastKey() == K_DOWN .OR. lastKey() == K_ENTER )
   messageB( "�� �� ��ࠫ� ��� ...")
   return .F.
endif
if allTrim( M ->getList[ totPtr + 1]: varGet) <> "�������筮"
   g:varPut( Space( len( g:varGet)))
   return .T.
endif
if ( ptr := aScan( aFrom1, {|x| allTrim( x) == allTrim( g: varGet)})) >0 .AND.;
   !empty( g:varGet) .AND. ( lastKey() = K_DOWN .OR. lastKey() = K_ENTER)
   rest  := sumRest( aFrom2[ ptr, 2], setCurDate())
   if rest < M ->getList[ totPtr]: varGet .AND. !useRequest
      messageB( "���⮪ �� ���� ������ " + lTrim( mStr( rest)) + "!!!" )
      return .F.
   endif
   if len( aFrom2[ ptr]) > 2
      TRUST ->( baseSavePos())
      TRUST ->( dbGoTo( aFrom2[ ptr, 3]))
      chckTrustDate( setCurDate(), @curS)
      if TRUST ->type_1 = 2
         if M ->getList[ totPtr]: varGet > curS
            errLog( "�� ����७���� N "     + TRUST ->number + ;
                   " ����� ���� �� ����� " + mStr( curS))
            TRUST ->( baseRestPos())
            return .F.
         endif
      endif
      TRUST ->( baseRestPos())
   endif
endif
return .T.

static funcDef chkSum ;
   with g:O, aCode:A, aName:A, aBuf:A, aCnt:A, totPtr:N, useRequest:L ;
   local rV := .T., tot, tmpSum := 0

if g:varGet < 0
   messageB( "�㬬� ������ ���� ������������� !!!")
   return .F.
endif

totalSum( aName, aCode, aBuf, aCnt, totPtr, useRequest)

return rV

static funcDef totalSum ;
   with aName:A, aCode:A, aBuf:A, aCnt:A, totPtr:N, useRequest:L ;
   local i, ptr, tot := 0.00, curSeria, tmpSum, cnt, mode
   for i := 1 to len( aCnt)
       curSeria := M ->getList[ aCnt[ i] - 1]:varGet
       ptr      := aScan( aName, {|x| allTrim( x ) == allTrim( curSeria)})
       if empty( aCode[ ptr])
          M ->getList[ aCnt[ i]]:varPut( 0)
          M ->getList[ aCnt[ i]]:display()
          ptr := 0
       endif
       if ptr > 0
          cnt    := M ->getList[ aCnt[ i]]:varGet
          if kasLimit( aCode[ ptr]) < cnt .AND. !useRequest
             cnt := 0
             if alarm( ;
                { "����� �� �������� " + rTrim( aName[ ptr])      ,;
                  "��⠭����� " + Str( kasLimit( aCode[ ptr]),5, 0) ,;
                  "�� ��⠥��� �த��� " +                          ;
                   nStr(M ->getList[ aCnt[ i]]:varGet), ""          ,;
                  "�� ��� " }                                    ,;
                { "��祣� �� �த�����"                             ,;
                  "�த��� ���⮪ " + Str( kasLimit( aCode[ ptr]),5, 0) }) = 2
                cnt := kasLimit( aCode[ ptr])
             endif
             M ->getList[ aCnt[ i]]:varPut( cnt)
             M ->getList[ aCnt[ i]]:display()
          endif
          tmpSum := gszBuy( aBuf[ ptr]) * M ->getList[ aCnt[ i]]:varGet
          tot    += tmpSum
          M ->getList[ aCnt[ i] + 1]:varPut( tmpSum)
          M ->getList[ aCnt[ i] + 1]:display()
       endif
   next
   M ->getList[ totPtr]:varPut( tot)
   M ->getList[ totPtr]:display()
return tot



static funcDef chkDecSum ;
   with g:O, aLimit:A, aLimCodes:A, aCode:A, aName:A, aBuf:A,;
        aCnt:A, totPtr:N, i:N ;
   local rV := .T., tot, tmpSum := 0, curSeria, ptr, p2

if g:varGet < 0
   messageB( "�㬬� ������ ���� ������������� !!!")
   return .F.
endif

curSeria := M ->getList[ aCnt[ i] - 1]:varGet
ptr      := aScan( aName,   {|x| allTrim( x) == allTrim( curSeria)})
if ptr > 0
   if g:varGet > 0
      p2 := gszSale( aBuf[ ptr], g:varGet)
      if p2 = 0
         messageB( "����饭� �த��� ��⮩ " + DtoC( gszDateSale( aBuf[ ptr], g:varGet)))
         return .F.
      endif
   endif
   ptr := aScan( aLimCodes, {|x| allTrim( x) == allTrim( aCode[ ptr])})
   if ptr >0 .AND. g:varGet > aLimit[ ptr]
      messageB( "� ������ �㯫��� "    + nStr( aLimit[ i]) + " ������権." + ;
                "�� ��⠥��� �த��� " + nStr( g:varGet))
   endif
endif
totalDecSum( aName, aCode, aBuf, aCnt, totPtr)

return rV

static funcDef totalDecSum ;
   with aName:A, aCode:A, aBuf:A, aCnt:A, totPtr:N ;
   local i, ptr, tot := 0.00, curSeria, tmpSum, cnt, mode
   for i := 1 to len( aCnt)
       curSeria := M ->getList[ aCnt[ i] - 1]:varGet
       ptr      := aScan( aName, {|x| allTrim( x ) == allTrim( curSeria)})
       if empty( aCode[ ptr])
          M ->getList[ aCnt[ i]]:varPut( 0)
          M ->getList[ aCnt[ i]]:display()
          ptr := 0
       endif
       if ptr > 0
          cnt    := M ->getList[ aCnt[ i]]:varGet
          tmpSum := gszSale( aBuf[ ptr], cnt) * cnt
          tot    += tmpSum
          M ->getList[ aCnt[ i] + 1]:varPut( tmpSum)
          M ->getList[ aCnt[ i] + 1]:display()
       endif
   next
   M ->getList[ totPtr]:varPut( tot)
   M ->getList[ totPtr]:display()
return tot



static funcDef bufGetSumma with aBufStr:C, ptr:N ;
   local rV := "", arr
   arr := CtoA( aBufStr, ";")
   if len( arr) >= ptr then rV := arr[ ptr]
return rV

funcDef gszNominal with aBufStr:C local rV
return val( bufGetSumma( aBufStr, 1))

funcDef gszSale with aBufStr:C, cnt:N local rV, ptr
do case
   case cnt <= 1000 do ptr := 4
   case cnt >= 5001 do ptr := 8
   otherwise        do ptr := 6
endcase
return val( bufGetSumma( aBufStr, ptr))

funcDef gszDateSale with aBufStr:C, cnt:N local rV, ptr
do case
   case cnt <= 1000 do ptr := 5
   case cnt >= 5001 do ptr := 9
   otherwise        do ptr := 7
endcase
return CtoD( bufGetSumma( aBufStr, ptr))

funcDef gszBuy with aBufStr:C local rV
return val( bufGetSumma( aBufStr, 2))

funcDef gszPct with aBufStr:C local rV
return val( bufGetSumma( aBufStr, 3))

funcDef gszDateKef with aBufStr:C local rV
return CtoD( bufGetSumma( aBufStr, 10))

funcDef gszBufSeria with aBufStr:C local rV
return bufGetSumma( aBufStr, 11)


static funcDef chkName ;
   with preS:C, aName:A, aCode:A, aBuf:A, curPtr:N, aCnt:A, totPtr:N, useR:L ;
   local tot := 0.00, i

if len( M ->getList) >= totPtr
   for i := 1 to len( aCnt)
     if aCnt[ i] - 1 <> curPtr                .AND. ;
        !empty( M ->getList[ curPtr]:varGet ) .AND. ;
        M ->getList[ curPtr]:varGet == M ->getList[ aCnt[ i] - 1]:varGet
      messageB( "����� ��� ������権 㦥 ����...")
      clear typeAHead
      return .F.
     endif
   next
endif

if len( M ->getList) >= totPtr .AND. preS <> M ->getList[ curPtr]:varGet

   for i := 1 to len( aCnt)
     if aCnt[ i] - 1 <> curPtr                .AND. ;
        !empty( M ->getList[ curPtr]:varGet ) .AND. ;
        M ->getList[ curPtr]:varGet == M ->getList[ aCnt[ i] - 1]:varGet
      messageB( "����� ��� ������権 㦥 ����...")
      clear typeAHead
      return .F.
     endif
   next
   M ->getList[ curPtr + 1]:varPut( 0)
   M ->getList[ curPtr + 1]:display()
   M ->getList[ curPtr + 2]:varPut( 0)
   M ->getList[ curPtr + 2]:display()
   if useDec()
      tot := totalDecSum( aName, aCode, aBuf, aCnt, totPtr)
   else
      tot := totalSum( aName, aCode, aBuf, aCnt, totPtr, useR)
   endif

endif
return .T.

autofunction setvalue useDec init .F.

static funcDef getCurDocum with recNo:N local rV
CLIENTS ->( baseSavePos())
CLIENTS ->( dbGoTo( recNo))
rV := cliDocum()
CLIENTS ->( baseRestPos())
return left( rV, 50)

static funcDef getAFrom ;
   with  arr1:AU, arr2:AU, useRequest:L;
   local clientName, curD

accPrizList( "15 1", @arr1, @arr2)
aAdd( arr1, NIL); aIns( arr1, 1)
aAdd( arr2, NIL); aIns( arr2, 1)
arr1[ 1] := Space( 20)
arr2[ 1] := {}
curD     := setCurDate()

if useRequest then return NIL

TRUST ->( baseSavePos())
TRUST ->( dbSetOrder(4))
ACC   ->( baseSavePos())
ACC   ->( dbSetOrder( 1))

clientName := upper( padR( cliFullName(), 45))
TRUST ->( dbSeek( clientName))
while TRUST ->fio == clientName .AND. CLIENTS ->( !eof())
  if TRUST ->pasNum == CLIENTS ->pasNum .AND. ;
     curD  >= TRUST ->dateBeg .AND. curD <= TRUST ->dateEnd .AND. ;
     TRUST ->status == 1
     if ACC ->( dbSeek( TRUST ->schet))
        if left( ACC ->PRIZ, 4) == "15 1" .AND. ACC ->typeOf <> "02"
           aAdd( arr1, "��� "     + splitSSchet( ACC ->schet) + ;
                       " ���⮪ " + mStr( sumRest( ACC ->schet, setCurDate() )))
           aAdd( arr2, { ACC ->( recNo()), ACC ->schet, TRUST ->( recNo()) })
        endif
     endif
  endif
  skip 1 alias TRUST
enddo

ACC   ->( baseRestPos())
TRUST ->( baseRestPos())

return NIL

* NEW OBJECT FILE
funcDef changeOwner ;
   with g:O, ownerRec:N, rec1:N, rec2:N, useRequest:LU ;
   local bUp, bDn, bTop, bBtm, aH, bH, aC, fk, aF, aStr, cStr,  ;
         userDefineKeys, oB, key, Sel, curCode, bEsc, sch1, sch2

DEFAULT useRequest TO .F.
curCode := CLIENTS ->code

Sel  := .F.
bUp  := {|| cliUp()    };   bDn  := {|| cliDown()  }
bTop := {|| cliTop()   };   bBtm := {|| cliBottom()}
aC   := {{ "", {|| cliSurName() + " " + cliName() + " " + cliSName() }}}
aF   := {{ "������",       funBlock( cliIndex  ),,{|| .F.}} ,;
         { "* ��p��",      funBlock( cliCity   ),,{|| .F.}} ,;
         { "����",        funBlock( cliStreet ),,{|| .F.}} ,;
         { "*���",         funBlock( cliHouse  ),,{|| .F.}} ,;
         { "*��p���",      funBlock( cliBlock  ),,{|| .F.}} ,;
         { "*��.",         funBlock( cliFlat   ),,{|| .F.}} ,;
         { "���㬥�� ",    {|| cliDocum()      },,{|| .F.}}}

aStr := { "          ", "<�������> ", "          ", "<��ᯮ��> "}
userDefineKey := {;
  { K_ENTER, {|| Sel := .T., __KeyBoard( chr( K_ESC)) }}}
if getPrevalidate( g)
   g:SetFocus()
   while .T.
     key := Inkey( 0)
     if key = K_ENTER .OR. key = K_ESC .OR. key = K_DOWN .OR. key = K_UP .OR. ;
        key = K_PGDN  .OR. key = K_PGUP
        getApplyKey( g, key)
     endif
     if g:exitState <> GE_NOEXIT .AND. key <> K_SPACE
        if getPostValidate( g) then exit
        g:exitState := GE_NOEXIT
     else
        CLIENTS ->( baseSavePos())
        CLIENTS ->( dbSetOrder( 2))
        CLIENTS ->( dbGoTo( ownerRec))
        oB := browseFtStartUp( 2, 2,, aC, bUp, bDn, bTop, bBtm, bH, fk, cStr, aF,15)
        CLIENTS ->( browseFiz( 2, 2, oB, aH, aC,,,,fk, userDefineKeys, aStr))
        browseFtExit()
        if Sel
           if !useRequest .AND. len( M ->getList) >= 16
              sch1 := sch2 := ""
              getIncomSchet( CLIENTS ->code, @sch1, @sch2, @rec1, @rec2)
              M ->getList[ 15]:varPut( sch1)
              M ->getList[ 15]:buffer := sch1
              M ->getList[ 15]:Changed:= .T.
              M ->getList[ 15]:display()

              M ->getList[ 16]:varPut( sch2)
              M ->getList[ 16]:buffer := sch2
              M ->getList[ 16]:Changed:= .T.
              M ->getList[ 16]:display()
           endif
           if !( useRequest .AND. curCode <> CLIENTS ->code )
              g:varPut( padR( cliFullName(), 35))
              g:buffer := padR( cliFullName(), 35)
              g:changed := .T.
              g:display()
              if len( M ->getList) >= 2
                 M ->getList[ 2]:varPut( cliDocum())
                 M ->getList[ 2]:buffer  := cliDocum()
                 M ->getList[ 2]:changed := .T.
                 M ->getList[ 2]:Display()
              endif
              ownerRec := CLIENTS ->(recNo())
           else
              if useRequest .AND. curCode <> CLIENTS ->code
                 messageB( "�� �� ����� ��ନ�� ����祭�� �� ��㣮� ��� !!!")
              endif
           endif
        endif
        clear TypeAHead
        CLIENTS ->( baseRestPos())
        g:exitState := GE_ENTER
     endif
   enddo
   g:KillFocus()
endif
// setKey( K_ESC, bEsc)
return NIL


* NEW OBJECT FILE
funcDef viewOwner with code:C, ownerRec:N ;
   local bUp, bDn, bTop, bBtm, aH, bH, aC, fk, aF, aStr, cStr,  ;
         userDefineKeys, oB, key, Sel := .F., curCode, bEsc, sch1, sch2

curCode  := CLIENTS ->code
code     := ""
ownerRec := 0

Sel  := .F.
bUp  := {|| cliUp()    };   bDn  := {|| cliDown()  }
bTop := {|| cliTop()   };   bBtm := {|| cliBottom()}
aC   := {{ "", {|| cliSurName() + " " + cliName() + " " + cliSName() }}}
aF := {{ "������", {|| padR( cliFio(), 45) }} ,;
       { "������",       funBlock( cliIndex  ),,{|| .F.}} ,;
       { "* ��p��",      funBlock( cliCity   ),,{|| .F.}} ,;
       { "����",        funBlock( cliStreet ),,{|| .F.}} ,;
       { "*���",         funBlock( cliHouse  ),,{|| .F.}} ,;
       { "*��p���",      funBlock( cliBlock  ),,{|| .F.}} ,;
       { "*��.",         funBlock( cliFlat   ),,{|| .F.}} ,;
       { "��ᯮp� �p.", funBlock( cliPasSer1),,{||.F. }} ,;
       { "* - ",         funBlock( cliPasSer2),,{|| .F.}} ,;
       { "* N ",         funBlock( cliPasNum ),,{|| .F.}} ,;
       { "�뤠�",        funBlock( cliPasGiv ),,{|| .F.}}}
 
aStr := { "          ", "<�������> ", "          ", "<��ᯮ��> "}
userDefineKey := {;
  { K_ENTER, {|| Sel := .T., __KeyBoard( chr( K_ESC)) }}}
CLIENTS ->( baseSavePos())
CLIENTS ->( dbSetOrder( 2))
CLIENTS ->( dbGoTop())
fk := {, {}}
oB := browseFtStartUp( 2, 2,, aC, bUp, bDn, bTop, bBtm, bH, fk, cStr, aF)
CLIENTS ->( browseFiz( 2, 2, oB, aH, aC,,,,fk, userDefineKeys, aStr))
browseFtExit()
if Sel
  ownerRec := CLIENTS ->(recNo())
  code     := CLIENTS ->code
endif
clear TypeAHead
CLIENTS ->( baseRestPos())

return Sel



funcDef changeSchet with g:O, curRec:N ;
   local bUp, bDn, bTop, bBtm, aH, bH, aC, fk, aF, aStr, cStr,  ;
         userDefineKeys, oB, key, arr1, arr2, ptr, code, Sel, bEsc

bEsc := SetKey( K_ESC, NIL)

bUp  := {|| cliUp()    };   bDn  := {|| cliDown()  }
bTop := {|| cliTop()   };   bBtm := {|| cliBottom()}
aC   := {{ "", {|| cliSurName() + " " + cliName() + " " + cliSName() }}}
aF   := {{ "������",       funBlock( cliIndex  ),,{|| .F.}} ,;
         { "* ��p��",      funBlock( cliCity   ),,{|| .F.}} ,;
         { "����",        funBlock( cliStreet ),,{|| .F.}} ,;
         { "*���",         funBlock( cliHouse  ),,{|| .F.}} ,;
         { "*��p���",      funBlock( cliBlock  ),,{|| .F.}} ,;
         { "*��.",         funBlock( cliFlat   ),,{|| .F.}} ,;
         { "���㬥�� ",    {|| cliDocum()      },,{|| .F.}}}
Sel  := .F.
aStr := { "          ", "<�������> ", "          ", "<��ᯮ��> "}
userDefineKey := {;
  { K_ENTER, {|| Sel := .T., __KeyBoard( chr( K_ESC)) }}}
if getPrevalidate( g)
   g:SetFocus()
   while .T.
     key := Inkey( 0)
     // if key = K_ENTER .OR. key = K_ESC .OR. key = K_DOWN .OR. key = K_UP .OR. ;
     //    key = K_PGDN  .OR. key = K_PGUP
     if ( key = K_ENTER .OR. key = K_ESC .OR. key = K_DOWN .OR. key = K_UP .OR. ;
          key = K_PGDN  .OR. key = K_PGUP ) .AND. !empty( g:varGet)
        getApplyKey( g, key)
     endif
     if g:exitState <> GE_NOEXIT .AND. key <> K_SPACE
        if getPostValidate( g) then exit
        g:exitState := GE_NOEXIT
     else
        CLIENTS ->( baseSavePos())
        if curRec > 0
           ACC ->( baseSavePos())
           ACC ->( dbGoTo( curRec))
           code := ACC ->code
           ACC ->( baseRestPos())
           CLIENTS ->( dbSetOrder( 1))
           CLIENTS ->( dbSeek( code))
        endif
        CLIENTS ->( dbSetOrder( 2))
        oB := browseFtStartUp( 2, 2,, aC, bUp, bDn, bTop, bBtm, bH, fk, cStr, aF,15)
        CLIENTS ->( browseFiz( 2, 2, oB, aH, aC,,,,fk, userDefineKeys, aStr))
        browseFtExit()
        if Sel
           accPrizList( "15 1", @arr1, @arr2)
           addCards( @arr1, @arr2)
           ptr := if( len( arr1) > 1, popUp( 10, 5, arr1), 1)
           if !empty( arr1) .AND. ptr > 0
              curRec   := arr2[ ptr, 1]
              g:varPut(   arr2[ ptr, 2])
              g:buffer := arr2[ ptr, 2]
              g:changed := .T.
              g:display()
           endif
        else
           // messageB( "�� ��ࠫ� ��� ? �� �� �������� �� �⮬...")
        endif
        clear TypeAHead
        CLIENTS ->( baseRestPos())
        g:exitState := GE_ENTER
     endif
   enddo
   g:KillFocus()
endif
SetKey( K_ESC, bEsc)
return NIL


* NEW OBJECT FILE
funcDef gszPrint with newContract:L ;
   local from, seria, arr, rV := .F., aDoc, addr, i, str, mustSave ,;
         str1, str2, arr2 := {}, arr3, fio, type, otherPerson := "",;
         mustPrint, docNumb, ptrWisc, cOplata, recN, totSumma := 0 ,;
         cTotSumma, arrDoc

arr := ctoA( CONTRACT ->mBuffer, ";")
if empty( arr)
   messageB( "��� ���ଠ樨 �� ��������� !!!")
   return .F.
endif
arr2 := ctoA( arr[ 1], ",")

docNumb   := CONTRACT ->docNumb
mustPrint := .T.
recN      := CONTRACT ->( recNo())
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 2))
if CONTRACT ->( dbSeek( docNumb))
   while CONTRACT ->docNumb == docNumb .AND. CONTRACT ->( !eof())
     if CONTRACT ->( recNo()) <> recN
        mustPrint := .F.; exit
     endif
     skip 1 alias CONTRACT
   enddo
   // mustPrint := CONTRACT ->( netSkipDown()) .AND. ;
   //    CONTRACT ->docNumb <> docNumb
endif
CONTRACT ->( baseRestPos())

if allTrim( arr2[ 1]) <> allTrim( arr2[ 2])
   CLIENTS ->( baseSavePos())
   CLIENTS ->( dbSetOrder( 1))
   CLIENTS ->( dbSeek( arr2[ 2], .F.))
   aDoc := razbivka( cliDocum(), {{ 40, "L"}, { 40, "L"}})
   addr := razbivka( cliFullAddress(), {{ 30, "L"}, { 30, "L"}})
   from := { "���� �த��� " + cliFullName(),;
             aDoc[ 1]                                  ,;
             aDoc[ 2]                                  ,;
             "����   : " + addr[ 1]                   ,;
             "          " + addr[ 2]                   ,;
             "����䮭 : " + cliPhone()                 ,;
             "���㤠��⢥��� ���ॣ�⥫�� ������ ������樨 �����-�����࣠"}
   CLIENTS ->( dbSeek( arr2[ 1], .F.))
   aDoc := razbivka( cliDocum(), {{ 40, "L"}, { 40, "L"}})
   addr := razbivka( cliFullAddress(), {{ 30, "L"}, { 30, "L"}})
   otherPerson := { cliFullName(), aDoc[ 1], aDoc[ 2],;
             "����   : " + addr[ 1]                 ,;
             "          " + addr[ 2]                 ,;
             "����䮭 : " + cliPhone(), "", ""       ,;
             "                             ______________/__________________/",;
             "                                 �������         䠬����       ",;
             "", "" }


   CLIENTS ->( baseRestPos())
else
   from := { "���� �த��� ��� ���㤠��⢥��� ���ॣ�⥫�� ������ ������樨",;
             "�����-�����࣠"}
endif
seria := getSeria()
if len( arr) >= 6 then arr3 := CtoA( arr[ 6], ",")
if len( arr3) <> 2
   messageB( "���०���� ������� CONTRACT.DBF. ������ �� 䨫��� !!!")
   str1 := str2 := ""
else
   str1 := { "�믫��� �㯮����� ��室� ",;
             "��� " + confOtdNumb() + " " + rTrim( confFilial()) + ;
             " ��. N " + arr3[ 1] }
   str2 := { "����᫥��� �।�� �� �த���/����襭�� ������権",;
             "��� " + confOtdNumb() + " " + rTrim( confFilial()) + ;
             " ��. N " + arr3[ 2]}
endif
mustSave := mustSaveOpen( .T.)
arrDoc   := CtoA( CONTRACT ->mBuffer, ";")
while .T.

   if !empty( CONTRACT ->debetAcc) 
      cOplata := "��������� ����᫥���� � ��� N " + CONTRACT ->debetAcc
   else
      cOplata := "�����묨 ���⠭��"
      if len( arrDoc) >= 7 .AND. !empty( arrDoc[ 7])
         cOplata += " " + arrDoc[ 7] + " �� " + myDtoC( CONTRACT ->dateCur)
      endif
   endif
   if empty( CONTRACT ->buffer)
      if confUseGSZDogovor()
         seria   := getSeriaDog(, @totSumma)
         cOplata := getS( totSumma)
      endif
      /*
      servDummy( {{ "@from", from }, { "@seria", seria},;
                  { "@depo", CONTRACT ->docNumb  },;
                  { "@oplata", cOplata           },;
                  { "@uprfilial",  getUprFilial() },;
                  { "@uprdover",   getUprDover()  },;
                  { "@doggszname", getDoverName() }})
      */
      cTotSumma := nStr( totSumma)
      //
      // 27.10.2000 ������� ࠧ����⥫� �� �� �� ���졥 ����殢�
      //
      if confUseGSZDogovor() .AND. !confUseAgent()
        cTotSumma := strTran( cTotSumma, ".", "-")
      endif
      servDummy( {{ "@uprfilial",  getUprFilial()      },;
                  { "@uprdover",   getUprDover()       },;
                  { "@doggszname", getDoverName()      },;
                  { "@seria",      seria               },;
                  { "@from",       from                },;
                  { "@summa",      cTotSumma           },;
                  { "@debSchet",   CONTRACT ->debetAcc },;
                  { "@rsumma",     rSumma(totSumma)    },;
                  { "@depo",       CONTRACT ->docNumb  },;
                  { "@intdoc",     CONTRACT ->intDoc   },;
                  { "@oplata",     cOplata             }})
      CLIENTS ->( baseSavePos())
      CLIENTS ->( dbSetOrder( 1))
      CLIENTS ->( dbSeek( arr2[ 1], .F.))
      if confUseGSZDogovor() 
        printOpen( "GSZDG", 1)
      else
        printOpen( "GSZ3A")
      endif
      CLIENTS ->( baseRestPos())
      SaveLastOrder( { getSavedOpen()})
      servDummy({})
   else
      seria   := getSeria3()
      ptrWisc := val( subStr( CONTRACT ->buffer, 4, 2))
      do case
         case ptrWisc == 1 do type := "�������筮"
         case ptrWisc == 3 do type := "� ���� ���� ࠧ��饭��"
         otherwise         do type := ""
      endcase
      servDummy( {{ "@docNumb",  CONTRACT ->intDoc  },;
                  { "@dateBeg",  CONTRACT ->dateCur },;
                  { "@debSchet", CONTRACT ->debetAcc},;
                  { "@type",     type               },;
                  { "@seria",    seria              },;
                  { "@curdate",  setCurDate()       },;
                  { "@fio",      cliFullName()      },;
                  { "@oplata",   cOplata            }})
      printOpen( "GSZ9B")
      SaveLastOrder( { getSavedOpen()})
      servDummy({})
   endif

   CLIENTS ->( baseSavePos())
   CLIENTS ->( dbSetOrder( 1))
   CLIENTS ->( dbSeek( arr2[ 2], .F.))
   servDummy( {{ "@str1", str1               },;
               { "@str2", str2               },;
               { "@depo", CONTRACT ->docNumb },;
               { "@other_person", otherPerson}} )
   if mustPrint
      if confUseGSZDogovor() then OK( "��⠢�� ���� ���� �㬠�� ��� ���� ������ ��������")
      printOpen( "GSZ3B")
   endif
   servDummy({})
   CLIENTS ->( baseRestPos())
   SaveLastOrder( { getSavedOpen()}, .F.)
   rV := yesNo( "���㬥��� �ᯥ�⠭� �ࠢ��쭮 ?")
   exit if rV
enddo
mustSaveOpen( mustSave)
return rV .AND. yesNo( "������ �����ᠭ� ?")

static funcDef getUprFilial
return setUpValue( "UPRFL", .F.)

static funcDef getUprDover
return setUpValue( "UPRDV", .F.)

static funcDef getDoverName
return setUpValue( "UPRNA", .F.)




function limDeleteSumma( summa, kasCode)
local opened, rv, old_area:=select()
local sum:=0

opened := select( "KASSA") >0
if opened =.F.
  rV:=baseNSOpen( kasInfo())
endif
if opened .OR. rV
    if KASSA ->( dbSeek( kasCode))
      if KASSA ->( mRLock())
        //�����塞 ���⮪ �� ����
        if summa > 0
          KASSA ->summa := KASSA ->summa - summa
        else
          KASSA ->summa := KASSA ->summa - summa
        endif
        //㬥��蠥� ᮮ⢥�. ������
        if summa > 0
          KASSA ->obK := KASSA ->obK - summa
        else
          KASSA ->obD := KASSA ->obD + summa
        endif
        rv:=summa
        KASSA ->( netUnLock())
      endif
    endif
  if opened = .F. then baseMClose( kasInfo())
  dbselectarea(old_area)
endif
return rv


static funcDef getSeria3 with curStr:CMU;
   local seria, aCode, aName, aBuf, ptr, i, arr, str, tmpS

   curStr:= if( empty(curStr), CtoA(CONTRACT ->mBuffer, ";"), CtoA(curStr,";"))
   seria := {}
   assGetInfo( GSZ_CODE_2, @aCode, @aName, @aBuf)
   for i := 2 to 4
       str := ""
       arr := CtoA( curStr[ i], ",")
       ptr := aScan( aCode, {|x| allTrim( x) == allTrim( arr[ 1])})
       if ptr > 0
          tmpS := razbivka( rTrim( aName[ ptr]), {{ 15, "L"}, { 15, "L"}})
          str  := "|" + padR( "����� ����",     20)              + ;
                  "|" + tmpS[ 1]                                  + ;
                  "|" + padL( arr[ 2] + "/ᮣ��᭮ �⮨�-�", 21) + ;
                  "|" + DtoC( CONTRACT ->dateEnd) + "|"
          aAdd( seria, str)
          if !empty( tmpS[ 2])
             str := "|" + Space( 20) + "|" + tmpS[ 2] + "|" + Space( 21) + ;
                    "|" + Space( 10) + "|"
             aAdd( seria, str)
          endif
       elseif !empty( arr[ 1])
          messageB( "��� " + curStr[ i] + " �� ������ !!!!")
          return {}
       endif
   next

return seria

static funcDef getSeria with curStr:CMU;
   local seria, aCode, aName, aBuf, ptr, i, arr, str, tmpS, tot

   curStr:= if( empty(curStr), CtoA(CONTRACT ->mBuffer, ";"), CtoA(curStr,";"))
   tot   := 0
   seria := {}
   assGetInfo( GSZ_CODE_2, @aCode, @aName, @aBuf)
   for i := 2 to 4
       str := ""
       arr := CtoA( curStr[ i], ",")
       ptr := aScan( aCode, {|x| allTrim( x) == allTrim( arr[ 1])})
       if ptr > 0
          tmpS := gszNominal( aBuf[ ptr])
          getSeria2( @seria, aName[ ptr], tmpS, val(arr[ 2]), val(arr[ 3]),;
            nil, CtoD( arr[ 4]), arr[ 5], 78)
          tot += val( arr[ 2]) * val( arr[ 3])
       elseif !empty( arr[ 1])
          messageB( "��� " + curStr[ i] + " �� ������ !!!!")
          return {}
       endif
   next
   addStr( seria, "- �� �㬬� " + getS( tot), 78)
return seria

static funcDef getSeriaDog with curStr:CMU, tot:NU;
   local seria, aCode, aName, aBuf, ptr, i, arr, str, tmpS

   curStr:= if( empty(curStr), CtoA(CONTRACT ->mBuffer, ";"), CtoA(curStr,";"))
   tot   := 0
   seria := {}
   assGetInfo( GSZ_CODE_2, @aCode, @aName, @aBuf)
   for i := 2 to 4
       str := ""
       arr := CtoA( curStr[ i], ",")
       ptr := aScan( aCode, {|x| allTrim( x) == allTrim( arr[ 1])})
       if ptr > 0
          tmpS := gszNominal( aBuf[ ptr])
          getSeria2Dog( @seria, aName[ ptr], tmpS, val(arr[ 2]), val(arr[ 3]),;
            nil, CtoD( arr[ 4]), arr[ 5], 78)
          tot += val( arr[ 2]) * val( arr[ 3])
       elseif !empty( arr[ 1])
          messageB( "��� " + curStr[ i] + " �� ������ !!!!")
          return {}
       endif
   next
return seria


static funcDef getSeria2Dog ;
   with seria:A, curName:C, tmpS:N, cnt:N, curSale:N, curD:DU,;
        dateKef:D, thisSeria:C, len:N ;
   local str
//str := "- ���� ��ॣ���஢��  �������᢮� 䨭��ᮢ �� " + thisSeria
str := Space( 45) + chr(27) + chr( 15) + rTrim( thisSeria) + chr( 27) + chr( 18)
addStr( @seria, str, 200)

//str := "- ���⥭� - �����-������ � ��� ������ 䨭��ᮢ ���������樨 �����-�����࣠"
str := chr( 0)
addStr( @seria, str, len)
str := chr( 0)
addStr( @seria, str, len)

// str := "- " + rTrim( curName) + ", �������쭠� �⮨����� " + getS( tmpS) + ","
str := Space( 14) + padR( curName,18) + Space( 18) + chr(27) + chr( 15) + ;
       getS( tmpS) + chr(27) + chr( 18)
addStr( @seria, str, len)

//if empty( curD)
//   str := "- 業� ����� ������樨 � (��⮬ �������饣� �����樥��) " + getS( curSale) + ","
//else
//   str := "- 業� ��㯠 ����� ������樨 �� " + DtoC( curD) + " " + getS( curSale) + ","
//endif

str := chr( 0)
addStr( @seria, str, len)

if empty( curD)
  str := Space( 30) + chr( 27) + chr( 15) + DtoC( dateKef) + " " + getS( curSale) + chr( 27) + chr( 18)
else
  str := Space( 30) + chr( 27) + chr( 15) + DtoC( curD) + " " + getS( curSale) + chr( 27) + chr( 18)
endif
addStr( @seria, str, 180)

//str := "- ������⢮ ������権 " + nStr( cnt) + "(" + rStrW( cnt) + ") ���"+","
str := Space( 23) + nStr( cnt) + "(" + rStrW( cnt) + ") ���"
addStr( @seria, str, len)
addStr( seria, "",  78)

return NIL


static funcDef getSeria22Dog ;
   with seria:A, curName:C, tmpS:N, cnt:N, curSale:N, curD:DU,;
        dateKef:D, thisSeria:C, len:N ;
   local str
str := Space( 45) + chr(27) + chr( 15) + rTrim( thisSeria) + chr( 27) + chr( 18)
addStr( @seria, str, 200)

str := chr( 0)
addStr( @seria, str, len)
str := chr( 0)
addStr( @seria, str, len)

str := Space( 14) + padR( curName,18) + Space( 18) + nStr( tmpS)
addStr( @seria, str, len)

if empty( curD)
  str := Space( 30) + chr( 27) + chr( 15) + DtoC( dateKef) + " " + getS( curSale) + chr( 27) + chr( 18)
else
  str := Space( 30) + chr( 27) + chr( 15) + DtoC( curD) + " " + getS( curSale) + chr( 27) + chr( 18)
endif
addStr( @seria, str, 180)

str := chr( 0)
addStr( @seria, str, len)

str := Space( 24) + nStr( cnt) + "(" + rStrW( cnt) + ") ���"
addStr( @seria, str, len)
addStr( seria, "",  78)

return NIL



static funcDef getSeria2 ;
   with seria:A, curName:C, tmpS:N, cnt:N, curSale:N, curD:DU,;
        dateKef:D, thisSeria:C, len:N ;
   local str

str := "- �������樮��� ����� �� N " + thisSeria + ","
addStr( @seria, str, len)

str := "- " + rTrim( curName) + ", �������쭠� �⮨����� " + getS( tmpS) + ","
addStr( @seria, str, len)

if empty( curD)
   // str := "- 業� ��㯠 ����� ������樨 �� "  + DtoC( dateKef) + " " + getS( curSale) + ","
   str := "- 業� ����� ������樨 � ��� �� "  + DtoC( dateKef) + " " + getS( curSale) + ","
else
   str := "- 業� ����� ������樨 � ��� �� "  + DtoC( curD) + " " + getS( curSale) + ","
   str := "- 業� ��㯠 ����� ������樨 �� " + DtoC( curD) + " " + getS( curSale) + ","
endif
addStr( @seria, str, 78)

str := "- ������⢮ ������権 " + nStr( cnt) + "(" + rStrW( cnt) + ") ���"+","
addStr( @seria, str, len)
addStr( seria, "",  78)

return NIL

funcDef addStr with seria:A, str:C, len:N
if len( str) >= len
   str := razbivka( str, {{ len - 2, "L"}, { len - 2, "L"}})
   aAdd( seria, str[ 1]); aAdd( seria, "  " + str[ 2])
else
   aAdd( seria, str)
endif
return NIL


// funcDef getS with tmpS:N
// return lTrim( mStr( tmpS)) + "("  + rSumma( tmpS) + ")"

funcDef gszClose local rV := .F.
if !empty( CONTRACT ->buffer)
   if WISC ->( rV  := mAppend())
      WISC ->schet := CONTRACT ->intDoc
      WISC ->type  := GSZ_CLOSE_WISC
      WISC ->( netUnLock())
   endif
else
  messageB( "�� ࠧ���� ����祭��, � �� �������筮� !!!")
endif
return rV

funcDef gszIzmPrint with cReason:AU
DEFAULT cReason TO {;
   "1. _____________    ___________________________   ____________________________",;
   "",;
   "2. _____________    ___________________________   ____________________________",;
   "",;
   "3. _____________    ___________________________   ____________________________"}



servDummy( {{ "@depo", CONTRACT ->docNumb},;
            { "@reason", cReason         }})
printOpen( "GSZ13")
servDummy( {})
return NIL

funcDef gszWorks local i, arr
if CONTRACT ->code2 <> "73"
   messageB( "��� ०�� ����㯥� ⮫쪮 ��� ���� !")
   return NIL
endif
arr := { "������ �� ��������� ������ ��������"}
i := popUp( 5, 5, arr)
do case
   case i = 1 do gszIzmPrint()
endcase
return NIL

* NEW OBJECT FILE
funcDef escBlock local old
old := setKey( K_ESC, NIL)
errLog( { "�� ������ ���⢥न�� �� ���� ����� ����⨥� ������ ENTER"})
setKey( K_ESC, old)
return NIL

funcDef setEsc local old
  old := setKey( K_ESC, {|| escBlock()})
return old

funcDef delEsc with old:BU
setKey( K_ESC, old)
return NIL

//
// ����� ���� 2
//
funcDef gszPrint2 local i, str, aCode, aName, aBuf

if DtoS( setCurDate()) <> setupWDate()
   messageB( "��� ���ଠ樨 � ���� !!!")
   return .F.
endif

assGetInfo( GSZ_CODE_2, @aCode, @aName, @aBuf)
if len( aCode) < 1
   messageB( "��� ���ଠ樨 �� ��������� !!!")
   return .F.
endif


str := DtoC( setCurDate()) + Space( 20) + "��� " + confOtdNumb() + endl() + ;
       "       �������� �� ���"            + endl() + ;
       "+-------------------------------+" + endl() + ;
       "| ����� ������� | ��� ������� |" + endl() + ;
       "+-------------------------------+" + endl()
//       0123456789012345 01234567890123
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 3))
CONTRACT ->( dbSeek( "73"))
statStartUp( "���� ������஢ �� ���")
while CONTRACT ->code2 == "73" .AND. CONTRACT ->( !Eof())
  if CONTRACT ->dateCur == setCurDate() .AND. CONTRACT ->status == 1
     str += "| " + padR( CONTRACT ->intDoc,  14) + ;
           " | " + padR( CONTRACT ->docNumb, 12) + " |" + endl()
  endif
  skip 1 alias CONTRACT
  statProc()
enddo
str += "+-------------------------------+" + endl()
statExit()
CONTRACT ->( baseRestPos())
editMemo( str, .F.)
if yesNo( "��ᯥ���� ?") then printAOrder( {str})
return NIL


funcDef gszPrint3 ;
   local aCode, aName, aBuf, str, arr, i, arrWisc, arr2, ptr, cnt, code

if DtoS( setCurDate()) <> setupWDate()
   messageB( "��� ���ଠ樨 � ���� !!!")
   return .F.
endif

assGetInfo( GSZ_CODE_2, @aCode, @aName, @aBuf)
if len( aCode) < 1
   messageB( "��� ���ଠ樨 �� ��������� !!!")
   return .F.
endif
arr := Array( len( aCode))
for i := 1 to len( aCode) do ;
  arr[ i] := { aName[ i], aCode[ i], 0, 0, kasLimit( aCode[ i]) }

WISC ->( baseSavePos())
WISC ->( dbSetOrder( 2))
WISC ->( dbSeek( GSZ_WISC))
statStartUp( "�����⮢�� ����")
while WISC ->type == GSZ_WISC .AND. WISC ->( !Eof())
  arrWisc := CtoA( WISC ->buffer, ";")
  for i := 2 to 4
    arr2 := CtoA( arrWisc[ i], ",")
    cnt  := val( arr2[ 2])
    code := arr2[ 1]
    if ( ptr := aScan( arr, {|x| x[ 2] == code})) > 0
       arr[ ptr, 4] += cnt
    endif
  next
  statProc()
  skip 1 alias WISC
enddo
str := DtoC( setCurDate()) + Space( 20) + "��� " + confOtdNumb() + endl() + ;
       "���� � ࠡ�� ��� � �������ﬨ ��� ���" + endl() + endl() + ;
       "+--------------------------------------------------------------+" + endl() + ;
       "|  ��� ���� | ����㯭� � �த��� | ���室 | ����㯭� � �த��� |" + endl() + ;
       "|           | �� ��砫� ���      |        | �� ����� ���       |" + endl() + ;
       "+--------------------------------------------------------------+" + endl()
//       01234567890 01234567890123456789 01234567 01234567890123456789
for i := 1 to len( arr)
  arr[ i, 3] := arr[ i, 4] + arr[ i, 5]
  str += "| " + padR( arr[ i, 2], 9) + " | " + padL( arr[ i, 3], 18) + ;
        " | " + padL( arr[ i, 4], 6) + " | " + padL( arr[ i, 5], 18) + ;
        " | " + endl()
next
str += "+--------------------------------------------------------------+" + endl()
statExit()
WISC ->( baseRestPos())
editMemo( str, .F.)
if yesNo( "��ᯥ���� ?") then printAOrder( {str})


return NIL

