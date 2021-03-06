*============================================================================
* 
*                  �ணࠬ�� : MON.EXE
*                  ����      : baseaccn.prg
*                  ����     : ���⫨� �.�.
*
*                  �����祭��: �뤠� ���ᨩ
*
*----------------------------------------------------------------------------
*
* 18-06-2003  TMV
* ��⠢���� �஢�ઠ �� ����������� �뤠� ����.
* �-�� pensGive(), setKeys()
*
* 30-10-2003  TMV ��� ����
* ��� ⥪��� ��⮢ �� �뤠� ��⠢���� �஢�ઠ - �᫨ ������ �� ����⨥
* �� �����ᠭ�, ��� ���⠥��� �, �᫨ ����� "�� �����ᠭ�" - ������
* �⬥�����.
* �������� �-�� pensGive().
*
* 02-12-2004 TAT
* � �����祭�� ���⥦�: '�믫�� �।�� � ⥪�饣� ���ᨮ����� ���'
* �������� �� "�믫�� � ������ �� ����ॡ������"

#include "common.ch"
#include "clippExt.ch"
#include "box.ch"

#include "mon.ch"
#include "inkey.ch"


static advPens := NIL
static advHome := NIL

 
//
// �㭪�� �뤠� ���ᨨ. ����室��� ����� �� ���ᨮ���� ���
// ������� ������. �뤠� �ந�������� ⮫쪮 �᫨ ���� ���⮪
//
funcDef pensGive ;
   local get, aHistory, i, nWidth := 15, tmp := {}, summa, summa1, summa2  ,;
         aOrder, dDate, nazn, scr, color, s := select(), arr, arr2         ,;
         trustPtr := -1, showStr, ro, sim, thePensCode := opDecode( "1001"),;
         theOrder, fk, trustCurDocum, trustFIO, trustAddr, lPens

if msgShowMessage()

   lPens := isPens( ACC -> PRIZ )


   showStr := if( lPens, "�믫�� ���ᨩ ", "�믫�� � ��� ")
   showStr += "( ��� N " + ACC ->schet + " )"
   allowTrust( .F.)  // ����頥� ᢥ�� � TRUST


   Save screen to scr
   color := setColor()

   prepareScreen()
   messageStartUp( "�����⮢�� �믨᪨")

   // histView( .T.)
   aHistory := ctoA( getHistory( ACC ->schet), endl())
   // histView( .F.)
   if len( aHistory) > nWidth
      for i := 1 to 6 do aAdd( tmp, aHistory[ i])
      for i := len( aHistory) - 9 to len( aHistory) do aAdd( tmp, aHistory[ i])
   else
      tmp := aClone( aHistory)
   endif

   messageExit( 0)

   messageStartUp( padC( showStr, maxCol() - 3))

   @ 4, 1, 19, maxCol() - 1 BOX B_SINGLE + " " color "w+/b"
   for i:= 1 to len( tmp) do @ 4 + i, 2 say padR( tmp[i], maxCol()-3) color "w+/b"

   dDate  := setCurDate()
   summa  := sumRest( ACC ->schet, dDate )
   summa1 := summa
   summa2 := 0.00
//  Replaced by TMV  11-10-2001
   nazn = '�믫�� ������� �������� �।��'

   arr := trustWorkList()
   if len( arr) >0
      arr2 := { "�������� ������ " + cliFullName() }
      for trustPtr := 1 to len( arr) do aAdd( arr2, arr[ trustPtr][ 2, 5])
      trustPtr := popUp( 8, 2, arr2)
   endif
   if trustPtr > 1
      allowTrust( .T.)
      TRUST ->( dbGoTo( arr[ trustPtr - 1, 1]))
      showStr += "�� ����७���� ( N " + TRUST ->number + " ) " + TRUST ->fio
      trustCurDocum := trustCurDocum()
      trustFIO      := trustFIO()
      trustAddr     := trustAddr()
   endif

   get     := {{ "�㬬� �뤠�  : ", block( summa1),,,;
                      {|g| changeS1( @summa, @summa1, @summa2, @get, g) }},;
               { "�㬬� ���⪠ : ", block( summa2),,,;
                      {|g| changeS2( @summa, @summa1, @summa2, @get, g) }} }

//  Replaced 04-10-2001 by TMV
   fk := {, {{ "�믨᪠", {|| prnHist() }}},;
            {{ "�थ�",   {|| prnOrd( trustCurDocum, trustFIO, trustAddr)} }},;
            {{ "�����",  {|| zab_FeedOform() }       }},;
            {{ "�����",   {|| alo_sprNeop( acc->schet, clients->code)}}},;
            {{ "����७", {|| trustView()     }       }},;
            {{ "�।��",  {|| if( chckMicro(), accCredOpen(), .F.)} }},;
            {{ "��� %",   {|| ChangePcts()    }       }},;
            {{ "��ࠢ��", {|| alo_sprGive( acc->schet) } }},;
            {{ "������",  {|| paymCash( .F., acc->schet) }       }} }

// Add by TMV  18-06-2003 With <confUseMicro()>
if !confUseMicro()
	fk[7]:={}      // {{ "�।��",  {|| if( chckMicro(), accCredOpen(), .F.)} }}
endif
// Add by TMV  18-06-2003 With <confUseMicro()> -- End

//   fk := {, {{ "�믨᪠", {|| prnHist() }}},;
//            {{ "�थ�",   {|| prnOrd( trustCurDocum, trustFIO, trustAddr)} }},;
//            {},;
//            {},;
//            {{ "����७", {|| trustView()     }       }},;
//            {},;
//            {},;
//            {{ "��ࠢ��", {|| alo_sprGive( acc->schet) } }};
//            }
   fKeysStartUp( fk)
   fKeysProc( fk)
   setKeys( trustCurDocum, trustFIO, trustAddr)

   setKey( K_F3, {|| prnOrd( trustCurDocum, trustFIO, trustAddr) })

   if getUp(20,25,get)
      //
      // �믫�⨬ �����
      //
      if summa1 <= sumRest( ACC ->schet, dDate) .AND. summa1 > 0

         operGetInfo( thePensCode, @ro, @sim)

         if allowTrust()
            // aOrder := post_order( theOrder := ordIncOut( dDate), .T., dDate  ,;
            //           trustFIO(), trustAddr(), ACC ->schet, summa1, nazn, sim,;
            //           ro, .T., trustCurDocum)
            
            // 
            // 23.03.2001
            //
            //aOrder := post_order( theOrder := ordIncOut( dDate), .T., dDate  ,;
            //          cliFullName(), cliFullAddress(), ACC ->schet, summa1   ,;
            //          nazn, sim, ro, .T., trustOrderInfo( trustCurDocum))
            aOrder := post_order( theOrder := ordIncOut( dDate), .T., dDate  ,;
                      cliFullName(), cliFullAddress(), ACC ->schet, summa1   ,;
                      nazn, sim, ro, .T., trustOrderInfo( trustCurDocum)     ,;
                      trustFIO(), trustAddr() )
         else
//  Replaced by TMV  03-10-2001
            aOrder := post_order( theOrder := ordIncOut( dDate), .T., dDate,;
                      cliFullName(), cliFullAddress(), ACC ->schet, summa1 ,;
                      nazn, sim, ro, .T., cliDocum())
//				  aOrder := kasRKOPOR( theOrder := ordIncOut( dDate),;
//						dDate, ACC ->schet, ACC ->schet, summa1, cliFullName(),;
//						'', nazn, sim, ro, cliDocum(), cliFullAddress())
         endif
         if yesNo( "�� ����⢨⥫쭮 ��� �믮����� �஢���� �� �㬬� " + cStr( summa1) + " ?" )
            if summa1 <= kasRest()                                         
               //
               // ��砫� �����
               //
               schetPrintOpen( .T.)
               printHistory( ACC ->schet, dDate, - summa1)
               printAOrder( aOrder)
               saveLastOrder( aOrder)
               /* �᪫�祭� ᮣ��᭮ �㬠��� �� 14.01.2000
               if trustPtr > 1
                  aOrder := trustGiveInfo( trustCurDocum)
                  printAOrder( aOrder)
                  SaveLastOrder( aOrder, .F.)
               endif */
               printAdvert()


               if !sumWrite( ACC ->schet, - summa1, dDate, thePensCode,;
                  theOrder,, trustGetNumber())
                  errLog({ "�஢���� �� ������� " + cliFullName() + " �� ������� !",;
                          "", "��� " + splitSSchet( ACC ->schet)                 ,;
                          "�㬬� " + cStr( summa1), "", ""              ,;
                          "������ ����� �थ� � �������㠫쭮� ०���"})
               else
                  //
                  // ���४�� TRUST
                  //
                  if allowTrust()
                     if trustRLock()
                        trustLastSum( setCurDate(), summa1)
                        trustUnLock()
                     endif
                  endif
               endif
            else
               errLog( "�� �஢���� �맮��� ������ ��᭮�� ᠫ줮 �� ���� !",;
                      "", "����९�� ����� !")
            endif // kasRest
         endif
      else
         errLog({"�� ��⠥��� �믫���� " + mStr( summa1),;
                "���⮪ �� ����       " + mStr(sumRest(ACC ->schet,dDate))})
      endif
   endif

   fKeysExit()
   delKeys()

   messageExit( 0)
   rest screen from scr
   setColor( color)
   allowTrust( .F.)
endif

return NIL
 
static funcDef setKeys with trustCurDocum:CU, trustFIO:CU, trustAddr:CU local doc
doc := trustCurDocum
setKey( K_F3, {|| delKeys(), prnOrd( doc, trustFIO, trustAddr),    setKeys( doc) })
setKey( K_F2, {|| delKeys(), prnHist(),       setKeys( doc) })
setKey( K_F4, {|| delKeys(), zab_FeedOform(), setKeys( doc)})
setKey( K_F5, {|| delKeys(), alo_sprNeop( acc->schet, clients->code), setKeys( doc)})
setKey( K_F6, {|| delKeys(), trustView()    , setKeys( doc)})
// Replaced by TMV  18-06-2003
setKey( K_F7, {|| delKeys(), if(confUseMicro(),if( chckMicro(), accCredOpen(), 0), 0), setKeys( doc)})
//setKey( K_F7, {|| delKeys(), if( chckMicro(), accCredOpen(), 0), setKeys( doc)})
setKey( K_F8, {|| delKeys(), ChangePcts()   , setKeys( doc)})
setKey( K_F9, {|| delKeys(), alo_sprGive( acc->schet),   setKeys( doc)})
setKey( K_F10,{|| delKeys(), paymCash( .F., acc->schet), setKeys( doc)})
return NIL

static funcDef delKeys
setKey( K_F2, NIL)
setKey( K_F3, NIL)
setKey( K_F4, NIL)
setKey( K_F5, NIL)
setKey( K_F6, NIL)
setKey( K_F7, NIL)
setKey( K_F8, NIL)
setKey( K_F9, NIL)
setKey( K_F10,NIL)
return NIL


funcDef prnHist
if yesNo( "�� ����⢨⥫쭮 ��� �������� �믨�� ?")
   printHistory( ACC ->schet)
endif
return NIL

funcDef prnOrd with trustCurDocum:CU, trustFIO:CU, trustAddr:CU, useIndiv:LU ;
   local theCode, ro, sim, nazn, aOrder, theOrder, lPrintDate, dDate,;
         summa, rest

DEFAULT useIndiv TO .F.

if yesNo( "�� ����⢨⥫쭮 ��� �������� �थ� � ���⠢��� �� ���?" )
   theCode    := ORD_33_DATE
   dDate      := setCurDate()
   nazn       := "�믫�� �।�� � ���ᨮ����� ��� /�� ����/"
   if confUseDate()
      lPrintDate := .T.
   else
      lPrintDate := yesNo( "�� ��� �������� �थ� � �����?" )
   endif
   summa      := sumRest( ACC ->schet, dDate)
   rest       := summa
   if !getUp( 10, 5, {{ "������ �㬬� �थ� ", block( summa),,,;
      {|g| g:varGet > 0 .AND. g:varGet <= rest }}})
      return NIL
   endif
   if !lPrintDate then theCode := ORD_33_NODATE

   operGetInfo( theCode, @ro, @sim)
   theOrder := ordIncOut( dDate)
   if allowTrust() .AND. !empty( trustCurDocum())
      // aOrder := homeOneOrder( trustFIO(), trustAddr(), trustCurDocum, theOrder,;
      //    nazn, ro, sim, summa, lPrintDate, .T., dDate, conf904Work())
      aOrder := homeOneOrder( cliFullName(), cliFullAddress(), trustCurDocum,;
         theOrder, nazn, ro, sim, summa, lPrintDate, .T., dDate, conf904Work(),;
         trustFIO, trustAddr)
   else
      aOrder := homeOneOrder( cliFullName(), cliFullAddress(), cliDocum()   ,;
         theOrder, nazn, ro, sim, summa, lPrintDate, .T., dDate, conf904Work())
   endif
   schetPrintOpen( .F.)
   if lPrintDate
      printHistory( ACC ->schet, dDate, - summa)
   else
      printHistory( ACC ->schet, dDate,,.F.)
   endif
   printAOrder( aOrder); printAdv2( .T.)
   if accRLock();  ACC ->datPens := dDate;  accUnLock();  endif
   if !sumWrite( ACC ->schet, - summa, dDate, theCode, theOrder, CLIENTS ->distrCode)
      errLog({ "�஢���� �� ������� �� ���������!",;
              "������ ����� �थ� � �������㠫쭮� ०���"})
   else
     if useIndiv
        OK( "�஢���� ���������.")
     else
        OK( { "�஢���� ���������.", "", ""         ,;
              "�᫨ �� ��� �� 㢨���� � �믨᪥," ,;
              "�� ࠧ ������ � ०�� �믫��� "     })
     endif
   endif
endif
return NIL

//
// �믫�� ���ᨨ ��� ��������� � ���� ࠧ
//
funcDef ordIndiv local arr := {}, arr2 := {}, ptr, nd := 0
if cliGetHome( @nD)
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 2))
   pensMakeArrs( @arr, @arr2, .T., .T.)
   if len( arr) = 0
     errLog( "� ������ ��� ���ᨮ���� ��⮢ � ���⪠��!" )
   else
     ptr := if( len( arr) == 1, 1, popUp( 5, 5, arr))
     if ptr > 0
       ACC ->( dbGoTo( arr2[ ptr]))
       if msgShowMessage() then prnOrd(,,, .T.)   
     endif
   endif
   ACC ->( baseRestPos())   
else
  messageB( "�� ������ ��⠭����� �ਧ��� ����祭�� ���ᨨ �� ����!" )
endif   

return NIL


static func changeS1( summa, summa1, summa2, aGet, g)
local rV := .T., theSumma := 0
if g:varGet < 0
   messageB( "����⥫�� �㬬� �������⨬�!" )
   return .F.
endif

if g:varGet() > summa
   errLog( "�� �஢���� �맮��� ��᭮� ᠫ줮!" )
   rV := .F.
else

   //
   // ����� �� ���� ⠪�� �㬬� ?
   //
   if allowTrust()
      chckTrustDate( setCurDate(), @theSumma)
      if TRUST ->type_1 == 2
         if g:varGet() > theSumma
            errLog( "�� ����७���� N "     + TRUST ->number + ;
                   " ����� ���� �� ����� " + mStr( theSumma))
            rV := .F.
         endif
      endif
   endif
   if rV
      summa1 := g:varGet()
      summa2 := summa - summa1
      getReProc( aGet)
   endif

endif

if rV .AND. summa1 > kasRest()
   errLog( { "���⮪ �� ����   " + mStr( kasRest()) ,;
            "�� ��⠥��� ���� " + mStr( summa1)    ,;
            "", "������� ����९����� ����� !" })
   rV := .F.
endif
return rV


static func changeS2( summa, summa1, summa2, aGet, g)
local rV := .T.

if g:varGet < 0
   messageB( "����⥫�� �㬬� ??? ")
   return .F.
endif

if g:varGet() > summa
   errLog( "�� �஢���� �맮��� ��᭮� ᠫ줮 !")
   rV := .F.
else
   summa2 := g:varGet()
   summa1 := summa - summa2
   getReProc( aGet)
endif

if rV .AND. summa1 > kasRest()
   errLog( { "���⮪ �� ����   " + mStr( kasRest()) ,;
            "�� ��⠥��� ���� " + mStr( summa1)    ,;
            "", "������� ����९����� ����� !" })
   rV := .F.
endif

return rV



static func changeIt( g, rest, total)
local rV := .T.

return rV


static procDef prepareScreen with helpH:ACBU

   setCursor( 0)
   set message to ( maxRow()) center
   setBlink( .F.)
   setColor( left( normalColor( setScrColor()), ;
                   at( "/", normalColor( setScrColor()))))  // �ᥣ�� �� �୮�
   scroll()
   @ 0, 1 SAY dDate( setCurDate())
   dispBox( 2, 1, maxRow()-1, maxCol()-1, "���������", setScrColor())
   clLineA( 1,        1, maxCol()-1, normalCByte( setMnuColor())) // Main menu
   clLineA( maxRow(), 1, maxCol()-1, normalCByte( setMnuColor())) // Mess line
   setColor( setMnuColor())
   displayStatusLine()

return


autofunction setValue allowTrust init .F.
autofunction setValue mustAdvPrint  init NIL
autofunction setValue mustHomePrint init NIL

funcDef printAdvert local arr, i, str

chkAdvFiles()
// if empty( advPens) then advPens := newFPath( ".\adv_pens.pic", confPathOD())
if file( advPens)
   arr := { memoRead( advPens)}
   if !mustAdvPrint() .AND. confPrintUsel() then return NIL
   printAOrder( arr,, if( !confPrintUsel(), chr(12) + endl(), "") )
//elseif !confPrintUsel()                  //  Removed by TMV  13-12-2001
//   arr := Array( 14)
//   arr := aFill( arr, "")
//   printAOrder( arr,, chr(12) + endl())
endif
return NIL

static funcDef chkAdvFiles local i, str

if empty( advHome) then advHome := newFPath( ".\adv_home.pic", confPathOD())
if empty( advPens) then advPens := newFPath( ".\adv_pens.pic", confPathOD())
if mustAdvPrint() == NIL
   str := CtoA( memoRead( advPens), endl())
   mustAdvPrint( .F.)
   for i := 1 to len(str) do if !empty( allTrim(str[i])) then mustAdvPrint(.T.)
endif

if mustHomePrint() == NIL
   str := CtoA( memoRead( advHome), endl())
   mustHomePrint( .F.)
   for i := 1 to len(str) do if !empty( allTrim(str[i])) then mustHomePrint(.T.)
endif

return NIL


funcDef printAdv2 with skip12Lines:L local arr, skipLines

chkAdvFiles()
if file( advHome)
   if !mustHomePrint() .AND. confPrintUsel() then return NIL
   arr := { memoRead( advHome)}
else
   if !mustHomePrint() .AND. confPrintUsel() then return NIL
   skipLines := if( skip12Lines, 14, 3)
   arr := Array( skipLines)
   arr := aFill( arr, "")
endif

if ascan(arr, {|x| !empty(x)}) > 0 then printAOrder( arr)
//printAOrder( arr)  //  Replaced by TMV  19-12-2001

return NIL


funcDef msgGetMessage with arr:A
return if( len( arr) >0, arr[ 1], "")

funcDef msgGetResult with arr:A
return if( len( arr) >0, arr[ 2] == 1, .T.)

funcDef msgShowMessage with arr:ACU local rV, msg
   DEFAULT arr TO spr_infoSeek( ACC ->schet)
   if valType( arr) == "C" then arr := spr_infoSeek( arr)
   msg := msgGetMessage( arr)
   rV  := msgGetResult(  arr)
   if !empty( msg) then messageB( "����饭�� : " + rTrim( msg))
return rV