*============================================================================
* 
*                  �ணࠬ�� : MON.EXE
*                  ����      : baseaccn.prg
*                  ����     : ���⫨� �.�.
*
*                  �����祭��: ���ᠭ�� 䠩�� ����७���⥩
*
*----------------------------------------------------------------------------
*
* 26-01-2003  TMV
* �� ।���஢���� ����७���� ⥯��� �� ������� �� ����� - trustDovEdit()
*
* 27-02-2003  TMV
* ��࠭� �஢�ઠ ����� ��ᯮ�� � ������� �-�� chckNumb(). �-�� trustDovEdit()
*  trustCurDocum().
*
* 05-02-2004  TMV
* � �ଥ ����७���� ��������� ��� ��ப� "�����஢�� ������".
* �-�� trustGetOrder()
*
* 18-02-2004  TMV
* �������� �ଠ ����७����.
* �-�� trustGetOrder()
*
* 29-03-2004  TMV
* �������� �ଠ ����७����.
* �-�� trustGetOrder()
*
* 19-04-2004  TMV
* �������� �ଠ ����७����. ��࠭ ����஫� �� ����묨 ��ᯮ�� ����७����.
* �-�� trustGetOrder(), trustDocum(), trustDovEdit()
*

#include "common.ch"
#include "clippExt.ch"

* NEW OBJECT FILE

func trustInfo
static dbInfo := { ".\trust.DBF", "TRUST",;
                  {{ "code",      "C", 12, 0},;     // ��� ������
                   { "schet",     "C", 20, 0},;     // ��� �� ����� �뤠�� ����७�����
                   { "dateBeg",   "D",  8, 0},;     // ��砫� ����⢨�
                   { "dateEnd",   "D",  8, 0},;     // ��� ����砭�� ����⢨�
                   { "fio",       "C", 45, 0},;     // ��� ����७���� ���
                   { "Address",   "C", 50, 0},;     // ���� ����७���� ���
                   { "pasSer1",   "C",  8, 0},;     // ��ᯮ��� �����
                   { "docType",   "C",  2, 0},;
                   { "pasSer2",   "C",  2, 0},;
                   { "pasNum",    "C",  6, 0},;
                   { "pasGiv",    "C", 40, 0},;
                   { "placeGive", "C", 50, 0},;     // ���� �뤠� ����७����
                   { "dateGive",  "D",  8, 0},;     // ��� �뤠�
                   { "typeGive",  "N",  1, 0},;     // 2 - �뤠�� �� �� ����
                   { "status",    "N",  1, 0},;     // ����� ����७����
                   { "D_status",  "D",  8, 0},;
                   { "lastDate",  "D",  8, 0},;     // ��� ��᫥����� �ᯮ�짮�����
                   { "lastSum",   "N", 15, 2},;     // �㬬� �� ��᫥���� �����
                   { "cntUse",    "N",  3, 0},;     // ���-�� �㬬 �����
                   { "number",    "C",  8, 0},;     // ����७��� �����
                   { "type_1",    "N",  1, 0},;     // <= 1 - �� ���죨, 2 - �� ����� �㬬� �� �����
                   { "summa_1",   "N", 15, 2},;     // �㬬� �� �����
                   { "buffer",    "C", 30, 0},;
                   { "lEdited",   "L",  1, 0}},;    // �ਬ�砭��
                  {{, "code + schet + DtoS( dateEnd)"        } ,;
                   {, "schet+ DtoS( dateEnd)"                } ,;
                   {, "right( number, 2) + left( number, 5)" } ,;
                   {, "fio"                                  }},;
                  0}
return dbInfo

func trustDovInfo
static dbInfo := { ".\TRUSTDOV.DBF", "TRUSTDOV",;
                    {{ "fio",  "C", 40, 0} ,;
                     { "work", "C", 60, 0}},;
                    {}, 0, .T.}
return dbInfo

funcDef trustPath with newPath:CU
return basesPath( { trustDovInfo(), trustInfo()}, newPath)

funcDef trustOpen
return basesNSOpen( { trustDovInfo(), trustInfo()})

funcDef trustXOpen
return basesXOpen( { trustDovInfo(), trustInfo()})

funcDef trustClose
return basesMClose( { trustDovInfo(), trustInfo()  })

funcDef trustCreate
return basesCreate( { trustDovInfo(), trustInfo() })

funcDef trustAppend
return trust ->( mAppend())

funcDef trustRLock
return trust ->( mRLock())

funcDef trustUnLock local rV
  TRUST -> lEdited := .T.
  rV := trust ->( netUnLock())
return rV

//
// ���� ����� ����७����
//
funcDef trustDovNew local i, rV := .F.

  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  if ACC ->( !dbSeek( cliCode()))
     messageB( "� ������ ��� ��⮢ !")
     ACC ->( baseRestPos())
     return .F.
  endif
  ACC ->( baseRestPos())

if trustAppend()

   i := dialog( { ;
      "���ᨮ��� ����� ��ନ�� ����७����� �� ࠡ��� � ᢮��  " ,;
      "��⮬ �� ����� ࠡ���, �� ����� ��⥫��⢠ (� ����), �   " ,;
      "������ � �.�. ����� ��ନ�� ����७����� ����� � ��   " ,;
      "����.��� ����� ������ �� �뤠���� � ��஭��� �࣠����樨" ,;
      "����७���� �롥�� �㭪� :                             " ,;
      "          <���� ������ �� �����. ����७����>           " ,;
      "�뤠�� ����७����� �� ���� �����, �ᯮ���� �㭪� :     " ,;
      "             <��ନ�� ����७����� �� ����>             "},;
      { "���� ������ �� �����. ����७����" ,;
        "��ନ�� ����७����� �� ����" })
   if i = 1
      TRUST ->typeGive := 2
      getUp( 2, 2,;
            {{ "���� �뤠� ����७���� : ", block( TRUST ->placeGive)},;
             { "��� �뤠� ����७����  : ", block( TRUST ->dateGive)}} )
   else
      TRUST ->typeGive  := 1
      TRUST ->placeGive := rTrim( confFilial()) + " " + confOtdName()
      TRUST ->dateGive  := setCurDate()
   endif

   TRUST ->dateBeg := setCurDate()
   TRUST ->dateEnd := setCurDate()

   if trustDovEdit( .T.)
      TRUST ->number := trustNextNumber()
      TRUST ->code   := cliCode()
      TRUST ->status := 1

      if i > 1 then trustPrint()
      rV := .T.
   else
      TRUST ->( clearRec())
   endif
   trustUnLock()
endif

return rV

funcDef trustNextNumber local rV := "", seekStr
rV := confTrustNextCode()
/*
TRUST ->( baseSavePos())
TRUST ->( dbSetOrder( 3))

seekStr := str( Year( setCurDate()) - 1900, 2) + "9999"
TRUST ->( netSeek( seekStr, .T.))

if right( TRUST ->number, 2) <> left( seekStr, 2) then TRUST ->( netSkipUp())
if right( TRUST ->number, 2) <> left( seekStr, 2)
   rV := "00001"
else
   rV := padL( val( left( TRUST ->number, 5)) + 1, 5, "0")
endif
rV    := rV + "-" + str( Year( setCurDate()) - 1900, 2)

TRUST ->( baseRestPos())
*/

return rV

funcDef trustGetNumber
return if( allowTrust() .AND. TRUST ->type_1 == 2, "DOV:" + TRUST ->number, "")

funcDef trustNumber
return TRUST ->number


//
// ������஢���� ����७����. �������� ����७����� ���
// ⥪�饣� ������ � ��� !
//
funcDef trustDovEdit with newTrust:L ;
   local get, rV, arr1, newType_1, aSchet := { }, nSchet := 1,;
         docCode, aDocName, aDocCode, docPtr, canEdit, tDate

aDocName := aDocName()
aDocCode := aDocCode()
docPtr   := aTrimmedScan( aDocCode, "09")
if docPtr > 0
   aDel( aDocName, docPtr); aSize( aDocName, len( aDocName) -1)
   aDel( aDocCode, docPtr); aSize( aDocCode, len( aDocCode) -1)
endif

docCode := TRUST ->docType
if empty( docCode) then docCode := "01"
docPtr  := max( 1, aTrimmedScan( aDocCode, docCode))
canEdit := docPtr == 1


tDate    := TRUST ->dateBeg
arr1 := { "�� ������� �।�⢠"                 ,;
          "�������筮 ������� �।�⢠"          ,;
          "�㬬� ������ � ���᫥��� ��業��"   ,;
          "�������筮 �㬬� ���᫥���� ��業⮢" }
newType_1 := arr1[ max( 1, TRUST ->type_1)]
ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

accSeek( cliCode())
while ACC ->code == cliCode() .AND. ACC ->( !eof())
      aAdd( aSchet, ACC ->schet)
      skip 1 alias ACC
enddo

ACC ->( baseRestPos())
if len( aSchet) == 0
  messageB( "� ������ ��� ��⮢ !")
  return .F.
endif


nSchet := max( 1, aScan( aSchet, {|x| x == TRUST ->schet}))

get := {{ "�������� ��� ", block( nSchet), aClone( aSchet),{|| newTrust}},;
        { "*" + chr( 25)                                            } ,;
        { "���     : " + cliFullName()                              } ,;
        { "��ᯮ�� : " + cliPasSer() + " N " + cliPasNum()          } ,;
        { "����   : " + cliFullAddress()                           } ,;
        { replicate( "-", 50)                                       } ,;
        { "���������� ����          : ",  block( TRUST ->fio),,,{|g|chckName( g)} } ,;
        { "�஦����饥 �� �����    : ",  block( TRUST ->Address)   } ,;
        { "��� ���㬥��", block( docPtr), aDocName,,;
               {|g| paspCanClear( g, get, aDocName, aDocCode, @canEdit)}},;
;// Replaced by TMV  19-04-2004 -- ��࠭ ����஫� �ਨ ��ᯮ��
        { "���� ���㬥�� ", block( TRUST ->pasSer1),,;
;//               {|g| paspClear( g, canEdit)}, {|g| chckSer1( g,,canEdit)}} ,;
               {|g| paspClear( g, canEdit)}} ,;
        { "*-",               block( TRUST ->pasSer2),,;
;//               {|g| paspClear( g, canEdit)}, {|g| chckSer2( g,,canEdit)}} ,;
               {|g| paspClear( g, canEdit)}} ,;
;// Replaced by TMV  19-04-2004 -- ��࠭ ����஫� �ਨ ��ᯮ�� -- End
;// Replaced by TMV  27-02-2003 -- ��࠭ ����஫� ����� ��ᯮ��
        { "*����� ���㬥��", block( TRUST ->pasNum ),"@K 999999"           ,;
               {|g| paspClear( g, canEdit)}} ,;
        { "" },;
        { "���㬥�� �뤠�  ", block( TRUST ->pasGiv)} ,;
        { "" }                                                        ,;
        { "�������� ", block( TRUST ->type_1), arr1,{|| newTrust},{|g| trustChkType( g, arr1, aSchet, @newType_1)} },;
        { "* �㬬� :", block( TRUST ->summa_1),,{|g| newTrust .AND. canEditSum1( g, newType_1, arr1)},{|g| g:varGet >= 0} },;
        { "" }                                                        ,;
        { "���� �뤠� ����७���� : ", block( TRUST ->placeGive),,{|| TRUST ->typeGive == 2} } ,;
        { "��� �뤠� ����७����  : ", block( TRUST ->dateGive) ,,{|| TRUST ->typeGive == 2} } ,;
        { "�ਬ�砭��                : ", block( TRUST ->buffer)    } ,;
        { "" }                                                        ,;
        { "��ଫ��� ����७����� �� �ப c ", block(TRUST->dateBeg),,{|| newTrust},;
              {|g| tmpDateBeg( g, @tDate)}},;
        { "* �� ",                        block( TRUST ->dateEnd)   ,,{|| newTrust},;
              {|g| chkDateEnd( g, tDate )}}}

// Replaced by TMV  27-02-2003 -- ��࠭ ����஫� ����� ��ᯮ��
//        { "*����� ���㬥��", block( TRUST ->pasNum ),"@K 999999"           ,;
//               {|g| paspClear( g, canEdit)}, {|g| chckNumb( g,,canEdit)}} ,;

if ( rV := getUp( 5, 5, get))
// Revoved by TMV 26-01-2003
//   if getUpdated() .AND. !newTrust then TRUST ->number := trustNextNumber()
   TRUST ->pasSer2 := upper( TRUST ->pasSer2)
   TRUST ->schet   := aSchet[   nSchet]
   TRUST ->docType := aDocCode[ docPtr]
endif
return rV

static funcDef tmpDateBeg with g:O, tmpDate:D 
  tmpDate := g:varGet()
  if empty( TRUST ->number) .AND. TRUST ->typeGive == 1
    if tmpDate < TRUST ->dateGive
      messageB( "����७����� �� ����� ��稭����� ࠭�� " + DtoC( TRUST ->dateGive))
      clear typeAHead
      return .F.
    endif
  endif
return .T.

static funcDef chkDateEnd with g:O, tmpDate:D 
if g:varGet() < tmpDate
   messageB( "��� ����砭�� ����� ���� ��砫� !!!")
   return .F.
endif
if ( g:varGet - tmpDate ) > 365 * 3 
   messageB( "����७����� ����� �뤠�� �� �ப �� ����� ��� ��� !")
   return .F.
endif
  
return .T.


static funcDef trustChkType with g:O, arr1:A, aSchet:A, newType_1:C ;
   local rV  := .T., ptr, theSchet
   newType_1 := g:varGet
   ptr       := aTrimmedScan( arr1, g:varGet)
   theSchet  := M ->getList[ 1]:varGet
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))
   ACC ->( dbSeek( theSchet))
   if isSr() .AND. ptr < 3
      messageB( "��� ०�� ����㯥� ⮫쪮 ��� ⥪��� ��⮢ !!!")
      rV := .F.
   endif
   if ptr == 3 .AND. !isSr()
      messageB( "��� ०�� ����㯥� ⮫쪮 ��� ��筮�� ������ !!!")
      rV := .F.
   endif
   if ptr == 4 .AND. !isCanDopVznos()
      messageB( "��� ०�� ����㯥� ⮫쪮 ��� ������⥫쭮�� ������ !!!")
      rV := .F.
   endif
   ACC ->( baseRestPos())
return rV


static func canEditSum1( g, newType_1, arr1)
local rV, ptr

ptr := aScan( arr1, {|x| allTrim( x) == alltrim( newType_1)})
rV  := ptr == 2
if ptr <> 2 .AND. g:varGet <> 0
   g:varPut( 0.00)
endif
return ptr == 2

func trustAStatus
return { "�������"     ,;
         "������"        ,;
         "�ਮ�⠭�����" ,;
         "㤠���"        ,;
         "���㫨஢��"   }



//
// ᯨ᮪ ���� ����७���⥩ �� ⥪�饬� �������
//
funcDef trustView local aH, aC, fk, b, bUp, bDn, bTop, bBtm, s, rV, opened

aH := {{ "������ : " + cliFullName()                  },;
       { "������������������������������������������" }}

aC := {{ "N ���.",                block( TRUST ->number)  },;
       { "���",                  block( TRUST ->schet)   },;
       { "�������� ����७����", block( TRUST ->fio)     },;
       { "�����",              {, block( TRUST ->status), trustAStatus() }},;
       { "����砭��",             block( TRUST ->dateEnd) }}

fk := {, {{ "�����" , {|| dovAdd() }      }},,   ;
         {{ "������", {|| dovEdit()}      }},,   ;
         {{ "���㫨", {|| trustDelOrder()}}},,,  ;
         {{ "�����", {|| trustPrint()}  }}  }


b    := {|| TRUST ->code == cliCode()}
bUp  := {|| netSkipUp(   b)}
bDn  := {|| netSkipDown( b)}
bTop := {|| netGoTop(    b)}
bBtm := {|| netGoBottom( b)}

bUp  := {|| trustBrUp(   TRUST ->code)}
bDn  := {|| trustBrDown( TRUST ->code)}
bTop := {|| trustBrTop(  TRUST ->code)}
bBtm := {|| trustBrBtm(  TRUST ->code)}

s      := select()
rV     := .F.
opened := select( "TRUST") >0

if !opened then rV := trustOpen()

if opened .OR. rV

   TRUST ->( baseSavePos())
   TRUST ->( dbSetOrder( 1))
   TRUST ->( netSeek( cliCode(), .T.))
   rV := cliCode() == TRUST ->code
   if !rV
      if yesNo( { "� ������ " + cliFullName() + " ��� ����७���⥩",;
                  "", "�� ��� ����� ����� ����७����� ?"})
         rV := trustDovNew()
      endif
   endif
   if rV then TRUST ->( browseUp( 2, 2, aH, aC, bUp, bDn, bTop, bBtm,,,,, fk))

   TRUST ->( baseRestPos())
   if !opened then trustClose()

endif

return NIL

static funcDef dovAdd

if yesNo( "�� ��� ����� ����� ����७����� ?")
   if trustDovNew() then getCurOB(): refreshAll(): forceStable()
endif

return NIL


static funcDef dovEdit

if trustRLock()
   if trustDovEdit( .F.) then getCurOB(): refreshCurrent(): forceStable()
   trustUnLock()
endif

return NIL

funcDef trustPrint
return  printAOrder( trustGetOrder())


static funcDef trustBrTrue with code:C local rV
   rV := TRUST ->code == code
return rV

static funcDef trustBrSeek with code:C local rV
   TRUST ->( dbSeek( code, .T.))
   rV := trustBrTrue( code)
return rV

static funcDef trustBrLast with code:C local rV, seekStr
   TRUST ->( dbSeek( code))
   while TRUST ->code == code .AND. TRUST ->( !eof())
      skip 1 alias TRUST
   enddo
   if !trustBrTrue( code) then TRUST ->( netSkipUp())
   rV := trustBrTrue( code)
return rV

static funcDef trustBrTop with code:C local rV
   if !trustBrSeek( code)
      TRUST ->( mAppend())
      TRUST ->( netUnLock())
   endif
return rV

static funcDef trustBrBtm with code:C local rV
   if !trustBrLast( code)
      TRUST ->( mAppend())
      TRUST ->( netUnLock())
   endif
return rV

static funcDef trustBrDown with code:C local rV
   if rV := TRUST ->( netSkipDown())
      if !( rV := trustBrTrue( code))
         TRUST ->( netSkipUp())
         if !trustBrTrue( code)
            TRUST ->( mAppend())
            TRUST ->( netUnLock())
         endif
      endif
   endif
return rV

static funcDef trustBrUp with code:C local rV
   if rV := TRUST ->( netSkipUp())
      if !( rV := trustBrTrue( code))
         TRUST ->( netSkipDown())
         if !trustBrTrue( code)
            TRUST ->( mAppend())
            TRUST ->( netUnLock())
         endif
      endif
   endif
return rV



funcDef trustGetOrder local text, arr1, arr2, StrSchet, subPriz

TRUSTDOV ->( viewUp( {{ "�롥�� �����⥫�"             } ,;
                      { "��������� ",  {|| TRUSTDOV->work}} ,;
                      { "�������������������"            }} ,;
                     {{ "���", {|| TRUSTDOV ->fio} }}       ,;
                     {|| netSkipUp()}, {|| netSkipDown()}   ,;
                     {|| netGoTop() }, {|| netGoBottom()} ))

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 1))
if ACC ->( dbSeek( TRUST ->schet))
   arr1 := { "�� ������� �।�⢠"                 ,;
             "�������筮 ������� �।�⢠ � �㬬�"   ;
                    + str( TRUST ->summa_1) + " ��.",;
             "�㬬� ������ � ���᫥��� ��業��"   ,;
             "�������筮 �㬬� ���᫥���� ��業⮢" }
   arr2 := { "", rSumma( TRUST ->summa_1), "", ""}
   subPriz := left( ACC ->PRIZ, 3)
   // strSchet := ""
   // if !isPens(ACC ->PRIZ) then strSchet := "� ����� ��� N " + ACC ->schet
   strSchet := "N " + ACC ->schet
else
   messageB( "�� ������ ��� " + TRUST ->schet)
   ACC ->( baseRestPos())
   return {}
endif
ACC ->( baseRestPos())
//if confUseAgent()
// Replaced by TMV 18-02-2004
text := {""                                                     ,;
;// Replaced by TMV 19-04-2004
;//         padC("� � � � � � � � � � � �", 78)                    ,;
;//         padC("N " + allTrim( TRUST ->number ), 78)             ,; // Add by TMV 29-03-2004
         padC("������������ N " + allTrim( TRUST ->number ), 78)   ,;
         "�����-������"                                      ,;
         ""                                                     ,;
			padC( R2TextDate( TRUST ->dateGive), 78)               ,;
;//         "                                N " + TRUST ->number  ,;
         ""                                                     ,;
         "�," + rTrim( cliFullName()) + ','                     ,;
         "�஦����騩(��) �� ����� " + cliFullAddress()        ,;
         cliDocum( .T.)                                         ,;
         "������� "+ upper( rTrim( TRUST ->fio)) + ","          ,;
			"�஦����饬�(��) �� �����:"                          ,;
         TRUST ->address                                        ,;
			trustDocum()                                           ,;
;// Replaced by TMV 19-04-2004
;//         "������� �ਭ������騥 ��� ������� �।�⢠, ��।���� ���� ��",;
;//			'������ � '+confBankName()+', � ���᫥��� ��業�� �� ������ࠬ',;
;//			'������, �ᯨ�뢠���� �� ���� � ᮢ����� �� ����⢨�, �易���',;
;//			'� �⨬ ����祭���.',;
			'�ᯮ�裂���� ���� �������, ��室�騬�� �� ��� N ' + allTrim(TRUST ->schet) + ',' ,;
			'�ᯨ�뢠���� �� ���� � ᮢ����� �� ����⢨�, �易��� � �����',;
			'����祭���.',;
;// Replaced by TMV 19-04-2004 -- End
         ""                                                     ,;
         "�ப ����⢨� ����७���� � "+ rDate(TRUST->dateBeg) ,;
         "                          �� "+ rDate(TRUST->dateEnd) ,;
         ""                                                     ,;
         "������� �����⥫� _________________(__________________________)" ,;
         ""                                                     ,;
         replicate( "=", 60)                                    ,;
         ""                                                     ,;
         ""                                                     ,;
         "����७����� �� ��� " + rTrim( TRUST ->fio)           ,;
         "㤮�⮢����"                                          ,;
         ""                                                     ,;
			'��������� ' + rTrim(TRUSTDOV ->work)                  ,;
         ""                                                     ,;
         '������� _____________ (' + PadR(rTrim(TRUSTDOV ->fio),30) + ')',;
         ""                                                     ,;
         ""                                                     ,;
         ""                                                     ,;
         "   �.�." }

return text

funcDef trustPasSer
return rTrim( TRUST ->pasSer1) + " - " + TRUST ->pasSer2

funcDef trustPasNum
return TRUST ->pasNum

funcDef trustPasGiv
return allTrim(TRUST ->pasGiv)

funcDef trustAddr
return TRUST ->Address

funcDef trustFIO
return upper( rTrim( TRUST ->FIO))




funcDef trustDocum with useENDL:LU local rV, docPtr, docName, endl
   DEFAULT useENDL TO .F.
   endl    := if( useENDL, endl(), "")
   docPtr  := aTrimmedScan( aDocCode(),;
              if( empty( TRUST ->docType), "01", TRUST ->docType))
   docName := rTrim( aDocName()[ docPtr])
	if !empty(allTrim(trustPasNum()) + allTrim(trustPasGiv()) )  // Add by TMV 19-04-2004
	   if docPtr == 1 // ��ᯮ��
   	   rV := docName + " ��� " + trustPasSer() + " N " + trustPasNum() + ;
      	      endl    + " �뤠� " + trustPasGiv()
	   else
   	   rV := "���㬥�� " + docName + " �뤠� " + trustPasGiv()
   	endif
	else                                         // Add by TMV 19-04-2004
		rV = ' '                                  // Add by TMV 19-04-2004
	endif                                        // Add by TMV 19-04-2004
return rV

//
// �����頥� ᯨ᮪ ����७���⥩, �� ����� ����� �뤠��
// ���죨. TRUST ������ ���� �����, � ���� ����� �� ��࠭���
// ������ � ���
//
funcDef trustWorkList with curDate:DU local arr := { }

DEFAULT curDate TO setCurDate()

TRUST ->( baseSavePos())
TRUST ->( dbSetOrder( 2))

TRUST ->( dbSeek( ACC ->schet, .T.))
while TRUST ->schet == ACC ->schet .AND. TRUST ->( !eof())

      //
      // ������ � �������騩 ��ਮ�
      //
      if curDate >= TRUST ->dateBeg .AND. curDate <= TRUST ->dateEnd
         //
         // ����७����� ���������
         //
         if TRUST ->status <= 1
            //
            // ����� ����� �� ���죨 ( <= 1 ) ���
            // ��।������� �㬬� �� �����
            //
            // if chckTrustDate( curDate)
               aAdd( arr, { TRUST ->( recNo()), TRUST ->( od_Gather()) })
            // endif
         endif
      endif

      skip 1 alias TRUST
enddo

TRUST ->( baseRestPos())
return arr

//
// �஢�ઠ ���� � �㬬� �� ���. �����
// �����頥� �㬬�, ������ ����� ॠ�쭮 ���� � �ਧ��� �����
// �� ᭨����
//
funcDef chckTrustDate with curDate:D, theSumma:NU local rV := .F.

theSumma := 0
if TRUST ->type_1 <= 1 .OR. isSR()
   rV       := .T.
   theSumma := sumRest( ACC ->schet, curDate)
else

   if month( curDate) == month( TRUST ->lastDate) .AND. ;
      year(  curDate) == year(  TRUST ->lastDate)

      theSumma := TRUST ->summa_1 - TRUST ->lastSum
      rV       := theSumma > 0
   else
      theSumma := TRUST ->summa_1
      rV := TRUST ->lastDate < curDate
   endif

endif

return rV

//
// ��������� �㬬� � lastSum
//
funcDef trustLastSum with curDate:DU, newSum:N

DEFAULT curDate TO setCurDate()

if TRUST ->type_1 == 2

   if !( month( curDate) == month( TRUST ->lastDate) .AND. ;
         year(  curDate) == year(  TRUST ->lastDate))

      TRUST ->lastDate := curDate
      TRUST ->lastSum  := 0
   endif
   TRUST ->lastSum := TRUST ->lastSum + newSum
endif
return TRUST ->lastSum


funcDef trustRollBackLastSum with schet:C, trNumber:C, summa:N ;
   local rV := .T., code

if left( trNumber, 4) <> "DOV:" .OR. summa >0 then return .T.
trNumber := subStr( trNumber, 5, 8)
ACC ->( baseSavePos())
ACC ->( dbSetOrder( 1))
ACC ->( dbSeek( schet))
code := ACC ->code
ACC ->( baseRestPos())
if empty( code) then return .F.

TRUST ->( baseSavePos())
TRUST ->( dbSetOrder( 1))
if TRUST ->( dbSeek( code + schet))
   while TRUST ->code == code .AND. TRUST ->schet == schet .AND. ;
         TRUST ->( !eof())
      if TRUST ->number == trNumber .AND. TRUST ->type_1 == 2
         if trustRLock()
            TRUST ->lastSum += summa/ 100
            trustUnLock()
         endif
         exit
      endif
      skip 1 alias TRUST
   enddo
endif
TRUST ->( baseRestPos())

return rV


funcDef trustCurDocum with useEndl:LU;
   local get, rV, arr1,    docCode, aDocName, aDocCode, docPtr,;
         canEdit, pasSer1, pasSer2, pasNum,   pasGiv, docName

DEFAULT useEndl TO .F.

docCode := TRUST ->docType
if empty( docCode) then docCode := "06"
docPtr  := aTrimmedScan( aDocCode(), docCode)
canEdit := docPtr == 1

aDocName := aDocName()
aDocCode := aDocCode()

pasSer1 := TRUST ->pasSer1
pasSer2 := TRUST ->pasSer2
pasNum  := TRUST ->pasNum
pasGiv  := TRUST ->pasGiv
get := {{ "��ᯮ��� ����� ����७���� ��� "     },;
        { trustFIO()                                },;
        { "�।��� ���㬥��"                     },;
        { replicate( "�", 50)                       } ,;
        { "��� ���㬥��", block( docPtr), aDocName,,;
               {|g| paspCanClear( g, get, aDocName, aDocCode, @canEdit)}},;
        { "���� ���㬥�� ", block( pasSer1 ),,;
               {|g| paspClear( g, canEdit)}, {|g| chckSer1( g,,canEdit)}} ,;
        { "*-",               block( pasSer2 ),,;
               {|g| paspClear( g, canEdit)}, {|g| chckSer2( g,,canEdit)}} ,;
;// Replaced by TMV  27-02-2003 -- ��࠭ ����஫� ����� ��ᯮ��
        { "*����� ���㬥��", block( pasNum ),"@K 999999"           ,;
               {|g| paspClear( g, canEdit)}} ,;
        { "" },;
        { "���㬥�� �뤠�  ", block( pasGiv)}}

// Replaced by TMV  27-02-2003 -- ��࠭ ����஫� ����� ��ᯮ��
//        { "*����� ���㬥��", block( pasNum ),"@K 999999"           ,;
//               {|g| paspClear( g, canEdit)}, {|g| chckNumb( g,,canEdit)}} ,;

// if trustRLock()
   getUp( 5, 5, get)
//    if getUpdated()
//       TRUST ->docType := aDocCode[ docPtr]
//       trustUnLock()
//    else
//       TRUST ->( netUnLock())
//    endif
// endif

docName := rTrim( aDocName()[ docPtr])
if docPtr == 1 // ��ᯮ��
   rV := docName + " ��� " + rTrim(pasSer1) + "-" + pasSer2 + " N " + ;
         pasNum  + if( useEndl, endl(), "")    + " �뤠� " + pasGiv
else
   rV := "���㬥�� " + docName + if( useEndl, endl(), " ") + pasGiv
endif

return rV

//
// ���ᨢ ��� ���� ��᫥ �뤠� �����
//
funcDef trustGiveInfo with curDocum:CU local text
DEFAULT curDocum TO trustCurDocum()
   text := { "", "", ""                                           ,;
             "�㬬� ����祭� �� ����७����  " + TRUST ->number  ,;
             "��ଫ����� �ࠦ������          " + upper( TRUST ->fio) ,;
             "�஦����饬�(��)      �� ����� " + TRUST ->address ,;
             curDocum                                             ,;
             "����७����� �뤠�� "                               ,;
             "���� �뤠� " + rTrim( TRUST ->placeGive)          ,;
             "��� �뤠� "  + DtoC(  TRUST ->dateGive)           ,;
             "�ਬ�砭��                      " + TRUST ->buffer }
return text

funcDef trustOrderInfo with curDocum:CU local str
DEFAULT curDocum TO trustCurDocum()
   str := "�� ����७���� " + rTrim( TRUST ->number) + " �� " + ;
          DtoC( TRUST ->dateBeg) + ", ��ଫ����� �ࠦ������ " + ;
          upper( rTrim( TRUST ->fio)) + ". " + curDocum
return str


static funcDef trDelShowSchet with schet:C local rV
  rV := subStr( schet, 1, 5) + " " + subStr( schet, 6, 3) + " " + ;
        subStr( schet, 9, 1) + " " + subStr( schet,10, 4) + " " + ;
        subStr( schet,14, 2) + " " + subStr( schet,16, 5)
return rV

function trustDelOrder
local i, text, arr, fio, doc
local _name, _acc, _adr
local _pas:=" ���  __________  �����  ______"
local nam :={ replicate("_",34), replicate("_",34)}
local adr :={ replicate("_",39), replicate("_",39)}
local mil :={ replicate("_",39), replicate("_",39)}


if yesNo( "�� ����⢨⥫쭮 ��� ���㫨஢��� ����७����� ?")

    _acc := splitSSchet( TRUST ->schet)
    _name := cliFullName()
     nam  := razbivka( _name,{{34,"C"},{34,"C"}})
    _pas  := " ���  " + cliPasSer1() + " - " + cliPasSer2() + ;
             "  ����� " + cliPasNum()

    _adr  := cliFullAddress()
     adr  := razbivka(_adr,{{39,"L"},{39,"L"}})
     mil  :=  razbivka( cliPasGiv(),{{39,"L"},{39,"L"}})

     if trustRLock()

        TRUST -> status   := 5
        TRUST -> d_status := SetCurDate()
        //if confUseAgent()
          arr := aClone( servDummy())
          fio := razbivka( cliFullName(),   {{ 40, "L"}, { 40, "L"}})
          adr := razbivka( cliFullAddres(), {{ 35, "L"}, { 35, "L"}})
          doc := razbivka( cliDocum(),      {{ 34, "L"}, { 40, "L"}})
          servDummy( {{ "@datecur",  setCurDate()     },;
                      { "@bankname", confBankName()   },;
                      { "@filial",   confFilial()     },;
                      { "@otd",      confOtdNumb()    },;
                      { "@fio1",     fio[ 1]          },;
                      { "@fio2",     fio[ 2]          },;
                      { "@doc1",     doc[ 1]          },;
                      { "@doc2",     doc[ 2]          },;
                      { "@addr1",    adr[ 1]          },;
                      { "@addr2",    adr[ 2]          },;
                      { "@number",   TRUST ->number   },;
                      { "@datebegin", rDate( TRUST ->dateBeg)},;
                      { "@placegive",TRUST ->fio},; 
                      { "@schet",    trDelShowSchet( TRUST ->schet)    },;
                      { "@rdate",    rDate( setCurDate())}})
          printOpen( "trdel")
          servDummy( arr)/*
        else
          text := { "",;
                    "                                      � " + confOtdName()    ,;
                    '                             ' + confBankName() ,;
                    "                                       ��⥫� " + nam[1]  ,;
                    "                                       "           + nam[2]  ,;
                    "                                      /䠬����,���,����⢮/",;
                    "                             ��ᯮ��" + _pas       ,;
                    "                             �뤠�  " + mil[ 1]    ,;
                    "                                    " + mil[ 2]    ,;
                    "                             ����  " + adr[ 1]    ,;
                    "                                       "+adr[2]    ,;
                    "", ""                                              ,;
                    "                         � � � � � � � � �", ""    ,;
                    "        ���� ��� ����७����� N " + trust->number +;
                    " �� " + rDate( trust ->dateBeg)                    ,;
                    "�뤠���� " + trust->fio                            ,;
                    "�� ����祭�� �������� �।�� � ����� ⥪�饣� ���",;
                    "N "+_acc ,;
                    "� "+ rDate( setCurDate()) + " ����� ������⢨⥫쭮�",;
                    ""                                                  ,;
                    padc( rDate( setCurDate()), 40) + "__________________________________" ,;
                    space(53)                          +                 "�������",;
                    "", "", "" }
            printAOrder( text)
        endif*/
        trustUnLock()
      endif
endif

return NIL

//
// �㭪樨 ��� ��� ���� ����७����
//

function textDayDate(dto)
local sss:={ "��ࢮ�",;
             "��஥",;
             "����",;
             "�⢥�⮥",;
             "��⮥",;
             "��⮥",;
             "ᥤ쬮�",;
             "���쬮�",;
             "����⮥",;
             "����⮥",;
             "��������⮥",;
             "�������⮥",;
             "�ਭ���⮥",;
             "���ୠ��⮥",;
             "��⭠��⮥",;
             "��⭠��⮥",;
             "ᥬ����⮥",;
             "��ᥬ����⮥",;
             "����⭠��⮥",;
             "�����⮥",;
             "������� ��ࢮ�",;
             "������� ��஥",;
             "������� ����",;
             "������� �⢥�⮥",;
             "������� ��⮥",;
             "������� ��⮥",;
             "������� ᥤ쬮�",;
             "������� ���쬮�",;
             "������� ����⮥",;
             "�ਤ�⮥",;
             "�ਤ��� ��ࢮ�" }
return sss[day(dto)]

function RodtextDayDate(dto)
local sss:={ "��ࢮ��",;
             "��ண�",;
             "���쥣�",;
             "�⢥�⮣�",;
             "��⮣�",;
             "��⮣�",;
             "ᥤ쬮��",;
             "���쬮��",;
             "����⮣�",;
             "����⮣�",;
             "��������⮣�",;
             "�������⮣�",;
             "�ਭ���⮣�",;
             "���ୠ��⮣�",;
             "��⭠��⮣�",;
             "��⭠��⮣�",;
             "ᥬ����⮣�",;
             "��ᥬ����⮣�",;
             "����⭠��⮣�",;
             "�����⮣�",;
             "������� ��ࢮ��",;
             "������� ��ண�",;
             "������� ���쥣�",;
             "������� �⢥�⮣�",;
             "������� ��⮣�",;
             "������� ��⮣�",;
             "������� ᥤ쬮��",;
             "������� ���쬮��",;
             "������� ����⮣�",;
             "�ਤ�⮣�",;
             "�ਤ��� ��ࢮ��" }
return sss[day(dto)]

function textMonDate(dto)
local sss:={ "  ﭢ���  ", " 䥢ࠫ�  ", "  ����   ", "  ��५�  ",;
             "   ���    ", "   ���   ", "   ���   ", " ������  ",;
             " ᥭ���� ", " ������  ", "  �����  ", " �������  "}
return sss[month(dto)]

function textYeaDate(dto)
return str(year(dto),4)+" �."

function textDate(dto)
return textDayDate(dto)+textMonDate(dto)+textYeaDate(dto)

function RtextDate(dto)
local str
str:=textDayDate(dto)
return upper(substr(str,1,1))+substr(str,2)+textMonDate(dto)+textYeaDate(dto)

function R2textDate(dto)
local str
str:=textDayDate(dto)
return upper(substr(str,1,1))+substr(str,2)+textMonDate(dto)+textYearDate(dto)


function RodtextDate(dto)
return RodtextDayDate(dto)+textMonDate(dto)+textYeaDate(dto)

function textYearDate(dto)
local arr, rV := "", ptr

arr := {;
{ 1990, "���� ����� �������� ���ﭮ�⮣�"         },;
{ 1991, "���� ����� �������� ���ﭮ�� ��ࢮ��"   },;
{ 1992, "���� ����� �������� ���ﭮ�� ��ண�"   },;
{ 1993, "���� ����� �������� ���ﭮ�� ���쥣�"  },;
{ 1994, "���� ����� �������� ���ﭮ�� �⢥�⮣�"},;
{ 1995, "���� ����� �������� ���ﭮ�� ��⮣�"    },;
{ 1996, "���� ����� �������� ���ﭮ�� ��⮣�"   },;
{ 1997, "���� ����� �������� ���ﭮ�� ᥤ쬮��"  },;
{ 1998, "���� ����� �������� ���ﭮ�� ���쬮��"  },;
{ 1999, "���� ����� �������� ���ﭮ�� ����⮣�"  },;
{ 2000, "��������筮��"                             },;
{ 2001, "��� ����� ��ࢮ��"                        },;
{ 2002, "��� ����� ��ண�"                        },;
{ 2003, "��� ����� ���쥣�"                       },;
{ 2004, "��� ����� �⢥�⮣�"                     },;
{ 2005, "��� ����� ��⮣�"                         },;
{ 2006, "��� ����� ��⮣�"                        },;
{ 2007, "��� ����� ᥤ쬮��"                       },;
{ 2008, "��� ����� ���쬮��"                       },;
{ 2009, "��� ����� ����⮣�"                       }}

ptr := aScan( arr, {|x| x[ 1] == Year( dto) })
if ptr > 0
   rV := arr[ ptr, 2] 
else
   rV := nStr( Year( dto))
endif
return rV + " ����"