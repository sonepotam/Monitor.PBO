*============================================================================
* 
*                  Программа : MON.EXE
*                  Файл      : baseaccn.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: Описание файла доверенностей
*
*----------------------------------------------------------------------------
*
* 26-01-2003  TMV
* При редактировании доверенности теперь НЕ меняется ее номер - trustDovEdit()
*
* 27-02-2003  TMV
* Убрана проверка номера паспорта с помощью ф-ии chckNumb(). Ф-ии trustDovEdit()
*  trustCurDocum().
*
* 05-02-2004  TMV
* В форме доверенности добавлено две строки "Расшифровка подписи".
* Ф-ия trustGetOrder()
*
* 18-02-2004  TMV
* Изменена форма доверенности.
* Ф-ия trustGetOrder()
*
* 29-03-2004  TMV
* Изменена форма доверенности.
* Ф-ия trustGetOrder()
*
* 19-04-2004  TMV
* Изменена форма доверенности. Убран контроль за данными паспорта доверенного.
* Ф-ия trustGetOrder(), trustDocum(), trustDovEdit()
*

#include "common.ch"
#include "clippExt.ch"

* NEW OBJECT FILE

func trustInfo
static dbInfo := { ".\trust.DBF", "TRUST",;
                  {{ "code",      "C", 12, 0},;     // код клиента
                   { "schet",     "C", 20, 0},;     // счет на который выдана доверенность
                   { "dateBeg",   "D",  8, 0},;     // начало действия
                   { "dateEnd",   "D",  8, 0},;     // дата окончания действия
                   { "fio",       "C", 45, 0},;     // имя доверенного лица
                   { "Address",   "C", 50, 0},;     // адрес доверенного лица
                   { "pasSer1",   "C",  8, 0},;     // паспортные данные
                   { "docType",   "C",  2, 0},;
                   { "pasSer2",   "C",  2, 0},;
                   { "pasNum",    "C",  6, 0},;
                   { "pasGiv",    "C", 40, 0},;
                   { "placeGive", "C", 50, 0},;     // место выдачи доверенности
                   { "dateGive",  "D",  8, 0},;     // дата выдачи
                   { "typeGive",  "N",  1, 0},;     // 2 - выдана не на почте
                   { "status",    "N",  1, 0},;     // статус доверенности
                   { "D_status",  "D",  8, 0},;
                   { "lastDate",  "D",  8, 0},;     // дата последнего использования
                   { "lastSum",   "N", 15, 2},;     // сумма за последний месяц
                   { "cntUse",    "N",  3, 0},;     // кол-во сумм снятых
                   { "number",    "C",  8, 0},;     // внутренний номер
                   { "type_1",    "N",  1, 0},;     // <= 1 - все деньги, 2 - не более суммы за месяц
                   { "summa_1",   "N", 15, 2},;     // сумма за месяц
                   { "buffer",    "C", 30, 0},;
                   { "lEdited",   "L",  1, 0}},;    // примечания
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
// Ввод новой доверенности
//
funcDef trustDovNew local i, rV := .F.

  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  if ACC ->( !dbSeek( cliCode()))
     messageB( "У клиента нет счетов !")
     ACC ->( baseRestPos())
     return .F.
  endif
  ACC ->( baseRestPos())

if trustAppend()

   i := dialog( { ;
      "Пенсионер может оформить доверенность на работу со своим  " ,;
      "счетом по месту работы, по месту жительства (в ЖЭКе), у   " ,;
      "нотариуса и т.д. Также оформить доверенность можно и на   " ,;
      "почте.Для ввода данных по выданной в сторонней организации" ,;
      "доверенности выберите пункт :                             " ,;
      "          <Ввод данных по действ. доверенности>           " ,;
      "Выдать доверенность на почте можно, используя пункт :     " ,;
      "             <Оформить доверенность на почте>             "},;
      { "Ввод данных по действ. доверенности" ,;
        "Оформить доверенность на почте" })
   if i = 1
      TRUST ->typeGive := 2
      getUp( 2, 2,;
            {{ "Место выдачи доверенности : ", block( TRUST ->placeGive)},;
             { "Дата выдачи доверенности  : ", block( TRUST ->dateGive)}} )
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
// Редактирование доверенности. Вводится доверенность для
// текущего клиента и счета !
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
arr1 := { "Все денежные средства"                 ,;
          "Ежемесячно денежные средства"          ,;
          "Сумму вклада и начисленные проценты"   ,;
          "Ежемесячно сумму начисленных процентов" }
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
  messageB( "У клиента нет счетов !")
  return .F.
endif


nSchet := max( 1, aScan( aSchet, {|x| x == TRUST ->schet}))

get := {{ "Владелец счета ", block( nSchet), aClone( aSchet),{|| newTrust}},;
        { "*" + chr( 25)                                            } ,;
        { "ФИО     : " + cliFullName()                              } ,;
        { "Паспорт : " + cliPasSer() + " N " + cliPasNum()          } ,;
        { "Адрес   : " + cliFullAddress()                           } ,;
        { replicate( "-", 50)                                       } ,;
        { "ДОВЕРЕННОЕ ЛИЦО          : ",  block( TRUST ->fio),,,{|g|chckName( g)} } ,;
        { "Проживающее по адресу    : ",  block( TRUST ->Address)   } ,;
        { "Тип документа", block( docPtr), aDocName,,;
               {|g| paspCanClear( g, get, aDocName, aDocCode, @canEdit)}},;
;// Replaced by TMV  19-04-2004 -- Убран контроль серии паспорта
        { "Серия документа ", block( TRUST ->pasSer1),,;
;//               {|g| paspClear( g, canEdit)}, {|g| chckSer1( g,,canEdit)}} ,;
               {|g| paspClear( g, canEdit)}} ,;
        { "*-",               block( TRUST ->pasSer2),,;
;//               {|g| paspClear( g, canEdit)}, {|g| chckSer2( g,,canEdit)}} ,;
               {|g| paspClear( g, canEdit)}} ,;
;// Replaced by TMV  19-04-2004 -- Убран контроль серии паспорта -- End
;// Replaced by TMV  27-02-2003 -- Убран контроль номера паспорта
        { "*Номер документа", block( TRUST ->pasNum ),"@K 999999"           ,;
               {|g| paspClear( g, canEdit)}} ,;
        { "" },;
        { "Документ выдан  ", block( TRUST ->pasGiv)} ,;
        { "" }                                                        ,;
        { "ПОЛУЧАТЬ ", block( TRUST ->type_1), arr1,{|| newTrust},{|g| trustChkType( g, arr1, aSchet, @newType_1)} },;
        { "* сумма :", block( TRUST ->summa_1),,{|g| newTrust .AND. canEditSum1( g, newType_1, arr1)},{|g| g:varGet >= 0} },;
        { "" }                                                        ,;
        { "Место выдачи доверенности : ", block( TRUST ->placeGive),,{|| TRUST ->typeGive == 2} } ,;
        { "Дата выдачи доверенности  : ", block( TRUST ->dateGive) ,,{|| TRUST ->typeGive == 2} } ,;
        { "Примечания                : ", block( TRUST ->buffer)    } ,;
        { "" }                                                        ,;
        { "Оформлять доверенность на срок c ", block(TRUST->dateBeg),,{|| newTrust},;
              {|g| tmpDateBeg( g, @tDate)}},;
        { "* по ",                        block( TRUST ->dateEnd)   ,,{|| newTrust},;
              {|g| chkDateEnd( g, tDate )}}}

// Replaced by TMV  27-02-2003 -- Убран контроль номера паспорта
//        { "*Номер документа", block( TRUST ->pasNum ),"@K 999999"           ,;
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
      messageB( "Доверенность не может начинаться ранее " + DtoC( TRUST ->dateGive))
      clear typeAHead
      return .F.
    endif
  endif
return .T.

static funcDef chkDateEnd with g:O, tmpDate:D 
if g:varGet() < tmpDate
   messageB( "Дата окончания меньше даты начала !!!")
   return .F.
endif
if ( g:varGet - tmpDate ) > 365 * 3 
   messageB( "Доверенность можно выдать на срок не более трех лет !")
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
      messageB( "Этот режим доступен только для текущих счетов !!!")
      rV := .F.
   endif
   if ptr == 3 .AND. !isSr()
      messageB( "Этот режим доступен только для срочного вклада !!!")
      rV := .F.
   endif
   if ptr == 4 .AND. !isCanDopVznos()
      messageB( "Этот режим доступен только для накопительного вклада !!!")
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
return { "действует"     ,;
         "закрыт"        ,;
         "приостановлен" ,;
         "удален"        ,;
         "аннулирован"   }



//
// список ВСЕХ доверенностей по текущему клиенту
//
funcDef trustView local aH, aC, fk, b, bUp, bDn, bTop, bBtm, s, rV, opened

aH := {{ "Клиент : " + cliFullName()                  },;
       { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}

aC := {{ "N дов.",                block( TRUST ->number)  },;
       { "Счет",                  block( TRUST ->schet)   },;
       { "Владелец доверенности", block( TRUST ->fio)     },;
       { "Статус",              {, block( TRUST ->status), trustAStatus() }},;
       { "Окончание",             block( TRUST ->dateEnd) }}

fk := {, {{ "Новая" , {|| dovAdd() }      }},,   ;
         {{ "Редакт", {|| dovEdit()}      }},,   ;
         {{ "Аннули", {|| trustDelOrder()}}},,,  ;
         {{ "Печать", {|| trustPrint()}  }}  }


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
      if yesNo( { "У клиента " + cliFullName() + " нет доверенностей",;
                  "", "Вы хотите ввести новую доверенность ?"})
         rV := trustDovNew()
      endif
   endif
   if rV then TRUST ->( browseUp( 2, 2, aH, aC, bUp, bDn, bTop, bBtm,,,,, fk))

   TRUST ->( baseRestPos())
   if !opened then trustClose()

endif

return NIL

static funcDef dovAdd

if yesNo( "Вы хотите ввести новую доверенность ?")
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

TRUSTDOV ->( viewUp( {{ "Выберите доверителя"             } ,;
                      { "Должность ",  {|| TRUSTDOV->work}} ,;
                      { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"            }} ,;
                     {{ "ФИО", {|| TRUSTDOV ->fio} }}       ,;
                     {|| netSkipUp()}, {|| netSkipDown()}   ,;
                     {|| netGoTop() }, {|| netGoBottom()} ))

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 1))
if ACC ->( dbSeek( TRUST ->schet))
   arr1 := { "все денежные средства"                 ,;
             "ежемесячно денежные средства в сумме"   ;
                    + str( TRUST ->summa_1) + " руб.",;
             "сумму вклада и начисленные проценты"   ,;
             "ежемесячно сумму начисленных процентов" }
   arr2 := { "", rSumma( TRUST ->summa_1), "", ""}
   subPriz := left( ACC ->PRIZ, 3)
   // strSchet := ""
   // if !isPens(ACC ->PRIZ) then strSchet := "с моего счета N " + ACC ->schet
   strSchet := "N " + ACC ->schet
else
   messageB( "Не найден счет " + TRUST ->schet)
   ACC ->( baseRestPos())
   return {}
endif
ACC ->( baseRestPos())
//if confUseAgent()
// Replaced by TMV 18-02-2004
text := {""                                                     ,;
;// Replaced by TMV 19-04-2004
;//         padC("Д О В Е Р Е Н Н О С Т Ь", 78)                    ,;
;//         padC("N " + allTrim( TRUST ->number ), 78)             ,; // Add by TMV 29-03-2004
         padC("ДОВЕРЕННОСТЬ N " + allTrim( TRUST ->number ), 78)   ,;
         "Санкт-Петербург"                                      ,;
         ""                                                     ,;
			padC( R2TextDate( TRUST ->dateGive), 78)               ,;
;//         "                                N " + TRUST ->number  ,;
         ""                                                     ,;
         "Я," + rTrim( cliFullName()) + ','                     ,;
         "проживающий(ая) по адресу " + cliFullAddress()        ,;
         cliDocum( .T.)                                         ,;
         "доверяю "+ upper( rTrim( TRUST ->fio)) + ","          ,;
			"проживающему(ей) по адресу:"                          ,;
         TRUST ->address                                        ,;
			trustDocum()                                           ,;
;// Replaced by TMV 19-04-2004
;//         "получать принадлежащие мне денежные средства, переданные мною во",;
;//			'вклады в '+confBankName()+', и начисленные проценты по договорам',;
;//			'вклада, расписываться за меня и совершать все действия, связанные',;
;//			'с этим поручением.',;
			'распоряжаться моим вкладом, находящимся на счете N ' + allTrim(TRUST ->schet) + ',' ,;
			'расписываться за меня и совершать все действия, связанные с данным',;
			'поручением.',;
;// Replaced by TMV 19-04-2004 -- End
         ""                                                     ,;
         "Срок действия доверенности с "+ rDate(TRUST->dateBeg) ,;
         "                          по "+ rDate(TRUST->dateEnd) ,;
         ""                                                     ,;
         "Подпись доверителя _________________(__________________________)" ,;
         ""                                                     ,;
         replicate( "=", 60)                                    ,;
         ""                                                     ,;
         ""                                                     ,;
         "Доверенность на имя " + rTrim( TRUST ->fio)           ,;
         "удостоверяю"                                          ,;
         ""                                                     ,;
			'Должность ' + rTrim(TRUSTDOV ->work)                  ,;
         ""                                                     ,;
         'Подпись _____________ (' + PadR(rTrim(TRUSTDOV ->fio),30) + ')',;
         ""                                                     ,;
         ""                                                     ,;
         ""                                                     ,;
         "   М.П." }

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
	   if docPtr == 1 // паспорт
   	   rV := docName + " серия " + trustPasSer() + " N " + trustPasNum() + ;
      	      endl    + " выдан " + trustPasGiv()
	   else
   	   rV := "Документ " + docName + " выдан " + trustPasGiv()
   	endif
	else                                         // Add by TMV 19-04-2004
		rV = ' '                                  // Add by TMV 19-04-2004
	endif                                        // Add by TMV 19-04-2004
return rV

//
// возвращает список доверенностей, по которым можно выдать
// деньги. TRUST должен быть открыт, и надо стоять на выбранном
// клиенте и счете
//
funcDef trustWorkList with curDate:DU local arr := { }

DEFAULT curDate TO setCurDate()

TRUST ->( baseSavePos())
TRUST ->( dbSetOrder( 2))

TRUST ->( dbSeek( ACC ->schet, .T.))
while TRUST ->schet == ACC ->schet .AND. TRUST ->( !eof())

      //
      // попали в действующий период
      //
      if curDate >= TRUST ->dateBeg .AND. curDate <= TRUST ->dateEnd
         //
         // доверенность действующая
         //
         if TRUST ->status <= 1
            //
            // можно стнять все деньги ( <= 1 ) или
            // определенную сумму за месяц
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
// проверка даты и суммы за посл. месяц
// возвращает сумму, которую можно реально снять и признак можно
// ли снимать
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
// изменение суммы в lastSum
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
get := {{ "Паспортные данные доверенного лица "     },;
        { trustFIO()                                },;
        { "Предъявлен документ"                     },;
        { replicate( "▀", 50)                       } ,;
        { "Тип документа", block( docPtr), aDocName,,;
               {|g| paspCanClear( g, get, aDocName, aDocCode, @canEdit)}},;
        { "Серия документа ", block( pasSer1 ),,;
               {|g| paspClear( g, canEdit)}, {|g| chckSer1( g,,canEdit)}} ,;
        { "*-",               block( pasSer2 ),,;
               {|g| paspClear( g, canEdit)}, {|g| chckSer2( g,,canEdit)}} ,;
;// Replaced by TMV  27-02-2003 -- Убран контроль номера паспорта
        { "*Номер документа", block( pasNum ),"@K 999999"           ,;
               {|g| paspClear( g, canEdit)}} ,;
        { "" },;
        { "Документ выдан  ", block( pasGiv)}}

// Replaced by TMV  27-02-2003 -- Убран контроль номера паспорта
//        { "*Номер документа", block( pasNum ),"@K 999999"           ,;
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
if docPtr == 1 // паспорт
   rV := docName + " серия " + rTrim(pasSer1) + "-" + pasSer2 + " N " + ;
         pasNum  + if( useEndl, endl(), "")    + " выдан " + pasGiv
else
   rV := "Документ " + docName + if( useEndl, endl(), " ") + pasGiv
endif

return rV

//
// массив для печати после выдачи денег
//
funcDef trustGiveInfo with curDocum:CU local text
DEFAULT curDocum TO trustCurDocum()
   text := { "", "", ""                                           ,;
             "Сумма получена по доверенности  " + TRUST ->number  ,;
             "Оформленной гражданину          " + upper( TRUST ->fio) ,;
             "Проживающему(ей)      по адресу " + TRUST ->address ,;
             curDocum                                             ,;
             "Доверенность выдана "                               ,;
             "Место выдачи " + rTrim( TRUST ->placeGive)          ,;
             "Дата выдачи "  + DtoC(  TRUST ->dateGive)           ,;
             "Примечания                      " + TRUST ->buffer }
return text

funcDef trustOrderInfo with curDocum:CU local str
DEFAULT curDocum TO trustCurDocum()
   str := "по доверенности " + rTrim( TRUST ->number) + " от " + ;
          DtoC( TRUST ->dateBeg) + ", оформленной гражданину " + ;
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
local _pas:=" серия  __________  номер  ______"
local nam :={ replicate("_",34), replicate("_",34)}
local adr :={ replicate("_",39), replicate("_",39)}
local mil :={ replicate("_",39), replicate("_",39)}


if yesNo( "Вы действительно хотите аннулировать доверенность ?")

    _acc := splitSSchet( TRUST ->schet)
    _name := cliFullName()
     nam  := razbivka( _name,{{34,"C"},{34,"C"}})
    _pas  := " серия  " + cliPasSer1() + " - " + cliPasSer2() + ;
             "  номер " + cliPasNum()

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
                    "                                      В " + confOtdName()    ,;
                    '                             ' + confBankName() ,;
                    "                                       заявитель " + nam[1]  ,;
                    "                                       "           + nam[2]  ,;
                    "                                      /фамилия,имя,отчество/",;
                    "                             паспорт" + _pas       ,;
                    "                             выдан  " + mil[ 1]    ,;
                    "                                    " + mil[ 2]    ,;
                    "                             адрес  " + adr[ 1]    ,;
                    "                                       "+adr[2]    ,;
                    "", ""                                              ,;
                    "                         З А Я В Л Е Н И Е", ""    ,;
                    "        Прошу мою доверенность N " + trust->number +;
                    " от " + rDate( trust ->dateBeg)                    ,;
                    "выданную " + trust->fio                            ,;
                    "на получение денежных средств с моего текущего счета",;
                    "N "+_acc ,;
                    "с "+ rDate( setCurDate()) + " считать недействительной",;
                    ""                                                  ,;
                    padc( rDate( setCurDate()), 40) + "__________________________________" ,;
                    space(53)                          +                 "подпись",;
                    "", "", "" }
            printAOrder( text)
        endif*/
        trustUnLock()
      endif
endif

return NIL

//
// Функции Леши для печати доверенности
//

function textDayDate(dto)
local sss:={ "первое",;
             "второе",;
             "третье",;
             "четвертое",;
             "пятое",;
             "шестое",;
             "седьмое",;
             "восьмое",;
             "девятое",;
             "десятое",;
             "одиннадцатое",;
             "двенадцатое",;
             "тринадцатое",;
             "четырнадцатое",;
             "пятнадцатое",;
             "шестнадцатое",;
             "семнадцатое",;
             "восемнадцатое",;
             "девятнадцатое",;
             "двадцатое",;
             "двадцать первое",;
             "двадцать второе",;
             "двадцать третье",;
             "двадцать четвертое",;
             "двадцать пятое",;
             "двадцать шестое",;
             "двадцать седьмое",;
             "двадцать восьмое",;
             "двадцать девятое",;
             "тридцатое",;
             "тридцать первое" }
return sss[day(dto)]

function RodtextDayDate(dto)
local sss:={ "первого",;
             "второго",;
             "третьего",;
             "четвертого",;
             "пятого",;
             "шестого",;
             "седьмого",;
             "восьмого",;
             "девятого",;
             "десятого",;
             "одиннадцатого",;
             "двенадцатого",;
             "тринадцатого",;
             "четырнадцатого",;
             "пятнадцатого",;
             "шестнадцатого",;
             "семнадцатого",;
             "восемнадцатого",;
             "девятнадцатого",;
             "двадцатого",;
             "двадцать первого",;
             "двадцать второго",;
             "двадцать третьего",;
             "двадцать четвертого",;
             "двадцать пятого",;
             "двадцать шестого",;
             "двадцать седьмого",;
             "двадцать восьмого",;
             "двадцать девятого",;
             "тридцатого",;
             "тридцать первого" }
return sss[day(dto)]

function textMonDate(dto)
local sss:={ "  января  ", " февраля  ", "  марта   ", "  апреля  ",;
             "   мая    ", "   июня   ", "   июля   ", " августа  ",;
             " сентября ", " октября  ", "  ноября  ", " декабря  "}
return sss[month(dto)]

function textYeaDate(dto)
return str(year(dto),4)+" г."

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
{ 1990, "одна тысяча девятьсот девяностого"         },;
{ 1991, "одна тысяча девятьсот девяносто первого"   },;
{ 1992, "одна тысяча девятьсот девяносто второго"   },;
{ 1993, "одна тысяча девятьсот девяносто третьего"  },;
{ 1994, "одна тысяча девятьсот девяносто четвертого"},;
{ 1995, "одна тысяча девятьсот девяносто пятого"    },;
{ 1996, "одна тысяча девятьсот девяносто шестого"   },;
{ 1997, "одна тысяча девятьсот девяносто седьмого"  },;
{ 1998, "одна тысяча девятьсот девяносто восьмого"  },;
{ 1999, "одна тысяча девятьсот девяносто девятого"  },;
{ 2000, "двухтысячного"                             },;
{ 2001, "две тысячи первого"                        },;
{ 2002, "две тысячи второго"                        },;
{ 2003, "две тысячи третьего"                       },;
{ 2004, "две тысячи четвертого"                     },;
{ 2005, "две тысячи пятого"                         },;
{ 2006, "две тысячи шестого"                        },;
{ 2007, "две тысячи седьмого"                       },;
{ 2008, "две тысячи восьмого"                       },;
{ 2009, "две тысячи девятого"                       }}

ptr := aScan( arr, {|x| x[ 1] == Year( dto) })
if ptr > 0
   rV := arr[ ptr, 2] 
else
   rV := nStr( Year( dto))
endif
return rV + " года"