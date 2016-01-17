*============================================================================
* 
*                  Программа : MON.EXE
*                  Файл      : baseclie.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: Описание общих операций для базы клиентов
*----------------------------------------------------------------------------
*
*   cliInfo()                -> dbInfo
*   cliPath( [ newPath])     -> dbInfo
*   cliOpen()                -> T/F
*   cliXOpen()               -> T/F
*   cliClose()               -> NIL
*   cliCreate()              -> NIL
*   cliSeek( par, lSoftSeek) -> T/F
*
*   cliSurname( [ newValue]) -> oldValue    фамилия
*   cliName(    [ newValue]) -> oldValue    имя
*   cliSName(   [ newValue]) -> oldValue    отчество
*   cliCode(    [ newValue]) -> oldValue    код
*   cliPasSer1( [ newValue]) -> oldValue    первая часть серии паспорта
*   cliPasSer2( [ newValue]) -> oldValue    вторая часть серии паспорта
*   cliPasNum(  [ newValue]) -> oldValue    номер паспорта
*   cliPasGiv(  [ newValue]) -> oldValue    паспорт выдан
*   cliIndex(   [ newValue]) -> oldValue    индекс
*   cliCity(    [ newValue]) -> oldValue    город
*   cliStreet(  [ newValue]) -> oldValue    улица
*   cliHouse(   [ newValue]) -> oldValue    дом
*   cliBlock(   [ newValue]) -> oldValue    корпус
*   cliFlat(    [ newValue]) -> oldValue    квартира
*   cliPhone(   [ newValue]) -> oldValue    телефон
*   cliBuffer(  [ newValue]) -> oldValue    буфер
*
*   cliFullName()            -> Value       полное ФИО
*   cliPasSer()              -> Value       серия паспорта
*   cliFullAddress()         -> Value       полный адрес ( строка )
*
*   cliAppend()              -> T/F
*   cliRLock()               -> T/F
*   cliUnLock()              -> NIL
*
*   cliEdit()                -> T/F         редактирование данных о клиенте
*   cliNewClient()           -> T/F         ввод нового клиента
*----------------------------------------------------------------------------
*
* 21-10-2002 TMV
* Исправлена ф-ия cliInfo(), cliEdit(), cliKodYl(), _chckAddr(), monInit
*   - добавлено поле KODYL
*
* 13-11-2002 TMV
* Добавлена операция уменьшения лимита кредитования - Alt-F4
* Ф-ия cliView(), добавлена ф-ия cliLimitEdit(), cliLimit(),
*   getWiscLimit(), chkLimitSum(), 
*
* 20-11-2002 TMV
* При операции уменьшения лимита выключена генерация записи пожелания
*  949  в WISC. Ф-ия cliLimitEdit().
*
* 27-11-2002 TMV
* Добавлена ф-ия просмотра состояния подписи заявления клиента по Ctrl+F4
* в ф-ии cliView()
*
* 09-12-2002 TMV
* Из проверки адреса убран контроль улицы для НЕВладика. Ф-ия _chckAddr()
*
* 05-01-2003 TMV
* Введена ф-ия чтения настройки по KODYL. Ф-ия cliInfo()
*
* 20-01-2003 TMV
* Изменен формат вывода данных для паспорта РФ - ф-ия cliDocum()
*
* 02-12-2003 TMV
* Возвращен вариант, когда наличие поля KODYL в файле CLIENTS зависит
* от параметра UPSC.
* Ф-ия cliInfo()
*
* 25-03-2004 TMV
* Новый порядок работы с картами через OpenWay
* Ф-ии cardDocShow()
* Новая ф-ия cliCardList()
*
* 29-03-2004 TMV
* Новый порядок работы с картами через OpenWay - новые документы.
* Ф-ии cardDocShow()
*
* 03-04-2004 TMV
* Новый параметр в вызове ф-ии wsCardList()
* Ф-ия cliCardList()
*
* 15-04-2004 TMV
* Вставлена проверка на наличие списка типов карт.
* Ф-ия cliCardList()
*
* 29-06-2004 TMV
* Новая ф-ия для получения названия док-та getDocumName()
*

#include "common.ch"
#include "clippExt.ch"
#include "inkey.ch"

#include "mon.ch"

static isKodYlExist := .F.  // Add by TMV  21-10-2002

static isLimitDolg := .F.   // Add by TMV  26-02-2003

static isLineNoStrah := .F.   // Add by TMV  21-04-2003

func cliInfo
// Replaced by TMV <static> to <local>  21-04-2003
static dbInfo := {".\CLIENTS.DBF", "CLIENTS", {},;
						{{ , "code"                              } ,;
                   { , 'Surname + " " + Name + " " + sName'} ,;
                   { , "lEdited"                           } ,;
                   { , "pasNum"                            }},;
						0}

if useKodYl()  // Add by TMV  21-10-2002
	isKodYlExisi := .T.
endif

if useLimitDolg()           // Add by TMV  26-02-2003
	isLimitDolg := .T.       // Add by TMV  26-02-2003
endif                       // Add by TMV  26-02-2003

if useLineAllNoStrah()     // Add by TMV  21-04-2003 And EDIT 06-06-2003
	isLineNoStrah := .T.    // Add by TMV  21-04-2003
endif                      // Add by TMV  21-04-2003

//Replaced by TMV // Add by TMV  21-04-2003
//And come back by TMV 02-12-2003
if isKodYlExisi
   dbInfo[3] :=    {{ "code",     "C", 12, 0},;
                   { "pasSer1",  "C",  8, 0},;
                   { "pasSer2",  "C",  2, 0},;
                   { "pasNum",   "C",  6, 0},;
                   { "pasGiv",   "C", 40, 0},;
                   { "surname",  "C", 20, 0},;
                   { "name",     "C", 20, 0},;
                   { "sName",    "C", 20, 0},;
                   { "iIndex",   "C",  6, 0},;
                   { "City",     "C", 15, 0},;
                   { "Street",   "C", 30, 0},;
                   { "House",    "C", 10, 0},;
                   { "Block",    "C",  2, 0},;
                   { "Flat",     "C",  4, 0},;
                   { "Phone",    "C", 11, 0},;
                   { "buffer",   "C", 10, 0},;
                   { "distrCode","N",  2, 0},;
                   { "credSumma","N", 15, 0},;
                   { "minSum",   "N", 15, 0},;
                   { "birth_Date", "D",8, 0},;
                   { "colibri",  "C", 10, 0},;
                   { "lEdited",  "L",  1, 0},;
                   { "KODYL",    "N",  4, 0}}  // Add by TMV  21-10-2002
else
   dbInfo[3] :=    {{ "code",     "C", 12, 0},;
                   { "pasSer1",  "C",  8, 0},;
                   { "pasSer2",  "C",  2, 0},;
                   { "pasNum",   "C",  6, 0},;
                   { "pasGiv",   "C", 40, 0},;
                   { "surname",  "C", 20, 0},;
                   { "name",     "C", 20, 0},;
                   { "sName",    "C", 20, 0},;
                   { "iIndex",   "C",  6, 0},;
                   { "City",     "C", 15, 0},;
                   { "Street",   "C", 30, 0},;
                   { "House",    "C", 10, 0},;
                   { "Block",    "C",  2, 0},;
                   { "Flat",     "C",  4, 0},;
                   { "Phone",    "C", 11, 0},;
                   { "buffer",   "C", 10, 0},;
                   { "distrCode","N",  2, 0},;
                   { "credSumma","N", 15, 0},;
                   { "minSum",   "N", 15, 0},;
                   { "birth_Date", "D",8, 0},;
                   { "colibri",  "C", 10, 0},;
                   { "lEdited",  "L",  1, 0}}
endif
// Add by TMV  21-04-2003
	if isLineNoStrah
		aAdd( dbInfo[3], { "CREDLINE", "N", 12, 0})
		aAdd( dbInfo[3], { "CREDDATE", "D",  8, 0})
		aAdd( dbInfo[3], { "CRD_NUM",  "N",  6, 0})
		aAdd( dbInfo[3], { "CREDOPEN", "D",  8, 0})
	endif
return dbInfo

static procedure cHelp()
  local arr:=;
{  "Команды редактирования таблицы                                     ", ;
   "───────────────────────────────────────────────────────────────────", ;
   "Ключ                                Действие                       ", ;
   "───────────────────────────────────────────────────────────────────", ;
   "<Стрелки>                           Перемещения курсора            ", ;
   "<Home>                              Сдвиг в начало видимой строки  ", ;
   "<End>                               Сдвиг в конец  видимой строки  ", ;
   "<Ctrl>-<Home>                       Сдвиг в начало строки таблицы  ", ;
   "<Ctrl>-<End>                        Сдвиг в конец  строки таблицы  ", ;
   "<PgUp>                              Сдвиг на страницу вверх        ", ;
   "<PgDn>                              Сдвиг на страницу вниз         ", ;
   "<Ctrl>-<PgUp>                       Сдвиг в начало таблицы         ", ;
   "<Ctrl>-<PgDn>                       Сдвиг в конец  таблицы         ", ;
   "<Insert>                            Вставить строку таблицы        ", ;
   "<Enter>                             Редактирование элемента        ", ;
   "<Esc>                               Закончить редактирование       ", ;
   "───────────────────────────────────────────────────────────────────", ;
   " Остальные возможные операциии осуществляются при нажатии на клави-", ;
   "ши Shift, Ctrl, Alt. При этом изменяются подсказки, находящиеся на ", ;
   "последней строке экрана."   }

  helpHandler( arr)
return

funcDef cliPath with newPath:CU
return basePath( cliInfo(), newPath)

funcDef cliOpen
return baseMOpen( cliInfo())

funcDef cliXOpen
return baseMXOpen( cliInfo())

funcDef cliClose
return baseMClose( cliInfo())

funcDef cliCreate
return baseCreate( cliInfo())

funcDef cliSeek with par:C, softSeek:LU
return CLIENTS ->( netSeek( par, softSeek))

funcDef cliSurname with newValue:CU local oldValue
oldValue := CLIENTS ->surName
if !isNIL( newValue) then CLIENTS ->surName := newValue
return oldValue

funcDef cliName with newValue:CU local oldValue
oldValue := CLIENTS ->Name
if !isNIL( newValue) then CLIENTS ->Name := newValue
return oldValue

funcDef cliSName with newValue:CU local oldValue
oldValue := CLIENTS ->sName
if !isNIL( newValue) then CLIENTS ->sName := newValue
return oldValue

funcDef cliCode with newValue:CU local oldValue
oldValue := CLIENTS ->code
if !isNIL( newValue) then CLIENTS ->code := newValue
return oldValue

funcDef cliPasSer1 with newValue:CU local oldValue
oldValue := CLIENTS ->pasSer1
if !isNIL( newValue) then CLIENTS ->pasSer1 := newValue
return oldValue

funcDef cliPasSer2 with newValue:CU local oldValue
oldValue := CLIENTS ->pasSer2
if !isNIL( newValue) then CLIENTS ->pasSer2 := newValue
return oldValue

funcDef cliPasSer
return rTrim( cliPasSer1()) + "-" + rTrim( cliPasSer2())

funcDef cliPasNum with newValue:CU local oldValue
oldValue := CLIENTS ->pasNum
if !isNIL( newValue) then CLIENTS ->pasNum := newValue
return oldValue

funcDef cliPasGiv with newValue:CU local oldValue
oldValue := CLIENTS ->pasGiv
if !isNIL( newValue) then CLIENTS ->pasGiv := newValue
return oldValue

funcDef cliDocum with useENDL:LU ;
   local rV, docPtr, docName, endl, docType, pasSer, pasNum
   DEFAULT useENDL     TO .F.
   endl    := if( useENDL, endl(), " ")
   docType := left( CLIENTS ->buffer, 2)
   docPtr  := aTrimmedScan( aDocCode(), if( empty( docType), "01",docType))
   docName := rTrim( aDocName()[ docPtr])
   if docPtr == 1 .Or. docPtr == 2 // паспорт и паспорт РФ
     pasSer := cliPasSer()
     pasNum := cliPasNum()
// Replaced by TMV  20-01-2003
     if left( allTrim( pasSer ), 1) $ "0,1,2,3,4,5,6,7,8,9"
       rV := "Паспорт " + rTrim( cliPasSer1()) + " " + ;
             rTrim( cliPasSer2()) + " " + pasNum + endl + "выдан " + cliPasGiv()
     else
       rV := docName + " серия " + PasSer + " N " + PasNum + endl + "выдан " + cliPasGiv()
     endif
   else
      rV := "Документ " + docName + " " + cliPasGiv()
   endif
return rV


funcDef cliDocum2 with par1:CU, par2:CU local rV, docPtr, docName, endl, docType
   docType := left( CLIENTS ->buffer, 2)
   docPtr  := aTrimmedScan( aDocCode(), if( empty( docType), "01",docType))
   docName := rTrim( aDocName()[ docPtr])
   if docPtr == 1 .Or. docPtr == 2 // паспорт и пасаорт РФ
      par1 := docName + " серия " + cliPasSer() + " N " + cliPasNum()
      par2 := "выдан " + cliPasGiv()
      rV   := docName + " серия " + cliPasSer() + " N " + cliPasNum() + ;
              "выдан " + cliPasGiv()
   else
      par1 := "Документ " + docName
      par2 := cliPasGiv()
      rV   := "Документ " + docName + " " + cliPasGiv()
   endif
return rV


funcDef cliIndex with newValue:CU local oldValue
oldValue := CLIENTS ->iIndex
if !isNIL( newValue) then CLIENTS ->iIndex := newValue
return oldValue

funcDef cliCity with newValue:CU local oldValue
oldValue := CLIENTS ->city
if !isNIL( newValue) then CLIENTS ->city := newValue
return oldValue

funcDef cliStreet with newValue:CU local oldValue
oldValue := CLIENTS ->Street
if !isNIL( newValue) then CLIENTS ->Street := newValue
return oldValue

// Add by TMV  21-10-2002
funcDef cliKodYl with newValue:NU local oldValue
oldValue := CLIENTS ->KODYL
if !isNIL( newValue) then CLIENTS ->KODYL := newValue
return oldValue

funcDef cliHouse with newValue:CU local oldValue
oldValue := CLIENTS ->house
if !isNIL( newValue) then CLIENTS ->house := newValue
return oldValue

funcDef cliBlock with newValue:CU local oldValue
oldValue := CLIENTS ->block
if !isNIL( newValue) then CLIENTS ->block := newValue
return oldValue

funcDef cliFlat with newValue:CU local oldValue
oldValue := CLIENTS ->flat
if !isNIL( newValue) then CLIENTS ->flat := newValue
return oldValue

funcDef cliPhone with newValue:CU local oldValue
oldValue := CLIENTS ->phone
if !isNIL( newValue) then CLIENTS ->phone := newValue
return oldValue

funcDef cliBDate with newValue:DU local oldValue
oldValue := CLIENTS ->birth_Date
if !isNIL( newValue) then CLIENTS ->birth_Date := newValue
return oldValue

funcDef cliBuffer with newValue:CU local oldValue
oldValue := CLIENTS ->buffer
if !isNIL( newValue) then CLIENTS ->buffer := newValue
return oldValue

funcDef cliFullName
return rTrim( CLIENTS ->Surname) + ' ' + ;
       rTrim( CLIENTS ->name)    + ' ' + ;
       rTrim( CLIENTS ->sName)


funcDef cliFullAddress local rV := ""

if !empty( CLIENTS ->iIndex) then rV += CLIENTS ->iIndex + ","
if !empty( CLIENTS ->city)   then rV += rTrim( CLIENTS ->city)  + ", "
if !empty( CLIENTS ->Street) then rV += rTrim( CLIENTS ->Street) + ", "
if !empty( CLIENTS ->House)  then rV += "д."    + rTrim( CLIENTS ->House) + ", "
if !empty( CLIENTS ->Block)  then rV += "корп." + rTrim( CLIENTS ->Block) + ", "
if !empty( CLIENTS ->Flat)   then rV += "кв."   + rTrim( CLIENTS ->Flat)

if right( rV, 1) = "," then rV := left( rV, len( rV) - 1)
return rV

funcDef cliAppend
return CLIENTS ->( mAppend())

funcDef cliRLock
return CLIENTS ->( mRLock())

funcDef cliUnLock
   CLIENTS ->lEdited := .T.
return CLIENTS ->( netUnLock())

funcDef cliNextCode local rV, theDate, theDay, theMonth, theYear, arr
rV := confNextCliCode()

return rV

funcDef aDocName local arr
   arr := { "Паспорт                                          ",;
            "Паспорт гражданина РФ                            ",;
            "Свидетельство о рождении                         ",;
            "Удостоверение личности офицера,мичмана,прапорщика",;
            "Справка об освобождении из мест лишения свободы  ",;
            "Временное удостоверение                          ",;
            "Заграничный паспорт                              ",;
            "Дипломатический паспорт                          ",;
            "Военный билет рядового,сержанта,матроса          ",;
            "Иные                                             ",;
            "Не предъявлен                                    "}
return arr

funcDef aDocCode local arr
   arr := { "01",;
            "21",;
            "02",;
            "04",;
            "06",;
            "07",;
            "03",;
            "08",;
            "05",;
            "09",;
            "00" }
return arr

funcDef cliDocument local arr := {}, docCode, docPtr
   docCode := left( CLIENTS ->buffer, 2)
   if empty( docCode) then docCode := "06"
   docPtr  := aTrimmedScan( aDocCode(), docCode)
   if docPtr == 1
      aAdd( arr, "паспорт :" + cliPasSer() + " N " + cliPasNum())
      aAdd( arr, "выдан   :" + cliPasGiv())
   elseif docPtr == 2   //  Add by TMV  13-11-2001
      aAdd( arr, rTrim( aDocName()[ docPtr])+":"+cliPasSer() + " " + cliPasNum())
      aAdd( arr, "выдан   :" + cliPasGiv())
   else
      aAdd( arr, rTrim( aDocName()[ docPtr]) + " : " + cliPasGiv())
   endif
return arr

funcDef cliGetHome with nD:N local rV := .F.
  nD := 0
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  accSeek( cliCode())
  while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
//  Replaced by TMV  20-05-2002
    if isPens( ACC ->PRIZ) .AND. ACC ->typeOf == "00"
       nD := val( subStr( ACC ->buffer, 2, 2))
       if left(   ACC ->buffer, 1) = "!" then rV := .T.
       exit
    endif
    skip 1 alias ACC
  enddo
  ACC ->( baseRestPos())
return rV

funcDef cliSetHome with isHome:L, nD:N
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  accSeek( cliCode())
  while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
    if isPens( ACC ->PRIZ) .AND. ACC ->typeOf == "00"
       if ACC ->( mRLock())
          if isHome
             accSetHome( nD)
          else
             ACC ->buffer := " " + subStr( ACC ->buffer, 2)
          endif
          accUnLock()
       endif
    endif
    skip 1 alias ACC
  enddo
  ACC ->( baseRestPos())
return NIL


funcDef cliEdit with newClient:LU, retCode:NU ;
   local rV, get, aDistr, aCode, nDistr := 1, docPtr, canEdit,;
   docCode, aDocName, aDocCode, nD := 0, ptrHome := 1, ptrRes

DEFAULT newClient TO .F.

retCode := 0
distrArray( @aDistr, @aCode)
nDistr := max( 1, CLIENTS ->distrCode)
nDistr := aScan( aCode, {|x| x == nDistr })

aDocName := aClone( aDocName())
aDocCode := aClone( aDocCode())
docPtr := aTrimmedScan( aDocCode, "09")
if docPtr > 0
   aDel( aDocName, docPtr); aSize( aDocName, len( aDocName) - 1)
   aDel( aDocCode, docPtr); aSize( aDocCode, len( aDocCode) - 1)
endif

docCode := left( CLIENTS ->buffer, 2)
if empty( docCode) then docCode := "01"
docPtr  := aTrimmedScan( aDocCode, docCode)
canEdit := docPtr == 1
ptrRes  := max( 1, val( subStr( CLIENTS ->buffer, 3, 1)))

get := {{ upper( "Введите данные по клиенту :") } ,;
        { "" } ,;
        { "Фамилия       ", funBlock( cliSurname),,{|| newClient},{|g| chckName( g)}} ,;
        { "Имя           ", funBlock( cliName)   ,,{|| newClient},{|g| chckName( g)}} ,;
        { "*Отчество ",     funBlock( cliSName)  ,,{|| newClient},{|g| chckName( g), .T.}} ,;
        { "Дата рождения ", funBlock( cliBDate),,, {|g| chckBDate( g:varGet)}} ,;
        { "*          Резидент ", block( ptrRes), { "Резидент", "Нерезидент" },{|| newClient} },;
        { "---------------------------------------------------------------------------" } ,;
        { "ДАHHЫЕ О ДОКУМЕHТЕ :" } ,;
        { "Тип документа", block( docPtr), aDocName,,;
               {|g| paspCanClear( g, get, aDocName, aDocCode, @canEdit)}},;
        { "Серия документа ", funBlock( cliPasSer1),"@K XXXXX"},;
        { "*-",               funBlock( cliPasSer2),"@K XX" },;
        { "*Номер документа", funBlock( cliPasNum ),"@K 999999"},;
        { "" },;
        { "Документ выдан  ", funBlock( cliPasGiv)} ,;
        { "---------------------------------------------------------------------------" } ,;
        { "АДРЕС :" } ,;
        { "Индекс ",         funBlock( cliIndex  )} ,;
        { "*Нас.пункт ",     funBlock( cliCity   )} ,;
        { "" },;
        { "Улица ",          funBlock( cliStreet )} ,;
        { "" },;
        { "Дом ",            funBlock( cliHouse  )} ,;
        { "*Корпус",         funBlock( cliBlock  )} ,;
        { "*Квартира",       funBlock( cliFlat   )} ,;
        { "*Телефон",        funBlock( cliPhone  )} ,;
        { "---------------------------------------------------------------------------" } ,;
        { "Участок доставки пенсии ", block( nDistr), aDistr},;
        { "Надомник                ", block( ptrHome), { "Нет", "Да"}},;
        { "*День выплаты", block( nD), "99",,{|g| chknD( g)}          }}

if isKodYlExisi
	get[22] := { "Код улицы ",   funBlock( cliKodYl )}
endif

if cliGetHome( @nD) then ptrHome := 2

if ( rV := getUp( 1, 1, get, {|| cliRLock()}, {|| cliUnLock()}))
  if empty( cliSurName()) .OR. empty( cliName()) .OR. ; // проверка ФИО
     !_chckPasp( aDocCode()[ docPtr])            .OR. ; // проверка паспорта
     !_chckAddr()                                       // проверка адреса
      rV := .F.
      cliRLock()
  else
    if newClient
      retCode := _seekTheSame( aDocCode()[ docPtr])
      if !empty( retCode)
        if dialog( ;
             { "Найден клиент с таким же ФИО и паспортными данными.", ""},;
             { "Перейти к этому клиенту", "Работать с НОВЫМ клиентом" }) <> 1
           retCode := 0
        endif
      endif
    endif
    if cliRLock()
      if getUpdated() then cliSetHome( ptrHome = 2, nD)
      CLIENTS ->distrCode := aCode[ nDistr]
      CLIENTS ->buffer    := aDocCode[ docPtr] + nStr( ptrRes) + ;
        subStr( CLIENTS ->buffer, 4)
    endif
  endif
endif
return rV

//
// поиск такого же клиента
//
static funcDef _seekTheSame with paspCode:C ;
  local retCode := 0, seekStr, paspSer, paspNum, paspGiv, recNo

  seekStr  := CLIENTS ->surName + " " + CLIENTS ->Name + " " + CLIENTS ->sName
  paspSer  := cliPasSer1() + "-" + cliPasSer2()
  paspNum  := cliPasNum()
  paspGiv  := strTran( cliPasGiv(), " ", "")
  recNo    := CLIENTS ->( recNo())

  CLIENTS ->( baseSavePos())
  CLIENTS ->( dbSetOrder( 2))
  if CLIENTS ->( dbSeek( seekStr))
     while CLIENTS ->( &(indexKey( 0))) == seekStr .AND. CLIENTS ->( !eof())
       if CLIENTS ->( recNo()) <> recNo
         if paspCode == left( CLIENTS ->buffer, 2)
           if paspCode == "01"
             if paspSer == cliPasSer1() + "-" + cliPasSer2() .AND. ;
                paspNum == cliPasNum()
                retCode := CLIENTS ->( recNo())
             endif
           else
             if paspGiv == strTran( cliPasGiv(), " ", "")
                retCode := CLIENTS ->( recNo())
             endif
           endif
         endif
       endif
       skip 1 alias CLIENTS
     enddo
  endif

  CLIENTS ->( baseRestPos())
return retCode
//
// проверка адреса при вводе
//
static funcDef _chckAddr local rV := .T.
   rV := !empty( cliCity()).AND. !empty( cliHouse()) // Add by TMV  09-12-2002
	if isKodYlExisi                         // Add by TMV  21-10-2002   
		rV = (rV .And. !empty(cliKodYl()))   // Add by TMV  21-10-2002
	endif                                 // Add by TMV  21-10-2002
   if !rV then messageB( "Адрес введен не полностью !")
return rV

//
// проверка паспорта при вводе
//
static funcDef _chckPasp with docCode:C local rV := .T.
  do case
     case docCode == "00" do rV := .F.
     case docCode == "01"
       rV := !empty( cliPasSer1()) .AND. !empty( cliPasSer2()) .AND. ;
             !empty( cliPasNum())  .AND. !empty( cliPasGiv())
     otherwise
       rV := !empty( cliPasGiv())
  endcase
  if !rV then messageB( "Паспорт введен не полностью !")
return rV

static funcDef chknD with g:O local rV := .T.
   if g:varGet < 0
      messageB( "Вы должны ввести положительное число !")
      rV := .F.
   endif
   if g:varGet > 31
      messageB( "В месяце не может быть больше 31 дня !!!")
      rV := .F.
   endif
return rV

funcDef chckBDate with bDate:D local rV := .T.
if !empty( bDate) .AND. bDate > setCurDate()
   messageB( "Эта дата еще не наступила !!!")
   return .F.
endif
return rV

funcDef fizEmptyPasp
return empty( cliPasSer1()) .OR. empty( cliPasSer2()) .OR. ;
       empty( cliPasNum())   .OR. empty( cliPasGiv())

funcDef fizEmptyName
return empty( cliSurName()) .OR. empty( cliName())


funcDef chckName with g:O local len := len( g:varGet())
  g:varPut( padR( upper( allTrim( g:varGet())), len ))
return sayIfEmpty( g:varGet())

funcDef sayIfEmpty with value:anyType local rV := .T.
if empty( value) then messageB( "Это поле не может быть пустым"); rV := .F.
return rV

return rV

funcDef cliNewClient local rV := .F., retCode

if cliAppend()
   cliCode( cliNextCode())
   rV := cliEdit( .T., @retCode)
   if !rV .OR. !empty( retCode)
      CLIENTS ->( clearRec())
      CLIENTS ->( netUnLock())
      if !empty( retCode)
        CLIENTS ->( dbGoTo( retCode))
        rV := .T.
      endif
   else
      rV := .T.
      cliUnLock()
   endif
endif

return rV

funcDef cliTop with b:BU
return CLIENTS ->( netGoTop( b))

funcDef cliDown with b:BU
return CLIENTS ->( netSkipDown( b))

funcDef cliUp with b:BU
return CLIENTS ->( netSkipUp( b))

funcDef cliBottom with b:BU
return CLIENTS ->( netGoBottom( b))

funcDef cliFio
return rTrim( cliSurName()) + " " + left( cliName(), 1) + "." + ;
       left( cliSName(), 1) + "."

funcDef cliView ;
        local aH, aC, fk, cStr, aF, bUp, bDn, bTop, bBtm, bH, str,;
              userDefineKeys, bApp, bDel, oB, aSTr, arr

arr := ;
{  "Команды редактирования таблицы                                     ", ;
   "───────────────────────────────────────────────────────────────────", ;
   "Ключ                                Действие                       ", ;
   "───────────────────────────────────────────────────────────────────", ;
   "<Стрелки>                           Перемещения курсора            ", ;
   "<Home>                              Сдвиг в начало видимой строки  ", ;
   "<End>                               Сдвиг в конец  видимой строки  ", ;
   "<Ctrl>-<Home>                       Сдвиг в начало строки таблицы  ", ;
   "<Ctrl>-<End>                        Сдвиг в конец  строки таблицы  ", ;
   "<PgUp>                              Сдвиг на страницу вверх        ", ;
   "<PgDn>                              Сдвиг на страницу вниз         ", ;
   "<Ctrl>-<PgUp>                       Сдвиг в начало таблицы         ", ;
   "<Ctrl>-<PgDn>                       Сдвиг в конец  таблицы         ", ;
   "<Insert>                            Вставить строку таблицы        ", ;
   "<Enter>                             Редактирование элемента        ", ;
   "<Esc>                               Закончить редактирование       ", ;
   "───────────────────────────────────────────────────────────────────", ;
   " Остальные возможные операциии осуществляются при нажатии на клави-", ;
   "ши Shift, Ctrl, Alt. При этом изменяются подсказки, находящиеся на ", ;
   "последней строке экрана."   }

if monOpen()

   if !fast() then isNak()

//  Данные в филиал не отправлены
if !chkOdClose()  //  Add by TMV  15-01-2002
   fk   := {, {{ "Операции", {|| cliOperations()}}  ,;
               { "Процен",   {|| ChangePcts()   }}  ,;
               { "Сообщ",    {|| spr_InfoWork() }}  ,;
               { "Уведом",   {|| notifPrint()   }}} ,;
              {{ "Касса",    {|| cliKassa()     }}  ,;
               { "Доход",    {|| incServSprav() }}} ,;
              {{ "Редакт",   {|| cliEdit()      }}  ,;
					,;
               { "ЗаявПр",   {|| homeSetSigned(.F.)}},; && Add by TMV  27-11-2002
				   { "Лимит",    {|| cliLimitEdit() }}} ,;  && Add by TMV  13-11-2002
              {{ "НадУст",   {|| homeSetOn()    }} ,;
               { "ОрдНад",   {|| ordIndiv()     }} ,;
               { "ПодпЗа",   {|| homeSetSigned()}}  ,;
               { "Извещ",    {|| S20SetSigned() }}} ,;
              {{ "Бланки",   {|| printBlanks()  }}  ,;
               { "ПосДок",   {|| printLastDoc() }}  ,;
               { "Забота",   {|| printDocs()    }}} ,;
              {{ "Карты",    {|| viewCards()    }}  ,;
               { "ДопПоиск", {|| newSearch()    }}  ,;
               { "КарДок",   {|| cardDocShow()  }}  ,;
               { "ЕщеПоиск", {|| new2Search()  }}} ,;
              {{ "Сторно",   {|| strnView()     }}  ,;
               { "Дефект",   {|| cliDefect()    }} },;
              {,, { "Колибр",   {|| showColibri()  }}} }


	if isLineNoStrah                                    // Add by TMV  21-04-2003
		fk[4,2] = { "Задолж",    {|| credLineNoStrah() }}   // Add by TMV  21-04-2003
	endif                                                // Add by TMV  21-04-2003

else  //  Данные уже отправлены - исправления не допустимы //  Add by TMV  15-01-2002
	fk:={}
endif
   bUp  := {|| cliUp()    }
   bDn  := {|| cliDown()  }
   bTop := {|| cliTop()   }
   bBtm := {|| cliBottom()}

   aC := {{ "", {|| cliSurName() + "       " + cliName() + "        " + cliSName() }}}

   aF := {{ "Клиент", {|| padR( cliFio(), 45) }} ,;
          { "Индекс",       funBlock( cliIndex  ),,{|| .F.}} ,;
          { "* Гоpод",      funBlock( cliCity   ),,{|| .F.}} ,;
          { "Улица",        funBlock( cliStreet ),,{|| .F.}} ,;
          { "*Дом",         funBlock( cliHouse  ),,{|| .F.}} ,;
          { "*Коpпус",      funBlock( cliBlock  ),,{|| .F.}} ,;
          { "*Кв.",         funBlock( cliFlat   ),,{|| .F.}} ,;
          { "Паспоpт сеp.", funBlock( cliPasSer1),,{||.F. }} ,;
          { "* - ",         funBlock( cliPasSer2),,{|| .F.}} ,;
          { "* N ",         funBlock( cliPasNum ),,{|| .F.}} ,;
          { "Выдан",        funBlock( cliPasGiv ),,{|| .F.}}}


   aStr := { "          ", "<Фамилия> ", "          ", "<Паспорт> "}
//  Данные в филиал не отправлены
	if !chkOdClose()  //  Add by TMV  15-01-2002
   	userDefineKey := {{ K_ENTER, {|| GivePension()}}}
	endif

   CLIENTS ->( dbSetOrder( 2))
   CLIENTS ->( dbGoTop())
	Select CLIENTS
   oB  := browseFtStartUp( 2, 1, aH , aC, bUp, bDn, bTop, bBtm, bH, fk,cStr, aF)
   @ wStWindTop()[ 3] - 2, wStWindTop()[ 2] + 5 SAY "<Ins> - НОВЫЙ КЛИЕНТ"
   CLIENTS ->( browseFiz( 2, 1, oB, aH, aC       ,,;
                          bApp, bDel, fk           ,;
                          userDefineKeys, aStr))

   browseFtExit()

   monClose()

endif

return arr


funcDef new2Search local i
i := dialog( { "Установка порядка просмотра"},;
             { "Поиск по фамилии", "Поиск по номеру паспорта"})
if i > 0
   CLIENTS ->( dbSetOrder( 2 * i))
   getCurOB(): refreshAll(): forceStable()
endif
return NIL

funcDef newSearch local rV, index, recNo, get, buffer, ptr := 1, arr

buffer := Space( 20)
arr    := { "номеру паспорта", "номеру счета", "номеру доверенности"}
rV     := .F.
if getUp( 5, 5,;
   {{ "Дополнительный поиск"          },;
    { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
    { "Поиск по :", block( ptr), arr  },;
    { "*" + chr( 25)                  },;
    { "Искать   :", block( buffer)    }})
   do case
      case ptr == 1
        buffer := padR( allTrim( buffer), 6)
        messageStartUp( "Поиск паспорта N" + buffer + ". Подождите !!!")
        index := CLIENTS ->( indexOrd())
        recNo := CLIENTS ->( recNo())
        CLIENTS ->( dbSetOrder( 4))
        rV := CLIENTS ->( dbSeek( buffer))
        messageExit( 0)
        CLIENTS ->( dbSetOrder( index))
        if rV
           getCurOB(): refreshAll(): forceStable()
        else
           CLIENTS ->( dbGoTo( recNo))
           messageB( "Клиент с паспортом " + buffer + " не найден !!!")
        endif
      case ptr == 2
         buffer := allTrim( buffer)
         do case
            case len( buffer) = 13
                 buffer := schet20( buffer)
            case len( buffer) = 20
            otherwise
                 errLog( { "Вы можете ввести 20- или 13-значный счет,",;
                          " а Вы ввели " + buffer})
                 return .F.
         endcase
         messageStartUp( "Поиск счета N " + buffer)
         ACC ->( baseSavePos())
         ACC ->( dbSetOrder( 1))
         ACC ->( dbSeek( buffer, .T.))
         if ( rV := buffer == left( ACC ->schet, len( buffer)))
            index := CLIENTS ->( indexOrd())
            CLIENTS ->( dbSetOrder( 1))
            CLIENTS ->( dbSeek( ACC ->code))
            CLIENTS ->( dbSetOrder( index))
            getCurOB(): refreshAll(): forceStable()
         else
           messageB( "Клиент со счетом " + buffer + " не найден !!!")
         endif
         ACC     ->( baseRestPos())
         messageExit( 0)
      case ptr == 3
           buffer := allTrim( buffer)
           if len( buffer) <> 8
              messageB( "Номер доверенности состоит из 8 символов !!!")
              return NIL
           endif
           TRUST ->( baseSavePos())
           TRUST ->( dbSetOrder( 3))
           index := CLIENTS ->( indexOrd())
           recNo := CLIENTS ->( recNo())
           CLIENTS ->( dbSetOrder( 1))
           if TRUST   ->( dbSeek( right( buffer, 2) + left( buffer, 5))) .AND. ;
              CLIENTS ->( dbSeek( TRUST ->code)) .AND. TRUST ->number == buffer
              CLIENTS ->( dbSetOrder( index))
              getCurOB(): refreshAll(): forceStable()
           else
              CLIENTS ->( dbSetOrder( index))
              CLIENTS ->( dbGoTo( recNo))
              messageB( "Клиент с доверенностью N " + buffer + " не найден !!!")
           endif

   endcase
endif
return NIL

funcDef viewCards local arr
   crView()
return NIL

funcDef printBlanks local arr, i

arr := {{ "Бланк расходного ордера", .T., {|| blnkDebet()  }} ,;
        { "Бланк приходного ордера", .T., {|| blnkCredit() }} ,;
        { "Бланк поручения на перечисление в другой Банк", .T.            ,;
                   {|| printOpen( "mem36", 1) }},;
        { "Титульный лист на кас.документы"  , !confUseAgent(), {|| blnkTitle()   }} ,;
        { "Лист-разделитель на кас.документы", !confUseAgent(), {|| blnkListRazd() }} ,;
        { "Бланк на перечисление единого социального налога (ЕСН)",.T.,{|| printOpen( "esn", 1)}} }

popUp( 5, 5, arr)
return NIL

funcDef printLastDoc local arr
if yesNo( "Вы действитедьно хотите повторить печать ?")
   lastDebOrder()
endif
return NIL

funcDef printDocs local arr
   arr := {{ "Магазины Заботы+", .T., {|| zab_TovAssort()/*, prnReset()*/}} }
   popUp( 5, 5, arr)
return NIL

funcDef lastDebOrder local ;
        fileName := newFPath( userName() + ".ord", confPathUser()), aOrder

if file( fileName)
   aOrder := { memoRead( fileName)}
   printAOrder( aOrder)
else
   errLog( "Нет информации о последнем расходном ордере !")
endif

return NIL

//
// печать нескольких копий
//
funcDef printCntOrders with aOrder:A local i := 1, ptr

if getUp( 5, 5, {{ "Введите кол-во копий", block( i) }})
   for ptr := 1 to i do printAOrder( aOrder)
endif
return NIL

funcDef blnkDebet local aOrder, oldOrder := isBlankOrder()

isBlankOrder( .T.)

aOrder := post_order( 0, .F., CtoD( ""), "", "", Space(12),;
                      0, "", "", "", .F.,"", "", "")
printCntOrders( aOrder)
isBlankOrder( oldOrder)
return NIL

funcDef blnkCredit local aOrder, oldOrder := isBlankOrder()

isBlankOrder( .T.)

aOrder := vznos_nal( 0, CtoD( ""), "", "", 0, Space( len( confOtdName())),;
                     "", confKassa(), "", .F.)
printCntOrders( aOrder)
isBlankOrder( oldOrder)
return NIL

funcDef isPens with priz:C
return left( priz, 9) == "15 1  1  "

funcDef isCardPens with priz:C
return left( priz, 9) == "15 4  1  "

funcDef pensMakeArrs with arr:AU, arr2:AU, pensOnly:L, useRest:LU local rest
   DEFAULT useRest TO .F.
   ACC ->( netSeek( CLIENTS ->code))
   while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())
      if if( pensOnly, isPens( ACC ->priz), left( ACC ->priz, 6) == "15 1  ")
         rest := sumRest( ACC ->schet, setCurDate())
         if if( useRest, rest > 0, .T.)
            aAdd( arr, { "Счет " + splitSSchet( ACC ->schet) + ;
                         " Остаток " + mStr( rest)})
            aAdd( arr2,ACC ->( recNo()) )
         endif
      endif
      skip 1 alias ACC
   end

return len( arr) > 0


funcDef clientIsPens local rV := .F.
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 2))
   ACC ->( netSeek( CLIENTS ->code))
   while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())
     if isPens( ACC ->priz) .AND. ACC ->typeOf == "00"
       rV := .T.
       exit
     endif
     skip 1 alias ACC
   end
   ACC ->( baseRestPos())
return rV

funcDef paspCheck local rV
  if left( CLIENTS ->buffer, 2) == "01"
     rV := !empty( CLIENTS ->pasSer1) .AND. !empty( CLIENTS ->pasSer2) .AND. ;
           !empty( CLIENTS ->pasNum)  .AND. !empty( CLIENTS ->pasGiv)
  else
     rV := !empty( CLIENTS ->pasGiv)
  endif
return rV

//
// переходник для вызова выплаты пенсии
//
funcDef GivePension local i, arr := { }, arr2 := { }, rest

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

if !pensMakeArrs( @arr, @arr2, .T.)
    pensMakeArrs( @arr, @arr2, .F.)
endif

do case
   case len( arr) = 0
        errLog( "У клиента нет пенсионных счетов с остатками")
   case len( arr) = 1
        ACC ->( dbGoTo( arr2[ 1]))
        if cliRLock()
           if if( paspCheck(), .T., cliEdit( .F.))
              pensGive()
           endif
           CLIENTS ->( netUnLock())
        endif
   otherwise
        if ( i := popUp( 5, 5, arr)) >0
           ACC ->( dbGoTo( arr2[ i]))
           if cliRLock()
              if if( paspCheck(), .T., cliEdit( .F.))
                 pensGive()
              endif
              CLIENTS ->( netUnLock())
           endif
        endif
endcase

ACC ->( baseRestPos())
return NIL

//
// переходник для вызова выплаты пенсии
//
funcDef ChangePcts local i, arr := { }, arr2 := { }, rest

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

ACC ->( netSeek( CLIENTS ->code))
while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())
      if isPens( ACC ->PRIZ) .OR. isCardPens( ACC ->PRIZ)
         rest := sumRest( ACC ->schet, setCurDate())
         aAdd( arr, { "Счет " + splitSSchet( ACC ->schet) + ;
                      " Остаток " + mStr( rest)})
         aAdd( arr2,ACC ->( recNo()) )
      endif
      skip 1 alias ACC
end

do case
   case len( arr) = 0
        errLog( "У клиента нет текущих счетов !")
   case len( arr) = 1
        ACC ->( dbGoTo( arr2[ 1]))
        if cliRLock()
           chngPcts()
           CLIENTS ->( netUnLock())
        endif
   otherwise
        if ( i := popUp( 5, 5, arr)) >0
           ACC ->( dbGoTo( arr2[ i]))
           if cliRLock()
              chngPcts()
              CLIENTS ->( netUnLock())
           endif
        endif
endcase

ACC ->( baseRestPos())
return NIL


static funcDef chngPcts local arr, Schet, fio, theDate, theType, ch := "Y",;
	sPasp1:='', sPasp2:=''    // Add by TMV  13-11-2001

theType := if( SubStr( ACC ->buffer, 6, 1) == "Y", "ЕЖЕМЕСЯЧНО", "ЕЖЕГОДНО")
if yesNo( { "Изменение вида начисления процентов по счету " + ACC ->schet,;
            ""                                                           ,;
            "Вы действительно хотите производить начисление процентов "  ,;
            theType + " ?"})
   theDate := addMonth( firstDay( setCurDate()), 1)
	cliDocum2(@sPasp1, @sPasp2)  // Add by TMV  13-11-2001
   arr := aChangePcts( cliFullName(), ACC ->schet, theDate, theType,; // Edit by TMV  13-11-2001
		sPasp1, sPasp2, cliFullAddress(), cliPhone()	)                  // Edit by TMV  13-11-2001
   printAOrder( arr)
   printAOrder( arr)
   if accRLock()
      if SubStr( ACC ->buffer, 6, 1) == "Y" then ch := " "
      ACC ->buffer := left( ACC ->buffer, 5) + Ch + SubStr( ACC ->buffer, 7)
      accUnLock()
   endif
endif

return NIL

funcDef cliOperations local bH:=""
popUp( 5, 5, {{ "Выдача...",      .T., {{ "Текущий счет( пенсия )", .T., {|| givePension()     }},;
                                     { "Текущие счета(иные)   ", .T., {|| decSchet( INC_TEC)}},;
                                     { "Срочные вклады        ", .T., {|| decSchet( INC_SR )}} ;
                                    }},;
              { "Прием...",       .T., {{ "Срочные вклады ", .T., {|| incSchet( INC_SR )}},;
                                     { "На текущий счет", .T., {|| incSchet( INC_TEC)}} ;
                                    }},;
              {"Поручения..."     ,.T.,{ ;
                                     { "Долгосрочные   ", .T., {|| contBrowse() }},;
                                     { "Разовые...     ", .T., ;
                                       {{ "Безналичные перечисления...",  .T., {|| accTrans()    }},;
                                        { "Заказ карт", .F., {|| wsBrwCard()}}};
                                     };
                                    }},;
              { "Платежи...     ",      .T., ;
                                       {{ "Разовые Наличные",          .T., {|| paymCash( .T.)}},;
                                        { "Разовые Безналичные",       .T., {|| paymCash( .F.)}},;
                                        { "Лимиты и тарифы сервисных карт", .T., {|| cardApp4()}},;
                                        { "Сервисные карты",           .T., {|| cardMakePaym()}},;
                                        { "Наличные платежи от юр.лиц",.T., {|| alo_orgBrw()  }},;
                                        { "Список организаций",.T., {|| _provPrint()  }}};
                                     },;
              {'"Забота+"(продукты)...', .T.,{{ "Прием заказов",   .T., {|| zab_FeedOform() }},;
                                          { "Печать ассортимента", .T., {|| zab_FeedAssort()}},;
                                          { "Просмотр заказов",    .T., {|| zab_FeedBrowse()}},;
                                          { "Сообщение оператору", .T., {|| zab_FeedMes()   }} ;
                                    }},;
              {"Микрокредиты...", confUseMicro(),;
                                    {{ "Выдача  микрокредита", .T., {|| if( chckMicro(), accCredOpen(), .F.)}},;
;// Replaced by TMV 22-05-2003
                                     { "Возврат микрокредита", .T., {|| if( chckMicro(), ;
													if( useLineNoStrah(), crdRetNoStrah(), credRetCred()), .F.)}},;
                                     { "Возврат процентов", if( useLineNoStrah(), .F., .T.), ;
												  {|| if( chckMicro(), credRetPcts(), .F.)}} ;
                                    }} ,;
              { "Доверенность   ", .T., {|| trustView()  }},;
              {"Выписки и справки", .T., {||alo_spr()     }},;
              {"Сообщение по счету", .T.,{|| spr_InfoCWork() }},;
              {"Карточные счета...", .T., {;
                             { "Заказ карт", .T., {|| wsBrwCard()}},;
                             { "Реестр заказанных карт", .T., {|| zkReestr() }},;
                             { "Блокировка/Постановка в стоп-лист карты...",    .T.,;
                                {{ "Блокировка карты",       .T., {|| wsBlockCard()}},;
                                 { "Постановка в стоп-лист", .T., {|| wsStopList() }} },;
                             },;
                             { "Карточные документы", .T., {|| cardDocShow()}},;
                             { "Расчеты по пластиковым картам",        .F., {|| kas1009() }},;
                             { "Прием наличных по пластиковым картам", .F., {|| kas1027() }} ;
                             }},;
              { "Просм.Безналичных", .T., {|| paymView()    }},;
              { "Журнал операций",   .T., {|| downJournal() }},;
              { "Сторно",  .T.,  {|| strnView()     }},;
              { "Облигации ГСЗ...", .T.,;
                           {{ "Покупка клиентом  ГСЗ", .T., {|| gszOper( "BUY")  }},;
                            { "Досрочная продажа ГСЗ", .T., {|| gszOper( "SALE") }} ;
                       }};
,bH })

return NIL

funcDef gszCheckPasp local rV := .T.
if cliRLock()
  while !paspCheck()
    rV := .F.
    if !cliEdit( .F.)
      exit if paspCheck()
      exit if !yesNo( "Еще раз заполнить данные ?")
      continue
    endif
    if !paspCheck() then messageB( "Паспортные данные введены не полностью !")
  enddo
  CLIENTS ->( netUnLock())
  rV := paspCheck()
endif
return rV

funcDef gszOper with Oper:C local rV := NIL
  if gszCheckPasp()
    do case
       case Oper == "BUY"  do rV := gszBuyGSZ()
       case Oper == "SALE" do rV := gszSaleGSZ()
       otherwise           do errOK( "Неверный вызов gszOper !")
    endcase
  else
    messageB( "Паспортные данные клиента не заполнены !")
  endif
return NIL

funcDef chckMicro local rV := .T.

if empty( confPctSchet())
   errLog( "Не введен счет возврата процентов !")
   clear typeAHead
   rV := .F.
else
   if empty( confStrSchet())
      errLog( "Не введен накопительный счет !")
      clear typeAHead
      rV := .F.
   endif
endif

return rV

funcDef cliKassa

popUp( 5, 5, {{ "Приход        ", .T., {|| if( confUseAgent(), kas1027(), kasInc())}},;
              { "Расход        ", .T., {|| if( confUseAgent(), kas1009(), kasDec())}},;
              { "Сверка...     ",  .T., {{ if( !confUseAgent(), "Кассовый журнал", "Реестр операций"), .T., {|| kassJournal()}} ,;
                                         { "Справка об оборотах", !confUseAgent(), {|| kassSvod()   }} ,;
                                         { "Книга регистрации отк-х счетов", .T., {|| bookOpenAccount()}} ,; // Add by TMV 25-09-2001
                                         { "Ведомость надомников", .T., {|| journ904()   }} ,;
                                         { "Договоры по ГСЗ", .T., {|| gszPrint2()  }} ,;
                                         { "Лимиты ГСЗ",      .T., {|| gszPrint3()  }}  ;
                               }},;
              { "Надомники...  ",  .T., ;
                                 {{ "Печать ордеров",  .T., {|| homePrep()      }},;
                                  { "Печать из файла", .T., {|| homeProof( .F.) }},;
                                  { "-"                                          },;
                                  { "Списание       ",  .T., {|| homeProof()     }} ;
                               }},;
             { "-" },;
             { "Информационная справка...", .T.,;
                                {{ "Редактирование справки", .T., {|| infoEdit()}},;
                                 { "Печать справки",         .T., {|| infoPrint()}};
                                 }}})
return NIL

function sayCliFooting( nTop, nLeft)
local par1, par2

@ nTop,    nLeft say "Клиент "; @ row(), col() say;
                                     padR( allTrim( cliSurName()) + " " +;
                                     allTrim( cliName()    ) + " " +;
                                     allTrim( cliSName()   ), 45 )
if !isKodYlExisi   // Add by TMV  21-10-2002
	@ nTop+1, nLeft say "Индекс ";   @ row(), col() say cliIndex()
	@ row(),  col() say " Гоpод ";   @ row(), col() say cliCity()
	@ nTop+2, nLeft say "Улица " ;   @ row(), col() say cliStreet()
	@ row(),  col() say " Дом "  ;   @ row(), col() say cliHouse()
	@ row(),  col() say " Коpпус ";  @ row(), col() say cliBlock()
	@ row(),  col() say " Кв. "  ;   @ row(), col() say cliFlat()
else
	@ nTop+1, nLeft say "Индекс ";   @ row(), col() say cliIndex()
	@ row(),  col() say " Гоpод ";   @ row(), col() say cliCity()
	@ row(),  col() say " Улица " ;   @ row(), col() say cliStreet()
	@ nTop+2, nLeft say "Дом "  ;   @ row(), col() say cliHouse()
	@ row(),  col() say " Коpпус ";  @ row(), col() say cliBlock()
	@ row(),  col() say " Кв. "  ;   @ row(), col() say cliFlat()
	@ row(),  col() say " Код ул. "; @ row(), col() say cliKodYl() // Add by TMV  21-10-2002
endif

cliDocum2( @par1, @par2)
@ nTop+3,  nLeft say padR( par1, 60)
@ nTop+4,  nLeft say padR( par2, 60)

return NIL

funcDef monInit local dbInfo := {}, s := eStr( 0)

basePath( SetUpInfo(), confPathOD())  // Moved from down by TMV  21-10-2002
basePath( infoInfo(), confPathOD())

aAdd( dbInfo, cliInfo())
aAdd( dbInfo, accInfo())

aAdd( dbInfo, sumInfo())
aAdd( dbInfo, sumPInfo())

aAdd( dbInfo, wiscInfo())
aAdd( dbInfo, wiscTypeInfo())

aAdd( dbInfo, operInfo())
aAdd( dbInfo, ordTempInfo())

aAdd( dbInfo, vidInfo())

aAdd( dbInfo, trustInfo())
aAdd( dbInfo, trustDovInfo())

aAdd( dbInfo, distrInfo())
aAdd( dbInfo, kasInfo())
aAdd( dbInfo, agentInfo())

aAdd( dbInfo, modInfo())
aAdd( dbInfo, cashersInfo())
aAdd( dbInfo, admInfo())

aAdd( dbInfo, credInfo())

aAdd( dbInfo, provInfo())
aAdd( dbInfo, prov2Info())
aAdd( dbInfo, contInfo())
aAdd( dbInfo, ticInfo())

aAdd( dbInfo, assInfo())
aAdd( dbInfo, spr_Info())

aAdd( dbInfo, tempInfo())
aAdd( dbInfo, logInfo())
aAdd( dbInfo, errInfo())

basePath( nodesInfo(), confPathOD())
if file( nodesInfo()[ 1]) then aAdd( dbInfo, nodesInfo())

basesPath( confPathOD(), dbInfo)

aAdd( dbInfo, confInfo())
if !file( logInfo()[ 1]) then baseCreate( logInfo())
if !file( errInfo()[ 1]) then baseCreate( errInfo())
basesInit( dbInfo)
if !file( agentInfo()[ 1]) then baseCreate( agentInfo())

if !file( admInfo()[ 1]) then baseCreate( admInfo())

if !file( ticInfo()[ 1]) then baseCreate( ticInfo())

return NIL

funcDef monInfo
return { cliInfo(),  accInfo(),      sumInfo()  ,;
         wiscInfo(), wiscTypeInfo(), operInfo() ,;
         trustInfo(), trustDovInfo(), sumPInfo(),;
         contInfo(), ticInfo() }

funcDef monOpen
return basesMOpen( monInfo()) .AND. tempOpen()

funcDef monXOpen
return basesXOpen( monInfo()) .AND. tempXOpen()

funcDef monClose
return basesMClose( monInfo()) .AND. tempClose()

funcDef tempXOpen
return baseMXOpen( tempInfo())


autofunction setValue isDebug init .F.

funcDef cardDocShow local arr1, arr2, ptr, cnt := 1, arr3, i,;
	pasGiv := razbivka( cliPasGiv(), {{ 35, "L"}, { 35, "L"}}),;    // Add by TMV 24-03-2004
	adr := razbivka( cliFullAddress(), {{ 25, "L"}, { 35, "L"}}),; // Add by TMV 24-03-2004
	arrCards := {'____________________',;
					 '__________________________________________________'} // Add by TMV 24-03-2004

arr1 := { ;
"Распоряжение на блокирование карты"                      ,;
"Заявление о спорной операции"                            ,;
"Заявление о возврате карты из банкомата"                 ,;
"Заявление о неполучении наличных денег"                  ,;
"Расписка в получении карты"                              ,;
"Расписка об изъятии карты"                               ,;
"Расписка в получении ПИН-конверта"                       ,;
"Заявление о потерe карты(ПИН)"                           ,;
"Реестр ПИН-конвертов,подл.уничтожению"                   ,;
"Реестр изъятых карт"                                     ,;
"Заявление на прекр.действия карты"                       ,;
"Заявление на выпуск нового ПИН"                          ,; // Add by TMV 26-03-2004
"Заявление на разблокирование карты"                      ,; // Add by TMV 26-03-2004
"Отчет об изъятии карты"                                  }

arr2 := { ;
"CRD10",;
"CRD11",;
"CRD13",;
"CRD14",;
"CRD2" ,;
"CRD4" ,;
"CRD5" ,;
"CRD6" ,;
"CRD16",;
"CRD17",;
"CRD21",;
"CRD25",;     // Add by TMV 26-03-2004
"CRD26",;     // Add by TMV 26-03-2004
"CRD23"}

ptr := popUp( 5, 5, arr1)
if ptr > 0
// Add by TMV 24-03-2004
	if at( Str( ptr, 1), '1356') > 0 .Or. ptr == 12 .Or. ptr == 13
		if Len(arrCards := cliCardList()) == 0
			arrCards := {'____________________',;
							 '__________________________________________________'}
		endif
	endif
// Add by TMV 24-03-2004 -- End
   servDummy( {{ "@rdate",     rDateFull( setCurDate()) },;
               { "@filial",    confFilial() },;
               { "@otd",       confOtdNumb()},;
               { "@filialotd", rTrim( confFilial()) + " " + confOtdNumb()},;
               { "@date",      setCurDate() },;
               { "@time",      time()       },;
               { "@crdNumb",   arrCards[1] },;
               { "@d1",        setCurDate() + 14},;
               { "@d2",        setCurDate() + 15},;
					{ "@type",      arrCards[2] },;                     // Add by TMV 24-03-2004
					{ "@fio",       cliFullName()},;                    // Add by TMV 24-03-2004
					{ "@pasnum",    cliPasSer() + ' ' + cliPasNum() },; // Add by TMV 24-03-2004
					{ "@pasgiv1",   pasGiv[1]},;                        // Add by TMV 24-03-2004
					{ "@pasgiv2",   pasGiv[2]},;                        // Add by TMV 24-03-2004
					{ "@adr1",      adr[1]},;                           // Add by TMV 24-03-2004
					{ "@adr2",      adr[2]},;                           // Add by TMV 24-03-2004
					{ "@tel",       cliPhone()},;                       // Add by TMV 24-03-2004
				   { "@cliFIO" ,   allTrim(cliSurname()) + ' ' +;
					 Left( cliName(), 1) + '.' +;
					 Left( cliSName(), 1) + '.' };             // Add by TMV 24-03-2004
					})
   getUp( 5, 5,;
    {{ "Введите количество копий ", block( cnt), "99",,{|g| g:varGet >0}}})
   printOpen( arr2[ ptr], cnt, .T.)
   servDummy( {})
endif
return NIL

funcDef cliDefect ;
  local fullName, fullAddr, rV := .F., nD := 0, newND := 0, ar1, ar2, curSchet

accPrizList( "15 1  1  1  ", @ar1, @ar2)
if len( ar1) < 1
   messageB( "У клиента нет действующих пенсионных счетов !")
   return .F.
endif
curSchet := ar2[ 1][ 2]
fullName := Space( 60)
fullAddr := Space( 60)
cliGetHome( @nD); newND := 0
if getUp( 5, 5,;
    {{ "Внесите данные для дефектной ведомости  "},;
     { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
     { "Заполнять только строки, нуждающиеся в изменении"},;
     { ""                                        },;
     { "Данные в базе "                          },;
     { "Фамилия " + cliFullName()                },;
     { "Адрес   " + cliFullAddress()             },;
     { "День выплаты " + nStr( nD)               },;
     { "----------------------------------------"},;
     { "Фамилия ", block( fullName)              },;
     { "Адрес   ", block( fullAddr)              },;
     { "День выплаты", block( newND), "99"       }})
   if !empty( fullName) .AND. cliFullName()    <> allTrim( fullName)
      if WISC ->( rV  := mAppend())
         WISC ->type  := "905 "
         WISC ->schet := curSchet
         WISC ->buffer:= cliCode() + ";" + "1" + ";" + cliFullName() + ";" + ;
                         fullName
         WISC ->( netUnLock())
      endif
   endif
   if !empty( fullAddr) .AND. cliFullAddress() <> allTrim( fullAddr)
      if WISC ->( rV  := mAppend())
         WISC ->type  := "905 "
         WISC ->schet := curSchet
         WISC ->buffer:= cliCode() + ";" + "2" + ";" + cliFullAddress() + ;
                         ";" + fullAddr
         WISC ->( netUnLock())
      endif
   endif
   if !empty( newND) .AND. nD <> newND
      if WISC ->( rV  := mAppend())
         WISC ->type  := "905 "
         WISC ->schet := curSchet
         WISC ->buffer:= cliCode() + ";" + "3" + ";" + nStr( nD) + ";" + nStr( newND)
         WISC ->( netUnLock())
      endif
   endif
   if rV then messageB( "Данные для дефектной ведомости сформированы !!!")
endif
return NIL

funcDef showColibri ;
  local cardNumb, cardDate, rV := .F., askBefore := .F., prevNumb, prevDate
  if !clientIsPens()
    messageB( "Эта услуга доступна только для пенсионеров !")
    return rV
  endif
  prevNumb  := cardNumb  := left( CLIENTS ->colibri, 7)
  cardDate  := subStr( CLIENTS ->colibri, 8, 3)
  prevDate  := cardDate  := decodeKrupDate( cardDate)
  askBefore := !empty( cardNumb) .OR. !empty( cardDate)
  cardDate  := setCurDate()
  if getUp( 5, 5, {{ "Оформление карты колибри"},;
                   { "Предыдущий номер карты " + prevNumb},;
                   { "Предыдущая дата выдачи " + DtoC( prevDate)},;
                   { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
                   { "Номер карты", block( cardNumb), "XXXXXXX",,{|g| chkColib( g)}},;
                   { "Дата выдачи", block( cardDate)}})
     if askBefore 
       if !yesNo( "Клиент уже получал карту. Выдавать повторно ?") then return .F.
     endif
     if CLIENTS ->( rV := mRLock())
        cardDate := EncodeKrupDate( cardDate)
        CLIENTS ->colibri := padR( cardNumb, 7) + cardDate
        CLIENTS ->lEdited := .T.
        CLIENTS ->( netUnLock())
     endif
     if rV
       servDummy( {{ "@colibri", cardNumb}})
       printOpenCol( "COLIB", 1)
       servDummy( {})
     endif
  endif
return NIL

static funcDef chkColib with g:O local rV := .T.
  if len( allTrim( g: varGet)) < 7
    rV := .F.
    if !yesNo( "Номер карты введен не полностью, продолжить ввод ?")
      keyBoard chr( K_ESC)
    endif
  endif
return rV


// Add by TMV  13-11-2002
funcDef cliLimitEdit local rV:=.T., gets, nSummaLimit,;
	arr:={}, arr2:={}, sSchet:='', s, sDoc:='', sDocNew:=''

if setUpValue('PMKL') != '1'
	return rV
endif

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

if !pensMakeArrs( arr, arr2, .T.)
	errLog( "У клиента нет пенсионных счетов")
	ACC ->( baseRestPos())
   return rV
endif

ACC->(dbgoto(arr2[1]))
sSchet = ACC->SCHET
ACC ->( baseRestPos())
nSummaLimit:=CLIENTS->CredSumma/100

gets := {{ upper( "Введите новую сумму лимита") } ,;
        { "----------------------------------------------------------------" } ,;
        { "Клиент : " + cliFio()} ,;
        { "Пенсионный счет : " + sSchet} ,;
        { "Текущая сумма лимита : " + Str( CLIENTS->CredSumma/100, 15, 2) } ,;
        { "----------------------------------------------------------------" } ,;
        { "" } ,;
        { "Новая сумма лимита   : ", block( nSummaLimit), "9999999.99",,;
				{|g| chkLimitSum(g)} }}

if ( rV := getUp( 6, 3, gets )) .And.;
		yesNo( { "Изменение лимита кредитования",;
				  " ",;
            	"Вы действительно хотите изменить лимит для",;
					cliFio() + " ?"})

	s = select()
	sDocNew := tmpGet( "NEWLIMITDOC")
	sDoc    := "949" + confRegCode() + confOtdCode() + padL( sDocNew, 6, "0")
	sDocNew := nStr( val( sDocNew ) + 1)
	tmpSet( "NEWLIMITDOC", sDocNew)
	tmpUnLock()
	select ( s)
	if cliRLock() 
// Remov by TMV  20-11-2002
		cliLimit(nSummaLimit * 100)
	endif
	cliUnLock()
   messageB( "Лимит изменен .")

endif

return rV


// Add by TMV  13-11-2002
funcDef cliLimit with newValue:NU local oldValue
oldValue := CLIENTS ->CredSumma
if !isNIL( newValue) then CLIENTS ->CredSumma := newValue
return oldValue

// Add by TMV  13-11-2002
funcDef chkLimitSum with g:O
if g:varGet < 0
	messageB( "Сумма не может быть отрицательной !!!")
	return .F.
endif
return .T.


// Add by TMV  13-11-2002
funcDef getWiscLimit With dDate :D, sCode :C local rV:=.F., s:=select()

Select WISC
Locate For WISC->TYPE == '949 ' .And.;
	Right(allTrim(WISC->BUFFER), 10) == DtoC(dDate) .And.;
	Left(WISC->BUFFER, 12) == sCode
rV = Found()
Select (s)

return rV


// Add by TMV  13-11-2002
// Лимит задолжености по кредиту
funcDef cliCredLimit with newValue:NU local oldValue
oldValue := CLIENTS ->CredLimit
if !isNIL( newValue) then CLIENTS ->CredLimit := newValue
return oldValue

// Add by TMV  21-04-2003
// Лимит задолжености по кредиту по нестраховой схеме
funcDef cliCredLine with newValue:NU local oldValue
oldValue := CLIENTS ->CredLine
if !isNIL( newValue) then CLIENTS ->CredLine := newValue
return oldValue

// Add by TMV  21-04-2003
// Дата окончания действия лимита задолжености по кредиту по нестраховой схеме
// Rename from <cliCredDateNoStrah> by TMV  04-06-2003
//
funcDef cliCrdDClosNoStrah with newValue:DU local oldValue
oldValue := CLIENTS ->CredDate
if !isNIL( newValue) then CLIENTS ->CredDate := newValue
return oldValue

// Add by TMV  04-06-2003
// Дата установки лимита задолжености по кредиту по нестраховой схеме
//
funcDef cliCrdDOpenNoStrah with newValue:DU local oldValue
oldValue := CLIENTS ->CredOpen
if !isNIL( newValue) then CLIENTS ->CredOpen := newValue
return oldValue

// Add by TMV  21-04-2003
// Свободный номер транша по кредиту по нестраховой схеме
funcDef cliCredTranshNoStrah with newValue:NU local oldValue
oldValue := CLIENTS ->Crd_Num
if empty(oldValue) then oldValue = 1
if !isNIL( newValue) then CLIENTS ->Crd_Num := newValue
return oldValue

// Add by TMV 24-03-2004
***************************************
** Список пластиковых карт по клиенту
***************************************
funcDef cliCardList local rV := {},;
	j, i := 0, arrCards := wsCardList( , 'A,D'), arr := {}, arrCode, arrName,;
	sName := ''   // Add by TMV 15-04-2004
// Add to <wsCardList> new param <'A,D'> by TMV 03-04-2004
if Len(arrCards) == 0 then return rV
	
aEval( arrCards, {|x| aAdd( arr, x[1])} )
if Len(arr) > 0
	i = popUp(5, 15, arr)
endif
if i > 0
   assGetInfo( "10", @arrCode, @arrName)
	if Len(arrCode) > 0  .And. ;                     // Add by TMV 15-04-2004
			(j := aScan(arrCode, arrCards[i, 3])) > 0
		sName = arrName[j]                           // Add by TMV 15-04-2004
	endif                                           // Add by TMV 15-04-2004
	if empty(sName)
		MessageB('Данный тип карты не разрешено использовать!')  // Add by TMV 15-04-2004
	else
		rV := { arrCards[i, 1], sName } // Replace <arrName> to <sName> by TMV 14-04-2004
	endif
endif

return rV

// Add by TMV 29-06-2004
funcDef getDocumName local rV :='', docType, docPtr
   docType = left( CLIENTS ->buffer, 2)
   docPtr  = aTrimmedScan( aDocCode(), if( empty( docType), "01",docType))
   rV = allTrim( aDocName()[ docPtr])
return rV

funcDef popUp with y1:NUA, x1:NU, aTitles:ACBU, selected:NU, bH:ACBU ;
   local height, width, rV
   set key K_F1 to cHelp()
   if isArray( y1)
      //
      // y1, x1 опущены
      //
      rV := popUp( row()+1, col()-2, y1, x1, aTitles)
   else
      //
      // y1, x1 указаны ( может быть как NIL или ,, )
      //
      DEFAULT y1 TO row()+1
      DEFAULT x1 TO col()-2
      popPrep( aTitles)
      height := popHeight( aTitles)
      width  := popWidth(  aTitles)
      popStartUp(  @y1, @x1, aTitles, bH,       height, width)
      rV := popRead( y1, x1, aTitles, selected, height, width)
      popExit()
   endIf
return rV
