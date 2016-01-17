*============================================================================
*
*                  Программа : MON.EXE
*                  Файл      : basewisc.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: Описание файла желаний и районов доставки
*
*----------------------------------------------------------------------------
*
* 07-08-2003  TMV
* При формировании 855 пожелания сделано формирование операции 8700
* и печать док-ов по переводу.
* Ф-ия wiscTran42309().
*
* 25-03-2004 TMV
* Новый порядок работы с картами через OpenWay
* Ф-ии wiscBrwCard(), zkLine(), wsCrAppend(), wsCrDecode(), wsBlockCard()
* Новая ф-ия wsCardList()
*
* 31-03-2004 TMV
* Новый порядок работы с картами через OpenWay - доработка.
* Поправлен механизм отбора контрактов по пласт. картам. Теперь просматриваются
* все записи в CONTRACT.dbf по данному счету.
* Ф-ии wiscBrwCard(), wsCrCount().
* 
* 01-04-2004 TMV
* Исправлена запись информации в wisc по выпускаемым картам. Тип карты 
* (осн-доп) пишется в 6-ю позицию, а тип операции (ПИН, дубликат...) - в 8-ю.
* Номер карты (при необходимости) пишется в последнюю (9-ю) позицию Wisc,
* а тип карты по прежнему пишется в первую позицию.
* Ф-ия wsCrAppend(), wsCrDecode()
* 
* 02-04-2004 TMV
* Исправлено заполнение типа карты в WISC при перевыпуске.
* Ф-ия wsCrAppend()
* Убрано поле "Дата начала блокировки".
* Ф-ия wsBlockCard()
*
* 03-04-2004 TMV
* В список карт для перевыпуска включаются только карты со статусом A и D.
* Список выводится на экран даже для одной карты.
* Ф-ия wsCardList(), wsCrAppend().
*
* 09-04-2004 TMV
* Исключена возможность заказа второй карты, если еще нет основной.
* После номера карты в Wisc ставится <;>
* Ф-ия wsCrAppend(), wsCrCount().
* При подсчете кол-ва карт теперь учитываются карты со статусом D.
* Ф-ия wsCrCount()
*
* 15-04-2004 TMV
* При перевыпуске доп. карты убран вопрос о владельце этой карты.
* Из формы заказа убрано поле "За плату/бесплатно".
* Запрещена выдача дубликата/перевыпуск типов карт, которые не разрешено
*  использовать.
* Ф-ия wscrAppend()
*
* 19-04-2004 TMV
* Изменен алгоритм просмотра списка карт. Сначала проверяется наличие любых
*  карт, а затем при перевыпуске учитываются только карты со статусом D и A
* Ф-ии wsCrAppend(), wsCrCount() и wsCardList().
*
* 19-05-2004 TMV
* Исключена возможность блокировки и постановки в стоп-лист карт с
* OWS_Priz = 0.
* Ф-ии wsBlockCard() и wsStopList().
*
* 28-05-2004 TMV
* При блокировке и постановке в стоп-лист с номером карты выводится ее статус.
* Ф-ии wsBlockCard(), wsStopList()
*
* 23-08-2004 TAT
* процедура контроля возможности заказа новой карты
* wsOrderCnt
*
* 23-12-2004
* wsOrderCnt дублировано в процедуре wsCr2Append



#include "common.ch"
#include "inkey.ch"

#include "clippext.ch"
#include "mon.ch"

#define CR_VISA_ELECTRON "40004 "
 
func wiscInfo
static dbInfo := { ".\WISC.DBF", "WISC",;
                  {{ "schet",    "C", 20, 0} ,;
                   { "type",     "C",  4, 0} ,;
                   { "ID",       "C",  6, 0} ,;
                   { "buffer",   "M",512, 0}},;
                  {{, "schet+type"}, { ,"type" }} ,;
                  0}
return dbInfo

funcDef wiscPath with newPath:CU
return basePath( wiscInfo(), newPath)

funcDef wiscOpen
return baseMOpen( wiscInfo())

funcDef wiscXOpen
return baseMXOpen( wiscInfo())

funcDef wiscClose
return baseMClose( wiscInfo())

funcDef wiscCreate
return baseCreate( wiscInfo())

autofunction setValue WiscUseAppend init .F.

funcDef wiscAppend local rV := .F.
if wiscUseAppend()
   rV := wQuickApp()
else 
   rV := wisc ->( mAppend())
endif
return rV

funcDef wQuickApp local rV := .F.
  WISC ->( dbAppend())
  if WISC ->( !netErr())
     rV := .T.
  else
     rV := WISC ->( mAppend())
  endif
return rV

funcDef wiscRLock
return wisc ->( mRLock())

funcDef wiscUnLock
return wisc ->( netUnLock())

funcDef wiscSeek with schet:C, type:C
return WISC ->( netSeek( schet + type))


* NEW OBJECT FILE

func wiscTypeInfo
static dbInfo := { ".\WISCType.DBF", "WISCTYPE",;
                  {{ "type",     "C",  4, 0} ,;
                   { "name",     "C", 60, 0}},;
                  {{, "type"}, { , "name"}} ,;
                  0}
return dbInfo

funcDef wiscTypePath with newPath:CU
return basePath( wiscTypeInfo(), newPath)

funcDef wiscTypeOpen
return baseMOpen( wiscTypeInfo())

funcDef wiscTypeXOpen
return baseMXOpen( wiscTypeInfo())

funcDef wiscTypeClose
return baseMClose( wiscTypeInfo())

funcDef wiscTypeCreate
return baseCreate( wiscTypeInfo())

funcDef wiscTypeAppend
return wiscType ->( mAppend())

funcDef wiscTypeRLock
return wiscType ->( mRLock())

funcDef wiscTypeUnLock
return wiscType ->( netUnLock())


funcDef wiscHistory with schet:C local s, cHistory, rV, opened

s        := select()
cHistory := ""
rV       := .T.
opened   := Select( "WISC") >0

if !opened then rV := wiscOpen()
if opened .OR. rV
   if wiscSeek( schet, WS_HISTORY) then cHistory := WISC ->buffer
   if !opened then wiscClose()
endif

select ( s)
return cHistory

funcDef wiscDov with summa:N local rV := .T.
if allowTrust()
   if wiscAppend()
      WISC ->schet := ACC ->schet
      WISC ->type  := "DOV"
      WISC ->buffer:= nStr( summa) + ";" + trustGetNumber()
   endif
endif
return rV

funcDef wiscDovSeek with schet:C, trNumber:C, summa:N local rV := .F., arr
if WISC ->( dbSeek( schet + "DOV"))
   trNumber := "DOV:" + trNumber
   while WISC ->schet == schet .AND. WISC ->type == "DOV"
      arr := AtoC( WISC ->buffer, ";")
      if val( arr[ 1]) == summa .AND. trNumber == arr[ 2]
         rV := .T.
         exit
      endif
      skip 1 alias WISC
   enddo
endif
return rV


* NEW OBJECT FILE

func operInfo
static dbInfo := { ".\OPER.DBF", "OPER",;
                  {{ "type",     "C",  4, 0} ,;
                   { "name",     "C", 60, 0} ,;
                   { "ro",       "C",  2, 0} ,;
                   { "sim",      "C",  2, 0} ,;
                   { "agent_oper", "C",4, 0}},;
                  {{, "type"}, { , "name"}} ,;
                  0}
return dbInfo

funcDef operPath with newPath:CU
return basePath( operInfo(), newPath)

funcDef operOpen
return baseMOpen( operInfo())

funcDef operXOpen
return baseMXOpen( operInfo())

funcDef operClose
return baseMClose( operInfo())

funcDef operCreate
return baseCreate( operInfo())

funcDef operAppend
return oper ->( mAppend())

funcDef operRLock
return oper ->( mRLock())

funcDef operUnLock
return oper ->( netUnLock())

funcDef operGetInfo with typeOf:C, ro:CU, sim:CU, name:CU local rV, s, opened, found

ro := sim := name := ""
s  := select()
rV := .T.

opened := select( "OPER") >0
if !opened then rV := baseNSOpen( operInfo())

if opened .OR. rV

   OPER ->( baseSavePos())
   OPER ->( dbSetOrder( 1))

   if ( found := OPER ->( dbSeek( typeOf)) )
      ro   := OPER ->ro
      sim  := OPER ->sim
      name := OPER ->name
   endif

   OPER ->( baseRestPos())
   if !opened then baseMClose( operInfo())
endif
select ( s)

return found



* NEW OBJECT FILE

func distrInfo
static dbInfo := { ".\DISTRICT.DBF", "DISTR",;
                  {{ "station",  "C",  2, 0} ,; // Mike !!!!!!!!!!!!! Добавил
                   { "type",     "N",  2, 0} ,;
                   { "name",     "C", 60, 0}},;
                  {{, "type"}, { , "name"}} , 0}
return dbInfo

funcDef distrPath with newPath:CU
return basePath( distrInfo(), newPath)

funcDef distrOpen
return baseMOpen( distrInfo())

funcDef distrXOpen
return baseMXOpen( distrInfo())

funcDef distrClose
return baseMClose( distrInfo())

funcDef distrCreate
return baseCreate( distrInfo())

funcDef distrAppend
return distr ->( mAppend())

funcDef distrRLock
return distr ->( mRLock())

funcDef distrUnLock
return distr ->( netUnLock())

funcDef distrArray with arr1:AU, arr2:AU, b:BU ;
        local arr := { }, opened, rV := .F., s := select()

DEFAULT b TO {|| .T.}

arr1 := { }
arr2 := { }

opened := select( "DISTR") >0
if !opened then rV := baseNSOpen( distrInfo())

if opened .OR. rV
   DISTR ->( netGoTop())
   while DISTR ->( !eof())
     if Eval( b)
        aAdd( arr1, DISTR ->name)
        aAdd( arr2, DISTR ->type)
     endif
     skip 1 alias DISTR
   enddo
   if !opened then baseMClose( distrInfo())
endif
if aScan( arr2, 0) == 0
   aAdd( arr1, NIL); aAdd( arr2, NIL)
   aIns( arr1, 1);   aIns( arr2, 1)
   arr1[ 1] := "Нет информации"
   arr2[ 1] := 0
endif
select ( s)
return NIL


funcDef wishReport local d1, d2, s

d1 := setCurDate()
d2 := setCurDate()
s  := select()

if wiscOpen()

   if getUp( 5, 5, {{ "Счет " + splitSSchet( ACC ->schet)} ,;
                    { ""                   } ,;
                    { "Запрос на получение выписки за период"},;
                    { "с  ", block( d1)     } ,;
                    { "по ", block( d2)     }})
      if WISC ->( mAppend())
         WISC ->schet  := ACC ->schet
         WISC ->type   := "1"
         WISC ->buffer := DtoC( d1) + ";" + DtoC( d2)
         WISC ->( netUnLock())
      endif
   endif
   wiscClose()
endif

return NIL

funcDef wiscTop with schet:C, type:C local rV := .T.
   WISC ->( dbSeek( schet + type))
   if !( rV := wiscTrue( schet, type))
      WISC ->( mAppend())
   endif
return rV

funcDef wiscTrue with schet:C, type:C local rV
   rV := WISC ->schet == schet .AND. WISC ->type == type
return rV

funcDef wiscBottom with schet:C, type:C local rV := .T., nextType
   nextType := padR( nStr( val( type) + 1), 4)
   WISC ->( dbSeek( schet + nextType, .T.))
   if wiscTrue( schet, type)
      WISC ->( dbSkip( -1))
   endif
   rV := wiscTrue( schet, type)
return rV

funcDef wiscDown with schet:C, type:C local rV
   if rV := WISC ->( netSkipDown())
      if !( rV := wiscTrue( schet, type))
         WISC ->( netSkipUp())
         if !wiscTrue( schet, type)
            WISC ->( mAppend())
            WISC ->( netUnLock())
         endif
      endif
   endif
return rV

funcDef wiscUp with schet:C, type:C local rV
   if rV := WISC ->( netSkipUp())
      if !( rV := wiscTrue( schet, type))
         WISC ->( netSkipDown())
         if !wiscTrue( schet, type)
            WISC ->( mAppend())
            WISC ->( netUnLock())
         endif
      endif
   endif
return rV


funcDef wsBrwCard local arr1, arr2, ptr, rV
   accPrizList( "15 4", @arr1, @arr2)
   do case
      case len( arr1) <  1
           messageB( "У клиента нет карточных счетов !!!")
           return .F.
      case len( arr1) == 1
           ptr := 1
      case len( arr1) >  1
           ptr := popUp( arr1)
           if ptr < 1 then return .F.
   endcase
ACC ->( baseSavePos())
ACC ->( dbGoto( arr2[ ptr, 1]))
rV := wiscBrwCard( arr2[ ptr, 2])
ACC ->( baseRestPos())


return rV


funcDef wiscBrwCard with schet:C ;
   local aH, aC, fk, arrName, arrCode, arr,;
         bUp, bDown, bTop, bBtm, bApp, ptr,;
         wType := "979 ", aBase, aPaym, ;
         arrName1 := {}, arrCode1 := {}, j

   assGetInfo( "10", @arrCode, @arrName)

   if clientIsPens()
      if left( ACC ->priz, 7) == "15 4  1"
         for j = 1 to len( arrCode)
            if subStr( arrCode[ j], 3, 1) == "1"
               aadd( arrCode1, arrCode[ j])
               aadd( arrName1, arrName[ j])
            endif
         next
      else
         for j = 1 to len( arrCode)
            if subStr( arrCode[ j], 3, 1) <> "1"
               aadd( arrCode1, arrCode[ j])
               aadd( arrName1, arrName[ j])
            endif
         next
      endif
   endif

   if !clientIsPens()
      asize( arrName1, 0)
      asize( arrCode1, 0)
      for j = 1 to len( arrCode)
         if subStr( arrCode[ j], 3, 1) <> "1"
            aadd( arrCode1, arrCode[ j])
            aadd( arrName1, arrName[ j])
         endif
      next
   endif

   if len( arrCode1) < 1
      messageB( "Нет типов карт !!!")
      return .F.
   endif

   aBase := { "*", " "}
   aPaym := { "За плату", "Без платы"}
   aH := {{ "Запросы на оформление карт"       },;
          { "Тип карты", {, {|| showArr( arrName1, wsCrGetType( arr))}, arrName1}},;   //OVD
          { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}
   aC := {{ "Дата",           {|| wsCrGetDate(   arr)},,               {|| .F.}},;
          { "Основная",       {|| showArr( aBase, wsCrGetBase(   arr))},{|| .F.}},;
          { "Плата",          {|| showArr( aPaym, wsCrGetPaym(   arr))},{|| .F.}},;
          { "Владелец карты", {|| wsCrGetClient( arr)},,            {|| .F.}}}

   fk := {,,{{ "Новый",  {|| wsCr2Append( schet, .T.)}}},,,,,;
            {{ "Удален", {|| wsCrDelete()  }}}  }

   bUp   := {|| wsCrDecode( @arr, arrCode1, wiscUp(     schet, wType))}  //OVD
   bDown := {|| wsCrDecode( @arr, arrCode1, wiscDown(   schet, wType))}  //OVD
   bTop  := {|| wsCrDecode( @arr, arrCode1, wiscTop(    schet, wType))}  //OVD
   bBtm  := {|| wsCrDecode( @arr, arrCode1, wiscBottom( schet, wType))}  //OVD

   if wsCrCount( schet ) == 0 .And.;
      !yesNo( { "Вы действительно хотите заказать новую карту ?"})
      return .F.
    endif


   if !wiscTop( schet, wType)

	//tat 23/08/04
	//нужно проверить возможность заказа карты
	//23-12-2004 дублировано в процедуре wsCr2Append
	if !wsOrderCnt()
	      return .F.
	endif

	if !wsCrAppend( schet, .T.)
		return .F.
	endif
   endif

// Replaced by TMV 31-03-2004 -- End
   Eval( bTop)
   browseUp( 5, 5, aH, aC, bUp, bDown, bTop, bBtm, bApp,,,, fk)
return .T.

static funcDef showArr with arr:A, ptr:N local rV
  if ptr > len( arr)
     messageB( "Ошибка длины массива !")
     rV := arr[ 1]
  else
     rV := arr[ ptr]
  endif
return rV


static func wsCr2Append( par1, par2, par3)
local rV

	//tat 23/08/04
	//нужно проверить возможность заказа карты
	//23-12-2004 дублировано в процедуре wsCr2Append
	if !wsOrderCnt()
	      return .F.
	endif

   rV := wsCrAppend( par1, par2, par3)
   getCurOB(): refreshAll(): forceStable()
return rV

funcDef zkReestr ;
  local gets, d1, d2, i, fileName, ptr, arrName, arrCode

  d1   := d2 := SetCurDate()
  ptr  := 0
  gets := {{ "Печать реестра заказов карт"},;
           { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
           { "с       :", block( d1)                           },;
           { "по      :", block( d2),,,{|g| g:varGet >= M ->GetList[ 1]:varGet}}}

  assGetInfo( "10", @arrCode, @arrName)

  if len( arrCode) < 1
     messageB( "Нет типов карт !!!")
     return .F.
  endif
  if getUp( 5, 5, gets)
     operStartUp( "Подготовка документа", i := 1, WISC ->( lastRec()))

     fileName := newFPath( "cards.txt", confPathUser())
     if file( fileName) then fErase( fileName)
     zkHeader( d1, d2, fileName)
     WISC ->( baseSavePos())
     WISC ->( dbSetOrder( 2))
     WISC ->( dbSeek( "979 "))
     while WISC ->type == "979 " .AND. WISC ->( !Eof())
       zkLine( @ptr, d1, d2, arrCode, arrName, fileName)
       skip 1 alias WISC
       operProc( i++)
     enddo
     WISC ->( baseRestPos())
     zkFooter( ptr, fileName)
     operExit()
     protView( fileName)
  endif
return NIL

static funcDef zkHeader with d1:D, d2:D, fileName:C local str
   str := chr( 27 ) + chr( 15 ) +;
confBankName()                + endl() +;
"Филиал "    + confFilial()        + endl() +;
"Отделение " + confOtdNumb()       + endl() +;
"                                            РЕЕСТР "      + endl() + ;
"                                            заказов карт" + endl() + ;
"                   за период с " + DtoC( d1) + " по " + DtoC( d2)  + endl() + ;
"------------------------------------------------------------------------------------------------------------------------------------------" + endl() +;
"| N  |   Дата   | Фамилия Имя Отчество                   | Номер карт.счета   | Тип карты                              |Осн./Доп.|С дог./|" + endl() +;
"|    |  заказа  | Фамилия,Имя - латинскими буквами       |                    |                                        |         |без дог|" + endl() +;
"------------------------------------------------------------------------------------------------------------------------------------------"
//123  0123456789 0123456789012345678901234567890123456789 01234567890123456789 0123456789012345678901234567890123456789 012345678 0123456
return eFileWrite( str, fileName)

static funcDef zkLine ;
  with ptr:N, d1:D, d2:D, arrCode:A, arrName:A, fileName:C ;
  local Str, arr, latName, aBase := { "Да", "Нет"},;
        aPaym := { "За плату", "Без платы"}, zkDate

  wsCrDecode( @arr, arrCode, nil)
  if arr[1] <= 0 then Return NIL        // Add by TMV 23-03-2004
  zkDate := wsCrGetDate( arr)
  if zkDate < d1 .OR. zkDate > d2 then return NIL

  latName := wsCrGetLatName( arr) + " " + wsCrGetLSName( arr )
  Str := "|" + Str( ++ptr, 4) + "|" + DtoC( zkDate) + ;
         "|" + padR( wsCrGetClient( arr), 40) + "|" + WISC ->schet + ;
         "|" + padR( arrName[ arr[ 1] ] , 40) + ;
         "|" + padR( aBase[ wsCrGetBase(   arr)],  9) + ;
         "|" + padR( aPaym[ wsCrGetPaym(   arr)],  7) + "|"
if !empty( latName)
   Str += endl()
   Str += "|" + Space( 4) + "|" + Space( 10) + "|" + padR( latName, 40) + ;
          "|" + Space(20) + "|" + Space( 40) + "|" + Space( 9) + ;
          "|" + Space( 7) + "|"
endif
return eFileWrite( str, fileName)

static funcDef zkFooter with ptr:N, fileName:C local str, i
   str := ;
"------------------------------------------------------------------------------------------------------------------------------------------" + endl() + ;
"Итого заказов " + nStr( ptr) + endl() + ;
"Подпись составителя ___________________"

str += endl() + chr( 27 ) + chr( 64 )

for i := 1 to 16 do str := str + endl()

return eFileWrite( str, fileName)


funcDef wsCrDelete local rV
   if alarm( { "Вы действительно хотите удалить запись ?"}, { "Нет", "Да"}) == 2
      rV := WISC ->( mDelete())
      getCurOB(): refreshAll(): forceStable()
   endif
return rV


//tat 23/08/04
// возможность заказать вторую карту за день - 
// проверка на наличие имеющихся карт
funcDef wsOrderCnt local arr1, arr, i, pr1:=0, pr2:=0, rV

  accPrizList( "15 4", @arr1, @arr)
  if (len(arr) <= 0)
    rV := .T.
  else
    for i := 1 to len( arr)
      // количество карт на счетах клиента
      pr1 += wsCrCount( arr[i][2], 1)
      // количество сделанных и пока не исполненных заказов
      pr2 += wsCrCount( arr[i][2], -1)
    next

    if (pr1 == 0) .and. (pr2 > 0)
           messageB( 'При отсутствии карт у клиента сегодня можно заказать только одну карту !!!')
           rV := .F.
    else
           rV := .T.
    endif
  endif

return rV
//
funcDef wsCrAppend with schet:C, mustPrint:LU, retArr:AU ;
   local rV, arr, arrName, arrCode, isBase, nextRV      ,;
         tmpCard, tmpWhat, tmpLatSName, tmpLatName      ,;
         tmpPaym, aPaym, recNo, recNew, newCode,;
         useDPK1 := .F., crfio, crAddr, crType, pict,;
			crCount := 0,;  // Edit by TMV  18-03-2004
			arrMenu := {'Выпуск новой карты','Перевыпуск карты'},; // Add by TMV 18-03-2004
			arrMenu1:= {'Перевыпуск просроченной карты',;
							'Выпуск дубликата карты'},; // Add by TMV 18-03-2004
			arrMenu2:= {'Старый ПИН','Новый ПИН'},; // Add by TMV 18-03-2004
			arrCards := {}, i,;                              // Add by TMV 18-03-2004
			arrCardsPan := {}, arrGetUp, sHeader,;           // Add by TMV 18-03-2004
			typeCard := -1, ptr, ptrCard := 0, sCardPan := '',; // Add by TMV 18-03-2004
			arrType:={'1','1','2','3','4'}, ;                   // Add by TMV 18-03-2004
                        arrCode1 := {}, arrName1 := {}, j, arrS := {}

//  -1 - Error
//   0 - Основная новая карта
//   1 - Доп. новая карта
//   2 - Перевыпуск со старым ПИН
//   3 - Перевыпуск с новым ПИН
//   4 - Выпуск дубликата карты

   DEFAULT mustPrint TO .F.
   aPaym      := { "За плату", "Без платы"}
   tmpCard    := tmpPaym     := 1
   tmpWhat    := 2
   tmpLatName := tmpLatSName := Space( 30)
  
   tmpLatName  := padR( encode2Lat( CLIENTS ->name   ), 30)
   tmpLatSName := padR( encode2Lat( CLIENTS ->surname), 30)

   pict       := replicate( "A", 30)

   assGetInfo( "10", @arrCode, @arrName)

   if clientIsPens()
      if left( ACC ->priz, 7) == "15 4  1"
         for j = 1 to len( arrCode)
            if subStr( arrCode[ j], 3, 1) == "1"
               aadd( arrCode1, arrCode[ j])
               aadd( arrName1, arrName[ j])
            endif
         next
      else
         for j = 1 to len( arrCode)
            if subStr( arrCode[ j], 3, 1) <> "1"
               aadd( arrCode1, arrCode[ j])
               aadd( arrName1, arrName[ j])
            endif
         next
      endif
   endif

   if !clientIsPens()
      asize( arrName1, 0)
      asize( arrCode1, 0)
      for j = 1 to len( arrCode)
         if subStr( arrCode[ j], 3, 1) <> "1"
            aadd( arrCode1, arrCode[ j])
            aadd( arrName1, arrName[ j])
         endif
      next
   endif

   if len( arrCode1) < 1
      messageB( "Отсутствует список типов карт по выбранному счету !!!")
      return .F.
   endif

   rV      := .F.
   crCount = wsCrCount( schet)
   isBase = .F.
      if crCount == 0   // У клиента нет ни карт, ни заявки
         isBase = .T.
         typeCard = 0
      else
         arrCards := wsCardList(schet)
         if Len(arrCards) > 0 // Есть карты
            ptr = popUp(arrMenu)
            do case
               case ptr == 1
                  typeCard = if( aScan( arrCards, {|x| x[2] > 0} ) > 0, 1, 0)
                  if typeCard == 0 .And. wsCrCount( schet, -1) > 0  // Add by TMV 09-04-2004
                     MessageB('У клиента еще нет основной карты !') // Add by TMV 09-04-2004
                     typeCard = -1                                  // Add by TMV 09-04-2004
                  else                                              // Add by TMV 09-04-2004
                     isBase = (typeCard == 0)
                  endif                                             // Add by TMV 09-04-2004
               case ptr == 2
                  arrCards := wsCardList(schet, 'A,D')  // Add 19-04-2004
                  if Len(arrCards) > 0                  // Add 19-04-2004
                     if (ptr := popUp(arrMenu1)) > 0
                        aEval( arrCards, {|x| aAdd( arrCardsPan, x[1] +' - '+ x[5])})
                        ptrCard = popUp(arrCardsPan)
                        if ptrCard > 0
                           isBase = (arrCards[ptrCard,4] == 1) // Add by TMV 01-04-2004
                           sCardPan = arrCards[ptrCard,1]
                           if arrCards[ptrCard,2] == 2
                              if ptr == 1
                                 if (ptr := popUp(arrMenu2)) > 0
                                    typeCard = ptr + 1
                                 endif
                              else
                                 typeCard = 4
                              endif
                           else                         // Add 19-04-2004
                              MessageB('Необходимо заказать новую карту !')  // Add 19-04-2004
                           endif                        // Add 19-04-2004
                        endif
                     endif
                  else
                     MessageB('У клиента нет активных карт !')
                  endif
            endcase
         else      // Нет карт, но уже есть заявка на выпуск
              MessageB('У клиента еще нет основной карты !') // Add by TMV 09-04-2004
         endif
      endif
      if typeCard < 0 then return rV

   recNo   := CLIENTS ->( recNo())
   crfio   := crAddr := crType := ""

   CLIENTS ->( baseSavePos())
   if !isBase .And. typeCard < 2  // Add by TMV 15-04-2004
      recNew := 0; newCode := ""
      nextRV := .F.
      if yesNo( "Владелец карты является клиентом отделения ?")
        if viewOwner( @newCode, @recNew)
           CLIENTS ->( dbGoTo( recNew))
           nextRV := .T.
        endif
      else
         nextRV := cliNewClient()
      endif
      crFio   := cliFullName()
      crAddr  := cliFullAddress()
      useDPK1 := !isBase .AND. nextRV
      tmpLatName  := padR( encode2Lat( CLIENTS ->name   ), 30)
      tmpLatSName := padR( encode2Lat( CLIENTS ->surname), 30)
   else
      nextRV := .T.
   endif

   if nextRV .AND. crCount <= 15
        if typeCard < 2
      	arrGetUp := {{ "Оформление новой "+if(typeCard == 0, "основной", "дополнительной")+" карты"  },;
                       { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
                       { "Тип карты         :", block( tmpCard), arrName1 },;
                       { "*" + chr( 25)                                  },;
                       { "Латинское имя     :", block( tmpLatName), pict,,{|g| sayCheckLat( g:varGet)} },;
                       { "Латинская фамилия :", block( tmpLatSName),pict,,{|g| sayCheckLat( g:varGet)} }}
        else
           do case
              case typeCard = 2
                 sHeader = 'Перевыпуск со старым ПИН'
              case typeCard = 3
                 sHeader = 'Перевыпуск с новым ПИН'
              case typeCard = 4
                 sHeader = 'Выпуск дубликата карты'
           endcase
           tmpCard = 0  // Add by TMV 15-04-2004

           if ptrCard > 0 then tmpCard = aScan( arrCode1, arrCards[ ptrCard, 3]) //OVD

           if tmpCard == 0 then MessageB('Данный тип карты не разрешено использовать!') // Add by TMV 15-04-2004
              arrGetUp := {{ sHeader  },;
                       { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
                       { "Номер карты       : " + sCardPan + ' ' +;
                               if(tmpCard > 0, arrName1[ tmpCard], '') },;
                       { " "                                       },;
                       { "Латинское имя     :", block( tmpLatName), pict,,{|g| sayCheckLat( g:varGet)} },;
                       { "Латинская фамилия :", block( tmpLatSName),pict,,{|g| sayCheckLat( g:varGet)} }}
        endif
        if tmpCard > 0 .And. getUp( 5, 5, arrGetUp )
           retArr := { arrName1[ tmpCard], tmpLatName, tmpLatSName,;  // OVD
                    aPaym[ tmpPaym],   aCrWhat()[ tmpWhat]}
           crType := arrName1[ tmpCard]                               // OVD

         if WISC ->( mAppend())
            WISC ->schet  := schet
            WISC ->type   := "979"
            WISC ->buffer := arrCode1[ tmpCard]     + ";" + ;   // OVD
                            CLIENTS ->code        + ";" + ;
                            tmpLatName            + ";" + ;
                            tmpLatSName           + ";" + ;
                            myDtoC( setCurDate()) + ";" + ;
                            if( isBase,"1","2") + ";" + ; // Edit by TMV 01-04-2004
                            nStr( tmpPaym)      + ";" + ;
                            arrType[typeCard+1] + ";" + ; // Edit by TMV 01-04-2004
                            sCardPan + ";"  // Moved from position 1 by TMV 01-04-2004
            WISC ->( netUnLock())

            rV := .T.
            if mustPrint

               crFio  := razbivka( crFio , {{ 31, "L"}, { 31, "L"}})
               crAddr := razbivka( crAddr, {{ 19, "L"}, { 19, "L"}, { 19, "L"}, { 19, "L"}})
               crType := razbivka( crType, {{ 20, "L"}, { 20, "L"}})

               ACC ->( baseSavePos())
               ACC ->( dbSetOrder( 1))
               ACC ->( dbSeek( schet))
               CLIENTS ->( dbGoTo( recNo))
               servDummy( {{ "@latfio",   rTrim( tmpLatName) + " " + rTrim( tmpLatSName) },;
                           { "@crd_type", arrName1[ tmpCard]          },;  //OVD
                           { "@schet_type", accSchetType( ACC ->PRIZ)},;
                           { "@schet718", schet                      },;
                           { "@crfio1", crFio[ 1]                    },;
                           { "@crfio2", crFio[ 2]                    },;
                           { "@craddr1", crAddr[ 1]                  },;
                           { "@craddr2", crAddr[ 2]                  },;
                           { "@craddr3", crAddr[ 3]                  },;
                           { "@craddr4", crAddr[ 4]                  },;
                           { "@crtype1", crType[ 1]                  },;
                           { "@crtype2", crType[ 2]                  },;
                           { "@d1",      setCurDate() + 14           },;
                           { "@d2",      setCurDate() + 15           } })

               ACC ->( baseRestPos())

               if useDPK1 
                  printOpen( "DPK1")
               else
                 if confUseAgent()
                    printOpen( "kk1")
                 else
                   if arrCode[ tmpCard] == CR_VISA_ELECTRON
                      printOpen( "CRD22")
                   else
                      printOpen( "kk1")
                   endif
                 endif
               endif
               servDummy( {})
               if conf12UseAgent()
                  if dialog({"Карточка образцов подписей оформлена? ", "" } , {"Да", "Нет"}) = 2
                     // Печать карточки с образцами подписей
                     aadd( arrS, { "", "", "", "", ACC->( recNo( ))})
                     SKSPrintOpen( arrS, 1)
                  endif
               endif

            endif
        endif
      endif
   else
      if crCount > 15
         messageB( "Превышено кол-во карт !!!")
      endif
   endif
   CLIENTS ->( baseRestPos())
return rV
//
funcDef wsCrCount with schet:C,;
		whereSeek:NU ;  // Add by TMV 09-04-2004
		local rV := 0, arr, i, arr2

DEFAULT whereSeek TO 0  // Add by TMV 09-04-2004
// 0 - искать везде
// 1 - искать только в CONTRACT
//-1 - искать только в WISC

if whereSeek >= 0        // Add by TMV 09-04-2004
   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 4))
   if CONTRACT ->( dbSeek( schet))
      do while !CONTRACT->(Eof()) ;                // Ad by TMV 31-03-2004
         .And. CONTRACT->CREDITACC == schet     // Ad by TMV 31-03-2004
           arr := CtoA( CONTRACT ->mBuffer, ";")
           if len( arr) > 0
      	      if empty( arr[ len( arr)])
                 aSize( arr, len( arr) - 1)
              endif
   	   endif
      	for i := 1 to len( arr)
           arr2 := CtoA( arr[ i], ",")
           if len( arr2) >= 3 .AND. arr2[ 3] $ "A,N,O,R,D,M,S,B,C" 
			  	                             // Add <D> by TMV 09-04-2004
			  	                             // Add <M,S,B,C> by TMV 19-04-2004
              rV += 1
           endif
        next
			CONTRACT->(dbSkip())                      // Ad by TMV 31-03-2004
      enddo                                        // Ad by TMV 31-03-2004
      
   endif
   CONTRACT ->( baseRestPos())
endif                   // Add by TMV 09-04-2004

if whereSeek <=0        // Add by TMV 09-04-2004
   WISC ->( baseSavePos())
   WISC ->( dbSetOrder( 1))
   if WISC ->( dbSeek( schet + "979 "))
      while WISC ->schet == schet .AND. WISC ->type == "979 "
         rV++
         skip 1 alias WISC
      enddo
   endif
   WISC ->( baseRestPos())
endif                   // Add by TMV 09-04-2004

return rV

funcDef wsCrDecode with arr:AU, arrCode:A, retCode:LU;
   local ptr
   arr := CtoA( WISC ->buffer, ";")
   if empty( arr) then return retCode
   ptr := max( 1, aTrimmedScan( arrCode, arr[ 1]))
   if Len(arr) > 8 .And. Len( arr[9] ) > 2
      arr[1] := 0
   else
      arr[ 1] := ptr
   endif
return retCode

funcDef wsCrGetType with arr:A, newValue:NU ;
   local oldValue
   if empty( arr) then return 1
   oldValue := val( arr[ 1])
   if !isNIL( newValue)
      arr[ 1] := nStr( newValue)
   endif
return oldValue

funcDef wsCrGetClient with arr:A, newValue:CU ;
   local oldValue
   if empty( arr) then return Space( 30)
   CLIENTS ->( baseSavePos())
   CLIENTS ->( dbSetOrder( 1))
   CLIENTS ->( dbSeek( arr[ 2]))
   oldValue := cliFIO()
   if !isNIL( newValue)
      arr[ 2] := newValue
   endif
   CLIENTS ->( baseRestPos())
return oldValue

funcDef wsCrGetLatName with arr:A, newValue:CU ;
   local oldValue

   if empty( arr) then return Space( 30)
   oldValue := arr[ 3]
   if !isNIL( newValue)
      arr[ 3] := newValue
   endif
return allTrim( oldValue )

funcDef wsCrGetLSName with arr:A, newValue:CU ;
   local oldValue
   if empty( arr) then return Space( 30)
   oldValue := arr[ 4]
   if !isNIL( newValue)
      arr[ 4] := newValue
   endif
return allTrim( oldValue )

funcDef wsCrGetDate with arr:A, newValue:DU ;
   local oldValue
   if empty( arr) then return CtoD( "")
   oldValue := CtoD( arr[ 5])
   if !isNIL( newValue)
      arr[ 5] := DtoC( newValue)
   endif
return oldValue

funcDef wsCrGetBase with arr:A, newValue:NU ;
   local oldValue

   if empty( arr) then return 1
   oldValue := val( arr[ 6])
   if !isNIL( newValue)
      arr[ 6] := nStr( newValue)
   endif
return oldValue

funcDef wsCrGetPaym with arr:A, newValue:NU ;
   local oldValue
   if empty( arr) then return 1
   oldValue := val( arr[ 7])
   if !isNIL( newValue)
      arr[ 7] := nStr( newValue)
   endif
return oldValue

funcDef wsCrGetWhat with arr:A, newValue:NU ;
   local oldValue
   if empty( arr) then return 1
   oldValue := val( arr[ 8])
   if !isNIL( newValue)
      arr[ 8] := nStr( newValue)
   endif
return oldValue


* NEW OBJECT FILE
function agentInfo
static dbInfo := { ".\AGENT.DBF", "AGENT",;
                  {{ "f_name",   "C", 20, 0},;
                   { "name",     "C", 20, 0},;
                   { "s_name",   "C", 20, 0},;
                   { "post",     "C", 80, 0},;
                   { "docum",    "C", 80, 0},;
                   { "Act",      "C",  1, 0},;
                   { "ID1",      "C",  6, 0}},;
                  {{, "ACT + F_NAME + NAME + S_NAME"},;
                   {, "ID1"                }}, 0}

return dbInfo

funcDef agentPath with newPath:CU
return basePath( agentInfo(), newPath)

funcDef agentOpen
return baseMOpen( agentInfo())

funcDef agentClose
return baseMClose( agentInfo())

autofunction setValue agentOK init .T.

funcDef agentSelect local str := "", s := select(), b, aH, aC
agentOK( .F.)
if agentOpen()
   b := {|| AGENT ->ACT = "1"}
   netGoTop( b)
   aH := {{ "Представители банка"}}
   aC := {{ "Фамилия",  {|| AGENT ->f_name}},;
          { "Имя    ",  {|| AGENT ->name  }},;
          { "Отчество", {|| AGENT ->s_name}}  }
   if viewUp( 5, 5, aH, aC,;
          {|| netSkipUp( b)}, {|| netSkipDown( b)},;
          {|| netGoTop(  b)}, {|| netGoBottom( b)}) > 0
      str := rTrim( AGENT ->post) + " " + rTrim( AGENT ->f_name) + " " + ;
             rTrim( AGENT ->name) + " " + rTrim( AGENT ->s_name) + " " + ;
             rTrim( AGENT ->docum)
      str := rTrim( str)
      agentOK( .T.)
   endif
   agentClose()
   select ( s)
endif
return str

funcDef agentBrowse local s := select(), aH, aC, fk
if agentOpen()
   fk := {,,, {{ "Редакт", {|| agentEdit()} }} }
   aH := {{ "Представители банка"}}
   aC := {{ "Фамилия",  {|| AGENT ->f_name}},;
          { "Имя    ",  {|| AGENT ->name  }},;
          { "Отчество", {|| AGENT ->s_name}}}
   browseUp( 5, 5, aH, aC, ;
          {|| netSkipUp()}, {|| netSkipDown()},;
          {|| netGoTop( )}, {|| netGoBottom()}, {||agentApp()},,,, fk)
   agentClose()
   select ( s)
endif
return NIL

funcDef agentEdit local rV := .F.

if AGENT ->( mRLock())
   if getUp( 5, 5,;
           {{ "Фамилия",   block( AGENT ->f_Name),,,{|g| sayIfEmpty( g)}},;
            { "Имя",       block( AGENT ->name  ),,,{|g| sayIfEmpty( g)}},;
            { "Отчетсво",  block( AGENT ->s_Name),,,{|g| sayIfEmpty( g)}},;
            { "Должность", block( AGENT ->post  ),,,{|g| sayIfEmpty( g)}},;
            { "Документ",  block( AGENT ->docum ),,,{|g| sayIfEmpty( g)}}})
      AGENT ->f_Name := upper( AGENT ->f_Name)
      AGENT ->s_Name := upper( AGENT ->s_Name)
      AGENT ->Name   := upper( AGENT ->Name)
      getCurOB(): refreshAll(): forceStable()
      rV := .T.
   endif
   AGENT ->( netUnLock())
endif

return rV


funcDef agentApp local rV, recN
   if AGENT ->( rV := mAppend())
      recN := AGENT ->( recNo())
      if agentEdit()
         AGENT ->( dbGoTo( recN))
         if AGENT ->( mRLock())
            AGENT ->ID1  := getNextAgentID()
            AGENT ->ACT  := "1"
            AGENT ->( netUnLock())
         endif
      endif
      getCurOB():goTop()
      getCurOB(): refreshAll(): forceStable()
   endif
return .F.

static funcDef getNextAgentID local rV := ""
   AGENT ->( baseSavePos())
   AGENT ->( netGoTop())
   while AGENT ->( !eof())
     if val( AGENT ->ID1) > val( rV) then rV := AGENT ->ID1
     skip 1 alias AGENT
   enddo
   AGENT ->( baseRestPos())
   rV := padL( val( rV) + 1, 6, "0")
return rV

* NEW OBJECT FILE
funcDef wsBlockCard ;
  local a1, a2, d1, d2, cDest, crdNumb, ptr, clientFIO, aCr, curSchet, i, Str,;
		  adr := razbivka( cliFullAddress(), {{ 30, "L"}, { 40, "L"}}),; // Add by TMV 24-03-2004
		  pasGiv := razbivka( cliPasGiv(), {{ 40, "L"}, { 40, "L"}}),;   // Add by TMV 24-03-2004
		  aCrPriz := {},;   // Add by TMV  19-05-2004
		  aCrMenu := {}     // Add by TMV  27-05-2004

  accPrizList( "15 4  ", @a1, @a2)
  if len( a1) < 1
     messageB( "У клиента нет открытых карточных счетов !")
     return NIL
  endif
  ptr := 1  
  if len( a1) > 1 then ptr := popUp( 2, 2, a1)
  if ptr < 1 then return NIL
  curSchet := a2[ ptr, 2]
  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 1))
  CONTRACT ->( dbSeek( cliCode() + "09"))
  aCr := {}
  while CONTRACT ->code  == CLIENTS ->code .AND. ;
        CONTRACT ->code2 == "09"           .AND. ;
        CONTRACT ->( !eof())
    if CONTRACT ->creditAcc == curSchet
       Str := CtoA( CONTRACT ->mBuffer, ";")
       for i := 1 to len( Str)
           if !empty( Str[ i])
              Str[ i] := CtoA( Str[ i], ",")
              aAdd( aCr, crGetPAN( Str[ i]))
				  aAdd( aCrPriz, Val(allTrim(crGetOWSpriz( Str[ i] ))) ) // Add by TMV 19-05-2004
				  aAdd( aCrMenu, allTrim(crGetPAN( Str[ i]) + ' ' + ;
				  		allTrim(Str[i, 3])))   // Add by TMV  27-05-2004
           endif
       next
    endif
    skip 1 alias CONTRACT
  enddo
  CONTRACT ->( baseRestPos())
  if len( aCr) < 1
     messageB( "На счете " + curSchet + " нет карт !!!")
     return NIL
  endif
  d1        := setCurDate()
  d2        := setCurDate()
  cDest     := Space( 40)
  clientFIO := padR( cliFullName(), 60)
  crdNumb   := 1

  if !getUp( 5, 5,{{ "Блокировка карты                "               },;
                   { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"               },;
                   { "Клиент                   " + cliFullName()      },;
                   { "Карточный счет           " + curSchet           },;
                   { ""                                               },;
                   { "Номер карты              ", block( crdNumb), aCrMenu},;
                   { "Причина постановки       ", block( cDest)       };
						 })
     return NIL
  endif 
  str := ""
  if empty( clientFIO) then str := "Вы не ввели данных клиента !"
  d2 = d1

  if aCrPriz[crdNumb] = 0 ;
  		then str = 'Карта ' + aCr[ crdNumb] + ' не мигрирована в OpenWay! блокировка невозможна.'

  if !empty( str)
     messageB( str)
     return NIL
  endif
  servDummy( {{ "@clientFIO", cliFullName()       },;
              { "@crdNumb",   aCr[ crdNumb]       },;
              { "@cDest",     cDest               },;
              { "@d1",        rDate( d1)          },;
              { "@d2",        rDate( d2)          },;
              { "@rdate",     rDate( setCurDate())},;
              { "@date",      DtoC( setCurDate())},;     // Add by TMV 24-03-2004
				  { "@pasnum",    cliPasSer() + ' ' + cliPasNum()},; // Add by TMV 24-03-2004
              { "@adr1",      adr[1]},;                  // Add by TMV 24-03-2004
              { "@adr2",      adr[2]},;                  // Add by TMV 24-03-2004
              { "@pasgiv1",   pasgiv[1]},;               // Add by TMV 24-03-2004
              { "@pasgiv2",   pasgiv[2]},;               // Add by TMV 24-03-2004
              { "@tel",       cliPhone()},;              // Add by TMV 24-03-2004
				  { "@cliFIO" ,   allTrim(cliSurname()) + ' ' +;
					Left( cliName(), 1) + '.' +;
					Left( cliSName(), 1) + '.' };             // Add by TMV 24-03-2004
				  })
  printOpen( "c_d10")
  servDUmmy( {})
  if WISC ->( mAppend())
     WISC ->schet  := curSchet
     WISC ->type   := "978 "
     WISC ->buffer := aCr[ crdNumb] + ";" + curSchet + ";" + "C" + ";" + ;
                      myDtoC( d1) + ";" + myDtoC( d2) + ";"
     WISC ->( netUnLock())
  else
     errLog( "Поручение отправить не удалось !!!")
  endif
return NIL


funcDef wsStopList ;
  local a1, a2, d1, d2, cDest, crdNumb, ptr, clientFIO, aCr, curSchet, i, Str,;
		  aCrPriz := {},;   // Add by TMV  19-05-2004
		  aCrMenu := {}     // Add by TMV  27-05-2004

  accPrizList( "15 4  ", @a1, @a2)
  if len( a1) < 1
     messageB( "У клиента нет открытых карточных счетов !")
     return NIL
  endif
  ptr := 1
  if len( a1) > 1 then ptr := popUp( 2, 2, a1)
  if ptr < 1 then return NIL
  curSchet := a2[ ptr, 2]
  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 1))
  CONTRACT ->( dbSeek( cliCode() + "09"))
  aCr := {}
  while CONTRACT ->code  == CLIENTS ->code .AND. ;
        CONTRACT ->code2 == "09"           .AND. ;
        CONTRACT ->( !eof())
    if CONTRACT ->creditAcc == curSchet
       Str := CtoA( CONTRACT ->mBuffer, ";")
       for i := 1 to len( Str) 
           if !empty( Str[ i])
              Str[ i] := CtoA( Str[ i], ",")
              aAdd( aCr, crGetPAN( Str[ i]))
				  aAdd( aCrPriz, Val(allTrim(crGetOWSpriz( Str[ i] ))) ) // Add by TMV 19-05-2004
				  aAdd( aCrMenu, allTrim(crGetPAN( Str[ i]) + ' ' + ;
				  		allTrim(Str[i, 3])))   // Add by TMV  27-05-2004
           endif
       next
    endif
    skip 1 alias CONTRACT
  enddo       
  CONTRACT ->( baseRestPos())
  if len( aCr) < 1
     messageB( "На счете " + curSchet + " нет карт !!!")
     return NIL
  endif
  d1        := setCurDate() + 1
  cDest     := Space( 40)
  clientFIO := padR( cliFullName(), 60)
  crdNumb   := 1

  if !getUp( 5, 5,{{ "Постановка в стоп-лист"                         },;
                   { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"               },;
                   { "Клиент                   " + cliFullName()      },;
                   { "Карточный счет           " + curSchet           },;
                   { ""                                               },;
                   { "Номер карты              ", block( crdNumb), aCrMenu}})
     return NIL
  endif 

  if aCrPriz[crdNumb] = 0
  		MessageB('Карта ' + aCr[ crdNumb] + ' не мигрирована в OpenWay! блокировка невозможна.')
		return NIL
  endif

  servDummy( {{ "@clientFIO", cliFullName()       },;
              { "@crdNumb",   aCr[ crdNumb]       },;
              { "@rdate",     rDate( setCurDate())}})
  printOpen( "crd21")
  servDUmmy( {})
  if WISC ->( mAppend())
     WISC ->schet  := curSchet
     WISC ->type   := "978 "
     WISC ->buffer := aCr[ crdNumb] + ";" + curSchet + ";" + "S" + ";" + ;
                      myDtoC( d1) + ";;"
     WISC ->( netUnLock())
  else
     errLog( "Поручение отправить не удалось !!!")
  endif
return NIL


static funcDef checkLat with str:C local rV := .T., i, ch
  for i := 1 to len( str)
    ch := subStr( str, i, 1)
    if ( Upper( ch) < "A" .OR. Upper( ch) > "Z" ) .And. Upper( ch) <> "-" // Edit by TMV  23-11-2001
      rV := .F.
    endif
  next
return rV

static funcDef sayCheckLat with cStr:C local rV, nKey
  rV   := .T.
  nKey := lastKey()
  if empty( cStr ) // .and. ( nKey != K_SH_TAB .and. nKey != K_UP )
    messageB( "Поле должно быть заполнено !")
    rV := .F.
  endIf
  if rV
    rV := checkLat( allTrim( cStr ))
    if !rV then messageB( "Допустимы только латинские буквы !" )
  endIf
return rV

static funcDef encode2Lat with  cStr:C;
                          local rV := "", i, ch, aRus, aLat, ptr, nLen
// Edit by TMV  23-11-2001 ( Add "-" )
  aRus := { "А", "Б", "В", "Г", "Д", "Е", "Ж", "З", "И", "Й", "К", "Л", "М", "Н",;
            "О", "П", "Р", "С", "Т", "Ф", "Х", "Э", "Ц", "Ъ", "Ь", "Ш", "Щ", "Ч",;
            "Ю", "Я", "У", "Ы" , "-" }
  aLat := { "A", "B", "V", "G", "D", "E", "J", "Z", "I", "I", "K", "L", "M", "N",;
            "O", "P", "R", "S", "T", "F", "H", "E", "C", "Y", "'", "SH", "SH", "CH",;
            "JU", "JA", "OU", "YI" , "-" }
  nLen := len( cStr )
  for i := 1 to nLen
    ch  := subStr( cStr, i, 1 )
    if ch $ "Ьь"
      if ( i == nLen ) .or. !( subStr( cStr, i + 1, 1 )  $ "АЕИОЫЭЮЯ" )
        ch := ""
      else
        ch := if( ch == "Ь", "Y", "y" )
      endIf
    else
      ptr := aScan( aRus, {| x | x == ch })
      if ptr > 0 
        ch := aLat[ ptr]
      else
        ptr := aScan( aRus, {| x | lower( x ) == lower( ch )})
        if ptr > 0 then ch := lower( aLat[ ptr ])
      endIf 
    endIf
    rV  += ch
  next
return rV

function isWiscBik(sSchet, nDoc, sBic)
local rV:=.F., n:=WISC->(recNo()), nOrd:=WISC->(dbsetorder())
WISC->(dbsetorder(1))
WISC->(dbseek(sSchet))
do while WISC->Schet == sSchet .And. !WISC->(eof()) .And.;
		!(rV:=(getWiscBic(nDoc) == sBic))
	WISC->(dbskip())
enddo
WISC->(dbsetorder(nOrd))
WISC->(dbgoto(n))
return rV

function getWiscBic(nDoc)
local rV:=WISC->Buffer, n:=0
if (n:=at('DOCNO:', rV))>0
	if (n:=Val( Alltrim( Substr(rV, n+6, 6) )))==nDoc
		n=at('BIC:', rV)
		rV=( Substr( rV, n+4, 9))
	endif
endif
return rV

// 
// Пожелание для перехода с 42309 на 42301
//
funcDef wiscTran42309 local rV := .F., sSchetNew,;
	openedW := (Select('WISC') > 0), wiscOrd := 0, wiscRec, iSelect := Select(),;
	sSchet, sCode := CLIENTS->CODE,;
	mnSumma := 0, lWisc := .F., lSum := .F.,;
	openedS := (Select('SUMS') > 0),;
	aOrder := {}, aWisc := {},;
	bankDest := if( confUseAgent(), Alltrim(configValue('FILIALNAME')),;
		confBankName() + " " + rTrim( confFilial()) + " " + confOtdName()),;
	theDest := padR( cliFullName(), 50),;
	curDest := 'Перечисление ден. ср-в в соотв. с заявл. ' +;
		'клиента о закрытии счета от ' + DtoC(setCurDate()),;
	curSource := cliFullName(), memDoc := 0,;
	sBikDest := confBic(),;
	KorSchet := confKorConfSchet(), aDist := {},;
	sInnDest := '', summa := 0


if !file('s42309.prn') .Or.;
		empty(sSchetNew := getPens42301(sCode, .T.)) .Or.;
		empty(sSchet := getPens42301(sCode, .F.)) Then Return .T.
	
lWisc = (openedW .Or. wiscOpen()) // Add by TMV  07-08-2003
lSum = (openedS .Or. sumOpen())   // Add by TMV  07-08-2003

if lWisc .And. lSum
	SUMS ->( baseSavePos())                   // Add by TMV  07-08-2003
	mnSumma = sumRest( sSchet, setCurDate() ) // Add by TMV  07-08-2003
	wiscOrd = WISC->(indexOrd())
	wiscRec = WISC->(recNo())
	WISC->(dbSetOrder(1))
	WISC->( dbSeek( sSchet, .T. ))
	do While WISC->SCHET == sSchet .And. !WISC->(Eof())
		if allTrim(WISC->Type) == '855'
			rV = .T.
			Exit
		endif
		WISC->(dbSkip())
	enddo
	if !rV
		if yesNo( { "Клиент подписал заявление на перевод",;
						 "пенсионного счета с 42309 на 42301 ?"})

			if mnSumma > 0
				memDoc = ordIncMem( setCurDate())
				aDist = razbivka( curDest, {{50, "L"}, {50, "L"}, {50, "L"}} )
				if confUseAgent()
					aWisc := platPorPerevod( sSchet, sSchetNew, bankDest,;
						mnSumma, curDest, theDest, memDoc, KorSchet, sBikDest, sInnDest)
				else
					aOrder  := memOrd2( memDoc, setCurDate(), sSchet, sSchetNew,;
						mnSumma, curDest, "9", bankDest, theDest)
					aWisc := memWisc2( sSchet, sSchetNew, theDest, mnSumma, curDest,;
						aDist)
				endif
				printAOrder( aWisc)
				if Len( aOrder ) > 0 Then printAOrder( aOrder)
			endif

			if (rV := wiscAppend())
				WISC->SCHET := sSchet
				WISC->TYPE := '855'
				WISC->BUFFER := sSchet + ';' + sSchetNew + ';' + Str(0, 20)
				wiscUnLock()
				if mnSumma > 0 then;
					rV = sumWrite( sSchet, -mnSumma, setCurDate(),;
						'8700', 1, , 'F' + sSchetNew) // Add by TMV  07-08-2003
			endif
		else
			rV = .T.
		endif
	elseif yesNo( { "Снять отметку о согласии на перевод",;
						 "пенсионного счета с 42309 на 42301 ?"})
		if (rV := WISC ->( mDelete()))

         SUMS ->( dbSetOrder( 1))
         SUMS ->( dbSeek( sSchet, .T.))
         while SUMS ->SCHET == sSchet .AND. SUMS ->( !Eof())
				if opEqual( SUMS ->typeOf, '8700') .AND. SUMS->DDATE = setCurDate()
					if ( rV := SUMS ->( mDelete()) ) then SUMS ->( netUnLock())
					exit
				endif
				SUMS->( dbSkip() )
          enddo
		endif

	endif
	if openedW
		WISC->( dbSetOrder(wiscOrd))
		WISC->( dbGoTo(wiscRec))
	else
		wiscClose()
	endif

	if !openedS
		sumClose()
	else
		SUMS ->( baseRestPos())
	endif
	Select(iSelect)

endif

if !rV
   messageB('Ошибка при записи информации о переводе на счет 42301 !')
endif		
return rV
// 
funcDef wsCardList With curSchet:CU, typeCard:CU,;
	forOWS:CU;     // Add by TMV 09-04-2004
	local rV:={}, Str, aStr1, i

DEFAULT forOWS TO .F.   // Add by TMV 09-04-2004
DEFAULT typeCard TO ''  // Add by TMV 19-04-2004

CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 1))
CONTRACT ->( dbSeek( cliCode() + "09"))
while CONTRACT ->code  == CLIENTS ->code .AND. ;
      CONTRACT ->code2 == "09"           .AND. ;
      CONTRACT ->( !eof())
  if if(isNil(curSchet), .T., CONTRACT ->creditAcc == curSchet)
     Str := CtoA( CONTRACT ->mBuffer, ";")
     for i := 1 to len( Str)
         if !empty( Str[ i])
            aStr1 := CtoA( Str[ i], ",")
				if len( aStr1) >= 3 .AND. ( aStr1[ 3] $ typeCard ; //"A,N,O,R" // Edit by TMV 03-04-2004
						.Or. empty(typeCard) ) ;             // Add by TMV 19-04-2004
						.And. (!forOWS .Or. Val(allTrim(crGetOWSpriz( aStr1 ))) > 0)
            	aAdd( rV, {crGetPAN( aStr1), Val(allTrim(crGetOWSpriz( aStr1 ))),;
								  aStr1[1], crGetBase( aStr1 ),;
								  aStr1[3] })  // Add by TMV 03-04-2004
				endif
         endif
     next
  endif
  skip 1 alias CONTRACT
enddo
CONTRACT ->( baseRestPos())
return rV