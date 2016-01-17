*=============================================================================
*     
*                        Программа  : MON.EXE
*                        Файл       : sumsStrn.prg
*                        Автор      : Цейтлин П.М.
*
*------------------------------------------------------------------------------
*
* 15-11-2002 TMV
* Изменена схема проводок при выдаче микрокредита - добавлена новая проводка.
*  Ф-ии strnDelSumma(), del1007(), sumsCheck(), check1007()
*
* 25-11-2002 TMV
* В измененной схеме проводок вторая проводка 1007 переделана на 1001.
* Ф-ии strnDelSumma(), del1007(), check1007()
*
* 28-11-2002 TMV
* Введена новая операция 1060 (7032.)
*  и операция 1001 по выдаче кредита с ТПС заменена на 1060 (7032).
*  Ф-ии del1007(), check1007(), strnDelSumma().
* 
* 02-12-2002 TMV
* Изменена схема проводок при возврате микрокредита - добавлена новая проводка
*  2060 (7033).
*  ф-ии strnDelSumma(), del2003(), del2004(),
* 
* 05-12-2002 TMV
* При расчете красного сальдо исключены операции 1060, 2060, 7032, 7033
*  Ф-ия strnRedRest()
*
* 09-12-2002 TMV
* Заменены операции при безналичной выдаче ссуды -
*  1007 на 6031, 2003(7004) на 6136, 2004(7005) на 6137.
*  Ф-ии strnDelSumma(), del2003(), del2004(), strnDeleteKassa(), chech2004()
*  Новые ф-ии - del1060()
*
* 17-12-2002 TMV
* Обработка оп. 206  заменена на 6136 и 6137 - strnDelSumma()
* Операция 6136 заменена на 2060 - del2003()
* Операция 6137 заменена на 2060 - del2004(), sumsCheck()
*--ОТМЕНЕНО
*
* 10-01-2003 TMV
* Восстановлена старая схема проводок по выдаче ссуды на Отделениях.
* Ф-ии strnDelSumma(), del2003(), del2004(), sumsCheck(), check1007(),
*  check2004().
*
* 14-10-2003 TMV
* Вставлена проверка на соответствие заполнения поля SUMS->BUFFRES
* правилам НЕстраховой схемы кредитов. Если не соответствует - проверяем
* пожелания по старой схеме.
* Ф-ия check2004()
*
* 05-01-2004 TMV
* Вставлена прверка - работает или нет нестраховая схема кредитов.
* Ф-ия del2003()
*
* 05-02-05 TAT
* исправлены функции 
* del2004, check2004 
* в связи с введением счетов просроченных процентов и соответствующих проводок
* 

#include "clippExt.ch"
#include "common.ch"
#include "gsz.ch"
#include "mon.ch"

funcDef strnView local aH, aC, fk, s, order

   aH := {{ "Удаление проводок"                          },;
          { "Клиент"  , {|| strnViewClient()},, {|| .F.} },;
          { "Операция", {|| strnViewOper()  },, {|| .F.} },;
          { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"}}

   aC := {{ "Запись #", {|| SUMS ->( recNo())}},;
          { "Дата",   {|| SUMS ->dDate     }},;
          { "Счет",   {|| SUMS ->schet     }},;
          { "N док.", {|| SUMS ->nDoc      }},;
          { "Сумма",  {|| SUMS ->summa/ 100}}}
   fk  := {,{{ "Сорт", {|| viewOrder()}}},,,,,;
            {{ "Поиск",{|| viewSeek() }}}    ,;
            {{ "Удален", {|| strnDelSumma()}}} }
   order := SUMS ->( indexOrd())
   SUMS ->( browseUp( 5, 5, aH, aC, {|| netSkipUp()}, {|| netSkipDown()},;
            {|| netGoTop()}, {|| netGoBottom()},,,,, fk))

   SUMS ->( dbSetOrder( order))
return NIL

static funcDef viewOrder local ptr
  ptr := dialog( { "Сортировка проводок по"},;
           { "номеру счета", "сумме"})
  do case
     case ptr == 1
          SUMS ->( dbSetOrder( 1))
     case ptr == 2
          SUMS ->( dbSetOrder( 4))
  endcase
  getCurOB(): refreshAll(): forceStable()
return NIL

static funcDef viewSeek local pict, ord, value
   ord := SUMS ->( indexOrd())
   do case
   case ord == 1
        pict  := "99999999999999999999"
        value := Space( 20)
   case ord == 4
        pict  := "999999999999.99"
        value := 0
   otherwise
        messageB( "Нет поиска по индексу # "  + nStr( ord) + " !!!")
        return NIL
   endcase
   if getUp( 5, 5, {{ "Поиск ", block( value), pict}})
      if ord == 4 then value := Str( value * 100, 15)
      if SUMS ->( dbSeek( value, .T.))
         getCurOB(): refreshAll(): forceStable()
      else
         messageB( "Не найдено !!!")
      endif
   endif
return NIL


* -------------- показ ФИО клиента ---------------------
static funcDef strnViewClient local rV
   ACC     ->( baseSavePos())
   CLIENTS ->( baseSavePos())

   ACC     ->( dbSetOrder( 1))
   CLIENTS ->( dbSetOrder( 1))
   if ACC ->( dbSeek( SUMS ->schet )) .AND. CLIENTS ->( dbSeek( ACC ->code ))
      rV := padR( cliFullName(), 45)
   else
      rV := padR( "Нет данных", 45)
   endif

   CLIENTS ->( baseRestPos())
   ACC     ->( baseRestPos())
return rV

* --------- показ вида операции ------------------------
static funcDef strnViewOper local rV
   OPER ->( baseSavePos())
   OPER ->( dbSetOrder( 1))
   OPER ->( dbSeek( SUMS ->typeOf))
   rV := OPER ->name
   OPER ->( baseRestPos())
return rV

* -------------- проверка на красное сальдо -------------
static funcDef strnRedRest local rV := .F., isClient, sum
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))
   isClient := ACC ->( dbSeek( SUMS ->schet ))
   ACC ->( baseRestPos())
// Add 1060, 2060, 7032, 7033 by TMV  05-12-2002
// Add 6136, 6137 by TMV  09-12-2002
   if isClient .AND. ! ( SUMS ->typeOf $ "5301,5302,1060,2060,7033,7032,6136" )
      sum := round( SUMS ->summa/ 100, 2)
      if left( SUMS ->schet, 3) == "455"
         if SUMS ->summa < 0 .AND. sum < sumRest( SUMS ->schet, setCurDate())
            rV := .T.
         endif
      else
         if SUMS ->summa > 0 .AND. sum > sumRest( SUMS ->schet, setCurDate())
            rV := .T.
         endif
      endif
   endif
return rV

static funcDef strnAsk local rV
   rV := alarm( { "Вы действительно хотите удалить проводку ?"},;
             { "Нет", "Да" }) == 2
return rV
                
funcDef strnDeleteKassa local rV := .T.
   if kasTypes( SUMS ->typeOf)
      rV := kasDeleteSumma( SUMS ->summa/ 100, "999")
   endif

   if opEqual( SUMS ->typeOf, "1007") .AND. SUMS ->summa < 0
		kasDeleteSumma( SUMS ->summa/ 100, "003")
   endif
return rV


* ------------------ удаление проводки ------------------
static funcDef strnDelSumma ;
   local rV := .F., summa, schet2, buffer2, date, summa2, s
//toProc( "Начало удаления проводки")
if strnRedRest()
   messageB( "Удаление этой проводки вызовет красное сальдо !!!")
   return .F.
endif
//toProc( "Проверка на красное сальдо")
if !strnAsk() then return .F.
//toProc( "Определение типа")

do case
   case SUMS ->typeOf $ "9001,9002,9003,9004,9005,9006"   // ордера надомникам
        messageB( 'Операцию можно удалить в режиме "Надомники - Списание"')
   case opArrEqual( SUMS ->typeOf, "1002,1004")   // ордер пенса
        //toProc( "Удаление 1002 ")
        strnDeleteKassa()
        //toProc( "Касса откорректирована ")
        if ( rV := SUMS ->( mDelete()))
           SUMS ->( netUnLock())
        endif
        //toProc( "Проводка удалена ")
   case opArrEqual( SUMS ->typeOf, "3001,3007,3011,3013,3015,3017,1027") // приход по кассе
        strnDeleteKassa()
        if ( rV := SUMS ->( mDelete()))
           SUMS ->( netUnLock())
        endif
   case opArrEqual( SUMS ->typeOf, "3019")   // страховой взнос по карточному овердрафту
        rV := del3019()
   case left( SUMS ->buffer, 1) = "S"
        rV := cardStorn()
   case opArrEqual( SUMS ->typeOf, "3002,3008,1009,3010,3014,3012,3016,3018")   // расход по кассе,пласт.карты
        strnDeleteKassa()
        if ( rV := SUMS ->( mDelete()))
           SUMS ->( netUnLock())
        endif
   case opArrEqual( SUMS ->typeOf, "1001,1003")       // выплата
        //toProc( "Удаление 1002 ")
		 if !empty( SUMS->Buffer ) .And. ACC->(dbSeek( SUMS->Schet )) ; // Add by TMV  25-11-2002
		 		.And. isPens( ACC->PRIZ )    // Add by TMV  25-11-2002
           messageB( "Вы должны удалить проводку по выдаче кредита!") // Add by TMV  25-11-2002
		 else                                 // Add by TMV  25-11-2002
        strnDeleteKassa()
        //toProc( "Касса откорректирована ")
        trustRollBackLastSum( SUMS ->schet, SUMS ->buffer, SUMS ->summa)
        //toProc( "Поиск в TRUST ")
        if del9100() .AND. ( rV := SUMS ->( mDelete()))
           SUMS ->( netUnLock())
        endif
		 endif           // Add by TMV  25-11-2002
        //toProc( "Проводка удалена ")
   case SUMS ->typeOf $ GSZ_BUY_NAL + "," + GSZ_BUY_BEZNAL
        rV := delGSZ()
   case opEqual( SUMS ->typeOf, "2001")       // взнос через кассу
        strnDeleteKassa()
        if ( rV := SUMS ->( mDelete()))
           SUMS ->( netUnLock())
        endif
   case opArrEqual( SUMS ->typeOf, "2012,5011,5511")   // продуктовая забота
        rV := delPZabota()
   case left( SUMS ->buffer, 1) == "A" // одноразовый платеж
        rV := delOnePay()
   case opEqual( SUMS ->typeOf, "5010") .AND. left( SUMS ->buffer, 1) $ "F"   // перечисления на НЕклиентский счет
        strnDeleteKassa()
        if ( rV := SUMS ->( mDelete()))
           SUMS ->( netUnLock())
        endif
   case opEqual( SUMS ->typeOf, "5200") .AND. left( SUMS ->buffer, 1) $ "B,C" // перечисление по клиентским счетам
        rV := del5200()
   case opEqual( SUMS ->typeOf, "6030") .AND. left( SUMS ->buffer, 1) $ "B,C" // кредиты на покупку продуктов
         rV := del6030()
   case opArrEqual( SUMS ->typeOf, "5010,5201") .AND. left( SUMS ->buffer, 1) $ "B,C" // перечисление по клиентским счетам
        strnDeleteKassa()
        summa2  := - SUMS ->summa
        schet2  := subStr( SUMS ->buffer, 2, 20)
        buffer2 := if( left( SUMS ->buffer, 1) == "B", "C", "B") + SUMS ->schet
        buffer2 := padR( buffer2, 30)
        Date    := SUMS ->dDate
        SUMS ->( baseSavePos())
        SUMS ->( dbSetOrder( 1))
        SUMS ->( dbSeek( schet2 + DtoS( date) + Str( summa2, 15)))
        while SUMS ->schet == schet2 .AND. SUMS ->dDate == date .AND. ;
              SUMS ->summa == summa2 .AND. SUMS ->( !Eof())
           exit if ( rV := opArrEqual( SUMS ->typeOf, "5010,5201") .AND. SUMS ->buffer == buffer2)
           skip 1 alias SUMS
        enddo
        if rV
           if rV := SUMS ->( mDelete())
              SUMS ->( netUnLock())
              SUMS ->( baseRestPos())
              if rV := SUMS ->( mDelete())
                 SUMS ->( netUnLock())
              else
                 messageB( "Удалена корреспондирующая проводка, а текущая - НЕТ !!!")
              endif
           endif
        else
           messageB( "Не найдена корреспондирующая проводка!")
           SUMS ->( baseRestPos())
        endif
   //
   // забота( первый взнос )
   //
   case opArrEqual( SUMS ->typeOf, "2010,2011,5010") .AND. len( rTrim(SUMS ->buffer)) = 13 .AND.;
        left( SUMS ->buffer, 2) == "06"
        rV := delZabota()
   case opArrEqual( SUMS ->typeOf, "1051,1052") .AND. SUMS ->summa < 0  // закрытие счета с выплатой остатка
        ACC ->( baseSavePos()); rV := .F.
        ACC ->( dbSetOrder( 1))
        if ACC ->( dbSeek( SUMS ->schet))
           if rV := accRLock()
              ACC ->datClose := CtoD( "")
              ACC ->typeOf   := "00"
              accUnLock()
            endif
        endif
        if rV
           strnDeleteKassa()
           trustRollBackLastSum( SUMS ->schet, SUMS ->buffer, SUMS ->summa)
           if del9100() .AND. SUMS ->( mDelete()) then SUMS ->( netUnLock())
        else
           messageB( "Проводку нельзя удалить!")
        endif
   case opEqual( SUMS ->typeOf, "2002") .AND. SUMS ->summa > 0 // открытие вклада
        if SUMS ->summa > sumRest( SUMS ->schet, setCurDate())
           strnDeleteKassa()
           if SUMS ->( rV := mDelete()) then SUMS ->( netUnLock())
        else
           messageB( "Удаление этой проводки вызовет появление красного сальдо !!!")
        endif
   case opEqual( SUMS ->typeOf, "1007")  .AND. SUMS ->summa < 0 // выдача ссуды
        rV := del1007()
   case opEqual( SUMS ->typeOf, "3006")
        messageB( "Вы должны удалить проводку по выдаче кредита!")
   case opEqual( SUMS ->typeOf, "1060")  .AND. SUMS ->summa < 0 // выдача ссуды безналом
        rV := del1060()
   case confUseAgent() .And. opEqual( SUMS ->typeOf, "2060")
        if ( Left( allTrim( SUMS ->Buffer), 4) == 'PCT:') .OR.;
           ( _isProsrPct( SUMS ->Schet))
              rV := del2004()
        else
              rV := del2003()
        endif
   case opArrEqual( SUMS ->typeOf, "5301,5302")       // урегулирование сальдо узла
        strnDeleteKassa()
        if ( rV := SUMS ->( mDelete()))
           SUMS ->( netUnLock())
        endif
   case opEqual( SUMS ->typeOf, "7100")  // блокировка для перевода денег на другой счет
        if ( rV := SUMS ->( mDelete()))
           SUMS ->( netUnLock())
        endif
   case opEqual( SUMS ->typeOf, "2003")
        if confUseAgent()
            messageB( "Вы должны удалить проводку по погашению кредита!")
        else
            rV := del2003()
        endif
// <confUseAgent()> для работы в режиме Отделения
   case opEqual( SUMS ->typeOf, "2004")  // Add by TMV  02-12-2002
        if confUseAgent()
            messageB( "Вы должны удалить проводку по возврату процентов по кредиту!")
        else
            rV := del2004()
        endif
   otherwise
        messageB( "Удаление этой операции не производится !!!")
endcase
if rV then getCurOB(): refreshAll(): forceStable()
return rV

static funcDef del3019 local rV := .F., canContinue := .F., prevSumma
  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 2))
  if CONTRACT ->( dbSeek( left( SUMS ->buffer, INTDOC_LEN)))
    if CONTRACT ->status = 1 .AND. CONTRACT ->dateCur == setCurDate()
      messageB( "Сначала закройте поручение N " + CONTRACT ->docNumb)
    else
      if CONTRACT ->dateCur == setCurDate()
        canContinue := .T.
      else
        if WISC ->( dbSeek( substr( CONTRACT ->mBuffer, 22, 20) + "955 "))
          if WISC ->( canContinue := mDelete())
             WISC ->( netUnLock())
             if CONTRACT ->( mrLock())
               prevSumma := val( CONTRACT ->buffer)
               CONTRACT ->summa  := round( prevSumma * 100, 0)
               CONTRACT ->buffer := ""
               CONTRACT ->( netUnLock())
             endif
          endif
        endif
      endif
      if canContinue  
        strnDeleteKassa()
        if ( rV := SUMS ->( mDelete())) then SUMS ->( netUnLock())
      endif
    endif
  endif
  CONTRACT ->( baseRestPos())
return rV

static func noWisc
return noYes( "Не найдено пожелание. Продолжать ?")

//
// Удаление проводки по заботе
//
static funcDef delPZabota ;
   local rV := .F., newOper, arr, i, sum1 := 0, sum2 := 0,;
         docNumb, dDate, wiscFound := .F.

   strnDeleteKassa()
   docNumb := left( SUMS ->buffer, 14)
   dDate   := SUMS ->dDate
   WISC ->( dbSeek( SUMS ->schet + "981 "))
   while WISC ->schet == SUMS ->schet .AND. WISC ->type == "981 "
     if left( WISC ->buffer, 14) == docNumb
      if WISC ->( mRLock()) .AND. SUMS ->( mRLock())
         arr := CtoA( WISC ->buffer, ";")
         sum1 := sum2 := 0
         for i := 8 to len( arr)
             if left( arr[ i], 2) == "80" then sum2++
             if left( arr[ i], 2) <> "80" then sum1++
             i++
         next
         wiscFound := .T.
         exit
      end if
     end if
     skip 1 alias WISC
   enddo
   do case
     // мы находимся на проводке с продуктовой заботой и есть лекарства
     case SUMS ->typeOf == "5011" .AND. ( sum2 > 0 .OR. !wiscFound)
       delP2Zab( "5511", dDate, docNumb, SUMS ->schet)
     // мы находимся на проводке с лекарствами и есть продуктовая забота
     case SUMS ->typeOf == "5511" .AND. ( sum1 > 0  .OR. !wiscFound)
       delP2Zab( "5011", dDate, docNumb, SUMS ->schet)
   endcase
   if SUMS ->( mDelete())
     if wiscFound .AND. ( rV := WISC ->( mDelete()))
       WISC ->( netUnLock())
     else
       messageB( "Не могу удалить данные в массиве пожеланий !!!")
       rV := .T.
     endif
   endif
return rV

static funcDef delP2Zab       ;
  with Oper:C, d1:D, docNumb:C, schet:C ;
  local rV := .F.

   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 3))
   SUMS ->( dbSeek( Oper + DtoS( d1) + schet))
   while SUMS ->typeOf == Oper  .AND. SUMS ->dDate == d1 .AND. ;
         SUMS ->schet  == schet .AND. SUMS ->( !eof())
     if left( SUMS ->buffer, 14) == docNumb
        if SUMS ->( rV := mDelete()) then SUMS ->( netUnLock())
     endif
     skip 1 alias SUMS
   enddo
   SUMS ->( baseRestPos())
return rV


//
// удаление заботы
//
static funcDef delZabota local docNumb, wiscFound := .F., oper, arr
  oper    := SUMS ->typeOf
  docNumb := rTrim( SUMS ->buffer)
  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 2))
  if CONTRACT ->( !dbSeek( docNumb))
     CONTRACT ->( baseRestPos())
     messageB( "Не найдена запись в базе долгосрочных поручений !")
     return .F.
  endif
  //
  // это первый взнос, наличка
  //
  if opEqual( oper, "2010")
    if wiscSeek( CONTRACT ->debetAcc, "989 ")
       while WISC ->schet = CONTRACT ->debetAcc .AND. WISC ->( !eof())
         arr := CtoA( WISC ->buffer, ";")
         
         // if left( WISC ->buffer, INTDOC_LEN) = rTrim( SUMS ->buffer) .AND. ;
         if rTrim( arr[ 1]) == rTrim( SUMS ->buffer)           .AND. ;
            round( val( arr[ 9]), 2) == round( SUMS ->summa/ 100, 2) .AND. ;
            WISC ->type == "989 "
           if WISC ->( mDelete()) then wiscFound := .T.
           exit
         endif
         skip 1 alias WISC
       enddo
    endif
    if !wiscFound then messageB( "Пожелание не найдено !")
  endif
  //
  // это последний взнос, наличка
  //
  if opEqual( oper, "2011")
    if wiscSeek( CONTRACT ->debetAcc, "988 ")
       while WISC ->schet = CONTRACT ->debetAcc .AND. WISC ->( !eof())
         arr := CtoA( WISC ->buffer, ";")
         if rTrim( arr[ 1]) == rTrim( SUMS ->buffer)           .AND. ;
            round( val( arr[ 6]), 2) == round( SUMS ->summa/ 100, 2) .AND. ;
            WISC ->type == "988 "
         // if left( WISC ->buffer, INTDOC_LEN) = rTrim( SUMS ->buffer) .AND. ;
         //   WISC ->type == "988 "
           if WISC ->( mDelete()) then wiscFound := .T.
           exit
         endif
         skip 1 alias WISC
       enddo
    endif
    if !wiscFound then messageB( "Пожелание не найдено !")
    if CONTRACT ->( mRLock())
       CONTRACT ->status  := 1
       CONTRACT ->lEdited := .T.
       CONTRACT ->( netUnLock())
    endif
  endif
  //
  // безнал 5010
  //
  if opEqual( oper, "5010")
    messageB( "Такую проводку удалить нельзя !")
     CONTRACT ->( baseRestPos())
     return .F.
  endif
  strnDeleteKassa()
  if SUMS ->( mDelete())
     CONTRACT ->( baseRestPos())
     return .T.
  endif
  CONTRACT ->( baseRestPos())
return .F.

//
// удаление ГСЗ, оно же ГСИО
//
static funcDef delGSZ ;
   local rV := .F., nDoc, intDoc, arr, i, cnt, code, arr2, buf2, wiscFound
   strnDeleteKassa()
   intDoc := padR( left( SUMS ->buffer, INTDOC_LEN), 20)
   nDoc   := SUMS ->( recNo())
   buf2      := subStr( SUMS ->buffer, INTDOC_LEN + 2, 12)
   wiscFound := .F.
   if wiscSeek( intDoc, GSZ_WISC)
      intDoc := WISC ->schet
      while WISC ->schet == intDoc .AND. WISC ->type = GSZ_WISC .AND. ;
            WISC ->( !eof())
        exit if val(WISC ->ID) == nDoc
        skip 1 alias WISC
      enddo
      if val(WISC ->ID) == nDoc
        wiscFound := .T./*
        arr := CtoA( WISC ->buffer, ";")
        for i := 2 to 4
          arr2 := CtoA( arr[ i], ",")
          cnt  := val( arr2[ 2])
          code := arr2[ 1]
          limDeleteSumma( - cnt, code)
        next*/
      endif
   endif
   //if !wiscFound then ;
   //  errOK( { "Не найдено пожелание. После удаления проводки нужно ",;
   //           "откорректировать размер лимита по данным облигациям."})

   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 3))
   if CONTRACT ->( dbSeek( left( intDoc, INTDOC_LEN), .F.))
      arr := CtoA( CONTRACT ->mBuffer, ";")
      for i := 2 to 4
        arr2 := CtoA( arr[ i], ",")
        cnt  := val( arr2[ 2])
        code := arr2[ 1]
        limDeleteSumma( - cnt, code)
      next
     if CONTRACT ->( mDelete())
        trustRollBack( SUMS ->schet, buf2, SUMS ->summa)
        if SUMS ->(  mDelete())
          if wiscFound .AND. WISC ->( mDelete())
             rV := .T.
             WISC ->( netUnLock())
           endif
           SUMS ->( netUnLock())
        endif
        CONTRACT ->( netUnLock())
     endif
   endif
   CONTRACT ->( baseRestPos())

return .T.

//
// Кредиты на покупку продуктов 
//
static funcDef del6030 ;
  local rV := .F., summa2, schet2, buffer2, Date, srSChet,;
        arr, strSum, schet, ordInc, pSchet, sum2


   SUMS ->( baseSavePos())
   summa2  := - SUMS ->summa
   schet2  := subStr( SUMS ->buffer, 2, 20)
   buffer2 := if( left( SUMS ->buffer, 1) == "B", "C", "B") + SUMS ->schet
   Date    := SUMS ->dDate
   buffer2 := padR( buffer2, 30)
   SUMS ->( dbSetOrder( 1))
   SUMS ->( dbSeek( schet2 + DtoS( date) + Str( summa2, 15)))
   while SUMS ->schet == schet2 .AND. SUMS ->dDate == date .AND. ;
         SUMS ->summa == summa2 .AND. SUMS ->( !Eof())
     exit if ( rV := SUMS ->typeOf == "6030".AND. SUMS ->buffer == buffer2)
     skip 1 alias SUMS
   enddo
   if rV

   //CARDNEW Проверять надо по типу счета, т.к. 42301 - это еще и карточный счет
//      pSchet := if( left( schet2, 5) == "42301", schet2, subStr( buffer2, 2, 20))
      pSchet := if( left( schet2, 5) == "42301" .OR. left( schet2, 5) == "40817" , schet2, subStr( buffer2, 2, 20)) //OVD 18.07.2005
      sum2   := sumRest( pSchet, setCurDate())
      if !( sum2 >= abs( summa2/ 100))
         errLog( "После удаления этой проводки остаток по счету " + pSchet +;
                " станет отрицательным !!!")
         SUMS ->( baseRestPos())
         return .F.
      endif
      if SUMS ->( mDelete())
         SUMS ->( baseRestPos())

         // удалять проводку нельзя !!!
         if left( schet2, 5) == "42301" .OR. left( schet2, 5) == "40817"   //OVD 18.07.2005
            schet2 := subStr( buffer2, 2, 20)
         endif
         if WISC ->( dbSeek( schet2 + "998 "))
            arr    := CtoA( WISC ->buffer, ";")
            strSum  := padL( round( val( arr[ 5]) * 100, 0), 15)
            schet   := confStrSchet()
            ordInc  := val( arr[ 4])
            SUMS ->( baseSavePos())
            SUMS ->( dbSetOrder( 1))
            if SUMS ->( dbSeek( schet + DtoS( Date) + strSum))
               strSum := val( strSum)
               while SUMS ->schet  == schet  .AND. SUMS ->summa == strSum  .AND. ;
                     opEqual( SUMS ->typeOf, "3006") .AND. SUMS ->dDate == Date    .AND. ;
                     SUMS ->( !eof())
                     if SUMS ->nDoc == ordInc
                        strnDeleteKassa()
                        if SUMS ->( mDelete())
                           SUMS ->( netUnLock())
                           if WISC ->( mDelete())
                              WISC ->( netUnLock())
                           else
                              messageB( "Не могу удалить пожелание ! Обратитесь на филиал !!!")
                           endif
                           rV := .T.
                           exit
                        endif
                     endif
                     skip 1 alias SUMS
               enddo
            endif
            SUMS ->( baseRestPos())

         endif
         // SUMS ->( baseRestPos())
         if rV
            if SUMS ->( rV := mDelete())
               // SUMS ->( netUnLock())
            else
              messageB( "Не удалось удалить проводку !")
            endif
         endif
      else
         SUMS ->( baseRestPos())
         messageB( "Не удалось удалить корреспондирующую проводку !")
      endif
   else
      messageB( "Не найдена корреспондирующая проводка !!!")
      SUMS ->( baseRestPos())
   endif
return rV

//
// перечисление по клиентским счетам
//
static funcDef del5200 local rV := .F., summa2, schet2, buffer2, Date, srSchet

   SUMS ->( baseSavePos())
   summa2  := - SUMS ->summa
   schet2  := subStr( SUMS ->buffer, 2, 20)
   buffer2 := if( left( SUMS ->buffer, 1) == "B", "C", "B") + SUMS ->schet
   if left( SUMS ->buffer, 1) == "B"
      srSchet := SUMS ->schet
   else
      srSchet := subStr( SUMS ->buffer, 2, 20)
   endif
   buffer2 := padR( buffer2, 30)
   Date    := SUMS ->dDate

   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 1))
   SUMS ->( dbSeek( schet2 + DtoS( date) + Str( summa2, 15)))
   while SUMS ->schet == schet2 .AND. SUMS ->dDate == date .AND. ;
         SUMS ->summa == summa2 .AND. SUMS ->( !Eof())
     exit if ( rV := SUMS ->typeOf == "5200" .AND. SUMS ->buffer == buffer2)
     skip 1 alias SUMS
   enddo

   if rV
      if rV := SUMS ->( mDelete())
         SUMS ->( netUnLock())
         SUMS ->( baseRestPos())
         if rV := SUMS ->( mDelete())
            SUMS ->( netUnLock())
            if !empty( srSchet)
               ACC ->( baseSavePos()); rV := .F.
               ACC ->( dbSetOrder( 1))
               if ACC ->( dbSeek( srSchet))
                  if rV := accRLock()
                     ACC ->datClose := CtoD( "")
                     ACC ->typeOf   := "00"
                     accUnLock()
                  endif
               endif
           endif
         else
            messageB( "Удалена корреспондирующая проводка, а текущая - НЕТ!")
         endif
      endif
   else
      messageB( "Не найдена корреспондирующая проводка!")
      SUMS ->( baseRestPos())
   endif

return rV

//
// возврат ссуды
//
static funcDef del2003 local rV := .F.,;
   strSum := SUMS->SUMMA,;
   sDoc := allTrim( Str(SUMS->NDOC) ) + ';' + allTrim(SUMS ->SCHET),;
   rNo := SUMS->(recNo()),;
   isCredLStrah := useLineNoStrah(), crdNum := 0, curDate, arr

   if confUseAgent()
      SUMS ->( baseSavePos())
      SUMS ->( dbSetOrder( 4))
      SUMS ->( dbSeek( Str( strSum, 15, 0), .T.))
      while SUMS ->Summa == strSum .AND. SUMS ->( !Eof())
         arr := CtoA( SUMS ->buffer, ";")
         if opEqual( SUMS ->typeOf, "6136") .AND.;
         if( isCredLStrah, arr[ 1]+";"+arr[ 2] == sDoc .And. Int( Val( arr[ 3])) == crdNum,;
         allTrim( SUMS ->buffer) == sDoc) .And. SUMS ->(recNo()) != rNo
         if ( rV := SUMS ->( mDelete()) ) then SUMS ->( netUnLock())
            exit
         endif
         skip 1 alias SUMS
      enddo
      SUMS ->( baseRestPos())
   else 
      rV:=.T.
   endif

   if isCredLStrah
      SUMP ->( baseSavePos())
      SUMP ->( dbSetOrder( 1))
      if SUMP ->( dbSeek( SUMS ->schet + DtoS( SUMS->dDate) + Str( -SUMS ->SUMMA, 15, 0)) )
          do while SUMP ->SCHET == SUMS ->SCHET .And. SUMP ->dDate == SUMS ->dDate;
             .And. SUMP ->SUMMA == -SUMS ->SUMMA .And. !SUMP ->( Eof())
               if opEqual( SUMP->TypeOf, if( confUseAgent(), "6136", "2003"))
               crdNum = SUMP ->CRD_NUM
               if SUMP ->( mDelete()) then SUMP ->( netUnLock())
                   exit
               endif
               SUMP ->( dbSkip())
          enddo
      else
          messageB( "Не найдено соответсвие в SUMP !!!")
      endif
      SUMP ->( baseRestPos())
   endif

   if rV
      if isCredLStrah
         if !crdWiscNoStrah( -Round( SUMS ->Summa/100, 2), SUMS ->Schet, crdNum)
             messageB( "Не обработана запись в файле пожеланий !!!")
         endif
      endif
      strnDeleteKassa()
      if ( rV := SUMS ->( mDelete())) then SUMS ->( netUnLock())
   endif
return rV
//
// возврат процентов
//
static funcDef del2004 local rV := .F., schet, arr,;
    sPens, mnSumma,;
    isCredLStrah := useLineNoStrah(), crdNum := 0, curDate,;
    prosr, schetp, isSum, arrR := {}, j, rest := 0.00, recNo, codeCl, ;
    sum47427, wiscBuff

    ACC ->( baseSavePos())
    ACC ->( dbSetOrder( 1))

    recNo := SUMS ->( recNo())

    if ACC ->( dbSeek( SUMS ->schet ))
       codeCl := ACC ->code
    else
       messageB( "Повреждена таблица ACCOUNT.DBF !!!")
       return .F.
    endif

    SUMS ->( dbGoTop())
    do while  SUMS ->( !Eof())
       if ACC ->( dbSeek( SUMS ->schet )) .AND. ACC ->code == codeCl
          if  Left( allTrim( SUMS ->Buffer), 4) <> "PCT:"
             aadd( arrR, { ACC ->schet, ACC ->code, ACC ->priz, SUMS ->typeof, SUMS ->summa, SUMS ->buffer})
          else 
             if _isSumPctKr( SUMS ->schet)
                sum47427 := Str( -SUMS ->summa, 15, 0)
             endif
          endif
       endif
       skip 1 alias SUMS
    enddo

    for j = 1 to len( arrR)
       if arrR[ j, 5] > 0
          messageB( "Нельзя удалить возврат процентов, так как есть ВОЗВРАТ ССУДЫ !!!")
          SUMS ->( baseRestPos())
          SUMS ->( dbGoTo( recNo))
          return .F.
       endif
    next

   SUMS ->( dbGoTo( recNo))
   isSum := _isSumPctKr( SUMS ->schet)

   schet := SUMS ->SCHET
   prosr := _isProsrPct( schet)
   sPens := subStr( SUMS ->buffer, 5, 20)
   mnSumma := SUMS->SUMMA
   curDate = SUMS->dDate

   if empty( schet)
      messageB( "Не указан счет !!!")
      return rV
   endif

   if isCredLStrah
      arr := CtoA( SUMS ->buffer, ";")
      crdNum = Int( Val( arr[2]) )      
   endif

  if !confUseAgent() .and. !prosr           // 05-02-2005 TAT
      schet := subStr( SUMS ->buffer, 5, 20)
  endif

  if WISC ->( dbSeek( schet + if( isCredLStrah, "968 ", "997 ")))
     while WISC ->schet == schet .AND. ;
         WISC ->type == if( isCredLStrah, "968 ", "997 ") .AND. WISC ->(!eof())
       arr := CtoA( WISC ->buffer, ";")
       if isCredLStrah .Or. val( arr[ 2]) == SUMS ->nDoc .AND. ;
          round( val( arr[ 3]), 2) == round( SUMS ->summa/ 100, 2)
          //
          // теперь поищем в SUMP !!!
          //
          // 05-02-2005 TAT при поиске в SUMP заменили schet на schetp
          schetp = schet
          if ( prosr)
              schetp = arr[ 4]
          endif
          if SUMP ->( dbSeek( schetp + DtoS( SUMS ->dDate) + Str( -SUMS ->summa, 15)))
              if isCredLStrah
                 do while SUMP ->SCHET == SCHETp .And. SUMP ->dDate == SUMS ->dDate;
                    .And. SUMP ->SUMMA == -SUMS ->SUMMA .And. !SUMP ->( Eof())
                     if opEqual( SUMP->TypeOf, "2004") .AND. SUMP ->CRD_NUM == crdNum
                         if SUMP ->( mDelete()) then SUMP ->( netUnLock())
                         exit
                     endif
                     SUMP ->( dbSkip())
                 enddo
              else
                 if SUMP ->( mDelete()) then SUMP ->( netUnLock())
              endif
          else
              if !isSum
                 messageB( "Не найдено соответсвие в SUMP !!!")
              else
                  if len( arr) > 0
                     wiscBuff := arr[ 4] + Dtos( ctod( arr[ 1])) + sum47427
                     if SUMP ->( dbSeek( wiscBuff))
                        if SUMP ->( mDelete()) then SUMP ->( netUnLock())
                    endif
                 endif
              endif
          endif

// Add <confUseAgent()> by TMV  08-01-2003 для работы в режиме Отделения
          if confUseAgent()
              SUMS ->( baseSavePos())
              SUMS ->( dbSetOrder( 1))
              SUMS ->( dbSeek( sPens, .T.))
              while SUMS ->SCHET == sPens .AND. SUMS ->( !Eof())
                  arr := CtoA( SUMS ->buffer, ";")
                  if opEqual( SUMS ->typeOf, if( confUseAgent(), "6137", "2004")) .AND.;
                      Left( SUMS ->buffer, 4) == 'PCT:' .And.;
                      SUMS->SUMMA == mnSumma ;
                      .And. ( !isCredLStrah .Or. Int( Val( arr[2]) ) == crdNum)  // Add by TMV  03-06-2003
                       if ( rV := SUMS ->( mDelete()) ) then SUMS ->( netUnLock())
                      exit
                  endif
                  skip 1 alias SUMS
              enddo
              SUMS ->( baseRestPos())
          endif
          strnDeleteKassa()
          if SUMS ->( mDelete())
             SUMS ->( netUnLock())
             if !isCredLStrah
                if WISC ->( mDelete())
                    WISC ->( netUnLock())
                else
                    messageB( "Не могу удалить пожелание ! Обратитесь на филиал !!!")
                endif
             else
                crdWiscNoStrah( -Round( mnSumma/100, 2), Schet, crdNum)
             endif
             rV := .T.
             exit
          endif
       endif
       skip 1 alias WISC
     enddo
  endif
return rV
//
static funcDef del9100 local rV := .T., arr, sum, sSchet, curDate

    arr := CtoA( SUMS ->buffer, ";")
    if len( arr) <> 2 then return .T.

    sum := getStrToInt( arr[ 2])

    if sum <= 0 then return .T.
    sSchet   := SUMS ->schet
    curDate := SUMS ->dDate
    SUMS ->( baseSavePos())

    if SUMS ->( dbSeek( sSchet + DtoS( curDate) + Str( sum, 15)))
       rV := .F.
       while SUMS ->schet == sSchet .AND. SUMS ->dDate == curDate .AND. ;
             SUMS ->summa == sum

          if SUMS ->typeOf == "9100"
             if ( rV := SUMS ->( mDelete()))
                SUMS ->( netUnLock())
             endif
          endif
          skip 1 alias SUMS
       enddo
    endif

    SUMS ->( baseRestPos())

return rV


// Add by TMV  09-12-2002
// выдача ссуды безналом
//
static funcDef del1060 ;
   local rV := .F., arr, strSum, curDate, schet, ordInc, docNumb,;
		rNo := SUMS->(recNo()),;  // Add by TMV  15-11-2002
		sTmp:="",;                // Add by TMV  09-12-2002
	   isCredLStrah := useLineNoStrah(), crdNum := 0 // Add by TMV  27-05-2003

   docNumb := rTrim( SUMS ->buffer)
   schet   := confStrSchet()

	strSum = SUMS->Summa           // Add by TMV  15-11-2002
	curDate = SUMS ->dDate     // Add by TMV  27-05-2003

   SUMS ->( baseSavePos())

   SUMS ->( dbSetOrder( 4))
   SUMS ->( dbSeek( Str( strSum, 15, 0), .T.))
   while SUMS ->Summa == strSum .AND. SUMS ->( !Eof())
     if opEqual( SUMS ->typeOf, "6031") .AND.;
	  		rTrim( SUMS ->buffer) == docNumb .And. SUMS->(recNo()) != rNo
		 sTmp = SUMS->Schet         // Add by TMV  09-12-2002
       if SUMS ->( mDelete()) then SUMS ->( netUnLock())
       exit
     endif
     skip 1 alias SUMS
   enddo

// Add by TMV  27-05-2003
	if isCredLStrah
   	SUMP ->( baseSavePos())
	   SUMP ->( dbSetOrder( 1))
		if SUMP ->( dbSeek(sTmp + DtoS(curDate) + Str( -strSum, 15, 0)) )
			do while SUMP->SCHET == sTmp .And. SUMP->dDate == curDate;
					.And. SUMP->SUMMA == -strSum .And. !SUMP->(Eof())
				if opEqual(SUMP->TypeOf, "1060")
					crdNum = SUMP ->CRD_NUM
					if SUMP ->( mDelete()) then SUMP ->( netUnLock())
					exit
				endif
				SUMP->(dbSkip())
			enddo
		endif
   	SUMP ->( baseRestPos())
	endif

   if WISC ->( dbSeek( sTmp + if(isCredLStrah, "967 ", "998 ")))
// Add by TMV  27-05-2003
		if isCredLStrah
			do while WISC ->SCHET == sTmp .And. ;
					WISC ->TYPE == if(isCredLStrah, "967 ", "998 ");
					.And. !WISC ->(Eof())
				arr := CtoA( WISC ->buffer, ";")  // Add by TMV  03-06-2003
				if Int( Val( arr[7]) ) == crdNum
					if WISC ->( mDelete()) then WISC ->( netUnLock())
					exit
				endif
				WISC ->(dbSkip())
			enddo

		else              // Add by TMV  27-05-2003
			if WISC ->( mDelete()) then WISC ->( netUnLock())
		endif             // Add by TMV  27-05-2003
   endif
   SUMS ->( dbSetOrder( 1))
   SUMS ->( dbSeek( Schet, .T.))
   while SUMS ->schet == Schet .AND. SUMS ->( !Eof())
     if opEqual( SUMS ->typeOf, "3006") .AND. rTrim( SUMS ->buffer) == docNumb
       strnDeleteKassa()
       if SUMS ->( mDelete()) then SUMS ->( netUnLock())
       exit
     endif
     skip 1 alias SUMS
   enddo

   SUMS ->( baseRestPos())
   strnDeleteKassa()
   if SUMS ->( mDelete()) then SUMS ->( netUnLock())
     
return .T.

//
// выдача ссуды
//
static funcDef del1007 ;
   local rV := .F., arr, strSum, curDate, schet, ordInc, docNumb,;
		rNo := SUMS->(recNo()),;  // Add by TMV  15-11-2002
	   isCredLStrah := useLineNoStrah(), crdNum := 0  // Add by TMV  27-05-2003
		
   docNumb := rTrim( SUMS ->buffer)
   schet   := confStrSchet()

	strSum = SUMS->Summa           // Add by TMV  15-11-2002
	curDate = SUMS ->dDate     // Add by TMV  27-05-2003

// Add by TMV  27-05-2003
	if isCredLStrah
		arr := CtoA( SUMS ->buffer, ";")  // Add by TMV  06-06-2003
		crdNum = Int( Val(arr[2]) )       // Add by TMV  06-06-2003
	   docNumb := arr[1]                 // Add by TMV  06-06-2003
   	SUMP ->( baseSavePos())
	   SUMP ->( dbSetOrder( 1))
		if SUMP ->( dbSeek(SUMS ->schet + DtoS(curDate) + Str( -strSum, 15, 0)) )
			do while SUMP->SCHET == SUMS->SCHET .And. SUMP->dDate == SUMS->dDate;
					.And. SUMP->SUMMA == -SUMS->SUMMA .And. !SUMP->(Eof())
				if opEqual(SUMP->TypeOf, "1007") .And. SUMP ->CRD_NUM == crdNum
					if SUMP ->( mDelete()) then SUMP ->( netUnLock())
					exit
				endif
				SUMP->(dbSkip())
			enddo
		else
			messageB( "Не найдено соответсвие в SUMP !!!")
		endif
   	SUMP ->( baseRestPos())
	endif

// Add by TMV  27-05-2003
	if isCredLStrah
	   if WISC ->( dbSeek( SUMS ->schet + "967 "))
			do while WISC ->SCHET == SUMS ->schet .And. WISC ->TYPE == "967 ";
					.And. !WISC ->(Eof())
				arr := CtoA( WISC ->buffer, ";")  // Add by TMV  03-06-2003
				if Int( Val( arr[7] )) == crdNum
					if WISC ->( mDelete()) then WISC ->( netUnLock())
					exit
				endif
				WISC ->(dbSkip())
			enddo
		endif

	else              // Add by TMV  27-05-2003
	   if WISC ->( dbSeek( SUMS ->schet + "998 "))
			if WISC ->( mDelete()) then WISC ->( netUnLock())
		endif             // Add by TMV  27-05-2003
	endif                // Add by TMV  27-05-2003
   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 1))
   SUMS ->( dbSeek( Schet, .T.))
   while SUMS ->schet == Schet .AND. SUMS ->( !Eof())
     if opEqual( SUMS ->typeOf, "3006") .AND. rTrim( SUMS ->buffer) == docNumb
       strnDeleteKassa()
       if SUMS ->( mDelete()) then SUMS ->( netUnLock())
       exit
     endif
     skip 1 alias SUMS
   enddo

   SUMS ->( baseRestPos())

   strnDeleteKassa()
   if SUMS ->( mDelete()) then SUMS ->( netUnLock())
     
return .T.


* NEW OBJECT FILE -------- проверка проводок на соответсвие пожеланиям -----
funcDef sumsCheck ;
   local rV := .T., i, str := ""
   if monOpen()
      operStartUp( "Проверка проводок и поручений...", i := 1, SUMS ->( lastRec()))
      SUMS ->( baseSavePos())
      SUMS ->( netGoTop())
      while SUMS ->( !Eof())
        do case
           case SUMS ->typeOf $ GSZ_BUY_NAL + "," + GSZ_BUY_BEZNAL // ГСЗ
                rV := checkGsz( @str) .AND. rV
           case SUMS ->typeOf $ "2012,5011"
                rV := checkPZabota( @str) .AND. rV
           case left( SUMS ->buffer, 1) == "A"
                rV := checkOncePay( @str) .AND. rV
           case SUMS ->typeOf == "5200" .AND. left( SUMS ->buffer, 1) $ "B,C"
                rV := check5200( @str) .AND. rV
           case opArrEqual( SUMS ->typeOf, "5010,5201" ) .AND. left( SUMS ->buffer, 1) $ "B,C"
                rV := checkTransfers( @str, "5010,5201" ) .AND. rV
           case SUMS ->typeOf $ "2011,5010" .AND. ;
                len( rTrim(SUMS ->buffer)) = INTDOC_LEN .AND.;
                left( SUMS ->buffer, 2) == "06"
                rV := checkZabota( @str) .AND. rV
           case opEqual( SUMS ->typeOf, "1007")  .AND. SUMS ->summa < 0
                if ACC->(dbSeek( SUMS->Schet )) .And. !isPens( ACC->PRIZ )
                   rV := check1007( @str) .AND. rV
                endif
           case opEqual( SUMS ->typeOf, "2004") .AND. left( SUMS ->buffer, 4) == "PCT:"
                rV := check2004( @str) .AND. rV
           case opEqual( SUMS ->typeOf, "2060") .AND. left( SUMS ->buffer, 4) == "PCT:" ;
                .AND. !_isSumPctKr( SUMS ->schet)
                rV := check2004( @str) .AND. rV
        endcase

        skip 1 alias SUMS
        operProc( i++)
      enddo
      SUMS ->( baseRestPos())
      operExit()
      if !rV
         str := "Ошибки в базе проводок " + endl() + replicate("-",75) + endl() + str
         editMemo( str, .F.)
         if yesNo( "Распечатать протокол ?") then printAOrder( {str})
         if Upper( SetUpValue( "STOP")) == "YES"
            errLog( "Невозможно перейти к модему !")
         else
            rV := noYes( "Вы хотите перейти к модему ?")
            if rV then errLog( "Переход к модему при ошибках в SUMS", .F.)
         endif
      endif
      monClose()
   endif
return rV


static funcDef checkGSZ with str:C ;
   local rV := .F., nDoc, intDoc, arr, i, cnt, code, arr2, buf2, s1 := ""

   intDoc := padR( left( SUMS ->buffer, INTDOC_LEN), 20)
   nDoc   := SUMS ->( recNo())
   buf2   := subStr( SUMS ->buffer, INTDOC_LEN + 2, 12)
   s1 := "При покупке ГСЗ по счету " + SUMS ->schet + endl() + " сумма " + ;
         lTrim( mStr( SUMS ->summa/ 100)) + " возникла ошибка :" + endl()

   if wiscSeek( intDoc, GSZ_WISC)
      intDoc := WISC ->schet
      while WISC ->schet == intDoc .AND. WISC ->type = GSZ_WISC .AND. ;
            WISC ->( !eof())
        exit if val(WISC ->ID) == nDoc
        skip 1 alias WISC
      enddo
      if val(WISC ->ID) == nDoc
         CONTRACT ->( baseSavePos())
         CONTRACT ->( dbSetOrder( 3))
         if CONTRACT ->( dbSeek( left( intDoc, INTDOC_LEN), .F.))
            rV := .T.
         else
            s1 += "    не найдена запись в CONTRACT"
         endif
         CONTRACT ->( baseRestPos())
      else
        s1 += "    не найдено пожелание с номером документа " + nStr( nDoc)
      endif
   else
     s1 += "    не найдено пожелание..."
   endif
   if !rV
     s1  += endl() + "        Номер договора      : " + intDoc + endl() + ;
                     "        Номер записи в SUMS : " + Str( SUMS ->( recNo()))
     str += endl() + s1
     errLog( s1, .F.)
   endif
return rV

* ------------ продуктовая забота ----------------------------
static funcDef checkPZabota with str:C local rV := .F., s1 := ""
   if WISC ->( dbSeek( SUMS ->schet + "981 "))
      while WISC ->schet == SUMS ->schet .AND. WISC ->type == "981 "
        if left( WISC ->buffer, 14) == left( SUMS ->buffer, 14)
           rV := .T.; exit
        endif
        skip 1 alias WISC
      enddo
   endif
   if !rV
   s1 := "При проверке продуктовой заботы по счету " + SUMS ->schet  + endl() + ;
         "    сумма " + lTrim( mStr( SUMS ->summa/ 100))  + " возникла ошибка :" + endl() + ;
         "    не найдено пожелание к договору " + left(SUMS->buffer,14)
      str += endl() + s1
      errLog( s1, .F.)
   endif
return rV

* ------------- разовый платеж ----------------------------
static funcDef checkOncePay with str:C local s1, rV := .T.
   if WISC ->( !dbSeek( left( SUMS ->buffer, 20) + "977 "))
      s1 := "При проверке разовых платежей по счету " + SUMS ->schet + endl() + ;
            "    сумма " + lTrim( mStr( SUMS ->summa/ 100))+ " возникла ошибка :" + endl() + ;
            "    не найдено пожелание"
      str += endl() + s1
      errLog( s1, .F.)
      rV := .F.
   endif
return rV

* --------------- перечисление со счета на счет -----------------------
static funcDef check5200 with str:C ;
   local rV := .F., summa2, schet2, buffer2, Date, srSchet, s1

   s1 := "При проверке перечисления по счету " + SUMS ->schet        + ;
         " сумма " + mStr( SUMS ->summa/ 100) +  endl() +"     возникла ошибка :" + endl()
   SUMS ->( baseSavePos())
   summa2  := - SUMS ->summa
   schet2  := subStr( SUMS ->buffer, 2, 20)
   buffer2 := if( left( SUMS ->buffer, 1) == "B", "C", "B") + SUMS ->schet
   if left( SUMS ->buffer, 1) == "B"
      srSchet := SUMS ->schet
   else
      srSchet := subStr( SUMS ->buffer, 2, 20)
   endif
   buffer2 := padR( buffer2, 30)
   Date    := SUMS ->dDate

   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 1))
   SUMS ->( dbSeek( schet2 + DtoS( date) + Str( summa2, 15)))
   while SUMS ->schet == schet2 .AND. SUMS ->dDate == date .AND. ;
         SUMS ->summa == summa2 .AND. SUMS ->( !Eof())
     exit if ( rV := SUMS ->typeOf == "5200".AND. SUMS ->buffer == buffer2)
     skip 1 alias SUMS
   enddo

   SUMS ->( baseRestPos())
   if !rV
      s1 := "При проверке перечисления по счету " + SUMS ->schet + endl() + ;
            "    сумма " + lTrim( mStr( SUMS ->summa/ 100)) + " возникла ошибка :" + endl() + ;
            "    не найдена корреспондирующая проводка !!!"
      errLog( s1, .F.)
      str += endl() + s1
   endif
return rV

static funcDef checkTransfers with str:C, opCodes:C ;
   local rV := .F., summa2, schet2, buffer2, Date, s1

   SUMS ->( baseSavePos())
   summa2  := - SUMS ->summa
   schet2  := subStr( SUMS ->buffer, 2, 20)
   buffer2 := if( left( SUMS ->buffer, 1) == "B", "C", "B") + SUMS ->schet
   buffer2 := padR( buffer2, 30)
   Date    := SUMS ->dDate
   SUMS ->( dbSetOrder( 1))
   SUMS ->( dbSeek( schet2 + DtoS( date) + Str( summa2, 15)))
   while SUMS ->schet == schet2 .AND. SUMS ->dDate == date .AND. ;
         SUMS ->summa == summa2 .AND. SUMS ->( !Eof())
     exit if ( rV := opArrEqual( SUMS ->typeOf, opCodes ) .AND. SUMS ->buffer == buffer2)
     skip 1 alias SUMS
   enddo
   SUMS ->( baseRestPos())
   if !rV
      s1 := "При проверке перечисления по счету " + SUMS ->schet + endl() + ;
            "    сумма " + lTrim( mStr( SUMS ->summa/ 100)) + " возникла ошибка :" + endl() + ;
            "    не найдена корреспондирующая проводка!"
      errLog( s1, .F.)
      str += endl() + s1
   endif
return rV

static funcDef check1007 with str:C ;
   local rV := .F., arr, strSum, curDate, schet, ordInc, s1,;
		rV1 := .F.,;  // Add by TMV  15-11-2002
	   isCredLStrah := useLineAllNoStrah(), crdNum := 0 // Add by TMV  02-06-2003

   s1 := "При проверке выдачи кредита по счету " + SUMS ->schet + endl() + ;
         "    сумма " + lTrim( mStr( SUMS ->summa/ 100)) + " возникла ошибка :" + endl()

   if WISC ->( dbSeek( SUMS ->schet + if(isCredLStrah, "967 ", "998 ")))
      arr     := CtoA( WISC ->buffer, ";")
      strSum  := padL( round( val( arr[ if(isCredLStrah, 2, 5)]) * 100, 0), 15)
      curDate := SUMS ->dDate
      schet   := confStrSchet()
      ordInc  := val( arr[ 4])
      SUMS ->( baseSavePos())
      SUMS ->( dbSetOrder( 1))
   // Edited by TMV  02-06-2003 - add <!isCredLStrah>
      if !isCredLStrah
   		if SUMS ->( dbSeek( schet + DtoS( curDate) + strSum))
            strSum := val( strSum)
            while SUMS ->schet  == schet  .AND. SUMS ->summa == strSum  .AND. ;
                  opEqual( SUMS ->typeOf, "3006") .AND. SUMS ->dDate == curDate .AND. ;
                  SUMS ->( !eof())
                  if SUMS ->nDoc == ordInc
                     rV := .T.
                     exit
                  endif
              skip 1 alias SUMS
            enddo
         endif
   	else
   		rV = .T.
   	endif
      if !rV then s1 += "    не найдена проводка по страховому взносу !"
   // Add by TMV  15-11-2002
   	if confUseAgent()  // Add <confUseAgent()> by TMV 08-01-2003
      	schet = allTrim( arr[if(isCredLStrah, 1, 6)] )
      	ordInc = arr[if(isCredLStrah, 4, 1)]
   	  	SUMS ->( dbSeek( schet + DtoS( curDate), .T.))
         while SUMS ->schet  == schet  .AND. ;
               SUMS ->dDate == curDate .AND. ;
               SUMS ->( !eof())
            if opEqual( SUMS ->typeOf, "1060") .AND. ; // Replace <1001 to 1060 (7032)>
      				allTrim(SUMS ->buffer) == ordInc     // by TMV 28-11-2002
               rV := .T. .And. rV
      			rV1 = .T.
               exit
            endif
         	skip 1 alias SUMS
      	enddo
         if !rV1 then s1 += " не найдена вторая проводка по выдаче кредита !"
   	endif  // Add <confUseAgent()> by TMV 08-01-2003

      SUMS ->( baseRestPos())
   else
      s1 += "    не найдено пожелание."
   endif
   if !rV
      errLog( s1, .F.)
      str += endl() + s1
   endif

return rV

static funcDef check2004 with str:C ;
  local rV := .F., schet, arr, s1,;
	   isCredLStrah := useLineAllNoStrah(), crdNum := 0,; // Add by TMV  02-06-2003
           prosr, schetp  // 05-02-2005 TAT

   s1 := "При проверке возврата % по счету " + SUMS ->schet + endl() + ;
         "    сумма " + lTrim( mStr( SUMS ->summa/ 100)) + " возникла ошибка :" + endl()

   prosr := _isProsrPct( SUMS->SCHET) // 05-02-2005 TAT 
   if !confUseAgent();         // Add by TMV  08-01-2003
        .and. !prosr           // 05-02-2005 TAT
   	schet := subStr( SUMS ->buffer, 5, 20)
   else
   	schet := SUMS->Schet
   endif

   if isCredLStrah
   	arr := CtoA( SUMS ->buffer, ";")
   	if Len( arr ) > 1  // Add by TMV 14-10-2003
   		crdNum = Int( Val( arr[2] ))
   	endif
   endif

   if empty( schet)
      s1 += "Не указан счет !"
   else
     if isCredLStrah;  // Add by TMV  03-06-2003 Для нестраховой схемы
     		.And. Len( arr ) > 1  // Add by TMV 14-10-2003
     		rV = crdWiscNoStrah(round( SUMS ->summa/ 100, 2), Schet, crdNum, .T.)

     elseif WISC ->( dbSeek( schet + if(isCredLStrah .And. Len( arr ) > 1, "968 ", "997 ")))
        while WISC ->schet == schet .AND. ;
   	  		WISC ->type == if(isCredLStrah .And. Len( arr ) > 1, "968 ", "997 ") .AND. WISC ->(!eof())

          arr := CtoA( WISC ->buffer, ";")
          if val( arr[ 2]) == SUMS ->nDoc .AND. ;
             round( val( arr[ 3]), 2) == round( SUMS ->summa/ 100, 2)
             //
             // теперь поищем в SUMP !!!
             //
             // 05-02-2005 TAT при поиске в SUMP заменили schet на schetp
             schetp = schet
             if (prosr)
   	          schetp = arr[ 4]
             endif
             if SUMP ->( !dbSeek( schetp + DtoS(SUMS ->dDate) + Str(-SUMS ->summa,15)))
                s1 += "    не найдено соответсвие в SUMP !"
             else
                rV := .T.
                exit
             endif
          endif
          skip 1 alias WISC
        enddo
     endif
     if !rV
        s1 += "    не найдено пожелание"
     endif
   endif
   if !rV
      errLog( s1, .F.)
      str += endl() + s1
   endif
return rV


static funcDef checkZabota with str:C local docNumb, s1, rV := .F.

   s1 := "При проверке заботы по счету " + SUMS ->schet + endl() + ;
         "    сумма " + lTrim( mStr( SUMS ->summa/ 100)) + " возникла ошибка :" + endl()
   docNumb := rTrim( SUMS ->buffer)
   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 2))
   if CONTRACT ->( !dbSeek( docNumb))
      s1 += "    не найдена запись в базе долгосрочных поручений !"
   else
     if !wiscSeek( CONTRACT ->debetAcc, "988 ") .AND. ;
        !wiscSeek( CONTRACT ->debetAcc, "989 ")
        s1 += "    не найдено пожелание !"
     else
       while WISC ->schet = CONTRACT ->debetAcc .AND. WISC ->( !eof())
         if left( WISC ->buffer, INTDOC_LEN) = rTrim( SUMS ->buffer)
            rV := .T.
         endif
         skip 1 alias WISC
       enddo
       if !rV then s1 += "    не найдено пожелание !"
     endif
   endif
   CONTRACT ->( baseRestPos())
   if !rV
      errLog( s1, .F.)
      str += endl() + s1
   endif
return rV

//
// удаление разового платежа
//
funcDef delOnePay local opened := Select( "TICKETS") >0, rT:=.F., s
  s = select()

  if !opened then rT := ticOpen()       //Возвращаем старый номер пр.карточки
  if opened .OR. rT
    rT = .F.
    TICKETS->(dbsetorder(1))
    if ticSeek( SUMS ->buffer )
      CLIENTS->(dbsetorder(1))
      if cliSeek( TICKETS ->code )
        if cliRLock()
          CLIENTS->colibri := TICKETS ->colibri
          cliUnLock()
        endif
        ticOpen()
        if ticRLock()
          TICKETS->code    := space(12)
          TICKETS->buffer  := space(30)
          TICKETS->colibri := space(10)
          ticUnLock()
        endif
      endif
    endif
    if !opened then ticClose()
  endif

  select (s)
  if WISC ->( dbSeek( left( SUMS ->buffer, 20) + "977 "))
    if WISC ->( mDelete()) then WISC ->( netUnLock())
  endif
  strnDeleteKassa()
  if SUMS ->( mDelete()) then SUMS ->( netUnLock())

return .T.

funcDef getStrToInt with sValue:C local rV := 0, i:= 0
   sValue = allTrim( sValue )
   do while Left( sValue, 1) == '0'
   	sValue = SubStr( sValue, 2)
   enddo
   if (i:=at('.', sValue) ) > 0
   	sValue = Left(sValue, i - 1) + SubStr( sValue, i + 1)
   endif
   rV = val( sValue )
return rV