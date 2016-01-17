*==============================================================================
*                    Программа : mon.exe
*                    Файл      : pc_pos.prg
*                    Автор     : Цейтлин П.М.
*
*                    Назначение: Работа с PC-POS
*
*------------------------------------------------------------------------------
*
* 23-08-2002  TMV
*  В функции редактирования записей таблици PROT введена блокировка этих записей
*
* 16-01-2003  TMV
* Заменено Петробанк на МДМ. Ф-ии crOpenCheque(), crCloseCheque(),
*  crAutoSuccess(), crAutoFail()
*
* 28-10-2003  TMV
* Сделаны доработки для перехода на систему OpenWay
* Новые ф-ии crReasonCheck(), crReasonView(), getReasonFali() и crRest().
* Исправлены ф-ии crView(), crGiveMoney(), crOpenDay(), crCloseDay(),
*  crRunAuto(), crLinkTest(), crReverse(), crOpenCheque(), crCloseCheque(),
*  close2Cheque(), close1Cheque(), crAutoSuccess(), crAutoFail(),
*  crDateTime(), crIsGiveMoneyLast(), checkError(), crChkLast(),
*  isPackSended(), showOper(), getResponse(), crProtOutPos(), crTryCloseDay().
*
* 02-12-2003  TMV
* Пропущенная поправка к доработке по OpenWay.
* Ф-ия crLinkTest().
*
* 19-12-2003  TMV
* Добавлено "закрытие" транзакции при операции по карточке.
* Ф-ия crRunAuto()
* Включена для доп. видав чека подпись клиента.
* Ф-ия crAutoSuccess()
*
* 24-12-2003  TMV
* Поправлена процедура авторизации в части обработки ошибок
*  при авторизации.
* Ф-ия crRunAuto()
*
* 30-12-2003  TMV
* Исправления по исправленному ТЗ для работы с OpenWay
* Ф-ии crAutoCheque(), crAutoSuccess(), crLocTag(), crSchemeTag()
* Новая ф-ия crDateTShort()
*
* 05-01-2004  TMV
* Доработки по рез-там тестирования PC_POS.
* Исправлена печать чека на отказ от операции (в две колонки).
* В формат даты при записи в реестр операций добавлено значение года
*  (если оно не передано PC_POS)
* Ф-ии crReverse(), crDateTime(), checkError().
*
* 14-01-2004  TMV
* Исправлена ошибка в типе параметра ф-ии crDateTime()
*
* 16-01-2004  TMV
* Исправлена ф-ия toBalance() - из суммы убираются ведущие нули.
*
* 19-01-2004  TMV
* Исправлена печать чеков Открытия и Закрытия дня (ф-ии crOpenCheque() и crCloseCheque())
* и определение схемы обслуживания (ф-ия crSchemeTag()).
*
* 28-01-2004  TMV
* При записи протокола операций удаляются ведущие пробелы из номера карточки
* Ф-ия checkError()
*
* 09-02-2004  TMV
* Исправлены формы чеков на взнос, снятие наличных, открытие и закрытие дня.
* Ф-ии crOpenCheque(), crCloseCheque(), crAutoSuccess(), crAutoFail().
*
* 02-07-2007  TMV
* Исправлена ошибка в вызове ф-ии для получения пути к  файлу out.pos
* Ф-ия crMakeInPos()
*

#include "clippExt.ch"
#include "common.ch"
#include "inkey.ch"

#define POS_DSK "POS_DSK"
#define OPEN_DAY_MODE    0
#define CLOSE_DAY_MODE   1
#define AUTO_MODE        2
#define LINK_TEST_MODE   4
#define REVERSE_MODE     7
// Change version by TMV 27-10-2003
//#define PROG_VERSION "PB1.02 18.08.98"
#define PROG_VERSION "PB1.03 27.10.03"


funcDef crView
   popUp( 5, 10, {{ "Открытие дня",        .T., {|| crOpenDay()   }},;
                   { "Выдача наличных",     .T., {|| crGetMoney()  }},;
                   { "Внесение наличных",   .T., {|| crGiveMoney() }},;
                   { "Отмена операции",     .T., {|| crReverse()   }},;
                   { "Проверка связи",      .T., {|| crLink1Test() }},;
                   { "Запрос остатка",      .T., {|| crRest()      }},; // Add by TMV 23-10-2003
                   { "-"}                                          ,;
                   { "Статистика работы  ", .F., {|| crShowStat()  }},;
                   { "Удаление протоколов", .F., {|| crDeleteCR()  }},;
                   { "Протоколы работы...", .T., {|| crCardProt()  }},;
                   { "Закрытие дня",        .T., {|| crCloseDay()  }},;
                   { "-"}                                          ,;
                   { "Печать последнего документа...", .T.,;
                    {{ "Чек открытия дня", .T., {|| crPrintLastDoc( OPEN_DAY_MODE )}} ,;
                     { "Чек авторизации ", .T., {|| crPrintLastDoc( AUTO_MODE     )}} ,;
                     { "Чек закрытия дня", .T., {|| crPrintLastDoc( CLOSE_DAY_MODE)}}} ;
                   },;
                   { "-"}                                          ,;
                   { "Настройка", .T., {|| crOptions()}},;
                   { "Циклические операции...", .T.,;
                       {{ "Закрытие дня",              .T., {|| crTryCircleCloseDay()}},;
                        { "Проверка связи",            .T., {|| crCircleLinkTest()}}} ;
                   };
          })
return NIL

funcDef crGetMoney local summa := 0.00
  if getUp( {{ "Выдача наличных", block( summa),,,{|g| g:varGet > 0 .AND. crGetDelay()} }})
    crRunAuto( , "01", summa)
  endif
return NIL

funcDef crGiveMoney local summa := 0.00
  if getUp( {{ "Внесение наличных", block( summa),,,{|g| g:varGet > 0 .AND. crGetDelay()} }})
// Replaced by TMV  10-10-2003  - код операции внесения с 20 на 21
*    if crRunAuto( , "20", summa)
    if crRunAuto( , "21", summa)
    endif
  endif
return NIL

funcDef crGetDelay
  inkey( 1)
  clear typeAhead
return .T.


* ------------------ Операции ------------------------------------

//
// открытие дня
//
funcDef crOpenDay with prevOK:LU, mustWait:LU ;
  local rV := .F., inArr, outArr, str
  DEFAULT prevOK TO .T.
  DEFAULT mustWait TO getMustWait()

// Replaced by TMV  24-10-2003
//  inArr := { 8, 2, 4, 12, 12, 14, 1, 22, 13, 2, 24, 24, 24, 1, 6}
  inArr := { 8, 2, 4, 6, 6, 6, 6, 14, 1, 22, 13, 2, 24, 24, 24, 1, 6}
// Replaced by TMV  24-10-2003
//  str := "0" + if( mustWait, "0", "1") + "002" + if( prevOK, "0", "1") + "00000000000000"
  str := "0" + if( mustWait, "0", "1") + "003" + if( prevOK, "0", "1") + "00000000000000"
  if crMakeInPos( str)
     crRunPcPos()
     if crReadOutPos( OPEN_DAY_MODE, inArr, @outArr)
       if isPackSended( outArr, OPEN_DAY_MODE) then clsAuto()
       if checkError( OPEN_DAY_MODE, outArr)
         str := crOpenCheque( outArr)
         crSetChequeNumb()
         crSaveLastDoc( str, OPEN_DAY_MODE)
         printAOrder( { str})
         rV := .T.
       endif
     endif
  endif

return rV


//
// собрать статистику
//
funcDef crShowStats local str
  str := close1Cheque() + endl() + endl() + endl() + close2Cheque() + endl()
  editMemo( str)
  if yesNo( "Распечатать ?") then printAorder( {str})
return NIL

//
// закрытие дня
//
funcDef crCloseDay with prevok:LU, mustWait:LU ;
   local rV := .F., str, inArr, outArr, cCode := space( 6), codeAuto,;
         time, pan, summa, autoOpen
  DEFAULT prevOK TO getLastAuto() //.T.
  DEFAULT mustWait TO getMustWait()

// Replaced by TMV  24-10-2003
//  inArr := { 8, 2, 4, 12, 12, 14, 1, 22, 13, 2, 24, 24, 24, 1}
  inArr := { 8, 2, 4, 6, 6, 6, 6, 14, 1, 22, 13, 2, 24, 24, 24, 1, 6}
// Replaced by TMV  24-10-2003
//  str := "1" + if( mustWait, "0", "1") + "002" + if( prevOK, "0", "1") + "00000000000000"
  str := "1" + if( mustWait, "0", "1") + "003" + if( prevOK, "0", "1") + "00000000000000"

  if crMakeInPos( str)
     crRunPcPos()
     if crReadOutPos( CLOSE_DAY_MODE, inArr, @outArr)
       if isPackSended( outArr, CLOSE_DAY_MODE) then clsAuto()
       if !checkError( CLOSE_DAY_MODE, outArr)
// Replaced by TMV 21-10-2003
         errOk( { "ОШИБКА ЗАКРЫТИЯ ДНЯ",;
                 "ДЕНЬ ЗАКРЫТЬ НЕ УДАЛОСЬ."} )
			return .F.
/*
         if yesNo( { "ОШИБКА ЗАКРЫТИЯ ДНЯ",;
                     "ДЕНЬ ЗАКРЫТЬ НЕ УДАЛОСЬ.",;
                     "ПРОВЕСТИ АВАРИЙНОЕ ЗАКРЫТИЕ ?"})
            crAddInfo( @autoOpen, @codeAuto, @time, @pan, @summa)
            while .T.
              if getUp( 5, 5, {{ "Аварийное закрытие дня"},;
                    {'1. Позвоните в Центр Авторизации "Петрокарт" по телефону: 326-89-32'},;
                    {"2. Сверьте данные последней операции с оператором Центра Авторизации:"},;
                    {"   Код авторизации " + codeAuto               },;
                    {"   Сумма операции  " + Str( summa, 15, 2)},;
                    {"   Номер карты     " + pan                    },;
                    {"   Время операции  " + time                   },;
                    {"" },;
                    {"3. Введите код, который Вам сообщил оператор:", block( cCode), "999999"},;
                    {""},;
                    {""},;
                    {""}})
                 exit if cCode == autoOpen
                 if dialog( { "Введен неверный код.",;
                              "Позвоните в Центр Авторизации и уточните код."},;
                            { "ВЫПОЛНИТЬ"}) < 1
                    return .F.
                 endi
              else
                return .F.
              endif
            enddo
              //
              // восстановим выходной массив в соответствие с запомненной
              // структурой
              //
              outArr[ 3] := crGetDayN()
              outArr[ 4] := right( DtoS( date()), 6) + strTran( time(), ":", "")
              outArr[ 5] := tpRead( "time_open", "C")
              outArr[ 6] := tpRead( "term_loc",  "C")
              outArr[ 7] := tpRead( "term_loc_tag", "C")
              outArr[ 8] := tpRead( "merch_ident",  "C")
              outArr[ 9] := tpRead( "city",         "C")
              outArr[10] := tpRead( "country",      "C")
              outArr[11] := tpRead( "addr",         "C")
              outArr[12] := tpRead( "addr_ext_ru",  "C")
              outArr[13] := tpRead( "addr_ext_lat", "C")
         else
           return .F.
         endif
*/
// Replaced by TMV 21-10-2003  -- End
       endif
       str := crCloseCheque( outArr) + endl() + close1Cheque() + ;
              endl() + endl() + ;
              crCloseCheque( outArr) + endl() + close2Cheque() + ;
              endl() + endl()
       crSaveLastDoc( str, CLOSE_DAY_MODE)

       printAOrder( {str})
       rV := .T.
       //endif
     endif
  endif
  //
  // удаление протоколов работы
  //
  crDeleteCR()
return rV

//
// Авторизация
//
funcDef crRunAuto with prevOK:LU, procCode:C, summa:N, track2:CU, mustWait:LU ;
  local rV := .F., inArr, outArr, str, rVV := .F., i
  DEFAULT prevOK TO getLastAuto() //.T.
  DEFAULT track2 TO replicate( "0", 40)
  DEFAULT mustWait TO getMustWait()

  if len( track2) < 40
    track2 := padL( nStr( len( track2)), 3, "0") + track2
  endif
  inArr := { 8, 2, 37, 19, 6, 6, 12, 6, 3, 2, 6, 12, 1, 2, 6, 14, 1, 22, 13, 2, 24, 24, 24, 1,;
				4, 12, 19,;  &&  Add by TMV 09-10-2003
				1}
// Replaced by TMV 10-10-2003 - изменена версия с 2 на 3
// и добавлена обработка операции "Запрос остатка"
*  str := "2" + if( mustWait, "0", "1") + "002" + if( prevOK, "0", "1") + "00000000000000" + endl() + ;
  if procCode == "OS"  // Запрос остатка
	  str := "3" + if( mustWait, "0", "1") + "003" + if( prevOK, "0", "1") + "00000000000000"
  else
	  str := "2" + if( mustWait, "0", "1") + "003" + if( prevOK, "0", "1") + "00000000000000" + endl() + ;
         track2                                             + endl() + ;
         padL( round(summa * 100, 0), 12, "0") + "00" + procCode + ;
         "00" + "00" + "810"
  endif
  if crMakeInPos( str)
     crRunPcPos()
     if crReadOutPos( AUTO_MODE, inArr, @outArr,, .T.)
       //
       // пакет в центр не был отправлен, значит текущей транзакции нет
       // и не было !
       //
       if isPackSended( outArr, AUTO_MODE)
         if !prevOK
           setCanceled()
         endif
         strtAuto()
       else
         clsAuto()
       endif
// Replaced by TMV 22-10-2003
       if checkError( AUTO_MODE, outArr, procCode, summa)
          str := crAutoCheque( procCode, summa, outArr)
          crSaveLastDoc( str, AUTO_MODE)
// Add by TMV  27-10-2003
			 if procCode == "01" .And. outarr[ 10] <> "00" then ;
					errOk( getReasonFali( outarr[10] ) )
// Add by TMV  27-10-2003 -- End
          printAOrder( {str})
          rVV := .T.
// Replaced by TMV 10-10-2003
*          if procCode <> "20"
          if procCode <> "21"
             endAuto()
          else
            endAuto()  // Moved from down by TMV 24-12-2003
            if outarr[ 2] == "00" .AND. outarr[ 24] == "1" .AND. outarr[ 10] <> "00"
//               endAuto() // Moved Up by TMV 24-12-2003
               return .T.
            endif
          endif
// Replaced <20> to <21> and Removed by TMV 10-10-2003
*          while procCode == "20"
/*          while procCode == "21"
            if yesNo( { "Проверьте, что чек распечатан.",;
                        ""                              ,;
                        "Если чек не распечатан, то"    ,;
                        ""                              ,;
                        "ПОВТОРИТЬ ПЕЧАТЬ ЧЕКА ?"       })
               printAOrder( {str})
            else
               while .T.
                 i := dialog( { "Проверьте правильность чека." , ""},;
                               { "ПОДТВЕРДИТЬ ОПЕРАЦИЮ", "ОТМЕНИТЬ ОПЕРАЦИЮ" })
                 exit if i > 0
               enddo
               //if dialog( { "Проверьте правильность чека." , ""},;
               //            { "ПОДТВЕРДИТЬ ОПЕРАЦИЮ", "ОТМЕНИТЬ ОПЕРАЦИЮ" }) == 1
               if i == 1
                  // if checkError( LINK_TEST_MODE, crLinkTest( .T.)) then endAuto() // setLastAuto( prevOK)
                  crLinkTest( .T.)
                  endAuto()
               else
                  //
                  // главное - вызвать функцию, даже если она завершится
                  // неуспешно, то сумма все равно будет откатана назад
                  //
                  // if crReverse( .F.)  then endAuto() // setLastAuto( prevOK)
                  crReverse( .F.)
                  endAuto()
               endif
               exit
            endif
          enddo
*/
          rV := .T.
       else
         if outArr[ 2] <> "00" .AND. outArr[ 24] == "1"
           if dialog( { "Ошибка " + outArr[ 2],;
                        "                 ВНИМАНИЕ !",;
                        "      Последний сеанс связи не завершен.",;
                        "Сумма на счете клиента может быть заблокирована.",;
                        "Для разблокирования суммы :"                    },;
                        { "Повторить операцию", "Выполнить проверку связи"}) = 1
              // rV := crRunAuto( .F., procCode, summa, outArr[ 3])
             return .F.
           else
              rV := crLinkTest()
              rV := rV[ 2] == "00" .AND. rV[ 4] == "1"
           endif
         endif
       endif
     endif
  endif
return rV

//
// Проверка связи
//
funcDef crLinkTest with prevOK:LU, mustWait:LU local rV := .F., inArr, outArr, str
  DEFAULT prevOK   TO getLastAuto() // .T.
  DEFAULT mustWait TO getMustWait()
//Replaced by TMV  02-12-2003
//  inArr  := { 8, 2, 12, 1}
  inArr  := { 8, 2, 6, 6, 1}
  outArr := { "", "", "", "", ""}
//Replaced by TMV  24-10-2003
//  str := "4" + if( mustWait, "0", "1") + "002" + if( prevOK, "0", "1") + "00000000000000"
  str := "4" + if( mustWait, "0", "1") + "003" + if( prevOK, "0", "1") + "00000000000000"
  if crMakeInPos( str)
     crRunPcPos()
     if crReadOutPos( LINK_TEST_MODE, inArr, @outArr)
       if isPackSended( outArr, LINK_TEST_MODE) then clsAuto()
       if checkError( LINK_TEST_MODE, outArr)
          rV := .T.
          setLastAuto( prevOK)
       endif
     endif
  endif
return outArr

funcDef crLink1Test local arr, rV := .T.
  arr := crLinkTest()
  if arr[ 2] == "00" .AND. arr[ 4] == "1"
    OK( "Связь с Цетром есть !!!")
  else
    rV := .F.
    // errOK( { "Связи с Цетром нет", "Код ответа " + arr[ 2],;
    //         "Пакет в Центр " + if( arr[ 4] == "1", "", "НЕ ") + "отправлен."})
  endif
return rV

//
// Отмена последней транзакции
//
funcDef crReverse with prevOK:LU, mustWait:LU ;
  local rV := .F., inArr, outArr, str, prevIn,;
		prevOut, summa, procCode, dayN, s := select(), lastOper, docN,;
		str1 := "", aStr:={}, aStr1:={}, i  // Add by TMV  30-12-2003
  DEFAULT prevOK TO getLastAuto() //.T.
  DEFAULT mustWait TO getMustWait()
  if tpRead( "autoOK", "C") <> "yes"
     messageB( "Данная операция невозможна !!!")
     return .F.
  endif

  //
  // выясним была ли последняя операция вненсением наличных
  //
  //lastOper := crIsGiveMoneyLast()

  //
  // читаем предыдущий чек
  //
  prevIn := { 8, 2, 37, 19, 6, 6, 12, 6, 3, 2, 6, 12, 1, 2, 6, 14, 1, 22, 13, 2, 24, 24, 24, 1,;
				  4, 12, 19,;  &&  Add by TMV 09-10-2003
				  1}
  crReadOutPos( AUTO_MODE, prevIn, @prevOut, .T.)
  //
  // начинаем транзацию
  //
//Replaced by TMV  24-10-2003
//  inArr  := { 8, 2, 12, 14, 1, 22, 13, 2, 24, 24, 24, 1}
  inArr  :=   { 8, 2, 6, 6, 14, 1, 22, 13, 2, 24, 24, 24, 1, 6}
//Replaced by TMV  24-10-2003
//  str := "7" + if( mustWait, "0", "1") + "002" + if( prevOK, "0", "1") + "00000000000000"
  str := "7" + if( mustWait, "0", "1") + "003" + if( prevOK, "0", "1") + "00000000000000"
  if crMakeInPos( str)
     crRunPcPos()
     if crReadOutPos( REVERSE_MODE, inArr, @outArr)
       if isPackSended( outArr, REVERSE_MODE) then clsAuto()
       if checkError( REVERSE_MODE, outArr)
         if prevOut[ 10] == "00"
           dayN := crGetDayN()
           crProtPrep( dayN)
           if crProtOpen()
              PROT ->( dbGoBottom())
              procCode := PROT ->operCode
              summa    := PROT ->summa - PROT ->komSumma
              if !crIsSucAuto()
                messageB( "Последняя операция авторизации не была успешной !")
                crProtClose()
                select (s)
              endif
              crProtClose()
           endif
           select (s)
           //OK( "Чеки " + prevOut[ 14])
           docN := crChequeNumb()
// Replaced by TMV 05-01-2004
//           str := crAutoSuccess( subStr( prevOut[ 14], 1, 1), .F., ;
//                  procCode, summa, prevOut, docN)                + ;
//                  endl() + endl() + endl() + endl()              + ;
//                  crAutoSuccess( subStr( prevOut[ 14], 2, 1), .F., ;
//                  procCode, summa, prevOut, docN) + endl() + endl()
				str := crAutoSuccess( subStr( prevOut[ 14], 1, 1), .F., ;
					procCode, summa, prevOut, docN, .F.)
				str1:= crAutoSuccess( subStr( prevOut[ 14], 2, 1), .F., ;
					procCode, summa, prevOut, docN)
// Add by TMV 05-01-2004
				if !empty( str1 )
					if At( "|", str ) > 0
						aStr = CtoA( str, "|" )
						aStr1 = CtoA( str1, endl())
						str = ""
						for i = 1 to Len(aStr)
							if i <= Len(aStr1)
								aStr[i] += " | " + aStr1[i]
							else
								aStr[i] += " |"
							endif
							aStr[i] += endl()
							str += aStr[i]
						next
					else
						str += str1
					endif
				endif
// Add by TMV 05-01-2004 -- End
         else
           str := crAutoFail( prevOut)
         endif
         printAOrder( {str})
         crSaveLastDoc( str, AUTO_MODE)
// Add by TMV  27-10-2003 -- End
         rV := .T.
       endif
     endif
  endif
return rV


*----------------------------- ЧЕКИ -------------------------------------
//
// чек открытия дня
//
funcDef crOpenCheque with arr:A local str
//Replaced by TMV 16-01-2003
//         "PETROVSKIY NARODNY BANK"             + endl() + ;
//Replaced by TMV 24-10-2003
/*
  str := Space( 8) + "PETROCARD"               + endl() + ;
         "MDM BANK S-PETERSBURG"               + endl() + ;
         "BANK.TEL.:(812)326-89-49"            + endl() + ;
         padC( allTrim( arr[ 8]), 24)          + endl() + ;
         padC( allTrim( arr[ 6]), 24)          + endl() + ;
         padC( crLocTag( arr[ 7]), 24)         + endl() + ;
         padC( allTrim( arr[ 11]), 24)         + endl() + ;
         padC( allTrim( arr[ 13]), 24)         + endl() + ;
         padC( allTrim( arr[9]+" "+arr[10]),24)+ endl() + ;
         ""                                    + endl() + ;
         "ТЕРМИНАЛN "+ arr[ 1]                 + endl() + ;
         crDateTime( arr[ 4])            + endl() + ;
         "ОТКРЫТИЕ ДНЯ N " + arr[ 3]           + endl() + ;
         "ПРЕДЫДУЩЕЕ ЗАКРЫТИЕ:"                + endl() + ;
         crDateTime( arr[ 5])            + endl() + ;
         "ВЕРСИЯ:" + PROG_VERSION             + endl()
*/
  str := ""                                    + endl() + ;
         PadC("ОАО МДМ-БАНК СПБ", 35)          + endl() + ;
         PadC("ТЕЛ.БАНКА:(812)326-89-49", 35)  + endl() + ;
         padC( allTrim( arr[10]), 35)          + endl() + ;
         padC( allTrim( arr[ 8]), 35)          + endl() + ;
         padC( crLocTag( arr[ 9]), 35)         + endl() + ;
         padC( allTrim( arr[13]), 35)          + endl() + ;
         padC( allTrim( arr[ 15]), 35)         + endl() + ;
         padC( allTrim( arr[11])+" "+allTrim(arr[12]),35)+ endl() +; // Add <allTrim> by TMV 09-02-2004
         "ТЕРМИНАЛ N "+ arr[ 1]                 + endl() + ;
;//         crDateTime( arr[ 4] + arr[5])         + endl() + ;
         crDateTShort( arr[ 4] + arr[5])       + endl() + ;
         "ОТКРЫТИЕ ДНЯ N " + arr[ 3]           + endl() + ;
         "ПРЕДЫДУЩЕЕ ЗАКРЫТИЕ"                + endl() + ;
;//         crDateTime( arr[ 6] + arr[7])         + endl() + ;
         crDateTShort( arr[ 6] + arr[7])       + endl() + ;
			""                                    + endl()
// Replaced by TMV <crDateTime> to <crDateTShort>  19-01-2004
return str

//
// чек закрытия дня
//
funcDef crCloseCheque with arr:A local str
//Replaced by TMV 16-01-2003
//         "PETROVSKIY NARODNY BANK"             + endl() + ;
// Replaced by TMV 24-10-2003
/*
  str := Space( 8) + "PETROCARD"               + endl() + ;
         "MDM BANK S-PETERSBURG"               + endl() + ;
         "BANK.TEL.:(812)326-89-49"            + endl() + ;
         padC( allTrim( arr[ 8]), 24)          + endl() + ;
         padC( allTrim( arr[ 6]), 24)          + endl() + ;
         padC( crLocTag( arr[ 7]), 24)   + endl() + ;
         padC( allTrim( arr[ 11]), 24)         + endl() + ;
         padC( allTrim( arr[ 13]), 24)         + endl() + ;
         padC( allTrim( arr[ 9] + " " + arr[ 10]), 24)   + endl() + ;
         ""                                    + endl() + ;
         "ТЕРМИНАЛN "+ arr[ 1]                 + endl() + ;
                                               + endl() + ;
         "Закрытие дня N " + arr[ 3]           + endl() + ;
         crDateTime( arr[ 4])                  + endl() + ;
         ""                                    + endl() + ;
         "Открытие дня было:"                  + endl() + ;
         crDateTime( arr[ 5])                  + endl()
*/
  str := ""                                    + endl() + ;
         PadC("ОАО МДМ-БАНК СПБ", 35)          + endl() + ;
         PadC("ТЕЛ.БАНКА:(812)326-89-49", 35)  + endl() + ;
         padC( allTrim( arr[10]), 35)          + endl() + ;
         padC( allTrim( arr[ 8]), 35)          + endl() + ;
         " "                                   + endl() + ;
         padC( allTrim( arr[ 13]), 35)         + endl() + ;
         padC( allTrim( arr[ 15]), 35)         + endl() + ;
         padC( allTrim( arr[11]) + " " + allTrim(arr[ 12]), 35) + endl() +; // Add <allTrim> by TMV 09-02-2004
         "ТЕРМИНАЛ N "+ arr[ 1]                + endl() + ;
         "ЗАКРЫТИЕ ДНЯ N " + arr[ 3]           + endl() + ;
;//         crDateTime( arr[ 4] + arr[5])         + endl() + ;
         crDateTShort( arr[ 4] + arr[5])       + endl() + ;
         "ОТКРЫТИЕ ДНЯ БЫЛО"                   + endl() + ;
;//         crDateTime( arr[ 6] + arr[7])         + endl() + ;
         crDateTShort( arr[ 6] + arr[7])       + endl() + ;
			""                                    + endl()
// Replaced by TMV <crDateTime> to <crDateTShort>  19-01-2004
return str


static funcDef close2Cheque ;
  local dayN, str :="", s := select(),;
        nalSumma     := 0.00, nalCnt     := 0,;
        nalCancSumma := 0.00, nalCancCnt := 0,;
        givSumma     := 0.00, givCnt     := 0,;
        givCancSumma := 0.00, givCancCnt := 0,;
        givKomSumma  := 0.00


  dayN := crGetDayN()
  crProtPrep( dayN)
  if crProtOpen()
     while PROT ->( !Eof())
       do case
          case rTrim( PROT ->code) == "OPEN"
            if PROT ->result <> "00"
              str += " " + PROT ->time + "  "  + "ОТКРЫТИЕ ДНЯ        " + ;
                   PROT ->result + "/00" + endl()
            endif
          case rTrim( PROT ->code) == "CLOSE"
            if PROT ->result <> "00"
              str += " " + PROT ->time + "  "  + "ЗАКРЫТИЕ ДНЯ        " + ;
                   PROT ->result + "/00" +  endl()
            endif
          case rTrim( PROT ->code) == "AUTO"
            if PROT ->respCode <> "00" .OR. PROT ->result <> "00"
              str += " " + PROT ->time + "  " + left( PROT ->pan, 16) + "    " + ;
                     PROT ->result + "/" + PROT ->respCode + endl()
            endif
            if PROT ->summa > 0      .AND. PROT ->respCode == "00" .AND. ;
               PROT ->result == "00" .AND. PROT ->packState == "1"
              if PROT ->operCode == "01"
                 givSumma += PROT ->summa
                 givCnt++
                 if PROT ->isReversed
                   givCancSumma += PROT ->summa
                   givCancCnt++
                 else
                   givKomSumma += PROT ->komSumma
                 endif
              endif
// Replaced by TMV 10-10-2003
*              if PROT ->operCode == "20"
              if PROT ->operCode == "21"
                 nalSumma += PROT ->summa
                 nalCnt++
                 if PROT ->isReversed
                   nalCancSumma += PROT ->summa
                   nalCancCnt++
                 endif
              endif
            endif
       endcase
       skip 1 alias PROT
     enddo
     crProtClose()
     select (s)
  endif
  str := ;
    "Внесение наличных:"                                         + endl() + ;
    "Общее количество операций: " + nStr( nalCnt)                + endl() + ;
    "Общая сумма операций:      " + lTrim( Str( nalSumma, 15, 2))+ endl() + ;
    ""                                                           + endl() + ;
    "Количество отмененных операций: " + nStr( nalCancCnt)       + endl() + ;
    "Сумма отмененных операций:      " + lTrim( Str( nalCancSumma, 15, 2)) + endl() + ;
    ""                                               + endl() + ;
    "Итого внесено наличных:"                        + endl() + ;
    "сумма: " + Str( nalSumma - nalCancSumma, 15, 2) + endl() + ;
    "количество: " + nStr( nalCnt - nalCancCnt)      + endl() + ;
    ""                                               + endl() + ;
    "Выдача наличных:"                                         + endl() + ;
    "Общее кол-во операций: " + nStr( givCnt)                + endl() + ;
    "Общая сумма операций : " + lTrim( Str( givSumma, 15, 2)) + endl() + ;
    ""                                                           + endl() + ;
    "Количество отмененных операций: " + nStr( givCancCnt)       + endl() + ;
    "Сумма отмененных операций:      " + lTrim( Str( givCancSumma, 15, 2)) + endl() + ;
    ""                                                           + endl() + ;
    "Сумма комиссионных: " + Str( givKomSumma, 15, 2)            + endl() + ;
    ""                                                           + endl() + ;
    "Итого выдано наличных:"                                     + endl() + ;
    "сумма: " + Str( givSumma - givKomSumma - givCancSumma, 15, 2) + endl() + ;
    "количество: " + nStr( givCnt - givCancCnt)                  + endl() + ;
    ""                                                           + endl() + ;
    "                 Протокол сообщений :           "           + endl() + ;
    "+----------------------------------------------+"           + endl() + ;
    "| Время    Номер карты         Код ошибки/     |"           + endl() + ;
    "|                              Код ответа      |"           + endl() + ;
    "+----------------------------------------------+"           + endl() + ;
    str + endl() + endl() + endl()
return str


static funcDef close1Cheque ;
  local dayN, str1 :="", str2 := "", str := "", s := select(),;
        nalSumma     := 0.00, nalCnt     := 0,;
        nalCancSumma := 0.00, nalCancCnt := 0,;
        givSumma     := 0.00, givCnt     := 0,;
        givCancSumma := 0.00, givCancCnt := 0,;
        givKomSumma  := 0.00


  dayN := crGetDayN()
  crProtPrep( dayN)
  if crProtOpen()
     while PROT ->( !Eof())
       if rTrim( PROT ->code) == "AUTO"
         if PROT ->summa > 0      .AND. PROT ->respCode == "00" .AND. ;
            PROT ->result == "00" .AND. PROT ->packState == "1"
           if PROT ->operCode == "01"
             givSumma += PROT ->summa
             givCnt++
             if PROT ->isReversed
               givCancSumma += PROT ->summa
               givCancCnt++
             else
               givKomSumma += PROT ->komSumma
               str2 += " " + PROT ->time + Space( 2) + left( PROT ->pan, 16) + ;
                       Space( 5) + Str( PROT ->summa, 15, 2) + Space( 3) + ;
                       Str( PROT ->komSumma, 12, 2) + Space( 2) + ;
                       Str( PROT ->summa - PROT ->komSumma, 15, 2) + endl()
             endif
           endif
// Replaced by TMV 10-10-2003
*           if PROT ->operCode == "20"
           if PROT ->operCode == "21"
             nalSumma += PROT ->summa
             nalCnt++
             if PROT ->isReversed
               nalCancSumma += PROT ->summa
               nalCancCnt++
             else
               str1 += " " + PROT ->time + Space( 9) + left( PROT ->pan, 16) + ;
                       Space( 4) + Str( PROT ->summa, 15, 2) + endl()

             endif
           endif
            endif
       endif
       skip 1 alias PROT
     enddo
     crProtClose()
     select (s)
  endif
  str := ;
    "                  Внесение наличных без вычета отмененных операций:" + endl() + ;
    "Итого: Количество операций: " + nStr( nalCnt)         + endl() + ;
    "       Итоговая сумма : " + Str( nalSumma, 15, 2)     + endl() + ;
    "                  Отмена операций внесения наличных:" + endl() + ;
    "Итого: Количество операций: " + nStr( nalCancCnt)     + endl() + ;
    "       Итоговая сумма : " + Str( nalCancSumma, 15, 2) + endl() + ;
    "                  Внесено наличных:"                  + endl() + ;
    "+----------------------------------------------------+" + endl() + ;
    "| Время           Номер карты          Сумма         |" + endl() + ;
    "+----------------------------------------------------+" + endl() + ;
    str1                                                     + endl() + ;
    "Итого: Количество операций: " + nStr( nalCnt - nalCancCnt) + endl() + ;
    "       Внесено наличных: " + Str( nalSumma - nalCancSumma, 15, 2) + endl() + ;
    + endl()                                               + endl() + ;
    "       Выдача наличных без вычета комиссионных и отмененных операций:" + endl() + ;
    "Итого: Количество операций: " + nStr( givCnt)         + endl() + ;
    "       Итоговая сумма : " + Str( givSumma, 15, 2)     + endl() + ;
    "                  Отмена операций выдачи наличных:"   + endl() + ;
    "Итого: Количество операций: " + nStr( givCancCnt)     + endl() + ;
    "       Итоговая сумма : " + Str( givCancSumma, 15, 2) + endl() + ;
    "                  Выдано наличных:"                   + endl() + ;
    "+------------------------------------------------------------------------------+" + endl() + ;
    "| Время      Номер карты         Сумма операции   Комиссионные  Выдано наличных|" + endl() + ;
    "+------------------------------------------------------------------------------+" + endl() + ;
    str2 + endl() + ;
    "Итого:                          " + ;
     Str( givSumma - givCancSumma, 15, 2) + Space( 3) + ;
     Str( givKomSumma, 12, 2)                + Space( 2) + ;
     Str( givSumma - givCancSumma - givKomSumma, 15, 2) + endl() + endl() + ;
    "Итого: Количество операций: " + nStr( givCnt - givCancCnt) + endl()  + ;
    "       Выдано наличных: " + Str( givSumma - givCancSumma - givKomSumma, 15, 2) + endl()
return str


//
// чек авторизации
//
funcDef crAutoCheque with procCode:C, summa:N, arr:A local str := "", docN,;
	str1 := "", aStr:={}, aStr1:={}, i  // Add by TMV  30-12-2003
   //
   // пакет отправлен в центр авторизации и результат = 0
   //
   if arr[ 2] == "00" .AND. arr[ 24] == "1"
    if arr[ 10] == "00"
      //OK( "Чеки " + arr[ 14])
      docN := crChequeNumb()
// Replaced by TMV  30-12-2003
//      str  := crAutoSuccess( subStr( arr[ 14], 1, 1), .T., procCode, summa, arr, docN) + ;
//             endl() + endl() + endl() + endl() + ;
//             crAutoSuccess( subStr( arr[ 14], 2, 1), .T., procCode, summa, arr, docN) + ;
//             endl() + endl()
      str  := crAutoSuccess( subStr( arr[ 14], 1, 1), .T., procCode, summa, arr, docN, .F.)
      str1 := crAutoSuccess( subStr( arr[ 14], 2, 1), .T., procCode, summa, arr, docN)
// Add by TMV 30-12-2003
		if !empty( str1 )
			if (procCode == "01" .Or. procCode == "21") .And. At( "|", str ) > 0
				aStr = CtoA( str, "|" )
				aStr1 = CtoA( str1, endl())
				str = ""
				for i = 1 to Len(aStr)
					if i <= Len(aStr1)
						aStr[i] += " | " + aStr1[i]
					else
						aStr[i] += " |"
					endif
					aStr[i] += endl()
					str += aStr[i]
				next
			else
				str += str1
			endif
		endif
// Add by TMV 30-12-2003 -- End
    else
		str := crAutoFail( arr)
    endif
   endif

return str

//
// чек при успешной авторизации
//
funcDef crAutoSuccess ;
   with type:C, isNormal:L, procCode:C, summa:N, arr:A, docN:C, ;
	lEndLine:LU ;  // Add by TMV  30-12-2003
   local str := "", realSumma,;
		sEndLine := ""  // Add by TMV 30-12-2003
  if empty( type) then return str

DEFAULT lEndLine TO .T.                         // Add by TMV 30-12-2003

sEndLine = if(procCode == "OS" .Or. lEndLine, endl(), "|")  // Add by TMV 30-12-2003

//Replaced by TMV 16-01-2003
//         "PETROVSKIY NARODNY BANK"             + endl() + ;
//Replaced by TMV 09-10-2003
/*  str := Space( 8) + "PETROCARD"               + endl() + ;
         "MDM BANK S-PETERSBURG"               + endl() + ;
         "BANK.TEL.:(812)326-89-49"            + endl() + ;
         padC( allTrim( arr[18]), 24)          + endl() + ;
         padC( allTrim( arr[16]), 24)           + endl() + ;
         padC( allTrim( crLocTag( arr[17])),24)+ endl() + ;
         padC( allTrim( arr[ 21]), 24)         + endl() + ;
         padC( allTrim( arr[ 23]), 24)         + endl() + ;
         padC( allTrim( arr[19]+" "+arr[20]),24)+endl() + ;
         ""                                    + endl() + ;
         "ТЕРМИНАЛN "+ arr[ 1]                 + endl() + ;
         padL( "ЧЕК N " + docN, 24)            + endl() + ;
         crDateTime( arr[ 5] + arr[ 6])        + endl() + ;
         ""                                    + endl()
*/
  str := Space(35)                             + sEndLine + ;
         padC("ОАО МДМ-БАНК СПБ", 35)          + sEndLine + ;
         padC("ТЕЛ.БАНКА:(812)326-89-49", 35)  + sEndLine + ;
         padC( allTrim( arr[18]), 35)          + sEndLine + ;
         padC( allTrim( arr[16]), 35)          + sEndLine + ;
         Space(35)                             + sEndLine + ;
         padC( allTrim( arr[ 21]), 35)         + sEndLine + ;
         padC( allTrim( arr[ 23]), 35)         + sEndLine + ;
         padC( allTrim( arr[19])+" "+ allTrim(arr[20]),35)+sEndLine +; // Add <allTrim> by TMV 09-02-2004
         PadR("ТЕРМИНАЛ N "+ arr[ 1], 35)      + sEndLine + ;
         crDateTShort( arr[ 5] + arr[ 6]) + if( procCode == "OS", "", padL( "ЧЕК N " + docN, 21)) + sEndLine + ;
         padC( allTrim( crSchemeTag( arr[ 13])), 35) + sEndLine
	if procCode == "OS" then str += Space(35) + sEndLine
//Replaced by TMV 09-10-2003 -- End

// Add by TMV 09-10-2003
	str += PadR("КАРТА " + allTrim(arr[ 4]), 35) + sEndLine  // Add <allTrim> by TMV 09-02-2004
	if procCode == "01" then STR += Space(35) + sEndLine
	str += PadR('СРОК ДЕЙСТВИЯ ' + right(arr[25], 2) + '/' + Left(arr[25], 2), 35) + sEndLine
	if procCode == "21" then str += PadR('КАССИР ' + allTrim(ARR[27]), 35) + sEndLine
	str += 'RRN ' + arr[26] + PadL( allTrim('КОД АВТ. ' + arr[11]), 19) + sEndLine
  do case
     case procCode == "01" .AND. isNormal
       str += PadC("ВЫДАЧА НАЛИЧНЫХ", 35) + sEndLine
     case procCode == "01" .AND. !isNormal
       str += PadC("ОТМЕНА ВЫДАЧИ НАЛИЧНЫХ", 35) + sEndLine
  endcase
// Add by TMV 09-10-2003 -- End

// Removed by TMV 09-10-2003
*         if procCode == "01"
*           str += padC( allTrim( crSchemeTag( arr[ 13])),24) + endl()
*         endif
*         str += "КАРТА:" + arr[ 4]             + endl() + ;
*         ""                                    + endl() + ;
*         "ТРАССА:  " + arr[ 8]                 + endl() + ;
*         "КОД АВТ. " + arr[11]                 + endl()
*  do case
*     case procCode == "01" .AND. isNormal
*       str += Space( 9) + "ВЫДАЧА НАЛИЧНЫХ" + endl()
*     case procCode == "01" .AND. !isNormal
*       str +=    "  ОТМЕНА ВЫДАЧИ НАЛИЧНЫХ" + endl()
*  endcase
// Removed by TMV 09-10-2003 -- End

  realSumma := round( val( arr[ 7])/ 100, 2)
  summa     := round( summa, 2)

  if procCode == "01"
    if realSumma <> summa
// Replaced by TMV  30-12-2003
//      str += "ОБЩАЯ СУММА" + padL( nStr( realSumma), 10) + "РУБ" + sEndLine + ;
//             "КОМИССИОННЫЕ"+ padL( nStr( realSumma - summa),9)+"РУБ" + sEndLine
      str += "СУММА ОПЕРАЦИИ" + padL( nStr( realSumma) + " РУБ", 21) + sEndLine + ;
             "КОМИССИОННЫЕ"+ padL( nStr( realSumma - summa) +" РУБ", 23) + sEndLine
    endif
//Replaced by TMV 09-02-2004
//    str += if( isNormal, "СУММА К ВЫДАЧЕ", "СУММА ОПЕРАЦИИ") // + sEndLine Edit by TMV 21-10-2003
    str += "СУММА К ВЫДАЧЕ"
// Replaced by TMV 21-10-2003
*	 str += padL( nStr( summa), 21) + "РУБ" + sEndLine
    str += padL( nStr( summa) + " РУБ", 21) + sEndLine
  endif
// Replaced by TMV 10-10-2003
*  if procCode == "20"
  if procCode == "21"
    if isNormal
      str += PadC("ВНЕСЕНИЕ НАЛИЧНЫХ", 35) + sEndLine
    else
      str += PadC("ОТМЕНА ВНЕСЕНИЯ НАЛИЧНЫХ", 35) + sEndLine
    endif
    str += "СУММА" + PadL( Str( realSumma, 16, 2) + " РУБ", 30) + sEndLine
  endif

// Add by TMV  22-10-2003 for "Запрос остатка"
  if type == "E"
  		str := str + 'ОСТАТОК' + PadL( toBalance( arr[ 12]) + ' РУБ', 28) + sEndLine
  else
// Add by TMV  22-10-2003 for "Запрос остатка"  -- End
// Removed by TMV 25-12-2003
*     if type == "F" ;
*     		.And. procCode == "01" // Add by TMV 21-10-2003
*       if isNormal
*         str := str + 'ОСТАТОК:' + toBalance( arr[ 12]) +  'РУБ' + sEndLine
*       endif
*     endif
//     str := str + sEndLine
//     if procCode == "01" then str += "" + sEndLine  // Add by TMV 21-10-2003
     if type $ "C,F" ;
     		.And. procCode == "01" // Add by TMV 21-10-2003
       str := str + PadR("КЛИЕНТ__________________", 35, "_") + sEndLine
       str := str + PadR("      __________________", 35, "_") + sEndLine
       str := str + PadR("      __________________", 35, "_") + sEndLine
       str := str + PadR("      __________________", 35, "_") + sEndLine
       str := str + PadR("      __________________", 35, "_") + sEndLine
     endif
     if type $ "C,B,F,D" // Add <C,D> by TMV 19-12-2003 // .or. ( type == "D" .and. !isNormal)
// Removed by TMV  09-02-2004
//			 .And. procCode == "01") .OR. (type == "D" .And. procCode == "21")  // Add by TMV 30-12-2003
		  str := str + PadR("ПОДПИСЬ КЛИЕНТА: ___________", 35) + sEndLine
     endif
     str := str + PadR("ПОДПИСЬ КАССИРА: ___________", 35) + sEndLine
*     str := str + sEndLine
// Replaced by TMV  30-12-2003
//     if type $ "F,G"
//       str := str + Space( 8)  + 'СПАСИБО.' + sEndLine
//       str := str + 'ПОЖАЛУЙСТА,СОХРАНИТЕ ЧЕК' + sEndLine
//     endif
     if type =="F"
       str := str + 'ВНИМАНИЕ:БАНК-ЭМИТЕНТ МОЖЕТ ВЗИМАТЬ' + sEndLine
       str := str + 'С ДЕРЖАТЕЛЯ КАРТЫ ПЛАТУ ЗА ВЫДАЧУ  ' + sEndLine
       str := str + 'НАЛИЧНЫХ НА ОБОРУДОВАНИИ ДР. БАНКОВ' + sEndLine
     endif
     str := str + 'СПАСИБО! ПОЖАЛУЙСТА, СОХРАНИТЕ ЧЕК.' + sEndLine
  endif  // Add by TMV  22-10-2003 for "Запрос остатка"
return str

//
// чек при неуспешной авторизации
//
funcDef crAutoFail with arr:A local str := "", arCodes, ptr, sName

   sName   := "В ОПЕРАЦИИ ОТКАЗАНО"
// Replaced by TMV 27-10-2003
/*   arCodes := {;
       { "61", "ПРЕВЫШЕН ЛИМИТ СУММЫ" + endl() + Space( 7) + "СНЯТИЯ"},;
       { "51", "ПРЕВЫШЕН ОСТАТОК"},;
       { "01", "ВВОД ПИНа ОБЯЗАТЕЛЕН"},;
       { "55", "ПИН НЕВЕРЕН"},;
       { "04", "КАРТА ЗАДЕРЖАНА"},;
       { "33", "КАРТА ЗАДЕРЖАНА"},;
       { "43", "КАРТА ЗАДЕРЖАНА"},;
       { "07", "КАРТА ЗАДЕРЖАНА"},;
       { "41", "КАРТА ЗАДЕРЖАНА"},;
       { "02", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "30", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "56", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "60", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "63", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "66", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "79", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "99", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "75", "ИСЧЕРПАНО КОЛИЧЕСТВО" + endl() + Space( 5) + "НАБОРОВ ПИН"},;
       { "65", "ПРЕВЫШЕН ЛИМИТ ПО" + endl() + "КОЛИЧЕСТВУ СНЯТИЙ"},;
       { "57", "ОПЕРАЦИЯ ЗАПРЕЩЕНА" + endl() + Space( 5) + "ДЛЯ КАРТЫ"},;
       { "54", "ИСТЕК СРОК ДЕЙСТВИЯ КАРТЫ"}}
*/
	arCodes = crReasonCheck()
// Replaced by TMV 27-10-2003 -- End

   ptr := aScan( arCodes, {|x| x[ 1] == arr[ 10]})
   if ptr > 0 then sName := arCodes[ ptr, 2]
//Replaced by TMV 16-01-2003
//          " PETROVSKIY NARODNY BANK"            + endl() + ;
//Replaced by TMV 09-10-2003
*   str := "   ОТКАЗАНО/-DECLINED"               + endl() + ;
*         "MDM BANK S-PETERSBURG"               + endl() + ;
*          padC( arr[19] + " " + arr[ 20], 24)   + endl() + ;
*         ""                                     + endl() + ;
*         "ТЕРМИНАЛN "+ arr[ 1]                  + endl() + ;
*         crDateTime( arr[ 5] + arr[ 6])         + endl() + ;
*         "КАРТА:" + arr[ 4]                     + endl() + ;
*         "КОД "   + arr[10]                     + endl() + ;
*         sName                                  + endl() + ;
*         "ОСТАТОК:" + toBalance( arr[12]) + "РУБ"           + endl()

   str := PadC("ОТКАЗАНО/-DECLINED", 35)        + endl() + ;
         PadC("ОАО МДМ-БАНК СПБ", 35)           + endl() + ;
         padC( allTrim(arr[19]) + "  " + allTrim(arr[ 20]), 35) + endl() + ; // Add <allTrim> by TMV 09-02-2004
;//         Space(35)                              + endl() + ; // Removed by TMV 09-02-2004
         PadR("ТЕРМИНАЛ N "+ arr[ 1], 35)       + endl() + ;
         PadR(crDateTime( arr[ 5] + arr[ 6]), 35) + endl() + ;
         PadR("КАРТА " + allTrim(arr[ 4]), 35) + endl() + ; // Add <allTrim> by TMV 09-02-2004
         PadR("СРОК ДЕЙСТВИЯ " + right(arr[25], 2) + '/' + Left(arr[25], 2), 35) + endl() +;
         PadR("КОД "   + arr[10], 35)           + endl() + ;
         PadR( sName, 35)                       + endl() + ;
         Space(35)                              + endl()
// Replaced by TMV 21-10-2003
//       "СРОК ДЕЙСТВИЯ " + arr[ 4]             + endl() + ;

//Replaced by TMV 09-10-2003 -- End
return str

static funcDef toBalance with str:C ;
local sTmp := '', i := 0    // Add by TMV 16-01-2004
  if empty( str)
    str += " "
  else
	 do while ++i < Len( str) - 3 ;
	 		.And. Val( subStr( str, i, 1) ) == 0 // Add by TMV 16-01-2004
	 enddo                                     // Add by TMV 16-01-2004
	 sTmp = Space(i - 1) + subStr( str, i)     // Add by TMV 16-01-2004
// Replaced by TMV 16-01-2004
//    str := left( str, len( str) - 2) + "." + right( str, 2)
    str := left( sTmp, len( sTmp) - 2) + "." + right( sTmp, 2)
  endif
return str

* ----------- Вспомогательные функции -----------------------------------

//
// чтение выходного файла
//
funcDef crReadOutPos with mode:N, inArr:A, outArr:AU, usePrev:LU, savePrev:LU ;
  local rV := .F., str, fileName, i

  DEFAULT usePrev  TO .F.
  DEFAULT savePrev TO .F.

  outArr := {}; aSize( outArr, len( inArr)); aFill( outArr, "")
  fileName := "out.pos"
  if usePrev then fileName := "prev.pos"
  fileName := newFPath( fileName, crPosPath())
  if !file( fileName)
    errOK( "Нет файла " + fileName)
    return rV
  endif
  str := memoRead( fileName)
  if savePrev then memoWrit( newFPath( "prev.pos", crPosPath()), str)
  do case
     case mode == OPEN_DAY_MODE .OR. mode == CLOSE_DAY_MODE .OR. ;
          mode == AUTO_MODE     .OR. mode == LINK_TEST_MODE .OR. ;
          mode == REVERSE_MODE
       for i := 1 to len( inArr)
         outArr[ i] := left( str, inArr[ i])
         str        := subStr( str, inArr[ i] + 1)
       next
       rV := .T.
     otherwise
  endcase

  //
  // запишем OUT_POS в протокол
  //
  crProtOutPos( mode, outArr)


return rV


//
// создание входного файла и удаление выходного файла
//
funcDef crMakeInPos with arr:CA ;
  local str := "", rV := .F., i, fileName, nHandle
  fileName := newFPath( "in.pos", crPosPath())
  if file( fileName) then fErase( fileName)
  if valType( arr) == "A"
    for i := 1 to len( arr) do str += arr[ i]
  else
    str := arr
  endif
  if !( rV := memoWrit( fileName, str))
    errOK( "Ошибка записи in.pos # " + nStr( fError()) )
  else
    //
    // Если удалось записать IN_POS
    //
    crProtInPos( str)
    //
    // удаление старого ответа
    //
// Replaced by TMV 02-07-2004
//    fileName := newFPath( crPosPath(), "out.pos")
    fileName := newFPath( "out.pos", crPosPath())
    fErase( fileName)
    if file( fileName)
      errOK( "Ошибка удаления " + fileName)
      rV := .F.
    endif
  endif
return rV



//
// путь к pc-pos
//
function crPosPath( newPath)
static oldPath := NIL
  if oldPath == NIL then oldPath := getEnv( POS_DSK)
  if empty( oldPath)
    errOK( { "Не установлена переменная " + POS_DSK + " !" ,;
             "Продолжение работы невозможно"})
  endif
  if !empty( newPath) then oldPath := newPath
return oldPath

autofunction setValue pcPath init ".\"

//
// Запуск внешней программы
//
funcDef crRunPcPos ;
  local fileName, Screen0, ems, xms, umb, mde, bl, msg

  Msg      := "Запуск сеанса связи"
  fileName := newFPath( "pc_pos.exe", pcPath())
  if !file( fileName)
    messageB("Не найден файл: "+ fileName +" ! продолжить - <ESC>")
    return nil
  endif
  save screen to Screen0
  bl := setBlink()
  if Msg != NIL then messageStartUp( Msg)
  ems := swpUseEms( .F.)
  umb := swpUseUmb( .F.)
  xms := swpUseXms( .F.)
  mde := swpVidMde( .T.)
  swpRunCmd( fileName, 0, "", "")
  swpUseEms( ems)
  swpUseUmb( umb)
  swpUseXms( xms)
  swpVidMde( mde)
  setBlink(  bl)
  if Msg != NIL then messageExit()
  restore screen from Screen0
return NIL

//
// раскодировка местоположения отделения
//
funcDef crLocTag with str:C local rV := "", arr, pos
  arr := {{ "1", "BRANCH"           }, { "2", "OUTLET"           },;
          { "3", "STORE"            }, { "4", "DRUGSTORE"        },;
          { "A", "DRUGSTORE"        }, { "C", "CAFE"             },;
          { "E", "EXCHANGE OFFICE"  }, { "G", "GROCERY SHOP"     },;
          { "H", "HOTEL"            }, { "K", "CASHIER OFFICE"   },;
          { "M", "SHOP"             }, { "R", "RESTAURANT"       },;
          { "Y", "SUPERMARKET"      }, { "O", "CONVERSION OFFICE"},;
			 { "X", "ОТДЕЛЕНИЕ"        }, { "Z", "ОБМЕННЫЙ ПУНКТ"   }}  // Add by TMV 29-12-2003
  pos := aScan( arr, {|x|x[ 1] == str})
  if pos > 0 then rV := arr[ pos, 2]
return rV

//
// раскодировка даты и времени
//
funcDef crDateTime with str:C, ;
	lFullDate:LU ;        // Add by TMV 05-01-2004 And Edit <CU> to <LU> 14-01-2004
	local rV,;
	sYear := allTrim( subStr( str, 1, 2) ) // Add by TMV 21-10-2003

DEFAULT lFullDate TO .F. // Add by TMV 05-01-2004

	if empty(sYear) .And. lFullDate then ;
		sYear = SubStr( DtoS( setCurDate()), 3, 2)  // Add by TMV 05-01-2004
// Replaced by TMV 21-10-2003
*   rV := subStr( str, 5, 2) + "/" + subStr( str, 3, 2) + "/" + ;
*         subStr( str, 1, 2) + " " + subStr( str, 7, 2) + ":" + ;
*         subStr( str, 9, 2) + ":" + subStr( str,11, 2)
	rV := subStr( str, 5, 2) + "/" + subStr( str, 3, 2) + ;
		if(empty(sYear), "", "/" + sYear) + " " + ;
		subStr( str, 7, 2) + ":" + ;
		subStr( str, 9, 2) + ":" + subStr( str,11, 2)
return rV

// Add by TMV 30-12-2003
// раскодировка даты и времени без года
//
funcDef crDateTShort with str:C local rV
	rV := subStr( str, 5, 2) + "/" + subStr( str, 3, 2) + " " + ;
		subStr( str, 7, 2) + ":" + ;
		subStr( str, 9, 2) + ":" + subStr( str,11, 2)
return rV

//
// раскодировка платежной системы
//
funcDef crSchemeTag with code:C local pos, rV := "", arr
// Replaced by TMV 29-12-2003
/*   arr := {{ "0", ""                 }, { "1", "EDC/MAESTRO"        },;
           { "2", "EUROCHEQUE/CIRRUS"}, { "4", "EUROCARD/MASTERCARD"},;
           { "5", "VISA"             }, { "6", "VISA ELECTRON"      },;
           { "7", "PLUS"             }, { "8", "PETROCARD"          }}
*/
   arr := {{ "0", ""                 }, { "1", "MAESTRO"        },;
           { "2", "MAESTRO"          }, { "4", "MASTERCARD"},;
           { "5", "VISA"             }, { "6", "VISA"      },;
           { "7", "PLUS"             }, { "8", "PETROCARD"          },;
			  { "8", "PETROCARD"        }, { "9", "DINERS"},;    // Add by TMV 29-12-2003
			  { "a", "МДМ-БАНК СПБ"     }, { "b", "МДМ-БАНК"}}   // Add by TMV 29-12-2003
// Edit by TMV - add function <Upper> - 19-01-2004
  pos := aScan( arr, {|x| Upper(x[ 1]) == Upper(code)})
  if pos > 0 then rV := arr[ pos, 2]
return rV

//
// Номер чека
//
funcDef crChequeNumb local rV, oldValue, newValue
  oldValue := max( val( tpRead( "ЧЕК", "C")), 1)
  newValue := oldValue + 1
  tpWrite( "ЧЕК", "C", nStr( newValue))
return nStr( oldValue)

funcDef crSetChequeNumb
  tpWrite( "ЧЕК", "C",  "1")
return NIL

//
// номер дня
funcDef crGetDayN local rV := ""
  rV := tpRead( "НОМЕР ДНЯ", "C")
return rV
funcDef crSetDayN with dayN:C
  tpWrite( "НОМЕР ДНЯ", "C", dayN)
return NIL

//
// Получение дополнительной информации для закрытия дня
//
funcDef crAddInfo with autoOpen:CU, codeAuto:CU, time:CU, pan:CU, summa:NU ;
   local rV := .T., s := select(), isFound := .F., foundOpen := .F.

  autoOpen := codeAuto := time := pan := ""; summa := 0.00
  crProtPrep( crGetDayN() )
  if crProtOpen()
    PROT ->( dbGoBottom())
    while PROT ->( !bof())
      if rTrim( PROT ->code) == "OPEN" .AND. PROT ->result == "00" .AND. ;
         PROT ->packState == "1" .AND. !foundOpen
         autoOpen  := PROT ->auto
         foundOpen := .T.
      endif

      if rTrim( PROT ->code) == "AUTO" .AND. !PROT ->isReversed       .AND. ;
         PROT ->result       == "00"   .AND.  PROT ->packState == "1" .AND. ;
         PROT ->respCode     == "00"   .AND. !isFound
         codeAuto := PROT ->auto
         time     := PROT ->time
         pan      := PROT ->pan
         summa    := PROT ->summa - PROT ->komSumma
         isFound  := .T.
      endif
      skip -1 alias PROT
    enddo
    crProtClose()
  endif

return rV

* ----------------------- Протоколы работы --------------------------------
func crProtInfo
static dbInfo := { "", "PROT",;
                   {{ "date",       "C",   8, 0},;
                    { "time",       "C",   8, 0},;
                    { "pan",        "C",  19, 0},;
                    { "auto",       "C",   6, 0},;
                    { "track2",     "C",  40, 0},;
                    { "operCode",   "C",   2, 0},;
                    { "summa",      "N",  15, 2},;
                    { "komSumma",   "N",  15, 2},;
                    { "balance",    "C",  12, 0},;
                    { "result",     "C",   2, 0},;
                    { "packState",  "C",   1, 0},;
                    { "respCode",   "C",   2, 0},;
                    { "code",       "C",  10, 0},;
                    { "isReversed", "L",   1, 0},;
                    { "isAccepted", "L",   1, 0}}, {}, 0, .F.  }
return dbInfo

funcDef crProtPrep with dayN:C local path, dbInfo
  // path := confPathOD()
  path := crPosPath()
  if right( path, 1) <> "\" then path += "\"
  dbInfo := crProtInfo()
  dbInfo[ 1] := path + "pr" + dayN + ".DBF"
return NIL

autofunction setValue crOpenBefore init .F.

funcDef crProtOpen local rV := .T.
  if !file( crProtInfo()[ 1]) then baseCreate( crProtInfo())
  crOpenBefore( select( "PROT") > 0)
  if select( "PROT") == 0
    rV := baseMXOpen( crProtInfo())
  endif
return rV

funcDef crIsSucAuto local rV := .F.
   if rTrim( PROT ->code) == "AUTO"
     if PROT ->result == "00" .AND. PROT ->packState == "1"
       rV := PROT ->respCode == "00"
     endif
   endif
return rV


funcDef crProtClose
  if !crOpenBefore() then baseMClose( crProtInfo())
return NIL


funcDef crIsGiveMoneyLast local rV := .F., s := select()
  if crProtOpen()
     PROT ->( dbGoBottom())
     if rTrim( PROT ->code) == "AUTO"
        if PROT ->result == "00" .AND. PROT ->packState == "1"
// Replaced by TMV 10-10-2003
*          rV := PROT ->respCode == "00" .AND. PROT ->operCode == "20"
          rV := PROT ->respCode == "00" .AND. PROT ->operCode == "21"
        endif
     endif
     crProtClose()
  endif
  select (s)
return rV


*---------------- Проверка на ошибку -------------------------------
funcDef checkError with mode:N, arr:A, operCode:CU, summa:NU, isSilent:LU;
  local rV := .T., s := select(), dayN, setAutoOK := .F.
  DEFAULT isSilent TO .F.

  do case
     //
     // Открытие дня
     //
     case mode == OPEN_DAY_MODE
        rV := crChkLast( arr, mode)
        dayN := arr[ 3]
        if val( dayN) == 0 then dayN := crGetDayN()
        if val( arr[ 3]) <> 0
         crProtPrep( arr[ 3])
         if crProtOpen()
           if rV
             crSetDayN( arr[ 3])
             //
             // сохраняем структуру присланную в КЦ
             //
// Replaced by TMV 24-10-2003
/*
             tpWrite( "time_open",    "C", arr[ 4])
             tpWrite( "term_loc",     "C", arr[ 6])
             tpWrite( "term_loc_tag", "C", arr[ 7])
             tpWrite( "merch_ident",  "C", arr[ 8])
             tpWrite( "city",         "C", arr[ 9])
             tpWrite( "country",      "C", arr[10])
             tpWrite( "addr",         "C", arr[11])
             tpWrite( "addr_ext_ru",  "C", arr[12])
             tpWrite( "addr_ext_lat", "C", arr[13])
*/
             tpWrite( "time_open",    "C", arr[ 5])
             tpWrite( "term_loc",     "C", arr[ 8])
             tpWrite( "term_loc_tag", "C", arr[ 9])
             tpWrite( "merch_ident",  "C", arr[10])
             tpWrite( "city",         "C", arr[11])
             tpWrite( "country",      "C", arr[12])
             tpWrite( "addr",         "C", arr[13])
             tpWrite( "addr_ext_ru",  "C", arr[14])
             tpWrite( "addr_ext_lat", "C", arr[15])
// Replaced by TMV 24-10-2003 -- End
           endif
           PROT ->( dbAppend())
// Replaced by TMV 24-10-2003
/*
           PROT ->date      := subStr( crDateTime( arr[ 4]), 1, 8)
           PROT ->time      := subStr( crDateTime( arr[ 4]),10, 8)
           PROT ->result    := arr[  2]
           PROT ->packState := arr[ 14]
           PROT ->auto      := arr[ 15]
*/
// Edit by TMV  05-01-2004 (add new parametr)
           PROT ->date      := subStr( crDateTime( arr[ 4] + arr[5], .T.), 1, 8)
           PROT ->time      := subStr( crDateTime( arr[ 4] + arr[5], .T.),10, 8)
           PROT ->result    := arr[  2]
           PROT ->packState := arr[ 16]
           PROT ->auto      := arr[ 17]
// Replaced by TMV 24-10-2003 -- End
           PROT ->code      := "OPEN"
			  PROT->(dbunlock())   &&  Add by TMV  23-08-2002
           PROT ->( dbSkip( 0))
           crProtClose()
         endif
       endif
       //if !rV
       //  errOK( { "ОШИБКА ОТКРЫТИЯ ДНЯ", "Код ответа " + arr[ 2],;
       //           "Пакет в Центр " + if( arr[ 14] == "1", "", "НЕ ") + "отправлен."})
       //endif
     //
     // день закрыт
     //
     case mode == CLOSE_DAY_MODE
       rV := crChkLast( arr, mode)
       crProtPrep( crGetDayN() )
       if arr[ 14] == "1" .AND. crProtOpen()
         PROT ->( dbAppend())
// Replaced by TMV 24-10-2003
/*
         PROT ->date      := subStr( crDateTime( arr[ 4]), 1, 8)
         PROT ->time      := subStr( crDateTime( arr[ 4]),10, 8)
         PROT ->result    := arr[ 2]
         PROT ->packState := arr[ 14]
*/
// Edit by TMV  05-01-2004 (add new parametr)
         PROT ->date      := subStr( crDateTime( arr[ 4] + arr[5], .T.), 1, 8)
         PROT ->time      := subStr( crDateTime( arr[ 4] + arr[5], .T.),10, 8)
         PROT ->result    := arr[ 2]
         PROT ->packState := arr[ 16]
// Replaced by TMV 24-10-2003 -- End
         PROT ->code      := "CLOSE"
			PROT->(dbunlock())   &&  Add by TMV  23-08-2002
         PROT ->( dbSkip( 0))
         crProtClose()
       endif
       //if !rV .AND. !isSilent
       //  errOK( { "ОШИБКА ЗАКРЫТИЯ ДНЯ", "Код ответа " + arr[ 2],;
       //       "Пакет в Центр " + if( arr[ 14] == "1", "", "НЕ ") + "отправлен."})
       //endif
     //
     // авторизация
     //
     case mode == AUTO_MODE
       rV := crChkLast( arr, mode)
       crProtPrep( crGetDayN() )
       if arr[ 24] == "1" .AND. crProtOpen()
         PROT ->( dbAppend())
// Edit by TMV  05-01-2004 (add new parametr)
         PROT ->date      := subStr( crDateTime( arr[ 5] + arr[ 6], .T.), 1, 8)
         PROT ->time      := subStr( crDateTime( arr[ 5] + arr[ 6], .T.),10, 8)
         PROT ->auto      := arr[11]
// Edit by TMV 28-01-2004 - add <allTrim>
         PROT ->pan       := allTrim(arr[ 4])
         PROT ->track2    := arr[ 3]
         PROT ->operCode  := operCode
         if val( arr[ 7])/ 100 > 0
           PROT ->summa     := val( arr[ 7])/ 100
           PROT ->komSumma  := val( arr[ 7])/ 100 - summa
           PROT ->balance   := arr[ 12]
         endif
         PROT ->result    := arr[ 2]
         PROT ->packState := arr[ 24]
         PROT ->respCode  := arr[ 10]
		   if operCode == 'OS'            // Add by TMV  24-10-2003
				PROT ->code      := "BAL"   // Add by TMV  24-10-2003
			else                           // Add by TMV  24-10-2003
         	PROT ->code      := "AUTO"
			endif                          // Add by TMV  24-10-2003
			PROT->(dbunlock())   &&  Add by TMV  23-08-2002
         PROT ->( dbSkip( 0))
         crProtClose()
         //
         // операция авторизации прошла успешно, если
         // 1. пакет в центр отправлен           packState = 1
         // 2. сеанс связи прошел без ошибок     result   = 00
         // 3. снятие денег прошло нормально     respCode = 00
         //
         setAutoOK := arr[2] == "00" .AND. arr[10] == "00" .AND. arr[24] == "1"
      endif
    case mode == REVERSE_MODE
      rV := crChkLast( arr, mode)
      if rV then crRevLast()
    case mode == LINK_TEST_MODE
      rV := crChkLast( arr, mode)
    otherwise
 endcase
 showErrMessage( mode, arr)
 select (s)
 tpWrite( "autoOK", "C", if( setAutoOK, "yes", "no"))
return rV

//
// проверка ответа на операцию
//
static funcDef crChkLast with arr:A, mode:N local rV := .F.
  do case
     case mode == OPEN_DAY_MODE
// Replaced by TMV  24-10-2003
//        rV := arr[ 2] == "00" .AND. arr[ 14] == "1"
        rV := arr[ 2] == "00" .AND. arr[ 16] == "1"
     case mode == CLOSE_DAY_MODE
// Replaced by TMV  24-10-2003
//       rV := arr[ 2] == "00" .AND. arr[ 14] == "1"
       rV := arr[ 2] == "00" .AND. arr[ 16] == "1"
     case mode == AUTO_MODE
       rV := arr[ 2] == "00" .AND. arr[ 24] == "1"
    case mode == REVERSE_MODE
      rV := .T. // ( arr[ 2] == "00" .AND. arr[ 12] == "1" ) .OR. crIsGiveMoneyLast()
    case mode == LINK_TEST_MODE
// Replaced by TMV  24-10-2003
//      rV := arr[ 2] == "00" .AND. arr[ 4] == "1"
      rV := arr[ 2] == "00" .AND. arr[ 5] == "1"
    otherwise
 endcase
return rV

//
// проверка отправки пакета
//
static funcDef isPackSended with arr:A, mode:N local rV := .F.
  do case
     case mode == OPEN_DAY_MODE
// Replaced by TMV 24-10-2003
//       rV := arr[ 14] == "1"
       rV := arr[ 16] == "1"
     case mode == CLOSE_DAY_MODE
// Replaced by TMV 24-10-2003
//       rV := arr[ 14] == "1"
       rV := arr[ 16] == "1"
     case mode == AUTO_MODE
       rV := arr[ 24] == "1"
    case mode == REVERSE_MODE
// Replaced by TMV 24-10-2003
//       rV := arr[ 12] == "1"
       rV := arr[ 13] == "1"
    case mode == LINK_TEST_MODE
// Replaced by TMV 24-10-2003
//       rV := arr[  4] == "1"
       rV := arr[  5] == "1"
    otherwise
 endcase
return rV

//
// установка признака отката операции
//
static funcDef crRevLast local s := select()
  crProtPrep( crGetDayN() )
  if crProtOpen()
    PROT ->( dbGoBottom())
	 PROT->(rlock())   &&  Add by TMV  23-08-2002
    PROT ->isReversed := .T.
	 PROT->(dbunlock())   &&  Add by TMV  23-08-2002
    PROT ->( dbSkip( 0))
    crProtClose()
  endif
  select (s)
return NIL


* ---------------- работа с temp_pos ---------------------------
func tpInfo
static dbInfo := { "temp_pos.dbf", "temp_pos",;
                 {{ "COMMENT",   "C", 20, 0},;
                  { "DATE_TEMP", "D",  8, 0},;
                  { "STR_TEMP",  "C", 30, 0},;
                  { "NUM_TEMP",  "N", 15, 0}},;
                 {{ "temp_pos.ntx", "comment"}}, 0, .F.}
return dbInfo


funcDef tpWrite with str:C, strType:C, newValue:anyType ;
  local rV := .F., dbInfo := tpInfo(), s := select()
  basePath( dbInfo, crPosPath())
  if !file( dbInfo[ 1]) then baseCreate( dbInfo)
  if baseMOpen( dbInfo)
     if TEMP_POS ->( !dbSeek( str))
        TEMP_POS ->( dbAppend())
        if ( rV := TEMP_POS ->( !netErr()))
          TEMP_POS ->comment := str
        endif
     else
        rV := TEMP_POS ->( mRLock())
     endif
     if rV
       if strType = "C" then TEMP_POS ->str_Temp  := newValue
       if strType = "D" then TEMP_POS ->date_Temp := newValue
       if strType = "N" then TEMP_POS ->num_Temp  := newValue
       TEMP_POS ->( netUnLock())
     endif
     baseMClose( dbInfo)
     select (S)
  endif
return rV


funcDef tpRead with str:C, strType:CU ;
  local dbInfo := tpInfo(), s := select(), curValue
  DEFAULT strType TO "C"
  basePath( dbInfo, crPosPath())
  if !file( dbInfo[ 1]) then baseCreate( dbInfo)
  if baseMOpen( dbInfo)
     TEMP_POS ->( dbSeek( str))
     if strType = "C" then curValue := rTrim( TEMP_POS ->str_Temp)
     if strType = "D" then curValue := TEMP_POS ->date_Temp
     if strType = "N" then curValue := TEMP_POS ->num_Temp
     baseMClose( dbInfo)
     select (S)
  endif
return curValue


* ---------------- Циклические операции закрытия дня ----------------
funcDef crOptions ;
   local gets, rV, timeBeg, timeEnd, timePeriod, timePer2, useCircle, str, mustWait
   timeBeg    := tpRead( "timebeg",    "C")
   timeEnd    := tpRead( "timeend",    "C")
   timePeriod := tpRead( "timeperiod", "C")
   timePer2   := tpRead( "timeperiod2","C")
   useCircle  := tpRead( "usecircle",  "N")
   mustWait   := tpRead( "wait",       "N")
   if empty( timeBeg) then timeBeg := "  :  "
   if empty( timeEnd) then timeEnd := "  :  "
   if empty( timePeriod) then timePeriod := "  "
   if len( timePeriod) < 2 then timePeriod := " " + timePeriod
   if empty( timePer2) then timePer2 := "  "
   if len( timePer2) < 2 then timePer2 := " " + timePer2
   if rV := getUp( 5, 5,;
       {{ "Настройки циклич. закрытия дня "},;
        { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
        { "Производить циклич.закрытие дня", block( useCircle), { "Нет", "Да"}},;
        { "Начало проверки   ", block( timeBeg), "99:99"},;
        { "Окончание проверки", block( timeEnd), "99:99"},;
        { "Период проверки   ", block( timePeriod), "99"},;
        { "* мин."                                      },;
        { "" },;
        { "" },;
        { "" },;
        { "Циклическая проверка связи"                  },;
        { "Период проверки   ", block( timePer2),"99"},;
        { "* мин."                                      },;
        { "" },;
        { "" },;
        { "Ожидать нажатия клавиши", block( mustWait), { "Да", "Нет"}}  })
      if time2Min( timeBeg) > time2Min( timeEnd)
        errOK( "Время начала больше времени окончания !")
      else
        if val( timePeriod) <= 0
          errOK( "Неверный период закрытия дня !")
        else
          if val( timePer2) <= 0
            errOK( "Неверный период циклич.проверки связи !")
          else
            tpWrite( "timebeg",     "C", timeBeg)
            tpWrite( "timeend",     "C", timeEnd)
            tpWrite( "timeperiod",  "C", timePeriod)
            tpWrite( "timeperiod2", "C", timePer2)
            tpWrite( "usecircle",   "N", useCircle)
            tpWrite( "wait",        "N", mustWait)
            getMustWait( .T.)
          endif
        endif
      endif
   endif
return rV

funcDef time2Min with str:C local rV := 0
  rV := val( left( str, 2)) * 60 + val( right( str, 2))
return rV

* ----------- циклич.закрытие дня ---------------------------

funcDef crTryCloseDay with cheque:CU;
  local rV := .F., curTime, timeBeg, timeEnd,;
        lastTime, timePer, inArr, outArr, str
  if tpRead( "usecircle", "N") == 2
    curTime  := left( time(), 5)
    timeBeg  := rTrim( tpRead( "timebeg",    "C"))
    timeEnd  := rTrim( tpRead( "timeend",    "C"))
    lastTime := rTrim( tpRead( "lasttime",   "C"))
    timePer  := rTrim( tpRead( "timeperiod", "C"))
    //
    // текущее время входит в интервал проверки и оно больше
    // времени последней проверки
    //
    if time2Min( curTime) >= time2Min( timeBeg) .AND. ;
       time2Min( curTime) <= time2Min( timeEnd) .AND. ;
       time2Min( curTime) >= time2Min( lastTime)
      //
      // время последнего расчета плюс интервал меньше текущего времени
      //
      if ( time2Min( lastTime) + val( timePer)) <= time2Min( curTime)
// Replaced by TMV  24-10-2003
//        inArr := { 8, 2, 4, 12, 12, 14, 1, 22, 13, 2, 24, 24, 24, 1}
		  inArr := { 8, 2, 4, 6, 6, 6, 6, 14, 1, 22, 13, 2, 24, 24, 24, 1, 6}
// Replaced by TMV  24-10-2003 номер версии с 2 на 3
//        str := "11002000000000000000"
        str := "11003000000000000000"
        if crMakeInPos( str)
         crRunPcPos()
         if crReadOutPos( CLOSE_DAY_MODE, inArr, @outArr)
          if checkError( CLOSE_DAY_MODE, outArr,,, .T.)
            rV := .T.
            cheque := crCloseCheque( outArr) + endl() + close1Cheque() + ;
                     endl() + endl() + ;
                     crCloseCheque( outArr) + endl() + close2Cheque() + ;
                     endl() + endl()
          endif
         endif
        endif
        tpWrite( "lasttime", "C", left( time(), 5))
      endif
    endif
  endif
return rV

funcDef crTryCircleCloseDay ;
  local rV, timePer, key := 0, str := ""
  crOptions()
  tpWrite( "lasttime", "C", "")
  while key <> K_ESC
    timePer  := val( tpRead( "timeperiod", "C"))
    exit if crTryCloseDay( @str)
    messageStartUp( "Ожидание начала сравнения/нажатия клавиши(продолжение)...")
    clear typeAHead
    key := inkey( timePer * 60)
    messageExit( 0)
  enddo
  if !empty( str) then printAOrder( {str})
return rV

* ---------------- циклич.проверка связи ----------------------

funcDef crTryLinkTest ;
  local rV := .F., curTime, timeBeg, timeEnd,;
        lastTime, timePer, inArr, outArr, str
    curTime  := left( time(), 5)
    lastTime := rTrim( tpRead( "lasttime2",   "C"))
    timePer  := rTrim( tpRead( "timeperiod2", "C"))
    //
    // текущее время входит в интервал проверки и оно больше
    // времени последней проверки
    //
    if time2Min( curTime) >= time2Min( lastTime)
      //
      // время последнего расчета плюс интервал меньше текущего времени
      //
      if ( time2Min( lastTime) + val( timePer)) <= time2Min( curTime)
        // rV := crLinkTest(, .T.)
        rV := checkError( LINK_TEST_MODE, crLinkTest( .T., .F.))
        tpWrite( "lasttime2", "C", left( time(), 5))
      endif
    endif
return rV

funcDef crCircleLinkTest ;
  local rV, timePer, key := 0, str := ""
  crOptions()
  tpWrite( "lasttime2", "C", "")
  while key <> K_ESC
    timePer  := val( tpRead( "timeperiod2", "C"))
    if crTryLinkTest()
       OK( "Связь с центром есть !")
       exit
    endif
    messageStartUp( "Ожидание начала сравнения/нажатия клавиши(продолжение)...")
    clear typeAHead
    key := inkey( timePer * 60)
    messageExit( 0)
  enddo
return rV

//
// проверка последней авторизации
//
funcDef getLastAuto local s := select(), rV := .T., getAuto
  getAuto := getAuto()
  rV := empty( getAuto) .OR. getAuto == "end"
  if !rV then OK( { "Предыдущая операция авторизации завершена неуспешно",;
                    "и будет отменена !"})
  select (s)
return rV

funcDef setLastAuto with prevOK:L local s := select()
  endAuto()
return NIL

static funcDef strtAuto
return tpWrite( "transaction", "C", "start")

static funcDef endAuto
  setAccepted()
return tpWrite( "transaction", "C", "end")

static funcDef clsAuto
  setAccepted()
return tpWrite( "transaction", "C", "")

static funcDef getAuto
return tpRead( "transaction", "C")

static funcDef setAccepted local s := select()
  crProtPrep( crGetDayN() )
  if crProtOpen()
     PROT ->( dbGoBottom())
     //while PROT ->( !Bof())
       if rTrim(PROT ->code) == "AUTO"
          //if !PROT ->isAccepted
				PROT->(rlock())   &&  Add by TMV  23-08-2002
            PROT ->isAccepted := .T.
				PROT->(dbunlock())   &&  Add by TMV  23-08-2002
            //exit
          //endif
       endif
      // skip -1 alias PROT
     //enddo
     crProtClose()
  endif
  select (s)
return NIL


static funcDef setCanceled local s := select()
  crProtPrep( crGetDayN() )
  if crProtOpen()
     PROT ->( dbGoBottom())
     //while PROT ->( !Bof())
       if rTrim(PROT ->code) == "AUTO"
          //if !PROT ->isReversed
				PROT->(rlock())   &&  Add by TMV  23-08-2002
            PROT ->isReversed := .T.
				PROT->(dbunlock())   &&  Add by TMV  23-08-2002
            //exit
          //endif
       endif
     //  skip -1 alias PROT
     //enddo
     crProtClose()
  endif
  select (s)
return NIL

//
// сохранение последнего чека
//
funcDef crSaveLastDoc with S:C, mode:N ;
  local docN := crGetDayN(), fileName := ""
  do case
     case mode == AUTO_MODE
       fileName := "au" + docN + ".prn"
     case mode == OPEN_DAY_MODE
       fileName := "op" + docN + ".prn"
     case mode == CLOSE_DAY_MODE
       fileName := "cl" + docN + ".prn"
  endcase
  fileName := newFPath( fileName, crPosPath())
  memoWrit( fileName, S)
return NIL

funcDef crPrintLastDoc with mode:N ;
  local docN := crGetDayN(), fileName := "", S
  do case
     case mode == AUTO_MODE
       fileName := "au" + docN + ".prn"
     case mode == OPEN_DAY_MODE
       fileName := "op" + docN + ".prn"
     case mode == CLOSE_DAY_MODE
       fileName := "cl" + docN + ".prn"
  endcase
  fileName := newFPath( fileName, crPosPath())
  S := memoRead( fileName)
  // if yesNo( "Показать документ до печати ?") then editMemo( S)
  if yesNo( "Печатать документ ?")           then printAOrder( {S})
return NIL

*------------------ просмотр протоколов работы --------------
funcDef crCardProt local aH, aC, fk, arrDir, arr := {}, fileMask, i

  fileMask := newFPath( "pr????.dbf", crPosPath())
  arrDir   := Directory( fileMask)
  for i := 1 to len( arrDir)
    if len( arrDir[ i, 1]) == 10
       aAdd( arr, "День " + subStr( arrDir[ i, 1], 3, 4))
    endif
  next
  if len( arr) > 0
    while ( i := popUp( 5, 5, arr)) > 0
       crViewThis( arrDir[ i, 1])
    enddo
  else
    messageB( "Нет файлов с протоколами работы")
  endif
return NIL

static funcDef crViewThis with fileName:C local aH, aC, fk, s := select()
   //        { "Остаток          ", {|| PROT ->balance},, {||.F.}},;
   aH := {{ "День " + subStr( fileName, 3, 4)                 },;
          { "Операция         ", {|| showOper()    },, {||.F.}},;
          { "Операция отменена", {|| showReversed()},, {||.F.}},;
          { "Печать чека      ", {|| showAccepted()},, {||.F.}},;
          { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"     } }

   aC := {{ "Дата",        block( PROT ->date    ), .F.},;
          { "Время",       block( PROT ->time    ), .F.},;
          { "Карта",       block( PROT ->pan     ), .F.},;
          { "Сумма",       block( PROT ->summa   ), .F.},;
          { "Комиссия",    block( PROT ->komSumma), .F.},;
          { "Код ошибки",  {|| showResult()      }, .F.}}
  fk := {, {}}
  crProtPrep( subStr( fileName, 3, 4))
  if crProtOpen()
     PROT ->( browseUp( 2, 2, aH, aC,;
          {|| netSkipUp()}, {|| netSkipDown()},;
          {|| netGoTop()},  {|| netGoBottom()},,,,, fk))
     crProtClose()
     select( s)
  endif
return NIL

static funcDef showOper local rV := ""
  do case
     case rTrim( PROT ->code) == "OPEN"
       rV := "Открытие дня"
     case rTrim( PROT ->code) == "CLOSE"
       rV := "Закрытие дня"
     case rTrim( PROT ->code) == "AUTO"
       rV := "Авторизация."
// Replaced by TMV 10-10-2003
*       if PROT ->operCode == "20"
       if PROT ->operCode == "21"
         rV += "Внесение наличных."
       else
         rV += "Выдача наличных.  "
       endif
     case rTrim( PROT ->code) == "BAL"  // Add by TMV 28-10-2003
       rV := "Запрос остатка."          // Add by TMV 28-10-2003
  endcase
return padR( rV, 30)

static funcDef showAccepted local rV := ""
  if rTrim( PROT ->code) == "AUTO"
    rV := "НЕ подтверждена"
    if PROT ->isAccepted then rV := "Подверждена"
  endif
return padR( rV, 30)

static funcDef showReversed local rV := ""
  if rTrim( PROT ->code) == "AUTO"
    rV := "Нет"
    if PROT ->isReversed then rV := "Да "
  endif
return rV

static funcDef showResult local rV
  rV := PROT ->packState + "/" + PROT ->result + "/" + PROT ->respCode
return rV

static func getMustWait( reAssign)
static wait
  DEFAULT reAssign TO .F.
  if empty( wait) .OR. reAssign
    wait := tpRead( "wait", "N") < 2
  endif
return wait

funcDef showErrMessage with mode:N, arrResp:A ;
  local arr, result, packState

  getResponse( mode, arrResp, @result, @packState)

  do case
     case mode == REVERSE_MODE .AND. packState == "1" .AND. ;
                  result <> "00" .AND. crIsGiveMoneyLast()
       arr := { "Ошибка " + result + ".",;
                "ОПЕРАЦИЯ ВНЕСЕНИЯ НАЛИЧНЫХ ОТМЕНЕНА." }
     case result $ "42,43,44,45,46,47,48"
       arr := { "Ошибка" + result + ".",;
                "ОШИБКА ПИН-клавиатуры",;
                "Проверьте шнур подключения ПИН-клавиатуры.",;
                "Если ошибка повторилась, позвоните в Центр Авторизации."}
     case result $ "11,20,80"
       arr := { "Ошибка " + result + ".",;
                "НЕТ КОНТАКТА."         ,;
                "Проверьте шнур подключения терминала к линии.",;
                "Повторите операцию."   ,;
                "Если ошибка повторилась - позвоните в Центр Авторизации."}
     case result $ "14"
       arr := { "Ошибка " + result + ".",;
                "НЕТ ТОНА."             ,;
                "Проверьте подключение, попросите освободить параллельный",;
                "телефон и повторите операцию." }
     case result $ "25,26"
       arr := { "Ошибка " + result + ".",;
                "НЕТ ОТВЕТА."           ,;
                "Повторите операцию."   ,;
                "Если ошибка повторилась, позвоните в Центр Авторизации."}
     case result $ "10,11,12,13,14,15,16,17"
       arr := { "Ошибка " + result + ".",;
                "НЕТ МОДЕМА."           ,;
                "Повторите операцию."   ,;
                "Если ошибка повторилась, позвоните в Центр Авторизации."}
     case result $ "40,41,60,61"
       arr := { "Ошибка " + result + ".",;
                "ОШИБКА ВВОДА ПИН."     ,;
                "Повторите операцию."   ,;
                "Если ошибка повторилась, позвоните в Центр Авторизации."}
     case result $ "65,66,67,68"
       arr := { "Ошибка " + result + ".",;
                "ОШИБКА ПРИ ЧТЕНИИ КАРТЫ."     ,;
                "Повторите операцию."   ,;
                "Если ошибка повторилась, позвоните в Центр Авторизации."}
     case packState == "1" .AND. result <> "00"
       arr := { "Ошибка " + result + ".",;
                "Повторите операцию."   ,;
                "Если ошибка повторилась, позвоните в Центр Авторизации."}
     /*
     case mode == OPEN_DAY_MODE .AND. packState == "1" .AND. result <> "00"
       arr := { "Ошибка " + result + ".",;
                "ОШИБКА ОТКРЫТИЯ ДНЯ."  ,;
                "Повторите операцию."   ,;
                "Если ошибка повторилась, позвоните в Центр Авторизации."}
     case mode == LINK_TEST_MODE .AND. packState == "1" .AND. result <> "00"
       arr := { "Ошибка " + result + ".",;
                "ОШИБКА ПРОВЕРКИ СВЯЗИ.",;
                "Повторите операцию."   ,;
                "Если ошибка повторилась, позвоните в Центр Авторизации."}*/
     endcase
     if !empty( arr) then errOK( arr)
return NIL

funcDef getResponse with mode:N, arr:A, result:CU, packState:CU
do case
  case mode == AUTO_MODE
    result    := arr[  2]
    packState := arr[ 24]
  case mode == OPEN_DAY_MODE
    result    := arr[  2]
// Replaced by TMV  28-10-2003
//    packState := arr[ 14]
    packState := arr[ 16]
  case mode == CLOSE_DAY_MODE
    result    := arr[  2]
// Replaced by TMV  28-10-2003
//    packState := arr[ 14]
    packState := arr[ 16]
  case mode == LINK_TEST_MODE
    result    := arr[  2]
// Replaced by TMV  28-10-2003
//    packState := arr[  4]
    packState := arr[  5]
  case mode == REVERSE_MODE
    result    := arr[  2]
// Replaced by TMV  28-10-2003
//    packState := arr[ 12]
    packState := arr[ 13]
  otherwise
     result := ""
     packState := ""
endcase
return !empty( result) .AND. !empty( packState)


* ------------- протоколирование запросов и ответов КЦ ------------------------
static funcDef crProtInPos with str:C ;
  local fileName, sDateTime, sWrite, firstCh, arr

  sDateTime := myDtoC( Date()) + "  " + Time()
  fileName  := ".\cr" + left( DtoS( Date()), 6) + ".mon"
  fileName  := newFPath( fileName, crPosPath())
  if !file( fileName)
     arr := { "Файл протоколов работы PC_POS. Сформирован Монитором ПБО",;
              "Имя файла : " + fileName,;
              "Время создания " + sDateTime ,;
              replicate( "=", 80), "", ""}
     eFileWrite( arr, fileName)
  endif

  sWrite    := "[ " + sDateTime + " ] Запрос : "
  firstCh   := subStr( str, 1, 1)
  do case
     case firstCh == "0" do sWrite += "OPEN__DAY"
     case firstCh == "1" do sWrite += "CLOSE_DAY"
     case firstCh == "2"
          sWrite += "AUTORIZ: "
          arr := CtoA( str, endl())
          if len( arr) < 3
            sWrite += "WRONG_INPOS"
          else
            sWrite += "[ " + "PREV " + ;
                       if( subStr( arr[ 1], 6, 1) == "0", "OK", "NO") + ",  " + ;
                       subStr( arr[ 3], 1, 12) + ",  " + ;
                       subStr( arr[ 3],15,  2) + " ]"
          endif
     case firstCh == "3" do sWrite += "RESERVED "
     case firstCh == "4" do sWrite += "LINK_TEST"
     case firstCh == "5" do sWrite += "READ_2_TR"
     case firstCh == "7" do sWrite += "REVERSE_L"
     otherwise
         sWrite += "ERROR: " + str
  endcase
  eFileWrite( sWrite, fileName)
return NIL

funcDef crProtOutPos with mode:N, arr:A ;
  local fileName, sDateTime, sWrite

  sDateTime := myDtoC( Date()) + "  " + Time()
  fileName  := ".\cr" + left( DtoS( Date()), 6) + ".mon"
  fileName  := newFPath( fileName, crPosPath())
  if !file( fileName)
     arr := { "Файл протоколов работы PC_POS. Сформирован Монитором ПБО",;
              "Имя файла : " + fileName,;
              "Время создания " + sDateTime ,;
              replicate( "=", 80), "", ""}
     eFileWrite( arr, fileName)
  endif
  sWrite    := "[ " + sDateTime + " ] Ответ  : "


  do case
     case mode == OPEN_DAY_MODE
// Replaced by TMV  28-10-2003
//       if len( arr) < 15
       if len( arr) < 17
         sWrite += "[ WRONG RESPONCE IS " + aToC( arr) + " ]"
       else
         sWrite += crDateTime( arr[ 4]) + "  " + ;
                   arr[ 14] + "/" + arr[ 2]+ "/00  " + ;
                   "DAY # " + arr[ 3]
       endif
     case mode == CLOSE_DAY_MODE
// Replaced by TMV  28-10-2003
//       if len( arr) < 14
       if len( arr) < 17
         sWrite += "[ WRONG RESPONCE IS " + aToC( arr) + " ]"
       else
         sWrite += crDateTime( arr[ 4]) + "  " + ;
                   arr[ 14] + "/" + arr[ 2]+ "/00  " + ;
                   "DAY # " + arr[ 3]
       endif
     case mode == AUTO_MODE
// Replaced by TMV  28-10-2003
//       if len( arr) < 25
       if len( arr) < 27
         sWrite += "[ WRONG RESPONCE IS " + aToC( arr) + " ]"
       else
         sWrite += crDateTime( arr[ 5] + arr[ 6]) + "  "   + ;
                   arr[ 24] + "/" + arr[ 2]+ "/" + arr[ 10]+ ;
                   "  " + arr[ 11] + "  " + arr[ 4] + "  " + ;
                   arr[ 7] + "  "
       endif
     case mode == LINK_TEST_MODE
// Replaced by TMV  28-10-2003
//       if len( arr) < 4
       if len( arr) < 5
         sWrite += "[ WRONG RESPONCE IS " + aToC( arr) + " ]"
       else
         sWrite += crDateTime( arr[ 3]) + "  " + arr[ 4] + "/" + ;
                   arr[ 2] + "/00"
       endif
     case mode == REVERSE_MODE
       if len( arr) < 12
         sWrite += "[ WRONG RESPONCE IS " + aToC( arr) + " ]"
       else
         sWrite += crDateTime( arr[ 3]) + "  " + ;
                   arr[ 12] + "/" + arr[ 2] + "/00" + "  "
       endif
  endcase
  eFileWrite( sWrite, fileName)
return NIL


* -------- удаление старых протоколов работы
static funcDef crDeleteCR ;
  local curDate, protDate, arrDir, curDay, lastDate, i, fileMask
  lastDate := tpRead( "last_cr_erased", "C")
  curDay := Day( Date())
  curDate  := left( DtoS( Date()), 6)
  if curDay >= 10 .AND. lastDate < curDate
    fileMask := newFPath( "cr??????.mon", crPosPath())
    arrDir   := Directory( fileMask)
    for i := 1 to len( arrDir)
      if len( arrDir[ i, 1]) == 12
         protDate := subStr( arrDir[ i, 1], 3, 6)
         if protDate < curDate
            fErase( newFPath( arrDir[ i, 1], crPosPath()))
         endif
      endif
    next
    tpWrite( "last_cr_erased", "C", curDate)
  endif
return NIL

// Add by TMV 22-10-2003
***************************************
** Запрос остатка
***************************************
funcDef crRest
if yesNo( "Запросить остаток ?") then crRunAuto( , "OS", 0)
return NIL

// Add by TMV 27-10-2003
***************************************
** Список причин отказа в выдаче
***************************************
static funcDef crReasonCheck local arr := {;
       { "61", "ПРЕВЫШЕН ЛИМИТ СУММЫ" + endl() + Space( 7) + "СНЯТИЯ"},;
       { "51", "ПРЕВЫШЕН ОСТАТОК"},;
       { "01", "ВВОД ПИНа ОБЯЗАТЕЛЕН"},;
       { "55", "ПИН НЕВЕРЕН"},;
       { "04", "КАРТА ЗАДЕРЖАНА"},;
       { "33", "КАРТА ЗАДЕРЖАНА"},;
       { "43", "КАРТА ЗАДЕРЖАНА"},;
       { "07", "КАРТА ЗАДЕРЖАНА"},;
       { "41", "КАРТА ЗАДЕРЖАНА"},;
       { "02", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "30", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "56", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "60", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "63", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "66", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "79", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "99", "В ОПЕРАЦИИ ОТКАЗАНО"},;
       { "75", "ИСЧЕРПАНО КОЛИЧЕСТВО" + endl() + Space( 5) + "НАБОРОВ ПИН"},;
       { "65", "ПРЕВЫШЕН ЛИМИТ ПО" + endl() + "КОЛИЧЕСТВУ СНЯТИЙ"},;
       { "57", "ОПЕРАЦИЯ ЗАПРЕЩЕНА" + endl() + Space( 5) + "ДЛЯ КАРТЫ"},;
       { "54", "ИСТЕК СРОК ДЕЙСТВИЯ КАРТЫ"}}
return arr

// Add by TMV 27-10-2003
***************************************
** Выдать причину отказа в выдаче
***************************************
funcDef getReasonFali with sCode:CU local rV := "", i := 0
DEFAULT sCode TO ""

i = aScan( crReasonView(), {|x| at( sCode, x[1]) > 0 })
if i > 0 then rV = crReasonView()[i, 2]
return rV

// Add by TMV 27-10-2003
***************************************
** Список причин отказа в выдаче
***************************************
static funcDef crReasonView local arr := {;
       { "", {"Позвоните в центр авторизации для выяснения причин."}},;
       { "61", {"ПРЕВЫШЕН ЛИМИТ СУММЫ СНЯТИЯ.",;
					 "Предложите повторить операцию с меньшей суммой."}},;
       { "51", {"ПРЕВЫШЕН ОСТАТОК",;
					 "Предложите повторить операцию с меньшей суммой."}},;
       { "01", {"ВВОД ПИНа ОБЯЗАТЕЛЕН"}},;
       { "55", {"ПИН НЕВЕРЕН"}},;
       { "04,07,36,37,38",;
		 	{"КАРТА ЗАДЕРЖАНА"}},;
       { "33", {"КАРТА ЗАДЕРЖАНА","Истек срок ее действия."}},;
       { "41,43", {"КАРТА ЗАДЕРЖАНА ПО ПРИЧИНЕ МОШЕННИЧЕТСВА",;
					 "ПРИМИТЕ МЕРЫ К ЗАДЕРЖАНИЮ КЛИЕНТА!"}},;
       { "02", {}},;
       { "03,06,12,13,14,15,21,25,30,31,60,63,66,76,89,91,94,96",;
		 	{"Позвоните в центр авторизации для выяснения причин."}},;
       { "56", {}},;
       { "11,19,77,79,80,82,83,84,85,95",;
		 	{"Ошибка при транзакции. Повторите операцию."}},;
       { "99", {}},;
       { "75", {"ИСЧЕРПАНО КОЛИЧЕСТВО НАБОРОВ ПИН"}},;
       { "65", {"ПРЕВЫШЕН ЛИМИТ ПО КОЛИЧЕСТВУ СНЯТИЙ"}},;
       { "57,58,62", {"ОПЕРАЦИЯ ЗАПРЕЩЕНА ДЛЯ ДАННОЙ КАРТЫ"}},;
       { "54", {"ИСТЕК СРОК ДЕЙСТВИЯ КАРТЫ"}}}

aEval( arr, {|x| aSize(x[2], Len(x[2])+1), aIns(x[2], 1),;
				 x[2,1] := "В ОПЕРАЦИИ ОТКАЗАНО"} )
return arr