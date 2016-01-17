*=============================================================================
*
*                   Программа : mon.exe
*                   Файл      : basecred.prg
*                   Автор     : Цейтлин П.М.
*
*                   Микрокредиты
*
*-----------------------------------------------------------------------------
*
* 15-11-2002 TMV
* Изменена схема проводок при выдаче микрокредита - добавлена новая проводка.
*  Ф-ии credNew()
*
* 22-11-2002 TMV
* Исправлена печать рас. ордера при выдаче микрокредита в ф-ии credNew()
*
* 27-11-2002 TMV
* Добавлен новый док-т при выдаче кредита - поручение на перечисление в
*  ф-ии credNew()
*
* 28-11-2002 TMV
* Заменена операция 1001 по выдаче кредита с ТПС на 1060 (7032). Изменено
*  назначение платежа в поручении на перевод. Ф-ия credNew().
* 
* 02-12-2002 TMV
* Изменена схема проводок при погашении микрокредита - добавлена новая
*  проводка 2060 (7033).
*  Ф-ии credNew(), credRetPcts().
*  Новая ф-ия getPensSchet()
*
* 05-12-2002 TMV
* Исправлены ошибки при печати док-ов при выдаче и возврате ссуды и проц.
*  Ф-ии credNew(), credRetCred(), credRetPcts()
*
* 09-12-2002 TMV
* При выдаме ссуды операция 7027 заменена на 6031. Ф-ия credNew()
*  Операция 2003 - на 6136. Ф-ия credRetCred()
*  Операция 2004 - на 6137. Ф-ия credRetPcts()
*
* 11-12-2002 TMV
* Проценты по ссуде в SUMP будут иметь код 7005 вместо 6137
* Ф-ии credRetPcts()
*
* 15-12-2002 TMV
* Поправлены проц-ры расчета оставшейся ссуды и процентов credRetPcts()
*
* 17-12-2002 TMV
* Операции 2060 и 6136/6137 поменены местами.
*  credRetPcts(), credRetCred()
*--ОТМЕНЕНО
*
* 24-12-2002 TMV
* Запрещена выдача ссуды в случае, когда разрешено изменение лимита и у
*  клиента нет поручения на перевод с ТПС - credNew()
* Изменено назначение платежа при выдаче ссуды.
* Ф-ии credZab(), credNew()
*
* 10-01-2003  TMV
* Восстановлена старая схема проводок по выдаче ссуды на Отделениях.
* Ф-ии credNew(), credRetPcts(), credRetCred(), 
* Добавлена проверка наличия выдачи и погашения ссуды в один день -
*  новая ф-ия chkCredInOut()
*
* 03-11-2003  TMV
* Сделана возможность выдать кредит по новой схеме в тот же день, когда старый
* кредит погашен.
* Ф-ия credLineNoStrah()
* 
* 11-11-2003  TMV для Питера
* Исключена возможность выдачи кредита по старой схеме на счет для
* нестраховой схемы.
* Ф-ия credNew()
*
*
* 01-12-2003  TMV
* Изменено кол-во экземпляров договора и приложения к нему - теперь 2 экз.
* Ф-ии prnCrdLinPril(), prnCrdLinDog()
*
* 29-07-2007 TMV
* Изменение карточных счетов на 45509
* Ф-ии credZab().
*
* 02-12-2004 TAT
* В назначении платежа: 'Выплата средств с текущего пенсионного счета'
* заменено на "Выплата со вклада до востребования"
*
* 05-02-05 TAT
* credRetPcts() - изменения в связи с введением просроченных процентов
* _isProsrCrd()
* _isProsrPct()
*
* Во всех ордерах печатаем реальную дату возврата кредита (по kr_tprou.dbf):
* crednew()
* credzab()
* credNewNoStrah()
*
* 25.05.2005 OVD
* Изменения по возврату микрокредитов
* credRPcts() - показываем просроченные проценты
*
* 20.06.2005 OVD
* credRetMkPr() - заносит данные по возврату микрокредитов
*


#include "clippExt.ch"
#include "common.ch"
#include "inkey.ch"
#include "getexit.ch"

memVar GetList
static limitSum, pensSum, dopSum, pctSum, strSum, minPens

//
// функции работы с базой ставок по кредитам
//
func credInfo
static dbInfo := { "KR_TPROU.DBF", "CRED_PCT",;
                  {{ "tip",      "C",  2, 0},;
                   { "max_sum",  "N", 15, 0},;
                   { "max_srok", "N", 15, 0},;
                   { "procent",  "N",  7, 2},;
                   { "proc_str", "N",  7, 2},;
                   { "min_str",  "N", 15, 2},;
                   { "max_C",    "N", 15, 0}},;
                  {{, "tip + Str( max_Srok,15,0) + Str( max_Sum,15,0)"}}, 0}
return dbInfo

funcDef credPath with newPath:CU
return basePath( credInfo(), newPath)

funcDef credOpen
return baseMOpen( credInfo())

funcDef credClose
return baseMClose( credInfo())

//
// максимальная сумма по кредиту
//
funcDef credMaxSum with curTip:CU local rV := 0, maxCred := 0
   DEFAULT curTip TO "03"

   CRED_PCT ->( dbSeek( curTip, .T.))
   while CRED_PCT ->tip == curTip .AND. CRED_PCT ->( !Eof())
         maxCred := max( maxCred, CRED_PCT ->max_C)
         rV := max( rV, CRED_PCT ->max_Sum)
         skip 1 alias CRED_PCT
   enddo
   if rsKasHas() .AND. curTip $ "03,08" then rV := max( rV, maxCred)
return rV

funcDef rsKasHas local rV := .F.
   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 1))
   if CONTRACT ->( dbSeek( CLIENTS ->code + "76", .F.))
      while CONTRACT ->code == CLIENTS ->code .AND. CONTRACT ->code2 = "76"
         if CONTRACT ->code3   == "76"         .AND. ;
            CONTRACT ->status  == 1
            rV := .T.; exit
         endif
         skip 1 alias CONTRACT
      enddo
   endif
   CONTRACT ->( baseRestPos())
return rV

//
// Процентная ставка по сумме
//
funcDef credPtr with theSumma:N, theSrok:N, curTip:CU ;
        local tmpSrok, tmpDate, arr

   DEFAULT curTip TO "03"

   CRED_PCT ->( dbSeek( curTip + Str( theSrok, 15), .T.))
   if CRED_PCT ->tip <> curTip then CRED_PCT ->( netSkipUp())
   if CRED_PCT ->tip == curTip
      tmpSrok := CRED_PCT ->max_Srok

      CRED_PCT ->( dbSeek( curTip + Str( tmpSrok, 15) + Str( theSumma, 15), .T.))
      if CRED_PCT ->max_Srok <> tmpSrok  then CRED_PCT ->( netSkipUp())

      arr := CRED_PCT ->( od_Gather())
   endif
return arr

//
// массив процентных ставок по срокам
//
funcDef credAStav with theSumma:N, curTip:CU local arr := {}
   DEFAULT curTip TO "03"

   CRED_PCT ->( baseSavePos())
   CRED_PCT ->( netGoTop())
   while CRED_PCT ->( !Eof())
         if CRED_PCT ->max_Sum == theSumma .AND. CRED_PCT ->tip == curTip
            aAdd( arr, { CRED_PCT ->max_Srok, CRED_PCT ->Procent})
         endif
         skip 1 alias CRED_PCT
   enddo
   //
   // сортируем по сроку
   //
   aSort( arr,,, {|x, y| x[ 1] < y[ 1]})
   CRED_PCT ->( baseRestPos())

return arr

//
// формирование массива процентных ставок для печати
//
funcDef credAText with aStav:A, curPct2:UA local arr := {}, i, str

   DEFAULT curPct2 TO {}
   if len( aStav) == 1 .OR. confUseAgent()
      str := "при полном возврате кредита по ставке " + nStr( aStav[ 1, 2]) + ;
             " % годовых за весь срок кредита;"
      addStr( arr, str, 78)

      str := "Процентная ставка при полном возврате кредита : " + ;
              nStr( aStav[ 1, 2]) + " % годовых за весь срок кредита;"
      addStr( curPct2, str, 78)
   else
      if confUseNewCred()
         str := "- при полном возврате ссуды по "  + ;
                DtoC( setCurDate() + aStav[ 1, 1]) + ;
                " включительно - по ставке "       + ;
                nStr( aStav[ 1, 2]) + " % годовых за весь срок ссуды;"
         addStr( arr, str, 78)

         str := "Процентная ставка при полном возврате ссуды по " + ;
                DtoC( setCurDate() + aStav[ 1, 1]) + ":  "       + ;
                nStr( aStav[ 1, 2]) + " % годовых за весь срок ссуды;"
         AddStr( curPct2, str, 78)

         for i := 2 to len( aStav) - 1
          str := "- при полном возврате ссуды c "            + ;
                 DtoC( setCurDate() + aStav[ i -1, 1] +1)    + ;
                 " до " + DtoC( setCurDate() + aStav[ i, 1]) + ;
                 " - по ставке " + nStr( aStav[ i, 2])       + ;
                 " % годовых за весь срок ссуды;"
          addStr( arr, str, 78)
          str := "Процентная ставка при полном возврате ссуды с "    + ;
                 DtoC( setCurDate() + aStav[ i -1, 1] + 1)           + ;
                 " до " + DtoC( setCurDate() + aStav[ i, 1]) + ":  " + ;
                 nStr( aStav[ i, 2]) + " % годовых за весь срок ссуды;"
          AddStr( curPct2, str, 78)
         next
         if i <= len( aStav)
            str := "- при полном возврате ссуды c " + ;
                   DtoC( setCurDate() + aStav[ i -1, 1] + 1) + ;
                   " - по ставке " + nStr( aStav[ i, 2]) + ;
                   " % годовых за весь срок ссуды."
            addStr( arr, str, 78)
            str := "Процентная ставка при полном возврате ссуды с "    + ;
                   DtoC( setCurDate() + aStav[ i -1, 1] + 1) + ":  "   + ;
                   nStr( aStav[ i, 2]) + " % годовых за весь срок ссуды."
            AddStr( curPct2, str, 78)
         endif

      else
      str := "- при погашении ссуды до " + DtoC( setCurDate() + aStav[ 1, 1]) + ;
             " включительно по ставке " + nStr( aStav[ 1, 2]) + " % годовых"
      aAdd( arr, str)
      for i := 2 to len( aStav) - 1
          str := "- при погашении ссуды c " + DtoC( setCurDate() + aStav[ i -1, 1]) +;
                 " до " + DtoC( setCurDate() + aStav[ i, 1])                        +;
                 " по ставке " + nStr( aStav[ i, 2]) + " % годовых"
          aAdd( arr, str)
      next
      if i <= len( aStav)
         str := "- при погашении ссуды после " + DtoC( setCurDate() + aStav[ i -1, 1]) + ;
                " по ставке " + nStr( aStav[ i, 2]) + " % годовых"
         aAdd( arr, str)
      endif
      endif
   endif
return arr

funcDef chkPrintOrder local rV := .T.
  SUMS ->( baseSavePos())
  ACC  ->( baseSavePos())
  SUMS ->( dbSetOrder( 1))
  ACC  ->( dbSetOrder( 2))
  if ACC ->( dbSeek( CLIENTS ->code))
     while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
       if isPens( ACC ->PRIZ)
          if SUMS ->( dbSeek( ACC ->schet))
             while SUMS ->schet == ACC ->schet .AND. SUMS ->( !eof())
               if SUMS ->typeOf $ "9001,9002"
                  rV := .F.
                  exit
               endif
               skip 1 alias SUMS
             enddo
          endif
       endif
       exit if !rV
       skip 1 alias ACC
     enddo
  endif
  ACC  ->( baseRestPos())
  SUMS ->( baseRestPos())
return rV

//
// список пенсионных счетов
//
funcDef accPensSchet with arr:AU, arr2:AU local rest

   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 2))
   arr  := {}
   arr2 := {}
   ACC ->( netSeek( CLIENTS ->code))
   while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())
         if isPens( ACC ->priz) .AND. ACC ->typeOf == "00"
            rest := sumRest( ACC ->schet, setCurDate())
            if rest < 10
               aAdd( arr, "Счет " + ACC->schet + " Остаток " + lTrim(mStr(rest)) )
               aAdd( arr2, { ACC ->( recNo()), val( SubStr( ACC ->buffer, 2, 2)),;
                             ACC ->schet} )
            endif
         endif
         skip 1 alias ACC
   end
   ACC ->( baseRestPos())
return NIL

funcDef logErr with str:CA local crFile := ".\cr_err.log"
   errOK( str)
   eFileWrite( "Счет : " + ACC ->schet, crFile)
   eFileWrite( str, crFile)
   eFileWrite( endl() + endl(), crFile)
return str

//
// оформление нового кредита : для текущего клиента
// счет еще не открыт !
//
funcDef credNew with curSchet:CU ;
        local rV := .F., s1, s2, aGets, credSum := 0 ,;
              docNumb, s, n := 1, aOrder, ro, sim    ,;
              credOper, strOper, credNazn, strNazn   ,;
              theOrder, arr, arr2, nPens, nextPens   ,;
              ordInc, ordOut, kasRecNo               ,;
              balSchetPtr := 1, arrName, arrCode     ,;
              kasRest, printAgain := .T., mustSave   ,;
              curPct2, curPcts, aStav,;
              sPensSchet:='', numPoruc, ll, ;
              addr := razbivka( cliFullAddress(), {{ 35, "L"}, { 95, "L"}})

   credOper := opDecode( "1007")
   strOper  := opDecode( "3006")

   if setUpValue('PMKL') == '1'
       if !transExist( cliCode(), "54")
           logErr('У клиента нет поручений на перевод с ТПС.')
           return rV
       endif
   endif

   credNazn := "Выдача социального кредита"

   strNazn := "Тарифный сбор"

   s := select()
   if credOpen()
      //
      // подготовка сумм
      //
      limitSum := kasRest( "003")
      if rsKasHas() then limitSum := 999999999.99
      kasRest  := kasRest( "999")
      pensSum  := CLIENTS ->credSumma/ 100
      dopSum   := credMaxSumma()
      s1       := aMin( { pensSum, dopSum, limitSum})
      s1       := int( s1)
      //
      // определим процентную ставку
      //
      credPtr( s1, 60)
      pctSum   := int( round( s1 * CRED_PCT ->Procent * 60/ 365/ 100, 0))
      //
      // страховой взнос
      //
      strSum := max( s1 * CRED_PCT ->proc_Str/ 100, CRED_PCT ->min_Str)
      s2     := aMin( { pensSum - pctSum - 0, dopSum, limitSum})
      s2     := int( s2)

      arrName := {}; arrCode := {}; balSchetPtr := 1
      vidArray( "15 3  ", @arrName, @arrCode, .F.)

      nPens = aScan( arrCode, {|x| Left( x[4], 7) == ACC_CRD_NOSTRAH} )
      if nPens > 0
          aDel( arrCode, nPens)
          aSize( arrCode, Len(arrCode) - 1 )
          aDel( arrName, nPens)
          aSize( arrName, Len(arrName) - 1 )
      endif
      //
      // пенсионные счета
      //
      accPensSchet( @arr, @arr2)
      nPens := 1
      aGets  := {{ "Текущий лимит отделения " + mStr( kasRest( "003"))         },;
                 { "Остаток по кассе        " + mStr( kasRest)                 },;
                 { "Допустимая сумма : ", block( s2),"999 999 999 999", {||.F.}},;
                 { ""                                                          },;
                 { "Пенсионный счет  : ", block( nPens), aClone( arr),,{|g|setMinPens(g, arr, arr2)}},;
                 { "Тип кредита      : ", block( n), aCredTypes(),,              ;
                                     {|g|chkCredType(g,@aGets,@s2,@credSum,@nPens, arr)}},;
                 { "Сумма кредита    : ", block( credSum),,,                     ;
                                     {|g| chkCredSum(g,@aGets,@s2,@credSum)}}}
      if confUseAgent() then aGets[ 2] := { ""}
      if len( arrName) > 1
         aAdd( aGets, { "Тип кредита      : ", block( balSchetPtr), arrName})
      endif
      //
      // оформим кредит
      //
      if s2 >0
         if empty( arr)
            logErr( "Нет пенсионных счетов с остатком < 10-00 руб.")
         else
         SorryPiter()
         if getUp( 5, 5, aGets) .AND. len( arrName) >0
            minPens := sumPMinPens( arr2[ nPens, 3])

            sPensSchet = arr2[ nPens, 3]  // Add by TMV  15-11-2002
            //
            // откроем счет
            //
            if noYes( { "Оформить кредит на  сумму " + mStr( credSum)})
               if credGoCred( curSchet, arrCode[ balSchetPtr, 5],;
                      arrCode[ balSchetPtr, 4])
                  docNumb := confCredDoc()
                  credNazn   += ".Заявление N " + docNumb
                  strNazn    += ".Заявление N " + docNumb

                  ordOut := theOrder := ordIncOut( setCurDate())
                  ordInc             := ordIncIn( setCurDate())
                  //
                  // определим номер поручения
                  //
                  TEMP ->( dbSetOrder( 1))
                  if TEMP ->( dbSeek( "NUM_PORUC"))
                     numPoruc := allTrim( Str( TEMP ->num_temp + 1))
                     if TEMP ->( mRLock())
                        TEMP ->num_temp := val( numPoruc)
                        TEMP ->( netUnLock())
                      endif
                  else
                     TEMP ->( mAppend())
                     TEMP ->comment := "NUM_PORUC"
                     TEMP ->num_temp := 1
                     TEMP ->( netUnLock())
                     numPoruc := "1"
                  endif

                  //
                  // определим месяц следующей пенсии
                  //
                  nextPens := setCurDate() + CRED_PCT ->MAX_SROK
                  strSum   := max( credSum * CRED_PCT ->proc_Str/ 100, CRED_PCT ->min_Str)
                  strSum   := val( Str( strSum))
                  curPcts  := credAText( credAStav( CRED_PCT ->max_Sum), @curPct2)

                  servDummy( {{ "@docNumb" , docNumb        },;
                              { "@nextPens", DtoC( nextPens)},;
                              { "@curPcts",  curPcts        },;
                              { "@curPct2",  curPct2        },;
                              { "@summaRSO", lTrim( mStr( strSum)) + " ( " + rSumma( strSum) + " )"},;
                              { "@schet716", ACC ->schet    },;
                              { "@sch711",   arr2[ nPens, 3]},;
                              { "@datOpen",  setCurDate()   },;
                              { "@ordInc",   ordInc         },;
                              { "@szsk",     confSZSK()     },;
                              { "@ordOut",   ordOut         },;
                              { "@addr1",    addr[ 1]       },;
                              { "@addr2",    addr[ 2]       },;
                              { "@otdnumb",  confOtdNumb()  },;
                              { "@nPoruc",   numPoruc       }})


                  if kasRest( "999") >= credSum
                     if if( rsKasHas(), .T., kasRest( "003") >= credSum)
                        //
                        // оформление микрокредита
                        //
                        SRSumma( credSum)
                        mustSave := mustSaveOpen( .T.)
                        operGetInfo( credOper, @ro, @sim)
                        while printAgain
                          //
                          // заява на выдачу кредита
                          //
                          if confUseNewCred()
                             printOpen( if( n == 1, "cr1n", "cr2n"), 1)
                          else
                             printOpen( if( n == 1, "cr1", "cr2"))
                          endif
                          SaveLastOrder( { getSavedOpen()})
                          if confUseAgent() // Add by TMV 08-01-2003
                             aOrder := memKred( ACC ->schet, sPensSchet, credSum,;
                                'Выдача социальноой ссуды по заявлению N '+docNumb) // Edit by TMV  05-12-2002
altd()
                             printAOrder( aOrder)
                             saveLastOrder( aOrder, .F.)
                          endif
                          //
                          // ордер etc
                          //
                          aOrder := homeOneOrder( cliFullName(), cliFullAddress(),;
                            cliDocum(), theOrder, ;
                             if(confUseAgent(), /*'Выплата средств с текущего пенсионного счета' 02-12-2004 TAT*/ "Выплата со вклада до востребования",;
                              credNazn), ro, sim, credSum, .T., .T., setCurDate(),;
                                   , , , if(confUseAgent(), sPensSchet, nil)) // Add by TMV  22-11-2002
   // Add <confUseAgent()> by TMV  08-01-2003 для работы в режиме Отделения
                          printAOrder( aOrder)
                          saveLastOrder( aOrder, .F.)
                          operGetInfo( opDecode( "3006"), @ro, @sim)
                          aOrder := vznos_nal( ordInc,setCurDate(), cliFullName(),;
                             confStrSchet(), strSum,;
                             confBankName()+','+ rTrim(confFilial()), strNazn    ,;
                             confKassa(), sim,ro,,.T.)
                          printAOrder( aOrder)
                          saveLastOrder( aOrder, .F.)
                          printAgain := !yesNo( "Документы распечатаны правильно ?")
                        enddo

                        //Если печать на принтер 
                        if confPrintTo() == 1 .OR. confPrintTo() == 3 
                            set device to printer  //Прогон бумаги на 6.5 см
                            set printer to
                            set console off
                            for ll=1 to 5
                               @ prow()+ll, 1 say space(1) + chr(13) + chr(10)
                            next
                            set printer off
                            set console on
                            set device to screen
                        endif

                        mustSaveOpen( mustSave)
                        SRSumma( 0)
                        servDummy( {})
                        sumUseAppend(  .T.)
                        wiscUseAppend( .T.)
   //   для в режиме Отделения
                        if sumWrite( ACC ->schet, - credSum, setCurDate(),;
                                                                           if(confUseAgent(), opDecode( "6031"), opDecode( "1007")),;
                                                                           theOrder,,docNumb)
                           inkey( 0.2)
   //   для того, чтобы не делать вторую проводку для Отделения
                         if !confUseAgent() .Or. sumWrite( sPensSchet, - credSum,;
                                                                           setCurDate(), opDecode( "1060"), theOrder,,docNumb)
                           inkey( 0.2)

                           if sumWrite( confStrSchet(), strSum, setCurDate(), opDecode( "3006"), ordInc,,docNumb)
                              inkey( 0.2)
                              if wiscAppend()
                                 WISC ->type   := "998"
                                 WISC ->schet  := ACC ->schet
                                 WISC ->buffer := docNumb             + ";" + ;
                                               if( n == 2, '2', '03') + ";" + ;
                                                  myDtoC( setCurDate()) + ";" + ;
                                                  nStr( ordInc)       + ";" + ;
                                                  nStr( strSum)       + ";" + ;
                                                  arr2[ nPens, 3]
                                 WISC ->( netUnLock())
                              else
                                 errLog( { "Поручение не сформировано !"        ,;
                                          "Запишите номер счета и ФИО клиента" ,;
                                          "и обратитесь на узел связи !!!" })
                              endif
                           else
                              errLog( { "Проводка по оплате страхового взноса не сделана" ,;
                                       "и поручение не сформировано !!!"    ,;
                                       "Запишите номер счета и ФИО клиента" ,;
                                       "и обратитесь на узел связи !!!" })
                           endif
                                                            endif  // Add by TMV  15-11-2002
                        else
                           errLog( "Проводка по выдаче кредита не сделана !")
                        endif
                        sumUseAppend(  .F.)
                        wiscUseAppend( .F.)
                   
                     else
                        errLog( "Вы пытаетесь снять сумму большую, чем лимит отделения !!!")
                     endif
                  else
                     errLog( "Вы пытаетесь снять сумму большую, чем остаток по кассе !!!")
                  endif
               endif
         endif
         endif
      endif
      else
        logErr( "В выдаче кредита отказано !")
      endif
      credClose()
      select ( s)
   endif

return rV

funcDef SorryPiter
//   messageB( "Ъ.. !!!")
return NIL

//
// установка на счет
//
static funcDef credGoCred with curSchet:CU, balSchet:C, curPriz:C ;
   local rV := .F., index

   DEFAULT curSchet TO ""

   if empty( curSchet) .OR. ( left( curSchet, 5) <> balSchet)
      if accNewSchet( balSchet, setCurDate(), curPriz)
         ACC ->code    := CLIENTS ->code
         ACC ->priz    := curPriz
         ACC ->lEdited := .T.
         rV := .T.
      endif
   else
     index := ACC ->( indexOrd())
     ACC ->( dbSetOrder( 1))
     rV := accSeek( curSchet)
     ACC ->( dbSetOrder( index))
   endif
return rV .AND. msgShowMessage()


function aCredTypes
   static types := { "Процентный", "Беспроцентный" }
return types


funcDef setMinPens with g:O, arr:A, arr2:A local rV := .T., i

   minPens := 0.00
   i := aTrimmedScan( arr, g:varGet)
   if i > 0 then minPens := sumPMinPens( arr2[ i, 3])

return rV

funcDef chkCredType ;
   with g:O, aGets:A, s2:N, credSum:N, nPens:N, arr:A ;
   local rV := .T.

   if allTrim( g:varGet()) == allTrim( aCredTypes()[ 2])
      if ( rV := noYes({ ;
                     "Беспроцентный кредит может быть выдан                    ",;
                     "лишь в экстренных случаях, как то:                       ",;
                     "смерть родственника, оформление билетов и т.п.           ",;
                     "Hа отделении должен остаться документ (или его копия),   ",;
                     "подтверждающий право на получение беспроцентного кредита.",;
                     "Выдавать беспроцентный кредит?" }))

          s2 := aMin( { pensSum - 0, limitSum})
          s2 := int( s2)
      endif
   else
      s2 := aMin( { pensSum - pctSum - 0, dopSum, limitSum})
      s2 := int( s2)
   endIf
   credSum := GetList[ 4]:varGet()
   nPens   := aTrimmedScan( arr, getList[ 2]: varGet)
   getReProc( aGets)

return rV

//
// проверка суммы кредита
//
funcDef chkCredSum with g:O, aGets:A, s2:N, credSum:N local rV := .T.,;
        isLineNoStrah := useLineNoStrah()        // Add by TMV  05-06-2003

   if g:varGet() < 10
      if lastKey() <> K_UP .AND. lastKey() <> K_SH_TAB
         logErr( "Минимальная сумма кредита - 10-00 руб.")
         rV := .F.
      endif
      if g:varGet < 0
         messageB( "Отрицательная сумма ?!!")
         rV := .F.
      endif
   else
      if g:varGet() <= s2
         rV := .T. // все нормально
      else
         if !isLineNoStrah .And.;        // Add by TMV  05-06-2003
                                   g:varGet() > s2 .AND. g:varGet() < aMin( { minPens, dopSum, limitSum})
            messageB( "Здесь должна печататься справка !")
            rV := .T. // может выдать а может и нет
         else
            logErr( "В выдаче кредита на такую сумму вам отказано !")
            rV := .F.
         endif
      endif
   endif
   if rV
      if g:varGet() > if( rsKasHas(), 999999999.99, kasRest( "003"))
         messageB( "Вы ввели сумму больше, чем лимит отделения !!!")
         rV := .F.
      endif
      if g:varGet() > kasRest( "999")
         messageB( "Вы ввели сумму больше, чем остаток по кассе !!!")
         rV := .F.
      endif
   endif
return rV

// 05-02-05 TAT Определение счета просрочки кредитов
funcDef _isProsrCrd with schet:C
return left( schet, 5) = "45815"

// 05-02-05 TAT Определение счета просрочки процентов
funcDef _isProsrPct with schet:C
return left( schet, 5) = "45915"

// OVD Учет суммы начисленных процентов за пользование кредитами
funcDef _isSumPctKr with schet:C
return left( schet, 5) = "47427" 

//
//  определение процентов
//
funcDef credRPcts with recNo:NU ;
   local arr, arr2, n := 1, fk, rV := 0.00,;
         aOrder, ordInc, ro, sim, strRest,;
         sPensSchet := getPensSchet( CLIENTS->CODE ), ;
         accprosrcrd := ".", prizprosrcrd := "", dtprosrcrd, restprosrcrd := 0, arrprosrpct := {}, ;
         i, l, rest := 0.00, restPct

   DEFAULT recNo TO 0

   // 05-02-05 TAT если приходим из просроченного договора - работаем со всем
   // списком имеющихся кредитов
    if ( recNo > 0)
       ACC ->( baseSavePos())
       ACC ->( dbGoTo( recNo))
       if _isProsrCrd( ACC ->schet)
          recNo = 0
       endif
    endif

   // 05-02-05 TAT анализ просроченных кредитов и процентов - проводится всегда!
    ACC ->( baseSavePos())
    ACC ->( dbSetOrder( 2))

    if accSeek( CLIENTS ->code)
       while CLIENTS ->code == ACC ->code .AND. ACC ->( !Eof())
           if _isProsrPct( ACC ->schet) .OR. ( _isSumPctKr( ACC ->schet) .AND. ACC ->rest <> 0)  /// OVD
               rest = -sumRest( ACC ->schet, setCurDate()) 
               if rest <> 0
                  aAdd(arrprosrpct, {ACC ->schet})  
               endif
           endif
           // просрочки кредитов просто суммируем, чтобы узнать, есть ли они
           if _isProsrCrd(ACC ->schet)
               accprosrcrd = ACC ->schet 
               prizprosrcrd = ACC ->PRIZ 
               dtprosrcrd = ACC ->datOpen
               restprosrcrd = restprosrcrd - sumRest( ACC ->schet, setCurDate()) 
           endif
           skip 1 alias ACC
       enddo 
    endif

    if empty( recNo)
      arr := {}
      ACC ->( baseSavePos())
      ACC ->( dbSetOrder( 2))
      if accSeek( CLIENTS ->code)
         while CLIENTS ->code == ACC ->code .AND. ACC ->( !Eof())
           if (isCred() .OR. isZabCred()) 
              if !confUseAgent()
                   rest := round( sumPType( ACC ->schet, opDecode("2004")), 2)
              else
                   rest := round( sumPType( ACC ->schet, opDecode("2004"));
                                + sumPType( ACC ->schet, opDecode("6137")), 2)
              endif
              if rest <> 0
                 strRest := padL( lTrim( mStr( rest)), 13)
                 if (len(arr) == 0) .and. (len(arrprosrpct) > 0) .and. (accprosrcrd <> ".")  
                   aAdd( arr, { brwShowSchet( accprosrcrd)         ,;
                                padR( vidGetName(prizprosrcrd), 30),;
                                DtoC(dtprosrcrd)                   ,;
                                strRest                            ,;
                                ACC ->( recNo())                   ,;
                                SplitSSchet(accprosrcrd)           })
                 else 
                   aAdd( arr, { brwShowSchet( ACC ->schet)        ,;
                                padR( vidGetName( ACC ->PRIZ), 30),;
                                DtoC( ACC ->datOpen)              ,;
                                strRest                           ,;
                                ACC ->( recNo())                  ,;
                                SplitSSchet( ACC ->schet)         })
                 endif
              endif
           endif
           skip 1 alias ACC
         enddo
      endif
      ACC ->( baseRestPos())
    endif 

    if recNo > 0
      ACC ->( baseSavePos())
      ACC ->( dbGoTo( recNo))
      if !confUseAgent()
          rest := sumPType( ACC ->schet, opDecode("2004"))
      else
          rest := sumPType( ACC ->schet, opDecode("2004")) ;
                + sumPType( ACC ->schet, opDecode("6137"))
      endif
    endif

    ACC ->( baseRestPos())
    rV := rest

return rV
//
funcDef credSums with schet:CU, datSchet:D  local rV := 0.00
   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 1))
   SUMS ->( netSeek( schet + dTos( datSchet)))
   while schet == SUMS ->schet .AND. datSchet == SUMS ->ddate .AND. !SUMS->( eof())
      rV += SUMS ->summa
      skip 1 alias SUMS
   enddo
   rV := rV/ 100
return rV
//
// возврат процентов
//      
funcDef credRetPcts with summa:NU, recNo:NU ;
   local arr, n := 1, fk, rV := 0.00,;
         aOrder, ordInc, ro, sim, strRest,;
         sPensSchet := getPensSchet( CLIENTS->CODE ), ;
         accprosrcrd := ".", prizprosrcrd := "", dtprosrcrd, restprosrcrd := 0, arrprosrpct := {}, ;
         summa_, summar, i, l, DohAcc, rest := 0.00, restPct, sumCred := 0.00, ;
         credSu := 0.00, strComm

   DEFAULT recNo TO 0

   // 05-02-05 TAT если приходим из просроченного договора - работаем со всем
   // списком имеющихся кредитов
   if ( recNo > 0)
      ACC ->( baseSavePos())
      ACC ->( dbGoTo( recNo))
      if _isProsrCrd( ACC ->schet)
         recNo = 0
      endif
   endif

   // 05-02-05 TAT анализ просроченных кредитов и процентов - проводится всегда!
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 2))
   if accSeek( CLIENTS ->code)
      while CLIENTS ->code == ACC ->code .AND. ACC ->( !Eof())
          if _isProsrPct( ACC ->schet) .OR. ( _isSumPctKr( ACC ->schet) .AND. ACC ->rest <> 0) /// OVD
             rest =  -sumRest( ACC ->schet, setCurDate()) 
             if rest <> 0
                aAdd(arrprosrpct, {ACC ->schet})  
             endif
         endif
         // просрочки кредитов просто суммируем, чтобы узнать, есть ли они
         if _isProsrCrd(ACC ->schet)
             accprosrcrd = ACC ->schet 
             prizprosrcrd = ACC ->PRIZ 
             dtprosrcrd = ACC ->datOpen
             restprosrcrd = restprosrcrd - sumRest( ACC ->schet, setCurDate()) 
         endif
         skip 1 alias ACC
      enddo
   endif

   if empty( recNo)
      arr := {}
      ACC ->( baseSavePos())
      ACC ->( dbSetOrder( 2))
      if accSeek( CLIENTS ->code)
         while CLIENTS ->code == ACC ->code .AND. ACC ->( !Eof())
           if ( isCred() .OR. isZabCred()) 
              if !confUseAgent()
                   rest := round( sumPType( ACC ->schet, opDecode("2004")), 2)
              else
                   rest := round( sumPType( ACC ->schet, opDecode("2004"));
                                + sumPType( ACC ->schet, opDecode("6137")), 2) // 05-02-2005 TAT
              endif
              if rest <> 0
                 strRest := padL( lTrim( mStr( rest)), 13)
                 if (len(arr) == 0) .and. (len(arrprosrpct) > 0) .and. (accprosrcrd <> ".")  
                   aAdd( arr, { brwShowSchet( accprosrcrd)         ,;
                                padR( vidGetName(prizprosrcrd), 30),;
                                DtoC(dtprosrcrd)                   ,;
                                strRest                            ,;
                                ACC ->( recNo())                   ,;
                                SplitSSchet(accprosrcrd)           })
                 else 
                   aAdd( arr, { brwShowSchet( ACC ->schet)        ,;
                                padR( vidGetName( ACC ->PRIZ), 30),;
                                DtoC( ACC ->datOpen)              ,;
                                strRest                           ,;
                                ACC ->( recNo())                  ,;
                                SplitSSchet( ACC ->schet)         })
                 endif
              endif
           endif
           skip 1 alias ACC
         enddo
      endif
      ACC ->( baseRestPos())
   endif 

   restPct := rest

   if recNo > 0
      ACC ->( baseSavePos())
      ACC ->( dbGoTo( recNo))
      if !confUseAgent()
          rest := sumPType( ACC ->schet, opDecode("2004"))
      else
          rest := sumPType( ACC ->schet, opDecode("2004"));
                + sumPType( ACC ->schet, opDecode("6137")) // 05-02-2005 TAT
      endif

      sumCred := summa
      rest := round( rest, 2)

      if summa < rest
         summa := summa
      else
         summa := rest 
      endif

      if rest > 0
      // 05-02-05 TAT  показываем счет просрочки в случае просроченных процентов 
         ordInc := ordIncIn( setCurDate())
         operGetInfo( if(confUseAgent(), opDecode("6137"), opDecode("2004")),;
                      @ro, @sim)
         useZabota( .T.)

         DohAcc = confPctSchet()
         if _isProsrCrd( accprosrcrd)
            DohAcc = setUpProsrPctsSchet()
         endif

         if len( arrprosrpct) > 0 
            if _isProsrPct( arrprosrpct[ 1, 1])
               DohAcc = setUpProsrPctsSchet()
            endif
         endif

         l = .T. // сделаны ли все проводки
         summar = summa
         if confUseAgent()

            useZabota( .T.)
            aOrder := vznos_nal( ordInc, setCurDate(), cliFullName()  ,;
                        sPensSchet,;
                        summa, confBankName()+','+ rTrim(confFilial()) ,;
                        "Взнос на текущий счет",;
                        confKassa(), sim, ro,,.T.)
            useZabota( .F.)
            printAOrder( aOrder)
            SaveLastOrder( aOrder)
         endif

         for i := 1 to len( arrprosrpct)
            // для проводки по погашению просрочки
            summa_ = min(- sumRest( arrprosrpct[ i, 1], setCurDate()), summar)
            // для проводки по погашению процентов
            summar = summar - summa_

            if !_isProsrPct( arrprosrpct[ i, 1])
               credSu := credSums( arrprosrpct[ i, 1], setCurDate())
               if summa <= rest .AND. left( arrprosrpct[ i, 1], 5) <> "70101" ;
                 .AND. ( restPct - credSu) >= 0
                  if summa > restPct - credSu 
                     summa_ := restPct - credSu
                     summar := summa - summa_
                  else
                     summa_ := summa
                     summar := 0.00
                  endif
               endif
            endif

            if summa_ > 0
               strComm := iif( !_isSumPctKr( arrprosrpct[ i, 1]), "Возврат просроченных процентов по социальной ссуде", ;
                    "Возврат процентов по социальной ссуде")
               if confUseAgent()
                   aOrder := memWisc2( sPensSchet, arrprosrpct[ i, 1], '', summa_,;
                             '', strComm)
                   printAOrder( aOrder)
                   SaveLastOrder( aOrder)
               else
                   useZabota( .T.)
                   aOrder := vznos_nal( ordInc, setCurDate(), cliFullName()  ,;
                                        arrprosrpct[ i, 1],;
                                        summa_, confBankName()+','+ rTrim(confFilial()) ,;
                                        "Возврат просроченных процентов",;
                                        confKassa(), sim, ro,,.T.)

                   useZabota( .F.)
                   printAOrder( aOrder)

                   SaveLastOrder( aOrder)
               endif

               // проводки по погашению просрочки отражаем на счете просроченных процентов
               // (делаем проводки, аналогичные проводкам по погашению ссуд)

               if l .and.;
                     !sumWrite( arrprosrpct[i,1], summa_, setCurDate(),;
                             if( confUseAgent(), opDecode( "2060"), opDecode( "2004")),;
                               ordInc,,;
                               "PCT:" + if( confUseAgent(), sPensSchet, ACC->Schet))

                     l = .F.
                     logErr( "Проводка по возврату процентов не сделана !")

               elseif confUseAgent() .And. l .And.;
                         !sumWrite( sPensSchet, summa_, setCurDate(),;
                         if( confUseAgent(), opDecode( "6137"), opDecode( "2004")),;
                         ordInc,, "PCT:" + arrprosrpct[i,1])  // Replaced  by TMV  02-12-2002

                      l = .F.
                      logErr( "Проводка по возврату процентов на ТПС не сделана !")
               else
                      if WISC ->( mAppend())
                         WISC ->type   := "997"
                         WISC ->schet  := arrprosrpct[ i, 1]
                         WISC ->buffer := myDtoC( setCurDate()) + ";" + ;
                                          nStr( ordInc) + ";" + ;
                                          nStr( summa_) + ";" + ;
                                          ACC ->schet
                         WISC ->( netUnLock())
                       endif

                       sumPWrite( ACC ->schet, - summa_, setCurDate(),;
                                  if(confUseAgent(), opDecode("6137"), opDecode("2004")))
               endif
           endif
         next

         if (summar > 0)
            strComm := iif( subStr( DohAcc, -3) = "500", "Возврат просроченных процентов по социальной ссуде", ;
                "Возврат процентов по социальной ссуде")
            if confUseAgent()
                aOrder := memWisc2( sPensSchet,  DohAcc, '', summar,;
                          '', strComm)
                printAOrder( aOrder)
                SaveLastOrder( aOrder)
            else
                useZabota( .T.)
                aOrder := vznos_nal( ordInc, setCurDate(), cliFullName()  ,;
                                     DohAcc,;
                                    summar, confBankName()+','+ rTrim(confFilial()) ,;
                                    "Возврат процентов",;
                                    confKassa(), sim, ro,,.T.)
                useZabota( .F.)
                printAOrder( aOrder)
                SaveLastOrder( aOrder)
            endif
         endif

         if (summar <> 0) .and. l  
            if !sumWrite( if(confUseAgent(), sPensSchet,  DohAcc),;
                   summar, setCurDate(),;
                  if( confUseAgent(), opDecode( "6137"), opDecode( "2004")),;
                  ordInc,, "PCT:" + if( confUseAgent(),  DohAcc, ACC->Schet))  // Replaced  by TMV  02-12-2002

               logErr( "Проводка по возврату процентов на ТПС не сделана !")

            elseif confUseAgent() .and.;
                   !sumWrite( ACC->Schet,  summar, setCurDate(),; 
                              opDecode( "2060"),;
                              ordInc,, "PCT:" + sPensSchet)

               logErr( "Проводка по возврату процентов не сделана !")
            else
               if WISC ->( mAppend())
                  WISC ->type   := "997"
                  WISC ->schet  := ACC ->schet
                  WISC ->buffer := myDtoC( setCurDate()) + ";" + ;
                                   nStr( ordInc)       + ";" + ;
                                   nStr( summar)
                  WISC ->( netUnLock())
                endif

                sumPWrite( ACC ->schet, - summar, setCurDate(),;
                         if( confUseAgent(), opDecode( "6137"), opDecode( "2004"))) 
            endif
         endif
      endif
   endif

   ACC ->( baseRestPos())

   rV := sumCred - rest

return rV
//
funcDef credSSchet with schet:C, priz:C local rV
   rV := left( getBalSchet( schet), 3) $ "455,457" .AND. ;
         left( priz, 6) $ "15 3  "
return rV
//
// возврат кредита
//
funcDef credRetCred ;
  local rest := 0.00, restPct := 0.00, restSum := 0, summa := 0.00, ; 
        arr := {}, arr2, n := 1, rV := .F., aOrder, ;
        ordInc, ro, sim, fk, recNo, fullName, s, ;
        sPensSchet := getPensSchet( CLIENTS->CODE ), ;
        sBuff2004 := "", ZPctSum := 0, restPctS := 0.00, jj

   if confUseAgent() .And. empty( sPensSchet )
      messageB( "Не найден пенсионный счет клиента !")
      return NIL
   endif

   arr := brwGetRSchet( 3)

   if len( arr) > 0
//    recNo := brwViewSchet( arr)
//      asort( arr, , ,{|x, y| x[ 1] < y[ 1]})
      recNo :=  arr[ 1, 5]
      if recNo > 0

         ZPctSum := credRPcts( recNo)
    
         ACC ->( baseSavePos())
         ACC ->( dbGoTo( recNo))
         if left( ACC ->PRIZ, 6) $ "15 7  "
            messageB( "Этот кредит нельзя гасить наличными !")
            ACC ->( baseRestPos())
            return .F.
         endif
         for jj = 1 to len( arr)
            if left( arr[ jj, 1], 5) = "45503"
               recNo :=  arr[ jj, 5]
               ACC ->( dbGoTo( recNo))
               rest += sumRest( ACC ->schet, setCurDate())
            endif
            if left( arr[ jj, 1], 5) = "45815"
               recNo :=  arr[ jj, 5]
               ACC ->( dbGoTo( recNo))
               rest += sumRest( ACC ->schet, setCurDate())
            endif
         next

         recNo :=  arr[ 1, 5]
         ACC ->( baseSavePos())
         ACC ->( dbGoTo( recNo))

         restPctS :=  restPct - ZPctSum
         restSum := rest + restPctS
         if round( restSum, 2) = 0
            messageB( "У клиента нет задолженности по кредиту !!!")
            return NIL
         endif

         if restSum < 0
            if getUp( 5, 5, {{ "Возврат кредита  " +  " "      } ,;
                             { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" },;
                             { "Задолженность по процентам" + mStr( iif( round( restPctS, 2) = 0, 0.00, -restPctS)) } ,;
                             { "Задолженность по кредиту  " + mStr( -rest  ) } ,;
                             { "Итого долг:               " + mStr( -restSum) } ,;
                             { "" },;
                             { "Введите сумму возврата          ", block( summa),,,;
                                {|g| g:varGet <= round( -restSum, 2) .AND. g:varGet > 0 }}})


               if round( ZPctSum, 2) <> 0    //гасим проценты
                  s := select()
                  recNo :=  arr[ 1, 5]
                  ACC ->( baseSavePos())
                  ACC ->( dbGoTo( recNo))
                  summa := credRetPcts( summa, recNo)
                  select ( s)
               endif

               for jj = 1 to len( arr)
                  if summa > 0 .AND. left( arr[ jj, 1], 5) = "45815" ;
                    .AND. arr[ jj, 8] < 0                //Просроченные кредиты
                     recNo :=  arr[ jj, 5]
                     ACC ->( dbGoTo( recNo))
                     summa := credRetMkPr( recNo, summa, arr[ jj, 8])
                  endif
                  if summa > 0 .AND. left( arr[ jj, 1], 5) = "45503" ;
                    .AND. arr[ jj, 8] < 0                //Микрокредит
                     recNo :=  arr[ jj, 5]
                     ACC ->( dbGoTo( recNo))
                     summa := credRetMkPr( recNo, summa, arr[ jj, 8])
                  endif
               next

            endif
         endif
         ACC ->( baseRestPos())
      else
         messageB( "Нет задолженности по микроредитам !")
      endif
      return NIL
   else
      messageB( "Нет задолженности по микроредитам !")
   endif

return NIL
//
funcDef credRetMkPr with recNo:NU, summa:NU, rest:NU ;
  local arr, arr2, n := 1, rV := 0.00, aOrder, ;
        ordInc, ro, sim, fk, fullName, s, ;
        sPensSchet := getPensSchet( CLIENTS->CODE ), ;
        sBuff2004 := ''

        ordInc := ordIncIn( setCurDate())

        operGetInfo( if( confUseAgent(), opDecode( "6136"), opDecode( "2003")),;
                                      @ro, @sim)

        sBuff2004 = allTrim( Str(ordInc) ) + ';' + sPensSchet

        fullName := credFullName()

        aOrder := vznos_nal( ordInc, setCurDate(), fullName     ,;
        if(confUseAgent(), sPensSchet, ACC->Schet),;
                     summa, cliFullName()  ,;
        if(confUseAgent(), "Взнос на текущий счет", "Возврат кредита"),;
                     confKassa(), sim, ro)
        printAOrder( aOrder)

        sBuff2004 = allTrim( Str(ordInc) ) + ';' + ACC ->schet
        if confUseAgent()
             aOrder := memWisc2( sPensSchet, ACC ->schet, '', summa,;
                                '', 'Возврат социальной ссуды')
             printAOrder( aOrder)
        endif
// Add <confUseAgent()> by TMV  08-01-2003 для работы в режиме Отделения
        if !sumWrite( ACC ->schet, summa, setCurDate(),;
        if(confUseAgent(), opDecode( "2060"), opDecode( "2003")),;
                        ordInc,,;
        if(confUseAgent(), sPensSchet, nil))
               logErr( "Проводка по оплате ссуды со счета клиента не сделана !")
// Add <confUseAgent()> by TMV  08-01-2003 для работы в режиме Отделения
        elseif confUseAgent() .And.;
              !sumWrite( sPensSchet, summa, setCurDate(), opDecode( "6136"),;
                       ordInc,, sBuff2004)
              logErr( "Проводка по оплате ссуды не сделана !")

        else
              if kasOpen()
                  if KASSA ->( dbSeek( "003"))
                      if KASSA ->( mRLock())
                           KASSA ->summa := KASSA ->summa + summa * 100
                           KASSA ->( netUnLock())
                       endif
                   endif
                   kasClose()
              endif

        endif
        //
        //        credRetPcts( recNo)
        //

return rV
//
funcDef credFullName local clientCode, fullName := cliFullName()

  clientCode := credOtherPerson()
  if !empty( clientCode)
    CLIENTS ->( baseSavePos())
    CLIENTS ->( dbSetOrder( 1))
    if CLIENTS ->( dbSeek( clientCode))
       fullName := cliFullName()
    endif
    CLIENTS ->( baseRestPos())
  endif
return fullName
//
static funcDef chkRetPct with schet:C local rV := .T., rest
return rV
//
funcDef aMin with arr:A local rV, i
   rV := arr[ 1]
   for i := 2 to len( arr) do rV := min( arr[ i], rV)
return rV

funcDef isZabCred
return ACC ->PRIZ == "15 9  1  0  "

funcDef isCardCred
return left( ACC ->PRIZ, 9) == "15 7  4  "

funcDef isCred
return credSSchet( ACC ->schet, ACC ->PRIZ)

funcDef accZabCred with arr:AU, arr2:AU ;
   local curSchet, arrName, arrCode

   arr  := {}
   arr2 := {}
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 2))

   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 3))

   arrName := {}; arrCode := {}
   vidArray( "15 9  1  0  ", @arrName, @arrCode)

   accSeek( CLIENTS ->code)
   while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())

         if isZabCred() .AND. ACC ->typeOf == "00" .AND. ;
            chkCredOpen( arrCode, ACC ->schet)                  .AND. ;
            ( SUMS ->( dbSeek( opDecode( "1007") + DtoS( setCurDate()) + ACC ->schet)) .OR. ;
              sumRest( ACC ->schet, setCurDate()) <> 0)

            aAdd( arr, "Счет "     + ACC ->schet)
            aAdd( arr2, { ACC ->( recNo()), ACC ->schet})
         else
            //
            // вернем 716 счет, по которому можно выдать кредит
            // это действующий счет, по которому не было движения сегодня
            //  и остаток нулевой !!!
            //
            if isZabCred() .AND. ACC ->typeOf == "00" .AND. ;
               chkCredOpen( arrCode, ACC ->schet)     .AND. ;
               ( SUMS ->( !dbSeek( opDecode( "1007") + DtoS( setCurDate()) + ACC ->schet)) .OR. ;
                sumRest( ACC ->schet, setCurDate()) == 0)
                curSchet := ACC ->schet
            endif
         endif
         skip 1 alias ACC
   enddo

   SUMS ->( baseRestPos())
   ACC  ->( baseRestPos())

return curSchet


//
// оформление ссуды на покупку товаров заботы
//
funcDef credZab with schet:C, sum:N ;
   local rV := .F., ar1, ar2, credSchet, credSum, minSum, pctSum, strSum, s,;
         docNumb, credNazn, strNazn, ordMem, ordInc, curPcts, nextPens  ,;
         curPct2, printAgain, aOrder, ro, sim, canWrite := .F., mustSave,;
         maxSum

   if !credZabChk( schet, sum) then return .F.
   s := select()
   if credOpen()
      credSum := sum - int( sumRest( schet, setCurDate()))
      //
      // сумма кредита кратна 1 р. в сторону увеличения
      //
      credSum := round( credSum + 0.49999999, 0)  
      maxSum  := aMin( { credMaxSum( "08"), CLIENTS ->credSumma/ 100})
      if credSum > maxSum
         credClose()
         select (s)
         messageB( "Сумма кредита не может превышать " + lTrim( mStr( maxSum)) + "!!!")
         return .F.
      endif
        
      if isNIL( credPtr( credSum, 60, "08"))
         credClose()
         select (s)
         messageB( "Нет процентных ставок для кредита на покупку товаров !!!")
         return .F.
      endif
      pctSum  := int( round( credSum * CRED_PCT ->Procent * 60/ 365/ 100, 0))
      strSum  := max( credSum * CRED_PCT ->proc_Str/ 100, CRED_PCT ->min_Str)
      minSum  := 10.00
      if credSum < minSum
         if !yesNo( "Внесете наличными " + mStr( credSum) + " ?")
            credSum := minSum
         else
           credClose(); select (s)
           return .F.
         endif
      endif

      credSchet := accZabCred( @ar1, @ar2)
      if yesNo( { "Вы хотите оформить кредит на сумму " + mStr( credSum),;
                  "для покупки товаров ?" })
   // Replaced by TMV 29-07-2004
         if credGoCred( credSchet, "45509", "15 9  1  0  ")
            ACC ->( netUnLock())
            docNumb  := confZabCredDoc()
            credNazn := 'Выдача ссуды на оплату прод.заказов,приобретенных в ЗАО "Забота+"'

           strNazn := "Тарифный сбор"
   // Replace by TMV 24-12-2002 -- End
            
            credNazn += ".Заявление N " + docNumb
            strNazn  += ".Заявление N " + docNumb
            ordMem   := ordIncMem( setCurDate())
            ordInc   := ordIncIn( setCurDate())
            //
            // определим месяц следующей пенсии
            //
            nextPens := setCurDate() + CRED_PCT ->MAX_SROK 
            strSum   := max( credSum * CRED_PCT ->proc_Str/ 100, CRED_PCT ->min_Str)
            strSum   := val( Str( strSum))
            curPcts  := credAText( credAStav( CRED_PCT ->max_Sum, "08"), @curPct2)
            SRSumma( credSum)
            printAgain := .T.    
            mustSave   := mustSaveOpen( .T.)
            while printAgain
              servDummy( {{ "@docNumb" , docNumb        },;
                          { "@nextPens", DtoC( nextPens)},;
                          { "@curPcts",  curPcts        },;
                          { "@curPct2",  curPct2        },;
                          { "@summaRSO", lTrim( mStr( strSum)) + " ( " + rSumma( strSum) + " )"},;
                          { "@schet716", ACC ->schet    },;
                          { "@sch711",   schet          },;
                          { "@szsk",     confSZSK()     },;
                          { "@datOpen",  setCurDate()   },;
                          { "@ordInc",   ordInc         },;
                          { "@ordOut",   ordMem         }})

              //
              // так как заявления идентичны согласно письму Конкиной от 14.05.2001
              // то теперь печатается бланк из обычной ссуды
              //
              //
              printOpen( "cr1n", 1)
              SaveLastOrder( { getSavedOpen()})
              operGetInfo( opDecode( "6030"), @ro, @sim)
              if !confUseAgent()
                memOrdNormal( ordMem, setCurDate(), ACC ->schet, schet, credSum,;
                   credNazn, ro, cliFullName(), cliFullName(),;
                   confBankName() + " " + confFilial())
              endif
              SaveLastOrder( { getSavedOpen()})
              operGetInfo( opDecode( "3006"), @ro, @sim)
              useZabota( .T.)
              aOrder := vznos_nal( ordInc,setCurDate(), cliFullName(),;
                 confStrSchet(), strSum, confBankName()+','+ rTrim(confFilial()),;
                 strNazn, confKassa(), sim, ro,,.t.)
              useZabota( .F.)
              printAOrder( aOrder)
              saveLastOrder( aOrder, .F.)
              printAgain := !yesNo( "Документы распечатаны правильно ?")
            enddo
            mustSaveOpen( mustSave)
            SRSumma( 0)
            servDummy( {})
            //
            // генерация проводок
            //
            ACC ->( baseSavePos())
            ACC ->( dbSetOrder( 1))
            if ACC ->( dbSeek( Schet))
               if ACC ->( canWrite := mRLock())
                  accSetPZab( "Y")
                  ACC ->( netUnLock())
               else
                  errLog( "Не удалось отметить текущий счет для выдачи микрокредита !")
               endif
            else
               errLog( "Не удалось найти текущий счет для выдачи микрокредита !")
            endif
            ACC ->( baseRestPos())
            if canWrite .AND. ;
               sumWrite( ACC ->schet, -credSum, setCurDate(), "6030", ordMem,, "B" + schet)
               if sumWrite( schet, credSum, setCurDate(), "6030", ordMem,, "C" + ACC ->schet)
                  if sumWrite( confStrSchet(), strSum, setCurDate(), opDecode( "3006"), ordInc)
                     if WISC ->( mAppend())
                        WISC ->type   := "998"
                        WISC ->schet  := ACC ->schet
                        WISC ->buffer := docNumb + ";08;" + ;
                           myDtoC( setCurDate()) + ";" + ;
                           nStr( ordInc)       + ";" + ;
                           nStr( strSum)       + ";" + ;
                           schet
                        WISC ->( netUnLock())
                        rV := .T.
                     else
                        errLog( { "Поручение не сформировано !"        ,;
                                  "Запишите номер счета и ФИО клиента" ,;
                                  "и обратитесь на узел связи !!!" })
                     endif
                  else
                     errLog( "Проводка по счету " + confStrSchet() + " не сделана !")
                  endif
               else
                  errLog( "Проводка по счету " + schet + " не сделана !")
               endif
            else
               errLog( "Проводка по счету " + ACC ->schet + " не сделана !")
            endif
         endif
      endif
      credClose(); select (s)
   endif

return rV

funcDef isTechCred
return ACC ->PRIZ $ "15 7  1  0  ,15 7  2  0  ,15 7  3  0  "

funcDef hasTechCred local rV := .F.
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  if ACC ->( dbSeek( CLIENTS ->code))
    while ACC ->code == CLIENTS ->code .AND. CLIENTS ->( !eof())
      if isTechCred()
        if sumRest( ACC ->schet, setCurDate()) <> 0
           rV := .T.
           exit
        endif
      endif
      skip 1 alias ACC
    enddo
  endif  
  ACC ->( baseRestPos())
return rV

*------------- проверка ------------------------
funcDef credZabChk with schet:C, sum:N local rV := .F., sumCred, cnt
  if sumRest( schet, setCurDate()) >= sum
     messageB( "На счете " + schet + " достаточно денег для оплаты заказа !")
     return .F.
  endif
  credExist( @cnt)
  if cnt > 0
     messageB( "У клиента оформлен лимит кредитования !")
     return .F.
  endif
  if hasTechCred()
    messageB( "У клиента есть карточный овердрафт !")
    return .F.
  endif
  if CLIENTS ->credSumma == 0
     messageB( "Филиал запретил кредитование !")
     return .F.
  endif
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  accSeek( CLIENTS ->code)
  while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
    if isZabCred() .OR. isCardCred()
       if sumRest( ACC ->schet, setCurDate()) <> 0
          messageB( "По счету " + ACC ->schet + " есть остаток !")
          ACC ->( baseRestPos())
          return .F.
       endif
       if sumPType( ACC ->schet, opDecode("2004")) > 0
          messageB( "По счету " + ACC ->schet + " есть неуплаченные проценты !")
          ACC ->( baseRestPos())
          return .F.
       endif
    endif
    skip 1 alias ACC
  enddo
  ACC ->( baseRestPos())
return .T.

funcDef credZabCntr with schet:C, sum:N ;
  local totSumma := abs( CLIENTS ->credSumma/ 100)
  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 5))
  if CONTRACT ->( dbSeek( schet))
     while CONTRACT ->debetAcc == schet .AND. CONTRACT ->( !eof())
       if CONTRACT ->Summa > 0
          totSumma -= CONTRACT ->summa/ 100
       endif
       if CONTRACT ->code2 == "09" .AND. CONTRACT ->code3 == "71" .AND. ;
          CONTRACT ->Summa == -100
          CONTRACT ->( baseRestPos())
          return .F.
       endif
       skip 1 alias CONTRACT
     enddo
  endif
  CONTRACT ->( baseRestPos())
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  if ACC ->( dbSeek( CLIENTS ->code))
     while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
       if isCred()
          totSumma := totSumma - abs( sumRest( ACC ->schet, setCurDate())) - ;
             ( sumPType( ACC ->schet, opDecode("2004")) - ;
               sumSType( ACC ->schet, opDecode("2004"))  )
       endif
       skip 1 alias ACC
     enddo
  endif
  ACC ->( baseRestPos())
return totSumma >= sum

// Поиск пенсионного счета клиента
funcdef getPensSchet with sCode:CU local rV := ''

   DEFAULT sCode TO CLIENTS->CODE

   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 2))
   ACC ->( netSeek( sCode))
   do while ACC ->code == sCode .AND. ACC ->( !eof())
           if isPens( ACC ->priz) .AND. ACC ->typeOf == "00"
                   rV = ACC->SCHET
                   exit
           endif
           ACC->(dbskip())
   enddo
   ACC ->( baseRestPos())

return rV

// Проверка наличия выдачи и погашения ссуды в один день
funcDef chkCredInOut With sSchet:CU local rV := .T., nSumma := 0

   DEFAULT sSchet To " "

   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 3))

   if confUseAgent()
           rV = !(SUMS ->( dbSeek( opDecode( "6031") + DtoS( setCurDate()) + sSchet));
                   .And. SUMS ->( dbSeek( opDecode( "2060") + DtoS( setCurDate()) + sSchet)))
   else
           rV = !(SUMS ->( dbSeek( opDecode( "1007") + DtoS( setCurDate()) + sSchet));
                   .And. SUMS ->( dbSeek( opDecode( "2003") + DtoS( setCurDate()) + sSchet)))
   endif

   SUMS ->( baseRestPos())
   if !rV then errLog({"Нельзя выдать кредит, т.к. клиент сегодня его уже получил и погасил.",;
                                                     "Сначала удалите проводки по погашению и выдаче этого кредита,",;
                                                     "а потом заводите новую операцию"})

return rV


// Add by TMV  20-02-2003
// Установка лимита задолжености по кредиту
funcDef credLimitDolg With isAsk:LU local rV := .F., s := select(), limitSum := 0,;
        pensSum := 0, dopSum := 0, s1 := 0, pctSum := 0,;
        strSum := 0, oldSum := 0, credSum := 0, s2 := 0, aGets := {}

   DEFAULT isAsk To .F.
           
   if credOpen()
      //
      // подготовка сумм
      //
           oldSum := cliCredLimit()/100
      limitSum := kasRest( "003")
      if rsKasHas() then limitSum := 999999999.99
      pensSum  := CLIENTS ->credSumma/ 100
      dopSum   := credMaxSumma()
      s1       := aMin( { pensSum, dopSum, limitSum})
      s1       := int( s1)
      //
      // определим процентную ставку
      //
      credPtr( s1, 60)
      pctSum   := int( round( s1 * CRED_PCT ->Procent * 60/ 365/ 100, 0))
      //
      // страховой взнос
      //
      strSum := max( s1 * CRED_PCT ->proc_Str/ 100, CRED_PCT ->min_Str)
      s2     := aMin( { pensSum - pctSum - 0, dopSum, limitSum})
      s2     := int( s2)
           credSum = min( s2, oldSum )

      aGets  := {{ "Текущий лимит отделения " + mStr( kasRest( "003"))         },;
                 { "Допустимая сумма : ", block( s2),"999 999 999 999", {||.F.}},;
                 { ""                                                          },;
                 { "Сумма кредита    : ", block( credSum),,,                     ;
                                     {|g| chkCredDolg( g, s2)}}}
      //
      // оформим лимит задолженности
      //
      if s2 >0
         if getUp( 5, 5, aGets)
                           if credSum > 0 .And. cliRLock()
                                   cliCredLimit( credSum*100 ) 
                                   cliUnLock()
                                   rV = .T.
                           messageB( "Лимит задолженности по кредиту установлен.")
                                   if isAsk
                                           rV = yesNo( "Оформить выдачу кредита ?")
                                   endif
                           endif
                   endif
           else
                   messageB( "Допустимая сумма кредита равна 0.")
           endif
      credClose()
      select ( s)
   endif

return rV

//
// проверка суммы лимита задолженности
//
funcDef chkCredDolg with g:O, s2:N local rV := .T.

   if g:varGet < 0
      messageB( "Отрицательная сумма ?!!")
      rV := .F.
   elseif g:varGet() > s2
      logErr( "В выдаче кредита на такую сумму вам отказано !")
      rV := .F.
   endif

return rV

// Add by TMV  21-04-2003
// Установка лимита задолжености по кредиту по нестраховой схеме
//
funcDef credLineNoStrah With isAsk:LU ;
local rV := .F., s := select(), wIndex :=0,;
        limitSum := 0,  pensSum := 0, dopSum := 0, s1 := 0, pctSum := 0,;
        strSum := 0, oldSum := 0, oldDateLine, credSum := 0, s2 := 0, aGets := {},;
        dDateLine := AddMonth( setCurDate(), 12), mMin := 0,;
        arr := {}, arr2 := {}, lNewSchet := .T., docNumb := 0,;
        curSchet, cnt, i := 0, arrName := {}, arrCode := {},;
        sPensSchet := getPensSchet( CLIENTS->CODE ), lWisc := .T.,;
        credDogNum := '', credDogDate := CtoD(""),;
        oldDateOpen := CtoD(""),;
        lError := .F. // Add by TMV 03-11-2003

   DEFAULT isAsk To .F.
           
   oldSum = int( cliCredLine() )
   oldDateLine = cliCrdDClosNoStrah()
   oldDateOpen = cliCrdDOpenNoStrah()

   if empty(sPensSchet)
           messageB('У клиента не найден пенсионный счет !')
           return rV
   endif

   curSchet := accACred( @arr, @arr2)
   if (len( arr) > 0 .Or. !empty(curSchet) ) .And. empty(accNoStrahCred())

           if len(arr2) > 0
                   for i = 1 to len(arr2)
                           lError = ( lError .Or. (sumRest( arr2[i,2], setCurDate()) != 0) )
                   next
           endif
           if !empty(curSchet)
                   lError = ( lError .Or. (sumRest( curSchet, setCurDate()) != 0) )
           endif

           if lError                     // Add by TMV 03-11-2003
                   errLog( "У клиента есть микрокредиты по страховой схеме !")
                   return rV                     
           endif                         // Add by TMV 03-11-2003
   endif
   curSchet = ''

   if credOpen()
      //
      // подготовка сумм
      //
           curSchet := accNoStrahCred()  && accACred( @arr, @arr2)
           if !empty( curSchet )
                   mMin = Max( 0, -sumRest( curSchet, setCurDate()) )
           endif
           limitSum := kasRest( "003")
           if rsKasHas() then limitSum := 999999999.99
           pensSum  := CLIENTS ->credSumma/ 100
       dopSum   := credMaxSumma("17")
       s1       := aMin( { pensSum, dopSum, limitSum})
       s1       := int( s1)
       //
       // определим процентную ставку
       //
       credPtr( s1, 60, "17")
       //
       // страховой взнос
       //
       s2     := s1
       credSum = min( s2, oldSum )

      vidArray( ACC_CRD_NOSTRAH, @arrName, @arrCode, .F.)
           aGets  := {{ "Текущий лимит отделения " + mStr( kasRest( "003"))         },;
                   { "Допустимая max сумма : ", block( s2),"999 999 999 999", {||.F.}},;
                   { "Допустимая min сумма : ", block( mMin),"999 999 999 999", {||.F.}},;
                        { ""                                                          },;
                     { "Сумма лимита     : ", block( credSum),,, ;
                                     {|g| chkCrdLSum( g, s2, mMin)}},;
                 { "Срок действия до : ", block( dDateLine),,,                     ;
                                     {|g| chkCrdLDate( g )}};
                                     }
       //
       // оформим лимит задолженности
       //
      if s2 > 0
              if getUp( 5, 5, aGets) .And.;
                                   yesNo( "Оформить лимит по кредитной линии ?")
                           if credSum > 0 .And. credSum != oldSum
                                   credGoCred( curSchet, arrCode[ 1, 5], arrCode[ 1, 4])
                                   credDogNum = getCrdNumNoStrah()
                                   credDogDate = setCurDate()
                                   if oldSum = 0  // Лимит установлен впервые
                                           prnSetLimNoStrah(credSum)
                                           prnCrdLinDog(credSum, sPensSchet, credDogNum, credDogDate)
                                   else
                                           prnSetLimNoStrah(credSum, .T., credDogNum, credDogDate)
                                           prnCrdLinPril(credSum, sPensSchet, credDogNum, credDogDate)
                                   endif
                                   if cliRLock()
                                           cliCredLine( credSum )
                                           cliCrdDClosNoStrah( dDateLine )
                                           cliCrdDOpenNoStrah( setCurDate() )
                                           cliUnLock()
                                           wIndex = WISC ->( indexOrd())
                                           WISC ->( dbSetOrder( 1 ))
                                           wiscUseAppend( .T.)
                                           if wiscSeek( ACC ->schet, "969" )
                                                   if (lWisc := wiscRLock())
                                                           WISC ->buffer := sPensSchet + ";" +;
                                                                   Str(credSum, 19) + ";" +;
                                                                   Right( DtoS( dDateLine ), 2) + subStr( DtoS( dDateLine ), 5, 2) +;
                                                                   Left( DtoS( dDateLine ), 4) + ";" +;
                                                                   Right( DtoS( setCurDate() ), 2) + subStr( DtoS( setCurDate() ), 5, 2) +;
                                                                   Left( DtoS( setCurDate() ), 4) + ";" +;
                                                                   ACC->Schet + ";" +;
                                                                   subStr( WISC -> Buffer, 81 )
                                                   endif
                                           else
                                                   if (lWisc := wiscAppend())

                                  docNumb := allTrim(CLIENTS->CODE) + DtoS(setCurDate())
                                                           WISC ->type   := "969"
                                                           WISC ->schet  := ACC ->schet
                                                           WISC ->buffer := sPensSchet + ";" +;
                                                                   Str(credSum, 19) + ";" +;
                                                                   Right( DtoS( dDateLine ), 2) + subStr( DtoS( dDateLine ), 5, 2) +;
                                                                   Left( DtoS( dDateLine ), 4) + ";" +;
                                                                   Right( DtoS( setCurDate() ), 2) + subStr( DtoS( setCurDate() ), 5, 2) +;
                                                                   Left( DtoS( setCurDate() ), 4) + ";" +;
                                                                   ACC->Schet + ";" +;
                                                                   docNumb + ";" + docNumb
                                                   endif
                                           endif
                                           if lWisc
                                                   WISC ->( netUnLock())
                                                   rV = .T.
                                                   messageB( "Лимит по кредитной линии установлен.")
                                                   if isAsk
                                                           rV = yesNo( "Оформить выдачу кредита ?")
                                                   endif
                                           else
                                                   if (lWisc := cliRLock())
                                                           cliCredLine( oldSum )
                                                           cliCrdDClosNoStrah( oldDateLine )
                                                           cliCrdDOpenNoStrah( oldDateOpen )
                                                           cliUnLock()
                                                   endif
                                                   if !lWisc
                                                   errLog( { "Поручение не сформировано !"       ,;
                                                   "Запишите номер счета и ФИО клиента" ,;
                                                   "и обратитесь на узел связи !!!" })
                                                   else
                                                   errLog( { "Поручение не сформировано !"       ,;
                                                   "Повторите операцию установки лимита." })
                                                   endif
                                           endif
                                           wiscUseAppend( .F.)
                                           WISC ->( dbSetOrder( wIndex ))
                                   else
                                   errLog( { "Поручение не сформировано !"       ,;
                                           "Повторите операцию установки лимита." })
                                   endif
                   endif
                   endif
           else
                   messageB( "Допустимая сумма лимита равна 0.")
      endif
           credClose()
           select ( s)
   endif

return rV

//
// проверка суммы лимита задолженности
//
funcDef chkCrdLSum with g:O, s2:N, mMin:N local rV := .T.

   if g:varGet < 0
      messageB( "Отрицательная сумма ?!!")
      rV := .F.
   elseif g:varGet() > s2 .Or. g:varGet() < mMin
      logErr( "В установке лимита на такую сумму вам отказано !")
      rV := .F.
   endif

return rV

//
// проверка суммы лимита задолженности
//
funcDef chkCrdLDate with g:O local rV := .T.

   if g:varGet < setCurDate()
      messageB( "Дата окончания действия кред. линии меньше текущей !")
      rV := .F.
   endif

return rV

// Add by TMV  24-04-2003
// Проверка - можно ли выдать кредит по нестраховой схеме
//
funcDef crdCredLineCan With sCode: C local rV := .T.,;
        dDateLine := cliCrdDClosNoStrah(), sPensSchet := getPensSchet( sCode )

   if empty(sPensSchet)
           logErr( "У клиента нет пенсионных счетов.")
           rV = .F.
   endif
   if rV .And. cliLimit() <= 0
           logErr( "В выдаче кредита отказано.")
           rV = .F.
   endif
   if rV .And. cliCredLine() <= 0
           logErr( "В выдаче кредита отказано - не установлен лимит линии кредитования.")
           rV = .F.
   endif
   if rV .And. !empty(dDateLine) .And. dDateLine < setCurDate()
      logErr( "В выдаче кредита отказано - срок действия истек.")
           rV = .F.
   endif
return rV

// оформление нового кредита по нестраховой схеме : для текущего клиента
// счет уже открыт !
//
funcDef credNewNoStrah with curSchet:CU ;
        local rV := .F., s1, s2, aGets, credSum := 0 ,;
              docNumb, s, n := 1, aOrder, ro, sim    ,;
              credOper, strOper, credNazn, strNazn   ,;
              theOrder, arr, arr2, nPens, nextPens   ,;
              ordInc, ordOut, kasRecNo               ,;
              kasRest, printAgain := .T., mustSave   ,;
              curPct2, curPcts, aStav,;
                                  arrName := {}, arrCode := {}, balSchetPtr := 1,;
                                  sPensSchet := getPensSchet( CLIENTS->CODE ),;
                                  credNum := 0,;
                                  credRestSum := 0,;
                                  credRestPtc := 0,;
                                  credDogNum := '', credDogDate := CtoD("")


   if isNil( curSchet )
           logErr('У клиента нет ссудного счета. Сначала установите лимит !')
           return rV
   endif
   credDogNum = getCrdNumNoStrah()
   credDogDate = cliCrdDOpenNoStrah()

   credOper := opDecode( "1007")
   strOper  := opDecode( "3006")

   if setUpValue('PMKL') == '1'
           if !transExist( cliCode(), "54")
                   logErr('У клиента нет поручений на перевод с ТПС.')
                   return rV
           endif
   endif

   if sumRest( sPensSchet, setCurDate() ) >= 10
           logErr( "На пенсионном счете остаток > 10-00 руб.")
           return rV
   endif

   credNazn := "Выдача социального кредита"
   strNazn := "Тарифный сбор"

   credRestSum = Abs( sumRest( curSchet, setCurDate()) )
   if !confUseAgent()
           credRestPtc = Abs( round( sumPType( curSchet, opDecode("2004")), 2))
   else
           credRestPtc = Abs( round( sumPType( curSchet, opDecode("2004")), 2) )
   endif

   s := select()
   if credOpen()
      //
      // подготовка сумм
      //
      limitSum := kasRest( "003")
      if rsKasHas() then limitSum := 999999999.99
      kasRest  := kasRest( "999")
      pensSum  := cliCredLine()
      dopSum   := credMaxSumma("17")
      s1       := aMin( { pensSum, dopSum, limitSum})
      s1       := int( s1)
      //
      // определим процентную ставку
      //
      credPtr( s1, 60, "17")
      pctSum   := int( round( s1 * CRED_PCT ->Procent * 60/ 365/ 100, 0))
      //
      // страховой взнос
      //
      strSum := max( s1 * CRED_PCT ->proc_Str/ 100, CRED_PCT ->min_Str)
      s2     := aMin( { pensSum - pctSum - credRestPtc - credRestSum, dopSum, limitSum})
      s2     := int( s2)

      vidArray( ACC_CRD_NOSTRAH, @arrName, @arrCode, .F.)
      //
      // пенсионные счета
      //
      nPens := 1
      aGets  := {{ "Текущий лимит отделения " + mStr( kasRest( "003"))         },;
                 { "Остаток по кассе        " + mStr( kasRest)                 },;
                 { "Допустимая сумма : ", block( s2),"999 999 999 999", {||.F.}},;
                 { ""                                                          },;
                 { "Пенсионный счет  : ", block( sPensSchet),, {||.F.}},;
                 { "Ссудный счет     : ", block( curSchet),, {||.F.}},;
                 { "Сумма кредита    : ", block( credSum),,,                     ;
                                     {|g| chkCredSum(g,@aGets,@s2,@credSum)}}}
      if confUseAgent() then aGets[ 2] := { ""}
      //
      // оформим кредит
      //
      if s2 >0
         SorryPiter()
         if getUp( 5, 5, aGets) .AND. len( arrName) >0
            minPens := sumPMinPens( sPensSchet )

            //
            // откроем счет
            //
            if noYes( { "Оформить кредит на  сумму " + mStr( credSum)})

               if credGoCred( curSchet, arrCode[ 1, 5],;
                      arrCode[ 1, 4])

                  docNumb := allTrim(CLIENTS->CODE) + DtoS(setCurDate())
                  credNazn   += ".Заявление N " + docNumb
                  strNazn    += ".Заявление N " + docNumb

                  ordOut := theOrder := ordIncOut( setCurDate())
                  ordInc             := ordIncIn( setCurDate())
                  //
                  // определим месяц следующей пенсии
                  //
                  nextPens := setCurDate() + CRED_PCT ->MAX_SROK // 05-02-2005 TAT
                  strSum   := max( credSum * CRED_PCT ->proc_Str/ 100, CRED_PCT ->min_Str)
                  strSum   := val( Str( strSum))
                  curPcts  := credAText( credAStav( CRED_PCT ->max_Sum, "17"), @curPct2)
                  servDummy( {{ "@docNumb" , docNumb        },;
                              { "@nextPens", DtoC( nextPens)},;
                              { "@curPcts",  curPcts        },;
                              { "@curPct2",  curPct2        },;
                              { "@summaRSO", lTrim( mStr( strSum)) + " ( " + rSumma( strSum) + " )"},;
                              { "@schet716", curSchet    },;
                              { "@sch711",   sPensSchet},;
                              { "@datOpen",  setCurDate()   },;
                              { "@ordInc",   ordInc         },;
                              { "@szsk",     confSZSK()     },;
                              { "@ordOut",   ordOut         }})

                  if kasRest( "999") >= credSum
                     if if( rsKasHas(), .T., kasRest( "003") >= credSum)
                        //
                        // оформление микрокредита
                        //
                        SRSumma( credSum)
                        mustSave := mustSaveOpen( .T.)
                        operGetInfo( credOper, @ro, @sim)
                        while printAgain
                          //
                          // заява на выдачу кредита
                          //
                          prnGetLimCred(credSum, strSum, credDogNum, credDogDate)
                          SaveLastOrder( { getSavedOpen()})

                     if confUseAgent() // Add by TMV 08-01-2003
                           aOrder := memKred( curSchet, sPensSchet, credSum,;
                                           'Выдача социальноой ссуды по заявлению N '+docNumb) // Edit by TMV  05-12-2002
                           printAOrder( aOrder)
                           saveLastOrder( aOrder, .F.)
                     endif
                          //
                          // ордер etc
                          //
                          aOrder := homeOneOrder( cliFullName(), cliFullAddress(),;
                            cliDocum(), theOrder, ;
                                                                    if(confUseAgent(), /*'Выплата средств с текущего пенсионного счета' 02-12-2004 TAT*/ "Выплата со вклада до востребования",;
                                                                    credNazn), ;
                                                                    ro, sim, credSum, .T.,;
                            .T., setCurDate(),;
                                                                    , , , if(confUseAgent(), sPensSchet, nil)) // Add by TMV  22-11-2002

                          printAOrder( aOrder)
                          saveLastOrder( aOrder, .F.)
                          printAgain := !yesNo( "Документы распечатаны правильно ?")
                        enddo
                        mustSaveOpen( mustSave)
                        SRSumma( 0)
                        servDummy( {})
                                                           if cliRLock()
                                                                   credNum = cliCredTranshNoStrah()
                                                                   cliCredTranshNoStrah( credNum + 1 )
                                                                   cliUnLock()
                           sumUseAppend(  .T.)
                           wiscUseAppend( .T.)
                           if sumWrite( curSchet, - credSum, setCurDate(),;
                                                                   if(confUseAgent(), opDecode( "6031"), opDecode( "1007")),;
                                                                   theOrder,,;
                                                                   docNumb + ';' + Padl( allTrim(Str( credNum )), 4, '0'))
                              inkey( 0.2)
                            if !confUseAgent() .Or. sumWrite( sPensSchet, - credSum,;
                                                                   setCurDate(), opDecode( "1060"), theOrder,,;
                                                                   docNumb + ';' + Padl( allTrim(Str( credNum )), 4, '0'))
                              inkey( 0.2)

                                 if wiscAppend()
                                    WISC ->type   := "967"
                                    WISC ->schet  := curSchet

                                    WISC ->buffer := sPensSchet + ";" +;
                                                     nStr( credSum)       + ";" + ;
                                                     myDtoC( setCurDate()) + ";" + ;
                                                                     docNumb             + ";" + ;
                                                                     if( n == 2, '2', '03') + ";" + ;
                                                     nStr( ordInc)       + ";" + ;
                                                                     Padl( allTrim(Str( credNum )), 4, '0')
                                    WISC ->( netUnLock())
                                                           sumPWrite( curSchet, credSum, setCurDate(),;
                                                           if(confUseAgent(), opDecode("1060"), opDecode("1007")),;
                                                                   credNum)
                                 else
                                    errLog( { "Поручение не сформировано !"        ,;
                                             "Запишите номер счета и ФИО клиента" ,;
                                             "и обратитесь на узел связи !!!" })
                                 endif
                                                                    endif  // Add by TMV  15-11-2002
                           else
                              errLog( "Проводка по выдаче кредита не сделана !")
                           endif
                           sumUseAppend(  .F.)
                           wiscUseAppend( .F.)
                        else
                           errLog( "Ошибка при определении номера транша. Проводки НЕ сделаны !")
                                                           endif
                     else
                        errLog( "Вы пытаетесь снять сумму большую, чем лимит отделения !!!")
                     endif
                  else
                     errLog( "Вы пытаетесь снять сумму большую, чем остаток по кассе !!!")
                  endif
               endif
         endif
         endif
      else
        logErr( "В выдаче кредита отказано !")
      endif
      credClose()
      select ( s)
   endif

return rV

// Add by TMV  21-05-2003
// возврат кредита и процентов по нестраховой схеме
//
funcDef crdRetNoStrah ;
  local rest, summa := 0.00, arr, arr2, n := 1, restPct,;
                  sPensSchet := getPensSchet( CLIENTS->CODE ),;
                  aCred := {}, aPct := {}, nCred := 0, nPct := 0,;
                  nSummaRv := 0, nTransh := 9999999      // Add by TMV  03-06-2003

   if confUseAgent() .And. empty( sPensSchet )
           messageB( "Не найден пенсионный счет клиента !")
           return NIL
   endif

   arr := brwGetSchet( 5)
   if len( arr) > 0
      n = arr[1, 5]
      if n > 0
         ACC ->( baseSavePos())
         ACC ->( dbGoTo( n))
         if left( ACC ->PRIZ, 6) $ "15 7  "
            messageB( "Этот кредит нельзя гасить наличными !")
            ACC ->( baseRestPos())
            return .F.
         endif
         rest = sumRest( ACC ->schet, setCurDate())
         if rest = 0
            messageB( "У клиента нет задолженности по кредиту !!!")
         elseif rest < 0
            rest = -rest
            restPct = round( sumPType( ACC ->schet, opDecode("2004")), 2)
            if getUp( 5, 5, {{ "Возврат кредита по счету " +        ;
                               splitSSchet( ACC ->schet)         } ,;
                             { "Задолженность по кредиту " + mStr(rest) } ,;
                             { "Задолженность по проц-ам " + mStr(restPct) } ,;
                             { "" },;
                             { "Введите сумму возврата ", block( summa),,,;
                                {|g| g:varGet <= (rest + restPct) .AND. g:varGet > 0 }}})
                                    aCred := restNoStrah(ACC->Schet)
                                    aPct := restPctNoStrah(ACC->Schet)

                                   do while summa > 0 .And. ( Len(aCred) > 0 .Or. Len(aPct) > 0 ) ;
                                                   .And. nTransh > 0
                                           nTransh = 9999999                // Add by TMV  23-06-2003
                                           aEval( aCred, {|x| nTransh := Min( nTransh, x[1])} )
                                           aEval( aPct, {|x| nTransh := Min( nTransh, x[1])} )
                                           if nTransh > 0
                                                   nSummaRv = crdNoStrah( summa, @aCred, @aPct, ACC->Schet,;
                                                           sPensSchet, nTransh )
                                                   if nSummaRv > 0
                                                           crdWiscNoStrah(nSummaRv, ACC->Schet, nTransh)
                                                   endif
                                                   summa -= nSummaRv
                                           endif
                                   enddo
                           endif
                   endif
         ACC ->( baseRestPos())
           endif
   endif

return NIL

// Add by TMV  21-05-2003
// возврат кредита по нестраховой схеме
//
funcDef crdRetCrNoStrah With sSchet :C, summa :N, sPensSchet :C, nTransh :N ;
  local rest, arr, arr2, n := 1, rV := .T., aOrder,;
        ordInc, ro, sim, fk, recNo, fullName,;
                  sBuff2004 := ''

   ordInc := ordIncIn( setCurDate())
   operGetInfo( if(confUseAgent(), opDecode( "6136"), opDecode( "2003")),;
                @ro, @sim)
        sBuff2004 = allTrim( Str(ordInc) ) + ';' + sPensSchet

   fullName := credFullName()

   aOrder := vznos_nal( ordInc, setCurDate(), fullName     ,;
             if(confUseAgent(), sPensSchet, ACC->Schet),;
                                 summa, cliFullName()  ,;
             if(confUseAgent(), "Взнос на текущий счет","Возврат кредита"),;
                                 confKassa(), sim, ro)
   printAOrder( aOrder)

        sBuff2004 = allTrim( Str(ordInc) ) + ';' + ACC ->schet + ";" +;
                        Padl( allTrim(Str( nTransh )), 4, '0')  // Add <nTransh> by TMV  03-06-2003
        if confUseAgent()
                aOrder := memWisc2( sPensSchet, ACC ->schet, '', summa,;
                                        '', 'Возврат социальной ссуды')
                printAOrder( aOrder)
        endif

   if !sumWrite( ACC ->schet, summa, setCurDate(),;
                        if(confUseAgent(), opDecode( "2060"), opDecode( "2003")),;
                        ordInc,,;
                        sPensSchet + ";" + Padl( allTrim(Str( nTransh )), 4, '0'))
      logErr( "Проводка по оплате ссуды со счета клиента не сделана !")
                rV = .F.
   elseif !sumPWrite( ACC ->schet, -summa, setCurDate(),;
                                if(confUseAgent(), opDecode( "6136"), opDecode( "2003")),;
                                nTransh)
                rV = .F.
      logErr( "Дубль проводкив SUMP по оплате ссуды со счета клиента не сделана !")
   elseif confUseAgent() .And.;
                        !sumWrite( sPensSchet, summa, setCurDate(), opDecode( "6136"),;
                        ordInc,, sBuff2004)
      logErr( "Проводка по оплате ссуды не сделана !")
                rV = .F.
   else
      if kasOpen()
         if KASSA ->( dbSeek( "003"))
            if KASSA ->( mRLock())
               KASSA ->summa := KASSA ->summa + summa * 100
               KASSA ->( netUnLock())
            endif
         endif
         kasClose()
      endif
   endif

return rV

// возврат процентов по нестраховой схеме
//
funcDef crdRetPctNoStrah with sSchet :C, summa :N, sPensSchet :C, nTransh :N ;
   local rest, n := 1, fk, rV := .T.,;
         aOrder, ordInc, ro, sim, strRest

   ordInc := ordIncIn( setCurDate())
   operGetInfo( if(confUseAgent(), opDecode("6137"), opDecode("2004")),;
                @ro, @sim)
   useZabota( .T.)
   aOrder := vznos_nal( ordInc, setCurDate(), cliFullName()  ,;
                        if(confUseAgent(), sPensSchet, confPctSchet()),;
                                                                summa, confBankName()+','+ rTrim(confFilial()) ,;
                        if(confUseAgent(), "Взнос на текущий счет", "Возврат процентов"),;
                                                                confKassa(), sim, ro,,.T.)
   useZabota( .F.)
   printAOrder( aOrder)
   SaveLastOrder( aOrder)
        if confUseAgent() .And. !empty( sPensSchet )
                aOrder := memWisc2( sPensSchet, confPctSchet(), '', summa,;
                        '', 'Возврат процентов по социальной ссуде')
                printAOrder( aOrder)
        SaveLastOrder( aOrder)
        endif
   if !sumWrite( if(confUseAgent(), sPensSchet, confPctSchet()),;
                        summa, setCurDate(),;
                        if(confUseAgent(), opDecode("6137"), opDecode("2004")),;
         ordInc,, "PCT:" + if(confUseAgent(), confPctSchet(), ACC->Schet) + ";" + ;
                        Padl( allTrim(Str( nTransh )), 4, '0'))  // Add <nTransh> by TMV  03-06-2003
      logErr( "Проводка по возврату процентов на ТПС не сделана !")
                rV = .F.
   elseif confUseAgent() .And.;
                        !sumWrite( ACC->Schet, summa, setCurDate(), opDecode("2060"),;
         ordInc,, "PCT:" + sPensSchet + ";" + ;
                        Padl( allTrim(Str( nTransh )), 4, '0'))  // Add <nTransh> by TMV  03-06-2003
      logErr( "Проводка по возврату процентов не сделана !")
                rV = .F.
   else
       sumPWrite( ACC ->schet, - summa, setCurDate(),;
                        opDecode("2004"), nTransh)
   endif

return rV

// Поиск процентов по кредитам для нестраховой схемы
//
// Определение кредитного счета по нестраховой схеме
//
funcDef isCredNoStrah
return Left( ACC->PRIZ, 7 ) == ACC_CRD_NOSTRAH

// Add by TMV  20-05-2003
// Непогашенный остаток по траншу кредита - проценты для нестраховой схемы
//
funcDef restPctNoStrah With sSchet:C Local aSchet := {}, i := 0, j := 0
 
   SUMP ->( baseSavePos())
   SUMP ->( dbSetOrder( 1))
   SUMP ->( dbSeek( sSchet, .T.))

   while SUMP ->schet == sSchet .AND. SUMP ->( !Eof())
           if opEqual( SUMP->typeOf, "2004")
                   if Len(aSchet) > 0 .And.;
                                   (i := aScan(aSchet, {|x| x[1] == SUMP->CRD_NUM}) ) > 0
                           aSchet[i,2] += SUMP->SUMMA
                   else
                           aAdd(aSchet, {SUMP->CRD_NUM, SUMP->SUMMA})
                   endif
           endif
           SUMP->(dbSkip())
   enddo
   SUMP ->( baseRestPos())

   if (i := Len(aSchet)) > 0
           aSchet := aSort( aSchet, {|x,y| x[1]<y[1]} )
           for j := 1 to i
                   if aSchet[j,2] = 0
                           aDel(aSchet, j)
                           --i
                   endif
           next
           aSize( aSchet, i)
           aEval( aSchet, {|x| x[2] := Round( x[2]/100, 2 )} )
   endif

return aSchet

// Add by TMV  20-05-2003
// Непогашенный остаток по траншу кредита для нестраховой схемы
//
funcDef restNoStrah With sSchet:C Local aSchet := {}, i := 0, j := 0
 
   SUMP ->( baseSavePos())
   SUMP ->( dbSetOrder( 1))
   SUMP ->( dbSeek( sSchet, .T.))

   while SUMP ->schet == sSchet .AND. SUMP ->( !Eof())
           if opEqual( SUMP->typeOf, if(confUseAgent(), "6136", "2003"));
                           .Or. opEqual( SUMP->typeOf, if(confUseAgent(), "1060", "1007"))
                   if Len(aSchet) > 0 .And.;
                                   (i := aScan(aSchet, {|x| x[1] == SUMP->CRD_NUM}) ) > 0
                           aSchet[i,2] += SUMP->SUMMA
                   else
                           aAdd(aSchet, {SUMP->CRD_NUM, SUMP->SUMMA})
                   endif
           endif
           SUMP->(dbSkip())
   enddo
   SUMP ->( baseRestPos())

   if (i := Len(aSchet)) > 0
           aSchet := aSort( aSchet, {|x,y| x[1]<y[1]} )
           for j := 1 to i
                   if aSchet[j,2] = 0
                           aDel(aSchet, j)
                           --i
                   endif
           next
           aSize( aSchet, i)
           aEval( aSchet, {|x| x[2] := Round( x[2]/100, 2 )} )
   endif

return aSchet

// Add by TMV  21-05-2003
// Поэтапное погашение кредита или процентов по нему для нестраховой схемы
funcDef crdNoStrah With nSumma :N, aCred :A, aPct :A, sSchet :C, sPens :C, nTransh :N ;
        local nCred := 999999, nPct := 999999, rV := 0, iCred := 0, iPct := 0,;
        nTmp := 0
        
        iCred = aScan( aCred, {|x| x[1] == nTransh} )
        iPct = aScan( aPct, {|x| x[1] == nTransh} )
        if iPct > 0   // Погасить проценты
                nTmp = Min( aPct[iPct, 2], nSumma )
                crdRetPctNoStrah( sSchet, nTmp, sPens, aPct[iPct, 1] )
                if nTmp = aPct[iPct, 2]
                        aDel( aPct, iPct )
                        aSize( aPct, Len(aPct) - 1)
                else
                        aPct[iPct, 2] -= nTmp
                endif
                rV = nTmp
        endif
        if iCred > 0 .And. (nTmp := nSumma - nTmp) > 0
                nTmp = Min( aCred[iCred, 2], nTmp )
                crdRetCrNoStrah( sSchet, nTmp, sPens, aCred[iCred, 1] )
                if nTmp = aCred[iCred, 2]
                        aDel( aCred, iCred )
                        aSize( aCred, Len(aCred) - 1)
                else
                        aCred[iCred, 2] -= nTmp
                endif
                rV += nTmp
        endif

return rV

// Add by TMV  29-05-2003
// Заявления на открытие кред. линии и на изменение лимита
//
funcDef prnSetLimNoStrah With Summa :N, crdUpdate :LU, sNumDog :CU, dDate :DU ;
   local arr, str := '',;
        aDoc := razbivka( cliDocum(), {{ 40, "L"}, { 40, "L"}}),;
        addr := razbivka( cliFullAddress(), {{ 44, "L"}, { 44, "L"}}),;
        aSumma

   DEFAULT crdUpdate TO .F.
   DEFAULT sNumDog TO ""
   DEFAULT dDate TO CtoD("")

   str = getS( Summa )
   aSumma := razbivka( str, {{if( crdUpdate, 80, 70), "L"}, {80, "L"}} )
   arr := {{ "@atrname",  confFilial()   },;
           { "@filial",   confFilial()   },;
           { "@otdnumb",  confOtdNumb()  },;
           { "@otdname",  confOtdName()  },;
           { "@bankname", confBankName() },;
           { "@fio",      cliFullName()  },;
           { "@dogov",    sNumDog        },;
           { "@datdogov", DtoC(dDate)    },;
           { "@doc1",     aDoc[ 1]       },;
           { "@doc2",     aDoc[ 2]       },;
           { "@tel",      cliPhone()     },;
           { "@addr1",    addr[ 1]       },;
           { "@addr2",    addr[ 2]       },;
           { "@curdate",  setCurDate()   },;
           { "@strsumma1",   aSumma[1]   },;
           { "@strsumma2",   aSumma[2] }}

   str = extractStrOpen( if( crdUpdate, 'crlinup', 'crlin'))
   str = makeZ( str, arr)

   if confShowIt() == 2 then str = editMemo( str, .T.)
   doPrint( str, 1 )

return NIL

// Add by TMV  29-05-2003
// Заявления на выдачу кредита по кред. линии
//
funcDef prnGetLimCred With credSum :N, pctSum :N, sNumDog :C, dDate :D ;
   local arr, str := '',;
        aDoc := razbivka( cliDocum(), {{ 40, "L"}, { 40, "L"}}),;
        addr := razbivka( cliFullAddress(), {{ 44, "L"}, { 44, "L"}}),;
        aSumma, strSum

   str = getS( credSum )
   aSumma := razbivka( str, {{37, "L"}, {80, "L"}} )
   strSum = getS( pctSum )
   arr := {{ "@atrname",  confFilial()   },;
           { "@filial",   confFilial()   },;
           { "@otdnumb",  confOtdNumb()  },;
           { "@otdname",  confOtdName()  },;
           { "@bankname", confBankName() },;
           { "@fio",      cliFullName()  },;
           { "@dogov",    sNumDog        },;
           { "@datdogov", DtoC(dDate)    },;
           { "@doc1",     aDoc[ 1]       },;
           { "@doc2",     aDoc[ 2]       },;
           { "@tel",      cliPhone()     },;
           { "@addr1",    addr[ 1]       },;
           { "@addr2",    addr[ 2]       },;
           { "@curdate",  setCurDate()   },;
           { "@strcom",   strSum         },;
           { "@strsumma1",   aSumma[1]   },;
           { "@strsumma2",   aSumma[2] }}

   str = extractStrOpen( 'cropen')
   str = makeZ( str, arr)

   if confShowIt() == 2 then str = editMemo( str, .T.)
   doPrint( str, 1 )

return NIL

// Определение номера кредитного договора
//
funcDef getCrdNumNoStrah With sType :CU local rV := '', arr

   DEFAULT sType TO 'N'

   if Upper(sType) == 'N'
           rV = CLIENTS->CODE
   else
           rV = cliCrdDOpenNoStrah()
   endif

return rV

// Печать кредитного договора
//
funcDef prnCrdLinDog With credSum :N, schet :C, sNumDog :C, dDate :D, ;
        nPage :NU ;  // Add by TMV  01-12-2003
   local arr, str := '',;
        aDoc := razbivka( cliDocum(), {{ 50, "L"}, { 50, "L"}}),;
        addr := razbivka( cliFullAddress(), {{ 70, "L"}, { 70, "L"}}),;
        aSumma

   DEFAULT nPage TO 2

   str = getS( credSum )
   aSumma := razbivka( str, {{80, "L"}, {80, "L"}} )

   arr := {{ "@atrname",  confFilial()   },;
           { "@filial",   confFilial()   },;
           { "@otdnumb",  confOtdNumb()  },;
           { "@otdname",  confOtdName()  },;
           { "@bankname", confBankName() },;
           { "@bankdov",  confUprDover()  },;
           { "@fiobank",  confUprFilial() },;
           { "@fio",      cliFullName()  },;
           { "@dogov",    sNumDog        },;
           { "@datdogov", DtoC(dDate)    },;
           { "@schet",    schet          },;
           { "@doc1",     aDoc[ 1]       },;
           { "@doc2",     aDoc[ 2]       },;
           { "@tel",      cliPhone()     },;
           { "@addr1",    addr[ 1]       },;
           { "@addr2",    addr[ 2]       },;
           { "@curdate",  setCurDate()   },;
           { "@strsumma1",   aSumma[1]   },;
           { "@strsumma2",   aSumma[2] }}

   str = extractStrOpen( 'crzvlnew')
   str = makeZ( str, arr)

   if confShowIt() == 2 then str = editMemo( str, .T.)
   doPrint( str, nPage ) // Replace <1> to <nPAge> by TMV 01-12-2003

return NIL

// Add by TMV 30-05-2003
// Печать дополнения к кредитному договору
//
funcDef prnCrdLinPril With credSum :N, schet :C, sNumDog :C, dDate :D, ;
        nPage :NU ;
   local arr, str := '',;
        aDoc := razbivka( cliDocum(), {{ 50, "L"}, { 50, "L"}}),;
        addr := razbivka( cliFullAddress(), {{ 70, "L"}, { 70, "L"}}),;
        aSumma

   DEFAULT nPage TO 2

   str = getS( credSum )
   aSumma := razbivka( str, {{80, "L"}, {80, "L"}} )

   arr := {{ "@atrname",  confFilial()   },;
           { "@filial",   confFilial()   },;
           { "@otdnumb",  confOtdNumb()  },;
           { "@otdname",  confOtdName()  },;
           { "@bankname", confBankName() },;
           { "@bankdov",  confUprDover()  },;
           { "@fiobank",  confUprFilial() },;
           { "@fio",      cliFullName()  },;
           { "@dogov",    sNumDog        },;
           { "@datdogov", DtoC(dDate)    },;
           { "@schet",    schet          },;
           { "@doc1",     aDoc[ 1]       },;
           { "@doc2",     aDoc[ 2]       },;
           { "@tel",      cliPhone()     },;
           { "@addr1",    addr[ 1]       },;
           { "@addr2",    addr[ 2]       },;
           { "@curdate",  setCurDate()   },;
           { "@strsumma1",   aSumma[1]   },;
           { "@strsumma2",   aSumma[2] }}

   str = extractStrOpen( 'crzvlup')
   str = makeZ( str, arr)

   if confShowIt() == 2 then str = editMemo( str, .T.)
   doPrint( str, nPage ) // Replace <1> to <nPAge> by TMV 01-12-2003

return NIL

// Add by TMV  03-06-2003
// Запись пожелания по возврату ссуды по нестраховой схеме
//
funcDef crdWiscNoStrah with Summa :N, sSchet :C, nTransh :N, lSeek :LU ;
        local nSumma := 0, arr, sBuffer, rV := .F.,;
        docNumb := allTrim(CLIENTS->CODE) + DtoS(cliCrdDOpenNoStrah())

   DEFAULT lSeek To .F.

   if WISC ->(dbSeek( sSchet + "968 " ))
           do while WISC->Schet == sSchet .And. WISC->type == "968 " ;
                           .And. !WISC->(Eof())
                   arr := CtoA( WISC ->buffer, ";")
                   nSumma = Round(Val( arr[3] ), 2)
                   if Int( Val( arr[4] )) == nTransh
                           if !lSeek
                                   nSumma += Summa
                                   if nSumma <= 0
                                           WISC ->( mDelete())
                                   elseif wiscRLock()
                                           sBuffer = arr[1] + ";" + arr[2] + ";" + nStr(nSumma) + ";" +;
                                                   + Padl( allTrim(Str( nTransh )), 4, '0')
                                           WISC->Buffer := sBuffer
                                   endif
                                   wiscUnLock()
                           endif
                           rV = .T.
                           Exit
                   endif
                   WISC->(dbSkip())
           enddo
   endif
   if !rV .And. WISC ->( mAppend())
           WISC ->type   := "968"
           WISC ->schet  := sSchet
           WISC ->buffer := myDtoC( setCurDate()) + ";" + ;
                                   docNumb + ";" + ;
                                   nStr( Summa) + ";" + ;
                                   Padl( allTrim(Str( nTransh )), 4, '0') // Add by TMV 27-05-2003
           WISC ->( netUnLock())
           rV = .T.
   endif
return rV

