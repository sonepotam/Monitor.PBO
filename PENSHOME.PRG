*============================================================================
*    
*                  Программа : MON.EXE
*                  Файл      : pensHome.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: Работа с надомниками
*----------------------------------------------------------------------------
*
* homePrep()          -> NIL    печать ордеров надомников
* setTypeOf()         -> NIL    снятие/ установка признака type
*
* homeOrdPrint()      -> NIL    печать ордеров надомников
* homeProof()         -> NIL    печать ордеров
*
* homeOneOrder( nDoc, nazn, ro, sim,;           подготовка массива с ордером
*              nSumma, lPrintDate, lPrintPasp,; для печати или показа
*              dDate) -> aOrder
* printAOrder( aOrder)-> NIL        печать одного ордера
*
* printHistory( schet, dDate) ->NIL печать выписки ( не реализовано )
* getHistory( schet, dDate)   ->str получить строку выписки
*----------------------------------------------------------------------------

* 30-07-2002 TMV
* Исправлена ф-ия homePrep. В блок для поля СУММА добавлены блоки кода для
* блокировки-разблокировки записи при редактировании суммы
*
* 21-10-2002 TMV
* Добавлена обработка поля KODYL в проц-ру homePrep.
*
* 22-11-2002 TMV
* В ф-ии homeOneOrder() добавлен необязательный параметр - счет дебета в
*  ордере. Если не указан - печатается как ранее ACC->Schet
* При формировании ордеров надомников исключаются умершие клиенты.
*  homePrep(), homeProof()
* Если не подписано заявление - оно печатается вместо ордеров - homeOrdPrint().
*
* 27-11-2002 TMV
* В ф-ии установки подписи на заявлении клиента добавлен режим просмотра
* homeSetSigned()
*
* 09-12-2002 TMV
* В ф-ии homeOrdPrint() введена разбивка на Влад/неВлад. для сортировки
*  ордеров надомников.
*
* 16-12-2002 TMV
* Изменена выписка по счету для новых проводок выдачи ссуды.
* Ф-ия getHistory(), новая ф-ия getEqualSums().
* В ф-ии getHistLine() введен необязательный параметр.
*
* 17-12-2002 TMV
* Заменены 6136 и 6137 на 2060 в getHistory(), getEqualSums()
*-- ОТМЕНЕНО
*
* 18-12-2002 TMV
* Изменена печать выписки с операциями по ссудам - getHistory()
*
* 05-01-2003  TMV
* Исправлена процедура расчета общей суммы ордеров надомников - getDistrInfo()
* Введена ф-ия чтения настройки по KODYL. Ф-ия homePrep()
* Введена настройка порядка сортировки ордеров надомников - homeOrdPrint()
*
* 27-02-2003  TMV
* Печать ордеров надомников для Владика переделана - ордера (поручения)
*  печатаются на одном листе с заявлением на открытие счета.
*  Ф-ии homeOrdPrint()
*
* 28-02-2003  TMV
* Доработка печати ордеров надомников - для режима "Зарезервированных счетов"
*  Ф-ии homeOrdPrint()
*
* 03-03-2003  TMV
* При печати ордеров надомников исправлена ошибка - НЕ печатается ордер, если
*  сумма ордера равна 0. Ф-ия homeOrdPrint()
*
* 07-03-2003  TMV
* Исправлены ошибки (переполнение стека при печати ордеров надомников).
* Убран запрос на "подписаность" заявления при списании ордеров надомников.
* Сортировка при просмотре ордеров сделана так же, как и при печати.
* Ф-ия homePrep(), homeOrdPrint()
*
* 01-07-2003  TMV
* При печати ордеров надомников для Владика заблокирована печать заявлений на
* открытие счетов 42309%
*
* 02-09-2004 TAT
* printAOrder: reset принтера после печати 
* (для мгновенной печати страницы)


#include "common.ch"
#include "clippExt.ch"
#include "inkey.ch"

#include "mon.ch"
memVar getList

* NEW OBJECT FILE
//
// Вывод на экран список ордеров
// Пока что выводим только ранее не напечатанные ордера
//
funcDef homePrep ;
   local tmp, aH, aC, fk, dDate, theDay, i, s := select(), theRest, minSumma ,;
         useHome := confIsHome(), distr, usePaymDate := confUsePaymDate() < 2,;
         aGets,;
			lKodYl := useKodYl(),; // Add by TMV  21-10-2002
		   isWorkAccReserv := workAccReserv()  // Add by TMV  22-11-2002


distr := setUpValue( "DISTR")
if empty( distr)
   messageB( "Нет настройки участков DISTR !!! Обратитесь на филиал !!!")
   return .F.
endif

// Edited by TMV  21-10-2002
if lKodYl   // Add by TMV  21-10-2002
	tmp := { ".\TMP.DBF", "TMP",;
   	      {{ "fio",       "C", 40, 0} ,;
      	    { "summa",     "N", 15, 2} ,;
         	 { "schet",     "C", 20, 0} ,;
	          { "Type",      "N",  1, 0} ,;
   	       { "distrCode", "N",  2, 0} ,;
      	    { "cliRecNo",  "N", 15, 0} ,;
         	 { "accRecNo",  "N", 15, 0} ,;
				 { "Street",    "C", 30, 0} ,;  // Add by TMV  13-12-2001
				 { "House",     "C", 10, 0} ,;  // Add by TMV  13-12-2001
				 { "Flat",      "C",  4, 0} ,;  // Add by TMV  13-12-2001
				 { "Block",     "C",  2, 0} ,;  // Add by TMV  13-12-2001
				 { "KODYL",     "N",  4, 0}},;  // Add by TMV  21-10-2002
	         {{ , "fio"}                 ,;
   	       { , "eStr( distrCode, 5) + fio"},;
				 { , "eStr( distrCode, 5) + Str(KODYL, 4) + Street + Padl(allTrim(House),3,' ') + Padl(allTrim(Block),3,' ') + Padl(allTrim(Flat),4,' ')"}},; // Add by TMV  13-01-2002
				 0, .T.}
else
	tmp := { ".\TMP.DBF", "TMP",;
   	      {{ "fio",       "C", 40, 0} ,;
      	    { "summa",     "N", 15, 2} ,;
         	 { "schet",     "C", 20, 0} ,;
	          { "Type",      "N",  1, 0} ,;
   	       { "distrCode", "N",  2, 0} ,;
      	    { "cliRecNo",  "N", 15, 0} ,;
         	 { "accRecNo",  "N", 15, 0} ,;
				 { "Street",    "C", 30, 0} ,;  // Add by TMV  13-12-2001
				 { "House",     "C", 10, 0} ,;  // Add by TMV  13-12-2001
				 { "Flat",      "C",  4, 0} ,;  // Add by TMV  13-12-2001
				 { "Block",     "C",  2, 0}},;  // Add by TMV  13-12-2001
	         {{ , "fio"}                 ,;
   	       { , "eStr( distrCode, 5) + fio"},;
				 { , "eStr( distrCode, 5) + Street + Padl(allTrim(House),3,' ') + Padl(allTrim(Block),3,' ') + Padl(allTrim(Flat),4,' ')"}},; // Add by TMV  13-01-2002
				 0, .T.}
endif
// Edited by TMV  21-10-2002 -- End

/*********************************/
// Mike !!!!!!!!!!!!!!!!!!!!!!!!!!
tmp[1] := ".\TMP" + thisStation() + ".DBF"
// Mike !!!!!!!!!!!!!!!!!!!!!!!!!!
/*********************************/

basePath( tmp, confPathTemp())

dDate  := setCurDate()
theDay := Day( dDate)
aGets  := {{ "Печать ордеров надомников"          },;
           { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }, NIL}
if usePaymDate
   aGets[ 3] := { "Введите дату последнего зачисления", block( dDate)}
else
   aGets[ 3] := { "Введите день выплаты", block( theDay), "99" }
endif
if !getUp( 5, 5, aGets) then return NIL

if baseCreate( tmp)
   if baseMXOpen( tmp) .and. distrOpen()  // Mike !!!!!!!!!!!!!!!!!!!
      minSumma := confMinSumma()

      DISTR -> ( dbSetOrder( 1)) // Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!
      ACC     ->( baseSavePos())
      CLIENTS ->( baseSavePos())
      CLIENTS ->( dbSetOrder( 1))
      ACC     ->( dbGoTop())

      operStartUp( "Подготовка массива надомников", i := 1, ACC ->( lastRec()))
      while ACC ->( !eof())
        if if( usePaymDate,;
                 ACC ->Paym_Date == dDate,;
                 val( subStr( ACC ->buffer, 2, 2)) == theDay);
           .AND. ( accIsHome() .OR. useHome )
           if ((theRest := sumRest( ACC ->schet, setCurDate() )) >0 .AND. theRest >= minSumma);
           		.Or. (isWorkAccReserv .And. !accIsSigned()) // Add by TMV  22-11-2002
              if cliSeek( ACC ->code)
                 if MikeChkDistr( CLIENTS ->distrCode);  // Mike !!!!!!!!!!!!!!!!!!!
							.And. if(isWorkAccReserv, !isAcctDead(ACC ->schet), .T.)  // Add by TMV  22-11-2002
                    TMP ->( dbAppend())
                    TMP ->fio := cliFullName()
                    TMP ->summa     := theRest
                    TMP ->schet     := ACC ->schet
                    TMP ->distrCode := CLIENTS ->distrCode
                    TMP ->cliRecNo  := CLIENTS ->( recNo())
                    TMP ->accRecNo  := ACC     ->( recNo())
                    TMP ->Street    := CLIENTS ->Street   // Add by TMV  13-12-2001
                    TMP ->House     := CLIENTS ->House    // Add by TMV  13-12-2001
                    TMP ->Block     := CLIENTS ->Block    // Add by TMV  13-12-2001
                    TMP ->Flat      := CLIENTS ->Flat     // Add by TMV  13-12-2001
						  if lKodYl                          // Add by TMV  21-10-2002
                       TMP ->KODYL  := CLIENTS ->KODYL // Add by TMV  21-10-2002   
						  endif                              // Add by TMV  21-10-2002
                 endif
              endif
           endif
        endif
        skip 1 alias ACC
        operProc( i++)
      enddo
      operExit()
      CLIENTS ->( baseRestPos())
      ACC     ->( baseRestPos())

      aH := {{ "Список ордеров для печати на " + DtoC( setCurDate()) },;
             { "День выплаты пенсии - " + nStr( theDay)            },;
             { "<ПРОБЕЛ>  - установить/ снять отментку  "          },;
             { "Серый плюс/минус  - установить/ снять все отметки" },;
             { "Сумма", {|| TMP ->summa},,{|| .F.}                 },;
             { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}
      if usePaymDate then ;
         aH[ 2] := { "Дата зачисления - " + DtoC( dDate)}
      aC := {{ "Печать" ,  { , block( TMP ->type), { " ", "*"}} } ,;
             { "Надомник", {|| TMP ->fio         }}  ,;
             { "Счет",     {|| TMP ->schet       }}  ,;
             { "Сумма   ", { , block( TMP ->summa),,, ;
	             {|g| chckSumma( g, dDate, minSumma)}}, .T.,, ;
					 {|g|TMP->(rLock())}, {|g|TMP->(dbunlock())} }} && Add by TMV 30-07-2002

      fk := {,{{ "Итог",    {|| itog()       }}} }


      TMP ->( netGoTop())
      if TMP ->( !emptyRec())

         setKey( K_SPACE,      {|| setTypeOf() })
         setKey( K_GREY_PLUS,  {|| setAllOn()  })
         setKey( K_GREY_MINUS, {|| setAllOff() })

         TMP ->( browseUp( 2, 2, aH, aC       ,;
                 {|| netSkipUp()  }, {|| netSkipDown()}  ,;
                 {|| netGoTop()   }, {|| netGoBottom()},,,,, fk))
         setKey( K_SPACE, NIL)
         setKey( K_GREY_PLUS,  NIL)
         setKey( K_GREY_MINUS, NIL)

         if yesNo( if( conf904Work(), "Печатать поручения ?",;
                       "Печать ордера ?")) then homeOrdPrint( setCurDate())
      else
         errLog( "Нет данных !")
      endif
      baseMClose( tmp)
      distrClose() // Mike !!!!!!!!!!!!!!!!!!!!!!!!!!
   endif
   baseDelete( tmp)
endif
select ( s)
return NIL

funcDef setAllOn

messageStartUp( "Установка признака")
TMP ->( baseSavePos())
TMP ->( netGoTop())
while TMP ->( !eof())
		do while !TMP->(rlock())
			inkey(.2)
		enddo
      TMP ->type := 2
		TMP->(dbunlock())
      skip 1 alias TMP
enddo
TMP ->( baseRestPos())
getCurOB(): refreshAll(): forceStable()
messageExit( 0)

return NIL

funcDef setAllOff

messageStartUp( "Снятие признака")
TMP ->( baseSavePos())
TMP ->( netGoTop())
while TMP ->( !eof())
		do while !TMP->(rlock())
			inkey(.2)
		enddo
      TMP ->type := 0
		TMP->(dbunlock())
      skip 1 alias TMP
enddo
TMP ->( baseRestPos())
getCurOB(): refreshAll(): forceStable()
messageExit( 0)

return NIL


//
// проверим была ли выплачена пенсия для надомника ы этом же месяце
//
static funcDef chckDates with dDate:D local rV

rV := dDate >= ACC ->datPens .AND. ;
      firstDay( dDate) <> firstDay( ACC ->datPens)

return rV


funcDef setTypeOf

do while !TMP->(rlock())  //  Add by TMV 23-10-2001
	inkey(.2)
enddo
if TMP ->type < 2
   TMP ->type := 2
else
   TMP ->type := 1
endif
TMP->(dbunlock())         //  Add by TMV 23-10-2001
// TMP ->( netSkipDown())
getCurOB(): Down()
getCurOB(): refreshAll(): forceStable()

return NIL

static funcDef chckSumma with g:O, dDate:D, minSumma:N local rV := .T., rest

rest := sumRest( TMP ->schet, dDate)
if int(round(100 * g:varGet(), 0)) > int(round(100 * rest, 0))
   errLog( { "Вы пытаетесь снять со счета " + splitSSchet( TMP ->schet) ,;
            "сумму " + mStr( g:varGet())                 ,;
            ", а остаток на нем " + mStr( rest) })
   rV := .F.
endif
if g:varGet() < minSumma
   errLog( { "Вы пытаетесь выплатить     " + mStr( g:varGet()) ,;
            "а минимальная сумма ордера " + mStr( minSumma) })
   rV := .F.
endif
return rV

***********

//
// печать ордеров надомников
//
funcDef homeOrdPrint with dDate:D ;
        local i, aPrnOrder, file := ".\orders.", nazn ,;
              aCode, aDistr, tmpDistr, lPrintDate  ,;
              theCode := ORD_33_DATE, ro, sim      ,;
              tmpSumma := 0, tmpCnt := 0           ,;
              totSumma := 0, totCnt := 0, theOrder ,;
              isFirst, canPrint := .T., printNow,;
				  lPrnPasQuest := .F.,;
				  isWorkAccReserv := workAccReserv(),;  // Add by TMV  22-11-2002
				  sZayav := '',;  // Add by TMV  27-02-2003
				  iOrdAcc := ACC->(indexOrd()),;      // Add by TMV 23-01-2004
				  iOrdCli := CLIENTS->(indexOrd())    // Add by TMV 23-01-2004

usePrnReset( .F.)
// lPrintDate := yesNo( "Печатать дату в расходных ордерах ?")
if confUseDate()
   lPrintDate := .T.
elseif isWorkAccReserv   // Add by TMV  22-11-2002
   lPrintDate := .F.     // Add by TMV  22-11-2002
else
   lPrintDate := yesNo( "Печатать дату в расходных ордерах ?")
endif

do case
   //
   // выплатное поручение с датой
   //
   case conf904Work()  .AND.  lPrintDate do theCode := ORD_904_DATE
   //
   // выплатное поручение без даты
   //
   case conf904Work()  .AND. !lPrintDate do theCode := ORD_904_NODATE
   //
   // ордер надомнику с датой
   //
   case !conf904Work() .AND. lPrintDate  do theCode := ORD_33_DATE
   //
   // ордер надомнику без даты
   //
   case !conf904Work() .AND. !lPrintDate do theCode := ORD_33_NODATE
endcase


printNow := !noYes( "Печатать в файл ?")

distrArray( @aDistr, @aCode)

file += padL( Day( dDate), "0", 2)
file = newFPath( file, confPathTemp())
#ifdef VLAD
	nazn = "Выплата средств с пенсионного счета" // "/на дому/" Edited by TMV 25-10-2001
#else
	nazn = "Выплата средств с пенсионного счета /на дому/"
#endif

// Removed by TMV  23-01-2004
ACC     ->( baseSavePos())
CLIENTS ->( baseSavePos())

TMP     ->( baseSavePos())  // Add by TMV  07-03-2003

ACC     ->( dbSetOrder( 1))
CLIENTS ->( dbSetOrder( 1))

#ifdef VLAD  // Add by TMV  09-12-2002
	TMP ->( dbSetOrder( 3))   // Replaced 2 to 3 by TMV  13-12-2001
#else        // Add by TMV  09-12-2002
//   Replaced by TMV  05-01-2003
//	TMP ->( dbSetOrder( 2))   // Add by TMV  09-12-2002
	TMP ->( dbSetOrder( confSortOrd() ))   // Add by TMV  09-12-2002
#endif       // Add by TMV  09-12-2002

TMP ->( netGoTop())
operStartUp( "Печать ордеров надомников", i := 1, TMP ->( lastRec()))

operGetInfo( theCode, @ro, @sim)
while TMP ->( !eof())


      tmpDistr := TMP ->distrCode
      getDistrInfo( tmpDistr, dDate, @tmpSumma, @tmpCnt)
      totSumma += tmpSumma
      totCnt   += tmpCnt

      isFirst  = .T.

      while tmpDistr == TMP ->distrCode .AND. TMP ->( !eof())

		sZayav = ''  // Add by TMV  07-03-2003
		aPrnOrder := {} // Add by TMV  07-03-2003

      if TMP ->type == 2
         if ACC ->( netSeek( TMP ->schet))

            CLIENTS ->( dbGoTo( TMP ->cliRecNo))
            if isFirst .AND. printNow
               printAOrder( { "", "", "", "", "",;
                              "---------------------------------------------------------------------------",;
                              "", "", "", "", "",;
                              "Выплата пенсий надомникам" ,;
                    "Участок доставки " + aDistr[ max( 1, aScan( aCode, {|x| x == TMP ->distrCode})) ],;
                    "Дата    : " + DtoC( dDate),;
                    "Ордеров : " + nStr( tmpCnt) + "     На общую сумму : " + mStr( tmpSumma) + " руб." ,;
                    "---------------------------------------------------------------------------",;
                    "", "" })
               isFirst = .F.
            endif


// Работа с зарезервированными счетами

// Add by TMV  22-11-2002
            if isWorkAccReserv .And.;                 // Add by TMV  22-11-2002
						!accIsSigned()                      // Add by TMV  22-11-2002

#ifndef VLAD  // Add by TMV  28-02-2003

      			if printOpen(, 1) then accSetPrinted() // Add by TMV  22-11-2002

// Add by TMV  28-02-2003 - Begin
#else  // Add by TMV  28-02-2003
				
					if TMP ->summa > 0  // Add by TMV  03-03-2003
						theOrder = ordIncOut( dDate)

   		         aPrnOrder := homeOneOrder( cliFullName(), cliFullAddress()       ,;
      		        	cliDocum(), theOrder,nazn, ro, sim, TMP ->summa, lPrintDate,;
         		     	.T., dDate, conf904Work())

						if Left(ACC->Schet, 5) != '42309'  // Add by TMV 01-07-2003
		     				if printOpen(, 1,,.F., @sZayav)
								accSetPrinted()
								aAdd( aPrnOrder, replicate('=', 75) )
								aAdd( aPrnOrder, ' ' )
								aAdd( aPrnOrder, sZayav )
							endif
						else                  // Add by TMV  09-07-2003
							aPrnOrder := {}    // Add by TMV  09-07-2003
						endif                              // Add by TMV 01-07-2003

					else                // Add by TMV  03-03-2003
						if Left(ACC->Schet, 5) != '42309'  // Add by TMV 01-07-2003
		     				if printOpen(, 1) then accSetPrinted() // Add by TMV  03-03-2003
						endif
					endif                                     // Add by TMV  03-03-2003

					lPrnPasQuest = (empty(cliPasSer1()) .Or. empty(cliPasSer2()) .Or.;
						empty(cliPasNum()))
            //
            // делаем проводку
            //

					if TMP ->summa > 0;  // Add by TMV  03-03-2003
						   .And. Len(aPrnOrder) > 0  // Add by TMV  09-07-2003
		            if cliRLock() .AND. sumRest( TMP ->schet, dDate) >= TMP ->summa
               //
               // установим дату последней выплаты пенсии
               //
		               ACC ->( dbGoTo( TMP ->accRecNo))
  			            if printNow
								printAOrder( aPrnOrder)
  	   		            printAdv2( .F.)
     	   		         if lPrintDate  //  Moved by TMV from up  03-03-2003
        	   		         printHistory( TMP ->schet, dDate, - TMP ->summa)
           	   		   else
              	   		   printHistory( TMP ->schet, dDate,, .F.)
	                  	endif
								if lPrnPasQuest then prnPasQuest(cliFullName()) //  Add by TMV  10-12-2001
     		   	      endif
        		   	   if accRLock();  ACC ->datPens := dDate;  accUnLock();  endif
           		   	if !sumWrite( TMP ->schet, - TMP ->summa, dDate, theCode, theOrder, TMP ->distrCode)
            		      errLog({ "Проводка по клиенту " + TMP->fio + " НЕ СДЕЛАНА !",;
 	            		           "", "Счет " + TMP ->schet                          ,;
    	            		        "Сумма " + mStr( TMP ->summa), "", ""              ,;
       	            		     "Повторите печать ордера в индивидуальном режиме"})
	            	   endif
		            else
  			           if int(round(100 * sumRest( TMP ->schet, dDate), 0)) < int(round(100 * TMP ->summa, 0))
     			           errLog( { "Вы пытаетесь снять со счета " + TMP ->schet ,;
        			                 "сумму " + mStr( TMP ->summa)                ,;
           			              ", а остаток на нем " + mStr( sumRest( TMP ->schet, dDate)) })
              			else
								errLog( "Ордер по счету " + TMP ->schet + " не дейсвителен !")
	   	            endif
  		   	      endif
     		   	   CLIENTS ->( netUnLock())
					endif               // Add by TMV  03-03-2003
#endif
// Add by TMV  28-02-2003 -- End
// Работа с зарезервированным счетом -- End

				else                                      // Add by TMV  22-11-2002
// Работа с обычным счетом
				theOrder := ordIncOut( dDate)

            aPrnOrder := homeOneOrder( cliFullName(), cliFullAddress()       ,;
              	cliDocum(), theOrder,nazn, ro, sim, TMP ->summa, lPrintDate,;
              	.T., dDate, conf904Work())

				lPrnPasQuest = (empty(cliPasSer1()) .Or. empty(cliPasSer2()) .Or.;
					empty(cliPasNum()))
            //
            // делаем проводку
            //
            if cliRLock() .AND. sumRest( TMP ->schet, dDate) >= TMP ->summa
               //
               // установим дату последней выплаты пенсии
               //
               ACC ->( dbGoTo( TMP ->accRecNo))
               if printNow

                  if lPrintDate
                     printHistory( TMP ->schet, dDate, - TMP ->summa)
                  else
                     printHistory( TMP ->schet, dDate,, .F.)
                  endif
                  printAOrder( aPrnOrder)
                  printAdv2( .F.)
						if lPrnPasQuest then prnPasQuest(cliFullName()) //  Add by TMV  10-12-2001
               endif
               if accRLock();  ACC ->datPens := dDate;  accUnLock();  endif
               if !sumWrite( TMP ->schet, - TMP ->summa, dDate, theCode, theOrder, TMP ->distrCode)
                  errLog({ "Проводка по клиенту " + TMP->fio + " НЕ СДЕЛАНА !",;
                          "", "Счет " + TMP ->schet                          ,;
                          "Сумма " + mStr( TMP ->summa), "", ""              ,;
                          "Повторите печать ордера в индивидуальном режиме"})
               endif
            else
              if int(round(100 * sumRest( TMP ->schet, dDate), 0)) < int(round(100 * TMP ->summa, 0))
                 errLog( { "Вы пытаетесь снять со счета " + TMP ->schet ,;
                          "сумму " + mStr( TMP ->summa)                ,;
                          ", а остаток на нем " + mStr( sumRest( TMP ->schet, dDate)) })
               else
                 errLog( "Ордер по счету " + TMP ->schet + " не дейсвителен !")
               endif
            endif
            CLIENTS ->( netUnLock())

				endif // Add by TMV  22-11-2002 Работа с зарезервированными счетами

         endif
      endif

		skip 1 alias TMP
      operProc( i++)
      enddo
enddo
//prnReset()
if printNow
   printAOrder( { "", "", "", "",;
      "---------------------------------------------------------------------------",;
      "Дата      : " + DtoC( dDate),;
      "Отделение : " + confOtdNumb()                ,;
      "Выплата пенсий надомникам ( всего ордеров )" ,;
      "Ордеров : " + nStr( totCnt) + "     На общую сумму : " + mStr( totSumma) + " руб." ,;
      "---------------------------------------------------------------------------",;
      "", "" })
  // prnReset()
endif

operExit()
usePrnReset( .T.)

// Replaced by TMV  23-01-2004
CLIENTS ->( baseRestPos())
ACC     ->( baseRestPos())

TMP ->( dbSetOrder( 1))
TMP ->( netGoTop())

if !conf904Work()
   if totSumma > kasRest()
      errLog( { "Вы распечатали ордеров на сумму " + mStr( totSumma)  ,;
               "Остаток в кассе                 " + mStr( kasRest()) })
   endif
endif

return NIL

***********
/*
//
// печать ордеров надомников
//
funcDef homeOrdPrint with dDate:D ;
        local i, aPrnOrder, file := ".\orders.", nazn ,;
              aCode, aDistr, tmpDistr, lPrintDate  ,;
              theCode := ORD_33_DATE, ro, sim      ,;
              tmpSumma := 0, tmpCnt := 0           ,;
              totSumma := 0, totCnt := 0, theOrder ,;
              isFirst, canPrint := .T., printNow,;
				  lPrnPasQuest := .F.,;
				  isWorkAccReserv := workAccReserv(),;  // Add by TMV  22-11-2002
				  sZayav := '',;  // Add by TMV  27-02-2003
				  iOrdAcc := ACC->(indexOrd()),;      // Add by TMV 23-01-2004
				  iOrdCli := CLIENTS->(indexOrd())    // Add by TMV 23-01-2004

usePrnReset( .F.)
// lPrintDate := yesNo( "Печатать дату в расходных ордерах ?")
if confUseDate()
   lPrintDate := .T.
elseif isWorkAccReserv   // Add by TMV  22-11-2002
   lPrintDate := .F.     // Add by TMV  22-11-2002
else
   lPrintDate := yesNo( "Печатать дату в расходных ордерах ?")
endif

do case
   //
   // выплатное поручение с датой
   //
   case conf904Work()  .AND.  lPrintDate do theCode := ORD_904_DATE
   //
   // выплатное поручение без даты
   //
   case conf904Work()  .AND. !lPrintDate do theCode := ORD_904_NODATE
   //
   // ордер надомнику с датой
   //
   case !conf904Work() .AND. lPrintDate  do theCode := ORD_33_DATE
   //
   // ордер надомнику без даты
   //
   case !conf904Work() .AND. !lPrintDate do theCode := ORD_33_NODATE
endcase


printNow := !noYes( "Печатать в файл ?")

distrArray( @aDistr, @aCode)

file += padL( Day( dDate), "0", 2)
file = newFPath( file, confPathTemp())
nazn = "Выплата средств с пенсионного счета /на дому/"

// Removed by TMV  23-01-2004
ACC     ->( baseSavePos())
CLIENTS ->( baseSavePos())

TMP     ->( baseSavePos())  // Add by TMV  07-03-2003

ACC     ->( dbSetOrder( 1))
CLIENTS ->( dbSetOrder( 1))

TMP ->( dbSetOrder( confSortOrd() ))   // Add by TMV  09-12-2002

TMP ->( netGoTop())
operStartUp( "Печать ордеров надомников", i := 1, TMP ->( lastRec()))

operGetInfo( theCode, @ro, @sim)
while TMP ->( !eof())


      tmpDistr := TMP ->distrCode
      getDistrInfo( tmpDistr, dDate, @tmpSumma, @tmpCnt)
      totSumma += tmpSumma
      totCnt   += tmpCnt

      isFirst  = .T.

      while tmpDistr == TMP ->distrCode .AND. TMP ->( !eof())

		sZayav = ''  // Add by TMV  07-03-2003
		aPrnOrder := {} // Add by TMV  07-03-2003

      if TMP ->type == 2
         if ACC ->( netSeek( TMP ->schet))

            CLIENTS ->( dbGoTo( TMP ->cliRecNo))
            if isFirst .AND. printNow
               printAOrder( { "", "", "", "", "",;
                              "---------------------------------------------------------------------------",;
                              "", "", "", "", "",;
                              "Выплата пенсий надомникам" ,;
                    "Участок доставки " + aDistr[ max( 1, aScan( aCode, {|x| x == TMP ->distrCode})) ],;
                    "Дата    : " + DtoC( dDate),;
                    "Ордеров : " + nStr( tmpCnt) + "     На общую сумму : " + mStr( tmpSumma) + " руб." ,;
                    "---------------------------------------------------------------------------",;
                    "", "" })
               isFirst = .F.
            endif


// Работа с зарезервированными счетами

// Add by TMV  22-11-2002
            if isWorkAccReserv .And.;                 // Add by TMV  22-11-2002
						!accIsSigned()                      // Add by TMV  22-11-2002

				
					if TMP ->summa > 0  // Add by TMV  03-03-2003
						theOrder = ordIncOut( dDate)

   		         aPrnOrder := homeOneOrder( cliFullName(), cliFullAddress()       ,;
      		        	cliDocum(), theOrder,nazn, ro, sim, TMP ->summa, lPrintDate,;
         		     	.T., dDate, conf904Work())

						if Left(ACC->Schet, 5) != '42309'  // Add by TMV 01-07-2003
		     				if printOpen(, 1,,.F., @sZayav)
								accSetPrinted()
								aAdd( aPrnOrder, replicate('=', 75) )
								aAdd( aPrnOrder, ' ' )
								aAdd( aPrnOrder, sZayav )
							endif
						else                  // Add by TMV  09-07-2003
							aPrnOrder := {}    // Add by TMV  09-07-2003
						endif                              // Add by TMV 01-07-2003

					else                // Add by TMV  03-03-2003
						if Left(ACC->Schet, 5) != '42309'  // Add by TMV 01-07-2003
		     				if printOpen(, 1) then accSetPrinted() // Add by TMV  03-03-2003
						endif
					endif                                     // Add by TMV  03-03-2003

					lPrnPasQuest = (empty(cliPasSer1()) .Or. empty(cliPasSer2()) .Or.;
						empty(cliPasNum()))
            //
            // делаем проводку
            //

					if TMP ->summa > 0;  // Add by TMV  03-03-2003
						   .And. Len(aPrnOrder) > 0  // Add by TMV  09-07-2003
		            if cliRLock() .AND. sumRest( TMP ->schet, dDate) >= TMP ->summa
               //
               // установим дату последней выплаты пенсии
               //
		               ACC ->( dbGoTo( TMP ->accRecNo))
  			            if printNow
								printAOrder( aPrnOrder)
  	   		            printAdv2( .F.)
     	   		         if lPrintDate  //  Moved by TMV from up  03-03-2003
        	   		         printHistory( TMP ->schet, dDate, - TMP ->summa)
           	   		   else
              	   		   printHistory( TMP ->schet, dDate,, .F.)
	                  	endif
								if lPrnPasQuest then prnPasQuest(cliFullName()) //  Add by TMV  10-12-2001
     		   	      endif
        		   	   if accRLock();  ACC ->datPens := dDate;  accUnLock();  endif
           		   	if !sumWrite( TMP ->schet, - TMP ->summa, dDate, theCode, theOrder, TMP ->distrCode)
            		      errLog({ "Проводка по клиенту " + TMP->fio + " НЕ СДЕЛАНА !",;
 	            		           "", "Счет " + TMP ->schet                          ,;
    	            		        "Сумма " + mStr( TMP ->summa), "", ""              ,;
       	            		     "Повторите печать ордера в индивидуальном режиме"})
	            	   endif
		            else
  			           if int(round(100 * sumRest( TMP ->schet, dDate), 0)) < int(round(100 * TMP ->summa, 0))
     			           errLog( { "Вы пытаетесь снять со счета " + TMP ->schet ,;
        			                 "сумму " + mStr( TMP ->summa)                ,;
           			              ", а остаток на нем " + mStr( sumRest( TMP ->schet, dDate)) })
              			else
								errLog( "Ордер по счету " + TMP ->schet + " не дейсвителен !")
	   	            endif
  		   	      endif
     		   	   CLIENTS ->( netUnLock())
					endif               // Add by TMV  03-03-2003
// Add by TMV  28-02-2003 -- End
// Работа с зарезервированным счетом -- End

				else                                      // Add by TMV  22-11-2002
// Работа с обычным счетом
				theOrder := ordIncOut( dDate)

            aPrnOrder := homeOneOrder( cliFullName(), cliFullAddress()       ,;
              	cliDocum(), theOrder,nazn, ro, sim, TMP ->summa, lPrintDate,;
              	.T., dDate, conf904Work())

		lPrnPasQuest = (empty(cliPasSer1()) .Or. empty(cliPasSer2()) .Or.;
					empty(cliPasNum()))
            //
            // делаем проводку
            //
            if cliRLock() .AND. sumRest( TMP ->schet, dDate) >= TMP ->summa
               //
               // установим дату последней выплаты пенсии
               //
               ACC ->( dbGoTo( TMP ->accRecNo))
               if printNow

                  if lPrintDate
                     printHistory( TMP ->schet, dDate, - TMP ->summa)
                  else
                     printHistory( TMP ->schet, dDate,, .F.)
                  endif
                  printAOrder( aPrnOrder)
                  printAdv2( .F.)

						if lPrnPasQuest then prnPasQuest(cliFullName()) //  Add by TMV  10-12-2001
               endif
               if accRLock();  ACC ->datPens := dDate;  accUnLock();  endif
               if !sumWrite( TMP ->schet, - TMP ->summa, dDate, theCode, theOrder, TMP ->distrCode)
                  errLog({ "Проводка по клиенту " + TMP->fio + " НЕ СДЕЛАНА !",;
                          "", "Счет " + TMP ->schet                          ,;
                          "Сумма " + mStr( TMP ->summa), "", ""              ,;
                          "Повторите печать ордера в индивидуальном режиме"})
               endif
            else
              if int(round(100 * sumRest( TMP ->schet, dDate), 0)) < int(round(100 * TMP ->summa, 0))
                 errLog( { "Вы пытаетесь снять со счета " + TMP ->schet ,;
                          "сумму " + mStr( TMP ->summa)                ,;
                          ", а остаток на нем " + mStr( sumRest( TMP ->schet, dDate)) })
               else
                 errLog( "Ордер по счету " + TMP ->schet + " не дейсвителен !")
               endif
            endif
            CLIENTS ->( netUnLock())

				endif // Add by TMV  22-11-2002 Работа с зарезервированными счетами

         endif
      endif

		skip 1 alias TMP
      operProc( i++)
      enddo
enddo
//prnReset()
if printNow
   printAOrder( { "", "", "", "",;
      "---------------------------------------------------------------------------",;
      "Дата      : " + DtoC( dDate),;
      "Отделение : " + confOtdNumb()                ,;
      "Выплата пенсий надомникам ( всего ордеров )" ,;
      "Ордеров : " + nStr( totCnt) + "     На общую сумму : " + mStr( totSumma) + " руб." ,;
      "---------------------------------------------------------------------------",;
      "", "" })
  // prnReset()
endif

operExit()
usePrnReset( .T.)

// Replaced by TMV  23-01-2004
CLIENTS ->( baseRestPos())
ACC     ->( baseRestPos())

TMP ->( dbSetOrder( 1))
TMP ->( netGoTop())

if !conf904Work()
   if totSumma > kasRest()
      errLog( { "Вы распечатали ордеров на сумму " + mStr( totSumma)  ,;
               "Остаток в кассе                 " + mStr( kasRest()) })
   endif
endif

return NIL
*/

//
// подсчет кол-ва ордеров и их суммы
//
static func getDistrInfo( theDistr, theDate, tmpSumma, tmpCnt, n1, n2, cmp2)
local lCompare := !isNIL( n1) .AND. !isNIL( n2),;
   isWorkAccReserv := workAccReserv()  // Add by TMV  22-11-2002

tmpSumma := tmpCnt := 0
TMP ->( baseSavePos())

// DEFAULT cmp2 TO .T.

while theDistr == TMP ->distrCode .AND. TMP ->( !eof()) .AND. ;
      if( lCompare, TMP ->nDoc >= n1 .AND. TMP ->nDoc <= n2, .T.)
   if (( TMP ->type == 2 .AND. sumRest( TMP ->schet, theDate) >= TMP ->summa ) .OR. ;
         ( lCompare .AND. TMP ->type == 2))
      ACC ->( netSeek( TMP ->schet))      // Add by TMV  05-01-2003
      if !isWorkAccReserv .Or. accIsSigned() // Add by TMV  22-11-2002
	      tmpSumma += TMP ->summa
   	   tmpCnt++
		endif
   endif
   skip 1 alias TMP
enddo

TMP ->( baseRestPos())

return NIL

//
// подтверждение ордеров
//
funcDef homeProof with canObr:LU ;
   local tmp, aH, aC, fk, dDate, theDay, total, lMore, i,;
         theCode, s := select(), theCode2, n, aCode, distr,;
		  isWorkAccReserv := workAccReserv(), rV  // Add by TMV  22-11-2002

distr := setUpValue( "DISTR")
if empty( distr)
   messageB( "Нет настройки участков DISTR !!! Обратитесь на филиал !!!")
   return .F.
endif


DEFAULT canObr TO .T.
theCode  := ORD_33_DATE
theCode2 := ORD_904_NODATE

tmp := { ".\TMP_PROOF.dbf", "TMP",;
        {{ "fio",       "C", 45, 0} ,;
         { "summa",     "N", 15, 2} ,;
         { "schet",     "C", 20, 0} ,;
         { "distrCode", "N",  2, 0} ,;
         { "nDoc",      "N",  4, 0} ,;
         { "recNo",     "N", 10, 0} ,;
         { "type",      "N",  1, 0} ,;
         { "code",      "C",  4, 0} ,;
         { "dDate",     "D",  8, 0} ,;
		 	{ "Street",    "C", 30, 0} ,;  // Add by TMV  13-12-2001
			{ "House",     "C", 10, 0} ,;  // Add by TMV  13-12-2001
			{ "Flat",      "C",  4, 0} ,;  // Add by TMV  13-12-2001
			{ "Block",     "C",  2, 0}},;  // Add by TMV  13-12-2001
        {{, "fio"                 } ,;
         {, "Str( distrCode, 2) + Str( nDoc, 4)"},; // Replaced by TMV 13-12-2001
         {, "schet"               }}, 0, .T.}


tmp[1] := ".\TMP_PR" + thisStation() + ".DBF"

basePath( tmp, confPathTemp())

dDate  := setCurDate()
n      := 1
if !getUp( 5, 5, {{ "Ордера надомников" }           ,;
                  { "Ордера                  ", block( n),;
                     { " с датой" ,;
                       "без даты" }},;
                  { "*" + chr( 25)      }            ,;
                  { "Введите дату для ордеров", block( dDate),,{|g|chckDate()}}})
    return NIL
endif

if n == 1
   aCode := { ORD_33_DATE, ORD_904_DATE}
else
   aCode := { ORD_33_NODATE, ORD_904_NODATE}
endif

theDay := Day( dDate)
lMore  := .f.
total  := 0.00

if tempCreate( tmp)    // baseCreate( tmp)
   if baseMXOpen( tmp) .AND. distrOpen()

      ACC     ->( baseSavePos())
      CLIENTS ->( baseSavePos())
      SUMS    ->( baseSavePos())

      CLIENTS ->( dbSetOrder( 1))
      SUMS    ->( dbSetOrder( 2))
      ACC     ->( dbSetOrder( 1))

      SUMS    ->( netGoTop())
      if n == 1 then SUMS ->( dbSeek( DtoS( dDate), .T.))

      operStartUp( "Подготовка массива надомников", i := 1, SUMS ->( lastRec()))
      while if( n == 1, SUMS ->dDate == dDate, .T.) .AND. SUMS ->( !eof())

            if aScan( aCode, {|x| x == SUMS ->typeOf}, 1) >0      .AND. ;
               accSeek( SUMS ->schet) .AND. SUMS ->summa  <= 0.00 .AND. ;
               mikeChkDistrict( SUMS ->distrCode) ;
					.And. if(isWorkAccReserv, !isAcctDead(ACC ->schet), .T.)  // Add by TMV  22-11-2002
               //
               // добавим в массив
               //
               TMP ->( dbAppend())

// Replaced by TMV 13-12-2001
               if cliSeek( ACC ->code)
						TMP ->fio    := cliFullName()
                  TMP ->Street := CLIENTS ->Street   // Add by TMV  13-12-2001
                  TMP ->House  := CLIENTS ->House    // Add by TMV  13-12-2001
                  TMP ->Block  := CLIENTS ->Block    // Add by TMV  13-12-2001
                  TMP ->Flat   := CLIENTS ->Flat     // Add by TMV  13-12-2001
					endif
               TMP ->summa     := Abs( SUMS ->summa)/ 100
               TMP ->schet     := SUMS ->schet
               TMP ->recNo     := SUMS ->( recNo())
               TMP ->dDate     := SUMS ->dDate
               TMP ->code      := SUMS ->typeOf
               TMP ->distrCode := SUMS ->distrCode
               TMP ->nDoc      := SUMS ->nDoc
               total += TMP ->summa

               lMore := .T.
            endif
            skip 1 alias SUMS
            operProc( i++)
      enddo
      SUMS    ->( baseRestPos())
      CLIENTS ->( baseRestPos())
      ACC     ->( baseRestPos())
      operExit()

      if lMore

         aH := {{ "Выданные ордера надомникам за " + DtoC( dDate)     },;
                { "<ПРОБЕЛ> - установить/снять отметку"               },;
                { "Серый плюс/минус  - установить/ снять все отметки" },;
                { "Сумма", {|| TMP ->summa},,{|| .F.}                 },;
                { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}

         aC := {{ "Подт" , {, block( TMP ->type), { " ", "*"}}} ,;
                { "Уч",       {|| TMP ->distrCode     }},;
                { "Орд",      {|| TMP ->nDoc          }},;
                { "Надомник", {|| padL( TMP ->fio, 37)}},;
                { "Счет",     {|| TMP ->schet         }},;
                { "Сумма   ", {|| TMP ->summa         }} }

         fk := {,{{ "Итог",    {|| itog()       }}}  ,,,;
                 {{ "Печать",  {|| home2Prn( aCode[ 1], dDate, n) }}},, ;
                 {{ "Сорт",    {|| home2Sort()  }}   ,;
                  { "Поиск",   {|| home2Seek()  }}}  ,;
                 {{ "Удалить", {|| tmpDelOrder()}}} }

         TMP ->( netGoTop())
         if TMP ->( emptyRec())
            errLog( "Нет данных для показа")
         else
            setKey( K_SPACE, {|| setTypeOf()})
            setKey( K_GREY_PLUS,  {|| setAllOn()  })
            setKey( K_GREY_MINUS, {|| setAllOff() })

            TMP ->( browseUp( 2, 2, aH, aC       ,;
                             {|| netSkipUp()  }  ,;
                             {|| netSkipDown()}  ,;
                             {|| netGoTop()   }  ,;
                             {|| netGoBottom()},,,,, fk))
            setKey( K_SPACE,      NIL)
            setKey( K_GREY_PLUS,  NIL)
            setKey( K_GREY_MINUS, NIL)

            if canObr .AND. yesNo( "Обработать ?") then homeProofObr( n == 1)
            SUMS ->( netUnLock())
         endif

      endif
      baseMClose( tmp)
      distrClose()
   endif
   baseDelete( tmp)
endif
select ( s)

return NIL

static funcDef home2Prn with theCode:C, dDate:D, nn1:N ;
   local gets, distr, n1, n2, aDistr, aCode, tmpSumma,;
         tmpCnt, ro, sim, nazn, aOrder, conf904Work  ,;
         lPrintDate, bSpace, tDistr, b,;
		   lPrnPasQuest := .F.,;
			i:=0, iW:=wselect()

   bSpace     := SetKey( K_SPACE)
   SetKey( K_SPACE, NIL)
   lPrintDate := nn1 == 1

   if confIsDistr() .AND. !empty( thisStation())
      b := {|| DISTR ->Station == thisStation()}
   endif
   usePrnReset( .F.)

   distrArray( @aDistr, @aCode, b)
   operGetInfo( theCode, @ro, @sim)
   nazn := "Выплата средств с пенсионного счета /на дому/"
   conf904Work := conf904Work()

   Distr     := 1
   n1 := n2  := 1
   gets := {{ "Повторная печать ордеров надомников"   },;
            { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"   },;
            { "Участок доставки", block( distr)        ,;
            aDistr,, {|g| setGrn( g, aDistr, aCode)  }},;
            { "*" + chr( 25)                          },;
            { ""                                      },;
            { "Ордера с ", block( n1), "9999"         },;
            { "*по ",      block( n2), "9999"         }}
   if getUp( 5, 5, gets)
      TMP     ->( baseSavePos())
      ACC     ->( baseSavePos())
      CLIENTS ->( baseSavePos())

      TMP     ->( dbSetOrder( 2))
      ACC     ->( dbSetOrder( 1))
      CLIENTS ->( dbSetOrder( 1))

      Distr  := max( 1, Distr)
      tDistr := aCode[ Distr]
      TMP ->( dbSeek( Str( tDistr, 2), .T.))
      if empty( tDistr)
         while TMP ->( emptyRec()) .AND. TMP ->( !eof())
           skip 1 alias TMP
         enddo
      endif
      getDistrInfo( tDistr, dDate, @tmpSumma, @tmpCnt, n1, n2, .F.)
      printAOrder( { "", "", "", "", "",;
        "---------------------------------------------------------------------------",;
        "", "", "", "", ""                         ,;
        "Выплата пенсий надомникам"                ,;
        "Район доставки " + aDistr[ Distr ]        ,;
        "Дата    : " + DtoC( TMP ->dDate)          ,;
        "Ордеров : " + nStr( tmpCnt) + "     На общую сумму : " + mStr( tmpSumma) + " руб." ,;
        "---------------------------------------------------------------------------",;
        "", "" })

      TMP ->( dbSeek( Str( tDistr, 2) + Str( n1, 4), .T.))
      statStartUp( "Печать ордеров надомников с " + nStr( n1) + " по " + nStr( n2))
      while TMP ->distrCode == tDistr .AND. ;
            TMP ->nDoc >= n1 .AND. TMP ->nDoc <= n2

         if ACC     ->( dbSeek( TMP ->schet)) .AND. ;
            CLIENTS ->( dbSeek( ACC ->code )) .AND. ;
            TMP ->type == 2

            aOrder := homeOneOrder( cliFullName(), cliFullAddress(),;
               cliDocum(), TMP ->nDoc, nazn, ro, sim, TMP ->summa  ,;
               lPrintDate, .T., dDate, conf904Work)

				//  Add by TMV  10-12-2001
				lPrnPasQuest = (empty(cliPasSer1()) .Or. empty(cliPasSer2()) .Or.;
					empty(cliPasNum()))
            schetPrintOpen( .F.)
            if lPrintDate
               printHistory( TMP ->schet, dDate, - TMP ->summa)
            else
               printHistory( TMP ->schet, dDate,, .F.)
            endif
            // printHistory( TMP ->schet, dDate, - TMP ->summa)
            printAOrder( aOrder)
            printAdv2( .F.)
				if lPrnPasQuest then prnPasQuest(cliFullName()) //  Add by TMV  10-12-2001

         endif
         skip 1 alias TMP
         statProc()

      enddo
      statExit()

      CLIENTS ->( baseRestPos())
      ACC     ->( baseRestPos())
      TMP     ->( baseRestPos())
   endif
   //prnReset()
   SetKey( K_SPACE, bSpace)
usePrnReset( .T.)
return NIL

static funcDef setGrn with g:O, aDistr:A, aCode:A ;
   local rV := .F., distr, tmp1, tmp2

   Distr := g:varGet()
   Distr := aTrimmedScan( aDistr, Distr)
   Distr := aCode[ Distr]
   TMP ->( baseSavePos())
   TMP ->( dbSetOrder( 2))

   if TMP ->( dbSeek( Str( Distr, 2) ))
      if empty( distr) .AND. TMP ->( emptyRec())
         while TMP ->( emptyRec()) .AND. TMP ->( !Eof())
           TMP ->( dbSkip())
         enddo
      endif
      tmp1 := TMP ->nDoc
      TMP ->( dbSeek( Str( distr + 1, 2), .T.))
      if TMP ->distrCode <> Distr .OR. TMP ->( eof()) then TMP ->( dbSkip( -1))
      tmp2 := TMP ->nDoc
      M ->getList[ 2]:varPut( tmp1)
      M ->getList[ 2]:display()
      M ->getList[ 3]:varPut( tmp2)
      M ->getList[ 3]:display()
      rV := .T.
   else
      messageB( "Нет данных по доставочному участку " + nStr( distr))
   endif
   TMP ->( baseRestPos())
return rV


static funcDef home2Sort local arr, i
   arr := { "По Фамилии", "По участкам", "По счетам"}
   i := dialog( { "Вы хотите просматривать ордера"}, arr)
   if i < 1 then return NIL
   TMP ->( dbSetOrder( i))
   getCurOB(): refreshAll(): forceStable()
return NIL

static funcDef home2Seek local gets, seekStr, str2, idx, pict
   idx := TMP ->( indexOrd())
   do case
      case idx == 1
           seekStr := Space( 30)
           gets := {{ "Введите фамилию", block( seekStr)}}
      case idx == 2
           seekStr := Space( 2)
           str2    := Space( 4)
           gets := {{ "Введите номер участка", block( seekStr), "@R 99"},;
                    { "         номер ордера", block( str2), "@R 9999" }}
      case idx == 3
           seekStr := Space( 20)
           pict    := "@R 99999 999 9 9999 9999999"
           gets := {{ "Введите номер счета", block( SeekStr), pict}}
   endcase
   if getUp( 5, 5, gets)
      if idx == 2
         seekStr := padl( allTrim( seekStr), 2) + padL( alltrim( str2), 4)
      endif
      seekStr := rTrim( seekStr)
      TMP ->( dbSeek( upper( seekStr), .T.))
      getCurOB():refreshAll(): forceStable()
   endif
return NIL



static func chckDate
return "с датой" $ lower( allTrim( getList[ 1]:varGet()))

static funcDef itog local rV := 0, i, cnt := 0

TMP ->( baseSavePos())
TMP ->( netGoTop())
operStartUp( "Подсчет итоговой суммы", i := 1, TMP ->( lastRec()))
while TMP ->( !eof())

      if TMP ->type == 2
         rV += TMP ->summa
         cnt++
      endif
      skip 1 alias TMP
      operProc( i++)
enddo
operExit()
TMP ->( baseRestPos())

OK( { "Кол-во ордеров " + Str( cnt),;
      "Итоговая сумма " + mStr( rV)})

return NIL
//
static funcDef tmpDelOrder local summa := 0, schet

   SUMS ->( dbGoto( TMP ->recNo))
   summa := SUMS ->summa/ 100
   schet := TMP ->schet

   if TMP ->schet == SUMS ->schet .AND. ;
      TMP ->summa == round( abs( SUMS ->summa/ 100), 2)

      if sumRLock()
         do case
            //
            // аннулируем поручение без даты
            //
            case SUMS ->typeOf == ORD_904_NODATE
                 SUMS ->typeOf := ORD_904_ESC
            //
            // аннулируем ордер без даты
            //
            case SUMS ->typeOf == ORD_33_NODATE
                 SUMS ->typeOf := ORD_33_ESC
            //
            // поручение с датой или ордер с датой просто затираем
            //
            otherwise
                 SUMS ->( clearRec())
         endcase
         SUMS ->( netUnLock())
         TMP  ->( mDelete())
      endif
   else
      errLog( { "Не найдена проводка, соответствующая ордеру",;
               "счет  " + TMP ->schet,;
               "сумма " + mStr( TMP ->summa) })
   endif
   getCurOB(): refreshAll(): forceStable()

return NIL

static funcDef homeProofObr with withDate:L ;
   local i, theKind := opDecode( "1002"), summa , str, file := ".\svMem.txt",;
         cntSumma := 0, schet2, is904, tCode

is904 := .F.
operStartUp( "Подтверждение ордеров надомников ", i := 1, TMP ->( lastRec()) )
TMP ->( netGoTop())

// is904 := aScan( { ORD_904_NODATE, ORD_904_DATE}) >0
if file( file) then fErase( file)
str := { "                               " + rDate( setCurDate()) ,;
         "                    СВОДНЫЙ МЕМОРИАЛЬНЫЙ ОРДЕР N " + nStr( ordIncSvMem( setCurDate())),;
         "" ,; // 10        14         14            20
         "---------------------------------------------------------------------------" ,;
         "|   Дата   |        Дебет       |        Кредит      |          Сумма     |" ,;
         "---------------------------------------------------------------------------" }
//         0123456789 01234567890123456789 01234567890123456789 01234567890123456789
eFileWrite( str, file)
schet2 := padL( confKorSchet(), 20)
while TMP ->( !eof())

      if TMP ->type == 2
         SUMS ->( dbGoTo( TMP ->recNo))
         if TMP ->schet == SUMS ->schet .AND. ;
            TMP ->dDate == SUMS ->dDate .AND. ;
            round( TMP ->summa, 2) == round( - SUMS ->summa / 100, 2)
            //
            // если прошел ордер - > корректируем остаток
            //
            if aScan( { ORD_33_DATE, ORD_33_NODATE}, {|x| x == SUMS ->typeOf}) >0
               summa := kasWriteSumma( SUMS ->summa/ 100)
            else
               is904 := .T.
            endif
            if if( is904, .T., abs( Summa) >0)
               if sumRLock()
                  do case
                     case SUMS ->typeOf == ORD_33_DATE    do tCode := opDecode( "1002")
                     case SUMS ->typeOf == ORD_33_NODATE  do tCode := opDecode( "1004")
                     case SUMS ->typeOf == ORD_904_NODATE do tCode := opDecode( "1008")
                     case SUMS ->typeOf == ORD_904_DATE   do tCode := opDecode( "1005")
                  endcase
                  SUMS ->typeOf := tCode
                  if !withDate then SUMS ->dDate := setCurDate()
                  if tCode == opDecode( "1008") .OR. tCode == opDecode( "1005")
                     str := "|" + DtoC( SUMS ->dDate) + "|" + ;
                         SUMS ->schet + "|" + schet2  + "|" + ;
                         padL( mStr( - SUMS ->summa/ 100), 20) + "|"
                     eFileWrite( str, file)
                     cntSumma -= SUMS ->summa/ 100
                  endif
                  sumUnLock()
                  //
                  // установка признака подписанного заявления
                  //
                  if ACC ->( netSeek( TMP ->schet)) .AND. ;
                     accIsPrinted()
                     accSetSigned()    // Add by TMV  07-03-2003

                  endif

               endif
            else
               errLog( "Подкрепите кассу !")
               exit
            endif
         else
           errLog( { "Не найден ордер по счету " + splitSSchet( TMP ->schet) ,;
                    "Сумма ордера " + mStr( TMP ->summa)      ,;
                    "Обратитесь на узел связи" })
         endif
      endif

      skip 1 alias TMP
      operProc( i++)
enddo
operExit()

str := { "---------------------------------------------------------------------------" ,;
         ""                                                            ,;
         "ИТОГО                                                 " + mStr(cntSumma) ,;
         ""                                                            ,;
         "За что : Доставка пенсий на дом РУПС"                        ,;
         ""                                                            ,;
         "Контролер: _____________________"                            }
eFileWrite( str, file)

if is904 then protView( file)

return NIL

autofunction setValue getDonName    init ""
autofunction setValue getDonExeName init ""
autofunction setValue useStopDon init .T.
//
// печать на принтер ордера
//
//Изменено 03.05.2001 в 12:57, Залманов А.З.----------------vvvvvvvvvvvvv
funcDef printAOrder with aOrder:A, startCode:CU, stopCode:CU, lPreview:LU ; 
   local i, tmpOrder := {}, try := 3, rV := .F., str := "",;
         donEndl := chr( 10) + chr( 13), isReady := .T., ;
         imaFile, cScreen, confBat, imaBat, soderBat := ""
   public inStat
                    
   default lPreview to .T.
   if confShowIt() == 2
      aEval( aOrder, {|x| str += x + endl()})
      if lPreview then editMemo( str)
   endif

   do case
     case confPrintTo() == 1

        DEFAULT startCode TO "&4"
        DEFAULT stopCode  TO ""
        startCode += donEndl + chr( 27) + chr( 33) + chr( 1) + donEndl
        stopCode  += donEndl
        while try > 0

            if if( confPrintDon() .or. confPrintUsel(), .F., !checkPrinter()) // Mike !!!!!!!!!!!!!!!
               errLog( { "Принтер не готов !", "" ,;
                        "Включите принтер и попробуйте еще раз !" })
               try--
               loop
            endif
            if confPrintDon()
               if file( getDonName()) then fErase( getDonName())
               eFileWrite( startCode, getDonName())
               for i := 1 to len( aOrder) do eFileWrite( aOrder[ i], getDonName())
               eFileWrite( stopCode, getDonName())
               runExe( getDonExeName())
            else // Mike !!!!!!!!!!!!!!!
               if confPrintUsel()
                    messageStartUp(" Ждите идет печать ... ")
                    setport(confUselPort(),@isReady)
                    if .not. isReady
                       try--
                       loop
                    endif
                    if .not. MikePCheck(frg2a())
                       errLog( { "Регистратор не готов !", "" ,;
                                "Включите и попробуйте еще раз !" })
                       try--
                       messageExit( 0)
                       loop
                    endif
                    //frg2a()
                    if StartCode != "&2"
                       MikePBlank( aOrder)
                    else
                       MikePCheque( MikeStoA(aOrder[1]))
                    endif
                    messageExit( 0)
                    clear typeAHead
               else
                  set console off
                  set printer on

                  for i := 1 to len( aOrder)
                    ?? ( aOrder[ i] + chr( 13 ) + chr( 10 ))
                  next

                  rV := .T.
                  set printer off
                  set console on
                  //if usePrnReset() then prnReset()
               endif
            endif
            exit
        enddo

     case confPrintTo() == 2             //Печать в файл

        eFileWrite( aOrder, ".\debug.prn")

     case confPrintTo() == 3             //Печать из .bat файла

        if empty( inStat)
           imaFile := "debug00.prn"
           imaBat  := "prn00.bat"
        else
           imaFile := "debug" + inStat + ".prn"
           imaBat  := "prn" + inStat + ".bat"
        endif

        confBat := "WinPrn.exe " + imaFile +" " + '"-f=Courier New" -b=0 -o=p -s=11 "-p=' + allTrim( confNaimPrint( )) + '"'

        if file( ".\" + imaFile)
           erase ( ".\" + imaFile)
        endif
        eFileWrite( aOrder, ".\" +imaFile)

        if !file( ".\" + imaBat)
           eFileWrite( confBat, ".\" +imaBat)
        endif
       
        cScreen := saveScreen() 
         run ( imaBat)
         set printer off
         set console on
        restScreen(,,,, cScreen )

   endcase
return rV

autofunction setValue usePrnReset init .T.
//
// печать выписки
//
function printHistory( schet, dDate, summa)
return printAOrder( { getHistory( schet, dDate, summa)}, "&2")

funcDef printWiscR local dbInfo, s, Schet, arr1, arr2
accPrizList( "", @arr1, @arr2,, .T., .F.)
Schet := 1
if !getUp( 5, 5, {{ "Выписка за последний месяц"    },;
                  { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
                  { "Введите счет", block( schet), arr1}})
   return NIL
endif
Schet := arr2[ Schet, 2]

dbInfo := aClone( wiscInfo())
dbInfo[ 1] := ".\WISCR.DBF"
dbInfo[ 4][ 1][ 1] := ""
dbInfo[ 4][ 2][ 1] := ""

basePath( dbInfo, confPathOD())
if !file( dbInfo[ 1]) 
   messageB( "Массив выписок не найден !!!")
   return NIL
endif
s := Select()
WISC ->( baseSavePos())
baseMClose( wiscInfo())
if Select( "WISC") > 0
   messageB( "Выписку нельзя получить !!!")
   return NIL
endif

if baseMOpen( dbInfo)
   printAOrder( { getHistory( Schet, SetCurDate() - 1,,, .T.)})
   // printHistory( Schet, SetCurDate() - 1)
   baseMClose( dbInfo)
endif
if !baseMOpen( wiscInfo())
   messageB( "Ошибка при открытии таблицы выписок. Выйдите из программы !!!")
   return NIL
endif
WISC ->( baseRestPos())
Select (S)


return NIL


autofunction setValue histView init .F.
//
// получение выписки
//


static funcDef getHistHeader with schet:C local rV, d1, d2, hhHeader, h1, h2
 
  if (confPrintDon() .or. confPrintUsel()) .AND. !histView() // Mike !!!!!!!!!!!!!!!
    rV := replicate( "-", 38) + endl() + confBankName() + endl() + endl() + ;
          left( cliFullName(), 40) + endl() + "Счет " + schet       + endl() + ;
          replicate( "-", 38)           + endl()
    return rV
  endif
  d1 := d2 := DtoC( setCurDate())
  if wiscSeek( schet, WS_HISTORY) then d1 := left( WISC ->buffer, 10)
  SUMS ->( dbSeek( nStr( val( schet) + 1), .T.))
  SUMS ->( netSkipUp())
  if SUMS ->schet == schet then d2 := DtoC( SUMS ->dDate)
  schet := left( schet, 5) + " " + subStr( schet, 6, 3) + " " + ;
    subStr( schet, 9, 1) + " " + subStr( schet, 10, 4) + " " + right( schet, 7)
  hhHeader := confBankName()+ ' ' + rTrim( confFilial()) + " " + confOtdName()
  h1 := "Выписка с " + d1 + " по " + d2 + " " + hhHeader
  h2 := ""
  if len( h1) > 76
    h2 := subStr( h1, 77); h1 := left( h1, 76)
  endif
  rV := replicate( "=", 76) + endl() + h1 + endl() + h2 + endl() + ;
	 padL(time(), 76) + endl() +;  //  Add by TMV 30-10-2001
    padR( cliFullName(), 39) + "Лицевой счет " + schet + endl()+;
    replicate( "-", 76) + endl() + ;
    "   Дата    Вид операции" + Space( 22) + "Остаток  Расход(Дт)  Приход(Кт)"+ endl()
return rV

funcDef getFilNumb
return padL( confFilNumb(), 3)

funcDef getWHistBody with schet:C local rV := "", str2, pos, pos2, i
  if !((confPrintDon() .or. confPrintUsel()) .and. !histView()) // Mike !!!!!!!!!!!!!!!
     rV := wiscHistory( schet)
  else
   str2 := CtoA( wiscHistory( schet), endl())
   for i := 1 to len( str2)
    if !empty( CtoD( left( str2[ i], 10)))
      rV += replicate("-", 38) + endl() + Space(6) + left( str2[i], 10) + endl()
    else
      rV += subStr( str2[i], 12, 29) + allTrim( subStr( str2[ i], 42)) + endl()
    endif
   next
  endif
return rV 

funcDef getRestIfEmpty with schet:C, dDate:D, rest:N, mustUpper:LU local rV, str
  DEFAULT mustUpper TO .F.
  str := getHistSumma( rest)
  if (confPrintDon() .or. confPrintUsel()) .AND. !histView() // Mike !!!!!!!!!!!!!!!
     rV := replicate( "-", 38) + endl() + Space( 6) + DtoC( dDate) + ;
           endl() + padR( "Остаток на счете ", 25) + str + endl()
  else
     rV := /*replicate( "-", 76) + endl() + */DtoC( dDate) + " Остаток на счете" + ;
       Space( 15) + str + endl() + replicate( "-", 76) + endl()
  endif
  if mustUpper then rV := upper( rV)
return rV

funcDef getHLine with dDate:DU, operName:C, summa:N, showDelim:LU;
  local rV,str
  // DEFAULT mustUpper TO .F.
  DEFAULT showDelim TO .F.
  dDate := if( empty( dDate), dDate := Space( 10), DtoC( dDate))
  if summa > 0
    str := getHistSumma( summa)
    if (confPrintDon() .or. confPrintUsel()) .AND. !histView() // Mike !!!!!!!!!!!!!!!
      rV := replicate( "-", 38) + endl() + Space( 6) + dDate + endl()  + ;
            padR( operName, 25) + str + endl()
    else
      rV := if( showDelim, replicate( "-", 76)  + endl(), "") + ;
            dDate + " " + padR( operName, 29) + Space( 26) + str + endl()
    endif
  else
    summa := - summa
    str := getHistSumma( summa)
    if (confPrintDon() .or. confPrintUsel()) .AND. !histView() // Mike !!!!!!!!!!!!!!!
      rV := replicate( "-", 38) + endl() + Space( 6) + dDate + endl() + ;
        padR( operName, 25) + str + endl()
    else
      rV := if( showDelim, replicate( "-", 76)  + endl(), "") + ;
       dDate + " " + padR( operName, 29) + Space( 14) + str + endl()
    endif
  endif
return rV

static funcDef gethistSumma with sum:N local rV
  rV := StrTran( lTrim( mStr( sum)), ".", "")
  rV := padL( rV, 10)
return rV


funcDef getHistory ;
   with schet:CU, dDate:DU, summa:NU, viewRest:LU, useWiscR:LU ;
   local cHistory := "", s := select(), rest, strTemp, strBnk, strSchet ,;
         strClient, str, strHist, str2,;
         i, pos2, pos, wHist,;
			nRecord := 0 // Add by TMV  16-12-2002


DEFAULT schet    TO ACC ->schet
DEFAULT dDate    TO setCurDate()
DEFAULT viewRest TO .T.
DEFAULT useWiscR TO .F.
//
// шапка выписки
//
cHistory := getHistHeader( schet)
//
// Выписка из массива желаний
//
wHist := getWHistBody(  schet)
if !empty( wHist) then cHistory += wHist
if useWiscR then return cHistory
//
// если нет данных, то допишем начальный остаток
//
rest := accSumma( schet)
str  := getHistSumma( rest)
if (confPrintDon() .or. confPrintUsel()) .AND. !histView() // Mike !!!!!!
  cHistory += DtoC(dDate) + endl() + padR( "Остаток на счете", 25) + str + endl()
else
  cHistory += DtoC( dDate) + " ОСТАТОК НА СЧЕТЕ" + Space( 15) + str + endl()           
endif
//
// Проводки
//
SUMS ->( baseSavePos())
SUMS ->( dbSetOrder( 1))
SUMS ->( netSeek( schet, .T.))
while SUMS ->schet == schet .AND. SUMS ->dDate <= dDate .AND. SUMS ->( !eof())
// Replace by TMV  18-12-2002
//  strHist  := getHistLine()
  strHist  := getHistLine( ;
		opEqual( SUMS ->typeOf, '6136') .Or. opEqual( SUMS ->typeOf, '6137') )
  if !empty( strHist)
    cHistory += strHist + endl()
  endif
// Add by TMV  16-12-2002 - Для новой схемы выдачи и возврата ссуды
	if opEqual( SUMS ->typeOf, '6136') .Or. opEqual( SUMS ->typeOf, '1060');
			.Or. opEqual( SUMS ->typeOf, '6137')
// Replace by TMV  17-12-2002
//	if opEqual( SUMS ->typeOf, '2060') .Or. opEqual( SUMS ->typeOf, '1060')
		nRecord := getEqualSums( schet, SUMS->TypeOf, SUMS->Summa,;
			allTrim(SUMS->Buffer) )
		if nRecord > 0
// Replace by TMV  18-12-2002
//			strHist  := getHistLine(.T.)
			strHist  := getHistLine(opEqual( SUMS ->typeOf, '6031'))
			SUMS->(dbSetOrder(1))
			SUMS->(dbGoTo(nRecord))
			if !empty( strHist)
				cHistory += strHist + endl()
			endif
		endif
	endif
// Add by TMV  16-12-2002 -- End
  skip 1 alias SUMS
enddo
SUMS ->( baseRestPos())
//
// допишем остаток
//
rest := sumRest( schet, dDate, .T.)
if !empty( summa)
  if (confPrintDon() .or. confPrintUsel()) .AND. !histView() // Mike !!!!!!!!!!!!!!!
  else
    cHistory += replicate( "-", 76) + endl()
  endif

  if summa >0
    cHistory += getHLine( dDate, "Внесено наличными", summa, .F.)
  else
    cHistory += getHLine( dDate, "ВЫДАНО НАЛИЧНЫМИ", summa, .F.)
  endif
   rest += summa
   str := getHistSumma( rest)
   if viewRest
      if (confPrintDon() .or. confPrintUsel()) .AND. !histView() // Mike !!!!!!
         cHistory += replicate( "=", 38) + endl() + Space(6) + DtoC(dDate) + ;
            endl() + padR( "ОСТАТОК НА СЧЕТЕ", 25) + str + endl()          + ;
            replicate( "=", 38) + endl()
      else
         cHistory += DtoC( dDate) + " ОСТАТОК НА СЧЕТЕ" + Space( 15) + str + ;
            endl() + replicate( "=", 76) + endl()
      endif
   endif
else
 cHistory += getRestIfEmpty( schet, dDate, rest)
endif
return cHistory


// Edit by TMV  16-12-2002 - add new parameter <lNegative>
//static funcDef getHistLine local str := "", sum, lenSum, aNames
funcDef getHistLine with lNegative:LU local str := "", sum, lenSum, aNames

DEFAULT lNegative TO .F.  // Add by TMV  16-12-2002

if SUMS ->typeOf < "9000" .OR. histView() .OR. ;
   SUMS ->typeOf $ "9100,9001,9002"
   OPER ->( netSeek( SUMS ->typeOf))
   if (confPrintDon() .or. confPrintUsel()) .AND. !histView() // Mike !!!!!!!!!!!!!!!
      str := replicate( "-", 38) + endl() + Space( 6) + ;
             DtoC( SUMS ->dDate) + endl()
      sum    := " " + lTrim( mStr( abs( SUMS ->summa/ 100)))
      lenSum := len( sum)
      if len( rTrim( OPER ->name)) < ( 35 - lenSum)
         str   += padR( OPER ->name, 35 - lenSum) + sum
      else
        aNames := razbivka( OPER ->name, {{ 35 - lenSum, "L"}, { 35, "L"}})
        str    += aNames[ 1] + sum + endl() + aNames[ 2]
      endif
   else
     if len( rTrim( OPER ->name)) < 29
        str := DtoC( SUMS ->dDate) + " " + padR( OPER ->name, 29) + Space( 14)
// Edit by TMV  16-12-2002 - add param lNegative
//        if SUMS ->summa >0 then str += space( 12)
        if SUMS ->summa > 0 .And. !lNegative .Or.;
		  		SUMS->Summa < 0 .And. lNegative then str += space( 12)
        str += getHistSumma( abs( SUMS ->summa/ 100))
     else
        aNames := razbivka( OPER ->name, {{ 29, "L"}, { 29, "L"}})
        str := DtoC( SUMS ->dDate) + " " + aNames[ 1] + Space( 14)

// Edit by TMV  16-12-2002 - add param lNegative
//        if SUMS ->summa >0 then str += space( 12)
        if SUMS ->summa > 0 .And. !lNegative .Or.;
		  		SUMS->Summa < 0 .And. lNegative then str += space( 12)
        str += getHistSumma( abs( SUMS ->summa/ 100)) + endl()
        str += Space( 10) + " " + aNames[ 2]
     endif
   endif
endif

return str

//
// Составление одного ордера. При этом печатаются данные для
// текущего клиента и счета.
//
funcDef homeOneOrder ;
  with cliName:C, cliAddr:C, cliDocum:C, nDoc:N, nazn:C, ro:C, sim:C,;
       nSumma:N, lPrintDate:LU, lPrintPasp:LU, dDate:DU, is904:LU   ,;
       trustFIO:CU, trustAddr:CU,   ;
		 schetPens:CU ;        // Add by TMV  22-11-2002
       local aOneOrder

DEFAULT dDate      TO setCurDate()
DEFAULT is904      TO .F.
DEFAULT lPrintDate TO .T.
DEFAULT lPrintPasp TO .T.

DEFAULT schetPens TO ACC ->schet  // Add by TMV  22-11-2002

if !is904
// Replace < ACC->SCHET to schetPens > by TMV  22-11-2002
   aOneOrder := post_order( nDoc, lPrintDate, dDate, cliName, cliAddr   ,;
             schetPens, nSumma, nazn, sim, ro, lPrintPasp, cliDocum,;
             trustFIO, trustAddr)
else
   aOneOrder := PaymentPens( nDoc, dDate, cliName, cliAddr, confPostName(),;
             confNodeName(), nSumma, "серия " + cliPasSer() + ", N "   +;
             cliPasNum(), cliPasGiv(), lPrintDate, lPrintPasp)
endif

return aOneOrder

//
// установка признака надомника
//
funcDef homeSetOn ;
  local arr := { }, arr2 := { }, arr3 := { }, n := 1, nD := Day( setCurDate()),;
        aDistr, aCode, nDistr, isHome := 1


distrArray( @aDistr, @aCode)
nDistr := CLIENTS ->distrCode
nDistr := aScan( aCode, {|x| x == nDistr})


ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))
ACC ->( netSeek( CLIENTS ->code))

while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())

      if isPens( ACC ->priz) .AND. val( ACC ->typeOf) = 0
         aAdd( arr, "Счет " + splitSSchet( ACC ->schet))
         aAdd( arr2, ACC ->( recNo()) )
         aAdd( arr3, SubStr( ACC ->buffer, 2, 2))
      endif
      skip 1 alias ACC
enddo
ACC ->( baseRestPos())

if len( arr) >0
   if len( arr) > 1
      n := popUp( 5, 5, arr)
      if n < 1 then return NIL
   endif
   ACC ->( dbGoTo( arr2[ n]))
   nD := if( empty( arr3[ n]), nD, arr3[ n])
   isHome := if( accIsHome(), 2, 1)
   if getUp( 5, 5,;
       {{ "Установка признака надомника"                     },;
        { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                     },;
        { "Пенсионный счет : " + arr[ n]                     },;
        { "Надомник        :", block( isHome), { "Нет", "Да"}},;
        { "День выплаты    :", block( nD), "99"              },;
        { "Участок         :", block( nDistr), aDistr        }})
      if accRLock()
         if isHome == 2
            accSetHome( nD)
         else
            ACC ->buffer := " " + subStr( ACC ->buffer, 2)
         endif
         if cliRLock()
            CLIENTS ->distrCode := aCode[ nDistr]
            cliUnLock()
         endif
         accUnLock()
      endif
   endif
else
   errLog( "У клиента нет пенсионных счетов !")
endif
return NIL

//
// Установка признака подписи заявления
//

funcDef homeSetSigned with lEdit:LU ;  // Add <lEdit> by TMV  27-11-2002
	local arr := { }, i, arr2 := { }

DEFAULT lEdit TO .T.  // Add by TMV  27-11-2002

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

if ACC ->( netSeek( CLIENTS ->code))
   while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
//         if SubStr( ACC ->buffer, 5, 1) <> "Y"  //  .and. accIsHome()
//  Remove by TMV  26-11-2001 - Возможность снятия отметки о подписании заявления
//            aAdd( arr,  splitSSchet( ACC ->schet))  //  Replaced by TNV  23-11-2001
            aAdd( arr,  splitSSchet( ACC ->schet) +;
					if(SubStr( ACC ->buffer, 5, 1) == "Y", ' -подписано!', ''))
            aAdd( arr2, ACC ->( recNo()))
//         endif
         skip 1 alias ACC
   enddo
   if len( arr) >0
      i := 1
		// Edited by TMV  27-11-2002
      if getUp( 5, 5, {{ if(lEdit, "Установка","Просмотр ") + " признака наличия подписанного заявления"},;
                       { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
                       { "Выберите счет", block( i), arr             }}) ;
				.And. lEdit  // Add by TMV  27-11-2002
         ACC ->( dbGoTo( arr2[ i]))
         if accRLock()
            ACC ->buffer := left( ACC ->buffer, 4) +;     // Edited by TMV  26-11-2001
					if(SubStr( ACC ->buffer, 5, 1) == "Y"," ","Y") +;
					right( ACC ->buffer, 5)
            accUnLock()
         endif
      endif
   else
      errLog( "У всех счетов клиента заявления подписаны !")
   endif
endif
ACC ->( baseRestPos())
return NIL


//
// установка признака о вручении заявления
//
funcDef S20SetSigned local arr := { }, i, arr2 := { }

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

if ACC ->( netSeek( CLIENTS ->code))
   while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
         if SubStr( ACC ->buffer, 7, 1) <> "Y"  //  .and. accIsHome()
            aAdd( arr,  splitSSchet( ACC ->schet))
            aAdd( arr2, ACC ->( recNo()))
         endif
         skip 1 alias ACC
   enddo
   if len( arr) >0
      i := 1
      if getUp( 5, 5, {{ "Извещение об изменении номера счета вручено ?"},;
                       { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
                       { "Выберите счет", block( i), arr             }})
         ACC ->( dbGoTo( arr2[ i]))
         if accRLock()
            ACC ->buffer := left( ACC ->buffer, 6) + "Y" + right(ACC->buffer,3)
            accUnLock()
         endif
      endif
   else
      errLog( "У всех счетов клиента извещения вручены !")
   endif
endif
ACC ->( baseRestPos())
return NIL

func schetSpaced( schet)
return schet


* NEW OBJECT FILE
funcDef notifPrint local d1, d2, cnt, ptr := 1, arr, fio, addr
d1  := setCurDate() - 30
d2  := setCurDate()
cnt := 1

if !getUp( 5, 5,;
   {{ "Печать уведомлений об открытии новых счетов" },;
    { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" },;
    { "Дата открытия счетов с  ", block( d1)        },;
    { "                     по ", block( d2)        },;
    { "Печатать не более       ", block( cnt), "999",,;
                                 {|g| g:varGet > 0} }})
    return NIL
endif
ACC     ->( baseSavePos())
CLIENTS ->( baseSavePos())
CLIENTS ->( dbSetOrder( 1))
statStartUp( "Печать уведомлений об открытии счета")
ACC ->( netGoTop())
while ACC ->( !eof())
  if ACC ->datOpen >= d1 .AND. ACC ->datOpen <= d2 .AND. ;
     empty( accNotif())  .AND. isPens( ACC ->PRIZ)
     CLIENTS ->( dbSeek( ACC ->code))
     arr := notifArr( cliFullName(), cliFullAddress(), ACC ->datOpen)
     printAOrder( arr)
     if accRLock()
        accNotif( "P")
        accUnLock()
     endif
     exit if ptr >= cnt
     ptr++
  endif
  statProc()
  skip 1 alias ACC
enddo
statExit()
CLIENTS ->( baseRestPos())
ACC     ->( baseRestPos())
messageB( "Напечатано " + nStr( ptr) + " уведомлений !!!")

return NIL

procDef prnPasQuest with sFio:C
printAOrder( {"============================ линия отрыва ============================",;
				  " Паспортные данные  " + sFio,;
				  " Паспорт серия ________________ номер _________________________",;
				  " кем и когда выдан ___________________________________________________",;
				  " _____________________________________________________________________",;
				  "",;
				  "============================ линия отрыва ============================"} )
return

// Add by TMV 16-12-2002
// Поиск парных проводок по выдаче и погашению ссуды

funcDef getEqualSums With schet:C, sType:C, nSumma:N, sBuffer:C;
	local nRec:=SUMS->(recNo()), nOrd:=SUMS->(indexOrd()), lSeek:=.F., i:=0

SUMS->(dbSetOrder(4))
if SUMS->(dbSeek( Str( nSumma, 15, 0) ))
	do while SUMS->SUMMA == nSumma .And. !SUMS->(Eof())
// Replace by TMV  17-12-2002
		if opEqual( sType, '6136') .And. opEqual( SUMS->TypeOf, '2060');
				.And. allTrim(SUMS->Buffer) == schet;
				.Or.;
				opEqual( sType, '1060') .And. opEqual( SUMS->TypeOf, '6031');
				.And. allTrim(SUMS->Buffer) == sBuffer;
				.Or.;
				opEqual( sType, '6137') .And. opEqual( SUMS->TypeOf, '2060');
				.And. allTrim(SUMS->Buffer) == ('PCT:' + schet)
//		if opEqual( sType, '2060') .And. opEqual( SUMS->TypeOf, '6136');
//				.And. allTrim(SUMS->Buffer) == schet;
//				.Or.;
//				opEqual( sType, '1060') .And. opEqual( SUMS->TypeOf, '6031');
//				.And. allTrim(SUMS->Buffer) == sBuffer;
//				.Or.;
//				opEqual( sType, '2060') .And. opEqual( SUMS->TypeOf, '6137');
//				.And. allTrim(SUMS->Buffer) == ('PCT:' + schet)
			lSeek = .T.
			Exit
		endif
		SUMS->(dbSkip())
	enddo
endif
if !lSeek
	SUMS->(dbSetOrder(nOrd))
	SUMS->(dbGoTo(nRec))
	nRec = 0
endif
return nRec