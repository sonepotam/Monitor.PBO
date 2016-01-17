*------------------------------------------------------------------------------
*
*                         Программа  : MON.EXE
*                         Файл       : baseServ.prg
*                         Автор      : Цейтлин П.М.
*                         Назначение : Ведение долгосрочных поручений
*
*------------------------------------------------------------------------------
*
* 28-10-2002 TMV
* Введен новый вид долгосрочных поручений - перевод на свой счет в другом отд.
* Поправлены ф-ии contEdit(), contPrintIt(), prov2_Name(), prov2_Code(),
*  contContClose(), contInitServ(), 
* Новые ф-ии transAccEdit(), transAccPrint(), chkTranSchet(), transAccPaym()
*  transAccLastPaym(), transAccClose()
*
* 31-10-2002 TMV
* И к предидущему - вставлен контроль, чтобы нельзя было ввести это поручение,
*  если у клиента уже есть поручение на перевод остатка.
*  В ф-ию contLock() вставлен вызов ф-ии transExist()
*
* 11-11-2002 TMV
* Исправлена печать поручения на перевод в другое отд. - ф-ия transAccPrint()
*
* 14-11-2002 TMV
* Заблокирована установка лимита на кредитование по карточному счету, если
*  в SETUP.DBF установлен флаг CCPC = 1
*  Ф-ия contEdit()
*
* 20-11-2002 TMV
* Изменен порядок заведения счетов для перевода в другое отд. - теперь
*  "на счет" выбирается из списка, а "Со счета" - вводится. И эта операция
*  доступна только на отделении.
*  Ф-ия transAccEdit(), prov2_Name(), prov2_Code().
*
* 21-11-2002 TMV
* При проверке номера счета запрещен ввод при неверном ключе.
* Ф-ия chkTranSchet()
*
* 22-11-2002 TMV
* Исправлена ошибка - неверно записывался счет в WISC->Schet
*  ф-ия transAccPaym()
*
* 24-12-2002
* Переделан флаг установки лимита на кредитование по карточному счету. Если
*  в SETUP.DBF установлен флаг CCPC = 1 - изменение возможно, если нет -
*  запрещено.
*  Ф-ия contEdit()
*
* 17-09-2003 TMV
* Срок действия поручения 54 изменен с 4 месяцев на 12 и установлена
*  автоматическая пролонгация.
* Ф-ия transAccEdit()
*
* 01-12-2003 TMV
* Включены в список доступных для поручений новые накопительные счета
* Ф-ия transEdit()
*
* 25-03-2004 TMV
* Новый порядок работы с картами через OpenWay
* Новая ф-ия crGetOWSpriz()
*
* 30-03-2004  TMV
* Введен новый тип счета - 15 2  125Х
* Ф-ия transEdit()
*
* 23.05.2005  OVD
* Блокировка закрытия СКС при наличие остатка по счету 45509
*

#include "clippExt.ch"
#include "common.ch"
#include "getExit.ch"
#include "inkey.ch"
#include "gsz.ch"
#include "mon.ch"

#define msgS1        "Вы должны ОБЯЗАТЕЛЬНО указать ФАМИЛИЮ и ИНИЦИАЛЫ абонента."
#define msgS2        "Во вводе допустимы только РУССКИЕ буквы."

* NEW OBJECT FILE ---- описание базы поставщиков услуг
func provInfo
static dbInfo := { ".\PROVIDER.DBF", "PROVIDER",;
                 {{ "code2",     "C",  2, 0},;
                  { "code3",     "C",  2, 0},;
                  { "name",      "C", 30, 0},;
                  { "shortName", "C", 10, 0},;
                  { "accCash",   "C", 20, 0},;
                  { "accTran",   "C", 20, 0},;
                  { "buffer",    "C", 30, 0},;
                  { "roCash",    "C",  4, 0}},;
                 {{, "code2+code3"} ,;
                  {, "code3"      }}, 0}
return dbInfo

funcDef provPath with newPath:CU
return basePath( provInfo(), newPath)

funcDef provOpen
return baseMOpen( provInfo())

funcDef provClose
return baseMClose( provInfo())

* NEW OBJECT FILE

func prov2Info
static dbInfo := { ".\PROV2.DBF", "PROV2",;
                 {{ "code3",     "C",  2, 0},;
                  { "name",      "C", 30, 0},;
                  { "buffer",    "C", 30, 0}},;
                 {{, "code3"      }}, 0}
return dbInfo

funcDef prov2Path with newPath:CU
return basePath( prov2Info(), newPath)

funcDef prov2Open
return baseMOpen( prov2Info())

funcDef prov2Close
return baseMClose( prov2Info())

funcDef prov2Buffer with provCode:C local rV := "", s := select()
  if prov2Open()
   if PROV2 ->( dbSeek( provCode)) then rV := PROV2 ->buffer
   prov2Close()
  endif
  select (s)
return rV

* NEW OBJECT FILE --- получение информации по поставщику
funcDef provGetInfo with code2:C, code3:CU, arrName:AU, arrCode:AU ,;
                    arrCash:AU, arrTran:AU, arrBuf:AU               ;
                    local s := select(), used, opened := .T.

DEFAULT code3 TO ""
arrName := {}; arrCash := {}; arrTran := {}; arrCode := {}; arrBuf := {}
used := baseUsed( provInfo())
if !used then opened := provOpen()

if used .OR. opened
   PROVIDER ->( baseSavePos())
   PROVIDER ->( dbSetOrder( 1))
   PROVIDER ->( dbSeek( code2 + code3, .T.))
   while PROVIDER ->code2 == code2 .AND. ;
     if( empty( code3), .T., PROVIDER ->code3 == code3)
     aAdd( arrName, PROVIDER ->name)
     aAdd( arrCode, PROVIDER ->code3)
     aAdd( arrCash, PROVIDER ->accCash)
     aAdd( arrTran, PROVIDER ->accTran)
     aAdd( arrBuf,  alltrim(PROVIDER ->buffer))
     skip 1 alias PROVIDER
   enddo
   PROVIDER ->( baseRestPos())
endif
if !used then provClose()
select ( s)
return NIL


* NEW OBJECT FILE --------------- список магазинов

autofunction setValue assNames  init ""
autofunction setValue assCodes  init ""
autofunction setValue assBuffer init ""

func assInfo
static dbInfo := { ".\ASSORT.DBF", "ASSORT",;
                  {{ "code2",    "C",  2, 0},;
                   { "code3",    "C",  2, 0},;
                   { "code",     "C",  6, 0},;
                   { "name",     "C", 80, 0},;
                   { "buffer",   "C",120, 0},;
                   { "Live",     "L",  1, 0}},;
                  {{, "code2+code3+code"}, {, "code"}}, 0}
return dbInfo

funcDef assPath with newPath:CU
return basePath( assInfo(), newPath)

funcDef assOpen
return baseMOpen( assInfo())

funcDef assClose
return baseMClose( assInfo())

funcDef assGetInfo ;
   with code2:C, arrCode:AU, arrName:AU, arrBuf:AU, code3:CU, useLive:LU ;
   local s := select(), used, opened := .T.

DEFAULT useLive TO .T.

used := baseUsed( assInfo())
arrCode := {}; arrName := {}; arrBuf := {}
if !used then opened := assOpen()
if used .OR. opened
   ASSORT ->( baseSavePos())
   ASSORT ->( netGoTop())
   while ASSORT ->( !Eof())
     if if( useLive, ASSORT ->live, .T.) .AND. ;
                 ASSORT ->code2 == code2 .AND. ;
        if( empty( code3), .T., ASSORT ->code3 == code3)
        aAdd( arrCode, ASSORT ->code)
        aAdd( arrName, ASSORT ->name)
        aAdd( arrBuf,  ASSORT ->buffer)
     endif
     skip 1 alias ASSORT
   enddo

   assNames(  arrName)
   assCodes(  arrCode)
   assBuffer( arrBuf )

   ASSORT ->( baseRestPos())
endif
if !used .AND. opened then assClose()
select ( s)

return NIL

func aMonths
static arr := { ;
"январь",  "февраль",  "март",    "апрель", "май", "июнь", "июль",;
"август" , "сентябрь", "октябрь", "ноябрь", "декабрь" }
return arr

* NEW OBJECT FILE --------------- оформление поручения для Заботы+
funcDef zabEdit ;
  local rV := .T., gets, aMonths, aassNames, aassCodes, ptrass, docNumb,;
        accSchet, accSchet2, ptrSchet, month1, year1, month2, year2    ,;
        nextMonth, curSumma, provName, provTran, provCode, ptrProv, aNames2

if CONTRACT ->dateCur == setCurDate()
  OK( { "Поручение, открытое сегодня, редактировать нельзя.",;
        "Аннулируйте текукущее поручение(F8) и введите правильное."})
  return .F.
endif

accPrizList( ACC_VID_PENS, @accSchet,   @accSchet2)  // Edit by TMV  23-11-2001
															  // Replaced "15 1" to ACC_VID_PENS

nextMonth := addMonth( setCurDate(), 1)
if !empty( CONTRACT ->dateBeg) then nextMonth := CONTRACT ->dateBeg
year1  := Year(  nextMonth)
month1 := Month( nextMonth)

nextMonth := addMonth( nextMonth, 1)
if !empty( CONTRACT ->dateEnd) then nextMonth := CONTRACT ->dateEnd
year2  := Year(  nextMonth)
month2 := Month( nextMonth)

curSumma := CONTRACT ->summa/ 100

fillProvList( "06", @provName, @provCode)
do case
   case len( provName) < 1
        messageB( "По данной услуге нет поставщиков !!!")
        return .F.
   case len( provName) = 1
        ptrProv := 1
   case len( provName) > 1
        ptrProv := popUp( 5, 5, provName)
        if ptrProv = 0 then return .F.
endcase

assGetInfo( "06", @aassCodes, @aassNames,, provCode[ ptrProv])
if len( aassNames) < 1
   messageB( "Справочник магазинов пуст !!!")
   rV := .F.
endif
if len( accSchet) < 1
   messageB( "У клиента нет действующих пенсионных счетов !!!")
   rV := .F.
endif

aNames2 := {}
for ptrAss := 1 to len( aAssNames)
    aAdd( aNames2, "N " + aAssCodes[ ptrAss] + ", " + aAssNames[ ptrAss])
next

ptrass := ptrSchet := 1
gets := {{ "Оформление продажи товаров в кредит"          },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"          },;
         { "Поставщик  : ", block( ptrProv), provName,{||.F.}},;
         { "Магазин    : ", block( ptrass), aNames2       },;
         { "*" + chr( 25)                                 },;
         { ""                                             },;
         { "Дебет.счет : ", block( ptrSchet), accSchet    },;
         { "*" + chr( 25)                                 },;
         { "Сумма      : ", block( curSumma),,,{|g| chkZabSumma( g)}},;
         { ""                                             },;
         { "Начало     : ", block(month1),aMonths(),{||.F.}},;
         { "*" + chr( 25)                                 },;
         { "*",             block( year1), "9999",{||.F.} },;
         { "*г."                                          },;
         { "Окончание  : ", block( month2), aMonths()     },;
         { "*" + chr( 25)                                 },;
         { "*",             block( year2), "9999"         },;
         { "*г."                                          }}

if rV .AND. ( rV := getUp( 5, 5, gets))
   CONTRACT ->code     := cliCode()
   CONTRACT ->code3    := provCode[  ptrProv ]
   CONTRACT ->debetAcc := accSchet2[ ptrSchet, 2]
   CONTRACT ->summa    := curSumma * 100
   CONTRACT ->dateBeg  := packDate( 1, month1, year1)
   CONTRACT ->dateEnd  := packDate( 1, month2, year2)
   if empty( CONTRACT ->docNumb)
      CONTRACT ->dateCur  := setCurDate()
      CONTRACT ->docNumb := confZabDoc( aassCodes[ ptrass])
      CONTRACT ->intDoc  := CONTRACT ->docNumb
   else
      //
      // редактирование существующего поручения
      //
      if WISC ->( mAppend())
         WISC ->schet  := CONTRACT ->debetAcc
         WISC ->type   := "989"
         WISC ->buffer := CONTRACT ->docNumb + ";" + ;
           left( CONTRACT ->buffer, 1)       + ";" + ;
           CONTRACT ->debetAcc       + ";" + DtoC(CONTRACT->dateBeg)  + ";" + ;
           DtoC( CONTRACT ->dateEnd) + ";" + nStr(CONTRACT->summa/100)+ ";" + ;
           CONTRACT ->code2          + ";" + CONTRACT ->code3         + ";" + ;
           nStr( CONTRACT ->summa/ 100)
         WISC ->( netUnLock())
      endif
   endif
endif
return rV

funcDef zabPrint with newContract:L local provName, provCode,  ptr, fullName
fillProvList( "06", @provName, @provCode)
ptr := aTrimmedScan( provCode, CONTRACT ->code3)
if ptr <= 0 then return .F.
fullName := provName[ ptr]
provName := razbivka( provName[ ptr], {{ 12, "L"}, { 12, "L"}, { 12, "L"}})
servDummy( {{ "@docNumb",  CONTRACT ->docNumb          },;
            { "@servSum",  CONTRACT ->summa/ 100       },;
            { "@debSchet", CONTRACT ->debetAcc         },;
            { "@month1",   cMonth( CONTRACT ->dateBeg) },;
            { "@year1",    Year(   CONTRACT ->dateBeg) },;
            { "@month2",   cMonth( CONTRACT ->dateEnd) },;
            { "@year2",    Year(   CONTRACT ->dateEnd) },;
            { "@provName",  fullName                   },;
            { "@provName1", provName[  1]              },;
            { "@provName2", provName[  2]              },;
            { "@provName3", provName[  3]              }})
printOpen( "zab")
servDummy( {})
return NIL

funcDef zabClose ;
   local gets, accSchet, accSchet2, ptrSchet, ptrNal, summa, code, ro ,;
         sim, aOrder, ordInc, creditAcc, curDest, arrCash, arrTran, rV,;
         provName, provCode, fullName, ptr

rV := .F.
accPrizList( "15 1", @accSchet,   @accSchet2)
accSchet := {}
aEval( accSchet2, {|x| aAdd( accSchet, x[ 2])}, 1)
ptrSchet := min( 1, len( accSchet)); ptrNal := 1
summa    := 0
gets     := {{ "Закрытие поручения"                            },;
             { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                          },;
             { "Сумма платежа   :", block( summa),,,{|g| g:varGet > 0}},;
             { "Способ платежа  :", block( ptrNal)              ,;
                     { "Наличные", "Безналичные"}              },;
             { "*" + chr( 25)                                  },;
             { "Дебетуемый счет :", block( ptrSchet), accSchet },;
             { "*" + chr( 25)                                  }}
if rV := getUp( 5, 5, gets)
   code := if( ptrNal == 1, "2011", "5010")
   code := opDecode( code)
   operGetInfo( code, @ro, @sim, @curDest)
   curDest    := "Досрочное погашение кредита по товарам в рассрочку"

   provGetInfo( CONTRACT ->code2, CONTRACT ->code3,,, @arrCash, @arrTran)
   if ptrNal == 1
     if len( arrCash) <> 1
        messageB( "Произошла ошибка.")
        return rV
     else
        creditAcc := arrCash[ 1]
     endif
      ordInc := ordIncIn( setCurDate())
      useZabota( .T.)
      aOrder := vznos_nal( ordInc, setCurDate(), cliFullName(),;
         creditAcc, summa, confBankName()+','+ rTrim(confFilial()),;
         curDest, confKassa(), sim, ro,, .T.)
      useZabota( .F.)
   else
     if len( arrTran) <> 1
        messageB( "Произошла ошибка.")
        return rV
     else
        creditAcc := arrTran[ 1]
     endif
     ordInc := ordIncMem( setCurDate())
     aOrder := memOrder( ordInc, setCurDate(), accSchet2[ ptrSchet, 2],;
         creditAcc, summa, curDest, ro)
     creditAcc := accSchet2[ ptrSchet, 2]
     summa     := - summa
   endif

   fillProvList( "06", @provName, @provCode)
   ptr := aTrimmedScan( provCode, CONTRACT ->code3)
   if ptr <= 0 then return .F.
   fullName := provName[ ptr]

   servDummy( {{ "@dateBeg",   CONTRACT ->dateCur},;
               { "@nextMonth", firstDay( addMonth( setCurDate(), 1)) },;
               { "@docNumb",   CONTRACT ->docNumb},;
               { "@provName",  fullName          }})
   printOpen( "zac")
   servDummy( {})
   if yesNo( "Клиент подписал заявление ?")
      printAOrder( aOrder)
      if ptrNal == 2
         aOrder := memWisc( creditAcc, abs( summa), curDest)
         printAOrder( aOrder)
      endif
      if summa < 0
         if ( summa + sumRest( creditAcc, setCurDate())) < 0
            messageB( "Эта проводка вызовет красное сальдо по счету " + ;
            creditAcc)
            return .F.
         endif
      endif
      if !sumWrite( creditAcc, summa, setCurDate(), code, ordInc,, CONTRACT ->docNumb)
         errLog( "Проводка НЕ сделана!")
      else
         contLastPaym( CONTRACT ->docNumb, ptrNal == 1, abs( summa))
      endif
   else
      messageB( "Поручение НЕ закрыто!"); rV := .F.
   endif
endif
return rV
autofunction setValue useZabota init .F.


funcDef zabFirstPaym with docNumb:C, isCash:L, summa:N ;
  local s, opened := .T., used, rV := .F.
if empty( docNumb) .OR. empty( summa) then return .F.
s := select()
if empty( docNumb) then return .F.
used := baseUsed( contInfo())
if !used then opened := contOpen()
if used .OR. opened
   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 2))
   if CONTRACT ->( dbSeek( docNumb))
      if CONTRACT ->( mRlock())
         CONTRACT ->buffer := if( isCash, "0", "1")  + subStr( CONTRACT ->buffer, 2)
         CONTRACT ->( netUnLock())
      endif
      if WISC ->( mAppend())
         WISC ->schet  := CONTRACT ->debetAcc
         WISC ->type   := "989"
         WISC ->buffer := docNumb   + ";" + if( isCash, "0", "1")     + ";" + ;
           CONTRACT ->debetAcc + ";" + DtoC( CONTRACT ->dateBeg)      + ";" + ;
           DtoC( CONTRACT ->dateEnd) + ";" + nStr( CONTRACT ->summa/ 100)     ;
           + ";" + CONTRACT ->code2  + ";" + CONTRACT ->code3 + ";" + nStr( summa)
         WISC ->( netUnLock())
      endif
   endif
   CONTRACT ->( baseRestPos())
endif
if !used .AND. opened then contClose()
select ( s)

return rV

* ------------------ последний платеж -------------

static funcDef zabHaveFirstPaym with docNumb:C ;
   local arr, rV := .F.
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 2))
if CONTRACT ->( dbSeek( docNumb))
   if CONTRACT ->dateCur <> setCurDate()
      CONTRACT ->( baseRestPos())
      return .T.
   endif
   WISC ->( baseSavePos())
   if WISC ->( dbSeek( CONTRACT ->debetAcc + "989 "))
      while WISC ->schet == CONTRACT ->debetAcc .AND. ;
            WISC ->type  == "989 " .AND. WISC ->( !eof())
        arr := CtoA( WISC ->buffer, ";")
        if allTrim( arr[ 1]) == allTrim( docNumb)
           rV := .T.
        endif
        skip 1 alias WISC
      enddo
   endif
   WISC ->( baseRestPos())
endif
CONTRACT ->( baseRestPos())

return rV


funcDef zabLastPaym with docNumb:C, isCash:L, summa:N, isAnn:LU ;
  local s, opened := .T., used, rV := .F.

DEFAULT isAnn TO .F.

s := select()
if empty( docNumb) then return .F.
used := baseUsed( contInfo())
if !used then opened := contOpen()
if used .OR. opened
   if zabHaveFirstPaym( docNumb)
      CONTRACT ->( baseSavePos())
      CONTRACT ->( dbSetOrder( 2))
      if CONTRACT ->( dbSeek( docNumb))
         if WISC ->( mAppend())
            WISC ->schet  := CONTRACT ->debetAcc
            WISC ->type   := "988"
            WISC ->buffer := docNumb                        + ";" + ;
                   if( isCash, "0", "1")                    + ";" + ;
                   CONTRACT ->debetAcc                      + ";" + ;
                   CONTRACT ->code2 + ";" + CONTRACT ->code3+ ";" + ;
                   nStr( summa) + ";" + if( isAnn, "1", "0")+ ";"
            WISC ->( netUnLock())
         endif
      else
         messageB( "В справочнике CONTRACT нет договора " + docNumb)
      endif
      CONTRACT ->( baseRestPos())
   else
      errLog( "Поручение " + docNumb + " аннулировано без первого взноса!")
   endif
endif
if !used .AND. opened then contClose()
select ( s)
return rV

* NEW OBJECT FILE -------- обработка поручений -----------
func contInfo
static dbInfo := { ".\CONTRACT.DBF", "CONTRACT",;
                  {{ "code2",       "C",  2, 0},;
                   { "code3",       "C",  2, 0},;
                   { "code",        "C", 12, 0},;
                   { "intDoc",      "C", 15, 0},;
                   { "debetAcc",    "C", 20, 0},;
                   { "creditAcc",   "C", 20, 0},;
                   { "docNumb",     "C", 15, 0},;
                   { "dateBeg",     "D",  8, 0},;
                   { "dateEnd",     "D",  8, 0},;
                   { "dateCur",     "D",  8, 0},;
                   { "summa",       "N", 15, 0},;
                   { "status",      "N",  1, 0},;
                   { "buffer",      "C", 10, 0},;
                   { "mBuffer",     "M",512, 0},;
                   { "lEdited",     "L",  1, 0}},;
                  {{, "code+code2" }, {, "docNumb"    },;
                   {, "intDoc"     }, {, "creditAcc"  },;
                   {, "debetAcc"   }                  }, 0}
return dbInfo

funcDef contPath with newPath:CU
return basePath( contInfo(), newPath)

funcDef contOpen
return baseMOpen( contInfo())

funcDef contClose
return baseMClose( contInfo())

funcDef contAnn
   if CONTRACT ->( mRLock())
      CONTRACT ->status  := 5
      CONTRACT ->lEdited := .T.
      CONTRACT ->( netUnLock())
   endif
   getCurOB(): refreshCurrent(): forceStable()
return NIL


funcDef contType with code:CU local rV := "", ptr
ptr := aTrimmedScan( prov2_Code(), code)
if ptr > 0 then rV := prov2_Name()[ ptr]
return padR( rV, 35)

* ------- поиск договора в базе поручений
funcDef contSeekDocNumb with docNumb:C, code3:CU ;
  local rV := .F., opened := .T., used, s

if empty( docNumb) then return .F.
messageStartUp( "Поиск договора " + docNumb)
s    := select()
used := baseUsed( contInfo())
if !used then opened := contOpen()
if used .OR. opened
   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 2))
   if CONTRACT ->( dbSeek( docNumb))
      if !isNIL( code3)
         while rTrim( CONTRACT ->docNumb) == docNumb .AND. CONTRACT ->( !Eof())
           if CONTRACT ->code3 == code3
              // rV := .T.
              rV := CONTRACT ->status == 1
              exit
           endif
           skip 1 alias CONTRACT
         enddo
      else
         rV := CONTRACT ->status == 1
      endif
   endif
   CONTRACT ->( baseRestPos())
endif
select ( s)
messageExit( 0)
return rV

funcDef contBrowse local b, bUp, bDown, bTop, bBtm, aH, aC, fk, code

code  := cliCode()
b     := {|| CONTRACT ->code == cliCode()}
bUp   := {|| contBrUp(   code)}
bDown := {|| contBrDown( code)}
bTop  := {|| contBrTop(  code)}
bBtm  := {|| contBrBtm(  code)}

aH    := {{ "Клиент " + cliFullName()        },;
          { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" },;
          { "Тип услуги      : ", {|| contType( CONTRACT ->code2)},, {|| .F.}},;
          { "Дебетуемый счет : ", {|| CONTRACT ->debetAcc        },, {|| .F.}},;
          { "Пролонгация     : ", {|| showProls()                },, {|| .F.}}}

aC    := {{ "Договор",     block( CONTRACT ->intDoc)     },;
          { "Начало",      block( CONTRACT ->dateBeg)    },;
          { "Окончание",   block( CONTRACT ->dateEnd)    },;
          { "Сумма",       {|| contShowSumma()          }},;
          { "Статус",      {, block( CONTRACT ->status), trustAStatus() }}}

fk    := {,,{{ "Новый",  {|| contEdit( .T.)   }} }  ,;
            {{ "Редак",  {|| contEdit( .F.)   }} }  ,;
            {{ "Печать", {|| contPrintIt()    }} }  ,;
            {{ "Карты",  {|| cardBrowse()     }}    ,;
             { "ГСЗ",    {|| gszWorks()       }}}  ,,;
            {{ "Закрыт", {|| contContClose()  }}    ,;
             { "Возоб",  {|| contContAgain()  }}} }
CONTRACT ->( dbSeek( cliCode()))

CONTRACT ->( browseUp( 2, 2, aH, aC, bUp, bDown, bTop, bBtm,,,,, fk))
return NIL

static funcDef showProls local rV
   if subStr( CONTRACT ->buffer, 3, 1) == "Y"
      rV := "Есть"
   else
      rV := "Нет "
   endif
return rV

static funcDef contBrTrue with code:C local rV
   rV := CONTRACT ->code == code
return rV

static funcDef contBrSeek with code:C local rV
   CONTRACT ->( dbSeek( code, .T.))
   rV := contBrTrue( code)
return rV

static funcDef contBrLast with code:C local rV, seekStr
   CONTRACT ->( dbSeek( code))
   while CONTRACT ->code == code .AND. CONTRACT ->( !eof())
      skip 1 alias CONTRACT
   enddo
   if !contBrTrue( code) then CONTRACT ->( netSkipUp())
   rV := contBrTrue( code)
return rV

static funcDef contBrTop with code:C local rV
   if !contBrSeek( code)
      CONTRACT ->( mAppend())
      CONTRACT ->( netUnLock())
   endif
return rV

static funcDef contBrBtm with code:C local rV
   if !contBrLast( code)
      CONTRACT ->( mAppend())
      CONTRACT ->( netUnLock())
   endif
return rV

static funcDef contBrDown with code:C local rV
   if rV := CONTRACT ->( netSkipDown())
      if !( rV := contBrTrue( code))
         CONTRACT ->( netSkipUp())
         if !contBrTrue( code)
            CONTRACT ->( mAppend())
            CONTRACT ->( netUnLock())
         endif
      endif
   endif
return rV

static funcDef contBrUp with code:C local rV
   if rV := CONTRACT ->( netSkipUp())
      if !( rV := contBrTrue( code))
         CONTRACT ->( netSkipDown())
         if !contBrTrue( code)
            CONTRACT ->( mAppend())
            CONTRACT ->( netUnLock())
         endif
      endif
   endif
return rV


static funcDef contShowSumma local rV, mStr
do case
   case CONTRACT ->summa == -1
        rV   := mStr( 0.00)
   case CONTRACT ->summa <  -1
        mStr := mStr( - CONTRACT ->summa)
        rV   := right( mStr, len( mStr) -1) + "%"
   otherwise
        rV   := mStr( CONTRACT ->summa/ 100)
endcase
return rV

static funcDef contPrintIt
do case
   case CONTRACT ->code2 == "01" do  appPrint(   .F.)
   case CONTRACT ->code2 == "02" do  phonePrint( .F.)
   case CONTRACT ->code2 == "04" do  petcPrint(  .F.)
   case CONTRACT ->code2 == "06" do  zabPrint(   .F.)
   case CONTRACT ->code2 == "07" do  transPrint( .F.)
   case CONTRACT ->code2 == "09" do  cardPrint(  .F.)
   case CONTRACT ->code2 == "11" do  magPrint(   .F.)
   case CONTRACT ->code2 == "14" do  messageB( "Заявление нельзя распечатать !")
   case CONTRACT ->code2 == "76" do  rsKasPrint( .F.)
   case CONTRACT ->code2 == GSZ_CODE_2 do gszPrint( .F.)
   case CONTRACT ->code2 == "54" do  transAccPrint( .F.)  // Add by TMV  28-10-2002
endcase
return NIL

funcDef contCalcTodaySum ;
  with curCode:C, curDate:D ;
  local rV := 0.00, curRec

curRec := CONTRACT ->( recNo())
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 1))
CONTRACT ->( dbSeek( curCode, .T.))
while CONTRACT ->code == curCode .AND. CONTRACT ->( !eof())
  if CONTRACT ->dateBeg <= curDate .AND. CONTRACT ->dateEnd >= curDate .AND. ;
     CONTRACT ->dateCur == curDate .AND. CONTRACT ->status  == 1       .AND. ;
     CONTRACT ->( recNo()) <> curRec
     if CONTRACT ->summa > 0
        if CONTRACT ->code2 == "15" .AND. CONTRACT ->code3 == "73" .AND. ;
           empty( CONTRACT ->buffer)
           //
           // это разовое поручение по ГСЗ - > не учитываем
           //
        else
           //
           // простое поручение
           //
           rV += CONTRACT ->summa
        endif
     endif
  endif
  skip 1 alias CONTRACT
enddo
CONTRACT ->( baseRestPos())

return round( rV/ 100, 2)

funcDef contContAgain

if noYes( "Вы действительно хотите возобновить поручение ?")
   if CONTRACT ->( mRLock())
      CONTRACT ->status := 1
      CONTRACT ->lEdited := .T.
      CONTRACT ->( mRLock())
      getCurOB(): refreshCurrent(): forceStable()
   endif
endif

return NIL

static funcDef chkZabSumma with g:O local rV, tmpSum

tmpSum := contCalcTodaySum( CLIENTS ->code, setCurDate())
tmpSum := round( abs( CLIENTS ->credSumma)/ 100 - tmpSum, 2)
if !( rV := g:varGet <= tmpSum)
   errLog({ "Максимальная сумма ежемесячного списания " + mStr( tmpSum),;
           "Вы ввели " + mStr( g:varGet) })
   clear typeAHead
endif
if rV .AND. g:varGet < 0
   errLog( "Вы должны ввести положительное число !!!")
   clear typeAHead
   rV := .F.
endif
return rV

funcDef prov2_Name ;
local arr := { "Оплата жилья и коммунальных услуг ",;
               "Оплата телефонов                  ",;
               "Перечисления на накопительный счет",;
               "Перечисления на карточный счет    ",;
               "Товары в рассрочку Забота+        ",;
               "Покупка облигаций ГСЗ             ",;
               "Комплексное обслуживание тек.счета",;
               "Оплата электроэнергии"             ,;
               "Установка лимита кредитования"     ,;
               "Подписка в рассрочку"              ,;
               "Периодическая покупка валюты"      }
// Edit by TMV  20-11-2002
if val( setUpValue( "PLACE")) == 0  //  Add by TMV  20-11-2002
   aAdd( arr, "Перечисление средств на свой счет")   // Add by TMV 28-10-2002
endif                               //  Add by TMV  20-11-2002
return arr

funcDef prov2_Code local arr
   arr  := { "01", "02", "07", "09", "06", GSZ_CODE_2, "76", "04", "14", "11", "52"}
// Edit by TMV  20-11-2002
if val( setUpValue( "PLACE")) == 0  //  Add by TMV  20-11-2002
	aAdd( arr, "54")  // Add by TMV  28-1-2002
endif                               //  Add by TMV  20-11-2002
return arr

funcDef contLock with newContract:L, ptr:NCU local rV
if newContract
   rV := CONTRACT ->( mAppend())
   CONTRACT ->code     := cliCode()
   do case
      case valType( ptr) == "N"
      case valType( ptr) == "C"
           ptr := aScan( prov2_Code(), {|x| x == ptr})
      otherwise
         ptr := popUp( 5, 5, prov2_Name())

			if ptr > 0 .And.;          // Add by TMV  31-10-2002
					transExist(cliCode(), prov2_Code()[ ptr])  // Add by TMV  31-10-2002
   			messageB( "Уже есть поручение на перевод всей суммы !!!")
				ptr = 0
			endif                      // Add by TMV  31-10-2002
   endcase
   if ptr < 1 then return .F.
   CONTRACT ->code2   := prov2_Code()[ ptr]
   CONTRACT -> status := 1
else
   rV := CONTRACT ->( mRLock())
endif
return rV

funcDef contUnLock with delRecord:L
   CONTRACT ->lEdited := .T.
   if delRecord
      CONTRACT ->( clearRec())
   endif
   CONTRACT ->( netUnLock())
return NIL


funcDef contEdit with newContract:L, ptr:NCU, mustRefresh:LU ;
   local rV := .F., s := select()

DEFAULT mustRefresh TO .T.
if contLock( newContract, ptr)
   do case
      case CONTRACT ->code2 == "06"
           rV := zabEdit()
           if rV then zabPrint( newContract)
      case CONTRACT ->code2 == "01"
           rV := appEdit()
           if rV then appPrint( newContract)
      case CONTRACT ->code2 == "02"
           rV := phoneEdit( newContract)
           if rV then phonePrint( newContract)
      case CONTRACT ->code2 == "04"
           rV := petcEdit( newContract)
           if rV then petcPrint( newContract)
      case CONTRACT ->code2 == "07"
           rV := transEdit()
           if rV then transPrint( newContract)
      case CONTRACT ->code2 == "09"
           rV := cardEdit()
           if rV then cardPrint( newContract)
      case CONTRACT ->code2 == "11"
           rV := MagEdit()
           if rV then MagPrint( newContract)
      case CONTRACT ->code2 == "14"
			if setUpValue('CCPC') != '1'                   // Add by TMV  14-11-2002
   			messageB( "Установка лимита запрещена !!!") // Add by TMV  14-11-2002
			else                                           // Add by TMV  14-11-2002
           rV := credEdit()
           // if rV then credPrint( newContract)
			endif
      case CONTRACT ->code2 == "76"
           rV := rsKasEdit()
           if rV then rsKasPrint( newContract)
      case CONTRACT ->code2 == GSZ_CODE_2
           rV := gszEdit( newContract)
      case CONTRACT ->code2 == "52"
           rV := valEdit( newContract)
           if rV then valPrint( newContract)
      case CONTRACT ->code2 == "54"             //  Add by TMV  28-10-2002 
           rV := transAccEdit()                    //  Add by TMV  28-10-2002
           if rV then transAccPrint( newContract)  //  Add by TMV  28-10-2002
   endcase
endif
contUnLock( !rV .AND. newContract)

select ( s)
if mustRefresh
   if CONTRACT ->( emptyRec())
      contBrBtm( cliCode())
   endif
   getCurOB(): refreshAll(): forceStable()
endif
return rV

autofunction setValue servDummy init {}

* NEW OBJECT FILE ----------- закрыть договор
funcDef contContClose local ptr := 1, rV := .F.
if CONTRACT ->( !emptyRec())
   if CONTRACT ->status <= 1
      if noYes( "Вы действительно хотите закрыть поручение ?")
         if CONTRACT ->( mRLock())
            if CONTRACT ->code2 $ "06"
               ptr := dialog( "Поручение нужно", { "Закрыть", "Аннулировать"})
            endif
            do case
               case ptr == 1
                    do case
                       case CONTRACT ->code2 == "01" do rV := appClose()
                       case CONTRACT ->code2 == "02" do rV := phoneClose()
                       case CONTRACT ->code2 == "04" do rV := petcClose()
                       case CONTRACT ->code2 == "06" do rV := zabClose()
                       case CONTRACT ->code2 == "07" do rV := transClose()
                       case CONTRACT ->code2 == "09" do rV := crClose()
                       case CONTRACT ->code2 == "11" do rV := magClose()
                       case CONTRACT ->code2 == "14" do rV := credCrClose()
                       case CONTRACT ->code2 == "76" do rV := rsKasClose()
                       case CONTRACT ->code2 == GSZ_CODE_2 do rV := gszClose()
                       case CONTRACT ->code2 == "52" do rV := valClose()
//  Add by TMV  28-10-2002
                       case CONTRACT ->code2 == "54" do rV := transAccClose()
                    endcase
                    if rV then CONTRACT ->status  := 2
               case ptr == 2
                 if noYes( "Вы действительно хотите аннулировать поручение ?")
                    do case
                       case CONTRACT ->code2 == "06"
                         zabLastPaym( CONTRACT ->docNumb, .T., 0, .T.)
                       // case CONTRACT ->code2 == "04"
                       //  petcPrint( .F., .T.)
                       //  petcLastPaym( .T.)
                    endcase
                    CONTRACT ->status := 5
                    endif
            endcase
            if rV then CONTRACT ->lEdited := .T.
            CONTRACT ->( netUnLock())
            getCurOB(): refreshAll(): forceStable()
         endif
      endif
   endif
endif
return NIL

* NEW OBJECT FILE ---------- оформление поручения -------------
funcDef contFirstPaym with docNumb:C, isCash:L, summa:N local rV
   rV := zabFirstPaym( docNumb, isCash, summa)
return rV

* ------------------ последний платеж -------------
funcDef contLastPaym with docNumb:C, isCash:L, summa:N local rV
   rV := zabLastPaym( docNumb, isCash, summa)
return rV

* --------------- оплата за квартиру -------------------------
funcDef appFirstDate with g:O, firstDate:D, ptr:N local rV, realDate, m1, y1
   m1       := aTrimmedScan( aMonths(), M ->getList[ ptr]: varGet)
   y1       := g:varGet
   realDate := packDate( 1, m1, y1)
   rV       := .T.
   if realDate < firstDay( firstDate) .AND. ( g:exitState == GE_DOWN .OR.  g:exitState == GE_ENTER)
      messageB( "Дата начала не может быть меньше " + ;
         DtoC( firstDay( firstDate)) + " !!!")
      rV := .F.
   endif
return rV

funcDef appLastDate with g:O, firstDate:D, ptr1:N, ptr2:N, isPetc:LU ;
   local fDate, realDate, y1, m1, y2, m2

   DEFAULT isPetc TO .F.

   m1       := aTrimmedScan( aMonths(), M ->getList[ ptr1]: varGet)
   y1       := M ->getList[ ptr1 + 1]: varGet
   fDate    := packDate( 1, m1, y1)

   m2       := aTrimmedScan( aMonths(), M ->getList[ ptr2]: varGet)
   y2       := M ->getList[ ptr2 + 1]: varGet
   realDate := packDate( 1, m2, y2)

   if realDate < fDate .AND. ( g:exitState == GE_DOWN .OR. g:exitState == GE_ENTER)
      messageB( "Дата окончания не может быть меньше " + DtoC( fDate) + " !!!")
      return .F.
   endif
   if isPetc .AND. ( realDate - fDate > 181) .AND. ( g:exitState == GE_DOWN .OR. g:exitState == GE_ENTER)
      messageB( "Поручение можно заключить только на полгода")
      return .F.
   endif

return .T.


funcDef checkD1 with g:O local rV
   rV := g:varGet >= setCurDate()
   if !rV then ;
      messageB( "Дата не может быть меньше " + DtoC( setCurDate()) + " !!!")
return rV

// Add by TMV  18-05-2002
// Проверка даты начала поручения для карточного счета.
// Должна быть не меньше даты открытия этого счета
funcDef checkDBeg with g:O, aMask:AC, aSchet:AC, ptr:N, inc:NU local rV, firstDate, dateInc,;
		n := 0, sAcc := '', nWin
   DEFAULT inc to -1
	sAcc = allTrim(M ->getList[ ptr]:varGet)
	n = aScan(aMask, {|x| Left(allTrim(x), 30) = Left(sAcc, 30)}, 1)
   firstDate := fSchOpenDate(aSchet[n, 2])
   rV := g:varGet >= firstDate
   if !rV then ;
      messageB( "Дата не может быть меньше " + DtoC( firstDate) + " !!!")
   if rV .AND. inc > 0
      dateInc := addMonth( firstDate, inc)
      if g:varGet > dateInc
         rV := .F.
         messageB( "Дата не может быть больше " + DtoC( DateInc) + " !!!")
      endif
   endif
return rV

funcDef checkD2 with g:O, ptr:N, inc:NU local rV, firstDate, dateInc
   DEFAULT inc to -1
   firstDate := M ->getList[ ptr]:varGet
   rV := g:varGet >= firstDate
   if !rV then ;
      messageB( "Дата не может быть меньше " + DtoC( firstDate) + " !!!")
   if rV .AND. inc > 0
      dateInc := addMonth( firstDate, inc)
      if g:varGet > dateInc
         rV := .F.
         messageB( "Дата не может быть больше " + DtoC( DateInc) + " !!!")
      endif
   endif
return rV

funcDef appDocNumb with g:O ;
   local rV := .T., i,  curSchet, shiftDig, testSum, testChar, testDig, modDig,;
         calcDig, checkVal, recNo

   curSchet := allTrim( g:varGet)
   if len( curSchet) < 9
      messageB( "Длина лицевого счета не может быть меньше 9 символов !!!")
      return .F.
   endif

   shiftDig := { 3, 7, 9, 10, 5, 8, 4, 2}
   testSum  := 0
   checkVal := left( curSchet, 8)
   testDig  := val( right( curSchet, 1))
   rV       := .T.
   for i := 1 to 8
     testChar := subStr( checkVal, i, 1)
     if testChar == "-" then testChar := "0"
     testSum  += val( testChar) * shiftDig[ i]
   next
   modDig  := testSum % 11
   calcDig := if( modDig == 0 .OR. modDig == 1, 0, 11 - modDig)
   if testDig <> calcDig
      messageB( "Лицевой счет необходимо исправить !")
      rV := .F.
      return rV
   endif

//   if testDig <> calcDig
//      rV := !yesNo( { "Лицевой счет " + curSchet + " НЕВЕРЕН !",;
//                      "Будете его исправлять ?"})
//   endif
   if rV
      CONTRACT ->( baseSavePos())
      CONTRACT ->( dbSetOrder( 2))
      recNo := CONTRACT ->( recNo())
      if CONTRACT ->( rV := dbSeek( curSchet))
         if CONTRACT ->( recNo()) == recNo then CONTRACT ->( dbSkip())
         if CONTRACT ->docNumb == curSchet .AND. CONTRACT ->( !eof())
            messageB( "Счет " + curSchet + " уже оплачивается со счета " + ;
               CONTRACT ->debetAcc + " !!!"); rV := .F.
         endif
      else
         rV := .T.
      endif
      CONTRACT ->( baseRestPos())
   endif
return rV

funcDef appEdit ;
   local rV:= .F., gets, tmpNumber, accSchet, accSchet2,    ptrSchet,;
         year1, month1,  nextMonth, month2,   year2, toAdd, ptrProv ,;
         yesNo, ptrProl, firstDate, provName, provCode,     canEdit

accPrizList( "15 1", @accSchet,   @accSchet2)
ptrSchet := max( 1, aScan( accSchet2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))

nextMonth := addMonth( setCurDate(), if( Day( setCurDate()) >= 23, 2, 1))

firstDate := firstDay( nextMonth)
if !empty( CONTRACT ->dateBeg) then nextMonth := CONTRACT ->dateBeg
year1  := Year(  nextMonth)
month1 := Month( nextMonth)

nextMonth := addMonth( nextMonth, 1)
if !empty( CONTRACT ->dateEnd) then nextMonth := CONTRACT ->dateEnd

year2  := Year(  nextMonth)
month2 := Month( nextMonth)

fillProvList( "01", @provName, @provCode)
ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))

if len( accSchet) < 1
   messageB( "У клиента нет действующих пенсионных счетов !!!")
   return .F.
endif

if len( provName) < 1
   messageB( "По данной услуге нет поставщиков !!!")
   return .F.
endif

yesNo := { "Есть", "Нет "}
ptrProl := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", 1, 2)

canEdit := empty( CONTRACT ->intDoc)
if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc := confAppDoc()
endif

gets :={{ "Оплата жилья и коммунальных услуг" },;
        { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" },;
        { "Номер        :", block( CONTRACT ->intDoc),,{||.F.}},;
        { "Лицевой счет :", block( CONTRACT ->docNumb),,{|| canEdit},;
                        {|g| appDocNumb( g)             }},;
        { ""                                             },;
        { "Дебет.счет   :", block( ptrSchet), accSchet,{|| canEdit}   },;
        { "*" + chr( 25)                                 },;
        { "Поставщик    :", block( ptrProv), provName, {|| canEdit}   },;
        { "*" + chr( 25)                                 },;
        { "Начало       :", block(month1)                 ,;
                  aClone( aMonths()),{|| canEdit}                     },;
        { "*" + chr( 25)                                 },;
        { "*",              block( year1), "9999"    ,{||canEdit},;
                        {|g| appFirstDate(g,firstDate,5)}},;
        { "*г."                                          },;
        { "Окончание    :", block( month2), aMonths()    },;
        { "*" + chr( 25)                                 },;
        { "*",              block( year2), "9999"        ,,;
                        {|g|appLastDate(g,firstDate,5,7)}},;
        { "*г."                                          },;
        { "Пролонгация  :", block( ptrProl), yesNo   }}

rV := getUp( 5, 5, gets)
if rV
   CONTRACT ->code     := cliCode()
   CONTRACT ->code3    := provCode[  ptrProv ]
   CONTRACT ->debetAcc := accSchet2[ ptrSchet, 2]
   CONTRACT ->dateBeg  := packDate( 1, month1, year1)
   CONTRACT ->dateEnd  := lastDay( packDate( 1, month2, year2))
   if empty( CONTRACT ->dateCur) then CONTRACT ->dateCur  := setCurDate()
   CONTRACT ->buffer   := left( CONTRACT ->buffer, 2) + ;
      if( ptrProl <= 1, "Y", "N") + subStr( CONTRACT ->buffer, 4)
   appFirstPaym()
endif
return rV

funcDef appPrint with newContract:L ;
   local orgName1, orgName2, orgName3, prol1, prol2,;
         orgName,  provName, provCode, ptrProv, hasProl

if newContract
   fillProvList( "01", @provName, @provCode)
   ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))
   hasProl := subStr( CONTRACT ->buffer, 3, 1) == "Y"

   prol1   := if( hasProl, "8. НАСТОЯЩЕЕ ПОРУЧЕНИЕ МОЖЕТ БЫТЬ ПРОЛОНГИРОВАНО НА ТАКОЙ ЖЕ ПЕРИОД,", "")
   prol2   := if( hasProl, "   ЕСЛИ МНОЙ НЕ БУДЕТ ПИСЬМЕННО ЗАЯВЛЕНО О ЕГО ПРЕКРАЩЕНИИ.", "")

   orgName := provName[ ptrProv]
   orgName := razbivka( orgName, {{ 14, "L"}, { 14, "L"}, { 14, "L"}, { 14, "L"}})

   servDummy( {{ "@docNumb",  CONTRACT ->intDoc           },;
               { "@lSchet",   CONTRACT ->docNumb          },;
               { "@debSchet", CONTRACT ->debetAcc         },;
               { "@d1",       CONTRACT ->dateBeg          },;
               { "@d2",       CONTRACT ->dateEnd          },;
               { "@orgName",  provName[ ptrProv]          },;
               { "@orgName1", orgName[ 1]                 },;
               { "@orgName2", orgName[ 2]                 },;
               { "@orgName3", orgName[ 3]                 },;
               { "@orgName4", orgName[ 4]                 },;
               { "@prol1",    prol1                       },;
               { "@prol2",    prol2                       },;
               { "@curdate",  rDate( setCurDate())        },;
               { "@atrname",  "Отделение N "+ confOtdNumb() +;
                    ", " + rTrim( confFilial()) + ' ' + confBankName()} })
   printOpen( "cm1")
   servDummy( {})
else
   contInitServ()
   printOpen( "cm3")
   contCloseServ()
endif
return NIL


funcDef appFirstPaym local rV, prol
if rV := WISC ->( mAppend())
   prol := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", "1", "0")
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := "987"
   WISC ->buffer := CONTRACT ->intDoc         + ";" + ;
                    CONTRACT ->debetAcc       + ";" + ;
                    DtoC( CONTRACT ->dateBeg) + ";" + ;
                    DtoC( CONTRACT ->dateEnd) + ";" + ;
                    CONTRACT ->code2          + ";" + ;
                    CONTRACT ->code3          + ";" + ;
                    prol                      + ";" + ;
                    CONTRACT ->docNumb        + ";"
   WISC ->( netUnLock())
endif
return rV

funcDef appLastPaym local rV
if rV := WISC ->( mAppend())
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := "986"
   WISC ->buffer := CONTRACT ->intDoc + ";" + CONTRACT ->debetAcc + ";" + ;
                    CONTRACT ->code2  + ";" + CONTRACT ->code3    + ";" + ;
                    CONTRACT ->docNumb        + ";"
   WISC ->( netUnLock())
endif
return rV

funcDef appClose local rV, arr, nextD
   if rV := appLastPaym()
      contInitServ()
      arr   := servDummy()
      nextD := addMonth(setCurDate(), 1)
      if Day( nextD) >= 23 then nextD := addMonth( nextD, 1)
      // aAdd( arr, { "@stopdate", rMonth( nextD) + " " + sYear( nextD) + " г."})
      aAdd( arr, { "@stopdate", firstDay( nextD) })
      aAdd( arr, { "@typewisc", "на периодическое перечисление коммунальных платежей"})
      // printOpen( "cm2")
      printOpen( "serst")
      contCloseServ()
   endif
return rV



* ----------------- комплексное обслуживание тек.счета --------------------
funcDef rsKasEdit ;
   local rV:= .F., gets, tmpNumber, accSchet, accSchet2, ptrSchet  ,;
         toAdd, ptrProv, yesNo, ptrProl, provName, provCode, a1, a2,;
         aBuf

if rsKasExist()
   messageB( "У этого клиента уже есть такой договор !!!")
   return .F.
endif

assGetInfo( "76", @a1, @a2, @aBuf)
if len( a1) < 1
   messageB( "Нет информации о тарифах")
   return .F.
endif

accPrizList( "15 1  1  ", @accSchet,   @accSchet2)
ptrSchet := max( 1, aScan( accSchet2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))

fillProvList( "76", @provName, @provCode)
ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))

if len( accSchet) < 1
   messageB( "У клиента нет действующих пенсионных счетов !!!")
   return .F.
endif

if len( provName) < 1
   messageB( "По данной услуге нет поставщиков !!!")
   return .F.
endif

yesNo := { "Есть", "Нет "}
ptrProl := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", 1, 2)

if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc  := confRsKas()
   CONTRACT ->dateBeg := setCurDate()
   CONTRACT ->dateEnd := setCurDate() + 365
endif
gets :={{ "Комплексное обслуживание ТПС" },;
        { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" },;
        { "Номер           :", block( CONTRACT ->intDoc),,{||.F.} },;
        { "Пенсионный счет :", block( ptrSchet), accSchet         },;
        { "*" + chr( 25)                                          },;
        { ""                                                      },;
        { "Начало          :", block( CONTRACT ->dateBeg),,{||.F.}},;
        { "Окончание       :", block( CONTRACT ->dateEnd),,{||.F.}},;
        { "Пролонгация     :", block( ptrProl), yesNo       }}
if rV := getUp( 5, 5, gets)
   CONTRACT ->code     := cliCode()
   CONTRACT ->code3    := "76"
   CONTRACT ->summa    := val( aBuf[ 1]) * 100
   CONTRACT ->debetAcc := accSchet2[ ptrSchet, 2]
   if empty( CONTRACT ->dateCur) then CONTRACT ->dateCur  := setCurDate()
   CONTRACT ->buffer   := left( CONTRACT ->buffer, 2) + ;
      if( ptrProl <= 1, "Y", "N") + subStr( CONTRACT ->buffer, 4)
   rsKasFirstPaym()
endif
return rV

funcDef rsKasExist with param:NU local rV := .F., recNo
param := 0
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 1))
recNo := CONTRACT ->( recNo())
CONTRACT ->( dbSeek( CLIENTS ->code + "76", .F.))
while CONTRACT ->code  == CLIENTS ->code .AND. ;
      CONTRACT ->code2 == "76" .AND. CONTRACT ->( !eof())
      //
      // 15.05.2000  удалено по итогам разговора с Мишей:
      // дата окончания выгружается первым числом месяца( конвертер )
      //
      // if CONTRACT ->dateBeg <= setCurDate() .AND. ;
      //   CONTRACT ->dateEnd >  setCurDate() .AND. ;
      if CONTRACT ->status  == 1            .AND. ;
         CONTRACT ->code3   == "76"
         param++
         if CONTRACT ->( recNo()) <> recNo then rV := .T.
      endif
      skip 1 alias CONTRACT
enddo
CONTRACT ->( baseRestPos())
return rV

funcDef rsKasFirstPaym local hasProl
if WISC ->( mAppend())
   hasProl := subStr( CONTRACT ->buffer, 3, 1) == "Y"
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := RS_BEG
   WISC ->buffer := CONTRACT ->intDoc + ";" + DtoC(CONTRACT  ->dateBeg)+";"+;
            DtoC( CONTRACT ->dateEnd) + ";" + nStr(CONTRACT->summa/100)+";"+;
            if( hasProl, "1", "0")
   WISC ->( netUnLock())
endif
return NIL

funcDef rsKasLastPaym local hasProl, rV
if WISC ->( dbSeek( CONTRACT ->debetAcc + RS_END))
   if allTrim( CONTRACT ->intDoc ) == allTrim(CtoA( WISC ->buffer, ";")[ 1])
      messageB( "Поручение на закрытие соглашения сформировано !!!")
      return .F.
   endif
endif
if WISC ->( rV := mAppend())
   hasProl := subStr( CONTRACT ->buffer, 3, 1) == "Y"
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := RS_END
   WISC ->buffer := CONTRACT ->intDoc + ";" + DtoC( setCurDate())
   WISC ->( netUnLock())
endif
return rV


funcDef rsKasPrint with newContract:L ;
   local orgName1, orgName2, orgName3, prol1, prol2, prol3,;
         orgName,  provName, provCode, ptrProv, hasProl, prol := {}

fillProvList( "76", @provName, @provCode)
ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))
hasProl := subStr( CONTRACT ->buffer, 3, 1) == "Y"

if hasProl 
   prol := { "",;
             "По окончании срока действия соглашения, при отсутствии долга по оплате",;
             "расчетно-кассового комплексного обслуживания, продолжить его действие на",;
             "тот же срок (1 год).",;
             ""}
endif
servDummy( {{ "@debSchet", CONTRACT ->debetAcc         },;
            { "@d1",       CONTRACT ->dateBeg          },;
            { "@d2",       CONTRACT ->dateEnd          },;
            { "@prol",     prol                        },;
            { "@summa",    getS( CONTRACT ->summa/ 100)},;
            { "@curD",     CONTRACT ->dateBeg          }})
printOpen( "RSOPN")
servDummy( {})
return NIL


funcDef rsKasClose local arr
   if rsKasLastPaym()
      servDummy( {{ "@atr2name", rTrim( confFilial()) + " " + confOtdName()},;
                  { "@debSchet", CONTRACT ->debetAcc                       },;
                  { "@curD2",    setCurDate() + 5                          }})
      printOpen( "RSCLO")
      servDummy({})
   endif
return .F.


*--------------------- оплата за телефоны ----------------------
funcDef phoneEdit with newContract:L;
   local accSchet, accSchet2, ptrSchet, gets, tmpName, tmpStreet,;
         tmpFlat, tmpBlock, tmpHouse, aPhCode, aPhName, ptrPhone,;
         ptrAB, aAB,aAB2, tmpPct, firstDate, nextMonth, pSubType,;
         year1, month1, year2, month2, provName, provCode       ,;
         ptrProl, ptrProv, rV, yesNo, tmpStr

yesNo    := { "Есть", "Нет"}
accPrizList( "15 1", @accSchet,   @accSchet2)
ptrSchet := max( 1, aScan( accSchet2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))

nextMonth := addMonth( setCurDate(), 1)
firstDate := nextMonth
if !empty( CONTRACT ->dateBeg) then nextMonth := CONTRACT ->dateBeg
year1  := Year(  nextMonth)
month1 := Month( nextMonth)

nextMonth := addMonth( nextMonth, 1)
if !empty( CONTRACT ->dateEnd) then nextMonth := CONTRACT ->dateEnd

year2  := Year(  nextMonth)
month2 := Month( nextMonth)

fillProvList( "02", @provName, @provCode)
ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))

if len( provName) < 1
    messageB( "Для этой услуги нет поставщиков !!!")
    return .F.
endif

if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc := confAppDoc()
endif

aAB       := { "АБОНЕНТСКАЯ ПЛАТА"}
aAB2      := { "01"}
tmpStr    := CONTRACT ->mBuffer
tmpName   := getKeyWord( tmpStr, "ФИО",   cliFio())
tmpStreet := getKeyWord( tmpStr, "УЛ.",   CLIENTS ->Street)
tmpHouse  := getKeyWord( tmpStr, "ДОМ",   CLIENTS ->house)
tmpBlock  := getKeyWord( tmpStr, "КОРП.", CLIENTS ->Block)
tmpFlat   := getKeyWord( tmpStr, "КВ.",   CLIENTS ->Flat)
tmpPct    := getKeyWord( tmpStr, "ЛЬГОТЫ", "00")
ptrPhone  := getKeyWord( tmpStr, "ТИП",   "ЛК")
pSubType  := getKeyWord( tmpStr, "ВИД",   " ")
ptrAB     := getKeyWord( tmpStr, "ОПЛАТА", "01")

tmpName   := padR( tmpName,   30)
tmpStreet := padR( tmpStreet, 30)
tmpHouse  := padR( tmpHouse,   7)
tmpBlock  := padR( tmpBlock,   4)
tmpFlat   := padR( tmpFlat,    4)

ptrAB     := max( 1, aTrimmedScan( aAB2, ptrAB))

assGetInfo( "02", @aPhCode, @aPhName)
if len( aPhCode) < 1
   messageB( "Отсутсвуют виды телефонов !!!")
   return .F.
endif
ptrPhone := max( 1, aTrimmedScan( aPhCode, ptrPhone))
ptrProl  := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", 1, 2)

gets := {{ "Оплата телефона"                                               },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                                         },;
         { "Номер        :", block( CONTRACT ->intDoc),,{|| newContract}   },;
         { "*Абонент :",     block( CONTRACT ->docNumb), "@R 999-99-99",     ;
                 {|| newContract}, {|g| checkPhone( g)                    }},;
         { "Дебет.счет   :", block( ptrSchet), accSchet,{||newContract}    },;
         { "*" + chr( 25)                                                  },;
         { "ФИО          :", block( tmpName),,                               ;
                                 {|| newContract},{|g| checkName( g)      }},;
         { "Улица        :", block( tmpStreet),,                             ;
                 {|| newContract}, {|g| checkStreet( g)                   }},;
         { "Дом          :", block( tmpHouse),,                              ;
                 {|| newContract}, {|g| checkHouse( g)                    }},;
         { "*Корпус :",      block( tmpBlock),,                              ;
                 {|| newContract},  {|g| checkBlock( g, "0")              }},;
         { "*Квартира :",    block( tmpFlat),,                               ;
                 {|| newContract}, {|g| checkFlat( g, "0")                }},;
         { ""                                                              },;
         { "Тел.узел     :", block( ptrProv), provName, {|| newContract}   },;
         { "*" + chr( 25)                                                  },;
         { "Тип телефона :", block( ptrPhone), aPhName,{|| newContract}     ,;
                              {|g| checkPType( g, aPhCode, aPhName)       }},;
         { "Вид телефона :", block( pSubType), "@!X"                        ,;
                              {|g| newContract .AND. preSubType( g, aPhCode, aPhName)        },;
                              {|g|postSubType( g, aPhCode, aPhName)       }},;
         { "Вид оплаты   :", block( ptrAB), aAB, {|| newContract}          },;
         { "*" + chr( 25)                                                  },;
         { "Проц.льгот   :", block( tmpPct),"@K 99", {|| newContract}       ,;
                               {|g| checkPct( g)                          }},;
         { "Начало       :", block(month1), aClone( aMonths()),{|| newContract}             },;
         { "*" + chr( 25)                                                  },;
         { "*",              block( year1), "9999",{|| newContract}         ,;
                         {|g| appFirstDate( g, firstDate, 14)             }},;
         { "*г."                                                           },;
         { "Окончание    :", block( month2), aMonths()                     },;
         { "*" + chr( 25)                                                  },;
         { "*",              block( year2), "9999"                         ,,;
                         {|g| appLastDate( g, firstDate, 14, 16)          }},;
         { "*г."                                                           },;
         { "Пролонгация  :", block( ptrProl), yesNo                       }}

if rV := getUp( 4, 5, gets)
   setKeyWord( @tmpStr, "ТИП",     aPhCode[ ptrPhone])
   setKeyWord( @tmpStr, "ВИД",     pSubType)
   setKeyWord( @tmpStr, "ОПЛАТА",  aAB2[ ptrAB])
   setKeyWord( @tmpStr, "ЛЬГОТЫ",  tmpPct)
   setKeyWord( @tmpStr, "ФИО",     tmpName)
   setKeyWord( @tmpStr, "УЛ.",     tmpStreet)
   setKeyWord( @tmpStr, "ДОМ",     tmpHouse)
   setKeyWord( @tmpStr, "КОРП.",   tmpBlock)
   setKeyWord( @tmpStr, "КВ.",     tmpFlat)

   CONTRACT ->debetAcc := accSchet2[ ptrSchet, 2]
   CONTRACT ->mBuffer  := tmpStr
   CONTRACT ->dateBeg  := packDate( 1, month1, year1)
   CONTRACT ->dateEnd  := packDate( 1, month2, year2)
   if empty( CONTRACT ->dateCur) then CONTRACT ->dateCur  := setCurDate()
   CONTRACT ->code3    := provCode[ ptrProv]
   CONTRACT ->buffer   := left( CONTRACT ->buffer, 2) + ;
      if( ptrProl <= 1, "Y", "N") + subStr( CONTRACT ->buffer, 4)
   phoneFirstPaym()
endif

return rV

funcDef checkPct with g:O local rV := .T., value, i
   if val( g: varGet) < 0
      messageB( "Проценты не могут быть ОТРИЦАТЕЛЬНЫМИ !!!")
      rV := .F.
   endif
return rV

funcDef preSubType with g:O, aPhCode:A, aPhName:A local code, rV
  code := aPhCode[ aTrimmedScan( aPhName, M ->getList[ 10]:varGet)]
  rV   := allTrim( code) == "ЛК"
return rV

funcDef postSubType with g:O, aPhCode:A, aPhName:A local code, rV
rV := g:varGet $ "ЛК" .AND. len( allTrim( g:varGet)) == 1
if !rV
   messageB( 'Допустимые значения - "Л"-индивидуальный "К"-коллективный !!!')
endif
return rV

funcDef checkPType with g:O, aPhCode:A, aPhName:A local name, type

type := allTrim( aPhCode[ aTrimmedScan( aPhName, g:varGet)])
name := allTrim( upper( M ->getList[ 4]:varGet))

if ( type == "К" .OR. type == "КБ" ) .AND. name <> "КОЛЛЕКТИВ"
   messageB( "Неверное значение фамилии для КОЛЛЕКТИВНОГО телефона !!!")
   return .F.
else
   if type <> "К" .AND. type <> "КБ" .AND. name == "КОЛЛЕКТИВ"
      messageB( "Неверное значение ФАМИЛИИ для данного типа телефона !!!")
      return .F.
   endif
endif

return .T.


funcDef checkFlat with g:O, defValue:C local rV
if empty( g:varGet)
   rV := noYes( "Вы уверены, что номер КВАРТИРЫ отсутсвует ?")
   if !rV then return .F.
   g:varPut( defValue)
   g:display()
endif
return .T.

funcDef checkBlock with g:O, defValue:C local rV
if empty( g:varGet)
   rV := noYes( "Вы уверены, что номер КОРПУСА отсутствует ?")
   if !rV then return .F.
   g:varPut( defValue)
   g:display()
endif
return .T.

funcDef checkHouse with g:O local str, i, ch
if empty( g:varGet)
   messageB( "Вы должны ОБЯЗАТЕЛЬНО ввести номер дома !!!")
   return .F.
endif
str := alltrim( g:varGet)
for i := 1 to len( str)
  ch := subStr( str, i, 1)
  if !( isCir( ch) .OR. ch == "/")
     messageB( msgS2)
     return .F.
  endif
next

return .T.

funcDef checkStreet with g:O

if empty( g:varGet)
   messageB( "Вы должны ОБЯЗАТЕЛЬНО ввести название улицы !!!")
   return .F.
endif
if !isCir( g:varGet)
   messageB( msgS2)
   return .F.
endif
return .T.

static funcDef cirillic with Char:C local rV, nChar
nChar := Asc( Char)
rV    := ( nChar >= 128 .AND. nChar <= 175 ) .OR. ;
         ( nChar >= 224 .AND. nChar <= 241 ) .OR. ;
         ( Char  >= "0" .AND. Char  <= "9" ) .OR. ;
           Char  == "-" .OR.   Char == "."   .OR. ;
           Char  == " "
return rV

funcDef isCir with str:C local rV := .T., i, len
  len := len( str)
  for i := 1 to len( str)
     rV := rV .AND. cirillic( subStr( str, i, 1))
     exit if !rV
  next
return rV


static funcDef checkName with g:O local rV, aStr, i, tmpName, pos

tmpName := allTrim( g:varGet)

if upper( tmpName) == "КОЛЛЕКТИВ" then return .T.
aStr := {}
pos  := at( " ", tmpName)
if pos > 0
   aAdd( aStr, left( tmpName, pos - 1))
   tmpName := subStr(tmpName, pos + 1)
endif
pos := at( ".", tmpName)
if pos > 0
   aAdd( aStr, left( tmpName, pos - 1))
   tmpName := subStr(tmpName, pos + 1)
endif
pos := at( ".", tmpName)
if pos > 0
   aAdd( aStr, left( tmpName, pos - 1))
   tmpName := subStr(tmpName, pos + 1)
endif

if len( aStr) <> 3
   messageB( msgS1)
   return .F.
endif

for i := 1 to 3
  if !isCir( aStr[ i])
     messageB( msgS2)
     return .F.
  endif
next

return .T.

static funcDef checkPhone with g:O local rV := .T., phoneNumb, recNo

phoneNumb := allTrim( g:varGet)
if empty( phoneNumb)
   messageB( "Вы должны ОБЯЗАТЕЛЬНО ввести номер телефона !!!")
   return .F.
endif

if len( phoneNumb) < 7
   messageB( "Номер телефона не может быть МЕНЬШЕ 7 символов !!!")
   return .F.
endif

CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 2))
recNo := CONTRACT ->( recNo())
CONTRACT ->( dbSeek( phoneNumb))
phoneNumb := padR( phoneNumb, len( CONTRACT ->docNumb))
while CONTRACT ->docNumb == phoneNumb .AND. CONTRACT ->( !eof())
   if CONTRACT ->( recNo()) <> recNo .AND. ;
      CONTRACT ->docNumb == phoneNumb .AND. CONTRACT ->status = 1
      messageB( "Номер " + phoneNumb + " уже оплачивается со счета " + ;
               CONTRACT ->debetAcc + " !!!")
      CONTRACT ->( baseRestPos())
      return .F.
   endif
   skip 1 alias CONTRACT
enddo
CONTRACT ->( baseRestPos())

return .T.

funcDef phonePrint with newContract:L ;
  local provName, provCode, ptrProv, aStr, tmpStr ,;
        ptrPhone, aPhCode, aPhName, hh, ss, bb, ff,;
        prol1, prol2, pct, pCode, tmpName

if newContract
   fillProvList( "02", @provName, @provCode)
   tmpStr  := CONTRACT ->mBuffer
   ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))
   aStr    := CtoA( provName[ ptrProv], " ")
   pCode   := getKeyWord( tmpStr, "ОПЛАТА", "01")

   ptrPhone  := getKeyWord( tmpStr, "ТИП",   "ЛК" )
   ss        := getKeyWord( tmpStr, "УЛ.",   ""   )
   hh        := getKeyWord( tmpStr, "ДОМ",   ""   )
   bb        := getKeyWord( tmpStr, "КОРП.", ""   )
   ff        := getKeyWord( tmpStr, "КВ.",   ""   )
   tmpName   := getKeyWord( tmpStr, "ФИО",   cliFio())
   pct       := getKeyWord( tmpStr, "ЛЬГОТЫ", "00")
   assGetInfo( "02", @aPhCode, @aPhName)
   if len( aPhCode) < 1
      messageB( "Отсутсвуют виды телефонов !!!")
      return .F.
   endif
   ptrPhone := max( 1, aTrimmedScan( aPhCode, ptrPhone))
   prol1    := prol2 := ""
   if subStr( CONTRACT ->buffer, 3, 1) == "Y"
      prol1 := "8. НАСТОЯЩЕЕ ПОРУЧЕНИЕ МОЖЕТ БЫТЬ ПРОЛОНГИРОВАНО НА ТАКОЙ ЖЕ ПЕРИОД,"
      prol2 := "   ЕСЛИ МНОЙ НЕ БУДЕТ ПИСЬМЕННО ЗАЯВЛЕНО О ЕГО ПРЕКРАЩЕНИИ."
   endif

   servDummy( {{ "@docNumb",  CONTRACT ->intDoc    },;
               { "@debSchet", CONTRACT ->debetAcc  },;
               { "@uzel",     aStr[ 1]             },;
               { "@assName",  aPhName[ ptrPhone]   },;
               { "@d1",       CONTRACT ->dateBeg   },;
               { "@d2",       CONTRACT ->dateEnd   },;
               { "@tel_",     CONTRACT ->docNumb   },;
               { "@pCode",    pCode                },;
               { "@ss",       ss                   },;
               { "@hh",       hh                   },;
               { "@bb",       bb                   },;
               { "@ff",       ff                   },;
               { "@shortName",tmpName              },;
               { "@prol1",    prol1                },;
               { "@prol2",    prol2                },;
               { "@pct",      pct                  }})
   printOpen( "tel")
   servDummy( {})
else
   contInitServ()
   printOpen( "te3")
   contCloseServ()
endif
return NIL

funcDef phoneFirstPaym ;
   local rV, prol, tmpName, tmpStreet, tmpHouse, tmpBlock, tmpFlat,;
         tmpPct, ptrPhone, pSubType, ptrAB, tmpStr

if rV := WISC ->( mAppend())

   tmpStr    := CONTRACT ->mBuffer
   tmpName   := getKeyWord( tmpStr, "ФИО",   cliFio())
   tmpStreet := getKeyWord( tmpStr, "УЛ.",   CLIENTS ->Street)
   tmpHouse  := getKeyWord( tmpStr, "ДОМ",   CLIENTS ->house)
   tmpBlock  := getKeyWord( tmpStr, "КОРП.", CLIENTS ->Block)
   tmpFlat   := getKeyWord( tmpStr, "КВ.",   CLIENTS ->Flat)
   tmpPct    := getKeyWord( tmpStr, "ЛЬГОТЫ", "00")
   ptrPhone  := getKeyWord( tmpStr, "ТИП",   "ЛК")
   pSubType  := getKeyWord( tmpStr, "ВИД",   " ")
   ptrAB     := getKeyWord( tmpStr, "ОПЛАТА", "01")

   prol := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", "1", "0")
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := "985"
   WISC ->buffer := CONTRACT ->intDoc          + ";" + ;
                    CONTRACT ->debetAcc        + ";" + ;
                    DtoC( CONTRACT ->dateBeg)  + ";" + ;
                    DtoC( CONTRACT ->dateEnd)  + ";" + ;
                    CONTRACT ->code2           + ";" + ;
                    CONTRACT ->code3           + ";" + ;
                    prol                       + ";" + ;
                    CONTRACT ->docNumb         + ";" + ;
                    tmpName                    + ";" + ;
                    tmpStreet                  + ";" + ;
                    tmpHouse                   + ";" + ;
                    tmpBlock                   + ";" + ;
                    tmpFlat                    + ";" + ;
                    ptrPhone                   + ";" + ;
                    pSubType                   + ";" + ;
                    ptrAB                      + ";" + ;
                    tmpPct
   WISC ->( netUnLock())
endif
return rV

funcDef phoneLastPaym local rV
if rV := WISC ->( mAppend())
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := "984"
   WISC ->buffer := CONTRACT ->intDoc          + ";" + ;
                    CONTRACT ->debetAcc        + ";" + ;
                    CONTRACT ->code2           + ";" + ;
                    CONTRACT ->code3           + ";" + ;
                    CONTRACT ->docNumb         + ";"
   WISC ->( netUnLock())
endif
return rV

funcDef phoneClose local rV, arr, nextD
   if rV := phoneLastPaym()
      contInitServ()
      arr   := servDummy()
      nextD := addMonth( firstDay( setCurDate()), 1)
      //aAdd( arr, { "@stopdate", rMonth( nextD) + " " + sYear( nextD) + " г."})
      aAdd( arr, { "@stopdate", nextD})
      aAdd( arr, { "@typewisc", 'на оплату услуг АО "Петербургская Телефонная Сеть"'})
      // printOpen( "te2")
      printOpen( "serst")
      contCloseServ()
   endif
return rV

* ---------------------- перевод с текущего счета на накопительный -----------
funcDef transEdit ;
   local rV, gets, aSource, aSource2, nextMonth, firstDate,;
         month1, year1, month2, year2, aDest, aDest2      ,;
         ptrSource, ptrDest, provName, provCode, ptrProv  ,;
         tmpSum, ptrKind, aKind, ptrProl, yesNo, d1, d2

yesNo   := { "Есть", "Нет"}
ptrProl := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", 1, 2)

accPrizList( "15 1", @aSource,   @aSource2)
ptrSource := max( 1, aScan( aSource2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))
if len( aSource) < 1
   messageB( "У клиента нет открытых текущих счетов !!!")
   return .F.
endif

accPrizList( "15 2  5  0", @aDest,   @aDest2)
accPrizList( "15 2  42 0", @aDest,   @aDest2,, .F.)

// Add by TMV 01-12-2003
accPrizList( "15 2  1050", @aDest,   @aDest2,, .F.)
accPrizList( "15 2  1060", @aDest,   @aDest2,, .F.)
accPrizList( "15 2  1070", @aDest,   @aDest2,, .F.)
accPrizList( "15 2  1120", @aDest,   @aDest2,, .F.)
accPrizList( "15 2  1130", @aDest,   @aDest2,, .F.)
accPrizList( "15 2  1140", @aDest,   @aDest2,, .F.)
// Add by TMV 01-12-2003 -- End

accPrizList( "15 2  125", @aDest,   @aDest2,, .F.) // Add by TMV 30-03-2004

ptrDest := max( 1, aScan( aDest2, {|x| x[ 2] == CONTRACT ->creditAcc}, 1))
if len( aDest) < 1
   messageB( "У клиента нет открытых накопительных счетов !!!")
   return .F.
endif

if empty( CONTRACT ->dateBeg)
   CONTRACT ->dateBeg := setCurDate() + 1
   CONTRACT ->dateEnd := addMonth( CONTRACT ->dateBeg, 1)
endif

tmpSum  := CONTRACT ->summa/ 100
aKind   := { "Всего остатка", "Определенной суммы"}
ptrKind := if( tmpSum > 0, 2, 1)

if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc := confTransDoc()
endif

gets := {{ "Перечисление средств на накопительный счет"              },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"              },;
         { "Номер        :", block( CONTRACT ->intDoc),,{||.F.}      },;
         { "Перечисление :", block( ptrKind), aKind                  },;
         { "*" + chr( 25)                                            },;
         { "Начало       :", block( CONTRACT ->dateBeg),,,{|g| .T./*checkD1( g)*/   }},;
         { "На счет      :", block( ptrDest), aDest                  },;
         { "*" + chr( 25)                                            },;
         { "Сумма        :", block( tmpSum), "@K9 999 999 999"        ,;
            {|g| checkType( aKind)}, {|g| checkTransSum( g)      }},;
         { "Со счета     :", block( ptrSource), aSource             }}
         // { "Пролонгация  :", block( ptrProl), yesNo                 }}
ptrProl := 2
if rV := getUp( 5, 5, gets)
   CONTRACT ->dateEnd   := addMonth( CONTRACT ->dateBeg, 4)
   CONTRACT ->docNumb   := CONTRACT ->intDoc
   CONTRACT ->debetAcc  := aSource2[ ptrSource, 2]
   CONTRACT ->creditAcc := aDest2[    ptrDest,   2]
   CONTRACT ->code3     := "71" // provCode[ 1]
   CONTRACT ->summa     := -1
   if ptrKind == 2 then CONTRACT ->summa := tmpSum * 100
   if empty( CONTRACT ->dateCur) then CONTRACT ->dateCur  := setCurDate()
   CONTRACT ->buffer   := left( CONTRACT ->buffer, 2) + ;
      if( ptrProl <= 1, "Y", "N") + subStr( CONTRACT ->buffer, 4)
   transFirstPaym()
endif

return rV


funcDef checkType with aKind:A local ptr
   ptr := aTrimmedScan( aKind, M ->getList[ 2]:varGet)
return ptr == 2

funcDef checkTransSum with g:O local rV, lastKey, addMin, addMax
   lastKey := lastKey()
   if lastKey == K_UP .OR. lastKey == K_SH_TAB then return .T.
   if g:varGet < 0
      messageB( "Сумма взноса не может быть отрицательной !!!")
      return .F.
   endif
   vidGetAdd( "15 2  5  0", @addMin, @addMax)
   if !( g:varGet >= addMin/ 100 .AND. g:varGet <= addMax/ 100)
      errLog( { "Вы можете ввести сумму в пределах ",;
               "от " + lTrim( mStr( addMin/ 100)) + ;
               "до " + lTrim( mStr( addMax/ 100))  ,;
               "а Вы ввели " + lTrim( mStr( g:varGet)) })
      return .F.
   endif
return .T.

funcDef transPrint with newContract:L local name, debSchet, credSchet

if newContract
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))
   ACC ->( dbSeek( CONTRACT ->creditAcc))
   name := '"' + rTrim( vidGetName( ACC ->PRIZ)) +'"'
   ACC ->( baseRestPos())
   if confUseAgent()
     debSchet  := CONTRACT ->debetAcc
     credSchet := CONTRACT ->creditAcc
   else
     debSchet  := transSplitSchet( CONTRACT ->debetAcc) 
     credSchet := transSplitSchet( CONTRACT ->creditAcc)
   endif
   servDummy( {{ "@otdnumb",      confOtdNumb()                         },;
               { "@debSchet",     debSchet                              },;
               { "@credSchet",    credSchet                             },;
               { "@upperVidName", upper( name)                          },;
               { "@vidName",      name                                  },;
               { "@d1",           CONTRACT ->dateBeg                    },;
               { "@rSumma",       getS( CONTRACT ->summa/ 100)          },;
               { "@sum",          lTrim( mStr( CONTRACT ->summa/ 100))+" рублей"}})
   if CONTRACT ->summa < 0
      printOpen( "tr_1")
   else
      printOpen( "tr_2")
   endif
   servDummy( {})
else
   contInitServ()
   printOpen( "tr2")
   contCloseServ()
endif
return NIL

static funcDef transSplitSchet with S:C local rV
  rV := left( S, 5)      + " " + SubStr( S, 6, 3) + " " + ;
        SubStr( S, 9, 5) + " " + SubStr( S,14, 7)
return rV

funcDef transFirstPaym local rV, prol, sum
if rV := WISC ->( mAppend())
   prol := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", "1", "0")
   sum  := CONTRACT ->summa
   sum  := if( sum >0, sum/ 100, sum)
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := "983"
   WISC ->buffer := CONTRACT ->intDoc         + ";" + ;
                    CONTRACT ->debetAcc       + ";" + ;
                    DtoC( CONTRACT ->dateBeg) + ";" + ;
                    DtoC( CONTRACT ->dateEnd) + ";" + ;
                    CONTRACT ->code2          + ";" + ;
                    CONTRACT ->code3          + ";" + ;
                    prol                      + ";" + ;
                    CONTRACT ->creditAcc      + ";" + ;
                    nStr( sum)+ ";"
   WISC ->( netUnLock())
endif
return rV

funcDef transLastPaym local rV
if rV := WISC ->( mAppend())
   WISC ->schet := CONTRACT ->debetAcc
   WISC ->type   := "982"
   WISC ->buffer := CONTRACT ->intDoc         + ";" + ;
                    CONTRACT ->debetAcc       + ";" + ;
                    CONTRACT ->code2          + ";" + ;
                    CONTRACT ->code3          + ";" + ;
                    CONTRACT ->creditAcc      + ";"
   WISC ->( netUnLock())
endif
return rV


funcDef transClose local rV
if rV := transLastPaym()
   contInitServ()
   aAdd( servDummy(), { "@d2", setCurDate() + 1})
   aAdd( servDummy(), { "@debSchet",     transSplitSchet(CONTRACT ->debetAcc) })
   aAdd( servDummy(), { "@credSchet",    transSplitSchet(CONTRACT ->creditAcc)})
   printOpen( "tr3")
   contCloseServ()
endif
return rV

// Add by TMV  28-10-2002
// Для перевода с текущего счета на текущий счет в другом отделении
//
funcDef transAccLastPaym local rV
if rV := WISC ->( mAppend())
   WISC ->schet := CONTRACT ->creditAcc
   WISC ->type   := "948"
   WISC ->buffer := CONTRACT ->intDoc         + ";" + ;
                    CONTRACT ->debetAcc       + ";" + ;
                    CONTRACT ->code2          + ";" + ;
                    CONTRACT ->code3          + ";" + ;
                    CONTRACT ->creditAcc      + ";"
   WISC ->( netUnLock())
endif
return rV


// Add by TMV  28-10-2002
// Для перевода с текущего счета на текущий счет в другом отделении
//
funcDef transAccClose local rV
if rV := transAccLastPaym()
   contInitServ()
   aAdd( servDummy(), { "@d2", setCurDate() + 1})
   aAdd( servDummy(), { "@debSchet",     transSplitSchet(CONTRACT ->debetAcc) })
   aAdd( servDummy(), { "@credSchet",    transSplitSchet(CONTRACT ->creditAcc)})
   printOpen( "tr3")
   contCloseServ()
endif
return rV


* ------------------ перечисление с текущего на карточный
funcDef cardEdit ;
   local rV := .F., gets, aSource, aSource2,  aDest, aDest2      ,;
         ptrSource, ptrDest, provName, provCode, ptrProv  ,;
         tmpSum, ptrKind, aKind, ptrProl, yesNo, tmpPct ,;
			dDateSchOpen := CtoD(""), tmpDate := setCurDate() + 1  // Add by TMV  18-05-2002

accPrizList( "15 1", @aSource,   @aSource2)
ptrSource := max( 1, aScan( aSource2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))
//if len( aSource) < 1
//   messageB( "У клиента нет открытых текущих счетов !!!")
//   return .F.
//endif

accPrizList( "15 4", @aDest,   @aDest2)
ptrDest := max( 1, aScan( aDest2, {|x| x[ 2] == CONTRACT ->creditAcc}, 1))
if len( aDest) < 1
   messageB( "У клиента нет открытых карточных счетов !!!")
   return .F.
endif

if empty( CONTRACT ->dateBeg)
// Replaced by TMV  18-05-2002
   dDateSchOpen = fSchOpenDate(aDest2[ptrDest, 2])
   if dDateSchOpen <= tmpDate
      CONTRACT ->dateBeg := tmpDate
   else
      CONTRACT ->dateBeg := dDateSchOpen
   endif
   CONTRACT ->dateEnd := addMonth( CONTRACT ->dateBeg, 24)
endif

tmpSum := tmpPct := 0.00
if CONTRACT ->summa< 0
   tmpPct := - CONTRACT ->summa
else
   tmpSum := CONTRACT ->summa/ 100
endif

if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc := confTransDoc()
endif

gets := {{ "Перечисление средств на карточный счет"                  },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                  },;
         { "Номер        :", block( CONTRACT ->intDoc),,{||.F.}      },;
         { "*" + chr( 25)                                            },;
         { "На счет      :", block( ptrDest), aDest                  },;  // Moved by TMV  18-05-2002
         { "*" + chr( 25)                                            },;
         { "Начало       :", block( CONTRACT ->dateBeg),,,{|g| checkDBeg( g, aDest, aDest2, 2)} },;  // Edit by TMV  18-05-2002
         { "Окончание    :", block( CONTRACT ->dateEnd),,,{|g| checkD2( g, 3)}},; // Edit by TMV  18-05-2002
         { "Фиксир.сумма :", block( tmpSum),,, {|g| chkCrdSum( g, 6)}},;
         { "Проценты     :", block( tmpPct),,, {|g| chkCrdPct( g, 5)}},;
         { "Со счета     :", block( ptrSource), aSource             }}

if len( aSource) < 1 .OR. ( rV := getUp( 5, 5, gets))
   CONTRACT ->docNumb   := CONTRACT ->intDoc
   if len( aSource) > 0
      CONTRACT ->debetAcc  := aSource2[ ptrSource, 2]
   endif
   CONTRACT ->creditAcc := aDest2[ ptrDest, 2]
   CONTRACT ->code3     := "71"
   if empty( CONTRACT ->dateCur) then CONTRACT ->dateCur  := setCurDate()
   if tmpPct > 0
      CONTRACT ->summa := - tmpPct
   else
      CONTRACT ->summa := tmpSum * 100
   endif
   if len( aSource) > 0
      transFirstPaym()
   endif
endif

return rV

funcDef chkCrdSum with g:O, ptr:N local tmpPct
   if g:varGet < 0
      messageB( "Сумма не может быть отрицательной !!!")
      return .F.
   endif
   if g:varGet > 0
      tmpPct := M ->getList[ ptr]:varGet
      if tmpPct > 0
         messageB( "Нельзя указать одновременно и сумму и проценты !!!")
         return .F.
      endif
   endif
return .T.

funcDef chkCrdPct with g:O, ptr:N local tmpSum
   if g:varGet < 0
      messageB( "Проценты не могут быть отрицательными !!!")
      return .F.
   endif
   if g:varGet >100
      messageB( "Проценты не могут быть > 100 !!!")
      return .F.
   endif
   if g:varGet > 0
      tmpSum := M ->getList[ ptr]:varGet
      if tmpSum > 0
         messageB( "Нельзя указать одновременно и сумму и проценты !!!")
         return .F.
      endif
   endif
return .T.

funcDef cardPrint with newContract:L local tmpPct, tmpSum, str
tmpPct := tmpSum := 0
if CONTRACT ->summa< 0
   tmpPct := - CONTRACT ->summa
   tmpSum := ""
   str    := "в размере " + nStr( tmpPct) + " % от каждого поступления на текущий счет."
else
   tmpSum := CONTRACT ->summa/ 100
   tmpSum := getS( tmpSum)
   str    := { "один раз в календарный месяц сумму в размере", tmpSum + " ." }
endif
servDummy( {;
   { "@filial",    confFilial() },;
   { "@otd",       confOtdNumb()},;
   { "@filialotd", rTrim( confFilial()) + " " + confOtdNumb()},;
   { "@prc",       if( tmpPct <> 0, nStr( tmpPct), "___")    },;
   { "@sum",       tmpSum              },;
   { "@schet711",  CONTRACT ->debetAcc },;
   { "@schet718",  CONTRACT ->creditAcc},;
   { "@startdate", DtoC( CONTRACT ->dateBeg)},;
   { "@carddopattr", str               } })

printOpen( "CRD1")
return NIL

funcDef chkCredLine with cardSchet:C ;
   local rV := .T., clCode := CLIENTS ->code
   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 1))
   if CONTRACT ->( dbSeek( clCode + "14"))
     while CONTRACT ->code == clCode .AND. CONTRACT ->code2 == "14" .AND. ;
       CONTRACT ->( !eof())
       if subStr( CONTRACT ->mBuffer, 22, 20) == cardSchet .AND. ;
          CONTRACT ->status == 1
          rV := .F.
          errLog( ;
 { "У клиента действующий договор на предоставление лимита кредитования." ,;
          "Перед закрытием счета Вам необходимо закрыть этот договор." })
       endif
       skip 1 alias CONTRACT
     enddo
   endif
   CONTRACT ->( baseRestPos())   
return rV


funcDef crClose local rV := .T.
   // if chkCredLine() then 
   if( rV := transLastPaym())
     contInitServ()
     aAdd( servDummy(), { "@d2", setCurDate() + 1})
     aAdd( servDummy(), { "@debSchet",     transSplitSchet(CONTRACT ->debetAcc) })
     aAdd( servDummy(), { "@credSchet",    transSplitSchet(CONTRACT ->creditAcc)})
     printOpen( "tr3")
     contCloseServ()
   endif
return rV



* --------------- обработка карт ------------------------------------
funcDef aCrStatus local arr
   arr := { "Не определено             ",;
            "Удалена                   ",;
            "В стоп-листе              ",;
            "Активна                   ",;
            "Принята к изготовлению    ",;
            "Блокировка карты          ",;
            "Истек срок действия карты ",;
            "Карта не действует        ",;
            "Карта изготовлена не будет",;
            "Карта заказана"}
return arr

funcDef aCrStat2 local arr
   arr  := { " ", "D", "S", "A", "O", "B", "E", "F", "U", "R"}
return arr

funcDef aCrWhat local arr
  arr := { "Не определено                       " ,;
           "В первый раз                        " ,;
           "Не в первый раз                     " ,;
           "Внеочередное изг. по вине клиента   " ,;
           "Внеочередное изг. не по вине клиента"  }
return arr

funcDef acrWhat2 local arr
   arr := { " ", "1", "2", "3", "4"}
return arr

funcDef hasActiveCards with schet:C local rV := .F., arr, ptr
  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 4))
  CONTRACT ->( dbSeek( schet))
  while CONTRACT ->creditAcc == schet .AND. CONTRACT ->( !eof())
    if CONTRACT ->code2 == "09"
       arr := CtoA( CONTRACT ->mBuffer, ";")
       for ptr := 1 to len( arr)
         arr[ ptr] := CtoA( arr[ ptr], ",")
         if arr[ ptr, 3] $ "A,O" .AND. crGetExpDate( arr[ ptr]) >= setCurDate()
           rV := .T.
           exit
         end if
       next
    endif
    skip 1 alias CONTRACT
  enddo
  CONTRACT ->( baseRestPos())
return rV

funcDef cardBrowse ;
  local aC, aH, fk, arr, i, ptr, arrCode, arrName

   if CONTRACT ->code2 <> "09"
      messageB( "Этот режим предназначен только для перечисления на карты !!!")
      return .F.
   endif

   assGetInfo( "10", @arrCode, @arrName,,, .F.)
   if len( arrCode) < 1
      messageB( "Нет типов карт !!!")
      return .F.
   endif
   arr :=  ctoA( CONTRACT ->mBuffer, ";")
   if len( arr) > 0
      if empty( arr[ len( arr)])
         aSize( arr, len( arr) - 1)
      endif
   endif
   if len( arr) < 1
      messageB( "Нет оформленных карт !!!")
      return .F.
   endif

   aH  := {{ "Оформленные карты для " + cliFio()     },;
           { "Тип карты", {|| crViewType( arrName, arr[ ptr])},, {|| .F.}},;
           { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"}}

   aC  := {{ "Основная",     {, {|| crGetBase(    arr[ ptr]) },{ "*", " "}, {|| .F.}}},;
           { "Номер карты",  {, {|| crGetPAN( arr[ ptr])     },"@R 9999 9999 9999 9999", {|| .F.}}},;
           { "Действует до", {|| crGetExpDate( arr[ ptr])    },, {|| .F.}},;
           { "Статус",       {, {|| crGetStatus(  arr[ ptr]) }, aCrStatus(), {|| .F.}}},;
           { "Клиент",       {|| crGetClient(  arr[ ptr])    },, {|| .F.}}}
   fk  :=  {,{}}
   for i := 1 to len( arr)
       arr[ i]    := ctoA( arr[ i], ",")
       arr[ i, 1] := aTrimmedScan( arrCode, arr[ i, 1])
       arr[ i, 3] := aTrimmedScan( aCrStat2(), arr[ i, 3])
   next
   ptr := 1
   browseUp( 5, 5, aH, aC,;
     {|| crSkipUp( arr, @ptr)}, {|| crSkipDown( arr, @ptr)},;
     {|| crGoTop(  arr, @ptr)}, {|| crGoBtm(    arr, @ptr)},,,,, fk)
return NIL

funcDef crGoTop with arr:A, ptr:N
  ptr := 1
return NIL

funcDef crGoBtm with arr:A, ptr:N
   ptr := len( arr)
return NIL

funcDef crSkipUp with arr:A, ptr:N local rV := .F.
   if ptr > 1
      ptr--
      rV := .T.
   endif
return rV

funcDef crSkipDown with arr:A, ptr:N local rV := .F.
   if ptr < len( arr)
      ptr++
      rV := .T.
   endif
return rV

funcDef crViewType with arrName:A, arr:A local rV, ptr
  ptr := arr[ 1]
  if ptr > 0
     rV := arrName[ ptr]
  else
     if len( arrName) = 0
        rV := Space( 20)
     else
        rV := Space( len( arrName[ 1]))
     endif
  endif
return rV

funcDef crGetType with arr:A, newValue:NU ;
   local ptr, oldValue
   oldValue := arr[ 1]
   if !isNIL( newValue)
      arr[ 1] := newValue
   endif
return max( 1, oldValue)

funcDef crGetBase with arr:A, newValue:NU ;
   local oldValue := val( arr[ 8])
   if !isNIL( newValue)
      arr[ 8] := nStr( newValue)
   endif
return max( 1, oldValue)

funcDef crGetExpDate with arr:A, newValue:DU ;
  local oldValue := CtoD( arr[ 2])
  if !isNIL( newValue)
     arr[ 2] := DtoC( newValue)
  endif
return oldValue

funcDef crGetClient with arr:A, newValue:CU ;
   local oldValue

   CLIENTS ->( baseSavePos())
   CLIENTS ->( dbSetOrder( 1))
   CLIENTS ->( dbSeek( arr[ 4]))
   oldValue := cliFio()
   CLIENTS ->( baseRestPos())
   if !isNIL( newValue)
      arr[ 4] := newValue
   endif
return oldValue

funcDef crGetLatName with arr:A, newValue:CU ;
  local oldValue := CtoD( arr[ 5])
  if !isNIL( newValue)
     arr[ 5] := newValue
  endif
return oldValue

funcDef crGetLatSName with arr:A, newValue:CU ;
  local oldValue := CtoD( arr[ 6])
  if !isNIL( newValue)
     arr[ 6] := newValue
  endif
return oldValue

funcDef crGetStatus with arr:A, newValue:NU ;
   local oldValue := arr[ 3]
   if !isNIL( newValue)
      arr[ 3] := newValue
   endif
return max( 1, oldValue)

funcDef crGetWhatCard with arr:A, newValue:NU ;
   local oldValue := val( arr[ 10])
   if !isNIL( newValue)
      arr[ 3] := nStr( newValue)
   endif
return oldValue

funcDef crGetPan with arr:A, newValue:CU ;
   local oldValue
   if len( arr) < 11 then return ""
   oldValue := arr[ 11]
   if !isNIL( newValue)
      arr[ 11] := newValue
   endif
return oldValue

// Add by TMV 18-03-2004
funcDef crGetOWSpriz with arr:A, newValue:CU ;
   local oldValue
   if len( arr) < 12 then return "0"
   oldValue := arr[ 12]
   if !isNIL( newValue)
      arr[ 12] := newValue
   endif
return oldValue
* ------------- упаковка даты ---------------------------------------
funcDef packDate with Day:N, Month:N, Year:N local rV
  rV := CtoD( Str( Day) + "." + Str( Month) + "." + Str( Year))
return rV

* ------------- поиск в массиве без учета регистра ------------------
funcDef aTrimmedScan with arr:A, value:C local ptr
  ptr := aScan( arr, {|x| allTrim( upper( x)) == allTrim( upper( value))}, 1)
return ptr


* ------------ поиск ключевого слова в CONTRACT ---------------------
funcDef getKeyWord with strScan:CM, keyWord:C, defValue:C ;
   local rV := "", pos, ch

if ( pos := at( allTrim( keyWord) + ":", strScan)) > 0
   pos += len( allTrim( keyWord)) + 1
   while pos <= len( strScan)
      ch := subStr( strScan, pos++, 1)
      exit if ch == ";"
      rV += ch
   enddo
endif
if empty( rV) then rV := defValue
return rV

funcDef setKeyWord with strScan:C, keyWord:C, value:C ;
   local pos, p1, oldValue, str := keyWord + ":"
if str $ strScan
   oldValue := getKeyWord( strScan, keyWord, "")
   strScan  := strTran( strScan, str + oldValue, str + value)
else
   strScan  := strScan + str + value + ";"
endif

return strScan


funcDef contInitServ local hasProl, prol1, prol2

   hasProl := subStr( CONTRACT ->buffer, 3, 1) == "Y"
   prol1   := if( hasProl, "НАСТОЯЩЕЕ ПОРУЧЕНИЕ МОЖЕТ БЫТЬ ПРОЛОНГИРОВАНО НА ТАКОЙ ЖЕ ПЕРИОД,", "")
   prol2   := if( hasProl, "ЕСЛИ МНОЙ НЕ БУДЕТ ПИСЬМЕННО ЗАЯВЛЕНО О ЕГО ПРЕКРАЩЕНИИ.", "")

   servDummy( {{ "@intDoc",      CONTRACT ->intDoc   },;
               { "@datedoc",     CONTRACT ->dateCur  },;
               { "@otd",         confOtdNumb()       },;
               { "@d1",          CONTRACT ->dateBeg  },;
               { "@d2",          CONTRACT ->dateEnd  },;
               { "@fulladdress", cliFullAddress()    },;
               { "@prol1",       prol1               },;
               { "@prol2",       prol2               },;
               { "@curdate",     setCurDate()        }})
   if CONTRACT ->code2 == "07";
			.Or. CONTRACT ->code2 == "54"  // Add by TMV  28-10-2002
      aAdd( servDummy(), { "@credSchet", transSplitSchet( CONTRACT ->creditAcc)})
      aAdd( servDummy(), { "@debSchet",  transSplitSchet( CONTRACT ->debetAcc )})
   else
      aAdd( servDummy(), { "@credSchet", CONTRACT ->creditAcc})
      aAdd( servDummy(), { "@debSchet",  CONTRACT ->debetAcc })
   endif

return NIL

funcDef contCloseServ
   servDummy( {})
return NIL

funcDef isCard with priz:C
return left( priz, 6) == "15 4  "

funcDef contSchetCheck with schet:C ;
  local rV := .T., cliCode, fName, arrPct := {}, j

  arrPct := brwGetRSchet( 3)

  if len( arrPct) > 0    // OVD
     ACC ->( baseSavePos())
     ACC ->( dbSetOrder( 1))
     if ACC ->( netSeek( schet))
       if isCard( ACC ->priz) .AND. !eof()
           for j = 1 to len( arrPct)
              if subStr( arrPct[ j, 1], 1, 5) =="45509" .AND. arrPct[ j, 8] <> 0
                 errLog( "Имеются отстатки по СКС 45509. Счет закрывать нельзя !")
                 return .F.
              endif
           next
        endif
     endif
  endif

  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 4))
  CONTRACT ->( dbSeek( schet))
  while CONTRACT ->creditAcc == schet .AND. CONTRACT ->( !eof())
    if CONTRACT ->status = 1
       CLIENTS ->( baseSavePos())
       CLIENTS ->( dbSetOrder( 1))
       CLIENTS ->( dbSeek( CONTRACT ->code))
       fName := cliFullName()
       cliCode := cliCode()
       CLIENTS ->( baseRestPos())
       rV := .F.
       errLog( { "У клиента " + fName,;
                "Есть действуещее поручение N " + CONTRACT ->intDoc,;
                "",;
                "Перед закрытием счета Вам нужно закрыть это поручение !!!"})
       exit
    endif
    skip 1 alias CONTRACT
  enddo

  CONTRACT ->( dbSetOrder( 5))
  CONTRACT ->( dbSeek( schet))
  while CONTRACT ->debetAcc == schet .AND. CONTRACT ->( !eof())
    if CONTRACT ->status = 1
       CLIENTS ->( baseSavePos())
       CLIENTS ->( dbSetOrder( 1))
       CLIENTS ->( dbSeek( CONTRACT ->code))
       fName := cliFullName()
       cliCode := cliCode()
       CLIENTS ->( baseRestPos())
       rV := .F.
       errLog( { "У клиента " + fName,;
                "Есть действуещее поручение N " + rTrim( CONTRACT ->intDoc) + ;
                "по счету ", "",;
                "Перед закрытием счета Вам нужно закрыть это поручение !!!"})
       exit
    endif
    skip 1 alias CONTRACT
  enddo
  CONTRACT ->( baseRestPos())
return rV


* ------------------ подписка в рассрочку
funcDef magEdit ;
  local rV := .T., gets, provName, provCode, ptrProv, aAssCodes, aAssNames,;
        ptrMag := 1, nextMonth, year1, year2, month1, month2, accSchet    ,;
        accSchet2, ptrSchet, ptrProl, yesNo := { "Есть", "Нет"}, summa, d1,;
        d2, canEdit, bCan, aAssBuf, arrMBuf


fillProvList( "11", @provName, @provCode)
do case
   case len( provName) < 1
        messageB( "По данной услуге нет поставщиков !!!")
        return .F.
   case len( provName) = 1
        ptrProv := 1
   case len( provName) > 1
     if empty( CONTRACT ->intDoc)
        ptrProv := popUp( 5, 5, provName)
        if ptrProv = 0 then return .F.
     else
        ptrProv := aTrimmedScan( provCode, CONTRACT ->code3)
        if ptrProv = 0 then return .F.       
     endif
endcase

assGetInfo( "11", @aassCodes, @aassNames, @aassBuf, provCode[ ptrProv])
if len( aassNames) < 1
   messageB( "Справочник магазинов пуст !!!")
   return .F.
endif
accPrizList( "15 1", @accSchet,   @accSchet2)
ptrSchet := max( 1, aScan( accSchet2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))
if len( accSchet) < 1
   messageB( "У клиента нет действующих пенсионных счетов !!!")
   return .F.
endif

nextMonth := addMonth( setCurDate(), 1)
if !empty( CONTRACT ->dateBeg) then nextMonth := CONTRACT ->dateBeg
year1  := Year(  nextMonth)
month1 := Month( nextMonth)
//
// если дата работы - ноябрь, то начало поручения - декабрь тек.года,
// а окончание поручения - тоже декабрь, а не январь след.года
//
month2 := if( month1 <= 6, 6, 12)
year2  := year1

if !empty( CONTRACT ->dateEnd)
  nextMonth := CONTRACT ->dateEnd
  year2  := Year(  nextMonth)
  month2 := Month( nextMonth)
endif

canEdit := empty( CONTRACT ->intDoc)
bCan    := {|| canEdit}
ptrProl := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", 1, 2)
if month2 == 6 .OR. month2 == 12 then ptrProl := 1

//ptrSchet:= 1
summa   := CONTRACT ->summa/ 100
if empty( CONTRACT ->intDoc)
  summa := val( aAssBuf[ 1])
endif
arrMBuf := CtoA( CONTRACT ->mBuffer, ";")
if len( arrMBuf) > 0
  ptrMag := aTrimmedScan( aAssCodes, arrMBuf[ 1])
endif

gets := {{ "Подписка в рассрочку"                                    },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                  },;
         { "Поставщик    : " + ProvName[ ptrProv]                    },;
         { "Издание      : ", block( ptrMag), aAssNames, bCan,;
              {|g| _chkNewSum( g, aAssNames, aAssBuf)}},;
         { "Начало       : ", block(month1),aMonths(), bCan },;
         { "*" + chr( 25)                                            },;
         { "*",             block( year1), "9999", bCan, {|g|chkMagBeg( g)} },;
         { "*г."                                                     },;
         { "Окончание    : ", block( month2), aMonths() },;
         { "*" + chr( 25)                                            },;
         { "*",             block( year2), "9999",,{|g| chkMagEnd( g)}            },;
         { "*г."                                          },;
         { "Сумма        : ", block( summa),"999999999.99", {||.F.}  },;
         { "Дебет.счет   : ", block( ptrSchet), accSchet, bCan       },;
         { "*" + chr( 25)                                            },;
         { "Пролонгация  : ", block( ptrProl), yesNo,,{|g| chkProl(g)}}  }

if rV .AND. ( rV := getUp( 5, 5, gets))
  //
  // даже разговаривать с козлами не буду
  //
  d1 := packDate( 1, month1, year1)
  d2 := lastDay( packDate( 1, month2, year2))
  if empty( CONTRACT ->dateBeg)
    if d1 < firstDay( addMonth( setCurDate(), 1)) then return .F.
  endif
  if d1 > d2 then return .F.
  if month2 <> 6 .AND. month2 <> 12 .AND. ptrProl == 1 then return .F.
  if month1 <= 6 .AND. month2 > 6 then return .F.
  if d2 < setCurDate() .AND. ptrProl = 2 then return .F.
  if diffInMonths( d2, d1) > 6 then return .F.

  //
  // заполняем contract
  //
  CONTRACT ->code     := cliCode()
  CONTRACT ->code3    := provCode[  ptrProv ]
  CONTRACT ->debetAcc := accSchet2[ ptrSchet, 2]
  CONTRACT ->dateBeg  := packDate( 1, month1, year1)
  CONTRACT ->dateEnd  := lastDay( packDate( 1, month2, year2))
  CONTRACT ->summa    := summa * 100
  CONTRACT ->mBuffer  := aAssCodes[ ptrMag] + ";"
  CONTRACT ->buffer   := left( CONTRACT ->buffer, 2) + ;
  if( ptrProl <= 1, "Y", "N") + subStr( CONTRACT ->buffer, 4)
  if empty( CONTRACT ->docNumb)
    CONTRACT ->dateCur := setCurDate()
    CONTRACT ->docNumb := confMagDoc()
    CONTRACT ->intDoc  := CONTRACT ->docNumb
  endif
  magFirstPaym()
endif
return rV

static funcDef _chkNewSum with g:O, arrName:A, arrSums:A local rV := .T., ptr
  ptr := aTrimmedScan( arrName, g:varGet)
  if ptr > 0
    M ->getList[ 6]: varPut( val( arrSums[ ptr]))
    M ->getList[ 6]: display()
  endif
return rV


funcDef magClose local rV := .F., endDate, d1, d2, gets, ar1, month2, year2

if WISC ->( dbSeek( CONTRACT ->debetAcc + "935 "))
  while WISC ->schet == CONTRACT ->debetAcc .AND. WISC ->type == "935 " .AND.;
        WISC ->( !eof())
    ar1 := CtoA( WISC ->buffer, ";")
    if len( ar1) > 0 .AND. allTrim( ar1[ 1]) == allTrim( CONTRACT ->intDoc)
      messageB( "Поручение на закрытие уже сформировано !!!")
      return .F.
    endif
    skip 1 alias WISC
  enddo
endif

d1 := CONTRACT ->dateBeg
d2 := CONTRACT ->dateEnd
endDate := max( d1, firstDay( addMonth( setCurDate(), 1)))
month2  := Month( endDate)
year2   := Year(  endDate)
gets := {{ "Закрытие поручения"  },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
         { "Закрыть с ", block( month2), aMonths(),,{|g| chkMagBeg2( g, endDate)} },;
         { "*" + chr( 25)                                               },;
         { "*",          block( year2), "9999",{||.F.}                  }}

if getUp( 5, 5, gets)
  endDate := packDate( 1, month2, year2)
  servDummy( {{ "@filial",   confFilial()     },;
              { "@otdNumb",  confOtdNumb()    },;
              { "@intDoc",   CONTRACT ->intDoc},;
              { "@dateBeg",  DtoC( d1)        },;
              { "@stopdate", DtoC( endDate)   }})
   //printOpen( "mg2")
   printOpen( "serst")
   servDummy( {})
   magLastPaym( endDate)
endif
return .F.


funcDef magFirstPaym local arrMBuf, prol, rV := .T.
  prol := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", "1", "0")
  arrMBuf := CtoA( CONTRACT ->mBuffer, ";")
  if len( arrMBuf) = 0
    messageB( "Нет индекса издания !!!")
    return .F.
  endif
  if WISC ->( rV := mAppend())
    WISC ->schet  := CONTRACT ->debetAcc
    WISC ->type   := "945 "
    WISC ->buffer := CONTRACT ->intDoc + ";" + CONTRACT ->debetAcc + ";" + ;
        DtoC( CONTRACT ->dateBeg) + ";" + DtoC( CONTRACT ->dateEnd)+ ";" + ;
        CONTRACT ->code2 + ";" + CONTRACT ->code3 + ";" + prol     + ";" + ;
        arrMBuf[ 1] + ";"+ nStr( CONTRACT ->summa/ 100) + ";"
    WISC ->( netUnLock())
  endif
return rV

funcDef magLastPaym with dEnd:D local rV := .T., arrMBuf
  arrMBuf := CtoA( CONTRACT ->mBuffer, ";")
  if len( arrMBuf) = 0
    messageB( "Нет индекса издания !!!")
    return .F.
  endif
  if WISC ->( rV := mAppend())
    WISC ->schet  := CONTRACT ->debetAcc
    WISC ->type   := "935 "
    WISC ->buffer := CONTRACT ->intDoc + ";" + CONTRACT ->debetAcc + ";" + ;
         DtoC( dEnd) + ";" + CONTRACT ->code2 + ";" + CONTRACT ->code3+";"+;
         arrMBuf[ 1] + ";"
    WISC ->( netUnLock())
  endif
return rV

funcDef getExitUp with g:O
return g:exitState = GE_UP .OR. g:exitState = GE_TOP


static funcDef chkMagBeg with g:O ;
  local rV := .T., curD, firstDate, nMonth, nYear
  firstDate := firstDay( addMonth( setCurDate(), 1))
  nMonth    := aTrimmedScan( aMonths(), M ->GetList[ 2]:varGet)
  nYear     := M ->GetList[ 3]: varGet
  curD      := packDate( 1, nMonth, nYear)
  if curD < firstDate
    messageB( "Дата начала поручения не может быть меньше чем " + DtoC( firstDate))
    rV := .F.
  endif
return rV .OR. getExitUp( g)


static funcDef chkMagBeg2 with g:O, firstDate:D ;
  local rV := .T., curD, nMonth, nYear
  nMonth    := aTrimmedScan( aMonths(), M ->getList[ 1]:varGet)
  nYear     := M ->GetList[ 2]: varGet
  curD      := packDate( 1, nMonth, nYear)
  if curD < firstDate
    messageB( "Дата начала поручения не может быть меньше чем " + DtoC( firstDate))
    rV := .F.
  endif
return rV .OR. getExitUp( g)


static funcDef chkMagEnd with g:O ;
   local rV := .T., d1, d2, nMonth, nYear, nM1
   nMonth := aTrimmedScan( aMonths(), M ->getList[ 2]: varGet)
   nYear  := M ->GetList[ 3]: varGet
   d1     := packDate( 1, nMonth, nYear)
   nM1    := nMonth

   nMonth := aTrimmedScan( aMonths(), M ->getList[ 4]: varGet)
   nYear  := M ->GetList[ 5]: varGet
   d2     := lastDay( packDate( 1, nMonth, nYear))
   if d1 > d2
     messageB( "Дата начала поручения не может быть больше даты окончания !!!")
     rV := .F.
   endif
   if d2 < setCurDate() .AND. upper( allTrim( M ->getList[ 8]: varGet)) == "НЕТ"
     messageB( "Дата окончания поручения не может быть меньше текущей даты !!!")
      rV := .F.
   endif
   if nM1 >  6 .AND. nMonth<= 6
     messageB( "Поручение может действовать до конца декабря !")
     rV := .F.
   endif
   if nM1 <= 6 .AND. nMonth > 6
     messageB( "Поручение может действовать до конца июня !")
     rV := .F.
   endif
   if diffInMonths( d2, d1) > 6
     messageB( "Поручение можно заключать в пределах полугодия !")
     return .F.
   endif
return rV .OR. getExitUp( g)

funcDef diffInMonths with d1:D, d2:D ;
  local rV := 0, Y1, Y2, M1, M2
  Y1 := Year( d1); M1 := Month( d1)
  Y2 := Year( d2); M2 := Month( d2)
  rV := ( Y1 - Y2) * 12 + ( M1 - M2)
return rV

static funcDef chkProl with g:O local rV := .T., nMonth, ptr
  if lastKey() == 5 then return .T.
  if allTrim( g:varGet) == "Есть"
    nMonth := aTrimmedScan( aMonths(), M ->getList[ 4]: varGet)
    rV     := nMonth == 6 .OR. nMonth == 12
    if !rV then ;
      messageB( "Можно пролонгировать поручения,если они заканчиваются в июне или декабре")
  endif
return rV .OR. getExitUp( g)

funcDef magPrint with newContract:LU ;
  local provName, provCode, ptr, fullName, ptrProl, hasProl,;
        inn, izdCode, StrSumma, arrMBuf, ptrProv, prol

  fillProvList( "11", @provName, @provCode)
  ptr := aTrimmedScan( provCode, CONTRACT ->code3)
  if ptr <= 0 then return .F.
  fullName := provName[ ptr]

  ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))
  hasProl := subStr( CONTRACT ->buffer, 3, 1) == "Y"
  inn     := prov2Buffer( CONTRACT ->code3)

  arrMBuf := CtoA( CONTRACT ->mBuffer, ";")
  if len( arrMBuf) = 0 then return .F.
  izdCode := arrMBuf[ 1]
  StrSumma:= getS( CONTRACT ->summa/ 100)
  strSumma := razbivka( strSumma, {{ 50, "L"}, { 75, "L"}})

  if hasProl
    prol := { ;
      "7. ЕСЛИ ДО ОКОНЧАНИЯ ПЕРИОДА ДЕЙСТВИЯ ПОРУЧЕНИЯ, ОНО НЕ БУДЕТ ПРЕКРАЩЕНО В",;
      "   ОДНОМ ИЗ СЛУЧАВ, ПРЕДУСМОТРЕННЫХ В ПУНКТЕ 6, ТО ДЕЙСТВИЕ НАСТОЯЩЕГО  "  ,;
      "   ПОРУЧЕНИЯ АВТОМАТИЧЕСКИ ПРОДЛЕВАЕТСЯ НА СЛЕДУЮЩИЙ ПОЛУГОДОВОЙ ПЕРИОД "  ,;
      "   НА ПРЕЖНИХ УСЛОВИЯХ. "}
  else
      prol := "   - ПО ИСТЕЧЕНИИ ПЕРИОДА ДЕЙСТВИЯ ПОРУЧЕНИЯ;"
  endif
  servDummy( {{ "@docNumb",  CONTRACT ->intDoc           },;
              { "@provName", provName[ ptrProv]          },;
              { "@inn",      inn                         },;
              { "@debSchet", CONTRACT ->debetAcc         },;
              { "@d1",       CONTRACT ->dateBeg          },;
              { "@d2",       CONTRACT ->dateEnd          },;
              { "@orgName",  provName[ ptrProv]          },;
              { "@izdCode",  izdCode                     },;
              { "@summa1",   StrSumma[ 1]                },;
              { "@summa2",   StrSumma[ 2]                },;
              { "@prol",     prol                        },;
              { "@rdate",    rDate( setCurDate())        },;
              { "@atrname",  "ОТДЕЛЕНИЕ N "+ confOtdNumb() +;
                 ", " + rTrim( confFilial()) + ' ' + confBankName()} })
   printOpen( "mg1")
   servDummy( {})

return .T.


funcDef valEdit with newContract:LU ;
   local rV := .F., gets, aSource, aSource2, ptrSource, provName, provCode,;
         tmpSum, ptrKind, aKind, ptrProl, yesNo, tmpPct, ptrConv, aConv

if !valCanSign()
  messageB( "На отделении нельзя оформлять эту услугу !!!")
  return .F.
endif

accPrizList( "15 1", @aSource, @aSource2)
accPrizList( "15 4", @aSource, @aSource2,, .F.)
ptrSource := aTrimmedScan( aSource, CONTRACT ->debetAcc)
if len( aSource) < 1
   messageB( "У клиента нет открытых текущих и карточных счетов !!!")
   return .F.
endif

if CONTRACT ->dateCur == setCurDate()
  messageB( "Нельзя редактировать поручение в день открытия !!!")
  return .F.
endif

if empty( CONTRACT ->dateBeg)
   CONTRACT ->dateBeg := setCurDate() + 1
endif

tmpSum := tmpPct := 0.00
if CONTRACT ->summa< 0
   tmpPct := - CONTRACT ->summa
else
   tmpSum := CONTRACT ->summa/ 100
endif

if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc := confConvDoc()
endif
ptrConv := if( left( CONTRACT ->buffer, 1) == "2", 2, 1)
aConv   := { "Срочная конверсия", "Обычная конверсия" }

gets := {{ "Периодическая покупка валюты"                            },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                  },;
         { "Номер        :", block( CONTRACT ->intDoc),,{||.F.}      },;
         { "Начало       :", block( CONTRACT ->dateBeg),,{|| newContract},{|g| g:varGet > setCurDate() }},;
         { "Со счета     :", block( ptrSource), aSource,{|| newContract}              },;
         { "*" + chr( 25)                                            },;
         { "На счет      :", block( CONTRACT ->creditAcc),;
                    "99999999999999999999",,{|g| chkValSchet( g)   } },;
         { "Тип покупки  :", block( ptrConv), aConv                  },;
         { "Фиксир.сумма :", block( tmpSum),"99999999.99",, {|g| chkCrdSum( g, 7) .AND. chkMinValSum( g)}},;
         { "Проценты     :", block( tmpPct),"999.99",, {|g| chkCrdPct( g, 6)}}  }

rV := getUp( 5, 5, gets)
if rV
   CONTRACT ->docNumb   := CONTRACT ->intDoc
   CONTRACT ->debetAcc  := aSource2[ ptrSource, 2]
   CONTRACT ->code3     := "71"
   CONTRACT ->buffer    := nStr( ptrConv) + subStr( CONTRACT ->buffer, 2)
   if empty( CONTRACT ->dateCur) then CONTRACT ->dateCur  := setCurDate()
   if tmpPct > 0
      CONTRACT ->summa := - tmpPct
   else
      CONTRACT ->summa := tmpSum * 100
   endif
   valFirstPaym()
endif

return rV

funcDef chkMinValSum with g:O local minSum := valMinSum()
  if g:varGet > 0 .AND. g: varGet < minSum
    messageB( "Вы не можете оформить поручение на сумму менее " + nStr( minSum))
    return .F.
  endif
return .T.

funcDef chkValSchet with g:O local newSchet := "", rV := .T.
  newSchet := getKey( g: varGet)


  if len( allTrim( g: varGet)) == 0
     messageB( "Счет должен быть введен обязятельно !!!")
     rV := .F.
     return rV
  endif

  if newSchet <> g: varGet
    messageB( "Неверен контрольный разряд в счете !!!")
    rV := .F.
  endif

//CARDNEW Проверять надо по типу счета, т.к. 42301 - это еще и карточный счет
//  if rV .AND. left( newSchet, 5) <> "42301" 
//    messageB( "Можно вводить только 42301 счета !")

  if ( rV .AND. left( newSchet, 5) <> "42301") .AND. ( rV .AND. left( newSchet, 5) <> "40817" )  //OVD 18.07.2005
    messageB( "Можно вводить только 42301 или 40817 счета !")
    rV := .F.
  endif    
return rV

funcDef valPrint with newContract:LU ;
  local tmpSum, tmpPct, minSum := 0, ptrConv, aConv, d1, rDate, arTypePay
tmpPct := tmpSum := 0

if CONTRACT ->summa< 0
 arTypePay := { "",;
"в размере @prc**** % от каждого безналичного поступления на счет, но не менее ",;
"@minsum***************************************************************** рублей."}
else
 arTypePay := { ;
"ежемесячно в размере ",;
"@sum******************************************************************** рублей."}
endif

if CONTRACT ->summa< 0
   tmpPct := nStr( - CONTRACT ->summa)
   tmpSum := "______________________________________________________________"
else
   tmpSum := CONTRACT ->summa/ 100
   tmpSum := lTrim( mStr( tmpSum)) + " ( " + rSumma( tmpSum) + " )"
   tmpPct := "___"
endif
d1      := max( setCurDate() + 1, CONTRACT ->dateBeg)
ptrConv := if( left( CONTRACT ->buffer, 1) == "2", 2, 1)
aConv   := { "срочном", "обычном" }
rDate   := rDate( setCurDate())
//rDate   := left( rDate, len( rDate) - 4)
servDummy( {;
   { "@filial",    confFilial()               },;
   { "@otd",       confOtdNumb()              },;
   { "@docN",      CONTRACT ->docNumb         },;
   { "@curdate",   rDate                      },;
   { "@prc",       tmpPct                     },;
   { "@sum",       tmpSum                     },;
   { "@typePay",   arTypePay                  },;
   { "@minsum",    getS( valMinSum())         },;
   { "@valtype",   aConv[ ptrConv]            },;
   { "@d1",        DtoC( d1)  },;
   { "@debetAcc",  CONTRACT ->debetAcc        },;
   { "@creditAcc",  CONTRACT ->creditAcc      }})
printOpen( "VAL1")
servDummy( {})
return .T.

funcDef valFirstPaym local rV, sum, ptrConv
   sum  := CONTRACT ->summa
   sum  := if( sum >0, sum/ 100, sum)
   ptrConv := if( left( CONTRACT ->buffer, 1) == "2", 2, 1)
   if WISC ->( rV   := mAppend())
      WISC ->schet  := CONTRACT ->debetAcc
      WISC ->type   := "946 "
      WISC ->buffer := CONTRACT ->intDoc + ";" + nStr( sum)        + ";" + ;
                       CONTRACT ->creditAcc + ";" + nStr( ptrConv) + ";" + ;
                       myDtoC( CONTRACT ->dateBeg)
      WISC ->( netUnLock())
   endif
return rV

funcDef valLastPaym local rV := .T.
   if WISC ->( rV   := mAppend())
      WISC ->schet  := CONTRACT ->debetAcc
      WISC ->type   := "936 "
      WISC ->buffer := CONTRACT ->intDoc + ";" + myDtoC( setCurDate() + 1)
      WISC ->( netUnLock())
   endif
return rV

funcDef valClose local rV := .F., rDate
   CONTRACT ->dateEnd := setCurDate() + 1
   rV := valLastPaym()
   // rDate   := rDate( setCurDate())
   // rDate   := left( rDate, len( rDate) - 4)
   rDate := rDate( setCurDate() + 1)
   rDate := subStr( rDate, 1, len( rDate) - 5)
   servDummy( {;
      { "@filial",    confFilial()       },;
      { "@otd",       confOtdNumb()      },;
      { "@intDoc",    CONTRACT ->docNumb },;
      { "@stopdate",  rDate              },;
      { "@typewisc",  "на периодическую покупку валюты" },;
      { "@valuse",    "-заявки "         },;
      { "@docN",      CONTRACT ->docNumb },;
      { "@d2",        rDate              }})
   // printOpen( "val2")
   printOpen( "serst")
   servDummy( {})
return rV

static funcDef valMinSum local rV := 0.00, s := select()
  if provOpen()
     if PROVIDER ->( dbSeek( "5271"))
       rV := val( PROVIDER ->buffer)
     endif
     provClose()
     select (s)
  endif
return rV

static funcDef valCanSign local rV := .F., s := select()
  if provOpen()
     rV := PROVIDER ->( dbSeek( "5271"))
     provClose()
     select (s)
  endif
return rV


// Add by TMV  28-10-2002
// Перевод с текущего счета на текущий счет в другом отделении
//
funcDef transAccEdit ;
   local rV, gets, aSource, aSource2, nextMonth, firstDate,;
         month1, year1, month2, year2, aDest, aDest2      ,;
         ptrSource, ptrDest, provName, provCode, ptrProv  ,;
         tmpSum, ptrKind, aKind, ptrProl, yesNo, d1, d2   ,;
			sDest

yesNo   := { "Есть", "Нет"}
//ptrProl := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", 1, 2)
ptrProl := 1

accPrizList( "15 1", @aSource,   @aSource2)
ptrSource := max( 1, aScan( aSource2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))
if len( aSource) < 1
   messageB( "У клиента нет открытых текущих счетов !!!")
   return .F.
endif

if !empty( CONTRACT ->debetAcc)
	sDest = allTrim(CONTRACT ->debetAcc)
else
	sDest = Space(20)
endif

if empty( CONTRACT ->dateBeg)
   CONTRACT ->dateBeg := setCurDate() + 1
   CONTRACT ->dateEnd := addMonth( CONTRACT ->dateBeg, 1)
endif

tmpSum  := CONTRACT ->summa/ 100
aKind   := { "Всего остатка", "Определенной суммы"}
ptrKind := if( tmpSum > 0, 2, 1)

if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc := confAccTransDoc()
endif
//         { "Окончание    :", block( CONTRACT ->dateEnd),,,{|g| checkD2( g, 3, 4)       }},;

// Edit by TMV  20-11-2002
gets := {{ "Перечисление средств на тек. счет в другом отделении"    },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"  },;
         { "Номер        :", block( CONTRACT ->intDoc),,{||.F.}      },;
         { "*" + chr( 25)                                            },;
         { "Начало       :", block( CONTRACT ->dateBeg),,,{|g| .T./*checkD1( g)*/   }},;
         { "На счет      :", block( ptrSource), aSource             },;
         { "Со счета     :", block( sDest), "@R 99999 999 9 99999999999",,;
				{|g| chkTranSchet(g)}}}

if rV := getUp( 5, 5, gets)
// Edit by TMV  20-11-2002 - переставлены debet и credit
   CONTRACT ->dateEnd   := addMonth( CONTRACT ->dateBeg, 12) // Replaced <4> to <12> by TMV  17-09-2003
   CONTRACT ->docNumb   := CONTRACT ->intDoc
   CONTRACT ->debetAcc  := sDest  //aDest2[    ptrDest,   2]
   CONTRACT ->creditAcc := aSource2[ ptrSource, 2]
   CONTRACT ->code3     := "71" // provCode[ 1]
   CONTRACT ->summa     := -1
   if empty( CONTRACT ->dateCur) then CONTRACT ->dateCur  := setCurDate()
   CONTRACT ->buffer   := left( CONTRACT ->buffer, 2) + ;
      if( ptrProl <= 1, "Y", "N") + subStr( CONTRACT ->buffer, 4)
   transAccPaym()
endif

return rV

// Add by TMV  28-10-2002
// Для перевода с текущего счета на текущий счет в другом отделении
//
funcDef transAccPaym local rV, prol, sum
if rV := WISC ->( mAppend())
   prol := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", "1", "0")
   sum  := CONTRACT ->summa
   sum  := if( sum >0, sum/ 100, sum)
   WISC ->schet  := CONTRACT ->creditAcc // Replace < debetAcc to creditAcc>
													  // by TMV 22-11-2002
   WISC ->type   := "947"
   WISC ->buffer := CONTRACT ->intDoc         + ";" + ;
                    CONTRACT ->debetAcc       + ";" + ;
                    DtoC( CONTRACT ->dateBeg) + ";" + ;
                    DtoC( CONTRACT ->dateEnd) + ";" + ;
                    CONTRACT ->code2          + ";" + ;
                    CONTRACT ->code3          + ";" + ;
                    prol                      + ";" + ;
                    CONTRACT ->creditAcc      + ";" + ;
                    nStr( sum)+ ";"
   WISC ->( netUnLock())
endif
return rV


// Add by TMV  28-10-2002
// Для перевода с текущего счета на текущий счет в другом отделении
//
static funcDef chkTranSchet with g:O ;
   local rV := .T., sBik := g:varGet, aDist:={'42301','42309','42601','42609','40817','40820'}  //OVD 18.07.2005
//   local rV := .T., sBik := g:varGet, aDist:={'42301','42309','42601','42609'}

if empty( sBik)
   messageB( "Вы ОБЯЗАНЫ ввести кор. счет !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif len(alltrim( sBik)) != 20
   messageB( "Вы неполностью ввели кор. счет !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif aScan( aDist, {|x| x == Left(sBik, 5)} ) == 0
   messageB( "Вы неверно ввели первые 5 цифр счета !!! ")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif substr(sBik, 6, 3) != '810'
   messageB( "Вы неверно ввели код валюты - " + substr(sBik, 6, 3)+" !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif !chkKeyAcc(sBik) // Remove confBic() by TMV  21-11-2002
   messageB( "Проверьте кор. счет. Ключ вычислен неверно !!! ")
   rV = (.F. .OR. g:exitState == GE_UP) // Is on by TMV  21-11-2002
endif
return rV


// Add by TMV  28-10-2002
// Для перевода с текущего счета на текущий счет в другом отделении
//
funcDef transAccPrint with newContract:L local name, debSchet, credSchet

if newContract
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))
//   ACC ->( dbSeek( CONTRACT ->creditAcc))
   name := '"Текущий счет"'
   ACC ->( baseRestPos())
   if confUseAgent()
     debSchet  := CONTRACT ->debetAcc
     credSchet := CONTRACT ->creditAcc
   else
     debSchet  := transSplitSchet( CONTRACT ->debetAcc) 
     credSchet := transSplitSchet( CONTRACT ->creditAcc)
   endif
   servDummy( {{ "@otdnumb",      confOtdNumb()                         },;
               { "@debSchet",     debSchet                              },;
               { "@credSchet",    credSchet                             },;
               { "@upperVidName", upper( name)                          },;
               { "@vidName",      name                                  },;
               { "@d1",           CONTRACT ->dateBeg                    },;
               { "@rSumma",       getS( CONTRACT ->summa/ 100)          },;
               { "@sum",          lTrim( mStr( CONTRACT ->summa/ 100))+" рублей"},;
  					{ "@filial",   confFilial()     },;    && Add by TMV  11-11-2002
					{ "@numberp",      CONTRACT->INTDOC}}) // Add by TMV  11-11-2002
   if CONTRACT ->summa < 0
      printOpen( "tr_3")
   else
      printOpen( "tr_2")
   endif
   servDummy( {})
else
   contInitServ()
   printOpen( "tr2")
   contCloseServ()
endif
return NIL

// Add by TMV  31-10-2002
***************************************
** Поиск незакрытого поручения на перевод всего остатка
***************************************
funcDef transExist With sClient:C, sType:C local rV := .F.,;
	aType:={"54"},;  // Перевод на счет в другом отделении
	s := select(), i := CONTRACT->(indexord())

if aScan( aType, {|x| x == sType} ) > 0
	CONTRACT ->( baseSavePos())
	CONTRACT ->( dbSetOrder( 1))
	if CONTRACT->( dbSeek( sClient ))
		Select ('CONTRACT')
		Locate For CONTRACT->STATUS == 1 .And. CONTRACT->SUMMA < 0;
			While CONTRACT->CODE == sClient
		rV = found()
		Select( s )
	endif
	CONTRACT ->( dbSetOrder( i ))
	CONTRACT ->( baseRestPos())
endif

return rV