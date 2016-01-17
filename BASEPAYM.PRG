*==============================================================================
*
*                     Программа  : MON.EXE
*                     Файл       : basePaym.prg
*                     Автор      : Цейтлин П.М.
*
*                     Назначение : Прием платежей
*
*------------------------------------------------------------------------------
//06-05-99 Организован вызов ком. платежей безналично с ТПС из выплаты пенсии
//27.09.99 Закомментировано по просьбе Кирилла
//25-01-2000 Возможность выхода по ESC из окна ввода идентификатора
//02-02-2005 Продажа льготных проездных билетов
*
* 09-12-2002 TMV
* В "Просмотр безналичных" добавлены операции 6031, 6136, 6137
*  ф-ии paymView()
*
* 17-12-2002 TMV
* В "Просмотр безналичных" добавлены операции 7033
*  ф-ии paymView()
*--ОТМЕНЕНО
*

#include "clippExt.ch"
#include "common.ch"
#include "getexit.ch"
#include "inkey.ch"
#define GET_Y 3
#define GET_X 1
#define KOD_RAZ    "12"  //код услуги в provider.dbf для разовых платежей
#define KOD_ZABOTA "65"  //поставщик Забота+ - первый взнос
#define KOD_SERVIS "86"  //первый взнос для Забота-Сервис
#define KOD_ZAB_1F "1F"  //первый взнос для ТФ Забота
#define KOD_PF     "05"  //при работе с Пенс.Фондом задаем игривый вопрос
#define KOD_PFS    "43"  //штрафы в ПФ
#define KOD_PFP    "89"  //пени в ПФ
#define KOD_PFN    "93"  //недоимки в ПФ
#define KOD_TMS    "07"  //территориальный мед. фонд
#define KOD_FMS    "17"  //федеральный мед. фонд

#define SOCIAL_TAX_CODES "5H_5I_5J_5K_5M_5N_6D_6E_6F_6G_6H_6I_6J_6K_"
#define NOT_A_MONTH 13
#define EMPTY_DATE ctod( "" )

// #define sESN_LIST "5H,5I,5J,5K,5L,5M,5N"
#define sPF_LIST KOD_PF + "," + KOD_PFS + "," + KOD_PFP + "," + KOD_PFN

static yesCheck
static newKp := .F.
static aTarif := { "Не указан",;
                   "29% (28% + 1%)",;
                   "28%",;
                   "21,6% (20,6% + 1%)",;
                   "20,6%"}
static aNTarif := {{-1,  -1},;
                   {28,   1, "28%",   "1%"},;
                   {28,   0, "28%",   "0%"},;
                   {20.6, 1, "20,6%", "1%"},;
                   {20.6, 0, "20,6%", "0%"}}
static aOsn := { "за сокрытый фонд",;
                 "за непредоставление отчета",;
                 "за отказ от регистрации",;
                 ""}
static aVid := { "по акту камеральной проверки",;
                 "по акту документальной проверки",;
                 ""}
static aTip := { "по акту документальной проверки",;
                 "по соглашению",;
                 "по расчетной ведомости",;
                 ""}

static a_Mon := { "январь", "февраль", "март",     "апрель",  "май",    "июнь",;
                  "июль",   "август",  "сентябрь", "октябрь", "ноябрь", "декабрь",;
                  "<нет>" }

procedure paymView
// Replace "6136", "6137" to "7033" by TMV  17-12-2002
  viewOper( { "5010",;
				 "6031", "6136", "6137"},;     // Add by TMV  09-12-2002
				 { ""}, "Безналичные платежи")
return

function paymCash(isCash, tpSchet, arrSums)
local gets, curClient
local sum, str1, str2, ro, sim, oper, schet, About:="", summa1 := 0, aOrder
local accSchet, accSchet2, ptrSchet, doc1, codi, code2, code3
local summa := 0, i, aKom:={ }, ind, ordNum, arr1, arr2, n:=0, isZabt, kol
local plat, isCACC := .F., sCACCSchet
local tipClient, aTipClient, kolStr, aOrder1
local mon:={ , , , , }, yea:={ , , , , }
local peni:={ 0.00, 0.00, 0.00, 0.00, 0.00}
local aFam:={ , , , , }
local aPF :={ , , , , }
local old_format
local rv:=.T.
local path:=alltrim(confPathOD())
local indProc, allProc, sumProc
local isPF, isFF
local wasZab := .F., Sel := Select()
local aDocCnt := { 1, 1, 1, 1, 1}
local cInn := space( 12)
local cardNumb, cardN1, cardDate, prevNumb, prevDate
local code31:=0,ll
local cardDate1
local opened := Select( "TICKETS") >0, rT:=.F.

public vidat, bH:="", arr:={}
public askBefore:=.F. 
public provp1:="0000000000"
set key K_F1 to pHelp()

if len(path) <= 2
  path:=".\"
else
  if right( path, 1) != "\"
    path:=path+"\"
  endif
endif

mon[1]:=mon[2]:=mon[3]:=mon[4]:=mon[5]:=month(setCurDate())
yea[1]:=yea[2]:=yea[3]:=yea[4]:=yea[5]:=year(setCurDate())

curClient := padr(cliFullName(),40)
sum  := { 0.00, 0.00, 0.00, 0.00, 0.00}

plat := { space(40), space(40), space(40), space(40), space(40)}

tipClient :=2
aTipClient:={"Юр. лицо (объявл.,квит.,ордер)     ",;
             "Физ.лицо (квит., прих. ордер)      ",;
             "Организация(квит.,прих.ордер,ордер)"}

aFillProvs()

//  Выдана льготная карта?
 prevNumb  := cardNumb  := left( CLIENTS ->colibri, 7)
 cardDate  := subStr( CLIENTS ->colibri, 8, 3)
 prevDate  := cardDate  := decodeKrupDate( cardDate)
 askBefore := !empty( cardNumb) .OR. !empty( cardDate)

if len( aProvName()) > 1
  wisc->(dbgobottom())
  newKp:=setupRead( "NEWKP")
  aProvDocn( "init")
  aProcPeni( "init")
  basepath( assInfo(), base_path())
  baseMOpen( assInfo())

  if !empty( arrSums)
    assort->(dbsetorder(1))
    for i:=1 to len(arrSums)
      if i < 4
        kol := ascan( aProvCode3(), {|x| x == arrSums[i][1]})
        if kol = 0
          messageB("Неверный код поставщика "+arrSums[i][1])
          baseMClose( assInfo())
          select (sel)
          return .F.
        endif
        plat[i] := padr(aProvName()[kol], 40)
        sum[i]  := arrSums[i][2]
      endif
    next
    summa:=0
    for i:=1 to 5
      summa+=sum[i]
    next
    if summa = 0
      baseMClose( assInfo())
      select (sel)
      return .T.
    endif
    summa:=0
  endif
  if confUseAgent()
    if isCash  then aTipClient := { "Физ.лицо(Приходный ордер)"}
    if !isCash then aTipClient := { "Физ.лицо(Поручение)      "}
  endif

  gets:={{ "Разовые платежи"+space(58)},;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"  },;
         { "Плательщик      :", block( curClient), , {|| isCash} },;
         { "ИНН             :", block( cInn), "999999999999"     },;
         { "Тип плательщика :", block( tipClient), aTipClient, {|| isCash} },;
         { "1.Платеж", block( plat[1]), , ,;
                {|g| isCACC .OR. checkCode1(g, 1, @aFam)},;
                {|g| getVvod( g, aClone( aProvName()), aClone(aProvCode3()),;
                              a_mon, 5 + n, aClone( aProvBuff()), 1  )} },;
         { "*" + chr( 25)},;
         { "  Сумма ", block( sum[1]), "9999999.99",;
                       {|| provSum( 4 + n)},;
                       {|g| chkSum( g, 6 + n,  1, aPF, aClone(aProvCode3()), aClone( aProvBuff()), 1 ) } },;
         { "*За",      block( mon[1]),  a_mon,;
                       {|g| checkPeni(1)},;
                       {|g| kolPeni( 6 + n,  1) .and. checkMonth( g, 6 + n, 1 )}},;
         { "*",        block( yea[1]),  "9999",;
                       {|g| !empty( g: varGet()) .and. checkPeni(1)},;
                       {|g| kolPeni( 6 + n,  1)}},;
         { "*Кол-во докум.",   block( aDocCnt[1]), "99999",,{|g| g:varGet > 0} },;
         { ""                                                           },;
         { "2.Платеж", block( plat[2]), , ,;
                {|g| isCACC .OR. checkCode1(g, 2, @aFam)},;
                {|g| getVvod( g, aClone( aProvName()), aClone(aProvCode3()),;
                              a_mon, 10 + n, aClone( aProvBuff()), 3 )} },;
         { "*" + chr( 25)},;
         { "   Сумма", block( sum[2]), "9999999.99",;
                       {|| provSum( 9 + n )},;
                       {|g| chkSum( g, 11 + n,  2, aPF, aClone(aProvCode3()), aClone( aProvBuff()), 3 )} },;
         { "*За",      block( mon[2]),  a_mon,;
                       {|g| checkPeni(2)},;
                       {|g| kolPeni( 11 + n,  2) .and. checkMonth( g, 11 + n, 2 )}},;
         { "*",        block( yea[2]),  "9999",;
                       {|g| !empty( g: varGet()) .and. checkPeni(2)},;
                       {|g| kolPeni( 11 + n,  2)}},;
         { "*Кол-во докум.",   block( aDocCnt[2]), "99999",,{|g| g:varGet > 0} },;
         { ""                                                           } }
   aadd(gets,;
         { "3.Платеж", block( plat[3]), , ,;
                {|g| isCACC .OR. checkCode1(g, 3, @aFam)},;
                {|g| getVvod( g, aClone( aProvName()), aClone(aProvCode3()),;
                              a_mon, 15 + n, aClone( aProvBuff()), 5 )} })
   aadd(gets,;
         { "*" + chr( 25)})
   aadd(gets,;
         { "   Сумма", block( sum[3]), "9999999.99",;
                       {|| provSum( 14 + n )},;
                       {|g| chkSum( g, 16 + n,  3, aPF, aClone(aProvCode3()), aClone( aProvBuff()), 5 )} })
   aadd(gets,;
         { "*За",      block( mon[3]),  a_mon,;
                       {|g| checkPeni(3)},;
                       {|g| kolPeni( 16 + n,  3) .and. checkMonth( g, 16 + n, 3 )}})
   aadd(gets,;
         { "*",        block( yea[3]),  "9999",;
                       {|g| !empty( g: varGet()) .and. checkPeni(3)},;
                       {|g| kolPeni( 16 + n,  3)}})
   aadd(gets,;
         { "*Кол-во докум.",   block( aDocCnt[3]), "99999",,{|g| g:varGet > 0} })

   aadd(gets, { ""})
   //
   // Добавили 4 платеж
   //
   aadd(gets,;
         { "4.Платеж", block( plat[4]), , ,;
                {|g| isCACC .OR. checkCode1(g, 4, @aFam)},;
                {|g| getVvod( g, aClone( aProvName()), aClone(aProvCode3()),;
                              a_mon,  20 + n, aClone( aProvBuff()), 7 )} })
   aadd(gets,;
         { "*" + chr( 25)})
   aadd(gets,;
         { "   Сумма", block( sum[4]), "9999999.99",;
                       {|| provSum( 19 + n )},;
                       {|g| chkSum( g, 21 + n,  4, aPF, aClone(aProvCode3()), aClone( aProvBuff()), 7 )} })
   aadd(gets,;
         { "*За",      block( mon[4]),  a_mon,;
                       {|g| checkPeni(4)},;
                       {|g| kolPeni( 21 + n,  4) .and. checkMonth( g, 21 + n, 4 )}})
   aadd(gets,;
         { "*",        block( yea[4]),  "9999",;
                       {|g| !empty( g: varGet()) .and. checkPeni(4)},;
                       {|g| kolPeni( 21 + n,  4)}})
   aadd(gets,;
         { "*Кол-во докум.",   block( aDocCnt[4]), "99999",,{|g| g:varGet > 0} })
   aadd(gets, { ""})
   //
   // Добавили 5 платеж
   //
   aadd(gets,;
         { "5.Платеж", block( plat[5]), , ,;
                {|g| isCACC .OR. checkCode1(g, 5, @aFam)},;
                {|g| getVvod( g, aClone( aProvName()), aClone(aProvCode3()),;
                              a_mon, 25 + n, aClone( aProvBuff()), 9 )} })
   aadd(gets,;
         { "*" + chr( 25)})
   aadd(gets,;
         { "   Сумма", block( sum[5]), "9999999.99",;
                       {|| provSum( 24 + n )},;
                       {|g| chkSum( g, 26 + n,  5, aPF, aClone(aProvCode3()), aClone( aProvBuff()), 9 )} })
   aadd(gets,;
         { "*За",      block( mon[5]),  a_mon,;
                       {|g| checkPeni(5)},;
                       {|g| kolPeni( 26 + n,  5) .and. checkMonth( g, 26 + n, 5 )}})
   aadd(gets,;
         { "*",        block( yea[5]),  "9999",;
                       {|g| !empty( g: varGet()) .and. checkPeni(5)},;
                       {|g| kolPeni( 26 + n,  5)}})
   aadd(gets,;
         { "*Кол-во докум.",   block( aDocCnt[5]), "99999",,{|g| g:varGet > 0} })
   aadd(gets, { ""})

   aAdd( gets, { "Для выбора платежа из списка нажмите клавишу 'Пробел'"})
   aAdd( gets, { "Для ввода платежа по коду нажмите клавиши ( 0..9 )   "})

  if !isCash
    n:=1
    aAdd( Gets, NIL); aAdd( Gets, NIL); aAdd( Gets, NIL)
    aIns( Gets, 3); aIns( Gets, 3); aIns( Gets, 3)
    //06-05-99
    if empty( tpSchet)
      accPrizList( "15 1", @accSchet, @accSchet2)
      //
      // всавка для CACC
      //
      sCACCSchet := setUpValue( "CACC")
      if len( sCACCSchet) == 20 .and. empty( arrSums)
         isCACC := noYes( { "Вы хотите провести платеж со ",;
                           "СЧЕТА АККУМУЛИРОВАНИЯ КОММУНАЛЬНЫХ ПЛАТЕЖЕЙ ?"}) .AND. ;
                   noYes( { "Вы действительно решили работать со ",;
                           "СЧЕТОМ АККУМУЛИРОВАНИЯ КОММУНАЛЬНЫХ ПЛАТЕЖЕЙ ?"})
      endif
      if isCACC
         gets[ 1]  := { "Разовые платежи со СЧЕТА АККУМУЛИРОВАНИЯ КОММУНАЛЬНЫХ ПЛАТЕЖЕЙ"}
         curClient := rTrim( confFilial()) + "," + confOtdName()
         accSchet2 := {{ -1, sCACCSchet}}
         accSchet  := { sCACCSchet }
      endif
    else
      accSchet2 := {{ -1, tpSchet}}
      accSchet  := { "Счет "     + splitSSchet( tpSchet) + ;
                     " Остаток " + mStr( sumRest( tpSchet, setCurDate() )) }
    endif
    gets[ 3] := { "Со счета", block( ptrSchet), accSchet}
    gets[ 4] := { "*" + chr( 25)}
    gets[ 5] := { ""}
    ptrSchet := 1
    if len( accSchet) < 1
       messageB( "У клиента нет пенсионных счетов")
       baseMClose( assInfo())
       select (sel)
       return .F.
    endif
  endif

  setkey( K_ESC, {|| esckey()})

  if !empty(arrSums) .or. getUp( GET_Y, GET_X, gets, , ,{|| rHelp()})

    setkey( K_ESC, nil)
    for i:=1 to 5
      codi:=ascan(aProvName(),{|x| upper(alltrim(x))==upper(alltrim(plat[i]))})
      if sum[i] > 0.00 .and. codi > 1
        code2:=aProvCode2()[ codi]
        code3:=aProvCode3()[ codi]
        if code3="31"
          code31++
        endif

//      Проверяем на уже выданную карту
        if code3="31"   //Выдача льготной проездной карточки
          prevNumb  := cardNumb  := left( CLIENTS ->colibri, 7)
          cardDate  := subStr( CLIENTS ->colibri, 8, 3)
          prevDate  := cardDate  := decodeKrupDate( cardDate)
          askBefore := !empty( cardNumb) .OR. !empty( cardDate)
          cardDate  := setCurDate()
          cardNumb:=aProvDocn()[i][1]
        endif

        isPF:=at(code3, KOD_PF+","+KOD_PFS+","+KOD_PFP+","+KOD_PFN) > 0
        //тип операции при оплате наличными
        if isCash
          oper :=aProvOper()[ codi]
        endif
        if code3 == KOD_ZABOTA .or. code3 == KOD_SERVIS .or. code3 == KOD_ZAB_1F
          isZabt:=.T.
        else
          isZabt:=.F.
        endif
        if empty( oper)
          //Это товары в кредит ?
          if code3 == KOD_ZABOTA .or. code3 == KOD_SERVIS .or. code3 == KOD_ZAB_1F
            oper:= if( isCash, opDecode( "2010"), opDecode( "5010") )
          else
            oper:= if( isCash, opDecode( "2013"), opDecode( "5010") )
          endif
        else
          oper := opDecode( oper)
        endif

        operGetInfo( oper, @ro, @sim)
        summa:=round(summa+sum[i]+peni[i],2)
        if code3 == KOD_PF .or. code3 == KOD_PFS .or.;
             code3 == KOD_PFP .or. code3 == KOD_PFN
          str1:=if(aProvDocn()[i][2] = 'A', "", "За ")+"Рег. N "+;
                aProvDocn()[i][1]+" "
          if aProvDocn()[i][2] == 'B'
            str1+=alltrim( alltrim(aFam[i][1])+" "+alltrim(aFam[i][2])+" "+;
                           alltrim(aFam[i][3])+" "+alltrim(aFam[i][4]))+" "
          endif
          if code3 = KOD_PF
            str1+="Страховые взносы в ОПФР "
          elseif code3 = KOD_PFS
            str1+="Штрафы в ОПФР "
          elseif code3 = KOD_PFP
            str1+="Пени в ОПФР "
          elseif code3 = KOD_PFN
            str1+="Недоимки в ОПФР "
          endif
          old_format:=set(_SET_DATEFORMAT, 'dd.mm.yy')
          if code3 == KOD_PF
            if aPF[i][1][2] < 13 .and. aPF[i][1][3] > 1980 .and.;
                 aPF[i][1][3] <= year(setCurDate())
              str1+="за "+lower(alltrim(monthNN( aPF[i][1][2])))+;
                    str(aPF[i][1][3],5)+" года"
            endif
            indProc := aPF[i][1][1]
            if indProc > 1
              allProc := aNTarif [ indProc][1]+ aNTarif[ indProc][2]
              if aNTarif[ indProc][2] > 0
                sumProc := round(sum[i] / allProc * aNTarif[ indProc][1], 2)
                str1 += ", "+aNTarif[ indProc][3]+" - "+alltrim(mstr(sumProc))+" "
                sumProc := round(sum[i] - sumProc, 2)
                str1 += ", "+aNTarif[ indProc][4]+" - "+alltrim(mstr(sumProc))+" "
              else
                str1 += ", "+aNTarif[ indProc][3]+" - "+alltrim(mstr( sum[i]))
              endif
            endif
          elseif code3 = KOD_PFS
            if aPF[i][2][1] < 4
              str1+=aOsn[aPF[i][2][1]]+;
                    if(empty(aPF[i][2][2]), "",;
                       " по акту документальной проверки N "+aPF[i][2][2])+;
                    if(empty(aPF[i][2][3]), "", " от "+dtoc(aPF[i][2][3]))
            endif
          elseif code3 = KOD_PFP
            if aPF[i][3][1] < 3
              str1+=aVid[aPF[i][3][1]]+;
                    if(empty(aPF[i][3][2]), ""," N "+aPF[i][3][2])+;
                    if(empty(aPF[i][3][3]), ""," от "+dtoc(aPF[i][3][3]))
            endif
          elseif code3 = KOD_PFN
            str1+=aTip[aPF[i][4][1]]
            if aPF[i][4][1] < 4
              if aPF[i][4][1] < 3
                str1+=if(empty(aPF[i][4][2]), "", " N "+aPF[i][4][2])+;
                      if(empty(aPF[i][4][3]), "", " от "+dtoc(aPF[i][4][3]))
              else
                str1+=if(empty(aPF[i][4][4]), "", str(aPF[i][4][4],2)+" кв.")+;
                      if(empty(aPF[i][4][5]), "", str(aPF[i][4][5],5)+" г.")
              endif
            endif
          endif
          set(_SET_DATEFORMAT, old_format)
          str1+="; "
        else
          if isZabt
            //
            // это моя вставка
            //
            str1 := allTrim( aProvName()[ codi]) + ;
              if( empty( aProvDocn()[i, 1]), "", "/N " + aProvDocn()[i, 1] + "/")
            if !empty(aProvDocn()[i, 1]) .AND. ( ;
               !canAdd1ZPay( aProvDocn()[i][1]) .OR. ;
               contZabOtherDate( aProvDocn()[i][1])  )
              str1 += ",доплата"
            endif
            str1 += ": " + allTrim( mStr( round( sum[ i] + peni[ i], 2))) + "; "
          else
            //
            // Если производится платеж в рамках ЕСН, то назначение
            // платежа конструируется несколько по-другому:
            // (7значный код из PROV2 ->buffer), ТП: сумма за <период>
            //
            if code3 $ sESN_LIST()
               if len( rTrim( cInn)) < 12
                 errOK( "Для приема платежей ЕСН нужно ввести ИНН плательщика (12 знаков) !!!")
                 baseMClose( assInfo())
                 select (sel)
                 return .F.
               endif
               str1 := ecnNazn( code2, code3, sum[ i ],;
                                aProvDocn()[ i ][ 1 ],;
                                aProcPeni()[ i ][ 4 ],;
                                aProcPeni()[ i ][ 5 ])
            else
              str1 :=alltrim( aProvName()[  codi])+;
                   if(empty(aProvDocn()[i][1]), "", "/N "+aProvDocn()[i][1]+"/")+;
                   ": "+alltrim(mstr(round(sum[i]+peni[i],2)))+"; "
            endif
          endif
        endif
        doc1 :=aProvDocn()[ i ][ 2 ] + aProvDocn()[ i ][ 1 ]
        //это для печати общего поручения по разовым, исключая платежи в ПФ
        if !isPF
          About+=str1
          summa1:=round(summa1+sum[i]+peni[i],2)
        endif
        schet:=if(isCash, aProvCash()[ codi], aProvTran()[ codi])
        ind:=0
        //по товарам в кредит и всем видам платежей в ПФ
        //принудительно делаем отдельную проводку
        if !isZabt .and. code3 != KOD_PF .and. code3 != KOD_PFS .and.;
              code3 != KOD_PFP .and. code3 != KOD_PFN
          for kol:=1 to len(aKom)
            if aKom[kol][1] == schet .and. (!isCash .or. aKom[kol][16] == oper)
              //нельзя складывать данный платеж
              //с платежом по товарам в кредит и платежами в ПФ
              if !(aKom[kol][7][1] == KOD_ZABOTA .or.;
                   aKom[kol][7][1] == KOD_SERVIS .or.;
                   aKom[kol][7][1] == KOD_ZAB_1F .or.;
                   aKom[kol][7][1] == KOD_PF .or.;
                   aKom[kol][7][1] == KOD_PFS .or.;
                   aKom[kol][7][1] == KOD_PFP .or.;
                   aKom[kol][7][1] == KOD_PFN)// .and. aKom[ind][8][1] == '06')
                ind:=kol
                exit
              endif
            endif
          next
        endif
        if ind = 0
          ordNum:=if(isCash, ordIncIn( setCurDate()), ordIncMem( setCurDate()) )
          aadd(aKom, {  schet, round(sum[i]+peni[i],2), ordNum, str1,;
                      { round(sum[i]+peni[i],2)}, { sim}, { code3}, {code2},;
                      {doc1}, { peni[i]}, { aProvDocn()[i][3] },;
                      { aProcPeni()[i][1] }, { aProcPeni()[i][3] },;
                      { aProcPeni()[i][4] }, { sum[i] }, oper, { i }, ro,;
                      { aDocCnt[ i]  }})
        else
          aKom[ind][2] := round(aKom[ind][2]+sum[i]+peni[i],2)
          aKom[ind][4] += str1
          aadd(aKom[ind][ 5], round(sum[i]+peni[i],2))
          aadd(aKom[ind][ 6], sim)
          aadd(aKom[ind][ 7], code3)
          aadd(aKom[ind][ 8], code2)
          aadd(aKom[ind][ 9], doc1)
          aadd(aKom[ind][10], peni[i])
          aadd(aKom[ind][11], aProvDocn()[i][3])
          aadd(aKom[ind][12], aProcPeni()[i][1])
          aadd(aKom[ind][13], aProcPeni()[i][3])
          aadd(aKom[ind][14], aProcPeni()[i][4])
          aadd(aKom[ind][15], sum[i])
          aadd(aKom[ind][17], i)
          aAdd( aKom[ ind, 19], aDocCnt[ i])
        endif
      endif
    next

    for ind := 1 to len( aKom)
      if aKom[ind][7][1] == KOD_ZABOTA .or.;
         aKom[ind][7][1] == KOD_SERVIS .or.;
         aKom[ind][7][1] == KOD_ZAB_1F
           if !wasZab
              wasZab := .T.
            else
              messageB( "Найдено несколько первых взносов по Заботе ")
              baseMClose( assInfo())
              select (sel)
              return .F.
            endif
      endif
    next
    aOrder := {}
    if summa > 0.00
      if isCash           //Наличные платежи
        if code31>0       //Печать расписки на льгоный проезд
          servDummy( {{ "@colibri", cardNumb }})
          printOpenCol( "COLIB", 1)
          servDummy( {})
        endif
       if confUseAgent()
         aOrder := {}
         for ind := 1 to len( aKom)
           arr1  := aclone( aKom[ind, 5])
           arr2  := aclone( aKom[ind, 6])
           for i := len( arr1) + 1 to 5
             aAdd(arr1, 0); aAdd(arr2, " ")
           next
           aOrder1 := vznos_nal( aKom[ ind, 3], setCurDate()    ,;
               curClient + useInn( aKom[ ind][ 7], cInn)        ,;
               aKom[ ind, 1], arr1,;
               rTrim(confBankName())+","+rTrim(confFilial()),;
               aKom[ ind, 4], "", arr2,,, .T.)
           for kolStr := 1 to len( aOrder1) do aAdd( aOrder, aOrder1[ kolStr ])
         next
         printAOrder( aOrder)   //Печать после платежа
//
         if confPrintTo()= 1 //Если печать на принтер 
           set device to printer  //Прогон бумаги на 6.5 см
           set printer to
           set console off
           for ll=1 to 5
             @ prow()+ll,1 say space(1) + chr(13)+chr(10)
           next
           set printer off
           set console on
           set device to screen
         endif
//
       else               
        //может быть несколько ордеров
        for ind:=1 to len(aKom)
          isPF:=at(aKom[ind][7][1],;
                   KOD_PF+","+KOD_PFS+","+KOD_PFP+","+KOD_PFN) > 0
          isFF:= isPF .or. at(aKom[ind][7][1], KOD_TMS+","+KOD_FMS) > 0
         //
         // это моя вставка для заботы, но не моя
         //
         if aKom[ind, 7, 1] $  KOD_ZABOTA + "," + KOD_SERVIS + "," + KOD_ZAB_1F
            arr1 := aClone( aKom[ind, 5])
            arr2 := aClone( aKom[ind, 6])
            for i:= len( arr1) + 1 to 5
              aAdd( arr1, 0)
              aAdd( arr2, "  ")
            next
            aOrder := Vznos_Nal( aKom[ind][3], setCurDate(),;
               curClient + useInn( aKom[ ind][ 7], cInn)        ,;
               aKom[ ind, 1], arr1, confBankName()+','+ rTrim(confFilial()) ,;
               aKom[ ind, 4], confKassa(), arr2, aKom[ ind, 18])
            printAOrder( aOrder)
         else
          //не все так просто с этим vznos_nal
          arr1:=aclone(aKom[ind][5])
          arr2:=aclone(aKom[ind][6])
          for i:=len(arr1)+1 to 5 // 3 - внесено изменение
            aadd(arr1, 0)
            aadd(arr2, "  ")
          next
          //для платежей в ПФ принудительно печатаем квитанцию, прих.ордер,ордер
          //независимо от типа клиента
          if tipClient = 2 .or. tipClient = 3 .or. isPF
            //если суммы передашь в виде массива, то
            //обратно получишь не числовые значения, а уже строки (НУ, ПАША !!!)
            aOrder := vznos_nal(  aKom[ind][3], setCurDate(),;
               curClient + useInn( aKom[ ind][ 7], cInn)        ,;
                                 aKom[ind][1], arr1,;
                                 if(!isPF,confBankName()+','+ rTrim(confFilial()),;
                                    "ОПФР по СПб и Лен. области"),;
                                 aKom[ind][4],;
                                 confKassa(), arr2,;
                                 aKom[ind][18])
            //для организации, а также платежей в ПФ
            //дополнительно печатаем ордер
            if tipClient = 3 .or. isPF
              arr1:=aclone(aKom[ind][5])
              arr2:=aclone(aKom[ind][6])
              for i:=len(arr1)+1 to 5
                aadd(arr1, 0)
                aadd(arr2, "  ")
              next
              aOrder1 := dopOrd1(aKom[ind][3], setCurDate(),;
               curClient + useInn( aKom[ ind][ 7], cInn)        ,;
                                 aKom[ind][1], arr1,;
                                 if(!isPF,confOtdName(),;
                                    "ОПФР по СПб и Лен. области"),;
                                 aKom[ind][4],;
                                 balKas(), arr2,;
                                 if(!isPF, confBankName(), ""),;//"АБ 'Петровский' ", ""),;
                                 isPF)
              for kolStr:=1 to len(aOrder1)
                aadd(aOrder, aOrder1[kolStr])
              next
            endif
            printAOrder( aOrder)  //печатаем каждый ордер
          elseif tipClient = 1
            aOrder := dopObv(  aKom[ind][3], setCurDate(),;
               curClient + useInn( aKom[ ind][ 7], cInn)        ,;
                               aKom[ind][1], arr1,;
                               confOtdName(),;
                               if(isFF, aKom[ind][4],;
                                 "Обязательные платежи, добровольные взносы"),;
                               balKas(), arr2)
            arr1:=aclone(aKom[ind][5])
            arr2:=aclone(aKom[ind][6])
            for i:=len(arr1)+1 to 5
              aadd(arr1, 0)
              aadd(arr2, "  ")
            next
            aOrder1 := dopKvi( aKom[ind][3], setCurDate(),;
               curClient + useInn( aKom[ ind][ 7], cInn)        ,;
                               aKom[ind][1], arr1,;
                               confOtdName(),;
                               if(isFF, aKom[ind][4],;
                                 "Обязательные платежи, добровольные взносы"),;
                               balKas(), arr2)
            for kolStr:=1 to len(aOrder1)
              aadd(aOrder, aOrder1[kolStr])
            next
            arr1:=aclone(aKom[ind][5])
            arr2:=aclone(aKom[ind][6])
            for i:=len(arr1)+1 to 5
              aadd(arr1, 0)
              aadd(arr2, "  ")
            next
            aOrder1 := dopOrd2(aKom[ind][3], setCurDate(),;
               curClient + useInn( aKom[ ind][ 7], cInn)        ,;
                               aKom[ind][1], arr1,;
                               confOtdName(),aKom[ind][4],;
                               balKas(), arr2,;
                               if(!isPF, confBankName(), "")) //"АБ 'Петровский' ", ""))
            for kolStr:=1 to len(aOrder1)
              aadd(aOrder, aOrder1[kolStr])
            next
            printAOrder( aOrder)  //печатаем каждый ордер
          endif
         endif
        next
       endif
        //
        // Согласно СЗ от 1 филиала пропускаем 20 строк после печати ПКО
        //
/*
        if confSkip20()
          printAOrder( { "", "", "", "", "", "", "", "", "", "", ;
                         "", "", "", "", "", "", "", "", "", "" })
//Изменено 03.05.2001 в 12:57, Залманов А.З.---------------------^^^^^^^
        endif
*/
      else
        //
        // безналичное перечисление
        //
        schet  := accSchet2[ ptrSchet, 2]
        if if( isCACC, .T., summa <= sumRest( schet, setCurDate()))
          //получим образ поручения (не даю в get редактировать клиента,
          //так как все равно в memWisc используется cliFullName()
          //
          // CACC !!!
          //
          if !isCACC
            //Для всех платежей, кроме Пенс. Фонда, общее поручение
            if summa1 > 0 .and. len( About) > 0
              aOrder := memWisc( schet, summa1, About)
              printAOrder( aOrder)

              if confPrintTo()= 1 //Если печать на принтер 
                  set device to printer  //Прогон бумаги на 6.5 см
                  set printer to
                  set console off
                  for ll=1 to 5
                    @ prow()+ll,1 say space(1) + chr(13)+chr(10)
                  next
                  set printer off
                  set console on
                  set device to screen
              endif

            endif
            for ind:=1 to len(aKom)
              if at(aKom[ind][7][1],;
                    KOD_PF+","+KOD_PFS+","+KOD_PFP+","+KOD_PFN) > 0
                //Для Пенс. Фонда 2 экз. поручения
                //(реализовано внутри myMemWisc)
                aOrder := myMemWisc( schet, aKom[ind][2],;
                                    aKom[ind][4], aKom[ind][1])
                printAOrder( aOrder)
              endif
            next
          endif
          aOrder := {}
          //мемориальные ордера
          //
          // ордер не печатается в агентских отношениях
          //
          if !confUseAgent()
          for ind:=1 to len(aKom)
            if at(aKom[ind][7][1],;
                    KOD_PF+","+KOD_PFS+","+KOD_PFP+","+KOD_PFN) > 0
              aOrder1:=myMemOrder(aKom[ind][3], setCurDate(), cliFullName(),;
                                  aKom[ind][2], schet, aKom[ind][1],;
                                  aKom[ind][4])
              //нужно два мем. ордера !!!
              aorder:=aclone(aOrder1)
              for kolStr:=1 to len(aOrder1)
                aadd(aOrder, aOrder1[kolStr])
              next
            else
              if aKom[ind, 7, 1] $  KOD_ZABOTA + "," + KOD_SERVIS + "," + KOD_ZAB_1F
                //
                // для заботы ничего не печатаем по СЗ i055032001
                //
              else
                aOrder:= memOrder( aKom[ind][3], setCurDate(), schet,;
                   aKom[ind][1], aKom[ind][2], aKom[ind][4], ro)
              endif
            endif
            if len( aOrder) > 0 then printAOrder( aOrder)
          next
          endif

          if code31>0       //Печать расписки на льгоный проезд - безнал
            servDummy( {{ "@colibri", cardNumb }})
            printOpenCol( "COLIB", 1)
            servDummy( {})
          endif

        else
          errOK( "Эта проводка вызовет красное сальдо по счету !!!")
          baseMClose( assInfo())
          select (sel)

          return .F.
        endif
      endif
      //запомним последний ордер
      saveLastOrder( aOrder)
      if yesno("Клиент подписал документы ?")
        wasZab := .F.
        for ind:=1 to len(aKom)
          if aKom[ind][7][1] == KOD_ZABOTA .or.;
             aKom[ind][7][1] == KOD_SERVIS .or.;
             aKom[ind][7][1] == KOD_ZAB_1F
            isZabt:=.T.
          else
            isZabt:=.F.
          endif
          str1:=""
          //последовательно занесем в буфер по каждому платежу
          //код организации, код услуги, частную сумму и
          //код договора в организации
          //далее заносим доп. код организации,
          //месяц и год, за которые сделан платеж
          //сумму штрафа, период в днях и процент штрафа за один день
          old_format:=set(_SET_DATEFORMAT,"dd.mm.yyyy")
          for i:=1 to len(aKom[ind][7])

            str1+=aKom[ind][7][i]+";"+aKom[ind][8][i]+";"+;
                  alltrim(str(aKom[ind][15][i],12,2))+";"+;
                  if(empty(aKom[ind][ 9][i])," ",aKom[ind][ 9][i])+";"
            //
            // для пенсюков оставлена старая структура поручения,
            // для новых поручений изменена в соответствии с ЕСН
            //
            //if !( aKom[ ind][ 7][ i] $ sPF_LIST )
               str1 += left( DtoS( aKom[ ind][ 14][ i]), 6) + ";" + ;
                       nStr( aKom[ ind][ 19][ i]) + ";"
            //endif

            if newKp
              str1+=;
                  if(empty(aKom[ind][11][i])," ",aKom[ind][11][i])+";"+;
                  myDtoC(aKom[ind][14][i])+";"+;
                  alltrim(str(aKom[ind][10][i],12,2))+";"+;
                  alltrim(str(aKom[ind][13][i],6))+";"+;
                  alltrim(str(aKom[ind][12][i],12,2))+";"
            endif
          next
          set(_SET_DATEFORMAT,old_format)
          //по товарам в кредит заносим в sums->buffer код договора
          if sumWrite( if(isCash, aKom[ind][1], schet),;
                       if(isCash, aKom[ind][2], -aKom[ind][2]),;
                       setCurDate(), aKom[ind][16], aKom[ind][3], ,;
                       if(isZabt, aKom[ind][9][1], nil))
            /*НА БУДУЩЕЕ, ПРОВЕРЕНО - МИН НЕТ*/
            if !isZabt
              //заполняем wisc, так как для трех платежей еще хватит места в
              //sums->buffer, но для пяти надо 65 символов - нерационально

              //уникальный идентификатор - стоим на проводке
              str2:='A'+confOtdCode()+padl(sums->(recno()),5,"0")+;
                    indDate(sums->ddate)+indTime(sums->ttime)
              if sums->(mrlock())
                sums->buffer := str2
                sums->(netunlock())
              endif
              wisc->(dbappend())
              if !neterr()
                wisc->schet := str2
                wisc->type  := '977'
                //спереди добавим накопительный счет и общее количество
                //платежей, сделанных одной проводкой
                //а в конце всегда запишем клиента
                wisc->buffer:= aKom[ind][1]+";"+;
                   alltrim(str(len(aKom[ind][7]),2))+";"+str1+;
                   upper(alltrim(curClient))+";" + ;
                   if( !empty( useInn( aKom[ ind][ 7], cInn)), cInn, "") + ";"
                if at(aKom[ind][7][1], KOD_PF+","+KOD_PFS+","+;
                                       KOD_PFP+","+KOD_PFN) > 0
                  old_format:=set(_SET_DATEFORMAT, 'dd.mm.yyyy')
                  i:=aKom[ind][17][1]
                  str1:=padl(aKom[ind][3],6,"0")+";"
                  //обработка ФИО и адреса, если плятят за другого
                  if aProvDocn()[i][2] == "B"
                    for kol:=1 to 4
                      if empty(aFam[i][kol])
                        str1+=" ;"
                      else
                        str1+=alltrim(aFam[i][kol])+";"
                      endif
                    next
                  else
                    str1+=" ; ; ; ;"
                  endif
                  //обработка специфической информации
                  if aKom[ind][7][1] = KOD_PF
                    if !empty(aPF[i]) .and. !empty(aPF[i][1])
                      if aPF[i][1][1] > 1 .and. aPF[i][1][1] < 6
                        str1+=str(aPF[i][1][1] - 1,1)+";"
                      else
                        str1+="0;"
                      endif
                      if aPF[i][1][2] < 13 .and. aPF[i][1][3] > 1980 .and.;
                           aPF[i][1][3] <= year(setCurDate())
                        str1+=alltrim(str(aPF[i][1][2],2))+";"+;
                              str(aPF[i][1][3],4)+";"
                      else
                        str1+=" ; ;"
                      endif
                    else
                      str1+='0; ; ;'
                    endif
                  elseif aKom[ind][7][1] = KOD_PFS
                    if !empty(aPF[i]) .and. !empty(aPF[i][2])
                      if aPF[i][2][1] < 4
                        str1+=str(aPF[i][2][1],1)+";"
                      else
                        str1+=" ;"
                      endif
                      if !empty(aPF[i][2][2])
                        str1+=alltrim(aPF[i][2][2])+";"
                      else
                        str1+=" ;"
                      endif
                      if !empty(aPF[i][2][3])
                        str1+=dtoc(aPF[i][2][3])+";"
                      else
                        str1+=" ;"
                      endif
                    else
                      str1+=' ; ; ;'
                    endif
                  elseif aKom[ind][7][1] = KOD_PFP
                    if !empty(aPF[i]) .and. !empty(aPF[i][3])
                      if aPF[i][3][1] < 3
                        str1+=str(aPF[i][3][1],1)+";"
                      else
                        str1+=" ;"
                      endif
                      if !empty(aPF[i][3][2])
                        str1+=alltrim(aPF[i][3][2])+";"
                      else
                        str1+=" ;"
                      endif
                      if !empty(aPF[i][3][3])
                        str1+=dtoc(aPF[i][3][3])+";"
                      else
                        str1+=" ;"
                      endif
                    else
                      str1+=' ; ; ;'
                    endif
                  elseif aKom[ind][7][1] = KOD_PFN
                    if !empty(aPF[i]) .and. !empty(aPF[i][4])
                      if aPF[i][4][1] < 4
                        str1+=str(aPF[i][4][1],1)+";"
                      else
                        str1+=" ;"
                      endif
                      if aPF[i][4][1] < 3
                        if !empty(aPF[i][4][2])
                          str1+=alltrim(aPF[i][4][2])+";"
                        else
                          str1+=" ;"
                        endif
                        if !empty(aPF[i][4][3])
                          str1+=dtoc(aPF[i][4][3])+";"
                        else
                          str1+=" ;"
                        endif
                        str1+=" ; ;"
                      elseif aPF[i][4][1] = 3
                        str1+=" ; ;"
                        if aPF[i][4][4] > 0 .and. aPF[i][4][4] < 5 .and.;
                             aPF[i][4][5] > 1980 .and.;
                               aPF[i][4][5] <= year(setCurDate())
                          str1+=str(aPF[i][4][4],1)+";"+str(aPF[i][4][5],4)+";"
                        else
                          str1+=" ; ;"
                        endif
                      else
                        str1+=" ; ; ; ;"
                      endif
                    else
                      str1+=' ; ; ;'
                    endif
                  endif
                  wisc->buffer := wisc->buffer+str1
                  set(_SET_DATEFORMAT,old_format)
                endif
                wisc->(netunlock())
              else
                messageB("Ошибка записи в wisc !!!")
              endif
            else
              zabFirstPaym( aKom[ ind][ 9][ 1], isCash, aKom[ ind][ 2])
            endif
          else
            errOK( "Проводка не сделана !!!")
            rv:=.F.
          endif
        next
        if code31>0       //Заносим данные на льгоный проезд
          if !opened then rT := ticOpen()
          if opened .OR. rT
            rT = .F.
            if ticAppend()
              TICKETS->code    := CLIENTS ->code
              TICKETS->buffer  := SUMS ->buffer
              TICKETS->colibri := CLIENTS ->colibri
              ticUnLock()
            else
              messageB("Ошибка записи в TICKETS !!!")
            endif
            if !opened then ticClose()
          endif

          if CLIENTS ->( rV := mRLock())
            cardDate:= EncodeKrupDate( cardDate)
            CLIENTS ->colibri := padR( cardNumb, 7) + cardDate
            CLIENTS ->lEdited := .T.
            CLIENTS ->( netUnLock())
          endif
        endif

      else
        rv:=.F.
      endif    // 
    endif
  else

    //SET ESCAPE ON
    setkey( K_ESC, nil)

  endif
  baseMClose( assInfo())
  select (sel)

else
  messageB("Не определены организации для приема по разовым платежам")
endif

return rv

procedure escKey()
local oldBlk:=setkey( K_ESC, nil)
dialog({"Перед выходом из окна приема платежей",;
        "Подтвердите все данные клавишей <Enter>"},;
       {"Ok"})
setkey( K_ESC, oldBlk)
return

function checkPeni( ind)
if aProcPeni()[ind][1] > 0 .OR. aProcPeni()[ind][1] = -1
  return .T.
endif
return .F.

funcDef checkMonth with  oG:O, nInd:N, nNum:N;
                   local cMonth, nMonth, cName, cCode, cYear, nCode, dDate
  public getList
  cMonth := oG: varGet()
  nMonth := aSearchTrim( a_Mon, cMonth )
  cName  := getList[ nInd - 2 ]: varGet()
  nCode  := aSearchTrim( aProvName(), cName ) 
  if empty( nCode )
    messageB( "Неверно указано наименование услуги!" )
    return .F.
  endIf
  cCode  := aProvCode3()[ nCode ]
  if ( nMonth == NOT_A_MONTH )
    if cCode $ sESN_LIST()
      getList[ nInd + 1 ]: varPut( 0 )
      getList[ nInd + 1 ]: display()
      aProcPeni()[ nNum ][ 4 ] := EMPTY_DATE 
      aProcPeni()[ nNum ][ 5 ] := 1
      return .T.
    else
      messageB( "Необходимо указать месяц оплаты." )
      return .F.
    endIf
  endIf
  cYear := nStr( getList[ nInd + 1 ]: varGet())
  if ( nMonth == NOT_A_MONTH )
    cMonth := "01"
    aProcPeni()[ nNum ][ 5 ] := 1
  else
    cMonth := padL( nStr( nMonth ), 2, "0" )
    aProcPeni()[ nNum ][ 5 ] := 0
  endIf
  dDate := ctod( "01/" + cMonth + "/" + cYear )
  aProcPeni()[ nNum ][ 4 ] := dDate
return .T.

function kolPeni( ind, kol)
local yea, mon, mons, sum, period, peni, cCode, aCodes
local dto, dto1
local old_format:=set(_SET_DATEFORMAT, "yyyy.mm.dd")

public getList
//а штраф берем ?
peni := aProcPeni()[ kol ][ 1 ]
if peni > 0
//   messageB( "Неверный формат ASSORT. Ошибка в информации о расчете пени !!!")
//   break
  mons:=upper(alltrim(getList[ind]:varget()))
  mon:=padl(ascan( a_mon, {|x| upper(x) == mons}), 2, "0")
  yea:=padr(getList[ind+1]:varget(),4,"0")
  dto:=ctod(yea+"."+mon+".01")
//  aProcPeni()[kol][4]:=dto
  //платеж за более ранний месяц ?
  if substr(dtos(dto),1,6) < substr(dtos(setCurDate()),1,6)
    //рассчитаем период учета процентов
    //1. послед. день месяца, за который проведен платеж
    dto1:=dto+32
    dto1:=dto1-day(dto1)
    //2. добавим количество дней в след. месяце, в которые человек
    //еще может провести оплату без штрафа
    dto1:=dto1+aProcPeni()[kol][2]
    //3. получим период в днях для начисления штрафа на основную сумму
    period:=setCurDate() - dto1
    if period > 0
      sum:=int(round( getList[ind-1]:varget() * 100, 0))
      sum:=period * sum * peni / 10000
      sum:=round(sum,2)
      getList[ind+2]:varput( sum)
      getList[ind+2]:display()
      aProcPeni()[kol][3]:=period
    else
      getList[ind+2]:varput( 0.00)
      getList[ind+2]:display()
      aProcPeni()[kol][3]:=0
    endif
  else
      getList[ind+2]:varput( 0.00)
      getList[ind+2]:display()
      aProcPeni()[kol][3]:=0
  endif
else
  //
  // это моя вставка для работы с ЕСН
  //
  if peni == -1
    mons:=upper(alltrim(getList[ind]:varget()))
    mon:=padl(ascan( a_mon, {|x| upper(x) == mons}), 2, "0")
    yea:=padr(getList[ind+1]:varget(),4,"0")
    dto:=ctod(yea+"."+mon+".01")
//    aProcPeni()[kol][4]:=dto
  endif
endif
set(_SET_DATEFORMAT, old_format)
return .T.

static function ProvSum( n)
public getList
if empty(getList[n]:varget())
  return .F.
endif
return .T.

static function chkSum(g, ind, kol, aPF, mas3, mas4, nom_g)
local str1, i, jj
local rV := .T.
local pfTarif, pfMon, pfYear
local pfsOsn, pfsNom, pfsDate
local pfpVid, pfpNom, pfpDate
local pfnTip, pfnNom, pfnDate, pfnKv, pfnYear
local aMon :=aclone(monthNN())
local mas33, mas44

public getList
public provp1

mas33:=aClone(mas3)
mas44:=aClone(mas4)

aadd(aMon,"")

if g:varGet() <= 0.00
  g:varput(0.0)
  messageB( "Введите сумму платежа")
  rV := .F.
else
  for jj=1 to len(mas3)
    if mas33[jj] == substr(provp1,nom_g,2)
      if val(mas44[jj])#0
        if g:varGet()#val(mas44[jj])
          g:varput(val(mas44[jj]))
        endif
      endif
    endif
  next

  str1:=upper(alltrim(getList[ind - 2]:varGet()))

  i:=ascan(aProvName(), {|x| upper(alltrim(x)) == str1})
  if i > 0 .and. (aProvCode3()[i] == KOD_PF .or. aProvCode3()[i] == KOD_PFS .or.;
                  aProvCode3()[i] == KOD_PFP .or. aProvCode3()[i] == KOD_PFN)
    setkey( K_ESC, nil)
    if yesCheck
      str1:=aProvCode3()[i]
      if str1 == KOD_PF
        if !empty( aPF[kol]) .and. !empty(aPF[kol][1])
          pfTarif := aPF[kol][1][1]
          pfMon   := aPF[kol][1][2]
          pfYear  := aPF[kol][1][3]
        else
          pfTarif := 1
          pfMon   := 13//month(setCurDate())
          pfYear  := year(setCurDate())
        endif
        if getUP(10,8,;
            {{"Введите дополнительные данные о страховом взносе в ПФ"},;
             {"Тариф", block(pfTarif), aclone(aTarif)},;
             {"*"},;
             {"*за",   block(pfMon), aMon},;
             {"*",     block(pfYear), "9999"},;
             {"*год"}})
        endif
        if empty( aPF[kol])
          aPF[kol]    := {{pfTarif, pfMon, pfYear}, , , }
        else
          aPF[kol][1] := { pfTarif, pfMon, pfYear}
        endif
      endif
      if str1 == KOD_PFS
        if !empty( aPF[kol]) .and. !empty(aPF[kol][2])
          pfsOsn  := aPF[kol][2][1]
          pfsNom  := padr(aPF[kol][2][2], 20)
          pfsDate := aPF[kol][2][3]
        else
          pfsOsn  := 4
          pfsNom  := space(20)
          pfsDate := ctod("")
        endif
        if getUP(10,8,;
            {{"Введите дополнительные данные о штрафе в ПФ"},;
             {"Основание для перечисления", block(pfsOsn), aclone(aOsn)},;
             {"*"},;
             {"Номер акта докум. проверки", block(pfsNom)},;
             {"*От", block(pfsDate)}})
        endif
        pfsNom:=korrectNom( pfsNom)
        if empty( aPF[kol])
          aPF[kol]    := { , {pfsOsn, alltrim(pfsNom), pfsDate}, , }
        else
          aPF[kol][2] := {pfsOsn, alltrim(pfsNom), pfsDate}
        endif
      endif
      if str1 == KOD_PFP
        if !empty( aPF[kol]) .and. !empty(aPF[kol][3])
          pfpVid  := aPF[kol][3][1]
          pfpNom  := padr(aPF[kol][3][2], 20)
          pfpDate := aPF[kol][3][3]
        else
          pfpVid  := 3
          pfpNom  := space(20)
          pfpDate := ctod("")
        endif
        if getUP(10,8,;
            {{"Введите дополнительные данные о пенях в ПФ"},;
             {"Вид акта проверки  ", block(pfpVid), aclone(aVid)},;
             {"*"},;
             {"Номер акта проверки", block(pfpNom)},;
             {"*От", block(pfpDate)}})
        endif
        pfpNom:=korrectNom(pfpNom)
        if empty( aPF[kol])
          aPF[kol]    := { , , {pfpVid, alltrim(pfpNom), pfpDate}, }
        else
          aPF[kol][3] := {pfpVid, alltrim(pfpNom), pfpDate}
        endif
      endif
      if str1 == KOD_PFN
        if !empty( aPF[kol]) .and. !empty(aPF[kol][4])
          pfnTip  := aPF[kol][4][1]
          pfnNom  := padr(aPF[kol][4][2], 20)
          pfnDate := aPF[kol][4][3]
          pfnKv   := aPF[kol][4][4]
          pfnYear := aPF[kol][4][5]
        else
          pfnTip  := 4
          pfnNom  := space(20)
          pfnDate := ctod("")
          //if month(setCurDate()) < 4
          //  pfnKv := 1
          //elseif month(setCurDate()) >= 4 .and. month(setCurDate()) < 7
          //  pfnKv := 2
          //elseif month(setCurDate()) >= 7 .and. month(setCurDate()) < 10
          //  pfnKv := 3
          //else
          //  pfnKv := 4
          //endif
          //pfnYear := year(setCurDate())
          pfnKv   :=0
          pfnYear :=0
        endif
        if getUP(10,8,;
            {{"Введите дополнительные данные о недоимках в ПФ"},;
             {"Тип документа  ", block(pfnTip), aclone(aTip)},;
             {"*"},;
             {"Для докум.проверки/соглашения:"},;
             {"        номер", block(pfnNom)},;
             {"*дата", block(pfnDate)},;
             {"Для расчетной ведомости:"},;
             {"        номер квартала", block(pfnKv), "9"},;
             {"*г",   block(pfnYear), "9999"}})
        endif
        if pfnTip = 1 .or. pfnTip = 2
          pfnKv:=0
          pfnYear:=0
          pfnNom := korrectNom( pfnNom)
        else
          pfnNom:=""
          pfnDate:=ctod("")
          if pfnYear <= 1980 .or. pfnYear > year(setCurDate())
            pfnYear:=0
          endif
          if pfnKv > 4
            pfnKv = 0
          endif
        endif
        if empty( aPF[kol])
          aPF[kol]    := { , , , {pfnTip, alltrim(pfnNom), pfnDate, pfnKv, pfnYear}}
        else
          aPF[kol][4] := {pfnTip, alltrim(pfnNom), pfnDate, pfnKv, pfnYear}
        endif
      endif
    endif
    setkey( K_ESC, {|| esckey()})
  else
    kolPeni( ind, kol)
  endif
endif
return rV

autofunction setValue _provName  init {}
autofunction setValue _provCode3 init {}
autofunction setValue _provCode2 init {}
autofunction setValue _provCash  init {}
autofunction setValue _provTran  init {}
autofunction setValue _provOper  init {}
autofunction setValue _provBuff  init {}

autofunction setValue _prov2Code init {}
autofunction setValue _prov2Name init {}
autofunction setValue _prov2Buff init {}

autofunction setValue aProvName  init {}
autofunction setValue aProvCode3 init {}
autofunction setValue aProvCode2 init {}
autofunction setValue aProvCash  init {}
autofunction setValue aProvTran  init {}
autofunction setValue aProvOper  init {}
autofunction setValue aProvBuff  init {}

autofunction setValue sEsn_List init ","



static function aProvDocn( par)
static arr
if valType(par) = "C"
  arr:={ {"","",""}, {"","",""}, {"","",""}, {"","",""}, {"","",""} }
endif
return arr

static function aProcPeni( par)
static arr
if valType(par) = "C"
  arr:={ { 0, 0, 0, ctod(""), 0}, { 0, 0, 0, ctod(""), 0},;
         { 0, 0, 0, ctod(""), 0}, { 0, 0, 0, ctod(""), 0},;
         { 0, 0, 0, ctod(""), 0} }
endif
return arr

funcDef fillProvs local s := select(), opened := .T., used

if !empty( _provName()) then return NIL
_provName( {});_provCode3( {});_provCode2( {});_provCash( {});_provTran( {})
_provBuff( {})

used := baseUsed( provInfo())
if !used then opened := provOpen()

if used .OR. opened
   PROVIDER ->( baseSavePos())
   provider ->(dbsetorder(1))
   PROVIDER ->( netGoTop())
   while PROVIDER ->( !Eof())
     if PROVIDER ->code3 != "71"
        aAdd( _provName(),  PROVIDER ->name)
        aAdd( _provCode3(), PROVIDER ->code3)
        aAdd( _provCode2(), PROVIDER ->code2)
        aAdd( _provCash(),  PROVIDER ->accCash)
        aAdd( _provTran(),  PROVIDER ->accTran)
        aAdd( _provBuff(),  PROVIDER ->Buffer )
        aAdd( _provOper(),  PROVIDER -> roCash)
     endif
     skip 1 alias PROVIDER
   enddo
   PROVIDER ->( baseRestPos())
endif
if !used .AND. opened then provClose()

//
// поставщики
//
opened := .T.; used := baseUsed( prov2Info())
if !used then opened := prov2Open()
if used .OR. opened
   PROV2 ->( baseSavePos())
   PROV2 ->( netGoTop())
   while PROV2 ->( !Eof())
     aAdd( _prov2Name(), PROV2 ->name)
     aAdd( _prov2Code(), PROV2 ->code3)
     aAdd( _prov2Buff(), PROV2 ->buffer)
     if "ESN" $ upper( PROV2 ->buffer)
        sEsn_List( sEsn_List() + PROV2 ->code3 + ",")
     endif
     skip 1 alias PROV2
   enddo
   PROV2 ->( baseRestPos())
endif
if !used .AND. opened then prov2Close()
select ( s)

return NIL

procedure aFillProvs()
local i
*local isti

if empty( aProvName() )
  //добавим пустые значения, чтобы пользователь мог
  //пропустить второй и третий разовый платеж
  aAdd( aProvName(),  space(40))
  aAdd( aProvCode3(), space(2))
  aAdd( aProvCode2(), space(2))
  aAdd( aProvCash(),  space(20))
  aAdd( aProvTran(),  space(20))
  aAdd( aProvOper(),  "")
  aAdd( aProvBuff(),  space(30))

  for i:=1 to len( _provName())
    if _provCode2()[i] == KOD_RAZ
      if !empty( _provCash()[i]) .and. !empty( _provTran()[i])
        aAdd( aProvName(),  _provName()[i] )
        aAdd( aProvCode3(), _provCode3()[i])
        aAdd( aProvCode2(), _provCode2()[i])
        aAdd( aProvCash(),  _provCash()[i] )
        aAdd( aProvTran(),  _provTran()[i] )
        aAdd( aProvOper(),  _provOper()[i] )
        aAdd( aProvBuff(),  _provBuff()[i] )
      else
        dialog({"В справочнике не заполнен счет для наличного",;
                "или безналичного перечисления по услуге:    ",;
                _provName()[i],;
                "Платежи по этой услуге приниматься не будут "},;
               {"Ok"})
      endif
    endif
  next
endif
return
//
//поиск по коду услуги всех кодов и имен поставщиков
funcDef fillProvList with curCode2:C, arrName:AU, arrCode:AU ;
  local i, cnt, ptr
  arrName := {}; arrCode := {}
  cnt := len( _provName())
  for i := 1 to cnt
    if _provCode2()[ i] == curCode2
       ptr := aScan( _prov2Code(), {|x| x == _provCode3()[ i]})
       if ptr > 0
          aAdd( arrName, _prov2Name()[ ptr])
          aAdd( arrCode, _prov2Code()[ ptr])
       endif
    endif
  next
return NIL

function checkCode1( g, ndoc, aFam)
local rv:=.T., rv1:=.T., rv2:=.T., rv3:=.T., rv4:=.T.
local str:=upper(alltrim(g:varGet())), str2:="", str3, pic, lns, valid:={|| .T.}
local arr, i, ind
local get, cod2, cod3
local oldDoc:=aProvDocn()[ndoc][1]
local arr1:={ }, arr2:={ }, arr3:={ }
local str4:=space(20)
local fam1, fam2, fam3, adr1
local oldBlk  //25-01-2000
local zabAddPaym

if yesCheck = .F.
  return .T.
endif

if !empty(str) .and. len(str) > 2
  i  :=ascan( aProvName(), {|x| upper(alltrim(x)) == str})
  cod2:=aProvCode2()[i]
  cod3:=aProvCode3()[i]
  //messageB(cod2+cod3)
  //отдельно договор по Заботе+
  if i > 0 .or. cod3 == KOD_ZABOTA .or.;
        cod3 == KOD_SERVIS .or. cod3 == KOD_ZAB_1F
    if i > 0
      arr:=aclone( raionKmn( aProvCode3()[i]) )
    endif
    if len(arr) = 0 .and. (cod3 == KOD_ZABOTA .or.;
           cod3 == KOD_SERVIS .or. cod3 == KOD_ZAB_1F)
      arr:={ , 13, "Введите номер договора", "06"+confOtdNumb()+"XXXXXXXX", }
    endif
    get:={{"Введите идентификатор", block(str2), pic, , valid}}
    //доп проверка на лояльность - кроме кода услуги остальные элементы
    //массива могут быть не заполнены
    if len(arr) > 0 .and. !(empty(arr[2]) .and. empty(arr[3]) .and. empty(arr[4]))
      if !empty(arr[3])
        get[1][1]:=arr[3]
      endif
      if !empty(arr[4])
        get[1][1]+=" (шаблон "+arr[4]+")"
        pic:=arr[4]
        lns:=len(pic)
        str2:=""
        for i:=1 to len(arr[4])
          if at(substr(arr[4],i,1),"0123456789") > 0
            str2+=substr(arr[4],i,1)
          else
            str2+=" "
          endif
        next
        valid:={|g| if(len(alltrim(g:buffer)) = lns, .T., .F.)}
      elseif !empty(arr[2])
        lns:=arr[2]
        str2:=space(lns)
        valid:={|g| if(len(alltrim(g:buffer)) = lns, .T., .F.)}
      else
        lns:=20
        str2:=space(lns)
      endif
      //
      if arr[1] == KOD_PF .or. arr[1] == KOD_PFS .or.;
           arr[1] == KOD_PFP .or. arr[1] == KOD_PFN
        valid:={|g| if(len(alltrim(g:buffer)) = 9 .or.;
          (len(alltrim(g:buffer)) = 10 .and. left(alltrim(g:buffer),2) = "57") .or.;
          (len(alltrim(g:buffer)) = 11 .and. left(alltrim(g:buffer),3) = "057"),;
          .T., .F.)}
      endif
      //если уже введен договор ранее, то покажем его
      if !empty( oldDoc)
        str2:=padr(oldDoc, lns)
      endif
      get[1][3]:=pic
      get[1][5]:=valid
      oldBlk:=setkey( K_ESC, nil)  //25-01-2000
      if getUp(8,10, get)
        str2:=alltrim(str2)
        if !empty(arr[5])
          if ascan(arr[5], {|x| x == substr(str2,1,len(x)) } ) = 0
            messageB("Идентификатор (договор) не может быть произвольным")
            rv1:=.F.
          endif
        endif
        if !empty(arr[4])
          for i:=1 to len(arr[4])
            if at(substr(arr[4],i,1),"0123456789") > 0 .and.;
                 substr(arr[4],i,1) != substr(str2,i,1)
              messageB("Идентификатор (договор) должен соответствовать шаблону")
              rv:=.F.
              exit
            endif
          next
        endif
        if cod3 == KOD_ZABOTA .or. cod3 == KOD_SERVIS .or. cod3 == KOD_ZAB_1F
          rV3 := .T.; rV := .T.
          //
          // проверим наличие первого взноса
          //
          zabAddPaym := !canAdd1ZPay( Str2)
          if contSeekDocNumb( str2, cod3)
             CONTRACT ->( baseSavePos())
             CONTRACT ->( dbSetOrder( 2))
             if CONTRACT ->( dbSeek( str2)) .AND. CONTRACT ->status == 1
               if !zabAddPaym .AND. CONTRACT ->dateCur == setCurDate()
                  //
                  // первого взноса нет и поручение открыто сегодня
                  //
               else
                 if yesNo( { "По этому договору уже сделан взнос,",;
                             "вы хотите внести дополнительную оплату ?" })
                    rV := .T.
                 else
                   rV3 := .F.
                 endif
               endif
             else
               messageB( "Это поручение уже закрыто !!!")
               rV := .F.; rV3 := .F.
             endif
             CONTRACT ->( baseRestPos())
          else
            messageB("Сначала оформите долгосрочное поручение по товарам")
            rv3:=.F.; rV := .F.
          endif
        endif
        if cod3 == KOD_PF .or. cod3 == KOD_PFS .or.;
               cod3 == KOD_PFP .or. cod3 == KOD_PFN
          messageStartUp("Обязательно выберите правильный ответ")
          do while .T.
            i:=dialog({"Клиент платит за себя или за другого человека ?"},;
                      {"За себя","За другого человека"})
            if i > 0
              if i = 1
                aProvDocn()[ndoc][2]:="A"
              else
                aProvDocn()[ndoc][2]:="B"
                if empty(aFam[ndoc])
                  fam1:=space(20)
                  fam2:=space(20)
                  fam3:=space(20)
                  adr1:=space(50)
                else
                  fam1:=padr(aFam[ndoc][1],20)
                  fam2:=padr(aFam[ndoc][2],20)
                  fam3:=padr(aFam[ndoc][3],20)
                  adr1:=padr(aFam[ndoc][4],50)
                endif
                if getUp(10,8,;
                  {{"Введите данные человека, за которого проводится платеж"},;
                   {"Фамилия",  block(fam1) },;
                   {"*Имя",     block(fam2) },;
                   {"Отчество", block(fam3) },;
                   {"Адрес",    block(adr1) }})
                endif
                fam1:=upper(alltrim(fam1))
                fam2:=upper(alltrim(fam2))
                fam3:=upper(alltrim(fam3))
                adr1:=upper(alltrim(adr1))
                aFam[ndoc]:={ fam1, fam2, fam3, adr1}
              endif
              exit
            endif
          enddo
          messageExit()
        else
          aProvDocn()[ndoc][2]:=""
        endif
        rv:=(rv1 .and. rv2 .and. rv3 .and. rv4)
      else
        messageB("Идентификатор (договор) необходимо ввести с квитанции")
        rv:=.F.
      endif
      setkey( K_ESC, oldBlk)  //25-01-2000
    endif
  endif
else
  g:varput( space(40))
endif

//
// для начала сбросим информацию о пенях
//
aProvDocn()[ndoc][3]:=""
aProcPeni()[ndoc][1]:=0
aProcPeni()[ndoc][2]:=0
aProcPeni()[ndoc][3]:=0
aProcPeni()[ndoc][4]:=ctod("")


//поиск данных в базе assort
assort->(dbsetorder(1))
if !empty(str) .and. len(str) > 2 .and.;
     rv = .T. .and. assort->(dbseek( "12" + cod3))
  do while !assort->(eof()) .and. assort->code2 == cod2 .and. assort->code3 == cod3
    aadd(arr1, alltrim(assort->code))
    aadd(arr2, alltrim(assort->name))
    aadd(arr3, alltrim(assort->buffer))
    assort->(dbskip())
  enddo
  //данные о штрафе и днях
  aProvDocn()[ndoc][3]:=""
  aProcPeni()[ndoc][1]:=0
  aProcPeni()[ndoc][2]:=0
  aProcPeni()[ndoc][3]:=0
  aProcPeni()[ndoc][4]:=ctod("")
  ind:=0
  if len(arr1) = 1
    ind:=1
  elseif len(arr1) > 1
    messageStartUp("Обязательно выберите конкретную организацию")
    do while ind = 0
      ind:=popup( 10, 5, arr2)
    enddo
    messageExit()
  endif
  if ind > 0
    aProvDocn()[ndoc][3]:=arr1[ind]
    str4:="Орг. "+arr1[ind]
    if len(arr3[ind]) > 0
      i:=at(";", arr3[ind])
      if i > 0
        aProcPeni()[ndoc][1]:=val(substr(arr3[ind],1,i-1))
        aProcPeni()[ndoc][2]:=int(val(substr(arr3[ind],i+1)))
      else
        aProcPeni()[ndoc][1]:=val(arr3[ind])
      endif
    endif
  endif
endif
if len(str2) > 0 .and. rv = .T.
  str3:=padr("Идентификатор (договор) N "+str2,40)
  aProvDocn()[ndoc][1]:=str2
else
  str3:=space(40)
  aProvDocn()[ndoc][1]:=""
endif

@ g:row() + 2, g:col() say str3 + str4  color normalcolor( setGetColor())

return rv

//Чтение файла raion.rmn
function raionKmn( kod)
//формат подмассива:
//{код услуги, кол-во симв. в шаблоне, название идентификатора,
// шаблон(имеет приоритет над кол-вом симв.в шаблоне,
// строка символов, с которых может начинаться шаблон}
static arr
local str, str1, chr1, namef
local tek, i, j, max, priz
if valType(arr) = "U"
  arr:={ }
  namef:=newFPath( "raion.kmn" , alltrim(confPathOD()) )
  if file( namef)
    str:=memoread( namef)
  endif
  max:=mlcount(str, 80)
  tek:=0
  for i:=1 to max
    str1:=rtrim(memoline(str, 80, i))
    chr1:=lower(left(str1,1))
    if len(chr1) = 1
      //это код поставщика
      if chr1 == "["
        str1:=substr(str1,2)
        j:=at("]",str1)
        if j > 1
          str1:=left(str1,j-1)
          //это не мусор
          if !empty(str1) .and. at(left(str1,1),"0123456789") > 0 .and.;
                       at(left(str1,1),"0123456789") > 0
            tek:=ascan(arr,{|x| x[1] == str1})
            if tek = 0
              aadd(arr,{str1, , , , "" })
              tek:=len(arr)
            endif
          endif
        endif
      elseif chr1 == "m"
        if tek > 0
          j:=val(substr(str1,2))
          if j > 0 .and. j < 16
            arr[tek][2] := j
          endif
        endif
      elseif chr1 == "n"
        if tek > 0
          j:=alltrim(substr(str1,2))
          arr[tek][3] := j
        endif
      elseif chr1 == "f"
        if tek > 0
          j:=alltrim(substr(str1,2))
          arr[tek][4] := j
        endif
      else
        if tek > 0
          priz:=.T.
          for j:=1 to len(str1)
            if at(substr(str1,j,1),"0123456789") = 0
              priz:=.F.
              exit
            endif
          next
          if priz = .T.
            if empty(arr[tek][5])
              arr[tek][5] := { str1 }
            else
              aadd(arr[tek][5], str1 )
            endif
          endif
        endif
      endif
    endif
  next
  //заменяем данные по ПФ
  j:=ascan(arr, {|x| x[1] == KOD_PF})
  if j > 0
    arr[j][2] := 11
    arr[j][3] := "Регистр. номер клиента в ПФ"
    arr[j][4] := "XXXXXXXXXXX"
  else
    aadd(arr, {KOD_PF, 11, "Регистр. номер клиента в ПФ", "XXXXXXXXXXX", ""})
  endif
  j:=ascan(arr, {|x| x[1] == KOD_PFS})
  if j > 0
    arr[j][2] := 11
    arr[j][3] := "Регистр. номер клиента в ПФ"
    arr[j][4] := "XXXXXXXXXXX"
  else
    aadd(arr, {KOD_PFS, 11, "Регистр. номер клиента в ПФ", "XXXXXXXXXXX", ""})
  endif
  j:=ascan(arr, {|x| x[1] == KOD_PFP})
  if j > 0
    arr[j][2] := 11
    arr[j][3] := "Регистр. номер клиента в ПФ"
    arr[j][4] := "XXXXXXXXXXX"
  else
    aadd(arr, {KOD_PFP, 11, "Регистр. номер клиента в ПФ", "XXXXXXXXXXX", ""})
  endif
  j:=ascan(arr, {|x| x[1] == KOD_PFN})
  if j > 0
    arr[j][2] := 11
    arr[j][3] := "Регистр. номер клиента в ПФ"
    arr[j][4] := "XXXXXXXXXXX"
  else
    aadd(arr, {KOD_PFN, 11, "Регистр. номер клиента в ПФ", "XXXXXXXXXXX", ""})
  endif
endif
j:=ascan(arr, {|x| x[1] == kod})
if j > 0
  return arr[j]
endif
return { }

function indTime( time)
return left(time,2)+substr(time,4,2)+right(time,2)

function indDate( date)
  local str :=substr(dtos( date),3,2)
  local str1:="123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  str+=substr(str1, month(date),1)
  str+=substr(str1, day(date),  1)
return str

procedure getVvod( get, mas1, mas2, a_mon, kol, masbuf, nom_get)
local str:="", key, i, ind, jj, nom_p, istina:=.F.
local arr
local mon, yea
local mas12, oldBlk
local masb,zapis

public vidat:=.F.
public askBefore 
public provp1
public getList
zapis:=0.0

masb:=aClone(masbuf)

mon:=month(setCurDate())
yea:=year(setCurDate())
mas12:=aclone(mas1)
for i:=2 to len(mas2)  //первый элемент пустой
  mas12[i]+=' ('+mas2[i]+')'
next
if(getprevalidate(get))
  get:setfocus()
  get:exitstate:=GE_NOEXIT
  get:changed:=.T.
  yesCheck:=.F.
  do while .T.
    key=inkey(0)
    if key = K_ENTER .or. key = K_ESC .or. key = K_DOWN .or. key = K_UP .or.;
       key = K_PGDN  .or. key = K_PGUP
      getapplykey(get,key)
    endif
    if (get:exitstate != GE_NOEXIT)
      if getpostvalidate(get) = .T.
        exit
      else
        get:exitstate:=GE_NOEXIT
        loop
      endif
    elseif key >= 32    //По горячим клавишам
      if at( chr(key), "0123456789") > 0 .OR. isAlpha( chr( key))
        str:=alltrim(get:varget())
        if len(str) = 0 .or. len(str) > 2
          str:=Upper( Chr( key))
          get:varput(padr(str,40))
          provp1:=stuff(provp1,nom_get,2,"00")
          zapis:=0
        elseif len(str) = 1
          str:=str+ Upper( chr(key))
          arr:={ }
          for i:=1 to len(mas2)
            if mas2[i] == str
              aadd( arr, alltrim(mas1[i]))
              zapis=val(masb[i])
            endif
          next
          if len(arr) = 0
            messageB("В справочнике отсутствует код организации "+str)
            provp1:=stuff(provp1,nom_get,2,"00")
            get:varput(" ",40)
            zapis:=0
          elseif len(arr) = 1
            if !clientIsPens().and.str="31" 
              messageB("Услуга доступна только для пенсионеров ")
              get:varput(" ",40)
              zapis:=0
            else
              istina:=.F.
              for jj=1 to 5 step 2
                nom_p=substr(provp1,jj,2)
                if nom_p="31"
                  istina=.T.
                  exit
                endif
              next
              if istina.and.str="31"
                messageB("Оплата за льготный проездной билет уже введена ")
                get:varput(" ",40)
                zapis:=0
              else
                if str<>"31"
                  get:varput(padr(arr[1],40))
                endif
                if askBefore.and.str="31" 
                  if yesNo( "Клиент уже получал карту. Выдавать повторно ?")
                    vidat:=.T.
                    get:varput(padr(arr[1],40))
                  else
                    vidat:=.F.
                    get:varput(" ",40)
                    zapis:=0
                  endif
                else
                  get:varput(padr(arr[1],40))
                endif
              endif
              provp1:=stuff(provp1,nom_get,2,str)
              if (!vidat.and.askBefore.and.str="31" )
                provp1:=stuff(provp1,nom_get,2,"00")
                zapis:=0
              endif
            endif
          else
            messageStartUp("Организации с кодом "+str+;
                           " соответствует несколько услуг")
            oldBlk:=setkey( K_ESC, nil)
            i:=popup(arr)
            setkey( K_ESC, oldBlk)
            messageExit()
            if i = 0
              get:varput(padr(" ",40))
            else
              get:varput(alltrim(arr[i]),40)
            endif
          endif
        endif
        yesCheck:=.T.
      elseif chr(key) = " "     //Список по пробелу
        oldBlk:=setkey( K_ESC, nil)
        i:=popup(mas12)
        setkey( K_ESC, oldBlk)
        if i = 0
          if len(alltrim(get:varget())) < 2
            get:varput(padr(" ",40))
            yesCheck:=.T.
          endif
          provp1:=stuff(provp1,nom_get,2,"00")
          zapis:=0
        else
          if !clientIsPens().and.mas2[i]="31" 
            messageB("Услуга доступна только для пенсионеров ")
            get:varput(space(40),40)
            zapis:=0
            provp1:=stuff(provp1,nom_get,2,"00")
          else
            istina:=.F.
            for jj=1 to 5 step 2
              nom_p=substr(provp1,jj,2)
              if nom_p="31"
                istina=.T.
                exit
              endif
            next
            if istina.and.mas2[i]="31"
              messageB("Оплата за льготный проездной билет уже введена ")
              get:varput(space(40),40)
              zapis:=0
            else
              if mas2[i]<>"31"
                get:varput(padr(mas1[i],40))
              endif
              if askBefore.and.mas2[i]="31" 
                if yesNo( "Клиент уже получал карту. Выдавать повторно ?")
                  vidat:=.T.
                  zapis:=val(masb[i])
                else
                  vidat:=.F.
                  get:varput(space(40),40)
                  zapis:=0
                endif
              else
                zapis:=val(masb[i])
              endif
              provp1:=stuff(provp1,nom_get,2,mas2[i])
              if !vidat.and.askBefore.and.mas2[i]="31" 
                provp1:=stuff(provp1,nom_get,2,"00")
                zapis:=0
                get:varput(space(40),40)
              else
                get:varput(padr(mas1[i],40))
              endif
            endif
          endif
        endif
        yesCheck:=.T.
      endif
      if yesCheck
        if zapis#0         //Вводим сумму из BUF
          getList[kol  ]:varput(zapis)
        else
          getList[kol  ]:varput(0.0)
        endif
        getList[kol+1]:varput( alltrim(a_mon[mon]))
        getList[kol+2]:varput( yea)
        getList[kol+3]:varput( 1)  // установим кол-во документов в 1
        getList[kol  ]:display()
        getList[kol+1]:display()
        getList[kol+2]:display()
        getList[kol+3]:display()
      endif
      get:killfocus()
      get:display()
      get:setfocus()
    endif
  enddo
  get:changed:=.T.
  get:killfocus()
endif
return

//печать перечня разовых платежей, которые принимаются на отделении
procedure _provPrint()
local arr, i
local arr1, arr2, arr3, arr4

aFillProvs()
if len( aProvName()) > 1
  arr1:=aClone( aProvCode3())
  arr2:=aClone( aProvName())
  arr3:=aClone( aProvCash())
  arr4:=aClone( aProvTran())
  arr:={"","",;
  "                       Организации, в адрес которых можно",;
  "                           принимать разовые платежи",;
  "",;
  "|-----------------------------------------------------------------------------|",;
  "| Код |    Наименование организации (платежа)    |Счета для нал.и безнал.спис.|",;
  "|-----|------------------------------------------|----------------------------|"}
  for i:=2 to len( aProvName())
    aadd(arr,"| "+padl( arr1[i],3)+" | "+padr( arr2[i],40)+" |нал."+;
             accNew20Space(arr3[i])+"|")
    aadd(arr,"|     |                                          |без."+;
             accNew20Space(arr4[i])+"|")
  next
  aadd(arr,"|-----------------------------------------------------------------------------|")
  aadd(arr,"")
  aadd(arr,"")
  printAorder( arr)
else
  messageB("Не определены организации для приема разовым платежам")
endif
return

static function dopOrd1( nd, dto, name, sch, sums, otdName, nazn, balKas, sims,;
                         otdName1, isPF)
local text, sum:=0, ssum, i, filName
nd:=padl(nd, 6, "0")
dto:=space(9)+padc(Rdate(dto), 24)
name:=padr(name, 47)
for i:=1 to len(sums)
  sum:=round(sum+sums[i],2)
next
otdName:=padr(otdName, 35)
nazn:=razbivka(nazn, {{60,"L"},{78,"L"},{78,"L"},{78,"L"},{78,"L"}})
ssum:=razbivka(rsumma(sum),{{60,"L"},{60,"L"},{60,"L"}})
sum :=padl(alltrim(mStr(sum)),15)
for i:=1 to 5
  if sums[i] != 0
    sums[i]:= padl(alltrim(mstr(sums[i])) , 13)
    sims[i]:= padl(sims[i],2,"0")
  else
    sums[i]:=space(13)
  endif
next
filName:=padr(confFilial(), 35)
otdName1:=padr(otdName1,24)
text:={;
"-------------------------------------------------------------------------------",;
"                            ОРДЕР   N  |    "+nd+"     |    | Форма  0482008  |",;
                  dto   +        "      -----------------    ------------------|",;
"                                                   ДЕБЕТ       СУММА          |",;
"От кого                                         ------------------------------|",;
                  name  +                      " |Сч. N "+balKas+"| "+sum+        " |",;
"                                                ------------|  Общая          |",;
"Банк получателя                                    КРЕДИТ   |                 |",;
       padr(confBankName(),35)    +"-------------------------|-----------------|",;
                  filName+         "|Код|                    | частные     |код|",;
"Получатель "+    otdName1+        "----|                    |-------------|---|",;
                  otdName+         "    |"+sch+             "|"+sums[1]+  "| "+sims[1]+"|",;
"----                                   |                    |"+sums[2]+  "| "+sims[2]+"|",;
"                                       |                    |"+sums[3]+  "| "+sims[3]+"|",;
"                                       |                    |"+sums[4]+  "| "+sims[4]+"|",;
"Код|                                   |NN сч.              |"+sums[5]+  "| "+sims[5]+"|",;
"------------------------------------------------------------|-----------------|",;
                  ssum[1]+                                  "|Вид. опер.|  09  |",;
                  ssum[2]+                                  "|----------|------|",;
                  ssum[3]+                                  "|Назн.плат.|      |",;
"Сумма прописью                                              |----------|------|",;
"Назначение взноса                                           |N гр.банка|      |",;
                  nazn[1]                                +  "-------------------",;
                  nazn[2]                                                      ,;
                  nazn[3]                                                      ,;
                  nazn[4]                                                      ,;
                  nazn[5]                                                      ,;
if(isPF,;
"                                 Бухгалтер                  Кассир",;
"Подпись вносителя                Операционист               Кассир"),;
"",;
"",;
"",;
"",;
"",;
""}
return text

function dopObv( nd, dto, name, sch, sums, otdName, nazn, balKas, sims,;
                 otdName1)
local text, sum:=0, ssum, i, filName
nd:=padl(nd, 6, "0")
dto:=space(16)+padc(Rdate(dto), 24)
name:=padr(name, 41)
for i:=1 to len(sums)
  sum:=round(sum+sums[i],2)
next
if empty(otdName1)
  otdName1:=confBankName()+" " //"АБ 'Петровский' "
endif
otdName:=padr(otdName1+otdName, 46)
//nazn:=razbivka(nazn, {{60,"L"},{60,"L"},{60,"L"},{60,"L"},{60,"L"}})
ssum:=razbivka(rsumma(sum),{{57,"L"},{78,"L"},{78,"L"}})
sum :=padl(alltrim(mStr(sum)),15)
for i:=1 to 5
  if sums[i] != 0
    sums[i]:= padl(alltrim(mstr(sums[i])) , 13)
    sims[i]:= padl(sims[i],2,"0")
  else
    sums[i]:=space(13)
  endif
next
filName:=padr(confBankName() + "," + rTrim( confFilial()), 41)
text:={;
"-------------------------------------------------------------------------------",;
"                        ОБЪЯВЛЕНИЕ  N  |    "+nd+"     | |        0402001     |",;
"                   на взнос наличными  ----------------- ---------------------|",;
                   dto +                "                 ---------------------|",;
"От кого "+        name+                      +  "Для зач.|"+sch+             "|",;
"                                                 на сч.N ---------------------|",;
"Банк получателя "+filName                              +"|   "+sum+        "  |",;
"Получатель "     +otdName+                              "| Сумма   цифрами    |",;
                   ssum[1]+                              "---------------------|",;
                   ssum[2]+                                                   "|",;
                   ssum[3]+                                                   "|",;
"Сумма прописью                                                                |",;
"Назначение взноса   "+padr(nazn,60)+                                           "|",;
"Подпись вносителя                       Бухгалтер                             |",;
"                                        Деньги принял кассир                  |",;
""}
return text

function dopKvi( nd, dto, name, sch, sums, otdName, nazn, balKas, sims,;
                 otdName1)
local text, sum:=0, ssum, i, filName
nd:=padl(nd, 6, "0")
dto:=space(16)+padc(Rdate(dto), 24)
name:=padr(name, 41)
for i:=1 to len(sums)
  sum:=round(sum+sums[i],2)
next
if empty(otdName1)
  otdName1:=confBankName()+" " //"АБ 'Петровский' "
endif
otdName:=padr(otdName1+otdName, 46)
//nazn:=razbivka(nazn, {{60,"L"},{60,"L"},{60,"L"},{60,"L"},{60,"L"}})
ssum:=razbivka(rsumma(sum),{{57,"L"},{78,"L"},{78,"L"}})
sum :=padl(alltrim(mStr(sum)),15)
for i:=1 to 5
  if sums[i] != 0
    sums[i]:= padl(alltrim(mstr(sums[i])) , 13)
    sims[i]:= padl(sims[i],2,"0")
  else
    sums[i]:=space(13)
  endif
next
filName:=padr( confBankName() + "," + rTrim( confFilial()), 41)
text:={;
"-------------------------------------------------------------------------------",;
"                        КВИТАНЦИЯ   N  |    "+nd+"     | |        0402001     |",;
"                                       ----------------- ---------------------|",;
                   dto +                "                 ---------------------|",;
"От кого "+        name+                      +  "Для зач.|"+sch+             "|",;
"                                                 на сч.N ---------------------|",;
"Банк получателя "+filName                              +"|   "+sum+        "  |",;
"Получатель "     +otdName+                              "| Сумма   цифрами    |",;
                   ssum[1]+                              "---------------------|",;
                   ssum[2]+                                                   "|",;
                   ssum[3]+                                                   "|",;
"Сумма прописью                                                                |",;
"Назначение взноса   "+     padr(nazn,60) +                                     "|",;
"Подпись вносителя                        Бухгалтер                            |",;
"                                         Деньги принял кассир                 |",;
""}
return text

static function dopOrd2( nd, dto, name, sch, sums, otdName, nazn, balKas, sims,;
                         otdName1)
local text, sum:=0, ssum, i, filName
nd:=padl(nd, 6, "0")
dto:=space(16)+padc(Rdate(dto), 24)
name:=padr(name, 47)
for i:=1 to len(sums)
  sum:=round(sum+sums[i],2)
next
otdName:=padr(otdName, 35)
nazn:=razbivka(nazn, {{60,"L"},{60,"L"},{60,"L"},{60,"L"},{60,"L"}})
ssum:=razbivka(rsumma(sum),{{60,"L"},{60,"L"},{60,"L"}})
sum :=padl(alltrim(mStr(sum)),15)
for i:=1 to 5
  if sums[i] != 0
    sums[i]:= padl(alltrim(mstr(sums[i])) , 13)
    sims[i]:= padl(sims[i],2,"0")
  else
    sums[i]:=space(13)
  endif
next
filName:=padr(confFilial(), 35)
otdName1:=padr(otdName1,24)
text:={;
"-------------------------------------------------------------------------------",;
"                            ОРДЕР   N  |     "+nd+"      | |      0402001     |",;
"                                       ------------------- -------------------|",;
                 dto +                  "                                      |",;
"                                                   ДЕБЕТ          СУММА       |",;
"От кого                                         ------------------------------|",;
                 name+                         " |Сч. N "+balKas+"| "+sum        +" |",;
"                                                ------------|     Общая       |",;
"                                                   КРЕДИТ   |                 |",;
"Банк получателя                    -------------------------|-----------------|",;
padr(confBankName()+", ",35)+      "|код|                    |    частные  |код|",;
padr(rTrim( confFilial()),35)+     "----|                    |-------------|---|",;
"Получатель "   +otdName1+         "    |"+sch+             "|"+sums[1]+  "| "+sims[1]+"|",;
                 otdName+          "    |                    |"+sums[2]+  "| "+sims[2]+"|",;
"                                       |                    |"+sums[3]+  "| "+sims[3]+"|",;
"                                       |                    |"+sums[4]+  "| "+sims[4]+"|",;
"                                       |NN сч.              |"+sums[5]+  "| "+sims[5]+"|",;
"------------------------------------------------------------|-----------------|",;
"Назначение взноса                                                                ",;
                 nazn[1]+                                   "                   ",;
                 nazn[2]+                                   "                   ",;
                 nazn[3]+                                   "                   ",;
                 nazn[4]+                                   "                   ",;
                 nazn[5]+                                   "                   ",;
"",;
"    Бухгалтер                        Кассир",;
"",;
"",;
"",;
"",;
""}
return text





function myMemWisc( schet, summa, about, schet2)
local text
local str1, i, max, old_format
local filName:=alltrim(confFilial())

if len(filName) > 30
  filName:=substr(filName,1,30)
endif
old_format:=set(_SET_DATEFORMAT, 'dd.mm.yyyy')
str1:=razbivka(about,{{ 60,"L"},{76,"L"},{76,"L"}})
text:={;
"",;
"                           "+confBankName()+" "+filName,;
"",;
"                                ПОРУЧЕНИЕ",;
"              на перечисление денежных средств по текущему счету",;
"",;
"Прошу перечислить с моего текущего счета N "+schet,;
cliFullName(),;
"----------------------------------------------------------------------------",;
"                              (ФИО вкладчика)",;
alltrim(mstr(summa)),;
"("+alltrim(padr(rSumma(summa),76))+")",;
"В "+confBankName()+" "+filName+" счет N "+schet2,;
"----------------------------------------------------------------------------",;
"                Куда: N р/с, наименование банка получателя, БИК",;
"Перечисление: "+str1[1],;
str1[2],;
str1[3],;
"----------------------------------------------------------------------------",;
"                           (За что, указать НДС)",;
"",;
"Дата: "+dtoc(setCurDate())+;
                "                           Вкладчик ________________________",;
"",;
"                                          Бухгалтер ________________________",;
"",;
""}
max:=len(text)
for i:=1 to max
  aadd(text, text[i])
next
set(_SET_DATEFORMAT, old_format)
return text

function myMemOrder(nd, dto, fio, sum, sch1, sch2, nazn)
local text
local filName:=padr(alltrim(confFilial()), 39)
local sum1, sum2

nd :=padl(nd,6,"0")
dto:=padc(rdate(dto),39)
fio:=razbivka(fio,{{ 29, "L"},{ 39, "L"}})
sum1:=mstr(sum,16)
sum2:=razbivka(rsumma(sum),{{44,"L"},{60,"L"}})
nazn:=razbivka(nazn,{{60,"L"},{60,"L"},{60,"L"}})
text:={;
"-------------------------------------------------------------------------------",;
"           МЕМОРИАЛЬНЫЙ ОРДЕР N        |      "+nd+"     |  |     0481008     |",;
              dto                     +"-------------------  ------------------|",;
"Плательщик: "+     fio[1]              +"       ДЕБЕТ            СУММА        |",;
                    fio[2]            +"---------------------------------------|",;
"Банк плательщика:                      |"+      sch1      +"|                 |",;
              padr(confBankName(),39) +"| сч. N              |"+    sum1    +" |",;
                    filName           +"|--------------------|                 |",;
"Получатель:                                  КРЕДИТ         |                 |",;
"ОПФР по СПб и Лен. области             |--------------------|-----------------|",;
"                                       |                    |                 |",;
"Банк получателя:                       |"+      sch2      +"|                 |",;
              padr(confBankName(),39) +"|                    |                 |",;
                    filName           +"|NN сч.              |                 |",;
"------------------------------------------------------------|-----------------|",;
"Сумма прописью: "+       sum2[1]                          +"|Вид. опер.|   09 |",;
                          sum2[2]                          +"|----------|------|",;
"                                                            |Назн.плат.|      |",;
"------------------------------------------------------------|----------|------|",;
"Назначение платежа                                          |Срок плат.|      |",;
                          nazn[1]                          +"|----------|------|",;
                          nazn[2]                          +"|Очер.плат.|      |",;
                          nazn[3]                          +"|----------|------|",;
" Приложение на      лист    Подписи                         |N гр.банка|      |",;
"--------------------------------------------------------------------------------",;
"",;
"",;
"",;
"",;
""}
return text

function korrectNom( str1)
local str2:="", i, priz:=.F.
for i:=1 to len(str1)
  if substr(str1,i,1) != ";"
    str2+=substr(str1,i,1)
  else
    priz:=.T.
  endif
next
if priz
  dialog({"В номере акта не допускается использование символа ';'",;
          "Номер "+str1+" заменен на "+str2},;
         {"Ok"})
endif
return str2

static procedure rHelp()
  local keyF1
  setkey( K_ESC, nil)
  keyF1:=setkey( K_F1, nil)
  message('Все и так достаточно просто')
  setkey( K_ESC, {|| esckey()})
  setkey( K_F1, keyF1)
return

static procedure pHelp()
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

static funcDef canAdd1ZPay with docN:C local rV := .T.
   messageStartUp( "Поиск первого взноса по договору " + docN)
   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 3))
   rV := canAddZPay( docN, opDecode( "2010") )
   if rV then rV := canAddZPay( docN, opDecode( "5010") )
   SUMS ->( baseRestPos())
   messageExit( 0)
return rV

static funcDef zabFirstD with docN:C local rV := .T.
  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 3))
  rV := CONTRACT ->( dbSeek( docN)) .AND. CONTRACT ->dateCur == setCurDate()
  CONTRACT ->( baseRestPos())
return rV

static funcDef canAddZPay with docN:C, oper:C local rV := .T.
   SUMS ->( dbSeek( oper, .T.))
   while SUMS ->typeOf == oper .AND. SUMS ->( !eof())
     if rTrim( SUMS ->buffer) == docN
       rV := .F.
       exit
     endif
     skip 1 alias SUMS
   enddo
return rV

//
// проверим, что поручение открыто в другой день
//
funcDef contZabOtherDate with docN:C local rV := .F.
  CONTRACT ->( baseSavePos())
  CONTRACT ->( dbSetOrder( 2))
  if CONTRACT ->( dbSeek( docN))
    rV := CONTRACT ->status == 1 .AND. CONTRACT ->dateCur < setCurDate()
  endif
  CONTRACT ->( baseRestPos())
return rV


static funcDef ecnNazn ;
   with code2:C, code3:C, summa:N, docN:C, aDate:D, nFlag:N ;
   local str := "", arr, sDate, s := select(), ptr
ptr := aTrimmedScan( _prov2Code(), code3)
if ptr > 0
  if ASSORT ->( dbSeek( code2 + code3))
    if nFlag == 0 
      sDate := right( DtoC( aDate), 7)
    else
      sDate := replicate( "_", 15 ) //nStr( year( aDate )) + " год"
    endIf
    arr := CtoA( ASSORT ->buffer, ";")
    str := "(" + rTrim( subStr( _prov2Buff()[ ptr], 5) ) + "), " + arr[ 3] + ":" + ;
           lTrim( mStr( summa)) + " в " + arr[ 4] + " за " + sDate  + ;
           if( empty( docN), "", " (N " + docN + ")") + "; "
  endif
endif
return str

static funcDef useInn with arr:A, cInn:C local rV := "", i
  for i := 1 to len( arr)
    if arr[ i] $ sESN_LIST()
      rV := "ИНН " + cInn
      exit
    endif
  next
return rV