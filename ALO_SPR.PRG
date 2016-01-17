//  
// 13.08.99 Введен вызов функции printWiscR Цейтлин П.М.
//  
*
* 27-01-2003 TMV
* Из списка убран запрос на возобновление выплат с НТПС - alo_sprNew()
*
* 19-08-2003 TMV
* Убрана проверка на наличие комплексного обслуживания для операции
*  <Справка о составе пенсии>
* Ф-ия alo_sprPrint()
*
* 28-08-2003 TMV
* При печати справки о составе пенсии в начале вставляются 4 строки
* Ф-ия alo_sprPrint()
*
* 15-03-2004 TMV
* Убрана возможность добавить новый запрос с помощью клавиши <INS>.
* Ф-ия alo_spr().
*
* 29-07-2004
* Изменить проверку на "карточность" счета
* Ф-ия alo_sprNew()
*

#include "common.ch"
#include "clippext.ch"
#define USL "73"
    
static arr_spr:=;
{{"Справка о составе пенсии                 ", "700" },;
 {"Справка о расчете пенсии                 ", "701" },;
 {"Справка о пенсионере                     ", "702" },;
 {"Стандартная выписка по счету             ", "703" },;
 {"Выписка по карточному счету              ", "704" },;
 {"Справка по коммунальным платежам         ", "705" },;
 {"Отчет по операциям с облигациями         ", "706" },;
 {"Выписка по счету депо                    ", "707" },;
 {"Справка об оплате пор.'Петроэлектосбыт'  ", "708" },;
 {"Справка о доходах                        ", "751" },;
 {"Запрос справки о составе пенсии          ", "800" },;
 {"Запрос справки о расчете пенсии          ", "801" },;
 {"Запрос справки о пенсионере              ", "802" },;
 {"Запрос стандартной выписки по счету      ", "803" },;
 {"Запрос выписки-отчета по карточному счету", "804" },;
 {"Запрос справки по комм. платежам         ", "805" },;
 {"Запрос отчета по операциям с облигациями ", "806" },;
 {"Запрос выписки по счету депо             ", "807" },;
 {"Запрос справки по пор.'Петроэлектросбыт' ", "808" },;
 {"Запрос на оформ.заявления о врем.отъезде ", "850" },;
 {"Запрос на возобновление выплаты с НТПС   ", "915" },;
 {"Выписка от кассы                         ", "9999"},;
 {"Выписка из архива                        ", "-1"  }}


static funcDef agentExclude local arr, i, ptr
  if confUseAgent()
    arr := { "800", "801", "802", "805", "806", "807", "808" }
    for i := 1 to len( arr)
      if ( ptr := aScan( arr_spr, {|x| x[ 2] == arr[ i]})) > 0
        aDel( arr_spr, ptr); aSize( arr_spr, len( arr_spr) - 1)
      endif
    next    
  endif
return NIL

procedure alo_spr( )
local old_area:=select()
local old_format:=(_SET_DATEFORMAT, "dd.mm.yyyy")
local kod:=clients->code
local a_schets:=accAllSchets( clients->code)
local i:=1, j, str_type:=""
local aTyp:={ }
local arrHelp:={"Помощь не готова"}
local aH:={{"Получение справок и оформление запросов"},;
           {"Клиент "+cliFullName()}}
local aC
local bH:={|| viewHelp(arrhelp)}
local bUp :={|| pred(@i)   }, bDn :={|| sled(@i,len(aTyp)) },;
      bTop:={|| i:=1, .T.  }, bBtm:={|| i:=len(aTyp), .T.}
local fk:={{},;
           {{"Запрос", {|| alo_sprNew( a_schets, aTyp, @i, kod,;
                                       newOutKod, .T., yesNtps)}}},;
           {},;
           {},;
           {{"Печать", {|| alo_sprPrint( aTyp, @i, newOutKod, .T.)}}},;
           {},;
           {},;
           {{"Удалить", {|| alo_sprDel( aTyp, @i)}}};
          }
local bApp, bDel
local ob
local newOutKod
local yesNtps
local gets, arrDepo, ptrDepo

if len(a_schets) > 0
  asort(a_schets, , ,{|x, y| x[1] > y[1]})
  yesNtps:=.F.
  for j:=1 to len(a_schets)
    if at(a_schets[j][4], "15 1  1  2  ") > 0
      yesNtps := .T.
    endif
  next
  for j:=1 to len(arr_spr)
    str_type+=arr_spr[j][2]+";"
  next
  readWiscZap ( @aTyp, a_schets, str_type)
  wisc->(basesavepos())
  mselect("WISC")
  newOutKod := readNewOutKod()
  // readWiscOutKod( newOutKod, @aTyp)
  readWiscOutKod( getSchetDepo(), @aTyp)

  if len(aTyp) = 0
    messageStartUp("На счета клиента еще не оформлены запросы. Введите первый запрос")
    alo_sprNew( a_schets, aTyp, 0, kod, newOutKod, .T., yesNtps)
    messageExit()
  endif
  if len(aTyp) > 0
    aC:={{"Счет/Клиент",         {|| retSch(aTyp[i])}},;
         {"Тип справки/запроса", {|| retSpr(aTyp[i][2])}},;
         {"Обработан",           {|| dtoc(aTyp[i][3])}}}
    i:=1
    ob:=browseStartUp( 1, 1, aH, aC, bUp, bDn, bTop, bBtm, bH, fk)
// Removed by TMV 15-03-2004
    bApp:={|| alo_sprNew( a_schets, aTyp, @i, kod, newOutKod, .T., yesNtps)}
    bDel:={|| alo_sprDel( aTyp, @i)}
// Replaced by TMV 15-03-2004
    browseProc( 1, 1, ob, aH, aC, , , , fk)
    browseExit()
  endif

wisc->(baserestpos())
dbselectarea( old_area)
set(_SET_DATEFORMAT, old_format)

else
  messageB("Клиент не имеет счетов на данном отделении")
endif

return

static funcDef retSch with arr:A local rV := "По клиенту"
do case
   case arr[ 2] $ "706,806"
     rV := arr[ 1]
   case len( arr[ 1]) > 0
     rV := accNew20Space( arr[ 1])
endcase
return padR( rV, 25)

static function retSpr( typ)
local i:=ascan(arr_spr,{|x| x[2] == typ})
if i > 0
  return arr_spr[i][1]
endif
return space(35)

static procedure viewhelp( arr)
helpHandler( arr)
return

static procedure alo_sprNew( a_schets, aTyp, i, kod, newOutKod,;
                             yesBrowse, yesNtps)
local aName:={ }, aCode:={ }, aSch:={ }, aOwn:={ }, j, j1, tip, sch, dto1, dto2
local aPor:={ }, aPrr:={ }, aIdn:={ }, aNtps:={ }, aNtpsSch:={ }
local ob
local priz, schet, kol, buf, por, ident, pic
local txt:={ }
local str1
local aDto1, aDto2
local gets, arrDepo, ptrDepo, rV := .F.
local arr

for j:=1 to len(arr_spr)
  if ( val(arr_spr[j][2]) >= 800 .and. val(arr_spr[j][2]) < 900 ) .OR. ;
     val( arr_Spr[ j, 2]) = -1
// Removed by TMV  27-01-2003
    aadd(aName, arr_spr[j][1])
    aadd(aCode, arr_spr[j][2])
  endif
next
for j:=1 to len(a_schets)
  aadd(aSch, accNew20Space(a_schets[j][1]))
  if yesNtps .and. at(a_schets[j][4],"15 1  1  2  ") > 0
    aadd( aNtps, accNew20Space(a_schets[j][1]))
    aadd( aNtpsSch, a_schets[j][1])
  endif
next

tip:=1
sch:=1
dto1:=dto2:=setCurDate()
do while .T.
  priz:=.F.
  aSch:={ }
  aOwn:={ }
  aPrr:={ }
  aPor:={ }
  aIdn:={ }
  aDto1:={ }
  aDto2:={ }
  sch:=1
  schet:=""
  dto1:=dto2:=setCurDate()
  if yesBrowse
    messageStartUp("Выберите запрос")
    tip:=popup(8,10,aName)
    messageExit()
    exit if tip <= 0

    //
    // 13.08.99 Вставлен вызов выписки из архива
    //
    if aCode[ tip] == "-1" // выписка из архива
       printWiscR()
       exit
    endif

  else
    tip:=ascan( aCode, {|x| x == "850"})
  endif
  if tip = 0
    exit
  elseif at(aCode[tip],"806,807") > 0
    if len( newOutKod) > 0
      if aCode[tip] == "806"
        arrDepo := getSchetDepo()
        ptrDepo := 1
        dto1    := dto1 - 91
        gets    := {{"Запросить выписку с", block(dto1)},;
                    {"*по",                 block(dto2)}}
        if len( arrDepo) > 1 
          aAdd( gets, { "Счет депо", block( ptrDepo), aClone( arrDepo)})
        endif
        if getUp(8,10, gets)
          if len( arrDepo) > 1 then newOutKod := arrDepo[ ptrDepo]          
          priz:=.T.
        endif
      else
        priz:=.T.
      endif
    else
      messageB("На отделении отсутствует информация о покупке клиентом облигаций")
      priz:=.F.
    endif
  //выписки по счетам
  elseif at(aCode[tip],"803,804") > 0
    for j:=1 to len(a_schets)
// CARDNEW  TMV 15-08-2002
// Изменить проверку на "карточность" счета
// Replaced by TMV 29-07-2004
      if aCode[tip] == "803" .or. _isCard(a_schets[j][1])
        aadd(aSch, accNew20Space(a_schets[j][1])+" открыт "+dtoc(a_schets[j][2]))
        aadd(aOwn, a_schets[j][1])
      endif
    next
    if len(aSch) > 0
      sch:=1
      dto1:=dto1 - 91
      if getUp(8,10,{{"Выберите счет",       block(sch), aSch},;
                     {"*"},;
                     {"Запросить выписку с", block(dto1)},;
                     {"*по",                 block(dto2)}})
        priz:=.T.
        schet:=aOwn[sch]
        //проверим, по НТПС заказать выписку можно только при наличии
        //запроса на возобновления операций по счету
        for j:=1 to len(a_schets)
          if a_schets[j][1] == schet
            if at(a_schets[j][4], "15 1  1  2  ") > 0
              if ascan( aTyp, {|x| x[1] == schet .and. x[2] == "915"}) = 0
                dialog({"Выписку по НТПС можно запросить только",;
                        "после оформления запроса на возобновление",;
                        "операций по этому счету"},;
                       {"Ok"})
                priz :=.F.
                schet:=""
              endif
              if priz .and. dto2 < setCurDate()
                dialog({"Выписка по НТПС должна оформляться по текущую",;
                        "дату включительно. Оформите запрос заново,",;
                        "согласившись с предложенной датой окончания"},;
                       {"Ok"})
                priz :=.F.
                schet:=""
              endif
            endif
            exit
          endif
        next
      endif
    else
      messageB(if(aCode[tip] == "803", "У клиента отсутствуют счета",;
                         "У клиента отсутствуют карточные счета"))
    endif
  elseif at(aCode[tip],"800,801,802") > 0
    //проверим возможность запроса справки о расчете пенсии
    priz:=.T.
    if aCode[tip] == '801'
      if setupRead( "MENUW", @str1)
        if len(str1) > 0 .and. left(str1,1) == '1'
          messageB('Заказ справки о расчете пенсии временно запрещен')
          priz:=.F.
        endif
      endif
    endif
    if priz
      //поиск пенсионных счетов
      for j:=1 to len(a_schets)
        if at(a_schets[j][4],"15 1  1  1  ") > 0
          aadd(aSch, accNew20Space(a_schets[j][1]))
          aadd(aOwn, a_schets[j][1])
        endif
      next
      if len(aSch) > 0
        priz:=.T.
        schet:=aOwn[1]
      else
        messageB("У клиента отсутствуют текущие пенсионные счета")
        priz:=.F.
      endif
    endif
  elseif aCode[tip] == "805"
    //выберем по клиенту все оформленные долгосрочные по телефонам и ВЦКП
    //if contOpen()
      //соберем все действующие договора
      contract->(basesavepos())
      contract->(dbsetorder(1))
      contract->(dbseek(kod))
      do while !contract->(eof()) .and. contract->code == kod
        if (contract->code2 == "01" .and. contract->code3 == "01") .or.;
               contract->code2 == "02"
          aadd(aSch, accNew20Space(contract->debetacc))
          aadd(aOwn, contract->debetacc)
          aadd(aPrr, "Поручение "+if(contract->code2 == "02",;
                     " тел. ", " ВЦКП ")+contract->intdoc+;
                     if(contract->code2 == "02", " тел.   "+contract->docnumb,;
                       " идент. "+contract->docnumb))
          aadd(aIdn, alltrim(contract->intdoc))
          aadd(aPor, alltrim(contract->docnumb))
        endif
        contract->(dbskip())
      enddo
      contract->(baserestpos())
      //contClose()
      j1:=1
      aadd(aPrr, "Получить справку по закрытому договору ВЦКП        ")
      aadd(aPrr, "Получить справку по закрытому договору по телефонам")
      //выберем поручение
      if getUp(8,10,{{"Выберите договор", block(j1), aPrr}})
        if j1 < len(aPrr) - 1
          schet:=aOwn[j1]
          ident:=aIdn[j1]
          por  :=aPor[j1]
          priz:=.T.
        //ввод вручную
        else
          //поиск пенсионных счетов
          aSch:={ }
          aOwn:={ }
          for j:=1 to len(a_schets)
            if at(a_schets[j][4],"15 1  1  1  ,15 1  5  1  ") > 0
              aadd(aSch, accNew20Space(a_schets[j][1]))
              aadd(aOwn, a_schets[j][1])
            endif
          next
          if len(a_schets) > 0
            if j1 != len(aPrr)
              j:=1
              por:=space(9)
              ident:="01"+confOtdCode()+"      "
              pic  :="01"+confOtdCode()+"XXXXXX"
              if getup(8,10,{{"Счет, с которого проводилась оплата", block(j), aSch},;
                             {"Номер договора                     ", block(ident), pic},;
                             {"Идентификатор клиента в ВЦКП       ", block(por)}})
                if len(alltrim(ident)) = 12 .and. len(alltrim(por)) = 9
                  priz:=.T.
                  schet:=aOwn[j]
                else
                  messageB("Неверно введен номер договора или идентификатор")
                endif
              endif
            else
              j:=1
              por:=space(7)
              ident:="02"+confOtdCode()+"      "
              pic  :="02"+confOtdCode()+"XXXXXX"
              if getup(8,10,{{"Счет, с которого проводилась оплата", block(j), aSch},;
                             {"Номер договора                     ", block(ident), pic},;
                             {"Телефон клиента                    ", block(por)}})
                if len(alltrim(ident)) = 12 .and. len(alltrim(por)) = 7
                  priz:=.T.
                  schet:=aOwn[j]
                else
                  messageB("Неверно введен номер договора или телефон")
                endif
              endif
            endif
          else
            messageB("У клиента отсутстствуют текущие счета")
          endif
        endif
        if priz = .T.
          dto1:=dto1-91
          if getup(10,25, {{"Запросить справку за период"},;
                           {"с",  block(dto1)},;
                           {"*по", block(dto2)}})
          else
            priz:=.F.
          endif
        endif
      endif
    //endif
  elseif aCode[tip] == "850"
    //поиск пенсионных счетов
    for j:=1 to len(a_schets)
      if at(a_schets[j][4],"15 1  1  1  ") > 0
        schet:=a_schets[j][1]
        priz:=.T.
        exit
      endif
    next
    if priz = .T.
      priz:=.F.
      dto1:=ctod("")
      kol :=0
      dto1 := getNeopDate( aTyp, schet)
      do while .T.
        if getUp(14,5,;
          {{"           Заявление о временном отъезде пенсионера"},;
           {""},;
           {padr(cliFullName(),63)},;
           {padl("Адрес: "+cliFullAddress(), 63)},;
           {replicate("─",63)},;
           {"На срок (мес.) : ",       block(kol),"99"},;
           {"*ИЛИ         По дату : ", block(dto1)} ,;
           {""},;
           {"Должно быть корректно заполнено одно из двух полей ввода"}})
          if kol > 0
            dto1:=ADDMONTH(setCurDate(),kol)
          elseif !empty(dto1) .and. dto1 > setCurDate()
            dto1:=dto1
          else
            messageB("Должно быть корректно заполнено одно из двух полей ввода")
            LOOP
          endif
          if YesNo({"Сведения о неоплате по пенсионеру",;
                    "не будут переданы до "+dtoc(dto1),;
                    "",;
                    "Принять заявление ?" })
            //if openPrn()
              if confUseAgent()
                arr := {{ "@bankname", confBankName()},;
                        { "@filial",   confFilial()  },;
                        { "@otd",      confOtdNumb() },;
                        { "@fio",      cliFullName() },;
                        { "@curdate",  setCurDate()  },;
                        { "@d1",       setCurDate()  },;
                        { "@d2",       dto1          },;
                        { "@schet",    schet         }}
                 txt := ExtractStrOpen( "neop")
                 txt := makeZ( txt, arr)
                 txt := txt + endl() + endl() + txt
                 txt := CtoA( txt, endl())
              else
              for j:=1 to 2
                  aadd(txt,"")
                  aadd(txt,"")
                  aadd(txt,"                                             Начальнику")
                  aadd(txt,"                                    Почтово-Банковского Отделения")
                  aadd(txt,"                                               N "+confOtdNumb())
                  aadd(txt,"")
                  aadd(txt,"                       З А Я В Л Е Н И Е")
                  aadd(txt,"")
                  aadd(txt,"   Я, "+cliFullName()+" личный номер ________, в связи")
                  aadd(txt,"с планируемым отсутствием с "+dtoc(setCurDate())+" по "+;
                                        dtoc(dto1)+" прошу перечислять пенсию")
                  aadd(txt,"(пособие) на мой текущий счет N "+schet)
                  aadd(txt,"")
                  aadd(txt,"   "+dtoc(SetCurDate())+"              ______________________ подпись")
                  aadd(txt,"                           ______________________ расшифровка")
              next
              endif
              printAOrder( txt)
            //  closePrn()
            //endif
            priz:=.T.
            exit
          endif
        else
          exit
        endif
      enddo
    endif
  elseif aCode[tip] == "915"
    j:=0
    if len(aNtps) = 1
      if yesno("Возобновлять операции по счету "+aNtps[1]+" ?")
        j:=1
      endif
    elseif len(aNtps) > 1
      messageStartUp("Выберите счет, операции по которому будут возобновлены")
      j:=popup(10,30,aNtps)
      messageExit()
    endif
    if j > 0
      priz := .T.
      schet:= aNtpsSch[j]
      if ascan( aTyp, {|x| x[1] == schet .and. x[2] == "915"}) > 0
        messageB("Уже оформлен запрос на возобновление операций по счету "+;
                 schet)
        priz := .F.
        schet:= ""
      endif
    endif
  elseif aCode[tip] == "808"
    priz:=.F.
      //соберем все договора
      contract->(basesavepos())
      contract->(dbsetorder(1))
      contract->(dbseek(kod))
      do while !contract->(eof()) .and. contract->code == kod
        if contract->code2 == "04" .and. contract->code3 == "80"
          aadd(aSch, accNew20Space(contract->debetacc))
          aadd(aOwn, contract->debetacc)
          aadd(aPrr, "Поручение 'Петроэлектросбыт' N "+;
                     +alltrim(contract->intdoc)+;
                     " (абонент "+alltrim(contract->docnumb)+")")
          aadd(aIdn, alltrim(contract->intdoc))
          aadd(aPor, alltrim(contract->docnumb))
          aadd(aDto1, contract->dateBeg)
          aadd(aDto2, contract->dateEnd)
        endif
        contract->(dbskip())
      enddo
      contract->(baserestpos())
      if len(aPor) > 0
        j1:=1
        if getUp(10, 8,{{"Выберите договор c ЗАО 'Петроэлектросбыт'"},;
                        {"", block(j1), aPrr}})
          if aDto1[j1] > setCurDate()
            messageB("Выбранное поручение начнет действовать только с "+;
                      dtoc(aDto1[j1]))
          else
            schet:=aOwn[j1]
            dto1:=aDto1[j1]
            dto2:=aDto2[j1]
            if dto2 > setCurDate()
              dto2:=setCurDate()
            endif
            if getUp(10,8,{{"Поручение с ЗАО 'Петроэлектросбыт' N "+;
                            aIdn[j1]+" (абонент "+aPor[j1]+")"},;
                           {"Период действия   с "+dtoc(aDto1[j1])+;
                            " по  "+dtoc(aDto2[j1])},;
                           {"Запросить справку с", block(dto1)},;
                           {"*по",                 block(dto2)}})
              priz:=.T.
              if priz .and.;
                 (dto1 < aDto1[j1] .or. dto1 > aDto2[j1] .or.;
                  dto2 < aDto1[j1] .or. dto2 > aDto2[j1])
                messageB('Запросить данные по поручению N '+aIdn[j1]+;
                         ' можно за '+dtoc(Adto1[j1])+' - '+;
                         dtoc(aDto2[j1]))
                priz:=.F.
              endif
              if priz .and.;
                 (dto1 > setCurDate() .or. dto2 > setCurDate())
                messageB('Нельзя запрашивать справку за будущий период')
                priz:=.F.
              endif
            endif
          endif
        endif
      else
        message("Данные о договорах с 'Петроэлектросбытом' отсутствуют")
      endif
  endif
  if priz = .T. .or. !yesBrowse
    exit
  endif
enddo
if priz = .T. .and. at(aCode[tip],"803,804,805,806,808") > 0 .and.;
     dto1 > dto2
  messageB('Вы неправильно ввели период получения справки '+;
           dtoc(dto1)+' - '+dtoc(dto2))
  priz:=.F.
endif
if priz = .T.
  //распишем содержимое буфера
  if aCode[tip] == "850"
    buf:=alltrim( kod)+";"+dtoc(dto1)+";"
  elseif at(aCode[tip],"803,804") > 0
    buf:= schet+";"+dtoc(dto1)+";"+dtoc(dto2)+";"
  elseif at(aCode[tip],"800,801,802") > 0
    buf:=alltrim( kod)+";"
  elseif aCode[tip] == "805"
    buf:=ident+";"+por+";"+dtoc(dto1)+";"+dtoc(dto2)+";"
  elseif aCode[tip] == "806"
    buf:=dtoc(dto1)+";"+dtoc(dto2)
  elseif aCode[tip] == "807"
    buf:=""
  elseif aCode[tip] == "915"
    buf:=schet
  elseif aCode[tip] == "808"
    buf:=aIdn[j1]+';'+aPor[j1]+';'+dtoc(dto1)+';'+dtoc(dto2)+';'
  endif

  j:=0
  if aCode[tip] != "805"
    if at(aCode[tip], "806,807") > 0
      j:=ascan(aTyp, {|x| x[2] == aCode[tip]})
      //
      // учитываем и счет депо
      //
      if aCode[tip] == "806" 
        j:=ascan(aTyp, {|x| allTrim( x[ 1]) == allTrim( newOutKod) .AND. x[ 2] == aCode[ tip]})
      endif
    else
      j:=ascan(aTyp, {|x| x[1] == schet .and. x[2] == aCode[tip]})
    endif
  endif
  if j = 0
    wisc->(dbappend())
    rV := .T.
    if neterr() = .F.
      if at(aCode[tip],"806,807") > 0
        wisc->schet := newOutKod
      else
        wisc->schet := schet
      endif
      wisc->type  := aCode[tip]
      wisc->buffer:= buf
      wisc->(netunlock())
      if at(aCode[tip],"806,807") > 0
       if aCode[tip] == "806"
         aadd( aTyp, { newOutKod, alltrim(wisc->type), ctod(""), wisc->(recno()) })
       else
         aadd( aTyp, { "", alltrim(wisc->type), ctod(""), wisc->(recno()) })
       endif
      else
        aadd( aTyp, { wisc->schet, alltrim(wisc->type),;
                      ctod(""), wisc->(recno()) })
      endif
      //если это не первый запрос
      if i > 0 .and. yesBrowse
        i:=len(aTyp)
        ob:=getCurOB()
        ob:refreshAll()
        do while !ob:stabilize()
        enddo
      endif
    endif
  else
    if yesno({if(at(aCode[tip], "806,807") > 0,;
              "На клиента",;
              "На счет "+accNew20Space(schet) ),;
              "уже оформлен "+aName[tip],;
              "Заменить данные в запросе ?"})
      wisc->(dbgoto(aTyp[j][4]))
      if wisc->(mrlock())
        if at(aCode[tip],"806,807") > 0
          wisc->schet := newOutKod
        else
          wisc->schet := schet
        endif
        wisc->type  := aCode[tip]
        wisc->buffer:= buf
        wisc->(netunlock())
        i:=j
        if at(aCode[tip],"806,807") > 0
          if aCode[tip] == "807" then aTyp[i][1]  := ""
        else
          // aTyp[i][1]  := a_schets[sch][1] Бей Пенса
          aTyp[ i][ 1] := schet
        endif
        aTyp[i][2]  := aCode[tip]
        aTyp[i][3]  := ctod("")
        if yesBrowse
          ob:=getCurOB()
          ob:refreshAll()
          do while !ob:stabilize()
          enddo
        endif
      endif
    endif
  endif
endif
if yesBrowse
  clear typeahead
endif
return

static procedure alo_sprDel(aTyp, i)
local newI
local ob:=getCurOb()
local schet, priz:=.T.

if (val(aTyp[i][2]) >= 800 .and. val(aTyp[i][2]) < 900) .or.;
        aTyp[i][2] == "915"
  if yesno({"Вы действительно хотите удалить",;
            retSpr(aTyp[i][2])})
    if aTyp[i][2] == "915"
      schet:=aTyp[i][1]
      if ascan( aTyp, {|x| x[1] == schet .and.;
                           val(x[2]) >= 800 .and. val(x[2]) < 900}) > 0
        priz:=.F.
        dialog({"Перед удалением запроса на возобновление операций по НТПС",;
                "необходимо удалить все запросы на получение выписок по",;
                "этому счету /"+schet+"/"},;
               {"Ok"})
      endif
    endif
    if priz
      wisc->(dbgoto(aTyp[i][4]))
      if wisc->(mdelete())
        wisc->(netunlock())
        if len(aTyp) = 1
          keyboard chr(27)
          return
        elseif i == len(aTyp)
          asize(aTyp, i-1)
          i--
        else
          aDel(aTyp, i)
          asize(aTyp,len(aTyp)-1)
        endif
        ob:refreshAll()
        do while !ob:stabilize()
        enddo
      endif
    endif
  endif
else
  messageB("Вы можете удалять только запросы")
endif

clear typeahead
return

function sumForPeriod( dtoPeriod, buf)
local sum1:=0
local aSum:={ }
local ind
local str1

  //messageB('Buf = '+buf)
  //messageB('dtoPeriod = '+str(dtoPeriod,6))
  if len(buf) > 0
    if at('-', buf) > 0
      do while .T.
        if len(buf) = 0 
          exit
        endif
        ind := at(';', buf)
        if ind > 0
          str1:=substr(buf,1,ind-1)
          buf :=substr(buf,ind+1,len(buf)-ind)
        else
          str1:=buf
          buf :=""
        endif
        ind:=at('-',str1)
        if ind > 1 .and. ind < len(str1)
          aadd( aSum, { val(substr(str1,1,ind-1)), val(substr(str1,ind+1,len(str1)-ind)) })
        endif
      enddo
      asort( aSum, , , {|x,y| x[1] < y[1]})
      ind:=len(aSum)
      if ind > 0
        if dtoPeriod > aSum[ind][1]
          sum1:=aSum[ind][2]
        else
          for ind:=len(aSum) to 1 step -1
            if dtoPEriod <= aSum[ind][1]
              sum1:=aSum[ind][2]
            endif
          next
        endif
      endif
    else
      sum1 := val(buf)
    endif
  endif
return sum1

procedure alo_sprPrint(aTyp, i, newOutKod, yesBrowse)
local str, cHistory, strTemp, strBnk, StrSchet, StrClient, schet
local ind, arr:={ }, max
local Ob:=getCurOb()
local prin:=.F., kodYsl, sumYsl
local arr1:={ }, arr2:={ }
local old_format, dtoRepFirst, dtoPeriod, strRepFirst, strRepEnd

if (val(aTyp[i][2]) >= 700 .and. val(aTyp[i][2]) < 800) .or. Atyp[i][2] == "9999"
  if aTyp[i][4] = 0
    if aTyp[i][2] == "751"
      incServSprav()
    endif
  else
    wisc->(basesavepos())
    wisc->(dbgoto( aTyp[i][4]))
    str:=alltrim(wisc->buffer)
    if aTyp[ i, 2] == "703"
      old_format := set(_SET_DATEFORMAT,'dd.mm.yyyy')
      strRepFirst:= substr(str,1,10)
      dtoRepFirst:= ctod(strRepFirst)
      dtoPeriod  := setCurDate() - dtoRepFirst
      strRepEnd  := ""
      ind := rat("ОСТАТОК НА СЧЕТЕ", upper(str))
      if ind > 20
        strRepEnd  := substr(str, ind-11,10)
      else
        ind := rat("ИСХОДЯЩИЙ ОСТАТОК", upper(str))
        if ind > 20
          strRepEnd  := substr(str, ind-11,10)
        endif
      endif
      set(_SET_DATEFORMAT, old_format)
    endif
    max:=mlcount(str, 100)
    if max > 500
      max:=500
      messageB("Сообщение превышает 500 строк - будет распечатана только часть")
    endif
    if max > 0
// Add by TMV 28-08-2003
      if aTyp[i][2] == "700"
			for ind = 1 to 4
				aAdd( arr, "" )
			next
		endif
// Add by TMV 28-08-2003  -- End
      if aTyp[ i, 2] == "703"
         schet := aTyp[ i, 1]
         strTemp  := rTrim( confFilial()) + " отд. N " + rTrim( confOtdNumb())
         strBnk   := ' '+confBankName()+' ' //' АБ "Петровский" '
         strSchet := "Лицевой счет "+left(schet,5)+' '+substr(schet,6,3)+' '+;
                     substr(schet,9,1)+' '+substr(schet,10,4)+' '+right(schet,7)
         strClient:= cliFullName()
         //
         aadd( arr, replicate( "=", 76))
         aadd( arr, "Выписка по видам операций" +;
                    Space( 76 - 25 - len( strTemp)-len( strBnk)) +;
                    strBnk + strTemp)
         aadd( arr, "с " + strRepFirst +;
                    if( len(strRepEnd) > 0, " по " + strRepEnd, ""))
         aadd( arr, strClient + Space(76-len( strSchet)-len(strClient)) +;
                    strSchet)
         aadd( arr, replicate( "-", 76))
         aadd( arr, "   Дата    " + padR( "Вид операции", 29) +;
                    "     Остаток  Расход(Дт)  Приход(Кт)")
      elseif aTyp[i][2] == "9999"
        editMemo( getHistory( aTyp[ i][ 1]))
        if yesNo( "Печатать выписку ?") then printHistory( aTyp[i][1])
      endif
      if aTyp[i][2] != "9999"
        if aTyp[i][2] == "706"
          //сначала заголовок
          //
          // убрано 15.06.2001
          //
          //aadd(arr, confBankName() ) //'АБ "Петровский"')
          //aadd(arr, 'Депозитарий')
          aadd(arr, '')
          aadd(arr, padc("ОТЧЕТ", 80))
          aadd(arr, padc("О совершении операций с государственными "+;
                         "сберегательными именными облигациями", 80))
          aadd(arr, padc("Санкт-Петербурга", 80))
          aadd(arr, "")
          aadd(arr, "Владелец счета: "+cliFullName())
          aadd(arr, padr("Документ: "+cliDocum(),80))
          //
          // внесены изменения 18.12.2000
          //
          // aadd(arr, "Счет депо: "+newOutKod)
          aAdd( arr, "Счет депо: " + WISC ->schet)
          aadd(arr, replicate('-',80))
        endif
        for ind:=1 to max
          aadd(arr, rtrim(memoline( str, 100, ind)))
        next
        if aTyp[i][2] == "706"
          aadd(arr, replicate('-',80))
          aadd(arr, "Дата: "+dtoc(setCurDate()))
          aadd(arr, "Ответственный исполнитель        _____________/_____________/")
          aadd(arr, "                                    подпись       фамилия")
          //
          // убрано 15.06.2001
          //
          aadd(arr, "                               Штамп отделения")
        endif
        aadd(arr, "")
        //
        prin:=.T.
        //найдем ТПС клиента
        accPrizList("15 1  1  1", @arr1, @arr2)
        //надо проверять наличие поручения на комплексное обслуживание"
        if aTyp[i][2] == "700" .or. aTyp[i][2] == "701" .or.;
           (aTyp[i][2] == "703" .and. ascan( arr2, {|x| x[2] == aTyp[i][1]}) > 0)
          basepath( assInfo(), base_path())
          if baseMOpen( assInfo())
            if aTyp[i][2] == "700"
              kodYsl := "78"
            elseif aTyp[i][2] == "701"
              kodYsl := "79"
            elseif aTyp[i][2] == "703"
              kodYsl := "77"
            endif
            assort->(dbsetorder(1))
            //messageB(kodYsl)
            if assort->(dbseek( "76"+kodYsl))
              if kodYsl = "78" .or. kodYsl = "79"
                sumYsl := val(alltrim(assort->buffer))
              else
                sumYsl := sumForPeriod( dtoPeriod, alltrim(assort->buffer))
              endif
              if sumYsl < 0.005
                sumYsl := 0
              endif
            else
              sumYsl := 0
            endif
            //messageB(str(sumYsl,15,2))
            baseMClose( assInfo())
            if sumYsl > 0 
              editMemo( AtoC( arr, endl()))
              if !readKompl() ;
				  		.Or. kodYsl == "78"   // Add by TMV  19-08-2003
// Add by TMV  19-08-2003
              	 if kodYsl == "78"
	                ind:=dialog({"Клиент должен оплатить ",;
                             "стоимость услуги по предоставлению выписки или справки",;
                             "Плата за услугу составляет   "+mstr(sumYsl)+" руб."},;
                            { "Наличными", "Безналично", "Отказаться от операции"})
					 else
// Add by TMV  19-08-2003 -- End
	                ind:=dialog({"Клиент не заключил договор на комплексное обслуживание",;
                             "текущих пенсионных счетов, поэтому он должен оплатить ",;
                             "стоимость услуги по предоставлению выписки или справки",;
                             "Плата за услугу составляет   "+mstr(sumYsl)+" руб."},;
                            { "Наличными", "Безналично", "Отказаться от операции"})
					 endif   // Add by TMV  19-08-2003
                //if aTyp[i][2] == "700"
                //  kodYsl := "78"
                //  sumYsl := 2.00
                //elseif aTyp[i][2] == "701"
                //  kodYsl := "79"
                //  sumYsl := 2.00
                //elseif aTyp[i][2] == "703"
                //  kodYsl := "77"
                //  sumYsl := 1.50
                //endif
                if ind = 1
                  //
                  // Эта функция вызывается, чтобы не писать слово ТРАНЗИТНЫЙ
                  // в строке "Для начисления на счет "
                  //
                  useZabota( .T.)
                  prin:=paymCash( .T., , {{ kodYsl, sumYsl }})
                  useZabota( .F.)
                elseif ind = 2
                  ind:=ascan(arr2, {|x| x[2] == aTyp[i][1]})
                  if sumRest( arr2[ind][2], setCurDate()) >= sumYsl
                    prin:=paymCash( .F., arr2[ind][2], {{ kodYsl, sumYsl }})
                  else
                    if dialog({"На счете "+arr2[ind][2]+" недостаточно средств"},;
                              {"Оплатить наличными", "Отказаться от операции"}) = 1
                      prin:=paymCash( .T., , {{ kodYsl, sumYsl }})
                    else
                      prin:=.F.
                    endif
                  endif
                else
                  prin:=.F.
                endif
              endif
            endif
          else
            prin:=.F.
          endif
        endif
        if prin
          if yesNo( "Печатать документ ?") then printAorder( arr)
        endif
      endif
    else
      messageB("С филиала пришло пустое сообщение")
    endif
    if prin
      if wisc->type != "9999" .and. yesno("Удалить сообщение из списка ?")
        wisc->(dbgoto( aTyp[i][4]))
        if wisc->(mrlock())
          wisc->schet := ""
          wisc->type  := ""
          wisc->buffer:= ""
          wisc->(netunlock())
        endif
        //если в массиве было только одно сообщение
        if len(aTyp) = 1
          if yesBrowse
            keyboard chr(27)
          else
            aTyp:={ }
          endif
          return
        elseif i == len(aTyp)
          asize(aTyp, i-1)
          i--
        else
          aDel(aTyp, i)
          asize(aTyp,len(aTyp)-1)
        endif
        if yesBrowse
          ob:refreshAll()
          do while !ob:stabilize()
          enddo
        endif
      endif
    endif
    wisc->(baserestpos())
  endif
else
  messageB("Нельзя распечатать запрос")
endif
return

static function readNewOutKod()
local newOutKod := ""
  //найдем внешний код по облигациям
  contract->(basesavepos())
  contract->(dbsetorder(1))
  if contract->(dbseek( clients->code + USL))
    newOutKod := alltrim(contract->docnumb)
  else
    newOutKod := ""
  endif
  contract->(baserestpos())
return newOutKod


//
// А вот это уже новое 13.12.2000
//
static function getSchetDepo()
local newOutKod := "", arr := {}, i := 1
  //найдем внешний код по облигациям
  contract->(basesavepos())
  contract->(dbsetorder(1))
  if contract->(dbseek( clients->code + USL))
    //
    // 13.12.2000 Изменения в программе по просьбе Слепцова
    //
    while CONTRACT ->code == CLIENTS ->code .AND. CONTRACT ->code2 == USL .AND. ;
      CONTRACT ->( !eof())
      if aTrimmedScan( arr, CONTRACT ->docNumb) == 0
        aAdd( arr, CONTRACT ->docNumb)
      endif
      skip 1 alias CONTRACT
    enddo
  endif
  contract->(baserestpos())
return arr


static function readKompl()
local rv:=.F.
local dto1 := setCurDate()

  //первое число текущего месяца
  dto1:=dto1 - day(dto1) + 1
  contract->(basesavepos())
  contract->(dbsetorder(1))
  contract->(dbseek( clients->code+"76" ))
  do while !contract->(eof()) .and. contract->code == clients->code .and.;
              contract->code2 == "76" .and. contract->code3 == "76"
    if contract->dateBeg <= setCurDate() .and.;
         contract->dateEnd >= dto1 .and.;
           contract->status = 1
      rv:=.T.
      exit
    endif
    contract->(dbskip())
  enddo
  contract->(baserestpos())
return rv

//
// 13.12.2000 Внесены изменения
//
// 18.12.2000 В массив первым номером добавлен счет депо
//
static procedure readWiscOutKod( arr, aTyp)
local i, newOutKod, curSchet
if !empty( arr)
  //добавляем справки и запросы по счету депо
  for i := 1 to len( arr)
    newOutKod := arr[ i]
    if wisc->(dbseek( padr( newOutKod, 20) ))
      while !wisc->(eof()) .and. alltrim(wisc->schet) == allTrim( newOutKod)
        curSchet := ""
        if allTrim( wisc ->type) $ "706,806" then curSchet := allTrim( wisc ->schet)
        aadd(aTyp, { curSchet, alltrim(wisc->type), ctod(""), wisc->(recno()) })
        wisc->(dbskip())
      enddo
    endif
  next
endif
return

procedure readWiscZap ( aTyp, a_schets, str_type)
local i, priz
  wisc->(basesavepos())
  wisc->(dbsetorder(1))
  for i:=1 to len(a_schets)
    //собираем справки, которые пришли на отделение
    //и запросы, которые будут отправлены с отделения
    wisc->(dbseek(a_schets[i][1]+arr_spr[1][2], .T.))
    do while !wisc->(eof()) .and. wisc->schet == a_schets[i][1]
      if at(alltrim(wisc->type), str_type) > 0
        //не показываем выписки по счету НТПС
        priz:=.T.
        if !empty(a_schets[i][4]) .and.;
            at(a_schets[i][4], "15 1  1  2  ") > 0 .and.;
             (wisc->type == "9999" .or. wisc->type == "703")
          priz:=.F.
        endif
        if priz
          aadd(aTyp, { wisc->schet, alltrim(wisc->type),;
                       ctod(""), wisc->(recno()) })
        endif
      endif
      wisc->(dbskip())
    enddo
  next
  //всегда добавляем справку о доходах
  i:=ascan(arr_spr, {|x| x[2] == "751"})
  if i > 0
    aadd(aTyp, { "", "751", ctod(""), 0 })
  endif
  wisc->(baserestpos())
return

procedure alo_sprGive( tpSchet)
local str_type:="700;701;751"
local aTyp:={ }
local i, strKod

readWiscZap( aTyp, {{ tpSchet, , , }}, str_type)
if len( aTyp) > 0
  messageStartUp("Выберите справку, которую хотите получить")
  i:=popup(8,10,{"Справка о составе пенсии",;
                 "Справка о расчете пенсии",;
                 "Справка о доходах"})
  messageExit()
  if i > 0
    strKod:=substr(str_type, (i-1)*4+1, 3)
    i:=ascan(aTyp, {|x| x[2] == strKod})
    if i > 0
      alo_sprPrint( aTyp, i, "", .F.)
    else
      dialog({"Информация о данной справке не пришла с филиала или",;
              "она уже была распечатана ранее и удалена после печати"},;
             {"Ok"})
    endif
  endif
else
  messageB("Информация о справках не пришла с филиала")
endif
return

procedure alo_sprNeop( tpSchet, kod)
local str_type:="850;"
local aTyp:={ }

readWiscZap( aTyp, {{ tpSchet, , , }}, str_type)
alo_sprNew( {{ tpSchet, , ,"15 1  1  1"}}, aTyp, 0, kod, "", .F., .F.)

return


funcDef getNeopDate with arr:A, schet:C ;
  local i, rV := CtoD( ""), ar2, sDate, D, M, Y
  if empty( rV)
    sDate := "TT" + CLIENTS ->buffer 
    sDate := decodeFileName( sDate)[ 3]
    if !empty( sDate)
      if right( DtoC( sDate), 1) < "6" 
        D := Day( sDate)
        M := Month( sDate)
        Y := Year(  sDate) + 10
        sDate := CtoD( nStr( D) + "." + nStr( M) + "." + nStr( Y))
      endif
      rV := sDate
    endif
  endif     
  for i := 1 to len( arr)
   if arr[ i, 1] == schet .AND. arr[ i, 2] == "850"
     WISC ->( baseSavePos())
     WISC ->( dbGoTo( arr[ i, 4]))
     ar2 := CtoA( WISC ->buffer, ";")
     rV  := CtoD( ar2[ 2])
     WISC ->( baseRestPos())
   endif
  next
return rV