//16-06-97 В чек добавлена проверка на лимит подписи и регистрации
//18-06-97 Перед авторизацией добавлена проверка на лимит регистрации
//         При ошибочных responce-кодах выводится соответ. сообщение
//           (не прошла авторизация)
//         Отказался от формирования контрольной ленты
//           (файлы с расширением ".LEN")
//19-09-97 Из статистики сообщений исключаем итоговые суммы
//27-12-99 Изменена работа с массивом aTips
//04-09-2000 Изменено название банка
*
* 27-01-2003 TMV
* Заменено Петробанк на МДМ - chekOpenDay(), chekCloseDay(), chekAuto(),
*  chekOtkaz().
*

#include "common.ch"
#include "clippext.ch"
#include "inkey.ch"
#include "al_card.ch"
#define temp_posDefs;
  { "temp_pos.dbf", "temp_pos",;
 {{ "COMMENT",   "C", 20, 0},;
  { "DATE_TEMP", "D",  8, 0},;
  { "STR_TEMP",  "C", 30, 0},;
  { "NUM_TEMP",  "N", 15, 0}},;
 {{ "temp_pos.ntx", "comment"}}, 0, .F.}
#define POS "POS_DSK"
#define TOWN1

#define kplatDefs;
  { "kplatDbf.dbf", "kplat",;
 {{ "CODE",      "C",  2, 0},;
  { "CHEK",      "N",  6, 0},;
  { "SUMMA",     "N", 15, 2},;
  { "CARD",      "C", 38, 0},;
  { "TEK_TIME",  "C",  8, 0},;
  { "TEK_DATE",  "D",  8, 0},;
  { "FO_TIME",   "C",  8, 0},;
  { "FO_DATE",   "D",  8, 0},;
  { "KOD_ERR",   "C", 10, 0},;
  { "REZULT",    "C",  2, 0},;
  { "RESP_CODE", "C",  2, 0},;
  { "PACK_STATE","C",  1, 0},;
  { "SCHET",     "C", 20, 0},;
  { "AUTO_CODE", "C",  6, 0},;
  { "TRACE",     "C",  6, 0},;
  { "PAN",       "C", 19, 0},;
  { "CARD_TAG",  "C",  1, 0}},;
 { }, 0, .F.}

#define CR_LF chr(13)+chr(10)

#define CFUNC .F.

#define VERS "AL_CARD VER 2, 11.02.99"

static aTips:={ "BRANCH", "OUTLET", "STORE", "DRUGSTORE",;
                "DRUGSTORE", "CAFE", "EXCHANGE OFFICE",;
                "GROCERY SHOP", "HOTEL", "CASHIER OFFICE",;
                "SHOP", "RESTAURANT", "SUPERMARKET"}
static kodTips:="1234ACEGHKMRY"
static aTiss:={ "EDC/MAESTRO", "EUROCHEQUE/CIRRUS", "EUROCHEQUE",;
                "EUROCARD/MASTERCARD", "VISA", "VISA/ELECTRON",;
                "PLUS"}

procedure cardWorkDop( priz, priz1)
local sum, kolZ, kod
local a_in, i, priz3:="0", priz4:="0"

if priz == 2
  if priz1 == "nal" .or. priz1 == "zabota"
    sum:=0 / 100
    if getUp(10,10,{{"Введите сумму к выдаче", block(sum), "999999.99"},;
                 {"*руб."}} ) .and. sum > 0

      a_in:={{ sum, if( priz1== "nal", NalKod, ZabKod), "010000", "810", 10}}
    else
      return
    endif
  elseif priz1 == "cred"
    sum:=0 / 100
    if getUp(10,10,{{"Введите сумму взноса", block(sum), "999999.99"},;
                   {"*руб."}} ) .and. sum > 0
      a_in:={{ sum, CredKod, "200000", "810", 10}}
      priz3:="1"
      priz4:="1"
    else
      return
    endif
  else
    kolZ=1
    a_in:={ }
    if getUp(10,10,{{"Введите количество запросов", block(kolZ)}}) .and.;
                            kolZ >= 1
      for i:=1 to kolZ
        sum:=0 / 100
        kod:=0
        if getUp(10,10,{{"Сумма", block(sum), "999999.99"},;
                        {"*руб."},;
                        {"Код  ", block(kod), "99"}} )
          aadd(a_in,{ sum, padl(kod,2,"0"), "000000", "810", 10})
        else
          return
        endif
      next
    else
      return
    endif
  endif
endif

cardWork( priz, priz1, a_in, "0", , priz4, priz3)

return

procedure cardWorkProv()
local priz:=0
local sum, auto_code, trace, pan, nal, card_tag

if kplatOpen()
  kplat->(dbgobottom())
  //последняя операция была связана с оплатой и т.п.
  if !empty(kplat->code) .and.;
      ( at(kplat->code,NalKod+ZabKod+CredKod+BlockSchKod) > 0 .or.;
           (kplat->code >= "00" .and. kplat->code <= "99"))
    if kplat->rezult == "00" .and. kplat->resp_code == "00" .and.;
         kplat->pack_state == "1"
      sum :=kplat->summa
      if kplat->(fcount()) >= 14
        auto_code := kplat->auto_code
      else
        auto_code := ""
      endif
      if kplat->(fcount()) >= 15
        trace := kplat->trace
      else
        trace := ""
      endif
      if kplat->(fcount()) >= 16
        pan := kplat->pan
      else
        pan := ""
      endif
      if kplat->(fcount()) >= 16
        card_tag := kplat->card_tag
      else
        card_tag := ""
      endif
      if kplat->code = NalKod
        nal:=1
      else
        nal:=0
      endif
      priz:=dialog({"Проверьте данные последней операции",;
                    "Сумма транзакции:"+mstr(kplat->summa),;
                    "Код авторизации: "+;
                    if(kplat->(fieldpos("AUTO_CODE")) > 0, kplat->auto_code, "")},;
                   {"Провести отмену последней транзакции",;
                    "Отказаться от отмены"})
    else
      dialog({"Последняя операция не была завершена",;
              "из-за ошибок связи.Отмена невозможна"},;
             {"Ok"})
    endif
  else
    dialog({"Последняя операция не была связана с оплатой,",;
            "выдачей наличных и т.п. Отмена невозможна"},;
           {"Ok"})
  endif
  kplatClose()
endif
if priz = 1
  cardWork( 7, , , "0", , "0", "0", , ,;
            {sum, auto_code, trace, pan, nal, card_tag})
endif
return
/*
procedure cardWorkPart()
local str1:=space(8)
local str2:=space(6)
local str3:=space(20)
local i:=1
local sum:=0 / 100
local mon:={"январь","февраль","март","апрель","май","июнь",;
            "июль","август","сентябрь","октябрь","ноябрь","декабрь"}
local a_in
if getUp(10,8,{{"Введите данные с чека"},;
               {"Терминал, на котором была проведена операция", block(str1)},;
               {"Код авторизации отменяемой операции         ", block(str2)},;
               {"Месяц, когда была проведена операция        ", block(i), mon},;
               {"Номер карты (смотри на чеке)     ", block(str3)},;
               {"Сумма, которую необходимо вернуть", block(sum)}}) .and. sum > 0
  str1:=padl(alltrim(str1),8,"0")
  str2:=padl(alltrim(str2),6,"0")
  str3:=padr(alltrim(str3),20)
  a_in:={{ sum, RepKod, "900000", "810", 10}}
  sum :=int(round(sum * 100,0))
  cardWork( 8, , a_in, "0", , "0", "0", str3,;
            str1+padl(sum,12,"0")+"900000"+str2+padl(i,2,"0"))

endif
return
*/
procedure cardWork( priz, priz1, a_in, prizNoKey, kod, priz4, priz3,;
                    dopStr1, dopStr2, aMas)
local a_out
local pack_state
local prizOper
local a_text, tekRow, maxRow
local tipReg    //16-06-97
local newChek, kol, prizChek
local temp_pos:=temp_posDefs
local schet, schets:={ }, schet13
local pri1, sum1, sum2, nd
local fio, adr, fios, adrs, sums
local str1, str2

if len( posPath( 1)) = 0
  return
endif

monSavePos()

//поиск открытых карточных счетов при блокировке
if priz = 6
#ifdef TOWN
  acc->(dbsetorder(4))
  acc->(dbseek(kod))
  do while !acc->(eof()) .and. acc->kli_kod == kod
// CARDNEW  TMV 15-08-2002
// Изменить проверку на "карточность" счета
    if at(acc->bal, "42308,42608") > 0 .and. empty(acc->dat_clos)
      aadd( schets, acc->bal+acc->key+acc->client)
    endif
    acc->(dbskip())
  enddo
  if len(schets) = 0
    messageB("У клиента отсутствуют действующие карточные счета")
    return
  elseif len(schets) = 1
    schet := accNew200(schets[1])
  else
    schet  := numBQmas( 10, 8, schets,;
              "Выберите счет, остаток на котором будет заблокирован")
    if empty(schet)
      return
    endif
  endif
  schet13:= left(schet,5)+substr(schet,9,1)+right(schet,7)
#else

#endif
endif

//при отмене сначала печатается чек, и только потом проводится операция
if priz = 7
  do while .T.
    newChek := temp_posR( "НОМЕР ЧЕКА", "N") + 1
    temp_posW( "НОМЕР ЧЕКА", , , newChek)
    //сам заполняю массив a_out
    a_out:={ }
    a_out:={ {temp_posR( "НОМЕР ТЕРМИНАЛА", "C")} }
    aadd( a_out[1], "00")
    str1:=time()
    str2:=left(str1,2)+substr(str1,4,2)+right(str1,2)
    aadd( a_out[1], substr(dtos(setCurDate()),3,6)+str2)
    aadd( a_out[1], temp_posR( "T1_LOCATION", "C"))
    aadd( a_out[1], temp_posR( "T2_LOCATION", "C"))
    aadd( a_out[1], temp_posR( "T3_LOCATION", "C"))
    aadd( a_out[1], temp_posR( "CITY", "C"))
    aadd( a_out[1], temp_posR( "COUNTRY", "C"))
    aadd( a_out[1], temp_posR( "ADDRESS", "C"))
    aadd( a_out[1], space(24))
    aadd( a_out[1], space(24))
    aadd( a_out[1], "1")
    pack_state:=12
    chekAuto( priz, "ret", a_out, , pack_state, @tipReg, newChek, aMas)
    nd:=dialog({"Проверьте данные предыдущей авторизации"},;
               {"Чек распечатан","Повторить печать чека","Отказаться от операции"})
    if nd = 1 .or. nd = 0
      exit
    elseif nd = 3
      return
    endif
  enddo
endif

if priz = 1
  if temp_posR( "НОМЕР ДНЯ", "N") = 1
    dialog({"День уже закрыт. Воспользуйтесь пунктом",;
            "<Печать последнего чека>"},;
           {"Ok"})
    return
  endif
endif

//создание входного файла
if createInPos( priz, a_in, prizNoKey, schet, priz4, priz3, dopStr1, dopStr2)
  ferase( posPath() + "out.pos")
if CFUNC = .F.
#ifdef TOWN
  runExe( exe_Path(), "pc_pos.exe", "", "Запуск PC_POS.EXE", "Yes")
#else
  runExe( "pc_pos.exe")
#endif
endif
  //чтение данных из выходного файла
  a_out:=readOutPos( priz)
  //в выходной структуре что-то есть
  if len(a_out) > 0
    //для разных типов структур найдем номер признака отправки пакета
    //и занесем данные в протокол
    do case
      case priz = 0
        pack_state:=14
        if a_out[1][2] == "00" .and. a_out[1][pack_state] == "1"
          basepath( temp_pos, posPath())
          if baseMOpen( temp_pos) = .T.
            //пропишем номер открытого дня и признак, что день еще открыт
            temp_posW( "НОМЕР ДНЯ", padl( a_out[1][3], 6, "0"), , 0)
            //обнулим номер последнего чека
            temp_posW( "НОМЕР ЧЕКА", , , 0)
            //пропишем код закрытия
            temp_posW( "КОД ЗАКРЫТИЯ", padl(alltrim(a_out[1][15]), 6, "0"))
            //установим время для автоматич. закрытия
            temp_posW( "ЗАКР.ДНЯ:ПРОВЕРКА",;
                       temp_posR( "ЗАКР.ДНЯ:НАЧАЛО", "C"))
            temp_posW( "НОМЕР ТЕРМИНАЛА", padl( a_out[1][1], 8, "0"))
            temp_posW( "ОТКРЫТИЕ ДНЯ", padr( a_out[1][4], 12, "0"))
            temp_posW( "T1_LOCATION", alltrim( a_out[1][6]))
            temp_posW( "T2_LOCATION", alltrim( a_out[1][7]))
            temp_posW( "T3_LOCATION", alltrim( a_out[1][8]))
            temp_posW( "CITY",        alltrim( a_out[1][9]))
            temp_posW( "COUNTRY",     alltrim( a_out[1][10]))
            temp_posW( "ADDRESS",     alltrim( a_out[1][11]))
            baseMClose( temp_pos)
          endif
        endif
        PWrite_01( OpenDayKod,  pack_state, a_out, 4, "0",       priz)
      case priz = 1
        pack_state:=14
        if a_out[1][2] == "00" .and. a_out[1][pack_state] == "1"
          //укажем, что день закрыт
          temp_posW( "НОМЕР ДНЯ", , , 1)
        endif
        PWrite_01( CloseDayKod, pack_state, a_out, 4, prizNoKey, priz)
      case priz = 2
        pack_state := 24
        prizChek := .F.
        for kol:=1 to len(a_out)
          //проверка отправки пакета
          if a_out[kol][pack_state] == "1"
            //проверка результата операции
            if a_out[kol][2] == "00"
              //проверка responce-кода (ответ карточного центра)
              if a_out[kol][10] == "00"
                prizChek := .T.
                a_in[kol][5]:=0
              else
                a_in[kol][5]:=1
              endif
              //сброс флага блокировки суммы
              cardFlag( 0)
            else
              a_in[kol][5]:=2
              //если пакет отправлен, а ответ не получен,
              //то установим признак блокировки суммы текущего запроса
              //в центре авторизации
              cardFlag( 1)
            endif
          else
            a_in[kol][5]:=3
            //ничего не делаем с признаком блокировки
            //суммы из предыдущего запроса
          endif
        next
        //получим номер следующего чека,
        //если есть хоть один авторизованный запрос
        if prizChek = .T.
          newChek := temp_posR( "НОМЕР ЧЕКА", "N") + 1
          temp_posW( "НОМЕР ЧЕКА", , , newChek)
        else
          newChek := 0
        endif
        PWrite_01( '--',        pack_state, a_out, 5, "0",       priz,;
                   10,      , a_in, newChek)
      case priz = 4
        pack_state:= 4
        PWrite_01( CheckKod,    pack_state, a_out, 3, prizNoKey, priz)
      case priz = 6
        pack_state:=13
        PWrite_01( BlockSchKod, pack_state, a_out, 4, "0",       priz,;
                   5, schet, a_in)
      case priz = 7
        newChek := temp_posR( "НОМЕР ЧЕКА", "N") + 1
        temp_posW( "НОМЕР ЧЕКА", , , newChek)
        pack_state:=12
        PWrite_01( RetKod,      pack_state, a_out, 3, "0",       priz,;
                     ,      ,     , newChek, aMas)
      case priz = 8
        pack_state:=13
        PWrite_01( RepKod,      pack_state, a_out, 4, "0",       priz,;
                   5,      , a_in)
    endcase
    //обработка результатов из массива A_OUT
    prizOper:=0
    //это не запрос на авторизацию ?
    if priz != 2
      //пакет отправлен ?
      if a_out[1][pack_state] == "1"
        //результат равен "00" - все нормально
        if a_out[1][2] == "00"
          do case
            case priz = 0
              //открытие дня:
              //напечатаем чек на открытие дня
              a_text := chekOpenDay( a_out[1])
              if len(a_text) > 0
#ifdef TOWN
                if openPrn()
                  if out_print() = 2
                    ? chr(15)
                  else
                    ? chr(18)
                  endif
                  for tekRow:=1 to len(a_text)
                    ? a_text[ tekRow]
                  next
                  for tekRow:=1 to 17
                    ?
                  next
                  closePrn()
                endif
#else
                printAorder( a_text)
#endif
              endif
            case priz = 1
              chekCloseDay( a_out[1])

            case priz = 4

            //выдача наличными по заблокированной карте
            case priz = 6
              if a_out[1][2] == "00" .and. a_out[1][pack_state] == "1"
                sum1:=int(round(val(a_out[1][7]),0))
                if sum1 > 0
                  pri1:=.F.
                  do while .T.
                    sum2:=0.00
                    if getup(8,10,;
                      {{"Остаток на счете клиента"+mstr(round(sum1 / 100, 2))},;
                       {"Введите сумму выдачи    ", block( sum2)}})
                      sum2:=int(round(sum2 * 100, 0))
                      if sum2 > 0 .and. sum2 <= sum1
                        pri1:=.T.
                        exit
                      else
                        if dialog({"Вы неверно ввели сумму !!!",;
                                   "Если вы откажетесь от операции, то все-равно",;
                                   "сегодня уже не снимете деньги со счета"},;
                                  {"Вернуться к вводу суммы","Прервать операцию"}) != 1
                          exit
                        endif
                      endif
                    endif
                  enddo
                  //выдаем деньги
                  if pri1
#ifdef TOWN
                    fio:=""
                    adr:=""
                    if acc->bal+acc->key+acc->client == schet13 .and.;
                                 acc->kli_kod == fiz->kli_kod
                      adr:=fizStrAdres()
                    else
                      acc->(basesavepos())
                      fiz->(basesavepos())
                      acc->(dbsetorder(6))
                      fiz->(dbsetorder(4))
                      if acc->(dbseek( schet13)) .and. fiz->(dbseek(acc->kli_kod))
                        fio:=fiz_kliFName()
                        adr:=fizStrAdres()
                      endif
                      fiz->(baserestpos())
                      acc->(baserestpos())
                    endif
                    nd:=numord( setCurDate(), "out")
                    sums:=razbivka( rSumma(round(sum2 / 100,2)),;
                                    {{45,"L"},{45,"L"},{45,"L"}})
                    fios:=razbivka( fio,;
                                    {{38,"L"},{38,"L"}})
                    adrs:=razbivka( adr,;
                                    {{38,"L"},{38,"L"}})
                    readTemplate( base_path()+"template\al_card1.tpl", "",;
                                  {{  1, str(nd,6)},;
                                   {  2, rDate( setCurDate())},;
                                   {  3, fios[1]},;
                                   {  4, fios[2]},;
                                   {  5, adrs[1]},;
                                   {  6, adrs[2]},;
                                   {  7, schet},;
                                   {  8, alltrim( mstr( round(sum2 / 100,2)))},;
                                   {  9, sums[1]},;
                                   { 10, sums[2]},;
                                   { 11, sums[3]},;
                                   { 12, ""},;
                                   { 13, ""},;
                                   { 14, ""},;
                                   { 15, nameDe_f_aul()},;
                                   { 16, left(schKass(),5)}},;
                                  "al_card.txt" )
                    showPrintDocument( "","al_card.txt",80)
#else

#endif
                    if yesno("Клиент подписал документы ?")
#ifdef TOWN
                      qprovodArhkli( schet13, schKass(), 9, nd, sum2,;
                                     setCurDate(), SKO())
                      provodArhsal( schet13, -sum2, setCurDate())
#else

#endif
                    endif
                  endif
                else
                  messageB("На счете клиента РЕАЛЬНО нет средств")
                endif
              endif
            case priz = 7
              //pack_state:=12
              //chekAuto( priz, "ret", a_out, , pack_state, @tipReg, newChek, aMas)
          endcase
          //сброс флага блокировки предыдущей суммы, если запрос отправлен
          cardFlag( 0)
        else
          prizOper:=2
          //18-07-97 Надо установить флаг ошибки после неудачной проверки
          //         связи или после неудачного закрытия дня
          if priz = 1 .or. priz = 4
            cardFlag( 1)
          endif
        endif
      else
        prizOper :=3
      endif
    else
      //авторизация
      //16-06-97 анализируется тип чека
      chekAuto( priz, priz1, a_out, a_in, pack_state, @tipReg, newChek)
      //16-06-97 анализируется тип чека
      if tipReg == "B"
        dialog({"ВНИМАНИЕ !!!",;
                "Клиент превысил лимит подписи",;
                "Предложите ему расписаться на экземпляре чека,",;
                "который останется у операциониста"}, {"Ok"})
      elseif tipReg == "C"
        dialog({"ВНИМАНИЕ !!!",;
                "Клиент превысил лимит регистрации",;
                "Предложите ему разборчиво вписать фамилию, имя,",;
                "отчество и расписаться на экземпляре чека,",;
                "который останется у операциониста"}, {"Ok"})
      endif
    endif
  endif
endif

monRestPos()

return

//путь к настроечным файлам
function posPath( par1)
local str:=alltrim( getenv( POS ))
if len(str) > 2 .and. right(str,1) != "\"
  str += "\"
endif
if len(str) = 0
  if valType( par1) = "N"
    dialog({ "Не установлена переменная окружения "+POS,;
             "Работать с магнитными картами нельзя"},;
           { "Ok"})
  endif
endif
return str


function createInPos( priz, a_in, prizNoKey, schet, priz4, priz3, dopStr1, dopStr2)
local rv:=.F., str, i
local ff
local aReq:=aReqDefs, aReqLen:=0
local prizLReg, sumLPodp, sumLReg  //18-06-97
local oldx, oldy, oldc, oldcolor, scr00
local rvStr:="", lenSr
local prizNoVer:="2" //перешел на версию 2

if valType(prizNoKey) != "C"
  messageB("Неправильный параметр prizNoKey в процедуре createInPos()")
  return .F.
endif
ff:=fcreate( posPath() + "in.pos")
if ff > 0
  fwrite(ff, str(priz,1) + prizNoKey + priz3 + priz4 +prizNoVer+;
             replicate("0",15) + CR_LF)
  //для авторизации введем карту
  if priz = 2
    for i:=1 to len(aReq)
      aReqLen += aReq[i]
    next
    //str     := read_card()
    str:=replicate( '0', 37);
//  ДЛЯ ОТЛАДКИ
//    str:="05897970000003011=98055011494685599999"
//
    if len(str) > 0
      cardNewTek( str)
      //это не кредитная операция
      if priz4 == "0"
        /*
        //18-06-97 проверка на лимит регистрации
        sumLPodp := cardLimit( "lowlimit",  substr(str, 2, 37) )
        sumLReg  := cardLimit( "highlimit", substr(str, 2, 37) )
        prizLReg:=.F.
        //если лимит регистрации равен нулю, то данный лимит неограничен
        if sumLReg > 0
          for i:=1 to len(a_in)
            if a_in[i][1] * 100 >= sumLReg
              prizLReg := .T.
              exit
            endif
          next
          if prizLReg == .T.
            if dialog({"Внимание !!! Превышен лимит регистрации",;
                       "Предложите клиенту предъявить документ, удостоверяющий",;
                       "личность. В противном случае операция с картой не может",;
                       "быть проведена."},;
                      {"Клиент предъявил документ", "Отменить операцию"}) != 1
              return .F.
            endif
          endif
        endif
        */
      endif
      //занесем содержимое 2-й дорожки в файл
      //fwrite(ff, substr(str, 2, 37) + CR_LF)
      //fwrite(ff, padl(len(str),3,"0") + str + CR_LF)
      fwrite(ff, "000" + str + CR_LF)
      //занесем данные по платежам
      if valType(a_in) != "A"
        a_in:={ }
      endif
      if len(a_in) > 0
        for i:=1 to len(a_in)
          str := padl(int(round(a_in[i][1]*100,0)),aReq[1],"0")//сумма в коп.
          str += padl( a_in[i][2],            aReq[2], "0")  //тип операции
          str += padl( a_in[i][3],            aReq[3], "0")  //процессинг-код
          str += padl( a_in[i][4],            aReq[4], "0")  //код валюты
          fwrite(ff, str + CR_LF)
        next
      else
        fwrite(ff, replicate("0",aReqLen - 3)+"810"+CR_LF)
      endif
      rv:=.T.
    else
      rv:=.F.
    endif
  elseif priz = 6
    fwrite(ff, replicate("0", 37)+CR_LF)
    fwrite(ff, schet+replicate("0", 10)+CR_LF)
    fwrite(ff, replicate("0", 40)+CR_LF)  //резерв
    fwrite(ff, replicate("0", 40)+CR_LF)  //резерв
    rv:=.T.
  elseif priz = 8
    fwrite(ff, dopStr1+CR_LF)
    fwrite(ff, dopStr2+CR_LF)
    fwrite(ff, replicate("0", 40)+CR_LF)  //резерв
    fwrite(ff, replicate("0", 40)+CR_LF)  //резерв
    rv:=.T.
  else
    rv:=.T.
  endif
  fclose(ff)
endif
if CFUNC
  oldY := row()
  oldX := col()
  oldC := setCursor( 0)
  oldcolor:=setcolor("W/N,,,,")
  scr00:=savescreen( 0, 0, maxrow(), maxcol())
  cls
  setpos( 0, 0)
  //inkey(0)
  if priz = 0
    //открытие дня
    rvStr := PopenDay( prizNoKey + priz3 + priz4 +;
             replicate("0",16) + CR_LF)
  elseif priz = 1
    rvStr := PcloseDay( prizNoKey + priz3 + priz4 +;
             replicate("0",16) + CR_LF)
  elseif priz = 4
    //rvStr := PlinkTest( prizNoKey + priz3 + priz4 +;
    //         replicate("0",16) + CR_LF)
  elseif priz = 2
    /*
    if len( cardNewTek()) > 0 .and. len(a_in) > 0
      str:=""
      for i:=1 to len(a_in)
        str += padl( a_in[i][1] * 100, aReq[1], "0")  //сумма в копейках
        str += padl( a_in[i][2],       aReq[2], "0")  //тип операции
        str += padl( a_in[i][3],       aReq[3], "0")  //процессинг-код
        str += padl( a_in[i][4],       aReq[4], "0")  //код валюты
      next
      rvStr := PautoReq( str, len(a_in), substr( cardNewTek(), 2), prizNoKey +;
                         priz3 + priz4 +;
                         replicate("0",16) + CR_LF)
    endif
    */
  endif
  restscreen( 0, 0, maxrow(), maxcol(), scr00)
  setpos( oldY, oldX)
  setCursor( oldC)
  setcolor(oldcolor)
  ff:=fcreate( posPath()+"out.pos")
  if ff > 0
    if priz != 2
      fwrite( ff, rvStr)
    else
      lenSr:=len(a_in) * 148
      for i:=1 to len(a_in)
        fwrite(ff,padr(substr( rvStr, 1+(i-1)*lenSr, lenSr),lenSr,"0")+CR_LF)
      next
    endif
    fclose(ff)
  endif
endif
return rv


function readOutPos( priz)
local strFile:=memoread( posPath() + "out.pos"), str
local arr, aRv:={ }
local i, j, tekKol, kolStr
local aResLen

do case
  case priz = 0
    arr:=oResDefs
  case priz = 1
    arr:=cResDefs
  case priz = 2
    arr:=aResDefs
  case priz = 4
    arr:=lResDefs
  case priz = 6
    arr:=bResDefs
  case priz = 7
    arr:=rResDefs
  case priz = 8
    arr:=tResDefs
endcase
//это не запрос на авторизацию
if priz != 2
  str:=strFile
  aRv:=array( 1, len(arr))
  tekKol:=0
  for i:=1 to len( arr)
    aRv[1][i] := alltrim( substr(str, tekKol+1, arr[i]) )
    tekKol += arr[i]
  next
//обработка ответа запроса на авторизацию
else
  aResLen:=0
  for j:=1 to len(arr)
    aResLen += arr[j]
  next
  aResLen+=2     //еще две позиции на возврат каретки и перевод строки
  kolStr:=int( len(strFile) / aResLen)
  //есть ли хотя бы одна строчка в выходном файле ?
  if kolStr > 0
    aRv:=array( kolStr, len(arr) )
    for j:=1 to kolStr
      str:=substr( strFile, (j-1) * aResLen + 1, aResLen)
      tekKol:=0
      for i:=1 to len( arr)
        aRv[j][i] := alltrim( substr( str, tekKol+1, arr[i]) )
        tekKol += arr[i]
      next
    next
  else
    aRv:={ }
  endif
endif

return aRv

//чтение карты
//Содержимое второй дорожки 37 символов
//При считывании в начало добавляется символ и, возможно, в конец
//также добавляется символ (зависит от считывателя)
//Ограничивающие служебные символы также зависят от настроек считывателя
//Обычно вначале ставится '?' или ';', а в конце может стоять '?'
//Например '?5897970000003011=97055011494685599999'
//или      ';5897970000003011=97055011494685599999?'
/*
function read_card()
local key, strCard:="", i
local scr00:=savescreen(10,0,12,79)
local min:=0

//messageStartUp("Проведите картой через карт-ридер")
@10,22 say "┌───────────────────────────────────┐" color "W/B"
@11,22 say "│ Проведите картой через карт-ридер │" color "W/B"
@12,22 say "└───────────────────────────────────┘" color "W/B"
for i:=1 to 40
  key:=inkey( min)
  if key = 0
    strCard:=""
    messageB("Не пытайтесь меня обмануть")
    exit
  elseif key = 27
    strCard:=""
    exit
  elseif key = 13 .or. key = 10
    exit
  endif

  //если переключено на русскую клавиатуру,
  //то латинский "?" соответствует русской ","
  //if len(strCard) = 0  // .and. (key == "," .or. key == "?")
  //  key:="0"
  //endif

  strCard += chr(key)
  min:=0.1
next
restScreen(10,0,12,79,scr00)
*/
//messageExit()
/*
if len(strCard) < 38
  messageStartUp("Ошибка считывания карты")
  tone(440,2)
  messageExit()
  strCard:=""
elseif len(strCard) >= 38
  //проверка на префикс - тестовая, социальная, классическая и т.д.
  //(key!="?5897970" .and. key!="?5897972" .and. key="?5897973")
  if len( cardLimit( "bin", substr(strCard, 2, 37) ) ) > 0
    strCard:=substr(strCard, 1, 38)
  else
    messageB("Данная карта не обслуживается в системе ПЕТРОКАРТ")
    strCard:=""
  endif
endif
*/
//clear typeahead
//return strCard

//чек на открытие дня
function chekOpenDay( arr)
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yy")
local text:={ }

aadd(text, '')
aadd(text, padc('PETROCARD', 35))
//aadd(text, padc('PETROVSKIY BANK', 35))

// Replaced by TMV 27-01-2003
//aadd(text, padc(' PETROVSKIY NARODNY BANK',35))  //04-09-2000
aadd(text, padc(' MDM BANK S-PETERSBURG',35))

aadd(text, padc('BANK TEL: (812)326-89-49',35))
aadd(text, getTips(arr[7], arr[6], .T.))  //27-12-99
aadd(text, padc(alltrim(arr[8]), 35))  //filS()
aadd(text, padc(arr[11], 35))
aadd(text, padc(alltrim(arr[9])+" "+arr[10], 35))
aadd(text, padc('TERMINAL '+arr[1], 35))
aadd(text, padc( strToDateTime( arr[4]), 35))
aadd(text, padc('DAY OPEN N '+arr[3], 35))
aadd(text, padc('LAST CLOSE: '+strToDateTime( arr[5]), 35))
aadd(text, padc(VERS, 35))

set(_SET_DATEFORMAT, old_format)
return text

//
procedure chekCloseDay( arr)
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yy")
local text:={ }
local card_path:=posPath()  //base_path()+alltrim("card\ ")
local a_sum:={ }, ind, a_line, i
local first, kolNal:=0, sumNal:=0, sledStr
local err, tim, kod
local specKod, tipDoc
local kodDay, numTerm
local curD, curT, curDt, curDt2
local cash:={ { 0, 0}, { 0, 0} }, prizNal
local cStr
local a_mes:={"   ОТКРЫТИЕ ДНЯ    ",;
              "   ЗАКРЫТИЕ ДНЯ    ",;
              "  ПРОВЕРКА СВЯЗИ   ",;
              "  ОТМЕНА ПРЕД.ОП.  "}
local fnames
if len( posPath( 1)) = 0
  return
endif

kodDay :=temp_posR( "НОМЕР ДНЯ", "C")
numTerm:=temp_posR( "НОМЕР ТЕРМИНАЛА", "C")
curDt2 :=temp_posR( "ОТКРЫТИЕ ДНЯ", "C")

if valType( arr) != "A"
  specKod := space( 7)
  tipDoc  := 1
  if getup( 8,15, {{"Если вы хотите распечатать итоговый чек, выберите в  "},;
                   {"качестве документа итоговый чек и введите специальный"},;
                   {"код, который Вам сообщили в Центре Авторизации.      "},;
                      {"Тип документа  ", block(tipDoc),;
                            {"Статистика сообщений",;
                             "Итоговый чек        "} },;
                      {"Специальный код", block(specKod)}} )
    specKod := padl( alltrim(specKod), 6, "0")
    if tipDoc = 2 .and. temp_posR( "НОМЕР ДНЯ", "N") = 1
      dialog({"Вы не имеете право получить итоговый чек после",;
              "успешного закрытия дня. Воспользуйтесь пунктом",;
              "<Печать последнего чека>"},;
             {"Ok"})
      return
    endif
    if tipDoc = 2
      if file(card_path+"k_"+kodDay+".dbf")
        USE (card_path+"k_"+kodDay+".dbf") NEW ALIAS kplat READONLY
        if !neterr()
          locate for kplat->code == "ZZ"
          err:=found()
          kplat->(dbclosearea())
          if !err
            dialog({"Печать итогового чека по коду возможна только после",;
                    "хотя бы одной неудачной попытки закрыть день"},;
                   {"Ok"})
            return
          endif
        else
          messageB("Сбой !!! Ошибка открытия файла "+card_path+"k_"+kodDay+".dbf")
          return
        endif
      else
        messageB("Сбой !!! Отсутствует файл "+card_path+"k_"+kodDay+".dbf")
        return
      endif
    endif
    if tipDoc = 2 .and. specKod != temp_posR( "КОД ЗАКРЫТИЯ", "C")
      dialog({"Введенный код не совпал с эталоном",;
              "Нельзя распечатать итоговый чек.  ",;
              "Уточните код в Центре авторизации "},;
             {"Отменить печать"})
      return
    endif
    arr:={ , , , , ,;
            temp_posR( "T1_LOCATION", "C"),;
            temp_posR( "T2_LOCATION", "C"),;
            temp_posR( "T3_LOCATION", "C"),;
            temp_posR( "CITY",        "C"),;
            temp_posR( "COUNTRY",     "C"),;
            temp_posR( "ADDRESS",     "C") }
  else
    return
  endif
else
  tipDoc:=0
  if temp_posR( "НОМЕР ДНЯ", "N") = 1
    //dialog({"День уже закрыт. Воспользуйтесь пунктом",;
    //        "<Печать последнего чека>"},;
    //       {"Ok"})
    //return
  endif
endif

aadd(text, '')
aadd(text, padc('PETROCARD', 35))
//aadd(text, padc('PETROVSKIY BANK' , 35))

// Replaced by TMV 27-01-2003
//aadd(text, padc(' PETROVSKIY NARODNY BANK',35))  //04-09-2000
aadd(text, padc(' MDM BANK S-PETERSBURG',35))

aadd(text, padc('BANK TEL: (812)326-89-49',35))
aadd(text, getTips(arr[7], arr[6], .T.))  //27-12-99
aadd(text, padc(arr[8], 35))
if tipDoc = 0 .or. tipDoc = 2
  //aadd(text, padc("ИТОГОВЫЙ  ЧЕК", 35))
  if tipDoc = 2
    aadd(text, padc("<SPECIAL CODE "+specKod+">", 35))
  endif
else
  aadd(text, padc("STATISTICS", 35))
endif
aadd(text, padc(arr[11], 35))
aadd(text, padc(alltrim(arr[9])+' '+arr[10], 35))
if tipDoc = 0
  aadd(text, padc('TERMINAL '+arr[1], 35))
  aadd(text, padc(strToDateTime(arr[4]), 35))
  aadd(text, padc('DAY CLOSE N '+arr[3], 35))
  aadd(text, padc('LAST OPEN: '+strToDateTime(arr[5]), 35))
else
  aadd(text, padc('TERMINAL '+numTerm, 35))
  curD:=dtos( setCurDate())
  curT:=time()
  curDt:=substr(curD,3,6)+substr(curT,1,2)+substr(curT,4,2)+substr(curT,7,2)
  aadd(text, padc(strToDateTime( curDt), 35))
  aadd(text, padc('DAY N '+kodDay, 35))
  aadd(text, padc('LAST OPEN: '+strToDateTime(curDt2), 35))
endif

fnames:=card_path + "k_" + kodDay +".cl"
if tipDoc = 0
  fnames+="o"
elseif tipDoc = 1
  fnames+="s"
elseif tipDoc = 2
  fnames+="i"
endif

if openPrn( fnames) = .T.

  if file(card_path+"k_"+kodDay+".dbf") = .T.
    USE (card_path+"k_"+kodDay+".dbf") NEW ALIAS kplat READONLY
  endif

  if select("KPLAT") > 0
    kplat->(dbgotop())
    do while !kplat->(eof())
      if kplat->code == NalKod .and.;
                 kplat->rezult == "00" .and.;
                 kplat->resp_code == "00" .and.;
                 kplat->pack_state == "1"
        cash[1][1]+=kplat->summa
        cash[1][2]++
        prizNal:=1
      elseif kplat->code == RetKod
        if prizNal = 1
          if kplat->rezult == "00" .and. kplat->pack_state == "1"
            cash[2][1]+=kplat->summa
            cash[2][2]++
            prizNal:=0
          endif
        endif
      else
        prizNal:=0
      endif
      kplat->(dbskip())
    enddo
  endif
  aadd(text, padr('CASH TOTAL', 35))
  aadd(text, padr('AMOUNT  '+str(cash[1][1], 13, 2), 35))
  aadd(text, padr('OPERATIONS'+str(cash[1][2] , 11), 35))
  aadd(text, padr('CASH REVERSAL', 35))
  aadd(text, padr('AMOUNT  '+str(cash[2][1], 13, 2), 35))
  aadd(text, padr('OPERATIONS'+str(cash[2][2], 11), 35))
  aadd(text, padr('CASH', 35))
  aadd(text, padr('AMOUNT  '+str(cash[1][1]-cash[2][1], 13, 2), 35))
  aadd(text, padr('OPERATIONS'+str(cash[1][2]-cash[2][2], 11), 35))
  aadd(text, padr('CREDIT TOTAL', 35))
  aadd(text, padr('AMOUNT  '+str(0 / 100, 13, 2), 35))
  aadd(text, padr('OPERATIONS'+str( 0, 11), 35))
  aadd(text, padr('CREDIT REVERSAL', 35))
  aadd(text, padr('AMOUNT  '+str(0 / 100, 13, 2), 35))
  aadd(text, padr('OPERATIONS'+str( 0, 11), 35))
  aadd(text, padr('CREDIT', 35))
  aadd(text, padr('AMOUNT  '+str(0 / 100, 13, 2), 35))
  aadd(text, padr('OPERATIONS'+str( 0, 11), 35))
  aadd(text, padr('SIGNATURE CASHIER:', 35))
  aadd(text, replicate("_", 35))
  aadd(text, '')
  aadd(text, '')  //конец официального чека

  if select("KPLAT") > 0
    kplat->(dbgotop())
    ind:=0
    do while !kplat->(eof())
      if !empty(kplat->code)
        //это платеж (не проверка связи, не открытие дня, не закрытие дня)
        if kplat->code != CheckKod .and.;
               kplat->code != OpenDayKod .and.;
               kplat->code != CloseDayKod
          //есть сумма и авторизация прошла успешно
          if kplat->summa > 0 .and.;
                 kplat->rezult == "00" .and.;
                 kplat->resp_code == "00" .and.;
                 kplat->pack_state == "1"
            ind:=ascan(a_sum,{|x| x[1] == kplat->code})
            if ind > 0
              a_sum[ind][2]++
              a_sum[ind][3]+=kplat->summa
            else
              aadd(a_sum,{kplat->code, 1, kplat->summa, 0, 0})
              ind:=len(a_sum)
            endif
          elseif kplat->code = RetKod
            if ind > 0
              if kplat->rezult == "00" .and.;
                 kplat->pack_state == "1"
                a_sum[ind][4]++
                a_sum[ind][5]+=kplat->summa
              endif
            endif
          else
            ind:=0
          endif
        endif
      endif
      kplat->(dbskip())
    enddo
  endif

  //первый отчет
  for i:=1 to len(text)
    ? text[i]
  next
  //19-09-97 в статистику сообщений не включаем общие суммы
  if tipDoc != 1
    utilitBopen()
    for i:=1 to len(a_sum)
      if a_sum[i][1] == NalKod
        a_line:={ 'Выдача наличных' }
      elseif a_sum[i][1] == ZabKod
        a_line:={ 'ЦПО "Забота+"' }
      else
        a_line:=utilitBcode( a_sum[i][1])
      endif
      for ind:=1 to len(a_line)
        ? a_line[ind]
      next
      ? 'Общее количество операций: '+str( a_sum[i][2],5)
      ? 'Итоговая сумма:       '+str( a_sum[i][3],10,2)+' руб.'
      ? 'Отменено операций:         '+str( a_sum[i][4],5)
      ? 'На сумму:             '+str( a_sum[i][5],10,2)+' руб.'
      ? 'Проведено операций:        '+str( a_sum[i][2] - a_sum[i][4], 5)
      ? 'На сумму:             '+str( a_sum[i][3] - a_sum[i][5],10,2)+' руб.'
      ?
    next
    utilitBclose()
  endif
  ?
  ? "                            ПРОТОКОЛ СООБЩЕНИЙ"
  ?
  first:=.F.
  sledStr:=.T.
  ? "  Время ","Код","    Номер карты    ","Код ошибки",;
    "  Время ","Код","    Номер карты    ","Код ошибки"
  ? "        ","   ","                   ","код ответа",;
    "        ","   ","                   ","код ответа"
  ?
  if select("KPLAT") > 0
    kplat->(dbgotop())
    do while !kplat->(eof())
      err := padr(kplat->kod_err,10)
      if kplat->(fcount()) > 15
        cStr:=kplat->pan
      else
        cStr:=substr(kplat->card,1,16)
      endif
      cStr:=padr(cStr, 19)
      //если была ошибка связи, то возьмем время с рабочей станции
      if kplat->rezult != "00" .or. kplat->pack_state == "0"
        tim := padr( kplat->tek_time, 8)
      else
        tim := padr( kplat->fo_time,  8)
      endif
      kod := padr(kplat->code, 3)
      //любое сообщение кроме открытия и закрытия
      if kplat->code == OpenDayKod
        ? tim, "   ", a_mes[1], err
        sledStr:=.T.
      elseif kplat->code == CloseDayKod
        ? tim, "   ", a_mes[2], err
        sledStr:=.T.
      elseif kplat->code == CheckKod
        if sledStr = .T.
          ? tim, "   ", a_mes[3], err
          sledStr:=.F.
        else
          ?? "", tim, "   ", a_mes[3], err
          sledStr:=.T.
        endif
      elseif kplat->code == RetKod
        if sledStr = .T.
          ? tim, "   ", a_mes[4], err
          sledStr:=.F.
        else
          ?? "", tim, "   ", a_mes[4], err
          sledStr:=.T.
        endif
      elseif !empty(kplat->code)
        if sledStr = .T.
          ?  tim, kod, cStr, err
          sledStr:=.F.
        else
          ?? "", tim, kod, cStr, err
          sledStr:=.T.
        endif
      endif
      kplat->(dbskip())
    enddo
  endif
  ?
  ?
  //19-09-97 в статистику сообщений не включаем общие суммы
  if tipDoc != 1
    //второй отчет
    //for i:=1 to len(text)
    //  ? text[i]
    //next
    ?
    ? "                            ВЫДАЧА НАЛИЧНЫХ"
    ?
    first:=.F.
    sledStr:=.T.
    if select("KPLAT") > 0
      kplat->(dbgotop())
      do while !kplat->(eof())
        //успешная выдача наличными
        if kplat->code == NalKod .and.;
                   kplat->rezult == "00" .and.;
                   kplat->resp_code == "00" .and.;
                   kplat->pack_state == "1"
          if first = .F.
            first := .T.
    ? "  Время ","   Номер карты     ","   Сумма  ",;
      "  Время ","   Номер карты     ","   Сумма  "
    ?
          endif
          kolNal++
          sumNal+=kplat->summa
          if kplat->(fcount()) > 15
            cStr:=kplat->pan
          else
            cStr:=substr(kplat->card,1,16)
          endif
          cStr:=padr(cStr, 19)
          if sledStr = .T.
            ?  kplat->fo_time, cStr, str(kplat->summa,10,2)
            sledStr:=.F.
          else
            ?? "", kplat->fo_time, cStr, str(kplat->summa,10,2)
            sledStr:=.T.
          endif
        endif
        kplat->(dbskip())
      enddo
    ?
    ? "Итого:  количество операций "+str(kolNal,6)
    ? "        общая сумма   "+str(sumNal, 12, 2)
    endif
  endif

  for i:=1 to 17
    ?
  next

  if select("KPLAT") > 0
    kplat->(dbclosearea())
  endif

  closePrn()
#ifdef TOWN
  if openPrn() = .T.
    if out_print() = 2
      ? chr(15)
    else
      ? chr(18)
    endif
    closePrn()
    copy file ( fnames) to prn
  endif
#else
  text:={ }
  sledStr:=memoread( fnames)
  ind:=mlcount( sledStr, 85)
  for i:=1 to ind
    aadd(text, memoline(sledStr, 85, i))
  next
  printAorder( text)
#endif

else
  messageB("Ошибка записи чека закрытия дня")
endif

set(_SET_DATEFORMAT, old_format)
return

//чек на авторизацию
//tip1 определяет выдачу наличными ('nal')
//     или безналичное списание коммунальных платежей ('beznal')
//     или безналичное списание по заботе ('zabota')
procedure chekAuto( priz, tip1, arr, a_in, pack_state, tipReg, newChek, aMas)
local card_path:=posPath()  //base_path()+alltrim("card\ ")
local old_format, i
local text1:={ }, text2:={ }
local numTerm, numCard, kodAuto, endDT, endOst, numTrace
local prizAuto:=.F., prizresp:=.F., prizRet:=.F.
local a_line, kolCode
local tipR
local tekRow, maxRow
local allKol:=0, allSum:=0
local kodDay
local ind, text, sledStr

tipReg:=" "       //16-06-97

tip1:=lower(alltrim(tip1))

//расшифровка данных, ищем последний авторизованный запрос
//чтобы взять остаток, время и т.д.
//16-06-97 также анализируем превышение лимита подписи или регистрации
kodAuto:=space(6)
numTerm :=arr[1][1]
for i:=1 to len(arr)
  //проверка отправки пакета
  if arr[i][pack_state] == "1"
    if priz = 2
      //проверка результата операции
      if arr[i][2] == "00"
        //проверка responce-кода (ответ карточного центра)
        if arr[i][10] == "00"
          //укажем, что данная сумма была авторизована успешно
          prizAuto:=.T.  //среди запросов есть и авторизованный
          //номер карты, остаток, время
          numCard :=arr[i][4]
          endDT   :=arr[i][5]+arr[i][6]
          endOst  :=substr( arr[i][12], 1, 10)+"."+;
                    substr( arr[i][12], 11)  //10 цифр без копеек
          numTrace:=arr[i][8]
          kodAuto :=padr( arr[i][11], 6)
          //16-06-97 обработка типа чека (обычный,с лимитом подп. или регистр.)
          tipR   :=substr( arr[i][14], 1, 1)
          if ( tipReg == " " .and. at( tipR, "ABC") > 0 ) .or.;
             ( tipReg == "A" .and. at( tipR, "BC" ) > 0 ) .or.;
             ( tipReg == "B" .and. tipR == "C")
            tipReg := tipR
          endif
        //18-03-99 все, что надо, перенесено в чек отказа
        //else
        //  if arr[i][10] == "51" .or. arr[i][10] == "61" .or. arr[i][10] = "65"
        //    //укажем, что запрос был отослан, но в авторизации отказано
        //    //из-за слишком большой суммы
        //    prizResp:=.T.
        //    //номер карты, остаток, время
        //    numCard :=arr[i][4]
        //    endDT   :=arr[i][5]+arr[i][6]
        //    endOst  :=substr( arr[i][12], 1, 10)  //10 цифр без копеек
        //    numTrace:=arr[i][8]
        //  endif
        endif
      endif
    else
      prizRet := .T.
      numCard :=aMas[4]
      endDT   :=arr[i][3]
      numTrace:=aMas[3]
      kodAuto :=aMas[2]
    endif
  endif
next

//хотя бы один запрос был авторизован или в авторизации было отказано
if prizAuto = .T. .or. prizResp = .T. .or. prizRet = .T.
  old_format:=set(_SET_DATEFORMAT, "dd.mm.yy")

  aadd(text1, '')
  aadd(text1, padc('PETROCARD', 35))
  if !prizRet
    if !empty(arr[1][13]) .and. arr[1][13] > '0' .and. arr[1][13] < '8' .and.;
         len(arr[1][13]) = 1
      aadd(text1, padc(aTiss[val(arr[1][13])], 35))
    endif
  else
    if !empty(aMas[6]) .and. aMas[6] > '0' .and. aMas[6] < '8' .and.;
         len(aMas[6]) = 1
      aadd(text1, padc(aTiss[val(aMas[6])], 35))
    endif
  endif
  //aadd(text1, padc('PETROVSKIY BANK', 35))

// Replaced by TMV 27-01-2003
//  aadd(text1, padc(' PETROVSKIY NARODNY BANK',35))  //04-09-2000
aadd(text, padc(' MDM BANK S-PETERSBURG',35))

  aadd(text1, padc('BANK TEL: (812)326-89-49',35))
  if prizRet
    ind:=5
  else
    ind:=17
  endif
  aadd(text1, getTips(arr[1][ind], arr[1][ind-1], .F.))  //27-12-99
  //if !empty(arr[1][ind]) .and. arr[1][ind] > '0' .and. arr[1][ind] < '5'
  //  aadd(text1, padc(aTips[val(arr[1][ind])]+' '+arr[1][ind-1], 35))
  //else
  //  aadd(text1, padc('CASHIER OFFICE '+getOtd(), 35))
  //endif
  aadd(text1, padc(arr[1][ind+1], 35))
  aadd(text1, padc(arr[1][ind+4], 35))
  aadd(text1, padc(alltrim(arr[1][ind+2])+' '+arr[1][ind+3], 35))
  aadd(text1, padc('TERMINAL '+numTerm, 35))
  if (prizAuto = .T. .or. prizRet = .T.) .and. newChek > 0
    aadd(text1, padc("N "+padl( newChek, 5, "0"), 35))
  else
    aadd(text1, padc("N      ", 35))
  endif
  aadd(text1, padr( strToDateTime( endDT), 35))
  aadd(text1, padr( numCard, 35))
  aadd(text1, padr( "TRACE "+numTrace, 35))
  aadd(text1, padr( "AUTH "+kodAuto, 35))
  if tip1 == 'nal'
    aadd(text1, padr( "CASH", 35))
  elseif tip1 == 'beznal' .or. tip1 == 'zabota'
    aadd(text1, padr( "SALE", 35))
  elseif tip1 == 'ret'
    if aMas[5] = 1
      aadd(text1, padr( "CASH REVERSAL", 35))
    elseif aMas[5] = 0
      aadd(text1, padr( "REFUND", 35))
    endif
  endif
  if !prizRet
    //расшифруем безналичные платежи - откроем базу утилит
    if tip1 == "beznal"
      utilitBopen()
    endif
    //укажем все суммы (по каждому запросу)
    for i:=1 to len(arr)
      //проверка отправки пакета
      if arr[i][2] == "00" .and. arr[i][pack_state] == "1"
        if tip1 == "beznal"
          a_line := utilitBcode( a_in[i][2])
          for kolCode:=1 to len(a_line)
            aadd(text1, padr(a_line[ kolCode], 35))
          next
        endif
        if arr[i][10] == "00"
          allKol++
          allSum += val(arr[i][7] )
          aadd(text1, padr('AMOUNT  '+str(val(arr[i][7]) / 100, 13, 2)+' '+;
                      arr[i][9], 35))
        else
          aadd(text1, padr('REFUSE  '+str(val(arr[i][7]) / 100, 13, 2)+' '+;
                      arr[i][9], 35))
        endif
      endif
    next
    //закроем базу утилит
    if tip1 == "beznal"
      utilitBclose()
    endif
  else
    aadd(text1, padr('AMOUNT  '+str(aMas[1], 13, 2)+' RUR', 35))
  endif
  //до этого места чеки клиенту и оператору совпадают
  text2 := aclone( text1)

  /*
  //общая сумма авторизованных платежей по безналич. перечислениям
  if tip1 == "beznal"
    aadd(text1, padr('Общая списанная сумма '+;
                     alltrim(str(allSum,8,2))+' руб.', 35, " "))
  endif
  */
  if !prizRet
    //16-06-97 на чеке оператора при превышении лимита
    if tipReg == "C"
      aadd(text2, padr('CLIENT:', 35))
      aadd(text2, replicate("_", 35))
      aadd(text2, replicate("_", 35))
      aadd(text2, replicate("_", 35))
    endif
    if tipReg == "B" .or. tipReg == "C"
      aadd(text2, padr('SIGNATURE:', 35))
      aadd(text2, replicate("_", 35))
    endif
    /*
    //при выдаче наличными на чеке оператора
    if tip1 == 'nal'
      aadd(text2, padr('Подпись операциониста', 35, " "))
      aadd(text2, replicate("_",35))
      aadd(text2, 'Подпись контролера ________________')
    endif
    */

    //на чеке клиента остаток
    aadd(text1, padr('BALANCE '+endOst+' RUR', 35))
  else
    aadd(text1, padr('SIGNATURE CASHIER:', 35))
    aadd(text1, replicate("_", 35))
  endif
  aadd(text1, padc('THANK YOU, PLEASE KEEP THIS', 35))
  aadd(text1, padc('RECEIPT', 35))

  //данные есть, если массивы не пустые
  if len(text1) > 0 .and. len(text2) > 0
    if len(text1) > len(text2)
      maxRow := len(text1)
    else
      maxRow := len(text2)
    endif
    //запись данных в файл
    kodDay := temp_posR( "НОМЕР ДНЯ", "C")
    if openPrn( card_path + "k_" + kodDay +".aut" ) = .T.
      for tekRow:=1 to maxRow
        if len(text1) >= tekRow
          ? text1[ tekRow], space(5)
        else
          ? space(35), space(5)
        endif
        if len(text2) >= tekRow .and. (prizAuto = .T. .or. prizRet = .T.)
          ?? text2[ tekRow]//, space(5)
        else
          ??
        endif
      next
      for tekRow:=1 to 17
        ?
      next
      closePrn()
    endif
#ifdef TOWN
    if openPrn()
      if out_print() = 2
        ? chr(15)
      else
        ? chr(18)
      endif
      closePrn()
      copy file ( card_path + "k_" + kodDay +".aut" ) to prn
    endif
#else
  text:={ }
  sledStr:=memoread( card_path + "k_" + kodDay +".aut")
  ind:=mlcount( sledStr, 85)
  for i:=1 to ind
    aadd(text, memoline(sledStr, 85, i))
  next
  printAorder( text)
#endif
  endif
  set(_SET_DATEFORMAT, old_format)
endif

return

procedure chekOtkaz( rezMess, term, sdtime, pan, bal, val)
local i, text:={ "ОТКАЗАНО/DECLINED",;
;//                 "PETROVSKIY BANK SPB RU",;
;// Replaced by TMV 27-01-2003
;//                 ' PETROVSKIY NARODNY BANK',;  //04-09-2000
					  ' MDM BANK S-PETERSBURG',;
                 'BANK TEL: (812)326-89-49',;
                 "TERMINAL "+term}
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yy")
local astr

aadd( text, strToDateTime( sdtime))
aadd( text, "CARD "+pan)
astr:=razbivka( rezMess[3],{{20,"C"},{20,"C"},{20,"C"}})
for i:=1 to 3
  if len(alltrim(astr[i])) > 0
    aadd( text, astr[i])
  endif
next
aadd( text, "")
if len(alltrim(bal)) > 0
  aadd( text, "BALANCE "+left(bal, 10)+"."+right(bal,2)+" "+val)
endif
aadd( text, "")
aadd( text, "")
#ifdef TOWN
    if openPrn()
      if out_print() = 2
        ? chr(15)
      else
        ? chr(18)
      endif
      for i:=1 to len(text)
        ? padc( text[i], 24)
      next
      closePrn()
    endif
#else
  printAorder( text)
#endif
set(_SET_DATEFORMAT, old_format)
return

procedure chekAutoEnd()
local card_path:=posPath()  //base_path()+alltrim("card\ ")
local card_name
local arr:={"Последний чек авторизации",;
            "Последний чек закрытия дня"}
local i, name:=temp_posR( "НОМЕР ДНЯ", "C")
local opened:=temp_posR( "НОМЕР ДНЯ", "N")

if len( posPath( 1)) = 0
  return
endif
i:=popup( 8, 10, arr)
if i > 0
  card_name:="k_"+temp_posR( "НОМЕР ДНЯ", "C")
  if i = 1
    card_name+=".aut"
  else
    card_name+=".clo"
    if opened = 0
      messageB("Текущий день еще не закрыт")
      return
    endif
  endif
  if file( card_path + card_name )
    if openPrn()
      if out_print() = 2
        ? chr(15)
      else
        ? chr(18)
      endif
      closePrn()
      copy file ( card_path + card_name ) to prn
    endif
  else
    messageB("Невозможно найти последний чек")
  endif
endif
return
/*
function formatDtime( str)
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yy")
local strRv:="", dto

str:=padr(str,12,"0")
if str != "000000000000" .and. str != "            "
  //получим дату
  dto:=ctod( substr( str,5,2)+"."+substr( str,3,2)+"."+substr( str,1,2))
  //преобразуем дату в строку
  strRv:=padl(day(dto),2,"0")+textMonDate( dto)+textYeaDate(dto)
  //добавим время
  strRv:=padr(strRv,27," ")+;
       substr( str,7,2)+":"+substr( str,9,2)+":"+substr( str,11,2)
else
  strRv:=str
endif
set(_SET_DATEFORMAT, old_format)
return strRv

//преобразуем дату в строку
function formatDate( dto)
local strRv
if day(dto) > 0 .and. month(dto) > 0 .and. year(dto) > 0
  strRv:=padl(day(dto),2,"0")+textMonDate( dto)+textYeaDate(dto)
else
  strRv:=space(10)
endif
return strRv
*/
//преобразуем время из вида "hhmmss" в вид "hh:mm:ss"
function formatTime( str)
str:=padr( str,6,"0")
return substr( str,1,2)+":"+substr( str,3,2)+":"+substr( str,5,2)

procedure newCard()
local temp_pos:=temp_posDefs
local endDay := "000000", endChek := 0
local dir, i
local card_path:=posPath()  //base_path()+alltrim("card\ ")

if len( posPath()) = 0
  return
endif

if !file( posPath() + "temp_pos.dbf")
  /*
  if temp->(dbseek("КАРТА")) = .T.
    endDay  := substr( temp->str_temp, 4, 6)
    endChek := temp->num_temp
  endif
  */
  basepath( temp_pos, posPath())
  basecreate(temp_pos)
  baseMOpen(temp_pos)
  temp_posW( "НОМЕР ДНЯ",  endDay, , 1)
  temp_posW( "НОМЕР ЧЕКА", , , endChek)
  baseMClose(temp_pos)
endif
    /*
    //19-05-98 переносим файлы в новый каталог
    if file( posPath() + "temp_pos.dbf")
      dir := directory( base_path()+alltrim("card\ ")+"k_??????.dbf" )
      for i:=1 to len(dir)
        copy file ( base_path()+alltrim("card\ ")+dir[i][1]) to (posPath()+dir[i][1])
        ferase( base_path()+alltrim("card\ ")+dir[i][1])
      next
      dir := directory( base_path()+alltrim("card\ ")+"k_??????.clo" )
      for i:=1 to len(dir)
        copy file ( base_path()+alltrim("card\ ")+dir[i][1]) to (posPath()+dir[i][1])
        ferase( base_path()+alltrim("card\ ")+dir[i][1])
      next
      dir := directory( base_path()+alltrim("card\ ")+"k_??????.aut" )
      for i:=1 to len(dir)
        copy file ( base_path()+alltrim("card\ ")+dir[i][1]) to (posPath()+dir[i][1])
        ferase( base_path()+alltrim("card\ ")+dir[i][1])
      next
    endif
    */
    //удаление старых файлов ("nnnnnn" - номер открытого дня)
/*  18-06-97
    //"K_nnnnnn.LEN" (контрольная лента)
    dir := directory( base_path()+alltrim("card\ ")+"k_??????.len" )
    if len( dir) > 10
      asort( dir,,,{|x,y| x[1] < y[1]})
      for i:=1 to len( dir) - 10
        ferase( base_path() + dir[i][1])
      next
    endif
*/
    //"K_nnnnnn.DBF" (расшифровка платежей)
    endDay := temp_posR( "НОМЕР ДНЯ", "C")
    dir := directory( card_path + "k_??????.dbf" )
    for i:=1 to len( dir)
      if val( substr(dir[i][1], 3, 6)) < (val( endDay) - 10)
        ferase( card_path + dir[i][1])
        ferase( card_path + substr(dir[i][1], 1, 8) + ".clo")
        ferase( card_path + substr(dir[i][1], 1, 8) + ".cli")
        ferase( card_path + substr(dir[i][1], 1, 8) + ".cls")
        ferase( card_path + substr(dir[i][1], 1, 8) + ".aut")
      endif
    next
return

procedure temp_posW( strFind, str1, dto1, num1)
local pPath:=posPath()
local tempClose:=.F.
if select("TEMP_POS") = 0
  if file(pPath + "temp_pos.dbf") = .T.
    USE (pPath+"temp_pos.dbf") INDEX (pPath+"temp_pos.ntx") ALIAS temp_pos NEW SHARED
    tempClose := .T.
  endif
endif
if select("TEMP_POS") > 0
  if temp_pos->(dbseek( strFind)) = .T.
    if temp_pos->(mrlock())
      //temp_pos->comment  := strFind
      if valType( str1) == "C"
        temp_pos->str_temp := str1
      endif
      if valType( dto1) == "D"
        temp_pos->date_temp:= dto1
      endif
      if valType( num1) == "N"
        temp_pos->num_temp:= num1
      endif
      temp_pos->(dbcommit())
      temp_pos->(dbunlock())
      temp_pos->(dbskip( 0))
    endif
  else
    temp_pos->(dbappend())
    if temp_pos->(neterr()) = .F.
      temp_pos->comment  := strFind
      if valType( str1) == "C"
        temp_pos->str_temp := str1
      endif
      if valType( dto1) == "D"
        temp_pos->date_temp:= dto1
      endif
      if valType( num1) == "N"
        temp_pos->num_temp:= num1
      endif
      temp_pos->(dbcommit())  &&  Add by TMV  23-08-2002
      temp_pos->(dbunlock())  &&  Add by TMV  23-08-2002
    endif
  endif
  if tempClose = .T.
    temp_pos->(dbclosearea())
  endif
else
  messageB("Отсутствует файл "+pPath+"temp_pos.dbf")
endif
return

function temp_posR( strFind, priz)
local pPath:=posPath()
local rv:=nil
local tempClose:=.F.
if select("TEMP_POS") = 0
  if file(pPath + "temp_pos.dbf") = .T.
    USE (pPath+"temp_pos.dbf") INDEX (pPath+"temp_pos.ntx") ALIAS temp_pos NEW SHARED
    tempClose := .T.
  endif
endif
if select("TEMP_POS") > 0
  if temp_pos->(dbseek( strFind)) = .T.
    if priz == "C"
      rv := alltrim( temp_pos->str_temp)
    elseif priz == "D"
      rv := temp_pos->date_temp
    elseif priz == "N"
      rv := temp_pos->num_temp
    else
      messageB("Ошибка в процедуре temp_posR()")
    endif
  else
    messageB("Не найдена строка "+strFind+" в процедуре temp_posR()")
  endif
  if tempClose = .T.
    temp_pos->(dbclosearea())
  endif
else
  messageB("Отсутствует файл "+pPath+"temp_pos.dbf")
endif
return rv

function temp_posC( strFind)
local pPath:=posPath()
local rv:=.F.
local tempClose:=.F.
if select("TEMP_POS") = 0
  if file(pPath + "temp_pos.dbf") = .T.
    USE (pPath+"temp_pos.dbf") INDEX (pPath+"temp_pos.ntx") ALIAS temp_pos NEW SHARED
    tempClose := .T.
  endif
endif
if select("TEMP_POS") > 0
  rv := temp_pos->(dbseek( strFind))
  if tempClose = .T.
    temp_pos->(dbclosearea())
  endif
endif
return rv

procedure utilitBopen()
local name, namex
name := base_path() + "utilit.dbf"
namex:= base_path() + "utilitc.ntx"
if file( name ) = .T. .and. file( namex ) =.T.
  USE ( name ) NEW ALIAS utilit INDEX ( namex ) SHARED READONLY
endif
return

procedure utilitBclose()
if select("UTILIT") > 0
  utilit->(dbclosearea())
endif
return

function utilitBcode( kod)
local a_line:={ }, line
if select("UTILIT") > 0 .and. valtype( kod) == "C"
  if utilit->(dbseek( kod)) = .T.
    line := alltrim( utilit->serv )
    if len( line) <= 35
      a_line:={ line}
    else
      a_line:=razbivka(line,{{35,"L"},{35,"L"}})
    endif
  endif
endif
return a_line

function cardFlag( new)
static old

if pcount() = 1
  old:=new
  if old == 1
    //запомним время, когда произошел сбой
    cardBadTime( time())
  endif
endif
return old

function cardBadTime( new)
static old
if pcount() = 1
  old:=new
endif
return old

procedure cardCheck()
local  old_time, new_time, tekMin, oldMin

if cardFlag() == 1
  //каждые 10 минут запускаем проверку связи
  new_time:=time()
  old_time:=cardBadTime()
  tekMin:=val(substr(new_time,1,2)) * 60 + val(substr(new_time,4,2))
  oldMin:=val(substr(old_time,1,2)) * 60 + val(substr(old_time,4,2))
  if (tekMin - oldMin) >= 10
    cardWork (4, 0, , "1", , "0", "0")
    cardBadTime( new_time)
  endif
endif
return
/*
//18-06-97
function cardLimit( priz, card2)
static a_limit
local i

priz:=upper(alltrim( priz))
if priz == "INIT"
  a_limit := { }
  if file( base_path() + "card\limits.dbf" ) = .T.
    USE ( base_path() + "card\limits.dbf" ) ALIAS limits NEW SHARED READONLY
    limits->(dbgotop())
    do while !limits->(eof())
      if !empty(limits->bin)
        aadd( a_limit, { alltrim( limits->label), alltrim( limits->bin),;
                         limits->lowlimit, limits->highlimit,;
                         limits->mask } )
      endif
      limits->(dbskip())
    enddo
    limits->(dbclosearea())
  endif
  return a_limit
else
  i := ascan( a_limit, {|x| x[2] == substr( card2, 1, len(x[2]) ) } )
  if priz == "LABEL"
    return if( i > 0, a_limit[i][1], "")
  elseif priz == "BIN"
    return if( i > 0, a_limit[i][2], "")
  elseif priz == "LOWLIMIT"
    return if( i > 0, a_limit[i][3], 0)
  elseif priz == "HIGHLIMIT"
    return if( i > 0, a_limit[i][4], 0)
  elseif priz == "MASK"
    return if( i > 0, a_limit[i][5], 0)
  endif
endif
return .F.
*/
//18-06-97
function cardRespCod (kod)
local rv:={ "Неизвестна", "", "UNKNOWN ERROR       CODE "}

kod := padl( kod, 2, "0")
rv[3]:=rv[3]+kod
if file( base_path() + "card\respcod.dbf") = .T.
  USE ( base_path() + "card\respcod.dbf") ALIAS respcod NEW SHARED READONLY
  dbselectarea("RESPCOD")
  locate for respcod->cod == kod
  if respcod->(found())
    rv := { alltrim(respcod->comment), alltrim(respcod->err_cod),;
          if(respcod->(fcount()) >= 4,;
             alltrim(respcod->english), "CODE "+kod)}
  else
    //поищем ошибку по умолчанию
    respcod->(dbgotop())
    locate for respcod->cod == "**"
    if respcod->(found())
      rv := { alltrim(respcod->comment), alltrim(respcod->err_cod),;
            if(respcod->(fcount()) >= 4,;
               alltrim(respcod->english)+" "+kod, "DECLINED CODE "+kod)}
    endif
  endif
  respcod->(dbclosearea())
endif
rv[2]:=rv[2]+"/"+kod
return rv

function cardRezCod(kod, state, tipZ)
local rv:={ "Неизвестна", ""}

kod := padl( kod, 2, "0")
if file( base_path() + "card\rezcod.dbf") = .T.
  USE ( base_path() + "card\rezcod.dbf") ALIAS rezcod NEW SHARED READONLY
  dbselectarea("REZCOD")
  locate for rezcod->cod == kod .and.;
    at(state, rezcod->pack) > 0 .and. len(rtrim(rezcod->tip)) > tipZ+1 .and.;
      substr(rezcod->tip, tipZ+1, 1) == "*"
  if rezcod->(found())
    rv := { alltrim(rezcod->comment), alltrim(rezcod->err_cod) }
  else
    //поищем ошибку по умолчанию
    locate for rezcod->cod == "**" .and.;
      at(state, rezcod->pack) > 0 .and. len(rtrim(rezcod->tip)) > tipZ+1 .and.;
        substr(rezcod->tip, tipZ+1, 1) == "*"
    if rezcod->(found())
      rv := { alltrim(rezcod->comment), alltrim(rezcod->err_cod) }
    endif
  endif
  rezcod->(dbclosearea())
endif
rv[2]:=rv[2]+"/"+kod
return rv

//Процедура протоколирования для открытия дня, закрытия дня,
//блокировки счета, проверки связи
procedure PWrite_01( kod, pack_state, a_out, dt, prizNoKey, priz, rc,;
                     schet, a_in, newChek, aMas)
local old_format:=set(_SET_DATEFORMAT,"dd.mm.yy")
local rezMess
local i:=1

if kplatOpen()
  for i:=1 to len(a_out)
    kplat->(dbappend())
    if priz = 2
      kplat->code      := a_in[i][2]
    else
      kplat->code      := kod
    endif
    kplat->tek_time  := time()
    kplat->tek_date  := date()
    if kod == BlockSchKod
      kplat->summa     := val(a_out[i][7]) / 100
    elseif kod == RepKod
      kplat->summa     := a_in[i][1]
      kplat->card      := a_out[i][3]
    elseif priz = 2
      kplat->chek      := newChek
      kplat->summa     := a_in[i][1]
      if a_out[i][3] == replicate("0",37)
        kplat->card      := substr( cardNewTek(), 2)
      else
        kplat->card      := a_out[i][3]
      endif
    elseif kod == RetKod
      kplat->chek      := newChek
      kplat->summa     := aMas[1]
    endif
    //проверяем на ошибку связи
    if a_out[i][2] != "00" .or. a_out[i][pack_state] == "0"
      rezMess := cardRezCod(a_out[i][2], a_out[i][pack_state], priz)
      //выведем и сообщение
      if kod == CloseDayKod .or. kod == CheckKod
        dialogTime({"Ошибка связи !!!",;
                    "Код ошибки: "+rezMess[2]+" "+rezMess[1]},;
                   prizNoKey, 10)
      else
        dialog({"Ошибка связи !!!",;
                "Код ошибки: "+rezMess[2]+" "+rezMess[1]},;
               {"Ok"})
      endif
      kplat->kod_err:=rezMess[2]
    else
      if kod == BlockSchKod .or. kod == RepKod .or. priz = 2
        //проверяем на ошибку авторизации - неправильный RESPONCE-код
        if a_out[i][ rc] != "00"
          rezMess:= cardRespCod( a_out[i][ rc])
          if kod == BlockSchKod
            dialog({"Ошибка блокировки счета !!!",;
                    "Причина отказа: "+rezMess[2]+" "+rezMess[1]},;
                   {"Ok"})
          elseif priz = 2
            dialog({"Ошибка авторизации !!!",;
                    "Сумма "+alltrim(mStr(a_in[i][1]))+;
                    if(a_in[i][2] == "ZV", " не переведена на карту", " не авторизована"),;
                    "Причина отказа: "+rezMess[2]+" "+rezMess[1]},;
                   {"Ok"})
          elseif kod == RepKod
            dialog({"Ошибка частичной отмены операции !!!",;
                    "Причина отказа: "+rezMess[2]+" "+rezMess[1]},;
                   {"Ok"})
          endif
          kplat->kod_err := rezMess[2]
          //печать чека отказа
          if priz = 2
            chekOtkaz( rezMess, a_out[i][1], a_out[i][ dt]+a_out[i][ dt+1],;
                       a_out[i][4], a_out[i][12], a_out[i][9])
          endif
        endif
      endif
      if kod == BlockSchKod .or. kod == RepKod .or. priz = 2
        //здесь два отдельных поля для даты и времени,
        //следующих друг за другом
        //время и дата с FO возвращается как при удачной авторизации,
        //так и при отказе в авторизации
        kplat->fo_date   := ctod( substr(a_out[i][ dt], 5, 2)+"."+;
                                  substr(a_out[i][ dt], 3, 2)+"."+;
                                  substr(a_out[i][ dt], 1, 2) )
        kplat->fo_time   := formatTime( a_out[i][ dt+1])
      else
        //заполним время и дату с FO, хранящиеся в одном поле
        kplat->fo_time   := formatTime( substr(a_out[i][ dt],7) )
        kplat->fo_date   := ctod( substr(a_out[i][ dt], 5, 2)+"."+;
                                  substr(a_out[i][ dt], 3, 2)+"."+;
                                  substr(a_out[i][ dt], 1, 2) )
      endif
    endif
    kplat->rezult    := a_out[i][2]
    if kod == BlockSchKod .or. kod == RepKod .or. priz = 2
      kplat->resp_code := a_out[i][ rc]
    endif
    kplat->pack_state:= a_out[i][pack_state]
    if kod == BlockSchKod .and. kplat->(fcount()) >= 13
      kplat->schet     := schet
    endif
    if kod == BlockSchKod .or. kod == RepKod .or. priz = 2
      if kplat->(fcount()) >= 14
        kplat->auto_code := a_out[i][11]
      endif
      if kplat->(fcount()) >= 15
        kplat->trace     := a_out[i][8]
      endif
      if kplat->(fcount()) >= 16
        kplat->pan       := a_out[i][4]
      endif
    endif
    if priz = 2
      if kplat->(fcount()) >= 17
        kplat->card_tag  := a_out[i][13]
      endif
    endif
  next
  kplatClose()
endif

set(_SET_DATEFORMAT, old_format)
return

procedure cardClosePar( priz, rv)
static par1, par2, par3, par4, par5
local str1:="ЗАКР.ДНЯ:КОНТРОЛЬ"
local str2:="ЗАКР.ДНЯ:НАЧАЛО"
local str3:="ЗАКР.ДНЯ:КОНЕЦ"
local str4:="ЗАКР.ДНЯ:ИНТЕРВАЛ"
local str5:="ЗАКР.ДНЯ:ПРОВЕРКА"
local arr_time:={ "00:00", "00:30", "01:00", "01:30",;
                  "02:00", "02:30", "03:00", "03:30",;
                  "04:00", "04:30", "05:00", "05:30",;
                  "06:00", "06:30", "07:00", "07:30",;
                  "08:00", "08:30", "09:00", "09:30",;
                  "10:00", "10:30", "11:00", "11:30",;
                  "12:00", "12:30", "13:00", "13:30",;
                  "14:00", "14:30", "15:00", "15:30",;
                  "16:00", "16:30", "17:00", "17:30",;
                  "18:00", "18:30", "19:00", "19:30",;
                  "20:00", "20:30", "21:00", "21:30",;
                  "22:00", "22:30", "23:00", "23:30" }
local arr_yes:={ "ДА", "НЕТ" }
local arr_min:={ "10", "20", "30"}
local s1, s2, s3, s4
local time_beg, time_end, time_zap, time_tek

if len( posPath()) = 0
  return
endif

rv:=.F.

if priz = 1
  if temp_posC( str1) = .F.       //1 - контролировать закрытие
    temp_posW( str1, , , 1)       //2 - не контролировать
  else
    par1 := temp_posR( str1, "N")
  endif
  //начальная установка закрытия дня
  if temp_posC( str2) = .F.
    temp_posW( str2, "19:00")    //время начала контроля
  else
    par2 := temp_posR( str2, "C")
  endif
  if temp_posC( str3) = .F.
    temp_posW( str3, "23:00")     //контролировать до времени
  else
    par3 := temp_posR( str3, "C")
  endif
  if temp_posC( str4) = .F.       //автоматически запускать
    temp_posW( str4, , , 30)      //через 10, 20 или 30 минут (в минутах)
  else
    par4 := temp_posR( str4, "N")
  endif
  if temp_posC( str5) = .F.       //время послед. автоматич.
    temp_posW( str5, "19:00")     //запуска закрытия дня
  else
    par5 := temp_posR( str5, "C")
  endif
elseif priz = 2
  s1 := par1
  s2 := ascan( arr_time, par2)
  s3 := ascan( arr_time, par3)
  s4 := ascan( arr_min,  str(par4, 2))
  if getup(5,15,{{"АВТОМАТИЧЕСКИЙ КОНТРОЛЬ ЗАКРЫТИЯ ДНЯ"},;
                 {"   ПРИ РАБОТЕ С МАГНИТНЫМИ КАРТАМИ  "},;
                 {"Производить контроль    ",   block( s1), arr_yes},;
                 {"Время начала контроля  ",    block( s2), arr_time},;
                 {"Время оконч. контроля  ",    block( s3), arr_time},;
                 {"Интервал контроля (в мин.)", block(s4), arr_min}})
    if s2 != s3
      par1 := s1
      temp_posW( str1, , , par1)
      par2 := alltrim( arr_time[s2])
      temp_posW( str2, par2)
      par3 := alltrim( arr_time[s3])
      temp_posW( str3, par3)
      par4 := val( arr_min[s4])
      temp_posW( str4, , , par4)
      par5 := par2              //снова выставляем время первого
                                //предполагаемого контроля закрытия дня
                                //равным времени начала контроля
      temp_posW( str5, par5)
      rv:=.T.
    else
      dialog({ "Время начала не должно совпадать со временем",;
               "окончания контроля. Введите параметры корректно" },;
             { "Ok" })
    endif
  endif
elseif priz = 3
  //производить проверку
  if par1 = 1
    //текущий день еще не закрыт
    if temp_posR( "НОМЕР ДНЯ", "N") = 0
      time_beg:=timeToMin( par2)
      time_end:=timeToMin( par3)
      if time_end < time_beg
        time_end += 3600
      endif
      time_zap:=timeToMin( par5)
      if time_zap < time_beg
        time_zap += 3600
      endif
      time_tek:=timeToMin( time())
      if time_tek < time_beg
        time_tek += 3600
      endif
      //текущее время попало в интервал, ограниченный
      //временем начала и окончания контроля, а также
      //превысило предполагаемое время запуска процедуры
      //закрытия дня
      if time_beg <= time_tek .and. time_tek < time_end .and.;
                     time_zap <= time_tek
        cardWork( 1, "", , "1", , "0", "0")
        //иначе найдем следующее предполагаемое время запуска
        time_zap := time_tek + par4
        if time_zap > 3600
          time_zap -= 3600
        endif
        par5 := minToTime( time_zap)
        temp_posW( str5, par5)
      endif
    endif
  endif
endif
return

procedure cardCloseDop()
local priz, i, rv
local scr00

if len( posPath()) = 0
  return
endif

if temp_posR( "НОМЕР ДНЯ", "N") = 1
  messageB("Последний день уже был закрыт")
else
  messageStartUp(;
       padc("Перед запуском процедуры контроля проверьте настройки", 78, " "))
  cardClosePar( 2, @rv)
  messageExit()
  if rv = .T.
    if temp_posR( "ЗАКР.ДНЯ:КОНТРОЛЬ", "N") = 2
      i := dialog({"В настройках закрытия дня флаг 'Производить контроль'   ",;
                   "установлен в 'НЕТ'.Процедура контроля выполнена не будет"},;
                  { "Ok" })
    else
      scr00:=savescreen( 1, 0, 7, 79)
      @ 1, 10 say "┌────────────────────────────────────────────────────────┐" color "W/R"
      @ 2, 10 say "│                КОНТРОЛЬ  ЗАКРЫТИЯ  ДНЯ                 │" color "W/R"
      @ 3, 10 say "│                                                        │" color "W/R"
      @ 4, 10 say "│                                                        │" color "W/R"
      @ 5, 10 say "│                                                        │" color "W/R"
      @ 6, 10 say "│         ВЫХОД  ИЗ ПРОЦЕДУРЫ  ПО КЛАВИШЕ  <ESC>         │" color "W/R"
      @ 7, 10 say "└────────────────────────────────────────────────────────┘" color "W/R"
      do while inkey(1) != K_ESC .and. temp_posR( "НОМЕР ДНЯ", "N") = 0
        @ 3, 12 say  "Контроль осуществляется с "+;
                     temp_posR( "ЗАКР.ДНЯ:НАЧАЛО", "C")+;
                     " по "+;
                     temp_posR( "ЗАКР.ДНЯ:КОНЕЦ", "C") ;
                     color "W/R"
        @ 4, 12 say  "Текущее время "+time() color "W/R"
        @ 5, 12 say  "Время следующего запуска процедуры закрытия дня "+;
                     temp_posR( "ЗАКР.ДНЯ:ПРОВЕРКА", "C") ;
                     color "W/R"
        cardClosePar( 3)
      enddo
      restscreen( 1, 0, 7, 79, scr00)
    endif
  endif
endif
return

function timeToMin( str1)
return val(substr( str1, 1, 2)) * 60 + val(substr( str1, 4, 2))

function minToTime( min1)
local hour, minute
do while min1 > 3600
  min1 -= 3600
enddo
hour   := int( min1 / 60)
minute := int( min1 % 60)
return padl( hour, 2, "0")+":"+padl( minute, 2, "0")

procedure cardCheckClose()

if len( posPath()) = 0
  return
endif

if temp_posR( "НОМЕР ДНЯ", "N") = 0
  dialog({"ВНИМАНИЕ !!!",;
          "При работе с Центром Авторизации Вы не закрыли последний день",;
          "Не забудьте его закрыть при следующем запуске программы"},;
         {"Ok"})
endif
return

procedure dialogTime( arrText, prizNoKey, nsec)
local scr00, maxX:=0, maxY:=0
local x, y, i
local oldY, oldX, oldC
if prizNoKey == "0"
  dialog( arrText, {"Ok"})
elseif prizNoKey == "1"
  oldY := row()
  oldX := col()
  oldC := setCursor( 0)
  for i:=1 to len(arrText)
    if maxY = (maxRow() - 8)
      exit
    endif
    maxY++
    if len(arrText[i]) > (maxCol() - 8)
      arrText[i] := substr( arrText[i], 1, maxCol() - 8)
    endif
    if maxX < len(arrText[i])
      maxX := len(arrText[i])
    endif
  next
  y:= int(maxRow() / 2 - maxY / 2 - 1)
  x:= int(maxCol() / 2 - maxX / 2 - 2)
  scr00 := savescreen( y, x, y + maxY + 1, x + maxX + 2 + 1)
  @ y, x say "┌"+replicate("─", maxX+2)+"┐" color "W/B"
  for i:=1 to maxY
    @ y+i, x say "│ "+padc(arrText[i], maxX, " ")+" │" color "W/B"
  next
  @ y+maxY+1, x say "└"+replicate("─", maxX+2)+"┘" color "W/B"
  inkey(nSec)
  restscreen( y, x, y + maxY + 1, x + maxX + 2 + 1, scr00)
  setpos( oldY, oldX)
  setCursor( oldC)
endif
return

function cardNewTek( new)
static old
if pcount() = 1
  old:=new
endif
return old

function kplatOpen()
local rv:=.F.
local card_path:=posPath()  //base_path()+alltrim("card\ ")
local card_name:="k_"+temp_posR( "НОМЕР ДНЯ", "C")+".dbf"
local kplatDbf := kplatDefs

if !file( card_path+card_name)
  dbcreate( card_path+card_name, kplatDbf[3])
endif
if file( card_path+card_name)
  USE ( card_path+card_name ) NEW ALIAS kplat EXCLUSIVE
  rv:=!neterr()
endif
if !rv
  messageB("Не открыть файл "+card_path+card_name)
endif
return rv

procedure kplatClose()
  kplat->(dbclosearea())
return

function strToDateTime( str)
local str1:=""
if str != "000000000000" .and. str != "            " .and. len(str) = 12
  str1:=substr(str,5,2)+"/"+substr(str,3,2)+"/"+substr(str,1,2)+"  "+;
        substr(str,7,2)+":"+substr(str,9,2)+":"+substr(str,11)
else
  str1:=space(18)
endif
return  str1

static function getOtd()
local str1
#ifdef TOWN
  str1:=otd()
#else
  str1:=confOtdNumb()
#endif
return str1
//27-12-99
function getTips( s1, s2, p1)
local s3, i
s3:='CASHIER OFFICE '+getOtd()
if !empty( s1) .and. s1 != '0'
  i:=at(s1, kodTips)
  if i > 0
    s3:=aTips[ i]+' '+alltrim(s2)+if( p1, ' '+getOtd(), '')
  endif
endif
return padc(s3, 35)