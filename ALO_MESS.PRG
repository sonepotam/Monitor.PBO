//09-12-98 Обработка периода действия сообщений
//10-09-99 ЗАпрет на ввод пустых сообщений
#include "al_obl.ch"
#include "common.ch"
#include "clippExt.ch"

static arrmes

function spr_info()
static spr_info:=spr_infoDefs
return spr_info

function spr_infoOpen( flag)
local rv:=.T.
if select("SPR_INFO") = 0
  if file( spr_info()[1]) = .F.
    basecreate(spr_info())
  endif
  rv  :=baseMOpen(spr_info())
  flag:=.T.
else
  flag:=.F.
endif
return rv

procedure spr_infoClose( flag)
if flag = .T.
  baseMClose(spr_info())
endif
return

//поиск в базе сообщений по счету - возвращаем сообщение и его тип
function spr_infoSeek( schet)
   local rv:={ }, flag
   local old_area :=select()


   if spr_infoOpen( @flag)
     if ( spr_info ->( dbSeek( schet)))
       if spr_info->dto_beg <= setCurDate()
         if empty( spr_info->dto_end) .or. spr_info->dto_end >= setCurDate()
           rv:={ spr_info->message, spr_info->tip_mes,;
                 spr_info->dto_beg, spr_info->dto_end,;
                 0 }
         endif
       endif
     endif
     spr_infoClose( flag)
   endif
   //проверка на НТПС
   if left( schet,5) == '42301' .OR. left( schet,5) == '40817'    //OVD 18.07.2005
     acc->(basesavepos())
     acc->(dbsetorder(1))
     if acc->( dbseek( schet)) .and. at( acc->priz,"15 1  1  2  ") > 0
       rv:={ "Нельзя работать с НТПС", 2, ctod(""), ctod(""), 1}
     endif
     acc->(baserestpos())
   endif
   dbselectarea(old_area)
return rv

//СПРАВОЧНИК ИНФОРМАЦИОННЫХ СООБЩЕНИЙ
procedure spr_infoWork()
local old_area:=select()
local flag
local arrinf:={"Можно работать","Нельзя работать"}
local arrhelp:={;
          "             Помощь по работе с клавишами              ",;
          "",;
          "по клавише         <F1> - Помощь по работе с клавишами ",;
          "по клавише         <F2> - Ввод данных                  ",;
          "по клавише         <F4> - Коррекция данных             ",;
          "по клавише         <F5> - Печать по отметкам           ",;
          "по клавише         <F7> - Поиск                        ",;
          "по клавише         <F8> - Удаление                     ",;
          "по клавише        < + > - Отметка для печати информации",;
          "по клавише        < - > - Снять отметку о печати       ",;
          "по клавише        <F10> - ВЫХОД ИЗ ПРОГРАММЫ           ",;
          "-------------------------------------------------------",;
          "по клавише        <ESC> - ВЫХОД ИЗ ПРОГРАММЫ           ",;
          "",;
          "ДЛЯ ВЫХОДА ИЗ ЭТОЙ ПОДСКАЗКИ НАЖМИТЕ КЛАВИШУ  <ESC>    "}
local aH:={{padc("СПРАВОЧНИК   ИНФОРМАЦИОННЫХ  СООБЩЕНИЙ", 70)}, {""}}
local aC:={{ "Отм.",            {|| Otmmes() },                       .F. },;
           { " Номер счета ",   {|| accNew20Space(spr_info->schet) }, .F. },;
           { " Сообщение ",     {|| spr_info->message },              .F. },;
           { " Тип сообщения ", {|| funcmes(spr_info->tip_mes) },     .F. },;
           { " Период действия",{|| spr_infoPe(spr_info->dto_beg,;
                                               spr_info->dto_end)},   .F. }}
local fk:={{},;
           {{"ВВОД",   {|| spr_infoVvod( arrinf)} }},;
           {},;
           {{"КОРРЕК", {|| spr_infoEdit(arrinf) } }},;
           {{"ПЕЧАТЬ", {|| spr_infoPrint()      } }},;
           {},;
           {{"ПОИСК",  {|| spr_infoFind()       } }},;
           {{"УДАЛЕН", {|| spr_infoDel()        } }},;
           {},;
           {{"ВЫХОД",  {|| _out()               } }} }
local bH:={|| viewHelp(arrhelp)}
local bUp :={|| netskipup()}, bDn :={|| netskipdown()},;
      bTop:={|| netgotop()},  bBtm:={|| netgobottom()}
local bApp:={|| spr_infoVvod( arrinf)}
local ob

if spr_infoOpen( @flag)
  setMesKey()
  arrmes:={ }
  dbselectarea("SPR_INFO")
  netGoTop()
  ob:=browseStartUp( 1, 1, aH, aC, bUp, bDn, bTop, bBtm, bH, fk)
  @ wstWindTop()[3]-2, wstWindTop()[2]+5 say "<Ins - ввод нового сообщения>"
  browseProc( 1, 1, ob, aH, aC, , bApp, , fk)
  browseExit()
  //netBrw( 1, 1, aC, aB,,,,,,{|| viewHelp(arrhelp)}, aF)
  spr_infoClose( flag)
  delMesKey()
endif
dbselectarea( old_area)
return

// Ввод в базу сообщений
function spr_infoVvod( arrinf)
local message:=space(40), schet, i
local oB:=getCurOB()
local dtob:=ctod(""), dtoe:=ctod("")
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yyyy")
local rV := .F.

delmeskey()
acc->(basesavepos())
acc->(dbsetorder(1))
schet:=numBQ(8, 10, , "Счет, для которого необходимо ввести сообщение")
if !empty(schet)
  if acc->(dbseek( schet))
    if spr_info->(dbseek(schet))
      dialog({"По счету "+accNew20Space(schet)+" в базе уже есть сообщение",;
              "Спозиционируйтесь на счете и откорректируйте сообщение"},;
             {"Ok"})
    else
      i:=1
      if getUp(7,15,;
         {{ "Счет          " + accNew20Space(schet)},;
          { "Сообщение    ", block(message)},;
          { "Тип сообщения", block(i), arrinf},;
          { "Дополнительно  можно указать период действия сообщения."},;
          { "Если не указана  дата начала, то сообщение действует с "},;
          { "момента ввода. Если не указана дата окончания действия,"},;
          { "то действие сообщения неограничено.                    "},;
          { "начало действия", block(dtob)},;  //09-12-98
          { "*окончание",      block(dtoe)}})  //09-12-98
        //10-09-99
        if !empty(alltrim(message))
          //09-12-98 дополнит. проверка
          if !empty(dtob) .and. !empty(dtoe) .and. dtob > dtoe
            messageb("Неверно введен период действия")
          else
            messageStartUp(" ... Ждите идет запись в базу spr_info.DBF ")
            if spr_info->(mAppend())
              rV := .T.
              spr_info->schet   := schet
              spr_info->message := upper(alltrim( message))
              spr_info->tip_mes := i
              spr_info->dto_beg := dtob
              spr_info->dto_end := dtoe
              spr_info-> (netUnLock())
            endif
            messageExit()
          endif
        else
          messageB("Нельзя вводить пустое сообщение")
        endif
      endif
      oB:refreshAll()
      delay while .NOT. oB:stabilize()
    endif
  else
    messageB(schet+" - счет отсутствует в справочнике счетов")
  endif
endif
acc->(baserestpos())
setmeskey()
set(_SET_DATEFORMAT, old_format)
return rV

function funcmes(num)
local rv
if num = 1
  rv:="Можно работать "
elseif num = 2
  rv:="Нельзя работать"
else
  rv:="               "
endif
return rv

//поиск по номеру счета в базе информационных сообщений
procedure spr_infoFind()
local oB:=getCurOB(), schet
local old_rec:=spr_info->(recno()), new_rec
delmeskey()
schet:=numBQ( 8, 10, , "Введите счет для поиска")
if !empty(schet)
  spr_info ->(DbSetOrder(1))
  if spr_info->(dbseek( schet))
    old_rec := spr_info->( recNo())
    oB: refreshAll()
    oB: forceStable()
    while spr_info->( recNo()) != old_rec .and. !spr_info->( bof())
      oB: up()
      oB: forceStable()
    enddo
  else
    spr_info->(dbgoto(old_rec))
    messageB(accNew20Space(schet) + " - счет отсутствует в базе сообщений")
  endif
endif
setmeskey()
return
// УДАЛЕНИЕ ИЗ СПРАВОЧНИКА БАЗЫ СООБЩЕНИЙ
procedure spr_infoDel()
local oB:=getCurOB()
delmeskey()
if dialog({"Удалять из справочника сообщение по счету "+;
           accNew20Space(spr_info->schet)},;
          {"Нет", "Да"}) = 2
  if spr_info->(mRlock()) //если захватили запись
    spr_info->(mDelete())
    spr_info->(netUnLock())
    oB:refreshAll()
    delay while .NOT. oB:stabilize()
  endif
endif
setmeskey()
return
// Редактирование БАЗЫ СООБЩЕНИЙ
procedure spr_infoEdit( arrinf)
local message, i, dtob, dtoe
local oB:=getCurOB()
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yyyy")

delmeskey()
message:=padl(spr_info->message,40)
i      :=spr_info->tip_mes
dtob   :=spr_info->dto_beg
dtoe   :=spr_info->dto_end
if getUp(7,15,;
   {{ "Счет          " + accNew20Space(spr_info->schet)},;
    { "Сообщение    ", block(message)},;
    { "Тип сообщения", block(i), arrinf },;
    { "Дополнительно  можно указать период действия сообщения."},;
    { "Если не указана  дата начала, то сообщение действует с "},;
    { "момента ввода. Если не указана дата окончания действия,"},;
    { "то действие сообщения неограничено.                    "},;
    { "начало действия", block(dtob)},;  //09-12-98
    { "*окончание",      block(dtoe)}})  //09-12-98
  //10-09-99
  if !empty(alltrim(message))
    //09-12-98 дополнит. проверка
    if !empty(dtob) .and. !empty(dtoe) .and. dtob > dtoe
      messageb("Неверно введен период действия")
    else
      messageStartUp("Запись в базу spr_info")
      if spr_info->(mRlock())
        spr_info->message := upper(alltrim(message))
        spr_info->tip_mes := i
        spr_info->dto_beg := dtob
        spr_info->dto_end := dtoe
        spr_info->(netUnLock())
      endif
      messageExit()
    endif
  else
    messageB("Нельзя вводить пустое сообщение")
  endif
endif
oB:refreshAll()
delay while .NOT. oB:stabilize()
setmeskey()
set(_SET_DATEFORMAT, old_format)
return

// Для печати справочника кодов операций
function Otmmes()
if ascan( arrmes,{|x| x = spr_info->(recno())}) > 0
  return '*'
endif
return ' '
//установка отметки
procedure setMesOtm()
local oB:=getCurOB()
if ascan( arrmes,{|x| x = spr_info->(recno())}) = 0
  aadd(arrmes, spr_info->(recno()))
  oB:refreshAll()
  delay while .NOT. oB:stabilize()
endif
return
//снятие отметки
procedure delMesOtm()
local oB:=getCurOB(), i, len
i:=ascan( arrmes,{|x| x = spr_info->(recno())})
if i > 0
  len:=len(arrmes)
  adel( arrmes, i)
  asize(arrmes, len-1)
  oB:refreshAll()
  delay while .NOT. oB:stabilize()
endif
return
//Меню для печати из справочника сообщений
procedure spr_infoPrint()
local nv:=0,i:=1
delmeskey()
spr_info->(basesavepos())
nv:=popUp( 7,23,{"Печатать весь справочник           ",;
                 "Печатать текущее сообщение по счету",;
                 "Печатать отмеченные счета          "})
if nv > 0 .and. openPrn()
  ? chr(18)
  ? "         Справочник   информационных  сообщений"
  ?
  ? "      Номер счета         Сообщение                                Действие сообщения"
  if nv = 1
    spr_info->(netGoTop())
    do while !spr_info->(eof())
      ? accNew20Space(spr_info->schet)+ "  " +spr_info->message+ " "+;
        alltrim(spr_infoPe(spr_info->dto_beg, spr_info->dto_end))
      spr_info->(dbskip())
    enddo
  elseif nv = 2
      ? accNew20Space(spr_info->schet)+ "  " +spr_info->message+ " "+;
        alltrim(spr_infoPe(spr_info->dto_beg, spr_info->dto_end))
  elseif nv = 3
    for i:=1 to len(arrmes)
      spr_info->(dbgoto( arrmes[i]))
      ? accNew20Space(spr_info->schet)+ "  " +spr_info->message+ " "+;
        alltrim(spr_infoPe(spr_info->dto_beg, spr_info->dto_end))
    next
  endif
  ?
  ?
  closePrn()
endif
spr_info->(baserestpos())
setmeskey()
return

static procedure setmeskey()
SetKey( ASC("+"), {|| setMesOtm()} )
setkey( ASC("-"), {|| delMesOtm()} )
return

static procedure delmeskey()
SetKey( ASC("+"), nil )
setkey( ASC("-"), nil )
return

static procedure viewHelp(arrhelp)
DelMesKey()
HelpHandler(arrhelp)
SetMesKey()
return

//выход из объекта по нажатию F10
static procedure _out()
keyboard chr(27)
return

procedure spr_infoCWork()
local kod:=clients->code
local arr:=accAllSchets( kod), i, flag
local old_format:=set(_SET_DATEFORMAT,"dd.mm.yyyy")
local arrinf:={"Можно работать","Нельзя работать"}
local aC:={{"Оформление и коррекция информационных сообщений по счетам"},;
           {"Счета клиента "+alltrim(clients->surname)+" "+;
                   alltrim(clients->name)+" "+alltrim(clients->sname)}}

if len(arr) > 0
  if spr_infoOpen( @flag)
    i:=1
    browseUp( 1, 1, aC,;
                 {{"        Счет        ", {|| accNew20Space(arr[i][1])}, .F.},;
                  {"Дата откр.",           {|| dateOpen(  arr[i][2])},    .F.},;
                  {"Дата закр.",           {|| dateClose( arr[i][3])},    .F.},;
                  {"Сообщение",            {|| prizInfo( arr[i][1])},     .F.}},;
                 {|| pred(@i)          },;
                 {|| sled(@i, len(arr))},;
                 {|| i:=1, .T.         },;
                 {|| i:=len(arr), .T.  },,,, {|| sorry()},;
                 {{},;
                  {},;
                  {},;
                  {{"КОРРЕК", {|| spr_infoCVvod(arr[i][1], arrinf)} }},;
                  {},;
                  {},;
                  {},;
                  {{"УДАЛЕН", {|| spr_infoCDel( arr[i][1], arrinf)}  }} })
    spr_infoClose( flag)
  endif
else
  messageB("У клиента отсутствуют счета")
endif
set(_SET_DATEFORMAT, old_format)
return

function accAllSchets( kod)
local arr:={ }
acc->(basesavepos())
acc->(dbsetorder(2))
acc->(dbseek( kod,.T.))
do while !acc->(eof()) .and. acc->code == kod
  aadd( arr, { acc->schet, acc->datopen, acc->datclose, acc->priz })
  acc->(dbskip())
enddo
acc->(baserestpos())
return arr

static function dateOpen( dto)
return dtoc(dto)

static function dateClose( dto)
return dtoc(dto)

static function prizInfo( schet)
local rv
if len(spr_infoSeek( schet)) > 0
  rv:="Установлено"
else
  rv:="Отсутствует"
endif
return rv

procedure spr_infoCVvod(schet, arrinf)
local ob:=getCurOb()
local arr:=spr_infoSeek( schet)
local message, i, str, dtob:=ctod(""), dtoe:=ctod("")
if len(arr) > 0 .and. arr[5] = 1
  messageB("НТПС: сообщение нельзя редактировать")
  return
endif
if len(arr) > 0
  message:=arr[1]
  i      :=arr[2]
  str    :="Коррекция сообщения по счету "
  dtob   :=arr[3]
  dtoe   :=arr[4]
else
  message:=space(40)
  i      :=1
  str    :="Ввод сообщения по счету "
endif
str+=accNew20Space( schet)
if getUp(8,10,{{str},;
               {"Сообщение    ", block(message)},;
               {"Тип сообщения", block(i), arrinf},;
               { "Дополнительно  можно указать период действия сообщения."},;
               { "Если не указана  дата начала, то сообщение действует с "},;
               { "момента ввода. Если не указана дата окончания действия,"},;
               { "то действие сообщения неограничено.                    "},;
               { "начало действия", block(dtob)},;  //09-12-98
               { "*окончание",      block(dtoe)}})  //09-12-98
  //10-09-99
  if !empty(alltrim(message))
    //09-12-98 дополнит. проверка
    if !empty(dtob) .and. !empty(dtoe) .and. dtob > dtoe
      messageb("Неверно введен период действия")
    else
      if spr_info->(dbseek(schet))
        if spr_info->(mrlock())
          spr_info->message:=upper(alltrim( message))
          spr_info->tip_mes:=i
          spr_info->dto_beg:=dtob
          spr_info->dto_end:=dtoe
          spr_info->(netunlock())
        endif
      else
        if spr_info->(mappend())
          spr_info->schet  :=schet
          spr_info->message:=upper(alltrim( message))
          spr_info->tip_mes:=i
          spr_info->dto_beg:=dtob
          spr_info->dto_end:=dtoe
          spr_info->(netunlock())
        endif
      endif
    endif
  else
    messageB("Нельзя вводить пустое сообщение")
  endif
endif
oB:refreshAll()
delay while .NOT. oB:stabilize()
return

procedure spr_infoCDel(schet, arrinf)
local ob:=getCurOb()
local arr:=spr_infoSeek( schet)
if len(arr) > 0 .and. arr[5] = 1
  messageB("НТПС: сообщение нельзя удалить")
  return
endif
if len( arr) > 0
  if yesNo({"Вы собираетесь удалить сообщение по счету "+accNew20Space(schet),;
            padr("Сообщение  "+arr[1],50),;
            padr("Тип работы "+arrinf[ arr[2]],50) })
    if spr_info->(dbseek(schet))
      spr_info->(mdelete())
      spr_info->(netunlock())
    endif
  endif
endif
oB:refreshAll()
delay while .NOT. oB:stabilize()
return

static function spr_infoPe( dtob, dtoe)
local str:=""
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yyyy")
if empty(dtob) .and. empty(dtoe)
  str:="Неограничено "
elseif empty(dtob) .and. !empty(dtoe)
  str:="По "+dtoc(dtoe)
elseif !empty(dtob) .and. empty(dtoe)
  str:="С  "+dtoc(dtob)
else
  str:=dtoc(dtob)+"-"+dtoc(dtoe)
endif
set(_SET_DATEFORMAT, old_format)
return padc(str,21)