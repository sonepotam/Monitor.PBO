//09-12-98 ��ࠡ�⪠ ��ਮ�� ����⢨� ᮮ�饭��
//10-09-99 ����� �� ���� ������ ᮮ�饭��
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

//���� � ���� ᮮ�饭�� �� ���� - �����頥� ᮮ�饭�� � ��� ⨯
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
   //�஢�ઠ �� ����
   if left( schet,5) == '42301' .OR. left( schet,5) == '40817'    //OVD 18.07.2005
     acc->(basesavepos())
     acc->(dbsetorder(1))
     if acc->( dbseek( schet)) .and. at( acc->priz,"15 1  1  2  ") > 0
       rv:={ "����� ࠡ���� � ����", 2, ctod(""), ctod(""), 1}
     endif
     acc->(baserestpos())
   endif
   dbselectarea(old_area)
return rv

//���������� �������������� ���������
procedure spr_infoWork()
local old_area:=select()
local flag
local arrinf:={"����� ࠡ����","����� ࠡ����"}
local arrhelp:={;
          "             ������ �� ࠡ�� � �����蠬�              ",;
          "",;
          "�� ������         <F1> - ������ �� ࠡ�� � �����蠬� ",;
          "�� ������         <F2> - ���� ������                  ",;
          "�� ������         <F4> - ���४�� ������             ",;
          "�� ������         <F5> - ����� �� �⬥⪠�           ",;
          "�� ������         <F7> - ����                        ",;
          "�� ������         <F8> - ��������                     ",;
          "�� ������        < + > - �⬥⪠ ��� ���� ���ଠ樨",;
          "�� ������        < - > - ����� �⬥�� � ����       ",;
          "�� ������        <F10> - ����� �� ���������           ",;
          "-------------------------------------------------------",;
          "�� ������        <ESC> - ����� �� ���������           ",;
          "",;
          "��� ������ �� ���� ��������� ������� �������  <ESC>    "}
local aH:={{padc("����������   ��������������  ���������", 70)}, {""}}
local aC:={{ "��.",            {|| Otmmes() },                       .F. },;
           { " ����� ��� ",   {|| accNew20Space(spr_info->schet) }, .F. },;
           { " ����饭�� ",     {|| spr_info->message },              .F. },;
           { " ��� ᮮ�饭�� ", {|| funcmes(spr_info->tip_mes) },     .F. },;
           { " ��ਮ� ����⢨�",{|| spr_infoPe(spr_info->dto_beg,;
                                               spr_info->dto_end)},   .F. }}
local fk:={{},;
           {{"����",   {|| spr_infoVvod( arrinf)} }},;
           {},;
           {{"������", {|| spr_infoEdit(arrinf) } }},;
           {{"������", {|| spr_infoPrint()      } }},;
           {},;
           {{"�����",  {|| spr_infoFind()       } }},;
           {{"������", {|| spr_infoDel()        } }},;
           {},;
           {{"�����",  {|| _out()               } }} }
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
  @ wstWindTop()[3]-2, wstWindTop()[2]+5 say "<Ins - ���� ������ ᮮ�饭��>"
  browseProc( 1, 1, ob, aH, aC, , bApp, , fk)
  browseExit()
  //netBrw( 1, 1, aC, aB,,,,,,{|| viewHelp(arrhelp)}, aF)
  spr_infoClose( flag)
  delMesKey()
endif
dbselectarea( old_area)
return

// ���� � ���� ᮮ�饭��
function spr_infoVvod( arrinf)
local message:=space(40), schet, i
local oB:=getCurOB()
local dtob:=ctod(""), dtoe:=ctod("")
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yyyy")
local rV := .F.

delmeskey()
acc->(basesavepos())
acc->(dbsetorder(1))
schet:=numBQ(8, 10, , "���, ��� ���ண� ����室��� ����� ᮮ�饭��")
if !empty(schet)
  if acc->(dbseek( schet))
    if spr_info->(dbseek(schet))
      dialog({"�� ���� "+accNew20Space(schet)+" � ���� 㦥 ���� ᮮ�饭��",;
              "�����樮������� �� ��� � �⪮�४���� ᮮ�饭��"},;
             {"Ok"})
    else
      i:=1
      if getUp(7,15,;
         {{ "���          " + accNew20Space(schet)},;
          { "����饭��    ", block(message)},;
          { "��� ᮮ�饭��", block(i), arrinf},;
          { "�������⥫쭮  ����� 㪠���� ��ਮ� ����⢨� ᮮ�饭��."},;
          { "�᫨ �� 㪠����  ��� ��砫�, � ᮮ�饭�� ������� � "},;
          { "������ �����. �᫨ �� 㪠���� ��� ����砭�� ����⢨�,"},;
          { "� ����⢨� ᮮ�饭�� ����࠭�祭�.                    "},;
          { "��砫� ����⢨�", block(dtob)},;  //09-12-98
          { "*����砭��",      block(dtoe)}})  //09-12-98
        //10-09-99
        if !empty(alltrim(message))
          //09-12-98 ��������. �஢�ઠ
          if !empty(dtob) .and. !empty(dtoe) .and. dtob > dtoe
            messageb("����୮ ������ ��ਮ� ����⢨�")
          else
            messageStartUp(" ... ���� ���� ������ � ���� spr_info.DBF ")
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
          messageB("����� ������� ���⮥ ᮮ�饭��")
        endif
      endif
      oB:refreshAll()
      delay while .NOT. oB:stabilize()
    endif
  else
    messageB(schet+" - ��� ��������� � �ࠢ�筨�� ��⮢")
  endif
endif
acc->(baserestpos())
setmeskey()
set(_SET_DATEFORMAT, old_format)
return rV

function funcmes(num)
local rv
if num = 1
  rv:="����� ࠡ���� "
elseif num = 2
  rv:="����� ࠡ����"
else
  rv:="               "
endif
return rv

//���� �� ������ ��� � ���� ���ଠ樮���� ᮮ�饭��
procedure spr_infoFind()
local oB:=getCurOB(), schet
local old_rec:=spr_info->(recno()), new_rec
delmeskey()
schet:=numBQ( 8, 10, , "������ ��� ��� ���᪠")
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
    messageB(accNew20Space(schet) + " - ��� ��������� � ���� ᮮ�饭��")
  endif
endif
setmeskey()
return
// �������� �� ����������� ���� ���������
procedure spr_infoDel()
local oB:=getCurOB()
delmeskey()
if dialog({"������� �� �ࠢ�筨�� ᮮ�饭�� �� ���� "+;
           accNew20Space(spr_info->schet)},;
          {"���", "��"}) = 2
  if spr_info->(mRlock()) //�᫨ ��墠⨫� ������
    spr_info->(mDelete())
    spr_info->(netUnLock())
    oB:refreshAll()
    delay while .NOT. oB:stabilize()
  endif
endif
setmeskey()
return
// ������஢���� ���� ���������
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
   {{ "���          " + accNew20Space(spr_info->schet)},;
    { "����饭��    ", block(message)},;
    { "��� ᮮ�饭��", block(i), arrinf },;
    { "�������⥫쭮  ����� 㪠���� ��ਮ� ����⢨� ᮮ�饭��."},;
    { "�᫨ �� 㪠����  ��� ��砫�, � ᮮ�饭�� ������� � "},;
    { "������ �����. �᫨ �� 㪠���� ��� ����砭�� ����⢨�,"},;
    { "� ����⢨� ᮮ�饭�� ����࠭�祭�.                    "},;
    { "��砫� ����⢨�", block(dtob)},;  //09-12-98
    { "*����砭��",      block(dtoe)}})  //09-12-98
  //10-09-99
  if !empty(alltrim(message))
    //09-12-98 ��������. �஢�ઠ
    if !empty(dtob) .and. !empty(dtoe) .and. dtob > dtoe
      messageb("����୮ ������ ��ਮ� ����⢨�")
    else
      messageStartUp("������ � ���� spr_info")
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
    messageB("����� ������� ���⮥ ᮮ�饭��")
  endif
endif
oB:refreshAll()
delay while .NOT. oB:stabilize()
setmeskey()
set(_SET_DATEFORMAT, old_format)
return

// ��� ���� �ࠢ�筨�� ����� ����権
function Otmmes()
if ascan( arrmes,{|x| x = spr_info->(recno())}) > 0
  return '*'
endif
return ' '
//��⠭���� �⬥⪨
procedure setMesOtm()
local oB:=getCurOB()
if ascan( arrmes,{|x| x = spr_info->(recno())}) = 0
  aadd(arrmes, spr_info->(recno()))
  oB:refreshAll()
  delay while .NOT. oB:stabilize()
endif
return
//��⨥ �⬥⪨
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
//���� ��� ���� �� �ࠢ�筨�� ᮮ�饭��
procedure spr_infoPrint()
local nv:=0,i:=1
delmeskey()
spr_info->(basesavepos())
nv:=popUp( 7,23,{"������ ���� �ࠢ�筨�           ",;
                 "������ ⥪�饥 ᮮ�饭�� �� ����",;
                 "������ �⬥祭�� ���          "})
if nv > 0 .and. openPrn()
  ? chr(18)
  ? "         ��ࠢ�筨�   ���ଠ樮����  ᮮ�饭��"
  ?
  ? "      ����� ���         ����饭��                                ����⢨� ᮮ�饭��"
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

//��室 �� ��ꥪ� �� ������ F10
static procedure _out()
keyboard chr(27)
return

procedure spr_infoCWork()
local kod:=clients->code
local arr:=accAllSchets( kod), i, flag
local old_format:=set(_SET_DATEFORMAT,"dd.mm.yyyy")
local arrinf:={"����� ࠡ����","����� ࠡ����"}
local aC:={{"��ଫ���� � ���४�� ���ଠ樮���� ᮮ�饭�� �� ��⠬"},;
           {"��� ������ "+alltrim(clients->surname)+" "+;
                   alltrim(clients->name)+" "+alltrim(clients->sname)}}

if len(arr) > 0
  if spr_infoOpen( @flag)
    i:=1
    browseUp( 1, 1, aC,;
                 {{"        ���        ", {|| accNew20Space(arr[i][1])}, .F.},;
                  {"��� ���.",           {|| dateOpen(  arr[i][2])},    .F.},;
                  {"��� ����.",           {|| dateClose( arr[i][3])},    .F.},;
                  {"����饭��",            {|| prizInfo( arr[i][1])},     .F.}},;
                 {|| pred(@i)          },;
                 {|| sled(@i, len(arr))},;
                 {|| i:=1, .T.         },;
                 {|| i:=len(arr), .T.  },,,, {|| sorry()},;
                 {{},;
                  {},;
                  {},;
                  {{"������", {|| spr_infoCVvod(arr[i][1], arrinf)} }},;
                  {},;
                  {},;
                  {},;
                  {{"������", {|| spr_infoCDel( arr[i][1], arrinf)}  }} })
    spr_infoClose( flag)
  endif
else
  messageB("� ������ ���������� ���")
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
  rv:="��⠭������"
else
  rv:="���������"
endif
return rv

procedure spr_infoCVvod(schet, arrinf)
local ob:=getCurOb()
local arr:=spr_infoSeek( schet)
local message, i, str, dtob:=ctod(""), dtoe:=ctod("")
if len(arr) > 0 .and. arr[5] = 1
  messageB("����: ᮮ�饭�� ����� ।���஢���")
  return
endif
if len(arr) > 0
  message:=arr[1]
  i      :=arr[2]
  str    :="���४�� ᮮ�饭�� �� ���� "
  dtob   :=arr[3]
  dtoe   :=arr[4]
else
  message:=space(40)
  i      :=1
  str    :="���� ᮮ�饭�� �� ���� "
endif
str+=accNew20Space( schet)
if getUp(8,10,{{str},;
               {"����饭��    ", block(message)},;
               {"��� ᮮ�饭��", block(i), arrinf},;
               { "�������⥫쭮  ����� 㪠���� ��ਮ� ����⢨� ᮮ�饭��."},;
               { "�᫨ �� 㪠����  ��� ��砫�, � ᮮ�饭�� ������� � "},;
               { "������ �����. �᫨ �� 㪠���� ��� ����砭�� ����⢨�,"},;
               { "� ����⢨� ᮮ�饭�� ����࠭�祭�.                    "},;
               { "��砫� ����⢨�", block(dtob)},;  //09-12-98
               { "*����砭��",      block(dtoe)}})  //09-12-98
  //10-09-99
  if !empty(alltrim(message))
    //09-12-98 ��������. �஢�ઠ
    if !empty(dtob) .and. !empty(dtoe) .and. dtob > dtoe
      messageb("����୮ ������ ��ਮ� ����⢨�")
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
    messageB("����� ������� ���⮥ ᮮ�饭��")
  endif
endif
oB:refreshAll()
delay while .NOT. oB:stabilize()
return

procedure spr_infoCDel(schet, arrinf)
local ob:=getCurOb()
local arr:=spr_infoSeek( schet)
if len(arr) > 0 .and. arr[5] = 1
  messageB("����: ᮮ�饭�� ����� 㤠����")
  return
endif
if len( arr) > 0
  if yesNo({"�� ᮡ�ࠥ��� 㤠���� ᮮ�饭�� �� ���� "+accNew20Space(schet),;
            padr("����饭��  "+arr[1],50),;
            padr("��� ࠡ��� "+arrinf[ arr[2]],50) })
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
  str:="����࠭�祭� "
elseif empty(dtob) .and. !empty(dtoe)
  str:="�� "+dtoc(dtoe)
elseif !empty(dtob) .and. empty(dtoe)
  str:="�  "+dtoc(dtob)
else
  str:=dtoc(dtob)+"-"+dtoc(dtoe)
endif
set(_SET_DATEFORMAT, old_format)
return padc(str,21)