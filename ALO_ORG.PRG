#include 'common.ch'
#include 'clippext.ch'
#include "getexit.ch"
#include "inkey.ch"

static aOrg, aKod, aNazn, aNaznKod
static kodOrg, kodNazn, kodAlg, kodPrc

// 17-06-2002 TMV
// ���-� alo_orgClose() �������� �.�., ��, �᫨ ������� �� ��ॢ�� ࠢ�� 0,
// � �ணࠬ�� �� ��⠥��� ᤥ���� �஢���� �� �����ᨨ.
// 

function orgInfoDbf()
static dbInfo:={ ".\orgInfo.dbf", "orgInfo",;
               {{ "code2",    "C",   2, 0},;
                { "code3",    "C",   2, 0},;
                { "code",     "C",   6, 0},;
                { "name",     "C", 160, 0},;
                { "shortname","C",  20, 0},;
                { "inn",      "C",  12, 0},;
                { "schet",    "C",  40, 0},;
                { "bank",     "C", 160, 0},;
                { "rekv",     "C", 160, 0},;
                { "bik",      "C",   9, 0},;
                { "alg_vozn", "C",   1, 0},;
                { "prc_fiks", "N",  10, 2}},;
               { }, 0, .F.}
return dbInfo

procedure alo_orgNazn
local old_area, kod2, kod3, kod

old_area:=select()
aNazn:={ }
aNaznKod:={ }
kod2:=alltrim(substr(kodOrg,1,2))
kod3:=alltrim(substr(kodOrg,3,2))
kod :=alltrim(substr(kodOrg,5,6))
assort->(dbsetorder(1))
if assort->(dbseek(kod2+kod3+kod))
  do while !assort->(eof()) .and. assort->code2 == kod2 .and.;
            assort->code3 == kod3 .and. assort->code = kod
    aadd( aNazn, alltrim(assort->name))
    aadd( aNaznKod, alltrim(assort->buffer))
    assort->(dbskip())
  enddo
else
  if assort->(dbseek(kod2+kod3+"ALL"))
    do while !assort->(eof()) .and. assort->code2 == kod2 .and.;
              assort->code3 == kod3 .and. upper(alltrim(assort->code)) == "ALL"
      aadd( aNazn, alltrim(assort->name))
      aadd( aNaznKod, alltrim(assort->buffer))
      assort->(dbskip())
    enddo
  endif
endif
dbselectarea(old_area)
return

function isAlgInclude( kod)
local str1, str2
if at(kodAlg,'0,1,2,3') > 0
  str2:=alltrim(str(kodPrc,10,2))
  if at(kodAlg,'0,2') > 0
    str2:=str2+'% '
  elseif at(kodAlg,'1,3') > 0
    str2:=str2+' ��. '
  endif
else
  str2:='��������, '
endif
if at(kodAlg,'0,1,4') > 0
  str1:='����祭� � ���⥦'
elseif at(kodAlg,'2,3,5') > 0
  str1:='������. � ���⥦�'
endif
return padr(str2+str1,40)

function provSumVozn( get)
local rv:=.F.
  if kodAlg = '4' .or. kodAlg = '5'
    rv:=.T.
  endif
return rv

function checkSumVozv( get)
local rv:=.T., sum1, sum2

public getList

sum1:=get:varget()
sum2:=getList[3]:varget()
if sum1 >= 0
  if kodAlg = '4'
    if sum1 > sum2
      messageB("�㬬� ������ࠦ����� �� ����� ���� ����� ���⥦�")
      rv:=.F.
    endif
  endif
else
  rv:=.F.
endif  
return rv

function alo_orgVvod
local orgName:=space(20), nazn:=space(60)
local fio:=space(50), sumPlat:=0, sumVozn:=0
local aName, str1, rv:=.F., ind, arr, old_area

aOrg:={ }
aKod:={ }
aNazn:={ }
aNaznKod:={ }
kodOrg:=""
orgInfo->(dbgotop())
do while !orgInfo->(eof())
  if orgInfo->code2 = "50"
    aadd( aOrg, orgInfo->shortName)
    aadd( aKod, padr(orgInfo->code2,2)+padr(orgInfo->code3,2)+;
                padr(orgInfo->code,6))
    //����� ��� ��ࢮ� �࣠����樨, ����饩 ᯨ᮪ �����祭�� ���⥦�
    if len(kodOrg) = 0
      kodOrg :=aKod[1]
      orgName:=aOrg[1]
      kodAlg :=orgInfo->alg_vozn
      kodPrc :=orgInfo->prc_fiks
      str1:="������ ����. "+alltrim(orgInfo->name)+" ��� "+orgInfo->bik+;
            " ��� "+orgInfo->inn
      aName:=razbivka(str1, {{70,"L"},{70,"L"}})
      alo_orgNazn()
      if len(aNazn) > 0
        nazn   :=padr(aNazn[1],60)
        kodNazn:=aNaznKod[1]
      else
        nazn   :=space(60)
        kodNazn:=""
        kodOrg :=""  //�㤥� �᪠�� ᫥������ �࣠������
      endif
    endif
  endif
  orgInfo->(dbskip())
enddo
if len(aOrg) = 0
  messageB("��������� ᯨ᮪ �࣠����権")
  return .F.
endif
if len(kodOrg) = 0
  messageB("��������� ᯨ᮪ �����祭�� ���⥦� ��� ��� �࣠����権")
  return .F.
endif
if getUp(5,2,{{"���� ������ ��� ��� ��-4"},;
              {"��������������������������"},;
              {"�࣠������   ", block(orgName), , , ,;
                                 {|get| getVvodOrg( get)}},;
              {aName[1]},;
              {aName[2]},;
              {"���⥫�騪/���", block(fio)},;
              {"�㬬� ���⥦� ", block(sumPlat), '9999999.99', ,;
                                 {|get| checkSumVvod( get)}},;
              {"������ࠦ�����", block(sumVozn), '9999999.99',;
                                 {|get| provSumVozn( get)},;
                                 {|get| checkSumVozn( get)}},;
              {"*"+isAlgInclude()},;
              {"����.���⥦�  ", block(nazn), , , ,;
                                 {|get| getVvodNazn(get)}}})
  if sumPlat <= 0
    messageB("�� ⠪ � �� ����� �㬬� ���⥦�")
    return .F.
  endif
  if len(alltrim(nazn)) = 0
    messageB("��᪮࡭�, �� �� �� ��ࠫ� �����祭�� ���⥦�")
    return .F.
  endif
  if at(kodAlg,'0,2') > 0
    sumVozn:=round(sumPlat / 100 * kodPrc, 2)
  endif
  if at(kodAlg,'0,1,4') > 0
    if sumPlat <= sumVozn
      messageB("�㬬� ������ࠦ����� ��������� ����� �㬬� ���⥦�")
      return .F.
    endif
  elseif at(kodAlg,'2,3,5') > 0
    sumPlat:=round(sumPlat+sumVozn,2)
  endif
  ind:=dialog({"�஢���� �������� ����� !!!",;
               padr("�࣠������  "+orgName,60),;
               padr("����.���⥦� "+nazn,60),;
               padr("���⥫�騪   "+fio,60),;
               padr("���� �㬬� ���⥦� : "+mstr(sumPlat),60),;
               padr("� ⮬ �᫥ �������.: "+mstr(sumVozn),60)},;
              { "��ଫ��� ��� ��-4", "�⪠������"})
  if ind = 1
    old_area:=select()
    dbselectarea("ORGINFO")
    orginfo->(dbgotop())
    locate for orginfo->code2 = alltrim(substr(kodOrg,1,2)) .and.;
               orginfo->code3 = alltrim(substr(kodOrg,3,2)) .and.;
               orginfo->code  = alltrim(substr(kodOrg,5,6))
    if found()
      arr:=docPD4( sumPlat, sumVozn, setCurDate(),;
                   alltrim(orginfo->bank), alltrim(orginfo->rekv),;
                   alltrim(orginfo->name), orginfo->inn,;
                   orginfo->bik, alltrim(orginfo->schet),;
                   alltrim(nazn), alltrim(fio))
      do while .T.
        PrintAOrder(arr)
        if yesNo("��ଠ ��-4 �ᯥ�⠭� �ࠢ��쭮 ?")
          wisc->(dbappend())
          if !neterr()
            str1:="U"+confOtdCode()+padl(wisc->(recno()),5,"0")+;
                  indDate( setCurDate())+indTime( time())
            wisc->schet :=str1
            wisc->type  :="976"
            str1:=alltrim(substr(kodOrg,1,2))+';'+alltrim(substr(kodOrg,3,2))+';'+;
                  alltrim(substr(kodOrg,5,6))+';'+alltrim(fio)+';'+;
                  kodNazn+';'+;
                  alltrim(str(sumPlat,10,2))+';'+;
                  alltrim(str(sumVozn,10,2))+';'+;
                  dtoc(setCurDate())+'; ;'
            wisc->buffer:=str1
            wisc->(dbunlock())
            rv:=.T.
          else
            messageB("�訡�� ����� ������, 㭨�⮦�� ���㬥��")
          endif
          exit
        else
          if yesNo("������� ����� ?") = .F.
            exit
          endif
        endif
      enddo
    else
      messageB("�訡�� ���᪠ �࣠����樨")
    endif
    dbselectarea(old_area)
  endif
endif
return rv

function docPD4( sumPl, sumVo, dto, bank, rekv, name,;
                 inn, bik, schet, nazn, fio)
local arr:={ }, str1, ind, summa
local aBank, aName, aStrNazn, aFio

aBank := razbivka( bank + " " + rekv, {{28,"L"},{30,"L"}})
aFio  := razbivka( fio, {{ 32, "L"}, { 25, "L"}})
aName := razbivka( name, {{32,"L"},{43,"L"}})
if len(alltrim(aName[2])) > 43
  aName[1]:=substr(aName[1],1,40)+"..."
endif

str1:=alltrim(nazn)
if sumVo > 0
  ind:=int(sumVo+0.001)  //�㡫�
  str1:=str1+" � �.�. ���-"+alltrim(str(ind,10))+" ��."
  ind:=int((sumVo - ind) * 100 + 0.001)  //�������
  if ind < 10
    str1:=str1+"0"+alltrim(str(ind,10))+" ���."
  else
    str1:=str1+alltrim(str(ind,10))+" ���."
  endif
endif

aStrNazn:=razbivka(str1, {{22,"L"},{43,"L"}})

ind:=int(sumPl+0.001)  //�㡫�
str1:=alltrim(str(ind,10))+" ��."
ind:=int((sumPl - ind) * 100 + 0.001)  //�������
if ind < 10
  str1:=str1+"0"+alltrim(str(ind,10))+" ���."
else
  str1:=str1+alltrim(str(ind,10))+" ���."
endif
summa := str1

str1 := extractStrOpen( "pd4")
str1 := makeZ( str1, {{ "@name1",   aName[ 1]     },;
                      { "@name2",   aName[ 2]     },;
                      { "@inn",     inn           },;
                      { "@schet",   schet         },;
                      { "@bank1",   aBank[ 1]     },;
                      { "@bank2",   aBank[ 2]     },;
                      { "@bik",     bik           },;
                      { "@nazn1",   aStrNazn[ 1]  },;
                      { "@nazn2",   aStrNazn[ 2]  },;
                      { "@summa",   summa         },;
                      { "@fio1",    aFio[ 1]      },;
                      { "@fio2",    aFio[ 2]      },;
                      { "@curdate", myDtoC( dto)}})
arr := { str1, endl(), endl() }

return arr

function checkSumVvod( get)
local rV, sum1, sum2

public getList

sum1:=get:varGet()
if sum1 <= 0.00
  messageB( "������ �㬬� ���⥦�")
  rv:=.F.
else
  if at(kodAlg,'0,2') > 0
    sum2:=round(sum1 / 100 * kodPrc, 2)
    getList[4]:varput(sum2)
    getList[4]:display()
  endif
  rv:=.T.
  sum2:=getList[4]:varget()
  if at(kodAlg,'0,1,4') > 0
    if sum1 <= sum2
      message("�㬬� ���⥦� ������ ���� ����� ������ࠦ�����")
      rv:=.F.
    endif
  endif
endif
return rv

procedure getVvodOrg( get)
local str1, aName, nazn:=space(60), ind, yesCheck, key, old_area
local arr, oldKodOrg
local newName, newNazn

public getList

arr:=aclone(aOrg)
if(getprevalidate(get))
  get:setfocus()
  get:exitstate:=GE_NOEXIT
  get:changed:=.T.
  yesCheck:=.F.
  do while .T.
    key=inkey(0)
    if key = K_ENTER .or. key = K_ESC    .or.;
       key = K_DOWN  .or. key = K_UP     .or.;
       key = K_PGDN  .or. key = K_PGUP   .or.;
       key = K_TAB   .or. key = K_SH_TAB
      getapplykey(get,key)
    endif
    if (get:exitstate != GE_NOEXIT)
      /*
      if lastkey()=27
        get:exitstate:=GE_NOEXIT
        loop
      */
      if getpostvalidate(get) = .T.
        exit
      else
        get:exitstate:=GE_NOEXIT
        loop
      endif
    elseif key >= 32
      yesCheck:=.F.
      if chr(key) = " "
        ind:=popup( arr)
        if ind > 0 .and. aKod[ind] != kodOrg
          oldKodOrg:=kodOrg
          kodOrg:=aKod[ind]
          newName:=aOrg[ind]
          yesCheck:=.T.
        endif
      endif
      if yesCheck
        alo_orgNazn()
        if len(aNazn) > 0
          nazn   :=aNazn[1]
          newNazn:=aNaznKod[1]
          old_area:=select()
          dbselectarea("ORGINFO")
          orginfo->(dbgotop())
          locate for orgInfo->code2 = rtrim(substr(kodOrg,1,2)) .and.;
                     orgInfo->code3 = rtrim(substr(kodOrg,3,2)) .and.;
                     orgInfo->code  = rtrim(substr(kodOrg,5,6))
          if found()
            kodNazn:=newNazn
            kodAlg :=orgInfo->alg_vozn
            kodPrc :=orgInfo->prc_fiks
            str1:="������ ����. "+alltrim(orgInfo->name)+" ��� "+orgInfo->bik+;
                  " ��� "+orgInfo->inn
            aName:=razbivka(str1, {{70,"L"},{70,"L"}})
            str1:=isAlgInclude()
            get:varput(padr(newName,20))
            getList[2]:varput(space(50))
            getList[3]:varput(0.00)
            if at(kodAlg,'1,3') > 0
              getList[4]:varput(kodPrc)
            else
              getList[4]:varput(0.00)
            endif
            getList[5]:varput(padr(nazn,60))
            getList[2]:display()
            getList[3]:display()
            getList[4]:display()
            getList[5]:display()
            @ 9,3  say aName[1] color normalcolor( setGetColor())
            @10,3  say aName[2] color normalcolor( setGetColor())
            @13,29 say str1     color normalcolor( setGetColor())
          else
            messageB("�訡�� ���᪠ ��࠭��� �࣠����樨,"+;
                     "�࣠������ ����� �����")
            kodOrg:=oldKodOrg
            alo_orgNazn()
          endif
          dbselectarea(old_area)
        else
          messageB("��������� ᯨ᮪ �����祭�� ���⥦�,"+;
                   "�࣠������ ����� �����")
          kodOrg:=oldKodOrg
          alo_orgNazn()
        endif
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

procedure getVvodNazn( get)
local arr, key, ind

arr:=aclone(aNazn)
if(getprevalidate(get))
  get:setfocus()
  get:exitstate:=GE_NOEXIT
  get:changed:=.T.
  do while .T.
    key=inkey(0)
    if key = K_ENTER .or. key = K_ESC    .or.;
       key = K_DOWN  .or. key = K_UP     .or.;
       key = K_PGDN  .or. key = K_PGUP   .or.;
       key = K_TAB   .or. key = K_SH_TAB
      getapplykey(get,key)
    endif
    if (get:exitstate != GE_NOEXIT)
      if getpostvalidate(get) = .T.
        exit
      else
        get:exitstate:=GE_NOEXIT
        loop
      endif
    elseif key >= 32
      if chr(key) = " "
        ind:=popup( arr)
        if ind > 0
          get:varput(padr(aNAzn[ind],60))
          kodNazn:=aNaznKod[ind]
        endif
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

procedure alo_orgBrw()
local isWisc, indWisc, recWisc, old_area, isAssort, indAss, recAss


basepath( orgInfoDbf(), confPathOD())
if file(orgInfoDbf()[1])
  if baseMOpen( orgInfoDbf())
    isWisc:=.F.
    old_area:=select()
    if select("WISC") > 0
      isWisc:=.T.
      recWisc:=wisc->(recno())
      indWisc:=wisc->(indexord())
    else
      basepath( wiscInfo(), confPathOD())
      if !baseMOpen( wiscInfo())
        messageB("�訡�� ������ wisc.dbf")
      endif
    endif
    isAssort:=.F.
    if select("ASSORT") > 0
      isAssort:=.T.
      recAss:=assort->(recno())
      indAss:=assort->(indexord())
    else
      basepath(assInfo(), confPathOD())
      if !baseMOpen( assInfo())
        messageB("�訡�� ������ assort.dbf")
      endif
    endif
    //
    if select("WISC") > 0 .and. select("ASSORT") > 0
      wisc->(dbsetorder(2))
      //������ �� ᥣ���� �� ���
      if wisc->(dbseek("976")) = .F.
        if yesNo("�� �㤥� ������� ���� ���⥦ ?")
          alo_OrgVvod()
        endif
      endif
      //⠪ ����� ��-⠪� ����� ?
      if wisc->(dbseek("976"))
        dbselectarea("WISC")
        viewup(2,2,{{"������ ������� �।�⢠ �� �ਤ��᪨� ���"},;
                    {""} },;
             {{"������������",       {|| retOrgName(alltrim(wisc->buffer))}},;
              {"�㬬�",              {|| retSumPlat(alltrim(wisc->buffer))}},;
              {"������ࠦ�����",     {|| retSumVozn(alltrim(wisc->buffer))}}},;
             {|| wiscUp()      },;
             {|| wiscDown()    },;
             {|| wiscTop()     },;
             {|| wiscBottom() },,,;
             { { },;
               {{"����",  {|| addPlat()}      } },;
               {{"������", {|| alo_orgReestr()}} },;
               {{"������", {|| alo_orgClose()} } },;
               {{"�����", {|| printPlat()}    } },,,;
               {{"������", {|| delPlat()}      } },;
               {{"��୮", {|| alo_orgStorno()}} };
             })
      endif
    endif
    //
    if select("WISC") > 0
      if isWisc
        wisc->(dbsetorder(indWisc))
        wisc->(dbgoto(recWisc))
      else
        wisc->(dbclosearea())
      endif
    endif
    if select("ASSORT") > 0
      if isAssort
        assort->(dbsetorder(indAss))
        assort->(dbgoto(recAss))
      else
        assort->(dbclosearea())
      endif
    endif
    //
    baseMClose( orgInfoDbf())
    dbselectarea(old_area)
  else
    messageB("�訡�� ������ 䠩�� orgInfo.dbf")
  endif
else
  messageB("��������� 䠩� � �࣠�����ﬨ")
endif
return


static procedure wiscTop()
wisc->(dbseek( "976"))
return

static procedure wiscBottom()
wisc->(dbseek( "977", .T.))
wisc->(dbskip(-1))
return

static function wiscUp()
local old_rec, rv:=.F.

old_rec:=wisc->(recno())
wisc->(dbskip(-1))
if !wisc->(bof()) .and. alltrim(wisc->type) = "976"
  rv:=.T.
else
  wisc->(dbgoto(old_rec))
endif
return rv

static function wiscDown()
local old_rec, rv:=.F.

old_rec:=wisc->(recno())
wisc->(dbskip())
if !wisc->(eof()) .and. alltrim(wisc->type) = "976"
  rv:=.T.
else
  wisc->(dbgoto(old_rec))
endif
return rv

static function retOrgName( str1)
local name:="", old_area, str2, str3, str4

old_area:=select()
dbselectarea("ORGINFO")
str3:=razborka( str1, ';', , 1)
str4:=razborka( str1, ';', , 2)
str2:=razborka( str1, ';', , 3)
orgInfo->(dbgotop())
locate for orgInfo->code2 == str3 .and.;
           orgInfo->code3 == str4 .and.;
           orgInfo->code  == str2
if found()
  name:=orgInfo->shortName
endif
dbselectarea(old_area)
return padr(name,20)

static function retSumPlat( str1)
local str2
str2:=razborka( str1, ';', , 6)
return padl(str2,15)

static function retSumVozn( str1)
local str2
str2:=razborka( str1, ';', , 7)
return padl(str2,15)

static procedure addPlat()
local ob:=getCurOb()
local old_rec, new_rec

if isOrgDayClose()
  message("���� 㦥 ������")
  return
endif

old_rec:=wisc->(recno())
if yesNo("�� ��� ����� ���� ���⥦ ?") .and. alo_orgVvod()
  new_rec:=wisc->(recno())
  ob:refreshAll()
  ob:forceStable()
  do while !wisc->(bof()) .and. wisc->(recno()) != new_rec
    ob:up()
    ob:forceStable()
  enddo
  ob:refreshAll()
  ob:forceStable()
else
  wisc->(dbgoto(old_rec))
endif
return

static procedure printPlat()
local kod, kod2, kod3, sumPlat, sumVozn, dto, fio, nazn, old_area
local ind, str1, arr

kod :=razborka(wisc->buffer, ';', , 3)
kod2:=razborka(wisc->buffer, ';', , 1)
kod3:=razborka(wisc->buffer, ';', , 2)
old_area:=select()
dbselectarea("ORGINFO")
orginfo->(dbgotop())
locate for orginfo->code2 = kod2 .and.;
           orginfo->code3 = kod3 .and.;
           orginfo->code  = kod
if found()
  str1:=razborka(wisc->buffer, ';', , 6)
  sumPlat:=val(str1)
  str1:=razborka(wisc->buffer, ';', , 7)
  sumVozn:=val(str1)
  str1:=razborka(wisc->buffer, ';', , 8)
  dto:=ctod(str1)
  fio :=razborka(wisc->buffer, ';', , 4)
  str1:=razborka(wisc->buffer, ';', , 5)
  kodOrg:=padr(kod2,2)+padr(kod3,2)+padr(kod,6)
  nazn:=""
  alo_orgNazn()
  for ind:=1 to len(aNaznKod)
    if aNaznKod[ind] = str1
      nazn:=aNazn[ind]
      exit
    endif
  next
  arr:=docPD4( sumPlat, sumVozn, dto,;
               alltrim(orginfo->bank), alltrim(orginfo->rekv),;
               alltrim(orginfo->name), orginfo->inn,;
               orginfo->bik, alltrim(orginfo->schet),;
               alltrim(nazn), alltrim(fio))
  printAOrder(arr)
endif
dbselectarea(old_area)
return

static procedure delPlat()
local ob:=getCurOb()
local new_rec:=0, tek_rec

if isOrgDayClose()
  message("���� 㦥 ������")
  return
endif

if yesNo('�� ����⢨⥫쭮 ��� 㤠���� ���⥦ ?')
  //������ ������, �� ������ ��⠭�� ��᫥ 㤠����� ������� ���
  tek_rec:=wisc->(recno())
  wisc->(dbskip())
  if !wisc->(eof()) .and. alltrim(wisc->type) = "976"
    new_rec:=wisc->(recno())
  endif
  if wisc->(recno()) != tek_rec
    wisc->(dbskip(-1))
  endif
  if new_rec = 0
    wisc->(dbskip(-1))
    if !wisc->(bof()) .and. alltrim(wisc->type) = "976"
      new_rec:=wisc->(recno())
    endif
    if wisc->(recno()) != tek_rec
      wisc->(dbskip())
    endif
  endif
  wisc->(mdelete())
  if new_rec != 0
    wisc->(dbgoto( new_rec))
  else
    if yesNo("�� �㤥� ������� ���� ���⥦ ?") .and. alo_orgVvod()
      new_rec:=wisc->(recno())
    endif
  endif
  if new_rec != 0
    ob:refreshAll()
    ob:forceStable()
    do while !wisc->(bof()) .and. wisc->(recno()) != new_rec
      ob:up()
      ob:forceStable()
    enddo
    ob:refreshAll()
    ob:forceStable()
  else
    keyboard chr(27)  //��室 �� browseUp
  endif
endif
return

procedure alo_orgReestr
local old_rec, arr:={ }, ind:=0, kol
local kod, kod2, kod3, sumPlat, sumVozn, str1, sum1:=0, dto
local aBank, sch

old_rec:=wisc->(recno())
if wisc->(dbseek("976"))
  do while !wisc->(eof()) .and. alltrim(wisc->type) == "976"
    if ind=0
      aadd(arr,"")
      aadd(arr,"������ �ਭ���� ���� ������� �।��")
      aadd(arr,"�� "+rDate(setCurDate()))
      aadd(arr,"")
      aadd(arr,"--------------------------------------------------------------------------------")
      aadd(arr,"| ������������ � ������᪨� ४������ |   ��� �����⥫�  |                   |")
      aadd(arr,"|            �����⥫�               |                    |                   |")
      aadd(arr,"|------------------------------------------------------------------------------|")
    endif
    kod :=razborka(wisc->buffer, ';', , 3)
    kod2:=razborka(wisc->buffer, ';', , 1)
    kod3:=razborka(wisc->buffer, ';', , 2)
    dbselectarea("ORGINFO")
    orginfo->(dbgotop())
    locate for orginfo->code2 = kod2 .and.;
               orginfo->code3 = kod3 .and.;
               orginfo->code  = kod
    if found()
      str1:=alltrim(orginfo->name)+" "+alltrim(orginfo->bank)+" "+;
            alltrim(orginfo->rekv)
      aBank:=razbivka(str1,{{37,"L"},{37,"L"},{37,"L"}})
      sch :=alltrim(orginfo->schet)
    else
      aBank:={space(37)}
      sch:=""
    endif
    str1:=razborka(wisc->buffer, ';', , 6)
    sumPlat:=val(str1)
    str1:=razborka(wisc->buffer, ';', , 7)
    sumVozn:=val(str1)
    str1:=razborka(wisc->buffer, ';', , 8)
    dto:=ctod(str1)
    ind++
    sum1:=round(sum1+sumPlat,2)
    aadd(arr,"|"+padr(aBank[1],37)+"|"+padr(orginfo->schet,20)+"|"+;
             str(sumPlat,18,2)+" |")
    if len(aBank) > 1 .and. len(alltrim(aBank[2])) > 0
      for kol:=2 to len(aBank)
        if len(alltrim(aBank[kol])) = 0
          exit
        else
          aadd(arr,"|"+padr(aBank[kol],37)+;
                   "|                    |                   |")
        endif
      next
    endif
    wisc->(dbskip())
  enddo
      aadd(arr,"|------------------------------------------------------------------------------|")
      aadd(arr,"|                                                �⮣�     |"+str(sum1,18,2)+" |")
      aadd(arr,"--------------------------------------------------------------------------------")
      aadd(arr,"")
      aadd(arr,"�ਫ������ � ॥���� - "+alltrim(str(ind,6))+" ����祭��� ����饭�� �. ��-4.")
      aadd(arr,"")
      aadd(arr,"�������� � ॥��� ���㬥��� ᤠ�")
      aadd(arr,"")
      aadd(arr,"��砫쭨�   ��� N ________              ___________________/___________________/")
      aadd(arr,"")
      aadd(arr,"������    ��� N ________              ___________________/___________________/")
      aadd(arr,"�᭮���� �����")
      aadd(arr,"")
      aadd(arr,"      ��������� �⥬����")
      aadd(arr,"")
      aadd(arr,"�������� � ॥��� ���㬥��� �ਭ�")
      aadd(arr,"")
      aadd(arr,"����஫��   ��� N ________              ___________________/___________________/")
      aadd(arr,"")
      aadd(arr,"�����      ��� N ________              ___________________/___________________/")
      aadd(arr,"")
      aadd(arr,"�⠬� ���")
      aadd(arr,"")
      aadd(arr,"")
  printAOrder(arr)
  printAOrder(arr)
endif
wisc->(dbgoto(old_rec))
return

function isOrwDayClose()
local rv:=.F., old_area, recWisc, indWisc, isWisc

old_area:=select()
isWisc:=.F.
if select("WISC") > 0
  recWisc:=wisc->(recno())
  indWisc:=wisc->(indexord())
  isWisc:=.T.
else
  basepath( wiscInfo(), confPathOD())
  baseMOpen( wiscInfo())
endif
if select("WISC") > 0
  wisc->(dbsetorder(2))
  if wisc->(dbseek("976")) .and. alltrim(wisc->type) == "976"
    if isOrgDayClose()
      rv:=.T.  //���⥦� �ਭ�������, � ���� ������
    endif
  else
    rv:=.T.    //���⥦� ��.��� ����� �� �ਭ�������
  endif
  if isWisc
    wisc->(dbsetorder(indWisc))
    wisc->(dbgoto(recWisc))
  else
    baseMClose( wiscInfo())
  endif
else
  rv:=.F.  //�����-� ᡮ� - ���� �� ������
endif    
dbselectarea(old_area)
return rv


function isOrgDayClose()
local rv:=.F., old_area, recSum, indSum, isSum

old_area:=select()
isSum:=.F.
if select("SUMS") > 0
  recSum:=sums->(recno())
  indSum:=sums->(indexord())
  isSum:=.T.
else
  basepath( sumInfo(), confPathOD())
  baseMOpen( sumInfo())
endif
if select("SUMS") > 0
  sums->(dbsetorder(3))
  if sums->(dbseek("3009"))
    rv:=.T.
  endif
  if isSum
    sums->(dbsetorder(indSum))
    sums->(dbgoto(recSum))
  else
    baseMClose( sumInfo())
  endif
endif    
dbselectarea(old_area)
return rv

static procedure alo_orgClose
local isSums:=.F., isSetup:=.F., old_area, rv, theOrder, recWisc
local sumPlat, sumVozn, dto, str1, sum1:=0, sum2:=0
local ob:=getCurOb()

old_area:=select()
recWisc:=wisc->(recno())
if isOrgDayClose()
  message("���� 㦥 ������")
  return
endif
if select("SUMS") > 0
  sums->(basesavepos())
  isSums:=.T.
else
  basepath( sumInfo(), confPathOD())
  baseMOpen( sumInfo())
endif
if select("SETUP") > 0
  setup->(basesavepos())
  isSetup:=.T.
else
  basepath( setupInfo(), confPathOD())
  baseMOpen( setupInfo())
endif
if select("SUMS") > 0 .and. select("SETUP") > 0
  dbselectarea("SETUP")
  locate for rtrim(setup->paramname) = "KOR"
  if found()
    if setup->(mrlock())
      if wisc->(dbseek("976"))
        do while !wisc->(eof()) .and. alltrim(wisc->type) = "976"
          str1:=razborka(wisc->buffer, ';', , 6)
          sumPlat:=val(str1)
          str1:=razborka(wisc->buffer, ';', , 7)
          sumVozn:=val(str1)
          str1:=razborka(wisc->buffer, ';', , 8)
          dto:=ctod(str1)
          sum1:=round(sum1+sumPlat,2)
          sum2:=round(sum2+sumVozn,2)
          wisc->(dbskip())
        enddo
        str1:="U"+confOtdNumb()+dtos(setCurDate())
        rv:=kasInc( .T., sum1)
        if valType(rv) = "L" .and. rv = .T.
          if sums->(mrlock())
            sums->buffer:=str1
            sums->(dbunlock())
          endif
          theOrder:=ordIncMem( setCurDate())
//  Replaced by TMV  17-06-2002 - �⮡ ����� �뫮 ࠡ���� ��� �����ᨨ
          if sum2 > 0 .And. !sumWrite( confKorSchet(), -sum2, setCurDate(), "6009", theOrder, , str1)
            messageB("�஢���� �� ������ࠦ����� �� ᤥ����. ��筮 ᮮ��� �� 䨫���")
          else
            if wisc->(dbseek("976"))
              do while !wisc->(eof()) .and. alltrim(wisc->type) = "976"
                if wisc->(mrlock())
                  wisc->buffer:=wisc->buffer+str1+";"
                  wisc->(dbunlock())
                endif
                wisc->(dbskip())
              enddo
            endif
            messageB("����稩 ���� �� �ਥ�� ������� �।�� ��.��� ������� ������")
          endif
        endif
      endif
      setup->(dbunlock())
    else
      messageB("��������, � ��. �⠭樨 㦥 ����뢠�� ����")
    endif
  else
    messageB("�� ������� ��ப� <KOR> � setup.dbf")
  endif
endif
if select("SETUP") > 0
  if isSetup
    setup->(baserestpos())
  else
    baseMClose( setupInfo())
  endif
endif
if select("SUMS") > 0
  if isSums
    sums->(baserestpos())
  else
    baseMClose( sumInfo())
  endif
endif
dbselectarea(old_area)
wisc->(dbgoto(recWisc))
//�祭� ����᭮ - �᫨ � browse �����뢠���� ⮫쪮 2 �����,
//� ॠ�쭮 � ���� ⥪��� ������ - ᫥����� �� ������� � ����� 976
if wisc->(eof()) .or. alltrim(wisc->type) != "976"
  wiscUp()
endif
ob:refreshAll()
ob:forceStable()
return

static procedure alo_orgStorno
local old_area, isSums, str1, recWisc
local ob:=getCurOb()

if isOrgDayClose() = .F.
  message("���� �� �����")
  return
endif

old_area:=select()
recWisc:=wisc->(recno())
if select("SUMS") > 0
  sums->(basesavepos())
  isSums:=.T.
else
  basepath( sumInfo(), confPathOD())
  baseMOpen( sumInfo())
endif
if select("SUMS") > 0
  sums->(dbsetorder(3))
  if sums->(dbseek("3009"))
    if sums->(mdelete())
      if sums->(dbseek("6009"))
        if !sums->(mdelete())
          dialog({"������ �� 䨫��� !!!",;
                  "�訡�� 㤠����� �஢���� �� ������ࠦ�����",;
                  "��楤�� �⪠� �㤥� �த������"},;
                 {"Ok"})
        endif
      endif
      str1:="U"+confOtdNumb()+dtos(setCurDate())
      if wisc->(dbseek("976"))
        do while !wisc->(eof()) .and. alltrim(wisc->type) = "976"
          if right(wisc->buffer,13) == str1+";"
            if wisc->(mrlock())
              wisc->buffer := substr(wisc->buffer,1,len(wisc->buffer)-13)
              wisc->(dbunlock())
            endif
          endif
          wisc->(dbskip())
        enddo
      endif
      messageB("�⪠� �஢����")
    else
      messageB("�訡�� 㤠����� �஢���� �� ����, �⪠� �� �஢����")
    endif
  else
    messageB("�⪠� �� �஢���� - �� ������� �஢���� �� ����")
  endif
endif
if select("SUMS") > 0
  if isSums
    sums->(baserestpos())
  else
    baseMClose( sumInfo())
  endif
endif
dbselectarea(old_area)
wisc->(dbgoto(recWisc))
//�祭� ����᭮ - �᫨ � browse �����뢠���� ⮫쪮 2 �����,
//� ॠ�쭮 � ���� ⥪��� ������ - ᫥����� �� ������� � ����� 976
if wisc->(eof()) .or. alltrim(wisc->type) != "976"
  wiscUp()
endif
ob:refreshAll()
ob:forceStable()
return