//дополнительный переходник
//27.09.99 TextMonDate, TextYeaDate перенесены в basetrst.prg
#include "clippext.ch"
#include "common.ch"

static arr:={ }

function NumbQ(y, x, schet, mes)
local pl, pr, kl, rv:="", arr

if valType( mes) == "C"
  messageStartUp( mes)
endif
if empty( schet)
//  schet := "423018100" + confOtdCode() + "0000000"
    schet := "408178100" + confOtdCode() + "0000000"
endif
pl:=left  (schet, 5)
pr:=substr(schet, 9, 1)
kl:=right (schet, 7)
arr:={{ "Номер счета : ", block( pl),"@K 99999",,;
          {| g| g:varPut( padl( allTrim( g:varGet()), 5, '0')), .T. }},;
      { "*810"},;
      { "*",              block( pr),"@K 9"  ,,;
          {| g| g:varPut( padl( allTrim( g:varGet()), 1, '0')), .T. }},;
      { "*"+confOtdCode() },;
      { "*",              block( kl),"@K 9999999",,;
          {| g| g:varPut( padl( allTrim( g:varGet()), 7, '0')), .T. }}}
if getUp(y, x, arr)
  rv:=pl+"810"+pr+confOtdCode()+kl
endif
if valType( mes) == "C"
  messageExit()
endif
return rv

function accNew20Space( sch)
local rv
if empty(sch)
  rv:=space(24)
else
// Replaced by TMV  18-05-2002
//	rv:=left(sch,5)+" 810 "+substr(sch,9,1)+" "+confOtdCode()+" "+right(sch,7)
	rv:=left(sch,5)+" 810 "+substr(sch,9,1)+" "+SubStr(sch, 10, 4)+" "+right(sch,7)
endif
return rv

function OpenPrn( pR, pR1)
local rV := TRUE

   if ValType( pR)  != "C" then pR  := "PRN"
   if ValType( pR1) != "L" then pR1 := FALSE
   if Upper( pR) == "PRN"
      if ReadyPrn()
         set console off
         set printer on
      else
         rV := FALSE
      endif
   else
      set console off
      if pR1
         set alternate to ( pR) additive
      else
         set alternate to ( pR)
      endif
      set alternate on
   endif
return rV

function ReadyPrn()
   do while !isPrinter()
      if alarm( { "Принтер не готов !",                          ;
                  "Вставьте бумагу или включите <onLine>" },     ;
                  {  " Закончить ",                              ;
                     " Принтер подготовлен, продолжить " }) == 1 ;
      then return .F.
   enddo
return .T.
&& End of ReadyPrn() *

procedure ClosePrn()
      set printer off
      set console on
      set alternate off
      close alternate
return

function pred(a)
if a>1
  a--
  return .T.
endif
return .F.

function sled(a,b)
if a<b
  a++
  return .T.
endif
return .F.


//function textMonDate(dto)
//local sss:={ "  января  ", " февраля  ", "  марта   ", "  апреля  ",;
//             "   мая    ", "   июня   ", "   июля   ", " августа  ",;
//             " сентября ", " октября  ", "  ноября  ", " декабря  "}
//return sss[month(dto)]

//function textYeaDate(dto)
//return str(year(dto),4)+" г."

function nameDe_f_aul()
return alltrim(confFilial())+" ПБО "+alltrim(confOtdNumb())

function out_print()
return 2

function base_path()
static path
if empty(path)
  path:=alltrim( confPathOD())
  if len(path) > 1 .and. right(path,1) != "\"
    path+="\"
  endif
endif
return path

function exe_path()
static path
if empty(path)
  path:=alltrim( confPathExe())
  if len(path) > 1 .and. right(path,1) != "\"
    path+="\"
  endif
endif
return path

procedure monSavePos()
aadd(arr, select())
return

procedure monRestPos()
local i:=len(arr)
if len(arr) > 0
  dbselectarea(arr[i])
  asize(arr, i - 1)
endif
return

function monthNn( kol)
local arr:={ " Январь   "," Февраль  "," Март     "," Апрель   ",;
             " Май      "," Июнь     "," Июль     "," Август   ",;
             " Сентябрь "," Октябрь  "," Ноябрь   "," Декабрь  "}
if valtype(kol) = "N" .and. kol > 0 .and. kol < 13
  return arr[kol]
elseif valtype(kol) = "D" .and. !empty(kol)
  return arr[ month(kol)]
endif
return arr
