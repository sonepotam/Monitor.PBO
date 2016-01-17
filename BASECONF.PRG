*============================================================================
*          
*                  Программа : MON.EXE
*                  Файл      : baseconf.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: Описание настроечных файлов
*
*----------------------------------------------------------------------------
*
* 28-10-2002 TMV
* Введен новый вид долгосрочных поручений - перевод на свой счет в другом отд.
* Новые ф-ии confAccTransDoc()
*
* 05-01-2003 TMV
* Добавнена настройка для сортировки ордеров надомников SortHome - confInfo(),
*  confOtdEdit(), Ф-ии для ее получения confSortOrd().
* Введена ф-ия чтения настройки по KODYL - useKodYl().
*
* 16-01-2003 TMV
* Введена ф-ия чтения настройки WISK - isWiskOverWrite()
*
* 27-02-2003  TMV
* Убрана проверка номера паспорта с помощью ф-ии chckNumb(). Ф-ии cashEdit(),
*  admCurEdit().
*
* 05-02-2005 TAT
* setUpProsrPctsSchet() - счет доходов при наличии просроченных процентов
*
* 28.06.2005 OVD
* confNaimPrint() - опредаляет наименование принтера для печати из .bat файла
*

#include "common.ch"
#include "clippExt.ch"
#include "gsz.ch"

#include "tools.ch"

static curWork

static LineNoStrah  // Add by TMV  21-04-2003 - определение работы
						  // по нестраховой схеме кредитования

func confInfo
static dbInfo := { ".\CONFIG.DBF", "CONFIG",;
                   {{ "pathOD",     "C", 30} ,;
                    { "pathUser",   "C", 30} ,;
                    { "pathExe",    "C", 30} ,;
                    { "pathOutBox", "C", 30} ,;
                    { "pathInbox",  "C", 30} ,;
                    { "pathTemp",   "C", 30} ,;
                    { "filial",     "C", 30} ,;
                    { "OtdName",    "C", 30} ,;
                    { "OtdNumb",    "C",  3} ,;
                    { "OtdNode",    "C",  3} ,;
                    { "Sign1",      "C", 40} ,;
                    { "Sign2",      "C", 40} ,;
                    { "pathProt",   "C", 60} ,;
                    { "printTo",    "N",  1} ,;
                    { "printNaim",  "C", 50} ,;
                    { "showIt",     "N",  1} ,;
                    { "cnt",        "N",  3} ,;
                    { "minSumma",   "N", 15, 2},;
                    { "UnitID",     "N",  2, 0},;
                    { "BONumb",     "N",  3, 0},;
                    { "work",       "N",  1, 0},;
                    { "postName",   "C", 15, 0},;
                    { "port",       "N",  1, 0},; // Mike !!!!!!!!!!!!!!!!
                    { "ListRows",   "N",  2, 0},; // Mike !!!!!!!!!!!!!!!!
                    { "isHome",     "N",  1, 0},;
                    { "useDate",    "N",  1, 0},;
                    { "isDistr",    "N",  1, 0},;
                    { "skip20",     "N",  1, 0},;
                    { "usePaymDat", "N",  1, 0},;
                    { "useDogovor", "N",  1, 0},;
                    { "useGSZDogo", "N",  1, 0},;
                    { "printType",  "N",  1, 0},;
                    { "schKor",     "C", 20},;
                    { "Bic",        "C", 12},;
                    { "FilialName", "C", 60},;
                    { "SortHome",   "N", 1}},;  // Add by TMV  05-01-2003
                   { }, 0}
return dbInfo

funcDef confPath with newPath:CU
return basePath( confInfo(), newPath)

funcDef confOpen
return baseMOpen( confInfo())

funcDef confXOpen
return baseMXOpen( confInfo())

funcDef confCreate
return baseCreate( confInfo())

funcDef confClose
return baseMClose( confInfo())

funcDef confUseDogovor local rV, opened := Select( "CONFIG") >0, value,s
s := select()
value := 0
if !opened then rV := confOpen()
if opened .OR. rV
   value := CONFIG ->useDogovor
   if !opened then confClose()
endif
select ( s)
return value == 2 .AND. !confUseAgent()

funcDef confSkip20 local rV, opened := Select( "CONFIG") >0, value,s
s := select()
value := 0
if !opened then rV := confOpen()
if opened .OR. rV
   value := CONFIG ->skip20    
   if !opened then confClose()
endif
select ( s)
return value == 2


funcDef confCnt local rV, opened := Select( "CONFIG") >0, value := 50,s
s := select()
value := 0
if !opened then rV := confOpen()
if opened .OR. rV
   value := CONFIG ->cnt
   if !opened then confClose()
endif
select ( s)
return value 


funcDef confUseGSZDogovor local rV, opened := Select( "CONFIG") >0, value,s
s := select()
value := 0
if !opened then rV := confOpen()
if opened .OR. rV
   value := CONFIG ->useGSZDogo
   if !opened then confClose()
endif
select ( s)
return value == 2 .AND. !confUseAgent()


funcDef confKassa local rV, opened := Select( "CONFIG") >0, value := ".\",s
s := select()
value := setUpKassa()
if empty( value)
   if !opened then rV := confOpen()
   if opened .OR. rV
      value := CONFIG ->schKassa
      if !opened then confClose()
   endif
endif
select ( s)
return value

funcDef confBONumb local rV, opened := Select( "CONFIG") >0, value := .F.,s
s := select()
if empty( value)
  if !opened then rV := confOpen()
  if opened .OR. rV
    value := CONFIG ->BONumb
    if !opened then confClose()
 endif
endif
select ( s)
return value


funcDef confIsHome local rV, opened := Select( "CONFIG") >0, value := .F.,s
s := select()
if empty( value)
   if !opened then rV := confOpen()
   if opened .OR. rV
      value := CONFIG ->isHome == 2
      if !opened then confClose()
   endif
endif
select ( s)
return value


funcDef tmpGet with par:C local value := ""
  eFileWrite( { "Чтение счетчика " + par,;
                "------------------------------------------",;
                "Время    : " + DtoC( Date()) + ":" + Time()},;
                ".\counters.log")
  if TEMP ->( dbSeek( par)) 
     if TEMP ->( newRLock())
        value := rTrim( TEMP ->Str_Temp)
        eFileWrite( "Значение :  " + value, ".\counters.log")
     else
        eFileWrite( "Отказ от ожидания счетчика " + userName(), ".\counters.log")
     endif
  endif
return value

funcDef tmpUnLock
return TEMP ->( netUnLock())

funcDef newRLock local rV := .F.
   assert( used(), "mRLock в пустой области !")
   rV := .T.
   if .NOT.netRLock( 1)
      messageStartUp( "Жду освобождения базы " + alias( select()) + ;
                     " для получения значения счетчика ...")
      do while .NOT.netRLock( 5)
         loop if mLockTryMore()
         rV = .F.
         exit
      endDo
      messageExit()
   endIf
return rV


funcDef tmpSet with par:C, newValue:C local rV := .T.
   if rTrim( TEMP ->Comment) <> par
      if rV := TEMP ->( mAppend())
         TEMP ->Comment := Upper( par)
      endif
   else
      rV := TEMP ->( newRLock())
   endif
   if !rV 
      errLog( { "Не могу изменить счетчик " + par, "Обратитесь на филиал !!!"})
      return ""
   endif
   TEMP ->Str_Temp := newValue
   TEMP ->( netUnLock())
   eFileWrite( { "Запись счетчика " + par,;
                 "------------------------------------------",;
                 "Время    : " + DtoC( Date()) + ":" + Time()   ,;
                 "Значение : " + TEMP ->Str_Temp, "", ""}, ".\counters.log")   
return TEMP ->Str_Temp

funcDef confNextCliCode ;
  local theDate, theDay, theMonth, theYear, arr,;
        tmpValue, rV, opened, value, s

value    := ""
opened   := Select( "CONFIG") >0
theDate  := setCurDate()
theDay   := Day( theDate)
theMonth := Month( theDate)
theYear  := right( nStr( Year( theDate)), 1)

arr      := { "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c"}

tmpValue := "A" + padL( confOtdNumb(), 3, "0") + padL( theDay, 2, "0") + ;
      arr[ theMonth] + theYear

s := select()
value := tmpGet( "CLI_CODE")       
if tmpValue == left( value, 8)
   value := tmpValue + padL( val( right( value, 4)) + 1, 4, "0")
else
   value := tmpValue + "0001"
endif
tmpSet( "CLI_CODE", value)
tmpUnLock()
select ( s)

return value

funcDef confTrustNextCode ;
  local Year, rV, opened, value, s

value  := ""
opened := Select( "CONFIG") >0
Year   := subStr( DtoS( setCurDate()), 3, 2)
s := select()
value := tmpGet( "TRUST_CODE")
if empty( value) then value := getTrustCode()
if right( value, 2) == Year
   value := padL( val( left( value, 5)) + 1, 5, "0") + "-" + Year
else
  value := "00001-" + Year
endif
tmpSet( "TRUST_CODE", value)
tmpUnLock()
select ( s)

return value

static funcDef getTrustCode local seekStr, rV
TRUST ->( baseSavePos())
TRUST ->( dbSetOrder( 3))

seekStr := str( Year( setCurDate()) - 1900, 2) + "9999"
TRUST ->( netSeek( seekStr, .T.))

if right( TRUST ->number, 2) <> left( seekStr, 2) then TRUST ->( netSkipUp())
if right( TRUST ->number, 2) <> left( seekStr, 2)
   rV := "00001"
else
   rV := padL( val( left( TRUST ->number, 5)) + 1, 5, "0")
endif
rV    := rV + "-" + str( Year( setCurDate()) - 1900, 2)
TRUST ->( baseRestPos())

return rV

funcDef confUsePaymDate local rV, opened := Select( "CONFIG") >0, value := .F.,s
s := select()
if empty( value)
   if !opened then rV := confOpen()
   if opened .OR. rV
      value := CONFIG ->usePaymDate
      if !opened then confClose()
   endif
endif
select ( s)
return value

funcDef confUseDate local rV, opened := Select( "CONFIG") >0, value := .F.,s
s := select()
if empty( value)
   if !opened then rV := confOpen()
   if opened .OR. rV
      value := CONFIG ->useDate <= 1
      if !opened then confClose()
   endif
endif
select ( s)
return value

funcDef confGSZIncom local rV
  rV := setUpValue( "GSZIN")
return rV

funcDef confGSZS2 local rV
  rV := setUpValue( "GSZS2")
return rV

funcDef confFilNumb local rV
  rV := setUpValue( "FILNUMB")
return rV


funcDef confIsDistr local rV, opened := Select( "CONFIG") >0, value := .F.,s
   value := setUpDistr() == "yes"
return value

funcDef confWork local value
   value := val( SetUpValue( "pens"))
return value

funcDef confUseTPL local value
   value := SetUpValue( "tpl") == "yes"
return value


funcDef confUseMicro local rV
    rV := SetUpValue( "Micro") == "yes"
return rV

funcDef confNDACC local value
   value := SetUpValue( "NDACC")
return value

funcDef conf904Work with rescan:LU

DEFAULT rescan TO .F.
if isNIL( curWork) .OR. rescan
   curWork := confWork() == 2
endif

return curWork

funcDef configValue with fieldName:C local rV, opened := Select( "CONFIG") >0, value := "",s
  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG -> ( fieldGet( fieldPos( fieldName )))
     if !opened then confClose(); select ( s)
  endif
return value

//  Function add by TMV  21-12-2001
funcDef setConfigValue with fieldName:C, fieldValue:C;
	local rV, opened := Select( "CONFIG") >0, s, value:=''
  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
  		if CONFIG ->( mRLock())
      	value = CONFIG -> ( fieldPut( fieldPos( fieldName ), fieldValue))
			CONFIG ->( netUnLock())
		endif
      if !opened then confClose(); select ( s)
  endif
return (value == fieldValue)

funcDef confValue with setupName:C, configName:C local rV
  rV := setUpValue( setupName, .F. )
  if empty( rV )
    rV := configValue( configName )
  endIf
return allTrim( rV )

funcDef confStrSchet local rV, opened := Select( "CONFIG") >0, value := ".\",s

value := setUpStrSchet()
if empty( value)
   s := select()
   if !opened then rV := confOpen()
   if opened .OR. rV
      value := CONFIG ->strSchet
      if !opened then confClose(); select ( s)
   endif
endif
return value

funcDef confPctSchet ;
   local rV, opened := Select( "CONFIG") >0, value := ".\",s

value := setUpPctsSchet()
if empty( value)
   s := select()
   if !opened then rV := confOpen()
   if opened .OR. rV
      value := CONFIG ->pctSchet
      if !opened then confClose(); select ( s)
   endif
endif
return value

funcDef confPrintTo local rV, opened := Select( "CONFIG") >0, value := 0, s
  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->printTo
     if !opened then confClose(); select ( s)
  endif
return value

funcDef confNaimPrint local rV, opened := Select( "CONFIG") >0, value := 0, s
  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->printNaim
     if !opened then confClose(); select ( s)
  endif
return value

func confShowIt
static show := 0
local rV, opened := Select( "CONFIG") >0, value := 0,s

if empty( show)
   s := select()
   if !opened then rV := confOpen()
   if opened .OR. rV
      show := CONFIG ->showIt
      if !opened then confClose(); select ( s)
   endif
endif
return show

func confPrnType
static printType
local opened := Select( "CONFIG") >0, s, rV := .F.

if isNIL( printType)
   s := select()
   if !opened then rV := confOpen()
   if opened .OR. rV
      printType := CONFIG ->printType
      if !opened then confClose(); select ( s)
   endif
endif
return printType

funcDef confPrintDon()
return confPrnType() == 2

funcDef confConfName with inStation: C local rV := .F., fileName

   fileName := '.\config' + inStation + '.dbf'
   confInfo()[1] := fileName
   if .not. file(fileName) then rV := confCreate()
return rV

funcDef confPrintUsel()
return confPrnType() == 3

func confUselPort
static portNumber
local opened := Select( "CONFIG") >0, s, rV := .F.

if isNIL( portNumber)
   s := select()
   if !opened then rV := confOpen()
   if opened .OR. rV
      portNumber := CONFIG ->port
      if !opened then confClose(); select ( s)
   endif
endif
return portNumber

//Количество строк на лист при печати на фискальный регистратор
func confRowsPerList
static RowsPerList
local opened := Select( "CONFIG") >0, s, rV := .F.

if isNIL( RowsPerList)
   s := select()
   if !opened then rV := confOpen()
   if opened .OR. rV
      RowsPerList := CONFIG ->listrows
      if !opened then confClose(); select ( s)
   endif
endif
return RowsPerList

funcDef confKorSchet local rV, opened := Select( "CONFIG") >0, value := "",s

value := setUpKorSchet()
s := select()
if empty( value)
   if !opened then rV := confOpen()
   if opened .OR. rV
      value := CONFIG ->schKor
      if !opened then confClose(); select ( s)
   endif
endif
return value

funcDef confKor2Schet local value := ""
   value := setUpKor2Schet()
   if empty( value) then value := confKorSchet()
return value

// Next Functions Add by TMV  21-11-2001
funcDef confKorConfSchet local rV, opened := Select( "CONFIG") >0, value := "",s
s := select()
if !opened then rV := confOpen()
if opened .OR. rV
	if CONFIG->(FieldPos('SCHKOR')) > 0 then value := CONFIG ->schKor
	if !opened then confClose(); select ( s)
endif
return value

funcDef confBic local rV, opened := Select( "CONFIG") >0, value := "",s
s := select()
if !opened then rV := confOpen()
if opened .OR. rV
	if CONFIG->(FieldPos('BIC')) > 0 then value := Padr(CONFIG ->Bic, 9)
	if !opened then confClose(); select ( s)
endif
return value

funcDef confMinSumma local rV, opened := Select( "CONFIG") >0, value := 0,s

  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->minSumma
     if !opened then confClose(); select ( s)
  endif
return value

funcDef confPathOD local rV, opened := Select( "CONFIG") >0, value := ".\",s

  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->pathOD
     if !opened then confClose(); select ( s)
  endif
return value

funcDef confPathUser local rV, opened := Select( "CONFIG") >0, value := ".\",s

  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->pathUser
     if !opened then confClose(); select ( s)
  endif
return value

funcDef confPathTemp local rV, opened := Select( "CONFIG") >0, value := ".\",s

  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->pathTemp
     if !opened then confClose(); select ( s)
  endif
return value


funcDef confPathExe local rV, opened := Select( "CONFIG") >0, value := ".\",s

  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->pathExe
     if !opened then confClose()
  endif
  select ( s)
return value

funcDef confPathInbox local rV, opened := Select( "CONFIG") >0, value := ".\",s

  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->pathInbox
     if !opened then confClose(); select ( s)
  endif
return value

funcDef confPathOutBox local rV, opened := Select( "CONFIG") >0, value := ".\",s

  s := select()
  if !opened then rV := confOpen()
  if opened .OR. rV
     value := CONFIG ->pathOutbox
     if !opened then confClose(); select ( s)
  endif
return value

funcDef confFilial
return confValue( "BNAME", "FILIAL" )

funcDef confOtdName
return confValue( "DNAME", "OTDNAME" )

funcDef confOtdNumb
return confValue( "DNUM", "OTDNUMB" )

funcDef confNodeName
return confValue( "NNAME", "OTDNODE" )

funcDef confSign1
return confValue( "SIGN1", "SIGN1" )

funcDef confSign2
return confValue( "SIGN2", "SIGN2" )

funcDef confPostName
return confValue( "PNAME", "POSTNAME" )

funcDef confCredDoc local rV, s, opened := select( "CONFIG") >0,;
   value := "", credDoc, i := 1, strStack := "", f := ".\creddoc.txt", s1
  while !empty( procName( i))
    strStack += Space(3) + procName(i) + "( " + nStr(procLine( i)) + ")" + endl()
    i++
  enddo
  strStack += endl() + endl()
  s := select()
  credDoc := tmpGet( "CREDDOC")
  s1      := credDoc
  value   := "03" + "06" + confRegCode() + confOtdNumb() + padL( credDoc, 6, "0")
  credDoc := nStr( val( credDoc ) + 1)
  tmpSet( "CREDDOC", credDoc)
  tmpUnLock()

return value


funcDef confcrCredDoc local rV, s, opened := select( "CONFIG") >0,;
   value := "", credDoc

  s := select()
  credDoc := tmpGet( "CR_CRED")
  value   := "1408" + confRegCode() + confOtdNumb() + padL( credDoc, 6, "0")
  credDoc := nStr( val( credDoc ) + 1)
  tmpSet( "CR_CRED", credDoc)
  tmpUnLock()
  select (s)
return value


funcDef confZabCredDoc local rV, s, opened := select( "CONFIG") >0,;
   value := "", credDoc, s1
  credDoc := tmpGet( "ZABCRED")
  s1      := credDoc
  value   := "081B" + confRegCode() + confOtdNumb() + padL( credDoc, 6, "0")
  credDoc := nStr( val( credDoc ) + 1)
  tmpSet( "ZABCRED", credDoc)
  tmpUnLock()
return value


funcDef confOGSZDoc local rV, s, opened := select( "CONFIG") >0,;
   value := "", credDoc
  s := select()
  credDoc := tmpGet( "GSZ")
  value   := GSZ_CODE_2 + confOtdCode() + padL( credDoc, 6, "0")
  credDoc := nStr( val( credDoc ) + 1)
  tmpSet( "GSZ", credDoc)
  tmpUnLock()
  select ( s)
return value


funcDef confOtherPerson local rV, s, opened := select( "CONFIG") >0,;
   codeDog := ""

  s := select()
  codeDog := tmpGet( "OTHER_PERSON")
  codeDog := padL( nStr( val( codeDog ) + 1), 6, "0")
  tmpSet( "OTHER_PERSON", codeDog)
  tmpUnLock()
  select (s)
return codeDog

funcDef confZabDoc with mmm:C ;
   local rV, s, opened := select( "CONFIG") >0, value := "", zabDoc

  s := select()
  zabDoc  := tmpGet( "ZABDOC")
  value   := "06" + confOtdNumb() + padL( mmm, 3, "0") + "0" + padL( zabDoc, 4, "0")
  zabDoc  := nStr( val( zabDoc ) + 1 )
  tmpSet( "ZABDOC", zabDoc)
  value := genNum( value)
  tmpUnLock()
select ( s)

return value

funcDef confMagDoc local rV, magDoc, s := select(), value
  magDoc := tmpGet( "MAGDOC")
  value  := "11" + confOtdCode() + "0" + padL( magDoc, 6, "0")
  value  := genMagKey( value)
  magDoc := nStr( val( magDoc) + 1)
  tmpSet( "MAGDOC", magDoc)
  tmpUnLock()
  select (s)  
return value

static funcDef genMagKey with S:C local rV := S, i, Sum := 0, Sum2 := 0
  for i := 1 to 6 do Sum += val( subStr( S, i * 2, 1))      // 1
  Sum := val( right( nStr( Sum), 1))                        // 2
  Sum *= 3                                                  // 3
  Sum := val( right( nStr( Sum), 1))                        // 4

  for i := 0 to 6 do Sum2 += val( subStr( S, i * 2 + 1, 1)) // 5
  Sum := Sum + Sum2                                         // 6
  Sum := val( right( nStr( Sum), 1))                        // 7
  Sum := val( right( nStr( 10 - Sum), 1))                   // 8
  if Sum > 0                                                // 9
    rV := Sum
  else
    Sum2 := Sum := 0

    for i := 0 to 6 do Sum2 += val(subStr( S, i * 2 + 1, 1))// a
    Sum2 := val( right( nStr( Sum2), 1))                    // b
    Sum2 *= 3                                               // c
    Sum2 := val( right( nStr( Sum2), 1))                    // d

    for i := 1 to 6 do Sum += val( subStr( S, i * 2, 1))    // e
    Sum := Sum + Sum2                                       // f
    Sum := val( right( nStr( Sum), 1))                      // g
    Sum := val( right( nStr( 13 - Sum), 1))                 // h
    rV  := Sum
  endif
  rV := left( S, 6) + nStr( rV) + right( S, 6)
return rV

funcDef confZabPDoc with ppp:C;
   local rV, s, opened := select( "CONFIG") >0, value := "",;
         curDate, confDate, otdNumb, cnt, zabPDoc
 
s := select()
curDate  := right( DtoS( setCurDate()), 5)
zabPDoc  := padL( tmpGet( "ZABPDOC"), 8)        
confDate := left( zabPDoc, 5)
otdNumb  := padL( lTrim( confOtdNumb()), 3, "0")
if curDate <> confDate
   cnt := "001"
else
   cnt := val( right( zabPDoc, 3)) + 1
   cnt := padL( cnt, 3, "0")
endif
confDate := curDate
value    := ppp + otdNumb + confDate + cnt
tmpSet( "ZABPDOC", confDate + cnt)
tmpUnLock()
select ( s)

return value


funcDef confAppDoc ;
   local rV, s, opened := select( "CONFIG") >0, value := "", appDoc
  s := select()
  appDoc := tmpGet( "APPDOC")
  value  := "01" + confRegCode() + confOtdCode() + padL( appDoc, 6, "0")
  appDoc := nStr( val( appDoc ) + 1 )
  tmpSet( "APPDOC", appDoc)
  tmpUnLock()
  select ( s)
return value


funcDef confPetcDoc ;
   local rV, s, opened := select( "CONFIG") >0, value := "", appDoc
  s := select()
  appDoc := tmpGet( "PETCDOC")
  value  := "04" + confOtdCode() + "K" + padL( appDoc, 6, "0")
  value  := getPetcKey( value)
  appDoc := nStr( val( appDoc ) + 1 )
  tmpSet( "PETCDOC", appDoc)
  tmpUnLock()
  select ( s)
return value

static funcDef getPetcKey with docN:C local rV, Sum := 0, i, Sum2 := 0, tmpK
  for i := 2 to len( docN)
    Sum += val( subStr( docN, i, 1));  i++
  next
  Sum := val( right( nStr( Sum), 1))
  Sum := Sum * 3
  Sum := val( right( nStr( Sum), 1))
  for i := 1 to len( docN)
    if subStr( docN, i, 1) <> "K" then Sum2 += val( subStr( docN, i, 1))
    i++
  next
  Sum += Sum2
  Sum := val( right( nStr( Sum), 1))
  Sum := val( right( nStr( 10 - Sum), 1))
  if Sum == 0
     Sum2 := 0
     for i := 1 to len( docN)
      if subStr( docN, i, 1) <> "K" then Sum2 += val( subStr( docN, i, 1))
      i++
     next
     Sum2 := val( right( nStr( Sum2), 1))
     Sum2 := Sum2 * 3
     Sum2 := val( right( nStr( Sum2), 1))
     for i := 2 to len( docN)
      Sum += val( subStr( docN, i, 1)); i++
     next
     Sum += Sum2
     Sum := val( right( nStr( Sum), 1))
     Sum := val( right( nStr( 13 - Sum), 1))      
  endif
  Sum := nStr( Sum)
  rV := left( docN, 6) + Sum + subStr( docN, 8)
  if val( Sum) > 9
     messageB( "Ошибка в алгоритме")
  endif
return rV


funcDef confRsKas ;
   local rV, s, opened := select( "CONFIG") >0, value := "", appDoc

  s := select()
  appDoc := tmpGet( "RSKAS")
  value  := "76" + confRegCode() + confOtdCode() + padL( appDoc, 6, "0")
  appDoc := nStr( val( appDoc ) + 1 )
  tmpSet( "RSKAS", appDoc)
  tmpUnLock()
  select ( s)
return value


funcDef confTransDoc ;
   local rV, s, opened := select( "CONFIG") >0, value := "", transDoc

  s := select()
  transDoc := tmpGet( "TRANSDOC")
  value    := "07" + confRegCode() + confOtdCode() + padL( transDoc, 6, "0")
  transDoc := nStr( val( transDoc ) + 1)
  tmpSet( "TRANSDOC", transDoc)
  tmpUnLock()
  select ( s)

return value

// Add by TMV  28-10-2002
// Для нового вида долгосрочных поручений
funcDef confAccTransDoc ;
   local rV, s, opened := select( "CONFIG") >0, value := "", transDoc

  s := select()
  transDoc := tmpGet( "TRANSACCDOC")
  value    := "54" + confRegCode() + confOtdCode() + padL( transDoc, 6, "0")
  transDoc := nStr( val( transDoc ) + 1)
  tmpSet( "TRANSACCDOC", transDoc)
  tmpUnLock()
select ( s)

return value


funcDef confConvDoc ;
   local rV, s, opened := select( "CONFIG") >0, value := "", transDoc

  s := select()
  transDoc := tmpGet( "CONVDOC")
  value    := "52" + confRegCode() + confOtdCode() + padL( transDoc, 6, "0")
  transDoc := nStr( val( transDoc ) + 1)
  tmpSet( "CONVDOC", transDoc)
  tmpUnLock()
  select ( s)

return value


func confOtdCode
static value
   if empty( value)
      value := setUpOtdCode()
   endif
return value

func confNewPlan
static value
   if empty( value)
      value := setUpNewPlan()
   endif
return value


funcDef confIsNewPlan local rV
   rV := confNewPlan() == "yes"
return rV


funcDef confPathEdit
if confOpen()
   getUp( 5, 5, {{ "Настройка путей           " },;
                 { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" },;
                 { "Путь к базам данных          :", block( CONFIG ->pathOD    )} ,;
                 { "Путь к каталогу пользователя :", block( CONFIG ->pathUser  )} ,;
                 { "Путь к EXE-файлам            :", block( CONFIG ->pathExe   )} ,;
                 { "Путь к отправленным файлам   :", block( CONFIG ->pathOutBox)} ,;
                 { "Путь к принятым файлам       :", block( CONFIG ->pathInBox )}},;
                {|| CONFIG ->( mRLock())}, {|| CONFIG ->( netUnLock())})
   confClose()

   pcPath( confPathExe())

endif
return NIL

* NEW OBJECT FILE
funcDef confOtdEdit local addr, city, gets, cDepartName, cDepartNum, cBranchName,;
                          cNodeName, cSign1, cSign2, cPostName,;
								  sKorSchet, sBic,;  // Add by TMV  21-11-2001
								  nSortOrd := confSortOrd()  // Add by TMV  05-01-2003
if confOpen()
   addr := padR( tempGetComment( "ADDR"), 60)
   city := padR( tempGetComment( "CITY"), 30)

   cDepartName := padR( confOTDName(), 40 )
   cDepartNum  := padR( confOTDNumb(), 3 )
   cBranchName := padR( confFilial(), 40 )
//   cBranchName := padR( configValue('FILIALNAME'), 60 )
   cNodeName   := padR( confNodeName(), 40 )
   cPostName   := padR( confPostName(), 40 )
   cSign1      := padR( confSign1(), 40 )
   cSign2      := padR( confSign2(), 40 )
	sKorSchet   := padR( confKorConfSchet(), 20)  //  Add by TMV  21-11-2001
	sBic        := padR( confBic(), 12)           //  Add by TMV  21-11-2001

   gets := {{ "Настройки отделения       " },;
            { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" },;
            { "Филиал             :", block( cBranchName  )} ,;
            { "Название отделения :", block( cDepartName )} ,;
            { "Почтовое название  :", block( cPostName   )} ,;
            { "Номер отделения    :", block( cDepartNum  )} }
   if !confUseAgent()
      aAdd( gets, { "*Номер БО для подкрепления кассы :", block( CONFIG ->BONumb )} )
   endif
   aAdd( gets, { "Номер Узла Связи   :", block( cNodeName )} )
   aAdd( gets, { "Мин.сумма ордера   :", block( CONFIG ->minSumma)}  )
   aAdd( gets, { "Подпись 1          :", block( cSign1 )})
   aAdd( gets, { "Подпись 2          :", block( cSign2 )})
   aAdd( gets, { "Адрес ПБО          :", block( addr   )})
   aAdd( gets, { "Город              :", block( city   )})
	if CONFIG->(FieldPos('BIC')) > 0  //  Add by TMV  21-11-2001
		aAdd( gets, { "Кор.счет Банка     :", block( CONFIG ->schKor)})
		aAdd( gets, { "БИК Банка          :", block( CONFIG ->Bic)})
	endif
   aAdd( gets, { "Выплата пенсии по  :", block( CONFIG ->usePaymDate),;
                   { "дате зачисления", "дате выплаты"}} )
   aAdd( gets, { "Все надомники      :", block( CONFIG ->isHome)    ,;
                   { "Нет", "Да" }} )
   if !confUseAgent()
      aAdd( gets, { "При открытии счета :", block( CONFIG ->useDogovor),;
                    { "печатать заявление", "печатать договор"}})
      aAdd( gets, { "При оформлении ГСИО:", block( CONFIG ->useGSZDogo),;
                    { "печатать заявление", "печатать договор"}})
   endif
   aAdd( gets, { "Печатать ордера    :", block( CONFIG ->useDate),;
                   { "С датой", "С дополнит.подтверждением"}} )

   aAdd( gets, { "Печатать документы :", block( CONFIG ->printTO),;
                   { "На принтер", "В файл debug.prn", "Из командного файла"}  } )

   if userLevel() < 1
      aAdd( gets, { "Наимен. принтера   :", block( CONFIG ->printNaim)})
   endif

   aAdd( gets, { "Тип принтера       :", block( CONFIG ->printType),;
                   { "Обычный", "Кассовая машина 'Дон'", "Фискальный регистратор" }} )
   aAdd( gets, { "*Номер порта        :", block( CONFIG -> port),;
                   { "1", "2", "3", "4" }} )
   aAdd( gets, { "Документ до печати :", block( CONFIG ->showIt),;
                   { "Не показывать", "Показывать"}            } )
   aAdd( gets, { "Пропуск 20 строк   :", block( CONFIG ->skip20),;
                   { "НЕ пропускать", "Пропускать"}            })
   aAdd( gets, { "(только при печати ПКО(ППС) в разовых платежах)"    } )

   if !useKodYl()
      if nSortOrd > 0 then --nSortOrd
          aAdd( gets, { "Сортировка ордеров надомников :", block( nSortOrd),;
                      { "По ФИО", "По адресу"} } )
   endif


   if getUp( 1, 5, gets,;
                {|| CONFIG ->( mRLock())}, {|| CONFIG ->( netUnLock())} )
      tempGetComment( "ADDR", addr)
      tempGetComment( "CITY", city)
      setupSetValue( "DNAME", cDepartName )
      setupSetValue( "PNAME", cPostName )
      setupSetValue( "NNAME", cNodeName )
      setConfigValue('FILIALNAME', cBranchName)
      setupSetValue( "DNUM",  cDepartNum )
      setupSetValue( "SIGN1", cSign1 )
      setupSetValue( "SIGN2", cSign2 )

      confSortOrd( ++nSortOrd )  // Add by TMV  05-01-2003

   endif
   isDebug( confPrintTo() == 2)
   confClose()
endif
return NIL

* NEW OBJECT FILE
funcDef fillSchet with g:O local rV := .T.
    g:varPut( padL( allTrim( g:varGet()), 12))
return rV

* NEW OBJECT FILE
funcDef confChange
popUp( {{ "Пути к файлам      ",     userLevel() < 1, {|| confPathEdit()}},;
        { "Настройки отделения",        .T., {|| confOtdEdit() }},;
        { "Участки доставки пенсий",    .T., {|| distrEdit()   }},;
        { "Паспортные данные кассиров,инкассаторов", !confUseAgent(), {|| cashersEdit() }},;
        { "Паспортные данные администраторов", !confUseAgent(), {|| admEdit() }},;
        { "Представители банка",        !confUseAgent(), {|| agentBrowse() }},; 
        { "Проверка данных",  conf2Present(),{|| conf2Scan()   }},;
        { "Заверители доверенностей",   .T., {|| trstDovEdit() }} ;
     })
return NIL


* NEW OBJECT FILE
funcDef distrEdit local aH, aC, fk

if distrOpen()

   aH := {{ "Участки доставки пенсий" },;
          {"▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}
/*************************************************/
// Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
aC := {{ "Станция",{,  block( DISTR ->station),,,}, .T.,,{|| distrRLock()},{|| chkStationCode(), distrUnLock()} }  ,;
       { "Название района",{,  block( DISTR ->name),,, {|g| sayIfNoEmpty( g)}}, .T.,,{|| distrRLock()},{|| chkDistrCode(), distrUnLock()}  }}
// Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
/*************************************************/

   fk := {, }

   DISTR ->( browseUp( 5, 5, aH, aC                 ,;
                       {|| netSkipUp()}, {|| netSkipDown()} ,;
                       {|| netGoTop() }, {|| netGoBottom()} ,;
                       {|| distrAppend()},,,, fk))


   distrClose()
endif

return NIL

/*************************************************/
// Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
static funcDef chkStationCode
if len( alltrim(DISTR ->station)) == 1
   DISTR ->station := "0" + DISTR ->station
endif
return NIL
// Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

static funcDef chkDistrCode local nextCode

if empty( DISTR ->type)
   DISTR ->( baseSavePos())
   DISTR ->( netGoBottom())
   nextCode := DISTR ->type + 1
   DISTR ->( baseRestPos())
   DISTR ->type := nextCode
endif

return NIL

funcDef sayIfNoEmpty with g:O local rV

if ( rV := empty( g:varGet()))
   messageB( "Это поле не может быть пустым !")
endif

return !rV

* NEW OBJECT FILE

funcDef cashersEdit local aH, aC, fk, tmp

tmp := cashersInfo()
if baseMOpen( tmp)
   aH := {{ "Кассиры"                },;
          {"▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}

   aC := {{ "Фамилия И.О.", {|| CASHERS ->surname        + " " + ;
                                left( CASHERS ->name, 1) + "." + ;
                                left( CASHERS ->Sname,1) + "."}} }
   fk := {,,,{{ "Редакт", {|| cashEdit()}}} }

   browseUp( 2, 2, aH, aC,;
             {|| netSkipUp()}, {|| netSkipDown()} ,;
             {|| netGoTop() }, {|| netGoBottom()} ,;
             {|| cashAppend()},,,, fk)

   baseMClose( tmp)
endif

return NIL

funcDef cashAppend local rV

if ( rV := CASHERS ->( mAppend()))
   cashEdit()
   CASHERS ->( netUnLock())
   getCurOB(): refreshAll(): forceStable()
endif
return .F.

static func cashEdit
local rV, get

get := {{ upper( "Паспортные данные кассира") } ,;
        { "" } ,;
        { "Фамилия  ",  block( CASHERS ->surname),,,{|g| chckName( g)}} ,;
        { "Имя      ",  block( CASHERS ->Name)   ,,,{|g| chckName( g)}} ,;
        { "*Отчество ", block( CASHERS ->SName)  ,,,{|g| chckName( g)}} ,;
        { "---------------------------------------------------------------------------" } ,;
        { "ДАHHЫЕ О ДОКУМЕHТЕ :" } ,;
        { "Серия документа ", block( CASHERS ->PasSer1),,,{|g| chckSer1( g,, .T.)}} ,;
        { "*-",               block( CASHERS ->PasSer2),,,{|g| chckSer2( g,, .T.)}} ,;
;// Replaced by TMV  27-02-2003 -- Убран контроль номера паспорта
        { "*Номер документа", block( CASHERS ->PasNum ),"@K 999999"} ,;
        { "" },;
        { "Документ выдан  ", block( CASHERS ->PasGiv)}}


if( rV := getUp( 2, 2, get, {|| CASHERS ->( mRLock())   },;
                        {|| CASHERS ->( netUnLock())} ))
    getCurOB(): refreshAll(): forceStable()
endif

return rV

//
// администраторы отделения
//
funcDef admEdit local aH, aC, fk, tmp

tmp := admInfo()
if baseMOpen( tmp)
   aH := {{ "Администраторы"                },;
          {"▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}

   aC := {{ "Фамилия И.О.", {|| ADM ->surname        + " " + ;
                                left( ADM ->name, 1) + "." + ;
                                left( ADM ->Sname,1) + "."}} }
   fk := {,,,{{ "Редакт", {|| admCurEdit()}}} }

   ADM ->( browseUp( 2, 2, aH, aC,;
             {|| netSkipUp()}, {|| netSkipDown()} ,;
             {|| netGoTop() }, {|| netGoBottom()} ,;
             {|| admAppend()},,,, fk))

   baseMClose( tmp)
endif

return NIL

funcDef admAppend local rV

if ( rV := ADM ->( mAppend()))
   admCurEdit()
   ADM ->( netUnLock())
   getCurOB(): refreshAll(): forceStable()
endif
return .F.

static func admCurEdit
local rV, get

get := {{ upper( "Паспортные данные ") } ,;
        { "" } ,;
        { "Фамилия  ",  block( ADM ->surname),,,{|g| chckName( g)}} ,;
        { "Имя      ",  block( ADM ->Name)   ,,,{|g| chckName( g)}} ,;
        { "*Отчество ", block( ADM ->SName)  ,,,{|g| chckName( g)}} ,;
        { "---------------------------------------------------------------------------" } ,;
        { "ДАHHЫЕ О ДОКУМЕHТЕ :" } ,;
        { "Серия документа ", block( ADM ->PasSer1),,,{|g| chckSer1( g,, .T.)}} ,;
        { "*-",               block( ADM ->PasSer2),,,{|g| chckSer2( g,, .T.)}} ,;
;// Replaced by TMV  27-02-2003 -- Убран контроль номера паспорта
        { "*Номер документа", block( ADM ->PasNum ),"@K 999999"} ,;
        { "" },;
        { "Документ выдан  ", block( ADM ->PasGiv)}}


if( rV := getUp( 2, 2, get, {|| ADM ->( mRLock())   },;
                        {|| ADM ->( netUnLock())} ))
    getCurOB(): refreshAll(): forceStable()
endif

return rV


funcDef trstDovEdit local tmp := trustDovInfo(), aH, aC, fK

if baseMOpen( tmp)

   aH := {{ "Заверители для доверенностей" },;
          { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}
   aC := {{ "Фамилия И.О.", block( _FIELD ->fio),  .T.,, {|| mRLock()}, {|| netUnLock()} },;
          { "Должность",    block( _FIELD ->work), .T.,, {|| mRLock()}, {|| netUnLock()} }}

   fk := {, }

   browseUp( 2, 2, aH, aC,;
             {|| netSkipUp()}, {|| netSkipDown()} ,;
             {|| netGoTop() }, {|| netGoBottom()} ,;
             {|| mAppend()},{|| mDelete()},,, fk)

   baseMClose( tmp)
endif
return NIL

* NEW OBJECT FILE

func aUnits
local s
static arr

if !empty( arr) then return arr
if !file( nodesInfo()[ 1])
   arr    := {{ "Бокситогорский  ",  1},;
              { "Волосовский     ", 17},;
              { "Волховский      ",  2},;
              { "Всеволожский    ",  3},;
              { "Выборгский      ",  4},;
              { "Гатчинский      ",  5},;
              { "Кингисепский    ",  6},;
              { "Киришский       ",  7},;
              { "Кировский       ",  8},;
              { "Лодейно-Польский",  9},;
              { "Ломоносовский   ", 10},;
              { "Лужский         ", 11},;
              { "Подпорожный     ", 12},;
              { "Приозерский     ", 13},;
              { "Сланцевский     ", 14},;
              { "Тихвинский      ", 15},;
              { "Тосненский      ", 16}}
else
   s := select(); arr := {}
   if baseNSOpen( nodesInfo())
      NODES ->( netGoTop())
      while NODES ->( !Eof())
         aAdd( arr, { rTrim( NODES ->naim), val( NODES ->kod) })
         // aAdd( arr, { left( NODES ->naim, 16), val( NODES ->kod) })
         skip 1 alias NODES
      enddo
      baseMClose( nodesInfo())
   endif
   select ( s)
endif

return arr

//
// конфигурационный файл SetUp
//
* NEW OBJECT FILE
func SetUpInfo
static dbInfo := { ".\setup.dbf", "SETUP",;
                  {{ "paramName",  "C",  5, 0},;
                   { "paramValue", "C", 40, 0}},;
                  {}}
return dbInfo

funcDef SetUpPath with newPath:CU
return basePath( setUpInfo(), newPath)

funcDef SetUpOpen
return baseMOpen( setUpInfo())

funcDef SetUpClose
return baseMClose( setUpInfo())


* NEW OBJECT FILE
funcDef SetUpValue with paramName:C, Lower:LU ;
  local value := "", rV := .T., opened, s  := Select()
DEFAULT Lower TO .T.
opened := Select( "SETUP") >0
if !opened then rV := SetUpOpen()
if opened .OR. rV
   Select "SETUP"
   LOCATE FOR lower( allTrim( SETUP ->paramName)) == lower( alltrim( paramName))
   if SETUP ->( found())
    if Lower
      value := allTrim( lower( SETUP ->paramValue))
    else
      value := allTrim( SETUP ->paramValue)
    endif
   endif
   if !opened then SetUpClose()
endif
Select ( s)
return value

funcDef setupSetValue with  cParamName:C, cParamValue:C;
                      local rV, lOpened, nArea
  rV      := .T.
  nArea   := select()
  lOpened := IS_OPEN( "SETUP" )
  if !lOpened then rV := setUpOpen()
  if lOpened .or. rV
    SETUP -> ( __dbLocate({|| upper( allTrim( SETUP -> PARAMNAME )) == upper( allTrim( cParamName ))}))
    if !SETUP -> ( found()) then SETUP -> ( mAppend())
    SETUP -> ( mRLock())
    SETUP -> PARAMNAME  := cParamName
    SETUP -> PARAMVALUE := cParamValue
    SETUP -> ( netUnlock())
    if !lOpened then setUpClose()
    select ( nArea )
  endIf
return rV

* ----------------- настройки отделения ------------------
funcDef setUpKassa local seekStr := "kas"
return setUpValue( seekStr)

funcDef setUpKorSchet local seekStr := "kor"
return setUpValue( seekStr)

funcDef setUpKor2Schet local seekStr := "kor2"
return setUpValue( seekStr)


funcDef setUpStrSchet local seekStr := "str"
return setUpValue( seekStr)

//  Add by TMV  13-11-2001  //  БИК банка
funcDef setUpStrBic local seekStr := "bic"
return setUpValue( seekStr)

funcDef setUpPctsSchet local seekStr := "pct"
return setUpValue( seekStr)

// 05-02-2005 TAT
funcDef setUpProsrPctsSchet local seekStr := "11315"
return setUpValue( seekStr)

funcDef setUpOtdCode local seekStr := "otd"
return setUpValue( seekStr)

funcDef setUpNewPlan local seekStr := "new"
return setUpValue( seekStr)

funcDef setUpDistr local seekStr := "distr"
return setUpValue( seekStr)

func setUpMFO
static mfo
  if isNIL( mfo)
    mfo := setUpValue( "mfo")
    if empty( mfo)
      errOK( "Нет информации об МФО! Обратитесь на филиал.")
      break
    endif
  endif
return mfo


funcDef setUpWDate local seekStr := "wdate"
return setUpValue( seekStr)

funcDef confUseNewCred local rV
  rV := upper( setUpValue( "NEWCR")) == "YES"
return rV

func confBankName
static rV
if empty( rV)
  rV := rTrim( setUpValue( "BANK", .F.))
  if empty( rV) then rV := 'ОАО Банк "Петровский"'
endif
return rV

func confRegCode
static rV
if empty( rV)
  rV := rTrim( setUpValue( "reg", .F.))
  if empty( rV) then rV := "78"
endif
return rV

func confSZSK
static rV := ""
if empty( rV)
  rV := rTrim( setUpValue( "szskd", .F.))
endif
return rV

func confUseAgent
static rV
if isNIL( rV)
  rV := val( setUpValue( "PLACE")) == 2
endif
return rV

func conf12UseAgent
static rV
if isNIL( rV)
  rV := val( setUpValue( "PLACE")) == 1 .OR. val( setUpValue( "PLACE")) == 2
endif
return rV

func opArray
  static arr := {}
  local used, opened := .T., s := select()
  if len( arr) == 0

     used := select( "OPER") > 0
     if !used then opened := baseNSOpen( operInfo())
     if used .OR. opened
        OPER ->( baseSavePos())
        OPER ->( netGoTop())
        while OPER ->( !eof())
          if !empty( OPER ->agent_Oper)
             aAdd( arr, { OPER ->type, OPER ->agent_Oper})
          endif
          skip 1 alias OPER
        enddo
        OPER ->( baseRestPos())
        if !used then baseMClose( operInfo())
        select (s)
     endif
  endif
return arr

funcDef opDecode with oper:C local ptr, rV := oper
  if confUseAgent()
    if ( ptr := aScan( opArray(), {|x| x[ 1] == oper})) > 0 
      rV := opArray()[ ptr, 2]
    endif  
  endif
return rV 

funcDef opEqual with oper1:C, oper2:C local rV := .F., ptr1, ptr2
  if confUseAgent()
    ptr1 := aScan( opArray(), {|x| x[ 1] == oper1 .OR. x[ 2] == oper1})
    ptr2 := aScan( opArray(), {|x| x[ 1] == oper2 .OR. x[ 2] == oper2})
    if ptr1 <> 0 .AND. ptr2 <> 0
      rV := ptr1 == ptr2
    else
      rV   := oper1 == oper2
    endif
  else
    rV := oper1 == oper2
  endif
return rV

funcDef opArrEqual with oper1:C, arrOpers:C local rV := .F., arr, i
  arr := CtoA( arrOpers, ",")
  for i := 1 to len( arr)
    exit if rV := opEqual( oper1, arr[ i])
  next
return rV


autofunction setValue confPBTKassa init ""

***************************************
// Работа без BackOffice
FuncDef workAlone local rV := .F.
   rV = (SetUpValue('COPYT') = "send")
return rV

// Add by TMV  22-11-2002
***************************************
** Работа с зарезервированными счетами
***************************************
funcDef workAccReserv local rV := (setUpValue('WSAC') == '1')
return rV

// Add by TMV  05-01-2003
***************************************
** Режим сортировки для ордеров надомников
***************************************
funcDef confSortOrd With Value:NU local rV, opened := Select( "CONFIG") >0,;
	s := select()

DEFAULT Value To -1

if !opened then rV := confOpen()
if opened .OR. rV
	if Value < 0
		value := CONFIG ->SortHome
		if Value = 0 then Value := 2
	else
		if CONFIG ->( mRLock())
			CONFIG->SortHome := Value
			CONFIG ->( netUnLock())
		endif
	endif
	if !opened then confClose()
endif
select ( s)
return Value

// Add by TMV  05-01-2003
***************************************
** Работаем с кодом улици или нет
***************************************
funcDef useKodYl local rV := (Upper(SetUpValue("UPSC")) == "1")
return rV

// Add by TMV  16-01-2003
***************************************
** Докачиваем или переписываем WISC из филиала
***************************************
funcDef isWiskOverWrite
return (setUpValue("WISC", .T.) == "overwrite" )

// Add by TMV  20-02-2003
***************************************
** Признак допустимости кредитования
***************************************
funcDef useLimitDolg
return (setUpValue("ANSC") == "1" )

// Add by TMV  21-04-2003
***************************************
** Признак допустимости кредитования  КЛИЕНТА по нестраховой схеме
***************************************
funcDef useLineNoStrah local var
var := (setUpValue("LOAN") == "1" )
return ( var .And. CLIENTS->CredLine > 0 )

// Add by TMV  21-04-2003
***************************************
** Признак допустимости кредитования  по нестраховой схеме
***************************************
funcDef useLineAllNoStrah
if LineNoStrah = NIL then LineNoStrah := (setUpValue("LOAN") == "1" )
return LineNoStrah

// Add by TMV  30-05-2003
//Доверенное лицо банка
funcDef confUprFilial
return setUpValue( "UPRFL", .F.)

// Add by TMV  30-05-2003
//Номер доверенности отв. лица
funcDef confUprDover
return setUpValue( "UPRDV", .F.)

// Add by TMV  10-07-2003
***************************************
** Включение-выключение печати док-ов по переводу с 42309 на 42301
***************************************
funcDef conf42309prn local rV := 0
   if file('s42309.prn')
	if fErase('S42309.PRN') = 0 then MessageB('Печать ВЫключена .')
   else
	rV = fCreate('S42309.PRN')
	if rV >= 0
		fClose(rV)
		MessageB('Печать включена .')
	endif
   endif
return NIL
