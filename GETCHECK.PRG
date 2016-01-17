/***************************************************************************

      Модуль    : GETCHECK ( Функции проверки ввода )
      Файл      : GETCHECK.PRG
      Автор     : Залманов А.З.

***************************************************************************/

#include "common.ch"
#include "clippext.ch"

#include "fileio.ch"
#include "directry.ch"
#include "inkey.ch"

autofunction setValue rSymbols init "АБВГДЕЖЗИЙКЛМНОПРСТЧФХЭЮЯЪЬШЩЦУЫ"
autofunction setValue lSymbols init "ABCDEFGHIJKLMNOPQRSTUVWXYZ'"
autofunction setValue dSymbols init "0123456789"  
autofunction setValue sSymbols init ';".-():/\+N'

autofunction setValue serSymb1   init "IVXMLC"
autofunction setValue serSymb2   init "АБВГДЕЖЗИЙКЛМНОПРСТЧФХЭЮЯЪЬШЩЦУЫ"
autofunction setValue numSymbols init "0123456789"

autofunction setValue docNames init;
          { "Паспорт гражданина Российской Федерации          ",;
            "Паспорт гражданина СССР                          ",;
            "Свидетельство о рождении                         ",;
            "Удостоверение личности офицера,мичмана,прапорщика",;
            "Справка об освобождении из мест лишения свободы  ",;
            "Временное удостоверение                          ",;
            "Заграничный паспорт                              ",;
            "Дипломатический паспорт                          ",;
            "Военный билет рядового,сержанта,матроса          ",;
            "Иной                                             "}

static funcDef checkKb local nKey
  nKey := lastKey()
return nKey == K_UP .or. nKey == K_SH_TAB 

funcDef notEmpty with value: ACNDU local rV
  if checkKb() then return .T.
  rV := empty( value )
  if rV then messageB( "Поле должно быть заполнено!" )
return !rV

funcDef notEmptyOrZero with value: ACNDU local rV
  if checkKb() then return .T.
  rV := empty( value )
  if rV .and. isNUMBER( value ) 
    rV := !noYes( "Вы уверены, что значение должно быть нулевым?" )
  endIf
  if rV then messageB( "Поле должно быть заполнено!" )
return !rV
  
funcDef upCase with oG:O local rV, cValue
  if checkKb() then return .T.
  cValue := oG: varGet()
  rV     := empty( cValue )
  if rV
    messageB( "Поле должно быть заполнено!" )
  else
    oG: varPut( upper( cValue ))
  endIf
return !rV

funcDef upCaseEmpty with oG:O
  oG: varPut( upper( oG: varGet()))
return .T.

funcDef checkPStr with oG:O, cSymbolSet:C, nDocCode:NU, cAltSymbolSet:CU;
                  local cValue, i, cSymbol, rV, lPassport
  default nDocCode to 0
  lPassport := !empty( cAltSymbolSet )
  if checkKb() then return .T.
  rV := .T.
  if empty( oG: varGet()) .and.;
     (!lPassport .or. ( nDocCode == 1 .or. nDocCode == 2 ))
    messageB( "Поле должно быть заполнено!" )
    return .F.
  endIf
  if lPassport
    if nDocCode == 1
      rV := checkStr( oG, cAltSymbolSet ) 
    elseif nDocCode == 2
      rV := checkStr( oG, cSymbolSet )
    else
      rV := .T.
    endIf
  else
    rV := checkStr( oG, cSymbolSet ) 
  endIf
return rV 

funcDef checkStr with  oG:O, cSymbolSet:C, toUpper:LU, mayBeEmpty:LU;
                 local cValue, i, cSymbol, rV
default toUpper    to .T.
default mayBeEmpty to .T.
  if checkKb() then return .T.
  rV     := .T.
  cValue := upper( oG: varGet())
  if empty( cValue )
    if !mayBeEmpty
      messageB( "Поле должно быть заполнено!" )
      return .F.
    else
      return .T.
    endIf
  endIf
  if toUpper then oG: varPut( cValue )
  for i := 1 to len( cValue )
    cSymbol := subStr( cValue, i, 1 ) 
    if cSymbol != " " .and. !( cSymbol $ cSymbolSet )
      messageB( "Недопустимый символ " + cSymbol + " в поле ввода!" )
      rV := .F.
      exit
    endIf
  next
return rV

funcDef translitStr with cStr:C local i, j, aRus, aLat, rV, cSymbol
  rV   := ""
  aRus := { "А", "Б", "В", "Г", "Д", "Е", "Ж", "З", "И", "Й", "К", "Л", "М", "Н",;
            "О", "П", "Р", "С", "Т", "Ф", "Х", "Э", "Ц", "Ъ", "Ь", "Ш", "Щ", "Ч",;
            "Ю", "Я", "У", "Ы" }
  aLat := { "A", "B", "V", "G", "D", "E", "J", "Z", "I", "I", "K", "L", "M", "N",;
            "O", "P", "R", "S", "T", "F", "H", "E", "C", "'", "'", "SH", "SH", "CH",;
            "JU", "JA", "OU", "YI" }
  for i := 1 to len( cStr )
    cSymbol := subStr( cStr, i, 1)
    j := aSearch( aRus,  cSymbol )
    if j > 0 then cSymbol := aLat[ j ]
    rV += cSymbol
  next
return rV

funcDef translit with oG:O local rV
  if !upCase( oG ) then return .F.
  oG: varPut( translitStr( oG: varGet()))
return .T.

funcDef translitEmpty with oG:O local rV
  upCaseEmpty( oG )
  oG: varPut( translitStr( oG: varGet()))
return .T.

funcDef translitGet with oG:O, i:N
  public getList
  oG: varPut( translitStr( getList[ i ]: varGet()))
return .T.

funcDef translitToGet with oG:O, i:N
  public getList
  getList[ i ]: varPut( translitStr( upper( oG: varGet())))
  getList[ i ]: display()
return .T.

funcDef translitVar with oG:O, cVar:C, i:N
  public getList
  cVar := translitStr( upper( oG: varGet()))
  getList[ i ]: varPut( cVar )
  getList[ i ]: display()
return .T.

funcDef setDocCode with oG:O, nDocCode:N
  nDocCode := aSearchTrim( docNames(), allTrim( oG: varGet()))
return .T.

funcDef checkLen with cS:C, nMax:N
  if len( cS ) > nMax
    messageB( "Превышена максимально допустимая длина значения - " + nStr( nMax) + " символов!" )
    return .F.
  endIf
return .T.
