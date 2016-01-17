*------------------------------------------------------------------------------
*  
*                         Программа  : MON.EXE
*                         Файл       : cardPaym.prg
*                         Автор      : Цейтлин П.М.
*                         Назначение : Прием платежей по картам
*
*------------------------------------------------------------------------------
#include "clippExt.ch"
#include "common.ch"
#include "getExit.ch"
#include "inkey.ch"
#include "gsz.ch"

//
// генерация платежа
//
funcDef cardMakePaym ;
  local gets, provName, provCode, accSchet, accSchet2, ptrSchet,;
        ptr1, ptr2, ptr3, crName1, crPrice1, crCount1, str      ,;
        crName2, crPrice2, crCount2, crName3, crPrice3, crCount3,;
        asCode1, asCode2, asCode3, alg1, algSum1, alg2, algSum2 ,;
        alg3, algSum3, fName, aPaym, S, preName1, preName2      ,;
        preName3, sumFine, sumPlat, totalSum := 0.00, sumFine1  ,;
        sumFine2, sumFine3, ptr4, ptr5,                          ;
        crPrice4, crPrice5, crName4, crName5, crCount4, crCount5,;
        asCode4, asCode5, alg4, alg5, algSum4, algSum5, preName4,;
        preName5, sumFine4, sumFine5


aFillProvs()
fillProvList( "51", @provName, @provCode)
accPrizList( "15 1", @accSchet,   @accSchet2)
aAdd( accSchet,  NIL); aIns( accSchet, 1); accSchet[ 1] := "Наличными"
aAdd( accSchet2, NIL); aIns( accSchet2,1); accSchet2[1] := { 0, "NAL"}
ptrSchet := 1
if len( provName) < 1
  messageB( "По данной услуге нет поставщиков !!!")
  return .F.
endif
provName := aClone( provName); aAdd( provName, "")
aIns( provName, 1); provName[ 1] := ""
provCode := aClone( provCode); aAdd( provCode, "")
aIns( provCode, 1); provCode[ 1] := ""
 
ptr1 := ptr2 := ptr3 := ptr4 := ptr5 := 1
crName1 := space( 20); crPrice1 := 0.00; crCount1 := 0; asCode1 := ""; sumFine1 := 0
crName2 := space( 20); crPrice2 := 0.00; crCount2 := 0; asCode2 := ""; sumFine2 := 0
crName3 := space( 20); crPrice3 := 0.00; crCount3 := 0; asCode3 := ""; sumFine3 := 0
crName4 := space( 20); crPrice4 := 0.00; crCount4 := 0; asCode4 := ""; sumFine4 := 0
crName5 := space( 20); crPrice5 := 0.00; crCount5 := 0; asCode5 := ""; sumFine5 := 0
alg1 := ""; algSum1 := 0
alg2 := ""; algSum2 := 0
alg3 := ""; algSum3 := 0
alg4 := ""; algSum4 := 0
alg5 := ""; algSum5 := 0
fName   := padR( cliFullName(), 60)
preName1 := crName1; preName2 := crName2; preName3 := crName3
preName4 := crName4; preName5 := crName5

gets := {{ "Прием платежей( Сервисные карты )"         },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"  },;
         { "Клиент", block( fName),,,{|g| !empty( g:varGet)}  },;
         { "Счет  ", block( ptrSchet), accSchet      },;
         { "*" + chr( 25)},;
         { ""                                        },;
         { "1. Платеж ", block( ptr1), provName,{|g| preName1 := g:varGet, .T.},;
            {|g| clearGets( g, preName1, 3, {{ 7, sumFine2}, { 11, sumFine3},{ 15, sumFine4}, { 19, sumFine5}})}},;
         { "*" + chr( 25)},;
         { "    карта ", block( crName1),,,,;
            {|g| cardReader( g, 3, @asCode1, @alg1, @algSum1, provName, provCode, @sumFine1)}   },;
         { "*" + chr( 25)},;
         { "* цена ",       block( crPrice1), "99999.99",{||.F.}},;
         { "* кол-во ",     block( crCount1), "999",,;
            {|g| checkCount( g, 3, asCode1, @alg1, @algSum1,;
                   {{ 7, sumFine2}, { 11, sumFine3},;
                    { 15, sumFine4}, { 19, sumFine5}}, @sumFine1,;
                   {{ asCode2, 10}, { asCode3, 14},;
                    { asCode4, 18}, { asCode5, 22}};
  )} },;
         { " " },;
         { "2. Платеж ", block( ptr2), provName,{|g| preName2 := g:varGet, .T.},;
            {|g| clearGets( g, preName2, 7, {{ 3, sumFine1}, { 11, sumFine3},{ 15, sumFine4}, { 19, sumFine5}})}},;
         { "*" + chr( 25)},;
         { "    карта ", block( crName2),,,,;
            {|g| cardReader( g, 7, @asCode2, @alg2, @algSum2, provName, provCode, @sumFine2)}   },;
         { "*" + chr( 25)},;
         { "* цена ",       block( crPrice2), "99999.99",{||.F.}},;
         { "* кол-во ",     block( crCount2), "999",,;
            {|g| checkCount( g, 7, asCode2, alg2, algSum2,;
                   {{ 3, sumFine1}, { 11, sumFine3},;
                    { 15, sumFine4}, { 19, sumFine5}}, @sumFine2,;
                   {{ asCode1, 6}, { asCode3, 14},;
                    { asCode4,18}, { asCode5, 22}};
  )} },;
         { " " },;
         { "3. Платеж ", block( ptr3), provName,{|g| preName3 := g:varGet, .T.},;
             {|g| clearGets( g, preName3, 11, {{ 3, sumFine1}, { 7, sumFine2},{ 15, sumFine4}, { 19, sumFine5} })}},;
         { "*" + chr( 25)},;
         { "    карта ", block( crName3),,,,;
            {|g| cardReader( g, 11, @asCode3, @alg3, @algSum3, provName, provCode, @sumFine3)}   },;
         { "*" + chr( 25)},;
         { "* цена ",       block( crPrice3), "99999.99",{||.F.}},;
         { "* кол-во ",     block( crCount3), "999",,;
             {|g| checkCount( g, 11, asCode3, alg3, algSum3,;
                    {{ 3, sumFine1}, { 7, sumFine2},;
                     {15, sumFine4}, {19, sumFine5}}, @sumFine3,;
                   {{ asCode1, 6}, { asCode2, 10},;
                    { asCode4,18}, { asCode5, 22}};
   )} },;
         { " " } }

aAdd( gets,         { "4. Платеж ", block( ptr4), provName,{|g| preName4 := g:varGet, .T.},;
            {|g| clearGets( g, preName4, 15, {{ 3, sumFine1}, { 11, sumFine3},{ 7, sumFine2}, { 19, sumFine5}})}} )
aAdd( gets,         { "*" + chr( 25)} )
aAdd( gets,         { "    карта ", block( crName4),,,,;
            {|g| cardReader( g, 15, @asCode4, @alg4, @algSum4, provName, provCode, @sumFine4)}   } )
aAdd( gets,         { "*" + chr( 25)} )
aAdd( gets,         { "* цена ",       block( crPrice4), "99999.99",{||.F.}} )
aAdd( gets,         { "* кол-во ",     block( crCount4), "999",,;
            {|g| checkCount( g, 15, asCode4, alg4, algSum4,;
                    {{ 3, sumFine1}, { 11, sumFine3},;
                     { 7, sumFine2}, { 19, sumFine5}}, @sumFine4,;
                   {{ asCode1, 6}, { asCode3, 14},;
                    { asCode2,10}, { asCode5, 22}};
  )} } )
aAdd( gets,         { " " } )

aAdd( gets,         { "5. Платеж ", block( ptr5), provName,{|g| preName5 := g:varGet, .T.},;
             {|g| clearGets( g, preName5, 19, {{ 3, sumFine1}, { 7, sumFine2},{ 11, sumFine3}, { 15, sumFine4}})}} )
aAdd( gets,         { "*" + chr( 25)} )
aAdd( gets,         { "    карта ", block( crName5),,,,;
            {|g| cardReader( g, 19, @asCode5, @alg5, @algSum5, provName, provCode, @sumFine5)}   } )
aAdd( gets,         { "*" + chr( 25)} )
aAdd( gets,         { "* цена ",       block( crPrice5), "99999.99",{||.F.}} )
aAdd( gets,         { "* кол-во ",     block( crCount5), "999",,;
             {|g| checkCount( g, 19, asCode5, alg5, algSum5,;
                    {{ 3, sumFine1}, { 7, sumFine2},;
                     { 11, sumFine3}, { 15, sumFine4}}, @sumFine5,;
                   {{ asCode1, 6}, { asCode3, 14},;
                    { asCode4,18}, { asCode2, 10}};
)} } )
aAdd( gets,         { " " } )
aAdd( gets,         { "Итого    :", block( totalSum), "99999999.99", {|| .F.} } )
aAdd( gets,         { " " })
if getUp( 5, 5, gets)
  s := select()
  aPaym := {}
  if provOpen()
    if ptr1 > 0 .AND. crCount1 > 0
      sumPlat := crPrice1 * crCount1
      //calcFine( alg1, algSum1, sumPlat, @sumFine, @str)
      
      aAdd( aPaym, { "51", provName[ ptr1], provCode[ ptr1], crName1, asCode1,;
            crPrice1, crCount1, getRo( ptrSchet == 1, "51", provCode[ ptr1]),;
            getSchet( ptrSchet == 1, "51", provCode[ ptr1]), sumFine1,;
            getShortName( "51", provCode[ ptr1], asCode1)})
    endif
    if ptr2 > 0 .AND. crCount2 > 0
      sumPlat := crPrice2 * crCount2
      //calcFine( alg2, algSum2, sumPlat, @sumFine, @str)
      
      aAdd( aPaym, { "51", provName[ ptr2], provCode[ ptr2], crName2, asCode2,;
            crPrice2, crCount2, getRo( ptrSchet == 1, "51", provCode[ ptr2]),;
            getSchet( ptrSchet == 1, "51", provCode[ ptr2]), sumFine2,;
            getShortName( "51", provCode[ ptr2], asCode2)})
    endif
    if ptr3 > 0 .AND. crCount3 > 0
      sumPlat := crPrice3 * crCount3
      //calcFine( alg3, algSum3, sumPlat, @sumFine, @str)
      
      aAdd( aPaym, { "51", provName[ ptr3], provCode[ ptr3], crName3, asCode3,;
            crPrice3, crCount3, getRo( ptrSchet == 1, "51", provCode[ ptr3]),;
            getSchet( ptrSchet == 1, "51", provCode[ ptr3]), sumFine3,;
            getShortName( "51", provCode[ ptr3], asCode3)})
    endif
    if ptr4 > 0 .AND. crCount4 > 0
      sumPlat := crPrice4 * crCount4
      //calcFine( alg4, algSum4, sumPlat, @sumFine, @str)
      
      aAdd( aPaym, { "51", provName[ ptr4], provCode[ ptr4], crName4, asCode4,;
            crPrice4, crCount4, getRo( ptrSchet == 1, "51", provCode[ ptr4]),;
            getSchet( ptrSchet == 1, "51", provCode[ ptr4]), sumFine4,;
            getShortName( "51", provCode[ ptr4], asCode4)})
    endif
    if ptr5 > 0 .AND. crCount5 > 0
      sumPlat := crPrice5 * crCount5
      //calcFine( alg5, algSum5, sumPlat, @sumFine, @str)
      
      aAdd( aPaym, { "51", provName[ ptr5], provCode[ ptr5], crName5, asCode5,;
            crPrice5, crCount5, getRo( ptrSchet == 1, "51", provCode[ ptr5]),;
            getSchet( ptrSchet == 1, "51", provCode[ ptr5]), sumFine5,;
            getShortName( "51", provCode[ ptr5], asCode5)})
    endif
    provClose()
    select (s)
  endif
  if len( aPaym) = 0 then return .F.
  aPaym := aSort( aPaym,,,{|x, y| x[ 8] < y[ 8] .AND. x[ 9] < y[ 9]})
  cardWrite( ptrSchet == 1, accSchet2[ ptrSchet, 2], fName, aPaym)
endif
return .T.
 
static funcDef getShortName with code2:C, code3:C, asCode:C ;
  local arrName, arrCode, arrBuf, ptr, rV := "", arr

  assGetInfo( code2, @arrCode, @arrName, @arrBuf, code3)
  ptr := aTrimmedScan( arrCode, asCode)
  if ptr > 0 
    arr := CtoA( arrBuf[ ptr], ";")
    if len( arr) > 3 then rV := rTrim( rV  := arr[ 4])
  endif
return rV

  
//
// переходим к генерации проводок и печати документов
//
static funcDef cardWrite with isCash:L, debSchet:C, fName:C, aPaym:A ;
  local i := 1, curRo, curSchet, curS, curID, arr, ordInc, buffer, j, arSums,;
        aOrder, strDest, sim, ro, tmpSum, ptrLim, arLim, thisLim, xLim,;
        arSum, arSim, arPtr, curSim
  arSums := {}
  saveLastOrder( {})
  while i <= len( aPaym)
    curRo := aPaym[ i, 8]; curSchet := aPaym[ i, 9]
    curS  := 0; arr := {}; strDest  := ""
    arSum := { 0, 0, 0, 0, 0}
    arSim := { 0, 0, 0, 0, 0}
    arPtr := 1

    operGetInfo( curRo,, @curSim)


    while i <= len( aPaym) .AND. aPaym[ i, 8] = curRo .AND. aPaym[ i, 9] = curSchet
      tmpSum  := aPaym[ i, 6] * aPaym[ i, 7]
      if aPaym[ i, 10] > 0 then tmpSum += aPaym[ i, 10]
      curS += tmpSum
      aAdd( arr, aPaym[ i])      
      strDest += aPaym[ i, 11] + " (" + nStr( aPaym[ i, 7]) + " шт.): " + ;
                 lTrim( strTran( Str( tmpSum, 15, 2), ".", "-")) + ";"
      i++

      arSum[ arPtr] := tmpSum
      arSim[ arPtr] := curSim
      arPtr++

    enddo
    ordInc := if( isCash, ordIncIn( setCurDate()), ordIncMem( setCurDate()) )
    buffer := curSchet + ";" + nStr( len( arr)) + ";"
    for j := 1 to len( arr)
      buffer += arr[ j, 1] + ";" + arr[ j, 3] + ";" + arr[ j, 5] + ";" + ;
            nStr( arr[ j, 7]) + ";" + nStr( arr[ j, 7] * arr[ j, 6] + max( 0, arr[ j, 10]))+ ";" + ;
            nStr( abs( arr[ j,10])) + ";"
    next
    buffer += fName + ";" + myDtoC( setCurDate()) + ";"
    operGetInfo( curRo, @ro, @sim)
    aAdd( arSums, { curSchet, curS, setCurDate(), curRo, ordInc, buffer, strDest, ro})
    if isCash 
       useZabota( .T.)
       aOrder := vznos_nal( ordInc, setCurDate(), fName, curSchet, arSum,;
          confBankName()+','+ rTrim(confFilial()), strDest, confKassa(), arSim, ro,, .T.)
       // aOrder := vznos_nal( ordInc, setCurDate(), fName, curSchet, curS,;
       //   confBankName()+','+ rTrim(confFilial()), strDest, confKassa(), sim, ro,, .T.)
       useZabota( .F.)
       //aOrder[ 15] := padR( "Назначение платежа Обязательные платежи, добровольные взносы;", 78) + "|"
       //aOrder[ 16] := Space( 78) + "|"
       //aOrder[ 17] := Space( 78) + "|"
       printAOrder( aOrder)
       saveLastOrder( aOrder, .F.)
    endif 
  enddo
  if !isCash
    //
    // печатаем поручение
    // 
    curS    := 0
    strDest := ""
    for i := 1 to len( arSums)
      curS    += arSums[ i, 2]
      strDest += arSums[ i, 7]
    next
    if curS > sumRest( debSchet, setCurDate())
      errOK( "Эта проводка вызовет красное сальдо по счету " + debSchet + " !!!")
      return .F.
    endif
    aOrder := memWisc( debSchet, curS, strDest)
    printAOrder( aOrder)
    saveLastOrder( aOrder, .F.)
    //
    // печатаем ордера
    //
    if !confUseAgent()
      for i := 1 to len( arSums)
         aOrder := memOrder( arSums[ i, 5], arSums[ i, 3], debSchet,;
             arSums[ i, 1], arSums[ i, 2], arSums[ i, 7], arSums[ i, 8])
         printAOrder( aOrder)
         saveLastOrder( aOrder, .F.)
      next
    endif
  endif


  if !yesNo( "Клиент подписал документ ?") then return .F.
  for i := 1 to len( arSums)
    curSchet := arSums[ i, 1]
    curS     := arSums[ i, 2]
    curRo    := arSums[ i, 4]
    ordInc   := arSums[ i, 5]
    buffer   := arSums[ i, 6]
    arLim    := buf2Arr( buffer)
    for ptrLim := 1 to len( arLim)
      // thisLim := kasLimitSumma( - arLim[ ptrLim, 4], arLim[ ptrLim, 3])
      // if thisLim == 0
      thisLim := kasLimit( arLim[ ptrLim, 3])
      if thisLim < arLim[ ptrLim, 4]
        messageB( "Нельзя выдать более " + lTrim( Str( thisLim, 0)) + ;
                  " карт типа " + ;
                  getShortName( "51", arLim[ ptrLim, 2],;
                       rTrim( arLim[ ptrLim, 3]))  )
        //
        // двигаем на возврат, поэтому нам не нужны больши переменные цикла
        // и их можно задействовать для возврата лимитов на прежний уровень
        //
        for i := 1 to ptrLim - 1
          kasLimitSumma( arLim[ i, 4], arLim[ i, 3])
        next
        return .F.
      else
        thisLim := kasLimitSumma( - arLim[ ptrLim, 4], arLim[ ptrLim, 3])
      endif
    next          
    if sumWrite( if( isCash, curSchet, debSchet), if( isCash, curS, - curS),;
       setCurDate(), curRo, ordInc)
     if SUMS ->( mRLock())
       curID := "S" + confOtdCode() + padL( SUMS ->( recNo()), 5, "0") + ;
             indDate( SUMS-> dDate) + indTime( SUMS-> tTime)
       SUMS ->buffer := curID
       SUMS ->( netUnLock())
       if WISC ->( mAppend())
         WISC ->schet  := curID
         WISC ->type   := "975 "
         //
         // последним элементом в буфер дописываем тип взноса
         // 1 - наличными
         // 2 - безналично
         //
         WISC ->buffer := buffer + if( isCash, "1", "2") + ";"
         WISC ->( netUnLock())
       endif
     endif
    endif
  next
return .T.

funcDef buf2Arr with buffer:CM local cnt, i, arr := {}, ar
  ar  := CtoA( buffer, ";")
  cnt := val( ar[ 2]) - 1
  for i := 0 to cnt
    aAdd( arr, { ar[ 3 + i * 6], ar[ 4 + i * 6], ar[ 5 + i * 6],;
                 val( ar[ 6 + i * 6]), val( ar[ 7 + i * 6]), val( ar[ 8 + i * 6])}) 
  next
return arr

static funcDef getRo with isCash:L, code2:C, code3:C ;
  local rV := if( isCash, opDecode( "2013"), opDecode( "5010"))
  if isCash .AND. PROVIDER ->( dbSeek( code2 + code3)) .AND. ;
     !empty( PROVIDER ->roCash) 
     rV := opDecode( PROVIDER ->roCash)
  endif
return rV

static funcDef getSchet with isCash:L, code2:C, code3:C local rV := ""
  if PROVIDER ->( dbSeek( code2 + code3)) ;
    then rV := if( isCash, PROVIDER ->accCash, PROVIDER ->accTran)
return rV



//
// очистка getList после изменения поставщика
//
static funcDef clearGets with g:O, preName:C, ptr:N, arSums:A
 if upper( allTrim( g:varGet)) <> upper( allTrim( preName))
  M ->getList[ ptr + 1] : varPut( space( 20))
  M ->getList[ ptr + 2] : varPut( 0 )
  M ->getList[ ptr + 3] : varPut( 0 )
  @ M ->getList[ ptr + 3]:row() + 1, M ->getList[ ptr + 3]:col() - 37 ;
    SAY Space( 40) color normalColor( setGetColor())

  M ->getList[ ptr + 1] : display()
  M ->getList[ ptr + 2] : display()
  M ->getList[ ptr + 3] : display()

  calcTotalSum( arSums, 0, 0)

 endif
return .T.

static funcDef checkCount ;
  with g:O, pos:N, asCode:C, alg:C, algSum:N, arSums:A, sumFine:N, arrCards:A ;
  local rV := .T., cnt := "", isFound, sumPlat, str, get, i, itogCnt


  //if getExitUp( g) .OR. lastkey() = 5 then return .T.
  cnt := kasLimit( rTrim( asCode), @isFound)

  itogCnt := g:varGet()
  for i := 1 to len( arrCards)
    if arrCards[ i, 1] == asCode .AND. !empty( asCode)
      itogCnt += M ->getList[ arrCards[ i, 2]]: varGet
    endif
  next

  //if isFound
    if itogCnt > cnt
      messageB( "Установлен лимит - " + lTrim( Str( cnt, 0)) + " шт. ")
      rV := .F.
    endif
  //endif
  if rV
    sumPlat := M ->getList[ pos + 2]: varGet * M ->getList[ pos + 3]: varGet     
    calcFine( alg, algSum, sumPlat, @sumFine, @str)
    if sumPlat = 0 then sumFine := 0
    calcTotalSum( arSums, sumPlat, sumFine)

    get := M ->getList[ pos + 3]
    if round( sumFine, 2) = 0 then str := Space( 40)
    @ g:row() + 1, g:col() + 3 - 40 SAY padL( str, 40) color normalColor( setGetColor())
  endif
return rV

static funcDef calcTotalSum with arSums:A, sumPl:N, sumFn:N ;
  local i, sumPlat, sumFine, str, totalSum := 0, pos

  totalSum := sumPl + max( 0, sumFn)
  for i := 1 to len( arSums)
    pos := arSums[ i, 1]
    sumPlat := M ->getList[ pos + 2]: varGet * M ->getList[ pos + 3]: varGet     
    sumFine := arSums[ i, 2]
    totalSum += sumPlat + max( 0, sumFine)
  next
  // messageB( "Итого : " + mStr( totalSum))
  M ->getList[ 23]: varPut( totalSum)
  M ->getList[ 23]: display()  
return NIL


//
// блок для чтения карт у данного поставщика
//
static funcDef cardReader with g:O, pos:N, asCode:C, alg:C, algSum:N,;
  provName:A, provCode:A, sumFine:N ;
  local key := 0, charKey, keyStr := ""

  g:setFocus()
  g:exitState := GE_NOEXIT
  while .T.
    key     := inkey( 0)
    charKey := upper( chr( key))
    do case
      case key = K_SPACE
        select2Card( g, pos, @asCode, @alg, @algSum, provName, provCode, @sumFine)
        g:exitState := GE_DOWN
        exit
      case key = K_ENTER .OR. key = K_DOWN
        g:exitState := GE_DOWN
        exit
      case key = K_UP
        g:exitState := GE_UP
        exit
      case key = K_ESC
        g:exitState := GE_ESCAPE
        exit
      otherwise
        // getApplyKey( g, key)
    endcase
  enddo
  g:killFocus()

return NIL


//
// выбор карты у поставщика
//
static funcDef select2Card ;
   with g:O, pos:N, asCode:C, alg:C, algSum:N, provName:A, provCode:A, sumFine:N;
   local rV := .T., ptr, crName, crPrice, assCode, assName, assBuf,;
         ptrName, arrFine, sumPlat, str, get, i

if getExitUp( g) .OR. lastkey() = 5 then return .T.
ptr := aTrimmedScan( provName, M ->getList[ pos]: varGet)
crName := Space( 20); crPrice := 0
if ptr > 1
  assGetInfo( "51", @assCode, @assName, @assBuf, provCode[ ptr])
  //
  // вырежем карты с нулевым лимитом
  //
  i := 1
  while i <= len( assCode)
    if kasLimit( assCode[ i]) <= 0
      aDel( assCode, i); aSize( assCode, len( assCode) -1)
      aDel( assName, i); aSize( assName, len( assName) -1)
      aDel( assBuf,  i); aSize( assBuf,  len( assBuf ) -1)
    else
      i++
    endif
  enddo

  ptrName:= popUp( assName)
  if ( rV := ptrName > 0)
    crName  := assName[ ptrName]
    asCode  := assCode[ ptrName]
    arrFine := CtoA( assBuf[ ptrName], ";")
    crPrice := val( arrFine[ 1])
    alg     := arrFine[ 2]
    algSum  := val( rTrim( arrFine[ 3]))
    M ->getList[ pos + 1]: varPut( padR( lTrim( crName), 20))
    M ->getList[ pos + 1]: display()
    M ->getList[ pos + 2]: varPut( crPrice)
    M ->getList[ pos + 2]: display()

    sumPlat := M ->getList[ pos + 2]: varGet * M ->getList[ pos + 3]: varGet     
    calcFine( alg, algSum, sumPlat, @sumFine, @str)
    get := M ->getList[ pos + 3]
    if sumFine = 0 then str := Space( 40)
    @ get:row() + 1, get:col() + 3 - 40 SAY padL( str, 40) color normalColor( setGetColor())
  endif
endif
return rV

autofunction setValue cardTotalSum init 0.00



//
// обработка алгоритма начисления пеней
//
funcDef calcFine with alg:C, algSum:N, sumPlat:N, sumFine:NU, str:CU ;
  local rV := .T., str1 := "", str2 := ""
  str := ""; sumFine := 0
  if alg $ "0,1,2,3"
    str2 := nStr( algSum)
    if alg $ "0,2"
      str2 += " %,"
      sumFine := sumPlat * algSum/ 100
      str2 += nStr( sumFine) +","
    else
      str2 += " руб.,"
      sumFine := algSum
    endif
  else
    str2 := "вводится "
    if sumPlat > 0
      while !getUp( 5, 5, {{ "Вознаграждение ", block( sumFine), "9999999.99",,;
               {|g|chkSumFine( g, alg, sumPlat)} }})
        messageB( "Введите сумму платежа")
      enddo
    endif
    str2 += nStr( sumFine) + " руб.,"
  endif
  if alg $ "0,1,4"
    str1 := "включено в платеж "
    sumFine := - sumFine
  else
    str1 := "добавл.к платежу "
  endif

  str  := str1 + str2
return rV

funcDef chkSumFine with g:O, alg:C, sumPlat:N
  if alg $ "4" .AND. g:varGet > sumPlat
    messageB( "Сумма вознаграждения должна быть меньше суммы платежа !")
    return .F.
  endif
  if g:varGet < 0 
    messageB( "Введите положительную сумму !" )
    return .F.
  endif
   
return .T.

//
// сторнирование такой проводки
//
funcDef cardStorn local rV, curID, arLim, i
  curID := allTrim( SUMS ->buffer)
  if WISC ->( dbSeek( curID + "975 "))
    arLim := buf2Arr( WISC ->buffer)
    for i := 1 to len( arLim) 
      kasLimitSumma( arLim[ i, 4], arLim[ i, 3])
    next
    if WISC ->( mDelete()) then WISC ->( netUnLock())
  else
    messageB( "Проводка сторнируется без пожелания. Лимиты карт НЕ восстановлены !!!")
  endif
  strnDeleteKassa()
  if SUMS ->( rV := mDelete())
    SUMS ->( netUnLock())
  endif
return rV


//
// Приложение 4  - перечень карт
//
funcDef cardApp4 ;
  local s := select(), arr := {}, ptr, cnt, arrBuf, tmpCode2, isFirst, str,;
        crPrice, recN, i

str := confBankName() + endl()     + endl() + ;
  "Лимиты и тарифы сервиcных карт" + endl() + ;
  "ПБО N " + confOtdNumb()         + endl() + ;
  "на "    + myDtoC( setCurDate()) + endl() + ;
  ""                               + endl() + ;
  replicate( "-", 64)              + endl() + ;
  "|" + padC( "Наименование карты", 21) + "|" + padC( "Номинал", 19)        + ;
  "|" + padC( "Стоимость", 11) + "|" + padC( "Кол-во", 8) + "|" + endl()    + ;
  "|" + space( 21) + "|" + space( 19) + "|" + space( 11) + "|" + space( 8) + ;
  "|" + endl() + replicate( "-", 64)        + endl()


if basesNSOpen( { assInfo(), prov2Info()})
  ASSORT ->( dbSeek( "51"))
  statStartUp( "Лимиты и тарифы сервиcных карт")
  while ASSORT ->code2 == "51" .AND. ASSORT ->( !eof())
    if ASSORT ->Live 
        cnt := kasLimit( ASSORT ->code)
        if PROV2 ->( dbSeek( ASSORT ->code3)) .AND. cnt > 0
          arrBuf := CtoA( ASSORT ->buffer, ";")
          crPrice:= arrBuf[ 1]
          ptr := aScan( arr, {|x| x[ 1] == ASSORT ->code3}, 1)
          if ptr == 0
            aAdd( arr, { ASSORT ->code3, PROV2 ->name, {}})
            ptr := len( arr)
          endif
          aAdd( arr[ ptr, 3], { arrBuf[ 5], crPrice, cnt})
        endif
    endif
    skip 1 alias ASSORT
    statProc()
  enddo
  //
  // массив сформирован, теперь печатаем его
  //
  for ptr := 1 to len( arr)
    for i := 1 to len( arr[ ptr, 3])
      str += newLine( i == 1, arr[ ptr, 2], arr[ ptr, 3, i, 1], ;
             arr[ ptr, 3, i, 2], arr[ ptr, 3, i, 3])
      str += "|" + if( i < len( arr[ ptr, 3]), space( 21) + "|",;
                       replicate( "-", 21) + "-" )   +     ; 
             replicate( "-", 41) + endl()
    next
  next
  statExit()
  basesMClose( { assInfo(), prov2Info()})
  printAOrder( ctoA( str, endl()) )
endif
select (s)
return NIL

static funcDef newLine ;
  with isFirst:L, provName:C, nominal:C, price:C, cnt:N ;
  local str := "|", sPrice := Str( val( price), 11, 2)

 sPrice := left( sPrice, 8) + "=" + right( sPrice, 2)
 str += if( isFirst, padR( provName, 21), space( 21)) + "|"
 if len( rTrim( nominal)) > 19
   nominal := razbivka( nominal, {{ 19, "L"}, { 19, "L"}})
 else 
   nominal := { nominal, ""}
 endif
 str += padC( nominal[ 1], 19) + "|"
 str += padC( sPrice,  11) + "|" 
 str += padC( nStr( cnt),  8) + "|"
 str += endl()
 if !empty( nominal[ 2])
   str += "|" + space( 21) + "|" + padC( nominal[ 2], 19) + ;
          "|" + space( 11) + "|" + space( 8) + "|" + endl()
 endif

return str
