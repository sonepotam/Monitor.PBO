*------------------------------------------------------------------------------
* 
*                         Программа  : MON.EXE
*                         Файл       : baseSrv2.prg
*                         Автор      : Цейтлин П.М.
*                         Назначение : Ведение долгосрочных поручений
*                                      (лимит карточного кредитования)
*------------------------------------------------------------------------------
#include "clippExt.ch"
#include "common.ch"
#include "getExit.ch"
#include "inkey.ch"
#include "gsz.ch"
* ------------------ оформление карточного овердрафта
*
* 29-07-2007 TMV
* Изменение карточных счетов на 45509
* Ф-ии chkCredRests(), chk2CredRests().
*
funcDef CredEdit ;
   local rV := .F., gets, aSource, aSource2,  aDest, aDest2, canEdit    ,;
         ptrCur, ptrCard, tmpSum, str1, str2, efPrc, plusPrc, strPrc, s2,;
         crType := "11", pensSum, dopSum, s1, pctSum, strSum, s, minStr ,;
         oldSumma, ro, sim, aOrder, ordInc, strNazn, printAgain := .T.  ,;
         prevSumma, cntMonths, curD

if !chkCredUsage() then return .F.
if credExist()
   messageB( "У этого клиента уже есть такой договор !!!")
   return .F.
endif
if hasTechCred()
  messageB( "У клиента есть карточный овердрафт !")
  return .F.
endif


if CONTRACT ->dateCur == setCurDate()
  OK( { "Это поручение можно будет корректировать завтра",;
        "сегодня его можно закрыть, удалить проводку по ",;
        "внесению страховой суммы, а затем открыть новое",;
        "поручение."})
  return .F.
endif
str1 := str2 := 0
tmpSum := CONTRACT ->summa/ 100
s := select()
if credOpen()
pensSum  := CLIENTS ->credSumma/ 100
dopSum   := credMaxSumma( crType)
// s1       := aMin( { pensSum, dopSum})
// s1       := int( s1)
s1 := int( pensSum)
if empty( CONTRACT ->buffer)
  prevSumma := tmpSum
else
  prevSumma := val( CONTRACT ->buffer)/ 100
endif

accPrizList( "15 1  1  ", @aSource,   @aSource2)
ptrCur := max( 1, aScan( aSource2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))
if len( aSource) < 1
   messageB( "У клиента нет открытых текущих пенсионных счетов !!!")
   return .F.
endif

accPrizList( "15 4  1  ", @aDest,   @aDest2)
ptrCard := max( 1, aScan( aDest2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))
if len( aDest) < 1
   messageB( "У клиента нет открытых карточных пенсионных счетов !!!")
   return .F.
endif

canEdit := .F.
if empty( CONTRACT ->dateBeg)
   CONTRACT ->dateBeg := setCurDate() + 1
   CONTRACT ->dateEnd := addMonth( CONTRACT ->dateBeg, 2)
   cntMonths := 2
else
  curD := CONTRACT ->dateBeg
  cntMonths := 0
  while curD < CONTRACT ->dateEnd
    curD := addMonth( curD, 1)
    cntMonths++
  enddo
endif

if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc  := confcrCredDoc()
   CONTRACT ->docNumb := CONTRACT ->intDoc
   CONTRACT ->dateCur := setCurDate()
   canEdit := .T.
endif

//
// расчет процентов и страховой суммы
//
getProcents( @efPrc, @plusPrc, @strPrc, @minStr)
getSums( efPrc, strPrc, minStr, CONTRACT ->summa/ 100, cntMonths, @str1, @str2)
OldSumma := str1
getSums( efPrc, strPrc, minStr, s1, cntMonths, @str1, @str2)

//pctSum := int( round( s1 * efPrc * 60/ 365/ 100, 0))
pctSum := s1 * efPrc * 60/ 365/ 100
s2     := aMin( { s1 - pctSum, dopSum})
s2     := int( s2)
tmpSum := CONTRACT ->summa/ 100

gets := {{ "Установка лимита овердрафта по карте  "                   },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                   },;
         { "Номер         :", block( CONTRACT ->intDoc),,{||.F.}      },;
         { "Начало        :", block( CONTRACT ->dateBeg),,{||.F.}     },;
         { "Окончание     :", block( CONTRACT ->dateEnd),,{||.F.}     },;
         { "Максимальная сумма : " + mStr( s2)                        },;
         { "" },;
         { "Текущий счет    :", block( ptrCur ), aSource,{||canEdit}   },;
         { "Карточный счет  :", block( ptrCard), aDest,  {||canEdit}     },;
         { "Период действия :", block( cntMonths), "99", {||canEdit},{|g| chkMonths(g)}},;
         { "* месяцев"                                                 },;
         { "Сумма лимита    :", block( tmpSum), "999999999",, {|g| chkCredSum(g, s2)    }}  }
if getUp( 5, 5, gets)
   CONTRACT ->code    := CLIENTS ->code
   CONTRACT ->code3   := "08"
   CONTRACT ->status  := 1
   CONTRACT ->summa   := tmpSum * 100
   CONTRACT ->mBuffer := aSource2[ ptrCur, 2] + "," + aDest2[ ptrCard, 2]
   CONTRACT ->buffer  := nStr( prevSumma)
   CONTRACT ->dateEnd := addMonth( CONTRACT ->dateBeg, cntMonths)
   getSums( efPrc, strPrc, minStr, tmpSum, cntMonths, @str1, @str2)
   str1 := str1 - oldSumma
   rV     := credFirstPaym( str1, str2)
   ordInc := ordIncIn( setCurDate())
   while printAgain
     credPrint( canEdit)
     SaveLastOrder( { getSavedOpen()})
     strNazn  := "Оплата страхового взноса/доп.страх. взноса за страхование от н/с.Договор " + CONTRACT ->intDoc
     operGetInfo( opDecode( "3019"), @ro, @sim)
     aOrder := vznos_nal( ordInc,setCurDate(), cliFullName(),;
        confStrSchet(), str1, confBankName()+','+ rTrim(confFilial()),;
        strNazn, confKassa(), sim, ro)
     printAOrder( aOrder)
     saveLastOrder( aOrder, .F.)
     printAgain := !yesNo( "Документы распечатаны правильно ?")
   enddo
   if str1 > 0 .AND. !sumWrite( confStrSchet(), str1, setCurDate(), opDecode( "3019"),;
      ordInc,,CONTRACT ->docNumb)
      errLog( { "Проводка по оплате страхового взноса не сделана" ,;
                "и поручение не сформировано !!!"    ,;
                "Запишите номер счета и ФИО клиента" ,;
                "и обратитесь на узел связи !!!" })
   endif

endif
credClose()
select (s)
endif
return rV

static funcDef chkMonths with g:O 
  if g:varGet >12 .OR. g:varGet < 2 
    messageB( "Можно ввести период от 2 до 12 месяцев включительно !")
    return .F.
  endif
return .T.

static funcDef getSums ;
  with efPrc:N, strPrc:N, minStr:N, tmpSum:N, cntMonths:N, str1:NU, str2:NU ;
  local pctSum, strSum

   // pctSum := int( round( tmpSum * efPrc * 60/ 365/ 100, 0))
   pctSum := tmpSum * efPrc * 60/ 365/ 100
   if minStr > 0
     strSum := min( tmpSum * strPrc* cntMonths/ 100, minStr)
   else
     strSum := tmpSum * strPrc* cntMonths/ 100
   endif
   str1   := strSum
   str2   := tmpSum + pctSum

return NIL



static funcDef getProcents with efPrc:NU, plusPrc:NU, strPrc:NU, minStr:NU ;
  local rV := .T., s := select(), opened := select( "CRED_PCT") > 0, ss := .F.
  plusPrc   := vidGetPrc( "15 7  4  0  ")
  efPrc := 0; strPrc := 0; minStr := 0
  if !opened then ss := credOpen()
  if opened .OR. ss
    if CRED_PCT ->( dbSeek( "11"))
      efPrc   := CRED_PCT ->procent
      strPrc  := CRED_PCT ->proc_str
      minSTr  := CRED_PCT ->min_str
    endif
    if !opened .AND. ss then credClose()
  endif
  select (s)
return rV

static funcDef chkCredSum with g:O, s2:N local rV := .F.,  credSum, contSum
  if g:varGet <= 0
    messageB( "Можно вводить только положительную сумму !")
  else
    credSum := s2
    contSum := CONTRACT ->summa/ 100
    if g:varGet <= credSum .AND. g:varGet >= contSum
       rV := .T.
    else
      errOK( { "Вы можете заказать овердрафт не менее " + mStr( contSum),;
               "и не более " + mStr( credSum) })
    endif
  endif
return rV

funcDef credFirstPaym with str1:N, str2:N ;
  local rV := .F., curSchet, cardSchet, mustContinue
  curSchet  := left(   CONTRACT ->mBuffer, 20)
  cardSchet := subStr( CONTRACT ->mBuffer, 22, 20)
  if WISC ->( dbSeek( cardSchet + "955 "))
    mustContinue := WISC ->( mrLock())
  else
    mustContinue := WISC ->( mAppend())
  endif
  if ( rV   := mustContinue)
     WISC ->schet  := cardSchet
     WISC ->type   := "955 "
     WISC ->buffer := CONTRACT ->code + ";" + CONTRACT ->intDoc    + ";" + ;
       curSchet + ";" + cardSchet+ ";" + DtoC( setCurDate())       + ";" + ;
       DtoC( CONTRACT ->dateBeg) + ";" + DtoC( CONTRACT ->dateEnd) + ";" + ;
       nStr( CONTRACT ->summa/100)+";"+nStr( str1) + ";" + nStr( str2) + ";"
     WISC ->( netUnLock())
  endif
return rV

static funcDef credGetFPaym with str1:NU, str2:NU ;
  local rV := .F., curSchet, cardSchet, arr
  curSchet  := left(   CONTRACT ->mBuffer, 20)
  cardSchet := subStr( CONTRACT ->mBuffer, 22, 20)
  str1 := 0; str2 := 0
  if WISC ->( dbSeek( cardSchet + "955 "))
     arr  := CtoA( WISC ->buffer, ";")
     str1 := val( arr[ 9])
     str2 := val( arr[10])
  endif
return rV


funcDef chkCredUsage local rV := .F.
 if CredCards1()
   if chkCredRests()
    if CLIENTS ->credSumma > 0
       rV := .T.
    else
      messageB( "Филиал отказал в кредитовании !")
    endif
   endif
 endif
return rV

static funcDef credCards1 local rV := .F., i, arr, found100Prc := .F.
 //
 // поиск поручений по карточному счету, у них есть карты
 // Cirrus/Maestro/Петрокарт или VISA Electorn
 //
 CONTRACT ->( baseSavePos())
 CONTRACT ->( dbSetOrder( 1))
 ACC      ->( baseSavePos())
 ACC      ->( dbSetOrder( 1))
 if CONTRACT ->( dbSeek( CLIENTS ->code + "09"))
   while CONTRACT ->CODE == CLIENTS ->code .AND. CONTRACT ->( !Eof())
     if CONTRACT ->code2 == "09" .AND. CONTRACT ->code3 == "71" .AND. ;
        CONTRACT ->status == 1
        if CONTRACT ->summa = - 100
          if ACC ->( dbSeek( CONTRACT ->creditACC))
            if left( ACC ->PRIZ, 9) == "15 4  1  "
              found100Prc := .T.
            endif
          endif
        endif
        arr := CtoA( CONTRACT ->mBuffer, ";")
        for i := 1 to len( arr)
          arr[ i] := CtoA( arr[ i], ",")
          if .T. // arr[ i, 1] $ "40004," // есть карта заданного типа
            if arr[ i, 3] $ "A,O"  // она есть или принята к изготовлению
              rV := .T.
            endif
          endif
        next
     endif
     skip 1 alias CONTRACT
   enddo
 endif
 ACC      ->( baseRestPos())
 CONTRACT ->( baseRestPos())
 if !found100Prc
   errOK( { "У клиента нет поручения на перечисление 100-%"  ,;
            "на КПС денежных средств, поступающих на ТПС"})
 endif
 if found100Prc
   rV := rV .OR. chkCredWisc()
   if !rV then messageB( "У клиента нет карт VISA/Electron или Cirrus/Maestro")
 else
   rV := .F.
 endif
return rV

static funcDef chkCredWisc local rV := .F.
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  ACC ->( dbSeek( CLIENTS ->code))
  while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
    if accisCard( ACC ->schet)
       if WISC ->( dbSeek( ACC ->schet + "979 "))
          rV := .T.; exit
       endif
    endif
    skip 1 alias ACC
  enddo
  ACC ->( baseRestPos())
return rV


static funcDef chkCredRests local rV := .T., S
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  if ACC ->( dbSeek( CLIENTS ->code))
    while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
      // if left( ACC ->schet, 5) == "45503" .AND. ACC ->PRIZ <> "15 7  4  0  "
// ACC45503 TMV Изменить на 45509 15-08-2002
// Replaced by TMV 29-07-2004
//      if left( ACC ->schet, 5) == "45503" .AND. !isCardCred()
      if left( ACC ->schet, 5) == "45509" .AND. !isCardCred()
        if sumRest( ACC ->schet, setCurDate()) <> 0 then S := "По счету " + ACC ->schet + " есть остаток !"
        if ( sumPType( ACC ->schet, opDecode("2004")) - ;
             sumSType( ACC ->schet, opDecode("2004")) ) > 0
           S  := "По счету " + ACC ->schet + " есть задолженность по процентам !"
        endif
        if !empty( S); messageB( S); rV := .F.; exit; endif
      endif
      skip 1 alias ACC
    enddo
  endif
  ACC ->( baseRestPos())
return rV

//
// Остатки по всем счетам типа 15 7 и проценты по ним
//
static funcDef chk2CredRests local rV := .T., S
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  if ACC ->( dbSeek( CLIENTS ->code))
    while ACC ->code == CLIENTS ->code .AND. ACC ->( !eof())
// ACC45503 TMV Изменить на 45509  15-08-2002
// Replaced by TMV 29-07-2004
//      if left( ACC ->schet, 5) == "45503" .AND. left( ACC ->PRIZ, 6) == "15 7  "
      if left( ACC ->schet, 5) == "45509" .AND. left( ACC ->PRIZ, 6) == "15 7  "
        if sumRest( ACC ->schet, setCurDate()) <> 0 then S := "По счету " + ACC ->schet + " есть остаток !"
        if ( sumPType( ACC ->schet, opDecode("2004")) - ;
             sumSType( ACC ->schet, opDecode("2004")) ) > 0
           S  := "По счету " + ACC ->schet + " есть задолженность по процентам !"
        endif
        if !empty( S); messageB( S); rV := .F.; exit; endif
      endif
      skip 1 alias ACC
    enddo
  endif
  ACC ->( baseRestPos())
return rV


funcDef credExist with param:NU local rV := .F., recNo
param := 0
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 1))
recNo := CONTRACT ->( recNo())
CONTRACT ->( dbSeek( CLIENTS ->code + "14", .F.))
while CONTRACT ->code  == CLIENTS ->code .AND. ;
      CONTRACT ->code2 == "14" .AND. CONTRACT ->( !eof())
  if CONTRACT ->status == 1 .AND. CONTRACT ->code3 == "08"
     param++
     if CONTRACT ->( recNo()) <> recNo then rV := .T.
  endif
  skip 1 alias CONTRACT
enddo
CONTRACT ->( baseRestPos())
return rV

funcDef credPrint with newContract:LU ;
 local arr, efPrc, plusPrc, strPrc, str1, str2, sch711, sch718, minStr

 sch711 := left( CONTRACT ->mBuffer, 20)
 sch718 := subStr( CONTRACT ->mBuffer, 22, 20)
 getProcents( @efPrc, @plusPrc, @strPrc, @minStr)
 credGetFPaym( @str1, @str2)
 if newContract
   arr := {{ "@summa",    getS( CONTRACT ->summa/ 100)},;
           { "@curPcts",  nStr( plusPrc)       },;
           { "@sch711",   sch711               },;
           { "@sch718",   sch718               },;
           { "@d1",       CONTRACT ->dateBeg   },;
           { "@d2",       CONTRACT ->dateEnd   },;
           { "@datOpen",  CONTRACT ->dateCur   },;
           { "@summaRSO", getS( str1)          },;
           { "@sum105",   getS( str2)          },;
           { "@docNumb",  CONTRACT ->intDoc    }}
   servDummy( arr)
   printOpen( "lim1")
 else
   arr := {{ "@summa",    getS( CONTRACT ->summa/ 100)},;
           { "@curPcts",  nStr( plusPrc)       },;
           { "@sch711",   sch711               },;
           { "@sch718",   sch718               },;
           { "@d1",       CONTRACT ->dateBeg   },;
           { "@d2",       CONTRACT ->dateEnd   },;
           { "@datOpen",  setCurDate()         },;
           { "@datedoc",  CONTRACT ->dateCur   },;
           { "@summaRSO", getS( str1)          },;
           { "@sum105",   getS( str2)          },;
           { "@szsk",     confSZSK()           },;
           { "@docNumb",  CONTRACT ->intDoc    }}
   servDummy( arr)
   printOpen( "lim2")
 endif
 servDummy( {})
return .T.

funcDef credcrClose local rV := .F., cardSchet, arr
  cardSchet := substr( CONTRACT ->mBuffer, 22, 20)
  if CONTRACT ->dateCur == setCurDate()
    if WISC ->( dbSeek( cardSchet + "955 "))
      if WISC ->( rV := mDelete())
        WISC ->( netUnLock())
      endif
    endif
  else
    if CONTRACT ->dateEnd > setCurDate()
      if chk2CredRests()
        arr := {{ "@docNumb",  CONTRACT ->intDoc    } ,;
                { "@datedoc",  CONTRACT ->dateCur   }}
        servDummy( arr)
        printOpen( "lim3")
        servDummy( {})
        if WISC ->( rV := mAppend())
          WISC ->schet  := cardSchet
          WISC ->type   := "954"
          WISC ->buffer := CONTRACT ->code + ";" + CONTRACT ->intDoc + ";" + ;
                 myDtoC( setCurDate() + 1) + ";"
          WISC ->( netUnLock())
        endif
      endif
    else
      messageB( "Дата окончания поручения меньше текущей даты !")
    endif
    // messageB( "Можно закрывать такие поручения только в день открытия !")
  endif
return rV