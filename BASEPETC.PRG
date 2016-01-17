*============================================================================
* 
*                       Программа : MON.EXE
*                       Файл      : basePetc.prg
*                       Назначение: Поручение по петроэлектросбыту
*
*                       Автор     : Цейтлин П.М.
*
*--------------------- оплата за телефоны ----------------------
#include "common.ch"
#include "clippExt.ch"

funcDef petcEdit with newContract:L;
   local accSchet, accSchet2, ptrSchet, gets, tmpName, tmpStreet  ,;
         tmpFlat, tmpBlock, tmpHouse, tmpPct, firstDate, nextMonth,;
         year1, month1, year2, month2, provName, provCode         ,;
         ptrProl, ptrProv, rV, yesNo, tmpStr, useSeparat          ,;
         tmpDayPct, tmpNightPct, ptrSeparat := 0

yesNo    := { "Есть", "Нет"}
accPrizList( "15 1", @accSchet,   @accSchet2)
ptrSchet := max( 1, aScan( accSchet2, {|x| x[ 2] == CONTRACT ->debetAcc}, 1))
ptrProl  := if( subStr( CONTRACT ->buffer, 3, 1) = "N", 2, 1)
nextMonth := addMonth( setCurDate(), 1)
firstDate := nextMonth
if !empty( CONTRACT ->dateBeg) then nextMonth := CONTRACT ->dateBeg
year1  := Year(  nextMonth)
month1 := Month( nextMonth)

nextMonth := addMonth( nextMonth, 1)
if !empty( CONTRACT ->dateEnd) then nextMonth := CONTRACT ->dateEnd

year2  := Year(  nextMonth)
month2 := Month( nextMonth)
fillProvList( "04", @provName, @provCode)
ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))

if len( provName) < 1
    messageB( "Для этой услуги нет поставщиков !!!")
    return .F.
endif



if empty( CONTRACT ->intDoc)
   CONTRACT ->intDoc := confPetcDoc()
endif
useSeparat := isSeparat( CONTRACT ->mBuffer)

tmpStr    := CONTRACT ->mBuffer
tmpName   := getKeyWord( tmpStr, "ФИО",   cliFullName())
tmpStreet := getKeyWord( tmpStr, "УЛ.",   CLIENTS ->Street)
tmpHouse  := getKeyWord( tmpStr, "ДОМ",   CLIENTS ->house)
tmpBlock  := getKeyWord( tmpStr, "КОРП.", CLIENTS ->Block)
tmpFlat   := getKeyWord( tmpStr, "КВ.",   CLIENTS ->Flat)
if useSeparat
   tmpDayPct   := getKeyWord( tmpStr, "РАСХОД-ДЕНЬ", " 100")
   tmpNightPct := getKeyWord( tmpStr, "РАСХОД-НОЧЬ", " 100")
   ptrSeparat  := 1
else
   tmpPct      := getKeyWord( tmpStr, "РАСХОД", " 100")
endif

tmpName   := padR( tmpName,   30)
tmpStreet := padR( tmpStreet, 30)
tmpHouse  := padR( tmpHouse,   7)
tmpBlock  := padR( tmpBlock,   4)
tmpFlat   := padR( tmpFlat,    4)
 
gets := {{ "Плата за электроэнергию"                                       },;
         { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                                     },;
         { "Номер        :", block( CONTRACT ->intDoc),,{|| .F.}   },;
         { "*Абонентский номер :",     block( CONTRACT ->docNumb), "9999999",        ;
                 {|| newContract}, {|g| checkPetc( g)                      }},;
         { "Дебет.счет   :", block( ptrSchet), accSchet,{||newContract}    },;
         { "*" + chr( 25)                                                  },;
         { "ФИО          :", block( tmpName),,                               ;
                                 {|| newContract},{|g| !empty( g: varGet)  }},;
         { "Улица        :", block( tmpStreet),,                             ;
                 {|| newContract}, {|g| checkStreet( g)                   }},;
         { "Дом          :", block( tmpHouse),,                              ;
                 {|| newContract}, {|g| checkHouse( g)                    }},;
         { "*Корпус :",      block( tmpBlock),,                              ;
                 {|| newContract},  {|g| checkBlock( g, "0")              }},;
         { "*Квартира :",    block( tmpFlat),,                               ;
                 {|| newContract}, {|g| checkFlat( g, "0")                }},;
         { ""                                                              }}
if useSeparat
  aAdd( gets, { "Средний расход     дневное время :", block( tmpDayPct),"@K 99999",;
           {|| newContract}, {|g| checkRashod( g)}})
  aAdd( gets, { "*    ночное время :", block( tmpNightPct),"@K 99999",;
           {|| newContract}, {|g| checkRashod( g)}})
else
  aAdd( gets, { "Средний расход :", block( tmpPct),"@K 99999",;
           {|| newContract}, {|g| checkRashod( g)}})
endif
aAdd( gets, { "Начало         :", block(month1), aClone( aMonths()),{|| newContract}})
aAdd( gets, { "*" + chr( 25) })
aAdd( gets, { "*", block( year1), "9999",{|| newContract}         ,;
      {|g| appFirstDate( g, firstDate, 10 + ptrSeparat) }})
aAdd( gets, { "*г."          })
aAdd( gets, { "Окончание      :", block( month2), aMonths() })
aAdd( gets, { "*" + chr( 25) })
aAdd( gets, { "*",              block( year2), "9999",,;
      {|g| appLastDate( g, firstDate, 10 + ptrSeparat, 12 + ptrSeparat, .T.)}})
aAdd( gets,        { "*г." })
aAdd( gets, { "Пролонгация    :", block( ptrProl), yesNo })

if rV := getUp( 4, 5, gets)
   if useSeparat
     setKeyWord( @tmpStr, "РАСХОД-ДЕНЬ",  tmpDayPct  )
     setKeyWord( @tmpStr, "РАСХОД-НОЧЬ",  tmpNightPct)
   else
     setKeyWord( @tmpStr, "РАСХОД",  tmpPct)
   endif
   setKeyWord( @tmpStr, "ФИО",     tmpName)
   setKeyWord( @tmpStr, "УЛ.",     tmpStreet)
   setKeyWord( @tmpStr, "ДОМ",     tmpHouse)
   setKeyWord( @tmpStr, "КОРП.",   tmpBlock)
   setKeyWord( @tmpStr, "КВ.",     tmpFlat)

   CONTRACT ->debetAcc := accSchet2[ ptrSchet, 2]
   CONTRACT ->mBuffer  := tmpStr
   CONTRACT ->dateBeg  := packDate( 1, month1, year1)
   CONTRACT ->dateEnd  := lastDay( packDate( 1, month2, year2))
   if empty( CONTRACT ->dateCur) then CONTRACT ->dateCur  := setCurDate()
   CONTRACT ->code3    := provCode[ ptrProv]
   CONTRACT ->buffer   := left( CONTRACT ->buffer, 2) + ;
      if( ptrProl <= 1, "Y", "N") + subStr( CONTRACT ->buffer, 4)
   petcFirstPaym()
endif

return rV


static funcDef isSeparat with str:CM local rV
if empty( str)
   rV := dialog( { "Тип прибора по учету расхода электроэнергии, установленный у абонента"},;
                        { "Общесуточного учета", "Раздельного учета"}) == 2
else
   rV := at( "РАСХОД-ДЕНЬ", CONTRACT ->mBuffer) > 0 .AND. ;
         at( "РАСХОД-НОЧЬ", CONTRACT ->mBuffer) > 0
endif
return rV 

static funcDef checkRashod with g:O
  if val( g:varGet) <= 0
     messageB( "Вы должны ввести положительное число !!!")
     return .F.
  endif
return .T.

static funcDef checkPetc with g:O local rV := .T., phoneNumb, recNo

phoneNumb := allTrim( g:varGet)
if empty( phoneNumb)
   messageB( "Вы должны ОБЯЗАТЕЛЬНО ввести абонентский номер !!!")
   return .F.
endif

if len( phoneNumb) < 7
   messageB( "Абонентский номер не может быть МЕНЬШЕ 7 символов !!!")
   return .F.
endif

CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 2))
recNo := CONTRACT ->( recNo())
CONTRACT ->( dbSeek( phoneNumb))
phoneNumb := padR( phoneNumb, len( CONTRACT ->docNumb))
while CONTRACT ->docNumb == phoneNumb .AND. CONTRACT ->( !eof())
   if CONTRACT ->( recNo()) <> recNo .AND. ;
      CONTRACT ->docNumb == phoneNumb .AND. CONTRACT ->status = 1 .AND. ;
      CONTRACT ->code2   == "04"
      messageB( "Номер " + phoneNumb + " уже оплачивается со счета " + ;
               CONTRACT ->debetAcc + " !!!")
      CONTRACT ->( baseRestPos())
      return .F.
   endif
   skip 1 alias CONTRACT
enddo
CONTRACT ->( baseRestPos())

return .T.

funcDef petcPrint with newContract:L, isAnn:LU ;
  local provName, provCode, ptrProv, aStr, tmpStr , prol1, prol2, pct, pCode,;
        tmpName, abAddr, bb, ff, hh, ss, ann1, ann2, nextD, strPeriod, addr1,;
        addr2, otdFil
   //messageB( "Printing")
   DEFAULT isAnn TO .F.
   ann1 := ann2 := ""
   strPeriod := "с " + DtoC( CONTRACT ->dateBeg) + " по " + DtoC( CONTRACT ->dateEnd)
   if !newContract
      ann1 := "ПОРУЧЕНИЕ " + rTrim( CONTRACT ->intDoc) + " от " + ;
              rDate( CONTRACT ->dateCur) + " АННУЛИРОВАНО ПО МОЕМУ"
      ann2 := "ТРЕБОВАНИЮ."
      nextD := addMonth( setCurDate(), 1)
      // strPeriod := "прекращено с " + rMonth( nextD) + " " + sYear( nextD)
   endif

   fillProvList( "04", @provName, @provCode)
   tmpStr  := CONTRACT ->mBuffer
   ptrProv := max( 1, aScan( provCode, {|x| x == CONTRACT ->code3}, 1))
   aStr    := CtoA( provName[ ptrProv], " ")
   
   if confUseAgent()
     addr1   := razbivka( cliFullAddr(), {{ 55, "L"}, { 55, "L"}})
   else
     addr1   := razbivka( cliPetsAddr(), {{ 55, "L"}, { 55, "L"}})
   endif
   addr2   := addr1[ 2]
   addr1   := addr1[ 1]

   ss        := getKeyWord( tmpStr, "УЛ.",   ""   )
   hh        := getKeyWord( tmpStr, "ДОМ",   ""   )
   bb        := getKeyWord( tmpStr, "КОРП.", ""   )
   ff        := getKeyWord( tmpStr, "КВ.",   ""   )
   if isSeparat( tmpStr)
      pct := "ДНЕВНОЕ ВРЕМЯ - "      + getKeyWord( tmpStr, "РАСХОД-ДЕНЬ","00")+;
             "       НОЧНОЕ ВРЕМЯ - "+ getKeyWord( tmpStr, "РАСХОД-НОЧЬ","00")
   else
      pct       := getKeyWord( tmpStr, "РАСХОД","00")
   endif
   tmpName   := getKeyWord( tmpStr, "ФИО",   cliFullName())
   abAddr    := rTrim( ss) + " ДОМ " + rTrim( hh)
   if !empty( bb) .AND. bb <> "0" then abAddr += " КОРП. "   + rTrim( bb)
   if !empty( ff) then abAddr += " КВАРТИРА " + rTrim( ff)
   abAddr    := razbivka( abAddr, {{ 44, "L"}, { 44, "L"}})

   prol1    := prol2 := ""
   if subStr( CONTRACT ->buffer, 3, 1) == "Y"
      prol1 := "8. НАСТОЯЩЕЕ ПОРУЧЕНИЕ МОЖЕТ БЫТЬ ПРОЛОНГИРОВАНО НА ТАКОЙ ЖЕ ПЕРИОД,"
      prol2 := "   ЕСЛИ МНОЙ НЕ БУДЕТ ПИСЬМЕННО ЗАЯВЛЕНО О ЕГО ПРЕКРАЩЕНИИ."
   endif
   otdFil := rTrim( confFilial()) + " " + confBankName()
   if confUseAgent() then otdFil := rTrim(confOtdName()) + "," + otdFil

   servDummy( {{ "@docNumb",  CONTRACT ->intDoc    },;
               { "@debSchet", CONTRACT ->debetAcc  },;
               { "@provName", ProvName[ ptrProv]   },;
               { "@abAddr1",  abAddr[ 1]           },;
               { "@abAddr2",  abAddr[ 2]           },;
               { "@period",   strPeriod            },;
               { "@abName",   tmpName              },;
               { "@fromD",    rDate( CONTRACT ->dateCur)},;
               { "@abNumb",   CONTRACT ->docNumb   },;
               { "@prol1",    prol1                },;
               { "@prol2",    prol2                },;
               { "@pct",      pct                  },;
               { "@ann1",     ann1                 },;
               { "@ann2",     ann2                 },;
               { "@addr1",    addr1                },;
               { "@addr2",    addr2                },;
               { "@otdfil",   otdFil}})
   printOpen( "petc1")
   servDummy( {})
return NIL

static funcDef cliPetsAddr local rV := ""

if !empty( CLIENTS ->Street) then rV += rTrim( CLIENTS ->Street)
if !empty( CLIENTS ->House)  then rV += " ДОМ "     + rTrim( CLIENTS ->House)
if !empty( CLIENTS ->Block)  then rV += " КОРП. "    + rTrim( CLIENTS ->Block)
if !empty( CLIENTS ->Flat)   then rV += " КВАРТИРА " + rTrim( CLIENTS ->Flat) 

if right( rV, 1) = "," then rV := left( rV, len( rV) - 1)
return rV



funcDef petcFirstPaym ;
   local rV, prol, tmpName, tmpStreet, tmpHouse, tmpBlock, tmpFlat,;
         tmpPct, tmpStr, isSeparat

if rV := WISC ->( mAppend())

   tmpStr    := CONTRACT ->mBuffer
   tmpName   := getKeyWord( tmpStr, "ФИО",   cliFio())
   tmpStreet := getKeyWord( tmpStr, "УЛ.",   CLIENTS ->Street)
   tmpHouse  := getKeyWord( tmpStr, "ДОМ",   CLIENTS ->house)
   tmpBlock  := getKeyWord( tmpStr, "КОРП.", CLIENTS ->Block)
   tmpFlat   := getKeyWord( tmpStr, "КВ.",   CLIENTS ->Flat)
   isSeparat := isSeparat( CONTRACT ->mBuffer)
   if isSeparat 
      tmpPct    := getKeyWord( tmpStr, "РАСХОД-ДЕНЬ", "0") + ";1;" + ;
                   getKeyWord( tmpStr, "РАСХОД-НОЧЬ", "0")
   else
      tmpPct    := getKeyWord( tmpStr, "РАСХОД", "0") + ";0;0"
   endif

   prol := if( subStr( CONTRACT ->buffer, 3, 1) == "Y", "1", "0")
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := "944"
   WISC ->buffer := CONTRACT ->intDoc          + ";" + ;
                    CONTRACT ->debetAcc        + ";" + ;
                    myDtoC( CONTRACT ->dateBeg)  + ";" + ;
                    myDtoC( CONTRACT ->dateEnd)  + ";" + ;
                    CONTRACT ->code2           + ";" + ;
                    CONTRACT ->code3           + ";" + ;
                    prol                       + ";" + ;
                    CONTRACT ->docNumb         + ";" + ;
                    tmpName                    + ";" + ;
                    tmpStreet                  + ";" + ;
                    tmpHouse                   + ";" + ;
                    tmpBlock                   + ";" + ;
                    tmpFlat                    + ";" + ;
                    tmpPct                     + ";"
   WISC ->( netUnLock())
endif
return rV

funcDef petcLastPaym with isAnn:LU local rV
DEFAULT isAnn TO .F.
if rV := WISC ->( mAppend())
   WISC ->schet  := CONTRACT ->debetAcc
   WISC ->type   := "934"
   WISC ->buffer := CONTRACT ->intDoc          + ";" + ;
                    CONTRACT ->debetAcc        + ";" + ;
                    CONTRACT ->code2           + ";" + ;
                    CONTRACT ->code3           + ";" + ;
                    CONTRACT ->docNumb         + ";" + ;
                    if( isAnn, "1", "0")
   WISC ->( netUnLock())
endif
return rV

funcDef petcClose local rV, arr, nextD
   if rV := petcLastPaym()
      nextD := addMonth( setCurDate(), 1)
      // nextD := rMonth( nextD) + " " + sYear( nextD)
      contInitServ()
      arr := servDummy()

      aAdd( arr, { "@filial", confFilial()             })
      aAdd( arr, { "@otd",    "ПБО " + confOtdNumb()   })
      aAdd( arr, { "@fromD",  DtoC( CONTRACT ->dateCur)})
      aAdd( arr, { "@stopdate", firstDay( nextD)       })
      //printOpen( "petc2")
      printOpen( "serst")
      contCloseServ()
   endif
return rV

