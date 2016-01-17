******************************************************************************
*                         
*           Программа  : OTD_MON.EXE
*           Файл       : zOpenSch.prg
*           Автор      : Цейтлин П.М.
*
*           Назначение : Печать заявлений на открытие счета
*
*-----------------------------------------------------------------------------
* printOpen 30-09-2002 TMV
* При вычислении дат dat_open1 и dat_open2 получаемые значения уменьшены на 1 день
*
* getPayPrc 30-09-2002 TMV
*
* getPayPeriod 30-09-2002 TMV
*
* 29-11-2002 TMV
* Сделана принудит. первая заглавная буква в сумме прописью.
* Ф-ия getS()
*
* 03-12-2002 TMV
* При печати заявления, если оно не было подписано истоит признак работы
*  с зарезервированными счетами, в качестве даты открытия счета ставится
*  текущая дата - ф-ия printOpen().
*
* 27-02-2003  TMV
* Печать ордеров надомников для Владика переделана - ордера (поручения)
*  печатаются на одном листе с заявлением на открытие счета.
*  Ф-ии printOpen()
*
* 11-03-2003  TMV
* По параметру отключается печать док-та. Ф-ия printOpen()
*
* 30-10-2003  TMV
* Изменены шаблоны для печати договора о срочном вкладе, используемые
* в ф-ия findPos().
*

#include "clippExt.ch"
#include "common.ch"
#include "mon.ch"

#define BEG_SYMBOL "@"
#define BEG_DATE CtoD( "15.06.2001")
#define STR_FROM_FILES "GSZDG,GS12B"

#define DEBUG_PRINT_FILE ".\debug.prn"

static aFileName := ;
             {{ "15 1  5  1  ", "ktc"  },;  // клиент текущий       счет
              { "15 1  7  1  ", "ktc"  },;  // клиент текущий-нерез.счет
              { "15 1  1  1  ", "ptc"  },;  // текущий пенсионный   счет
              { "15 1  1  2  ", "ptc"  },;  // текущий пенсионный   счет
              { "15 4  1  0  ", "pkc"  },;  // карточный пенсионный счет
              { "15 4  4  1  ", "kkc"  },;  // карточный счет нерезидента
              { "15 4  2  0  ", "kkc"  }}   // карточный клиентский счет

  
funcDef printOpen with par2:CU, cnt:NU, showAlways:LU,;
	isNeedPrint:LU, sPrintText:CU;   // Add by TMV  27-02-2003
   local i, str, fileName, arr, aPasport, cPriz, cPL, cPR, cKL, cSchet711 ,;
         cSchet718, dat_open, nPR, d2, pos, try, dat_close     ,;
         strSumma, rV := .F., str2 := "", unitName, unitName1, ptr,strSkip,;
         str105Sum, aDoc, addr, j, vSumma, filOtd, otdFil, srok, type := 1,;
         lSeek, foundCr,;
         isWorkAccReserv := workAccReserv(), rV1 := 1


  DEFAULT cnt        TO 2
  DEFAULT showAlways TO .F.

  DEFAULT isNeedPrint TO .T.
  DEFAULT sPrintText TO " "

  lSeek = empty( par2)

  aPasport  := { cliPasSer(), cliPasNum(), cliPasGiv()}
  strSumma  = getS( SRSumma())
  if accIsNevsk( ACC ->PRIZ) .AND. confUseDogovor()
    strSumma := strTran( strSumma, ".", "")
  endif

  if len( strSumma) > 75
    strSumma := razbivka( strSumma, {{ 75, "L"}, { 75, "L"}})
  endif

  str105Sum := getS( SRSumma() * 1.05)

  cPriz     := ACC ->PRIZ

  dat_open  := if( isWorkAccReserv .And. !accIsSigned(), setCurDate(), ACC ->DATOPEN)

  d2        = setCurDate()
  nPR       = Str( vidGetPrc( cPriz), 7, 2)
  dat_close = vidGetDatClose( dat_Open, cPriz)
  dat_Close--

  filOtd    = rTrim( confFilial()) + " " + rTrim( confOtdName())
  otdFil    = rTrim( confOtdName())+ "," + rTrim( confFilial()) + ' ' + confBankName()

  cSchet711 := makeSchet711( lSeek, ACC ->schet)
   
  if usePctsBuffer()
     if !empty( ACC ->begPct)
        nPR = Str( val( left( ACC ->begPct, 5)), 7, 2)
     endif
  endif
  nPR = lTrim( nPR)
  if right( nPR, 2) == "00"
    nPR = left( nPR, len( nPR) - 3)
  endif

  aDoc := razbivka( cliDocum(), {{ 40, "L"}, { 40, "L"}})
  addr := razbivka( cliFullAddress(), {{ 44, "L"}, { 44, "L"}})
  unitName  = rTrim( confPostName()) //rTrim( confNodeName()) // Repl by TMV 17-10-2001
  unitName1 = left( unitName, len( unitName) - 2) + "ого"

  arr = {{ "@atrname",  confFilial()   },;
        { "@schet_type", accSchetType( ACC ->PRIZ)},;
        { "@filial",   confFilial()   },;
        { "@otdnumb",  confOtdNumb()  },;
        { "@otdname",  confOtdName()  },;
        { "@filialotd",filotd         },;
        { "@otdfil",   otdfil         },;
        { "@bankname", confBankName() },;
        { "@unitName", unitName       },;
        { "@unitNam1", unitName1      },;
        { "@f_name",   cliSurname()   },;
        { "@name",     cliName()      },;
        { "@s_name",   cliSName()     },;
        { "@fio",      cliFullName()  },;
        { "@fiovzn",   cliFullName()  },;
        { "@doc",      cliDocum()     },;
        { "@doc1",     aDoc[ 1]       },;
        { "@doc2",     aDoc[ 2]       },;
        { "@pas_ser1", aPasport[ 1]   },;
        { "@pas_ser2", ""             },;
        { "@pas_num",  aPasport[ 2]   },;
        { "@pas_mil",  aPasport[ 3]   },;
        { "@index",    cliIndex()     },;
        { "@sity",     cliCity()      },;
        { "@str_",     ""             },;
        { "@str_name", cliStreet()    },;
        { "@house",    cliHouse()     },;
        { "@blok",     cliBlock()     },;
        { "@flat",     cliFlat()      },;
        { "@tel",      cliPhone()     },;
        { "@adres",   cliFullAddress()},;
        { "@addr1",    addr[ 1]       },;
        { "@addr2",    addr[ 2]       },;
        { "@schet711", cSchet711      },;
        { "@d1",       setCurDate()+1 },;
        { "@d2",       addMonth( setCurDate(), 1)},;
        { "@dat_open", dat_open       },;
        { "@dat_open1",setCurDate()+13},;  // Edit by TMV  30-09-2001
        { "@dat_open2",setCurDate()+14},;  // Числа уменьшены на 1 (было 14 и 15)
        ;
        { "@pr",       nPR            },;
        { "@curdate",  setCurDate()   },;
        { "@currdate", rDate( setCurDate())   },;
        { "@dat_close",dat_close      },;
        { "@dat_voz",  dat_close + 1  },;
        { "@summa",    strSumma       },;
        { "@sum105",   lTrim(str105Sum)        },;
        { "@payprc",   getPayPrc( ACC ->schet, nPR) },;
        { "@paySprc",  getPayPeriod( ACC ->schet, nPR)},;
        { "@payNprc",  getPayPrc( ACC ->schet, nPR)},;
        { "@priz",     vidGetName( cPriz)      },;
        { "@sumDesc",  getSumDesc(ACC ->schet) },;
        { "@docNumb",  getDocNumb()            },;
        { "@otd_address", getOtdAddress()      },;
        { "@from_bank",   getFromBank( lSeek)  },;
        { "@dat_prc",     dat_open + 101       },;
        { "@dop_pr",      0.1                  },;
        { "@podval",      getPodval()          },;
        { "@header",      getHeader()          },;
        { "@zenitpromdate", ACC ->datOpen + 100},;
        { "@zenitsumma"   , getS( srSumma() / 365 * 149/ 100) },;
        { "@srok", showSrok( ACC ->PRIZ)       },;
        { "@dopstr",        makeDopStr()       }}

  if isKindDeal() .AND. lSeek
     vSumma := getSKindDeal( SRSumma(), dat_open, dat_close - 1)
     aAdd( arr, { "@sum_pelican", getS( vSumma)})
  endif

  if len( servDummy()) > 0
     for i := 1 to len( servDummy())
         pos = aScan( arr, {|x| x[ 1] == servDummy()[ i, 1]})
         if pos > 0
            arr[ pos, 2] := servDummy()[ i, 2]
         else
            aAdd( arr, { servDummy()[ i, 1], servDummy()[ i, 2]})
         endif
     next
  else
     aAdd( arr, { "@ordInc",     ordIn() })
     aAdd( arr, { "@ordOut",     ordOut()})
  endif

  if upper(par2) == 'CM3' then aAdd( arr, {'@dateDoc', CONTRACT->datecur})

  aFileName = readPrintZ( @aFileName)
  pos = findPos( @aFileName, lSeek, par2)

  getSavedOpen( "")   // найдем строку

  if pos >0
    str = extractStrOpen( aFileName[ pos, 2])
    str = makeZ( str, arr)
    if confShowIt() == 2 .OR. showAlways then str = editMemo( str, .T.)
    if mustSaveOpen() then getSavedOpen( str)
	if isNeedPrint         // Add by TMV  27-02-2003
        	doPrint( str, cnt )
	else                   // Add by TMV  27-02-2003
		sPrintText = str    // Add by TMV  27-02-2003
	endif                  // Add by TMV  27-02-2003
        rV = .T.
  else
        rV = .F.
  endif

return rV
//
static funcDef findPos with aFileName:A, lSeek:L, par2:CU ;
  local pos := 0, cPriz := ACC ->PRIZ, foundCr := .F., useDogovor, printType, ;
     arrName, arrCode

  if lSeek
     useDogovor := confUseDogovor() .AND. !confUseAgent()
     printType  := vidGetPrintType( ACC ->PRIZ)

     arrName := {}; arrCode := {}
     vidArray( cPriz, @arrName, @arrCode) 

     if len( arrCode) == 0
        par2 := ""
     else
        if useDogovor
           par2 := upper( allTrim( subStr( arrCode[ 1][ 8], 16, 10)))
        else
           par2 := upper( allTrim( subStr( arrCode[ 1][ 8], 6, 10)))
        endif
     endif
     if len( alltrim( par2)) == 0
        do case
          case printType == "1"
            lSeek := .F.; par2 := if( useDogovor, "ag_comm2", "ag_comm")
          case printType == "2"
            lSeek := .F.; par2 := if( useDogovor, "agna2", "agnak")
          case printType == "3"
            lSeek := .F.; par2 := if( useDogovor, "aglo2", "aglon")
          case printType == "4"
            lSeek := .F.; par2 := if( useDogovor, "ag1y2", "ag1y")
          case printType == "5"
            lSeek := .F.; par2 := if( useDogovor, "agcr2", "agcr")
        endcase
     else
        lSeek := .F.
     endif
  endif

  if lSeek
     if isSR()
        if !hasOwnProp( aFileName, cPriz)
           cPriz := "cr"; foundCr := .T.
        endif
     endif
     if foundCr 
        pos := aScan( aFileName, {|x| lower( x[ 2]) == lower( cPriz) })
     else
        pos := aScan( aFileName, {|x| lower( x[ 1]) == lower( cPriz) })
     endif
  else
     cPriz := lower( par2)
     pos := aScan( aFileName, {|x| lower( x[ 2] ) == lower( cPriz)}) 
  endif

  if pos = 0 .AND. len( alltrim( par2)) <> 0
     messageB( "Шаблон для печати с именем: " + par2 + " не найден!")
  endif

return pos
//
funcDef doPrint with cStr:C, nCopies:N local cSkip, i, rV, nPos, nTry
  nPos := 0
  nTry := 0
  do while .T.
    cSkip := endl() + replicate( "-", 70 ) + endl()
    if isDebug()
      for i := 1 to nCopies
        eFileWrite( cStr,  DEBUG_PRINT_FILE )
        eFileWrite( cSkip, DEBUG_PRINT_FILE )
      next
      rV := .T.
      exit
    endIf
    if if( confPrintDon() .or. confPrintUsel(), .T., checkPrinter())
      if confPrintDon()
        if file( getDonName()) then fErase( getDonName())
        eFileWrite( "&4", getDonName())
        for i := 1 to nCopies - 1
          eFileWrite( cStr, getDonName())
          eFileWrite( chr( 12) + endl(), getDonName())
        next
        eFileWrite( cStr, getDonName())
        eFileWrite( endl(), getDonName())
        runExe( getDonExeName())
      else
        if confPrintUsel() 
          for i := 1 to nCopies do printAOrder( MikeSToA( cStr ))
        else
          set device  to print
          set printer to
          for i := 1 to nCopies
            if printAsk() .and. confUseDogovor()
              set device to screen
              OK( "Вставьте лист бумаги в принтер" )
              set device  to print
              set printer to
            endIf
            initEpson()
            devOut( cStr  )
            devOut( cSkip )
          next
          set device to screen
        endIf
      endIf
      rV := .T.
      exit
    else
      errOK( "Принтер не готов")
    endIf
    nPos++
    exit if nPos > 3
  endDo
return rV

static func initEpson
local str := chr( 12) + chr( 27) + chr( 57) + chr( 27) + chr( 99) + ;
             chr( 48) + chr(  4) + chr( 27) + chr( 99) + chr( 52) + ;
             chr( 16)
if bankPost() = 2
   DevOut( str)
endif
return nil

func printZInfo()
local dbInfo := { ".\printZ.dbf", "printZ",;
                {{ "name", "C",   15, 0},;
                 { "data", "C", 4096, 0}},;
                 {}, 0}
return dbInfo

funcDef extractStrOpen with sSearch:C ;
   local str := "", tmp, s := select(), dirSpec, fileName

if confUseTPL()
   fileName := newFPath( ".\printz.tpl", confPathOD())
   str := ReadDocument( fileName, sSearch)
   return str
endif

tmp := printZInfo()
basePath( tmp, confPathOD())

dirSpec := Directory( tmp[ 1])
if len( dirSpec) == 1
   if dirSpec[ 1, 3] < BEG_DATE
      messageB( "У Вас старый файл шаблонов ( PRINTZ ). Обратитесь на филиал !!!")
   endif
endif
if upper( sSearch) $ STR_FROM_FILES
  dirSpec := newFPath( sSearch, confPathOD())
  str = MemoRead( dirSpec)
else
  if baseNSOpen( tmp)
    locate for allTrim( lower( printZ ->name)) == allTrim( lower( sSearch))
    if printZ ->( found())
      str = printZ ->data
    endif
    baseMClose( tmp)
    select ( s)
  endif
endif
return str

//
// На входе строка с заявлением на выходе
// заявление
//
funcDef makeZ with str:CM, aArr:A local aStr, i, pos
//
// перешел к массиву
//
aStr := ctoA( str, endl())
if confUseTPL()
 ident2Arr( @aStr, @aArr)
endif
//
// в каждой строке массива ищем символ начала кода
//
for i := 1 to len( aStr)

    while  BEG_SYMBOL $ aStr[ i]
       aStr[ i] = removeReference( aStr[ i], @aArr)  // Add <@> by TMV  12-03-2003
    enddo

next

str := atoC( aStr)

return str

funcDef alignValue with cValue:CDN, nLen:N, cAlign:C, cFill:C local rV
  do case
    case cAlign == "L"
      rV = padL( cValue, nLen, cFill )
    case cAlign == "C"  
      rV = padC( cValue, nLen, cFill )
    otherwise
      rV = padR( cValue, nLen, cFill )
  end case
return rV

//
// удаление из строки всех кодовых слов
//
// Replaced <aArr> to <aArrId> by TMV  12-03-2003
funcDef removeReference with str:C, aArrId:A ;
                        local i, pos, ch, ident, res, ident2, j, str2,;
                              nLen, cAlign, cFill

while ( pos := at( BEG_SYMBOL, str)) >0
      ident = BEG_SYMBOL
      pos++
      while .T.
            ch = subStr( str, pos, 1)
            //
            // если это символ, допустимый в идентификаторе,
            // то добавим его в ident
            //
            if ( ch >= "A" .AND. ch <= "Z") .OR. ;
               ( ch >= "a" .AND. ch <= "z") .OR. ;
               ( ch >= "0" .AND. ch <= "9") .OR. ;
               ch = "_" .OR. ch = "." .OR. ch = "*"

               ident += ch
               pos++
            else
               //
               // найдем в массиве aArrId идентификатор и замением его на
               // значение и выходим из цикла поиска тек. идентификатора
               //
               ident2 = strTran( ident, ".", "")
               ident2 = strTran( ident2,"*", "")
               i = aScan( aArrId, {|x| x[ 1] == ident2})
               res = space( len( ident))
               if i >0
                  nLen = len( aArrId[ i ]) 
                  if ( nLen > 2  ) .and. !empty( aArrId[ i ][ 3 ])
                    cAlign = aArrId[ i ][ 3 ]
                  else
                    cAlign = "R"
                  endIf
                  if ( nLen > 3 ) .and. !empty( aArrId[ i ][ 4 ])
                    cFill = aArrId[ i ][ 4 ]
                  else
                    cFill = " "
                  endIf
                  if !isArray( aArrId[ i, 2])
                     res = alignValue( aArrId[ i ][ 2 ], len( ident ), cAlign, cFill )
                     str = strTran( str, ident, res, 1, 1)
                   else
                     // str2 := str
                     str = ""
                     for j := 1 to len( aArrId[ i, 2])
                         res = alignValue( aArrId[ i ][ 2 ][ j ], len( ident ), cAlign, cFill )
                         res = rTrim( res)
                         str += res + if( j < len( aArrId[ i, 2]), endl(), "")
                     next
                  endif
               endif
               str = strTran( str, ident, res, 1, 1)
               exit
            endif
      enddo
enddo
return str
//
funcDef getPayPrc with schet:C, nPR:C local rV, recNo, index, char, sType
   if ACC ->schet <> schet
      recNo := ACC ->( recNo())
      index := ACC ->( indexOrd())
      ACC ->( dbSetOrder( 1))
      ACC ->( dbSeek( schet, .F.))
   endif
   char := subStr( ACC ->buffer, 6, 1)
   if char == "Y"
      rV := "по окончании года."
   else
     //if accIsCard( schet) // .AND. !confUseAgent()
     //  rV := "ежемесячно не позднее 5 числа месяца, следующего за отчетным."
     //else
   rV := "ежемесячно по ставке " + nPR + "% годовых."
   endif
   if !isNIL( recNo)
      ACC ->( dbSetOrder( index))
      ACC ->( dbGoTo( recNo))
   endif
return rV
//
//  Add by TMV  11-10-2001
//
funcDef getPayPeriod with schet:C, nPR:C local rV, recNo, index, char, sType
   if ACC ->schet <> schet
      recNo := ACC ->( recNo())
      index := ACC ->( indexOrd())
      ACC ->( dbSetOrder( 1))
      ACC ->( dbSeek( schet, .F.))
   endif
   char := subStr( ACC ->buffer, 6, 1)
   if char == "Y"
      rV := "по окончании года."
   else
   rV := "ежемесячно по ставке " + nPR + "% годовых."
   endif
   if !isNIL( recNo)
      ACC ->( dbSetOrder( index))
      ACC ->( dbGoTo( recNo))
   endif
return rV

funcDef getSumDesc ;
  with schet:C, desc2:CU local arr := {}, arrTran, foundKas, foundNotKas, Str

desc2 := ""
foundKas := foundNotKas := ""
SUMS ->( baseSavePos())
SUMS ->( dbSetOrder( 1))
if SUMS ->( dbSeek( schet, .T.))
   // SUMS ->( dbSkip())
   if SUMS ->schet == schet
      provGetInfo( "08", "65",,,, @arrTran)
      aAdd( arr, "из них:")
      while SUMS ->schet == schet
        Str := "  в сумме:" + getS( SUMS ->summa/ 100)
        if len( Str) > 75
          Str := razbivka( Str, {{ 75, "L"}, { 75, "L"}})
          aAdd( arr, str[ 1]); aAdd( arr, str[ 2])
        else
          aAdd( arr, Str)
        endif
        if kasTypes( SUMS ->typeOf)
           aAdd( arr, "наличными квитанция: N " + nStr( SUMS ->nDoc) + " от "+;
             DtoC( SUMS ->dDate))
           foundKas := "наличными"
        else
           aAdd( arr, "перечислением со счета N " + getSchet2( arrTran) + ;
             " ордер N " + nStr( SUMS ->nDoc) + " от " + DtoC( SUMS ->dDate))
           foundNotKas := "безналичным перечислением"
        endif
        skip 1 alias SUMS
      enddo
      desc2 := foundKas
      if !empty( foundKas) .AND. !empty( foundNotKas)
         desc2 += " и " + foundNotKas
      else
         if !empty( foundNotKas) then desc2 := foundNotKas
      endif
   endif
else
   arr := { ""}
endif
SUMS ->( baseRestPos())
return arr

funcDef addDesc2 with desc2:C, newDesc2:C
if !( newDesc2 $ desc2 )
   if empty( desc2)
      desc2 := newDesc2
   else
      desc2 += " и " + newDesc2
   endif
endif
return desc2

autofunction setValue SRSumma init 0

autofunction setValue printAskBefore init .T.
autofunction setValue setDocnumb init "000000"

func bankPost(); return 1

procDef protView with fileName:C local str, mode
   //
   // Просмотр отчета
   //
   removeClock()
   editMemo( memoRead( fileName ), .F. )
   if yesNo( "Печатать документ?" ) then clPrintFile( fileName )
   installClock()
return

static funcDef getDocNumb local rV := ""
if confUseDogovor()
   rV := tmpGet( "DOC_NUMB")
   rV := val( rV) + 1
   tmpSet( "DOC_NUMB", nStr( rV))
endif
return padL( rV, 5, "0")

static funcDef getOtdAddress local rV := ""
   rV := tempGetComment( "ADDR")
return rV

funcDef isRozha
return ACC ->PRIZ == "15 2  46 0  "

static funcDef getFromBank with lSeek:LU local rV := ""
DEFAULT lSeek TO .T.
if lSeek
  if confUseAgent() then return rV
     if confUseDogovor()
       rV := agentSelect()
       if empty( rV) then break
       rV := "ОТ БАНКА: " + rV
       if len( rV) > 75 then rV := razbivka( rV, {{ 75, "L"}, { 75, "L"}})
     endif
endif
return rV

autofunction setValue mustSaveOpen init .F.
autofunction setValue getSavedOpen init ""

funcDef getS with tmpS:N ;
local sTmp := rSumma( tmpS)                       // Add by TMV  29-11-2002

sTmp = Upper( Left(sTmp, 1) ) + SubStr( sTmp, 2 ) // Add by TMV  29-11-2002
return lTrim( strTran( mStr( tmpS), ".", "") ) + "("  + sTmp + ")"

static funcDef getLastDay with d1:D local rV
   rV := CtoD( "31/12/" + nStr( Year( d1)))
return rV

static funcDef getCntDays with Year:N local rV := 365
  if ( Year % 4 ) = 0
     rV += 1
  endif
return rV

funcDef getSKindDeal with Sum:N, d1:D, d2:D local rV := 0, d3 
d3 := getLastDay( d1)
while .T.
  if d3 < d2
     rV += Sum * ( d3 - d1)/ getCntDays( Year( d3))/ 100
  else
     rV += Sum * ( d2 - d1)/ getCntDays( Year( d2))/ 100
     exit
  endif
  d1 := d3
  d3 := getLastDay( d3 + 1)
enddo
   
return rV

funcDef getPodval local arr, curPlace := val( setUpValue( "PLACE"))
 do case
    case curPlace == 1
      arr := { "",;
               "                             Подпись клиента(пенсионера)",;
               "                             (является образцом подписи) _____________________",;
               "",;
               "                 Расшифровка фамилии клиента(пенсионера) _____________________",;
               "" }
    case curPlace == 2 .OR. curPlace == 0
      arr := { "",;
               "Клиент:              _______________________   ___________________________ ",;
               "                            подпись                 фамилия  ", "" }

 endcase
return arr

funcDef getHeader local arr, curPlace := val( setUpValue( "PLACE"))
do case
   case curPlace == 1
      arr := { "@curdate************************                 В " + confNodeName()    ,;
               "                                            районный узел почтовой связи",;
               "                                            Ленинградской области       ",;
               "",;
               "                                                 В @atrname******************",;
               '                                                 ' + confBankName()}
   case curPlace == 2 .OR. curPlace == 0
     arr := { "Дата @curdate**            В @bankname********************",;
              "                           @atrname***********************"}
endcase   

return arr

func eStr2( par1, par2, par3)
local str
  if valType( par1) == "C" then par1 := val( par1)
  str := eStr( par1, par2, par3)
  str := strTran( str, ".", "-")
return str

static funcDef makeSchet711 with lSeek:L, cSchet711:C 

if lSeek 
 if confUseAgent()
   //
   // пока ничего не делаем
 else
   //
   // временная ставка
   if confUseDogovor()
     cSchet711 := SubStr( cSchet711, 1, 5) + " " + ;
                  SubStr( cSchet711, 6, 3) + " " + ;
                  SubStr( cSchet711, 9, 5) + " " + ;
                  SubStr( cSchet711,14, 3) + " " + ;
                  SubStr( cSchet711,17, 4)               

   endif
 endif
endif   
return cSchet711

static funcDef isLongerYear local rV := .F., cnt := 0, srok
  srok := vidGetSrok( ACC ->PRIZ, @cnt)
  rV := ( cnt = 0 .AND. srok >= 365) .OR. ( cnt = 1 .AND. srok >= 12)
return rV

//
static funcDef hasOwnProp with arr:A, priz:C local rV := .F.
  rV := aScan( arr, {|x| x[ 1] == priz})
return rV > 0

static function readPrintZ( arr)
  static isRead := .F.
  local s := select(), info := printZInfo()
  if !isRead
    basePath( info, confPathOD())
    if isRead := baseNSOpen( info)
       while PRINTZ ->( !eof())
         if aScan( arr, {|x| upper( x[ 2]) == upper( rTrim( PRINTZ ->name))})=0
           aAdd( arr, { "", upper( rTrim( PRINTZ ->name))} )
         endif
         skip 1 alias PRINTZ
       enddo
       baseMClose( info)
    endif
    select (s)
  endif
return arr

funcDef showSrok with priz:C ;
 local srok := 0, cnt := 0, str := "", arDays, arM, lastX
 srok := vidGetSrok( priz, @cnt)
 arDays := {{ 0, "дней"}, { 1, "день"}, { 2, "дня" }, { 3, "дня" },;
            { 4, "дня" }, { 5, "дней"}, { 6, "дней"}, { 7, "дней"},;
            { 8, "дней"}, { 9, "дней"}}
 arM    := {{ 0, "месяцев"}, { 1, "месяц"}, { 2, "месяца"},;
            { 3, "месяца" }, { 4, "месяца"}}
 if srok > 0
   do case
      case cnt == 0 // дни
        lastX := val( right( nStr( srok), 1))
        cnt   := aScan( arDays, {|x| x[ 1] == lastX})
        str   := nStr( srok) + " " + arDays[ cnt, 2]
      case cnt == 1 // месяцы
        cnt := aScan( arM, {|x| x[ 1] == srok})
        str := nStr( srok) + " " 
        if cnt > 0
          str += arM[ cnt, 2]
        else
          str += "месяцев"
        endif
      otherwise
   endcase
 endif
return str


funcDef makeDopStr local str := { "" }
   do case
      case isZenit2001()
        str := { "", ;
           'Прошу по окончании срока вклада перечислить со счета вклада на счет ФК "Зенит"',;
           "средства в сумме                                                              ",;
           "@zenitsumma*********************************************************************",;
           'на осуществление благотворительной программы "Резерв".', ""}
      case isNak()
        str :={ '  С Положением о срочном вкладе ' + ;
               if( ACC ->PRIZ == "15 2  42 0  ", '"Пенсионный накопительный"',;
                   '"Накопительный"') + ' @bankname***********************',;
               'ознакомлен и согласен.' }
      case isKindDeal()
        str := { "",;
                 'Прошу по окончании срока вклада перечислить  со счета вклада на счет ',;
                 'Благотворительного Движения "Золотой Пеликан" средства в сумме       ',;
                 '@sum_pelican*************************************************',;
                 'на осуществление благотворительной программы "Социальный автобус".   ',;
                 ""}
   endcase
return str


function ReadDocument( fileName, docN)
local str := "", nHandle, fSize, bufLen := 10000, toRead := 0, sResult := ""
local ptr, delimLeft := "[", delimRight := "]"
local docName := upper( delimLeft + docN + delimRight)
if !file( fileName) 
  return ""
endif

nHandle := fOpen( fileName)
if nHandle > - 1
   fSize := fSeek( nHandle, 0, 2)
   fSeek( nHandle, 0, 0)
   while toRead < fSize
     str := fReadStr( nHandle, bufLen)
     toRead += len( str)
     ptr := at( docName, str)
     if ptr > 0
       ptr := fSeek( nHandle, -( len( str) - ptr ) + len( docName) + 1, 1) 
       str := fReadStr( nHandle, bufLen)
       ptr := at( delimLeft, str)
       if ptr > 0
         sResult := subStr( str, 1, ptr - len( delimLeft))
       endif     
       exit
     endif
   enddo
   fClose( nHandle)
endif
eFileWrite( "++++++++++++++++++++++++++++++++++++++++++++++++", ".\result.txt")
eFileWrite( sResult, ".\result.txt")
eFileWrite( "++++++++++++++++++++++++++++++++++++++++++++++++", ".\result.txt")
return sResult


funcDef ident2Arr with aDoc:A, arr:A ;
   local i := 1, curIdent, curValue, pos
while i <= len( aDoc)
  if left( aDoc[ i], 1) = "#" 
     pos := at( ":=", aDoc[ i])
     if pos > 0
       curIdent := subStr( aDoc[ i], 2,  pos - 1)
       curValue := subStr( aDoc[ i], pos + 1 + 2)
       aAdd( arr, { "@" + allTrim( curIdent), &curValue})
       aDel( aDoc, i); aSize( aDoc, len( aDoc) - 1)
     else
       i++
     endif
  else
    i++
  endif
enddo

return NIL

//
// Печать расписки на получение льготного проездного билета
//
funcDef printOpenCol with par2:CU, cnt:NU, showAlways:LU,;
	isNeedPrint:LU, sPrintText:CU;   // Add by TMV  27-02-2003
   local i, str, fileName, arr, aPasport, cPriz, cPL, cPR, cKL, cSchet711 ,;
         cSchet718, dat_open, nPR, d2, pos, try, dat_close     ,;
         strSumma, rV := .F., str2 := "", unitName, unitName1, ptr,strSkip,;
         str105Sum, aDoc, addr, j, vSumma, filOtd, otdFil, srok, type := 1,;
         lSeek, foundCr,;
		  isWorkAccReserv := workAccReserv()  // Add by TMV  03-12-2002

DEFAULT cnt        TO 2
DEFAULT showAlways TO .F.

DEFAULT isNeedPrint TO .T.  // Add by TMV  27-02-2003
DEFAULT sPrintText TO " "  // Add by TMV  27-02-2003

lSeek = empty( par2)

cPriz     := ACC ->PRIZ

dat_open  := if( isWorkAccReserv .And. !accIsSigned(), setCurDate(), ACC ->DATOPEN)

aDoc := razbivka( cliDocum(), {{ 40, "L"}, { 40, "L"}})
addr := razbivka( cliFullAddress(), {{ 44, "L"}, { 44, "L"}})
unitName  = rTrim( confPostName())
unitName1 = left( unitName, len( unitName) - 2) + "ого"

arr = {{ "@filial",   confFilial()   },;
        { "@bankname", confBankName() },;
        { "@fio",      cliFullName()  },;
        { "@doc1",     aDoc[ 1]       },;
        { "@doc2",     aDoc[ 2]       },;
        { "@addr1",    addr[ 1]       },;
        { "@addr2",    addr[ 2]       },;
        { "@tel",      cliPhone()     },;
        { "@curdate",  setCurDate()   }}

if len( servDummy()) > 0
   for i := 1 to len( servDummy())
       pos = aScan( arr, {|x| x[ 1] == servDummy()[ i, 1]})
       if pos > 0
          arr[ pos, 2] := servDummy()[ i, 2]
       else
          aAdd( arr, { servDummy()[ i, 1], servDummy()[ i, 2]})
       endif
   next
endif

aFileName = readPrintZ( @aFileName)
pos = findPos( @aFileName, lSeek, par2)
//
// найдем строку
//
getSavedOpen( "")

if pos >0
   str = extractStrOpen( aFileName[ pos, 2])
   str = makeZ( str, arr)

   if confShowIt() == 2 .OR. showAlways then str = editMemo( str, .T.)

   if mustSaveOpen() then getSavedOpen( str)

	if isNeedPrint         // Add by TMV  27-02-2003
       	     doPrint( str, cnt )
	else                   // Add by TMV  27-02-2003
	     sPrintText = str    // Add by TMV  27-02-2003
	endif                  // Add by TMV  27-02-2003

   rV = .T.

endif

return rV
