*============================================================================
*                                                                              
*                  Программа : MON.EXE
*                  Файл      : kassoper.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: Операции по кассе
*
*----------------------------------------------------------------------------
//03-08-98  в РКО поменяны местами кор. счет и счет кассы
*
* 25-11-2002 TMV
* При печати кассового журнала введено разделение - для Владика и нет
* Ф-ия printJournal()
*
* 09-12-2002 TMV
* Введены операции 6031, 6136, 6137
*  ф-ии getSchet2(), downJournal()
*
* 17-12-2002 TMV
* Заменена обработка операций 6136 и 6137 - на 7033.
* Ф-ия getSchet2()
*--ОТМЕНЕНО
*
* 18-12-2002 TMV
* Изменен реестр прихода/расхода - makeJournal()
*
* 12-08-2003 TMV
* При формировании реестров есть можно отметить/разотметить записи со
* счетами 42309 или 42301.
* Ф-ии setJrnOnAll() и setJrnOffAll().
*
* 25-08-2003  TMV
* В Кассовом журнале сделана печать суммы ИТОГО прописью с большой буквы.
* Ф-ия printJournal()
*
* 06-05-2004  TMV
* Для кассовых операций по карточкам изменен счет, для чего в SETUP введен
*  новый признак <POS>.
* Ф-ии kasInc(), kasDec(), kas1027(), kas1009()
*
* 29-07-2004 TMV
* Изменение карточных счетов на 45509
* Ф-ии _isCard(), _isTek().
*

#include "common.ch"
#include "inkey.ch"
#include "clippExt.ch"
#include "gsz.ch"
#include "mon.ch"

* NEW OBJECT FILE
 
funcDef kasGetKorSchet with ptrNazn:N, mustAsk:LU local credSchet

DEFAULT mustAsk TO .T.
do case
   case ptrNazn == 1 
        credSchet := confKorSchet()
   case ptrNazn == 2
        credSchet := confNDACC()
        if empty( credSchet) then credSchet := confKorSchet()
   case ptrNazn == 3
        credSchet := setUpValue( "PKAS")
        if empty( credSchet) then credSchet := confKorSchet()
   case ptrNazn == 4
        // credSchet := setUpValue( "CASHM")
        if mustAsk then credSchet := getCashMSchet()
        if empty( credSchet) then credSchet := confKorSchet()
   case ptrNazn == 5
        credSchet := setUpValue( "DEN")
   case ptrNazn == 6
        credSchet := setUpValue( "KASS+")
   case ptrNazn == 7
        credSchet := setUpValue( "CASH-")
   case ptrNazn == 8
        credSchet := setUpValue( "CASH+")
   case ptrNazn == 9
   otherwise
        messageB( "Неверный параметр при определении кор.счета !")
        break
endcase
return credSchet


funcDef kasInc with isOrg:LU, sum1:NU;
   local summa := 0.00, get, ro, sim, theCode := "3001", n := 1, arr  ,;
         arr2 := { }, i, nK := 1, aOrder, fullName, pasSer, j         ,;
         theOrder, aReceipt34b, aNazn, aCode, ptrNazn := 1,credSchet  ,;
         bankSchet, balSchet, toKassa, curAdm, admArr, admArr2, admPtr,;
         curReci, recNazn, curNazn


aNazn := { "Подкрепление операционной кассы",;
           "Подкрепление от ОС    ",;
           "Возврат из банкомата  ",;
           "Излишки в кассе ПБО"   ,;
           "Излишки в банкомате"   ,;
           "Подкрепление из БО "   ,;
           "Прием наличных по пластиковым картам" }
aCode := { opDecode( "3001"), opDecode( "3007"), opDecode( "3011"),;
           opDecode( "3013"), opDecode( "3016"), opDecode( "3017"),;
           opDecode( "1027")}

if valType(isOrg) = "U"
  isOrg:=.F.
endif
if isOrg
  aadd(aNazn, "Прием от УС платежей юр.лиц")
  aadd(aCode, opDecode( "3009"))
  theCode:= opDecode( "3009")
  ptrNazn:=len(aNazn)
  summa:=sum1
endif

operGetInfo( theCode, @ro, @sim)

arr   := kasCashers()
for i := 1 to len( arr)
    aAdd( arr2, rTrim( arr[ i, 5]) + " " + rTrim( arr[ i, 6]) + " " + ;
                rTrim( arr[ i, 7]))
next
aAdd( arr, nil); aAdd( arr2, nil)
aIns( arr, 1);   aIns( arr2, 1)
arr[ 1] := Space( 10); arr2[ 1] := Space( 10)



//
// администраторы
//
admArr  := kasAdm()
admArr2 := {}
admPtr  := 1
if len( admArr) < 1
  messageB( "Введите данные администраторов ПБО !")
  return .F.
endif
for i := 1 to len( admArr) 
  aAdd( admArr2, rTrim( admArr[ i, 5]) + " " + rTrim( admArr[ i, 6]) + " " + ;
                rTrim( admArr[ i, 7]))
next


get := {{ "Подкрепление кассы/ от ОС"                                },;
        { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                   },;
        { "Счет кассы                 : " + splitSSchet( confKassa())},;
        { "Символ кассовой отчетности : ", {|| sim},, {||.F.}        },;
        { "Род операции               : ", {|| ro },, {||.F.}        },;
        { "Назначение платежа         : ", block( ptrNazn), aClone( aNazn),;
            {|g| !isOrg},;
            {|g| changeK1( g, @get, aNazn, aCode, 2, @credSchet, @ro  ,;
                                                               @sim)}},;
        { ""                                                         },;
        { "Сумма в рублях             : ", block( summa) , , {|| !isOrg}},;
        { "Дата проводки              : " + DtoC(setCurDate())       },;
        { ""                                                         },;
        { "Кассир                     : ", block( nK), aClone( arr2) },;
        { "Администратор              : ", block( admPtr), aClone( admArr2)   },;
        { "Копий                      : ", block( n)}}

if isOrg
  credSchet := confKorSchet()
else
  credSchet := kasGetKorSchet( ptrNazn)
  operGetInfo( theCode, @ro, @sim)
endif

if getUp( 5, 5, get)

   curAdm := admArr2[ admPtr]
   if !isOrg
     do case
      case ptrNazn == 6
           if empty( confBONumb())
             messageB( "Введите номер БО для покрепления кассы")
             return NIL
           endif
      case ptrNazn == 1 .AND. empty( setUpValue( "DEN"))
        messageB( "Для данной операции нет счета")
        return NIL
      case ptrNazn == 3 .AND. !chkCashMSchet()
        messageB( "Для данной операции нет счета")
        return NIL
      case ptrNazn == 4 .AND. empty( setUpValue( "KASS+"))
        messageB( "Для данной операции нет счета")
        return NIL
      case ptrNazn == 5 .AND. ;
           ( empty( setUpValue( "CASH+")) .OR. empty( setUpValue( "CASHM")))
        messageB( "Для данной операции нет счета")
        return NIL
// Replaced by TMV 06-05-2004
//      case ptrNazn == 7 .AND. empty( setUpValue( "SCARD"))
      case ptrNazn == 7 .AND. empty( setUpValue( "POS"))
        messageB( "Для данной операции нет счета")
        return NIL
     endcase
   endif
   theCode := aCode[ ptrNazn]
   operGetInfo( theCode, @ro, @sim, @curNazn)
   if yesNo( "Выполнять проводку на сумму " + cStr( summa) + " ?" )
     if isOrg
       fullName := rTrim( confPostName()) + " через нач-ка ОПС " + rTrim( curAdm)
     else
      credSchet := kasGetKorSchet( ptrNazn)
      do case
         case ptrNazn == 1
              fullName := confBankName()+ " " + rTrim( confFilial())
              credSchet:= confKor2Schet()
              if ptrNazn == 6 then fullName += ", БО " + nStr( confBONumb())
              if ptrNazn == 1 then credSchet := setUpValue( "DEN")
         case ptrNazn == 2
              fullName := rTrim( confPostName()) + " через " + rTrim( curAdm)
              // credSchet:= confKor2Schet()
         case ptrNazn == 7 
// Replaced by TMV 06-05-2004
//              credSchet := setUpValue( "SCARD")
              credSchet := setUpValue( "POS")
         case ptrNazn >= 3
              fullName := confBankName() + " " + rTrim( confFilial()) + ;
                          if( ptrNazn == 6, " БО " + nStr( confBONumb()), "") +;
                          " Через " + arr2[ nK]
              if ptrNazn == 6 
                credSchet:= confKor2Schet()
              endif
         //case ptrNazn > 3
         //     fullName := arr2[ nK]
         otherwise 
              fullName := "НЕТ ИНФОРМАЦИИ"
      endcase
     endif
     theOrder  := ordIncIn( setCurDate())
     //03-08-98

     if isOrg
        usePensOrg( theOrder, credSchet, fullName, arr2[ nK], curAdm, summa, sim, ro)
     else
      curReci  := rTrim( confBankName() + ", " + confFilial())
      if left( confKassa(), 5) <> "20202" then curReci += ", " + confOtdName()
      do case
         case ptrNazn == 1
           fullName := rTrim( confBankName() + " " + confFilial()) + " Через " + arr2[ nK]
           for i := 1 to n
             saveLastOrder( {}, .T.)
             aOrder := kasPKO2( theOrder, fullName, setCurDate(), confKassa(),;
                credSchet, summa, sim, ro, curReci, curNazn) // aNazn[ ptrNazn])
 
             printAOrder( aOrder)
             saveLastOrder( aOrder, .F.)
           next
         case ptrNazn == 2
              toKassa := rTrim(confFilial())
              toKassa += if( ptrNazn = 6, ' ' + confBankName()+ ' ', " ")
              toKassa += confOtdName()
              if ptrNazn == 2 then recNazn := "Подкрепление"
              for i := 1 to n
                saveLastOrder( {}, .T.)
                aReceipt34b := receipt34b( theOrder, setCurDate(),;
                        credSchet, summa, arr2[ nK], curAdm, recNazn)
                printAOrder( aReceipt34b)
                saveLastOrder( aReceipt34b, .F.)
                aOrder := kasPKO( theOrder, setCurDate(), fullName,;
                   credSchet, summa, toKassa,  curNazn ,; // aNazn[ ptrNazn]     
                   confKassa(), sim, .T.)
                 printAOrder( aOrder)
                 saveLastOrder( aOrder, .F.)
              next
         case ptrNazn >= 3 .AND. ptrNazn <> 7
              balSchet := confKassa()
              if aCode[ ptrNazn] $ "3013,3016"
                 if ptrNazn == 5
                    credSchet := setUpValue( "CASH+")
                 else
                    credSchet := setUpValue( "KASS+")
                 endif
              else
               if ptrNazn <> 6
                 bankSchet := getCashMSchet()
                 credSchet := bankSchet
                 if empty( credSchet) then return .F.
               endif
              endif
              for i := 1 to n
                  saveLastOrder( {}, .T.)
                  aOrder := kasPKO2( theOrder, fullName, setCurDate(),;
                            confKassa(), credSchet, summa, sim, ro   ,;
                            curReci, curNazn) 
                  printAOrder( aOrder)
                  saveLastOrder( aOrder, .F.)
              next
      endcase
     endif
      if !sumWrite( credSchet, summa, setCurDate(), theCode, theOrder)
         errLog( "Проводка не сделана ! Попробуйте еще раз")
      else
        return .T.
      endif
   endif
endif
return NIL


funcDef usePensOrg ;
   with theOrder:N, credSchet:C, fullName:C, kassir:C, curAdm:C, summa:N, sim:C,ro:C;
   local aOrder, i, n := 1, toKassa

toKassa := rTrim(confFilial())+" "+confOtdName()
for i := 1 to n
/*
  aOrder:={ }
  saveLastOrder( {}, .T.)
  aadd(aOrder,"                        КВИТАНЦИЯ N      -----------    -----------------------")
  aadd(aOrder,"         к приходному кассовому ордеру   | "+padl(theOrder,6,"0")+"  |    |       0402001       |")
  aadd(aOrder,"         "+padc(rDate(setCurDate()),32)+"-----------    -----------------------")
  aadd(aOrder,"")
  aadd(aOrder,"От кого "+padr(confPostName(),39)            +"Для зач. -----------------------")
  aadd(aOrder,"через нач-ка ОПС "+padr( curAdm,30)    +"на сч. N |"+credSchet+        "|")
  aadd(aOrder,"Банк получателя "+padR(confBankName()+","+confFilial(),40)+"-----------------------")
  aadd(aOrder,"Получатель "+padr(toKassa,45)                         +"|"+mstr(summa)+      "|")
  aadd(aOrder,"                                                        -----------------------")
  aadd(aOrder,"Ст.кассир "+ kassir)
  aadd(aOrder,"")
  aadd(aOrder,rSumma(summa))
  aadd(aOrder,"Сумма прописью")
  aadd(aOrder,"")
  aadd(aOrder,"Назначение платежа "+"Подкрепление от ОПС")//aNazn[ ptrNazn])
  aadd(aOrder,"")
  aadd(aOrder,"Деньги принял кассир")
  aadd(aOrder,"Штамп ПБО")
  aadd(aOrder,"")
  
  printAOrder( aOrder)
  saveLastOrder( aOrder, .F.)
  aOrder := kasPKO( theOrder, setCurDate(), fullName,;
     credSchet, summa, toKassa, "Подкрепление от ОПС",; //aNazn[ ptrNazn],;
     confKassa(), sim, .T.)
     printAOrder( aOrder)
     saveLastOrder( aOrder, .F.)
*/
  useZabota( .T.)
  aOrder := vznos_nal( theOrder, setCurDate(), fullName, credSchet, summa,;
     toKassa, "Подкрепление от ОПС", confKassa(), sim, ro,,.T.)
  useZabota( .F.)
  printAOrder( aOrder)
  saveLastOrder( aOrder, .F.)

next
return NIL



funcDef chkCashMSchet local i, bankSchet := {}, curSchet
  for i := 0 to 99
    curSchet := setUpValue( "CAS" + padL( nStr( i ), 2, '0' ))
    if !empty( curSchet ) then aAdd( bankSchet, curSchet )
  next
  if empty( bankSchet )
    for i := 0 to 9
      curSchet := setUpValue( "CASH" + nStr( i))
      if !empty( curSchet) then aAdd( bankSchet, curSchet)   
    next
  endIf
return len( bankSchet) > 0


funcDef getCashMSchet ;
   local rV := "", bankSchet := {}, i := 1, curSchet
  for i := 0 to 99
    curSchet := setUpValue( "CAS" + padL( nStr( i ), 2, '0' ))
    if !empty( curSchet ) then aAdd( bankSchet, curSchet )   
  next
  if empty( bankSchet )
    for i := 0 to 9
      curSchet := setUpValue( "CASH" + nStr( i))
      if !empty( curSchet) then aAdd( bankSchet, curSchet)   
    next
  endIf
  //if !empty( setUpValue( "CASH1")) then aAdd( bankSchet, setUpValue( "CASH1"))
  //if !empty( setUpValue( "CASH2")) then aAdd( bankSchet, setUpValue( "CASH2"))
  //if !empty( setUpValue( "CASH3")) then aAdd( bankSchet, setUpValue( "CASH3"))
  do case
    case len( bankSchet) = 0 do return ""
    case len( bankSchet) = 1 do return bankSchet[ 1]
    case len( bankSchet) > 1 
      i := 1
      if getUp( 5, 5, {{ "Выберите счет для операции"},;
                       { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
                       { "Счет ", block( i), aClone( bankSchet)},;
                       { "*" + chr( 25)}})
         rV := bankSchet[ i]
      else 
        messageB( "Счет не выбран !")
        return ""
      endif
  endcase
return rV


* NEW OBJECT FILE

funcDef kasDec ;
   local summa := 0.00, get, ro, sim, theCode := "3002", n := 1, arr,;
         arr2 := { }, i, nK := 1, aOrder, fullName, pasSer, rest    ,;
         fullAddr, theOrder, aReceipt34a, aNazn, aCode, ptrNazn     ,;
         debSchet, bankSchet, curDest, kassirName, postName,pasGived,;
         credSchet, admArr, admArr2, admPtr, curAdm, curAdmPasp     ,;
         buffer, curNazn

buffer  := ""

aNazn   := { "Вывоз излишков из операционной кассы "      ,;
             "Подкрепление переводных операций(40504 сч.)",;
             "Расчеты по пластиковым картам"              ,;
             "Подкрепление банкомата"                     ,;
             "Недостача в кассе ПБО"                      ,;
             "Недостача в банкомате"                      ,;
             "Вывоз излишков в БО"   }
aCode   := { opDecode( "3002"), opDecode( "3008"), opDecode( "1009"),;
             opDecode( "3010"), opDecode( "3012"), opDecode( "3015"),;
             opDecode( "3018") }  //Mike !!!!!!!!!!! 03.08.98
ptrNazn := 1

bankSchet := SetUpValue( "CASHM")

operGetInfo( aCode[ ptrNazn], @ro, @sim)

arr   := kasCashers()
for i := 1 to len( arr)
    aAdd( arr2, rTrim( arr[ i, 5]) + " " + rTrim( arr[ i, 6]) + " " + ;
                rTrim( arr[ i, 7]))
next
aAdd( arr, nil); aAdd( arr2, nil)
aIns( arr, 1);   aIns( arr2, 1)
arr[ 1] := Space( 10); arr2[ 1] := Space( 10)

//
// администраторы
//
admArr  := kasAdm()
admArr2 := {}
admPtr  := 1
if len( admArr) < 1
  messageB( "Введите данные администраторов ПБО !")
  return .F.
endif
for i := 1 to len( admArr) 
  aAdd( admArr2, rTrim( admArr[ i, 5]) + " " + rTrim( admArr[ i, 6]) + " " + ;
                rTrim( admArr[ i, 7]))
next


debSchet := kasGetKorSchet( ptrNazn)
rest     := kasRest()
get      := {{ "Возврат излишков/Подкрепление пер.операций"     } ,;
             { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"     } ,;
             { "Счет кассы                 : " + splitSSchet( confKassa())          } ,;
             { "Символ кассовой отчетности : ", {|| sim},, {||.F.}                  } ,;
             { "Род операции               : ", {|| ro },, {||.F.}                  } ,;
             { "Назначение платежа         : ", block( ptrNazn), aClone( aNazn),,;
                 {|g| changeK1( g, @get, aNazn, aCode, 2, @debSchet, @ro, @sim)} },;
             { ""                                               } ,;
             { "Остаток в кассе            : " + mStr( rest)    } ,;
             { ""                                               } ,;
             { "Сумма в рублях             : ", block( summa),,,{|g| g:varGet() <= rest}   } ,;
             { "Дата проводки              : " + DtoC(setCurDate())},;
             { ""                                               } ,;
             { "Кассир                     : ", block( nK), aClone( arr2)} ,;
             { "Администратор              : ", block( admPtr), aClone( admArr2)   },;
             { "Копий                      : ", block( n)       }}

if getUp( 5, 5, get)

   use3015( ptrNazn == 7)

   curAdm     := admArr2[ admPtr]
   curAdmPasp := "Паспорт серия " + rTrim( admArr[ admPtr, 1]) + "-" + ;
                 admArr[ admPtr, 2] + " N " + admArr[ admPtr, 3]     + ;
                 " выдан " + admArr[ admPtr, 4]
   do case
      case ptrNazn == 1 .AND. empty( setUpValue( "DEN"))
        messageB( "Нет счета для данной операции")
        return NIL
      case ptrNazn == 2 .AND. empty( curAdmPasp)
        messageB( "Нет паспортных данных администратора !")
        return NIL
// Replaced by TMV 06-05-2004
//      case ptrNazn == 3 .AND. empty( setUpValue( "SCARD"))
      case ptrNazn == 3 .AND. empty( setUpValue( "POS"))
        messageB( "Нет счета для данной операции")
        return NIL
      case ptrNazn == 4 .AND. !chkCashMSchet() 	// 16.07.2001 Залманов А.З.
      //empty( setUpValue( "CASH1") + setUpValue( "CASH2") + setUpValue( "CASH3"))
      //case ptrNazn == 4 .AND. empty( bankSchet)
        messageB( "Нет счета для данной операции !")
        return NIL
      case ptrNazn == 5 .AND. empty( setUpValue( "KASS-"))
        messageB( "Нет счета для данной операции !")
        return NIL
      case ptrNazn == 6 .AND. ( empty( setUpValue( "CASH-")) .OR. !chkCashMSchet()) 	
// 16.07.2001 Залманов А.З.
//        empty( setUpValue( "CASH1") + setUpValue( "CASH2") + setUpValue( "CASH3")))
//           ( empty( setUpValue( "CASH-")) .OR. empty( setUpValue( "CASHM")))
        messageB( "Нет счета для данной операции !")
        return NIL
       case ptrNazn == 7
         if empty( confBONumb())
           messageB( "Введите номер БО для покрепления кассы")
          return NIL
         endif
   endcase
   operGetInfo( aCode[ ptrNazn], @ro, @sim, @curNazn)
   if yesNo( "Выполнять проводку на сумму " + cStr( summa) + " ?" )
      if summa <= kasRest()
         kassirName := arr2[ nK]
         if nK > 1
            pasSer     := "Паспорт серия " + rTrim( arr[ nK, 1] ) + "-" + ;
                          arr[ nK, 2] + " N " + arr[ nK, 3]
            pasGived   := rTrim( arr[ nK, 4])
         else
            pasSer := pasGived := Space( 30)
         endif
         if ptrNazn == 4
            // if empty( debSchet)
              bankSchet := getCashMSchet()
              debSchet  := bankSchet
            // endif
            //fullName  := rTrim( confPostName()) + " через " + kassirName
            fullName := confBankName() + ", " + rTrim( confFilial()) + ;
                        " через " + arr2[ nK]
         else
            fullName  := rTrim( confPostName()) + " через " + curAdm
         endif      
         do case 
            //
            // вывоз излишков операционной кассы
            //
            case ptrNazn == 1
                 fullName := confBankName() + ", " + rTrim( confFilial()) + ;
                             " через " + arr2[ nK]
                 debSchet := setUpValue( "DEN")
            case ptrNazn == 2
                 // fullName := rTrim( fullName) + "," + rTrim( curAdmPasp)
                 fullName := rTrim( fullName)
                 pasSer   := "Паспорт серия " + rTrim( admArr[ admPtr, 1]) + ;
                             "-" + admArr[ admPtr, 2] + " N " + ;
                             admArr[ admPtr, 3]
                 pasGived := admArr[ admPtr, 4]
            case ptrNazn == 3
// Replaced by TMV 06-05-2004
//                 debSchet := setUpValue( "SCARD")
                 debSchet := setUpValue( "POS")
            case ptrNazn == 5 
                 debSchet := setUpValue( "KASS-")         
                 //fullName := arr2[ nK]
                 fullName := confBankName() + ", " + rTrim( confFilial()) + ;
                       " через " + arr2[ nK]
            case ptrNazn == 6
                 debSchet := setUpValue( "CASH-")         
                 //fullName := arr2[ nK]
                 fullName := confBankName() + ", " + rTrim( confFilial()) + ;
                       " через " + arr2[ nK]
            case ptrNazn == 7
                 debSchet := confKorSchet()
                 // fullName := "Через " + curAdm
                 fullName := confBankName() + ", " + rTrim( confFilial()) + ;
                       " через " + arr2[ nK] // curAdm
         endcase
         postName := rTrim( confNodeName()) + " УС отделение " + rTrim( confPostName())
         theOrder := ordIncIn( setCurDate())
         credSchet:= confKassa()
         if ptrNazn = 6 
           credSchet := getCashMSchet()
           buffer    := credSchet
         endif
         if ptrNazn <> 3
            //if ptrNazn == 7
            //   aOrder := kasRKO_BO( theOrder, setCurDate(), debSchet, credSchet,;
            //             summa, fullName, kassirName, curAdmPasp, curNazn /*aNazn[ ptrNazn]*/, sim, ro)
            //else
               aOrder := kasRKO( theOrder, setCurDate(), debSchet, credSchet,;
                         summa, fullName, kassirName, curNazn /*aNazn[ ptrNazn]*/, sim, ro,;
                         pasSer, pasGived)
            //endif
            aReceipt34a := {}
            if ptrNazn == 2 .OR. ptrNazn == 3
               aReceipt34a := Receipt34a( theOrder, setCurDate(), confFilial(),;
                   confOtdName(), confNodeName(), kassirName, curAdm, summa, curNazn /*aNazn[ ptrNazn]*/)
               //aAdd( aReceipt34a, replicate( "-", 78))
            endif
            for i := 1 to len( aOrder) do aAdd( aReceipt34a, aOrder[ i])
            for i := 1 to n do printAOrder( aReceipt34a)
            saveLastOrder( aReceipt34a)
         endif
         if !sumWrite( debSchet, - summa, setCurDate(), aCode[ ptrNazn], theOrder,, buffer)
            errLog( "Проводка не выполнена. Попробуйте еще раз")
         elseif ptrNazn = 3
             messageB( "Проводка выполнена.")
         endif
      else
         errLog( "Эта проводка вызовет красное сальдо по кассе !")
      endif

   endif
endif
//prnReset()
return NIL






funcDef kas1027 ;
   local summa := 0.00, get, ro, sim, theCode := "3002", n := 1, arr,;
         arr2 := { }, i, nK := 1, aOrder, fullName, pasSer, rest    ,;
         fullAddr, theOrder, aReceipt34a, aNazn, aCode, ptrNazn     ,;
         debSchet, bankSchet, curDest, kassirName, postName,pasGived,;
         credSchet, admArr, admArr2, admPtr, curAdm, curAdmPasp, buffer

buffer  := ""
aNazn   := { "Внесение наличных по пластиковым картам" }
aCode   := { opDecode( "1027") }
ptrNazn := 1

operGetInfo( aCode[ ptrNazn], @ro, @sim)
get      := {{ "Внесение наличных по пластиковым картам"     } ,;
             { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"     } ,;
             { "Род операции               : ", {|| ro },, {||.F.}                  } ,;
             { "Назначение платежа         : ", block( ptrNazn), aClone( aNazn)},;
             { ""                                               } ,;
             { "Сумма в рублях             : ", block( summa),,,{|g| g:varGet() > 0}   } ,;
             { "Дата проводки              : " + DtoC(setCurDate())},;
             { ""                                               } }

if getUp( 5, 5, get)

// Replaced by TMV 06-05-2004
//   if empty( setUpValue( "SCARD"))
   if empty( setUpValue( "POS"))
     messageB( "Нет счета для данной операции")
     return NIL
   endif
   if yesNo( "Выполнять проводку на сумму " + cStr( summa) + " ?" )
// Replaced by TMV 06-05-2004
//     credSchet := setUpValue( "SCARD")
     credSchet := setUpValue( "POS")
     theOrder := ordIncIn( setCurDate())
     if !sumWrite( credSchet, summa, setCurDate(), aCode[ ptrNazn], theOrder,, buffer)
        errLog( "Проводка не сделана ! Попробуйте еще раз")
     else
        messageB( "Проводка сделана !!!")
     endif
   endif
endif
return NIL


funcDef kas1009 ;
   local summa := 0.00, get, ro, sim, theCode := "3002", n := 1, arr,;
         arr2 := { }, i, nK := 1, aOrder, fullName, pasSer, rest    ,;
         fullAddr, theOrder, aReceipt34a, aNazn, aCode, ptrNazn     ,;
         debSchet, bankSchet, curDest, kassirName, postName,pasGived,;
         credSchet, admArr, admArr2, admPtr, curAdm, curAdmPasp, buffer

buffer  := ""

aNazn   := { "Выдача наличных по пластиковым картам" }
aCode   := { opDecode( "1009") }
ptrNazn := 1

bankSchet := SetUpValue( "CASHM")

operGetInfo( aCode[ ptrNazn], @ro, @sim)


debSchet := kasGetKorSchet( ptrNazn)
get      := {{ "Выдача наличных по пластиковым картам"     } ,;
             { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"     } ,;
             { "Род операции               : ", {|| ro },, {||.F.}                  } ,;
             { "Назначение платежа         : ", block( ptrNazn), aClone( aNazn)},;
             { ""                                               } ,;
             { "Сумма в рублях             : ", block( summa),,,{|g| g:varGet() > 0}   } ,;
             { "Дата проводки              : " + DtoC(setCurDate())},;
             { ""                                               } }

if getUp( 5, 5, get)

// Replaced by TMV 06-05-2004
//   if empty( setUpValue( "SCARD"))
   if empty( setUpValue( "POS"))
     messageB( "Нет счета для данной операции")
     return NIL
   endif
   if yesNo( "Выполнять проводку на сумму " + cStr( summa) + " ?" )
// Replaced by TMV 06-05-2004
//     debSchet := setUpValue( "SCARD")
     debSchet := setUpValue( "POS")
     theOrder := ordIncIn( setCurDate())
     if !sumWrite( debSchet, - summa, setCurDate(), aCode[ ptrNazn], theOrder,, buffer)
        errLog( "Проводка не сделана ! Попробуйте еще раз")
     else
        messageB( "Проводка сделана !!!")
     endif
   endif
endif
return NIL





funcDef changeK1 with g:O, get:A, aNazn:A, aCode:A, ptrNDACC:N,;
        korSchet:C, ro:C, sim:C local ptr
ptr := aScan( aNazn, {|x| allTrim( x ) == allTrim( g:varGet) })
if ptr > 0 
   korSchet := kasGetKorSchet( ptr, .F.)
   operGetInfo( aCode[ ptr], @ro, @sim)
   if empty( ro)  then ro  := "  "
   if empty( sim) then sim := "  "
   getReProc( get)
endif
return .T.

* NEW OBJECT FILE
funcDef kasUS local ordInc, get, ro, sim, oper, summa, sumRest,;
  credSchet, debSchet, schetNDACC, nazn, aOrder

schetNDACC := confNDACC()
if empty( schetNDACC)
   messageB( "Нет счета NDACC !!!")
   return .F.
endif
sumRest := sumRest( schetNDACC, setCurDate())
do case
   case sumRest <= 0
        oper      := "5301"
        debSchet  := confKorSchet()
        credSchet := confNDACC()
   case sumRest > 0
        oper      := "5302"
        debSchet  := confNDACC()
        credSchet := confKor2Schet()
endcase
summa := abs( sumRest)
operGetInfo( oper, @ro, @sim, @nazn)
get := {{ "Урегулирование сальдо по счету УС"         },;
        { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"         },;
        { "Дебет                      : " + debSchet  },;
        { "Кредит                     : " + credSchet },;
        { "Род операции               : " + ro        },;         
        { "Символ кассовой отчетности : " + sim       },;
        { ""                                          },;
        { "Сумма                      : ", block( summa)}}
if getUp( 5, 5, get)
   ordInc := ordIncMem( setCurDate())
   aOrder := memOrder( ordInc, setCurDate(), debSchet, credSchet, summa, nazn, ro)
   printAOrder( aOrder)
   saveLastOrder( aOrder)
   if sumRest < 0
      summa := summa
   else
      summa := - summa
   endif
   if !sumWrite( schetNDACC, summa, setCurDate(), oper, ordInc)
      errLog( "Проводка не сделана ! Попробуйте еще раз")
   endif
endif

return NIL


* NEW OBJECT FILE

func cashersInfo
static tmp := { ".\cashers.dbf", "CASHERS",;
        {{ "pasSer1", "C",  8, 0},;
         { "pasSer2", "C",  2, 0},;
         { "pasNum",  "C",  6, 0},;
         { "pasGiv",  "C", 20, 0},;
         { "surname", "C", 20, 0},;
         { "name",    "C", 20, 0},;
         { "sname",   "C", 20, 0}},;
         { }, 0}
return tmp


funcDef kasCashers local arr := { }, tmp, s := select()

tmp := cashersInfo()

basePath( tmp, confPathOD())
if baseNSOpen( tmp)
   while .T.
         aAdd( arr, od_Gather())
         exit if !netSkipDown()
   enddo
   baseMClose( tmp)
   select ( s)
endif

return arr



func admInfo
static tmp := { ".\adm.dbf", "ADM",;
        {{ "pasSer1", "C",  8, 0},;
         { "pasSer2", "C",  2, 0},;
         { "pasNum",  "C",  6, 0},;
         { "pasGiv",  "C", 20, 0},;
         { "surname", "C", 20, 0},;
         { "name",    "C", 20, 0},;
         { "sname",   "C", 20, 0}},;
         { }, 0}
return tmp


funcDef kasAdm local arr := { }, tmp, s := select()

tmp := admInfo()

basePath( tmp, confPathOD())
if baseNSOpen( tmp)
   while .T.
         aAdd( arr, od_Gather())
         exit if !netSkipDown()
   enddo
   baseMClose( tmp)
   select ( s)
endif

return arr

funcDef kassJournal local theDate := setCurDate(), n, arr, rV

arr := { "приходу", "расходу"}
n   := 1
if confUseAgent()
  rV := getUp( 5, 5,;
       {{ "Реестр ", block( n), {"принятых средств",;
											"выплаченных средств"}},;
        { "*" + Chr( 25) },;
        { "за     ", block( theDate) }} )
else
  rV := getUp( 5, 5, {{ "Кассовый журнал по ", block( n), arr},;
                    { "*" + Chr( 25) },;
                    { "за                 ", block( theDate) }} )
endif
if rV
	if n < 3
		makeJournal( theDate, n == 1)
	else
		makeReestr( theDate, n - 2)
	endif
endif

return NIL


funcDef Journ904 local theDate := setCurDate(), n, arr


if getUp( 5, 5, {{ "Ведомость надомников "},;
                 { "за                   ", block( theDate) }} )

   make904( theDate)
endif


return NIL

static funcDef getFioFromSums local rV := "", arr, ptr

do case
   case left( SUMS ->buffer, 1) == "A"
     if WISC ->( dbSeek( left( SUMS ->buffer, 20) + "977 "))
       arr := CtoA( WISC ->buffer, ";")
       ptr := val( arr[ 2])
//  Replace by TMV  4-02-2002
//       rV  := arr[ 3 + ptr * 4]
       rV  := arr[ 3 + ptr * 6]
     endif
   case left( SUMS ->buffer, 1) == "S"
     if WISC ->( dbSeek( left( SUMS ->buffer, 20) + "975 "))
       arr := CtoA( WISC ->buffer, ";")
       ptr := val( arr[ 2])
       rV  := arr[ 3 + ptr * 6]
     endif
   otherwise
     if ACC ->( dbSeek( SUMS ->schet)) .AND. CLIENTS ->( dbSeek( ACC ->code))
       rV := cliFullName()
     endif
endcase
return rV

//
// формирование кассового журнала
//
funcDef makeJournal with theDate:D, lPositiveSum:L ;
                    local aH, aC, fk, tmp, total, i,;
                    theRo, theSim, s, cnt, aCode

s := select(); total := 0.00; cnt := 0
ACC     ->( baseSavePos())
CLIENTS ->( baseSavePos())
SUMS    ->( baseSavePos())

tmp := { "tmp_kas.dbf", "TMP",;
        {{ "met",   "C",  1, 0} ,;
         { "ro",    "C",  2, 0} ,;
         { "nDoc",  "N",  4, 0} ,;
         { "schet", "C", 20, 0} ,;
         { "sim",   "C",  2, 0} ,;
         { "summa", "N", 15, 2} ,;
         { "fio",   "C", 30, 0}},;
        {{, "nDoc"}}, 0}

SUMS ->( netGoTop())
basePath( tmp, confPathTemp())
if baseCreate( tmp)
   if baseMXOpen( tmp)
      ACC     ->( dbSetOrder( 1))
      CLIENTS ->( dbSetOrder( 1))
      operStartUp( "Подготовка кассового журнала", i := 1, SUMS ->( lastRec() ))
      while SUMS ->( !eof())
        if SUMS ->dDate == theDate          .AND. ;
           SUMS ->typeOf < "9000"           .AND. ;
           kasTypes( SUMS ->typeOf)         .AND. ;
           if( lPositiveSum, SUMS ->summa >0, SUMS ->summa <0)

           operGetInfo( SUMS ->typeOf, @theRo, @theSim)
           TMP ->( dbAppend())
           TMP ->met   := '*'
           TMP ->ro    := theRo
           TMP ->nDoc  := SUMS ->nDoc
// Add by TMV 18-12-2002
			  if opEqual( SUMS ->typeOf, '2060')             // Add by TMV 18-12-2002
			  	if Left( SUMS->Buffer, 4) == 'PCT:'
           		TMP ->schet := subStr( SUMS->Buffer, 5, 20)
				else
           		TMP ->schet := Left( SUMS->Buffer, 20)
				endif
			  else
           	TMP ->schet := SUMS ->schet
			  endif
// Add by TMV 18-12-2002 -- End
           TMP ->sim   := theSim
           TMP ->summa := abs( SUMS ->summa/ 100)
           TMP ->fio   := getFioFromSums()
           total       += TMP ->summa
           cnt++
        endif
        skip 1 alias SUMS
        operProc( i++)
      enddo
      operExit()

      TMP ->( netGoTop())
      if TMP ->( emptyRec())
         errLog( "Нет данных для показа")
      else
         aH := {{ "Кассовый журнал по " + if( lPositiveSum, "ПРИХОДУ", "РАСХОДУ") } ,;
                { "за " + rDate( theDate)                                         } ,;
                { "Документов - " + nStr( cnt)                                    } ,;   
                { "Итого по документу  " + cStr( total)                           } ,;
                { "Клиент ", block( TMP ->fio),,{||.F.}                           } ,;
                { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                       }}
         if confUseAgent()
           aH[ 1] := { "Реестр " + if( lPositiveSum, "ПРИНЯТЫХ", "ВЫПЛАЧЕННЫХ") + ;
                       " средств" }
         endif
         aC := {{ "Печать",          {|| TMP ->met       }} ,;
					 { "Род опер",        {|| TMP ->ro        }} ,;
                { "N док",           {|| TMP ->nDoc      }} ,;
                { "Счет клиента",    {|| splitSSchet( TMP ->schet)}} ,;
                { "Символ",          {|| TMP ->sim       }} ,;
                { "Сумма",           {|| TMP ->summa     }} ,;
                { "Сумма документа", {|| TMP ->summa     }} }
         fk := {,,,,,,;
					 {{ "Поиск", {|| seekJournal()}}},,;
                {{ "Печать", {|| printJournal( theDate, lPositiveSum)}} }}

         setKey( K_SPACE, {|| setJournOf() })  //  Add by TMV  22-11-2001
         setKey( K_GREY_PLUS, {|| setJrnOnAll() })  //  Add by TMV  19-04-2002
         setKey( K_GREY_MINUS, {|| setJrnOffAll() })  //  Add by TMV  19-04-2002

         TMP ->( browseUp( 2, 2, aH, aC,;
                  {|| netSkipUp()   },;
                  {|| netSkipDown() },;
                  {|| netGoTop()    },;
                  {|| netGoBottom() },,,,, fk))
                  
         setKey( K_SPACE, NIL )                //  Add by TMV  22-11-2001
         setKey( K_GREY_PLUS, NIL )          //  Add by TMV  19-04-2002
         setKey( K_GREY_MINUS, NIL )         //  Add by TMV  19-04-2002

      endif

      baseMClose( tmp)
   endif
   baseDelete( tmp)
endif

SUMS    ->( baseRestPos())
CLIENTS ->( baseRestPos())
ACC     ->( baseRestPos())
select ( s)
return NIL



//
// просмотр поручений
//
funcDef viewOper with aOper:CA, aWisc:CA, name:C ;
   local aH, aC, fk, tmp, total, i, theRo, theSim, s, cnt,;
         theDate := setCurDate(), arrTran


if valType( aOper) == "C" then aOper := { aOper}
if valType( aWisc) == "C" then aWisc := { aWisc}

s := select(); total := 0.00; cnt := 0
ACC     ->( baseSavePos())
CLIENTS ->( baseSavePos())
SUMS    ->( baseSavePos())

provGetInfo( "08", "65",,,, @arrTran)

tmp := { "tmp_kas2.dbf", "TMP",;
        {{ "ro",    "C",  2, 0} ,;
         { "nDoc",  "N",  4, 0} ,;
         { "schet", "C", 20, 0} ,;
         { "schet2","C", 20, 0} ,;
         { "sim",   "C",  2, 0} ,;
         { "oper",  "C",  4, 0} ,;
         { "recNo", "N", 15, 0} ,;
         { "summa", "N", 15, 2}},;
        {{, "schet"}}, 0}

SUMS ->( netGoTop())
basePath( tmp, confPathTemp())
if baseCreate( tmp)
   if baseMXOpen( tmp)

      operStartUp( "Подготовка " + name, i := 1, SUMS ->( lastRec() ))
      while SUMS ->( !eof())

            if checkOper( theDate, aOper)
               operGetInfo( SUMS ->typeOf, @theRo, @theSim)
               TMP ->( dbAppend())
               TMP ->oper   := SUMS ->typeOf
               TMP ->ro     := theRo
               TMP ->nDoc   := SUMS ->nDoc
               TMP ->schet  := SUMS ->schet
               TMP ->schet2 := getSchet2( arrTran)
               TMP ->sim    := theSim
               TMP ->summa  := abs( SUMS ->summa/ 100)
               TMP ->recNo  := SUMS ->( recNo())
               total        += TMP ->summa
               cnt++
            endif
            skip 1 alias SUMS
            operProc( i++)
      enddo
      operExit()

      TMP ->( netGoTop())
      if TMP ->( emptyRec())
         errLog( "Нет данных для показа")
      else
         aH := {{ name } ,;
                { "за " + rDate( theDate)                   },;
                { "Документов - " + nStr( cnt)              },;   
                { "Итого по документу  " + cStr( total)     },;
                { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}
         aC := {{ "N док",           {|| TMP ->nDoc   }} ,;
                { "Род опер",        {|| TMP ->ro     }} ,;
                { "Дебет",           {|| TMP ->schet  }} ,;
                { "Кредит",          {|| TMP ->schet2 }} ,;
                { "Сумма",           {|| TMP ->summa  }}}
         fk := {,,,, {{ "Печать", {|| operPrint( theDate)}}}  }
         TMP ->( browseUp( 2, 2, aH, aC,;
                  {|| netSkipUp()   },;
                  {|| netSkipDown() },;
                  {|| netGoTop()    },;
                  {|| netGoBottom() },,,,, fk))
                  
      endif

      baseMClose( tmp)
   endif
   baseDelete( tmp)
endif

SUMS    ->( baseRestPos())
CLIENTS ->( baseRestPos())
ACC     ->( baseRestPos())
select ( s)
return NIL

static funcDef checkOper with curDate:D, aOper:A local rV 
  rV := SUMS ->dDate == curDate .AND. ;
        aScan( aOper, {|x| x == SUMS ->typeOf}) > 0 .AND. ;
        left( SUMS ->buffer, 1) <> "C"
return rV

funcDef getSchet2 with arrTran:A local rV := "", pos,;
	sumTmp := 0, sBuff := ""  // Add by TMV  09-12-2002

   do case
      case SUMS ->typeOf == "5011" .AND. len( arrTran) == 1
           rV := arrTran[ 1]
      case SUMS ->typeOf == "5010"
           do case
              case left( SUMS ->buffer, 1) $ "B,C"
                   rV := subStr( SUMS ->buffer, 2, 20)
              case left( SUMS ->buffer, 1) == "F"
                   rV := confKorSchet()
              case WISC ->( dbSeek( left( SUMS ->buffer, 20) + "977 "))
                   if ( pos := at( ";", WISC ->buffer)) >0
                      rV := left( WISC ->buffer, pos - 1)
                   endif
           endcase
// Add by TMV 09-12-2002
// and REPLACED 17-12-2002
      case SUMS ->typeOf == "6136"
			if ( pos := at( ";", SUMS ->buffer)) >0
				rV := subStr( SUMS ->buffer, pos + 1, 20)
			endif
      case SUMS ->typeOf == "6137"
			rV := subStr(SUMS ->buffer, 5, 20)
//      case SUMS ->typeOf == "7033"
//			if Left( SUMS->Buffer, 4 ) == 'PCT:'
//				rV := subStr(SUMS ->buffer, 5, 20)
//			elseif ( pos := at( ";", SUMS ->buffer)) >0
//				rV := subStr( SUMS ->buffer, pos + 1, 20)
//			endif
//  REPLACED 17-12-2002 -- End
      case SUMS ->typeOf == "6031"
			sBuff = SUMS->Buffer
			sumTmp = SUMS->Summa
			SUMS ->( baseSavePos())
			SUMS ->( dbSetOrder( 4))
			SUMS ->( dbSeek( Str( sumTmp, 15, 0)))
			do while SUMS->Summa == sumTmp .And. !SUMS->(EOF())
				if SUMS->TypeOf == "7032" .And. SUMS->Buffer == sBuff
					rV := SUMS ->Schet
					Exit
				endif
				SUMS->(dbSkip())
			enddo
			SUMS ->( baseRestPos())
// Add by TMV 09-12-2002 -- End
   endcase
return rV


funcDef operPrint with theDate:D local i, arrDebet, arrCredit, arrSumma
   TMP ->( baseSavePos())
   TMP ->( netGoTop())
   arrDebet := {}; arrCredit := {}; arrSumma := {}
   operStartUp( "Подготовка мем.ордера", i := 1, TMP ->( lastRec()))
   while TMP ->( !Eof())
      aAdd( arrDebet,  TMP ->schet)
      aAdd( arrCredit, TMP ->schet2)
      aAdd( arrSumma,  TMP ->summa)
      skip 1 alias TMP
      operProc( i++)
   enddo
   operExit()
   if !empty( arrDebet)
      printAOrder( memOrder( 0, theDate, arrDebet, arrCredit, arrSumma, "", ""))
   endif
   TMP ->( baseRestPos())
return NIL


funcDef operDelete with aOper:A, aWisc:A local recNo, ptr

ptr := aScan( aOper, {|x| x == TMP ->oper})
messageStartUp( "Удаление проводки...")
SUMS ->( dbGoTo( TMP ->recNo))
if SUMS ->typeOf == TMP ->oper .AND. ;
   SUMS ->schet == TMP ->schet .AND. ;
   abs( SUMS ->summa/ 100) == TMP ->summa
   if SUMS ->( mRLock())
      if ptr > 0 .AND. ptr <= len( aWisc)
         if wiscOpen()
            if WISC ->( dbSeek( TMP ->schet + aWisc[ ptr]))
               WISC ->( mDelete())
            endif
            wiscClose()
         endif         
      endif
      SUMS ->( netUnLock())
   endif
else
   messageB( "Проводка не найдена !!!")
endif
messageExit( 0)

return NIL

//
// просмотр поручений
//
funcDef make904 with theDate:D ;
                local aH, aC, fk, tmp, total, i, theRo, theSim, s, cnt

s := select(); total := 0.00; cnt := 0
ACC     ->( baseSavePos())
CLIENTS ->( baseSavePos())
SUMS    ->( baseSavePos())


tmp := { "tmp_kas.dbf", "TMP",;
        {{ "ro",    "C",  2, 0} ,;
         { "nDoc",  "N",  4, 0} ,;
         { "schet", "C", 20, 0} ,;
         { "sim",   "C",  2, 0} ,;
         { "summa", "N", 15, 2}},;
        {{, "schet"}}, 0}

SUMS ->( netGoTop())
basePath( tmp, confPathTemp())
if baseCreate( tmp)
   if baseMXOpen( tmp)

      operStartUp( "Подготовка кассового журнала", i := 1, SUMS ->( lastRec() ))
      while SUMS ->( !eof())

            if SUMS ->dDate == theDate .AND. ;
               opArrEqual( SUMS ->typeOf, "1005,1008")
                      
               operGetInfo( SUMS ->typeOf, @theRo, @theSim)
               TMP ->( dbAppend())
               TMP ->ro    := theRo
               TMP ->nDoc  := SUMS ->nDoc
               TMP ->schet := SUMS ->schet
               TMP ->sim   := theSim
               TMP ->summa := abs( SUMS ->summa/ 100)
               total       += TMP ->summa
               cnt++
            endif
            skip 1 alias SUMS
            operProc( i++)
      enddo
      operExit()

      TMP ->( netGoTop())
      if TMP ->( emptyRec())
         errLog( "Нет данных для показа")
      else
         aH := {{ "Выплата пенсий надомникам" } ,;
                { "за " + rDate( theDate)                                         } ,;
                { "Документов - " + nStr( cnt)                                    } ,;   
                { "Итого по документу  " + cStr( total)                           } ,;
                { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                       }}
         aC := {{ "Род опер",        {|| TMP ->ro        }} ,;
                { "N док",           {|| TMP ->nDoc      }} ,;
                { "Счет клиента",    {|| splitSSchet( TMP ->schet)}} ,;
                { "Символ",          {|| TMP ->sim       }} ,;
                { "Сумма",           {|| TMP ->summa     }} ,;
                { "Сумма документа", {|| TMP ->summa     }} }
         fk := {,,,,,,,, {{ "Справка", {|| calcRec16()}}} }

         TMP ->( browseUp( 2, 2, aH, aC,;
                  {|| netSkipUp()   },;
                  {|| netSkipDown() },;
                  {|| netGoTop()    },;
                  {|| netGoBottom() },,,,, fk))
                  
      endif

      baseMClose( tmp)
   endif
   baseDelete( tmp)
endif

SUMS    ->( baseRestPos())
CLIENTS ->( baseRestPos())
ACC     ->( baseRestPos())
select ( s)
return NIL

funcDef calcRec16 local cnt := 0, ASumma := 0
messageStartUp( "Подготовка справки к мем.ордеру")
TMP ->( baseSavePos())
TMP ->( netGoTop())
while TMP ->( !Eof())
      cnt++
      ASumma += TMP ->summa
      skip 1 alias TMP
enddo
printCntOrders( Receipt16( setCurDate(), cnt, ASumma))
TMP ->( baseRestPos())
messageExit()
return NIL


static funcDef setAltOn
  set console   off
  set alternate to ( getDonName()) additive
  set alternate on
return NIL

static funcDef setAltOff
  set console   on
  set alternate to
  set alternate off
return NIL

funcDef printJournal with theDate:D, lPositiveSum:L ;
        local i, total := 0, str, file, cnt := 0, useDon   ,;
              startCode, stopCode, str1, cntLines, cntPages,;
              useRoll, useFile, sHeader,;
				  sText := ''  // Add by TMV  25-08-2003


useDon   := confPrintDon()
useRoll := yesNo( "Печатать " + if( confUseAgent(), "реестр","кассовый журнал") +;
	" на рулон ?")
useFile := !useDon .AND. noYes( "Печатать в файл ?")
TMP ->( baseSavePos())
TMP ->( netGoTop())
file := newFPath( "jorn_" + if( lPositiveSum, "c", "d") + ".txt", confPathTemp())
if file( file) then fErase( file)
useStopDon( .F.)

sHeader := "Печать " + if( confUseAgent(), "реестра", "кассового журнала")
operStartUp( sHeader, i := 1, TMP ->( lastRec()) )

///////// Заменено TMV 25-09-2001
#ifndef VLAD
str := { if( !useDon, chr( 15), "") + "Банк " + confBankName()           ,;
         "Отделение " + confOtdNumb(), ""                                ,;
         "Кассовый журнал по " + if( lPositiveSum, "приходу", "расходу") ,;
         "за " + rDate( theDate),;
         "Счет кассы: " + confKassa(), ""}
#else
str := { if( !useDon, chr( 15), ""),;
		  "                    Счет кассы: " + confKassa(),;
        Center("Аналитический кассовый журнал по " + if( lPositiveSum, "приходу", "расходу"), 70),;
        Center("за " + rDateFull( theDate), 70), ""}
#endif

/*
str := { if( !useDon, chr( 15), ""),;
		  "                    Счет кассы: " + confKassa(),;
        Center("Аналитический кассовый журнал по " + if( lPositiveSum, "приходу", "расходу"), 70),;
        Center("за " + rDateFull( theDate), 70), ""}
*/
if confUseAgent()
  str := { if( !useDon, chr( 15), "") + confBankName() + " " + confFilial()  ,;
         confNodeName() + " " + confPostName(), "",;
         "Реестр " + if( lPositiveSum, "принятых", "выплаченных") + " средств",;
         "за " + rDateFull( theDate), ""}
endif                    

cntPages := 1
cntLines := 14
fillHeader( @str, useDon, @cntPages)
if useDon
   if file( getDonName()) then fErase( getDonName())
   setAltOn()
   ? "&4" + chr( 27) + chr( 33) + chr( 1) + chr( 10) + chr( 13)
   for i := 1 to len( str) do ? str[ i]
   setAltOff()
endif

while TMP ->( !eof())
	if !empty(TMP->met)
      if cntLines >= 55
         cntLines := 8
         if useDon then str := {}
// Replace by TMV  28-11-2002
//   		aAdd( str, " -----------------------------------------------")
			aAdd( str, "---------------------------------------------------------"+if(!confUseAgent(),"----", "")+"-----------------")
         if !useDon .AND. !useRoll then aAdd( str, chr( 12))
         fillHeader( @str, useDon, @cntPages)
      endif
      str1 := "|" + TMP ->schet          + "|" + TMP ->fio           + ;
              "|" + padL( TMP ->nDoc, 4) +;
				  if(!confUseAgent(),"|" + padC( TMP ->sim, 3), "") + ;
              "|" + padL( TMP ->summa, 15, 2)  + "|"
      if useDon
         setALtOn()
         ? str1
         setAltOff()
      else
         aAdd( str, str1)
         if len( str) > 200
            if useFile
               eFileWrite( str, file)
            else
               printAOrder( str)
            endif
            str := {}
         endif
      endif
      cntLines++
      cnt++
      total += TMP ->summa
	endif
	skip 1 alias TMP
   operProc( i++)
enddo

// Replace by TMV  28-11-2002
//str1 := { " -----------------------------------------------",;
sText = allTrim( rSumma( total ))                      // Add by TMV  25-08-2003
sText = Upper( Left( sText, 1) ) + subStr( sText, 2 )  // Add by TMV  25-08-2003
str1 := {"---------------------------------------------------------"+if(!confUseAgent(),"----", "")+"-----------------",;
	if( cntLines > 45 .AND. !useRoll , chr( 12), ""),;
	"","                           ИТОГО " + padL( total, 15, 2), "",;
	"    ИТОГО: (сумма прописью)",;
	sText,;
	"    Количество документов: " + nStr( cnt), "",;
	"    Кассир                    ____________________" , "",;
	"    Ответственный исполнитель ____________________" +;
			if( !useDon, chr( 18), ""),;
if( !useDon .AND. !useRoll, "", chr( 12))}
// Replaced to <sText> by TMV 25-08-2003
//	rSumma( total),;

if confUseAgent()
   str1 := {"--------------------------------------------------------------------------",; 
				if( cntLines > 45 .AND. !useRoll , chr( 12), ""),;
             "", "","ИТОГО             " + padL( total, 15, 2), "", ""     ,;
             "Общее кол-во документов - " + nStr( cnt) + " на сумму "      ,;
             sText                                                         ,;
             "Ответственный исполнитель ______________________________"    +;
             if( !useDon, chr( 18), ""),;
             if( !useDon .AND. !useRoll, "", chr( 12))}
endif
// Replaced to <sText> by TMV 25-08-2003
//	rSumma( total),;


if useDon
   setAltOn()
   for i := 1 to len( str) do ? str1[ i]
   ? chr( 10) + chr( 13) + chr( 12) + chr( 10) + chr( 13)
   setAltOff()
   runExe( getDonExeName())
else
   for i := 1 to len( str1) do aAdd( str, str1[ i])
   //
   // если печать идет на рулон, то выведем еще строк
   //
   if useRoll then for i := 1 to 15 do aAdd( str, "")
   if useFile
      eFileWrite( str, file)
   else
      printAOrder( str)
   endif
   str := {}
endif


operExit()
useStopDon( .T.)
TMP ->( baseRestPos())

if useFile then viewTemps( file)
return NIL


static funcDef fillHeader with str:A, useDon:L, cntPages:N
aAdd( str, "")
aAdd( str, "Страница : " + Str( cntPages++))
///////  Заменено  TMV  25-09-2001
if useDon
//   aAdd( str, "-----------------------------------------------------------------------------|")
//   aAdd( str, "|Род| N  |           Счет клиента  |Особ.|Сим|Сумма по симв. |Сумма документа|")
//   aAdd( str, "|оп.|док |                         |отмет|вол|кассовой отчетн|               |")
//   aAdd( str, "-----------------------------------------------------------------------------|")
   aAdd( str, " -----------------------------------------------")
   aAdd( str, "|Номер счета клиента | N   |Шифр|Сумма рублях РФ|")
   aAdd( str, "|                    | п/п |док.|               |")
   aAdd( str, "|-----------------------------------------------|")
else
///////  Заменено  TMV  25-09-2001
   aAdd( str, "---------------------------------------------------------"+if(!confUseAgent(),"----", "")+"-----------------")
   aAdd( str, "|Номер лицевого счета|       Плательщик             |N   "+if(!confUseAgent(),"|   ", "")+"|Сумма документа|")
   aAdd( str, "|                    |                              |док "+if(!confUseAgent(),"|СКО", "")+"|               |")
   aAdd( str, "---------------------------------------------------------"+if(!confUseAgent(),"----", "")+"-----------------")
endif
return NIL

* NEW OBJECT FILE
//
// сводная справка о кассовых оборотах
//
funcDef kassSVod local str, s, cStr, tmpDebet, tmpCredit, i,;
                       cntDebet, cntCredit


      SUMS ->( baseSavePos())
      SUMS ->( dbSetOrder( 2))
      SUMS ->( dbSeek( DtoS( setCurDate())))
      operStartUp( "Подсчет оборотов по кассе", i := 1, SUMS ->( lastRec()))
      tmpDebet := tmpCredit :=0
      cntDebet := cntCredit :=0
      while SUMS ->dDate == setCurDate() .AND. SUMS ->( !eof())
            if SUMS ->typeOf < "9000" .AND. ;
               kasTypes( SUMS ->typeOf)

               if SUMS ->summa >0
                  tmpCredit += SUMS ->summa
                  cntCredit++
               else
                  tmpDebet  -= SUMS ->summa
                  cntDebet++
               endif
            endif
            skip 1 alias SUMS
            operPRoc( i++)
      enddo
      operExit()
      SUMS ->( baseRestPos())


      str := { confBankName()                                       ,;
               "Отделение " + confOtdNumb()                         ,;
               "", "", "     Сводная справка о кассовых оборотах"   ,;
               "", "за " + rDateFull( setCurDate()), ""                 ,;
               "---------------------------------------------------",;
               "|                 | кол.док. |        Сумма       |",;
               "---------------------------------------------------",;
               "|  Приход за день |" + padC( cntCredit, 10)         +;
               "|" + padL( strTran( mStr(tmpCredit/100), ".", ""), 20) + "|",;
               "---------------------------------------------------",;
               "|  Расход за день |" + padC( cntDebet,10)           +;
               "|" + padL( strTran( mStr( tmpDebet/ 100), ".", ""), 20)+ "|",;
               "---------------------------------------------------",;
               "", "", ""                                           ,;
               "Заведующий кассой _____________________"            ,;
               "", "С бухгалтерским журналом сверено"               ,;
               "", "Бухгалтер         _____________________" }
      cStr := ""
      aEval( str, {|x| cStr += x + endl() }) 
      editMemo( cStr)
      for i := 1 to 12 do aAdd( str, "")
      if yesNo( "Распечатать справку ?") then printAOrder( str)

return NIL

funcDef _isSR with Schet:C 
return left( schet, 5) $ "42302,42303,42304,42305,42306,42307,42602,42603,42604,42605,42606,42607"

// Edited by TMV  29-07-2004
// Изменена проверка на "карточность" счета
funcDef _isCard with schet:C ;
local opened := Select( "ACC") >0, s := select(), rV:=.F.  // Add by TMV 29-07-2004
if !opened then rV := accOpen()
if opened .OR. rV
	rV = .F.
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))
   if accSeek(schet) then rV = (Left( ACC->PRIZ, 6) == "15 4  ")
   ACC ->( baseRestPos())
   if !opened then accClose()
endif
select ( s)
Return rV
// Replaced by TMV 29-07-2004 -- End


// Edited by TMV  29-07-2004
// Изменена проверка на типа счета
funcDef _isTek with schet:C ;
local opened := Select( "ACC") >0, s := select(), rV:=.F.  // Add by TMV 29-07-2004
//CARDNEW Проверять надо по типу счета, т.к. 42301 - это еще и карточный счет
// Replaced by TMV 29-07-2004
//return left( schet, 5) $ "42301,42601"
if !opened then rV := accOpen()
if opened .OR. rV
	rV = .F.
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))
   if accSeek(schet) then rV = (Left( ACC->PRIZ, 6) == "15 1  ")
   ACC ->( baseRestPos())
   if !opened then accClose()
endif
select ( s)
Return rV
// Replaced by TMV 29-07-2004 -- End


funcDef downJournal ;
   local curD := setCurDate(), i, arr, Str, arrSchet,;
         firstRest, lastRest, p, kasRest
if !getUp( 5, 5, {{ "Подготовка журнала операций"   },;
                  { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
                  { "Введите дату", block( curD)    }})
   return NIL
endif
arr := {{ 0, 0},;
        { 0, 0},;
        { 0, 0},;
        { 0, 0},;
        {}     ,;
        { 0, 0},;
        { 0, 0},;
        { 0, 0},;
        { 0, 0},;
        { 0, 0},;
        { 0, 0},;
        { 0, 0},;
                ;
        { 0, 0},; //13 - с  текущих на текущие
        { 0, 0},; //14 - с  текущих на срочные
        { 0, 0},; //15 - с  текущих на карточные
        { 0, 0},; //16 - со срочных на текущие
        { 0, 0},; //17 - с  карточных на текущие
                ;
        {},     ; //18 - платежи по сервисным картам
        { 0, 0},; //19 - со срочных на карточные  //  Add by TMV  20-05-2002
        { 0, 0},; //20 - погашено микрокредитов     //  Add by TMV  09-12-2002
        { 0, 0} ; //21 - погашено микрокредитов %   //  Add by TMV  09-12-2002
               }

operStartUp( "Подготовка журнала операций", i := 1, SUMS ->( recNo()))
SUMS ->( netGoTop())
p := GSZ_BUY_BEZNAL
p := GSZ_BUY_NAL
while SUMS ->( !Eof())
  if SUMS ->dDate <> curD 
     skip 1 alias SUMS
     operProc( i++)
     loop
  endif
  //
  // кассовые операции
  if kasTypes( SUMS ->typeOf)
     if SUMS ->summa > 0
        addDown( arr, 1, SUMS ->summa/ 100)
     else
         addDown( arr, 2, SUMS ->summa/ 100)
     endif
  endif
  //
  // переводные операции
  //
  if SUMS ->typeOf == "3008" 
     addDown( arr, 3, SUMS ->summa/ 100)
  endif
  if SUMS ->typeOf == "3007"
     addDown( arr, 4, SUMS ->summa/ 100)
  endif
  //
  // ГСЗ
  //
  if SUMS ->typeOf == GSZ_BUY_BEZNAL .OR. SUMS ->typeOf == GSZ_BUY_NAL
     addDown( arr, 11, SUMS ->summa/ 100)
  endif
  //
  // разовые платежи
  //
  if left( SUMS ->buffer, 1) == "A"
     addPlat( arr)
  endif
  if left( SUMS ->buffer, 1) == "S"
    addServCard( arr)
  endif


  if opArrEqual( SUMS ->typeOf, "2010,5010") .AND. len( rTrim( SUMS ->buffer)) == 13 .AND. ;
     left( SUMS ->buffer, 2) = "06"
     addZabota( arr)
  endif
  //
  // выдача микрокредита
  //
  if opEqual( SUMS ->typeOf, "1007")
     addDown( arr, 6, SUMS ->summa/ 100)
  endif
  if opEqual( SUMS ->typeOf, "6030") .AND. left( SUMS ->schet, 2) == "45"
     addDown( arr,  6, SUMS ->summa/ 100)
     addDown( arr, 12, SUMS ->summa/ 100)
  endif

// Add by TMV  09-12-2002
  //
  // выдача микрокредита безналом
  //
  if opEqual( SUMS ->typeOf, "1060") // Replace 6031 to 1060 by TMV 15-12-2002
     addDown( arr, 6, SUMS ->summa/ 100)
  endif
  // Возврат ссуды
  if opEqual( SUMS ->typeOf, "6136") // Replace 1060 to 6136 by TMV 15-12-2002
     addDown( arr, 20, SUMS ->summa/ 100)
  endif
  if opEqual( SUMS ->typeOf, "2060") .And. Left( SUMS->Buffer, 4) == 'PCT:'
     addDown( arr, 21, SUMS ->summa/ 100)
  endif
// Add by TMV  09-12-2002 -- End

  //
  // оформлено срочных вкладов
  //
  if opEqual( SUMS ->typeOf, "2002")
     addDown( arr, 7, SUMS ->summa/ 100)
  endif
  //
  // закрыто срочных вкладов
  //
  if opEqual( SUMS ->typeOf, "1052")
     addDown( arr, 8, SUMS ->summa/ 100)
  endif
  //
  // безналичные перечисления
  //
  if opArrEqual( SUMS ->typeOf, "5010,5200,5201") .AND. left( SUMS ->buffer, 1) $ "F,B"
     do case
        case _isTek( SUMS ->schet) .AND. _isTek( subStr( SUMS ->buffer, 2, 20))
          addDown( arr, 13, SUMS ->summa/ 100)
        case _isTek( SUMS ->schet) .AND. _isSR(  subStr( SUMS ->buffer, 2, 20))
          addDown( arr, 14, SUMS ->summa/ 100)
        case _isTek( SUMS ->schet) .AND. _isCard(subStr( SUMS ->buffer, 2, 20))
          addDown( arr, 15, SUMS ->summa/ 100)
        case _isSR(  SUMS ->schet) .AND. _isTek( subStr( SUMS ->buffer, 2, 20))
          addDown( arr, 16, SUMS ->summa/ 100)
        case _isCard( SUMS ->schet) .AND. _isTek(subStr( SUMS ->buffer, 2, 20))
          addDown( arr, 17, SUMS ->summa/ 100)
//  Add by TMV  20-05-2002
        case _isSR(  SUMS ->schet) .AND. _isCard( subStr( SUMS ->buffer, 2, 20))
          addDown( arr, 19, SUMS ->summa/ 100)
//////////////////
     endcase
  endif

  skip 1 alias SUMS
  operProc( i++)
enddo
addDown() // очистка стека счетов
operExit()
firstRest := kasRest( "998")
lastRest  := firstRest - arr[ 2, 2] + arr[ 1, 2]
kasRest   := kasRest( "999")
if confUseAgent()
   Str := "Журнал операций Агента за " + rDateFull( curD) + endl() + endl() + ;
       "1. Прием/выплата денежных средств"           + endl() + ;
       "   Приход  кол-во: " + padL( arr[ 1, 1], 4)           + ;
                     " сумма : " + mStr( arr[ 1, 2]) + endl() + ;
       "   Расход  кол-во: " + padL( arr[ 2, 1], 4)           + ;
                     " сумма : " + mStr( arr[ 2, 2]) + endl()
else
   Str := "Журнал операций за " + rDateFull( curD) + endl() + endl() + ;
       "1. Касса"                                    + endl() + ;
       "   Остаток денежных средств на начало дня"            + ;
               mStr( firstRest)                      + endl() + ;
       "   Приход  кол-во: " + padL( arr[ 1, 1], 4)           + ;
                     " сумма : " + mStr( arr[ 1, 2]) + endl() + ;
       "   Расход  кол-во: " + padL( arr[ 2, 1], 4)           + ;
                     " сумма : " + mStr( arr[ 2, 2]) + endl() + ;
       "   Остаток денежных средств на конец дня "            + ;
                               mStr( lastRest) + endl()
endif
str += ;
       "2. Переводные операции"                      + endl() + ;
       "   Выдано на переводные операции    сумма : "         + ;
                                   mStr( arr[ 3, 2]) + endl() + ;
       "   Возвращено с переводных операций сумма : "         + ;
                                   mStr( arr[ 4, 2]) + endl() + ;
       "3. Платежи населения"                        + endl() + ;
           getPlatStr( arr)                                   + ;  
       "4. Микрокредиты"                             + endl() + ;
       "   Выдано кол-во  : " + padL( arr[ 6, 1], 4)          + ;
                     " сумма : " + mStr( arr[ 6, 2]) + endl() + ;
       "   Из них под продуктовые заказы" + endl()            + ;
       "          кол-во  : " + padL( arr[12, 1], 4)          + ;
                     " сумма : " + mStr( arr[12, 2]) + endl() + ;
       "   Погашено кол-во : " + padL( arr[20, 1], 4)         + ; // Add by TMV  09-12-2002
                     " сумма: " + mStr( arr[20, 2]) + endl()  + ; // Add by TMV  09-12-2002
       "   Погашено% кол-во: " + padL( arr[21, 1], 4)         + ; // Add by TMV  09-12-2002
                     " сумма: " + mStr( arr[21, 2]) + endl()  + ; // Add by TMV  09-12-2002
       "5. Срочные вклады"                           + endl() + ;
       "   Оформлено кол-во : " + padL( arr[ 7, 1], 4)        + ;
                     " сумма : " + mStr( arr[ 7, 2]) + endl() + ;
       "   Закрыто   кол-во : " + padL( arr[ 8, 1], 4)        + ;
                     " сумма : " + mStr( arr[ 8, 2]) + endl() + ;
       "6. Комплексное обслуживание"                 + endl() + ;
       getRsKasStr()                                 + endl() + ;
       "7. Продажа Облигаций ГСЗ СПб"                + endl() + ;
       "   Продажа     кол-во : " + padL( arr[ 11, 1], 4)    + ;
                    " сумма : " + mStr( arr[ 11, 2]) + endl() + ;
       "8. Б/н перечисления с текущих на текущие: "  + endl() + ;
       "                                 кол-во : "  + padL( arr[ 13, 1], 4) + endl() + ;
       "                                 сумма  : "  + mStr( arr[ 13, 2]) + endl() +    ;
       "9. Б/н перечисления с текущих на срочные: "  + endl() + ;
       "                                 кол-во : "  + padL( arr[ 14, 1], 4) + endl() + ;
       "                                 сумма  : "  + mStr( arr[ 14, 2]) + endl() +    ;
       "10.Б/н перечисления с текущих на карточ.: "  + endl() + ;
       "                                 кол-во : "  + padL( arr[ 15, 1], 4) + endl() + ;
       "                                 сумма  : "  + mStr( arr[ 15, 2]) + endl() +    ;
       "11.Б/н перечисления со срочных на текущ.: "  + endl() + ;
       "                                 кол-во : "  + padL( arr[ 16, 1], 4) + endl() + ;
       "                                 сумма  : "  + mStr( arr[ 16, 2]) + endl() +    ;
       "12.Б/н перечисления со срочных на карт. : "  + endl() + ; //  Add by TMV  20-05-2002
       "                                 кол-во : "  + padL( arr[ 19, 1], 4) + endl() + ; //  Add by TMV  20-05-2002
       "                                 сумма  : "  + mStr( arr[ 19, 2]) + endl() +    ; //  Add by TMV  20-05-2002
       "13.Б/н перечисления с карточ. на текущие: "  + endl() + ;
       "                                 кол-во : "  + padL( arr[ 17, 1], 4) + endl() + ;
       "                                 сумма  : "  + mStr( arr[ 17, 2]) + endl() +    ;
       "14. Продажа сервисных карт:" + endl() + ;
       showServCard( arr) + endl() + ;
       endl()

for i := 1 to 15 do Str += endl()
editMemo( Str)
if yesNo( "Печатать журнал операций ?")
   printAOrder( { Str})
endif
if !confUseAgent() .AND. round( kasRest, 2) <> round( lastRest, 2)
//if round( kasRest, 2) <> round( lastRest, 2)  //  Replace by TMV 05-10-2001
  if yesNo( { "Остаток по кассе не соответствует журналу операций",;
              "Остаток по кассе           - " + mStr( kasRest)    ,;
              "Остаток в журнале операций - " + mStr( lastRest)   ,;
              "Исправить остаток на " + mStr( lastRest) + " ?" })
     kasSetRest( "999", lastRest)     
  endif
endif

return NIL

static funcDef getPlatStr with arr:A local Str := "", i, s
  s := Select()
  if prov2Open()
     for i := 1 to len( arr[ 5])
       if PROV2 ->( dbSeek( arr[ 5][ i][ 1]))
          arr[ 5][ i][ 1] := PROV2 ->name
       endif
       Str += "   " + arr[ 5][ i][ 1]                  + ;
              " кол-во : " + padL( arr[ 5][ i][ 2], 4) + ;
              " сумма  : " + mStr( arr[ 5][ i][ 3])    + endl()
     next
     prov2Close()
     select (s)
  endif
return Str

static funcDef getRsKasStr local Str := "", cnt
  WISC ->( baseSavePos())
  WISC ->( dbSetOrder( 2))
  WISC ->( dbSeek( RS_BEG))
  Str := "   Оформлено кол-во : " + ;
         padL( downRsKasGet( RS_BEG, "Поиск заключенных договоров на РКО"), 4) + endl() + ;
         "   Закрыто   кол-во : " + ;
         padL( downRsKasGet( RS_END, "Поиск закрытых договоров на РКО"), 4) + endl() 
   WISC ->( baseRestPos())
return Str

static funcDef downRsKasGet with RSType:C, Str:C local cnt := 0
  WISC ->( dbSeek( RSType))
  statStartUp( Str)
  cnt := 0
  while WISC ->type == RSType .AND. WISC ->( !eof())
    cnt++
    skip 1 alias WISC
  enddo
  statExit()
return cnt

static funcDef addZabota with arr:A local i, Sum
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 3))
if CONTRACT ->( dbSeek( left( SUMS ->buffer, INTDOC_LEN)))
   i := aScan( arr[ 5], {|x| x[ 1] == CONTRACT ->code3}, 1)
   Sum := abs( SUMS ->summa/ 100)
   if i == 0
      aAdd( arr[ 5], { CONTRACT ->code3, 1, Sum})
   else
      arr[ 5][ i][ 2] += 1
      arr[ 5][ i][ 3] += Sum
   endif       
else
   messageB( "В справочнике долгосроных поручений нет договора " + ;
      left( SUMS ->buffer, INTDOC_LEN))
endif
CONTRACT ->( baseRestPos())
return NIL

static funcDef addPlat with arr:A ;
  local useNewKP := setupRead( "NEWKP"), aStr, i1, i, ptr, code3, Sum, cntPlat
  if WISC ->( !dbSeek( left( SUMS ->buffer, 20) + "977 "))
     messageB( "Не найдено поручение к записи # " + nStr( SUMS ->( recNo())))
     return NIL
  endif
  aStr := CtoA( WISC ->buffer, ";")
  i1   := val( aStr[ 2])
  ptr  := 3
  while i1 > 0
    code3 := aStr[ ptr]
    Sum   := val( aStr[ ptr + 2])
    cntPlat := max( 1, val( aStr[ ptr + 5]))
    i := aScan( arr[ 5], {|x| x[ 1] == code3}, 1)
    if i == 0
       aAdd( arr[ 5], { code3, cntPlat, Sum})
    else
       arr[ 5][ i][ 2] += cntPlat
       arr[ 5][ i][ 3] += Sum
    endif    
    ptr += 4
    ptr += 2 // внесено 07.06.2001
    if useNewKP then ptr += 5
    i1--
  enddo
return NIL


function addDown( arr, ptr, Sum)
static arrSchet := {}
  if pCount() = 0 
     arrSchet := {}
     return NIL
  endif  
  if ptr == 7
     if aScan( arrSchet, {|x| x == SUMS ->schet}) = 0
        aAdd( arrSchet, SUMS ->schet)
        arr[ ptr, 1] += 1
     endif
  else
     arr[ ptr, 1] += 1
  endif
  arr[ ptr, 2] += abs( Sum)
return NIL

funcDef addServCard with arr:A ;
  local bufArr, i, ptr, asCode, asName, arrCode, arrName

   if WISC ->( dbSeek( left( SUMS ->buffer, 20) + "975 "))
     bufArr := buf2Arr( WISC ->buffer)
     for i := 1 to len( bufArr)
       assGetInfo( bufArr[ i, 1], @arrCode, @arrName,, bufArr[ i, 2])
       ptr    := aTrimmedScan( arrCode, bufArr[ i, 3])
       asName := arrName[ ptr]
       ptr    := aScan( arr[ 18], {|x| x[ 1] == bufArr[ i, 1] .AND. ;
                                       x[ 2] == bufArr[ i, 2] .AND. ;
                                       x[ 3] == bufArr[ i, 3]}, 1)
       if ptr == 0
         aAdd( arr[ 18], { bufArr[ i, 1], bufArr[ i, 2], bufArr[ i, 3], asName, 0, 0})
         ptr := len( arr[ 18])
       endif
       arr[ 18, ptr, 5] += bufArr[ i, 4]
       arr[ 18, ptr, 6] += bufArr[ i, 5]
     next
   endif
return NIL

funcDef showServCard with arr:A local i, str := "", sSumma
  for i := 1 to len( arr[ 18])
    sSumma := str( arr[ 18, i, 6], 15, 2)
    sSumma := left( sSumma, 12) + "-" + right( sSumma, 2)
    str += space( 4) + padR( arr[ 18, i, 4], 30) + " -    кол-во: " + ;
           Str( arr[ 18, i, 5], 3) + "  сумма: " + sSumma + endl()
  next
return str

//  Add by TMV  25-09-2001
////////////////////////////////////
funcDef bookOpenAccount local rV := .T., tmp, i := 0, s := select(),;
	aH, aC, fk, theDate := setCurDate()

rV = getUp( 5, 5, {{ "Книга регистрации открытых счетов"},;
                   { "                              за ", block( theDate) }})
if !rV
	return rV
endif
	
ACC     ->( baseSavePos())
CLIENTS ->( baseSavePos())

tmp := { "tmp_book.dbf", "TMP",;
        {{ "data",  "D",  8, 0} ,;
         { "schet", "C", 20, 0} ,;
         { "code", "C", 12, 0} ,;
         { "name", "C", 20, 0} ,;
         { "sname", "C", 20, 0} ,;
         { "surname",   "C", 20, 0}},;
        {{, "right(schet,5)"}}, 0}

SUMS ->( netGoTop())
basePath( tmp, confPathTemp())
if baseCreate( tmp)
   if baseMXOpen( tmp)
      ACC     ->( dbSetOrder( 4))
      CLIENTS ->( dbSetOrder( 1))
      operStartUp( "Подготовка книги регистрации счетов", i := 1, ACC->(lastrec()))
		ACC->(dbgotop())
      do while ACC ->( !eof()) //.And. ACC->LEdited
			if ACC->DatOpen == theDate
				CLIENTS->(dbseek(ACC->Code))
				TMP ->( dbAppend())
				TMP ->data   := ACC->Datopen
				TMP ->code  := ACC ->code
				TMP ->schet  := ACC ->schet
				TMP ->surname:= CLIENTS->Surname
				TMP ->name   := CLIENTS->name
				TMP ->sname  := CLIENTS->Sname
			endif
			ACC->(dbskip())
			operProc( ++i)
      enddo
      operExit()
      TMP ->( netGoTop())
      if TMP ->( emptyRec())
         errLog( "Нет данных для показа")
      else
         aH := {{ "Книга регистрации счетов" } ,;
                { "за " + rDate( theDate)                                         } ,;
                { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"                       }}
         aC := {{ "Дата открытия", {|| TMP ->data}} ,;
                { "Счет клиента" , {|| splitSSchet( TMP ->schet)}} ,;
                { "ФИО клиента"  , {|| getFioAccounts()}} }
         fk := {,,,,,,,,;
                {{ "Печать", {|| prnBookOpenAcc( theDate)}} }}

         TMP ->( browseUp( 2, 2, aH, aC,;
                  {|| netSkipUp()   },;
                  {|| netSkipDown() },;
                  {|| netGoTop()    },;
                  {|| netGoBottom() },,,,, fk))
                  
      endif

      baseMClose( tmp)
   endif
   baseDelete( tmp)
endif

CLIENTS ->( baseRestPos())
ACC     ->( baseRestPos())
select ( s)
return rV

funcDef getFioAccounts
return padr(alltrim(TMP->Surname)+' '+alltrim(TMP->Name)+' '+alltrim(TMP->Sname), 50)

funcDef prnBookOpenAcc with theDate:D ;
        local i, total := 0, str, file, cnt := 0, useDon   ,;
              startCode, stopCode, str1, cntLines, cntPages,;
              useRoll, useFile, sHeader, aStr:={}, iStr

useDon   := confPrintDon()
useRoll := yesNo( "Печатать книги на рулон ?")
useFile := !useDon .AND. noYes( "Печатать в файл ?")
TMP ->( baseSavePos())
TMP ->( netGoTop())
file := newFPath( "book_.txt", confPathTemp())
if file( file) then fErase( file)
useStopDon( .F.)

sHeader := "Печать книги регистрации открытых счетов"
operStartUp( sHeader, i := 1, TMP ->( lastRec()) )


str := { if( !useDon, chr( 15), "") + "Банк " + confBankName()           ,;
         confPostName(), ""                                ,;
         "Книга регистрации открытых счетов" ,;
         "за " + rDateFull( theDate) }

cntPages := 1
cntLines := 14
fillHdrBook( @str, useDon, @cntPages)
if useDon
   if file( getDonName()) then fErase( getDonName())
   setAltOn()
   ? "&4" + chr( 27) + chr( 33) + chr( 1) + chr( 10) + chr( 13)
   for i := 1 to len( str) do ? str[ i]
   setAltOff()
endif

while TMP ->( !eof())
      if cntLines >= 55
         cntLines := 8
         if useDon then str := {}
//			aAdd( str, " --------------------------------------------------------------------------------------------------------------------------")
         if !useDon .AND. !useRoll then aAdd( str, chr( 12))
         fillHdrBook( @str, useDon, @cntPages)
      endif
/*
     "|Дата    |Дата и   |   Наименование    |Наименование|    Номер счета     |  Порядок и  |Дата сообщения|  Дата  |Примеча-|")
	  				   Б/н						       текущий счет                      не позднее     не требуется
					  от "__"					       в валюте РФ                       рабочего дня,
					 _________  				       по учету                          следующего за
					  _____г.					       социальных                        днем проведе-
													       выплат физи-                      ния операции
													       ческим лицам
													       -резидентам
*/
aStr:={}
aadd(aStr, '|'+dtoc(TMP->data)+'|    Б/н   |'+TMP->Surname+'|текущий счет|'+;
	TMP->Schet+'|не позднее   | не требуется |        |        |')
aadd(aStr, '|          |    от    |'+TMP->Name+'|в валюте РФ |                    |рабочего дня,|              |        |        |')
aadd(aStr, '|          |'+dtoc(TMP->data)+'|'+TMP->SName+'|по учету    |                    |следующего за|              |        |        |')
aadd(aStr, '|          |          |'+space(20)+'|социальных  |                    |днем проведе-|              |        |        |')
aadd(aStr, '|          |          |'+space(20)+'|выплат физи-|                    |ния операции |              |        |        |')
aadd(aStr, '|          |          |'+space(20)+'|ческим лицам|                    |             |              |        |        |')
aadd(aStr, '|          |          |'+space(20)+'|-резидентам |                    |             |              |        |        |')
aAdd(aStr, "|---------------------------------------------------------------------------------------------------------------------------|")
      if useDon
         setALtOn()
			for iStr=1 to len(aStr)
				?aStr[iStr]
			next
//         ? str1
         setAltOff()
      else
			aEval( aStr, {|x|aAdd( str, x)})
//         aAdd( str, str1)
         if len( str) > 200
            if useFile
               eFileWrite( str, file)
            else
               printAOrder( str)
            endif
            str := {}
         endif
      endif
      cntLines+=7
      cnt++
      skip 1 alias TMP
      operProc( i++)
enddo

str1 := { if( cntLines > 55 .AND. !useRoll , chr( 12), ""),;
	if( !useDon, chr( 18), ""),;
	if( !useDon .AND. !useRoll, "", chr( 12))}


if useDon
	setAltOn()
   for i := 1 to len( str) do ? str1[ i]
   ? chr( 10) + chr( 13) + chr( 12) + chr( 10) + chr( 13)
   setAltOff()
   runExe( getDonExeName())
else
   for i := 1 to len( str1) do aAdd( str, str1[ i])
   //
   // если печать идет на рулон, то выведем еще строк
   //
   if useRoll then for i := 1 to 15 do aAdd( str, "")
   if useFile
      eFileWrite( str, file)
   else
      printAOrder( str)
   endif
   str := {}
endif

operExit()
useStopDon( .T.)
TMP ->( baseRestPos())

if useFile then viewTemps( file)

return NIL

static funcDef fillHdrBook with str:A, useDon:L, cntPages:N
aAdd( str, "")
aAdd( str, "Страница : " + Str( cntPages++))
aAdd( str, " ---------------------------------------------------------------------------------------------------------------------------")
aAdd( str, "|   Дата   | Дата и   |    Наименование    |Наименование|    Номер счета     |  Порядок и  |Дата сообщения|  Дата  |Примеча-|")
aAdd( str, "| открытия |номер до- |      клиента       |(цель) счета|                    |периодичность|налоговым орга|закрытия|  ния   |")
aAdd( str, "|  счета   |говора об |                    |            |                    |   выдачи    |нам, фондам об| счета  |        |")
aAdd( str, "|          |открытии  |                    |            |                    |  выписок    |   открытии   |        |        |")
aAdd( str, "|          |  счета   |                    |            |                    |             |    счета     |        |        |")
aAdd( str, "|---------------------------------------------------------------------------------------------------------------------------|")
return NIL
//
// формирование кассового журнала
//
funcDef makeReestr with theDate:D, nOrder:N ;
                    local aH, aC, fk, tmp, total, i,;
                    theRo, theSim, s, cnt, aCode, sBicBank:=confBic()

s := select(); total := 0.00; cnt := 0
ACC     ->( baseSavePos())
CLIENTS ->( baseSavePos())
SUMS    ->( baseSavePos())

tmp := { "tmp_ree.dbf", "TMP",;
        {{ "met",   "C",  1, 0} ,;
         { "ro",    "C",  2, 0} ,;
         { "nDoc",  "N",  4, 0} ,;
         { "schet", "C", 20, 0} ,;
         { "sim",   "C",  2, 0} ,;
         { "summa", "N", 15, 2} ,;
         { "fio",   "C", 40, 0}},;
        {{, "nDoc"}}, 0}

SUMS ->( netGoTop())
basePath( tmp, confPathTemp())
if baseCreate( tmp)
   if baseMXOpen( tmp)
      ACC     ->( dbSetOrder( 1))
      CLIENTS ->( dbSetOrder( 1))
      operStartUp( "Подготовка реестра переводов", i := 1, SUMS ->( lastRec() ))
      while SUMS ->( !eof())
        if SUMS ->dDate == theDate .AND. ;
		  	  SUMS ->typeOf == "7100" .And. ;
           if(nOrder == 1, !isWiscBik(SUMS->Schet, SUMS->nDoc, sBicBank),;
			  		isWiscBik(SUMS->Schet, SUMS->nDoc, sBicBank))
//           if(nOrder == 1, SUMS ->typeOf == "6140", SUMS ->typeOf == "5010")

           operGetInfo( SUMS ->typeOf, @theRo, @theSim)
           TMP ->( dbAppend())
           TMP ->met   := '*'
           TMP ->ro    := theRo
           TMP ->nDoc  := SUMS ->nDoc
           TMP ->schet := SUMS ->schet
           TMP ->sim   := theSim
           TMP ->summa := abs( SUMS ->summa/ 100)
           TMP ->fio   := getFioFromSums()
           total       += TMP ->summa
           cnt++
        endif
        skip 1 alias SUMS
        operProc( i++)
      enddo
      operExit()

      TMP ->( netGoTop())
      if TMP ->( emptyRec())
         errLog( "Нет данных для показа")
      else
         aH := {{ "Реестр перевода средств " +;
					  if(nOrder == 1, "в другой банк", "на счет в филиале")} ,;
                { "за " + rDate( theDate)                   } ,;
                { "Документов - " + nStr( cnt)              } ,;   
                { "Итого по документу  " + cStr( total)     } ,;
                { "Клиент ", block( TMP ->fio),,{||.F.}     } ,;
                { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀" }}
         aC := {{ "Печать",          {|| TMP ->met       }} ,;
                { "Род опер",        {|| TMP ->ro        }} ,;
                { "N док",           {|| TMP ->nDoc      }} ,;
                { "Счет клиента",    {|| splitSSchet( TMP ->schet)}} ,;
                { "Символ",          {|| TMP ->sim       }} ,;
                { "Сумма",           {|| TMP ->summa     }} ,;
                { "Сумма документа", {|| TMP ->summa     }} }
         fk := {,,,,,,,,;
                {{ "Печать", {|| printReestr( theDate, nOrder)}} }}

         setKey( K_SPACE, {|| setJournOf() })  //  Add by TMV  22-11-2001

         TMP ->( browseUp( 2, 2, aH, aC,;
                  {|| netSkipUp()   },;
                  {|| netSkipDown() },;
                  {|| netGoTop()    },;
                  {|| netGoBottom() },,,,, fk))

         setKey( K_SPACE, NIL)                //  Add by TMV  22-11-2001
                  
      endif

      baseMClose( tmp)
   endif
   baseDelete( tmp)
endif

SUMS    ->( baseRestPos())
CLIENTS ->( baseRestPos())
ACC     ->( baseRestPos())
select ( s)
return NIL

//  Add by TMV 23-10-2001
funcDef setJournOf
do while !TMP->(rlock())  //  Add by TMV 23-10-2001
	inkey(.2)
enddo
if empty(TMP ->met)
   TMP ->met := '*'
else
   TMP ->met := ' '
endif
TMP->(dbunlock())         //  Add by TMV 23-10-2001
// TMP ->( netSkipDown())
getCurOB(): Down()
getCurOB(): refreshAll(): forceStable()
return NIL
//  END of Add by TMV 23-10-2001

//  Add by TMV 19-04-2002
funcDef setJrnOnAll local nRecTmp := TMP->(recno()),;
	rV := .F., aMask := {' 42301 ', ' 42309 '},;  // Add by TMV  12-08-2003
	sMask := '', n := 1             // Add by TMV  12-08-2003

// Add by TMV  12-08-2003

do while !TMP->(EOF())
	if empty(sMask) .Or. Left( allTrim(TMP->SCHET), 5) == sMask  // Add by TMV  12-08-2003
		do while !TMP->(rlock())
			inkey(.2)
		enddo
		if empty(TMP ->met)
   		TMP ->met := '*'
		endif
		TMP->(dbunlock())
	endif             // Add by TMV  12-08-2003
	TMP->(dbskip())
enddo
TMP->(dbgoto(nRecTmp))
getCurOB(): refreshAll(): forceStable()
return NIL
//  END of Add by TMV 19-04-2002

//  Add by TMV 19-04-2002
funcDef setJrnOffAll local nRecTmp := TMP->(recno()),;
	rV := .F., aMask := {' 42301 ', ' 42309 '},;  // Add by TMV  12-08-2003
	sMask := '', n := 1             // Add by TMV  12-08-2003

// Add by TMV  12-08-2003

do while !TMP->(EOF())
	if empty(sMask) .Or. Left( allTrim(TMP->SCHET), 5) == sMask  // Add by TMV  12-08-2003
		do while !TMP->(rlock())
			inkey(.2)
		enddo
		if !empty(TMP ->met)
	   	TMP ->met := ' '
		endif
		TMP->(dbunlock())
	endif             // Add by TMV  12-08-2003
	TMP->(dbskip())
enddo
TMP->(dbgoto(nRecTmp))
getCurOB(): refreshAll(): forceStable()
return NIL
//  END of Add by TMV 19-04-2002

//  Add by TMV 23-04-2002
funcDef seekJournal local nRecTmp := TMP->(recno()), buffer := 1, rV := .F.

if getUp( 5, 5, {{ "Номер док-та :", block( buffer) }})
	messageStartUp( "Поиск док-та " + Str(buffer, 6) + ". Подождите !!!")
	rV := TMP ->( dbSeek( buffer))
	messageExit( 0)
	if rV
		getCurOB(): refreshAll(): forceStable()
	else
		TMP->( dbGoTo( nRecTmp))
		messageB( "Документ номер " + Str(buffer, 6) + " не найден !!!")
	endif
endif

return NIL
//  END of Add by TMV 23-04-2002


funcDef printReestr with theDate:D, nOrder:N ;
	local i, total := 0, str, file, cnt := 0, useDon   ,;
   	startCode, stopCode, str1, cntLines, cntPages,;
      useRoll, useFile, sHeader, aSumma := {}


useDon   := confPrintDon()
useRoll := yesNo( "Печатать реестр на рулон ?")
useFile := !useDon .AND. noYes( "Печатать в файл ?")
TMP ->( baseSavePos())
TMP ->( netGoTop())
file := newFPath( "rees_" + if( nOrder == 1, "c", "d") + ".txt", confPathTemp())
if file( file) then fErase( file)
useStopDon( .F.)

sHeader := "Печать " + if( confUseAgent(), "реестра", "кассового журнала")
operStartUp( sHeader, i := 1, TMP ->( lastRec()) )

str := {if( !useDon, chr( 15), "") + confBankName() + " " + confFilial()  ,;
        confPostName(), ""                                    ,;
        "Реестр средств, подлежащих переводу        " +;
		  if( nOrder == 1, "внешний", "внутренний"),;
        "за " + rDateFull( theDate), ""}

cntPages := 1
cntLines := 14
filHeadReestr( @str, @cntPages)
if useDon
   if file( getDonName()) then fErase( getDonName())
   setAltOn()
   ? "&4" + chr( 27) + chr( 33) + chr( 1) + chr( 10) + chr( 13)
   for i := 1 to len( str) do ? str[ i]
   setAltOff()
endif

while TMP ->( !eof())
	if !empty(TMP->met)
      if cntLines >= 55
         cntLines := 8
         if useDon then str := {}
			aAdd( str, " -------------------------------------------------------------------------------")
         if !useDon .AND. !useRoll then aAdd( str, chr( 12))
         filHeadReestr( @str, @cntPages)
      endif
      str1 := "|" + TMP ->schet          + "|" + TMP ->fio           + ;
              "|" + padL( TMP ->nDoc, 4) +; // "|" + padC( TMP ->sim, 3) + ;
              "|" + padL( TMP ->summa, 11, 2)  + "|"
      if useDon
         setALtOn()
         ? str1
         setAltOff()
      else
         aAdd( str, str1)
         if len( str) > 200
            if useFile
               eFileWrite( str, file)
            else
               printAOrder( str)
            endif
            str := {}
         endif
      endif
      cntLines++
      cnt++
      total += TMP ->summa
	endif
   skip 1 alias TMP
   operProc( i++)
enddo

aSumma := razbivka(rSumma( total), {{50, "L"}, {50, "L"}, {50, "L"}})
str1 := {" -------------------------------------------------------------------------------",;
			if( cntLines > 55 .AND. !useRoll , chr( 12), ""),;
         "", "ИТОГО             " + padL( total, 15, 2), "",;
         "Общее кол-во документов - " + nStr( cnt) + " на сумму "}
for i=1 to len(aSumma) do aadd(str1, aSumma[i])
aadd(str1, "Ответственный исполнитель ______________________________")
aadd(str1, if( !useDon, chr( 18), "") + if( !useDon .AND. !useRoll, "", chr( 12)))


if useDon
   setAltOn()
   for i := 1 to len( str) do ? str1[ i]
   ? chr( 10) + chr( 13) + chr( 12) + chr( 10) + chr( 13)
   setAltOff()
   runExe( getDonExeName())
else
   for i := 1 to len( str1) do aAdd( str, str1[ i])
   //
   // если печать идет на рулон, то выведем еще строк
   //
   if useRoll then for i := 1 to 15 do aAdd( str, "")
   if useFile
      eFileWrite( str, file)
   else
      printAOrder( str)
   endif
   str := {}
endif


operExit()
useStopDon( .T.)
TMP ->( baseRestPos())

if useFile then viewTemps( file)
return NIL

static funcDef filHeadReestr with str:A, cntPages:N
aAdd( str, "")
aAdd( str, "Страница : " + Str( cntPages++))
aAdd( str, " ------------------------------------------------------------------------------")
aAdd( str, "|Номер лицевого счета|                 Плательщик             |N   |   Сумма   |")
aAdd( str, "|                    |                                        |расп| документа |")
aAdd( str, "|------------------------------------------------------------------------------|")

return NIL