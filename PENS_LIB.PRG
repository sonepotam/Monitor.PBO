*============================================================================
*                                                                          
*               Программа : MON.EXE
*               Файл      : pens_lib.prg
*               Автор     : Рудницкий А.В.
*
*               Описание  : Ордера на взнос наличными и расходный ордер
*
*               Автор MON.EXE выражает глубокую благодарность за
*               сэкономленое  время  на прописывание этого !!!!!
*
*---------------------------------------------------------------------------

* 25-10-2002  TMV
* Для НЕ Владика оставлен старый вариант РКО
*
* 27-11-2002  TMV
* Добавлен новый док-т - поручение на переч-е для выдачи кредита по новой схеме
* Ф-ия memKred()
* В ф-ии post_order() для НЕпенсионных счетов обнуляется переменная name,
*  которая печатается в 3-ей строке адреса.
*
* 05-12-2002  TMV
* Изменены данные в форме поручения на прием ср-в - vznos_nal()
*
* 18-02-2003  TMV
* К названию филиала в мем. ордере добавлено название банка. Ф-ия memOrd2().
*
* 12-03-2003  TMV
*
* 25-09-2003  TMV
* При переводе с 42309 на 42301 сделана печать 
* "Правила обслуживания вкладов до востребования 'Пенсионный'"
* Ф-ия prnTran423009(), новая ф-ия rullServVclad()
*
* 29-06-2004  TMV
* Вместо "зашитого" расходного ордера сделан шаблон RKO3 и поправлен
*  расходный ордер в других операциях.
* Ф-ия Post_Order(), kasRKO().
*
* 29-07-2004  TMV
* Изменения карточных счетов с 42308 на 42301 по указанию ЦБ.
* Новая ф-ия печати уведомлений pCrdOldPrint().
*
* 02-12-2004 TAT 
* post_order()
* kasPKO2()
* kasPKO()
* vznos_nal()
* (810)->(643)

#include "clippExt.ch"
#include "common.ch"

#include "mon.ch"

//Печать ордера по шаблону
//Номер ордера       0 - 999999 (если "0",то подставляем пустую строку)
//Дата в ордере                 (если пустая, то печатаем ордер без даты)
//Фамилия если больше 37 символов, то на две строчки
//Адрес если больше 37 символов, то на две строки
//Сумма - в рублях
//Если паспорт есть, то yes_pas = .T., иначе pas_ser, pas_num, pas_mil
//содержат строки, которые без изменения попадут в ордер
function post_order(nd,yes_date,date,fio,adr,schet,sum,nazn,sim,rod,;
                         yes_pasp, docName, trustFio, trustAddr)
local fio1, fio2, adr1, adr2,;
 pas1, pas2, pas3, pas4, name, aDoc,;
 strSum, sum1, sum2, sum3,;
 text, i, kasSchet := confKassa(),;
 kassa, arr, str, usePens := .F.,;
 pasSer1 := '', pasSer2 := '', apasSer := {'','',''},;  // Add by TMV 29-06-2004
 pasGived := '', dest := ''  // Add by TMV 29-06-2004

DEFAULT trustAddr TO ""
DEFAULT trustFIO  TO ""

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 1))
ACC ->( dbSeek( schet))
usePens := isPens( ACC ->PRIZ)
ACC ->( baseRestPos())

if confUseAgent()
  if isBlankOrder()
    arr := {}
  else
    fio1 := razbivka( fio, {{ 35, "L"}, { 35, "L"}})
    sum1 := razbivka( rSumma( sum), {{ 63, "L"}, { 78, "L"}})
	 sum1[1] = Upper(SubStr( sum1[1], 1, 1)) + SubStr( sum1[1], 2)
    adr1 := razbivka( adr, {{ 42, "L"}, { 42, "L"}})
//  Edited by TMV  25-04-2002
    name = rTrim( fio) + " Счет N " + schet + " " +;
	 	confBankName() + " " + confFilial()
//  Edited by TMV  25-04-2002  --  End
    name := razbivka( name, {{ 67, "L"}, { 78, "L"}})
    if allowTrust()
      docName := rTrim( nazn) + " " + docName
      pas1    := razbivka( docName, {{ 78, "L"}, { 78, "L"}, { 78, "L"}, { 78, "L"} })
      fio1    := razbivka( trustfio,  {{ 35, "L"}, { 35, "L"}})
      adr1    := razbivka( trustaddr, {{ 42, "L"}, { 42, "L"}})
    else
      if usePens
        pas2 := razbivka( "Предъявлен " + docName, {{ 78, "L"}, { 78, "L"}})
      else
        pas2 := razbivka( docName, {{ 78, "L"}, { 78, "L"}})
      endif
      pas1 := { nazn, "", pas2[ 1], pas2[ 2]}
    endif
    if !usePens
      adr1[ 1] := adr1[ 2] := ""
    endif


    arr  := {{ "@docn",     padL( nD, 6, "0")              },;
             { "@date",     if( yes_date, rDate( date), '"____"______________ 200__г.')},;
             { "@fio1",     fio1[ 1]                       },;
             { "@fio2",     fio1[ 2]                       },;
             { "@curplace", confNodeName() + ', ' + confOtdName()},;
             { "@sum",      toDefSum( sum), "L"            },;
             { "@sum1",     sum1[ 1]                       },;
             { "@sum2",     sum1[ 2]                       },;
             { "@addr1",    adr1[  1]                      },;
             { "@addr2",    adr1[  2]                      },;
             { "@client1",  name[ 1]                       },;
             { "@client2",  name[ 2]                       },;
             { "@dest1",    pas1[ 1]                       },;
             { "@dest2",    pas1[ 2]                       },;
             { "@dest3",    pas1[ 3]                       },;
             { "@dest4",    pas1[ 4]                       }}
  endif
else
  schet = schet20( allTrim( schet))
  nd    = if( nd = 0, space( 6), padl(nd,6,"0"))
  date  = if( yes_date, padR( allTrim( rDate(date)), 34, " "), padr('"____"______________ 200__г.', 34, ' ')) // TMV  21-09-2001
//  fio   := razbivka( allTrim( fio), {{ 34, "L"}, { 34, "L"}}) // TMV  21-09-2001
  fio   := razbivka( allTrim( fio), {{ 64, "L"}, { 64, "L"}})
  fio1  := fio[ 1]; fio2  := fio[ 2]

  strSum = if( isBlankOrder(), "", allTrim( rSumma( sum)))
  strSum = upper( subStr( strSum, 1, 1)) + substr( strSum, 2)
  sum    = if( isBlankOrder(), "", allTrim( mStr( sum)))
  strSum += ' (643)'  // Add by TMV 02-07-2004  //02-12-2004 TAT (810)->(643)

  sum2=space(54); sum3=space(54)
  if len(strSum) <= 54
    sum1=padr(strSum,54," ")
  else
    i=rat(" ",substr(strSum,1,54))
    if i = 0
      i=54
    endif
    sum1=padr(substr(strSum,1,i-1),54," ")
    strSum=substr(strSum,i+1)
    if len(strSum) <= 54
      sum2=padr(strSum,54," ")
    else
      i=rat(" ",substr(strSum,1,54))
      if i = 0 then i := 54
      sum2=padr(substr(strSum,1,i-1),54," ")
      sum3=padr(substr(strSum,i+1),54," ")
    endif
  endif

  if isBlankOrder()
     rod  = sim := "  "
     name = Space( 34)
  else
     sim   =padl(sim,2,"0")
     rod   =padl(rod,2,"0")
     name  = padR( confOtdName(), 34, " ")
  endif
  adr  = allTrim( adr) // + ". " + confOtdName()
  Adr  := razbivka( allTrim( adr), {{ 34, "L"}, { 34, "L"}, {34, "L"}})
  adr1 = adr[ 1]; adr2  = adr[ 2]
  name = adr[ 3]   // Remove from Down by TMV  27-11-2002

  //
  // для непенсионных счетов не должно быть адреса клиента
  //
  if !usePens then adr1 := adr2 := name := space( 34) // Add <name> 27-11-2002
  
     
//  name := adr[ 3] // Remove Up by TMV  27-11-2002
  nazn := alltrim(nazn)
  if allowTrust()
   docName := rTrim( nazn) + " " + docName
// Edit by TMV  25-10-2002
   aDoc    := razbivka( docName, {{ 70,"L"}, { 70,"L"}, { 70,"L"}, { 70,"L"}})
	pasSer1 = subStr(docName, Len(rTrim( nazn))+1)
	pasSer1 = Left(pasSer1, at("выдан", pasSer1) - 1)   // Add by TMV 29-06-2004
   apasSer := razbivka( pasSer1, {{ 60,"L"}, { 60,"L"}, { 60,"L"}})
	pasSer1 = apasSer[1]
	pasGived = right(docName, Len(docName) - at("выдан", docName)-5)   // Add by TMV 29-06-2004
   pas1    = aDoc[ 1]
   pas2    = aDoc[ 2]
   pas3    = aDoc[ 3]
   pas4    = aDoc[ 4]
  else
//   aDoc    := razbivka( docName, {{ 54,"L"}, { 54,"L"}, { 54,"L"}})
// Edit by TMV  25-10-2002
   aDoc    := razbivka( docName, {{ 70,"L"}, { 70,"L"}, { 70,"L"}})
   pas1    = padR( nazn, 54)
   pas2    = aDoc[ 1]
   pas3    = aDoc[ 2]
   pas4    = aDoc[ 3]
	pasSer1 = getDocumName() + ' ' +allTrim(cliPasSer1()) + " " + allTrim(cliPasSer2()) + " N " + cliPasNum()   // Add by TMV 29-06-2004
	pasGived = cliPasGiv()   // Add by TMV 29-06-2004
  endif
  kassa = confBankName() + ' ' + rTrim( confFilial())
//  Edited by TMV  25-04-2002
  if usePens .OR. left( kasSchet, 5) <> "20202" then ;
     kassa += ' ПБО ' + confOtdNumb()  
//  Edited by TMV  25-04-2002  --  End

//  kassa := razbivka( kassa, {{ 34, "L"}, { 34, "L"}})
  sum = padL( strTran( sum, ".", ""), len( sum))
  kassa := razbivka( kassa, {{ 54, "L"}, { 54, "L"}})
//  TMV 21-09-2001
    arr  := {{ "@docN",     padL( nD, 6, "0")              },;
             { "@rDate",     date},;
             { "@curDest1",     fio1                       },;
             { "@curDest2",     fio2                       },;
             { "@debSchet",     schet                      },;
             { "@credSchet",    kasSchet                   },;
             { "@kassa1", kassa[1]},;
             { "@kassa2", kassa[2]},;
             { "@sko",         sim                         },;
             { "@summa",       sum                         },;
             { "@rSumma1",     sum1                        },;
             { "@rSumma2",     sum2                        },;
             { "@rSumma3",     sum3                        },;
;// Replaced by TMV  29-06-2004
;//             { "@dest",        pas1                        },;
             { "@dest",     if(empty(dest), pas1, dest) },;
             { "@pass1",    pas2                       },;
             { "@pass2",    pas3                       },;
             { "@pass3",    pas4                       },;
				 { "@bik",      confBic()                  },;  // Add by TMV 29-06-2004
				 { "@pasSer1",  pasSer1                    },;  // Add by TMV 29-06-2004
				 { "@pasSer2",  apasSer[2]                 },;  // Add by TMV 29-06-2004
				 { "@pasSer3",  apasSer[3]                 },;  // Add by TMV 29-06-2004
				 { "@pasGived", pasGived                   }}   // Add by TMV 29-06-2004
//             { "@client1",  name[ 1]                       },;
//             { "@client2",  name[ 2]                       },;
//  TMV 21-09-2001 // End
endif
//123456789012345678901234567890123456789012345678901234567890123456789012345678

altD()

if confUseAgent()
  str := ExtractStrOpen( "post")
//  str := ExtractStrOpen( "rko_new")  //  TMV  20-09-2001
  str  := makeZ( str, arr)
  text := CtoA( str, endl())
else

  str := ExtractStrOpen( "rko_new")  //  TMV  20-09-2001
  str  := makeZ( str, arr)
  text := CtoA( str, endl())
endif
return text


function razbivka(stroka,_arr)
local text:={}, max:=0
local i, str1, ind

for i:=1 to len(_arr)
  max+=_arr[i][1]
next

if len(stroka) > max
   stroka:=substr(stroka,1,max)
endif


for i:=1 to len(_arr)
  stroka:=alltrim(stroka)
  if len(stroka) <= _arr[i][1]
    str1=stroka
    stroka:=""
  else
    ind:=rat(" ",substr(stroka,1,_arr[i][1]))
    if ind = 0
      str1=substr(stroka,1,_arr[i][1])
      stroka:=substr(stroka,_arr[i][1]+1)
    else
      str1=substr(stroka,1,ind-1)
      stroka:=substr(stroka,ind+1)
    endif
  endif
  if upper(_arr[i][2]) == "L"
    str1=padr(str1,_arr[i][1]," ")
  elseif upper(_arr[i][2]) == "R"
    str1=padl(str1,_arr[i][1]," ")
  elseif upper(_arr[i][2]) == "C"
    str1=padc(str1,_arr[i][1]," ")
  else
    str1=alltrim(str1)
  endif
  aadd(text, str1)
next
return text


autofunction setValue useToMen init .F.


//
// Приходный ордер при приеме ПБО подкрепления, получ. из банка
//
funcDef kasPKO2 ;
   with docNumb:N, from:C, curDate:D, debSchet:C, credSchet:C, summa:N, ;
        sim:C, ro:C, curReci:C, curDest:C ;
   local rSumma, filial, text, strSumma


//debSchet := "сч. N " + left( debSchet, 5)
rSumma   := rSumma( Summa)
docNumb  := padL( docNumb, 6, "0")
strSumma := padC( lTrim( strTran( mStr( Summa), ".", "")), 17)
from     := razbivka( from, {{ 37, "L"}, { 37, "L"}, { 37, "L"}})
// Replaced by TMV  24-12-2001
filial   := razbivka( rTrim( confBankName()) + ", " + confFilial(), {{ 19, "L"}, { 35, "L"}, { 35, "L"}})
curReci  := razbivka( curReci, {{ 39, "L"}, { 39, "L"}})

curDest  := razbivka( curDest, {{ 60, "L"}, { 60, "L"}})
rSumma   := razbivka( rSumma( Summa) +;
	' (643)',;  // Add by TMV 02-07-2004   //02-12-2004 TAT (810)->(643)
	{{ 60, "L"}, { 60, "L"}, { 60, "L"}})
rSumma[1] = Upper(SubStr( rSumma[1], 1, 1)) + SubStr( rSumma[1], 2)

text := extractStrOpen( "PKO2")
text := makeZ( text, {{ "@docN",      docNumb       },;
                      { "@rDate",     rDate(curDate)},;
                      { "@fio1",      from[ 1]      },;
                      { "@fio2",      from[ 2]      },;
                      { "@fio3",      from[ 3]      },;
                      { "@filial1",   filial[ 1]    },;
                      { "@filial2",   filial[ 2]    },;
                      { "@filial3",   filial[ 3]    },;
                      { "@credSchet", credSchet     },;
                      { "@debSchet",  debSchet      },;
                      { "@reci1",     curReci[ 1]   },;
                      { "@reci2",     curReci[ 2]   },;
                      { "@summa",     strSumma      },;
                      { "@summa1",    padL( allTrim( strSumma), 12)},;
                      { "@s1",        sim           },;
                      { "@ro",        ro            },;
                      { "@rSumma1",   rSumma[ 1]    },;
                      { "@rSumma2",   rSumma[ 2]    },;
                      { "@nazn1",     curDest[ 1]   },;
                      { "@nazn2",     curDest[ 2]   }})
text := CtoA( text, endl())
return text



//
// просто приходный ордер ( кассовый )
//
function kasPKO(   nd,;     // - номер документа
                   dat,;    // - дата
                   fr,;     // - от кого принят взнос
                   acc,;    // - счет зачисления
                   sum,;    // - сумма цифрами
                   men,;    // - получатель
                   ab,;     // - назначение взноса
                   bal_cas,;// - дебетовый счет ( счет кассы )
                   sim,;    // - символ кассовой отчетности
                   isKassa,;// - подкрепление кассы
                   i)       // - номер экземпляра

local text:={}    // Это мы и должны вернуть - бланк для взноса наличными
local the_date, ; // 58 - дата (буквами)
      from,     ; // 40 - от кого принят взнос
      to_acc,   ; // 14 - счет зачисления
      to_bank,  ; // 36 - банк получателя
      the_sum,  ; // 16 - сумма цифрами
      to_men,   ; // 46 - получатель
      sum_pr,   ; // 78 - сумма прописью, возможно поделенная на две части
      about,    ; // 60 - назначение взноса
      deb,      ; // дебетовый счет  ( счет кассы ) - балансовая часть
      prop,     ; // сумма прописью
      simb,     ; 
      arr, bank, cSum


nD   := padl(nd,6,"0")
from := razbivka( fr, {{ 38, "L"}, { 38, "L"}, { 38, "L"}} )

to_acc := schet20( allTrim( acc))
to_bank:=padr( alltrim( confOtdName()),34," ")

//сумма под Митюгова
the_sum:=padl( allTrim( strTran( mStr( sum), ".", "")), 14," ")
//Изменено 03.05.2001 в 18:04, Залманов А.З.
cSum := padL( strTran( allTrim( mStr( sum )), ".", "" ), 13 )
//
if useToMen()
   to_men:= men
else
   to_men := confBankName() + ", " + rTrim( confFilial())
// Replaced by TMV  16-10-2001
   if left( bal_cas, 5) <> "20202" then to_men += ", ПБО " + rTrim( confOtdNumb())
endif
to_men := razbivka( allTrim( to_men), {{ 39, "L"}, { 39, "L"}})

//сумма прописью
sum_pr:=razbivka(rSumma(sum) +;
	' (643)',;  // Add by TMV 02-07-2004    //02-12-2004 TAT (810)->(643) 
	{{ 60, "L"},{ 60, "L"}, { 60, "L"}} )
sum_pr[1] = Upper( SubStr( sum_pr[1], 1, 1)) + SubStr( sum_pr[1], 2)
//назначение платежа
about := razbivka( ab, {{ 60, "L"}, { 60, "L"} })
//дебетовый счет
//
simb:=padc(sim,3," ")
bank := razbivka(confBankName() + ", " + confFilial(), {{ 19, "L"}, { 35, "L"}})
//
// просто ордер без квитанции
//
arr := {{ "@docN" ,    nD            },;
        { "@rDate",    rDate( dat)   },;
        { "@fio1",     from[ 1]      },;
        { "@fio2",     from[ 2]      },;
        { "@fio3",     from[ 3]      },;
        { "@debSchet", bal_cas       },;
        { "@credSchet",acc           },;
        { "@summa",    the_Sum       },;
        { "@bank1",    bank[ 1]      },;
        { "@bank2",    bank[ 2]      },;
        { "@reci1",    to_men[ 1]    },;
        { "@reci2",    to_men[ 2]    },;
        { "@summa1",   cSum          },;
        { "@s1",       simb          },;
        { "@ro",       9             },;
        { "@rSumma1", sum_pr[ 1]     },;
        { "@rSumma2", sum_pr[ 2]     },;
        { "@rSumma3", sum_pr[ 3]     },;
        { "@nazn1",   about[ 1]      },;
        { "@nazn2",   about[ 2]      } }

text := extractStrOpen( "pko4")
text := makeZ( text, arr)
text := CtoA( text, endl())

/*
//         012345678901234567890123456789012345678901234567890
// for i := 1 to 3
aadd(text,"----------------------------------------------------------------------------")
aadd(text,"         ПРИХОДНЫЙ КАССОВЫЙ ОРДЕР   N  |      "+nd+"     | |Форма  0482008 |") //+ Str( i,1) + "экз")
aadd(text,"                                       ------------------- -----------------")
aadd(text,                     the_date                            +"                     ")
aadd(text,"")
// aadd(text,"Сумма " + the_Sum + "                           ДЕБЕТ                          ")
   aadd(text,Space( 20)         + "                           ДЕБЕТ                          ")
aadd(text,"От кого " + from[ 1] +                  "     --------------------------------")
aadd(text,             from[ 2]                    +"|"  +  deb +"|"+the_sum+   "     |")
aadd(text,"                                           ------------|     Общая         |")
aadd(text,"                                               КРЕДИТ  |                   |")

// aadd(text,'Банк получателя  АБ "ПЕТРОВСКИЙ",' + padR( confFilial(), 22)+ ' -------------------|')
//                            01234567890123456
// aadd(text,'                             |код |                    |    частные    |код|')
aadd(text,'Банк получателя ' + padR( confBankName() + ",", 39)+ ' -------------------|')
aadd(text, padR( confFilial(), 29) + '|код |                    |    частные    |код|')
aadd(text,"|---------------------------------|                    |-------------------|")
aadd(text,"Получатель                        |"     +  to_acc +  "|"+the_sum+" |"+simb+"|")
aadd(text, to_men[1]                       +"|                    |-------------------|")
aadd(text, to_men[2]                       +"|                    |               |   |")
aadd(text,"                                  |                    |-------------------|")
aadd(text,"-----------------                 |                    |               |   |")
aadd(text,"|Код            |                 |NN сч.              |               |   |")
aadd(text,"---------------------------------------------------------------------------|")
            //                                012345678901234567890
aadd(text,"Сумма прописью                                          |Вид. опер.    | 9 |")
aadd(text,sum_pr[ 1] +                            "                |------------------|")
aadd(text,sum_pr[ 2] +                            "                |Назн.плат.    |   |")
aadd(text,sum_pr[ 3] +                            "                |------------------|")
aadd(text,"Назначение платежа                                      |N гр.банка    |   |")
aadd(text,about +                                                " -------------------|")
aadd(text,"")
aadd(text,"Подпись вносителя :   _________________")
aadd(text,"")
aadd(text,"Контролер : __________________               Кассир: __________________ ")
aadd(text,"")
aadd(text,"")
aadd(text, replicate( "-", 78))
// next
*/

return text

autofunction setValue use3015 init .F.
 
funcDef kasRKO ;
  with docNumb:N, curDate:D, debSchet:C, credSchet:C, summa:N, giveTo:C,;
       kassir:C, nazn:C, sim:C, ro:C, pasSer:C, pasGived:C ;
  local text, postName, curDest, rSumma, kassa

postName := rTrim( confPostName()) + " УС " + rTrim( confNodeName())
rSumma   := if( Summa   >0, rSumma( Summa), "") +;
	' (643)'  // Add by TMV 02-07-2004    //02-12-2004 TAT (810)->(643)
rSumma = Upper(SubStr( rSumma, 1, 1)) + SubStr( rSumma, 2)
docNumb  := if( docNumb >0, padL( docNumb, 6, "0"), Space( 6))
Summa    := if( Summa   >0, lTrim( strTran( mStr( Summa), ".", "")), "")
kassa    := confBankName() + ', '+rTrim( confFilial()) 
if left( credSchet, 5) <> "20202" then kassa += ", ПБО " + confOtdNumb()
kassa    := razbivka( kassa,  {{ 38, "L"}, { 38, "L"}})
curDest  := razbivka( giveTo, {{ 39, "L"}, { 39, "L"}, { 39, "L"}, { 39, "L"}})
kassir   := razbivka( kassir, {{ 32, "L"}, { 40, "L"}})
rSumma   := razbivka( rSumma, {{ 56, "L"}, { 56, "L"}, { 56, "L"}})

text := extractStrOpen( "RKO")
text := makeZ( text, {{ "@docN",      docNumb       },;
                      { "@curDate",   curDate       },;
                      { "@rDate",     rDate(curDate)},;
                      { "@curDest1",  curDest[ 1]   },;
                      { "@curDest2",  curDest[ 2]   },;
                      { "@curDest3",  curDest[ 3]   },;
                      { "@curDest4",  curDest[ 4]   },;
                      { "@debSchet",  debSchet      },;
                      { "@credSchet", credSchet     },;
                      { "@summa",     Summa         },;
                      { "@rSumma1",   rSumma[ 1]    },;
                      { "@rSumma2",   rSumma[ 2]    },;
                      { "@rSumma3",   rSumma[ 3]    },;
                      { "@postName",  postName      },;
                      { "@dest",      nazn          },;
                      { "@kassir1",   kassir[ 1]    },;
                      { "@kassir2",   kassir[ 2]    },;
                      { "@kassa1",    kassa[ 1]     },;
                      { "@kassa2",    kassa[ 2]     },;
                      { "@postName",  confPostName()},;
                      { "@sko",       sim           },;
                      { "@ro",        ro            },;
                      { "@bik",       confBic()     },; // Add by TMV 30-06-2004
                      { "@pasSer",    pasSer        },;
                      { "@pasGived",  pasGived      }})
text := CtoA( text, endl())
return text

funcDef kasRKOPOR ;
  with docNumb:N, curDate:D, debSchet:C, credSchet:C, summa:N, giveTo:C,;
       kassir:C, nazn:C, sim:C, ro:C, pasSer:C, sAdres:C ;
  local text, postName, curDest, rSumma, kassa, passport, adrescl

postName := rTrim( confPostName()) + " УС " + rTrim( confNodeName())
rSumma   := if( Summa   >0, rSumma( Summa), "")
rSumma = Upper(SubStr( rSumma, 1, 1)) + SubStr( rSumma, 2)
docNumb  := if( docNumb >0, padL( docNumb, 6, "0"), Space( 6))
Summa    := if( Summa   >0, lTrim( strTran( mStr( Summa), ".", "")), "")
kassa    := confBankName() + ', '+rTrim( confFilial()) 
if left( credSchet, 5) <> "20202" then kassa += ", ПБО " + confOtdNumb()
kassa    := razbivka( kassa,  {{ 50, "L"}, { 50, "L"}})
curDest  := razbivka( giveTo, {{ 44, "L"}, { 44, "L"}})
kassir   := razbivka( kassir, {{ 50, "L"}, { 40, "L"}})
rSumma   := razbivka( rSumma, {{ 56, "L"}, { 56, "L"}, { 56, "L"}})
passport := razbivka( pasSer, {{ 56, "L"}, { 56, "L"}})
adrescl  := razbivka( sAdres, {{ 44, "L"}, { 44, "L"}})

text := extractStrOpen( "RKO_POR")
text := makeZ( text, {{ "@docN",      docNumb       },;
                      { "@curDate",   curDate       },;
                      { "@rDate",     rDate(curDate)},;
                      { "@curDest1",  curDest[ 1]   },;
                      { "@curDest2",  curDest[ 2]   },;
                      { "@curDest3",  adrescl[ 1]   },;
                      { "@curDest4",  adrescl[ 2]   },;
                      { "@debSchet",  debSchet      },;
                      { "@credSchet", credSchet     },;
                      { "@summa",     Summa         },;
                      { "@rSumma1",   rSumma[ 1]    },;
                      { "@rSumma2",   rSumma[ 2]    },;
                      { "@rSumma3",   rSumma[ 3]    },;
                      { "@postName",  postName      },;
                      { "@dest",      nazn          },;
                      { "@kassir1",   kassir[ 1]    },;
                      { "@kassir2",   kassir[ 2]    },;
                      { "@kassa1",    kassa[ 1]     },;
                      { "@kassa2",    kassa[ 2]     },;
                      { "@postName",  confPostName()},;
                      { "@sko",       sim           },;
                      { "@ro",        ro            },;
                      { "@pasSer1",   passport[1]   },;
                      { "@pasSer2",   passport[2]   }})
text := CtoA( text, endl())
return text


funcDef kasRKO_BO ;
  with docNumb:N, curDate:D, debSchet:C, credSchet:C, summa:N, giveTo:C,;
       kassir:C, curAdmPasp:C, nazn:C, sim:C, ro:C ;
  local text, postName, curDest, rSumma, kassa, pas1, pas2

postName := rTrim( confPostName()) + " УС " + rTrim( confNodeName())
rSumma   := if( Summa   >0, rSumma( Summa), "")
rSumma = Upper(SubStr( rSumma, 1, 1)) + SubStr( rSumma, 2)
docNumb  := if( docNumb >0, padL( docNumb, 6, "0"), Space( 6))
Summa    := if( Summa   >0, lTrim( strTran( mStr( Summa), ".", "")), "")
kassa    := rTrim( confFilial()) + ', ' + confBankName() + ' '
pas1     := razbivka( curAdmPasp, {{ 35, "L"}, { 35, "L"}})
pas2     := pas1[ 2]
pas1     := pas1[ 1]

kassa    := razbivka( kassa,  {{ 38, "L"}, { 38, "L"}})
curDest  := razbivka( giveTo, {{ 39, "L"}, { 39, "L"}, { 39, "L"}, { 39, "L"}})
kassir   := razbivka( kassir, {{ 32, "L"}, { 40, "L"}})
rSumma   := razbivka( rSumma, {{ 56, "L"}, { 56, "L"}, { 56, "L"}})

text := extractStrOpen( "RKO2")
text := makeZ( text, {{ "@docN",      docNumb       },;
                      { "@curDate",   curDate       },;
                      { "@rDate",     rDate(curDate)},;
                      { "@curDest1",  curDest[ 1]   },;
                      { "@curDest2",  curDest[ 2]   },;
                      { "@curDest3",  curDest[ 3]   },;
                      { "@curDest4",  curDest[ 4]   },;
                      { "@debSchet",  debSchet      },;
                      { "@credSchet", credSchet     },;
                      { "@summa",     Summa         },;
                      { "@rSumma1",   rSumma[ 1]    },;
                      { "@rSumma2",   rSumma[ 2]    },;
                      { "@rSumma3",   rSumma[ 3]    },;
                      { "@postName",  postName      },;
                      { "@dest",      nazn          },;
                      { "@kassir1",   kassir[ 1]    },;
                      { "@kassir2",   kassir[ 2]    },;
                      { "@kassa1",    kassa[ 1]     },;
                      { "@kassa2",    kassa[ 2]     },;
                      { "@postName",  confPostName()},;
                      { "@sko",       sim           },;
                      { "@ro",        ro            },;
                      { "@pas1",      pas1          },;
                      { "@pas2",      pas2          }})
text := CtoA( text, endl())
return text


//Назначение:  Возвращает массив - ОБЪЯВЛЕНИЕ НА ВЗНОС НАЛИЧНЫМИ
function vznos_nal(nd,;     // - номер документа
                   dat,;    // - дата
                   fr,;     // - от кого принят взнос
                   acc,;    // - счет зачисления
                   sum,;    // - сумма цифрами
                   men,;    // - получатель
                   ab,;     // - назначение взноса
                   bal_cas,;// - дебетовый счет ( счет кассы )
                   sim,;    // - символ кассовой отчетности
                   ro ,;// - подкрепление кассы
                   useGSZ, useTransit)

local text:={}    // Это мы и должны вернуть - бланк для взноса наличными
local the_date, ; // 58 - дата (буквами)
      from,     ; // 40 - от кого принят взнос
      to_acc,   ; // 14 - счет зачисления
      to_bank,  ; // 36 - банк получателя
      the_sum,  ; // 16 - сумма цифрами
      to_men,   ; // 46 - получатель
      sum_pr,   ; // 78 - сумма прописью, возможно поделенная на две части
      about,    ; // 60 - назначение взноса
      deb,      ; // дебетовый счет  ( счет кассы ) - балансовая часть
      prop,     ; // сумма прописью
      simb, i,  ;
      sum_pr1, to_men1, arr, rsum, arDest, transStr := "", bankReci, arBankReci
local arAboutLens, lenPtr, arNazn2 := {}, sDest

DEFAULT ro    TO "9"
DEFAULT useGSZ     TO .F.
DEFAULT useTransit TO .F.

if valType( ro) == "L" then ro := "9"

if useTransit .AND. !useZabota() then transStr := "транзитный "

if confUseAgent()
  if isBlankOrder()
    arr := {{ "@dest1", "счет N "}}
  else
    if valType( sum) = "N"
      if valType( sim) =="C" then sim := val( sim)
      sum := { sum, 0, 0, 0, 0, 0}
//      sim := { sim, 0, 0, 0, 0, 0}  // Replaced by TMV  16-10-2001
      sim := { 0, 0, 0, 0, 0, 0}
    endif
    for i := 1 to len( sim)
      if valType( sim[ i]) == "C" then sim[ i] := val( sim[ i])
    next

    the_Sum := 0.00

    for i := 1 to len( sum ) do the_Sum += sum[ i ]    

    //
    // было
    //
    // sDest:= transStr + "счет N " + acc + " Клиента: " + ;
    //   if( useTransit, "", men) + " В " + confBankName() + " "     + ;
    //   rTrim( confFilial())
    sDest:= transStr + "счет N " + acc + ;
       if( useTransit, " ", " Клиента: " + men+ " В ")  + confBankName() + " " +;
       rTrim( confFilial()) +;
		 rTrim( confOtdName() ) // Add by TMV  05-12-2002
    from := razbivka( fr, {{ 38, "L"}, { 42, "L"}})
    rSum := razbivka( rSumma( the_Sum), {{ 63, "L"}, { 78, "L"}})
	 rSum[1] = Upper(SubStr( rSum[1], 1, 1)) + SubStr( rSum[1], 2)
    arDest := razbivka( sDest, {{ 60, "L"}, { 78, "L"}})
    //ab   := razbivka( ab, {{ 60, "L"}, { 78, "L"}})
    about   := razbivka( ab, arRazbivkaLens( ab, 60, 78))
    for i := 2 to min( len( about), 3) do aAdd( arNazn2, about[ i] + "|")
    arr  := {{ "@docn",      padL( nD, 6, "0")      },;
             { "@schet711",  acc                    },;
             { "@rdate",     rDate( dat)            },;
             { "@from",      men                    },;
             { "@from1",     from[ 1]               },;
             { "@from2",     from[ 2]               },;
             { "@sum",       padL( toDefSum(the_Sum), 20)},; 
             { "@bankreci1", confBankName() + ","   },;
             { "@bankreci2", rTrim( confFilial()) + ", " + confOtdName() },;
             { "@rsum1",     rSum[ 1]               },;
             { "@rsum2",     rSum[ 2]               },;
             { "@nazn1",     about[ 1]              },;
             { "@nazn2",     arNazn2                },;
             { "@bankreci3", confFilial()           },;
             { "@reci",      rTrim( confFilial()) + ", " + confOtdName() },; // Replaced <confNodeName()> to <rTrim( confFilial())> by TMV 05-12-2002
             { "@sum1",      padL( toDefSum( sum[ 1]), 15)     },;
             { "@sim1",      padL( lTrim( eStr( sim[ 1])), 4) },;
             { "@sum2",      padL( toDefSum( sum[ 2]), 15)     },;
             { "@sim2",      padL( lTrim( eStr( sim[ 2])), 4) },;
             { "@sum3",      padL( toDefSum( sum[ 3]), 15)     },;
             { "@sim3",      padL( lTrim( eStr( sim[ 3])), 4) },;
             { "@sum4",      padL( toDefSum( sum[ 4]), 15)     },;
             { "@sim4",      padL( lTrim( eStr( sim[ 4])), 4) },;
             { "@sum5",      padL( toDefSum( sum[ 5]), 15)     },;
             { "@sim5",      padL( lTrim( eStr( sim[ 5])), 4) },;
             { "@dest1",     arDest[ 1]     },;
             { "@dest2",     arDest[ 2]     }}
// Replaced <confNodeName()> to <rTrim( confFilial())> by TMV 05-12-2002
//             { "@reci",      confNodeName() + ", " + confOtdName() },;
   endif
   text := extractStrOpen( "pko3")
//   text := extractStrOpen( "pko_new")  //  TMV  20-09-2001
   text := makeZ( text, arr)
   return CtoA( text, endl())   
endif



//номер документа
to_men:=padr(men,40," ")
if isBlankOrder()
   nd       := Space( 6)
   the_Date := Space( 56)
   to_acc   := Space( 20)
   to_men   := { Space( 35), Space( 35)}
   to_men1  := Space(46)
else
   nd       := padl(nd,6,"0")
   the_date := padc( rDate(dat),56," ")
   to_Acc   := schet20( allTrim( acc))
   //
   // Убрано 23.04.2001 для реализации положения 58/1
   //
   //if left( acc, 2) $ "42,45"
     to_men := men
   //else
   //  to_men   := confBankName()+ ',' + rTrim( confFilial()) + ", " + rTrim( confOtdName())
   //endif
   to_men1  := padR( to_men, 46)
   to_men   := razbivka( to_men, {{ 35, "L"}, { 35, "L"}})
endif

//от кого принят платеж
from:=razbivka( fr, {{ 38,"L"}, {38,"L"}})
//банк получателя
to_bank := Space( 29)

if valType( sum) = "N"
   sum := { sum, 0, 0, 0, 0}
   sim := { sim, 0, 0, 0, 0}
endif
//Изменено 03.05.2001 в 12:57, Залманов А.З.
//    the_Sum := sum[ 1] + sum[ 2] + sum[ 3] 
    the_Sum := 0
    for i := 1 to len( sum ) do the_Sum += sum[ i ]    
//
if isBlankOrder()
   sum_Pr  := { space( 77), space( 77)}
   the_Sum := Space( 14)
   sum_pr1 := { Space( 41), Space( 56), Space( 56)}
else
   sum_pr  := razbivka( rSumma( the_Sum), {{ 77, "L"}, { 77, "L"}})
	sum_pr[1] = Upper(SubStr( sum_pr[1], 1, 1)) + SubStr( sum_pr[1], 2)
   sum_pr1 := razbivka(rSumma( the_Sum) +;
		' (643)',;  // Add by TMV 02-07-2004   //02-12-2004 TAT (810)->(643)
		{{ 41, "L"}, { 56, "L"}, { 56, "L"}})
	sum_pr1[1] = Upper(SubStr( sum_pr1[1], 1, 1)) + SubStr( sum_pr1[1], 2)
   the_sum := padl( alltrim( strTran( mStr( the_Sum), ".", "")), 14)
endif

for i := 1 to 5
    if sum[ i] = 0
       sum[ i] := Space( 13) //!!
       sim[ i] := Space(  5)
    else
       sum[ i] := padl( allTrim( mStr( sum[ i])), 13)
       sum[ i] := padL( strTran( sum[ i], ".", ""), 13)
       sim[ i] := lTrim( padL( sim[ i], 5))
    endif
next

//назначение платежа
// about:=padr(ab,55," ")
about   := razbivka( ab, arRazbivkaLens( ab, 60, 78))
for i := 2 to min( len( about), 3) do aAdd( arNazn2, about[ i] + "|")

   

// about := razbivka( ab, {{ 60, "L"}, { 60, "L"}, { 60, "L"}})
//дебетовый счет
// deb:=" сч. N "+padL( lTrim( bal_cas), 5,"0")
//
simb:=padc(sim,5," ")

bankReci := confBankName() + ", " + rTrim( confFilial())
// Edited by TMV  16-10-2001
if left( bal_cas, 5) <> "20202" then bankReci += ", ПБО " + confOtdNumb()
// Edited by TMV  16-10-2001  --  End
arBankReci := razbivka( bankReci, {{ 17, "L"}, { 35, "L"}})
bankReci   := razbivka( bankReci, {{ 41, "L"}, { 57, "L"}})

arr  := {{ "@docN",      nD             },;
         { "@rDate",     if( empty( dat), "", rDate( dat))    },;
         { "@fio1",      from[ 1]       },;
         { "@fio2",      from[ 2]       },;
         { "@credSchet", to_acc         },;
         { "@debSchet",  bal_cas        },;
         { "@summa",     the_Sum        },;
         { "@rSumma1",   sum_pr1[ 1]    },;
         { "@rSumma2",   sum_pr1[ 2]    },;
         { "@rSumma3",   sum_pr1[ 3]    },;
         { "@bankreci1", bankReci[ 1]   },;
         { "@bankreci2", bankReci[ 2]   },;
         { "@bank1",     arBankReci[ 1] },;
         { "@bank2",     arBankReci[ 2] },;
         { "@reci1",     to_men[ 1]     },;
         { "@reci2",     to_men[ 2]     },;
         { "@nazn1",     about[ 1]      },;
         { "@nazn2",     arNazn2        },;
         { "@ro",        ro             },;
         { "@summa1",    sum[ 1]        },;
         { "@s1",        sim[ 1]        },;
         { "@summa2",    sum[ 2]        },;
         { "@s2",        sim[ 2]        },;
         { "@summa3",    sum[ 3]        },;
         { "@s3",        sim[ 3]        },;
         { "@summa4",    sum[ 4]        },;
         { "@s4",        sim[ 4]        },;
         { "@summa5",    sum[ 5]        },;
         { "@s5",        sim[ 5]        }}



text := extractStrOpen( "pko")
//text := extractStrOpen( "pko3")
text := makeZ( text, arr)
text := CtoA( text, endl())

/*

//         0123456789012345678901234567890123456789012345678901234567890123456789012345678
aadd(text,"-------------------------------------------------------------------------------")
aadd(text,"                         КВИТАНЦИЯ  N   |     "+nd+"     | |      0402001     |")
aadd(text,"                                        ------------------ --------------------")
aadd(text,                     the_date                         + " ----------------------")
aadd(text,"От кого "+          from[1]                 +" Для зач.  |"      +  to_acc  +"|")
aadd(text,"        "+          from[2]                 +" на сч.N   |--------------------|")
                                                                  // 01234567890123456789
if isBlankOrder()
   aadd(text,'Банк получателя   ' + Space( 16) + Space( 22)                   + ' |   ' + the_sum  +'   |')
else
   //aadd(text,'Банк получателя   АБ "ПЕТРОВСКИЙ",' + padR( confFilial(), 22) + ' |   ' + the_sum +'   |')
   //                           012345678901234567
   aadd(text,"Банк получателя " + padR( confBankName() + ",", 40) + ' |   ' + the_sum +'   |')
   to_Bank := padR( confFilial(), 29)
endif
aadd(text,                to_bank     +"                            |   Сумма   цифрами  |")
aadd(text,"Получатель "+       to_men1                 +           "---------------------|")
aadd(text,                     sum_pr[1]                                              +" |")
aadd(text,                     sum_pr[2]                                              +" |")
aadd(text,"Сумма прописью                                                                |")
aadd(text,"Назначение взноса "+ about[1]                                          +"     |")
aAdd(text,about[2]                                                           +  "        |")
aAdd(text,about[3]                                                           +  "        |")
aadd(text,"  М. П.           Бухгалтер             Деньги принял кассир                  |")

from:=razbivka( fr, {{ 42,"L"}, { 42,"L"}})

//         0123456789012345678901234567890123456789012345678901234567890
aadd(text,"-------------------------------------------------------------------------------")
aadd(text,"                   ПРИХОДНЫЙ           |      "+nd+"     | |      0482008     |")
aadd(text,"                   КАССОВЫЙ ОРДЕР   N  ------------------- --------------------")
aadd(text,                     the_date                            +"                     ")
aadd(text,"                                                ДЕБЕТ             СУММА        ")
aadd(text,"От кого                                   -------------------------------------")
aadd(text,                     from[1] +            "|"  +  deb +" |"+the_sum+   "       |")
aadd(text,                     from[2]             +"--------------|     Общая           |")
aadd(text,"                                                КРЕДИТ  |                     |")
if isBlankOrder()
   aadd(text,'Банк получателя   ' + Space( 16) + Space( 22) + '----------------------|')
   // sum_pr1 := { Space( 41), Space( 56), Space( 56)}
else
   // aadd(text,'Банк получателя   АБ "ПЕТРОВСКИЙ",' + padR( confFilial(), 22) + '----------------------|')
   // sum_pr1:=razbivka(rSumma(sum[1]+sum[2]+sum[3]), {{ 41, "L"},{ 56, "L"}, { 56, "L"}} )
   aadd(text,"Банк получателя " + padR( confBankName() + ",", 39) + ' |   ' + the_sum +'    |')
   to_Bank := padR( confFilial(), 29)
endif
aadd(text,                to_bank     +" |код |                    |    частные    | код |")
aadd(text,"                              -----|                    |---------------------|")
aadd(text,"Получатель                         |"     +  to_acc +  "|"+sum[1]+" |"+sim[1]+"|")
aadd(text,to_men[1]                         +"|                    |---------------------|")
aadd(text,to_men[2]                         +"|                    |"+sum[2]+" |"+sim[2]+"|")
aadd(text,"-------------------------------    |                    |---------------------|")
aadd(text,"Код                           |    |NN сч.              |"+sum[3]+" |"+sim[3]+"|")
aadd(text,"------------------------------------------------------------------------------|")
aadd(text,"                                                        |Вид. опер.|      9   |")
aadd(text,"                                                        |---------------------|")
aadd(text,"Сумма прописью " + sum_pr1[ 1] +                       "|Назн.плат.|          |")
aadd(text,sum_pr1[ 2] +                                           "|---------------------|")
aadd(text,sum_pr1[ 3] +                                           "|N гр.банка|          |")
aadd(text,"Назначение платежа                                      -----------------------")
aadd( text, about[1])
aadd( text, about[2])
aadd( text, about[3])

// aadd(text,"")
if useGSZ
   aadd(text,"Подпись вносителя          Операционист              Кассир     ")
else
   aadd(text,"Подпись вносителя          Контролер         Бухгалтер              Кассир     ")
endif
aadd(text,"")
aadd(text,"")
aadd(text,"")

*/

return text

autofunction setValue isBlankOrder init .F.


// Edited by TMV 13-11-2001
funcDef aChangePcts with fio:C, Schet:C, theDate:D, theType:C,;
	sPasport:C, sVid:C, sAdres:C, sTelefon:C ;
  local text, servDummy, aTmpPas:={}, aTmpAdr:={}
if .T. //confUseAgent()
  aTmpPas=razbivka(sVid, {{38, "L"},{38, "L"}})
  aTmpAdr=razbivka(sAdres, {{32, "L"},{32, "L"},{32, "L"}})
  servDummy :={{ "@bankname", confBankName() },;
               { "@filial",   confFilial()   },;
               { "@fio",      fio            },;
               { "@pasp1",    sPasport       },;
               { "@pasp2",    aTmpPas[1]     },;
               { "@pasp3",    aTmpPas[2]     },;
               { "@adres1",   aTmpAdr[1]     },;
               { "@adres2",   aTmpAdr[2]     },;
               { "@adres3",   aTmpAdr[3]     },;
               { "@telefon",  sTelefon       },;
               { "@lastname", left(fio, at(' ', fio) -1) },;
               { "@d1",       myDtoC( theDate) },;
               { "@curdate",  myDtoC( setCurDate()) },;
               { "@schet",    schet          },;
               { "@typepct",  theType        }}
  text := makeZ( extractStrOpen( "chpct"), servDummy)
  text := CtoA( text, endl())
endif
/*
else
text := { "                           Управляющему филиалом " + confFilial(),;
          "                           от " + fio                            ,;
          "                                                                ",;
          "                                                                ",;
          "                       Заявление                                ",;
          "                                                                ",;
          "                                                                ",;
          "     Прошу с " + DtoC( theDate) + " изменить порядок начисления" ,;
          "процентов по моему текущему счету N " + Schet + "."              ,;
          "Начисление процентов производить " + Upper( theType)             ,;
          "                                                                ",;
          "                                                                ",;
          rDate( SetCurDate()) + "                       ___________________________ ",;
          "( дата заполнения )                         ( подпись клиента ) ",;
          "                                                                ",;
          "                                                                ",;
          "Штамп отделения                          _______________________",;
          "                                         (подпись операциониста)" }
endif*/

return text

funcDef receipt34b with docNumb:N,  theDate:D,;
  credSchet:C, theSumma:N, kassir:C, osLider:C, nazn:C local text, rSumma, from

rSumma   := if( theSumma >0, rSumma( theSumma), "")
rSumma = Upper(SubStr( rSumma, 1, 1)) + SubStr( rSumma, 2)
docNumb  := if( docNumb  >0, padL( docNumb, 6, "0"), Space( 6))
theSumma := if( theSumma >0, lTrim( strTran( mStr( theSumma), ".", "")), "")
// Replaced by TMV  16-10-2001
from     := rTrim( confFilial()) + ", ПБО " + confOtdNumb()

text := extractStrOpen( "RC34B")
text := makeZ( text, {{ "@nDoc",      docNumb        },;
                      { "@curDate",   rDate( theDate)},;
                      { "@from",      from           },;
                      { "@rSumma",    rSumma         },;
                      { "@bankname",  confBankName() + ", " + confFilial()},;
                      { "@summa",     theSumma       },;
                      { "@dest",      nazn           },;
                      { "@kassir",    kassir         },;
                      { "@postName",  confPostName() },;
                      { "@osLider",   osLider        },;
                      { "@credSchet", credSchet      }})
text := CtoA( text, endl())

return text



funcDef receipt34a with docNumb:N,  theDate:D, filial:C, otdName:C,;
                        unitName:C, kassir:C, osLider:C, theSumma:N, nazn:CU ;
                   local text, rSumma, from

DEFAULT nazn TO "ПОДКРЕПЛЕНИЕ ПЕРЕВОДНЫХ ОПЕРАЦИЙ(673сч.)"

rSumma   := if( theSumma >0, rSumma( theSumma), "")
rSumma = Upper(SubStr( rSumma, 1, 1)) + SubStr( rSumma, 2)
docNumb  := if( docNumb  >0, padL( docNumb, 6, "0"), Space( 6))
theSumma := if( theSumma >0, lTrim( strTran( mStr( theSumma), ".", "")), "")
// Replaced by TMV  16-10-2001
from     := confBankName() + ", " + rTrim( filial) + ", ПБО " + confOtdNumb()
text := extractStrOpen( "RC34A")
text := makeZ( text, {{ "@nDoc",     docNumb                            },;
                      { "@curDate",  rDate( theDate)                    },;
                      { "@from",     from                               },;
                      { "@bank",     confBankName() + ", " + confFilial()},;
                      { "@sign1",    osLider                            },;
                      { "@rSumma",   rSumma                             },;
                      { "@summa",    theSumma                           },;
                      { "@dest",     nazn                               },;
                      { "@kassir",   kassir                             },;
                      { "@postName", confPostName()                     }})
text := CtoA( text, endl())
return text


funcDef PaymentPens with docNumb:N, theDate:D,  fio:C, address:C,;
                         otdNumb:C, unitName:C, theSumma:N      ,;
                         pasport:C, pasGived:C, lPrintDate:L    ,;
                         lPrintPasp:L ;
                    local text, rSumma, rDate

docNumb  := if( docNumb  >0, nStr(   docNumb),  "")
rSumma   := if( theSumma >0, rSumma( theSumma), "")
rSumma = Upper(SubStr( rSumma, 1, 1)) + SubStr( rSumma, 2)
theSumma := if( theSumma >0, strTran( mStr( theSumma), ".", ""), "")
rDate    := if( lPrintDate,  rDate( theDate), '"____"______________ 200__г.')
pasport  := if( lPrintPasp,  pasport,  "")
pasGived := if( lPrintPasp,  pasGived, "")

text := { ;
"                         ВЫПЛАТНОЕ ПОРУЧЕНИЕ N " + docNumb         ,;
"                         Дата " + rDate                            ,;
"                                                                 " ,;
"Я, " + fio                                                         ,;
"                                                                 " ,;
"Проживающий(ая) по адресу :"                                       ,;
address                                                             ,;
"                                                                 " ,;
"Подтверждаю получение мною пенсии от отделения " + otdNumb + " " +  ;
unitName + " РУПС",;
"в сумме " + theSumma                                               ,;
"Сумма прописью : "                                                 ,;
rSumma                                                              ,;
"                                                                 " ,;
"Указанную сумму получил(а) : _______________________"              ,;
"Расшифровка фамилии        : _______________________"              ,;
"                                                                 " ,;
"Отметки"                                                           ,;
"Предъявлен паспорт : " + pasport                                   ,;
"Выдан              : " + pasGived                                  ,;
"                                                                 " ,;
"Сумму выдал ________________         Контролер __________________" ,;
"                                                                 " ,;
"                                                         К Ш     " ,;
"                                                                 " ,;
"                                                                 " ,;
"                                                                 "  }
return text

//
// титульный лист на кассовые документы
//
funcDef blnkTitle local aOrder

aOrder := { ;
"Штамп                                    Срок хранения 75 лет",;
""                                                             ,;
"Документы за __________________________"                      ,;
""                                                             ,;
"Кассовые документы"                                           ,;
""                                                             ,;
"1.Приходные кассовые документы кол-во __ шт. ______________ Руб.___коп." ,;
""                                                             ,;
"2.Расходные кассовые документы кол-во __ шт. ______________ Руб.___коп." ,;
""                                                             ,;
"Мемориальные документы на сумму              ______________ Руб.___коп." ,;
""                                                             ,;
"Заведующий кассой"                                            ,;
""                                                             ,;
"Кассир" }

printCntOrders( aOrder)
return NIL

//
// Лист-разделитель
//
funcDef blnkListRazd local aOrder

aOrder := {;
"Мемориальные документы за __________________________" ,;
""                                                     ,;
"                       на сумму ___________________ " ,;
""                                                     ,;
"Приложений _______________________" }

printCntOrders( aOrder)
return NIL

funcDef Receipt16 with dDate:D, cnt:N, ASumma:N local arr
arr := { "", "", "", "",;
"                           СПРАВКА                           ",;
"                                                             ",;
"к мемориальному ордеру от " + rDate( dDate)                   ,;
"                                                             ",;
"                                                             ",;
"От отд.связи " + confPostName()                               ,;
"Принято оплаченных выплатных поручений " + nStr( cnt) + " шт.",;
"На сумму " + lTrim(strTran( mStr( ASumma), ".", ""))+" ("+rSumma(ASumma)+")"     ,;
"         (цифрами и прописью)                                ",;
"                                                             ",;
"                                                             ",;
"Контролер ___________________                                ",;
"Ст.Кассир ___________________                                ",;
"                                                             ",;
"Штамп                                                        "}
return arr

* NEW OBJECT FILE  ------- справка по доходу
funcDef servSprav with schet:C local arr := {}, sumMin, cnt, i

arr := { ;
'                                        ' + confBankName()            ,;
Space( 40) + rTrim( confFilial()) + " " + confOtdName()          ,;
""                                                                     ,;
"                                 СПРАВКА                            " ,;
""                                                                     ,;
"Дата выдачи " + rDate( setCurDate())                                  ,;
"Клиент " + cliFullName()                                              ,;
"Лицевой счет " + schet                                                ,;
"Минимальная сумма ежемесячного поступления"                           ,;
"за три последних месяца " + mStr( CLIENTS ->minSum/ 100)              ,;
"Допустимая сумма ежемесячного списания " + mStr( abs( CLIENTS ->credSumma)/ 100),;
""                                                                     ,;
"Подпись клиента            ___________________________"               ,;
""                                                                     ,;
"Подпись отвественного лица ___________________________"                                                          ,;
"", "" }
// cnt := len( arr)
// for i := 1 to cnt do aAdd( arr, arr[ i])

return arr

funcDef incServSprav local arr, arr2, i := 0, str

accPrizList( "15 1", @arr, @arr2)
do case
   case len( arr) == 0
        messageB( "У клиента нет пенсионных счетов !!!")
   case len( arr) == 1
        i := 1
   otherwise
        getUp( 5, 5, {{ "Выберите счет", block( i), arr}})
endcase
if i > 0
   if !empty( str := servSprav( arr2[ i, 2]))
      editMemo( AtoC( str))
      if yesNo( "Печатать справку ?") then printAOrder( str)
   else
      messageB( "Нет информации о доходах !!!")
   endif
endif
return NIL


* NEW OBJECT FILE  ------------ мем.ордер
funcDef memOrder with nDoc:N, curDate:D, debetAcc:CA, creditAcc:CA,;
   summa:NA, curDest:C, ro:C local arr, str2, i
//одной строчки на адреса организаций мало
// str2:=razbivka( curDest,{{50,"L"},{70,"L"},{70,"L"},{70,"L"}})
str2 := razbivka( curDest, arRazbivkaLens( curDest, 50, 70))

do case
   case valType( debetAcc) == "C" .AND. valType( creditAcc) == "C"
        debetAcc  := { debetAcc}
        creditAcc := { creditAcc}
        summa     := { summa}
   case valType( debetAcc) == "A" .AND. valType( creditAcc) == "A"
   otherwise
        messageB( "Неверный вызов !!!")
        return {}
endcase

arr := { ;
"                                         Род операции N " + ro        ,;
""                                                                     ,;
"                        МЕМОРИАЛЬНЫЙ ОРДЕР N " + eStr( nDoc)           ,;
"                            " + rDate( curDate)                       ,;
""                                                                     ,;
"+----------+--------------------+--------------------+--------------+",;
"|   Дата   | Дебет ( N счета )  | Кредит ( N счета ) | Сумма (руб.) |",;
"+----------+--------------------+--------------------+--------------+"}
for i := 1 to len( debetAcc)
    aAdd( arr, ;
    "|" + DtoC( curDate) + "|" + debetAcc[ i] + "|" + creditAcc[ i] + "|" + ;
    padL( lTrim( dStr( summa[ i])), 14) + "|")
next
aAdd( arr, "+----------+--------------------+--------------------+--------------+")
aAdd( arr, ""                                                                     )
aAdd( arr, "Назначение платежа: "+str2[1]                                         )
for i := 2 to min( len( str2), 3) do aAdd( arr, str2[ i])

//aAdd( arr, str2[2]                                                                )
//aAdd( arr, str2[3]                                                                )
//aAdd( arr, str2[4]                                                                )
aAdd( arr, "Бухгалтер : ___________________________"                              )
aAdd( arr, "Ответсвенный исполнитель : ______________________________"            )
return arr


* ---------- пожелание на перечисление со счета денег
funcDef memWisc with schet:C, summa:N, curDest:C, anyWayUseAgent:LU ;
  local arr, i, cnt, str2, str, strSumma, arStr2 := {}
//одной строчки на адреса организаций мало

DEFAULT anyWayUseAgent TO .F.
// str2:=razbivka( curDest,{{40,"L"},{70,"L"},{70,"L"},{70,"L"}})
str2 := razbivka( curDest, arRazbivkaLens( curDest, 40, 70))
for i := 2 to min( len( str2), 3) do aAdd( arStr2, str2[ i])

strSumma := getS( summa)
if len( strSumma) > 75
  strSumma := razbivka( strSumma, {{ 75, "L"}, { 75, "L"}})
endif
arr := {{ "@curdate",   setCurDate()   },;
        { "@bankname",  confBankName() },;
        { "@filial",    confFilial()   },;
        { "@otd",       confOtdNumb()  },;
        { "@fio",       cliFullName()  },;
        { "@schet",     schet          },;
        { "@summa",     strSumma       },;
        { "@dest1",     str2[ 1]       },;
        { "@dest2",     arStr2         }}

str := extractStrOpen( "mem35")
str := makeZ( str, arr)
arr := CtoA( str, endl())

aAdd( arr, " " ); aAdd( arr, " " ); aAdd( arr, " " )
cnt := len( arr)
for i := 1 to cnt do aAdd( arr, arr[ i])
return arr


funcDef memWisc2 ;
  with debSchet:C, credSchet:C, bankDest:C, summa:N, curDest:C, dest:AC ;
  local arr, ADest := "", i, str, arrDest := {}


if valType( dest) == "C" 
   dest := razbivka( dest, {{ 50, "L"}, { 50, "L"}, { 50, "L"},;
                            { 50, "L"}, { 50, "L"}})
endif
for i := 2 to len( dest) 
  if !empty( dest[ i]) then aAdd( arrDest, dest[ i])
next

arr := {{ "@bankname", confBankName()},;
        { "@filial"  , confFilial()  },;
        { "@otd"     , confOtdNumb() },;
        { "@curdate" , setCurDate()  },;
        { "@fio"     , cliFullName() },;
        { "@debschet", debSchet      },;
        { "@credschet",credSchet     },;
        { "@summa"   , getS( summa)  },;
        { "@dest1",    dest[ 1]      },;
        { "@dest2_5",  arrDest       },;
        { "@destname", bankDest      }}
str := ExtractStrOpen( "mem34")
str := makeZ( str, arr)
arr := CtoA( str, endl())
return arr


// Add by TMV  27-11-2002
// Для выдачи кредита по новой схеме
funcDef memKred ;
  with debSchet:C, credSchet:C, summa:N, dest:C ;
  local arr, ADest := "", i, str, arrDest := {}

/*
if valType( dest) == "C" 
   dest := razbivka( dest, {{ 50, "L"}, { 50, "L"}, { 50, "L"},;
                            { 50, "L"}, { 50, "L"}})
endif
for i := 2 to len( dest) 
  if !empty( dest[ i]) then aAdd( arrDest, dest[ i])
next
*/
// Add by TMV  09-07-2003
str = cliDocum()
arrDest := razbivka( str, {{ 50, "L"}, { 50, "L"}})
// Add by TMV  09-07-2003  -- End
arr := {{ "@bankname", confBankName()},;
        { "@filial"  , confFilial()  },;
        { "@otd"     , confOtdNumb() },;
        { "@curdate" , setCurDate()  },;
        { "@fio"     , cliFullName() },;
        { "@debschet", debSchet      },;
        { "@credschet",credSchet     },;
        { "@summa"   , getS( summa)  },;
        { "@doc1"    , arrDest[1]    },;
        { "@doc2"    , arrDest[2]    },;
        { "@dest1",    dest          }}
str := ExtractStrOpen( "mem_kr")
str := makeZ( str, arr)
arr := CtoA( str, endl())
return arr


funcDef memWisc3 with debSchet:C, bankDest:C, summa:N, curDest:C, dest:AC,;
        docNumb:C, docDate:D local arr, ADest := "", i

summa := val( str( summa, 15, 2))
if valType( dest) == "C" then dest := { dest}
for i := 1 to len( dest)
    if !empty( dest[ i]) then ADest += dest[ i] + endl()
next
ADest := left( ADest, len( ADest) - 2)
arr := { ;
""                                                                     ,;
""                                                                     ,;
Space( 30) + rTrim( confFilial()) + " " + confOtdName()          ,;
""                                                                     ,;
"                             ПОРУЧЕНИЕ                               ",;
"             на перечисление денежных средств по текущему счету      ",;
""                                                                     ,;
"Прошу перечислить с моего текущего счета N " + debSchet               ,;
cliFullName()                                                          ,;
"---------------------------------------------------------------------",;
"                             (ФИО вкладчика)                         ",;
"в " + bankDest + " " + lTrim( dStr(summa)) + "(" + rSumma(summa) + ")",;
curDest                                                                ,;
"---------------------------------------------------------------------",;
"   (Куда: N р/с, наименование банка получателя, МФО)"                 ,;
""                                                                     ,;
ADest                                                                  ,;
"---------------------------------------------------------------------",;
"                 (За что, указать НДС)"                               ,;
"согласно договора (счета) N " + docNumb + " от " + DtoC( docDate)     ,;
"Вид оплаты: пред.оплата, услуги оказаны, товар получен"               ,;
""                                                                     ,;
"Дата: " + DtoC( setCurDate()) +                                        ;
       "                              Вкладчик ______________________" ,;
"",;
"",;
"                                     Бухгалтер _____________________" ,;
""                                                                     ,;
""                                                                     }
return arr


* --------------------- мем.ордер -----------------------------------------
funcDef memOrd2 with nDoc:N, curDate:D, debetAcc:C, creditAcc:C, summa:N ,;
                     curDest:C, ro:C, bankDest:C, toDest:C                ;
                local arr, from, sumStr, curBank, rSumma


from    := razbivka( cliFullName(), {{ 39, "L"}, { 39, "L"}})
sumStr  := padl( allTrim( strTran( mStr( summa), ".", "")), 15)
// Replaced by TMV  18-02-2003
//curBank := razbivka( rTrim( confFilial()) + " " + confOtdName(),;
//           {{ 39, "L"}, { 39, "L"}})
curBank := razbivka( confBankName() + " " + rTrim( confFilial()) + " " + confOtdName(),;
           {{ 39, "L"}, { 39, "L"}})

bankDest:= razbivka( bankDest, {{ 39, "L"}, { 39, "L"}})
toDest  := razbivka( toDest,   {{ 39, "L"}, { 39, "L"}})
curDest := razbivka( curDest,  {{ 60, "L"}, { 60, "L"}, { 60, "L"}})
rSumma  := razbivka( rSumma( summa),  {{ 60, "L"}, { 60, "L"}, { 60, "L"}})
rSumma[1] = Upper(SubStr( rSumma[1], 1, 1)) + SubStr( rSumma[1], 2)
arr  := { ;
"-------------------------------------------------------------------------------"       ,;
"               МЕМОРИАЛЬНЫЙ ОРДЕР   N  |" + Str( nDoc, 17) + "|  |     0481008     |" ,;
"                                       -------------------  ------------------|"       ,;
"               "+padC(rDate(curDate),21)+"                                          |" ,;
"                                                ДЕБЕТ             СУММА       |"       ,;
"Плательщик                             ---------------------------------------|"       ,;
from[ 1] +                             "|" + debetAcc +     "|  " + sumStr  + "|"       ,;
from[ 2] +                             "|                    |                 |"       ,;
"Банк плательщика                       |                    |                 |"       ,;
curBank[ 1] +                          "|NN сч.              |                 |"       ,;
curBank[ 2] +                          "---------------------|                 |"       ,;
"                                               КРЕДИТ       |                 |"       ,;
"Получатель                             ---------------------|                 |"       ,;
toDest[ 1] +                           "|" + creditAcc +    "|                 |"       ,;
toDest[ 2] +                           "|                    |                 |"       ,;
"Банк получателя                        |                    |                 |"       ,;
bankDest[ 1] +                         "|                    |                 |"       ,;
bankDest[ 2] +                         "|NN сч.              |                 |"       ,;
"------------------------------------------------------------|-----------------|"       ,;
rSumma[ 1] +                                                "|Вид. опер.|"+padC(ro,6)+"|",;
rSumma[ 2] +                                                "|----------|------|"       ,;
rSumma[ 3] +                                                "|Назн.плат.|      |"       ,;
"                                                            |----------|------|"       ,;
"Назначение платежа                                          |Срок плат.|      |"       ,;
curDest[ 1] +                                               "|----------|------|"       ,;
curDest[ 2] +                                               "|Очер.плат.|      |"       ,;
curDest[ 3] +                                               "|----------|------|"       ,;
"Приложение на _______лист     Подписи                       |N гр.банка|      |"       ,;
"                                                            -------------------"       }
return arr


funcDef memOrdNormal ;
   with nDoc:N, curDate:D, debetAcc:C, creditAcc:C, summa:N ,;
        curDest:C, ro:C, from:C, reci:C, bankreci:C          ;
   local arr, fio, sumStr, curBank, rSumma

fio    := razbivka( from, {{ 37, "L"}, { 37, "L"}})
rSumma := razbivka( rSumma( summa),  {{ 56, "L"}, { 56, "L"}, { 56, "L"}})
rSumma[1] = Upper(SubStr( rSumma[1], 1, 1)) + SubStr( rSumma[1], 2)
curDest:= razbivka( curDest,  {{ 56, "L"}, { 56, "L"}, { 56, "L"}})

arr := {{ "@docN" ,   nStr( nDoc)         },;
        { "@rDate",   rDate( setCurDate())},;
        { "@otdname", confBankName() + ' ' + confFilial()},;
        { "@fio1",    fio[ 1]             },;
        { "@fio2",    fio[ 2]             },;
        { "@reci",    reci                },;
        { "@bankreci",bankreci            },;
        { "@summa",   allTrim(mStr(summa))},;
        { "@rSumma1",  rSumma[ 1]         },;
        { "@rSumma2",  rSumma[ 2]         },;
        { "@rSumma3",  rSumma[ 3]         },;
        { "@nazn1",    curDest[ 1]        },;
        { "@nazn2",    curDest[ 2]        },;
        { "@nazn3",    curDest[ 3]        },;
        { "@ro",       ro                 },;
        { "@debSchet", debetAcc           },;
        { "@credSchet",creditAcc          }}
servDummy( arr)
printOpen( "mem")
servDummy( {})
return NIL


funcDef notifArr with fio:C, Addr:C, d1:D local arr, otdN := confOtdNumb(),s1
if confUseAgent()

   arr := {;
      "Уважаемый(ая) " + fio                                             ,;
      "проживающий(ая) по адресу: " + Addr                               ,;
      replicate("=",78)                                                  ,;
      "На почтовом отделении N " + otdN + " в " + confBankName() + " Вам открыт текущий ",;
      "пенсионный счет, на который "  + if( empty( d1), "", " с " + dtoc( d1)) + ;
      " поступают средства из Управления ",;
      "социальной защиты населения. Ознакомиться  с правилами выполнения операций по",;
      "Вашему счету Вы можете во время работы Вашего отделения N " + otdN + ".",;
      ""                                                                ,;
      "                    Пожалуйста, возьмите паспорт !"              ,;
      replicate("-",78), "" }
else
   arr := {;
      "Уважаемый(ая) "              + fio                                         ,;
      "проживающий(ая) по адресу: " + Addr                                        ,;
      replicate("=",78)                                                          ,;
      "На почтовом отделении N " + otdN                                           +;
      ' в ' + confBankName() + ' Вам открыт текущий пенсионный'                  ,;
      "счет, на который" + if( empty( d1), "", " с " + dtoc( d1))                 +;
      " поступают средства из Горцентра по начислению и "                         ,;
      "выплате пенсий и пособий. Ознакомиться  с правилами выполнения операций по",;
      "Вашему счету Вы можете во время работы Вашего отделения N "+ otdN + "."    ,;
      ""                                                                          ,;
      "                    Пожалуйста, возьмите паспорт !"                        ,;
      replicate("-",78), "" }
endif
if len( arr[ 2]) > 78
   s1      := razbivka( arr[ 2], {{ 78, "L"}, { 78, "L"}})
   arr[ 2] := s1[ 1] + endl() + s1[ 2]
endif
return arr

//
// аналог DtoC только для Date German
//
function myDtoC( curD)
local rV := DtoS( curD)
if len( rV) < 8
   messageB( "Сбой в расчете даты. Обратитесь на филиал !!!")
   rV := "20" + rV
endif
rV := right( rV, 2) + "." + subStr( rV, 5, 2) + "." + left( rV, 4)
return rV

function toDefSum( sum, isSilent)
local str := str( sum, 15, 2)
  DEFAULT isSilent TO .F.
  str := left( str, 12) + "-" + right( str, 2)
return if( isSilent .OR. sum == 0, "", lTrim( str))



funcDef arRazbivkaLens with dest:C, len1:N, len2:N ;
  local arAboutLens, lenPtr, i

  arAboutLens := { { len1, "L"}}
  lenPtr      := round( ( len( dest) - len1)/ len2 + 0.49, 0)
  if lenPtr > 0
     for i := 1 to lenPtr do aAdd( arAboutLens, { len2, "L"})
  endif

return arAboutLens

////////////////////////////////////////
//  Распоряжение на перевод ср-в на счет
//  Add by TMV 08-10-2001
funcDef raspPerevod ;
  with debSchet:C, credSchet:C, bankDest:C, summa:N, curDest:C, NameDest:C,;
  	theOrder:N, KorSchet:C, BikDest:C ;
  local arr, ADest := "", i, str, arrDest := {}, aSumma := {}, sSumma,;
  	dest := {}

sSumma = getS( summa)
aSumma := razbivka( sSumma, {{ 53, "L"}, { 53, "L"}, { 53, "L"}})
dest := razbivka( curDest, {{ 53, "L"}, { 53, "L"}, { 53, "L"}})

arr := {{ "@bankname", confBankName()},;
        { "@filial"  , confFilial()  },;
        { "@otd"     , confPostName() },;
        { "@nord"    , padL( str(theOrder,6), 6, "0")},;
        { "@curdate" , rDate(setCurDate())  },;
        { "@paspnum" , cliPasSer()+cliPasNum() },;
        { "@paspwho" , cliPasGiv() },;
        { "@fio"     , cliFullName() },;
        { "@debschet", debSchet      },;
        { "@credschet",credSchet     },;
        { "@summa1"   , aSumma[1]    },;
        { "@summa2"   , aSumma[2]    },;
        { "@summa3"   , aSumma[3]    },;
        { "@dest1",    dest[ 1]      },;
        { "@dest2",    dest[ 2]      },;
        { "@dest3",    dest[ 3]      },;
        { "@destbik",  BikDest       },;
        { "@korschet", KorSchet      },;
        { "@destname", NameDest      },;
        { "@bankdest", bankDest      }}
str := ExtractStrOpen( "rsp_per")
str := makeZ( str, arr)
arr := CtoA( str, endl())
return arr

////////////////////////////////////////
//  Плат. поручение на перевод ср-в на счет
//  Add by TMV 12-11-2001
funcDef platPorPerevod ;
  with debSchet:C, credSchet:C, bankDest:C, summa:N, curDest:C, NameDest:C,;
  	theOrder:N, KorSchet:C, BikDest:C, sInnDest:C ;
  local arr, ADest := "", i, str, arrDest := {}, aSumma := {}, sSumma,;
  	dest := {}, aFioDest:={}, aFio:={}, abankDest:={}, sPasp:=cliDocum(),;
	aBank:={}

sSumma = rSumma( summa)
sSumma = Upper(Substr( sSumma, 1, 1)) + Substr( sSumma, 2)
aSumma := razbivka( sSumma, {{ 60, "L"}, { 60, "L"}})
//curDest+=', '+cliDocum()
dest := razbivka( curDest, {{ 72, "L"}, { 72, "L"}})
aFioDest=razbivka(NameDest, {{ 33, "L"}, { 33, "L"}})
aFio=razbivka(cliFullName(), {{ 33, "L"}, { 33, "L"}})
abankDest=razbivka(bankDest, {{ 39, "L"}, { 39, "L"}})
aBank=razbivka(Alltrim(configValue('FILIALNAME')), {{ 39, "L"}, { 39, "L"}})

arr := {{ "@bankplat1", confBankName()},;  // Replaced by TMV  24-12-2001
        { "@bankplat2", confFilial()  },;  // Replaced by TMV  24-12-2001
        { "@otd"     , confPostName() },;
        { "@number"  , padL( str(theOrder,6), 6, "0")},;
        { "@data"    , rDate(setCurDate())  },;
        { "@paspnum" , cliPasSer()+cliPasNum() },;
        { "@paspwho" , cliPasGiv() },;
		  { "@summa"   , strtran( str(summa, 15, 2), '.', '-') },;
        { "@fio1"     , aFio[1] },;
        { "@fio2"     , aFio[2] },;
        { "@accplat" , debSchet      },;
        { "@accpol"  , credSchet     },;
        { "@summastr1"   , aSumma[1]    },;
        { "@summastr2"   , aSumma[2]    },;
        { "@innpol"   , sInnDest    },;
        { "@nazn1",    dest[ 1]      },;
        { "@nazn2",    dest[ 2]      },;
        { "@pasp",    sPasp      },;
        { "@bicpol",  BikDest       },;
        { "@accbankpo", KorSchet      },;
		  { "@accbankpl", setUpKorSchet() },;   //  Replaced by TMV  24-12-2001
		  { "@bicplat"  , setUpStrBic()   },;   //  Replaced by TMV  24-12-2001
        { "@fiopol1", aFioDest[1]      },;
        { "@fiopol2", aFioDest[2]      },;
        { "@bankpol1", abankDest[1]      },;
        { "@bankpol2", abankDest[2]      }}
str := ExtractStrOpen( "platpor")
str := makeZ( str, arr)
arr := CtoA( str, endl())
return arr

// Add by TMV  03-07-2003
// Печать док-ов при переходе с 42309 на 42301
//
funcDef prnTran423009 local sCode, sSchetOld, sSchetNew, arrDummy,;
	nAccRecord := ACC->(RecNo()), nCliRecord := CLIENTS->(RecNo()), aMem,;
	accOrd := ACC->( indexOrd())

if !file('s42309.prn') Then return NIL
	
if Left(ACC->Schet, 5) == '42309' .And.;
		!empty( sSchetNew := getPens42301(CLIENTS->CODE, .T.))
	sSchetOld = ACC->Schet
	ACC->( dbSetOrder(1))
	ACC->(dbSeek( sSchetNew))
// Заявление на открытие
	printOpen('ptc_new')
	ACC->( dbSetOrder(accOrd))
	ACC->(dbGoTo(nAccRecord))

// Заявление на закрытие
	arrDummy := servDummy()
	servDummy( {{ "@schet711",  sSchetOld}})
	printOpen( "tsclo", 1)
	servDummy(arrDummy)

// Заявление на перевод
	aMem = memKred(sSchetOld, sSchetNew, 0, "")
	printAorder(aMem)

// Add by TMV 25-09-2003
// Правила обслуживания вкладов до востребования "Пенсионный"
	aMem = rullServVclad()
	printAorder(aMem)
// Add by TMV 25-09-2003 -- End

endif
return NIL

// Add by TMV  03-07-2003
// Поиск второго пенсионного счета 42301
//
funcDef getPens42301 With sCode:C, bNewAcc:L local rV := "", accOrd, accRec
// CARDNEW
accOrd = ACC->( indexOrd())
accRec = ACC->( recNo())
ACC->( dbSetOrder(2))
ACC->( dbSeek(sCode))
do While ACC->CODE == sCode .And. !ACC->(Eof())
// Replaced by TMV 27-07-2004
//	if Left(ACC->Schet, 5) == if( bNewAcc, '42301', '42309' )
	if  if( bNewAcc, _isTek(ACC->Schet), Left(ACC->Schet, 5) == '42309' )
		rV = ACC->Schet
		Exit
	endif
	ACC->(dbSkip())
enddo
ACC->( dbSetOrder(accOrd))
ACC->( dbGoTo(accRec))

return rV

// Add by TMV 25-09-2003
// Правила обслуживания вкладов до востребования 'Пенсионный'
funcDef rullServVclad local arr, str
	arr := {{ "@fio", cliFullName()},;
   	     { "@curDate", rDate(setCurDate())}}
	str := ExtractStrOpen( "pr575")
	str := makeZ( str, arr)
	arr := CtoA( str, endl())
return arr

// Add by TMV 29-07-2004
// Печать уведомления о смене карточного счета
//
funcDef pCrdOldPrint with schetNew:C, schetOld:C ;
local rV := .T., str,;
	arr := {{'@data', setCurDate()},;
		 {'@schetnew', schetNew},;
		 {'@schetold', schetOld}}

str := ExtractStrOpen( "cruved")
str := makeZ( str, arr)
arr := CtoA( str, endl())
return arr