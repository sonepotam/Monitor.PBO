//17-09-98 Переведена на работу с наборами
//18-12-98 Работа с ассортиментом и наборами одновременно
//25-01-99 Исправлена корявая ошибка при расчете allSum в функции addSum2
//         Если пользователь неск. раз нажимал клавишу "Стрелка вниз" или
//         "Стрелка вверх", то valid-условие отрабатывает, но в
//         get-е пользователь оставался и след. нажатие стрелки снова приводило
//         к отработке условия - сумма снова увеличивалась.
//10-11-99 Лекарства в Заботе+
//23-02-00 Перечитываем остаток
#include "clippext.ch"
#include "common.ch"
#define INIT_PRN CHR(27)+CHR(70 )+CHR(27)+CHR(64 )+CHR(27 )+CHR(18)
#define UPFONT   CHR(27)+CHR(120)+"1"    +CHR(27 )+CHR(107)+"0"+;
                 CHR(27)+CHR(33 )+CHR(32)
#define DOWNFONT CHR(27)+CHR(15 )+CHR(27)+CHR(120)+"0"
#define kodApteka '2K'

static allSum

funcDef isZabotaPresent local arr, rV := .T.
  arr:=assortGet( "08", "65", 1)
  if len( arr) < 1
    messageB( "Отсутствует поставщик услуг !")
    rV := .F.
  endif
return rV

 
procedure zab_feedAssort()
local arr, kol:=1, i, j, strTekNom
local old_area:=select()
local str1, str2, str3
local arr_text:={ }
local rv, rvDate:=''

if !isZabotaPresent() then return 


if setupRead( "ZAB+", @str1)
  rv:=setupRead("WDATE", @rvDate)
  if !rv .or. rvDate != dtos(setCurDate())
    messageB("Не выгружен ассортимент по заботе на текущий день")
    return
  endif
  setupRead( "ZABI", @str2)
  if setupRead( "ZAB3", @str3) = .F.
    str3:="в течение трех рабочих дней"
  endif
  str1:=padr( str1, 60)
  str2:=padr( str2, 60)
  str3:=padr( str3, 35)
  arr:=assortGet( "08", "65", 1)
  if len(arr) > 0
    getup( 8, 10,;
           {{"Введите количество копий для печати ассортимента", block(kol)},;
            {"Получить заказ Вы можете", block(str3)},;
            {"", block(str1)},;
            {"", block(str2)}})
    setupWrite( "ZAB+", @str1)
    setupWrite( "ZABI", @str2)
    setupWrite( "ZAB3", @str3)
    if kol > 0
      aadd(arr_text, '')
      aadd(arr_text, '')
      aadd(arr_text, padc('А С С О Р Т И М Е Н Т', 66))
      aadd(arr_text, padc('продовольственных товаров ЗАО "Забота+"', 66))
      aadd(arr_text, padc(rDate( setCurDate()), 66))
      aadd(arr_text, '|----------------------------------------------------------------|')
      aadd(arr_text, '| N  |         НАИМЕНОВАНИЕ  ТОВАРА         |КОЛИЧЕСТВО|СТОИМОСТЬ|')
      aadd(arr_text, '| п/п|                                      |          |   руб.  |')
      if  right(arr[1][1], 4) != "0000"
      aadd(arr_text, '|----|--------------------------------------|----------|---------|')
      endif
      for j:=1 to len(arr)
        if arr[j][11] > 0
          strTekNom:=str(arr[j][11],3)
        else
          strTekNom:="   "
        endif
        if  right(arr[j][1], 4) == "0000"
      aadd(arr_text, '|----------------------------------------------------------------|')
      aadd(arr_text, '|'+strTekNom+"  "+padr(arr[j][2],59)+'|')
          if !empty(arr[j][3])
      aadd(arr_text, '|     '+padr(arr[j][3],59)+'|')
          endif
      aadd(arr_text, '|----------------------------------------------------------------|')
        else
      aadd(arr_text, "|"+strTekNom+" | "+padr(arr[j][2],37)+"|"+padc(arr[j][4],10)+"|"+;
              padl(alltrim(mstr(arr[j][5])), 9)+"|")
          if !empty(arr[j][3])
      aadd(arr_text, '|    | '+padr(arr[j][3],37)               +'|          |         |')
          endif
        endif
      next
      aadd(arr_text, '|----------------------------------------------------------------|')
      aadd(arr_text, '')
      aadd(arr_text, 'Получить заказ Вы можете '+str3)
      aadd(arr_text, str1)
      aadd(arr_text, str2)
      if !ConfPrintUzel()
        for i:=1 to 14
          aadd(arr_text, '')
        next
      endif
      for j:=1 to kol
        printAOrder( arr_text)
      next
    endif
  else
    messageB("На сегодня ассортимент продуктов не пришел")
  endif
else
  messageB("На данном отделении нельзя оформлять продуктовые заказы")
endif
dbselectarea( old_area)
return

function zab_feedOform()
local kod:=clients->code
local arr, i, j
local old_area:=select()
local str1, str2, str3
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yyyy")
local cNumZak, allNds:=0, allKol:=0
local allSumT:=0, allNdsT:=0, allKolT:=0
local allSumL:=0, allNdsL:=0, allKolL:=0
local tekSchet:="", beznal:=.T.
local arr1:={ }, arr2:={ }, ost, arr3:={ }, arr4
local arr_text:={ }, arr_text1:={ }, arr_wisc
local tipOp, ro, sim, naim, ndoc, zabSchet, lekSchet
local rv, rvDate:=''
local INIT_PRNp, UPFONTp, DOWNFONTp
local ptrSchet := 1
local tipAssort, tipAs, lena
local tekSost, exitZakaz, sum, aOrder
local lek, tov
local ZZZoform

//if confUseAgent() then return .F.
if !isZabotaPresent() then return .F.

if chkPrintOrder() = .F.
  messageB("У клиента есть распечатанные ордера на дом. Нельзя оформлять заказ")
  return .F.
endif

if confPrintDon() .or. confPrintUsel()
  INIT_PRNp :=""
  UPFONTp   :=""
  DOWNFONTp :=""
else
  INIT_PRNp :=INIT_PRN
  UPFONTp   :=UPFONT
  DOWNFONTp :=DOWNFONT
endif

if setupRead( "ZAB3", @str3) .and. zabSch( @zabSchet) .and. lekSch( @lekSchet)
  rv:=setupRead("WDATE", @rvDate)
  if !rv .or. rvDate != dtos(setCurDate())
    messageB("Не выгружен ассортимент по заботе на текущий день")
    return .F.
  endif
  setupRead( "ZAB+", @str1)
  setupRead( "ZABI", @str2)
  //получаем массив код продуктов и названий продуктов
  arr:=assortGet( "08", "65", 2, @arr3)
  if len(arr) > 0
    tekSost:=.F.
    exitZakaz:=.F.
    //
    // маленькая коррекция продуктовой Заботы
    //
    accPrizList( "15 1", @arr1, @arr2)
    do case
       case len( arr1) == 0
         messageB( "У клиента нет текущих счетов !!!")
         return .F.
       case len( arr1) == 1
         ptrSchet := 1
       otherwise
         ptrSchet := 1
         if !getUp( 5, 5, {{ "Оформление продуктового заказа"},;
              { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"},;
              { "Выберите счет", block( ptrSchet), arr1}})
             messageB( "Не выбран счет для оплаты !!!")
             return .F.
         endif
    endcase
    tekSchet := arr2[ ptrSchet, 2]
    //проверим, что по счету нет сообщения
    arr4  := spr_infoSeek(tekSchet)
    if len(arr4) > 0
      messageB(tekSchet+" - "+arr4[1])
      //нельзя работать со счетом
      if arr4[2] = 2
        return .F.
      endif
    endif
    allSum:= 0
    lena:=len(arr3)
    do while .T.
      i := 1
      //23-02-00 перечитываем остаток по счету
      ost   := sumRest( tekSchet, setCurDate())
      ZZZoform := .F.
      //
      browseup( 3,3,;
      {{ " Оформление продуктового заказа  "            },;
       { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"            },;
       { "Клиент : " + cliFullName()                    },;
       ;//{ "Оплата : " + arr1[ ptrSchet]                  },;
       { "Оплата : Счет " + SplitSSchet(tekSchet) + " Остаток " + mstr(ost)},;
       { ""                                             },;
       { "Укажите в столбце 'Заказ' количество товара, которое хочет заказать"},;
       { "клиент. После окончания ввода нажмите клавишу 'Esc' для оформления "},;
       { ""                                             },;
       { "Общая сумма заказа", {|| mStr( allSum / 100)} ,, {|| .F.} },;
       { ""                                            }},;
      {{" N ",          {|| padl( arr3[ i,11],  3)}, .F. },;
       {"     Товар",   {|| padR( arr3[ i, 2], 40)}, .F. },;
       {"Поставка",     {|| padR( arr3[ i, 4],  8)}, .F. },;
       {"Цена",         {|| str(  arr3[ i, 5],6,2)}, .F. },;
       {"Заказ",   ;
           {, block( arr3[ i, 7]),"9999",,;
           {|g| addSum2( g, arr3, i, ost, @tekSost, @exitZakaz)}}, .T.}},;
      {|| pred( @i)       },;
      {|| sled( @i, lena) },;
      {|| i:=1, .T.       },;
      {|| i:=lena, .T.    }, , , {||keyboardEsc( exitZakaz)})
      if exitZakaz
        exit
      elseif allSum > int(round(ost * 100, 0))
        do while .T.
          j:=dialog({"Сумма заказа превышает остаток на счете. Клиент может внести  ",;
                     "часть суммы наличными, вообще отказаться от оформления заказа,",;
                     "снова отредактировать список продуктов, которые он хочет      ",;
                     "получить, или оформить ссуду"},;
                    {"Взнос наличными",;
                     "Отказ от заказа",;
                     "Редактирование",;
                     "Выдача ссуды"})
          if j = 1
            sum:=(allSum - int(round(ost * 100, 0))) / 100
            if getUp( 8, 10,;
                {{"Для оформления заказа клиент должен внести"},;
                 {"на счет "+tekSchet+" сумму  "+str(sum,7,2)},;
                 {"Осуществить приход на счет на сумму", block(sum), "9999.99"}})
              sum:=int(round(sum * 100, 0))
              if sum >= (allSum - int(round(ost * 100, 0)))
                ndoc:=ordIncIn( setCurDate())
                //квитанция и ордер
                aOrder := vznos_nal( ndoc,;
                    setCurDate(), cliFullName(), tekSchet, sum / 100,;
                    cliFullName()/*confOtdName()*/, "Вклад на текущий счет", confKassa(), 16)
                printAOrder( aOrder)
                SaveLastOrder( aOrder, .F.)
                if !sumWrite( tekSchet, sum / 100, setCurDate(), opDecode( "2001"), ndoc)
                  messageB("Ошибка формирования проводки")
                  exitZakaz:=.T.
                endif
                exit
              else
                dialog({"Сумма "+alltrim(mstr(sum / 100))+;
                        " недостаточна для оформления заказа"},;
                       {"Ok"})
              endif
            endif
          elseif j = 2
            exitZakaz:=.T.
            exit
          elseif j = 3
            exit
          elseif j = 4
             ZZZoform := credZab( tekSchet, round(allSum / 100,2))
             if ZZZoform = .T.
               exit
             else
               dialog({"Ссуда не оформлена, внесите",;
                       "недостающую сумму наличными"},;
                      {"Ok"})
             endif
            //  messageB("К сожалению в данной версии программы выдача ссуды "+;
            //           "еще не реализована")
          endif
        enddo
        if j = 1 .or. j = 2 .or. j = 4
          exit        //продолжение оформления определяется в exitZakaz
        elseif j = 3  //продолжим редактирование
          loop
        endif
      elseif allSum <= int(round(ost * 100, 0))
        exit
      endif
    enddo
    //заказ оформлен
    if exitZakaz = .F. .and. allSum > 0
      if ZZZoform = .T. .or.;
           yesno("Оформлять заказ на сумму "+mstr(allSum / 100)+" ?")
        i:=1
        beznal:=.T.
        tipOp:="5011"
        if i > 0 .and. zab_feedND( @cNumZak)
          //получим атрибуты проводки
          operGetInfo( tipOp, @ro, @sim, @naim)
          if beznal = .T.
            ndoc:=ordIncMem( setCurDate())
          else
            ndoc:=ordIncIn( setCurDate())
          endif
          //10-11-99 поиск лекарств
          lek:=.F.
          tov:=.F.
          for i:=1 to len(arr3)
            if arr3[i][7] > 0
              if left(arr3[i][1],2) == '80'
                lek:=.T.
              else
                tov:=.T.
              endif
            endif
          next
          aadd( arr_text, replicate('-', 70))
          aadd( arr_text,  'СЧЕТ-КВИТАНЦИЯ                                  (ЗАКАЗЧИКУ)')
          aadd( arr_text,  alltrim(confFilial())+', отделение N '+padl(confOtdNumb(),3,"0"))
          aadd( arr_text,  'Внимание! Получить заказ Вы можете '+str3)
          aadd( arr_text,  str1)
          aadd( arr_text,  "")
          aadd( arr_text,  str2)
          aadd( arr_text,  INIT_PRNp+DOWNFONTp+'Заказ N '+UPFONTp+cNumZak)
          aadd( arr_text,  INIT_PRNp+DOWNFONTp+" от "+dtoc(setCurDate()))
          //aadd( arr_text,  'Поставщик заказа: ЗАО "Забота+"')
          aadd( arr_text,  '')
          aadd( arr_text,  "Заказчик: "+cliFullName()+;
            " Паспорт: "+alltrim(CliPasSer1())+"-"+alltrim(CliPasSer2())+" N "+CliPasNum())
          aadd( arr_text,  'Заказаны товары:')
          for i:=1 to len(arr3)
            if arr3[i][7] > 0
              //18-12-98 найдем все продукты, входящие в набор
              if arr3[i][10] = .T.
                for j:=1 to len(arr)
                  if left(arr[j][1],2) == left(arr3[i][1],2) .and.;
                       right(arr[j][1],4) != "0000"
                    aadd( arr_text,  padr(arr[j][2],35)+ str(arr3[i][7],4)+"*"+padr(arr[j][4],10)+;
                                 padl(alltrim(mstr( round(arr[j][5]  * arr3[i][7], 2))),8))
                  endif
                next
              //запишем сразу данный продукт
              else
                    aadd( arr_text,  padr(arr3[i][2],35)+str(arr3[i][7],4)+"*"+padr(arr3[i][4],10)+;
                                 padl(alltrim(mstr( round(arr3[i][5] * arr3[i][7], 2))),8))
              endif
            endif
          next
          aadd( arr_text,  'На общую сумму (рублей):'+space(14)+mStr(allSum / 100))
          aadd( arr_text,  '')
          aadd( arr_text,  'Заказ выдан __________  работник пункта выдачи __________')
          if tov
            aadd( arr_text,  replicate('-', 70))
            aadd( arr_text,  'СЧЕТ-ИЗВЕЩЕНИЕ                           получатель ЗАО "ЗАБОТА+"')
            aadd( arr_text,  alltrim(confFilial())+', отделение N '+padl(confOtdNumb(),3,"0"))
            aadd( arr_text,  INIT_PRNp+DOWNFONTp+'Заказ N '+UPFONTp+cNumZak)
            aadd( arr_text,  INIT_PRNp+DOWNFONTp+" от "+dtoc(setCurDate()))
            //aadd( arr_text,  'Поставщик заказа: ЗАО "Забота+"')
            aadd( arr_text,  '')
            aadd( arr_text,  "Заказчик: "+cliFullName()+;
            " Паспорт: "+alltrim(CliPasSer1())+"-"+alltrim(CliPasSer2())+" N "+CliPasNum())
            aadd( arr_text,  'Прошу поставить товары:')
            for i:=1 to len(arr3)
              if arr3[i][7] > 0 .and. left(arr3[i][1],2) != '80'
                //18-12-98 найдем все продукты, входящие в набор
                if arr3[i][10] = .T.
                  for j:=1 to len(arr)
                    if left(arr[j][1],2) == left(arr3[i][1],2) .and.;
                         right(arr[j][1],4) != "0000"
                      aadd( arr_text,  padr(arr[j][2],35)+ str(arr3[i][7],4)+"*"+padr(arr[j][4],10)+;
                                   padl(alltrim(mstr( round(arr[j][5]  * arr3[i][7], 2))),8))
                      allSumT += int(round( arr[j][5]  * arr3[i][7] * 100,0))
                      allNdsT += int(round( arr[j][6]  * arr3[i][7] * 100,0))
                      allKolT++
                    endif
                  next
                //запишем сразу данный продукт
                else
                      aadd( arr_text,  padr(arr3[i][2],35)+str(arr3[i][7],4)+"*"+padr(arr3[i][4],10)+;
                                   padl(alltrim(mstr( round(arr3[i][5] * arr3[i][7], 2))),8))
                      allSumT += int(round( arr3[i][5] * arr3[i][7] * 100,0))
                      allNdsT += int(round( arr3[i][6] * arr3[i][7] * 100,0))
                      allKolT++
                endif
              endif
            next
            aadd( arr_text,  'На общую сумму (рублей):'+space(14)+mStr(allSumT / 100)+;
              ' (в т.ч. НДС '+alltrim(mstr(allNdsT / 100))+')')
            aadd( arr_text, '')
            aadd( arr_text,  space(30)+'Заказчик _______________________________')
          endif
          if lek
            aadd( arr_text,  replicate('-', 70))
            aadd( arr_text,  'СЧЕТ-ИЗВЕЩЕНИЕ                    получатель ООО "Аптека-ЗАБОТА+"')
            aadd( arr_text,  alltrim(confFilial())+', отделение N '+padl(confOtdNumb(),3,"0"))
            aadd( arr_text,  INIT_PRNp+DOWNFONTp+'Заказ N '+UPFONTp+cNumZak)
            aadd( arr_text,  INIT_PRNp+DOWNFONTp+" от "+dtoc(setCurDate()))
            aadd( arr_text,  '')
            aadd( arr_text,  "Заказчик: "+cliFullName()+;
            " Паспорт: "+alltrim(CliPasSer1())+"-"+alltrim(CliPasSer2())+" N "+CliPasNum())
            aadd( arr_text,  'Прошу поставить лекарственные препараты:')
            for i:=1 to len(arr3)
              if arr3[i][7] > 0 .and. left(arr3[i][1],2) == '80'
                //18-12-98 найдем все лекарства
                if arr3[i][10] = .T.
                  for j:=1 to len(arr)
                    if left(arr[j][1],2) == left(arr3[i][1],2) .and.;
                         right(arr[j][1],4) != "0000"
                      aadd( arr_text,  padr(arr[j][2],35)+ str(arr3[i][7],4)+"*"+padr(arr[j][4],10)+;
                                   padl(alltrim(mstr( round(arr[j][5]  * arr3[i][7], 2))),8))
                      allSumL += int(round( arr[j][5]  * arr3[i][7] * 100,0))
                      allNdsL += int(round( arr[j][6]  * arr3[i][7] * 100,0))
                      allKolL++
                    endif
                  next
                //запишем сразу данный продукт
                else
                      aadd( arr_text,  padr(arr3[i][2],35)+str(arr3[i][7],4)+"*"+padr(arr3[i][4],10)+;
                                   padl(alltrim(mstr( round(arr3[i][5] * arr3[i][7], 2))),8))
                      allSumL += int(round( arr3[i][5] * arr3[i][7] * 100,0))
                      allNdsL += int(round( arr3[i][6] * arr3[i][7] * 100,0))
                      allKolL++
                endif
              endif
            next
            aadd( arr_text,  'На общую сумму (рублей):'+space(14)+mStr(allSumL / 100)+;
              '')  //' (в т.ч. НДС '+alltrim(mstr(allNdsL / 100))+')')
            aadd( arr_text, '')
            aadd( arr_text,  space(30)+'Заказчик _______________________________')
          endif
          aadd( arr_text, replicate('-', 70))
          allKol:=allKolT+allKolL
          allNds:=allNdsT+allNdsL
          if beznal = .T.
            aadd( arr_text1,  replicate('-', 70))
            aadd( arr_text1,  'ПОРУЧЕНИЕ                                (Остается в ПБО)')
            aadd( arr_text1,  alltrim(confFilial())+', отделение N '+padl(confOtdNumb(),3,"0"))
            aadd( arr_text1,  INIT_PRNp+DOWNFONTp+'Заказ N '+UPFONTp+cNumZak)
            aadd( arr_text1,  INIT_PRNp+DOWNFONTp+" от "+dtoc(setCurDate()))
            aadd( arr_text1,  '')
            aadd( arr_text1,  'ФИО: '+cliFullName()+;
            " Паспорт: "+alltrim(CliPasSer1())+"-"+alltrim(CliPasSer2())+" N "+CliPasNum())
            aadd( arr_text1,  'Прошу перечислить с моего текущего счета N '+tekSchet)
            if tov
              aadd( arr_text1,  '- в ЗАО "Забота+" оплату за товары в сумме '+;
                                alltrim(mstr(allSumT / 100))+;
                                ' руб. (в т.ч. НДС '+alltrim(mstr(allNdsT / 100))+')')
              aadd( arr_text1,  '('+rSumma(allSumT / 100)+')')
            endif
            if lek
              aadd( arr_text1,  '- в ООО "Аптека-Забота+" оплату за лекарственные препараты')
              aadd( arr_text1,  'в сумме '+alltrim(mstr(allSumL / 100))+;
              ' руб., НДС не облагается')  //(в т.ч. НДС '+alltrim(mstr(allNds / 100))+')')
              aadd( arr_text1,  '('+rSumma(allSumL / 100)+')')
            endif
            if tov
              aadd( arr_text1,  '')
              aadd( arr_text1,  'С положением о приобретении продовольственных товаров в ЗАО "Забота+"')
              aadd( arr_text1,  'с оплатой по безналичному расчету с текущих счетов')// АБ "Петровский"')
              aadd( arr_text1,  confBankName()+' ОЗНАКОМЛЕН.')
            endif
            if lek
              aadd( arr_text1,  '')
              aadd( arr_text1,  'С положением о приобретении лекарственных препаратов в ООО "Аптека-')
              aadd( arr_text1,  'Забота+" с оплатой по безналичному расчету с текущих счетов') // АБ "Петровский"')
              aadd( arr_text1,  confBankName()+' ОЗНАКОМЛЕН.')
            endif
            aadd( arr_text1,  '')
            aadd( arr_text1,  'Ответственный исполнитель______________           Заказчик__________________')
            aadd( arr_text1,  '')
            aadd( arr_text1,  'Штамп отделения')
            aadd( arr_text1,  '---------------------------------------------------------------------')
          else
            if tov
              arr_text1:=vznos_nal( ndoc, setCurDate(), cliFullName(), zabSchet, allSumT / 100,;
                             'ЗАО "Забота+"',;
                             'Оплата заказа продуктов питания. Заказ: '+cNumZak+;
                             'В т.ч. НДС '+alltrim(mStr(allNdsT / 100)),;
                             confKassa(), sim)
            endif
            if lek
              arr_text1:=vznos_nal( ndoc, setCurDate(), cliFullName(), zabSchet, allSumL / 100,;
                             'ООО "Аптека-Забота+"',;
                             'Оплата заказа лекарственных препаратов. Заказ: '+cNumZak+;
                             ' НДС не облагается',;
                             confKassa(), sim)
            endif
          endif
          if !ConfPrintUzel()
            for i:=1 to 14
              aadd(arr_text1, '')
            next
          endif
          printAorder( arr_text)
          SaveLastOrder( arr_text)
          printAorder( arr_text1)
          SaveLastOrder( arr_text1)
          if yesno("Клиент подписал документы ?")
            //делаем проводку
            if beznal = .T.
              if tov
                rv:=sumWrite( tekSchet, -allSumT / 100, setCurDate(), "5011",;
                              ndoc, , cNumZak+";"+alltrim(clients->code))
              endif
              if lek
                //получим новый номер документа, если проводка по
                //продуктам уже была
                if tov
                  ndoc:=ordIncMem( setCurDate())
                endif
                rv:=sumWrite( tekSchet, -allSumL / 100, setCurDate(), "5511",;
                              ndoc, , cNumZak+";"+alltrim(clients->code))
              endif
            else
              rv:=sumWrite( zabSchet,  allSum / 100, setCurDate(), "2012",;
                            ndoc, , cNumZak+";"+alltrim(clients->code))
            endif
            if rv
              //запись в базу желаний
              arr_wisc:={cNumZak,clients->code,setCurDate(),;
                         if(beznal = .F., 1, 2), allSum / 100,;
                         allNds / 100, allKol}
              for i:=1 to len(arr3)
                if arr3[i][7] > 0
                  if arr3[i][10] = .T.
                    for j:=1 to len(arr)
                      if left(arr[j][1],2) == left(arr3[i][1],2) .and.;
                           right(arr[j][1],4) != "0000"
                        aadd(arr_wisc, arr[j][1])
                        aadd(arr_wisc, arr3[i][7])
                      endif
                    next
                  else
                        aadd(arr_wisc, arr3[i][1])
                        aadd(arr_wisc, arr3[i][7])
                  endif
                endif
              next
              if wisc->(mappend())
                wisc->schet :=if(beznal = .T., tekSchet, zabSchet)
                wisc->type  :="981"
                wisc->buffer:=sborka( arr_wisc)
                wisc->(netunlock())
              endif
            else
              messageB("Проводка не сделана, заказ не оформлен")
            endif
          else
            messageB("Вы отказались от оформления заказа !!!")
          endif
        endif
      endif
    endif
  else
    messageB("На сегодня ассортимент продуктов не пришел")
  endif
else
  messageB("Необходимо распечатать ассортимент продуктов за сегодня")
endif
set(_SET_DATEFORMAT, old_format)
dbselectarea( old_area)
return .F.

static function assBySample( arr)
local curSample, nStart, nCount, i := 1, cnt 
  while i <= len( arr)
    curSample := left( arr[ i, 1], 2)
    cnt       := 0
    if right( arr[ i, 1], 4) == "0000" then i++
    nStart := i
    while i <= len( arr) .AND. left( arr[ i, 1], 2) == curSample
      cnt++; i++
    enddo
    if cnt > 0 then arr := aSort( arr, nStart, cnt, {|x, y| x[ 2] < y[ 2]})
  enddo
return arr

static function assortGet( kod2, kod3, priz, arr3)
local arr:={ }, arr1:={ }
local a_str, sum, nds, kod, ind
local i, lena, pri:=.F., str1
local tekNom

if baseMOpen(assinfo())
  assort->(dbseek(kod2))
  do while !assort->(eof()) .and. assort->code2 == kod2
    kod:=alltrim(assort->code)
    if assort->code3 == kod3 //.and. len(kod) = 6
      a_str:=razborka(assort->buffer, ";", 3)
      sum  :=val(a_str[2])
      nds  :=val(a_str[3])
      aadd(arr, {kod, substr(assort->name,1,40),;
                 substr(assort->name,41), a_str[1],;
                 sum, nds, 0, 0, assort->live, .F., 0})
    endif
    assort->(dbskip())
  enddo
  baseMClose(assinfo())
  //отсортируем по возрастанию кодов
  asort(arr, , , {|x,y| x[1] < y[1]})
  //добавим пустые строки разделов там, где нет строк, заканчивающихся "0000"
  if priz != 3 .and. len(arr) > 0
    lena:=len(arr)
    str1:="--"
    for i:=1 to lena
      if str1 != left(arr[i][1],2)
        str1:=left(arr[i][1],2)
        if right(arr[i][1],4) != "0000"
          pri:=.T.
          aadd(arr, {str1+"0000", "", "", "",;
                     0, 0, 0, 0, .T., .F., 0})
        endif
      endif
    next
  endif
  //снова отсортируем, если добавляли строки
  if pri
    asort(arr, , , {|x,y| x[1] < y[1]})
  endif
  arr := assBySample( arr)
  tekNom:=1
  //подготовим суммы по наборам
  if priz != 3
    pri := .F.
    for i:=1 to len(arr)
      //установим признак набора для данного раздела
      if right(arr[i][1],4) == "0000" .and. len(arr[i][2]) >= 5 .and.;
           upper(left(arr[i][2],5)) = "НАБОР"
        ind :=i
        arr[i][10]:=.T.
        arr[i][4] :="НАБОР"
        arr[i][11]:=tekNom
        tekNom++
        str1:=left(arr[i][1],2)
        pri :=.T.
      endif
      //дополним сумму и НДС по разделу, если товар принадлежит набору
      if pri = .T. .and. str1 == left(arr[i][1],2)
        if right(arr[i][1],4) != "0000"
          arr[ind][5] += arr[i][5]
          arr[ind][6] += arr[i][6]
          arr[ind][9] := arr[i][9]
        endif
      //иначе сбросим признак набора
      else
        pri :=.F.
        str1:="--"
        if right(arr[i][1],4) != "0000"
          arr[i][11]:=tekNom
          tekNom++
        endif
      endif
    next
  endif
  //подготовим и массив для просмотра
  if priz = 2
    pri :=.F.
    arr3:={ }
    for i:=1 to len(arr)
      //начало раздела
      if right(arr[i][1],4) == "0000"
        str1:=left(arr[i][1],2)
        //набор добавляем сразу
        if arr[i][10] = .T.
          aadd(arr3, { arr[i][1], arr[i][2], arr[i][3], arr[i][4], arr[i][5],;
                       arr[i][6], arr[i][7], arr[i][8], arr[i][9], arr[i][10],;
                       arr[i][11]})
          pri:=.T.
        else
          pri:=.F.
        endif
      else
        //если это не раздел с набором, то добавим в массив для просмотра
        if pri = .F.
          aadd(arr3, { arr[i][1], arr[i][2], arr[i][3], arr[i][4], arr[i][5],;
                       arr[i][6], arr[i][7], arr[i][8], arr[i][9], arr[i][10],;
                       arr[i][11]})
        endif
      endif
    next
  endif
endif
return arr

function razborka( str, sim, kol, num)
local arr:={ }, i
do while .T.
  i:=at( sim, str)
  if i > 0
    aadd(arr, substr(str, 1, i - 1))
    str:=substr(str, i+1)
  else
    if len(str) > 0
      aadd( arr, str)
    endif
    exit
  endif
  if valType(num) == "N" .and. num == len(arr)
    return arr[num]
  endif
enddo
if valType(num) == "N"
  if num <= len(arr)
    return arr[num]
  else
    return ""
  endif
endif
if valType(kol) == "N" .and. len(arr) < kol
  for i:=len(arr)+1 to kol
    aadd(arr, "")
  next
endif
return arr

function sborka( arr)
local str:="", i, str1
for i:=1 to len(arr)
  if valType(arr[i]) == "C"
    str += alltrim(arr[i])
  elseif valType(arr[i]) == "N"
    str1 := alltrim(str(arr[i],15,2))
    if right(str1,3) == ".00"
      str += substr( str1, 1, len(str1) - 3)
    else
      str += str1
    endif
  elseif valType(arr[i]) == "D"
    str += dtoc(arr[i])
  endif
  str += ";"
next
return str

return str

function addSum2( g, arr, i, ost, tekSost, exitZakaz)
local rV := .T., kol, sum, tmpSum, tmpSum1, j
  tmpSum :=0
  for j:=1 to len(arr)
    if j != i
      kol    := arr[ j, 7]
      if kol > 0
        sum    := arr[ j, 5]
        tmpSum := tmpSum + int( round( kol      * sum * 100, 0))
      endif
    endif
  next
  sum    := arr[ i, 5]
  tmpSum := tmpSum + int( round( g:varGet() * sum * 100, 0))
  if round( tmpSum/ 100, 2) > round( ost, 2)
     if tekSost = .F.
       j:=dialog({"Сумма заказа превышает остаток на счете",;
                  "Продолжать оформление заказа ?"},;
                 {"Да","Нет"}) //, контролировать остаток на счете"})
       if j = 1
         allSum := tmpSum
         tekSost:=.T.
       else
         //отказ от оформления заказа
         g:varput( 0)
         g:display()
         exitZakaz:=.T.
         //clear typeahead
         //keyboard chr(27) //не отрабатывает здесь выход по ESC
       endif
     else
       tmpSum1 := tmpSum / 100 - ost
       j:=dialog({"Сумма заказа превышает остаток на счете на ",;
                  alltrim(mStr( tmpSum1)) + " руб."},;
                 {"Клиент согласен", "Клиент против"})
       if j = 1
         allSum := tmpSum
       else
         g:varput( 0)
       endif
     endif
     rV := .T.
  else
     tekSost:=.F.
     allSum := tmpSum
  endif
return rV

function setupRead( priz, str)
local old_area, rv:=.F.
old_area:=select()
USE (setupinfo()[1]) NEW ALIAS setup READONLY SHARED
if neterr() = .F.
  locate for rtrim(setup->paramname) == priz
  if found()
    rv:=.T.
    str:=rtrim(setup->paramvalue)
  else
    str:=""
  endif
endif
setup->(dbclosearea())
dbselectarea(old_area)
return rv

function setupWrite( priz, str)
local old_area, rv:=.F.
old_area:=select()
USE (setupinfo()[1]) NEW ALIAS setup SHARED
if neterr() = .F.
  locate for rtrim(setup->paramname) == priz
  if found()
    if setup->(mrlock())
      rv:=.T.
      setup->paramvalue:=str
      setup->(netunlock())
    endif
  else
    if setup->(mappend())
      setup->paramname :=priz
      setup->paramvalue:=str
      setup->(netunlock())
    endif
  endif
endif
setup->(dbclosearea())
dbselectarea(old_area)
return rv

function zab_feedND( nd)
local arr:={}, rv:=.F.
local str1
if setupRead( "ZABP", @str1)
  if len(alltrim(str1)) < 4
    nd:=confZabPDoc( padl(alltrim( str1),3,"0") )
    rv:=.T.
  else
    messageB("Неверная длина кода пункта выдачи продуктов. Заказы не оформляются")
  endif
else
  messageB("Не указан код пункта выдачи продуктов. Заказы не оформляются")
endif

return rv

procedure zab_feedBrowse()
local old_area:=select()
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yyyy")
local str1
local arrZakaz

if !isZabotaPresent() then return 


clients->(basesavepos())
//найдем код пункта выдачи
setupRead( "ZABP", @str1)
if len(str1) < 4
  arrZakaz:=assortGet( "08", "65", 3)
  if len(arrZakaz) > 0
    str1:=padl(str1, 3, "0")
    mselect("wisc")
    wisc->(basesavepos())
    wisc->(dbsetorder(2))
    clients->(dbsetorder(1))
    if wisc->(dbseek( "981")) .and. alltrim(wisc->type) == "981"
        browseup( 1, 1,;
         {{"               ОФОРМЛЕННЫЕ   ПРОДУКТОВЫЕ   ЗАКАЗЫ               "}},;
         {{"Номер заказа",       {|| ret1()},       .F.},;
          {"Клиент ",            {|| ret2()},       .F.},;
          {"Оплата",             {|| ret3()},       .F.},;
          {"Общая сумма заказа", {|| mStr(ret4())}, .F.}},;
         {|| wisc->(wskipup())         },;
         {|| wisc->(wskipdown())       },;
         {|| wisc->(wgotop())          },;
         {|| wisc->(wgobottom())       },,,,{|| sorry()},;
         {{},;
          {{"Ордер",  {|| wmem(arrZakaz)}  }},;
          {},;
          {},;
          {},;
          {},;
          {{"Поиск",  {|| wseek( str1)} }},;
          {{"Удален", {|| wdel(arrZakaz)}  }} })
    else
      messageB("На отделении еще не оформлялись продуктовые заказы")
    endif
    wisc->(baserestpos())
  else
    messageB("На сегодня ассортимент продуктов не пришел")
  endif
else
  messageB("Неверно указан код пункта выдачи заказов")
endif
clients->(baserestpos())
dbselectarea(old_area)
set(_SET_DATEFORMAT, old_format)
return

static function ret1()
return left(wisc->buffer,14)

static function ret2()
local str:=razborka(wisc->buffer,";", ,2)
if clients->(dbseek(str))
  str:=padr(cliFullName(), 35)
else
  str:=space(35)
endif
return str

static function ret3()
local str:=razborka(wisc->buffer,";", ,4)
return if( val(str) = 1, "Налич.", "Безнал")

static function ret4()
local str:=razborka(wisc->buffer,";", ,5)
return val(str)

static function ret5()
local str:=razborka(wisc->buffer,";", ,3)
return str

static function wskipup()
local old_rec:=wisc->(recno()), rv:=.T.
wisc->(dbskip(-1))
if wisc->(bof()) .or. alltrim(wisc->type) != "981"
  wisc->(dbgoto( old_rec))
  rv:=.F.
endif
return rv

static function wskipdown()
local old_rec:=wisc->(recno()), rv:=.T.
wisc->(dbskip())
if wisc->(eof()) .or. alltrim(wisc->type) != "981"
  wisc->(dbgoto( old_rec))
  rv:=.F.
endif
return rv

static function wgotop()
local old_rec:=wisc->(recno()), rv:=.T.
wisc->(dbseek("981", .T.))
if alltrim(wisc->type) != "981"
  wisc->(dbgoto( old_rec))
  rv:=.F.
endif
return rv

static function wgobottom()
local old_rec:=wisc->(recno()), rv:=.T.
wisc->(dbseek("9810", .T.))
wisc->(dbskip(-1))
if alltrim(wisc->type) != "981"
  wisc->(dbgoto( old_rec))
  rv:=.F.
endif
return rv

static procedure wseek( kod)
local ob:=getCurOb(), rv:=.F.
local str, old_rec, new_rec
str := kod+padl(confOtdNumb(),3,"0")+substr(dtos(setCurDate()),4)+"   "
if getUp(8,10,{{"Введите номер заказа", block(str)}}) .and. len(alltrim(str)) = 14
  old_rec:=wisc->(recno())
  wisc->(wgotop())
  do while .T.
    if left(wisc->buffer,14) == str
      rv:=.T.
      new_rec:=wisc->(recno())
      exit
    endif
    if wisc->(wskipdown()) = .F.
      exit
    endif
  enddo
  if rv
    oB: refreshAll()
    oB: forceStable()
    while wisc->( recNo()) != new_rec .and. !wisc->(bof())
      oB: up()
      oB: forceStable()
    endDo
  else
    messageB("Не найден заказ с номером "+str)
    wisc->(dbgotop( old_rec))
  endif
endif
return

static procedure wdel(arrZakaz)
local tip, zabSchet, dto, sum, buf, kliSchet
local new_rec:=0, ob:=getCurOb()
local rv
local numP, kolP, sumP, sumT, sumL, err, kolZakaz, ind

if yesno("Вы действительно желаете удалить заказ N "+ret1()+" ?")
  //найдем запись, на которую встанем после удаления данного счета
  wisc->(dbskip())
  if !wisc->(eof()) .and. alltrim(wisc->type) == "981"
    new_rec:=wisc->(recno())
  endif
  wisc->(dbskip(-1))
  if new_rec = 0
    wisc->(dbskip(-1))
    if !wisc->(bof()) .and. alltrim(wisc->type) == "981"
      new_rec:=wisc->(recno())
    endif
    if !wisc->(bof()) //при достижении начала файла стоим на первой записи
      wisc->(dbskip())
    endif
  endif
  kliSchet:=wisc->schet
  tip:=val( razborka(wisc->buffer,";", ,4) )
  dto:=ctod(razborka(wisc->buffer,";", ,3) )
  sum:=val(razborka(wisc->buffer,";", ,5))
  buf:=razborka(wisc->buffer,";", ,1)
  //анализ данных в заказе (продовольственные товары и
  //лекарственные препараты)
        kolZakaz:=val(razborka(wisc->buffer,";", , 7))
        sumT:=0
        sumL:=0
        for ind:=1 to 2*kolZakaz step 2
          numP  := razborka(wisc->buffer,";", , 7+ind)
          kolP  := val(razborka(wisc->buffer,";", , 8+ind))
          sumP  := sumFromAssort( arrZakaz, numP)
          if sumP < 0
            messageb('Ошибка в ассортименте по товару с кодом '+numP)
            err++
            exit
          endif
          sumP  := int(round( sumP * kolP * 100,0))
          if left(numP,2) == "80"
            sumL+= sumP
          else
            sumT+= sumP
          endif
        next
  //в буфер проводки занесен код клиента и номер заказа
  buf:=alltrim(buf+";"+razborka(wisc->buffer,";", ,2))
  if wisc->(mdelete())
    if new_rec != 0
      wisc->(dbgoto( new_rec))
      ob:refreshAll()
      ob:forceStable()
      do while !wisc->(bof()) .and. wisc->(recno()) != new_rec
        ob:up()
        ob:forceStable()
      enddo
    else
      keyboard chr(27)  //выход из browseUp
    endif
    //удаляем проводки
    if sumT > 0
      sumT:=round(sumT / 100, 2)
      rv:=sumDelete( kliSchet,;
                     if(tip = 2, -sumT, sumT),;
                     if(tip = 2, "5011", "2012"), dto, buf)
      if rv = .F.
        messageB("Не найдена проводка по оплате удаленного заказа (продукты)")
      endif
    endif
    if sumL > 0
      sumL:=round(sumL / 100, 2)
      rv:=sumDelete( kliSchet,;
                     if(tip = 2, -sumL, sumL),;
                     if(tip = 2, "5511", "2012"), dto, buf)
      if rv = .F.
        messageB("Не найдена проводка по оплате удаленного заказа (лекарства)")
      endif
    endif
  endif
endif
return

function sumDelete( schet, sum, typeof, dto, buf)
local rv:=.F.
local old_area:=select()

sums->(basesavepos())
sums->(dbsetorder(1))
sums->(dbseek(schet))
do while !sums->(eof()) .and. sums->schet == schet
  if sums->ddate = dto .and. sums->summa = int(round(sum * 100,0)) .and.;
       sums->typeof == typeof .and. (empty(buf) .or. buf == alltrim(sums->buffer))
    if sums->(mdelete())
      if kasTypes( typeof)
        kasDeleteSumma( sum, "999")
      endif
      rv:=.T.
    endif
  endif
  sums->(dbskip())
enddo
sums->(baserestpos())
dbselectarea(old_area)
return rv

function kasDeleteSumma( summa, kasCode)
local opened, rv, old_area:=select()
local sum:=0

opened := select( "KASSA") >0
if opened =.F.
  rV:=baseNSOpen( kasInfo())
endif
if opened .OR. rV
    if KASSA ->( dbSeek( kasCode))
      if KASSA ->( mRLock())
        //изменяем остаток по кассе
        if summa > 0
          KASSA ->summa := KASSA ->summa - summa * 100 //остаток уменьшаем
        else
          KASSA ->summa := KASSA ->summa - summa * 100 //остаток увеличиваем
        endif
        //уменьшаем соответ. обороты
        if summa > 0
          KASSA ->obK := KASSA ->obK - summa * 100
        else
          KASSA ->obD := KASSA ->obD + summa * 100
        endif
        rv:=summa
        KASSA ->( netUnLock())
      endif
    endif
  if opened = .F.
    baseMClose( kasInfo())
  endif
  dbselectarea(old_area)
endif
return rv

static procedure wmem( arrZakaz)
local old_format:=set(_SET_DATEFORMAT, "dd.mm.yyyy")
local zabSchet, lekSchet
local sum:=0, sum1, kolP, numP, sumT, sumL, sumP, priz:=.F.
local arr:={"","",""}
local kol:=0, ind, kolZakaz, err:=0

if zabSch( @zabSchet) .and. lekSch( @lekSchet)
  wisc->(basesavepos())
  wisc->(wgotop())
  do while .T.
    if ret3() == 'Безнал'
      priz :=.T.
      exit
    endif
    if wisc->(wskipdown()) = .F.
      exit
    endif
  enddo
  if priz = .T.
    aadd(arr, space(65)+dtoc(setCurDate()))
    aadd(arr, '')
    aadd(arr, padc( "СВОДНЫЙ МЕМОРИАЛЬНЫЙ ОРДЕР N ", 78))
    aadd(arr, '')
    aadd(arr, replicate("-", 78))
    aadd(arr, "|    ДАТА    |    ДЕБЕТ ( N сч.)    |    КРЕДИТ ( N сч.)   |      СУММА      |")
    aadd(arr, replicate("-", 78))
    do while .T.
      if ret3() == 'Безнал'
        kolZakaz:=val(razborka(wisc->buffer,";", , 7))
        sumT:=0
        sumL:=0
        for ind:=1 to 2*kolZakaz step 2
          numP  := razborka(wisc->buffer,";", , 7+ind)
          kolP  := val(razborka(wisc->buffer,";", , 8+ind))
          sumP  := sumFromAssort( arrZakaz, numP)
          if sumP < 0
            messageb('Ошибка в ассортименте по товару с кодом '+numP)
            err++
            exit
          endif
          sumP  := int(round( sumP * kolP * 100,0))
          if left(numP,2) == "80"
            sumL+= sumP
          else
            sumT+= sumP
          endif
        next
        sum1:= ret4()
        sum += int(round( sum1 * 100, 0))
        kol++
        if sumT > 0
    aadd(arr, '| '+ret5()+' | '+wisc->schet+' | '+zabSchet+' | '+mStr(sumT / 100, 15)+' |')
    aadd(arr, replicate("-", 78))
        endif
        if sumL > 0
    aadd(arr, '| '+ret5()+' | '+wisc->schet+' | '+lekSchet+' | '+mStr(sumL / 100, 15)+' |')
    aadd(arr, replicate("-", 78))
        endif
      endif
      if wisc->(wskipdown()) = .F.
        exit
      endif
    enddo
    aadd(arr, '')
    aadd(arr, 'Назначение платежа. Оплата продовольственных заказов.')
    aadd(arr, '                    Количество '+alltrim(str(kol,6))+;
                                   '; сумма '+alltrim(mStr(sum / 100, 15)) )
    aadd(arr, '')
    aadd(arr, '                          КОНТРОЛЕР:____________________')
    aadd(arr, '')
    aadd(arr, '                          БУХГАЛТЕР:____________________')
    aadd(arr, '')
    aadd(arr, '')
    aadd(arr, '')
    aadd(arr, '')
    aadd(arr, '')
    if err = 0
      printAorder( arr)
    endif
  else
    messageB("Отсутствуют заказы, оплаченные со счетов клиентов")
  endif
  wisc->(baserestpos())
endif
set(_SET_DATEFORMAT, old_format)
return

function sumFromAssort( arr, numP)
local i
i:=ascan(arr, {|x| x[1] == numP})
if i > 0
  return arr[i][5]
endif
return -1

procedure zab_feedMes()

if !isZabotaPresent() then return 

if file( newFPath("zab_send.txt", confPathOD()))
  protView( newFPath("zab_send.txt", confPathOD()))
else
  messageb("Отсутствует файл с сообщением")
endif
return

function zabSch( zabSchet)
local arr:={ }, rv:=.F.
provGetInfo( "08", "65", , , , @arr)
if len(arr) = 1
  zabSchet:= arr[1]
  rv:=.T.
else
  messageB("Не указан поставщик услуги по продуктовым заказам")
endif
return rv

function lekSch( lekSchet)
local arr:={ }, rv:=.F.
provGetInfo( "08", kodApteka, , , , @arr)
if len(arr) = 1
  lekSchet:= arr[1]
  rv:=.T.
else
  messageB("Не указан поставщик услуги по лекарственным препаратам")
endif
return rv


//
// функция изменена
//

static funcDef getZabAssort with zabName:C, arr:A, arr_Text:A ;
   local i
  aadd(arr_text, '                          СПИСОК')
  aadd(arr_text, '           пунктов продажи товаров в рассрочку')
  aadd(arr_text, '                    через ' + zabName)
  aadd(arr_text, '')
  aadd(arr_text, '|-----------------------------------------------------------|')
  aadd(arr_text, '| Код |          Наименование и адрес          |   Статус   |')
  aadd(arr_text, '|-----|----------------------------------------|------------|')
  for i:=1 to len(arr)
    aadd(arr_text, '| '+padl(arr[i][1],3,"0")+' |'+padl(arr[i][2],40)+'| '+;
    if(arr[i][9], "РАБОТАЕТ ", "ВР.ЗАКРЫТ")+'  |')
  next
  aadd(arr_text, '|-----------------------------------------------------------|')
  aadd(arr_text, '')
  aadd(arr_text, '')
return NIL

procedure zab_tovAssort()
local arr1, i, j, kol:=1, arr2, arr3
local arr_text:={ ""}
local arrName, arrCode, ptrProv, arr, zabName


fillProvList( "06", @arrName, @arrCode)
for i := 1 to len( arrCode)
  arr := assortGet( "06", arrCode[ i], 3)
  if len( arr) > 0
    zabName := ""
    if ( ptrProv := aTrimmedScan( _prov2Code(), arrCode[ i])) > 0
      zabName := _prov2Name()[ ptrProv]
    endif
    getZabAssort( zabName, arr, @arr_Text)
  endif
next
if len( arr_Text) > 1 .AND. ;
   getUp(8,10,{{"Введите количество копий для печати", block(kol)}})
    for j:=1 to kol
      printAorder(arr_text)
    next
else
  messageB("список магазинов не пришел на отделение")
endif

return

procedure keyboardESC( exitZakaz)
if exitZakaz
  keyboard chr(27)
endif
return

