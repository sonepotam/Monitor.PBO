*============================================================================
*
*                  Программа : MON.EXE
*                  Файл      : baseaccn.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: Описание файла счетов
*
*----------------------------------------------------------------------------
*
*         В поле buffer пишется признак надомника в следующем формате
*   !<день_выплаты_2_сивола><состояние_ордера_1_символ>
*   Состояния ордера могут быть следующими :
*   0) ничего
*   1) распечатан
*   2) вручен
*   3) уничтожен
*
*   accInfo()                  -> dbInfo
*   ac]cPath( [ newPath])       -> dbInfo
*   accOpen()                  -> T/F
*   accXOpen()                 -> T/F
*   accClose()                 -> NIL
*   accCreate()                -> NIL
*
*   accSeek(  schet, softSeek) -> T/F
*   accSumma( schet, dDate)    -> summa  поиск остатка по счету из ACCOUNTS
*
*   accIsHome()                -> T/F    надомник ?
*   accSetHome( [ date])       -> NIL    установить признак надомника
*   accSetOrderType( order)    -> NIL    установить тип ордера
*   accGetOrderType()          -> order  получить признак ордера
*   accGetDay()                -> nDay   получить день выплаты
*
*
*----------------------------------------------------------------------------
* accTrans 19-06-2002 TMV
* убрана обработка условия - последний элемент в списке счетов кредита или нет
*
* chkAccIncom 22-07-2002
* Изменен вызов ф-ии vidArray - явно указан параметр useLive=.F.
*
* 16-12-2002 TMV
* Изменена операция уплаты процентов по ссуде с 2004 на 6137
*  ф-ия credMustPct()
*
* 17-12-2002 TMV
* Изменен параметр "счет" в вызове процедуры (как было) в credMustPct()
*---- ОТМЕНЕНО
*
*
* 26-12-2002 TMV
* При проверке на наличие непогашеных процентов по ссуде исключены
*  закрытие ссудные счета. Ф-ия credMustPct()
*
* 10-01-2003 TMV
* При выдаче ссуды вставлена проверка - выдавалась и гасилась ли уже сегодня
*  ссуда этому клиенту. Ф-ия accCredOpen(), credMustPct()
*
* 18-02-2003  TMV
* К названию филиала в мем. ордере добавлено название банка. Ф-ия accTrans().
*
* 30-10-2003  TMV для Питера
* Для текущих счетов при выдаче вставлена проверка - если заявление на открытие
* не подписано, оно печатается и, если опять "не подписано" - операция
* отменяется.
* Изменена ф-ия accDec2().
* Новая ф-ия DogisPrint().
*
* 11-11-2003  TMV для Питера
* Исключена возможность выдачи кредита по старой схеме, если LOAN = 1
* Ф-ия accCredOpen()
*
* 08-01-2004  TMV для Питера
* Изменена проверка на "погашенность" процентов в ф-ии credMustPct()
*
* 25-03-2004 TMV
* Новый порядок работы с картами через OpenWay
* Ф-ии safePrintOpen()
*
* 30-03-2004  TMV
* Введен новый тип счета - 15 2  125Х
* Ф-ия accTrans()
*
* 29-07-2007 TMV
* Изменение карточных счетов на 45509
* Ф-ии bal716().
* Новые ф-ии accCrdOldAcc(), accCrdPrnOld()
*
* 23-08-2004 TAT
* процедура контроля возможности заказа новой карты
* Обращение к BaseWisc\wsOrderCnt
*
* 20-09-2004 TAT
* длина поля BUFFER в ACCOUNT = 31 (вместо 10)
*
* 02-12-2004 TAT
* на печати ордеров заменено назначение платежа: 
* "Вклад на текущий счет" -> "Взнос на вклад до востребования", "Взнос на карточный счет"
* "Выплата с текущего счета" ->  "Выплата со вклада до востребования", "Выплата с карточного счета"
*
* TAT 05-02-05 
* в chkAddSchet() - добавлен анализ счетов просроченных процентов
*
* 21.06.2005 OVD
* в accNew20Schet() - производится проверка на наличие маски при открытии счета
* при наличии маски символы добавляются в номер счета, функция accMaskSchet()
*
*----------------------------------------------------------------------------
#include "common.ch"
#include "clippExt.ch"
#include "getExit.ch"
#include "inkey.ch"
#include "mon.ch"
//#include "bases.ch"   //OVD
*******
#define DBI_NAME    1
#define DBI_ALIAS   2
#define DBI_STRUCT  3
#define DBI_INDEXES 4
#define DBI_APPENDS 5
#define DBI_NOCHECK 6

#define NTX_NAME    1
#define NTX_FORMULA 2
#define NTX_UNIQUE  3

#define setMultiTag() .F.

func accInfo
static dbInfo := { ".\ACCOUNTS.DBF", "ACC",;
                  {{ "code",     "C", 12} ,;
                   { "schet",    "C", 20} ,;
                   { "priz",     "C", 12} ,;
                   { "rest",     "N", 15} ,;
                   { "datOpen",  "D",  8} ,;
                   { "datClose", "D",  8} ,;
                   { "datPens",  "D",  8} ,;
                   { "typeOf",   "C",  2} ,;
                   { "buffer",   "C", 31} ,;
                   { "lEdited",  "L",  1} ,;
                   { "begPct",   "C",  7} ,;
                   { "Paym_Date","D",  8}},;
                  {{ , "schet"    }  ,;
                   { , "code"     }  ,;
                   { , "lEdited"  }  ,;
                   { , "left( schet, 5) + right( schet, 7)"}} ,;
                  0}
return dbInfo

func bikInfo
local dbbikInfo := { ".\BNKSEEK.DBF", "BNKBIK",;
                  {},;
                  {{ , "NEWNUM"}} ,;
                  0}
return dbbikInfo

funcDef accPath with newPath:CU
return basePath( accInfo(), newPath)

funcDef accOpen
return baseMOpen( accInfo())

funcDef accXOpen
return baseMXOpen( accInfo())

funcDef accClose
return baseMClose( accInfo())

funcDef accCreate
return baseCreate( accInfo())

funcDef accSeek with schet:C, softSeek:LU
return ACC ->( netSeek( schet, softSeek))

funcDef accRLock
return ACC ->( mRLock())

funcDef accUnLock
   ACC ->lEdited := .T.
return ACC ->( netUnLock())

funcDef accSumma with Schet:C, dDate:DU local rV := 0.00

  if ACC ->schet <> schet
     ACC ->( baseSavePos())
     ACC ->( dbSetOrder( 1))
     if accSeek( schet) then rV := ACC ->rest/ 100
     ACC ->( baseRestPos())
  else
     rV := ACC ->rest/ 100
  endif

return rV

funcDef accIsHome
return left( ACC ->buffer, 1) = "!"

funcDef accSetHome with date:DNCU local b

DEFAULT date TO setCurDate()
if isDate( date)
   b := "!" + padL( Day( date), 2, "0") + ORDER_RESERVED
else
   b := "!" + padL( date, 2, "0") + ORDER_RESERVED
endif
ACC ->buffer := b + SubStr( ACC ->buffer, 5)
return NIL

funcDef accGetHome local rV
  rV := val( subStr( ACC ->buffer, 2, 2))
return rV

funcDef accSetOrderType with order:N

if accIsHome()
   ACC ->buffer := left(   ACC ->buffer, 3) + nStr( order) + ;
                   SubStr( ACC ->buffer, 5)
endif
return NIL

funcDef accGetOrderType local rV := -1
if accIsHome() then rV := subStr( ACC ->buffer, 4, 1)
return rV

funcDef accGetDay local rV := 0
if accIsHome() then rV := val( subStr( ACC ->buffer, 2, 2)) // Mike !!!!!!!!!!!
return rV

funcDef accNotif with newValue:CU local oldValue := subStr( ACC ->buffer, 8, 1)
   if !empty( newValue)
      ACC ->buffer := left( ACC ->buffer, 7) + left( newValue, 1) + ;
            subStr( ACC ->buffer, 9)
   endif
return oldValue

funcDef accSetPZab with newValue:CU local oldValue := subStr( ACC ->buffer, 9, 1)
   if !empty( newValue)
      ACC ->buffer := left( ACC ->buffer, 8) + left( newValue, 1) + ;
            subStr( ACC ->buffer, 10)
   endif
return oldValue

//
// последний занятый счет
//
funcDef accLastSchet with balSchet:C local rV := "", template, schet

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 1))

template := padL( confOtdNumb(), 3, "0")
schet    := balSchet + template + "50000"

ACC ->( dbSeek( schet, .T.))

if ACC ->schet <> schet
   ACC ->( dbSkip( -1))
   if left( lTrim( ACC ->schet), 3) <> lTrim( balSchet) .OR. ACC ->( Bof())
      rV := padL( balSchet + template + "00000", 20)
   else
      rV := ACC ->schet
   endif
else
   accSchetAbort()
endif

ACC ->( baseRestPos())
return rV

funcDef accSchetAbort
   errLog( { "Вы исчерпали лимит счетов", "",;
            "Немедленно позвоните на филиал",;
            "дальнейшая работа с программой невозможна !"})
   close all
   setColor( "w/n")
   cls; quit
return NIL

funcDef accNewMask with cPriz:C ;
   local rV := "", arr1 := {}, arr2 := {}, tempP, Simvol, j

   vidArray( cPriz, @arr1, @arr2)

   if len( arr2) <> 0
      tempP := allTrim( subStr( arr2[ 1, 8], 26, 7))
      if len( tempP) > 0
         for j = 1 to 7
            Simvol := subStr( tempP, j, 1)
            if IsdiGit( Simvol)
               tempP := PosRepl( tempP, Simvol, j)
            else
               tempP := PosRepl( tempP, " ", j)
            endif
         next
         rV := allTrim( tempP)
      endif
   endif
return rV

* ------------ получение 1 свободного номера счета для нового плана счетов
funcDef accNew20Schet with balSchet:C, cPriz:C ;
   local newSchet, seekStr, clientKey, clientKey1, ;
         Maska := "", j, Simvol, rV := newSchet, ;
         Poisk := "0050000", lMaska := 0, aaa,bbb

   Maska := accNewMask( cPriz)
   lMaska := len( Maska)

   if lMaska <> 0
      for j = 1 to 7
         Simvol := subStr( Maska, j, 1)
         if IsdiGit( Simvol)
            Poisk := PosRepl( Poisk, Simvol, j)
         endif
      next
   endif

   if !empty( tmpGet( "CLI_CODE"))
      ACC ->( baseSavePos())
      ACC ->( dbSetOrder( 4))

      balSchet := lTrim( balSchet)
      if len( balSchet) < 5
          errLog( "Вы выбрали неверный балансовый счет !!!")
          break
      endif

      seekStr := balSchet + Poisk
      ACC ->( dbSeek( seekStr, .T.))
      if ACC ->( &( indexKey( 0))) == seekStr
         accSchetAbort()
      endif

      if ACC ->( dbSeek( balSchet, .T.))
         do whil .T.
            clientKey := right( ACC ->schet, 5)
            ACC ->( dbSkip( ))
            if left( ACC ->schet, 5) <> balSchet .OR. ACC ->( eof())
               exit
            endif
         enddo
         clientKey1 := clientKey
         clientKey := padL( nStr( val( right( clientKey, 5)) + 1), 5, "0")

         if lMaska > 2
            if subStr( clientKey, lMaska - 2, 1) <> subStr( clientKey1, lMaska - 2, 1)
                errLog( "Длина маски превышает количество счетов ! " )
                newSchet := ""
                return newSchet
            endif
         endif
      else
         clientKey := "00001"
      endif

      newSchet := balSchet + "810" + "K" + confOtdCode() + "00" + clientKey
      if lMaska > 0
         for j = 1 to 7
            Simvol := subStr( Maska, j, 1)
            if IsdiGit( Simvol)
               newSchet := PosRepl( newSchet, Simvol, 13 + j)
            endif
         next
      endif
      newSchet := getKey( newSchet)                           
                                                              
      ACC ->( baseSavePos())                                  
      ACC ->( dbSetOrder( 1))                                 
      if ACC ->( dbSeek( newSchet)) 
         do whil ACC ->( dbSeek( newSchet)) 
            clientKey := padL( nStr( val( right( clientKey, 5)) + 1), 5, "0")
            newSchet := balSchet + "810" + "K" + confOtdCode() + "00" + clientKey
            if lMaska > 0
               for j = 1 to 7
                  Simvol := subStr( Maska, j, 1)
                  if IsdiGit( Simvol)
                     newSchet := PosRepl( newSchet, Simvol, 13 + j)
                  endif
               next
            endif
            newSchet := getKey( newSchet)                           

            ACC ->( dbSkip( ))
            if left( ACC ->schet, 5) <> balSchet .OR. ACC ->( eof())
               exit
            endif
         enddo
      endif                                                   
   endif

   if ACC ->( dbSeek( newSchet)) 
      errLog( "Ошибка!  Счет: " + newSchet + " уже открыт!")
      newSchet := ""                                       
      return newSchet                                      
   endif

/*   Оставлено для сравнения

      seekStr := balSchet + Poisk
      ACC ->( dbSeek( seekStr, .T.))
      if ACC ->( &( indexKey( 0))) <> seekStr
         ACC ->( dbSkip( -1))
         if left( ACC ->schet, 5) <> balSchet .AND. ACC -> ( !bof()) //.AND. len( allTrim( ACC ->schet)) <> 0
            clientKey := "00001"
         else
            clientKey := padL( nStr( val( right( ACC ->schet, 5)) + 1), 5, "0")
         endif
         if len( allTrim( dopMask)) > 0   //Длина маски > 2
            for j = 1 to len( allTrim( dopMask))
               Simvol := subStr( dopMask, j, 1)
               if IsdiGit( Simvol)
                  if val( subStr( clientKey, j, 1)) <> 0
                     errLog( "Длина маски счета: " + allTrim( subStr( arr1[ 1], 1, 37)) + " превышает количество счетов ! " )
                     newSchet := ""
                     return newSchet
                  else
                     clientKey := PosRepl( clientKey, Simvol, j)
                  endif
               endif
            next
            seekStr := balSchet + clientKey
            ACC ->( dbSeek( seekStr, .T.))
            if ACC ->( &( indexKey( 0))) <> seekStr
               ACC ->( dbSkip( -1))
               if left( ACC ->schet, 5) == balSchet .AND. ACC -> ( !bof()) 
                  clientKey := padL( nStr( val( right( ACC ->schet, 5)) + 1), 5, "0")
               endif
            endif
            seekStr := balSchet + subStr( Poisk, 1, 2) + clientKey
            if ACC ->( dbSeek( seekStr, .T.))
               if left( ACC ->schet, 5) == balSchet .AND. ACC -> ( !eof()) 
                  clientKey := padL( nStr( val( right( ACC ->schet, 5)) + 1), 5, "0")
               endif
            endif
         endif
         newSchet := balSchet + "810" + "K" + confOtdCode() + "00" + clientKey
         if len( allTrim( Maska)) <> 0
            for j = 1 to 7
               Simvol := subStr( Maska, j, 1)
               if IsdiGit( Simvol)
                  newSchet := PosRepl( newSchet, Simvol, 13 + j)
               endif
            next
         endif
         newSchet := getKey( newSchet)
         ACC ->( baseSavePos())
         ACC ->( dbSetOrder( 1))
         if ACC ->( dbSeek( newSchet))
            errLog( "Ошибка! Счет: " + newSchet + " уже открыт!")
            newSchet := ""
            return newSchet
         endif
      else
         accSchetAbort()
      endif
      ACC ->( baseRestPos())
   endif
*/
return newSchet

funcDef getKey with sSchet:C ;
  local rV, sUslSum:="71371371371371371371371",;
        sKod := setUpMFO(), iTmp, iSum := 0

  sSchet := sKod + left( sSchet, 8) + "0" + right( sSchet, 11)
  for iTmp := 1 to 23
     iSum += val( right( nSTR( val( subStr( sSchet, iTmp, 1)) * val( subStr( sUslSum, iTmp, 1))),1))
  endfor
  sSchet := subStr( sSchet, 4, 8) + right( nSTR( iSum * 3), 1) + right( sSchet, 11)

  rV := sSchet
return rV
//
static funcDef tIndex with dbInfo:A ;
   local dbName, dbAlias, dbStruct, ntxInfo, unique, rV, i, path, ext
   basePrep( dbInfo)
   dbName   := dbInfo[ DBI_NAME   ]
   dbAlias  := dbInfo[ DBI_ALIAS  ]
   dbStruct := dbInfo[ DBI_STRUCT ]
   ntxInfo  := dbInfo[ DBI_INDEXES]
   rV       := .F.
   if len( ntxInfo) = 0
      rV := .T.
   else
      messageStartUp( "Создаю индексы временной базы " + dbName)
      if mCheckBase( dbName, dbStruct)
         if mUse( dbName, dbAlias, .T.)
            if mFLock()
               for i=1 to len( ntxInfo)

                  path := getFPath( ntxInfo[ i][ 1])
                  ext  := getFExt(  ntxInfo[ i][ 1])

                  ntxInfo[ i][ 1] := newFileName( path, getTmpName( path), ext)
                  if len( ntxInfo[ i]) < 3
                     unique := NIL
                  else
                     unique := ntxInfo[ i][ NTX_UNIQUE]
                  endIf
                  if .NOT.setMultiTag()
                     if .NOT.mIndex( ntxInfo[ i][ NTX_NAME],    ;
                                     ntxInfo[ i][ NTX_FORMULA], ;
                                     unique)
                        rV := .F.
                        exit
                     else
                        rV := .T.
                     endIf
                  else
                     if .NOT.mIndex( newFExt( dbName, indexExt()), ;
                                     ntxInfo[ i][ NTX_FORMULA],    ;
                                     unique,                       ;
                                     getFName( ntxInfo[ i][ NTX_NAME]))
                        rV := .F.
                        exit
                     else
                        rV := .T.
                     endIf
                  endIf
               endFor
            endIf
            netClose()
         endIf
      endIf
      messageExit()
   endIf
return rV
 
//
// открытие нового счета
//
funcDef accNewSchet with balSchet:C, dDate:DU, cPriz:C ;
        local rV, newSchet, nPct, s, arr := {}, recNo

DEFAULT dDate TO setCurDate()

if ( rV := ACC ->( mAppend()))
   recNo := ACC ->( recNo())
   if confIsNewPlan()
      newSchet := accNew20Schet( balSchet, cPriz)
   else
      balSchet := padL( allTrim( balSchet), 4)
      newSchet := " " + nStr( val( accLastSchet( balSchet)) + 1)
   endif
   if newSchet == ""
      return .F.
   endif

   if empty( newSchet)   //OVD
      s = select()
      arr := tempInfo()
      tempClose()
      tIndex( arr)
      tempOpen()
      select ( s)
      if confIsNewPlan()
         newSchet := accNew20Schet( balSchet, cPriz)
      else
         balSchet := padL( allTrim( balSchet), 4)
         newSchet := " " + nStr( val( accLastSchet( balSchet)) + 1)
      endif
      if empty( newSchet)
         errLog( "Повреждена база данных:  TEMP.DBF !!!" )
         return .F.
      endif
   endif

   ACC ->( dbGoTo( recNo))
   ACC ->lEdited := .T.
   ACC ->schet   := newSchet
   ACC ->datOpen := dDate
   ACC ->typeOf  := "00"
   ACC ->priz    := cPriz

   nPct := vidGetPrc( cPriz)
   accFillPrc( nPct)
   ACC ->( dbSkip( 0))  // !!!!
   tmpUnLock()
endif

return rV
//
funcDef accPrizList ;
   with priz:C, arr:AU, arr2:AU, usePcts:LU, mustCut:LU, useTypeOf:LU ;
   local theCode := CLIENTS ->code, len := len( priz)

DEFAULT mustCut   TO .T.
DEFAULT useTypeOf TO .T.

if mustCut then arr := {}; arr2 := {}
ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

accSeek( theCode)
while theCode == ACC ->code .AND. ACC ->( !eof())
  if left(ACC->PRIZ, len) == priz .AND. if( useTypeOf, ACC ->typeOf <>"02", .T.)
     aAdd( arr, "Счет "     + splitSSchet( ACC ->schet) + ;
                " Остаток " + mStr( sumRest( ACC ->schet, setCurDate() )))
     aAdd( arr2, { ACC ->( recNo()), ACC ->schet})
  endif
  skip 1 alias ACC
enddo
ACC ->( baseRestPos())
return NIL


static funcDef chkAddSchet with par:NC local rV
   if par == 3
      rV := left( ACC ->PRIZ, 6) $ "15 3  ,15 9  ,15 7  " .AND. (.not. _isProsrPct(ACC->Schet)) // TAT 05-02-05 не показывать счета просрочки процентов
   elseif par == 5                                  // Add by TMV 21-05-2003
      rV := left( ACC ->PRIZ, 7) == ACC_CRD_NOSTRAH // Add by TMV 21-05-2003
   else
      if par == INC_SR
         rV := left( ACC ->PRIZ, 6) == "15 2  " .AND. ACC ->typeOf <> "02"
      else
         rV := left( ACC ->PRIZ, 6) $ "15 1  _15 4  _" .AND. ACC ->typeOf <> "02"
      endif
   endif
return rV
//
static funcDef chkAddRSchet with par:NC local rV
   if par == 3
      rV := left( ACC ->PRIZ, 6) $ "15 3  ,15 6  ,15 7  "
   endif
return rV

static funcDef safeDialogOpen with par:N
  if yesNo( "Вы хотите открыть новый счет ?")
     begin sequence
       accDialogOpen( par)
       keyboard chr(27)
     recover
       keyboard chr(27)
     end
  endif
return NIL

//
// Приход на счета различных типов
//
funcDef incSchet with par:N ;
        local theCode := CLIENTS ->code, i, s, ch := "0",;
              arr := { }, arr2 := {}, fk, recNo
arr := brwGetSchet( par)
if len( arr) > 0
   fk := {, {{ "Новый", {|| safeDialogOpen( par)} }} }
   recNo := brwViewSchet( arr, fk)
   if recNo > 0
      ch := if( par == INC_SR, "15 2  ", "15 1  ")
      ACC ->( dbGoTo( recNo))
      if ( par == INC_TEC) .OR. ( par == INC_SR .AND. ;
         ( isCanDopVznos() .OR. ACC ->datOpen == setCurDate()) )
         begin sequence
           accIncom( ch)
         end
      else
         errLog( "Для данного типа вклада довложение запрещено !")
      endif
   endif
   return NIL
else
  safeDialogOpen( par)
  return NIL
endif

if par <> INC_SR .AND. par <> INC_TEC then return NIL
if par == INC_SR
   ch := "15 2  "
else
   ch := "15 1  "
endif


ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

accSeek( theCode)
while theCode == ACC ->code .AND. ACC ->( !eof())

      if chkAddSchet( par)
         aAdd( arr, "Счет "     + splitSSchet( ACC ->schet) + ;
                    " Остаток " + mStr( sumRest( ACC ->schet, setCurDate() )))
         aAdd( arr2, { ACC ->( recNo()), ACC ->schet})
      endif
      skip 1 alias ACC
enddo

ACC ->( baseRestPos())

if len( arr) >0
   aAdd( arr, { "-"})
   aAdd( arr, { "Открыть новый счет ?"})
   aAdd( arr2, { -1, ""})
   aAdd( arr2, { -1, ""})
   i := popUp( 5, 5, arr)
   do case
      case i == len( arr)
        begin sequence
           accDialogOpen( par)
        end
      case i == 0
      otherwise
           ACC ->( dbGoTo( arr2[ i, 1]))
           if ( par == INC_TEC) .OR. ( par == INC_SR .AND. ;
                ( isCanDopVznos() .OR. ACC ->datOpen == setCurDate()) )
              begin sequence
                if ACC ->schet == arr2[ i, 2] then accIncom( ch)
              end
           else
              errLog( "Для данного типа вклада довложение запрещено !")
           endif
   endcase
else
   if yesNo( "Вы хотите открыть новый счет ?")
      begin sequence
        accDialogOpen( par)
      end
   endif
endif

return NIL

funcDef isCanDopVznos
return isNak()

funcDef isSr
return left( ACC ->priz, 6) == "15 2  "

funcDef isSR35
return ACC ->priz == "15 2  1  0  "

funcDef chkCredOpen with arrCode:A, schet:C local rV
  schet := getBalSchet( schet)
  rV    := aScan( arrCode, {|x| x[ 5] == schet}) > 0
return rV

//
// список ссудных счетов : действующие
//
funcDef accACred with arr:AU, arr2:AU ;
  local curSchet, arrName, arrCode


   arr  := {}
   arr2 := {}
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 2))

   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 3))

   arrName := {}; arrCode := {}
   vidArray( "15 3  ", @arrName, @arrCode)

   accSeek( CLIENTS ->code)
   while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())

      if ( credSSchet( ACC ->schet, ACC ->PRIZ) /*.OR. isCardCred()*/).AND. ;
         ACC ->typeOf == "00" .AND. ;
         chkCredOpen( arrCode, ACC ->schet) .AND. ;
         ( SUMS ->( dbSeek( opDecode( "1007") + DtoS( setCurDate()) + ACC ->schet)) .OR.;
           sumRest( ACC ->schet, setCurDate()) <> 0);
                          .And. !useLineNoStrah()  // Add by TMV  25-04-2003

         aAdd( arr, "Счет "     + ACC ->schet)
         aAdd( arr2, { ACC ->( recNo()), ACC ->schet})
      else
         //
         // вернем 716 счет, по которому можно выдать кредит
         // это действующий счет, по которому не было движения сегодня
         //  и остаток нулевой !!!
         //
         if ( credSSchet( ACC ->schet, ACC ->PRIZ) /*.OR. isCardCred()*/) .AND. ;
            ACC ->typeOf == "00" .AND. ;
            chkCredOpen( arrCode, ACC ->schet)                  .AND. ;
            ( SUMS ->( !dbSeek( opDecode( "1007") + DtoS( setCurDate()) + ACC ->schet)) .OR. ;
             sumRest( ACC ->schet, setCurDate()) == 0 ;
                                 .Or. useLineNoStrah() )   // Add by TMV  25-04-2003
             curSchet := ACC ->schet
         endif
      endif
      skip 1 alias ACC
   enddo

   SUMS ->( baseRestPos())
   ACC  ->( baseRestPos())

return curSchet

funcDef credMustPct local rV := .T.,;
                        sPensSchet := getPensSchet( CLIENTS->CODE ),; // Add by TMV  16-12-2002
                        nSumma := 0   // Add by TMV  08-01-2003

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))
accSeek( CLIENTS ->code)

while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())
  if ( credSSchet( ACC ->schet, ACC ->PRIZ) .OR. isCardCred()) ;
               .And. empty( ACC->DatClose )
         if confUseAgent()
            nSumma = sumPType( ACC ->schet, opDecode( "2004")) - ;
              sumSType( sPensSchet, opDecode("6137"))
         else
            nSumma = sumPType( ACC ->schet, opDecode( "2004")) - ;
              sumSType( sPensSchet, opDecode("2004"))
         endif
         if round( nSumma, 2) > 0   // Replaced (<>) to (>) by TMV 08-01-2004
         rV := .F.; exit
    endif
  endif
  skip 1 alias ACC
enddo
ACC  ->( baseRestPos())

return rV

* ---------- получить балансовый счет ----------
funcDef getBalSchet with schet:C local rV
   if confIsNewPlan()
      rV := left( schet, 5)
   else
      rV := left( allTrim( schet), 3)
      if len( allTrim( schet)) < 11
         rV := left( rV, 2)
      endif
      rV := padL( rV, 5)
   endif
return rV

* -----  716( 45502 )    счет ---------------
funcDef bal716
return if( confIsNewPlan(), "45509", "  716")

funcDef bal711
//CARDNEW Проверять надо по типу счета, т.к. 42301 - это еще и карточный счет
return if( confIsNewPlan(), "42301", "  711")
funcDef bal711A
return if( confIsNewPlan(), "40817", "  711")

funcDef balKas
return padL( getBalSchet( confKassa()), 5, "0")

funcDef bal718
return if( confIsNewPlan(), "42301", "  718")
funcDef bal718A
return if( confIsNewPlan(), "40817", "  718")

//
// подготовка и открытие микрокредита
//
funcDef accCredOpen local theCode := CLIENTS ->code,;
                          arr := {}, arr2 := {}    ,;
                          curSchet, cnt,;
                          isCredLStrah := useLineNoStrah() // Add by TMV  25-04-2003

curSchet := accACred( @arr, @arr2)
credExist( @cnt)


do case
   case len( arr) > 0 ;
                .And. !isCredLStrah   // Add by TMV  25-04-2003
     errLog( "У клиента уже есть микрокредиты !")
     errLog( "Клиент :" + cliFullName(), .F.)

   case (len( arr) > 0 .Or. !empty(curSchet) ) .And. ;
                        isCredLStrah .And. empty(accNoStrahCred())
     errLog( "У клиента есть микрокредиты по страховой схеме !")

   case CLIENTS ->credSumma <= 0
     errLog( "Клиенту отказано в выдаче кредита !")
     errLog( "Клиент :" + cliFullName() + "сумма: " + mStr( CLIENTS ->credSumma), .F.)
   case if( rsKasHas(), .F., kasRest( "003") <= 0)
     errLog( "Исчерпан лимит отделения !")
   case !chkPrintOrder()
     messageB( "У клиента есть распечатанный и не списанный ордер надомника !")
   case !credMustPct() ;
                .And. !isCredLStrah  // Add by TMV  25-04-2003
     messageB( "Сначала нужно вернуть проценты !")
   case hasTechCred()
     messageB( "У клиента есть карточный овердрафт !")
   case cnt > 0
     messageB( "У клиента оформлен лимит кредитования !")
   otherwise
     if accChkRests();
                       .And. chkCredInOut(curSchet)  // Add by TMV  10-01-2003

                if isCredLStrah ;
                                .Or. useLineAllNoStrah()  // Add by TMV  11-11-2003
                        if crdCredLineCan(theCode) then credNewNoStrah(curSchet)

                else   &&if if(useLimitDolg(), credLimitDolg(.T.), .T. )
                        credNew( curSchet)
                endif
          endif
endcase
return NIL


funcDef accChkRests local rV := .T., rest, balSchet, str

   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 2))

   accSeek( CLIENTS ->code)
   while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())

      balSchet := getBalSchet( ACC ->schet)
      rest     := sumRest( ACC ->schet, setCurDate() )
      do case
         case balSchet == bal711() .OR. balSchet == bal718() ;
             .OR. balSchet == bal711A() .OR. balSchet == bal718A()
              if rest > 10
                 str := "У клиента на " + if( balSchet == bal711() .OR. balSchet == bal711A()  ,;
                     "текущем", "карточном") + " счете есть остаток " +;
                     mStr( rest)
                 rV := .F.; exit
              endif
         case credSSchet( ACC ->schet, ACC ->PRIZ) .OR. isCardCred()
              if rest > 0
                 str := "У клиента уже взят кредит на сумму " + mStr( rest)
                 rV := .F.; exit
              endif
         otherwise
              if rest > 0
                 str := "У клиента на счете " + splitSSchet( ACC ->schet) + ;
                        " есть остаток " + mStr( rest)
                 rV := .F.; exit
              endif
      endcase
      skip 1 alias ACC
   enddo
   ACC ->( baseRestPos())
   if !rV then errLog( str)
return rV


funcDef accIsCard with schet:C local rV, balSchet := left( ACC ->schet, 5)
   rV := _isCard(schet)
return rV

funcDef safeCloseSchet with ch:C, arr:A, ptr:N
   if cliRLock()
     ACC ->( dbGoTo( arr[ ptr][ 5]))
     if !empty( ACC ->datClose)
        messageB( "Счет уже закрыт !")
     else
        accDec2( ch)
     endif
     CLIENTS ->( netUnLock())
   endif
return NIL

//
// Расход со счетов различных типов
//
funcDef decSchet with par:N ;
        local theCode := CLIENTS ->code, i, s, ch := "0",;
              arr := { }, arr2 := {}, fk, recNo, mode


if par <> INC_SR .AND. par <> INC_TEC then return NIL

if par == INC_SR
   ch := "15 2  "
   mode := 5
else
   ch := "15 1  _15 4  "
endif

arr := brwGetSchet( par)
if len( arr) < 1
   messageB( "У клиента нет действующих " + ;
      if( par == INC_TEC, "текущих", "срочных") + " счетов.")
   return .F.
endif
if par == INC_TEC
   fk := {,,,,,,,;
     {{ "Закрыть", {|| safeCloseSchet( ch, arr, brwGetPtr())}}} }
endif
recNo := brwViewSchet( arr, fk, mode)
if recNo <= 0 then return NIL
ACC ->( dbGoTo( recNo))
if par == INC_TEC
   if accIsCard( ACC ->schet) .AND. hasActiveCards( ACC ->schet)
      messageB( "С карточного счета нельзя снимать наличные средства.")
   else
      if cliRLock()
         accDec( ch)
         CLIENTS ->( netUnLock())
      endif
   endif
else
  if cliRLock()
     if isCanDopVznos() .OR. is45x150() .OR. isKindDeal() .OR. ;
        isNam10()       .OR. isZenit2001()
        dopVznosWork( ch)
     else
        accDec2( ch)
     endif
     CLIENTS ->( netUnLock())
   endif
endif
return NIL

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

accSeek( theCode)
while theCode == ACC ->code .AND. ACC ->( !eof())

      if left( ACC ->PRIZ, 6) $ ch .AND. ACC ->typeOf <> "02"
         aAdd( arr, "Счет "     + splitSSchet( ACC ->schet) + ;
                    " Остаток " + mStr( sumRest( ACC ->schet, setCurDate() )))
         aAdd( arr2, { ACC ->( recNo()), ACC ->schet})
      endif
      skip 1 alias ACC
enddo
ACC ->( baseRestPos())

if len( arr) < 1
   messageB( "У клиента нет действующих " + ;
      if( par == INC_TEC, "текущих", "срочных") + " счетов !!!")
   return .F.
endif

if par == INC_TEC
   ch := "0"
   if par == INC_SR then ch := "1"
   if len( arr) >0
      aAdd( arr, { "-"})
      aAdd( arr, { "Закрыть счет ?"})
      aAdd( arr2, { -1, ""})
      aAdd( arr2, { -1, ""})
      i := popUp( 5, 5, arr)
      do case
         case i == len( arr)
              aSize( arr,  len( arr)  - 2)
              aSize( arr2, len( arr2) - 2)
              i := popUp( 5, 5, arr)
              if i >0
                 ACC ->( dbGoTo( arr2[ i, 1]))
                 if cliRLock()
                    if ACC ->schet == arr2[ i,2] then accDec2( ch)
                    CLIENTS ->( netUnLock())
                 endif
              endif
         case i == 0
         otherwise
              ACC ->( dbGoTo( arr2[ i, 1]))
              if accIsCard( ACC ->schet)  .AND. hasActiveCards( ACC ->schet)
                 messageB( "С карточного счета нельзя получать деньги наличными !!!")
              else
                 if cliRLock()
                    if ACC ->schet == arr2[ i,2] then accDec( ch)
                    CLIENTS ->( netUnLock())
                 endif
              endif

      endcase
   else
      errLog( "У клиента нет таких счетов !")
   endif

else

   if ( i := popUp( 5, 5, arr)) >0
      ACC ->( dbGoTo( arr2[ i, 1]))
      if cliRLock()
         if ACC ->schet == arr2[ i,2]
            if isCanDopVznos() .OR. is45x150() .OR. ;
               isKindDeal() .OR. isZenit2001()
               dopVznosWork( ch)
            else
               accDec2( ch)
            endif
         endif
         CLIENTS ->( netUnLock())
      endif
   endif

endif

return NIL

//
// Снятие процентов или закрытие счета для вклада с доп.взносами
//
funcDef dopVznosWork with ch:C ;
   local i, summa, pctSumPCode, pctSumSCode, Str

//
// подсчет суммы процентов
//
summa := sumPctDop()
//
// если можно выплатить проценты то выкатим диалог с суммой
// если уже нельзя то сразу предлагаем закрыть счет
//
if summa >0
   Str := if( is45x150(), "Вклад 45 x 150", "Накопительный вклад")
   if isNam10() then Str := "Петровский юбилейный"
   if isZenit2001() then str := "Зенит-2001"
   Str := if( isKindDeal(), 'Вклад Доброе дело', Str)
   i := dialog({ Str + ": счет " + ACC ->schet, "Вы хотите", ""},;
        { "Снять проценты - " + lTrim( mStr( summa)), "Закрыть вклад"})
   do case
      case i == 1
           accDec( ch)
      case i == 2
           accDec2( ch)
   endcase
else
   //
   // все проценты уже выплачены - закроем вклад
   //
   accDec2( ch)
endif


return NIL

funcDef sumPctDop local i, summa, pctSumPCode, pctSumSCode
   //
   // подсчет суммы процентов
   //
   summa := 0.00
   pctSumPCode := "1006"
   pctSumSCode := "1003,5201"
   //
   // в SUMP с кодом 1006 находится сумма процентов, ранее начисленных
   // по вкладу минус проценты уже выплаченные от открытия счета до
   // текущего дня
   //
   SUMP ->( baseSavePos())
   SUMP ->( dbSetOrder( 1))
   if SUMP ->( netSeek( ACC ->schet + DtoS( setCurDate()), .T. ))
      while SUMP ->schet  == ACC ->schet .AND. ;
            SUMP ->dDate  == setCurDate()

            if opEqual( SUMP ->typeOf, pctSumPCode)
               summa := SUMP ->summa/ 100
               exit
            endif
            skip 1 alias SUMP
      enddo
   endif
   SUMP ->( baseRestPos())
   //
   // теперь вычтем проценты по вкладу уже выплаченные сегодня
   //
   SUMS ->( baseSavePos())
   SUMS ->( dbSetOrder( 1))
   if SUMS ->( netSeek( ACC ->schet + DtoS( setCurDate()), .T. ))
      while SUMS ->schet  == ACC ->schet .AND. ;
            SUMS ->dDate  == setCurDate()

            if opArrEqual( SUMS ->typeOf, pctSumSCode) .AND. ;
               SUMS ->summa < 0
               summa += SUMS ->summa/ 100
            endif
            skip 1 alias SUMS
      enddo
   endif
   SUMS ->( baseRestPos())

return summa

//
// диалоговое открытие счета
//
funcDef accDialogOpen with par:N ;
        local rV, arr1, arr2, nType := 1, get, summa := 0.00, aOrder  ,;
              arr, ptr, theOrder, tmpArr1, tmpArr2, i, retArr, client2,;
              dogCode, mustSave, persName, sumDesc, desc2, pict, Str  ,;
              canContinue := .T., latfio, naznp, arrCode, arrName, ;
              Soob := "", j, arrS := {}

naznp = "Взнос на вклад до востребования"
if (isCards())
   naznp = "Взнос на карточный счет"
endif
arr := {{ opDecode( "2001"), /*"Вклад на текущий счет" 02-12-2004 TAT*/ naznp},;
        { opDecode( "2002"), rTrim( vidGetName( ACC ->PRIZ)) + ", " + ;
                  nStr( vidGetPrc(  ACC ->PRIZ)) + " % ГОДОВЫХ."}}

if par == INC_SR
   pict := "999999999"
   vidArray( "15 2", @arr1, @arr2)
   ptr := 2
else
   pict := "999999999.99"
   vidArray( { "15 4", "15 1"}, @arr1, @arr2)
   ptr := 1
endif

if len( arr1) == 0
   errLog( { "Отсутствует список видов вкладов! ", "Выполнение операции невозможно " })
   return .F.
endif

get := {{ "Вид вклада", block( nType), arr1 } ,;
        { "*" + chr( 25)                    } ,;
        { ""                                } ,;
        { "Сумма     ", block( summa), pict }}

if getUp( 5, 5, get)

//нужно проверить возможность открытия карточного счета
   if left( arr2[ nType, 4], 4) == "15 4"
      if !wsOrderCnt()
         if (summa <> 0)
            messageB('Проводка не сделана!')
         endif
         return .F.
      endif
   endif

   for j = 1 to len( arr2)
      if left( arr2[ j, 4], 7) == "15 4  2"
         Soob := Rtrim( subStr( arr1[ j], 1, 37))
      endif
   next

   if left( arr2[ nType, 4], 7) == "15 4  1" .AND. !clientIsPens()
      errLog( { "При открытии карточного счета для клиентов ",;
                   "необходимо выбрать: " + Soob})
      return .F.
   endif

   //
   // надо напечать объявление на открытие счета
   //
   if summa == 0 .OR. noYes( { "Вы действительно хотите внести сумму " + mStr( summa)})
      if chkIncSumma( summa, arr2, nType)
      if accNewSchet( " " + arr2[ nType, 5],, arr2[ nType, 4])
         ACC ->code := CLIENTS ->code
         ACC ->priz := arr2[ nType, 4]
         accUnLock()
         arr := {{ opDecode( "2001"), /*"Вклад на текущий счет" 02-12-2004 TAT*/ naznp},;
                 { opDecode( "2002"), rTrim( vidGetName( ACC ->PRIZ)) + ", " + ;
                           nStr( vidGetPrc(  ACC ->PRIZ)) + " % ГОДОВЫХ."}}

         //
         // Исключено по СЗ i0041099a
         //
         if isCards() //left( ACC ->priz, 4) == "15 4"
            wsCrAppend( ACC ->schet, .F., @retArr)
            if !empty( retArr)
                if confUseAgent()
                  latfio := rTrim( retArr[ 3]) + " " + rTrim( retArr[ 2])
                else
                  latfio := rTrim( retArr[ 2]) + " " + rTrim( retArr[ 3])
                endif
                servDummy({{ "@latfio",  latfio              },;
                   { "@schet_type", accSchetType( ACC ->PRIZ)},;
                   { "@crd_type",   retArr[ 1]               },;
                   { "@schet718",   ACC ->schet              },;
                   { "@debet711",   "" /*CONTRACT ->debetAcc*/  }})
            endif
         endif
         SRSumma( summa)
         //
         // делаем проводку и печатем ордер
         //
         if summa > 0
            aOrder := vznos_nal( theOrder := ordIncIn( setCurDate()),;
                setCurDate(), cliFullName(), ACC ->schet, summa     ,;
                cliFullName()/*confOtdName()*/, arr[ ptr, 2], confKassa(), 16)
            if !empty( client2 := accOtherPerson())  .AND. ;
               !empty( dogCode := confOtherPerson())
               if accOtherWisc( client2, dogCode, summa, theOrder, setCurDate())
                  persName := addPrintOtherPerson( client2)
                  aOrder   := vznos_nal( theOrder, setCurDate(), persName,;
                     ACC ->schet, summa, cliFullName(), arr[ ptr, 2]     ,;
                     confKassa(), 16)
               endif
            else
               addPrintOtherPerson()
            endif
            sumDesc := getSumDesc( ACC ->schet, @desc2)
            desc2   := addDesc2( desc2, "наличными")
            if len( sumDesc) > 1
               Str := "  в сумме : " + getS( summa)
               if len( str) > 75
                  str := razbivka( str, {{ 75, "L"}, { 75, "L"}})
                  aAdd( sumDesc, str[ 1])
                  aAdd( sumDesc, str[ 2])
               else
                  aAdd( sumDesc, Str)
               endif
            endif
            aAdd( sumDesc, "наличными квитанция : N " + nStr( theOrder) + ;
                           " от " + DtoC( setCurDate()) )
            aAdd( servDummy(), { "@sumDesc", sumDesc })
            aAdd( servDummy(), { "@desc2",   desc2   })
            mustSave := mustSaveOpen( .T.)
            schetPrintOpen( !isSr() )
            if accIsNevsk( ACC ->PRIZ) .AND. confUseDogovor() ;
               then canContinue := yesNo( "Клиент подписал договор ?")
            if CanContinue
               if isSr() .AND. !accIsSigned() then accSetSigned()
               mustSaveOpen( mustSave)
               servDummy( {})

               printAOrder( aOrder)
               SaveLastOrder( { getSavedOpen()})
               SaveLastOrder( aOrder, .F.)
               aOrder := { getHistory( ACC ->schet, setCurDate(), summa)}
               printAOrder( aOrder, "&2")
               saveLastOrder( aOrder, .F.)

               if conf12UseAgent()
                  // Печать карточки с образцами подписей
                  aadd( arrS, { "", "", "", "", ACC->( recNo( ))})
                  SKSPrintOpen( arrS, 1)
                  saveLastOrder( aOrder, .F.)
               endif

               printAdvert()
               if !sumWrite( ACC ->schet, summa, setCurDate(), arr[ ptr, 1], theOrder)
                  errLog( { "Проводка не сделана!", "", "Попробуйте еще раз. " })
               endif
               //
               // если клиент открыл накопительный(пенсионный накопительный)
               // вклад и у него есть действующий ТПС, то ему предлагается
               // оформить поручение на перечисление средств на этот счет
               //
               nakMakeTrans()
            endif
         else
              if !isSR()
                 mustSave := mustSaveOpen( .T.)
                 schetPrintOpen( .T.)
                 mustSaveOpen( mustSave)
                 servDummy( {})
                 printAdvert()
              endif
         endif
      endif
      else
      errLog( { "На этот тип счета вносятся суммы из диапазона",;
               "от " + mStr( arr2[ nType, 2]/ 100) + " до " + mStr( arr2[ nType, 3]/ 100) ,;
               "", "а вы вносите " + mStr( summa) })
      endif  // сумма не в диапазоне
   else
      errLog( "Проводка не сделана")
   endif
endif

return rV

funcDef nakMakeTrans local rV := .T., isFound
  if isCanDopVznos()
     isFound := .F.
     ACC ->( baseSavePos())
     ACC ->( dbSetOrder( 2))
     if ACC ->( dbSeek( CLIENTS ->code))
        while CLIENTS ->code == ACC ->code .AND. ACC ->( !Eof())
          if isPens( ACC ->PRIZ) .AND. ACC ->typeOf == "00"
            isFound := .T.;  exit
          endif
          skip 1 alias ACC
        enddo
     endif
     ACC ->( baseRestPos())
     if !isFound then return .T.
     rV := .F.
     if yesNo( { "Вы хотите перечислять деньги со своего пенсионного счета ",;
                 "на накопительный вклад ?"})
        //
        // создаем новое поручение без возможности refresh
        // так как нет активного browse-объекта
        //
        rV := contEdit( .T., "07", .F.)
     endif
  endif
return rV

static funcDef chkIncSumma ;
   with summa:N, arr2:A, nType:N ;
   local rV := .F., isFound


if summa >= ( arr2[ nType, 2]/ 100) .AND. summa <= ( arr2[ nType, 3]/ 100)
   rV := .T.
else
   if summa < 0
      messageB( "Вы вносите отрицательную сумму ??? Мда...")
   elseif summa == 0
       rV := noYes( "Вы внесете сумму безналичным перечислением ?")
       if left( arr2[ nType, 4], 4) == "15 2"
          isFound := .F.
          ACC ->( baseSavePos())
          ACC ->( dbSetOrder( 2))
          if ACC ->( dbSeek( CLIENTS ->code))
            while ACC ->code == CLIENTS ->code .AND. ACC ->( !Eof())
              if isPens( ACC ->PRIZ)
                 if sumSType( ACC ->schet, "9002") < 0
                    isFound := .T.; exit
                 endif
              endif
              skip 1 alias ACC
            enddo
          endif
          ACC ->( baseRestPos())
          if isFound
             errOK( "Нельзя открыть счет, так как у клиента есть ордер надомника без даты !")
             rV := .F.
          endif
       endif
   endif
endif
return rV

funcDef accSchetType with priz:C local rV
   do case
      case priz == "15 4  1  0  " .AND. !confUseAgent()
           rV := "счет пенсионера"
      case priz == "15 4  1  0  " .AND. confUseAgent()
           rV := "пенсионный счет"
      case priz == "15 4  2  0  "
           rV := "счет клиента"
      case priz == "15 4  4  1  "
           rV := "счет клиента"
      case priz == "15 4  3  0  "
           rV := "счет сотрудника"
      otherwise
           rV := ""
   endcase
return rV

//
// Собственно приход на счет
//
funcDef accIncom with par:C ;
   local rV, nType := 1, get, nazn, summa := 0.00, aOrder, arr, ptr,;
         theOrder, oldSave, client2, persName, dogCode, sumDesc    ,;
         desc2, pict, str, naznp

if !msgShowMessage() then return .F.

/* добавлено 02-12-2004 TAT*/
naznp = "Взнос на вклад до востребования"
if (isCards())
   naznp = "Взнос на карточный счет"
endif
arr := {{ opDecode( "2001"), /*"Вклад на текущий счет" 02-12-2004 TAT*/ naznp},;
        { opDecode( "2002"), rTrim( vidGetName( ACC ->PRIZ)) + ", " + ;
                  nStr( vidGetPrc(  ACC ->PRIZ)) + " % ГОДОВЫХ."}}
pict := "99999999"
ptr  := 1
if par == "15 2  "
   ptr := 2
   if sumRest( ACC ->schet, setCurDate()) > 0
    arr[ 2, 2] := "Дополнительный взнос на " + arr[ 2, 2]
   end if
else
   pict += ".99"
endif
nazn := arr[ ptr, 2]

if isCards() then arr[ 1, 2] := /*"Вклад на текущий карточный счет" 02-12-2004 TAT*/ "Взнос на карточный счет"

get  := {{ "Счет      " + splitSSchet( ACC ->schet)  },;
         { "" },;
         { "Сумма     ", block( summa),pict,,{|g|chkAccIncom( g) .AND. g:varGet > 0}  }}

if getUp( 5, 5, get) .AND. chkAccIncom( summa, ACC ->schet)
   //
   // делаем проводку и печатем ордер
   //
   if noYes( { "Вы действительно хотите внести сумму " + mStr( summa) + " ?"})
      theOrder := ordIncIn( setCurDate())
      aOrder   := vznos_nal( theOrder, setCurDate(), cliFullName() ,;
                             ACC ->schet, summa, cliFullName()/*confOtdName()*/     ,;
                             arr[ ptr, 2], confKassa(), 16)
      if ptr == 2
         if ACC ->datOpen == setCurDate() then accClrSigned()
         SRSumma( summa + sumRest( ACC ->schet, setCurDate()))
      else
         SRSumma( summa)
      endif
      dogCode := accOtherGet( ACC ->schet)
      if !empty( client2 := accOtherPerson()) // .AND. ;
         if accOtherWisc( client2, dogCode, summa, theOrder, setCurDate())
            persName := addPrintOtherPerson( client2)
            aOrder   := vznos_nal( theOrder, setCurDate(), persName,;
               ACC ->schet, summa, cliFullName(), arr[ ptr, 2], confKassa(), 16)
            aAdd( servDummy(), { "@docNumb", dogCode})
         endif
      else
         addPrintOtherPerson()
      endif
      oldSave := mustSaveOpen( .T.)
      if .T.
         //
         // для текущих вкладов надо спрашивать подписано ли заявление
         // для срочных вкладов - убрать такой запрос : 12.03.99
         //
         sumDesc := getSumDesc( ACC ->schet, @desc2)
         desc2   := addDesc2( desc2, "наличными")
         if len( sumDesc) > 1
            Str := "  в сумме : " + getS( summa)
            if len( str) > 75
               str := razbivka( str, {{ 75, "L"}, { 75, "L"}})
               aAdd( sumDesc, str[ 1])
               aAdd( sumDesc, str[ 2])
            else
               aAdd( sumDesc, Str)
            endif
         endif
         aAdd( sumDesc, "наличными квитанция : N " + nStr( theOrder) + ;
                        " от " + DtoC( setCurDate()) )
         aAdd( servDummy(), { "@sumDesc", sumDesc })
         aAdd( servDummy(), { "@desc2"  , desc2   })
         schetPrintOpen( !isSr() )
         if isSr() .AND. !accIsSigned() then accSetSigned()
         saveLastOrder( getSavedOpen())
      endif
      mustSaveOpen( oldSave); servDummy( {})
      SRSumma( 0); printAOrder( aOrder)
      saveLastOrder( aOrder, .F.)
      printAdvert()
// Add by TMV 29-07-2004
                str = accCrdPrnOld(ACC ->schet)
                if !empty( str )
                        aOrder = pCrdOldPrint(ACC ->schet, str)
                        printAOrder( aOrder)
                        accCrdPrnOld(ACC ->schet, .T.)
                endif
// Add by TMV 29-07-2004 -- End
      sumWrite( ACC ->schet, summa, setCurDate(), arr[ ptr, 1], theOrder)
      printHistory( ACC ->schet, setCurDate())
   else
      errLog( { "Проводка не сделана!", "" ,;
               "Попробуйте еще раз. " })
   endif
endif
  
return rV

static funcDef chkAccIncom with g:ON, curSchet:CU ;
   local rV := .T., addMin, addMax, Summa, ar1, ar2

summa := if( valType( g) == "O", g:varGet, g)
if summa < 0
   messageB( "Введена недопустимая сумма!")
   return .F.
endif

if !empty( curSchet)
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))
   ACC ->( dbSeek( curSchet))
   if ACC ->datOpen == setCurDate() .AND. sumRest( ACC ->schet, setCurDate()) == 0
      // это 1 взнос на вклад
      vidArray( ACC ->PRIZ, @ar1, @ar2, ,.F.) 

      if !( rV := chkIncSumma( summa, ar2, 1))
        errLog( { "На этот тип счета вносятся суммы из диапазона"       ,;
                  "от " + mStr( ar2[ 1, 2]/ 100) + " до " + mStr( ar2[ 1, 3]/ 100) ,;
                  "", "а вы вносите " + mStr( summa) })
      endif
   else
     vidGetAdd( ACC ->PRIZ, @addMin, @addMax)
     if summa < addMin/ 100 .OR. summa > addMax/ 100
        errLog( { "На этот тип счета вносятся суммы из диапазона"       ,;
                  "от " + mStr( addMin/ 100) + " до " + mStr( addMax/ 100) ,;
                  "", "а вы вносите " + mStr( summa) })
        rV := .F.
     endif
   endif
   if isSR()
      if !( isCanDopVznos() .OR. ACC ->datOpen == setCurDate())
        errLog( "По этому типу вклада довложение запрещено !")
        rV := .F.
      endif
   endif
   ACC ->( baseRestPos())
endif


return rV

//
// Собственно снятие со счета
//
funcDef accDec with par:C ;
   local nType := 1, get, nazn, summa := 0.00, aOrder, arr, ptr, rV := .T.,;
         trustPtr := -1, arrTrust, arr2, ro, sim, theOrder, trustCurDocum ,;
         trustOrderInfo, sumPctDop := 0, bufSums, mode, naznp

if !msgShowMessage() then return .F.


/* добавлено 02-12-2004 TAT*/
naznp = "Выплата со вклада до востребования"
if (isCards())
        naznp = "Выплата с карточного счета"
endif
arr := {{ opDecode( "1001"), /*"Выплата с текущего счета " 02-12-2004 TAT*/ naznp},;
        { opDecode( "1003"), "Выплата со срочного счета"}}

allowTrust( .F.)
arrTrust := trustWorkList()
if len( arrTrust) >0
   arr2 := { "Владелец вклада " + cliFullName() }
   for trustPtr := 1 to len( arrTrust) do aAdd( arr2, arrTrust[ trustPtr][ 2, 5])
   trustPtr := popUp( 8, 2, arr2)
endif
if trustPtr > 1
   allowTrust( .T.)
   TRUST ->( dbGoTo( arrTrust[ trustPtr - 1, 1]))
   trustCurDocum := trustCurDocum()
endif


ptr  := 1
if par == "15 2  "
   ptr  := 2
   mode := 5
endif
nazn := arr[ ptr, 2]

get  := {{ "Счет      " + splitSSchet( ACC ->schet)} ,;
         { "Остаток   " + mStr( sumRest( ACC ->schet, setCurDate())) },;
         { "" } ,;
         { "Сумма     ", block( summa),,,{|g| chckSumma( g)} }}

if getUp( 5, 5, get)
   //
   // делаем проводку и печатем ордер
   //
   if noYes( { "Вы действительно хотите снять сумму " + mStr( summa) + " ?"})
      if summa <= sumRest( ACC ->schet, setCurDate())

         if summa <= kasRest()
            operGetInfo( arr[ ptr, 1], @ro, @sim)
            theOrder := ordIncOut( setCurDate())
            if AllowTrust()
               trustOrderInfo := trustOrderInfo( trustCurDocum)
               aOrder := homeOneOrder( cliFullName(), cliFullAddress(),;
                   trustOrderInfo, theOrder, nazn, ro, sim, summa, .T.,;
                   .T., setCurDate(),, trustFIO(), trustAddr())
            else
               aOrder := homeOneOrder( cliFullName(), cliFullAddress(),;
                  cliDocum(), theOrder, nazn, ro, sim, summa, .T., .T.,;
                  setCurDate())
            endif
            SRSumma( summa)
            schetPrintOpen( .T.)
            SRSumma( 0)
            printAOrder( aOrder)
            saveLastOrder( aOrder)
            //if allowTrust() then printAOrder( trustGiveInfo( trustCurDocum))
            printAdvert()
// Add by TMV 29-07-2004
                                sim = accCrdPrnOld(ACC ->schet)
                                if !empty( sim )
                                        aOrder = pCrdOldPrint(ACC ->schet, sim)
                    printAOrder( aOrder)
                                        accCrdPrnOld(ACC ->schet, .T.)
                                endif
// Add by TMV 29-07-2004 -- End
            bufSums := trustGetNumber()
            if isNam10() then sumPctDop := sumPctDop()
            if isNam10() .AND. sumPctDop <> 0
              bufSums := bufSums + ";" + nStr( summa)
            endif
            if !sumWrite( ACC ->schet, - summa, setCurDate(), arr[ ptr, 1],;
               theOrder,, bufSums)
               errLog( { "Проводка не выполнена.", "" , "Попробуйте еще раз. " })
               rV := .F.
            else
               //
               // если это петровский юбилейный, то надо сделать проводку 9100
               //
               if isNam10() .AND. sumPctDop <> 0
                 sumWrite( ACC ->schet, summa, setCurDate(), "9100", 0)
               endif
               printHistory( ACC ->schet, setCurDate())
               if allowTrust() .AND. trustRLock()
                  trustLastSum( setCurDate(), summa)
                  trustUnLock()
               endif
            endif
         else
            errLog( { "Эта проводка вызовет появление красного сальдо по кассе !",;
                     "", "Подкрепите кассу !" })
            rV := .F.
         endif
      else
         errLog( "Эта проводка вызовет появление красного сальдо по счету !")
      endif
   endif
endif

allowTrust( .F.)

return rV


funcDef saveLastOrder with aOrder:AC, eraseBefore:LU ;
   local fileName := userName() + ".ord"

   DEFAULT eraseBefore TO .T.
   fileName := newFPath( fileName, confPathUser())
   if file( fileName) .AND. eraseBefore then fErase( fileName)
   eFileWrite( aOrder, fileName)

return NIL

//
// Закрытие счета
//
funcDef accDec2 with par:C ;
  local nType := 1, get, nazn, summa := 0.00, aOrder, arr, ptr, rV := .T.,;
        arrTrust, trustPtr := -1, arr2, thePctCode, theSumma, pctSum, ro ,;
        sim, old := allowTrust(), theOrder, mustSave, trustCurDocum      ,;
        operID, trustOrderInfo
if !msgShowMessage() then return .F.
if !contSchetCheck( ACC ->schet) then return .F.
if isCards() .AND. !chkCredLine(ACC->schet) then return .F.
if !checkCardAccForClose( ACC ->schet) then return .F.

 DogisPrint(ACC ->schet)

arr := {{ opDecode( "1051"), "Закрытие текущего счета"},;
        { opDecode( "1052"), "Закрытие срочного счета"}}

arr2:= {{ opDecode( "4001"), "Начислить проценты по ТПС"},;
        { opDecode( "4002"), "Начислить проценты по срочному вкладу"}}

ptr  := 1
if par == "15 2  " then ptr := 2

nazn := arr[ ptr, 2]

thePctCode := arr2[ ptr, 1]


if ptr == 2
   allowTrust( .F.)
   arrTrust := trustWorkList()
   if len( arrTrust) >0
      arr2 := { "Владелец вклада " + cliFullName() }
      for trustPtr := 1 to len( arrTrust) do aAdd( arr2, arrTrust[ trustPtr][ 2, 5])
      trustPtr := popUp( 8, 2, arr2)
   endif
   if trustPtr > 1
      allowTrust( .T.)
      TRUST ->( dbGoTo( arrTrust[ trustPtr - 1, 1]))
      trustCurDocum := trustCurDocum()
   endif

endif


//
// делаем проводку и печатем ордер
//
summa  := sumRest( ACC ->schet, setCurDate())

pctSum := 0
if SUMP ->( netSeek( ACC ->schet + DtoS( setCurDate()), .T. ))
   while SUMP ->schet  == ACC ->schet .AND. ;
         SUMP ->dDate  == setCurDate()

         if SUMP ->typeOf == thePctCode
            pctSum := SUMP ->summa/ 100
            exit
         endif
         skip 1 alias SUMP
   enddo
endif

if allowTrust()
   chckTrustDate( setCurDate(), @theSumma)
   if theSumma < ( summa + pctSum) .AND. TRUST ->type_1 == 2
      messageB( "Вы не можете снять такую сумму по этой доверенности.")
      rV := .F.
   else
      if isSR() .AND. TRUST ->type_1 == 4
         messageB( "По этой доверенности можно только снимать проценты!")
         rV := .F.
      endif
   endif
endif

if rV
   theOrder := ordIncOut( setCurDate())
   if noYes( { "Вы действительно хотите закрыть счет " + ;
               splitSSchet( ACC ->schet)                ,;
               "сумма остатка   " + mStr( summa)        ,;
               "сумма процентов " + mStr( pctSum)}) .AND.;
      chkCards()

      if ( summa + pctSum) <= kasRest()
         if summa <= sumRest( ACC ->schet, setCurDate())
            if ( summa  + pctSum) >0

               operGetInfo( arr[ ptr, 1], @ro, @sim)
               if AllowTrust()
                  // aOrder := homeOneOrder( trustFIO(), trustAddr()          ,;
                  //    trustCurDocum, theOrder, nazn, ro, sim, summa + pctSum,;
                  //    .T., .T., setCurDate())
                  //
                  // 27.04.2001
                  //
                  // trustOrderInfo := if( isSR() .AND. !confUseAgent(), trustCurDocum, trustOrderInfo( trustCurDocum))
                  trustOrderInfo := trustOrderInfo( trustCurDocum)
                  aOrder := homeOneOrder( cliFullName(), cliFullAddress()   ,;
                     trustOrderInfo, theOrder, nazn, ro, sim, summa + pctSum,;
                     .T., .T., setCurDate(),, trustFIO(), trustAddr())
               else
                  aOrder := homeOneOrder( cliFullName(), cliFullAddress()  ,;
                     cliDocum(), theOrder, nazn, ro, sim, summa + pctSum   ,;
                     .T., .T., setCurDate())
               endif
               SRSumma( summa)
               mustSave := mustSaveOpen( .T.)
               if isCards()
                  servDummy( {{ "@schet718", ACC ->schet }})
               endif
               schetPrintOpen( .T.)
               servDummy( {})
               saveLastOrder( { getSavedOpen()})
               mustSaveOpen( mustSave)
               SRSumma( 0)
               printAOrder( aOrder)
               saveLastOrder( aOrder, .F.)

               printAdvert()
               operID := if( pctSum > 0, nStr( pctSum), "")
               if if( summa + pctSum>0,;
                  sumWrite( ACC ->schet, - summa - pctSum,  setCurDate(),;
                     arr[ ptr, 1], theOrder,, trustGetNumber()+";" +operID),.T.)
                  if allowTrust() .AND. trustRLock()
                     trustLastSum( setCurDate(), summa + pctSum)
                     trustUnLock()
                  endif
                  if pctSum <> 0
                     sumWrite( ACC ->schet, pctSum, setCurDate(), "9100", 0)
                  endif
                  //
                  // запись блокирована так что все OK !
                  //
                  if isPens( ACC ->PRIZ)
                     messageB( "Пенсионный счет нельзя закрыть !!!")
                  else
                     if accRLock()
                        ACC ->datClose := setCurDate()
                        ACC ->typeOf   := "02"
                        accUnLock()
                        schetClosePrint( ACC ->schet)
                     endif
                  endif
                  // удаляем поручения
                  crCloseCards()
               endif
               aOrder := {getHistory( ACC ->schet, setCurDate())}
               printAOrder( aOrder, "&2")
               saveLastOrder( aOrder, .F.)
            else
              if ( summa + pctSum ) = 0
                 aOrder := {getHistory( ACC ->schet, setCurDate())}
                 printAOrder( aOrder, "&2")
                 saveLastOrder( aOrder, .F.)
                 printAdvert()
                 if isPens( ACC ->PRIZ)
                    messageB( "Пенсионный счет нельзя закрыть !!!")
                 else
                   if crCloseCards()
                    if accRLock()
                       ACC ->datClose := setCurDate()
                       ACC ->typeOf   := "02"
                       accUnLock()
                       schetClosePrint( ACC ->schet)
                       messageB( "Счет " + ACC ->schet + " закрыт !!!")
                     endif
                    endif
                 endif
                 crCloseCards()
               endif
            endif
         else
           errLog( "Эта проводка вызовет появление красного сальдо по счету !")
         endif
      else
         errLog( { "Выдача такой суммы вызовет появление красного сальдо по кассе",;
                  "", "Подкрепите кассу !"})
      endif
   else
      errLog( { "Проводка не сделана !!!", "" , "Попробуйте еще раз. " })
      rV := .F.
   endif
endif
allowTrust( old)

return rV

funcDef isCards local rV
   rV := left( ACC ->priz, 6) == "15 4  "
return rV


static funcDef chkCards local rV := .T., arr, i
if isCards()

   //
   // проверим заказ карт в WISC
   //
   if WISC ->( dbSeek( ACC ->schet + "979 "))
     messageB( "У клиента есть поручения на изготовление карт !!!")
     return .F.
   endif

   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 4))
   if CONTRACT ->( dbSeek( ACC ->schet))
      while CONTRACT ->creditAcc == ACC ->schet .AND. CONTRACT ->( !Eof())
        if CONTRACT ->code2 == "09" .AND. CONTRACT ->code3 == "71"
           arr := CtoA( CONTRACT ->mBuffer, ";")
           for i := 1 to len( arr)
               arr[ i] := CtoA( arr[ i], ",")
               arr[ i, 3] := aTrimmedScan( aCrStat2(), arr[ i, 3])
               if !( crGetStatus( arr[ i]) == 2 .OR. crGetStatus( arr[ i]) == 3)
                  rV := .F.
                  messageB( "У клиента есть действующие, принятые к изготовлению карты !!!")
                  // CONTRACT ->( baseRestPos())
                  exit
               endif
           next/*
           if CONTRACT ->lEdited .AND. rV
              errLog( { "Сегодня поручение на перечисление средств изменяли !!!",;
                       "Пожалуйста, подождите до завтра !!!" })
              rV := .F.
           endif*/
        endif
        exit if !rV
        skip 1 alias CONTRACT
      enddo
   endif
   CONTRACT ->( baseRestPos())
endif
if isCanDopVznos() .or. isNak()
   CONTRACT ->( baseSavePos())
   CONTRACT ->( dbSetOrder( 4))
   if CONTRACT ->( dbSeek( ACC ->schet))
      while CONTRACT ->creditAcc == ACC ->schet .AND. CONTRACT ->( !Eof())
        if CONTRACT ->status <= 1
           messageB( "По счету " + ACC ->schet + " есть действующие поручения...")
           rV := .F.
           exit
        endif
        skip 1 alias CONTRACT
      enddo
   endif
   CONTRACT ->( baseRestPos())
endif
return rV

static funcDef crCloseCards local rV := .T.
if isCards() then return .T.
messageStartUp( "Закрытие поручений по счету " + ACC ->schet)
CONTRACT ->( baseSavePos())
CONTRACT ->( dbSetOrder( 4))
if CONTRACT ->( dbSeek( ACC ->schet))
   while CONTRACT ->creditAcc == ACC ->schet .AND. CONTRACT ->( !eof())
     if CONTRACT ->status == 1 .OR. CONTRACT ->status == 3
        if CONTRACT ->( mRLock())
           if crClose()
              CONTRACT ->status  := 2
              CONTRACT ->lEdited := .T.
           else
              rV := .F.
           endif
           CONTRACT ->( netUnLock())
        else
           rV := .F.
        endif
     endif
     skip 1 alias CONTRACT
   enddo
endif
CONTRACT ->( baseRestPos())
messageExit( 0.5)
return rV

static funcDef chckSumma with g:O local rV, theSumma := 0.00

if g:varGet < 0
   messageB( "Вводим отрицательные суммы ??? Ладно ...")
   return .F.
endif
if !( rV := g:varGet() <= sumRest( ACC ->schet, setCurDate()))
   errLog( "Эта проводка вызовет появление красного сальдо по счету !")
else
   //
   // можно ли снять такую сумму ?
   //
   if allowTrust()
      chckTrustDate( setCurDate(), @theSumma)
      if TRUST ->type_1 == 2
         if g:varGet() > theSumma
            errLog( "По доверенности N "     + TRUST ->number + ;
                   " можно снять не более " + mStr( theSumma))
            rV := .F.
         endif
      endif
   endif
endif
//
// вклад с допвзносами
//
if rV .AND. isCanDopVznos()
   theSumma := sumPctDop()
   if round( g:varGet(), 2) > round( theSumma, 2)
      rV := .F.
      errLog( { "Можно снять процентов не более - " + mStr( theSumma),;
               "а Вы пытаетесь снять           - " + mStr( g:varGet()) })
   endif
endif
if rV .AND. ( is45x150() .OR. isKindDeal() .OR. isNam10() .OR. isZenit2001() )
   theSumma := sumPctDop()
   if round( g:varGet(), 2) <> round( theSumma, 2)
      rV := .F.
      errLog( { "Можно снять процентов - " + mStr( theSumma),;
               "а Вы пытаетесь снять  - " + mStr( g:varGet()) })
   endif
endif
return rV

//
// Проверка на то, было ли распечатано заявление на открытие счета
// и если было, то печать и установка признака печати
//
autofunction setValue printAsk init .F.

funcDef schetPrintOpen with canAsk:LU

DEFAULT canAsk TO .T.
if SubStr( ACC ->buffer, 5, 1) <> "Y"
   usePctsBuffer( .T.)
   printAsk( .T.)
   if canAsk
      if accIsPrinted()
         if noYes( "Заявление подписано ?")
            accSetSigned()
         else
            printAndAsk()
         endif
      else
         printAndAsk()
      endif
   else
      if printOpen() then accSetPrinted()
   endif
   usePctsBuffer( .F.)
   printAsk( .F.)
endif

accS20Check( canAsk)

return NIL

funcDef PrintAndAsk with par2:CU
   if if( pCount() = 2, printOpen( par2), printOpen()) .AND. ;
      noYes( "Заявление подписано ?")
      accSetSigned()
   endif
return NIL

funcDef accIsPrinted local rV
    rV := SubStr( ACC ->buffer, 5, 1) == "P"
return rV

funcDef accIsSigned local rV
    rV := SubStr( ACC ->buffer, 5, 1) == "Y"
return rV

funcDef accIsClosPrinted local rV
    rV := SubStr( ACC ->buffer,10, 1) == "Y"
return rV

funcDef accSetClosPrinted
   if accRLock()
      ACC ->buffer := left(  ACC ->buffer, 9) + "Y" + subStr( ACC ->buffer, 11)
      accUnLock()
   endif
return NIL


funcDef accSetPrinted
   if accRLock()
      ACC ->buffer := left(  ACC ->buffer, 4) + "P" + subStr( ACC ->buffer, 6)
      accUnLock()
   endif
return NIL

funcDef accSetSigned
   if accRLock()
      ACC ->buffer := left(  ACC ->buffer, 4) + "Y" + subStr( ACC ->buffer, 6)
      accUnLock()
   endif
return NIL

funcDef accClrSigned local isLocked
   isLocked := aScan( ACC ->( dbRLockList()), {|x| ACC ->( recNo()) == x}) > 0
   if if( isLocked, .T., accRLock())
      ACC ->buffer := left(  ACC ->buffer, 4) + " " + subStr( ACC ->buffer, 6)
      if isLocked
         accUnLock()
      else
         ACC ->lEdited := .T.
      endif
   endif
return NIL
funcDef accS20IsPrinted local rV
    rV := SubStr( ACC ->buffer, 7, 1) == "P"
return rV

funcDef accS20SetPrinted
   if accRLock()
      ACC ->buffer := left(  ACC ->buffer, 6) + "P" + subStr( ACC ->buffer, 8)
      accUnLock()
   endif
return NIL

funcDef accS20SetSigned
   if accRLock()
      ACC ->buffer := left(  ACC ->buffer, 6) + "Y" + subStr( ACC ->buffer, 8)
      accUnLock()
   endif
return NIL

funcDef accS20Print local str
   str := WISC ->buffer
   str := str + endl() + replicate( "-", 70) + endl() + ;
          str + endl()
return printAOrder( { str})

funcDef accS20Check with canAsk:LU

DEFAULT canAsk TO .T.

if subStr( ACC ->buffer, 7, 1) <> "Y" .AND. wiscSeek( ACC ->schet, "2000")
   if canAsk
      if accS20IsPrinted()
         if yesNo( "Уведомление об изменении номера счета вручено ?")
            accS20SetSigned()
         else
            accS20Print()
         endif
      else
         accS20Print()
         accS20SetPrinted()
      endif
   else
      accS20Print()
      accS20SetPrinted()
   endif
endif
return NIL

autofunction setValue usePctsBuffer init .F.
//
// заполнение признака печати заявления при открытии счета
// запись должна быть блокирована
//
funcDef accFillPrc with thePct:N

ACC ->buffer := left( ACC ->buffer, 4) + "N" + right( ACC ->buffer, 5)
ACC ->begPct := padL( thePct, 5, "0")

return NIL

* ------------ безналичные перечисления со счета на счет
funcDef accTrans ;
   local arrDebet, arrDebet2, arrCredit, arrCredit2, ptr, ptrDebet,;
         ptrCredit, arr, arr2, i, msgS1, msgS2, msgS3, msgS, gets ,;
         summa, debSchet, credSchet, pict, isFound, bankDest      ,;
         theDest, curDest, aOrder, curSource, dest1, dest2, dest3 ,;
         docNumb, dest4, dest5, d2, aWisc, memDoc, typeOf, lastZ  ,;
         sumDesc, desc2, oldClient, newClient, pctSum, arrDest,;
                        sCredDest, theOrder, sBikDest, sKorSchet,;   //  Add By TMV  25-09-2001
                        KorSchet, sBankDest, BikDest, sInnDest,;   //  Add By TMV  25-09-2001
                        sNote:=''  //  Add By TMV  25-09-2001

  ptr := popUp( 5, 5, (arrDest := { "Взнос на срочный вклад "     ,;
                        "Возврат срочного вклада"                 ,;
                        "Снятие процентов с накопительного вклада",;
                        "Перечисление на текущий(карточный) счет ",;
                        "Безналичное закрытие текущего(карточного) счета"})) &&,;
  if ptr < 1 then return .F.

  pict   := "99999999"
  if ptr <> 1 then pict += ".99"
  lastZ  := .T.
  typeOf := "5010"
  msgS1  := "У клиента нет текущих счетов."
  msgS2  := "У клиента нет срочных счетов."
  msgS3  := "У клиента нет накопительных счетов."

//  Removed by TMV 04-10-2001
  do case
     case ptr == 1
          accPrizList( "15 1", @arrDebet,  @arrDebet2)
          if len( arrDebet) < 1 then msgS := msgS1
          accPrizList( "15 2", @arrCredit, @arrCredit2)
     case ptr == 2
          accPrizList( "15 2", @arrDebet,  @arrDebet2, .T.)
          if len( arrDebet) < 1 then msgS := msgS2
          accPrizList( "15 1", @arrCredit, @arrCredit2)
          addCards( @arrCredit, @arrCredit2)
          typeOf := "5200"
     case ptr == 3
          accPrizList( "15 2  5  0  ", @arrDebet,  @arrDebet2)
          accPrizList( "15 2  29 0  ", @arrDebet,  @arrDebet2,, .F.)
          accPrizList( "15 2  42 0  ", @arrDebet,  @arrDebet2,, .F.)
          accPrizList( "15 2  49 0  ", @arrDebet,  @arrDebet2,, .F.)
          accPrizList( "15 2  50 0  ", @arrDebet,  @arrDebet2,, .F.)
          accPrizList( "15 2  53 0  ", @arrDebet,  @arrDebet2,, .F.)

          accPrizList( "15 2  39 0  ", @arrDebet,  @arrDebet2,, .F.)
          accPrizList( "15 2  44 0  ", @arrDebet,  @arrDebet2,, .F.)

                         // Add by TMV 30-03-2004
          accPrizList( "15 2  125", @arrDebet,  @arrDebet2,, .F.) 

          if len( arrDebet) < 1 then msgS := msgS3
          accPrizList( "15 1", @arrCredit, @arrCredit2)
          addCards( @arrCredit, @arrCredit2)
          typeOf := "5201"
     case ptr == 4
          accPrizList( "15 1", @arrDebet,  @arrDebet2)
          addCards( @arrDebet, @arrDebet2)
          if len( arrDebet) < 1 then msgS := msgS1
          accPrizList( "15 1", @arrCredit,  @arrCredit2)
          addCards( @arrCredit, @arrCredit2)
     case ptr == 5
          accPrizList( "15 1", @arrDebet,  @arrDebet2)
          accPrizList( "15 4", @arrDebet,  @arrDebet2,, .F.)
          if len( arrDebet) < 1 then msgS := msgS1
          accPrizList( "15 1", @arrCredit,  @arrCredit2)
          addCards( @arrCredit, @arrCredit2)
          typeOf := "5200"
     case ptr == 6  ////////  Add by TMV  25-09-2001
                    accPrizList( "15 1", @arrDebet,  @arrDebet2)
                         addCards( @arrDebet, @arrDebet2)
                         if len( arrDebet) < 1 then msgS := msgS1
                         accPrizList( "15 1", @arrCredit,  @arrCredit2)
                         addCards( @arrCredit, @arrCredit2)
                         typeOf := "7100"
                         sNote=' в другой банк'
     case ptr == 7  ////////  Add by TMV  25-09-2001
                    accPrizList( "15 1", @arrDebet,  @arrDebet2)
                         addCards( @arrDebet, @arrDebet2)
                         if len( arrDebet) < 1 then msgS := msgS1
                         accPrizList( "15 1", @arrCredit,  @arrCredit2)
                         addCards( @arrCredit, @arrCredit2)
                         typeOf := "7100"
                         sNote=' на счет в филиале'
  endcase
  if !empty( msgS)
     messageB( msgS)
     return .F.
  endif

  ptrCredit := ptrDebet := 1
  sCredDest := space(20)   //  Add By TMV  25-09-2001
  summa := 0.00
  isFound   := .T.
// Replaced by TMV  24-12-2001
  bankDest  := padR( confBankName() + " " + AllTrim(confFilial()) +;
        " ПБО " + confOtdNumb(), 50)


  theDest   := padR( cliFullName(), 50)
  curDest   := "перечисление денежных средств на счет   "
  curSource := cliFullName()
// Edit by TMV  20-05-2002 -- NOT for Piter
  if Len(arrCredit) < 1
                messageB("У клиента открыт только один счет.")
                Return .F.
  endif
  credSchet( "")
  sCredDest:= space(20)   //  Add By TMV  25-09-2001
  sBikDest := space(9)    //  Add By TMV  25-09-2001
  BikDest  := space(9)    //  Add By TMV  25-09-2001
  KorSchet := space(20)   //  Add By TMV  25-09-2001
  sInnDest := space(12)   //  Add By TMV  25-09-2001
  if confUseAgent()
        curDest   := "безналичный перевод денежных средств"
  endif
  dest1   := padR( lTrim( arrDest[ ptr]), 50)  // Removed by TMV  16-10-2001
  dest2 := dest3 := dest4 := dest5 := Space( 50)
 
//  Replaced by TMV 04-10-2001
  gets := {{ "Безналичное перечисление" + sNote      },;
           { "Клиент : " + cliFullName()             },;
           { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"      },;
           { "Дебет  :", block( ptrDebet), arrDebet,,  ;
             {|| chngShow( arrDebet, arrDebet2, ptr)} },;
           { "*" + chr( 25)                          },;
           { "Кредит :", block( ptrCredit), arrCredit,,;
             {|g| chkCredit( g, arrCredit, @isFound, ptr)}},;
           { "*" + chr( 25)                          },;
           { ""                                      },;
           { "Сумма  :", block( summa),pict,{|| ptr <> 2} ,;
                 {|g| chkSumma( g, arrDebet, arrDebet2,;
                         arrCredit, arrCredit2, ptr)}},;
           { ""                                      },;
           { "Банк получателя :", block( bankDest)   },;
           { "Получатель      :", block( theDest ),,{||chkDest( arrCredit, @lastZ) }},;
           { "Назн.платежа    :", block( dest1)},;  //,,{|| !confUseAgent()} },; //  Edited by TMV 04-10-2001
           { "                :", block( dest2)},;  //,,{|| !confUseAgent()} },; //  Edited by TMV 04-10-2001
           { "                :", block( dest3)} }  //,,{|| !confUseAgent()} }}  //  Edited by TMV 04-10-2001

        BikDest:=sBikDest:=setUpStrBic()
        KorSchet=setUpKorSchet()

  if ptr == 6 .Or. ptr == 7     //  Add By TMV  25-09-2001
        if ptr == 7
                KorSchet=confKorConfSchet()  // setUpKorSchet()
        else
                bankDest = space(50)
        endif
        theDest = space(50)
        asize(gets, Len(gets) + 2)
        ains(gets, 6); ains(gets, 6); ains(gets, 6)
        gets[6]={ "Бик банка получателя :", block( sBikDest),;
                                '999999999', {|| ptr = 6},;
                                {|g| chkBikDest( g, ptr, @BikDest)}}
   gets[7]={ "Кор.счет :", block( KorSchet),;
                                '@R 99999 999 9 99999999999',{|| ptr = 6},;
                           {|g| chkKorSchet(g, BikDest)}}
   gets[8]={ "ИНН :", block( sInnDest), '999999999999',{|| ptr = 6}}
        gets[9]={ "Счет получателя :", block( sCredDest),'@R 99999 999 9 99999999999',;
                                {|| ptr = 6.or.ptr = 7}, {|g| chkCredSchet(g, BikDest, ptr)}}
   aDel( gets, 10); aSize( gets, len( gets) - 1)
   gets[14]={ "Получатель      :", block( theDest )}
  endif
//  Removed by TMV 04-10-2001
  if ptr == 2 .OR. ptr == 5
     aDel( gets, 9); aSize( gets, len( gets) - 1)
  endif
  if ptr == 3
     aAdd( gets, NIL); aIns( gets, 9)
     gets[ 9] := ;
           { "%%     :", {|| pctDopShow( arrDebet, arrDebet2)},,{|| .F.}}
  endif

  if getUp( 2, 2, gets)
     curDest := allTrim( dest1) + " " + allTrim( dest2) + " " + ;
                allTrim( dest3)
     curDest := allTrim( curDest)

     debSchet := arrDebet2[ ptrDebet, 2]
//  Removed by TMV 04-10-2001
     if ptr == 2 .OR. ptr == 5
        summa := sumRest(  debSchet, setCurDate()) + ;
                 sumPType( debSchet, opDecode( "4002")) + sumPType( debSchet, opDecode( "4001"))
     endif
          if ptr != 6 .And. ptr != 7  //  Add By TMV  25-09-2001
             credSchet := arrCredit2[ ptrCredit, 2]
          else
             credSchet := sCredDest
             isFound = .F.
          endif
     if credSchet == debSchet
        messageB( "По дебету и по кредиту указан один и тот же счет !!!")
        return .F.
     endif

     if !msgShowMessage( credSchet) .OR. !msgShowMessage( debSchet)
        return .F.
     endif
//  Removed by TMV 04-10-2001
     if ( ptr == 1 .OR. ptr == 2 .OR. ptr == 5) .AND. isFound
        if !chkAccIncom( summa, credSchet)
           return .F.
        endif
     endif
     ACC ->( baseSavePos())
     ACC ->( dbSetOrder( 1))
          //  Add By TMV  25-09-2001
          if ptr != 6 .And. ptr != 7
                if ACC ->( dbSeek( credSchet)) .AND. isSR()
        if round( Summa -int( Summa), 2) <> 0.00
          messageB( "Сумма должна быть кратна 1.00 руб.")
          ACC ->( baseRestPos())
          return .F.
        endif
      endif
          endif
//  Removed by TMV 04-10-2001
     if ptr == 1 .AND. ACC ->( dbSeek( credSchet)) .AND. ACC ->typeOf == "02"
        ACC ->( baseRestPos())
        errOK( "Счет " + credSchet + " закрыт!")
        return .F.
     endif

     ACC ->( baseRestPos())

     //
     // вот оно СЗ i055032001 пункт 2.4
     //
//  Removed by TMV 04-10-2001
     if ptr == 2
       if !contSchetCheck( debSchet) then return .F.
     endif

     if ptr == 5
       if !checkCardAccForClose( debSchet) then return .F.
     endif


     //
     // генерация проводок !!!
     //
     memDoc  := ordIncMem( setCurDate())
     d2      := CtoD( "")
     docNumb := Space( 30)

     if .T. // confUseAgent() .OR. getUp( 2, 2, gets)

        if yesNo( "Вы действительно хотите перечислить сумму " + nStr( summa) + " ?")
                        if confUseAgent()
           aOrder := {}
// Add by TMV 18-05-2002
           aWisc := memWisc2( debSchet, credSchet, theDest, summa, curDest,;
                { dest1, dest2, dest3, dest4, dest5})
         else
           aOrder  := memOrd2( memDoc,setCurDate(), debSchet, credSchet, summa,;
                      curDest, "9", bankDest, theDest)
           aWisc := memWisc2( debSchet, credSchet, theDest/*bankDest*/, summa, curDest,;
                    { dest1, dest2, dest3, dest4, dest5})
         endif
           printAOrder( aWisc)
                          if ptr == 6 .Or. ptr == 7
                                        printAOrder( aWisc)
                                        if ptr == 6 then printAOrder( aWisc)
                          endif
           if Len( aOrder ) > 0 Then printAOrder( aOrder)  //  Removed by TMV 04-10-2001
           if ptr == 1 //Replaced by TMV 05-10-2001
              SRSumma( summa)
              ACC ->( baseSavePos())
              ACC ->( dbSetOrder( 1))
              if ACC ->( dbSeek( credSchet))
                 //
                 // сделать перечисление
                 //
                 nakMakeTrans()

                 SRSumma( summa)
                 if isSR()
                    if ACC ->datOpen == setCurDate() then accClrSigned()
                    SRSumma( summa + sumRest( ACC ->schet, setCurDate()))
                 endif
                 sumDesc := getSumDesc( ACC ->schet, @desc2)
                 if len( sumDesc) > 1 then aAdd( sumDesc, "  в сумме : " + getS( summa))
                 aAdd( sumDesc, "перечислением со счета : N " + debSchet + ;
                   " ордер N " + nStr( memDoc) + " от " + DtoC( setCurDate()))
                 aAdd( servDummy(), { "@sumDesc", sumDesc })
                 aAdd( servDummy(), { "@desc2", addDesc2( desc2, "безналичным перечислением")})
                 if ptr == 1 .AND. isFound
                    oldClient := cliCode()
                    newClient := cliCode()
                    ACC ->( baseSavePos())
                    ACC ->( dbSetOrder( 1))
                    if ACC ->( dbSeek( credSchet))
                       newClient := ACC ->code
                    endif
                    ACC ->( baseRestPos())
                    if oldClient <> newClient
                       CLIENTS ->( baseSavePos())
                       CLIENTS ->( dbSetOrder(1))
                       CLIENTS ->( dbSeek(  newClient))
                       addPrintOtherPerson( oldClient)
                       schetPrintOpen( .T.)
                       CLIENTS ->( baseRestPos())
                    else
                       addPrintOtherPerson()
                       schetPrintOpen( .T.)
                    endif
                 else
                   addPrintOtherPerson()
                   schetPrintOpen( .T.)
                 endif

                 if isSr() .AND. !accIsSigned() then accSetSigned()
                 servDummy( {})
              else
                 messageB( "Счет " + credSchet + " не найден !!!")
              endif
              ACC ->( baseRestPos())
           endif
//  Removed by TMV 04-10-2001
           if ( ptr == 2 .OR. ptr == 5)  .AND. ;
              summa > ( sumRest( debSchet, setCurDate()) + ;
              sumPType( debSchet, "4002") + sumPType( debSchet, "4001") )
              messageB( "Остаток по счету " + debSchet + "меньше " + ;
                 lTrim( mStr( summa)))
              return .F.
           endif

//  Removed by TMV 04-10-2001
           if ptr == 3
              ACC ->( baseSavePos())
              ACC ->( dbSetOrder( 1))
              if ACC ->( dbSeek( debSchet))
                 if summa > sumPctDop()
                    messageB( "Можно снять " + lTrim( mStr( sumPctDop())) )
                    ACC ->( baseRestPos())
                    return .F.
                 endif
              endif
              ACC ->( baseRestPos())
           endif


           pctSum := sumPType( debSchet, "4002") + sumPType( debSchet, "4001")
//  Removed by TMV 04-10-2001
           if ptr == 2 .AND. pctSum <> 0
             sumWrite( debSchet, pctSum, setCurDate(), "9100", 0)
           endif
           if isFound
              if sumWrite( debSchet, - summa, setCurDate(), typeOf,;
                 memDoc,, "B" + credSchet)
                 ACC ->( baseSavePos())
                 ACC ->( dbSetOrder( 1))
                 if ACC ->( dbSeek( debSchet))
                    if accRLock()
                       // возврат срочного вклада
                       if ( ptr == 2 .OR. ptr == 5 ) .AND. !isPens( ACC ->priz)
                          ACC ->datClose := setCurDate()
                          ACC ->typeOf   := "02"
                          accUnLock()
                          schetClosePrint( ACC ->schet, .F., credSchet)

                       else
                          ACC ->( netUnLock())
                       endif
                    else
                       errLog( "Счет " + debSchet + " не закрыт!")
                    endif
                 else
                    errLog( "Счет " + debSchet + " не найден и не закрыт!")
                 endif
                 ACC ->( baseRestPos())
                 if !sumWrite( credSchet, summa, setCurDate(), typeOf, memDoc,,;
                    "C" + debSchet)
                    errLog( "Проводка НЕ СДЕЛАНА. Обратитесь на филиал!")
                 endif
              else
                  errLog( "Проводка НЕ СДЕЛАНА. Обратитесь на филиал!")
              endif
           else
                                  if ptr < 6
                if !sumWrite( debSchet, - summa, setCurDate(), typeOf,;
                                memDoc,, "F" + credSchet())
                        errLog( "Проводка НЕ СДЕЛАНА. Обратитесь на филиал!")
                endif
                                  else
                if !sumWrite( debSchet, - summa, setCurDate(), typeOf,;
                                memDoc,, "F" + sCredDest + sBikDest) .Or.; // Replaced by TMV 04-10-2001
                                                        !wiscSumWrite( debSchet, summa, setCurDate(), typeOf,;
                                                                curDest, sCredDest, sBikDest, KorSchet, theDest,;
                                                                sInnDest, memDoc, bankDest)
                        errLog( "Проводка НЕ СДЕЛАНА. Обратитесь на филиал!")
                endif
                                  endif
           endif
                          printHistory( debSchet, setCurDate())
        endif
     endif
  endif
return .T.

autofunction setValue credSchet init ""

static funcDef chkDest with arrCredit:A, lastZ:LU local rV, ptr
   ptr := aScan( arrCredit,;
          {|x| allTrim( x ) == allTrim( M ->getList[ 2]:varGet) })
   rV := lastZ := ptr == len( arrCredit)
return rV


static funcDef chkCredit with g:O, aCredit:A, isFound:L, ptr:N ;
   local rV := .T., ptr2, pict, newName

   ptr2 := aTrimmedScan( aCredit, g:varGet)
   if ptr2 == len( aCredit) .And. aCredit[ptr2] == ACC_TARGET_ANY
      if empty( credSchet()) then credSchet( Space( 20))
      pict  := "@R 99999 999 9 9999 9999999"
      if !getUp( 5, 5, {{"Введите счет по кредиту : ", funBlock( credSchet),;
            pict,, {|g| chkSchet( g, @isFound, @newName, ptr) }}})
         credSchet( "")
         rV := .F.
      endif
      if !empty( newName)
                if ptr == 3
                                M ->getList[ 6]:varPut( newName)
                M ->getList[ 6]:display()
                        elseif ptr == 5 .Or. ptr == 2
                M ->getList[ 4]:varPut( newName)
                M ->getList[ 4]:display()
                        else
                M ->getList[ 5]:varPut( newName)
                M ->getList[ 5]:display()
                        endif
      endif
   else
      credSchet( "")
   endif
   @ M ->getList[ 2]:row() + 1, M ->getList[ 2]:col() ;
     say padL( credSchet(), 20) color normalColor( setGetColor())
return rV

funcDef addCards with arr1:A, arr2:A local tmp1, tmp2, i
   accPrizList( "15 4", @tmp1, @tmp2)
   for i := 1 to len( tmp1)
       aAdd( arr1,  tmp1[ i])
       aAdd( arr2, aClone( tmp2[ i]))
   next
return NIL

static funcDef chkSumma with g:O, arr1:A, arr2:A, aCred:A, aCred2:A, ptr:N ;
   local rV := .T., ptr2, value, rest, addMin, addMax, sumPctDop, ar1, ar2
   if empty( g:varGet)
      messageB( "Вы ОБЯЗАНЫ ввести сумму !!!")
      return .F. .OR. g:exitState == GE_UP
   endif
   if g:varGet < 0
      messageB( "Сумма ДОЛЖНА быть положительной !!!")
      return .F. .OR. g:exitState == GE_UP
   endif
   value := M ->getList[ 1]:varGet
   ptr2 := aTrimmedScan( arr1, value)
   if ptr2 > 0
      rest := sumRest( arr2[ ptr2, 2], setCurDate())
      if rest < g:varGet
         messageB( "Остаток по этому счету " + nStr( rest) + " будет КРАСНОЕ сальдо !!!")
         return .F. .OR. g:exitState == GE_UP
      endif
      if ptr = 6 .or. ptr = 7   //  Add By TMV  25-09-2001
         return .T. .OR. g:exitState == GE_UP
      endif
      ACC ->( baseSavePos())
      value := M ->getList[ 2]:varGet
      ptr2  := aTrimmedScan( aCred, value)
      if ptr2 == len( aCred) .AND. !empty( credSchet())
         if credSchet() == confKorSchet()
            ACC ->( baseRestPos())
            return g:exitState == GE_UP
         endif
         ACC ->( dbSeek( credSchet()))
      else
         ACC ->( dbGoTo( aCred2[ ptr2, 1]))
      endif
      do case
         case ptr == 1 .OR. ptr == 5
              if isCanDopVznos() .OR. ACC ->datOpen == setCurDate()
                if sumRest( ACC ->schet, setCurDate()) = 0 .AND. ;
                   ACC ->datOpen == setCurDate() .AND. isSR()
                   vidArray( ACC ->PRIZ, @ar1, @ar2)
                   if !( rV := chkIncSumma( g:varGet, ar2, 1))
                      errLog( { "На этот тип счета вносятся суммы из диапазона"       ,;
                                "от " + mStr( ar2[ 1, 2]/ 100) + " до " + mStr( ar2[ 1, 3]/ 100) ,;
                                "", "а вы вносите " + mStr( g:varGet) })
                   endif
                else
                 vidGetAdd( ACC ->Priz, @addMin, @addMax)
                 addMin := addMin/ 100
                 addMax := addMax/ 100
                 if !( (g:varGet >= addMin .AND. g:varGet <= addMax))
                    errLog( { "Вы пытаетесь довложить сумму " + nStr( g:varGet),;
                             "по данному типу вклада разрешено довложение "   ,;
                             "от " + nStr( addMin) + " до " + nStr( addMax) })
                    rV := .F.
                 endif
                endif
              else
                 messageB( "Довложения по данному типу вклада запрещены !!!")
                 rV := .F.
              endif
         case ptr == 2
              if rest <> g:varGet
                 messageB( "Вы не можете снять часть вклада !!!")
                 rV := .F.
              endif
         case ptr == 3
              ptr2 := aTrimmedScan( arr1, M ->getList[ 1]:varGet)
              if ptr2 > 0
                 ACC ->( baseSavePos())
                 ACC ->( dbGoTo( arr2[ ptr2, 1]))
                 sumPctDop := sumPctDop()
                 if sumPctDop == 0
                    messageB( "У счета нет процентов для снятия !!!")
                    rV := .F.
                 else
                    if sumPctDop < g:varGet
                       messageB( "Начислено процентов всего-то " + nStr( sumPctDop) + " !!!")
                       rV := .F.
                    endif
                    if ( is45x150() .OR. isKindDeal() .OR. isNam10() .OR. isZenit2001() ) .AND. ;
                       round( sumPctDop, 2) <> round( g:varGet, 2)
                       messageB( "Вы можете снять только " + mStr( sumPctDop))
                       rV := .F.
                    endif
                 endif
                 ACC ->( baseRestPos())
              endif
         case ptr == 4
              rV := .T.
      endcase
      ACC ->( baseRestPos())
   endif
return rV .OR. g:exitState == GE_UP

static funcDef pctDopShow with arr1:A, arr2:A local rV := 0.00, ptr2
  if len( M ->getList) == 0 then return rV
  ptr2 := aTrimmedScan( arr1, M ->getList[ 1]:varGet)
  if ptr2 > 0
     ACC ->( baseSavePos())
     ACC ->( dbGoTo( arr2[ ptr2, 1]))
     rV := sumPctDop()
     ACC ->( baseRestPos())
  endif
return rV

static funcDef chngShow with arr1:A, arr2:A, ptr:N
if len( M ->getList) >= 3 .AND. ptr == 3
   M ->getList[ 3]:varPut( pctDopShow( arr1, arr2))
   M ->getList[ 3]:display()
endif
return .T.

static funcDef chkSchet with g:O, isFound:LU, newName:CU, ptr:N ;
   local rV := .T., newSchet, addMin, addMax
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))

   isFound := ACC ->( dbSeek( g:varGet))
   if !isFound .AND. g:varGet <> confKorSchet()
       messageB( "Счет " + g:varGet  + " не найден в базе данных !!!")
       ACC ->( baseRestPos())
       return .F.
   else
      do case
         case ptr == 1
              rV := left( ACC ->Priz, 4) == "15 2"
         case ptr == 2
              rV := left( ACC ->priz, 4) == "15 1" .OR. ;
                    left( ACC ->priz, 4) == "15 4" .OR. ;
                    left( ACC ->Priz, 4) == "15 2"
         case ptr == 3
              rV := left( ACC ->priz, 4) == "15 1" .OR. ;
                    left( ACC ->priz, 4) == "15 4"
         case ptr == 4
              rV := left( ACC ->priz, 4) == "15 1" .OR. ;
                    left( ACC ->priz, 4) == "15 4"
         case ptr == 5  // не понял ?
              if isSR()
                 rV := .F.
                 if ACC ->datOpen == setCurDate() .OR. isCanDopVznos()
                    rV := .T.
                 endif
              else
                 rV := left( ACC ->priz, 4) == "15 1" .OR. ;
                       left( ACC ->priz, 4) == "15 4"
              endif
      endcase
      if !rV
         messageB( "Тип счета не соотвествует режиму !!!")
      endif
      CLIENTS ->( baseSavePos())
      CLIENTS ->( dbSetOrder( 1))
      if CLIENTS ->( dbSeek( ACC ->code))
         newName := padR( cliFullName(), 50)
      endif
      CLIENTS ->( baseRestPos())
   endif
   ACC ->( baseRestPos())
return rV

funcDef strStw local bOld, str1, str2
   bOld := SetKey( K_ALT_P)
   setKey( K_ALT_P, NIL)
   str1 := ;
      chr(  32) + chr( 140) + chr( 174) + chr( 173) + chr( 168) + chr( 226) + ;
      chr( 174) + chr( 224) + chr(  32) + chr( 143) + chr( 129) + chr( 142) + ;
      chr(  32) + chr(  40) + chr(  32) + chr( 144) + chr( 160) + chr( 161) + ;
      chr( 174) + chr( 226) + chr( 160) + chr(  32) + chr( 225) + chr(  32) + ;
      chr( 175) + chr( 165) + chr( 173) + chr( 225) + chr( 168) + chr( 174) + ;
      chr( 173) + chr( 165) + chr( 224) + chr( 160) + chr( 172) + chr( 168) + ;
      chr(  41)


   str2 := chr( 128) + chr( 129) + chr(  34) + chr( 143) + chr( 165) + ;
          chr( 226) + chr( 224) + chr( 174) + chr( 162) + chr( 225) + ;
          chr( 170) + chr( 168) + chr( 169) + chr(  34) + chr(  40) + ;
          chr(  99) + chr(  41) + chr(  49) + chr(  57) + chr(  57) + ;
          chr(  56)

   OK( { str1, "", str2})
   setKey( K_ALT_P, bOld)
return NIL

funcDef accIsNevsk with cPriz:C local rV, thePriz, aPriz
   thePriz := left( cPriz, 9)
   aPriz   := { "15 2  18 ", "15 2  19 ", "15 2  20 ",;
                "15 2  21 ", "15 2  22 ", "15 2  23 "}
   rV      := aScan( aPriz, {|x| x == thePriz}) >0
return rV


* ------------------- внос вклада на 3 лицо !!! -------
funcDef accOtherPerson local rV := "", arr1, newCode, newRec
  vidArray( ACC ->PRIZ,, @arr1,, .F.)
  if left( arr1[ 1, 8], 1) == "1"
     if dialog( { "Если вклад сделан НА ИМЯ ТРЕТЬЕГО ЛИЦА,",;
                  "то Вы должны ввести данные о вносителе ",;
                  ""                                       ,;
                  "Вы хотите ввести данные о ВНОСИТЕЛЕ ?"} ,;
                { "НЕТ", "ДА"}) == 2
        CLIENTS ->( baseSavePos())
        newCode := ""; newRec := 0
        if viewOwner( @newCode, @newRec)
          CLIENTS ->( dbGoTo( newRec))
          rV := CLIENTS ->code
        else
          if cliNewClient()
             rV := CLIENTS ->code
          endif
        endif
        CLIENTS ->( baseRestPos())
     endif
  endif
return rV



funcDef credOtherPerson local rV := "", arr1 := {}, newCode, newRec


  vidArray( ACC ->PRIZ,, @arr1,, .F.)
  if len( arr1) <> 0
    if left( arr1[ 1, 8], 1) == "1"
      if !noYes( "Кредит возвращается владельцем счета ?")
         CLIENTS ->( baseSavePos())
         newCode := ""; newRec := 0
         if viewOwner( @newCode, @newRec)
           CLIENTS ->( dbGoTo( newRec))
           rV := CLIENTS ->code
         else
           if cliNewClient()
              rV := CLIENTS ->code
           endif
         endif
         CLIENTS ->( baseRestPos())
      endif
    endif
  endif
return rV


* ----------------- поручение о третьем лице !!! ---------
funcDef accOtherWisc ;
  with  newClient:C, dogCode:C, summa:N, nDoc:N, curDate:D ;
  local rV := .F., d := ";"
  if empty( newClient) then return .T.
  if WISC ->( mAppend())
     WISC ->schet  := ACC ->schet
     WISC ->type   := WISC_OTHER_PERSON
     WISC ->buffer := newClient + d + dogCode + d + WISC ->schet + d + ;
                      nStr( summa)  + d + nStr( nDoc) + d + DtoC( curDate)
     WISC ->( netUnLock())
     rV := .T.
  endif
return rV

funcDef accOtherGet with schet:C local rV := "??????", arr
if WISC ->( dbSeek( schet + WISC_OTHER_PERSON))
   arr := CtoA( WISC ->buffer, ";")
   if len( arr) >= 2 then rV := arr[ 2]
endif
return rV

* ---------------- дополнение информации о вносителе -----------------
funcDef addPrintOtherPerson with newClient:CU ;
  local arr, paspName, i, aDoc, persName, doc, addr,;
        curClient := CLIENTS ->code
arr := servDummy()


CLIENTS ->( baseSavePos())
CLIENTS ->( dbSetOrder( 1))

if !empty( newClient) .AND. CLIENTS ->( dbSeek( newClient))
   paspName := ""
   persName := cliFullName()
   aDoc     := razbivka( cliDocum(), {{ 40, "L"}, { 40, "L"}})
   for i := 1 to len( aDoc) do paspName += aDoc[ i] + endl()

   addr := razbivka( "Адрес :" + cliFullAddress(), {{ 44, "L"}, { 44, "L"}})

   aAdd( arr, { "@fiovzn",   persName})
   aAdd( arr, { "@doc1vzn",  adoc[ 1] })
   aAdd( arr, { "@doc2vzn",  adoc[ 2] })
   aAdd( arr, { "@addr1vzn", addr[ 1] })
   aAdd( arr, { "@addr2vzn", addr[ 2] })
   aAdd( arr, { "@telvzn",   "Тел : " + cliPhone()})
endif

if isNam10()      then fillNam10(     @arr, newClient)
if isNewYear()    then fillNewYear(   @arr, paspName, newClient)

   fillAgent(     @arr, curClient, newClient)

CLIENTS ->( baseRestPos())
return persName

static funcDef fillNewYear with arr:A, paspName:CU, newClient:CU
if !empty( newClient)
  //
  // взнос на 3 лицо
  //
  if confUseDogovor()
    aAdd( arr, { "@other_person",;
     { "ВНОСИТЕЛЬ :" + cliFullName(), rTrim( paspName) ,;
       ""                                                                       ,;
       "Дата : _____________   _________________      _________________________",;
       "                       Подпись Вкладчика      Фамилия                  ",;
       ""                                       , ;
       "Дата : @dat_open**     _________________      _________________________",;
       "                       Подпись Вносителя      Фамилия                  "}})
  else
    aAdd( arr, { "@other_person",;
         { "Дата : @dat_open**     _________________      _________________________",;
           "                       Подпись Вносителя      фамилия                  ",;
           ""                                                                       ,;
           "Дата : _____________   _________________      _________________________",;
           "                       Подпись Вкладчика      фамилия                  "}})
  endif
else
  //
  // взнос на свое имя
  //
  if confUseDogovor()
     aAdd( arr, { "@other_person",{;
      "Дата : @dat_open**     _________________      _________________________",;
      "                       Подпись Вкладчика             Фамилия           "}})
  else
    aAdd( arr, { "@other_person",{;
       "Дата : @dat_open**     _________________      _________________________",;
       "                        подпись Вкладчика             фамилия           "}})
  endif
endif
return nil

static funcDef fillNam10 with arr:A, newClient:CU
if !empty( newClient)
  //
  // взнос на 3 лицо
  //
  if confUseDogovor()
    aAdd( arr, { "@other_person", { ""                      ,;
        "Дата : _____________   Подпись                                         ",;
        "                       Вкладчика ____________ Фамилия _________________",;
        ""                                       , ;
        "Дата : @dat_open**     Подпись                                         ",;
        "                       Вносителя ____________ Фамилия _________________"}})
    aAdd( arr, { "@vznositel", { "",;
                  "Вноситель: от @fiovzn*******************************************",;
                  "@doc1vzn******************************************",;
                  "@doc2vzn******************************************"}})
   else
    aAdd( arr, { "@other_person", { ""                      ,;
          ""                                                                       ,;
          "Дата : _____________   _________________      _________________________",;
          "                       подпись Вкладчика      фамилия                  ",;
          ""                                       , ;
          "Дата : @dat_open**     _________________      _________________________",;
          "                       подпись Вносителя      фамилия                  "}})
   endif
else
  //
  // оформляется на себя
  //
  if confUseDogovor()
   aAdd( arr, { "@other_person",;
        { "",;
          "Подпись ______________________  Фамилия ______________________"}})
  else
    aAdd( arr, { "@other_person", { ""                      ,;
          ""                                                                       ,;
          "Дата : @dat_open**     _________________      _________________________",;
          "                       подпись Вкладчика      фамилия                  "}})
  endif
endif
return NIL

static funcDef getDateVoz local rV := ACC ->datClose
  if empty( rV)
     if !empty( rV := vidGetDatClose( ACC ->datOpen, ACC ->PRIZ))
       if isSpring() .OR. isNam10() .OR. isNewYear() then rV--
     endif
  endif
return rV


static funcDef fillZenit2001 with arr:A, curCode:C, newCode:CU ;
   local aDoc, addr, arr2 := {}
CLIENTS ->( baseSavePos())
CLIENTS ->( dbSetOrder( 1))
//
// кто вносит
//
CLIENTS ->( dbSeek( if( empty( newCode), curCode, newCode)))

aDoc := razbivka( cliDocum(), {{ 53, "L"}, { 53, "L"}})
addr := razbivka( cliFullAddress(), {{ 53, "L"}, { 53, "L"}})

aAdd( arr, { "@fiovzn",   cliFullName()})
aAdd( arr, { "@doc1vzn",  adoc[ 1] })
aAdd( arr, { "@doc2vzn",  adoc[ 2] })
aAdd( arr, { "@addr1vzn", addr[ 1] })
aAdd( arr, { "@addr2vzn", addr[ 2] })
aAdd( arr, { "@telvzn",   cliPhone()})
aAdd( arr, { "@fio_",     cliFullName()})

if confUseDogovor()
  if empty( newCode)
    aAdd( arr, { "@other_person",{;
      "ВКЛАДЧИК: " + upper( cliFullName()), cliDocum()                     ,;
      ""                                                                   ,;
      "Дата @curdate**   ____________________    _________________________",;
      "                   Подпись Вкладчика            Фамилия            ",;
      ""  }})
    aAdd( arr, { "@other_person2",{;
      "Дата @curdate**   ____________________    _________________________",;
      "                   Подпись Вкладчика            Фамилия            ",;
      ""  }})
  else
    CLIENTS ->( dbSeek( curCode))
    arr2 := {   "ВКЛАДЧИК: "  + upper( cliFullName()), cliDocum(), ""}
    CLIENTS ->( dbSeek( newCode))
    aAdd( arr2, "ВНОСИТЕЛЬ: " + upper( cliFullName()) )
    aAdd( arr2, cliDocum() )
    aAdd( arr2, ""         )
    aAdd( arr2,"Дата _________________     _____________________   __________________________")
    aAdd( arr2,"                             Подпись Вкладчика        Фамилия                ")
    aAdd( arr2,""                                                                             )
    aAdd( arr2,"Дата @curdate**            _____________________   __________________________")
    aAdd( arr2,"                             Подпись Вносителя        Фамилия                ")
    aAdd( arr, { "@other_person", arr2})
    aAdd( arr, { "@other_person2",{;
               "Дата _________________     _____________________   __________________________",;
               "                             Подпись Вкладчика        Фамилия                ",;
               ""                                                                             ,;
               "Дата @curdate**            _____________________   __________________________",;
               "                             Подпись Вносителя        Фамилия                "}})
  endif
else
  if empty( newCode)
     aAdd( arr, { "@other_person",{;
       "Клиент:               ____________________   _________________________",;
       "                              подпись             фамилия             "}})
  else
   aAdd( arr, { "@other_person",{;
     "Вноситель             ____________________   _________________________",;
     "                        подпись Вносителя             фамилия         ",;
     ""                                                                      ,;
     "Клиент:               ____________________   _________________________",;
     'Дата "____".____.200_         подпись             фамилия             '}})
  endif
endif

//
// на чье имя
//
if !empty( newCode) .AND. CLIENTS ->( dbSeek( curCode))
   aDoc := razbivka( cliDocum(), {{ 72, "L"}, { 72, "L"}})
   addr := razbivka( "адрес регистрации:" + cliFullAddress(), {{ 72, "L"}, { 72, "L"}})

   aAdd( arr, { "@fio_",   cliFullName()})
   aAdd( arr, { "@doc_1",  adoc[ 1] })
   aAdd( arr, { "@doc_2",  adoc[ 2] })
   aAdd( arr, { "@addr_1", addr[ 1] })
   aAdd( arr, { "@addr_2", addr[ 2] })
   aAdd( arr, { "@tel_",   "телефон: " + cliPhone()})

endif
if isRozha()
   aAdd( arr, { "@other_person2",{;
    "Дата : @dat_open**     _________________      _________________________",;
    "                       Подпись Вкладчика      фамилия                  "}})
else
   aAdd( arr, { "@other_person2",;
   "Подпись Вкладчика ________________ Подпись Вносителя ____________________"})
endif

CLIENTS ->( baseRestPos())

return NIL

//
// агентские отношения
//
static funcDef fillAgent with arr:A, curCode:C, newCode:CU ;
   local aDoc, addr, arr2 := {}, fio, fiovzn, fioDocum, fioVznDocum
CLIENTS ->( baseSavePos())
CLIENTS ->( dbSetOrder( 1))
//
// кто вносит
//
CLIENTS ->( dbSeek( if( empty( newCode), curCode, newCode)))

aDoc := razbivka( cliDocum(), {{ 53, "L"}, { 53, "L"}})
addr := razbivka( cliFullAddress(), {{ 53, "L"}, { 53, "L"}})

aAdd( arr, { "@fiovzn",   cliFullName()})
aAdd( arr, { "@doc1vzn",  adoc[ 1] })
aAdd( arr, { "@doc2vzn",  adoc[ 2] })
aAdd( arr, { "@addr1vzn", addr[ 1] })
aAdd( arr, { "@addr2vzn", addr[ 2] })
aAdd( arr, { "@telvzn",   cliPhone()})
aAdd( arr, { "@fio",      cliFullName()})
fio    := cliFullName()
fiovzn := cliFullName()
fioDocum := cliDocum()
fioVznDocum := cliDocum()
if empty( newCode)
  aAdd( arr, { "@other_person",{;
    "Клиент:               ____________________   _________________________",;
    "                              подпись             фамилия             "}})
  aAdd( arr, { "@other_person2",{;
      "Дата @curdate**   ____________________    _________________________",;
      "                   Подпись Вкладчика            Фамилия            ",;
      ""  }})
else
   aAdd( arr, { "@other_person",{;
     "Вноситель             ____________________   _________________________",;
     "@curdate**              подпись Вносителя               фамилия       ",;
     ""                                                                      ,;
     "Клиент: ______________ ___________________   _________________________",;
     '             дата             подпись                   фамилия       '}})
    aAdd( arr, { "@other_person2",{;
               "Дата:_________________     _____________________   __________________________",;
               "                             Подпись Вкладчика        Фамилия                ",;
               ""                                                                             ,;
               "Дата @curdate**            _____________________   __________________________",;
               "                             Подпись Вносителя        Фамилия                "}})
endif

//
// на чье имя
//
if !empty( newCode) .AND. CLIENTS ->( dbSeek( curCode))
   aDoc := razbivka( cliDocum(), {{ 72, "L"}, { 72, "L"}})
   addr := razbivka( "адрес регистрации:" + cliFullAddress(), {{ 72, "L"}, { 72, "L"}})
   arr2 := { "", aDoc[ 1]}
   if !empty( aDoc[ 2]) then aAdd( arr2, aDoc[ 2])
   aAdd( arr2, addr[ 1])
   if !empty( addr[ 2]) then aAdd( arr2, addr[ 2])
   aAdd( arr2, "телефон: " + cliPhone())
   aAdd( arr2, "")
   aAdd( arr, { "@fio",   cliFullName()})
   aAdd( arr, { "@dopattr", arr2})
   fio      := cliFullName()
   fioDocum := cliDocum()
endif

CLIENTS ->( baseRestPos())

if !confUseAgent() .AND. confUseDogovor()
   if fio == fioVzn
    arr2 := { "ВКЛАДЧИК: "  + upper( fio), fioDocum, "",;
              "Дата @curdate**            _____________________   __________________________",;
              "                             Подпись Вкладчика        Фамилия                "}
   else
    arr2 := { "ВКЛАДЧИК: "  + upper( fio), fioDocum, "",;
              "ВНОСИТЕЛЬ: " + upper( fioVzn), fioVznDocum, "" ,;
              "Дата:_________________     _____________________   __________________________",;
              "                             Подпись Вкладчика        Фамилия                ",;
              ""                                                                             ,;
              "Дата @curdate**            _____________________   __________________________",;
              "                             Подпись Вносителя        Фамилия                "}
   endif
   aAdd( arr, { "@other_person", arr2})
endif


return NIL


* --------------- функции для работы с таблицей счетов ------------
funcDef brwGetSchet with par:NC local arr := {}, strRest, rest, s := select()

if vidOpen()
  ACC ->( baseSavePos())
  ACC ->( dbSetOrder( 2))
  accSeek( cliCode())
  while cliCode() == ACC ->code .AND. ACC ->( !eof())
    if chkAddSchet( par)
       rest := sumRest( ACC ->schet, setCurDate())
       strRest := padL( lTrim( mStr( abs( rest))), 13)
       strRest += if( rest < 0, " Д", " К")
       aAdd( arr, { brwShowSchet( ACC ->schet)        ,;
                    padR( vidGetName( ACC ->PRIZ), 30),;
                    DtoC( ACC ->datOpen)              ,;
                    strRest                           ,;
                    ACC ->( recNo())                  ,;
                    SplitSSchet( ACC ->schet)         ,;
                    getDateVoz()                      })
    endif
    skip 1 alias ACC
  enddo
  ACC ->( baseRestPos())
  vidClose()
endif
select (s)
return arr

//
//  Показываем задолженность по микрокредиту
//
funcDef brwGetRSchet with par:NC ;
   local arr := {}, strRest, rest, s := select()

   if vidOpen()
     ACC ->( baseSavePos())
     ACC ->( dbSetOrder( 2))
     accSeek( cliCode())
     while cliCode() == ACC ->code .AND. ACC ->( !eof())
       if chkAddRSchet( par)
          rest := sumRest( ACC ->schet, setCurDate())
          strRest := padL( lTrim( mStr( abs( rest))), 13)
          strRest += if( rest < 0, " Д", " К")
          if empty( ACC ->datclose)
              aAdd( arr, { brwShowSchet( ACC ->schet)        ,;
                           padR( vidGetName( ACC ->PRIZ), 30),;
                           DtoC( ACC ->datOpen)              ,;
                           strRest                           ,;
                           ACC ->( recNo())                  ,;
                           SplitSSchet( ACC ->schet)         ,;
                           getDateVoz()                      ,;
                           rest                              ,;
                           ACC ->priz                        ,; 
                           cliCode()                          })
          endif
       endif
       skip 1 alias ACC
     enddo
     ACC ->( baseRestPos())
     vidClose()
   endif
   select (s)
return arr

funcDef brwShowSchet with Schet:C local rV
  rV := Schet13( Schet)
  rV := left( rV, 5) + " " + subStr( rV, 6, 1) + " " + right( rV, 7)
return rV


funcDef brwViewSchet with arr:A, fk:AU, mode:NU ;
  local rV := 0, ptr := 1, aH, aC, x
  DEFAULT mode TO 0

   aH := {{ "Клиент " + cliFullName()                             },;
          { "▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀"}}
   aC := {{ "Счет",      {|| arr[ ptr][ 1]}},;
          { "Тип ",      {|| arr[ ptr][ 2]}},;
          { "Дата отк.", {|| arr[ ptr][ 3]}},;
          { "Остаток",   {|| arr[ ptr][ 4]}} }
   if mode == 4
      aC[ 4] := { "Проценты",   {|| arr[ ptr][ 4]}}
   endif
   //
   // показывать дату окончания срока вклада ( для срочного вклада)
   if mode == 5
     aAdd( aH, NIL); aIns( aH, 2)
     aH[ 2] := { "Дата окончания срока хранения ", {|| arr[ ptr][ 7]},,{||.F.}}
   endif
   DEFAULT fk TO {, {}}
   if len( fk) < 5 then aSize( fk, 5)
   if empty( fk[ 5])
      fk[ 5] := {{ "Заявл", {|| safePrintOpen( arr, ptr)} }}
   endif

   if conf12UseAgent()
      if len( fk) < 7 then aSize( fk, 7)
      if empty( fk[ 7])
         fk[ 7] := {{ "Подп.", {|| SKSPrintOpen( arr, ptr)} }}
      endif
   endif

   brwGetPtr( 1)
   if viewUp( 3, 3, aH, aC,;
      {|| x := aBrUp(     @arr, @ptr), brwGetPtr( ptr), x},;
      {|| x := aBrDown(   @arr, @ptr), brwGetPtr( ptr), x},;
      {|| x := aBrTop(    @arr, @ptr), brwGetPtr( ptr), x},;
      {|| x := aBrBottom( @arr, @ptr), brwGetPtr( ptr), x},,, fk) > 0
      rV := arr[ ptr][ 5]
   endif

return rV
//
//  Печать карточки с образцами подписей
//
funcDef SKSPrintOpen with arr:A, ptr:N ;
  local arrName, arrCode, crdType := "", ar1, ar2, openVid, rV := .F., s, nPole, ;
        arrName1 := {}, arrCode1 := {}, j, ;
        addr := razbivka( cliFullAddress(), {{ 25, "L"}, { 25, "L"}, ;
           { 25, "L"}, { 25, "L"} }), ;
        aFio  := razbivka( cliFullName(), {{ 25, "L"}, { 32, "L"}}), ;
        pasGiv := razbivka( cliPasGiv(), {{ 35, "L"}, { 35, "L"}})

  ar1 := {}; ar2 := {}

  ACC ->( baseSavePos())
  ACC ->( dbGoTo( arr[ ptr][ 5]))

  if left( ACC ->priz, 5) <> "15 4 "
     messageB( "Печать карточки с образцами подписей только для СКС!")
     return NIL
  endif

  servDummy( {{ "@fio" ,     cliFullName()  },;
              { "@fio1" ,    aFio[ 1]       },;
              { "@fio2" ,    aFio[ 2]       },;
              { "@pasnum",    cliPasSer() + ' ' + cliPasNum() },;
              { "@pasgiv1",  pasGiv[1]      },;
              { "@pasgiv2",  pasGiv[2]      },;
              { "@schet716", ACC ->schet    },;
              { "@sign1",    confSign1()    },;
              { "@curdate",  setCurDate()   },;
              { "@addr1",    addr[ 1]       },;
              { "@addr2",    addr[ 2]       },;
              { "@addr3",    addr[ 3]       },;
              { "@addr4",    addr[ 4]       },;
              { "@tel",      cliPhone()     },;
              { "@otdname",  confOtdName()  },;
              { "@bankname", confBankName() },;
              { "@dater",    cliBDate() },;
              { "@otdnumb",  confOtdNumb()  }})

  printOpen( "SKS", 1)

return NIL

funcDef safePrintOpen with arr:A, ptr:N ;
  local arrName, arrCode, crdType := "", ar1, ar2
  ar1 := {}; ar2 := {}

  ACC ->( baseSavePos())
  ACC ->( dbGoTo( arr[ ptr][ 5]))
  vidArray( ACC ->PRIZ, @ar1, @ar2,,.F.)
  if subStr( ar2[ 1, 8], 2, 1) == "N"
     ACC ->( baseRestPos())
     messageB( "По этому типу вклада распечатать заявление нельзя!")
     return NIL
  endif
  SRSumma( sumRest( ACC ->schet, setCurDate() ))
  if accIsCard( ACC ->schet)
     assGetInfo( "10", @arrCode, @arrName,,, .F.)
     if len( arrCode) < 1
        messageB( "Нет типов карт !!!")
     else
        CONTRACT ->( baseSavePos())
        CONTRACT ->( dbSetOrder( 4))
        if CONTRACT ->( dbSeek( ACC ->schet))
           arr :=  ctoA( CONTRACT ->mBuffer, ";")
           if len( arr) > 0
              arr[ 1]    := CtoA( arr[ 1], ",")
              arr[ 1, 1] := aTrimmedScan( arrCode, arr[ 1, 1])
              crdType    := crViewType( arrName, arr[ 1])
           endif
        endif
        if empty( crdType)
           if wiscTop( ACC ->schet, "979 ")
              wsCrDecode( @arr, arrCode)
                                  if arr[1] > 0   // Add by TMV 23-03-2004
                crdType := arrName[ arr[ 1]]
                                  endif
           endif
        endif
     endif
     CONTRACT ->( baseRestPos())
     servDummy( {{ "@schet718", ACC ->schet},;
                 { "@crd_type", crdType    }})
  endif

  addPrintOtherPerson( accOtherPerson())
  printOpen()
  if accIsCard( ACC ->schet) then servDummy( {})
  ACC ->( baseRestPos())
return NIL

funcDef schetClosePrint with schet:C, isCash:LU, schet2:CU ;
   local arr, isFound, arrDummy

DEFAULT isCash TO .T.
if _isTek( schet)
   if isCash
     arr := { "выдать наличными полностью."}
   else
     ACC ->( baseSavePos())
     ACC ->( dbSetOrder( 1))
     isFound := ACC ->( dbSeek( schet2))
     ACC ->( baseRestPos())
     arr := { "перечислить на счет N " + schet2 + " в Банке " + ;
                  if( isFound, confBankName(), replicate( "_", 29)),;
              replicate( "_", 80)                                 ,;
              "корр/счет N ____________________________, БИК _______________________________" }
   endif
   arrDummy := servDummy()
   servDummy( {{ "@schet711",  schet},;
               { "@where2pay", arr  }})
   printOpen( "tsclo")
endif

return NIL


funcDef cardClosePrint with schet:C ;
   local arr, isFound, arrDummy, isCard

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 1))
ACC ->( dbSeek( schet))
if accIsCard( ACC ->schet)
   servDummy( {{ "@rdate",    rDateFull( setCurDate()) },;
               { "@filial",   confFilial()         },;
               { "@otd",      confOtdNumb()        },;
               { "@schet718", ACC ->schet          }})
   printOpen( "CRD9")
   servDummy( {})
endif
 ACC ->( baseRestPos())

return NIL

//
// если у клиента не все в порядке с картами, то
// проверим напечатано ли у него заявление, если нет
// то печатаем, если да, то спрашиваем
//

funcDef checkCardAccForClose with schet:C local rV := .T.

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 1))
if ACC ->( dbSeek( schet))
  if accIsCard( schet)
    if accIsClosPrinted()
      if yesNo( "Печатать повторно заявление на закрытие счета ?")
         cardClosePrint( schet)
         accSetClosPrint()
      endif
    else
      cardClosePrint( schet)
      accSetClosPrint()
    endif
    rV := chkCards()
  endif
endif
ACC ->( baseRestPos())
return rV


autofunction setValue brwGetPtr init 0

funcDef is45x150
return ACC ->PRIZ == "15 2  39 0  "

funcDef isKindDeal
return ACC ->PRIZ == "15 2  44 0  "

funcDef isSpring
return ACC ->PRIZ $ "15 2  47 0  ,15 2  48 0  "

funcDef isNam10
return ACC ->PRIZ $ "15 2  49 0  ,15 2  50 0  "

funcDef isNewYear
return ACC ->PRIZ $ "15 2  51 0  ,15 2  52 0  "

funcDef isZenit2001
return ACC ->PRIZ $ "15 2  53 0  "

func isNak
static str
local s := select(), rV := .F., opened := select( "VID") >0, printType
if isNIL( str)
  str:= ""
  messageStartUp( "Проверка типов вкладов")
  if !opened then rV := vidOpen()
  if opened .OR. rV
   VID ->( baseSavePos())
   VID ->( dbGoTop())
   while VID ->( !Eof())
     printType := vidGetPrintType( VID ->PRIZ)
     if printType == "2"
       str += VID ->PRIZ + ","
     endif
     if VID ->live .AND. ( printType < "1" .OR. printType > "5") .AND. ;
        left( VID ->PRIZ, 5) == "15 2 "
       errOK( "Для вклада " + VID ->name + " нет типа заявления для печати !")
     endif
     skip 1 alias VID
   enddo
   VID ->( baseRestPos())
   if !opened then vidClose()
   messageExit()
  endif
  select ( s)
return ACC ->PRIZ $ str

endif
return ACC ->PRIZ $ str


funcDef chkBikDest with g:O, ptr:N, BikDest:C ;
   local rV := .T., sBik := g:varGet
if empty( sBik)
   messageB( "Вы ОБЯЗАНЫ ввести БИК !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif len( sBik) != 9
   messageB( "Вы неполностью ввели БИК !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif left(sBik, 1) != '0'
   messageB( "Проверьте БИК. Первая цифра НЕ <0> !!!")
endif
if rV then BikDest = sBik
return rV

static funcDef chkKorSchet with g:O, sBikDest:C ;
   local rV := .T., sBik := g:varGet
if empty( sBik)
   messageB( "Вы ОБЯЗАНЫ ввести кор. счет !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif len(alltrim( sBik)) != 20
   messageB( "Вы неполностью ввели кор. счет !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif substr(sBik, 6, 3) != '810'
   messageB( "Вы неверно ввели код валюты - "+substr(sBik, 6, 3)+" !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif !chkKeyAcc(sBik, '0'+substr( sBikDest, 5, 2))
   messageB( "Проверьте кор. счет. Ключ вычислен неверно !!! ")
endif
return rV

static funcDef chkCredSchet with g:O, sBikDest:C, ptr:N ;
   local rV := .T., sBik := g:varGet
if empty( sBik)
   messageB( "Вы ОБЯЗАНЫ ввести счет !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif len(alltrim( sBik)) != 20
   messageB( "Вы неполностью ввели счет получателя !!!")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif substr(sBik, 6, 3) != '810'
   messageB( "Вы неверно ввели код валюты - "+substr(sBik, 6, 3)+" !!!")
   rV = (.F. .OR. g:exitState == GE_UP)

elseif ptr = 7 .And. Left(sBik, 3) !='423' .And. Left(sBik, 3) !='426' .And.;
                !isAccOrg(sBik)
   messageB( "Такие переводы внутри филиала делать нельзя !!! ")
   rV = (.F. .OR. g:exitState == GE_UP)
elseif ptr = 6 .And. !isAccOrg(sBik)
   messageB( "Такие внешние переводы делать нельзя !!! ")
   rV = (.F. .OR. g:exitState == GE_UP)

elseif !chkKeyAcc(sBik, right( sBikDest, 3)) //.and. ptr = 6
   messageB( "Проверьте счет получателя. Ключ вычислен неверно !!! ")

endif

return rV

// Add by TMV  27-06-2002
// Проверка - является этот счет счетом организации
***************************************
funcDef isAccOrg With sAccount:C ;
        local rV:=.T., old_area:=select()
basepath( orgInfoDbf(), confPathOD())
if file(orgInfoDbf()[1])
        if baseMOpen( orgInfoDbf())
                Select('ORGINFO')
                locate for allTrim(ORGINFO->SCHET) == sAccount
                rV = found()
                baseMClose( orgInfoDbf())
                dbselectarea(old_area)
        endif
endif
return rV
// Add by TMV  27-06-2002  --  End

***************************************
funcDef chkKeyAcc with sAccount:C, sKod:CU ;
    local rV, sUslSum:="71371371371371371371371", iTmp, iSum := 0,;
         sSchet
  if empty(sKod) then sKod = setUpMFO()
  sSchet:=sKod + left( sAccount, 8) + "0" + right( sAccount, 11)
  FOR iTmp:=1 TO 23
      iSum+=VAL(RIGHT(nSTR(VAL(SUBSTR(sSchet,iTmp,1))*VAL(SUBSTR(sUslSum,iTmp,1))),1))
  ENDFOR
  sSchet:=SUBSTR(sSchet,4,8)+RIGHT(nSTR(iSum*3),1)+RIGHT(sSchet,11)

  rV := (sSchet == sAccount)
return rV

funcDef seekBikDest with sBik:C, sBankDest:C, sKorSchet:C ;
        local rV := .T., abik := bikinfo()
basePath( abik, confPathOD())
if baseMOpen(abik)
        BNKBIK->(dbgotop())
        if BNKBIK->(dbseek(sBik))
                sBankDest = BNKBIK->Namep
                sKorSchet = BNKBIK->Ksnp
        else
                rV = .F.
        endif
        baseMClose(abik)
endif
return rV

funcDef wiscSumWrite With debSchet:C, summa:N, CurDate:D, typeOf:C,;
                curDest:C, sCredDest:C, sBikDest:C, KorSchet:C, theDest:C,;
                sInnDest:C, nDoc:N, sBankDest:C ;
                local rV := .T., s  := select(), opened := Select( "WISC") >0,;
                sBuf := ''

if !opened Then rV = wiscOpen()
if opened .Or. rV
        if WISC ->( mAppend())
                sBuf = 'DEBET:'+debSchet+'CREDIT:'+sCredDest+'BIC:'+sBikDest+;
                        'CCREDIT:'+KorSchet+'SUM:'+str(summa*100,20)+;
                        'DATE:'+dtos(CurDate)+'INN:'+padr(alltrim(sInnDest), 12)+;
                        'DEST:'+padR(alltrim(theDest), 50)+;
                        'DOCNO:'+padR(alltrim(str(nDoc)), 6)+;
                        'BANK_NAME:'+padr(alltrim(sBankDest),50)+;
                        'DETAILS:'+curDest
                WISC ->schet  := debSchet
                WISC ->type   := "820"
                WISC ->buffer := sBuf
                WISC ->( netUnLock())
   endif
endif
if !opened Then wiscClose()
Select(s)
return rV

// Add by TMV  18-05-2002
// Получение даты открытия счета
funcDef fSchOpenDate With sSchet:C local rV
   ACC ->( baseSavePos())
   ACC ->( dbSetOrder( 1))
   if ACC->(dbSeek( sSchet)) then rV = ACC ->datOpen
   ACC ->( baseRestPos())
return rV

// Add by TMV  22-11-2002
// Проверка сообщений по счету
funcDef isAcctDead with sSchet:C local arr := spr_infoSeek( sSchet )
return ( Len(arr) > 0 .And. (arr[2] = 2 .Or. at( 'УМЕР', Upper(arr[1])) > 0) )


// Add by TMV  23-04-2003
// список ссудных действующих счетов по нестраховой схеме
//
funcDef accNoStrahCred ;
  local curSchet := '', arrName := {}, arrCode := {}

ACC ->( baseSavePos())
ACC ->( dbSetOrder( 2))

SUMS ->( baseSavePos())
SUMS ->( dbSetOrder( 3))

vidArray( ACC_CRD_NOSTRAH, @arrName, @arrCode)

accSeek( CLIENTS ->code)
while CLIENTS ->code == ACC ->code .AND. ACC ->( !eof())

      if Left( ACC ->PRIZ, 7) = ACC_CRD_NOSTRAH .AND. ;
                 ACC ->typeOf == "00" .AND. ;
            chkCredOpen( arrCode, ACC ->schet)
         curSchet := ACC ->schet
      endif
      skip 1 alias ACC
enddo

SUMS ->( baseRestPos())
ACC  ->( baseRestPos())

return curSchet

// Add by TMV 28-10-2003
// Печать заявление на автоматически открытый счет
//
funcDef DogisPrint with sSchet :C local rV := .F.

if Left( ACC->PRIZ, 5) != "15 1 " .Or. accIsSigned() then return .T.

printOpen()
if noYes( "Заявление подписано ?")
        accSetSigned()
        rV = .T.
endif

return rV

// Add by TMV 29-07-2004
// Получение старого карточного счета
//
funcDef accCrdOldAcc With schet:C local rV := '', opened := (Select("ACC") != 0),;
        s := Select(), lOpen := .F.
if !opened then lOpen := accOpen()
if opened .OR. lOpen
        if ACC->(FieldSize(FieldPos('BUFFER'))) > 10
                ACC ->( baseSavePos())
                ACC ->( dbSetOrder( 1))
                if accSeek(schet) then rV = allTrim( subStr( ACC->BUFFER, 11, 20 ) )
                ACC ->( baseRestPos())
        endif
   if !opened then accClose()
endif
select ( s)
return rV

// Add by TMV 29-07-2004
// Признак печати уведомления о смене карточного счета
// Если возвращает непустое значение - это старый номер счета и
// надо печатать уведомление
//
funcDef accCrdPrnOld With schet:C, newVal:LU ;
        local rV := '', opened := (Select("ACC") != 0), s := Select(), ;
        lOpen := .F., sPrint := '', lSeek := .T., lPos := .T.

DEFAULT newVal TO .F.
if !opened then lOpen := accOpen()
if opened .OR. lOpen
        if Len(ACC->BUFFER) > 10
                if ACC->SCHET != schet
                        ACC ->( baseSavePos())
                        ACC ->( dbSetOrder( 1))
                        lSeek = accSeek(schet)
                        lPos = .F.
                endif
                if lSeek
                        sPrint = subStr( ACC->BUFFER, 31, 1 )
                        if sPrint == 'Y'
                                rV = allTrim( subStr( ACC->BUFFER, 11, 20 ) )
                                if newVal
                                if accRLock()
                                        ACC->BUFFER := subStr( ACC->BUFFER, 1, 30 ) + 'N' +;
                                                subStr( ACC->BUFFER, 32 )
                                                accUnLock()
                                endif
                                endif
                        else
                                rV = ''
                        endif
                endif
                if !lPos then ACC ->( baseRestPos())
        endif
   if !opened then accClose()
endif
select ( s)
return rV

