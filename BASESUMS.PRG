*============================================================================
*     
*                  Программа : MON.EXE
*                  Файл      : basesums.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: Описание файла проводок
*
*----------------------------------------------------------------------------
* kasTypes  TMV  13-08-2002
* для Питера исключена из реестров операция 5010
*
* 25-11-2002 TMV
* Из расчета остатка по счету убрана операция выдачи кредита - sumRest()
*
* 28-11-2002 TMV
* В расчете остатка операция 1001 заменена на новую - 1060 (7032) - sumRest()
*
* 02-12-2002 TMV
* Из расчета остатка по счету убрана операция возврата кредита 2004 - sumRest()
*
* 05-12-2002 TMV
* Из расчета остатка по счету убрана операция возврата кредита 2060 (7033) - sumRest()
*
* 09-12-2002 TMV
* Из типы операций для рееестров исключены 6136, 6137 и 6031.
*  Ф-ия kasTypes()
* Исправлен расчет остатка, ф-ия sumRest()
*
* 15-12-2002 TMV
* Поправлен расчет остатка в sumRest().
*
* 17-12-2002 TMV
* Поправлен расчет остатка в sumRest()
*--ОТМЕНЕНО
*
* 18-12-2002 TMV
* Поправлена ф-ия kasTypes()
*
* 19-12-2002 TMV
* Поправлена ф-ия kasTypes()
*
* 06-08-2003 TMV
* Поправлена ф-ия kasTypes() - вставлена операция 8700
*
* 05-02-05 TAT
* sumRest() - вствлен анализ на просрочку процентов, по которой остаток пересчитывать

#include "mon.ch"
#include "common.ch"
#include "clippExt.ch"
#include "gsz.ch"

* NEW OBJECT FILE
  
func sumInfo
static dbInfo := { ".\SUMS.DBF", "SUMS",;
                  {{ "schet",    "C", 20, 0} ,;
                   { "summa",    "N", 15, 0} ,;
                   { "dDate",    "D",  8, 0} ,;
                   { "printDate","D",  8, 0} ,;
                   { "tTime",    "C",  8, 0} ,;
                   { "typeOf",   "C",  4, 0} ,;
                   { "nDoc",     "N",  4, 0} ,;
                   { "owner",    "C",  8, 0} ,;
                   { "distrCode","N",  2, 0} ,;
                   { "buffer",   "C", 30, 0}},;
                  {{, "schet+DtoS( dDate)+Str(summa,15)"} ,;
                   {, "DtoS( dDate) + schet"          } ,;
                   {, "typeOf + DtoS( dDate) + schet" } ,;
                   {, "Str( summa,15,0)"              }},;
                  0}


//                  {{, "schet+DtoS( dDate)+Str(summa,15,0)"} ,;
return dbInfo

funcDef sumPath with newPath:CU
return basePath( sumInfo(), newPath)

funcDef sumOpen
return baseMOpen( sumInfo())

funcDef sumXOpen
return baseMXOpen( sumInfo())

funcDef sumClose
return baseMClose( sumInfo())

funcDef sumCreate
return baseCreate( sumInfo())

autofunction setValue sumUseAppend init .F.

funcDef sumAppend local rV := .F.
if sumUseAppend()
   SUMS ->( dbAppend())
   if !netErr()
      rV := .T.
   else
      rV := SUMS ->( mAppend())
   endif
else
  rV := SUMS ->( mAppend())
endif
return rV



funcDef sumRLock
return SUMS ->( mRLock())

funcDef sumUnLock
return SUMS ->( netUnLock())

* NEW OBJECT FILE
funcDef sumRest with Schet:C, dDate:D, exclude900:LU local rV := 0.00

DEFAULT exclude900 TO .F.

ACC  ->( baseSavePos())
SUMS ->( baseSavePos())
SUMS ->( dbSetOrder( 1))
SUMS ->( netSeek( schet))

exclude900 := exclude900 .AND. !histView()
while schet == SUMS ->schet .AND. ;
      dDate >= SUMS ->dDate .AND. ;
      SUMS ->( !eof())
      if SUMS ->typeOf <> ORD_904_ESC .AND. SUMS ->typeOf <> ORD_33_ESC
//  Edited by TMV  4-02-2002 для вклада "Юбилейный"
         if (exclude900 .AND. left( SUMS ->typeOf, 3) == "900") .Or.;
					(isUbileyAcc() .And. !isUbileyClose() .And. !exclude900 .AND. SUMS ->typeOf == "9100" ;
					.And. !isUbiPrcClose(Schet, SUMS->Summa) ) .Or.;
					(isUbileyAcc() .And. isUbileyClose() .And. exclude900 .AND. SUMS ->typeOf == "9100" ;
					.And. !isUbiPrcClose(Schet, SUMS->Summa));
					.Or. opEqual( SUMS ->typeOf, '1060');   // Add by TMV  15-12-2002
					.Or. opEqual( SUMS ->typeOf, '6136');   // Add by TMV  15-12-2002
					.Or. (opEqual( SUMS ->typeOf, '2060');  // Add by TMV  15-12-2002
					.And. Left(SUMS->Buffer, 4) == 'PCT:');
					.And. (.not. _isProsrPct(SUMS->schet)); // 05-02-2005 TAT
					.Or. opEqual( SUMS ->typeOf, '6137')   // Add by TMV  15-12-2002
					// Replaced 1001 to 1060 (7032) by TMV  28-11-2002

// Remove by TMV  17-12-2002
//					.And. Left(SUMS->Buffer, 4) == 'PCT:');
//					.Or. opEqual( SUMS ->typeOf, '6136');   // Add by TMV  15-12-2002

// Remove by TMV  09-12-2002
//					.Or. opEqual( SUMS ->typeOf, '2004')  // Add by TMV  02-12-2002
//					.Or. opEqual( SUMS ->typeOf, '1060'); // Add by TMV  25-11-2002
//					.Or. opEqual( SUMS ->typeOf, '2060');  // Add by TMV  05-12-2002
//					.Or. opEqual( SUMS ->typeOf, '6031');   // Add by TMV  15-12-2002
//					.Or. opEqual( SUMS ->typeOf, '6137');   // Add by TMV  15-12-2002
//					.Or. opEqual( SUMS ->typeOf, '6136');   // Add by TMV  15-12-2002
//					.Or. opEqual( SUMS ->typeOf, '1060')    // Add by TMV  15-12-2002
         else
            rV += SUMS ->summa
         endif
      endif
      skip 1 alias SUMS
enddo

rV := rV/ 100 + accSumma( schet, dDate)
SUMS ->( baseRestPos())
ACC  ->( baseRestPos())

return round( rV, 2)

* NEW OBJECT FILE
funcDef sumWrite with schet:C, summa:N, dDate:D, typeOf:C,;
                      nDoc:N, distrCode:NU, buffer:CU local rV

if summa == 0
   errLog( "Вы не можете сделать проводку на нулевую сумму")
   return .F.
endif

// if ( rV := sumAppend())
   if kasTypes(typeOf)
      summa := kasWriteSumma( summa)
      if opEqual( typeOf, "1007") .AND. summa < 0
         if rsKasHas()
            kasWriteSumma( summa, "003")
         else
            summa := kasWriteSumma( summa, "003")
         endif
      endif
   endif
   if summa == 0
      errLog( "Вы не можете сделать проводку на нулевую сумму")
      return .F.
   endif
if ( rV := sumAppend())

   SUMS ->schet      := schet
   SUMS ->summa      := summa * 100
   SUMS ->dDate      := dDate
   SUMS ->printDate  := dDate
   SUMS ->typeOf     := typeOf
   SUMS ->nDoc       := nDoc
   SUMS ->tTime      := time()
   SUMS ->owner      := userName() // nNetWhoAmI()
   if !isNIL( distrCode) then SUMS ->distrCode := distrCode
   if !isNIL( buffer   ) then SUMS ->buffer    := buffer
   sumWriteProt()

   sumUnLock()
endif

return rV

funcDef kasTypes with typeof:C
if typeOf <= "9000" .AND. ;
   !opEqual( typeOf, "1005") .AND. ;
   !opEqual( typeOf, "1008") .AND. ;
   !opEqual( typeOf, "6030") .AND. ;
   !opEqual( typeOf, "7100") .AND. ;  // Add by TMV 08-10-2001 Replaced 6140 to 7100 by TMV 21-11-2001
   !opEqual( typeOf, "5011") .AND. ;
   !opEqual( typeOf, "5200") .AND. ;
   !opEqual( typeOf, "5201") .AND. ;
   !opEqual( typeOf, "5301") .AND. ;
   !opEqual( typeOf, "5302") .AND. ;
   !opEqual( typeOf, "5511") .AND. ;
   !opEqual( typeOf, "3015") .AND. ;
   !opEqual( typeOf, "5002") .AND. ;  //  Add by TMV  25-09-2001
   !opEqual( typeOf, "8700") .AND. ;  //  Add by TMV  06-08-2003
   !opEqual( typeOf, GSZ_BUY_BEZNAL)
  return .T.
endif
return .F.

funcDef sumPWrite with schet:C, summa:N, dDate:D, typeOf:C, nDoc:NU local rV

if ( rV := SUMP ->( mAppend()))

   SUMP ->schet      := schet
   SUMP ->summa      := summa * 100
   SUMP ->dDate      := dDate
   SUMP ->typeOf     := typeOf

	if !isNil(nDoc) then SUMP->Crd_Num := nDoc  // Add by TMV  29-04-2002

   SUMP ->( netUnLock())
endif

return rV

* NEW OBJECT FILE

func sumPInfo
static dbInfo := { ".\SUMP.DBF", "SUMP",;
                  {{ "schet",    "C", 20} ,;
                   { "summa",    "N", 15} ,;
                   { "dDate",    "D",  8} ,;
                   { "typeOf",   "C",  4}},;
                  {{, "schet+DtoS( dDate)+Str(summa,15,0)"} ,;
                   {, "DtoS( dDate) + schet"          }},;
                  0}

// Add by TMV  29-04-2003
	if useLineAllNoStrah() .And. aScan( dbInfo[3], {|x| x[1] == "CRD_NUM"} ) == 0
		aAdd( dbInfo[3], { "CRD_NUM",  "N",  6, 0})
	endif
// Add by TMV  29-04-2003 -- End
return dbInfo

funcDef sumPPath with newPath:CU
return basePath( sumInfo(), newPath)

funcDef sumPOpen
return baseMOpen( sumPInfo())

funcDef sumPXOpen
return baseMXOpen( sumPInfo())

funcDef sumPClose
return baseMClose( sumPInfo())

funcDef sumPCreate
return baseCreate( sumPInfo())

funcDef sumPAppend
return sumP ->( mAppend())

funcDef sumPRLock
return sumP ->( mRLock())

funcDef sumPUnLock
return sumP ->( netUnLock())

funcDef sumPMinPens with schet:C
return sumPType( schet, opDecode( "1007"))


funcDef sumPType with schet:C, typeOf:C local rV := 0.00
 
SUMP ->( baseSavePos())
SUMP ->( dbSetOrder( 1))
SUMP ->( dbSeek( schet, .T.))

while SUMP ->schet == schet .AND. SUMP ->( !Eof())

      if SUMP ->typeOf == typeOf
         rV += SUMP ->summa/ 100
      endif
      skip 1 alias SUMP
enddo
SUMP ->( baseRestPos())

return rV

funcDef sumSType with schet:C, typeOf:C local rV := 0.00

SUMS ->( baseSavePos())
SUMS ->( dbSetOrder( 1))
SUMS ->( dbSeek( schet, .T.))

while SUMS ->schet == schet .AND. SUMS ->( !Eof())

      if SUMS ->typeOf == typeOf
         rV += SUMS ->summa/ 100
      endif
      skip 1 alias SUMS

enddo
SUMS ->( baseRestPos())

return rV


* NEW OBJECT FILE

func kasInfo
static dbInfo := { ".\kassa.dbf", "KASSA"  ,;
                  {{ "typeOf", "C",  4, 0} ,;
                   { "summa",  "N", 15, 0} ,;
                   { "obD",    "N", 15, 0} ,;
                   { "obK",    "N", 15, 0}},;
                  {{, "typeOf" }}, 0}
return dbInfo

funcDef kasPath with newPath:CU
return basePath( kasInfo(), newPath)

funcDef kasOpen
return baseNSOpen( kasInfo())

funcDef kasXOpen
return baseNSXOpen( kasInfo())

funcDef kasClose
return baseMClose( kasInfo())


//
// коррекция сальдо по кассе ( сумма в рублях)
//
funcDef kasWriteSumma with  summa:N, kasCode:CU ;
   local opened, rV := .T., s:= select(), rsKasHas


DEFAULT kasCode TO "999"
if summa == 0 then return 0

//
// забьем на остаток
//
if kasCode == "999" .AND. confUseAgent() then return summa // Remove By TMV 05-10-2001


opened := select( "KASSA") >0
if !opened then rV := baseNSOpen( kasInfo())

if opened .OR. rV

   rsKasHas := kasCode == "003" .AND. rsKasHas()
   if KASSA ->( dbSeek( kasCode))
      if KASSA ->summa >=0 .OR. summa >0 .OR. rsKasHas
         if KASSA ->( mRLock())
            if ( KASSA ->summa + summa * 100) >=0 .OR. rsKasHas .OR. anyCashRest()
               KASSA ->summa := KASSA ->summa + summa * 100
            else
               errLog( { "Остаток по кассе   " + mStr( KASSA ->summa/ 100),;
                        "Вы пытаетесь снять " + mStr( abs( summa))       ,;
                        "", "Подкрепите кассу !" })
               // summa := KASSA ->summa/ 100
               summa := 0.00
               // KASSA ->summa := 0.00
            endif
            if summa >0
               KASSA ->obK := KASSA ->obK + summa * 100
            else
               KASSA ->obD := KASSA ->obD - summa * 100
            endif
            KASSA ->( netUnLock())
         endif
      else
         summa := 0
         if kasCode == "999"
            errLog( { "Остаток по кассе " + mStr( KASSA ->summa/ 100),;
                     "", "Необходимо сделать подкрепление кассы"})
         endif
         if kasCode == "003"
            errLog( { "Лимит отделения " + mStr( KASSA ->summa/ 100),;
                     "", "Проводка не сделана"})
         endif
      endif
   endif
   if !opened then baseMClose( kasInfo())
   select ( s)
endif

return summa

funcDef kasRest with kasCode:CU, isFound:LU local rV := 0.00, s := select()

DEFAULT kasCode TO "999"
if kasOpen()
   if KASSA ->( isFound := dbSeek( kasCode)) then rV := KASSA ->summa/ 100
   kasClose()
   select ( s)
endif
//
// в случае агентских отношений, счет кассы является пассивно-активным
// и проверке не подлежит
//
if confUseAgent()
  if kasCode == "999" then rV := 999999999.99
else
  if subStr( confKassa(), 1, 5 ) == "20202" then rV := 999999999.99
endIf
return rV

funcDef anyCashRest
return  !confUseAgent() .and. ( subStr( confKassa(), 1, 5 ) == "20202" )

funcDef kasSetRest with kasCode:C, newSum:N local s := select()
if kasOpen()
  if KASSA ->( dbSeek( kasCode)) 
    if KASSA ->( mRLock())
      KASSA ->summa := newSum * 100
      KASSA ->( netUnLock())
    endif
  endif
  kasClose()
  select ( s)
endif
return NIL


funcDef kasLimitSumma with  summa:N, kasCode:C ;
   local opened, rV := .T., s:= select()

kasCode := padR( kasCode, 4)
if summa == 0 then return 0
opened := select( "KASSA") >0
if !opened then rV := baseNSOpen( kasInfo())
if opened .OR. rV
   if KASSA ->( dbSeek( kasCode))
      if KASSA ->summa >=0 .OR. summa >0
         if KASSA ->( mRLock())
            if ( KASSA ->summa + summa) >=0
               KASSA ->summa := KASSA ->summa + summa
            else
               errLog( { "Лимит " + nStr( KASSA ->summa),;
                        "Вы пытаетесь продать " + nStr( abs( summa))       ,;
                        "", "Лимита недостаточно!" })
               summa := 0
            endif
            if summa >0
               KASSA ->obK := KASSA ->obK + summa
            else
               KASSA ->obD := KASSA ->obD - summa
            endif
            KASSA ->( netUnLock())
         endif
      else
         summa := 0
         errLog( { "Лимит отделения " + nStr( KASSA ->summa)})
      endif
   endif
   if !opened then baseMClose( kasInfo())
   select ( s)
endif
return summa

funcDef kasLimit with kasCode:C, isFound:LU local rV := 0.00, s := select()
kasCode := padR( kasCode, 4)
if kasOpen()
   if KASSA ->( isFound := dbSeek( kasCode)) then rV := KASSA ->summa
   kasClose()
   select ( s)
endif
return rV


funcDef sumWriteProt local str := "", fileName := ".\prot.txt", i := 0

while !empty( procName( i))
      str := str + procName( i) + "(" + nStr( procLine( i++)) + "); "
enddo
str := "Дата : " + DtoC( setCurDate()) + "," + time() + ;
       " Счет : " + SUMS ->schet + " Сумма : " + mStr( SUMS ->summa/ 100) + ;
       " Стек : " + str
eFileWrite( str, fileName)

return NIL

// Add by TMV  4-02-2002
// а не "Юбилейный" ли это вклад ?
funcdef isUbileyAcc
return if( allTrim(ACC->PRIZ) == '15 2  49 0', .T., .F. )

// Add by TMV  4-02-2002
// а этот вклад закрыт или нет ?
funcdef isUbileyClose
return if( ACC->DATCLOSE == setCurDate(), .T., .F. )

// Add by TMV  22-04-2002
// а не проценты ли это при закрытии вклада ?
funcDef isUbiPrcClose With sAcc:C, nSumma:N local rV := .F., nSelect := SUMP->(dbsetorder())
SUMP->(dbsetorder(1))
rV = (SUMP->(dbseek(sAcc + dtos(setCurDate()) + str( nSumma, 15))) .And.;
		SUMP->TypeOf == '4002')
SUMP->(dbsetorder(nSelect))
return rV

// Add by TMV  20-05-2003
// Остаток по операции по номеру транша
//
funcDef sumPTypeTransh with schet:C, typeOf:C, transh:N local rV := 0.00
 
SUMP ->( baseSavePos())
SUMP ->( dbSetOrder( 1))
SUMP ->( dbSeek( schet, .T.))

while SUMP ->schet == schet .AND. SUMP ->( !Eof())

      if SUMP ->typeOf == typeOf .And. SUMP ->CRD_NUM == transh
         rV += SUMP ->summa/ 100
      endif
      skip 1 alias SUMP
enddo
SUMP ->( baseRestPos())

return rV