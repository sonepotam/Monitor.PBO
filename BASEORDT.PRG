*==========================================================================
*
*              Программа  : MON.EXE
*              Файл       : baseOrdT.prg
*              Автор      : Цейтлин П.М.
*
*              Назначение : Операции с ord_temp
*
*--------------------------------------------------------------------------
#include "clippExt.ch"
#include "common.ch"

#include "mon.ch"


func ordTempInfo
static dbInfo := { ".\ord_temp.dbf", "ORD_TEMP",;
                  {{ "date",      "D", 8, 0} ,;
                   { "In_num",    "N", 4, 0} ,;
                   { "Out_num",   "N", 4, 0} ,;
                   { "mem_num",   "N", 4, 0} ,;
                   { "SvMem_num", "N", 4, 0}},;
                  {{, "DtoS( date)"}}, 0}
return dbInfo

funcDef ordtempPath with newPath:CU
return basePath( ordTempInfo(), newPath)

funcDef ordTempOpen
return baseMOpen( ordTempInfo())

funcDef ordTempClose
return baseMClose( ordTempInfo())

funcDef ordIncIn with dDate:D local s := select(), opened, rV := .T., value

opened := select( "ORD_TEMP") >0
if !opened then rV := ordTempOpen()

if rV .OR. opened
if ORD_TEMP ->( !netSeek( DtoS( dDate))) 
   if ( rV := ORD_TEMP ->( mAppend()))
      ORD_TEMP ->date := dDate
   endif
else
   rV := ORD_TEMP ->( mRLock())
endif

if rV
   ORD_TEMP ->in_num := ORD_TEMP ->in_num+ 1
   ORD_TEMP ->( netUnLock())
endif 
value := ORD_TEMP ->in_num

endif

if !opened then ordTempClose()
select ( s)
return value


funcDef ordIn with dDate:DU local s := select(), opened, rV := .T., value  := 0

DEFAULT dDate TO setCurDate()
opened := select( "ORD_TEMP") >0
if !opened then rV := ordTempOpen()

if rV .OR. opened
   if ORD_TEMP ->( netSeek( DtoS( dDate))) 
      value := ORD_TEMP ->in_num
   endif 
endif

if !opened then ordTempClose()
select ( s)
return value

funcDef ordIncOut with dDate:D local s := select(), opened, rV := .T., value

opened := select( "ORD_TEMP") >0
if !opened then rV := ordTempOpen()

if rV .OR. opened

if ORD_TEMP ->( !netSeek( DtoS( dDate))) 
   if ( rV := ORD_TEMP ->( mAppend()))
      ORD_TEMP ->date := dDate
   endif
else
   rV := ORD_TEMP ->( mRLock())
endif

if rV
   ORD_TEMP ->out_num := ORD_TEMP ->out_num+ 1
   ORD_TEMP ->( netUnLock())
endif
value := ORD_TEMP ->out_num
endif

if !opened then ordTempClose()
select ( s)
return value


funcDef ordOut with dDate:DU local s := select(), opened, rV := .T., value := 0

DEFAULT dDate TO setCurDate()

opened := select( "ORD_TEMP") >0
if !opened then rV := ordTempOpen()

if rV .OR. opened
   if ORD_TEMP ->( netSeek( DtoS( dDate))) 
      value := ORD_TEMP ->out_num 
   endif 
endif

if !opened then ordTempClose()
select ( s)
return value


funcDef ordIncMem with dDate:D local s := select(), opened, rV := .T., value

opened := select( "ORD_TEMP") >0
if !opened then rV := ordTempOpen()

if rV .OR. opened

if ORD_TEMP ->( !netSeek( DtoS( dDate))) 
   if ( rV := ORD_TEMP ->( mAppend()))
      ORD_TEMP ->date := dDate
   endif
else
   rV := ORD_TEMP ->( mRLock())
endif

if rV
   ORD_TEMP ->mem_num := ORD_TEMP ->mem_num+ 1
   ORD_TEMP ->( netUnLock())
endif 
value := ORD_TEMP ->mem_num
endif

if !opened then ordTempClose()
select ( s)
return value

funcDef ordIncSvMem with dDate:D local s := select(), opened, rV := .T., value

opened := select( "ORD_TEMP") >0
if !opened then rV := ordTempOpen()

if rV .OR. opened

if ORD_TEMP ->( !netSeek( DtoS( dDate))) 
   if ( rV := ORD_TEMP ->( mAppend()))
      ORD_TEMP ->date := dDate
   endif
else
   rV := ORD_TEMP ->( mRLock())
endif

if rV
   ORD_TEMP ->SVmem_num := ORD_TEMP ->SVmem_num+ 1
   ORD_TEMP ->( netUnLock())
endif 
value := ORD_TEMP ->SVmem_num
endif

if !opened then ordTempClose()
select ( s)

return value

