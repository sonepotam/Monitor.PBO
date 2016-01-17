*============================================================================
*
*                  Программа : MON.EXE
*                  Файл      : basevidu.prg
*                  Автор     : Цейтлин П.М.
*
*                  Назначение: справочник типов счетов
*
*----------------------------------------------------------------------------
#include "common.ch"
#include "clippExt.ch"

#include "mon.ch"

func vidInfo
static dbInfo := { ".\VID.DBF", "VID",;
                 {{ "priz",     "C", 12, 0},;
                  { "name",     "C", 60, 0},;
                  { "srok",     "N",  3, 0},;
                  { "prc",      "N",  7, 2},;
                  { "type",     "N",  1, 0},;
                  { "summaMin", "N", 15, 0},;
                  { "summaMax", "N", 15, 0},;
                  { "live",     "L",  1, 0},;
                  { "balSchet", "C",  5, 0},;
                  { "addMin",   "N", 15, 0},;
                  { "addMax",   "N", 15, 0},;
                  { "buffer",   "C", 10, 0}},;
                 {{, "priz"},;
                  {, "subStr( buffer, 3, 1)"}}, 0}
return dbInfo

funcDef vidPath with newPath:CU
return basePath( vidInfo(), newPath)

funcDef vidOpen
return baseMOpen( vidInfo())

funcDef vidXOpen
return baseMXOpen( vidInfo())

funcDef vidClose
return baseMClose( vidInfo())

funcDef vidCreate
return baseCreate( vidInfo())

funcDef vidSeek with schet:C, softSeek:LU
return vid ->( netSeek( schet, softSeek))

funcDef vidArray with ch:CA, arr1:AU, arr2:AU, useStavka:LU, useLive:LU ;
   local s, rV, opened, len, tmpStr, b

DEFAULT useStavka TO .T.
DEFAULT useLive   TO .T.

arr1 := {}
arr2 := {}
len  := len( ch)
s      := select()
opened := select( "VID") >0
rV     := .F.

if !opened then rV := vidOpen()

if opened .OR. rV
   VID ->( baseSavePos())
   VID ->( dbSetOrder( 2))
   VID ->( netGoTop())
   if valType( ch) == "C" then ch := { ch}
   while .T.

         if if( useLive, VID ->live, .T.) .AND. ;
            aScan( ch, {|x| left( VID ->PRIZ, len( x)) == x }) > 0
//            tmpStr := rTrim( VID ->name)
            tmpStr :=  padr( VID ->name, 37, space(1))
            if useStavka then tmpStr += " Ставка " + Str( VID ->prc, 7, 2)

            aAdd( arr1, tmpStr)
            aAdd( arr2, { VID ->( recNo()), VID ->summaMin, VID ->summaMax,;
                          VID ->PRIZ,       VID ->balSchet, VID ->addMin,;
                          VID ->addMax,     VID ->buffer })
         endif
         exit if VID ->( !netSkipDown())
   enddo
   VID ->( baseRestPos())
   if !opened then vidClose()
endif

select ( s)
return NIL

funcDef vidGetPrc with priz:C local s, rV, opened, pcts := 0.00

s      := select()
opened := select( "VID") >0
rV     := .F.

if !opened then rV := vidOpen()
if opened .OR. rV

   if VID ->( netSeek( priz)) then pcts := VID ->prc
   if !opened then vidClose()
endif

select ( s)

return pcts

funcDef vidGetSrok with priz:C, type:N ;
  local s, rV, opened, srok := 0
  s      := select()
  opened := select( "VID") >0
  rV     := .F.
  if !opened then rV := vidOpen()
  if opened .OR. rV
    if VID ->( netSeek( priz)) 
       srok := VID ->srok
       type := VID ->type
    endif
    if !opened then vidClose()
  endif
  select ( s)
return srok

funcDef vidGetName with priz:C local s, rV, opened, pcts := ""

s      := select()
opened := select( "VID") >0
rV     := .F.

if !opened then rV := vidOpen()
if opened .OR. rV

   if VID ->( netSeek( priz)) then pcts := VID ->name
   if !opened then vidClose()
endif

select ( s)

return pcts

funcDef vidGetPrintType with priz:C ;
  local s     := select(), rV := .F., opened := select( "VID") >0,;
        value := ""

  if !opened then rV := vidOpen()
  if opened .OR. rV
   VID ->( baseSavePos())
   if VID ->( netSeek( priz)) then value := subStr( VID ->buffer, 5, 1)
   VID ->( baseRestPos())
   if !opened then vidClose()
  endif
  select ( s)
return value


funcDef vidGetDatClose with dat_Open:D, priz:C local s, rV, opened, dat_Close

s      := select()
opened := select( "VID") >0
rV     := .F.

dat_Close := CtoD( "")
if !opened then rV := vidOpen()
if opened .OR. rV

   if VID ->( netSeek( priz)) .AND. VID ->srok >0
      //
      // в месяцах
      if VID ->type == 1
         dat_Close := addMonth( dat_Open, VID ->srok)
      else
         dat_Close := dat_Open + VID ->srok
      endif

   endif
   if !opened then vidClose()
endif

select ( s)

return dat_Close

funcDef vidGetAdd with priz:C, addMin:NU, addMax:NU local rV, opened, s

s      := select()
opened := select( "VID") >0
rV     := .F.
addMin := addMax := 0

if !opened then rV := vidOpen()
if opened .OR. rV
   if VID ->( netSeek( priz))
      addMin := VID ->addMin
      addMax := VID ->addMax
   endif
   if !opened then vidClose()
endif
select ( s)

return NIL