*=============================================================================
*
*                        Программа  : mon.exe
*                        Файл       : error.prg
*                        Автор      : Цейтлин П.М.
*
*                        Назначение : Замена стандартного errorsys.prg
*
*----------------------------------------------------------------------------
#include "clippExt.ch"
#include "common.ch"
#include "error.ch"
#include "set.ch"

//
// запуск модуля
//
funcDef ErrorSys()
   ErrorBlock( {|e| DefError( e)})
return NIL
//
// обработчик ошибок
//
static func DefError(e)
local i, cMessage, aOptions, nChoice := 0, aStack
local oldConsole, oldPrinter, oldDevice

//
// деление на 0
//
if e:genCode == EG_ZERODIV then return 0
//
// стандартная обработка сетевых ошибок
//
if e:genCode == EG_OPEN .AND. e:osCode == 32 .AND. e:canDefault
   netErr( .T.)
   return .F.
endif
//
// ошибки блокировок
//
if e:genCode == EG_APPENDLOCK .and. e:canDefault
   NetErr( .T.)
   return  .F.									// NOTE
endif
//
// ловим ошибку принтера
//
if upper( e:subSystem) == "TERM" .AND. e:subCode == 0
   oldConsole := Set( _SET_CONSOLE)
   oldPrinter := Set( _SET_PRINTER)
   oldDevice  := Set( _SET_DEVICE)

   set console on
   set printer off
   set device to screen

   if alarm( "Ошибка печати !",;
       { "Принтер подготовлен, продолжить", "Закончить" }) != 2 
      // set console off
      // set printer on
      // set device to printer
      Set( _SET_CONSOLE, oldConsole)
      Set( _SET_PRINTER, oldPrinter)
      Set( _SET_DEVICE,  oldDevice)
      return .T.
   endif

   close all
   setColor( "w/n")
   cls
   removeClock()
   quit
endif

//
// вывод сообщения об ошибке
//
cMessage := ErrorMessage(e)
aOptions := {"Прервать"}
if e:canRetry   then aAdd( aOptions, "Повторить")
if e:canDefault then aAdd( aOptions, "По умолчанию")
aAdd( aOptions, { "Стек вызовов", {|| asrtViewStack( aErrorStack()) }})
//
// что делать ?
//
oldConsole := Set( _SET_CONSOLE)
oldPrinter := Set( _SET_PRINTER)
oldDevice  := Set( _SET_DEVICE)

set console on
set printer off
set device to screen

while nChoice == 0
      nChoice := dialog( cMessage, aOptions)
      if nChoice == len( aOptions) then nChoice := 0
enddo

Set( _SET_DEVICE,  oldDevice)
Set( _SET_CONSOLE, oldConsole)
Set( _SET_PRINTER, oldPrinter)

eFileWrite( { "Программа    : MON.exe"            ,;
              "Отделение    : " + "" /*confOtdName()*/   ,;
              "Ошибка       : " + cMessage, ""    ,;
              "Стек вызовов : " })
eFileWrite( aErrorStack())
eFileWrite( { "", "", "", "", "" })

if !empty( nChoice)
   if allTrim( aOptions[ nChoice]) == "Прервать"     then Break( e)
   if allTrim( aOptions[ nChoice]) == "Повторить"    then return .T.
   if allTrim( aOptions[ nChoice]) == "По умолчанию" then return .F.
endif

return .f.

//
// стек ошибок
//
static funcDef aErrorStack local aErr := {}, i := 3

while !empty( procName( i))
      aAdd( aErr, Trim(ProcName( i)) + "(" + nStr( procLine( i)) + ")" )
      i++
enddo

return aErr

//
// храбро позаимствуем эту функцию
//
static func ErrorMessage(e)
local cMessage

cMessage := if( e:severity > ES_WARNING, "Error ", "Warning " )

cMessage += if( valType( e:subSystem) == "C", e:subsystem(), "???")
cMessage += if( valType( e:subCode)   == "N", "/" + nStr( e:subCode), "/???")

if valType( e:description) == "C" then cMessage += ("  " + e:description)
if !empty(  e:filename)
   cMessage += (": " + e:filename)
elseif !empty( e:operation)
   cMessage += (": " + e:operation)
endif
if !empty( e:osCode) then cMessage += " (DOS Error " + nStr( e:osCode) + ") "


return (cMessage)

static procDef asrtViewStack with calls:A ;
   local view := "", y1, x1, dy, dx, y2, x2, i
   //
   // Определение координат окна
   //
   y1 := 0
   x1 := 0
   dy := textHeight( calls)
   dx := textWidth(  calls)
   y2 := y1 + dy + 1
   x2 := x1 + dx + 1 + 2
   //
   // Подготовка memo-строки
   //
   for i=1 to len( calls)
      view += calls[ i] + chr( 13) + chr( 10)
   endFor
   //
   // Вывод стека вызовов
   //
   windowStartUp( @y1, @x1, @y2, @x2, setPopColors(), 0)
   memoEdit( view, y1+1, x1+2, y2-1, x2-2, .F.,, dx, 8)
   windowExit()
return

