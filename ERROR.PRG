*=============================================================================
*
*                        �ணࠬ��  : mon.exe
*                        ����       : error.prg
*                        ����      : ���⫨� �.�.
*
*                        �����祭�� : ������ �⠭���⭮�� errorsys.prg
*
*----------------------------------------------------------------------------
#include "clippExt.ch"
#include "common.ch"
#include "error.ch"
#include "set.ch"

//
// ����� �����
//
funcDef ErrorSys()
   ErrorBlock( {|e| DefError( e)})
return NIL
//
// ��ࠡ��稪 �訡��
//
static func DefError(e)
local i, cMessage, aOptions, nChoice := 0, aStack
local oldConsole, oldPrinter, oldDevice

//
// ������� �� 0
//
if e:genCode == EG_ZERODIV then return 0
//
// �⠭���⭠� ��ࠡ�⪠ �⥢�� �訡��
//
if e:genCode == EG_OPEN .AND. e:osCode == 32 .AND. e:canDefault
   netErr( .T.)
   return .F.
endif
//
// �訡�� �����஢��
//
if e:genCode == EG_APPENDLOCK .and. e:canDefault
   NetErr( .T.)
   return  .F.									// NOTE
endif
//
// ����� �訡�� �ਭ��
//
if upper( e:subSystem) == "TERM" .AND. e:subCode == 0
   oldConsole := Set( _SET_CONSOLE)
   oldPrinter := Set( _SET_PRINTER)
   oldDevice  := Set( _SET_DEVICE)

   set console on
   set printer off
   set device to screen

   if alarm( "�訡�� ���� !",;
       { "�ਭ�� �����⮢���, �த������", "��������" }) != 2 
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
// �뢮� ᮮ�饭�� �� �訡��
//
cMessage := ErrorMessage(e)
aOptions := {"��ࢠ��"}
if e:canRetry   then aAdd( aOptions, "�������")
if e:canDefault then aAdd( aOptions, "�� 㬮�砭��")
aAdd( aOptions, { "�⥪ �맮���", {|| asrtViewStack( aErrorStack()) }})
//
// �� ������ ?
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

eFileWrite( { "�ணࠬ��    : MON.exe"            ,;
              "�⤥�����    : " + "" /*confOtdName()*/   ,;
              "�訡��       : " + cMessage, ""    ,;
              "�⥪ �맮��� : " })
eFileWrite( aErrorStack())
eFileWrite( { "", "", "", "", "" })

if !empty( nChoice)
   if allTrim( aOptions[ nChoice]) == "��ࢠ��"     then Break( e)
   if allTrim( aOptions[ nChoice]) == "�������"    then return .T.
   if allTrim( aOptions[ nChoice]) == "�� 㬮�砭��" then return .F.
endif

return .f.

//
// �⥪ �訡��
//
static funcDef aErrorStack local aErr := {}, i := 3

while !empty( procName( i))
      aAdd( aErr, Trim(ProcName( i)) + "(" + nStr( procLine( i)) + ")" )
      i++
enddo

return aErr

//
// �ࠡ� ��������㥬 ��� �㭪��
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
   // ��।������ ���न��� ����
   //
   y1 := 0
   x1 := 0
   dy := textHeight( calls)
   dx := textWidth(  calls)
   y2 := y1 + dy + 1
   x2 := x1 + dx + 1 + 2
   //
   // �����⮢�� memo-��ப�
   //
   for i=1 to len( calls)
      view += calls[ i] + chr( 13) + chr( 10)
   endFor
   //
   // �뢮� �⥪� �맮���
   //
   windowStartUp( @y1, @x1, @y2, @x2, setPopColors(), 0)
   memoEdit( view, y1+1, x1+2, y2-1, x2-2, .F.,, dx, 8)
   windowExit()
return

