#include "fileio.ch"
/*
   Этот файл содержит тестовую версию генератора документов.
   В качестве параметров используются файл шаблонов и название
   документа без скобок.

   В начале документа могут быть идетификаторы вида
   #docName := allTrim( CLIENTS ->name)

   Эти идентификаторы добавляются в массив идентификаторов, а значения
   вычисляются с помощью eval
   

*/

function test()
local str, sec, i
 sec := seconds()
 str := ReadDocument( ".\printz.tpl", "NAK")
 str := CtoA( str, chr( 13) + chr( 10))
 

 memoWrit( "res.txt", str)
 sec := seconds()
 for i := 1 to 2000
  use printz new
  locate for upper( allTrim( PRINTZ ->name)) == upper( allTrim( "nak"))
  str := PRINTZ ->data
  close printz
 next
 sec := seconds() -sec
 ? "Время выполнения " + Str( sec, 15)

return NIL

function ReadDocument( fileName, docN)
local str := "", nHandle, fSize, bufLen := 10000, toRead := 0
local ptr, delimLeft := "[", delimRight := "]"
local docName := delimLeft + docN + delimRight

if !file( fileName) 
  return ""
endif

nHandle := fOpen( fileName)
if nHandle > - 1
   fSize := fSeek( nHandle, 0, 2)
   fSeek( nHandle, 0, 0)
   while toRead <= fSize
     str := fReadStr( nHandle, bufLen)
     toRead += len( str)
     ptr := at( docName, str)
     if ptr > 0
       ptr := fSeek( nHandle, -( len( str) - ptr ) + len( docName) + 1, 1) 
       str := fReadStr( nHandle, bufLen)
       ptr := at( delimLeft, str)
       if ptr > 0
         str := subStr( str, 1, ptr - len( delimLeft))
       endif     
       exit
     endif  
   enddo
   fClose( nHandle)
endif
return str


function ident2Arr with aDoc:A, arr:A ;
   local i, curIdent, curValue, pos

for i := 1 to len( aDoc)
  if left( aDoc[ i], 1) = "#" 
     pos := at( ":=", aDoc[ i])
     if pos > 0
       curIdent := left( aDoc[ i], pos - 1)
       curValue := subStr( aDoc[ i], pos + 1 + 2)
       aAdd( arr, { "@" + allTrim( curIdent), &curValue})
     endif
  endif
next  

return NIL



funcDef CtoA with cStr:C, cDelim:C ;
 local nDelimLen := Len( cDelim), nPtr, aArr := {}

 while len( cStr) > 0
   if ( nPtr := at( cDelim, cStr)) > 0
      aAdd( aArr, SubStr( cStr, 1, nPtr - 1))
      cStr := SubStr( cStr, nPtr + nDelimLen) 
   else
      aAdd( aArr, cStr )
      cStr := ""
   endif
 enddo
return aArr
