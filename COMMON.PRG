/***************************************************************************

      Программа : MON
      Модуль    : COMMON ( Вспомогательные функции )
      Файл      : COMMON.PRG
      Автор     : Залманов А.З.

      Off-line Монитор оператора обслуживания физических лиц ИАС "Забота"

***************************************************************************/

#include "common.ch"
#include "clippext.ch"

#include "tools.ch"
#include "fileio.ch"

funcDef baseMOpen with dbInfo:A, lExclusive:LU, lFast:LU local i, rV
  default lExclusive to .F.
  default lFast to .F.
  if IS_OPEN( dbInfo[ 2 ]) then return .T.
  if fast() .or.lFast
    if lExclusive
      use ( dbInfo[ 1 ]) alias ( dbInfo[ 2 ]) new exclusive
    else
      use ( dbInfo[ 1 ]) alias ( dbInfo[ 2 ]) new shared
    endIf
    select ( dbInfo[ 2 ])
    for i := 1 to len( dbInfo[ 4 ]) do;
     ordListAdd( dbInfo[ 4 ][ i ][ 1 ])
    ( dbInfo[ 2 ]) -> ( netGoTop())
    rV := .T.
  else
    rV := baseOpen( dbInfo )
  endIf
return rV

funcDef baseMXOpen with dbInfo:A
return baseMOpen( dbInfo, .T. )

funcDef basesMOpen with aInfo:A local i, l, rV
  l := 0
  for i := 1 to len( aInfo )
    rV := baseMOpen( aInfo[ i ])
    if !rV
      l := i
      break
    endIf
  next
  for i := 1 to l do baseMClose( aInfo[ i ])
return rV

funcDef basesMExcOpen with aInfo:A local i, l, rV
  l := 0
  for i := 1 to len( aInfo )
    rV := baseMOpen( aInfo[ i ], .T., .T.)
    if !rV
      l := i
      break
    endIf
  next
  for i := 1 to l do baseMClose( aInfo[ i ])
return rV

funcDef baseMClose with dbInfo:A local rV
  if fast() 
    close ( dbInfo[ 2 ])
    rV := .T.
  else
    rV := baseClose( dbInfo )
  endIf
return rV

funcDef basesMClose with aInfo:A local i, rV
  rV := .T.
  for i := 1 to len( aInfo )
    if !baseMClose( aInfo[ i ]) then rV := .F.
  next
return rV