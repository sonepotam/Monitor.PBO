//
// 22.04.94
//
#include "common.ch"
  
//
//   Больничные листы : нижний уровень
//   Операции с месяцем( SprFondr )
//


//
//  <<  work  >>
//



//
//  Разбиение строки на токены
//
Function cTOa( cStr, cDelim )
Local nDelimLen := Len( cDelim )
Local nPtr 
Local aArr := { }


while len( cStr) > 0 //!Empty( cStr )

      if ( nPtr := at( cDelim, cStr ) ) > 0

         aAdd( aArr, SubStr( cStr, 1, nPtr - 1 )   )
         cStr := SubStr( cStr, nPtr + nDelimLen ) 

      else

         aAdd( aArr, cStr )
         cStr := ""
 
      endif

enddo

return aArr
              
//
// Массив в строку
//
Function aToc( aArr, cDelim)
Local i, cStr := ""

DEFAULT cDelim TO chr( 13) + chr( 10)

for i := 1  to  Len( aArr)
    cStr += any2Str( aArr[ i]) + cDelim
next
cStr := SubStr( cStr, 1, Len( cStr) - Len( cDelim))

return cStr
