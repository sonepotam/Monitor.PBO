//
// 22.04.94
//
#include "common.ch"
  
//
//   ���쭨�� ����� : ������ �஢���
//   ����樨 � ����楬( SprFondr )
//


//
//  <<  work  >>
//



//
//  ��������� ��ப� �� ⮪���
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
// ���ᨢ � ��ப�
//
Function aToc( aArr, cDelim)
Local i, cStr := ""

DEFAULT cDelim TO chr( 13) + chr( 10)

for i := 1  to  Len( aArr)
    cStr += any2Str( aArr[ i]) + cDelim
next
cStr := SubStr( cStr, 1, Len( cStr) - Len( cDelim))

return cStr
