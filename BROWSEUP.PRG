* =========================================================================
* 
   *  Стандартные заголовки
   *
#include "COMMON.CH"
#include "INKEY.CH"
   *
   *  Нестандартные заголовки
   *
#include "CLIPPEXT.CH"

#define mEval( blk) eval( mBlock( blk))

memVar getList

* ---------------------------------------------------- поля массива VIEW aC

#define VCOL_TITLE    1
#define VCOL_METHOD   2  /* block ( from view ) */
#define VCOL_DETALIZE 3  /* block               */

* -------------------------------------------------- поля массива BROWSE aC

#define BCOL_TITLE    1
#define BCOL_METHOD   2  /* block ( from view ) */
#define BCOL_EDIT     3  /* logical             */
#define BCOL_DETALIZE 4  /* block               */
#define BCOL_LOCK     5
#define BCOL_UNLOCK   6

* NEW OBJECT FILE ------------- управление стеком сохраненных BROWSE-объектов

funcDef view1StartUp with y1:N, x1:N, dy1:NU, dx1:NU, aH:AU, aC:A,      ;
                         bUp:B, bDn:B, bTop:B, bBtm:B, ;
                         bH:ACBU, fK:AU                ;
   local headerHeight,                                 ;
         lastValid,                                    ;
         dy,                                           ;
         dx,                                           ;
         oB,                                           ;
         curColumn,                                    ;
         i
   //
   // Подсказка по умолчанию
   //
   DEFAULT bH TO setViewHelp()
   //
   // Установка максимально допустимой высоты view-окна
   //
   if .NOT.empty( fK)
      lastValid := maxRow() - 2  // Последняя допустимая строка
   else
      lastValid := maxRow()
   endIf
   //
   // Подготовка массива GET-объектов в заголовке
   //
   getPrep( aH)
   //
   // Определим высоту BROWSE-окна
   //
   headerHeight := getHeight( aH)
   if y1 + headerHeight + 13 > lastValid
      y1 := lastValid - headerHeight - 13
   endIf
   dy := lastValid - y1 - 1
   //
   // Определим ширину BROWSE-окна
   //
   dx := viewWidth( aH, aC)
   //
   // Выведем окно для browse
   //
   wStGLstPush( getList)
   getList := {}
   windowAltStartUp( @y1, @x1, @dy, @dx, setBrwColor(), 0, bH)
   //
   // Нарисуем строку функциональных клавиш
   //
   fKeysStartUp( fK)
   //
   // Создадим browse-объект, добавим объекты-столбцы
   //
   dy1:= if( !empty( dy1), y1 + dy1, y1 + dy)
   dx1:= if( !empty( dx1), x1 + dx1, x1 + dx)
   // oB := TBrowseNew( y1+1+headerHeight, x1+1, y1+dy, x1+dx)
   oB := TBrowseNew( y1+1+headerHeight, x1+1, dy1, dx1)
  
   @ y1+2+headerHeight, x1      SAY "├"
   @ y1+2+headerHeight, x1+dx+1 SAY "┤"
   oB:headSep       := "─"
   oB:skipBlock     := {| n| bSkipBlock( n, bUp, bDn)}
   oB:goTopBlock    := bTop
   oB:goBottomBlock := bBtm
   for i=1 to len( aC)
         oB:addColumn( TBColumnNew( aC[ i][ VCOL_TITLE], ;
                                    mBlock( aC[ i][ VCOL_METHOD])))
   endFor
   //
   // Покажем объект
   //
   wStBrwsPush( { oB, .F., .F.})
   oB:refreshAll()
   delay while .NOT.oB:stabilize()
return oB

funcDef inkey0 with fk:AU local key := 0, i

if ( key := inkey( 0.2)) == 0
   while key == 0
         displayStatusLine()
         for i := 1 to 8
             key := inkey( 0.1)
             fKeysProc( fk)
             exit if key <> 0
         next
   if setKey( key) <> NIL then eval( setKey( key))
   enddo
endif
return key


funcDef aBrTop with arr:A, ptr:N
ptr := 1
return NIL

funcDef aBrBottom with arr:A, ptr:N
ptr := len( arr)
return NIL
 
funcDef aBrUp with arr:A, ptr:N local rV := .F., old := ptr
ptr := max( 1, ptr - 1)
return ptr <> old

funcDef aBrDown with arr:A, ptr:N local rV := .F., old := ptr
ptr := min( len( arr), ptr + 1)
return ptr <> old

* =========================================================================
* =========================================================================
