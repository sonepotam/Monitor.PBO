* =========================================================================
*    
*     ������⥪� : CLIPLIB2
*     ����       : C2W_VIEW.PRG
*     ����      : ���� �.�.
*
*     ����ணࠬ�� �뢮�� BROWSE-���� ��� ��ᬮ�� / �롮� ���ଠ樨
*     ( �祭� ����訥 ���⨪���� ���� )
*
* -------------------------------------------------------------------------
   *
   *  viewFtUp(      y1,x1,     aH,aC, bUp,bDn, bTop,bBtm, bIdle, bH, fK) -> colPos
   *  viewFtStartUp( y1,x1,     aH,aC, bUp,bDn, bTop,bBtm,        bH, fK) -> oB
   *  viewFtProc(    y1,x1, oB, aH,aC,                     bIdle,     fK) -> ��� ����ࠡ.������
   *  viewFtExit()                                                        -> NIL
   * ----------------------------------------------------------------------
   *
   *  browseFtUp(      y1,x1, aH,aC, bUp,bDn, bTop,bBtm, bApp,bDel, bIdle, bH, fK) -> lUpdated
   *  browseFtStartUp( y1,x1, aH,aC, bUp,bDn, bTop,bBtm,                   bH, fK) -> oB
   *  browseFtProc( y1, x1, oB, aH, aC,                  bIdle, bApp,bDel,     fK) -> lUpdated
   *  browseFtExit()                                                               -> NIL
   *
   * ----------------------------------------------------------------------
   *
   *  ����� ᮧ�����
   *
   *  v.1.0  - 14/03/93 ��ࢮ��砫�� ��ਠ��
   *  ...
   *  v.2.0  -  9/06/93 ��९��뢠���� �����
   *  ...
   *  v.2.1  -  9/09/93 �� ��⠫���樨 - �஢�ઠ �� ����� ������ᨢ� aC
   *  ...
   *  v.3.0  - 29/11/93 ����祭 � ClipLib2
   *  ...
   *  v.3.1  - 03-Mar-1994 ���. ࠧ������ �� ���㫨, ᫨������ � C2W_BRW?.prg
   *  ...
   *  v.3.2  - 20-Jun-1994 ���� ��� ��ப� ( ��ப� � ᨬ����� '+' )
   *  ...
   *  v.3.3  - 28-Jun-1994 ����� ⠡����
   *  v.3.31 - 02-Jul-1994 ������ ��ࠢ����� � ����
   *  v.3.32 - 04-Jul-1994 setViewHelp(), setBrwHelp()
   *  v.3.33 - 05-Jul-1994 ��१����� BROWSE-�㭪樨, ��२������� � C2W_VIEW
   *  v.3.34 - 28-Jul-1994 ��ࠢ����� � �ᯥ�⮪, �⥪ BROWSE-��ꥪ⮢
   *  v.3.35 - 29-Jul-1994 �������ਨ, ᫨� � C2W_BRWS, C2W_BPRN
   *  v.3.36 - 07-Feb-1995 ��ࠢ���� �㭪�� viewColWidth()
   *
   * ======================================================================
   *
   *  �⠭����� ���������
   *
#include "COMMON.CH"
#include "INKEY.CH"
   *
   *  ���⠭����� ���������
   *
#include "CLIPPEXT.CH"

#define mEval( blk) eval( mBlock( blk))

memVar getList

* ---------------------------------------------------- ���� ���ᨢ� VIEW aC

#define VCOL_TITLE    1
#define VCOL_METHOD   2  /* block ( from view ) */
#define VCOL_DETALIZE 3  /* block               */

* -------------------------------------------------- ���� ���ᨢ� BROWSE aC

#define BCOL_TITLE    1
#define BCOL_METHOD   2  /* block ( from view ) */
#define BCOL_EDIT     3  /* logical             */
#define BCOL_DETALIZE 4  /* block               */
#define BCOL_LOCK     5
#define BCOL_UNLOCK   6


* NEW OBJECT FILE ---------------------------------------------------- viewUp
//
// with : NU NU AU A B B B  B    BU ACBU AU
// or   : AU A  B  B B B BU ACBU AU
//
funcDef viewFtUp with y1:NUA, x1:NUA, aH:AUB, aC:AB,    ;
                    bUp:B, bDn:B, bTop:BU, bBtm:ACBU, ;
                    bIdle:BUA, bH:ACBU, fK:AU, cStr:CU, aF:AU, lSep:LU  ;
   local oB, key, rV := 0
   if isArray( x1)
      //
      // y1, x1 ���饭�
      //
      rV := viewUp( row()+1, col()-2, y1, x1, aH, aC, bUp, bDn, ;
                    bTop, bBtm, bIdle)
   else
      //
      // y1, x1 㪠���� ( ����� ���� ��� NIL ��� ,, )
      //
      DEFAULT y1 TO row()+1
      DEFAULT x1 TO col()-2
      oB := viewFTStartUp( @y1, @x1, aH, aC, bUp, bDn, bTop, bBtm, bH, fK,;
                           cStr, aF, lSep)
      do while ( key := viewFTProc( y1, x1, oB, aH, aC, bIdle, fK, cStr,;
                                    aF)) != K_ESC
         if key = K_ENTER
            rV := oB:colPos
            exit
         endIf
      endDo
      viewExit()
   endIf
return rV


* NEW OBJECT FILE --------------------------------------------------- startUp

funcDef viewFtStartUp ;
  with y1:N, x1:N, aH:AU, aC:A, bUp:B, bDn:B, bTop:B, bBtm:B  ,;
       bH:ACBU, fK:AU, cStr:CU, aF:AU, lSep:LU, dy1:NU, dx1:NU ;
  local headerHeight,                                 ;
         footerHeight,                                 ;
         lastValid,                                    ;
         oB,                                           ;
         curColumn,                                    ;
         i, dx, dy
   //
   // ���᪠��� �� 㬮�砭��
   //
   DEFAULT bH TO setViewHelp()
   // DEFAULT cStr TO ""
   //
   // ��⠭���� ���ᨬ��쭮 �����⨬�� ����� view-����
   //
   if .NOT.empty( fK)
      lastValid := maxRow() - 2  // ��᫥���� �����⨬�� ��ப�
   else
      lastValid := maxRow()
   endIf
   //
   // �����⮢�� ���ᨢ� GET-��ꥪ⮢ � ���������
   //
   getPrep( aH)
   //
   // �����⮢�� ���ᨢ� ������� � ���誥 
   getPrep( aF)
   //
   // ��।���� ����� BROWSE-����
   //
   headerHeight := getHeight( aH) + if( !isNIL( cStr), 1, 0)
   footerHeight := getHeight( aF)
   if y1 + headerHeight + 13 > lastValid
      y1 := lastValid - headerHeight - 13
   endIf
   dy := lastValid - y1 - 1
   //
   // ��।���� �ਭ� BROWSE-����( � ��⮬ ��p��� ���᪠ )
   //
   dx := max( viewWidth( aH, aC), viewWidth( aF, aC))
   if !isNIL( cStr) then dx := max( dx, len( cStr) + len( &(indexKey( 0)))  )
   //
   // �뢥��� ���� ��� browse
   //
   if !empty( dx1) then dx := dx1
   if !empty( dy1) then dy := dy1

   wStGLstPush( getList)
   getList := {}
   windowAltStartUp( @y1, @x1, @dy, @dx, setBrwColor(), 0, bH)
   if !isNIL( cStr) then @ y1 + 1, x1 + 1 SAY cStr
   //
   // ����㥬 ��ப� �㭪樮������ ������
   //
   fKeysStartUp( fK)
   //
   // �������� browse-��ꥪ�, ������� ��ꥪ��-�⮫���
   //
   oB := TBrowseNew( y1+headerHeight, x1+1, y1+1+dy-footerHeight-2, x1+dx)
   @ y1+1+headerHeight, x1      SAY "�"
//   @ y1+2+headerHeight, x1      SAY "�"
//   @ y1+2+headerHeight, x1+dx+1 SAY "�"
   @ y1+1+headerHeight, x1+dx+1 SAY "�"
   oB:headSep       := "�"
   if !isNIL( lSep) .AND. lSep then oB:colSep := "�"
   oB:skipBlock     := {| n| bSkipBlock( n, bUp, bDn)}
   oB:goTopBlock    := bTop
   oB:goBottomBlock := bBtm
   for i=1 to len( aC)
         oB:addColumn( TBColumnNew( aC[ i][ VCOL_TITLE], ;
                                    mBlock( aC[ i][ VCOL_METHOD])))
   endFor
   //
   // ������� ��ꥪ�
   //
   wStBrwsPush( { oB, .F., .F.})
   oB:refreshAll()
   delay while .NOT.oB:stabilize()
   if !empty( aF) then ;
   @ oB:nBottom + 1, oB:nLeft say replicate( "�", oB:nRight - oB:nLeft + 1)
return oB

* NEW OBJECT FILE ------------------------------------------------------ proc

#define K_UNAVAILABLE 9999

funcDef viewFtProc with y1:N, x1:N, oB:O, aH:AU, aC:A, bIdle:BU, fK:AU,;
                        cStr:CBU, aF:AU ;
   local key := 0, i, get, item // := space( len( &( indexKey( 0))))

   do while .T.

      if isCharacter( cStr) then @ y1 + 1, x1 + 1 SAY cStr
      if isBlock(     cStr) then @ y1 + 1, x1 + 1 SAY eval( cStr)

      wStBrwsTop()[ 1] := oB
      //
      // �᫨ ���� - � �����㥬 ��ꥪ� ��� ⥪���� ��ப�
      //
      if wStBrwsTop()[ 2] = .T.
         oB:refreshAll()
         delay while .NOT.oB:stabilize()
         wStBrwsTop()[ 2] := .F.
         wStBrwsTop()[ 3] := .F.
      endIf
      if wStBrwsTop()[ 3] = .T.
         oB:refreshCurrent()
         delay while .NOT.oB:stabilize()
         wStBrwsTop()[ 3] := .F.
      endIf
      //
      // �������� ����⨩, �᫨ ��� - �⠡������� � ��ਮ���᪠� ����ᮢ��
      //
      delay while .NOT.oB:stabilize() .AND. ( key := xHelpKey()) = 0
      if key = 0
         if ( key := xHelpKey( 0.2)) = 0
            displayStatusLine()
            do while key = 0
               if .NOT.empty( aH)
                  xGetProc( y1 + 1, x1, aH) // ��易⥫쭮 �� bIdle !!!
               endIf
               if .NOT. empty( aF)
                  xGetProc( oB:nBottom + 1, oB:nLeft, aF)
               endif
               if isBlock( bIdle) then eval( bIdle)
               for i=1 to 8
                  key = xHelpKey( 0.1)
                  fKeysProc( fK)
                  exit if key != 0
               endFor
            endDo
         endIf
      endIf
      //
      // �믮������ ����⢨� �� ������
      //
      if key = K_UNAVAILABLE     // ������ "⮫쪮 �⠡�������"
         key = 0
         loop
      endIf
      if fKeysProc( fK, key)  // ������ - �㭪樮���쭠�
         key = 0
         loop
      endIf
      do case
         case key = K_DOWN .AND. .NOT.oB:hitBottom do oB:down()
         case key = K_UP   .AND. .NOT.oB:hitTop    do oB:up()
         case key = K_PGDN                         do oB:pageDown()
         case key = K_PGUP                         do oB:pageUp()
         case key = K_CTRL_PGDN                    do oB:goBottom()
         case key = K_CTRL_PGUP                    do oB:goTop()
         case key = K_CTRL_END                     do oB:panEnd()
         case key = K_CTRL_HOME                    do oB:panHome()
         case key = K_RIGHT                        do oB:right()
         case key = K_LEFT                         do oB:left()
         case key = K_HOME                         do oB:home()
         case key = K_END                          do oB:end()
         case key = K_CTRL_F1                      do oB:refreshAll()
         case key = K_ALT_F1                       do oBSearch( oB, aC)
         case key = K_SH_F1                        do oBSearc2( oB, aC)
         case key = K_TAB .AND. .NOT.empty( aH)              // ���������
            xGetProc( y1+1, x1, aH)
            getRead(  y1+1, x1, aH)
            requiredStable()
         case key = K_SPACE                 .AND. ;
              len(     aC[ oB:colPos]) >= VCOL_DETALIZE .AND. ;
              isBlock( aC[ oB:colPos][ VCOL_DETALIZE])         // ��⠫�����
            if eval( aC[ oB:colPos][ VCOL_DETALIZE])
               requiredStable()
            endIf
         otherWise
            exit if key != 0
      endCase
      key = 0
   endDo
return key

* ---------------------------------------------------------- ������ �஢���

static procDef xGetProc with y1:N, x1:N, aH:AU ;
   local curY, curX, savedColor
   curY := row()
   curX := col()
   savedColor := setGetColor( setBrwColor()) // �㤥� GET 梥� BROWSE
   getProc( y1, x1, aH)
   setGetColor( savedColor)
   setPos( curY, curX)
return

static funcDef xHelpKey with nSecs:NU local nKey, bKeyBlock
   do while .T.
   nKey := if( pCount() = 0, inKey(), inKey( nSecs))
      exit if empty( bKeyBlock := setKey( nKey))
      eval( bKeyBlock, procName( 2), procLine( 2))
      if wStBrwsTop()[ 2]
         nKey := K_UNAVAILABLE
         exit
      endIf
   endDo
return nKey


* NEW OBJECT FILE ------------------------------------------------------ exit

procDef viewFtExit
   wStBrwsDrop()
   fKeysExit()
   windowExit()
   getList := wStGLstPop()
return


* NEW OBJECT FILE -------------------------------------------------- browseUp
//
// with : NU NU AU A B B B  B  BU BU   BU ACBU AU
// or   : AU A  B  B B B BU BU BU ACBU AU
//
funcDef browseFtUp ;
   with y1:NUA, x1:NUA, aH:AUB, aC:AB, bUp:B, bDn:B, bTop:BU, bBtm:BU,;
        bApp:BU, bDel:ACBU, bIdle:BUA, bH:ACBU, fK:AU, cStr:CU, aF:AU,;
        dy1:NU, dx1:NU local oB, rV
   if isArray( x1)
      //
      // y1, x1 ���饭�
      //
      rV := browseUp( row()+1, col()-2, y1, x1, aH, aC, bUp, bDn, ;
                      bTop, bBtm, bApp, bDel, bIdle)
   else
      //
      // y1, x1 㪠���� ( ����� ���� ��� NIL ��� ,, )
      //
      DEFAULT y1 TO row()+1
      DEFAULT x1 TO col()-2
      oB := browseStartUp( @y1, @x1,     aH, aC, bUp, bDn, bTop, bBtm, bH, fK)
      rV := browseProc(     y1,  x1, oB, aH, aC, bIdle, bApp, bDel,        fK)
      browseExit()
   endIf
return rV

* NEW OBJECT FILE --------------------------------------------------- startUp

funcDef browseFtStartUp ;
   with y1:N, x1:N, aH:AU, aC:A, bUp:B, bDn:B, bTop:B, bBtm:B,;
        bH:ACBU, fK:AU, cStr:CU, aF:AU, dy1:NU, dx1:NU ;
   local i
   //
   // ���᪠��� �� 㬮�砭��
   //
   DEFAULT bH TO setBrwHelp()
   for i=1 to len( aC)
      if len( aC[ i]) < BCOL_UNLOCK
         aC[ i] := aSize( aC[ i], BCOL_UNLOCK)
      endIf
   endFor
return viewFtStartUp( @y1, @x1, aH,aC,bUp,bDn,bTop,bBtm,bH,fK,cStr,aF,,dy1,dx1)

* NEW OBJECT FILE ----------------------------------------------- proc / exit

funcDef browseFtProc with y1:N, x1:N, oB:O, aH:AU, aC:A, ;
                        bIdle:BU, bApp:BU, bDel:BU,    ;
                        fK:AU                          ;
   local key, updated := .F., tmp
   do while ( key := viewProc( y1, x1, oB, aH, aC, ;
                               bIdle, fK)) != K_ESC
      tmp := browseHandle( key, oB, aH, aC, bApp, bDel)
      if isLogical( tmp)
         if tmp
            if .NOT.updated then updated := .T.
            requiredStable()
         endIf
      else
         // assert( isNumber( tmp), valType( tmp))
         beep()
         loop
      endIf
   endDo
return updated

* NEW OBJECT FILE


procDef browseFtExit
   viewExit()
return


* =========================================================================
* =========================================================================
