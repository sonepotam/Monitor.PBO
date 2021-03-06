* =========================================================================
*
*     ������⥪� : CLIPLIB2
*     ����       : C2M_PROG.PRG
*     ����      : ���� �.�.
*
*     �᭮���� ���� �ணࠬ��
*
* -------------------------------------------------------------------------
   *
   *  ����� ᮧ�����
   *
   *  v.1.0  - 26/01/93 ��ࢮ��砫�� ��ਠ��
   *  ...
   *  v.1.1  - ??/??/?? ��ன begin/end seq - start/exit ��� 1��, rec - ��� 2
   *  ...
   *  v.1.2  - 22/04/93 ��७�ᥭ � ������⥪� CLIPMAIN, ��������� mainProg,..
   *  ...
   *  v.1.3  - 16/06/93 logonStartUp() ������� ��뢠�� windowAltStartUp()
   *  ...
   *  v.2.0  - 29/11/93 ����祭 � ClipLib2
   *  v.2.01 -  6/12/93 ��������� ������� ��࠭���� / ����⠭������� ��࠭�
   *  v.2.02 - 10/01/94 �맮� netTime()
   *
   *  v.2.1  - 03-Mar-1994 ���. ࠧ������ �� ���㫨, ᫨ﭨ� � C2M_PRG2.prg
   *  v.2.11 - 10-Jun-1994 ॢ���� ( ��ࢮ��砫�� ��ਠ�� )
   *  v.2.12 - 23-Jun-1994 popExt -> popUp
   *  v.2.13 - 24-Jun-1994 ����砥��� CLIPMAIN.CH
   *  v.2.14 - 14-Jul-1994 㯠����� ��� �� ���뢠���� �� <Esc>
   *  v.2.15 - 22-Jul-1994 �����প� ��⮪��� ॣ����樨 ���짮��⥫��
   *  v.2.2  - 10-Aug-1994 updateMenus �믮������ ��᫥ startUpCode
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

#define ITEM_PROMPT  1
#define ITEM_MESSAGE 2
#define ITEM_RECORD  3
#define ITEM_HELP    4

* NEW OBJECT FILE -------------------------------- 䠩� ��⮪��� ॣ����樨

procDef mainProgP1 with aItems:A, logonText:A,                    ;
                      startUpCode:B, exitCode:B, recoverCode:B, ;
                      helpH:ACBU                                ;
   local mainScr := saveScreen( 0, 0, maxRow(), maxCol())
   //
   // ��砫�� ��⠭����
   //
   setCancel( .F.)
   setKey( K_ALT_C, {|| BaltC() })
   netTime()
   setCurDate( date())
   begin sequence
      //
      // �����⮢�� ��࠭�
      //
      clipWindInit()
      readFirstColors()
      prepareScreen()
      installClock()
      //
      // �ਢ���⢨�, ����� ��஫�
      //
      logonStartUp( logonText)
      // break if .NOT.passCheck()
      //
      // ���짮��⥫�᪠� �㭪�� ����㧪�
      //
      evalIfBlock( startUpCode)
      logonExit()
      //
      // �⥭�� 梥⮢ ���짮��⥫� ( �᫨ ��।����� ), ���������� ��࠭�
      //
      // readColors()

      if baseNSOpen( usersInfo())

         if USERS ->( !dbSeek( "oper")) then USERS ->( netGoTop())

         setScrColor( USERS -> colorsScr)
         setMnuColor( USERS -> colorsMnu)
         setMesColor( USERS -> colorsMes)
         setPopColor( USERS -> colorsPop)
         setBrwColor( USERS -> colorsBrw)
         setGetColor( USERS -> colorsGet)
         setDlgColor( USERS -> colorsDlg)
         setErrColor( USERS -> colorsErr)
         setHlpColor( USERS -> colorsHlp)
         setOprColor( USERS -> colorsOpr)
         setMemColor( USERS -> colorsMem)
         userUnSelect()
      endif

      prepareScreen( helpH)
      //
      // �����⮢�� � �뢮� �᭮����� ����; 横� ���� �� �����訫� �ணࠬ��
      //
      updateMenP( aItems)
      mainMenu( aItems, recoverCode)
   endSequence
   //
   // ���짮��⥫�᪠� �㭪�� �����襭�� ࠡ���
   //
   messageStartUp( "�� ᢨ����� !")
   evalIfBlock( exitCode)

   messageExit( 1)
   //
   // ����⠭������� ��࠭�
   //
   releaseScreen()
   removeClock()
   restScreen( 0, 0, maxRow(), maxCol(), mainScr)
return

static procedure BaltC
   local old := setKey( K_ALT_C, NIL)
   local key := alarm( "�� ������ ��������� ������ Alt-C", ;
                       { "�த������", "��ࢠ��", "�������� ࠡ���" })
   //
   // ��ࠡ�⪠ �⢥� ���짮��⥫�
   //
   do case
      case key = 2
         setKey( K_ALT_C, old)
         break
      case key = 3
         quit
   endCase
   setKey( K_ALT_C, old)
return

/*
funcDef userFirst local rV := .F., userFirst, users := usersInfo()
   userFirst := { users[ 1], users[ 2], users[ 3], {}, users[ 5] } // �.���
   if .NOT.file( userFirst[ 1])  then return .F.
   if .NOT.baseNSCheck( userFirst) then return .F.
   if baseNSOpen( userFirst)
      if .NOT.USERS ->( emptyRec())
         rV := .T.
      endIf
      if .NOT.rV then USERS ->( netClose())
   endIf
return rV
*/