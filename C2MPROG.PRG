* =========================================================================
*
*     Библиотека : CLIPLIB2
*     Файл       : C2M_PROG.PRG
*     Автор      : Митюгов Д.В.
*
*     Основное меню программы
*
* -------------------------------------------------------------------------
   *
   *  История создания
   *
   *  v.1.0  - 26/01/93 первоначальный вариант
   *  ...
   *  v.1.1  - ??/??/?? второй begin/end seq - start/exit для 1го, rec - для 2
   *  ...
   *  v.1.2  - 22/04/93 перенесен в библиотеку CLIPMAIN, добавлено mainProg,..
   *  ...
   *  v.1.3  - 16/06/93 logonStartUp() напрямую вызывает windowAltStartUp()
   *  ...
   *  v.2.0  - 29/11/93 включен в ClipLib2
   *  v.2.01 -  6/12/93 добавлены команды сохранения / восстановления экрана
   *  v.2.02 - 10/01/94 вызов netTime()
   *
   *  v.2.1  - 03-Mar-1994 авт. разбиение на модули, слияние с C2M_PRG2.prg
   *  v.2.11 - 10-Jun-1994 ревизия ( первоначальный вариант )
   *  v.2.12 - 23-Jun-1994 popExt -> popUp
   *  v.2.13 - 24-Jun-1994 включается CLIPMAIN.CH
   *  v.2.14 - 14-Jul-1994 упаковка баз не прерывается по <Esc>
   *  v.2.15 - 22-Jul-1994 поддержка протокола регистрации пользователей
   *  v.2.2  - 10-Aug-1994 updateMenus выполняется после startUpCode
   *
   * ======================================================================
   *
   *  Стандартные заголовки
   *
#include "COMMON.CH"
#include "INKEY.CH"
   *
   *  Нестандартные заголовки
   *
#include "CLIPPEXT.CH"

#define ITEM_PROMPT  1
#define ITEM_MESSAGE 2
#define ITEM_RECORD  3
#define ITEM_HELP    4

* NEW OBJECT FILE -------------------------------- файл протокола регистрации

procDef mainProgP1 with aItems:A, logonText:A,                    ;
                      startUpCode:B, exitCode:B, recoverCode:B, ;
                      helpH:ACBU                                ;
   local mainScr := saveScreen( 0, 0, maxRow(), maxCol())
   //
   // Начальные установки
   //
   setCancel( .F.)
   setKey( K_ALT_C, {|| BaltC() })
   netTime()
   setCurDate( date())
   begin sequence
      //
      // Подготовка экрана
      //
      clipWindInit()
      readFirstColors()
      prepareScreen()
      installClock()
      //
      // Приветствие, запрос пароля
      //
      logonStartUp( logonText)
      // break if .NOT.passCheck()
      //
      // Пользовательская функция загрузки
      //
      evalIfBlock( startUpCode)
      logonExit()
      //
      // Чтение цветов пользователя ( если определены ), обновление экрана
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
      // Подготовка и вывод основного меню; цикл пока не завершили программу
      //
      updateMenP( aItems)
      mainMenu( aItems, recoverCode)
   endSequence
   //
   // Пользовательская функция завершения работы
   //
   messageStartUp( "До свидания !")
   evalIfBlock( exitCode)

   messageExit( 1)
   //
   // Восстановление экрана
   //
   releaseScreen()
   removeClock()
   restScreen( 0, 0, maxRow(), maxCol(), mainScr)
return

static procedure BaltC
   local old := setKey( K_ALT_C, NIL)
   local key := alarm( "Вы нажали комбинацию клавиш Alt-C", ;
                       { "Продолжить", "Прервать", "Завершить работу" })
   //
   // Обработка ответа пользователя
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
   userFirst := { users[ 1], users[ 2], users[ 3], {}, users[ 5] } // Б.инд
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