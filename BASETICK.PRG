*============================================================================
* 
*                  Программа : MON.EXE
*                  Файл      : basetick.prg
*                  Автор     : Омельченко В.Д.
*
*                  Назначение: Описание операций для базы льготных проездных документов
*----------------------------------------------------------------------------
*
*   ticInfo()                -> dbInfo
*   ticPath( [ newPath])     -> dbInfo
*   ticOpen()                -> T/F  - SHARED
*   ticXOpen()               -> T/F  - EXCLUS
*   ticClose()               -> NIL
*   ticCreate()              -> NIL
*   ticSeek( par, lSoftSeek) -> T/F
*   ticIndex()               -> NIL
*
*   ticAppend()              -> T/F
*   ticRLock()               -> T/F
*   ticUnLock()              -> NIL
*
*   ticEdit()                -> T/F         
*----------------------------------------------------------------------------
#include "mon.ch"
#include "common.ch"
#include "clippExt.ch"
#include "inkey.ch"

func ticInfo
  static dbInfo := {".\TICKETS.DBF", "TICKETS", { },{{,'BUFFER'}},0}
  dbInfo[3]:={{"CODE","C",12,0},;
            {"BUFFER","C",30,0},;
            {"COLIBRI","C",10,0}}
return dbInfo

funcDef ticPath with newPath:CU
return basePath( ticInfo(), newPath )

funcDef ticOpen
return baseOpen( ticInfo() )

funcDef ticXOpen
return baseXOpen( ticInfo() )

funcDef ticClose
return baseMClose( ticInfo() )

funcDef ticCreate
return baseCreate( ticInfo() )

funcDef ticSeek with par:C, softSeek:LU
return TICKETS->( netSeek( par, softSeek) )

funcDef ticAppend
return TICKETS ->( mAppend())

funcDef ticIndex
return baseIndex( ticInfo() )

funcDef ticRLock
return TICKETS ->( mRLock())

funcDef ticUnLock
return TICKETS ->( netUnLock())
