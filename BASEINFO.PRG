/***************************************************************************

      Программа : MON
      Модуль    : BASEINFO ( Манипуляции с информационной справкой )
      Файл      : BASEINFO.PRG
      Автор     : Залманов А.З.

      Off-line Монитор оператора обслуживания физических лиц ИАС "Забота"

***************************************************************************/

#include "common.ch"
#include "clippext.ch"

#include "fileio.ch"
#include "directry.ch"
#include "inkey.ch"

#define MC_NOT_USED 0
#define MC_OPTIONAL 1
#define MC_AGENT    2
#define MC_BANK     3
#define MC_ALL      4

#define DT_DATE    4
#define DT_CHAR    5
#define DT_NUMBER  7

funcDef infoCheck local infoSpec, rV, lAgent, nCost, cVal, cName
  infoSpec := infoInfo()
  if !baseMOpen( infoSpec )
    errLog({ "Не найден файл информационной справки." })
    return .F.
  endIf
  rV     := .T.
  lAgent := confUseAgent()
  statStartup( "Проверка содержания информационной справки..." )
  do while !INFO -> ( eof())
    statProc()
    nCost := INFO -> MANDATORY
    cVal  := INFO -> DATA
    cName := allTrim( INFO -> PROMPT )
    if empty( cVal ) .and. (( nCost == MC_ALL ) .or.;
                            ( nCost == MC_AGENT .and.  lAgent ) .or.;
                            ( nCost == MC_BANK  .and. !lAgent))
      errLog({ "Не заполнено обязательное для заполнение поле",;
               '"' + cName + '"' })
      rV := .F.
    endIf
    exit if !INFO -> ( netSkipDown())
  endDo
  statExit()
  baseMClose( infoSpec )
return rV

funcDef infoEdit local rV, aG, aR, infoSpec, s, cName, cVal, nType, cPicture,;
                       bValid, nCost, lAgent, i, cExpr
  aG := {}
  aR := {} 
  infoSpec := infoInfo()
  if !baseMOpen( infoSpec )
    errLog({ "Не найден файл информационной справки." })
    return .F.
  endIf
  rV     := .T.
  lAgent := confUseAgent()
  aAdd( aG, { s := "Информационная справка " + confOtdName() + " за " + wordDate( setCurDate())})
  aAdd( aG, { replicate( chr( 223 ), len( s ))})
  statStartup( "Чтение содержания информационной справки..." )
  do while !INFO -> ( eof())
    statProc()
    nCost := INFO -> MANDATORY
    if nCost != MC_NOT_USED 
      cVal  := allTrim( INFO -> DATA )
      cName := padR( allTrim( INFO -> PROMPT ), 40 )
      nType := INFO -> DATATYPE
      do case 
        case nType == DT_DATE
          cVal     := ctod( cVal )
          cPicture := nil
        case nType == DT_NUMBER
          if !empty( cVal ) 
            cVal := val( cVal )
          else
            cVal := 0.00
          endIf
          cPicture := "999 999 999.99"
        otherwise
          cVal     := padR( cVal, 200 )
          cPicture := "@S30"
      end case
      aAdd( aR, INFO -> ( recNo()))
      do case
        case ( nCost == MC_ALL ) .or. (( nCost == MC_AGENT ) .and.  lAgent )
          bValid := {| oG | notEmptyOrZero( oG: varGet())}
        case ( nCost == MC_ALL ) .or. (( nCost == MC_BANK  ) .and. !lAgent )
          bValid := {| oG | notEmptyOrZero( oG: varGet())}
        otherwise
          bValid := nil
      end case
      aAdd( aG, { cName, cVal, cPicture,, bValid })
    endIf
    exit if !INFO -> ( netSkipDown())
  endDo
  statExit()
  if getUp( 2, 2, aG )
    for i := 1 to len( aR )
      INFO -> ( dbGoTo( aR[ i ]))
      INFO -> ( mRLock())
      INFO -> DATA := any2str( aG[ 2 + i ][ 2 ])
      INFO -> ( netUnlock())
    next
  endIf
  baseMClose( infoSpec )
return rV

funcDef infoPrint local infoSpec, dCur, tplName, cName, nType, cValue, i

  altD()

  dCur     := setCurDate()
  tplName  := dcgnExtract( "info" )
  infoSpec := infoInfo()
  if empty( tplName )
    errLog( "Отсутствует шаблон информационной справки!",, .T. )
    return .F.
  endIf
  dcgnBlnPath( ".\"  )
  dcgnDocPath( ".\"  )
  dcgnName( getFName( tplName ))
  if !dcgnBlnLoad()
    errLog( "Hевозможно загрузить шаблон информационной справки!",, .T. )
    ferase( tplName )
    return .F.
  endIf
  if !baseMOpen( infoSpec )
    errLog( "Невозможно загрузить данные информационной справки!",, .T. )
    ferase( tplName )
    return .F.
  endIf
  statStartup( "Выполняется формирование документа..." )
  dcgnDocReset()
  dcgnOpen()
  dcgnFldFill({{ "@departName", confOTDName(),    "L" },;
               { "@curDate",    wordDate( dCur ), "L" }})
  dcgnDocFill( "header" )
  do while !INFO -> ( eof())
    statProc()
    cName  := INFO -> PROMPT
    nType  := INFO -> DATATYPE
    cValue := INFO -> DATA
    if nType == DT_CHAR 
      cValue := textFormat( cValue, 30 )
      if len( cValue ) == 0 then cValue := { "" }
    else
      cValue := { cValue }
    end case
    dcgnFldFill({{ "@infoComment", cName,       "L" },;
                 { "@infoValue",   cValue[ 1 ], "L" }})
    dcgnDocFill( "body1" )
    for i := 2 to len( cValue )
      dcgnFldFill({{ "@infoValue",   cValue[ i ], "L" }})
      dcgnDocFill( "body" + nStr( i ))    
    next
    dcgnDocFill( "separator" )
    exit if !INFO -> ( netSkipDown())
  endDo
  dcgnPut()
  dcgnClose()
  statExit()
  dcgnShow()
  if !yesNo( "Печатать документ?" )
    dcgnErase()
    ferase( tplName )
    baseMClose( infoSpec )
    return .F.
  endIf
  if !checkPrinter()
    dcgnErase()
    ferase( tplName )
    baseMClose( infoSpec )
    return .F.
  endIf
  dcgnGo()
  ferase( tplName )
  dcgnErase()
  baseMClose( infoSpec )
return .T.

