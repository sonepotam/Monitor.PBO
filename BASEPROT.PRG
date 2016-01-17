*==============================================================================
*  
*                    Программа : MON.EXE
*                    Файл      : baseProt.prg
*                    Автор     : Цейтлин П.М.
*
*------------------------------------------------------------------------------
#include "clippExt.ch"
#include "fileio.ch"
#include "common.ch"

func logInfo
static arr := { ".\modemLog.DBF", "MODEMLOG",;
               {{ "fileName", "C", 12, 0},;
                { "fSize",    "N", 10, 0},;
                { "fDate",    "D",  8, 0},; 
                { "fTime",    "C",  8, 0},;
                { "wDate",    "D",  8, 0},;
                { "wTime",    "C",  8, 0},;
                { "fError",   "N",  3, 0},;
                { "userName", "C", 15, 0}},;
               {}, 0}
return arr

funcDef logPath with newPath:CU
return basePath( logInfo(), newPath)

funcDef logOpen
return baseMOpen( logInfo())

funcDef logXOpen
return baseXOpen( logInfo() )

funcDef logClose
return baseMClose( logInfo())

funcDef logWrite with fileInfo:A, errN:N local rV := .F., arr, key, crc, s
s   := Select()
crc := ReadCrc( logInfo())
if logOpen()
   key := getCrcKey()
   if MODEMLOG ->( mAppend())
      MODEMLOG ->fileName := Encode( fileInfo[ 1], key)
      MODEMLOG ->fSize    := Encode( fileInfo[ 2], key)
      MODEMLOG ->fDate    := Encode( fileInfo[ 3], key)
      MODEMLOG ->fTime    := Encode( fileInfo[ 4], key)
      MODEMLOG ->fError   := Encode( errN,         key)
      MODEMLOG ->wDate    := Encode( setCurDate(), key)
      MODEMLOG ->wTime    := Encode( time(),       key)
      MODEMLOG ->userName := Encode( userName(),   key)
      Crc += Crc( MODEMLOG ->fileName) + Crc( MODEMLOG ->fSize ) + ;
             Crc( MODEMLOG ->fDate   ) + Crc( MODEMLOG ->fError) + ;
             Crc( MODEMLOG ->fTime   ) + Crc( MODEMLOG ->wDate ) + ;
             Crc( MODEMLOG ->wTime   ) + Crc( MODEMLOG ->userName)
      MODEMLOG ->( netUnLock())
      rV := .T.
   endif
   logClose()
   if rV then WriteCrc( logInfo(), Crc, key)
endif
select (s)
return rV


funcDef logDummy with fileName:C local arr
   arr := { fileName, 0, CtoD( ""), ""}
return arr

func errInfo 
static arr := { ".\errlog.DBF", "ERRLOG",;
               {{ "Str",      "C", 250, 0},;
                { "wDate",    "D",   8, 0},;
                { "wTime",    "C",   8, 0},;
                { "Stack",    "C", 200, 0},;
                { "userName", "C",  15, 0}},;
               {}, 0}
return arr

funcDef errPath with newPath:CU
return basePath( errInfo(), newPath)

funcDef errOpen
return baseMOpen( errInfo())

funcDef errXOpen
return baseXOpen( errInfo() )

funcDef errClose
return baseMClose( errInfo())

funcDef errLog with str:CA, mustSay:LU, msgB:LU ;
  local i, Stack := "", Sel, S, inCrc, curCRC := 0, key, rV

   DEFAULT mustSay TO .T.
   DEFAULT msgB    TO .F.
   if mustSay
     if msgB
       messageB( str )
     else
       errOK( str)
     endIf
   endIf
   S := Str
   if valType( Str) == "A"
      S := ""; for i := 1 to len( str) do S += Str[ i] + endl()
   endif
   i := 1
   while !empty( procName( i))
     Stack += procName( i) + "(" + nStr( procLine( i++)) + ")" + endl()
   enddo
   Sel := Select()
   if !file( errInfo()[ 1]) then baseCreate( errInfo())
   inCrc := ReadCrc( errInfo())
   if inCrc < 0
      errOK( "Поврежден файл протокола. Немедленно обратитесь на филиал!")
      return NIL
   endif
   if baseNSOpen( errInfo())
      // inCrc  := ReadCrc( errInfo())
      curCrc := 0
      key    := getCrcKey()
      if ERRLOG ->( rV := mAppend())
         ERRLOG ->Str      := EnCode( S,            key)
         ERRLOG ->wDate    := EnCode( setCurDate(), key)
         ERRLOG ->wTime    := EnCode( time(),       key)
         ERRLOG ->Stack    := EnCode( Stack,        key)
         ERRLOG ->userName := EnCode( userName(),   key)
         curCrc            := Crc( ERRLOG ->Str  ) + Crc( ERRLOG ->wDate) + ;
                              Crc( ERRLOG ->wTime) + Crc( ERRLOG ->Stack) + ;
                              Crc( ERRLOG ->userName)
         ERRLOG ->( netUnLock())
      endif
      baseMClose( errInfo())
      if rV then WriteCrc( errInfo(), curCrc + inCrc, key)
   endif
   select (Sel) 
return NIL
/*
funcDef CrcRec local crc := 0, i
   for i := 1 to fCount() do crc += Crc( fieldGet( i))
return crc
*/
funcDef getCrcKey local key
 key := confOtdCode()
 key := right( key, 1) + subStr( key, 3, 1) + subStr( key, 2, 1) + left( key, 1)
 key := int( val( key) % 256)
return key

/*
funcDef ReadCrc with dbInfo:A ;
   local InCrc := 0, RealCrc := 0, cBuffer := space( 20), hFile

hFile := FOPEN( dbInfo[ 1], FO_READ + FO_SHARED)
if fError() <> 0
   messageB( "Не открыть базу")
   return -1
else
   fSeek( hFile, 12, FS_SET)
   if fRead( hFile, @cBuffer, 20) == 20
      cBuffer := allTrim( cBuffer)
      if empty( cBuffer)
         InCrc := 0
      else
         InCrc := val( allTrim( cBuffer))
      endif
   endif
   fClose( hFile)
endif
return InCrc

//
// проверка CRC. Возвращаемые значения
// 0 - OK
// 1 - не удалось открыть базу
// 2 - база взломана
// 3 - база не зашифрована
//
funcDef CheckCrc with baseDefs:A ;
   local rV := 0, InCrc := 0, RealCrc := 0,;
         cBuffer := space(20), i := 0, hFile

statStartUp( "Проверка CRC")
hFile := fOpen( baseDefs[1], FO_READ + FO_SHARED)

if fError() <> 0
   statExit()
   return 1     
endif
fSeek( hFile, 12, FS_SET)
if fRead( hFile, @cBuffer, 20) == 20
   cBuffer := allTrim( cBuffer)
   if empty( cBuffer)
      InCrc := 0
   else
      InCrc := Val(alltrim(cBuffer))
   endif
endif
fClose( hFile)
statProc()
if InCrc > 0
   ( baseDefs[ 2]) ->( baseSavePos())
   ( baseDefs[ 2]) ->( dbGoTop())
   while ( baseDefs[ 2]) ->( !Eof())
     for i := 1 to Len( baseDefs[ 3])
         RealCrc := RealCrc + Crc(( baseDefs[ 2]) -> ( fieldGet( i)))
     next
     ( baseDefs[ 2]) ->( dbSkip())
     statProc()
   enddo
   ( baseDefs[ 2]) ->( baseRestPos())   
   if InCrc <> RealCrc then rV := 2
else
   rV := 3
endif
statExit()
return rV
*/

