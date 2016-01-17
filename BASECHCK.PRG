*=============================================================================
*
*                         à®£à ¬¬   : MON.EXE
*                         ” ©«       : baseCheck.prg
*                         €¢â®à      : –¥©â«¨­ .Œ.
*
*-----------------------------------------------------------------------------
*
*
#include "clippExt.ch"
#include "common.ch"

*
* 05-01-2003  TMV
* ‚ á¯¨á®ª ®¯¥à æ¨©, ª®â®àë¥ ­¥ ­ ¤® ¢ª«îç âì ¢ à áç¥â ®¡®à®â®¢,
*  ¢ª«îç¥­ë ¯à®¢®¤ª¨ ¯® ¡¥§­. ááã¤ ¬ 6031, 6136, 6137 - sayNormalStr()
*  

//
// ®¯¨á ­¨¥ ¡ §ë ¯ãâ¥© ª ®â¤¥«¥­¨ï¬
//
func conf2Info
static dbInfo := { "CONFIG2.DBF", "CONFIG2",;
                   {{ "path",     "C", 50, 0}  ,;
                    { "otdName",  "C", 15, 0}  ,;
                    { "otdNumb",  "C",  3, 0}} ,;
                   {}, 0, .T.}
return dbInfo

funcDef conf2Path with path:CU
return basePath( conf2Info(), path)

funcDef conf2Open
return baseMOpen( conf2Info())

funcDef conf2Close
return baseMClose( conf2Info())
//
// ¯à®¢¥àª  ­ «¨ç¨ï ä ©«  ª®­ä¨£ãà æ¨¨
//
funcDef conf2Present
return file( conf2Info()[ 1])

funcDef confTemp2
return ".\CHECK"

//
//  ‘ª ­¨à®¢ ­¨¥ ª â «®£®¢
//
funcDef conf2Scan ;
        local i, aDirectory, zipFile, tempPath, str,;
              totObd, totOBK, totKasPlus, totKasMinus, totRest, ptr



totObd := totOBK := totKasPlus := totKasMinus := totRest := 0
if conf2Open()
   tempPath := confTemp2()
   operStartUp( "‘ª ­¨à®¢ ­¨¥ ª â «®£®¢ ®â¤¥«¥­¨©", i := 1, CONFIG2 ->( lastRec()))
   str := sayStrHeader()
   while .T.

         if CONFIG2 ->( !emptyRec())

            clearDir( confTemp2())
            prepDir( newFPath( "*.zip", CONFIG2 ->path))
            aDirectory := Directory( newFPath( "s?" + CONFIG2 ->otdNumb + ;
                                     "???.dbf", confTemp2()))
            do case
               case len( aDirectory) = 0 do sayEmptyStr(  aDirectory, @str)
               case len( aDirectory) > 1 do sayErrorStr(  aDirectory, @str)
               otherwise
                    sayNormalStr( CONFIG2 ->otdName, aDirectory,  @str,;
                                  @totOBD, @totOBK, @totKasPlus, @totKasMinus, @totRest)
            endcase
            clearDir( confTemp2())
         endif
         operProc( i++)
         exit if CONFIG2 ->( !netSkipDown())
   enddo

   str += "³" + padR( "ˆ’ŽƒŽ", 15) + "³" + padC( "", 12)  + "³" +  ;
          str( totobd,  15, 2) + "³" + str( totobk, 15, 2) + "³" +  ;
          str( totRest, 15, 2) + "³" + endl() + ;
         "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"


   operExit()
   conf2Close()

   editMemo( str)
   if yesNo( " á¯¥ç â âì ?") 
      i := 2
      if getUp( 5, 5, {{ "‚¢¥¤¨â¥ ª®«-¢® ª®¯¨© ¤«ï ¯¥ç â¨", block( i) }})
         for ptr := 1 to i do printAOrder( { str})
      endif
   endif

endif
return NIL


static funcDef sayStrHeader local str

str := ;
"                              ‘¢¥àª  ¤ ­­ëå ¯® ®â¤¥«¥­¨ï¬                     " + endl() + ;
"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" + endl() + ;
"³ Žâ¤¥«¥­¨¥     ³   ” ©«     ³   Šà¥¤¨â®¢ë¥  ³   „¥¡¥â®¢ë¥   ³  Žáâ â®ª ¯®   ³" + endl() + ;
"³               ³            ³ ®¡®à®âë ¯® 33 ³ ®¡®à®âë ¯® 33 ³      ª áá¥    ³" + endl() + ;
"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´" + endl()

return str


static funcDef sayErrorStr with aDirectory:A, str:C
str += "³" + CONFIG2 ->otdName + "³" + ;
       padC( "‚ ª â «®£¥ " + rTrim( CONFIG2 ->path) + ;
       " á«¨èª®¬ ¬­®£® ä ©«®¢", 60) + "³" + endl()
return NIL

static funcDef sayEmptyStr with aDirectory:A, str:C
str += "³" + padC( "¥â ä ©«", 60) + "³" + endl()
return NIL


static funcDef sayNormalStr ;
       with  otdName:C, aDirectory:A, str:C, totOBD:N, totOBK:N ,;
             totKasPlus:N, totKasMinus:N, totRest:N              ;
       local fileName, s := select(), obd33, obk33, kasPlus     ,;
             kasMinus, prepKassa, aCode

fileName := newFPath( aDirectory[ 1, 1], confTemp2())

obd33 := obk33 := kasPlus := kasMinus := prepKassa := 0
if file( fileName)
aCode := { opDecode( "1005"), opDecode( "1008"),;
			  opDecode( "6031"), opDecode( "6136"),; // Add by TMV 05-01-2003
			  opDecode( "6137")}                     // Add by TMV 05-01-2003
prepKassa := prepKassa()
use ( fileName) alias CHECK new
while !eof()
      if CHECK ->typeOf < "9000" .AND. ;
         aScan( aCode, {|x| x == CHECK ->typeOf}) == 0
         if CHECK ->summa <0
            obd33 -= CHECK ->summa/ 100
            if left( CHECK ->schet, 4) == " 904"
               kasMinus -= CHECK ->summa/ 100
            endif
         else
            obk33 += CHECK ->summa/ 100
            if left( CHECK ->schet, 4) == " 904"
               kasPlus += CHECK ->summa/ 100
            endif
         endif
      endif
      skip
enddo

close CHECK
select ( s)

totOBD      += obd33
totOBK      += obk33
totKasPlus  += kasPlus
totKasMinus += kasMinus
totRest     += prepKassa
endif

str += "³" + padR( otdName, 15) + "³" + padC( aDirectory[ 1, 1], 12) + "³" +  ;
       str( obd33, 15, 2)       + "³" + str( obk33, 15, 2)     + "³" +  ;
       str( prepKassa, 15, 2)   + "³" + endl()

return NIL


funcDef prepDir with zipFile:C local str, fileName
str := "pkunzip.exe " + zipFile + " " + confTemp2() + " >nul"
runExe( str)
return NIL

funcDef clearDir with path:C local arr
arr := Directory( newFPath( "*.*", path))
aEval( arr, {|x| fErase( newFPath( x[ 1], path)) })
return NIL


funcDef prepKassa local rV := 0, file, aDir, s := select()

aDir := Directory( newFPath( "k?" + CONFIG2 ->otdNumb + "???.dbf", confTemp2()))
if len( aDir) == 1
   file := newFPath( aDir[ 1, 1], confTemp2())
   use ( file) alias KASSA new
   rV := KASSA ->summa/ 100
   select ( s)
   close KASSA
endif
return rV