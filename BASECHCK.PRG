*=============================================================================
*
*                         �ணࠬ��  : MON.EXE
*                         ����       : baseCheck.prg
*                         ����      : ���⫨� �.�.
*
*-----------------------------------------------------------------------------
*
*
#include "clippExt.ch"
#include "common.ch"

*
* 05-01-2003  TMV
* � ᯨ᮪ ����権, ����� �� ���� ������� � ���� ����⮢,
*  ����祭� �஢���� �� ����. ��㤠� 6031, 6136, 6137 - sayNormalStr()
*  

//
// ���ᠭ�� ���� ��⥩ � �⤥�����
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
// �஢�ઠ ������ 䠩�� ���䨣��樨
//
funcDef conf2Present
return file( conf2Info()[ 1])

funcDef confTemp2
return ".\CHECK"

//
//  �����஢���� ��⠫����
//
funcDef conf2Scan ;
        local i, aDirectory, zipFile, tempPath, str,;
              totObd, totOBK, totKasPlus, totKasMinus, totRest, ptr



totObd := totOBK := totKasPlus := totKasMinus := totRest := 0
if conf2Open()
   tempPath := confTemp2()
   operStartUp( "�����஢���� ��⠫���� �⤥�����", i := 1, CONFIG2 ->( lastRec()))
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

   str += "�" + padR( "�����", 15) + "�" + padC( "", 12)  + "�" +  ;
          str( totobd,  15, 2) + "�" + str( totobk, 15, 2) + "�" +  ;
          str( totRest, 15, 2) + "�" + endl() + ;
         "������������������������������������������������������������������������������"


   operExit()
   conf2Close()

   editMemo( str)
   if yesNo( "��ᯥ���� ?") 
      i := 2
      if getUp( 5, 5, {{ "������ ���-�� ����� ��� ����", block( i) }})
         for ptr := 1 to i do printAOrder( { str})
      endif
   endif

endif
return NIL


static funcDef sayStrHeader local str

str := ;
"                              ���ઠ ������ �� �⤥�����                     " + endl() + ;
"����������������������������������������������������������������������������Ŀ" + endl() + ;
"� �⤥�����     �   ����     �   �।�⮢�  �   ����⮢�   �  ���⮪ ��   �" + endl() + ;
"�               �            � ������ �� 33 � ������ �� 33 �      ����    �" + endl() + ;
"����������������������������������������������������������������������������Ĵ" + endl()

return str


static funcDef sayErrorStr with aDirectory:A, str:C
str += "�" + CONFIG2 ->otdName + "�" + ;
       padC( "� ��⠫��� " + rTrim( CONFIG2 ->path) + ;
       " ᫨誮� ����� 䠩���", 60) + "�" + endl()
return NIL

static funcDef sayEmptyStr with aDirectory:A, str:C
str += "�" + padC( "��� 䠩�", 60) + "�" + endl()
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

str += "�" + padR( otdName, 15) + "�" + padC( aDirectory[ 1, 1], 12) + "�" +  ;
       str( obd33, 15, 2)       + "�" + str( obk33, 15, 2)     + "�" +  ;
       str( prepKassa, 15, 2)   + "�" + endl()

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