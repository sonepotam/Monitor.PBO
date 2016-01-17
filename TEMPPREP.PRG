#include "clippExt.ch"
funcDef TempPrep2 with dbInfo:A local path, ext, j
   basePrep( dbInfo)
   //
   // Название временной базы данных
   //
   path := getFPath( dbInfo[ 1])
   ext  := getFExt(  dbInfo[ 1])
   dbInfo[ 1] := newFileName( path, getTmpName( path), ext)
   //
   // Названия индексных файлов
   //
   inkey( 0.2)
   for j=1 to len( dbInfo[ 4])
      path := getFPath( dbInfo[ 4][ j][ 1])
      ext  := getFExt(  dbInfo[ 4][ j][ 1])
      dbInfo[ 4][ j][ 1] := newFileName( path, getTmpName( path), ext)
      inkey( 0.5)
   endFor
return dbInfo
