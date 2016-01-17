*==============================================================================
*
*                     Программа : tempCreate.prg
*                     Файл      : tempCreate.prg
*                     Автор     : Цейтлин П.М.
*
*------------------------------------------------------------------------------
#include "clippExt.ch"
#include "fileio.ch"
#define DBI_NAME    1
#define DBI_ALIAS   2
#define DBI_STRUCT  3
#define DBI_INDEXES 4
#define DBI_APPENDS 5
#define DBI_NOCHECK 6

#define NTX_NAME    1
#define NTX_FORMULA 2
#define NTX_UNIQUE  3

#define setMultiTag() .F.


funcDef tempCreate with dbInfo:A ;
   local dbName, dbAlias, dbStruct, rV := .F., path, ext, flagHandle

   basePrep( dbInfo)
   path := getFPath( dbInfo[ 1])
   ext  := getFExt(  dbInfo[ 1])

if ( flagHandle := flagOpen( path)) <> - 1

   dbInfo[ 1] := newFileName( path, getTmpName( path), ext)
   dbName   := dbInfo[ DBI_NAME   ]
   dbAlias  := dbInfo[ DBI_ALIAS  ]
   dbStruct := dbInfo[ DBI_STRUCT ]
   messageStartUp( "Создаю временную базу " + dbName)
   if mDeleteBase( dbName, dbAlias)
      if mCreate(  dbName, dbStruct)
         inkey( 0.5)
         if tempIndex( dbInfo)
            rV = .T.
         endIf
      endIf
   endIf
   messageExit()
   flagClose( flagHandle)
endif
return rV


static funcDef tempIndex with dbInfo:A ;
   local dbName, dbAlias, dbStruct, ntxInfo, unique, rV, i, path, ext
   basePrep( dbInfo)
   dbName   := dbInfo[ DBI_NAME   ]
   dbAlias  := dbInfo[ DBI_ALIAS  ]
   dbStruct := dbInfo[ DBI_STRUCT ]
   ntxInfo  := dbInfo[ DBI_INDEXES]
   rV       := .F.
   if len( ntxInfo) = 0
      rV := .T.
   else
      messageStartUp( "Создаю индексы временной базы " + dbName)
      if mCheckBase( dbName, dbStruct)
         if mUse( dbName, dbAlias, .T.)
            if mFLock()
               for i=1 to len( ntxInfo)

                  path := getFPath( ntxInfo[ i][ 1])
                  ext  := getFExt(  ntxInfo[ i][ 1])

                  ntxInfo[ i][ 1] := newFileName( path, getTmpName( path), ext)
                  if len( ntxInfo[ i]) < 3
                     unique := NIL
                  else
                     unique := ntxInfo[ i][ NTX_UNIQUE]
                  endIf
                  if .NOT.setMultiTag()
                     if .NOT.mIndex( ntxInfo[ i][ NTX_NAME],    ;
                                     ntxInfo[ i][ NTX_FORMULA], ;
                                     unique)
                        rV := .F.
                        exit
                     else
                        rV := .T.
                     endIf
                  else
                     if .NOT.mIndex( newFExt( dbName, indexExt()), ;
                                     ntxInfo[ i][ NTX_FORMULA],    ;
                                     unique,                       ;
                                     getFName( ntxInfo[ i][ NTX_NAME]))
                        rV := .F.
                        exit
                     else
                        rV := .T.
                     endIf
                  endIf
               endFor
            endIf
            netClose()
         endIf
      endIf
      messageExit()
   endIf
return rV

funcDef flagOpen with path:C local flagName, hFlag
   flagName := newFPath( ".\busy.$$$", path)
   while ( hFlag := setFlag( flagName)) = -1
      exit if !yesNo( { "Не могу заблокировать файл !",;
                        "Попробовать еще раз ?"})
   enddo
return hFlag

funcDef flagClose with hFlag:N
return clearFlag( hFlag)


static funcDef setFlag with flagName:C local rV, fName, fHandle, path
  path := getFPath( flagName)
  if isFlag( flagName ) then return -1
  fclose( fcreate( newFPath( flagName, path), FC_NORMAL ))
return fopen( newFPath( flagName, path), FO_EXCLUSIVE )

static funcDef isFlag with flagName:C local rV, fName, fHandle
  // fName := flagsPath() + flagName
  fName := flagName
  rV    := file( fName )
  if !rV then return .F.
  rV := ( fHandle := fopen( fName, FO_EXCLUSIVE )) == -1
  if !rV then fclose( fHandle )
return rV

static funcDef clearFlag with flagHandle:N
return fclose( flagHandle )
