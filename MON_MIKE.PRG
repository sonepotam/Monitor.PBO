*=============================================================================
*
*                Программа : MON.EXE
*                Файл      : mon_mike.
*                Автор     : Цейтлин П.М.
*
*-----------------------------------------------------------------------------
#include "clippExt.ch"
/**************************************************************************/
//Проверка назначен ли станции район доставки
function MikeChkDistr( inDistrCode)
static isDistr
local rV := .F.
  if isDistr == nil then isDistr := confIsDistr()
  if !isDistr then return .T.
  if thisStation() == '' then return .T.
  if DISTR -> ( netSeek( inDistrCode))
     if DISTR -> Station == thisStation() then rV := .T.
  endif
return rV
 
/*******************************************************************/
// Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Перенос измененных данных в базу для отправки
function MikeBaseSend( baseDefs, fileName)
local outBaseDefs := {}
local rV := .F., fieldDate, fieldNumeric, fieldCharacter, fieldLogical, i
   outBaseDefs := aclone(baseDefs)
   outBaseDefs[1] := fileName   //Full name!!!!
   outBaseDefs[2] := "MIKEALIAS"
   outBaseDefs[4] := {}
   if baseCreate( outBaseDefs)
      if .not. (baseMOpen( outBaseDefs) .and. baseMOpen( baseDefs))
         if baseUsed( outBaseDefs) then baseMClose( outBaseDefs)
         if baseUsed( baseDefs) then baseMClose( baseDefs)
         return rV
      endif
      (baseDefs[2]) -> ( dbGoTop())
      do while .not. (baseDefs[2]) -> ( eof())
         if (baseDefs[2]) -> ledited
            MIKEALIAS -> ( dbAppend())
            if .not. MIKEALIAS -> ( netErr())
               for i := 1 to len( outBaseDefs[3])
//                  MIKEALIAS ->( fieldPut( i, ( baseDefs[ 2]) ->( fieldGet( i))))
                  do case
                     case valType((baseDefs[2]) -> ( fieldGet(i))) == 'C'
                        fieldCharacter := (baseDefs[2]) -> ( fieldGet(i))
                        MIKEALIAS -> (fieldPut(i,fieldCharacter))
                     case valType((baseDefs[2]) -> ( fieldGet(i))) == 'N'
                        fieldNumeric := (baseDefs[2]) -> ( fieldGet(i))
                        MIKEALIAS -> (fieldPut(i,fieldNumeric))
                     case valType((baseDefs[2]) -> ( fieldGet(i))) == 'L'
                        fieldLogical := (baseDefs[2]) -> ( fieldGet(i))
                        MIKEALIAS -> (fieldPut(i,fieldLogical))
                     case valType((baseDefs[2]) -> ( fieldGet(i))) == 'D'
                        fieldDate := (baseDefs[2]) -> ( fieldGet(i))
                        MIKEALIAS -> (fieldPut(i,fieldDate))
                  endcase
               next
            else
               if baseUsed( outBaseDefs) then baseMClose( outBaseDefs)
               if baseUsed( baseDefs) then baseMClose( baseDefs)
               return rV
            endif
         endif
         (baseDefs[2]) -> ( dbskip())
      enddo
      if baseMClose( outBaseDefs) .and. baseMClose( baseDefs) then rV := .T.
   endif
return rV
// Mike !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
/*******************************************************************/

