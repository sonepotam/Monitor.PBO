/********************************************************************/
/*                   Генегация номера договора                      */
/********************************************************************/
FUNCTION GenNum(sNumTreat)
LOCAL iTmp     :=0  //Вспомогательная переменная
LOCAL iCheckSum:=0  //Контрольная сумма
LOCAL sTmpNum  :="" //Самостоятельно вычисленный номер
LOCAL sKey     :="" //Значение контрольного ключа
 //
 //Ставим вместо ключа ноль
 //
 sTmpNum:=LEFT(sNumTreat,8)+"0"+RIGHT(sNumTreat,4)
 //
 //Генерация контрольной суммы
 //
 iCheckSum:=0
 FOR iTmp:=1 TO 6
   iCheckSum+=  VAL(SUBSTR(sTmpNum,2*iTmp-1,1))+;
              3*VAL(SUBSTR(sTmpNum,2*iTmp  ,1))
 ENDFOR
 iCheckSum+=VAL(RIGHT(sTmpNum,1))
 sKey:=nSTR(10-CatMoreTen(iCheckSum))
 //
 //Если не равен то конец
 //
 IF sKey!="10"
    RETURN LEFT(sNumTreat,8)+sKey+RIGHT(sNumTreat,4)
 ENDIF
 //
 //Если ноль - то перерасчет
 //
 iCheckSum:=0
 FOR iTmp:=1 TO 6
   iCheckSum+=3*VAL(SUBSTR(sTmpNum,2*iTmp-1,1))+;
                VAL(SUBSTR(sTmpNum,2*iTmp  ,1))
 ENDFOR
 iCheckSum+=3*VAL(RIGHT(sTmpNum,1))
 sKey:=RIGHT(nSTR(13-CatMoreTen(iCheckSum)),1)
RETURN LEFT(sTmpNum,8)+sKey+RIGHT(sTmpNum,4)
/********************************************************************/
/*              Отбрасывание десяток, сотен и т.д.                  */
/********************************************************************/
STATIC FUNCTION CatMoreTen(iTmp)
LOCAL iRound:=ROUND(iTmp,-1)//Первоначальное округление
 //
 //Корректировка iRound
 //
 IF iRound>iTmp
    iRound-=10
 ENDIF
RETURN iTmp-iRound
