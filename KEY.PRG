/********************************************************************/
/*                   �������� ����� �������                      */
/********************************************************************/
FUNCTION GenNum(sNumTreat)
LOCAL iTmp     :=0  //�ᯮ����⥫쭠� ��६�����
LOCAL iCheckSum:=0  //����஫쭠� �㬬�
LOCAL sTmpNum  :="" //�������⥫쭮 ���᫥��� �����
LOCAL sKey     :="" //���祭�� ����஫쭮�� ����
 //
 //�⠢�� ����� ���� ����
 //
 sTmpNum:=LEFT(sNumTreat,8)+"0"+RIGHT(sNumTreat,4)
 //
 //������� ����஫쭮� �㬬�
 //
 iCheckSum:=0
 FOR iTmp:=1 TO 6
   iCheckSum+=  VAL(SUBSTR(sTmpNum,2*iTmp-1,1))+;
              3*VAL(SUBSTR(sTmpNum,2*iTmp  ,1))
 ENDFOR
 iCheckSum+=VAL(RIGHT(sTmpNum,1))
 sKey:=nSTR(10-CatMoreTen(iCheckSum))
 //
 //�᫨ �� ࠢ�� � �����
 //
 IF sKey!="10"
    RETURN LEFT(sNumTreat,8)+sKey+RIGHT(sNumTreat,4)
 ENDIF
 //
 //�᫨ ���� - � �������
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
/*              ����뢠��� ����⮪, �⥭ � �.�.                  */
/********************************************************************/
STATIC FUNCTION CatMoreTen(iTmp)
LOCAL iRound:=ROUND(iTmp,-1)//��ࢮ��砫쭮� ���㣫����
 //
 //���४�஢�� iRound
 //
 IF iRound>iTmp
    iRound-=10
 ENDIF
RETURN iTmp-iRound
