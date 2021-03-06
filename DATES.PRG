/***************************************************************************

      �ணࠬ�� : MON
      �����    : DATES ( P������ �஢�ન ��� )
      ����      : DATES.PRG
      ����     : �������� �.�.

      Off-line ������ ������ ���㦨����� 䨧��᪨� ��� ��� "�����"

***************************************************************************/

#include "clippext.ch"

#include "common.ch"
#include "inkey.ch"
#include "box.ch"

autoFunction setValue month1 init;
                 {"  ﭢ���  ","  䥢ࠫ� ","   ����  ","  ��५�  ",;
                  "   ���    ","   ���   ","   ���   "," ������  ",;
                  " ᥭ���� ","  ������ ","   ����� "," �������  "}

autoFunction setValue month2 init;
                 {"  ﭢ���  ","  䥢ࠫ� ","   ����   ","  ��५�  ",;
                  "   ���    ","   ���   ","   ���   "," ������   ",;
                  " ᥭ���� ","  ������ ","   ����� "," �������  "}

funcDef monthYear with dDate:D
return ( allTrim( month2()[ month( dDate )]) + " " + nStr( year( dDate )) + " �." )

funcDef monthID with dDate:D
return padL( nStr( month( dDate )), 2, "0" ) + subStr( nStr( year( dDate )), 3, 2 )

funcDef nMonth with dDate:D
return strTran( str( month( dDate ), 2, 0 ), " ", "0" )

funcDef nYear with date:D
return str( year( date ), 4, 0 )

funcDef shortDate with  dateVal:D;
                  local saveCentury := __setCentury(),;
                        dateStr
  __setCentury( .F. )
  set date german
  dateStr := dtoc( dateVal )
  set date british
  __setCentury( saveCentury )
return dateStr

funcDef wmonth with  date:D
return allTrim( month1()[ month( date )])

funcDef wordDate with date:D local s
  s := dtoc( date )
  s := subStr( s, 1, 2 ) + " " +;
       wmonth( date ) + " " + subStr( s, 7, 4 ) + " �."
return s

funcDef yearLength with dDate:D
return if( empty( mod( year( dDate ), 4 )), 366, 365 )

funcDef fullYear with year:CN
  if ISCHARACTER( year )
    year := val( year )
  endIf
  if year > 80 
    year := year + 1900
  else
    year := year + 2000
  endIf
return nStr( year )
