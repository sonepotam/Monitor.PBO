//
//  ����� �ࠢ����� ����� card718
//

//
//  << work >>
//

//
//  �㭪樨 �����
//
//  cardOpen() ->T/F  -  ����⨥ ���� card � ࠧ���塞�� ०���
//  cardXOpen()->T/F  -     ---  //  ---    � ��᪫���� ०���
//  cardClose()       -  �����⨥ ����
//  cardInfo()        -  ������� ���� card
//  cardBInfo()       -  ���ଠ�� �� �᭮��� �����
//  cardInit()        -  ���樠������ ��� ����筮�� ���
//  cardBOpen()       -  ����⨥ �᭮���� ��� 
//  cardBXOpen()      -  � �� � ��᪫���� ०���
//  cardBClose()      -  �����⨥ �᭮���� ���
//
#include "clippExt.ch"
#include "common.ch"

//
//  ����⨥ ����
//
funcDef cardOpen()
return baseMOpen( cardInfo())


//
//  ����⨥ ���� � ������� ०���
//
Function cardXOpen()
return baseMXOpen( cardInfo() )

//
// �����⨥ ����
//
Function cardClose()
return baseMClose( cardInfo() )

//
//  ���ᠭ�� ��������
//
Function cardInfo()
Static aArr := { ".\card718.dbf", "card",;
                 {{ "schet", "C", 20, 0 } ,;   // ���
                  { "sumF",  "N", 15, 0 } ,;   // 䨪�஢����� �㬬�
                  { "_sumF", "N", 15, 0 } ,;
                  { "datF",  "D",  8, 0 } ,;   // � ��� �� ���᫥���
                  { "sum1",  "N", 15, 0 } ,;   // �㬬�, �����. � ��௫���
                  { "prc1",  "N",  6, 2 } ,;   // ��業�� � ��௫���
                  { "dat1",  "D",  8, 0 } ,;   // ��� ����᫥���
                  { "sum2",  "N", 15, 0 } ,;   // �㬬�, �����. � �६��
                  { "prc2",  "N",  6, 2 } ,;   // % � �६��
                  { "dat2",  "D",  8, 0 } ,;   // ��� ����᫥���
                  { "sum3",  "N", 15, 0 } ,;   // �㬬�,�����. �� ���.������
                  { "prc3",  "N",  6, 2 } ,;   // % � ������
                  { "dat3",  "D",  8, 0 } ,;
                  { "_datF", "D",  8, 0 } ,;
                  { "_dat1", "D",  8, 0 } ,;
                  { "_dat2", "D",  8, 0 } ,;
                  { "_dat3", "D",  8, 0 } ,;
                  { "crdType","N", 1, 0 } ,;
                  { "crdPay","N",  1, 0 } ,;
                  { "crdOpen","N", 1, 0 } ,;
                  { "dat_kart","D",8, 0 }},;   // ���
                 { { "card718", "schet" } },;
                  1}

return aArr
