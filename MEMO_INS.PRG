parameter sFile
sWait='������ ��ࠢ���.'
aFile:={}
sMask = diskname() + ':\' + curdir() + "\printz\*"
sPrintZ :='PRINTZ.DBF'
i:=0
?
?
?
?
?
?
?
if !empty(sFile)
	if upper(sFile) == '/F'
		sFile = '        '
		?'������ ��� 䠩�� � 蠡�����'
		?
		@ row()-1, 30 get sFile
		read
		if lastkey() != 13 .Or. empty(sFile)
			return
		endif
	elseif at( '?', upper(sFile)) > 0
		?'�ணࠬ�� ���४�஢�� 蠡����� ��� "������"'
		?
		?'��ଠ� ����᪠ - MEMO_INS [ /? | /F | <��� 䠩��> ]'
		?'  /?          - �맮� �⮩ �ࠢ��'
		?'  /F          - �㤥� ����襭� ��� 䠩��-蠡����'
		?'  <��� 䠩��> - ��� 䠩�� 蠡����'
		?
		?'�᫨ ��ࠬ��� �� 㪠��� - �������� �� 蠡����, ��� ������ ���� 䠩��'
		?'� ��⠫��� PRINTZ.'
		?'�᫨ 䠩� ���� - 蠡��� �㤥� �������� � ����.'
		?
		?'�����-蠡���� ������ �ᯮ�������� � ��⠫��� PRINTZ, �'
		?'⠡��� PRINTZ.DBF - � ��⠫���, 㪠������ � CONFIG.DBF --> PathOD.'
		?
		?
		?
		?
		clear typeahead
		inkey(0)
		return
	endif
	aadd(aFile, {upper(allTrim(sFile)), 0})
else
	?'����� �㤥� �஢����� ��⮬���᪮� ���������� 蠡�����.'
	?'��� ��砫� ���������� ������ <ENTER>, ��� ���� ������� ��� ��室�.'
	?
	?
	?
	?
	sound(900,10)
	clear typeahead
	if inkey(0)!=13
		?
		?'���������� �⬥���� !'
		?
		sound(900,10)
		inkey(1)
		return
	endif
	?'���� ��⮬���᪮� ���������� 蠡�����...'
	aFile=directory(sMask)
endif
?
Use config Shared
if neterr()
	?'�訡�� ������ CONFIG.DBF !'
	?
	sound(900,10)
	inkey(5)
	return
endif
sPrintZ = allTrim(PATHOD) + sPrintZ
Use
if left(sPrintZ, 1) == '.'
	sPrintZ = SubStr( sPrintZ, 2)
endif
if left(sPrintZ, 1) == '\'
	sPrintZ = SubStr( sPrintZ, 2)
endif
if !file(sPrintZ)
	?'�� ������ 䠩� ' + sPrintZ + ' !'
	?
	sound(900, 10)
	inkey(5)
	return
endif
Use &sPrintZ Exclusive
for i=1 to len(aFile)
	sWait=' - 蠡��� ��ࠢ���.'
	sFile = aFile[i,1]
	?Padr(sFile, 10, ' ')
	locate for alltrim(name)==sFile
	b = found()
	if !file("printz\"+sFile)
		loop
	endif
	s = memoread("printz\"+sFile)
	if len(aFile) == 1
		Cls
		memoedit(s, 0,0,20,79,.F.)
		Cls
		?
		?' �⮡� ��࠭����� 蠡��� ������ <ENTER>, ��� ���� ������� ��� ��室�.'
		?
		sound(900,10)
		clear typeahead
		if inkey(0)!=13
			sWait=' - 蠡��� �� ��ࠢ���.'
			i = 5
		endif
	endif
	if !b
		dbappend()
		replace name with sFile
		sWait=' - 蠡��� ��������.'
	endif
	repl DATA with s
	??sWait
	if inkey(.1) == 27
		exit
	endif
next
?
if i == Len(aFile) + 1
	?'���������� �����祭�....'
else
	?'���������� ��ࢠ��....'
endif
?
sound(900, 10)
inkey(2)
Use