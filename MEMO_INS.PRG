parameter sFile
sWait='Шаблон исправлен.'
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
		?'Введите имя файла с шаблоном'
		?
		@ row()-1, 30 get sFile
		read
		if lastkey() != 13 .Or. empty(sFile)
			return
		endif
	elseif at( '?', upper(sFile)) > 0
		?'Программа корректировки шаблонов для "Монитор"'
		?
		?'Формат запуска - MEMO_INS [ /? | /F | <имя файла> ]'
		?'  /?          - вызов этой справки'
		?'  /F          - будет запрошено имя файла-шаблона'
		?'  <имя файла> - имя файла шаблона'
		?
		?'Если параметр не указан - обновяися все шаблоны, для которых есть файлы'
		?'в каталоге PRINTZ.'
		?'Если файл новый - шаблон будет добавлен в базу.'
		?
		?'Файлы-шаблоны должны располагаться в каталоге PRINTZ, а'
		?'таблица PRINTZ.DBF - в каталоге, указанном в CONFIG.DBF --> PathOD.'
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
	?'Сейчас будет проведено автоматическое обновление шаблонов.'
	?'Для начала обновление нажмите <ENTER>, или дргую клавишу для выхода.'
	?
	?
	?
	?
	sound(900,10)
	clear typeahead
	if inkey(0)!=13
		?
		?'Обновление отменено !'
		?
		sound(900,10)
		inkey(1)
		return
	endif
	?'Идет автоматическое обновление шаблонов...'
	aFile=directory(sMask)
endif
?
Use config Shared
if neterr()
	?'Ошибка открытия CONFIG.DBF !'
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
	?'Не найден файл ' + sPrintZ + ' !'
	?
	sound(900, 10)
	inkey(5)
	return
endif
Use &sPrintZ Exclusive
for i=1 to len(aFile)
	sWait=' - шаблон исправлен.'
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
		?' Чтобы сохраненить шаблон нажмите <ENTER>, или дргую клавишу для выхода.'
		?
		sound(900,10)
		clear typeahead
		if inkey(0)!=13
			sWait=' - шаблон НЕ исправлен.'
			i = 5
		endif
	endif
	if !b
		dbappend()
		replace name with sFile
		sWait=' - шаблон добавлен.'
	endif
	repl DATA with s
	??sWait
	if inkey(.1) == 27
		exit
	endif
next
?
if i == Len(aFile) + 1
	?'Обновление закончено....'
else
	?'Обновление прервано....'
endif
?
sound(900, 10)
inkey(2)
Use