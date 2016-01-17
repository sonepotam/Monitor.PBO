***************************************
**
**  Закачка данных по счетам
**  из пенсионного фонда
**
**  и некоторые измененные функции
**
**  Mon_conv.prg
**  Автор: Трунов М.В.
**
***************************************
#include "clippext.ch"
#include "common.ch"
#include "mon.ch"

//static ACC_VID_PENS := '15 1  1  1  '

func ConvPfInfo
static dbInfo := { "TT", "CONVERT",;
						{{ "YZEL_SV",     "C", 4, 0},;
						 { "POST_OT",     "C", 3, 0},;
						 { "DATA_RJ",     "D", 8, 0},;
						 { "SUMMA",       "N",10, 0},;
						 { "F_I_O",       "C",40, 0},;
						 { "INDEKC",      "N", 6, 0},;
						 { "GOROD",       "C",20, 0},;
						 { "PROEZD",      "C",20, 0},;
						 { "DOM",         "C", 6, 0},;
						 { "KORP",        "C", 3, 0},;
						 { "KV",          "C", 4, 0},;
						 { "PASP_S1",     "C", 8, 0},;
						 { "PASP_S2",     "C", 8, 0},;
						 { "PASP_NO",     "C",10, 0},;
						 { "PASP_D",      "D", 8, 0},;
						 { "PASP_ATR",    "C",40, 0};
						},;
						{}, 0, .T.}
return dbInfo


procDef impAccFromFond local i := 0, sFile:='', aFile:={}, j:=2, str:='',;
	sCurDir:=Diskname() + ':\' + Curdir(), sDbfDir, aV := {},;
	aError:={'   Ошибки при закачке файлов из ПФ',''}, bConvert := .F.,;
	sMess := ''
sFile=confPathInbox()
sDbfDir = confPathTemp()
if left( sFile, 1 ) == '.' Then sFile =Strtran( alltrim( sFile ), '.', sCurdir, 1,1)
if left( sDbfDir, 1 ) == '.' Then sDbfDir =Strtran( alltrim( sDbfDir ), '.', sCurdir, 1,1)
sCurDir+='\'
sDbfDir += 'PF\'
messageStartUp( "Прием данных из ПФ " + sFile)
aFile = Directory(sFile+'*.zip')
for i=1 to len(aFile)
	j=1
	windowstartup(5,5,9,74,'b/w,w+/b')
	sound(900, 5)
	@ 6,6 say center('Закачать файл '+aFile[i,1] + '?',66)
	@ 8,24 prompt '   Ok   '
	@ 8,48 prompt ' Cancel '
	menu to j
	windowexit()
	if j=1
		bConvert = .T.
      str = "pkunzip.exe -o " + sFile + aFile[i,1] + " " + sDbfDir + ' *.DBF >nul'
      runExe( str)
		aeval( getFileFromPf(sDbfDir), {|x|aadd(aV, x)})
		aeval( Directory(sDbfDir+'*.dbf'), {|x|ferase(sDbfDir+x[1])})
	endif
	str=''
next
if Len(aV) > 0
	aeval( aV, {|x| aadd(aError, x[1] + endl() + '       ' +;
		if( x[2]=0, '- закачка не производилась.','- закачка файла прервана!'))})
	if ascan( aV, {|x| x[2]<0}) = 0
		sMess = 'Прием успешно завершен.'
	else
		sMess = 'Прием завершен с ошибками.'
	endif
   aEval( aError, {|x| str += x + endl()})
/*	windowstartup(5, 1, 20, 78, 'w+/r,n/w')
	sound(900,10)
	achoice(6, 2, 19, 77, aError)
	windowexit()
	*/
else
	if bConvert
		sMess = 'Прием успешно завершен.'
	else
		sMess = 'Прием отменен.'
	endif
endif
if !empty( sMess )
	messageStartUp( sMess )
	sound(900, 5)
	inkey(2)
   if !empty(str) then editMemo( str)
	inkey(2)
	messageExit()
endif
messageExit()
return

funcDef getFileFromPf with sPath:C local aV:={}, I:=0, rV := .T.,;
	bV := .T., iErr, aFile := {}, sFile, aInfo:=ConvPfInfo()
if monOpen()
	aFile := Directory( sPath + '*.DBF')
	ACC->(dbSetOrder(2))
	CLIENTS->(dbSetOrder(2))
	for i=1 to len(aFile)
		sFile = sPath + aFile[i,1]
		aInfo[1]=sFile
		if (bV := baseMOpen( aInfo, .T.))
//			aadd(aV, {'Закачивается файл  ' + sFile, -1})
			iErr = getDataFromPf(sFile)
			if iErr < 0
				aadd(aV, {'Не закачался файл  ' + sFile, -1})
			elseif iErr > 0
				aadd(aV, {'Повторная попытка закачки файла ' + sFile, 0})
			endif
			do case
				case iErr = -1
					aadd(aV, {' - ошибка при определении клиента', -1})
				case iErr = -2
					aadd(aV, {' - ошибка при определении счета клиента', -1})
				case iErr = -3
					aadd(aV, {' - ошибка при занесении операции начисления', -1})
				case iErr = -10
					aadd(aV, {' - структура файла не подходит', -1})
			endcase
		else
			aadd(aV, {'Не открылся файл ' + sFile, -1})
		endif
		if bV then baseMClose(aInfo)
	next
	monClose()
endif
return aV

funcDef getDataFromPf with sFile:C local rV := 0, i := 0, sCode := '',;
	sAccount := '', aInfo := ConvPfInfo()

aeval(aInfo[3], {|x| if(CONVERT->(fieldpos(x[1])) > 0, .T., --rV)})
if rV < 0
	rV = -10
	return rV
endif
rV = 0
operStartUp( "Прием данных из ПФ " + sFile, 0, CONVERT->(lastrec()))
clear typeahead
do while !CONVERT->(eof()) .and. lastkey() != 27
	if empty(( sCode := getCliCode())) then sCode = setNewClient()
	if empty(sCode)
		rV = -1
		exit
	endif
	if empty(( sAccount := getAccount(sCode) )) then sAccount = setNewAccount(sCode)
	if empty(sAccount)
		rV = -2
		exit
	endif
	if  CONVERT->SUMMA > 0
		if ACC->Paym_Date != setCurDate()
			if !setNewTrans( sAccount, CONVERT->SUMMA/100)
				rV = -3
				exit
			endif
		else
			rV = 1
		endif
	endif
	CONVERT->(dbskip())
	operProc(CONVERT->(recno()))
	inkey()
enddo
operExit()
return rV

funcDef getCliCode local rV := '', oldSel:=select(), sName := '',;
	sNameSeek := '', sPas1, sPas2, sNomer
select CLIENTS
sName = alltrim(CONVERT->F_I_O)
sNameSeek = left( sName, at(' ', sName)-1)
dbseek(sNameSeek, .T.)
if alltrim(CLIENTS->Surname) = sNameSeek
	sNomer = alltrim(CONVERT->PASP_NO)
	sPas1 = alltrim(CONVERT->PASP_S1)+alltrim(CONVERT->PASP_S2)
	locate rest for (alltrim(CLIENTS->Surname) +" "+ alltrim(CLIENTS->Name);
		+" "+ alltrim(CLIENTS->sName)) == sName
	do while found()
		if alltrim(CLIENTS->pasNum) == sNomer .and.;
				(alltrim(CLIENTS->pasSer1)+alltrim(CLIENTS->pasSer2)) == sPas1
			rV=CLIENTS->code
			exit
		endif
		dbskip()
		locate rest for (alltrim(CLIENTS->Surname) +" "+ alltrim(CLIENTS->Name);
			+" "+ alltrim(CLIENTS->sName)) == sName
	enddo
endif
select(oldSel)
return rV

funcDef setNewClient local rV := '', sFio, sName, sTmp, oldSel := select()
if cliAppend()
	sTmp = alltrim(CONVERT->F_I_O)
	sFio = left( sTmp, at(' ', sTmp) - 1)
	sTmp = alltrim( substr( sTmp, at(' ', sTmp) + 1, len(sTmp)))
	sName= left( sTmp, at(' ', sTmp) - 1)
	sTmp = alltrim( substr( sTmp, at(' ', sTmp) + 1, len(sTmp)))
   rV = cliNextCode()
	cliCode(rV)
	cliName(sName)
	cliSurname(sFio)
	cliSName(sTmp)
	cliPasSer1(alltrim(CONVERT->PASP_S1))
	cliPasSer2(alltrim(CONVERT->PASP_S2))
	cliPasNum(alltrim(CONVERT->PASP_NO))
	cliPasGiv(alltrim(CONVERT->PASP_ATR) + ' ' +Dtoc(CONVERT->PASP_D))
	cliIndex(alltrim(Str(CONVERT->INDEKC, 6)))
	cliCity(alltrim(CONVERT->GOROD))
	cliStreet(alltrim(CONVERT->PROEZD))
	cliHouse(alltrim(CONVERT->DOM))
	cliBlock(alltrim(CONVERT->KORP))
	cliFlat(alltrim(CONVERT->KV))
	cliBDate(CONVERT->DATA_RJ)
	cliBuffer('01')
   cliUnLock()
endif
return rV

funcDef getAccount with sCode:C local rV := ''
ACC->(dbSetOrder(2))
if ACC->(dbseek(sCode))
	do while ACC->CODE == sCode
		if ACC->PRIZ == ACC_VID_PENS .And. empty(ACC->DatClose)
			rV=ACC->Schet
			exit
		endif
		ACC->(dbskip())
	enddo
endif
return rV

funcDef setNewAccount with sCode:C local rV := '', sBalAcc := '',;
	opened := (select( "VID") > 0 ), opened1 := .T., oldSel := select()
if !opened then opened1 = vidOpen()
if !opened1
	return rV
endif
VID ->( dbSetOrder( 1))
if VID->(dbseek(ACC_VID_PENS))
	sBalAcc = VID->balSchet
	if accNewSchet(sBalAcc, setCurDate(), ACC_VID_PENS)
		ACC ->code := sCode
		ACC ->priz := ACC_VID_PENS
		accUnLock()
		rV=ACC->Schet
	endif
endif
if !opened then vidClose()
select( oldSel )
return rV

funcDef setNewTrans with sAccount:C, mnSumma:N local rV :=.F., theOrder
theOrder = ordIncIn( setCurDate())
if (rV := sumWrite( sAccount, mnSumma, setCurDate(), '5002', theOrder))
	if accRLock()
		ACC->Paym_Date := setCurDate()
//		ACC->Rest := ACC->Rest + mnSumma * 100
		ACC->(dbUnLock())
		rV = .T.
	endif
endif
/*
ACC->(dbSetOrder(1))
if ACC->(dbseek(sAccount))
	if accRLock()
		if ACC->Paym_Date != setCurDate()
			ACC->Rest := Acc->Rest + mnSumma
			ACC->Paym_Date := setCurDate()
			ACC->LEdited := .F.
			ACC->(dbUnLock())
		endif
		rV = .T.
	endif
endif
*/
return rV

// Дата в формате DD mmmm YYYY
//
funcDef rDateFull with dDate:D local sDate := rDate(dDate)
if at(' ', sDate) < 3 then sDate = '0' + sDate
return sDate