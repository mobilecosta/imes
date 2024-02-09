#INCLUDE "SIGACUS.CH" 
#INCLUDE "PROTHEUS.CH"
#DEFINE PULALINHA CHR(13)+CHR(10)
/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o   ³ a440F4   ³ Autor ³ Gilson do Nascimento  ³ Data ³ 29/04/94 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o³ Faz a consulta ao controle de Poder Terceiros.             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe  ³a440F4(a,b,c,ExpC1,ExpC2,ExpC3,ExpC4,ExpC5,ExpC6,ExpC7,ExpC8³±±
±±ÃÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametro³ a,b,c = parametros padroes quando utiliza-se o Set Key     ³±±
±±³         ³ ExpC1 = Arquivo a pesquisar no SX3                         ³±±
±±³         ³ ExpC2 = Variavel que sera usada para o Seek                ³±±
±±³         ³ ExpC3 = Nome do campo em o cursor deve estar posicionado   ³±±
±±³         ³         para ativar esta rotina.                           ³±±
±±³         ³ ExpC4 = Se rotina de Entrada ou Saida                      ³±±
±±³         ³ ExpC5 = Codigo do Cliente / Fornecedor                     ³±±
±±³         ³ ExpC6 = Loja                                               ³±±
±±³         ³ ExpC7 = Codigo da Tes                                      ³±±
±±³         ³ ExpC8 = Tipo da Nota                                       ³±±
±±ÃÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso     ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function A440F4(cAlias,cCodPRD,cLocal,cCampoF3,cESx,cCliFor,cLoja,lAltera,lRecebto,nRecSD2,cTpCliFor,aNFOrigBen,lRetBenef)

Local nSaldo   := 0,;
nPosNNF  := 0,;
nPosSER  := 0,;
nPosQTD  := 0,;
nPosID6  := 0,;
nPosPRC  := 0,;
nPosLoc  := 0,;
nPosLote := 0,;
nPosSLote:= 0,;
nPosDtVld:= 0,;
nPosPoten:= 0,;
nQtdPed  := 0,;
nPosItOri:= 0,;
nPProvEnt:= 0,;
nPNumSeri:= 0,;
nPLocaliz:= 0,;
cProvEnt := ""

LOCAL nSavOrdSD2:= SD2->(IndexOrd()),;
nPosAliqIcm		:= 0

Local nIndex,nPosEl,cCampo
Local nOption  := 0,nRet:=0, nOAT
Local cSeek:=" "
Local aArrayF4 := {},;
aAuxArF4 := {},;
aQtTamX3 := {},;
aProvEnt := {}

Local nHdl  := Nil,;
oDlg  := Nil,;
oQual := Nil

Local cTipoLote := "",;
cLoteCtl  := "",;
cItemOri  := "",;
cNumLote  := "",;
cSeekLote := "",;
cSeekSD2  := "",;
cNumSeri  := "",;
cLocaliz  := "",;
dDtValid  := Ctod(""),;  		// Sergio Fuzinaka - 05.11.01
nPotencia := 0

Local aAreaSD1:={}
Local aAreaSD2:={}
Default aNFOrigBen := {}
Default lRetBenef  := .F.

lRecebto	:= If( (lRecebto == NIL),.F.,lRecebto )

If !Empty( cCodPRD ) .And. (AllTrim( Upper( cCampoF3 ) ) == "B6_PRODUTO")

	cTipoLote:=If(Rastro(cCodPRD),If(Rastro(cCodPRD,"S"),"S","L"),"N")

	nHdl := GetFocus()

	For nIndex := 1 To Len( aHeader )

		cCampo := SubStr( AllTrim( aHeader[ nIndex,2 ] ),3 )

		Do Case
			Case  (cESx == "D") .And. (cCampo == "_PRCVEN")
				nPosPRC := nIndex

			Case !(cESx == "D") .And. (cCampo == "_VUNIT")
				nPosPRC := nIndex


			Case !(cESx == "D") .And. (cCampo == "_PICM")
				nPosAliqIcm := nIndex

			Case (cCampo == "_NFORI")
				nPosNNF := nIndex

			Case cCampo == "_SERIE"
				nPosSER := nIndex

			Case cCampo == "_SERIORI"
				nPosSER := nIndex

			Case cCampo == "_ITEMORI"
				nPosItOri:= nIndex

			Case cCampo == "_IDENTB6"
				nPosID6 := nIndex

			Case cCampo == "_QUANT"
				nPosQTD := nIndex

			Case cCampo == "_QTDVEN"
				nPosQTD := nIndex

			Case cCampo == "_LOCAL"
				nPosLoc := nIndex

			Case cCampo == "_LOTECTL"
				nPosLote:= nIndex

			Case cCampo == "_NUMLOTE"
				nPosSLote:= nIndex

			Case cCampo == "_DTVALID"
				nPosDtVld:= nIndex

			Case cCampo == "_POTENCI"
				nPosPoten:= nIndex

			Case cCampo == "_PROVENT"
				nPProvEnt:= nIndex

			Case cCampo == "_NUMSERI"
				nPNumSeri:= nIndex

			Case cCampo == "_LOCALIZ"
				nPLocaliz:= nIndex

		EndCase
	Next

	If IsTriangular()
		cSeek:=cCodPrd
	Else
		cSeek:=(cCodPrd+cClifor+cLoja+"R")
	Endif

	dbSelectArea(cAlias)
	dbSetOrder(2)

	If dbSeek( xFilial( cAlias )+cSeek )

		If Type("__cExpF4") != "C"
			__cExpF4 := ".T."
		Endif

		aQtTamX3 := TamSX3( "B6_QUANT" )

		While !Eof() .And. ;
			(xFilial(cAlias)+cSeek == If(IsTriangular(),B6_FILIAL+B6_PRODUTO,B6_FILIAL+B6_PRODUTO+B6_CLIFOR+B6_LOJA+B6_PODER3))

			// BOPS - Verifica se trata-se de cliente ou fornecedor para
			// prevenir casos onde o codigo de cliente e fornecedor sao iguais
			// e os dois tem poder de terceiros
			If Valtype(cTpCliFor) == "C" .And. !Empty(cTpClifor) .And. cTpCliFor # B6_TPCF
				dbSkip()
				Loop
			EndIf

			If &__cExpF4

				nQtdPed:=0

				If ((cESx == "E") .And. (SB6->B6_TIPO == "E")) .Or. (cESx != "E" .And. B6_TIPO == "D")

					For nIndex := 1 To Len( aCols )
						If !lRetBenef .And. !aCols[nIndex][Len(aCols[nIndex])].And.aCols[ nIndex,nPosID6 ] == SB6->B6_IDENT .And.(n # nIndex)
							nQtdPed += aCols[ nIndex,nPosQtd ]
						EndIf
					Next nIndex

					If lAltera .And. !Empty(SC5->C5_NOTA) // este pedido tem itens faturados
						dbSelectArea("SC6")
						dbSetOrder(2)
						dbseek(xFilial("SC6")+SB6->B6_PRODUTO+SC5->C5_NUM)
						While xFilial("SC6")+SB6->B6_PRODUTO+SC5->C5_NUM == C6_FILIAL+C6_PRODUTO+C6_NUM .And. !Eof()
							If SB6->(B6_DOC+B6_SERIE) == C6_NFORI+C6_SERIORI
								nQtdPed -= C6_QTDENT
							EndIf
							dbSkip()
						EndDo
						dbSetOrder(1)
						dbSelectArea("SB6")
					EndIf
					If lAltera .And. !lRecebto
						If ((nPosEl := AScan( aQtdLib,{ | aEl | aEl[ 2 ] == SB6->B6_IDENT } )) # 0)
							nSaldo   := B6_SALDO - B6_QULIB + aQtdLib[ nPosEl,3 ]+aQtdLib[ nPosEl,4 ] -  nQtdPed
						Else
							nSaldo   := B6_SALDO - B6_QULIB - nQtdPed
						Endif
					Else
						nSaldo := B6_SALDO - B6_QULIB - nQtdPed
					EndIf

					If (nSaldo > 0)
						cItemOri	:=	""
						// Pesquisa rastreabilidade
						If cTipoLote $ "SL" .Or. cPaisLoc <> "BRA"
							If SB6->B6_TES <= "500"
								dbSelectArea("SD1")
								aAreaSD1:=GetArea()
								dbSetOrder(1)
								cSeekLote:=xFilial("SD1")+SB6->B6_DOC+SB6->B6_SERIE+SB6->B6_CLIFOR+SB6->B6_LOJA+SB6->B6_PRODUTO
								dbSeek(cSeekLote)
								cLoteCtl:=CriaVar("D1_LOTECTL");cNumLote:=CriaVar("D1_NUMLOTE");dDtValid:=CriaVar("D1_DTVALID");nPotencia:=CriaVar("D1_POTENCI")
								While !Eof() .And. D1_FILIAL+D1_DOC+D1_SERIE+D1_FORNECE+D1_LOJA+D1_COD == cSeekLote
									If D1_IDENTB6 == SB6->B6_IDENT
										cLoteCtl	:=	SD1->D1_LOTECTL
										cNumLote	:=	SD1->D1_NUMLOTE
										dDtValid	:=	SD1->D1_DTVALID
										nPotencia:=	SD1->D1_POTENCI
										cItemOri	:=	SD1->D1_ITEM
										cNumSeri	:=	SD1->D1_NUMSERI
										cLocaliz	:=	SD1->D1_LOCALIZ
										If cPaisLoc == "ARG" .And. nPProvEnt > 0
											cProvEnt	:= SD1->D1_PROVENT
										Endif
										Exit
									EndIf
									dbSkip()
								EndDo
								RestArea(aAreaSD1)
							ElseIf SB6->B6_TES > "500"
								dbSelectArea("SD2")
								aAreaSD2:=GetArea()
								dbSetOrder(3)
								cSeekLote:=xFilial("SD2")+SB6->B6_DOC+SB6->B6_SERIE+SB6->B6_CLIFOR+SB6->B6_LOJA+SB6->B6_PRODUTO
								dbSeek(cSeekLote)
								While !Eof() .And. D2_FILIAL+D2_DOC+D2_SERIE+D2_CLIENTE+D2_LOJA+D2_COD == cSeekLote
									If D2_IDENTB6 == SB6->B6_IDENT
										cLoteCtl	:=	SD2->D2_LOTECTL
										cNumLote	:=	SD2->D2_NUMLOTE
										dDtValid	:=	SD2->D2_DTVALID
										nPotencia	:=	SD2->D2_POTENCI
										cItemOri	:=	SD2->D2_ITEM
										cNumSeri	:=	SD2->D2_NUMSERI
										cLocaliz	:=	SD2->D2_LOCALIZ
										If cPaisLoc == "ARG" .And. nPProvEnt > 0
											cProvEnt	:= SD2->D2_PROVENT
										Endif
										Exit
									EndIf
									dbSkip()
								EndDo
								RestArea(aAreaSD2)
								dbSelectArea("SB6")
							EndIf
						Else
							If SB6->B6_TES > "500"
								dbSelectArea("SD2")
								aAreaSD2:=GetArea()
								dbSetOrder(3)
								cSeekSD2:=xFilial("SD2")+SB6->B6_DOC+SB6->B6_SERIE+SB6->B6_CLIFOR+SB6->B6_LOJA+SB6->B6_PRODUTO
								dbSeek(cSeekSD2)
								While !Eof() .And. D2_FILIAL+D2_DOC+D2_SERIE+D2_CLIENTE+D2_LOJA+D2_COD == cSeekSD2
									If D2_IDENTB6 == SB6->B6_IDENT
										cItemOri	:=	SD2->D2_ITEM
									EndIf
									dbSkip()
								EndDo
								RestArea(aAreaSD2)
								dbSelectArea("SB6")
							EndIf							
						EndIf

						AAdd( aArrayF4,{ SB6->B6_DOC,;
						SB6->B6_SERIE,;
						DtoC( SB6->B6_EMISSAO ),;
						xPadl( Str( nSaldo,aQtTamX3[ 1 ],aQtTamX3[ 2 ] ),100 ),SB6->B6_LOCAL,cLoteCtl,cNumLote,dDtValid,nPotencia,cItemOri,IIf(lRetBenef,SB6->B6_PRUNIT,),IIf(lRetBenef,SB6->B6_IDENT,),cNumSeri,cLocaliz})
						AAdd( aAuxArF4,{ nSaldo,;
						SB6->B6_IDENT,;
						SB6->B6_PRUNIT } )
						If cPaisLoc == "ARG" .And. nPProvEnt > 0
							Aadd(aProvEnt,cProvEnt)
						Endif
					EndIf
				EndIf
			EndIf
			dbSelectArea(cAlias)
			dbSkip(1)
		EndDo
		If (Len( aAuxArF4 ) # 0)
			If lRetBenef
				aNFOrigBen := aClone(aArrayF4)
				nRet := 1
			Else
				DEFINE MSDIALOG oDlg TITLE OemToAnsi(STR0001) From 9,0 To 18,60 OF oMainWnd	//"Notas Poder Terceiro"
					@ .25,.9 LISTBOX oQual VAR cVar Fields HEADER OemToAnsi(STR0002),OemToAnsi(STR0003),OemToAnsi(STR0004),OemToAnsi(STR0005),OemToAnsi(STR0006),OemToAnsi(STR0018),OemToAnsi(STR0021),OemToAnsi(STR0016) SIZE 190,62 ON DBLCLICK (nOption := 1,oDlg:End())	//"Nota"###"S‚rie"###"Emiss„o"###"Saldo Atual"###"Local"###"Lote"###"Sub-Lote"###"Validade"
					oQual:SetArray( aArrayF4 )
					oQual:bLine := { || { aArrayF4[ oQual:nAT,1 ],aArrayF4[ oQual:nAT,2 ],aArrayF4[ oQual:nAT,3 ],aArrayF4[ oQual:nAT,4 ],aArrayF4[ oQual:nAT,5 ],aArrayF4[ oQual:nAT,6 ],aArrayF4[ oQual:nAT,7 ] ,aArrayF4[ oQual:nAT,8 ]} }
				DEFINE SBUTTON FROM 5   ,200  TYPE 1 ACTION (nOption := 1,oDlg:End()) ENABLE OF oDlg
				DEFINE SBUTTON FROM 17.5,200  TYPE 2 ACTION oDlg:End()                ENABLE OF oDlg

				ACTIVATE MSDIALOG oDlg VALID (nOAT := oQual:nAT,.t.)

				If nOption == 1

					If nPosItOri> 0 ;	aCols[n,nPosItOri]:=aArrayF4[nOAT,10]	;	Endif

					aCols[n,nPosNNF]  :=aArrayF4[nOAT,1]
					aCols[n,nPosSER]  :=aArrayF4[nOAT,2]
					aCols[n,nPosLoc]  :=aArrayF4[nOAT,5]
					aCols[n,nPosID6]  :=aAuxArF4[nOAT,2]
					//tratamento aqui
					aCols[n,nPosPrc]  :=aAuxArF4[nOAT,3]
					If cPaisLoc == "ARG" .And. nPProvEnt > 0
						aCols[n,nPProvEnt] := aProvEnt[nOAT]
					Endif
					If nPosLote > 0 ;	aCols[n,nPosLote] :=aArrayF4[nOAT,6]	;	Endif
					If nPosSLote> 0 ;	aCols[n,nPosSLote]:=aArrayF4[nOAT,7]	;	Endif
					If nPosDtVld> 0 ;	aCols[n,nPosDtVld]:=aArrayF4[nOAT,8]	;	Endif
					If nPosPoten> 0 ;	aCols[n,nPosPoten]:=aArrayF4[nOAT,9]	;	Endif

					//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
					//³ Nas NFs de entrada posiciona SD2 origem e obtem aliq. ICMS   ³
					//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
					If cEsx == "E" .And. (nPosAliqIcm > 0 .Or. cPaisLoc<>"BRA")
						dbSelectArea("SD2")
						dbSetOrder(4)
						If dbSeek(xFilial("SD2")+aAuxArF4[nOAT,2])
							If nPosAliqIcm > 0
								aCols[n][nPosAliqIcm]	:= SD2->D2_PICM
							EndIf
							nRecSD2	:= SD2->(RecNo())
						EndIf
						dbSetOrder(nSavOrdSD2)
					EndIf
					If !lRecebto
						aCols[n,nPosQtd]	:= Val(aArrayF4[ nOAT,4 ])
						&(ReadVar())		:= Val(aArrayF4[ nOAT,4 ])
						nRet				:= Val(aArrayF4[ nOAT,4 ])
					Else
						&(ReadVar())		:= aArrayF4[ nOAT,1 ]
					EndIf
					DbSelectArea( cAlias )  ; SetFocus( nHdl )
				EndIf
			EndIf
		ElseIf !lRetBenef
			Help(" ",1,"A440N/SB6")
		EndIf
	ElseIf !lRetBenef
		Help(" ",1,"a440F4") ; DbSetOrder( 1 )
	Endif
Else
	Return( If( lRecebto, TamSX3( "D1_NFORI" )[ 1 ] ,0 ) )
EndIf
Return( nRet )

/*
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ F4Compl  ³ Autor ³ Rosane L. Chene       ³ Data ³ 11/05/95 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Faz a consulta das Notas Fiscais para compl.de IPI         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ F4Compl(a,b,c,ExpC1,ExpC2,ExpC3,ExpC4                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ a,b,c = parametros padroes quando utiliza-se o Set Key     ³±±
±±³          ³ ExpC1 = Codigo do Cliente ou Fornecedor                    ³±±
±±³          ³ ExpC2 = Loja                                               ³±±
±±³          ³ ExpC3 = Codigo do Produto                                  ³±±
±±³          ³ ExpC4 = Codigo do Produto                                  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function F4Compl(a,b,c,cCliFor,cLoja,cProd,cProg,nRecSD1,cVar,cIndPres,cCodA1U,nLinAtv)
Local aArrayF4[0]
Local aObjects  := {}
Local aInfo     := {}
Local aPosObj   := {}
Local aSize     := MsAdvSize( .F. )
Local aRecSD1	:= {}
Local cSeek     := ""
Local cTexto1   := ""
Local cTexto2   := ""
Local cCadastro := ""
Local cCampo    := ""
Local nX        := 0
Local cAlias    := Alias()
Local nOrdem    := IndexOrd()
Local nRec      := RecNo()
Local nOAT      := 0
Local nPosNf    := 0
Local nPosSer   := 0
Local nPosIt    := 0
Local nPosOp    := 0
Local nPosOrdem := 0
Local nOpca     := 0
Local nHdl      := GetFocus()
Local oDlg
Local oQual
Local cAliasQry	:= GetNextAlias()
Local lSIGAF7CON := ExistBlock('SIGAF7CON')
Local lIntMnt   := SuperGetMV("MV_NGMNTES",.F.,"N") == "S" .Or. SuperGetMV("MV_NGMNTCM",.F.,"N") == "S"
Local aRecSD2	:= {}
Local cQry		:= ""
Local lInterF1	:= SF1->(FieldPos("F1_INDPRES")) > 0 .And. SF1->(FieldPos("F1_CODA1U")) > 0
Local nPosLocal := 0

Default cIndPres	:= ""
Default cCodA1U		:= ""
Default nLinAtv		:= 0

cVar := If(cVar==Nil,ReadVar(),cVar)       // variavel corrente

If Substr(cVar,6,6)!= "_NFORI"
	Return Nil
Endif

If cProg $ "A440/A920"
	cArq  := "SF2"
	cSeek := "F2_FILIAL+F2_CLIENTE+F2_LOJA"
Else
	cArq  := "SF1"
	cSeek := "F1_FILIAL+F1_FORNECE+F1_LOJA"
Endif

If cProg $ "A440/A920"
	BeginSql Alias cAliasQry
	SELECT
		SD2.D2_FILIAL, SD2.D2_COD, SD2.D2_DOC, SD2.D2_SERIE, SD2.D2_CLIENTE, SD2.D2_LOJA, SD2.D2_ORIGLAN, SD2.D2_ITEM, SD2.D2_TOTAL, SD2.D2_VALIPI, D2_VALICM, SD2.D2_PRCVEN,
		SF2.F2_FILIAL, SF2.F2_CLIENTE, SF2.F2_LOJA, SF2.F2_TIPO, SF2.F2_DOC, SF2.F2_SERIE, SD2.R_E_C_N_O_
	FROM
		%table:SD2% SD2, %table:SF2% SF2
	WHERE
		SD2.D2_FILIAL=%xFilial:SD2% AND
		SD2.D2_COD=%Exp:cProd% AND
		SD2.D2_CLIENTE=%Exp:cCliFor% AND
		SD2.D2_LOJA=%Exp:cLoja% AND
		SD2.D2_ORIGLAN<>'LF' AND
		SD2.%NotDel% AND

		SF2.F2_FILIAL=%xFilial:SF2% AND
		SF2.F2_DOC=SD2.D2_DOC AND
		SF2.F2_SERIE=SD2.D2_SERIE AND
		SF2.F2_CLIENTE=SD2.D2_CLIENTE AND
		SF2.F2_LOJA=SD2.D2_LOJA AND
		SF2.%NotDel% AND
		SF2.F2_TIPO NOT IN('D','B','P','I')
	ORDER BY
		SD2.D2_FILIAL, SD2.D2_DOC, SD2.D2_SERIE, SD2.D2_CLIENTE, SD2.D2_LOJA, SD2.D2_COD, SD2.D2_ITEM
	EndSql
Else
	cQry := " SELECT"
	cQry += " 	SD1.D1_FILIAL, SD1.D1_COD, SD1.D1_DOC, SD1.D1_SERIE, SD1.D1_FORNECE, SD1.D1_LOJA, SD1.D1_ORIGLAN, SD1.D1_ITEM, SD1.D1_TOTAL, SD1.D1_VALIPI, SD1.D1_VUNIT, SD1.R_E_C_N_O_ SD1RECNO, SD1.D1_OP, SD1.D1_ORDEM, SD1.D1_LOCAL,"
	cQry += " 	SF1.F1_FILIAL, SF1.F1_FORNECE, SF1.F1_LOJA, SF1.F1_TIPO, SF1.F1_DOC, SF1.F1_SERIE"
	cQry += " FROM "
	cQry += RetSqlName("SD1") + " SD1, " + RetSqlName("SF1") + " SF1"
	cQry += " WHERE"
	cQry += " 	SD1.D1_FILIAL= '" + xFilial("SD1") + "' AND"
	cQry += " 	SD1.D1_COD= '" + cProd + "' AND"
	cQry += " 	SD1.D1_FORNECE= '" + cCliFor + "' AND"
	cQry += " 	SD1.D1_LOJA= '" + cLoja + "' AND"
	cQry += " 	SD1.D1_ORIGLAN<>'LF' AND"
	cQry += " 	SD1.D_E_L_E_T_ = ' ' AND"

	cQry += " 	SF1.F1_FILIAL= '" +xFilial("SF1") + "' AND"
	cQry += " 	SF1.F1_DOC=SD1.D1_DOC AND"
	cQry += " 	SF1.F1_SERIE=SD1.D1_SERIE AND"
	cQry += " 	SF1.F1_FORNECE=SD1.D1_FORNECE AND"
	cQry += " 	SF1.F1_LOJA=SD1.D1_LOJA AND"
	cQry += " 	SF1.D_E_L_E_T_ = ' ' AND"
	cQry += " 	SF1.F1_TIPO NOT IN('D','B','P','I')"

	If lInterF1 .And. Type("cFormul") == "C" .And. cFormul == "S" .And. nLinAtv > 0
		cQry += "	AND SF1.F1_INDPRES IN ('" + SubStr(cIndPres,1,1) + "',' ')"
		cQry += "	AND SF1.F1_CODA1U IN ('" + cCodA1U + "',' ')" 
	Endif

	cQry += " ORDER BY"
	cQry += " 	SD1.D1_FILIAL, SD1.D1_DOC, SD1.D1_SERIE, SD1.D1_FORNECE, SD1.D1_LOJA, SD1.D1_COD, SD1.D1_ITEM"

	cQry := ChangeQuery( cQry )

	dbUseArea( .t., "TOPCONN", TcGenQry( ,,cQry ), cAliasQry, .f., .t. )
EndIf

If (cAliasQry)->(Eof())
	HELP(" ",1,"F4NAONOTA")
	dbSelectArea(cAlias)
	dbSetOrder(nOrdem)
	dbGoto(nRec)
	Return .T.
Endif

For Nx:=1 to Len(aHeader)
	cCampo := Trim(aHeader[nx][2])
	cCampo := Subs(cCampo,3,len(cCampo)-2)
	If cCampo == "_NFORI"
		nPosNf	:= nx
	ElseIf cCampo == "_SERIORI"
		nPosSer	:= nx
	ElseIf cCampo == "_ITEMORI"
		nPosIt 	:= nx
	ElseIf cCampo == "_OP"
		nPosOp 	:= nX
	ElseIf cCampo == "_ORDEM"
		nPosOrdem 	:= nX
	ElseIf cCampo == "_LOCAL"
		nPosLocal 	:= nX
	Endif
Next Nx

While !(cAliasQry)->(Eof())
If cProg $ "A440/A920"
	AADD(aArrayF4,{(cAliasQry)->D2_DOC,(cAliasQry)->D2_SERIE,(cAliasQry)->D2_ITEM,STR((cAliasQry)->D2_TOTAL,11,2),Str((cAliasQry)->D2_VALIPI,11,2),Str((cAliasQry)->D2_VALICM,11,2),Str((cAliasQry)->D2_PRCVEN,11,2)})
	If cProg == "A920"
		aAdd(aRecSD2,(cAliasQry)->R_E_C_N_O_)
	EndIf
Else
	AADD(aArrayF4,{(cAliasQry)->D1_DOC,(cAliasQry)->D1_SERIE,(cAliasQry)->D1_ITEM,STR((cAliasQry)->D1_TOTAL,11,2),Str((cAliasQry)->D1_VALIPI,11,2),Str((cAliasQry)->D1_VALIPI,11,2),Str((cAliasQry)->D1_VUNIT,11,2),(cAliasQry)->D1_LOCAL})
	If lIntMnt
		aAdd(aArrayF4[Len(aArrayF4)],(cAliasQry)->D1_OP)
		aAdd(aArrayF4[Len(aArrayF4)],(cAliasQry)->D1_ORDEM)
	EndIf
	aAdd(aRecSD1,(cAliasQry)->SD1RECNO)
EndIf
(cAliasQry)->(dbSkip())
End


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Ponto de Entrada para alterar o array da visualicao do F7    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If lSIGAF7CON .And. !cProg $ "A440/A920"
	aRetPE:= ExecBlock("SIGAF7CON",.F.,.F.,{aArrayF4,aRecSD1})
	If ValType(aRetPE) == "A" .And.	 ValType(aRetPE[1]) == "A" .And. ValType(aRetPE[2]) == "A"
	    If len(aRetPE[1]) == len(aRetPE[2])
		aArrayF4:= aClone(aRetPE[1])
		aRecSD1 := aClone(aRetPE[2])
		EndIF
	EndIf
EndIf

If !Empty(aArrayF4)

	aSize[1] /= 1.5
	aSize[2] /= 1.5
	aSize[3] /= 1.5
	aSize[4] /= 1.3
	aSize[5] /= 1.5
	aSize[6] /= 1.3
	aSize[7] /= 1.5

	AAdd( aObjects, { 100, 020,.T.,.F.,.T.} )
	AAdd( aObjects, { 100, 060,.T.,.T.,.T.} )
	AAdd( aObjects, { 100, 020,.T.,.F.} )
	aInfo   := { aSize[ 1 ], aSize[ 2 ], aSize[ 3 ], aSize[ 4 ], 3, 3 }
	aPosObj := MsObjSize( aInfo, aObjects,.T.)


	cCadastro:= OemToAnsi(STR0007)+"-"+OemToAnsi(STR0033) 	//"Notas Fiscais de Origem"
	nOpca := 0
	DEFINE MSDIALOG oDlg TITLE cCadastro From aSize[7],000 To aSize[6],aSize[5] OF oMainWnd PIXEL

	@ aPosObj[1,1],aPosObj[1,2] MSPANEL oPanel PROMPT "" SIZE aPosObj[1,3],aPosObj[1,4] OF oDlg CENTERED LOWERED

    If cArq  == "SF1"
		cTexto1 := AllTrim(RetTitle("F1_FORNECE"))+"/"+AllTrim(RetTitle("F1_LOJA"))+": "+SA2->A2_COD+"/"+SA2->A2_LOJA+"  -  "+RetTitle("A2_NOME")+": "+SA2->A2_NOME
	Else
		cTexto1 := AllTrim(RetTitle("F2_CLIENTE"))+"/"+AllTrim(RetTitle("F2_LOJA"))+": "+SA1->A1_COD+"/"+SA1->A1_LOJA+"  -  "+RetTitle("A1_NOME")+": "+SA1->A1_NOME
	EndIf
	@ 002,005 SAY cTexto1 SIZE aPosObj[1,3],008 OF oPanel PIXEL
	cTexto2 := AllTrim(RetTitle("B1_COD"))+": "+SB1->B1_COD+"/"+SB1->B1_DESC
	@ 012,005 SAY cTexto2 SIZE aPosObj[1,3],008 OF oPanel PIXEL

	@ aPosObj[2,1],aPosObj[2,2] LISTBOX oQual VAR cVar Fields HEADER OemToAnsi(STR0002),OemToAnsi(STR0003),OemToAnsi(STR0008),OemToAnsi(STR0012),OemToAnsi(STR0013),OemToAnsi(STR0032),OemToAnsi(STR0010) SIZE aPosObj[2,3],aPosObj[2,4] ON DBLCLICK (nOpca := 1,oDlg:End()) PIXEL	//"Nota"###"S‚rie"###"Item"###"Valor Item"###"Valor IPI"
	oQual:SetArray(aArrayF4)
	oQual:bLine := { || {aArrayF4[oQual:nAT][1],aArrayF4[oQual:nAT][2],aArrayF4[oQual:nAT][3],aArrayF4[oQual:nAT][4],aArrayF4[oQual:nAT][5],aArrayF4[oQual:nAT][6],aArrayF4[oQual:nAT][7]}}

	DEFINE SBUTTON FROM aPosObj[3,1]+000,aPosObj[3,4]-030  TYPE 1 ACTION (nOpca := 1,oDlg:End()) 	ENABLE OF oDlg PIXEL
	DEFINE SBUTTON FROM aPosObj[3,1]+012,aPosObj[3,4]-030 TYPE 2 ACTION oDlg:End() 					ENABLE OF oDlg PIXEL

	ACTIVATE MSDIALOG oDlg VALID (nOAT := oQual:nAT, .t.) CENTERED

	If nOpca == 1
		If cProg == "A920"
			nRecSD1	:= aRecSD2[nOAT]
		ElseIf cProg != "A440"
			nRecSD1	:= aRecSD1[nOAT]
		EndIf

		aCols[n][nPosNf] := aArrayF4[nOAT][1]
		aCols[n][nPosSer] := aArrayF4[nOAT][2]
		aCols[n][nPosIt]  := aArrayF4[nOAT][3]
		If !(cProg $ "A440/A920") .And. Len(aArrayF4[nOAT]) > 7
			If nPosLocal > 0
				aCols[n][nPosLocal]  := aArrayF4[nOAT][8]
			Endif
		Endif
		If lIntMnt .And. Len(aArrayF4[nOAT]) > 8
			If nPosOp > 0
				aCols[n][nPosOp] := aArrayF4[nOAT][9]
			EndIf
			If nPosOrdem > 0
				aCols[n][nPosOrdem] := aArrayF4[nOAT][10]
			EndIf
		EndIf
		&(ReadVar()) 		:= aArrayF4[nOAT][1]
	Endif
Else
	HELP(" ",1,"F4NAONOTA")
Endif

dbSelectArea(cAlias)
dbSetOrder(nOrdem)
dbGoto(nRec)
SetFocus(nHdl)

Return .T.

/*
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ F4Lote   ³ Autor ³ Marcos Bregantim      ³ Data ³ 19/09/94 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Faz a consulta aos Saldos do Lotes da Rastreabilidade      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ F4Lote(a,b,c,ExpC1,ExpC2,ExpC3)                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ a,b,c = parametros padroes quando utiliza-se o Set Key     ³±±
±±³          ³ ExpC1 = Programa que chamou a rotina                       ³±±
±±³          ³ ExpC2 = Codigo do Produto                                  ³±±
±±³          ³ ExpC3 = Local                                              ³±±
±±³          ³ ExpL4 = lParam                                             ³±±
±±³          ³ ExpC5 = Localizacao                                        ³±±
±±³          ³ ExpN6 = Verifica se atualiza o aCols do Lote               ³±±
±±³          ³ ExpC6 = Numero da Ordem de Producao                        ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function F4Lote(	a		, b			, c			, cProg		,;
					cCod	, cLocal	, lParam	, cLocaliz	,;
					nLoteCtl, cOP		, lLoja		, lAtNumLote, cCampo)
Local aStruSB8      := {}
Local aArrayF4      := {}
Local aHeaderF4     := {}
Local nX
Local cVar
Local cSeek
Local cWhile
Local nEndereco
Local cAlias        := Alias()
Local nOrdem        := IndexOrd()
Local nRec          := RecNo()
Local nValA440      := 0
Local nHdl          := GetFocus()
Local cCpo
Local oDlg2
Local cCadastro
Local nOpca
Local cLoteAnt      := ""
Local cLoteFor      := ""
Local dDataVali     := ""
Local dDataCria     := ""
Local lAdd          := .F.
Local nSalLote      := 0
Local nSalLote2     := 0
Local nPotencia     := 0
Local nPos2         := 7
Local nPos3         := 5
Local nPos4         := 9
Local nPos5         := 10
Local nPos6         := 11
Local nPos7         := 12
Local nPos8         := 13
Local aTamSX3       := {}
Local nOAT
Local aCombo1       :={STR0018, STR0016, STR0017}
Local aPosObj       := {}
Local aObjects      := {}
Local aSize         := MsAdvSize(.F.)

Local cCombo1       := ""
Local oCombo1
Local lRastro       := Rastro(cCod,"S")
Local aAreaSBF      := {}
Local cQuery        := ""
Local cAliasSB8     := "SB8"
Local nLoop         := 0
Local aUsado        := {}
Local cLote241      := ''
Local cSLote241     := ''
Local lLote         := .F.
Local lSLote        := .F.
Local nPos          := 0
Local nPCod241      := 0
Local nPLoc241      := 0
Local nPLote241     := 0
Local nPSLote241    := 0
Local nQuant241     := 0
Local nPQuant241    := 0
Local nPCod261      := 0
Local nPLoc261      := 0
Local nPosLt261     := 0
Local nPSlote261    := 0
Local nQuant261     := 0
Local nPosQuant     := 0
Local nPosQtdLib    := 0
Local nMultiplic    := 1
Local lRet          := .T.
Local lSelLote      := (SuperGetMV("MV_SELLOTE") == "1")
Local lMTF4Lote     := .T.
Local lExisteF4Lote := ExistBlock("F4LoteHeader")
Local cNumDoc       := ""
Local cSerie        := ""
Local cFornece      := ""
Local cLoja         := ""
Local cLocProc      := GetMvNNR( 'MV_LOCPROC' , '99' )
Local lEmpPrev      := If(SuperGetMV("MV_QTDPREV")== "S",.T.,.F.)
Local nSaldoCons    := 0
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ MV_VLDLOTE - Utilizado para visualizar somente os lotes que  |
//| possuem o campo B8_DATA com o valor menor ou igual a database|
//| do sistema                                                   ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
Local lVldDtLote    := SuperGetMV("MV_VLDLOTE",.F.,.T.)

Local cFilialSB8
Local lConsVenc     := NIL
Local lMV_LOTVENC   := SuperGetMV("MV_LOTVENC",.F.,"S") == "S"
Local aAreaSD4		:= {}
Local cLoteCtl 		:= ""
Local lFirst		:= .T.
Local lFiltraOp		:= SuperGetMV("MV_F4FILOP", .F., .F.) .And. cProg == "A685"

Default cLocaliz   := ""
Default cOP        := ""
Default nLoteCtl   := 1
Default lLoja      := .F.
Default lAtNumLote := .T.
Default Ccampo     := ""

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Ponto de entrada para impedir a apresentacao da Dialog de Saldos ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If ExistBlock("MTF4LOTE")
	lMTF4Lote := ExecBlock("MTF4LOTE",.F.,.F.,{cProg})
	If Valtype(lMTF4Lote) <> "L"
		lMTF4Lote := .T.
	EndIf
EndIf

cCpo := ReadVar()
lParam := IIf(lParam== NIL, .T., lParam)
SB1->(dbSetOrder(1))
SB1->(MsSeek(xFilial("SB1")+cCod))
lLote  := Rastro(cCod)
lSLote := Rastro(cCod, 'S')
If !lLote
	Help(" ",1,"NAORASTRO")
	Return nil
Endif
If !lRastro
	nPos2:=1;nPos3:=5;nPos4:=8;nPos5:=9;nPos6:=10;nPos7:=11;nPos8:=12
EndIf
If (cProg == "A240" .Or. cProg == "A241") .And. cCpo != "M->D3_NUMLOTE" .And. cCpo != "M->D3_LOTECTL"
	Return nil
Endif
If cProg == "A100"
	If cTipo != "D"
		Return Nil
	Endif
Endif
If cProg == "A440" .And. cCpo != "M->C6_NUMLOTE" .And. cCpo != "M->C6_LOTECTL"
	Return Nil
Endif
If cProg == "A240"
	IF  M->D3_TM <= "500" .and. SF5->F5_APROPR != "S" .And. SB1->B1_APROPRI == "I"
		cLocal := cLocProc
	Endif
Endif
If cProg == "A241"
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Inicializa o array aUsado com os Lotes ja digitados no aCols ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	nMultiplic := If(cTM<='500',1,-1)
	For nX := 1 To Len(aHeader)
		If '_COD'         == Right(AllTrim(aHeader[nX, 2]), 4)
			nPCod241   := nX
		ElseIf '_LOCAL'   == Right(AllTrim(aHeader[nX, 2]), 6)
			nPLoc241   := nX
		ElseIf '_LOTECTL' == Right(AllTrim(aHeader[nX, 2]), 8)
			cLote241   := aCols[n, nX]
			nPLote241  := nX
		ElseIf '_NUMLOTE' == Right(AllTrim(aHeader[nX, 2]), 8)
			cSLote241  := aCols[n, nX]
			nPSlote241 := nX
		ElseIf '_QUANT'   == Right(AllTrim(aHeader[nX, 2]), 6)
			nQuant241  := aCols[n, nX]
			nPQuant241 := nX
		EndIf
	Next nX
	For nX := 1 To Len(aCols)
		If !(nX==n) .And. If(ValType(aCols[nX,Len(aCols[nX])])=='L', !aCols[nX,Len(aCols[nX])], .T.)
			If aCols[nX, nPCod241] == cCod .And. aCols[nX,nPLoc241] == cLocal
				If (nPos:=aScan(aUsado, {|x| x[1] == aCols[nX,nPLote241] .And. If(lSLote, x[2]==aCols[nX, nPSlote241], .T.)})) == 0
					aAdd(aUsado, {aCols[nX, nPLote241], aCols[nX, nPSlote241], (aCols[nX, nPQuant241]*nMultiplic)})
				Else
					aUsado[nPos, 3] += (aCols[nX, nPQuant241]*nMultiplic)
				EndIf
			EndIf
		EndIf
	Next nX
	IF  cTm <= "500" .and. SF5->F5_APROPR != "S" .And. SB1->B1_APROPRI == "I"
		cLocal := cLocProc
	Endif
Endif
If cProg == "A261" // Transferencia interna mod. II
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Inicializa o array aUsado com os Lotes ja digitados no aCols ³
	//³ Importante: A rotina MATA261 utiliza posicoes fixas no aCols ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

	// Localiza as saidas do lote
	nPCod261   := 1  // Produto origem
	nPLoc261   := 4  // Local origem
	nPosLt261  := Iif(!__lPyme,12,9)  // Lote
	nPSlote261 := Iif(!__lPyme,13,10) // Sub-lote
	nQuant261  := 16 // Quantidade

	For nX := 1 To Len(aCols)
		If !(nX==n) .And. If(ValType(aCols[nX,Len(aCols[nX])])=='L', !aCols[nX,Len(aCols[nX])], .T.)
			If aCols[nX, nPCod261] == cCod .And. aCols[nX,nPLoc261] == cLocal
				If (nPos:=aScan(aUsado, {|x| x[1] == aCols[nX,nPosLt261] .And. If(lSLote, x[2]==aCols[nX, nPSlote261], .T.)})) == 0
					aAdd(aUsado, {aCols[nX, nPosLt261], aCols[nX, nPSlote261], (aCols[nX, nQuant261]*-1)})
				Else
					aUsado[nPos, 3] += (aCols[nX, nQuant261]*-1) // Saida do lote
				EndIf
			EndIf
		EndIf
	Next nX

	// Localiza as entradas no lote
	nPCod261   := 6  // Produto destino
	nPLoc261   := Iif(!__lPyme,9,8)  // Local destino  	//Armazem Destino
	nPosLt261  := Iif(!__lPyme,20,17) // Lote destino  //Lote Destino
	nQuant261  := 16 // Quantidade 	//Quantidade

	For nX := 1 To Len(aCols)
		If !(nX==n) .And. If(ValType(aCols[nX,Len(aCols[nX])])=='L', !aCols[nX,Len(aCols[nX])], .T.)
			If aCols[nX, nPCod261] == cCod .And. aCols[nX,nPLoc261] == cLocal
				If (nPos:=aScan(aUsado, {|x| x[1] == aCols[nX,nPosLt261]})) == 0
					aAdd(aUsado, {aCols[nX, nPosLt261], Nil, (aCols[nX, nQuant261])})
				Else
					aUsado[nPos, 3] += (aCols[nX, nQuant261]) // Entrada no lote
				EndIf
			EndIf
		EndIf
	Next nX
Endif
If cProg == "A270" .And. cCpo != "M->B7_NUMLOTE" .And. cCpo != "M->B7_LOTECTL"
	Return Nil
Endif
If cProg == "A380" .And. cCpo != "M->D4_NUMLOTE" .And. cCpo != "M->D4_LOTECTL"
	Return Nil
Endif
If cProg == "A381" .And. cCpo != "M->D4_NUMLOTE" .And. cCpo != "M->D4_LOTECTL"
	Return Nil
Endif
If cProg == "A275" .And. cCpo != "M->DD_NUMLOTE" .And. cCpo != "M->DD_LOTECTL"
	Return Nil
Endif

If cPaisLoc $ "ARG|POR|EUA"
	If cProg == "A465" .And. ;
		cCpo != "M->D2_NUMLOTE" .and. cCpo != "M->D2_LOTECTL" .and.;
		cCpo != "M->CN_NUMLOTE" .And. cCpo != "M->CN_LOTECTL" .and.;
		cCpo != "M->D1_NUMLOTE" .And. cCpo != "M->D1_LOTECTL"
		Return Nil
	EndIf
Endif

If cProg == "A440"
	nPosQuant := Ascan(aHeader,{|x| AllTrim(x[2])=="C6_QTDVEN"})
	nPosQtdLib:= Ascan(aHeader,{|x| AllTrim(x[2])=="C6_QTDLIB"})
Endif

// Verifica se o arquivo que chamou a consulta tem potencia para informar no lote
If Type("nPosPotenc") != "N"
	nPosPotenc := 0
Endif

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica o arquivo a ser pesquisado                          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB8")
dbSetOrder(1)
cSeek := cCod+cLocal
dbSeek(xFilial("SB8")+cSeek)
If !Found()
	HELP(" ",1,"F4LOTE")
	dbSelectArea(cAlias)
	dbSetOrder(nOrdem)
	dbGoto(nRec)
	Return nil
Endif

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Obtem o numero de casas decimais que dever ser utilizado na  ³
//³ consulta.                                                    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aTamSX3:=TamSX3(Substr(cCpo,4,3)+"QUANT")
If Empty(aTamSX3)
	aTamSX3:=TamSX3("B8_SALDO")
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Caso utilize controle de enderecamento e tenha endereco      ³
//³ preenchido.                                                  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If Localiza(cCod) .And. !Empty(cLocaliz)
	dbSelectArea("SB8")
	dbSetOrder(3)
	dbSelectArea("SBF")
	aAreaSBF:=GetArea()
	dbSetOrder(1)
	cSeek:=xFilial("SBF")+cLocal+cLocaliz+cCod
	dbSeek(cSeek)
	Do While !Eof() .And. cSeek == BF_FILIAL+BF_LOCAL+BF_LOCALIZ+BF_PRODUTO
		If SB8->(dbSeek(xFilial("SB8")+SBF->BF_PRODUTO+SBF->BF_LOCAL+SBF->BF_LOTECTL+If(!Empty(SBF->BF_NUMLOTE),SBF->BF_NUMLOTE,"")))
			If lVldDtLote .And. SB8->B8_DATA > dDataBase
				SBF->(dbSkip())
				Loop
			EndIf
			If !Empty(SBF->BF_NUMLOTE) .And. lRastro
				AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SBF", "SBF", {SBF->BF_NUMLOTE,SBF->BF_PRODUTO,Str(SBFSaldo(),14,aTamSX3[2]),Str(SBFSaldo(,,,.T.),14,aTamSX3[2]),SB8->B8_DTVALID,SB8->B8_LOTEFOR,SBF->BF_LOTECTL,SB8->B8_DATA,SB8->B8_POTENCI,SBF->BF_LOCALIZ,SBF->BF_NUMSERI}))
			Else
				AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SBF", "SBF", {SBF->BF_LOTECTL,SBF->BF_PRODUTO,Str(SBFSaldo(),14,aTamSX3[2]),Str(SBFSaldo(,,,.T.),14,aTamSX3[2]),SB8->B8_DTVALID,SB8->B8_LOTEFOR,SB8->B8_DATA,SB8->B8_POTENCI,SBF->BF_LOCALIZ,SBF->BF_NUMSERI}))
			EndIf
		EndIf
		dbSelectArea("SBF")
		dbSkip()
	EndDo
	RestArea(aAreaSBF)
ElseIf lSLote
	SB8->( dbSetOrder( 1 ) )
	cAliasSB8 := GetNextAlias()

	aStruSB8 := SB8->( dbStruct() )

	cQuery := "SELECT * FROM " + RetSqlName( "SB8" ) + " SB8 "
	cQuery += "WHERE "
	cQuery += "B8_FILIAL='"  + xFilial( "SB8" )	+ "' AND "
	cQuery += "B8_PRODUTO='" + cCod            	+ "' AND "
	cQuery += "B8_LOCAL='"   + cLocal          	+ "' AND "
	cQuery += IIf(lVldDtLote,"B8_DATA <= '" + DTOS(dDataBase) 	+ "' AND ","")
	cQuery += "D_E_L_E_T_=' ' "
	cQuery += "ORDER BY " + SqlOrder( SB8->( IndexKey() ) )

	cQuery := ChangeQuery( cQuery )

	dbUseArea( .t., "TOPCONN", TcGenQry( ,,cQuery ), cAliasSB8, .f., .t. )

	For nLoop := 1 To Len( aStruSB8 )
		If aStruSB8[ nLoop, 2 ] <> "C"
			TcSetField( cAliasSB8, aStruSB8[nLoop,1],	aStruSB8[nLoop,2],aStruSB8[nLoop,3],aStruSB8[nLoop,4])
		EndIf
	Next nLoop

	While !( cAliasSB8 )->(Eof()) .And. xFilial("SB8")+cSeek == ( cAliasSB8 )->B8_FILIAL+( cAliasSB8 )->B8_PRODUTO+( cAliasSB8 )->B8_LOCAL
		If !(cProg $ "A100/A240/A440/A241/A270/A465/A685/AT460")
			If SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.,IIf(cProg=="A380",dDataBase,Nil)) > 0
				AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SB8", cAliasSB8, {( cAliasSB8 )->B8_NUMLOTE, ( cAliasSB8 )->B8_PRODUTO, Str(SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.),14,aTamSX3[2]), Str(SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.),14,aTamSX3[2]), ( cAliasSB8 )->B8_DTVALID, ( cAliasSB8 )->B8_LOTEFOR, ( cAliasSB8 )->B8_LOTECTL, ( cAliasSB8 )->B8_DATA,( cAliasSB8 )->B8_POTENCI}))
			Endif
		ElseIf cProg == "A240" .Or. cProg == "A241" .Or. cProg == "A261"
			If cProg <> "A261" .And. SF5->(FieldPos("F5_LOTVENC")) > 0
				lConsVenc := lMV_LOTVENC
				If !lConsVenc
					lConsVenc := Posicione("SF5",1,xFilial("SF5")+Iif(cProg=="A240",M->D3_TM,cTM),"F5_LOTVENC") == '1'
				EndIf
			EndIf
			nSalLote  := SB8Saldo(NIL,lConsVenc,NIL,NIL,cAliasSB8,lEmpPrev,.T.)
			nSalLote2 := SB8Saldo(,lConsVenc,,.T.,cAliasSB8,lEmpPrev,.T.)
			If cProg == 'A241' .Or. cProg == "A261"
				//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
				//³ Atualiza o Saldo com as quantidades ja digitadas no aCols ³
				//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
				If QtdComp(nSalLote) > QtdComp(0)
					If (nPos:=aScan(aUsado, {|x| x[1] == ( cAliasSB8 )->B8_LOTECTL .And. x[2] == ( cAliasSB8 )->B8_NUMLOTE})) > 0
						nSalLote  += aUsado[nPos, 3]
						nSalLote2 += ConvUM(cCod, aUsado[nPos, 3], 0, 2)
					EndIf
				EndIf
			EndIf
			IF SF5->F5_TIPO == "D" .or. nSalLote > 0
				AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SB8", cAliasSB8, {( cAliasSB8 )->B8_NUMLOTE, ( cAliasSB8 )->B8_PRODUTO, Str(nSalLote,14,aTamSX3[2]), Str(nSalLote2,14,aTamSX3[2]), ( cAliasSB8 )->B8_DTVALID, ( cAliasSB8 )->B8_LOTEFOR, ( cAliasSB8 )->B8_LOTECTL, ( cAliasSB8 )->B8_DATA,( cAliasSB8 )->B8_POTENCI}))
			Endif
		ElseIf cProg $ "A100/A270"
			AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SB8", cAliasSB8, {( cAliasSB8 )->B8_NUMLOTE, ( cAliasSB8 )->B8_PRODUTO, Str(SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.),14,aTamSX3[2]), Str(SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.),14,aTamSX3[2]), ( cAliasSB8 )->B8_DTVALID, ( cAliasSB8 )->B8_LOTEFOR, ( cAliasSB8 )->B8_LOTECTL, ( cAliasSB8 )->B8_DATA,( cAliasSB8 )->B8_POTENCI}))
		ElseIf cProg == "A440" .Or. cProg == "AT460"
			nValA440 := QtdLote(( cAliasSB8 )->B8_PRODUTO,( cAliasSB8 )->B8_LOCAL,( cAliasSB8 )->B8_NUMLOTE,.F.,( cAliasSB8 )->B8_LOTECTL)
			If SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.)-nValA440 > 0
				AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SB8", cAliasSB8, {( cAliasSB8 )->B8_NUMLOTE, ( cAliasSB8 )->B8_PRODUTO, Str(SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.)-nValA440,14,aTamSX3[2]), Str(SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.)-ConvUM(( cAliasSB8 )->B8_PRODUTO,nValA440,0,2),14,aTamSX3[2]), ( cAliasSB8 )->B8_DTVALID, ( cAliasSB8 )->B8_LOTEFOR, ( cAliasSB8 )->B8_LOTECTL, ( cAliasSB8 )->B8_DATA,( cAliasSB8 )->B8_POTENCI}))
			Endif
		ElseIf cProg == "A685"
			If (SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.) > 0 .And. lParam) .Or. (!lParam)
			   AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SB8", cAliasSB8, {( cAliasSB8 )->B8_NUMLOTE, ( cAliasSB8 )->B8_PRODUTO, Str(SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.),14,aTamSX3[2]), Str(SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.),14,aTamSX3[2]), ( cAliasSB8 )->B8_DTVALID, ( cAliasSB8 )->B8_LOTEFOR, ( cAliasSB8 )->B8_LOTECTL, ( cAliasSB8 )->B8_DATA,( cAliasSB8 )->B8_POTENCI}))
			ElseIf !Empty(cOP) .And. ( SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.,,,cOP) > 0 .And. lParam )
			   AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SB8", cAliasSB8, {( cAliasSB8 )->B8_NUMLOTE, ( cAliasSB8 )->B8_PRODUTO, Str(SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.),14,aTamSX3[2]), Str(SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.),14,aTamSX3[2]), ( cAliasSB8 )->B8_DTVALID, ( cAliasSB8 )->B8_LOTEFOR, ( cAliasSB8 )->B8_LOTECTL, ( cAliasSB8 )->B8_DATA,( cAliasSB8 )->B8_POTENCI}))
			EndIf
		ElseIf cProg $ "A465"
			AADD(aArrayF4, F4LoteArray(cProg, lSLote, "SB8", cAliasSB8, {( cAliasSB8 )->B8_NUMLOTE, ( cAliasSB8 )->B8_PRODUTO, Str((SB8SALDO(,,,,cAliasSB8,lEmpPrev,,,.T.)-(SB8SALDO(.T.,,,,cAliasSB8,lEmpPrev,,,.T.)+nValA440)),14,aTamSX3[2]), ;
			Str((SB8SALDO(,,,.T.,cAliasSB8,lEmpPrev,,,.T.)-(SB8SALDO(.T.,,,.T.,cAliasSB8,lEmpPrev,,,.T.)+ConvUM(( cAliasSB8 )->B8_PRODUTO,nValA440,0,2))),14,aTamSX3[2]), ;
			( cAliasSB8 )->B8_DTVALID, ( cAliasSB8 )->B8_LOTEFOR, ( cAliasSB8 )->B8_LOTECTL, ( cAliasSB8 )->B8_DATA,( cAliasSB8 )->B8_POTENCI}))
		Endif
		( cAliasSB8 )->( dbSkip() )
	EndDo

	( cAliasSB8 )->( dbCloseArea() )
	dbSelectArea( "SB8" )


Else
	SB8->( dbSetOrder( 3 ) )
	cAliasSB8 := GetNextAlias()

	aStruSB8 := SB8->( dbStruct() )

	//--Filtra lote atrelado a OP no apontamento de perda
	If lFiltraOp
		aAreaSD4 := SD4->( GetArea() )
		SD4->( DbSetOrder(2) )
		If SD4->( DbSeek( FWxFilial("SD4") + cOrdemP + cCod + cLocal ) )
			While SD4->( !EOF() ) .And.;
				SD4->(D4_FILIAL + D4_OP + D4_COD + D4_LOCAL ) == ( FWxFilial("SD4") + cOrdemP + cCod + cLocal )

					If !Empty(SD4->D4_LOTECTL)
						If lFirst
							cLoteCtl := SD4->D4_LOTECTL
						Else
							cLoteCtl += ";" + SD4->D4_LOTECTL
						EndIf		
					EndIf

					lFirst := .F.

				SD4->(DbSkip())
			EndDo

			//--Necessario incrementar a OP para que seja considerada na busca de saldo pela função SB8Saldo
			cOp := cOrdemP
		EndIf
		RestArea(aAreaSD4)
	EndIf

	cQuery := "SELECT * FROM " + RetSqlName( "SB8" ) + " SB8 "
	cQuery += "WHERE "
	cQuery += "B8_FILIAL='"  + xFilial( "SB8" )	+ "' AND "
	cQuery += "B8_PRODUTO='" + cCod            	+ "' AND "
	cQuery += "B8_LOCAL='"   + cLocal          	+ "' AND "

	If lFiltraOp
		If !Empty(cLoteCtl)
			cQuery += "B8_LOTECTL IN " + FormatIn( cLoteCtl, ";") + " AND "
		EndIf
	EndIf

	cQuery += IIf(lVldDtLote,"B8_DATA <= '" + DTOS(dDataBase) 	+ "' AND ","")
	cQuery += "D_E_L_E_T_=' ' "
	cQuery += "ORDER BY " + SqlOrder( SB8->( IndexKey() ) )

	cQuery := ChangeQuery( cQuery )

	dbUseArea( .t., "TOPCONN", TcGenQry( ,,cQuery ), cAliasSB8, .f., .t. )

	For nLoop := 1 To Len( aStruSB8 )
		If aStruSB8[ nLoop, 2 ] <> "C"
			TcSetField( cAliasSB8, aStruSB8[nLoop,1],	aStruSB8[nLoop,2],aStruSB8[nLoop,3],aStruSB8[nLoop,4])
		EndIf
	Next nLoop

	cFilialSB8 := xFilial("SB8")
	While !( cAliasSB8 )->( Eof()) .And. cFilialSB8+cSeek == ( cAliasSB8 )->B8_FILIAL+( cAliasSB8 )->B8_PRODUTO+( cAliasSB8 )->B8_LOCAL
		cLoteAnt  := ( cAliasSB8 )->B8_LOTECTL
		cLoteFor  := ( cAliasSB8 )->B8_LOTEFOR
		dDataVali := ( cAliasSB8 )->B8_DTVALID
		dDataCria := ( cAliasSB8 )->B8_DATA
		nPotencia := ( cAliasSB8 )->B8_POTENCI
		cNumDoc   := ( cAliasSB8 )->B8_DOC
		cSerie    := ( cAliasSB8 )->B8_SERIE
		cFornece  := ( cAliasSB8 )->B8_CLIFOR
		cLoja     := ( cAliasSB8 )->B8_LOJA

		lAdd      := .F.
		nSalLote  := 0
		nSalLote2 := 0
		If cProg == "A440" .Or. cProg == "AT460"
			nValA440 := QtdLote(( cAliasSB8 )->B8_PRODUTO,( cAliasSB8 )->B8_LOCAL,"",.F.,cLoteAnt)
		EndIf
		While !( cAliasSB8 )->( Eof() ) .And. cFilialSB8+cSeek+cLoteAnt == ( cAliasSB8 )->B8_FILIAL+( cAliasSB8 )->B8_PRODUTO+( cAliasSB8 )->B8_LOCAL+( cAliasSB8 )->B8_LOTECTL
			If !(cProg $ "A100/A240/A440/A241/A242/A270/AT460/A685")
				nSalLote += SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.,IIf(cProg == "A380",dDataBase,Nil))
				nSalLote2+= SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.,IIf(cProg == "A380",dDataBase,Nil))
			ElseIf cProg == "A240" .Or. cProg == "A241" .Or. cProg == "A242"
				If cProg <> "A242" .And. SF5->(FieldPos("F5_LOTVENC")) > 0
					lConsVenc := lMV_LOTVENC
					If !lConsVenc
						lConsVenc := Posicione("SF5",1,xFilial("SF5")+Iif(cProg=="A240",M->D3_TM,cTM),"F5_LOTVENC") == '1'
					EndIf
				EndIf
				nSalLote += SB8Saldo(NIL,lConsVenc,NIL,NIL,cAliasSB8,lEmpPrev,.T.,,,cOP)
				nSalLote2+= SB8Saldo(,lConsVenc,,.T.,cAliasSB8,lEmpPrev,.T.,,,cOP)
			ElseIf cProg $ "A100/A270"
				nSalLote += SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.)
				nSalLote2+= SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.)
			ElseIf cProg == "A440" .Or. cProg == "AT460"
				nSalLote  += SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.) - nValA440
				nSalLote2 += SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.) - ConvUM(cCod,nValA440,0,2)
                nValA440 :=0
			ElseIf cProg == "A685"
				If Empty(cOP)
					nSalLote += SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.,IIf(cProg == "A380",dDataBase,Nil))
					nSalLote2+= SB8Saldo(,,,.T.,cAliasSB8,lEmpPrev,.T.,IIf(cProg == "A380",dDataBase,Nil))
				Else
					nSalLote += SB8Saldo(NIL,NIL,NIL,NIL,cAliasSB8,lEmpPrev,.T.,IIf(cProg == "A380",dDataBase,Nil),,cOP)
					nSalLote2+= SB8Saldo(NIL,NIL,NIL,.T.,cAliasSB8,lEmpPrev,.T.,IIf(cProg == "A380",dDataBase,Nil),,cOP)
				EndIf
			EndIf
			( cAliasSB8 )->( dbSkip() )
		EndDo
		If cProg == 'A241' .Or. cProg == "A261"
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Atualiza o Saldo com as quantidades ja digitadas no aCols ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			If QtdComp(nSalLote,.t.) > QtdComp(0,.t.) 
				If (nPos:=aScan(aUsado, {|x| x[1] == cLoteAnt})) > 0
					nSalLote  += aUsado[nPos, 3]
					nSalLote2 += ConvUM(cCod, aUsado[nPos, 3], 0, 2)
				EndIf
			EndIf
		EndIf
		If QtdComp(nSalLote,.t.) > QtdComp(0,.t.) .Or. ((cProg == "A270" .And. !lParam) .Or. (cProg == "A685" .And. !lParam) .Or. ((cProg == "A240" .Or. cProg == "A241") .And. SF5->F5_TIPO == "D") .Or. (cProg == "A242" .And. cCpo == "M->D3_LOTECTL"))
			AADD(aArrayF4, F4LoteArray(cProg, lSLote, "", "", {cLoteAnt,cCod,Str(nSalLote,aTamSX3[1],aTamSX3[2]),Str(nSalLote2,aTamSX3[1],aTamSX3[2]), (dDataVali), cLoteFor, dDataCria,nPotencia,cNumDoc,cSerie,cFornece,cLoja}))
		EndIf
	EndDo

	( cAliasSB8 )->( dbCloseArea() )
	dbSelectArea( "SB8" )

EndIf

If ExistBlock("F4LOTIND")
	aRetPE:= ExecBlock("F4LOTIND",.F.,.F.,{aArrayF4})
	If ValType(aRetPE) == "A" .And. Len(aRetPE) > 0
		aArrayF4:= aClone(aRetPE)
	EndIf
EndIf

If lMTF4Lote
	If !Empty(aArrayF4)

		AAdd( aObjects, { 100, 100, .t., .t.,.t. } )
		AAdd( aObjects, { 100, 30, .t., .f. } )

		aSize[ 3 ] -= 50
		aSize[ 4 ] -= 50

		aSize[ 5 ] -= 100
		aSize[ 6 ] -= 100

		aInfo   := { aSize[ 1 ], aSize[ 2 ], aSize[ 3 ], aSize[ 4 ], 3, 2 }
		aPosObj := MsObjSize( aInfo, aObjects )

		cCadastro := OemToAnsi(STR0014)	//"Saldos por Lote"
		nOpca := 0

		DEFINE MSDIALOG oDlg2 TITLE cCadastro From aSize[7],00 To  aSize[6],aSize[5] OF oMainWnd PIXEL
		@ 7.1,.4 Say OemToAnsi(STR0023) //"Pesquisa Por: "
		If lSLote
			aHeaderF4 := {STR0011,STR0015,STR0005,STR0041,STR0016,STR0017,STR0018,STR0024,STR0029,STR0042,STR0003,STR0043,STR0044} //"Sub-Lote"###"Produto"###"Saldo Atual"###"Saldo Atual 2aUM"###"Validade"###"Lote Fornecedor"###"Lote"###"Dt Emissao"###"Potencia"###"Nota Fiscal"###"Serie"###"Cliente/Fornecedor"###"Loja"
			aHeaderF4 := RetExecBlock("F4LoteHeader", {cProg, lSLote, aHeaderF4}, "A", aHeaderF4)

			If lExisteF4Lote
				AjustaPosHeaderF4(aHeaderF4, @nPos2, @nPos3, @nPos4, @nPos5, @nPos6, @nPos7, @nPos8)
			EndIf

	      oQual := VAR := cVar := TWBrowse():New( aPosObj[1][1], aPosObj[1][2], aPosObj[1][3], aPosObj[1][4],,aHeaderF4,,,,,,,{|nRow,nCol,nFlags|(nOpca := 1,oDlg2:End())},,,,,,, .F.,, .T.,, .F.,,, )
			oQual:SetArray(aArrayF4)
			oQual:bLine := { || aArrayF4[oQual:nAT] }
		Else
			aHeaderF4 := {STR0018,STR0015,iIf(cProg == 'A270',STR0087,STR0005),iIf(cProg == 'A270',STR0088,STR0041),STR0016,STR0017,STR0024,STR0029,STR0042,STR0003,STR0043,STR0044}//"Lote"###"Produto"###"Saldo Atual"###"Saldo Atual 2aUM"###"Validade"###"Lote Fornecedor"###"Dt Emissao"###"Potencia"###"Nota Fiscal"###"Serie"###"Cliente/Fornecedor"###"Loja"
			aHeaderF4 := RetExecBlock("F4LoteHeader", {cProg, lSLote, aHeaderF4}, "A", aHeaderF4)

			If lExisteF4Lote
				AjustaPosHeaderF4(aHeaderF4, @nPos2, @nPos3, @nPos4, @nPos5, @nPos6, @nPos7, @nPos8)
			EndIf

	      oQual := VAR := cVar := TWBrowse():New( aPosObj[1][1], aPosObj[1][2], aPosObj[1][3], aPosObj[1][4],,aHeaderF4,,,,,,,{|nRow,nCol,nFlags|(nOpca := 1,oDlg2:End())},,,,,,, .F.,, .T.,, .F.,,, )
			oQual:SetArray(aArrayF4)
			oQual:bLine := { || aArrayF4[oQual:nAT] }
		EndIf
		@ aPosObj[2][1]+10,aPosObj[2][2] Say OemToAnsi(STR0023) PIXEL //"Pesquisa Por: "
		@ aPosObj[2][1]+10,aPosObj[2][2]+50 MSCOMBOBOX oCombo1 VAR cCombo1 ITEMS aCombo1 SIZE 100,44  VALID F4LotePesq(cCombo1,aArrayF4,oQual) OF oDlg2 FONT oDlg2:oFont PIXEL

		DEFINE SBUTTON FROM aPosObj[2][1]+10 ,aPosObj[2][4]-58  TYPE 1 ACTION (nOpca := 1,oDlg2:End()) ENABLE OF oDlg2
		DEFINE SBUTTON FROM aPosObj[2][1]+10 ,aPosObj[2][4]-28   TYPE 2 ACTION oDlg2:End() ENABLE OF oDlg2

		ACTIVATE MSDIALOG oDlg2 VALID (nOAT := oQual:nAT,.t.) CENTERED

		If nOpca ==1
			If cProg == "A260" .Or. cProg == "A242"
				If !(Substr(cCpo,7) == "LOTECTL" .Or. Substr(cCpo,7) == "_LOTECT")
					If lSLote
						cNumLote := aArrayF4[nOAT][1]
					EndIf
					cLoteDigi:= aArrayF4[nOAT][nPos2]
					dDtValid := aArrayF4[nOAT][nPos3]
					nPotencia:= aArrayF4[nOAT][nPos4]
				EndIf
				If cProg == "A242"
					If Substr(cCpo,7) == "LOTECTL" .Or. Substr(cCpo,7) == "_LOTECT"
						&(ReadVar()) :=  aArrayF4[nOAT][nPos2]
						If Type('aCols') == 'A'
							If lSLote
								aCols[n][nPosLote]:=aArrayF4[nOAT][1]
							EndIf
							If nLoteCtl == 1
								aCols[n][nPosLotCTL] :=aArrayF4[nOAT][nPos2]
								aCols[n][nPosDValid] :=aArrayF4[nOAT][nPos3]
							EndIf
							If nPosPotenc > 0
								aCols[n][nPosPotenc] :=aArrayF4[nOAT][nPos4]
							EndIf
						Endif
					EndIf
				EndIf
				If cPaisLoc $ "ARG|POR|EUA"
					If cProg == "A260"
						nQuant260D := 0.00
						nQuant260  := Val(aArrayF4[nOAT][3])
						nQuant260D := ConvUm(aArrayF4[nOAT][2],nQuant260,nQuant260D,2)
					EndIf
				EndIf

			ElseIf cProg == "A240"
				If lSLote
					nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "D3_NUMLOTE" } )
					If nEndereco > 0
						aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][1]
						M->D3_NUMLOTE := aArrayF4[nOAT][1]
					EndIf
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "D3_LOTECTL" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos2]
					M->D3_LOTECTL := aArrayF4[nOAT][nPos2]
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "D3_DTVALID" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos3]
					M->D3_DTVALID := aArrayF4[nOAT][nPos3]
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "D3_POTENCI" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos4]
					M->D3_POTENCI := aArrayF4[nOAT][nPos4]
				EndIf
			ElseIf cProg == "A270"
				If lSLote
					nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "B7_NUMLOTE" } )
					If nEndereco > 0
						aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][1]
						M->B7_NUMLOTE := aArrayF4[nOAT][1]
					EndIf
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "B7_LOTECTL" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos2]
					M->B7_LOTECTL := aArrayF4[nOAT][nPos2]
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "B7_DTVALID" } )
				If nEndereco > 0
					M->B7_DTVALID := aArrayF4[nOAT][nPos3]
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos3]
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "B7_NUMDOC " } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos5]
					M->B7_NUMDOC:=SB8->B8_DOC
				EndIf

				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "B7_SERIE  " } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos6]
					M->B7_SERIE:=SB8->B8_SERIE
				EndIf

				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "B7_FORNECE" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos7]
					M->B7_FORNECE:=SB8->B8_CLIFOR
				EndIf

				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "B7_LOJA   " } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos8]
					M->B7_LOJA:=SB8->B8_LOJA
				EndIf

			ElseIf cProg == "A380"
				If lSLote
					nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "D4_NUMLOTE" } )
					If nEndereco > 0
						aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][1]
						M->D4_NUMLOTE := aArrayF4[nOAT][1]
					EndIf
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "D4_LOTECTL" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos2]
					M->D4_LOTECTL := aArrayF4[nOAT][nPos2]
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "D4_DTVALID" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos3]
					M->D4_DTVALID :=  aArrayF4[nOAT][nPos3]
				EndIf
			ElseIf cProg == "A275"
				If lSLote
					nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "DD_NUMLOTE" } )
					If nEndereco > 0
						aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][1]
						M->DD_NUMLOTE := aArrayF4[nOAT][1]
					EndIf
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "DD_LOTECTL" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos2]
					M->DD_LOTECTL := aArrayF4[nOAT][nPos2]
				EndIf
				nEndereco := Ascan(aGets,{ |x| Subs(x,9,10) == "DD_DTVALID" } )
				If nEndereco > 0
					aTela[Val(Subs(aGets[nEndereco],1,2))][Val(Subs(aGets[nEndereco],3,1))*2] := aArrayF4[nOAT][nPos3]
					M->DD_DTVALID :=  aArrayF4[nOAT][nPos3]
				EndIf
			ElseIf cProg == "A465"
				If lRastro
				   aCols[n][nPosLote] := aArrayF4[nOAT][1]
					aCols[n][nPosLotCTL] := aArrayF4[nOAT][nPos2]
				Else
	            aCols[n][nPosLotCTL] := aArrayF4[nOAT][1]
				EndIf
				aCols[n][nPosDValid] := aArrayF4[nOAT][5]
				If Substr(cCpo,7) == "LOTECTL"
				   If lRastro
					  	&(ReadVar()) :=  aArrayF4[nOAT][nPos2]
					Else
						&(ReadVar()) :=  aArrayF4[nOAT][1]
				   EndIf
				Else
					If lRastro
						&(ReadVar()) :=  aArrayF4[nOAT][1]
					EndIf
				EndIf
			ElseIf cProg == "AT460"
				If lSLote
					If SubStr(cCpo,8) == "NUMLOT"
						&(ReadVar()) := aArrayF4[nOAT][1]
					Else
						GDFieldPut("ABA_NUMLOT",aArrayF4[nOAT][1],n)
					EndIf
				EndIf
				If SubStr(cCpo,8) == "LOTECT"
					&(ReadVar()) := aArrayF4[nOAT][nPos2]
				Else
					GDFieldPut("ABA_LOTECT",aArrayF4[nOAT][nPos2],n)
				EndIf
			ElseIf cProg == "A310"
				If lSLote
					cNumLote := aArrayF4[nOAT][1]
				EndIf
				cLoteDigi:= aArrayF4[nOAT][nPos2]
				dDtValid2 := aArrayF4[nOAT][nPos3]
				nQuant:=0
				nQuant2UM :=0
			ElseIf cProg == "ESTA009"
				FwFldPut("IN2_LOTECT",aArrayF4[nOAT][nPos2] )
				If lSLote
					FwFldPut("IN2_NUMLOT",aArrayF4[nOAT][1])
				EndIf
			ElseIf cProg == "AGR900"
				If 	Alltrim(cCpo) == "M->NPN_LOTE"
					FwFldPut("NPN_LOTE", aArrayF4[nOAT][nPos2])
				EndIf
			ElseIf cProg == "A311"
				If cCpo == "M->NNT_LOTECT"
					If lSLote
						M->NNT_NUMLOT	:=  PadR(aArrayF4[nOAT][1],TamSx3("NNT_NUMLOT")[1])
						FwFldPut( "NNT_NUMLOT" , PadR(aArrayF4[nOAT][1],TamSx3("NNT_NUMLOT")[1]),,,,.T.)
					EndIf
					M->NNT_LOTECT	:= aArrayF4[nOAT][nPos2]
					M->NNT_NUMLOT	:=  PadR(aArrayF4[nOAT][1],TamSx3("NNT_NUMLOT")[1])
				ElseIf cCpo == "M->NNT_NUMLOT"
					M->NNT_NUMLOT	:=  PadR(aArrayF4[nOAT][1],TamSx3("NNT_NUMLOT")[1])
					FwFldPut( "NNT_LOTECT" , aArrayF4[nOAT][nPos2],,,,.T. )
				EndIf
			ElseIf cProg == "ACDI011"
				MV_PAR05 := aArrayF4[nOAT][nPos2] 	// LOTE
				If lSLote
					MV_PAR06 := aArrayF4[nOAT][1] 	// SUBLOTE
				EndIf
				MV_PAR07 := aArrayF4[nOAT][nPos3]
			Else
				lRet := .T.
				If lSelLote .and. nPosQuant > 0
					SB8->(DbSetOrder(3))
					If lSLote
						cSeek:=xFilial("SB8")+cCod+cLocal+aArrayF4[nOAT][nPos2]+aArrayF4[nOAT][1]
						cWhile:="SB8->(B8_FILIAL+B8_PRODUTO+B8_LOCAL+B8_LOTECTL+B8_NUMLOTE)"
					Else
						cSeek:=xFilial("SB8")+cCod+cLocal+aArrayF4[nOAT][nPos2]
						cWhile:="SB8->(B8_FILIAL+B8_PRODUTO+B8_LOCAL+B8_LOTECTL)"
					EndIf
					dbSeek(cSeek)
					nSaldoCons:=0
					While !EOF() .And. cSeek == &(cWhile)
						nSaldoCons+=SB8SALDO(,,,,,lEmpPrev,,,.T.)
						dbSkip()
					End
					If IIf(cProg == "A440",aCols[n][nPosQtdLib] > nSaldoCons,aCols[n][nPosQuant] > nSaldoCons)
						If cProg == "A440"
							Aviso(STR0030,STR0045,{"Ok"}) //"Atencao"###"Quantidade informada e maior que a quantidade do lote selecionado, modifique a quantidade do item"
						Else
							Aviso(STR0030,STR0031,{"Ok"}) //"Atencao"###"Quantidade informada e maior que a quantidade disponivel do lote selecionado, modifique a quantidade liberada do item"
						EndIf
						lRet := .F.
					EndIf
				EndIf
				If lRet
					If !Empty(cProg) .And. Type('aCols') == 'A'
						If lSLote
							If lLoja
								aColsDet[n][nPosLote]:=aArrayF4[nOAT][1]
							Else
								If lAtNumLote
									aCols[n][nPosLote]:=aArrayF4[nOAT][1]
								EndIf
							EndIf
						EndIf
						If nLoteCtl == 1
							If lLoja
								aColsDet[n][nPosLotCTL] :=aArrayF4[nOAT][nPos2]
								aColsDet[n][nPosDValid] :=aArrayF4[nOAT][nPos3]
							Else
								aCols[n][nPosLotCTL] :=aArrayF4[nOAT][nPos2]
								aCols[n][nPosDValid] :=aArrayF4[nOAT][nPos3]
							EndIf
						EndIf
						If nPosPotenc > 0
							aCols[n][nPosPotenc] :=aArrayF4[nOAT][nPos4]
						EndIf
					Endif
					If Substr(cCpo,7) == "LOTECTL" .Or. Substr(cCpo,7) == "_LOTECT"
						&(ReadVar()) :=  aArrayF4[nOAT][nPos2]
					else
						cCampo :=  aArrayF4[nOAT][nPos2]
						If lSLote
							&(ReadVar()) :=  aArrayF4[nOAT][1]
						EndIf
					Endif
				EndIf
			EndIf
		EndIf
	Else
		HELP(" ",1,"F4LOTE")
	Endif
EndIf
dbSelectArea(cAlias)
dbSetOrder(nOrdem)
dbGoto(nRec)
SetFocus(nHdl)
Return Nil

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿±±
±±³ Fun‡„o    ³ AjustaPosHeaderF4                                          ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Autor     ³ Julio C.Guerato				             ³ Data ³ 22/03/12 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Descri‡„o ³ Ajuste variaveis nPos caso as mesmas tenham sido alteradas ³±±
±±³ 		  ³ o seu posicionamento através do pe:  F4LoteHeader		   ³±±
±±³ 		  ³ As variáveis nPosX estão sendo passadas como referência    ³±±
±±³           | para serem atualizadas conforme o novo posicionamento      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³  Uso      ³ SigaEST/SIGAPCP                                            ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function AjustaPosHeaderF4(aHeaderF4, nPos2, nPos3, nPos4, nPos5, nPos6, nPos7, nPos8)
Local n:=0

nPos2:=iif((n:=AScan(aHeaderF4,{|aHeaderF4| aHeaderF4 == STR0018}))>0,n,nPos2)
nPos3:=iif((n:=AScan(aHeaderF4,{|aHeaderF4| aHeaderF4 == STR0016}))>0,n,nPos3)
nPos4:=iif((n:=AScan(aHeaderF4,{|aHeaderF4| aHeaderF4 == STR0029}))>0,n,nPos4)
nPos5:=iif((n:=AScan(aHeaderF4,{|aHeaderF4| aHeaderF4 == STR0042}))>0,n,nPos5)
nPos6:=iif((n:=AScan(aHeaderF4,{|aHeaderF4| aHeaderF4 == STR0003}))>0,n,nPos6)
nPos7:=iif((n:=AScan(aHeaderF4,{|aHeaderF4| aHeaderF4 == STR0043}))>0,n,nPos7)
nPos8:=iif((n:=AScan(aHeaderF4,{|aHeaderF4| aHeaderF4 == STR0044}))>0,n,nPos8)

Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿±±
±±³ Fun‡„o    ³ ListBoxAll()                                               ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Autor     ³ Rodrigo de Almeida Sartorio              ³ Data ³ 05/10/98 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Descri‡„o ³ Troca marcador entre x e branco para todos itens do array  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³  Uso      ³ SigaEST/SIGAPCP                                            ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function ListBoxAll(nRow,nCol,oLbx,oOk,oNo,aArray)
Local oMenu,nChoice:=0,zi
MENU oMenu POPUP
MENUITEM STR0019 ACTION nChoice:=1	//"&Marca Todos"
SEPARATOR
MENUITEM STR0020 ACTION nChoice:=2	//"&Desmarca Todos"
ENDMENU
ACTIVATE POPUP oMenu AT nRow - 60, nCol OF oLbx
// Marca Todos
If nChoice == 1
	For zi:=1 to Len(aArray)
		aArray[zi,1]:=.T.
	Next zi
	// Desmarca Todos
ElseIf nChoice == 2
	For zi:=1 to Len(aArray)
		aArray[zi,1]:=.F.
	Next zi
EndIf
// Atualiza Array
If nChoice == 1 .Or. nChoice == 2
	oLbx:SetArray(aArray)
	oLbx:bLine := { || {If(aArray[oLbx:nAt,1],oOk,oNo),aArray[oLbx:nAt,2]}}
	oLbx:Refresh()
EndIf
Return

/*
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡„o    ³fLibRejCQ ³ Autor ³ Fernando Joly Siquini ³ Data ³ 09/02/99 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡„o ³ Retorna os Itens Liberados e Rejeitados do CQ.             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ fLibRejCQ(ExpC1, ExpC2, ExpC3, ExpC4, ExpC5, ExpC6, ExpC7) ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno	 ³ Um Array no seguinte formato:                              ³±±
±±³       	 ³ Array[n,1] = Tipo de Movimenta‡„o (0=Qtd.Orig./1=Lib/2=Rej)³±±
±±³       	 ³ Array[n,2] = Quantidade Movimentada                        ³±±
±±³       	 ³ Array[n,3] = Local Destino da Movimenta‡„o                 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ ExpC1 = Codigo do Produto;                                 ³±±
±±³          ³ ExpC2 = Numero da NF;                                      ³±±
±±³          ³ ExpC3 = S‚rie da NF;                                       ³±±
±±³          ³ ExpC4 = Codigo do Fornecedor;                              ³±±
±±³          ³ ExpC5 = Loja do Fornecedor;                                ³±±
±±³          ³ ExpC6 = Lote do Produto;                                   ³±±
±±³          ³ ExpC7 = Numero do Item da NF.                              ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ Ger‚rico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Function fLibRejCQ(cProduto, cDocOri, cSerieOri, cCliente, cLoja, cLote, cItemOri)

//-- Inicializa Variaveis Locais
Local cAliAnt    := Alias()
Local nRecAnt    := Recno()
Local nOrdAnt    := Indexord()
Local nSD1Rec    := SD1->(Recno())
Local nSD1Ord    := SD1->(IndexOrd())
Local nSD7Rec    := SD7->(Recno())
Local nSD7Ord    := SD7->(IndexOrd())
Local aRet       := {}
Local cSeekSD1   := xFilial('SD1')+cDocOri+cSerieOri+cCliente+cLoja+cProduto
Local cSeekSD7   := xFilial('SD7')+SD1->D1_NUMCQ+SD1->D1_COD+SD1->D1_LOCAL
Local lAchou     := .F.

//-- Reinicializa Variaveis
cLote := IF(cLote==NIL,cLote:=Space(6),cLote)

//-- Inicializa Ordens dos Arquivos utilizados na Fun‡„o
SD1->(dbSetOrder(1))
SD7->(dbSetOrder(1))

//-- Procura Notas originarias da Movimenta‡„o no CQ
If SD1->(dbSeek(cSeekSD1, .F.))
	//-- Verifica se esta posicionado no item correto
	If cItemOri # NIL
		Do While !SD1->(Eof()) .And. ;
			cSeekSD1 == SD1->D1_FILIAL+SD1->D1_DOC+SD1->D1_SERIE+SD1->D1_FORNECE+SD1->D1_LOJA+SD1->D1_COD
			If SD1->D1_ITEM == cItemOri
				lAchou := .T.
				Exit
			EndIf
			SD1->(dbSkip())
		EndDo
		SD1->(dbGoto(If(!lAchou,nSD1Rec,SD1->(Recno()))))
		cSeekSD7 := xFilial('SD7')+SD1->D1_NUMCQ+SD1->D1_COD+SD1->D1_LOCAL
	EndIf
	//-- Procura a Movimenta‡„o no CQ referente a Nota Original
	If SD7->(dbSeek(cSeekSD7, .F.))
		Do While !SD7->(Eof()) .And. ;
			cSeekSD7 == SD7->D7_FILIAL+SD7->D7_NUMERO+SD7->D7_PRODUTO+SD7->D7_LOCAL
			If SD7->D7_TIPO >= 1.And.SD7->D7_TIPO <= 2.And.SD7->D7_ESTORNO # 'S'
				If (nPos:=aScan(aRet,{|x|x[1]==SD7->D7_TIPO.And.x[3]==SD7->D7_LOCDEST}))==0
					aAdd(aRet, {0,0,''})
					nPos := Len(aRet)
					aRet[nPos, 1] := SD7->D7_TIPO
					aRet[nPos, 3] := SD7->D7_LOCDEST
				EndIf
				aRet[nPos, 2] += SD7->D7_QTDE
			ElseIf SD7->D7_TIPO == 0
				aAdd(aRet, {0, SD7->D7_SALDO, SD7->D7_LOCAL})
			EndIf
			SD7->(dbSkip())
		EndDo
	EndIf
EndIf

SD1->(dbSetOrder(nSD1Ord))
SD1->(dbGoto(nSD1Rec))
SD7->(dbSetOrder(nSD7Ord))
SD7->(dbGoto(nSD7Rec))
dbSelectArea(cAliAnt)
dbSetOrder(nOrdAnt)
dbGoto(nRecAnt)

Return aRet


/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³F4LotePesq³Autor³Patricia A. Salomao       ³ Data ³24.07.00  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Ordena o ListBox de acordo com a opcao escolhida no ComboBox ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ExpC1	: Opcao escolhida no ComboBox ( 1-Lote / 2-Validade   ³±±
±±³          ³ExpA2	: Array contendo as informacoes dos Lotes             ³±±
±±³          ³ExpC3	: Objeto ListBox                                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³Funcao F4Lote()                                              ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Function F4LotePesq(cCombo,aArrayF4,oQual)

Local nPosicao := aScan(oQual:aHeaders,cCombo)

aArrayF4:=aSort( aArrayF4,,, { | x , y | x[nPosicao] < y[nPosicao] } )
oQual:Refresh()

Return ( .T. )

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³GravaSDG    ³Autor³Patricia A. Salomao    ³ Data ³26.10.2001 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Grava Custo do Movimento de Transporte                       ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ExpC1 - Alias do Arquivo                                     ³±±
±±³          ³ExpC2 - Tipo de Rateio (Veiculo/Viagem ou Frota)             ³±±
±±³          ³ExpA1 - Array contendo Rateio por Veiculo/Viagem ou por Frota³±±
±±³          ³ExpA2 - Array Contendo os Custos do Item rateado             ³±±
±±³          ³ExpC3 - Numero do Documento                                  ³±±
±±³          ³ExpC4 - Codigo da Despesa de Transporte                      ³±±
±±³          ³ExpL1 - Indica se o programa que originou o Rateio e' de Movi³±±
±±³          ³        mentos Internos (Mata240/Mata241)                    ³±±
±±³          ³ExpC5 - Numero da Sequencia                                  ³±±
±±³          ³ExpD1 - Data de Vencimento                                   ³±±
±±³          ³ExpN1 - Valor Cobrado                                        ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³Generico                                                     ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function GravaSDG(cAlias,cTpRateio,aDados,aCustoVei,cDoc,cCodDesp,lMovim,cSeqSDG,dDatVenc,nValCob)

DEFAULT cAlias    := ""
DEFAULT cTpRateio := ""
DEFAULT aDados    := {}
DEFAULT aCustoVei := {}
DEFAULT cDoc      := NextNumero("SDG",1,"DG_DOC",.T.)
DEFAULT cCodDesp  := ""
DEFAULT lMovim    := .F.
DEFAULT cSeqSDG   := &(cAlias+"->"+Subs(cAlias,2,2)+"_NUMSEQ")
DEFAULT dDatVenc  := dDataBase
DEFAULT nValCob   := 0

RecLock("SDG",.T.)
SDG->DG_ORIGEM   := cAlias
SDG->DG_FILIAL   := xFilial("SDG")
SDG->DG_DOC      := cDoc
SDG->DG_EMISSAO  := &(cAlias+"->"+Subs(cAlias,2,2)+"_EMISSAO")
SDG->DG_SEQMOV   := &(cAlias+"->"+Subs(cAlias,2,2)+"_NUMSEQ")
SDG->DG_TES      := &(cAlias+"->"+Subs(cAlias,2,2)+If(lMovim,"_TM","_TES"))
SDG->DG_CODDES   := cCodDesp
SDG->DG_NUMSEQ   := cSeqSDG
SDG->DG_SEQORI   := cSeqSDG
SDG->DG_DATVENC  := dDatVenc
SDG->DG_VALCOB   := nValCob
SDG->DG_SALDO    := nValCob
SDG->DG_STATUS   := StrZero(1,Len(SDG->DG_STATUS)) //-- Em Aberto

If Len(aDados) > 0
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Verifica se o rateio foi feito por Veiculo/Viagem ou por Frota.       ³
	//³cTpRateio == "V" - Rateio do Item da NF foi feito por Veiculo/Viagem. ³
	//³cTpRateio == "F" - Rateio do Item da NF foi feito por Frota           ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	SDG->DG_ITEM     := aDados[1]
	SDG->DG_CODVEI   := If(cTpRateio=="V", aDados[2], "")
	SDG->DG_FILORI   := If(cTpRateio=="V", aDados[3], "")
	SDG->DG_VIAGEM   := If(cTpRateio=="V", aDados[4], "")
	SDG->DG_TOTAL    := If(cTpRateio=="V", aDados[5], aCustoVei[1])
	If Len(aCustoVei) > 0
		SDG->DG_CUSTO1 := aCustoVei[1]
		SDG->DG_CUSTO2 := aCustoVei[2]
		SDG->DG_CUSTO3 := aCustoVei[3]
		SDG->DG_CUSTO4 := aCustoVei[4]
		SDG->DG_CUSTO5 := aCustoVei[5]
		SDG->DG_PERC   := aCustoVei[6]
	EndIf
EndIf

DT7->(dbSetOrder(1))
If 	DT7->(MsSeek(xFilial('DT7')+cCodDesp))
	SDG->DG_CLVL    := DT7->DT7_CLVL
	SDG->DG_ITEMCTA := DT7->DT7_ITEMCT
	SDG->DG_CONTA   := DT7->DT7_CONTA
	SDG->DG_CC      := DT7->DT7_CC
EndIf

MsUnlock()
Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³EstornaSDG  ³Autor³Patricia A. Salomao    ³ Data ³26.10.2001³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Estorna o Custo do Movimento de Transporte (Integracao TMS) ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ExpC1 - Alias do Arquivo                                    ³±±
±±³          ³ExpC2 - Numeracao Sequencial                                ³±±
±±³          ³ExpL1 - Contabilizacao On Line ?                            ³±±
±±³          ³ExpN1 - Cabecalho do Lancamento Contabil                    ³±±
±±³          ³ExpN2 - Total do Lancamento Contabil (@)                    ³±±
±±³          ³ExpC3 - Lote para Lancamento Contabil                       ³±±
±±³          ³ExpC4 - Nome do Programa                                    ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³Mata103/Mata240/Mata241/Tmsa070                             ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function EstornaSDG(cAlias,cNumSeq,lCtbOnLine,nHdlPrv,nTotalLcto,cLote,cProg)

Local aAreaAnt     := GetArea()
Local aAreaSDG     := SDG->(GetArea())
Default cNumSeq    := &(cAlias+"->"+Subs(cAlias,2,2)+"_NUMSEQ")
Default lCtbOnLine := .F.
Default nHdlPrv    := 0
Default nTotalLcto := 0
Default cLote      := ""
Default cProg      := 'MATA103'

dbSelectArea('SDG')
dbSetOrder(7)
If MsSeek(xFilial("SDG")+cAlias+cNumSeq)
	Do While !Eof() .And. DG_FILIAL+DG_ORIGEM+DG_SEQMOV == xFilial('SDG')+cAlias+cNumSeq
		If lCtbOnLine .And. nHdlPrv <> 0
		   If !Empty(SDG->DG_DTLANC)
				nTotalLcto+=DetProva(nHdlPrv,"902",cProg,cLote)
			EndIf
			If  !Empty(SDG->DG_DTLAEMI)
				nTotalLcto+=DetProva(nHdlPrv,"904",cProg,cLote)
			EndIf
		EndIf
		Reclock("SDG",.F.)
		dbDelete()
		MsUnLock()
		SDG->(dbSkip())
	EndDo
EndIf

RestArea(aAreaAnt)
RestArea(aAreaSDG)

Return

/*/
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡„o    ³F4NfOri   ³ Autor ³ Eduardo Riera         ³ Data ³07.02.2001 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³          ³Interface de visualizacao dos documentos de entrada/saida    ³±±
±±³          ³para devolucao                                               ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ExpC1: Nome da rotina chamadora                              ³±±
±±³          ³ExpN2: Numero da linha da rotina chamadora              (OPC)³±±
±±³          ³ExpC4: Nome do campo GET em foco no momento             (OPC)³±±
±±³          ³ExpC5: Codigo do Cliente/Fornecedor                          ³±±
±±³          ³ExpC6: Loja do Cliente/Fornecedor                            ³±±
±±³          ³ExpC7: Codigo do Produto                                     ³±±
±±³          ³ExpC8: Local a ser considerado                               ³±±
±±³          ³ExpN9: Numero do recno do SD1/SD2                            ³±±
±±³          ³ExpD10: Date of Invoice									   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³Nenhum                                                       ³±±
±±³          ³                                                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡„o ³Esta rotina tem como objetivo atualizar os eventos vinculados³±±
±±³          ³a uma solicitacao de compra:                                 ³±±
±±³          ³A) Atualizacao das tabelas complementares.                   ³±±
±±³          ³B) Atualizacao das informacoes complementares a SC           ³±±
±±³          ³                                                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Materiais                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Function F4NfOri(cRotina,nLinha,cReadVar,cCliFor,cLoja,cProduto,cPrograma,cLocal,nRecSD2,nRecSD1,dInvDate,cIndPres,cCodA1U,nLinAtv)

Local aArea     := GetArea() 
Local aAreaSF1  := SF1->(GetArea())
Local aStruTRB  := {}
Local aStruSD1  := {}
Local aStruSD2  := {}
Local aStruSF1  := {}
Local aStruSF2  := {}
Local aValor    := {}
Local aOrdem    := {AllTrim(RetTitle("F2_DOC"))+"+"+AllTrim(RetTitle("F2_SERIE")),AllTrim(RetTitle("F2_EMISSAO"))}
Local aChave    := {}
Local aPesq     := {}
Local aNomInd   := {}
Local aObjects  := {}
Local aInfo     := {}
Local aPosObj   := {}
Local aSize     := MsAdvSize( .F. )
Local aHeadTRB  := {}
Local aSavHead  := aClone(aHeader)
Local cAliasSD1 := "SD1"
Local cAliasSD2 := "SD2"
Local cAliasSF1 := "SF1"
Local cAliasSF2 := "SF2"
Local cAliasSF4 := "SF4"
Local cAliasTRB := "F4NFORI"
Local cNomeTrb  := ""
Local cQuery    := ""
Local cCombo    := ""
Local cTexto1   := ""
Local cTexto2   := ""
Local lRetorno  := .F.
Local lSkip     := .F.
Local cTpCliFor := "C"
Local nX        := 0
Local nY        := 0
Local nSldQtd   := 0
Local nSldQtd2  := 0
Local nSldLiq   := 0
Local nSldBru   := 0
Local nHdl      := GetFocus()
Local nOpcA     := 0
Local nPNfOri   := 0
Local nPSerOri  := 0
Local nPItemOri := 0
Local nPLocal   := 0
Local nPPrUnit  := 0
Local nPPrcVen  := 0
Local nValDes   := 0
Local nPQuant   := 0
Local nPQuant2UM:= 0
Local nPLoteCtl := 0
Local nPNumLote := 0
Local nPDtValid := 0
Local nPPotenc  := 0
Local nPValor   := 0
Local nPValDesc := 0
Local nPDesc    := 0
Local nPOrigem  := 0
Local nPDespacho:= 0
Local nPTES     := 0
Local nPCf		:= 0
Local nPProvEnt := 0
Local nPConcept := 0
Local nPClasFis := 0
Local nD1Fabric := 0
Local nPPeso	:= 0
Local nFciCod   := 0
Local nPCC      := 0 // Posición Centro de Costo
Local nPConta   := 0 
Local nPItemCTa := 0 
Local nPCLVL    := 0 
Local nPOrProd  := 0
Local nPNIT	    := 0
Local xPesq     := ""
Local oDlg
Local oCombo
Local oGet
Local oGetDb
Local oPanel
Local cFiltraQry:=""
Local lFiltraQry:=ExistBlock('F4NFORI') 
Local lUsaNewKey:= TamSX3("F2_SERIE")[1] == 14 // Verifica se o novo formato de gravacao do Id nos campos _SERIE esta em uso
Local lUUID     := cPrograma == "A100" .AND. cPaisLoc == "MEX"  .and. funname() $ "MATA465N" // Activación de campos
Local lDescSai  := IIF(cPaisLoc == "BRA",SuperGetMV("MV_DESCSAI",.F.,"2"),SuperGetMV("MV_DESCSAI",.F.,"1")) == "1"
Local cCtrl     := CHR(13) + CHR(10) // Salto de línea para los UUID Relacionados
Local nUniaduD1 	:= 0
Local nUsdaduD1 	:= 0
Local nValaduD1 	:= 0
Local nCanaduD1 	:= 0
Local nFraccaD1 	:= 0
Local cProvFE		:= SuperGetMV("MV_PROVFE",,"")
Local cPreSD        := ""
Local lCSTOri       := SuperGetMv("MV_CSTORI",.F.,.F.)
Local lCFDUso       := IIf(Alltrim(GetMv("MV_CFDUSO", .T., "1"))<>"0",.T.,.F.)
Local nDesIt        := 0
Local nDesPro       := 0
Local nTpDocS 	    := IIf(Type("aCfgNF")=="A", aCfgNF[1], 0) 
Local lNtDbCrd      := cPaisLoc == "COL" .And. cPrograma == "A466" .and. FunName() $ "MATA466N" 
Local nMoedaOri := 0
Local nMdaConv := 0    
Local nTotalOri := 0
Local nTtalConv := 0
Local lInterC5F1:= SC5->(FieldPos("C5_INDPRES")) > 0 .And. SC5->(FieldPos("C5_CODA1U")) > 0 .And. ;
				   SF1->(FieldPos("F1_INDPRES")) > 0 .And. SF1->(FieldPos("F1_CODA1U")) > 0
Local bValDcTrns := Nil
Local lPQTSegUM	 := .T.
Local lDtCpoLote := SDT->(FieldPos("DT_LOTE")) > 0 .And. SDT->(FieldPos("DT_DTVALID")) > 0 .And. SDT->(FieldPos("DT_DFABRIC")) > 0
Local dDTB8DTVALID := CtoD("//")
Local dDTB8DFABRIC := CtoD("//")
Local lDocOriNC  := (IsInCallStack("MATA466N") .And. IsInCallStack("LocXStock"))
Local nPosEC05DB
Local nPosEC05CR
Local nPosEC06DB
Local nPosEC06CR
Local nPosEC07DB
Local nPosEC07CR
Local nPosEC08DB
Local nPosEC08CR
Local nPosEC09DB
Local nPosEC09CR

DEFAULT cReadVar := ReadVar()
DEFAULT dInvDate := dDataBase
DEFAULT cIndPres := ""
DEFAULT cCodA1U	 := ""
DEFAULT nLinAtv	 := 0

PRIVATE aRotina  := {}

DbSelectArea("SD1")
nPosEC05DB:= FieldPos("D1_EC05DB")
nPosEC05CR:= FieldPos("D1_EC05CR")
nPosEC06DB:= FieldPos("D1_EC06DB")
nPosEC06CR:= FieldPos("D1_EC06CR")
nPosEC07DB:= FieldPos("D1_EC07DB")
nPosEC07CR:= FieldPos("D1_EC07CR")
nPosEC08DB:= FieldPos("D1_EC08DB")
nPosEC08CR:= FieldPos("D1_EC08CR")
nPosEC09DB:= FieldPos("D1_EC09DB")
nPosEC09CR:= FieldPos("D1_EC09CR")

For nX := 1 To 11	// Walk_Thru
	aAdd(aRotina,{"","",0,0})
Next

If ("_NFORI"$cReadVar) .Or. ( lUsaNewKey .And. ("_SERIORI"$cReadVar .Or. "_ITEMORI"$cReadVar )  )
	Do Case
		Case cPrograma $ "A440|A466|LOJA920|ARUS466"
			cTpCliFor := "F"
			aChave    := {"D1_DOC+D1_SERIE","D1_EMISSAO"}
			aPesq     := {{Space(Len(SD1->D1_DOC+SD1->D1_SERIE)),"@!"},{Ctod(""),"@!"}}
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Montagem do arquivo temporario dos itens do SD1                     ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			dbSelectArea("SX3")
			dbSetOrder(1)
			MsSeek("SD1")
			While !Eof() .And. SX3->X3_ARQUIVO == "SD1"
				If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL .And.;
					Trim(SX3->X3_CAMPO) <> "D1_COD" .And.;
					SX3->X3_CONTEXT<>"V" .And.;
					SX3->X3_TIPO<>"M" ) .Or.;
					Trim(SX3->X3_CAMPO) == "D1_DOC" .Or.;
					Trim(SX3->X3_CAMPO) == "D1_SERIE"  .Or.;
					Trim(SX3->X3_CAMPO) == "D1_EMISSAO" .Or.;
					Trim(SX3->X3_CAMPO) == "D1_TIPO"
					aadd(aHeadTrb,{ TRIM(X3Titulo()),;
						SX3->X3_CAMPO,;
						SX3->X3_PICTURE,;
						SX3->X3_TAMANHO,;
						SX3->X3_DECIMAL,;
						SX3->X3_VALID,;
						SX3->X3_USADO,;
						SX3->X3_TIPO,;
						SX3->X3_ARQUIVO,;
						SX3->X3_CONTEXT,;
						IIf(AllTrim(SX3->X3_CAMPO)$"D1_DOC#D1_SERIE#D1_ITEM#D1_TIPO","00",SX3->X3_ORDEM) })
					aadd(aStruTRB,{SX3->X3_CAMPO,SX3->X3_TIPO,SX3->X3_TAMANHO,SX3->X3_DECIMAL,IIf(AllTrim(SX3->X3_CAMPO)$"D1_DOC#D1_SERIE#D1_ITEM","00",SX3->X3_ORDEM)})
					If Trim(SX3->X3_CAMPO) == "D1_VUNIT"
						aadd(aHeadTrb,{ OemToAnsi(STR0025),; //"Valor Liquido"
							"D1_V_UNIT2",;
							SX3->X3_PICTURE,;
							SX3->X3_TAMANHO,;
							SX3->X3_DECIMAL,;
							SX3->X3_VALID,;
							SX3->X3_USADO,;
							SX3->X3_TIPO,;
							SX3->X3_ARQUIVO,;
							SX3->X3_CONTEXT,;
							IIf(AllTrim(SX3->X3_CAMPO)$"D1_DOC#D1_SERIE#D1_ITEM#D1_TIPO","00",SX3->X3_ORDEM) })
						aadd(aStruTRB,{"D1_V_UNIT2",SX3->X3_TIPO,SX3->X3_TAMANHO,SX3->X3_DECIMAL,IIf(AllTrim(SX3->X3_CAMPO)$"D1_DOC#D1_SERIE#D1_ITEM","00",SX3->X3_ORDEM)})
					EndIf
				EndIf
				dbSelectArea("SX3")
				dbSkip()
			EndDo
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³Walk-Thru                   ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			ADHeadRec("SD1",aHeadTrb)
			aSize(aHeadTrb[Len(aHeadTrb)-1],11)
			aSize(aHeadTrb[Len(aHeadTrb)],11)
			aHeadTrb[Len(aHeadTrb)-1,11] := "ZY"
			aHeadTrb[Len(aHeadTrb),11]	 := "ZZ"
			aadd(aStruTRB,{"D1_ALI_WT","C",3,0,"ZY"})
			aadd(aStruTRB,{"D1_REC_WT","N",18,0,"ZZ"})

			aHeadTrb := aSort(aHeadTrb,,,{|x,y| x[11] < y[11]})
			aStruTrb := aSort(aStruTrb,,,{|x,y| x[05] < y[05]})

			cNomeTrb := FWOpenTemp(cAliasTRB,aStruTRB,,.T.)

			dbSelectArea(cAliasTRB)
			For nX := 1 To Len(aChave)
				aAdd( aNomInd , StrTran( (SubStr( cNomeTrb, 1 , 7 ) + Chr( 64 + nX ) ), "_" , "") )
				IndRegua(cAliasTRB,aNomInd[nX],aChave[nX])
			Next nX
			dbClearIndex()
			For nX := 1 To Len(aNomInd)
				dbSetIndex(aNomInd[nX])
			Next nX
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Atualizacao do arquivo temporario com base nos itens do SD1         ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			If lFiltraQry
				cFiltraQry	:=	ExecBlock('F4NFORI',.F.,.F.,{"SD1",cPrograma,cClifor,cLoja})
				If ValType(cFiltraQry) <> 'C'
					cFiltraQry	:=	''
				Endif
			Endif
			dbSelectArea("SF1")
			dbSetOrder(2)
		    cAliasSF1 := "F4NFORI_SQL"
		    cAliasSD1 := "F4NFORI_SQL"
		    cAliasSF4 := "F4NFORI_SQL"
		    aStruSF1 := SF1->(dbStruct())
		    aStruSD1 := SD1->(dbStruct())
			cQuery := "	SELECT	"
			cQuery += "		SF4.F4_PODER3,	"
			cQuery += "		SD1.R_E_C_N_O_ D1_REC_WT,	"
			cQuery += "		SF1.F1_FILIAL, SF1.F1_FORNECE, SF1.F1_LOJA, SF1.F1_TIPO, SF1.F1_DOC, SF1.F1_SERIE,	"
			cQuery += "		SD1.D1_FILIAL, SD1.D1_COD, SD1.D1_TIPO, SD1.D1_DOC, SD1.D1_SERIE, SD1.D1_FILIAL, SD1.D1_FORNECE,	"
			cQuery += "		SD1.D1_LOJA, SD1.D1_QTDEDEV, SD1.D1_VALDEV, SD1.D1_ORIGLAN, SD1.D1_TES	"
			If cPaisLoc <> "BRA"
				cQuery += "	,SD1.D1_TIPODOC	"
			EndIf
			For nX := 1 To Len(aStruTRB)
				If !"D1_REC_WT"$aStruTRB[nX][1] .And. !"D1_ALI_WT"$aStruTRB[nX][1] .And. !"D1_V_UNIT2"$aStruTRB[nX][1]
					cQuery += ","+aStruTRB[nX][1]
				EndIf
			Next nX
			cQuery += "		FROM	" +	RetSqlName("SF1")+" SF1,	"
			cQuery +=  					RetSqlName("SD1")+"	SD1,	"
			cQuery +=  RetSqlName("SF4")+" SF4 "
			cQuery += "		WHERE "
			cQuery += "			SF1.F1_FILIAL = '"+xFilial("SF1")+"' AND	"
			cQuery += "			SF1.F1_FORNECE = '"+cCliFor+"' AND	"
			cQuery += "			SF1.F1_LOJA = '"+cLoja+"' AND	"
			cQuery += "			SF1.D_E_L_E_T_=' ' AND	"
			cQuery += "			SD1.D1_FILIAL='"+xFilial("SD1")+"' AND	"
			cQuery += "			SD1.D1_FORNECE=SF1.F1_FORNECE AND	"
			cQuery += "			SD1.D1_LOJA=SF1.F1_LOJA AND	"
			cQuery += "			SD1.D1_DOC=SF1.F1_DOC AND	"
			cQuery += "			SD1.D1_SERIE=SF1.F1_SERIE AND	"
			cQuery += "			SD1.D1_TIPO=SF1.F1_TIPO AND	"
			cQuery += "			SD1.D1_COD='"+cProduto+"' AND	"
			cQuery += "			SD1.D1_ORIGLAN<>'LF' AND	"
			cQuery += "			SD1.D_E_L_E_T_=' ' AND	"
			cQuery += "			SF4.F4_FILIAL='"+xFilial("SF4")+"' AND	"
			cQuery += "			SF4.F4_CODIGO=SD1.D1_TES AND	"
			cQuery += "			SF4.D_E_L_E_T_=' '	"
			cQuery += "		AND	SF1.F1_EMISSAO <= '" + DTOS(dInvDate) + "'"
		    If !Empty(cFiltraQry)
				cQuery += "	AND	"+cFiltraQry
	        Endif
			If lNtDbCrd .AND. FINDFUNCTION( "lxObtnFltr" )
				cQuery += lxObtnFltr(nTpDocS,.T.)
			EndIf
			cQuery += "	ORDER BY	"+SqlOrder(SF1->(IndexKey()))

			cQuery := ChangeQuery(cQuery)
			dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliasSF1,.T.,.T.)

			For nX := 1 To Len(aStruSD1)
				If aStruSD1[nX][2] <> "C"
					TcSetField(cAliasSF1,aStruSD1[nX][1],aStruSD1[nX][2],aStruSD1[nX][3],aStruSD1[nX][4])
				EndIf
			Next nX
			For nX := 1 To Len(aStruSF1)
				If aStruSF1[nX][2] <> "C"
					TcSetField(cAliasSF1,aStruSF1[nX][1],aStruSF1[nX][2],aStruSF1[nX][3],aStruSF1[nX][4])
				EndIf
			Next nX

			lPQTSegUM := (cAliasTRB)->(ColumnPos("D1_QTSEGUM")) > 0

			While !Eof() .And. (cAliasSF1)->F1_FILIAL = xFilial("SF1") .And.;
				(cAliasSF1)->F1_FORNECE == cCliFor .And.;
				(cAliasSF1)->F1_LOJA == cLoja
				lSkip := .F.
				While !Eof() .And. xFilial("SD1") == (cAliasSD1)->D1_FILIAL .And.;
					cProduto == (cAliasSD1)->D1_COD .And.;
					(cAliasSF1)->F1_DOC == (cAliasSD1)->D1_DOC .And.;
					(cAliasSF1)->F1_SERIE == (cAliasSD1)->D1_SERIE .And.;
					(cAliasSF1)->F1_FORNECE == (cAliasSD1)->D1_FORNECE .And.;
					(cAliasSF1)->F1_LOJA == (cAliasSD1)->D1_LOJA

					If (cAliasSF4)->F4_PODER3 == "N" .And. !Empty((cAliasSD1)->D1_TES) .And. (cAliasSD1)->D1_ORIGLAN<>"LF" .And. (cAliasSD1)->D1_TIPO<>"D"

						If Empty(cFiltraQry) .Or.  ValType(cFiltraQry) == "C"
				        	If cPaisloc = "BRA" .or. Iif( "MATA102" $ Funname(), (cAliasSD1)->D1_TIPODOC >= "50", (cAliasSD1)->D1_TIPODOC < "50" )
								aValor := A410SNfOri((cAliasSD1)->D1_FORNECE,(cAliasSD1)->D1_LOJA,(cAliasSD1)->D1_DOC,(cAliasSD1)->D1_SERIE,(cAliasSD1)->D1_ITEM,(cAliasSD1)->D1_COD,,cLocal,cAliasSD1,Nil,Nil,cPrograma $ "A440")
								nSldQtd:= aValor[1]
								nSldQtd2:=ConvUm((cAliasSD1)->D1_COD,nSldQtd,0,2)
								nSldLiq:= aValor[2]
								If cPrograma $ "A466|ARUS466" .And. cPaisLoc <>"BRA" .And. nSldLiq == 0 .And. ((cAliasSD1)->D1_TOTAL-(cAliasSD1)->D1_VALDESC) == 0  // Item com desconto total
									nSldBru:= (cAliasSD1)->D1_TOTAL
								Else							
									nSldBru := 	IIf(cPrograma $ "A440" .And. (cAliasSD1)->D1_VALDESC > 0,;
													nSldLiq+a410Arred(((cAliasSD1)->D1_VUNIT-(nSldLiq/IIf(nSldQtd==0,1,nSldQtd)))*a410Arred(nSldQtd,"C6_QTDVEN"),"C6_VALDESC"),;
													nSldLiq+A410Arred(nSldLiq*(cAliasSD1)->D1_VALDESC/((cAliasSD1)->D1_TOTAL-(cAliasSD1)->D1_VALDESC),"C6_VALOR"))
								EndIf
								If ( IIF( cPrograma $ "LOJA920", nSldQtd <> 0,  nSldQtd <> 0 .Or. nSldLiq <> 0 ) )
									If Empty(aValor[3])
										RecLock(cAliasTRB,.T.)
										For nX := 1 To Len(aStruTRB)
											//FieldPos necessário para não realizar o FieldPut nos campos virtuais. Ex.: D1_V_UNIT2
											If !(AllTrim(aStruTRB[nX][1]) $ "D1_ALI_WT|D1_REC_WT|D1_V_UNIT2")
												If (cAliasSD1)->(FieldPos(aStruTRB[nX][1]))<>0
													(cAliasTRB)->(FieldPut(nX,(cAliasSD1)->(FieldGet(FieldPos(aStruTRB[nX][1])))))
												EndIf
											EndIf
										Next nX
										If cPaisLoc == "BRA" .Or. (!lDocOriNC .Or. nSldQtd <> (cAliasSD1)->D1_QUANT)
											(cAliasTRB)->D1_QUANT := a410Arred(nSldQtd,"C6_QTDVEN")
										EndIf
										If lPQTSegUM
											(cAliasTRB)->D1_QTSEGUM:= a410Arred(nSldQtd2,"C6_UNSVEN")
										EndIf
										If cPaisLoc == "BRA" .Or. (!lDocOriNC .Or. nSldQtd <> (cAliasSD1)->D1_QUANT)
											(cAliasTRB)->D1_TOTAL := a410Arred(nSldBru,"C6_VALOR")
											(cAliasTRB)->D1_VUNIT := a410Arred(nSldBru/IIf(nSldQtd==0,1,nSldQtd),"C6_PRCVEN")
										EndIf
										If Abs((cAliasTRB)->D1_VUNIT-(cAliasSD1)->D1_VUNIT)<0.01
											(cAliasTRB)->D1_VUNIT := (cAliasSD1)->D1_VUNIT
										EndIf
										If (cAliasTRB)->D1_VALDESC>0
											(cAliasTRB)->D1_V_UNIT2 := a410Arred(nSldLiq/IIf(nSldQtd==0,1,nSldQtd),"C6_PRCVEN")
											(cAliasTRB)->D1_VALDESC := a410Arred(((cAliasSD1)->D1_VUNIT-(nSldLiq/IIf(nSldQtd==0,1,nSldQtd)))*a410Arred(nSldQtd,"C6_QTDVEN"),"C6_VALDESC")
										Else
											(cAliasTRB)->D1_V_UNIT2:= (cAliasTRB)->D1_VUNIT
										EndIf
										(cAliasTRB)->D1_ALI_WT := "SD1"
										If cPaisloc $ "RUS|ARG" 
											(cAliasTRB)->D1_REC_WT:= (cAliasSD1)->D1_REC_WT
										EndIf
										MsUnLock()
									Else
										For nY := 1 To Len(aValor[3])
											RecLock(cAliasTRB,.T.)
											For nX := 1 To Len(aStruTRB)
												If !(AllTrim(aStruTRB[nX][1]) $ "D1_ALI_WT|D1_REC_WT|D1_V_UNIT2")
													(cAliasTRB)->(FieldPut(nX,(cAliasSD1)->(FieldGet(FieldPos(aStruTRB[nX][1])))))
												EndIf
											Next nX
											(cAliasTRB)->D1_VUNIT := a410Arred(nSldBru/IIf(nSldQtd==0,1,nSldQtd),"C6_PRCVEN")
											If Abs((cAliasTRB)->D1_VUNIT-(cAliasSD1)->D1_VUNIT)<0.01
												(cAliasTRB)->D1_VUNIT := (cAliasSD1)->D1_VUNIT
											EndIf
											If (cAliasTRB)->D1_VALDESC>0
												(cAliasTRB)->D1_V_UNIT2	:= a410Arred(nSldLiq/IIf(nSldQtd==0,1,nSldQtd),"C6_PRCVEN")
											Else
												(cAliasTRB)->D1_V_UNIT2:= (cAliasTRB)->D1_VUNIT
											EndIf
											(cAliasTRB)->D1_QUANT  := a410Arred(aValor[3][nY][1],"C6_QTDVEN")
											If lPQTSegUM
												(cAliasTRB)->D1_QTSEGUM:= a410Arred(aValor[3][nY][2],"C6_UNSVEN")
											EndIf
											(cAliasTRB)->D1_TOTAL  := a410Arred(aValor[3][nY][1]*(cAliasTRB)->D1_V_UNIT2,"C6_VALOR")
											(cAliasTRB)->D1_LOCAL  := aValor[3][nY][3]
											(cAliasTRB)->D1_ALI_WT := "SD1"
											If cPaisloc $ "RUS|ARG" 
												(cAliasTRB)->D1_REC_WT:= (cAliasSD1)->D1_REC_WT
											EndIf
											MsUnLock()
										Next nY
									EndIf
								EndIf
							EndIf
						Endif
				    EndIf
					dbSelectArea(cAliasSD1)
					dbSkip()
					lSkip := .T.
				EndDo
			EndDo
			dbSelectArea(cAliasSF1)
			dbCloseArea()
			dbSelectArea("SF1")
		OtherWise
			cTpCliFor := "C"
			aChave    := {"D2_DOC+D2_SERIE","D2_EMISSAO"}
			aPesq     := {{Space(Len(SD2->D2_DOC+SD2->D2_SERIE)),"@!"},{Ctod(""),"@!"}}
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Montagem do arquivo temporario dos itens do SF2                     ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			If lUUID
				dbSelectArea("SX3")
				dbSetOrder(1)
				MsSeek("SF2")
				While !Eof() .And. SX3->X3_ARQUIVO == "SF2"
					If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL .And.;
						SX3->X3_CONTEXT<>"V") .And.;
						Trim(SX3->X3_CAMPO) == "F2_FECTIMB" .Or.;
						Trim(SX3->X3_CAMPO) == "F2_UUID"
						aadd(aHeadTrb,{ TRIM(X3Titulo()),;
							SX3->X3_CAMPO,;
							SX3->X3_PICTURE,;
							SX3->X3_TAMANHO,;
							SX3->X3_DECIMAL,;
							SX3->X3_VALID,;
							SX3->X3_USADO,;
							SX3->X3_TIPO,;
							SX3->X3_ARQUIVO,;
							SX3->X3_CONTEXT,;
							IIf(AllTrim(SX3->X3_CAMPO)$"D1_DOC#D1_SERIE#D1_ITEM#D1_TIPO","00",SX3->X3_ORDEM) })
						aadd(aStruTRB,{SX3->X3_CAMPO,SX3->X3_TIPO,SX3->X3_TAMANHO,SX3->X3_DECIMAL,IIf(AllTrim(SX3->X3_CAMPO)$"D1_DOC#D1_SERIE#D1_ITEM","00",SX3->X3_ORDEM)})
					EndIf
					SX3->(dbSkip())
				EndDo
			EndIf
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Montagem do arquivo temporario dos itens do SD2                     ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			dbSelectArea("SX3")
			dbSetOrder(1)
			MsSeek("SD2")
			While !Eof() .And. SX3->X3_ARQUIVO == "SD2"
				If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL .And.;
					Trim(SX3->X3_CAMPO) <> "D2_COD" .And.;
					SX3->X3_CONTEXT <> "V"  .And.;
					SX3->X3_TIPO<>"M" ) .Or.;
					Trim(SX3->X3_CAMPO) == "D2_DOC" .Or.;
					Trim(SX3->X3_CAMPO) == "D2_SERIE"  .Or.;
					Trim(SX3->X3_CAMPO) == "D2_EMISSAO" .Or.;
					Trim(SX3->X3_CAMPO) == "D2_TIPO" .Or.;
					Trim(SX3->X3_CAMPO) == "D2_PRUNIT" .Or. ;
					Trim(SX3->X3_CAMPO) == "D2_DESCZFR"
					Aadd(aHeadTrb,{ TRIM(X3Titulo()),;
						SX3->X3_CAMPO,;
						SX3->X3_PICTURE,;
						SX3->X3_TAMANHO,;
						SX3->X3_DECIMAL,;
						SX3->X3_VALID,;
						SX3->X3_USADO,;
						SX3->X3_TIPO,;
						SX3->X3_ARQUIVO,;
						SX3->X3_CONTEXT,;
						IIf(AllTrim(SX3->X3_CAMPO)$"D2_DOC#D2_SERIE#D2_ITEM#D2_TIPO","00",SX3->X3_ORDEM) })
					aadd(aStruTRB,{SX3->X3_CAMPO,SX3->X3_TIPO,SX3->X3_TAMANHO,SX3->X3_DECIMAL,IIf(AllTrim(SX3->X3_CAMPO)$"D2_DOC#D2_SERIE#D2_ITEM","00",SX3->X3_ORDEM)})
				EndIf
				dbSelectArea("SX3")
				dbSkip()
			EndDo
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³Walk-Thru                   ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			ADHeadRec("SD2",aHeadTrb)
			aSize(aHeadTrb[Len(aHeadTrb)-1],11)
			aSize(aHeadTrb[Len(aHeadTrb)],11)
			aHeadTrb[Len(aHeadTrb)-1,11] := "ZX"
			aHeadTrb[Len(aHeadTrb),11]	 := "ZY"
			aadd(aStruTRB,{"D2_ALI_WT","C",3,0,"ZX"})
			aadd(aStruTRB,{"D2_REC_WT","N",18,0,"ZY"})

			aadd(aStruTRB,{"D2_TOTAL2","N",18,2,"ZZ"})
			aHeadTrb := aSort(aHeadTrb,,,{|x,y| x[11] < y[11]})
			aStruTrb := aSort(aStruTrb,,,{|x,y| x[05] < y[05]})

			cNomeTrb := FWOpenTemp(cAliasTRB,aStruTRB,,.T.)

			dbSelectArea(cAliasTRB)
			For nX := 1 To Len(aChave)
				aAdd( aNomInd , StrTran( (SubStr( cNomeTrb, 1 , 7 ) + Chr( 64 + nX ) ), "_" , "") )
				IndRegua(cAliasTRB,aNomInd[nX],aChave[nX])
			Next nX
			dbClearIndex()
			For nX := 1 To Len(aNomInd)
				dbSetIndex(aNomInd[nX])
			Next nX
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Atualizacao do arquivo temporario com base nos itens do SD2         ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			If lFiltraQry
				cFiltraQry	:=	ExecBlock('F4NFORI',.F.,.F.,{"SD2",cPrograma,cClifor,cLoja})
				If ValType(cFiltraQry) <> 'C'
					cFiltraQry	:=	''
				Endif
			Endif
			dbSelectArea("SF2")
			dbSetOrder(2)

		    cAliasSF2 := "F4NFORI_SQL"
		    cAliasSD2 := "F4NFORI_SQL"
		    cAliasSF4 := "F4NFORI_SQL"
		    aStruSF2 := SF2->(dbStruct())
		    aStruSD2 := SD2->(dbStruct())
			cQuery := "SELECT SF4.F4_PODER3,SD2.R_E_C_N_O_ SD2RECNO,"
			cQuery += "SF2.F2_FILIAL,SF2.F2_CLIENTE,SF2.F2_LOJA,"
			If lUUID
				cQuery += "SF2.F2_FECTIMB,SF2.F2_UUID, "
			EndIF
			cQuery += "SF2.F2_TIPO,SF2.F2_DOC,SF2.F2_SERIE,SD2.D2_FILIAL,SD2.D2_COD,"
			cQuery += "SD2.D2_TIPO,SD2.D2_DOC,SD2.D2_SERIE,SD2.D2_FILIAL,SD2.D2_CLIENTE,"
			cQuery += "SD2.D2_LOJA,SD2.D2_QTDEDEV,SD2.D2_VALDEV,SD2.D2_ORIGLAN,SD2.D2_TES,SD2.D2_TIPOREM "
			If cPaisLoc <> "BRA"
				cQuery += ",SD2.D2_TIPODOC "
			EndIf
			For nX := 1 To Len(aStruTRB)
				If !"D2_REC_WT"$aStruTRB[nX][1] .And. !"D2_ALI_WT"$aStruTRB[nX][1] .And. !"D2_TOTAL2"$aStruTRB[nX][1]
					cQuery += ","+aStruTRB[nX][1]
				EndIf
			Next nX
			cQuery += " FROM "+RetSqlName("SF2")+" SF2,"
			cQuery +=  RetSqlName("SF4")+" SF4,"			
			cQuery +=  RetSqlName("SD2")+" SD2 "

			If lInterC5F1 .And. Type("cFormul") == "C" .And. cFormul == "S" .And. nLinAtv > 0 
				cQuery += "LEFT JOIN "+ RetSqlName("SC5")+" SC5 ON "
				cQuery += "SC5.C5_FILIAL='"+xFilial("SC5")+"' AND "
				cQuery += "SC5.C5_NUM=SD2.D2_PEDIDO AND "
				cQuery += "SC5.C5_CLIENTE=SD2.D2_CLIENTE AND "
				cQuery += "SC5.C5_LOJACLI=SD2.D2_LOJA AND "
				cQuery += "SC5.C5_INDPRES IN ('"+SubStr(cIndPres,1,1)+"',' ') AND "
				cQuery += "SC5.C5_CODA1U IN ('" + cCodA1U + "',' ') AND "
				cQuery += "SC5.D_E_L_E_T_ = ' ' "  
			Endif

			cQuery += "WHERE "
			cQuery += "SF2.F2_FILIAL = '"+xFilial("SF2")+"' AND "
			cQuery += "SF2.F2_CLIENTE = '"+cCliFor+"' AND "
			cQuery += "SF2.F2_LOJA = '"+cLoja+"' AND "
			If lUUID
				cQuery += "SF2.F2_UUID <> ' ' AND "
			EndIF
			cQuery += "SF2.D_E_L_E_T_=' ' AND "
			cQuery += "SD2.D2_FILIAL='"+xFilial("SD2")+"' AND "
			cQuery += "SD2.D2_CLIENTE=SF2.F2_CLIENTE AND "
			cQuery += "SD2.D2_LOJA=SF2.F2_LOJA AND "
			cQuery += "SD2.D2_DOC=SF2.F2_DOC AND "
			cQuery += "SD2.D2_SERIE=SF2.F2_SERIE AND "
			cQuery += "SD2.D2_TIPO=SF2.F2_TIPO AND "

			If IsInCallStack("Documentos") .And. Type("cTipo") == "U"
				cTipo := SDS->DS_TIPO
			EndIf

			IF cTipo == "N" // Tipo da nota de entrada
				cQuery += "F2_TIPO = 'B' AND " // Tipo da nota de saída
			ELSE
			   cQuery += "F2_TIPO not in('B','D') AND " // Tipo da nota de saída
			ENDIF

			cQuery += "SD2.D2_COD='"+cProduto+"' AND "
			cQuery += "SD2.D2_ORIGLAN<>'LF' AND "
			cQuery += "SD2.D_E_L_E_T_=' ' AND "
			cQuery += "SF4.F4_FILIAL='"+xFilial("SF4")+"' AND "
			cQuery += "SF4.F4_CODIGO=SD2.D2_TES AND "
			cQuery += "SF4.D_E_L_E_T_=' ' "
			cQuery += "	AND	SF2.F2_EMISSAO <= '" + DTOS(dInvDate) + "' "

	      	If !Empty(cFiltraQry)
				cQuery += " AND "+cFiltraQry
      		Endif
			cQuery += "ORDER BY "+SqlOrder(SF2->(IndexKey()))

			cQuery := ChangeQuery(cQuery)

			dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliasSF2,.T.,.T.)

			For nX := 1 To Len(aStruSD2)
				If aStruSD2[nX][2] <> "C"
					TcSetField(cAliasSF2,aStruSD2[nX][1],aStruSD2[nX][2],aStruSD2[nX][3],aStruSD2[nX][4])
				EndIf
			Next nX
			For nX := 1 To Len(aStruSF2)
				If aStruSF2[nX][2] <> "C"
					TcSetField(cAliasSF2,aStruSF2[nX][1],aStruSF2[nX][2],aStruSF2[nX][3],aStruSF2[nX][4])
				EndIf
			Next nX

			While !Eof() .And. (cAliasSF2)->F2_FILIAL = xFilial("SF2") .And.;
				(cAliasSF2)->F2_CLIENTE == cCliFor .And.;
				(cAliasSF2)->F2_LOJA == cLoja
				lSkip := .F.

				While !Eof() .And. xFilial("SD2") == (cAliasSD2)->D2_FILIAL .And.;
					cProduto == (cAliasSD2)->D2_COD .And.;
					(cAliasSF2)->F2_DOC == (cAliasSD2)->D2_DOC .And.;
					(cAliasSF2)->F2_SERIE == (cAliasSD2)->D2_SERIE .And.;
					(cAliasSF2)->F2_CLIENTE == (cAliasSD2)->D2_CLIENTE .And.;
					(cAliasSF2)->F2_LOJA == (cAliasSD2)->D2_LOJA


					If (cAliasSD2)->D2_TIPO ==(cAliasSF2)->F2_TIPO .And. ( (cAliasSF4)->F4_PODER3 == "N" .Or. ((cAliasSF4)->F4_PODER3 == "R" .And. (cAliasSD2)->D2_TIPOREM == "A")) .And. !Empty((cAliasSD2)->D2_TES) .And. (cAliasSD2)->D2_ORIGLAN<>"LF" .And. (cAliasSD2)->D2_TIPO<>"D"

			      		If Empty(cFiltraQry) .Or. ValType(cFiltraQry) == "C"
			         		If cPaisloc = "BRA" .or. Iif( "MATA462" $ Funname(), (cAliasSD2)->D2_TIPODOC >= "50", (cAliasSD2)->D2_TIPODOC < "50" )
								nSldQtd:= (cAliasSD2)->D2_QUANT-(cAliasSD2)->D2_QTDEDEV
								nSldQtd2:=ConvUm((cAliasSD2)->D2_COD,nSldQtd,0,2)
								If cPaisLoc == "COL" .OR. (cPaisLoc =="PER" .and. Funname() == "MATA462DN")
									//Se realiza la resta del valor proporcional devuelto (guardado en la bd) al valor total del documento,
                                	//tomando en cuenta el descuento total y proporcial
                                    nDesIt := (cAliasSD2)->D2_DESCON / (cAliasSD2)->D2_QUANT
                                    nDesPro:= nDesIt*(cAliasSD2)->D2_QTDEDEV
                                	nSldBru:= ((cAliasSD2)->D2_TOTAL+(cAliasSD2)->D2_DESCON)-((cAliasSD2)->D2_VALDEV+nDesPro)
								Else
							 	   	nSldBru := (cAliasSD2)->D2_TOTAL+((cAliasSD2)->D2_DESCON+(cAliasSD2)->D2_DESCZFR)-(cAliasSD2)->D2_VALDEV
                                EndIf
								If cPrograma $ "A140I"
									nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="DT_NFORI"})
									nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="DT_SERIORI"})
									nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="DT_ITEMORI"})
									nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="DT_TOTAL"})
									nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="DT_QUANT"})
								ElseIf cPrograma $ "A465D"
									nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_NFORI"})
									nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_SERIORI"})
									nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="D2_ITEMORI"})
									nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_TOTAL"})
									nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_QUANT"})
									nPQuant2UM:= aScan(aHeader,{|x| AllTrim(x[2])=="D2_QTSEGUM"})
								Else
									//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
									//³ Verifica a quantidade ja informada no Documento de Entrada          ³
									//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
									nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_NFORI"})
									nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_SERIORI"})
									nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="D1_ITEMORI"})
									nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_TOTAL"})
									nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_QUANT"})
									nPQuant2UM:= aScan(aHeader,{|x| AllTrim(x[2])=="D1_QTSEGUM"})
									nValDes   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_VALDESC"})
								EndIf
								For nX := 1 To Len(aCols)
									If nX <> N .And.;
											!aCols[nX][Len(aHeader)+1] .And.;
											aCols[nX][nPNfOri] == (cAliasSD2)->D2_DOC .And.;
											aCols[nX][nPSerOri] == (cAliasSD2)->D2_SERIE .And.;
											aCols[nX][nPItemOri] == (cAliasSD2)->D2_ITEM
										nSldQtd -= aCols[nX][nPQuant]
										nSldBru -= (aCols[nX][nPValor] + IIf(nValDes > 0 .And. cPaisLoc == "COL",aCols[nX][nValDes],0) )
									EndIf
								Next nX
								nSldQtd2:=ConvUm((cAliasSD2)->D2_COD,nSldQtd,0,2)
								nSldLiq:=nSldBru-A410Arred(nSldBru*((cAliasSD2)->D2_DESCON+(cAliasSD2)->D2_DESCZFR)/((cAliasSD2)->D2_TOTAL+((cAliasSD2)->D2_DESCON+(cAliasSD2)->D2_DESCZFR)),"C6_VALOR")
								//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
								//³ Atualiza o arquivo de trabalho                                      ³
								//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
								If nSldQtd <> 0 .Or. nSldLiq <> 0
									RecLock(cAliasTRB,.T.)
									For nX := 1 To Len(aStruTRB)
										If !(AllTrim(aStruTRB[nX][1]) $ "D2_ALI_WT|D2_REC_WT|D2_TOTAL2")
											(cAliasTRB)->(FieldPut(nX,(cAliasSD2)->(FieldGet(FieldPos(aStruTRB[nX][1])))))
										EndIf
									Next nX
									(cAliasTRB)->D2_QUANT := a410Arred(nSldQtd,"C6_QTDVEN")
									(cAliasTRB)->D2_QTSEGUM:= a410Arred(nSldQtd2,"C6_UNSVEN")
									(cAliasTRB)->D2_TOTAL := a410Arred(nSldLiq,"C6_VALOR")
									(cAliasTRB)->D2_TOTAL2:= a410Arred(nSldBru,"C6_VALOR")
									(cAliasTRB)->D2_PRCVEN:= a410Arred(nSldLiq/IIf(nSldQtd==0,1,nSldQtd),"C6_PRCVEN")
									If Abs((cAliasTRB)->D2_PRCVEN-(cAliasSD2)->D2_PRCVEN)<= 0.01
										(cAliasTRB)->D2_PRCVEN := (cAliasSD2)->D2_PRCVEN
									EndIf
									If (cAliasTRB)->D2_DESCON+(cAliasTRB)->D2_DESCZFR>0
										(cAliasTRB)->D2_PRUNIT:= a410Arred(nSldBru/IIf(nSldQtd==0,1,nSldQtd),"C6_PRCVEN")
									Else
										(cAliasTRB)->D2_PRUNIT := (cAliasTRB)->D2_PRCVEN
									EndIf
									(cAliasTRB)->D2_REC_WT:= (cAliasSD2)->SD2RECNO
									(cAliasTRB)->D2_ALI_WT := "SD2"
									MsUnLock()
								EndIf
							EndIf
				  		EndIf
					Endif
					dbSelectArea(cAliasSD2)
					dbSkip()
					lSkip := .T.
				EndDo
				If !lSkip
					dbSelectArea(cAliasSF2)
					dbSkip()
				EndIf
			EndDo

		(cAliasSF2)->(dbCloseArea())

	EndCase
	cPreSD := IIf(cTpCliFor == "C", "D2", "D1")
	If (cAliasTRB)->(LastRec())<>0
		PRIVATE aHeader := aHeadTRB
		xPesq := aPesq[(cAliasTRB)->(IndexOrd())][1]
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³ Posiciona registros                                                 ³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		If cTpCliFor == "C"
			dbSelectArea("SA1")
			dbSetOrder(1)
			MsSeek(xFilial("SA1")+cCliFor+cLoja)
		Else
			dbSelectArea("SA2")
			dbSetOrder(1)
			MsSeek(xFilial("SA2")+cCliFor+cLoja)
		EndIf
		dbSelectArea("SB1")
		dbSetOrder(1)
		MsSeek(xFilial("SB1")+cProduto)

		dbSelectArea(cAliasTRB)
		dbGotop()
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³ Calcula as coordenadas da interface                                 ³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		aSize[1] /= 1.5
		aSize[2] /= 1.5
		aSize[3] /= 1.5
		aSize[4] /= 1.3
		aSize[5] /= 1.5
		aSize[6] /= 1.3
		aSize[7] /= 1.5

		AAdd( aObjects, { 100, 020,.T.,.F.,.T.} )
		AAdd( aObjects, { 100, 060,.T.,.T.} )
		AAdd( aObjects, { 100, 020,.T.,.F.} )
		aInfo   := { aSize[ 1 ], aSize[ 2 ], aSize[ 3 ], aSize[ 4 ], 3, 3 }
		aPosObj := MsObjSize( aInfo, aObjects,.T.)

		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³ Interface com o usuario                                             ³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		DEFINE MSDIALOG oDlg TITLE OemToAnsi(STR0007) FROM aSize[7],000 TO aSize[6],aSize[5] OF oMainWnd PIXEL //"Notas Fiscais de Origem"
		@ aPosObj[1,1],aPosObj[1,2] MSPANEL oPanel PROMPT "" SIZE aPosObj[1,3],aPosObj[1,4] OF oDlg CENTERED LOWERED
		If cTpCliFor == "C"
			cTexto1 := AllTrim(RetTitle("F2_CLIENTE"))+"/"+AllTrim(RetTitle("F2_LOJA"))+": "+SA1->A1_COD+"/"+SA1->A1_LOJA+"  -  "+RetTitle("A1_NOME")+": "+SA1->A1_NOME
		Else
			cTexto1 := AllTrim(RetTitle("F1_FORNECE"))+"/"+AllTrim(RetTitle("F1_LOJA"))+": "+SA2->A2_COD+"/"+SA2->A2_LOJA+"  -  "+RetTitle("A2_NOME")+": "+SA2->A2_NOME
		EndIf
		@ 002,005 SAY cTexto1 SIZE aPosObj[1,3],008 OF oPanel PIXEL
		cTexto2 := AllTrim(RetTitle("B1_COD"))+": "+SB1->B1_COD+"/"+SB1->B1_DESC
		@ 012,005 SAY cTexto2 SIZE aPosObj[1,3],008 OF oPanel PIXEL

		@ aPosObj[3,1]+00,aPosObj[3,2]+00 SAY OemToAnsi(STR0027) PIXEL //Pesquisar por:
		@ aPosObj[3,1]+12,aPosObj[3,2]+00 SAY OemToAnsi(STR0026) PIXEL //Localizar
		@ aPosObj[3,1]+00,aPosObj[3,2]+40 MSCOMBOBOX oCombo VAR cCombo ITEMS aOrdem SIZE 100,044 OF oDlg PIXEL ;
		VALID ((cAliasTRB)->(dbSetOrder(oCombo:nAt)),(cAliasTRB)->(dbGotop()),xPesq := aPesq[(cAliasTRB)->(IndexOrd())][1],.T.)
	  	@ aPosObj[3,1]+12,aPosObj[3,2]+40 MSGET oGet VAR xPesq Of oDlg PICTURE aPesq[(cAliasTRB)->(IndexOrd())][2] PIXEL ;
	  	VALID ((cAliasTRB)->(MsSeek(Iif(ValType(xPesq)=="C",AllTrim(xPesq),xPesq),.T.)),.T.).And.IIf(oGetDb:oBrowse:Refresh()==Nil,.T.,.T.)

	  	oGetDb := MsGetDB():New(aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4],1,"Allwaystrue","allwaystrue","",.F., , ,.F., ,cAliasTRB)

		If (cPaisLoc == "COL" .And. !Empty(cProvFE)) .OR. (cPaisLoc == "BOL" .And. lCFDUso)
			If lNtDbCrd .AND. nTpDocS == 22 .AND. FindFunction('lxVlDcTrns')
				bValDcTrns :={||  lxVlDcTrns((cAliasTRB)->&(cPreSD+"_DOC"), (cAliasTRB)->&(cPreSD+"_SERIE"),(cAliasTRB)->&(cPreSD+"_FORNECE"), (cAliasTRB)->&(cPreSD+"_LOJA")) }
			Else
				bValDcTrns := {||  FxMIVldItS((cAliasTRB)->&(cPreSD+"_DOC"), (cAliasTRB)->&(cPreSD+"_SERIE")) }
			EndIf
			DEFINE SBUTTON FROM aPosObj[3,1]+000,aPosObj[3,4]-030 TYPE 1 ACTION (nOpcA := 1, IIf(Eval(bValDcTrns), oDlg:End(), nOpcA := 0)) ENABLE OF oDlg
		Else
			DEFINE SBUTTON FROM aPosObj[3,1]+000,aPosObj[3,4]-030 TYPE 1 ACTION (nOpcA := 1,oDlg:End()) ENABLE OF oDlg
		EndIf
		DEFINE SBUTTON FROM aPosObj[3,1]+012,aPosObj[3,4]-030 TYPE 2 ACTION (nOpcA := 0, oDlg:End()) ENABLE OF oDlg
		ACTIVATE MSDIALOG oDlg CENTERED

		If nOpcA == 1
			lRetorno := .T.
			aHeader   := aClone(aSavHead)
			Do Case
		 		Case cPrograma $ "A440|A466|LOJA920|ARUS466"
					If cPrograma $ "LOJA920|A466"
			 			nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_NFORI"})
			 			nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_SERIORI"})
			 			nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="D2_ITEMORI"})
			 			nPLocal   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_LOCAL"})
			 			nPPrcVen  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_PRCVEN"})
			 			nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_QUANT"})
			 			nPQuant2UM:= aScan(aHeader,{|x| AllTrim(x[2])=="D2_QTSEGUM"})
			 			nPLoteCtl := aScan(aHeader,{|x| AllTrim(x[2])=="D2_LOTECTL"})
			 			nPNumLote := aScan(aHeader,{|x| AllTrim(x[2])=="D2_NUMLOTE"})
			 			nPDtValid := aScan(aHeader,{|x| AllTrim(x[2])=="D2_DTVALID"})
			 			nPPotenc  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_POTENCI"})
			 			nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_TOTAL"})
			 			nPValDesc := aScan(aHeader,{|x| AllTrim(x[2])=="D2_DESCON"})
			 			nPDesc    := aScan(aHeader,{|x| AllTrim(x[2])=="D2_DESC"})
						nPTES     := aScan(aHeader,{|x| AllTrim(x[2])=="D2_TES"})
						nPProvEnt := aScan(aHeader,{|x| AllTrim(x[2])=="D2_PROVENT"})
						nPConcept := aScan(aHeader,{|x| AllTrim(x[2])=="D2_CONCEPT"})
						nPConta	  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_CONTA"})
						nPItemCTa := aScan(aHeader,{|x| AllTrim(x[2])=="D2_ITEMCC"})
						nPCC	  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_CCUSTO"})
						nPOrProd  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_OP"})
						nPCLVL	  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_CLVL"})
						nPNIT	  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_NIT"})
						If cPaisLoc == "RUS"
							nPCf := aScan(aHeader,{|x| AllTrim(x[2])=="D2_CF"})
						EndIf
					ElseIf cPaisLoc == "RUS" .And. cPrograma == "ARUS466"
						nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_NFORI"})
			 			nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_SERIORI"})
			 			nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="D1_ITEMORI"})
			 			nPLocal   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_LOCAL"})
			 			nPPrcVen  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_VUNIT"})
			 			nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_QUANT"})
			 			nPQuant2UM:= aScan(aHeader,{|x| AllTrim(x[2])=="D1_QTSEGUM"})
			 			nPLoteCtl := aScan(aHeader,{|x| AllTrim(x[2])=="D1_LOTECTL"})
			 			nPNumLote := aScan(aHeader,{|x| AllTrim(x[2])=="D1_NUMLOTE"})
			 			nPDtValid := aScan(aHeader,{|x| AllTrim(x[2])=="D1_DTVALID"})
			 			nPPotenc  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_POTENCI"})
			 			nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_TOTAL"})
			 			nPValDesc := aScan(aHeader,{|x| AllTrim(x[2])=="D1_VALDESC"})
			 			nPDesc    := aScan(aHeader,{|x| AllTrim(x[2])=="D1_DESC"})
						nPTES     := aScan(aHeader,{|x| AllTrim(x[2])=="D1_TES"})
						nPProvEnt := aScan(aHeader,{|x| AllTrim(x[2])=="D1_PROVENT"})
						nPConcept := aScan(aHeader,{|x| AllTrim(x[2])=="D1_CONCEPT"})
						nPCf := aScan(aHeader,{|x| AllTrim(x[2])=="D1_CF"})
		 		    Else
			 			nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="C6_NFORI"})
			 			nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="C6_SERIORI"})
			 			nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="C6_ITEMORI"})
			 			nPLocal   := aScan(aHeader,{|x| AllTrim(x[2])=="C6_LOCAL"})
			 			nPPrUnit  := aScan(aHeader,{|x| AllTrim(x[2])=="C6_PRUNIT"})
			 			nPPrcVen  := aScan(aHeader,{|x| AllTrim(x[2])=="C6_PRCVEN"})
			 			nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="C6_QTDVEN"})
			 			nPQuant2UM:= aScan(aHeader,{|x| AllTrim(x[2])=="C6_UNSVEN"})
			 			nPLoteCtl := aScan(aHeader,{|x| AllTrim(x[2])=="C6_LOTECTL"})
			 			nPNumLote := aScan(aHeader,{|x| AllTrim(x[2])=="C6_NUMLOTE"})
			 			nPDtValid := aScan(aHeader,{|x| AllTrim(x[2])=="C6_DTVALID"})
			 			nPPotenc  := aScan(aHeader,{|x| AllTrim(x[2])=="C6_POTENCI"})
			 			nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="C6_VALOR"})
			 			nPValDesc := aScan(aHeader,{|x| AllTrim(x[2])=="C6_VALDESC"})
						nPTES     := aScan(aHeader,{|x| AllTrim(x[2])=="C6_TES"})
						nPProvEnt := aScan(aHeader,{|x| AllTrim(x[2])=="C6_PROVENT"})
						nPClasFis := aScan(aHeader,{|x| AllTrim(x[2])=="C6_CLASFIS"})
					Endif
					If cPaisLoc <> "BRA"
						If nPTES <> 0
							If cPaisLoc == "RUS" .And. cPrograma == "ARUS466"
								aCols[N][nPTES] := (cAliasTRB)->D1_TES
							Else
								SF4->(DbSetOrder(1))
								If SF4->(MsSeek(xFilial("SF4")+(cAliasTRB)->D1_TES)) .And. !Empty(SF4->F4_TESDV)
									aCols[N][nPTES] := SF4->F4_TESDV
								Endif
							EndIf
			 			Endif
			 			If cPaisLoc == "RUS" .And. nPCf <> 0
							aCols[N][nPCf] := (cAliasTRB)->D1_CF
						EndIf
			 			If nPProvEnt <> 0
			 				aCols[N][nPProvEnt] := (cAliasTRB)->D1_PROVENT
			 			Endif
			 		Endif
		 			If nPNfOri <> 0
		 				aCols[N][nPNfOri] := (cAliasTRB)->D1_DOC
		 			EndIf
		 			If nPSerOri <> 0
		 				aCols[N][nPSerOri] := (cAliasTRB)->D1_SERIE
		 			EndIf
	 				If nPItemOri <> 0
		 				aCols[N][nPItemOri] := (cAliasTRB)->D1_ITEM
	 				EndIf
	 				If nPPrUnit <> 0
		 				aCols[N][nPPrUnit] := (cAliasTRB)->D1_VUNIT
		 			EndIf
					If lCSTOri .And. nPClasFis <> 0
						aCols[N][nPClasFis] := (cAliasTRB)->D1_CLASFIS
					EndIf
					
					If cPaisLoc <> "BRA"
						If nPConta <> 0
							aCols[n][nPConta] := (cAliasTRB)->D1_CONTA
						EndIf
						If nPItemCTA <> 0
							aCols[n][nPItemCTA] := (cAliasTRB)->D1_ITEMCTA
						EndIf
						If nPCC <> 0 
							aCols[n][nPCC] := (cAliasTRB)->D1_CC
						EndIf
						If nPOrProd <> 0 
							aCols[n][nPOrProd] := (cAliasTRB)->D1_OP
						EndIf
						If nPCLVL <> 0
							aCols[n][nPCLVL] := (cAliasTRB)->D1_CLVL
						EndIf
						If nPNIT <> 0
							aCols[n][nPNIT] := (cAliasTRB)->D1_NIT
						EndIf
					Endif

		 			If nPPrcVen <> 0
		 				If AllTrim( Upper( cPrograma ) ) == "LOJA920"
			 				aCols[N][nPPrcVen] := (cAliasTRB)->D1_VUNIT
		 				Else
			 				If cPaisLoc == "ARG"
  			 					dbSelectArea("SF1")
			 					aAreaSF1 := SF1->(GetArea())
			 					SF1->(dbSetOrder(1))
			 					MsSeek(xFilial("SF1") + (cAliasTRB)->D1_DOC + (cAliasTRB)->D1_SERIE)

			 					If M->F2_MOEDA <> SF1->F1_MOEDA .AND. FunName() == "MATA102DN"
			 						nMoedaOri := (cAliasTRB)->D1_V_UNIT2
			 						nMdaConv := xMoeda(nMoedaOri, SF1->F1_MOEDA, M->F2_MOEDA, , MsDecimais(M->F2_MOEDA), , M->F2_TXMOEDA)
			 						aCols[N][nPPrcVen] := nMdaConv
			 					Else
			 						aCols[N][nPPrcVen] := (cAliasTRB)->D1_V_UNIT2
			 					Endif
				 				RestArea(aAreaSF1)
			 				Else
			 					aCols[N][nPPrcVen] := (cAliasTRB)->D1_V_UNIT2
			 				Endif
			 			EndIf
		 			EndIf
		 			If nPLocal <> 0
		 				aCols[N][nPLocal] := (cAliasTRB)->D1_LOCAL
		 			EndIf
		 			If nPQuant <> 0 .And. (aCols[N][nPQuant] > (cAliasTRB)->D1_QUANT .Or. aCols[N][nPQuant] == 0 )
						aCols[N][nPQuant] := (cAliasTRB)->D1_QUANT
						If nPQuant2UM <> 0
							aCols[N][nPQuant2UM] := (cAliasTRB)->D1_QTSEGUM
						EndIf
					EndIf
					If nPConcept <> 0
			 				aCols[N][nPConcept] := (cAliasTRB)->D1_CONCEPT
			 		Endif
					If Rastro(cProduto) .And. ( SF4->(dbSeek(xFilial("SF4")+aCols[N][nPTES])) .And. SF4->F4_ESTOQUE == 'S' )
						If nPLoteCtl <> 0
							aCols[N][nPLoteCtl] := (cAliasTRB)->D1_LOTECTL
						EndIf
						If nPNumLote <> 0
							aCols[N][nPNumLote] := (cAliasTRB)->D1_NUMLOTE
						EndIf
						If nPDtValid <> 0 .Or. npPotenc <> 0
							dbSelectArea("SB8")
							dbSetOrder(3)
							If MsSeek(xFilial("SB8")+cProduto+aCols[N][nPLocal]+aCols[n][nPLoteCtl]+IIf(Rastro(cProduto,"S"),aCols[N][nPNumLote],""))
								If nPDtValid <> 0
									aCols[n][nPDtValid] := SB8->B8_DTVALID
								EndIf
								If npPotenc <> 0
									aCols[n][nPPotenc] := SB8->B8_POTENCI
								EndIf
							EndIf
						EndIf
					EndIf
					If cPrograma $ "A440"
						aCols[N,nPValor] := ((((cAliasTRB)->D1_TOTAL - (cAliasTRB)->D1_VALDESC) / (cAliasTRB)->D1_QUANT) * aCols[N,nPQuant])
						A410MultT("C6_QTDVEN",aCols[N,nPQuant])
						A410MultT("C6_PRCVEN",aCols[N,nPPrcVen])
					Endif
					//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
					//³ Ajusta o valor total do documento de saida qdo houver dev. total    ³
					//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
					If nPValDesc <> 0
						aCols[n][nPValDesc] := a410Arred((cAliasTRB)->D1_VALDESC,"C6_VALDESC")
						If cPrograma $ "A440" .And. aCols[n][nPValDesc]<>0
							A410MultT("C6_VALDESC",aCols[n][nPValDesc])
						EndIf
						If AllTrim( Upper( cPrograma ) ) == "LOJA920"
							aCols[n][nPValDesc] := (cAliasTRB)->D1_VALDESC
						EndIf
					EndIf

					If nPValDesc <> 0 .And. nPQuant <> 0 .And. nPDesc <> 0 .And. AllTrim( Upper( cPrograma ) ) == "LOJA920"
						aCols[n][nPDesc]    :=  (cAliasTRB)->D1_DESC
					EndIf

					//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
					//³ Ajusta o valor total do documento de saida qdo nao for Brasil       ³
					//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
					If cPrograma $ "A466" .and. cPaisLoc <>"BRA"
						dbSelectArea("SF1")
                        aAreaSF1 := SF1->(GetArea())
                        SF1->(dbSetOrder(1))
                        MsSeek(xFilial("SF1") + (cAliasTRB)->D1_DOC + (cAliasTRB)->D1_SERIE)
                        If M->F2_MOEDA <> SF1->F1_MOEDA .AND. FunName() == "MATA102DN"
                            nTotalOri := (cAliasTRB)->D1_TOTAL
                            nTtalConv := xMoeda(nTotalOri, SF1->F1_MOEDA, M->F2_MOEDA, , MsDecimais(M->F2_MOEDA), , M->F2_TXMOEDA)
                            aCols[N,nPValor] := nTtalConv-(cAliasTRB)->D1_VALDESC
                        Else
                            aCols[N,nPValor] := (cAliasTRB)->D1_TOTAL-(cAliasTRB)->D1_VALDESC
                        Endif
                        RestArea(aAreaSF1)
					EndIf
					If cPaisLoc == "RUS" .And. cPrograma == "ARUS466"
						aCols[N,nPValor] := (cAliasTRB)->D1_TOTAL
					EndIf
					If ("_NFORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D1_DOC
					EndIf

					If ("_SERIORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D1_SERIE
					EndIf

					If ("_ITEMORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D1_ITEM
					EndIf

					nRecSD1	:= (cAliasTRB)->D1_REC_WT
				Case cPaisLoc == "RUS" .And. cPrograma $ "A465D"
					nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_NFORI"})
					nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_SERIORI"})
					nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="D2_ITEMORI"})
					nPLocal   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_LOCAL"})
					nPPrcVen  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_PRCVEN"})
					nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_QUANT"})
					nPQuant2UM:= aScan(aHeader,{|x| AllTrim(x[2])=="D2_QTSEGUM"})
					nPLoteCtl := aScan(aHeader,{|x| AllTrim(x[2])=="D2_LOTECTL"})
					nPNumLote := aScan(aHeader,{|x| AllTrim(x[2])=="D2_NUMLOTE"})
					nPDtValid := aScan(aHeader,{|x| AllTrim(x[2])=="D2_DTVALID"})
					nPPotenc  := aScan(aHeader,{|x| AllTrim(x[2])=="D2_POTENCI"})
					nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="D2_TOTAL"})
					nPTES     := aScan(aHeader,{|x| AllTrim(x[2])=="D2_TES"})
					nPCf     := aScan(aHeader,{|x| AllTrim(x[2])=="D2_CF"})
					nPProvEnt := aScan(aHeader,{|x| AllTrim(x[2])=="D2_PROVENT"})
					nPConcept := aScan(aHeader,{|x| AllTrim(x[2])=="D2_CONCEPT"})

					If nPNfOri <> 0
		 				aCols[N][nPNfOri] := (cAliasTRB)->D2_DOC
		 			EndIf
		 			If nPSerOri <> 0
		 				aCols[N][nPSerOri] := (cAliasTRB)->D2_SERIE
		 			EndIf
	 				If nPItemOri <> 0
		 				aCols[N][nPItemOri] := (cAliasTRB)->D2_ITEM
	 				EndIf
	 				If nPPrcVen <> 0
		 				aCols[N][nPPrcVen] := (cAliasTRB)->D2_PRCVEN
		 			EndIf
					If nPLocal <> 0
		 				aCols[N][nPLocal] := (cAliasTRB)->D2_LOCAL
		 			EndIf
		 			If nPQuant <> 0
						aCols[N][nPQuant] := (cAliasTRB)->D2_QUANT
					EndIf
					If nPQuant2UM <> 0
						aCols[N][nPQuant2UM] := (cAliasTRB)->D2_QTSEGUM
					EndIf
					If nPConcept <> 0
			 			aCols[N][nPConcept] := (cAliasTRB)->D2_CONCEPT
			 		Endif
					If nPLoteCtl <> 0
						aCols[N][nPLoteCtl] := (cAliasTRB)->D2_LOTECTL
					EndIf
					If nPNumLote <> 0
						aCols[N][nPNumLote] := (cAliasTRB)->D2_NUMLOTE
					EndIf
					If nPTES <> 0
						aCols[N][nPTES] := (cAliasTRB)->D2_TES
					Endif
					If nPCf <> 0
						aCols[N][nPCf] := (cAliasTRB)->D2_CF
					Endif
					If nPDtValid <> 0 .Or. npPotenc <> 0
						dbSelectArea("SB8")
						dbSetOrder(3)
						If MsSeek(xFilial("SB8")+cProduto+aCols[N][nPLocal]+aCols[n][nPLoteCtl]+IIf(Rastro(cProduto,"S"),aCols[N][nPNumLote],""))
							If nPDtValid <> 0
								aCols[n][nPDtValid] := SB8->B8_DTVALID
							EndIf
							If npPotenc <> 0
								aCols[n][nPPotenc] := SB8->B8_POTENCI
							EndIf
						EndIf
					EndIf
					If nPValor <> 0
						aCols[N][nPValor] := (cAliasTRB)->D2_TOTAL
					EndIf
					If nPProvEnt <> 0
				 		aCols[N][nPProvEnt] := (cAliasTRB)->D2_PROVENT
				 	Endif

					If ("_NFORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D2_DOC
					EndIf

					If ("_SERIORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D2_SERIE
					EndIf

					If ("_ITEMORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D2_ITEM
					EndIf

					nRecSD2	:= (cAliasTRB)->D2_REC_WT
				OtherWise
					nRecSD2	:= (cAliasTRB)->D2_REC_WT
					SD2->(MsGoto(nRecSD2))
					If cPrograma $ "A140I"
						nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="DT_NFORI"})
			 			nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="DT_SERIORI"})
			 			nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="DT_ITEMORI"})
			 			nPPrcVen  := aScan(aHeader,{|x| AllTrim(x[2])=="DT_VUNIT"})
			 			nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="DT_QUANT"})
			 			nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="DT_TOTAL"})
			 			nPValDesc := aScan(aHeader,{|x| AllTrim(x[2])=="DT_VALDESC"})
						If lDtCpoLote
							nPLoteCtl   := aScan(aHeader,{|x| AllTrim(x[2])=="DT_LOTE"})
							nPDtValid   := aScan(aHeader,{|x| AllTrim(x[2])=="DT_DTVALID"})
							nD1Fabric 	:= aScan(aHeader,{|x| AllTrim(x[2])=="DT_DFABRIC"})
						Endif
			 			If aCols[N][nPQuant] > (cAliasTRB)->D2_QUANT
							Aviso(STR0030,STR0084,{"Ok"})	// "Quantidade a devolver superior ao item vendido. Efetue a geração do documento e realize o ajuste na pré-nota de entrada."
							lRetorno := .F.
			 			ElseIf aCols[N][nPQuant] <= (cAliasTRB)->D2_QUANT
							If nPNfOri <> 0
				 				aCols[N][nPNfOri] := (cAliasTRB)->D2_DOC
				 			EndIf
				 			If nPSerOri <> 0
				 				aCols[N][nPSerOri] := (cAliasTRB)->D2_SERIE
				 			EndIf
			 				If nPItemOri <> 0
				 				aCols[N][nPItemOri] := (cAliasTRB)->D2_ITEM
			 				EndIf

							If lDtCpoLote .And. Rastro(cProduto)
								If nPLoteCtl <> 0
									aCols[N][nPLoteCtl] := (cAliasTRB)->D2_LOTECTL
								EndIf 
								If nPDtValid <> 0 .or. nD1Fabric <> 0
									dDTB8DTVALID := GetAdvFVal("SB8","B8_DTVALID",xFilial("SB8")+cProduto+(cAliasTRB)->D2_LOCAL+aCols[n][nPLoteCtl]+IIf(Rastro(cProduto,"S"),(cAliasTRB)->D2_NUMLOTE,""),3)
									dDTB8DFABRIC := GetAdvFVal("SB8","B8_DFABRIC",xFilial("SB8")+cProduto+(cAliasTRB)->D2_LOCAL+aCols[n][nPLoteCtl]+IIf(Rastro(cProduto,"S"),(cAliasTRB)->D2_NUMLOTE,""),3)
									If nPDtValid <> 0
										aCols[n][nPDtValid] := Iif(!Empty(dDTB8DTVALID),dDTB8DTVALID,(cAliasTRB)->D2_DTVALID)
									EndIf
									If nD1Fabric <> 0
										aCols[n][nD1Fabric] := Iif(!Empty(dDTB8DFABRIC),dDTB8DFABRIC,(cAliasTRB)->D2_DFABRIC)
									EndIf
								EndIf
							EndIf
			 			Else
			 				Help(" ",1,"A410UNIDIF")
							lRetorno := .F.
						EndIf
					Else
			 			nPNfOri   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_NFORI"})
			 			nPSerOri  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_SERIORI"})
			 			nPItemOri := aScan(aHeader,{|x| AllTrim(x[2])=="D1_ITEMORI"})
			 			nPLocal   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_LOCAL"})
			 			nPPrcVen  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_VUNIT"})
			 			nPQuant   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_QUANT"})
			 			nPQuant2UM:= aScan(aHeader,{|x| AllTrim(x[2])=="D1_QTSEGUM"})
			 			nPLoteCtl := aScan(aHeader,{|x| AllTrim(x[2])=="D1_LOTECTL"})
			 			nPNumLote := aScan(aHeader,{|x| AllTrim(x[2])=="D1_NUMLOTE"})
			 			nPDtValid := aScan(aHeader,{|x| AllTrim(x[2])=="D1_DTVALID"})
			 			nPPotenc  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_POTENCI"})
			 			nPValor   := aScan(aHeader,{|x| AllTrim(x[2])=="D1_TOTAL"})
			 			nPValDesc := aScan(aHeader,{|x| AllTrim(x[2])=="D1_VALDESC"})
			 			nPDesc    := aScan(aHeader,{|x| AllTrim(x[2])=="D1_DESC"})
						nPOrigem	 := aScan(aHeader,{|x| AllTrim(x[2])=="D1_ORIGEM"})
						nPDespacho:= aScan(aHeader,{|x| AllTrim(x[2])=="D1_NUMDESP"})
						nPTES     := aScan(aHeader,{|x| AllTrim(x[2])=="D1_TES"})
						If cPaisLoc =="RUS"
							nPCf      := aScan(aHeader,{|x| AllTrim(x[2])=="D1_CF"})
						Endif
						nPProvEnt := aScan(aHeader,{|x| AllTrim(x[2])=="D1_PROVENT"})
						nD1Fabric := aScan(aHeader,{|x| AllTrim(x[2])=="D1_DFABRIC"})
						nPPeso	  := aScan(aHeader,{|x| Alltrim(x[2])=="D1_PESO"})
						nFciCod   := aScan(aHeader,{|x| Alltrim(x[2])=="D1_FCICOD"})
						nPCC	  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_CC"})
						nPConta	  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_CONTA"})
						nPItemCTa := aScan(aHeader,{|x| AllTrim(x[2])=="D1_ITEMCTA"})
						nPCLVL	  := aScan(aHeader,{|x| AllTrim(x[2])=="D1_CLVL"})
						nPosEC05DB:= If(nPosEC05DB>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC05DB"}),0)
						nPosEC05CR:= If(nPosEC05CR>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC05CR"}),0)
						nPosEC06DB:= If(nPosEC06DB>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC06DB"}),0)
						nPosEC06CR:= If(nPosEC06CR>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC06CR"}),0)
						nPosEC07DB:= If(nPosEC07DB>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC07DB"}),0)
						nPosEC07CR:= If(nPosEC07CR>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC07CR"}),0)
						nPosEC08DB:= If(nPosEC08DB>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC08DB"}),0)
						nPosEC08CR:= If(nPosEC08CR>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC08CR"}),0)
						nPosEC09DB:= If(nPosEC09DB>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC09DB"}),0)
						nPosEC09CR:= If(nPosEC09CR>0,aScan(aHeader,{|x| AllTrim(x[2])=="D1_EC09CR"}),0)

						If cPaisLoc <> "BRA"
							nUniaduD1 := aScan(aHeader,{|x| AllTrim(x[2]) == "D1_UNIADU"})
							nUsdaduD1 := aScan(aHeader,{|x| AllTrim(x[2]) == "D1_USDADU"})
							nValaduD1 := aScan(aHeader,{|x| AllTrim(x[2]) == "D1_VALADU"})
							nCanaduD1 := aScan(aHeader,{|x| AllTrim(x[2]) == "D1_CANADU"})
							nFraccaD1 := aScan(aHeader,{|x| AllTrim(x[2]) == "D1_FRACCA"})

							If (nUniaduD1 > 0  ,  aCOLS[N][nUniaduD1]:=(cAliasTRB)->D2_UNIADU,)
							If (nUsdaduD1 > 0  ,  aCOLS[N][nUsdaduD1]:=(cAliasTRB)->D2_USDADU,)
							If (nValaduD1 > 0  ,  aCOLS[N][nValaduD1]:=(cAliasTRB)->D2_VALADU,)
							If (nCanaduD1 > 0  ,  aCOLS[N][nCanaduD1]:=(cAliasTRB)->D2_CANADU,)
							If (nFraccaD1 > 0  ,  aCOLS[N][nFraccaD1]:=(cAliasTRB)->D2_FRACCA,)

							If nPTES <> 0
								SF4->(DbSetOrder(1))
								If SF4->(MsSeek(xFilial("SF4")+(cAliasTRB)->D2_TES)) .And. !Empty(SF4->F4_TESDV)
									aCols[N][nPTES] := SF4->F4_TESDV
								Endif
							Endif
				 			If nPProvEnt <> 0
				 				aCols[N][nPProvEnt] := (cAliasTRB)->D2_PROVENT
				 			Endif
						EndIf
						If cPaisLoc == "RUS" .And. nPCf <> 0
							aCols[N][nPCf] := (cAliasTRB)->D2_CF
						EndIf
			 			If nPNfOri <> 0
			 				aCols[N][nPNfOri] := (cAliasTRB)->D2_DOC
			 			EndIf
			 			If nPSerOri <> 0
			 				aCols[N][nPSerOri] := (cAliasTRB)->D2_SERIE
			 			EndIf
		 				If nPItemOri <> 0
			 				aCols[N][nPItemOri] := (cAliasTRB)->D2_ITEM
		 				EndIf
		 				If nPLocal <> 0
			 				aCols[N][nPLocal] := (cAliasTRB)->D2_LOCAL
						EndIf
			 			If nPPrcVen <> 0
							If cPaisLoc $ "PER|MEX|COL" .And. lDescSai
			 					aCols[N][nPPrcVen] := (cAliasTRB)->D2_PRCVEN
			 				Else
			 					aCols[N][nPPrcVen] := (cAliasTRB)->D2_PRUNIT
							EndIf
			 			EndIf
			 			If nPQuant <> 0 .And. ( aCols[N][nPQuant] > (cAliasTRB)->D2_QUANT .Or. aCols[N][nPQuant]==0 )
							aCols[N][nPQuant] := (cAliasTRB)->D2_QUANT
							If nPQuant2UM <> 0
								aCols[N][nPQuant2UM] := (cAliasTRB)->D2_QTSEGUM
							EndIf
						EndIf
						If Rastro(cProduto) .And. nPTES > 0
							If  ( SF4->(dbSeek(xFilial("SF4")+aCols[N][nPTES])) .And. SF4->F4_ESTOQUE == 'S' )
							If nPLoteCtl <> 0
								aCols[N][nPLoteCtl] := (cAliasTRB)->D2_LOTECTL
							EndIf
							If nPNumLote <> 0
								aCols[N][nPNumLote] := (cAliasTRB)->D2_NUMLOTE
							EndIf
							If nPDtValid <> 0 .or. nPPotenc <> 0 .Or. nPDespacho <> 0 .Or. nPOrigem <> 0
								dbSelectArea("SB8")
								dbSetOrder(3)
								If MsSeek(xFilial("SB8")+cProduto+aCols[N][nPLocal]+aCols[n][nPLoteCtl]+IIf(Rastro(cProduto,"S"),aCols[N][nPNumLote],""))
									If nPDtValid <> 0
										aCols[n][nPDtValid] := SB8->B8_DTVALID
									EndIf
									If nPPotenc <> 0
										aCols[n][nPPotenc] := SB8->B8_POTENCI
									EndIf
									If nPDespacho <> 0
										aCols[n][nPDespacho] := SB8->B8_NUMDESP
									EndIf
									If nPOrigem <> 0
										aCols[n][nPOrigem] := SB8->B8_ORIGEM
									EndIf
									If nD1Fabric <> 0
										aCols[n][nD1Fabric] := SB8->B8_DFABRIC
									EndIf
								EndIf
							EndIf
							EndIf
						EndIf
						If nPValDesc <> 0 .And. nPQuant <> 0 .And. nPDesc <> 0
							aCols[n][nPValDesc] := a410Arred(((cAliasTRB)->D2_PRUNIT-(cAliasTRB)->D2_PRCVEN)*IIf(aCols[n][nPQuant]==0,1,aCols[n][nPQuant]),"D1_VALDESC")
							aCols[n][nPDesc]    :=  (cAliasTRB)->D2_DESC
						EndIf
						If SD2->D2_QUANT+SD2->D2_QTDEDEV == aCols[n][nPQuant] // devolucao total
							aCols[n][nPValDesc] := (cAliasTRB)->D2_DESCON + (cAliasTRB)->D2_DESCZFR
							aCols[n][nPValor]	:= (cAliasTRB)->D2_TOTAL + IIF(lDescSai .AND. cPaisLoc $ "ARG|PER|MEX|COL",0,(cAliasTRB)->D2_DESCON) + (cAliasTRB)->D2_DESCZFR
						Else
							aCols[n][nPValor] := a410Arred(IIf(aCols[n][nPQuant]==0,1,aCols[n][nPQuant])*aCols[n][nPPrcVen],"D1_TOTAL")
							If aCols[n][nPValor] > (cAliasTRB)->D2_TOTAL2
								aCols[n][nPValor]   := (cAliasTRB)->D2_TOTAL2
								aCols[n][nPValDesc] := (cAliasTRB)->D2_TOTAL2-(cAliasTRB)->D2_TOTAL
							EndIf
						EndIf
						If nPPeso <> 0
							aCols[n][nPPeso] := (cAliasTRB)->D2_PESO
						EndIf
						If nFciCod <> 0
						   aCols[n][nFciCod] := (cAliasTRB)->D2_FCICOD
						EndIf
						If nPosEC05DB > 0 .And. !(Empty((cAliasTRB)->D2_EC05DB))
							aCols[n][nPosEC05DB]   :=  (cAliasTRB)->D2_EC05DB
						EndIf
						If nPosEC05CR > 0 .And. !(Empty((cAliasTRB)->D2_EC05CR))
							aCols[n][nPosEC05CR]   :=  (cAliasTRB)->D2_EC05CR
						EndIf
						If nPosEC06DB > 0 .And. !(Empty((cAliasTRB)->D2_EC06DB))
							aCols[n][nPosEC06DB]   :=  (cAliasTRB)->D2_EC06DB
						EndIf
						If nPosEC06CR > 0 .And. !(Empty((cAliasTRB)->D2_EC06CR))
							aCols[n][nPosEC06CR]   :=  (cAliasTRB)->D2_EC06CR
						EndIf
						If nPosEC07DB > 0 .And. !(Empty((cAliasTRB)->D2_EC07DB))
							aCols[n][nPosEC07DB]   :=  (cAliasTRB)->D2_EC07DB
						EndIf
						If nPosEC07CR > 0 .And. !(Empty((cAliasTRB)->D2_EC07CR))
							aCols[n][nPosEC07CR]   :=  (cAliasTRB)->D2_EC07CR
						EndIf
						If nPosEC08DB > 0 .And. !(Empty((cAliasTRB)->D2_EC08DB)) 
							aCols[n][nPosEC08DB]   :=  (cAliasTRB)->D2_EC08DB
						EndIf
						If nPosEC08CR > 0 .And. !(Empty((cAliasTRB)->D2_EC08CR))
							aCols[n][nPosEC08CR]   :=  (cAliasTRB)->D2_EC08CR
						EndIf
						If nPosEC09DB > 0 .And. !(Empty((cAliasTRB)->D2_EC09DB)) 
							aCols[n][nPosEC09DB]   :=  (cAliasTRB)->D2_EC09DB
						EndIf
						If nPosEC09CR > 0 .And. !(Empty((cAliasTRB)->D2_EC09CR))
							aCols[n][nPosEC09CR]   :=  (cAliasTRB)->D2_EC09CR
						EndIf

					EndIf

					If lUUID
						If !((cAliasTRB)->F2_UUID $ M->F1_UUIDREL)
							M->F1_UUIDREL +=  (cAliasTRB)->F2_UUID + cCtrl
						EndIf
					EndIF

					If ("_NFORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D2_DOC
					EndIf

					If ("_SERIORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D2_SERIE
					EndIf

					If ("_ITEMORI"$cReadVar)
						&(cReadVar) := (cAliasTRB)->D2_ITEM
					EndIf
					If nPCC <> 0 
						aCols[n][nPCC] := (cAliasTRB)->D2_CCUSTO
					EndIf
					If nPConta <> 0
						aCols[n][nPConta] := (cAliasTRB)->D2_CONTA
					EndIf
					If nPItemCTA <> 0
						aCols[n][nPItemCTA] := (cAliasTRB)->D2_ITEMCC
					EndIf
					If nPCLVL <> 0
						aCols[n][nPCLVL] := (cAliasTRB)->D2_CLVL
					EndIf
			EndCase
		EndIf
	Else
		HELP(" ",1,"F4NAONOTA")
	EndIf
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Restaura a integridade da rotina                                    ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	dbSelectArea(cAliasTRB)
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ARQUIVO TEMPORARIO DE MEMORIA (CTREETMP)                            ³
	//³A funcao MSCloseTemp ira substituir a linha de codigo abaixo:       ³
	//|--> dbCloseArea()                                                   |
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	FWCloseTemp(cAliasTRB,cNomeTrb)
EndIf
dbSelectArea("SA1")
RestArea(aArea)
SetFocus(nHdl)
Return(lRetorno)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³PotencLote³ Autor ³ Rodrigo de A. Sartorio³ Data ³ 17/06/02 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Pesquisa se o produto controla potencia de lote             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ PotencLote(cProd)    	                                  ³±±
±±³          ³ cProd := C¢digo do produto a ser pesquisado no SB1.        ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ EST/PCP/FAT/COM	                                          ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function PotencLote(cCod)
Local aArea:=GetArea()
Local aAreaSB1:=SB1->(GetArea())
Local lRet:= .F.
SB1->(dbSetOrder(1))
If SB1->B1_FILIAL+SB1->B1_COD # xFilial("SB1")+cCod
	SB1->(dbSeek(xFilial("SB1")+cCod))
EndIf
If !SB1->(Eof()) .And. Rastro(cCod)
	If SB1->B1_CPOTENC $ " 2"
		lRet:=.F.
	Else
		lRet:=.T.
	EndIf
EndIf
RestArea(aAreaSB1)
RestArea(aArea)
Return lRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³F4LoteArray³ Autor ³ Marcelo Iuspa        ³ Data ³ 20/10/04 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Pesquisa se o produto controla potencia de lote             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ PotencLote(cProd)    	                                  ³±±
±±³          ³ cProd := C¢digo do produto a ser pesquisado no SB1.        ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ EST/PCP/FAT/COM	                                          ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function F4LoteArray(cProg, lSLote, cAlias, cAliasTop, aArray)
Static lF4LoteArray
Local aRet := {}

If lF4LoteArray == Nil
	lF4LoteArray := ExistBlock("F4LoteArray")
Endif

If lF4LoteArray .AND. ((ValType(aRet := ExecBlock("F4LoteArray",.F.,.F.,{cProg, lSLote, cAlias, cAliasTop, aArray}))) == "A")
	aArray := Aclone(aRet)
Endif

Return(aArray)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³MAvalCusOp ³ Autor ³Rodrigo de A Sartorio ³ Data ³ 08/12/05 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Funcao utilizada para avaliar se custo do movimento interno³±±
±±³			 ³ deve ser agregado a ordem de producao informada.           ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ MAvalCusOP(cProduto,cTm)                                   ³±±
±±³          ³ cProduto := C¢digo do produto a ser pesquisado no SB1 e    ³±±
±±³          ³ avaliado para custeio.                                     ³±±
±±³          ³ cTm      := C¢digo do tipo de movimentacao interna a ser   ³±±
±±³          ³ pesquisada no SF5 e avaliada para custeio.                 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ EST/PCP/FAT/COM	                                          ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function MAvalCusOP(cProduto,cTm)
Local lRet := .T.
Local aArea:=GetArea()
Local aAreaSB1:=SB1->(GetArea())
Local aAreaSF5:=SF5->(GetArea())
// Checa se produto permite nao agregar custo
dbSelectArea("SB1")
dbSetOrder(1)
If MsSeek(xFilial("SB1")+cProduto) .And. SB1->B1_AGREGCU == "1"
	// Checa se tipo de movimentacao interna esta configurada para nao agregar custo
	dbSelectArea("SF5")
	dbSetOrder(1)
	If MsSeek(xFilial("SF5")+cTm) .And. SF5->F5_AGREGCU == "2"
		lRet:=.F.
    EndIf
EndIf
// Restaura area original
SB1->(RestArea(aAreaSB1))
SF5->(RestArea(aAreaSF5))

RestArea(aArea)
Return lRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ MVULMES   ³ Autor ³Rodrigo de A Sartorio ³ Data ³ 02/08/06 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Funcao utilizada para retornar o conteudo do parametro     ³±±
±±³			 ³ MVULMES ou a data do parametro MV_DBLQMOV.                 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ MVULMES()                                                  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function MVULMES()
Local dRet     := GetMV("MV_ULMES")
Local dDataBloq:= GetMV("MV_DBLQMOV",,dRet)

Return Max(dRet,dDataBloq)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿±±
±±³ Fun‡…o    ³ MatFilCalc (Original MA330FCalc)                           ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Autor     ³ Microsiga Software S/A                   ³ Data ³ 22/01/06 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Descri‡…o ³ Funcao para selecao das filiais para calculo por empresa   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros ³ ExpL1 = Indica se apresenta tela para selecao              ³±±
±±³           ³ ExpA2 = Lista com as filiais a serem consideradas (Batch)  ³±±
±±³           ³ ExpN6 = 0=Legado/1=CNPJ iguais/2=CNPJ+IE iguais/3=CNPJ Pai ³±±
±±³ÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³±±
±±³Retorno    ³ Array: aFilsCalc[3][n]                                     ³±±
±±³           ³ aFilsCalc[1][n]:           - Logico                        ³±±
±±³           ³ aFilsCalc[2][n]: Filial    - Caracter                      ³±±
±±³           ³ aFilsCalc[3][n]: Descricao - Caracter                      ³±±
±±³           ³ aFilsCalc[4][n]: CNPJ      - Caracter                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³  Uso      ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function MatFilCalc(lMostratela,aListaFil,lChkUser,lConsolida,nOpca,nValida,lContEmp, lJourney)
Local aFilsCalc	:= {}
// Variaveis utilizadas na selecao de categorias
Local oChkQual,lQual,oQual,cVarQ
// Carrega bitmaps
Local oOk       := LoadBitmap(GetResources(),"LBOK")
Local oNo       := LoadBitmap(GetResources(),"LBNO")
// Variaveis utilizadas para lista de filiais
Local nx        := 0
Local nAchou    := 0
Local lMtFilcal	:= ExistBlock('MTFILCAL')
Local aRetPE	:= {}
Local lIsBlind  := IsBlind()

Local aAreaSM0	:= SM0->(GetArea())
Local aSM0      := FWLoadSM0(.T.,,.T.)

Local cSelCNPJIE:=""
local nSelCNPJIE:=0

Local cEmpresa := FWCompany()

Default nValida	:= 0 //0=Legado Seleção Livre
Default lMostraTela	:= .F.
Default aListaFil	:= {{.T., cFilAnt}}
Default lchkUser	:= .T.
Default lConsolida	:= .F.
Default lContEmp 	:= .F.
Default nOpca		:= 1
Default lJourney	:= .F.

//-- Carrega filiais da empresa corrente
lChkUser := !GetAPOInfo("FWFILIAL.PRW")[4] < CTOD("10/01/2013")

aEval(aSM0,{|x| If(x[SM0_GRPEMP] == cEmpAnt .And.;
	              Iif (!lContEmp ,x[SM0_EMPRESA] == cEmpresa,.T.) .And.;
	              (!lChkUser .Or. x[SM0_USEROK].Or. (lIsBlind .AND. !lJourney)) .And.;
	              (x[SM0_EMPOK] .Or. (lIsBlind .AND. !lJourney)),;
		          aAdd(aFilsCalc,{.F.,x[SM0_CODFIL],x[SM0_NOMRED],x[SM0_CGC],Posicione("SM0",1,x[SM0_GRPEMP]+x[SM0_CODFIL],"M0_INSC"), ;
		               Posicione("SM0",1,x[SM0_GRPEMP]+x[SM0_CODFIL],"M0_INSCM")}),;
			      NIL)})

If lConsolida
	aSort(aFilsCalc,,,{|x,y| x[4]+x[5]+x[6]+x[2] < y[4]+y[5]+x[6]+y[2]}) //-- Ordena por CNPJ, IE, Insc.Municipal e Codigo de Filial para facilitar a quebra de rotinas consolidadas
EndIf

//-- Monta tela para selecao de filiais
If lMostraTela
	If lMtFilCal
		//-- Ponto de entrada para montagem de tela alternativa p/ selecao de filiais
		aFilsCalc := If(ValType(aRetPE := ExecBlock('MTFILCAL',.F.,.F.,{aFilsCalc})) == "A",aRetPE,aFilsCalc)
	Else
		DEFINE MSDIALOG oDlg TITLE OemToAnsi(STR0036) STYLE DS_MODALFRAME From 145,0 To 445,628 OF oMainWnd PIXEL
		oDlg:lEscClose := .F.
		@ 05,15 TO 125,300 LABEL OemToAnsi(STR0037) OF oDlg  PIXEL
		If lConsolida
			@ 15,20 LISTBOX oQual VAR cVarQ Fields HEADER "",OemToAnsi(STR0035),OemToAnsi(STR0039),OemToAnsi(STR0054),OemToAnsi(STR0055),OemToAnsi(STR0070) SIZE 273,105 ON DBLCLICK (aFilsCalc:=MtFClTroca(oQual:nAt,aFilsCalc,nValida,@nSelCNPJIE,@cSelCNPJIE),oQual:Refresh()) NoScroll OF oDlg PIXEL
			bLine := {|| {If(aFilsCalc[oQual:nAt,1],oOk,oNo),aFilsCalc[oQual:nAt,2],aFilsCalc[oQual:nAt,3],Transform(aFilsCalc[oQual:nAt,4],"@R 99.999.999/9999-99"),aFilsCalc[oQual:nAt,5],aFilsCalc[oQual:nAt,6]}}
		Else
			@ 15,20 CHECKBOX oChkQual VAR lQual PROMPT OemToAnsi(STR0038) SIZE 50, 10 OF oDlg PIXEL ON CLICK (AEval(aFilsCalc, {|z| z[1] := If(z[1]==.T.,.F.,.T.)}), oQual:Refresh(.F.))
			@ 30,20 LISTBOX oQual VAR cVarQ Fields HEADER "",OemToAnsi(STR0035),OemToAnsi(STR0039),OemToAnsi(STR0054) SIZE 273,090 ON DBLCLICK (aFilsCalc:=MtFClTroca(oQual:nAt,aFilsCalc),oQual:Refresh()) NoScroll OF oDlg PIXEL
			bLine := {|| {If(aFilsCalc[oQual:nAt,1],oOk,oNo),aFilsCalc[oQual:nAt,2],aFilsCalc[oQual:nAt,3],Transform(aFilsCalc[oQual:nAt,4],"@R 99.999.999/9999-99")}}
		EndIf
		oQual:SetArray(aFilsCalc)
		oQual:bLine := bLine
		DEFINE SBUTTON FROM 134,240 TYPE 1 ACTION If(MtFCalOk(@aFilsCalc,.T.,.T.,lConsolida,nValida),oDlg:End(),) ENABLE OF oDlg
		DEFINE SBUTTON FROM 134,270 TYPE 2 ACTION If(MtFCalOk(@aFilsCalc,.F.,.T.,lConsolida,nValida),oDlg:End(),) ENABLE OF oDlg
		ACTIVATE MSDIALOG oDlg CENTERED
	EndIf
//-- Valida lista de filiais passada como parametro
Else
	//-- Checa parametros enviados
	For nX := 1 to Len(aListaFil)
		If (nAchou := aScan(aFilsCalc,{|x| x[2] == aListaFil[nX,2]})) > 0
			aFilsCalc[nAchou,1] := .T.
		EndIf
	Next nX
	//-- Valida e assume somente filial corrente em caso de problema
	If !MtFCalOk(@aFilsCalc,.T.,.F.,lConsolida,nValida)
		For nX := 1 To Len(aFilsCalc)
			//--  Adiciona filial corrente
			aFilsCalc[nX,1] := aFilsCalc[nX,2] == cFilAnt
		Next nX
	EndIf
EndIf
RestArea(aAreaSM0)
Return aFilsCalc

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿±±
±±³ Fun‡…o    ³ MtFCalOk (Original MA330FOk)                               ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Autor     ³ Microsiga Software S/A                   ³ Data ³ 22/01/06 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Descri‡…o ³ Checa marcacao das filiais para calculo por empresa        ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros ³ ExpA1 = Array com a selecao das filiais                    ³±±
±±³           ³ ExpL2 = Valida array de filiais (.t. se ok e .f. se cancel)³±±
±±³           ³ ExpL3 = Mostra tela de aviso no caso de inconsistencia     ³±±
±±³           ³ ExpL4 = Indica se consolida ou não o arquivo			   ³±±
±±³           ³ ExpN5 = 0=Legado/1=CNPJ iguais/2=CNPJ+IE iguais/3=CNPJ Raiz³±±
±±³           ³ 4=CNPJ+IE+Inscr.Municipal iguais                           ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³  Uso      ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function MtFCalOk(aFilsCalc,lValidaArray,lMostraTela,lConsolida,nValida)
Local lRet    	:= .F.
Local nX   	  	:= 0
Local nOpca		:= 0
Local nPos		:= 0
Local aEmpresas := {}

Default lMostraTela := .T.
Default lConsolida	:= .F.
Default nValida		:= 0

If !lValidaArray
	aFilsCalc := {}
	lRet := .T.
Else
	//-- Checa se existe alguma filial marcada na confirmacao
	If !(lRet := aScan(aFilsCalc,{|x| x[1]}) > 0) .And. lMostraTela
		Aviso(OemToAnsi(STR0034),OemToAnsi(STR0040),{"Ok"})
	EndIf

	//-- Se rotina consolidada, valida se todas as filiais da empresa (CNPJ+IE) foram marcadas
	If lRet .And. lConsolida
		For nX := 1 To Len(aFilsCalc)
			If nValida == 1         		// CNPJ Igual
				nPos := aScan(aEmpresas,{|x| x[3] == aFilsCalc[nX,4]})
			ElseIf nValida == 2			// CNPJ + I.E. iguais
				nPos := aScan(aEmpresas,{|x| x[1] == aFilsCalc[nX,4]+aFilsCalc[nX,5]})
			ElseIf nValida == 3			// CNPJ Raiz
				nPos := aScan(aEmpresas,{|x| x[4] == Substr(aFilsCalc[nX,4],1,8)})
			ElseIf nValida == 4			// CNPJ + Insc.Municipal iguais
				nPos := aScan(aEmpresas,{|x| x[5] == aFilsCalc[nX,4]+aFilsCalc[nX,6] })
			Else						// Legado - não valida
				nPos := 0
			EndIf

			If !Empty(nPos) .And. aFilsCalc[nX,1] # aEmpresas[nPos,2]
				If Empty(nOpca)
					If lMostraTela
						nOpca := Aviso(STR0030,STR0056,{STR0057,STR0058,STR0059},2) //"A execução desta rotina foi parametrizada para modo consolidado porém não foram selecionadas todas as filiais de uma ou mais empresas. Deseja que estas filiais sejam adicionadas a seleção ou mantém a seleção atual?"
					Else
						nOpca := 1
					EndIf
				EndIf
				If nOpca == 1
					aEmpresas[nPos,2] := .T.
				Else
					If nOpca == 3
						lRet := .F.
					EndIf
					Exit
				EndIf
			ElseIf Empty(nPos)
				aAdd(aEmpresas,{aFilsCalc[nX,4]+aFilsCalc[nX,5], aFilsCalc[nX,1], aFilsCalc[nX,4], Substr(aFilsCalc[nX,4],1,8), aFilsCalc[nX,4]+aFilsCalc[nX,6] })
			EndIf
		Next nX

		If nOpca == 1
			aFilsCalc := {}

			//-- Percorre SM0 adicionando filiais com CNPJ+IE marcados
			SM0->(dbGoTop())
			If nValida < 2         		// CNPJ Igual
				SM0->(dbEval({|| If(aScan(aEmpresas,{|x| M0_CODIGO == cEmpAnt .And. x[3] == M0_CGC .And. x[2]}) == 0,NIL,aAdd(aFilsCalc,{.T.,M0_CODFIL,M0_FILIAL,M0_CGC,M0_INSC,M0_INSCM}))}))
			ElseIf nValida == 2			// CNPJ + I.E. iguais
				SM0->(dbEval({|| If(aScan(aEmpresas,{|x| M0_CODIGO == cEmpAnt .And. x[1] == M0_CGC+M0_INSC .And. x[2]}) == 0,NIL,aAdd(aFilsCalc,{.T.,M0_CODFIL,M0_FILIAL,M0_CGC,M0_INSC,M0_INSCM}))}))
			ElseIf nValida == 3			// CNPJ Raiz
				SM0->(dbEval({|| If(aScan(aEmpresas,{|x| M0_CODIGO == cEmpAnt .And. x[4] == SubStr(M0_CGC,1,8) .And. x[2]}) == 0,NIL,aAdd(aFilsCalc,{.T.,M0_CODFIL,M0_FILIAL,M0_CGC,M0_INSC,M0_INSCM}))}))
			ElseIf nValida == 4			// CNPJ + Insc.Municipal iguais
				SM0->(dbEval({|| If(aScan(aEmpresas,{|x| M0_CODIGO == cEmpAnt .And. x[5] == M0_CGC+M0_INSCM .And. x[2]}) == 0,NIL,aAdd(aFilsCalc,{.T.,M0_CODFIL,M0_FILIAL,M0_CGC,M0_INSC,M0_INSCM}))}))
			EndIf

			//-- Ordena por CNPJ+IE+Ins.Mun+Codigo para facilitar a quebra da rotina
			aSort(aFilsCalc,,,{|x,y| x[4]+x[5]+x[6]+x[2] < y[4]+y[5]+x[6]+y[2]})

		ElseIf nOpca # 3
			//-- Deleta filiais que nao serao processadas
			nX := 1
			While nX <= Len(aFilsCalc)
				If !aFilsCalc[nX,1]
					aDel(aFilsCalc,nX)
					aSize(aFilsCalc,Len(aFilsCalc)-1)
				Else
					nX++
				EndIf
			End
		EndIf
	EndIf 
EndIf

Return lRet 

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿±±
±±³ Fun‡…o    ³ MtFClTroca(Original CA330Troca)                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Autor     ³ Microsiga Software S/A                   ³ Data ³ 12/01/06 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Descri‡…o ³ Troca marcador entre x e branco                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros ³ ExpN1 = Linha onde o click do mouse ocorreu                ³±±
±±³           ³ ExpA2 = Array com as opcoes para selecao                   ³±±
±±³           ³ ExpN3 = Valida seleção de CNPJ/IE na função MatFilCalc     ³±±
±±³           ³ 0=Legado não Valida / 1=CNPJ / 2=CNPJ+IE / 3=CNPJ Raiz     ³±±
±±³           ³ 4=CNPJ+IE+Insc.Municipal 							       ³±±
±±³           ³ ExpN4 = Quantidade de Itens marcados - CNPJ/IE iguais      ³±±
±±³           ³ ExpC5 = CNPJ/IE selecionado                                ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³  Uso      ³ Protheus 8.11                                              ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±³05/08/2013 ³ Wagner Montenegro 										   ³±±
±±³			  ³ Adicionado os parametros ExpL3,ExpN4,ExpC5 para permitir   ³±±
±±³			  ³ seleção apenas de CNPJ/IE iguais na função MatFilCalc	   ³±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function MtFClTroca(nIt,aArray,nValida,nSelCNPJIE,cSelCNPJIE)
Default nValida := 0 	//1= Valida apenas CNPJ com mesmo numero MatFilCalc() / 2=Valida apenas CNPJ+IE com mesmo numero MatFilCalc()
						//3= Valida CNPJ Raiz (8 primeiros dígitos) com mesmo número/ 4= Valida CNPJ+IE+Insc.Municipal com mesmo número
If nValida == 0
	aArray[nIt,1] := !aArray[nIt,1]
Else
	If aArray[nIt,1]
	   	nSelCNPJIE--
		If nSelCNPJIE==0
 	   		cSelCNPJIE:=""
		Endif
		aArray[nIt,1] := !aArray[nIt,1]
	Else
 		If nSelCNPJIE > 0
 	    	If ( nValida==1 .and. aArray[nIt,4]==cSelCNPJIE ) .or. ( nValida==2 .and. aArray[nIt,4]+aArray[nIt,5]==cSelCNPJIE ) .or.;
 	    		( nValida==3 .and. Substr(aArray[nIt,4],1,8) == Substr(cSelCNPJIE,1,8) ) .or. ( nValida==4 .and. aArray[nIt,4]+aArray[nIt,6] == cSelCNPJIE )
	 	   		nSelCNPJIE++
		 	   	aArray[nIt,1] := !aArray[nIt,1]
	 	  	Else
	 	  		If nValida == 1
	 	  			//'SIGACUSCNPJ' ; 'A Consolidação por CNPJ está habilitado. Selecione apenas Filiais com o mesmo CNPJ [' ; '] já marcado, ou refaça sua seleção!'
		 	  		Help(nil,1,STR0060,nil,STR0061+Transform(cSelCNPJIE,"@R 99.999.999/9999-99")+STR0062,3,0)
		 	  	ElseIf nValida == 2
		 	  	   //'SIGACUSCNPJIE' ; 'A Consolidação por CNPJ+IE está habilitado. Selecione apenas Filiais com o mesmo CNPJ+IE [' ; '] já marcado, ou refaça sua seleção!'
		 	  		Help(nil,1,STR0063,nil,STR0064+Transform(Substr(cSelCNPJIE,1,14),"@R 99.999.999/9999-99")+" - "+Substr(cSelCNPJIE,15)+STR0062,3,0)
		 	  	ElseIf nValida == 3
					//'SIGACUSCNPJP' ; 'A Consolidação por CNPJ Raiz está habilitado. Selecione apenas Filiais com o mesmo CNPJ Raiz [' ; '] já marcado, ou refaça sua seleção!'
					Help(nil,1,STR0066,nil,STR0067+Transform(Substr(cSelCNPJIE,1,8),"@R 99.999.999")+" - "+Substr(cSelCNPJIE,15)+STR0062,3,0)
		 	  	Else
					//'SIGACUSCNPJIM' ; 'A Consolidação por CNPJ + Insc.Municiap está habilitado. Selecione apenas Filiais com o mesmo CNPJ e Inscrição Municipal [' ; '] já marcado, ou refaça sua seleção!'
					Help(nil,1,STR0068,nil,STR0069+Transform(Substr(cSelCNPJIE,1,14),"@R 99.999.999/9999-99")+" - "+Substr(cSelCNPJIE,15)+STR0062,3,0)
		 	  	Endif
	 	   	Endif
		Else
			nSelCNPJIE++
			If nValida==1									// Valida CNPJ
				cSelCNPJIE := aArray[nIt,4]
			ElseIf nValida ==2								// Valida CNPJ + I.E.
				cSelCNPJIE := aArray[nIt,4]+aArray[nIt,5]
			ElseIf nValida ==3								// Valida CNPJ Raiz (oito primeiros dígitos)
				cSelCNPJIE := Subs(aArray[nIt,4],1,8)
			Else											// Valida CNPJ + Insc.Municipal
				cSelCNPJIE := aArray[nIt,4]+aArray[nIt,6]
			Endif
			aArray[nIt,1] := !aArray[nIt,1]
 		Endif
 	Endif
Endif
Return aArray

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ FullRange³ Autor ³ Felipe Nunes de Toledo³ Data ³ 30/01/07 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descricao ³ Converte os parametros do tipo range, para um range cheio, ³±±
±±³          ³ caso o conteudo do parametro esteja vazio.                 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³FullRange(cPerg)                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ cPerg = Nome do Grupo de Perguntas                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ Nenhum                                                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Generico                                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function FullRange(cPerg)
Local aArea      := { SX1->(GetArea()), GetArea() }
Local aTamSX3    := {}
Local cMvPar     := ''
Local nTamSX1Cpo := Len(SX1->X1_GRUPO)

cPerg := Upper(PadR(cPerg,nTamSX1Cpo))

SX1->( dbSetOrder(1) )
SX1->( MsSeek(cPerg) )
Do While SX1->( !Eof() .And. Trim(X1_GRUPO) == Trim(cPerg) )
	If Upper(SX1->X1_GSC) == 'R'
		cMvPar := 'MV_PAR'+SX1->X1_ORDEM
		If Empty(&(cMvPar))
			aTamSX3 := TamSx3(SX1->X1_CNT01)
			If Upper(aTamSX3[3]) == 'C'
				&(cMvPar) := Space(aTamSX3[1])+'-'+Replicate('z',aTamSX3[1])
			ElseIf Upper(aTamSX3[3]) == 'D'
				&(cMvPar) := '01/01/80-31/12/49'
			ElseIf Upper(aTamSX3[3]) == 'N'
				&(cMvPar) := Replicate('0',aTamSX3[1])+'-'+Replicate('9',aTamSX3[1])
			EndIf
		EndIf
	EndIf
	SX1->( dbSkip() )
EndDo

RestArea( aArea[1] )
RestArea( aArea[2] )
Return Nil

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ UsaNewPrc ³ Autor ³ Andre Anjos			 ³ Data ³ 10/09/08 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descricao ³ Analisa parametro MV_USANPRC, que define se sera utilizado  ³±±
±±³ 		 | o componente NewProces nas rotinas de processamento		   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ Logico                                                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Generico                                                    ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function UsaNewPrc()
Return (SuperGetMV("MV_USANPRC",.F.,.F.))

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ ARetBenef ³ Autor ³ Rodrigo Toledo	 	 ³ Data ³ 10/09/12 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descricao ³ Carrega no aCols da nota de entrada os itens que foram 	   ³±±
±±³			 ³ enviados para remessa de beneficiamento					   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ Logico			   										   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Retorno de Beneficiamento                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function ARetBenef()
Local lRet		:= .T.
Local cNumOp   	:= CriaVar("D1_OP")
Local cProduto 	:= CriaVar("C2_PRODUTO")
Local cTipEnt  	:= CriaVar("D1_TES")
Local nQtdProd  := 0
Local nPos   	:= 0
Local nIndBaixa := 0
Local oDlgBen  	:= Nil
Local lMonAcols	:= .F.
Local nBkp		:= n
Local nPITEM	:= GDFieldPos("D1_ITEM")
Local nPCOD     := GDFieldPos("D1_COD")

If Type("aOPBenef") # "A"
	aOPBenef := {}
EndIf

If cTipo # "N"
	Aviso(STR0034,STR0049,{"OK"}) //"Atenção#Esta funcionalidade não se aplica a este tipo de documento."
	lRet := .F.
EndIf

If Empty(cA100For) .Or. Empty(cLoja)
	Aviso(STR0034,STR0050,{"OK"}) //"Atenção##Fornecedor náo informado no cabeçalho do documento."
	lRet := .F.
EndIf

If lRet
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Monta a Tela de Retorno de Beneficiamento   ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	DEFINE MSDIALOG oDlgBen TITLE STR0046 From 9,0 To 20,50 OF oMainWnd	//"Retorno de Beneficiamento"

	@ 12,10 SAY RetTitle("D1_OP") PIXEL OF oDlgBen  //Numero da OP
	@ 10,52 MSGET cNumOp PICTURE PesqPict("SC2","C2_NUM") F3 "SC2" VALID CusVPrdOp(cNumOp,@cProduto,@nQtdProd) OF oDlgBen PIXEL SIZE 55,10
	@ 28,10 SAY RetTitle("C2_PRODUTO") PIXEL OF oDlgBen  //Produto
	@ 26,52 MSGET cProduto WHEN .F. OF oDlgBen PIXEL SIZE 95,10
	@ 44,10 SAY RetTitle("C2_QUANT") PIXEL OF oDlgBen  //Quantidade
	@ 42,52 MSGET nQtdProd VALID CusVQtdPR0(cNumOp,nQtdProd,@nIndBaixa) Picture PesqPict("SC2","C2_QUANT") OF oDlgBen PIXEL SIZE 55,10
	@ 60,10 SAY RetTitle("D1_TES") PIXEL OF oDlgBen  //Tipo Entrada
	@ 58,52 MSGET cTipEnt F3 "SF4" VALID CusVldTes(@cTipEnt) OF oDlgBen PIXEL SIZE 30,10
	DEFINE SBUTTON FROM 22,160  TYPE 1 ACTION (lRet := .T.,oDlgBen:End()) ENABLE OF oDlgBen
	DEFINE SBUTTON FROM 35,160  TYPE 2 ACTION (lRet := .F.,oDlgBen:End()) ENABLE OF oDlgBen

	ACTIVATE MSDIALOG oDlgBen

	If lRet
		If (nPos := aScan(aOPBenef,{|x| x[1]+x[3]+x[4] == cNumOP+aCols[n,nPCOD]+aCols[n,nPITEM]})) == 0
			aAdd(aOPBenef,{cNumOP,nQtdProd,aCols[n,nPCOD],aCols[n,nPITEM]})
		Else
			aOPBenef[nPos,2] := nQtdProd
		EndIf

		MsgRun(STR0051,STR0052,{|| lMonACols := AProcReBen(cNumOP,nQtdProd,nIndBaixa,cTipEnt)})//"Processando estrutura e busca pelos documentos origem..."Aguarde"

		If !lMonAcols
			Aviso(STR0030,STR0047,{"Ok"}) //"Atencao"###"Nao foi identificado nenhuma remessa de beneficiamento para esta ordem de produção."
		Else
			oGetDados:lNewLine := .F.
			oGetDados:oBrowse:Refresh()
		EndIf

		If ExistBlock("NFRETBEN")
			ExecBlock("NFRETBEN",.F.,.F.,{cA100For,cLoja,cNumOp,nQtdProd,cTipEnt})
		EndIf

		n := nBkp
	EndIf
EndIf

Return lRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³AProcReBenºAutor  ³ Rodrigo Toledo	 º Data ³  04/11/12   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Processa a criacao dos itens de retorno de beneficiamento. º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ ARetBenef	                                              º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function AProcReBen(cNumOP,nQtdProd,nIndBaixa,cTipEnt)
Local lRet      := .F.
Local lRetBN    := .F.
Local nLinha    := 0
Local nItemOri  := 0
Local nQtdEstr  := 0
Local nX	    := 0
Local nY	    := 0
Local nPITEM	:= GDFieldPos("D1_ITEM")
Local nPCOD     := GDFieldPos("D1_COD")
Local nPQUANT 	:= GDFieldPos("D1_QUANT")
Local nPOP      := GDFieldPos("D1_OP")
Local nPTES  	:= GDFieldPos("D1_TES")
Local nPNFORI   := GDFieldPos("D1_NFORI")
Local nPSERIORI := GDFieldPos("D1_SERIORI")
Local nPITEMORI := GDFieldPos("D1_ITEMORI")
Local nPVUNIT	:= GDFieldPos("D1_VUNIT")
Local nPTOTAL	:= GDFieldPos("D1_TOTAL")
Local nPIDENTB6 := GDFieldPos("D1_IDENTB6")
Local nPLoteCtl := GDFieldPos("D1_LOTECTL")
Local nPNumLote := GDFieldPos("D1_NUMLOTE")
Local nPNumSeri := GDFieldPos("D1_NUMSERI")
Local nPLocaliz := GDFieldPos("D1_LOCALIZ")
Local nProvEnt 	:= aScan(aHeader,{|x| AllTrim(Subs(x[2],4)) $ "PROVENT"})
Local aArea		:= SD4->(GetArea())
Local aAreaAnt	:= {}
Local aNFOrig   := {}

SD4->(dbSetOrder(2))
SD4->(dbSeek(xFilial("SD4")+cNumOp))
While SD4->(!EOF()) .And. SD4->(D4_FILIAL+D4_OP) ==  xFilial("SD4")+cNumOp

	// Proteção para processo de beneficiamento para produtos tipo BN
	If !A103ChkBN(cNumOp, SD4->D4_COD, SD4->D4_LOCAL, SD4->(Recno()))
		//-- Se totalmente baixado ou item negativo, pula
		If !(SD4->D4_QUANT > 0)
			SD4->(dbSkip())
			Loop
		EndIf
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³ Posiciona na tabela SD4 para buscar o saldo do componente ³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	Else
		lRetBN := .T.
	EndIf

	nQtdEstr := nIndBaixa * SD4->D4_QTDEORI

	If A440F4("SB6",SD4->D4_COD,Posicione("SB1",1,xFilial("SB1")+SD4->D4_COD,'B1_LOCPAD'),"B6_PRODUTO","E",cA100For,cLoja,.F.,.F.,0,IIF(cTipo=="N","F","C"),@aNFOrig,.T.) > 0
		nX := 0
		While nQtdEstr > 0 .And. nX < Len(aNFOrig)
			For nX := 1 To Len(aNFOrig)

				// Tratamento para evitar que o novo item do aCols seja preenchido com os mesmos dados de um item anterior
				If !Empty(aNFOrig[nX,10])
					nItemOri := aScan(aCols,{|x| x[nPNFORI]+x[nPSERIORI]+Rtrim(x[nPITEMORI]+x[nPOP]) == aNFOrig[nX,1]+aNFOrig[nX,2]+aNFOrig[nX,10]+cNumOP})
				Else
					nItemOri := aScan(aCols,{|x| x[nPNFORI]+x[nPSERIORI]+x[nPIDENTB6] == aNFOrig[nX,1]+aNFOrig[nX,2]+aNFOrig[nX,12]})
				EndIf
				If nItemOri > 0
					If aCols[nItemOri][nPOP] != cNumOP
						Loop
					EndIf
				EndIf

				// Tratamento para retornar a nota correta quando se tratar de retorno de beneficiamento
				If lRetBN .And. !Empty(aNFOrig[nX,12])
					aAreaAnt := GetArea()
					SD2->(DbSetOrder(4))	// D2_FILIAL+D2_NUMSEQ
					If SD2->(DbSeek(xFilial("SD2") + aNFOrig[nX,12]))
						SGO->(DbSetOrder(2))	// GO_FILIAL+GO_NUMPV+GO_ITEMPV+GO_OP+GO_COD+GO_LOCAL
						If !SGO->(DbSeek(xFilial("SGO") + SD2->D2_PEDIDO + SD2->D2_ITEMPV + cNumOP + SD2->D2_COD + SD2->D2_LOCAL))
							Loop
						EndIf
					EndIf
					RestArea(aAreaAnt)
				EndIf
				
				nLinha := aScan(aCols,{|x| x[nPCOD]+x[nPOP]+x[nPNFORI]+x[nPSERIORI]+Rtrim(x[nPITEMORI]) == SD4->D4_COD+cNumOP+aNFOrig[nX,1]+aNFOrig[nX,2]+aNFOrig[nX,10]})

		 		If Empty(nLinha)
		 			If !Empty(aCols[n,nPCOD]) .And. !Empty(aCols[n,nPTES])
						nLinha := Len(aCols) + 1
						aAdd(aCols,Array(Len(aHeader)+1))
						For nY := 1 to Len(aHeader)
							If IsHeadRec(aHeader[nY,2])
								aCols[nLinha,nY] := 0
							ElseIf IsHeadAlias(aHeader[nY,2])
								aCols[nLinha,nY] := "SD1"
                                    Else
								aCols[nLinha,nY] := CriaVar(aHeader[nY,2],.T.)
							EndIf
						Next nY
						aTail(aCols[nLinha]) := .F.
						aCols[nLinha,nPITEM] := StrZero(nLinha,TamSX3("D1_ITEM")[1])
					Else
						nLinha := n
					EndIf
				EndIf

				n := nLinha

				// Executa as funcoes do CheckSX3 e forca a execucao dos trigers
				aCols[nLinha,nPCOD] := SD4->D4_COD
				M->D1_COD := aCols[nLinha,nPCOD]
				__READVAR := "M->D1_COD"
				CheckSX3("D1_COD",aCols[nLinha,nPCOD])
				If ExistTrigger('D1_COD')
					RunTrigger(2,nLinha,,'D1_COD')
				EndIf

				aCols[nLinha,nPQUANT] := Min(nQtdEstr,Val(aNFOrig[nX,4]))
				M->D1_QUANT := aCols[nLinha,nPQUANT]
				__ReadVar := 'M->D1_QUANT'
				CheckSX3('D1_QUANT',aCols[nLinha,nPQUANT])
				If ExistTrigger('D1_QUANT')
					RunTrigger(2,nLinha,,'D1_QUANT')
				EndIf

				aCols[nLinha,nPTES] := cTipEnt
				M->D1_TES := aCols[nLinha,nPTES]
				__ReadVar := 'M->D1_TES'
				CheckSX3('D1_TES',aCols[nLinha,nPTES])
				If ExistTrigger('D1_TES')
					RunTrigger(2,nLinha,,'D1_TES')
				EndIf

				aCols[nLinha,nPVUNIT] := aNFOrig[nX,11]
				M->D1_VUNIT := aCols[nLinha,nPVUNIT]
				__ReadVar := 'M->D1_VUNIT'
				If cPaisLoc == "BRA" .Or. (cPaisLoc == "ARG" .And. aCols[nLinha,nPVUNIT] > 0)
					CheckSX3('D1_VUNIT',aCols[nLinha,nPVUNIT])
				EndIf
				If ExistTrigger('D1_VUNIT')
					RunTrigger(2,nLinha,,'D1_VUNIT')
				EndIf

				aCols[nLinha,nPTOTAL] := aCols[nLinha,nPQUANT] * aCols[nLinha,nPVUNIT]
				M->D1_TOTAL := aCols[nLinha,nPTOTAL]
				__ReadVar := 'M->D1_TOTAL'
				CheckSX3('D1_TOTAL',aCols[nLinha,nPTOTAL])
				If ExistTrigger('D1_TOTAL')
					RunTrigger(2,nLinha,,'D1_TOTAL')
				EndIf

				aCols[nLinha,nPNFORI] := aNFOrig[nX,1]
				M->D1_NFORI := aCols[nLinha,nPNFORI]
				__ReadVar := 'M->D1_NFORI'
				CheckSX3('D1_NFORI',aCols[nLinha,nPNFORI])
				If ExistTrigger('D1_NFORI')
					RunTrigger(2,nLinha,,'D1_NFORI')
				EndIf

				aCols[nLinha,nPSERIORI] := aNFOrig[nX,2]
				M->D1_SERIORI := aCols[nLinha,nPSERIORI]
				__ReadVar := 'M->D1_SERIORI'
				CheckSX3('D1_SERIORI',aCols[nLinha,nPSERIORI])
				If ExistTrigger('D1_SERIORI')
					RunTrigger(2,nLinha,,'D1_SERIORI')
				EndIf

				aCols[nLinha,nPITEMORI] := aNFOrig[nX,10]
				M->D1_ITEMORI := aCols[nLinha,nPITEMORI]
				__ReadVar := 'M->D1_ITEMORI'
				CheckSX3('D1_ITEMORI',aCols[nLinha,nPITEMORI])
				If ExistTrigger('D1_ITEMORI')
					RunTrigger(2,nLinha,,'D1_ITEMORI')
				EndIf

				aCols[nLinha,nPIDENTB6] := aNFOrig[nX,12]
				M->D1_IDENTB6 := aCols[nLinha,nPIDENTB6]
				__ReadVar := 'M->D1_IDENTB6'
				CheckSX3('D1_IDENTB6',aCols[nLinha,nPIDENTB6])
				If ExistTrigger('D1_IDENTB6')
					RunTrigger(2,nLinha,,'D1_IDENTB6')
				EndIf

				aCols[nLinha,nPOP] := cNumOp
				M->D1_OP := aCols[nLinha,nPOP]
				__ReadVar:='M->D1_OP'
				CheckSX3('D1_OP',aCols[nLinha,nPOP])
				If ExistTrigger('D1_OP')
					RunTrigger(2,nLinha,,'D1_OP')
				EndIf

				If nPLoteCtl > 0
					aCols[nLinha,nPLoteCtl] := aNFOrig[nX,6]
					M->D1_LOTECTL := aCols[nLinha,nPLoteCtl]
					__ReadVar:='M->D1_LOTECTL'
					CheckSX3('D1_LOTECTL',aCols[nLinha,nPLoteCtl])
					If ExistTrigger('D1_LOTECTL')
						RunTrigger(2,nLinha,,'D1_LOTECTL')
					EndIf
				Endif

				If nPNumLote > 0
					aCols[nLinha,nPNumLote] := aNFOrig[nX,7]
					M->D1_NUMLOTE := aCols[nLinha,nPNumLote]
					__ReadVar:='M->D1_NUMLOTE'
					CheckSX3('D1_NUMLOTE',aCols[nLinha,nPNumLote])
					If ExistTrigger('D1_NUMLOTE')
						RunTrigger(2,nLinha,,'D1_NUMLOTE')
					EndIf
				Endif

				If nPNumSeri > 0
					aCols[nLinha,nPNumSeri] := aNFOrig[nX,13]
					M->D1_NUMSERI := aCols[nLinha,nPNumSeri]
					__ReadVar:='M->D1_NUMSERI'
					CheckSX3('D1_NUMSERI',aCols[nLinha,nPNumSeri])
					If ExistTrigger('D1_NUMSERI')
						RunTrigger(2,nLinha,,'D1_NUMSERI')
					EndIf
				Endif

				If nPLocaliz > 0
					aCols[nLinha,nPLocaliz] := aNFOrig[nX,14]
					M->D1_LOCALIZ := aCols[nLinha,nPLocaliz]
					__ReadVar:='M->D1_LOCALIZ'
					CheckSX3('D1_LOCALIZ',aCols[nLinha,nPLocaliz])
					If ExistTrigger('D1_LOCALIZ')
						RunTrigger(2,nLinha,,'D1_LOCALIZ')
					EndIf
				Endif

				//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
				//³ Atualiza a Funcao Fiscal                    ³
				//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
				MaColsToFis(aHeader,aCols,nLinha,"MT100",.F.)
				If cPaisLoc == "ARG" .And. nProvEnt > 0
					MaFisAlt("IT_PROVENT",aCols[nLinha,nProvEnt],nLinha)
				Endif
				If IsInCallStack("LOCXNF")
					Eval(bDoRefresh) //Atualiza o folder financeiro.
					Eval(bListRefresh)
				EndIf

				nQtdEstr -= Min(nQtdEstr,Val(aNFOrig[nX,4]))
				lRet := .T.
				If nQtdEstr <= 0
					Exit
				EndIf
			Next nX
		End
	EndIf

	SD4->(dbSkip())
End

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Apos atualizacao do aCols move o cursor para primeira linha do aCols ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
n := 1

SD4->(RestArea(aArea))
Return lRet


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ CusCarrPd ³ Autor ³ Rodrigo Toledo	 	 ³ Data ³ 27/08/12 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descricao ³ Caso a ordem de producao seja preenchida gatilhar o codigo  ³±±
±±³			 ³ do produto PA				   							   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ cOP   = Numero da Ordem de Producao              		   ³±±
±±³			 ³ cPrdOrd = Codigo do produto da ordem de producao			   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ Logico			   										   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Retorno de Beneficiamento                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function CusVPrdOp(cOP,cPrdOrd,nQuant)
Local lRet 	   := .T.
Local aAreaSC2 := SC2->(GetArea())

SC2->(dbSetOrder(1))
If SC2->(dbSeek(xFilial("SC2")+cOP))
	If !Empty(SC2->C2_DATRF)
		Help(" ",1,"MA240OPENC")
		lRet := .F.
	Else
		nQuant  := SC2->(C2_QUANT - C2_QUJE - C2_PERDA)
		cPrdOrd := SC2->C2_PRODUTO
	EndIf
Else
	Help(" ",1,"REGNOIS")
	lRet := .F.
EndIf

RestArea(aAreaSC2)
Return lRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o    ³ CusVldTes ³ Autor ³ Rodrigo Toledo	 	 ³ Data ³ 10/09/12 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descricao ³ Valida o codigo da TES informada pelo usuario  		   	   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ cCodTes = Codigo da TES              		   			   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ Logico			   										   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Retorno de Beneficiamento                                   ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function CusVldTes(cCodTES)
Local lRet 	   := .T.
Local aAreaSF4 := GetArea()

SF4->(dbSetOrder(1))
If !SF4->(dbSeek(xFilial("SF4")+cCodTES))
	Help('   ',1,'A103TPNFOR')
	lRet := .F.
ElseIf cTipo == "D" .And. SF4->F4_PODER3 <> "N"
	Help('   ',1,'A103TESNFD')
	lRet := .F.
ElseIf cTipo$"NB" .And. SF4->F4_PODER3 <> "D"
	Help('   ',1,'A103TESNFB')
	lRet := .F.
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica se a TES digitada pode ser utilizada na operacao ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If lRet
	lRet := MaAvalTes("E",cCodTES)
EndIf

RestArea(aAreaSF4)
Return lRet


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³MTEstornPR ºAutor  ³Rodrigo Toledo     º Data ³  18/09/12   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³Verifica se os empenhos foram baixados e se a OP tem saldo  º±±
±±º			 ³caso as duas condicoes sejam atendidas executar a rotina	  º±±
±±º			 ³automatica MATA250 para digitacoes das OP's	  			  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºParametros³ExpC1: Numero do documento de entrada           			  º±±
±±º			 ³ExpC2: Serie do documento de entrada           			  º±±
±±º			 ³ExpC3: Codigo do fornecedor           			  		  º±±
±±º			 ³ExpC4: Loja do fornecedor           			  		  	  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³Retorno de Beneficiamento                                   º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function MTEstornPR(cDoc,cSerie,cFornec,cLoja)
Local aAreaSD3 := SD3->(GetArea())
Local lRet     := .T.
Local aApont := {}
Local nRecsd3 := 0

Private lMsErroAuto := .F.

	SD3->(dbSetOrder(13))
	SD3->(dbSeek(xFilial("SD3")+xFilial("SF1")+cDoc+cSerie+cFornec+cLoja))

	Begin Transaction

	While !SD3->(EOF()) .And. AllTrim(SD3->(D3_FILIAL+D3_CHAVEF1)) == AllTrim(xFilial("SD3")+xFilial("SF1")+cDoc+cSerie+cFornec+cLoja)
		nRecsd3 := SD3->(Recno())
		If SD3->D3_ESTORNO # "S"
			aApont := {}
			Aadd(aApont,{"D3_DOC"    ,SD3->D3_DOC       ,Nil})
			Aadd(aApont,{"D3_OP"     ,SD3->D3_OP        ,Nil})
			Aadd(aApont,{"D3_COD"    ,SD3->D3_COD       ,Nil})
			Aadd(aApont,{"D3_UM"     ,SD3->D3_UM        ,Nil})
			Aadd(aApont,{"D3_QUANT"  ,SD3->D3_QUANT     ,Nil})
			Aadd(aApont,{"D3_LOCAL"  ,SD3->D3_LOCAL     ,Nil})
			Aadd(aApont,{"D3_CC"     ,SD3->D3_CC        ,Nil})
			Aadd(aApont,{"D3_EMISSAO",SD3->D3_EMISSAO   ,Nil})
			Aadd(aApont,{"D3_LOTECTL",SD3->D3_LOTECTL   ,Nil})
			Aadd(aApont,{"D3_DTVALID",SD3->D3_DTVALID   ,Nil})
			Aadd(aApont,{"D3_NUMSEQ" ,SD3->D3_NUMSEQ    ,Nil})
			Aadd(aApont,{"D3_CHAVE"  ,SD3->D3_CHAVE     ,Nil})
			Aadd(aApont,{"D3_CF"     ,"PR0"             ,Nil})
			aAdd(aApont,{"D3_CHAVEF1",SD3->D3_CHAVEF1   ,Nil})
			aAdd(aApont,{"RETBEN"    ,"T"               ,Nil})
			aAdd(aApont,{"INDEX"     , 4                ,Nil})
			MsExecAuto({|x,y| MATA250(x,y)},aApont,5)
			If lMsErroAuto
				DisarmTransaction()
				If !IsBlind()
					Mostraerro()
				EndIf
				lRet := .F.
				Exit
			EndIf
		Endif
		SD3->(dbSetOrder(13))
		SD3->(Dbgoto(nRecsd3))
		SD3->(dbSkip())
	EndDo

	End Transaction

	RestArea(aAreaSD3)

Return lRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³MTIncluiPR ºAutor  ³Rodrigo Toledo     º Data ³  30/08/12   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³Verifica se os empenhos foram baixados e se a OP tem saldo, º±±
±±º			 ³caso as duas condicoes sejam atendidas executar a rotina	  º±±
±±º			 ³automatica MATA250 para digitacoes das OP's	  			  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºParametros³ExpA1: Array contendo as OP's do remito de entrada          º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³MTIncluiPR()                                                º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function MTIncluiPR(aNFOps)
Local lRet     := .T.
Local nX       := 0
Local aMata250 := {}
Local cChaveF1 := SF1->(F1_FILIAL+F1_DOC+F1_SERIE+F1_FORNECE+F1_LOJA)
Local cFornecedor	:=SF1->F1_FORNECE
Local cLoja			:=SF1->F1_LOJA
Local cTMPAD   := SuperGetMv("MV_TMPAD")
Local cReqAut   := SuperGetMv("MV_REQAUT", .F. ,"A")
Local lFornece := SD3->(ColumnPos("D3_FORNDOC")) > 0 .And. SD3->(ColumnPos("D3_LOJADOC")) > 0

Default aNFOps := {}

Private lMsErroAuto := .F.

Begin Transaction

For nX := 1 To Len(aNFOps)
	aMata250 := {}
	aAdd(aMata250,{'D3_TM'         ,cTMPAD            ,Nil})
	aAdd(aMata250,{'D3_OP'         ,aNFOps[nX,1]      ,Nil})
	aAdd(aMata250,{'D3_QUANT'      ,aNFOps[nX,2]      ,Nil})
	aAdd(aMata250,{'D3_CHAVEF1'    ,cChaveF1          ,Nil})
	If lFornece
		aAdd(aMata250,{'D3_FORNDOC',cFornecedor		  ,Nil})
		aAdd(aMata250,{'D3_LOJADOC',cLoja			  ,Nil})
	EndIf
	If (cReqAut == 'A')
		aAdd(aMata250,{'REQAUT'    ,"A"               ,Nil})
	Else
		aAdd(aMata250,{'REQAUT'    ,"N"               ,Nil})
	EndIf
	aAdd(aMata250,{"RETBEN"        ,"T"               ,Nil})
	MSExecAuto({|x,y| mata250(x,y)},aMata250,3)
	If lMsErroAuto
		DisarmTransaction()
		If !IsBlind()
			Mostraerro()
		EndIf
		lRet := .F.
	EndIf
Next nX

End Transaction

Return lRet

//---------------------------------------------------------------------------
/*/{Protheus.doc} MTBuscaRE5
Esta funcao verifica se ja foi gerada requisicao do tipo RE5 para o produto
no retorno de beneficiamento via documento de entrada, para nao gerar
requisicao e baixa de empenhos em duplicidade no apontamento automatico da
producao.
Utilizada no programa MATA250 quando for um retorno de beneficiamento parcial.

@param   cChaveD1 - Chave para localizar o registro SD1 composta pelos campos:
                    D1_FILIAL + D1_DOC + D1_SERIE + D1_FORNECE + D1_LOJA
         cProduto - Codigo do produto
@author  carlos.capeli
@version Todas
@since   30/06/2021
@return  ExpL: Indica se exite RE5
/*/
//---------------------------------------------------------------------------
Function MTBuscaRE5(cChaveD1, cProduto, cOP)

	Local cQuery := ""
	Local cAliasRE5 := GetNextAlias()
	Local lRet := .F.
	Local aAreaAnt := {}

	Default cChaveD1 := ""
	Default cProduto := ""
	Default cOP      := ""

	If !Empty(cChaveD1) .And. !Empty(cProduto) .And. !Empty(cOP)

		aAreaAnt := GetArea()

		cQuery := "SELECT SD3.D3_CF "
		cQuery += "FROM " + RetSqlName("SD3") + " SD3 "
		cQuery += "INNER JOIN " + RetSqlName("SD1") + " SD1 "
		cQuery += "ON SD1.D1_NUMSEQ = SD3.D3_NUMSEQ "
		cQuery += "AND SD1.D1_COD = '" + cProduto + "' "
		cQuery += "AND SD1.D1_OP = '" + cOP + "' "
		cQuery += "AND SD1.D_E_L_E_T_ = ' ' "
		cQuery += "WHERE SD3.D3_FILIAL = '" + xFilial("SD3") + "' "
		cQuery += "AND SD3.D3_CF = 'RE5' "
		cQuery += "AND SD3.D3_OP = '" + cOP + "' "
		cQuery += "AND SD3.D3_ESTORNO = ' ' "
		cQuery += "AND SD3.D_E_L_E_T_ = ' ' "

		cQuery := ChangeQuery(cQuery)

		dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasRE5)

		If !(cAliasRE5)->(Eof())
			lRet := .T.
		EndIf

		(cAliasRE5)->(DbCloseArea())

		RestArea(aAreaAnt)
	EndIf

Return lRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³CusVQtdPR0 ºAutor  ³Rodrigo Toledo     º Data ³  09/11/12   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³Valida se a quantidade eh superior ao saldo da ordem de     º±±
±±º			 ³producao.	  												  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºParametros³ExpC1: Numero da Ordem de Producao				          º±±
±±º			 ³ExpN1: Quantidade da Ordem de Proucao			              º±±
±±º			 ³ExpN2: Indica a baixa de OP			              		  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³MTIncluiPR()                                                º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function CusVQtdPR0(cNumOp,nQtdProd,nIndBaixa)
Local lRet := .T.

SC2->(dbSetOrder(1))
SC2->(dbSeek(xFilial("SC2")+cNumOp))

If nQtdProd > SC2->(C2_QUANT - C2_QUJE)
	Aviso(STR0034,STR0053,{"OK"}) //Atenção##Quantidade superior ao saldo da ordem de produÇÃo.
	lRet := .F.
Else
	nIndBaixa := Min(1,nQtdProd/SC2->C2_QUANT)
EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} SIGACUS_V
Verifica a data da última alteração do SIGACUS (DEVERÁ SER RETIRADA APÓS A DIVISÃO DESTE FONTE ENTRE OS MÓDULOS)

@author jose.eulalio
@since 08/05/2014
@version P12
@return nRet
/*/
//-------------------------------------------------------------------
Function SIGACUS_V
Local nRet := 20140508
Return nRet
/*
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡ao	 ³RastroToNF  ³ Autor ³ TOTVS S/A   	    ³ Data ³ 10/11/14 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Rastreamento dos documentos de entrada atraves Lote/SubLote³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso		 ³ GENERICO													  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ cLoteCtl - Codigo do Lote                                  ³±±
±±³          ³ cSubLote - Codigo do SubLote                               ³±±
±±³          ³ cProdut  - Codigo do Produto                               ³±±
±±³          ³ cLocal   - Codigo do Armazem                               ³±±
±±³          ³ nRecOrig - USO INTERNO                                     ³±±
±±³          ³ cUltSeq  - USO INTERNO                                     ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function RastroToNF(cLoteCtl,cSubLote,cProdut,cLocal,nRecOrig,cUltSeq)

Local aArea    := GetArea()
Local aAreaSD5 := SD5->(GetArea())
Local aRetorno := {}
Private aRecur := {}
Private lRecur := .F.

aRetorno := RastrToNF2(cLoteCtl,cSubLote,cProdut,cLocal,nRecOrig,cUltSeq)

RestArea(aAreaSD5)
RestArea(aArea)

Return aRetorno
/*
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡ao	 ³RastrToNF2 ³ Autor ³ TOTVS S/A   	    ³ Data ³ 10/11/14 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Rastreamento dos documentos de entrada atraves Lote/SubLote³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso		 ³ GENERICO													  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ cLoteCtl - Codigo do Lote                                  ³±±
±±³          ³ cSubLote - Codigo do SubLote                               ³±±
±±³          ³ cProdut  - Codigo do Produto                               ³±±
±±³          ³ cLocal   - Codigo do Armazem                               ³±±
±±³          ³ nRecOrig - USO INTERNO                                     ³±±
±±³          ³ cUltSeq  - USO INTERNO                                     ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function RastrToNF2(cLoteCtl,cSubLote,cProdut,cLocal,nRecOrig,cUltSeq)
Local aArea    := GetArea()
Local aAreaSD5 := SD5->(GetArea())
Local aRetorno := {}
Local aLoteRE4 := {}
Local cTmSD3   := ""
Local cQuery   := ""
Local cCliFor  := ""
Local cCompara := ""
Local cAliasSD5:= "SD5"
Local cCampos  := "D5_FILIAL+D5_PRODUTO+D5_LOCAL+D5_LOTECTL"
Local lRastro  := Rastro(cProdut)
Local lSubLote := Rastro(cProdut,"S")
Local nX       := 0
Local nPos     := 0
Local lQuery   := .F.
Local oRetorno := JsonObject():new()
Local cKeyScan := ""

Default cUltSeq  := ""
Default cLoteCtl := ""
Default cSubLote := ""

If Ascan(aRecur,{|x| x == cProdut+cLoteCtl+cSubLote+cLocal}) > 0
	ConOut(STR0080+Alltrim(cProdut)+STR0081+Alltrim(cLoteCtl)+Alltrim(cSubLote)+STR0082+Alltrim(cLocal)+STR0083)//"RastrToNF2 - Existe recursividade no produto '"##"', lote/sublote '"##"' no armazém '"##"'. As informações para este item serão ignoradas."
	lRecur := .T.
Else
	AADD(aRecur,cProdut+cLoteCtl+cSubLote+cLocal)
EndIf

If !lRecur .And. !Empty(cProdut) .And. (lRastro .Or. lSubLote)
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Recupera as notas de entrada de composicao do saldo do lote                   ³
	//| aRetorno - Array contendo os itens da nota de entrada                         |
	//| aRetorno[nX,01] - Numero Documento     (D1_DOC)                               |
	//| aRetorno[nX,02] - Numero de Serie      (D1_SERIE)                             |
	//| aRetorno[nX,03] - Codigo do Fornecedor (D1_FORNECE)                           |
	//| aRetorno[nX,04] - Codigo da Loja       (D1_LOJA)                              |
	//| aRetorno[nx,05] - Codigo do Item da NF (D1_ITEM)                              |
	//| aRetorno[nX,06] - Codigo do Produto    (D1_COD)                               |
	//| aRetorno[nX,07] - Codigo do Armazem    (D1_LOCAL)                             |
	//| aRetorno[nX,08] - Codigo do Lote       (D1_LOTECTL)                           |
	//| aRetorno[nX,09] - Codigo do SubLote    (D1_NUMLOTE)                           |
	//| aRetorno[nX,10] - Numero Sequencial do item da NF (D1_NUMSEQ)                 |
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	dbSelectArea("SD5")
	dbSetOrder(2)
	#IFDEF TOP
		cAliasSD5 := GetNextAlias()
		cCliFor   := Space(Len(SD5->D5_CLIFOR))
		If Select(cAliasSD5) > 0
			dbSelectArea(cAliasSD5)
			dbCloseArea()
		EndIf
		lQuery := .T.
		cQuery := " SELECT D5_FILIAL,  D5_PRODUTO, "
		cQuery +=        " D5_LOCAL,   D5_LOTECTL, "
		cQuery +=        " D5_NUMLOTE, D5_NUMSEQ,  "
		cQuery +=        " D5_CLIFOR,  D5_LOJA,    "
		cQuery +=        " D5_ORIGLAN, D5_DOC,     "
		cQuery +=        " D5_SERIE,   "
		cQuery +=        " SD5.R_E_C_N_O_ RECNOSD5 "
		cQuery += " FROM " +RetSqlName('SD5') + ' SD5 '
		cQuery += " WHERE "
		cQuery +=         " SD5.D5_FILIAL  = '"+xFilial("SD5")+"'"
		cQuery +=     " AND SD5.D5_PRODUTO = '"+cProdut+"'"
		cQuery +=     " AND SD5.D5_LOCAL   = '"+cLocal+"'"
		If !Empty(cLoteCtl)
			cQuery += " AND SD5.D5_LOTECTL = '"+cLoteCtl+"'"
		EndIf
		If !Empty(cSubLote).And. lSubLote
			cQuery += " AND SD5.D5_NUMLOTE = '"+cSubLote+"'"
		EndIf
		cQuery +=     " AND SD5.D5_ESTORNO  = ' '"
		cQuery +=     " AND SD5.D5_ORIGLAN <> 'MAN'"
		cQuery +=     " AND SD5.D5_ORIGLAN <= '500'"
		cQuery +=     " AND SD5.D5_CLIFOR  <> '"+cCliFor+"' "
		cQuery +=     " AND SD5.D_E_L_E_T_  = ' '"
		cQuery += " ORDER BY "+SqlOrder(SD5->(IndexKey()))
		cQuery := ChangeQuery(cQuery)
		dbUseArea(.T.,'TOPCONN',TCGENQRY(,,cQuery),cAliasSD5,.F.,.T.)
		dbSelectArea(cAliasSD5)

	#ELSE

		If lSubLote .And. !Empty(cSubLote)
			cCampos +="+D5_NUMLOTE"
			cCompara+=cSubLote
		EndIf

		cCompara := xFilial('SD5')+cProdut+cLocal+cLoteCtl

	#ENDIF

	Do While !Eof() .And. (lQuery .Or. cCompara == &cCampos)
		//-- Impede o Processamento de Movimentacoes Estornadas
		If !lQuery .And. !Empty((cAliasSD5)->D5_ESTORNO)
			dbSkip()
			Loop
		EndIf

		// Entrada de Material
		dbSelectArea("SD1")
		dbSetOrder(5)
		// Nota fiscal de Entrada
		If MsSeek(xFilial("SD1")+(cAliasSD5)->D5_PRODUTO+(cAliasSD5)->D5_LOCAL+(cAliasSD5)->D5_NUMSEQ )
			aAdd(aRetorno,{SD1->D1_DOC,SD1->D1_SERIE,SD1->D1_FORNECE,SD1->D1_LOJA,SD1->D1_ITEM,SD1->D1_COD,SD1->D1_LOCAL,SD1->D1_LOTECTL,SD1->D1_NUMLOTE,SD1->D1_NUMSEQ})
		EndIf
		dbSelectArea(cAliasSD5)
		dbSkip()
	EndDo

	// Encerra area de trabalho temporaria
	If lQuery
	   (cAliasSD5)->(dbCloseArea())
	EndIf

	// Caso nao tenha documento de entrada procurar por transferencia de lotes (RE4/DE4)
	If Empty(aRetorno)
		dbSelectArea("SD5")
		dbSetOrder(2)
		#IFDEF TOP
			cAliasSD5 := GetNextAlias()
			If Select(cAliasSD5) > 0
				dbSelectArea(cAliasSD5)
				dbCloseArea()
			EndIf
			lQuery := .T.
			cQuery := " SELECT D5_FILIAL,  D5_PRODUTO, "
			cQuery +=        " D5_LOCAL,   D5_LOTECTL, "
			cQuery +=        " D5_NUMLOTE, D5_NUMSEQ,  "
			cQuery +=        " D5_CLIFOR,  D5_LOJA,    "
			cQuery +=        " D5_ORIGLAN, D5_DOC,     "
			cQuery +=        " D5_SERIE,   "
			cQuery +=        " SD5.R_E_C_N_O_ RECNOSD5 "
			cQuery += " FROM " +RetSqlName('SD5') + ' SD5 '
			cQuery += " WHERE "
			cQuery +=         " SD5.D5_FILIAL  = '"+xFilial("SD5")+"'"
			cQuery +=     " AND SD5.D5_PRODUTO = '"+cProdut+"'"
			cQuery +=     " AND SD5.D5_LOCAL   = '"+cLocal+"'"
			If !Empty(cLoteCtl)
				cQuery += " AND SD5.D5_LOTECTL = '"+cLoteCtl+"'"
			EndIf
			If !Empty(cSubLote).And. lSubLote
				cQuery += " AND SD5.D5_NUMLOTE = '"+cSubLote+"'"
			EndIf
			cQuery +=     " AND SD5.D5_ESTORNO = ' '"
			cQuery +=     " AND SD5.D5_ORIGLAN = '499'" //Devolucao interna transferencia
			cQuery +=     " AND SD5.D_E_L_E_T_ = ' '"
			cQuery += " ORDER BY "+SqlOrder(SD5->(IndexKey()))
			cQuery := ChangeQuery(cQuery)
			dbUseArea(.T.,'TOPCONN',TCGENQRY(,,cQuery),cAliasSD5,.F.,.T.)
			dbSelectArea(cAliasSD5)

		#ELSE

			If lSubLote .And. !Empty(cSubLote)
				cCampos +="+D5_NUMLOTE"
				cCompara+=cSubLote
			EndIf

			cCompara := xFilial('SD5')+cProdut+cLocal+cLoteCtl

		#ENDIF

		Do While !Eof() .And. (lQuery .Or. cCompara == &cCampos)
			cTmSD3  := ""

			// Verifica procedencia do registro para evitar loop eterno
			If (nRecorig != Nil .And. IIf(lQuery,(cAliasSD5)->RECNOSD5 == nRecOrig,(Recno() == nRecOrig)) )
				dbSkip()
				Loop
			EndIf

			//-- Impede o Processamento de Movimentacoes Estornadas
			If !lQuery .And. !Empty((cAliasSD5)->D5_ESTORNO)
				dbSkip()
				Loop
			EndIf

			//-- Impede voltar para um numero sequencial anterior
			If !Empty(cUltSeq) .And. (cAliasSD5)->D5_NUMSEQ <= cUltSeq
				dbSkip()
				Loop
			EndIf

			// Recupera registro de Origem de trasferencia
			dbSelectArea(cAliasSD5)
			cTmSD3:=AC040TM((cAliasSD5)->D5_NUMSEQ,(cAliasSD5)->D5_ORIGLAN)
			If cTmSD3 == "DE4"
				aLoteRE4 := BuscaLoteRE4((cAliasSD5)->D5_NUMSEQ,IIf(lQuery,(cAliasSD5)->RECNOSD5,Recno()))
				If !Empty(aLoteRE4)
					//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
					//³Chamada recursiva da funcao RastroToNF para encontrar a nota de origem.     |
					//³aLoteRE4 - Array que contem os lotes de origem do processo de transferencia.|
					//³aLoteRE4[1] - Codigo do Lote                                                |
					//³aLoteRE4[2] - Codigo do SubLote                                             |
					//³aLoteRE4[3] - Codigo do Produto                                             |
					//³aLoteRE4[4] - Codigo do Armazem                                             |
					//³aLoteRE4[5] - Numero Sequencial                                             |
					//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
					aRetRE4 := RastrToNF2(aLoteRE4[1],aLoteRE4[2],aLoteRE4[3],aLoteRE4[4],IIf(lQuery,(cAliasSD5)->RECNOSD5,Recno()),aLoteRE4[5])
					// Se estiver recursivo aborta o processamento
					If lRecur
						Exit
					EndIf
					//Alimenta o array aRetorno com o resultado da recursividade
					For nX := 1 to Len(aRetRE4)
						//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
						//| aRetorno - Array contendo os itens da nota de entrada                         |
						//| aRetorno[nX,01] - Numero Documento     (D1_DOC)                               |
						//| aRetorno[nX,02] - Numero de Serie      (D1_SERIE)                             |
						//| aRetorno[nX,03] - Codigo do Fornecedor (D1_FORNECE)                           |
						//| aRetorno[nX,04] - Codigo da Loja       (D1_LOJA)                              |
						//| aRetorno[nx,05] - Codigo do Item da NF (D1_ITEM)                              |
						//| aRetorno[nX,06] - Codigo do Produto    (D1_COD)                               |
						//| aRetorno[nX,07] - Codigo do Armazem    (D1_LOCAL)                             |
						//| aRetorno[nX,08] - Codigo do Lote       (D1_LOTECTL)                           |
						//| aRetorno[nX,09] - Codigo do SubLote    (D1_NUMLOTE)                           |
						//| aRetorno[nX,10] - Numero Sequencial do item da NF (D1_NUMSEQ)                 |
						//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
						cKeyScan := aRetRE4[nX,1]+aRetRE4[nX,2]+aRetRE4[nX,3]+aRetRE4[nX,4]+aRetRE4[nX,5]
						
						If oRetorno[cKeyScan] != NIL
							nPos := oRetorno[cKeyScan]
						Else
							nPos := 0
						EndIf
						
						If nPos == 0
							aAdd(aRetorno,aRetRE4[nX])
							oRetorno[cKeyScan] := len(aRetorno)
						EndIf
					Next nX
				EndIf
			EndIf
			dbSelectArea(cAliasSD5)
			dbSkip()
		EndDo

		// Encerra area de trabalho temporaria
		If lQuery
		   (cAliasSD5)->(dbCloseArea())
		EndIf
	EndIf
EndIf

If !lRecur
	ADel(aRecur,len(aRecur))
	ASize(aRecur,len(aRecur)-1)
EndIf

RestArea(aAreaSD5)
RestArea(aArea)
Return aRetorno

/*
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	 ³BuscaLoteRE4 ³ Autor ³ TOTVS S/A   		³ Data ³ 18/11/14 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Pesquisa o Lote de Origem - Movimento RE4					  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso		 ³ RastroToNF												  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ cNumSeq - Codigo Sequencial (D3_NUMSEQ/D5_NUMSEQ)          ³±±
±±³          ³ nRecno  - Numero do Registro da tabela SD5 posicionada     ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function BuscaLoteRE4(cNumSeq,nRecno)
Local aAreaAnt:= GetArea()
Local aRetorno:= {}
Local cFilSD5 := xFilial("SD5")

Default cNumSeq := ""
Default nRecno  := 0

dbSelectArea("SD5")
dbSetOrder(3)
dbSeek(xFilial("SD5")+cNumSeq)
While !Eof() .And. cFilSD5+cNumSeq == D5_FILIAL+D5_NUMSEQ
	//-- Impede o Processamento de Movimentacoes Estornadas
	If !Empty(SD5->D5_ESTORNO)
		dbSkip()
		Loop
	EndIf
	If Recno() != nRecno
		aRetorno:={SD5->D5_LOTECTL,SD5->D5_NUMLOTE,SD5->D5_PRODUTO,SD5->D5_LOCAL,SD5->D5_NUMSEQ}
		Exit
	EndIf
	dbSkip()
End
RestArea(aAreaAnt)
Return (aRetorno)

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³ Funcao     ³ MATIsNull ³ Autor ³ Materiais            ³ Data ³ 06/11/14 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Descricao  ³ Tratamento para ISNULL no cQuery em diferentes BD's        ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Function MATIsNull()

Local cFuncNull := " "
Local cDbType   := TCGetDB()

Do Case
	Case cDbType $ "DB2|POSTGRES"
		cFuncNull	:= "COALESCE"
	Case cDbType $ "ORACLE|INFORMIX"
  		cFuncNull	:= "NVL"
 	Otherwise
 		cFuncNull	:= "ISNULL"
EndCase

Return cFuncNull

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	  ³ ISalmTerc  ³ Autor ³ TOTVS S/A   	       ³ Data ³25/11/2015³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Valida armazem de terceiros                                ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso	  ³                                                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ cMcampo   - armazem digitado pelo usuário                  ³±±
±±³          ³                                                            ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

Function ISalmTerc(cMcampo)


Local lCampo      := .F.
Local cALMTERC  := GetMvNNR('MV_ALMTERC','80')
Local aAreaNNR := {}
Default cMcampo := ""

AjustaHelp()

IF AliasInDic("NNR")
	aAreaNNR := NNR->(GetArea())
	If !empty(cMcampo)
		NNR->(dbSelectArea("NNR"))
	    NNR->(DbSetOrder(1))
		IF NNR->(DbSeek(xFilial("NNR")+cMcampo))
		    if  Alltrim(cMcampo) $ cALMTERC
				lCampo := .T.
			Else
				lcampo := .F.
				HELP(" ",1,"NOARMAZEM")
			EndIf
		else
			HELP(" ",1,"NOARMAZEM")
			lCampo := .F.
		EndIf
	else
		HELP(" ",1,"NOARMAZEM")
	EndIf
	RestArea(aAreaNNR)
Elseif  Alltrim(cMcampo) $ cALMTERC
		lCampo:= .T.
Else
	lCampo:= .F.
	HELP(" ",1,"NOARMAZEM")
EndIf

Return lCampo

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡„o    | AjustaHelp    Autor  ³ TOTVS S/A   	      ³ Data ³25/11/2015|±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡„o ³Ajusta os helps                                                ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³AjustaHelp()                                                   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³Nenhum                                                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³Nenhum                                                         ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*/
Static Function AjustaHelp()
Local aHelpPor	:= {}
Local aHelpEng	:= {}
Local aHelpSpa	:= {}


aHelpPor :=	{"Armazém não Localizado "}
aHelpSpa :=	{"Arrendamiento no encuentra"}
aHelpEng :=	{"Lease Located not"}
PutHelp("PNOARMAZEM",aHelpPor,aHelpEng,aHelpSpa,.F.)

aHelpPor :=	{"Verifique o Parâmetro "," MV_ALMTERC e o cadastro"," de armazém"}
aHelpSpa :=	{"Comprobar Parámetro", "MV_ALMTERC","y la base de almacén"}
aHelpEng :=	{"Check Parameter", "MV_ALMTERC","and warehouse base"}
PutHelp("SNOARMAZEM",aHelpPor,aHelpEng,aHelpSpa,.F.)

Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	 ³ ProcCAT83   ³ Autor ³ TOTVS S/A   		³ Data ³ 07/08/15 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Preenche o campo D3_CODLAN dos movimentos na SD3           ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso		 ³ CAT83                                                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ dDtIni   - Data inicial dos registros a processar          ³±±
±±³          ³ dDtFim   - Data final dos registros a processar            ³±±
±±³          ³ lProcAll - Processa tudo (.T.) ou so D3_CODLAN vazio (.F.) ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function PCAT83_SD3(dDtIni,dDtFim,lProcAll)

Local cQuery	:= ""
Local cWhere	:= ""
Local cProduto	:= ""
Local aArea		:= GetArea()
Local cAliTMP	:= GetNextAlias()

Default lProcAll := .F.

#IFDEF TOP
If V240CAT83() .And. FindFunction("FSCODCAT83")
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Seleciona registros para processamento       ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	cQuery := "SELECT D3_COD,D3_CF,D3_OP,D3_DOC,D3_CODLAN,R_E_C_N_O_,D3_NUMSEQ FROM " + RetSQLName("SD3")
	cWhere := " WHERE D_E_L_E_T_ = ' ' AND D3_ESTORNO = ' ' AND D3_FILIAL = '" + xFilial("SD3") + "'"
	cWhere += " AND D3_CF NOT IN ('RE9','DE9') AND D3_EMISSAO BETWEEN '" + DtoS(dDtIni) + "'"
	cWhere += " AND '" + DtoS(dDtFim) + "' " + If(lProcAll,"","AND D3_CODLAN = ' ' ")
	cQuery += cWhere

	ChangeQuery(cQuery)
	dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliTMP,.T.,.T.)

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Limpa o D3_CODLAN quando processar todos os registros ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	If lProcAll
		cQuery := "UPDATE " + RetSQLName("SD3") + " SET D3_CODLAN = ''"
		cQuery += cWhere
		TcSqlExec(cQuery)
	EndIf

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Processa os movimentos da SD3                ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	dbSelectArea("SD3")
	While (cAliTMP)->(!Eof())
		SD3->(dbGoTo((cAliTMP)->R_E_C_N_O_))
		Reclock("SD3",.F.)
		If SD3->D3_CF == "DE7"
			cProduto := PCAT83_Ori((cAliTMP)->D3_NUMSEQ)
			SD3->D3_CODLAN := A240CAT83(cProduto, .T.)
		Else
			SD3->D3_CODLAN := A240CAT83(, .T.)
		EndIf
		SD3->(MSUnlock())
		(cAliTMP)->(dbSkip())
	EndDo

	(cAliTMP)->(DbCloseArea())
EndIf
#ENDIF

RestArea(aArea)

Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	 ³ PCAT83_Ori  ³ Autor ³ TOTVS S/A   		³ Data ³ 11/01/17 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Retorna o produto de origem do movimento                   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso		 ³ CAT83                                                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ cNumSeq  - Numero sequencial do movimento                  ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function PCAT83_Ori(cNumSeq)

Local cQuery	:= ""
Local cMovOri	:= ""
Local cRet		:= ""
Local aArea		:= GetArea()
Local cAliTMP	:= GetNextAlias()

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Define o movimento de Origem                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If SD3->D3_CF == "DE7"
	cMovOri := "RE7"
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Seleciona registros para processamento       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
cQuery := "SELECT D3_COD,D3_CF FROM "+RetSQLName("SD3")+" WHERE D_E_L_E_T_ = ' ' AND "
cQuery += "D3_FILIAL = '" + xFilial("SD3") + "' AND D3_NUMSEQ = '"+ cNumSeq +"' ORDER BY 2 DESC"

ChangeQuery(cQuery)
dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliTMP,.T.,.T.)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Busca o produto de origem da desmontagem     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
While (cAliTMP)->(!Eof())
	If (cAliTMP)->D3_CF == cMovOri
		cRet := (cAliTMP)->D3_COD
		Exit
	EndIf
	(cAliTMP)->(dbSkip())
EndDo

RestArea(aArea)

Return cRet

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	 ³ PCAT83_MOD  ³ Autor ³ TOTVS S/A   		³ Data ³ 10/08/15 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Retorna movimentos de MOD e GGF do periodo                 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso		 ³ CAT83                                                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ dDtIni   - Data inicial dos registros a processar          ³±±
±±³          ³ dDtFim   - Data final dos registros a processar            ³±±
±±³          ³ cMODIni  - Codigo Inicial                                  ³±±
±±³          ³ cMODFim  - Codigo Final                                    ³±±
±±³          ³ cAliTRB  - Alias Temporario                                ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Function PCAT83_MOD(dDtIni,dDtFim,cMODIni,cMODFim,cAliTRB)

Local cQuery	:= ""
Local cSubstr	:= "SUBSTRING"
Local cDbType	:= TCGetDB()
Local aArea		:= GetArea()

#IFDEF TOP
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Tratamento para SUBSTRING em diferentes BD's ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If cDbType $ "ORACLE/POSTGRES"
	cSubstr  := "SUBSTR"
EndIf

cQuery := "SELECT SC2.C2_PRODUTO, SD3.D3_COD, "
cQuery += "SUM(CASE WHEN SD3.D3_CF LIKE ('DE%') THEN (SD3.D3_QUANT*-1) "
cQuery += "WHEN SD3.D3_CF LIKE ('RE%') THEN (SD3.D3_QUANT) ELSE 0 END) AS QUANTIDADE "
cQuery += "FROM "+ RetSQLName("SD3") +" SD3 JOIN "+ RetSQLName("SB1")
cQuery += " SB1 ON SB1.B1_FILIAL = '"+ xFilial("SB1") +"' AND "
cQuery += "SB1.B1_COD = SD3.D3_COD AND SB1.D_E_L_E_T_ = ' ' "
cQuery += "JOIN "+ RetSQLName("SC2") +" SC2 ON SC2.C2_FILIAL = '"+ xFilial("SC2") +"' AND "
cQuery += "SC2.C2_NUM||SC2.C2_ITEM||SC2.C2_SEQUEN = SD3.D3_OP AND SC2.D_E_L_E_T_ = ' ' "
cQuery += "WHERE SD3.D3_FILIAL = '"+ xFilial("SD3") +"' AND SD3.D_E_L_E_T_ = ' ' AND "
cQuery += "SD3.D3_ESTORNO = ' ' AND SD3.D3_OP <> ' ' AND "
cQuery += "SD3.D3_EMISSAO BETWEEN '"+ DtoS(dDtIni) +"' AND '"+ DtoS(dDtFim) +"' AND "
cQuery += "SD3.D3_COD BETWEEN '"+ cMODIni +"' AND '"+ cMODFim +"'  AND "
cQuery += "(SD3.D3_CF LIKE ('RE%') OR SD3.D3_CF LIKE ('DE%')) AND "
cQuery += "("+ cSubstr +"(SB1.B1_COD,1,3) = 'MOD' OR SB1.B1_CCCUSTO <> ' ') "
cQuery += "GROUP BY SC2.C2_PRODUTO, SD3.D3_COD ORDER BY SC2.C2_PRODUTO, SD3.D3_COD"

cQuery := ChangeQuery(cQuery)
dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliTRB,.T.,.T.)
#ENDIF

RestArea(aArea)

Return

// ---------------------------------------------------------------------------
/*/{Protheus.doc} RetNFOri
Realiza a montagem de um arquivo tempórario (TRB) contendo os componentes
utilizados na produção de um produto e suas respectivas Notas Fiscais de
compra, dentro do periodo informado nos parametros. A busca e montagem do
arquivo temporario é reallizada com base no FIFO.
@author robson.ribeiro
@since 01/03/2016
@version 1.0
@param dDtIni,		data,	(Data inicial para Produção)
@param dDtFim,		data,	(Data final para Produção)
@param dDtIniCom,	data,	(Data inicial para Compra)
@param dDtFimCom,	data,	(Data final para Compra)
@param aAliVenda,	array,	(Array contendo os produtos que serão processados)
@param aCpoSD1,		array,	(Campos da SD1 que devem ser retornados no TRB)
@return ${return}, ${Retorna o Alias do arquivo temporário (TRB) gerado}
/*/
// ---------------------------------------------------------------------------
Function RetNFOri(dDtIni,dDtFim,dDtIniCom,dDtFimCom,aAliVenda,aCpoSD1,lTotal,oTable)

Local aComps		:= {}
Local cOP			:= ""
Local cAliRet		:= ""
Local cAliNF
Local nCpoPad		:= 0
Local nX, nY, nZ
Private cDbType		:= TCGetDB()

Default oTable		:= Nil
Default lTotal  	:= .F.

oTable := MontTRB(@cAliRet,aCpoSD1)
If !lTotal
	For nX := 1 To Len(aAliVenda)

		cOP		:= RetLastOP(dDtIni,dDtFim,aAliVenda[nX])

		If !Empty(cOP)
			aComps	:= RetOPCom(dDtIni,dDtFim,cOP)
		EndIf

		For nY := 1 to Len(aComps)
			cAliNF := RetUltNF(aComps[nY][1],dDtIniCom,dDtFimCom,aCpoSD1)
			While !(cAliNF)->(Eof())
				Reclock(cAliRet, .T.)
				(cAliRet)->PROD_PA		:= aAliVenda[nX]
				(cAliRet)->PROD_COMP	:= aComps[nY][1]
				(cAliRet)->QUANT		:= aComps[nY][2]
				(cAliRet)->OP_PA		:= cOP
				(cAliRet)->D1_DOC		:= (cAliNF)->D1_DOC		
				(cAliRet)->D1_SERIE		:= (cAliNF)->D1_SERIE	
				(cAliRet)->D1_FORNECE	:= (cAliNF)->D1_FORNECE	
				(cAliRet)->D1_LOJA		:= (cAliNF)->D1_LOJA	
				(cAliRet)->D1_ITEM		:= (cAliNF)->D1_ITEM	
				For nZ := 1 To Len(aCpoSD1)
					(cAliRet)->(&(aCpoSD1[nZ])) := (cAliNF)->(&(aCpoSD1[nZ]))
				Next nZ
				(cAliRet)->(MsUnlock())
				(cAliNF)->(DbSkip())
			EndDo
		Next nY
	Next nX
Else
	For nX := 1 To Len(aAliVenda)

		cOP		:= RetLastOP(dDtIni,dDtFim,aAliVenda[nX])

		If !Empty(cOP)
			aComps	:= RetOPCom(dDtIni,dDtFim,cOP)
			For nY := 1 to Len(aComps)
				cAliNF := RetUltNF(aComps[nY][1],dDtIniCom,dDtFimCom,aCpoSD1)
				While !(cAliNF)->(Eof())
					Reclock(cAliRet, .T.)
					(cAliRet)->PROD_PA		:= aAliVenda[nX]
					(cAliRet)->PROD_COMP	:= aComps[nY][1]
					(cAliRet)->QUANT		:= aComps[nY][2]
					(cAliRet)->OP_PA		:= cOP
					(cAliRet)->D1_DOC		:= (cAliNF)->D1_DOC		
					(cAliRet)->D1_SERIE		:= (cAliNF)->D1_SERIE	
					(cAliRet)->D1_FORNECE	:= (cAliNF)->D1_FORNECE	
					(cAliRet)->D1_LOJA		:= (cAliNF)->D1_LOJA	
					(cAliRet)->D1_ITEM		:= (cAliNF)->D1_ITEM	
					(cAliRet)->TIPO			:= 'Produc'
					For nZ := 1 To Len(aCpoSD1)
						(cAliRet)->(&(aCpoSD1[nZ])) := (cAliNF)->(&(aCpoSD1[nZ]))
					Next nZ
					(cAliRet)->(MsUnlock())
					(cAliNF)->(DbSkip())
				EndDo
			Next nY
	 	EndIf
		If !Empty(aAliVenda)
		 	cAliNF := RetUltNF(aAliVenda[nX],dDtIniCom,dDtFimCom,aCpoSD1)
			While !(cAliNF)->(Eof())
				Reclock(cAliRet, .T.)
				(cAliRet)->PROD_PA		:= aAliVenda[nX]
				(cAliRet)->PROD_COMP	:= " "
				(cAliRet)->QUANT		:= (cAliNF)->D1_QUANT	
				(cAliRet)->OP_PA		:= " "
				(cAliRet)->D1_DOC		:= (cAliNF)->D1_DOC		
				(cAliRet)->D1_SERIE		:= (cAliNF)->D1_SERIE	
				(cAliRet)->D1_FORNECE	:= (cAliNF)->D1_FORNECE	
				(cAliRet)->D1_LOJA		:= (cAliNF)->D1_LOJA	
				(cAliRet)->D1_ITEM		:= (cAliNF)->D1_ITEM	
				(cAliRet)->TIPO			:= 'Reven'
				nCpoPad := Len(cAliNF) - Len(aCpoSD1)
				For nZ := 1 To Len(aCpoSD1)
					(cAliRet)->(&(aCpoSD1[nZ])) := (cAliNF)->(&(aCpoSD1[nZ]))
				Next nZ
				(cAliRet)->(MsUnlock())
				(cAliNF)->(DbSkip())
			EndDo
		EndIf
	Next nX	
EndIf
If !empty(cAliNF)
	(cAliNF)->(dbCloseArea())
EndIf
Return cAliRet

// ---------------------------------------------------------------------------
/*/{Protheus.doc} RetOPCom
Lista todos os componentes que foram utilizados na produção do produto acabado
da Ordem de Produção.
@author robson.ribeiro
@since 01/03/2016
@version 1.0
@param dDtIni,		data,	(Data inicial para Produção)
@param dDtFim,		data,	(Data final para Produção)
@param cOP,		character,	(Código da Ordem de Produção)
@return ${return}, ${Array contendo os componentes do PA da OP}
/*/
// ---------------------------------------------------------------------------
Static Function RetOPCom(dDtIni,dDtFim,cOP)

Local cFuncSubst	:= If(cDbType $ "ORACLE/POSTGRES","SUBSTR","SUBSTRING")
Local cCccusto		:= Criavar("B1_CCCUSTO",.F.)
Local cAliComp		:= GetNextAlias()
Local aRet			:= {}
Local aComps		:= {}
Local cQuery		:= ""
Local nRecur		:= 0
Local nQtdProd		:= 0
Local nX

cQuery := "SELECT SUM(CASE WHEN SD3.D3_CF LIKE ('DE%') THEN (SD3.D3_QUANT*-1) "
cQuery += "WHEN SD3.D3_CF LIKE ('RE%') THEN (SD3.D3_QUANT) ELSE 0 END) AS QUANTIDADE, SD3.D3_COD "
cQuery += "FROM "+ RetSqlName("SD3") +" SD3 JOIN "+ RetSqlName("SB1") +" SB1 "
cQuery += "ON SB1.B1_FILIAL = '"+ xFilial("SB1") +"' AND SB1.B1_COD = SD3.D3_COD AND SB1.D_E_L_E_T_ = ' ' "
cQuery += "WHERE SD3.D3_FILIAL = '"+ xFilial("SD3") +"' AND SD3.D_E_L_E_T_ = ' ' AND SD3.D3_ESTORNO = ' ' "
cQuery += "AND SD3.D3_OP = '"+ cOP +"' AND (SD3.D3_CF LIKE ('RE%') OR SD3.D3_CF LIKE ('DE%')) "
cQuery += "AND "+ cFuncSubst +" (SB1.B1_COD,1,3) <> 'MOD' "
cQuery += "AND SB1.B1_CCCUSTO = '"+ cCccusto +"' AND SB1.D_E_L_E_T_ = ' ' "
cQuery += "GROUP BY D3_COD ORDER BY SD3.D3_COD"

cQuery := ChangeQuery(cQuery)
dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliComp,.T.,.T.)

nQtdProd := RetQtdProd(cOP)

While !(cAliComp)->(Eof())
	If IsPI((cAliComp)->D3_COD,dDtIni,dDtFim)
		aComps := RetPICom(dDtIni,dDtFim,(cAliComp)->D3_COD,@nRecur)
		nRecur := 0

		For nX := 1 to Len(aComps)
			Aadd(aRet,{aComps[nX][1],(aComps[nX][2] * (cAliComp)->QUANTIDADE) / nQtdProd})
		Next nX
	Else
		Aadd(aRet,{(cAliComp)->D3_COD,(cAliComp)->QUANTIDADE / nQtdProd})
	EndIf
	(cAliComp)->(dbSkip())
EndDo

(cAliComp)->(dbCloseArea())

Return aRet

// ---------------------------------------------------------------------------
/*/{Protheus.doc} RetPICom
Lista todos os componentes que foram utilizados na produção do produto
intermediário. Utiliza o FIFO para a busca da OP.
@author robson.ribeiro
@since 01/03/2016
@version 1.0
@param dDtIni,		data,		(Data inicial para Produção)
@param dDtFim,		data,		(Data final para Produção)
@param cComponente,	character,	(Código do PI)
@param nRecur, 		numérico,	(Controlador de Recursividade)
@return ${return}, ${Array contendo os componentes do PI da OP}
/*/
// ---------------------------------------------------------------------------
Static Function RetPICom(dDtIni,dDtFim,cComponente,nRecur)

Local cFuncSubst	:= If(cDbType $ "ORACLE/POSTGRES","SUBSTR","SUBSTRING")
Local cCccusto		:= Criavar("B1_CCCUSTO",.F.)
Local cAliComp		:= GetNextAlias()
Local aRet			:= {}
Local aComps		:= {}
Local cQuery		:= ""
Local cOP			:= ""
Local nQtdProd		:= 0
Local nX

If nRecur < 99 // Controle de recursividade
	nRecur++
	cOP			:= RetLastOP(dDtIni,dDtFim,cComponente)
	If !Empty(cOP)
		nQtdProd	:= RetQtdProd(cOP)

		cQuery := "SELECT SUM(CASE WHEN SD3.D3_CF LIKE ('DE%') THEN (SD3.D3_QUANT*-1) "
		cQuery += "WHEN SD3.D3_CF LIKE ('RE%') THEN (SD3.D3_QUANT) ELSE 0 END) AS QUANTIDADE, SD3.D3_COD "
		cQuery += "FROM "+ RetSqlName("SD3") +" SD3 JOIN "+ RetSqlName("SB1") +" SB1 "
		cQuery += "ON SB1.B1_FILIAL = '"+ xFilial("SB1") +"' AND SB1.B1_COD = SD3.D3_COD AND SB1.D_E_L_E_T_ = ' ' "
		cQuery += "WHERE SD3.D3_FILIAL = '"+ xFilial("SD3") +"' AND SD3.D_E_L_E_T_ = ' ' AND SD3.D3_ESTORNO = ' ' "
		cQuery += "AND SD3.D3_OP = '"+ cOP +"' AND (SD3.D3_CF LIKE ('RE%') OR SD3.D3_CF LIKE ('DE%')) "
		cQuery += "AND "+ cFuncSubst +" (SB1.B1_COD,1,3) <> 'MOD' "
		cQuery += "AND SB1.B1_CCCUSTO = '"+ cCccusto +"' AND SB1.D_E_L_E_T_ = ' ' "
		cQuery += "GROUP BY D3_COD ORDER BY SD3.D3_COD"

		cQuery := ChangeQuery(cQuery)
		dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliComp,.T.,.T.)

		While !(cAliComp)->(Eof())
			If IsPI((cAliComp)->D3_COD,dDtIni,dDtFim)
				aComps := RetPICom(dDtIni,dDtFim,(cAliComp)->D3_COD,@nRecur)

				For nX := 1 to Len(aComps)
					Aadd(aRet,{aComps[nX][1],(aComps[nX][2] * (cAliComp)->QUANTIDADE) / nQtdProd})
				Next nX
			Else
				Aadd(aRet,{(cAliComp)->D3_COD,(cAliComp)->QUANTIDADE / nQtdProd})
			EndIf
			(cAliComp)->(dbSkip())
		EndDo

		(cAliComp)->(dbCloseArea())
	EndIf
	nRecur--
EndIf

Return aRet


// ---------------------------------------------------------------------------
/*/{Protheus.doc} IsPI
Verifica se o produto é um Produto Intermediário.
@author robson.ribeiro
@since 01/03/2016
@version 1.0
@param cProd,	character,	(Código do Produto)
@param dDtIni,	data,		(Data inicial para Produção)
@param dDtFim,	data,		(Data final para Produção)
@return ${return}, ${Lógico indicando se o produto é ou não um PI}
/*/
// ---------------------------------------------------------------------------
Static Function IsPI(cProd,dDtIni,dDtFim)

Local cAliasPI		:= GetNextAlias()
Local cQuery		:= ""
Local lRet			:= .T.

cQuery := "SELECT MAX(SD3.D3_EMISSAO) AS PERIODO "
cQuery += " FROM "+ RetSqlName("SD3") +" SD3 "
cQuery += "WHERE SD3.D3_FILIAL = '"+ xFilial("SD3") +"' "
cQuery += "AND SD3.D_E_L_E_T_ = ' ' AND SD3.D3_ESTORNO = ' ' "
cQuery += "AND SD3.D3_CF IN ('PR0','PR1') AND SD3.D3_COD = '"+ cProd +"' "
cQuery += "AND SD3.D3_EMISSAO <= '"+ DtoS(dDtFim) +"'"

cQuery := ChangeQuery(cQuery)
dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliasPI,.T.,.T.)

If Empty((cAliasPI)->PERIODO)
	lRet := .F.
EndIf

(cAliasPI)->(DbCloseArea())

Return lRet


// ---------------------------------------------------------------------------
/*/{Protheus.doc} RetQtdProd
Retorna a quantidade já producida de uma ordem de produção.
@author robson.ribeiro
@since 01/03/2016
@version 1.0
@param cOP,		character,	(Código da Ordem de Produção)
@return ${return}, ${Quantidade Produzida da Ordem de Produção}
/*/
// ---------------------------------------------------------------------------
Static Function RetQtdProd(cOP)

Local cFuncSubst	:= If(cDbType $ "ORACLE/POSTGRES","SUBSTR","SUBSTRING")
Local cCccusto		:= Criavar("B1_CCCUSTO",.F.)
Local cAliasPI		:= GetNextAlias()
Local cQuery		:= ""
Local nRet			:= 0

cQuery := "SELECT SUM(SD3.D3_QUANT) AS QUANTIDADE, SD3.D3_COD "
cQuery += "FROM "+ RetSqlName("SD3") +" SD3 JOIN "+ RetSqlName("SB1") +" SB1 "
cQuery += "ON SB1.B1_FILIAL = '"+ xFilial("SB1") +"' AND SB1.B1_COD = SD3.D3_COD AND SB1.D_E_L_E_T_ = ' ' "
cQuery += "WHERE SD3.D3_FILIAL = '"+ xFilial("SD3") +"' AND SD3.D_E_L_E_T_ = ' ' AND SD3.D3_ESTORNO = ' ' "
cQuery += "AND SD3.D3_OP = '"+ cOP +"' AND SD3.D3_CF IN ('PR0','PR1') "
cQuery += "AND "+ cFuncSubst +" (SB1.B1_COD,1,3) <> 'MOD' "
cQuery += "AND SB1.B1_CCCUSTO = '"+ cCccusto +"' AND SB1.D_E_L_E_T_ = ' ' "
cQuery += "GROUP BY D3_COD ORDER BY SD3.D3_COD"

cQuery := ChangeQuery(cQuery)
dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliasPI,.T.,.T.)

If !Empty((cAliasPI)->QUANTIDADE)
	nRet := (cAliasPI)->QUANTIDADE
EndIf

(cAliasPI)->(DbCloseArea())

Return nRet

// ---------------------------------------------------------------------------
/*/{Protheus.doc} RetUltNF
Processa e identifica a ultima NF de Compra dentro de um período. Utiliza o
conceito FIFO.
@author robson.ribeiro
@since 01/03/2016
@version 1.0
@param cProduto, character, (Código do Produto)
@param dDtIniCom,	data,	(Data inicial para Compra)
@param dDtFimCom,	data,	(Data final para Compra)
@param aCpoSD1,		array,	(Campos da SD1 que devem ser retornados no TRB)
@return ${return}, ${Array com informações da NF de Compra.}
/*/
// ---------------------------------------------------------------------------
Static Function RetUltNF(cProduto,dDtIniCom,dDtFimCom,aCpoSD1)

Local cAliNF		:= GetNextAlias()
Local cQuery		:= ""
Local nX

cQuery := "SELECT D1_DTDIGIT, D1_DOC, D1_SERIE, D1_ITEM, D1_FORNECE, D1_LOJA, D1_QUANT "
For nX := 1 To Len(aCpoSD1)
	cQuery += ", " + aCpoSD1[nX]
Next nX
cQuery += " FROM "+ RetSqlName("SD1") +" SD1 JOIN "
cQuery += RetSqlName("SB1") +" SB1 ON SB1.B1_FILIAL = '"+ xFilial("SB1") +"' AND "
cQuery += "SB1.B1_COD = SD1.D1_COD WHERE SD1.D_E_L_E_T_ = ' ' AND SB1.D_E_L_E_T_ = ' ' AND "
cQuery += "SD1.D1_FILIAL = '"+ xFilial("SD1") +"' AND SD1.D1_COD = '"+ cProduto +"' AND "
cQuery += "SD1.D1_DTDIGIT BETWEEN '"+ DtoS(dDtIniCom) +"' AND '"+ DtoS(dDtFimCom) +"' AND "
cQuery += "SD1.D1_TIPO <> 'D'"
cQuery += "ORDER BY 1 DESC"

cQuery := ChangeQuery(cQuery)
dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliNF,.T.,.T.)

Return cAliNF

// ---------------------------------------------------------------------------
/*/{Protheus.doc} MontTRB
Realiza a montagem de Arquivo Temporário com base nos campos que devem ser
retornados, basedo no aCpoSD1.
@author robson.ribeiro
@since 01/03/2016
@version 1.0
@param cAliasTRB, character,	(Alias do Arquivo Temporário)
@param aCpoSD1,		array,		(Campos da SD1 que devem ser retornados no TRB)
/*/
// ---------------------------------------------------------------------------
Static Function MontTRB(cAliasTRB,aCpoSD1)
Local oTable		:= NIL
Local nTamCod		:= TamSX3("B1_COD")[1]
Local aCampos		:= {}

cAliasTRB	:= GetNextAlias()

// Layout do Arquivo de Trabalho
AADD(aCampos,{"PROD_PA"		,"C",nTamCod					,0						})
AADD(aCampos,{"PROD_COMP"	,"C",nTamCod					,0						})
AADD(aCampos,{"QUANT"   		,"N",TamSX3("D3_QUANT")[1]	,TamSX3("D3_QUANT")[2]})
AADD(aCampos,{"OP_PA"   		,"C",TamSX3("D3_OP")[1]		,0						})
AADD(aCampos,{"TIPO"			,"C",6							,0					   	})
AADD(aCampos,{"D1_DOC"   	,"C",TamSX3("D1_DOC")[1]		,0						})
AADD(aCampos,{"D1_SERIE"   	,"C",TamSX3("D1_SERIE")[1]	,0						})
AADD(aCampos,{"D1_FORNECE"	,"C",TamSX3("D1_FORNECE")[1],0						})
AADD(aCampos,{"D1_LOJA"   	,"C",TamSX3("D1_LOJA")[1]	,0						})
AADD(aCampos,{"D1_ITEM"   	,"C",TamSX3("D1_ITEM")[1]	,0						})

aEval(aCpoSD1,{|x| aAdd(aCampos,{x,TamSX3(x)[3],TamSX3(x)[1],TamSX3(x)[2]})})

cAliasTRB := GetNextAlias()

oTable := FWTemporaryTable():New(cAliasTRB)
oTable:SetFields(aCampos)
oTable:AddIndex("01",{"PROD_PA","PROD_COMP"})
oTable:Create()

Return oTable

// ---------------------------------------------------------------------------
/*/{Protheus.doc} RetLastOP
Processa e identifica a ultima OP produzida dentro de um período.
@author robson.ribeiro
@since 01/03/2016
@version 1.0
@param dDtIni,		data,	(Data inicial para Produção)
@param dDtFim,		data,	(Data final para Produção)
@param cProduto, character,	(Código do produto)
@return ${return}, ${Código da Ordem de Produção}
/*/
// ---------------------------------------------------------------------------
Static Function RetLastOP(dDtIni,dDtFim,cProduto)

Local cAliOP		:= GetNextAlias()
Local cQuery		:= ""
Local cRet			:= ""

// Busca a ultima OP produzida dentro do Periodo
cQuery := "SELECT MAX(SD3.D3_EMISSAO) AS EMISSAO, MAX(SD3.D3_NUMSEQ) AS NUMSEQ, MAX(SD3.D3_OP) AS OP "
cQuery += "FROM "+ RetSqlName("SD3") +" SD3 JOIN "+ RetSqlName("SB1") +" SB1 ON SB1.B1_FILIAL = '"
cQuery += xFilial("SB1") + "' AND SB1.B1_COD = SD3.D3_COD "
cQuery += "WHERE SD3.D3_FILIAL = '"+ xFilial("SD3") +"' AND SD3.D_E_L_E_T_ = ' ' AND SD3.D3_ESTORNO = ' ' AND "
cQuery += "SD3.D3_EMISSAO BETWEEN '"+ DtoS(dDtIni) +"' AND '"+ DtoS(dDtFim) +"' AND "
cQuery += "SD3.D3_CF IN ('PR0','PR1') AND SD3.D3_COD = '"+ cProduto +"'"

cQuery := ChangeQuery(cQuery)
dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliOP,.T.,.T.)

If !Empty((cAliOP)->OP)
	cRet := (cAliOP)->OP
EndIf

(cAliOP)->(dbCloseArea())

Return cRet


// ---------------------------------------------------------------------------
/*/{Protheus.doc} EstArmTerc
Verifica se esta sendo tratado armazem de terceiros
@author Squad Entradas
@since 02/09/2020
@version 1.0
@return lret
/*/
// ---------------------------------------------------------------------------
Function EstArmTerc()

Static cMV_CUSFIL
Static cFilialAnt
Static lConTerc
Static cLocTerc

Local lRet 		:= .F.
Local lCusFil  	:= .F.
Local lCusEmp	:= .F.

If cFilialAnt == Nil .Or. cFilialAnt<>cEmpAnt+cFilAnt .Or.;
	 ( lConTerc == Nil .And. cLocTerc == Nil .And. cMV_CUSFIL == Nil)

   //recebo a empresa e filial tratada.
   cFilialAnt := cEmpAnt+cFilAnt

   //Recebo os parâmetros
   lConTerc := SuperGetMv("MV_CONTERC",.F.,.F.)
   cLocTerc := GetMvNNR('MV_ALMTERC','80')
   cMV_CUSFIL := SuperGetMV('MV_CUSFIL',.F.,"A")
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica de armzem de terceiro ativo ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If lConTerc
	If !Empty(cLocTerc)
		lCusFil := AllTrim(cMV_CUSFIL ) == "F"
		lCusEmp := AllTrim(cMV_CUSFIL ) == "E"
		If ( lCusFil .Or. lCusEmp )
			lRet := .T.
		EndIf
	EndIf
EndIF

Return lRet
