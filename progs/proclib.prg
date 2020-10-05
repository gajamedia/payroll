* --------------------------------------------------------- *
FUNCTION akhirbln
* --------------------------------------------------------- *
	PARAMETER tgl
	DO CASE
		CASE MONTH(tgl)=1
			m.akhbln = 31
		CASE MONTH(tgl)=2
			IF MOD(YEAR(tgl),4)=0
				m.akhbln = 29
			ELSE
				m.akhbln = 28
			ENDIF
		CASE MONTH(tgl)=3
			m.akhbln = 31
		CASE MONTH(tgl)=4
			m.akhbln = 30
		CASE MONTH(tgl)=5
			m.akhbln = 31
		CASE MONTH(tgl)=6
			m.akhbln = 30
		CASE MONTH(tgl)=7
			m.akhbln = 31
		CASE MONTH(tgl)=8
			m.akhbln = 31
		CASE MONTH(tgl)=9
			m.akhbln = 30
		CASE MONTH(tgl)=10
			m.akhbln = 31
		CASE MONTH(tgl)=11
			m.akhbln = 30
		CASE MONTH(tgl)=12
			m.akhbln = 31
	ENDCASE
	RETURN m.akhbln

Function transbln
	para xbln
	do case
		case xbln = 1
			cbln = 'Januari'
		case xbln = 2
			cbln = 'Februari'
		case xbln = 3
			cbln = 'Maret'
		case xbln = 4
			cbln = 'April'
		case xbln = 5
			cbln = 'Mei'
		case xbln = 6
			cbln = 'Juni'
		case xbln = 7
			cbln = 'Juli'
		case xbln = 8
			cbln = 'Agustus'
		case xbln = 9
			cbln = 'September'
		case xbln = 10
			cbln = 'Oktober'
		case xbln = 11
			cbln = 'November'
		case xbln = 12
			cbln = 'Desember'
		otherwise
			cbln = 'No Date'
	endcase
Return cbln

*----------------------------------------------------------------------
*      Function : STRBULAN
*----------------------------------------------------------------------
FUNCTION strbulan
	PARAMETER trmtgl
	sbulan="Januari  Februari Maret    April    Mei      Juni     "+;
		"Juli     Agustus  SeptemberOktober  Nopember Desember "
	RETURN SUBSTR(sbulan,(MONTH(trmtgl)*9)-8,9)

* ------------------------------------------------------
* Fungsi TERBILANG - mengkonversi nilai uang ke kalimat
* ------------------------------------------------------
FUNCTION terbilang
	PARAMETER NUM     	 && 1234567890123456
	numcar 	= TRAN(NUM,"9999999999999.99")
	Poin    = VAL(SUBS(numcar,15,2))
	ratus	= VAL(SUBS(numcar,11,3))
	ribu 	= VAL(SUBS(numcar,8,3))
	juta  	= VAL(SUBS(numcar,5,3))
	milyar 	= VAL(SUBS(numcar,2,3))
	trilyun = VAL(SUBS(numcar,1,1))
	hasil = ""
	kata  = ""

	IF ratus <> 0
		hasil = KONVERSI(ratus)
	ENDIF

	IF ribu <> 0
		IF ribu <> 1
			hasil = KONVERSI(ribu) +" RIBU " +hasil
		ELSE
			hasil = "SERIBU "+ hasil
		ENDIF
	ENDIF

	IF juta <> 0
		hasil = KONVERSI(juta) +" JUTA " +hasil
	ENDIF

	IF milyar <> 0
		hasil = KONVERSI(milyar)+" MILYAR " +hasil
	ENDIF

	IF trilyun <> 0
		hasil = KONVERSI(trilyun)+" TRILYUN " +hasil
	ENDIF

	IF poin <> 0
		hasil = hasil + " RUPIAH "+KONPOIN(poin)+" SEN"
	ELSE
		hasil = hasil + " RUPIAH"
	ENDIF
	RETURN hasil

PROCEDURE KONVERSI
	PARAMETER X
	DIME H(10)
	H[1] = "SATU"
	H[2] = "DUA"
	H[3] = "TIGA"
	H[4] = "EMPAT"
	H[5] = "LIMA"
	H[6] = "ENAM"
	H[7] = "TUJUH"
	H[8] = "DELAPAN"
	H[9] = "SEMBILAN"
	H[10] = ""
	HURUF= TRAN(X,"999")
	D1 = VAL(SUBS(HURUF,1,1))
	D2 = VAL(SUBS(HURUF,2,1))
	D3 = VAL(SUBS(HURUF,3,1))

	IF D1 = 0
		D1 = 10
	ENDIF

	IF D2 = 0
		D2 = 10
	ENDIF

	IF D3 = 0
		D3 = 10
	ENDIF

	kata = ""

*--------- PULUHAN
	IF D2 = 1
		IF D3 = 10
			kata = "SEPULUH"
		ELSE
			IF D3 = 1
				kata = "SEBELAS"
			ELSE
				kata =  H[D3]+"BELAS"
			ENDIF
		ENDIF
	ELSE
		IF D2 = 10
			kata = H[D3]
		ELSE
			kata = H[D2] + " PULUH "+H[D3]
		ENDIF
	ENDIF

*------------- RATUSAN
	IF D1 = 1
		kata = "SERATUS "+kata
	ELSE
		IF D1 <> 10
			kata = H[D1] + " RATUS "+kata
		ENDIF
	ENDIF
	RETURN kata

PROCEDURE KONPOIN
	PARAMETER X
	DIME H(10)
	H[1] = "SATU"
	H[2] = "DUA"
	H[3] = "TIGA"
	H[4] = "EMPAT"
	H[5] = "LIMA"
	H[6] = "ENAM"
	H[7] = "TUJUH"
	H[8] = "DELAPAN"
	H[9] = "SEMBILAN"
	H[10] = ""
	HURUF= TRAN(X,"99")

	D1 = VAL(SUBS(HURUF,1,1))
	D2 = VAL(SUBS(HURUF,2,1))

	IF D1 = 0
		D1 = 10
	ENDIF

	IF D2 = 0
		D2 = 10
	ENDIF

	kata = ""

*--------- PULUHAN
	IF D1 = 1
		IF D2 = 10
			kata = "SEPULUH"
		ELSE
			IF D2 = 1
				kata = "SEBELAS"
			ELSE
				kata =  H[D2]+"BELAS"
			ENDIF
		ENDIF
	ELSE
		IF D1 = 10
			kata = H[D2]
		ELSE
			kata = H[D1] + " PULUH "+H[D2]
		ENDIF
	ENDIF
	RETURN kata

* --------------------------------------------------------------------
* Fungsi COUNTABLE - mengkonversi nilai uang ke kalimat (Bhs. Inggris)
* --------------------------------------------------------------------
FUNCTION countable
	PARAMETER NUM     &&1234567890123456
	numcar 	= TRAN(NUM,"9999999999999.99")
	Poin    = VAL(SUBS(numcar,15,2))
	ratus	= VAL(SUBS(numcar,11,3))
	ribu 	= VAL(SUBS(numcar,8,3))
	juta  	= VAL(SUBS(numcar,5,3))
	milyar 	= VAL(SUBS(numcar,2,3))
	trilyun = VAL(SUBS(numcar,1,1))
	hasil = ""
	kata  = ""

	IF ratus <> 0
		hasil = KONVERT(ratus)
	ENDIF

	IF ribu <> 0
		hasil = KONVERT(ribu) +" THOUSAND " +hasil
	ENDIF

	IF juta <> 0
		hasil = KONVERT(juta) +" MILLION " +hasil
	ENDIF

	IF milyar <> 0
		hasil = KONVERT(milyar)+" BILLION " +hasil
	ENDIF

	IF trilyun <> 0
		hasil = KONVERT(trilyun)+" TRILLION " +hasil
	ENDIF

	IF POIN <> 0
		hasil = hasil + " US $ "+POINT(POIN)+" CENT"
	ELSE
		hasil = hasil + " US $"
	ENDIF
	RETURN hasil

PROCEDURE KONVERT
	PARAMETER X
	DIME H(10)
	H[1] = "ONE"
	H[2] = "TWO"
	H[3] = "THREE"
	H[4] = "FOUR"
	H[5] = "FIVE"
	H[6] = "SIX"
	H[7] = "SEVEN"
	H[8] = "EIGHT"
	H[9] = "NINE"
	H[10] = ""
	HURUF= TRAN(X,"999")
	D1 = VAL(SUBS(HURUF,1,1))
	D2 = VAL(SUBS(HURUF,2,1))
	D3 = VAL(SUBS(HURUF,3,1))

	IF D1 = 0
		D1 = 10
	ENDIF

	IF D2 = 0
		D2 = 10
	ENDIF

	IF D3 = 0
		D3 = 10
	ENDIF

	kata = ""

* ---------------------
* 		PULUHAN
* ---------------------
	DO CASE
		CASE D2 = 10
			kata = H[D3]

		CASE D2 = 1
			DO CASE
				CASE D3 = 10
					kata = "TEN"
				CASE D3 = 1
					kata = "ELEVEN"
				CASE D3 = 2
					kata = 'TWELVE'
				CASE D3 = 3
					kata = 'THIRTEEN'
				CASE D3 = 5
					kata = 'FIFTEEN'
				OTHERWISE
					kata =  H[D3]+"TEEN"
			ENDCASE

		CASE D2 = 2
			DO CASE
				CASE D3 = 10
					kata = 'TWENTY'
				OTHERWISE
					kata = 'TWENTY '+ H[D3]
			ENDCASE

		CASE D2 = 3
			DO CASE
				CASE D3 = 10
					kata = 'THIRTY'
				OTHERWISE
					kata = 'THIRTY '+ H[D3]
			ENDCASE

		CASE D2 = 4
			DO CASE
				CASE D3 = 10
					kata = 'FORTY'
				OTHERWISE
					kata = 'FORTY '+ H[D3]
			ENDCASE

		CASE D2 = 5
			DO CASE
				CASE D3 = 10
					kata = 'FIFTY'
				OTHERWISE
					kata = 'FIFTY '+ H[D3]
			ENDCASE

		CASE D2 = 8
			DO CASE
				CASE D3 = 10
					kata = 'EIGHTY'
				OTHERWISE
					kata = 'EIGHTY '+ H[D3]
			ENDCASE

		OTHERWISE
			DO CASE
				CASE D3 = 10
					kata = H[D2] + "TY"
				OTHERWISE
					kata = H[D2] + "TY "+H[D3]
			ENDCASE

	ENDCASE

*------------- RATUSAN
	IF D1 <> 10
		kata = H[D1] + " HUNDRED "+kata
	ENDIF
	RETURN kata

PROCEDURE point
	PARAMETER X
	DIME H(10)
	H[1] = "ONE"
	H[2] = "TWO"
	H[3] = "THREE"
	H[4] = "FOUR"
	H[5] = "FIVE"
	H[6] = "SIX"
	H[7] = "SEVEN"
	H[8] = "EIGHT"
	H[9] = "NINE"
	H[10] = ""
	HURUF= TRAN(X,"99")

	D1 = VAL(SUBS(HURUF,1,1))
	D2 = VAL(SUBS(HURUF,2,1))

	IF D1 = 0
		D1 = 10
	ENDIF

	IF D2 = 0
		D2 = 10
	ENDIF

	kata = ""

*--------- PULUHAN
	DO CASE
		CASE D1 = 10
			kata = H[D2]

		CASE D1 = 1
			DO CASE
				CASE D2 = 10
					kata = "TEN"
				CASE D2 = 1
					kata = "ELEVEN"
				CASE D2 = 2
					kata = 'TWELVE'
				CASE D2 = 3
					kata = 'THIRTEEN'
				CASE D2 = 5
					kata = 'FIFTEEN'
				OTHERWISE
					kata =  H[D2]+"TEEN"
			ENDCASE

		CASE D1 = 2
			DO CASE
				CASE D2 = 10
					kata = 'TWENTY'
				OTHERWISE
					kata = 'TWENTY '+ H[D2]
			ENDCASE

		CASE D1 = 3
			DO CASE
				CASE D2 = 10
					kata = 'THIRTY'
				OTHERWISE
					kata = 'THIRTY '+ H[D2]
			ENDCASE

		CASE D1 = 4
			DO CASE
				CASE D2 = 10
					kata = 'FORTY'
				OTHERWISE
					kata = 'FORTY '+ H[D2]
			ENDCASE

		CASE D1 = 5
			DO CASE
				CASE D2 = 10
					kata = 'FIFTY'
				OTHERWISE
					kata = 'FIFTY '+ H[D2]
			ENDCASE

		CASE D1 = 8
			DO CASE
				CASE D2 = 10
					kata = 'EIGHTY'
				OTHERWISE
					kata = 'EIGHTY '+ H[D2]
			ENDCASE

		OTHERWISE
			DO CASE
				CASE D2 = 10
					kata = H[D1] + "TY"
				OTHERWISE
					kata = H[D1] + "TY "+H[D2]
			ENDCASE

	ENDCASE
	RETURN kata

* --------------------------------------------------------------- *
* STRHARI - Returns Indonesian Day
* --------------------------------------------------------------- *
FUNCTION strhari
	PARAMETER dday
	PRIVATE daystr
	daystr = "MingguSenin SelasaRabu  Kamis Jum'atSabtu "
	RETURN SUBSTR(daystr,6*DOW(dday)-5,6)

* --------------------------------------------------------------- *
* Fungsi    : ENCRYPT(<C1>,<N1>)
* Paramater : C1 = character yang akan di-encrypt/di-decrypt
*			  N1 = Mode ( 1 = Encrypt , 2 = Decrypt )
* Tanggal   : 1 Oktober 1996
* --------------------------------------------------------------- *
FUNCTION encrypt
	PARAMETER cpass, mode
	PRIVATE cencrypt
	cencrypt = ''

	DO CASE
		CASE mode = 1		&& Encrypt
			FOR i = 1 TO LEN(cpass)
				N   = TRAN(i,'@L 99')
				H&n = CHR(ASC(SUBSTR(cpass,i,1))+80)
			ENDFOR

			FOR j = 1 TO LEN(cpass)
				N   = TRAN(j,'@L 99')
				cencrypt = cencrypt + H&n
			ENDFOR

		CASE mode = 2		&& Decrypt
			FOR i = 1 TO LEN(cpass)
				N   = TRAN(i,'@L 99')
				H&n = CHR(ASC(SUBSTR(cpass,i,1))-80)
			ENDFOR

			FOR j = 1 TO LEN(cpass)
				N   = TRAN(j,'@L 99')
				cencrypt = cencrypt + H&n
			ENDFOR
	ENDCASE
	RETURN cencrypt

* --------------------------------------------------------- *
FUNCTION LONGDATE		&& Mengembalikan tanggal dengan
&& format dd bulan yyyy
* --------------------------------------------------------- *
	PARAMETER ddate
	RETURN ALLT(TRAN(DAY(ddate),'99'))+' '+ALLT(strbulan(ddate))+' '+TRAN(YEAR(ddate),'9999')

*----------------------------------------------------------------*
*      Procedure: DOALERT
*----------------------------------------------------------------*
PROCEDURE doalert
	PARAMETER MESS
	IF EMPTY(MESS)
		WAIT+'You must pass a message.' WINDOW
		RETURN
	ENDIF
	PRIVATE choice
	STORE 0 TO choice
DEFINE WINDOW alert FROM 8,10 TO 14,70 DOUBLE COLOR SCHEME 7
Activate WINDOW alert
@ 2, ((60-LEN(MESS))/2)  SAY MESS
@ 4,25 GET choice FUNCTION '*H  \?\<Batal'
READ MODAL
Release WINDOW alert
RETURN choice

*----------------------------------------------------------*
*  beep(<expN>)                                            *
*                                                          *
*  Note: Plays the songs from the Xbase Hit Parade.        *
*                                                          *
*  song      1 = error                                     *
*            2 = danger                                    *
*            3 = hello                                     *
*            4 = death                                     *
*            5 = slide up                                  *
*            6 = slide down                                *
*            7 = TULALIT                                   *
*----------------------------------------------------------*
FUNCTION beep
	PARAMETERS song
	PRIVATE song, i

	IF type('song') # 'N'       &&  if bizzare thing passed
		song = 0
	ENDIF

	DO CASE
		CASE song = 1
			SET BELL TO 800,1
			?? chr(7)
			= inkey(.1)
			?? chr(7)
		CASE song = 2
			SET BELL TO 523.3,1
			?? chr(7)
			= inkey(.1)
			?? chr(7)
			= inkey(.1)
			?? chr(7)
			= inkey(.1)
			?? chr(7)
			= inkey(.1)
		CASE song = 3
			SET BELL TO 261,3
			?? chr(7)
			SET BELL TO 349,4
			?? chr(7)
			SET BELL TO 440,1
			?? chr(7)
			SET BELL TO 349,1
			?? chr(7)
			SET BELL TO 261,3
			?? chr(7)
			SET BELL TO 349,4
			?? chr(7)
			SET BELL TO 440,8
			?? chr(7)
			SET BELL TO 349,6
			?? chr(7)
		CASE song = 4
			SET BELL TO 261,8
			?? chr(7)
			SET BELL TO 261,7
			?? chr(7)
			SET BELL TO 261,2
			?? chr(7)
			SET BELL TO 261,8
			?? chr(7)
			SET BELL TO 311,7
			?? chr(7)
			SET BELL TO 293,2
			?? chr(7)
			SET BELL TO 293,7
			?? chr(7)
			SET BELL TO 261,2
			?? chr(7)
			SET BELL TO 261,7
			?? chr(7)
			SET BELL TO 247,2
			?? chr(7)
			SET BELL TO 261,8
			?? chr(7)
		CASE song = 5
			FOR i = 400 to 1800 step 100
				SET BELL TO i,1
				?? chr(7)
			ENDFOR
		CASE song = 6
			FOR i = 1800 to 400 step -100
				SET BELL TO i,1
				?? chr(7)
			ENDFOR
		CASE song = 7
			SET bell to 500,1
			?? chr(7)
			SET bell to 1000,1
			?? chr(7)
			SET bell to 1500,1
			?? chr(7)
			SET bell to 2000,1
			?? chr(7)
			SET bell to 2500,1
			?? chr(7)
			SET bell to 3000,1
			?? chr(7)
			SET bell to 3500,1
			?? chr(7)
			SET bell to 4000,1
			?? chr(7)
		OTHERWISE
			SET BELL TO 440,6
			?? chr(7)
	ENDCASE

	RETURN(.t.)

*----------------------------------------------------------*
*  dblock(<expC>)                                          *
*                                                          *
*  Locks the named data table by changing the first byte   *
*  of the header.  Returns TRUE if successful, FALSE if    *
*  the program failed for any reason.                      *
*----------------------------------------------------------*
FUNCTION dblock
	PARAMETERS f_name
	PRIVATE f_name, f_alias, f_handle, byte1

	IF at('.',f_name) = 0         &&  no extension passed
		f_alias = f_name
		f_name = f_name + '.dbf'
	ELSE
		f_alias = substr(f_name,1,at('.',f_name)-1)
	ENDIF

	IF used(f_alias)
		= beep(7)
		= doalert('Cannot lock an open data table.')
		RETURN(.f.)
	ENDIF

	f_handle = fopen(f_name,2)    &&  open the data table

	IF f_handle = -1              &&  not successful
		= beep(7)
		= doalert('Unsuccessful in opening file.')
		RETURN(.f.)
	ENDIF

	byte1 = fread(f_handle,1)           && ambil byte pertama
	= fseek(f_handle,0)                 && kembali ke byte pertama
	DO CASE
		CASE byte1=CHR(3)                && jika = 03h (tanpa memo)
			= fwrite(f_handle,chr(78))  && tulis 'N' ke byte pertama
		CASE byte1=CHR(245)              && ada memo
			= fwrite(f_handle,chr(77))  && tulis 'M' ke byte pertama
	ENDCASE
	= fclose(f_handle)             && tutup file

	RETURN(.t.)

*----------------------------------------------------------*
*  dbunlock(<expC>)                                        *
*                                                          *
*  Unlocks the named data table by changing the first byte *
*  of the header.  Returns TRUE if successful, FALSE if    *
*  the program failed for any reason.                      *
*----------------------------------------------------------*
FUNCTION dbunlock
	PARAMETERS f_name
	PRIVATE f_name, f_alias, f_handle, byte1

	IF at('.',f_name) = 0               && tanpa extension
		f_alias = f_name
		f_name = f_name + '.dbf'
	ELSE
		f_alias = substr(f_name,1,at('.',f_name)-1)
	ENDIF

	f_handle = fopen(f_name,2)          && buka database

	IF f_handle = -1                    && tidak sukses ?
		= beep(7)
		= doalert('Unsuccessful in opening file.')
		RETURN(.f.)
	ENDIF

	byte1 = fread(f_handle,1)           && ambil byte pertama
	= fseek(f_handle,0)                 && kembali ke byte pertama
	DO CASE
		CASE byte1=CHR(78)               && jika = 'N' (tanpa memo)
			= fwrite(f_handle,chr(3))   && tulis CHR(3) ke byte pertama
		CASE byte1=CHR(77)               && jika = 'M' (dengan memo)
			= fwrite(f_handle,chr(245)) && tulis CHR(245) ke byte pertama
	ENDCASE
	= fclose(f_handle)                  && tutup file

	RETURN(.t.)

proc ChgPass
para cPass, cLev
	cWord = ''
	cLength = len(allt(cPass))
	cMidle = int(cLength/2)
	cMod = mod(cLength,2)
	do case
		case cLev=1
			for i = 1 to cLength
				do case
					case i=cMidle+1 and cMod=1
						cWord = cWord+chr(asc(subs(cPass,i,1))+70)
					case i<=cMidle
						cWord = cWord+chr(asc(subs(cPass,i,1))+asc(subs(cPass,i+cMidle,1)))
					case i>cMidle
						cWord = cWord+chr(asc(subs(cPass,i,1))+i)
				endc
			endf
		case cLev=2
			for i = 1 to cLength
				do case
					case i=cMidle+1 and cMod=1
						cWord = cWord+chr(asc(subs(cPass,i,1))-70)
					case i<=cMidle
						if cMod=0
							cVal = (i+cMidle+cMod)
						else
							if i>1
								cVal = (i+cMidle)
							else
								cVal = 70
							endi
						endi
						cWord = cWord+chr(asc(subs(cPass,i,1))-(asc(subs(cPass,i+cMidle,1))-cVal))
					case i>cMidle
						cWord = cWord+chr(asc(subs(cPass,i,1))-i)
				endc
			endf

	endc
retu cWord


proc ChgReg
para cPass, cLev
	cWord = ''
	cLength = len(allt(cPass))
	cMidle = int(cLength/2)
	cMod = mod(cLength,2)
	do case
		case cLev=1
			for i = 1 to cLength
				do case
					case i=cMidle+1 and cMod=1
						cWord = cWord+chr(asc(subs(cPass,i,1))+10)
					case i<=cMidle
						cWord = cWord+chr(asc(subs(cPass,i,1))+asc(subs(cPass,i+cMidle,1)))
					case i>cMidle
						cWord = cWord+chr(asc(subs(cPass,i,1))+i)
				endc
			endf
		case cLev=2
			for i = 1 to cLength
				do case
					case i=cMidle+1 and cMod=1
						cWord = cWord+chr(asc(subs(cPass,i,1))-10)
					case i<=cMidle
						if cMod=0
							cVal = (i+cMidle+cMod)
						else
							if i>1
								cVal = (i+cMidle)
							else
								cVal = 10
							endi
						endi
						cWord = cWord+chr(asc(subs(cPass,i,1))-(asc(subs(cPass,i+cMidle,1))-cVal))
					case i>cMidle
						cWord = cWord+chr(asc(subs(cPass,i,1))-i)
				endc
			endf

	endc
retu cWord


function strromawi
lparameters xnum
do case
	case xnum=1
		retu 'I'
	case xnum=2
		retu 'II'
	case xnum=3
		retu 'III'
	case xnum=4
		retu 'IV'
	case xnum=5
		retu 'V'
	case xnum=6
		retu 'VI'
	case xnum=7
		retu 'VII'
	case xnum=8
		retu 'VIII'
	case xnum=9
		retu 'IX'
	case xnum=10
		retu 'X'
	case xnum=11
		retu 'XI'
	case xnum=12
		retu 'XII'
	case xnum=13
		retu 'XIII'
	case xnum=14
		retu 'XIV'
	case xnum=15
		retu 'XV'
endc


procedure findmk
	valmk=0
	for i = 1 to 12
		nmxxxx='vgj'+trans(i,'@kzl 99')+'.gtetap'
		if &nmxxxx > 0
			valmk=valmk+1
		endif
	endfor
return valmk


procedure datachange
if isnull(getfldstate(-1))
	retu .f.
else
	retu ("2" $ getfldstate(-1) or ;
          "4" $ getfldstate(-1))
endi


procedure adddtl
PARAMETERS xtable
 appe blan in &xtable
 =TABLEUPDATE(.T.)


procedure deldtl
PARAMETERS xfile
SELECT &xfile
	if messagebox('Anda yakin akan '+iif(dele(),'memanggil kembali','menghapus')+' record detail tersebut ?',4+32+256,'Detail Transaksi')=6
		if delete()
			reca
		ELSE
*			REPLACE Usertime With Dtos(Date())+Time()+ChgPass(MainUserName,2)
			dele
			if !bof()
				skip -1
			endi
		endi
	endi

FUNCTION Validasi
PARAMETERS fsource,ftarget



FUNCTION trustee
* ----- Disable menu untuk level user -----*
FOR j = 1 TO 3
	DO case
		CASE j=1
			xmenu = 'File'
			xcount = 11
		CASE j=2
			xmenu = 'Transaksi'
			xcount = 2
		CASE j=3
			xmenu = 'Setup'
			xcount = 10
	ENDCASE
	
	FOR i = 1 TO xcount
		xbar = allt(STR(i))
		IF !EMPTY(PRMBAR(xmenu,i))
			xprompt = '\'+PRMBAR(xmenu,i)
			DEFINE BAR &xbar OF &xmenu PROMPT (xprompt)
		ENDIF
	NEXT
NEXT

* ----- Hak Akses -----*
SELECT 0
USE reports ORDER contents
REPLACE ALL show WITH .f.
SELECT 0
USE rights order userid NOUPDATE
GO TOP
if seek(MainUserName)
	DO WHILE userid=MainUserName AND !EOF()
		IF chgpass(menu,2)='r'				&& Hak Akses : Laporan			
			IF SEEK(prompt,'reports')
				REPLACE show WITH .t. in reports
			ENDIF
		ELSE								&& Hak Akses : selain Laporan
			xbar = ALLTRIM(STR(bar))
			xmenu = ALLTRIM(chgpass(menu,2))
			xprompt = ALLTRIM(prompt)
			DEFINE BAR &xbar OF &xmenu PROMPT (xprompt)
		ENDIF
		SELECT RIGHTS
		skip
	endd
ENDIF
USE IN rights
USE IN reports


PROCEDURE chgpart
PARAMETERS cKode, cDesc
	cWord = ''
	DO WHILE .t.
		cPos = AT('-',cDesc)
		IF cpos>0
			cWord = cWord + SUBSTR(cKode,1,cPos-1)
			cKode = SUBSTR(cKode,cPos,LEN(cKode))
			cDesc = SUBSTR(cDesc,cPos+1,LEN(cDesc))
			IF !EMPTY(cKode)
				cWord = cWord + '-'
			ENDIF
		ELSE
			cWord = cWord + cKode
			RETURN cWord
		ENDIF
	ENDDO


FUNCTION CodePsw
   PARA cx,cy,s,warna
   PRIVATE ktest,hsl
   SET CURS OFF
   ktest = ''
   hsl = 0
   @cx,cy SAY REPL(' ',s) COLO &warna
   DO WHILE .T.
      zkey = 0
      DO WHILE zkey=0
         zkey = INKEY(0,'H')
      ENDD   
      DO CASE
         CASE (zkey==27)
              SET CURS ON
              ktest = .F.
              EXIT
         CASE (zkey==127) OR (zkey==19)
              IF hsl > 0
                   hsl = hsl-1
                   ktest = SUBS(ktest,1,hsl)
                 ELSE
                   =BELL()
                   LOOP
              ENDI     
         CASE (zkey==13)
              EXIT      
         CASE (hsl==s)
              =BELL()
              LOOP
         OTHE
              ktest = ktest+CHR(zkey)
              hsl = hsl+1     
      ENDC
      @cX,cY SAY REPL(' ',s)   COLO &warna
      @cX,cY SAY REPL('þ',hsl) COLO &warna
   ENDD        
RETURN ktest

FUNCTION bell
   ?? CHR(7)


func lmonth
para bulan, tahun
do case
case ctod('31/'+str(bulan)+'/'+str(tahun))<>ctod('  /  /  ')
   last = 31
case ctod('30/'+str(bulan)+'/'+str(tahun))<>ctod('  /  /  ')
   last = 30
case ctod('29/'+str(bulan)+'/'+str(tahun))<>ctod('  /  /  ')
   last = 29
case ctod('28/'+str(bulan)+'/'+str(tahun))<>ctod('  /  /  ')
   last = 28
endc
retu last

function cbulan
parameter bulan
*   123456123456123456123456123456123456123456
private hr
bln=[JanFebMarAprMayJunJulAugSepOctNovDec]
return alltrim(subs(bln,-2+(3*bulan),3))