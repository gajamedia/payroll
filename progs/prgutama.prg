DO startup

PUBLIC keluar,UserKey,usericon,userback,userfirst
UserKey=SPACE(51)
STORE "" TO keluar
usericon = 'key04.ico'
userback = ''
userfirst = .T.

DO FORM login TO keluar

IF EMPTY(keluar)
 DO cleanup
 QUIT
ENDIF

DO mnutama.mpr

READ EVENTS

*----------------------------------------------------------------*
*      Procedure: STARTUP
*----------------------------------------------------------------*
PROCEDURE startup
 SET DEFAULT TO SYS(5) &&'E:\My Project\FoxPro\Payroll2\'
 SET PATH TO forms;data;progs;mnu;images;reports
 *OPEN DATABASE 'dbpayroll.dbc' EXCLUSIVE 
 
 SET TALK OFF 
 SET CONSOLE OFF 
 SET DATE BRITISH
 SET HOURS TO 24
 SET DELETED ON
 SET CENTURY ON
 SET ECHO OFF
 SET EXACT ON  
 SET STICKY ON
 SET ESCAPE OFF
 SET SAFE OFF
 SET STATUS BAR OFF
 SET MESSAGE TO 24 CENTER
 SET CURSOR ON
 SET UDFPARMS TO VALUE
 SET EXCLUSIVE OFF
 SET REPROCESS TO 30
 SET MULTILOCKS ON
 SET REFRESH TO 5 
 SET BELL ON 
 SET OPTIMIZE ON
 
 CLEAR ALL
 CLOSE ALL
 
 SET PROCEDURE TO proclib.prg

 _screen.Caption='Sistem Informasi Personalia - PT. MULTI ARTHAMAS GLASS INDUSTRY'
 _screen.WindowState= 2
 _screen.MaxButton=.F.
 _screen.MinButton=.T.
 _screen.Icon='factory.ico' 
 _screen.Picture='WINDOWS XP.JPG'

 PUSH MENU _MSYSMENU
 ON ERROR ErrorHandler(ERROR(),PROGRAM(),LINENO())
 ON SHUTDOWN DO onShutDown
RETURN

*----------------------------------------------------------------*
*      Procedure: CLEANUP
*----------------------------------------------------------------*
PROCEDURE cleanup
	POP MENU _MSYSMENU
	_SCREEN.CAPTION="Microsoft Visual FoxPro"
	_SCREEN.PICTURE=""
	CLEAR READ ALL
	CLEAR PROGRAM
	CLEAR ALL
	CLOSE ALL
	CLEAR
	SET COLO TO
	SET NEAR OFF
	ON KEY
	ON ERROR
	RESTORE MACROS
	SET ESCAPE ON
	SET TALK ON
	ON SHUTDOWN
	SET PATH TO
	RETURN TO MASTER

*----------------------------------------------------------------*
*      Function: ErrorHandler
*----------------------------------------------------------------*
FUNCTION ErrorHandler(nError,cMethod,nLine)
	LOCAL lcErrorMsg,lcCodeLineMsg,line1

	WAIT CLEAR
	lcErrorMsg=MESSAGE()+CHR(13)+CHR(13)
	line1=lcErrorMsg
	lcErrorMsg=lcErrorMsg+"Method:    "+cMethod
	lcCodeLineMsg=MESSAGE(1)
	IF BETWEEN(nLine,1,10000) AND NOT lcCodeLineMsg="..."
		lcErrorMsg=lcErrorMsg+CHR(13)+"Line:         "+ALLTRIM(STR(nLine))
		IF NOT EMPTY(lcCodeLineMsg)
			lcErrorMsg=lcErrorMsg+CHR(13)+CHR(13)+lcCodeLineMsg
		ENDIF
	ENDIF
	IF nError = 1884
		=MESSAGEBOX(line1+'Data sudah ada, ganti dengan nomor yang lain atau click tombol batal untuk membatalkan...',0+64,_screen.activeform.caption)
		RETURN TO MASTER
	ENDI
	IF MESSAGEBOX(lcErrorMsg,17,_SCREEN.CAPTION)#1
		ON ERROR
		RETURN .F.
	ENDIF
ENDFUNC

PROCEDURE onShutDown
	IF MESSAGEBOX("Apakah anda yakin ingin keluar program ?",36+256,_screen.caption)=6
		IF !USED('USERLIST')
			SELECT 0
			USE Userlist
		ELSE
			SELECT USERLIST
		ENDIF		
		DELETE FOR Key_ = UserKey		
		USE
		QUIT
	ENDIF
ENDPROC