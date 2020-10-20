!*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!*                                                              %
!*                        YALUK-ATP CODE                        %
!*                                                              %
!*   - INIT CODE												%
!      Condiciones de Frontera se realiza con una linea tipo
!		sin pérdidas con inducción electromagnética
!		se modifica la distancia de la línea para incluir un dx	
!		que es igual a v*dt a cada extremo de la línea
!		Grabar un solo archivo con todo los campos 
!		y mantenerlo en memoria sin leer archivo
!		Calcula el tiempo de inicio
!*   - Link with FOREIGN MODELS (Subroutine)                    %
!*    Incluye Resistencia en el cable                           %
!*    _ EV and EX equal for each conductor                                                          %
!*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****************************************************************
!*                INPUT AND OUTPUT DATA DESCRIPTION             *
!*                                                              *
!*		Xdata(1):timestep   (Tiempo de Execución)               *
!*		Xdata(2):timestop										*
!*		Xdata(3): número de conductores                         *
!*		Xdata(4):Num_lin										*
!*		Xvar(1): kmax											*
!*		Xvar(2): SIZE_T											* 
!*		Xvar(3)=dx												*
!*		Xvar(4)=o												*
!*		Xvar(5)=e												*
!*		Xvar(6)=conductividad									*
!*		Xvar(7):t0												*
!*		Xvar(8)=dx2
!*      Xvar(9)=nlin											*
!*      Xvar(10):maximum number of division in x				*
!*      Xvar(11):maximum number of conductors                   *
!*      Xvar(20)=ind_ini1 !initial variable for saving h values
!*		Xin(1):  t    (Tiempo de Execución)                     *
!*		Xin(2):  Vant(1,1)                                      *
!*		Xin(3):  Vant(1,kmax)                                   *
!*		Xin(4):  Iant(1,1)                                      *
!*		Xin(5):  Iant(1,kmax)									* 
!*		Xin(6):  Vant(2,1)                                      *
!*		Xin(7):  Vant(2,kmax)                                   *
!*		Xin(8): Iant(2,1)                                       *
!*		Xin(9): Iant(2,kmax) 									* 
!*		Xin(10): Vant(3,1)                                      *
!*		Xin(11): Vant(3,kmax)                                   *
!*		Xin(12): Iant(3,1)                                      *
!*		Xin(13): Iant(3,kmax)									* 
!*		Xin(14): Vant(4,1)                                      *
!*		Xin(15): Vant(4,kmax)                                   *
!*		Xin(16): Iant(4,1)                                      *
!*		Xin(17): Iant(4,kmax)									* 
!*		Xout(1): Vi(1,1)                                        *
!*		Xout(2): Vi(1,kmax)                                     *
!*		Xout(3): Ii(1,1)                                        *
!*		Xout(4): Ii(1,kmax)                                     *
!*		Xout(5): -Ev(1,1)*hm                                    *
!*		Xout(6): -Ev(1,kmax)*hm                                 *
!*		Xout(7): Vi(2,1)                                        *
!*		Xout(8): Vi(2,kmax)                                     *
!*		Xout(9): Ii(2,1)                                        *
!*		Xout(10):Ii(2,kmax)                                     *
!*		Xout(11):Vi(3,1)                                        *
!*		Xout(12):Vi(3,kmax)                                     *
!*		Xout(13):Ii(3,1)                                        *
!*		Xout(14):Ii(3,kmax)                                     *
!*		Xout(15):V(4,1)                                         *
!*		Xout(16):Vi(4,kmax)                                     *
!*		Xout(17):Ii(4,1)                                        *
!*		Xout(18):Ii(4,kmax)                                     *
!****************************************************************

SUBROUTINE yaluk_init(xdata,xin,xout,xvar)
!DEC$ ATTRIBUTES STDCALL, DLLEXPORT::yaluk_init

!! For Compaq Fortran USE DFLIB  !Module which manage information on directories 
!USE DFLIB !visual fortran 6.0
!USE DFPORT
!USE IFPORT ! visual fortran 9.
!USE IFCORE
!USE IFQWIN
	
IMPLICIT NONE

INTEGER SIZE_T,SIZE_T2,SIZE3,SIZE_ini, i, j, k,cond, kmax, SIZE_1,SIZE_2,g,n,ind_ini1,ind_ini,					&
     nmaxi1,GNI,conductividad,num_lin,ALLOC_ERR,ERRNUM,ERRNUM2,ERRNUM_2
DOUBLE PRECISION xin(*), xout(*), xvar(*),xdata(*)
DOUBLE PRECISION r, rant,dt, tmax,XA,XB, dt3,scale_factor,				&
     a,c, v, t, zlam, Hcan,tmax1,dt2,	dx2,dx,						&
     o, e,ang,t0,dti,ti,dti1,RSLT,h,angant,		&
     Ih1, Ih2, tao11, tao12,tao21,tao22, n1, n2, eta1, eta2,YR,L,	&
     BESSELI0,BESSELI1,t2,p,p12,pi, E_TIME,t02,				&
        s,rc,e0,mu,vl,kcampo,  	X0,Y0,XA0,XB0,YA0,YB0,t0_min,t0_max,t_second,dist_camp
		
!	INTEGER(2) ihr, imin, isec, i100th,ihr2, imin2, isec2, i100th2	

DOUBLE PRECISION v_s,zlam_s,Hcan_s,o_s,e_s,dx_s,Ih1_s,Ih2_s, &
				tao11_s,tao21_s,n1_s,n2_s,X0_s,Y0_s,rc_s,YA0_s,YB0_s,XA0_s,XB0_s, &
				dt_s,tmax_s 
INTEGER Conductividad_s,nlin

CHARACTER (LEN=5) ncase_s
CHARACTER (LEN=3) indarchiv
CHARACTER (LEN=30) archivo, casename,evalue,prueba_archiv
CHARACTER(LEN=512) dir
INTEGER(4) length,ncase,nline,status_int
LOGICAL(4) status, status_bas,status_ef,status_hm,triangular,Imprimir_campo,campo_distante
LOGICAL comparar
INTEGER, SAVE :: ALLOC_ERR_I

PARAMETER (pi=3.141592653589793D0)
PARAMETER (mu=(4*pi)*1E-7)
PARAMETER (c=2.99792458E8)
PARAMETER (e0=1/(mu*c**2))


DOUBLE PRECISION, ALLOCATABLE :: Rm(:,:),hm(:),Xi(:),hm_s(:),Xi_s(:),Cm(:,:),Lm(:,:),Li2(:,:),	&
		Ci(:,:),Li(:,:),LCi(:,:),LC(:,:),D1(:,:),D3i(:,:),D2(:,:),D6(:,:),D7(:,:),Mv1(:,:),Mv2(:,:),Mv3(:,:),		&
		D4i(:,:),D3(:,:),D4(:,:),D5(:,:),Ro(:),Rf(:),Mi1(:,:),Mi2(:,:),Mi3(:,:),Mi4(:,:),				&
		Evini(:),Evfin(:), EZ(:),EH(:),Ex(:,:,:),					&
		Zc(:,:),Zci(:,:)

DOUBLE PRECISION, ALLOCATABLE :: Icorr(:),tcorr(:),i02(:),i12(:)

! Evini: 3 dimensiones (primera número de conductores, segunda div, en x, y tercera Nómero de líneas
! Evfin: 3 dimensiones (primera número de conductores, segunda div, en x, y tercera Nómero de líneas
! Ex: 4 dimensiones (primera número de conductores, segunda div, en t, terc. div en x y cuarta Número de líneas
! kmaxt; número máximo de divisiones en las líneas
! max_lin: numero máxmio de líneas
!		Ci(4,4),Li(4,4),Lm(4,4),Cm(4,4),Ro(4),Rf(4),Rt,					&
!    	LCi(4,4),LC(4,4),D1(4,4),D3i(4,4),D2(4,4),D4i(4,4),				&
!       D3(4,4),D4(4,4),
     	
!*	----------------------------------------------
!Cm	COMMON DATA FOR LINE DESCRIPTION
!Cm		r: Distance for Electromagnetic Field Calculation
!Cm		hm: Line Height
!Cm		XR: Cordinate X
!Cm		YR: Cordinate Y
!*	----------------------------------------------
!*	----------------------------------------------
!Cm	COMMON DATA /DAT/ FOR CHANNEL
!Cm		c: light velocity
!Cm		v: return stroke velocity
!Cm		zlam: Atenuation MTL model if zlam=0  TL model
!Cm		Hcan: Height Channel
!Cm		dt: Discretization Time
!Cm		t0: Initial Calculation Time
!*	----------------------------------------------
!*	----------------------------------------------
!Cm	COMMON /DAT2/ DATA FOR HEIDLER CURRENT WAVEFORM
!*	-----------------------------------------------
!Cm		Ih1: Current Amplitude(function 1)
!Cm		Ih2: Curren Amplitude (function 2)
!Cm		tao11: Front Time Constant (function 1)
!Cm		tao21: Decay Time Constant (function 1)
!Cm		tao12: Front Time Constant (function 2)
!Cm		tao22: Decay Time Constant (function 2)
!Cm		n1: Amplitude Constant (function 1)
!Cm		n2: Amplitude Constant (function 2)
!*	-----------------------------------------------
!*	----------------------------------------------
!Cm	COMMON /MAGN/ DATA FOR MATRIX DIMMENSIONS
!*	-----------------------------------------------
!Cm		cond: Number of Line Conductors
!Cm		kmax: Maximum number of divisions on the Line
!Cm		nmax: Maximum number of divisions on Time
!*	------------------------------------------------

COMMON /COND2/ conductividad
COMMON /COND/  o,e
COMMON /LINE_YALUK/  SIZE_1,SIZE_2
COMMON /POINT/ r,h
COMMON /DAT/   v,zlam,Hcan,dt, dt2
COMMON /DAT2/ Ih1, Ih2, tao11, tao12,tao21,tao22, n1, n2, eta1, eta2
COMMON /ITYPE/ triangular
 
COMMON /INTEG2/ nmaxi1
COMMON /INTEG/ dti1
COMMON /LPARAM/ L
!	SAVE Cm,Lm,LC,LCi,Li,Ci,D1,D2,D3i,D4i,/LPARAM/ 
!	SAVE Ex,Evini,Evfin,Icorr, tcorr,i02,i12

EXTERNAL IPULSE,BESSELI0,BESSELI1,comparar


!kmaxt=100	! kmaxt; número máximo de divisiones en las líneas
!max_lin=20	! max_lin: numero máxmio de líneas

archivo(1:9)='yaluk.ini'
ERRNUM=0


OPEN (UNIT = 11, FILE = 'status_file.ylk', FORM='UNFORMATTED', STATUS = 'OLD', ERR=1011,IOSTAT=ERRNUM2)
READ (UNIT=11) dt_s,tmax_s,ncase_s,casename,t0_min,t0_max
CLOSE (UNIT=11)


1011 SELECT CASE(ERRNUM2>0)


	CASE (.TRUE.)
	!write(*,*)'ERRNUM=', ERRNUM2
	!  Get current directory
	!dir = FILE$CURDRIVE
	!length = GETDRIVEDIRQQ(dir)
	!IF (length .EQ. 0) THEN
	!  WRITE (*,*) 'Failed to get current directory'
	!END IF

	CALL GETCWD(dir, status_int)
	IF (status_int .NE. 0) THEN
	  WRITE (*,*) 'Failed to get current directory. Error:  ',status_int
	END IF

1012 SELECT CASE(ERRNUM>0)
	CASE (.TRUE.)
			!write(*,*) 'Configuration file not founded:',dir, 
			WRITE(*,*) 'Configuration file not founded:',TRIM(dir),'/',archivo
			write(*,*) 'please enter name file:'
			read(*,*) archivo
	END SELECT 

		write(*,*) "*****************************************************"
		write(*,*) "NATIONAL UNIVERSITY OF COLOMBIA"
		write(*,*) "Runing DLL-YALUK with R in conductor 2009"
		CALL GETENV ("CPU ", evalue) 
		write(*,*) 'Using CPU=',evalue
		write(*,*) 'Authors: Ernesto Perez - Edison Soto'
		write(*,*) 'Contact: eperezg@unal.edu.co - easotor@unal.edu.co'
		write(*,*) "******************************************************"


	OPEN (UNIT = 12, FILE = archivo(1:LEN_TRIM(archivo)), STATUS = 'OLD', ERR=1012,IOSTAT=ERRNUM)
		!READ(12,*) casename !Case Name
		!READ(12,*) ncase    !Case Number
		!IF (.NOT. EOF(12)) THEN
		!	READ(12,*) nlin     ! Maximum number of Lines
    	!	IF (.NOT. EOF(12)) THEN
	    !		READ(12,*) Xvar(11)     ! Maximum number of Lines
		!    ELSE
		!	    Xvar(11)=6
	    !	END IF
		!IF ()
		!ELSE
		!	nlin=0
		!END IF

		nline = 0
		DO while (.true.)
			nline = nline + 1
			SELECT CASE(nline)
			  CASE(1)
			    READ (12, *, end=999) casename
			  CASE(2)
			    READ (12, *, end=999) ncase
				nlin = 0
			  CASE(3)
			    READ (12, *, end=999) nlin
				Xvar(11) = 6
			  CASE(4)
			    READ (12, *, end=999) Xvar(11)
			END SELECT 
		ENDDO
		999 continue


	CLOSE (UNIT=12)
	!status = CHANGEDIRQQ(dir(1:length)//'\'//casename(1:LEN_TRIM(casename)))
	write(*,*) 'Trying to open: ',TRIM(dir)//'/'//casename(1:LEN_TRIM(casename))
	CALL CHDIR(TRIM(dir)//'/'//casename(1:LEN_TRIM(casename)), status_int)

		IF (status_int .EQ. 0) THEN
				!dir = FILE$CURDRIVE
				!length = GETDRIVEDIRQQ(dir)
				CALL GETCWD(dir, status_int)
			write(*,*) 'Current Directory for cases: ',TRIM(dir)
		ELSE
			!status = MAKEDIRQQ(dir(1:length)//'\'//casename(1:LEN_TRIM(casename)))
			write(*,*) 'Trying to create: ',TRIM(dir)//'/'//casename(1:LEN_TRIM(casename))
			CALL SYSTEM( 'mkdir ' //TRIM(dir)//'/'//casename(1:LEN_TRIM(casename)), status_int)
			IF (status_int .EQ. 0) THEN
				!write(*,*) ' Directory for cases was created ',dir(1:length)//'\'//casename(1:LEN_TRIM(casename))
				write(*,*) ' Directory for cases was created ',TRIM(dir)//'/'//casename(1:LEN_TRIM(casename))
				write(*,*) '**************************************************************'
				write(*,*) '*  PLEASE PUT THE LINE AND MISCELANEO FILES IN THIS DIRECTORY*'
				write(*,*) '**************************************************************'
				!pause

			ELSE
				!write(*,*) ' Error Creating Directory: ',dir(1:length)//'\'//casename(1:LEN_TRIM(casename))
				write(*,*) ' Error Creating Directory: ',TRIM(dir)//'/'//casename(1:LEN_TRIM(casename))
				write(*,*) ' Please check permissions'
			ENDIF
		END IF

		!****************************************************************************
		!OPENING LINE DATA FOR CALCULATING t0
		! CALCULATING MAXIMUM KMAXT
		!****************************************************************************
		cond=1 ! int(Xdata(3))
		ALLOCATE (Rm(cond,cond),hm(1:cond),Xi(1:cond), STAT=ALLOC_ERR)
		t0_min=1.D200
		t0_max=0
		Xvar(10)=0 !número máximo de divisiones
		IF (nlin.GT.0)		THEN
		
		WRITE(ncase_s,FMT=121) ncase
		121		FORMAT(I5.5)
			CALL OPEN_CORR_FILE (Ih1,Ih2,tao11,tao21,tao12,tao22,n1,n2,X0,Y0,ncase_s)
			CALL OPEN_MISC_FILE (v,zlam,Hcan,tmax1,SIZE_2,Conductividad,o,e,dx,triangular,dist_camp)
			DO i=1,nlin
				120 FORMAT(I3.3) 
				WRITE(indarchiv,FMT=120) int(i)
				CALL OPEN_LINE_FILE(rc,XA0,YA0,XB0,YB0,hm,Rm,Xi,cond,indarchiv)
				
				IF (r .NE.-1) THEN
					a=ATAN2((YA0-YB0),(XA0-XB0))
					XA=(XA0-X0)*cos(a)+(YA0-Y0)*sin(a)
					XB=(XB0-X0)*cos(a)+(YB0-Y0)*sin(a)
					YR=(YA0-Y0)*cos(a)-(XA0-X0)*sin(a)
					L=XA-XB
                    
                   if (nint(L/dx)+4 .GT. Xvar(10)) THEN
                     Xvar(10)=nint(L/dx)+4
                    END IF
					IF (XA .LT. 0 .AND. XA .GT. XB) THEN
						t0=(XA**2+Yr**2)**0.5/c;
					ELSEIF (XB .LT. 0 .AND. XB .GT. XA) THEN
						t0=(XB**2+Yr**2)**0.5/c;
					ELSEIF (XA .GT. 0 .AND. XA .LT. XB) THEN
						t0=(XA**2+Yr**2)**0.5/c;
					ELSEIF (XB .GT. 0 .AND. XB .LT. XA) THEN
						t0=(XB**2+Yr**2)**0.5/c;
					ELSE
						t0=abs(Yr)/c;
					ENDIF
					
					IF (t0.LE.t0_min) THEN
						t0_min=t0
					ELSEIF(t0.GE.t0_max) THEN
					    t0_max=t0
					END IF
				END IF

				

			END DO
			t0_min=t0_min-dt
		ELSE
			t0_min=0
			t0_max=0
			Xvar(10)=200
		END IF
    !****************************************************************************
    !****************************************************************************

			write(*,*)'t0_min=',t0_min !WRITES THE INITIAL TIME
		
		DEALLOCATE (Rm,hm,Xi,STAT=ALLOC_ERR)
		!____________________________________________________________________________
		!****************************************************************************

		dt_s=Xdata(1)			!data for comparing if the file was opened before
		tmax_s=Xdata(2)
		101		FORMAT(I5.5)
		!File for knowing if any line is already calculated
		WRITE(ncase_s,FMT=101) ncase
		OPEN (UNIT = 13, FILE = 'status_file.ylk', FORM='UNFORMATTED', STATUS = 'UNKNOWN', ERR=1013,IOSTAT=ERRNUM)
		WRITE (UNIT=13) dt_s,tmax_s,ncase_s,casename,t0_min,t0_max
		CLOSE(UNIT=13)
		write(*,*)'CASE NO:',ncase_s
1013	SELECT CASE(ERRNUM>0)
			CASE (.TRUE.)
			WRITE(*,*) 'Error saving status_file.ylk', ERRNUM
			END SELECT 

		END SELECT
ERRNUM=0
	
!*		-----------------------
!*		Setting Input Variables
!*		-----------------------
!		tmax = 20.D-6	- maximum simulation time given by ATP
!		dt=1D-8			- timestep given by ATP
!*		--------------------------------------------------------


dt=Xdata(1)
tmax=Xdata(2)
cond=int(Xdata(3))
num_lin=Xdata(4)
!t0_min=Xvar(9)
100 FORMAT(I3.3) 
WRITE(indarchiv,FMT=100) int(Xdata(4))
!open(UNIT = 99, FILE = 'cond_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN')
!write(UNIT = 99) cond
!close(UNIT = 99)


	  ALLOCATE (Rm(cond,cond),hm(1:cond),Xi(1:cond),hm_s(1:cond),Xi_s(1:cond),Cm(cond,cond),Lm(cond,cond),Li2(cond,cond),					&
		Ci(cond,cond),Li(cond,cond),LCi(cond,cond),LC(cond,cond),Mv1(cond,cond),Mv2(cond,cond),Mv3(cond,cond),Mi1(cond,cond),Mi2(cond,cond),Mi3(cond,cond),Mi4(cond,cond),		&
		D1(cond,cond),D3i(cond,cond),D2(cond,cond),D4i(cond,cond),D5(cond,cond),D6(cond,cond),D7(cond,cond),Zc(cond,cond),		&
		Zci(cond,cond),D3(cond,cond),D4(cond,cond),Ro(cond),Rf(cond), STAT=ALLOC_ERR)


IF (dt .EQ. dt_s .AND. tmax .EQ. tmax_s) THEN
	status_bas=.TRUE.
	open(UNIT = 14, FILE = 'bas_'//ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'OLD',ERR=1014,IOSTAT=ERRNUM)
	read(UNIT = 14, ERR=1021,IOSTAT=ERRNUM_2) v_s,zlam_s,Hcan_s,Conductividad_s,o_s,e_s,dx_s,Ih1_s,Ih2_s,tao11_s,tao21_s,n1_s,n2_s,X0_s,Y0_s,rc_s,YA0_s,YB0_s,XA0_s,XB0_s,hm_s,Xi_s,kmax,t0
	write(*,*) 'cargando datos Base de caso previo: ',indarchiv
	close (UNIT = 14)
	
1014 SELECT CASE(ERRNUM>0)
		CASE (.TRUE.)
			status_bas=.FALSE.
		END SELECT 
		
1021 SELECT CASE(ERRNUM_2>0)
		CASE (.TRUE.)
			status_bas=.FALSE.
		END SELECT 

ELSE
	status_bas=.FALSE.
ENDIF




!indarchiv:  Es el indicador del número de línea el cual se utiliza para los datos de línea
!			y para los archivos internos que se usan en Yaluk_exec
t=Xin(1)
!*		-----------------------------------------------------------------
!*         READING MISCELANEOUS PARAMETERS
!*		_________________________________________________________________

CALL OPEN_MISC_FILE (v,zlam,Hcan,tmax1,SIZE_2,Conductividad,o,e,dx,triangular,dist_camp)

!		------------------------------------
!        Reading Lightning Current Parameters
!		------------------------------------

CALL OPEN_CORR_FILE (Ih1,Ih2,tao11,tao21,tao12,tao22,n1,n2,X0,Y0,ncase_s)

!	------------------------------------------------------
!        Reading Line Position and Stroke Parameters
!	------------------------------------------------------
!	---------------------------
!*	Line Coordinates
!*	   <---- Xr-----> ==================
!*	   !             +      Line
!*	   !           +
!*	   !         +   
!*	YR !       +   
!*	   !     +  R
!*	   !   +
!*	   ! +
!*	   * (Lightning strike)   
!*	---------------------------
!      a:=atan2(YA0-YB0,XA0-XB0)
!      XA:=(XA0-X0)*cos(a)+(YA0-Y0)*sin(a)
!      XB:=(XB0-X0)*cos(a)+(YB0-Y0)*sin(a)
!      YR :=(YA0-Y0)*cos(a)-(XA0-X0)*sin(a)
!	L=XA-XB
!inicializando valores
 	CALL OPEN_LINE_FILE(rc,XA0,YA0,XB0,YB0,hm,Rm,Xi,cond,indarchiv)


!configuring maximum first window from front time
tmax1=1.8*(tao11**n1*tao21*n1)**(1/(1+n1))
  OPEN (UNIT = 101, FILE = 'interp.txt', STATUS = 'OLD',ERR=10101,IOSTAT=ERRNUM)
   READ (101,*) t_second
   READ (101,*) dt3
   CLOSE(101)
10101 SELECT CASE(ERRNUM>0)
	    CASE (.TRUE.)
			write(*,*) 'error leyendo interp.txt'
t_second=10.e-6 !maximum time for calculation on the farthest line
dt3=.5e-6           !discretization time for improving matrix size
		ENDSELECT 
 write(*,*) 'Line 482 pass'  
scale_factor=real(nint(dt3/dt))
write(*,*) 'Scale Factor: ',scale_factor  
dt3=dt*scale_factor      
write(*,*) 'dt3: ',dt3
SIZE_T = nint(tmax / dt)
write(*,*) 'SIZE_T: ',SIZE_T		!number of time division for the whole window 
write(*,*) 'SIZE_1: ',SIZE_1
SIZE_1= nint(tmax1/dt)
write(*,*) 'SIZE_1: ',SIZE_1			!number of time division for first window
!***********************************************************
! including new size elements for reducing matrix dimension
 write(*,*) 'Line 489 pass' !-----------------------------------------------------------
SIZE_ini=SIZE_1+nint((t0_max+t_second)/dt)
IF (SIZE_T.LE.SIZE_ini-nint(dt3/dt)) THEN
    SIZE_ini=SIZE_T-1
    SIZE3=1
ELSE
    SIZE3=nint((tmax-real(SIZE_ini)*dt)/dt3)
ENDIF
write(*,*) 'Line 497pass'
SIZE_T2=SIZE_ini+SIZE3
!***********************************************************
dti=.05D-8
dti1=dti
nmaxi1=nint(dt*SIZE_1/dti)
dt2= (tmax-tmax1)/SIZE_2 !time step for second window
 write(*,*) 'Line 504 pass' 
SIZE_2=SIZE_2+1			!number of time division for first window
 write(*,*) 'Line 505 pass' 
a=ATAN2((YA0-YB0),(XA0-XB0))
XA=(XA0-X0)*cos(a)+(YA0-Y0)*sin(a)
XB=(XB0-X0)*cos(a)+(YB0-Y0)*sin(a)
YR=(YA0-Y0)*cos(a)-(XA0-X0)*sin(a)
L=XA-XB

!*************************************************************************
!*      Courant Criteria
!*		-------------------------------------------------------
!*		dx=10			- line division size (given by equation)
!*		--------------------------------------------------------
!*************************************************************************

IF (dx.LE.dt*(1.1*c) .OR. dx .EQ.0) THEN
!	write(*,*) 'Warning: dx is too small or equal to 0. unconvergence is very likely' 
	dx=dt*(2*c)         !
	write(*,*) 'dx is automatically computed dx=',dx 
ENDIF
		

dx2=c*dt

kmax = nint((L-2*dx2)/dx+2) !taking into account the two bergeron lines for coupling with ATP
if (kmax.LE.3) THEN
    		write(*,*) 'Number of divisions on the line is too small - reduce dt value'
    		STOP 
ENDIF
dx=(L-2*dx2)/real(kmax-2)
IF ((L-(kmax-2)*dx-2*dx2).GE.(0.5*dx)) THEN
	kmax=kmax+1
END IF
!write(*,*)'longitud=',(kmax-2)*dx+2*dx2,'dx2=',dx2, '  dx=',dx


IF( .NOT. ALLOCATED(Evini)) THEN
    ALLOCATE (Evini(SIZE_T2+2),Evfin(SIZE_T2+2),EZ(SIZE_T+2),EH(SIZE_T+2),			&
		  Ex(cond,kmax+1,SIZE_T2+2),STAT=ALLOC_ERR)
ENDIF
IF( .NOT. ALLOCATED(Icorr)) THEN
    ALLOCATE (Icorr(nmaxi1+11*SIZE_2),tcorr(nmaxi1+11*SIZE_2),i02(SIZE_T+2),i12(SIZE_T+2),STAT=ALLOC_ERR_I) 
ENDIF
!-------------------------------------------------------
!ALLOC_ERR_I - INDICA SI LA CORRIENTE YA FUE CALCULADA
!-------------------------------------------------------


IF (status_bas .AND. comparar(hm,hm_s,cond)) THEN
	status_bas=.TRUE.
	status_hm=.TRUE.
!	write(*,*) 'comparando h(verdadero)'
ELSE
	status_bas=.TRUE.
	status_hm=.FALSE.
!	write(*,*) 'comparando h(falso)'

ENDIF

IF (status_bas .AND. v_s .EQ. v .AND. zlam_s .EQ. zlam .AND. Hcan .EQ. Hcan .AND. Conductividad_s .EQ. Conductividad		&
	.AND. o_s .EQ. o .AND. e_s .EQ. e .AND. dx_s .EQ. dx .AND. Ih1_s .EQ. Ih1 .AND. Ih2_s .EQ. Ih2 .AND. tao11_s .EQ. tao11	&
	.AND. tao21_s .EQ. tao21 .AND. n1_s .EQ. n1 .AND. n2_s .EQ. n2 .AND. X0_s .EQ. X0 .AND. Y0_s .EQ. Y0.AND. rc_s .EQ. rc	&
	.AND. YA0_s .EQ. YA0 .AND. YB0_s .EQ. YB0 .AND. XA0_s .EQ. XA0.AND. XB0_s .EQ. XB0 ) THEN

	    open(UNIT = 15, FILE = ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'OLD',ERR=1015,IOSTAT=ERRNUM)
		read(UNIT = 15) Evini,Evfin,Ex,D3i,D5,Zc,Zci,Mv1,Mv2,Mv3,Mi1,Mi2,Mi3,Mi4


!	write(*,*) 'cargando datos de Campo electromagnético del archivo previo linea'//indarchiv
	status_ef=.FALSE.
	close (UNIT = 15)
1015 SELECT CASE(ERRNUM>0)
		CASE (.TRUE.)
			status_ef=.TRUE.
		END SELECT 
ELSE
	status_ef=.TRUE.
ENDIF

!write(*,*) 'status_bas ',status_bas,' status_hm ',status_hm,' status_ef ',status_ef
 IF (status_bas .EQV. .FALSE. .OR. status_hm .EQV. .TRUE. .OR. status_ef .EQV. .TRUE. ) THEN
!******************************************************************************************
!				CALCULATING LINE PARAMETERS
!******************************************************************************************
	CALL LINEPARAM(cond,Xi,hm,rc,Lm,Cm,Zc)



!*--------------------------------------------------------------
!*
!       MATRICES CONSTANTES PARA EL CALCULO DE DIFERENCIAS 
!                        FINITAS
!* D1:  Constante para calculo de frontera inicio línea (dx/dt*C*Rom-unos)
!* D3i: Constante inicio línea  inv(dx/dt*C*Rom+unos)
!* D2:  Constante para calculo de frontera final línea (dx/dt*C*Rfm-unos)
!* D4i  Constante para calculo de frontera final línea  inv(dx/dt*C*Rfm+unos)
!* D5   Constante para calculo de frontera final línea  (dx/dt*C*Zc)
!* Zci: Inversa Impedancia Característica
!*
!*--------------------------------------------------------------

		CALL inv(Zc,cond,cond,Zci)
		CALL mult(Lm,Cm,LC,cond,cond,cond)
		CALL inv(Cm,cond,cond,Ci)
		CALL inv(Lm,cond,cond,Li)
		CALL inv(LC,cond,cond,LCi)

!*	--------------------------------
!*	matrix Construction M
!*	--------------------------------


Mv1=.5*dt/dx*Ci
Mv2=.25*dt*dt/dx*LCi
Mv3=.25*dt*dt/dx*matmul(Rm,LCi)
Mi1=dt*Li
Mi2=.5*dt*dt/(dx*dx)*LCi
Mi3=.5*dt*dt*matmul(Rm,Li)
!********************************************
!Considering loss in the conductor
 D6=dt*Rm
 D6=matmul(D6,Li)
 D7=0.5*dt*dt*matmul(Rm,Rm)
 !Li2=matmul(Li,Li)
 D7=matmul(D7,Li)
 Mi4=-D6-D7
!********************************************	

!*	--------------------------------
!*	Construcción de D1, D2, D3i, D4i
!*	--------------------------------
	D1=dx/dt*Cm
	D2=D1
	D5=matmul(D1,Zc)
	D1=D5
	D2=D5
	D3=D5
	D4=D5
	



DO 21, i=1,cond
  D1(i,i)=D1(i,i)-1
  D2(i,i)=D2(i,i)-1
  D3(i,i)=D3(i,i)+1
  D4(i,i)=D4(i,i)+1
  Mi4(i,i)=Mi4(i,i)+1 !Considering loss in the conductor

21	CONTINUE
	CALL inv(D3,cond,cond,D3i)
	CALL inv(D4,cond,cond,D4i)
 ENDIF




IF (status_ef) THEN


	
!*************************************************
! Calculating constants for heidler function
!----------------------------------------------------
	IF (tao11 .EQ.0 .OR. tao21 .EQ.0 ) THEN
		eta1=0.D0
	ELSE
		eta1 = DEXP(-(tao11/tao21)*(n1*tao21/tao11)**(1/n1)) !calculating eta1 for heidler current
	END IF
	IF (tao12 .EQ.0 .OR. tao22 .EQ. 0) THEN
		eta2 =0.D0
	ELSE
		eta2 = DEXP(-(tao12/tao22)*(n2*tao22/tao12)**(1/n2)) !calculating eta2 for heidler current
	END IF


!*	--------------------------------------------------------------
!                  Computing Initial time
!*	--------------------------------------------------------------
	IF (XA .LT. 0 .AND. XA .GT. XB) THEN
		t0=(XA**2+Yr**2)**0.5/c;
	ELSEIF (XB .LT. 0 .AND. XB .GT. XA) THEN
		t0=(XB**2+Yr**2)**0.5/c;
	ELSEIF (XA .GT. 0 .AND. XA .LT. XB) THEN
		t0=(XA**2+Yr**2)**0.5/c;
	ELSEIF (XB .GT. 0 .AND. XB .LT. XA) THEN
		t0=(XB**2+Yr**2)**0.5/c;
	ELSE
	    t0=abs(Yr)/c;
	ENDIF






!***************************************************************
!***************************************************************

	
	IF (t0 .GT. dist_camp/c) THEN
		write(*,*) 'Disregarding the EM field calculation for this line_',indarchiv
		write(*,*) 'too far away - for not disregarding EM field, change miscelaneo.txt file'
		campo_distante=.TRUE.
	ELSE
		campo_distante=.FALSE.
	END IF
!***************************************************************
!***************************************************************

!*	******************************************************************
!*                  Current Integral Computation
!*	--------------------------------------------------------------

!	Calculating First Window
!	--------------------------
	ti=0.D0

IF (campo_distante .EQV. .FALSE.) THEN
	IF (ERRNUM2 .GT. 0) THEN
	IF (triangular .EQV. .TRUE.) THEN
		write(*,*) 'Usando onda triangular'
	ELSE
		write(*,*) 'Usando onda Heidler'
	ENDIF
	END IF

	IF (ALLOC_ERR_I .EQ. 0 ) THEN  
      DO 1, i=1,nmaxi1
      
		IF (ti .EQ. 0) THEN
			Icorr(i)=0.D0
			tcorr(i)=0.D0
		ELSE
			IF (triangular .EQV. .FALSE.) THEN

			CALL INTEGRAL(IPULSE,0.D0,ti,RSLT,1.D-7,GNI)
			Icorr(i)=RSLT
			tcorr(i)=ti
		
			
			ELSE
			!*****************************
			! Integral for double ramp function
			!*******************************

				IF (ti .LE. tao11 ) THEN
					Icorr(i)=.5*Ih1*ti**2/tao11
					tcorr(i)=ti
				ELSEIF(ti .GT. tao11 .OR. ti .LT. 2*tao21-tao11) THEN
					Icorr(i)=.5*Ih1*tao11 !+ Ih1*(ti-tao11)-Ih1*(.5*ti*ti-ti*tao11)/(2*tao21-tao11)
					tcorr(i)=ti
				ELSE
					Icorr(i)=.5*Ih1*tao11 !+ Ih1*(tao21-tao11)-Ih1*(.5*tao21*tao21-tao21*tao11)/(2*tao21-tao11)
					tcorr(i)=ti
				END IF
			ENDIF
		
		ENDIF

		ti=ti+dti 

1     CONTINUE
		ti=ti-dti  !resting the extra dti no calculated
        dti=(dt*SIZE_T-ti)/(10*SIZE_2)

!*******************************************************
!	--------------------------
!	Calculating Second Window
!	--------------------------
!*******************************************************
      DO 2, j=1,11*SIZE_2
	    ti=ti+dti
		IF (triangular .EQV. .FALSE.) THEN
			!*****************************
			! Using Heidler Function
			!*****************************
			CALL INTEGRAL(IPULSE,0.D0,ti,RSLT,1.D-7,GNI)
			Icorr(nmaxi1+j)=RSLT
			tcorr(nmaxi1+j)=ti
		ELSE
			!*****************************
			! Integral for double ramp function . if triangular =.TRUE.
			!*******************************
			IF (ti .LE. tao11 ) THEN
				Icorr(nmaxi1+j)=.5*Ih1*ti**2/tao11
				tcorr(nmaxi1+j)=ti
			ELSEIF(ti .GT. tao11 .OR. ti .LT. 2*tao21-tao11) THEN
				Icorr(nmaxi1+j)=.5*Ih1*tao11 !+ Ih1*(ti-tao11)-Ih1*(.5*ti*ti-ti*tao11)/(2*tao21-tao11)
				tcorr(nmaxi1+j)=ti
			ELSE
				Icorr(nmaxi1+j)=.5*Ih1*tao11 !+ Ih1*(tao21-tao11)-Ih1*(.5*tao21*tao21-tao21*tao11)/(2*tao21-tao11)
				tcorr(nmaxi1+j)=ti
			END IF
		ENDIF

2     CONTINUE
!---------------------------------------------------------------------
! END CALTULATING Lightning Current
!*	******************************************************************

!************************************************+
! Script para grabar la corriente calculada
!************************************************

!	OPEN(UNIT=3, FILE='integrali.txt', ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
 !    WRITE(UNIT=3,FMT='(1X,E12.6E2,E12.6E2)')(tcorr(j),Icorr(j), j=1,(nmaxi1+10*SIZE_2+2))
!	CLOSE(3)

!*	--------------------------------------------------------------
!                  BESSEL Integrals Computation
!*	--------------------------------------------------------------
		IF (conductividad .EQ. 1) THEN
			p=o/(2*e0*e)
			t2=-dt
			DO 3 i=1,SIZE_T+2
				t2=t2+dt
				p12=p*(t2)
				i02(i)=BESSELI0(p12)
				i12(i)=BESSELI1(p12)
3			CONTINUE
		ENDIF
	ENDIF

!	OPEN(UNIT=4, FILE='bessel.txt', ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
!     WRITE(UNIT=4,FMT='(1X,E12.3E2,E12.3E2)')(i02(j),i12(j), j=1,(SIZE_T+2))
!	CLOSE(4)

!asignando valor de h para cada cálculo de la integral importante para Componente horizontal

   DO 51,k = 0,kmax
	  DO 50,g=1,cond
		h=hm(g)
	  IF (XA.LT.XB) THEN
		IF (k.EQ.0) THEN
			r = DSQRT( ( XA ) ** 2 + (YR+Xi(g)) * (YR+Xi(g)) )
			ang = ( (XA ) )/r

		ELSEIF (k.EQ.kmax) THEN

			!        2*vl*dt represents the two small lines at ends
		
			r = DSQRT( ( XA + 2*dx2 + (kmax-2) * dx ) ** 2 + (YR+Xi(g)) * (YR+Xi(g))  )
			ang = ( (XA + 2*dx2 + (kmax-2) * dx) )/r

		ELSE
			!vl*dt represents the small line at the begining
			r = DSQRT( ( XA + dx2 + (k-1) * dx ) ** 2 + (YR+Xi(g))  * (YR+Xi(g))  )
			ang = ( (XA + dx2 + (k-1) * dx) )/r

		ENDIF	  
	   ELSE
		IF (k.EQ.0) THEN
			r = DSQRT( ( XA ) ** 2 + YR * YR )
			ang = ( (-XA ) )/r
	

		ELSEIF(k.EQ.kmax) THEN
			!2*vl*dt represents the two small lines at ends
			r = DSQRT( ( XA - 2*dx2 - (kmax-2) * dx ) ** 2 + (YR+Xi(g))  * (YR+Xi(g))  )
			ang = ( (- XA + 2*dx2 + (kmax-2) * dx) )/r
		
		
		ELSE

			r = DSQRT( ( XA - dx2 - (k-1) * dx ) ** 2 + (YR+Xi(g))  * (YR+Xi(g))  )
			ang = ( (-XA + dx2 + (k-1) * dx) )/r	  
		END IF
		END IF
			t02=r/c; !initial time of the signal

		!SIZE_1= (tmax1+t02-t0)/dt			!considering t0 for initiation
		SIZE_1= (tmax1+t02-t0_min)/dt
		!SIZE_2=(tmax-(tmax1+t02-t0))/dt2	!considering t0 for initiation
		SIZE_2=(tmax-(tmax1+t02-t0_min))/dt2
		SIZE_2=SIZE_2+2
		IF (SIZE_2 .LT. 2) THEN
			SIZE_2=2
		END IF
		!IF ((SIZE_1*dt+t02-t0) .GT. tmax) THEN
		IF ((SIZE_1*dt+t02-t0_min) .GT. tmax) THEN
			SIZE_1=tmax/dt
			SIZE_2=2
		END IF
		  IF (1.005 .GT. abs(rant/r) .AND. abs(rant/r) .GT. .995 .AND. h .EQ. hm(1)) THEN
			n=nint(abs(r-rant)/c/dt)+1 
			Ex(g,k+1,n:SIZE_T+2)=Ex(g-1,k+1,1:SIZE_T+3-n)*abs(rant/r)/angant*ang
			!IF (k.EQ.0 ) THEN
			!  Evini(n:SIZE_T+2)=Evini(1:SIZE_T+3-n)*abs(rant/r)/angant*ang
			!ELSEIF (k .EQ. kmax) THEN
			!  Evfin(g,n:SIZE_T+2)=Evfin(g-1,1:SIZE_T+3-n)*abs(rant/r)/angant*ang
			!ENDIF
		  ELSE
			IF (k.EQ.0) THEN
				kcampo=1	!Identify if Ev will be calculated or not
				CALL EMFIELD(SIZE_T+2, EZ, EH,ang,Icorr,tcorr,i02,i12,kcampo,t0_min)
				!Evini(:)=EZ(:)
				Evini(1:SIZE_ini)=EZ(1:SIZE_ini)
				FORALL (i=1:SIZE3) 	Evini(SIZE_ini+i)=EZ(SIZE_ini+i*INT(scale_factor))			
			ELSEIF (k.EQ.kmax) THEN
				kcampo=1
				CALL EMFIELD(SIZE_T+2, EZ, EH,ang,Icorr,tcorr,i02,i12,kcampo,t0_min)
!				Evfin(:)=EZ(:)
				Evfin(1:SIZE_ini)=EZ(1:SIZE_ini)
				FORALL (i=1:SIZE3) 	Evfin(SIZE_ini+i)=EZ(SIZE_ini+i*INT(scale_factor))			

			ELSE
				kcampo=0
				CALL EMFIELD(SIZE_T+2, EZ,EH,ang,Icorr,tcorr,i02,i12,kcampo,t0_min)
				
			ENDIF
			
			!Ex(g,k+1,:)=EH(:)
			Ex(g,k+1,1:SIZE_ini)=EH(1:SIZE_ini)
			FORALL (i=1:SIZE3) Ex(g,k+1,SIZE_ini+i)=EH(SIZE_ini+i*INT(scale_factor))
		  ENDIF
			rant=r
			angant=ang
	

! WRITE(*,*)'punto ',k+1, 'de ', kmax+1
!	write(*,*) 'r=',r,' ang=', ang
50    CONTINUE
		rant=0.D0
	!IF (k .EQ. int(.25*kmax) .OR. k .EQ. int(.5*kmax) 	&
	!	.OR. k .EQ. int(.75*kmax) .OR. k .EQ. kmax ) THEN
	!	write(*,*)'Percentage calculated for line',indarchiv,' : ',int(real(k)/real(kmax)*100),'%'
	!END IF

51 CONTINUE
write(*,*)'*** EM Field calculated for line_',indarchiv

!****************************************
!Routina para imprimir campo
!***************************************
ELSE
	Evini=0
	Evfin=0
	Ex=0
ENDIF
Imprimir_campo=.TRUE.

IF (Imprimir_campo .EQV. .TRUE.) THEN
	IF (k.EQ.1 .OR. k .EQ. (kmax+1)) THEN
		OPEN(UNIT=3, FILE='campo'//indarchiv//'.txt', ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
		WRITE(UNIT=3,FMT='(1X,E14.6E2,E14.6E2,E14.6E2,E14.6E2)')(Evini(j),Evfin(j),Ex(cond,1,j),Ex(cond,kmax+1,j), j=1,SIZE_T2)
		CLOSE(3)
	ENDIF
ENDIF



t0=t0-t0_min

open(UNIT = 16, FILE = ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1016,IOSTAT=ERRNUM)
write(UNIT = 16) Evini,Evfin,Ex,D3i,D5,Zc,Zci,Mv1,Mv2,Mv3,Mi1,Mi2,Mi3,Mi4
close (UNIT = 16)

open(UNIT = 17, FILE = 'bas_'//ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1017,IOSTAT=ERRNUM)
write(UNIT = 17) v,zlam,Hcan,conductividad,o,e,dx,Ih1,Ih2,tao11,tao21,n1,n2,X0,Y0,rc,YA0,YB0,XA0,XB0,hm,Xi,kmax,t0
close (UNIT = 17)
				 
1016 SELECT CASE(ERRNUM>0)
		CASE (.TRUE.)
			write(*,*)'Error grabando',casename(1:LEN_TRIM(casename))//indarchiv//'.dat'
		END SELECT 
1017 SELECT CASE(ERRNUM>0)
		CASE (.TRUE.)
			write(*,*)'Error grabando ','bas_'//casename(1:LEN_TRIM(casename))//indarchiv//'.dat'
		END SELECT 



ELSE
write(*,*)'Reading from calculated EMField_',indarchiv
if(status_hm) THEN


open(UNIT = 19, FILE = ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1019,IOSTAT=ERRNUM)
write(UNIT = 19) Evini,Evfin,Ex,D3i,D5,Zc,Zci,Mv1,Mv2,Mv3,Mi1,Mi2,Mi3,Mi4
close (UNIT = 19)

open(UNIT = 18, FILE = 'bas_'//ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1018,IOSTAT=ERRNUM)
write(UNIT = 18) v,zlam,Hcan,conductividad,o,e,dx,Ih1,Ih2,tao11,tao21,n1,n2,X0,Y0,rc,YA0,YB0,XA0,XB0,hm,Xi,kmax,t0
close (UNIT = 18)
1019 SELECT CASE(ERRNUM>0)
		CASE (.TRUE.)
			write(*,*)'Error grabando',casename(1:LEN_TRIM(casename))//indarchiv//'.dat'
		END SELECT 
1018 SELECT CASE(ERRNUM>0)
		CASE (.TRUE.)
			write(*,*)'Error grabando ','bas_'//casename(1:LEN_TRIM(casename))//indarchiv//'.dat'
		END SELECT 

ENDIF



ENDIF
!********************************
! Definiendo Variables del Model
!--------------------------------
Xvar(1)= kmax											
Xvar(2)= SIZE_T 
Xvar(3)=dx											
Xvar(4)=o
Xvar(5)=e
Xvar(6)=conductividad
Xvar(7)=t0     !calculation of initial time
Xvar(8)=dx2
Xvar(9)=nlin !enviando número máximo de líneas
!Definiendo Variables de Salida
Xvar(12)=SIZE_T2
Xvar(13)=SIZE_ini
Xvar(14)=dt3
ind_ini1=21
Xvar(20)=ind_ini1
Xvar(ind_ini1:ind_ini1-1+cond)=hm(:)

		ind_ini=ind_ini1+cond


!***********************************************************
!	DO i=1,cond
!		Xvar(ind_ini             +(i-1)*cond:ind_ini-1            +i*cond)=D3i(i,:) 
!		Xvar(ind_ini +  cond*cond+(i-1)*cond:ind_ini-1+  cond*cond+i*cond)=D5(i,:)  
!		Xvar(ind_ini +2*cond*cond+(i-1)*cond:ind_ini-1+2*cond*cond+i*cond)=Zc(i,:)  
!		Xvar(ind_ini +3*cond*cond+(i-1)*cond:ind_ini-1+3*cond*cond+i*cond)=Li(i,:)  
!		Xvar(ind_ini +4*cond*cond+(i-1)*cond:ind_ini-1+4*cond*cond+i*cond)=Ci(i,:)  
!		Xvar(ind_ini +5*cond*cond+(i-1)*cond:ind_ini-1+5*cond*cond+i*cond)=LCi(i,:)
!	ENDDO
!***********************************************************




!	ind_ini=ind_ini+6*cond*cond+6*cond


!	DO i=1,cond
!		Xvar(ind_ini +3*cond*(kmax+1)+2*cond*(SIZE_T+2)+(i-1)*(SIZE_T+2)     :(ind_ini-1)+3*cond*(kmax+1)+2*cond*(SIZE_T+2)+i*(SIZE_T+2))=Evini(i,:)
!		Xvar(ind_ini +3*cond*(kmax+1)+3*cond*(SIZE_T+2)+(i-1)*(SIZE_T+2)     :(ind_ini-1)+3*cond*(kmax+1)+3*cond*(SIZE_T+2)+i*(SIZE_T+2))=Evfin(i,:)
!		write(*,*)'resta=',ind_ini +3*cond*(kmax+1)+4*cond*(SIZE_T+2)+(kmax+1)*(i-1)*(SIZE_T+2)+(k-1)*(SIZE_T+2)-((ind_ini-1)+3*cond*(kmax+1)+4*cond*(SIZE_T+2)+(kmax+1)*(i-1)*(SIZE_T+2)+k*(SIZE_T+2))
!		Do k=1,kmax+1
!		Xvar(ind_ini +3*cond*(kmax+1)+4*cond*(SIZE_T+2)+(kmax+1)*(i-1)*(SIZE_T+2)+(k-1)*(SIZE_T+2):(ind_ini-1)+3*cond*(kmax+1)+4*cond*(SIZE_T+2)+(kmax+1)*(i-1)*(SIZE_T+2)+k*(SIZE_T+2))=Ex(i,k,:)
		
!		End DO
!	ENDDO
DEALLOCATE (Zc,Zci,Ex,EH,EZ,Evfin,Evini,Cm,Lm,Ci,Li,LCi,LC,D1,D3i,D2,D4i, &
	D5,Ro,Rf,hm,Xi,hm_s,Xi_s,Rm,D7,D6,D3,D4,STAT=ALLOC_ERR)

END SUBROUTINE

!******************************************************************************************
!				CALCULATING LINE PARAMETERS
!******************************************************************************************
SUBROUTINE LINEPARAM(cond,Xi,hm,rc,Lm,Cm,Zc)
	INTEGER cond,i,j
    DOUBLE PRECISION Lm(cond,cond),Cm(cond,cond),Zc(cond,cond),Xi(cond),hm(cond),s,e0,pi,mu,c,rc

	
	PARAMETER (pi=3.141592653589793D0)
	PARAMETER (mu=(4*pi)*1E-7)
	PARAMETER (c=2.99792458E8)
	PARAMETER (e0=1/(mu*c**2))


	DO 2, i=1,cond

      DO 1, j=1,cond

            s=((hm(i)-hm(j))**2+(Xi(i)-Xi(j))**2)**.5 !computing the space between conductors
            IF (i.EQ.j) THEN
               Lm(i,i)=mu/(2*pi)*log(2*hm(i)/rc)
            ELSE
               Lm(i,j)=mu/(4*pi)*log(1+4*hm(i)*hm(j)/(s*s))
            END IF

1         ENDDO

2     ENDDO

	  
      CALL inv(Lm,cond,cond,Cm)

	Cm=Cm*mu*e0
	Zc=Lm/(mu*e0)**0.5 !Calculating surge impedance matrix
	
	RETURN      
END SUBROUTINE



SUBROUTINE OPEN_LINE_FILE(rc,XA0,YA0,XB0,YB0,hm,Rm,Xi,cond,indarchiv)
!DEC$ ATTRIBUTES, DLLEXPORT::OPEN_LINE_FILE

INTEGER cond, ERRNUM,i
DOUBLE PRECISION rc,XA0,YA0,XB0,YB0,Rm(cond,cond),hm(cond),Xi(cond)

CHARACTER (LEN=3) indarchiv
    Rm(:,:)=0
    hm(:)=0
    Xi(:)=0
    
  OPEN (UNIT = 10, FILE = 'linea_'//indarchiv//'.txt', STATUS = 'OLD',ERR=1010,IOSTAT=ERRNUM)
	  READ   (10, *) rc
	  READ   (10, *) XA0,YA0
      READ   (10, *) XB0,YB0
      READ   (10, *) hm(1),Rm(1,1)
	  READ   (10, *) Xi(1)
        Rm(1,1)=Rm(1,1)/1000
!	  READ   (10, *) Rm(i,i) !Conductor Resistance ohm/km
!		Rm(1,1)=0.000
	  IF (cond .GT. 1) THEN
		DO i=2,cond
			READ   (10, *) hm(i),Rm(i,i)  !Heigh for multiple conductors
			READ   (10, *) Xi(i)  !Horizontal Relative distance for multiple conductors
            Rm(i,i)=Rm(i,i)/1000 !Considering Resistance per meter it is read as resistance per km
		END DO

	  ENDIF
1010 SELECT CASE(ERRNUM>0)
	    CASE (.TRUE.)
			write(*,*) 'error leyendo linea_'//indarchiv//'.txt'
			rc=-1
		ENDSELECT 

  CLOSE (UNIT=10)
END SUBROUTINE



SUBROUTINE OPEN_MISC_FILE (v,zlam,Hcan,tmax1,SIZE_2,conductividad,o,e,dx,triangular,dist_camp)
!DEC$ ATTRIBUTES, DLLEXPORT::OPEN_MISC_FILE

DOUBLE PRECISION, INTENT(OUT) :: v,zlam,Hcan,tmax1,o,e,dx,dist_camp
INTEGER, INTENT(OUT) :: conductividad, SIZE_2
INTEGER :: i, ERRNUM
DOUBLE PRECISION :: SIZE_2D
LOGICAL, INTENT(OUT) :: triangular
	  WRITE(*,*) 'Reading miscelaneo.txt'
      OPEN (UNIT = 8, FILE = 'miscelaneo.txt', STATUS = 'OLD', ERR=1008,IOSTAT=ERRNUM)
      WRITE(*,*) 'v: ',v
	  READ   (8, *) v
	  WRITE(*,*) 'R v: ',v
	  READ   (8, *) zlam
	  WRITE(*,*) 'zlam: ',zlam
      READ   (8, *) Hcan
	  WRITE(*,*) 'Hcan: ',Hcan
      READ   (8, *) tmax1
	  WRITE(*,*) 'tmax1: ',tmax1, 'SIZE_2: ',SIZE_2
      READ   (8, *) SIZE_2
	  WRITE(*,*) 'SIZE_2: ',SIZE_2
	  !SIZE_2=int(SIZED)
      READ   (8, *) conductividad
	  WRITE(*,*) 'Conductividad: ',Conductividad
      READ   (8, *) o
	  WRITE(*,*) 'o: ',o
      READ   (8, *) e
	  WRITE(*,*) 'e: ',e
	  READ   (8, *) dx
	  WRITE(*,*) 'dx: ',dx
	  
	  ! IF (.NOT. EOF(8)) THEN
	  !		
	  !  	READ   (8, *) i ! waveform shape
	  ! 	IF (.NOT. EOF(8)) THEN
	  !			READ   (8, *) dist_camp  !Maximum distance for calculating EM Fields
	  !		ELSE
	  !			dist_camp=5000 !default value 5000m 
	  !		ENDIF
	  !	ELSE
	  !		write(*,*) 'Usando formato de miscelaneo antiguo'
	  !		i=0
	  !		dist_camp=5000.
	  !	END IF

      nline = 0
!	  DO while (.true.)
		nline = nline + 1
		!SELECT CASE(nline)
		!	CASE(1)
		!	READ   (8, *, end=1006) v
		!	CASE(2)
		!	READ   (8, *, end=1006) zlam
		!	CASE(3)
		!	READ   (8, *, end=1006) Hcan
		!	CASE(4)
		!	READ   (8, *, end=1006) tmax1
		!	CASE(5)
		!	READ   (8, *, end=1006) SIZE_2
		!	CASE(6)
		!	READ   (8, *, end=1006) Conductividad
		!	CASE(7)
		!	READ   (8, *, end=1006) o
		!	CASE(9)
		!	READ   (8, *, end=1006) e
		!	CASE(10)
	!DO while (.true.)
			i = 0
			dist_camp=5000			
			READ   (8, *, end=1007) i ! waveform shape
			READ   (8, *, end=1007) dist_camp  !Maximum distance for calculating EM Fields
		!END SELECT 
	  !ENDDO
	  1006 continue
	  write(*,*) 'Usando formato de miscelaneo antiguo'
	  1007 continue

	  IF (i .EQ. 1) THEN 
	  	triangular=1
	  ELSE
	  	triangular=0
	  ENDIF

		
1008 SELECT CASE(ERRNUM>0)
	    CASE (.TRUE.)
			write(*,*) 'error abriendo el archivo miscelaneo.txt error:',ERRNUM
			write(*,*) 'Cargando valores por defecto'

    		v = 1.3D8	!	- return stroke velocity
	    	zlam = 2000		!- Atenuation for MTL model use 0 for TL model
		    Hcan = 8000		!- Channel Height
    		tmax1= 3.d-6	!- simulation time for first window
	    	SIZE_2=30		!- Number of samples for the second window
		    Conductividad=1	!- Use 1(yes) to consider Conductivity
    		o=1.D-3			!- Ground Conductivity
	    	e=10			!- Relative ground permitivity
		    dx=10			!- space division if doesn't fulfill Courant criteria it is atumatically computed
!		----------------------------------------------------------------
		END SELECT
CLOSE (UNIT=8)

END SUBROUTINE

SUBROUTINE OPEN_CORR_FILE(Ih1,Ih2,tao11,tao21,tao12,tao22,n1,n2,X0,Y0,ncase_s)
!DEC$ ATTRIBUTES, DLLEXPORT::OPEN_CORR_FILE


DOUBLE PRECISION Ih1,Ih2,tao11,tao21,tao12,tao22,n1,n2,X0,Y0
CHARACTER (LEN=5) ncase_s
INTEGER ERRNUM

 OPEN (UNIT = 9, FILE = 'corr_'//ncase_s//'.txt', STATUS = 'OLD', ERR=1009,IOSTAT=ERRNUM)
      READ   (9, *) Ih1
      READ   (9, *) Ih2
      READ   (9, *) tao11
      READ   (9, *) tao21
      READ   (9, *) tao12
      READ   (9, *) tao22
      READ   (9, *) n1
      READ   (9, *) n2
      READ   (9, *) X0
      READ   (9, *) Y0      

1009 SELECT CASE(ERRNUM>0)
	    CASE (.TRUE.)
			write(*,*) 'error leyendo corr_'//ncase_s//'.txt'
		END SELECT 

  CLOSE (UNIT=9)

  END SUBROUTINE