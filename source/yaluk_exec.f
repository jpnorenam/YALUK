!*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!*                                                              %
!*                        YALUK-ATP CODE                        %
!*                                                              %
!*   - EXECUTION CODE											%
!      Condiciones de Frontera se realiza con una linea tipo
!		sin pérdidas con inducción electromagnética
!	- Sin leer datos                                            %
!*   - Link with FOREIGN MODELS (Subroutine)                    %
!*                                                              %
!*                                                              %
!*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****************************************************************
!*                INPUT AND OUTPUT DATA DESCRIPTION             *
!*                                                              *
!*		Xdata(1):timestep   (Tiempo de Execución)               *
!*		Xdata(2):timestop										*
!*		Xdata(3): número de conductores                         *
!*		Xdata(4):Num_lin										*
!*		Xvar(1): kmax											*
!*		Xvar(2): SIZE											* 
!*		Xvar(3):dx												*
!*		Xvar(4):o												*
!*		Xvar(5):e												*
!*		Xvar(6)=conductividad									*           
!*		Xvar(7):t0												*
!*		Xvar(8)=dx2												*
!*      Xvar(9)=nlin	!numero total de líneas					*
!*      Xvar(10):  numero máximo divisiones						*
!*      Xvar(20)=ind_ini1 !initial variable for saving h values
!*		Xin(1):  t    (Tiempo de Execución)                     *
!*		Xin(2):  Vrini(1)										*
!*		Xin(3):  Vrfin(1)										*
!*		Xin(4):  Vrini(2)										*
!*		Xin(5):  Vrfin(2)										*
!*		Xin(6):  Vrini(3)										*
!*		Xin(7):  Vrfin(3)										*
!*		Xin(8):  Vrini(4)										*
!*		Xin(9):  Vrfin(4)										*
!*      Xvar(10):maximum number of division in x				*
!*      Xvar(11):maximum number of conductors                   *
!*		Xout(1): V1ini(1)                                       *
!*		Xout(2): V1fin(1)										*
!*		Xout(3): V1ini(2)                                       *
!*		Xout(4): V1fin(2)										*
!*		Xout(5): V1ini(3)                                       *
!*		Xout(6): V1fin(3)										*
!*		Xout(7): V1ini(4)                                       *
!*		Xout(8): V1fin(4)										*
!****************************************************************
SUBROUTINE yaluk_exec(xdata,xin,xout,xvar) 
!DEC$ ATTRIBUTES STDCALL, DLLEXPORT::yaluk_exec

!use, intrinsic:: iso_fortran_env, only: stdin=>input_unit
!USE IFPORT
!USE DFLIB !visual fortran 6.0
	INTEGER SIZE, k,g,n,cond,  kmax, 					&
     conductividad,ALLOC_ERR,ERRNUM,cond_s,kmaxt,max_lin,num_lin,cond_max,       &
     ind_ini,ind_ini1,i,ind_f,SIZE_2,SIZE3,SIZE_ini              !Data used for interpolation
	DOUBLE PRECISION xin(*), xout(*), xvar(*),xdata(*)
	DOUBLE PRECISION dt, tmax,c,t,o,e,ti,pi,n_VR, 	&
         mu,e0,taog,F,ferrc,valmin,Fe,EE,dx2,dx,dt_s,tmax_s,t0,t0_min,           &
         Evn_ini(2),Evn_fin(2),ind,ind2,ind3,ind4,dt3                    !data for making interpolation
!	INTEGER(2) ihr, imin, isec, i100th,ihr2, imin2, isec2, i100th2	!variables de tiempo
!     DOUBLE PRECISION, ALLOCATABLE :: h(:),Xi(:),					&
!		Ci(:,:),Li(:,:),LCi(:,:),D3i(:,:),D5(:,:),					&
!		Evini(:,:),Evfin(:,:),Evini2(:),Evfin2(:), Ex(:,:,:),		&
!		A1(:,:),A2(:,:),Vp(:,:),Vpa(:,:),VR(:,:),dI(:,:,:),dI_p(:,:),			&
!    	A3(:,:),A4(:,:),B1(:,:),B2(:,:),B3(:,:),Zc(:,:),Zci(:,:),	&
!     	B4(:,:),B5(:,:),B6(:,:),Vant(:,:),Iant(:,:),				&
!		Vi(:,:),Ii(:,:),Vrini(:,:),Vrfin(:,:),V1ini(:),V1fin(:)

DOUBLE PRECISION, SAVE, ALLOCATABLE :: h(:),					&
		D3ic(:,:,:),D5c(:,:,:),					& !matrices constantes para cada línea
		Evini(:,:),Evfin(:,:),Ex(:,:,:,:),		&
		Zc_t(:,:,:),Zci_t(:,:,:),& !Surge impedance for all lines
		dI(:,:,:,:),Vp(:,:,:),Vpa(:,:,:),	&
		VR(:,:,:),Vant(:,:,:),Iant(:,:,:), &
		Mi1c(:,:,:),Mi2c(:,:,:),Mi3c(:,:,:),Mi4c(:,:,:),Mv1c(:,:,:),Mv2c(:,:,:),Mv3c(:,:,:) !Matrices multiplican tensiones y corrientes para cada linea
DOUBLE PRECISION, ALLOCATABLE :: Evini2(:),Evfin2(:),Ex2(:,:,:),&
		Exn(:,:),Exn_1(:,:),Exn_2(:,:),Exn_1p(:,:),&
		A3(:,:),A4(:,:),B1(:,:),B2(:,:),B3(:,:),B4(:,:),B5(:,:),B6(:,:),A1(:,:),A2(:,:),A5(:,:),A6(:,:),Vrini(:,:),Vrfin(:,:),V1ini(:),V1fin(:),&
		B7(:,:),Vi(:,:),Ii(:,:),dI_p(:,:),Mi1(:,:),Mi2(:,:),Mi3(:,:),Mi4(:,:),Mv1(:,:),Mv2(:,:),Mv3(:,:),D3i(:,:),D5(:,:),Zc(:,:),Zci(:,:)


	CHARACTER(LEN=3) indarchiv
	CHARACTER (LEN=5) ncase_s
	CHARACTER(LEN=30) casename
	EXTERNAL ferrc

		
	PARAMETER (pi=3.141592653589793D0)
	PARAMETER (mu=(4*pi)*1E-7)
	PARAMETER (c=2.99792458E8)
	PARAMETER (e0=1/(mu*c**2))


!******************************************************************************
	!Loading Variables from init model
!******************************************************************************

	kmax=nint(Xvar(1))											
	SIZE=nint(Xvar(2))
	dx=Xvar(3)											
	o=Xvar(4)
	e=Xvar(5)
	conductividad=Xvar(6)
	t0=Xvar(7)
	dx2=Xvar(8)	
	SIZE_2=nint(Xvar(12)) !reading size for matrix reduced
    SIZE3=nint(Xvar(13)) !matrix size without interpolation
    SIZE_ini=nint(Xvar(15))
    dt3=Xvar(14)
	!Loading Data from the ATP - case
	dt=Xdata(1)             !delta time
	tmax=Xdata(2)           !maximum simulation time
	cond=nint(Xdata(3))     ! number of conductors for this line
	num_lin=nint(Xdata(4))  !current line number
	t=Xin(1)
	n=nint((t/dt))+1        !time index number
	IF (n.LT.1) THEN
		n=1
	ENDIF				
	ind_ini1=nint(Xvar(20)) !index for reading h
	kmaxt=Xvar(10)	        ! kmaxt; número máximo de divisiones en las líneas
	IF (Xvar(9).NE.0) THEN
	    max_lin=Xvar(9)	    ! max_lin: numero máxmio de líneas
    ELSE
        max_lin=40          !número de líneas por defecto
    END IF
    cond_max=Xvar(11)       !máximo numero de conductores
!******************************************************************************
!******************************************************************************
	100 FORMAT(I3.3) 
	WRITE(indarchiv,FMT=100) int(Xdata(4)) !writing line number to string

    IF (.NOT. ALLOCATED(EX)) THEN
	ALLOCATE (D3ic(cond_max,cond_max,max_lin),D5c(cond_max,cond_max,max_lin),Zc_t(cond_max,cond_max,max_lin),Zci_t(cond_max,cond_max,max_lin),	&				
		STAT=ALLOC_ERR)
		!write(*,*) 'Err ',ALLOC_ERR
		ALLOC_ERR=0
	ALLOCATE	(Ex(cond_max,kmaxt,SIZE_2+2,max_lin),Evini(SIZE_2+2,max_lin),Evfin(SIZE_2+2,max_lin),				&				
		dI(SIZE+2,cond_max,kmaxt,max_lin),	&
		VR(cond_max,SIZE+2,max_lin),Vant(cond_max,1:kmaxt,max_lin),Iant(cond_max,1:kmaxt,max_lin),&
		Mi1c(cond_max,cond_max,max_lin),Mi2c(cond_max,cond_max,max_lin),Mi3c(cond_max,cond_max,max_lin),Mi4c(cond_max,cond_max,max_lin),Mv1c(cond_max,cond_max,max_lin),Mv2c(cond_max,cond_max,max_lin),Mv3c(cond_max,cond_max,max_lin),&
		Vpa(cond_max,kmaxt,max_lin),Vp(cond_max,kmaxt,max_lin),STAT=ALLOC_ERR)

        if (ALLOC_ERR .EQ. 179) THEN 
		    print *, 'error en allocate - overflow array'
		ENDIF
		
    ENDIF
		!Evini2,Evfin2,Ex2,D3i,D5,h,Zc,Zci,Mv1,Mv2,Mv3,Mi1,Mi2,Mi3,Mi4
		
		ALLOC_ERR=0
	ALLOCATE (h(cond),Ex2(cond,kmax+1,SIZE_2+2),Evini2(SIZE_2+2),Evfin2(SIZE_2+2),  &
		Exn(cond,kmax+1),Exn_1(cond,kmax+1),Exn_2(cond,kmax+1),Exn_1p(cond,kmax+1),& !matrix for replacing and interpolation
		A1(cond,1),A2(cond,1),A5(cond,1),A6(cond,1),&
     	A3(cond,1),A4(cond,1),B1(cond,1),B2(cond,1),B3(cond,1),Vi(cond,kmax+1),Ii(cond,kmax+1),								&
     	B4(cond,1),B5(cond,1),B6(cond,1),B7(cond,1),dI_p(SIZE+2,cond),&
		Mv1(cond,cond),Mv2(cond,cond),Mv3(cond,cond),Mi1(cond,cond),Mi2(cond,cond),Mi3(cond,cond),Mi4(cond,cond),&
		D3i(cond,cond),D5(cond,cond),Zc(cond,cond),Zci(cond,cond),&
		Vrini(cond,2),V1ini(cond),Vrfin(cond,2),V1fin(cond),& !tensiones que pasan de linea valor actual, valor pasado
		STAT=ALLOC_ERR)
		
		if (ALLOC_ERR .GT. 0) THEN 
		    print *, 'error en allocate - presione una tecla para continuar'
			!read(stdin,*)
		ENDIF


! CARGANDO CAMPOS
!CALL GETTIM (ihr, imin, isec, i100th)

!Cm	***************************************************************
!Cm	STARTING	Calculation of Conductivity Effect on Agrawal Model
!Cm	---------------------------------------------------------------
!	IF (t .LE. t0-dt .AND. t .GT. 0) THEN
!	open(UNIT = 17, FILE = 'temp_VR_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1017,IOSTAT=ERRNUM)
!	READ(UNIT = 17) VR !Vant,Iant,Vi,Ii,Vrfin,Vrini,V1ini,V1fin,Vp,VR
!	close (UNIT = 17)

!1017 SELECT CASE(ERRNUM>0)
!			CASE (.TRUE.)
!				write(*,*) 'Error saving leyendo ten-corr_'//indarchiv//'.dat  ',ERRNUM
!	ENDSELECT
!			ERRNUM=0
!			n_VR=0   ! valor para el control de escritura de la variable VR
!	ENDIF

h(1:cond)=Xvar(ind_ini1:ind_ini1-1+cond)

IF (t.EQ.0) THEN
	VR(:,:,num_lin)=0.D0
ELSE
	IF (conductividad .EQ. 1) THEN
		ind_ini=ind_ini1+7*cond+6*cond*cond
			DO g=1,cond
				taog=h(g)*h(g)*mu*o
				Fe=ferrc(dsqrt(taog/(t)))
				EE=dexp(taog /(t))
				F=mu/(pi*taog)*(0.5*pi**(-0.5)*dsqrt(taog /(t))+0.25*EE*Fe-0.25)
				valmin=1/(2*pi*h(g))*dsqrt(mu/(e0*e))
				VR(g,1,num_lin)=min(valmin,F)
			END DO
			!VR(g,1,num_lin)
	ELSE
		VR(:,:,num_lin)=0.D0
	ENDIF

ENDIF


IF (n .LE. 1) THEN
	OPEN (UNIT = 11, FILE = 'status_file.ylk', FORM='UNFORMATTED', STATUS = 'OLD', ERR=1011,IOSTAT=ERRNUM)
	READ (UNIT=11) dt_s,tmax_s,ncase_s,casename,t0_min
	
	CLOSE (UNIT=11)
1011 SELECT CASE(ERRNUM>0)
	    CASE (.TRUE.)
			write(*,*) 'error leyendo status_file.ylk',ERRNUM
		ENDSELECT 

	OPEN(UNIT = 12, FILE = ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1004,IOSTAT=ERRNUM)
	READ(12) Evini2,Evfin2,Ex2,D3i,D5,Zc,Zci,Mv1,Mv2,Mv3,Mi1,Mi2,Mi3,Mi4
	
	Mv1c(1:cond,1:cond,num_lin)=Mv1(1:cond,1:cond)
	Mv2c(1:cond,1:cond,num_lin)=Mv2(1:cond,1:cond)
	Mv3c(1:cond,1:cond,num_lin)=Mv3(1:cond,1:cond)
	Mi1c(1:cond,1:cond,num_lin)=Mi1(1:cond,1:cond)
	Mi2c(1:cond,1:cond,num_lin)=Mi2(1:cond,1:cond)
	Mi3c(1:cond,1:cond,num_lin)=Mi3(1:cond,1:cond)
	Mi4c(1:cond,1:cond,num_lin)=Mi4(1:cond,1:cond)
	D3ic(1:cond,1:cond,num_lin)=D3i(1:cond,1:cond)
	D5c(1:cond,1:cond,num_lin)=D5(1:cond,1:cond)
	Zc_t(1:cond,1:cond,num_lin)=Zc(1:cond,1:cond)
	Zci_t(1:cond,1:cond,num_lin)=Zci(1:cond,1:cond)
	close(12)
	Ex(1:cond,1:kmax+1,1:SIZE_2+2,num_lin)=Ex2(1:cond,1:kmax+1,1:SIZE_2+2)
	Evini(1:SIZE_2+2,num_lin)=Evini2(1:SIZE_2+2)
	Evfin(1:SIZE_2+2,num_lin)=Evfin2(1:SIZE_2+2)


1004 SELECT CASE(ERRNUM>0)
	    CASE (.TRUE.)
			write(*,*) 'error leyendo ',ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat',ERRNUM
			stop
		ENDSELECT 
END IF

IF (t0 .LT. dt) THEN
 t0=1.001*dt
ENDIF

!IF (t .GE. t0-dt) THEN 
IF (t .GE. t0) THEN
!write(*,*) 'Mi4=',Mi4
!**********************************************************************
!	Loading Init Values and Casename
!----------------------------------------------------------------------


!	OPEN (UNIT = 11, FILE = 'status_file.ylk', FORM='UNFORMATTED', STATUS = 'OLD', ERR=1011,IOSTAT=ERRNUM)
!	READ (UNIT=11) dt_s,tmax_s,cond_s,ncase_s,casename
!	CLOSE (UNIT=11)

!1011 SELECT CASE(ERRNUM>0)
!	    CASE (.TRUE.)
!			write(*,*) 'error leyendo status_file.ylk',ERRNUM
!		ENDSELECT 
!ERRNUM=0
!	OPEN(UNIT = 12, FILE = ncase_s//'_'//casename(1:LEN_TRIM(casename))//'_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1004,IOSTAT=ERRNUM)
!	READ(12) Evini,Evfin,Ex,Li,Ci,LCi,D3i,D5,h,Zc,Zci
!	close(12)
!1004 SELECT CASE(ERRNUM>0)
!	    CASE (.TRUE.)
!			write(*,*) 'error leyendo yalukp.dat  ',ERRNUM
!			stop
!		ENDSELECT 

!IF (n.GE.2) THEN
!	open(UNIT = 15, FILE = 'ten-corr_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1006,IOSTAT=ERRNUM)
!	read(15) dI,Vant,Iant,Vp,Vpa,VR !,Vant,Iant,Vi,Ii,Vrfin,Vrini,V1ini,V1fin,Vp,VR
!	close(15)
!ELSE
!	open(UNIT = 15, FILE = 'ten-corr_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1006,IOSTAT=ERRNUM)
!	write(15) Vant,Iant,Vi,Ii,Vrfin,Vrini,V1ini,V1fin,Vp,VR,dI
!	close(15)
!END IF
!	IF (n_VR .EQ. 0) THEN
!	open(UNIT = 18, FILE = 'temp_VR_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1018,IOSTAT=ERRNUM)
!	write(UNIT = 18) VR !Vant,Iant,Vi,Ii,Vrfin,Vrini,V1ini,V1fin,Vp,VR
!	close (UNIT = 18)
	n_VR=1
!1018 SELECT CASE(ERRNUM>0)
!			CASE (.TRUE.)
!			write(*,*) 'Error saving leyendo ten-corr_'//indarchiv//'.dat  ',ERRNUM
!			stop
!	ENDSELECT
!			ERRNUM=0 
!	ENDIF



!1006 SELECT CASE(ERRNUM>0)
!	    CASE (.TRUE.)
!			write(*,*) 'error leyendo ten-corr.dat  ',ERRNUM
!		ENDSELECT 


	ind_ini=ind_ini1+cond

!	write(*,*)'LCi',LCi
!	write(*,*)'LCi2',Xvar(ind_ini +5*cond*cond:ind_ini-1+6*cond*cond)

!	DO i=1,cond
!		D3i(i,:) =Xvar(ind_ini             +(i-1)*cond:ind_ini-1            +i*cond)
!		D5(i,:)  =Xvar(ind_ini +  cond*cond+(i-1)*cond:ind_ini-1+  cond*cond+i*cond)
!		Zc(i,:)  =Xvar(ind_ini +2*cond*cond+(i-1)*cond:ind_ini-1+2*cond*cond+i*cond)
!		Li(i,:)  =Xvar(ind_ini +3*cond*cond+(i-1)*cond:ind_ini-1+3*cond*cond+i*cond)
!		Ci(i,:)  =Xvar(ind_ini +4*cond*cond+(i-1)*cond:ind_ini-1+4*cond*cond+i*cond)
!		LCi(i,:) =Xvar(ind_ini +5*cond*cond+(i-1)*cond:ind_ini-1+5*cond*cond+i*cond)
!	ENDDO
	ind_ini=ind_ini+6*cond*cond
	
!**********************************************************************
!	Defining Voltages for use
!----------------------------------------------------------------------

	V1ini(1:cond)  =Xvar(ind_ini       :ind_ini-1+  cond)
	V1fin(1:cond)  =Xvar(ind_ini+  cond:ind_ini-1+2*cond)
	Vrini(1:cond,1)=Xvar(ind_ini+2*cond:ind_ini-1+3*cond)
!	Vrini(1:cond,2)=Xvar(ind_ini+3*cond:ind_ini-1+4*cond)
	Vrfin(1:cond,1)=Xvar(ind_ini+4*cond:ind_ini-1+5*cond)
!	Vrfin(1:cond,2)=Xvar(ind_ini+5*cond:ind_ini-1+6*cond)
!	write(*,*)'V1ini=',V1ini(1:cond)
!	write(*,*)'V1ini=',Xvar(ind_ini:ind_ini-1+cond)
	ind_ini=ind_ini+6*cond
	!DO i=1,cond
!		Vant(i,:)= Xvar(ind_ini +                (i-1)*(kmax+1)                       :(ind_ini-1)+            i*(kmax+1))	                 
!		Iant(i,:)= Xvar(ind_ini +  cond*(kmax+1)+(i-1)*(kmax+1)                       :(ind_ini-1)+  cond*(kmax+1)+i*(kmax+1))                
!		Vp(i,:)  = Xvar(ind_ini +2*cond*(kmax+1)+(i-1)*(kmax+1)                     :(ind_ini-1)+2*cond*(kmax+1)+i*(kmax+1))                
!		Vpa(i,:) = Xvar(ind_ini +3*cond*(kmax+1)+(i-1)*(kmax+1)                     :(ind_ini-1)+3*cond*(kmax+1)+i*(kmax+1))  
!		dI(:,i)  = Xvar(ind_ini +4*cond*(kmax+1)+  cond*(SIZE+2)+(i-1)*(SIZE+2)     :(ind_ini-1)+4*cond*(kmax+1)+  cond*(SIZE+2)+i*(SIZE+2))
!		Evini(i,:)= Xvar(ind_ini +3*cond*(kmax+1)+2*cond*(SIZE+2)+(i-1)*(SIZE+2)     :(ind_ini-1)+3*cond*(kmax+1)+2*cond*(SIZE+2)+i*(SIZE+2))
!		Evfin(i,:)= Xvar(ind_ini +3*cond*(kmax+1)+3*cond*(SIZE+2)+(i-1)*(SIZE+2)     :(ind_ini-1)+3*cond*(kmax+1)+3*cond*(SIZE+2)+i*(SIZE+2))
!	  Do k=1,kmax+1
!		Ex(i,k,:)= Xvar(ind_ini +3*cond*(kmax+1)+4*cond*(SIZE+2)+(i-1)*(kmax+1)*(SIZE+2)+(k-1)*(SIZE+2):(ind_ini-1)+3*cond*(kmax+1)+4*cond*(SIZE+2)+(i-1)*(kmax+1)*(SIZE+2)+k*(SIZE+2))
!	  END DO
	!ENDDO


	Vrini(1:cond,2)=Xin(2:cond+1)
	Vrfin(1:cond,2)=Xin(cond+2:2*cond+1)
! Calculating generators for bounding conditions

!*******************************************************************************************	
!*******************************************************************************************	
	ti=0.D0
!Cm	Ii=0
!Cm	Vi=0
!Cm	Vp=0
!Cm	dI=0
	Mv1(1:cond,1:cond)=Mv1c(1:cond,1:cond,num_lin)
	Mv2(1:cond,1:cond)=Mv2c(1:cond,1:cond,num_lin)
	Mv3(1:cond,1:cond)=Mv3c(1:cond,1:cond,num_lin)
	Mi1(1:cond,1:cond)=Mi1c(1:cond,1:cond,num_lin)
	Mi2(1:cond,1:cond)=Mi2c(1:cond,1:cond,num_lin)
	Mi3(1:cond,1:cond)=Mi3c(1:cond,1:cond,num_lin)
	Mi4(1:cond,1:cond)=Mi4c(1:cond,1:cond,num_lin)
	D3i(1:cond,1:cond)=D3ic(1:cond,1:cond,num_lin)
	D5(1:cond,1:cond)=D5c(1:cond,1:cond,num_lin)  
	Zc(1:cond,1:cond)=Zc_t(1:cond,1:cond,num_lin)  
	Zci(1:cond,1:cond)=Zci_t(1:cond,1:cond,num_lin)  

    !*************************************************
    !INTERPOLATION PROCESS
    !-------------------------------------------------
    !  Exn :Exn for time n  for all space divisions
    !  Exn :Exn_1 for time n-1 for all space divisions
    !  Exn :Exn_2 for time n-2 for all space divisions
    !  Exn :Exn_1p for time n+1 for all space divisions
    !  Evn_ini(1) for time n
    !  Evn_ini(2) for time n+1
    !  Evn_fin(1) for time n
    !  Evn_fin(2) for time n+1
    !**********************************************************
        IF (n.GT.SIZE3) THEN
            ind=(n-SIZE3)*dt/dt3
            ind2=(n-1-SIZE3)*dt/dt3
            ind3=(n-2-SIZE3)*dt/dt3
            ind4=(n+1-SIZE3)*dt/dt3
            if (n.GT.SIZE-dt3/dt-1) THEN
                ind=(n-SIZE3)*dt/dt3-2
            endif
                ind_f=floor(ind)

            Exn(1:cond,1:kmax+1) =  Ex(1:cond,1:kmax+1,SIZE3+ind_f,num_lin)+(Ex(1:cond,1:kmax+1,SIZE3+ind_f+1,num_lin)-Ex(1:cond,1:kmax+1,SIZE3+ind_f,num_lin))*(ind-ind_f)
            Exn_1(1:cond,1:kmax+1) = Exn-(Ex(1:cond,1:kmax+1,SIZE3+ind_f+1,num_lin)-Ex(1:cond,1:kmax+1,SIZE3+ind_f,num_lin))*(ind2-ind_f)
            Exn_2(1:cond,1:kmax+1) = Exn-(Ex(1:cond,1:kmax+1,SIZE3+ind_f+1,num_lin)-Ex(1:cond,1:kmax+1,SIZE3+ind_f,num_lin))*(ind3-ind_f)
            Exn_1p(1:cond,1:kmax+1) = Exn+(Ex(1:cond,1:kmax+1,SIZE3+ind_f+1,num_lin)-Ex(1:cond,1:kmax+1,SIZE3+ind_f,num_lin))*(ind4-ind_f)

            Evn_ini(1) = Evini(SIZE3+ind_f,num_lin)+(Evini(SIZE3+ind_f+1,num_lin)-Evini(SIZE3+ind_f,num_lin))*(ind-ind_f)
            Evn_ini(2) = Evini(SIZE3+ind_f,num_lin)+(Evini(SIZE3+ind_f+1,num_lin)-Evini(SIZE3+ind_f,num_lin))*(ind4-ind_f)
            Evn_fin(1) = Evfin(SIZE3+ind_f,num_lin)+(Evfin(SIZE3+ind_f+1,num_lin)-Evfin(SIZE3+ind_f,num_lin))*(ind-ind_f)
            Evn_fin(2) = Evfin(SIZE3+ind_f,num_lin)+(Evfin(SIZE3+ind_f+1,num_lin)-Evfin(SIZE3+ind_f,num_lin))*(ind4-ind_f)
         ELSE
            Exn(1:cond,1:kmax+1)=  Ex(1:cond,1:kmax+1,n,num_lin)
            Exn_1(1:cond,1:kmax+1) =  Ex(1:cond,1:kmax+1,n-1,num_lin)
            Exn_2(1:cond,1:kmax+1) =  Ex(1:cond,1:kmax+1,n-2,num_lin)
            Exn_1p(1:cond,1:kmax+1) =  Ex(1:cond,1:kmax+1,n+1,num_lin)
            Evn_ini = Evini(n:n+1,num_lin)
            Evn_fin = Evfin(n:n+1,num_lin)
        ENDIF
    !--------------------------------------------------
    ! ENDING INTERPOLATION
    !***************************************************

	DO 11,k=3,(kmax-1)

!		A1(:,1)=-dt*(Iant(:,k+1,num_lin)-Iant(:,k-1,num_lin))/(dx+dx)
!		A2(:,1)=0.5*dt*dt*(-((Ex(:,k+1,n-1,num_lin)-Ex(:,k-1,n-1,num_lin))/(dx+dx)			&
!     			-(Vant(:,k+1,num_lin)+Vant(:,k-1,num_lin)-2*Vant(:,k,num_lin))/(dx*dx))+		&
!     			(Vp(:,k+1,num_lin)-Vp(:,k-1,num_lin))/(dx+dx))
!		if (n .GT. 2) THEN
!		B1(:,1)=-dt*((Vant(:,k+1,num_lin)-Vant(:,k-1,num_lin))/(dx+dx)-				&
 !    		   Ex(:,k,n-1,num_lin)+Vp(:,k,num_lin)+(Vp(:,k,num_lin)-Vpa(:,k,num_lin))/2-(Ex(:,k,n,num_lin)-Ex(:,k,n-2,num_lin))*.25)
!		ELSE 
!		B1(:,1)=-dt*((Vant(:,k+1,num_lin)-Vant(:,k-1,num_lin))/(dx+dx)-				&
 !    		   Ex(:,k,n-1,num_lin)+Vp(:,k,num_lin)+(Vp(:,k,num_lin)-Vpa(:,k,num_lin))/2-(Ex(:,k,n,num_lin))*.25)
!		END IF
!		B2(:,1)=0.5*(dt*dt)*((Iant(:,k+1,num_lin)+Iant(:,k-1,num_lin)-2*Iant(:,k,num_lin))/(dx*dx))
 	    A1(1:cond,1)=-(Iant(1:cond,k+1,num_lin)-Iant(1:cond,k-1,num_lin))
		A2(1:cond,1)=(-((Exn_1(1:cond,k+1)-Exn_1(1:cond,k-1))			&
     			-2*(Vant(1:cond,k+1,num_lin)+Vant(1:cond,k-1,num_lin)-2*Vant(1:cond,k,num_lin))/(dx))+		&
     			(Vp(1:cond,k+1,num_lin)-Vp(1:cond,k-1,num_lin)))
		if (n .GT. 2) THEN
		B1(1:cond,1)=-((Vant(1:cond,k+1,num_lin)-Vant(1:cond,k-1,num_lin))/(dx+dx)-				&
     		   Exn_1(1:cond,k)+Vp(1:cond,k,num_lin)+(Vp(1:cond,k,num_lin)-Vpa(1:cond,k,num_lin))*.5-(Exn(1:cond,k)-Exn_2(1:cond,k))*.25)
		ELSE 
		B1(1:cond,1)=-((Vant(1:cond,k+1,num_lin)-Vant(1:cond,k-1,num_lin))/(dx+dx)-				&
     		   Exn_1(1:cond,k)+Vp(1:cond,k,num_lin)+(Vp(1:cond,k,num_lin)-Vpa(1:cond,k,num_lin))*.5-(Exn(1:cond,k))*.25)
		END IF
		B2(1:cond,1)=((Iant(1:cond,k+1,num_lin)+Iant(1:cond,k-1,num_lin)-2*Iant(1:cond,k,num_lin)))
		B3(1:cond,1)=-((Vant(1:cond,k+1,num_lin)-Vant(1:cond,k-1,num_lin))/(dx+dx)-				&
     		   Exn_1(1:cond,k)+Vp(1:cond,k,num_lin))

		A3=matmul(Mv1,A1)
		A4=matmul(Mv2,A2)
		A5=matmul(Mv3,A1) !Considering Line Losses
		
		B4=matmul(Mi1,B1)
		B5=matmul(Mi2,B2)
		B6=matmul(Mi3,B3)  !Considering Line Losses

		



	
	!Vi(:,k)= Vant(:,k,num_lin)+A3(:,1)+A4(:,1)  !Lossless Line
	!Ii(:,k)=Iant(:,k,num_lin)+B4(:,1)+B6(:,1)	 !Lossless Line

	Vi(1:cond,k)= Vant(1:cond,k,num_lin)+A3(1:cond,1)+A4(1:cond,1)+A5(1:cond,1)
	Ii(1:cond,k)=Iant(1:cond,k,num_lin)+B4(1:cond,1)+B5(1:cond,1)+B6(1:cond,1)
	
	Ii(1:cond,k)=matmul(Mi4,Ii(1:cond,k))		 !Considering Line Losses


			
	!Calculating Ground conductivity influence on the Line
	!--------------------------------------------------------
	IF (conductividad .EQ.1) THEN
			Vpa(1:cond,k,num_lin)=Vp(1:cond,k,num_lin)
			dI(n,1:cond,k,num_lin)=(Ii(1:cond,k)-Iant(1:cond,k,num_lin))
			dI_p(1:n,1:cond)=dI(1:n,1:cond,k,num_lin)
			DO g=1,cond
				Vp(g:g,k:k,num_lin)=matmul(VR(g:g,1:n,num_lin),dI_p(1:n,g:g))-VR(g,1,num_lin)*dI_p(1,g)/2-VR(g,n,num_lin)*dI_p(n,g)/2
			END DO
	ELSE
			Vpa(1:cond,k,num_lin)=0.D0
			dI(n,1:cond,k,num_lin)=0.D0
			Vp(1:cond,k,num_lin)=0.D0
	ENDIF
11	END DO

!       MATRICES CONSTANTES PARA EL CALCULO DE DIFERENCIAS 
!                        FINITAS
!* D3i: Constante inicio línea  inv(dx/dt*C*Zc+unos)
!* D5   Constante para calculo de frontera final línea  (dx/dt*C*Zc)
!*
!*--------------------------------------------------------------
!               Calculating inducing Source 
!---------------------------------------------------------
Vrini(1:cond,2)=Vrini(1:cond,2)+Vrini(1:cond,2)-V1ini(1:cond)+Evn_ini(1)*h(1:cond)
Vrfin(1:cond,2)=Vrfin(1:cond,2)+Vrfin(1:cond,2)-V1fin(1:cond)+Evn_fin(1)*h(1:cond)

Vrini(1:cond,1)=Vrini(1:cond,1)+(Exn_1(1:cond,1)+Exn(1:cond,2))*dx2*.5 
Vrfin(1:cond,1)=Vrfin(1:cond,1)-(Exn_1(1:cond,kmax)+Exn(1:cond,kmax+1))*dx2*.5

!               Calculating Bounding Conditions
!---------------------------------------------------------
	Vi(1:cond,2)=matmul(D3i(1:cond,1:cond),(Vrini(1:cond,1)+matmul(D5(1:cond,1:cond),Vant(:,2,num_lin))-matmul(Zc(1:cond,1:cond),Ii(1:cond,3))))
	Vi(1:cond,kmax)=matmul(D3i(1:cond,1:cond),(Vrfin(1:cond,1)+matmul(D5(1:cond,1:cond),Vant(1:cond,kmax,num_lin))+matmul(Zc(1:cond,1:cond),Ii(1:cond,kmax-1))))

!               Calculating Influence of Zg on the bounding
!------------------------------------------------------------------
	Ii(1:cond,2)=matmul(Zci(1:cond,1:cond),(-Vi(1:cond,2)+Vrini(1:cond,1)))
	Ii(1:cond,kmax)=matmul(Zci(1:cond,1:cond),(Vi(1:cond,kmax)-Vrfin(1:cond,1)))
	IF (conductividad .EQ.1) THEN
		Vpa(1:cond,2,num_lin)=Vp(1:cond,2,num_lin)
		dI(n,1:cond,2,num_lin)=(Ii(1:cond,2)-Iant(1:cond,2,num_lin))
		dI_p(1:n,1:cond)=dI(1:n,1:cond,2,num_lin)		
		DO g=1,cond
			Vp(g:g,2:2,num_lin)=matmul(VR(g:g,1:n,num_lin),dI_p(1:n,g:g))-VR(g,1,num_lin)*dI_p(1,g)/2-VR(g,n,num_lin)*dI_p(n,g)/2
		ENDDO
		dI(n,1:cond,kmax,num_lin)=(Ii(1:cond,kmax)-Iant(1:cond,kmax,num_lin))
		dI_p(1:n,1:cond)=dI(1:n,1:cond,2,num_lin)		
				Vpa(1:cond,kmax,num_lin)=Vp(1:cond,kmax,num_lin)
		DO g=1,cond
			Vp(g:g,kmax:kmax,num_lin)= matmul(VR(g:g,1:n,num_lin),dI_p(1:n,g:g))-VR(g,1,num_lin)*dI_p(1,g)/2-VR(g,n,num_lin)*dI_p(n,g)/2
		ENDDO
	ELSE
		Vpa(1:cond,kmax,num_lin)=0.D0
		Vp(1:cond,kmax,num_lin)=0.D0
		Vpa(1:cond,2,num_lin)=0.D0
		Vp(1:cond,2,num_lin)=0.D0

	ENDIF
!               Calculating Output Voltages
!---------------------------------------------------------
V1ini(1:cond)=Vi(1:cond,2)+Vi(1:cond,2) - Vrini(1:cond,1) -Evn_ini(2)*h(1:cond) -(Exn_1p(1:cond,1)+Exn(1:cond,2))*dx2*.5+Vp(1:cond,2,num_lin)
V1fin(1:cond)=Vi(1:cond,kmax)+Vi(1:cond,kmax) - Vrfin(1:cond,1) -Evn_fin(2)*h(1:cond) +(Exn_1p(1:cond,kmax+1)+Exn(1:cond,kmax))*dx2*.5+Vp(1:cond,kmax,num_lin)

!               Establishing Past Values to Iant
!---------------------------------------------------------
Iant(1:cond,1:kmax+1,num_lin)=Ii(1:cond,:)
Vant(1:cond,1:kmax+1,num_lin)=Vi(1:cond,:)

!               Establishing Past Values to input sources
!---------------------------------------------------------
Vrini(1:cond,1)=Vrini(1:cond,2)
Vrfin(1:cond,1)=Vrfin(1:cond,2)


	!----------------------------------------
	! OUTPUT
	!----------------------------------------
	Xout(1:cond)= V1ini(1:cond)
    Xout(cond+1:2*cond)= V1fin(1:cond)




!*--------------------------------------------------------------
!               Using Characteristics Method  
!--------------------------------------------__-------------
!V1(:,nmax)=Vent1(:)
!V2(:,nmax)=Vent2(:)

!Vr1(:,nmax)=2*V2(:,nmax-dtm)-Vr2(:,nmax-dtm)
!Vr2(:,nmax)=2*V1(:,nmax-dtm)-Vr1(:,nmax-dtm)

!Vr1(:,1:nmax-1)=Vr1(2:nmax)
!Vr2(:,1:nmax-1)=Vr2(2:nmax)
!V1(:,1:nmax-1)=V1(2:nmax)
!V2(:,1:nmax-1)=V2(2:nmax)


	!Definiendo Variables para la memoria de ATP (tener en cuenta que el número de variables


!open(UNIT = 14, FILE = 'ten-corr_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1014,IOSTAT=ERRNUM)
!write(UNIT = 14)  dI,Vant,Iant,Vp,Vpa,VR
!close (UNIT = 14)
!1014 SELECT CASE(ERRNUM>0)
!	    CASE (.TRUE.)
!			write(*,*) 'Error saving leyendo ten-corr_'//indarchiv//'.dat  ',ERRNUM
!		ENDSELECT 

!CALL GETTIM (ihr2, imin2, isec2, i100th2) 
!write(*,*)'hh:mm:ss.dd=',ihr2-ihr,':',imin2-imin,':',isec2-isec,'.',i100th2-i100th

ELSE

	Vi(:,:)=0.D0
	Ii(:,:)=0.D0
	V1ini(:)=0.D0
	V1fin(:)=0.D0
	Vant(:,:,num_lin)=0.D0
	Iant(:,:,num_lin)=0.D0
	Vrini(:,:)=0.D0
	Vrfin(:,:)=0.D0
	dI(:,:,:,num_lin)=0.D0
	Vp(:,:,num_lin)=0.D0
	Vpa(:,:,num_lin)=0.D0
	dI_p=0.D0



	Xout(1:cond)= 0.
    Xout(cond+1:2*cond)= 0.

!	open(UNIT = 16, FILE = 'temp_VR_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1016,IOSTAT=ERRNUM)
!	write(UNIT = 16) VR !Vant,Iant,Vi,Ii,Vrfin,Vrini,V1ini,V1fin,Vp,VR
!	close (UNIT = 16)

!1016 SELECT CASE(ERRNUM>0)
!			CASE (.TRUE.)
!				write(*,*) 'Error saving leyendo ten-corr_'//indarchiv//'.dat  ',ERRNUM
!	ENDSELECT
			ERRNUM=0 


ENDIF

	VR(1:cond,2:n+1,num_lin)=VR(1:cond,1:n,num_lin)  ! realizando corrimiento del vector VR
	
	VR(1:cond,1,num_lin)=0.D0
	ind_ini=ind_ini1+cond+6*cond*cond
	Xvar(ind_ini       :ind_ini-1+  cond)=V1ini(1:cond) 
	Xvar(ind_ini+  cond:ind_ini-1+2*cond)=V1fin(1:cond) 
	Xvar(ind_ini+2*cond:ind_ini-1+3*cond)=Vrini(1:cond,1)
	Xvar(ind_ini+3*cond:ind_ini-1+4*cond)=Vrini(1:cond,2)
	Xvar(ind_ini+4*cond:ind_ini-1+5*cond)=Vrfin(1:cond,1)
	Xvar(ind_ini+5*cond:ind_ini-1+6*cond)=Vrfin(1:cond,2)

	ind_ini=ind_ini+6*cond

!	DO i=1,cond
		!Xvar(ind_ini +                (i-1)*(kmax+1)                       :(ind_ini-1)+            i*(kmax+1))		=Vant(i,:,num_lin) 
		!Xvar(ind_ini +  cond*(kmax+1)+(i-1)*(kmax+1)                       :(ind_ini-1)+  cond*(kmax+1)+i*(kmax+1)) =Iant(i,:,num_lin) 
		!Xvar(ind_ini +2*cond*(kmax+1)+(i-1)*(kmax+1)                     :(ind_ini-1)+2*cond*(kmax+1)+i*(kmax+1))  =Vp(i,:)
		!Xvar(ind_ini +3*cond*(kmax+1)+(i-1)*(kmax+1)                     :(ind_ini-1)+3*cond*(kmax+1)+i*(kmax+1))  =Vpa(i,:)
!		Xvar(ind_ini +4*cond*(kmax+1)+              (i-1)*(SIZE+2)       :(ind_ini-1)+4*cond*(kmax+1)+                i*(SIZE+2))=VR(i,:)
!		Xvar(ind_ini +4*cond*(kmax+1)+  cond*(SIZE+2)+(i-1)*(SIZE+2)     :(ind_ini-1)+4*cond*(kmax+1)+  cond*(SIZE+2)+i*(SIZE+2))=dI(:,i)
!	ENDDO

!	IF (N .EQ. 1) THEN
!		open(UNIT = 15, FILE = 'ten-corr_'//indarchiv//'.dat', FORM='UNFORMATTED', STATUS = 'UNKNOWN',ERR=1015,IOSTAT=ERRNUM)
!		write(UNIT = 15) dI,Vant,Iant,Vp,Vpa,VR !Vant,Iant,Vi,Ii,Vrfin,Vrini,V1ini,V1fin,Vp,VR
!		close (UNIT = 15)
!
!1015 SELECT CASE(ERRNUM>0)
!			CASE (.TRUE.)
!				write(*,*) 'Error saving leyendo ten-corr_'//indarchiv//'.dat  ',ERRNUM
!			ENDSELECT 
!	ENDIF
 
!	DEALLOCATE (h,Xi,Ci,Li,LCi,D3i,D5,A1,			&
!				A2,Vp,VR,A3,A4,B1,B2,B3,B4,B5,B6,Vant,Iant,Vi,Ii,Evini,Evfin,	&
!				Ex,Vrini,V1ini,Vrfin,V1fin,Zc,STAT=ALLOC_ERR)

DEALLOCATE (h,Ex2,Evini2,Evfin2,Ii,Vi,A1,A2,A3,A4,B1,B2,B3,B4,B5,B6,dI_p,Mv1,Mv2,Mv3,Mi1,Mi2,Mi3,Mi4,Zc,Zci,STAT=ALLOC_ERR)
!	DEALLOCATE (Ex2,Evini2,Evfin2,Ii,Vi,A1,A2,A3,A4,B1,B2,B3,B4,B5,B6,dI_p,STAT=ALLOC_ERR)


	END SUBROUTINE
!********************************************************************************************

	function ferrc(x) RESULT (E)
	
	DOUBLE PRECISION a1, a2, a3, a4, a5, t,p, E, x
	a1=0.254829592
	a2=-0.284496736
	a3=1.421413741
	a4=-1.453152027
	a5=1.061405429
	p=0.3275911
	t=1/(1+p*x)

	E=(a1*t+a2*t**2+a3*t**3+a4*t**4+a5*t**5)*dexp(-x**2)

	END FUNCTION ferrc




