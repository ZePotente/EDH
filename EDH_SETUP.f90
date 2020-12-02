MODULE EDH_SETUP
    IMPLICIT NONE
    REAL(8), PARAMETER :: PI = 3.14159265359
CONTAINS
    !Inicializa los valores de los vectores UINI y V, y las dimensiones físicas y temporales finales.
    !O sea U con cond iniciales y frontera, y XFINAL y TFINAL
    SUBROUTINE INICIALIZAR_EDH(DX, UINI, V, XFINAL, TFINAL)
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: UINI, V
        REAL(8), INTENT(IN) :: DX
        REAL(8), INTENT(OUT) :: XFINAL, TFINAL
        !
        INTEGER :: N
        
        !Valores finales
        XFINAL = 0.8; TFINAL = 1.;
        
        N = NINT(XFINAL/DX) + 1 !Redondeo a int y sumo 1.
!        DX = XFINAL/N + 1 !Se ajusta el DX para que encaje con el valor de N entero.
        
        !Condiciones iniciales
        CALL SETUP_UINI(N, DX, XFINAL, UINI)
        
        !Condiciones de frontera
        CALL SETUP_FRONTERA(UINI) !aunque igualmente ya se supone que no varía porque se hace de i = 2 hasta N-1
        
        !Vector velocidad
        CALL SETUP_VELOCIDAD(N, V)
        
    END SUBROUTINE
    
    SUBROUTINE SETUP_UINI(N, DX, XFINAL, UINI)
        REAL(8), INTENT(IN) :: DX, XFINAL
        INTEGER, INTENT(IN) :: N
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: UINI
        !
        REAL(8) :: X, XTIRON
        REAL(8), PARAMETER :: ERRORTRUNC = 1E-6 !error limite teniendo en cuenta el error de truncamiento arrastrado por sumar numeros en punto flotante.
        INTEGER :: I
        ALLOCATE(UINI(N))
        X = 0.
        
!        DO I = 2, N-1
!            X = X + DX
!            UINI(I) = SIN(PI * X)
!        END DO
        
        I = 1; XTIRON = 0.3
        !a la izq del tirón
        DO WHILE(I <= N .AND. (ABS(X-XTIRON) - ERRORTRUNC) >= 0.) 
            UINI(I) = 3.*X / 100.
            PRINT *, X-XTIRON
            X = X + DX
            I = I + 1
        END DO
        !a la der del tirón
        DO WHILE(I <= N .AND. (ABS(X-XFINAL) - ERRORTRUNC) >= 0.)
            UINI(I) = (0.8-X) / 100.
            X = X + DX
            I = I + 1
        END DO
    END SUBROUTINE
    
    SUBROUTINE SETUP_FRONTERA(UINI)
        REAL(8), DIMENSION(:) :: UINI
        !
        INTEGER :: N
        
        N = SIZE(UINI)
        
        UINI(1) = 0.
        UINI(N) = 0.
    END SUBROUTINE
    
    SUBROUTINE SETUP_VELOCIDAD(N, V)
        INTEGER, INTENT(IN) :: N
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: V
        
        ALLOCATE(V(N))
        V = 0.
    END SUBROUTINE
END MODULE
