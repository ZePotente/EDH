PROGRAM EDH
    !Modulo
    USE EDP_TABLA
    USE VYM_IO
    USE EDH_SETUP
    USE EDH_CALC
    
    IMPLICIT NONE
    !---Tener cuidado que en el cálculo del primer paso, calculando el UANT2 está puesto por defecto que v(x) == 0. ---!
    
    REAL(8), DIMENSION(:), ALLOCATABLE :: UINI, V, UFIN
    REAL(8) :: DX, DT
    REAL(8) :: XFINAL, TFINAL
    
    DX = 0.1
    PRINT *, 'Valor inicial de DX = ', DX
    
    PRINT *, 'Inicializando variables'
    CALL INICIALIZAR_EDH(DX, UINI, V, XFINAL, TFINAL)
    PRINT *, 'Variables inicializadas'
    
    PRINT *, 'XFINAL = ', XFINAL; PRINT *, 'TFINAL = ', TFINAL;
    PRINT *, 'UINI = '
    CALL VEC_MOSTRAR(UINI)
    
    PRINT *, 'Calculando valor de DT.'
    DT = EDH_CALC_DT(DX)
    PRINT *, 'Valor de DT calculado. DT = ', DT
    
    PRINT *, 'Empezando la resolución del problema.'
    CALL EDH_RESOLUCION(UINI, V, UFIN, DX, DT, TFINAL)
    PRINT *, 'Resolución del problema finalizada.'
CONTAINS
    SUBROUTINE EDH_RESOLUCION(UINI, V, U, DX, DT, TFINAL)
        REAL(8), DIMENSION(:), INTENT(IN) :: UINI, V
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: U
        REAL(8), INTENT(IN) :: DX, DT, TFINAL
        !
        REAL(8), DIMENSION(:), ALLOCATABLE :: UANT2, UANT1
        REAL(8) :: T
        INTEGER :: I, N
        CHARACTER(*), PARAMETER :: ARCHIVO = 'EDH.txt'
        
        OPEN(1, FILE = ARCHIVO, ACTION = 'WRITE')
        
        N = SIZE(UINI)
        ALLOCATE(UANT1(N), U(N))
        U = UINI
        
        T = 0.
        !Escribo en el primer renglón las referencias de la tabla
        CALL T_CABECERA(DX, N)        
        !Escribo el paso inicial
        CALL T_PASOINICIAL(U, T)
        
        !Guardo u-1 en u, porque en el ciclo se va a hacer que u-1 = u, y que u = u+1 (se avanza en el paso).
        !Esto para no tener que inicializarlo aparte.
        CALL CREAR_UANT2(V, DT, UINI, UANT1)
        
        !El ciclo en sí.
        DO WHILE(T <= TFINAL)
            T = T + DT
            UANT2 = UANT1
            UANT1 = U
            DO I = 2, N-1
                U(I) = UANT1(I+1) + UANT1(I-1) - UANT2(I)
            END DO
            CALL T_PASO(U, T)
        END DO
        
        CLOSE(1)
    END SUBROUTINE
    
    SUBROUTINE CREAR_UANT2(V, DT, UANT1, UANT2)
        REAL(8), INTENT(IN) :: V(:), DT, UANT1(:)
        REAL(8), ALLOCATABLE, INTENT(OUT) :: UANT2(:)
        !
        INTEGER :: I, N
        
        N = SIZE(UANT1)
        ALLOCATE(UANT2(N))
        !Usar si v(x) != 0 para todo x
!        DO I = 2, N-1
!            UANT2(I) = V(I)*DT + (UANT1(I-1) + UANT1(I+1)) / 2.
!        END DO
        
        !Usar si v(x) == 0 para todo x.
        DO I = 2, N-1
            UANT2(I) = (UANT1(I-1) + UANT1(I+1)) / 2.
        END DO
    END SUBROUTINE
END PROGRAM
