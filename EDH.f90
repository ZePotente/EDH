PROGRAM EDH
    !Modulo
    USE VYM_IO
    USE EDH_SETUP
    USE EDH_CALC
    
    IMPLICIT NONE
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
CONTAINS
    
END PROGRAM
