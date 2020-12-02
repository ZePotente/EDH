MODULE EDH_CALC
    IMPLICIT NONE
    !T: Tensión = [m.kg]
    !g: Gravedad = [m/s²]
    !w: Peso = [kg]
    !T.g.DT²/w.DT² = 1, porque sí, para simplificar el cálculo, y porque supongo que no está el mismo inconveniente que en EDP.
    REAL(8), PARAMETER :: T = 1., G = 9.8, W = 1.
CONTAINS
    FUNCTION EDH_CALC_DX(DT)
        REAL(8) :: EDH_CALC_DX
        REAL(8), INTENT(IN) :: DT
        
        EDH_CALC_DX = DT*(SQRT(T*G/W))
    END FUNCTION
    
    FUNCTION EDH_CALC_DT(DX)
        REAL(8) :: EDH_CALC_DT
        REAL(8), INTENT(IN) :: DX
        
        EDH_CALC_DT = DX/(SQRT(T*G/W))
    END FUNCTION

END MODULE
