MODULE EDH_CALC
    IMPLICIT NONE
    !T: Tensión = [m.kg]
    !g: Gravedad = [m/s²]
    !w: Peso = [kg]
    !T.G/w = c²
    !T.g.DT²/w.DT² = 1, porque sí, para simplificar el cálculo, y porque supongo que no está el mismo inconveniente que en EDP.
    
    !También c² = T/(m/L) = T.L/m    (m/L es la densidad de la cuerda)
    REAL(8), PARAMETER :: T = 1.
    REAL(8), PARAMETER :: G = 9.8, W = 1.
!    REAL(8), PARAMETER :: M = 40., L = 10. !L debiera ser el mismo valor que el XFINAL de EDH_SETUP
CONTAINS
    FUNCTION EDH_CALC_DX(DT)
        REAL(8) :: EDH_CALC_DX
        REAL(8), INTENT(IN) :: DT
        
        EDH_CALC_DX = DT*EDH_CALC_C()
    END FUNCTION
    
    FUNCTION EDH_CALC_DT(DX)
        REAL(8) :: EDH_CALC_DT
        REAL(8), INTENT(IN) :: DX
        
        EDH_CALC_DT = DX/EDH_CALC_C()
    END FUNCTION
    
    !Separado para mayor facilidad en el cálculo
    !Utiliza las variables definidas en el módulo.
    !Calcula la velocidad de propagación de la onda (c)
    FUNCTION EDH_CALC_C()
        REAL(8) :: EDH_CALC_C
        EDH_CALC_C = SQRT(T*G/W)
!        EDH_CALC_C = SQRT(T*L/M)
    END FUNCTION
END MODULE
