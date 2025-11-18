FUNCTION ZPM_EXPORT_T_DESV_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  IWERK
*"  TABLES
*"      RESULT_DESVIO STRUCTURE  ZTPM_PAR_T_DESVIO
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

  NEW CL_MAIN_APP( )->GET_P_DESVIO(
      EXPORTING
         FILIAL   = FILIAL
      RECEIVING
        P_DESVIO = RESULT_DESVIO[] ).

ENDFUNCTION.
