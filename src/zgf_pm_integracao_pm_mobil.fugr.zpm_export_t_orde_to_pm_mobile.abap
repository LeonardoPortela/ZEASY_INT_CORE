FUNCTION ZPM_EXPORT_T_ORDE_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_ORDEM STRUCTURE  V_AUART
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

NEW CL_MAIN_APP( )->GET_TIPO_ORDEM(
    RECEIVING
      TIP_ORDEM = RESULT_ORDEM[] ).



ENDFUNCTION.
