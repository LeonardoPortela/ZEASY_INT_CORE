FUNCTION ZPM_EXPORT_T_NOTA_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_NOTA TYPE  ZTPM_PAR_T_NOTA_T
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

NEW CL_MAIN_APP( )->GET_TIPO_NOTAS(
    RECEIVING
      TIP_NOTA = RESULT_NOTA[] ).



ENDFUNCTION.
