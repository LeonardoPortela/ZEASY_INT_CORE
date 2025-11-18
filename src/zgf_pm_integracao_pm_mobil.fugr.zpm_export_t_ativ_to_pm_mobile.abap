FUNCTION ZPM_EXPORT_T_ATIV_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_ATIV STRUCTURE  T353I_T
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

NEW CL_MAIN_APP( )->GET_TIPO_ATIVIDADE(
    RECEIVING
      TIP_ATIVIDADE = RESULT_ATIV[] ).



ENDFUNCTION.
