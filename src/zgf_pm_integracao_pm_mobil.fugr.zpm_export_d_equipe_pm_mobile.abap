FUNCTION ZPM_EXPORT_D_EQUIPE_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_EQUIPE
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

NEW CL_MAIN_APP( )->GET_D_EQUIPE(
          RECEIVING
          D_EQUIPE = RESULT_EQUIPE[] ).



ENDFUNCTION.
