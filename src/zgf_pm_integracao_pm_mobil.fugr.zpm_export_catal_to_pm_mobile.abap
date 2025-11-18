FUNCTION ZPM_EXPORT_CATAL_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_CAT TYPE  ZTPM_P_CATAL_T
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

NEW CL_MAIN_APP( )->GET_CATALOGO(
    RECEIVING
      CATALOGO = RESULT_CAT[] ).

ENDFUNCTION.
