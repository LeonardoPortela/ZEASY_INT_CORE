FUNCTION ZPM_EXPORT_P_ORDE_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_P_ORD STRUCTURE  ZTPM_P_T_ORDEM
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

NEW CL_MAIN_APP( )->GET_P_ORDEM(
    RECEIVING
      P_ORDEM = RESULT_P_ORD[] ).



ENDFUNCTION.
