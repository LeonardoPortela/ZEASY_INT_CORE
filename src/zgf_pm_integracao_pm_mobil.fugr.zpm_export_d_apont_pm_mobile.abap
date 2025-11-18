FUNCTION ZPM_EXPORT_D_APONT_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_APONT STRUCTURE  ZTPM_D_M_APONT
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

 NEW CL_MAIN_APP( )->GET_D_APONT(
      RECEIVING
        D_APONT = RESULT_APONT[] ).



ENDFUNCTION.
