FUNCTION ZPM_EXPORT_T_PRIO_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_PRIOR STRUCTURE  T356_T
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

NEW CL_MAIN_APP( )->GET_TIPO_PRIOR(
    RECEIVING
      TIP_PRIOR = RESULT_PRIOR[] ).



ENDFUNCTION.
