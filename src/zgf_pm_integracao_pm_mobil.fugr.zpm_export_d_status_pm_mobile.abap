FUNCTION ZPM_EXPORT_D_STATUS_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_STATUS STRUCTURE  ZTPM_D_M_STATUS
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

  NEW CL_MAIN_APP( )->GET_D_STATUS(
            RECEIVING
            D_STATUS = RESULT_STATUS[] ).



ENDFUNCTION.
