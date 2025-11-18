FUNCTION ZPM_EXPORT_T_CENT_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RESULT_CENT STRUCTURE  T001W
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------


NEW CL_MAIN_APP( )->GET_TIPO_CENT(
        RECEIVING
        TIP_CENT = RESULT_CENT[] ).


ENDFUNCTION.
