FUNCTION ZPM_EXPORT_D_LOCAL_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  IWERK
*"     REFERENCE(STATUS) TYPE  C
*"  TABLES
*"      RESULT_LOCAL STRUCTURE  ZTPM_M_LOCAL_MOBILE
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

  NEW CL_MAIN_APP( )->GET_LOCAL(
       EXPORTING
         SITUACAO = STATUS
         FILIAL   = FILIAL
        RECEIVING
          LOCAL = RESULT_LOCAL[] ).



ENDFUNCTION.
