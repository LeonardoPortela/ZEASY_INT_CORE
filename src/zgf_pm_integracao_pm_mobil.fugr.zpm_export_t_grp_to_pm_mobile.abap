FUNCTION ZPM_EXPORT_T_GRP_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  IWERK
*"  TABLES
*"      RESULT_GRP STRUCTURE  T024I
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

  NEW CL_MAIN_APP( )->GET_TIPO_GRP(
      EXPORTING
         FILIAL   = FILIAL
      RECEIVING
        TIP_GRP = RESULT_GRP[] ).



ENDFUNCTION.
