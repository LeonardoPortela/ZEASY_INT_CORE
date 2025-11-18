FUNCTION ZPM_EXPORT_T_CTRA_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  IWERK
*"  TABLES
*"      RESULT_CTRAB STRUCTURE  ZTPM_PAR_T_CTRAB
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

  NEW CL_MAIN_APP( )->GET_TIPO_CTRAB(
          EXPORTING
         FILIAL   = FILIAL
          RECEIVING
          TIP_CTRAB = RESULT_CTRAB[] )."TIP_CTRAB

ENDFUNCTION.
