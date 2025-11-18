FUNCTION ZPM_EXPORT_D_NOTA_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  IWERK
*"     REFERENCE(DATA_DE) TYPE  SY-DATUM
*"     REFERENCE(DATA_ATE) TYPE  SY-DATUM
*"  TABLES
*"      RESULT_D_NOTA TYPE  ZEPM_D_NOTA_T
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------


  NEW CL_MAIN_APP( )->GET_D_NOTA(
     EXPORTING
           FILIAL   = FILIAL
           DATA     = CL_MAIN_APP=>GET_DATE_RANGE( FROM = DATA_DE  TO = DATA_ATE )
      RECEIVING
        D_NOTA = RESULT_D_NOTA[] ).


ENDFUNCTION.
