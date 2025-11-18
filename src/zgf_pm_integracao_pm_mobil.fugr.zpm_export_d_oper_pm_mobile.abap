FUNCTION ZPM_EXPORT_D_OPER_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  IWERK
*"     REFERENCE(DATA_DE) TYPE  SY-DATUM
*"     REFERENCE(DATA_ATE) TYPE  SY-DATUM
*"  TABLES
*"      RESULT_OPERACAO STRUCTURE  ZTPM_D_M_OPERACAO2
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------


  NEW CL_MAIN_APP( )->GET_D_OPERACAO(
           EXPORTING
             FILIAL   = FILIAL
             DATA     = CL_MAIN_APP=>GET_DATE_RANGE( FROM = DATA_DE  TO = DATA_ATE )
             RECEIVING
             D_OPERACAO = RESULT_OPERACAO[] ).


ENDFUNCTION.
