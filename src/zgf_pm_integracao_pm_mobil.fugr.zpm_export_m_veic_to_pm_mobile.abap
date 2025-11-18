FUNCTION ZPM_EXPORT_M_VEIC_TO_PM_MOBILE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DATA_DE) TYPE  CHAR16
*"     REFERENCE(DATA_ATE) TYPE  CHAR16
*"     REFERENCE(STATUS) TYPE  C
*"     REFERENCE(FILIAL) TYPE  IWERK
*"  TABLES
*"      RESULT STRUCTURE  ZTPM_M_VEIC_MOBILE
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------
**
  NEW CL_MAIN_APP( )->GET_VEICULOS(
     EXPORTING
       FILIAL   = FILIAL
       SITUACAO = STATUS
     RECEIVING
       VEICULOS = RESULT[]
   ).
ENDFUNCTION.
