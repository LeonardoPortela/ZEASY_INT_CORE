FUNCTION zpm_export_d_ordem_pm_mobile.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  WERKS_D
*"     REFERENCE(DATA_DE) TYPE  SY-DATUM
*"     REFERENCE(DATA_ATE) TYPE  SY-DATUM
*"     REFERENCE(UPDATE_AT) TYPE  TIMESTAMPL
*"  TABLES
*"      RESULT_D_ORDEM TYPE  ZTPM_D_M_ORDEM_T
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

  NEW cl_main_app( )->get_d_ordem(
          EXPORTING
         filial   = filial
         create_at     = cl_main_app=>get_date_range( from = data_de to = data_ate  )
         update_at     = update_at
          RECEIVING
          d_ordem = result_d_ordem[] ).



ENDFUNCTION.
