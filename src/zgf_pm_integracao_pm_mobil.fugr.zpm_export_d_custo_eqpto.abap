FUNCTION zpm_export_d_custo_eqpto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(MES_INICIO) TYPE  CHAR02 OPTIONAL
*"     REFERENCE(MES_FIM) TYPE  CHAR02 OPTIONAL
*"     REFERENCE(ANO) TYPE  GJAHR OPTIONAL
*"     REFERENCE(TP_OBJECT) TYPE  EQART OPTIONAL
*"     REFERENCE(CT_EQPTO) TYPE  EQTYP OPTIONAL
*"     REFERENCE(EQPTO) TYPE  EQUNR OPTIONAL
*"     REFERENCE(MODELO) TYPE  TYPBZ
*"  TABLES
*"      RESULT_CUST_EQPTO STRUCTURE  ZPME0066
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------

  NEW cl_main_app( )->get_custo_eqpto(
          EXPORTING
         filial     = filial
         mes_inicio = mes_inicio
         mes_fim    = mes_fim
         ano        = ano
         tp_object  = tp_object
         ct_eqpto   = ct_eqpto
         eqpto      = eqpto
         modelo     = modelo
          RECEIVING
          t_cust_eqpto = result_cust_eqpto[] ).



ENDFUNCTION.
