FUNCTION zpm_export_d_usuario_pm_mobile.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILIAL) TYPE  IWERK
*"     REFERENCE(USER_AD) TYPE  ZDE_USUARIO
*"  TABLES
*"      RESULT_USUARIO TYPE  ZTPM_D_M_USUARIO_T
*"  EXCEPTIONS
*"      RESULT_USUARIO
*"----------------------------------------------------------------------

  DATA: lv_pernr TYPE persno.

  NEW cl_main_app( )->get_d_usuario(
            EXPORTING
           filial   = filial
           user_ad  = user_ad
           pernr    = lv_pernr
            RECEIVING
            d_usuario = result_usuario[] ).
ENDFUNCTION.
