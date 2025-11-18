FUNCTION ZHCM_INFORMAR_SENHA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(E_SENHA) TYPE  ZDE_SENHA_30
*"  RAISING
*"      ZCX_SOL_MOBILE_RH
*"----------------------------------------------------------------------

  PERFORM BUSCAR_SENHA CHANGING E_SENHA.

ENDFUNCTION.
