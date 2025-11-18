interface ZIF_INTEGRACAO_OBS_ZLES0050
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_OBS_ZLES0050 type ref to ZIF_INTEGRACAO_OBS_ZLES0050 .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_J_1BNFDOC type J_1BNFDOC .
  data AT_J_1BNFE_ACTIVE type J_1BNFE_ACTIVE .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_OBS_ZLES0050) type ref to ZIF_INTEGRACAO_OBS_ZLES0050
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_OBS_ZLES0050) type ref to ZIF_INTEGRACAO_OBS_ZLES0050
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_OBS_ZLES0050) type ref to ZIF_INTEGRACAO_OBS_ZLES0050
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
