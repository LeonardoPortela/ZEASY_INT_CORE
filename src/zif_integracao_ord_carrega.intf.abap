interface ZIF_INTEGRACAO_ORD_CARREGA
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_ORD_CARREGA type ref to ZIF_INTEGRACAO_ORD_CARREGA .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_MSG_INBOUND type ZDE_MSG_INBOUND_ORDEM_CARREGA .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_ORD_CARREGA) type ref to ZIF_INTEGRACAO_ORD_CARREGA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_ORD_CARREGA) type ref to ZIF_INTEGRACAO_ORD_CARREGA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
    returning
      value(R_IF_INTEGRACAO_ORD_CARREGA) type ref to ZIF_INTEGRACAO_ORD_CARREGA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
