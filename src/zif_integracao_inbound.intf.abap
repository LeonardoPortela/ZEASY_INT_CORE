interface ZIF_INTEGRACAO_INBOUND
  public .


  class-data AT_IF_INTEGRACAO_INBOUND type ref to ZIF_INTEGRACAO_INBOUND .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG .

  methods VALIDAR_DADOS_INBOUND
    importing
      !I_DATA_INBOUND type STRING
    exporting
      value(E_STATUS_CODE) type CHAR03
    returning
      value(R_MSG_ERRO) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods PROCESSAR_REQUISICAO
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
      !E_MSG type STRING
    returning
      value(R_ZIF_INTEGRACAO_INBOUND) type ref to ZIF_INTEGRACAO_INBOUND
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_INBOUND) type ref to ZIF_INTEGRACAO_INBOUND
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods CONFIGURE_SERVER
    importing
      !I_HTTP_SERVER type ref to IF_HTTP_SERVER
    returning
      value(R_ZIF_INTEGRACAO_INBOUND) type ref to ZIF_INTEGRACAO_INBOUND
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
