interface ZIF_INTEGRACAO_PROTHEUS
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTE_PROTHEUS type ref to ZIF_INTEGRACAO_PROTHEUS .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_PROTHEUS) type ref to ZIF_INTEGRACAO_PROTHEUS
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_PROTHEUS) type ref to ZIF_INTEGRACAO_PROTHEUS
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_PROTHEUS) type ref to ZIF_INTEGRACAO_PROTHEUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
