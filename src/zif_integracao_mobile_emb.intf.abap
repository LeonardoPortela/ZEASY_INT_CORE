interface ZIF_INTEGRACAO_MOBILE_EMB
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTE_MOBILE_EMB type ref to ZIF_INTEGRACAO_MOBILE_EMB .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_MOBILE_EMB) type ref to ZIF_INTEGRACAO_MOBILE_EMB
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_MOBILE_EMB) type ref to ZIF_INTEGRACAO_MOBILE_EMB
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_MOBILE_EMB) type ref to ZIF_INTEGRACAO_MOBILE_EMB
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
