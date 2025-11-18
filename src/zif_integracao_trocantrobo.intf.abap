interface ZIF_INTEGRACAO_TROCANTROBO
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_TROCANTROBO type ref to ZIF_INTEGRACAO_TROCANTROBO .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_REQUEST type ZLESE0038 .
  data AT_RESPONSE type ZLESE0037 .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_TROCANTROBO) type ref to ZIF_INTEGRACAO_TROCANTROBO
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
      !ID_REFERENCIA type STRING optional
    returning
      value(R_IF_INTEGRACAO_TROCANTROBO) type ref to ZIF_INTEGRACAO_TROCANTROBO
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_TROCANTROBO) type ref to ZIF_INTEGRACAO_TROCANTROBO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_ITINERARIO
    returning
      value(R_IF_INTEGRACAO_TROCANTROBO) type ref to ZIF_INTEGRACAO_TROCANTROBO .
endinterface.
