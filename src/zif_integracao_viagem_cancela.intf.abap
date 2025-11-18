interface ZIF_INTEGRACAO_VIAGEM_CANCELA
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTE_VIAGEM_CANCELA type ref to ZIF_INTEGRACAO_VIAGEM_CANCELA .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_VIAGEM_CANCELA) type ref to ZIF_INTEGRACAO_VIAGEM_CANCELA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_VIAGEM_CANCELA) type ref to ZIF_INTEGRACAO_VIAGEM_CANCELA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
    returning
      value(R_IF_INTEGRACAO_VIAGEM_CANCELA) type ref to ZIF_INTEGRACAO_VIAGEM_CANCELA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
