interface ZIF_INTEGRACAO_PLAN_PRODUCE
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_PLAN_PRODUCE type ref to ZIF_INTEGRACAO_PLAN_PRODUCE .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_ORDEM_CARREGAMENTO type ZPPE0003 .
  data AT_RESPONSE type STRING .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_PLAN_PRODUCE) type ref to ZIF_INTEGRACAO_PLAN_PRODUCE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
      !ID_REFERENCIA type STRING optional
    returning
      value(R_IF_INTEGRACAO_PLAN_PRODUCE) type ref to ZIF_INTEGRACAO_PLAN_PRODUCE
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_PLAN_PRODUCE) type ref to ZIF_INTEGRACAO_PLAN_PRODUCE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_OC_CANCELAR
    returning
      value(R_IF_INTEGRACAO_PLAN_PRODUCE) type ref to ZIF_INTEGRACAO_PLAN_PRODUCE .
  methods SET_OC_MODIFICAR
    returning
      value(R_IF_INTEGRACAO_PLAN_PRODUCE) type ref to ZIF_INTEGRACAO_PLAN_PRODUCE .
  methods SET_OC_PESOVAZIO
    returning
      value(R_IF_INTEGRACAO_PLAN_PRODUCE) type ref to ZIF_INTEGRACAO_PLAN_PRODUCE .
endinterface.
