interface ZIF_INTEGRACAO_TCOT_CONTRATOS
  public .


  class-data AT_IF_INTEGRACAO_TCOT_CONT type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS .
  data AT_JSON type STRING .
  data AT_METODO type STRING .
  data AT_ID_REFERENCIA type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS .
  methods SET_INT_CONTRATOS
    importing
      !I_JSON type STRING
      !I_METODO type STRING
      !I_ID_REFERENCIA type STRING
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS
    raising
      ZCX_INTEGRACAO .
  methods GET_METODO
    exporting
      !E_METODO type STRING
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_TCOT_CONTRATOS) type ref to ZIF_INTEGRACAO_TCOT_CONTRATOS .
endinterface.
