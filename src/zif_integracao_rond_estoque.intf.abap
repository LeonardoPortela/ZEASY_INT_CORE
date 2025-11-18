interface ZIF_INTEGRACAO_ROND_ESTOQUE
  public .


  class-data AT_IF_INTEGRACAO_ROND_ESTOQUE type ref to ZIF_INTEGRACAO_ROND_ESTOQUE .
  data AT_JSON type STRING .
  data AT_ZMMT0008 type ZMMT0008_T .
  data AT_ID_REFERENCIA type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_ROND_ESTOQUE) type ref to ZIF_INTEGRACAO_ROND_ESTOQUE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_ROND_ESTOQUE) type ref to ZIF_INTEGRACAO_ROND_ESTOQUE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_ROND_ESTOQUE) type ref to ZIF_INTEGRACAO_ROND_ESTOQUE
    raising
      ZCX_INTEGRACAO .
  methods SET_INT_ESTOQUE
    importing
      !I_JSON type STRING
      !I_ZMMT0008_T type ZMMT0008_T
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_ROND_ESTOQUE) type ref to ZIF_INTEGRACAO_ROND_ESTOQUE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_ROND_ESTOQUE) type ref to ZIF_INTEGRACAO_ROND_ESTOQUE
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_ROND_ESTOQUE) type ref to ZIF_INTEGRACAO_ROND_ESTOQUE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_ROND_ESTOQUE) type ref to ZIF_INTEGRACAO_ROND_ESTOQUE .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_ROND_ESTOQUE) type ref to ZIF_INTEGRACAO_ROND_ESTOQUE .
endinterface.
