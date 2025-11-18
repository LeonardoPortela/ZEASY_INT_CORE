interface ZIF_INTEGRACAO_TOKEN
  public .


  class-data AT_IF_INTEGRACAO_TOKEN type ref to ZIF_INTEGRACAO_TOKEN .
  data AT_BUKRS type BUKRS .
  constants AT_FC_PROCESSAR_TOKEN type ZDE_DS_FUNCAO_PROCESSA value '/token' ##NO_TEXT.
  data AT_ZLEST0196 type ZLEST0196 .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_EMPRESA_TOKEN
    importing
      !I_BUKRS type BUKRS
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_ACCESS_TOKEN type STRING
      !E_TOKEN_TYPE type STRING
      !E_EXPIRES_IN type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_ACCESS_TOKEN type STRING
      !E_TOKEN_TYPE type STRING
      !E_EXPIRES_IN type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
