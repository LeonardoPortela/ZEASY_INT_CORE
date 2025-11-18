interface ZIF_INTEGRACAO_TKN_RONDONLINE
  public .


  class-data AT_IF_TOKEN_RONDONLINE type ref to ZIF_INTEGRACAO_TKN_RONDONLINE .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  constants AT_FC_END_POINT type STRING value '/login' ##NO_TEXT.
  data AT_ADD01 type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_TKN_RONDONLINE) type ref to ZIF_INTEGRACAO_TKN_RONDONLINE
    raising
      ZCX_INTEGRACAO .
  methods SET_USUARIO_SENHA
    exporting
      !E_ACCESS_TOKEN type STRING
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_TKN_RONDONLINE) type ref to ZIF_INTEGRACAO_TKN_RONDONLINE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_TKN_RONDONLINE) type ref to ZIF_INTEGRACAO_TKN_RONDONLINE
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_TKN_RONDONLINE) type ref to ZIF_INTEGRACAO_TKN_RONDONLINE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_TKN_RONDONLINE) type ref to ZIF_INTEGRACAO_TKN_RONDONLINE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_TKN_RONDONLINE) type ref to ZIF_INTEGRACAO_TKN_RONDONLINE
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_TKN_RONDONLINE) type ref to ZIF_INTEGRACAO_TKN_RONDONLINE
    exceptions
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_ACCESS_TOKEN type STRING
    returning
      value(R_IF_INTEGRACAO_TKN_RONDONLINE) type ref to ZIF_INTEGRACAO_TKN_RONDONLINE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
