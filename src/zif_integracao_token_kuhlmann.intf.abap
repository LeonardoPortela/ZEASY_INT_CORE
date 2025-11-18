interface ZIF_INTEGRACAO_TOKEN_KUHLMANN
  public .


  class-data AT_IF_TOKEN_KUHLMANN type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  constants AT_FC_END_POINT type STRING value '/login' ##NO_TEXT.
  class-data AT_SERVICO type STRING .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_TOKEN_KUHLMAN) type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_USUARIO_SENHA
    exporting
      !E_ACCESS_TOKEN type STRING
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_TOKEN_KUHLMAN) type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_TOKEN_KUHLMAN) type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_TOKEN_KUHLMAN) type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN_KUHLMAN) type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_TOKEN_KUHLMAN) type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN_KUHLMAN) type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_ACCESS_TOKEN type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN_KUHLMAN) type ref to ZIF_INTEGRACAO_TOKEN_KUHLMANN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      !I_SERVICO type ZTIPOWEBSERV .
endinterface.
