interface ZIF_INTEGRACAO_HVI_KUHLMANN
  public .


  data AT_CLIENTE_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_HVI_KUHLMANN type ref to ZIF_INTEGRACAO_HVI_KUHLMANN .
  constants AT_FC_END_POINT type STRING value '/api/hvi' ##NO_TEXT.
  class-data AT_SERVICO type ZTIPOWEBSERV .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_HVI_BUSCAR
    importing
      !I_CLIENTE_ID type STRING optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional .
endinterface.
