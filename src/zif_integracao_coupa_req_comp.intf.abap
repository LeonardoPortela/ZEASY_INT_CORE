interface ZIF_INTEGRACAO_COUPA_REQ_COMP
  public .


  data AT_CLIENTE_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INT_COUPA_REQ_COMP type ref to ZIF_INTEGRACAO_COUPA_REQ_COMP .
  constants AT_FC_END_POINT type STRING value SPACE ##NO_TEXT.
  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_REQ_COMPRA_STRUC type ZMMC_DADOS_INT_COUPA_EBAN .
  data AT_XML type STRING .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_TOKEN_WS type ZAUTH_WEBSERVICE .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_HVI_KUHLMANN) type ref to ZIF_INTEGRACAO_HVI_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_REQ_COMPRA_BUSCAR
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
      value(R_IF_INTEGRACAO_COUPA_REQ_COMP) type ref to ZIF_INTEGRACAO_COUPA_REQ_COMP
    raising
      ZCX_INTEGRACAO .
  methods SET_XML
    exporting
      !E_XML type STRING
    returning
      value(R_IF_INTEGRACAO_COUPA_REQ_COMP) type ref to ZIF_INTEGRACAO_COUPA_REQ_COMP
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    returning
      value(R_IF_INTEGRACAO_COUPA_REQ_COMP) type ref to ZIF_INTEGRACAO_COUPA_REQ_COMP
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_COUPA_REQ_COMP) type ref to ZIF_INTEGRACAO_COUPA_REQ_COMP
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_COUPA_REQ_COMP) type ref to ZIF_INTEGRACAO_COUPA_REQ_COMP
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_COUPA_REQ_COMP) type ref to ZIF_INTEGRACAO_COUPA_REQ_COMP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional .
  methods ENVIAR_COUPA
    returning
      value(R_RETURN_VALUE) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
