interface ZIF_INTEGRACAO_DATASHARE
  public .


  data AT_CLIENTE_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_DATASHARE type ref to ZIF_INTEGRACAO_DATASHARE .
  constants AT_FC_END_POINT type STRING value SPACE ##NO_TEXT.
  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_REQ_COMPRA_STRUC type ZMMC_DADOS_INT_COUPA_EBAN .
  data AT_XML type STRING .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_TOKEN_WS type ZAUTH_WEBSERVICE .
  data AT_ZHCME_PY_0003 type ZHCME_PY_0003_T .
  data AT_RETORNO type ZMMS007 .
  data AT_PARAMETROS type LIFNR .
  data AT_PROTOCOLO type STRING .
  data AT_ACEITE type CHAR1 .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_DATASHARE) type ref to ZIF_INTEGRACAO_DATASHARE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_DATASHARE) type ref to ZIF_INTEGRACAO_DATASHARE
    raising
      ZCX_INTEGRACAO .
  methods SET_XML
    exporting
      !E_XML type STRING
    returning
      value(R_IF_INTEGRACAO_DATASHARE) type ref to ZIF_INTEGRACAO_DATASHARE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    returning
      value(R_IF_INTEGRACAO_DATASHARE) type ref to ZIF_INTEGRACAO_DATASHARE
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_DATASHARE) type ref to ZIF_INTEGRACAO_DATASHARE
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_DATASHARE) type ref to ZIF_INTEGRACAO_DATASHARE
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_DATASHARE) type ref to ZIF_INTEGRACAO_DATASHARE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional .
  methods SET_CADASTRO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_PARAMETROS
    importing
      !I_PARAMETROS type LIFNR .
  methods SET_PROTOCOLO
    importing
      !I_PROTOCOLO type STRING optional .
  methods GET_PROTOCOLO
    returning
      value(R_PROTOCOLO) type STRING .
  methods SET_ACEITE
    importing
      !I_ACEITE type CHAR1 .
  methods GET_ACEITE
    returning
      value(R_ACEITE) type CHAR1 .
endinterface.
