interface ZIF_INTEGRACAO_IPORTSOLUTION
  public .


  data AT_CLIENTE_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_IPORTSOLUTION type ref to ZIF_INTEGRACAO_IPORTSOLUTION .
  constants AT_FC_END_POINT type STRING value SPACE ##NO_TEXT.
  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_REQ_COMPRA_STRUC type ZMMC_DADOS_INT_COUPA_EBAN .
  data AT_XML type STRING .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_TOKEN_WS type ZAUTH_WEBSERVICE .
  data AT_ZHCME_PY_0003 type ZHCME_PY_0003_T .
  data AL_ZHCME_PY_0003 type ZHCME_PY_0003 .
  data AT_PARAMETROS type ZHCME_PY_0004 .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_IPORTSOLUTION) type ref to ZIF_INTEGRACAO_IPORTSOLUTION
    raising
      ZCX_INTEGRACAO .
  methods GET_ACESSO
    importing
      !I_CLIENTE_ID type STRING optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_IPORTSOLUTION) type ref to ZIF_INTEGRACAO_IPORTSOLUTION
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_IPORTSOLUTION) type ref to ZIF_INTEGRACAO_IPORTSOLUTION
    raising
      ZCX_INTEGRACAO .
  methods SET_XML
    exporting
      !E_XML type STRING
    returning
      value(R_IF_INTEGRACAO_IPORTSOLUTION) type ref to ZIF_INTEGRACAO_IPORTSOLUTION
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    returning
      value(R_IF_INTEGRACAO_IPORTSOLUTION) type ref to ZIF_INTEGRACAO_IPORTSOLUTION
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_IPORTSOLUTION) type ref to ZIF_INTEGRACAO_IPORTSOLUTION
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_IPORTSOLUTION) type ref to ZIF_INTEGRACAO_IPORTSOLUTION
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_IPORTSOLUTION) type ref to ZIF_INTEGRACAO_IPORTSOLUTION
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional .
  methods SET_AUSENCIA .
  methods SET_PARAMETROS
    importing
      !I_PARAMETROS type ZHCME_PY_0004 .
endinterface.
