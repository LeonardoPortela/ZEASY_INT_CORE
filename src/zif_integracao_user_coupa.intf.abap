interface ZIF_INTEGRACAO_USER_COUPA
  public .


  data AT_CLIENTE_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_COUPA type ref to ZIF_INTEGRACAO_USER_COUPA .
  constants AT_FC_END_POINT type STRING value '/application/xml/' ##NO_TEXT.
  class-data AT_SERVICO type ZTIPOWEBSERV .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_USER_COUPA) type ref to ZIF_INTEGRACAO_USER_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_USER_BUSCAR
    importing
      !I_CLIENTE_ID type STRING optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_USER_COUPA) type ref to ZIF_INTEGRACAO_USER_COUPA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      value(E_USER) type ZDE_LOGIN optional
      value(E_ID) type STRING optional
      value(E_SERVICO) type /UI2/SERVICE_NAME optional
      value(E_HTTP_URL) type STRING optional
      value(E_TOKEN) type STRING optional
      value(E_METODO) type STRING optional
    returning
      value(R_IF_INTEGRACAO_USER_COUPA) type ref to ZIF_INTEGRACAO_USER_COUPA
    raising
      ZCX_INTEGRACAO .
  methods GET_XML
    importing
      value(E_ATIVO) type CHAR1 optional
      value(E_BLOQU) type CHAR1 optional
    exporting
      !E_XML type STRING
    returning
      value(R_IF_INTEGRACAO_USER_COUPA) type ref to ZIF_INTEGRACAO_USER_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_XML type STRING
    returning
      value(R_IF_INTEGRACAO_USER_COUPA) type ref to ZIF_INTEGRACAO_USER_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_USER_COUPA) type ref to ZIF_INTEGRACAO_USER_COUPA
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_USER_COUPA) type ref to ZIF_INTEGRACAO_USER_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_USER_COUPA) type ref to ZIF_INTEGRACAO_USER_COUPA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional .
  methods EXECUTE_PROCESS
    importing
      !I_USER type ZDE_LOGIN
      !I_ATIVO type FLAG
      !I_BLOQUEADO type FLAG
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
