interface ZIF_INTEGRACAO_ADIGITAL_POST
  public .


  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  class-data AT_INSTANCE type ref to ZIF_INTEGRACAO_ADIGITAL_POST .
  data AT_BAPIRET2_TAB type BAPIRET2_T .

  class-methods GET_INSTANCE
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_ADIGITAL_POST
    raising
      ZCX_INTEGRACAO .
  methods EXECUTE_MULTIPART
    importing
      !I_MULTI_TAB type ZDE_MULTIPART_FIELD_T
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'POST'
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods EXECUTE_SERVICE
    importing
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'GET'
      !I_BODY type STRING optional
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SEND_DOCS_ASSINADOS
    importing
      value(I_DOCS_ASSINADOS) type ZINC_DOCS_ASSINA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods PROCESS_CONTRACTS
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE optional
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods BEFORE_PUBLISHING
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods AFTER_PUBLISHING
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_MESSAGES
    returning
      value(R_RET) type BAPIRET2_T .
  methods SET_MESSAGE
    importing
      !I_MESSAGE type STRING
      !I_MSGTY type MSGTY optional .
endinterface.
