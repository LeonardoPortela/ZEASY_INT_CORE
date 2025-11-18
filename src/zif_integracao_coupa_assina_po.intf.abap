interface ZIF_INTEGRACAO_COUPA_ASSINA_PO
  public .


  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  class-data AT_INSTANCE type ref to ZIF_INTEGRACAO_COUPA_ASSINA_PO .

  class-methods GET_INSTANCE
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_COUPA_ASSINA_PO
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
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
