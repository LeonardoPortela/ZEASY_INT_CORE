interface ZIF_INTEGRACAO_BRY_ASSINA_GET
  public .


  data AT_END_POINT type STRING .
  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_JSON_PARTI_TEMPLATE type STRING .
  data AT_JSON_PARTICIPANTES type STRING .
  class-data AT_INSTANCE type ref to ZIF_INTEGRACAO_BRY_ASSINA_GET .
  data AT_BAPIRET2_TAB type BAPIRET2_T .

  class-methods GET_INSTANCE
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_BRY_ASSINA_GET
    raising
      ZCX_INTEGRACAO .
  class-methods GET_URL_ENVIRONMENT
    returning
      value(RV_URL) type STRING .
  methods EXECUTE_SERVICE
    importing
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'GET'
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_DOCS_ASSINADOS
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE optional
      !I_ID_PROCESSO type ZIN_ID_PROCESSO default '01'
    returning
      value(R_DOCS_ASSINADOS) type ZINC_DOCS_ASSINA .
  methods GET_MESSAGES
    returning
      value(R_RET) type BAPIRET2_T .
  methods SET_MESSAGE
    importing
      !I_MESSAGE type STRING
      !I_MSGTY type MSGTY optional .
  methods GET_REL_ASSINANTES
    importing
      !I_DOC_ASSINADO type ZINT_ASSINA01
    returning
      value(RS_REL_ASSINANTES) type ZINS_DOCS_ASSINA .
  methods GET_DOCS_ASSINADOS_BY_WF_ID
    importing
      !IV_WORKFLOW_BRY type STRING optional
    returning
      value(R_DOCS_ASSINADOS) type ZINC_DOCS_ASSINA .
endinterface.
