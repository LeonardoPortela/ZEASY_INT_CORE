interface ZIF_INTEGRACAO_ADIGITAL_GET
  public .


  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_SERVICE type /UI2/SERVICE_NAME .
  class-data AT_OBJECT type ref to ZIF_INTEGRACAO_ADIGITAL_GET .
  data AT_DOC_TAB type ZINS_ADIGITAL_CONTRATOS .
  data AT_BAPIRET2_TAB type BAPIRET2_T .

  class-methods GET_INSTANCE
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ADIGITAL_GET
    raising
      ZCX_INTEGRACAO .
  methods GET_DOCUMENTS
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE optional
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ADIGITAL_GET
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SEND_TO_APPROVAL
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ADIGITAL_GET
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods BEFORE_SENDING
    importing
      !I_REFERENCE type STRING
    returning
      value(R_ATTACHMENTS) type ZINS_ADIGITAL_ATTACHMENTS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SEND_TO_DESTINATION
    importing
      !I_DOCUMENT type ZINS_ADIGITAL_CONTRACT
      !IT_ANEXOS type ZINS_ADIGITAL_ATTACHMENTS
    returning
      value(R_ADIGITAL) type ZINT_ASSINA01
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods AFTER_SENDING
    importing
      !I_ADIGITAL type ZINT_ASSINA01
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ADIGITAL_GET
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods EXECUTE_MULTIPART
    importing
      !I_MULTI_TAB type ZDE_MULTIPART_FIELD_T
      !I_PARAMS type STRING default 'POST'
      !I_METHOD type ZDE_HTTP_METODO optional
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
  methods LOOKUP_VALUES_EXECUTE
    importing
      !I_LOOKUP_ID type STRING
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO .
  methods GET_MESSAGES
    returning
      value(R_RET) type BAPIRET2_T .
  methods SET_MESSAGE
    importing
      !I_MESSAGE type STRING
      !I_MSGTY type MSGTY optional
      !I_DOC_ID type STRING optional .
  methods GET_DOCUMENTS_BY_ID
    importing
      !ID_REFERENCIA type ZIN_ID_REFERENCIA
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ADIGITAL_GET
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_ASSINANTES
    importing
      !I_ID type STRING
    returning
      value(R_RELACAO) type STRING .
  methods LOG_UPDATE
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ADIGITAL_GET .
  methods PROCESS_CONTRACTS
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE optional
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ADIGITAL_GET
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
