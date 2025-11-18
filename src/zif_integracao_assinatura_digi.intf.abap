interface ZIF_INTEGRACAO_ASSINATURA_DIGI
  public .


  data AT_FC_END_POINT type STRING .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_TOKEN_WS type ZAUTH_WEBSERVICE .
  data AT_SERVICE type /UI2/SERVICE_NAME .
  class-data AT_IF_INT_ASSINATURA_DIGI type ref to ZIF_INTEGRACAO_ASSINATURA_DIGI .
  data AT_COLETA_TAB type ZINS_DADOS_INDEX_TAB .
  data AT_ID_COUNT type INT4 .
  data AT_ID_CURRENT type INT4 .
  data AT_ANEXOS type ZINC_DADOS_ANEXOS .
  data AT_ASSINA_TAB type ZINT_ASSINA01_TAB .
  data AT_DOCUMENTOS type ZINC_DADOS_XML_TAB .
  data AT_STEP_TAB type ZINC_COUPA_STEPS .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_ASSINATURA_DIG) type ref to ZIF_INTEGRACAO_ASSINATURA_DIGI
    raising
      ZCX_INTEGRACAO .
  methods GET_DOCUMENTS
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ASSINATURA_DIGI
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SEND_DOCUMENT_TO_APPROVAL
    returning
      value(R_RET) type ref to ZIF_INTEGRACAO_ASSINATURA_DIGI
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SEND_ATTACHMENT_TO_BRY
    importing
      value(I_DOCUMENT) type ZINS_DADOS_XML_TAB
      !IT_ANEXOS type ZINC_DADOS_ANEXOS
    returning
      value(R_ADIGITAL) type ZINT_ASSINA01
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SEND_WORKFLOW_KEY
    importing
      !I_DOCUMENT type ZINT_ASSINA01
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SEND_APPROVE
    importing
      value(I_DOCID) type ZIN_ID_REFERENCIA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ENDPOINT
    importing
      !I_ENDPOINT type STRING .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_ASSINATURA_DIG) type ref to ZIF_INTEGRACAO_ASSINATURA_DIGI .
  methods SEND_TO_SERVICE
    importing
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'PUT'
      !I_BODY type STRING optional
    returning
      value(R_XML) type ref to CL_XML_DOCUMENT
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_FROM_SERVICE
    importing
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'GET'
    returning
      value(R_XML) type ref to CL_XML_DOCUMENT
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GETX_FROM_SERVICE
    importing
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'GET'
    returning
      value(R_XSTRING) type ZDE_DATA_XSTRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods CONVERT_NODE_XML_TO_TABLE
    importing
      !I_INDEX type INT4 default 1
      !I_SEQ type INT4 default 1
    changing
      !CO_NODE type ref to IF_IXML_NODE optional
      value(C_COLETA_TAB) type ZINS_DADOS_INDEX_TAB .
  methods STRING_TO_XML_OBJECT
    importing
      !I_STRING_XML type STRING
    returning
      value(R_XML) type ref to CL_XML_DOCUMENT .
  methods GET_BODY_WORKFLOW_KEY
    importing
      !I_CHAVE type ZIN_CHAVE_COLETA
    returning
      value(R_RET) type STRING .
  methods CONVERT_XML_TO_DOCUMENT
    importing
      !I_XML_OBJECT type ref to CL_XML_DOCUMENT
    returning
      value(R_RET) type ZINC_DADOS_XML_TAB .
  methods CONVERT_XML_TO_ATTACHMENT
    importing
      !I_XML_OBJECT type ref to CL_XML_DOCUMENT
    returning
      value(R_RET) type ZINC_DADOS_ANEXOS .
  methods REMOVE_SPEC_CHAR
    importing
      !I_VALUE type MSGXX
    returning
      value(R_VALUE) type MSGXX .
  methods SET_ATTACHMENT_PROP
    importing
      !I_FILE_PATH type MSGXX
    exporting
      !E_APPLICATION_TYPE type MSGXX
      !E_FILE_NAME type MSGXX .
  methods GUI_INDICATOR_EXE .
  methods LOG_GENERATE
    importing
      !I_TEXT type STRING
      !I_MSGTY type MSGTY .
  methods JSON_REPLACE_HIFEN
    importing
      !I_VALUE type STRING
    returning
      value(R_VALUE) type STRING .
endinterface.
