interface ZIF_INTEGRACAO_INJECT
  public .


  data AT_ID_INTERFACE type ZDE_ID_INTERFACE .
  data AT_TP_INTEGRACAO type ZDE_TP_INTEGRACAO .
  data AT_TP_CANAL type ZDE_TP_CANAL_COMUNICACAO .
  data AT_TP_SINCRONIA type ZDE_TP_SINCRONIA .
  data AT_INFO_REQUEST_HTTP type ZDE_INTEGRACAO_HTTP_CONFIG .
  data AT_MSG_INBOUND type STRING .
  data AT_REFERENCIA type ZDE_CHAVE_REFERENCIA .
  data AT_ANONIMO type ZDE_CK_CHAMADA_ANONIMA .
  data AT_AUTENTICA_OPUS type ZDE_AUTENTICA_OPUS .
  data AT_SEND_AUTENTICAO type ZDE_SEND_ATENTICACAO .
  data AT_HEADER_FIELDS type ZDE_HEADER_FIELD_T .
  data AT_MULTIPART_FIELDS type ZDE_MULTIPART_FIELD_T .
  data AT_AUTENTICA_API_AD type ZDE_AUTENTICA_API_AD .
  data AT_AUTENTICA_MODULE type ZDE_AUTENTICA_MODULE .
  data AT_FORM_FIELDS type ZDE_HEADER_FIELD_T .
  constants CO_REQUEST_METHOD_GET type STRING value 'GET' ##NO_TEXT.
  constants CO_REQUEST_METHOD_POST type STRING value 'POST' ##NO_TEXT.
  constants CO_REQUEST_METHOD_DELETE type STRING value 'DELETE' ##NO_TEXT.
  constants CO_REQUEST_METHOD_PUT type STRING value 'PUT' ##NO_TEXT.
  data AT_NOT_STORE_LOG type ZDE_NOT_STORE_LOG .
  data AT_PROPERTYTYPE_LOGON_POPUP type I .

  methods SET_BEFORE_SEND_OUTBOUND_MSG
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO .
  methods SET_BEFORE_ERROR_OUTBOUND_MSG
    importing
      !I_MSG type STRING optional
    exporting
      !E_SUCESSO type CHAR01
    changing
      !C_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT .
  methods SET_PROCESSA_INBOUND
    importing
      !I_MSG type STRING
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !I_MSG_COMPLETA type ZINTEGRACAO
    exporting
      !E_SUCESSO type CHAR01
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_PROCESSA_RETORNO
    importing
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !I_MSG_COMPLETA type ZINTEGRACAO
      !I_MSG_RETORNO type STRING
    exporting
      !E_SUCESSO type CHAR01
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO .
  methods SET_INTEGRAR_INBOUND
    importing
      !I_MSG_INBOUND type STRING
      !I_MSG_COMPLETA type ZINTEGRACAO
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO optional
    exporting
      !E_MSG_OUTBOUND type STRING
      !E_SUCESSO type CHAR01
      !E_NM_CODE type CHAR03
      !E_MSG_ERRO type STRING
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_INTEGRAR_RETORNO
    importing
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !I_MSG_COMPLETA type ZINTEGRACAO
      !I_MSG_RETORNO type STRING
    exporting
      !E_SUCESSO type CHAR01
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_HEADER_REQUEST_HTTP
    importing
      !I_HEADER_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO .
  methods GET_HEADER_REQUEST_HTTP
    exporting
      !E_HEADER_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT
    raising
      ZCX_INTEGRACAO .
  methods SET_PARAMETRO
    importing
      value(I_WERKS) type WERKS_D optional
      value(I_LGORT) type LGORT_D optional
      value(I_CHARG) type CHARG_D optional
      value(I_CD_SAI) type CHAR20 optional
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT .
  methods GET_FORM_REQUEST_HTTP
    exporting
      !E_FORM_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT .
  methods SET_FORM_REQUEST_HTTP
    importing
      !I_FORM_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT .
endinterface.
