interface ZIF_INTEGRACAO_UPLOAD_AUTH
  public .


  class-data AT_IF_INTEGRACAO_UPLOAD_AUTH type ref to ZIF_INTEGRACAO_UPLOAD_AUTH .
  data AT_BLOB_PATH type STRING .
  constants AT_FC_PROCESSAR_STORAGE type ZDE_DS_FUNCAO_PROCESSA value '/storage/autorizar' ##NO_TEXT.
  data AT_BUKRS type BUKRS .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    importing
      !I_ID_REFERENCIA type ZDE_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_URL_UPLOAD type STRING
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_BLOB_PATH
    importing
      !I_BLOB_PATH type STRING
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH
    raising
      ZCX_INTEGRACAO .
  methods SET_EMPRESA
    importing
      !I_BUKRS type BUKRS
    returning
      value(R_IF_INTEGRACAO_UPLOAD_AUTH) type ref to ZIF_INTEGRACAO_UPLOAD_AUTH
    raising
      ZCX_INTEGRACAO .
endinterface.
