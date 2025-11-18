interface ZIF_INTEGRACAO_UPLOAD_EXEC
  public .


  class-data AT_IF_INTEGRACAO_UPLOAD_EXEC type ref to ZIF_INTEGRACAO_UPLOAD_EXEC .
  constants AT_FC_PROCESSAR_STORAGE type ZDE_DS_FUNCAO_PROCESSA value '/storage/executar' ##NO_TEXT.
  data AT_BUKRS type BUKRS .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_UPLOAD_EXEC) type ref to ZIF_INTEGRACAO_UPLOAD_EXEC
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      !I_URL_UPLOAD type STRING optional
      !I_VALIDA_TOKEN type CHAR1 optional
    returning
      value(R_IF_INTEGRACAO_UPLOAD_EXEC) type ref to ZIF_INTEGRACAO_UPLOAD_EXEC
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    importing
      !I_FILE_BIN type ZDE_DATA_XSTRING
    exporting
      !E_JSON_XSTRING type ZDE_DATA_XSTRING
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_UPLOAD_EXEC) type ref to ZIF_INTEGRACAO_UPLOAD_EXEC
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING optional
      !I_JSON_XSTRING type ZDE_DATA_XSTRING optional
    returning
      value(R_IF_INTEGRACAO_UPLOAD_EXEC) type ref to ZIF_INTEGRACAO_UPLOAD_EXEC
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    importing
      !I_ID_REFERENCIA type ZDE_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_UPLOAD_EXEC) type ref to ZIF_INTEGRACAO_UPLOAD_EXEC .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_UPLOAD_EXEC) type ref to ZIF_INTEGRACAO_UPLOAD_EXEC .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_UPLOAD_EXEC) type ref to ZIF_INTEGRACAO_UPLOAD_EXEC
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_EMPRESA
    importing
      !I_BUKRS type BUKRS
    returning
      value(R_IF_INTEGRACAO_UPLOAD_EXEC) type ref to ZIF_INTEGRACAO_UPLOAD_EXEC
    raising
      ZCX_INTEGRACAO .
endinterface.
