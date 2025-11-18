interface ZIF_INTEGRACAO_LOOKUP_COUPA
  public .


  class-data AT_IF_INTEGRACAO_LOOKUP_COUPA type ref to ZIF_INTEGRACAO_LOOKUP_COUPA .
  class-data AT_SERVICO type ZTIPOWEBSERV .
  class-data AT_XML type STRING .
  class-data AT_ID_REFERENCIA type STRING .
  class-data AT_TP_REFERENCIA type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_LOOKUP_COUPA) type ref to ZIF_INTEGRACAO_LOOKUP_COUPA
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_LOOKUP_VALUE
    importing
      !IV_EXTERNAL_REF_NUM type STRING
      !IV_LOOKUP_NAME type STRING optional
    exporting
      !EV_ID type STRING .
  methods SET_DS_URL
    importing
      !IV_ID type STRING
    returning
      value(R_IF_INTEGRACAO_LOOKUP_COUPA) type ref to ZIF_INTEGRACAO_LOOKUP_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_XML type STRING
    returning
      value(R_IF_INTEGRACAO_LOOKUP_COUPA) type ref to ZIF_INTEGRACAO_LOOKUP_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_LOOKUP_COUPA) type ref to ZIF_INTEGRACAO_LOOKUP_COUPA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional .
  methods SET_INIT_IMPORT
    importing
      !IS_COUPA_IMPORT_DATA type ZCOUPA_IMPORT_DATA
    exporting
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
