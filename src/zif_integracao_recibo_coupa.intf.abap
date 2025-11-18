interface ZIF_INTEGRACAO_RECIBO_COUPA
  public .


  class-data AT_IF_INTEGRACAO_RECIBO_COUPA type ref to ZIF_INTEGRACAO_RECIBO_COUPA .
  class-data AT_SERVICO type ZTIPOWEBSERV .
  class-data AT_XML type STRING .
  class-data AT_ID_REFERENCIA type STRING .
  class-data AT_TP_REFERENCIA type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_COUPA) type ref to ZIF_INTEGRACAO_RECIBO_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_COUPA) type ref to ZIF_INTEGRACAO_RECIBO_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_XML type STRING
    returning
      value(R_IF_INTEGRACAO_COUPA) type ref to ZIF_INTEGRACAO_RECIBO_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_COUPA) type ref to ZIF_INTEGRACAO_RECIBO_COUPA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      !I_SERVICO type ZTIPOWEBSERV .
  methods SET_INIT_IMPORT
    importing
      !IS_IMPORT_DATA type ZCOUPA_RECIBO
    exporting
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods CONVERT_ABAP_TO_XML
    importing
      !IS_IMPORT_DATA type ZCOUPA_RECIBO
    returning
      value(RV_XML) type STRING .
endinterface.
