interface ZIF_INTEGRACAO_ORCAMENTO_COUPA
  public .


  class-data AT_IF_INTEGRACAO_ORC_COUPA type ref to ZIF_INTEGRACAO_ORCAMENTO_COUPA .
  class-data AT_SERVICO type ZTIPOWEBSERV .
  class-data AT_XML type STRING .
  class-data AT_ID_REFERENCIA type STRING .
  class-data AT_TP_REFERENCIA type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_ZIF_INTEGRACAO_ORC_COUPA) type ref to ZIF_INTEGRACAO_ORCAMENTO_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_ZIF_INTEGRACAO_ORC_COUPA) type ref to ZIF_INTEGRACAO_ORCAMENTO_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_XML type STRING
    returning
      value(R_ZIF_INTEGRACAO_ORC_COUPA) type ref to ZIF_INTEGRACAO_ORCAMENTO_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
    returning
      value(R_ZIF_INTEGRACAO_ORC_COUPA) type ref to ZIF_INTEGRACAO_ORCAMENTO_COUPA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_INIT_IMPORT
    importing
      !IV_IS_APPROVED type XFLAG
      !IV_ID type STRING
      !IV_MENSAGEM type STRING
    exporting
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
