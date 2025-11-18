interface ZIF_INTEGRACAO_COUPA_REQUISIT
  public .


  class-data AT_IF_INTEGRACAO_REQUISIT type ref to ZIF_INTEGRACAO_COUPA_REQUISIT .
  class-data AT_SERVICO type ZTIPOWEBSERV .
  class-data AT_XML type STRING .
  class-data AT_ID_REFERENCIA type STRING .
  class-data AT_TP_REFERENCIA type STRING .
  data AT_REQUISICAO_COMPRA_COUPA type ZSREQUISICAO_COMPRA_COUPA .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_REQUISIT) type ref to ZIF_INTEGRACAO_COUPA_REQUISIT
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_REQUISIT) type ref to ZIF_INTEGRACAO_COUPA_REQUISIT
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_REQUISIT) type ref to ZIF_INTEGRACAO_COUPA_REQUISIT .
endinterface.
