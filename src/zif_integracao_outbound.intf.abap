interface ZIF_INTEGRACAO_OUTBOUND
  public .


  class-data AT_IF_INTEGRACAO_OUTBOUND type ref to ZIF_INTEGRACAO_OUTBOUND .
  data AT_AUTH_WEBSERVICE type ZAUTH_WEBSERVICE .
  data AT_AUTH_WEBSERVICE_BUKRS type ZAUTH_WS_0001_T .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO .
  methods EXECUTE_REQUEST
    importing
      !I_INFO_REQUEST type ANY optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_URL
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO .
  methods GET_DATA
    exporting
      !E_DATA type STRING
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO .
  methods SET_DATA
    importing
      !I_DATA type STRING
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO .
  methods SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods BUILD_INFO_REQUEST
    importing
      !I_INFO_REQUEST type ANY optional
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
