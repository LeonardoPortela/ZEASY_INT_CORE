interface ZIF_INTEGRACAO_COUPA_MATERIAL
  public .


  class-data AT_SERVICE type /UI2/SERVICE_NAME .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  class-data AT_INSTANCE type ref to ZIF_INTEGRACAO_COUPA_MATERIAL .
  data AT_BAPIRET2_TAB type BAPIRET2_T .

  class-methods GET_INSTANCE
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_COUPA_MATERIAL
    raising
      ZCX_INTEGRACAO .
  methods EXECUTE_SERVICE
    importing
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'POST'
      !I_BODY type STRING optional
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods BEFORE_EXECUTE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods AFTER_EXECUTE
    importing
      !R_IF_INTEGRACAO type ZINTEGRACAO .
  methods GET_MESSAGES
    returning
      value(R_RET) type BAPIRET2_T .
  methods SET_MESSAGE
    importing
      !I_MESSAGE type STRING
      !I_MSGTY type MSGTY optional .
endinterface.
