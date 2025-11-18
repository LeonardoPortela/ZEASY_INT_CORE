interface ZIF_INTEGRACAO_COUPA_GRP_MERCA
  public .


  data AT_CLIENTE_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INT_COUPA_GRP_MERCA type ref to ZIF_INTEGRACAO_COUPA_GRP_MERCA .
  constants AT_FC_END_POINT type STRING value '["id","category","name","active",{"parent":["id","name","category",{"custom_fields":{}}]},{"custom_fields":{}}]' ##NO_TEXT. "ID","CATEGORY","NAME","ACTIVE",{"PARENT":["ID","NAME","CATEGORY",{"CUSTOM_FIELDS":{
  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_GRP_MERCA_STRUC type ZMMS_GR_MERC_COUPA_ALV .
  data AT_XML type STRING .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_TOKEN_WS type ZAUTH_WEBSERVICE .
  data AT_XML_TEMPLATE type STRING .

  class-methods GET_INSTANCE
    importing
      !I_SERVICO type /UI2/SERVICE_NAME
    returning
      value(R_IF_INT_COUPA_GRP_MERCA) type ref to ZIF_INTEGRACAO_COUPA_GRP_MERCA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_COUPA_GRP_MERC) type ref to ZIF_INTEGRACAO_COUPA_GRP_MERCA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_GRP_MERCA_STRUC type ZMMS_GR_MERC_COUPA_ALV
    returning
      value(R_IF_INTEGRACAO_COUPA_GRP_MERC) type ref to ZIF_INTEGRACAO_COUPA_GRP_MERCA
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_COUPA_GRP_MERC) type ref to ZIF_INTEGRACAO_COUPA_GRP_MERCA
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_COUPA_GRP_MERC) type ref to ZIF_INTEGRACAO_COUPA_GRP_MERCA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_COUPA_GRP_MERC) type ref to ZIF_INTEGRACAO_COUPA_GRP_MERCA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional .
  methods ENVIAR_COUPA
    importing
      !I_GRP_MERCA_STRUC type ZMMS_GR_MERC_COUPA_ALV
    returning
      value(E_RET_INTR) type ZINTEGRCOUPA01
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
