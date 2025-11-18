interface ZIF_INTEGRACAO_BRY_ADIGITAL
  public .


  data AT_END_POINT type STRING .
  class-data AT_SERVICO type /UI2/SERVICE_NAME .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_JSON_PARTI_TEMPLATE type STRING .
  data AT_JSON_GRUPO_PARTI_TEMPLATE type STRING .
  data AT_JSON_PARTICIPANTES type STRING .
  data AT_JSON_GRUPO_PARTICIPANTES type STRING .
  class-data AT_INSTANCE type ref to ZIF_INTEGRACAO_BRY_ADIGITAL .
  data AT_DADOS type ZINS_DADOS_BRY_DADOS .
  data AT_ANEXOS type ZINS_ADIGITAL_ATTACHMENTS .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INT_BRY_ADIGITAL) type ref to ZIF_INTEGRACAO_BRY_ADIGITAL
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_OBJECT) type ref to ZIF_INTEGRACAO_BRY_ADIGITAL
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_BRY_ADIGITAL
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_BRY_ADIGITAL
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods ENVIAR_BRY
    exporting
      value(E_JSONX) type XSTRING
    returning
      value(E_JSON) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_END_POINT .
  methods GET_PARTICIPANTES
    returning
      value(R_JSON) type STRING .
  methods GET_GRUPO_PARTICIPANTES
    returning
      value(R_JSON) type STRING .
  methods BUSCAR_BRY
    returning
      value(E_JSON) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
