interface ZIF_INTEGRACAO_BRY_ADIGITAL_IN
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_BRY_ADIGITAL_IN type ref to ZIF_INTEGRACAO_BRY_ADIGITAL_IN .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_COLETA_REC type ZINC_BRY_IN_COLETA .
  data AT_JSON type STRING .
  data AT_INTEGRACAO type ZINTEGRACAO .

  class-methods GET_INSTANCE
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_BRY_ADIGITAL_IN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_BRY_ADIGITAL_IN
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_BRY_ADIGITAL_IN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
