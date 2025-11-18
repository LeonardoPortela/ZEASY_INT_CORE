interface ZIF_INTEGRACAO_RONDONLINE
  public .


  class-data AT_IF_INTEGRACAO_RONDONLINE type ref to ZIF_INTEGRACAO_RONDONLINE .
  data AT_JSON type STRING .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_RONDONLINE) type ref to ZIF_INTEGRACAO_RONDONLINE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_RONDONLINE) type ref to ZIF_INTEGRACAO_RONDONLINE .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_RONDONLINE) type ref to ZIF_INTEGRACAO_RONDONLINE
    raising
      ZCX_INTEGRACAO .
  methods SET_INT_ESTOQUE
    importing
      !I_JSON type STRING
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_RONDONLINE) type ref to ZIF_INTEGRACAO_RONDONLINE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_RONDONLINE) type ref to ZIF_INTEGRACAO_RONDONLINE
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_RONDONLINE) type ref to ZIF_INTEGRACAO_RONDONLINE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
