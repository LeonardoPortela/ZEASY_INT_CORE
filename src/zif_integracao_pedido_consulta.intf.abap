interface ZIF_INTEGRACAO_PEDIDO_CONSULTA
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  data AT_CONSULTA type ZDE_PEDIDO_CONSULTA .
  class-data AT_INTEGRACAO_PEDI_CONSULTA type ref to ZIF_INTEGRACAO_PEDIDO_CONSULTA .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .

  class-methods GET_INSTANCE
    returning
      value(R_INTEGRACAO_PEDIDO_CONSULTA) type ref to ZIF_INTEGRACAO_PEDIDO_CONSULTA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_INTEGRACAO_PEDIDO_CONSULTA) type ref to ZIF_INTEGRACAO_PEDIDO_CONSULTA
    raising
      ZCX_INTEGRACAO .
  methods GET_VALIDAR_DADOS
    returning
      value(R_INTEGRACAO_PEDIDO_CONSULTA) type ref to ZIF_INTEGRACAO_PEDIDO_CONSULTA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_INTEGRACAO_PEDIDO_CONSULTA) type ref to ZIF_INTEGRACAO_PEDIDO_CONSULTA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
