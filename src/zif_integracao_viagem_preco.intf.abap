interface ZIF_INTEGRACAO_VIAGEM_PRECO
  public .


  class-data AT_IF_INTEGRACAO_VIAGEM_PRECO type ref to ZIF_INTEGRACAO_VIAGEM_PRECO .
  data AT_VIAGEM type ZLEST0185 .
  constants AT_FC_PROCESSAR_VIAGEM_PRECO type ZDE_DS_FUNCAO_PROCESSA value '/viagens/id/aprova' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_VIAGEM_PRECO) type ref to ZIF_INTEGRACAO_VIAGEM_PRECO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_VIAGEM_PRECO_MOTORISTA
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID
      !I_WAERS type WAERS
      !I_FRETE type ZVALOR_FRETE
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_VIAGEM_PRECO) type ref to ZIF_INTEGRACAO_VIAGEM_PRECO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_VIAGEM_PRECO) type ref to ZIF_INTEGRACAO_VIAGEM_PRECO
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_VIAGEM_PRECO) type ref to ZIF_INTEGRACAO_VIAGEM_PRECO
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_VIAGEM_PRECO) type ref to ZIF_INTEGRACAO_VIAGEM_PRECO
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_VIAGEM_PRECO) type ref to ZIF_INTEGRACAO_VIAGEM_PRECO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_VIAGEM_PRECO) type ref to ZIF_INTEGRACAO_VIAGEM_PRECO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_VIAGEM_PRECO) type ref to ZIF_INTEGRACAO_VIAGEM_PRECO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
