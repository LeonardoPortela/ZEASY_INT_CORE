interface ZIF_INTEGRACAO_VIAGEM_STATUS
  public .


  class-data AT_IF_INTEGRACAO_VIAGEM_STATUS type ref to ZIF_INTEGRACAO_VIAGEM_STATUS .
  data AT_VIAGEM type ZDE_VIAGEM_ID .
  constants AT_FC_PROCESSAR_VIAGEM_STATUS type ZDE_DS_FUNCAO_PROCESSA value '/viagens/id/status' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_VIAGEM_STATUS) type ref to ZIF_INTEGRACAO_VIAGEM_STATUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_VIAGEM_STATUS
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_STATUS type ZDE_STATUS_CARGUERO_VIAGEM
    returning
      value(R_IF_INTEGRACAO_VIAGEM_STATUS) type ref to ZIF_INTEGRACAO_VIAGEM_STATUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_VIAGEM_STATUS) type ref to ZIF_INTEGRACAO_VIAGEM_STATUS
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_VIAGEM_STATUS) type ref to ZIF_INTEGRACAO_VIAGEM_STATUS
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_VIAGEM_STATUS) type ref to ZIF_INTEGRACAO_VIAGEM_STATUS
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_VIAGEM_STATUS) type ref to ZIF_INTEGRACAO_VIAGEM_STATUS .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_VIAGEM_STATUS) type ref to ZIF_INTEGRACAO_VIAGEM_STATUS .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_STATUS type ZDE_STATUS_CARGUERO_VIAGEM
    returning
      value(R_IF_INTEGRACAO_VIAGEM_STATUS) type ref to ZIF_INTEGRACAO_VIAGEM_STATUS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
