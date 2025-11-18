interface ZIF_INTEGRACAO_VIAGEM_CARRCAN
  public .


  class-data AT_IF_INTEGRACAO_CARRCAN type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN .
  data AT_VIAGEM type ZLEST0185 .
  constants AT_FC_PROCESSAR_VIAGEM_CARRCAN type ZDE_DS_FUNCAO_PROCESSA value '/viagens/id/carregamento/rejeitar' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_CARRCAN) type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_VIAGEM_CARRCAN
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID optional
      !I_TKNUM type TKNUM optional
      !I_REMESSAS type TAB_LIKP optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_CARRCAN) type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_CARRCAN) type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_CARRCAN) type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_CARRCAN) type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_CARRCAN) type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_CARRCAN) type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_CARRCAN) type ref to ZIF_INTEGRACAO_VIAGEM_CARRCAN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
