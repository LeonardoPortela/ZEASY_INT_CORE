interface ZIF_INTEGRACAO_VIAGEM_DESCARG
  public .


  class-data AT_IF_INTEGRACAO_DESCARG type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG .
  data AT_VIAGEM type ZLEST0185 .
  constants AT_FC_PROCESSAR_VIAGEM_DESCARG type ZDE_DS_FUNCAO_PROCESSA value '/viagens/id/descarregamento' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_DESCARG) type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_VIAGEM_DESCARREGAR
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID
      !I_DT_DESCARGA type ZDE_DT_CARREGAMENTO
      !I_PESO_LIQUIDO type ZDE_NM_PESO_CHEGADA
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_DESCARG) type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_DESCARG) type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_DESCARG) type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_DESCARG) type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_DESCARG) type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_DESCARG) type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_DESCARG) type ref to ZIF_INTEGRACAO_VIAGEM_DESCARG
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
