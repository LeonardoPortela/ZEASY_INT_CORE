interface ZIF_INTEGRACAO_VIAGEM_CARREGAR
  public .


  class-data AT_IF_INTEGRACAO_CARREGAR type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR .
  data AT_VIAGEM type ZLEST0185 .
  constants AT_FC_PROCESSAR_VIAGEM_CARREGA type ZDE_DS_FUNCAO_PROCESSA value '/viagens/id/carregamento' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  class-methods SET_VALIDA_ENVIO_CARREGAMENTO
    importing
      !I_CH_REFERENCIA type ZCH_REF optional
    returning
      value(E_ERRO) type CHAR1
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_VIAGEM_CARREGAR
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID optional
      !I_DT_CARREGAMENTO type ZDE_DT_CARREGAMENTO
      !I_PESO_TARA type ZDE_NM_PESO_TARA optional
      !I_PESO_LIQUIDO type ZDE_NM_PESO_LIQUIDO optional
      !I_REMESSAS type TAB_LIKP optional
      !I_TKNUM type TKNUM optional
      !I_NFE_CARREGAMENTO type ZLESS0007 optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_CARREGAR) type ref to ZIF_INTEGRACAO_VIAGEM_CARREGAR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
