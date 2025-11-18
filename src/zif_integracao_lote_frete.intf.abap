interface ZIF_INTEGRACAO_LOTE_FRETE
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_LOTE_FRETE type ref to ZIF_INTEGRACAO_LOTE_FRETE .
  data AT_LOTE_FRETE type ZLEST0181 .
  data AT_LOTE_FRETE_OLD type ZLEST0181 .
  constants AT_OPERACAO_TELA_I type ZDE_OPERACAO_TELA value 'I' ##NO_TEXT.
  constants AT_OPERACAO_TELA_U type ZDE_OPERACAO_TELA value 'U' ##NO_TEXT.
  constants AT_OPERACAO_TELA_D type ZDE_OPERACAO_TELA value 'D' ##NO_TEXT.
  constants AT_TP_PARCEIRO_REME_OV type J_1BPARVW value 'AG' ##NO_TEXT.
  constants AT_TP_PARCEIRO_DEST_OV type J_1BPARVW value 'WE' ##NO_TEXT.
  constants AT_TP_PARCEIRO_COLE_OV type J_1BPARVW value 'PC' ##NO_TEXT.
  constants AT_TP_PARCEIRO_COLE_PC type J_1BPARVW value 'PR' ##NO_TEXT.
  constants AT_TP_PARCEIRO_ENTR_OV type J_1BPARVW value 'LR' ##NO_TEXT.
  constants AT_TP_PARCEIRO_ENTR_PC type J_1BPARVW value 'ZT' ##NO_TEXT.
  constants AT_TP_PARCEIRO_FRETE_OV type J_1BPARVW value 'SP' ##NO_TEXT.
  constants AT_TP_PARCEIRO_PORTO_OV type J_1BPARVW value 'Z1' ##NO_TEXT.
  constants AT_TP_PARCEIRO_FORNECEDOR type J_1BPARTYP value 'V' ##NO_TEXT.
  constants AT_TP_PARCEIRO_CLIENTE type J_1BPARTYP value 'C' ##NO_TEXT.
  constants AT_TP_PARCEIRO_LOCAL_NEGOCIO type J_1BPARTYP value 'B' ##NO_TEXT.
  constants AT_FC_PROCESSAR_CANCELAR type ZDE_DS_FUNCAO_PROCESSA value 'CANCELAR' ##NO_TEXT.
  constants AT_FC_PROCESSAR_FINALIZAR type ZDE_DS_FUNCAO_PROCESSA value 'FINALIZAR' ##NO_TEXT.
  constants AT_FC_PROCESSAR_VOLUME type ZDE_DS_FUNCAO_PROCESSA value 'VOLUME' ##NO_TEXT.
  constants AT_FC_PROCESSAR_CRIAR type ZDE_DS_FUNCAO_PROCESSA value 'CRIAR' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO .
  class-methods SET_GERENCIA_LOTE
    importing
      !I_ORDEM_VENDA type DATA optional
      !I_PEDIDO_COMPRA type DATA optional
      !I_SOLICITACAO type DATA optional
      !I_NRO_CG_SAI type DATA optional
      !I_SINCRONIA type ZDE_TP_SINCRONIA optional
    exporting
      !E_ID_LOTE_FRETE type ZDE_ID_LOTE_FRETE
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  class-methods SET_ENVIA_FERTILIZANTES_OV
    importing
      !I_VBELN type VBELN_VA optional
      value(T_VBAP) type VBAP_T optional
    exporting
      value(E_ERRO) type CHAR1
      value(E_ERRO_MESG) type BAPI_MSG
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_DATA
    importing
      !I_DATA type STRING
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    importing
      !I_TIPO_JSON type ZDE_OPERACAO_TELA
    exporting
      !R_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SEND_MSG
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_NEW_LOTE_FRETE
    importing
      !I_ORDEM_VENDA type DATA optional
      !I_PEDIDO_COMPRA type DATA optional
      !I_SOLICITACAO type DATA optional
      !I_NRO_CG_SAI type DATA optional
    exporting
      !E_ID_LOTE_FRETE type ZDE_ID_LOTE_FRETE
      !E_LOTE_FRETE type ZLEST0181
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_NEW_ID_LOTE_FRETE
    changing
      !E_ID_LOTE_FRETE type ZDE_ID_LOTE_FRETE
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_ERROR .
  methods SET_DELETE_LOTE_FRETE
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_ERROR .
  methods SET_REGISTRO
    importing
      !I_ID_LOTE_FRETE type ZDE_ID_LOTE_FRETE
      !I_ORDEM_VENDA type DATA optional
      !I_PEDIDO_COMPRA type DATA optional
      !I_SOLICITACAO type DATA optional
      !I_NRO_CG_SAI type ZNRO_CG optional
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO .
  methods SET_NEW_LOTE_FRETE_OV
    importing
      !I_ORDEM_VENDA type ZDE_CARGUEIRO_OV
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_NEW_LOTE_FRETE_PC
    importing
      !I_PEDIDO_COMPRA type ZDE_CARGUEIRO_PC
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE .
  methods GET_ID_MATERIAL_CARGUERO
    importing
      !I_BUKRS type STRING
      !I_MATERIAL type STRING optional
      !I_MATKL type STRING optional
      !I_TP_MATERIAL type STRING optional
    exporting
      !E_ID_MATERIAL_CARGUERO type ZDE_ID_MT_CARGUERO
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_ID_UNI_ORG_CARGUERO
    importing
      !I_BUKRS type STRING
      !I_BRANCH type STRING
      !I_UNIDADE_MAGGI type STRING
    exporting
      !E_ID_UNG_CARGUERO type ZDE_ID_UNG_CARGUERO
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_LIMPAR
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(RIF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE .
  methods SET_GERENCIA_LOTE_OV
    importing
      !I_ORDEM_VENDA type DATA
    exporting
      !E_CK_NOVO type CHAR01
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_GERENCIA_LOTE_PC
    importing
      !I_PEDIDO_COMPRA type DATA
    exporting
      !E_CK_NOVO type CHAR01
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_GERENCIA_LOTE_SL
    importing
      !I_SOLICITACAO type DATA
    exporting
      !E_CK_NOVO type CHAR01
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_NEW_LOTE_FRETE_SL
    importing
      !I_SOLICITACAO type ZDE_CARGUEIRO_SL
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_GERENCIA_LOTE_CG_SAI_IN
    importing
      !I_NRO_CG type ZNRO_CG
    exporting
      !E_CK_NOVO type CHAR01
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_NEW_LOTE_FRETE_CG_SAI_IN
    importing
      !I_NRO_CG type ZNRO_CG
    returning
      value(R_IF_INTEGRACAO_LOTE_FRETE) type ref to ZIF_INTEGRACAO_LOTE_FRETE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
