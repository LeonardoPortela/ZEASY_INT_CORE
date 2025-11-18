interface ZIF_INTEGRACAO_AGRIQ
  public .


  class-data AT_IF_INTEGRACAO_AGRIQ type ref to ZIF_INTEGRACAO_AGRIQ .
  data AT_JSON type STRING .
  data AT_METODO type STRING .
  data AT_ID_REFERENCIA type STRING .
  data AT_WEBSERVICE type ZAUTH_WEBSERVICE .
  data AT_SET_TOKEN type STRING .
  data AT_NRO_CGD type ZNRO_CG .
  data AT_CH_REFERENCIA type ZCH_REF .
  data AT_ZSDT0001_ITEM type ZSDT0001_ITEM_T .
  data AT_ID_SOL_RECEITA type STRING .
  data AT_RECEITAKEY type ZRECEITAKEY .
  data AT_TIPO_ASSINATURA type CHAR1 .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_PDF_RECEITA_AGRIQ type XSTRING .
  data AT_DATA_INICIO type STRING .
  data AT_DATA_FINAL type STRING .
  data AT_DATA_FIM type DATUM .
  data AT_ATIVO type STRING .
  data AT_PAGE type INTEGER4 .
  data AT_MULTIPART type ZDE_MULTIPART_FIELD_T .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO .
  methods SET_MENSAGEM
    importing
      !I_COD_MSG type CHAR02
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      !I_METODO type STRING
      !I_ATIVO type CHAR01 optional
      !I_DATA type DATUM optional
      !I_CODMAPA type STRING optional
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
  methods GET_METODO
    exporting
      !E_METODO type STRING
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_HEADER_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
  methods SET_HEADER
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
  methods SET_EXEC_AGRIQ
    importing
      !I_METODO type STRING
      !I_ATIVO type CHAR01 optional
      !I_DATA type DATUM optional
      !I_CODMAPA type STRING optional
      !I_RECEITAKEY type ZRECEITAKEY optional
    exporting
      !E_PDF_RECEITA_AGRIQ type XSTRING
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_EXEC_SIAGRI
    importing
      !I_METODO type STRING
    exporting
      !E_RECEITAS type ZSDE0070
      !E_RTC type ZSDE0081
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_GERAR_SOL_RA
    importing
      !I_NRO_CGD type ZNRO_CG
      !I_CH_REFERENCIA type ZCH_REF
      !I_EXIBE_POPUP type CHAR1 optional
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_GRAVA_ZSDT0302
    importing
      !I_GERA_RA type CHAR1
      !I_QTD_RA type NUMC3
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
  methods SET_ELIMINA_ZSDT0302
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
  methods SET_JSON_SOLIC_RA
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
  methods SET_JSON_ATUALIZA_NFE
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
  methods SET_CONSULTAR_RECEITA_SIAGRI
    importing
      !I_DATA_INICIO type DATUM optional
      !I_DATA_FIM type DATUM optional
    exporting
      !E_RECEITAS type ZSDE0070
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CONSULTAR_RTC_SIAGRI
    importing
      !I_ATIVO type CHAR01 optional
    exporting
      !E_RTC type ZSDE0081
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CONSULTAR_SOL_RA
    importing
      !I_NRO_CGD type ZNRO_CG
      !I_CH_REFERENCIA type ZCH_REF
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CANCELAR_SOL_RA
    importing
      !I_NRO_CGD type ZNRO_CG
      !I_CH_REFERENCIA type ZCH_REF
      !I_ELIMINA_REG type CHAR1 optional
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_DATA_AGRIQ
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
      !I_FUNCAO_PROCESSA type STRING optional
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG_AGRIQ
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ENVIAR_ASSINATURA
    importing
      !I_RECEITAKEY type ZRECEITAKEY
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_PDF_DOCUMENTO_ASSINADO
    importing
      !I_NRO_CGD type ZNRO_CG
      !I_CH_REFERENCIA type ZCH_REF
    exporting
      !T_PDF_DOCTO_ASSINADO type ZSDT_PDF_FILES
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ATUALIZA_NFE_RECEITA
    importing
      !I_DOCNUM type J_1BDOCNUM
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_BAIXAR_RECEITAS_CONTA
    importing
      !I_DATA_INICIO type DATUM
      !I_DATA_FIM type DATUM optional
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_FORMATA_DATA
    importing
      !I_DATA type DATUM
    exporting
      value(E_DATA) type STRING
    returning
      value(R_IF_INTEGRACAO_AGRIQ) type ref to ZIF_INTEGRACAO_AGRIQ .
endinterface.
