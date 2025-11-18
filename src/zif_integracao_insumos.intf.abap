interface ZIF_INTEGRACAO_INSUMOS
  public .


  class-data AT_IF_INTEGRACAO_INSUMOS type ref to ZIF_INTEGRACAO_INSUMOS .
  data AT_JSON type STRING .
  data AT_METODO type STRING .
  data AT_ID_REFERENCIA type STRING .
  data AT_WEBSERVICE type ZAUTH_WEBSERVICE .
  data AT_SET_TOKEN type STRING .
  data AT_NRO_CGD type ZNRO_CG .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_NR_VENDA type ZNR_VENDA .
  data AT_TIPO_DOC type ZTIPO_DOC .
  data AT_ID_DOCUMENTO type ZID_DOCUMENTO .
  data AT_T_ZSDT0310 type ZSDT0310_T .
  data AT_T_ZSDT0311 type ZSDT0311_T .
  data AT_T_ZSDT0312 type ZSDT0312_T .
  data AT_T_ZSDT0013 type ZSDCT0013 .
  data AT_T_ZSDT0014 type ZSDCT0014 .
  data AT_T_ZSDT0015 type ZSDCT0015 .
  constants AT_CANCELADO type ZSTATUS_DOC value '10' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO .
  class-methods SET_FORMATA_DATA
    importing
      !I_DATA type DATUM
    returning
      value(E_DATA) type STRING .
  methods SET_GRAVAR_LOG
    importing
      !I_NR_VENDA type ZNR_VENDA
      !I_TIPO_DOC type ZTIPO_DOC
      !I_ID_DOCUMENTO type ZID_DOCUMENTO
      !I_TIPO_MSG type BAPI_MTYPE optional
      !I_MENSAGEM type STRING
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS .
  methods SET_GRAVAR_PARTICIPANTES
    importing
      !T_ZSDT0310 type ZSDT0310_T
      !T_PARTICIPANTES type ZSDE0036_T
      !I_METODO type STRING optional
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS .
  methods SET_JSON_CRIAR_DOCTO
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS .
  methods SET_MENSAGEM
    importing
      !I_COD_MSG type CHAR02
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      !I_METODO type STRING
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS .
  methods GET_METODO
    exporting
      !E_METODO type STRING
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_HEADER_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS .
  methods SET_HEADER
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS .
  methods SET_EXEC_INSUMOS
    importing
      !I_METODO type STRING
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CRIAR_DOCUMENTO
    importing
      !I_NR_VENDA type ZNR_VENDA
      !I_TIPO_DOC type ZTIPO_DOC
      !I_ID_DOCUMENTO type ZID_DOCUMENTO
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_OBTER_PARTICIPANTES
    importing
      !I_NR_VENDA type ZNR_VENDA
      !I_TIPO_DOC type ZTIPO_DOC
      !I_ID_DOCUMENTO type ZID_DOCUMENTO
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SOLICITAR_ASSINATURA
    importing
      !I_NR_VENDA type ZNR_VENDA
      !I_TIPO_DOC type ZTIPO_DOC
      !I_ID_DOCUMENTO type ZID_DOCUMENTO
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_OBTER_PDF_ASSINADO
    importing
      !I_DATA type ZINC_BRY_IN_COLETA
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_BAIXAR_PDF_ASSINADO
    importing
      !I_NR_DOC_GERADO type ZNR_DOC_GERADO
      !I_ID_DOC_AGRUPADOR type ZID_AGRUPADOR
    exporting
      !E_PDF_ASSINADO type XSTRING
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_OBTER_STATUS_ASSINATURA
    importing
      !I_ID_DOCUMENTO type ZID_AGRUPADOR
    exporting
      !T_STATUS_ASSINATURAS type ZSDE0075_T
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CANCELAR_COLETA
    importing
      !I_ID_DOCUMENTO type ZID_AGRUPADOR
      !I_CANCELAR_DOCUMENTO type CHAR1 optional
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CANCELAR_DOC_SIGAM
    importing
      !I_ID_DOCUMENTO type ZID_AGRUPADOR
    returning
      value(R_IF_INTEGRACAO_INSUMOS) type ref to ZIF_INTEGRACAO_INSUMOS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
