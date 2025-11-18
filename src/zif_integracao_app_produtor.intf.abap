interface ZIF_INTEGRACAO_APP_PRODUTOR
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_APP_PRODUTOR type ref to ZIF_INTEGRACAO_APP_PRODUTOR .
  data AT_RETORNO_HEADER type ZSDE006_T .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_RSPARAMS type RSPARAMS_TT .
  data AT_TCON type ZRSDSSELOPTS .
  data AT_CONT type ZRSDSSELOPTS .
  data AT_ORGV type ZRSDSSELOPTS .
  data AT_CDIS type ZRSDSSELOPTS .
  data AT_SATI type ZRSDSSELOPTS .
  data AT_FATU type ZRSDSSELOPTS .
  data AT_CLIE type ZRSDSSELOPTS .
  data AT_DATE type ZRSDSSELOPTS .
  data AT_RETORNO_ITEM type ZSDE002_T .
  data AT_TIPO type CHAR1 .
  data AT_RETORNO_REMESSA type ZSDE010_T .
  data AT_POSNR type ZRSDSSELOPTS .
  data AT_FORNECEDOR_SAP type LIFNR .
  data AT_RETORNO_TERMO type ZSDE012 .
  data AT_ACEITE type CHAR1 .
  data AT_RETORNO_ACEITE type ZMMS007 .
  data AT_PROTOCOLO type STRING .
  data AT_ORDENS type ZRSDSSELOPTS .
  data AT_INSCESTADUAL type ZRSDSSELOPTS .
  data AT_CLIENTE type KUNNR .
  data AT_ORIGEM type REPRES .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_APP_PRODUTOR) type ref to ZIF_INTEGRACAO_APP_PRODUTOR
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
      !ID_REFERENCIA type STRING optional
    returning
      value(R_IF_INTEGRACAO_APP_PRODUTOR) type ref to ZIF_INTEGRACAO_APP_PRODUTOR
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_APP_PRODUTOR) type ref to ZIF_INTEGRACAO_APP_PRODUTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_SOLICITACOES
    returning
      value(R_IF_INTEGRACAO_APP_PRODUTOR) type ref to ZIF_INTEGRACAO_APP_PRODUTOR .
  methods GET_ORDEM_VENDAS
    returning
      value(R_IF_INTEGRACAO_APP_PRODUTOR) type ref to ZIF_INTEGRACAO_APP_PRODUTOR .
  methods GET_REMESSAS
    returning
      value(R_IF_INTEGRACAO_APP_PRODUTOR) type ref to ZIF_INTEGRACAO_APP_PRODUTOR .
  methods SET_TERMOS_ACEITE
    returning
      value(R_IF_INTEGRACAO_APP_PRODUTOR) type ref to ZIF_INTEGRACAO_APP_PRODUTOR .
  methods SET_ENVIA_AL5
    importing
      !ACEITE type INDTYP
      !FORNECEDOR_SAP type LIFNR
    returning
      value(R_IF_INTEGRACAO_APP_PRODUTOR) type ref to ZIF_INTEGRACAO_APP_PRODUTOR .
endinterface.
