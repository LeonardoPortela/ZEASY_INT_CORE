interface ZIF_INTEGRACAO_HIST_FAT_COUPA
  public .


  data AT_CLIENTE_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_COUPA type ref to ZIF_INTEGRACAO_USER_COUPA .
  constants AT_FC_END_POINT type STRING value '/application/xml/' ##NO_TEXT.
  class-data AT_SERVICO type ZTIPOWEBSERV .
  data AT_RECIBO type TCURR .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_TOKEN_WS type ZAUTH_WEBSERVICE .
  data AT_XML type STRING .
  data AT_RETORNO type CHAR1 .
  data AT_SERVICOS type ZCOUPA_TT_SERVICOS .
  data AT_TT_RETORNO type ZCOUPA_TT_SERVICOS_RETORNO .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_USER_BUSCAR
    importing
      !I_CLIENTE_ID type STRING optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      value(E_HTTP_URL) type STRING optional
      value(E_METODO) type STRING optional
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA
    raising
      ZCX_INTEGRACAO .
  methods GET_XML
    exporting
      !E_XML type STRING
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_XML type STRING
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional .
  methods SET_RECIBO_BUSCAR
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA .
  methods GET_RETORNO
    exporting
      !E_HISTORICO type STRING
      !E_RETORNO type CHAR1
    returning
      value(R_IF_INTEGRACAO_HIST_FAT_COUPA) type ref to ZIF_INTEGRACAO_HIST_FAT_COUPA .
endinterface.
