interface ZIF_INTEGRACAO_REC_KUHLMANN
  public .


  data AT_CLIENTE_ID type STRING .
  data AT_NUMERO_OS type STRING .
  data AT_ROMANEIO type STRING .
  data AT_ENTRADA_INICIO type STRING .
  data AT_ANALISE_INICIO type STRING .
  data AT_ENTRADA_FINAL type STRING .
  data AT_ANALISE_FINAL type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_REC_KUHLMANN type ref to ZIF_INTEGRACAO_REC_KUHLMANN .
  constants AT_FC_END_POINT type STRING value '/api/hvirecover' ##NO_TEXT.
  class-data AT_TIPO type STRING .
  class-data AT_SERVICO type STRING .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_REC_KUHLMANN) type ref to ZIF_INTEGRACAO_REC_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_HVI_RECUPERAR
    importing
      !I_CLIENTE_ID type STRING optional                       "Cliente
      !I_NUMERO_OS type STRING optional                        "Ordem de Serviço
      !I_ROMANEIO type STRING optional                         "Romaneio
      !I_ENTRADA_INICIO type STRING optional                   "Data Entrada Início Date (dd/mm/aaaa)
      !I_ENTRADA_FINAL type STRING optional                    "Data Entrada Final Date (dd/mm/aaaa)
      !I_ANALISE_INICIO type STRING optional                   "Data Analise Início Date (dd/mm/aaaa)
      !I_ANALISE_FINAL type STRING optional                    "Data Analise Final Date (dd/mm/aaaa)
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_REC_KUHLMANN) type ref to ZIF_INTEGRACAO_REC_KUHLMANN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_REC_KUHLMANN) type ref to ZIF_INTEGRACAO_REC_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_REC_KUHLMANN) type ref to ZIF_INTEGRACAO_REC_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_REC_KUHLMANN) type ref to ZIF_INTEGRACAO_REC_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_REC_KUHLMANN) type ref to ZIF_INTEGRACAO_REC_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_REC_KUHLMANN) type ref to ZIF_INTEGRACAO_REC_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_REC_KUHLMANN) type ref to ZIF_INTEGRACAO_REC_KUHLMANN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      !I_SERVICO type ZTIPOWEBSERV .
  methods SET_TIPO
    importing
      !I_TIPO type ZTIPOWEBADM .
endinterface.
