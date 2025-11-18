interface ZIF_INTEGRACAO_CANCEL_REJEITAR
  public .


  class-data AT_IF_INTEGRACAO_CANCEL_REJ type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR .
  data AT_VIAGEM type ZLEST0185 .
  data AT_VIAGEM_TX type ZLEST0185TX .
  constants AT_FC_PROCESSAR_CANCEL_REJEITA type ZDE_DS_FUNCAO_PROCESSA value '/cancelamento/rejeitar' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_CANCEL_REJEITA) type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_NEW_CANCEL_REJEITAR
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_CANCEL_REJEITA) type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_CANCEL_REJEITA) type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_CANCEL_REJEITA) type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_CANCEL_REJEITA) type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_CANCEL_REJEITA) type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_CANCEL_REJEITA) type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_CANCEL_REJEITA) type ref to ZIF_INTEGRACAO_CANCEL_REJEITAR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
