interface ZIF_INTEGRACAO_COF_KUHLMANN
  public .


  data AT_OS_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_COF_KUHLMANN type ref to ZIF_INTEGRACAO_COF_KUHLMANN .
  constants AT_FC_END_POINT type STRING value '/api/hviok' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_COF_KUHLMANN) type ref to ZIF_INTEGRACAO_COF_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_HVI_CONFIRMAR
    importing
      !I_OS_ID type STRING
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_COF_KUHLMANN) type ref to ZIF_INTEGRACAO_COF_KUHLMANN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_COF_KUHLMANN) type ref to ZIF_INTEGRACAO_COF_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_COF_KUHLMANN) type ref to ZIF_INTEGRACAO_COF_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_COF_KUHLMANN) type ref to ZIF_INTEGRACAO_COF_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_COF_KUHLMANN) type ref to ZIF_INTEGRACAO_COF_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_COF_KUHLMANN) type ref to ZIF_INTEGRACAO_COF_KUHLMANN
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_COF_KUHLMANN) type ref to ZIF_INTEGRACAO_COF_KUHLMANN
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
