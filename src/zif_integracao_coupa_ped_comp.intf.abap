interface ZIF_INTEGRACAO_COUPA_PED_COMP
  public .


  types:
    BEGIN OF ty_filter,
          field type string,
          value type string,
        end of ty_filter .
  types:
    tt_filter TYPE TABLE OF ty_filter .

  data AT_CLIENTE_ID type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_COUPA type ref to ZIF_INTEGRACAO_COUPA_PED_COMP .
  constants AT_FC_END_POINT type STRING value '/application/xml/' ##NO_TEXT.
  class-data AT_SERVICO type /UI2/SERVICE_NAME .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_PED_COMP_COUPA) type ref to ZIF_INTEGRACAO_COUPA_PED_COMP
    raising
      ZCX_INTEGRACAO .
  methods SET_USER_BUSCAR
    importing
      !I_CLIENTE_ID type STRING optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_PED_COMP_COUPA) type ref to ZIF_INTEGRACAO_COUPA_PED_COMP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      value(E_ID_PO) type STRING optional
      value(E_INTEGRACAO) type STRING optional
      value(E_SERVICO) type /UI2/SERVICE_NAME optional
      value(E_HTTP_URL) type STRING optional
      value(E_TOKEN) type STRING optional
      value(E_METODO) type STRING optional
      value(IT_FILTER) type TT_FILTER optional
    returning
      value(R_IF_INTEGRACAO_PED_COMP_COUPA) type ref to ZIF_INTEGRACAO_COUPA_PED_COMP
    raising
      ZCX_INTEGRACAO .
  methods GET_XML
    exporting
      !E_XML type STRING
    returning
      value(R_IF_INTEGRACAO_PED_COMP_COUPA) type ref to ZIF_INTEGRACAO_COUPA_PED_COMP
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_XML type STRING
    returning
      value(R_IF_INTEGRACAO_PED_COMP_COUPA) type ref to ZIF_INTEGRACAO_COUPA_PED_COMP
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_PED_COMP_COUPA) type ref to ZIF_INTEGRACAO_COUPA_PED_COMP
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_PED_COMP_COUPA) type ref to ZIF_INTEGRACAO_COUPA_PED_COMP
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_PED_COMP_COUPA) type ref to ZIF_INTEGRACAO_COUPA_PED_COMP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional .
endinterface.
