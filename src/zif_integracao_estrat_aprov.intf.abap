interface ZIF_INTEGRACAO_ESTRAT_APROV
  public .


  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_ESTRAT_APROV type ref to ZIF_INTEGRACAO_ESTRAT_APROV .
  constants AT_FC_END_POINT type STRING value '/api/clientes' ##NO_TEXT.
  class-data AT_TIPO type STRING .
  class-data AT_SERVICO type STRING .
  class-data AT_JSON type STRING .
  class-data AT_CONSULTA type ZDE_CNS_ESTRAT_APROV .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_ESTRAT_APROV) type ref to ZIF_INTEGRACAO_ESTRAT_APROV
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_ESTRAT_APROV) type ref to ZIF_INTEGRACAO_ESTRAT_APROV
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
      !E_MSG type STRING
    returning
      value(R_IF_INTEGRACAO_ESTRAT_APROV) type ref to ZIF_INTEGRACAO_ESTRAT_APROV
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_ESTRAT_APROV
    returning
      value(R_IF_INTEGRACAO_ESTRAT_APROV) type ref to ZIF_INTEGRACAO_ESTRAT_APROV .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type ZTIPOWEBSERV .
endinterface.
