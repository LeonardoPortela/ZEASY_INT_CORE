interface ZIF_INTEGRACAO_AMAGGI_PLAY
  public .


  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
  class-data AT_IF_INTEGRACAO_AMAGGI_PLAY type ref to ZIF_INTEGRACAO_AMAGGI_PLAY .
  constants AT_FC_END_POINT type STRING value '/api/clientes' ##NO_TEXT.
  class-data AT_TIPO type STRING .
  class-data AT_SERVICO type STRING .
  class-data AT_JSON type STRING .

  class-methods GET_INSTANCE
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY
    raising
      ZCX_INTEGRACAO .
  methods SET_ENVIAR_USUARIO
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY
    raising
      ZCX_INTEGRACAO .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY
    raising
      ZCX_INTEGRACAO .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY
    raising
      ZCX_INTEGRACAO .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_TIPO
    importing
      value(I_TIPO) type ZTIPOWEBADM .
  methods SET_SERVICO
    importing
      !I_SERVICO type ZTIPOWEBSERV .
  methods SET_JSON
    importing
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_AMAGGI_PLAY) type ref to ZIF_INTEGRACAO_AMAGGI_PLAY .
endinterface.
