interface ZIF_INTEGRACAO_RODOV
  public .


  class-data AT_TOKEN type STRING .
  class-data AT_IF_INTEGRACAO_RODOV type ref to ZIF_INTEGRACAO_RODOV .
  class-data AT_SERVICO type STRING .
  data AT_JSON type STRING .
  class-data AT_RETURN type BAPIRET2 .
  class-data AT_DT_CHEGADA type ZDE_DT_CHEGADA .
  class-data AT_SIG_TERMINAL type ZDE_SIGLA_TERMINAL .
  class-data AT_CNPJ_TERMINAL type ZDE_CNPJ_TERMINAL .
  class-data AT_DADOS_DESCARGA type ZLEST0174_TMP_T .
  class-data AT_SRV_INTEGRACAO type ZDE_SRV_INTEGRACAO .
  class-data AT_SIGLA_TERMINAL type ZDE_SIGLA_TERMINAL .
  class-data AT_OK type CHAR01 .
  class-data AT_DESC_TERMINAL type CHAR40 .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_RODV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_RODV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_DS_DATA
    importing
      !I_JSON type STRING
    exporting
      !E_DADOS type ZDE_DADOS_MODAL_T
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_INT_DADOS
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_DADOS type ZDE_DADOS_MODAL_T
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods GET_JSON
    exporting
      !E_JSON type STRING
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_ID_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods GET_ID_REFERENCIA
    exporting
      !E_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods PUT_INT_ROD .
  methods MONTA_JSON
    exporting
      !I_DADOS type ZDE_DADOS_MODAL_T
    returning
      value(E_JSON) type STRING .
  methods GET_INT_ROD
    importing
      !I_SERVICO type CHAR02 optional
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_DATA type DATA
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_DS_URL_POST
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_TOKEN
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_DATA_CHEGADA
    importing
      !I_DATA_CHEGADA type ZDE_DT_CHEGADA
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_SIG_TERMINAL
    importing
      !I_SIG_TERMINAL type ZDE_SIGLA_TERMINAL
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_CNPJ_TERMINAL
    importing
      !I_CNPJ_TERMINAL type ZDE_CNPJ_TERMINAL
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SALVAR_DADOS
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods TRATA_RETORNO
    importing
      !I_DADOS type ZDE_DADOS_MODAL_T
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods PREPARAR_DADOS_GRAVACAO
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_SRV_INTEGRACAO
    importing
      !I_SRV_INTEGRACAO type ZDE_SRV_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_SIGLA_TERMINAL
    importing
      !I_SIGLA_TERMINAL type ZDE_SIGLA_TERMINAL
    returning
      value(R_IF_INTEGRACAO_RODOV) type ref to ZIF_INTEGRACAO_RODOV .
  methods SET_DESC_TERMINAL
    importing
      !I_TEXT type CHAR40 .
endinterface.
