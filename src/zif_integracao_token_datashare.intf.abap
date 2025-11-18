interface ZIF_INTEGRACAO_TOKEN_DATASHARE
  public .


  class-data AT_IF_INTEGRACAO_TOKEN type ref to ZIF_INTEGRACAO_TOKEN_DATASHARE .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN_DATASHARE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_TOKEN
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN_DATASHARE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN_DATASHARE .
  methods SET_SEND_MSG
    exporting
      !E_ACCESS_TOKEN type STRING
      !E_TOKEN_TYPE type STRING
      !E_EXPIRES_IN type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN_DATASHARE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
