interface ZIF_INTEGRACAO_TOKEN_ORDEM
  public .


  class-data AT_IF_INTEGRACAO_TOKEN type ref to ZIF_INTEGRACAO_TOKEN_ORDEM .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN_ORDEM
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_TOKEN
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN_ORDEM
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN_ORDEM .
  methods SET_SEND_MSG
    exporting
      !E_ACCESS_TOKEN type STRING
      !E_TOKEN_TYPE type STRING
      !E_EXPIRES_IN type STRING
    returning
      value(R_IF_INTEGRACAO_TOKEN) type ref to ZIF_INTEGRACAO_TOKEN_ORDEM
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
