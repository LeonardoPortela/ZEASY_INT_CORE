interface ZIF_INTEGRACAO_COUPA_FTP
  public .


  data AT_SERVICO type /UI2/SERVICE_NAME .
  class-data AT_INSTANCE type ref to ZIF_INTEGRACAO_COUPA_FTP .
  data AT_XDADOS type XSTRING .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .

  class-methods GET_INSTANCE
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_COUPA_FTP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  class-methods ENVIAR_COUPA
    importing
      !I_XFILE type XSTRING
      !I_FILE_NAME type CHAR50
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_COUPA_FTP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_COUPA_FTP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_DATA
    importing
      !I_XFILE type XSTRING
      !I_FILE_NAME type CHAR50
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_COUPA_FTP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_OBJECT) type ref to ZIF_INTEGRACAO_COUPA_FTP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
