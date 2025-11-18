interface ZIF_INTEGRACAO_NEW_ROMANEIO
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_NEW_ROMANEIO type ref to ZIF_INTEGRACAO_NEW_ROMANEIO .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_RECEBIDO type CHAR11 .
  data AT_ZSDT001 type ZSDS001 .
  data AT_RETORNO type ZSDT001 .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_NEW_ROMANEIO) type ref to ZIF_INTEGRACAO_NEW_ROMANEIO
    raising
      ZCX_INTEGRACAO .
  methods GET_VALIDAR_DADOS
    importing
      !I_NFE type ZDE_PROCESSO
    exporting
      !E_ZFIWRT0008 type ZFIWRT0008
      !E_ZFIWRT0009 type ZFIWRT0009_T
    returning
      value(R_IF_INTEGRACAO_NEW_ROMANEIO) type ref to ZIF_INTEGRACAO_NEW_ROMANEIO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
      !ID_REFERENCIA type STRING optional
    returning
      value(R_IF_INTEGRACAO_NEW_ROMANEIO) type ref to ZIF_INTEGRACAO_NEW_ROMANEIO .
  methods SET_NEW_ROMANEIO
    returning
      value(R_IF_INTEGRACAO_NEW_ROMANEIO) type ref to ZIF_INTEGRACAO_NEW_ROMANEIO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_NEW_ROMANEIO) type ref to ZIF_INTEGRACAO_NEW_ROMANEIO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
