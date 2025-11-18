interface ZIF_INTEGRACAO_COTTON_SAP
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_COTTON_SAP type ref to ZIF_INTEGRACAO_COTTON_SAP .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_ZPPT0002 type ZPPT016 .
  data AT_J_1BNFE_ACTIVE type J_1BNFE_ACTIVE .
  data AT_ZSDT0231 type ZSDT0231 .
  data AT_ZSDT0232 type ZDE_ZSDT0232_T .
  data AT_ZSDT0233 type ZDE_ZSDT0233_T .
  data AT_RETORNO type ZPPS0004 .
  data AT_RECEBIDO type CHAR11 .
  data AT_STATUS type ZPPE0002 .
  data AT_PROC_ZPPT0002 type ZPPT0002 .
  data AT_PROC_ZPPT0002_T type ZPPT0002_T .
  constants C_ZMMR0030 type BTCPROG value 'ZMMR0030_JOB_V2' ##NO_TEXT.
  constants C_ZMMR0031 type BTCPROG value 'ZMMR0031_COR' ##NO_TEXT.
  constants C_ZMMR0040 type BTCPROG value 'ZMMR0040_JOB_V2' ##NO_TEXT.
  constants C_ZMMR0041 type BTCPROG value 'ZMMR0041_JOB' ##NO_TEXT.
  data AT_DADOS_INBOUND type ZPPS0001 .
  data AT_PROTOCOLO_RECEBIMENTO type ZDE_ID_REFERENCIA .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_COTTON_SAP) type ref to ZIF_INTEGRACAO_COTTON_SAP
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
      !ID_REFERENCIA type STRING optional
    returning
      value(R_IF_INTEGRACAO_COTTON_SAP) type ref to ZIF_INTEGRACAO_COTTON_SAP
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_COTTON_SAP) type ref to ZIF_INTEGRACAO_COTTON_SAP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_PROCESSA_DADOS
    returning
      value(R_IF_INTEGRACAO_COTTON_SAP) type ref to ZIF_INTEGRACAO_COTTON_SAP
    raising
      ZCX_INTEGRACAO
      ZCX_PARCEIROS
      ZCX_NF_WRITER
      ZCX_ERROR .
  methods SET_ESTORNA_DADOS
    returning
      value(R_IF_INTEGRACAO_COTTON_SAP) type ref to ZIF_INTEGRACAO_COTTON_SAP
    raising
      ZCX_INTEGRACAO
      ZCX_PARCEIROS
      ZCX_NF_WRITER
      ZCX_ERROR .
  methods GET_DOUBLE_CHECK
    returning
      value(R_IF_INTEGRACAO_COTTON_SAP) type ref to ZIF_INTEGRACAO_COTTON_SAP .
  methods GET_CHECK_JOB
    exporting
      value(E_MSG) type STRING
    returning
      value(R_IF_INTEGRACAO_COTTON_SAP) type ref to ZIF_INTEGRACAO_COTTON_SAP .
  methods CONFIGURE_SERVER
    importing
      !I_HTTP_SERVER type ref to IF_HTTP_SERVER .
endinterface.
