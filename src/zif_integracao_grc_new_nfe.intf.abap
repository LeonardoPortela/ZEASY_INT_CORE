interface ZIF_INTEGRACAO_GRC_NEW_NFE
  public .


  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_IF_INTEGRACAO_GRC_NEW_NFE type ref to ZIF_INTEGRACAO_GRC_NEW_NFE .
  data AT_ID_INTEGRACAO type ZDE_ID_INTEGRACAO .
  data AT_J_1BNFDOC type J_1BNFDOC .
  data AT_J_1BNFE_ACTIVE type J_1BNFE_ACTIVE .
  data AT_ZSDT0231 type ZSDT0231 .
  data AT_ZSDT0232 type ZDE_ZSDT0232_T .
  data AT_ZSDT0233 type ZDE_ZSDT0233_T .
  data AT_LIMITE_FISCAL type NETWR .

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO_GRC_NEW_NFE) type ref to ZIF_INTEGRACAO_GRC_NEW_NFE
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INFO type ZDE_INTEGRACAO_HTTP_CONFIG
    returning
      value(R_IF_INTEGRACAO_GRC_NEW_NFE) type ref to ZIF_INTEGRACAO_GRC_NEW_NFE
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_MSG type STRING
      !E_PROTOCOLO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_GRC_NEW_NFE) type ref to ZIF_INTEGRACAO_GRC_NEW_NFE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_VALIDAR_DADOS
    importing
      !I_NFE type ZDE_PROCESSO
    exporting
      !E_ZFIWRT0008 type ZFIWRT0008
      !E_ZFIWRT0009 type ZFIWRT0009_T
    returning
      value(R_IF_INTEGRACAO_GRC_NEW_NFE) type ref to ZIF_INTEGRACAO_GRC_NEW_NFE
    raising
      ZCX_INTEGRACAO
      ZCX_PARCEIROS
      ZCX_NF_WRITER
      ZCX_ERROR .
  methods SET_NEW_DOC_DISCAL
    importing
      !I_DADOS type ZDE_PROCESSO
    exporting
      !E_DOCUMENTO type J_1BNFDOC
      !E_INFO_DOC_ELETRONICO type J_1BNFE_ACTIVE
    returning
      value(R_IF_INTEGRACAO_GRC_NEW_NFE) type ref to ZIF_INTEGRACAO_GRC_NEW_NFE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_LIMITE_FISCAL
    importing
      !I_MODEL type J_1BMODEL
    returning
      value(R_IF_INTEGRACAO_GRC_NEW_NFE) type ref to ZIF_INTEGRACAO_GRC_NEW_NFE .
  methods GET_LIMITE_FISCAL
    exporting
      value(E_LIMITE) type NETWR .
  methods SET_CRIAR_NF_PROPRIA
    importing
      !I_DADOS type ZDE_PROCESSO
    exporting
      !E_DOCUMENTO type J_1BDOCNUM
    returning
      value(R_IF_INTEGRACAO_GRC_NEW_NFE) type ref to ZIF_INTEGRACAO_GRC_NEW_NFE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
endinterface.
