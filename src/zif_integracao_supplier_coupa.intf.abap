interface ZIF_INTEGRACAO_SUPPLIER_COUPA
  public .


  class-data AT_IF_INTEGRACAO_SUP_COUPA type ref to ZIF_INTEGRACAO_SUPPLIER_COUPA .
  class-data AT_SERVICO type ZTIPOWEBSERV .
  class-data AT_XML type STRING .
  class-data AT_ID_REFERENCIA type STRING .
  class-data AT_TP_REFERENCIA type STRING .
  data AT_CNPJ type LFA1-STCD1 .
  data AT_CPF type LFA1-STCD2 .

  class-methods GET_INSTANCE
    importing
      !IV_EXECUTION_MODE type ZCOUPA_SUPPLIER_EXEC_MODE
    returning
      value(R_IF_INTEGRACAO_SUPPLIER_COUPA) type ref to ZIF_INTEGRACAO_SUPPLIER_COUPA
    raising
      ZCX_INTEGRACAO .
  class-methods GET_ID_FORNECEDOR_BY_CNPJ
    importing
      !IV_CNPJ type CHAR18
    exporting
      value(E_COMMODITY_NAME) type SRT_XML_DATA-TAG_VALUE
      value(E_ID_ADRESS_PRIMARY) type SRT_XML_DATA-TAG_VALUE
      value(E_ID_ADRESS_SUPLIER) type SRT_XML_DATA-TAG_VALUE
      value(E_ID_CONTACT) type SRT_XML_DATA-TAG_VALUE
    returning
      value(RV_ID_FORNECEDOR) type SRT_XML_DATA-TAG_VALUE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  class-methods GET_ID_FORNECEDOR_BY_CPF
    importing
      !IV_CPF type LFA1-STCD2
    exporting
      !E_COMMODITY_NAME type SRT_XML_DATA-TAG_VALUE
      !E_ID_ADRESS_PRIMARY type SRT_XML_DATA-TAG_VALUE
      value(E_ID_ADRESS_SUPLIER) type SRT_XML_DATA-TAG_VALUE
      value(E_ID_CONTACT) type SRT_XML_DATA-TAG_VALUE
    returning
      value(RV_ID_FORNECEDOR) type SRT_XML_DATA-TAG_VALUE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    returning
      value(R_IF_INTEGRACAO_SUPPLIER_COUPA) type ref to ZIF_INTEGRACAO_SUPPLIER_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_XML type STRING
    returning
      value(R_IF_INTEGRACAO_SUPPLIER_COUPA) type ref to ZIF_INTEGRACAO_SUPPLIER_COUPA
    raising
      ZCX_INTEGRACAO .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO_SUPPLIER_COUPA) type ref to ZIF_INTEGRACAO_SUPPLIER_COUPA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SERVICO
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional .
  methods SET_INIT_IMPORT
    importing
      !IS_COUPA_SUPPLIER_DATA_CREATE type ZCOUPA_SUPPLIER_CREATE optional
      !IS_COUPA_SUPPLIER_DATA_MODIFY type ZCOUPA_SUPPLIER_MODIFY optional
      !IS_MODIFY type XFLAG optional
      !IS_CREATE type XFLAG optional
      !IV_CNPJ type CHAR18 optional
      !IV_CPF type LFA1-STCD2 optional
    exporting
      !E_RETORNO_INTEGRACAO type ZINTEGRACAO_LOG
      !E_ID_FORNECEDOR type SRT_XML_DATA-TAG_VALUE
      !E_ID_ADRESS_PRIMARY type SRT_XML_DATA-TAG_VALUE
      !E_ID_ADRESS_SUPLIER type SRT_XML_DATA-TAG_VALUE
      !E_ID_CONTACT type SRT_XML_DATA-TAG_VALUE
      !E_COMMODITY_NAME type SRT_XML_DATA-TAG_VALUE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods CONVERT_ABAP_TO_XML
    importing
      !IS_COUPA_SUPPLIER_DATA_CREATE type ZCOUPA_SUPPLIER_CREATE optional
      !IS_COUPA_SUPPLIER_DATA_MODIFY type ZCOUPA_SUPPLIER_MODIFY optional
      !IS_CREATE type XFLAG optional
      !IS_MODIFY type XFLAG optional
    returning
      value(RV_XML) type STRING .
endinterface.
