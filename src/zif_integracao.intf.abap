interface ZIF_INTEGRACAO
  public .


  class-data AT_IF_INTEGRACAO type ref to ZIF_INTEGRACAO .
  data AT_MSG_INJECT type ref to ZIF_INTEGRACAO_INJECT .
  constants AT_ID_INTERFACE_LOTE_FRETE type ZDE_ID_INTERFACE value '001' ##NO_TEXT.
  constants AT_ID_INTERFACE_PRECO_FRETE type ZDE_ID_INTERFACE value '002' ##NO_TEXT.
  constants AT_ID_INTERFACE_ORD_CARREGAMEN type ZDE_ID_INTERFACE value '003' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_APROVAR type ZDE_ID_INTERFACE value '004' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_REJEITA type ZDE_ID_INTERFACE value '005' ##NO_TEXT.
  constants AT_ID_INTERFACE_SOL_OC_OPUS type ZDE_ID_INTERFACE value '006' ##NO_TEXT.
  constants AT_ID_INTERFACE_CAN_OC_OPUS type ZDE_ID_INTERFACE value '014' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_PRECO type ZDE_ID_INTERFACE value '007' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_CARRE type ZDE_ID_INTERFACE value '008' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_CACAN type ZDE_ID_INTERFACE value '009' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_STATUS type ZDE_ID_INTERFACE value '010' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIA_CANCELAR type ZDE_ID_INTERFACE value '011' ##NO_TEXT.
  constants AT_ID_INTERFACE_CANCEL_APROVAR type ZDE_ID_INTERFACE value '012' ##NO_TEXT.
  constants AT_ID_INTERFACE_CANCEL_REJEITA type ZDE_ID_INTERFACE value '013' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_DECARRE type ZDE_ID_INTERFACE value '025' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_APR_CAR type ZDE_ID_INTERFACE value '037' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_UPL_AUT type ZDE_ID_INTERFACE value '038' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_UPL_EXE type ZDE_ID_INTERFACE value '039' ##NO_TEXT.
  constants AT_ID_INTERFACE_VIAGEM_UPL_CON type ZDE_ID_INTERFACE value '041' ##NO_TEXT.
  data AT_MSG_INTEGRA type ZINTEGRACAO .
  data AT_MSG_MULTIPART type ZINTEGRACAO01_TAB .
  data AT_MULTIPART type ZDE_MULTIPART_FIELD_T .
  constants AT_TP_CANAL_COMUNICA_HTTP type ZDE_TP_CANAL_COMUNICACAO value '0' ##NO_TEXT.
  constants AT_TP_INTEGRACAO_INBOUND type ZDE_TP_INTEGRACAO value '1' ##NO_TEXT.
  constants AT_TP_INTEGRACAO_OUTBOUND type ZDE_TP_INTEGRACAO value '0' ##NO_TEXT.
  constants AT_QTD_PADRAO_TENTATIVAS type I value 5 ##NO_TEXT.
  constants AT_TP_SINCRONIA_ASSINCRONA type ZDE_TP_SINCRONIA value '0' ##NO_TEXT.
  constants AT_TP_SINCRONIA_SINCRONA type ZDE_TP_SINCRONIA value '1' ##NO_TEXT.
  constants AT_ID_INTERFACE_GRC_NEW_DOC type ZDE_ID_INTERFACE value '015' ##NO_TEXT.
  constants AT_ID_INTERFACE_GRC_CAN_DOC type ZDE_ID_INTERFACE value '016' ##NO_TEXT.
  constants AT_ID_INTERFACE_GRC_EST_DOC type ZDE_ID_INTERFACE value '017' ##NO_TEXT.
  constants AT_ID_INTERFACE_TOKEN type ZDE_ID_INTERFACE value '018' ##NO_TEXT.
  constants AT_ID_INTERFACE_AUT_OPUS_SIM type ZDE_AUTENTICA_OPUS value ABAP_TRUE ##NO_TEXT.
  constants AT_ID_INTERFACE_AUT_OPUS_NAO type ZDE_AUTENTICA_OPUS value ABAP_FALSE ##NO_TEXT.
  constants AT_ID_INTERFACE_AUT_SEND_SIM type ZDE_SEND_ATENTICACAO value ABAP_TRUE ##NO_TEXT.
  constants AT_ID_INTERFACE_AUT_SEND_NAO type ZDE_SEND_ATENTICACAO value ABAP_FALSE ##NO_TEXT.
  constants AT_ID_INTERFACE_EMBALAGEM type ZDE_ID_INTERFACE value '019' ##NO_TEXT.
  constants AT_ID_INTERFACE_ENERGIA type ZDE_ID_INTERFACE value '020' ##NO_TEXT.
  constants AT_ID_INTERFACE_COTTON type ZDE_ID_INTERFACE value '021' ##NO_TEXT.
  constants AT_ID_INTERFACE_CTB_NEW_DOC type ZDE_ID_INTERFACE value '022' ##NO_TEXT.
  constants AT_ID_INTERFACE_CTB_EST_DOC type ZDE_ID_INTERFACE value '023' ##NO_TEXT.
  constants AT_ID_INTERFACE_CTB_CNS_DOC type ZDE_ID_INTERFACE value '024' ##NO_TEXT.
  constants AT_ID_INTERFACE_KUHLMANN_AUT type ZDE_ID_INTERFACE value '026' ##NO_TEXT.
  constants AT_ID_INTERFACE_KUHLMANN_ANA type ZDE_ID_INTERFACE value '027' ##NO_TEXT.
  constants AT_ID_INTERFACE_KUHLMANN_CLI type ZDE_ID_INTERFACE value '028' ##NO_TEXT.
  constants AT_ID_INTERFACE_KUHLMANN_COF type ZDE_ID_INTERFACE value '029' ##NO_TEXT.
  constants AT_ID_INTERFACE_KUHLMANN_REC type ZDE_ID_INTERFACE value '030' ##NO_TEXT.
  constants AT_ID_INTERFACE_PEDI_CNS type ZDE_ID_INTERFACE value '031' ##NO_TEXT.
  constants AT_ID_INTERFACE_PROTHEUS type ZDE_ID_INTERFACE value '032' ##NO_TEXT.
  constants AT_TP_LOG_ENVIO type ZDE_TP_LOG_INTEGRACAO value '1' ##NO_TEXT.
  constants AT_TP_LOG_PROCESSAMENTO type ZDE_TP_LOG_INTEGRACAO value '2' ##NO_TEXT.
  constants AT_TP_LOG_INTEGRACAO type ZDE_TP_LOG_INTEGRACAO value '3' ##NO_TEXT.
  constants AT_ID_INTERFACE_ROND_ESTOQUE type ZDE_ID_INTERFACE value '035' ##NO_TEXT.
  constants AT_ID_INTERFACE_TCOT_CONTRATOS type ZDE_ID_INTERFACE value '040' ##NO_TEXT.
  constants AT_ID_INTERFACE_IMPROVEFY_JANO type ZDE_ID_INTERFACE value '043' ##NO_TEXT.
  constants AT_ID_INTERFACE_OBS_ZLES0050 type ZDE_ID_INTERFACE value '044' ##NO_TEXT.
  constants AT_ID_INTERFACE_DOCK_BANK type ZDE_ID_INTERFACE value '045' ##NO_TEXT.
  constants AT_ID_INTERFACE_COMB type ZDE_ID_INTERFACE value '046' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_REQ_COMP type ZDE_ID_INTERFACE value '047' ##NO_TEXT.
  constants AT_ID_INTERFACE_USER_COUPA type ZDE_ID_INTERFACE value '049' ##NO_TEXT.
  constants AT_ID_INTERFACE_LOOKUP_COUPA type ZDE_ID_INTERFACE value '048' ##NO_TEXT.
  constants AT_ID_INTERFACE_AUT_API_AD_SIM type ZDE_AUTENTICA_API_AD value ABAP_TRUE ##NO_TEXT.
  constants AT_ID_INTERFACE_AUT_API_AD_NAO type ZDE_AUTENTICA_API_AD value ABAP_FALSE ##NO_TEXT.
  data AT_INTERFACE_MODULE type ZDE_AUTENTICA_MODULE .
  constants AT_ID_INTERFACE_TAXA_CAMBIO type ZDE_ID_INTERFACE value '050' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_PED_COMP type ZDE_ID_INTERFACE value '051' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_GRP_MERC type ZDE_ID_INTERFACE value '052' ##NO_TEXT.
  constants AT_ID_INTERFACE_RECIBOS type ZDE_ID_INTERFACE value '053' ##NO_TEXT.
  constants AT_ID_INTERFACE_FORNECEDORES type ZDE_ID_INTERFACE value '054' ##NO_TEXT.
  constants AT_ID_INTERFACE_GET_ID_CNPJ type ZDE_ID_INTERFACE value '055' ##NO_TEXT.
  constants AT_ID_INTERFACE_GET_ID_CPF type ZDE_ID_INTERFACE value '056' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_ADIGITAL type ZDE_ID_INTERFACE value '057' ##NO_TEXT.
  constants AT_ID_INTERFACE_BRY_ADIGITAL type ZDE_ID_INTERFACE value '058' ##NO_TEXT.
  constants AT_ID_INTERFACE_LOOKUP_GET_ID type ZDE_ID_INTERFACE value '059' ##NO_TEXT.
  constants AT_ID_INTERFACE_TOKEN_COUPA type ZDE_ID_INTERFACE value '060' ##NO_TEXT.
  constants AT_ID_INTERFACE_COMPENSA_SCP type ZDE_ID_INTERFACE value '061' ##NO_TEXT.
  constants AT_ID_INTERFACE_BRY_AD_POST type ZDE_ID_INTERFACE value '062' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_MATERIAL type ZDE_ID_INTERFACE value '063' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_ORC type ZDE_ID_INTERFACE value '064' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_REQUISIT type ZDE_ID_INTERFACE value '065' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_ORDER type ZDE_ID_INTERFACE value '066' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_FTP type ZDE_ID_INTERFACE value '067' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_RECIBO type ZDE_ID_INTERFACE value '068' ##NO_TEXT.
  constants AT_ID_INTERFACE_APP_PRODUTOR type ZDE_ID_INTERFACE value '069' ##NO_TEXT.
  constants AT_ID_INTERFACE_COUPA_GET_PED type ZDE_ID_INTERFACE value '070' ##NO_TEXT.
  constants AT_ID_INTERF_ESTOR_DOC_CONTAB type ZDE_ID_INTERFACE value '071' ##NO_TEXT.
  constants AT_ID_INTERFACE_REAL_FRE_SIGAM type ZDE_ID_INTERFACE value '072' ##NO_TEXT.
  constants AT_ID_INTERFACE_O_CAR_OPUS type ZDE_ID_INTERFACE value '073' ##NO_TEXT.
  constants AT_ID_INTERFACE_DESENV_TOKEN type ZDE_ID_INTERFACE value '074' ##NO_TEXT.
  constants AT_ID_INTERFACE_DESENV_USER type ZDE_ID_INTERFACE value '075' ##NO_TEXT.
  constants AT_ID_INTERFACE_DESENV_ORG type ZDE_ID_INTERFACE value '076' ##NO_TEXT.
  constants AT_ID_INTERFACE_PERMISSAO_USER type ZDE_ID_INTERFACE value '077' ##NO_TEXT.
  constants AT_ID_INTERFACE_ACTS_FARDINHO type ZDE_ID_INTERFACE value '078' ##NO_TEXT.
  constants AT_ID_INTERFACE_AGRIQ type ZDE_ID_INTERFACE value '079' ##NO_TEXT.
  constants AT_ID_INTERFACE_SAP_SE_INFO_NF type ZDE_ID_INTERFACE value '080' ##NO_TEXT.
  constants AT_ID_INTERFACE_STATUS_MOBILE type ZDE_ID_INTERFACE value '081' ##NO_TEXT.
  constants AT_ID_INTERFACE_IPORTSOLUTION type ZDE_ID_INTERFACE value '082' ##NO_TEXT.
  constants AT_ID_CRIA_MODIFICA_NOTA type ZDE_ID_INTERFACE value '083' ##NO_TEXT.
  data AT_FORM_FIELDS type ZDE_HEADER_FIELD_T .
  constants AT_ID_INTERFACE_WEBSEMPRE type ZDE_ID_INTERFACE value '084' ##NO_TEXT.
  constants AT_ID_INTERFACE_TROCANTROBO type ZDE_ID_INTERFACE value '085' ##NO_TEXT.
  constants AT_ID_CRIA_MODIFICA_ORDEM type ZDE_ID_INTERFACE value '086' ##NO_TEXT.
  constants AT_ID_INTERFACE_DATASHARE type ZDE_ID_INTERFACE value '087' ##NO_TEXT.
  constants AT_ID_INTERFACE_PLAN_PRODUCE type ZDE_ID_INTERFACE value '088' ##NO_TEXT.
  constants AT_ID_INTERFACE_SIS type ZDE_ID_INTERFACE value '089' ##NO_TEXT.
  constants AT_ID_INTERFACE_TESTE_API type ZDE_ID_INTERFACE value '000' ##NO_TEXT.
  constants AT_ID_INTERFACE_AMAGGI_PLAY type ZDE_ID_INTERFACE value '090' ##NO_TEXT.
  constants AT_ID_INTERF_CANCEL_DOC_FATURA type ZDE_ID_INTERFACE value '091' ##NO_TEXT.
  constants AT_ID_INTERF_THUNDERS_OPERAT type ZDE_ID_INTERFACE value '092' ##NO_TEXT.
  constants AT_ID_INTERF_THUNDERS_CONCIL type ZDE_ID_INTERFACE value '093' ##NO_TEXT.
  constants AT_ID_INTERF_THUNDERS_TOKEN type ZDE_ID_INTERFACE value '094' ##NO_TEXT.
  constants AT_ID_ESTRAT_APROV type ZDE_ID_INTERFACE value '095' ##NO_TEXT.
  constants AT_ID_INTERF_SIGAM_HEDGE_CONSU type ZDE_ID_INTERFACE value '096' ##NO_TEXT.
  constants AT_ID_INTERF_SIGAM_HEDGE_REVER type ZDE_ID_INTERFACE value '097' ##NO_TEXT.
  constants AT_ID_INTERFACE_INSUMOS type ZDE_ID_INTERFACE value '109' ##NO_TEXT.
  constants AT_ID_INTERF_EQUIP_MOBMAN type ZDE_ID_INTERFACE value '128' ##NO_TEXT.
  constants AT_ID_INTERF_LOCAL_MOBMAN type ZDE_ID_INTERFACE value '133' ##NO_TEXT.
  constants AT_ID_INTERF_CENTRO_MOBMAN type ZDE_ID_INTERFACE value '134' ##NO_TEXT.
  constants AT_ID_INTERFACE_CLIENTE type ZDE_ID_INTERFACE value '156' ##NO_TEXT.
  constants AT_ID_INTERFACE_CLIENTE_TOKEN type ZDE_ID_INTERFACE value '169' ##NO_TEXT.
  constants AT_ID_TRACE_COTTON type ZDE_ID_INTERFACE value '168' ##NO_TEXT.
  constants AT_ID_CONS_REQ_COUPA type ZDE_ID_INTERFACE value '178' ##NO_TEXT.
  constants AT_ID_INTERF_RSDATA type ZDE_ID_INTERFACE value '214' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
  class-methods GET_QINFO_ALV
    returning
      value(R_QINFO) type LVC_T_QINF .
  class-methods GET_LINE_COLOR
    importing
      !I_TP_SINCRONIA type ZDE_TP_SINCRONIA
      !I_STATUS type CHAR04
      !I_SELECTED type CHAR01
      !I_ANONIMO type CHAR01
    returning
      value(R_LINE_COLOR) type CHAR04 .
  class-methods GET_LINE_COLOR_LOG
    importing
      !I_LINHA_LOG type ZINTEGRACAO_LOG
      !I_SELECTED type CHAR01
    returning
      value(R_LINE_COLOR) type CHAR04 .
  class-methods GET_NOT_STORE_LOG
    importing
      !I_ID_INTERFACE type ZDE_ID_INTERFACE
    returning
      value(R_NOT_STORE_LOG) type ZDE_NOT_STORE_LOG .
  methods GET_MSG_ALV
    importing
      !I_INTEGRACAO type ZINTEGRACAO optional
    exporting
      !E_INTEGRACAO_ALV type ZINTEGRACAO_ALV
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
  methods GET_MSG_LOG_ALV
    importing
      !I_INTEGRACAO_LOG type ZINTEGRACAO_LOG
    exporting
      !E_INTEGRACAO_LOG_ALV type ZINTEGRACAO_LOG_ALV
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
  methods SET_MSG_INJECT
    importing
      !I_MSG type ref to ZIF_INTEGRACAO_INJECT
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
  methods SET_NEW_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_ERROR .
  methods SET_REGISTRO
    importing
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_NEW_ID_INTEGRACAO
    exporting
      value(E_ID_INTEGRACAO) type ZDE_ID_INTEGRACAO
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_ERROR .
  methods SET_INBOUND_MSG
    exporting
      !E_MSG type STRING
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_OUTBOUND_MSG
    importing
      !I_FORCE type CHAR01 default ABAP_FALSE
    exporting
      !E_INTEGRACAO type ZINTEGRACAO
      !E_MENSAGEM type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SEND_MSG_HTTP
    importing
      !I_URL type STRING
      !I_URL_TOKEN type STRING
      !I_JSON type STRING optional
      !I_DATA_XSTRING type XSTRING optional
      !I_FORM_DATA type STRING optional
      !I_METODO type STRING default 'POST'
      !I_SERVER_PROTOCOLO type STRING default 'HTTP/1.1'
      !I_CONTENT_TYPE type STRING default 'application/json'
      !I_AUTENTICA_OPUS type CHAR01 default ABAP_FALSE
      !I_AUTENTICA_API_AD type CHAR01 default ABAP_FALSE
      !I_HEADER_FIELDS type ZDE_HEADER_FIELD_T optional
      !I_MULTIPART_FIELDS type ZDE_MULTIPART_FIELD_T optional
      !I_NOT_CONTENT_LENGTH type ZDE_NOT_CONTENT_LENGTH optional
      !I_AUTENTICA_MODULE type ZDE_AUTENTICA_MODULE optional
      !I_FORM_FIELDS type ZDE_HEADER_FIELD_T optional
      !I_PROPERTYTYPE_LOGON_POPUP type I optional
    exporting
      !E_CODE type ZDE_CODE_RETORNO
      !E_REASON type STRING
      !E_CDATA type STRING
      !E_DATA type XSTRING
      !E_HEADER type STRING
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_ERROR .
  methods SET_INTEGRAR_RETORNO
    exporting
      !E_DATA_RETORNO type STRING
      !E_ZINTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_PROCESSAR_RETORNO
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_INSTANCIA_OBJETO_INJECT
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_REGISTRO
    exporting
      !E_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
  methods GET_REGISTRO_LOG
    importing
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !I_DT_REGISTRO type ZDE_DT_REGISTRO
      !I_HR_REGISTRO type ZDE_HR_REGISTRO
    exporting
      !E_INTEGRACAO_LOG type ZINTEGRACAO_LOG
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
  methods CK_PERMISSAO_OUTBOUND
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
  methods SET_CANCELAR_ENVIO
    importing
      !I_REFERENCIA type ZDE_CHAVE_REFERENCIA
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO .
  methods SET_ENQUEUE
    importing
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !I_BLOQUEAR type CHAR01 default ' '
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
  methods FREE
    returning
      value(R_IF_INTEGRACAO) type ref to ZIF_INTEGRACAO
    raising
      ZCX_INTEGRACAO .
endinterface.
