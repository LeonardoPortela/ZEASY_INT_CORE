class ZCL_INTEGRACAO_MOBILE_ENERGIA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_MOBILE_ENERGIA .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_MOBILE_ENERGIA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface  = zif_integracao=>at_id_interface_via_cancelar.
    me->zif_integracao_inject~at_tp_integracao = zif_integracao=>at_tp_integracao_inbound.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'MAGGI_MOBILE_ENERGIA'.
    me->zif_integracao_inject~at_tp_canal = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_FALSE.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    E_SUCESSO = ABAP_TRUE.

    R_IF_INTEGRACAO_INJECT = ME.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    "Metodo para Integrar InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    E_SUCESSO = ABAP_TRUE.

    R_IF_INTEGRACAO_INJECT = ME.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    "Metodo para processoamento de InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_MOBILE_ENERGIA~GET_INSTANCE.

    IF ZIF_INTEGRACAO_MOBILE_ENERGIA~AT_IF_INTE_MOBILE_ENERGIA IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_MOBILE_ENERGIA~AT_IF_INTE_MOBILE_ENERGIA TYPE ZCL_INTEGRACAO_MOBILE_ENERGIA.
    ENDIF.
    R_IF_INTEGRACAO_MOBILE_ENERGIA = ZIF_INTEGRACAO_MOBILE_ENERGIA~AT_IF_INTE_MOBILE_ENERGIA.

  ENDMETHOD.


  METHOD zif_integracao_mobile_energia~set_ds_data.

*    DATA: I_INBOUND TYPE ZPME0011_T.
    DATA: i_inbound TYPE ztpm_d_m_ordem_t.

    "Incluir Texto JSON para integração
    r_if_integracao_mobile_energia = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

    "Validar Json
    /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = i_inbound ).
    IF i_inbound IS NOT INITIAL.
      me->zif_integracao_inject~at_referencia-id_referencia = i_inbound[ 1 ]-aufnr.
    ENDIF.

    CHECK i_inbound IS INITIAL.

    RAISE EXCEPTION TYPE zcx_integracao
      EXPORTING
        textid = VALUE #( msgid = zcx_integracao=>zcx_erro_body_recebido-msgid
                          msgno = zcx_integracao=>zcx_erro_body_recebido-msgno )
        msgid  = zcx_integracao=>zcx_erro_body_recebido-msgid
        msgno  = zcx_integracao=>zcx_erro_body_recebido-msgno
        msgty  = 'E'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_MOBILE_ENERGIA~SET_SEND_MSG.

    DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.
    DATA: I_INBOUND TYPE ZPME0011_T.

    R_IF_INTEGRACAO_MOBILE_ENERGIA = ME.

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY CHANGING DATA = I_INBOUND ).

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA =
    COND #( WHEN I_INBOUND IS NOT INITIAL
                    THEN COND #( WHEN I_INBOUND[ 1 ]-CADAUFNR IS NOT INITIAL
                                        THEN 'Update_Ordem'
                                        ELSE 'New_Ordem' )
                    ELSE 'criaOrdem' ).

    "Verificar a Função de Cada requisição
*    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = 'criaOrdem/'.

    CREATE OBJECT LC_INTEGRACAO.
*
*    LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
*      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_MOBILE_ENERGIA~AT_ID_INTEGRACAO
*      )->FREE(
*      ).
*
*    CLEAR: LC_INTEGRACAO.


    LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
  )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_MOBILE_ENERGIA~AT_ID_INTEGRACAO
  )->SET_PROCESSAR_RETORNO(
  )->SET_INTEGRAR_RETORNO(
       IMPORTING
         E_DATA_RETORNO =  DATA(E_DATA_RETORNO)
         E_ZINTEGRACAO_LOG = E_ZINTEGRACAO_LOG
  )->GET_REGISTRO( IMPORTING E_INTEGRACAO = DATA(E_INTEGRACAO)
  )->FREE(
  ).

    E_MSG = E_DATA_RETORNO.
    E_PROTOCOLO = ZCL_STRING=>LPAD( I_STR  = CONV #( E_INTEGRACAO-ID_INTEGRACAO ) I_QTD  = 15 I_CHAR = '0'  ).

    CLEAR: LC_INTEGRACAO.

  ENDMETHOD.
ENDCLASS.
