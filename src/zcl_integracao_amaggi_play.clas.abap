class ZCL_INTEGRACAO_AMAGGI_PLAY definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_AMAGGI_PLAY .

  methods CONSTRUCTOR
    importing
      !I_SERVICO type ZTIPOWEBSERV
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_AMAGGI_PLAY IMPLEMENTATION.


  METHOD constructor.
    me->zif_integracao_amaggi_play~set_servico( i_servico =  i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_amaggi_play.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    me->zif_integracao_amaggi_play~set_ds_url( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_AMAGGI_PLAY~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_AMAGGI_PLAY = ME.

    E_REFERENCIA-TP_REFERENCIA = 'AMAGGI_PLAY'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_AMAGGI_PLAY~AT_USUARIO.
  endmethod.


  METHOD zif_integracao_amaggi_play~get_instance.

    IF zif_integracao_amaggi_play~at_if_integracao_amaggi_play IS NOT BOUND.
      CREATE OBJECT zif_integracao_amaggi_play~at_if_integracao_amaggi_play
        TYPE zcl_integracao_amaggi_play
        EXPORTING
          i_servico = i_servico. " Tipo de Serviço WebService
*        CATCH zcx_integracao.    " .
    ENDIF.

    r_if_integracao_amaggi_play = zif_integracao_amaggi_play~at_if_integracao_amaggi_play.


  ENDMETHOD.


  method ZIF_INTEGRACAO_AMAGGI_PLAY~GET_JSON.
    R_IF_INTEGRACAO_AMAGGI_PLAY = ME.
  endmethod.


  method ZIF_INTEGRACAO_AMAGGI_PLAY~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_AMAGGI_PLAY = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

*     RAISE EXCEPTION TYPE ZCX_INTEGRACAO
*      EXPORTING
*        TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
*                          MSGNO = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO )
*        MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
*        MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO
*        MSGTY  = 'E'.

  endmethod.


  METHOD zif_integracao_amaggi_play~set_ds_url.

    DATA: t_header TYPE zde_header_field_t.

    r_if_integracao_amaggi_play = me.

    SELECT SINGLE * INTO @DATA(zauth_webservice)
      FROM zauth_webservice
     WHERE service    EQ 'AMAGGI_PLAY'.


    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_integracao_amaggi_play~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_integracao_amaggi_play~at_servico ).
    ENDIF.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    "APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=UTF-8' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=ISO-8859-1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Token' value = zauth_webservice-token ) TO me->zif_integracao_inject~at_header_fields.


    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = zauth_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token    = zauth_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = zauth_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.


  ENDMETHOD.


  METHOD zif_integracao_amaggi_play~set_enviar_usuario.


    r_if_integracao_amaggi_play = me.

    "ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_SENHA = I_SENHA.
    "ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO = I_USUARIO.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_amaggi_play~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = me->zif_integracao_amaggi_play~at_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_json ).
  ENDMETHOD.


  method ZIF_INTEGRACAO_AMAGGI_PLAY~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_AMAGGI_PLAY = ME.
    ME->ZIF_INTEGRACAO_AMAGGI_PLAY~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).
  endmethod.


  METHOD zif_integracao_amaggi_play~set_json.

    r_if_integracao_amaggi_play = me.

    IF e_json IS NOT INITIAL.
      me->zif_integracao_amaggi_play~at_json = e_json.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_amaggi_play~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    R_IF_INTEGRACAO_AMAGGI_PLAY = ME.


    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.
  ENDMETHOD.


  METHOD zif_integracao_amaggi_play~set_servico.

    IF i_servico IS NOT INITIAL.
      zif_integracao_amaggi_play~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_amaggi_play~set_tipo.


    IF i_tipo IS NOT INITIAL.
      zif_integracao_amaggi_play~at_tipo = i_tipo.
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

*    r_if_integracao_inject = me.
*
*    TRY .
*        CAST zcl_integracao_token_kuhlmann(
*               zcl_integracao_token_kuhlmann=>zif_integracao_token_kuhlmann~get_instance(
*                   i_servico = CONV #( me->zif_integracao_cli_kuhlmann~at_servico )
*                 )->set_ds_url(
*                 )->set_usuario_senha( "EXPORTING I_SENHA = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_SENHA I_USUARIO = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO
*                 )
*             )->zif_integracao_inject~get_header_request_http(
*          IMPORTING
*            e_header_fields = DATA(e_header_fields) ).
*
*        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).
*
*      CATCH zcx_error INTO DATA(ex_erro).
*
*        RAISE EXCEPTION TYPE zcx_integracao
*          EXPORTING
*            textid = VALUE #( msgid = ex_erro->zif_error~msgid
*                              msgno = ex_erro->zif_error~msgno
*                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
*                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
*                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
*                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
*            msgid  = ex_erro->zif_error~msgid
*            msgno  = ex_erro->zif_error~msgno
*            msgty  = 'E'
*            msgv1  = ex_erro->zif_error~msgv1
*            msgv2  = ex_erro->zif_error~msgv2
*            msgv3  = ex_erro->zif_error~msgv3
*            msgv4  = ex_erro->zif_error~msgv4.
*
*    ENDTRY.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.
