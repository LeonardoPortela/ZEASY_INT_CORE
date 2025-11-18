class ZCL_INTEGRACAO_TOKEN_KUHLMANN definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_TOKEN_KUHLMANN .
  interfaces ZIF_INTEGRACAO_INJECT .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TOKEN_KUHLMANN IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_token_kuhlmann~set_servico( i_servico =  i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_kuhlmann_aut.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_token_kuhlmann~set_ds_url( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
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


  METHOD ZIF_INTEGRACAO_TOKEN_KUHLMANN~GET_ID_REFERENCIA.
    R_IF_INTEGRACAO_TOKEN_KUHLMAN = ME.
    E_REFERENCIA-TP_REFERENCIA = 'KUHLMANN_TOKEN'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~AT_USUARIO.
  ENDMETHOD.


  METHOD zif_integracao_token_kuhlmann~get_instance.
    IF zif_integracao_token_kuhlmann~at_if_token_kuhlmann IS NOT BOUND.
      CREATE OBJECT zif_integracao_token_kuhlmann~at_if_token_kuhlmann
        TYPE zcl_integracao_token_kuhlmann
        EXPORTING
          i_servico = i_servico.   " Tipo de Serviço WebService
*        CATCH zcx_integracao.    " .
    ENDIF.

    IF i_servico IS NOT INITIAL.
      zif_integracao_token_kuhlmann~at_servico = i_servico.
    ENDIF.

    r_if_integracao_token_kuhlman = zif_integracao_token_kuhlmann~at_if_token_kuhlmann.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_TOKEN_KUHLMANN~GET_JSON.

    R_IF_INTEGRACAO_TOKEN_KUHLMAN = ME.

    E_JSON = '{' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             ' "email" : "' && ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~AT_USUARIO && '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             ' "senha" : "' && ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~AT_SENHA && '" ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             '}'.

  ENDMETHOD.


  method ZIF_INTEGRACAO_TOKEN_KUHLMANN~SET_DS_DATA.

    R_IF_INTEGRACAO_TOKEN_KUHLMAN = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  endmethod.


  METHOD zif_integracao_token_kuhlmann~set_ds_url.

    r_if_integracao_token_kuhlman = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo EQ 'K'
       AND servico EQ @me->zif_integracao_token_kuhlmann~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_integracao_token_kuhlmann~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_integracao_token_kuhlmann~at_servico ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && zif_integracao_token_kuhlmann=>at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_token_kuhlmann=>at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_token_kuhlmann~at_usuario = wa_webservice-usuario.
    me->zif_integracao_token_kuhlmann~at_senha   = wa_webservice-senha.
    me->zif_integracao_token_kuhlmann~set_id_referencia( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_TOKEN_KUHLMANN~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_TOKEN_KUHLMAN = ME.
    ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  endmethod.


  METHOD ZIF_INTEGRACAO_TOKEN_KUHLMANN~SET_SEND_MSG.

    TYPES BEGIN OF TY_RETORNO.
    TYPES: TOKEN TYPE STRING.
    TYPES END OF TY_RETORNO.

    R_IF_INTEGRACAO_TOKEN_KUHLMAN = ME.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO,
          LC_RETORNO  TYPE TY_RETORNO.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG(
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = DATA(E_INTEGRACAO)
      )->FREE(
      ).

    FREE: LC_INTEGRAR.
    CLEAR: LC_INTEGRAR.

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = E_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

    E_ACCESS_TOKEN = LC_RETORNO-TOKEN.

  ENDMETHOD.


  METHOD zif_integracao_token_kuhlmann~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_integracao_token_kuhlmann~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_TOKEN_KUHLMANN~SET_USUARIO_SENHA.

    R_IF_INTEGRACAO_TOKEN_KUHLMAN = ME.

    "ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~AT_USUARIO = I_USUARIO.
    "ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~AT_SENHA = I_SENHA.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~GET_JSON( IMPORTING E_JSON = DATA(LC_JSON)
      )->SET_DS_DATA( EXPORTING I_JSON = LC_JSON
      )->SET_DS_URL(
      )->SET_SEND_MSG(
           IMPORTING
             E_ID_INTEGRACAO = E_ID_INTEGRACAO
             E_ACCESS_TOKEN  = E_ACCESS_TOKEN
      ).

    CLEAR: ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
    APPEND VALUE #( NAME = 'Authorization'  VALUE = |Bearer { E_ACCESS_TOKEN }| ) TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.
ENDCLASS.
