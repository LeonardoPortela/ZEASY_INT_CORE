class ZCL_INTEGRACAO_CLI_KUHLMANN definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_CLI_KUHLMANN .
  interfaces ZIF_INTEGRACAO_INJECT .

  methods CONSTRUCTOR
    importing
      !I_SERVICO type ZTIPOWEBSERV
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_CLI_KUHLMANN IMPLEMENTATION.


  METHOD constructor.
    me->zif_integracao_cli_kuhlmann~set_servico( i_servico =  i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_kuhlmann_cli.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    me->zif_integracao_cli_kuhlmann~set_ds_url( ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CLI_KUHLMANN~GET_ID_REFERENCIA.
    R_IF_INTEGRACAO_CLI_KUHLMANN = ME.

    E_REFERENCIA-TP_REFERENCIA = 'KUHLMANN_CLIENTES'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO.

  ENDMETHOD.


  METHOD zif_integracao_cli_kuhlmann~get_instance.

    IF zif_integracao_cli_kuhlmann~at_if_integracao_cli_kuhlmann IS NOT BOUND.
      CREATE OBJECT zif_integracao_cli_kuhlmann~at_if_integracao_cli_kuhlmann
        TYPE zcl_integracao_cli_kuhlmann
        EXPORTING
          i_servico = i_servico. " Tipo de Serviço WebService
*        CATCH zcx_integracao.    " .
    ENDIF.

    r_if_integracao_cli_kuhlmann = zif_integracao_cli_kuhlmann~at_if_integracao_cli_kuhlmann.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CLI_KUHLMANN~GET_JSON.
    R_IF_INTEGRACAO_CLI_KUHLMANN = ME.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CLI_KUHLMANN~SET_CLIENTES_BUSCAR.

    R_IF_INTEGRACAO_CLI_KUHLMANN = ME.

    "ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_SENHA = I_SENHA.
    "ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO = I_USUARIO.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_CLI_KUHLMANN~GET_JSON( IMPORTING E_JSON = DATA(LC_JSON)
      )->SET_DS_DATA( EXPORTING I_JSON = LC_JSON
      )->SET_DS_URL(
      )->SET_SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO E_INTEGRACAO	= E_INTEGRACAO
      ).

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = E_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = E_CLIENTES ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_CLI_KUHLMANN~SET_DS_DATA.
    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_CLI_KUHLMANN = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  endmethod.


  METHOD zif_integracao_cli_kuhlmann~set_ds_url.

    r_if_integracao_cli_kuhlmann = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ 'K'
       AND servico EQ @me->zif_integracao_cli_kuhlmann~at_servico."'KU'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_integracao_cli_kuhlmann~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = conv #( me->zif_integracao_cli_kuhlmann~at_servico ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token    = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && zif_integracao_cli_kuhlmann=>at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_cli_kuhlmann=>at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_cli_kuhlmann~set_id_referencia( ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CLI_KUHLMANN~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_CLI_KUHLMANN = ME.
    ME->ZIF_INTEGRACAO_CLI_KUHLMANN~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CLI_KUHLMANN~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_CLI_KUHLMANN = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG(
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = E_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  ENDMETHOD.


  METHOD zif_integracao_cli_kuhlmann~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_integracao_cli_kuhlmann~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_cli_kuhlmann~set_tipo.

    IF i_tipo IS NOT INITIAL.
      zif_integracao_cli_kuhlmann~at_tipo = i_tipo.
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_integracao_token_kuhlmann(
               zcl_integracao_token_kuhlmann=>zif_integracao_token_kuhlmann~get_instance(
                   i_servico = CONV #( me->zif_integracao_cli_kuhlmann~at_servico )
                 )->set_ds_url(
                 )->set_usuario_senha( "EXPORTING I_SENHA = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_SENHA I_USUARIO = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO
                 )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.

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
