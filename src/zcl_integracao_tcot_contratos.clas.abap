class ZCL_INTEGRACAO_TCOT_CONTRATOS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_TCOT_CONTRATOS .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TCOT_CONTRATOS IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_tcot_contratos.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_sim.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'tracecotton'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.

*    DATA: lva_id_referencia TYPE zintegracao-id_referencia,
*          lva_id_integracao TYPE zintegracao-id_integracao.
*
*    lva_id_integracao =  c_integracao-id_integracao.
*    lva_id_referencia =  me->zif_integracao_tcot_contratos~at_id_referencia.
*
*    UPDATE zintegracao
*         SET id_referencia = lva_id_referencia
*           WHERE id_integracao        = lva_id_integracao
*           AND   id_interface         = '040'.
*    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_tcot_contratos~get_id_referencia.
    r_if_integracao_tcot_contratos = me.
    e_referencia-tp_referencia = 'TRACE COTTON - CONTRATOS'.
    e_referencia-id_referencia = me->zif_integracao_tcot_contratos~at_id_referencia.
  ENDMETHOD.


  METHOD zif_integracao_tcot_contratos~get_instance.
    IF zif_integracao_tcot_contratos~at_if_integracao_tcot_cont IS NOT BOUND.
      CREATE OBJECT zif_integracao_tcot_contratos~at_if_integracao_tcot_cont
        TYPE zcl_integracao_tcot_contratos.
    ENDIF.
    r_if_integracao_tcot_contratos = zif_integracao_tcot_contratos~at_if_integracao_tcot_cont.
  ENDMETHOD.


  METHOD zif_integracao_tcot_contratos~get_json.
    r_if_integracao_tcot_contratos = me.

    CHECK me->zif_integracao_tcot_contratos~at_json IS NOT INITIAL.
    e_json = me->zif_integracao_tcot_contratos~at_json.
  ENDMETHOD.


  METHOD zif_integracao_tcot_contratos~get_metodo.
    r_if_integracao_tcot_contratos = me.

    CHECK me->zif_integracao_tcot_contratos~at_metodo IS NOT INITIAL.
    e_metodo = me->zif_integracao_tcot_contratos~at_metodo.
  ENDMETHOD.


  METHOD zif_integracao_tcot_contratos~set_ds_data.
"Incluir Texto JSON para integração
    r_if_integracao_tcot_contratos = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  ENDMETHOD.


  METHOD zif_integracao_tcot_contratos~set_ds_url.
    r_if_integracao_tcot_contratos = me.

    DATA: v_url      TYPE string.

    CHECK me->zif_integracao_tcot_contratos~at_metodo IS NOT INITIAL.
    DATA(lva_metodo) = me->zif_integracao_tcot_contratos~at_metodo.

    SELECT SINGLE * FROM zauth_webservice INTO @DATA(wa_webservice)
       WHERE service = 'TRACE_COTTON_CONTRATOS'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'T'
                            attr2 = 'TC' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'T'
          msgv2  = 'TC'.
    ENDIF.

    CASE lva_metodo.
      WHEN 'POST'.
        CONCATENATE wa_webservice-url 'incluirContrato'    INTO v_url.
      WHEN 'PUT'.
        CONCATENATE wa_webservice-url 'atualizarContrato'  INTO v_url.
      WHEN 'DELETE'.
        CONCATENATE wa_webservice-url 'excluirContrato'    INTO v_url.
    ENDCASE.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=UTF-8' ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token    = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = v_url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo =  lva_metodo.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_tcot_contratos~set_id_referencia( ).

  ENDMETHOD.


  METHOD zif_integracao_tcot_contratos~set_id_referencia.
    "Incluir Chave de Referência
    r_if_integracao_tcot_contratos = me.
    me->zif_integracao_tcot_contratos~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_integracao_tcot_contratos~set_int_contratos.

    r_if_integracao_tcot_contratos = me.

    me->zif_integracao_tcot_contratos~at_json = i_json.
    me->zif_integracao_tcot_contratos~at_metodo = i_metodo.
    me->zif_integracao_tcot_contratos~at_id_referencia = i_id_referencia.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_tcot_contratos~set_ds_url(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->get_metodo( IMPORTING e_metodo = DATA(lc_metodo)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

  ENDMETHOD.


      METHOD zif_integracao_tcot_contratos~set_send_msg.
        DATA: lc_integrar TYPE REF TO zcl_integracao.

        r_if_integracao_tcot_contratos = me.

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
ENDCLASS.
