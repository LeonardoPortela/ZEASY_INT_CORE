class ZCL_INTEGRACAO_ORCAMENTO_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_ORCAMENTO_COUPA .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_ORCAMENTO_COUPA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface                            = zif_integracao=>at_id_interface_coupa_orc.
    me->zif_integracao_inject~at_tp_integracao                           = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal                                = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia                            = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus                          = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao                         = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.

    e_sucesso = abap_false.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_integracao_token_coupa(
               zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                 )->get_token( )
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

    APPEND VALUE #( name = 'Accept' value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    r_if_integracao_inject = me.
    e_sucesso              = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_orcamento_coupa~get_instance.

    FREE: zif_integracao_orcamento_coupa~at_if_integracao_orc_coupa.

    CREATE OBJECT zif_integracao_orcamento_coupa~at_if_integracao_orc_coupa
      TYPE zcl_integracao_orcamento_coupa.

    r_zif_integracao_orc_coupa = zif_integracao_orcamento_coupa~at_if_integracao_orc_coupa.

  ENDMETHOD.


  METHOD zif_integracao_orcamento_coupa~set_ds_data.

    r_zif_integracao_orc_coupa  = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.

  ENDMETHOD.


  METHOD zif_integracao_orcamento_coupa~set_ds_url.

    DATA: lt_werbservice TYPE STANDARD TABLE OF zauth_webservice.

    DATA: wa_webservice TYPE zauth_webservice.

    r_zif_integracao_orc_coupa  = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service EQ 'COUPA_ORCAMENTO'.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'C'
                            attr2 = 'C1' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'C'
          msgv2  = 'C1'.
    ENDIF.

    SORT lt_werbservice BY service.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'.

    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'COUPA_ORCAMENTO' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo           = 'PUT'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

  ENDMETHOD.


  METHOD zif_integracao_orcamento_coupa~set_init_import.

    DATA: e_integracao_return TYPE zintegracao_log.

    me->zif_integracao_orcamento_coupa~set_ds_url( ).

    me->zif_integracao_orcamento_coupa~set_ds_data( i_xml = space ).

    IF iv_is_approved IS NOT INITIAL.
      CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_url '/' iv_id '/'  'approve' '?reason=' iv_mensagem INTO me->zif_integracao_inject~at_info_request_http-ds_url.
    ELSE.
      CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_url '/' iv_id '/'  'reject'  '?reason=' iv_mensagem INTO me->zif_integracao_inject~at_info_request_http-ds_url.
    ENDIF.

    TRY.
        me->zif_integracao_orcamento_coupa~set_send_msg( IMPORTING
                                                          e_retorno_integracao = e_integracao_return ).

        e_retorno_integracao = e_integracao_return.

      CATCH zcx_error INTO DATA(lo_error).

        e_retorno_integracao = e_integracao_return.

        DATA(lc_msgid) = lo_error->zif_error~msgid.
        DATA(lc_msgno) = lo_error->zif_error~msgno.
        DATA(lc_msgv1) = lo_error->zif_error~msgv1.
        DATA(lc_msgv2) = lo_error->zif_error~msgv2.
        DATA(lc_msgv3) = lo_error->zif_error~msgv3.
        DATA(lc_msgv4) = lo_error->zif_error~msgv4.


        "Propagar erro de Comunicação para Fora
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = lc_msgid
                              msgno = lc_msgno
                              attr1 = CONV #( lc_msgv1 )
                              attr2 = CONV #( lc_msgv2 )
                              attr3 = CONV #( lc_msgv3 )
                              attr4 = CONV #( lc_msgv4 ) )
            msgid  = lc_msgid
            msgno  = lc_msgno
            msgty  = 'E'
            msgv1  = lc_msgv1
            msgv2  = lc_msgv2
            msgv3  = lc_msgv3
            msgv4  = lc_msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_orcamento_coupa~set_send_msg.

    DATA: lc_integrar   TYPE REF TO zcl_integracao.

    DATA: lo_integracao TYPE REF TO zif_integracao.

    DATA: e_integracao_log TYPE zintegracao_log.

    r_zif_integracao_orc_coupa = me.

    CREATE OBJECT lc_integrar.

    TRY.
        "Cria MSG para Integração via HTTP
        lo_integracao = lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me ) ).
        lo_integracao->set_new_msg( IMPORTING e_id_integracao = e_id_integracao ).
        lo_integracao->set_outbound_msg( IMPORTING e_mensagem = e_integracao_log ).
        lo_integracao->set_processar_retorno(  ).
        lo_integracao->set_integrar_retorno( ).
        lo_integracao->get_registro( IMPORTING  e_integracao = e_integracao ).
        lo_integracao->free( ).

        CLEAR: lc_integrar.

        e_retorno_integracao = e_integracao_log.

      CATCH zcx_error INTO DATA(lo_error).

        e_retorno_integracao = e_integracao_log.

        DATA(lc_msgid) = lo_error->zif_error~msgid.
        DATA(lc_msgno) = lo_error->zif_error~msgno.
        DATA(lc_msgv1) = lo_error->zif_error~msgv1.
        DATA(lc_msgv2) = lo_error->zif_error~msgv2.
        DATA(lc_msgv3) = lo_error->zif_error~msgv3.
        DATA(lc_msgv4) = lo_error->zif_error~msgv4.


        "Propagar erro de Comunicação para Fora
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = lc_msgid
                              msgno = lc_msgno
                              attr1 = CONV #( lc_msgv1 )
                              attr2 = CONV #( lc_msgv2 )
                              attr3 = CONV #( lc_msgv3 )
                              attr4 = CONV #( lc_msgv4 ) )
            msgid  = lc_msgid
            msgno  = lc_msgno
            msgty  = 'E'
            msgv1  = lc_msgv1
            msgv2  = lc_msgv2
            msgv3  = lc_msgv3
            msgv4  = lc_msgv4.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
