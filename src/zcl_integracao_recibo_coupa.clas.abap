class ZCL_INTEGRACAO_RECIBO_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_RECIBO_COUPA .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_RECIBO_COUPA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface                            = zif_integracao=>at_id_interface_coupa_recibo.
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
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_recibo_coupa~convert_abap_to_xml.

    CALL TRANSFORMATION zt_coupa_recibo
      SOURCE inventory_transaction = is_import_data
      RESULT XML rv_xml
      OPTIONS xml_header = 'no'.

  ENDMETHOD.


  METHOD zif_integracao_recibo_coupa~get_instance.

    FREE: zif_integracao_recibo_coupa~at_if_integracao_recibo_coupa.

    CREATE OBJECT zif_integracao_recibo_coupa~at_if_integracao_recibo_coupa
      TYPE zcl_integracao_recibo_coupa.

    r_if_integracao_coupa = zif_integracao_recibo_coupa~at_if_integracao_recibo_coupa .

  ENDMETHOD.


  METHOD zif_integracao_recibo_coupa~set_ds_data.

    r_if_integracao_coupa = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.

  ENDMETHOD.


  METHOD zif_integracao_recibo_coupa~set_ds_url.

    DATA: lt_werbservice TYPE STANDARD TABLE OF zauth_webservice.

    DATA: wa_webservice TYPE zauth_webservice.

    r_if_integracao_coupa  = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service EQ 'COUPA_RECIBO'.
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

    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'COUPA_RECIBO' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

  ENDMETHOD.


  METHOD zif_integracao_recibo_coupa~set_init_import.

    me->zif_integracao_recibo_coupa~at_xml = me->zif_integracao_recibo_coupa~convert_abap_to_xml( EXPORTING
                                                                                                     is_import_data = is_import_data ).

    CONCATENATE is_import_data-custom_fields-id_sap sy-datum(4) INTO me->zif_integracao_recibo_coupa~at_id_referencia.
    me->zif_integracao_recibo_coupa~at_tp_referencia  = 'MG'.

    me->zif_integracao_inject~at_info_request_http-ds_body = me->zif_integracao_recibo_coupa~at_xml.

    me->zif_integracao_recibo_coupa~set_ds_url( ).

    IF is_import_data-status = 'Voided'. "Cancelamento
      SELECT SINGLE *
        FROM ekbe
        INTO @DATA(w_original)
        WHERE ebeln = @is_import_data-ebeln
        AND   ebelp = @is_import_data-ebelp
        AND   vgabe = '1'
        AND   belnr = @is_import_data-belnr.
      IF sy-subrc IS INITIAL.
        IF w_original-lfbnr IS NOT INITIAL.
          SELECT SINGLE id_recibo_coupa
            FROM zmmt0155
            INTO @DATA(lv_id)
            WHERE ebeln = @is_import_data-ebeln
            AND   ebelp = @is_import_data-ebelp
            AND   belnr = @w_original-lfbnr.
        ELSE.
          SELECT MAX( belnr )
            FROM  ekbe
            INTO @DATA(lv_original)
           WHERE ebeln = @is_import_data-ebeln
           AND   ebelp = @is_import_data-ebelp
           AND   vgabe = '1'
           AND   xblnr = @w_original-xblnr
           AND   shkzg = 'S'
           AND   belnr < @is_import_data-belnr.
          "
          IF sy-subrc = 0.
            SELECT SINGLE id_recibo_coupa
             FROM zmmt0155
             INTO lv_id
             WHERE ebeln = is_import_data-ebeln
             AND   ebelp = is_import_data-ebelp
             AND   belnr = lv_original.
          ELSE.
            sy-subrc = 4.
          ENDIF.

        ENDIF.
        IF sy-subrc IS INITIAL.
          CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_url '/' lv_id '/' 'void' INTO me->zif_integracao_inject~at_info_request_http-ds_url.
          me->zif_integracao_inject~at_info_request_http-ds_metodo = 'PUT'.
        ENDIF.
      ENDIF.
    ENDIF.

    TRY.
        me->zif_integracao_recibo_coupa~set_send_msg( IMPORTING
                                                        e_id_integracao      =  DATA(id)
                                                        e_retorno_integracao = e_retorno_integracao ).

      CATCH zcx_error INTO DATA(lo_error).

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


  METHOD zif_integracao_recibo_coupa~set_send_msg.

    DATA: lc_integrar   TYPE REF TO zcl_integracao.

    DATA: lo_integracao TYPE REF TO zif_integracao.

    DATA: e_integracao_log TYPE zintegracao_log.

    r_if_integracao_coupa = me.

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


  METHOD zif_integracao_recibo_coupa~set_servico.

    zif_integracao_recibo_coupa~at_servico = i_servico.

  ENDMETHOD.
ENDCLASS.
