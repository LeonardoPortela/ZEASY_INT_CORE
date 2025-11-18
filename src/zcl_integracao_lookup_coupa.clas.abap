class ZCL_INTEGRACAO_LOOKUP_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_LOOKUP_COUPA .

  methods CONSTRUCTOR
    importing
      !IS_GET_ID type XFLAG default SPACE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_LOOKUP_COUPA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_lookup_coupa.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    IF is_get_id IS NOT INITIAL.
      me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_lookup_get_id.
    ENDIF.

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


  METHOD zif_integracao_lookup_coupa~get_id_lookup_value.

    DATA: lt_werbservice      TYPE STANDARD TABLE OF zauth_webservice.
    DATA: lt_xml_tab          TYPE srt_xml_data_tab.
    DATA: wa_webservice       TYPE zauth_webservice.
    DATA: lv_id      TYPE zde_id_integracao,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.
    DATA: e_integracao_return TYPE zintegracao_log.

    me->zif_integracao_lookup_coupa~at_id_referencia  = iv_external_ref_num.
    me->zif_integracao_lookup_coupa~at_tp_referencia  = iv_lookup_name.

    lv_id = me->zif_integracao_lookup_coupa~at_id_referencia.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service EQ 'COUPA_LOOKUP_GET_ID'.
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

    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'x-coupa-api-key'  value = 'be82ead986a55cbb33155ab2c7661641307a8b8b' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Accept'           value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'.

    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'COUPA_LOOKUP_GET_ID' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF iv_external_ref_num IS NOT INITIAL.
        CONCATENATE wa_webservice-url iv_external_ref_num INTO me->zif_integracao_inject~at_info_request_http-ds_url.
      ELSEIF iv_lookup_name IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF 'num' IN wa_webservice-url WITH 'code'.
        CONCATENATE wa_webservice-url iv_lookup_name INTO me->zif_integracao_inject~at_info_request_http-ds_url.
      ENDIF.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo           = 'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.


    TRY.
        me->zif_integracao_lookup_coupa~set_send_msg( IMPORTING
                                                        e_id_integracao      =  lv_id
                                                        e_retorno_integracao = e_integracao_return ).

        lv_string = e_integracao_return-ds_data_retorno.

        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = lv_string
          IMPORTING
            buffer = lv_xstring
          EXCEPTIONS
            failed = 1
            OTHERS = 2.
        IF sy-subrc <> 0.
        ENDIF.

        CALL FUNCTION 'SRTUTIL_CONVERT_XML_TO_TABLE'
          EXPORTING
            xdoc = lv_xstring
          IMPORTING
            data = lt_xml_tab.

        READ TABLE lt_xml_tab INTO DATA(ls_xml) WITH KEY tag_name  = 'id'
                                                         tag_level = 3.
        IF sy-subrc IS INITIAL.
          ev_id = ls_xml-tag_value.
        ENDIF.

      CATCH zcx_error INTO DATA(lo_error).

        FREE: ev_id.

    ENDTRY.


  ENDMETHOD.


  METHOD zif_integracao_lookup_coupa~get_instance.

    FREE: zif_integracao_lookup_coupa~at_if_integracao_lookup_coupa.

    CREATE OBJECT zif_integracao_lookup_coupa~at_if_integracao_lookup_coupa
      TYPE zcl_integracao_lookup_coupa.

    r_if_integracao_lookup_coupa = zif_integracao_lookup_coupa~at_if_integracao_lookup_coupa.

  ENDMETHOD.


  METHOD zif_integracao_lookup_coupa~set_ds_data.

    r_if_integracao_lookup_coupa  = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.

  ENDMETHOD.


  METHOD zif_integracao_lookup_coupa~set_ds_url.

    DATA: lt_werbservice TYPE STANDARD TABLE OF zauth_webservice.

    DATA: wa_webservice TYPE zauth_webservice.

    r_if_integracao_lookup_coupa = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service IN ('COUPA_TOKEN', 'COUPA_USER', 'COUPA_LOOKUP' ).
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

    CLEAR: me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = 'x-coupa-api-key'  value = 'be82ead986a55cbb33155ab2c7661641307a8b8b' ) TO me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = 'Accept'           value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'.

    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'COUPA_LOOKUP' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo           =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    IF iv_id IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_metodo         = 'PUT'.
      CONCATENATE wa_webservice-url '/' iv_id INTO me->zif_integracao_inject~at_info_request_http-ds_url.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_lookup_coupa~set_init_import.

    DATA: lo_integracao_lookup_coupa TYPE REF TO zcl_integracao_lookup_coupa.

    DATA: lt_xml_tab TYPE srt_xml_data_tab.

    DATA: l_external_refnum TYPE csks-kostl,
          l_aufnr           TYPE coas-aufnr.

    DATA: lv_id      TYPE zde_id_integracao,
          lv_string  TYPE string,
          lv_xstring TYPE xstring,
          lv_refnmum TYPE string,
          lv_refcode TYPE string,
          lv_parent  TYPE string,
          ls_id      TYPE string.

    DATA: e_integracao_return TYPE zintegracao_log.

    CREATE OBJECT lo_integracao_lookup_coupa
      EXPORTING
        is_get_id = 'X'.

    lv_string = is_coupa_import_data-xml.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_string
      IMPORTING
        buffer = lv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'SRTUTIL_CONVERT_XML_TO_TABLE'
      EXPORTING
        xdoc = lv_xstring
      IMPORTING
        data = lt_xml_tab.

    READ TABLE lt_xml_tab INTO DATA(ls_xml) WITH KEY tag_name  = 'external-ref-num'
                                                     tag_level = 2.
    IF sy-subrc IS INITIAL.
      lv_refnmum = ls_xml-tag_value.
    ENDIF.

    l_external_refnum = lv_refnmum.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_external_refnum
      IMPORTING
        output = l_external_refnum.

    SELECT COUNT(*)
      FROM csks
      WHERE kostl = l_external_refnum.
    IF sy-subrc IS INITIAL.
      READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'external-ref-code'
                                                 tag_level = 3.
      IF sy-subrc IS INITIAL.
        lv_refcode = ls_xml-tag_value.
        CONCATENATE lv_refcode '|' lv_refnmum INTO lv_parent.
        FREE:  lv_refnmum.
      ENDIF.
    ENDIF.

    READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'external-ref-code'
                                               tag_level = 3.
    IF sy-subrc IS INITIAL.
      SPLIT ls_xml-tag_value AT '|' INTO DATA(centro) DATA(clas_cont) DATA(centro_custo) DATA(ordem).
      l_aufnr = ordem.
      l_aufnr = |{ l_aufnr ALPHA = IN }|.

      SELECT COUNT(*)
        FROM v_npact
        WHERE aufnr = l_aufnr
        AND   vornr = lv_refnmum.
      IF sy-subrc IS INITIAL.
        CONCATENATE ls_xml-tag_value '|' lv_refnmum INTO lv_parent.
        FREE: lv_refnmum.
      ENDIF.
    ENDIF.

    lo_integracao_lookup_coupa->zif_integracao_lookup_coupa~get_id_lookup_value( EXPORTING
                                                                                   iv_external_ref_num = lv_refnmum
                                                                                   iv_lookup_name      = lv_parent
                                                                                 IMPORTING
                                                                                   ev_id               = ls_id ).

    me->zif_integracao_lookup_coupa~at_xml            = is_coupa_import_data-xml.
    me->zif_integracao_lookup_coupa~at_id_referencia  = is_coupa_import_data-id_referencia.
    me->zif_integracao_lookup_coupa~at_tp_referencia  = is_coupa_import_data-tp_referencia.

    lv_id = me->zif_integracao_lookup_coupa~at_id_referencia.

    me->zif_integracao_lookup_coupa~set_ds_url( EXPORTING
                                                 iv_id = ls_id ).
    me->zif_integracao_lookup_coupa~set_ds_data( i_xml = me->zif_integracao_lookup_coupa~at_xml ).

    TRY.
        me->zif_integracao_lookup_coupa~set_send_msg( IMPORTING
                                                        e_id_integracao      =  lv_id
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


  METHOD zif_integracao_lookup_coupa~set_send_msg.

    DATA: lc_integrar   TYPE REF TO zcl_integracao.

    DATA: lo_integracao TYPE REF TO zif_integracao.

    DATA: e_integracao_log TYPE zintegracao_log.

    r_if_integracao_lookup_coupa = me.

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


  METHOD zif_integracao_lookup_coupa~set_servico.

    zif_integracao_lookup_coupa~at_servico = i_servico.

  ENDMETHOD.
ENDCLASS.
