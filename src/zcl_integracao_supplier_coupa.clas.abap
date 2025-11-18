CLASS zcl_integracao_supplier_coupa DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_supplier_coupa .
    INTERFACES zif_integracao_inject .

    DATA gt_webservice TYPE STANDARD TABLE OF zauth_webservice .

    METHODS constructor
      IMPORTING
        !iv_execution_mode TYPE zcoupa_supplier_exec_mode .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_SUPPLIER_COUPA IMPLEMENTATION.


  METHOD constructor.

    CASE iv_execution_mode.
      WHEN '01'.
        me->zif_integracao_inject~at_id_interface = zif_integracao=>at_id_interface_fornecedores.
      WHEN '02'.
        me->zif_integracao_inject~at_id_interface = zif_integracao=>at_id_interface_get_id_cnpj.
      WHEN '03'.
        me->zif_integracao_inject~at_id_interface = zif_integracao=>at_id_interface_get_id_cpf.
      WHEN OTHERS.
    ENDCASE.

    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
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


  METHOD zif_integracao_supplier_coupa~convert_abap_to_xml.

    IF is_create IS NOT INITIAL.
      CALL TRANSFORMATION zt_coupa_supplier_create
        SOURCE supplier = is_coupa_supplier_data_create
        RESULT XML rv_xml
        OPTIONS xml_header = 'no'.
    ENDIF.

    IF is_modify IS NOT INITIAL.
      CALL TRANSFORMATION zt_coupa_supplier_modify
        SOURCE supplier = is_coupa_supplier_data_modify
        RESULT XML rv_xml
        OPTIONS xml_header = 'no'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_supplier_coupa~get_id_fornecedor_by_cnpj.

    TRY .

        zcl_integracao_supplier_coupa=>zif_integracao_supplier_coupa~get_instance( EXPORTING
                                                                                     iv_execution_mode = '02' "Obter fornecedor por CNPJ
          )->set_init_import( EXPORTING
                                iv_cnpj = iv_cnpj
                              IMPORTING
                               e_retorno_integracao   = DATA(gs_integracao_return)
                               e_id_fornecedor        = rv_id_fornecedor
                               e_commodity_name       = e_commodity_name
                               e_id_adress_primary    = e_id_adress_primary
                               e_id_adress_suplier    = e_id_adress_suplier
                               e_id_contact           = e_id_contact
                              ).

      CATCH zcx_integracao INTO DATA(ex_integra).

      CATCH zcx_error INTO DATA(ex_error).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_supplier_coupa~get_id_fornecedor_by_cpf.

    TRY .

        zcl_integracao_supplier_coupa=>zif_integracao_supplier_coupa~get_instance( EXPORTING
                                                                                     iv_execution_mode = '03' "Obter fornecedor por CPF
          )->set_init_import( EXPORTING
                                iv_cpf = iv_cpf
                              IMPORTING
                               e_retorno_integracao   = DATA(gs_integracao_return)
                               e_id_fornecedor        = rv_id_fornecedor
                               e_commodity_name       = e_commodity_name
                               e_id_adress_primary    = e_id_adress_primary
                               e_id_adress_suplier    = e_id_adress_suplier
                               e_id_contact           = e_id_contact
                              ).

      CATCH zcx_integracao INTO DATA(ex_integra).

      CATCH zcx_error INTO DATA(ex_error).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_supplier_coupa~get_instance.


    FREE: zif_integracao_supplier_coupa~at_if_integracao_sup_coupa.

    CREATE OBJECT zif_integracao_supplier_coupa~at_if_integracao_sup_coupa TYPE zcl_integracao_supplier_coupa
      EXPORTING
        iv_execution_mode = iv_execution_mode.

    r_if_integracao_supplier_coupa = zif_integracao_supplier_coupa~at_if_integracao_sup_coupa.

  ENDMETHOD.


  METHOD zif_integracao_supplier_coupa~set_ds_data.

    r_if_integracao_supplier_coupa  = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.

  ENDMETHOD.


  METHOD zif_integracao_supplier_coupa~set_ds_url.


    DATA: wa_webservice TYPE zauth_webservice.

    r_if_integracao_supplier_coupa  = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE me->gt_webservice
      WHERE service IN ('COUPA_FORNECEDOR_CRIAR', 'COUPA_FORNECEDOR_OBTER_CPF', 'COUPA_FORNECEDOR_OBTER_CNPJ', 'COUPA_FORNECEDOR_OBTER_CNPJ_SIM' ).
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

    SORT me->gt_webservice BY service.

*    CLEAR: me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = 'x-coupa-api-key'  value = 'be82ead986a55cbb33155ab2c7661641307a8b8b' ) TO me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = 'Accept'           value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'.

    CASE me->zif_integracao_inject~at_id_interface.
      WHEN '054'.

        READ TABLE me->gt_webservice INTO wa_webservice WITH KEY service = 'COUPA_FORNECEDOR_CRIAR' BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
        ENDIF.
        me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.

      WHEN '055'.

        READ TABLE me->gt_webservice INTO wa_webservice WITH KEY service = 'COUPA_FORNECEDOR_OBTER_CNPJ' BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONCATENATE wa_webservice-url me->zif_integracao_supplier_coupa~at_cnpj INTO me->zif_integracao_inject~at_info_request_http-ds_url.
        ENDIF.
        me->zif_integracao_inject~at_info_request_http-ds_metodo = 'GET'.

      WHEN '056'.

        READ TABLE me->gt_webservice INTO wa_webservice WITH KEY service = 'COUPA_FORNECEDOR_OBTER_CPF' BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CONCATENATE wa_webservice-url me->zif_integracao_supplier_coupa~at_cpf INTO me->zif_integracao_inject~at_info_request_http-ds_url.
        ENDIF.
        me->zif_integracao_inject~at_info_request_http-ds_metodo = 'GET'.

    ENDCASE.

    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

  ENDMETHOD.


  METHOD zif_integracao_supplier_coupa~set_init_import.

    DATA: lv_id            TYPE zde_id_integracao,
          lv_id_fornecedor TYPE string,
          lv_string        TYPE string,
          lv_xstring       TYPE xstring.

    DATA: lt_xml_tab TYPE srt_xml_data_tab.

    DATA: e_integracao_return TYPE zintegracao_log.

    CASE me->zif_integracao_inject~at_id_interface.
      WHEN '054'.
        IF is_create IS NOT INITIAL.
          me->zif_integracao_supplier_coupa~at_xml = me->zif_integracao_supplier_coupa~convert_abap_to_xml( EXPORTING
                                                                                                             is_coupa_supplier_data_create = is_coupa_supplier_data_create
                                                                                                             is_create                     = is_create ).
          me->zif_integracao_supplier_coupa~at_id_referencia  = is_coupa_supplier_data_create-number.
        ENDIF.
        IF is_modify IS NOT INITIAL.
          me->zif_integracao_supplier_coupa~at_xml = me->zif_integracao_supplier_coupa~convert_abap_to_xml( EXPORTING
                                                                                                             is_coupa_supplier_data_modify = is_coupa_supplier_data_modify
                                                                                                             is_modify                     = is_modify ).
          me->zif_integracao_supplier_coupa~at_id_referencia  = is_coupa_supplier_data_modify-number.
        ENDIF.
        me->zif_integracao_supplier_coupa~at_tp_referencia  = 'COUPA_FORNECEDORES'.
      WHEN '055'.
        me->zif_integracao_supplier_coupa~at_xml = space.
        me->zif_integracao_supplier_coupa~at_id_referencia  = iv_cnpj.
        me->zif_integracao_supplier_coupa~at_tp_referencia  = 'COUPA_FORNECEDORES'.
        me->zif_integracao_supplier_coupa~at_cnpj           = iv_cnpj.
      WHEN '056'.
        me->zif_integracao_supplier_coupa~at_xml = space.
        me->zif_integracao_supplier_coupa~at_id_referencia  = iv_cpf.
        me->zif_integracao_supplier_coupa~at_tp_referencia  = 'COUPA_FORNECEDORES'.
        me->zif_integracao_supplier_coupa~at_cpf            = iv_cpf.
    ENDCASE.

    lv_id = me->zif_integracao_supplier_coupa~at_id_referencia.

    me->zif_integracao_supplier_coupa~set_ds_url( ).

    IF is_modify IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_metodo = 'PUT'.

      lv_string = me->zif_integracao_supplier_coupa~at_xml.

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
                                                       tag_level = 2.
      IF sy-subrc IS INITIAL.
        lv_id_fornecedor = ls_xml-tag_value.
      ENDIF.

      CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_url '/' lv_id_fornecedor INTO me->zif_integracao_inject~at_info_request_http-ds_url.

      FREE: lt_xml_tab, ls_xml, lv_xstring, lv_string.
    ENDIF.

    IF me->zif_integracao_supplier_coupa~at_xml IS NOT INITIAL.
      me->zif_integracao_supplier_coupa~set_ds_data( i_xml = me->zif_integracao_supplier_coupa~at_xml ).
    ENDIF.

    TRY.
        me->zif_integracao_supplier_coupa~set_send_msg( IMPORTING
                                                        e_id_integracao      =  lv_id
                                                        e_retorno_integracao = e_integracao_return ).

        e_retorno_integracao = e_integracao_return.

        lv_string = e_retorno_integracao-ds_data_retorno.
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

        LOOP AT lt_xml_tab INTO ls_xml.
          DATA(l_tabix) = sy-tabix.

          IF ls_xml-tag_name EQ 'primary-address'.
            DATA(l_index) = l_tabix + 1.
            READ TABLE lt_xml_tab INTO DATA(ls_xml_adress) INDEX l_index.
            IF sy-subrc IS INITIAL AND ls_xml_adress-tag_name EQ 'id'.
              e_id_adress_primary = ls_xml_adress-tag_value.
              EXIT.
            ENDIF.
          ENDIF.

        ENDLOOP.

        LOOP AT lt_xml_tab INTO ls_xml.
          l_tabix = sy-tabix.

          IF ls_xml-tag_name EQ 'supplier-address'.
            l_index = l_tabix + 1.
            READ TABLE lt_xml_tab INTO ls_xml_adress INDEX l_index.
            IF sy-subrc IS INITIAL AND ls_xml_adress-tag_name EQ 'id'.
              e_id_adress_suplier = ls_xml_adress-tag_value.
              EXIT.
            ENDIF.
          ENDIF.

        ENDLOOP.

        LOOP AT lt_xml_tab INTO ls_xml.
          l_tabix = sy-tabix.

          IF ls_xml-tag_name EQ 'primary-contact'.
            l_index = l_tabix + 1.
            READ TABLE lt_xml_tab INTO ls_xml_adress INDEX l_index.
            IF sy-subrc IS INITIAL AND ls_xml_adress-tag_name EQ 'id'.
              e_id_contact = ls_xml_adress-tag_value.
              EXIT.
            ENDIF.
          ENDIF.

        ENDLOOP.

        READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'id'
                                                         tag_level = 3.
        IF sy-subrc IS INITIAL.
          e_id_fornecedor = ls_xml-tag_value.
        ENDIF.

        READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'name'
                                                   tag_type  = 'DATA'
                                                   tag_level = 5.
        IF sy-subrc IS INITIAL.
          IF  ls_xml-tag_value EQ 'Homologado'.
            e_commodity_name = 'Outros - (Não Crítico)'.
            EXIT.
          ENDIF.
          IF ls_xml-tag_value EQ 'Outros - (Não Crítico)'.
            e_commodity_name = 'Outros - (Não Crítico)'.
            EXIT.
          ENDIF.
          DELETE lt_xml_tab INDEX sy-tabix.
          READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'name'
                                                     tag_type  = 'DATA'
                                                     tag_level = 5.
          IF sy-subrc IS INITIAL.
            IF  ls_xml-tag_value EQ 'Homologado'.
              e_commodity_name = 'Outros - (Não Crítico)'.
              EXIT.
            ENDIF.
            IF ls_xml-tag_value EQ 'Outros - (Não Crítico)'.
              e_commodity_name = 'Outros - (Não Crítico)'.
              EXIT.
            ENDIF.
            DELETE lt_xml_tab INDEX sy-tabix.
            READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'name'
                                                       tag_type  = 'DATA'
                                                       tag_level = 5.
            IF sy-subrc IS INITIAL.
              IF  ls_xml-tag_value EQ 'Homologado'.
                e_commodity_name = 'Outros - (Não Crítico)'.
                EXIT.
              ENDIF.
              IF ls_xml-tag_value EQ 'Outros - (Não Crítico)'.
                e_commodity_name = 'Outros - (Não Crítico)'.
                EXIT.
              ENDIF.
              DELETE lt_xml_tab INDEX sy-tabix.
              READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'name'
                                                         tag_type  = 'DATA'
                                                         tag_level = 5.
              IF sy-subrc IS INITIAL.
                IF  ls_xml-tag_value EQ 'Homologado'.
                  e_commodity_name = 'Outros - (Não Crítico)'.
                  EXIT.
                ENDIF.
                IF ls_xml-tag_value EQ 'Outros - (Não Crítico)'.
                  e_commodity_name = 'Outros - (Não Crítico)'.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      CATCH zcx_error INTO DATA(lo_error).

        WAIT UP TO 3 SECONDS.

        FREE:  me->zif_integracao_inject~at_info_request_http-ds_url.

        READ TABLE me->gt_webservice INTO DATA(wa_webservice) WITH KEY service = 'COUPA_FORNECEDOR_OBTER_CNPJ_SIM' BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CASE me->zif_integracao_inject~at_id_interface.
            WHEN '055'.
              CONCATENATE wa_webservice-url me->zif_integracao_supplier_coupa~at_cnpj INTO me->zif_integracao_inject~at_info_request_http-ds_url.
            WHEN '056'.
              CONCATENATE wa_webservice-url me->zif_integracao_supplier_coupa~at_cpf INTO me->zif_integracao_inject~at_info_request_http-ds_url.
          ENDCASE.
        ENDIF.

        TRY.
            me->zif_integracao_supplier_coupa~set_send_msg( IMPORTING
                                                            e_id_integracao      =  lv_id
                                                            e_retorno_integracao = e_integracao_return ).

            e_retorno_integracao = e_integracao_return.

            lv_string = e_retorno_integracao-ds_data_retorno.
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

            READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'id'
                                                             tag_level = 3.
            IF sy-subrc IS INITIAL.
              e_id_fornecedor = ls_xml-tag_value.
            ENDIF.

            READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'name'
                                                       tag_type  = 'DATA'
                                                       tag_level = 5.
            IF sy-subrc IS INITIAL.
              IF  ls_xml-tag_value EQ 'Homologado'.
                e_commodity_name = 'Outros - (Não Crítico)'.
                EXIT.
              ENDIF.
              IF ls_xml-tag_value EQ 'Outros - (Não Crítico)'.
                e_commodity_name = 'Outros - (Não Crítico)'.
                EXIT.
              ENDIF.
              DELETE lt_xml_tab INDEX sy-tabix.
              READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'name'
                                                         tag_type  = 'DATA'
                                                         tag_level = 5.
              IF sy-subrc IS INITIAL.
                IF  ls_xml-tag_value EQ 'Homologado'.
                  e_commodity_name = 'Outros - (Não Crítico)'.
                  EXIT.
                ENDIF.
                IF ls_xml-tag_value EQ 'Outros - (Não Crítico)'.
                  e_commodity_name = 'Outros - (Não Crítico)'.
                  EXIT.
                ENDIF.
                DELETE lt_xml_tab INDEX sy-tabix.
                READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'name'
                                                           tag_type  = 'DATA'
                                                           tag_level = 5.
                IF sy-subrc IS INITIAL.
                  IF  ls_xml-tag_value EQ 'Homologado'.
                    e_commodity_name = 'Outros - (Não Crítico)'.
                    EXIT.
                  ENDIF.
                  IF ls_xml-tag_value EQ 'Outros - (Não Crítico)'.
                    e_commodity_name = 'Outros - (Não Crítico)'.
                    EXIT.
                  ENDIF.
                  DELETE lt_xml_tab INDEX sy-tabix.
                  READ TABLE lt_xml_tab INTO ls_xml WITH KEY tag_name  = 'name'
                                                             tag_type  = 'DATA'
                                                             tag_level = 5.
                  IF sy-subrc IS INITIAL.
                    IF  ls_xml-tag_value EQ 'Homologado'.
                      e_commodity_name = 'Outros - (Não Crítico)'.
                      EXIT.
                    ENDIF.
                    IF ls_xml-tag_value EQ 'Outros - (Não Crítico)'.
                      e_commodity_name = 'Outros - (Não Crítico)'.
                      EXIT.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

          CATCH zcx_error INTO lo_error.

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
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_supplier_coupa~set_send_msg.

    DATA: lc_integrar   TYPE REF TO zcl_integracao.

    DATA: lo_integracao TYPE REF TO zif_integracao.

    DATA: e_integracao_log TYPE zintegracao_log.

    r_if_integracao_supplier_coupa = me.

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


  METHOD zif_integracao_supplier_coupa~set_servico.

    zif_integracao_supplier_coupa~at_servico = i_servico.

  ENDMETHOD.
ENDCLASS.
