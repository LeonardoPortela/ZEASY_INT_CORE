CLASS zcl_integracao_viagem_aprovar DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_viagem_aprovar .

    METHODS constructor
      RAISING
        zcx_integracao
        zcx_error .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_VIAGEM_APROVAR IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_aprovar.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_aprovar.

    IF sy-subrc EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    me->zif_integracao_viagem_aprovar~set_ds_url( ).

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.

    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.

    DATA: lc_id_viagem TYPE zde_viagem_id.

    TRY .

        zcl_integracao_viagem_status=>zif_integracao_viagem_status~get_instance(
          )->set_viagem_status( EXPORTING
                                  i_viagem_id = c_integracao-id_referencia
                                IMPORTING
                                  e_id_integracao  = DATA(e_id_integracao)
                                  e_status         = DATA(e_status)
          ).

*01	Carregamento
*02	Liberação do Embarcador
*03	Carregado
*04	Descarregado
*05	Cancelado
*06	Rejeitado pelo Embarcador
*07	Emissão de Nota Fiscal de Trânsito
*08	Liberação do Carregamento
*09	Carregamento Rejeitado pelo Embarcador
*10	Carregamento
*11	Resposta da Solicitação de Cancelamento
*12	Carregamento
*13	Resposta da Solicitação de Cancelamento
*14	Cancelado

        "Já foi liberado o embarque e está com o status liberação do carregamento
        CHECK e_status EQ '08'.

        e_sucesso = abap_true.

        c_integracao-id_before_error_outbound = e_id_integracao.

      CATCH zcx_integracao.
      CATCH zcx_error.
    ENDTRY.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    SELECT SINGLE * INTO @DATA(wa_viagem)
      FROM zlest0185
     WHERE viagem_id EQ @i_integracao-id_referencia.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgid
                            msgno = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgno
                            attr1 = CONV #( i_integracao-id_referencia ) )
          msgid  = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgid
          msgno  = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_integracao-id_referencia ).
    ENDIF.

    TRY .
        CAST zcl_integracao_token(
               zcl_integracao_token=>zif_integracao_token~get_instance(
                 )->set_empresa_token( EXPORTING i_bukrs = wa_viagem-bukrs
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


  METHOD zif_integracao_viagem_aprovar~get_id_referencia.

    r_if_integracao_viagem_aprovar = me.

    e_referencia-tp_referencia = 'CARGUERO_VIAGEM_APROVAR'.
    e_referencia-id_referencia = me->zif_integracao_viagem_aprovar~at_viagem-viagem_id.

  ENDMETHOD.


  METHOD zif_integracao_viagem_aprovar~get_instance.

    IF zif_integracao_viagem_aprovar~at_if_integracao_viagem_apr IS NOT BOUND.
      CREATE OBJECT zif_integracao_viagem_aprovar~at_if_integracao_viagem_apr TYPE zcl_integracao_viagem_aprovar.
    ENDIF.
    r_if_integracao_viagem_aprovar = zif_integracao_viagem_aprovar~at_if_integracao_viagem_apr.

  ENDMETHOD.


  METHOD zif_integracao_viagem_aprovar~get_json.

    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
    DATA: lc_numero_ordem TYPE string,
          lc_oc           TYPE string,
          lc_url          TYPE string.
    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676

    r_if_integracao_viagem_aprovar = me.

    IF me->zif_integracao_viagem_aprovar~at_viagem-id_ordem IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(wa_zsdt0001od)
        FROM zsdt0001od
       WHERE id_ordem EQ @me->zif_integracao_viagem_aprovar~at_viagem-id_ordem.

      lc_numero_ordem = zcl_string=>concat( s1 = CONV #( wa_zsdt0001od-nr_safra ) s2 = CONV #( wa_zsdt0001od-nr_ordem ) sp = '-' ).
      lc_oc  = COND string( WHEN wa_zsdt0001od-nr_ordem IS INITIAL THEN 'SEM ORDEM CARREGAMENTO' ELSE lc_numero_ordem ).
      lc_url = COND string( WHEN wa_zsdt0001od-ds_url_file IS INITIAL THEN 'https://amaggi-app.tipfrete.com.br/' ELSE wa_zsdt0001od-ds_url_file ).
    ENDIF.

    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
    IF me->zif_integracao_viagem_aprovar~at_viagem-nro_sol IS NOT INITIAL.
      SELECT SINGLE *
        FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
       WHERE viagem_id EQ @me->zif_integracao_viagem_aprovar~at_viagem-viagem_id.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                              msgno = zcx_integracao=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Carga encontrada! Solicitação:' )
                              attr2 = CONV #( me->zif_integracao_viagem_aprovar~at_viagem-nro_sol ) )
            msgid  = zcx_integracao=>zcx_erro_geral-msgid
            msgno  = zcx_integracao=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Carga encontrada! Solicitação:' )
            msgv2  = CONV #( me->zif_integracao_viagem_aprovar~at_viagem-nro_sol ).
      ENDIF.

      lc_numero_ordem  = lwa_zmmt0201-nro_cg.
      lc_oc  = COND string( WHEN lc_numero_ordem IS INITIAL THEN 'SEM ORDEM CARREGAMENTO' ELSE lc_numero_ordem ).
      lc_url = COND string( WHEN lwa_zmmt0201-ds_url_file_carguero IS INITIAL THEN 'https://amaggi-app.tipfrete.com.br/' ELSE lwa_zmmt0201-ds_url_file_carguero ).

    ENDIF.
    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676


    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    IF me->zif_integracao_viagem_aprovar~at_viagem-nro_cg_sai_in IS NOT INITIAL.
      SELECT SINGLE *
        FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
       WHERE nro_cg EQ @me->zif_integracao_viagem_aprovar~at_viagem-nro_cg_sai_in.

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                              msgno = zcx_integracao=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Carga encontrada! Solicitação:' )
                              attr2 = CONV #( me->zif_integracao_viagem_aprovar~at_viagem-nro_sol ) )
            msgid  = zcx_integracao=>zcx_erro_geral-msgid
            msgno  = zcx_integracao=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Carga encontrada! Solicitação:' )
            msgv2  = CONV #( me->zif_integracao_viagem_aprovar~at_viagem-nro_sol ).
      ENDIF.

      lc_numero_ordem  = lwa_zsdt0133-nro_cg.
      lc_oc  = COND string( WHEN lc_numero_ordem IS INITIAL THEN 'SEM ORDEM CARREGAMENTO' ELSE lc_numero_ordem ).
      lc_url = COND string( WHEN lwa_zsdt0133-ds_url_file_carguero IS INITIAL THEN 'https://amaggi-app.tipfrete.com.br/' ELSE lwa_zsdt0133-ds_url_file_carguero ).

    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----



    e_json = '{ ' &&
             '  "ordem_carregamento": {' &&
             '    "numero": "' && lc_oc && '",' &&
             '    "documento_url": "' && lc_url && '" ' &&
             '  }' &&
             '}'.

  ENDMETHOD.


  METHOD zif_integracao_viagem_aprovar~set_ds_data.

    "Incluir Texto JSON para integração
    r_if_integracao_viagem_aprovar = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  ENDMETHOD.


  METHOD zif_integracao_viagem_aprovar~set_ds_url.

    r_if_integracao_viagem_aprovar = me.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_aprovar.

    IF sy-subrc EQ 0.

      SELECT SINGLE * INTO @DATA(lwa_zauth_webservice)
        FROM zauth_webservice
       WHERE service = 'CARGUERO_HOST'.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                              msgno = zcx_integracao=>zcx_servico_http_config-msgno
                              attr1 = 'O'
                              attr2 = '01' )
            msgid  = zcx_integracao=>zcx_servico_http_config-msgid
            msgno  = zcx_integracao=>zcx_servico_http_config-msgno
            msgty  = 'E'
            msgv1  = 'O'
            msgv2  = '01'.
      ENDIF.

      me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
      me->zif_integracao_inject~at_info_request_http-ds_content_type     = lwa_zauth_webservice-content_type.
      me->zif_integracao_inject~at_info_request_http-ds_url = lwa_zauth_webservice-url && 'viagens/' && me->zif_integracao_viagem_aprovar~at_viagem-viagem_id && '/aprovar'.
      me->zif_integracao_inject~at_info_request_http-ds_url_token = lwa_zauth_webservice-url_token.
      me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_viagem_aprovar=>at_fc_processar_viagem_aprovar.
      me->zif_integracao_inject~at_info_request_http-ds_metodo = 'PUT'.
      me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

      me->zif_integracao_inject~at_referencia-tp_referencia = 'CARGUERO_VIAGEM_APROVAR'.
      me->zif_integracao_inject~at_referencia-id_referencia = me->zif_integracao_viagem_aprovar~at_viagem-viagem_id.

      EXIT.

    ENDIF.


    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ 'O'
       AND servico EQ '01'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'O'
                            attr2 = '01' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'O'
          msgv2  = '01'.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && 'viagens/' && me->zif_integracao_viagem_aprovar~at_viagem-viagem_id && '/aprovar'.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_viagem_aprovar=>at_fc_processar_viagem_aprovar.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'PUT'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'CARGUERO_VIAGEM_APROVAR'.
    me->zif_integracao_inject~at_referencia-id_referencia = me->zif_integracao_viagem_aprovar~at_viagem-viagem_id.


  ENDMETHOD.


  METHOD zif_integracao_viagem_aprovar~set_id_referencia.

    "Incluir Chave de Referência
    r_if_integracao_viagem_aprovar = me.
    me->zif_integracao_viagem_aprovar~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_viagem_aprovar~set_new_viagem_aprovar.

    r_if_integracao_viagem_aprovar = me.

    SELECT SINGLE * INTO @me->zif_integracao_viagem_aprovar~at_viagem
      FROM zlest0185
     WHERE viagem_id EQ @i_viagem_id.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_viagem_aprovar~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_id_referencia(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao
      ).

    "FF #184986 - inicio
    "envio de e-mail se houver ID_ORDEM preenchido
    DATA(ls_viagem) = me->zif_integracao_viagem_aprovar~at_viagem.
    DATA: lt_string    TYPE TABLE OF swastrtab,
          lt_json_text TYPE soli_tab.

    IF ls_viagem-id_ordem IS NOT INITIAL.

      DATA(lv_matkl) = ls_viagem-matkl.

      " Se MATKL estiver vazio, buscar via VBAP
      IF lv_matkl IS INITIAL AND ls_viagem-vbeln IS NOT INITIAL.
        SELECT SINGLE matkl INTO @lv_matkl
          FROM vbap
          WHERE vbeln = @ls_viagem-vbeln.
      ENDIF.

      " Verifica se MATKL pertence ao STVARV 'MAGGI_GR_ALGODAO_PLUMA'
      SELECT SINGLE name INTO @DATA(lv_check)
        FROM tvarvc
        WHERE name = 'MAGGI_GR_ALGODAO_PLUMA'
          AND low  = @lv_matkl.

      IF sy-subrc = 0.

        " Buscar e-mails da ZMAIL
        SELECT email INTO TABLE @DATA(lt_emails)
          FROM zmail
          WHERE id_processo = 'OC_PLUMA'
            AND bukrs = @ls_viagem-bukrs
            AND ( werks = @ls_viagem-branch OR werks = '' ).

        IF lt_emails IS NOT INITIAL.

          " Buscar placa e número da ordem
          SELECT SINGLE nr_ordem, ds_placa_trator
            INTO @DATA(ls_ordem)
            FROM zsdt0001od
            WHERE id_ordem = @ls_viagem-id_ordem.

          " Buscar o JSON armazenado na integração
          SELECT SINGLE ds_body INTO @DATA(lv_json)
            FROM zintegracao
            WHERE id_referencia = @ls_viagem-viagem_id
              AND id_interface  = '003'.

          " Monta assunto
          DATA lv_subject TYPE so_obj_des.
          lv_subject = ls_ordem-ds_placa_trator && '/' && ls_ordem-nr_ordem && '/' && ls_viagem-vbeln && '/' && ls_viagem-viagem_id.

          " Monta corpo
          DATA lt_body TYPE soli_tab.
          lt_body = VALUE soli_tab( ( line = 'Iniciando com Criação de Monitoramento...' ) ).

          " Enviar e-mail via CL_BCS
          DATA(lo_bcs) = cl_bcs=>create_persistent( ).

          DATA lv_email_address TYPE adr6-smtp_addr.

          LOOP AT lt_emails INTO DATA(ls_email).
            lv_email_address = ls_email-email.

            DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( lv_email_address ).

            lo_bcs->add_recipient(
              EXPORTING
                i_recipient = lo_recipient
                i_express   = abap_true ).
          ENDLOOP.

          " Criar documento com corpo e assunto
          DATA(lo_document) = cl_document_bcs=>create_document(
            i_type    = 'RAW'
            i_text    = lt_body
            i_subject = lv_subject ).

          CALL FUNCTION 'SWA_STRING_SPLIT'
            EXPORTING
              input_string                 = lv_json
              max_component_length         = 255
            TABLES
              string_components            = lt_string
            EXCEPTIONS
              max_component_length_invalid = 1
              OTHERS                       = 2.
          IF sy-subrc = 0.

            LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<fs_string>).

              " Anexar JSON
              APPEND INITIAL LINE TO lt_json_text ASSIGNING FIELD-SYMBOL(<fs_json_text>).
              <fs_json_text> = <fs_string>-str.

            ENDLOOP.

            lo_document->add_attachment(
              i_attachment_type    = 'TXT'
              i_attachment_subject = 'Dados JSON da Viagem'
              i_att_content_text   = lt_json_text ).

          ENDIF.

          " Associar documento e enviar
          lo_bcs->set_document( lo_document ).
          lo_bcs->send( i_with_error_screen = abap_true ).

          COMMIT WORK.

        ENDIF.
      ENDIF.
    ENDIF.
    "FF #184986 - fim

  ENDMETHOD.


  METHOD zif_integracao_viagem_aprovar~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_viagem_aprovar = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

  ENDMETHOD.
ENDCLASS.
