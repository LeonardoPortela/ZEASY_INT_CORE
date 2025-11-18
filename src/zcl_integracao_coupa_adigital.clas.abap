class ZCL_INTEGRACAO_COUPA_ADIGITAL definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_ASSINATURA_DIGI .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.

  methods FORMAT_DATE_BRY
    importing
      !I_DATE type MSGTX
    returning
      value(R_DATE) type MSGTX .
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_ADIGITAL IMPLEMENTATION.


  METHOD constructor.
    "COUPA_INT_ASSINATURA_DIGITAL_STEP_01

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_adigital.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->zif_integracao_assinatura_digi~at_service = 'COUPA_INT_ASSINATURA_DIGITAL'.

    CLEAR me->zif_integracao_assinatura_digi~at_coleta_tab.
    CLEAR me->zif_integracao_assinatura_digi~at_assina_tab.

  ENDMETHOD.


  METHOD format_date_bry.

    DATA lv_datum TYPE sy-datum.
    DATA lv_string TYPE c LENGTH 20.
    DATA lv_inicial TYPE c.

    DATA(lv_date) = i_date.

    IF i_date IS NOT INITIAL.

      lv_string = i_date(10).

      REPLACE ALL OCCURRENCES OF '-' IN lv_string WITH ''.
      CONDENSE lv_string NO-GAPS.

      lv_datum = lv_string.

      IF lv_datum < sy-datum.
        lv_inicial = 'X'.
      ENDIF.

    ELSE.
      lv_inicial = 'X'.
    ENDIF.

    IF lv_inicial = 'X'.

      lv_date = '&1-&2-&3T&4:&5:&6-04:00'.

      REPLACE '&1' IN lv_date WITH sy-datum(4).
      REPLACE '&2' IN lv_date WITH sy-datum+4(2).
      REPLACE '&3' IN lv_date WITH sy-datum+6(2).
      REPLACE '&4' IN lv_date WITH sy-uzeit(2).
      REPLACE '&5' IN lv_date WITH sy-datum+2(2).
      REPLACE '&6' IN lv_date WITH sy-datum+4(2).

    ENDIF.

    REPLACE FIRST OCCURRENCE OF '-' IN lv_date WITH space.
    REPLACE FIRST OCCURRENCE OF '-' IN lv_date WITH space.
    REPLACE ALL OCCURRENCES OF ':' IN lv_date WITH space.

    CONDENSE lv_date NO-GAPS.

    r_date = lv_date.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~convert_node_xml_to_table.

    DATA(lv_index1) = i_index.
    DATA(lv_seq) = 0.

    DATA: lv_name  TYPE string,
          lo_child TYPE REF TO if_ixml_node.

    WHILE co_node IS NOT INITIAL. "Faz um loop enquanto existirem nós válidos

      DATA(lv_children) = co_node->num_children( ).

      IF  lv_children > 0.

        lo_child = co_node->get_first_child( ).

        IF lo_child->get_type( ) = lo_child->co_node_text. "Se o filho for um texto

          lv_name = co_node->get_name( ).
          DATA(lv_value) = co_node->get_value( ).
          DATA(lv_line) = co_node->get_line( ).
          DATA(lv_column) = co_node->get_column( ).
          "DATA(lv_column) = co_node->get

          APPEND INITIAL LINE TO c_coleta_tab ASSIGNING FIELD-SYMBOL(<fs_tab>).

          <fs_tab>-index = i_index.
          <fs_tab>-sequencia = i_seq.
          <fs_tab>-name = lv_name.
          <fs_tab>-value = lv_value.
          <fs_tab>-tab_flag = space.

        ELSEIF lo_child->get_type( ) = lo_child->co_node_element. "Se o filho for um nó

          APPEND INITIAL LINE TO c_coleta_tab ASSIGNING <fs_tab>.

          lv_name = co_node->get_name( ).
          DATA(lo_attributes) = co_node->get_attributes( ).
          DATA(lv_count) = lo_attributes->get_length( ).

          lv_line = co_node->get_line( ).
          lv_column = co_node->get_column( ).

          <fs_tab>-index = i_index.
          <fs_tab>-sequencia = i_seq.
          <fs_tab>-name = lv_name.
          <fs_tab>-tab_flag = 'X'.

          DO lv_count TIMES.
            DATA(lv_index) = sy-index - 1.
            DATA(lo_attribute) = lo_attributes->get_item( lv_index ).
            lv_name = lo_attribute->get_name( ).
            lv_value = lo_attribute->get_value( ).
            "WRITE: lv_name, '=', lv_value NO-GAP.
          ENDDO.

          " proxima sequencia
          lv_seq = i_seq + 1.

          CALL METHOD me->zif_integracao_assinatura_digi~convert_node_xml_to_table
            EXPORTING
              i_index      = lv_index1
              i_seq        = lv_seq
            CHANGING
              co_node      = lo_child
              c_coleta_tab = c_coleta_tab.

        ENDIF.

      ENDIF.

      co_node = co_node->get_next( ). "Obtem o próximo no

      IF co_node IS INITIAL.

        " vai para o proximo
        lv_index1 = i_index + 1.

      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~convert_xml_to_attachment.

    DATA lt_tab TYPE zins_dados_index_tab.

    DATA lv_name TYPE c LENGTH 30.
    DATA lv_field TYPE c LENGTH 30.

    CHECK i_xml_object IS BOUND.

    DATA(lo_attachments) = i_xml_object->get_first_node( )->get_first_child( ).

    CALL METHOD me->zif_integracao_assinatura_digi~convert_node_xml_to_table
      CHANGING
        co_node      = lo_attachments
        c_coleta_tab = lt_tab.

    LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<fs_xml>).

      IF <fs_xml>-index = 1 AND <fs_xml>-sequencia = 2 AND <fs_xml>-name = 'id'.
        APPEND INITIAL LINE TO r_ret ASSIGNING FIELD-SYMBOL(<fs_line>).
      ENDIF.

      lv_name = <fs_xml>-name.

      TRANSLATE lv_name TO UPPER CASE.

      REPLACE ALL OCCURRENCES OF '-' IN lv_name WITH '_'.

      lv_field = '<FS_LINE>-' && lv_name.

      ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_field>).

      CHECK sy-subrc EQ 0.

      IF <fs_xml>-tab_flag IS INITIAL.
        <fs_field> = <fs_xml>-value .
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~convert_xml_to_document.

    DATA lv_name TYPE c LENGTH 40.
    DATA lv_field TYPE c LENGTH 40.
    DATA lv_flag_approv TYPE c.

    CHECK i_xml_object IS BOUND.

    DATA(lo_contracts) = i_xml_object->get_first_node( )->get_first_child( ).

    CALL METHOD me->zif_integracao_assinatura_digi~convert_node_xml_to_table
      CHANGING
        co_node      = lo_contracts
        c_coleta_tab = me->zif_integracao_assinatura_digi~at_coleta_tab.

    LOOP AT me->zif_integracao_assinatura_digi~at_coleta_tab ASSIGNING FIELD-SYMBOL(<fs_xml>).

      IF <fs_xml>-index = 1 AND <fs_xml>-sequencia = 2 AND <fs_xml>-name = 'id'.
        APPEND INITIAL LINE TO r_ret ASSIGNING FIELD-SYMBOL(<fs_line>).
      ENDIF.

      lv_name = <fs_xml>-name.

      TRANSLATE lv_name TO UPPER CASE.

      REPLACE ALL OCCURRENCES OF '-' IN lv_name WITH '_'.

      lv_field = '<FS_LINE>-' && lv_name.

      IF lv_name = 'CURRENT_APPROVAL'.
        lv_flag_approv = 'X'.
      ENDIF.

      ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_field>).

      CHECK sy-subrc EQ 0.

      IF <fs_xml>-tab_flag IS INITIAL.

        IF lv_name = 'ID' AND lv_flag_approv = 'X'.
          CLEAR  lv_flag_approv.
          <fs_line>-current_approval_id = <fs_xml>-value.
        ENDIF.

*        IF lv_name = 'ID' AND <fs_xml>-sequencia NE 2.
*          CONTINUE.
*        ENDIF.

        CHECK <fs_field> IS INITIAL.

        <fs_field> = <fs_xml>-value.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~getx_from_service.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    DATA lv_msgtx TYPE string.

    IF me->zif_integracao_assinatura_digi~at_service IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_assinatura_digi~at_auth_ws
          WHERE service = me->zif_integracao_assinatura_digi~at_service.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

*    SELECT SINGLE * FROM zauth_webservice
*      INTO me->zif_integracao_assinatura_digi~at_token_ws
*        WHERE service = 'COUPA_TOKEN'.

    IF me->zif_integracao_assinatura_digi~at_auth_ws  IS INITIAL.
      "OR me->zif_integracao_assinatura_digi~at_token_ws IS INITIAL.


      lv_msgtx = `Serviço ` && me->zif_integracao_assinatura_digi~at_service && ` não está configurado na ZAUTH_WEBSERVICE`.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    me->zif_integracao_assinatura_digi~at_fc_end_point = i_params.
    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_assinatura_digi~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_assinatura_digi~at_auth_ws-url.

    REPLACE '#PARAMS#' IN me->zif_integracao_inject~at_info_request_http-ds_url WITH me->zif_integracao_assinatura_digi~at_fc_end_point.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_assinatura_digi~gui_indicator_exe( ).

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    FREE lc_integrar.

    IF ( e_integracao-nm_code NE 200 AND e_integracao-nm_code NE 201 AND e_integracao-nm_code NE 202 ).
      zcx_error=>zif_error~gera_erro_geral( i_texto = e_integracao-ds_data_retorno ).
    ENDIF.

    r_xstring = e_integracao-ds_data_xstring.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~get_body_workflow_key.

    r_ret = '<?xml version="1.0" encoding="UTF-8"?>' &&
            '<contract>' &&
                '<custom-fields>' &&
                    '<chave-coleta-assinatura>' &&  i_chave   &&'</chave-coleta-assinatura>' &&
                '</custom-fields>' &&
            '</contract>'.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~get_documents.

    DATA lo_child TYPE REF TO if_ixml_node.
    DATA lo_xml_ret TYPE REF TO cl_xml_document.
    DATA lv_params TYPE string.

    DATA lt_adigital TYPE TABLE OF zint_assina01.

    lv_params = '?status=pending_approval&[custom_fields]' &&
                '[chave_coleta_assinatura][blank]=true&fields=["id","created-at",' &&
                '"updated-at","name","number","type","version","start-date","end-date",' &&
                '"status",{"current_approval":["approver-id"]},{"custom_fields":' &&
                '["chave-coleta-assinatura","data-limite-assinaturas","relacao-assinantes",' &&
                '"exigir-download","proibir-rejeio","assinatura-sequencial","agrupar-documentos",' &&
                '"pagina-assinaturas","relatrio-assinaturas","protocolo-assinaturas"]}]'.

    "    lv_params = 'contracts/?status=pending_approval'.

    lo_xml_ret = me->zif_integracao_assinatura_digi~get_from_service( i_params = lv_params  ).

    me->zif_integracao_assinatura_digi~at_documentos = me->zif_integracao_assinatura_digi~convert_xml_to_document( lo_xml_ret ).

    IF me->zif_integracao_assinatura_digi~at_documentos IS NOT INITIAL.

      DATA lr_ref TYPE RANGE OF zin_id_referencia.

      LOOP AT me->zif_integracao_assinatura_digi~at_documentos ASSIGNING FIELD-SYMBOL(<fs_docs>).
        APPEND 'IEQ' && <fs_docs>-id TO lr_ref.
      ENDLOOP.

      SELECT * FROM zint_assina01
        INTO TABLE lt_adigital
            WHERE id_referencia IN lr_ref.

      IF sy-subrc EQ 0.

        LOOP AT lt_adigital ASSIGNING FIELD-SYMBOL(<fs_digital>).
          DELETE me->zif_integracao_assinatura_digi~at_documentos WHERE id = <fs_digital>-id_referencia.
        ENDLOOP.

      ENDIF.

    ENDIF.

    r_ret = me.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~get_from_service.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->zif_integracao_assinatura_digi~at_service IS NOT INITIAL.

      IF me->zif_integracao_assinatura_digi~at_auth_ws IS INITIAL.

        SELECT SINGLE * FROM zauth_webservice
          INTO me->zif_integracao_assinatura_digi~at_auth_ws
            WHERE service = me->zif_integracao_assinatura_digi~at_service.

      ENDIF.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

*    IF me->zif_integracao_assinatura_digi~at_token_ws IS INITIAL.
*
*      SELECT SINGLE * FROM zauth_webservice
*        INTO me->zif_integracao_assinatura_digi~at_token_ws
*          WHERE service = 'COUPA_TOKEN'.
*
*    ENDIF.

    IF me->zif_integracao_assinatura_digi~at_auth_ws  IS INITIAL.
       "OR me->zif_integracao_assinatura_digi~at_token_ws IS INITIAL.


      lv_msgtx = `Serviço ` && me->zif_integracao_assinatura_digi~at_service && ` não está configurado na ZAUTH_WEBSERVICE`.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    me->zif_integracao_assinatura_digi~at_fc_end_point = i_params.
    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_assinatura_digi~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_assinatura_digi~at_auth_ws-url.

    REPLACE '#PARAMS#' IN me->zif_integracao_inject~at_info_request_http-ds_url WITH me->zif_integracao_assinatura_digi~at_fc_end_point.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_assinatura_digi~gui_indicator_exe( ).

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    FREE lc_integrar.

    IF ( e_integracao-nm_code NE 200 AND e_integracao-nm_code NE 201 AND e_integracao-nm_code NE 202 ).
      zcx_error=>zif_error~gera_erro_geral( i_texto = e_integracao-ds_data_retorno ).
    ENDIF.


*    """ teste
*    DATA lt_teste TYPE zins_adigital_contratos.
*    DATA t_element_array  TYPE zde_element_array_t.
*
*    APPEND 'contract' TO t_element_array.
*
*    DATA(lv_json) = zcl_string=>xml_to_json( i_xml = e_integracao-ds_data_retorno i_element_array =  t_element_array ).
*
*    DATA(lv_json2) = me->zif_integracao_assinatura_digi~json_replace_hifen( lv_json ).
**
*
*    /ui2/cl_json=>deserialize( EXPORTING json = lv_json2 CHANGING data = lt_teste ).
*
*
*    """ teste

    r_xml = me->zif_integracao_assinatura_digi~string_to_xml_object( e_integracao-ds_data_retorno ).

    DATA(lv_error) = r_xml->find_simple_element( 'error' ).

    IF lv_error IS NOT INITIAL.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_error ).
    ENDIF.


  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~get_instance.

    IF zif_integracao_assinatura_digi~at_if_int_assinatura_digi IS NOT BOUND.
      zif_integracao_assinatura_digi~at_if_int_assinatura_digi = NEW zcl_integracao_coupa_adigital( ).
    ENDIF.

    r_if_integracao_assinatura_dig = zif_integracao_assinatura_digi~at_if_int_assinatura_digi.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~gui_indicator_exe.

    DATA lv_len TYPE i.

    DATA lv_text TYPE string.

    DATA lw_match TYPE match_result.

    CHECK me->zif_integracao_inject~at_info_request_http-ds_url IS NOT INITIAL.

    FIND '.com' IN me->zif_integracao_inject~at_info_request_http-ds_url RESULTS lw_match.

    lv_len = strlen( me->zif_integracao_inject~at_info_request_http-ds_url ) - lw_match-offset.
    DATA(lv_value) = substring( val = me->zif_integracao_inject~at_info_request_http-ds_url off = lw_match-offset len = lv_len ).

    REPLACE FIRST OCCURRENCE OF '/' IN lv_value WITH space.
    REPLACE FIRST OCCURRENCE OF '.com' IN lv_value WITH space.

    CONDENSE lv_value NO-GAPS.

    FIND '?' IN lv_value RESULTS lw_match.

    IF sy-subrc EQ 0.

      lv_value = substring( val = lv_value off = 0 len = lw_match-offset ).

    ENDIF.

    CHECK lv_value IS NOT INITIAL.

    lv_text = `Executando ` && lv_value.

    IF sy-batch IS INITIAL.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = lv_text.

    ELSE.

      me->zif_integracao_assinatura_digi~log_generate( i_msgty = 'S' i_text = lv_text ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~json_replace_hifen.

    DATA lv_string TYPE string.
    DATA lv_char TYPE c.

    DATA lv_ant TYPE c.
    DATA lv_pos TYPE c.

    DATA lv_cnt TYPE i VALUE -1.
    DATA(lv_len) = strlen( i_value ).

    DO lv_len TIMES.

      ADD 1 TO lv_cnt.

      IF sy-index > 2.

        IF i_value+lv_cnt(1) = '-'.

          DATA(lv_anterior) = lv_cnt - 1.
          DATA(lv_posterior) = lv_cnt + 1.

          lv_ant = i_value+lv_anterior(1).
          lv_pos = i_value+lv_posterior(1).

          TRANSLATE lv_ant TO UPPER CASE.
          TRANSLATE lv_pos TO UPPER CASE.

          IF lv_ant CA sy-abcde AND lv_pos CA sy-abcde.
            "lv_char =  '_'.
            lv_char =  '$'.
          ELSE.
            lv_char = i_value+lv_cnt(1).
          ENDIF.

        ELSE.
          lv_char = i_value+lv_cnt(1).

        ENDIF.

      ELSE.

        lv_char = i_value+lv_cnt(1).

      ENDIF.

      IF lv_char IS NOT INITIAL and lv_char NE  '$'.
        lv_string = lv_string && lv_char.
      ELSE.

        IF lv_char =  '$'.
          lv_string = lv_string.
        ELSE.
          lv_string = lv_string && ` `.
        ENDIF.

      ENDIF.

    ENDDO.

    r_value = lv_string.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~log_generate.

    DATA: lt_trtexts     TYPE trtexts,
          lw_trtexts     TYPE trtext,
          lv_texto(4000).

    DATA lv_msg1 TYPE sy-msgv1.
    DATA lv_msg2 TYPE sy-msgv1.
    DATA lv_msg3 TYPE sy-msgv1.
    DATA lv_msg4 TYPE sy-msgv1.

    lv_texto = i_text.

    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text  = lv_texto
        iv_len   = 30
      IMPORTING
        et_lines = lt_trtexts.

    LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

      CASE sy-tabix.
        WHEN 1.
          lv_msg1 = <fs_line>.
        WHEN 2.
          lv_msg2 = <fs_line>.
        WHEN 3.
          lv_msg3 = <fs_line>.
        WHEN 4.
          lv_msg4 = <fs_line>.
      ENDCASE.

    ENDLOOP.

    MESSAGE ID 'DS' TYPE 'S' NUMBER '016'
      WITH lv_msg1 lv_msg2 lv_msg3
        lv_msg4 DISPLAY LIKE i_msgty.

  ENDMETHOD.


    METHOD zif_integracao_assinatura_digi~remove_spec_char.

      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = i_value
        IMPORTING
          outtext           = r_value
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.

      IF sy-subrc <> 0.
        r_value = i_value.
      ENDIF.

    ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~send_approve.

    DATA lo_xml_ret TYPE REF TO cl_xml_document.

    DATA lv_params TYPE string.

    CHECK i_docid IS NOT INITIAL.

    TRY.

        lv_params = 'approvals/' && i_docid && '/approve?reason=Aprovado&fields=["id","status",' &&
                    '"approval-date","approver-type","approver-id","approvable-type","approvable-id"]'.

        lo_xml_ret = me->zif_integracao_assinatura_digi~send_to_service( i_params = lv_params i_method = 'PUT').

      CATCH zcx_integracao INTO DATA(ex_int).
        zcx_error=>zif_error~gera_erro_geral( i_texto = ex_int->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro).
        zcx_error=>zif_error~gera_erro_geral( i_texto = ex_int->get_longtext( ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~send_attachment_to_bry.

*    DATA lo_envio TYPE REF TO zcl_integracao_bry_adigital.
*    DATA lw_envio TYPE zins_dados_bry_dados.
*
*    DATA lw_sucesso TYPE zins_dados_bry_json.
*    DATA lw_erro TYPE zins_dados_bry_json_erro.
*
*    DATA lv_response TYPE string.
*
*    lw_envio-nomecoleta = i_document-name.
*
*    IF i_document-data_limite_assinaturas IS NOT INITIAL.
*      lw_envio-datalimite = format_date_bry( i_document-data_limite_assinaturas ).
*    ELSEIF i_document-end_date IS NOT INITIAL.
*      lw_envio-datalimite = format_date_bry( i_document-end_date ).
*    ELSE.
*      lw_envio-datalimite = format_date_bry( i_document-end_date ). "< se for inicial a função coloca data atual
*    ENDIF.
*
*    lw_envio-descricao = i_document-name.
*    lw_envio-padraoassinatura = 'PDF'.
*    lw_envio-exigirdownload = i_document-exigir_download.
*    lw_envio-proibirrejeicao = i_document-proibir_rejeio.
*    lw_envio-assinaturasequencial = i_document-assinatura_sequencial.
*    lw_envio-agrupardocumentos = i_document-agrupar_documentos.
*    lw_envio-local_assinatura = 'AUTOMATICA'.
*    lw_envio-relacao_assinantes = i_document-relacao_assinantes.
*
*    IF lw_envio-exigirdownload IS INITIAL.
*      lw_envio-exigirdownload = 'false'.
*    ENDIF.
*
*    IF lw_envio-proibirrejeicao IS INITIAL.
*      lw_envio-proibirrejeicao = 'false'.
*    ENDIF.
*    IF lw_envio-assinaturasequencial IS INITIAL.
*      lw_envio-assinaturasequencial = 'false'.
*    ENDIF.
*
*    IF lw_envio-agrupardocumentos IS INITIAL.
*      lw_envio-agrupardocumentos = 'false'.
*    ENDIF.
*
*    IF lw_envio-relacao_assinantes IS INITIAL.
*
*      zcx_error=>zif_error~gera_erro_geral( i_texto = 'Documento sem assinantes' ).
*
*    ENDIF.

*    CREATE OBJECT lo_envio
*      EXPORTING
*        it_anexos     = it_anexos
*        i_dados_envio = lw_envio.
*
*    lv_response = lo_envio->zif_integracao_bry_adigital~enviar_bry( ).
*
**    ELSE.
**
**      " -codigo provisorio ---------------->
**      lv_response = '{"sucesso":true,"chaveWorkflow":"8C3A2BA19F662D229ECF218826741761B622EB2C9C7B390D82594D9D3B0FB963",' &&
**                    '"chaveI18n":"servico.cadastro.sucesso","mensagem":"Coleta \u0027teste coleta\u0027 cadastrada com sucesso."}'.
**      " -codigo provisorio ---------------->
**
**    ENDIF.
*
*    IF lv_response CS 'status'.
*
*      /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*            CHANGING data = lw_erro ).
*
*      "#MUDAR - RAISE EXCP
*    ELSE.
*
*      /ui2/cl_json=>deserialize( EXPORTING json = lv_response
*          CHANGING data = lw_sucesso ).
*
*    ENDIF.
*
*    CHECK lw_sucesso IS NOT INITIAL.
*
*    r_adigital-id_referencia = i_document-id.
*    r_adigital-id_processo = '01'. "<- COUPA
*    r_adigital-etapa = '01'. "<- ENVIADO AO BRY
*    r_adigital-chave_coleta = lw_sucesso-chaveworkflow.
*    r_adigital-log_date = sy-datum.
*    r_adigital-log_uzeit = sy-uzeit.
*    r_adigital-log_name = sy-uname.
*    r_adigital-current_approval_id = i_document-current_approval_id.
*
*    MODIFY zint_assina01 FROM r_adigital.
*
*    TRY.
*
*        me->zif_integracao_assinatura_digi~send_workflow_key( r_adigital ).
*
*      CATCH zcx_integracao INTO DATA(ex_int).
*
*        me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = ex_int->get_longtext( ) ).
*
*      CATCH zcx_error INTO DATA(ex_erro).
*
*        me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = ex_erro->get_longtext( ) ).
*
*    ENDTRY.
*
*    TRY.
*        me->zif_integracao_assinatura_digi~send_approve( r_adigital-current_approval_id ).
*
*      CATCH zcx_integracao INTO ex_int.
*
*        me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = ex_int->get_longtext( ) ).
*
*      CATCH zcx_error INTO ex_erro.
*
*        me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = ex_erro->get_longtext( ) ).
*
*    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~send_document_to_approval.

*    DATA lv_text TYPE string.
*    DATA lt_tab_xml TYPE zins_dados_index_tab.
*
*    DATA lv_params TYPE string.
*    DATA lv_anexo_param TYPE string.
*
*    LOOP AT me->zif_integracao_assinatura_digi~at_documentos ASSIGNING FIELD-SYMBOL(<fs_documento>).
*
*      CLEAR lt_tab_xml.
*
*      lv_params = 'contracts/' && <fs_documento>-id && '/attachments'.
*
*      IF <fs_documento>-relacao_assinantes IS INITIAL.
*
*        lv_text = `Documento: ` && <fs_documento>-id && ` não tem assinantes`.
*
*        me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = lv_text ).
*
*        CONTINUE. "<- se nao tiver anexo, vai para o proximo
*
*      ENDIF.
*
*
*      TRY.
*          DATA(lo_xml_ret) = me->zif_integracao_assinatura_digi~get_from_service( i_params = lv_params  ).
*
*        CATCH zcx_integracao INTO DATA(ex_int).
*
*          lv_text = ex_int->get_longtext( ).
*
*          me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = lv_text ).
*
*          CONTINUE. "<- se nao tiver anexo, vai para o proximo
*
*        CATCH zcx_error INTO DATA(ex_erro).
*
*          lv_text = ex_erro->get_longtext( ).
*
*          me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = lv_text ).
*
*          CONTINUE. "<- se nao tiver anexo, vai para o proximo
*
*      ENDTRY.
*
*      CHECK lo_xml_ret IS BOUND.
*
*      DATA(lt_anexos) = me->zif_integracao_assinatura_digi~convert_xml_to_attachment( lo_xml_ret ).
*
*      LOOP AT lt_anexos ASSIGNING FIELD-SYMBOL(<fs_anexo>).
*
*        <fs_anexo>-file = me->zif_integracao_assinatura_digi~remove_spec_char( <fs_anexo>-file ).
*        <fs_anexo>-file_url = me->zif_integracao_assinatura_digi~remove_spec_char( <fs_anexo>-file_url ).
*
*        CALL METHOD me->zif_integracao_assinatura_digi~set_attachment_prop
*          EXPORTING
*            i_file_path        = <fs_anexo>-file_url
*          IMPORTING
*            e_application_type = <fs_anexo>-application_type
*            e_file_name        = <fs_anexo>-file_name.
*
*        CHECK <fs_anexo>-id IS NOT INITIAL AND <fs_anexo>-xfile IS INITIAL.
*
*        lv_anexo_param = 'contracts/' && <fs_documento>-id  && '/attachments/' && <fs_anexo>-id.
*
*        <fs_anexo>-xfile = me->zif_integracao_assinatura_digi~getx_from_service( i_params = lv_anexo_param  ).
*
*      ENDLOOP.
*
*      TRY.
*
*          me->zif_integracao_assinatura_digi~send_attachment_to_bry( EXPORTING i_document = <fs_documento> it_anexos = lt_anexos ).
*
*        CATCH zcx_integracao INTO ex_int.
*
*          lv_text = ex_int->get_longtext( ).
*
*          me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = lv_text ).
*
*          CONTINUE. "<- se nao conseguiu enviar vai para o proximo
*
*        CATCH zcx_error INTO ex_erro.
*
*          lv_text = ex_erro->get_longtext( ).
*
*          me->zif_integracao_assinatura_digi~log_generate( EXPORTING i_msgty = 'E' i_text = lv_text ).
*
*          CONTINUE. "<- se nao conseguiu enviar vai para o proximo
*
*      ENDTRY.
*    ENDLOOP.
*
*    r_ret = me.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~send_to_service.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->zif_integracao_assinatura_digi~at_service IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_assinatura_digi~at_auth_ws
          WHERE service = me->zif_integracao_assinatura_digi~at_service.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

*    SELECT SINGLE * FROM zauth_webservice
*      INTO me->zif_integracao_assinatura_digi~at_token_ws
*        WHERE service = 'COUPA_TOKEN'.

    IF me->zif_integracao_assinatura_digi~at_auth_ws IS INITIAL.
       "  OR me->zif_integracao_assinatura_digi~at_token_ws IS INITIAL.


      lv_msgtx = `Serviço ` && me->zif_integracao_assinatura_digi~at_service && ` não está configurado na ZAUTH_WEBSERVICE`.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    IF i_body IS NOT INITIAL.

      me->zif_integracao_inject~at_info_request_http-ds_body = i_body.

    ENDIF.

    me->zif_integracao_assinatura_digi~at_fc_end_point = i_params.
    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_assinatura_digi~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_assinatura_digi~at_auth_ws-url.

    REPLACE '#PARAMS#' IN me->zif_integracao_inject~at_info_request_http-ds_url WITH me->zif_integracao_assinatura_digi~at_fc_end_point.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    FREE lc_integrar.

    r_xml = me->zif_integracao_assinatura_digi~string_to_xml_object( e_integracao-ds_data_retorno ).

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~send_workflow_key.

    DATA lo_xml_ret TYPE REF TO cl_xml_document.

    DATA lv_params TYPE string.

    lv_params = 'contracts/' && i_document-id_referencia  &&'/?fields=["id",{"custom_fields":["chave-coleta-assinatura"]}]'.

    DATA(lv_body) = me->zif_integracao_assinatura_digi~get_body_workflow_key( i_document-chave_coleta ).

    lo_xml_ret = me->zif_integracao_assinatura_digi~send_to_service( i_params = lv_params  i_body = lv_body ).

    DATA(lv_node) = lo_xml_ret->find_simple_element('id').

    IF lv_node IS INITIAL.
      zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro ao atualizar status do documento no coupa').
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~set_attachment_prop.

    DATA lv_count TYPE i.
    DATA lv_appli TYPE c LENGTH 5.
    DATA lv_reverse TYPE msgxx.
    DATA lw_match TYPE match_result.

    CALL FUNCTION 'STRING_REVERSE'
      EXPORTING
        string  = i_file_path
        lang    = sy-langu
      IMPORTING
        rstring = lv_reverse.

    FIND FIRST OCCURRENCE OF '/' IN lv_reverse RESULTS lw_match.

    CHECK lw_match-offset IS NOT INITIAL.

    "SUBTRACT 1 FROM lw_match-offset.

    DO lw_match-offset TIMES.
      e_file_name = e_file_name && lv_reverse+lv_count(1).

      IF lv_appli NA '.'.
        lv_appli = lv_appli && lv_reverse+lv_count(1).
      ENDIF.

      ADD 1 TO lv_count.

    ENDDO.

    REPLACE '.' IN lv_appli WITH space.

    CALL FUNCTION 'STRING_REVERSE'
      EXPORTING
        string  = e_file_name
        lang    = sy-langu
      IMPORTING
        rstring = e_file_name.

    CALL FUNCTION 'STRING_REVERSE'
      EXPORTING
        string  = lv_appli
        lang    = sy-langu
      IMPORTING
        rstring = e_application_type.

    CASE e_application_type.
      WHEN 'pdf'.
        e_application_type = 'application/pdf'.
      WHEN 'csv'.
        e_application_type = 'text/csv'.
      WHEN 'doc'.
        e_application_type = 'application/msword'.
      WHEN 'docx'.
        e_application_type = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~set_ds_url.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_assinatura_digi~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_assinatura_digi~at_auth_ws-url && me->zif_integracao_assinatura_digi~at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_integracao_assinatura_digi~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    "me->zif_integracao_assinatura_digi~set_id_referencia( ).

    r_if_integracao_assinatura_dig = me.





  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~set_endpoint.

    me->zif_integracao_assinatura_digi~at_fc_end_point = i_endpoint.

  ENDMETHOD.


  METHOD zif_integracao_assinatura_digi~string_to_xml_object.

    DATA lv_new_ret TYPE string.
    DATA lv_char TYPE c.
    DATA lv_ant TYPE c.

    DATA(lv_count) = 0.

    CREATE OBJECT r_xml.

    DATA(lv_ret) = i_string_xml.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_ret WITH space.

    DATA(lv_len) = strlen( lv_ret ).

    DO lv_len TIMES.

      lv_char = lv_ret+lv_count(1).

      IF lv_char = space AND ( lv_ant = '>' OR lv_ant = space ).

        WHILE lv_char = space.

          ADD 1 TO lv_count.

          lv_char = lv_ret+lv_count(1).

        ENDWHILE.

        lv_new_ret = lv_new_ret && lv_char.

        ADD 1 TO lv_count.

      ELSE.

        IF lv_char = space.
          lv_new_ret = lv_new_ret && ` `.
        ELSE.
          lv_new_ret = lv_new_ret && lv_char.
        ENDIF.

        ADD 1 TO lv_count.

      ENDIF.

      IF lv_count >= lv_len.
        EXIT.
      ENDIF.

      lv_ant = lv_char.

    ENDDO.

    IF r_xml->parse_string( lv_new_ret ) NE 0.

      "lv_msgtx = 'Erro ao recuperar informações via GET, verificar request ao sistema: COUPA'.
      "zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_true.
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


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
