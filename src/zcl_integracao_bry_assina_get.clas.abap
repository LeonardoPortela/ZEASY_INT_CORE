class ZCL_INTEGRACAO_BRY_ASSINA_GET definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_BRY_ASSINA_GET .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_BRY_ASSINA_GET IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_bry_assina_get~at_servico = 'BRY_INT_ASSINATURA_DIGITAL'.
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_bry_adigital.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    IF me->zif_integracao_bry_assina_get~at_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_bry_assina_get~at_auth_ws
          WHERE service = me->zif_integracao_bry_assina_get~at_servico .

    ENDIF.

    IF me->zif_integracao_bry_assina_get~at_auth_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_bry_assina_get~execute_service.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->zif_integracao_bry_assina_get~at_servico IS NOT INITIAL.

      IF me->zif_integracao_bry_assina_get~at_auth_ws IS INITIAL.

        SELECT SINGLE * FROM zauth_webservice
          INTO me->zif_integracao_bry_assina_get~at_auth_ws
            WHERE service = me->zif_integracao_bry_assina_get~at_servico.

      ENDIF.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    IF me->zif_integracao_bry_assina_get~at_auth_ws  IS INITIAL.
      lv_msgtx = `Serviço ` && me->zif_integracao_bry_assina_get~at_servico && ` não está configurado na ZAUTH_WEBSERVICE`.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).
    ENDIF.

    "me->zif_integracao_bry_assina_get~at_end_point = i_params.
    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    "me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_bry_assina_get~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_bry_assina_get~at_auth_ws-url && i_params.

    "REPLACE '#PARAMS#' IN me->zif_integracao_inject~at_info_request_http-ds_url WITH me->zif_integracao_bry_assina_get~at_end_point.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    "me->zif_integracao_bry_assina_get~gui_indicator_exe( ).

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = r_integracao
      )->free(
      ).

    FREE lc_integrar.

  ENDMETHOD.


  METHOD zif_integracao_bry_assina_get~get_docs_assinados.

    DATA lr_id TYPE RANGE OF zin_id_referencia.
    DATA lt_docs TYPE TABLE OF zins_docs_assina.
    DATA lv_params TYPE string.
    DATA lv_text TYPE string.

    DATA lv_etapa TYPE zin_etapa VALUE '02'.

    SELECT * FROM zint_assina01
      INTO TABLE @DATA(lt_assina)
        WHERE etapa = @lv_etapa "<-- documentos assinados e ja sinalizados pela BRy
          AND id_processo = @i_id_processo
          AND id_referencia IN @ir_id_ref_range.

    CHECK sy-subrc EQ 0.

    CLEAR r_docs_assinados.

    LOOP AT lt_assina ASSIGNING FIELD-SYMBOL(<fs_docs>).

      CLEAR lt_docs.

      lv_params = 'coletas/' && <fs_docs>-chave_coleta && '/documentos-assinados'.

      TRY.

          DATA(lw_retorno) = me->zif_integracao_bry_assina_get~execute_service( i_params = lv_params ).

          IF lw_retorno-nm_code NE 200.
            zcx_error=>zif_error~gera_erro_geral( i_texto = lw_retorno-ds_data_retorno ).
          ENDIF.

          /ui2/cl_json=>deserialize( EXPORTING json = lw_retorno-ds_data_retorno CHANGING data = lt_docs ).

          CHECK lt_docs IS NOT INITIAL.

          DATA(lw_rel_assi) = me->zif_integracao_bry_assina_get~get_rel_assinantes( <fs_docs> ).

          IF lw_rel_assi IS NOT INITIAL.
            APPEND lw_rel_assi TO lt_docs.
          ENDIF.

          LOOP AT lt_docs ASSIGNING FIELD-SYMBOL(<fs_doc_key>) WHERE xfile IS INITIAL.

*            lv_params = 'coletas/documento/assinado/' && <fs_doc_key>-chavedocumento.
             lv_params = 'documento/assinado/' && <fs_doc_key>-chavedocumento. "Alteração rota da api / MM - Ajuste api da sap/coupa/bry #172605 / AOENNING

            <fs_doc_key>-id_referencia = <fs_docs>-id_referencia.

            CONDENSE <fs_doc_key>-nome NO-GAPS.

            " TROCA DO NOME POR ASSINADO -->>

            REPLACE '.pdf' IN <fs_doc_key>-nome WITH '-ASSINADO.pdf'.

            DATA(lw_xdoc) = me->zif_integracao_bry_assina_get~execute_service( i_params = lv_params ).

            IF lw_xdoc-nm_code NE 200.
              zcx_error=>zif_error~gera_erro_geral( i_texto = lw_xdoc-ds_data_retorno ).
            ENDIF.

            "<fs_doc_key>-file = lw_xdoc-ds_data_retorno.
            <fs_doc_key>-xfile = lw_xdoc-ds_data_xstring.

          ENDLOOP.

          CHECK lt_docs IS NOT INITIAL.

          APPEND LINES OF lt_docs TO r_docs_assinados.

        CATCH zcx_integracao INTO DATA(ex_int).

          lv_text = ex_int->get_longtext( ).

          me->zif_integracao_bry_assina_get~set_message( lv_text ).

          CONTINUE. "<- se nao tiver anexo, vai para o proximo

        CATCH zcx_error INTO DATA(ex_erro).

          lv_text = ex_erro->get_longtext( ).

          me->zif_integracao_bry_assina_get~set_message( lv_text ).

          CONTINUE. "<- se nao tiver anexo, vai para o proximo

      ENDTRY.


    ENDLOOP.

    SORT r_docs_assinados BY id_referencia ASCENDING.

  ENDMETHOD.


  METHOD zif_integracao_bry_assina_get~get_docs_assinados_by_wf_id.


    DATA lr_id TYPE RANGE OF zin_id_referencia.
    DATA lt_docs TYPE TABLE OF zins_docs_assina.
    DATA lv_params TYPE string.
    DATA lv_text TYPE string.

    CLEAR r_docs_assinados.

    CLEAR lt_docs.

    lv_params = 'coletas/' && iv_workflow_bry && '/documentos-assinados'.

    TRY.

        DATA(lw_retorno) = me->zif_integracao_bry_assina_get~execute_service( i_params = lv_params ).

        IF lw_retorno-nm_code NE 200.
          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_retorno-ds_data_retorno ).
        ENDIF.

        /ui2/cl_json=>deserialize( EXPORTING json = lw_retorno-ds_data_retorno CHANGING data = lt_docs ).

        CHECK lt_docs IS NOT INITIAL.

        "DATA(lw_rel_assi) = me->zif_integracao_bry_assina_get~get_rel_assinantes( <fs_docs> ).

*        IF lw_rel_assi IS NOT INITIAL.
*          APPEND lw_rel_assi TO lt_docs.
*        ENDIF.

        LOOP AT lt_docs ASSIGNING FIELD-SYMBOL(<fs_doc_key>) WHERE xfile IS INITIAL.

          lv_params = 'coletas/documento/assinado/' && <fs_doc_key>-chavedocumento.

          "<fs_doc_key>-id_referencia = <fs_docs>-id_referencia.

          CONDENSE <fs_doc_key>-nome NO-GAPS.

          " TROCA DO NOME POR ASSINADO -->>

          "REPLACE '.pdf' IN <fs_doc_key>-nome WITH '-ASSINADO.pdf'.

          DATA(lw_xdoc) = me->zif_integracao_bry_assina_get~execute_service( i_params = lv_params ).

          IF lw_xdoc-nm_code NE 200.
            zcx_error=>zif_error~gera_erro_geral( i_texto = lw_xdoc-ds_data_retorno ).
          ENDIF.

          "<fs_doc_key>-file = lw_xdoc-ds_data_retorno.
          <fs_doc_key>-xfile = lw_xdoc-ds_data_xstring.

        ENDLOOP.

        CHECK lt_docs IS NOT INITIAL.

        APPEND LINES OF lt_docs TO r_docs_assinados.

      CATCH zcx_integracao INTO DATA(ex_int).

        lv_text = ex_int->get_longtext( ).

        me->zif_integracao_bry_assina_get~set_message( lv_text ).

        "CONTINUE. "<- se nao tiver anexo, vai para o proximo

      CATCH zcx_error INTO DATA(ex_erro).

        lv_text = ex_erro->get_longtext( ).

        me->zif_integracao_bry_assina_get~set_message( lv_text ).

        "CONTINUE. "<- se nao tiver anexo, vai para o proximo

    ENDTRY.

    SORT r_docs_assinados BY id_referencia ASCENDING.

  ENDMETHOD.


  METHOD zif_integracao_bry_assina_get~get_instance.

    IF zif_integracao_bry_assina_get~at_instance IS NOT BOUND.
      zif_integracao_bry_assina_get~at_instance = NEW zcl_integracao_bry_assina_get( ).
    ENDIF.

    r_object = zif_integracao_bry_assina_get~at_instance.


  ENDMETHOD.


  METHOD zif_integracao_bry_assina_get~get_messages.

    r_ret = me->zif_integracao_bry_assina_get~at_bapiret2_tab.

  ENDMETHOD.


  METHOD zif_integracao_bry_assina_get~get_rel_assinantes.

    DATA lv_text TYPE string.

    DATA lv_params TYPE string.

    CLEAR rs_rel_assinantes.

    CHECK i_doc_assinado-chave_coleta IS NOT INITIAL.

    lv_params = 'documentos/' && i_doc_assinado-chave_coleta && '/relatorio-assinaturas'.

    TRY.

        DATA(lw_xdoc) = me->zif_integracao_bry_assina_get~execute_service( i_params = lv_params ).

        IF lw_xdoc-nm_code NE 200.
          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_xdoc-ds_data_retorno ).
        ENDIF.

        rs_rel_assinantes-id_referencia = i_doc_assinado-id_referencia.
        rs_rel_assinantes-xfile = lw_xdoc-ds_data_xstring.

        rs_rel_assinantes-tamanho = xstrlen( rs_rel_assinantes-xfile ).
        rs_rel_assinantes-chavedocumento = i_doc_assinado-chave_coleta.
        rs_rel_assinantes-nome = 'relacao_assinantes_' && i_doc_assinado-id_referencia &&'_.pdf'.

      CATCH zcx_integracao INTO DATA(ex_int).

        lv_text = ex_int->get_longtext( ).

        me->zif_integracao_bry_assina_get~set_message( lv_text ).

      CATCH zcx_error INTO DATA(ex_erro).

        lv_text = ex_erro->get_longtext( ).

        me->zif_integracao_bry_assina_get~set_message( lv_text ).

    ENDTRY.

  ENDMETHOD.


  method ZIF_INTEGRACAO_BRY_ASSINA_GET~GET_URL_ENVIRONMENT.

    CASE sy-sysid.
      WHEN 'DEV' OR 'QAS'.
        RV_URL = 'https://cloud-hom.bry.com.br'.
      WHEN 'PRD'.
        RV_URL = 'https://cloud.bry.com.br'.
    ENDCASE.


  endmethod.


  METHOD zif_integracao_bry_assina_get~set_message.

    DATA(lv_msgty) = i_msgty.

    CHECK i_message IS NOT INITIAL.

    IF lv_msgty IS INITIAL.
      lv_msgty = 'E'.
    ENDIF.

    APPEND INITIAL LINE TO me->zif_integracao_bry_assina_get~at_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_message>).

    DATA: lt_trtexts     TYPE trtexts,
          lw_trtexts     TYPE trtext,
          lv_texto(4000).

    DATA lv_msg1 TYPE sy-msgv1.
    DATA lv_msg2 TYPE sy-msgv1.
    DATA lv_msg3 TYPE sy-msgv1.
    DATA lv_msg4 TYPE sy-msgv1.

    lv_texto = i_message.

    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text  = lv_texto
        iv_len   = 30
      IMPORTING
        et_lines = lt_trtexts.

    LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

      CASE sy-tabix.
        WHEN 1.
          <fs_message>-message_v1 = <fs_line>.
        WHEN 2.
          <fs_message>-message_v2 = <fs_line>.
        WHEN 3.
          <fs_message>-message_v3 = <fs_line>.
        WHEN 4.
          <fs_message>-message_v4 = <fs_line>.
      ENDCASE.

    ENDLOOP.

    <fs_message>-id = 'DS'.
    <fs_message>-type = lv_msgty.
    <fs_message>-number = '016'.

    MESSAGE ID <fs_message>-id TYPE <fs_message>-type NUMBER <fs_message>-number
      WITH <fs_message>-message_v1 <fs_message>-message_v2
           <fs_message>-message_v3 <fs_message>-message_v4 INTO <fs_message>-message.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    DATA LT_HEADER_FIELDS  TYPE ZDE_HEADER_FIELD_T.

    APPEND VALUE #( NAME = 't' VALUE = ME->ZIF_INTEGRACAO_BRY_ASSINA_GET~AT_AUTH_WS-ADD01 ) TO LT_HEADER_FIELDS.
    APPEND VALUE #( NAME = 'Authorization' VALUE = |Basic { ME->ZIF_INTEGRACAO_BRY_ASSINA_GET~AT_AUTH_WS-TOKEN }| ) TO LT_HEADER_FIELDS. "// BUG-174950 WBARBOSA 23/04/25

    ME->ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP( I_HEADER_FIELDS = LT_HEADER_FIELDS ).

    R_IF_INTEGRACAO_INJECT = ME.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

*    DATA: ordem_servico TYPE zde_kuhlmann_hvi_retorno.
*break abap.
*    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
*    "/UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ordem_servico-os_id IS NOT INITIAL.

*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    ZCL_INTEGRACAO_COF_KUHLMANN=>ZIF_INTEGRACAO_COF_KUHLMANN~GET_INSTANCE(
*      )->SET_HVI_CONFIRMAR( EXPORTING
*        "I_USUARIO = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_USUARIO
*        "I_SENHA   = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_SENHA
*        I_OS_ID   = ORDEM_SERVICO-OS_ID
*      ).
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
