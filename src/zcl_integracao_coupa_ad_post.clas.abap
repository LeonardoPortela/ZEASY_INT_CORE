class ZCL_INTEGRACAO_COUPA_AD_POST definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_ADIGITAL_POST .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  methods APPROVE_ID_EXECUTE
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods UPDATE_WORKFLOW_KEY
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods UPDATE_STATUS
    importing
      !I_DOCUMENT type ZINS_DOCS_ASSINA
      !I_NEW_STATUS type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods EXECUTE_APPROVALS
    importing
      !I_REFERENCIA type ZIN_ID_REFERENCIA
    returning
      value(R_APPROVAL_ID) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SEND_DOC_ASSINADO
    importing
      value(I_DOC_ASSINADO) type ZINS_DOCS_ASSINA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.

  methods PROCESS_SIGNED_CONTRACT
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE optional
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods PROCESS_REFUSED_CONTRACT
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE optional .
  methods LOG_CONTRACT_ADD
    importing
      !I_REFERENCIA type ZIN_ID_REFERENCIA .
  methods GET_CONTRACT_URL
    importing
      !ID_CONTRATO type ZIN_ID_REFERENCIA
    returning
      value(R_URL) type STRING .
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_AD_POST IMPLEMENTATION.


  METHOD approve_id_execute.

    DATA lv_params TYPE string.
    DATA lv_erro TYPE c.
    DATA lv_message TYPE string.

    DATA lo_xml TYPE REF TO cl_xml_document.

    DO.

      IF lv_erro = 'X'.
        EXIT.
      ENDIF.

      "lv_params = 'contracts/' && i_document-id_referencia && '?&fields=[{"current_approval":["approver-id"]}]'.
      lv_params = 'contracts/' && i_document-id_referencia && '?&fields=[{"current_approval":["id"]}]'.

      lv_erro = 'X'. "< - se passar pelos checks, vai tirar o 'X'.

      DATA(lw_int) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_method = 'GET').

      CHECK lw_int-nm_code = 200 OR lw_int-nm_code = 201 OR lw_int-nm_code = 202.

      CREATE OBJECT lo_xml.

      CHECK lo_xml->parse_xstring( lw_int-ds_data_xstring ) = 0.

      "DATA(lv_approval_id) = lo_xml->find_simple_element( 'approver-id' ).
      DATA(lv_approval_id) = lo_xml->find_simple_element( 'id' ).

      CHECK lv_approval_id IS NOT INITIAL.

      lv_params = 'approvals/' && lv_approval_id && '/approve?reason=Aprovado'.

      lw_int = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_method = 'PUT').

      CHECK lw_int-nm_code = 200 OR lw_int-nm_code = 201 OR lw_int-nm_code = 202.

      lv_erro = ''.

    ENDDO.

  ENDMETHOD.


  METHOD constructor.

    me->zif_integracao_adigital_post~at_servico = 'COUPA_INT_ASSINATURA_DIGITAL'.
    me->zif_integracao_inject~at_id_interface    =  zif_integracao=>at_id_interface_coupa_adigital.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    IF me->zif_integracao_adigital_post~at_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_adigital_post~at_auth_ws
          WHERE service = me->zif_integracao_adigital_post~at_servico .

    ENDIF.

    IF me->zif_integracao_adigital_post~at_auth_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.



  ENDMETHOD.


  METHOD execute_approvals.

    DATA lv_params TYPE string.
    "DATA lv_message TYPE string.

    DATA lo_xml TYPE REF TO cl_xml_document.

    lv_params = 'contracts/' && i_referencia && '?&fields=[{"current_approval":["id"]}]'.

    DATA(lw_int) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_method = 'GET').

    CHECK lw_int-nm_code = 200 OR lw_int-nm_code = 201 OR lw_int-nm_code = 202.

    CREATE OBJECT lo_xml.

    CHECK lo_xml->parse_xstring( lw_int-ds_data_xstring ) = 0.

    r_approval_id = lo_xml->find_simple_element( 'id' ).

  ENDMETHOD.


  METHOD get_contract_url.

    DATA(lv_rev) = me->zif_integracao_adigital_post~at_auth_ws-url.

    REPLACE 'api/' IN lv_rev WITH space.

    r_url = lv_rev && 'contracts/show/' && id_contrato && '#summary'.

  ENDMETHOD.


  METHOD log_contract_add.

    DATA lv_params TYPE string.
    DATA lv_url_cont TYPE string.
    DATA lv_template TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.

    lv_params = 'requisitions/' && me->zif_integracao_adigital_post~at_auth_ws-add01 &&'/comments'.

    lv_template = `<comment><comments>@CLMS - Jurídico - Erro de integração na assinatura do contrato: ` &&
                  `#ID_CONTRATO Resultado: Assinatura recusada no sistema de destino; Favor verificar. #URL_CONTRATO</comments></comment>`.

    TRY.

        "LOOP AT i_log_tab ASSIGNING FIELD-SYMBOL(<fs_log>).

        lv_body = lv_template.

        lv_url_cont = get_contract_url( i_referencia ).

*          READ TABLE me->zif_integracao_adigital_get~at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_contract>)
*            WITH KEY id = <fs_log>-id_referencia.
*
*          CHECK <fs_contract> IS ASSIGNED.

        "REPLACE '#DESCRI_CONTRATO' IN lv_body WITH <fs_contract>-name.
        REPLACE ALL OCCURRENCES OF '#ID_CONTRATO' IN lv_body WITH i_referencia.

        REPLACE '#MSG_ERRO' IN lv_body WITH 'Assinatura foi recusada no sistema de destino'.
        REPLACE '#URL_CONTRATO' IN lv_body WITH lv_url_cont.

        DATA(lw_return) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_body = lv_body i_method = 'POST' ).

        IF lw_return-nm_code > 206.

          lv_text = `Inserção de comentarios para: ` && i_referencia && 'falhou'.

          zcx_error=>zif_error~gera_erro_geral( i_texto =  lv_text ).

        ENDIF.

        "ENDLOOP.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_adigital_post~set_message( ex_int->get_longtext( ) ).
        EXIT.

      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_adigital_post~set_message( ex_erro->get_longtext( ) ).
        EXIT.

    ENDTRY.

  ENDMETHOD.


  METHOD process_refused_contract.

    DATA lv_mess TYPE string.
    DATA lv_params TYPE string.

    SELECT * FROM zint_assina01
      INTO TABLE @DATA(lt_assina)
        WHERE etapa = '98' "<-- documentos recusados
          AND id_processo = '01'.

    LOOP AT lt_assina ASSIGNING FIELD-SYMBOL(<fs_docs>).

      TRY.

          DATA(lv_id_approve) = execute_approvals( EXPORTING i_referencia = <fs_docs>-id_referencia ).

          lv_params = '/approvals/' && lv_id_approve && '/reject?reason=ReasonForFailure'.

          DATA(lw_return) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_method = 'PUT').

          IF lw_return-nm_code < 206.

            lv_mess = `Recusa do contrato: ` && <fs_docs>-id_referencia && ` foi realizada no COUPA`.

            me->zif_integracao_adigital_post~set_message( EXPORTING i_message = lv_mess i_msgty = 'S' ).

            UPDATE zint_assina01 SET etapa = '99'
              log_date = sy-datum
              log_uzeit = sy-uzeit
              log_name = sy-uname
            WHERE id_referencia = <fs_docs>-id_referencia.

            COMMIT WORK AND WAIT.

          ELSE.

            lv_mess = `Recusa do contrato: ` && <fs_docs>-id_referencia && ` retornou erro: ` && lw_return-ds_data_retorno.
            me->zif_integracao_adigital_post~set_message( lv_mess ).

          ENDIF.

        CATCH zcx_integracao INTO DATA(ex_int) .

          lv_mess = `Recusa do contrato: ` && <fs_docs>-id_referencia && ` retornou exceção: ` && ex_int->get_longtext( ).

          me->zif_integracao_adigital_post~set_message( lv_mess ).

        CATCH zcx_error INTO DATA(ex_erro) .

          lv_mess = `Recusa do contrato: ` && <fs_docs>-id_referencia && ` retornou exceção: ` && ex_erro->get_longtext( ).

          me->zif_integracao_adigital_post~set_message( lv_mess ).

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD process_signed_contract.

    DATA lt_cab TYPE TABLE OF zins_docs_ref.
    DATA lw_cab TYPE zins_docs_ref.
    DATA lo_object TYPE REF TO zcl_integracao_bry_assina_get.
    DATA lv_text TYPE string.
    DATA lt_contracts TYPE zinc_docs_assina.
    DATA lt_enviar TYPE zinc_docs_assina.
    DATA lv_params TYPE string.
    DATA lv_body TYPE string.

    CREATE OBJECT lo_object.

    lt_contracts = lo_object->zif_integracao_bry_assina_get~get_docs_assinados( ir_id_ref_range = ir_id_ref_range ).

    APPEND LINES OF lo_object->zif_integracao_bry_assina_get~get_messages( )
      TO me->zif_integracao_adigital_post~at_bapiret2_tab.

    CHECK lt_contracts IS NOT INITIAL.

    LOOP AT lt_contracts ASSIGNING FIELD-SYMBOL(<fs_contract>).
      lw_cab-id_referencia = <fs_contract>-id_referencia.
      COLLECT lw_cab INTO lt_cab.
    ENDLOOP.

    LOOP AT lt_cab ASSIGNING FIELD-SYMBOL(<fs_cab>).

      CLEAR lt_enviar.
      break rblima.
      LOOP AT lt_contracts ASSIGNING <fs_contract> WHERE id_referencia = <fs_cab>-id_referencia.

        send_doc_assinado( <fs_contract> ).

        "APPEND <fs_contract> TO lt_enviar.
      ENDLOOP.

      "CHECK lt_enviar IS NOT INITIAL.

      TRY.

          " "{{URL}}/contracts/:contract_id/attachments - UPLOAD DO CONTRATO ASSINADO
          "me->zif_integracao_adigital_post~send_docs_assinados( lt_enviar ).

          " antes de publicar
          me->zif_integracao_adigital_post~before_publishing( <fs_contract> ).

          "{{URL}}/contracts/:id/update_published
          TRY.
              "lv_params = 'contracts/' && <fs_contract>-id_referencia && '/update_published'.

              "DATA(lw_int) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_method = 'PUT').

              lv_params = 'contracts/' && <fs_contract>-id_referencia.

              lv_body = '<?xml version="1.0" encoding="UTF-8"?><contract><status>published</status></contract>'.

              DATA(lw_int) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_body = lv_body   i_method = 'PUT').

            CATCH zcx_integracao INTO DATA(ex_int).

            CATCH zcx_error INTO DATA(ex_erro).

          ENDTRY.

          me->zif_integracao_adigital_post~after_publishing( <fs_contract> ).

          UPDATE zint_assina01 SET etapa = '03' "<-- processo encerrado finalizada
              log_date = sy-datum
              log_uzeit = sy-uzeit
              log_name = sy-uname
            WHERE id_referencia = <fs_contract>-id_referencia.

          COMMIT WORK AND WAIT.

          lv_text = `Contrato: ` && <fs_contract>-id_referencia && ` foi finalizado com sucesso`.

          me->zif_integracao_adigital_post~set_message( EXPORTING i_msgty = 'S' i_message = lv_text ).

        CATCH zcx_integracao INTO ex_int.
          me->zif_integracao_adigital_post~set_message( ex_int->get_longtext( ) ).
          CONTINUE.
        CATCH zcx_error INTO ex_erro.
          me->zif_integracao_adigital_post~set_message( ex_erro->get_longtext( ) ).
          CONTINUE.
      ENDTRY.


    ENDLOOP.


*    LOOP AT lt_contracts INTO DATA(lw_contract).
*
*
*      READ TABLE lt_contracts ASSIGNING FIELD-SYMBOL(<fs_contract>) INDEX sy-tabix.
*
*      AT NEW id_referencia.
*        CLEAR lt_enviar.
*      ENDAT.
*
*      APPEND lw_contract TO lt_enviar.
*
*      AT END OF id_referencia.
*
*        TRY.
*
*            " "{{URL}}/contracts/:contract_id/attachments - UPLOAD DO CONTRATO ASSINADO
*            me->zif_integracao_adigital_post~send_docs_assinados( lt_enviar ).
*
*            " antes de publicar
*            me->zif_integracao_adigital_post~before_publishing( <fs_contract> ).
*
*            "{{URL}}/contracts/:id/update_published
*            TRY.
*                "lv_params = 'contracts/' && <fs_contract>-id_referencia && '/update_published'.
*
*                "DATA(lw_int) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_method = 'PUT').
*
*                lv_params = 'contracts/' && <fs_contract>-id_referencia.
*
*                lv_body = '<?xml version="1.0" encoding="UTF-8"?><contract><status>published</status></contract>'.
*
*                DATA(lw_int) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_body = lv_body   i_method = 'PUT').
*
*              CATCH zcx_integracao INTO DATA(ex_int).
*
*              CATCH zcx_error INTO DATA(ex_erro).
*
*            ENDTRY.
*
*            me->zif_integracao_adigital_post~after_publishing( <fs_contract> ).
*
*            UPDATE zint_assina01 SET etapa = '03' "<-- processo encerrado finalizada
*                log_date = sy-datum
*                log_uzeit = sy-uzeit
*                log_name = sy-uname
*              WHERE id_referencia = <fs_contract>-id_referencia.
*
*            COMMIT WORK AND WAIT.
*
*            lv_text = `Contrato: ` && <fs_contract>-id_referencia && ` foi finalizado com sucesso`.
*
*            me->zif_integracao_adigital_post~set_message( EXPORTING i_msgty = 'S' i_message = lv_text ).
*
*          CATCH zcx_integracao INTO ex_int.
*            me->zif_integracao_adigital_post~set_message( ex_int->get_longtext( ) ).
*            CONTINUE.
*          CATCH zcx_error INTO ex_erro.
*            me->zif_integracao_adigital_post~set_message( ex_erro->get_longtext( ) ).
*            CONTINUE.
*        ENDTRY.
*
*      ENDAT.
*
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD send_doc_assinado.

    DATA lt_multi TYPE zde_multipart_field_t.
    DATA lv_params TYPE string.
    DATA lv_id_ref TYPE zin_id_referencia.

    CHECK i_doc_assinado IS NOT INITIAL.

    TRY .

        lv_id_ref = i_doc_assinado-id_referencia.

        lv_params = 'contracts/' && lv_id_ref && '/attachments'.

        "LOOP AT i_docs_assinados ASSIGNING FIELD-SYMBOL(I_DOC_ASSINADO).

        DATA(lv_value) = 'form-data; name="attachment[file]"; filename="' && i_doc_assinado-nome && '"'.

        APPEND VALUE #( header_field = 'content-disposition' header_value = lv_value  ) TO lt_multi.

        IF i_doc_assinado-file IS NOT INITIAL.
          APPEND VALUE #( header_field = 'content-type' header_value = 'application/pdf' value = i_doc_assinado-file  ) TO lt_multi.
        ELSEIF i_doc_assinado-xfile IS NOT INITIAL.
          APPEND VALUE #( header_field = 'content-type' header_value = 'application/pdf' xvalue = i_doc_assinado-xfile  ) TO lt_multi.
        ENDIF.

        APPEND VALUE #( header_field = 'content-disposition' header_value = 'form-data; name="attachment[type]"' value = 'file' ) TO lt_multi.

        "ENDLOOP.

        DATA(lw_int) = me->zif_integracao_adigital_post~execute_multipart( i_params = lv_params i_multi_tab = lt_multi ).

        IF lw_int-nm_code > 205.
          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_int-ds_data_retorno ).
        ENDIF.

      CATCH zcx_integracao INTO DATA(lo_integ).
        me->zif_integracao_adigital_post~set_message( EXPORTING i_message = lo_integ->get_longtext( ) ).
        "zcx_error=>zif_error~gera_erro_geral( i_texto = lw_int-ds_data_retorno ).
      CATCH zcx_error INTO DATA(lo_erro).
        me->zif_integracao_adigital_post~set_message( EXPORTING i_message = lo_erro->get_longtext( ) ).
        "zcx_error=>zif_error~gera_erro_geral( i_texto = lw_int-ds_data_retorno ).

    ENDTRY.

  ENDMETHOD.


  METHOD UPDATE_STATUS.

    DATA lv_params TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    lv_params = 'contracts/' && i_document-id_referencia && '/' && i_new_status.

    " MENSAGEM EM CASO DE ERRO
    lv_text = `Para o documento: ` && i_document-id_referencia && ` não foi possivel alterar o status para ` && i_new_status.

    TRY.

        DATA(lw_return) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_method = 'PUT' i_body = lv_body ).

        IF lw_return-nm_code > 205.

          me->zif_integracao_adigital_post~set_message( lv_text ).

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int) .

        me->zif_integracao_adigital_post~set_message( lv_text ).

      CATCH zcx_error INTO DATA(ex_erro) .
        me->zif_integracao_adigital_post~set_message( lv_text ).

    ENDTRY.

  ENDMETHOD.


  METHOD update_workflow_key.

    DATA lv_params TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    " --- atualizando a chave de assinatura no documento -------"
    lv_params = 'contracts/' && i_document-id_referencia && '/?fields=["id",{"custom_fields":["chave-coleta-assinatura"]}]'.

    lv_body = '<?xml version="1.0" encoding="UTF-8"?><contract><custom-fields>' &&
              '<chave-coleta-assinatura>' && i_document-chavedocumento && '</chave-coleta-assinatura>' &&
              '</custom-fields></contract>'.

    " MENSAGEM EM CASO DE ERRO
    lv_text = `Para o documento: ` && i_document-id_referencia && ` não foi possivel adicionar a chave de coleta`.

    TRY.

        DATA(lw_return) = me->zif_integracao_adigital_post~execute_service( i_params = lv_params i_method = 'PUT' i_body = lv_body ).

        IF lw_return-nm_code > 205.

          me->zif_integracao_adigital_post~set_message( lv_text ).

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int) .

        me->zif_integracao_adigital_post~set_message( lv_text ).

      CATCH zcx_error INTO DATA(ex_erro) .
        me->zif_integracao_adigital_post~set_message( lv_text ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_adigital_post~after_publishing.

    DATA lv_params TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    CHECK i_document-chavedocumento IS NOT INITIAL.

    DATA lv_message TYPE string.

    TRY.

        update_workflow_key( i_document ).
        "approve_id_execute( i_document ).

      CATCH zcx_integracao INTO DATA(ex_int).

        lv_message = `Tentativa 2 de aprovar aprovadores falhou para o contrato: ` && i_document-id_referencia && `Exceção: ` && ex_int->get_longtext( ).
        me->zif_integracao_adigital_post~set_message( lv_message ).

      CATCH zcx_error INTO DATA(ex_erro).

        lv_message = `Tentativa 2 de aprovar aprovadores falhou para o contrato: ` && i_document-id_referencia && `Exceção: ` && ex_erro->get_longtext( ).
        me->zif_integracao_adigital_post~set_message( lv_message ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_adigital_post~before_publishing.

    DATA lv_message TYPE string.

    TRY.

        "update_workflow_key( i_document ).
        approve_id_execute( i_document ).

        update_status( i_document = i_document i_new_status = 'ccc_signature_created' ).

        update_status( i_document = i_document i_new_status = 'complete' ).

      CATCH zcx_integracao INTO DATA(ex_int).

        lv_message = `Tentativa 1 de aprovar aprovadores falhou para o contrato: ` && i_document-id_referencia && `Exceção: ` && ex_int->get_longtext( ).
        me->zif_integracao_adigital_post~set_message( lv_message ).

      CATCH zcx_error INTO DATA(ex_erro).

        lv_message = `Tentativa 1 de aprovar aprovadores falhou para o contrato: ` && i_document-id_referencia && `Exceção: ` && ex_erro->get_longtext( ).
        me->zif_integracao_adigital_post~set_message( lv_message ).

    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_adigital_post~execute_multipart.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->zif_integracao_adigital_post~at_servico IS NOT INITIAL.

      IF me->zif_integracao_adigital_post~at_auth_ws IS INITIAL.

        SELECT SINGLE * FROM zauth_webservice
          INTO me->zif_integracao_adigital_post~at_auth_ws
            WHERE service = me->zif_integracao_adigital_post~at_servico.

      ENDIF.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    IF me->zif_integracao_adigital_post~at_auth_ws  IS INITIAL.
      lv_msgtx = `Serviço ` && me->zif_integracao_adigital_post~at_servico && ` não está configurado na ZAUTH_WEBSERVICE`.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).
    ENDIF.

    REPLACE '#PARAMS#' IN me->zif_integracao_adigital_post~at_auth_ws-url WITH space.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'multipart/form-data'.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_adigital_post~at_auth_ws-url && i_params.
    "me->zif_integracao_inject~at_multipart_fields = i_multi_tab.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    zcl_string2=>gui_indicator_url( me->zif_integracao_inject~at_info_request_http-ds_url ) .

    CREATE OBJECT lc_integrar.


    lc_integrar->zif_integracao~at_multipart = i_multi_tab.
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


  METHOD zif_integracao_adigital_post~execute_service.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->zif_integracao_adigital_post~at_servico IS NOT INITIAL.

      IF me->zif_integracao_adigital_post~at_auth_ws IS INITIAL.

        SELECT SINGLE * FROM zauth_webservice
          INTO me->zif_integracao_adigital_post~at_auth_ws
            WHERE service = me->zif_integracao_adigital_post~at_servico.

      ENDIF.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    IF me->zif_integracao_adigital_post~at_auth_ws  IS INITIAL.
      lv_msgtx = `Serviço ` && me->zif_integracao_adigital_post~at_servico && ` não está configurado na ZAUTH_WEBSERVICE`.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).
    ENDIF.

    REPLACE '#PARAMS#' IN me->zif_integracao_adigital_post~at_auth_ws-url WITH space.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_adigital_post~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_adigital_post~at_auth_ws-url && i_params.
    CLEAR me->zif_integracao_inject~at_multipart_fields.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    IF i_body IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_body = i_body.
    ELSE.
      CLEAR me->zif_integracao_inject~at_info_request_http-ds_body.
    ENDIF.

    zcl_string2=>gui_indicator_url( me->zif_integracao_inject~at_info_request_http-ds_url ).

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


  METHOD zif_integracao_adigital_post~get_instance.

    IF zif_integracao_adigital_post~at_instance IS NOT BOUND.

      zif_integracao_adigital_post~at_instance = NEW zcl_integracao_coupa_ad_post( ).

    ENDIF.

    r_object = zif_integracao_adigital_post~at_instance.

  ENDMETHOD.


  METHOD zif_integracao_adigital_post~get_messages.

    r_ret = me->zif_integracao_adigital_post~at_bapiret2_tab.

  ENDMETHOD.


  METHOD zif_integracao_adigital_post~process_contracts.

    process_signed_contract( ir_id_ref_range[] ).

    process_refused_contract( ir_id_ref_range[] ).

  ENDMETHOD.


  METHOD zif_integracao_adigital_post~send_docs_assinados.

    DATA lt_multi TYPE zde_multipart_field_t.
    DATA lv_params TYPE string.
    DATA lv_id_ref TYPE zin_id_referencia.

    CHECK i_docs_assinados IS NOT INITIAL.

    lv_id_ref = i_docs_assinados[ 1 ]-id_referencia.

    lv_params = 'contracts/' && lv_id_ref && '/attachments'.

    LOOP AT i_docs_assinados ASSIGNING FIELD-SYMBOL(<fs_anexos>).

      DATA(lv_value) = 'form-data; name="attachment[file]"; filename="' && <fs_anexos>-nome && '"'.

      APPEND VALUE #( header_field = 'content-disposition' header_value = lv_value  ) TO lt_multi.

      IF <fs_anexos>-file IS NOT INITIAL.
        APPEND VALUE #( header_field = 'content-type' header_value = 'application/pdf' value = <fs_anexos>-file  ) TO lt_multi.
      ELSEIF <fs_anexos>-xfile IS NOT INITIAL.
        APPEND VALUE #( header_field = 'content-type' header_value = 'application/pdf' xvalue = <fs_anexos>-xfile  ) TO lt_multi.
      ENDIF.

      APPEND VALUE #( header_field = 'content-disposition' header_value = 'form-data; name="attachment[type]"' value = 'file' ) TO lt_multi.

    ENDLOOP.

    DATA(lw_int) = me->zif_integracao_adigital_post~execute_multipart( i_params = lv_params i_multi_tab = lt_multi ).

    IF lw_int-nm_code > 205.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lw_int-ds_data_retorno ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_adigital_post~set_message.

    DATA(lv_msgty) = i_msgty.

    CHECK i_message IS NOT INITIAL.

    IF lv_msgty IS INITIAL.
      lv_msgty = 'E'.
    ENDIF.

    APPEND INITIAL LINE TO me->zif_integracao_adigital_post~at_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_message>).

    DATA: lt_trtexts     TYPE trtexts,
          lw_trtexts     TYPE trtext,
          lv_texto(4000).

    DATA lv_msg1 TYPE sy-msgv1.
    DATA lv_msg2 TYPE sy-msgv1.
    DATA lv_msg3 TYPE sy-msgv1.
    DATA lv_msg4 TYPE sy-msgv1.

    "lv_texto = i_message.

    lv_texto = zcl_string2=>remove_spec_char( i_message ).

    TRY .

        CALL FUNCTION 'TR_SPLIT_TEXT'
          EXPORTING
            iv_text  = lv_texto
            iv_len   = 40
          IMPORTING
            et_lines = lt_trtexts.

    ENDTRY.

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


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    CHECK me->zif_integracao_inject~at_header_fields IS INITIAL.

    TRY .

        " debug
        IF sy-uname = 'RBLIMA' OR sy-sysid = 'DEV'.
          APPEND VALUE #( name = 'x-coupa-api-key' value = 'be82ead986a55cbb33155ab2c7661641307a8b8b' ) TO me->zif_integracao_inject~at_header_fields.
        ELSE.

          CAST zcl_integracao_token_coupa(
                 zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                   )->get_token( )
               )->zif_integracao_inject~get_header_request_http(
            IMPORTING
              e_header_fields = DATA(e_header_fields) ).


          me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).
        ENDIF.

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

    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
