class ZCL_INTEGRACAO_COUPA_AD_GET definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_ADIGITAL_GET .
  interfaces ZIF_INTEGRACAO_INJECT .

  data GV_PARAMS type STRING .

  methods CONSTRUCTOR .
  methods GET_ASSINANTES_EXTRAS
    importing
      !IW_DADOS_CONTRATO type ZINS_ADIGITAL_CONTRACT
    returning
      value(R_RELACAO) type STRING .
  methods GET_ATTACHMENTS
    importing
      !I_REFERENCE type STRING
    returning
      value(R_ATTACHMENTS) type ZINS_ADIGITAL_ATTACHMENTS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_ATTACHMENTS2
    importing
      !I_REFERENCE type STRING
    returning
      value(R_ATTACHMENTS) type ZINS_ADIGITAL_ATTACHMENTS
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods LOG_CONTRACT_ADD
    importing
      !I_LOG_TAB type ZINC_ASSINA02
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods LOG_MERGE
    changing
      !C_LOG_TAB type ZINC_ASSINA02
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.

  data GV_CURRENT_ID type ZIN_ID_REFERENCIA .

  methods FORMAT_DATE_BRY
    importing
      !I_DATE type STRING optional
    returning
      value(R_DATE) type STRING .
  methods CPF_BY_EMAIL
    importing
      !I_EMAIL type STRING
    returning
      value(R_CPF) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods GET_CONTRACT_URL
    importing
      !ID_CONTRATO type ZIN_ID_REFERENCIA
    returning
      value(R_URL) type STRING .
  methods GET_FORMATED_DATE
    importing
      !IV_DATE type SY-DATUM
    returning
      value(R_DATE_S) type STRING .
  methods GET_RESPONSAVEL_ASSINATURA
    returning
      value(R_RESPON) type MSGTX .
  methods MESSAGE_LENGTH
    importing
      !IV_XFILE type XSTRING .
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_AD_GET IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_adigital.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->zif_integracao_adigital_get~at_service = 'COUPA_INT_ASSINATURA_DIGITAL'.

  ENDMETHOD.


  METHOD cpf_by_email.

    DATA lv_mess TYPE string.

    DATA lv_cpf TYPE string.

   IF i_email = 'daniel.filippo@pwc.com' AND sy-sysid = 'QAS'.
      r_cpf = '15706790701'.
      EXIT.
    ENDIF.

    IF i_email = 'ramon.lima@reclike.com.br' AND sy-sysid = 'QAS'.
      r_cpf = '05332137974'.
      EXIT.
    ENDIF.

    CHECK i_email IS NOT INITIAL.

    SELECT SINGLE cpf_nr FROM zhcmt0007
      INTO lv_cpf
        WHERE email_ad = i_email.

    IF sy-subrc EQ 0.

      REPLACE ALL OCCURRENCES OF '.' IN lv_cpf WITH space.
      REPLACE ALL OCCURRENCES OF '-' IN lv_cpf WITH space.

      CONDENSE lv_cpf NO-GAPS.

      r_cpf = lv_cpf.

    ELSE.

      lv_mess = `CPF para o email: ` && i_email && ` não foi encontrado`.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_mess ).

      "me->zif_integracao_adigital_get~set_message( lv_mess ).

    ENDIF.

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


  METHOD get_assinantes_extras.

    DATA lv_mess TYPE string.
    DATA lo_xml TYPE REF TO cl_xml_document.
    DATA lv_email TYPE string.
    DATA lv_cpf   TYPE string.
    DATA lv_nome TYPE string.

    TRY .

        IF iw_dados_contrato-customfields-compradorresponsvel IS NOT INITIAL.

          " ------------------ DADOS DO COMPRADOR
          IF iw_dados_contrato-customfields-compradorresponsvel-customfields-email IS INITIAL.

            DATA(lw_lookup) = me->zif_integracao_adigital_get~lookup_values_execute( iw_dados_contrato-customfields-compradorresponsvel-id ).

            IF lw_lookup-nm_code < 206.

              CREATE OBJECT lo_xml.

              IF lo_xml->parse_xstring( lw_lookup-ds_data_xstring ) = 0.
                lv_email = lo_xml->find_simple_element( 'email' ).
              ENDIF.

            ENDIF.

          ELSE.

            lv_email = iw_dados_contrato-customfields-compradorresponsvel-customfields-email.

          ENDIF.

          " busca no HR
          lv_cpf = me->cpf_by_email( lv_email ).

          IF lv_email IS INITIAL OR lv_cpf IS INITIAL.

            lv_mess = `Contrato: ` && iw_dados_contrato-id && `, sem dados de email para comprador`.

            me->zif_integracao_adigital_get~set_message( lv_mess ).

          ELSE.

            r_relacao =  lv_cpf && '-' && iw_dados_contrato-customfields-compradorresponsvel-name
                         && '-' && lv_email && '-' && 'ASSINATURA_ELETRONICA' && ';'. "'CERTIFICADO_DIGITAL' && ';'.

          ENDIF.

        ELSE.

          " ------------------ DADOS DO CREATED-BY

          lv_email = iw_dados_contrato-createdby-email.
          lv_cpf = me->cpf_by_email( lv_email ).
          lv_nome = iw_dados_contrato-createdby-fullname.

          IF lv_cpf IS NOT INITIAL.

            r_relacao =  r_relacao && lv_cpf && '-' && lv_nome
                                   && '-' && lv_email && '-' && 'ASSINATURA_ELETRONICA' && ';'. "'CERTIFICADO_DIGITAL' && ';'.

          ENDIF.

        ENDIF.

        " ------------------ DADOS DO GESTOR TECNICO

        lv_email = iw_dados_contrato-customfields-GESTORCONTRATO-email.

        lv_cpf = me->cpf_by_email( lv_email ).
        lv_nome = iw_dados_contrato-customfields-GESTORCONTRATO-fullname.

        IF lv_cpf IS NOT INITIAL.

          r_relacao =  r_relacao && lv_cpf && '-' && lv_nome
                                 && '-' && lv_email && '-' && 'ASSINATURA_ELETRONICA' && ';'. "'CERTIFICADO_DIGITAL' && ';'.

        ENDIF.

        " ------------------ DADOS RESPONSAVEL JURIDICO
        DATA(lv_juridico) = iw_dados_contrato-customfields-responsaveljuridico-customfields-relacaoassinantes.

        IF lv_juridico IS NOT INITIAL.

          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_juridico WITH ';'.

          REPLACE ALL OCCURRENCES OF `"` IN lv_juridico WITH space.
          REPLACE ALL OCCURRENCES OF `'` IN lv_juridico WITH space.

          REPLACE ALL OCCURRENCES OF ',' IN lv_juridico WITH '-'.
          REPLACE ALL OCCURRENCES OF `- ` IN lv_juridico WITH '-'.
          REPLACE ALL OCCURRENCES OF `-;` IN lv_juridico WITH space.
          r_relacao = r_relacao && lv_juridico.

        ENDIF.

        " ------------------ DADOS SUPPLIER
        DATA(lv_supplier) = iw_dados_contrato-supplier-customfields-relaodeassinantes.

        IF lv_supplier IS NOT INITIAL.

          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_supplier WITH ';'.

          REPLACE ALL OCCURRENCES OF `"` IN lv_supplier WITH space.
          REPLACE ALL OCCURRENCES OF `'` IN lv_supplier WITH space.

          REPLACE ALL OCCURRENCES OF ',' IN lv_supplier WITH '-'.
          REPLACE ALL OCCURRENCES OF `- ` IN lv_supplier WITH '-'.
          REPLACE ALL OCCURRENCES OF `AL-` IN lv_supplier WITH 'AL'.
          REPLACE ALL OCCURRENCES OF `CA-` IN lv_supplier WITH 'CA'.

          r_relacao = r_relacao && ';' && lv_supplier.

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_adigital_get~set_message( i_message = ex_int->get_longtext( ) i_doc_id =  iw_dados_contrato-id ).

      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_adigital_get~set_message( i_message = ex_erro->get_longtext( ) i_doc_id =  iw_dados_contrato-id ).

    ENDTRY.

  ENDMETHOD.


  METHOD get_attachments.

    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.

    gv_params = 'contracts/' && i_reference && '/retrieve_legal_agreement'.

    TRY.

        DATA(lw_return) = me->zif_integracao_adigital_get~execute_service( i_params = gv_params i_method = 'GET' ).

        IF lw_return-nm_code > 206.

          lv_text = `Requisição de legal agreement: ` && i_reference && 'falhou'.

          zcx_error=>zif_error~gera_erro_geral( i_texto =  lv_text ).

        ENDIF.

        "APPEND 'attachment' TO t_element_array.

        "zcl_string2=>xml_to_table( EXPORTING i_xml = lw_return-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = r_attachments ).

        APPEND INITIAL LINE TO r_attachments-attachments-attachment ASSIGNING FIELD-SYMBOL(<fs_attach>).

        <fs_attach>-id = '001'.
        <fs_attach>-createdat = format_date_bry( ).
        <fs_attach>-updatedat = format_date_bry( ).
        <fs_attach>-type = 'AttachmentFile'.
        <fs_attach>-intent = 'Internal'.
        <fs_attach>-file = 'C:/temp/file.pdf'.
        <fs_attach>-fileurl = 'htps://temp/file.pdf'.
        <fs_attach>-file_name = 'file.pdf'.
        <fs_attach>-application_type = 'application/pdf'.
        <fs_attach>-xfile = lw_return-ds_data_xstring.

        message_length( <fs_attach>-xfile ).

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_adigital_get~set_message( ex_int->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_adigital_get~set_message( ex_erro->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_attachments2.

    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.

    gv_params = 'contracts/' && i_reference && '/attachments'.

    TRY.
        " // RECUPERA RELAÇÃO DE ANEXOS --
        DATA(lw_return) = me->zif_integracao_adigital_get~execute_service( i_params = gv_params i_method = 'GET' ).

        IF lw_return-nm_code > 206.

          "lv_text = 'Requisição de anexos do contrato:' && i_reference && 'falhou'.

          "zcx_error=>zif_error~gera_erro_geral( i_texto =  lv_text ).

        ENDIF.

        APPEND 'attachment' TO t_element_array.

        zcl_string2=>xml_to_table( EXPORTING i_xml = lw_return-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = r_attachments ).

        LOOP AT r_attachments-attachments-attachment ASSIGNING FIELD-SYMBOL(<fs_anexo>).

          <fs_anexo>-file = zcl_string2=>remove_spec_char( <fs_anexo>-file ).

          <fs_anexo>-fileurl = zcl_string2=>remove_spec_char( <fs_anexo>-fileurl ).

          zcl_string2=>file_properties( EXPORTING i_file_path = <fs_anexo>-fileurl IMPORTING e_application_type = <fs_anexo>-application_type e_file_name = <fs_anexo>-file_name ).

          CHECK <fs_anexo>-id IS NOT INITIAL AND <fs_anexo>-xfile IS INITIAL.

          gv_params = 'contracts/' && i_reference && '/attachments/' && <fs_anexo>-id.

          <fs_anexo>-xfile = me->zif_integracao_adigital_get~execute_service( i_params = gv_params i_method = 'GET' )-ds_data_xstring.

          message_length( <fs_anexo>-xfile ).

        ENDLOOP.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_adigital_get~set_message( ex_int->get_longtext( ) ).
        EXIT.

      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_adigital_get~set_message( ex_erro->get_longtext( ) ).
        EXIT.

    ENDTRY.

  ENDMETHOD.


  METHOD get_contract_url.

    DATA(lv_rev) = me->zif_integracao_adigital_get~at_auth_ws-url.

    REPLACE 'api/' IN lv_rev WITH space.

    r_url = lv_rev && 'contracts/show/' && id_contrato && '#summary'.

  ENDMETHOD.


  METHOD get_formated_date.

    r_date_s = iv_date(4) && '-' && iv_date+4(2) && '-' && iv_date+6(2).

  ENDMETHOD.


  METHOD get_responsavel_assinatura.

    DATA lt_set_tab TYPE rgsbv_tab.

    CLEAR r_respon.

    CALL FUNCTION 'G_SET_FETCH'
      EXPORTING
        setnr           = '0000MAGGI_RESPONSAVEL_BRY'
      TABLES
        set_lines_basic = lt_set_tab
      EXCEPTIONS
        no_authority    = 1
        set_is_broken   = 2
        set_not_found   = 3
        OTHERS          = 4.

    IF sy-subrc <> 0.
      "p_erro = 'X'.
      "EXIT.
    ENDIF.

    READ TABLE lt_set_tab ASSIGNING FIELD-SYMBOL(<fs_dat>) INDEX 1.

    IF sy-subrc EQ 0.

      r_respon = <fs_dat>-from.

    ENDIF.

  ENDMETHOD.


  METHOD log_contract_add.

    DATA lv_url_cont TYPE string.
    DATA lv_template TYPE string.
    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.

    gv_params = 'requisitions/' && me->zif_integracao_adigital_get~at_auth_ws-add01 &&'/comments'.

    lv_template = `<comment><comments>@CLMS - Jurídico - #DESCRI_CONTRATO Erro de integração na assinatura do contrato: ` &&
                  `#ID_CONTRATO Resultado: #MSG_ERRO; Favor verificar. #URL_CONTRATO</comments></comment>`.

    TRY.

        LOOP AT i_log_tab ASSIGNING FIELD-SYMBOL(<fs_log>).

          lv_body = lv_template.

          lv_url_cont = get_contract_url( <fs_log>-id_referencia ).

          READ TABLE me->zif_integracao_adigital_get~at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_contract>)
            WITH KEY id = <fs_log>-id_referencia.

          CHECK <fs_contract> IS ASSIGNED.

          REPLACE '#DESCRI_CONTRATO' IN lv_body WITH <fs_contract>-name.
          REPLACE ALL OCCURRENCES OF '#ID_CONTRATO' IN lv_body WITH <fs_log>-id_referencia.

          REPLACE '#MSG_ERRO' IN lv_body WITH <fs_log>-message.
          REPLACE '#URL_CONTRATO' IN lv_body WITH lv_url_cont.

          DATA(lw_return) = me->zif_integracao_adigital_get~execute_service( i_params = gv_params i_body = lv_body i_method = 'POST' ).

          IF lw_return-nm_code > 206.

            lv_text = 'Requisição de anexos do contrato:' && <fs_log>-id_referencia && 'falhou'.

            zcx_error=>zif_error~gera_erro_geral( i_texto =  lv_text ).

          ENDIF.

        ENDLOOP.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_adigital_get~set_message( ex_int->get_longtext( ) ).
        EXIT.

      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_adigital_get~set_message( ex_erro->get_longtext( ) ).
        EXIT.

    ENDTRY.

  ENDMETHOD.


  METHOD log_merge.

    " se a tabela de log nao estiver vazia
    CHECK c_log_tab IS NOT INITIAL.

    " ve se encontra mensagens já salvas
    SELECT * FROM zint_assina02
      INTO TABLE @DATA(lt_log)
        FOR ALL ENTRIES IN @c_log_tab
          WHERE id_referencia = @c_log_tab-id_referencia
            AND id_processo = @c_log_tab-id_processo
            AND id_mensagem = @c_log_tab-id_mensagem
            AND msgty = @c_log_tab-msgty
            AND msgid = @c_log_tab-msgid.

    CHECK sy-subrc EQ 0.

    " se encontrar elimina do log atual, não precisa gravar duas vezes
    LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).

      DELETE c_log_tab WHERE id_referencia = <fs_log>-id_referencia
                         AND id_processo = <fs_log>-id_processo
                         AND id_mensagem = <fs_log>-id_mensagem
                         AND msgty = <fs_log>-msgty
                         AND msgid = <fs_log>-msgid.

      CHECK c_log_tab IS INITIAL.

      EXIT.

    ENDLOOP.

  ENDMETHOD.


  METHOD message_length.

    DATA lv_id TYPE string.

    DATA lv_char TYPE c LENGTH 30.

    DATA lv_mess TYPE string.

    DATA lv_div TYPE i VALUE 1000000.

    DATA lv_mb TYPE p DECIMALS 3.

    DATA(lv_length) = xstrlen( iv_xfile ).

    lv_mb = lv_length / lv_div.

    WRITE lv_mb TO lv_char LEFT-JUSTIFIED.

    lv_mess = `Arquivo com tamanho ` && lv_char && ` MB`.

    lv_id = me->gv_current_id.

    CALL METHOD me->zif_integracao_adigital_get~set_message
      EXPORTING
        i_message = lv_mess
        i_msgty   = 'W'
        i_doc_id  = lv_id.

  ENDMETHOD.


  METHOD zif_integracao_adigital_get~after_sending.


    DATA lv_body TYPE string.
    DATA lv_text TYPE string.

    CHECK i_adigital IS NOT INITIAL.

    MODIFY zint_assina01 FROM i_adigital.

    "APPEND i_adigital TO gt_contracts.

    " foi retirado no dia 06.04.2022 - porque o published vai ser feito após a coleta
*    CHECK i_adigital-chave_coleta IS NOT INITIAL.
*
*    " --- atualizando o status para publish -------"
*    gv_params = 'contracts/' && i_adigital-id_referencia && '/update_published'.
*
*    DATA(lw_return) = me->zif_integracao_adigital_get~execute_service( i_params = gv_params i_method = 'PUT' ).
*
*    IF lw_return-nm_code > 205.
*      lv_text = `Para o documento: ` && i_adigital-id_referencia && ` não foi possivel atualizar para published`.
*      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_text ).
*    ENDIF.
*
*
*    " --- atualizando a chave de assinatura no documento -------"
*    gv_params = 'contracts/' && i_adigital-id_referencia && '/?fields=["id",{"custom_fields":["chave-coleta-assinatura"]}]'.
*
*    lv_body = '<?xml version="1.0" encoding="UTF-8"?><contract><custom-fields>' &&
*              '<chave-coleta-assinatura>' && i_adigital-chave_coleta && '</chave-coleta-assinatura>' &&
*              '</custom-fields></contract>'.
*
*    lw_return = me->zif_integracao_adigital_get~execute_service( i_params = gv_params i_method = 'PUT' i_body = lv_body ).
*
*    IF lw_return-nm_code > 205.
*      lv_text = `Para o documento: ` && i_adigital-id_referencia && `não foi possivel adicionar a chave de coleta`.
*      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_text ).
*    ENDIF.



  ENDMETHOD.


  METHOD zif_integracao_adigital_get~before_sending.

    "BREAK RBLIMA.

    "IF 1 = 2."TESTE
    r_attachments = me->get_attachments( i_reference ).
    "ELSE.
    DATA(lw_attach_normal) = me->get_attachments2( i_reference ).
    "ENDIF.

    IF lw_attach_normal-attachments-attachment IS NOT INITIAL.
      APPEND LINES OF lw_attach_normal-attachments-attachment TO r_attachments-attachments-attachment.
    ENDIF.



  ENDMETHOD.


  method ZIF_INTEGRACAO_ADIGITAL_GET~EXECUTE_MULTIPART.
  endmethod.


  METHOD zif_integracao_adigital_get~execute_service.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->zif_integracao_adigital_get~at_service IS NOT INITIAL.

      IF me->zif_integracao_adigital_get~at_auth_ws IS INITIAL.

        SELECT SINGLE * FROM zauth_webservice
          INTO me->zif_integracao_adigital_get~at_auth_ws
            WHERE service = me->zif_integracao_adigital_get~at_service.

      ENDIF.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    IF me->zif_integracao_adigital_get~at_auth_ws  IS INITIAL.
      lv_msgtx = `Serviço ` && me->zif_integracao_adigital_get~at_service && ` não está configurado na ZAUTH_WEBSERVICE`.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_adigital_get~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_adigital_get~at_auth_ws-url && i_params.
    CLEAR me->zif_integracao_inject~at_multipart_fields.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    "me->ZIF_INTEGRACAO_ADIGITAL_GET~gui_indicator_exe( ).

    CREATE OBJECT lc_integrar.

    IF i_body IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_body = i_body.
    ELSE.
      CLEAR me->zif_integracao_inject~at_info_request_http-ds_body.
    ENDIF.

    zcl_string2=>gui_indicator_url( me->zif_integracao_inject~at_info_request_http-ds_url ).

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


  METHOD zif_integracao_adigital_get~get_assinantes.

    DATA lv_params TYPE string.
    DATA lv_erro TYPE c.
    DATA lv_message TYPE string.

    DATA lo_xml TYPE REF TO cl_xml_document.

    CHECK i_id IS NOT INITIAL.

    lv_params = 'lookup_values/' && i_id.

    DATA(lw_int) = me->zif_integracao_adigital_get~execute_service( i_params = lv_params i_method = 'GET').

    CHECK lw_int-nm_code = 200 OR lw_int-nm_code = 201 OR lw_int-nm_code = 202.

    CREATE OBJECT lo_xml.

    CHECK lo_xml->parse_xstring( lw_int-ds_data_xstring ) = 0.

    DATA(lv_rel_assin) = lo_xml->find_simple_element( 'relacao-assinantes' ).

    CHECK lv_rel_assin IS NOT INITIAL.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_rel_assin WITH ';'.

    REPLACE ALL OCCURRENCES OF `"` IN lv_rel_assin WITH space.
    REPLACE ALL OCCURRENCES OF `'` IN lv_rel_assin WITH space.

    REPLACE ALL OCCURRENCES OF ',' IN lv_rel_assin WITH '-'.
    REPLACE ALL OCCURRENCES OF `- ` IN lv_rel_assin WITH '-'.
    REPLACE ALL OCCURRENCES OF `-;` IN lv_rel_assin WITH ';'.

    r_relacao = lv_rel_assin.

  ENDMETHOD.


  METHOD zif_integracao_adigital_get~get_documents.

    DATA lv_date TYPE sy-datum.

    DATA t_element_array  TYPE zde_element_array_t.

    CLEAR me->zif_integracao_adigital_get~at_bapiret2_tab.

*    gv_params = 'contracts/?status=pending_approval&[custom_fields]' &&
*                '[chave_coleta_assinatura][blank]=true&fields=["id","created-at",' &&
*                '"updated-at","name","number","type","version","start-date","end-date",' &&
*                '"status",{"current_approval":["approver-id"]},{"custom_fields":' &&
*                '["chave-coleta-assinatura","data-limite-assinaturas","relacao-assinantes",' &&
*                '"exigir-download","proibir-rejeio","assinatura-sequencial","agrupar-documentos",' &&
*                '"pagina-assinaturas","relatrio-assinaturas","protocolo-assinaturas"]}]'.

    lv_date = sy-datum.

    SUBTRACT 5 FROM lv_date.

    DATA(lv_date_s) = get_formated_date( lv_date ).

    gv_params = 'contracts/?status=pending_approval&created_at[gt]=' && lv_date_s.

    TRY .

        DATA(lw_return) = me->zif_integracao_adigital_get~execute_service( i_params = gv_params i_method = 'GET' ).

        IF lw_return-nm_code > 206.

          zcx_error=>zif_error~gera_erro_geral( i_texto = 'Requisição de contratos com problema' ).

        ELSEIF lw_return-nm_code = '404'.

          zcx_error=>zif_error~gera_erro_geral( i_texto = 'Sem contratos para o periodo selecionado' ).

        ENDIF.

        APPEND 'contract' TO t_element_array.
        APPEND 'relacaoassinantes' TO t_element_array.

        zcl_string2=>xml_to_table( EXPORTING i_xml = lw_return-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = me->zif_integracao_adigital_get~at_doc_tab ).

        IF ir_id_ref_range IS NOT INITIAL.
          DELETE me->zif_integracao_adigital_get~at_doc_tab-contracts-contract WHERE id NOT IN ir_id_ref_range.
        ENDIF.

        DELETE me->zif_integracao_adigital_get~at_doc_tab-contracts-contract WHERE currentapproval-approverid <> '28'.

        IF me->zif_integracao_adigital_get~at_doc_tab-contracts-contract IS NOT INITIAL.

          DATA lr_ref TYPE RANGE OF zin_id_referencia.

          LOOP AT me->zif_integracao_adigital_get~at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_docs>).

            APPEND 'IEQ' && <fs_docs>-id TO lr_ref.

            " 13.04.2022 - alteração na estrutura do xml vindo do coupa ->
            "<fs_docs>-customfields-relacaoassinantes_old = <fs_docs>-customfields-relacaoassinantes-relacaoassinante-customfields-relacaoassinantes.

*---> S4 Migration - 11/07/2023 - PS
*            LOOP AT <fs_docs>-customfields-relacaoassinantes ASSIGNING FIELD-SYMBOL(<fs_assina>).
*
*              DATA(lv_lista) = me->zif_integracao_adigital_get~get_assinantes( <fs_assina>-relacaoassinante-id ).
*
*              <fs_docs>-customfields-relacaoassinantes_old = <fs_docs>-customfields-relacaoassinantes_old && lv_lista.
*
*            ENDLOOP.
*<--- S4 Migration - 11/07/2023 - PS

            <fs_docs>-customfields-relacaoassinantes_old = <fs_docs>-customfields-relacaoassinantes_old && me->get_assinantes_extras( <fs_docs> ).

            <fs_docs>-customfields-exigirdownload_old = <fs_docs>-customfields-exigirdownload-name.
            <fs_docs>-customfields-proibirrejeio_old = <fs_docs>-customfields-proibirrejeicao-name.
            <fs_docs>-customfields-assinaturasequencial_old = <fs_docs>-customfields-assinaturasequencial-name.
            <fs_docs>-customfields-agrupardocumentos_old = <fs_docs>-customfields-agrupardocumentos-name.

            " 13.04.2022 - alteração na estrutura do xml vindo do coupa -<

            " DEBUG TESTE RAMON -->
            "BREAK RBLIMA.
            IF 1 = 2.
              <fs_docs>-customfields-relacaoassinantes_old = '05332137974-RAMON LIMA-ramon.lima@reclike.com.br-ASSINATURA_ELETRONICA'.
            ENDIF.
            " DEBUG TESTE RAMON --<

          ENDLOOP.

          SELECT * FROM zint_assina01
            INTO TABLE @DATA(lt_adigital)
                WHERE id_referencia IN @lr_ref
                  AND id_processo = '01'
                  AND etapa <> '99'. " < -- OU SEJA SE NÃO EXCLUI OS QUE FORAM RECUSADOS

          IF sy-subrc EQ 0.

            LOOP AT lt_adigital ASSIGNING FIELD-SYMBOL(<fs_digital>).
              DELETE me->zif_integracao_adigital_get~at_doc_tab-contracts-contract WHERE id = <fs_digital>-id_referencia.
            ENDLOOP.

          ENDIF.

        ENDIF.



        r_ret = me.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_adigital_get~set_message( ex_int->get_longtext( ) ).

      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_adigital_get~set_message( ex_erro->get_longtext( ) ).

    ENDTRY.


  ENDMETHOD.


  METHOD zif_integracao_adigital_get~get_documents_by_id.


    DATA t_element_array  TYPE zde_element_array_t.

    CLEAR me->zif_integracao_adigital_get~at_bapiret2_tab.

    gv_params = 'contracts/' && id_referencia.

    TRY .

        DATA(lw_return) = me->zif_integracao_adigital_get~execute_service( i_params = gv_params i_method = 'GET' ).

        IF lw_return-nm_code > 206.

          zcx_error=>zif_error~gera_erro_geral( i_texto = 'Requisição de contratos com problema' ).

        ENDIF.

        APPEND 'contract' TO t_element_array.

        zcl_string2=>xml_to_table( EXPORTING i_xml = lw_return-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = me->zif_integracao_adigital_get~at_doc_tab ).

        IF me->zif_integracao_adigital_get~at_doc_tab-contracts-contract IS NOT INITIAL.

          DATA lr_ref TYPE RANGE OF zin_id_referencia.

          LOOP AT me->zif_integracao_adigital_get~at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_docs>).
            APPEND 'IEQ' && <fs_docs>-id TO lr_ref.
          ENDLOOP.

          SELECT * FROM zint_assina01
            INTO TABLE @DATA(lt_adigital)
                WHERE id_referencia IN @lr_ref.

          IF sy-subrc EQ 0.

            LOOP AT lt_adigital ASSIGNING FIELD-SYMBOL(<fs_digital>).
              DELETE me->zif_integracao_adigital_get~at_doc_tab-contracts-contract WHERE id = <fs_digital>-id_referencia.
            ENDLOOP.

          ENDIF.

        ENDIF.

        r_ret = me.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_adigital_get~set_message( ex_int->get_longtext( ) ).

      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_adigital_get~set_message( ex_erro->get_longtext( ) ).

    ENDTRY.




  ENDMETHOD.


  METHOD zif_integracao_adigital_get~get_instance.

    IF zif_integracao_adigital_get~at_object IS NOT BOUND.
      zif_integracao_adigital_get~at_object = NEW zcl_integracao_coupa_ad_get( ).
    ENDIF.

    r_ret = zif_integracao_adigital_get~at_object.

  ENDMETHOD.


  METHOD zif_integracao_adigital_get~get_messages.

    r_ret = me->zif_integracao_adigital_get~at_bapiret2_tab.

  ENDMETHOD.


  METHOD zif_integracao_adigital_get~log_update.

    DATA lv_cont TYPE i.
    DATA lt_log TYPE TABLE OF zint_assina02.

    DATA(lt_message) = me->zif_integracao_adigital_get~get_messages( ).

    DATA(lv_inidt) = sy-datum.
    DATA(lv_fimdt) = sy-datum.

    SORT lt_message BY parameter ASCENDING.

    r_ret = me.

    LOOP AT lt_message INTO DATA(lw_mess).

      READ TABLE lt_message ASSIGNING FIELD-SYMBOL(<fs_mess>) INDEX sy-tabix.

      CHECK sy-subrc EQ 0.

      AT NEW parameter.
        CLEAR lv_cont.
      ENDAT.

      ADD 1 TO lv_cont.

      APPEND INITIAL LINE TO lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).

      <fs_log>-id_referencia = <fs_mess>-parameter.
      <fs_log>-id_processo = '01'.
      <fs_log>-msgdt = sy-datum.
      <fs_log>-msghr = sy-uzeit.
      <fs_log>-id_mensagem = lv_cont.
      <fs_log>-msgty = <fs_mess>-type.
      <fs_log>-msgid = <fs_mess>-number.
      <fs_log>-message = <fs_mess>-message.
      <fs_log>-msgv1 = <fs_mess>-message_v1.
      <fs_log>-msgv2 = <fs_mess>-message_v2.
      <fs_log>-msgv3 = <fs_mess>-message_v3.
      <fs_log>-msgv4 = <fs_mess>-message_v4.

    ENDLOOP.

    log_merge( CHANGING c_log_tab = lt_log ).

    MODIFY zint_assina02 FROM TABLE lt_log.

    CHECK lt_log IS NOT INITIAL.

    log_contract_add( lt_log ).

    SUBTRACT 40 FROM lv_inidt.
    SUBTRACT 10 FROM lv_fimdt.

    DELETE FROM zint_assina02 WHERE msgdt BETWEEN lv_inidt AND lv_fimdt.

  ENDMETHOD.


  METHOD zif_integracao_adigital_get~lookup_values_execute.

    DATA lv_params TYPE string.

    CHECK i_lookup_id IS NOT INITIAL.

    lv_params = 'lookup_values/' && i_lookup_id.

    r_integracao = me->zif_integracao_adigital_get~execute_service( i_params = lv_params i_method = 'GET').


  ENDMETHOD.


  METHOD zif_integracao_adigital_get~process_contracts.

    " Recupera contratos
    me->zif_integracao_adigital_get~get_documents( ir_id_ref_range ).

    " envia para aprovação
    me->zif_integracao_adigital_get~send_to_approval( ).

    " atualiza log
    me->zif_integracao_adigital_get~log_update( ).

  ENDMETHOD.


  METHOD zif_integracao_adigital_get~send_to_approval.

    DATA lv_text TYPE string.

    DATA t_element_array  TYPE zde_element_array_t.
    DATA lw_anexos TYPE zins_adigital_attachments.

    r_ret  = me.

    LOOP AT me->zif_integracao_adigital_get~at_doc_tab-contracts-contract ASSIGNING FIELD-SYMBOL(<fs_documento>).

      gv_current_id = <fs_documento>-id.

      "IF sy-sysid NE 'DEV' AND sy-uname NE 'RBLIMA'.

      IF <fs_documento>-customfields-relacaoassinantes_old IS INITIAL.

        lv_text = `Documento: ` && <fs_documento>-id && ` não tem assinantes`.

        me->zif_integracao_adigital_get~set_message( i_message = lv_text  i_doc_id = <fs_documento>-id ).

        CONTINUE. "<- se nao tiver anexo, vai para o proximo

      ENDIF.
      "ELSE.
      "<fs_documento>-customfields-relacaoassinantes_old = '05332137974 - Ramon Lima - ramon.lima@reclike.com.br'.
      "ENDIF.

      TRY.

          DATA(lw_assina) = me->zif_integracao_adigital_get~send_to_destination(
            EXPORTING i_document = <fs_documento> it_anexos = me->zif_integracao_adigital_get~before_sending( <fs_documento>-id ) ).

          IF lw_assina-chave_coleta IS INITIAL.

            lv_text = `Documento: ` && <fs_documento>-id && ` não gerou chave no sistema destino`.

            me->zif_integracao_adigital_get~set_message( i_message = lv_text  i_doc_id = <fs_documento>-id ).

            CONTINUE. "<- se nao tiver anexo, vai para o proximo

          ELSE.

            me->zif_integracao_adigital_get~after_sending( lw_assina ).

            lv_text = `Documento: ` && <fs_documento>-id && ` enviado para coleta de assinaturas`.

            me->zif_integracao_adigital_get~set_message( EXPORTING i_message = lv_text i_msgty = 'S' i_doc_id = <fs_documento>-id ).

            COMMIT WORK AND WAIT.

          ENDIF.

        CATCH zcx_integracao INTO DATA(ex_int).
          me->zif_integracao_adigital_get~set_message( EXPORTING i_message = ex_int->get_longtext( preserve_newlines = 'X' ) i_doc_id = <fs_documento>-id  ).
          CONTINUE.

        CATCH zcx_error INTO DATA(ex_erro).
          me->zif_integracao_adigital_get~set_message( EXPORTING i_message = ex_erro->get_longtext( preserve_newlines = 'X' ) i_doc_id = <fs_documento>-id  ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_integracao_adigital_get~send_to_destination.

    DATA lo_envio TYPE REF TO zcl_integracao_bry_adigital.
    DATA lw_envio TYPE zins_dados_bry_dados.

    DATA lw_sucesso TYPE zins_dados_bry_json.
    DATA lw_erro TYPE zins_dados_bry_json_erro.

    DATA lv_response TYPE string.
    DATA lv_mess TYPE string.

    lw_envio-nomecoleta = i_document-name.

    IF sy-sysid = 'QAS'.
      lw_envio-nomecoleta = i_document-name && ` (enviado por ambiente de testes - sem validade)`.
    ENDIF.

    IF i_document-customfields-datalimiteassinaturas IS NOT INITIAL.
      lw_envio-datalimite = format_date_bry( i_document-customfields-datalimiteassinaturas ).
    ELSEIF i_document-enddate IS NOT INITIAL.
      lw_envio-datalimite = format_date_bry( i_document-enddate ).
    ELSE.
      lw_envio-datalimite = format_date_bry( '' ).
    ENDIF.

    lw_envio-descricao = i_document-name.
    lw_envio-padraoassinatura = 'PDF'.
    lw_envio-exigirdownload = i_document-customfields-exigirdownload_old.
    lw_envio-proibirrejeicao = i_document-customfields-proibirrejeio_old.
    lw_envio-assinaturasequencial = i_document-customfields-assinaturasequencial_old.
    lw_envio-agrupardocumentos = i_document-customfields-agrupardocumentos_old.
    lw_envio-local_assinatura = 'AUTOMATICA'.
    lw_envio-relacao_assinantes = i_document-customfields-relacaoassinantes_old.

    IF lw_envio-exigirdownload IS INITIAL.
      lw_envio-exigirdownload = 'false'.
    ENDIF.

    IF lw_envio-proibirrejeicao IS INITIAL.
      lw_envio-proibirrejeicao = 'false'.
    ENDIF.
    IF lw_envio-assinaturasequencial IS INITIAL.
      lw_envio-assinaturasequencial = 'false'.
    ENDIF.

    "IF lw_envio-agrupardocumentos IS INITIAL.
    "lw_envio-agrupardocumentos = 'false'.
    "ENDIF.

    lw_envio-agrupardocumentos = 'true'.

    lw_envio-codigoresponsavel = get_responsavel_assinatura( ).

    CREATE OBJECT lo_envio
      EXPORTING
        it_anexos     = it_anexos
        i_dados_envio = lw_envio.

    lv_response = lo_envio->zif_integracao_bry_adigital~enviar_bry( ).

    IF lv_response CS 'status'.

      /ui2/cl_json=>deserialize( EXPORTING json = lv_response
            CHANGING data = lw_erro ).

      zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).

    ELSE.

      /ui2/cl_json=>deserialize( EXPORTING json = lv_response
          CHANGING data = lw_sucesso ).

    ENDIF.

    IF lw_erro IS NOT INITIAL.

      "lv_mess = lw_erro-mensagens[ 1 ]-mensagem.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lw_erro-mensagens[ 1 ]-mensagem ).

    ENDIF.


    CHECK lw_sucesso IS NOT INITIAL.

    r_adigital-id_referencia = i_document-id.
    r_adigital-id_processo = '01'. "<- COUPA
    r_adigital-etapa = '01'. "<- ENVIADO AO BRY
    r_adigital-nome = i_document-name.
    r_adigital-chave_coleta = lw_sucesso-chaveworkflow.
    r_adigital-log_date = sy-datum.
    r_adigital-log_uzeit = sy-uzeit.
    r_adigital-log_name = sy-uname.

  ENDMETHOD.


  METHOD zif_integracao_adigital_get~set_message.

    DATA(lv_msgty) = i_msgty.

    CHECK i_message IS NOT INITIAL.

    IF lv_msgty IS INITIAL.
      lv_msgty = 'E'.
    ENDIF.

    APPEND INITIAL LINE TO me->zif_integracao_adigital_get~at_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_message>).

    DATA: lt_trtexts     TYPE trtexts,
          lw_trtexts     TYPE trtext,
          lv_texto(4000).

    DATA lv_msg1 TYPE sy-msgv1.
    DATA lv_msg2 TYPE sy-msgv1.
    DATA lv_msg3 TYPE sy-msgv1.
    DATA lv_msg4 TYPE sy-msgv1.

    lv_texto = zcl_string2=>remove_spec_char( i_message ).

    REPLACE ALL OCCURRENCES OF '&' IN lv_texto WITH space.

    CONDENSE lv_texto.

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
    <fs_message>-parameter = i_doc_id.

    MESSAGE ID <fs_message>-id TYPE <fs_message>-type NUMBER <fs_message>-number
      WITH <fs_message>-message_v1 <fs_message>-message_v2
           <fs_message>-message_v3 <fs_message>-message_v4 INTO <fs_message>-message.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_true.
  ENDMETHOD.


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
