FUNCTION z_sd_webservice_adm_file_pdf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(XML) TYPE  STRING
*"     VALUE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_PDF_FILE) TYPE  ZDE_DATA_XSTRING
*"  EXCEPTIONS
*"      HTTP_COMMUNICATION_FAILURE
*"      HTTP_INVALID_STATE
*"      HTTP_PROCESSING_FAILED
*"      HTTP_INVALID_TIMEOUT
*"      ERRO_WEB_SERVICE
*"      ERRO_XML_SOLICITA
*"----------------------------------------------------------------------
*{   INSERT         DEVK9A1XQT                                        1

  DATA: url                 TYPE string,
        http_client         TYPE REF TO if_http_client,
        return_code         TYPE i,
        v_tamanho           TYPE string,
        v_tamanhoi          TYPE i,
        xml_ret             TYPE REF TO cl_xml_document,
        xml_ret_itm         TYPE REF TO cl_xml_document,
        xml_node_root       TYPE REF TO if_ixml_node,
        xml_node            TYPE REF TO if_ixml_node,
        xml_protocolo       TYPE string,
        ds_msg              TYPE string,
        tag_name            TYPE string,
        iterator            TYPE REF TO if_ixml_node_iterator,
        xml_node_res        TYPE REF TO if_ixml_node,
        v_valor             TYPE string,
        ds_status           TYPE string,
        v_data              TYPE c LENGTH 8,
        id_op_viagem_adm    TYPE zidoperacaoviagem,
        wa_retorno_contrato TYPE zcontrato_ciot_t,
        msg_var1            LIKE balm-msgv1,
        msg_var2            LIKE balm-msgv2,
        msg_var3            LIKE balm-msgv3,
        msg_var4            LIKE balm-msgv4,
        wa_setleaf          TYPE setleaf,
        xml_var             TYPE zexml-xml,
        l_pdf_string        TYPE string,
        wa_obs              TYPE zcte_viagem_obs.

  FREE: l_pdf_string, e_pdf_file.

  "OBJETO PARA SALVAR O LOG DE ERRO.
  DATA: obj_util TYPE REF TO zcl_util.

  "CIAR OBJETO PARA CASO TENHA ALGUM EXCEPTION.
  CREATE OBJECT obj_util.

*-US 140617-30.12.2024-#140617-JT-inicio
  IF lc_integra_tip IS NOT BOUND.
    CREATE OBJECT lc_integra_tip.
  ENDIF.
  IF i_docnum IS NOT INITIAL.
    lc_integra_tip->set_referencia( CONV #( i_docnum ) ).
  ENDIF.
*-US 140617-30.12.2024-#140617-JT-fim

*-US 140617-30.12.2024-#140617-JT-inicio
  TRY .
      xml_protocolo = lc_integra_tip->set_integra_viagem( i_servico = 'CONSULTAR_PDF_VIAGEM' i_json = xml ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'RESP' ).
      MESSAGE e028 WITH url RAISING http_communication_failure.
    CATCH zcx_error INTO DATA(ex_error).
      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'RESP' ).
      MESSAGE e028 WITH url RAISING http_communication_failure.
  ENDTRY.

*  SELECT SINGLE url
*    INTO url
*    FROM zciot_webservice
*   WHERE tipo    EQ 'H'
*     AND servico EQ 'VI'.
*
*  cl_http_client=>create_by_url( EXPORTING url = url IMPORTING client = http_client ).
*
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = '~request_method'
*      value = 'POST'.
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = '~server_protocol'
*      value = 'HTTP/1.1'.
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = 'Content-Type'
*      value = 'application/xml; charset=UTF-8'.
*
*  v_tamanhoi = strlen( xml ).
*  v_tamanho = v_tamanhoi.
*
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = 'Content-Length'
*      value = v_tamanho.
*  CALL METHOD http_client->request->set_cdata
*    EXPORTING
*      data   = xml
*      offset = 0
*      length = v_tamanhoi.
*  CALL METHOD http_client->send
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3
*      http_invalid_timeout       = 4.
*
*  CASE sy-subrc.
*    WHEN 1.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
*      MESSAGE e028 WITH url RAISING http_communication_failure.
*    WHEN 2.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E029' i_tipo = 'REQU' ).
*      MESSAGE e029 WITH url RAISING http_invalid_state.
*    WHEN 3.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E030' i_tipo = 'REQU' ).
*      MESSAGE e030 WITH url RAISING http_processing_failed.
*    WHEN 4.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E031' i_tipo = 'REQU' ).
*      MESSAGE e031 WITH url RAISING http_invalid_timeout.
*  ENDCASE.
*
*  CALL METHOD http_client->receive
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3.
*
*  CASE sy-subrc.
*    WHEN 1.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'RESP' ).
*      MESSAGE e028 WITH url RAISING http_communication_failure.
*    WHEN 2.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E029' i_tipo = 'RESP' ).
*      MESSAGE e029 WITH url RAISING http_invalid_state.
*    WHEN 3.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E030' i_tipo = 'RESP' ).
*      MESSAGE e030 WITH url RAISING http_processing_failed.
*  ENDCASE.
*
*  http_client->response->get_status( IMPORTING code = return_code ).
*
*  xml_protocolo = http_client->response->get_cdata( ).
*
*  http_client->close( ).
*-US 140617-30.12.2024-#140617-JT-fim

  CREATE OBJECT xml_ret.

  CALL METHOD xml_ret->parse_string
    EXPORTING
      stream  = xml_protocolo
    RECEIVING
      retcode = v_tamanhoi.

  CALL METHOD xml_ret->find_node
    EXPORTING
      name = 'mensagem'
    RECEIVING
      node = xml_node.

  IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
    CALL METHOD xml_node->get_value
      RECEIVING
        rval = ds_msg.
  ENDIF.

  IF ds_msg NE 'SUCESSO'.
    CALL FUNCTION 'ZMESSAGE_PREPARE'
      EXPORTING
        msg_completa = space
      CHANGING
        msg_text     = ds_msg
        msg_var1     = msg_var1
        msg_var2     = msg_var2
        msg_var3     = msg_var3
        msg_var4     = msg_var4.

    MESSAGE e023 WITH msg_var1 msg_var2 msg_var3 msg_var4 RAISING erro_xml_solicita.
  ENDIF.

  "Leitura da tag arquivo
  CALL METHOD xml_ret->find_node
    EXPORTING
      name = 'arquivo'
    RECEIVING
      node = xml_node.

  IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
    iterator = xml_node->create_iterator( ).
    xml_node = iterator->get_next( ).
    WHILE NOT xml_node IS INITIAL.
      CASE xml_node->get_type( ).
        WHEN: if_ixml_node=>co_node_element.
          tag_name = xml_node->get_name( ).
          IF tag_name EQ 'dados'.
            l_pdf_string = xml_node->get_value( ).
          ENDIF.
      ENDCASE.
      xml_node = iterator->get_next( ).
    ENDWHILE.
  ENDIF.

  CHECK l_pdf_string IS NOT INITIAL.

  CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
    EXPORTING
      input  = l_pdf_string
    IMPORTING
      output = e_pdf_file
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

ENDFUNCTION.
