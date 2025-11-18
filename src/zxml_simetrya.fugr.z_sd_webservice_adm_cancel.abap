FUNCTION z_sd_webservice_adm_cancel.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(XML) TYPE  STRING
*"  EXPORTING
*"     VALUE(XML_CANCELA) TYPE  STRING
*"     VALUE(VIAGEM_CANCELADA) TYPE  CHAR01
*"     VALUE(MENSAGEM) TYPE  STRING
*"     REFERENCE(URL_END) TYPE  STRING
*"  EXCEPTIONS
*"      HTTP_COMMUNICATION_FAILURE
*"      HTTP_INVALID_STATE
*"      HTTP_PROCESSING_FAILED
*"      HTTP_INVALID_TIMEOUT
*"      ERRO_WEB_SERVICE
*"----------------------------------------------------------------------

  DATA: url           TYPE string,
        http_client   TYPE REF TO if_http_client,
        return_code   TYPE i,
        v_tamanho     TYPE string,
        v_tamanhoi    TYPE i,
        xml_ret       TYPE REF TO cl_xml_document,
        xml_ret_itm   TYPE REF TO cl_xml_document,
        xml_node_root TYPE REF TO if_ixml_node,
        xml_node      TYPE REF TO if_ixml_node,
        xml_node_res  TYPE REF TO if_ixml_node,
        v_valor       TYPE string.

  CLEAR : viagem_cancelada.

*-US 140617-30.12.2024-#140617-JT-inicio
  TRY .
      xml_cancela = lc_integra_tip->set_integra_viagem( i_servico = 'CANCELAR_VIAGEM' i_json = xml ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E028' p_tipo = 'RESP'.
      MESSAGE e028 WITH url RAISING http_communication_failure.
    CATCH zcx_error INTO DATA(ex_error).
      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E028' p_tipo = 'RESP'.
      MESSAGE e028 WITH url RAISING http_communication_failure.
  ENDTRY.

*  SELECT SINGLE url INTO url FROM zciot_webservice WHERE tipo EQ 'E' AND servico EQ space.
*  "url = 'http://gvt.neus.com.br:8084/wsconvenio/resources/autorizacaoCancelamentoViagem'.
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
*  v_tamanhoi = strlen( xml ).
*  v_tamanho = v_tamanhoi.
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = 'Content-Length'
*      value = v_tamanho.
*  CALL METHOD http_client->request->set_cdata
*    EXPORTING
*      data   = xml
*      offset = 0
*      length = v_tamanhoi.
*
*  CALL METHOD http_client->send
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3
*      http_invalid_timeout       = 4.
*
*  CASE sy-subrc.
*    WHEN 1.
*      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E028' p_tipo = 'RESP'.
*      MESSAGE e028 WITH url RAISING http_communication_failure.
*    WHEN 2.
*      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E029' p_tipo = 'RESP'.
*      MESSAGE e029 WITH url RAISING http_invalid_state.
*    WHEN 3.
*      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E030' p_tipo = 'RESP'.
*      MESSAGE e030 WITH url RAISING http_processing_failed.
*    WHEN 4.
*      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E031' p_tipo = 'RESP'.
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
*      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E028' p_tipo = 'REQU'.
*      MESSAGE e028 WITH url RAISING http_communication_failure.
*    WHEN 2.
*      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E029' p_tipo = 'REQU'.
*      MESSAGE e029 WITH url RAISING http_invalid_state.
*    WHEN 3.
*      CALL FUNCTION 'Z_LOG_WEBSERVICE' EXPORTING p_url = url p_msg = 'E030' p_tipo = 'REQU'.
*      MESSAGE e030 WITH url RAISING http_processing_failed.
*  ENDCASE.
*
*  http_client->response->get_status( IMPORTING code = return_code ).
*
*  xml_cancela = http_client->response->get_cdata( ).
*
*  http_client->close( ).
*-US 140617-30.12.2024-#140617-JT-fim

  CREATE OBJECT xml_ret.

  CALL METHOD xml_ret->parse_string
    EXPORTING
      stream  = xml_cancela
    RECEIVING
      retcode = v_tamanhoi.

  CALL METHOD xml_ret->find_node
    EXPORTING
      name = 'codigoMensagem'
    RECEIVING
      node = xml_node.

  IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).

    CALL METHOD xml_node->get_value
      RECEIVING
        rval = v_valor.

    IF v_valor EQ '00000'.
      viagem_cancelada = 'X'.
    ENDIF.

  ENDIF.

  CALL METHOD xml_ret->find_node
    EXPORTING
      name = 'mensagem'
    RECEIVING
      node = xml_node.

  IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
    CALL METHOD xml_node->get_value
      RECEIVING
        rval = v_valor.

    mensagem = v_valor.
  ENDIF.

ENDFUNCTION.
