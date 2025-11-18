FUNCTION z_sd_webservice_adm_cred.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(XML) TYPE  STRING
*"  EXPORTING
*"     VALUE(XML_CREDITO) TYPE  STRING
*"     VALUE(VIAGEM_AUTORIZADA) TYPE  CHAR01
*"     VALUE(LINK_CONTRATO) TYPE  CLIKE
*"     VALUE(LINK_RESUMO) TYPE  CLIKE
*"     VALUE(LINK_PEDAGIO) TYPE  CLIKE
*"     VALUE(LINK_CARGA_PEDAGIO) TYPE  CLIKE
*"  EXCEPTIONS
*"      HTTP_COMMUNICATION_FAILURE
*"      HTTP_INVALID_STATE
*"      HTTP_PROCESSING_FAILED
*"      HTTP_INVALID_TIMEOUT
*"      ERRO_WEB_SERVICE
*"      ERRO_WS_CONVENIO
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
        tag_name      TYPE string,
        iterator      TYPE REF TO if_ixml_node_iterator,
        v_valor       TYPE string.

  "OBJETO PARA SALVAR O LOG DE ERRO.
  DATA: obj_util TYPE REF TO zcl_util.

  CREATE OBJECT obj_util.

  CLEAR : viagem_autorizada, link_contrato, link_resumo.

*-US 140617-30.12.2024-#140617-JT-inicio
  TRY .
      xml_credito = lc_integra_tip->set_integra_viagem( i_servico = 'AUTORIZAR_VIAGEM' i_json = xml ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
      MESSAGE e028 WITH url RAISING http_communication_failure.
    CATCH zcx_error INTO DATA(ex_error).
      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
      MESSAGE e028 WITH url RAISING http_communication_failure.
  ENDTRY.

*  SELECT SINGLE url INTO url FROM zciot_webservice WHERE tipo EQ 'F' AND servico EQ space.
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
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
*      MESSAGE e028 WITH url RAISING http_communication_failure.
*    WHEN 2.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E029' i_tipo = 'REQU' ).
*      MESSAGE e029 WITH url RAISING http_invalid_state.
*    WHEN 3.
*      obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E030' i_tipo = 'REQU' ).
*      MESSAGE e030 WITH url RAISING http_processing_failed.
*  ENDCASE.
*
*  http_client->response->get_status( IMPORTING code = return_code ).
*
*  xml_credito = http_client->response->get_cdata( ).
*
*  http_client->close( ).
*-US 140617-30.12.2024-#140617-JT-fim

  CREATE OBJECT xml_ret.

  CALL METHOD xml_ret->parse_string
    EXPORTING
      stream  = xml_credito
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
      viagem_autorizada = 'X'.
    ENDIF.
  ENDIF.

  IF viagem_autorizada EQ 'X'.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'linkContrato'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      link_contrato = v_valor.
    ENDIF.

*-#130491-04.04.2024-JT-inicio
    IF   link_contrato IS     INITIAL OR
       ( link_contrato IS NOT INITIAL AND link_contrato(4) <> 'http' ).
      CALL METHOD xml_ret->find_node
        EXPORTING
          name = 'contrato'
        RECEIVING
          node = xml_node.

      IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
        iterator = xml_node->create_iterator( ).
        xml_node = iterator->get_next( ).
        WHILE NOT xml_node IS INITIAL.
          CASE xml_node->get_type( ).
            WHEN: if_ixml_node=>co_node_element.
              tag_name = xml_node->get_name( ).
              IF tag_name EQ 'nome'.
                link_contrato = xml_node->get_value( ).
              ENDIF.
          ENDCASE.
          xml_node = iterator->get_next( ).
        ENDWHILE.
      ENDIF.
    ENDIF.
*-#130491-04.04.2024-JT-fim

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'linkDocumentoAux'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      link_resumo = v_valor.
    ENDIF.

*-#130491-04.04.2024-JT-inicio
    IF  link_resumo IS     INITIAL OR
      ( link_resumo IS NOT INITIAL AND link_resumo(4) <> 'http' ).
      CALL METHOD xml_ret->find_node
        EXPORTING
          name = 'documentoAux'
        RECEIVING
          node = xml_node.

      IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
        iterator = xml_node->create_iterator( ).
        xml_node = iterator->get_next( ).
        WHILE NOT xml_node IS INITIAL.
          CASE xml_node->get_type( ).
            WHEN: if_ixml_node=>co_node_element.
              tag_name = xml_node->get_name( ).
              IF tag_name EQ 'nome'.
                link_resumo = xml_node->get_value( ).
              ENDIF.
          ENDCASE.
          xml_node = iterator->get_next( ).
        ENDWHILE.
      ENDIF.
    ENDIF.
*-#130491-04.04.2024-JT-fim

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'linkComprovantePedagio'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      link_pedagio = v_valor.
    ENDIF.

*-#130491-04.04.2024-JT-inicio
    IF  link_pedagio IS     INITIAL OR
      ( link_pedagio IS NOT INITIAL AND link_pedagio(4) <> 'http' ).
      CALL METHOD xml_ret->find_node
        EXPORTING
          name = 'comprovantePedagio'
        RECEIVING
          node = xml_node.

      IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
        iterator = xml_node->create_iterator( ).
        xml_node = iterator->get_next( ).
        WHILE NOT xml_node IS INITIAL.
          CASE xml_node->get_type( ).
            WHEN: if_ixml_node=>co_node_element.
              tag_name = xml_node->get_name( ).
              IF tag_name EQ 'nome'.
                link_pedagio = xml_node->get_value( ).
              ENDIF.
          ENDCASE.
          xml_node = iterator->get_next( ).
        ENDWHILE.
      ENDIF.
    ENDIF.
*-#130491-04.04.2024-JT-fim

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'linkCargaVisaPedagio'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.

      link_carga_pedagio = v_valor.
    ENDIF.

  ELSE.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'mensagem'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.

      MESSAGE v_valor TYPE 'E' RAISING erro_ws_convenio.

    ENDIF.

  ENDIF.

ENDFUNCTION.
