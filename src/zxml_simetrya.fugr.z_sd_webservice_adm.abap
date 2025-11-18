FUNCTION z_sd_webservice_adm.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(XML) TYPE  STRING
*"     REFERENCE(P_ST_CIOT) TYPE  ZST_CIOT DEFAULT '0'
*"  EXPORTING
*"     VALUE(P_ST_CIOT_R) TYPE  ZST_CIOT
*"     VALUE(XML_PROTOCOLO) TYPE  STRING
*"     VALUE(CD_PROTOCOLO) TYPE  ZPROTOCOLO
*"     VALUE(CNPJ_CONTRATANTE) TYPE  STCD1
*"     VALUE(ID_VIAGEM) TYPE  ZIDVIAGEMSOLI
*"     VALUE(DS_MSG) TYPE  STRING
*"     VALUE(DT_SOLICITACAO) TYPE  J_1BCREDAT
*"     VALUE(HR_SOLICITACAO) TYPE  J_1BCRETIM
*"     VALUE(DT_RETORNO) TYPE  J_1BCREDAT
*"     VALUE(HR_RETORNO) TYPE  J_1BCRETIM
*"     REFERENCE(URL_END) TYPE  STRING
*"  TABLES
*"      IT_RETORNO_CONTRATO STRUCTURE  ZCONTRATO_CIOT_T OPTIONAL
*"      IT_OBS STRUCTURE  ZCTE_VIAGEM_OBS OPTIONAL
*"  EXCEPTIONS
*"      HTTP_COMMUNICATION_FAILURE
*"      HTTP_INVALID_STATE
*"      HTTP_PROCESSING_FAILED
*"      HTTP_INVALID_TIMEOUT
*"      ERRO_WEB_SERVICE
*"      ERRO_XML_SOLICITA
*"----------------------------------------------------------------------

  DATA: url                 TYPE string,
        http_client         TYPE REF TO if_http_client,
        return_code         TYPE i,
        v_tamanho           TYPE string,
        v_tamanhoi          TYPE i,
        xml_ret             TYPE REF TO cl_xml_document,
        xml_ret_itm         TYPE REF TO cl_xml_document,
        xml_node_root       TYPE REF TO if_ixml_node,
        xml_node            TYPE REF TO if_ixml_node,
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
        wa_obs              TYPE zcte_viagem_obs.

  "OBJETO PARA SALVAR O LOG DE ERRO.
  DATA: obj_util TYPE REF TO zcl_util.

  IF p_st_ciot EQ '0'.

    "CIAR OBJETO PARA CASO TENHA ALGUM EXCEPTION.
    CREATE OBJECT obj_util.

    CLEAR: xml_protocolo, url_end.

*-US 140617-30.12.2024-#140617-JT-inicio
    TRY .
        xml_protocolo = lc_integra_tip->set_integra_viagem( EXPORTING i_servico = 'CADASTRAR_VIAGEM' i_json = xml
                                                            IMPORTING e_url     = url_end ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
        MESSAGE e028 WITH url RAISING http_communication_failure.
      CATCH zcx_error INTO DATA(ex_error).
        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
        MESSAGE e028 WITH url RAISING http_communication_failure.
    ENDTRY.

*    SELECT SINGLE url INTO url FROM zciot_webservice WHERE tipo EQ 'I' AND servico EQ space.
*
*    IF ( sy-subrc EQ 0 ).
*      url_end = url.
*    ENDIF.
*
*    "url = 'http://gvt.neus.com.br:8084/wsconvenio/resources/cadastroViagem'.
*    cl_http_client=>create_by_url( EXPORTING url = url IMPORTING client = http_client ).
*
*    CALL METHOD http_client->request->set_header_field
*      EXPORTING
*        name  = '~request_method'
*        value = 'POST'.
*    CALL METHOD http_client->request->set_header_field
*      EXPORTING
*        name  = '~server_protocol'
*        value = 'HTTP/1.1'.
*    CALL METHOD http_client->request->set_header_field
*      EXPORTING
*        name  = 'Content-Type'
*        value = 'application/xml; charset=UTF-8'.
*
*    v_tamanhoi = strlen( xml ).
*    v_tamanho = v_tamanhoi.
*
*    CALL METHOD http_client->request->set_header_field
*      EXPORTING
*        name  = 'Content-Length'
*        value = v_tamanho.
*    CALL METHOD http_client->request->set_cdata
*      EXPORTING
*        data   = xml
*        offset = 0
*        length = v_tamanhoi.
*    CALL METHOD http_client->send
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*        http_invalid_timeout       = 4.
*
*    CASE sy-subrc.
*      WHEN 1.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
*        MESSAGE e028 WITH url RAISING http_communication_failure.
*      WHEN 2.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E029' i_tipo = 'REQU' ).
*        MESSAGE e029 WITH url RAISING http_invalid_state.
*      WHEN 3.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E030' i_tipo = 'REQU' ).
*        MESSAGE e030 WITH url RAISING http_processing_failed.
*      WHEN 4.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E031' i_tipo = 'REQU' ).
*        MESSAGE e031 WITH url RAISING http_invalid_timeout.
*    ENDCASE.
*
*    CALL METHOD http_client->receive
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3.
*
*    CASE sy-subrc.
*      WHEN 1.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'RESP' ).
*        MESSAGE e028 WITH url RAISING http_communication_failure.
*      WHEN 2.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E029' i_tipo = 'RESP' ).
*        MESSAGE e029 WITH url RAISING http_invalid_state.
*      WHEN 3.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E030' i_tipo = 'RESP' ).
*        MESSAGE e030 WITH url RAISING http_processing_failed.
*    ENDCASE.
*
*    http_client->response->get_status( IMPORTING code = return_code ).
*
*    xml_protocolo = http_client->response->get_cdata( ).
*
*    http_client->close( ).
*-US 140617-30.12.2024-#140617-JT-fim

    CREATE OBJECT xml_ret.

    CALL METHOD xml_ret->parse_string
      EXPORTING
        stream  = xml_protocolo
      RECEIVING
        retcode = v_tamanhoi.
    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'numeroProtocolo'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).

      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_valor
        IMPORTING
          output = cd_protocolo.

    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'cnpjContratante'
      RECEIVING
        node = xml_node.
    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_valor
        IMPORTING
          output = cnpj_contratante.
    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'idSolicitacaoContratante'
      RECEIVING
        node = xml_node.
    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_valor
        IMPORTING
          output = id_viagem.
    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'msg'
      RECEIVING
        node = xml_node.
    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = ds_msg.
    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'dataChegadaSolicitacao'
      RECEIVING
        node = xml_node.
    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      v_data = v_valor(8).
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_INPUT'
        EXPORTING
          input  = v_data
        IMPORTING
          output = dt_solicitacao.
      hr_solicitacao   = v_valor+8(6).
    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'dataRetornoSolicitacao'
      RECEIVING
        node = xml_node.
    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      v_data = v_valor(8).
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_INPUT'
        EXPORTING
          input  = v_data
        IMPORTING
          output = dt_retorno.
      hr_retorno = v_valor+8(6).
    ENDIF.

*    -----------
*<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
*<retornoSolicitacaoCIOT>
*    <numeroProtocolo>2015102920024654</numeroProtocolo>
*    <cnpjContratante>77294254001670</cnpjContratante>
*    <idSolicitacaoContratante>00069124520001001140</idSolicitacaoContratante>
*    <msg>OK</msg>
*    <dataChegadaSolicitacao>30102015082045</dataChegadaSolicitacao>
*    <dataRetornoSolicitacao>30102015093747</dataRetornoSolicitacao>
*    <observacoes>
*        <observacao>[WSC-00071] O MOTORISTA NAO ESTA ASSOCIADO A UM CARTAO FRETE.</observacao>
*        <observacao>[WSC-00072] O MOTORISTA NAO ESTA ASSOCIADO A UM CARTAO PEDAGIO.</observacao>
*        <observacao>[WSC-00097] O MOTORISTA NAO ESTA ASSOCIADO A UM CARTAO FRETE BANDEIRA.</observacao>
*    </observacoes>
*</retornoSolicitacaoCIOT>
    "Leitura das Observações
    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'observacoes'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      iterator = xml_node->create_iterator( ).
      xml_node = iterator->get_next( ).
      WHILE NOT xml_node IS INITIAL.
        CASE xml_node->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            tag_name = xml_node->get_name( ).
            IF tag_name EQ 'observacao'.
              tag_name = xml_node->get_value( ).
              MOVE tag_name TO wa_obs-ds_protocolo.
              APPEND wa_obs TO it_obs.
            ENDIF.
        ENDCASE.
        xml_node = iterator->get_next( ).
      ENDWHILE.
    ENDIF.

    IF ds_msg NE 'OK'.

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

  ELSE.

    CLEAR: xml_protocolo.

*-US 140617-30.12.2024-#140617-JT-inicio
    TRY .
        xml_protocolo = lc_integra_tip->set_integra_viagem( i_servico = 'CONSULTAR_VIAGEM' i_json = xml ).

      CATCH zcx_integracao INTO ex_integra.
        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
        MESSAGE e028 WITH url RAISING http_communication_failure.
      CATCH zcx_error INTO ex_error.
        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
        MESSAGE e028 WITH url RAISING http_communication_failure.
    ENDTRY.

*    SELECT SINGLE url INTO url FROM zciot_webservice WHERE tipo EQ 'C' AND servico EQ space.
*
*    "url = 'http://gvt.neus.com.br:8084/wsconvenio/resources/consultaResgateViagem'.
*    cl_http_client=>create_by_url( EXPORTING url = url IMPORTING client = http_client ).
*
*    CALL METHOD http_client->request->set_header_field
*      EXPORTING
*        name  = '~request_method'
*        value = 'POST'.
*    CALL METHOD http_client->request->set_header_field
*      EXPORTING
*        name  = '~server_protocol'
*        value = 'HTTP/1.1'.
*    CALL METHOD http_client->request->set_header_field
*      EXPORTING
*        name  = 'Content-Type'
*        value = 'application/xml; charset=UTF-8'.
*    v_tamanhoi = strlen( xml ).
*    v_tamanho = v_tamanhoi.
*    CALL METHOD http_client->request->set_header_field
*      EXPORTING
*        name  = 'Content-Length'
*        value = v_tamanho.
*    CALL METHOD http_client->request->set_cdata
*      EXPORTING
*        data   = xml
*        offset = 0
*        length = v_tamanhoi.
*
*    CALL METHOD http_client->send
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*        http_invalid_timeout       = 4.
*
*    CASE sy-subrc.
*      WHEN 1.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'REQU' ).
*        MESSAGE e028 WITH url RAISING http_communication_failure.
*      WHEN 2.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E029' i_tipo = 'REQU' ).
*        MESSAGE e029 WITH url RAISING http_invalid_state.
*      WHEN 3.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E030' i_tipo = 'REQU' ).
*        MESSAGE e030 WITH url RAISING http_processing_failed.
*      WHEN 4.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E031' i_tipo = 'REQU' ).
*        MESSAGE e031 WITH url RAISING http_invalid_timeout.
*    ENDCASE.
*
*    CALL METHOD http_client->receive
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3.
*
*    CASE sy-subrc.
*      WHEN 1.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E028' i_tipo = 'RESP' ).
*        MESSAGE e028 WITH url RAISING http_communication_failure.
*      WHEN 2.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E029' i_tipo = 'RESP' ).
*        MESSAGE e029 WITH url RAISING http_invalid_state.
*      WHEN 3.
*        obj_util->ping_tipcard( EXPORTING i_url  = url i_cod  = 'E030' i_tipo = 'RESP' ).
*        MESSAGE e030 WITH url RAISING http_processing_failed.
*    ENDCASE.
*
*    http_client->response->get_status( IMPORTING code = return_code ).
*
*    xml_protocolo = http_client->response->get_cdata( ).
*
*    http_client->close( ).
*-US 140617-30.12.2024-#140617-JT-fim

    CREATE OBJECT xml_ret.

    CALL METHOD xml_ret->parse_string
      EXPORTING
        stream  = xml_protocolo
      RECEIVING
        retcode = v_tamanhoi.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'idOperacaoViagem'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).

      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.

      id_op_viagem_adm = v_valor.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = id_op_viagem_adm
        IMPORTING
          output = id_op_viagem_adm.

    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'dataChegadaViagem'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      v_data = v_valor(8).
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_INPUT'
        EXPORTING
          input  = v_data
        IMPORTING
          output = dt_solicitacao.
      hr_solicitacao   = v_valor+8(6).
    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'dataRetornoViagem'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = v_valor.
      v_data = v_valor(8).
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_INPUT'
        EXPORTING
          input  = v_data
        IMPORTING
          output = dt_retorno.
      hr_retorno = v_valor+8(6).
    ENDIF.

*0  Pendente
*1  Enviado
*2  Autorizado
*3  Rejeitado
*4  Enviado Aut. Credito
*5  Creditado
*6  Fechado (Pago Cockpit)
*7  Enviado Cancelamento
*8  Cancelado

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'msg'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = ds_msg.
    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'statusOperacao'
      RECEIVING
        node = xml_node.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
      CALL METHOD xml_node->get_value
        RECEIVING
          rval = ds_status.
      IF ds_status EQ 'PENDENTE'.
        p_st_ciot_r = '2'. "Autorizada a Emissão CT-e (Pendêncte NEUS)
      ELSEIF ds_status EQ 'AUTORIZADO'.
        p_st_ciot_r = '2'.
      ELSEIF ds_status EQ 'CREDITADO'.
        p_st_ciot_r = '5'.
      ELSEIF ds_status EQ 'CANCELADO'.
        p_st_ciot_r = '8'.
      ELSEIF ( ds_status EQ 'OUTROS' ) OR ( ds_status EQ 'ERRO' ).
        p_st_ciot_r = '3'.
      ENDIF.
    ENDIF.

    CALL METHOD xml_ret->find_node
      EXPORTING
        name = 'retornoItens'
      RECEIVING
        node = xml_node_root.

    IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node_root IS INITIAL ) AND ( p_st_ciot_r NE '3' )
        AND ( p_st_ciot_r NE '8' ).
      CALL METHOD xml_ret->find_node
        EXPORTING
          name = 'retornoItem'
          root = xml_node_root
        RECEIVING
          node = xml_node.

      IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).

        CLEAR: wa_retorno_contrato.

        wa_retorno_contrato-id_op_viagem_adm = id_op_viagem_adm.

        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'nuContrato'
          RECEIVING
            node = xml_node_res.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node_res IS INITIAL ).
          CALL METHOD xml_node_res->get_value
            RECEIVING
              rval = wa_retorno_contrato-nucontrato.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_retorno_contrato-nucontrato
            IMPORTING
              output = wa_retorno_contrato-nucontrato.
        ENDIF.

        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'rntrcContratado'
          RECEIVING
            node = xml_node_res.
        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node_res IS INITIAL ).
          CALL METHOD xml_node_res->get_value
            RECEIVING
              rval = wa_retorno_contrato-rntrccontratado.
        ENDIF.

        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'tipoContratado'
          RECEIVING
            node = xml_node_res.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node_res IS INITIAL ).
          CALL METHOD xml_node_res->get_value
            RECEIVING
              rval = wa_retorno_contrato-tipocontratado.
        ENDIF.

        IF wa_retorno_contrato-tipocontratado IS INITIAL.
          ds_msg = '(WSC) Não foi retornado o Tipo do Contratado!'.
        ENDIF.

        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'numeroCIOT'
          RECEIVING
            node = xml_node_res.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node_res IS INITIAL ).
          CALL METHOD xml_node_res->get_value
            RECEIVING
              rval = wa_retorno_contrato-numerociot.
        ENDIF.

        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'cartaoBandeira'
          RECEIVING
            node = xml_node_res.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node_res IS INITIAL ).
          CALL METHOD xml_node_res->get_value
            RECEIVING
              rval = wa_retorno_contrato-cartaobandeira.
        ENDIF.

        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'tipoPagamentoContrato'
          RECEIVING
            node = xml_node_res.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node_res IS INITIAL ).
          CALL METHOD xml_node_res->get_value
            RECEIVING
              rval = wa_retorno_contrato-tipopagamentocontrato.
        ENDIF.

        APPEND wa_retorno_contrato TO it_retorno_contrato.
      ELSE.
        p_st_ciot_r = '3'.
      ENDIF.
    ELSE.
      p_st_ciot_r = '3'.
    ENDIF.

  ENDIF.

ENDFUNCTION.
