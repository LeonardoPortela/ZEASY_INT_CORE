class ZCL_WEBSERVICE_TRACE definition
  public
  create public .

*"* public components of class ZCL_WEBSERVICE_TRACE
*"* do not include other source files here!!!
public section.

  types:
    BEGIN OF TY_OAUTH,
        LOGIN        TYPE STRING,
        ACCESS_TOKEN TYPE STRING,
      END OF TY_OAUTH .

  methods ATUALIZA_TRACE
    importing
      !T_FARDO type ZPMT0059
      !I_JSON type STRING optional
      !ID_REFERENCIA type CHAR11 optional
    returning
      value(RET_CODE) type I .
  methods AUTHENTICATION
    returning
      value(RESULT) type TY_OAUTH .
  methods ENVIA_INTEGRACAO
    importing
      !E_CLIENT type ref to CL_HTTP_CLIENT
      !ID_REFERENCIA type CHAR11
      !ID_INTERFACE type ZDE_ID_INTERFACE optional .
  PROTECTED SECTION.
*"* protected components of class ZCL_WEBSERVICE_TRACE
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_WEBSERVICE_TRACE IMPLEMENTATION.


  METHOD atualiza_trace.

    DATA: v_author TYPE ty_oauth.

    DATA: lv_json TYPE string.

    v_author = authentication( ).

    IF v_author-access_token  IS NOT INITIAL.
      "//Call service
      SELECT SINGLE *
         FROM zauth_webservice INTO @DATA(ws_service)
        WHERE service = 'ENVIA_TRACE_ATUALIZA'.

      IF sy-subrc NE 0.
        ret_code = '001'.
        EXIT.
      ENDIF.

      DATA(_url) =  ws_service-url.
      CALL METHOD cl_http_client=>create_by_url
        EXPORTING
          url                = CONV #( _url )
        IMPORTING
          client             = DATA(http_client)
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4.

      CALL METHOD http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.

      CALL METHOD http_client->request->set_header_field
        EXPORTING
          name  = '~server_protocol'
          value = 'HTTP/1.1'.

      CONCATENATE 'Bearer' v_author-access_token INTO v_author-access_token SEPARATED BY space.

      CALL METHOD http_client->request->set_header_field
        EXPORTING
          name  = 'Authorization'
          value = v_author-access_token.


      CALL METHOD http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'application/json'.

*-#133288-05.02.2024-JT-inicio
      IF i_json IS NOT INITIAL.   "
        lv_json = i_json.
      ELSE.
        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data   = t_fardo
          RECEIVING
            r_json = lv_json.
      ENDIF.

      CALL METHOD http_client->request->set_cdata
        EXPORTING
          data = lv_json.
*-#133288-05.02.2024-JT-fim

      http_client->send( ).

      CALL METHOD http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.

      http_client->response->get_status( IMPORTING code = ret_code ).

*      IF ret_code EQ 200.

*        CALL FUNCTION 'NUMBER_GET_NEXT'
*          EXPORTING
*            nr_range_nr             = '01'
*            object                  = 'ZSEQ_RECEB'
*          IMPORTING
*            number                  = id_referencia
*          EXCEPTIONS
*            interval_not_found      = 1
*            number_range_not_intern = 2
*            object_not_found        = 3
*            quantity_is_0           = 4
*            quantity_is_not_1       = 5
*            interval_overflow       = 6
*            buffer_overflow         = 7
*            OTHERS                  = 8.
*      ELSE.
**        id_referencia = '999999999'.
*      ENDIF.

      CALL METHOD me->envia_integracao
        EXPORTING
          e_client      = CAST #( http_client )
          id_referencia = id_referencia.

      http_client->close( ).

    ENDIF.

  ENDMETHOD.


  METHOD AUTHENTICATION.


    DATA: LV_JSON     TYPE STRING,
          RETURN_CODE TYPE I.

    SELECT SINGLE *
          FROM ZAUTH_WEBSERVICE INTO @DATA(WS_SERVICE)
         WHERE SERVICE = 'ENVIA_TRACE_TOKEN'.

    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.

    LV_JSON = '{ "username": '.
    CONCATENATE LV_JSON '"' WS_SERVICE-USERNAME '","password": "' WS_SERVICE-PASSWORD '"}' INTO LV_JSON.

    DATA(_URL) =  WS_SERVICE-URL.
    "//Call service
    CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
      EXPORTING
        URL                = CONV #( _URL )
      IMPORTING
        CLIENT             = DATA(HTTP_CLIENT)
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~request_method'
        VALUE = 'POST'.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~server_protocol'
        VALUE = 'HTTP/1.1'.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Content-Type'
        VALUE = 'application/json'.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_CDATA
      EXPORTING
        DATA = LV_JSON.



    HTTP_CLIENT->SEND( ).

    CALL METHOD HTTP_CLIENT->RECEIVE
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        OTHERS                     = 4.

    DATA(_RESULT) = HTTP_CLIENT->RESPONSE->GET_CDATA( ).

    HTTP_CLIENT->RESPONSE->GET_STATUS( IMPORTING CODE = RETURN_CODE ).

    DATA(_JSON_DESERIALIZER) = NEW CL_TREX_JSON_DESERIALIZER( ).

    IF RETURN_CODE EQ '200'.
      /UI2/CL_JSON=>DESERIALIZE(
        EXPORTING
          JSON        = _RESULT
        CHANGING
          DATA        = RESULT
      ).
    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD envia_integracao.

    DATA: l_id     TYPE zde_id_integracao.
    DATA: w_integracao     TYPE zintegracao,
          w_integracao_log TYPE zintegracao_log.

    CLEAR: w_integracao,
           w_integracao_log.

    DATA(i_resultado)       = e_client->request->get_cdata( ).
    DATA(i_url)             = e_client->request->get_header_field( '~request_uri' ).
    DATA(i_metodo)          = e_client->request->get_header_field( '~request_method' ).
    DATA(i_content_type)    = e_client->request->get_header_field( 'Content-Type' ).
    DATA(i_host)            = e_client->request->get_header_field( 'host' ).
    DATA(i_server_protocol) = e_client->request->get_header_field( '~server_protocol').
    DATA(i_content_length)  = e_client->request->get_header_field( 'content-length').

    e_client->response->get_status( IMPORTING code = DATA(e_status) ).

    DATA(e_resultado)       = e_client->response->get_cdata( ).

    CLEAR w_integracao.

    zcl_integracao=>zif_integracao~get_instance( )->get_new_id_integracao(
        IMPORTING e_id_integracao = w_integracao-id_integracao ).

    w_integracao-id_interface         = COND #( WHEN id_interface IS NOT INITIAL THEN id_interface ELSE '034' ).
    w_integracao-id_referencia        = id_referencia.
    w_integracao-dt_registro          = sy-datum.
    w_integracao-hr_registro          = sy-uzeit.
    w_integracao-us_registro          = sy-uname.

    w_integracao-ck_integrado         = abap_true.
    w_integracao-dt_integrado         = sy-datum.
    w_integracao-hr_integrado         = sy-uzeit.
    w_integracao-us_integrado         = sy-uname.
    w_integracao-ds_url               = |http://{ i_host }{ i_url }|.
    w_integracao-ds_body              = i_resultado.
    w_integracao-ds_metodo            = i_metodo.
    w_integracao-ds_content_type      = i_content_type.
    w_integracao-ds_formato           = 'JSON'.
    w_integracao-ds_funcao_processa   = i_url.

    CONCATENATE  '['
                    '{"name":"~request_uri",    "value":' i_url             '},'
                    '{"name":"~request_method", "value":' i_metodo          '},'
                    '{"name":"~server_protocol","value":' i_server_protocol '},'
                    '{"name":"content-type",    "value":' i_content_type    '},'
                    '{"name":"content-length",  "value":' i_content_length  '}'
                 ']'
           INTO w_integracao-ds_header.

    w_integracao-nm_code              = e_status.
    w_integracao-ds_data_retorno      = e_resultado.
    w_integracao-tp_integracao        = '0'.
    w_integracao-tp_sincronia         = '1'.
    w_integracao-ck_retornou          = abap_true.
    w_integracao-ck_processado        = abap_true.
    w_integracao-dt_processado        = sy-datum.
    w_integracao-hr_processado        = sy-uzeit.
    w_integracao-us_processado        = sy-uname.
    MODIFY zintegracao             FROM w_integracao.

    CLEAR w_integracao_log.

    w_integracao_log-mandt            = sy-mandt.
    w_integracao_log-id_integracao    = w_integracao-id_integracao.
    w_integracao_log-dt_registro      = sy-datum.
    w_integracao_log-hr_registro      = sy-uzeit.
    w_integracao_log-us_registro      = sy-uname.
    w_integracao_log-nm_code          = e_status.
    w_integracao_log-ds_data_retorno  = e_resultado.
    w_integracao_log-dt_resposta      = sy-datum.
    w_integracao_log-hr_resposta      = sy-uzeit.
    w_integracao_log-us_resposta      = sy-uname.
    MODIFY zintegracao_log         FROM w_integracao_log.

  ENDMETHOD.
ENDCLASS.
