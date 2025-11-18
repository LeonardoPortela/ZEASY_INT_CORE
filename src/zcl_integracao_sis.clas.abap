CLASS zcl_integracao_sis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_sis .

    METHODS constructor
      IMPORTING
        VALUE(i_servico)    TYPE /ui2/service_name OPTIONAL
        VALUE(i_req)        TYPE zmmc_dados_int_coupa_eban OPTIONAL
        VALUE(i_parametros) TYPE zhcme_py_0004 OPTIONAL
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_SIS IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_sis~set_servico( i_servico = i_servico ).
    me->zif_integracao_sis~set_parametros( i_parametros = i_parametros ).

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_sis.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    IF i_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_sis~at_auth_ws
          WHERE service = i_servico.

    ENDIF.

    SELECT SINGLE *
      FROM zauth_webservice
      INTO me->zif_integracao_sis~at_token_ws
        WHERE service = 'SIS_TOKEN'.

    IF me->zif_integracao_sis~at_auth_ws  IS INITIAL OR
       me->zif_integracao_sis~at_token_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_form_request_http.
    r_if_integracao_inject = me.
    e_form_fields = me->zif_integracao_inject~at_form_fields.
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

    TRY .
        CAST zcl_integracao_token_sis(
               zcl_integracao_token_sis=>zif_integracao_token_sis~get_instance(
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

  ENDMETHOD.


  METHOD zif_integracao_inject~set_form_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields = i_form_fields.
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


  METHOD zif_integracao_inject~set_parametro.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_sis~get_empresas_sis.


*// Tipos de Dados
    TYPES: BEGIN OF ty_token,
             response TYPE zmme0006_t,
           END OF ty_token.

    DATA: zmme0006    TYPE TABLE OF zmme0006,
          retorno     TYPE ty_token,
          it_zmmt0165 TYPE TABLE OF zmmt0165,
          wa_zmmt0165 TYPE zmmt0165.


    me->zif_integracao_inject~at_referencia =
    VALUE #(
              tp_referencia = 'SIS - Empresas'
    ).

    me->zif_integracao_sis~set_ds_url(
           )->set_ds_data(
           )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao) e_integracao = e_integracao ).

    BREAK-POINT.

    IF e_integracao-nm_code EQ 200.

      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json        = e_integracao-ds_data_retorno
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data        = retorno.

      SELECT *
        FROM zmmt0165
        INTO TABLE @DATA(it_empresas).

      LOOP AT retorno-response INTO DATA(wa_ret).

        READ TABLE it_empresas INTO DATA(wa_empresas) WITH KEY id = wa_ret-id.

        IF wa_empresas-werks IS INITIAL.

          wa_zmmt0165 =
          VALUE #(
                    id          = wa_ret-id
                    name_sis    = wa_ret-name
                    dt_registro = sy-datum
                    hr_registro = sy-uzeit
                    us_registro = sy-uname
                 ).

          APPEND wa_zmmt0165 TO it_zmmt0165.
        ENDIF.

        CLEAR: wa_empresas.

      ENDLOOP.

      MODIFY zmmt0165 FROM TABLE it_zmmt0165.

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_sis~get_id_referencia.
    r_if_integracao_sis = me.
  ENDMETHOD.


  method ZIF_INTEGRACAO_SIS~GET_INSTANCE.
  endmethod.


  METHOD zif_integracao_sis~set_ds_data.
    r_if_integracao_sis = me.
  ENDMETHOD.


  METHOD zif_integracao_sis~set_ds_url.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = me->zif_integracao_sis~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url              = me->zif_integracao_sis~at_auth_ws-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo           = me->zif_integracao_sis~at_auth_ws-method.

    me->zif_integracao_sis~set_id_referencia( ).

    r_if_integracao_sis = me.

  ENDMETHOD.


  METHOD zif_integracao_sis~set_id_referencia.
    r_if_integracao_sis = me.
    me->zif_integracao_sis~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_integracao_sis~set_indicador_sis.
*    https://api.tblmanager.com/v1.0/indicator/5583/field

    TYPES: BEGIN OF ty_fields,
             id     TYPE string,
             name   TYPE string,
             type   TYPE string,
             calc   TYPE string,
             column TYPE string,
           END OF ty_fields.

    DATA: it_field          TYPE TABLE OF ty_fields,
          id_empresa        TYPE string,
          vl_json_indicador TYPE string.

    DATA: id_indicador TYPE string,
          vl_url       TYPE string.

    CASE indicador-matnr.
      WHEN '000000000000184924'.
        id_indicador = '5638'.
    ENDCASE.

    me->zif_integracao_inject~at_referencia =
        VALUE #(
                 tp_referencia = 'SIS - Indicador Fields'
               ).
    me->zif_integracao_sis~set_ds_url( ).

    vl_url = me->zif_integracao_inject~at_info_request_http-ds_url.
    vl_url = |{ vl_url }/{ id_indicador }/field|.

    me->zif_integracao_inject~at_info_request_http-ds_url = vl_url.

    me->zif_integracao_sis~set_ds_data(
      )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao) e_integracao  = DATA(e_integracao) ).

    CALL METHOD zcl_pp_services=>convert_to_utf8
      CHANGING
        json = e_integracao-ds_data_retorno.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json        = e_integracao-ds_data_retorno
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = it_field.

    DELETE it_field WHERE calc IS NOT INITIAL.

    SELECT SINGLE *
      FROM zmmt0165
      INTO @DATA(wa_empresa)
      WHERE werks EQ @indicador-werks.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'De/Para para Empresa ' && indicador-werks && ' não encontrado!' TYPE 'E'.
      EXIT.
    ENDIF.

    CLEAR id_empresa.
    id_empresa = |{ wa_empresa-id ALPHA = OUT }|.
    CONDENSE id_empresa NO-GAPS.

    vl_json_indicador =
    '{' &&
      '"reference": "'   && indicador-gjahr && '" ,' &&
      '"status": "'      && 6               && '" ,' &&
      '"unit_id": "'     && |{ id_empresa ALPHA = OUT }|   && '" ,' &&
      '"latitude": "'    && '10.45463547'   && '" ,' &&
      '"longitude": "'   && '-16.123123'    && '" ,' &&

      '"fld11562061": "' && |{ sy-datum }/{ sy-uzeit }| && '" ,' &&
      '"fld11562063": "' && |{ indicador-gjahr }/{ wa_empresa-id }| && '" ,' &&
      '"fld11562065": "' && '10894'         && '" ,' &&
      '"fld11562069": "' && indicador-mbgbtr  && '" ,' &&
      '"fld11562071": "' && '88'              && '"' &&
    '}'.

    me->zif_integracao_sis~set_ds_url( ).

    vl_url = me->zif_integracao_inject~at_info_request_http-ds_url.
    vl_url = |{ vl_url }/{ id_indicador }/response|.

    me->zif_integracao_inject~at_info_request_http-ds_url = vl_url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = if_rest_message=>gc_method_post.

    me->zif_integracao_inject~at_referencia =
        VALUE #(
                 tp_referencia = 'SIS - Indicador Post'
               ).

    me->zif_integracao_inject~at_info_request_http-ds_body = vl_json_indicador.

    CLEAR: id_integracao, e_integracao.

    me->zif_integracao_sis~set_ds_data(
      )->set_send_msg( IMPORTING e_id_integracao = id_integracao e_integracao  = e_integracao ).

  ENDMETHOD.


  METHOD zif_integracao_sis~set_parametros.
    CHECK i_parametros IS NOT INITIAL.
    zif_integracao_sis~at_parametros = i_parametros.
  ENDMETHOD.


  METHOD zif_integracao_sis~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_sis = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

  ENDMETHOD.


  METHOD zif_integracao_sis~set_servico.
    CHECK i_servico IS NOT INITIAL.
    zif_integracao_sis~at_servico = i_servico.
  ENDMETHOD.


  METHOD zif_integracao_sis~set_xml.
    r_if_integracao_sis = me.
  ENDMETHOD.
ENDCLASS.
