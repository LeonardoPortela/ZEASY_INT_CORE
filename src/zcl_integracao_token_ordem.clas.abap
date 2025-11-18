CLASS zcl_integracao_token_ordem DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_token_ordem .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TOKEN_ORDEM IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_cria_modifica_nota.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

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
    r_if_integracao_inject = me.
    e_sucesso              = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
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
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_token_ordem~get_instance.

    IF zif_integracao_token_ordem~at_if_integracao_token IS NOT BOUND.
      CREATE OBJECT zif_integracao_token_ordem~at_if_integracao_token
        TYPE zcl_integracao_token_ordem.
    ENDIF.

    r_if_integracao_token = zif_integracao_token_ordem~at_if_integracao_token.

  ENDMETHOD.


  METHOD zif_integracao_token_ordem~get_token.


    TYPES:
      BEGIN OF ty_token,
        username TYPE string,
        password TYPE string,
      END OF ty_token.

    DATA: l_xml               TYPE string,
          l_table_timestamp   TYPE p,
          l_current_timestamp TYPE p.

    DATA: lt_zintegracao0001 TYPE STANDARD TABLE OF zintegracao0001,
          lt_tvarvc          TYPE STANDARD TABLE OF tvarvc,
          wl_token           TYPE ty_token.

    DATA: data_inicial TYPE dats,
          data_final   TYPE dats,
          hora_inicial TYPE tims,
          hora_final   TYPE tims,
          vl_result    TYPE sytabix.


    r_if_integracao_token = me.

    FREE: l_xml.

    SELECT *
      FROM zauth_webservice
      INTO TABLE @DATA(lt_werbservice)
      WHERE service EQ 'TOKEN_ORDEM_MOBMAN'.

    IF sy-subrc IS INITIAL.
      READ TABLE lt_werbservice INTO DATA(ls_webservice) INDEX 1.
      IF sy-subrc IS INITIAL.

        me->zif_integracao_inject~at_header_fields =
        VALUE #(
                  ( name  = '~request_method' value = ls_webservice-method )
                  ( name  = 'Content-Type'    value = 'application/json; charset=UTF-8' )
                  ( name  = 'Accept-Charset'  value = 'utf-8' )
               ).

      ENDIF.

      wl_token =
      VALUE #(
                username = ls_webservice-username
                password = ls_webservice-password
      ).

      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data        = wl_token
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        RECEIVING
          r_json      = l_xml.

    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_body = l_xml.

    me->zif_integracao_token_ordem~set_ds_url( ).
    me->zif_integracao_token_ordem~set_send_msg( IMPORTING
                                                  e_access_token  = DATA(e_access_token)
                                                  e_token_type    = DATA(e_token_type)
                                                  e_expires_in    = DATA(e_expires_in)
                                          ).
  ENDMETHOD.


  METHOD zif_integracao_token_ordem~set_ds_url.

    DATA: lt_werbservice TYPE STANDARD TABLE OF zauth_webservice.

    DATA: wa_webservice TYPE zauth_webservice.

    r_if_integracao_token  = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service EQ 'TOKEN_ORDEM_MOBMAN'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'C'
                            attr2 = 'C1' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'C'
          msgv2  = 'C1'.
    ENDIF.

    SORT lt_werbservice BY service.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'TOKEN_ORDEM_MOBMAN' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = if_rest_message=>gc_method_post.

  ENDMETHOD.


  METHOD zif_integracao_token_ordem~set_send_msg.

    TYPES: BEGIN OF ty_retorno,
             access_token TYPE string,
             token_type   TYPE string,
             expires_in   TYPE string,
           END OF ty_retorno.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.

    r_if_integracao_token = me.

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

    FREE: lc_integrar.
    CLEAR: lc_integrar.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

    e_access_token = e_integracao-ds_data_retorno.
*    e_access_token = lc_retorno-access_token.
    e_token_type   = 'Bearer'.
*    e_expires_in   = lc_retorno-expires_in.

    CONCATENATE e_token_type e_access_token INTO DATA(l_header_token) SEPARATED BY space.
    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization'  value = l_header_token ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
