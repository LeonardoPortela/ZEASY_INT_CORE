class ZCL_INTEGRACAO_TOKEN_IPORTSO definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_TOKEN_IPORTSO .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TOKEN_IPORTSO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_token_coupa.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_form_fields = me->zif_integracao_inject~at_form_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    r_if_integracao_inject = me.
    e_sucesso              = abap_false.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields = i_form_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_token_iportso~get_instance.

    IF zif_integracao_token_iportso~at_if_integracao_token IS NOT BOUND.
      CREATE OBJECT zif_integracao_token_iportso~at_if_integracao_token
        TYPE zcl_integracao_token_iportso.
    ENDIF.

    r_if_integracao_token = zif_integracao_token_iportso~at_if_integracao_token.

  ENDMETHOD.


  METHOD zif_integracao_token_iportso~get_token.

    DATA: l_xml               TYPE string,
          l_table_timestamp   TYPE p,
          l_current_timestamp TYPE p.

    DATA: lt_zintegracao0001 TYPE STANDARD TABLE OF zintegracao0001,
          lt_tvarvc          TYPE STANDARD TABLE OF tvarvc.

    r_if_integracao_token = me.

*--------------------------------------------------------------------------------------------------------------------*
*  Verificação Existencia Token Valido - Ini
*--------------------------------------------------------------------------------------------------------------------*
    SELECT SINGLE *
      FROM zintegracao0001 INTO @DATA(ls_zintegracao0001)
      WHERE id_token EQ '0001'.

    IF sy-subrc IS INITIAL.

      l_table_timestamp   = ls_zintegracao0001-time_stamp.

      GET TIME STAMP FIELD DATA(l_timestamp_before).
      l_current_timestamp = l_timestamp_before.

      IF cl_abap_tstmp=>subtract( tstmp1 = l_current_timestamp
                                  tstmp2 = l_table_timestamp ) < ls_zintegracao0001-expires_in.

        "Token ainda está válido.
        CONCATENATE ls_zintegracao0001-token_type ls_zintegracao0001-access_token INTO DATA(l_header_token) SEPARATED BY space.
        CLEAR: me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_header_fields =
        VALUE #(
                  ( name  = 'Authorization'   value = l_header_token )
               ).

        EXIT.
      ENDIF.

    ENDIF.

*--------------------------------------------------------------------------------------------------------------------*
*  Verificação Existencia Token Valido - Fim
*--------------------------------------------------------------------------------------------------------------------*

    FREE: l_xml.

    SELECT *
      FROM zauth_webservice
      INTO TABLE @DATA(lt_werbservice)
      WHERE service EQ 'TGG_REFEITORIO_TOKEN'.

    IF sy-subrc IS INITIAL.
      READ TABLE lt_werbservice INTO DATA(ls_webservice) INDEX 1.
      IF sy-subrc IS INITIAL.

        me->zif_integracao_inject~at_header_fields =
        VALUE #(
                  ( name  = '~request_method' value = if_rest_message=>gc_method_post )
                  ( name  = 'Content-Type'    value = 'application/x-www-form-urlencoded' )
                  ( name  = 'Authorization'   value = 'Basic aVBvcnRTb2x1dGlvbnNfQXBpX1BhZHJhbzpzZWNyZXQ=' )
               ).

        me->zif_integracao_inject~at_form_fields =
        VALUE #(
                 ( name = 'grant_type' value = 'password' )
                 ( name = 'username'   value = ls_webservice-username )
                 ( name = 'password'   value = ls_webservice-password )
                 ( name = 'scope'      value = 'offline_access' )
               ).
      ENDIF.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_body = l_xml.

    me->zif_integracao_token_iportso~set_ds_url( ).
    me->zif_integracao_token_iportso~set_send_msg( IMPORTING
                                                  e_access_token  = DATA(e_access_token)
                                                  e_token_type    = DATA(e_token_type)
                                                  e_expires_in    = DATA(e_expires_in)
                                          ).

    GET TIME STAMP FIELD DATA(l_timestamp_after).

    APPEND VALUE #( id_token     = '0001'
                    access_token = e_access_token
                    token_type   = e_token_type
                    expires_in   = e_expires_in - 300
                    time_stamp   = l_timestamp_after
                  ) TO lt_zintegracao0001.

    MODIFY zintegracao0001 FROM TABLE lt_zintegracao0001.

  ENDMETHOD.


  METHOD zif_integracao_token_iportso~set_ds_url.

    DATA: lt_werbservice TYPE STANDARD TABLE OF zauth_webservice.

    DATA: wa_webservice TYPE zauth_webservice.

    r_if_integracao_token  = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service EQ 'TGG_REFEITORIO_TOKEN'.

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

*    CLEAR: me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #(
*                    name = 'Content-Type'
*                    value = 'application/x-www-form-urlencoded'
*                  ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
*    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/x-www-form-urlencoded'.

    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'TGG_REFEITORIO_TOKEN' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.


  ENDMETHOD.


  METHOD zif_integracao_token_iportso~set_send_msg.

    TYPES: BEGIN OF ty_retorno,
             access_token TYPE string,
             token_type   TYPE string,
             expires_in   TYPE string,
           END OF ty_retorno.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.

    r_if_integracao_token = me.

    CREATE OBJECT lc_integrar.


*    me->zif_integracao_inject~at_form_fields

    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/x-www-form-urlencoded'.

    lc_integrar->zif_integracao~at_form_fields = me->zif_integracao_inject~at_form_fields.

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

    e_access_token = lc_retorno-access_token.
    e_token_type   = lc_retorno-token_type.
    e_expires_in   = lc_retorno-expires_in.

    CONCATENATE e_token_type e_access_token INTO DATA(l_header_token) SEPARATED BY space.
    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization'  value = l_header_token ) TO me->zif_integracao_inject~at_header_fields.



  ENDMETHOD.
ENDCLASS.
