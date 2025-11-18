class ZCL_INTEGRACAO_TOKEN_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_TOKEN_COUPA .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TOKEN_COUPA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_token_coupa.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

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


  METHOD zif_integracao_token_coupa~get_instance.

    IF zif_integracao_token_coupa~at_if_integracao_token IS NOT BOUND.
      CREATE OBJECT zif_integracao_token_coupa~at_if_integracao_token
        TYPE zcl_integracao_token_coupa.
    ENDIF.

    r_if_integracao_token = zif_integracao_token_coupa~at_if_integracao_token.

  ENDMETHOD.


  METHOD zif_integracao_token_coupa~get_token.

    DATA: l_xml               TYPE string,
          l_table_timestamp   TYPE p,
          l_current_timestamp TYPE p.

    DATA: lt_zmmt0153 TYPE STANDARD TABLE OF zmmt0153,
          lt_tvarvc   TYPE STANDARD TABLE OF tvarvc.

    r_if_integracao_token = me.

*--------------------------------------------------------------------------------------------------------------------*
*  Verificação Existencia Token Valido - Ini
*--------------------------------------------------------------------------------------------------------------------*
    SELECT *
      FROM tvarvc
      INTO TABLE lt_tvarvc
      WHERE name EQ 'COUPA_CHECK_TOKEN_VALIDO'
        AND low  EQ abap_true.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE *
        FROM  zmmt0153 INTO @DATA(ls_zmmt0153)
       WHERE last_registro = @abap_true.

      IF sy-subrc IS INITIAL.

        l_table_timestamp   = ls_zmmt0153-time_stamp.

        GET TIME STAMP FIELD DATA(l_timestamp_before).
        l_current_timestamp = l_timestamp_before.

        IF cl_abap_tstmp=>subtract( tstmp1 = l_current_timestamp
                                    tstmp2 = l_table_timestamp ) < 80000.

          "Token ainda está válido.
          CONCATENATE ls_zmmt0153-token_type ls_zmmt0153-access_token INTO DATA(l_header_token) SEPARATED BY space.
          CLEAR: me->zif_integracao_inject~at_header_fields.
          APPEND VALUE #( name = 'Authorization'  value = l_header_token ) TO me->zif_integracao_inject~at_header_fields.
          EXIT.
        ENDIF.

      ENDIF.

    ENDIF.
*--------------------------------------------------------------------------------------------------------------------*
*  Verificação Existencia Token Valido - Fim
*--------------------------------------------------------------------------------------------------------------------*

    FREE: l_xml.

    SELECT *
      FROM zauth_webservice
      INTO TABLE @DATA(lt_werbservice)
      WHERE service EQ 'COUPA_TOKEN'.
    IF sy-subrc IS INITIAL.
      READ TABLE lt_werbservice INTO DATA(ls_webservice) INDEX 1.
      IF sy-subrc IS INITIAL.
        TRANSLATE ls_webservice-add01 TO LOWER CASE.
        TRANSLATE ls_webservice-add02 TO LOWER CASE.
        CONCATENATE l_xml ls_webservice-add01 ls_webservice-add02 '&grant_type=client_credentials&scope=' INTO l_xml.
      ENDIF.
    ENDIF.

    SELECT *
      FROM ztoken_coupa
      INTO TABLE @DATA(lt_scopes).
    IF sy-subrc IS INITIAL.
      LOOP AT lt_scopes INTO DATA(ls_scopes).
        TRANSLATE ls_scopes-scope TO LOWER CASE.
        DATA(l_len) =  strlen( ls_scopes-scope ).
        CONCATENATE l_xml ls_scopes-scope(l_len) space INTO l_xml RESPECTING BLANKS.
      ENDLOOP.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_body = l_xml.

    me->zif_integracao_token_coupa~set_ds_url( ).
    me->zif_integracao_token_coupa~set_send_msg( IMPORTING e_access_token = DATA(e_access_token)
                                                           e_token_type   = DATA(e_token_type)
                                                           e_expires_in   = DATA(e_expires_in)
                                                          ).

    GET TIME STAMP FIELD DATA(l_timestamp_after).

    APPEND VALUE #( access_token = e_access_token token_type = e_token_type expires_in = e_expires_in time_stamp = l_timestamp_after last_registro = abap_true ) TO lt_zmmt0153.

    UPDATE zmmt0153 SET last_registro = abap_false
      WHERE last_registro = abap_true.

    MODIFY zmmt0153 FROM TABLE lt_zmmt0153.

  ENDMETHOD.


  METHOD zif_integracao_token_coupa~set_ds_url.

    DATA: lt_werbservice TYPE STANDARD TABLE OF zauth_webservice.

    DATA: wa_webservice TYPE zauth_webservice.

    r_if_integracao_token  = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service EQ 'COUPA_TOKEN'.
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

    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type' value = 'application/x-www-form-urlencoded' ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/x-www-form-urlencoded'.

    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'COUPA_TOKEN' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.

  ENDMETHOD.


  METHOD zif_integracao_token_coupa~set_send_msg.

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

    e_access_token = lc_retorno-access_token.
    e_token_type   = lc_retorno-token_type.
    e_expires_in   = lc_retorno-expires_in.

    CONCATENATE e_token_type e_access_token INTO DATA(l_header_token) SEPARATED BY space.
    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization'  value = l_header_token ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
