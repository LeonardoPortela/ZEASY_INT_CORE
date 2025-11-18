class ZCL_INTEGRACAO_TOKEN_SAP_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_TOKEN_SAP_COUPA .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TOKEN_SAP_COUPA IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>AT_ID_CONS_REQ_COUPA.
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


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_token_sap_coupa~get_instance.

    IF zif_integracao_token_sap_coupa~at_if_integracao_token_coupa IS NOT BOUND.
      CREATE OBJECT zif_integracao_token_sap_coupa~at_if_integracao_token_coupa
        TYPE zcl_integracao_token_sap_coupa.
    ENDIF.

    r_if_integracao_token_coupa = zif_integracao_token_sap_coupa~at_if_integracao_token_coupa.

  ENDMETHOD.


  METHOD zif_integracao_token_sap_coupa~get_token.

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


    r_if_integracao_token_coupa = me.

*    FREE: l_xml.

*    SELECT *
*      FROM zauth_webservice
*      INTO TABLE @DATA(lt_werbservice)
*      WHERE service EQ 'CONS_REQ_COUPA_SAP'.
*
**    IF sy-subrc IS INITIAL.
*      READ TABLE lt_werbservice INTO DATA(ls_webservice) INDEX 1.
*      IF sy-subrc IS INITIAL.

*        me->zif_integracao_inject~at_header_fields =
*        VALUE #(
*                  ( name  = '~request_method' value = ls_webservice-method )
*                  ( name  = 'Content-Type'    value = 'application/json; charset=UTF-8' )
*                  ( name  = 'Accept-Charset'  value = 'utf-8' )
*               ).
*
*      ENDIF.

*      wl_token =
*      VALUE #(
*                username = ls_webservice-username
*                password = ls_webservice-password
*      ).

*      CALL METHOD /ui2/cl_json=>serialize
*        EXPORTING
*          data        = wl_token
*          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*        RECEIVING
*          r_json      = l_xml.

*      ENDIF.

      me->zif_integracao_inject~at_info_request_http-ds_body = l_xml.

      me->zif_integracao_token_sap_coupa~set_ds_url( ).
      me->zif_integracao_token_sap_coupa~set_send_msg( IMPORTING
                                                    e_access_token  = DATA(e_access_token)
                                                    e_token_type    = DATA(e_token_type)
                                                    e_expires_in    = DATA(e_expires_in)
                                            ).
    ENDMETHOD.


  METHOD zif_integracao_token_sap_coupa~set_ds_url.

    DATA: lt_werbservice TYPE STANDARD TABLE OF zauth_webservice.

    DATA: wa_webservice TYPE zauth_webservice,
          zparam TYPE string.

    r_if_integracao_token_coupa  = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service EQ 'CONS_REQ_COUPA_SAP'.

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
          msgv1  = 'URL não cadastrada'
          msgv2  = 'Consulta req. Coupa x SAP'.
    ENDIF.

    SORT lt_werbservice BY service.
    clear: zparam.
    zparam = '?grant_type=client_credentials&client_id=78f3261f488fd393df19375d96a13d83&client_secret=134bed42f0fac99c63ab48905722a52421fc9e679d059b2de8fae0ac39b97ba0&scope=core.requisition.read'.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'CONS_REQ_COUPA_SAP' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-token && zparam.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = if_rest_message=>gc_method_post.
  ENDMETHOD.


  METHOD zif_integracao_token_sap_coupa~set_send_msg.

    TYPES: BEGIN OF ty_retorno,
             access_token TYPE string,
             token_type   TYPE string,
             expires_in   TYPE string,
           END OF ty_retorno.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          ztoken TYPE string,
          lc_retorno  TYPE ty_retorno.

    r_if_integracao_token_coupa = me.

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
    ztoken   = |{ lc_retorno-token_type } { lc_retorno-access_token }|.
*    e_expires_in   = lc_retorno-expires_in.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization'  value = ztoken ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'accept'   value = 'application/json' ) TO me->zif_integracao_inject~at_header_fields.



  ENDMETHOD.
ENDCLASS.
