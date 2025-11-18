class ZCL_INTEGRACAO_SISDEV definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .

  data AT_WEBSERVICE type ZAUTH_WEBSERVICE .
  data AT_WS_0001 type ZAUTH_WS_0001 .
  data AT_SERVICO type STRING .
  data AT_JSON type STRING .
  data AT_REFERENCIA type STRING .
  data AT_METODO type STRING .
  data AT_BUKRS type BUKRS .
  data AT_BRANCH type J_1BBRANC_ .

  methods SET_INTEGRA_SISDEV
    importing
      !I_SERVICO type STRING
      !I_METODO type STRING
      !I_DOCNUM type J_1BDOCNUM
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_JSON type STRING
    returning
      value(R_XML) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_REFERENCIA
    importing
      !I_REFERENCIA type STRING .
  methods SET_DS_URL
    importing
      !I_SERVICO type STRING
    exporting
      !E_URL type STRING
    returning
      value(R_INTEGRACAO_SISDEV) type ref to ZCL_INTEGRACAO_SISDEV
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_INTEGRACAO_SISDEV) type ref to ZCL_INTEGRACAO_SISDEV .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_MENSAGEM type ZINTEGRACAO_LOG
      !E_HEADER_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_INTEGRACAO_SISDEV) type ref to ZCL_INTEGRACAO_SISDEV
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_SISDEV IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'INTEGRACAO_SISDEV'.

  ENDMETHOD.


  METHOD set_ds_data.

    DATA: lv_var_string TYPE string,
          lv_string     TYPE string,
          lv_xstring    TYPE xstring.

    r_integracao_sisdev = me.

    CASE me->at_servico.
      WHEN 'INDEA_DEVOLUCAO_AGROTOXICO' OR 'INDEA_DEVOLUCAO_SEMENTES' OR 'INDEA_ENTRADA_AGROTOXICO' OR
           'INDEA_ENTRADA_SEMENTES'     OR 'INDEA_SAIDA_AGROTOXICO'   OR 'INDEA_SAIDA_SEMENTES'.

        lv_var_string = |{ me->at_ws_0001-username }:{ me->at_ws_0001-password }|.

        "Convert base64.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = lv_var_string
          IMPORTING
            buffer = lv_xstring
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

        CHECK sy-subrc = 0.

        "Converter XSTRING em BASE64.
        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = lv_xstring
          IMPORTING
            output = lv_string.

        CHECK lv_string IS NOT INITIAL.

        me->zif_integracao_inject~at_header_fields =
          VALUE #(
                 ( name  = 'Content-Type'      value = 'application/json; charset=UTF-8' )
                 ( name  = 'Accept'            value = 'application/json; charset=UTF-8' )
                 ( name  = 'Authorization'     value = |Basic { lv_string }| )
                 ).

*       me->zif_integracao_inject~at_form_fields =
*         VALUE #(
*                ( name = 'grant_type'         value = 'client_credentials' )
*                ).

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json; charset=UTF-8'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = me->at_metodo.
    ENDCASE.

  ENDMETHOD.


  METHOD set_ds_url.

    DATA: lv_tipo    TYPE ztipowebadm,
          lv_servico TYPE ztipowebserv,
          lv_url     TYPE string.

    r_integracao_sisdev = me.

    FREE: me->at_webservice, me->at_ws_0001.

    SELECT SINGLE *
      FROM zauth_ws_0001
      INTO me->at_ws_0001
     WHERE service = i_servico
       AND bukrs   = me->at_bukrs
       AND branch  = me->at_branch.

    DATA(lv_subrc1) = sy-subrc.

    SELECT SINGLE *
      INTO me->at_webservice
      FROM zauth_webservice
     WHERE service = i_servico.

    DATA(lv_subrc2) = sy-subrc.

    IF lv_subrc1 IS NOT INITIAL OR lv_subrc2 IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'T'
                            attr2 = 'TC' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'T'
          msgv2  = 'TC'.
    ENDIF.

    e_url                                                              = me->at_webservice-url.
    me->at_servico                                                     = i_servico.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = 'application/json; charset=UTF-8'.
    me->zif_integracao_inject~at_info_request_http-ds_url              = me->at_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.

  ENDMETHOD.


  METHOD set_integra_sisdev.

    DATA: lv_integracao TYPE zintegracao,
          lv_mensagem   TYPE zintegracao_log,
          lv_retorno    TYPE string,
          lv_error      TYPE c.

    FREE:  me->zif_integracao_inject~at_header_fields,
           me->zif_integracao_inject~at_info_request_http,
           r_xml.

    CASE i_servico.
      WHEN 'INDEA_DEVOLUCAO_AGROTOXICO'.
        lv_integracao-id_interface = '270'.
      WHEN 'INDEA_DEVOLUCAO_SEMENTES'.
        lv_integracao-id_interface = '271'.
      WHEN 'INDEA_ENTRADA_AGROTOXICO'.
        lv_integracao-id_interface = '272'.
      WHEN 'INDEA_ENTRADA_SEMENTES'.
        lv_integracao-id_interface = '273'.
      WHEN 'INDEA_SAIDA_AGROTOXICO'.
        lv_integracao-id_interface = '274'.
      WHEN 'INDEA_SAIDA_SEMENTES'.
        lv_integracao-id_interface = '275'.
    ENDCASE.

    me->zif_integracao_inject~at_tp_integracao            = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_referencia-tp_referencia = i_servico.
    me->zif_integracao_inject~at_referencia-id_referencia = i_docnum.
    me->zif_integracao_inject~at_id_interface             = lv_integracao-id_interface.
    me->at_json                                           = i_json.
    me->at_metodo                                         = i_metodo.
    me->at_bukrs                                          = i_bukrs.
    me->at_branch                                         = i_branch.

    TRY.
        me->set_ds_url(   EXPORTING i_servico       = i_servico
         )->set_ds_data(  EXPORTING i_integracao    = lv_integracao
         )->set_send_msg( IMPORTING e_id_integracao = DATA(lv_id_integracao)
                                    e_integracao    = lv_integracao
                                    e_mensagem      = lv_mensagem
         ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        r_xml        = lv_mensagem-ds_data_retorno.

        IF r_xml IS NOT INITIAL.
          RETURN.
        ELSE.
          RAISE EXCEPTION TYPE zcx_integracao
            EXPORTING
              textid = VALUE #( msgid = ex_integra->msgid
                                msgno = ex_integra->msgno
                                attr1 = CONV #( ex_integra->msgv1 )
                                attr2 = CONV #( ex_integra->msgv2 )
                                attr3 = CONV #( ex_integra->msgv3 )
                                attr4 = CONV #( ex_integra->msgv4 ) )
              msgid  = ex_integra->msgid
              msgno  = ex_integra->msgno
              msgty  = 'E'
              msgv1  = CONV #( ex_integra->msgv1 )
              msgv2  = CONV #( ex_integra->msgv2 )
              msgv3  = CONV #( ex_integra->msgv3 )
              msgv4  = CONV #( ex_integra->msgv4 ).
        ENDIF.

      CATCH zcx_error      INTO DATA(ex_error).    "  "
        r_xml        = lv_mensagem-ds_data_retorno.

        IF r_xml IS NOT INITIAL.
          RETURN.
        ELSE.
          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = ex_error->msgid
                                msgno = ex_error->msgno
                                attr1 = CONV #( ex_error->msgv1 )
                                attr2 = CONV #( ex_error->msgv2 )
                                attr3 = CONV #( ex_error->msgv3 )
                                attr4 = CONV #( ex_error->msgv4 ) )
              msgid  = ex_error->msgid
              msgno  = ex_error->msgno
              msgty  = 'E'
              msgv1  = CONV #( ex_error->msgv1 )
              msgv2  = CONV #( ex_error->msgv2 )
              msgv3  = CONV #( ex_error->msgv3 )
              msgv4  = CONV #( ex_error->msgv4 ).
        ENDIF.
    ENDTRY.

*---------------------------------------
*-- retorno JSON
*---------------------------------------
    CASE i_servico.

      WHEN 'INDEA_DEVOLUCAO_AGROTOXICO' OR 'INDEA_DEVOLUCAO_SEMENTES' OR 'INDEA_ENTRADA_AGROTOXICO' OR
           'INDEA_ENTRADA_SEMENTES'     OR 'INDEA_SAIDA_AGROTOXICO'   OR 'INDEA_SAIDA_SEMENTES'.

        r_xml = lv_mensagem-ds_data_retorno.

    ENDCASE.

  ENDMETHOD.


  METHOD SET_REFERENCIA.

    me->at_referencia = i_referencia.

  ENDMETHOD.


  METHOD set_send_msg.

    DATA: lc_integrar    TYPE REF TO zcl_integracao.

    r_integracao_sisdev = me.

    CREATE OBJECT lc_integrar.

    lc_integrar->zif_integracao~at_form_fields = me->zif_integracao_inject~at_form_fields.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg(      IMPORTING e_id_integracao  = e_id_integracao
      )->set_outbound_msg( IMPORTING e_mensagem       = e_mensagem
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro(     IMPORTING e_integracao     = e_integracao
      )->free(
      ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
