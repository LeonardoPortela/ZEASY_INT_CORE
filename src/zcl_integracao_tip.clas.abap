class ZCL_INTEGRACAO_TIP definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .

  data AT_WEBSERVICE type ZCIOT_WEBSERVICE .
  data AT_SERVICO type STRING .
  data AT_JSON type STRING .
  data AT_REFERENCIA type STRING .

  methods SET_INTEGRA_VIAGEM
    importing
      !I_SERVICO type STRING
      !I_JSON type STRING
    exporting
      !E_URL type STRING
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
      value(R_INTEGRACAO_TIP) type ref to ZCL_INTEGRACAO_TIP
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_INTEGRACAO_TIP) type ref to ZCL_INTEGRACAO_TIP .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      !E_MENSAGEM type ZINTEGRACAO_LOG
      !E_HEADER_FIELDS type ZDE_HEADER_FIELD_T
    returning
      value(R_INTEGRACAO_TIP) type ref to ZCL_INTEGRACAO_TIP
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TIP IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'INTEGRACAO_TIP'.

  ENDMETHOD.


  METHOD set_ds_data.

    r_integracao_tip = me.

    CASE me->at_servico.
      WHEN 'CADASTRAR_VIAGEM' OR 'AUTORIZAR_VIAGEM'     OR 'CANCELAR_VIAGEM ' OR
           'CONSULTAR_VIAGEM' OR 'CONSULTAR_PDF_VIAGEM' OR 'AJUSTAR_VIAGEM'.
        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml; charset=UTF-8'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.
    ENDCASE.

  ENDMETHOD.


  METHOD set_ds_url.

    DATA: lv_tipo    TYPE ztipowebadm,
          lv_servico TYPE ztipowebserv,
          lv_url     TYPE string.

    r_integracao_tip = me.

    CASE i_servico.
      WHEN 'CADASTRAR_VIAGEM'.
        lv_tipo    = 'I'.
        lv_servico = abap_off.
      WHEN 'AUTORIZAR_VIAGEM'.
        lv_tipo    = 'F'.
        lv_servico = abap_off.
      WHEN 'CANCELAR_VIAGEM '.
        lv_tipo    = 'E'.
        lv_servico = abap_off.
      WHEN 'CONSULTAR_VIAGEM'.
        lv_tipo    = 'C'.
        lv_servico = abap_off.
      WHEN 'CONSULTAR_PDF_VIAGEM'.
        lv_tipo    = 'H'.
        lv_servico = 'VI'.
      WHEN 'AJUSTAR_VIAGEM'.
        lv_tipo    = 'J'.
        lv_servico = 'VI'.
    ENDCASE.

    SELECT SINGLE *
      INTO @DATA(w_webservice)
      FROM zciot_webservice
     WHERE tipo    = @lv_tipo
       AND servico = @lv_servico.

    IF sy-subrc IS NOT INITIAL.
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

    e_url             = w_webservice-url.
    me->at_webservice = w_webservice.
    me->at_servico    = i_servico.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = 'application/xml; charset=UTF-8'.
    me->zif_integracao_inject~at_info_request_http-ds_url              = w_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.

  ENDMETHOD.


  METHOD set_integra_viagem.

    DATA: lv_integracao TYPE zintegracao,
          lv_mensagem   TYPE zintegracao_log,
          lv_retorno    TYPE string,
          lv_error      TYPE c.

    FREE:  me->zif_integracao_inject~at_header_fields,
           me->zif_integracao_inject~at_info_request_http,
           r_xml.

    CASE i_servico.
      WHEN 'CADASTRAR_VIAGEM'.
        lv_integracao-id_interface = '254'.
      WHEN 'AUTORIZAR_VIAGEM'.
        lv_integracao-id_interface = '255'.
      WHEN 'CANCELAR_VIAGEM '.
        lv_integracao-id_interface = '256'.
      WHEN 'CONSULTAR_VIAGEM'.
        lv_integracao-id_interface = '257'.
      WHEN 'CONSULTAR_PDF_VIAGEM'.
        lv_integracao-id_interface = '258'.
      WHEN 'AJUSTAR_VIAGEM'.
        lv_integracao-id_interface = '263'.
    ENDCASE.

    me->zif_integracao_inject~at_tp_integracao            = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_referencia-tp_referencia = i_servico.
    me->zif_integracao_inject~at_referencia-id_referencia = me->at_referencia.
    me->zif_integracao_inject~at_id_interface             = lv_integracao-id_interface.
    me->at_json                                           = i_json.

    TRY.
        me->set_ds_url(   EXPORTING i_servico       = i_servico
                          IMPORTING e_url           = e_url
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

      WHEN 'CADASTRAR_VIAGEM' OR 'AUTORIZAR_VIAGEM'     OR 'CANCELAR_VIAGEM ' OR
           'CONSULTAR_VIAGEM' OR 'CONSULTAR_PDF_VIAGEM' OR 'AJUSTAR_VIAGEM'.

        r_xml = lv_mensagem-ds_data_retorno.

    ENDCASE.

  ENDMETHOD.


  METHOD set_referencia.

    me->at_referencia = i_referencia.

  ENDMETHOD.


  METHOD set_send_msg.

    DATA: lc_integrar    TYPE REF TO zcl_integracao.

    r_integracao_tip = me.

    CREATE OBJECT lc_integrar.

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


  METHOD zif_integracao_inject~get_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_form_request_http.
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
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
