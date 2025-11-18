class ZCL_INTEGRACAO_BRY_ADIGITAL_IN definition
  public
  create public .

public section.

  interfaces ZIF_INTEGRACAO_BRY_ADIGITAL_IN .
  interfaces ZIF_INTEGRACAO_INJECT .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_BRY_ADIGITAL_IN IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = '062'.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    "me->zif_integracao_inject~at_referencia-tp_referencia = 'NFE_SAP_GRC'.


  ENDMETHOD.


  METHOD zif_integracao_bry_adigital_in~get_instance.

    IF zif_integracao_bry_adigital_in~at_bry_adigital_in IS INITIAL.
      zif_integracao_bry_adigital_in~at_bry_adigital_in = NEW zcl_integracao_bry_adigital_in( ).
    ENDIF.

    r_object =  zif_integracao_bry_adigital_in~at_bry_adigital_in.

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital_in~set_ds_data.

    CHECK i_info IS NOT INITIAL.

    "lv_json = /ui2/cl_json=>serialize( EXPORTING data = i_info ).

    /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body
               CHANGING data = me->zif_integracao_bry_adigital_in~at_coleta_rec ).

    me->zif_integracao_bry_adigital_in~at_integracao-ds_url = i_info-ds_url.
    me->zif_integracao_bry_adigital_in~at_integracao-ds_body = i_info-ds_body.
    me->zif_integracao_bry_adigital_in~at_integracao-ds_body_xstring = i_info-ds_body_xstring.
    me->zif_integracao_bry_adigital_in~at_integracao-ds_metodo = i_info-ds_metodo.
    me->zif_integracao_bry_adigital_in~at_integracao-ds_server_protocolo = i_info-ds_server_protocolo.
    me->zif_integracao_bry_adigital_in~at_integracao-ds_content_type = i_info-ds_content_type.
    me->zif_integracao_bry_adigital_in~at_integracao-ds_ip_origem = i_info-ds_ip_origem.
    me->zif_integracao_bry_adigital_in~at_integracao-ds_formato = i_info-ds_formato.

    r_object = me.


  ENDMETHOD.


  METHOD zif_integracao_bry_adigital_in~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_object = me.

    MOVE-CORRESPONDING me->zif_integracao_bry_adigital_in~at_integracao TO me->zif_integracao_inject~at_info_request_http.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_bry_adigital_in~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
           IMPORTING
             e_data_retorno = DATA(e_data_retorno)
             e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    e_msg = e_data_retorno.
    "e_protocolo = zcl_string=>lpad( i_str  = CONV #( e_integracao-id_integracao ) i_qtd  = 15 i_char = '0'  ).

    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  endmethod.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA lv_etapa TYPE zin_etapa.
    DATA lv_mess TYPE string.
    DATA lv_type TYPE c.

    IF zif_integracao_bry_adigital_in~at_coleta_rec-status NE 'CONCLUIDO'.

*      e_nm_code  = '400'.
*      e_msg_outbound = `Status diferente de concluido`.
*      e_sucesso = abap_true.
*      RETURN.

      lv_etapa = '98'.

      lv_mess = 'Coleta recusada na BRY'.

      lv_type = 'E'.

    ELSE.

      lv_etapa = '02'.

      lv_mess = 'Coleta finalizada BRY'.

      lv_type = 'S'.

    ENDIF.

    IF zif_integracao_bry_adigital_in~at_coleta_rec-chave IS INITIAL.

      e_nm_code  = '400'.
      e_msg_outbound = `Informar chave de coleta`.
      e_sucesso = abap_true.
      RETURN.

    ENDIF.

    IF strlen( zif_integracao_bry_adigital_in~at_coleta_rec-chave ) < 64.

      e_nm_code  = '400'.
      e_msg_outbound = `Chave invalida`.
      e_sucesso = abap_true.
      RETURN.

    ENDIF.

    SELECT SINGLE * FROM zint_assina01
      INTO @DATA(lw_coleta)
      WHERE chave_coleta = @zif_integracao_bry_adigital_in~at_coleta_rec-chave.

    IF sy-subrc NE 0.

      e_nm_code  = '400'.
      e_msg_outbound = `Chave não encontrada`.
      e_sucesso = abap_true.
      RETURN.

    ENDIF.

    IF lw_coleta-etapa NE '01'.

      e_nm_code  = '400'.
      e_msg_outbound = `Chave de coleta em etapa diferente`.
      e_sucesso = abap_true.
      RETURN.

    ENDIF.

    lw_coleta-etapa = lv_etapa.

    lw_coleta-log_date = sy-datum.
    lw_coleta-log_uzeit = sy-uzeit.
    lw_coleta-log_name = sy-uname.

    MODIFY zint_assina01 FROM lw_coleta.

    e_nm_code = '200'.
    e_msg_outbound = `Coleta recebida`.
    e_sucesso = abap_true.

    IF e_sucesso = abap_true.

      DATA lw_log TYPE zint_assina02.

      lw_log-id_referencia = lw_coleta-id_referencia.
      lw_log-id_processo = lw_coleta-id_processo.
      lw_log-msgdt = sy-datum.
      lw_log-msghr = sy-uzeit.
      lw_log-id_mensagem = '1'.
      lw_log-msgty = lv_type.
      lw_log-msgid = '000'.
      lw_log-message = lv_mess.
      lw_log-msgv1 = space.
      lw_log-msgv2 = space.
      lw_log-msgv3 = space.
      lw_log-msgv4 = space.

      MODIFY zint_assina02 FROM lw_log.

    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
  endmethod.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
  endmethod.
ENDCLASS.
