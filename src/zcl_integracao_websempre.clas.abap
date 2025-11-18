class ZCL_INTEGRACAO_WEBSEMPRE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_WEBSEMPRE .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_WEBSEMPRE IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_websempre.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
*    me->zif_integracao_inject~at_referencia-tp_referencia = 'OBS_NF_ZLES0050'.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
  endmethod.


  METHOD zif_integracao_websempre~get_instance.

    IF zif_integracao_websempre~at_if_integracao_websempre IS NOT BOUND.
      CREATE OBJECT zif_integracao_websempre~at_if_integracao_websempre TYPE zcl_integracao_websempre.
    ENDIF.
    r_if_integracao_websempre = zif_integracao_websempre~at_if_integracao_websempre.

  ENDMETHOD.


  METHOD zif_integracao_websempre~set_ds_data.
    r_if_integracao_websempre = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

    me->zif_integracao_inject~at_referencia =
    VALUE #(
              tp_referencia = me->zif_integracao_inject~at_info_request_http-ds_funcao_processa
           ).

  ENDMETHOD.


  METHOD zif_integracao_websempre~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    r_if_integracao_websempre = me.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_websempre~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
           IMPORTING
             e_data_retorno =  DATA(e_data_retorno)
             e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    SELECT COUNT(*)
       FROM tvarvc
       WHERE name EQ 'Z_DADOS_RETORNO_HCM_API'
         AND low EQ abap_true.

    IF sy-subrc IS INITIAL.
      e_integracao-ds_data_retorno = e_msg.
    ENDIF.

    IF e_integracao-id_integracao IS NOT INITIAL.
      UPDATE zintegracao SET ds_data_retorno = e_integracao-ds_data_retorno
        WHERE id_integracao = e_integracao-id_integracao.
    ENDIF.

    e_protocolo = zcl_string=>lpad( i_str  = CONV #( e_integracao-id_integracao ) i_qtd  = 15 i_char = '0'  ).

    CLEAR: lc_integracao.


  ENDMETHOD.
ENDCLASS.
