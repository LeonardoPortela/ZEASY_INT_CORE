class ZCL_INTEGRACAO_COUPA_ORDER definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COUPA_ORDER .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_ORDER IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_order.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD zif_integracao_coupa_order~get_instance.

    CREATE OBJECT zif_integracao_coupa_order~at_if_integracao_order
      TYPE zcl_integracao_coupa_order.

    r_if_integracao_order = zif_integracao_coupa_order~at_if_integracao_order.

  ENDMETHOD.


  METHOD zif_integracao_coupa_order~set_ds_data.

    CHECK i_info IS NOT INITIAL.

    /ui2/cl_json=>deserialize( EXPORTING
                                json = i_info-ds_body
                               CHANGING
                               data  = me->zif_integracao_coupa_order~at_pedido_compra_coupa ).

    r_if_integracao_order = me.

  ENDMETHOD.


  METHOD zif_integracao_coupa_order~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_if_integracao_order  = me.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
    )->set_processar_retorno(
    )->set_integrar_retorno(
         IMPORTING
           e_data_retorno = DATA(e_data_retorno)
           e_zintegracao_log = e_zintegracao_log
    )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
    )->free(
    ).

    e_msg = e_data_retorno.

    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: ls_class_contabil_coupa TYPE zclassificacao_contabil_coupa,
          ls_parametros_orcamento TYPE zparametros_verifca_orcamento,
          ls_codigo_operacao      TYPE c LENGTH 2,
          ls_status               TYPE string,
          ls_saldo                TYPE string,
          ls_mensagem             TYPE string,
          ls_id                   TYPE string,
          ls_anln2                TYPE anln2,
          ls_is_approved          TYPE xflag.

    READ TABLE me->zif_integracao_coupa_order~at_pedido_compra_coupa-order_line_changes INTO DATA(ls_lines) INDEX 1.
    IF sy-subrc IS INITIAL.
      IF ls_lines-account IS NOT INITIAL.
        SPLIT ls_lines-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
                                                ls_class_contabil_coupa              ls_parametros_orcamento-kostl
                                                ls_parametros_orcamento-cost_object  ls_codigo_operacao
                                                ls_parametros_orcamento-saknr.
        IF ls_class_contabil_coupa EQ 'A'.
          SPLIT ls_lines-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
                                                  ls_class_contabil_coupa              ls_parametros_orcamento-kostl
                                                  ls_parametros_orcamento-cost_object  ls_anln2
                                                  ls_codigo_operacao                   ls_parametros_orcamento-saknr.

          CONCATENATE ls_parametros_orcamento-cost_object '-' ls_anln2 INTO ls_parametros_orcamento-cost_object.
        ENDIF.
      ELSE.
        LOOP AT ls_lines-account_allocations INTO DATA(ls_account_allocations).
          FREE: ls_parametros_orcamento.

          SPLIT ls_account_allocations-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
                                                                ls_class_contabil_coupa              ls_parametros_orcamento-kostl
                                                                ls_parametros_orcamento-cost_object  ls_codigo_operacao
                                                                ls_parametros_orcamento-saknr.
          IF ls_class_contabil_coupa EQ 'A'.
            SPLIT ls_account_allocations-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
                                                                  ls_class_contabil_coupa              ls_parametros_orcamento-kostl
                                                                  ls_parametros_orcamento-cost_object  ls_anln2
                                                                  ls_codigo_operacao                   ls_parametros_orcamento-saknr.

            CONCATENATE ls_parametros_orcamento-cost_object '-' ls_anln2 INTO ls_parametros_orcamento-cost_object.
          ENDIF.

          ls_parametros_orcamento-currency = ls_lines-currency-code.
          ls_parametros_orcamento-total    = ls_lines-total.

          CALL FUNCTION 'Z_VERIFICA_ORCAMENTO_COUPA'
            EXPORTING
              iv_class_contabil_coupa       = ls_class_contabil_coupa
              is_parametros_orcamento       = ls_parametros_orcamento
            IMPORTING
              ev_status                     = ls_status
              ev_saldo                      = ls_saldo
              ev_mensagem                   = ls_mensagem
            EXCEPTIONS
              class_contabil_coupa_invalida = 1
              OTHERS                        = 2.
          IF sy-subrc <> 0.
          ENDIF.

          IF ls_status EQ 'rejected'.
            EXIT.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.

    ls_parametros_orcamento-currency = ls_lines-currency-code.
    ls_parametros_orcamento-total    = ls_lines-total.

    ls_id = me->zif_integracao_coupa_order~at_pedido_compra_coupa-current_approval-id.

    IF ls_status IS INITIAL.
      CALL FUNCTION 'Z_VERIFICA_ORCAMENTO_COUPA'
        EXPORTING
          iv_class_contabil_coupa       = ls_class_contabil_coupa
          is_parametros_orcamento       = ls_parametros_orcamento
        IMPORTING
          ev_status                     = ls_status
          ev_saldo                      = ls_saldo
          ev_mensagem                   = ls_mensagem
        EXCEPTIONS
          class_contabil_coupa_invalida = 1
          OTHERS                        = 2.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.

    CLEAR: ls_is_approved.

    CASE ls_status.
      WHEN 'approved'.
        ls_is_approved = 'X'.
      WHEN 'rejected'.
        ls_is_approved = space.
      WHEN OTHERS.
    ENDCASE.

    TRY .
        zcl_integracao_orcamento_coupa=>zif_integracao_orcamento_coupa~get_instance(
          )->set_init_import( EXPORTING
                               iv_id           = ls_id
                               iv_is_approved  = ls_is_approved
                               iv_mensagem     = ls_mensagem
                             IMPORTING
                               e_retorno_integracao = DATA(e_zintegracao_log)
                              ).

        e_nm_code = '200'.
        e_sucesso = abap_true.

        e_msg_outbound = '{ "status" : "'       && ls_status    && '"' && cl_abap_char_utilities=>newline && ',' &&
                            '"mensagem" : "'    && ls_mensagem  && '"' && cl_abap_char_utilities=>newline && '}'.

      CATCH zcx_integracao INTO DATA(ex_integra).
        e_nm_code      = '400'.
        e_sucesso      = abap_true.
        e_msg_outbound = e_zintegracao_log-ds_data_retorno.
      CATCH zcx_error INTO DATA(ex_error).
        e_nm_code      = '400'.
        e_sucesso      = abap_true.
        e_msg_outbound = e_zintegracao_log-ds_data_retorno.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
  ENDMETHOD.
ENDCLASS.
