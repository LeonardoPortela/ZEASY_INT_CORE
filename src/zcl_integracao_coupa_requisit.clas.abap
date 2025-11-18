class ZCL_INTEGRACAO_COUPA_REQUISIT definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COUPA_REQUISIT .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_REQUISIT IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_requisit.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD zif_integracao_coupa_requisit~get_instance.

    CREATE OBJECT zif_integracao_coupa_requisit~at_if_integracao_requisit
      TYPE zcl_integracao_coupa_requisit.

    r_if_integracao_requisit = zif_integracao_coupa_requisit~at_if_integracao_requisit.

  ENDMETHOD.


  METHOD zif_integracao_coupa_requisit~set_ds_data.

    r_if_integracao_requisit = me.

    CHECK i_info IS NOT INITIAL.

    /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body
                               CHANGING  data = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa ).


    me->zif_integracao_inject~at_info_request_http-ds_body = i_info-ds_body.

  ENDMETHOD.


  METHOD zif_integracao_coupa_requisit~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_if_integracao_requisit = me.

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


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    " codigo antigo
    DATA: ls_class_contabil_coupa TYPE zclassificacao_contabil_coupa,
          ls_parametros_orcamento TYPE zparametros_verifca_orcamento,
          ls_codigo_operacao      TYPE c LENGTH 2,
          ls_status               TYPE string,
          ls_saldo                TYPE string,
          ls_mensagem             TYPE string,
          ls_mess_full            TYPE string,
          ls_id                   TYPE string,
          ls_anln2                TYPE anln2,
          ls_is_approved          TYPE xflag.

    DATA(lt_requi) = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-requisition_lines.

    LOOP AT lt_requi ASSIGNING FIELD-SYMBOL(<fs_requi>).

      IF <fs_requi>-account_allocations IS INITIAL.

        IF <fs_requi>-account IS NOT INITIAL.

          APPEND INITIAL LINE TO <fs_requi>-account_allocations ASSIGNING FIELD-SYMBOL(<fs_alloc>).

          <fs_alloc>-id = <fs_requi>-account-id.
          <fs_alloc>-amount = <fs_requi>-total.
*          <fs_alloc>-pct =
          <fs_alloc>-account-id = <fs_requi>-account-id.
          <fs_alloc>-account-name = <fs_requi>-account-name.
          <fs_alloc>-account-code = <fs_requi>-account-code.

        ENDIF.

      ENDIF.

    ENDLOOP.

*    READ TABLE me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-requisition_lines INTO DATA(ls_lines) INDEX 1.
*    IF sy-subrc IS INITIAL.
*      IF ls_lines-account IS NOT INITIAL.
*        SPLIT ls_lines-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
*                                                ls_class_contabil_coupa              ls_parametros_orcamento-kostl
*                                                ls_parametros_orcamento-cost_object  ls_codigo_operacao
*                                                ls_parametros_orcamento-saknr.
*        IF ls_class_contabil_coupa EQ 'A'.
*          SPLIT ls_lines-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
*                                                  ls_class_contabil_coupa              ls_parametros_orcamento-kostl
*                                                  ls_parametros_orcamento-cost_object  ls_anln2
*                                                  ls_codigo_operacao                   ls_parametros_orcamento-saknr.
*
*          CONCATENATE ls_parametros_orcamento-cost_object '-' ls_anln2 INTO ls_parametros_orcamento-cost_object.
*        ENDIF.
*      ELSE.

    " RAMON - CORREÇÃO PARA MAIS DE UM ITEM -->>>

    LOOP AT lt_requi INTO DATA(ls_req_lines).

      LOOP AT ls_req_lines-account_allocations ASSIGNING <fs_alloc>.

        FREE: ls_parametros_orcamento.

        SPLIT <fs_alloc>-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
                                                  ls_class_contabil_coupa              ls_parametros_orcamento-kostl
                                                  ls_parametros_orcamento-cost_object  ls_codigo_operacao
                                                  ls_parametros_orcamento-saknr.
        IF ls_class_contabil_coupa EQ 'A'.

          SPLIT <fs_alloc>-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
                                                    ls_class_contabil_coupa              ls_parametros_orcamento-kostl
                                                    ls_parametros_orcamento-cost_object  ls_anln2
                                                    ls_codigo_operacao                   ls_parametros_orcamento-saknr.

          CONCATENATE ls_parametros_orcamento-cost_object '-' ls_anln2 INTO ls_parametros_orcamento-cost_object.

        ENDIF.

        ls_parametros_orcamento-currency = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-currency-code.
        ls_parametros_orcamento-total    = ls_req_lines-total.

        CLEAR ls_mensagem.

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

        IF ls_mess_full NS ls_mensagem.
          ls_mess_full = ls_mess_full && `, ` && ls_mensagem.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

**    LOOP AT me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-requisition_lines INTO DATA(ls_req_lines).
**
**
**    FREE: ls_parametros_orcamento.
**
**    SPLIT ls_req_lines-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
**                                                          ls_class_contabil_coupa              ls_parametros_orcamento-kostl
**                                                          ls_parametros_orcamento-cost_object  ls_codigo_operacao
**                                                          ls_parametros_orcamento-saknr.
**    IF ls_class_contabil_coupa EQ 'A'.
**      SPLIT ls_req_lines-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
**                                                            ls_class_contabil_coupa              ls_parametros_orcamento-kostl
**                                                            ls_parametros_orcamento-cost_object  ls_anln2
**                                                            ls_codigo_operacao                   ls_parametros_orcamento-saknr.
**
**      CONCATENATE ls_parametros_orcamento-cost_object '-' ls_anln2 INTO ls_parametros_orcamento-cost_object.
**    ENDIF.
**
**    ls_parametros_orcamento-currency = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-currency-code.
**    ls_parametros_orcamento-total    = ls_req_lines-total.
**
**    CLEAR ls_mensagem.
**
**    CALL FUNCTION 'Z_VERIFICA_ORCAMENTO_COUPA'
**      EXPORTING
**        iv_class_contabil_coupa       = ls_class_contabil_coupa
**        is_parametros_orcamento       = ls_parametros_orcamento
**      IMPORTING
**        ev_status                     = ls_status
**        ev_saldo                      = ls_saldo
**        ev_mensagem                   = ls_mensagem
**      EXCEPTIONS
**        class_contabil_coupa_invalida = 1
**        OTHERS                        = 2.
**
**    IF sy-subrc <> 0.
**    ENDIF.
**
**    ls_mess_full = ls_mess_full && `, ` && ls_mensagem.
**
**  ENDLOOP.

    ls_mess_full = ls_mess_full+2.

    " RAMON - CORREÇÃO PARA MAIS DE UM ITEM --<<<<<<



**        LOOP AT ls_lines-account_allocations INTO DATA(ls_account_allocations).
**          FREE: ls_parametros_orcamento.
**
**          SPLIT ls_account_allocations-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
**                                                                ls_class_contabil_coupa              ls_parametros_orcamento-kostl
**                                                                ls_parametros_orcamento-cost_object  ls_codigo_operacao
**                                                                ls_parametros_orcamento-saknr.
**          IF ls_class_contabil_coupa EQ 'A'.
**            SPLIT ls_account_allocations-account-code AT '-' INTO ls_parametros_orcamento-bukrs        ls_parametros_orcamento-werks
**                                                                  ls_class_contabil_coupa              ls_parametros_orcamento-kostl
**                                                                  ls_parametros_orcamento-cost_object  ls_anln2
**                                                                  ls_codigo_operacao                   ls_parametros_orcamento-saknr.
**
**            CONCATENATE ls_parametros_orcamento-cost_object '-' ls_anln2 INTO ls_parametros_orcamento-cost_object.
**          ENDIF.
**
**          ls_parametros_orcamento-currency = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-currency-code.
**          ls_parametros_orcamento-total    = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-total.
**
**          CALL FUNCTION 'Z_VERIFICA_ORCAMENTO_COUPA'
**            EXPORTING
**              iv_class_contabil_coupa       = ls_class_contabil_coupa
**              is_parametros_orcamento       = ls_parametros_orcamento
**            IMPORTING
**              ev_status                     = ls_status
**              ev_saldo                      = ls_saldo
**              ev_mensagem                   = ls_mensagem
**            EXCEPTIONS
**              class_contabil_coupa_invalida = 1
**              OTHERS                        = 2.
**          IF sy-subrc <> 0.
**          ENDIF.
**
**          IF ls_status EQ 'rejected'.
**            EXIT.
**          ENDIF.
**
**        ENDLOOP.
*  ENDIF.
*    ENDIF.

    ls_parametros_orcamento-currency = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-currency-code.
    ls_parametros_orcamento-total    = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-total.

    ls_id = me->zif_integracao_coupa_requisit~at_requisicao_compra_coupa-current_approval-id.

*    IF ls_status IS INITIAL.
*      CALL FUNCTION 'Z_VERIFICA_ORCAMENTO_COUPA'
*        EXPORTING
*          iv_class_contabil_coupa       = ls_class_contabil_coupa
*          is_parametros_orcamento       = ls_parametros_orcamento
*        IMPORTING
*          ev_status                     = ls_status
*          ev_saldo                      = ls_saldo
*          ev_mensagem                   = ls_mensagem
*        EXCEPTIONS
*          class_contabil_coupa_invalida = 1
*          OTHERS                        = 2.
*      IF sy-subrc <> 0.
*      ENDIF.
*    ENDIF.

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
                               iv_mensagem     = ls_mess_full
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


  METHOD zif_integracao_inject~set_integrar_retorno.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
  ENDMETHOD.
ENDCLASS.
