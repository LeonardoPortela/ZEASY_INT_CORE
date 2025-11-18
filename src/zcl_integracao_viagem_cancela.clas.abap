class ZCL_INTEGRACAO_VIAGEM_CANCELA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_VIAGEM_CANCELA .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_VIAGEM_CANCELA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface  = zif_integracao=>at_id_interface_via_cancelar.
    me->zif_integracao_inject~at_tp_integracao = zif_integracao=>at_tp_integracao_inbound.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'CARGUERO_VIAGEM_CANCELAR'.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.


  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_FALSE.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: I_INBOUND   TYPE ZDE_CARGUERO_CANCELA_VIAGEM,
          LC_APROVAR  TYPE REF TO ZCL_INTEGRACAO_CANCEL_APROVAR,
          LC_REJEITAR TYPE REF TO ZCL_INTEGRACAO_CANCEL_REJEITAR.

    R_IF_INTEGRACAO_INJECT = ME.

    CLEAR: E_MSG_OUTBOUND, E_SUCESSO.

    " Cadastrar/Alterar/Deletar Placa
    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND  CHANGING DATA = I_INBOUND ).

    SELECT SINGLE * INTO @DATA(WA_ZLEST0185)
      FROM ZLEST0185
     WHERE VIAGEM_ID EQ @I_INBOUND-DATA-VIAGEM_ID.

    IF SY-SUBRC IS NOT INITIAL.
      E_SUCESSO = ABAP_TRUE. "Não é pra cair nessa condicão, mas caso cair, deve finalizar o processamento do registro...
      RETURN.
    ENDIF.

    CASE WA_ZLEST0185-CK_CANCELADO.
      WHEN ABAP_TRUE.

        "Autorizar Cancelamento da Viagem
        CREATE OBJECT LC_APROVAR.
        LC_APROVAR->ZIF_INTEGRACAO_CANCEL_APROVAR~SET_NEW_CANCEL_APROVAR(
              EXPORTING I_VIAGEM_ID = WA_ZLEST0185-VIAGEM_ID IMPORTING E_ID_INTEGRACAO = WA_ZLEST0185-ID_INTEGRACAO_CAN_APROVAR
          ).
        CLEAR: LC_APROVAR.

        E_SUCESSO = ABAP_TRUE.
        UPDATE ZLEST0185
           SET ID_INTEGRACAO_CAN_APROVAR = WA_ZLEST0185-ID_INTEGRACAO_CAN_APROVAR
               CK_INTEGRADA              = ABAP_TRUE
         WHERE VIAGEM_ID EQ WA_ZLEST0185-VIAGEM_ID.
        COMMIT WORK.

      WHEN ABAP_FALSE.

        "Rejeitar Cancelamento da Viagem
        CREATE OBJECT LC_REJEITAR.
        LC_REJEITAR->ZIF_INTEGRACAO_CANCEL_REJEITAR~SET_NEW_CANCEL_REJEITAR(
              EXPORTING I_VIAGEM_ID = WA_ZLEST0185-VIAGEM_ID IMPORTING E_ID_INTEGRACAO = WA_ZLEST0185-ID_INTEGRACAO_REJEITAR
          ).
        CLEAR: LC_REJEITAR.

        E_SUCESSO = ABAP_TRUE.
        UPDATE ZLEST0185
           SET ID_INTEGRACAO_CAN_REJEITAR = WA_ZLEST0185-ID_INTEGRACAO_CAN_REJEITAR
               CK_INTEGRADA               = ABAP_TRUE
         WHERE VIAGEM_ID EQ WA_ZLEST0185-VIAGEM_ID.
        COMMIT WORK.

    ENDCASE.


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    "Metodo para Integrar InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.

    DATA: i_inbound       TYPE zde_carguero_cancela_viagem,
          lc_msg_ret_canc TYPE char200,
          lc_msg          TYPE zlest0185tx-tx_validacao_cancelar.

    r_if_integracao_inject = me.

    " Ler JSON Recebido """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Cadastrar/Alterar/Deletar Placa
    /ui2/cl_json=>deserialize( EXPORTING json = i_msg CHANGING data = i_inbound ).

    "Criar Cadastro da Viagem no SAP """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(wa_zlest0185)
      FROM zlest0185
     WHERE viagem_id EQ @i_inbound-data-viagem_id.

    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676 --->>

    IF ( sy-subrc IS NOT INITIAL ).
      CONCATENATE 'Viagem ID:' i_inbound-data-viagem_id 'não existe!' INTO lc_msg SEPARATED BY space.
    ENDIF.

    DATA(_processo_definido) = abap_false.

*-----------------------------------------------------------------------------------*
*     Validações via Ordem Carregamento no OPUS
*-----------------------------------------------------------------------------------*
    IF ( wa_zlest0185-id_ordem IS NOT INITIAL ) AND ( lc_msg IS INITIAL ).

      _processo_definido = abap_true.

      "Verificar Ordem de Carregamento
      SELECT SINGLE * INTO @DATA(wa_zsdt0001)
        FROM zsdt0001
       WHERE id_ordem EQ @wa_zlest0185-id_ordem.

      "Verificar se Tem Romaneio
      SELECT SINGLE * INTO @DATA(wa_zsdt0001od)
        FROM zsdt0001od
       WHERE id_ordem EQ @wa_zlest0185-id_ordem.

      SELECT SINGLE * INTO @DATA(wa_zlest0108)
        FROM zlest0108
       WHERE id_ordem EQ @wa_zlest0185-id_ordem.

      IF wa_zsdt0001  IS NOT INITIAL OR
         wa_zlest0108 IS NOT INITIAL.  "*-#137606-04.04.2024-JT
        lc_msg = 'Existe uma Carga com esta Viagem!'.
      ENDIF.

      IF wa_zsdt0001od-tp_status EQ zcl_ordem_carregamento=>st_tp_status_fechado.
        lc_msg = 'Ordem de Carregamento está Fechada para esta Viagem!'.
      ENDIF.

      "Cancelar no OPUS a Ordem de Carregamento
      IF ( lc_msg IS INITIAL ) AND ( wa_zsdt0001od IS NOT INITIAL ).
        TRY .
            zcl_cancelar_oc_opus=>zif_cancelar_oc_opus~get_instance(
                       )->set_ordem_carregamento( i_id_ordem = wa_zsdt0001od-id_ordem
                       )->set_cancelar_ordem_carrega( EXPORTING i_zlest0185     = wa_zlest0185
                                                      IMPORTING e_id_integracao = wa_zlest0185-id_integracao_can_oc
                                                                e_integracao    = DATA(e_integracao)
                                                                e_cancelada     = DATA(e_oc_cancelada) ).

            IF e_oc_cancelada EQ abap_false.
              lc_msg_ret_canc = e_integracao-ds_data_retorno.
              MESSAGE s018(zintegra) WITH lc_msg_ret_canc+000(50)
                                          lc_msg_ret_canc+050(50)
                                          lc_msg_ret_canc+100(50)
                                          lc_msg_ret_canc+150(50) INTO lc_msg.
            ENDIF.

          CATCH zcx_integracao INTO DATA(ex_integracao).
            lc_msg = ex_integracao->zif_error~get_msg_erro( ).
          CATCH zcx_error INTO DATA(ex_error).
            lc_msg = ex_error->zif_error~get_msg_erro( ).
        ENDTRY.
      ENDIF.

    ENDIF.

*-----------------------------------------------------------------------------------*
*     Validações via Autorização Embarque Insumos - Carga Entrada
*-----------------------------------------------------------------------------------*
    IF ( lc_msg IS INITIAL ) AND ( _processo_definido EQ abap_false ).

      SELECT SINGLE *
        FROM zmmt0201 INTO @DATA(lwa_zmmt0201)
       WHERE viagem_id EQ @wa_zlest0185-viagem_id.

      IF sy-subrc EQ 0.

        _processo_definido = abap_true.

        SELECT SINGLE *
          from zmmt0203 INTO @DATA(lwa_zmmt0203)
         WHERE nro_cg eq @lwa_zmmt0201-nro_cg.

        IF sy-subrc eq 0.
          lc_msg = 'A Carga para essa viagem já possui notas vinculadas! Operação não permitida!'.
        ENDIF.

        IF lc_msg IS INITIAL.
          zcl_carga_entrada_insumos=>cancela_carga(
          EXPORTING
            i_nro_carga = lwa_zmmt0201-nro_cg
            i_motivo    = 'Carga Cancelada pelo Sistema Carguero/Strada!'
          IMPORTING
            e_msg_erro = DATA(lv_msg) ).

          if lv_msg is NOT INITIAL.
            CONCATENATE 'Não foi possivel cancelar a Carga: Motivo:' lc_msg INTO lc_msg SEPARATED BY space.
          endif.
        ENDIF.

      ENDIF.

    ENDIF.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>

*-----------------------------------------------------------------------------------*
*     Validações via Autorização Embarque Insumos - Carga Saida
*-----------------------------------------------------------------------------------*

    IF ( lc_msg IS INITIAL ) AND ( _processo_definido EQ abap_false ).

      DATA: lwa_dados_logistico TYPE zsds382.

      SELECT SINGLE *
        FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
       WHERE viagem_id EQ @wa_zlest0185-viagem_id.

      IF sy-subrc EQ 0.

        _processo_definido = abap_true.

        SELECT SINGLE *
          from zsdt0410 INTO @DATA(lwa_zsdt0410)
         WHERE nro_cg eq @lwa_zsdt0133-nro_cg.

        IF sy-subrc eq 0.
          lc_msg = 'A Carga para essa viagem já possui notas vinculadas! Operação não permitida!'.
        ENDIF.

        IF lc_msg IS INITIAL.
           "Apagar Dados Logistico Carga
          CLEAR: lwa_dados_logistico.
          lwa_dados_logistico-nro_cg = lwa_zsdt0133-nro_cg.
          lc_msg = zcl_carga_saida_insumos=>atualizar_dados_logistico( i_header = lwa_dados_logistico i_remove_data_aut_emb = abap_true ).

          if lc_msg is NOT INITIAL.
            CONCATENATE 'Não foi possivel cancelar a Carga: Motivo:' lc_msg INTO lc_msg SEPARATED BY space.
          endif.
        ENDIF.

      ENDIF.

    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

*    IF ( sy-subrc IS INITIAL ) AND ( wa_zlest0185-id_ordem IS NOT INITIAL ).
*
*      "Verificar Ordem de Carregamento
*      SELECT SINGLE * INTO @DATA(wa_zsdt0001)
*        FROM zsdt0001
*       WHERE id_ordem EQ @wa_zlest0185-id_ordem.
*
*      "Verificar se Tem Romaneio
*      SELECT SINGLE * INTO @DATA(wa_zsdt0001od)
*        FROM zsdt0001od
*       WHERE id_ordem EQ @wa_zlest0185-id_ordem.
*
**-#137606-04.04.2024-JT-inicio
*      SELECT SINGLE * INTO @DATA(wa_zlest0108)
*        FROM zlest0108
*       WHERE id_ordem EQ @wa_zlest0185-id_ordem.
**-#137606-04.04.2024-JT-fim
*
*      IF wa_zsdt0001  IS NOT INITIAL OR
*         wa_zlest0108 IS NOT INITIAL.  "*-#137606-04.04.2024-JT
*        lc_msg = 'Existe uma Carga com esta Viagem!'.
*      ENDIF.
*
*      IF wa_zsdt0001od-tp_status EQ zcl_ordem_carregamento=>st_tp_status_fechado.
*        lc_msg = 'Ordem de Carregamento está Fechada para esta Viagem!'.
*      ENDIF.
*
*    ELSE.
*
*      IF ( sy-subrc IS NOT INITIAL ).
*        CONCATENATE 'Viagem ID:' i_inbound-data-viagem_id 'não existe!' INTO lc_msg SEPARATED BY space.
*      ELSE.
*        CONCATENATE 'Viagem ID:' i_inbound-data-viagem_id 'sem Ordem de Carregamento!' INTO lc_msg SEPARATED BY space.
*      ENDIF.
*
*    ENDIF.

*    "Cancelar no OPUS a Ordem de Carregamento
*    IF ( lc_msg IS INITIAL ) AND ( wa_zsdt0001od IS NOT INITIAL ).
*      TRY .
*          zcl_cancelar_oc_opus=>zif_cancelar_oc_opus~get_instance(
*                     )->set_ordem_carregamento( i_id_ordem = wa_zsdt0001od-id_ordem
*                     )->set_cancelar_ordem_carrega( EXPORTING i_zlest0185     = wa_zlest0185
*                                                    IMPORTING e_id_integracao = wa_zlest0185-id_integracao_can_oc
*                                                              e_integracao    = DATA(e_integracao)
*                                                              e_cancelada     = DATA(e_oc_cancelada) ).
*
*          IF e_oc_cancelada EQ abap_false.
*            lc_msg_ret_canc = e_integracao-ds_data_retorno.
*            MESSAGE s018(zintegra) WITH lc_msg_ret_canc+000(50)
*                                        lc_msg_ret_canc+050(50)
*                                        lc_msg_ret_canc+100(50)
*                                        lc_msg_ret_canc+150(50) INTO lc_msg.
*          ENDIF.
*
*        CATCH zcx_integracao INTO DATA(ex_integracao).
*          lc_msg = ex_integracao->zif_error~get_msg_erro( ).
*        CATCH zcx_error INTO DATA(ex_error).
*          lc_msg = ex_error->zif_error~get_msg_erro( ).
*      ENDTRY.
*    ENDIF.

    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676 <<---


    IF lc_msg IS INITIAL.
      wa_zlest0185-ck_cancelado = abap_true.
    ENDIF.

    CASE wa_zlest0185-ck_cancelado.

      WHEN abap_true.
        "Sucesso
        UPDATE zlest0185
           SET ck_cancelado         = abap_true
               id_integracao_can_oc = wa_zlest0185-id_integracao_can_oc
         WHERE viagem_id EQ wa_zlest0185-viagem_id.

        "Erro
        UPDATE zlest0185tx
           SET tx_validacao_cancelar = 'Processada com Sucesso!'
         WHERE viagem_id EQ wa_zlest0185-viagem_id.

        COMMIT WORK.

      WHEN abap_false.
        "Erro
        UPDATE zlest0185tx
           SET tx_validacao_cancelar = lc_msg
         WHERE viagem_id EQ wa_zlest0185-viagem_id.

        COMMIT WORK.

    ENDCASE.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    "Metodo para processoamento de InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_CANCELA~GET_INSTANCE.

    IF ZIF_INTEGRACAO_VIAGEM_CANCELA~AT_IF_INTE_VIAGEM_CANCELA IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_VIAGEM_CANCELA~AT_IF_INTE_VIAGEM_CANCELA TYPE ZCL_INTEGRACAO_VIAGEM_CANCELA.
    ENDIF.
    R_IF_INTEGRACAO_VIAGEM_CANCELA = ZIF_INTEGRACAO_VIAGEM_CANCELA~AT_IF_INTE_VIAGEM_CANCELA.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_CANCELA~SET_DS_DATA.

    DATA: I_INBOUND TYPE ZDE_CARGUERO_CANCELA_VIAGEM.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_VIAGEM_CANCELA = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.

    "Validar Json
    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = I_INBOUND ).
    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = I_INBOUND-DATA-VIAGEM_ID.
    CHECK I_INBOUND-DATA-VIAGEM_ID IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_INTEGRACAO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
                          MSGNO = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO )
        MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
        MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_CANCELA~SET_SEND_MSG.

    DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_VIAGEM_CANCELA = ME.

    "Verificar a Função de Cada requisição
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = '/cancelamento/solicitar'.

    CREATE OBJECT LC_INTEGRACAO.

    LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_VIAGEM_CANCELA~AT_ID_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRACAO.

  ENDMETHOD.
ENDCLASS.
