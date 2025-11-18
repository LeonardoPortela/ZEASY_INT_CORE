class ZCL_INTEGRACAO_VIAGEM_REJEITA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_VIAGEM_REJEITAR .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_VIAGEM_REJEITA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_rejeita.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_rejeita.

    IF SY-SUBRC EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    me->zif_integracao_viagem_rejeitar~set_ds_url( ).

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.

    DATA: LC_ID_VIAGEM TYPE ZDE_VIAGEM_ID.

    TRY .

        ZCL_INTEGRACAO_VIAGEM_STATUS=>ZIF_INTEGRACAO_VIAGEM_STATUS~GET_INSTANCE(
          )->SET_VIAGEM_STATUS( EXPORTING
                                  I_VIAGEM_ID = C_INTEGRACAO-ID_REFERENCIA
                                IMPORTING
                                  E_ID_INTEGRACAO  = DATA(E_ID_INTEGRACAO)
                                  E_STATUS         = DATA(E_STATUS)
          ).

*01	Carregamento
*02	Liberação do Embarcador
*03	Carregado
*04	Descarregado
*05	Cancelado
*06	Rejeitado pelo Embarcador
*07	Emissão de Nota Fiscal de Trânsito
*08	Liberação do Carregamento
*09	Carregamento Rejeitado pelo Embarcador
*10	Carregamento
*11	Resposta da Solicitação de Cancelamento
*12	Carregamento
*13	Resposta da Solicitação de Cancelamento
*14	Cancelado

        "Já foi rejeitado o embarque e está com o status Rejeitado pelo Embarcador
        CHECK E_STATUS EQ '06'.

        E_SUCESSO = ABAP_TRUE.

        C_INTEGRACAO-ID_BEFORE_ERROR_OUTBOUND = E_ID_INTEGRACAO.

      CATCH ZCX_INTEGRACAO.
      CATCH ZCX_ERROR.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    SELECT SINGLE * INTO @DATA(wa_viagem)
      FROM zlest0185
     WHERE viagem_id EQ @i_integracao-id_referencia.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgid
                            msgno = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgno
                            attr1 = CONV #( i_integracao-id_referencia ) )
          msgid  = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgid
          msgno  = zcx_integracao=>zcx_id_integracao_nao_econtrad-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_integracao-id_referencia ).
    ENDIF.

    TRY .
        CAST zcl_integracao_token(
               zcl_integracao_token=>zif_integracao_token~get_instance(
                 )->set_empresa_token( EXPORTING i_bukrs = wa_viagem-bukrs
                 )
             )->zif_integracao_inject~get_header_request_http( IMPORTING e_header_fields = DATA(e_header_fields) ).

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_VIAGEM_REJEITAR~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_VIAGEM_REJ = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_VIAGEM_APROVAR'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM-VIAGEM_ID.

  endmethod.


  METHOD ZIF_INTEGRACAO_VIAGEM_REJEITAR~GET_INSTANCE.

    IF ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_IF_INTEGRACAO_VIAGEM_REJ IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_IF_INTEGRACAO_VIAGEM_REJ TYPE ZCL_INTEGRACAO_VIAGEM_REJEITA.
    ENDIF.
    R_IF_INTEGRACAO_VIAGEM_REJ = ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_IF_INTEGRACAO_VIAGEM_REJ.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_REJEITAR~GET_JSON.

    R_IF_INTEGRACAO_VIAGEM_REJ = ME.

    DATA(MSG_VIAG) = 'Mensagem Viagem: '
                     && COND STRING( WHEN ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_VIAGEM IS INITIAL THEN 'Sem Mensagem de Processamento de Viagem'
                                     ELSE ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_VIAGEM ).

    DATA(MSG_PROP) = 'Mensagem Proprietario: '
                     && COND STRING( WHEN ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_PROP IS INITIAL THEN 'Sem Mensagem de Processamento do Proprietário do(s) Veículo(s)'
                                     ELSE ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_PROP ).

    DATA(MSG_VEIC) = 'Mensagem Veículos: '
                     && COND STRING( WHEN ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_VEICULOS IS INITIAL THEN 'Sem Mensagem de Processamento do(s) Veículo(s)'
                                     ELSE ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_VEICULOS ).

    DATA(MSG_MOTO) = 'Mensagem Motorista: '
                     && COND STRING( WHEN ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_MOT IS INITIAL THEN 'Sem Mensagem de Processamento do Motorista'
                                     ELSE ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_MOT ).

    DATA(MSG_PREC) = 'Mensagem Preço de Frete: '
                     && COND STRING( WHEN ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_PRECO IS INITIAL THEN 'Sem Mensagem de Processamento do Preço de Frete'
                                     ELSE ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_PRECO ).

    DATA(MSG_OC)   = 'Mensagem Solicitação Ordem Carregamento OPUS: '
                     && COND STRING( WHEN ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_OC IS INITIAL THEN 'Sem Mensagem de Processamento de Solicitação Ordem Carregamento OPUS'
                                     ELSE ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_VALIDACAO_OC ).

    DATA(MSG_ERRO) = ZCL_STRING=>CONCAT( S1 = MSG_VIAG S2 = MSG_PROP SP = CONV #( CL_ABAP_CHAR_UTILITIES=>NEWLINE ) ).
    MSG_ERRO = ZCL_STRING=>CONCAT( S1 = MSG_ERRO S2 = MSG_VEIC SP = CONV #( CL_ABAP_CHAR_UTILITIES=>NEWLINE ) ).
    MSG_ERRO = ZCL_STRING=>CONCAT( S1 = MSG_ERRO S2 = MSG_MOTO SP = CONV #( CL_ABAP_CHAR_UTILITIES=>NEWLINE ) ).
    MSG_ERRO = ZCL_STRING=>CONCAT( S1 = MSG_ERRO S2 = MSG_PREC SP = CONV #( CL_ABAP_CHAR_UTILITIES=>NEWLINE ) ).
    MSG_ERRO = ZCL_STRING=>CONCAT( S1 = MSG_ERRO S2 = MSG_OC SP = CONV #( CL_ABAP_CHAR_UTILITIES=>NEWLINE ) ).

    E_JSON = '{ '  && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
      ' "motivo": " ' && ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX-TX_PROCESSAMENTO ) && '" , ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
      ' "motivo_rejeicao_suporte" : "' && ZCL_STRING=>TIRA_ACENTOS( ZCL_STRING=>CONVERT_TO_UTF8( MSG_ERRO ) ) && '" ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
      ' }'.

  ENDMETHOD.


  method ZIF_INTEGRACAO_VIAGEM_REJEITAR~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_VIAGEM_REJ = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  endmethod.


  METHOD ZIF_INTEGRACAO_VIAGEM_REJEITAR~SET_DS_URL.

    R_IF_INTEGRACAO_VIAGEM_REJ = ME.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_rejeita.

    IF SY-SUBRC EQ 0.

      SELECT SINGLE * INTO @DATA(LWA_ZAUTH_WEBSERVICE)
        FROM ZAUTH_WEBSERVICE
       WHERE SERVICE = 'CARGUERO_HOST'.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
                              MSGNO = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
                              ATTR1 = 'O'
                              ATTR2 = '01' )
            MSGID  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
            MSGNO  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
            MSGTY  = 'E'
            MSGV1  = 'O'
            MSGV2  = '01'.
      ENDIF.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
                              MSGNO = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
                              ATTR1 = 'O'
                              ATTR2 = '01' )
            MSGID  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
            MSGNO  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
            MSGTY  = 'E'
            MSGV1  = 'O'
            MSGV2  = '01'.
      ENDIF.

      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO          = 'JSON'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE     = LWA_ZAUTH_WEBSERVICE-CONTENT_TYPE.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM-VIAGEM_ID && '/rejeitar'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN = LWA_ZAUTH_WEBSERVICE-URL_TOKEN.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_VIAGEM_REJEITAR=>AT_FC_PROCESSAR_VIAGEM_REJEITA.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

      ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-TP_REFERENCIA = 'CARGUERO_VIAGEM_REJEITAR'.
      ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM-VIAGEM_ID.

      EXIT.

    ENDIF.


    SELECT SINGLE * INTO @DATA(WA_WEBSERVICE)
      FROM ZCIOT_WEBSERVICE
     WHERE TIPO    EQ 'O'
       AND SERVICO EQ '01'.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
                            ATTR1 = 'O'
                            ATTR2 = '01' )
          MSGID  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
          MSGTY  = 'E'
          MSGV1  = 'O'
          MSGV2  = '01'.
    ENDIF.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO          = 'JSON'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE     = WA_WEBSERVICE-CONTENT_TYPE.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM-VIAGEM_ID && '/rejeitar'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN = WA_WEBSERVICE-URL_TOKEN.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_VIAGEM_REJEITAR=>AT_FC_PROCESSAR_VIAGEM_REJEITA.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-TP_REFERENCIA = 'CARGUERO_VIAGEM_REJEITAR'.
    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM-VIAGEM_ID.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_REJEITAR~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_VIAGEM_REJ = ME.
    ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_VIAGEM_REJEITAR~SET_NEW_VIAGEM_REJEITAR.

    R_IF_INTEGRACAO_VIAGEM_REJ = ME.

    SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM
      FROM ZLEST0185
     WHERE VIAGEM_ID EQ @I_VIAGEM_ID.

    SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~AT_VIAGEM_TX
      FROM ZLEST0185TX
     WHERE VIAGEM_ID EQ @I_VIAGEM_ID.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_VIAGEM_REJEITAR~GET_JSON( IMPORTING E_JSON = DATA(LC_JSON)
      )->SET_DS_DATA( EXPORTING I_JSON = LC_JSON
      )->SET_DS_URL(
      )->SET_ID_REFERENCIA(
      )->SET_SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      ).

  endmethod.


  METHOD ZIF_INTEGRACAO_VIAGEM_REJEITAR~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_VIAGEM_REJ = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  ENDMETHOD.
ENDCLASS.
