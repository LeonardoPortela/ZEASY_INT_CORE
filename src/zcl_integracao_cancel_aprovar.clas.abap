class ZCL_INTEGRACAO_CANCEL_APROVAR definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_CANCEL_APROVAR .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_CANCEL_APROVAR IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE    = ZIF_INTEGRACAO=>AT_ID_INTERFACE_CANCEL_APROVAR.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO   = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_OUTBOUND.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL        = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_OPUS  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_OPUS_SIM.
    ME->ZIF_INTEGRACAO_INJECT~AT_SEND_AUTENTICAO = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_SEND_SIM.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @ZIF_INTEGRACAO=>AT_ID_INTERFACE_CANCEL_APROVAR.

    IF SY-SUBRC EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    ME->ZIF_INTEGRACAO_CANCEL_APROVAR~SET_DS_URL( ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CANCEL_APROVAR~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_CANCEL_APROVAR = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_CANCEL_APROVAR'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_CANCEL_APROVAR~AT_VIAGEM-VIAGEM_ID.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CANCEL_APROVAR~GET_INSTANCE.

    IF ZIF_INTEGRACAO_CANCEL_APROVAR~AT_IF_INTEGRACAO_CANCEL_APR IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_CANCEL_APROVAR~AT_IF_INTEGRACAO_CANCEL_APR TYPE ZCL_INTEGRACAO_CANCEL_APROVAR.
    ENDIF.
    R_IF_INTEGRACAO_CANCEL_APROVAR = ZIF_INTEGRACAO_CANCEL_APROVAR~AT_IF_INTEGRACAO_CANCEL_APR.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CANCEL_APROVAR~GET_JSON.

    R_IF_INTEGRACAO_CANCEL_APROVAR = ME.

    E_JSON = ''.

  ENDMETHOD.


  method ZIF_INTEGRACAO_CANCEL_APROVAR~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_CANCEL_APROVAR = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  endmethod.


  METHOD ZIF_INTEGRACAO_CANCEL_APROVAR~SET_DS_URL.

    R_IF_INTEGRACAO_CANCEL_APROVAR = ME.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @ZIF_INTEGRACAO=>AT_ID_INTERFACE_CANCEL_APROVAR.

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

      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO          = 'JSON'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE     = LWA_ZAUTH_WEBSERVICE-CONTENT_TYPE.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_CANCEL_APROVAR~AT_VIAGEM-VIAGEM_ID && '/cancelamento/aprovar'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN = LWA_ZAUTH_WEBSERVICE-URL_TOKEN.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_CANCEL_APROVAR=>AT_FC_PROCESSAR_CANCEL_APROVAR.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

      ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-TP_REFERENCIA = 'CARGUERO_CANCEL_APROVAR'.
      ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_CANCEL_APROVAR~AT_VIAGEM-VIAGEM_ID.

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
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_CANCEL_APROVAR~AT_VIAGEM-VIAGEM_ID && '/cancelamento/aprovar'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN = WA_WEBSERVICE-URL_TOKEN.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_CANCEL_APROVAR=>AT_FC_PROCESSAR_CANCEL_APROVAR.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-TP_REFERENCIA = 'CARGUERO_CANCEL_APROVAR'.
    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_CANCEL_APROVAR~AT_VIAGEM-VIAGEM_ID.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_CANCEL_APROVAR~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_CANCEL_APROVAR = ME.
    ME->ZIF_INTEGRACAO_CANCEL_APROVAR~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_CANCEL_APROVAR~SET_NEW_CANCEL_APROVAR.

    R_IF_INTEGRACAO_CANCEL_APROVAR = ME.

    SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_CANCEL_APROVAR~AT_VIAGEM
      FROM ZLEST0185
     WHERE VIAGEM_ID EQ @I_VIAGEM_ID.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_CANCEL_APROVAR~GET_JSON( IMPORTING E_JSON = DATA(LC_JSON)
      )->SET_DS_DATA( EXPORTING I_JSON = LC_JSON
      )->SET_DS_URL(
      )->SET_ID_REFERENCIA(
      )->SET_SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      ).

  endmethod.


  method ZIF_INTEGRACAO_CANCEL_APROVAR~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_CANCEL_APROVAR = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  endmethod.


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

        "Já foi liberado o embarque e está com o status liberação do carregamento
        CHECK E_STATUS EQ '05'.

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
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

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


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.
ENDCLASS.
