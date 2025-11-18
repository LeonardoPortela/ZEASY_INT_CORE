class ZCL_INTEGRACAO_VIAGEM_PRECO definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_VIAGEM_PRECO .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_VIAGEM_PRECO IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_preco.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_preco.

    IF SY-SUBRC EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    me->zif_integracao_viagem_preco~set_ds_url( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
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


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_VIAGEM_PRECO~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_VIAGEM_PRECO = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_VIAGEM_PRECO_MOTO'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_VIAGEM_PRECO~AT_VIAGEM-VIAGEM_ID.

  endmethod.


  METHOD ZIF_INTEGRACAO_VIAGEM_PRECO~GET_INSTANCE.

    IF ZIF_INTEGRACAO_VIAGEM_PRECO~AT_IF_INTEGRACAO_VIAGEM_PRECO IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_VIAGEM_PRECO~AT_IF_INTEGRACAO_VIAGEM_PRECO
        TYPE ZCL_INTEGRACAO_VIAGEM_PRECO.
    ENDIF.
    R_IF_INTEGRACAO_VIAGEM_PRECO = ZIF_INTEGRACAO_VIAGEM_PRECO~AT_IF_INTEGRACAO_VIAGEM_PRECO.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_PRECO~GET_JSON.

    TYPES BEGIN OF TY_ALTERA_PRECO.
    TYPES: VALOR_FRETE_MOTORISTA TYPE ZVALOR_FRETE.
    TYPES END OF TY_ALTERA_PRECO.

    DATA: LC_PRECO TYPE TY_ALTERA_PRECO.

    R_IF_INTEGRACAO_VIAGEM_PRECO = ME.

    LC_PRECO-VALOR_FRETE_MOTORISTA = ME->ZIF_INTEGRACAO_VIAGEM_PRECO~AT_VIAGEM-NM_VR_FRETE_MOTO_NEGO.

    E_JSON = ZCL_FMCALL_BASE=>ABAP2JSON( ABAP_DATA = LC_PRECO ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_PRECO~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_VIAGEM_PRECO = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_PRECO~SET_DS_URL.

    R_IF_INTEGRACAO_VIAGEM_PRECO = ME.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_preco.

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
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN        = LWA_ZAUTH_WEBSERVICE-URL_TOKEN.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_VIAGEM_PRECO~AT_VIAGEM-VIAGEM_ID && '/valor/motorista'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_VIAGEM_PRECO=>AT_FC_PROCESSAR_VIAGEM_PRECO.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

      ME->ZIF_INTEGRACAO_VIAGEM_PRECO~SET_ID_REFERENCIA( ).


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
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN        = WA_WEBSERVICE-URL_TOKEN.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_VIAGEM_PRECO~AT_VIAGEM-VIAGEM_ID && '/valor/motorista'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_VIAGEM_PRECO=>AT_FC_PROCESSAR_VIAGEM_PRECO.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

    ME->ZIF_INTEGRACAO_VIAGEM_PRECO~SET_ID_REFERENCIA( ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_PRECO~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_VIAGEM_PRECO = ME.
    ME->ZIF_INTEGRACAO_VIAGEM_PRECO~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_PRECO~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO,
          LC_MSG_ERRO TYPE ZDE_MSG_CAARG_RETORNO_ERRO.

    R_IF_INTEGRACAO_VIAGEM_PRECO = ME.

    CREATE OBJECT LC_INTEGRAR.

    TRY .
        "Cria MSG para Integração via HTTP
        LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
          )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
          )->SET_OUTBOUND_MSG( IMPORTING E_MENSAGEM = DATA(E_MENSAGEM)
          ).
      CATCH ZCX_ERROR INTO DATA(EX_ERRO).

        IF E_MENSAGEM-DS_DATA_RETORNO IS NOT INITIAL.
          /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = E_MENSAGEM-DS_DATA_RETORNO CHANGING DATA = LC_MSG_ERRO ).
          IF LC_MSG_ERRO-ERROR-MESSAGE IS NOT INITIAL.
            ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LC_MSG_ERRO-ERROR-MESSAGE ).
          ENDIF.
        ENDIF.

        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_ERRO->ZIF_ERROR~MSGID
                              MSGNO = EX_ERRO->ZIF_ERROR~MSGNO
                              ATTR1 = CONV #( EX_ERRO->ZIF_ERROR~MSGV1 )
                              ATTR2 = CONV #( EX_ERRO->ZIF_ERROR~MSGV2 )
                              ATTR3 = CONV #( EX_ERRO->ZIF_ERROR~MSGV3 )
                              ATTR4 = CONV #( EX_ERRO->ZIF_ERROR~MSGV4 ) )
            MSGID  = EX_ERRO->ZIF_ERROR~MSGID
            MSGNO  = EX_ERRO->ZIF_ERROR~MSGNO
            MSGTY  = 'E'
            MSGV1  = EX_ERRO->ZIF_ERROR~MSGV1
            MSGV2  = EX_ERRO->ZIF_ERROR~MSGV2
            MSGV3  = EX_ERRO->ZIF_ERROR~MSGV3
            MSGV4  = EX_ERRO->ZIF_ERROR~MSGV4.

    ENDTRY.

    LC_INTEGRAR->ZIF_INTEGRACAO~SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = DATA(E_INTEGRACAO)
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_PRECO~SET_VIAGEM_PRECO_MOTORISTA.

    R_IF_INTEGRACAO_VIAGEM_PRECO = ME.

    SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_VIAGEM_PRECO~AT_VIAGEM
      FROM ZLEST0185
     WHERE VIAGEM_ID EQ @I_VIAGEM_ID.

    ME->ZIF_INTEGRACAO_VIAGEM_PRECO~AT_VIAGEM-WAERS = I_WAERS.
    ME->ZIF_INTEGRACAO_VIAGEM_PRECO~AT_VIAGEM-NM_VR_FRETE_MOTO_NEGO = I_FRETE.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_VIAGEM_PRECO~GET_JSON( IMPORTING E_JSON = DATA(LC_JSON)
      )->SET_DS_DATA( EXPORTING I_JSON = LC_JSON
      )->SET_DS_URL(
      )->SET_SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      ).

    MODIFY ZLEST0185 FROM ME->ZIF_INTEGRACAO_VIAGEM_PRECO~AT_VIAGEM.
    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
