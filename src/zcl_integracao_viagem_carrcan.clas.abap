class ZCL_INTEGRACAO_VIAGEM_CARRCAN definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_VIAGEM_CARRCAN .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_VIAGEM_CARRCAN IMPLEMENTATION.


  METHOD constructor.
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_cacan.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.


    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_cacan.

    IF SY-SUBRC EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    me->zif_integracao_viagem_carrcan~set_ds_url( ).
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


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


  method ZIF_INTEGRACAO_VIAGEM_CARRCAN~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_CARRCAN = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_VIAGEM_CARRCAN'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_VIAGEM_CARRCAN~AT_VIAGEM-VIAGEM_ID.

  endmethod.


  METHOD ZIF_INTEGRACAO_VIAGEM_CARRCAN~GET_INSTANCE.

    IF ZIF_INTEGRACAO_VIAGEM_CARRCAN~AT_IF_INTEGRACAO_CARRCAN IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_VIAGEM_CARRCAN~AT_IF_INTEGRACAO_CARRCAN
        TYPE ZCL_INTEGRACAO_VIAGEM_CARRCAN.
    ENDIF.
    R_IF_INTEGRACAO_CARRCAN = ZIF_INTEGRACAO_VIAGEM_CARRCAN~AT_IF_INTEGRACAO_CARRCAN.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_CARRCAN~GET_JSON.

    TYPES BEGIN OF TY_CARREGAR.
    TYPES: MOTIVO_REJEICAO_SISTEMA TYPE STRING.
    TYPES: MOTIVO_REJEICAO_USUARIO TYPE STRING.
    TYPES END OF TY_CARREGAR.

    DATA: LC_CARREGA TYPE TY_CARREGAR.

    R_IF_INTEGRACAO_CARRCAN = ME.

    E_JSON = ZCL_FMCALL_BASE=>ABAP2JSON( ABAP_DATA = LC_CARREGA ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_CARRCAN~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_CARRCAN = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_CARRCAN~SET_DS_URL.

    R_IF_INTEGRACAO_CARRCAN = ME.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_cacan.

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
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_VIAGEM_CARRCAN~AT_VIAGEM-VIAGEM_ID && '/carregamento/rejeitar'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_VIAGEM_CARRCAN=>AT_FC_PROCESSAR_VIAGEM_CARRCAN.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

      ME->ZIF_INTEGRACAO_VIAGEM_CARRCAN~SET_ID_REFERENCIA( ).

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
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_VIAGEM_CARRCAN~AT_VIAGEM-VIAGEM_ID && '/carregamento/rejeitar'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_VIAGEM_CARRCAN=>AT_FC_PROCESSAR_VIAGEM_CARRCAN.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

    ME->ZIF_INTEGRACAO_VIAGEM_CARRCAN~SET_ID_REFERENCIA( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_VIAGEM_CARRCAN~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_CARRCAN = ME.
    ME->ZIF_INTEGRACAO_VIAGEM_CARRCAN~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).


  endmethod.


  method ZIF_INTEGRACAO_VIAGEM_CARRCAN~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_CARRCAN = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG(
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = DATA(E_INTEGRACAO)
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  endmethod.


  METHOD zif_integracao_viagem_carrcan~set_viagem_carrcan.

    r_if_integracao_carrcan = me.

    IF i_viagem_id IS NOT INITIAL.
      SELECT SINGLE * INTO @me->zif_integracao_viagem_carrcan~at_viagem
        FROM zlest0185
       WHERE viagem_id EQ @i_viagem_id.

    ELSEIF i_tknum IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_vttk)
        FROM vttk
       WHERE tknum EQ @i_tknum.

      IF sy-subrc IS INITIAL AND wa_vttk-id_viagem IS NOT INITIAL.

        SELECT SINGLE * INTO @me->zif_integracao_viagem_carrcan~at_viagem
          FROM zlest0185
         WHERE viagem_id EQ @wa_vttk-id_viagem.

      ELSEIF sy-subrc IS INITIAL AND wa_vttk-id_ordem IS NOT INITIAL.

        SELECT SINGLE * INTO @me->zif_integracao_viagem_carrcan~at_viagem
          FROM zlest0185
         WHERE id_ordem EQ @wa_vttk-id_ordem.

      ELSEIF sy-subrc IS INITIAL.

        IF wa_vttk-id_romaneio IS NOT INITIAL.

          SELECT SINGLE * INTO @DATA(wa_romaneio)
            FROM zsdt0001
           WHERE ch_referencia EQ @wa_vttk-id_romaneio.

          IF sy-subrc IS INITIAL AND wa_romaneio-id_ordem IS NOT INITIAL.
            SELECT SINGLE * INTO @me->zif_integracao_viagem_carrcan~at_viagem
              FROM zlest0185
             WHERE id_ordem EQ @wa_romaneio-id_ordem.
          ENDIF.

        ELSE.

          SELECT SINGLE * INTO @wa_romaneio
            FROM zsdt0001
           WHERE doc_transp EQ @wa_vttk-tknum.

          IF sy-subrc IS INITIAL AND wa_romaneio-id_ordem IS NOT INITIAL.
            SELECT SINGLE * INTO @me->zif_integracao_viagem_carrcan~at_viagem
              FROM zlest0185
             WHERE id_ordem EQ @wa_romaneio-id_ordem.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    CHECK me->zif_integracao_viagem_carrcan~at_viagem IS NOT INITIAL.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    CHECK me->zif_integracao_viagem_carrcan~at_viagem-nro_cg_sai_in IS INITIAL AND
          me->zif_integracao_viagem_carrcan~at_viagem-nro_sol is INITIAL.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    IF me->zif_integracao_viagem_carrcan~at_viagem-ck_carregado EQ abap_false.
      "Consultar Status para verificar se realmente não está carregado

      zcl_integracao_viagem_status=>zif_integracao_viagem_status~get_instance(
        )->set_viagem_status( EXPORTING i_viagem_id = me->zif_integracao_viagem_carrcan~at_viagem-viagem_id IMPORTING e_status = DATA(e_status)
        ).

      "01	Carregamento
      "02	Liberação do Embarcador
      "03	Carregado
      "04	Descarregado
      "05	Cancelado
      "06	Rejeitado pelo Embarcador
      "07	Emissão de Nota Fiscal de Trânsito
      "08	Liberação do Carregamento
      "09	Carregamento Rejeitado pelo Embarcador
      "10	Carregamento
      "11	Resposta da Solicitação de Cancelamento
      "12	Carregamento
      "13	Resposta da Solicitação de Cancelamento
      "14	Cancelado

      IF e_status EQ '03'.
        me->zif_integracao_viagem_carrcan~at_viagem-ck_carregado = abap_true.
      ENDIF.

    ENDIF.

    IF me->zif_integracao_viagem_carrcan~at_viagem-ck_carregado EQ abap_true.

      CHECK 1 EQ 2.

      "Inclui Json na Mesagem a Ser Enviada
      me->zif_integracao_viagem_carrcan~get_json( IMPORTING e_json = DATA(lc_json)
        )->set_ds_data( EXPORTING i_json = lc_json
        )->set_ds_url(
        )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao
        ).

      me->zif_integracao_viagem_carrcan~at_viagem-ck_carregado = abap_false.
      MODIFY zlest0185 FROM me->zif_integracao_viagem_carrcan~at_viagem.
      COMMIT WORK.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
