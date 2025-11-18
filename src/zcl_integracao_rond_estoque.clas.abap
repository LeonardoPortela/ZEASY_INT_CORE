class ZCL_INTEGRACAO_ROND_ESTOQUE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_ROND_ESTOQUE .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_ROND_ESTOQUE IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE    = ZIF_INTEGRACAO=>AT_ID_INTERFACE_ROND_ESTOQUE.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO   = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_OUTBOUND.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL        = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_SINCRONIA    = ZIF_INTEGRACAO=>AT_TP_SINCRONIA_SINCRONA.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_OPUS  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_OPUS_NAO.
    ME->ZIF_INTEGRACAO_INJECT~AT_SEND_AUTENTICAO = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_SEND_SIM.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.
*
*    TRY .
*        CAST ZCL_INTEGRACAO_TKN_RONDONLINE(
*               ZCL_INTEGRACAO_TKN_RONDONLINE=>ZIF_INTEGRACAO_TKN_RONDONLINE~GET_INSTANCE(
*                 )->SET_USUARIO_SENHA(              )
*             )->ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP(
*          IMPORTING
*            E_HEADER_FIELDS = DATA(E_HEADER_FIELDS) ).
*
*        ME->ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP( I_HEADER_FIELDS = E_HEADER_FIELDS ).
*
*      CATCH ZCX_ERROR INTO DATA(EX_ERRO).
*
*        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
*          EXPORTING
*            TEXTID = VALUE #( MSGID = EX_ERRO->ZIF_ERROR~MSGID
*                              MSGNO = EX_ERRO->ZIF_ERROR~MSGNO
*                              ATTR1 = CONV #( EX_ERRO->ZIF_ERROR~MSGV1 )
*                              ATTR2 = CONV #( EX_ERRO->ZIF_ERROR~MSGV2 )
*                              ATTR3 = CONV #( EX_ERRO->ZIF_ERROR~MSGV3 )
*                              ATTR4 = CONV #( EX_ERRO->ZIF_ERROR~MSGV4 ) )
*            MSGID  = EX_ERRO->ZIF_ERROR~MSGID
*            MSGNO  = EX_ERRO->ZIF_ERROR~MSGNO
*            MSGTY  = 'E'
*            MSGV1  = EX_ERRO->ZIF_ERROR~MSGV1
*            MSGV2  = EX_ERRO->ZIF_ERROR~MSGV2
*            MSGV3  = EX_ERRO->ZIF_ERROR~MSGV3
*            MSGV4  = EX_ERRO->ZIF_ERROR~MSGV4.
*
*    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
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


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.

    DATA: rondon_est_retorno TYPE zde_rondon_est_retorno,
          git_zmmt0008       TYPE TABLE OF zmmt0008,
          gwa_zmmt0008       LIKE LINE OF  git_zmmt0008.

    /ui2/cl_json=>deserialize( EXPORTING json = i_msg_retorno CHANGING data = rondon_est_retorno ).

    CHECK rondon_est_retorno-id_historico IS NOT INITIAL.

    "git_zmmt0008[] =  me->zif_integracao_rond_estoque~at_zmmt0008.

    LOOP AT me->zif_integracao_rond_estoque~at_zmmt0008 INTO gwa_zmmt0008.

      IF gwa_zmmt0008-status = '1'.
        UPDATE zmmt0008
        SET status = '2' "(Integrado)
            id_hist_rondon   = rondon_est_retorno-id_historico
            integ_rondonline = i_id_integracao
          WHERE werks      = gwa_zmmt0008-werks
          AND   lgort      = gwa_zmmt0008-lgort
          AND   charg      = gwa_zmmt0008-charg.
        COMMIT WORK AND WAIT.
      ELSE.
        IF  gwa_zmmt0008-status = '3'.
          UPDATE zmmt0008
                  SET status = '4'
                      id_hist_rondon     = rondon_est_retorno-id_historico
                      motorista          = space
                      nr_romaneio        = space
                      vbeln              = space
                      vbeln_vf           = space
                      placa_cav          = space
                      nfnum              = space
                      kunnr              = space
                      integ_rondonline   = space
                      dt_inicial_integ   = space
                      WHERE werks     = gwa_zmmt0008-werks
                        AND lgort     = gwa_zmmt0008-lgort
                        AND charg     = gwa_zmmt0008-charg.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_rond_estoque~get_id_referencia.
    r_if_integracao_rond_estoque = me.
    e_referencia-tp_referencia = 'RONDONLINE - ESTOQUE'.
    e_referencia-id_referencia = me->zif_integracao_rond_estoque~at_id_referencia.
  ENDMETHOD.


  METHOD zif_integracao_rond_estoque~get_instance.
    IF zif_integracao_rond_estoque~at_if_integracao_rond_estoque IS NOT BOUND.
      CREATE OBJECT zif_integracao_rond_estoque~at_if_integracao_rond_estoque
        TYPE zcl_integracao_rond_estoque.
    ENDIF.
    r_if_integracao_rond_estoque = zif_integracao_rond_estoque~at_if_integracao_rond_estoque.
  ENDMETHOD.


  METHOD zif_integracao_rond_estoque~get_json.
    r_if_integracao_rond_estoque = me.

    CHECK me->zif_integracao_rond_estoque~at_json IS NOT INITIAL.
    e_json = me->zif_integracao_rond_estoque~at_json.
  ENDMETHOD.


  METHOD zif_integracao_rond_estoque~set_ds_data.
    "Incluir Texto JSON para integração
    r_if_integracao_rond_estoque = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
  ENDMETHOD.


  METHOD zif_integracao_rond_estoque~set_ds_url.
    r_if_integracao_rond_estoque = me.

    SELECT SINGLE * FROM zauth_webservice INTO @DATA(wa_webservice)
           WHERE service = 'RONDONLINE'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'R'
                            attr2 = 'RO' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'R'
          msgv2  = 'RO'.
    ENDIF.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization'    value = |Bearer {  wa_webservice-add01  }| ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/json' ) TO me->zif_integracao_inject~at_header_fields.


    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token    = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.


    me->zif_integracao_rond_estoque~set_id_referencia( ).

  ENDMETHOD.


  METHOD zif_integracao_rond_estoque~set_id_referencia.
    "Incluir Chave de Referência
    r_if_integracao_rond_estoque = me.
    me->zif_integracao_rond_estoque~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_integracao_rond_estoque~set_int_estoque.
    r_if_integracao_rond_estoque = me.

    CLEAR:  me->zif_integracao_rond_estoque~at_zmmt0008.

    me->zif_integracao_rond_estoque~at_json = i_json.
    me->zif_integracao_rond_estoque~at_zmmt0008 = i_zmmt0008_t.


    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_rond_estoque~set_ds_url(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).
  ENDMETHOD.


  METHOD zif_integracao_rond_estoque~set_send_msg.
    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_rond_estoque = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.
  ENDMETHOD.
ENDCLASS.
