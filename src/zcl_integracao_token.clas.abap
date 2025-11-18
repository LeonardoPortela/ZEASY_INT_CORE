class ZCL_INTEGRACAO_TOKEN definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_TOKEN .
  interfaces ZIF_INTEGRACAO_INJECT .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TOKEN IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_token.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.


    me->zif_integracao_token~set_ds_url( ).

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


  METHOD ZIF_INTEGRACAO_TOKEN~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_TOKEN = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_TOKEN'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_TOKEN~AT_BUKRS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_TOKEN~GET_INSTANCE.

    IF ZIF_INTEGRACAO_TOKEN~AT_IF_INTEGRACAO_TOKEN IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_TOKEN~AT_IF_INTEGRACAO_TOKEN
        TYPE ZCL_INTEGRACAO_TOKEN.
    ENDIF.
    R_IF_INTEGRACAO_TOKEN = ZIF_INTEGRACAO_TOKEN~AT_IF_INTEGRACAO_TOKEN.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_TOKEN~GET_JSON.

    R_IF_INTEGRACAO_TOKEN = ME.

    SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_TOKEN~AT_ZLEST0196
      FROM ZLEST0196
     WHERE BUKRS EQ @ME->ZIF_INTEGRACAO_TOKEN~AT_BUKRS.

    E_JSON = '{' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             ' "client_id" : "' && ME->ZIF_INTEGRACAO_TOKEN~AT_ZLEST0196-CLIENT_ID && '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             ' "client_secret" : "' && ME->ZIF_INTEGRACAO_TOKEN~AT_ZLEST0196-CLIENT_SECRET && '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             ' "scopes" : "' && ME->ZIF_INTEGRACAO_TOKEN~AT_ZLEST0196-SCOPES && '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             ' "grant_type" : "' && ME->ZIF_INTEGRACAO_TOKEN~AT_ZLEST0196-GRANT_TYPE && '" ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             '}'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_TOKEN~SET_DS_DATA.

    R_IF_INTEGRACAO_TOKEN = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_TOKEN~SET_DS_URL.

    R_IF_INTEGRACAO_TOKEN = ME.

*    SELECT SINGLE * INTO @DATA(WA_WEBSERVICE)
*      FROM ZCIOT_WEBSERVICE
*     WHERE TIPO    EQ 'O'
*       AND SERVICO EQ '01'.

    SELECT SINGLE * INTO @DATA(LWA_ZAUTH_WEBSERVICE)
      FROM ZAUTH_WEBSERVICE
     WHERE SERVICE = 'CARGUERO_TOKEN'.

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

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO = 'JSON'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = LWA_ZAUTH_WEBSERVICE-CONTENT_TYPE.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN = LWA_ZAUTH_WEBSERVICE-URL_TOKEN.
    "IF WA_WEBSERVICE-URL_TOKEN IS NOT INITIAL.
    "  ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL_TOKEN.
    "ELSE.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL.
    "ENDIF.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_TOKEN=>AT_FC_PROCESSAR_TOKEN.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'POST'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

    ME->ZIF_INTEGRACAO_TOKEN~SET_ID_REFERENCIA( ).

  ENDMETHOD.


  METHOD zif_integracao_token~set_empresa_token.

    r_if_integracao_token = me.

    me->zif_integracao_token~at_bukrs = i_bukrs.

    "LES - Consulta Documentos Viagem Carguero US 171450 - WPP --->>>
    zcl_gestao_token=>get_token_valido( EXPORTING i_id_token = '0013' i_processo_token = CONV #( i_bukrs )
                                        IMPORTING e_token = DATA(lwa_token)
                                        RECEIVING r_header_authorization = DATA(lit_header_auth) ).

    IF lit_header_auth[] IS NOT INITIAL AND i_bukrs IS NOT INITIAL.

      SELECT SINGLE * INTO @me->zif_integracao_token~at_zlest0196
        FROM zlest0196
       WHERE bukrs EQ @me->zif_integracao_token~at_bukrs.

      IF sy-subrc EQ 0.
        me->zif_integracao_inject~at_header_fields = lit_header_auth.

        READ TABLE lit_header_auth INTO DATA(lwa_header_token) INDEX 1.

        APPEND VALUE #( name = 'access_token' value = lwa_token-access_token ) TO me->zif_integracao_inject~at_header_fields.
        APPEND VALUE #( name = 'token_type'   value = lwa_token-token_type )   TO me->zif_integracao_inject~at_header_fields.
        APPEND VALUE #( name = 'client_id'    value = me->zif_integracao_token~at_zlest0196-client_id ) TO me->zif_integracao_inject~at_header_fields.
        APPEND VALUE #( name = 'apikey '      value = me->zif_integracao_token~at_zlest0196-apikey )    TO me->zif_integracao_inject~at_header_fields.

        EXIT.
      ENDIF.

    ENDIF.
    "LES - Consulta Documentos Viagem Carguero US 171450 - WPP <<<---

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_token~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg(
           IMPORTING
             e_id_integracao = e_id_integracao
             e_access_token  = e_access_token
             e_token_type    = e_token_type
             e_expires_in    = e_expires_in
      ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_TOKEN~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_TOKEN = ME.
    ME->ZIF_INTEGRACAO_TOKEN~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  METHOD zif_integracao_token~set_send_msg.

    TYPES BEGIN OF ty_retorno.
    TYPES: access_token TYPE string.
    TYPES: expires_in TYPE string.
    TYPES: token_type TYPE string.
    TYPES END OF ty_retorno.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.

    r_if_integracao_token = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    FREE: lc_integrar.
    CLEAR: lc_integrar.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

    e_access_token = lc_retorno-access_token.
    e_token_type = lc_retorno-token_type.
    e_expires_in = lc_retorno-expires_in.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'access_token'  value = e_access_token ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'token_type'  value = e_token_type ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'client_id'  value = me->zif_integracao_token~at_zlest0196-client_id ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'apikey '  value = me->zif_integracao_token~at_zlest0196-apikey ) TO me->zif_integracao_inject~at_header_fields.


    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'.

    IF sy-subrc EQ 0.
      APPEND VALUE #( name = 'Authorization'  value = |Bearer { e_access_token } | ) TO me->zif_integracao_inject~at_header_fields.
    ENDIF.

    "LES - Consulta Documentos Viagem Carguero US 171450 - WPP --->>>
    zcl_gestao_token=>update_token( i_id_token = '0013' i_access_token = e_access_token  i_token_type = 'Bearer' i_processo_token = CONV #( me->zif_integracao_token~at_bukrs ) ).
    "LES - Consulta Documentos Viagem Carguero US 171450 - WPP <<<---


  ENDMETHOD.
ENDCLASS.
