class ZCL_INTEGRACAO_VIAGEM_STATUS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_VIAGEM_STATUS .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_VIAGEM_STATUS IMPLEMENTATION.


  METHOD constructor.
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_status.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_status.

    IF SY-SUBRC EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    me->zif_integracao_viagem_status~set_ds_url( ).
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


  METHOD ZIF_INTEGRACAO_VIAGEM_STATUS~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_VIAGEM_STATUS = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_VIAGEM_STATUS'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_VIAGEM_STATUS~AT_VIAGEM.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_STATUS~GET_INSTANCE.

    IF ZIF_INTEGRACAO_VIAGEM_STATUS~AT_IF_INTEGRACAO_VIAGEM_STATUS IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_VIAGEM_STATUS~AT_IF_INTEGRACAO_VIAGEM_STATUS
        TYPE ZCL_INTEGRACAO_VIAGEM_STATUS.
    ENDIF.
    R_IF_INTEGRACAO_VIAGEM_STATUS = ZIF_INTEGRACAO_VIAGEM_STATUS~AT_IF_INTEGRACAO_VIAGEM_STATUS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_STATUS~GET_JSON.

    R_IF_INTEGRACAO_VIAGEM_STATUS = ME.

  ENDMETHOD.


  method ZIF_INTEGRACAO_VIAGEM_STATUS~SET_DS_DATA.

    R_IF_INTEGRACAO_VIAGEM_STATUS = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  endmethod.


  METHOD zif_integracao_viagem_status~set_ds_url.

    r_if_integracao_viagem_status = me.

*-CS2021000253-26.04.2024-#59941-JT-inicio
*-----------------------------------
*-- checar se ordem de terceiro, se fr utilizar integration
*-----------------------------------
    DATA: l_servico TYPE zauth_webservice-service.

    DATA(l_terceiro) = abap_false.

    SELECT SINGLE  agente_frete
      INTO @DATA(l_agente_frete)
      FROM zlest0185
     WHERE viagem_id =  @me->zif_integracao_viagem_status~at_viagem.

    IF sy-subrc = 0 AND l_agente_frete IS NOT INITIAL.
      TRY.
          zcl_fornecedores=>zif_parceiros~get_instance(
            )->set_parceiro( i_parceiro = l_agente_frete
            )->ck_parceiro_terceiro(
            ).
          l_terceiro = abap_true.

        CATCH zcx_parceiros INTO DATA(lx_parceiros).
          l_terceiro = abap_false.
      ENDTRY.
    ENDIF.

    l_servico = COND #( WHEN l_terceiro = abap_false THEN 'CARGUERO_HOST'
                                                     ELSE 'CARGUERO_HOST_TERCEIRO' ).
*-CS2021000253-26.04.2024-#59941-JT-fim

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_status.

    IF sy-subrc EQ 0.

      SELECT SINGLE * INTO @DATA(lwa_zauth_webservice)
        FROM zauth_webservice
       WHERE service = @l_servico. "'CARGUERO_HOST'.  "*-CS2021000253-26.04.2024-#59941-JT

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                              msgno = zcx_integracao=>zcx_servico_http_config-msgno
                              attr1 = 'O'
                              attr2 = '01' )
            msgid  = zcx_integracao=>zcx_servico_http_config-msgid
            msgno  = zcx_integracao=>zcx_servico_http_config-msgno
            msgty  = 'E'
            msgv1  = 'O'
            msgv2  = '01'.
      ENDIF.


      me->zif_integracao_inject~at_id_interface  = zif_integracao=>at_id_interface_viagem_status.
      me->zif_integracao_inject~at_tp_integracao = zif_integracao=>at_tp_integracao_outbound.
      me->zif_integracao_inject~at_tp_canal      = zif_integracao=>at_tp_canal_comunica_http.
      me->zif_integracao_inject~at_tp_sincronia  = zif_integracao=>at_tp_sincronia_sincrona.

      me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
      me->zif_integracao_inject~at_info_request_http-ds_content_type     = lwa_zauth_webservice-content_type.
      me->zif_integracao_inject~at_info_request_http-ds_url = lwa_zauth_webservice-url && 'viagens/' && me->zif_integracao_viagem_status~at_viagem && '/status'.
      me->zif_integracao_inject~at_info_request_http-ds_url_token = lwa_zauth_webservice-url_token.
      me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_viagem_status=>at_fc_processar_viagem_status.
      me->zif_integracao_inject~at_info_request_http-ds_metodo = 'GET'.
      me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

      me->zif_integracao_viagem_status~set_id_referencia( ).


      EXIT.

    ENDIF.


    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ 'O'
       AND servico EQ '01'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'O'
                            attr2 = '01' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'O'
          msgv2  = '01'.
    ENDIF.

    me->zif_integracao_inject~at_id_interface  = zif_integracao=>at_id_interface_viagem_status.
    me->zif_integracao_inject~at_tp_integracao = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal      = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia  = zif_integracao=>at_tp_sincronia_sincrona.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && 'viagens/' && me->zif_integracao_viagem_status~at_viagem && '/status'.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_viagem_status=>at_fc_processar_viagem_status.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_viagem_status~set_id_referencia( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_VIAGEM_STATUS~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_VIAGEM_STATUS = ME.
    ME->ZIF_INTEGRACAO_VIAGEM_STATUS~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  endmethod.


  METHOD ZIF_INTEGRACAO_VIAGEM_STATUS~SET_SEND_MSG.

    TYPES BEGIN OF TY_RETORNO.
    TYPES: DESCRICAO TYPE STRING.
    TYPES: STATUS TYPE STRING.
    TYPES END OF TY_RETORNO.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO,
          LC_RETORNO  TYPE TY_RETORNO.

    R_IF_INTEGRACAO_VIAGEM_STATUS = ME.

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

    FREE: LC_INTEGRAR.
    CLEAR: LC_INTEGRAR.

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = E_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

    E_STATUS = ZCL_STRING=>LPAD( I_STR  = LC_RETORNO-STATUS I_QTD  = 2 I_CHAR = '0' ).

  ENDMETHOD.


  METHOD zif_integracao_viagem_status~set_viagem_status.

    r_if_integracao_viagem_status = me.

    me->zif_integracao_viagem_status~at_viagem = i_viagem_id.

*-CS2021000253-26.04.2024-#59941-JT-inicio
    me->zif_integracao_viagem_status~set_ds_url( ).
*-CS2021000253-26.04.2024-#59941-JT-fim

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_viagem_status~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_status = e_status
      ).

  ENDMETHOD.
ENDCLASS.
