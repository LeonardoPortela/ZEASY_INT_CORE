class ZCL_INTEGRACAO_UPLOAD_EXEC definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_UPLOAD_EXEC .

  data URL_UPLOAD type STRING .
  data VALIDA_TOKEN type CHAR1 .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_UPLOAD_EXEC IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_upl_exe.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

*-#147361-31.07.2024-JT-inicio
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_upl_exe.

    IF sy-subrc EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.
*-#147361-31.07.2024-JT-fim

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    DATA: w_header_fields TYPE zde_header_field.

    r_if_integracao_inject = me.

    IF ME->zif_integracao_upload_exec~AT_BUKRS IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>ZCX_ERRO_GERAL-msgid
                            msgno = zcx_integracao=>ZCX_ERRO_GERAL-msgno
                            attr1 = 'Empresa não informada para Execução Upload')
          msgid  = zcx_integracao=>ZCX_ERRO_GERAL-msgid
          msgno  = zcx_integracao=>ZCX_ERRO_GERAL-msgno
          msgty  = 'E'
          msgv1  = 'Empresa não informada para Execução Upload'.
    ENDIF.

    IF me->valida_token = abap_true.
      TRY .
          CAST zcl_integracao_token(
                 zcl_integracao_token=>zif_integracao_token~get_instance(
                   )->set_empresa_token( EXPORTING i_bukrs = ME->zif_integracao_upload_exec~AT_BUKRS
                   )
               )->zif_integracao_inject~get_header_request_http(
            IMPORTING
              e_header_fields = DATA(e_header_fields) ).

*-----------------------------
*------ set header
*-----------------------------
          APPEND VALUE #( name = 'url'                        value = me->url_upload )    TO e_header_fields.
          APPEND VALUE #( name = 'x-ms-blob-type'             value = 'BlockBlob' )       TO e_header_fields.
          APPEND VALUE #( name = 'upload-content-type'        value = 'application/pdf' ) TO e_header_fields.
          APPEND VALUE #( name = 'Content-Transfer-Encoding'  value = 'Binary' )          TO e_header_fields.

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
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_upload_exec~get_id_referencia.

    r_if_integracao_upload_exec = me.

    e_referencia-tp_referencia = me->zif_integracao_inject~at_referencia-tp_referencia.
    e_referencia-id_referencia = me->zif_integracao_inject~at_referencia-id_referencia.


  ENDMETHOD.


  METHOD zif_integracao_upload_exec~get_instance.

    IF zif_integracao_upload_exec~at_if_integracao_upload_exec IS NOT BOUND.
      CREATE OBJECT zif_integracao_upload_exec~at_if_integracao_upload_exec
        TYPE zcl_integracao_upload_exec.
    ENDIF.

    r_if_integracao_upload_exec = zif_integracao_upload_exec~at_if_integracao_upload_exec.

  ENDMETHOD.


  METHOD zif_integracao_upload_exec~get_json.

    r_if_integracao_upload_exec = me.

*--------------------------
*---- File upload
*--------------------------
    e_json_xstring = i_file_bin.

  ENDMETHOD.


  METHOD zif_integracao_upload_exec~set_ds_data.

    DATA: t_header_fields TYPE zde_header_field_t,
          w_header_fields TYPE zde_header_field.

    FREE: t_header_fields.

*-----------------------------
*-- set header
*-----------------------------
    APPEND VALUE #( name = 'url'                        value = me->url_upload )    TO t_header_fields.
    APPEND VALUE #( name = 'x-ms-blob-type'             value = 'BlockBlob' )       TO t_header_fields.
    APPEND VALUE #( name = 'upload-content-type'        value = 'application/pdf' ) TO t_header_fields.
    APPEND VALUE #( name = 'Content-Transfer-Encoding'  value = 'Binary' )          TO t_header_fields.

    "Incluir Texto JSON para integração
    r_if_integracao_upload_exec = me.

    me->zif_integracao_inject~at_info_request_http-ds_body             = i_json.
    me->zif_integracao_inject~at_info_request_http-ds_body_xstring     = i_json_xstring.
    me->zif_integracao_inject~set_header_request_http( i_header_fields = t_header_fields ).

  ENDMETHOD.


  METHOD zif_integracao_upload_exec~set_ds_url.

    DATA: l_direct TYPE char01.

    r_if_integracao_upload_exec = me.

*-#147361-31.07.2024-JT-inicio
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_upl_exe.

    IF sy-subrc EQ 0.
      l_direct = abap_true.

      SELECT SINGLE * INTO @DATA(wa_webservice)
        FROM zauth_webservice
       WHERE service = 'CARGUERO_HOST'.

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
    ELSE.
*-#147361-31.07.2024-JT-inicio
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF wa_webservice "*-#147361-31.07.2024-JT
        FROM zciot_webservice
       WHERE tipo    EQ 'O'
         AND servico EQ '01'.

      l_direct = abap_false.

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
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = COND #( WHEN l_direct = abap_true THEN 'application/pdf'  "*-#147361-31.07.2024-JT-inicio
                                                                                                           ELSE wa_webservice-content_type ).
    me->zif_integracao_inject~at_info_request_http-ds_url_token = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_viagem_carregar=>at_fc_processar_viagem_carrega.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = COND #( WHEN l_direct = abap_true THEN 'PUT'  "*-#147361-31.07.2024-JT-inicio
                                                                                                 ELSE 'POST' ).
    me->zif_integracao_inject~at_info_request_http-ds_url = COND #( WHEN l_direct = abap_true    THEN i_url_upload  "*-#147361-31.07.2024-JT-inicio
                                                                                                 ELSE wa_webservice-url && 'storage/upload' ).
    me->url_upload   = i_url_upload.
    me->valida_token = i_valida_token.

  ENDMETHOD.


  method ZIF_INTEGRACAO_UPLOAD_EXEC~SET_EMPRESA.

    r_if_integracao_upload_exec = me.

    me->zif_integracao_upload_exec~at_bukrs = I_BUKRS.

  endmethod.


  METHOD zif_integracao_upload_exec~set_id_referencia.

    "Incluir Chave de Referência
    r_if_integracao_upload_exec = me.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'INTEGRACAO_UPLOAD_EXECUTAR'.
    me->zif_integracao_inject~at_referencia-id_referencia = i_id_referencia.


  ENDMETHOD.


  METHOD zif_integracao_upload_exec~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_upload_exec = me.

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

  ENDMETHOD.
ENDCLASS.
