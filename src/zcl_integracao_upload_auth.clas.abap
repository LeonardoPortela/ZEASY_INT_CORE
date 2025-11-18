class ZCL_INTEGRACAO_UPLOAD_AUTH definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_UPLOAD_AUTH .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_UPLOAD_AUTH IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_upl_aut.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

*-#147361-31.07.2024-JT-inicio
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_upl_aut.

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


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    r_if_integracao_inject = me.

    if me->zif_integracao_upload_auth~at_bukrs is INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
         EXPORTING
           textid = VALUE #( msgid = zcx_integracao=>ZCX_ERRO_GERAL-msgid
                             msgno = zcx_integracao=>ZCX_ERRO_GERAL-msgno
                             attr1 = 'Não informada empresa para Autorização Upload' )
           msgid  = zcx_integracao=>ZCX_ERRO_GERAL-msgid
           msgno  = zcx_integracao=>ZCX_ERRO_GERAL-msgno
           msgty  = 'E'
           msgv1  = 'Não informada empresa para Autorização Upload'.
    endif.

    TRY .
        CAST zcl_integracao_token(
               zcl_integracao_token=>zif_integracao_token~get_instance(
                 )->set_empresa_token( EXPORTING i_bukrs = me->zif_integracao_upload_auth~at_bukrs
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


  METHOD zif_integracao_upload_auth~get_id_referencia.

    r_if_integracao_upload_auth = me.

    e_referencia-tp_referencia = me->zif_integracao_inject~at_referencia-tp_referencia.
    e_referencia-id_referencia = me->zif_integracao_inject~at_referencia-id_referencia.

  ENDMETHOD.


  METHOD zif_integracao_upload_auth~get_instance.

    IF zif_integracao_upload_auth~at_if_integracao_upload_auth IS NOT BOUND.
      CREATE OBJECT zif_integracao_upload_auth~at_if_integracao_upload_auth
        TYPE zcl_integracao_upload_auth.
    ENDIF.

    r_if_integracao_upload_auth = zif_integracao_upload_auth~at_if_integracao_upload_auth.

  ENDMETHOD.


  METHOD zif_integracao_upload_auth~get_json.

    R_IF_INTEGRACAO_UPLOAD_AUTH = me.

    IF me->zif_integracao_upload_auth~at_blob_path is INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>ZCX_ERRO_GERAL-msgid
                            msgno = zcx_integracao=>ZCX_ERRO_GERAL-msgno
                            attr1 = 'Blob path não encontrado' )
          msgid  = zcx_integracao=>ZCX_ERRO_GERAL-msgid
          msgno  = zcx_integracao=>ZCX_ERRO_GERAL-msgno
          msgty  = 'E'
          msgv1  = 'Blob path não encontrado'.
    endif.

    e_json = '{"resources":[{"blob_path":"' && me->zif_integracao_upload_auth~at_blob_path && '","permission":"create"}]}'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_UPLOAD_AUTH~SET_BLOB_PATH.

    r_if_integracao_upload_auth = me.

    me->zif_integracao_upload_auth~at_blob_path = i_blob_path.

  ENDMETHOD.


  METHOD zif_integracao_upload_auth~set_ds_data.

    "Incluir Texto JSON para integração
    r_if_integracao_upload_auth = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  ENDMETHOD.


  METHOD zif_integracao_upload_auth~set_ds_url.

    r_if_integracao_upload_auth = me.

*-#147361-31.07.2024-JT-inicio
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_upl_aut.

    IF sy-subrc EQ 0.

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
*-#147361-31.07.2024-JT-inicio
    ELSE.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF wa_webservice "*-#147361-31.07.2024-JT
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
    ENDIF.

*------------------------------
*-- apikey
*------------------------------
    IF me->zif_integracao_upload_auth~at_bukrs IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Empresa não informação para realização do UPLOAD' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Empresa não informação para realização do UPLOAD'.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(wa_zles0196)
      FROM zlest0196
     WHERE bukrs = @me->zif_integracao_upload_auth~at_bukrs.

    IF ( sy-subrc NE 0 ) OR ( wa_zles0196-apikey IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Tenante não configura para a Empresa:' && zif_integracao_upload_auth~at_bukrs )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Tenante não configura para a Empresa:' && zif_integracao_upload_auth~at_bukrs.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_upload_auth=>at_fc_processar_storage.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && 'storage/autorizar?apikey=' && wa_zles0196-apikey.

  ENDMETHOD.


  method ZIF_INTEGRACAO_UPLOAD_AUTH~SET_EMPRESA.


    r_if_integracao_upload_auth = me.

    ME->zif_integracao_upload_auth~at_bukrs = I_BUKRS.


  endmethod.


  METHOD zif_integracao_upload_auth~set_id_referencia.

    "Incluir Chave de Referência
    r_if_integracao_upload_auth = me.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'INTEGRACAO_UPLOAD_AUTORIZAR'.
    me->zif_integracao_inject~at_referencia-id_referencia = i_id_referencia.

  ENDMETHOD.


  METHOD zif_integracao_upload_auth~set_send_msg.

    TYPES BEGIN OF ty_url.
    TYPES: url       TYPE string.
    TYPES: blob_path TYPE string.
    TYPES: token_sas TYPE string.
    TYPES END OF ty_url.

    DATA: t_url    TYPE TABLE OF ty_url.

    TYPES BEGIN OF ty_retorno.
    TYPES: resources LIKE t_url.
    TYPES END OF ty_retorno.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.

    r_if_integracao_upload_auth = me.

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

    FREE: lc_integrar,
          lc_retorno.

*-------------------
*-- obtem URL
*-------------------
    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno
                               CHANGING  data = lc_retorno ).

    READ TABLE lc_retorno-resources INTO DATA(wa_resources) INDEX 1.

    e_url_upload = wa_resources-url.

  ENDMETHOD.
ENDCLASS.
