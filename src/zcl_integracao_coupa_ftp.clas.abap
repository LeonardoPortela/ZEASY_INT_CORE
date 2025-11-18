class ZCL_INTEGRACAO_COUPA_FTP definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COUPA_FTP .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_FTP IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_coupa_ftp~at_servico      = 'COUPA_INT_FTP'.
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_ftp.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad = zif_integracao=>at_id_interface_aut_api_ad_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module = 'coupa'.

    IF me->zif_integracao_coupa_ftp~at_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_coupa_ftp~at_auth_ws
          WHERE service = me->zif_integracao_coupa_ftp~at_servico .

    ENDIF.

    IF me->zif_integracao_coupa_ftp~at_auth_ws  IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_coupa_ftp~enviar_coupa.

    IF i_xfile IS INITIAL.
      zcx_error=>zif_error~gera_erro_geral( i_texto = 'Arquivo não foi informado' ).
    ENDIF.

    zif_integracao_coupa_ftp~get_instance(
      )->set_ds_url(
      )->set_ds_data( EXPORTING i_xfile = i_xfile i_file_name = i_file_name
      )->set_send_msg( ).

  ENDMETHOD.


  METHOD zif_integracao_coupa_ftp~get_instance.

    IF zif_integracao_coupa_ftp~at_instance IS NOT BOUND.

      zif_integracao_coupa_ftp~at_instance = NEW zcl_integracao_coupa_ftp( ).

    ENDIF.

    r_object = zif_integracao_coupa_ftp~at_instance.

  ENDMETHOD.


  METHOD zif_integracao_coupa_ftp~set_ds_data.

    DATA lv_file_url TYPE string.

    lv_file_url = '/' && i_file_name.

    zcl_string2=>file_properties( EXPORTING i_file_path = lv_file_url IMPORTING e_file_name = DATA(lv_file_name) e_application_type = DATA(lv_app_type) ).

    DATA(lv_value) = 'form-data; name="formFiles"; filename="' && lv_file_name && '"'.

    APPEND VALUE #( header_field = 'Content-Type' header_value = lv_app_type ) TO me->zif_integracao_inject~at_multipart_fields.
    APPEND VALUE #( header_field = 'content-disposition' header_value = lv_value xvalue = i_xfile ) TO me->zif_integracao_inject~at_multipart_fields.

    "APPEND VALUE #( header_field = 'Content-Type' header_value = 'application/text'   ) TO me->zif_integracao_inject~at_multipart_fields.
    APPEND VALUE #( header_field = 'content-disposition' header_value = 'form-data; name="path"' value = '/Incoming/PurchaseOrders' ) TO me->zif_integracao_inject~at_multipart_fields.

    r_object = me.

  ENDMETHOD.


  METHOD zif_integracao_coupa_ftp~set_ds_url.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_coupa_ftp~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_coupa_ftp~at_auth_ws-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_integracao_coupa_ftp~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.

    r_object = me.

  ENDMETHOD.


  METHOD zif_integracao_coupa_ftp~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_object = me.

    CREATE OBJECT lc_integrar.

    lc_integrar->zif_integracao~at_multipart = me->zif_integracao_inject~at_multipart_fields.
    lc_integrar->zif_integracao~at_msg_integra-ds_url_token = me->zif_integracao_coupa_ftp~at_auth_ws-url_token.
    "lc_integrar->zif_integracao~at_msg_inject->at_autentica_module = me->zif_integracao_coupa_ftp~at_auth_ws-add02.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    IF e_integracao-nm_code < 205.
      MESSAGE s016(ds) WITH e_integracao-ds_data_retorno.
    ELSE.
      MESSAGE s016(ds) WITH e_integracao-ds_data_retorno DISPLAY LIKE 'E'.
    ENDIF.


    CLEAR: lc_integrar.

    r_object = me.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    "APPEND VALUE #( name = 'Content-Type' value = 'multipart/form-data' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Accept-Encoding' value = 'gzip, deflate, br' ) TO me->zif_integracao_inject~at_header_fields.

    r_if_integracao_inject = me.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.

    r_if_integracao_inject = me.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
