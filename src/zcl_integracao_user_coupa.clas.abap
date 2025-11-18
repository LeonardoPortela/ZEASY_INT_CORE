class ZCL_INTEGRACAO_USER_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_USER_COUPA .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_USER_COUPA IMPLEMENTATION.


  METHOD constructor.

    "me->zif_integracao_user_coupa~at_servico = 'COUPA_USER' .
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_user_coupa.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_user_coupa~at_auth_ws
        WHERE service = 'COUPA_USER'.

    IF sy-subrc NE 0.
      zcx_integracao=>zif_error~gera_erro_geral( 'Serviço não configurado' ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_integracao_token_coupa(
               zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                 )->get_token( )
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

*    DATA lt_header_fields  TYPE zde_header_field_t.
*
*    APPEND INITIAL LINE TO lt_header_fields ASSIGNING FIELD-SYMBOL(<fs_field>).
*    <fs_field>-name = 'x-coupa-api-key'.
*    <fs_field>-value = me->zif_integracao_inject~at_info_request_http-ds_url_token.
*
*    APPEND INITIAL LINE TO lt_header_fields ASSIGNING <fs_field>.
*    <fs_field>-name = 'Accept'.
*    <fs_field>-value = 'application/xml'.
*
*    me->zif_integracao_inject~set_header_request_http( i_header_fields = lt_header_fields ).

    APPEND VALUE #( name = 'Accept' value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

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

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

*    DATA: ordem_servico TYPE retorno.


*    "/UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ordem_servico-os_id IS NOT INITIAL.

*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    ZCL_INTEGRACAO_USER_COUPA=>ZIF_INTEGRACAO_USER_COUPA~GET_INSTANCE(
*      )->SET_CONFIRMAR( EXPORTING
*        "I_USUARIO = ME->ZIF_INTEGRACAO_USER_COUPA~AT_USUARIO
*
*      ).
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    r_if_integracao_inject = me.

*    DATA: ORDEM_SERVICO TYPE ZDE_KUHLMANN_HVI_RETORNO.
*
*    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
*    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ORDEM_SERVICO-OS_ID IS NOT INITIAL.
*
*    DATA: HVI    TYPE ZPP_KUHLMANN_HVI,
*          IT_HVI TYPE TABLE OF ZPP_KUHLMANN_HVI.
*
*    CLEAR: IT_HVI[].
*
*    LOOP AT ORDEM_SERVICO-FARDOS INTO DATA(WA_FARDO).
*      CLEAR: HVI.
*      HVI-FARDO  = WA_FARDO-FARDO.
*      HVI-OS_ID  = ORDEM_SERVICO-OS_ID.
*      HVI-OS_NR  = ORDEM_SERVICO-NUMERO_OS.
*      HVI-ROMANEIO = ORDEM_SERVICO-ROMANEIO.
*      HVI-LABORATORIO = ORDEM_SERVICO-LABORATORIO.
*
*      DATA(DT_ENTRADA) = ORDEM_SERVICO-DATA_ENTRADA(10).
*      REPLACE ALL OCCURRENCES OF '-' IN DT_ENTRADA WITH ''.
*      HVI-DATA_ENTRADA = DT_ENTRADA.
*
*      DATA(DT_ANALISE) = ORDEM_SERVICO-DATA_ANALISE(10).
*      REPLACE ALL OCCURRENCES OF '-' IN DT_ANALISE WITH ''.
*      HVI-DATA_ANALISE = DT_ANALISE.
*
*      HVI-HVI_MIC      = WA_FARDO-MIC.
*      HVI-HVI_POL      = WA_FARDO-POL.
*      HVI-HVI_LEN      = WA_FARDO-LEN.
*      HVI-HVI_STR      = WA_FARDO-STR.
*      HVI-HVI_UNF      = WA_FARDO-UNF.
*      HVI-HVI_ELG      = WA_FARDO-ELG.
*      HVI-HVI_MAT      = WA_FARDO-MAT.
*      HVI-HVI_RD       = WA_FARDO-RD.
*      HVI-HVI_B        = WA_FARDO-B.
*      HVI-HVI_CG       = WA_FARDO-CG.
*      HVI-HVI_LEAF     = WA_FARDO-LEAF.
*      HVI-HVI_ARE      = WA_FARDO-ARE.
*      HVI-HVI_COUNT    = WA_FARDO-COUNT.
*      HVI-HVI_SFI      = WA_FARDO-SFI.
*      HVI-HVI_CSP      = WA_FARDO-CSP.
*      HVI-HVI_SCI      = WA_FARDO-SCI.
*      HVI-HVI_MAT_PORC = WA_FARDO-MAT_PORC.
*      HVI-HVI_POL_FRAC = WA_FARDO-POL_FRAC.
*      HVI-HVI_LOTE     = WA_FARDO-LOTE.
*      HVI-HVI_PADRAO   = WA_FARDO-PADRAO.
*      APPEND HVI TO IT_HVI.
*    ENDLOOP.
*
*    IF IT_HVI[] IS NOT INITIAL.
*      MODIFY ZPP_KUHLMANN_HVI FROM TABLE IT_HVI.
*      COMMIT WORK AND WAIT.
*    ENDIF.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~execute_process.

    DATA lv_retorno TYPE string.
    DATA lv_xml TYPE string.
    DATA lt_text_tab TYPE TABLE OF text_tab.

    DATA lv_num TYPE num10.
    DATA lv_id TYPE string.

    me->zif_integracao_user_coupa~set_ds_url( e_user = i_user e_metodo   = 'GET' ).

    me->zif_integracao_user_coupa~set_send_msg(
      IMPORTING e_id_integracao = DATA(e_id_integracao)
                e_integracao = DATA(e_integracao) ).


    lv_retorno =  e_integracao-ds_data_retorno.

    REPLACE ALL OCCURRENCES OF '#' IN lv_retorno WITH ''.

    CONDENSE lv_retorno NO-GAPS.

    CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
      EXPORTING
        text                = lv_retorno
        flag_no_line_breaks = abap_true
        line_length         = 10
        langu               = 'P'
      TABLES
        text_tab            = lt_text_tab.

    READ TABLE lt_text_tab INTO DATA(lw_xml) INDEX 9.

    IF sy-subrc IS INITIAL.
      lv_num = lw_xml.
      lv_id = lv_num.
      SHIFT lv_id LEFT DELETING LEADING '0'.
    ENDIF.

*          ---------------------
    me->zif_integracao_user_coupa~get_xml( EXPORTING e_ativo = i_ativo e_bloqu = i_bloqueado
                                               IMPORTING e_xml = lv_xml  ).

    me->zif_integracao_user_coupa~set_ds_data( i_xml = lv_xml ).


    me->zif_integracao_user_coupa~set_ds_url( e_user = i_user e_metodo = 'PUT' e_id = lv_id ).


    me->zif_integracao_user_coupa~set_send_msg( IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                                               e_integracao = DATA(e_integracao_post) ).

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~get_id_referencia.

    r_if_integracao_user_coupa = me.

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~get_instance.

    IF zif_integracao_user_coupa~at_if_integracao_coupa IS NOT BOUND.
      zif_integracao_user_coupa~at_if_integracao_coupa = NEW zcl_integracao_user_coupa( ).
    ENDIF.

    r_if_integracao_user_coupa = zif_integracao_user_coupa~at_if_integracao_coupa.

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~get_xml.

    IF e_ativo = 'X'.
      e_xml = '<user><active>TRUE</active></user>'.
    ELSEIF e_bloqu = 'X'.
      e_xml = '<user><active>FALSE</active></user>'.
    ENDIF.

    r_if_integracao_user_coupa = me.

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~set_ds_data.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.

    r_if_integracao_user_coupa = me.

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~set_ds_url.

    DATA lv_http_url TYPE string.

    IF e_metodo = 'PUT'.

      CONCATENATE me->zif_integracao_user_coupa~at_auth_ws-url e_id INTO lv_http_url.

      CONDENSE lv_http_url NO-GAPS.

    ELSEIF e_metodo = 'GET'.

      lv_http_url = me->zif_integracao_user_coupa~at_auth_ws-url && '?login=' && e_user && '&fields=["id","login","email"]'.
      CONDENSE lv_http_url NO-GAPS.

    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml;charset=UTF-8'.
*    me->zif_integracao_inject~at_info_request_http-ds_url_token = me->zif_integracao_user_coupa~at_auth_ws-add01.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = e_metodo.
    me->zif_integracao_inject~at_info_request_http-ds_url = lv_http_url.

    me->zif_integracao_inject~at_info_request_http-ds_url = lv_http_url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_user_coupa~set_id_referencia( ).

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~set_id_referencia.

    r_if_integracao_user_coupa = me.

    me->zif_integracao_user_coupa~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_user_coupa = me.

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


  METHOD zif_integracao_user_coupa~set_servico.

    IF i_servico IS NOT INITIAL.
      zif_integracao_user_coupa~at_servico = i_servico.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_user_coupa~set_user_buscar.



  ENDMETHOD.
ENDCLASS.
