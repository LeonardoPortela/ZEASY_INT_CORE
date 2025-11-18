class ZCL_INTEGRACAO_COUPA_GRP_MERCA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COUPA_GRP_MERCA .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional
    raising
      ZCX_INTEGRACAO .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_GRP_MERCA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_coupa_grp_merca~set_servico( i_servico = i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_grp_merc.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->zif_integracao_coupa_grp_merca~at_xml_template = '<?xml version="1.0" encoding="utf-8" ?> <commodity><active>true</active><name>#01#</name>' &&
    '<category>#02#</category><parent><name>#03#</name></parent><custom-fields><conta-razo>#04#</conta-razo></custom-fields></commodity>'.

    "me->zif_integracao_coupa_GRP_MERCA~at_GRP_MERCAra_struc = i_req.
    IF i_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_coupa_grp_merca~at_auth_ws
          WHERE service = i_servico.

    ENDIF.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_coupa_grp_merca~at_token_ws
        WHERE service = 'COUPA_TOKEN'.

    IF me->zif_integracao_coupa_grp_merca~at_auth_ws  IS INITIAL OR
       me->zif_integracao_coupa_grp_merca~at_token_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_coupa_grp_merca~enviar_coupa.

    DATA lv_text2 TYPE char100.

    TRY.

        me->zif_integracao_coupa_grp_merca~set_ds_url(
              )->set_ds_data( i_grp_merca_struc
              )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao) e_integracao  = DATA(e_integracao)  ).

        DATA lo_xml_ret TYPE REF TO cl_xml_document.

        CREATE OBJECT lo_xml_ret.

        CHECK lo_xml_ret->parse_string( e_integracao-ds_data_retorno ) = 0.

        DATA(lv_node) = lo_xml_ret->find_simple_element( 'id' ).

        IF lv_node IS NOT INITIAL.

          e_ret_intr-id_integr = i_grp_merca_struc-matkl. "lv_node.
          e_ret_intr-ident_proc = 'GM'.
          e_ret_intr-dt_atual = sy-datum.
          e_ret_intr-hr_atual = sy-uzeit.
          e_ret_intr-status = 'S'.

        ELSEIF lo_xml_ret->parse_string( e_integracao-ds_data_retorno ) = 0.

          lv_node = lo_xml_ret->find_simple_element( 'error' ).

          lv_text2 = lv_node.

          CALL FUNCTION 'ES_REMOVE_SPECIAL_CHARACTER'
            EXPORTING
              text1       = lv_text2
            IMPORTING
              corr_string = lv_text2.

          lv_node = lv_text2.

          SHIFT lv_node LEFT DELETING LEADING space.

          TRANSLATE lv_node TO UPPER CASE.

          IF lv_node CS 'ALREADY'.

            e_ret_intr-id_integr = i_grp_merca_struc-matkl.
            e_ret_intr-ident_proc = 'GM'.
            e_ret_intr-dt_atual = sy-datum.
            e_ret_intr-hr_atual = sy-uzeit.
            e_ret_intr-status = 'D'.

          ELSE.
            zcx_error=>zif_error~gera_erro_geral( i_texto = lv_node ).
          ENDIF.

        ENDIF.

        CHECK e_ret_intr IS NOT INITIAL.

        MODIFY zintegrcoupa01 FROM e_ret_intr.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_coupa_grp_merca~get_id_referencia.

    r_if_integracao_coupa_grp_merc = me.

  ENDMETHOD.


  METHOD zif_integracao_coupa_grp_merca~get_instance.

    IF zif_integracao_coupa_grp_merca~at_if_int_coupa_grp_merca IS NOT BOUND.
      zif_integracao_coupa_grp_merca~at_if_int_coupa_grp_merca = NEW zcl_integracao_coupa_grp_merca( i_servico ).
    ENDIF.

    r_if_int_coupa_grp_merca = zif_integracao_coupa_grp_merca~at_if_int_coupa_grp_merca.

  ENDMETHOD.


  METHOD zif_integracao_coupa_grp_merca~set_ds_data.

    DATA lv_xgpo TYPE string.
    DATA lv_saknr TYPE string.
    DATA lv_cat TYPE string.

    me->zif_integracao_coupa_grp_merca~at_grp_merca_struc = i_grp_merca_struc.

    CLEAR me->zif_integracao_coupa_grp_merca~at_xml.

    me->zif_integracao_coupa_grp_merca~at_xml = me->zif_integracao_coupa_grp_merca~at_xml_template.

    lv_xgpo = me->zif_integracao_coupa_grp_merca~at_grp_merca_struc-wgbez60.
    lv_saknr = me->zif_integracao_coupa_grp_merca~at_grp_merca_struc-saknr.
    SHIFT lv_saknr LEFT DELETING LEADING '0'.

    REPLACE '#01#' IN me->zif_integracao_coupa_grp_merca~at_xml WITH lv_xgpo.
    REPLACE '#02#' IN me->zif_integracao_coupa_grp_merca~at_xml WITH lv_cat.
    REPLACE '#03#' IN me->zif_integracao_coupa_grp_merca~at_xml WITH me->zif_integracao_coupa_grp_merca~at_grp_merca_struc-wgbez.
    REPLACE '#04#' IN me->zif_integracao_coupa_grp_merca~at_xml WITH lv_saknr.

    me->zif_integracao_inject~at_info_request_http-ds_body = me->zif_integracao_coupa_grp_merca~at_xml.

    r_if_integracao_coupa_grp_merc = me.

  ENDMETHOD.


  METHOD zif_integracao_coupa_grp_merca~set_ds_url.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_coupa_grp_merca~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_coupa_grp_merca~at_auth_ws-url && me->zif_integracao_coupa_grp_merca~at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_integracao_coupa_grp_merca~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_coupa_grp_merca~set_id_referencia( ).

    r_if_integracao_coupa_grp_merc = me.

  ENDMETHOD.


  METHOD zif_integracao_coupa_grp_merca~set_id_referencia.

    r_if_integracao_coupa_grp_merc = me.

    me->zif_integracao_coupa_grp_merca~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_coupa_grp_merca~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_coupa_grp_merc = me.

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


  METHOD zif_integracao_coupa_grp_merca~set_servico.

    IF i_servico IS NOT INITIAL.
      zif_integracao_coupa_grp_merca~at_servico = i_servico.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_true.
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

*    DATA: ordem_servico TYPE zde_kuhlmann_hvi_retorno.
*break abap.
*    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
*    "/UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ordem_servico-os_id IS NOT INITIAL.

*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    ZCL_INTEGRACAO_COF_KUHLMANN=>ZIF_INTEGRACAO_COF_KUHLMANN~GET_INSTANCE(
*      )->SET_HVI_CONFIRMAR( EXPORTING
*        "I_USUARIO = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_USUARIO
*        "I_SENHA   = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_SENHA
*        I_OS_ID   = ORDEM_SERVICO-OS_ID
*      ).
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
*    break abap.
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
ENDCLASS.
