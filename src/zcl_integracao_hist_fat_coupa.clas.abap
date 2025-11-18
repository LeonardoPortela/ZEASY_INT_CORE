class ZCL_INTEGRACAO_HIST_FAT_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_HIST_FAT_COUPA .

  methods CONSTRUCTOR
    importing
      !I_SERVICO type ZTIPOWEBSERV optional
      !IWA_HISTORICO_FATURAMENTO type STRING .
protected section.
private section.

  types:
    type_t_bapiesllc TYPE TABLE OF bapiesllc .
  types:
    type_t_bapiesklc TYPE TABLE OF bapiesklc .
  types:
    type_t_BAPIESKNC TYPE TABLE OF BAPIESKNC .

  data AT_HISTORICO_FATURAMENTO type STRING .
  data AT_PEDIDOS type ZCOUPA_PEDIDO_CUSTOM_FIELDS .
  data AT_HISTORICO type STRING .
ENDCLASS.



CLASS ZCL_INTEGRACAO_HIST_FAT_COUPA IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_recibos.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    " Move as informacoes da taxa de cambio
    me->at_historico_faturamento = iwa_historico_faturamento.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_hist_fat_coupa~at_auth_ws
        WHERE service = 'COUPA_CUSTOM_FIELDS'.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_hist_fat_coupa~at_token_ws
        WHERE service = 'COUPA_TOKEN'.

    IF me->zif_integracao_hist_fat_coupa~at_auth_ws  IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~get_id_referencia.

    r_if_integracao_hist_fat_coupa = me.

    e_referencia-id_referencia = sy-datum.
    e_referencia-tp_referencia = 'COUPA_HIST_FATURAMENTO'.

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~get_instance.

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~get_retorno.

    r_if_integracao_hist_fat_coupa = me.

    e_retorno = me->zif_integracao_hist_fat_coupa~at_retorno.

    e_historico = me->at_historico.

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~get_xml.

    DATA: lv_full_xml TYPE string.

    lv_full_xml = '<order-header>'.
    lv_full_xml = lv_full_xml && '<custom-fields>'.
    lv_full_xml = lv_full_xml && '<histrico-de-faturamento>'.
    lv_full_xml = lv_full_xml && at_historico_faturamento.
    lv_full_xml = lv_full_xml && '</histrico-de-faturamento>'.
    lv_full_xml = lv_full_xml && '</custom-fields>'.
    lv_full_xml = lv_full_xml && '</order-header>'.


    e_xml = lv_full_xml.

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~set_ds_data.

    r_if_integracao_hist_fat_coupa = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~set_ds_url.

    r_if_integracao_hist_fat_coupa = me.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_hist_fat_coupa~at_auth_ws-url &&
                                                            e_http_url.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = e_metodo.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_hist_fat_coupa~set_id_referencia( ).

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~set_id_referencia.

    r_if_integracao_hist_fat_coupa = me.

    me->zif_integracao_hist_fat_coupa~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~set_recibo_buscar.
    r_if_integracao_hist_fat_coupa = me.
  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    DATA: e_integracao_log TYPE zintegracao_log.

    r_if_integracao_hist_fat_coupa = me.

    CREATE OBJECT lc_integrar.

    TRY.
        lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
          )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
          )->set_outbound_msg(
          )->set_processar_retorno(
          )->set_integrar_retorno(
          )->get_registro( IMPORTING e_integracao = e_integracao
          )->free( ).

        CLEAR: lc_integrar.
      CATCH zcx_error INTO DATA(lo_error).

        e_retorno_integracao = e_integracao_log.

        DATA(lc_msgid) = lo_error->zif_error~msgid.
        DATA(lc_msgno) = lo_error->zif_error~msgno.
        DATA(lc_msgv1) = lo_error->zif_error~msgv1.
        DATA(lc_msgv2) = lo_error->zif_error~msgv2.
        DATA(lc_msgv3) = lo_error->zif_error~msgv3.
        DATA(lc_msgv4) = lo_error->zif_error~msgv4.


        "Propagar erro de Comunicação para Fora
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = lc_msgid
                              msgno = lc_msgno
                              attr1 = CONV #( lc_msgv1 )
                              attr2 = CONV #( lc_msgv2 )
                              attr3 = CONV #( lc_msgv3 )
                              attr4 = CONV #( lc_msgv4 ) )
            msgid  = lc_msgid
            msgno  = lc_msgno
            msgty  = 'E'
            msgv1  = lc_msgv1
            msgv2  = lc_msgv2
            msgv3  = lc_msgv3
            msgv4  = lc_msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_integracao_hist_fat_coupa~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_hist_fat_coupa~set_user_buscar.

    r_if_integracao_hist_fat_coupa = me.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_true.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
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

  endmethod.


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


  METHOD zif_integracao_inject~set_processa_retorno.

    DATA: lva_xml_xstring TYPE xstring.

    DATA: lt_xml_table TYPE STANDARD TABLE OF smum_xmltb,
          lt_xml_info  TYPE TABLE OF smum_xmltb,
          lt_return    TYPE STANDARD TABLE OF bapiret2.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = i_msg_retorno
      IMPORTING
        buffer = lva_xml_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lva_xml_xstring
        TABLES
          xml_table = lt_xml_table[]  " XML Table structure used for retreive and output XML doc
          return    = lt_return[].    " XML Table structure used for retreive and output XML doc

      lt_xml_info[] = CORRESPONDING #( lt_xml_table[] ).
      LOOP AT lt_xml_info ASSIGNING FIELD-SYMBOL(<fs_info>).

        DATA(lva_tabix) = sy-tabix.
        TRANSLATE <fs_info>-cname TO UPPER CASE.

        CASE <fs_info>-cname.
          WHEN 'HISTORICO-DE-FATURAMENTO' OR 'HISTRICO-DE-FATURAMENTO'.
            me->at_historico = <fs_info>-cvalue.
        ENDCASE.
      ENDLOOP.

    ENDIF.



    DATA: lit_result_xml TYPE abap_trans_resbind_tab.

    DATA: lwa_result_xml TYPE abap_trans_resbind,
          lwa_pedidos    TYPE zcoupa_pedido_custom_fields,
          lwa_rif_ex     TYPE REF TO cx_root,
          lwa_var_text   TYPE string VALUE 'Error in Imput XML File'.

    DATA: lva_retorno TYPE string.

    CLEAR: lit_result_xml,
           lit_result_xml[],
           lwa_pedidos.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo = 'GET'.
      GET REFERENCE OF lwa_pedidos INTO lwa_result_xml-value.
      lwa_result_xml-name = 'ORDER_HEARDER'.
      APPEND lwa_result_xml TO lit_result_xml.

      TRY.
          CALL TRANSFORMATION zt_coupa_pedido_custom_fields2
          SOURCE XML i_msg_retorno
          RESULT (lit_result_xml).

          me->at_pedidos = lwa_pedidos.

          me->at_historico = lwa_pedidos-custom_fields-histrico_de_faturamento.

        CATCH cx_root INTO lwa_rif_ex.

      ENDTRY.

      me->zif_integracao_hist_fat_coupa~at_retorno = e_sucesso.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
