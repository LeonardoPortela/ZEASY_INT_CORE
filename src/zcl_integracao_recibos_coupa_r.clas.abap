class ZCL_INTEGRACAO_RECIBOS_COUPA_R definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_RECIBOS_COUPA_R .

  data AT_TAXA_CAMBIO_STRUCT type ZMMS_DADOS_TX_CAMBIO_COUPA .
  data AT_VALORES_RETORNO type ZCOUPA_SERVICOS_RETORNO .

  methods CONSTRUCTOR
    importing
      !I_SERVICO type ZTIPOWEBSERV optional
      !I_WA_VALORES type ZCOUPA_SERVICOS_RETORNO optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_RECIBOS_COUPA_R IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_recibos.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    " Move as informacoes do serviço
    me->at_valores_retorno = i_wa_valores.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_recibos_coupa_r~at_auth_ws
        WHERE service = 'COUPA_RECIBOS'.

    IF me->zif_integracao_recibos_coupa_r~at_auth_ws  IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

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

    DATA: lva_buffer TYPE xstring.

    DATA: lit_xml_table TYPE TABLE OF smum_xmltb,
          lit_return    TYPE TABLE OF bapiret2.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = i_msg_retorno
      IMPORTING
        buffer = lva_buffer
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lva_buffer
        TABLES
          xml_table = lit_xml_table
          return    = lit_return.
    ENDIF.

    LOOP AT lit_xml_table ASSIGNING FIELD-SYMBOL(<lfs_xml_table>).
      TRANSLATE <lfs_xml_table>-cname TO UPPER CASE.
    ENDLOOP.

    READ TABLE lit_xml_table TRANSPORTING NO FIELDS WITH KEY cname = 'ID'.
    IF sy-subrc IS INITIAL.
      e_sucesso = 'S'.
    ELSE.
      READ TABLE lit_xml_table TRANSPORTING NO FIELDS WITH KEY cname = 'ERROR'.
      IF sy-subrc IS INITIAL.
        e_sucesso = abap_false.
      ENDIF.
    ENDIF.

    me->zif_integracao_recibos_coupa_r~at_retorno = e_sucesso.

  ENDMETHOD.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~GET_ID_REFERENCIA.
    r_if_integracao_recibos_coupa = me.

    e_referencia-id_referencia = sy-datum.
    e_referencia-tp_referencia = 'COUPA_SERVICO_R'.
  endmethod.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~GET_RETORNO.
    r_if_integracao_recibos_coupa = me.

    e_retorno = me->zif_integracao_recibos_coupa_r~at_retorno.
  endmethod.


  METHOD zif_integracao_recibos_coupa_r~get_xml.
    DATA: lv_full_xml TYPE string.

*    lv_full_xml = '<inventory-transaction>'.

*    lv_full_xml = lv_full_xml && '<id type="integer">'.
*    lv_full_xml = lv_full_xml && at_valores_retorno-id.
*    lv_full_xml = lv_full_xml && '</id>'.

    IF at_valores_retorno-num_folha IS NOT INITIAL.
      lv_full_xml = lv_full_xml && '<custom-fields>'.
      lv_full_xml = lv_full_xml && '<id-sap>'.
      lv_full_xml = lv_full_xml && at_valores_retorno-num_folha.
      lv_full_xml = lv_full_xml && '</id-sap>'.
      lv_full_xml = lv_full_xml && '</custom-fields>'.
    ENDIF.
*    lv_full_xml =  lv_full_xml && '</inventory-transaction>'.

    e_xml = lv_full_xml.

  ENDMETHOD.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~SET_DS_DATA.
    r_if_integracao_recibos_coupa = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.
  endmethod.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~SET_DS_URL.
    r_if_integracao_recibos_coupa = me.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_recibos_coupa_r~at_auth_ws-url &&
                                                            e_http_url.
*    me->zif_integracao_inject~at_info_request_http-ds_body_xstring

    me->zif_integracao_inject~at_info_request_http-ds_metodo = e_metodo.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_recibos_coupa_r~set_id_referencia( ).
  endmethod.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~SET_ID_REFERENCIA.
    r_if_integracao_recibos_coupa = me.

    me->zif_integracao_recibos_coupa_r~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~SET_RECIBO_BUSCAR.
    r_if_integracao_recibos_coupa = me.
  endmethod.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~SET_SEND_MSG.
    DATA: lc_integrar TYPE REF TO zcl_integracao.

    DATA: e_integracao_log TYPE zintegracao_log.

    r_if_integracao_recibos_coupa = me.

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
  endmethod.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~SET_SERVICO.
    IF i_servico IS NOT INITIAL.
      zif_integracao_recibos_coupa_r~at_servico = i_servico.
    ENDIF.
  endmethod.


  method ZIF_INTEGRACAO_RECIBOS_COUPA_R~SET_USER_BUSCAR.
  endmethod.
ENDCLASS.
