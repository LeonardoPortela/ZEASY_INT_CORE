class ZCL_INTEGRACAO_TX_CAMBIO_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_TX_CAMB_COUPA .

  data AT_TAXA_CAMBIO_STRUCT type ZMMS_DADOS_TX_CAMBIO_COUPA .

  methods CONSTRUCTOR
    importing
      !I_SERVICO type ZTIPOWEBSERV optional
      !I_TAXA_CAMBIO type ZMMS_DADOS_TX_CAMBIO_COUPA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TX_CAMBIO_COUPA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_taxa_cambio.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    " Move as informacoes da taxa de cambio
    me->at_taxa_cambio_struct = i_taxa_cambio.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_tx_camb_coupa~at_auth_ws
        WHERE service = 'COUPA_TAXA_CAMBIO'.
*   IF sy-subrc IS INITIAL.
*     me->zif_integracao_tx_camb_coupa~at_auth_ws-url = me->zif_integracao_tx_camb_coupa~at_auth_ws-url &&
*     'exchange_rates?fields=["id","rate","rate_date",{"from_currency": ["code","decimals"]},{"to_currency": ["code","decimals"]}]'.
*   ENDIF.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_tx_camb_coupa~at_token_ws
        WHERE service = 'COUPA_TOKEN'.

    IF me->zif_integracao_tx_camb_coupa~at_auth_ws  IS INITIAL OR
       me->zif_integracao_tx_camb_coupa~at_token_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  endmethod.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
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
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    DATA: lva_buffer TYPE xstring.

    DATA: lit_xml_table TYPE TABLE OF smum_xmltb,
          lit_return    TYPE TABLE OF bapiret2.

    DATA: lva_already TYPE c LENGTH 7 VALUE 'already'.

    IF i_msg_retorno CS lva_already.
      me->zif_integracao_tx_camb_coupa~at_retorno = 'D'.
    ELSE.

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

      me->zif_integracao_tx_camb_coupa~at_retorno = e_sucesso.

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_tx_camb_coupa~get_id_referencia.
    r_if_integracao_tx_camb_coupa = me.

    e_referencia-id_referencia = sy-datum.
    e_referencia-tp_referencia = 'COUPA_TX_CAMBIO'.
  ENDMETHOD.


  method ZIF_INTEGRACAO_TX_CAMB_COUPA~GET_INSTANCE.

  endmethod.


  METHOD zif_integracao_tx_camb_coupa~get_retorno.
    e_retorno = me->zif_integracao_tx_camb_coupa~at_retorno.
    r_if_integracao_tx_camb_coupa = me.
  ENDMETHOD.


  METHOD zif_integracao_tx_camb_coupa~get_xml.

    DATA: lv_full_xml TYPE string.

    " Inicial XML
    lv_full_xml = '<exchange-rate>'.

    " Valor da taxa de cambio
    lv_full_xml = lv_full_xml && '<rate type="decimal">'.
    lv_full_xml = lv_full_xml && me->at_taxa_cambio_struct-taxa_cambio.
    lv_full_xml = lv_full_xml && '</rate>'.

    " Data do dia.
    lv_full_xml = lv_full_xml && '<rate-date type="dateTime">'.
    lv_full_xml = lv_full_xml && me->at_taxa_cambio_struct-data_taxa_cambio.
    lv_full_xml = lv_full_xml && '</rate-date>'.

    " Moeda from
    lv_full_xml = lv_full_xml && '<from-currency>'.
    lv_full_xml = lv_full_xml && '<code>'.
    lv_full_xml = lv_full_xml && me->at_taxa_cambio_struct-moeda_procedente.
    lv_full_xml = lv_full_xml && '</code>'.
    lv_full_xml = lv_full_xml && '</from-currency>'.

    " Moeda from
    lv_full_xml = lv_full_xml && '<to-currency>'.
    lv_full_xml = lv_full_xml && '<code>'.
    lv_full_xml = lv_full_xml && me->at_taxa_cambio_struct-moeda_destino.
    lv_full_xml = lv_full_xml && '</code>'.
    lv_full_xml = lv_full_xml && '</to-currency>'.

    " Finaliza XML
    lv_full_xml = lv_full_xml && '</exchange-rate>'.

    e_xml = lv_full_xml.

  ENDMETHOD.


  METHOD zif_integracao_tx_camb_coupa~set_ds_data.

    r_if_integracao_tx_camb_coupa = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.

  ENDMETHOD.


  METHOD zif_integracao_tx_camb_coupa~set_ds_url.

    r_if_integracao_tx_camb_coupa = me.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_tx_camb_coupa~at_auth_ws-url &&
    'exchange_rates?fields=["id","rate","rate_date",{"from_currency": ["code","decimals"]},{"to_currency": ["code","decimals"]}]'..
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_tx_camb_coupa~set_id_referencia( ).

  ENDMETHOD.


  METHOD zif_integracao_tx_camb_coupa~set_id_referencia.

    r_if_integracao_tx_camb_coupa = me.

    me->zif_integracao_tx_camb_coupa~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).


  ENDMETHOD.


  METHOD zif_integracao_tx_camb_coupa~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_tx_camb_coupa = me.

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


  METHOD zif_integracao_tx_camb_coupa~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_integracao_tx_camb_coupa~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_TX_CAMB_COUPA~SET_TX_CAMBIO_BUSCAR.

    r_if_integracao_tx_camb_coupa = me.

  endmethod.


  method ZIF_INTEGRACAO_TX_CAMB_COUPA~SET_USER_BUSCAR.
  endmethod.
ENDCLASS.
