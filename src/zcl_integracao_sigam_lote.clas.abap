class ZCL_INTEGRACAO_SIGAM_LOTE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_SIGAM_LOTE .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.

  class-methods CONVERTE_RESPONSE_ITAB
    importing
      !I_REQUEST type ZSDE0007
      !I_RESPONSE_JSON type STRING
    returning
      value(R_RET_TAB) type ZSDC0005 .
ENDCLASS.



CLASS ZCL_INTEGRACAO_SIGAM_LOTE IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_sigam_lote~at_servico      = 'SIGAM_CONSULTA_LOTE_COMPRA'.
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_ftp.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    "me->zif_integracao_inject~at_autentica_api_ad = zif_integracao=>at_id_interface_aut_api_ad_sim.
    "me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    "me->zif_integracao_inject~at_autentica_module = 'goflux'.

    me->zif_integracao_inject~at_autentica_api_ad = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_autentica_module = ''.


    IF me->zif_integracao_sigam_lote~at_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_sigam_lote~at_auth_ws
          WHERE service = me->zif_integracao_sigam_lote~at_servico .

    ENDIF.

    IF me->zif_integracao_sigam_lote~at_auth_ws  IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD converte_response_itab.

    DATA lt_prod_perm TYPE TABLE OF cvp_s_lifnr.

    DATA lw_req TYPE zsde0009.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_response_json
      CHANGING
        data = lw_req.

    LOOP AT lw_req-data ASSIGNING FIELD-SYMBOL(<fs_struc>).

      APPEND INITIAL LINE TO r_ret_tab ASSIGNING FIELD-SYMBOL(<fs_respo>).

      <fs_respo>-bukrs = <fs_struc>-empresa.
      <fs_respo>-nu_compra = <fs_struc>-numerocompra.
      <fs_respo>-compra = <fs_struc>-compra.
      <fs_respo>-id_compra = <fs_struc>-idcompra.
      <fs_respo>-tipooperacao = <fs_struc>-tipooperacao.
      <fs_respo>-transgenia = <fs_struc>-transgenia.
      <fs_respo>-ano = <fs_struc>-anoliquidez.
      <fs_respo>-mes = <fs_struc>-mesliquidez.
      <fs_respo>-periodo = <fs_struc>-periodoliquidez.

      <fs_respo>-filial = |{ <fs_struc>-filial ALPHA = IN }|.
      <fs_respo>-filial_ped = |{ i_request-idfilial ALPHA = IN }|.
      <fs_respo>-matnr = |{ <fs_struc>-produto ALPHA = IN }|.
      <fs_respo>-produtor = |{ <fs_struc>-produtor ALPHA = IN }|.
      <fs_respo>-ponto_coleta = |{ <fs_struc>-pontocoleta ALPHA = IN }|.
      <fs_respo>-local_entrega = |{ <fs_struc>-localentrega ALPHA = IN }|.

      "<fs_respo>-lcl_entr_branch = <fs_struc>-localentrega.

      WRITE <fs_respo>-local_entrega TO <fs_respo>-lcl_entr_branch NO-ZERO LEFT-JUSTIFIED.

      UNPACK <fs_respo>-lcl_entr_branch TO <fs_respo>-lcl_entr_branch.

      LOOP AT <fs_struc>-portos ASSIGNING FIELD-SYMBOL(<fs_portos>).

        IF sy-tabix = 1.
          <fs_respo>-porto = <fs_portos>-porto.
        ELSE.
          <fs_respo>-porto = <fs_respo>-porto && `,` && <fs_portos>-porto.
        ENDIF.

        LOOP AT <fs_struc>-produtorespermitido ASSIGNING FIELD-SYMBOL(<fs_prod_perm>).

          APPEND INITIAL LINE TO <fs_respo>-prod_perm_tab ASSIGNING FIELD-SYMBOL(<fs_prod_respo>).
          APPEND INITIAL LINE TO lt_prod_perm ASSIGNING FIELD-SYMBOL(<fs_prods>).

          <fs_prods>-lifnr = <fs_prod_perm>-produtor.
          <fs_prod_respo>-produtor = <fs_prod_perm>-produtor.
          <fs_prod_respo>-filial = <fs_prod_perm>-filial.

        ENDLOOP.

        IF <fs_respo>-prod_perm_tab IS NOT INITIAL.
          <fs_respo>-icon_prod_perm = '@5W@'.
        ELSE.
          <fs_respo>-icon_prod_perm = ''.
        ENDIF.

      ENDLOOP.

      SORT lt_prod_perm.
      DELETE ADJACENT DUPLICATES FROM lt_prod_perm.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = <fs_struc>-unidademedida
        IMPORTING
          output         = <fs_respo>-un_lote
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
        <fs_respo>-un_lote = <fs_struc>-unidademedida.
      ENDIF.

      <fs_respo>-tp_frete = <fs_struc>-tipofrete.
      <fs_respo>-lote = <fs_struc>-compra.
      <fs_respo>-qtde = <fs_struc>-quantidade && '.00'.
      <fs_respo>-saldo = <fs_struc>-saldoentregar.

      <fs_respo>-nr_safra = <fs_struc>-safra.
      <fs_respo>-produtoderivado = <fs_struc>-produtoderivado.

      <fs_respo>-desc_produtor = ''.

      <fs_respo>-desc_coleta = ''.

      <fs_respo>-desc_local = ''.

      <fs_respo>-desc_porto = ''.

    ENDLOOP.

    CHECK r_ret_tab IS NOT INITIAL.

    SELECT * FROM lfa1
      INTO TABLE @DATA(lt_lfa1_prod)
        FOR ALL ENTRIES IN @r_ret_tab
          WHERE lifnr = @r_ret_tab-produtor.

    SELECT * FROM lfa1
      INTO TABLE @DATA(lt_lfa1_prod_perm)
        FOR ALL ENTRIES IN @lt_prod_perm
          WHERE lifnr = @lt_prod_perm-lifnr.

    SELECT * FROM lfa1
      INTO TABLE @DATA(lt_lfa1_col)
        FOR ALL ENTRIES IN @r_ret_tab
          WHERE lifnr = @r_ret_tab-ponto_coleta.

    SELECT * FROM kna1
      INTO TABLE @DATA(lt_kna1_ent)
        FOR ALL ENTRIES IN @r_ret_tab
          WHERE kunnr = @r_ret_tab-local_entrega.

    SELECT * FROM j_1bbranch
      INTO TABLE @DATA(lt_branch)
        FOR ALL ENTRIES IN @r_ret_tab
          WHERE branch = @r_ret_tab-lcl_entr_branch.

    SELECT * FROM makt
       INTO TABLE @DATA(lt_makt)
         FOR ALL ENTRIES IN @r_ret_tab
           WHERE matnr = @r_ret_tab-matnr
             AND spras = @sy-langu.

    SELECT lifnr FROM lfa1
      INTO TABLE @DATA(lt_lfa1_zfic)
        FOR ALL ENTRIES IN @r_ret_tab
          WHERE lifnr = @r_ret_tab-local_entrega
            AND land1 = 'BR'
            AND ktokk  = 'ZFIC'.

    LOOP AT r_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

      READ TABLE lt_lfa1_prod ASSIGNING FIELD-SYMBOL(<fs_prod>)
       WITH KEY lifnr = <fs_ret>-produtor.

      IF sy-subrc EQ 0.
        <fs_ret>-desc_produtor = <fs_prod>-name1.
      ENDIF.

*      READ TABLE lt_lfa1_por ASSIGNING FIELD-SYMBOL(<fs_por>)
*       WITH KEY lifnr = <fs_ret>-porto.
*
*      IF sy-subrc EQ 0.
*        <fs_ret>-desc_porto = <fs_por>-name1.
*      ENDIF.

      READ TABLE lt_lfa1_col ASSIGNING FIELD-SYMBOL(<fs_col>)
        WITH KEY lifnr = <fs_ret>-ponto_coleta.

      IF sy-subrc EQ 0.
        <fs_ret>-desc_coleta = <fs_col>-name1.
      ENDIF.

      READ TABLE lt_kna1_ent ASSIGNING FIELD-SYMBOL(<fs_ent>)
        WITH KEY kunnr = <fs_ret>-local_entrega.

      IF sy-subrc EQ 0.
        <fs_ret>-desc_local = <fs_ent>-name1.
      ENDIF.

      READ TABLE lt_branch ASSIGNING FIELD-SYMBOL(<fs_branch>)
        WITH KEY branch = <fs_ret>-lcl_entr_branch.

      IF sy-subrc EQ 0.
        <fs_ret>-lcl_entr_bukrs = <fs_branch>-bukrs.
      ENDIF.

      READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<fs_mat>)
       WITH KEY matnr = <fs_ret>-matnr.

      IF sy-subrc EQ 0.
        <fs_ret>-maktx = <fs_mat>-maktx.
      ENDIF.

      READ TABLE lt_lfa1_zfic ASSIGNING FIELD-SYMBOL(<fs_zfic>)
        WITH KEY lifnr = <fs_ret>-local_entrega.

      IF sy-subrc EQ 0.
        <fs_ret>-zfic_branch = <fs_zfic>-lifnr+6(4).
      ENDIF.

      LOOP AT <fs_ret>-prod_perm_tab ASSIGNING FIELD-SYMBOL(<fs_prod_2>).

        READ TABLE lt_lfa1_prod_perm ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
          WITH KEY lifnr = <fs_prod_2>-produtor.

        CHECK sy-subrc EQ 0.

        <fs_prod_2>-name1 = <fs_lfa1>-name1.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    "APPEND VALUE #( name = 'Content-Type' value = 'application/json' ) TO me->zif_integracao_inject~at_header_fields.

    r_if_integracao_inject = me.

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

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_sigam_lote~enviar_sigam.

    IF i_request IS INITIAL.
      zcx_error=>zif_error~gera_erro_geral( i_texto = 'Informar dados a serem consultados' ).
    ENDIF.

    zif_integracao_sigam_lote~get_instance(
      )->set_ds_url(
      )->set_ds_data( EXPORTING i_request = i_request
      )->set_send_msg( IMPORTING e_integracao = DATA(lw_ret) ).


    IF lw_ret-nm_code < 2005.
      MESSAGE s016(ds) WITH 'Dados consultados com sucesso'.
    ELSE.
      MESSAGE s016(ds) WITH 'Erro ao consultar' DISPLAY LIKE 'E'.
    ENDIF.

    r_respo_tab = converte_response_itab( i_request = i_request   i_response_json = lw_ret-ds_data_retorno ).

  ENDMETHOD.


  METHOD zif_integracao_sigam_lote~get_instance.

    IF zif_integracao_sigam_lote~at_instance IS NOT BOUND.

      zif_integracao_sigam_lote~at_instance = NEW zcl_integracao_sigam_lote( ).

    ENDIF.

    r_object = zif_integracao_sigam_lote~at_instance.

  ENDMETHOD.


  METHOD zif_integracao_sigam_lote~set_ds_data.

    me->zif_integracao_sigam_lote~at_body = /ui2/cl_json=>serialize( data = i_request ).

    r_object = me.

  ENDMETHOD.


  METHOD zif_integracao_sigam_lote~set_ds_url.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    "me->zif_integracao_inject~at_info_request_http-ds_form_data = me->zif_integracao_sigam_lote~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_sigam_lote~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_sigam_lote~at_auth_ws-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_integracao_sigam_lote~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_integracao_sigam_lote~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = me->zif_integracao_sigam_lote~at_auth_ws-url_token.

    r_object = me.

  ENDMETHOD.


  METHOD zif_integracao_sigam_lote~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_object = me.

    CREATE OBJECT lc_integrar.

*    lc_integrar->zif_integracao~at_msg_integra-ds_url_token = me->zif_integracao_sigam_lote~at_auth_ws-url_token.
*    lc_integrar->zif_integracao~at_msg_integra-ds_body = me->zif_integracao_sigam_lote~at_body.
*
    me->zif_integracao_inject~at_info_request_http-ds_body = me->zif_integracao_sigam_lote~at_body.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = me->zif_integracao_sigam_lote~at_auth_ws-url_token.


    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

*    IF e_integracao-nm_code < 205.
*      MESSAGE s016(ds) WITH e_integracao-ds_data_retorno.
*    ELSE.
*      MESSAGE s016(ds) WITH e_integracao-ds_data_retorno DISPLAY LIKE 'E'.
*    ENDIF.


    CLEAR: lc_integrar.

    r_object = me.

  ENDMETHOD.
ENDCLASS.
