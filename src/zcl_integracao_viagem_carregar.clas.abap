class ZCL_INTEGRACAO_VIAGEM_CARREGAR definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_VIAGEM_CARREGAR .

  data AT_NFE_CARREGAMENTO type ZLESS0007 .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_VIAGEM_CARREGAR IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_carre.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_carre.

    IF SY-SUBRC EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    me->zif_integracao_viagem_carregar~set_ds_url( ).
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


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_VIAGEM_CARREGAR~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_CARREGAR = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_VIAGEM_CARREGAR'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_VIAGEM_CARREGAR~AT_VIAGEM-VIAGEM_ID.

  endmethod.


  METHOD ZIF_INTEGRACAO_VIAGEM_CARREGAR~GET_INSTANCE.

    IF ZIF_INTEGRACAO_VIAGEM_CARREGAR~AT_IF_INTEGRACAO_CARREGAR IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_VIAGEM_CARREGAR~AT_IF_INTEGRACAO_CARREGAR
        TYPE ZCL_INTEGRACAO_VIAGEM_CARREGAR.
    ENDIF.
    R_IF_INTEGRACAO_CARREGAR = ZIF_INTEGRACAO_VIAGEM_CARREGAR~AT_IF_INTEGRACAO_CARREGAR.

  ENDMETHOD.


  METHOD zif_integracao_viagem_carregar~get_json.

    TYPES BEGIN OF ty_pesos.
    TYPES: quantidade TYPE zde_nm_peso_bruto.
    TYPES: unidade_medida TYPE string.
    TYPES END OF ty_pesos.

    TYPES BEGIN OF ty_carregar.
    TYPES: data_carregamento TYPE string.
    TYPES: peso_tara TYPE ty_pesos.
    TYPES: peso_liquido TYPE ty_pesos.
    TYPES END OF ty_carregar.

    DATA: lc_carrega TYPE ty_carregar.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
    TYPES BEGIN OF ty_carregar_com_nfe.
    TYPES: data_carregamento TYPE string.
    TYPES: peso_tara TYPE ty_pesos.
    TYPES: peso_liquido TYPE ty_pesos.
    TYPES: nota_fiscal TYPE zless0007.
    TYPES END OF ty_carregar_com_nfe.

    DATA: lc_carrega_nfe TYPE ty_carregar_com_nfe.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--

    r_if_integracao_carregar = me.

    SELECT SINGLE * INTO @DATA(wa_zlest0181)
      FROM zlest0181
     WHERE id_lote_frete EQ @me->zif_integracao_viagem_carregar~at_viagem-id_lote_frete.

    IF sy-subrc IS INITIAL.
      DATA(unidade) = wa_zlest0181-gewei.
    ELSE.
      unidade = 'KG'.
    ENDIF.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_carre.

    IF sy-subrc EQ 0.
      lc_carrega-data_carregamento           = me->zif_integracao_viagem_carregar~at_viagem-dt_carregado+0(4) && '-' &&
                                               me->zif_integracao_viagem_carregar~at_viagem-dt_carregado+4(2) && '-' &&
                                               me->zif_integracao_viagem_carregar~at_viagem-dt_carregado+6(2).
    ELSE.


      lc_carrega-data_carregamento           = me->zif_integracao_viagem_carregar~at_viagem-dt_carregado+6(2) && '/' &&
                                               me->zif_integracao_viagem_carregar~at_viagem-dt_carregado+4(2) && '/' &&
                                               me->zif_integracao_viagem_carregar~at_viagem-dt_carregado(4).
    ENDIF.

    lc_carrega-peso_tara-quantidade        = me->zif_integracao_viagem_carregar~at_viagem-nm_peso_tara.
    lc_carrega-peso_tara-unidade_medida    = unidade.
    lc_carrega-peso_liquido-quantidade     = me->zif_integracao_viagem_carregar~at_viagem-nm_peso_liquido.
    lc_carrega-peso_liquido-unidade_medida = unidade.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
    IF me->at_nfe_carregamento IS NOT INITIAL.
      MOVE-CORRESPONDING lc_carrega TO lc_carrega_nfe.
      lc_carrega_nfe-nota_fiscal = me->at_nfe_carregamento.

      IF lwa_call_direct_carguero IS NOT INITIAL.
        lc_carrega_nfe-nota_fiscal-data_emissao     = me->at_nfe_carregamento-data_emissao+0(4) && '-' &&
                                                      me->at_nfe_carregamento-data_emissao+4(2) && '-' &&
                                                      me->at_nfe_carregamento-data_emissao+6(2).
      ELSE.
        lc_carrega_nfe-nota_fiscal-data_emissao     = me->at_nfe_carregamento-data_emissao+6(2) && '/' &&
                                                      me->at_nfe_carregamento-data_emissao+4(2) && '/' &&
                                                      me->at_nfe_carregamento-data_emissao(4).
      ENDIF.

      e_json = zcl_fmcall_base=>abap2json( abap_data = lc_carrega_nfe ).
    ELSE.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--
      e_json = zcl_fmcall_base=>abap2json( abap_data = lc_carrega ).
    ENDIF. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

  ENDMETHOD.


  method ZIF_INTEGRACAO_VIAGEM_CARREGAR~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_CARREGAR = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  endmethod.


  METHOD ZIF_INTEGRACAO_VIAGEM_CARREGAR~SET_DS_URL.

    R_IF_INTEGRACAO_CARREGAR = ME.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @zif_integracao=>at_id_interface_viagem_carre.

    IF SY-SUBRC EQ 0.

      SELECT SINGLE * INTO @DATA(LWA_ZAUTH_WEBSERVICE)
        FROM ZAUTH_WEBSERVICE
       WHERE SERVICE = 'CARGUERO_HOST'.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
                              MSGNO = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
                              ATTR1 = 'O'
                              ATTR2 = '01' )
            MSGID  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
            MSGNO  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
            MSGTY  = 'E'
            MSGV1  = 'O'
            MSGV2  = '01'.
      ENDIF.

      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO          = 'JSON'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE     = LWA_ZAUTH_WEBSERVICE-CONTENT_TYPE.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN = LWA_ZAUTH_WEBSERVICE-URL_TOKEN.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_VIAGEM_CARREGAR~AT_VIAGEM-VIAGEM_ID && '/carregamento'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_VIAGEM_CARREGAR=>AT_FC_PROCESSAR_VIAGEM_CARREGA.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

      ME->ZIF_INTEGRACAO_VIAGEM_CARREGAR~SET_ID_REFERENCIA( ).


      EXIT.

    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_WEBSERVICE)
      FROM ZCIOT_WEBSERVICE
     WHERE TIPO    EQ 'O'
       AND SERVICO EQ '01'.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
                            ATTR1 = 'O'
                            ATTR2 = '01' )
          MSGID  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
          MSGTY  = 'E'
          MSGV1  = 'O'
          MSGV2  = '01'.
    ENDIF.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO          = 'JSON'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE     = WA_WEBSERVICE-CONTENT_TYPE.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN = WA_WEBSERVICE-URL_TOKEN.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'viagens/' && ME->ZIF_INTEGRACAO_VIAGEM_CARREGAR~AT_VIAGEM-VIAGEM_ID && '/carregamento'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_VIAGEM_CARREGAR=>AT_FC_PROCESSAR_VIAGEM_CARREGA.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

    ME->ZIF_INTEGRACAO_VIAGEM_CARREGAR~SET_ID_REFERENCIA( ).


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_VIAGEM_CARREGAR~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_CARREGAR = ME.
    ME->ZIF_INTEGRACAO_VIAGEM_CARREGAR~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_VIAGEM_CARREGAR~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_CARREGAR = ME.

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

    CLEAR: LC_INTEGRAR.

  endmethod.


  METHOD zif_integracao_viagem_carregar~set_valida_envio_carregamento.

    DATA: l_status   TYPE zde_status_carguero_viagem.

    FREE: l_status,
          e_erro.

*---------------------------------------
*-- romaneio
*---------------------------------------
    SELECT id_ordem, fat_contingencia_ecc
      FROM zsdt0001
      INTO ( @DATA(l_id_ordem) , @DATA(fat_contingencia_ecc) )
        UP TO 1 ROWS
     WHERE ch_referencia = @i_ch_referencia.
    ENDSELECT.

    CHECK sy-subrc = 0              AND
        fat_contingencia_ecc = abap_false AND
        ( l_id_ordem IS NOT INITIAL AND
          l_id_ordem <> '0000000000' ).

*---------------------------------------
*-- viagem
*---------------------------------------
    SELECT viagem_id
      FROM zlest0185
      INTO @DATA(l_viagem_id)
        UP TO 1 ROWS
     WHERE id_ordem = @l_id_ordem.
    ENDSELECT.

    CHECK sy-subrc = 0.

*---------------------------------------
*-- Verificar status carregamento
*---------------------------------------
    "01	Carregamento
    "02	Liberação do Embarcador
    "03	Carregado (Pode ser Reenviado os dados não dé erro)
    "04	Descarregado
    "05	Cancelado
    "06	Rejeitado pelo Embarcador
    "07	Emissão de Nota Fiscal de Trânsito
    "08	Liberação do Carregamento
    "09	Carregamento Rejeitado pelo Embarcador
    "10	Carregamento
    "11	Resposta da Solicitação de Cancelamento
    "12	Carregamento
    "13	Resposta da Solicitação de Cancelamento
    "14	Cancelado
    "15 A. Documentos de Trânsito

    TRY.
        zcl_integracao_viagem_status=>zif_integracao_viagem_status~get_instance(
          )->set_viagem_status(
               EXPORTING
                 i_viagem_id = l_viagem_id
               IMPORTING
                 e_status    = l_status
          ).

      CATCH zcx_integracao INTO DATA(ex_integracao).
        IF NOT ( ex_integracao->msgid EQ zcx_integracao=>zcx_servico_http_config-msgid AND
                 ex_integracao->msgno EQ zcx_integracao=>zcx_servico_http_config-msgno ).
          e_erro = abap_true.
          EXIT.
        ENDIF.

      CATCH zcx_error INTO DATA(ex_error).
        e_erro = abap_true.
        EXIT.
    ENDTRY.

    IF l_status <> '01' AND
       l_status <> '03' AND
       l_status <> '12'.
      e_erro = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_viagem_carregar~set_viagem_carregar.

    DATA: lva_msg_erro TYPE string,
          v_peso_sub   TYPE zsdt0001-peso_fiscal. "CS2024000283 Parte 2 - ALGODÃO NO CARGUERO (STRADA) #180557 - BG

    r_if_integracao_carregar = me.

    IF i_remessas IS NOT INITIAL.

      READ TABLE i_remessas INTO DATA(lwa_remessa) INDEX 1.
      IF sy-subrc EQ 0 AND lwa_remessa-vbeln IS NOT INITIAL.
        SELECT SINGLE *
          FROM zlest0108 INTO @DATA(lwa_zlest0108)
         WHERE vbeln = @lwa_remessa-vbeln.

        IF sy-subrc EQ 0.
          CHECK lwa_zlest0108-fat_contingencia_ecc EQ abap_false.
        ENDIF.
      ENDIF.

      "Procurar Viagem Rodoviária Carguero Pela VT
      SELECT * INTO TABLE @DATA(it_vttp)
        FROM vttp
         FOR ALL ENTRIES IN @i_remessas
       WHERE vbeln EQ @i_remessas-vbeln.

      CHECK sy-subrc IS INITIAL.

      "Procura Documento de Transporte Rodoviário
      SELECT * INTO TABLE @DATA(it_vttk)
        FROM vttk
         FOR ALL ENTRIES IN @it_vttp
       WHERE tknum EQ @it_vttp-tknum
         AND vsart EQ '01'. "Rodoviário

      CHECK sy-subrc IS INITIAL.

      "Procurar Romaneio
      READ TABLE it_vttk INDEX 1 INTO DATA(wa_vttk).

      CHECK sy-subrc IS INITIAL AND wa_vttk-id_ordem IS NOT INITIAL.

      IF wa_vttk-id_viagem IS NOT INITIAL.
        "Procura Viagem por ID VIagem
        SELECT SINGLE * INTO @me->zif_integracao_viagem_carregar~at_viagem
          FROM zlest0185
         WHERE viagem_id EQ @wa_vttk-id_viagem.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE * INTO @DATA(wa_romaneio)
            FROM zsdt0001
           WHERE id_ordem     EQ @me->zif_integracao_viagem_carregar~at_viagem-id_ordem
             AND tp_movimento EQ 'S'.
          " "CS2024000283 Parte 2 - ALGODÃO NO CARGUERO (STRADA) #180557 - BG
          IF sy-subrc IS INITIAL. "// BUG-185870 WBARBOSA 22/07/2025

            LOOP AT i_remessas INTO DATA(wa_remessa).
              v_peso_sub = v_peso_sub + wa_remessa-btgew.
            ENDLOOP.

            wa_romaneio-peso_subtotal = v_peso_sub.

          ENDIF."// BUG-185870 WBARBOSA 22/07/2025
          "CS2024000283 Parte 2 - ALGODÃO NO CARGUERO (STRADA) #180557 - BG

        ENDIF.

      ELSEIF wa_vttk-id_ordem IS NOT INITIAL.
        "Procura Viagem por Id Ordem de Carregamento
        SELECT SINGLE * INTO @me->zif_integracao_viagem_carregar~at_viagem
          FROM zlest0185
         WHERE id_ordem EQ @wa_vttk-id_ordem.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE * INTO @wa_romaneio
            FROM zsdt0001
           WHERE id_ordem EQ @wa_vttk-id_ordem
             AND tp_movimento EQ 'S'.
        ENDIF.

      ELSE.

        SELECT SINGLE * INTO @wa_romaneio
          FROM zsdt0001
         WHERE doc_transp EQ @wa_vttk-tknum.

        IF sy-subrc IS INITIAL AND wa_romaneio-id_ordem IS NOT INITIAL.
          "Procura Viagem por Id Ordem de Carregamento do Romaneio
          SELECT SINGLE * INTO @me->zif_integracao_viagem_carregar~at_viagem
            FROM zlest0185
           WHERE id_ordem EQ @wa_romaneio-id_ordem.
        ELSE.
          "Não Vai achar a Viagem
          sy-subrc = 1.
        ENDIF.

      ENDIF.

      CHECK sy-subrc IS INITIAL.

      CHECK wa_romaneio-fat_contingencia_ecc EQ abap_false.

      "Buscar Peso Tara
      IF wa_romaneio-peso_tara IS INITIAL OR wa_romaneio-peso_subtotal IS INITIAL.
        DATA: lc_romaneio TYPE REF TO zcl_romaneio.
        CREATE OBJECT lc_romaneio.
        lc_romaneio->set_registro( i_id_registro = wa_romaneio-ch_referencia ).
        wa_romaneio-peso_tara     = lc_romaneio->get_peso_tara( ).
        wa_romaneio-peso_subtotal = lc_romaneio->get_peso_subtotal( ).
        FREE lc_romaneio.
        CLEAR: lc_romaneio.
      ENDIF.

      "Datas e Pesos Anteriores
      DATA(dt_carregado)    = me->zif_integracao_viagem_carregar~at_viagem-dt_carregado.
      DATA(nm_peso_tara)    = me->zif_integracao_viagem_carregar~at_viagem-nm_peso_tara.
      DATA(nm_peso_liquido) = me->zif_integracao_viagem_carregar~at_viagem-nm_peso_liquido.

      me->zif_integracao_viagem_carregar~at_viagem-dt_carregado    = i_dt_carregamento.
      me->zif_integracao_viagem_carregar~at_viagem-nm_peso_tara    = CONV #( wa_romaneio-peso_tara ).
      me->zif_integracao_viagem_carregar~at_viagem-nm_peso_liquido = CONV #( wa_romaneio-peso_subtotal ).

    ELSE.

      SELECT SINGLE * INTO @me->zif_integracao_viagem_carregar~at_viagem
        FROM zlest0185
       WHERE viagem_id EQ @i_viagem_id.

      CHECK sy-subrc IS INITIAL.

      "Datas e Pesos Anteriores
      dt_carregado    = me->zif_integracao_viagem_carregar~at_viagem-dt_carregado.
      nm_peso_tara    = me->zif_integracao_viagem_carregar~at_viagem-nm_peso_tara.
      nm_peso_liquido = me->zif_integracao_viagem_carregar~at_viagem-nm_peso_liquido.

      me->zif_integracao_viagem_carregar~at_viagem-dt_carregado    = i_dt_carregamento.
      me->zif_integracao_viagem_carregar~at_viagem-nm_peso_tara    = i_peso_tara.
      me->zif_integracao_viagem_carregar~at_viagem-nm_peso_liquido = i_peso_liquido.

    ENDIF.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
    me->at_nfe_carregamento = i_nfe_carregamento.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--

    IF me->zif_integracao_viagem_carregar~at_viagem-tknum EQ i_tknum.
      DATA(ck_alteracao_vt) = abap_true.
      me->zif_integracao_viagem_carregar~at_viagem-tknum = i_tknum.
    ENDIF.

    "Mudou Peso/Data/Documento Transporte
    IF dt_carregado IS INITIAL AND nm_peso_tara IS INITIAL AND nm_peso_liquido IS INITIAL.
      DATA(ck_alteracao) = abap_false.
    ELSEIF dt_carregado    NE me->zif_integracao_viagem_carregar~at_viagem-dt_carregado    OR
           nm_peso_tara    NE me->zif_integracao_viagem_carregar~at_viagem-nm_peso_tara    OR
           nm_peso_liquido NE me->zif_integracao_viagem_carregar~at_viagem-nm_peso_liquido .
      ck_alteracao = abap_true.
    ENDIF.

    DATA(lva_inf_carregamento_carguero) = abap_true.

    IF ( me->zif_integracao_viagem_carregar~at_viagem-ck_carregado EQ abap_true ) AND ( ck_alteracao EQ abap_false ).

      "Verificar se Ainda está Carregado
      zcl_integracao_viagem_status=>zif_integracao_viagem_status~get_instance(
        )->set_viagem_status(
             EXPORTING
               i_viagem_id = me->zif_integracao_viagem_carregar~at_viagem-viagem_id
             IMPORTING
               e_status = DATA(e_status)
        ).

      "01	Carregamento
      "02	Liberação do Embarcador
      "03	Carregado (Pode ser Reenviado os dados não dé erro)
      "04	Descarregado
      "05	Cancelado
      "06	Rejeitado pelo Embarcador
      "07	Emissão de Nota Fiscal de Trânsito
      "08	Liberação do Carregamento
      "09	Carregamento Rejeitado pelo Embarcador
      "10	Carregamento
      "11	Resposta da Solicitação de Cancelamento
      "12	Carregamento
      "13	Resposta da Solicitação de Cancelamento
      "14	Cancelado
      "15 A. Documentos de Trânsito

      CHECK e_status EQ '01' OR
            e_status EQ '12' OR
            e_status EQ '03'.

    ELSE.

      "Verificar se Ainda está Carregado
      zcl_integracao_viagem_status=>zif_integracao_viagem_status~get_instance(
        )->set_viagem_status(
             EXPORTING
               i_viagem_id = me->zif_integracao_viagem_carregar~at_viagem-viagem_id
             IMPORTING
               e_status = e_status
        ).

      IF e_status EQ '15'.
        EXIT.
      ENDIF.

      IF e_status EQ '08'.
        lva_inf_carregamento_carguero = abap_false.
        CLEAR: e_id_integracao.
      ENDIF.

    ENDIF.

    IF lva_inf_carregamento_carguero EQ abap_true.
      "Inclui Json na Mesagem a Ser Enviada
      me->zif_integracao_viagem_carregar~get_json( IMPORTING e_json = DATA(lc_json)
        )->set_ds_data( EXPORTING i_json = lc_json
        )->set_ds_url(
        )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao
        ).

      SELECT SINGLE *
        FROM zintegracao INTO @DATA(lwa_integracao)
       WHERE id_integracao EQ @e_id_integracao.

      IF sy-subrc EQ 0.
        TRANSLATE lwa_integracao-ds_data_retorno TO UPPER CASE.

        IF lwa_integracao-ds_data_retorno = 'NULL'.

          lva_msg_erro = 'Carguero: Retorno informe de carregamento nao pode ser null!'.

          zcx_error=>zif_error~gera_erro_geral( lva_msg_erro ).
        ENDIF.
      ENDIF.
    ENDIF.

    me->zif_integracao_viagem_carregar~at_viagem-ck_carregado = abap_true.
    me->zif_integracao_viagem_carregar~at_viagem-id_integracao_carregar = e_id_integracao.
    MODIFY zlest0185 FROM me->zif_integracao_viagem_carregar~at_viagem.
    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
