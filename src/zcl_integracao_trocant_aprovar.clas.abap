class ZCL_INTEGRACAO_TROCANT_APROVAR definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_TROCANT_APROVAR .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TROCANT_APROVAR IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_viagem_apr_car.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

*-#147361-31.07.2024-JT-inicio
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_apr_car.

    IF sy-subrc EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    me->zif_integracao_trocant_aprovar~set_ds_url( ).
*-#147361-31.07.2024-JT-fim

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
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
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~get_id_referencia.

    r_if_integracao_carregar = me.

    e_referencia-tp_referencia = 'CARGUERO_TROCA_NOTA_APROVAR'.
    e_referencia-id_referencia = me->zif_integracao_trocant_aprovar~at_viagem-viagem_id.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~get_instance.

    IF zif_integracao_trocant_aprovar~at_if_integracao_carregar IS NOT BOUND.
      CREATE OBJECT zif_integracao_trocant_aprovar~at_if_integracao_carregar
        TYPE zcl_integracao_trocant_aprovar.
    ENDIF.
    r_if_integracao_carregar = zif_integracao_trocant_aprovar~at_if_integracao_carregar.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~get_json.

    DATA: l_chave    TYPE char44.

    r_if_integracao_carregar = me.

*--------------------------
*-- troca nota
*--------------------------
    SELECT *
      INTO @DATA(w_zsdt0001)
      FROM zsdt0001
        UP TO 1 ROWS
     WHERE ch_referencia = @i_ch_referencia.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_j_1bnfe_active)
      FROM j_1bnfe_active
        UP TO 1 ROWS
     WHERE docnum = @w_zsdt0001-nro_nf_prod.
    ENDSELECT.

    SELECT *
      INTO @DATA(w_j_1bnfdoc)
      FROM j_1bnfdoc
        UP TO 1 ROWS
     WHERE docnum = @w_zsdt0001-nro_nf_prod.
    ENDSELECT.

    CONCATENATE w_j_1bnfe_active-regio   "Região do emissor NF-e
                w_j_1bnfe_active-nfyear  "Ano da data do documento da NF-e
                w_j_1bnfe_active-nfmonth "Mês da data do documento da NF-e
                w_j_1bnfe_active-stcd1   "Nº CNPJ do emissor da NF-e
                w_j_1bnfe_active-model   "Modelo da nota fiscal
                w_j_1bnfe_active-serie   "SERIE
                w_j_1bnfe_active-nfnum9  "Nº NF-e de nove posições
                w_j_1bnfe_active-docnum9 "NF-e: nº aleatório
                w_j_1bnfe_active-cdv     "Dígito controle p/chave de acesso NF-e
           INTO l_chave.

    IF strlen( l_chave ) NE 44.
      CLEAR: l_chave.
    ENDIF.

    me->zif_integracao_trocant_aprovar~chave_nfe_embarcador = l_chave.

    e_json = zif_integracao_trocant_aprovar~monta_json_trocanota(
               EXPORTING i_chave          = l_chave
                         i_j_1bnfe_active = w_j_1bnfe_active
                         i_j_1bnfdoc      = w_j_1bnfdoc
                         i_zsdt0001       = w_zsdt0001 ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_TROCANT_APROVAR~GET_URL_DOCUMENTO.

    SELECT SINGLE *
      FROM ZLEST0185 INTO @DATA(LWA_VIAGEM)
     WHERE VIAGEM_ID EQ @I_VIAGEM_ID.

    IF ( SY-SUBRC NE 0 ) OR ( I_VIAGEM_ID IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>ZCX_ERRO_GERAL-msgid
                            msgno = zcx_integracao=>ZCX_ERRO_GERAL-msgno
                            attr1 = 'Não encontrada Viagem com Id:' && I_VIAGEM_ID )
          msgid  = zcx_integracao=>ZCX_ERRO_GERAL-msgid
          msgno  = zcx_integracao=>ZCX_ERRO_GERAL-msgno
          msgty  = 'E'
          msgv1  = 'Não encontrada Viagem com Id:' && I_VIAGEM_ID.
    endif.

    IF ( LWA_VIAGEM-BLOB_PATH IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>ZCX_ERRO_GERAL-msgid
                            msgno = zcx_integracao=>ZCX_ERRO_GERAL-msgno
                            attr1 = 'Não encontrado URL(blob_path) da Viagem'
                            attr2 = 'com Id:' && I_VIAGEM_ID
                            )
          msgid  = zcx_integracao=>ZCX_ERRO_GERAL-msgid
          msgno  = zcx_integracao=>ZCX_ERRO_GERAL-msgno
          msgty  = 'E'
          msgv1  = 'Não encontrado URL(blob_path) da Viagem'
          msgv2  = 'com Id:' && I_VIAGEM_ID.
    endif.

    r_url  = lwa_viagem-blob_path && '/DocumentosViagens' && i_viagem_id && '.pdf'.

  endmethod.


  METHOD zif_integracao_trocant_aprovar~monta_json_trocanota.

    DATA: l_dt_emis    TYPE string,
          l_brgew      TYPE string,
          l_peso_tara  TYPE string,
          l_peso_bruto TYPE string,
          l_url        TYPE string.

    CONCATENATE i_j_1bnfdoc-credat(4)   '-'
                i_j_1bnfdoc-credat+4(2) '-'
                i_j_1bnfdoc-credat+6(2) 'T'
                i_j_1bnfdoc-cretim(2)   ':'
                i_j_1bnfdoc-cretim+2(2) ':'
                i_j_1bnfdoc-cretim+4(2) 'Z'
           INTO l_dt_emis.

    l_brgew      = i_j_1bnfdoc-brgew.
    l_peso_tara  = i_zsdt0001-peso_tara.
    l_peso_bruto = i_zsdt0001-peso_bruto.

    IF ( i_zsdt0001-peso_tara IS INITIAL OR i_zsdt0001-peso_bruto IS INITIAL ) AND i_zsdt0001-ch_referencia IS NOT INITIAL.
      SELECT SINGLE *
        FROM ZSDT0001OVRO INTO @DATA(LWA_ZSDT001OVRO)
       WHERE ch_referencia_sai EQ @i_zsdt0001-ch_referencia.

      IF SY-SUBRC EQ 0.
        l_peso_tara  = LWA_ZSDT001OVRO-nm_peso_tara.
        l_peso_bruto = LWA_ZSDT001OVRO-nm_peso_bruto.
      ENDIF.
    ENDIF.

    l_url = zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_url_documento( EXPORTING i_viagem_id  = me->zif_integracao_trocant_aprovar~at_viagem-viagem_id ).

    CHECK l_url is NOT INITIAL.

    CONCATENATE '{"nota_fiscal":{'
                '"chave_acesso":"'       i_chave '",'
                '"data_emissao":"'       l_dt_emis '",'
                '"quantidade_faturada":' l_brgew ','
                '"url_documento":"'      l_url '",'
                '"numero":"'             i_j_1bnfe_active-nfnum9 '",'
                '"serie":"'              i_j_1bnfe_active-serie '"},'
                '"peso_tara":{'
                '"unidade_medida":"KG' '",'
                '"quantidade":'          l_peso_tara '},'
                '"peso_bruto":{'
                '"unidade_medida":"KG' '",'
                '"quantidade":'          l_peso_bruto '}'
                '}'
           INTO e_json.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~set_ds_data.

    "Incluir Texto JSON para integração
    r_if_integracao_carregar = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~set_ds_url.

    r_if_integracao_carregar = me.

*-#147361-31.07.2024-JT-inicio
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_call_direct_carguero)
     WHERE name = 'CALL_CARGUERO_DIRECT'
       AND low  = @zif_integracao=>at_id_interface_viagem_apr_car.

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
    ELSE.
*-#147361-31.07.2024-JT-inicio
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

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_viagem_carregar=>at_fc_processar_viagem_carrega.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'PUT'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && 'viagens/' && me->zif_integracao_trocant_aprovar~at_viagem-viagem_id && '/carregamento/aprovar'.
    me->zif_integracao_trocant_aprovar~set_id_referencia( ).

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~set_id_referencia.

    "Incluir Chave de Referência
    r_if_integracao_carregar = me.
    me->zif_integracao_trocant_aprovar~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~set_id_viagem.

    r_if_integracao_carregar = me.

*------------------------------------
* procura viagem
*------------------------------------
    SELECT *
      INTO @DATA(wa_vttk)
      FROM vttk
        UP TO 1 ROWS
     WHERE tknum EQ @i_tknum.
    ENDSELECT.

    CHECK wa_vttk-id_viagem IS NOT INITIAL.

*------------------------------------
*   Procura Viagem por ID VIagem
*------------------------------------
    SELECT SINGLE *
             INTO @me->zif_integracao_trocant_aprovar~at_viagem
             FROM zlest0185
            WHERE viagem_id EQ @wa_vttk-id_viagem.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_carregar = me.

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

    CLEAR: lc_integrar.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~set_viagem_carregar.

    DATA: lva_msg_erro TYPE string.

    r_if_integracao_carregar = me.

    FREE: e_id_integracao.

    IF i_remessas IS NOT INITIAL.

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
      READ TABLE it_vttp INDEX 1 INTO DATA(wa_vttp).
      READ TABLE it_vttk INDEX 1 INTO DATA(wa_vttk).

      CHECK sy-subrc IS INITIAL AND wa_vttk-id_ordem IS NOT INITIAL.

      IF wa_vttk-id_viagem IS NOT INITIAL.
        "Procura Viagem por ID VIagem
        SELECT SINGLE * INTO @me->zif_integracao_trocant_aprovar~at_viagem
          FROM zlest0185
         WHERE viagem_id EQ @wa_vttk-id_viagem.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE * INTO @DATA(wa_romaneio)
            FROM zsdt0001
           WHERE id_ordem     EQ @me->zif_integracao_trocant_aprovar~at_viagem-id_ordem
             AND tp_movimento EQ 'S'.
        ENDIF.

      ELSEIF wa_vttk-id_ordem IS NOT INITIAL.
        "Procura Viagem por Id Ordem de Carregamento
        SELECT SINGLE * INTO @me->zif_integracao_trocant_aprovar~at_viagem
          FROM zlest0185
         WHERE id_ordem EQ @wa_vttk-id_ordem.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE * INTO @wa_romaneio
            FROM zsdt0001
           WHERE id_ordem     EQ @wa_vttk-id_ordem
             AND tp_movimento EQ 'S'.
        ENDIF.

      ELSE.

        SELECT SINGLE * INTO @wa_romaneio
          FROM zsdt0001
         WHERE doc_transp   EQ @wa_vttk-tknum
           AND tp_movimento EQ 'S'.

        IF sy-subrc IS INITIAL AND wa_romaneio-id_ordem IS NOT INITIAL.

          "Procura Viagem por Id Ordem de Carregamento do Romaneio
          SELECT SINGLE * INTO @me->zif_integracao_trocant_aprovar~at_viagem
            FROM zlest0185
           WHERE id_ordem EQ @wa_romaneio-id_ordem.
        ELSE.
          "Não Vai achar a Viagem
          sy-subrc = 1.
        ENDIF.

      ENDIF.

      CHECK sy-subrc IS INITIAL.

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
      DATA(dt_carregado)    = me->zif_integracao_trocant_aprovar~at_viagem-dt_carregado.
      DATA(nm_peso_tara)    = me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_tara.
      DATA(nm_peso_liquido) = me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_liquido.

      me->zif_integracao_trocant_aprovar~at_viagem-dt_carregado    = i_dt_carregamento.
      me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_tara    = CONV #( wa_romaneio-peso_tara ).
      me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_liquido = CONV #( wa_romaneio-peso_subtotal ).

    ELSE.

      SELECT SINGLE * INTO @me->zif_integracao_trocant_aprovar~at_viagem
        FROM zlest0185
       WHERE viagem_id EQ @i_viagem_id.

      CHECK sy-subrc IS INITIAL.

      "Datas e Pesos Anteriores
      dt_carregado    = me->zif_integracao_trocant_aprovar~at_viagem-dt_carregado.
      nm_peso_tara    = me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_tara.
      nm_peso_liquido = me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_liquido.

      me->zif_integracao_trocant_aprovar~at_viagem-dt_carregado    = i_dt_carregamento.
      me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_tara    = i_peso_tara.
      me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_liquido = i_peso_liquido.

    ENDIF.

    IF me->zif_integracao_trocant_aprovar~at_viagem-tknum EQ i_tknum.
      DATA(ck_alteracao_vt) = abap_true.
      me->zif_integracao_trocant_aprovar~at_viagem-tknum = i_tknum.
    ENDIF.

    "Mudou Peso/Data/Documento Transporte
    IF dt_carregado IS INITIAL AND nm_peso_tara IS INITIAL AND nm_peso_liquido IS INITIAL.
      DATA(ck_alteracao) = abap_false.
    ELSEIF dt_carregado    NE me->zif_integracao_trocant_aprovar~at_viagem-dt_carregado    OR
           nm_peso_tara    NE me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_tara    OR
           nm_peso_liquido NE me->zif_integracao_trocant_aprovar~at_viagem-nm_peso_liquido .
      ck_alteracao = abap_true.
    ENDIF.

*---Chave nfe embarcador
    DATA(l_chave_embarcador) = me->zif_integracao_trocant_aprovar~at_viagem-chave_nfe_embarcador.
    me->zif_integracao_trocant_aprovar~chave_nfe_embarcador = l_chave_embarcador.

*--------------------------------------------------
*-- Valida status / envio aprovacao
*--------------------------------------------------
    TRY .
        zcl_integracao_trocant_aprovar=>zif_integracao_trocant_aprovar~get_instance(
          )->valida_envio_aprovacao(
               EXPORTING
                 i_ch_referencia = wa_romaneio-ch_referencia
               IMPORTING
                 e_erro          = DATA(l_erro)
                 e_status        = DATA(l_status)
                 e_msg_erro      = DATA(l_msg_erro)
                 e_id_integracao = e_id_integracao
          ).

      CATCH zcx_integracao.
      CATCH zcx_error.
    ENDTRY.

    IF l_erro = abap_true.
      lva_msg_erro = l_msg_erro.
      zcx_error=>zif_error~gera_erro_geral( lva_msg_erro ).
    ENDIF.

*----------------------------------------
*-- Envia aprovacao troca nota carguero
*----------------------------------------
    CASE l_status.

      WHEN '08'.
        TRY.
            me->zif_integracao_trocant_aprovar~get_json( EXPORTING i_ch_referencia = wa_romaneio-ch_referencia
                                                         IMPORTING e_json          = DATA(l_json_tn)
                  )->set_ds_data(  EXPORTING i_json          = l_json_tn
                  )->set_ds_url(
                  )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao
                  ).

          CATCH zcx_integracao.
            lva_msg_erro = 'Não foi possível enviar Aprovação ao Carguero!'.
            zcx_error=>zif_error~gera_erro_geral( lva_msg_erro ).

          CATCH zcx_error.
            lva_msg_erro = 'Não foi possível enviar Aprovação ao Carguero!'.
            zcx_error=>zif_error~gera_erro_geral( lva_msg_erro ).
        ENDTRY.

    ENDCASE.

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

    me->zif_integracao_trocant_aprovar~at_viagem-ck_carregado           = abap_true.
    me->zif_integracao_trocant_aprovar~at_viagem-id_integracao_carregar = e_id_integracao.
    me->zif_integracao_trocant_aprovar~at_viagem-chave_nfe_embarcador   = me->zif_integracao_trocant_aprovar~chave_nfe_embarcador.

    MODIFY zlest0185 FROM me->zif_integracao_trocant_aprovar~at_viagem.
    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~valida_chave_nfe.

    DATA: l_chave_nfe    TYPE char44.

    FREE: e_erro.

*---------------------------------------
*-- romaneio
*---------------------------------------
    SELECT nro_nf_prod, id_ordem
      FROM zsdt0001
      INTO (@DATA(l_nro_nf_prod), @DATA(l_id_ordem))
        UP TO 1 ROWS
     WHERE ch_referencia = @i_ch_referencia.
    ENDSELECT.

    CHECK sy-subrc = 0.

*---------------------------------------
*-- romaneio
*---------------------------------------
    SELECT chave_nfe_embarcador
      FROM zlest0185
      INTO @DATA(l_chave_nfe_embarcador)
        UP TO 1 ROWS
     WHERE id_ordem = @l_id_ordem.
    ENDSELECT.

    CHECK l_chave_nfe_embarcador IS NOT INITIAL.

*---------------------------------------
*-- n fiscal
*---------------------------------------
    IF l_nro_nf_prod IS NOT INITIAL.

      SELECT *
        INTO @DATA(w_j_1bnfe_active)
        FROM j_1bnfe_active
          UP TO 1 ROWS
       WHERE docnum = @l_nro_nf_prod.
      ENDSELECT.

      IF sy-subrc = 0.

*--------------------------------------
*  -- chafe NFE
*---------------------------------------
        CONCATENATE w_j_1bnfe_active-regio   "Região do emissor NF-e
                    w_j_1bnfe_active-nfyear  "Ano da data do documento da NF-e
                    w_j_1bnfe_active-nfmonth "Mês da data do documento da NF-e
                    w_j_1bnfe_active-stcd1   "Nº CNPJ do emissor da NF-e
                    w_j_1bnfe_active-model   "Modelo da nota fiscal
                    w_j_1bnfe_active-serie   "SERIE
                    w_j_1bnfe_active-nfnum9  "Nº NF-e de nove posições
                    w_j_1bnfe_active-docnum9 "NF-e: nº aleatório
                    w_j_1bnfe_active-cdv     "Dígito controle p/chave de acesso NF-e
               INTO l_chave_nfe.
      ENDIF.
    ENDIF.

    IF l_chave_nfe_embarcador <> l_chave_nfe.
      e_erro = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~valida_envio_aprovacao.

    FREE: e_erro,
          e_status,
          e_msg_erro,
          e_id_integracao.

    DATA: l_id_ordem             TYPE zsdt0001-id_ordem,
          l_doc_rem              TYPE zsdt0001-doc_rem,                   "#150020-27.08.2024-JT
          l_fat_contingencia_ecc TYPE zsdt0001-fat_contingencia_ecc.

    r_if_integracao_carregar = me.

*---------------------------------------
*-- romaneio
*---------------------------------------
    SELECT id_ordem fat_contingencia_ecc
      FROM zsdt0001
      INTO (l_id_ordem ,l_fat_contingencia_ecc)
        UP TO 1 ROWS
     WHERE ch_referencia = i_ch_referencia.
    ENDSELECT.

    CHECK sy-subrc = 0 AND l_fat_contingencia_ecc = abap_false AND l_id_ordem IS NOT INITIAL. "#150020-27.08.2024-JT

*---------------------------------------
*-- valida se troca nota
*---------------------------------------
    CHECK zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
                EXPORTING i_ch_referencia = i_ch_referencia
             ).

*---------------------------------------
*-- romaneio
*---------------------------------------
    SELECT viagem_id
      FROM zlest0185
      INTO @DATA(l_viagem_id)
        UP TO 1 ROWS
     WHERE id_ordem = @l_id_ordem.
    ENDSELECT.

    CHECK sy-subrc = 0.

*---------------------------------------
*-- status
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

*---------------------------------------
*-- Verificar se Ainda está Carregado
*---------------------------------------
    TRY.
        zcl_integracao_viagem_status=>zif_integracao_viagem_status~get_instance(
          )->set_viagem_status(
               EXPORTING
                 i_viagem_id = l_viagem_id
               IMPORTING
                 e_status    = e_status
          ).

      CATCH zcx_integracao.
        e_erro     = abap_true.
        e_msg_erro = 'Não foi possível obter Status do Carguero!'.
        RETURN.
      CATCH zcx_error.
        e_erro     = abap_true.
        e_msg_erro = 'Não foi possível obter Status do Carguero!'.
        RETURN.
    ENDTRY.

*----------------------------------------
*-- Status do Carguero
*----------------------------------------
    CASE e_status.

      WHEN '08'.
*----------------------------------------
*------ Envia aprovacao troca nota carguero
*----------------------------------------
*        TRY.
*            me->zif_integracao_trocant_aprovar~get_json( EXPORTING i_ch_referencia = i_ch_referencia
*                                                         IMPORTING e_json          = DATA(l_json_tn)
*                  )->set_ds_data(  EXPORTING i_json          = l_json_tn
*                  )->set_ds_url(
*                  )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao
*                  ).
*
*          CATCH zcx_integracao.
*            e_erro     = abap_true.
*            e_msg_erro = 'Não foi possível enviar Aprovação ao Carguero!'.
*            RETURN.
*          CATCH zcx_error.
*            e_erro     = abap_true.
*            e_msg_erro = 'Não foi possível enviar Aprovação ao Carguero!'.
*            RETURN.
*        ENDTRY.

      WHEN '15'.
*----------------------------------------
*------ valida doc transito
*----------------------------------------
        IF zif_integracao_trocant_aprovar~valida_chave_nfe(
                 EXPORTING i_ch_referencia = i_ch_referencia ) = abap_true.
          e_erro     = abap_true.
          e_msg_erro = 'Nota Fiscal do Embarcador foi alterada. Cancelar Viagem no Carguero'.
          RETURN.
        ENDIF.

      WHEN OTHERS.
        e_erro     = abap_true.
        e_msg_erro = 'Viagem Carguero deve estar como Liberação do Carregamento ou A.Doctos de Trânsito'.
        RETURN.

    ENDCASE.

*----------------------------------------
*-- atualiza id integracao
*----------------------------------------
*   IF e_id_integracao IS NOT INITIAL.
*     UPDATE zlest0185 SET id_integracao_carregar = e_id_integracao
*                          chave_nfe_embarcador   = me->zif_integracao_trocant_aprovar~chave_nfe_embarcador
*                    WHERE viagem_id              = l_viagem_id.
*   ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_trocant_aprovar~valida_remocao_docs_carguero.

    FREE: e_erro,
          e_msg_erro.

    r_if_integracao_carregar = me.

*---------------------------------------
*-- romaneio
*---------------------------------------
    SELECT id_ordem
      FROM zsdt0001
      INTO @DATA(l_id_ordem)
        UP TO 1 ROWS
     WHERE ch_referencia = @i_ch_referencia.
    ENDSELECT.

    CHECK ( sy-subrc = 0 ) AND ( l_id_ordem IS NOT INITIAL ).

*---------------------------------------
*-- valida se troca nota
*---------------------------------------
    CHECK zcl_faturamento=>zif_faturamento~get_romaneio_trocanota(
                EXPORTING i_ch_referencia = i_ch_referencia
             ).

*---------------------------------------
*-- romaneio
*---------------------------------------
    SELECT viagem_id
      FROM zlest0185
      INTO @DATA(l_viagem_id)
        UP TO 1 ROWS
     WHERE id_ordem = @l_id_ordem.
    ENDSELECT.

    CHECK ( sy-subrc = 0 ) AND ( l_viagem_id IS NOT INITIAL ).

*---------------------------------------
*-- status
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

*---------------------------------------
*-- Verificar se Ainda está Carregado
*---------------------------------------
    TRY.
        zcl_integracao_viagem_status=>zif_integracao_viagem_status~get_instance(
          )->set_viagem_status(
               EXPORTING
                 i_viagem_id = l_viagem_id
               IMPORTING
                 e_status    = DATA(e_status)
          ).

      CATCH zcx_integracao.
        e_erro     = abap_true.
        e_msg_erro = 'Não foi possível obter Status do Carguero!'.
        RETURN.
      CATCH zcx_error.
        e_erro     = abap_true.
        e_msg_erro = 'Não foi possível obter Status do Carguero!'.
        RETURN.
    ENDTRY.

*----------------------------------------
*-- Status do Carguero
*----------------------------------------
    CASE e_status.
      WHEN '05' OR '14' OR '15'.

      WHEN OTHERS.
        e_erro     = abap_true.
*       e_msg_erro = 'Viagem Carguero deve estar com status Aguardando Doctos de Trânsito'.
        e_msg_erro = 'Favor entrar em contato com a transportadora/logística.OV'.
        RETURN.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
