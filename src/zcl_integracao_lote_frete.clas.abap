CLASS ZCL_INTEGRACAO_LOTE_FRETE DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES ZIF_INTEGRACAO_INJECT .
    INTERFACES ZIF_INTEGRACAO_LOTE_FRETE .

    METHODS CONSTRUCTOR
      RAISING
        ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ALIASES SET_NEW_LOTE_FRETE_OV
      FOR ZIF_INTEGRACAO_LOTE_FRETE~SET_NEW_LOTE_FRETE_OV .
ENDCLASS.



CLASS ZCL_INTEGRACAO_LOTE_FRETE IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE    = ZIF_INTEGRACAO=>AT_ID_INTERFACE_LOTE_FRETE.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO   = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_OUTBOUND.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL        = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_OPUS  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_OPUS_SIM.
    ME->ZIF_INTEGRACAO_INJECT~AT_SEND_AUTENTICAO = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_SEND_SIM.


    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @ZIF_INTEGRACAO=>AT_ID_INTERFACE_LOTE_FRETE.

    IF SY-SUBRC EQ 0.
      me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    ENDIF.

    ME->ZIF_INTEGRACAO_LOTE_FRETE~SET_DS_URL( ).
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.

    SELECT SINGLE * INTO @DATA(WA_LOTE)
      FROM ZLEST0181
     WHERE ID_LOTE_FRETE EQ @I_INTEGRACAO-ID_REFERENCIA.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGNO
                            ATTR1 = CONV #( I_INTEGRACAO-ID_REFERENCIA ) )
          MSGID  = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_INTEGRACAO-ID_REFERENCIA ).
    ENDIF.

    TRY .
        CAST ZCL_INTEGRACAO_TOKEN(
               ZCL_INTEGRACAO_TOKEN=>ZIF_INTEGRACAO_TOKEN~GET_INSTANCE(
                 )->SET_EMPRESA_TOKEN( EXPORTING I_BUKRS = WA_LOTE-BUKRS
                 )
             )->ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP(
          IMPORTING
            E_HEADER_FIELDS = DATA(E_HEADER_FIELDS) ).

        ME->ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP( I_HEADER_FIELDS = E_HEADER_FIELDS ).

      CATCH ZCX_ERROR INTO DATA(EX_ERRO).

        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_ERRO->ZIF_ERROR~MSGID
                              MSGNO = EX_ERRO->ZIF_ERROR~MSGNO
                              ATTR1 = CONV #( EX_ERRO->ZIF_ERROR~MSGV1 )
                              ATTR2 = CONV #( EX_ERRO->ZIF_ERROR~MSGV2 )
                              ATTR3 = CONV #( EX_ERRO->ZIF_ERROR~MSGV3 )
                              ATTR4 = CONV #( EX_ERRO->ZIF_ERROR~MSGV4 ) )
            MSGID  = EX_ERRO->ZIF_ERROR~MSGID
            MSGNO  = EX_ERRO->ZIF_ERROR~MSGNO
            MSGTY  = 'E'
            MSGV1  = EX_ERRO->ZIF_ERROR~MSGV1
            MSGV2  = EX_ERRO->ZIF_ERROR~MSGV2
            MSGV3  = EX_ERRO->ZIF_ERROR~MSGV3
            MSGV4  = EX_ERRO->ZIF_ERROR~MSGV4.

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    DATA: RETORNO_CRIAR TYPE ZDE_CARGUERO_LOTE_CRIAR_RET.

    "Metodo para Integrar de retorno
    R_IF_INTEGRACAO_INJECT = ME.

    E_SUCESSO = ABAP_FALSE.

    """"""""""""""""""""""""""""""""""""""""""""""""""""

    CASE I_MSG_COMPLETA-DS_FUNCAO_PROCESSA.
      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_CANCELAR.

      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_FINALIZAR.

      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_VOLUME.

      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_CRIAR.

        CAST ZCL_INTEGRACAO_LOTE_FRETE( ME )->ZIF_INTEGRACAO_LOTE_FRETE~SET_REGISTRO( I_ID_LOTE_FRETE = CONV #( I_MSG_COMPLETA-ID_REFERENCIA ) ).

        "JSON -
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = RETORNO_CRIAR ).
        CAST ZCL_INTEGRACAO_LOTE_FRETE( ME )->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO = RETORNO_CRIAR-ID.

        IF CAST ZCL_INTEGRACAO_LOTE_FRETE( ME )->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO IS INITIAL.
          E_SUCESSO = ABAP_TRUE.
          EXIT.
        ENDIF.

        DATA(WA_ZLEST0181) = CAST ZCL_INTEGRACAO_LOTE_FRETE( ME )->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE.
        MODIFY ZLEST0181 FROM WA_ZLEST0181.
        COMMIT WORK AND WAIT.

        IF CAST ZCL_INTEGRACAO_LOTE_FRETE( ME )->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO  IS NOT INITIAL AND
           CAST ZCL_INTEGRACAO_LOTE_FRETE( ME )->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_CANCELADO EQ ABAP_TRUE.
          CAST ZCL_INTEGRACAO_LOTE_FRETE( ME )->ZIF_INTEGRACAO_LOTE_FRETE~SET_DELETE_LOTE_FRETE( ).
        ENDIF.

    ENDCASE.

    """"""""""""""""""""""""""""""""""""""""""""""""""""

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    DATA: RETORNO_CRIAR TYPE ZDE_CARGUERO_LOTE_CRIAR_RET.

    "Metodo para processoamento de retorno

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_FALSE.
    """"""""""""""""""""""""""""""""""""""""""""""""""""

    CASE I_MSG_COMPLETA-DS_FUNCAO_PROCESSA.
      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_CANCELAR.

      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_FINALIZAR.

      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_VOLUME.

      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_CRIAR.
        "JSON -
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = RETORNO_CRIAR ).
        CAST ZCL_INTEGRACAO_LOTE_FRETE( ME )->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO = RETORNO_CRIAR-ID.
    ENDCASE.

    "if RETORNO_CRIAR

    """"""""""""""""""""""""""""""""""""""""""""""""""""
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~get_id_material_carguero.

    DATA: lva_matnr_text TYPE mara-matnr.
    DATA: lva_text TYPE char255.

    r_if_integracao_lote_frete = me.

* "// #US-170726 WBARBOSA 05/05/2025
    IF i_material IS NOT INITIAL.
      SELECT SINGLE *
        INTO @DATA(wa_zlest0183)
        FROM zlest0183
       WHERE bukrs      EQ @i_bukrs
         AND matnr      EQ @i_material
         AND tp_produto EQ @i_tp_material.
    ELSEIF i_matkl IS NOT INITIAL.
      SELECT SINGLE *
          INTO @wa_zlest0183
          FROM zlest0183
         WHERE bukrs      EQ @i_bukrs
           AND matkl      EQ @i_matkl.
    ENDIF.
* "// #US-170726 WBARBOSA 05/05/2025

    e_id_material_carguero = wa_zlest0183-id_mt_carguero.

*---CS2020001303 - 10.11.2021 - JT - inicio
*   CHECK SY-SUBRC IS NOT INITIAL.
    IF sy-subrc <> 0.
      IF sy-tcode(2) EQ 'ME' OR sy-tcode = 'ZSDT0191' OR sy-tcode = 'ZSDT0081'.

        IF i_tp_material IS INITIAL.
          DATA(_tp_material) = 'Vazio'.
        ELSE.
          _tp_material = i_tp_material.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = i_material
          IMPORTING
            output = lva_matnr_text.
*"// #US-170726 WBARBOSA 05/05/2025
        IF i_material IS NOT INITIAL.
          lva_text = |Material: { lva_matnr_text }|.
        ENDIF.

        CONCATENATE 'Sem DE-PARA Empresa:' i_bukrs
                    lva_text
                    'Tp.Produto':  _tp_material INTO DATA(lva_msg_erro) SEPARATED BY space.
        "// #US-170726 WBARBOSA 05/05/2025
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = lva_msg_erro ).

      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
*---CS2020001303 - 10.11.2021 - JT - fim

    RAISE EXCEPTION TYPE zcx_integracao
      EXPORTING
        textid = VALUE #( msgid = zcx_integracao=>zcx_depara_material-msgid
                          msgno = zcx_integracao=>zcx_depara_material-msgno
                          attr1 = i_bukrs
                          attr2 = i_material
                          attr3 = i_tp_material
                           )
        msgid  = zcx_integracao=>zcx_depara_material-msgid
        msgno  = zcx_integracao=>zcx_depara_material-msgno
        msgty  = 'E'
        msgv1  = CONV #( i_bukrs )
        msgv2  = CONV #( i_material )
        msgv3  = CONV #( i_tp_material ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~GET_ID_REFERENCIA.

    RIF_INTEGRACAO_LOTE_FRETE  = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_LOTE_FRETE'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_LOTE_FRETE.

*    IF ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-VBELN IS NOT INITIAL.
*    ELSEIF ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-EBELN IS NOT INITIAL.
*      E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-EBELN && ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-EBELP.
*    ELSE.
*      E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_LOTE_FRETE.
*    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~GET_ID_UNI_ORG_CARGUERO.

    R_IF_INTEGRACAO_LOTE_FRETE = ME.

    CLEAR: E_ID_UNG_CARGUERO.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0197)
      FROM ZLEST0197
     WHERE BUKRS  EQ @I_BUKRS
       AND BRANCH EQ @I_BRANCH.

    E_ID_UNG_CARGUERO = WA_ZLEST0197-ID_UNG_CARGUERO.

    CHECK E_ID_UNG_CARGUERO IS INITIAL.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0184)
      FROM ZLEST0184
     WHERE BUKRS EQ @I_BUKRS
       AND KVGR4 EQ @I_UNIDADE_MAGGI.

    E_ID_UNG_CARGUERO = WA_ZLEST0184-ID_UNG_CARGUERO.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_INTEGRACAO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_DEPARA_UNDGOVERNACIONAL-MSGID
                          MSGNO = ZCX_INTEGRACAO=>ZCX_DEPARA_UNDGOVERNACIONAL-MSGNO
                          ATTR1 = I_BUKRS
                          ATTR2 = I_UNIDADE_MAGGI  )
        MSGID  = ZCX_INTEGRACAO=>ZCX_DEPARA_UNDGOVERNACIONAL-MSGID
        MSGNO  = ZCX_INTEGRACAO=>ZCX_DEPARA_UNDGOVERNACIONAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_BUKRS )
        MSGV2  = CONV #( I_UNIDADE_MAGGI ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~GET_INSTANCE.

    IF ZIF_INTEGRACAO_LOTE_FRETE~AT_IF_INTEGRACAO_LOTE_FRETE IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_LOTE_FRETE~AT_IF_INTEGRACAO_LOTE_FRETE TYPE ZCL_INTEGRACAO_LOTE_FRETE.
    ENDIF.
    R_IF_INTEGRACAO_LOTE_FRETE = ZIF_INTEGRACAO_LOTE_FRETE~AT_IF_INTEGRACAO_LOTE_FRETE.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~get_json.

    DATA: lc_msg_inclusao      TYPE zde_carguero_lote_criar,
          lc_msg_volume        TYPE zde_carguero_volume_carga,
          e_endereco           TYPE addr1_val,
          e_mail               TYPE ad_smtpadr,
          lc_transportador     TYPE zde_carguero_transportador,
          lv_volume_quantidade TYPE string.

    r_if_integracao_lote_frete = me.

    CASE i_tipo_json.
      WHEN zif_integracao_lote_frete=>at_operacao_tela_i.

*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
        IF     me->zif_integracao_lote_frete~at_lote_frete-vbeln IS NOT INITIAL.
          lc_msg_inclusao-referencia_integracao  =
*-CS2020001303 - 10.11.2021 - JT - inicio
               me->zif_integracao_lote_frete~at_lote_frete-branch && '-' &&
               me->zif_integracao_lote_frete~at_lote_frete-vbeln  && '-' &&
               me->zif_integracao_lote_frete~at_lote_frete-posnr.
*                           zcl_string=>concat( s1 = CONV #( me->zif_integracao_lote_frete~at_lote_frete-branch )
*                                               s2 = CONV #( me->zif_integracao_lote_frete~at_lote_frete-vbeln )
*                                               sp = '-' ).
*-CS2020001303 - 10.11.2021 - JT - fim
        ELSEIF me->zif_integracao_lote_frete~at_lote_frete-ebeln IS NOT INITIAL.
          lc_msg_inclusao-referencia_integracao  =
*-CS2020001303 - 12.11.2021 - JT - inicio
               me->zif_integracao_lote_frete~at_lote_frete-branch && '-' &&
               me->zif_integracao_lote_frete~at_lote_frete-ebeln  && '-' &&
               me->zif_integracao_lote_frete~at_lote_frete-ebelp.
*                           zcl_string=>concat( s1 = CONV #( me->zif_integracao_lote_frete~at_lote_frete-branch )
*                                               s2 = CONV #( me->zif_integracao_lote_frete~at_lote_frete-ebeln )
*                                               sp = '-' ).
*-CS2020001303 - 12.11.2021 - JT - fim

*-CS2025000249-26.03.2025-#170726-JT-inicio
        ELSEIF me->zif_integracao_lote_frete~at_lote_frete-nro_sol IS NOT INITIAL.
          lc_msg_inclusao-referencia_integracao  = 'SOL_REC_'     &&
               me->zif_integracao_lote_frete~at_lote_frete-branch && '_' &&
               me->zif_integracao_lote_frete~at_lote_frete-nro_sol.

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        ELSEIF me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in IS NOT INITIAL.
          lc_msg_inclusao-referencia_integracao  = 'CARGA_IN_'     &&
               me->zif_integracao_lote_frete~at_lote_frete-branch && '_' &&
               me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

        ENDIF.
*-CS2025000249-26.03.2025-#170726-JT-fim
*---CS2020000704 - JTASSONI - 01.10.2020 - fim

        "Se não tem fornecedor de frete a Unidade Organizacional
        " --> Filial Embarcadora
        "Se tem fornecedor de frete a Unidade Organizacional
        " --> Fornecedor de Frete Intercompany  - Filial Transportadora como Local de Negócio
        " --> Fornecedor é Local de Negócio     - Filial Embarcadora
        " --> Fornecedor não é Local de Negócio - Filial Embarcadora

        "DEFAULT = Filial Embarcadora
        " Fornecedor não é Local de Negócio - Filial Embarcadora
        " Fornecedor é Local de Negócio     - Filial Embarcadora
*        LC_MSG_INCLUSAO-UNIDADE_ORGANIZACIONAL = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS && ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BRANCH.
*
*        IF NOT ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_PARCEIRO_FRETE IS INITIAL.
*          CASE ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-TP_PARCEIRO_FRETE.
*            WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_TP_PARCEIRO_FORNECEDOR.
*
*              DATA(LC_TRANS_FOR) = ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
*                                     )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_PARCEIRO_FRETE
*                                     ).
*              TRY .
*                  LC_TRANS_FOR->CK_PARCEIRO_LOCAL_NEGOCIO( IMPORTING E_J_1BBRANCH  = DATA(E_J_1BBRANCH)
*                     )->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS ).
*                  "Fornecedor de Frete Intercompany  - Filial Transportadora como Local de Negócio
*                  LC_MSG_INCLUSAO-UNIDADE_ORGANIZACIONAL = E_J_1BBRANCH-BUKRS && E_J_1BBRANCH-BRANCH.
*                CATCH ZCX_PARCEIROS.
*              ENDTRY.
*            WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_TP_PARCEIRO_CLIENTE.
*              DATA(LC_TRANS_CLI) = ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE( )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_PARCEIRO_FRETE ).
*              TRY .
*                  LC_TRANS_CLI->CK_PARCEIRO_LOCAL_NEGOCIO( IMPORTING E_J_1BBRANCH  = E_J_1BBRANCH
*                    )->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS ).
*                  "Fornecedor de Frete Intercompany  - Filial Transportadora como Local de Negócio
*                  LC_MSG_INCLUSAO-UNIDADE_ORGANIZACIONAL = E_J_1BBRANCH-BUKRS && E_J_1BBRANCH-BRANCH.
*                CATCH ZCX_PARCEIROS.
*              ENDTRY.
*          ENDCASE.
*        ENDIF.

        me->zif_integracao_lote_frete~get_id_uni_org_carguero(
          EXPORTING
            i_bukrs = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
            i_branch = CONV #( me->zif_integracao_lote_frete~at_lote_frete-branch )
            i_unidade_maggi   = CONV #( me->zif_integracao_lote_frete~at_lote_frete-kvgr4 )
          IMPORTING
            e_id_ung_carguero = DATA(e_id_ung_carguero) ).

        lc_msg_inclusao-unidade_organizacional = e_id_ung_carguero.

        "Remetente """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        lc_msg_inclusao-remetente-referencia_integracao = me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme && me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme.
        CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme.
          WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
            DATA(wa_reme_for) = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
                                       )->get_email( IMPORTING e_mail = e_mail
                                       )->get_endereco( IMPORTING e_endereco = e_endereco
                                       ) )->at_lfa1.
            lc_msg_inclusao-remetente-cpf_cnpj           = COND string( WHEN wa_reme_for-stkzn EQ abap_true THEN wa_reme_for-stcd2 ELSE wa_reme_for-stcd1 ).
            lc_msg_inclusao-remetente-tipo_parceiro      = COND string( WHEN wa_reme_for-stkzn EQ abap_true THEN 'F' ELSE 'J' ).
            IF wa_reme_for-gbdat IS NOT INITIAL.
              lc_msg_inclusao-remetente-data_nascimento    = wa_reme_for-gbdat+0(4) && '-' && wa_reme_for-gbdat+4(2) && '-' && wa_reme_for-gbdat+6(2).
            ENDIF.
            lc_msg_inclusao-remetente-nome_razao_social  = wa_reme_for-name1.
            lc_msg_inclusao-remetente-nome_fantasia      = wa_reme_for-name1.
            lc_msg_inclusao-remetente-inscricao_estadual = wa_reme_for-stcd3.
            lc_msg_inclusao-remetente-telefone_celular   = wa_reme_for-telf2.
          WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
            DATA(wa_reme_cli) = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
                                       )->get_email( IMPORTING e_mail = e_mail
                                       )->get_endereco( IMPORTING e_endereco = e_endereco
                                       ) )->at_kna1.
            lc_msg_inclusao-remetente-cpf_cnpj           = COND string( WHEN wa_reme_cli-stkzn EQ abap_true THEN wa_reme_cli-stcd2 ELSE wa_reme_cli-stcd1 ).
            lc_msg_inclusao-remetente-tipo_parceiro      = COND string( WHEN wa_reme_cli-stkzn EQ abap_true THEN 'F' ELSE 'J' ).
            lc_msg_inclusao-remetente-nome_razao_social  = wa_reme_cli-name1.
            lc_msg_inclusao-remetente-nome_fantasia      = wa_reme_cli-name1.
            lc_msg_inclusao-remetente-inscricao_estadual = wa_reme_cli-stcd3.
            lc_msg_inclusao-remetente-telefone_celular   = wa_reme_cli-telf2.
        ENDCASE.
        lc_msg_inclusao-remetente-email     = e_mail.
        lc_msg_inclusao-remetente-telefone  = e_endereco-tel_number.
        "Remerente Endereço
        lc_msg_inclusao-remetente-endereco-referencia_integracao = lc_msg_inclusao-remetente-referencia_integracao.
        lc_msg_inclusao-remetente-endereco-inscricao_estadual    = lc_msg_inclusao-remetente-inscricao_estadual.
        lc_msg_inclusao-remetente-endereco-cep                   = e_endereco-post_code1.

        REPLACE ALL OCCURRENCES OF REGEX '-' IN lc_msg_inclusao-remetente-endereco-cep WITH ' '.
        CONDENSE lc_msg_inclusao-remetente-endereco-cep NO-GAPS.

        lc_msg_inclusao-remetente-endereco-uf                    = e_endereco-taxjurcode(2).
        lc_msg_inclusao-remetente-endereco-codigo_municipio_ibge = e_endereco-taxjurcode+3(7).
        lc_msg_inclusao-remetente-endereco-logradouro            = e_endereco-street.
        lc_msg_inclusao-remetente-endereco-numero                = COND string( WHEN e_endereco-house_num1 IS INITIAL THEN 'S/N' ELSE e_endereco-house_num1 ).
        lc_msg_inclusao-remetente-endereco-bairro                = e_endereco-city2.
        lc_msg_inclusao-remetente-endereco-complemento           = ''.
        lc_msg_inclusao-remetente-endereco-ponto_referencia      = ''.
        CLEAR: e_mail, e_endereco.

        "Destinatário """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        lc_msg_inclusao-destinatario-referencia_integracao = me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_dest && me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest.
        lc_msg_inclusao-destinatario-exportacao = COND string( WHEN me->zif_integracao_lote_frete~at_lote_frete-ck_exportacao EQ abap_true THEN 'True' ELSE 'False' ).
        CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_dest.
          WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

            DATA(wa_dest_for) = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest
                                       )->get_email( IMPORTING e_mail = e_mail
                                       )->get_endereco( IMPORTING e_endereco = e_endereco
                                       ) )->at_lfa1.
            lc_msg_inclusao-destinatario-cpf_cnpj           = COND string( WHEN wa_dest_for-stkzn EQ abap_true THEN wa_dest_for-stcd2 ELSE wa_dest_for-stcd1 ).
            lc_msg_inclusao-destinatario-tipo_parceiro      = COND string( WHEN wa_dest_for-stkzn EQ abap_true THEN 'F' ELSE 'J' ).
            IF wa_dest_for-gbdat IS NOT INITIAL.
              lc_msg_inclusao-destinatario-data_nascimento    = wa_dest_for-gbdat+0(4) && '-' && wa_dest_for-gbdat+4(2) && '-' && wa_dest_for-gbdat+6(2).
            ENDIF.
            lc_msg_inclusao-destinatario-nome_razao_social  = wa_dest_for-name1.
            lc_msg_inclusao-destinatario-nome_fantasia      = wa_dest_for-name1.
            lc_msg_inclusao-destinatario-inscricao_estadual = wa_dest_for-stcd3.
            lc_msg_inclusao-destinatario-telefone_celular   = wa_dest_for-telf2.

          WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.

            DATA(wa_dest_cli) = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest
                                       )->get_email( IMPORTING e_mail = e_mail
                                       )->get_endereco( IMPORTING e_endereco = e_endereco
                                       ) )->at_kna1.
            lc_msg_inclusao-destinatario-cpf_cnpj           = COND string( WHEN wa_dest_cli-stkzn EQ abap_true THEN wa_dest_cli-stcd2 ELSE wa_dest_cli-stcd1 ).
            lc_msg_inclusao-destinatario-tipo_parceiro      = COND string( WHEN wa_dest_cli-stkzn EQ abap_true THEN 'F' ELSE 'J' ).
            lc_msg_inclusao-destinatario-nome_razao_social  = wa_dest_cli-name1.
            lc_msg_inclusao-destinatario-nome_fantasia      = wa_dest_cli-name1.
            lc_msg_inclusao-destinatario-inscricao_estadual = wa_dest_cli-stcd3.
            lc_msg_inclusao-destinatario-telefone_celular   = wa_dest_cli-telf2.

        ENDCASE.
        lc_msg_inclusao-destinatario-email     = e_mail.
        lc_msg_inclusao-destinatario-telefone  = e_endereco-tel_number.
        "Destinatário Endereço
        lc_msg_inclusao-destinatario-endereco-referencia_integracao = lc_msg_inclusao-destinatario-referencia_integracao.
        lc_msg_inclusao-destinatario-endereco-inscricao_estadual    = lc_msg_inclusao-destinatario-inscricao_estadual.
        lc_msg_inclusao-destinatario-endereco-cep                   = e_endereco-post_code1.

        REPLACE ALL OCCURRENCES OF REGEX '-' IN lc_msg_inclusao-destinatario-endereco-cep WITH ' '.
        CONDENSE lc_msg_inclusao-destinatario-endereco-cep NO-GAPS.

        lc_msg_inclusao-destinatario-endereco-uf                    = e_endereco-taxjurcode(2).
        lc_msg_inclusao-destinatario-endereco-codigo_municipio_ibge = e_endereco-taxjurcode+3(7).
        lc_msg_inclusao-destinatario-endereco-logradouro            = e_endereco-street.
        lc_msg_inclusao-destinatario-endereco-numero                = COND string( WHEN e_endereco-house_num1 IS INITIAL THEN 'S/N' ELSE e_endereco-house_num1 ).
        lc_msg_inclusao-destinatario-endereco-bairro                = e_endereco-city2.
        lc_msg_inclusao-destinatario-endereco-complemento           = ''.
        lc_msg_inclusao-destinatario-endereco-ponto_referencia      = ''.
        CLEAR: e_mail, e_endereco.

        "Local Coleta """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        lc_msg_inclusao-local_coleta-referencia_integracao = me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole && me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole.
        lc_msg_inclusao-local_coleta-parceiro-referencia_integracao = me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole && me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole.
        CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole.
          WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

            DATA(wa_cole_for) = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
                                       )->get_email( IMPORTING e_mail = e_mail
                                       )->get_endereco( IMPORTING e_endereco = e_endereco
                                       ) )->at_lfa1.

            lc_msg_inclusao-local_coleta-nome = wa_cole_for-name1.
            lc_msg_inclusao-local_coleta-parceiro-cpf_cnpj           = COND string( WHEN wa_cole_for-stkzn EQ abap_true THEN wa_cole_for-stcd2 ELSE wa_cole_for-stcd1 ).
            lc_msg_inclusao-local_coleta-parceiro-tipo_parceiro      = COND string( WHEN wa_cole_for-stkzn EQ abap_true THEN 'F' ELSE 'J' ).
            IF wa_cole_for-gbdat IS NOT INITIAL.
              lc_msg_inclusao-local_coleta-parceiro-data_nascimento    = wa_cole_for-gbdat+0(4) && '-' && wa_cole_for-gbdat+4(2) && '-' && wa_cole_for-gbdat+6(2).
            ENDIF.
            lc_msg_inclusao-local_coleta-parceiro-nome_razao_social  = wa_cole_for-name1.
            lc_msg_inclusao-local_coleta-parceiro-nome_fantasia      = wa_cole_for-name1.
            lc_msg_inclusao-local_coleta-parceiro-inscricao_estadual = wa_cole_for-stcd3.
            lc_msg_inclusao-local_coleta-parceiro-telefone_celular   = wa_cole_for-telf2.

          WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.

            DATA(wa_cole_cli) = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
                                       )->get_email( IMPORTING e_mail = e_mail
                                       )->get_endereco( IMPORTING e_endereco = e_endereco
                                       ) )->at_kna1.

            lc_msg_inclusao-local_coleta-nome                        = wa_cole_cli-name1.
            lc_msg_inclusao-local_coleta-parceiro-cpf_cnpj           = COND string( WHEN wa_cole_cli-stkzn EQ abap_true THEN wa_cole_cli-stcd2 ELSE wa_cole_cli-stcd1 ).
            lc_msg_inclusao-local_coleta-parceiro-tipo_parceiro      = COND string( WHEN wa_cole_cli-stkzn EQ abap_true THEN 'F' ELSE 'J' ).
            lc_msg_inclusao-local_coleta-parceiro-nome_razao_social  = wa_cole_cli-name1.
            lc_msg_inclusao-local_coleta-parceiro-nome_fantasia      = wa_cole_cli-name1.
            lc_msg_inclusao-local_coleta-parceiro-inscricao_estadual = wa_cole_cli-stcd3.
            lc_msg_inclusao-local_coleta-parceiro-telefone_celular   = wa_cole_cli-telf2.

        ENDCASE.
        lc_msg_inclusao-local_coleta-parceiro-email     = e_mail.
        lc_msg_inclusao-local_coleta-parceiro-telefone  = e_endereco-tel_number.
        "Destinatário Endereço
        lc_msg_inclusao-local_coleta-parceiro-endereco-referencia_integracao = lc_msg_inclusao-local_coleta-referencia_integracao.
        lc_msg_inclusao-local_coleta-parceiro-endereco-inscricao_estadual    = lc_msg_inclusao-local_coleta-parceiro-inscricao_estadual.
        lc_msg_inclusao-local_coleta-parceiro-endereco-cep                   = e_endereco-post_code1.

        REPLACE ALL OCCURRENCES OF REGEX '-' IN lc_msg_inclusao-local_coleta-parceiro-endereco-cep WITH ' '.
        CONDENSE lc_msg_inclusao-local_coleta-parceiro-endereco-cep NO-GAPS.

        lc_msg_inclusao-local_coleta-parceiro-endereco-uf                    = e_endereco-taxjurcode(2).
        lc_msg_inclusao-local_coleta-parceiro-endereco-codigo_municipio_ibge = e_endereco-taxjurcode+3(7).
        lc_msg_inclusao-local_coleta-parceiro-endereco-logradouro            = e_endereco-street.
        lc_msg_inclusao-local_coleta-parceiro-endereco-numero                = COND string( WHEN e_endereco-house_num1 IS INITIAL THEN 'S/N' ELSE e_endereco-house_num1 ).
        lc_msg_inclusao-local_coleta-parceiro-endereco-bairro                = e_endereco-city2.
        lc_msg_inclusao-local_coleta-parceiro-endereco-complemento           = ''.
        lc_msg_inclusao-local_coleta-parceiro-endereco-ponto_referencia      = ''.
        lc_msg_inclusao-local_coleta-endereco = lc_msg_inclusao-local_coleta-parceiro-endereco.
        CLEAR: e_mail, e_endereco.

        "Local Entrega """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        lc_msg_inclusao-local_descarga-referencia_integracao = me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr && me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr.
        lc_msg_inclusao-local_descarga-parceiro-referencia_integracao = me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr && me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr.
        CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr.
          WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

            DATA(wa_entr_for) = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr
                                       )->get_email( IMPORTING e_mail = e_mail
                                       )->get_endereco( IMPORTING e_endereco = e_endereco
                                       ) )->at_lfa1.

            lc_msg_inclusao-local_descarga-nome = wa_entr_for-name1.
            lc_msg_inclusao-local_descarga-parceiro-cpf_cnpj           = COND string( WHEN wa_entr_for-stkzn EQ abap_true THEN wa_entr_for-stcd2 ELSE wa_entr_for-stcd1 ).
            lc_msg_inclusao-local_descarga-parceiro-tipo_parceiro      = COND string( WHEN wa_entr_for-stkzn EQ abap_true THEN 'F' ELSE 'J' ).
            IF wa_entr_for-gbdat IS NOT INITIAL.
              lc_msg_inclusao-local_descarga-parceiro-data_nascimento    = wa_entr_for-gbdat+0(4) && '-' && wa_entr_for-gbdat+4(2) && '-' && wa_entr_for-gbdat+6(2).
            ENDIF.
            lc_msg_inclusao-local_descarga-parceiro-nome_razao_social  = wa_entr_for-name1.
            lc_msg_inclusao-local_descarga-parceiro-nome_fantasia      = wa_entr_for-name1.
            lc_msg_inclusao-local_descarga-parceiro-inscricao_estadual = COND string( WHEN wa_entr_for-stcd3 IS NOT INITIAL THEN wa_entr_for-stcd3 ELSE 'ISENTO' ).
            lc_msg_inclusao-local_descarga-parceiro-telefone_celular   = wa_entr_for-telf2.

          WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.

            DATA(wa_entr_cli) = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr
                                       )->get_email( IMPORTING e_mail = e_mail
                                       )->get_endereco( IMPORTING e_endereco = e_endereco
                                       ) )->at_kna1.

            lc_msg_inclusao-local_descarga-nome = wa_entr_cli-name1.
            lc_msg_inclusao-local_descarga-parceiro-cpf_cnpj           = COND string( WHEN wa_entr_cli-stkzn EQ abap_true THEN wa_entr_cli-stcd2 ELSE wa_entr_cli-stcd1 ).
            lc_msg_inclusao-local_descarga-parceiro-tipo_parceiro      = COND string( WHEN wa_entr_cli-stkzn EQ abap_true THEN 'F' ELSE 'J' ).
            lc_msg_inclusao-local_descarga-parceiro-nome_razao_social  = wa_entr_cli-name1.
            lc_msg_inclusao-local_descarga-parceiro-nome_fantasia      = wa_entr_cli-name1.
            lc_msg_inclusao-local_descarga-parceiro-inscricao_estadual = COND string( WHEN wa_entr_cli-stcd3 IS NOT INITIAL THEN wa_entr_cli-stcd3 ELSE 'ISENTO' ).
            lc_msg_inclusao-local_descarga-parceiro-telefone_celular   = wa_entr_cli-telf2.

        ENDCASE.
        lc_msg_inclusao-local_descarga-parceiro-email     = e_mail.
        lc_msg_inclusao-local_descarga-parceiro-telefone  = e_endereco-tel_number.
        "Destinatário Endereço
        lc_msg_inclusao-local_descarga-parceiro-endereco-referencia_integracao = lc_msg_inclusao-local_descarga-referencia_integracao.
        lc_msg_inclusao-local_descarga-parceiro-endereco-inscricao_estadual    = lc_msg_inclusao-local_descarga-parceiro-inscricao_estadual.
        lc_msg_inclusao-local_descarga-parceiro-endereco-cep                   = e_endereco-post_code1.
        REPLACE ALL OCCURRENCES OF REGEX '-' IN lc_msg_inclusao-local_descarga-parceiro-endereco-cep WITH ' '.
        CONDENSE lc_msg_inclusao-local_descarga-parceiro-endereco-cep NO-GAPS.
        lc_msg_inclusao-local_descarga-parceiro-endereco-uf                    = e_endereco-taxjurcode(2).
        lc_msg_inclusao-local_descarga-parceiro-endereco-codigo_municipio_ibge = e_endereco-taxjurcode+3(7).
        lc_msg_inclusao-local_descarga-parceiro-endereco-logradouro            = e_endereco-street.
        lc_msg_inclusao-local_descarga-parceiro-endereco-numero                = COND string( WHEN e_endereco-house_num1 IS INITIAL THEN 'S/N' ELSE e_endereco-house_num1 ).
        lc_msg_inclusao-local_descarga-parceiro-endereco-bairro                = e_endereco-city2.
        lc_msg_inclusao-local_descarga-parceiro-endereco-complemento           = ''.
        lc_msg_inclusao-local_descarga-parceiro-endereco-ponto_referencia      = ''.
        lc_msg_inclusao-local_descarga-endereco = lc_msg_inclusao-local_descarga-parceiro-endereco.
        CLEAR: e_mail, e_endereco.

        lc_msg_inclusao-volume-quantidade     = me->zif_integracao_lote_frete~at_lote_frete-brgew.

        "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
        "CONDENSE lc_msg_inclusao-volume-quantidade NO-GAPS.
        "REPLACE ALL OCCURRENCES OF '.' IN lc_msg_inclusao-volume-quantidade WITH ','.
        "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676

        lc_msg_inclusao-volume-unidade_medida = COND string( WHEN me->zif_integracao_lote_frete~at_lote_frete-gewei = 'KG' THEN 'kg'
                                                             WHEN me->zif_integracao_lote_frete~at_lote_frete-gewei = 'TO' THEN 't' ).

        "Transportadores """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete IS NOT INITIAL.
          CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_frete.
            WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

              DATA(at_lfa1) = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance(
                                       )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete
                                       ) )->at_lfa1.

              CLEAR: lc_transportador.
              lc_transportador-cpf_cnpj           = at_lfa1-stcd1.

              "Início - DEVK9A1S6P - #124587 RSA
              lv_volume_quantidade = lc_msg_inclusao-volume-quantidade.
              TRANSLATE lv_volume_quantidade USING ',.'.
              CONDENSE lv_volume_quantidade NO-GAPS.
              lc_transportador-quantidade_produto = lv_volume_quantidade.
              "lc_transportador-quantidade_produto = lc_msg_inclusao-volume-quantidade.
              "CONDENSE lc_transportador-quantidade_produto NO-GAPS.
              "Fim - DEVK9A1S6P - #124587 RSA

              lc_transportador-liberada = 'False'.
              lc_transportador-valor_frete-modalidade_pagamento_frete  = '1'. "valorPorTonelada - valorPorViagem
              lc_transportador-valor_frete-valor_frete_trecho_fiscal   = '0.00'.
*-CS2019001158 - Jaime Tassoni - 12.11.2020 - inicio
              IF me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_true.
                lc_transportador-valor_frete-valor_frete_trecho_terceiro = '0.00'.
              ELSE.
                CLEAR: lc_transportador-valor_frete-valor_frete_trecho_terceiro.
              ENDIF.
*-CS2019001158 - Jaime Tassoni - 12.11.2020 - fim

            WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
            WHEN OTHERS.
          ENDCASE.
          APPEND lc_transportador TO lc_msg_inclusao-transportadores.
        ENDIF.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        lc_msg_inclusao-fob     = COND string( WHEN me->zif_integracao_lote_frete~at_lote_frete-tp_frete EQ zif_carga=>st_tp_frete_fob THEN 'True' ELSE 'False' ).
        lc_msg_inclusao-produto = me->zif_integracao_lote_frete~at_lote_frete-matnr.

        me->zif_integracao_lote_frete~get_id_material_carguero(
          EXPORTING
            i_bukrs       = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
            i_material    = CONV #( me->zif_integracao_lote_frete~at_lote_frete-matnr ) "WPP 16.05.2025  Integração SOlicitação Recebimento
            i_matkl       = CONV #( me->zif_integracao_lote_frete~at_lote_frete-matkl ) " WPP 16.05.2025  Integração SOlicitação Recebimento
            i_tp_material = CONV #( me->zif_integracao_lote_frete~at_lote_frete-tp_produto )
          IMPORTING
            e_id_material_carguero  = DATA(e_id_material_carguero) ).

        lc_msg_inclusao-produto = e_id_material_carguero.
        lc_msg_inclusao-embarque_terceiro          = COND string( WHEN me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro EQ abap_true THEN 'True' ELSE 'False' ).
        lc_msg_inclusao-emitir_nf_transito         = COND string( WHEN me->zif_integracao_lote_frete~at_lote_frete-ck_emite_nf_transito EQ abap_true THEN 'True' ELSE 'False' ).

        "Não existe por enquanto
        lc_msg_inclusao-troca_nota                 = COND string( WHEN me->zif_integracao_lote_frete~at_lote_frete-ck_troca_nota           EQ abap_true THEN 'True' ELSE 'False' ).
        lc_msg_inclusao-paga_frete_trecho_terceiro = COND string( WHEN me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro EQ abap_true THEN 'True' ELSE 'False' ).

        TRY .
            lc_msg_inclusao-observacoes = zcl_ordem_venda=>zif_ordem_venda~get_text_cab_formulario( i_vbeln = me->zif_integracao_lote_frete~at_lote_frete-vbeln ).
          CATCH zcx_ordem_venda.
          CATCH zcx_error.
        ENDTRY.

        IF lc_msg_inclusao-observacoes IS INITIAL.
          lc_msg_inclusao-observacoes = 'Deve ser definida uma mensagem fixa para os Lotes de Frete'.
        ENDIF.

        "Douglas Lime e Lucas Barros solicitarm para que quando for Frete de Entrada enviar tudo "Falso"
        IF me->zif_integracao_lote_frete~at_lote_frete-kvgr5 EQ '002'.
          lc_msg_inclusao-embarque_terceiro = 'False'.
          lc_msg_inclusao-emitir_nf_transito = 'False'.
          lc_msg_inclusao-troca_nota = 'False'.
          lc_msg_inclusao-fob = 'False'.
        ENDIF.

*-CS2025000249-26.03.2025-#170726-JT-inicio
        IF me->zif_integracao_lote_frete~at_lote_frete-nro_sol IS NOT INITIAL.

          IF me->zif_integracao_lote_frete~at_lote_frete-ck_troca_nota = abap_true.
            lc_msg_inclusao-troca_nota  = 'True'.
          ENDIF.

          lc_msg_inclusao-observacoes = zcl_solicitacao_entrada_insumo=>get_observacoes_lote_carguero( i_nro_sol =  me->zif_integracao_lote_frete~at_lote_frete-nro_sol ).
        ENDIF.
*-CS2025000249-26.03.2025-#170726-JT-fim

       "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
       IF me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in IS NOT INITIAL.
          lc_msg_inclusao-observacoes = zcl_carga_saida_insumos=>get_observacoes_lote_carguero( i_nro_cg = me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in ).
        ENDIF.
       "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

        r_json = zcl_fmcall_base=>abap2json( abap_data = lc_msg_inclusao ).

      WHEN zif_integracao_lote_frete=>at_operacao_tela_u.

        lc_msg_volume-volume_carga-quantidade     = me->zif_integracao_lote_frete~at_lote_frete-brgew.
        lc_msg_volume-volume_carga-unidade_medida = COND string( WHEN me->zif_integracao_lote_frete~at_lote_frete-gewei = 'KG' THEN 'kg'
                                                                 WHEN me->zif_integracao_lote_frete~at_lote_frete-gewei = 'TO' THEN 't' ).
        r_json = zcl_fmcall_base=>abap2json( abap_data = lc_msg_volume ).

      WHEN zif_integracao_lote_frete=>at_operacao_tela_d.
        "Finalizar Lote
        r_json = '{ "motivo": "' && 'Ordem de Venda Cancelada' && '" }'.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~GET_NEW_ID_LOTE_FRETE.

    R_IF_INTEGRACAO_LOTE_FRETE = ME.

    CHECK E_ID_LOTE_FRETE IS INITIAL.

    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        OBJECT           = 'ZIDLTFRETE'
      EXCEPTIONS
        FOREIGN_LOCK     = 1
        OBJECT_NOT_FOUND = 2
        SYSTEM_FAILURE   = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCX_INTEGRACAO=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZIDLTFRETE'
        IGNORE_BUFFER           = 'X'
      IMPORTING
        NUMBER                  = E_ID_LOTE_FRETE
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCX_INTEGRACAO=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

* Desbloqueia o objeto de numeração
    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        OBJECT           = 'ZIDLTFRETE'
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCX_INTEGRACAO=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~SET_DELETE_LOTE_FRETE.

    R_IF_INTEGRACAO_LOTE_FRETE = ME.

    CHECK ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_LOTE_FRETE IS NOT INITIAL.

    IF ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_CANCELADO EQ ABAP_FALSE.
      ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_CANCELADO = ABAP_TRUE.
      ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-DT_CANCELA   = SY-DATUM.
      ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-HR_CANCELA   = SY-UZEIT.
      ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-US_CANCELA   = SY-UNAME.
      MODIFY ZLEST0181 FROM ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE.
      COMMIT WORK.
    ENDIF.

    CHECK ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO IS NOT INITIAL.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_LOTE_FRETE~GET_JSON( EXPORTING I_TIPO_JSON = ZIF_INTEGRACAO_LOTE_FRETE=>AT_OPERACAO_TELA_D IMPORTING R_JSON = DATA(LC_JSON)
      )->SET_DS_DATA( EXPORTING I_DATA = LC_JSON
      )->SET_DS_URL(
      )->SET_ID_REFERENCIA(
      )->SET_SEND_MSG(
      ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_LOTE_FRETE = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_DATA.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~SET_DS_URL.

    R_IF_INTEGRACAO_LOTE_FRETE = ME.

    SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_CALL_DIRECT_CARGUERO)
     WHERE NAME = 'CALL_CARGUERO_DIRECT'
       AND LOW  = @ZIF_INTEGRACAO=>AT_ID_INTERFACE_LOTE_FRETE.

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
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN        = LWA_ZAUTH_WEBSERVICE-URL_TOKEN.
      CASE ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_CANCELADO .
        WHEN ABAP_TRUE.
          ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'lotes-embarcadores/' && ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO && '/finalizar'.
          ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_CANCELAR.
          ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
        WHEN ABAP_FALSE.
          CASE ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_FINALIZADO.
            WHEN ABAP_TRUE.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'lotes-embarcadores/' && ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO && '/finalizar'.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_FINALIZAR.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
            WHEN ABAP_FALSE.
              IF ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO IS NOT INITIAL.
                ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'lotes-embarcadores/' && ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO && '/volume-carga'.
                ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_VOLUME.
                ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
              ELSE.
                ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = LWA_ZAUTH_WEBSERVICE-URL && 'lotes-embarcadores'.
                ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_CRIAR.
                ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'POST'.
              ENDIF.
          ENDCASE.
      ENDCASE.

      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

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
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN        = WA_WEBSERVICE-URL_TOKEN.
    CASE ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_CANCELADO .
      WHEN ABAP_TRUE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'lotes-embarcadores/' && ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO && '/finalizar'.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_CANCELAR.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'POST'.
      WHEN ABAP_FALSE.
        CASE ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_FINALIZADO.
          WHEN ABAP_TRUE.
            ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'lotes-embarcadores/' && ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO && '/finalizar'.
            ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_FINALIZAR.
            ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'POST'.
          WHEN ABAP_FALSE.
            IF ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO IS NOT INITIAL.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'lotes-embarcadores/' && ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_CARGUERO && '/volume-carga'.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_VOLUME.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'PUT'.
            ELSE.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = WA_WEBSERVICE-URL && 'lotes-embarcadores'.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA = ZIF_INTEGRACAO_LOTE_FRETE=>AT_FC_PROCESSAR_CRIAR.
              ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO = 'POST'.
            ENDIF.
        ENDCASE.
    ENDCASE.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_envia_fertilizantes_ov.

    DATA: w_ordem_venda TYPE zde_cargueiro_ov,
          wvbap         TYPE vbapvb,
          xvbpa         TYPE TABLE OF vbpavb,
          wvbpa         TYPE vbpavb,
          xvbkd         TYPE TABLE OF vbkdvb,
          wvbkd         TYPE vbkdvb.

    FREE: xvbpa, xvbkd, e_erro, e_erro_mesg.

*-----------------------------------------------
*---obtem cabecalho OV
*-----------------------------------------------
    SELECT SINGLE *
      FROM vbak
      INTO @DATA(w_vbak)
     WHERE vbeln = @i_vbeln.

    CHECK sy-subrc = 0.

*-----------------------------------------------
*---obtem patrceiros
*-----------------------------------------------
    SELECT *
      FROM vbpa
      INTO TABLE @DATA(t_vbpa)
     WHERE vbeln = @i_vbeln.

*-----------------------------------------------
*---obtem dados comerciais
*-----------------------------------------------
    SELECT *
      FROM vbkd
      INTO TABLE @DATA(t_vbkd)
     WHERE vbeln = @i_vbeln.

    LOOP AT t_vbpa             INTO DATA(w_vbpa).
      MOVE-CORRESPONDING w_vbpa  TO wvbpa.
      APPEND wvbpa               TO xvbpa.
    ENDLOOP.

    LOOP AT t_vbkd             INTO DATA(w_vbkd).
      MOVE-CORRESPONDING w_vbkd  TO wvbkd.
      APPEND wvbkd               TO xvbkd.
    ENDLOOP.

*-----------------------------------------------
*---Envio carguero
*-----------------------------------------------
    LOOP AT t_vbap            INTO DATA(w_vbap).

      MOVE-CORRESPONDING w_vbap TO wvbap.
      MOVE-CORRESPONDING w_vbak TO w_ordem_venda.

      w_ordem_venda-item         = wvbap.
      w_ordem_venda-parceiros    = xvbpa[].
      w_ordem_venda-comercial    = xvbkd[].

      TRY .
          zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_gerencia_lote(
              EXPORTING i_ordem_venda   = w_ordem_venda
              IMPORTING e_id_lote_frete = w_vbap-id_lote_frete ).

        CATCH zcx_integracao INTO DATA(ex_integracao).
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = ex_integracao->msgid
              msgnr               = ex_integracao->msgno
              msgv1               = ex_integracao->msgv1
              msgv2               = ex_integracao->msgv2
              msgv3               = ex_integracao->msgv3
              msgv4               = ex_integracao->msgv4
            IMPORTING
              message_text_output = e_erro_mesg.

          e_erro = abap_true.
          EXIT.

        CATCH zcx_error      INTO DATA(ex_error).
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = ex_error->msgid
              msgnr               = ex_error->msgno
              msgv1               = ex_error->msgv1
              msgv2               = ex_error->msgv2
              msgv3               = ex_error->msgv3
              msgv4               = ex_error->msgv4
            IMPORTING
              message_text_output = e_erro_mesg.

          e_erro = abap_true.
          EXIT.
      ENDTRY.

*-----------------------------------------------
*-----Atualiza ID LOTE
*-----------------------------------------------
      UPDATE vbap
         SET id_lote_frete = w_vbap-id_lote_frete
       WHERE vbeln         = w_vbap-vbeln
         AND posnr         = w_vbap-posnr.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_gerencia_lote.

    DATA: r_lote_frete TYPE REF TO zcl_integracao_lote_frete,
          lv_mesg      TYPE string.  "*-CS2025000249-26.03.2025-#170726-JT

    CREATE OBJECT r_lote_frete.

    ""WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
    IF i_sincronia IS NOT INITIAL.
      r_lote_frete->zif_integracao_inject~at_tp_sincronia = i_sincronia.
    ENDIF.
    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676

    r_if_integracao_lote_frete = r_lote_frete.

    r_lote_frete->zif_integracao_lote_frete~set_new_lote_frete(
      EXPORTING
        i_ordem_venda              = i_ordem_venda
        i_pedido_compra            = i_pedido_compra
        i_solicitacao              = i_solicitacao      "*-CS2025000249-26.03.2025-#170726-JT-inicio
        i_nro_cg_sai               = i_nro_cg_sai       "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
      IMPORTING
        e_id_lote_frete            = e_id_lote_frete    " Identificador de Lote de Frete
        e_lote_frete               = DATA(e_lote_frete)
    ).


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_LOTE_FRETE = ME.
    ME->ZIF_INTEGRACAO_LOTE_FRETE~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_LOTE_FRETE~SET_LIMPAR.

    R_IF_INTEGRACAO_LOTE_FRETE = ME.

    CLEAR: ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE,
           ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE_OLD.

    CLEAR: ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA,
           ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_new_lote_frete.

*-CS2025000249-26.03.2025-#170726-JT-inicio
*    DATA: lc_ordem_venda   TYPE zde_cargueiro_ov.
**---CS2020000704 - JTASSONI - 01.10.2020 - inicio
*    FIELD-SYMBOLS: <xekpo> TYPE ANY TABLE,
*                   <yekpo> TYPE ANY TABLE.
*    DATA: lc_pedido_compra TYPE zde_cargueiro_pc,
*          t_xekpo          TYPE TABLE OF ekpo,
*          t_yekpo          TYPE TABLE OF ekpo.
*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
    DATA: lv_mesg  TYPE string.
*-CS2025000249-26.03.2025-#170726-JT-fim

    r_if_integracao_lote_frete = me.

    CLEAR: me->zif_integracao_lote_frete~at_lote_frete.

    DATA(ck_novo) = abap_false.

    IF i_ordem_venda IS NOT INITIAL.
*-CS2025000249-26.03.2025-#170726-JT-inicio
      TRY.
          me->zif_integracao_lote_frete~set_gerencia_lote_ov( EXPORTING i_ordem_venda   = i_ordem_venda
                                                              IMPORTING e_ck_novo       = ck_novo ).
        CATCH zcx_integracao INTO DATA(ex_integracao).
        CATCH zcx_error      INTO DATA(ex_error).
      ENDTRY.
*-CS2025000249-26.03.2025-#170726-JT-fim

    ELSEIF i_pedido_compra IS NOT INITIAL.
*-CS2025000249-26.03.2025-#170726-JT-inicio
      TRY.
          me->zif_integracao_lote_frete~set_gerencia_lote_pc( EXPORTING i_pedido_compra = i_pedido_compra
                                                              IMPORTING e_ck_novo       = ck_novo ).
        CATCH zcx_integracao INTO ex_integracao.
        CATCH zcx_error      INTO ex_error.
      ENDTRY.

    ELSEIF i_solicitacao   IS NOT INITIAL.
      TRY.
          me->zif_integracao_lote_frete~set_gerencia_lote_sl( EXPORTING i_solicitacao   = i_solicitacao
                                                              IMPORTING e_ck_novo       = ck_novo ).
        CATCH zcx_integracao INTO ex_integracao.
          lv_mesg = ex_integracao->get_text( ).
          zcx_integracao=>zif_error~gera_erro_geral( i_texto = lv_mesg ).
        CATCH zcx_error      INTO ex_error.
          lv_mesg = ex_error->get_text( ).
          zcx_error=>zif_error~gera_erro_geral( i_texto = lv_mesg ).
      ENDTRY.
*-CS2025000249-26.03.2025-#170726-JT-fim

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    ELSEIF i_nro_cg_sai IS NOT INITIAL.
      TRY.
          me->zif_integracao_lote_frete~set_gerencia_lote_cg_sai_in( EXPORTING
                                                                       i_nro_cg   = i_nro_cg_sai
                                                                     IMPORTING
                                                                       e_ck_novo  = ck_novo  ).
        CATCH zcx_integracao INTO ex_integracao.
          lv_mesg = ex_integracao->get_text( ).
          zcx_integracao=>zif_error~gera_erro_geral( i_texto = lv_mesg ).
        CATCH zcx_error      INTO ex_error.
          lv_mesg = ex_error->get_text( ).
          zcx_error=>zif_error~gera_erro_geral( i_texto = lv_mesg ).
      ENDTRY.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    ENDIF.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
    CHECK me->zif_integracao_lote_frete~at_lote_frete-vbeln         IS NOT INITIAL OR
          me->zif_integracao_lote_frete~at_lote_frete-ebeln         IS NOT INITIAL OR
          me->zif_integracao_lote_frete~at_lote_frete-nro_sol       IS NOT INITIAL OR
          me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in IS NOT INITIAL.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--

    CHECK me->zif_integracao_lote_frete~at_lote_frete_old-ck_cancelado NE abap_true.

    "Cria Id. de Lote de Frete
    me->zif_integracao_lote_frete~get_new_id_lote_frete( CHANGING e_id_lote_frete = me->zif_integracao_lote_frete~at_lote_frete-id_lote_frete ).
    e_id_lote_frete = me->zif_integracao_lote_frete~at_lote_frete-id_lote_frete.

    "Verificar Necessidade de Finalizar
    IF me->zif_integracao_lote_frete~at_lote_frete_old IS NOT INITIAL.

*     IF i_pedido_compra IS NOT INITIAL.
**-------------------------------------------------------
**-----  checa se quantidade do pedido foi alterada
**-------------------------------------------------------
*       FREE: t_xekpo, t_yekpo.
*       ASSIGN ('(SAPLMEPO)xekpo[]')  TO <xekpo>[].
*       IF sy-subrc = 0.
*         t_xekpo[] = <xekpo>[].
*       ENDIF.
*       ASSIGN ('(SAPLMEPO)yekpo[]')  TO <yekpo>[].
*       IF sy-subrc = 0.
*         t_yekpo[] = <yekpo>[].
*       ENDIF.
*
**      LOOP AT t_xekpo INTO DATA(xekpo).
*         READ TABLE t_yekpo INTO DATA(yekpo) WITH KEY ebeln = xekpo-ebeln
*                                                      ebelp = xekpo-ebelp.
*         IF sy-subrc = 0.
*           IF xekpo-menge <> yekpo-menge.
*             me->zif_integracao_lote_frete~at_lote_frete-brgew = xekpo-menge.
*             EXIT.
*           ENDIF.
*         ENDIF.
*       ENDLOOP.
*     ENDIF.

      IF me->zif_integracao_lote_frete~at_lote_frete_old-bukrs                   NE me->zif_integracao_lote_frete~at_lote_frete-bukrs                   AND
         me->zif_integracao_lote_frete~at_lote_frete_old-branch                  NE me->zif_integracao_lote_frete~at_lote_frete-branch                  AND
         me->zif_integracao_lote_frete~at_lote_frete_old-id_parceiro_reme        NE me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme        AND
         me->zif_integracao_lote_frete~at_lote_frete_old-tp_parceiro_reme        NE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme        AND
         me->zif_integracao_lote_frete~at_lote_frete_old-id_parceiro_dest        NE me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest        AND
         me->zif_integracao_lote_frete~at_lote_frete_old-tp_parceiro_dest        NE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_dest        AND
         me->zif_integracao_lote_frete~at_lote_frete_old-id_parceiro_cole        NE me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole        AND
         me->zif_integracao_lote_frete~at_lote_frete_old-tp_parceiro_cole        NE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole        AND
         me->zif_integracao_lote_frete~at_lote_frete_old-id_parceiro_entr        NE me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr        AND
         me->zif_integracao_lote_frete~at_lote_frete_old-tp_parceiro_entr        NE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr        AND
         me->zif_integracao_lote_frete~at_lote_frete_old-id_parceiro_frete       NE me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete       AND
         me->zif_integracao_lote_frete~at_lote_frete_old-tp_parceiro_frete       NE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_frete       AND
         me->zif_integracao_lote_frete~at_lote_frete_old-ck_exportacao           NE me->zif_integracao_lote_frete~at_lote_frete-ck_exportacao           AND
         me->zif_integracao_lote_frete~at_lote_frete_old-tp_frete                NE me->zif_integracao_lote_frete~at_lote_frete-tp_frete                AND
         me->zif_integracao_lote_frete~at_lote_frete_old-ck_troca_nota           NE me->zif_integracao_lote_frete~at_lote_frete-ck_troca_nota           AND
         me->zif_integracao_lote_frete~at_lote_frete_old-ck_paga_trecho_terceiro NE me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro AND
         me->zif_integracao_lote_frete~at_lote_frete_old-ck_emite_nf_transito    NE me->zif_integracao_lote_frete~at_lote_frete-ck_emite_nf_transito    AND
         me->zif_integracao_lote_frete~at_lote_frete_old-ck_embarque_terceiro    NE me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro.
        "Incluir Dados de Finalização
        "ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_FINALIZADO = ABAP_TRUE.
        "ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-DT_FINALIZADO = SY-DATUM.
        "ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-HR_FINALIZADO = SY-UZEIT.
        "ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-US_FINALIZADO = SY-UNAME.
        EXIT.
      ELSEIF me->zif_integracao_lote_frete~at_lote_frete_old-brgew EQ me->zif_integracao_lote_frete~at_lote_frete-brgew AND
             me->zif_integracao_lote_frete~at_lote_frete_old-gewei EQ me->zif_integracao_lote_frete~at_lote_frete-gewei.
        "Verificar se Possui Modificação de Volume
        e_lote_frete = me->zif_integracao_lote_frete~at_lote_frete.
        IF me->zif_integracao_lote_frete~at_lote_frete-id_carguero IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    e_lote_frete = me->zif_integracao_lote_frete~at_lote_frete.

    MODIFY zlest0181 FROM me->zif_integracao_lote_frete~at_lote_frete.

    COMMIT WORK.

    IF     me->zif_integracao_lote_frete~at_lote_frete-vbeln IS NOT INITIAL AND
           me->zif_integracao_lote_frete~at_lote_frete-posnr IS NOT INITIAL.
      UPDATE vbap
         SET id_lote_frete = me->zif_integracao_lote_frete~at_lote_frete-id_lote_frete
       WHERE vbeln EQ me->zif_integracao_lote_frete~at_lote_frete-vbeln
         AND posnr EQ me->zif_integracao_lote_frete~at_lote_frete-posnr.
*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
    ELSEIF me->zif_integracao_lote_frete~at_lote_frete-ebeln IS NOT INITIAL AND
           me->zif_integracao_lote_frete~at_lote_frete-ebelp IS NOT INITIAL.
      WAIT UP TO 3 SECONDS.
      UPDATE ekpo
         SET id_lote_frete = me->zif_integracao_lote_frete~at_lote_frete-id_lote_frete
       WHERE ebeln EQ me->zif_integracao_lote_frete~at_lote_frete-ebeln
         AND ebelp EQ me->zif_integracao_lote_frete~at_lote_frete-ebelp.
*---CS2020000704 - JTASSONI - 01.10.2020 - fim
*-CS2025000249-26.03.2025-#170726-JT-inicio
    ELSEIF me->zif_integracao_lote_frete~at_lote_frete-nro_sol IS NOT INITIAL.
      UPDATE zmmt0196
         SET id_lote_frete = me->zif_integracao_lote_frete~at_lote_frete-id_lote_frete
       WHERE nro_sol EQ me->zif_integracao_lote_frete~at_lote_frete-nro_sol.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    ELSEIF me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in IS NOT INITIAL.
      UPDATE zsdt0133
         SET id_lote_frete = me->zif_integracao_lote_frete~at_lote_frete-id_lote_frete
       WHERE nro_cg EQ me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    ENDIF.
*-CS2025000249-26.03.2025-#170726-JT-fim

    COMMIT WORK.

    CHECK me->zif_integracao_lote_frete~at_lote_frete-vbeln   IS NOT INITIAL OR
          me->zif_integracao_lote_frete~at_lote_frete-ebeln   IS NOT INITIAL OR
          me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in IS NOT INITIAL OR "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
          me->zif_integracao_lote_frete~at_lote_frete-nro_sol IS NOT INITIAL.    "*-CS2025000249-26.03.2025-#170726-JT

    "Se não integrado não integrar
    IF me->zif_integracao_lote_frete~at_lote_frete-ck_finalizado EQ abap_true OR me->zif_integracao_lote_frete~at_lote_frete-ck_cancelado EQ abap_true AND
       me->zif_integracao_lote_frete~at_lote_frete-id_carguero  IS INITIAL.
      me->zif_integracao_lote_frete~get_id_referencia( IMPORTING e_referencia = DATA(e_referencia) ).
      zcl_integracao=>zif_integracao~get_instance( )->set_cancelar_envio( i_referencia = e_referencia ).
      EXIT.
    ENDIF.

    IF me->zif_integracao_lote_frete~at_lote_frete-ck_cancelado  EQ abap_false AND
       me->zif_integracao_lote_frete~at_lote_frete-ck_finalizado EQ abap_false AND
       me->zif_integracao_lote_frete~at_lote_frete-id_carguero  IS INITIAL.
      me->zif_integracao_lote_frete~get_id_referencia( IMPORTING e_referencia = e_referencia ).
      zcl_integracao=>zif_integracao~get_instance( )->set_cancelar_envio( i_referencia = e_referencia ).
      ck_novo = abap_true.
    ENDIF.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_lote_frete~get_json(
        EXPORTING
          i_tipo_json = CONV #(
                          COND string(
                             WHEN ck_novo EQ abap_true  THEN zif_integracao_lote_frete=>at_operacao_tela_i
                             WHEN ck_novo EQ abap_false THEN zif_integracao_lote_frete=>at_operacao_tela_u ) )
        IMPORTING
          r_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_data = lc_json
      )->set_ds_url(
      )->set_id_referencia(
      )->set_send_msg(
      ).

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_new_lote_frete_ov.

    DATA: wa_j_1bbranch       TYPE j_1bbranch.
    DATA: auart_range         TYPE RANGE OF auart.
    DATA: auart_sem_frete_ent TYPE RANGE OF auart.
    DATA: auart_sem_troca_nf  TYPE RANGE OF auart.
    DATA: matkl_fertilizante  TYPE RANGE OF matkl.
    DATA: matkl_algodao_pluma  TYPE RANGE OF matkl,  "CS2024000283-08.10.2024-#138126-JT-inicio
          matkl_algodao_caroco TYPE RANGE OF matkl.  "CS2024000283-08.10.2024-#138126-JT-inicio

    r_if_integracao_lote_frete = me.

    DATA(lc_ordem_venda) = i_ordem_venda.

    " Regra de Criação de Lote de Ordem de Venda """""""""""""""""""""""""""""""""""""""""""""""""""""
    " Regra de Criação de Lote de Ordem de Venda """""""""""""""""""""""""""""""""""""""""""""""""""""

    SELECT *
      FROM setleaf
      INTO TABLE @DATA(it_setleaf)
     WHERE setname EQ 'MAGGI_CARGUERO_ODVENDA'.

    IF sy-subrc IS INITIAL.
      LOOP AT it_setleaf INTO DATA(wa_setleaf).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_setleaf-valfrom ) TO auart_range.
      ENDLOOP.
    ELSE.
      auart_range = VALUE
      #(
          ( sign = 'I' option = 'EQ' low = 'ZMIT' )
          ( sign = 'I' option = 'EQ' low = 'ZFEX' )
          ( sign = 'I' option = 'EQ' low = 'ZFUT' )
          ( sign = 'I' option = 'EQ' low = 'ZRFU' )
          ( sign = 'I' option = 'EQ' low = 'ZTPP' )
          ( sign = 'I' option = 'EQ' low = 'ZTPI' )
          ( sign = 'I' option = 'EQ' low = 'ZRFL' )
*-CS2020001303 - 10.11.2021 - JT - inicio
          ( sign = 'I' option = 'EQ' low = 'ZFTE' )
          ( sign = 'I' option = 'EQ' low = 'ZOFE' )
*-CS2020001303 - 10.11.2021 - JT - fim
       ).
    ENDIF.

*-CS2020001303 - 10.11.2021 - JT - inicio
*-----------------
*-- Tipos OV sem frete entrada
*-----------------
    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_auart_frete)
     WHERE name = 'CARGUERO_AUART_SEM_FRETE_ENT'.

    IF sy-subrc = 0.
      LOOP AT t_auart_frete INTO DATA(w_auart_frete).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = w_auart_frete-low ) TO auart_sem_frete_ent.
      ENDLOOP.
    ENDIF.

*-----------------
*-- Tipos OV sem troca nota
*-----------------
    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_auart_troca)
     WHERE name = 'CARGUERO_AUART_SEM_TROCA_NF'.

    IF sy-subrc = 0.
      LOOP AT t_auart_troca INTO DATA(w_auart_troca).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = w_auart_troca-low ) TO auart_sem_troca_nf.
      ENDLOOP.
    ENDIF.

*-----------------
*-- Grupo fertilizantes
*-----------------
    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_matkl_fert)
     WHERE name = 'MAGGI_GR_FERTILIZANTES'.

    IF sy-subrc = 0.
      LOOP AT t_matkl_fert INTO DATA(w_matkl_fert).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = w_matkl_fert-low ) TO matkl_fertilizante.
      ENDLOOP.
    ENDIF.
*-CS2020001303 - 10.11.2021 - JT - fim

*-CS2024000283-08.10.2024-#138126-JT-inicio
*-----------------
*-- Grupo algodao pluma
*-----------------
    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_matkl_pluma)
     WHERE name = 'MAGGI_GR_ALGODAO_PLUMA'.

    IF sy-subrc = 0.
      LOOP AT t_matkl_pluma INTO DATA(w_matkl_pluma).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = w_matkl_pluma-low ) TO matkl_algodao_pluma.
      ENDLOOP.
    ENDIF.

*-----------------
*-- Grupo algodao caroco
*-----------------
    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_matkl_caroco)
     WHERE name = 'MAGGI_GR_ALGODAO_CAROCO'.

    IF sy-subrc = 0.
      LOOP AT t_matkl_caroco INTO DATA(w_matkl_caroco).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = w_matkl_caroco-low ) TO matkl_algodao_caroco.
      ENDLOOP.
    ENDIF.
*-CS2024000283-08.10.2024-#138126-JT-fim

    IF ( lc_ordem_venda-auart NOT IN auart_range ).
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_lote_frete_ov-msgid msgno = zcx_integracao=>zcx_lote_frete_ov-msgno attr1 = CONV #( lc_ordem_venda-vbeln ) )
          msgid  = zcx_integracao=>zcx_lote_frete_ov-msgid
          msgno  = zcx_integracao=>zcx_lote_frete_ov-msgno
          msgty  = 'E'
          msgv1  = CONV #( lc_ordem_venda-vbeln ).
    ENDIF.

*---CS2020001303 - 10.11.2021 - JT - inicio
    IF lc_ordem_venda-auart IN auart_sem_frete_ent[] AND auart_sem_frete_ent[] IS NOT INITIAL.
      lc_ordem_venda-kvgr5 = '001'. "Não
    ENDIF.

    IF lc_ordem_venda-auart IN auart_sem_troca_nf[] AND auart_sem_troca_nf[] IS NOT INITIAL.
      lc_ordem_venda-ztrocanota = abap_off.
    ENDIF.
*---CS2020001303 - 10.11.2021 - JT - fim

*-CS2024000283-08.10.2024-#138126-JT-inicio
    SELECT SINGLE * INTO @DATA(wa_mara)
      FROM mara
     WHERE matnr EQ @lc_ordem_venda-item-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_mara-matkl
      IMPORTING
        output = wa_mara-matkl.

    IF ( wa_mara-matkl IN matkl_algodao_pluma[]  AND matkl_algodao_pluma[]  IS NOT INITIAL and lc_ordem_venda-auart = 'ZGLO' ) OR "CS2024000283 Parte 1 - ALGODÃO NO CARGUERO (STRADA) #180555 - BG
       ( wa_mara-matkl IN matkl_algodao_caroco[] AND matkl_algodao_caroco[] IS NOT INITIAL ) .
      lc_ordem_venda-kvgr5                                   = '001'.    " não (frete de entrada)
      me->zif_integracao_lote_frete~at_lote_frete-kvgr4      = abap_off. "(corredor)
*     me->zif_integracao_lote_frete~at_lote_frete-tp_produto = 'RR'.     "(tipo produto)
    ENDIF.
*-CS2024000283-08.10.2024-#138126-JT-fim

    IF lc_ordem_venda-kvgr5 IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_sem_info_frete_ent-msgid msgno = zcx_integracao=>zcx_sem_info_frete_ent-msgno )
          msgid  = zcx_integracao=>zcx_sem_info_frete_ent-msgid
          msgno  = zcx_integracao=>zcx_sem_info_frete_ent-msgno
          msgty  = 'E'.
    ENDIF.

    "Atribuir Dados de Ordem de Venda ao Lote de Frete """""""""""""""""""""""""""""""""""""""""""""""
    "Atribuir Dados de Ordem de Venda ao Lote de Frete """""""""""""""""""""""""""""""""""""""""""""""
    DELETE lc_ordem_venda-parceiros WHERE updkz EQ zif_integracao_lote_frete=>at_operacao_tela_d.
*    DELETE LC_ORDEM_VENDA-COMERCIAL WHERE POSNR IS INITIAL.

    "Trata os casos em que as ordens de venda não possuem dados de intens nessa
    IF ( lc_ordem_venda-auart EQ 'ZRFL' ) OR ( lc_ordem_venda-auart EQ 'ZRDC' ).
      DELETE lc_ordem_venda-comercial WHERE posnr IS INITIAL.
    ENDIF.

    READ TABLE lc_ordem_venda-comercial INTO DATA(wa_comercial) INDEX 1.

    me->zif_integracao_lote_frete~at_lote_frete-vbeln = lc_ordem_venda-vbeln.
    me->zif_integracao_lote_frete~at_lote_frete-kvgr4 = lc_ordem_venda-kvgr4.
    me->zif_integracao_lote_frete~at_lote_frete-kvgr5 = lc_ordem_venda-kvgr5.
    me->zif_integracao_lote_frete~at_lote_frete-posnr = lc_ordem_venda-item-posnr.
    me->zif_integracao_lote_frete~at_lote_frete-matnr = lc_ordem_venda-item-matnr.

*-CS2024000283-08.10.2024-#138126-JT-inicio
*   SELECT SINGLE * INTO @DATA(wa_mara)
*     FROM mara
*    WHERE matnr EQ @lc_ordem_venda-item-matnr.
*
*   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*     EXPORTING
*       input  = wa_mara-matkl
*     IMPORTING
*       output = wa_mara-matkl.
*-CS2024000283-08.10.2024-#138126-JT-fm

*---CS2020001303 - 10.11.2021 - JT - inicio
    IF wa_mara-matkl IN matkl_fertilizante[] AND matkl_fertilizante[] IS NOT INITIAL AND
       me->zif_integracao_lote_frete~at_lote_frete-kvgr4 = abap_off.
      me->zif_integracao_lote_frete~at_lote_frete-kvgr4 = '001'.
    ENDIF.
*---CS2020001303 - 10.11.2021 - JT - fim

    CASE wa_mara-matkl.
      WHEN '700170'. "Milho
        me->zif_integracao_lote_frete~at_lote_frete-tp_produto = 'RR'.
      WHEN '700110' OR '700160' OR '700190' OR '700120' OR '700200'. "Soja/Oleo/Farelo Hipro/Farelo Comum/Casca
        me->zif_integracao_lote_frete~at_lote_frete-tp_produto =
           COND string( WHEN lc_ordem_venda-kvgr3 = 'R' THEN 'RR'
           WHEN lc_ordem_venda-kvgr3 = 'C' THEN 'CO' ).
      WHEN OTHERS.
*---CS2020001303 - 10.11.2021 - JT - inicio
        IF ( wa_mara-matkl IN matkl_fertilizante[]   AND matkl_fertilizante[]   IS NOT INITIAL ) OR "*-CS2024000283-08.10.2024-#138126-JT-inicio
           ( wa_mara-matkl IN matkl_algodao_pluma[]  AND matkl_algodao_pluma[]  IS NOT INITIAL and lc_ordem_venda-auart = 'ZGLO' ) OR "*-CS2024000283-08.10.2024-#138126-JT-inicio "CS2024000283 Parte 1 - ALGODÃO NO CARGUERO (STRADA) #180555 - BG
           ( wa_mara-matkl IN matkl_algodao_caroco[] AND matkl_algodao_caroco[] IS NOT INITIAL ).   "*-CS2024000283-08.10.2024-#138126-JT-inicio
          me->zif_integracao_lote_frete~at_lote_frete-tp_produto = abap_off.
        ELSE.                                                                                           "*-CS2024000283-08.10.2024-#138126-JT-inicio
          zcx_integracao=>zif_error~gera_erro_geral( i_texto = 'Produto não gera Lote de Frete!' ).
        ENDIF.
*---CS2020001303 - 10.11.2021 - JT - fim
    ENDCASE.

    me->zif_integracao_lote_frete~at_lote_frete-gewei = lc_ordem_venda-item-gewei.
    me->zif_integracao_lote_frete~at_lote_frete-brgew = lc_ordem_venda-item-brgew.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = lc_ordem_venda-item-werks
      IMPORTING
        wa_j_1bbranch        = wa_j_1bbranch
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lc_texto).
      zcx_integracao=>zif_error~gera_erro_geral( i_texto = lc_texto ).
    ENDIF.

    me->zif_integracao_lote_frete~at_lote_frete-bukrs  = wa_j_1bbranch-bukrs.
    me->zif_integracao_lote_frete~at_lote_frete-branch = wa_j_1bbranch-branch.

    "Verificar Depara Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->zif_integracao_lote_frete~get_id_material_carguero(
      EXPORTING
        i_bukrs       = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
        i_material    = CONV #( me->zif_integracao_lote_frete~at_lote_frete-matnr )
        i_tp_material = CONV #( me->zif_integracao_lote_frete~at_lote_frete-tp_produto )
      IMPORTING
        e_id_material_carguero  = DATA(e_id_material_carguero) ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Verificar Depara Unidade Organizacional """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->zif_integracao_lote_frete~get_id_uni_org_carguero(
      EXPORTING
        i_bukrs = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
        i_branch = CONV #( me->zif_integracao_lote_frete~at_lote_frete-branch )
        i_unidade_maggi   = CONV #( me->zif_integracao_lote_frete~at_lote_frete-kvgr4 )
      IMPORTING
        e_id_ung_carguero = DATA(e_id_ung_carguero) ).
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Remetente
    READ TABLE lc_ordem_venda-parceiros
    WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_reme_ov INTO DATA(wa_parceiro).
    IF sy-subrc IS INITIAL.
      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme = wa_parceiro-kunnr.
      me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme = zif_integracao_lote_frete=>at_tp_parceiro_cliente.
    ENDIF.

    "Destinatário
    READ TABLE lc_ordem_venda-parceiros
    WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_dest_ov INTO wa_parceiro.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest = wa_parceiro-kunnr.
      me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_dest = zif_integracao_lote_frete=>at_tp_parceiro_cliente.
    ENDIF.

    "Local de Coleta
    READ TABLE lc_ordem_venda-parceiros
    WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_cole_ov INTO wa_parceiro.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole = wa_parceiro-lifnr.
      me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
    ENDIF.

    "Local de Entrega
    READ TABLE lc_ordem_venda-parceiros
    WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_entr_ov INTO wa_parceiro.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr = wa_parceiro-kunnr.
      me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr = zif_integracao_lote_frete=>at_tp_parceiro_cliente.
    ENDIF.

    "Troca Nota
    "Indica que o carregamento será feito em terceiro (embarque em terceiro) e que o Agente de Embarque
    "(pessoa responsável por informar o carregamento, através de um tablet) deverá enviar a nota fiscal
    "do produtor para que possa ser feito a entrada da nota.

    me->zif_integracao_lote_frete~at_lote_frete-ck_troca_nota = lc_ordem_venda-ztrocanota.

    "Embarque em Terceiro
    "Segue as mesmas características do Troca-Notas, porém o Agente de Embarque não é
    "obrigado a informar a nota fiscal do produtor.

    me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro = abap_false.
    me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro = abap_false.

*    CASE ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-TP_PARCEIRO_COLE.
*      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_TP_PARCEIRO_FORNECEDOR.
*        TRY .
*            ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
*              )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_PARCEIRO_COLE
*              )->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS
*              ).
*          CATCH ZCX_PARCEIROS.
*            ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_EMBARQUE_TERCEIRO = ABAP_TRUE.
*        ENDTRY.
*      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_TP_PARCEIRO_CLIENTE.
*        TRY .
*            ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE(
*              )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_PARCEIRO_COLE
*              )->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS
*              ).
*          CATCH ZCX_PARCEIROS.
*            ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_EMBARQUE_TERCEIRO = ABAP_TRUE.
*        ENDTRY.
*    ENDCASE.

    "CIF ou FOB
    "Indica se o lote se trata de uma Venda FOB. Ou seja, não será o embarcador que pagará o
    "frete. Se esta flag estiver marcada, no momento da criação do lote o embarcador não é
    "obrigado informar o valor do frete empresa. Em cenários de venda FOB, o destinatário do
    "lote poderá operar o sistema (quando possuir tenant cadastrado).

    CASE wa_comercial-inco1.
      WHEN zif_carga=>st_tp_frete_cif OR zif_carga=>st_tp_frete_cpt.

        "Amaggi Paga Frete
        me->zif_integracao_lote_frete~at_lote_frete-tp_frete = wa_comercial-inco1.

      WHEN zif_carga=>st_tp_frete_cfr.
        "CFR = FOB
        "Amaggi não paga frete
        me->zif_integracao_lote_frete~at_lote_frete-tp_frete = zif_carga=>st_tp_frete_fob.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_erro_ov_incotermo-msgid
                              msgno = zcx_integracao=>zcx_erro_ov_incotermo-msgno
                              attr1 = CONV #( wa_comercial-inco1 ) )
            msgid  = zcx_integracao=>zcx_erro_ov_incotermo-msgid
            msgno  = zcx_integracao=>zcx_erro_ov_incotermo-msgno
            msgty  = 'E'
            msgv1  = CONV #( wa_comercial-inco1 ).
    ENDCASE.

    IF wa_comercial-inco1 EQ zif_carga=>st_tp_frete_cif.
      "Transportadora Definida (Transportadora Própria)
      READ TABLE lc_ordem_venda-parceiros
      WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_frete_ov INTO wa_parceiro.
      IF sy-subrc IS INITIAL.
        me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete = wa_parceiro-lifnr.
        me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_frete = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
      ENDIF.

      IF me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete IS NOT INITIAL.
        TRY .
            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete
              )->ck_parceiro_local_negocio(
              ")->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS
              ).
          CATCH zcx_parceiros INTO DATA(ex_parceiros).    "
            zcx_integracao=>zif_error~gera_erro_geral( i_texto = ex_parceiros->get_text( ) ).
        ENDTRY.
      ENDIF.

    ELSEIF wa_comercial-inco1 EQ zif_carga=>st_tp_frete_cpt OR wa_comercial-inco1 EQ zif_carga=>st_tp_frete_cfr OR wa_comercial-inco1 EQ zif_carga=>st_tp_frete_fob.
      "Passar sem Parceiro de Frete Definido

    ELSE.
      "Transportadora Definida (Transportadora Terceira)
      zcx_integracao=>zif_error~gera_erro_geral( i_texto = 'Agente de Frete tem que ser SIM!' ).
    ENDIF.

    "Paga Trecho Terceiro
    "Indica se é ou não obrigatório informar o valor do frete empresa para o trecho terceiro
    "(trecho entre a filial do embarcador e o armazém de terceiro).
    me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.

    CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme.
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
          )->get_txjcd( IMPORTING e_txjcd = DATA(e_txjcd_reme)
          ).
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
          )->get_txjcd( IMPORTING e_txjcd = e_txjcd_reme
          ).
    ENDCASE.

    CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole.
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
          )->get_txjcd( IMPORTING e_txjcd = DATA(e_txjcd_cole)
          ).
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
          )->get_txjcd( IMPORTING e_txjcd = e_txjcd_cole
          ).
    ENDCASE.

    "Emitir NF de trânsito
    "Indica que o motorista deverá solicitar a emissão da nota fiscal de trânsito antes de partir
    "para o local de coleta. O motorista não poderá comparecer ao local de coleta sem a NF de trânsito
    "ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_EMITE_NF_TRANSITO = COND STRING( WHEN E_TXJCD_REME EQ E_TXJCD_COLE THEN ABAP_FALSE ELSE ABAP_TRUE ) .
    me->zif_integracao_lote_frete~at_lote_frete-ck_emite_nf_transito = abap_false.

*-CS2019001158 - Jaime Tassoni - 12.11.2020 - inicio
    IF     lc_ordem_venda-kvgr5 = '001'.
      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.
    ELSEIF lc_ordem_venda-kvgr5 = '002'.
      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_true.
    ELSE.
      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.
    ENDIF.
*-CS2019001158 - Jaime Tassoni - 12.11.2020 - fim

    "Mercadoria de Exportação
    me->zif_integracao_lote_frete~at_lote_frete-ck_exportacao = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_new_lote_frete_pc.

    DATA: wa_j_1bbranch       TYPE  j_1bbranch.
    DATA: bsart_range         TYPE RANGE OF bsart.
    DATA: bsart_sem_troca_nf  TYPE RANGE OF bsart.
    DATA: matkl_fertilizante  TYPE RANGE OF matkl.

    r_if_integracao_lote_frete = me.

    DATA(lc_pedido_compra) = i_pedido_compra.

*-----------------------------------------------
    " Regra de Criação de Lote de Pedido de compras """"""""""""""""""""""""""""""""""""""""""""""""""
*-----------------------------------------------
    SELECT *
      FROM setleaf
      INTO TABLE @DATA(it_setleaf)
     WHERE setname EQ 'MAGGI_CARGUERO_PEDCOMP'.

    IF sy-subrc IS INITIAL.
      LOOP AT it_setleaf INTO DATA(wa_setleaf).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_setleaf-valfrom ) TO bsart_range.
      ENDLOOP.
    ELSE.
      bsart_range = VALUE
      #(
*-CS2020001303 - 12.11.2021 - JT - inicio
          ( sign = 'I' option = 'EQ' low = 'ZFTE' )
*-CS2020001303 - 12.11.2021 - JT - fim
       ).
    ENDIF.

*-CS2020001303 - 12.11.2021 - JT - inicio
*-----------------
*-- Tipos PC sem troca nota
*-----------------
    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_bsart_troca)
     WHERE name = 'CARGUERO_BSART_SEM_TROCA_NF'.

    IF sy-subrc = 0.
      LOOP AT t_bsart_troca INTO DATA(w_bsart_troca).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = w_bsart_troca-low ) TO bsart_sem_troca_nf.
      ENDLOOP.
    ENDIF.

*-----------------
*-- Grupo fertilizantes
*-----------------
    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_matkl_fert)
     WHERE name = 'MAGGI_GR_FERTILIZANTES'.

    IF sy-subrc = 0.
      LOOP AT t_matkl_fert INTO DATA(w_matkl_fert).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = w_matkl_fert-low ) TO matkl_fertilizante.
      ENDLOOP.
    ENDIF.
*-CS2020001303 - 12.11.2021 - JT - fim

    IF ( lc_pedido_compra-bsart NOT IN bsart_range ).
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_lote_frete_ov-msgid
                            msgno = zcx_integracao=>zcx_lote_frete_ov-msgno
                            attr1 = CONV #( lc_pedido_compra-ebeln ) )
          msgid  = zcx_integracao=>zcx_lote_frete_ov-msgid
          msgno  = zcx_integracao=>zcx_lote_frete_ov-msgno
          msgty  = 'E'
          msgv1  = CONV #( lc_pedido_compra-ebeln ).
    ENDIF.


*-CS2020001303 - 12.11.2021 - JT - inicio
    IF lc_pedido_compra-bsart IN bsart_sem_troca_nf[] AND bsart_sem_troca_nf[] is NOT INITIAL.
      lc_pedido_compra-item-ztrocanota = abap_off.
    ENDIF.
*-CS2020001303 - 12.11.2021 - JT - fim

    me->zif_integracao_lote_frete~at_lote_frete-ebeln = lc_pedido_compra-ebeln.
    me->zif_integracao_lote_frete~at_lote_frete-kvgr4 = lc_pedido_compra-item-zkvgr4.
    me->zif_integracao_lote_frete~at_lote_frete-ebelp = lc_pedido_compra-item-ebelp.
    me->zif_integracao_lote_frete~at_lote_frete-matnr = lc_pedido_compra-item-matnr.

*----------------------------------------------
* Somente envia pedido de grãos e derivados ao carguero
*----------------------------------------------
    SELECT SINGLE *
             INTO @DATA(wa_mara)
             FROM mara
            WHERE matnr = @lc_pedido_compra-item-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_mara-matkl
      IMPORTING
        output = wa_mara-matkl.

    CASE wa_mara-matkl.
      WHEN '700170'. "Milho
        me->zif_integracao_lote_frete~at_lote_frete-tp_produto = 'RR'.
      WHEN '700110' OR '700160' OR '700190' OR '700120' OR '700200'. "Soja/Oleo/Farelo Hipro/Farelo Comum/Casca
        me->zif_integracao_lote_frete~at_lote_frete-tp_produto =
           COND string( WHEN lc_pedido_compra-item-zkvgr3 = 'R' THEN 'RR'
                        WHEN lc_pedido_compra-item-zkvgr3 = 'C' THEN 'CO' ).
      WHEN OTHERS.
*---CS2020001303 - 12.11.2021 - JT - inicio
        IF wa_mara-matkl IN matkl_fertilizante[] AND matkl_fertilizante[] is NOT INITIAL.
          me->zif_integracao_lote_frete~at_lote_frete-tp_produto = abap_off.
        ELSE.
          zcx_integracao=>zif_error~gera_erro_geral( i_texto = 'Produto não gera Lote de Frete!' ).
        ENDIF.
*---CS2020001303 - 12.11.2021 - JT - inicio
    ENDCASE.

    me->zif_integracao_lote_frete~at_lote_frete-gewei = lc_pedido_compra-item-gewei.
    me->zif_integracao_lote_frete~at_lote_frete-brgew = lc_pedido_compra-item-menge.

*&------------------------------------------------------------------------------------------
*&  Inicio ajuste realizado para pedidos ZUB notas de transferencias / CS2023000749 / AOENNING
*&------------------------------------------------------------------------------------------
IF lc_pedido_compra-bsart NE 'ZUB'.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = lc_pedido_compra-item-werks
      IMPORTING
        wa_j_1bbranch        = wa_j_1bbranch
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

ELSE.

      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
        EXPORTING
          centro               = lc_pedido_compra-reswk
        IMPORTING
          wa_j_1bbranch        = wa_j_1bbranch
        EXCEPTIONS
          informar_centro      = 1
          nao_centro_r_virtual = 2
          informar_centro_out  = 3
          informar_centro_v    = 4
          OTHERS               = 5.
    ENDIF.
*&------------------------------------------------------------------------------------------
*&  Fim ajuste realizado para pedidos ZUB notas de transferencias / CS2023000749 / AOENNING
*&------------------------------------------------------------------------------------------

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lc_texto).
      zcx_integracao=>zif_error~gera_erro_geral( i_texto = lc_texto ).
    ENDIF.

    me->zif_integracao_lote_frete~at_lote_frete-bukrs  = wa_j_1bbranch-bukrs.
    me->zif_integracao_lote_frete~at_lote_frete-branch = wa_j_1bbranch-branch.

*----------------------------------------------
*    "Verificar Depara Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*----------------------------------------------
    me->zif_integracao_lote_frete~get_id_material_carguero(
      EXPORTING
        i_bukrs       = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
        i_material    = CONV #( me->zif_integracao_lote_frete~at_lote_frete-matnr )
        i_tp_material = CONV #( me->zif_integracao_lote_frete~at_lote_frete-tp_produto )
      IMPORTING
        e_id_material_carguero  = DATA(e_id_material_carguero) ).
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
*----------------------------------------------
*    "Verificar Depara Unidade Organizacional """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*----------------------------------------------
    me->zif_integracao_lote_frete~get_id_uni_org_carguero(
      EXPORTING
        i_bukrs = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
        i_branch = CONV #( me->zif_integracao_lote_frete~at_lote_frete-branch )
        i_unidade_maggi   = CONV #( me->zif_integracao_lote_frete~at_lote_frete-kvgr4 )
      IMPORTING
        e_id_ung_carguero = DATA(e_id_ung_carguero) ).
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
*---------------------------------------------------------
*    "Remetente
*---------------------------------------------------------

    CASE lc_pedido_compra-bsart.
      WHEN 'ZUB'.

        me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lc_pedido_compra-reswk
          IMPORTING
            output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme.

      WHEN OTHERS.

        READ TABLE lc_pedido_compra-parceiros WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_cole_pc INTO DATA(wa_parceiro).
        IF sy-subrc IS INITIAL.
          me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
          me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme = wa_parceiro-lifn2.
        ENDIF.

    ENDCASE.





*   READ TABLE lc_pedido_compra-parceiros
*   WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_reme_ov INTO DATA(wa_parceiro).
*   IF sy-subrc IS INITIAL.
*     me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme = wa_parceiro-lifn2.
*     me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme = zif_integracao_lote_frete=>at_tp_parceiro_cliente.
*   ENDIF.

*---------------------------------------------------------
*    "Destinatário
*---------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_pedido_compra-item-werks
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_dest = zif_integracao_lote_frete=>at_tp_parceiro_cliente.

*    READ TABLE lc_ordem_venda-parceiros
*    WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_dest_ov INTO wa_parceiro.
*    IF sy-subrc IS INITIAL.
*      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest = wa_parceiro-kunnr.
*      me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_dest = zif_integracao_lote_frete=>at_tp_parceiro_cliente.
*    ENDIF.
*
*---------------------------------------------------------
*    "Local de Coleta
*---------------------------------------------------------
    READ TABLE lc_pedido_compra-parceiros
    WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_cole_pc INTO wa_parceiro.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole = wa_parceiro-lifn2.
      me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
    ENDIF.
*
*---------------------------------------------------------
*    "Local de Entrega / descarga
*---------------------------------------------------------
    READ TABLE lc_pedido_compra-parceiros
    WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_entr_pc INTO wa_parceiro.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr = wa_parceiro-lifn2.
      IF wa_parceiro-lifn2 IS INITIAL.
        me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr =
        me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest.
      ENDIF.
      me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr = zif_integracao_lote_frete=>at_tp_parceiro_cliente.
    ELSE.
      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr =
      me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest.
      me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr = zif_integracao_lote_frete=>at_tp_parceiro_cliente.
    ENDIF.

*    "Troca Nota
*    "Indica que o carregamento será feito em terceiro (embarque em terceiro) e que o Agente de Embarque
*    "(pessoa responsável por informar o carregamento, através de um tablet) deverá enviar a nota fiscal
*    "do produtor para que possa ser feito a entrada da nota.
*
    me->zif_integracao_lote_frete~at_lote_frete-ck_troca_nota = lc_pedido_compra-item-ztrocanota.
*
*    "Embarque em Terceiro
*    "Segue as mesmas características do Troca-Notas, porém o Agente de Embarque não é
*    "obrigado a informar a nota fiscal do produtor.
*
    me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro = abap_false.
    me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro = abap_false.
*
*    CASE ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-TP_PARCEIRO_COLE.
*      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_TP_PARCEIRO_FORNECEDOR.
*        TRY .
*            ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
*              )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_PARCEIRO_COLE
*              )->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS
*              ).
*          CATCH ZCX_PARCEIROS.
*            ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_EMBARQUE_TERCEIRO = ABAP_TRUE.
*        ENDTRY.
*      WHEN ZIF_INTEGRACAO_LOTE_FRETE=>AT_TP_PARCEIRO_CLIENTE.
*        TRY .
*            ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE(
*              )->SET_PARCEIRO( I_PARCEIRO = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-ID_PARCEIRO_COLE
*              )->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS
*              ).
*          CATCH ZCX_PARCEIROS.
*            ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_EMBARQUE_TERCEIRO = ABAP_TRUE.
*        ENDTRY.
*    ENDCASE.
*
*    "CIF ou FOB
*    "Indica se o lote se trata de uma Venda FOB. Ou seja, não será o embarcador que pagará o
*    "frete. Se esta flag estiver marcada, no momento da criação do lote o embarcador não é
*    "obrigado informar o valor do frete empresa. Em cenários de venda FOB, o destinatário do
*    "lote poderá operar o sistema (quando possuir tenant cadastrado).
*
    CASE lc_pedido_compra-item-inco1.
      WHEN zif_carga=>st_tp_frete_cif OR zif_carga=>st_tp_frete_cpt.

        "Amaggi Paga Frete
        me->zif_integracao_lote_frete~at_lote_frete-tp_frete = lc_pedido_compra-item-inco1.

      WHEN zif_carga=>st_tp_frete_cfr OR zif_carga=>st_tp_frete_fob.
        "CFR = FOB
        "Amaggi não paga frete
        me->zif_integracao_lote_frete~at_lote_frete-tp_frete = zif_carga=>st_tp_frete_fob.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_erro_ov_incotermo-msgid
                              msgno = zcx_integracao=>zcx_erro_ov_incotermo-msgno
                              attr1 = CONV #( lc_pedido_compra-item-inco1 ) )
            msgid  = zcx_integracao=>zcx_erro_ov_incotermo-msgid
            msgno  = zcx_integracao=>zcx_erro_ov_incotermo-msgno
            msgty  = 'E'
            msgv1  = CONV #( lc_pedido_compra-item-inco1 ).
    ENDCASE.

    IF lc_pedido_compra-item-inco1 EQ zif_carga=>st_tp_frete_cif.
      "Transportadora Definida (Transportadora Própria)
      READ TABLE lc_pedido_compra-parceiros
      WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_frete_ov INTO wa_parceiro.
      IF sy-subrc IS INITIAL.
        me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete = wa_parceiro-lifn2.
        me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_frete = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
      ENDIF.

      IF me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete IS NOT INITIAL.
        TRY .
            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete
              )->ck_parceiro_local_negocio(
              ")->CK_PARCEIRO_INTERCOMPANY( I_EMPRESA = ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-BUKRS
              ).
          CATCH zcx_parceiros INTO DATA(ex_parceiros).    "
            zcx_integracao=>zif_error~gera_erro_geral( i_texto = ex_parceiros->get_text( ) ).
        ENDTRY.
      ENDIF.

    ELSEIF lc_pedido_compra-item-inco1 EQ zif_carga=>st_tp_frete_cpt OR
           lc_pedido_compra-item-inco1 EQ zif_carga=>st_tp_frete_cfr OR
           lc_pedido_compra-item-inco1 EQ zif_carga=>st_tp_frete_fob.
      "Passar sem Parceiro de Frete Definido

    ELSE.
      "Transportadora Definida (Transportadora Terceira)
      zcx_integracao=>zif_error~gera_erro_geral( i_texto = 'Agente de Frete tem que ser SIM!' ).
    ENDIF.
*
*    "Paga Trecho Terceiro
*    "Indica se é ou não obrigatório informar o valor do frete empresa para o trecho terceiro
*    "(trecho entre a filial do embarcador e o armazém de terceiro).
    me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.

    CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme.
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
          )->get_txjcd( IMPORTING e_txjcd = DATA(e_txjcd_reme)
          ).
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
          )->get_txjcd( IMPORTING e_txjcd = e_txjcd_reme
          ).
    ENDCASE.
*
    CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole.
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
          )->get_txjcd( IMPORTING e_txjcd = DATA(e_txjcd_cole)
          ).
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
          )->get_txjcd( IMPORTING e_txjcd = e_txjcd_cole
          ).
    ENDCASE.
*
*    "Emitir NF de trânsito
*    "Indica que o motorista deverá solicitar a emissão da nota fiscal de trânsito antes de partir
*    "para o local de coleta. O motorista não poderá comparecer ao local de coleta sem a NF de trânsito
*    "ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_EMITE_NF_TRANSITO = COND STRING( WHEN E_TXJCD_REME EQ E_TXJCD_COLE THEN ABAP_FALSE ELSE ABAP_TRUE ) .
    me->zif_integracao_lote_frete~at_lote_frete-ck_emite_nf_transito = abap_false.

*-CS2019001158 - Jaime Tassoni - 12.11.2020 - inicio
    IF     lc_pedido_compra-item-zckfreteent = abap_false.
      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.
    ELSEIF lc_pedido_compra-item-zckfreteent = abap_true.
      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_true.
    ELSE.
      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.
    ENDIF.
*-CS2019001158 - Jaime Tassoni - 12.11.2020 - fim
*
*    "Mercadoria de Exportação
    me->zif_integracao_lote_frete~at_lote_frete-ck_exportacao = abap_true.
*
  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_registro.

    "Recupera Registro
    DATA: lc_ordem_venda   TYPE zde_cargueiro_ov.
*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
    DATA: lc_pedido_compra TYPE zde_cargueiro_pc.
*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
    DATA: lc_solicitacao   TYPE zde_cargueiro_sl.  "*-CS2025000249-26.03.2025-#170726-JT-inicio

    r_if_integracao_lote_frete = me.

    IF i_id_lote_frete IS INITIAL.

      IF     i_ordem_venda IS NOT INITIAL.
        lc_ordem_venda = i_ordem_venda.

        IF lc_ordem_venda-item-id_lote_frete IS INITIAL AND lc_ordem_venda-vbeln IS NOT INITIAL AND lc_ordem_venda-item-posnr IS NOT INITIAL.
          SELECT SINGLE id_lote_frete INTO @DATA(lc_lote_frete)
            FROM zlest0181
           WHERE vbeln EQ @lc_ordem_venda-vbeln
             AND posnr EQ @lc_ordem_venda-item-posnr.

          IF sy-subrc IS INITIAL.
            UPDATE vbap
               SET id_lote_frete = lc_lote_frete
             WHERE vbeln EQ lc_ordem_venda-vbeln
               AND posnr EQ lc_ordem_venda-item-posnr
               AND id_lote_frete EQ space.
          ENDIF.
        ENDIF.
      ELSEIF i_pedido_compra IS NOT INITIAL.

*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
        lc_pedido_compra = i_pedido_compra.

        IF lc_pedido_compra-item-id_lote_frete IS INITIAL AND
           lc_pedido_compra-ebeln              IS NOT INITIAL AND
           lc_pedido_compra-item-ebelp         IS NOT INITIAL.
          SELECT SINGLE id_lote_frete INTO @lc_lote_frete
            FROM zlest0181
           WHERE ebeln EQ @lc_pedido_compra-ebeln
             AND ebelp EQ @lc_pedido_compra-item-ebelp.

          IF sy-subrc IS INITIAL.
            UPDATE ekpo
               SET id_lote_frete = lc_lote_frete
             WHERE ebeln EQ lc_pedido_compra-ebeln
               AND ebelp EQ lc_pedido_compra-item-ebelp
               AND id_lote_frete EQ space.
          ENDIF.
        ENDIF.
*---CS2020000704 - JTASSONI - 01.10.2020 - fim

*-CS2025000249-26.03.2025-#170726-JT-inicio
      ELSEIF  i_solicitacao IS NOT INITIAL.
        lc_solicitacao = i_solicitacao.

        SELECT SINGLE *
         FROM zlest0181 INTO @DATA(lwa_zlest0181_exists)
        WHERE nro_sol EQ @lc_solicitacao-nro_sol.

        IF lwa_zlest0181_exists IS NOT INITIAL.
          LOOP AT lc_solicitacao-item INTO DATA(lwa_item_sol) WHERE id_lote_frete IS INITIAL.
            UPDATE zmmt0196
               SET id_lote_frete = lwa_zlest0181_exists-id_lote_frete
             WHERE nro_sol EQ lwa_item_sol-nro_sol
               AND seq     EQ lwa_item_sol-seq.
          ENDLOOP.
        ENDIF.

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      ELSEIF i_nro_cg_sai IS NOT INITIAL.

        SELECT SINGLE *
         FROM zlest0181 INTO @lwa_zlest0181_exists
        WHERE nro_cg_sai_in EQ @i_nro_cg_sai.

        IF lwa_zlest0181_exists IS NOT INITIAL.
          SELECT SINGLE *
            FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
           WHERE nro_cg EQ @i_nro_cg_sai.

          IF sy-subrc EQ 0 AND lwa_zsdt0133-id_lote_frete IS INITIAL.
            UPDATE zsdt0133
               SET id_lote_frete = lwa_zlest0181_exists-id_lote_frete
             WHERE nro_cg EQ i_nro_cg_sai.
          ENDIF.
        ENDIF.
        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

      ENDIF.
*-CS2025000249-26.03.2025-#170726-JT-fim
    ELSE.
      lc_lote_frete = i_id_lote_frete.
    ENDIF.

    CLEAR: me->zif_integracao_lote_frete~at_lote_frete.

    SELECT SINGLE * INTO @me->zif_integracao_lote_frete~at_lote_frete
      FROM zlest0181
     WHERE id_lote_frete EQ @lc_lote_frete.

    IF me->zif_integracao_lote_frete~at_lote_frete-vbeln IS NOT INITIAL OR
       me->zif_integracao_lote_frete~at_lote_frete-ebeln IS NOT INITIAL OR
       me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in IS NOT INITIAL OR "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
       me->zif_integracao_lote_frete~at_lote_frete-nro_sol IS NOT INITIAL. "CS2025000249-26.03.2025-#170726-JT-fim
      me->zif_integracao_lote_frete~at_lote_frete_old = me->zif_integracao_lote_frete~at_lote_frete.
    ENDIF.

    CHECK sy-subrc IS NOT INITIAL.

    RAISE EXCEPTION TYPE zcx_integracao
      EXPORTING
        textid = VALUE #( msgid = zcx_integracao=>zcx_lote_frete_not_found-msgid
                          msgno = zcx_integracao=>zcx_lote_frete_not_found-msgno
                          attr1 = CONV #( lc_lote_frete ) )
        msgid  = zcx_integracao=>zcx_lote_frete_not_found-msgid
        msgno  = zcx_integracao=>zcx_lote_frete_not_found-msgno
        msgty  = 'E'
        msgv1  = CONV #( lc_lote_frete ).

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    r_if_integracao_lote_frete = me.

    CREATE OBJECT lc_integracao.

    ""WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
    IF me->zif_integracao_inject~at_tp_sincronia = zif_integracao=>at_tp_sincronia_sincrona.

      lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
          )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_lote_frete~at_lote_frete-id_integracao
          )->set_outbound_msg( IMPORTING e_integracao = DATA(e_integracao)
          )->set_processar_retorno(
          )->set_integrar_retorno(
          )->get_registro( IMPORTING e_integracao = e_integracao
          )->free(
          ).
    ELSE.
    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676

      "Cria MSG para Integração via HTTP
      lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
        )->set_new_msg( IMPORTING e_id_integracao	= me->zif_integracao_lote_frete~at_lote_frete-id_integracao
        )->free(
        ).

    ENDIF.

    MODIFY zlest0181 FROM me->zif_integracao_lote_frete~at_lote_frete.
    COMMIT WORK.

    CLEAR: lc_integracao.

    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
*    MODIFY zlest0181 FROM me->zif_integracao_lote_frete~at_lote_frete.
*    COMMIT WORK.
    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_gerencia_lote_cg_sai_in.

    DATA: lwa_zlest0181  TYPE zlest0181,
          lv_mesg        TYPE string.

    e_ck_novo      = abap_false.

    CLEAR: lwa_zlest0181.

    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lwa_0133)
     WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0 AND lwa_0133-integrar_carguero eq abap_true.

    SELECT SINGLE *
      FROM zsdt0129 INTO @DATA(lwa_0129)
     WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE vbeln, matnr
      FROM zsdt0131 INTO @DATA(lwa_0131)
     WHERE nro_lote EQ @lwa_0129-nro_lote.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE matkl
      FROM mara INTO @DATA(lva_matkl)
     WHERE matnr EQ @lwa_0131-matnr.

    CHECK sy-subrc EQ 0.

    DATA(is_ok) = zcl_carga_saida_insumos=>check_disparo_carguero( lva_matkl ).

    CHECK is_ok IS NOT INITIAL.

    SELECT SINGLE *
      FROM zlest0181 INTO lwa_zlest0181
     WHERE nro_cg_sai_in EQ i_nro_cg.

    IF lwa_zlest0181 IS NOT INITIAL AND lwa_0133-id_lote_frete IS INITIAL.
      UPDATE ZSDT0133
         SET id_lote_frete = lwa_zlest0181-id_lote_frete
       WHERE nro_cg EQ i_nro_cg.
    ENDIF.

    IF lwa_zlest0181-id_lote_frete IS NOT INITIAL.

      TRY.
          DATA(lv_brgew_kg) = lwa_0133-qtd_total_kg.

        CATCH zcx_error INTO DATA(lwa_cx_error).
          DATA(l_mesg) = lwa_cx_error->get_text( ).
          zcx_error=>zif_error~gera_erro_geral( i_texto = l_mesg ).
      ENDTRY.

      IF lv_brgew_kg IS INITIAL or lwa_0133-dt_canc is NOT INITIAL.
        me->zif_integracao_lote_frete~set_registro( EXPORTING i_id_lote_frete = lwa_zlest0181-id_lote_frete )->set_delete_lote_frete( ).
        EXIT.
      ENDIF.

      TRY .
          me->zif_integracao_lote_frete~set_registro( EXPORTING i_id_lote_frete = lwa_zlest0181-id_lote_frete ).
          me->zif_integracao_lote_frete~at_lote_frete-dt_modifica = sy-datum.
          me->zif_integracao_lote_frete~at_lote_frete-hr_modifica = sy-uzeit.
          me->zif_integracao_lote_frete~at_lote_frete-us_modifica = sy-uname.
          me->zif_integracao_lote_frete~at_lote_frete-sem_oc_opus = 'X'.
        CATCH zcx_integracao.
          e_ck_novo = abap_true.
          me->zif_integracao_lote_frete~at_lote_frete-dt_registro  = sy-datum.
          me->zif_integracao_lote_frete~at_lote_frete-hr_registro  = sy-uzeit.
          me->zif_integracao_lote_frete~at_lote_frete-us_registro  = sy-uname.
          me->zif_integracao_lote_frete~at_lote_frete-sem_oc_opus  = 'X'.
      ENDTRY.
    ELSE.
      e_ck_novo = abap_true.
      me->zif_integracao_lote_frete~at_lote_frete-dt_registro  = sy-datum.
      me->zif_integracao_lote_frete~at_lote_frete-hr_registro  = sy-uzeit.
      me->zif_integracao_lote_frete~at_lote_frete-us_registro  = sy-uname.
      me->zif_integracao_lote_frete~at_lote_frete-sem_oc_opus  = 'X'.
    ENDIF.

    TRY.
        me->zif_integracao_lote_frete~set_new_lote_frete_cg_sai_in( EXPORTING i_nro_cg = i_nro_cg ).

      CATCH zcx_integracao INTO DATA(ex_integracao).
        lv_mesg = ex_integracao->get_text( ).
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = lv_mesg ).
      CATCH zcx_error      INTO DATA(ex_error).
        lv_mesg = ex_error->get_text( ).
        zcx_error=>zif_error~gera_erro_geral( i_texto = lv_mesg ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_gerencia_lote_ov.

    DATA: lc_ordem_venda   TYPE zde_cargueiro_ov.

    r_if_integracao_lote_frete = me.

    e_ck_novo      = abap_false.
    lc_ordem_venda = i_ordem_venda.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(t_matkl_fert)
     WHERE name EQ 'MAGGI_GR_FERTILIZANTES'
       AND low  EQ @lc_ordem_venda-item-matkl.

    IF sy-subrc EQ 0.
      lc_ordem_venda-insumos = abap_true.
    ELSE.
      lc_ordem_venda-insumos = abap_false.
    ENDIF.


    CHECK lc_ordem_venda-item-werks IS NOT INITIAL AND
          lc_ordem_venda-item-matnr IS NOT INITIAL AND
          lc_ordem_venda-vbeln IS NOT INITIAL AND
          lc_ordem_venda-item-posnr IS NOT INITIAL.

    IF lc_ordem_venda-item-id_lote_frete IS INITIAL AND lc_ordem_venda-vbeln IS NOT INITIAL AND lc_ordem_venda-item-posnr IS NOT INITIAL.
      SELECT SINGLE id_lote_frete INTO lc_ordem_venda-item-id_lote_frete
        FROM zlest0181
       WHERE vbeln EQ lc_ordem_venda-vbeln
         AND posnr EQ lc_ordem_venda-item-posnr.

      IF sy-subrc IS INITIAL.
        UPDATE vbap
           SET id_lote_frete = lc_ordem_venda-item-id_lote_frete
         WHERE vbeln EQ lc_ordem_venda-vbeln
           AND posnr EQ lc_ordem_venda-item-posnr.
      ENDIF.
    ENDIF.

    IF lc_ordem_venda-item-id_lote_frete IS NOT INITIAL.

      IF lc_ordem_venda-item-brgew IS INITIAL AND lc_ordem_venda-insumos EQ abap_false.
        me->zif_integracao_lote_frete~set_registro( EXPORTING i_id_lote_frete = lc_ordem_venda-item-id_lote_frete
           )->set_delete_lote_frete(
           ).
        EXIT.
      ENDIF.

      TRY .
          me->zif_integracao_lote_frete~set_registro( EXPORTING i_id_lote_frete = lc_ordem_venda-item-id_lote_frete ).
          me->zif_integracao_lote_frete~at_lote_frete-dt_modifica = sy-datum.
          me->zif_integracao_lote_frete~at_lote_frete-hr_modifica = sy-uzeit.
          me->zif_integracao_lote_frete~at_lote_frete-us_modifica = sy-uname.
        CATCH zcx_integracao.
          e_ck_novo = abap_true.
          me->zif_integracao_lote_frete~at_lote_frete-dt_registro  = sy-datum.
          me->zif_integracao_lote_frete~at_lote_frete-hr_registro  = sy-uzeit.
          me->zif_integracao_lote_frete~at_lote_frete-us_registro  = sy-uname.
      ENDTRY.
    ELSE.
      e_ck_novo = abap_true.
      me->zif_integracao_lote_frete~at_lote_frete-dt_registro  = sy-datum.
      me->zif_integracao_lote_frete~at_lote_frete-hr_registro  = sy-uzeit.
      me->zif_integracao_lote_frete~at_lote_frete-us_registro  = sy-uname.
    ENDIF.

    IF NOT ( ( lc_ordem_venda-item-brgew IS NOT INITIAL ) OR ( lc_ordem_venda-insumos EQ abap_true ) ). "No processo de Insumos, o Lote Embarcador não é deletado, pois pode ficar com a quantidade zerada.

      IF sy-tcode = 'ZSDT0191'.
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = 'Ordem de Venda sem Peso Bruto!' ).
      ENDIF.

      EXIT.
    ENDIF.

    TRY.
        me->zif_integracao_lote_frete~set_new_lote_frete_ov( EXPORTING i_ordem_venda = lc_ordem_venda ).
      CATCH zcx_integracao INTO DATA(ex_integracao).
      CATCH zcx_error      INTO DATA(ex_error).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_gerencia_lote_pc.

    DATA: lc_pedido_compra TYPE zde_cargueiro_pc.

    e_ck_novo        = abap_false.
    lc_pedido_compra = i_pedido_compra.

    IF lc_pedido_compra-item-id_lote_frete IS INITIAL AND
       lc_pedido_compra-item-ebeln         IS NOT INITIAL AND
       lc_pedido_compra-item-ebelp         IS NOT INITIAL.
      SELECT SINGLE id_lote_frete INTO lc_pedido_compra-item-id_lote_frete
        FROM zlest0181
       WHERE ebeln EQ lc_pedido_compra-item-ebeln
         AND ebelp EQ lc_pedido_compra-item-ebelp.

      IF sy-subrc IS INITIAL.
        UPDATE ekpo
           SET id_lote_frete = lc_pedido_compra-item-id_lote_frete
         WHERE ebeln EQ lc_pedido_compra-item-ebeln
           AND ebelp EQ lc_pedido_compra-item-ebelp.
      ENDIF.
    ENDIF.

    IF lc_pedido_compra-item-id_lote_frete IS NOT INITIAL.

      IF lc_pedido_compra-item-brgew IS INITIAL.
        me->zif_integracao_lote_frete~set_registro( EXPORTING i_id_lote_frete = lc_pedido_compra-item-id_lote_frete
           )->set_delete_lote_frete(
           ).
        EXIT.
      ENDIF.

      TRY .
          me->zif_integracao_lote_frete~set_registro( EXPORTING i_id_lote_frete = lc_pedido_compra-item-id_lote_frete ).
          me->zif_integracao_lote_frete~at_lote_frete-dt_modifica = sy-datum.
          me->zif_integracao_lote_frete~at_lote_frete-hr_modifica = sy-uzeit.
          me->zif_integracao_lote_frete~at_lote_frete-us_modifica = sy-uname.
        CATCH zcx_integracao.
          e_ck_novo = abap_true.
          me->zif_integracao_lote_frete~at_lote_frete-dt_registro  = sy-datum.
          me->zif_integracao_lote_frete~at_lote_frete-hr_registro  = sy-uzeit.
          me->zif_integracao_lote_frete~at_lote_frete-us_registro  = sy-uname.
      ENDTRY.
    ELSE.
      e_ck_novo = abap_true.
      me->zif_integracao_lote_frete~at_lote_frete-dt_registro  = sy-datum.
      me->zif_integracao_lote_frete~at_lote_frete-hr_registro  = sy-uzeit.
      me->zif_integracao_lote_frete~at_lote_frete-us_registro  = sy-uname.
    ENDIF.

    IF lc_pedido_compra-item-brgew IS INITIAL.
      IF sy-tcode = 'ZSDT0191'.
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = 'Pedido de Compra sem Peso Bruto!' ).
      ENDIF.
      EXIT.
    ENDIF.

    TRY.
        me->zif_integracao_lote_frete~set_new_lote_frete_pc( EXPORTING i_pedido_compra = lc_pedido_compra ).
      CATCH zcx_integracao INTO DATA(ex_integracao).
      CATCH zcx_error      INTO DATA(ex_error).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_gerencia_lote_sl.

    DATA: lc_solicitacao TYPE zde_cargueiro_sl,
          lwa_zlest0181  TYPE zlest0181,
          lv_mesg        TYPE string.

    e_ck_novo      = abap_false.
    lc_solicitacao = i_solicitacao.

    CLEAR: lwa_zlest0181.

    READ TABLE lc_solicitacao-item INTO DATA(lwa_item_sol_check) INDEX 1.
    CHECK sy-subrc EQ 0 AND lwa_item_sol_check-ebeln IS NOT INITIAL.

    SELECT SINGLE ebeln, matkl
      FROM ekpo INTO @DATA(lwa_ekpo)
     WHERE ebeln EQ @lwa_item_sol_check-ebeln
       AND ebelp EQ @lwa_item_sol_check-ebelp.

    CHECK sy-subrc EQ 0.

* "// Verifica se esta apto para realizar o Disparo ao Carguero
    "// US-177784 WBARBOSA 09/05/2025
    CALL METHOD zcl_solicitacao_entrada_insumo=>check_disparo_carguero
      EXPORTING
        i_matkl = lwa_ekpo-matkl
      RECEIVING
        is_ok   = DATA(is_ok).

    CHECK is_ok IS NOT INITIAL.
    "// US-177784 WBARBOSA 09/05/2025

    SELECT SINGLE *
      FROM zlest0181 INTO lwa_zlest0181
     WHERE nro_sol EQ lwa_item_sol_check-nro_sol.

    IF lwa_zlest0181 IS NOT INITIAL.
      LOOP AT lc_solicitacao-item INTO DATA(lwa_item_sol) WHERE id_lote_frete IS INITIAL.
        UPDATE zmmt0196
           SET id_lote_frete = lwa_zlest0181-id_lote_frete
         WHERE nro_sol EQ lwa_item_sol-nro_sol
           AND seq     EQ lwa_item_sol-seq.
      ENDLOOP.
    ENDIF.

    IF lwa_zlest0181-id_lote_frete IS NOT INITIAL.

      TRY.
         DATA(lv_brgew_kg) = zcl_solicitacao_entrada_insumo=>get_solicitacao_qtde( i_nro_sol  = lwa_item_sol_check-nro_sol
                                                                                   i_tp_saldo = 'C' ).
      CATCH zcx_error INTO DATA(lwa_cx_error).
          DATA(l_mesg) = lwa_cx_error->get_text( ).
          zcx_error=>zif_error~gera_erro_geral( i_texto = l_mesg ).
      ENDTRY.

      IF lv_brgew_kg is INITIAL.
        me->zif_integracao_lote_frete~set_registro( EXPORTING i_id_lote_frete = lc_solicitacao-id_lote_frete
           )->set_delete_lote_frete(
           ).
        EXIT.
      ENDIF.

      TRY .
          me->zif_integracao_lote_frete~set_registro( EXPORTING i_id_lote_frete = lc_solicitacao-id_lote_frete ).
          me->zif_integracao_lote_frete~at_lote_frete-dt_modifica = sy-datum.
          me->zif_integracao_lote_frete~at_lote_frete-hr_modifica = sy-uzeit.
          me->zif_integracao_lote_frete~at_lote_frete-us_modifica = sy-uname.
          "BS #169676 - inicio
          me->zif_integracao_lote_frete~at_lote_frete-sem_oc_opus                       = 'X'.
          "BS #169676 - fim
        CATCH zcx_integracao.
          e_ck_novo = abap_true.
          me->zif_integracao_lote_frete~at_lote_frete-dt_registro  = sy-datum.
          me->zif_integracao_lote_frete~at_lote_frete-hr_registro  = sy-uzeit.
          me->zif_integracao_lote_frete~at_lote_frete-us_registro  = sy-uname.
          "BS #169676 - inicio
          me->zif_integracao_lote_frete~at_lote_frete-sem_oc_opus                       = 'X'.
          "BS #169676 - fim
      ENDTRY.
    ELSE.
      e_ck_novo = abap_true.
      me->zif_integracao_lote_frete~at_lote_frete-dt_registro  = sy-datum.
      me->zif_integracao_lote_frete~at_lote_frete-hr_registro  = sy-uzeit.
      me->zif_integracao_lote_frete~at_lote_frete-us_registro  = sy-uname.
      "BS #169676 - inicio
      me->zif_integracao_lote_frete~at_lote_frete-sem_oc_opus                       = 'X'.
      "BS #169676 - fim
    ENDIF.

    TRY.
        me->zif_integracao_lote_frete~set_new_lote_frete_sl( EXPORTING i_solicitacao = lc_solicitacao ).

      CATCH zcx_integracao INTO DATA(ex_integracao).
        lv_mesg = ex_integracao->get_text( ).
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = lv_mesg ).
      CATCH zcx_error      INTO DATA(ex_error).
        lv_mesg = ex_error->get_text( ).
        zcx_error=>zif_error~gera_erro_geral( i_texto = lv_mesg ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_new_lote_frete_sl.

    DATA: lc_pedido          TYPE zde_cargueiro_pc,
          wa_j_1bbranch      TYPE  j_1bbranch,
          bsart_range        TYPE RANGE OF bsart,
          bsart_sem_troca_nf TYPE RANGE OF bsart,
          matkl_fertilizante TYPE RANGE OF matkl,
          lv_brgew_kg        TYPE brgew_ap,
          lv_forn            TYPE lifnr,
          l_mesg             TYPE string.

    r_if_integracao_lote_frete = me.

    DATA(lc_solicitacao) = i_solicitacao.

    CLEAR lv_brgew_kg.

    READ TABLE lc_solicitacao-item INTO DATA(lwa_item_sol_check) INDEX 1.
    CHECK sy-subrc EQ 0 AND lwa_item_sol_check-ebeln IS NOT INITIAL.

*-----------------------------------------------
*-- pedido de compras
*-----------------------------------------------
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF lc_pedido
      FROM ekko
     WHERE ebeln = lwa_item_sol_check-ebeln.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF lc_pedido-item
      FROM ekpo
     WHERE ebeln = lwa_item_sol_check-ebeln
       AND ebelp = lwa_item_sol_check-ebelp.

    SELECT SINGLE *
      from zlest0181 INTO @DATA(lwa_zlest0181)
      WHERE nro_sol = @lc_solicitacao-nro_sol.

    IF lwa_zlest0181-id_carguero is INITIAL.
      zcl_solicitacao_entrada_insumo=>check_tipo_frete(
        EXPORTING
          i_werks  = lc_solicitacao-werks
        IMPORTING
          e_inco1  = DATA(lv_inco1_lote_frete)
          e_agente = DATA(lv_agente_frete)
      ).
    else.
      lv_inco1_lote_frete = lwa_zlest0181-tp_frete.
      lv_agente_frete     = lwa_zlest0181-id_parceiro_frete.
    ENDIF.

    me->zif_integracao_lote_frete~at_lote_frete-nro_sol = lc_solicitacao-nro_sol.
    me->zif_integracao_lote_frete~at_lote_frete-matkl   = lc_pedido-item-matkl.
    me->zif_integracao_lote_frete~at_lote_frete-kvgr4   = lc_pedido-item-zkvgr4.

*----------------------------------------------
* Somente envia pedido de grãos e derivados ao carguero
*----------------------------------------------

    TRY.
        lv_brgew_kg = zcl_solicitacao_entrada_insumo=>get_solicitacao_qtde( i_nro_sol  = lwa_item_sol_check-nro_sol
                                                                            i_tp_saldo = 'C' ).
      CATCH zcx_error INTO DATA(ex_error).
        l_mesg = ex_error->get_text( ).
        zcx_error=>zif_error~gera_erro_geral( i_texto = l_mesg ).
    ENDTRY.

    CHECK lv_brgew_kg > 0.

    me->zif_integracao_lote_frete~at_lote_frete-gewei = 'KG'.
    me->zif_integracao_lote_frete~at_lote_frete-brgew = lv_brgew_kg.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = lc_solicitacao-werks
      IMPORTING
        wa_j_1bbranch        = wa_j_1bbranch
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lc_texto).
      zcx_integracao=>zif_error~gera_erro_geral( i_texto = lc_texto ).
    ENDIF.

    me->zif_integracao_lote_frete~at_lote_frete-bukrs  = wa_j_1bbranch-bukrs.
    me->zif_integracao_lote_frete~at_lote_frete-branch = wa_j_1bbranch-branch.

*----------------------------------------------
*   "Verificar Depara Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*----------------------------------------------
    me->zif_integracao_lote_frete~get_id_material_carguero(
      EXPORTING
        i_bukrs                = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
        i_matkl                = CONV #( me->zif_integracao_lote_frete~at_lote_frete-matkl )
      IMPORTING
        e_id_material_carguero = DATA(e_id_material_carguero) ).

*----------------------------------------------
*---Verificar Depara Unidade Organizacional """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*----------------------------------------------
    me->zif_integracao_lote_frete~get_id_uni_org_carguero(
      EXPORTING
        i_bukrs           = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
        i_branch          = CONV #( me->zif_integracao_lote_frete~at_lote_frete-branch )
        i_unidade_maggi   = CONV #( me->zif_integracao_lote_frete~at_lote_frete-kvgr4 )
      IMPORTING
        e_id_ung_carguero = DATA(e_id_ung_carguero) ).

*---------------------------------------------------------
*    "Remetente
*---------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_solicitacao-parceiro_pc
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

*---------------------------------------------------------
*    "Destinatário
*---------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_solicitacao-werks
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_dest = zif_integracao_lote_frete=>at_tp_parceiro_cliente.

*---------------------------------------------------------
*    "Local de Coleta
*---------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_solicitacao-parceiro_pc
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

*---------------------------------------------------------
*    "Local de Entrega / descarga
*---------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_solicitacao-parceiro_le
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

*    "Troca Nota
*    "Indica que o carregamento será feito em terceiro (embarque em terceiro) e que o Agente de Embarque
*    "(pessoa responsável por informar o carregamento, através de um tablet) deverá enviar a nota fiscal
*    "do produtor para que possa ser feito a entrada da nota.

    me->zif_integracao_lote_frete~at_lote_frete-ck_troca_nota = abap_true.  "Temporario para Primeira onda - Projeto Renova Insumos.
*
*    "Embarque em Terceiro
*    "Segue as mesmas características do Troca-Notas, porém o Agente de Embarque não é
*    "obrigado a informar a nota fiscal do produtor.
*
    me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro = abap_false.
    me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro = abap_false.
*
*    "CIF ou FOB
*    "Indica se o lote se trata de uma Venda FOB. Ou seja, não será o embarcador que pagará o
*    "frete. Se esta flag estiver marcada, no momento da criação do lote o embarcador não é
*    "obrigado informar o valor do frete empresa. Em cenários de venda FOB, o destinatário do
*    "lote poderá operar o sistema (quando possuir tenant cadastrado).
*
    CASE lv_inco1_lote_frete.
      WHEN zif_carga=>st_tp_frete_cif OR zif_carga=>st_tp_frete_cpt.
        "Amaggi Paga Frete
        me->zif_integracao_lote_frete~at_lote_frete-tp_frete = lv_inco1_lote_frete.

      WHEN zif_carga=>st_tp_frete_cfr OR zif_carga=>st_tp_frete_fob.
        "CFR / FOB
        "Amaggi não paga frete
        me->zif_integracao_lote_frete~at_lote_frete-tp_frete = zif_carga=>st_tp_frete_fob.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = zcx_integracao=>zcx_erro_ov_incotermo-msgid
                              msgno = zcx_integracao=>zcx_erro_ov_incotermo-msgno
                              attr1 = CONV #( lc_pedido-item-inco1 ) )
            msgid  = zcx_integracao=>zcx_erro_ov_incotermo-msgid
            msgno  = zcx_integracao=>zcx_erro_ov_incotermo-msgno
            msgty  = 'E'
            msgv1  = CONV #( lc_pedido-item-inco1 ).
    ENDCASE.

    CASE lv_inco1_lote_frete.
      WHEN zif_carga=>st_tp_frete_cif.
         "Transportadora Definida (Transportadora Própria)
          me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete = lv_agente_frete.
          me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_frete = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

          IF me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete IS NOT INITIAL.
            TRY .
                zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete
                  )->ck_parceiro_local_negocio(
                  )->ck_parceiro_intercompany( i_empresa = me->zif_integracao_lote_frete~at_lote_frete-bukrs
                  ).
              CATCH zcx_parceiros INTO DATA(ex_parceiros).    "
                zcx_integracao=>zif_error~gera_erro_geral( i_texto = ex_parceiros->get_text( ) ).
            ENDTRY.
          ENDIF.
      WHEN zif_carga=>st_tp_frete_cpt OR
           zif_carga=>st_tp_frete_cfr OR
           zif_carga=>st_tp_frete_fob..
        "Passar sem Parceiro de Frete Definido
      WHEN OTHERS.
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = 'Agente de Frete tem que ser SIM!' ).
    ENDCASE.
*
*    "Paga Trecho Terceiro
*    "Indica se é ou não obrigatório informar o valor do frete empresa para o trecho terceiro
*    "(trecho entre a filial do embarcador e o armazém de terceiro).
    me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.

    CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme.
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
          )->get_txjcd( IMPORTING e_txjcd = DATA(e_txjcd_reme)
          ).
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
          )->get_txjcd( IMPORTING e_txjcd = e_txjcd_reme
          ).
    ENDCASE.
*
    CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole.
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
          )->get_txjcd( IMPORTING e_txjcd = DATA(e_txjcd_cole)
          ).
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
          )->get_txjcd( IMPORTING e_txjcd = e_txjcd_cole
          ).
    ENDCASE.
*
*    "Emitir NF de trânsito
*    "Indica que o motorista deverá solicitar a emissão da nota fiscal de trânsito antes de partir
*    "para o local de coleta. O motorista não poderá comparecer ao local de coleta sem a NF de trânsito
*    "ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_EMITE_NF_TRANSITO = COND STRING( WHEN E_TXJCD_REME EQ E_TXJCD_COLE THEN ABAP_FALSE ELSE ABAP_TRUE ) .
    me->zif_integracao_lote_frete~at_lote_frete-ck_emite_nf_transito = abap_false.

*    IF     lc_pedido-item-zckfreteent = abap_false.
*      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.
*    ELSEIF lc_pedido-item-zckfreteent = abap_true.
*      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_true.
*    ELSE.
*      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.
*    ENDIF.

*    "Mercadoria de Exportação
    me->zif_integracao_lote_frete~at_lote_frete-ck_exportacao = abap_false.

    "BS #169676 - inicio
    "Não terá Ordem de Carregamento via sistema OPUS
    me->zif_integracao_lote_frete~at_lote_frete-sem_oc_opus = abap_true.

    "BS #169676 - fim
  ENDMETHOD.


  METHOD zif_integracao_lote_frete~set_new_lote_frete_cg_sai_in.

    DATA: lc_pedido          TYPE zde_cargueiro_pc,
          wa_j_1bbranch      TYPE  j_1bbranch,
          bsart_range        TYPE RANGE OF bsart,
          bsart_sem_troca_nf TYPE RANGE OF bsart,
          matkl_fertilizante TYPE RANGE OF matkl,
          lv_brgew_kg        TYPE brgew_ap,
          lv_forn            TYPE lifnr,
          l_mesg             TYPE string.

    r_if_integracao_lote_frete = me.

    CLEAR lv_brgew_kg.

    SELECT SINGLE *
      FROM zsdt0133 INTO @DATA(lwa_0133)
     WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0 AND lwa_0133-integrar_carguero EQ abap_true.

    SELECT SINGLE *
      FROM zsdt0129 INTO @DATA(lwa_0129)
     WHERE nro_cg EQ @i_nro_cg.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE vbeln, matnr, werks, cod_loc_emb
      FROM zsdt0131 INTO @DATA(lwa_0131)
     WHERE nro_lote EQ @lwa_0129-nro_lote.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zsdt0130 INTO @DATA(lwa_0130_last_sequencia)
     WHERE nro_lote    EQ @lwa_0129-nro_lote
       AND seq_entrega EQ ( SELECT MAX( seq_entrega )
                              FROM zsdt0130
                             WHERE nro_lote EQ @lwa_0129-nro_lote ).

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zsdt0132 INTO @DATA(lwa_0132)
     WHERE nr_rot EQ @lwa_0131-cod_loc_emb.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(lva_branch_ov)
     WHERE branch EQ @lwa_0131-werks.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM vbak INTO @DATA(lwa_vbak)
     WHERE vbeln EQ @lwa_0131-vbeln.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE matkl
      FROM mara INTO @DATA(lva_matkl)
     WHERE matnr EQ @lwa_0131-matnr.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(lwa_branch_matriz)
     WHERE bukrs      EQ @lva_branch_ov-bukrs
       AND branch     NE '0001'
       AND cgc_branch EQ '0001'.

    CHECK sy-subrc EQ 0.

    DATA(lv_inco1_lote_frete) = lwa_0129-inco1.

    me->zif_integracao_lote_frete~at_lote_frete-nro_cg_sai_in = i_nro_cg.
    me->zif_integracao_lote_frete~at_lote_frete-matkl         = lva_matkl.
    me->zif_integracao_lote_frete~at_lote_frete-kvgr4         = lwa_vbak-kvgr4.

    lv_brgew_kg = lwa_0133-qtd_total_kg.

    CHECK lv_brgew_kg > 0.

    me->zif_integracao_lote_frete~at_lote_frete-gewei = 'KG'.
    me->zif_integracao_lote_frete~at_lote_frete-brgew = lv_brgew_kg.

    me->zif_integracao_lote_frete~at_lote_frete-bukrs  = lwa_branch_matriz-bukrs.
    me->zif_integracao_lote_frete~at_lote_frete-branch = lwa_branch_matriz-branch.

*----------------------------------------------
*   "Verificar Depara Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*----------------------------------------------
    me->zif_integracao_lote_frete~get_id_material_carguero(
      EXPORTING
        i_bukrs                = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
        i_matkl                = CONV #( me->zif_integracao_lote_frete~at_lote_frete-matkl )
      IMPORTING
        e_id_material_carguero = DATA(e_id_material_carguero) ).

*----------------------------------------------
*---Verificar Depara Unidade Organizacional """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*----------------------------------------------
    me->zif_integracao_lote_frete~get_id_uni_org_carguero(
      EXPORTING
        i_bukrs           = CONV #( me->zif_integracao_lote_frete~at_lote_frete-bukrs )
        i_branch          = CONV #( me->zif_integracao_lote_frete~at_lote_frete-branch )
        i_unidade_maggi   = CONV #( me->zif_integracao_lote_frete~at_lote_frete-kvgr4 )
      IMPORTING
        e_id_ung_carguero = DATA(e_id_ung_carguero) ).

*---------------------------------------------------------
*    "Remetente
*---------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_branch_matriz-branch
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

*---------------------------------------------------------
*    "Destinatário
*---------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_branch_matriz-branch
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_dest.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_dest = zif_integracao_lote_frete=>at_tp_parceiro_cliente.

*---------------------------------------------------------
*    "Local de Coleta
*---------------------------------------------------------
    "SD - Ajuste Local Coleta Lote Carga Insumos US 191766 - WPP
    IF lwa_0132-lifnr_rot IS NOT INITIAL.
      lwa_0132-lifnr = lwa_0132-lifnr_rot.
    ENDIF.
    "SD - Ajuste Local Coleta Lote Carga Insumos US 191766 - WPP

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_0132-lifnr
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.

*---------------------------------------------------------
*    "Local de Entrega / descarga
*---------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_0130_last_sequencia-kunnr
      IMPORTING
        output = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_entr.

    me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_entr = zif_integracao_lote_frete=>at_tp_parceiro_cliente.

*    "Troca Nota
*    "Indica que o carregamento será feito em terceiro (embarque em terceiro) e que o Agente de Embarque
*    "(pessoa responsável por informar o carregamento, através de um tablet) deverá enviar a nota fiscal
*    "do produtor para que possa ser feito a entrada da nota.

    DATA(_embarque_luft) = zcl_carga_saida_insumos=>get_embarque_luft( i_nro_cg = i_nro_cg ).

    IF _embarque_luft EQ abap_true.
      me->zif_integracao_lote_frete~at_lote_frete-ck_troca_nota = abap_false.
    ELSE.
      me->zif_integracao_lote_frete~at_lote_frete-ck_troca_nota = abap_true.
    ENDIF.
*
*    "Embarque em Terceiro
*    "Segue as mesmas características do Troca-Notas, porém o Agente de Embarque não é
*    "obrigado a informar a nota fiscal do produtor.
*
    me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro = abap_false.
    me->zif_integracao_lote_frete~at_lote_frete-ck_embarque_terceiro = abap_false.
*
*    "CIF ou FOB
*    "Indica se o lote se trata de uma Venda FOB. Ou seja, não será o embarcador que pagará o
*    "frete. Se esta flag estiver marcada, no momento da criação do lote o embarcador não é
*    "obrigado informar o valor do frete empresa. Em cenários de venda FOB, o destinatário do
*    "lote poderá operar o sistema (quando possuir tenant cadastrado).
*
    CASE lv_inco1_lote_frete.
      WHEN zif_carga=>st_tp_frete_cif OR zif_carga=>st_tp_frete_cpt.
        "Amaggi Paga Frete
        me->zif_integracao_lote_frete~at_lote_frete-tp_frete = lv_inco1_lote_frete.

      WHEN zif_carga=>st_tp_frete_cfr OR zif_carga=>st_tp_frete_fob.
        "CFR / FOB
        "Amaggi não paga frete
        me->zif_integracao_lote_frete~at_lote_frete-tp_frete = zif_carga=>st_tp_frete_fob.

      WHEN OTHERS.
*"// US-169528 WBARBOSA 24/07/2025 INICIO
* Definição de Frete Será escolhido pela Carguero
*        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ERRO_OV_INCOTERMO-MSGID
*                              MSGNO = ZCX_INTEGRACAO=>ZCX_ERRO_OV_INCOTERMO-MSGNO
*                              ATTR1 = CONV #( LC_PEDIDO-ITEM-INCO1 ) )
*            MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_OV_INCOTERMO-MSGID
*            MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_OV_INCOTERMO-MSGNO
*            MSGTY  = 'E'
*            MSGV1  = CONV #( LC_PEDIDO-ITEM-INCO1 ).
*"// US-169528 WBARBOSA 24/07/2025 FIM
    ENDCASE.

    CASE lv_inco1_lote_frete.
      WHEN zif_carga=>st_tp_frete_cif.
        "Transportadora Definida (Transportadora Própria)
*        me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete = lv_agente_frete.
*        me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_frete = zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
*
*        IF me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete IS NOT INITIAL.
*          TRY .
*              zcl_fornecedores=>zif_parceiros~get_instance(
*                )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_frete
*                )->ck_parceiro_local_negocio(
*                )->ck_parceiro_intercompany( i_empresa = me->zif_integracao_lote_frete~at_lote_frete-bukrs
*                ).
*            CATCH zcx_parceiros INTO DATA(ex_parceiros).    "
*              zcx_integracao=>zif_error~gera_erro_geral( i_texto = ex_parceiros->get_text( ) ).
*          ENDTRY.
*        ENDIF.
      WHEN zif_carga=>st_tp_frete_cpt OR
           zif_carga=>st_tp_frete_cfr OR
           zif_carga=>st_tp_frete_fob.
        "Passar sem Parceiro de Frete Definido
      WHEN OTHERS.
*        zcx_integracao=>zif_error~gera_erro_geral( i_texto = 'Agente de Frete tem que ser SIM!' ).  "// US-169528 WBARBOSA 24/07/2025
*        "//Definição de Frete Será escolhido pela Carguero
    ENDCASE.
*
*    "Paga Trecho Terceiro
*    "Indica se é ou não obrigatório informar o valor do frete empresa para o trecho terceiro
*    "(trecho entre a filial do embarcador e o armazém de terceiro).
    me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.

    CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_reme.
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
          )->get_txjcd( IMPORTING e_txjcd = DATA(e_txjcd_reme)
          ).
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_reme
          )->get_txjcd( IMPORTING e_txjcd = e_txjcd_reme
          ).
    ENDCASE.
*
    CASE me->zif_integracao_lote_frete~at_lote_frete-tp_parceiro_cole.
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
          )->get_txjcd( IMPORTING e_txjcd = DATA(e_txjcd_cole)
          ).
      WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
        zcl_clientes=>zif_parceiros~get_instance(
          )->set_parceiro( i_parceiro = me->zif_integracao_lote_frete~at_lote_frete-id_parceiro_cole
          )->get_txjcd( IMPORTING e_txjcd = e_txjcd_cole
          ).
    ENDCASE.
*
*    "Emitir NF de trânsito
*    "Indica que o motorista deverá solicitar a emissão da nota fiscal de trânsito antes de partir
*    "para o local de coleta. O motorista não poderá comparecer ao local de coleta sem a NF de trânsito
*    "ME->ZIF_INTEGRACAO_LOTE_FRETE~AT_LOTE_FRETE-CK_EMITE_NF_TRANSITO = COND STRING( WHEN E_TXJCD_REME EQ E_TXJCD_COLE THEN ABAP_FALSE ELSE ABAP_TRUE ) .
    me->zif_integracao_lote_frete~at_lote_frete-ck_emite_nf_transito = abap_false.

*    IF     lc_pedido-item-zckfreteent = abap_false.
*      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.
*    ELSEIF lc_pedido-item-zckfreteent = abap_true.
*      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_true.
*    ELSE.
*      me->zif_integracao_lote_frete~at_lote_frete-ck_paga_trecho_terceiro = abap_false.
*    ENDIF.

*    "Mercadoria de Exportação
    me->zif_integracao_lote_frete~at_lote_frete-ck_exportacao = abap_false.

    "BS #169676 - inicio
    "Não terá Ordem de Carregamento via sistema OPUS
    me->zif_integracao_lote_frete~at_lote_frete-sem_oc_opus = abap_true.

    "BS #169676 - fim

  ENDMETHOD.
ENDCLASS.
