class ZCL_INTEGRACAO_GRC_NEW_NFE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_GRC_NEW_NFE .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_GRC_NEW_NFE IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_grc_new_doc.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_referencia-tp_referencia = 'NFE_SAP_GRC'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE~GET_INSTANCE.

    IF ZIF_INTEGRACAO_GRC_NEW_NFE~AT_IF_INTEGRACAO_GRC_NEW_NFE IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_GRC_NEW_NFE~AT_IF_INTEGRACAO_GRC_NEW_NFE TYPE ZCL_INTEGRACAO_GRC_NEW_NFE.
    ENDIF.
    R_IF_INTEGRACAO_GRC_NEW_NFE = ZIF_INTEGRACAO_GRC_NEW_NFE~AT_IF_INTEGRACAO_GRC_NEW_NFE.

  ENDMETHOD.


  METHOD zif_integracao_grc_new_nfe~get_limite_fiscal.

    e_limite = me->zif_integracao_grc_new_nfe~at_limite_fiscal.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE~GET_VALIDAR_DADOS.

    DATA: WA_ZFIWRT0009 TYPE LINE OF ZFIWRT0009_T.

    R_IF_INTEGRACAO_GRC_NEW_NFE = ME.

*
*    CLEAR: E_ZFIWRT0008,
*           E_ZFIWRT0009.
*
*    "Pesquisando Empresas e Filiais """"""""""""""""""""""""""""""""""""""""""
*    ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
*      )->SET_PARCEIRO_CNPJ_CPF_IE(
*            EXPORTING
*              I_CNPJ = CONV #( I_NFE-PROCESSO-INTGNFE-EMIT-CNPJ )
*              I_INSC_ESTATUAL = CONV #( I_NFE-PROCESSO-INTGNFE-EMIT-IE )
*      )->CK_PARCEIRO_LOCAL_NEGOCIO(
*            IMPORTING E_J_1BBRANCH  = DATA(E_J_1BBRANCH)
*      ).
*
*    "I_CABECALHO-OPERACAO = ??
*    E_ZFIWRT0008-BUKRS  = E_J_1BBRANCH-BUKRS.
*    E_ZFIWRT0008-BRANCH = E_J_1BBRANCH-BRANCH.
*
*    "Pesquisando Parceiro
*
*    ZCL_CLIENTES=>ZIF_PARCEIROS~GET_INSTANCE(
*      )->SET_PARCEIRO_CNPJ_CPF_IE(
*      EXPORTING
*        I_CNPJ             = CONV #( I_NFE-PROCESSO-INTGNFE-DEST-CNPJ )    " Code CGC
*        I_CPF              = CONV #( I_NFE-PROCESSO-INTGNFE-DEST-CPF )     " NºCPF
*        I_INSC_ESTATUAL    = CONV #( I_NFE-PROCESSO-INTGNFE-DEST-IE )     " Nº identificação fiscal 3
*      )->CK_ATIVO(
*      )->CK_ATIVO_EMPRESA( I_EMPRESA = E_J_1BBRANCH-BUKRS
*      )->GET_ID_PARCEIRO(
*      IMPORTING
*        E_PARCEIRO    = DATA(E_PARCEIRO)    " Identificação do parceiro (cliente, fornecedor, loc.negócio)
*      ).
*
*    E_ZFIWRT0008-PARID = E_PARCEIRO.
*
*    E_ZFIWRT0008-MOVE_PLANT = E_J_1BBRANCH-BRANCH.   "Centro Destino
*
*    "WL_ZFIWRT0008-MOVE_STLOC = WA_SAIDA_0100-LGORT_D.   "Deposito Destino
*
*    "Validar Padrão de Data
*    FIND REGEX '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' IN I_NFE-PROCESSO-INTGNFE-IDE-DHEMI(10).
*    IF SY-SUBRC IS NOT INITIAL.
*      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGID
*                            MSGNO = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGNO
*                            ATTR1 = 'Data Emissão' )
*          MSGID  = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGID
*          MSGNO  = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGNO
*          MSGTY  = 'E'
*          MSGV1  = 'Data Emissão'.
*    ENDIF.
*
*    E_ZFIWRT0008-BUDAT = SY-DATUM.
*    E_ZFIWRT0008-BLDAT = I_NFE-PROCESSO-INTGNFE-IDE-DHEMI(4) + I_NFE-PROCESSO-INTGNFE-IDE-DHEMI+5(2) + I_NFE-PROCESSO-INTGNFE-IDE-DHEMI+8(2).
*
*    "E_ZFIWRT0008-INCO1 = 'CIF'.
*    "E_ZFIWRT0008-INCO2 = 'CIF'.
*
*    "E_ZFIWRT0008-MOVE_BATCH      = WA_SAIDA_0100-CHARG.
*    "E_ZFIWRT0008-MOVE_MAT        = WA_SAIDA_0100-MATNR.
*
*    LOOP AT I_NFE-PROCESSO-INTGNFE-DET INTO DATA(WA_DTE).
*      CLEAR: WA_ZFIWRT0009.
*
*      WA_ZFIWRT0009-MATNR = WA_DTE-PROD-CPROD.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WA_ZFIWRT0009-MATNR
*        IMPORTING
*          OUTPUT = WA_ZFIWRT0009-MATNR.
*
*      SELECT SINGLE * FROM MARA INTO @DATA(WA_MARA) WHERE MATNR EQ @WA_ZFIWRT0009-MATNR.
*      IF SY-SUBRC IS NOT INITIAL.
*        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_MATERIAL_NAO_ENCONTRADO-MSGID
*                              MSGNO = ZCX_INTEGRACAO=>ZCX_MATERIAL_NAO_ENCONTRADO-MSGNO
*                              ATTR1 = WA_DTE-PROD-CPROD )
*            MSGID  = ZCX_INTEGRACAO=>ZCX_MATERIAL_NAO_ENCONTRADO-MSGID
*            MSGNO  = ZCX_INTEGRACAO=>ZCX_MATERIAL_NAO_ENCONTRADO-MSGNO
*            MSGTY  = 'E'
*            MSGV1  = CONV #( WA_DTE-PROD-CPROD ).
*      ENDIF.
*
*      WA_ZFIWRT0009-BWKEY = E_J_1BBRANCH-BRANCH.
*      WA_ZFIWRT0009-LGORT = E_ZFIWRT0008-MOVE_STLOC.
*      WA_ZFIWRT0009-MEINS = WA_MARA-MEINS.
*      WA_ZFIWRT0009-MENGE = WA_DTE-PROD-QCOM.
*      WA_ZFIWRT0009-NETWR = WA_DTE-PROD-VPROD.
*      WA_ZFIWRT0009-CHARG = E_ZFIWRT0008-MOVE_BATCH.
*
*      IF WA_ZFIWRT0009-MENGE > 0.
*        WA_ZFIWRT0009-NETPR  = WA_ZFIWRT0009-NETWR / WA_ZFIWRT0009-MENGE.
*      ENDIF.
*
*      APPEND WA_ZFIWRT0009 TO E_ZFIWRT0009.
*
*    ENDLOOP.
*
*
*    "Senão achar, gerar a nota fiscal
*    ME->AT_NF_WRITER = ZCL_NF_WRITER=>ZIF_NF_WRITER~GET_INSTANCE(
*      )->SET_CABECALHO( I_CABECALHO = E_ZFIWRT0008
*      ).
*
*    "Incluindo Itens
*    LOOP AT E_ZFIWRT0009 INTO WA_ZFIWRT0009.
*      ME->AT_NF_WRITER->ADD_ITEM( I_ITEM = WA_ZFIWRT0009 ).
*    ENDLOOP.
*
*    "Documentos Referenciados.
**        LOOP AT IT_ZSDT_RETLOTE INTO DATA(WL_ZSDT_RETLOTE).
**          ZCL_NF_WRITER=>ZIF_NF_WRITER~GET_INSTANCE( )->ADD_DOC_REF( I_DOCNUM =  WL_ZSDT_RETLOTE-DOCNUM ).
**        ENDLOOP.
*
*    ME->AT_NF_WRITER->VALIDAR_REGISTRO( ).

  ENDMETHOD.


  method zif_integracao_grc_new_nfe~set_criar_nf_propria.

    r_if_integracao_grc_new_nfe = me.

    data: t_element_array	     type zde_element_array_t,
          lwa_zsdt0231_proc    type zsdt0231_proc,
          l_sucesso            type char1,
          l_msg_erro           type string,
          lva_msg_aux          type c length 200,
          lva_registro_in_proc type char1.

    "Variaveis para a criação do documento fiscal do fornecedor US172357
    data: sl_header     type bapi_j_1bnfdoc,
          sl_header_add type bapi_j_1bnfdoc_add,
          sl_nfcheck    type bapi_j_1bnfcheck,
          tl_partner    type table of bapi_j_1bnfnad,
          wl_partner    type bapi_j_1bnfnad,
          tl_item       type table of bapi_j_1bnflin,
          sl_item       type bapi_j_1bnflin,
          tl_item_add   type table of bapi_j_1bnflin_add,
          wl_item_add   type bapi_j_1bnflin_add,
          tl_item_tax   type table of bapi_j_1bnfstx,
          wl_item_tax   type bapi_j_1bnfstx,
          tl_msg        type table of bapi_j_1bnfftx,
          wl_msg        type bapi_j_1bnfftx,
          tl_dados_adic type table of logbr_nf_texts_compatibility,
          vl_docnum     type j_1bnfdoc-docnum,
          tl_return     type table of bapiret2.
    "Variaveis para a criação do documento fiscal do fornecedor US172357

    free: e_documento.

    l_sucesso                 = abap_false.
    lva_registro_in_proc      = abap_false.

    lwa_zsdt0231_proc-obj_key = i_dados-processo-infosistemaorigem-id.
    insert zsdt0231_proc   from lwa_zsdt0231_proc.

    if sy-subrc eq 0.
      commit work.
      call function 'ENQUEUE_EZSDT0231_PROC'
        exporting
          obj_key        = lwa_zsdt0231_proc-obj_key
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.
      if sy-subrc <> 0.
        lva_registro_in_proc = abap_true.
      endif.
    else.
      call function 'ENQUEUE_EZSDT0231_PROC'
        exporting
          obj_key        = lwa_zsdt0231_proc-obj_key
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.

      if sy-subrc <> 0.
        lva_registro_in_proc = abap_true.
      endif.
    endif.

    if lva_registro_in_proc eq abap_true.
      raise exception type zcx_error
        exporting
          textid = value #( msgid = zcx_error=>zcx_erro_geral-msgid
                            msgno = zcx_error=>zcx_erro_geral-msgno
                            attr1 = conv #( 'Planilha' )
                            attr2 = conv #( lwa_zsdt0231_proc-obj_key )
                            attr3 = conv #( 'em processamento! Tente novamente mais tarde!' ) )
          msgty  = 'E'
          msgid  = zcx_error=>zcx_erro_geral-msgid
          msgno  = zcx_error=>zcx_erro_geral-msgno
          msgv1  = conv #( 'Planilha' )
          msgv2  = conv #( lwa_zsdt0231_proc-obj_key )
          msgv3  = conv #( 'em processamento! Tente novamente mais tarde!' ).
    endif.

    select single *
      into @me->zif_integracao_grc_new_nfe~at_zsdt0231
      from zsdt0231
     where obj_key eq @i_dados-processo-infosistemaorigem-id.

    if sy-subrc is initial and me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum is not initial.
      select single *
        into @data(wa_j_1bnfdoc)
        from j_1bnfdoc
       where docnum eq @me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum.

      if sy-subrc is not initial.
        delete from zsdt0231
              where obj_key eq i_dados-processo-infosistemaorigem-id
                and obj_key ne space.
        sy-subrc = 1.
      endif.
    endif.

    if sy-subrc is initial.
      select single *
        into @me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
        from j_1bnfdoc
       where docnum eq @me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum.

      select single *
        into @me->zif_integracao_grc_new_nfe~at_j_1bnfe_active
        from j_1bnfe_active
       where docnum eq @me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum.

      l_sucesso = abap_true.
    endif.

*---------------------------------------
*-- busca o limite de cada modelo Fiscal IR054082
*---------------------------------------
    call method me->zif_integracao_grc_new_nfe~set_limite_fiscal
      exporting
        i_model = '55'.    " Modelo da nota fiscal

    if l_sucesso ne abap_true.
*---------------------------------------
*---- Validações - Ini
*---------------------------------------
      try .
*---------------------------------------
*-------- Nota Fiscal Eletrônica
*---------------------------------------
          loop at i_dados-processo-intgnfe-ide-nfref into data(wa_nfref)
               where refnfe is not initial
                 and refcte is initial.

            if strlen( wa_nfref-refnfe ) ne 44.
              raise exception type zcx_error
                exporting
                  textid = value #( msgid = zcx_error=>zcx_erro_geral-msgid
                                    msgno = zcx_error=>zcx_erro_geral-msgno
                                    attr1 = conv #( 'Chave NFe:' )
                                    attr2 = wa_nfref-refnfe
                                    attr3 = conv #( 'inválida!' ) )
                  msgty  = 'E'
                  msgid  = zcx_error=>zcx_erro_geral-msgid
                  msgno  = zcx_error=>zcx_erro_geral-msgno
                  msgv1  = conv #( 'Chave NFe:' )
                  msgv2  = conv #( wa_nfref-refnfe )
                  msgv3  = conv #( 'inválida!' ).
            endif.
          endloop.

          loop at i_dados-processo-intgnfe-ide-nfref into wa_nfref where refnfe is initial and refcte is initial.

*---------------------------------------
*---------- Nota Fiscal PApel
*---------------------------------------
            if wa_nfref-refnf is not initial.
              if wa_nfref-refnf-cuf is initial and wa_nfref-refnf-siglauf is not initial.
                zcl_estado=>zif_estado~get_instance( )->get_id_bacen( exporting i_uf       = wa_nfref-refnf-siglauf
                                                                      importing e_id_bacen = data(e_id_bacen) ).
                wa_nfref-refnf-cuf = e_id_bacen.
              endif.

              if ( wa_nfref-refnf-cuf   is initial ) or
                 ( wa_nfref-refnf-aamm  is initial ) or
                 ( wa_nfref-refnf-cnpj  is initial ) or
                 ( wa_nfref-refnf-mod   is initial ) or
                 ( wa_nfref-refnf-serie is initial ) or
                 ( wa_nfref-refnf-nnf   is initial ).
                concatenate 'Dados Incompletos NFF:' 'CUF:'    wa_nfref-refnf-cuf
                                                     'AAMM:'   wa_nfref-refnf-aamm
                                                     'CNPJ:'   wa_nfref-refnf-cnpj
                                                     'MOD:'    wa_nfref-refnf-mod
                                                     'SERIE:'  wa_nfref-refnf-serie
                                                     'NNF:'    wa_nfref-refnf-nnf
                                                into l_msg_erro
                                           separated by space.

                raise exception type zcx_error
                  exporting
                    textid = value #( msgid = zcx_error=>zcx_erro_geral-msgid
                                      msgno = zcx_error=>zcx_erro_geral-msgno
                                      attr1 = l_msg_erro )
                    msgty  = 'E'
                    msgid  = zcx_error=>zcx_erro_geral-msgid
                    msgno  = zcx_error=>zcx_erro_geral-msgno
                    msgv1  = conv #( l_msg_erro ).
              endif.
            endif.

*---------------------------------------
*---------- Nota Fiscal Produtor
*---------------------------------------
            if wa_nfref-refnfp is not initial.
              if wa_nfref-refnfp-cuf is initial and wa_nfref-refnfp-siglauf is not initial.
                zcl_estado=>zif_estado~get_instance( )->get_id_bacen( exporting i_uf       = wa_nfref-refnfp-siglauf
                                                                      importing e_id_bacen = e_id_bacen ).
                wa_nfref-refnfp-cuf = e_id_bacen.
              endif.

              if ( wa_nfref-refnfp-cuf    is initial ) or
                 ( wa_nfref-refnfp-aamm   is initial ) or
                 ( wa_nfref-refnfp-cnpj   is initial and wa_nfref-refnfp-cpf is initial ) or
                 ( wa_nfref-refnfp-ie     is initial ) or
                 ( wa_nfref-refnfp-mod    is initial ) or
                 ( wa_nfref-refnfp-serie  is initial ) or
                 ( wa_nfref-refnfp-nnf    is initial ).

                concatenate 'Dados Incompletos NFP:' 'CUF:'    wa_nfref-refnfp-cuf
                                                     'AAMM:'   wa_nfref-refnfp-aamm
                                                     'CNPJ:'   wa_nfref-refnfp-cnpj
                                                     'CPF:'    wa_nfref-refnfp-cpf
                                                     'IE:'     wa_nfref-refnfp-ie
                                                     'MOD:'    wa_nfref-refnfp-mod
                                                     'SERIE:'  wa_nfref-refnfp-serie
                                                     'NNF:'    wa_nfref-refnfp-nnf
                                                into l_msg_erro
                                           separated by space.

                raise exception type zcx_error
                  exporting
                    textid = value #( msgid = zcx_error=>zcx_erro_geral-msgid
                                      msgno = zcx_error=>zcx_erro_geral-msgno
                                      attr1 = l_msg_erro )
                    msgty  = 'E'
                    msgid  = zcx_error=>zcx_erro_geral-msgid
                    msgno  = zcx_error=>zcx_erro_geral-msgno
                    msgv1  = conv #( l_msg_erro ).
              endif.
            endif.
          endloop.

*---------------------------------------
*-------- Inicio Validação de Limite do Fiscal IR054082
*---------------------------------------
          data: vl_total_det type netwr.

          call method me->zif_integracao_grc_new_nfe~get_limite_fiscal
            importing
              e_limite = data(limite_fiscal).

          if limite_fiscal is not initial and limite_fiscal > 0.
            loop at i_dados-processo-intgnfe-det into data(wa_det).
              add wa_det-prod-vprod to vl_total_det.
            endloop.

            if vl_total_det > limite_fiscal.
              select single *
                from zsdt0146
                into @data(wa_zsdt0146)
               where obj_key = @lwa_zsdt0231_proc-obj_key.

              if sy-subrc is not initial.
                l_msg_erro = |Valor do documento { lwa_zsdt0231_proc-obj_key } ultrapassa o limite permitido | &&
                             |e necessita de uma aprovação do Departamento | &&
                             |Fiscal. Favor criar uma FI ao indiretos no Soft | &&
                             |Expert solicitando a validação deste documento|.

                raise exception type zcx_error
                  exporting
                    textid = value #( msgid = zcx_error=>zcx_erro_geral-msgid
                                      msgno = zcx_error=>zcx_erro_geral-msgno
                                      attr1 = l_msg_erro )
                    msgty  = 'E'
                    msgid  = zcx_error=>zcx_erro_geral-msgid
                    msgno  = zcx_error=>zcx_erro_geral-msgno
                    msgv1  = conv #( l_msg_erro ).
              endif.
            endif.
          endif.
*-------- Fim Validação de Limite do Fiscal IR054082

*---------------------------------------
*-------- Validações - Fim
* -----------------------------------------------------------------------------------------------------------------------------------------------*
          me->zif_integracao_grc_new_nfe~set_new_doc_discal(
             exporting
               i_dados              = i_dados
             importing
              e_documento           = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
              e_info_doc_eletronico = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active ).

          l_sucesso = abap_true.
          data(lc_registro_criado) = abap_true.

        catch zcx_integracao into data(ex_integracao).
          data(ms_erro) = ex_integracao->zif_error~get_msg_erro( ).
        catch zcx_error into data(ex_erro).
          ms_erro = ex_erro->zif_error~get_msg_erro( ).
        catch cx_root into data(ex_root).
          ms_erro = ex_root->get_text( ).
      endtry.
    else.
      lc_registro_criado = abap_false.
    endif.

    case l_sucesso.
      when abap_true.

        if lc_registro_criado = abap_false .

          "Verificar Número Não Determinado """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          try .
              zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                )->set_registro(
                     exporting
                       i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                       i_sem_bloqueio = abap_true
                )->get_ck_determinar_numero(
                )->set_det_numero(
                )->get_registro(
                     importing
                       e_documento = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
                       e_info_doc_eletronico = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active
                ).

            catch zcx_doc_eletronico into data(ex_doc_eletronico).

              do 5 times.
                try .
                    zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                      )->set_registro(
                           exporting
                             i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                             i_sem_bloqueio = abap_true
                      )->set_autorizar(
                           exporting
                             i_aguardar = abap_true
                             i_ciclos   = 50
                             i_segundos = 10
                      )->get_registro(
                           importing
                             e_documento = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
                             e_info_doc_eletronico = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active
                      ).

                    select single *
                      into @data(w_active)
                      from j_1bnfe_active
                     where docnum = @me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-docnum.

                    if w_active-docsta = 1 and w_active-scssta = 0.
                      exit.
                    else.
                      wait up to 5 seconds.
                    endif.

                  catch zcx_doc_eletronico.
                  catch cx_root.
                endtry.
              enddo.
            catch cx_root.
          endtry.

        else.
          do 5 times.
            try .
                zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                  )->set_registro(
                       exporting
                         i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                         i_sem_bloqueio = abap_true
                  )->set_autorizar(
                       exporting
                         i_aguardar = abap_true
                         i_ciclos   = 50
                         i_segundos = 10
                  )->get_registro(
                       importing
                         e_documento = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
                         e_info_doc_eletronico = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active
                  ).

                select single *
                  into w_active
                  from j_1bnfe_active
                 where docnum = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-docnum.

                if w_active-docsta = 1 and w_active-scssta = 0.
                  exit.
                else.
                  wait up to 5 seconds.
                endif.

              catch zcx_doc_eletronico.
              catch cx_root.
            endtry.
          enddo.
        endif.

        data(e_chave) = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-regio &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-nfyear &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-nfmonth &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-stcd1 &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-model &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-serie &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-nfnum9 &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-docnum9 &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-cdv.



*      "US172357 - gerar doc fiscal nota produtor que deu origem a nota própria
        select single *
          from zib_nfe_dist_ter
          into @data(w_ter)
          where chave_nfe = @wa_nfref-refnfe.
        "item da nota
        select single *
          from zib_nfe_dist_itm
          into @data(w_itm)
          where chave_nfe = @wa_nfref-refnfe.

        select single matnr
         from j_1bnflin
         into @data(w_matnr)
         where docnum = @me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-docnum.


        call function 'Z_1B_NF_DOCUMENT_SELECT_2'
          exporting
            nf_number                = 0
            model                    = '55'
            series                   = w_ter-serie
            subseries                = '00'
            partner_id               = w_ter-p_emissor
            partner_type             = 'V'
            date                     = w_ter-dt_emissao
            i_nfeflag                = 'X'
            i_nfnum9                 = w_ter-numero
          importing
            doc_number               = vl_docnum
          exceptions
            document_not_found       = 4
            doc_with_same_year_found = 5
            doc_with_diff_year_found = 6
            too_many_documents_found = 8
            others                   = 8.

        if vl_docnum is initial.
          sl_header-nftype    = 'YF'.
          sl_header-doctyp    = 1.
          sl_header-direct    = 1.
          sl_header-docdat    = w_ter-dt_emissao.
          sl_header-pstdat    = sy-datum.
          sl_header-credat    = sy-datum.
          sl_header-form      = ''.
          sl_header-model     = '55'.
          sl_header-series    = w_ter-serie.
          sl_header-waerk     = 'BRL'.
          sl_header-bukrs     = w_ter-bukrs.
          sl_header-branch    = w_ter-branch.
          sl_header-parvw     = 'LF'.
          sl_header-parid     = w_ter-p_emissor.
          sl_header-inco1     = 'CIF'.
          sl_header-inco2     = 'CIF'.
          sl_header-nfe       =  'X'.
          sl_header-nfenum    = w_ter-numero.
          sl_header-docstat   = 1.
          sl_header-tpemis    = wa_nfref-refnfe+34(1).
          sl_header-access_key = wa_nfref-refnfe.
          "
          sl_header_add-nftot  = w_ter-vl_total_fatura.
          "
          sl_nfcheck           = 'X'.
          "
          wl_partner-parvw     = 'LF'. "fornecedor
          wl_partner-parid     = w_ter-p_emissor.
          wl_partner-partyp    = 'V'.  "Fornecedor
          append wl_partner to tl_partner.
          "
          sl_item-itmnum  = '10'.
          sl_item-matnr   = w_matnr.
          sl_item-bwkey   = w_ter-branch.

          select single matkl
            from mara
            into sl_item-matkl
          where matnr = w_matnr.

          select single wgbez
            from t023t
            into sl_item-maktx
          where matkl = sl_item-matkl
          and   spras = sy-langu.

          select single steuc
            from marc
            into sl_item-nbm
          where matnr = w_matnr
          and   werks = w_ter-branch..

          sl_item-matorg = '0'.
          sl_item-matuse = '0'.
          sl_item-reftyp  = 'ZW'.
          sl_item-refkey  = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-docnum. "nota própria
          sl_item-menge   = w_itm-prod_qtd_comerci.
          sl_item-meins   = 'KG'.
          sl_item-netpr   = w_itm-prod_vlr_und_com.
          sl_item-netwr   = w_ter-vl_total_fatura.

          sl_item-taxlw1  = 'IM9'.
          sl_item-taxlw2  = 'I03'.
          sl_item-taxlw4  = 'C70'.
          sl_item-taxlw5  = 'P70'.
          sl_item-itmtyp  = '01'. "Tipo item nota fiscal
          sl_item-incltx  = 'X'. "Valor e preço incluindo ICMS/ISS
          sl_item-werks   = w_ter-branch.

          select single regio
            from lfa1
            into @data(_regiop)
            where lifnr = @w_ter-p_emissor.

          select single regio
            from t001w
            into @data(_regiof)
            where werks = @w_ter-branch.

          if _regiop = _regiof.
            sl_item-cfop_10 = '1949AA'.
          else.
            sl_item-cfop_10 = '2949AA'.
          endif.

          append sl_item to tl_item.
          "
          wl_item_add-itmnum  = 10.
          wl_item_add-direct  = 1.
          append wl_item_add to tl_item_add.


          wl_msg-seqnum  = wl_msg-linnum  = 1.
          wl_msg-message = 'Operação de aquisição sem direito a crédito'.
          append wl_msg to tl_msg.

          wl_item_tax-itmnum = 10.
          wl_item_tax-taxtyp = 'ICM3'.
          wl_item_tax-othbas = w_ter-vl_total_fatura..
          append wl_item_tax to tl_item_tax.

          wl_item_tax-taxtyp = 'ICOF'.
          append wl_item_tax to tl_item_tax.

          wl_item_tax-taxtyp = 'IPIS'.
          append wl_item_tax to tl_item_tax.

          call function 'BAPI_J_1B_NF_CREATEFROMDATA'
            exporting
              obj_header     = sl_header
              obj_header_add = sl_header_add
              nfcheck        = sl_nfcheck
            importing
              e_docnum       = vl_docnum
            tables
              obj_partner    = tl_partner
              obj_item       = tl_item
              obj_item_add   = tl_item_add
              obj_item_tax   = tl_item_tax
              obj_header_msg = tl_msg
              obj_texts      = tl_dados_adic
              return         = tl_return.

          if ( vl_docnum is not initial ) .
            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = 'X'.
            update zib_nfe_forn set docnum        = vl_docnum
                               user_identificador = sy-uname
                               data_identificador = sy-datum
                               hora_identificador = sy-uzeit
            where nu_chave eq wa_nfref-refnfe.
            COMMIT WORK.
          else.
            call function 'BAPI_TRANSACTION_ROLLBACK'.
          endif.
        endif.
*      "US172357 - gerar doc fiscal nota produtor que deu origem a nota própria

      when abap_false.
        ms_erro = zcl_string=>tira_acentos( ms_erro ).
        ms_erro = zcl_string=>convert_to_utf8( ms_erro ).

*       RAISE EXCEPTION TYPE zcx_error
*         EXPORTING
*           textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
*                             msgno = zcx_error=>zcx_erro_geral-msgno
*                             attr1 = CONV #( 'Bad Request' )
*                             attr2 = ms_erro )
*           msgty  = 'E'
*           msgid  = zcx_error=>zcx_erro_geral-msgid
*           msgno  = zcx_error=>zcx_erro_geral-msgno
*           msgv1  = CONV #( 'Bad Request' )
*           msgv2  = CONV #( ms_erro ).

        select single *
          into @me->zif_integracao_grc_new_nfe~at_zsdt0231
          from zsdt0231
         where obj_key eq @i_dados-processo-infosistemaorigem-id.

        if sy-subrc is initial and me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum is not initial.
          select single *
            into @wa_j_1bnfdoc
            from j_1bnfdoc
           where docnum eq @me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum.

          if sy-subrc is not initial.
            delete from zsdt0231
                  where obj_key eq i_dados-processo-infosistemaorigem-id
                    and obj_key ne space.
          endif.
        endif.
    endcase.

    clear w_active.

    e_documento = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-docnum.

  endmethod.


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE~SET_DS_DATA.

    DATA: I_INBOUND       TYPE ZDE_PROCESSO,
          T_ELEMENT_ARRAY	TYPE ZDE_ELEMENT_ARRAY_T.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_GRC_NEW_NFE = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.

    APPEND 'det' TO T_ELEMENT_ARRAY.
    APPEND 'detPag' TO T_ELEMENT_ARRAY.
    APPEND 'NFref' TO T_ELEMENT_ARRAY.
    APPEND 'DI' TO T_ELEMENT_ARRAY.
    APPEND 'adi' TO T_ELEMENT_ARRAY.
    APPEND 'detExport' TO T_ELEMENT_ARRAY.
    APPEND 'med' TO T_ELEMENT_ARRAY.
    APPEND 'arma' TO T_ELEMENT_ARRAY.
    APPEND 'comb' TO T_ELEMENT_ARRAY.
    APPEND 'vol' TO T_ELEMENT_ARRAY.
    APPEND 'lacres' TO T_ELEMENT_ARRAY.
    APPEND 'dup' TO T_ELEMENT_ARRAY.
    APPEND 'pag' TO T_ELEMENT_ARRAY.
    APPEND 'procRef' TO T_ELEMENT_ARRAY.
    APPEND 'obsCont' TO T_ELEMENT_ARRAY.
    APPEND 'obsFisco' TO T_ELEMENT_ARRAY.

    CASE ZCL_STRING=>UPPER( CONV #( I_INFO-DS_FORMATO ) ).
      WHEN 'XML'.
        "Validar Json
        /UI2/CL_JSON=>DESERIALIZE(
           EXPORTING
             JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_INFO-DS_BODY I_ELEMENT_ARRAY =  T_ELEMENT_ARRAY )
           CHANGING DATA = I_INBOUND
        ).
      WHEN 'JSON'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = I_INBOUND ).
    ENDCASE.

    TRY .
        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~GET_VALIDAR_DADOS( EXPORTING I_NFE = I_INBOUND ).
      CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).

        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_PARCEIROS->MSGID
                              MSGNO = EX_PARCEIROS->MSGNO
                              ATTR1 = EX_PARCEIROS->MSGV1
                              ATTR2 = EX_PARCEIROS->MSGV2
                              ATTR3 = EX_PARCEIROS->MSGV3
                              ATTR4 = EX_PARCEIROS->MSGV4 )
            MSGID  = EX_PARCEIROS->MSGID
            MSGNO  = EX_PARCEIROS->MSGNO
            MSGTY  = 'E'
            MSGV1  = EX_PARCEIROS->MSGV1
            MSGV2  = EX_PARCEIROS->MSGV2
            MSGV3  = EX_PARCEIROS->MSGV3
            MSGV4  = EX_PARCEIROS->MSGV4.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).

        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_ERROR->MSGID
                              MSGNO = EX_ERROR->MSGNO
                              ATTR1 = EX_ERROR->MSGV1
                              ATTR2 = EX_ERROR->MSGV2
                              ATTR3 = EX_ERROR->MSGV3
                              ATTR4 = EX_ERROR->MSGV4 )
            MSGID  = EX_ERROR->MSGID
            MSGNO  = EX_ERROR->MSGNO
            MSGTY  = 'E'
            MSGV1  = EX_ERROR->MSGV1
            MSGV2  = EX_ERROR->MSGV2
            MSGV3  = EX_ERROR->MSGV3
            MSGV4  = EX_ERROR->MSGV4.

    ENDTRY.

    "Incluir SeqPlanilha do SIGAM
    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = I_INBOUND-PROCESSO-INFOSISTEMAORIGEM-ID.
    CHECK I_INBOUND-PROCESSO-INFOSISTEMAORIGEM-ID IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_INTEGRACAO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
                          MSGNO = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO )
        MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
        MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD zif_integracao_grc_new_nfe~set_limite_fical.

    SELECT SINGLE limite
      FROM zsdt0279
      INTO me->zif_integracao_grc_new_nfe~at_limite_fiscal
    WHERE model EQ i_model.

  ENDMETHOD.


  METHOD zif_integracao_grc_new_nfe~set_limite_fiscal.

    r_if_integracao_grc_new_nfe = me.

    CHECK i_model IS NOT INITIAL.

    SELECT SINGLE limite
      FROM zsdt0279
       INTO me->zif_integracao_grc_new_nfe~at_limite_fiscal
      WHERE model EQ i_model.

  ENDMETHOD.


  METHOD zif_integracao_grc_new_nfe~set_new_doc_discal.

    DATA: i_cabecalho	TYPE zde_fiscal_cabecalho,
          i_itens	    TYPE zde_fiscal_itens_t,
          w_item      TYPE zde_fiscal_itens,
          i_impostos  TYPE zde_fiscal_impostos_t,
          w_impostos  TYPE zde_fiscal_impostos,
          i_parceiros	TYPE zde_fiscal_parceiros_t,
          e_docnum    TYPE j_1bdocnum,
          e_retorno	  TYPE bapiret2_t,
          r_gerou     TYPE char01,
          ls_j1baj    TYPE j_1baj.

    CLEAR: e_documento,
           e_info_doc_eletronico.

    r_if_integracao_grc_new_nfe = me.

    "Pesquisando Empresas e Filiais """"""""""""""""""""""""""""""""""""""""""
    zcl_fornecedores=>zif_parceiros~get_instance(
      )->set_parceiro_cnpj_cpf_ie(
            EXPORTING
              i_cnpj = CONV #( i_dados-processo-intgnfe-emit-cnpj )
              i_insc_estatual = CONV #( i_dados-processo-intgnfe-emit-ie )
      )->ck_parceiro_local_negocio(
            IMPORTING e_j_1bbranch  = DATA(e_j_1bbranch)
      ).

    SELECT SINGLE partyp INTO @DATA(lv_partyp)
      FROM j_1baa
     WHERE nftype EQ @i_dados-processo-intgnfe-ide-sap_categoria_nota.

    CASE lv_partyp.
      WHEN 'C'.

        DATA(at_lfa1) = CAST zcl_fornecedores( zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro( i_parceiro = CONV #( i_dados-processo-intgnfe-dest-sap_id_parceiro ) ) )->at_lfa1.

        DATA(at_kna1) = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro_cnpj_cpf_ie(
                               EXPORTING i_cnpj = CONV #( at_lfa1-stcd1 ) i_cpf = CONV #( at_lfa1-stcd2 ) i_insc_estatual    = at_lfa1-stcd3 ) )->at_kna1.

        i_cabecalho-parid = at_kna1-kunnr.

      WHEN 'V'.
        i_cabecalho-parid  = i_dados-processo-intgnfe-dest-sap_id_parceiro.
    ENDCASE.

    "Validar Padrão de Data
    FIND REGEX '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' IN i_dados-processo-intgnfe-ide-dhemi(10).
    IF sy-subrc IS NOT INITIAL.

    ENDIF.

    i_cabecalho-nftype = i_dados-processo-intgnfe-ide-sap_categoria_nota.
    "I_CABECALHO-DOCTYP = I_DADOS-PROCESSO-INTGNFE-IDE-FINNFE.
    i_cabecalho-bukrs  = e_j_1bbranch-bukrs.
    i_cabecalho-branch = e_j_1bbranch-branch.
    i_cabecalho-pstdat = sy-datum.
    i_cabecalho-docdat = i_dados-processo-intgnfe-ide-dhemi(4) &&
                         i_dados-processo-intgnfe-ide-dhemi+5(2) &&
                         i_dados-processo-intgnfe-ide-dhemi+8(2).

    "Data de Emissão somente do dia
    IF i_cabecalho-docdat NE sy-datum.

      DATA: lv_datum TYPE c LENGTH 10.
      WRITE sy-datum TO lv_datum .

      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_dt_emissao-msgid
                            msgno = zcx_integracao=>zcx_erro_dt_emissao-msgno
                            attr1 = lv_datum )
          msgid  = zcx_integracao=>zcx_erro_dt_emissao-msgid
          msgno  = zcx_integracao=>zcx_erro_dt_emissao-msgno
          msgty  = 'E'
          msgv1  = CONV #( lv_datum ).
    ENDIF.

    CLEAR:
    i_cabecalho-brgew,
    i_cabecalho-ntgew,
    i_cabecalho-gewei.

    LOOP AT i_dados-processo-intgnfe-det INTO DATA(wa_det).

      "Número do Item
      w_item-itmnum = wa_det-a_nitem.
      "Tipo do Item da Nota Fiscal
      "w_item-ITMTYP = '01'.
      "Área de Avaliação
      w_item-bwkey = e_j_1bbranch-branch.
      "Centro
      w_item-werks = e_j_1bbranch-branch.

      i_cabecalho-gewei = wa_det-prod-ucom.

      "Quantidade
      w_item-menge = wa_det-prod-qcom.
      "Unidade
      w_item-meins = wa_det-prod-ucom.

      ADD wa_det-prod-qcom TO i_cabecalho-brgew.
      ADD wa_det-prod-qcom TO i_cabecalho-ntgew.

      "Despesas líquidas em moeda do documento
      "WA_OBJ_ITEM-NETOTH  = WA_ITENS-NETOTH.
      "Preço Líquido
      IF wa_det-prod-qcom IS NOT INITIAL AND wa_det-prod-qcom NE '0'.
        w_item-netpr = wa_det-prod-vprod / wa_det-prod-qcom.
        w_item-nfpri = w_item-netpr.
      ELSE.
        w_item-netpr = wa_det-prod-vprod.
      ENDIF.
      "Valor Líquido
      w_item-netwr = wa_det-prod-vprod.
      w_item-nfnet = w_item-netwr.
      "CFOP
      w_item-cfop_10 = wa_det-prod-cfop && 'AA'.

      "Leis Fiscais
      "ICMS
      w_item-taxlw1 = wa_det-imposto-sap_lei_fiscal_icm.
      "IPI
      w_item-taxlw2 = wa_det-imposto-sap_lei_fiscal_ipi.
      "COFINS
      w_item-taxlw4 = wa_det-imposto-sap_lei_fiscal_cof.
      "PIS
      w_item-taxlw5 = wa_det-imposto-sap_lei_fiscal_pis.

      "IPI
      CASE i_dados-processo-intgnfe-ide-tpnf.
        WHEN '0'.
          w_item-taxsi2 = '00003'.
        WHEN '1'.
          w_item-taxsi2 = '00053'.
      ENDCASE.

      "Material
      w_item-matnr = wa_det-prod-sap_matnr.

      IF wa_det-prod-sap_incltx EQ 'S'.
        w_item-incltx = abap_true.
      ENDIF.

      "pedido 19/03/2024
      w_item-xped     = wa_det-prod-xped.
      w_item-nitemped = wa_det-prod-nitemped.
      "pedido

      APPEND w_item TO i_itens.

      IF  wa_det-imposto-sap_icm_tp_imposto IS NOT INITIAL.
        w_impostos-itmnum = w_item-itmnum.
        w_impostos-taxtyp = wa_det-imposto-sap_icm_tp_imposto.
        w_impostos-base   = wa_det-imposto-sap_icm_mn_basico.
        w_impostos-rate   = wa_det-imposto-sap_icm_tx_imposto.
        w_impostos-taxval = wa_det-imposto-sap_icm_vr_fiscal.
        w_impostos-excbas = wa_det-imposto-sap_icm_mn_excluido.
        w_impostos-othbas = wa_det-imposto-sap_icm_ot_montante.

        CALL FUNCTION 'J_1BAJ_READ'
          EXPORTING
            taxtype              = w_impostos-taxtyp
          IMPORTING
            e_j_1baj             = ls_j1baj
          EXCEPTIONS
            not_found            = 1
            parameters_incorrect = 2
            OTHERS               = 3.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = sy-msgid
                                msgno = sy-msgno
                                attr1 = CONV #( sy-msgv1 )
                                attr2 = CONV #( sy-msgv2 )
                                attr3 = CONV #( sy-msgv3 )
                                attr4 = CONV #( sy-msgv4 ) )
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgty  = 'E'
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ENDIF.

        APPEND w_impostos TO i_impostos.
      ENDIF.

      IF  wa_det-imposto-sap_pis_tp_imposto IS NOT INITIAL.

        w_impostos-itmnum = w_item-itmnum.
        w_impostos-taxtyp = wa_det-imposto-sap_pis_tp_imposto.
        w_impostos-base   = wa_det-imposto-sap_pis_mn_basico.
        w_impostos-rate   = wa_det-imposto-sap_pis_tx_imposto.
        w_impostos-taxval = wa_det-imposto-sap_pis_vr_fiscal.
        w_impostos-excbas = wa_det-imposto-sap_pis_mn_excluido.
        w_impostos-othbas = wa_det-imposto-sap_pis_ot_montante.

        CALL FUNCTION 'J_1BAJ_READ'
          EXPORTING
            taxtype              = w_impostos-taxtyp
          IMPORTING
            e_j_1baj             = ls_j1baj
          EXCEPTIONS
            not_found            = 1
            parameters_incorrect = 2
            OTHERS               = 3.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = sy-msgid
                                msgno = sy-msgno
                                attr1 = CONV #( sy-msgv1 )
                                attr2 = CONV #( sy-msgv2 )
                                attr3 = CONV #( sy-msgv3 )
                                attr4 = CONV #( sy-msgv4 ) )
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgty  = 'E'
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ENDIF.

        APPEND w_impostos TO i_impostos.
      ENDIF.

      IF  wa_det-imposto-sap_cof_tp_imposto IS NOT INITIAL.

        w_impostos-itmnum = w_item-itmnum.
        w_impostos-taxtyp = wa_det-imposto-sap_cof_tp_imposto.
        w_impostos-base   = wa_det-imposto-sap_cof_mn_basico.
        w_impostos-rate   = wa_det-imposto-sap_cof_tx_imposto.
        w_impostos-taxval = wa_det-imposto-sap_cof_vr_fiscal.
        w_impostos-excbas = wa_det-imposto-sap_cof_mn_excluido.
        w_impostos-othbas = wa_det-imposto-sap_cof_ot_montante.

        CALL FUNCTION 'J_1BAJ_READ'
          EXPORTING
            taxtype              = w_impostos-taxtyp
          IMPORTING
            e_j_1baj             = ls_j1baj
          EXCEPTIONS
            not_found            = 1
            parameters_incorrect = 2
            OTHERS               = 3.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = sy-msgid
                                msgno = sy-msgno
                                attr1 = CONV #( sy-msgv1 )
                                attr2 = CONV #( sy-msgv2 )
                                attr3 = CONV #( sy-msgv3 )
                                attr4 = CONV #( sy-msgv4 ) )
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgty  = 'E'
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ENDIF.

        APPEND w_impostos TO i_impostos.
      ENDIF.

      IF  wa_det-imposto-sap_ipi_tp_imposto IS NOT INITIAL.

        w_impostos-itmnum = w_item-itmnum.
        w_impostos-taxtyp = wa_det-imposto-sap_ipi_tp_imposto.
        w_impostos-base   = wa_det-imposto-sap_ipi_mn_basico.
        w_impostos-rate   = wa_det-imposto-sap_ipi_tx_imposto.
        w_impostos-taxval = wa_det-imposto-sap_ipi_vr_fiscal.
        w_impostos-excbas = wa_det-imposto-sap_ipi_mn_excluido.
        w_impostos-othbas = wa_det-imposto-sap_ipi_ot_montante.

        CALL FUNCTION 'J_1BAJ_READ'
          EXPORTING
            taxtype              = w_impostos-taxtyp
          IMPORTING
            e_j_1baj             = ls_j1baj
          EXCEPTIONS
            not_found            = 1
            parameters_incorrect = 2
            OTHERS               = 3.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = sy-msgid
                                msgno = sy-msgno
                                attr1 = CONV #( sy-msgv1 )
                                attr2 = CONV #( sy-msgv2 )
                                attr3 = CONV #( sy-msgv3 )
                                attr4 = CONV #( sy-msgv4 ) )
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgty  = 'E'
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        ENDIF.

        APPEND w_impostos TO i_impostos.

      ENDIF.

    ENDLOOP.

    i_cabecalho-transp_mode = i_dados-processo-intgnfe-transp-modfrete.

    DATA: fiscal TYPE REF TO zcl_fiscal.
    CREATE OBJECT fiscal.

    fiscal->criar(
      EXPORTING
        i_cabecalho        = i_cabecalho    " Esturtura de transferência dados cabeçalho nota fiscal
        i_itens            = i_itens    " Tabela Estrutura de Itens p/ BAPI Fiscal
        i_impostos         = i_impostos    " Tabela Estrutura de Impostos para BAPI Fiscal
        i_parceiros        = i_parceiros    " Estrutura para parceiros p/ BAPI Fiscal
        i_obs_fiscal       = i_dados-processo-intgnfe-infadic-infadfisco
        i_obs_contribuinte = i_dados-processo-intgnfe-infadic-infcpl
        i_bapi_wait        = abap_true
      IMPORTING
        e_docnum           = e_docnum     " Nº documento
        e_retorno          = e_retorno    " Tabela de retorno
      RECEIVING
        r_gerou            = r_gerou   " Campo de texto do comprimento 1
      EXCEPTIONS
        data_fi_mm_nao     = 1
        nao_cte_forn       = 2
        documento_existe   = 3
        erro               = 4
        OTHERS             = 5 ).

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_error
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = CONV #( sy-msgv1 )
                            attr2 = CONV #( sy-msgv2 )
                            attr3 = CONV #( sy-msgv3 )
                            attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgty  = 'E'
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4.
    ENDIF.

    IF r_gerou EQ abap_false.
      LOOP AT fiscal->at_retorno INTO DATA(wa_mensagem) WHERE type EQ 'E'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = wa_mensagem-id
                              msgno = wa_mensagem-number
                              attr1 = CONV #( wa_mensagem-message_v1 )
                              attr2 = CONV #( wa_mensagem-message_v2 )
                              attr3 = CONV #( wa_mensagem-message_v3 )
                              attr4 = CONV #( wa_mensagem-message_v4 ) )
            msgid  = wa_mensagem-id
            msgno  = wa_mensagem-number
            msgty  = 'E'
            msgv1  = wa_mensagem-message_v1
            msgv2  = wa_mensagem-message_v2
            msgv3  = wa_mensagem-message_v3
            msgv4  = wa_mensagem-message_v4.
      ENDLOOP.

      LOOP AT fiscal->at_retorno INTO wa_mensagem WHERE type EQ 'A'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = wa_mensagem-id
                              msgno = wa_mensagem-number
                              attr1 = CONV #( wa_mensagem-message_v1 )
                              attr2 = CONV #( wa_mensagem-message_v2 )
                              attr3 = CONV #( wa_mensagem-message_v3 )
                              attr4 = CONV #( wa_mensagem-message_v4 ) )
            msgid  = wa_mensagem-id
            msgno  = wa_mensagem-number
            msgty  = 'E'
            msgv1  = wa_mensagem-message_v1
            msgv2  = wa_mensagem-message_v2
            msgv3  = wa_mensagem-message_v3
            msgv4  = wa_mensagem-message_v4.
      ENDLOOP.

      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_doc_nao_gerado-msgid
                            msgno = zcx_integracao=>zcx_doc_nao_gerado-msgno )
          msgid  = zcx_integracao=>zcx_doc_nao_gerado-msgid
          msgno  = zcx_integracao=>zcx_doc_nao_gerado-msgno
          msgty  = 'E'.
    ENDIF.

    "Salvar dados gerados
    me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum    = e_docnum.
    me->zif_integracao_grc_new_nfe~at_zsdt0231-obj_key   = i_dados-processo-infosistemaorigem-id.
    me->zif_integracao_grc_new_nfe~at_zsdt0231-protocolo = me->zif_integracao_grc_new_nfe~at_id_integracao.
    me->zif_integracao_grc_new_nfe~at_zsdt0231-finnfe    = i_dados-processo-intgnfe-ide-finnfe.
    MODIFY zsdt0231 FROM me->zif_integracao_grc_new_nfe~at_zsdt0231.

    DATA: lc_itm TYPE i VALUE 1.

    "Nota Fiscal Eletrônica
    LOOP AT i_dados-processo-intgnfe-ide-nfref INTO DATA(wa_nfref) WHERE refnfe IS NOT INITIAL AND refcte IS INITIAL.

     " USER STORY 81890 24-08-2022 / Anderson Oenning
     APPEND VALUE #( obj_key = i_dados-processo-infosistemaorigem-id
                     itmnum  = lc_itm
                     refnfe  = wa_nfref-refnfe
                     nf_complementada = wa_nfref-nfcomplementada " USER STORY 81890 24-08-2022 / Anderson Oenning
                     ) TO me->zif_integracao_grc_new_nfe~at_zsdt0232.
*      APPEND VALUE #( obj_key = i_dados-processo-infosistemaorigem-id
*                    itmnum  = lc_itm
*                    refnfe  = wa_nfref-refnfe ) TO me->zif_integracao_grc_new_nfe~at_zsdt0232.
     " USER STORY 81890 24-08-2022 / Anderson Oenning

      ADD 1 TO lc_itm.
    ENDLOOP.



    LOOP AT i_dados-processo-intgnfe-ide-nfref INTO wa_nfref WHERE refnfe IS INITIAL AND refcte IS INITIAL.

      "Nota Fiscal Papel
      IF wa_nfref-refnf-aamm IS NOT INITIAL.

        IF wa_nfref-refnf-cuf IS INITIAL AND wa_nfref-refnf-siglauf IS NOT INITIAL.
          zcl_estado=>zif_estado~get_instance( )->get_id_bacen( EXPORTING i_uf = wa_nfref-refnf-siglauf IMPORTING e_id_bacen = DATA(e_id_bacen) ).
          wa_nfref-refnf-cuf = e_id_bacen.
        ENDIF.

        APPEND VALUE #( obj_key = i_dados-processo-infosistemaorigem-id
                        itmnum  = lc_itm
                        cuf   = wa_nfref-refnf-cuf
                        aamm  = wa_nfref-refnf-aamm
                        cnpj  = wa_nfref-refnf-cnpj
                        mod   = wa_nfref-refnf-mod
                        serie = wa_nfref-refnf-serie
                        nnf   = wa_nfref-refnf-nnf ) TO me->zif_integracao_grc_new_nfe~at_zsdt0232.
        ADD 1 TO lc_itm.
      ENDIF.

      "Nota Fiscal Produtor
      IF wa_nfref-refnfp-aamm IS NOT INITIAL.

        IF wa_nfref-refnfp-cuf IS INITIAL AND wa_nfref-refnfp-siglauf IS NOT INITIAL.
          zcl_estado=>zif_estado~get_instance( )->get_id_bacen( EXPORTING i_uf = wa_nfref-refnfp-siglauf IMPORTING e_id_bacen = e_id_bacen ).
          wa_nfref-refnfp-cuf = e_id_bacen.
        ENDIF.

        APPEND VALUE #( obj_key = i_dados-processo-infosistemaorigem-id
                        itmnum  = lc_itm
                        cuf   = wa_nfref-refnfp-cuf
                        aamm  = wa_nfref-refnfp-aamm
                        cnpj  = wa_nfref-refnfp-cnpj
                        cpf   = wa_nfref-refnfp-cpf
                        ie    = wa_nfref-refnfp-ie
                        mod   = wa_nfref-refnfp-mod
                        serie = wa_nfref-refnfp-serie
                        nnf   = wa_nfref-refnfp-nnf ) TO me->zif_integracao_grc_new_nfe~at_zsdt0232.
        ADD 1 TO lc_itm.
      ENDIF.

    ENDLOOP.

    lc_itm = 1.
    LOOP AT i_dados-processo-intgnfe-pag INTO DATA(wa_pag).
      LOOP AT wa_pag-detpag INTO DATA(wa_pag_detpag).
        APPEND VALUE #( obj_key = i_dados-processo-infosistemaorigem-id
                        itmnum = lc_itm
                        tpag = wa_pag_detpag-tpag
                        vpag = wa_pag_detpag-vpag
                        cnpj = wa_pag_detpag-card-cnpj
                        tband = wa_pag_detpag-card-tband
                        caut = wa_pag_detpag-card-caut )
          TO me->zif_integracao_grc_new_nfe~at_zsdt0233.
        ADD 1 TO lc_itm.
      ENDLOOP.
    ENDLOOP.

    IF me->zif_integracao_grc_new_nfe~at_zsdt0232[] IS NOT INITIAL.
      MODIFY zsdt0232 FROM TABLE me->zif_integracao_grc_new_nfe~at_zsdt0232.
    ENDIF.

    IF me->zif_integracao_grc_new_nfe~at_zsdt0233[] IS NOT INITIAL.
      MODIFY zsdt0233 FROM TABLE me->zif_integracao_grc_new_nfe~at_zsdt0233.
    ENDIF.

    COMMIT WORK AND WAIT.

    TRY .

        zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = e_docnum
          )->set_registro(
            EXPORTING
              i_docnum           = e_docnum    " Nº documento
              i_sem_bloqueio     = abap_true
          )->set_det_numero(
          )->get_registro(
            IMPORTING
             e_documento           = e_documento
             e_info_doc_eletronico = e_info_doc_eletronico
          ).
      CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_doc_eletronico->msgid
                              msgno = ex_doc_eletronico->msgno
                              attr1 = ex_doc_eletronico->msgv1
                              attr2 = ex_doc_eletronico->msgv2
                              attr3 = ex_doc_eletronico->msgv3
                              attr4 = ex_doc_eletronico->msgv4 )
            msgid  = ex_doc_eletronico->msgid
            msgno  = ex_doc_eletronico->msgno
            msgv1  = ex_doc_eletronico->msgv1
            msgv2  = ex_doc_eletronico->msgv2
            msgv3  = ex_doc_eletronico->msgv3
            msgv4  = ex_doc_eletronico->msgv4
            msgty  = 'E'.
    ENDTRY.

  ENDMETHOD.


    METHOD ZIF_INTEGRACAO_GRC_NEW_NFE~SET_SEND_MSG.

      DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.

      CLEAR: E_ZINTEGRACAO_LOG.

      R_IF_INTEGRACAO_GRC_NEW_NFE = ME.

      "Verificar a Função de Cada requisição
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = '/nfe/newnfwriter'.

      CREATE OBJECT LC_INTEGRACAO.

      LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
        )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ID_INTEGRACAO
        )->SET_PROCESSAR_RETORNO(
        )->SET_INTEGRAR_RETORNO(
             IMPORTING
               E_DATA_RETORNO = DATA(E_DATA_RETORNO)
               E_ZINTEGRACAO_LOG = E_ZINTEGRACAO_LOG
        )->GET_REGISTRO( IMPORTING E_INTEGRACAO = DATA(E_INTEGRACAO)
        )->FREE(
        ).

      E_MSG = E_DATA_RETORNO.
      E_PROTOCOLO = ZCL_STRING=>LPAD( I_STR  = CONV #( E_INTEGRACAO-ID_INTEGRACAO ) I_QTD  = 15 I_CHAR = '0'  ).

      CLEAR: LC_INTEGRACAO.

    ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  method zif_integracao_inject~set_integrar_inbound.

    data: i_inbound         type zde_processo,
          t_element_array	  type zde_element_array_t,
          lwa_zsdt0231_proc type zsdt0231_proc.

    "Variaveis para a criação do documento fiscal do fornecedor US172357
    data: sl_header     type bapi_j_1bnfdoc,
          sl_header_add type bapi_j_1bnfdoc_add,
          sl_nfcheck    type bapi_j_1bnfcheck,
          tl_partner    type table of bapi_j_1bnfnad,
          wl_partner    type bapi_j_1bnfnad,
          tl_item       type table of bapi_j_1bnflin,
          sl_item       type bapi_j_1bnflin,
          tl_item_add   type table of bapi_j_1bnflin_add,
          wl_item_add   type bapi_j_1bnflin_add,
          tl_item_tax   type table of bapi_j_1bnfstx,
          wl_item_tax   type bapi_j_1bnfstx,
          tl_msg        type table of bapi_j_1bnfftx,
          wl_msg        type bapi_j_1bnfftx,
          tl_dados_adic type table of logbr_nf_texts_compatibility,
          vl_docnum     type j_1bnfdoc-docnum,
          tl_return     type table of bapiret2.
    "Variaveis para a criação do documento fiscal do fornecedor US172357

    data: lva_msg_aux type c length 200.

    e_sucesso = abap_false.
    r_if_integracao_inject = me.

    append 'det' to t_element_array.
    append 'detPag' to t_element_array.
    append 'NFref' to t_element_array.
    append 'DI' to t_element_array.
    append 'adi' to t_element_array.
    append 'detExport' to t_element_array.
    append 'med' to t_element_array.
    append 'arma' to t_element_array.
    append 'comb' to t_element_array.
    append 'vol' to t_element_array.
    append 'lacres' to t_element_array.
    append 'dup' to t_element_array.
    append 'pag' to t_element_array.
    append 'procRef' to t_element_array.
    append 'obsCont' to t_element_array.
    append 'obsFisco' to t_element_array.

    case zcl_string=>upper( conv #( i_msg_completa-ds_formato ) ).
      when 'XML'.
        /ui2/cl_json=>deserialize(
          exporting
            json = zcl_string=>xml_to_json( i_xml = i_msg_inbound  i_element_array = t_element_array )
          changing
            data = i_inbound ).
      when 'JSON'.
        /ui2/cl_json=>deserialize( exporting json = i_msg_inbound changing data = i_inbound ).
    endcase.

    data(lva_registro_in_proc) = abap_false.

    lwa_zsdt0231_proc-obj_key = i_inbound-processo-infosistemaorigem-id.

    insert zsdt0231_proc from lwa_zsdt0231_proc.

    if sy-subrc eq 0.

      commit work.

      call function 'ENQUEUE_EZSDT0231_PROC'
        exporting
          obj_key        = lwa_zsdt0231_proc-obj_key
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.
      if sy-subrc <> 0.
        lva_registro_in_proc = abap_true.
      endif.

    else.
      call function 'ENQUEUE_EZSDT0231_PROC'
        exporting
          obj_key        = lwa_zsdt0231_proc-obj_key
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.

      if sy-subrc <> 0.
        lva_registro_in_proc = abap_true.
      endif.
    endif.

    if lva_registro_in_proc eq abap_true.
      e_nm_code  = '400'.
      concatenate 'Planilha' lwa_zsdt0231_proc-obj_key 'em processamento! Tente novamente mais tarde!' into e_msg_erro separated by space.

      e_msg_outbound = '{ "erro" : "' && e_msg_erro && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso = abap_true.
      return.
    endif.

    select single * into @me->zif_integracao_grc_new_nfe~at_zsdt0231
      from zsdt0231
     where obj_key eq @i_inbound-processo-infosistemaorigem-id.

    if sy-subrc is initial and me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum is not initial.

      select single * into @data(wa_j_1bnfdoc)
        from j_1bnfdoc
       where docnum eq @me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum.

      if sy-subrc is not initial.
        delete from zsdt0231
         where obj_key eq i_inbound-processo-infosistemaorigem-id
           and obj_key ne space.
        sy-subrc = 1.
      endif.

    endif.

    if sy-subrc is initial.

      select single * into @me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
        from j_1bnfdoc
       where docnum eq @me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum.

      select single * into @me->zif_integracao_grc_new_nfe~at_j_1bnfe_active
        from j_1bnfe_active
       where docnum eq @me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum.

      e_sucesso = abap_true.

    endif.

*     "//busca o limite de cada modelo Fiscal IR054082
    call method me->zif_integracao_grc_new_nfe~set_limite_fiscal
      exporting
        i_model = '55'.    " Modelo da nota fiscal

    if e_sucesso ne abap_true.
      try .

*-----------------------------------------------------------------------------------------------------------------------------------------------*
*         Validações - Ini
*-----------------------------------------------------------------------------------------------------------------------------------------------*

          "Nota Fiscal Eletrônica
          loop at i_inbound-processo-intgnfe-ide-nfref into data(wa_nfref) where refnfe is not initial and refcte is initial.

            if strlen( wa_nfref-refnfe ) ne 44.
              e_nm_code  = '400'.
              concatenate 'Chave NFe:' wa_nfref-refnfe 'inválida!' into e_msg_erro separated by space.
              e_msg_outbound = '{ "erro" : "' && e_msg_erro && '"' && cl_abap_char_utilities=>newline && '}'.
              e_sucesso = abap_true.
              return.
            endif.

          endloop.

          loop at i_inbound-processo-intgnfe-ide-nfref into wa_nfref where refnfe is initial and refcte is initial.

            "Nota Fiscal Papel
            if wa_nfref-refnf is not initial.

              if wa_nfref-refnf-cuf is initial and wa_nfref-refnf-siglauf is not initial.
                zcl_estado=>zif_estado~get_instance( )->get_id_bacen( exporting i_uf = wa_nfref-refnf-siglauf importing e_id_bacen = data(e_id_bacen) ).
                wa_nfref-refnf-cuf = e_id_bacen.
              endif.

              if ( wa_nfref-refnf-cuf   is initial ) or
                 ( wa_nfref-refnf-aamm  is initial ) or
                 ( wa_nfref-refnf-cnpj  is initial ) or
                 ( wa_nfref-refnf-mod   is initial ) or
                 ( wa_nfref-refnf-serie is initial ) or
                 ( wa_nfref-refnf-nnf   is initial ).

                e_nm_code  = '400'.

                concatenate 'Dados Incompletos NFF:' 'CUF:'    wa_nfref-refnf-cuf
                                                     'AAMM:'   wa_nfref-refnf-aamm
                                                     'CNPJ:'   wa_nfref-refnf-cnpj
                                                     'MOD:'    wa_nfref-refnf-mod
                                                     'SERIE:'  wa_nfref-refnf-serie
                                                     'NNF:'    wa_nfref-refnf-nnf into e_msg_erro separated by space.

                e_msg_outbound = '{ "erro" : "' && e_msg_erro && '"' && cl_abap_char_utilities=>newline && '}'.
                e_sucesso = abap_true.
                return.

              endif.
            endif.

            "Nota Fiscal Produtor
            if wa_nfref-refnfp is not initial.

              if wa_nfref-refnfp-cuf is initial and wa_nfref-refnfp-siglauf is not initial.
                zcl_estado=>zif_estado~get_instance( )->get_id_bacen( exporting i_uf = wa_nfref-refnfp-siglauf importing e_id_bacen = e_id_bacen ).
                wa_nfref-refnfp-cuf = e_id_bacen.
              endif.

              if ( wa_nfref-refnfp-cuf    is initial ) or
                 ( wa_nfref-refnfp-aamm   is initial ) or
                 ( wa_nfref-refnfp-cnpj   is initial and wa_nfref-refnfp-cpf is initial ) or
                 ( wa_nfref-refnfp-ie     is initial ) or
                 ( wa_nfref-refnfp-mod    is initial ) or
                 ( wa_nfref-refnfp-serie  is initial ) or
                 ( wa_nfref-refnfp-nnf    is initial ).

                e_nm_code  = '400'.

                concatenate 'Dados Incompletos NFP:' 'CUF:'    wa_nfref-refnfp-cuf
                                                     'AAMM:'   wa_nfref-refnfp-aamm
                                                     'CNPJ:'   wa_nfref-refnfp-cnpj
                                                     'CPF:'    wa_nfref-refnfp-cpf
                                                     'IE:'     wa_nfref-refnfp-ie
                                                     'MOD:'    wa_nfref-refnfp-mod
                                                     'SERIE:'  wa_nfref-refnfp-serie
                                                     'NNF:'    wa_nfref-refnfp-nnf into e_msg_erro separated by space.

                e_msg_outbound = '{ "erro" : "' && e_msg_erro && '"' && cl_abap_char_utilities=>newline && '}'.
                e_sucesso = abap_true.
                return.

              endif.
            endif.
          endloop.

*         "// Inicio Validação de Limite do Fiscal IR054082
          data: vl_total_det type netwr.

          call method me->zif_integracao_grc_new_nfe~get_limite_fiscal
            importing
              e_limite = data(limite_fiscal).

          if limite_fiscal is not initial and limite_fiscal > 0.

            loop at i_inbound-processo-intgnfe-det into data(wa_det).
              add wa_det-prod-vprod to vl_total_det.
            endloop.

            if vl_total_det > limite_fiscal.

              select single *
                from zsdt0146
                into @data(wa_zsdt0146)
               where obj_key = @lwa_zsdt0231_proc-obj_key.
              if sy-subrc is not initial.

                e_nm_code  = '400'.
                e_msg_erro = |Valor do documento { lwa_zsdt0231_proc-obj_key } ultrapassa o limite permitido | &&
                             |e necessita de uma aprovação do Departamento | &&
                             |Fiscal. Favor criar uma FI ao indiretos no Soft | &&
                             |Expert solicitando a validação deste documento|.

                e_msg_outbound = '{ "erro" : "' && e_msg_erro && '"' && cl_abap_char_utilities=>newline && '}'.
                e_sucesso = abap_true.
                return.

              endif.
            endif.
          endif.
*         "// Fim Validação de Limite do Fiscal IR054082


          "IR106824 CS1015562 - Ini
          select single *
            from j_1baa into @data(lwa_j_1baa)
           where nftype eq @i_inbound-processo-intgnfe-ide-sap_categoria_nota.

          if sy-subrc ne 0 or i_inbound-processo-intgnfe-ide-sap_categoria_nota is initial.
            e_nm_code  = '400'.
            e_msg_erro = |Categoria de Nota informada não encontrada: { i_inbound-processo-intgnfe-ide-sap_categoria_nota } |.
            e_msg_outbound = '{ "erro" : "' && e_msg_erro && '"' && cl_abap_char_utilities=>newline && '}'.
            e_sucesso = abap_true.
            return.
          endif.

          if lwa_j_1baa-form is initial.
            e_nm_code  = '400'.
            e_msg_erro = |Categoria de nota informada : { i_inbound-processo-intgnfe-ide-sap_categoria_nota }, não é de nota própria! |.
            e_msg_outbound = '{ "erro" : "' && e_msg_erro && '"' && cl_abap_char_utilities=>newline && '}'.
            e_sucesso = abap_true.
            return.
          endif.
          "IR106824 CS1015562 - Fim

*-----------------------------------------------------------------------------------------------------------------------------------------------*
*         Validações - Fim
* -----------------------------------------------------------------------------------------------------------------------------------------------*

          me->zif_integracao_grc_new_nfe~set_new_doc_discal(
             exporting
               i_dados              = i_inbound
             importing
              e_documento           = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
              e_info_doc_eletronico = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active ).

          e_sucesso = abap_true.
          data(lc_registro_criado) = abap_true.

        catch zcx_integracao into data(ex_integracao).
          data(ms_erro) = ex_integracao->zif_error~get_msg_erro( ).
        catch zcx_error into data(ex_erro).
          ms_erro = ex_erro->zif_error~get_msg_erro( ).
        catch cx_root into data(ex_root).
          ms_erro = ex_root->get_text( ).
      endtry.
    else.
      lc_registro_criado = abap_false.
    endif.

    case e_sucesso.
      when abap_true.

        if lc_registro_criado = abap_false .

          "Verificar Número Não Determinado """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          try .
              zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                )->set_registro(
                     exporting
                       i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                       i_sem_bloqueio = abap_true
                )->get_ck_determinar_numero(
                )->set_det_numero(
                )->get_registro(
                     importing
                       e_documento = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
                       e_info_doc_eletronico = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active
                ).

            catch zcx_doc_eletronico into data(ex_doc_eletronico).

              try .
                  zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                    )->set_registro(
                         exporting
                           i_docnum = me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum
                           i_sem_bloqueio = abap_true
                    )->set_autorizar(
                    )->get_registro(
                         importing
                           e_documento = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc
                           e_info_doc_eletronico = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active
                    ).
                catch zcx_doc_eletronico.
                catch cx_root.
              endtry.
            catch cx_root.
          endtry.

        endif.
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        data(e_chave) = me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-regio &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-nfyear &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-nfmonth &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-stcd1 &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-model &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-serie &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-nfnum9 &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-docnum9 &&
                        me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-cdv.
        "US172357 - gerar doc fiscal nota produtor que deu origem a nota própria
        if me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-nftype ='ZW'.
          select single *
            from zib_nfe_dist_ter
            into @data(w_ter)
            where chave_nfe = @wa_nfref-refnfe.
          "item da nota
          select single *
            from zib_nfe_dist_itm
            into @data(w_itm)
            where chave_nfe = @wa_nfref-refnfe.

          select single matnr
           from j_1bnflin
           into @data(w_matnr)
           where docnum = @me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-docnum.


          call function 'Z_1B_NF_DOCUMENT_SELECT_2'
            exporting
              nf_number                = 0
              model                    = '55'
              series                   = w_ter-serie
              subseries                = '00'
              partner_id               = w_ter-p_emissor
              partner_type             = 'V'
              date                     = w_ter-dt_emissao
              i_nfeflag                = 'X'
              i_nfnum9                 = w_ter-numero
            importing
              doc_number               = vl_docnum
            exceptions
              document_not_found       = 4
              doc_with_same_year_found = 5
              doc_with_diff_year_found = 6
              too_many_documents_found = 8
              others                   = 8.

          if vl_docnum is initial.
            sl_header-nftype    = 'YF'.
            sl_header-doctyp    = 1.
            sl_header-direct    = 1.
            sl_header-docdat    = w_ter-dt_emissao.
            sl_header-pstdat    = sy-datum.
            sl_header-credat    = sy-datum.
            sl_header-form      = ''.
            sl_header-model     = '55'.
            sl_header-series    = w_ter-serie.
            sl_header-waerk     = 'BRL'.
            sl_header-bukrs     = w_ter-bukrs.
            sl_header-branch    = w_ter-branch.
            sl_header-parvw     = 'LF'.
            sl_header-parid     = w_ter-p_emissor.
            sl_header-inco1     = 'CIF'.
            sl_header-inco2     = 'CIF'.
            sl_header-nfe       =  'X'.
            sl_header-nfenum    = w_ter-numero.
            sl_header-docstat   = 1.
            sl_header-tpemis    = wa_nfref-refnfe+34(1).
            sl_header-access_key = wa_nfref-refnfe.
            "
            sl_header_add-nftot  = w_ter-vl_total_fatura.
            "
            sl_nfcheck           = 'X'.
            "
            wl_partner-parvw     = 'LF'. "fornecedor
            wl_partner-parid     = w_ter-p_emissor.
            wl_partner-partyp    = 'V'.  "Fornecedor
            append wl_partner to tl_partner.
            "
            sl_item-itmnum  = '10'.
            sl_item-matnr   = w_matnr.
            sl_item-bwkey   = w_ter-branch.

            select single matkl
              from mara
              into sl_item-matkl
            where matnr = w_matnr.

            select single wgbez
              from t023t
              into sl_item-maktx
            where matkl = sl_item-matkl
            and   spras = sy-langu.

            select single steuc
              from marc
              into sl_item-nbm
            where matnr = w_matnr
            and   werks = w_ter-branch..

            sl_item-matorg = '0'.
            sl_item-matuse = '0'.
            sl_item-reftyp  = 'ZW'.
            sl_item-refkey  = me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-docnum. "nota própria
            sl_item-menge   = w_itm-prod_qtd_comerci.
            sl_item-meins   = 'KG'.
            sl_item-netpr   = w_itm-prod_vlr_und_com.
            sl_item-netwr   = w_ter-vl_total_fatura.

            sl_item-taxlw1  = 'IM9'.
            sl_item-taxlw2  = 'I03'.
            sl_item-taxlw4  = 'C70'.
            sl_item-taxlw5  = 'P70'.
            sl_item-itmtyp  = '01'. "Tipo item nota fiscal
            sl_item-incltx  = 'X'. "Valor e preço incluindo ICMS/ISS
            sl_item-werks   = w_ter-branch.

            select single regio
              from lfa1
              into @data(_regiop)
              where lifnr = @w_ter-p_emissor.

            select single regio
              from t001w
              into @data(_regiof)
              where werks = @w_ter-branch.

            if _regiop = _regiof.
              sl_item-cfop_10 = '1949AA'.
            else.
              sl_item-cfop_10 = '2949AA'.
            endif.

            append sl_item to tl_item.
            "
            wl_item_add-itmnum  = 10.
            wl_item_add-direct  = 1.
            append wl_item_add to tl_item_add.


            wl_msg-seqnum  = wl_msg-linnum  = 1.
            wl_msg-message = 'Operação de aquisição sem direito a crédito'.
            append wl_msg to tl_msg.

            wl_item_tax-itmnum = 10.
            wl_item_tax-taxtyp = 'ICM3'.
            wl_item_tax-othbas = w_ter-vl_total_fatura..
            append wl_item_tax to tl_item_tax.

            wl_item_tax-taxtyp = 'ICOF'.
            append wl_item_tax to tl_item_tax.

            wl_item_tax-taxtyp = 'IPIS'.
            append wl_item_tax to tl_item_tax.

            call function 'BAPI_J_1B_NF_CREATEFROMDATA'
              exporting
                obj_header     = sl_header
                obj_header_add = sl_header_add
                nfcheck        = sl_nfcheck
              importing
                e_docnum       = vl_docnum
              tables
                obj_partner    = tl_partner
                obj_item       = tl_item
                obj_item_add   = tl_item_add
                obj_item_tax   = tl_item_tax
                obj_header_msg = tl_msg
                obj_texts      = tl_dados_adic
                return         = tl_return.

            if ( vl_docnum is not initial ) .
              call function 'BAPI_TRANSACTION_COMMIT'
                exporting
                  wait = 'X'.
              update zib_nfe_forn set docnum        = vl_docnum
                                 user_identificador = sy-uname
                                 data_identificador = sy-datum
                                 hora_identificador = sy-uzeit
              where nu_chave eq wa_nfref-refnfe.
              commit work.
            else.
              call function 'BAPI_TRANSACTION_ROLLBACK'.
            endif.
          endif.
        endif.
*      "US172357 - gerar doc fiscal nota produtor que deu origem a nota própria

        e_msg_outbound = '{ "protocolo" : "' &&
                                zcl_string=>lpad( i_str  = conv #( me->zif_integracao_grc_new_nfe~at_zsdt0231-protocolo ) i_qtd  = 15 i_char = '0'  ) &&  '" ,'
                                && cl_abap_char_utilities=>newline &&
             ' "docnum" : "' && me->zif_integracao_grc_new_nfe~at_j_1bnfdoc-docnum &&  '" ,' && cl_abap_char_utilities=>newline &&
             ' "nfenum" : "' && me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-nfnum9 &&  '" ,  ' && cl_abap_char_utilities=>newline &&
             ' "serie"  : "' && me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-serie &&  '", ' && cl_abap_char_utilities=>newline &&
             ' "model"  : "' && me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-model &&  '", ' && cl_abap_char_utilities=>newline &&
             ' "chave"  : "' && e_chave && '", ' && cl_abap_char_utilities=>newline &&
             ' "protocolo_autorizacao" : "' && me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-authcod &&  '", ' && cl_abap_char_utilities=>newline &&
             ' "cnpj"   : "' && me->zif_integracao_grc_new_nfe~at_j_1bnfe_active-stcd1 &&  '" ' && cl_abap_char_utilities=>newline &&
             ' }'.

        e_nm_code  = '200'.
        e_msg_erro = 'Ok'.

      when abap_false.
        ms_erro = zcl_string=>tira_acentos( ms_erro ).
        ms_erro = zcl_string=>convert_to_utf8( ms_erro ).

        e_nm_code  = '400'.
        e_msg_erro = 'Bad Request'.

        e_msg_outbound = '{ ' && cl_abap_char_utilities=>newline && '"protocolo" : " ' &&
                               zcl_string=>lpad( i_str  = conv #( me->zif_integracao_grc_new_nfe~at_zsdt0231-protocolo ) i_qtd  = 15 i_char = '0'  ) &&  '", ' &&
                            ' "erro" : "' && ms_erro && '"' && cl_abap_char_utilities=>newline &&
                          '}'.

        select single * into @me->zif_integracao_grc_new_nfe~at_zsdt0231
          from zsdt0231
         where obj_key eq @i_inbound-processo-infosistemaorigem-id.

        if sy-subrc is initial and me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum is not initial.

          select single * into @wa_j_1bnfdoc
            from j_1bnfdoc
           where docnum eq @me->zif_integracao_grc_new_nfe~at_zsdt0231-docnum.

          if sy-subrc is not initial.
            delete from zsdt0231
             where obj_key eq i_inbound-processo-infosistemaorigem-id
               and obj_key ne space.
          endif.

        endif.

    endcase.

    e_sucesso = abap_true.

  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
  endmethod.
ENDCLASS.
