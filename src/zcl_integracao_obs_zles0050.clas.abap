class ZCL_INTEGRACAO_OBS_ZLES0050 definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OBS_ZLES0050 .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_OBS_ZLES0050 IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_obs_zles0050.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_referencia-tp_referencia = 'OBS_NF_ZLES0050'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: i_inbound             TYPE zde_obs_zles0050,
          t_element_array	      TYPE zde_element_array_t,
          l_vbelv               TYPE vbak-vbeln,
          l_chave_nfe           TYPE char44,
          l_nfenum_loc(9)       TYPE n,
          l_nfenum_doc(9)       TYPE n,
          l_nfenum_char(15)     TYPE c,
          l_nfenum_cte_loc(9)   TYPE n,
          l_nfenum_cte_doc(9)   TYPE n,
          l_nfenum_cte_char(15) TYPE c,
          l_mblnr               TYPE mblnr,
          l_mjahr               TYPE mjahr,
          l_observacao          TYPE char_132,
          l_fornecimento        TYPE zdoc_rem,
          lva_msg_aux           TYPE c LENGTH 200.

    e_sucesso              = abap_false.
    e_nm_code              = abap_false.
    r_if_integracao_inject = me.

    APPEND 'det'       TO t_element_array.
    APPEND 'detPag'    TO t_element_array.
    APPEND 'NFref'     TO t_element_array.
    APPEND 'DI'        TO t_element_array.
    APPEND 'adi'       TO t_element_array.
    APPEND 'detExport' TO t_element_array.
    APPEND 'med'       TO t_element_array.
    APPEND 'arma'      TO t_element_array.
    APPEND 'comb'      TO t_element_array.
    APPEND 'vol'       TO t_element_array.
    APPEND 'lacres'    TO t_element_array.
    APPEND 'dup'       TO t_element_array.
    APPEND 'pag'       TO t_element_array.
    APPEND 'procRef'   TO t_element_array.
    APPEND 'obsCont'   TO t_element_array.
    APPEND 'obsFisco'  TO t_element_array.

    CASE zcl_string=>upper( CONV #( i_msg_completa-ds_formato ) ).
      WHEN 'XML'.
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = zcl_string=>xml_to_json( i_xml = i_msg_inbound i_element_array = t_element_array )
          CHANGING
            data = i_inbound ).
      WHEN 'JSON'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = i_inbound ).
    ENDCASE.

*----------------------------
* formatacao campos
*----------------------------
    REPLACE ALL OCCURRENCES OF REGEX '-' IN i_inbound-nfenum_nfe WITH space.
    REPLACE ALL OCCURRENCES OF REGEX '-' IN i_inbound-nfenum_cte WITH space.

    CONDENSE i_inbound-vbeln        NO-GAPS.
    CONDENSE i_inbound-nfenum_nfe   NO-GAPS.
    CONDENSE i_inbound-nfenum_cte   NO-GAPS.
    CONDENSE i_inbound-nr_sasl      NO-GAPS.
    CONDENSE i_inbound-nr_protocolo NO-GAPS.
    CONDENSE i_inbound-operacao     NO-GAPS.

*----------------------------
*-- valida operacao
*----------------------------
    IF i_inbound-operacao <> 'I' AND
       i_inbound-operacao <> 'R'.
      e_nm_code  = '400'.
      CONCATENATE 'Operação incorreta:' i_inbound-operacao
             INTO e_msg_erro SEPARATED BY space.
      e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso = abap_true.
      RETURN.
    ENDIF.

*----------------------------
*-- formata nr OV
*----------------------------
    l_vbelv = i_inbound-vbeln.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_vbelv
      IMPORTING
        output = l_vbelv.

*----------------------------
* verifica se trata de PEdido de compras
*----------------------------
    SELECT ebeln, reswk
      INTO @DATA(w_ekko)
        UP TO 1 ROWS
      FROM ekko
     WHERE ebeln = @l_vbelv.
    ENDSELECT.

    IF sy-subrc = 0.
*----------------------------
*---- trata-se de Pedido Compras
*----------------------------
      IF w_ekko-reswk IS INITIAL.
        SELECT werks
          INTO w_ekko-reswk
            UP TO 1 ROWS
          FROM ekpo
         WHERE ebeln = w_ekko-ebeln.
        ENDSELECT.
      ENDIF.

      l_nfenum_char = i_inbound-nfenum_nfe.
      l_nfenum_loc  = l_nfenum_char.

      SELECT docnum
        INTO @DATA(l_docnum)
        FROM j_1bnfdoc
          UP TO 1 ROWS
       WHERE   branch = @w_ekko-reswk
         AND ( direct = '1'
          OR   direct = '2' )
         AND   nfenum = @l_nfenum_loc
         AND   form  <> @abap_off.
      ENDSELECT.

      IF sy-subrc <> 0.
        e_nm_code  = '400'.
        CONCATENATE 'Doc.Fiscal não encontrado em J_1BNFSOC:' i_inbound-nfenum_nfe
               INTO e_msg_erro SEPARATED BY space.
        e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                            '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
        e_sucesso = abap_true.
        RETURN.
      ENDIF.

      SELECT refkey
        INTO @DATA(l_refkey)
        FROM j_1bnflin
          UP TO 1 ROWS
       WHERE docnum = @l_docnum.
      ENDSELECT.

      IF sy-subrc <> 0.
        e_nm_code  = '400'.
        CONCATENATE 'Doc.Fiscal não encontrado em J_1BNFLIN:' l_docnum
               INTO e_msg_erro SEPARATED BY space.
        e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                            '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
        e_sucesso = abap_true.
        RETURN.
      ENDIF.

      l_mblnr = l_refkey(10).
      l_mjahr = l_refkey+10(4).

*-------------------------------
*---- Pedido transferencia
*-------------------------------
      SELECT ebeln
        INTO @DATA(l_ebeln)
        FROM mseg
          UP TO 1 ROWS
       WHERE mblnr = @l_mblnr
         AND mjahr = @l_mjahr.
      ENDSELECT.

      IF sy-subrc = 0.
        IF w_ekko-ebeln <> l_ebeln.
          e_nm_code  = '400'.
          CONCATENATE 'Pedido de Compras incompatível com NF-e informado:' i_inbound-nfenum_nfe
                 INTO e_msg_erro SEPARATED BY space.
          e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                              '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
          e_sucesso = abap_true.
          RETURN.
        ENDIF.
      ELSE.
*-------------------------------
*------ Pedido importacao
*-------------------------------
        SELECT seq_lcto
          INTO @DATA(l_seq_lcto)
          FROM zfiwrt0008
            UP TO 1 ROWS
         WHERE docnum = @l_docnum
           AND branch = @w_ekko-reswk
           AND ebeln  = @w_ekko-ebeln.
        ENDSELECT.

        IF sy-subrc <> 0.
          e_nm_code  = '400'.
          CONCATENATE 'Pedido de Compras incompatível com NF-e informado:' i_inbound-nfenum_nfe
                 INTO e_msg_erro SEPARATED BY space.
          e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                              '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
          e_sucesso = abap_true.
          RETURN.
        ELSE.
          e_nm_code  = '200'.
          CONCATENATE 'Pedido de Importação não tem observação na transação ZLS0050:' i_inbound-nfenum_nfe
                 INTO e_msg_erro SEPARATED BY space.
          e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                              '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
          e_sucesso = abap_true.
          RETURN.
        ENDIF.
      ENDIF.

    ELSE.
*----------------------------
*---- trata-se de Ordem de VEnda
*---- buscar faturas OV
*----------------------------
      SELECT vbeln
        INTO TABLE @DATA(t_faturas)
        FROM vbfa
       WHERE vbelv = @l_vbelv
         AND vbtyp_n = 'M'.

      IF sy-subrc <> 0.
        e_nm_code  = '400'.
        CONCATENATE 'OV a faturar não encontrada:' l_vbelv
               INTO e_msg_erro SEPARATED BY space.
        e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                            '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
        e_sucesso = abap_true.
        RETURN.
      ENDIF.

      SORT t_faturas BY vbeln DESCENDING.

*----------------------------
*---- buscar DOCNUM
*----------------------------
      e_nm_code = abap_false.

      LOOP AT t_faturas INTO DATA(w_faturas).

        SELECT docnum
          INTO l_docnum
          FROM j_1bnflin
            UP TO 1 ROWS
         WHERE refkey = w_faturas-vbeln.
        ENDSELECT.

        IF sy-subrc <> 0.
          e_nm_code  = '400'.
          CONCATENATE 'Doc.Fiscal não encontrado em J_1BNFLIN:' w_faturas-vbeln
                 INTO e_msg_erro SEPARATED BY space.
          e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                              '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
          e_sucesso = abap_true.
          CONTINUE.
        ENDIF.

*----------------------------
*------ buscar NF-e para comparacao
*----------------------------
        SELECT nfenum
          INTO @DATA(l_nfenum)
          FROM j_1bnfdoc
            UP TO 1 ROWS
         WHERE docnum = @l_docnum
           AND cancel = @abap_false.
        ENDSELECT.

        IF sy-subrc <> 0.
          e_nm_code  = '400'.
          CONCATENATE 'Doc.Fiscal não encontrado em J_1BNFSOC:' l_docnum
                 INTO e_msg_erro SEPARATED BY space.
          e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                              '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
          e_sucesso = abap_true.
          CONTINUE.
        ENDIF.

        l_nfenum_char = i_inbound-nfenum_nfe.
        l_nfenum_loc  = l_nfenum_char.
        l_nfenum_char = l_nfenum.
        l_nfenum_doc  = l_nfenum_char.

        IF l_nfenum_loc <> l_nfenum_doc.
          e_nm_code  = '400'.
          CONCATENATE 'OV:' l_vbelv 'não possui NF Faturada com a numeração:' l_nfenum_loc
                 INTO e_msg_erro SEPARATED BY space.
          e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                              '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
          e_sucesso = abap_true.
          CONTINUE.
        ENDIF.

        e_nm_code = abap_false.

        EXIT.
      ENDLOOP.

      IF e_nm_code = '400'.
        RETURN.
      ENDIF.
    ENDIF.

*---------------------------------------
*-- n fiscal
*---------------------------------------
    SELECT *
      INTO @DATA(w_j_1bnfe_active)
      FROM j_1bnfe_active
        UP TO 1 ROWS
     WHERE docnum  = @l_docnum
       AND docsta  = 1
       AND scssta <> '2'.
    ENDSELECT.

    IF sy-subrc <> 0.
      e_nm_code  = '400'.
      CONCATENATE 'Nota fiscal não encontra-se autorizada:' l_nfenum_loc
             INTO e_msg_erro SEPARATED BY space.
      e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso = abap_true.
      RETURN.
    ELSE.
*--------------------------------------
*-- chafe NFE
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



*--------------------------------------
*-- Recuperar VT da NF-e
*---------------------------------------
    CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
      EXPORTING
        i_docnum = l_docnum
        i_direct = '2'
      IMPORTING
        e_vbeln  = l_fornecimento.

    IF l_fornecimento IS INITIAL.

      e_nm_code  = '400'.
      CONCATENATE 'Não encontrada o documento de fornecimento do Documento:' l_docnum
             INTO e_msg_erro SEPARATED BY space.

      e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso = abap_true.
      RETURN.

    ENDIF.


    SELECT SINGLE *
       FROM vttp INTO @DATA(lwa_vttp)
      WHERE vbeln EQ @l_fornecimento.

    IF sy-subrc NE 0.

      e_nm_code  = '400'.
      CONCATENATE 'Não encontrada o documento de transporte do fornecimento:' l_fornecimento
             INTO e_msg_erro SEPARATED BY space.

      e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso = abap_true.
      RETURN.

    ENDIF.

    TRY.
        zcl_faturamento=>zif_faturamento~get_instance( )->get_processo_emissao_docs(
          EXPORTING
             i_tknum            = CONV #( lwa_vttp-tknum )
          IMPORTING
             e_conhecimento     =  DATA(_emite_conhecimento)  ).
      CATCH zcx_faturamento INTO DATA(_zcx_fat).
      CATCH zcx_error       INTO DATA(_zcx_error).
    ENDTRY.


    CASE _emite_conhecimento.
      WHEN abap_true.

        IF i_inbound-nfenum_cte IS INITIAL.

          e_nm_code  = '400'.
          e_msg_erro = 'Operação com transporte e não informado o numero da DACTE!'.

          e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                              '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
          e_sucesso = abap_true.
          RETURN.

        ENDIF.

      WHEN abap_false.

        IF i_inbound-nfenum_cte IS NOT INITIAL.

          e_nm_code  = '400'.
          e_msg_erro = 'Operação sem transporte e foi informado o numero da DACTE!'.

          e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                              '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
          e_sucesso = abap_true.
          RETURN.

        ENDIF.

    ENDCASE.

*----------------------------
*-- buscar chave do CT-e vinculado a NF-e
*----------------------------
    IF  i_inbound-nfenum_cte IS NOT INITIAL.
      SELECT cd_chave_cte
        INTO @DATA(l_cd_chave_cte)
        FROM zib_cte_dist_n55 AS a
          UP TO 1 ROWS
       WHERE n55_chave_acesso = @l_chave_nfe
         AND EXISTS ( SELECT numr_cte
                        FROM zib_cte_dist_ter AS b
                       WHERE b~cd_chave_cte EQ a~cd_chave_cte
                         AND b~cd_modal     EQ '01' ).
      ENDSELECT.

      IF sy-subrc <> 0.
        e_nm_code  = '400'.
        CONCATENATE 'Doc.Fiscal:' l_nfenum_doc 'não encontrado em ZIB_CTE_DIST_N55.'
               INTO e_msg_erro SEPARATED BY space.
        e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                            '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
        e_sucesso = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

*----------------------------
*-- Busca número do CT-e para comparar
*----------------------------
    IF  i_inbound-nfenum_cte IS NOT INITIAL.
      SELECT numr_cte
        INTO @DATA(l_numr_cte)
        FROM zib_cte_dist_ter
          UP TO 1 ROWS
       WHERE cd_chave_cte = @l_cd_chave_cte
         AND cd_modal     = '01'.
      ENDSELECT.

      l_nfenum_cte_char = i_inbound-nfenum_cte.
      l_nfenum_cte_loc  = l_nfenum_cte_char.
      l_nfenum_cte_char = l_numr_cte.
      l_nfenum_cte_doc  = l_nfenum_cte_char.

      IF l_nfenum_cte_loc <> l_nfenum_cte_doc.
        e_nm_code  = '400'.
        CONCATENATE 'Nr.Ct-e SAP:' l_numr_cte 'diferente da Nr.Ct-e SoftExpert:' l_nfenum_cte_loc
               INTO e_msg_erro SEPARATED BY space.
        e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                            '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
        e_sucesso = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

*----------------------------
*-- ajuste campo observacao
*----------------------------
    SELECT *
      INTO @DATA(w_zlest0039)
      FROM zlest0039
        UP TO 1 ROWS
     WHERE docnum = @l_docnum.
    ENDSELECT.

    IF sy-subrc <> 0.
      e_nm_code  = '400'.
      CONCATENATE 'Doc.Fiscal:' i_inbound-nfenum_nfe 'não encontrado na transação ZLES0050.'
             INTO e_msg_erro SEPARATED BY space.
      e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso = abap_true.
      RETURN.
    ENDIF.

    IF i_inbound-operacao = 'I'.
      l_observacao = 'Carga Sinistrada:' && i_inbound-nr_sasl && '-' && i_inbound-nr_protocolo.
    ELSE.
      CLEAR l_observacao.
    ENDIF.

*----------------------------
*-- update observacao
*----------------------------
    w_zlest0039-observacao = l_observacao.
    w_zlest0039-tp_baixa   = '2'.     "*-CS2023000070-23.03.2023-#106745-JT

    CALL FUNCTION 'ENQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
      EXPORTING
        docnum         = w_zlest0039-docnum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0. "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA

      MODIFY zlest0039    FROM w_zlest0039.

      COMMIT WORK AND WAIT.

      CALL FUNCTION 'DEQUEUE_EZLEST0039' "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA
        EXPORTING
          docnum = w_zlest0039-docnum.

      "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA - Ini
    ELSE.

      e_nm_code  = '400'.
      e_msg_erro = 'Registro bloqueado para modificaçao...Aguarde!'.
      e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso = abap_true.
      RETURN.

    ENDIF.
    "161420 IR207175 - avaliar job ATUALIZA_COMP_J2 PSA - Fim

*----------------------------
*-- msg retorno
*----------------------------
    e_nm_code  = '200'.

    IF w_ekko-ebeln IS NOT INITIAL.
      CONCATENATE 'Observação do Pedido modificado com Sucesso:' l_vbelv
             INTO e_msg_erro SEPARATED BY space.
    ELSE.
      CONCATENATE 'Observação da OV modificada com Sucesso:'     l_vbelv
             INTO e_msg_erro SEPARATED BY space.
    ENDIF.

    e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                        '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
  endmethod.


  METHOD zif_integracao_obs_zles0050~get_instance.

    IF zif_integracao_obs_zles0050~at_if_integracao_obs_zles0050 IS NOT BOUND.
      CREATE OBJECT zif_integracao_obs_zles0050~at_if_integracao_obs_zles0050 TYPE zcl_integracao_obs_zles0050.
    ENDIF.
    r_if_integracao_obs_zles0050 = zif_integracao_obs_zles0050~at_if_integracao_obs_zles0050.

  ENDMETHOD.


  METHOD zif_integracao_obs_zles0050~set_ds_data.

    DATA: i_inbound       TYPE zde_obs_zles0050,
          t_element_array	TYPE zde_element_array_t.

    "Incluir Texto JSON para integração
    r_if_integracao_obs_zles0050 = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

    APPEND 'det' TO t_element_array.
    APPEND 'detPag' TO t_element_array.
    APPEND 'NFref' TO t_element_array.
    APPEND 'DI' TO t_element_array.
    APPEND 'adi' TO t_element_array.
    APPEND 'detExport' TO t_element_array.
    APPEND 'med' TO t_element_array.
    APPEND 'arma' TO t_element_array.
    APPEND 'comb' TO t_element_array.
    APPEND 'vol' TO t_element_array.
    APPEND 'lacres' TO t_element_array.
    APPEND 'dup' TO t_element_array.
    APPEND 'pag' TO t_element_array.
    APPEND 'procRef' TO t_element_array.
    APPEND 'obsCont' TO t_element_array.
    APPEND 'obsFisco' TO t_element_array.

    CASE zcl_string=>upper( CONV #( i_info-ds_formato ) ).
      WHEN 'XML'.
        "Validar Json
        /ui2/cl_json=>deserialize(
           EXPORTING
             json = zcl_string=>xml_to_json( i_xml = i_info-ds_body i_element_array =  t_element_array )
           CHANGING data = i_inbound
        ).
      WHEN 'JSON'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = i_inbound ).
    ENDCASE.

    me->zif_integracao_inject~at_referencia-id_referencia = i_inbound-vbeln && '-' && i_inbound-nfenum_nfe.

  ENDMETHOD.


  METHOD zif_integracao_obs_zles0050~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_if_integracao_obs_zles0050 = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = '/zles0050/observacao'.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_obs_zles0050~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
           IMPORTING
             e_data_retorno = DATA(e_data_retorno)
             e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    e_msg = e_data_retorno.
    e_protocolo = zcl_string=>lpad( i_str  = CONV #( e_integracao-id_integracao ) i_qtd  = 15 i_char = '0'  ).

    CLEAR: lc_integracao.

  ENDMETHOD.
ENDCLASS.
