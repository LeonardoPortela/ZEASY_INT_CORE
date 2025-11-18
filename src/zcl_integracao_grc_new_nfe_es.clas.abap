class ZCL_INTEGRACAO_GRC_NEW_NFE_ES definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_GRC_NEW_NFE_ES .
  interfaces ZIF_INTEGRACAO_INJECT .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_GRC_NEW_NFE_ES IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_grc_est_doc.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_referencia-tp_referencia = 'NFE_SAP_GRC'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE_ES~GET_INSTANCE.

    IF ZIF_INTEGRACAO_GRC_NEW_NFE_ES~AT_IF_INTEGRACAO_GRC_EST_NFE IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_GRC_NEW_NFE_ES~AT_IF_INTEGRACAO_GRC_EST_NFE TYPE ZCL_INTEGRACAO_GRC_NEW_NFE_ES.
    ENDIF.
    R_IF_INTEGRACAO_GRC_EST_NFE = ZIF_INTEGRACAO_GRC_NEW_NFE_ES~AT_IF_INTEGRACAO_GRC_EST_NFE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE_ES~SET_DS_DATA.

    DATA: I_INBOUND TYPE ZDE_PROCESSO_CAN.

    R_IF_INTEGRACAO_GRC_EST_NFE = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.

    CASE ZCL_STRING=>UPPER( CONV #( I_INFO-DS_FORMATO ) ).
      WHEN 'XML'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_INFO-DS_BODY ) CHANGING DATA = I_INBOUND ).
      WHEN 'JSON'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = I_INBOUND ).
    ENDCASE.

    IF I_INBOUND-I_ID IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_EMPTY_PRC_EXTERNAL-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_EMPTY_PRC_EXTERNAL-MSGNO )
          MSGID  = ZCX_INTEGRACAO=>ZCX_EMPTY_PRC_EXTERNAL-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_EMPTY_PRC_EXTERNAL-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF I_INBOUND-I_DOCNUM IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_EMPTY_DOCNUM-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_EMPTY_DOCNUM-MSGNO )
          MSGID  = ZCX_INTEGRACAO=>ZCX_EMPTY_DOCNUM-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_EMPTY_DOCNUM-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF I_INBOUND-I_DATAEMIS IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_EMPTY_DT_EMISSAO-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_EMPTY_DT_EMISSAO-MSGNO )
          MSGID  = ZCX_INTEGRACAO=>ZCX_EMPTY_DT_EMISSAO-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_EMPTY_DT_EMISSAO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    "Validar Padrão de Data
    FIND REGEX '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' IN I_INBOUND-I_DATAEMIS(10).
    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGNO
                            ATTR1 = 'Data Estorno' )
          MSGID  = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGNO
          MSGTY  = 'E'
          MSGV1  = 'Data Estorno'.
    ENDIF.

    SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_GRC_NEW_NFE_ES~AT_ZSDT0231
      FROM ZSDT0231
     WHERE OBJ_KEY EQ @I_INBOUND-I_ID
       AND DOCNUM  EQ @I_INBOUND-I_DOCNUM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_NOT_IDENTIFY_NF_WRITER-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_NOT_IDENTIFY_NF_WRITER-MSGNO )
          MSGID  = ZCX_INTEGRACAO=>ZCX_NOT_IDENTIFY_NF_WRITER-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_NOT_IDENTIFY_NF_WRITER-MSGNO
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE_ES~SET_SEND_MSG.

    DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_GRC_EST_NFE = ME.

    "Verificar a Função de Cada requisição
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = '/nfe/estnfwriter'.

    CREATE OBJECT LC_INTEGRACAO.

    LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_GRC_NEW_NFE_ES~AT_ID_INTEGRACAO
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

    data: i_inbound type zde_processo_can,
          i_can_dat	type j_1bnfdoc-candat.
    data: vdocnum_est type j_1bnfdoc-docnum.

    r_if_integracao_inject = me.

    e_sucesso = abap_false.

    try .

        case zcl_string=>upper( conv #( i_msg_completa-ds_formato ) ).
          when 'XML'.
            /ui2/cl_json=>deserialize( exporting json = zcl_string=>xml_to_json( i_xml = i_msg_inbound  ) changing data = i_inbound ).
          when 'JSON'.
            /ui2/cl_json=>deserialize( exporting json = i_msg_inbound changing data = i_inbound ).
        endcase.

        select single * into @me->zif_integracao_grc_new_nfe_es~at_zsdt0231
          from zsdt0231
         where obj_key eq @i_inbound-i_id
           and docnum  eq @i_inbound-i_docnum.

        if sy-subrc is not initial.
          raise exception type zcx_integracao
            exporting
              textid = value #( msgid = zcx_integracao=>zcx_not_identify_nf_writer-msgid
                                msgno = zcx_integracao=>zcx_not_identify_nf_writer-msgno )
              msgid  = zcx_integracao=>zcx_not_identify_nf_writer-msgid
              msgno  = zcx_integracao=>zcx_not_identify_nf_writer-msgno
              msgty  = 'E'.
        endif.

        "Validar Padrão de Data
        find regex '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' in i_inbound-i_dataemis(10).
        if sy-subrc is not initial.
          raise exception type zcx_integracao
            exporting
              textid = value #( msgid = zcx_integracao=>zcx_data_errada-msgid
                                msgno = zcx_integracao=>zcx_data_errada-msgno
                                attr1 = 'Data Estorno' )
              msgid  = zcx_integracao=>zcx_data_errada-msgid
              msgno  = zcx_integracao=>zcx_data_errada-msgno
              msgty  = 'E'
              msgv1  = 'Data Estorno'.
        endif.

        try .

            select single * into @data(wa_j_1bnfdoc)
              from j_1bnfdoc
             where docnum eq @me->zif_integracao_grc_new_nfe_es~at_zsdt0231-docnum.

            if wa_j_1bnfdoc-cancel eq abap_false.

              i_can_dat = i_inbound-i_dataemis(4) && i_inbound-i_dataemis+5(2) && i_inbound-i_dataemis+8(2).

              "Data de Emissão somente do dia
              if i_can_dat ne sy-datum.

                data: lv_datum type c length 10.
                write sy-datum to lv_datum .

                raise exception type zcx_integracao
                  exporting
                    textid = value #( msgid = zcx_integracao=>zcx_erro_dt_estorno-msgid
                                      msgno = zcx_integracao=>zcx_erro_dt_estorno-msgno
                                      attr1 = lv_datum )
                    msgid  = zcx_integracao=>zcx_erro_dt_estorno-msgid
                    msgno  = zcx_integracao=>zcx_erro_dt_estorno-msgno
                    msgty  = 'E'
                    msgv1  = conv #( lv_datum ).

              endif.

              data fiscal type ref to zcl_fiscal.
              create object fiscal.

              fiscal->estornar(
                exporting
                  i_doc_number = me->zif_integracao_grc_new_nfe_es~at_zsdt0231-docnum
                  i_ref_type   = space
                  i_ref_key    = space
                  i_can_dat    = i_can_dat
                  i_bapi_wait  = abap_true
                importing
                  e_docnum     = data(e_docnum)
                  e_retorno    = data(e_retorno)
                receiving
                  r_gerou      =  data(r_gerou)
              ).

              if r_gerou eq abap_false.
                read table e_retorno into data(wa_retorno) index 1.
                if sy-subrc is initial.
                  message id wa_retorno-id type wa_retorno-type number wa_retorno-number
                     with wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4 into data(ms_erro).
                  ms_erro = zcl_string=>convert_to_utf8( ms_erro ).
                  ms_erro = zcl_string=>tira_acentos( ms_erro ).
                endif.
              endif.

              clear: fiscal.

              wait up to 5 seconds.

              "US172357 cancela nota do produtor se existir
              select single *
                from zib_nfe_forn
                into @data(wnfe_forn)
                where docnum_ref = @me->zif_integracao_grc_new_nfe_es~at_zsdt0231-docnum.
              if sy-subrc = 0.
                select single docnum
                      from j_1bnfe_active
                      into @data(vl_docnum)
                  where regio   = @wnfe_forn-nu_chave(2)
                    and nfyear  = @wnfe_forn-nu_chave+2(2)
                    and nfmonth = @wnfe_forn-nu_chave+4(2)
                    and stcd1   = @wnfe_forn-nu_chave+6(14)
                    and model   = @wnfe_forn-nu_chave+20(2)
                    and serie   = @wnfe_forn-nu_chave+22(3)
                    and nfnum9  = @wnfe_forn-nu_chave+25(9)
                    and docnum9 = @wnfe_forn-nu_chave+34(9)
                    and cdv     = @wnfe_forn-nu_chave+43(1).
                if sy-subrc = 0.
                  select single * into wa_j_1bnfdoc
                     from j_1bnfdoc
                     where docnum eq vl_docnum.

                  if wa_j_1bnfdoc-cancel eq abap_false.
                    call function 'J_1B_NF_DOCUMENT_CANCEL'
                      exporting
                        doc_number               = vl_docnum
                        ref_type                 = space
                        ref_key                  = space
                        can_dat                  = sy-datum
                      importing
                        doc_number               = vdocnum_est
                      exceptions
                        document_not_found       = 1
                        cancel_not_possible      = 2
                        nf_cancel_type_not_found = 3
                        database_problem         = 4
                        docum_lock               = 5
                        nfe_cancel_simulation    = 6
                        others                   = 7.
                    if sy-subrc eq 0.
                      call function 'BAPI_TRANSACTION_COMMIT'
                        exporting
                          wait = 'X'.
                    else.

                    endif.
                  endif.
                endif.
              endif.
              "US172357 cancela nota do produtor se existir
            else.

              select single * into @wa_j_1bnfdoc
                from j_1bnfdoc
               where docref eq @me->zif_integracao_grc_new_nfe_es~at_zsdt0231-docnum.

              if sy-subrc is initial.
                r_gerou  = abap_true.
                e_docnum = wa_j_1bnfdoc-docnum.

                try .
                    zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = me->zif_integracao_grc_new_nfe_es~at_zsdt0231-docnum
                      )->set_registro( exporting i_docnum = me->zif_integracao_grc_new_nfe_es~at_zsdt0231-docnum i_sem_bloqueio = abap_true
                      )->set_clear_log_erro(
                      ).
                  catch cx_root.
                endtry.

              endif.

            endif.

          catch zcx_doc_eletronico into data(ex_doc_eletronico).
            clear: fiscal.
            ms_erro = ex_doc_eletronico->get_longtext( ).
            ms_erro = zcl_string=>convert_to_utf8( ms_erro ).
            ms_erro = zcl_string=>tira_acentos( ms_erro ).

          catch cx_root into data(ex_root).
            clear: fiscal.
            ms_erro = ex_root->get_text( ).
            ms_erro = zcl_string=>convert_to_utf8( ms_erro ).
            ms_erro = zcl_string=>tira_acentos( ms_erro ).
        endtry.

        case r_gerou.
          when abap_true.
            e_nm_code      = '200'.
            e_msg_erro     = 'Ok'.
            e_msg_outbound = '{ ' && cl_abap_char_utilities=>newline
                                  && '"protocolo" : " ' && zcl_string=>lpad( i_str  = conv #( i_msg_completa-id_integracao ) i_qtd  = 15 i_char = '0'  ) &&  '", ' && cl_abap_char_utilities=>newline
                                  && '"docnum_estorno" : "' && e_docnum && '" ' && cl_abap_char_utilities=>newline
                             && ' }'.
          when abap_false.
            e_nm_code      = '200'.
            e_msg_erro     = 'Ok'.
            e_msg_outbound = '{ ' && cl_abap_char_utilities=>newline && '"protocolo" : " ' &&
                                   zcl_string=>lpad( i_str  = conv #( i_msg_completa-id_integracao ) i_qtd  = 15 i_char = '0'  ) &&  '"' && cl_abap_char_utilities=>newline && '}'.
        endcase.

      catch zcx_integracao into data(ex_integracao).

        e_nm_code  = '400'.
        e_msg_erro = 'Bad Request'.
        ms_erro    = ex_integracao->zif_error~get_msg_erro( ).
        e_msg_outbound = '{ ' && cl_abap_char_utilities=>newline && '"protocolo" : " ' &&
                               zcl_string=>lpad( i_str  = conv #( i_msg_completa-id_integracao ) i_qtd  = 15 i_char = '0'  ) &&  '", ' &&
                            ' "erro" : "' && ms_erro && '"'
                            && cl_abap_char_utilities=>newline && '}'.
    endtry.

    e_sucesso = abap_true.

  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.
ENDCLASS.
