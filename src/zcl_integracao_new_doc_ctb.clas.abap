CLASS ZCL_INTEGRACAO_NEW_DOC_CTB DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES ZIF_INTEGRACAO_INJECT. "ZDE_NEW_DOC_CTB_T
    INTERFACES ZIF_INTEGRACAO_NEW_DOC_CTB. "ZDE_NEW_DOC_CTB_T

    METHODS CONSTRUCTOR .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_NEW_DOC_CTB IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE    = ZIF_INTEGRACAO=>AT_ID_INTERFACE_CTB_NEW_DOC.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO   = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_INBOUND.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL        = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_SINCRONIA    = ZIF_INTEGRACAO=>AT_TP_SINCRONIA_SINCRONA.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_OPUS  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_OPUS_NAO.
    ME->ZIF_INTEGRACAO_INJECT~AT_SEND_AUTENTICAO = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_SEND_NAO.
    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-TP_REFERENCIA = 'CTB_NEW_DOC'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.
    """""""""""""""""""""""""""""""""""""""

    r_if_integracao_inject = me.
    e_sucesso = abap_false.

    DATA: i_inbound TYPE zde_new_doc_ctb.

    me->zif_integracao_new_doc_ctb~set_ds_data( i_info = VALUE #( ds_body = i_msg_inbound ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Gravar na table zib_contabil """"""""""""""""""""""""""""""""""""

    /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = i_inbound ).

    SELECT SINGLE * INTO @me->zif_integracao_new_doc_ctb~at_zfit0158
      FROM zfit0158
     WHERE obj_key EQ @i_inbound-ch_referencia.

    IF sy-subrc IS INITIAL AND me->zif_integracao_new_doc_ctb~at_zfit0158-belnr IS NOT INITIAL.
      e_sucesso = abap_true.
    ENDIF.

    CASE e_sucesso.
      WHEN abap_false.

        IF me->zif_integracao_new_doc_ctb~at_zfit0158-ck_aguardar NE abap_true.

          me->zif_integracao_new_doc_ctb~at_zfit0158-protocolo   = me->zif_integracao_new_doc_ctb~at_id_integracao.
          me->zif_integracao_new_doc_ctb~at_zfit0158-obj_key     = i_inbound-ch_referencia.
          me->zif_integracao_new_doc_ctb~at_zfit0158-ck_aguardar = abap_true.
          MODIFY zfit0158 FROM me->zif_integracao_new_doc_ctb~at_zfit0158.
          COMMIT WORK AND WAIT.

          TRY .
              me->zif_integracao_new_doc_ctb~set_new_doc_contabil(
                   EXPORTING
                      i_dados = i_inbound
                   IMPORTING
                      e_documento = me->zif_integracao_new_doc_ctb~at_bkpf
                      e_erros     = DATA(e_erros)
              ).
              e_sucesso = abap_true.
              DATA(lc_registro_criado) = abap_true.

              me->zif_integracao_new_doc_ctb~at_zfit0158-belnr = me->zif_integracao_new_doc_ctb~at_bkpf-belnr.
              me->zif_integracao_new_doc_ctb~at_zfit0158-bukrs = me->zif_integracao_new_doc_ctb~at_bkpf-bukrs.
              me->zif_integracao_new_doc_ctb~at_zfit0158-gjahr = me->zif_integracao_new_doc_ctb~at_bkpf-gjahr.
              me->zif_integracao_new_doc_ctb~at_zfit0158-ck_aguardar = abap_false.
              MODIFY zfit0158 FROM me->zif_integracao_new_doc_ctb~at_zfit0158.
              COMMIT WORK AND WAIT.

            CATCH zcx_integracao INTO DATA(ex_integracao).
              DATA(ms_erro) = ex_integracao->zif_error~get_msg_erro( ).
              me->zif_integracao_new_doc_ctb~at_zfit0158-ck_aguardar = abap_false.
              MODIFY zfit0158 FROM me->zif_integracao_new_doc_ctb~at_zfit0158.
              COMMIT WORK AND WAIT.

            CATCH zcx_error INTO DATA(ex_erro).
              ms_erro = ex_erro->zif_error~get_msg_erro( ).
              me->zif_integracao_new_doc_ctb~at_zfit0158-ck_aguardar = abap_false.
              MODIFY zfit0158 FROM me->zif_integracao_new_doc_ctb~at_zfit0158.
              COMMIT WORK AND WAIT.

            CATCH cx_root INTO DATA(ex_root).
              ms_erro = ex_root->get_text( ).
              me->zif_integracao_new_doc_ctb~at_zfit0158-ck_aguardar = abap_false.
              MODIFY zfit0158 FROM me->zif_integracao_new_doc_ctb~at_zfit0158.
              COMMIT WORK AND WAIT.

          ENDTRY.

        ELSE.
          e_sucesso = abap_true.
        ENDIF.

      WHEN abap_true.
        lc_registro_criado = abap_false.
    ENDCASE.

    CLEAR: me->zif_integracao_new_doc_ctb~at_retorn.

    CASE e_sucesso.
      WHEN abap_true.

        DATA: lc_erro TYPE zde_erro.

        LOOP AT e_erros INTO DATA(wa_erros).
          lc_erro-erro = wa_erros-message.
          APPEND lc_erro TO me->zif_integracao_new_doc_ctb~at_retorn-erros.
          "IF LC_ERRO IS NOT INITIAL.
          "  LC_ERRO = LC_ERRO && ','.
          "ENDIF.
          "MS_ERRO = ZCL_STRING=>TIRA_ACENTOS( CONV #( WA_ERROS-MESSAGE ) ).
          "MS_ERRO = ZCL_STRING=>CONVERT_TO_UTF8( MS_ERRO ).
          "LC_ERRO = LC_ERRO && '{' && |"erro" : "{ MS_ERRO }"| && '}'.
        ENDLOOP.

        me->zif_integracao_new_doc_ctb~at_retorn-ch_referencia = me->zif_integracao_new_doc_ctb~at_zfit0158-obj_key.
        me->zif_integracao_new_doc_ctb~at_retorn-protocolo     = me->zif_integracao_new_doc_ctb~at_zfit0158-protocolo.
        me->zif_integracao_new_doc_ctb~at_retorn-gjahr         = me->zif_integracao_new_doc_ctb~at_zfit0158-gjahr.
        me->zif_integracao_new_doc_ctb~at_retorn-bukrs         = me->zif_integracao_new_doc_ctb~at_zfit0158-bukrs.
        me->zif_integracao_new_doc_ctb~at_retorn-belnr         = me->zif_integracao_new_doc_ctb~at_zfit0158-belnr.
        me->zif_integracao_new_doc_ctb~at_retorn-wait          = me->zif_integracao_new_doc_ctb~at_zfit0158-ck_aguardar.

        IF me->zif_integracao_new_doc_ctb~at_bkpf IS INITIAL AND
           me->zif_integracao_new_doc_ctb~at_zfit0158-belnr IS NOT INITIAL.
          SELECT SINGLE * INTO @me->zif_integracao_new_doc_ctb~at_bkpf
            FROM bkpf
           WHERE bukrs EQ @me->zif_integracao_new_doc_ctb~at_zfit0158-bukrs
             AND belnr EQ @me->zif_integracao_new_doc_ctb~at_zfit0158-belnr
             AND gjahr EQ @me->zif_integracao_new_doc_ctb~at_zfit0158-gjahr.
        ENDIF.

        IF me->zif_integracao_new_doc_ctb~at_bkpf-belnr IS NOT INITIAL.
          DATA rldnr_l116c10r3367 TYPE rldnr.
          CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
            IMPORTING
              e_rldnr       = rldnr_l116c10r3367
            EXCEPTIONS
              not_found     = 1
              more_than_one = 2.
          IF sy-subrc = 0.
*---> S4 MIGRATION 10/07/2023 - MA
*CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
*  EXPORTING
*    I_RLDNR = RLDNR_L116C10R3367
*    I_BUKRS = AT_BKPF-BUKRS
*    I_BELNR = AT_BKPF-BELNR
*    I_GJAHR = AT_BKPF-GJAHR
*  IMPORTING
*    ET_BSEG = ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_BSEG
*  EXCEPTIONS NOT_FOUND = 1.
            CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
              EXPORTING
                i_rldnr   = rldnr_l116c10r3367
                i_bukrs   = me->zif_integracao_new_doc_ctb~at_bkpf-bukrs
                i_belnr   = me->zif_integracao_new_doc_ctb~at_bkpf-belnr
                i_gjahr   = me->zif_integracao_new_doc_ctb~at_bkpf-gjahr
              IMPORTING
                et_bseg   = me->zif_integracao_new_doc_ctb~at_bseg
              EXCEPTIONS
                not_found = 1.
*<--- S4 MIGRATION 10/07/2023 - MA
          ENDIF.
          IF sy-subrc <> 0 OR lines( me->zif_integracao_new_doc_ctb~at_bseg ) = 0.
            sy-subrc = 4.
            sy-dbcnt = 0.
          ELSE.
            sy-dbcnt = lines( me->zif_integracao_new_doc_ctb~at_bseg ).
          ENDIF.

        ENDIF.

        me->zif_integracao_new_doc_ctb~at_retorn-doc_contabil = me->zif_integracao_new_doc_ctb~at_bkpf.
        me->zif_integracao_new_doc_ctb~at_retorn-doc_contabil_itens = me->zif_integracao_new_doc_ctb~at_bseg.

        me->zif_integracao_new_doc_ctb~at_retorn-reversed =
         COND #( LET revserse = me->zif_integracao_new_doc_ctb~at_bkpf-stblg IN WHEN revserse IS INITIAL THEN abap_false ELSE abap_true ).

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        e_nm_code  = '200'.
        e_msg_outbound = zcl_fmcall_base=>abap2json( EXPORTING abap_data = me->zif_integracao_new_doc_ctb~at_retorn ).

        "E_MSG_OUTBOUND = '{ ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "     ' "protocolo" : "' && ZCL_STR=>LPAD( I_STR  = CONV #( ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_ZFIT0158-PROTOCOLO ) I_QTD  = 15 I_CHAR = '0'  )->GET(  ) &&  '" ,' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "     ' "wait"  : "' && ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_ZFIT0158-CK_AGUARDAR &&  '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "     ' "bukrs" : "' && ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_ZFIT0158-BUKRS &&  '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "     ' "belnr" : "' && ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_ZFIT0158-BELNR &&  '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "     ' "gjahr" : "' && ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_ZFIT0158-GJAHR &&  '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "     ' "erros"  : [' && LC_ERRO && '] ' &&
        "     ' }'.

      WHEN abap_false.

        ms_erro = zcl_string=>tira_acentos( ms_erro ).
        ms_erro = zcl_string=>convert_to_utf8( ms_erro ).

        me->zif_integracao_new_doc_ctb~at_retorn-ch_referencia = me->zif_integracao_new_doc_ctb~at_zfit0158-obj_key.
        me->zif_integracao_new_doc_ctb~at_retorn-protocolo     = me->zif_integracao_new_doc_ctb~at_zfit0158-protocolo.
        me->zif_integracao_new_doc_ctb~at_retorn-gjahr         = me->zif_integracao_new_doc_ctb~at_zfit0158-gjahr.
        me->zif_integracao_new_doc_ctb~at_retorn-bukrs         = ''.
        me->zif_integracao_new_doc_ctb~at_retorn-belnr         = ''.
        me->zif_integracao_new_doc_ctb~at_retorn-gjahr         = ''.
        me->zif_integracao_new_doc_ctb~at_retorn-wait          = me->zif_integracao_new_doc_ctb~at_zfit0158-ck_aguardar.
        me->zif_integracao_new_doc_ctb~at_retorn-reversed      = ''.
        APPEND VALUE #( erro = ms_erro ) TO me->zif_integracao_new_doc_ctb~at_retorn-erros.

        e_nm_code  = '200'.
        e_msg_outbound = zcl_fmcall_base=>abap2json( EXPORTING abap_data = me->zif_integracao_new_doc_ctb~at_retorn ).

        "E_MSG_OUTBOUND = '{ ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "    ' "protocolo" : " ' && ZCL_STRING=>LPAD( I_STR  = CONV #( ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_ZFIT0158-PROTOCOLO ) I_QTD  = 15 I_CHAR = '0'  ) &&  '", ' &&
        "    ' "wait"  : "' && ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_ZFIT0158-CK_AGUARDAR &&  '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "    ' "bukrs" : "", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "    ' "belnr" : "", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "    ' "gjahr" : "", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "    ' "erros"  : [{"erro" : "' && MS_ERRO && '"' && '}]' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
        "    ' }'.

    ENDCASE.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_NEW_DOC_CTB~GET_INSTANCE.
    IF ZIF_INTEGRACAO_NEW_DOC_CTB~AT_INTEGRACAO_NEW_DOC_CTB IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_NEW_DOC_CTB~AT_INTEGRACAO_NEW_DOC_CTB TYPE ZCL_INTEGRACAO_NEW_DOC_CTB.
    ENDIF.
    R_ZIF_INTEGRACAO_NEW_DOC_CTB = ZIF_INTEGRACAO_NEW_DOC_CTB~AT_INTEGRACAO_NEW_DOC_CTB.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_NEW_DOC_CTB~GET_VALIDAR_DADOS.
    "Validar JSON de Entrada

    R_ZIF_INTEGRACAO_NEW_DOC_CTB = ME.

    IF ZCL_STR=>MATCHER_DATE( I_DATE = ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_DOC_CTB-DT_DOCUMENTO ) IS INITIAL.
      "Erro
    ENDIF.

    IF ZCL_STR=>MATCHER_DATE( I_DATE = ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_DOC_CTB-DT_LANCAMENTO ) IS INITIAL.
      "Erro
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_NEW_DOC_CTB~SET_DS_DATA.
    R_ZIF_INTEGRACAO_NEW_DOC_CTB = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.
    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_DOC_CTB ).
    ME->ZIF_INTEGRACAO_NEW_DOC_CTB~GET_VALIDAR_DADOS( ).

    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_DOC_CTB-CH_REFERENCIA.
    CHECK ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_DOC_CTB-CH_REFERENCIA IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_INTEGRACAO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
                          MSGNO = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO )
        MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
        MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD zif_integracao_new_doc_ctb~set_new_doc_contabil.

    " 01.11.2024 - 156939 - RAMON -->

    DATA: number        TYPE tbtcjob-jobcount,
          name          TYPE tbtcjob-jobname,
          t_rsparams    TYPE rsparams_tt,
          w_rsparams    TYPE rsparams,
          lv_ret_json   TYPE string,
          lv_dados_json TYPE string,
          lv_erro_json  TYPE string.

    CONCATENATE 'JOB_DOC_CTB' i_dados-nu_doc_referencia INTO name SEPARATED BY '_'.

    lv_dados_json = /ui2/cl_json=>serialize( data = i_dados ).

    "solicitar e Aguardar execução do job
    DATA(lv_json_ret) = zcl_job=>insert_job_fila_escalonamento( EXPORTING
                                             i_nome_job      = name
                                             i_report        = 'ZFI_R_NEW_DOC_CONTABIL'
                                             i_user_job      = sy-uname
                                             i_processar_retorno = abap_true
                                             i_dados_processar = lv_dados_json
                                             i_wait_schedule = abap_true
                                             i_wait_finish   = abap_true ).

    SPLIT lv_json_ret AT '###' INTO lv_ret_json lv_erro_json.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_ret_json
      CHANGING
        data = e_documento.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_erro_json
      CHANGING
        data = e_erros.

    " 01.11.2024 - 156939 - RAMON <--

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_NEW_DOC_CTB~SET_SEND_MSG.

    DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.

    CLEAR: E_ZINTEGRACAO_LOG.

    R_ZIF_INTEGRACAO_NEW_DOC_CTB = ME.

    "Verificar a Função de Cada requisição
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = '/ctb/newdoccontabil'.

    CREATE OBJECT LC_INTEGRACAO.

    LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_NEW_DOC_CTB~AT_ID_INTEGRACAO
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO( IMPORTING E_DATA_RETORNO = DATA(E_DATA_RETORNO) E_ZINTEGRACAO_LOG = E_ZINTEGRACAO_LOG
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = DATA(E_INTEGRACAO)
      )->FREE(
      ).

    E_MSG = E_DATA_RETORNO.
    E_PROTOCOLO = ZCL_STRING=>LPAD( I_STR  = CONV #( E_INTEGRACAO-ID_INTEGRACAO ) I_QTD  = 15 I_CHAR = '0'  ).

    CLEAR: LC_INTEGRACAO.

  ENDMETHOD.
ENDCLASS.
