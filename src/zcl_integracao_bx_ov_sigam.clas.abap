class ZCL_INTEGRACAO_BX_OV_SIGAM definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  types:
    BEGIN OF ty_erros,
        mensagem TYPE char100,
      END OF ty_erros .

  data:
    tl_erro TYPE  TABLE OF ty_erros .
  types:
    BEGIN OF ty_erro,
        erros LIKE tl_erro,
      END OF ty_erro .
  data:
    BEGIN OF zde_data_request,
        num_ov           TYPE zfit0026-vbeln,
        dt_pgto          TYPE zfit0026-data_pgto,
        vlr_reais        TYPE zfit0026-mont_rbdo,
        vlr_dolar        TYPE zfit0026-vlr_usd_sigam,
        moeda_cont_sigam TYPE zfit0026-moeda_sigam,
        num_compra_adt   TYPE zfit0026-num_comp_adiant,
        tx_cont_sigam    TYPE zfit0026-taxa_sigam,
        objkey_sigam     TYPE zfit0026-objkey_sigam,
        cond_pag         TYPE zfit0026-zterm,
        tp_bx_valor      TYPE zfit0026-tp_baixa_vlr_ov,
      END OF zde_data_request .
  data:
    tb_erro TYPE TABLE OF string .
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '187' ##NO_TEXT.
  data:
    BEGIN OF zde_data_response,
        erros TYPE TABLE OF ty_erro,
      END OF zde_data_response .
  data:
    BEGIN OF zde_data_response_doc,
        doc_contabil TYPE ZIB_CONTABIL_CHV-belnr,
      END OF zde_data_response_doc .

  methods Z_GERA_DOCUMENTO_CONTABIL
    importing
      !WA_ZFIT0026 type ZFIT0026
    exporting
      !E_STATUS_CODE type CHAR03
    returning
      value(R_MSG_ERRO) type STRING .
  methods Z_GERA_CONTABIL
    importing
      !I_MSG_INBOUND type STRING
      !I_MSG_COMPLETA type ZINTEGRACAO
      !I_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
    exporting
      !E_MSG_OUTBOUND type STRING
      !E_SUCESSO type CHAR01
      !E_NM_CODE type CHAR03
      !E_MSG_ERRO type STRING
      !WA_ZFIT0026 type ZFIT0026
    returning
      value(R_IF_INTEGRACAO_INJECT) type ref to ZIF_INTEGRACAO_INJECT .
  methods CONSTRUCTOR
    exceptions
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_BX_OV_SIGAM IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: w_data_inbound   TYPE zsdt0325_inb_cliente,
          lit_data_inbound TYPE zsdt0325_inbcli_t,
          w_tx             TYPE zsdt0327tx,
          lva_kna1         TYPE kna1,
          lva_tabix        TYPE sy-tabix,
          lva_duplic_flg   TYPE c,
          lva_kunnret      TYPE kna1-kunnr,
          lva_erro         TYPE string,
          lva_type         TYPE dd01v-datatype,
          l_erro(1).

    DATA: lwa_data_response_erro LIKE zde_data_response.
    DATA: lwa_data_request LIKE zde_data_request,
          lwa_data_response_doc  LIKE zde_data_response_doc.


    TYPES:
      BEGIN OF ty_erro,
        mensagem TYPE char100,
      END OF ty_erro.

    DATA: it_mensagem TYPE TABLE OF  ty_erro,
          wa_mensagem TYPE  ty_erro.

    CLEAR: r_msg_erro.

    IF ( me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'POST' AND  me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'PUT' ).
      r_msg_erro = 'Metodo informado não reconhecido!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_data_inbound ).
    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

    "FIELD-SYMBOLS: <fs_erro> TYPE lit_data_inbound.
    APPEND INITIAL LINE TO lwa_data_response_erro-erros ASSIGNING FIELD-SYMBOL(<fs_erro>).

    IF lwa_data_request-num_ov IS INITIAL.
      wa_mensagem = 'O num_ov é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      " <fs_erro> = wa_mensagem.
      l_erro = 'X'.
      " r_msg_erro = 'O num_ov é um campo obrigatório'.
      "RETURN.
    ENDIF.

    IF lwa_data_request-dt_pgto IS INITIAL.
      wa_mensagem =  'A dt_pgto é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      " RETURN.
    ENDIF.

    IF lwa_data_request-vlr_reais IS INITIAL.
      wa_mensagem =  'O vlr_reais é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      "RETURN.
    ENDIF.

    IF lwa_data_request-vlr_dolar IS INITIAL.
      wa_mensagem =  'O vlr_dolar é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      " RETURN.
    ENDIF.

    IF lwa_data_request-moeda_cont_sigam IS INITIAL.
      wa_mensagem =  'A moeda_cont_sigam é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      "RETURN.
    ENDIF.

    IF lwa_data_request-num_compra_adt IS INITIAL.
      wa_mensagem =  'O num_compra_adt é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      " RETURN.
    ENDIF.

    IF lwa_data_request-tx_cont_sigam IS INITIAL.
      wa_mensagem =  'A tx_cont_sigam é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      "RETURN.
    ENDIF.

    IF lwa_data_request-objkey_sigam IS INITIAL.
      wa_mensagem =  'O objKey_sigam é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      "RETURN.
    ENDIF.

    IF lwa_data_request-cond_pag IS INITIAL.
      wa_mensagem =  'A cond_pag é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      " RETURN.
    ELSE.
       SELECT SINGLE *
      FROM TVARVC INTO @DATA(LWA_TVARVC)
     WHERE NAME EQ 'COND_PGTO_ACERT_INS_SIGAM'
       AND LOW  EQ @lwa_data_request-cond_pag.

    IF SY-SUBRC is not INITIAL.
      "IF lwa_data_request-cond_pag NE 'C001' AND lwa_data_request-cond_pag NE 'C002' AND lwa_data_request-cond_pag NE 'C003'.
        wa_mensagem =  'A cond_pag informada não está prevista para o lançamento.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
        " RETURN.
      ENDIF.
    ENDIF.

    IF lwa_data_request-tp_bx_valor IS INITIAL.
      wa_mensagem =  'O tp_bx_valor é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
      "RETURN.
    ENDIF.

    "ObjKey_SIGAM
    "Criar essa coluna na tabela - Será uma chave de referencia que o sigam vai mandar, onde o Sap deverá verificar se já existe antes de executar.
    "Se existir, apenas devolve o doc contabil (ZFIT0026-DOCNUM) para o sigam / se não existir, executar o programa.

    select single * into @data(w_ZFIT0026) from ZFIT0026 where OBJKEY_SIGAM = @lwa_data_request-OBJKEY_SIGAM and VBELN eq @lwa_data_request-NUM_OV.

      if sy-SUBRC is INITIAL.
         IF w_ZFIT0026-STATUS EQ 'P'.
            wa_mensagem =  'Favor aguardar retorno do Doc Contabil.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
         ELSEIF ( w_ZFIT0026-STATUS eq 'G' or w_ZFIT0026-STATUS eq 'S') AND w_ZFIT0026-docnum IS NOT INITIAL.
           lwa_data_response_doc-doc_contabil = w_ZFIT0026-docnum.
           r_msg_erro = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response_doc ).
         ENDIF.
      endif.



    IF l_erro IS NOT INITIAL.
      r_msg_erro = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response_erro ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
    DATA: LWA_DATA_REQUEST       LIKE ZDE_DATA_REQUEST,
          LWA_DATA_RESPONSE_ERRO LIKE ZDE_DATA_RESPONSE,
          LWA_DATA_RESPONSE_DOC  LIKE ZDE_DATA_RESPONSE_DOC.


    DATA: VLR_JROS TYPE P DECIMALS 2.
    DATA: VLR_MULT TYPE P DECIMALS 2.
    DATA: FAT_PROP TYPE P DECIMALS 9.
    DATA: PROP_JUROS TYPE P DECIMALS 9.
    DATA: PROP_MULTA TYPE P DECIMALS 9.
    DATA: V_TAXA_CONTABIL TYPE KURRF.

    DATA: V_doc_contabil TYPE ZIB_CONTABIL_CHV-BELNR.

    DATA: V_processou(1),
          X_TOTAL TYPE NETWR.

    DATA: CALC                 TYPE C,
          V_DATA_AUX           TYPE DATUM,
          V_DATA_VENCIMENTO    TYPE DATUM,
          VLR_MULTA            TYPE VBRK-NETWR,
          VLR_JUROS            TYPE VBRK-NETWR,
          DT_PGTO_ANTER        TYPE DATUM,
          MONT_RBDO_ANTER      TYPE VBRK-NETWR,
          V_MONT_AUX           TYPE ZFIT0026-VLR_MULTA_RBDO,
          V_MONT_MOEDA_FIX     TYPE NETWR,
          V_MONT_MI            TYPE NETWR,
          V_TAXA               TYPE KURRF,
          V_MONT_MOEDA_PARC    TYPE NETWR,
          WA_ZSDT0053          TYPE ZSDT0053,
          W_WERKS(4),
          W_ZIB_CONTABIL_CHV   TYPE  ZIB_CONTABIL_CHV,
          IT_ZIB_CONTABIL_ERR3 TYPE TABLE OF ZIB_CONTABIL_ERR,
          WA_ERRO3             TYPE ZIB_CONTABIL_ERR.

    DATA: E_JSON    TYPE STRING,
          L_ERRO(1).

    DATA: "zib_contabil_wa type zib_contabil,
      CONT        TYPE NUMC10,
      OBJ_KEY     TYPE AWKEY,
      SGTXT       TYPE SGTXT,
      DATA(10)    TYPE C,
      DIA(2)      TYPE C,
      MES(2)      TYPE C,
      ANO(4)      TYPE C,
      WA_ZIB_CONT TYPE ZIB_CONTABIL,
      WA_0159     TYPE ZSDT0159,
      WA_KNA1     TYPE KNA1.

    DATA: DATA_SUM TYPE DATUM,
          TAXA     TYPE ZFIT0026-TAXA.


    DATA: VG_KNA1       TYPE KNA1.

    TYPES:
      BEGIN OF TY_ERRO,
        MENSAGEM TYPE CHAR100,
      END OF TY_ERRO.

    DATA: IT_MENSAGEM TYPE TABLE OF  TY_ERRO,
          WA_MENSAGEM TYPE  TY_ERRO.


    DATA: IT_ZSDT0040 TYPE TABLE OF ZSDT0040,
          WA_ZSDT0040 TYPE ZSDT0040,
          WA_ZSDT0041 TYPE ZSDT0041,
          WA_ZSDT0090 TYPE ZSDT0090,
          WA_ZFIT0026 TYPE ZFIT0026.

    DATA: VBELN_AUX     TYPE VBAK-VBELN,
          L_STATUS(1)   TYPE C,
          L_MESSAGE(64) TYPE C.
    DATA: WA_VBAK_VALOR TYPE VBAK.
    DATA: P_ERRO(1) TYPE C.

    DATA: B TYPE NUMC10.

    DATA: P_ZID       TYPE NUMC10,
          ZFIT0026_WA TYPE ZFIT0026,
          SOMA        TYPE ZFIT0026-MONT_MOEDA,

          TEXTO_01    TYPE C LENGTH 100 VALUE 'Valor do montante menor do que o valor',
          TEXTO_02    TYPE C LENGTH 100 VALUE 'do lançamento!',

          TEXTO_03    TYPE C LENGTH 100 VALUE 'Não existe montante suficiente',
          TEXTO_04    TYPE C LENGTH 100 VALUE 'para esse lançamento'.

    DATA: RAZAO_ESPECIAL TYPE C.

    DATA: IT_ZIB_CONTABIL_CHV TYPE STANDARD TABLE OF ZIB_CONTABIL_CHV,
          WA_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV,
          IT_Z0159            TYPE STANDARD TABLE OF ZSDT0159,
          WA_Z0159            TYPE ZSDT0159.

    DATA: VL_GDATU TYPE GDATU_INV,
          V_KURRF  TYPE KURRF.


    DATA: IT_VBAP     TYPE TABLE OF VBAP,
          IT_VBKD     TYPE TABLE OF VBKD,
          IT_T052     TYPE TABLE OF T052,
          WA_T052     TYPE T052,
          IT_ZSDT0090 TYPE STANDARD TABLE OF ZSDT0090,
          IT_ZSDT0041 TYPE TABLE OF ZSDT0041,
          IT_KNA1     TYPE TABLE OF KNA1,
          IT_ZFIT0026 TYPE TABLE OF ZFIT0026.
    "ja tendo o numero da OV
    DATA: MONT_OV         TYPE NETWR,
          MONT_PARCIAL    TYPE NETWR,
          MONT_USADO      TYPE NETWR,
          JUROS_CALCULADO TYPE NETWR,
          DATUM           LIKE SY-DATUM.

    TYPES: BEGIN OF TY_ZFIT0026_GERA,
             VBELN   TYPE ZFIT0026-VBELN,
             SEQ     TYPE ZFIT0026-SEQ,
             OBJ_KEY TYPE ZFIT0026-OBJ_KEY,
           END OF TY_ZFIT0026_GERA,

           BEGIN OF TY_ZIB_CONTABIL_CONT,
             OBJ_KEY   TYPE ZIB_CONTABIL-OBJ_KEY,
             SEQITEM   TYPE ZIB_CONTABIL-SEQITEM,
             BUKRS     TYPE ZIB_CONTABIL-BUKRS,
             INTERFACE TYPE ZIB_CONTABIL-INTERFACE,
             XBLNR     TYPE ZIB_CONTABIL-XBLNR,
           END OF TY_ZIB_CONTABIL_CONT.

    DATA: IT_ZFIT0026_GERA    TYPE TABLE OF TY_ZFIT0026_GERA,
          WA_ZFIT0026_GERA    TYPE TY_ZFIT0026_GERA,
          TABIX               TYPE SY-TABIX,
          SEQ_NOVO            TYPE ZFIT0026-SEQ,
          WA_VERIFICA_ZIB     TYPE ZIB_CONTABIL,
          WA_ZIB_CONTABIL_AUX TYPE ZIB_CONTABIL,
          IT_ZIB_CONT         TYPE TABLE OF TY_ZIB_CONTABIL_CONT,
          "wa_zib_cont         TYPE ty_zib_contabil_cont,
          VBELN_AUX_P         TYPE ZIB_CONTABIL-XBLNR,
          "wa_kna1             TYPE kna1,
          ZIB_CONTABIL_WA     TYPE ZIB_CONTABIL.


    DATA: W_ZFIT0026 TYPE ZFIT0026,
          QTD        TYPE SY-TABIX,
          I          TYPE I,
          V_TAXA1    TYPE UKURS_CURR.

    DATA: LVA_MSG_ERRO  TYPE STRING.

    R_IF_INTEGRACAO_INJECT = ME.

    "CHECK i_msg IS NOT INITIAL.

    "/ui2/cl_json=>deserialize( EXPORTING json = i_msg CHANGING data = lit_data_inbound ).
    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = LWA_DATA_REQUEST ).

    R_IF_INTEGRACAO_INJECT = ME.

    CLEAR: E_MSG_ERRO, E_SUCESSO, E_NM_CODE, E_MSG_OUTBOUND.", lwa_data_response.

    IF I_MSG_INBOUND IS NOT INITIAL.
      /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = LWA_DATA_REQUEST ).
    ENDIF.

    ME->ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND( EXPORTING I_DATA_INBOUND =  I_MSG_INBOUND IMPORTING E_STATUS_CODE  =  DATA(_STATUS_CODE)  RECEIVING R_MSG_ERRO = LVA_MSG_ERRO ).

    IF LVA_MSG_ERRO IS NOT INITIAL.

      IF _STATUS_CODE IS INITIAL .
        _STATUS_CODE = '400'. "Bad Request
      ENDIF.

      E_SUCESSO      = ABAP_TRUE.
      E_NM_CODE      = _STATUS_CODE.
      E_MSG_OUTBOUND = LVA_MSG_ERRO.
*      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
*                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
*                       '}'.
      RETURN.
    ENDIF.

    IF LWA_DATA_REQUEST IS NOT INITIAL."1
      APPEND INITIAL LINE TO LWA_DATA_RESPONSE_ERRO-ERROS ASSIGNING FIELD-SYMBOL(<FS_ERRO>).

      SELECT * FROM VBAK INTO TABLE @DATA(IT_VBAK)
              WHERE VBELN = @LWA_DATA_REQUEST-NUM_OV.

      CHECK NOT IT_VBAK[] IS INITIAL.


      SELECT *
      FROM VBKD
      INTO TABLE IT_VBKD
      FOR ALL ENTRIES IN IT_VBAK
      WHERE VBELN EQ IT_VBAK-VBELN.

      SELECT *
      FROM T052 INTO TABLE IT_T052
      FOR ALL ENTRIES IN  IT_VBKD
      WHERE ZTERM EQ IT_VBKD-ZTERM.


      " Dados de Taxa Travada
      CLEAR: IT_ZSDT0090.
      SELECT *
      FROM ZSDT0090
      INTO TABLE IT_ZSDT0090
      FOR ALL ENTRIES IN IT_VBAK
      WHERE VBELV EQ IT_VBAK-VBELN
      AND ESTORNO   NE 'X' "NAO~TRAZER ESTORNO
      AND CATEGORIA EQ 'C'.

      " Mestre de clientes (parte geral)
      SELECT *
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_VBAK
      WHERE KUNNR EQ IT_VBAK-KUNNR.

      SELECT *
      FROM VBAP
      INTO TABLE IT_VBAP
      FOR ALL ENTRIES IN IT_VBAK
      WHERE VBELN EQ IT_VBAK-VBELN
      AND NOT EXISTS ( SELECT *
                       FROM VBEP
                       WHERE VBELN EQ VBAP~VBELN
                       AND POSNR EQ VBAP~POSNR
                       AND LIFSP EQ '12' ).

      SELECT *
      FROM ZFIT0026
      INTO TABLE @DATA(IT_ZFIT0026_AUX)
      WHERE VBELN = @LWA_DATA_REQUEST-NUM_OV.

      SELECT SINGLE * INTO WA_ZFIT0026 FROM ZFIT0026 WHERE OBJKEY_SIGAM = LWA_DATA_REQUEST-OBJKEY_SIGAM AND VBELN EQ LWA_DATA_REQUEST-NUM_OV.

      IF SY-SUBRC IS NOT INITIAL. "1.1

        WA_ZFIT0026-VBELN =  LWA_DATA_REQUEST-NUM_OV. "api


        "vai na vbap para pegar os itens da OV
*SELECT * FROM vbap INTO TABLE @DATA(it_vbap)
*  WHERE vbeln = '0012126814'.

*        SELECT *
*        FROM VBKD
*        INTO TABLE IT_VBKD
*        FOR ALL ENTRIES IN IT_VBAK
*        WHERE VBELN EQ IT_VBAK-VBELN.
*
*        SELECT *
*        FROM T052 INTO TABLE IT_T052
*        FOR ALL ENTRIES IN  IT_VBKD
*        WHERE ZTERM EQ IT_VBKD-ZTERM.
*
*
*        " Dados de Taxa Travada
*        CLEAR: IT_ZSDT0090.
*        SELECT *
*        FROM ZSDT0090
*        INTO TABLE IT_ZSDT0090
*        FOR ALL ENTRIES IN IT_VBAK
*        WHERE VBELV EQ IT_VBAK-VBELN
*        AND ESTORNO   NE 'X' "NAO~TRAZER ESTORNO
*        AND CATEGORIA EQ 'C'.
*
*        " Mestre de clientes (parte geral)
*        SELECT *
*        FROM KNA1
*        INTO TABLE IT_KNA1
*        FOR ALL ENTRIES IN IT_VBAK
*        WHERE KUNNR EQ IT_VBAK-KUNNR.
*
*        SELECT *
*        FROM VBAP
*        INTO TABLE IT_VBAP
*        FOR ALL ENTRIES IN IT_VBAK
*        WHERE VBELN EQ IT_VBAK-VBELN
*        AND NOT EXISTS ( SELECT *
*                         FROM VBEP
*                         WHERE VBELN EQ VBAP~VBELN
*                         AND POSNR EQ VBAP~POSNR
*                         AND LIFSP EQ '12' ).
*
*        SELECT *
*        FROM ZFIT0026
*        INTO TABLE @DATA(IT_ZFIT0026_AUX)
*        WHERE VBELN = @WA_ZFIT0026-VBELN.

        WA_ZFIT0026-DATA_PGTO = LWA_DATA_REQUEST-DT_PGTO.  "API
        WA_ZFIT0026-ZTERM = LWA_DATA_REQUEST-COND_PAG. "API

        " ---------********** CALCULO DA TAXA DE CAMBIO ---------**********


        READ TABLE IT_ZSDT0090 INTO WA_ZSDT0090 WITH KEY  VBELV = WA_ZFIT0026-VBELN.

        IF SY-SUBRC IS INITIAL AND WA_ZSDT0090-KURRF IS NOT  INITIAL.
          WA_ZFIT0026-TAXA = WA_ZSDT0090-KURRF. "CALCULO
        ELSE.
          DATA: V_DATA  TYPE SY-DATUM,
                V_DATA1 TYPE SY-DATUM.

          V_DATA1 = LWA_DATA_REQUEST-DT_PGTO.

          CONCATENATE V_DATA1+6(2)  V_DATA1+4(2)  V_DATA1(4) INTO V_DATA.

          CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
            EXPORTING
              INPUT  = V_DATA
            IMPORTING
              OUTPUT = DATUM.

          SELECT SINGLE UKURS FROM TCURR
          INTO V_TAXA1
          WHERE GDATU EQ DATUM
          AND KURST EQ 'G'
          AND FCURR EQ 'USD'
          AND TCURR EQ 'BRL'.

          IF SY-SUBRC IS INITIAL.
            WA_ZFIT0026-TAXA = V_TAXA1.
          ENDIF.

        ENDIF.

        WA_ZFIT0026-VLR_USD_SIGAM = LWA_DATA_REQUEST-VLR_DOLAR. "API
        WA_ZFIT0026-NUM_COMP_ADIANT = LWA_DATA_REQUEST-NUM_COMPRA_ADT. "API
        WA_ZFIT0026-TAXA_SIGAM = LWA_DATA_REQUEST-TX_CONT_SIGAM. "API
        WA_ZFIT0026-MOEDA_SIGAM = LWA_DATA_REQUEST-MOEDA_CONT_SIGAM. "API
        WA_ZFIT0026-OBJKEY_SIGAM = LWA_DATA_REQUEST-OBJKEY_SIGAM. "API
        WA_ZFIT0026-TP_BAIXA_VLR_OV = LWA_DATA_REQUEST-TP_BX_VALOR. "API
        WA_ZFIT0026-DATA_REGISTRO = SY-DATUM.
        WA_ZFIT0026-FORMA_PAG = 'P'.
        WA_ZFIT0026-OBSERVACAO = 'LANÇAMENTO INTERFACE DO SIGAM'.

        READ TABLE IT_VBAK INTO DATA(WA_VBAK) WITH KEY VBELN = WA_ZFIT0026-VBELN.

        IF SY-SUBRC IS INITIAL. "2
          WA_ZFIT0026-MOEDA = WA_VBAK-WAERK.
          WA_ZFIT0026-BUKRS = WA_VBAK-BUKRS_VF.

          IF WA_ZFIT0026-MOEDA EQ 'BRL'.
            WA_ZFIT0026-taxa = 1.
          ENDIF.

          LOOP AT IT_ZFIT0026_AUX INTO DATA(WA_ZFIT0026_AUX) WHERE VBELN = WA_ZFIT0026-VBELN.
            MONT_USADO = MONT_USADO + WA_ZFIT0026_AUX-MONT_MOEDA.
            IF WA_ZFIT0026_AUX-DOCNUM IS NOT INITIAL.
              IF WA_ZFIT0026_AUX-VLR_JUROS_CALC IS INITIAL .
                JUROS_CALCULADO = JuRoS_CALCULADO -  WA_ZFIT0026_AUX-VLR_JUROS_RBDO.
              ELSE.
                JUROS_CALCULADO = JuRoS_CALCULADO + WA_ZFIT0026_AUX-VLR_JUROS_CALC.
              ENDIF.
            ENDIF.
          ENDLOOP.

          LOOP AT IT_VBAP INTO DATA(WA_VBAP) WHERE VBELN = WA_ZFIT0026-VBELN.
            MONT_OV = MONT_OV + WA_VBAP-NETWR + WA_VBAP-MWSBP.
          ENDLOOP.

          MONT_PARCIAL = MONT_OV -  MONT_USADO .
          "MONTANTE
          IF WA_ZFIT0026-MOEDA EQ 'USD'.
            WA_ZFIT0026-MONT_RBDO = LWA_DATA_REQUEST-VLR_REAIS / WA_ZFIT0026-TAXA.
          ELSE.
            WA_ZFIT0026-MONT_RBDO = LWA_DATA_REQUEST-VLR_REAIS.
          ENDIF.

          IF WA_ZFIT0026-MONT_RBDO EQ MONT_PARCIAL.
            WA_ZFIT0026-REC_VLR_TOTAL = 'X'.
          ENDIF.

          IF WA_ZFIT0026-MONT_RBDO EQ JUROS_CALCULADO AND MONT_PARCIAL IS INITIAL.
            WA_ZFIT0026-REC_VLR_TOTAL = 'X'.
          ENDIF.

          WA_ZFIT0026-MONT_MOEDA = WA_ZFIT0026-MONT_RBDO.

          IF  WA_ZFIT0026-MOEDA EQ 'USD'.
            WA_ZFIT0026-MONT_MI = WA_ZFIT0026-MONT_RBDO * WA_ZFIT0026-TAXA.
          ELSE.
            WA_ZFIT0026-MONT_MI = WA_ZFIT0026-MONT_RBDO .
          ENDIF.

          READ TABLE IT_VBKD INTO DATA(WA_VBKD) WITH KEY VBELN = WA_VBAK-VBELN BINARY SEARCH.

          IF SY-SUBRC IS INITIAL.
            READ TABLE IT_T052 INTO WA_T052 WITH KEY ZTERM = WA_VBKD-ZTERM.
            IF SY-SUBRC IS INITIAL AND WA_T052-ZDART = 'D'.
              WA_ZFIT0026-DATA_VENC = WA_VBKD-VALDT.
            ENDIF.

          ENDIF.


          "validações

          " Essa função Retorna o próximo dia util...
          " Caso a data entrada seja dia útil, a função retornar ela mesma.

          ZCL_MIRO=>GET_PROXIMO_DIA_UTIL(
          EXPORTING
            I_DATA_BASE         = WA_ZFIT0026-DATA_VENC
            I_SIGNUM            = '+'
            I_CK_DATA_ZLES0145  = ABAP_TRUE
          RECEIVING
            R_DATA      = V_DATA_AUX
          EXCEPTIONS
            ERRO        = 1
            OTHERS      = 2 ).


          IF WA_ZFIT0026-DATA_PGTO > V_DATA_AUX.
            "Insumos
            SELECT  SINGLE * FROM ZSDT0090 INTO WA_ZSDT0090
            WHERE VBELN EQ WA_ZFIT0026-VBELN.

            IF SY-SUBRC = 0.
              SELECT SINGLE * FROM ZSDT0040 INTO WA_ZSDT0040
                WHERE DOC_SIMULACAO EQ WA_ZSDT0090-DOC_SIMULACAO.
            ELSE.

              SELECT SINGLE * FROM ZSDT0041  INTO WA_ZSDT0041
              WHERE VBELN EQ WA_ZFIT0026-VBELN.

              IF SY-SUBRC = 0.
                SELECT SINGLE * FROM ZSDT0040 INTO WA_ZSDT0040
                WHERE DOC_SIMULACAO EQ WA_ZSDT0041-DOC_SIMULACAO.
              ENDIF.
            ENDIF.

            IF  CALC = ABAP_FALSE.
              DATA(D_ATRASO) = ( WA_ZFIT0026-DATA_PGTO - WA_ZFIT0026-DATA_VENC ).
              VLR_MULTA = WA_ZFIT0026-VLR_MULTA_RBDO.

              DATA(DIAS_ANO) = 360.     "Quantidade de dias no ano.
              CLEAR: FAT_PROP.
              FAT_PROP = ( WA_ZSDT0040-JUROS_ANO / 360 ) * D_ATRASO. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

              IF WA_ZFIT0026-REC_VLR_TOTAL EQ ABAP_TRUE. "Se o campo recebimento total for marcado, considerar o juros com base no total da OV.

                CLEAR: VLR_JUROS, VLR_JROS, PROP_MULTA, VLR_MULT.

                VLR_JROS = ( MONT_PARCIAL * FAT_PROP ) / 100.  "Valor do juros com base no valor total da OV.
                VLR_JUROS = VLR_JROS.

              ELSE.

                CLEAR: VLR_JUROS, VLR_JROS, PROP_MULTA, VLR_MULT.
                VLR_JROS = ( MONT_OV * FAT_PROP ) / 100.  "Valor do juros com base no valor total da OV.

                DATA(TOT_OV) = ( VLR_JROS + MONT_OV ). "Total da OV + juros.
                PROP_JUROS = ( VLR_JROS / TOT_OV )  * 100. " Porcentagem proporcional ao valor do juros
                PROP_MULTA = ( VLR_MULTA / TOT_OV )  * 100. " Porcentagem proporcional da multa.
                CLEAR TOT_OV.

                VLR_JUROS = ( PROP_JUROS * WA_ZFIT0026-MONT_RBDO ) / 100.
                VLR_MULTA =  ( PROP_MULTA * WA_ZFIT0026-MONT_RBDO ) / 100.

              ENDIF.

              WA_ZFIT0026-VLR_JUROS_RBDO = VLR_JUROS.
              WA_ZFIT0026-VLR_MULTA_RBDO = VLR_MULTA.

              WA_ZFIT0026-VLR_MULTA_CALC = VLR_MULTA.
              WA_ZFIT0026-VLR_JUROS_CALC = VLR_JUROS.

              CALC = ABAP_TRUE.
              DT_PGTO_ANTER   = WA_ZFIT0026-DATA_PGTO.
              MONT_RBDO_ANTER = WA_ZFIT0026-MONT_RBDO.

            ELSE.

              D_ATRASO = ( WA_ZFIT0026-DATA_PGTO - WA_ZFIT0026-DATA_VENC ).
              VLR_MULTA = WA_ZFIT0026-VLR_MULTA_RBDO.

              CLEAR: FAT_PROP.
              FAT_PROP = ( WA_ZSDT0040-JUROS_ANO / 360 ) * D_ATRASO. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

              IF WA_ZFIT0026-REC_VLR_TOTAL EQ ABAP_TRUE. "Se o campo recebimento total for marcado, considerar o juros com base no total da OV.

                CLEAR: VLR_JUROS, VLR_JROS, PROP_MULTA, VLR_MULT.
                VLR_JROS = ( MONT_PARCIAL * FAT_PROP ) / 100.  "Valor do juros com base no valor total da OV.
                VLR_JUROS = VLR_JROS.

              ELSE.

                CLEAR: VLR_JUROS, VLR_JROS, PROP_MULTA, VLR_MULT.
                VLR_JROS = ( MONT_OV * FAT_PROP ) / 100.  "Valor do juros com base no valor total da OV.

                DATA(TOTAL_OV) = ( VLR_JROS + MONT_OV ). "Total da OV + juros.
                PROP_JUROS = ( VLR_JROS / TOTAL_OV )  * 100. " Porcentagem proporcional ao valor do juros
                PROP_MULTA = ( VLR_MULTA / TOTAL_OV )  * 100. " Porcentagem proporcional da multa.

                VLR_JUROS = ( PROP_JUROS * WA_ZFIT0026-MONT_RBDO ) / 100.
                VLR_MULTA =  ( PROP_MULTA * WA_ZFIT0026-MONT_RBDO ) / 100.

              ENDIF.
            ENDIF.
          ELSE.
            WA_ZFIT0026-VLR_JUROS_CALC = VLR_JUROS.
            WA_ZFIT0026-VLR_MULTA_CALC = VLR_MULTA.
          ENDIF.

          IF WA_ZFIT0026-DATA_PGTO LE WA_ZFIT0026-DATA_VENC.
            WA_ZFIT0026-VLR_MULTA_RBDO = ' '.
            WA_ZFIT0026-VLR_JUROS_RBDO = ' '.
          ENDIF.

          CLEAR V_MONT_AUX.
          V_MONT_AUX = WA_ZFIT0026-MONT_RBDO.

*          IF WA_ZFIT0026-MONT_RBDO > MONT_PARCIAL.
*            WA_MENSAGEM =  'Valor do Montante menor do que o valor do lançamento'.
*            APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
*            L_ERRO = 'X'.
*          ENDIF.
          "Se for 56 ou 50 deverá pegar o valor carimbado no campo MONT_MOEDA e carimbar tambem no campo ZFIT0026-VLR_JUROS_RBDO /
          "Se for <> de 56 e 50 Deverá limpar o campo que calcula os juros ZFIT0026-VLR_JUROS_RBDO para que o campo MONT_RBDO fique igual ao MONT_MOEDA

          IF WA_ZFIT0026-TP_BAIXA_VLR_OV EQ '50' OR  WA_ZFIT0026-TP_BAIXA_VLR_OV EQ '56'.
            IF JUROS_CALCULADO IS INITIAL.
              WA_MENSAGEM =  'Não existe amortização para esta OV. Não há juros calculado.'.
              APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
              L_ERRO = 'X'.
            ELSE.
              IF WA_ZFIT0026-MONT_RBDO > JUROS_CALCULADO.
                WA_MENSAGEM =  'Valor do juros em aberto é menor que o valor do lançamento!'.
                APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
                L_ERRO = 'X'.

              ENDIF.
              WA_ZFIT0026-VLR_JUROS_RBDO = WA_ZFIT0026-MONT_MOEDA.
              WA_ZFIT0026-VLR_MULTA_CALC = ' '.
              WA_ZFIT0026-VLR_JUROS_CALC = ' '.
              WA_ZFIT0026-VLR_MULTA_RBDO = ' '.
              WA_ZFIT0026-MONT_MOEDA = ' '.
              WA_ZFIT0026-MONT_Mi = ' '.
            ENDIF.
          ELSE.
            IF WA_ZFIT0026-MONT_RBDO > MONT_PARCIAL.
              WA_MENSAGEM =  'Valor do Montante menor do que o valor do lançamento'.
              APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
              L_ERRO = 'X'.
            ENDIF.

            WA_ZFIT0026-VLR_MULTA_RBDO = ' '.
            WA_ZFIT0026-VLR_JUROS_RBDO = ' '.
          ENDIF.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              NR_RANGE_NR = '01'
              OBJECT      = 'ZID_LANC'
            IMPORTING
              NUMBER      = P_ZID.

          SELECT VBELN, SEQ
          FROM ZFIT0026
          INTO TABLE @DATA(IT_CONT_SEQ)
          WHERE VBELN EQ @WA_ZFIT0026-VBELN
          ORDER BY SEQ DESCENDING.

          READ TABLE IT_CONT_SEQ INTO DATA(WA_CONT_SEQ) INDEX 1.

          WA_ZFIT0026-ZID_LANC      = P_ZID.
          "wa_zfit0026-seq           = wa_cont_seq-seq.
          WA_ZFIT0026-SEQ           = WA_CONT_SEQ-SEQ + 1.
          "WA_ZFIT0026_GERA-SEQ      = wa_cont_seq-seq + 1.
          WA_ZFIT0026-UNAME         = 'JOBADM'.

          IF WA_ZFIT0026-DATA_PGTO IS NOT INITIAL.
            CALL FUNCTION 'Z_CONTROLE_FECHAMES'
              EXPORTING
                I_BUKRS  = WA_VBAK-BUKRS_VF
                I_DATA   = WA_ZFIT0026-DATA_PGTO
              IMPORTING
                E_STATUS = L_STATUS
                E_MESSA  = L_MESSAGE
              EXCEPTIONS
                ERROR    = 1
                OTHERS   = 2.

            IF L_STATUS = 'E'.
              DATA(L_MESS1) = L_MESSAGE(30).
              DATA(L_MESS2) = L_MESSAGE+30(34).
              CONCATENATE L_MESS1 L_MESS2 INTO WA_MENSAGEM.
              APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.

              L_ERRO = 'X'.
            ENDIF.
          ENDIF.

          IF ( WA_ZFIT0026-TAXA IS INITIAL ).
            WA_MENSAGEM =  'Não é possível fazer um lançamento sem a taxa.'.
            APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
            L_ERRO = 'X'.
          ELSE.
*            IF ( WA_ZFIT0026-MOEDA EQ 'BRL' ) AND ( WA_ZFIT0026-TAXA > TAXA ).
*              WA_MENSAGEM =  'Taxa incorreta para moeda.'.
*              APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
*              L_ERRO = 'X'.
*            ENDIF.
          ENDIF.
        ENDIF. "2

        IF L_ERRO IS INITIAL."3
          INSERT INTO ZFIT0026 VALUES WA_ZFIT0026.
          IF SY-SUBRC IS INITIAL.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.

          "****************************************************************************************
          "                                      GERAR CONTABIL                                   *
          "****************************************************************************************
          "me->Z_GERA_CONTABIL( ).
          TAXA = 1.

          IF WA_ZFIT0026-AJUSTE <> 'X'."4

            SELECT SINGLE * FROM ZSDT0159
            INTO WA_0159
            WHERE VBELN EQ  WA_ZFIT0026-VBELN.

            SELECT SINGLE * FROM ZSDT0053 INTO WA_ZSDT0053
            WHERE VBELN EQ WA_ZFIT0026-VBELN.

            DIA = WA_ZFIT0026-DATA_PGTO+6(2).
            MES = WA_ZFIT0026-DATA_PGTO+4(2).
            ANO = WA_ZFIT0026-DATA_PGTO(4).

            CONCATENATE DIA '.' MES '.' ANO INTO DATA.
            CONCATENATE WA_ZFIT0026-VBELN WA_ZFIT0026-SEQ SY-DATUM(4) INTO OBJ_KEY.

            SELECT SINGLE * FROM ZIB_CONTABIL INTO  WA_VERIFICA_ZIB WHERE OBJ_KEY EQ OBJ_KEY.

            IF ( SY-SUBRC EQ 0 )."5

              DATA: W TYPE I.
              W = 0.
              "me->Z_GERA_CONTABIL.
              SELECT VBELN SEQ OBJ_KEY
              FROM ZFIT0026
              INTO TABLE IT_ZFIT0026_GERA
              WHERE VBELN EQ WA_ZFIT0026-VBELN
                ORDER BY SEQ DESCENDING .

              IF ( SY-SUBRC EQ 0 ).
                READ TABLE IT_ZFIT0026_GERA INTO WA_ZFIT0026_GERA INDEX 1." TABIX.
                IF ( SY-SUBRC EQ 0 ).
                  CLEAR OBJ_KEY.
                  SEQ_NOVO = WA_ZFIT0026_GERA-SEQ + 1.
                  CONCATENATE WA_ZFIT0026_GERA-VBELN SEQ_NOVO SY-DATUM(4) INTO OBJ_KEY.
                  WHILE W EQ 0.

                    CLEAR: WA_VERIFICA_ZIB.
                    SELECT SINGLE * FROM ZIB_CONTABIL INTO WA_VERIFICA_ZIB WHERE OBJ_KEY EQ OBJ_KEY.
                    CLEAR: W_ZFIT0026.
                    SELECT SINGLE * FROM ZFIT0026 INTO W_ZFIT0026 WHERE SEQ EQ SEQ_NOVO AND VBELN  EQ WA_ZFIT0026-VBELN.
*          IF ( SY-SUBRC NE 0 ).
***       Verifica sequencia na ZIB e tambem na ZFIT0026 caso tenha retorna e adiciona outra seguencia.
                    IF WA_VERIFICA_ZIB IS INITIAL AND W_ZFIT0026 IS INITIAL.
                      W = 1.
                    ELSE.

                      CLEAR: OBJ_KEY.
                      SEQ_NOVO = SEQ_NOVO + 1.
                      CONCATENATE WA_ZFIT0026_GERA-VBELN SEQ_NOVO SY-DATUM(4) INTO OBJ_KEY.

                    ENDIF.
                  ENDWHILE.
                ENDIF.
              ENDIF.

            ELSE.
              IF WA_ZFIT0026-MOEDA EQ 'USD'.
                V_TAXA_CONTABIL =  WA_ZFIT0026-TAXA.

              ENDIF.
            ENDIF.

            ZIB_CONTABIL_WA-OBJ_KEY     = OBJ_KEY.
            ZIB_CONTABIL_WA-SEQITEM     = '0001'.

            "De => para centro virtual.
            SELECT SINGLE CENTRO_REAL
            FROM ZSDT_DEPARA_CEN
            INTO W_WERKS
            WHERE CENTROV_1 EQ WA_VBAP-WERKS.
            IF W_WERKS IS NOT INITIAL.
              ZIB_CONTABIL_WA-GSBER       = W_WERKS.
            ELSE.
              ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
            ENDIF.
            CLEAR: W_WERKS.

            ZIB_CONTABIL_WA-BUKRS       = WA_ZFIT0026-BUKRS.
            ZIB_CONTABIL_WA-INTERFACE   = '96'.
            ZIB_CONTABIL_WA-BKTXT       = 'Recbto Venda Insumos'.
            ZIB_CONTABIL_WA-BLDAT       = DATA.
            ZIB_CONTABIL_WA-BUDAT       = DATA.
            ZIB_CONTABIL_WA-GJAHR       = ANO.
            ZIB_CONTABIL_WA-MONAT       = MES.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WA_ZFIT0026-VBELN
              IMPORTING
                OUTPUT = ZIB_CONTABIL_WA-XBLNR.

            ZIB_CONTABIL_WA-BLART       = 'ND'.

            CONCATENATE 'Ordem de Venda Insumos' WA_ZFIT0026-VBELN INTO SGTXT SEPARATED BY SPACE.

            ZIB_CONTABIL_WA-BSCHL          = '09'.
            ZIB_CONTABIL_WA-HKONT          = WA_VBAK-KUNNR.
            ZIB_CONTABIL_WA-WAERS          = WA_ZFIT0026-MOEDA.
            ZIB_CONTABIL_WA-ZFBDT          = SPACE.
            ZIB_CONTABIL_WA-ZFBDT          = DATA.
            ZIB_CONTABIL_WA-ZLSPR          = ' '.
            ZIB_CONTABIL_WA-ZLSCH          = WA_ZFIT0026-FORMA_PAG.
            ZIB_CONTABIL_WA-KIDNO          = SPACE.
            ZIB_CONTABIL_WA-SGTXT          = SGTXT.
            ZIB_CONTABIL_WA-XREF1          = SPACE.
            ZIB_CONTABIL_WA-XREF2          = SPACE.
            ZIB_CONTABIL_WA-XREF3          = SPACE.

            "De => para centro virtual.
            SELECT SINGLE CENTRO_REAL
            FROM ZSDT_DEPARA_CEN
            INTO W_WERKS
            WHERE CENTROV_1 EQ WA_VBAP-WERKS.
            IF W_WERKS IS NOT INITIAL.
              ZIB_CONTABIL_WA-BUPLA       = W_WERKS.
            ELSE.
              ZIB_CONTABIL_WA-BUPLA       = WA_VBAP-WERKS.
            ENDIF.
            CLEAR: W_WERKS.

*          zib_contabil_wa-bupla          = wa_saida-vkbur. "Escritório de vendas "Comentado AOENNING 28/04/2020
            ZIB_CONTABIL_WA-ZTERM          = WA_ZFIT0026-ZTERM.
            IF WA_ZFIT0026-MOEDA EQ 'USD'.
              ZIB_CONTABIL_WA-KURRF          = V_TAXA_CONTABIL.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WA_ZFIT0026-VBELN
              IMPORTING
                OUTPUT = ZIB_CONTABIL_WA-ZUONR.
            ZIB_CONTABIL_WA-UMSKZ  = 'L'.
            ZIB_CONTABIL_WA-KOSTL          = SPACE.
            ZIB_CONTABIL_WA-AUFNR          = SPACE.
            ZIB_CONTABIL_WA-PRCTR          = SPACE.
            ZIB_CONTABIL_WA-WAERS_I        = 'BRL'.
            ZIB_CONTABIL_WA-WAERS_F        = WA_ZFIT0026-MOEDA.

            IF ( WA_ZFIT0026-MOEDA EQ 'USD' ).
              ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
              ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI    + ( ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ) * WA_ZFIT0026-TAXA ).
              ZIB_CONTABIL_WA-DMBE2          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
            ELSE.
              ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
              ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI    + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
              ZIB_CONTABIL_WA-DMBE2          = SPACE.
            ENDIF.

            ZIB_CONTABIL_WA-BVTYP          = SPACE.
            ZIB_CONTABIL_WA-HBKID          = SPACE.
            ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
            ZIB_CONTABIL_WA-BANKL          = SPACE.
            ZIB_CONTABIL_WA-BANKN          = SPACE.
            ZIB_CONTABIL_WA-NEWBW          = SPACE.
            ZIB_CONTABIL_WA-ANLN1          = SPACE.
            ZIB_CONTABIL_WA-ANLN2          = SPACE.

            INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
            CLEAR: ZIB_CONTABIL_WA.

            ZIB_CONTABIL_WA-OBJ_KEY     = OBJ_KEY.
            ZIB_CONTABIL_WA-SEQITEM     =  '0002'.

            "De => para centro virtual.
            SELECT SINGLE CENTRO_REAL
            FROM ZSDT_DEPARA_CEN
            INTO W_WERKS
            WHERE CENTROV_1 EQ WA_VBAP-WERKS.
            IF W_WERKS IS NOT INITIAL.
              ZIB_CONTABIL_WA-GSBER       = W_WERKS.
            ELSE.
              ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
            ENDIF.
            CLEAR: W_WERKS.

            ZIB_CONTABIL_WA-BUKRS       = WA_ZFIT0026-BUKRS.
            ZIB_CONTABIL_WA-INTERFACE   = '96'.
            ZIB_CONTABIL_WA-BKTXT       = 'Recbto Venda Insumos'.
            ZIB_CONTABIL_WA-BLDAT       = DATA.
            ZIB_CONTABIL_WA-BUDAT       = DATA.
            ZIB_CONTABIL_WA-GJAHR       = ANO.
            ZIB_CONTABIL_WA-MONAT       = MES.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WA_ZFIT0026-VBELN
              IMPORTING
                OUTPUT = ZIB_CONTABIL_WA-XBLNR.

            IF WA_ZFIT0026-MONT_MOEDA NE 0.
              ZIB_CONTABIL_WA-BSCHL          = '19'.
              ZIB_CONTABIL_WA-HKONT          = WA_VBAK-KUNNR.
              ZIB_CONTABIL_WA-BLART          = 'ND'.
              ZIB_CONTABIL_WA-WAERS          = WA_ZFIT0026-MOEDA.
              ZIB_CONTABIL_WA-ZFBDT          = DATA.
              ZIB_CONTABIL_WA-ZLSPR          = ' '.
              ZIB_CONTABIL_WA-ZLSCH          = WA_ZFIT0026-FORMA_PAG.
              ZIB_CONTABIL_WA-KIDNO          = SPACE.
              ZIB_CONTABIL_WA-SGTXT          = SGTXT.
              ZIB_CONTABIL_WA-XREF1          = SPACE.
              ZIB_CONTABIL_WA-XREF2          = SPACE.
              ZIB_CONTABIL_WA-XREF3          = SPACE.

              "De => para centro virtual.
              SELECT SINGLE CENTRO_REAL
              FROM ZSDT_DEPARA_CEN
              INTO W_WERKS
              WHERE CENTROV_1 EQ WA_VBAP-WERKS.
              IF W_WERKS IS NOT INITIAL.
                ZIB_CONTABIL_WA-BUPLA   = W_WERKS.
              ELSE.
                ZIB_CONTABIL_WA-BUPLA   = WA_VBAP-WERKS.
              ENDIF.
              CLEAR: W_WERKS.


              IF WA_ZFIT0026-MOEDA EQ 'USD'.
                ZIB_CONTABIL_WA-KURRF           = V_TAXA_CONTABIL.
              ENDIF.
*        ZIB_CONTABIL_WA-ZTERM          = WA_ZFIT0026-ZTERM.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  INPUT  = WA_ZFIT0026-VBELN
                IMPORTING
                  OUTPUT = ZIB_CONTABIL_WA-ZUONR.

              ZIB_CONTABIL_WA-UMSKZ          = 'L'.
              "ZIB_CONTABIL_WA-UMSKZ          = WA_ZFIT0026-RAZAO_ESPECIAL.
              ZIB_CONTABIL_WA-KOSTL          = SPACE.
              ZIB_CONTABIL_WA-AUFNR          = SPACE.
              ZIB_CONTABIL_WA-PRCTR          = SPACE.
              ZIB_CONTABIL_WA-WAERS_I        = 'BRL'.
              ZIB_CONTABIL_WA-WAERS_F        = WA_ZFIT0026-MOEDA.

              IF ( WA_ZFIT0026-MOEDA EQ 'USD' ).
                ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
                ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI.
                ZIB_CONTABIL_WA-DMBE2          = WA_ZFIT0026-MONT_MOEDA.
              ELSE.
                ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
                ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI.
                ZIB_CONTABIL_WA-DMBE2          = SPACE.
              ENDIF.

              ZIB_CONTABIL_WA-BVTYP          = SPACE.
              ZIB_CONTABIL_WA-HBKID          = SPACE.
              ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
              ZIB_CONTABIL_WA-BANKL          = SPACE.
              ZIB_CONTABIL_WA-BANKN          = SPACE.
              ZIB_CONTABIL_WA-NEWBW          = SPACE.
              ZIB_CONTABIL_WA-ANLN1          = SPACE.
              ZIB_CONTABIL_WA-ANLN2          = SPACE.
              INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
            ENDIF.

            CLEAR: ZIB_CONTABIL_WA, SGTXT.

            X_TOTAL = ( WA_ZFIT0026-VLR_JUROS_RBDO + WA_ZFIT0026-VLR_MULTA_RBDO ).
            IF X_TOTAL > 0."6
              CONCATENATE 'Receita de Juros sobre OV. Insumos' WA_ZFIT0026-VBELN INTO SGTXT SEPARATED BY SPACE.

              ZIB_CONTABIL_WA-OBJ_KEY     =  OBJ_KEY.
              ZIB_CONTABIL_WA-SEQITEM     = '0003'.

              "De => para centro virtual.
              SELECT SINGLE CENTRO_REAL
              FROM ZSDT_DEPARA_CEN
              INTO W_WERKS
              WHERE CENTROV_1 EQ WA_VBAP-WERKS.
              IF W_WERKS IS NOT INITIAL.
                ZIB_CONTABIL_WA-GSBER       = W_WERKS.
              ELSE.
                ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
              ENDIF.
              CLEAR: W_WERKS.

              ZIB_CONTABIL_WA-BUKRS       = WA_VBAK-BUKRS_VF.
              ZIB_CONTABIL_WA-INTERFACE   = '96'.
              ZIB_CONTABIL_WA-BKTXT = 'Recbto Juros/multa Insumos'.


              ZIB_CONTABIL_WA-BLDAT = DATA.
              ZIB_CONTABIL_WA-BUDAT = DATA.
              ZIB_CONTABIL_WA-GJAHR = ANO.
              ZIB_CONTABIL_WA-MONAT = MES.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT  = WA_ZFIT0026-VBELN
                IMPORTING
                  OUTPUT = ZIB_CONTABIL_WA-XBLNR.

              ZIB_CONTABIL_WA-BSCHL          = '50'.

              SELECT SINGLE * FROM KNA1
              INTO  WA_KNA1
              WHERE KUNNR EQ  WA_VBAK-KUNNR.

              ZIB_CONTABIL_WA-VBUND = WA_KNA1-VBUND.

              IF ZIB_CONTABIL_WA-VBUND EQ 'SOCIOS'.
                ZIB_CONTABIL_WA-HKONT          = '331004'.
              ELSE.
                ZIB_CONTABIL_WA-HKONT          = '331002'.
              ENDIF.
              ZIB_CONTABIL_WA-BLART       = 'ND'.

              ZIB_CONTABIL_WA-WAERS    = WA_ZFIT0026-MOEDA.
              ZIB_CONTABIL_WA-ZFBDT    = DATA.
              ZIB_CONTABIL_WA-ZLSPR    = ' '.
              ZIB_CONTABIL_WA-ZLSCH    = WA_ZFIT0026-FORMA_PAG.
              ZIB_CONTABIL_WA-KIDNO    = SPACE.
              ZIB_CONTABIL_WA-SGTXT    = SGTXT.
              ZIB_CONTABIL_WA-XREF1    = SPACE.
              ZIB_CONTABIL_WA-XREF2    = SPACE.
              ZIB_CONTABIL_WA-XREF3    = SPACE.

              "De => para centro virtual.
              SELECT SINGLE CENTRO_REAL
              FROM ZSDT_DEPARA_CEN
              INTO W_WERKS
              WHERE CENTROV_1 EQ WA_VBAP-WERKS.
              IF W_WERKS IS NOT INITIAL.
                ZIB_CONTABIL_WA-BUPLA    = W_WERKS.
              ELSE.
                ZIB_CONTABIL_WA-BUPLA    = WA_VBAP-WERKS.
              ENDIF.
              CLEAR: W_WERKS.


              IF WA_ZFIT0026-MOEDA EQ 'USD'.
                ZIB_CONTABIL_WA-KURRF     = V_TAXA_CONTABIL.
              ENDIF.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  INPUT  = WA_ZFIT0026-VBELN
                IMPORTING
                  OUTPUT = ZIB_CONTABIL_WA-ZUONR.

*               IF P_INS IS NOT INITIAL.
*                 ZIB_CONTABIL_WA-UMSKZ = 'L'.
*               ELSEIF P_MI IS NOT INITIAL.
*                 ZIB_CONTABIL_WA-UMSKZ = 'A'.
*               ENDIF.

              ZIB_CONTABIL_WA-KOSTL   = SPACE.
              ZIB_CONTABIL_WA-AUFNR   = SPACE.
              ZIB_CONTABIL_WA-PRCTR   = SPACE.
              ZIB_CONTABIL_WA-WAERS_I = 'BRL'.
              ZIB_CONTABIL_WA-WAERS_F = WA_ZFIT0026-MOEDA.

              IF WA_ZFIT0026-MOEDA EQ 'USD'.
                ZIB_CONTABIL_WA-WRBTR = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
                ZIB_CONTABIL_WA-DMBTR = ( ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ) * WA_ZFIT0026-TAXA ).
                ZIB_CONTABIL_WA-DMBE2 = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
              ELSE.
                ZIB_CONTABIL_WA-WRBTR  = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
                ZIB_CONTABIL_WA-DMBTR  = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
                ZIB_CONTABIL_WA-DMBE2  = SPACE.
              ENDIF.

              ZIB_CONTABIL_WA-BVTYP          = SPACE.
              ZIB_CONTABIL_WA-HBKID          = SPACE.
              ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
              ZIB_CONTABIL_WA-BANKL          = SPACE.
              ZIB_CONTABIL_WA-BANKN          = SPACE.
              ZIB_CONTABIL_WA-NEWBW          = SPACE.
              ZIB_CONTABIL_WA-ANLN1          = SPACE.
              ZIB_CONTABIL_WA-ANLN2          = SPACE.

              INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
              IF SY-SUBRC IS INITIAL.
                COMMIT WORK.
              ELSE.
                WA_MENSAGEM =  'Lançamento não foi gerado'.
                APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
                L_ERRO = 'X'.
                ROLLBACK WORK.
              ENDIF.
              CLEAR: ZIB_CONTABIL_WA.
            ENDIF."6
            UPDATE ZFIT0026
               SET  OBJ_KEY = OBJ_KEY
                    STATUS  = 'P'
               WHERE VBELN  EQ WA_ZFIT0026-VBELN
               AND SEQ    EQ WA_ZFIT0026-SEQ.

            IF SY-SUBRC IS INITIAL.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.

            WAIT UP TO 75 SECONDS.

            SELECT SINGLE *
            FROM ZIB_CONTABIL_CHV
            INTO W_ZIB_CONTABIL_CHV
            WHERE OBJ_KEY EQ OBJ_KEY
            AND NOT EXISTS ( SELECT *
                               FROM BKPF
                              WHERE BELNR EQ ZIB_CONTABIL_CHV~BELNR
                                AND BUKRS EQ ZIB_CONTABIL_CHV~BUKRS
                                AND GJAHR EQ ZIB_CONTABIL_CHV~GJAHR
                                AND STBLG NE SPACE ).

            IF SY-SUBRC IS INITIAL.
              V_doc_contabil = W_ZIB_CONTABIL_CHV-BELNR.
              UPDATE ZFIT0026
              SET DOCNUM  = V_doc_contabil
              STATUS      = 'S'
              WHERE VBELN  = WA_ZFIT0026-VBELN
              AND SEQ      = WA_ZFIT0026-SEQ.
              IF SY-SUBRC IS INITIAL.
                COMMIT WORK.
                I = I + 1.
                LWA_DATA_RESPONSE_DOC-DOC_CONTABIL = V_doc_contabil.
              ELSE.
                ROLLBACK WORK.
              ENDIF.
              V_PROCESSOU = 'X'.
            ELSE.
              SELECT  *
              FROM ZIB_CONTABIL_ERR
              INTO TABLE IT_ZIB_CONTABIL_ERR3
              WHERE OBJ_KEY EQ OBJ_KEY.

              IF SY-SUBRC IS INITIAL.

                LOOP AT IT_ZIB_CONTABIL_ERR3 INTO WA_ERRO3.
                  APPEND WA_ERRO3-MESSAGE TO <FS_ERRO>-ERROS.
                ENDLOOP.
                L_ERRO = 'X'.

                UPDATE ZFIT0026
                SET STATUS  = 'E'
                WHERE VBELN    = WA_ZFIT0026-VBELN
                      AND SEQ  = WA_ZFIT0026-SEQ.

                IF SY-SUBRC IS INITIAL.
                  COMMIT WORK.
                ELSE.
                  ROLLBACK WORK.
                ENDIF.
                V_PROCESSOU = 'X'.
              ENDIF.

            ENDIF.
          ENDIF."4
        ENDIF. "3

      ELSEIF WA_ZFIT0026-STATUS EQ 'E' OR WA_ZFIT0026-STATUS EQ 'P'.

        IF L_ERRO IS INITIAL. "3.1

          READ TABLE IT_VBAP INTO WA_VBAP WITH KEY  VBELn = WA_ZFIT0026-VBELN.
          READ TABLE IT_VBAk INTO WA_VBAk WITH KEY  VBELn = WA_ZFIT0026-VBELN.

          "****************************************************************************************
          "                                      GERAR CONTABIL                                   *
          "****************************************************************************************

          " ME->Z_GERA_DOCUMENTO_CONTABIL( EXPORTING  WA_ZFIT0026 = WA_ZFIT0026  IMPORTING E_STATUS_CODE  =  DATA(_STATUS_CODE)  RECEIVING R_MSG_ERRO = LVA_MSG_ERRO  ).
          TAXA = 1.

          IF WA_ZFIT0026-AJUSTE <> 'X'."4

            SELECT SINGLE * FROM ZSDT0159
            INTO WA_0159
            WHERE VBELN EQ  WA_ZFIT0026-VBELN.

            SELECT SINGLE * FROM ZSDT0053 INTO WA_ZSDT0053
            WHERE VBELN EQ WA_ZFIT0026-VBELN.

            DIA = WA_ZFIT0026-DATA_PGTO+6(2).
            MES = WA_ZFIT0026-DATA_PGTO+4(2).
            ANO = WA_ZFIT0026-DATA_PGTO(4).

            CONCATENATE DIA '.' MES '.' ANO INTO DATA.
            CONCATENATE WA_ZFIT0026-VBELN WA_ZFIT0026-SEQ SY-DATUM(4) INTO OBJ_KEY.

            SELECT SINGLE * FROM ZIB_CONTABIL INTO  WA_VERIFICA_ZIB WHERE OBJ_KEY EQ OBJ_KEY.

            IF ( SY-SUBRC EQ 0 )."5

              DATA: Z TYPE I.
              Z = 0.
              "me->Z_GERA_CONTABIL.
              SELECT VBELN SEQ OBJ_KEY
              FROM ZFIT0026
              INTO TABLE IT_ZFIT0026_GERA
              WHERE VBELN EQ WA_ZFIT0026-VBELN.

              IF ( SY-SUBRC EQ 0 ).
                READ TABLE IT_ZFIT0026_GERA INTO WA_ZFIT0026_GERA INDEX 1."TABIX.
                IF ( SY-SUBRC EQ 0 ).
                  CLEAR OBJ_KEY.
                  SEQ_NOVO = WA_ZFIT0026_GERA-SEQ + 1.
                  CONCATENATE WA_ZFIT0026_GERA-VBELN SEQ_NOVO SY-DATUM(4) INTO OBJ_KEY.
                  WHILE Z EQ 0.

                    CLEAR: WA_VERIFICA_ZIB.
                    SELECT SINGLE * FROM ZIB_CONTABIL INTO WA_VERIFICA_ZIB WHERE OBJ_KEY EQ OBJ_KEY.
                    CLEAR: W_ZFIT0026.
                    SELECT SINGLE * FROM ZFIT0026 INTO W_ZFIT0026 WHERE SEQ EQ SEQ_NOVO AND VBELN  EQ WA_ZFIT0026-VBELN.
*          IF ( SY-SUBRC NE 0 ).
***       Verifica sequencia na ZIB e tambem na ZFIT0026 caso tenha retorna e adiciona outra seguencia.
                    IF WA_VERIFICA_ZIB IS INITIAL AND W_ZFIT0026 IS INITIAL.
                      Z = 1.
                    ELSE.

                      CLEAR: OBJ_KEY.
                      SEQ_NOVO = SEQ_NOVO + 1.
                      CONCATENATE WA_ZFIT0026_GERA-VBELN SEQ_NOVO SY-DATUM(4) INTO OBJ_KEY.

                    ENDIF.
                  ENDWHILE.
                ENDIF.
              ENDIF.

              "ELSE.
            ENDIF.

            ZIB_CONTABIL_WA-OBJ_KEY     = OBJ_KEY.
            ZIB_CONTABIL_WA-SEQITEM     = '0001'.

            "De => para centro virtual.
            SELECT SINGLE CENTRO_REAL
            FROM ZSDT_DEPARA_CEN
            INTO W_WERKS
            WHERE CENTROV_1 EQ WA_VBAP-WERKS.
            IF W_WERKS IS NOT INITIAL.
              ZIB_CONTABIL_WA-GSBER       = W_WERKS.
            ELSE.
              ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
            ENDIF.
            CLEAR: W_WERKS.

            ZIB_CONTABIL_WA-BUKRS       = WA_ZFIT0026-BUKRS.
            ZIB_CONTABIL_WA-INTERFACE   = '96'.
            ZIB_CONTABIL_WA-BKTXT       = 'Recbto Venda Insumos'.
            ZIB_CONTABIL_WA-BLDAT       = DATA.
            ZIB_CONTABIL_WA-BUDAT       = DATA.
            ZIB_CONTABIL_WA-GJAHR       = ANO.
            ZIB_CONTABIL_WA-MONAT       = MES.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WA_ZFIT0026-VBELN
              IMPORTING
                OUTPUT = ZIB_CONTABIL_WA-XBLNR.

            ZIB_CONTABIL_WA-BLART       = 'ND'.

            CONCATENATE 'Ordem de Venda Insumos' WA_ZFIT0026-VBELN INTO SGTXT SEPARATED BY SPACE.

            ZIB_CONTABIL_WA-BSCHL          = '09'.
            ZIB_CONTABIL_WA-HKONT          = WA_VBAK-KUNNR.
            ZIB_CONTABIL_WA-WAERS          = WA_ZFIT0026-MOEDA.
            ZIB_CONTABIL_WA-ZFBDT          = SPACE.
            ZIB_CONTABIL_WA-ZFBDT          = DATA.
            ZIB_CONTABIL_WA-ZLSPR          = ' '.
            ZIB_CONTABIL_WA-ZLSCH          = WA_ZFIT0026-FORMA_PAG.
            ZIB_CONTABIL_WA-KIDNO          = SPACE.
            ZIB_CONTABIL_WA-SGTXT          = SGTXT.
            ZIB_CONTABIL_WA-XREF1          = SPACE.
            ZIB_CONTABIL_WA-XREF2          = SPACE.
            ZIB_CONTABIL_WA-XREF3          = SPACE.

            "De => para centro virtual.
            SELECT SINGLE CENTRO_REAL
            FROM ZSDT_DEPARA_CEN
            INTO W_WERKS
            WHERE CENTROV_1 EQ WA_VBAP-WERKS.
            IF W_WERKS IS NOT INITIAL.
              ZIB_CONTABIL_WA-BUPLA       = W_WERKS.
            ELSE.
              ZIB_CONTABIL_WA-BUPLA       = WA_VBAP-WERKS.
            ENDIF.
            CLEAR: W_WERKS.

*          zib_contabil_wa-bupla          = wa_saida-vkbur. "Escritório de vendas "Comentado AOENNING 28/04/2020
            ZIB_CONTABIL_WA-ZTERM          = WA_ZFIT0026-ZTERM.
            IF WA_ZFIT0026-MOEDA EQ 'USD'.
              ZIB_CONTABIL_WA-KURRF          = V_TAXA_CONTABIL.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WA_ZFIT0026-VBELN
              IMPORTING
                OUTPUT = ZIB_CONTABIL_WA-ZUONR.
            ZIB_CONTABIL_WA-UMSKZ  = 'L'.
            ZIB_CONTABIL_WA-KOSTL          = SPACE.
            ZIB_CONTABIL_WA-AUFNR          = SPACE.
            ZIB_CONTABIL_WA-PRCTR          = SPACE.
            ZIB_CONTABIL_WA-WAERS_I        = 'BRL'.
            ZIB_CONTABIL_WA-WAERS_F        = WA_ZFIT0026-MOEDA.

            IF ( WA_ZFIT0026-MOEDA EQ 'USD' ).
              ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
              ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI    + ( ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ) * WA_ZFIT0026-TAXA ).
              ZIB_CONTABIL_WA-DMBE2          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
            ELSE.
              ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
              ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI    + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
              ZIB_CONTABIL_WA-DMBE2          = SPACE.
            ENDIF.

            ZIB_CONTABIL_WA-BVTYP          = SPACE.
            ZIB_CONTABIL_WA-HBKID          = SPACE.
            ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
            ZIB_CONTABIL_WA-BANKL          = SPACE.
            ZIB_CONTABIL_WA-BANKN          = SPACE.
            ZIB_CONTABIL_WA-NEWBW          = SPACE.
            ZIB_CONTABIL_WA-ANLN1          = SPACE.
            ZIB_CONTABIL_WA-ANLN2          = SPACE.

            INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
            CLEAR: ZIB_CONTABIL_WA.

            ZIB_CONTABIL_WA-OBJ_KEY     = OBJ_KEY.
            ZIB_CONTABIL_WA-SEQITEM     =  '0002'.

            "De => para centro virtual.
            SELECT SINGLE CENTRO_REAL
            FROM ZSDT_DEPARA_CEN
            INTO W_WERKS
            WHERE CENTROV_1 EQ WA_VBAP-WERKS.
            IF W_WERKS IS NOT INITIAL.
              ZIB_CONTABIL_WA-GSBER       = W_WERKS.
            ELSE.
              ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
            ENDIF.
            CLEAR: W_WERKS.

            ZIB_CONTABIL_WA-BUKRS       = WA_ZFIT0026-BUKRS.
            ZIB_CONTABIL_WA-INTERFACE   = '96'.
            ZIB_CONTABIL_WA-BKTXT       = 'Recbto Venda Insumos'.
            ZIB_CONTABIL_WA-BLDAT       = DATA.
            ZIB_CONTABIL_WA-BUDAT       = DATA.
            ZIB_CONTABIL_WA-GJAHR       = ANO.
            ZIB_CONTABIL_WA-MONAT       = MES.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = WA_ZFIT0026-VBELN
              IMPORTING
                OUTPUT = ZIB_CONTABIL_WA-XBLNR.

            IF WA_ZFIT0026-MONT_MOEDA NE 0.
              ZIB_CONTABIL_WA-BSCHL          = '19'.
              ZIB_CONTABIL_WA-HKONT          = WA_VBAK-KUNNR.
              ZIB_CONTABIL_WA-BLART          = 'ND'.
              ZIB_CONTABIL_WA-WAERS          = WA_ZFIT0026-MOEDA.
              ZIB_CONTABIL_WA-ZFBDT          = DATA.
              ZIB_CONTABIL_WA-ZLSPR          = ' '.
              ZIB_CONTABIL_WA-ZLSCH          = WA_ZFIT0026-FORMA_PAG.
              ZIB_CONTABIL_WA-KIDNO          = SPACE.
              ZIB_CONTABIL_WA-SGTXT          = SGTXT.
              ZIB_CONTABIL_WA-XREF1          = SPACE.
              ZIB_CONTABIL_WA-XREF2          = SPACE.
              ZIB_CONTABIL_WA-XREF3          = SPACE.

              "De => para centro virtual.
              SELECT SINGLE CENTRO_REAL
              FROM ZSDT_DEPARA_CEN
              INTO W_WERKS
              WHERE CENTROV_1 EQ WA_VBAP-WERKS.
              IF W_WERKS IS NOT INITIAL.
                ZIB_CONTABIL_WA-BUPLA   = W_WERKS.
              ELSE.
                ZIB_CONTABIL_WA-BUPLA   = WA_VBAP-WERKS.
              ENDIF.
              CLEAR: W_WERKS.


              IF WA_ZFIT0026-MOEDA EQ 'USD'.
                ZIB_CONTABIL_WA-KURRF          = V_TAXA_CONTABIL.
              ENDIF.
*        ZIB_CONTABIL_WA-ZTERM          = WA_ZFIT0026-ZTERM.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  INPUT  = WA_ZFIT0026-VBELN
                IMPORTING
                  OUTPUT = ZIB_CONTABIL_WA-ZUONR.

              ZIB_CONTABIL_WA-UMSKZ          = 'L'.
              "ZIB_CONTABIL_WA-UMSKZ          = WA_ZFIT0026-RAZAO_ESPECIAL.
              ZIB_CONTABIL_WA-KOSTL          = SPACE.
              ZIB_CONTABIL_WA-AUFNR          = SPACE.
              ZIB_CONTABIL_WA-PRCTR          = SPACE.
              ZIB_CONTABIL_WA-WAERS_I        = 'BRL'.
              ZIB_CONTABIL_WA-WAERS_F        = WA_ZFIT0026-MOEDA.

              IF ( WA_ZFIT0026-MOEDA EQ 'USD' ).
                ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
                ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI.
                ZIB_CONTABIL_WA-DMBE2          = WA_ZFIT0026-MONT_MOEDA.
              ELSE.
                ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
                ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI.
                ZIB_CONTABIL_WA-DMBE2          = SPACE.
              ENDIF.

              ZIB_CONTABIL_WA-BVTYP          = SPACE.
              ZIB_CONTABIL_WA-HBKID          = SPACE.
              ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
              ZIB_CONTABIL_WA-BANKL          = SPACE.
              ZIB_CONTABIL_WA-BANKN          = SPACE.
              ZIB_CONTABIL_WA-NEWBW          = SPACE.
              ZIB_CONTABIL_WA-ANLN1          = SPACE.
              ZIB_CONTABIL_WA-ANLN2          = SPACE.
              INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
            ENDIF.

            CLEAR: ZIB_CONTABIL_WA, SGTXT.

            X_TOTAL = ( WA_ZFIT0026-VLR_JUROS_RBDO + WA_ZFIT0026-VLR_MULTA_RBDO ).
            IF X_TOTAL > 0."6
              CONCATENATE 'Receita de Juros sobre OV. Insumos' WA_ZFIT0026-VBELN INTO SGTXT SEPARATED BY SPACE.

              ZIB_CONTABIL_WA-OBJ_KEY     =  OBJ_KEY.
              ZIB_CONTABIL_WA-SEQITEM     = '0003'.

              "De => para centro virtual.
              SELECT SINGLE CENTRO_REAL
              FROM ZSDT_DEPARA_CEN
              INTO W_WERKS
              WHERE CENTROV_1 EQ WA_VBAP-WERKS.
              IF W_WERKS IS NOT INITIAL.
                ZIB_CONTABIL_WA-GSBER       = W_WERKS.
              ELSE.
                ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
              ENDIF.
              CLEAR: W_WERKS.

              ZIB_CONTABIL_WA-BUKRS       = WA_VBAK-BUKRS_VF.
              ZIB_CONTABIL_WA-INTERFACE   = '96'.
              ZIB_CONTABIL_WA-BKTXT = 'Recbto Juros/multa Insumos'.


              ZIB_CONTABIL_WA-BLDAT = DATA.
              ZIB_CONTABIL_WA-BUDAT = DATA.
              ZIB_CONTABIL_WA-GJAHR = ANO.
              ZIB_CONTABIL_WA-MONAT = MES.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  INPUT  = WA_ZFIT0026-VBELN
                IMPORTING
                  OUTPUT = ZIB_CONTABIL_WA-XBLNR.

              ZIB_CONTABIL_WA-BSCHL          = '50'.

              SELECT SINGLE * FROM KNA1
              INTO  WA_KNA1
              WHERE KUNNR EQ  WA_VBAK-KUNNR.

              ZIB_CONTABIL_WA-VBUND = WA_KNA1-VBUND.

              IF ZIB_CONTABIL_WA-VBUND EQ 'SOCIOS'.
                ZIB_CONTABIL_WA-HKONT          = '331004'.
              ELSE.
                ZIB_CONTABIL_WA-HKONT          = '331002'.
              ENDIF.
              ZIB_CONTABIL_WA-BLART       = 'ND'.

              ZIB_CONTABIL_WA-WAERS    = WA_ZFIT0026-MOEDA.
              ZIB_CONTABIL_WA-ZFBDT    = DATA.
              ZIB_CONTABIL_WA-ZLSPR    = ' '.
              ZIB_CONTABIL_WA-ZLSCH    = WA_ZFIT0026-FORMA_PAG.
              ZIB_CONTABIL_WA-KIDNO    = SPACE.
              ZIB_CONTABIL_WA-SGTXT    = SGTXT.
              ZIB_CONTABIL_WA-XREF1    = SPACE.
              ZIB_CONTABIL_WA-XREF2    = SPACE.
              ZIB_CONTABIL_WA-XREF3    = SPACE.

              "De => para centro virtual.
              SELECT SINGLE CENTRO_REAL
              FROM ZSDT_DEPARA_CEN
              INTO W_WERKS
              WHERE CENTROV_1 EQ WA_VBAP-WERKS.
              IF W_WERKS IS NOT INITIAL.
                ZIB_CONTABIL_WA-BUPLA    = W_WERKS.
              ELSE.
                ZIB_CONTABIL_WA-BUPLA    = WA_VBAP-WERKS.
              ENDIF.
              CLEAR: W_WERKS.


              IF WA_ZFIT0026-MOEDA EQ 'USD'.
                ZIB_CONTABIL_WA-KURRF     = V_TAXA_CONTABIL.
              ENDIF.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  INPUT  = WA_ZFIT0026-VBELN
                IMPORTING
                  OUTPUT = ZIB_CONTABIL_WA-ZUONR.

*               IF P_INS IS NOT INITIAL.
*                 ZIB_CONTABIL_WA-UMSKZ = 'L'.
*               ELSEIF P_MI IS NOT INITIAL.
*                 ZIB_CONTABIL_WA-UMSKZ = 'A'.
*               ENDIF.

              ZIB_CONTABIL_WA-KOSTL   = SPACE.
              ZIB_CONTABIL_WA-AUFNR   = SPACE.
              ZIB_CONTABIL_WA-PRCTR   = SPACE.
              ZIB_CONTABIL_WA-WAERS_I = 'BRL'.
              ZIB_CONTABIL_WA-WAERS_F = WA_ZFIT0026-MOEDA.

              IF WA_ZFIT0026-MOEDA EQ 'USD'.
                ZIB_CONTABIL_WA-WRBTR = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
                ZIB_CONTABIL_WA-DMBTR = ( ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ) * WA_ZFIT0026-TAXA ).
                ZIB_CONTABIL_WA-DMBE2 = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
              ELSE.
                ZIB_CONTABIL_WA-WRBTR  = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
                ZIB_CONTABIL_WA-DMBTR  = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
                ZIB_CONTABIL_WA-DMBE2  = SPACE.
              ENDIF.

              ZIB_CONTABIL_WA-BVTYP          = SPACE.
              ZIB_CONTABIL_WA-HBKID          = SPACE.
              ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
              ZIB_CONTABIL_WA-BANKL          = SPACE.
              ZIB_CONTABIL_WA-BANKN          = SPACE.
              ZIB_CONTABIL_WA-NEWBW          = SPACE.
              ZIB_CONTABIL_WA-ANLN1          = SPACE.
              ZIB_CONTABIL_WA-ANLN2          = SPACE.

              INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
              IF SY-SUBRC IS INITIAL.
                COMMIT WORK.
              ELSE.
                WA_MENSAGEM =  'Lançamento não foi gerado'.
                APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
                L_ERRO = 'X'.
                ROLLBACK WORK.
              ENDIF.

              CLEAR: ZIB_CONTABIL_WA.
            ENDIF."6
            UPDATE ZFIT0026
             SET  OBJ_KEY = OBJ_KEY
             STATUS  = 'P'
            WHERE VBELN  EQ WA_ZFIT0026-VBELN
           AND SEQ    EQ WA_ZFIT0026-SEQ.

            IF SY-SUBRC IS INITIAL.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.

            WAIT UP TO 75 SECONDS.

            SELECT SINGLE *
            FROM ZIB_CONTABIL_CHV
            INTO W_ZIB_CONTABIL_CHV
            WHERE OBJ_KEY EQ OBJ_KEY
            AND NOT EXISTS ( SELECT *
                               FROM BKPF
                              WHERE BELNR EQ ZIB_CONTABIL_CHV~BELNR
                                AND BUKRS EQ ZIB_CONTABIL_CHV~BUKRS
                                AND GJAHR EQ ZIB_CONTABIL_CHV~GJAHR
                                AND STBLG NE SPACE ).

            IF SY-SUBRC IS INITIAL.

              UPDATE ZFIT0026
              SET DOCNUM  = W_ZIB_CONTABIL_CHV-BELNR
              STATUS      = 'S'
              WHERE VBELN  = WA_ZFIT0026-VBELN
              AND SEQ      = WA_ZFIT0026-SEQ.

              IF SY-SUBRC IS INITIAL.
                COMMIT WORK.
                LWA_DATA_RESPONSE_DOC-DOC_CONTABIL = W_ZIB_CONTABIL_CHV-BELNR.
                V_PROCESSOU = 'X'.
              ELSE.
                ROLLBACK WORK.
              ENDIF.

            ELSE.
              SELECT  *
              FROM ZIB_CONTABIL_ERR
              INTO TABLE IT_ZIB_CONTABIL_ERR3
              WHERE OBJ_KEY EQ OBJ_KEY.

              IF SY-SUBRC IS INITIAL.
                LOOP AT IT_ZIB_CONTABIL_ERR3 INTO WA_ERRO3.
                  APPEND WA_ERRO3-MESSAGE TO <FS_ERRO>-ERROS.
                ENDLOOP.

                L_ERRO = 'X'.

                UPDATE ZFIT0026
                SET STATUS  = 'E'
                WHERE VBELN    = WA_ZFIT0026-VBELN
                      AND SEQ  = WA_ZFIT0026-SEQ.

                IF SY-SUBRC IS INITIAL.
                  COMMIT WORK.
                ELSE.
                  ROLLBACK WORK.
                ENDIF.
                V_PROCESSOU = 'X'.
              ENDIF.
            ENDIF.
          ENDIF."4
        ENDIF."3.1

        IF V_PROCESSOU NE 'X'.
          WA_MENSAGEM =  'Tempo de processamento esgotado. Tente novamente em 2 minutos.'.
          APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
          L_ERRO = 'X'.
        ENDIF.
      ENDIF. "1.1
    ENDIF."1

    IF L_ERRO IS NOT INITIAL.
      E_SUCESSO   = ABAP_FALSE.
      E_MSG_OUTBOUND = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DATA_RESPONSE_ERRO ).
    ELSE.
      E_SUCESSO   = ABAP_TRUE.
      E_MSG_OUTBOUND = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DATA_RESPONSE_DOC ).
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method Z_GERA_CONTABIL.
     r_if_integracao_inject = me.
    e_sucesso = abap_true.
    DATA: lwa_data_request       LIKE zde_data_request,
          lwa_data_response_erro LIKE zde_data_response,
          lwa_data_response_doc  LIKE zde_data_response_doc.
    DATA: e_json    TYPE string,
          l_erro(1).

     TYPES: BEGIN OF ty_zfit0026_gera,
           vbeln   TYPE zfit0026-vbeln,
           seq     TYPE zfit0026-seq,
           obj_key TYPE zfit0026-obj_key,
         END OF ty_zfit0026_gera,

         BEGIN OF ty_zib_contabil_cont,
           obj_key   TYPE zib_contabil-obj_key,
           seqitem   TYPE zib_contabil-seqitem,
           bukrs     TYPE zib_contabil-bukrs,
           interface TYPE zib_contabil-interface,
           xblnr     TYPE zib_contabil-xblnr,
         END OF ty_zib_contabil_cont.

  DATA: it_zfit0026_gera    TYPE TABLE OF ty_zfit0026_gera,
        wa_zfit0026_gera    TYPE ty_zfit0026_gera,
        tabix               TYPE sy-tabix,
        seq_novo            TYPE zfit0026-seq,
        wa_verifica_zib     TYPE zib_contabil,
        wa_zib_contabil_aux TYPE zib_contabil,
        it_zib_cont         TYPE TABLE OF ty_zib_contabil_cont,
        wa_zib_cont         TYPE ty_zib_contabil_cont,
        vbeln_aux_p         TYPE zib_contabil-xblnr,
        wa_kna1             TYPE kna1.


  DATA: data(10)   TYPE c,
        dia(2)     TYPE c,
        mes(2)     TYPE c,
        ano(4)     TYPE c,
        sgtxt      TYPE sgtxt,
        w_zfit0026 TYPE zfit0026,
        qtd        TYPE sy-tabix.



    DATA: "zib_contabil_wa type zib_contabil,
      cont        TYPE numc10,
      obj_key     TYPE awkey,
      wa_0159     TYPE zsdt0159.

    DATA: data_sum TYPE datum,
          taxa     TYPE zfit0026-taxa.


    DATA: vg_kna1       TYPE kna1.
        TYPES:
      BEGIN OF ty_erro,
        mensagem TYPE char100,
      END OF ty_erro.

          DATA: it_mensagem TYPE TABLE OF  ty_erro,
          wa_mensagem TYPE  ty_erro.


    DATA: it_zsdt0040 TYPE TABLE OF zsdt0040,
          wa_zsdt0040 TYPE zsdt0040,
          wa_zsdt0041 TYPE zsdt0041,
          wa_zsdt0090 TYPE zsdt0090.

      DATA: vbeln_aux     TYPE vbak-vbeln,
          l_status(1)   TYPE c,
          l_message(64) TYPE c.
    DATA: wa_vbak_valor TYPE vbak.
    DATA: p_erro(1) TYPE c.
 DATA: razao_especial TYPE c.

    DATA: it_zib_contabil_chv TYPE STANDARD TABLE OF zib_contabil_chv,
          wa_zib_contabil_chv TYPE zib_contabil_chv,
          it_z0159            TYPE STANDARD TABLE OF zsdt0159,
          wa_z0159            TYPE zsdt0159.

    DATA: vl_gdatu TYPE gdatu_inv,
          v_kurrf  TYPE kurrf.


    DATA: it_vbap     TYPE TABLE OF vbap,
          it_vbkd     TYPE TABLE OF vbkd,
          it_t052     TYPE TABLE OF t052,
          wa_t052     TYPE t052,
          it_zsdt0090 TYPE STANDARD TABLE OF zsdt0090,
          it_zsdt0041 TYPE TABLE OF zsdt0041,
          it_kna1     TYPE TABLE OF kna1,
          it_zfit0026 TYPE TABLE OF zfit0026.
    "ja tendo o numero da OV
    DATA: mont_ov      TYPE netwr,
          mont_parcial TYPE netwr,
          mont_usado   TYPE netwr,
          datum        LIKE sy-datum.


    DATA:
          "tabix               TYPE sy-tabix,
          "seq_novo            TYPE zfit0026-seq,
          "wa_verifica_zib     TYPE zib_contabil,
          "wa_zib_contabil_aux TYPE zib_contabil,
          "it_zib_cont         TYPE TABLE OF ty_zib_contabil_cont,
          "wa_zib_cont TYPE zib_contabil,
          "vbeln_aux_p         TYPE zib_contabil-xblnr,
          "wa_kna1             TYPE kna1,
          zib_contabil_wa     TYPE zib_contabil.


    DATA: "w_zfit0026 TYPE zfit0026,
          "qtd        TYPE sy-tabix,
          i          TYPE i,
          v_taxa1    TYPE ukurs_curr.

    DATA: lva_msg_erro  TYPE string.

"****************************************************************************************
"   GERAR CONTABIL
"****************************************************************************************
"****************************************************************************************
APPEND INITIAL LINE TO lwa_data_response_erro-erros ASSIGNING FIELD-SYMBOL(<fs_erro>).
           taxa = 1.

          IF wa_zfit0026-ajuste <> 'X'.
SELECT * FROM vbak INTO TABLE @DATA(it_vbak)
        WHERE vbeln = @wa_zfit0026-vbeln.

      CHECK NOT it_vbak[] IS INITIAL.


      "vai na vbap para pegar os itens da OV
*SELECT * FROM vbap INTO TABLE @DATA(it_vbap)
*  WHERE vbeln = '0012126814'.

      SELECT *
       FROM vbkd
       INTO TABLE it_vbkd
      FOR ALL ENTRIES IN it_vbak
       WHERE vbeln EQ it_vbak-vbeln.

      SELECT *
      FROM t052 INTO TABLE it_t052
      FOR ALL ENTRIES IN  it_vbkd
      WHERE zterm EQ it_vbkd-zterm.


      " Dados de Taxa Travada
      CLEAR: it_zsdt0090.
      SELECT *
        FROM zsdt0090
        INTO TABLE it_zsdt0090
        FOR ALL ENTRIES IN it_vbak
        WHERE vbelv EQ it_vbak-vbeln
        AND estorno   NE 'X' "NAO~TRAZER ESTORNO
        AND categoria EQ 'C'.

      " Mestre de clientes (parte geral)
      SELECT *
        FROM kna1
        INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_vbak
        WHERE kunnr EQ it_vbak-kunnr.

      SELECT *
       FROM vbap
        INTO TABLE it_vbap
      FOR ALL ENTRIES IN it_vbak
        WHERE vbeln EQ it_vbak-vbeln
           AND NOT EXISTS ( SELECT *
                             FROM vbep
                            WHERE vbeln EQ vbap~vbeln
                              AND posnr EQ vbap~posnr
                              AND lifsp EQ '12' ).

              SELECT SINGLE * FROM zsdt0159
                INTO wa_0159
                WHERE vbeln EQ  wa_zfit0026-vbeln.

              SELECT SINGLE * FROM zsdt0053 INTO @DATA(wa_zsdt0053)
               WHERE vbeln EQ @wa_zfit0026-vbeln.

                 READ TABLE it_vbak INTO DATA(wa_vbak) WITH KEY vbeln = wa_zfit0026-vbeln.
                 READ TABLE it_vbap INTO DATA(wa_vbap) WITH KEY vbeln = wa_zfit0026-vbeln.

              data_sum = sy-datum + 3.
              IF ( wa_zfit0026-taxa IS INITIAL ).
                wa_mensagem =  'Não é possível fazer um lançamento sem a taxa.'.
                APPEND wa_mensagem TO <fs_erro>-erros.

                l_erro = 'X'.

              ELSE.


                  dia = wa_zfit0026-data_pgto+6(2).
                  mes = wa_zfit0026-data_pgto+4(2).
                  ano = wa_zfit0026-data_pgto(4).

                  CONCATENATE dia '.' mes '.' ano INTO data.
                  CONCATENATE wa_zfit0026-vbeln wa_zfit0026-seq sy-datum(4) INTO obj_key.

                  SELECT SINGLE * FROM zib_contabil INTO wa_verifica_zib WHERE obj_key EQ obj_key.

                  IF ( sy-subrc EQ 0 ).

                    SELECT vbeln seq obj_key
                      FROM zfit0026
                      INTO TABLE it_zfit0026_gera
                    WHERE vbeln EQ wa_zfit0026-vbeln.

                    IF ( sy-subrc EQ 0 ).

                      SELECT SINGLE * FROM zsdt0053 INTO @DATA(wa_zsdt00531)
                      WHERE vbeln EQ @wa_zfit0026-vbeln.

                      DESCRIBE TABLE it_zfit0026_gera LINES tabix.

                      READ TABLE it_zfit0026_gera INTO wa_zfit0026_gera INDEX tabix.

                      IF ( sy-subrc EQ 0 ).

                        seq_novo = wa_zfit0026_gera-seq + 1.

                        dia = wa_zfit0026-data_pgto+6(2).
                        mes = wa_zfit0026-data_pgto+4(2).
                        ano = wa_zfit0026-data_pgto(4).

                        CONCATENATE dia '.' mes '.' ano INTO data.
                        CONCATENATE wa_zfit0026_gera-vbeln seq_novo sy-datum(4) INTO obj_key.

                        SELECT SINGLE * FROM zib_contabil INTO wa_verifica_zib WHERE obj_key EQ obj_key.

                        IF ( sy-subrc EQ 0 ).
                          CLEAR: wa_verifica_zib.

                          DATA: w TYPE i.

                          w = 0.
                          seq_novo = seq_novo + 1.

                          CONCATENATE wa_zfit0026_gera-vbeln seq_novo sy-datum(4) INTO obj_key.

                          WHILE w EQ 0.

                            CLEAR: wa_verifica_zib.
                            SELECT SINGLE * FROM zib_contabil INTO wa_verifica_zib WHERE obj_key EQ obj_key.
                            CLEAR: w_zfit0026.
                            SELECT SINGLE * FROM zfit0026 INTO w_zfit0026 WHERE seq EQ seq_novo AND vbeln  EQ wa_zfit0026-vbeln.


*          IF ( SY-SUBRC NE 0 ).
***       Verifica sequencia na ZIB e tambem na ZFIT0026 caso tenha retorna e adiciona outra seguencia.
                            IF wa_verifica_zib IS INITIAL AND w_zfit0026 IS INITIAL.
                              w = 1.
                            ELSE.

                              CLEAR: obj_key.
                              seq_novo = seq_novo + 1.
                              CONCATENATE wa_zfit0026_gera-vbeln seq_novo sy-datum(4) INTO obj_key.

                            ENDIF.
                          ENDWHILE.
                        ENDIF.

                        CLEAR: w_zfit0026.
                        " READ TABLE it_saida INTO wa_vbap WITH KEY vbeln  = wa_zfit0026-vbeln.

                        zib_contabil_wa-obj_key     = obj_key.
                        zib_contabil_wa-seqitem     = '0001'.

                        "De => para centro virtual.
                        SELECT SINGLE centro_real
                        FROM zsdt_depara_cen
                        INTO @DATA(w_werks)
                        WHERE centrov_1 EQ @wa_vbap-werks.
                        IF w_werks IS NOT INITIAL.
                          zib_contabil_wa-gsber       = w_werks.
                        ELSE.
                          zib_contabil_wa-gsber       = wa_vbap-werks.
                        ENDIF.
                        CLEAR: w_werks.

                        zib_contabil_wa-bukrs       = wa_zfit0026-bukrs.
                        zib_contabil_wa-interface   = '96'.

                        zib_contabil_wa-bktxt       = 'Recbto Venda Insumos'.

                        zib_contabil_wa-bldat       = data.
                        zib_contabil_wa-budat       = data.
                        zib_contabil_wa-gjahr       = ano.
                        zib_contabil_wa-monat       = mes.

                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                          EXPORTING
                            input  = wa_zfit0026-vbeln
                          IMPORTING
                            output = zib_contabil_wa-xblnr.

                        zib_contabil_wa-blart       = 'ND'.

                        " Lançamento - Conta de Débito
                        CONCATENATE 'Ordem de Venda Insumos' wa_zfit0026-vbeln INTO sgtxt SEPARATED BY space.

                        zib_contabil_wa-bschl          = '09'.
                        "zib_contabil_wa-hkont          = wa_vbap-kunnr.
                        zib_contabil_wa-waers          = wa_zfit0026-moeda.
                        zib_contabil_wa-zfbdt          = data.
                        zib_contabil_wa-zlspr          = ' '.
                        zib_contabil_wa-zlsch          = wa_zfit0026-forma_pag.
                        zib_contabil_wa-kidno          = space.
                        zib_contabil_wa-sgtxt          = sgtxt.
                        zib_contabil_wa-xref1          = space.
                        zib_contabil_wa-xref2          = space.
                        zib_contabil_wa-xref3          = space.

                        "De => para centro virtual.
                        SELECT SINGLE centro_real
                        FROM zsdt_depara_cen
                        INTO w_werks
                        WHERE centrov_1 EQ wa_vbap-werks.
                        IF w_werks IS NOT INITIAL.
                          zib_contabil_wa-bupla       = w_werks.
                        ELSE.
                          zib_contabil_wa-bupla       = wa_vbap-werks.
                        ENDIF.
                        CLEAR: w_werks.
*      ZIB_CONTABIL_WA-BUPLA          = wa_vbap-VKBUR.
                        zib_contabil_wa-zterm          = wa_zfit0026-zterm.

                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                          EXPORTING
                            input  = wa_zfit0026-vbeln
                          IMPORTING
                            output = zib_contabil_wa-zuonr.

                        zib_contabil_wa-umskz          = 'L'.

                        zib_contabil_wa-kostl          = space.
                        zib_contabil_wa-aufnr          = space.
                        zib_contabil_wa-prctr          = space.
                        zib_contabil_wa-waers_i        = 'BRL'.
                        zib_contabil_wa-waers_f        = wa_zfit0026-moeda.

                        IF ( wa_zfit0026-moeda EQ 'USD' ).
                          zib_contabil_wa-wrbtr          = wa_zfit0026-mont_moeda + ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                          zib_contabil_wa-dmbtr          = wa_zfit0026-mont_mi    + ( ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ) * wa_zfit0026-taxa ).
                          zib_contabil_wa-dmbe2          = wa_zfit0026-mont_moeda + ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                        ELSE.
                          zib_contabil_wa-wrbtr          = wa_zfit0026-mont_moeda + ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                          zib_contabil_wa-dmbtr          = wa_zfit0026-mont_mi    + ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                          zib_contabil_wa-dmbe2          = space.
                        ENDIF.

                        zib_contabil_wa-bvtyp          = space.
                        zib_contabil_wa-hbkid          = space.
                        zib_contabil_wa-rg_atualizado  = 'N'.
                        zib_contabil_wa-bankl          = space.
                        zib_contabil_wa-bankn          = space.
                        zib_contabil_wa-newbw          = space.
                        zib_contabil_wa-anln1          = space.
                        zib_contabil_wa-anln2          = space.

                        INSERT INTO zib_contabil VALUES zib_contabil_wa.
                        IF sy-subrc IS INITIAL.
                          COMMIT WORK.
                        ELSE.
                          ROLLBACK WORK.
                        ENDIF.

                        CLEAR: zib_contabil_wa.

                        zib_contabil_wa-obj_key     = obj_key.
                        zib_contabil_wa-seqitem     = '0002'.

                        "De => para centro virtual.
                        SELECT SINGLE centro_real
                        FROM zsdt_depara_cen
                        INTO w_werks
                        WHERE centrov_1 EQ wa_vbap-werks.
                        IF w_werks IS NOT INITIAL.
                          zib_contabil_wa-gsber       = w_werks.
                        ELSE.
                          zib_contabil_wa-gsber       = wa_vbap-werks.
                        ENDIF.
                        CLEAR: w_werks.

                        zib_contabil_wa-bukrs       = wa_zfit0026-bukrs.
                        zib_contabil_wa-interface   = '96'.

                        zib_contabil_wa-bktxt       = 'Recbto Venda Insumos'.

                        zib_contabil_wa-bldat       = data.
                        zib_contabil_wa-budat       = data.
                        zib_contabil_wa-gjahr       = ano.
                        zib_contabil_wa-monat       = mes.

                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                          EXPORTING
                            input  = wa_zfit0026-vbeln
                          IMPORTING
                            output = zib_contabil_wa-xblnr.

                        IF wa_zfit0026-mont_moeda NE 0.
                          zib_contabil_wa-bschl          = '19'.
                          "zib_contabil_wa-hkont          = wa_vbap-kunnr.

                          zib_contabil_wa-blart       = 'ND'.

                          zib_contabil_wa-waers          = wa_zfit0026-moeda.
                          zib_contabil_wa-zfbdt          = data.
                          zib_contabil_wa-zlspr          = ' '.
                          zib_contabil_wa-zlsch          = wa_zfit0026-forma_pag.
                          zib_contabil_wa-kidno          = space.
                          zib_contabil_wa-sgtxt          = sgtxt.
                          zib_contabil_wa-xref1          = space.
                          zib_contabil_wa-xref2          = space.
                          zib_contabil_wa-xref3          = space.

                          "De => para centro virtual.
                          SELECT SINGLE centro_real
                          FROM zsdt_depara_cen
                          INTO w_werks
                          WHERE centrov_1 EQ wa_vbap-werks.
                          IF w_werks IS NOT INITIAL.
                            zib_contabil_wa-bupla       = w_werks.
                          ELSE.
                            zib_contabil_wa-bupla       = wa_vbap-werks.
                          ENDIF.
                          CLEAR: w_werks.

                          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                            EXPORTING
                              input  = wa_zfit0026-vbeln
                            IMPORTING
                              output = zib_contabil_wa-zuonr.
                          zib_contabil_wa-umskz          = 'L'.
                          zib_contabil_wa-kostl          = space.
                          zib_contabil_wa-aufnr          = space.
                          zib_contabil_wa-prctr          = space.
                          zib_contabil_wa-waers_i        = 'BRL'.
                          zib_contabil_wa-waers_f        = wa_zfit0026-moeda.

                          IF ( wa_zfit0026-moeda EQ 'USD' ).
                            zib_contabil_wa-wrbtr          = wa_zfit0026-mont_moeda.
                            zib_contabil_wa-dmbtr          = wa_zfit0026-mont_mi.
                            zib_contabil_wa-dmbe2          = wa_zfit0026-mont_moeda.
                          ELSE.
                            zib_contabil_wa-wrbtr          = wa_zfit0026-mont_moeda.
                            zib_contabil_wa-dmbtr          = wa_zfit0026-mont_mi.
                            zib_contabil_wa-dmbe2          = space.
                          ENDIF.

                          zib_contabil_wa-bvtyp          = space.
                          zib_contabil_wa-hbkid          = space.
                          zib_contabil_wa-rg_atualizado  = 'N'.
                          zib_contabil_wa-bankl          = space.
                          zib_contabil_wa-bankn          = space.
                          zib_contabil_wa-newbw          = space.
                          zib_contabil_wa-anln1          = space.
                          zib_contabil_wa-anln2          = space.

                          INSERT INTO zib_contabil VALUES zib_contabil_wa.
                          IF sy-subrc IS INITIAL.

                            COMMIT WORK.
                          ELSE.

                            ROLLBACK WORK.

                          ENDIF.

                        ENDIF.
                        CLEAR zib_contabil_wa.

                        DATA x_total TYPE netwr.

                        x_total =  wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo.

                        IF x_total > 0.

                          CONCATENATE 'Receita de Juros sobre OV. Insumos' wa_zfit0026-vbeln INTO sgtxt SEPARATED BY space.

                          zib_contabil_wa-obj_key   = obj_key.
                          zib_contabil_wa-seqitem   = '0003'.

                          "De => para centro virtual.
                          SELECT SINGLE centro_real
                          FROM zsdt_depara_cen
                          INTO w_werks
                          WHERE centrov_1 EQ wa_vbap-werks.
                          IF w_werks IS NOT INITIAL.
                            zib_contabil_wa-gsber     = w_werks.
                          ELSE.
                            zib_contabil_wa-gsber     = wa_vbap-werks.
                          ENDIF.
                          CLEAR: w_werks.

                          zib_contabil_wa-bukrs     = wa_vbak-bukrs_vf.
                          zib_contabil_wa-interface = '96'.

                          zib_contabil_wa-bktxt = 'Recbto Juros/multa Insumos'.

                          zib_contabil_wa-bldat = data.
                          zib_contabil_wa-budat = data.
                          zib_contabil_wa-gjahr = ano.
                          zib_contabil_wa-monat = mes.
                          zib_contabil_wa-xblnr = |{ wa_zfit0026-vbeln ALPHA = OUT }|.

                          zib_contabil_wa-bschl = '50'.

                          SELECT SINGLE * FROM kna1
                               INTO  wa_kna1
                             WHERE kunnr EQ  wa_vbak-kunnr.

                          zib_contabil_wa-vbund = wa_kna1-vbund.

                          IF zib_contabil_wa-vbund EQ 'SOCIOS'.
                            zib_contabil_wa-hkont          = '331004'.
                          ELSE.
                            zib_contabil_wa-hkont          = '331002'.
                          ENDIF.

                          zib_contabil_wa-blart  = 'ND'.

                          zib_contabil_wa-waers     = wa_zfit0026-moeda.
                          zib_contabil_wa-zfbdt     = data.
                          zib_contabil_wa-zlspr     = ' '.
                          zib_contabil_wa-zlsch     = wa_zfit0026-forma_pag.
                          zib_contabil_wa-kidno     = space.
                          zib_contabil_wa-sgtxt     = sgtxt.
                          zib_contabil_wa-xref1     = space.
                          zib_contabil_wa-xref2     = space.
                          zib_contabil_wa-xref3     = space.

                          "De => para centro virtual.
                          SELECT SINGLE centro_real
                          FROM zsdt_depara_cen
                          INTO w_werks
                          WHERE centrov_1 EQ wa_vbap-werks.
                          IF w_werks IS NOT INITIAL.
                            zib_contabil_wa-bupla     = w_werks.
                          ELSE.
                            zib_contabil_wa-bupla     = wa_vbap-werks.
                          ENDIF.
                          CLEAR: w_werks.

*        ZIB_CONTABIL_WA-BUPLA     = wa_vbap-VKBUR.

                          zib_contabil_wa-zuonr = |{ wa_zfit0026-vbeln ALPHA = OUT }|.

                          zib_contabil_wa-kostl     = space.
                          zib_contabil_wa-aufnr     = space.
                          zib_contabil_wa-prctr     = space.
                          zib_contabil_wa-waers_i   = 'BRL'.
                          zib_contabil_wa-waers_f   = wa_zfit0026-moeda.

                          IF ( wa_zfit0026-moeda EQ 'USD' ).
                            zib_contabil_wa-wrbtr = ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                            zib_contabil_wa-dmbtr = ( ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ) * wa_zfit0026-taxa ).
                            zib_contabil_wa-dmbe2 = ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                          ELSE.
                            zib_contabil_wa-wrbtr = ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                            zib_contabil_wa-dmbtr = ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                            zib_contabil_wa-dmbe2 = space.
                          ENDIF.

                          zib_contabil_wa-bvtyp          = space.
                          zib_contabil_wa-hbkid          = space.
                          zib_contabil_wa-rg_atualizado  = 'N'.
                          zib_contabil_wa-bankl          = space.
                          zib_contabil_wa-bankn          = space.
                          zib_contabil_wa-newbw          = space.
                          zib_contabil_wa-anln1          = space.
                          zib_contabil_wa-anln2          = space.

                          INSERT INTO zib_contabil VALUES zib_contabil_wa.
                          IF sy-subrc IS INITIAL.

                            COMMIT WORK.
                          ELSE.

                            ROLLBACK WORK.

                          ENDIF.
                          CLEAR zib_contabil_wa.

                        ENDIF.

                        IF ( sy-subrc EQ 0 ).

                          UPDATE zfit0026 SET  seq = seq_novo
                                               obj_key = obj_key
                                              status  = 'P'
                                         WHERE vbeln  EQ wa_zfit0026-vbeln
                                           AND seq    EQ wa_zfit0026-seq.
                          IF sy-subrc IS INITIAL.

                            COMMIT WORK.
                          ELSE.

                            ROLLBACK WORK.

                          ENDIF.

                          WHILE i LT 1 .

                            SELECT SINGLE *
                       FROM zib_contabil_chv
                       INTO @DATA(w_zib_contabil_chv1)
                       WHERE obj_key EQ @obj_key
                         AND NOT EXISTS ( SELECT *
                                            FROM bkpf
                                           WHERE belnr EQ zib_contabil_chv~belnr
                                             AND bukrs EQ zib_contabil_chv~bukrs
                                             AND gjahr EQ zib_contabil_chv~gjahr
                                             AND stblg NE @space ).


                            IF sy-subrc IS INITIAL.

                              UPDATE zfit0026 SET docnum     = w_zib_contabil_chv1-belnr
                                                   status     = 'G'
                                       WHERE vbeln    = wa_zfit0026-vbeln
                                         AND seq      = wa_zfit0026-seq.
                              IF sy-subrc IS INITIAL.

                                COMMIT WORK.
                              ELSE.

                                ROLLBACK WORK.

                              ENDIF.
                              i = i + 1.
                              lwa_data_response_doc-doc_contabil = w_zib_contabil_chv1-belnr.
                            ELSE.


                              SELECT  *
                                                  FROM zib_contabil_err
                                                  INTO TABLE @DATA(it_zib_contabil_err)
                                                  WHERE obj_key EQ @obj_key.
*                       AND NOT EXISTS ( SELECT *
*                                          FROM bkpf
*                                         WHERE belnr EQ zib_contabil_chv~belnr
*                                           AND bukrs EQ zib_contabil_chv~bukrs
*                                           AND gjahr EQ zib_contabil_chv~gjahr
*                                           AND stblg NE @space ).


                              IF sy-subrc IS INITIAL.
                                i = i + 1.
                                LOOP AT it_zib_contabil_err INTO DATA(wa_erro).

                                  APPEND wa_erro-message TO <fs_erro>-erros.
                                  l_erro = 'X'.
                                ENDLOOP.

                                UPDATE zfit0026 SET status     = ' '
                                       WHERE vbeln    = wa_zfit0026-vbeln
                                         AND seq      = wa_zfit0026-seq.
                                IF sy-subrc IS INITIAL.

                                  COMMIT WORK.
                                ELSE.

                                  ROLLBACK WORK.

                                ENDIF.
                              ENDIF.

                            ENDIF.


                          ENDWHILE.
                        ENDIF.
                      ENDIF.
                    ENDIF.

                  ELSE.

                    zib_contabil_wa-obj_key     = obj_key.
                    zib_contabil_wa-seqitem     = '0001'.

                    "De => para centro virtual.
                    SELECT SINGLE centro_real
                     FROM zsdt_depara_cen
                     INTO @DATA(w_werks1)
                     WHERE centrov_1 EQ @wa_vbap-werks.
                    IF w_werks1 IS NOT INITIAL.
                      zib_contabil_wa-gsber       = w_werks1.
                    ELSE.
                      zib_contabil_wa-gsber       = wa_vbap-werks.
                    ENDIF.
                    CLEAR: w_werks1.


                    zib_contabil_wa-bukrs       = wa_vbak-bukrs_vf.
                    zib_contabil_wa-interface   = '96'.

                    zib_contabil_wa-bktxt       = 'Recbto Venda Insumos'.

                    zib_contabil_wa-bldat       = data.
                    zib_contabil_wa-budat       = data.
                    zib_contabil_wa-gjahr       = ano.
                    zib_contabil_wa-monat       = mes.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      EXPORTING
                        input  = wa_zfit0026-vbeln
                      IMPORTING
                        output = zib_contabil_wa-xblnr.

                    zib_contabil_wa-blart       = 'ND'.

                    " Lançamento - Conta de Débito

                    CONCATENATE 'Ordem de Venda Insumos' wa_zfit0026-vbeln INTO sgtxt SEPARATED BY space.

                    zib_contabil_wa-bschl          = '09'.
                    zib_contabil_wa-hkont          = wa_vbak-kunnr.
                    zib_contabil_wa-waers          = wa_zfit0026-moeda.
                    "ZIB_CONTABIL_WA-ZFBDT          = SPACE.
                    zib_contabil_wa-zfbdt          = data.
                    zib_contabil_wa-zlspr          = ' '.
                    zib_contabil_wa-zlsch          = wa_zfit0026-forma_pag.
                    zib_contabil_wa-kidno          = space.
                    zib_contabil_wa-sgtxt          = sgtxt.
                    zib_contabil_wa-xref1          = space.
                    zib_contabil_wa-xref2          = space.
                    zib_contabil_wa-xref3          = space.

                    "De => para centro virtual.
                    SELECT SINGLE centro_real
                    FROM zsdt_depara_cen
                    INTO w_werks
                    WHERE centrov_1 EQ wa_vbap-werks.
                    IF w_werks IS NOT INITIAL.
                      zib_contabil_wa-bupla       = w_werks.
                    ELSE.
                      zib_contabil_wa-bupla       = wa_vbap-werks.
                    ENDIF.
                    CLEAR: w_werks.

*                zib_contabil_wa-bupla          = wa_vbap-vkbur. "Escritório de vendas "Comentado AOENNING 28/04/2020
                    zib_contabil_wa-zterm          = wa_zfit0026-zterm.
                    IF wa_zfit0026-moeda EQ 'USD'.
                      zib_contabil_wa-kurrf          = wa_zfit0026-taxa.
                    ENDIF.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      EXPORTING
                        input  = wa_zfit0026-vbeln
                      IMPORTING
                        output = zib_contabil_wa-zuonr.


                    zib_contabil_wa-umskz  = 'L'.
                    zib_contabil_wa-kostl          = space.
                    zib_contabil_wa-aufnr          = space.
                    zib_contabil_wa-prctr          = space.
                    zib_contabil_wa-waers_i        = 'BRL'.
                    zib_contabil_wa-waers_f        = wa_zfit0026-moeda.

                    IF ( wa_zfit0026-moeda EQ 'USD' ).
                      zib_contabil_wa-wrbtr          = wa_zfit0026-mont_moeda + ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
                      zib_contabil_wa-dmbtr          = wa_zfit0026-mont_mi    + ( ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ) * wa_zfit0026-taxa ).
                      zib_contabil_wa-dmbe2          = wa_zfit0026-mont_moeda + ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
                    ELSE.
                      zib_contabil_wa-wrbtr          = wa_zfit0026-mont_moeda + ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
                      zib_contabil_wa-dmbtr          = wa_zfit0026-mont_mi    + ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
                      zib_contabil_wa-dmbe2          = space.
                    ENDIF.

                    zib_contabil_wa-bvtyp          = space.
                    zib_contabil_wa-hbkid          = space.
                    zib_contabil_wa-rg_atualizado  = 'N'.
                    zib_contabil_wa-bankl          = space.
                    zib_contabil_wa-bankn          = space.
                    zib_contabil_wa-newbw          = space.
                    zib_contabil_wa-anln1          = space.
                    zib_contabil_wa-anln2          = space.


                    INSERT INTO zib_contabil VALUES zib_contabil_wa.
                    IF sy-subrc IS INITIAL.

                      COMMIT WORK.
                    ELSE.

                      ROLLBACK WORK.

                    ENDIF.

                    CLEAR: zib_contabil_wa.


                    zib_contabil_wa-obj_key     = obj_key.
                    zib_contabil_wa-seqitem     =  '0002'.

                    "De => para centro virtual.
                    SELECT SINGLE centro_real
                    FROM zsdt_depara_cen
                    INTO w_werks
                    WHERE centrov_1 EQ wa_vbap-werks.
                    IF w_werks IS NOT INITIAL.
                      zib_contabil_wa-gsber       = w_werks.
                    ELSE.
                      zib_contabil_wa-gsber       = wa_vbap-werks.
                    ENDIF.
                    CLEAR: w_werks.

                    zib_contabil_wa-bukrs       = wa_vbak-bukrs_vf.
                    zib_contabil_wa-interface   = '96'.

                    zib_contabil_wa-bktxt       = 'Recbto Venda Insumos'.
                    zib_contabil_wa-bldat       = data.
                    zib_contabil_wa-budat       = data.
                    zib_contabil_wa-gjahr       = ano.
                    zib_contabil_wa-monat       = mes.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      EXPORTING
                        input  = wa_zfit0026-vbeln
                      IMPORTING
                        output = zib_contabil_wa-xblnr.

                    IF wa_zfit0026-mont_moeda NE 0.
                      zib_contabil_wa-bschl          = '19'.
                      zib_contabil_wa-hkont          = wa_vbak-kunnr.
                      zib_contabil_wa-blart       = 'ND'.
                      zib_contabil_wa-waers          = wa_zfit0026-moeda.
                      zib_contabil_wa-zfbdt          = data.
                      zib_contabil_wa-zlspr          = ' '.
                      zib_contabil_wa-zlsch          = wa_zfit0026-forma_pag.
                      zib_contabil_wa-kidno          = space.
                      zib_contabil_wa-sgtxt          = sgtxt.
                      zib_contabil_wa-xref1          = space.
                      zib_contabil_wa-xref2          = space.
                      zib_contabil_wa-xref3          = space.

                      "De => para centro virtual.
                      SELECT SINGLE centro_real
                      FROM zsdt_depara_cen
                      INTO w_werks
                      WHERE centrov_1 EQ wa_vbap-werks.
                      IF w_werks IS NOT INITIAL.
                        zib_contabil_wa-bupla   = w_werks.
                      ELSE.
                        zib_contabil_wa-bupla   = wa_vbap-werks.
                      ENDIF.
                      CLEAR: w_werks.


                      IF wa_zfit0026-moeda EQ 'USD'.
                        zib_contabil_wa-kurrf          = wa_zfit0026-taxa.
                      ENDIF.
*              ZIB_CONTABIL_WA-ZTERM          = wa_zfit0026-ZTERM.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                        EXPORTING
                          input  = wa_zfit0026-vbeln
                        IMPORTING
                          output = zib_contabil_wa-zuonr.


                      zib_contabil_wa-umskz          = 'L'.

                      "ZIB_CONTABIL_WA-UMSKZ          = wa_zfit0026-RAZAO_ESPECIAL.
                      zib_contabil_wa-kostl          = space.
                      zib_contabil_wa-aufnr          = space.
                      zib_contabil_wa-prctr          = space.
                      zib_contabil_wa-waers_i        = 'BRL'.
                      zib_contabil_wa-waers_f        = wa_zfit0026-moeda.

                      IF ( wa_zfit0026-moeda EQ 'USD' ).
                        zib_contabil_wa-wrbtr          = wa_zfit0026-mont_moeda.
                        zib_contabil_wa-dmbtr          = wa_zfit0026-mont_mi.
                        zib_contabil_wa-dmbe2          = wa_zfit0026-mont_moeda.
                      ELSE.
                        zib_contabil_wa-wrbtr          = wa_zfit0026-mont_moeda.
                        zib_contabil_wa-dmbtr          = wa_zfit0026-mont_mi.
                        zib_contabil_wa-dmbe2          = space.
                      ENDIF.

                      zib_contabil_wa-bvtyp          = space.
                      zib_contabil_wa-hbkid          = space.
                      zib_contabil_wa-rg_atualizado  = 'N'.
                      zib_contabil_wa-bankl          = space.
                      zib_contabil_wa-bankn          = space.
                      zib_contabil_wa-newbw          = space.
                      zib_contabil_wa-anln1          = space.
                      zib_contabil_wa-anln2          = space.


                      INSERT INTO zib_contabil VALUES zib_contabil_wa.
                      IF sy-subrc IS INITIAL.

                        COMMIT WORK.
                      ELSE.

                        ROLLBACK WORK.

                      ENDIF.
                    ENDIF.

                    CLEAR: zib_contabil_wa, sgtxt.

                    x_total = ( wa_zfit0026-vlr_juros_rbdo + wa_zfit0026-vlr_multa_rbdo ).
                    IF x_total > 0.


                      CONCATENATE 'Receita de Juros sobre OV. Insumos' wa_zfit0026-vbeln INTO sgtxt SEPARATED BY space.
                      zib_contabil_wa-obj_key     =  obj_key.
                      zib_contabil_wa-seqitem     = '0003'.

                      "De => para centro virtual.
                      SELECT SINGLE centro_real
                      FROM zsdt_depara_cen
                      INTO w_werks
                      WHERE centrov_1 EQ wa_vbap-werks.
                      IF w_werks IS NOT INITIAL.
                        zib_contabil_wa-gsber       = w_werks.
                      ELSE.
                        zib_contabil_wa-gsber       = wa_vbap-werks.
                      ENDIF.
                      CLEAR: w_werks.

                      zib_contabil_wa-bukrs       = wa_vbak-bukrs_vf.
                      zib_contabil_wa-interface   = '96'.
                      zib_contabil_wa-bktxt = 'Recbto Juros/multa Insumos'.
                      zib_contabil_wa-bldat = data.
                      zib_contabil_wa-budat = data.
                      zib_contabil_wa-gjahr = ano.
                      zib_contabil_wa-monat = mes.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = wa_zfit0026-vbeln
                        IMPORTING
                          output = zib_contabil_wa-xblnr.

                      zib_contabil_wa-bschl          = '50'.

                      SELECT SINGLE * FROM kna1
                           INTO  wa_kna1
                         WHERE kunnr EQ  wa_vbak-kunnr.

                      zib_contabil_wa-vbund = wa_kna1-vbund.

                      IF zib_contabil_wa-vbund EQ 'SOCIOS'.
                        zib_contabil_wa-hkont          = '331004'.
                      ELSE.
                        zib_contabil_wa-hkont          = '331002'.
                      ENDIF.
                      zib_contabil_wa-blart       = 'ND'.

                      zib_contabil_wa-waers    = wa_zfit0026-moeda.
                      zib_contabil_wa-zfbdt    = data.
                      zib_contabil_wa-zlspr    = ' '.
                      zib_contabil_wa-zlsch    = wa_zfit0026-forma_pag.
                      zib_contabil_wa-kidno    = space.
                      zib_contabil_wa-sgtxt    = sgtxt.
                      zib_contabil_wa-xref1    = space.
                      zib_contabil_wa-xref2    = space.
                      zib_contabil_wa-xref3    = space.

                      "De => para centro virtual.
                      SELECT SINGLE centro_real
                      FROM zsdt_depara_cen
                      INTO w_werks
                      WHERE centrov_1 EQ wa_vbap-werks.
                      IF w_werks IS NOT INITIAL.
                        zib_contabil_wa-bupla    = w_werks.
                      ELSE.
                        zib_contabil_wa-bupla    = wa_vbap-werks.
                      ENDIF.
                      CLEAR: w_werks.


                      IF wa_zfit0026-moeda EQ 'USD'.
                        zib_contabil_wa-kurrf    = wa_zfit0026-taxa.
                      ENDIF.

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                        EXPORTING
                          input  = wa_zfit0026-vbeln
                        IMPORTING
                          output = zib_contabil_wa-zuonr.

                      zib_contabil_wa-kostl   = space.
                      zib_contabil_wa-aufnr   = space.
                      zib_contabil_wa-prctr   = space.
                      zib_contabil_wa-waers_i = 'BRL'.
                      zib_contabil_wa-waers_f = wa_zfit0026-moeda.

                      IF wa_zfit0026-moeda EQ 'USD'.
                        zib_contabil_wa-wrbtr = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
                        zib_contabil_wa-dmbtr = ( ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ) * wa_zfit0026-taxa ).
                        zib_contabil_wa-dmbe2 = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
                      ELSE.
                        zib_contabil_wa-wrbtr  = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
                        zib_contabil_wa-dmbtr  = ( wa_zfit0026-vlr_multa_rbdo + wa_zfit0026-vlr_juros_rbdo ).
                        zib_contabil_wa-dmbe2  = space.
                      ENDIF.

                      zib_contabil_wa-bvtyp          = space.
                      zib_contabil_wa-hbkid          = space.
                      zib_contabil_wa-rg_atualizado  = 'N'.
                      zib_contabil_wa-bankl          = space.
                      zib_contabil_wa-bankn          = space.
                      zib_contabil_wa-newbw          = space.
                      zib_contabil_wa-anln1          = space.
                      zib_contabil_wa-anln2          = space.

                      INSERT INTO zib_contabil VALUES zib_contabil_wa.
                      IF sy-subrc IS INITIAL.

                        COMMIT WORK.
                      ELSE.

                        ROLLBACK WORK.

                      ENDIF.
                      CLEAR: zib_contabil_wa.
                    ENDIF.

                    IF ( sy-subrc EQ 0 ).

                      UPDATE zfit0026 SET  seq = seq_novo
                                           obj_key = obj_key
                                           status  = 'P'
                                         WHERE vbeln  EQ wa_zfit0026-vbeln
                                           AND seq    EQ wa_zfit0026-seq.
                      IF sy-subrc IS INITIAL.

                        COMMIT WORK.
                      ELSE.

                        ROLLBACK WORK.

                      ENDIF.


                      WHILE i LT 1 .

                        SELECT SINGLE *
                   FROM zib_contabil_chv
                   INTO @DATA(w_zib_contabil_chv)
                   WHERE obj_key EQ @obj_key
                     AND NOT EXISTS ( SELECT *
                                        FROM bkpf
                                       WHERE belnr EQ zib_contabil_chv~belnr
                                         AND bukrs EQ zib_contabil_chv~bukrs
                                         AND gjahr EQ zib_contabil_chv~gjahr
                                         AND stblg NE @space ).


                        IF sy-subrc IS INITIAL.

                          UPDATE zfit0026 SET docnum     = w_zib_contabil_chv-belnr
                                              status     = 'G'
                                   WHERE vbeln    = wa_zfit0026-vbeln
                                     AND seq      = wa_zfit0026-seq.
                          IF sy-subrc IS INITIAL.

                            COMMIT WORK.
                          ELSE.

                            ROLLBACK WORK.

                          ENDIF.
                          i = i + 1.
                          lwa_data_response_doc-doc_contabil = w_zib_contabil_chv-belnr.
                        ELSE.


                          SELECT  *
                                              FROM zib_contabil_err
                                              INTO TABLE @DATA(it_zib_contabil_err3)
                                              WHERE obj_key EQ @obj_key.
*                       AND NOT EXISTS ( SELECT *
*                                          FROM bkpf
*                                         WHERE belnr EQ zib_contabil_chv~belnr
*                                           AND bukrs EQ zib_contabil_chv~bukrs
*                                           AND gjahr EQ zib_contabil_chv~gjahr
*                                           AND stblg NE @space ).


                          IF sy-subrc IS INITIAL.
                            i = i + 1.
                            LOOP AT it_zib_contabil_err3 INTO DATA(wa_erro3).

                              APPEND wa_erro3-message TO <fs_erro>-erros.
                              l_erro = 'X'.
                            ENDLOOP.
                            UPDATE zfit0026 SET status     = ' '
                                      WHERE vbeln    = wa_zfit0026-vbeln
                                        AND seq      = wa_zfit0026-seq.
                            IF sy-subrc IS INITIAL.

                              COMMIT WORK.
                            ELSE.

                              ROLLBACK WORK.

                            ENDIF.
                          ENDIF.

                        ENDIF.

                      ENDWHILE.

                    ELSE.
                      wa_mensagem =  'Lançamento não foi gerado'.
                      APPEND wa_mensagem TO <fs_erro>-erros.
                      l_erro = 'X'.
                    ENDIF.
                  ENDIF.
                  CLEAR: zib_contabil_wa, wa_zfit0026, wa_vbap.
                ENDIF.
          ELSE.

          ENDIF.
  endmethod.


  METHOD Z_GERA_DOCUMENTO_CONTABIL.

    "R_IF_INTEGRACAO_INJECT = ME.
    "E_SUCESSO = ABAP_TRUE.
    DATA: LWA_DATA_REQUEST       LIKE ZDE_DATA_REQUEST,
          LWA_DATA_RESPONSE_ERRO LIKE ZDE_DATA_RESPONSE,
          LWA_DATA_RESPONSE_DOC  LIKE ZDE_DATA_RESPONSE_DOC.
    DATA: E_JSON    TYPE STRING,
          L_ERRO(1).

    TYPES: BEGIN OF TY_ZFIT0026_GERA,
             VBELN   TYPE ZFIT0026-VBELN,
             SEQ     TYPE ZFIT0026-SEQ,
             OBJ_KEY TYPE ZFIT0026-OBJ_KEY,
           END OF TY_ZFIT0026_GERA,

           BEGIN OF TY_ZIB_CONTABIL_CONT,
             OBJ_KEY   TYPE ZIB_CONTABIL-OBJ_KEY,
             SEQITEM   TYPE ZIB_CONTABIL-SEQITEM,
             BUKRS     TYPE ZIB_CONTABIL-BUKRS,
             INTERFACE TYPE ZIB_CONTABIL-INTERFACE,
             XBLNR     TYPE ZIB_CONTABIL-XBLNR,
           END OF TY_ZIB_CONTABIL_CONT.

    DATA: IT_ZFIT0026_GERA    TYPE TABLE OF TY_ZFIT0026_GERA,
          WA_ZFIT0026_GERA    TYPE TY_ZFIT0026_GERA,
          TABIX               TYPE SY-TABIX,
          SEQ_NOVO            TYPE ZFIT0026-SEQ,
          WA_VERIFICA_ZIB     TYPE ZIB_CONTABIL,
          WA_ZIB_CONTABIL_AUX TYPE ZIB_CONTABIL,
          IT_ZIB_CONT         TYPE TABLE OF TY_ZIB_CONTABIL_CONT,
          WA_ZIB_CONT         TYPE TY_ZIB_CONTABIL_CONT,
          VBELN_AUX_P         TYPE ZIB_CONTABIL-XBLNR,
          WA_KNA1             TYPE KNA1.


    DATA: DATA(10)             TYPE C,
          DIA(2)               TYPE C,
          MES(2)               TYPE C,
          ANO(4)               TYPE C,
          SGTXT                TYPE SGTXT,
          W_ZFIT0026           TYPE ZFIT0026,
          QTD                  TYPE SY-TABIX,
          CONT                 TYPE NUMC10,
          OBJ_KEY              TYPE AWKEY,
          WA_0159              TYPE ZSDT0159,
          V_PROCESSOU(1),
          W_WERKS(4),
          W_ZIB_CONTABIL_CHV   TYPE  ZIB_CONTABIL_CHV,
          IT_ZIB_CONTABIL_ERR3 TYPE TABLE OF ZIB_CONTABIL_ERR,
          WA_ERRO3             TYPE ZIB_CONTABIL_ERR.

    DATA: DATA_SUM TYPE DATUM,
          TAXA     TYPE ZFIT0026-TAXA.


    DATA: VG_KNA1       TYPE KNA1.
    TYPES:
      BEGIN OF TY_ERRO,
        MENSAGEM TYPE CHAR100,
      END OF TY_ERRO.

    DATA: IT_MENSAGEM TYPE TABLE OF  TY_ERRO,
          WA_MENSAGEM TYPE  TY_ERRO.


    DATA: IT_ZSDT0040 TYPE TABLE OF ZSDT0040,
          WA_ZSDT0040 TYPE ZSDT0040,
          WA_ZSDT0041 TYPE ZSDT0041,
          WA_ZSDT0090 TYPE ZSDT0090.

    DATA: VBELN_AUX     TYPE VBAK-VBELN,
          L_STATUS(1)   TYPE C,
          L_MESSAGE(64) TYPE C.
    DATA: WA_VBAK_VALOR TYPE VBAK.
    DATA: P_ERRO(1) TYPE C.
    DATA: RAZAO_ESPECIAL TYPE C.

    DATA: IT_ZIB_CONTABIL_CHV TYPE STANDARD TABLE OF ZIB_CONTABIL_CHV,
          WA_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV,
          IT_Z0159            TYPE STANDARD TABLE OF ZSDT0159,
          WA_Z0159            TYPE ZSDT0159.

    DATA: VL_GDATU TYPE GDATU_INV,
          V_KURRF  TYPE KURRF.


    DATA: IT_VBAP     TYPE TABLE OF VBAP,
          IT_VBKD     TYPE TABLE OF VBKD,
          IT_T052     TYPE TABLE OF T052,
          WA_T052     TYPE T052,
          IT_ZSDT0090 TYPE STANDARD TABLE OF ZSDT0090,
          IT_ZSDT0041 TYPE TABLE OF ZSDT0041,
          IT_KNA1     TYPE TABLE OF KNA1,
          IT_ZFIT0026 TYPE TABLE OF ZFIT0026.
    "ja tendo o numero da OV
    DATA: MONT_OV         TYPE NETWR,
          MONT_PARCIAL    TYPE NETWR,
          MONT_USADO      TYPE NETWR,
          DATUM           LIKE SY-DATUM,
          ZIB_CONTABIL_WA TYPE ZIB_CONTABIL,
          I               TYPE I,
          V_TAXA1         TYPE UKURS_CURR,
          LVA_MSG_ERRO    TYPE STRING.

    DATA: V_TAXA_CONTABIL TYPE KURRF,
          WA_VBAP         TYPE VBAP,
          WA_VBAK         TYPE VBAK,
          WA_ZSDT0053     TYPE ZSDT0053.


    APPEND INITIAL LINE TO LWA_DATA_RESPONSE_ERRO-ERROS ASSIGNING FIELD-SYMBOL(<FS_ERRO>).

    SELECT * FROM VBAK INTO TABLE @DATA(IT_VBAK)
              WHERE VBELN = @LWA_DATA_REQUEST-NUM_OV.

    CHECK NOT IT_VBAK[] IS INITIAL.


    SELECT *
    FROM VBKD
    INTO TABLE IT_VBKD
    FOR ALL ENTRIES IN IT_VBAK
    WHERE VBELN EQ IT_VBAK-VBELN.

    SELECT *
    FROM T052 INTO TABLE IT_T052
    FOR ALL ENTRIES IN  IT_VBKD
    WHERE ZTERM EQ IT_VBKD-ZTERM.


    " Dados de Taxa Travada
    CLEAR: IT_ZSDT0090.
    SELECT *
    FROM ZSDT0090
    INTO TABLE IT_ZSDT0090
    FOR ALL ENTRIES IN IT_VBAK
    WHERE VBELV EQ IT_VBAK-VBELN
    AND ESTORNO   NE 'X' "NAO~TRAZER ESTORNO
    AND CATEGORIA EQ 'C'.

    " Mestre de clientes (parte geral)
    SELECT *
    FROM KNA1
    INTO TABLE IT_KNA1
    FOR ALL ENTRIES IN IT_VBAK
    WHERE KUNNR EQ IT_VBAK-KUNNR.

    SELECT *
    FROM VBAP
    INTO TABLE IT_VBAP
    FOR ALL ENTRIES IN IT_VBAK
    WHERE VBELN EQ IT_VBAK-VBELN
    AND NOT EXISTS ( SELECT *
                     FROM VBEP
                     WHERE VBELN EQ VBAP~VBELN
                     AND POSNR EQ VBAP~POSNR
                     AND LIFSP EQ '12' ).

    READ TABLE IT_VBAP INTO WA_VBAP WITH KEY  VBELn = WA_ZFIT0026-VBELN.
    READ TABLE IT_VBAk INTO WA_VBAk WITH KEY  VBELn = WA_ZFIT0026-VBELN.

    "****************************************************************************************
    "                                      GERAR CONTABIL                                   *
    "****************************************************************************************
    TAXA = 1.

    IF WA_ZFIT0026-AJUSTE <> 'X'."4

      SELECT SINGLE * FROM ZSDT0159
      INTO WA_0159
      WHERE VBELN EQ  WA_ZFIT0026-VBELN.

      SELECT SINGLE * FROM ZSDT0053 INTO WA_ZSDT0053
      WHERE VBELN EQ WA_ZFIT0026-VBELN.

      DIA = WA_ZFIT0026-DATA_PGTO+6(2).
      MES = WA_ZFIT0026-DATA_PGTO+4(2).
      ANO = WA_ZFIT0026-DATA_PGTO(4).

      CONCATENATE DIA '.' MES '.' ANO INTO DATA.
      CONCATENATE WA_ZFIT0026-VBELN WA_ZFIT0026-SEQ SY-DATUM(4) INTO OBJ_KEY.

      SELECT SINGLE * FROM ZIB_CONTABIL INTO  WA_VERIFICA_ZIB WHERE OBJ_KEY EQ OBJ_KEY.

      IF ( SY-SUBRC EQ 0 )."5

        DATA: Z TYPE I.
        Z = 0.
        "me->Z_GERA_CONTABIL.
        SELECT VBELN SEQ OBJ_KEY
        FROM ZFIT0026
        INTO TABLE IT_ZFIT0026_GERA
        WHERE VBELN EQ WA_ZFIT0026-VBELN.

        IF ( SY-SUBRC EQ 0 ).
          READ TABLE IT_ZFIT0026_GERA INTO WA_ZFIT0026_GERA INDEX 1."TABIX.
          IF ( SY-SUBRC EQ 0 ).
            CLEAR OBJ_KEY.
            SEQ_NOVO = WA_ZFIT0026_GERA-SEQ + 1.
            CONCATENATE WA_ZFIT0026_GERA-VBELN SEQ_NOVO SY-DATUM(4) INTO OBJ_KEY.
            WHILE Z EQ 0.

              CLEAR: WA_VERIFICA_ZIB.
              SELECT SINGLE * FROM ZIB_CONTABIL INTO WA_VERIFICA_ZIB WHERE OBJ_KEY EQ OBJ_KEY.
              CLEAR: W_ZFIT0026.
              SELECT SINGLE * FROM ZFIT0026 INTO W_ZFIT0026 WHERE SEQ EQ SEQ_NOVO AND VBELN  EQ WA_ZFIT0026-VBELN.
*          IF ( SY-SUBRC NE 0 ).
***       Verifica sequencia na ZIB e tambem na ZFIT0026 caso tenha retorna e adiciona outra seguencia.
              IF WA_VERIFICA_ZIB IS INITIAL AND W_ZFIT0026 IS INITIAL.
                Z = 1.
              ELSE.

                CLEAR: OBJ_KEY.
                SEQ_NOVO = SEQ_NOVO + 1.
                CONCATENATE WA_ZFIT0026_GERA-VBELN SEQ_NOVO SY-DATUM(4) INTO OBJ_KEY.

              ENDIF.
            ENDWHILE.
          ENDIF.
        ENDIF.

        "ELSE.
      ENDIF.

      ZIB_CONTABIL_WA-OBJ_KEY     = OBJ_KEY.
      ZIB_CONTABIL_WA-SEQITEM     = '0001'.

      "De => para centro virtual.
      SELECT SINGLE CENTRO_REAL
      FROM ZSDT_DEPARA_CEN
      INTO W_WERKS
      WHERE CENTROV_1 EQ WA_VBAP-WERKS.
      IF W_WERKS IS NOT INITIAL.
        ZIB_CONTABIL_WA-GSBER       = W_WERKS.
      ELSE.
        ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
      ENDIF.
      CLEAR: W_WERKS.

      ZIB_CONTABIL_WA-BUKRS       = WA_ZFIT0026-BUKRS.
      ZIB_CONTABIL_WA-INTERFACE   = '96'.
      ZIB_CONTABIL_WA-BKTXT       = 'Recbto Venda Insumos'.
      ZIB_CONTABIL_WA-BLDAT       = DATA.
      ZIB_CONTABIL_WA-BUDAT       = DATA.
      ZIB_CONTABIL_WA-GJAHR       = ANO.
      ZIB_CONTABIL_WA-MONAT       = MES.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_ZFIT0026-VBELN
        IMPORTING
          OUTPUT = ZIB_CONTABIL_WA-XBLNR.

      ZIB_CONTABIL_WA-BLART       = 'ND'.

      CONCATENATE 'Ordem de Venda Insumos' WA_ZFIT0026-VBELN INTO SGTXT SEPARATED BY SPACE.

      ZIB_CONTABIL_WA-BSCHL          = '09'.
      ZIB_CONTABIL_WA-HKONT          = WA_VBAK-KUNNR.
      ZIB_CONTABIL_WA-WAERS          = WA_ZFIT0026-MOEDA.
      ZIB_CONTABIL_WA-ZFBDT          = SPACE.
      ZIB_CONTABIL_WA-ZFBDT          = DATA.
      ZIB_CONTABIL_WA-ZLSPR          = ' '.
      ZIB_CONTABIL_WA-ZLSCH          = WA_ZFIT0026-FORMA_PAG.
      ZIB_CONTABIL_WA-KIDNO          = SPACE.
      ZIB_CONTABIL_WA-SGTXT          = SGTXT.
      ZIB_CONTABIL_WA-XREF1          = SPACE.
      ZIB_CONTABIL_WA-XREF2          = SPACE.
      ZIB_CONTABIL_WA-XREF3          = SPACE.

      "De => para centro virtual.
      SELECT SINGLE CENTRO_REAL
      FROM ZSDT_DEPARA_CEN
      INTO W_WERKS
      WHERE CENTROV_1 EQ WA_VBAP-WERKS.
      IF W_WERKS IS NOT INITIAL.
        ZIB_CONTABIL_WA-BUPLA       = W_WERKS.
      ELSE.
        ZIB_CONTABIL_WA-BUPLA       = WA_VBAP-WERKS.
      ENDIF.
      CLEAR: W_WERKS.

*          zib_contabil_wa-bupla          = wa_saida-vkbur. "Escritório de vendas "Comentado AOENNING 28/04/2020
      ZIB_CONTABIL_WA-ZTERM          = WA_ZFIT0026-ZTERM.
      IF WA_ZFIT0026-MOEDA EQ 'USD'.
        ZIB_CONTABIL_WA-KURRF          = V_TAXA_CONTABIL.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_ZFIT0026-VBELN
        IMPORTING
          OUTPUT = ZIB_CONTABIL_WA-ZUONR.
      ZIB_CONTABIL_WA-UMSKZ  = 'L'.
      ZIB_CONTABIL_WA-KOSTL          = SPACE.
      ZIB_CONTABIL_WA-AUFNR          = SPACE.
      ZIB_CONTABIL_WA-PRCTR          = SPACE.
      ZIB_CONTABIL_WA-WAERS_I        = 'BRL'.
      ZIB_CONTABIL_WA-WAERS_F        = WA_ZFIT0026-MOEDA.

      IF ( WA_ZFIT0026-MOEDA EQ 'USD' ).
        ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
        ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI    + ( ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ) * WA_ZFIT0026-TAXA ).
        ZIB_CONTABIL_WA-DMBE2          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
      ELSE.
        ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
        ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI    + ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
        ZIB_CONTABIL_WA-DMBE2          = SPACE.
      ENDIF.

      ZIB_CONTABIL_WA-BVTYP          = SPACE.
      ZIB_CONTABIL_WA-HBKID          = SPACE.
      ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
      ZIB_CONTABIL_WA-BANKL          = SPACE.
      ZIB_CONTABIL_WA-BANKN          = SPACE.
      ZIB_CONTABIL_WA-NEWBW          = SPACE.
      ZIB_CONTABIL_WA-ANLN1          = SPACE.
      ZIB_CONTABIL_WA-ANLN2          = SPACE.

      INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
      CLEAR: ZIB_CONTABIL_WA.

      ZIB_CONTABIL_WA-OBJ_KEY     = OBJ_KEY.
      ZIB_CONTABIL_WA-SEQITEM     =  '0002'.

      "De => para centro virtual.
      SELECT SINGLE CENTRO_REAL
      FROM ZSDT_DEPARA_CEN
      INTO W_WERKS
      WHERE CENTROV_1 EQ WA_VBAP-WERKS.
      IF W_WERKS IS NOT INITIAL.
        ZIB_CONTABIL_WA-GSBER       = W_WERKS.
      ELSE.
        ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
      ENDIF.
      CLEAR: W_WERKS.

      ZIB_CONTABIL_WA-BUKRS       = WA_ZFIT0026-BUKRS.
      ZIB_CONTABIL_WA-INTERFACE   = '96'.
      ZIB_CONTABIL_WA-BKTXT       = 'Recbto Venda Insumos'.
      ZIB_CONTABIL_WA-BLDAT       = DATA.
      ZIB_CONTABIL_WA-BUDAT       = DATA.
      ZIB_CONTABIL_WA-GJAHR       = ANO.
      ZIB_CONTABIL_WA-MONAT       = MES.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_ZFIT0026-VBELN
        IMPORTING
          OUTPUT = ZIB_CONTABIL_WA-XBLNR.

      IF WA_ZFIT0026-MONT_MOEDA NE 0.
        ZIB_CONTABIL_WA-BSCHL          = '19'.
        ZIB_CONTABIL_WA-HKONT          = WA_VBAK-KUNNR.
        ZIB_CONTABIL_WA-BLART          = 'ND'.
        ZIB_CONTABIL_WA-WAERS          = WA_ZFIT0026-MOEDA.
        ZIB_CONTABIL_WA-ZFBDT          = DATA.
        ZIB_CONTABIL_WA-ZLSPR          = ' '.
        ZIB_CONTABIL_WA-ZLSCH          = WA_ZFIT0026-FORMA_PAG.
        ZIB_CONTABIL_WA-KIDNO          = SPACE.
        ZIB_CONTABIL_WA-SGTXT          = SGTXT.
        ZIB_CONTABIL_WA-XREF1          = SPACE.
        ZIB_CONTABIL_WA-XREF2          = SPACE.
        ZIB_CONTABIL_WA-XREF3          = SPACE.

        "De => para centro virtual.
        SELECT SINGLE CENTRO_REAL
        FROM ZSDT_DEPARA_CEN
        INTO W_WERKS
        WHERE CENTROV_1 EQ WA_VBAP-WERKS.
        IF W_WERKS IS NOT INITIAL.
          ZIB_CONTABIL_WA-BUPLA   = W_WERKS.
        ELSE.
          ZIB_CONTABIL_WA-BUPLA   = WA_VBAP-WERKS.
        ENDIF.
        CLEAR: W_WERKS.


        IF WA_ZFIT0026-MOEDA EQ 'USD'.
          ZIB_CONTABIL_WA-KURRF          = V_TAXA_CONTABIL.
        ENDIF.
*        ZIB_CONTABIL_WA-ZTERM          = WA_ZFIT0026-ZTERM.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = WA_ZFIT0026-VBELN
          IMPORTING
            OUTPUT = ZIB_CONTABIL_WA-ZUONR.

        ZIB_CONTABIL_WA-UMSKZ          = 'L'.
        "ZIB_CONTABIL_WA-UMSKZ          = WA_ZFIT0026-RAZAO_ESPECIAL.
        ZIB_CONTABIL_WA-KOSTL          = SPACE.
        ZIB_CONTABIL_WA-AUFNR          = SPACE.
        ZIB_CONTABIL_WA-PRCTR          = SPACE.
        ZIB_CONTABIL_WA-WAERS_I        = 'BRL'.
        ZIB_CONTABIL_WA-WAERS_F        = WA_ZFIT0026-MOEDA.

        IF ( WA_ZFIT0026-MOEDA EQ 'USD' ).
          ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
          ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI.
          ZIB_CONTABIL_WA-DMBE2          = WA_ZFIT0026-MONT_MOEDA.
        ELSE.
          ZIB_CONTABIL_WA-WRBTR          = WA_ZFIT0026-MONT_MOEDA.
          ZIB_CONTABIL_WA-DMBTR          = WA_ZFIT0026-MONT_MI.
          ZIB_CONTABIL_WA-DMBE2          = SPACE.
        ENDIF.

        ZIB_CONTABIL_WA-BVTYP          = SPACE.
        ZIB_CONTABIL_WA-HBKID          = SPACE.
        ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
        ZIB_CONTABIL_WA-BANKL          = SPACE.
        ZIB_CONTABIL_WA-BANKN          = SPACE.
        ZIB_CONTABIL_WA-NEWBW          = SPACE.
        ZIB_CONTABIL_WA-ANLN1          = SPACE.
        ZIB_CONTABIL_WA-ANLN2          = SPACE.
        INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
      ENDIF.

      CLEAR: ZIB_CONTABIL_WA, SGTXT.

      DATA(X_TOTAL) = ( WA_ZFIT0026-VLR_JUROS_RBDO + WA_ZFIT0026-VLR_MULTA_RBDO ).
      IF X_TOTAL > 0."6
        CONCATENATE 'Receita de Juros sobre OV. Insumos' WA_ZFIT0026-VBELN INTO SGTXT SEPARATED BY SPACE.

        ZIB_CONTABIL_WA-OBJ_KEY     =  OBJ_KEY.
        ZIB_CONTABIL_WA-SEQITEM     = '0003'.

        "De => para centro virtual.
        SELECT SINGLE CENTRO_REAL
        FROM ZSDT_DEPARA_CEN
        INTO W_WERKS
        WHERE CENTROV_1 EQ WA_VBAP-WERKS.
        IF W_WERKS IS NOT INITIAL.
          ZIB_CONTABIL_WA-GSBER       = W_WERKS.
        ELSE.
          ZIB_CONTABIL_WA-GSBER       = WA_VBAP-WERKS.
        ENDIF.
        CLEAR: W_WERKS.

        ZIB_CONTABIL_WA-BUKRS       = WA_VBAK-BUKRS_VF.
        ZIB_CONTABIL_WA-INTERFACE   = '96'.
        ZIB_CONTABIL_WA-BKTXT = 'Recbto Juros/multa Insumos'.


        ZIB_CONTABIL_WA-BLDAT = DATA.
        ZIB_CONTABIL_WA-BUDAT = DATA.
        ZIB_CONTABIL_WA-GJAHR = ANO.
        ZIB_CONTABIL_WA-MONAT = MES.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_ZFIT0026-VBELN
          IMPORTING
            OUTPUT = ZIB_CONTABIL_WA-XBLNR.

        ZIB_CONTABIL_WA-BSCHL          = '50'.

        SELECT SINGLE * FROM KNA1
        INTO  WA_KNA1
        WHERE KUNNR EQ  WA_VBAK-KUNNR.

        ZIB_CONTABIL_WA-VBUND = WA_KNA1-VBUND.

        IF ZIB_CONTABIL_WA-VBUND EQ 'SOCIOS'.
          ZIB_CONTABIL_WA-HKONT          = '331004'.
        ELSE.
          ZIB_CONTABIL_WA-HKONT          = '331002'.
        ENDIF.
        ZIB_CONTABIL_WA-BLART       = 'ND'.

        ZIB_CONTABIL_WA-WAERS    = WA_ZFIT0026-MOEDA.
        ZIB_CONTABIL_WA-ZFBDT    = DATA.
        ZIB_CONTABIL_WA-ZLSPR    = ' '.
        ZIB_CONTABIL_WA-ZLSCH    = WA_ZFIT0026-FORMA_PAG.
        ZIB_CONTABIL_WA-KIDNO    = SPACE.
        ZIB_CONTABIL_WA-SGTXT    = SGTXT.
        ZIB_CONTABIL_WA-XREF1    = SPACE.
        ZIB_CONTABIL_WA-XREF2    = SPACE.
        ZIB_CONTABIL_WA-XREF3    = SPACE.

        "De => para centro virtual.
        SELECT SINGLE CENTRO_REAL
        FROM ZSDT_DEPARA_CEN
        INTO W_WERKS
        WHERE CENTROV_1 EQ WA_VBAP-WERKS.
        IF W_WERKS IS NOT INITIAL.
          ZIB_CONTABIL_WA-BUPLA    = W_WERKS.
        ELSE.
          ZIB_CONTABIL_WA-BUPLA    = WA_VBAP-WERKS.
        ENDIF.
        CLEAR: W_WERKS.


        IF WA_ZFIT0026-MOEDA EQ 'USD'.
          ZIB_CONTABIL_WA-KURRF     = V_TAXA_CONTABIL.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = WA_ZFIT0026-VBELN
          IMPORTING
            OUTPUT = ZIB_CONTABIL_WA-ZUONR.

*               IF P_INS IS NOT INITIAL.
*                 ZIB_CONTABIL_WA-UMSKZ = 'L'.
*               ELSEIF P_MI IS NOT INITIAL.
*                 ZIB_CONTABIL_WA-UMSKZ = 'A'.
*               ENDIF.

        ZIB_CONTABIL_WA-KOSTL   = SPACE.
        ZIB_CONTABIL_WA-AUFNR   = SPACE.
        ZIB_CONTABIL_WA-PRCTR   = SPACE.
        ZIB_CONTABIL_WA-WAERS_I = 'BRL'.
        ZIB_CONTABIL_WA-WAERS_F = WA_ZFIT0026-MOEDA.

        IF WA_ZFIT0026-MOEDA EQ 'USD'.
          ZIB_CONTABIL_WA-WRBTR = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
          ZIB_CONTABIL_WA-DMBTR = ( ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ) * WA_ZFIT0026-TAXA ).
          ZIB_CONTABIL_WA-DMBE2 = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
        ELSE.
          ZIB_CONTABIL_WA-WRBTR  = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
          ZIB_CONTABIL_WA-DMBTR  = ( WA_ZFIT0026-VLR_MULTA_RBDO + WA_ZFIT0026-VLR_JUROS_RBDO ).
          ZIB_CONTABIL_WA-DMBE2  = SPACE.
        ENDIF.

        ZIB_CONTABIL_WA-BVTYP          = SPACE.
        ZIB_CONTABIL_WA-HBKID          = SPACE.
        ZIB_CONTABIL_WA-RG_ATUALIZADO  = 'N'.
        ZIB_CONTABIL_WA-BANKL          = SPACE.
        ZIB_CONTABIL_WA-BANKN          = SPACE.
        ZIB_CONTABIL_WA-NEWBW          = SPACE.
        ZIB_CONTABIL_WA-ANLN1          = SPACE.
        ZIB_CONTABIL_WA-ANLN2          = SPACE.

        INSERT INTO ZIB_CONTABIL VALUES ZIB_CONTABIL_WA.
        IF SY-SUBRC IS INITIAL.
          COMMIT WORK.
        ELSE.
          WA_MENSAGEM =  'Lançamento não foi gerado'.
          APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
          L_ERRO = 'X'.
          ROLLBACK WORK.
        ENDIF.

        CLEAR: ZIB_CONTABIL_WA.
      ENDIF."6
      UPDATE ZFIT0026
       SET  OBJ_KEY = OBJ_KEY
       STATUS  = 'P'
      WHERE VBELN  EQ WA_ZFIT0026-VBELN
     AND SEQ    EQ WA_ZFIT0026-SEQ.

      IF SY-SUBRC IS INITIAL.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

      WAIT UP TO 75 SECONDS.

      SELECT SINGLE *
      FROM ZIB_CONTABIL_CHV
      INTO W_ZIB_CONTABIL_CHV
      WHERE OBJ_KEY EQ OBJ_KEY
      AND NOT EXISTS ( SELECT *
                         FROM BKPF
                        WHERE BELNR EQ ZIB_CONTABIL_CHV~BELNR
                          AND BUKRS EQ ZIB_CONTABIL_CHV~BUKRS
                          AND GJAHR EQ ZIB_CONTABIL_CHV~GJAHR
                          AND STBLG NE SPACE ).

      IF SY-SUBRC IS INITIAL.

        UPDATE ZFIT0026
        SET DOCNUM  = W_ZIB_CONTABIL_CHV-BELNR
        STATUS      = 'G'
        WHERE VBELN  = WA_ZFIT0026-VBELN
        AND SEQ      = WA_ZFIT0026-SEQ.

        IF SY-SUBRC IS INITIAL.
          COMMIT WORK.
          LWA_DATA_RESPONSE_DOC-DOC_CONTABIL = W_ZIB_CONTABIL_CHV-BELNR.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

      ELSE.
        SELECT  *
        FROM ZIB_CONTABIL_ERR
        INTO TABLE IT_ZIB_CONTABIL_ERR3
        WHERE OBJ_KEY EQ OBJ_KEY.

        IF SY-SUBRC IS INITIAL.
          LOOP AT IT_ZIB_CONTABIL_ERR3 INTO WA_ERRO3.
            APPEND WA_ERRO3-MESSAGE TO <FS_ERRO>-ERROS.
          ENDLOOP.

          L_ERRO = 'X'.

          UPDATE ZFIT0026
          SET STATUS  = 'E'
          WHERE VBELN    = WA_ZFIT0026-VBELN
                AND SEQ  = WA_ZFIT0026-SEQ.

          IF SY-SUBRC IS INITIAL.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF."4

    IF V_PROCESSOU NE 'X'.
      WA_MENSAGEM =  'Tempo de processamento esgotado. Tente novamente em 2 minutos.'.
      APPEND WA_MENSAGEM TO <FS_ERRO>-ERROS.
      L_ERRO = 'X'.
    ENDIF.


    IF L_ERRO IS NOT INITIAL.
      "E_SUCESSO   = ABAP_FALSE.
      "E_MSG_OUTBOUND = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DATA_RESPONSE_ERRO ).
    ELSE.
      "E_SUCESSO   = ABAP_TRUE.
      "E_MSG_OUTBOUND = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DATA_RESPONSE_DOC ).
    ENDIF.
    RETURN.

  ENDMETHOD.
ENDCLASS.
