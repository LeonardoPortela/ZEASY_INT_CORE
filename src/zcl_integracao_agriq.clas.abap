CLASS ZCL_INTEGRACAO_AGRIQ DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES ZIF_INTEGRACAO_INJECT .
    INTERFACES ZIF_INTEGRACAO_AGRIQ .

    DATA AT_JSON TYPE STRING .

    METHODS CONSTRUCTOR
      RAISING
        ZCX_INTEGRACAO .
    METHODS SET_JSON
      IMPORTING
        !P1                          TYPE STRING OPTIONAL
        !P2                          TYPE STRING OPTIONAL
      RETURNING
        VALUE(R_IF_INTEGRACAO_AGRIQ) TYPE REF TO ZIF_INTEGRACAO_AGRIQ .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_AGRIQ IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE      = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AGRIQ.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO     = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_OUTBOUND.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL          = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_SINCRONIA      = ZIF_INTEGRACAO=>AT_TP_SINCRONIA_SINCRONA.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_OPUS    = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_OPUS_NAO.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_API_AD  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_API_AD_NAO.
    ME->ZIF_INTEGRACAO_INJECT~AT_SEND_AUTENTICAO   = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_SEND_SIM.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_MODULE  = 'AGRIQ'.

  ENDMETHOD.


  METHOD SET_JSON.

    R_IF_INTEGRACAO_AGRIQ = ME.

    IF P1 = '{' OR P1 = '[' OR P1 = ']'.
      AT_JSON = AT_JSON && P1.
      EXIT.
    ENDIF.

    IF P1 = '}'.
      AT_JSON = AT_JSON && P1 && ','.
      EXIT.
    ENDIF.

    AT_JSON = AT_JSON && '"' && P1 && '"' && ':'.

    IF P2 <> '#'.
      AT_JSON = AT_JSON && '"' && P2 && '"' && ','.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_AGRIQ     = ME.
    E_REFERENCIA-TP_REFERENCIA = 'AGRIQ'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_REFERENCIA.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE.

    IF ZIF_INTEGRACAO_AGRIQ~AT_IF_INTEGRACAO_AGRIQ IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_AGRIQ~AT_IF_INTEGRACAO_AGRIQ
        TYPE ZCL_INTEGRACAO_AGRIQ.
    ENDIF.
    R_IF_INTEGRACAO_AGRIQ = ZIF_INTEGRACAO_AGRIQ~AT_IF_INTEGRACAO_AGRIQ.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~GET_METODO.
    R_IF_INTEGRACAO_AGRIQ = ME.

    CHECK ME->ZIF_INTEGRACAO_AGRIQ~AT_METODO IS NOT INITIAL.
    E_METODO = ME->ZIF_INTEGRACAO_AGRIQ~AT_METODO.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~GET_PDF_DOCUMENTO_ASSINADO.

    DATA: LO_ENVIO          TYPE REF TO ZCL_INTEGRACAO_BRY_ADIGITAL,
          LW_ENVIO          TYPE ZINS_DADOS_BRY_DADOS,
          L_RESPONSEX       TYPE XSTRING,
          L_MERGED_DOCUMENT TYPE XSTRING,
          T_PDF_FILES       TYPE ZSDT_PDF_FILES,
          W_PDF_FILES       TYPE ZSDE_PDF_FILES,
          T_TABBIN          TYPE TABLE OF CHAR80,
          T_ANEXOS          TYPE ZINS_ADIGITAL_ATTACHMENTS,
          T_BINARY_TAB      TYPE TABLE OF SDOKCNTASC.

*-------------------------------
*-- instancia atributos
*-------------------------------
    R_IF_INTEGRACAO_AGRIQ = ME.

    FREE: T_PDF_FILES.

*-------------------------------
*-- verifica se receita existe
*-------------------------------
    SELECT *
      FROM ZSDT0298
      INTO TABLE @DATA(T_0298)
     WHERE NRO_CGD       = @I_NRO_CGD
       AND CH_REFERENCIA = @I_CH_REFERENCIA
       AND CANCELADO     = @ABAP_FALSE.

    IF SY-SUBRC <> 0.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '12' ).
    ENDIF.

*-------------------------------
*-- Todos IDs Receita da Carga
*-------------------------------
    LOOP AT T_0298 INTO DATA(W_0298).

      SELECT RECEITAKEY, CHAVE_PDF_ASSINADO, CHAVE_ASSINATURA, URL_PDF_ASSINADO, DOC_PDF_ASSINADO
        INTO @DATA(W_0218)
        FROM ZSDT0218
          UP TO 1 ROWS
       WHERE RECEITAKEY = @W_0298-RECEITAKEY
         AND CANCELADA  = @ABAP_OFF.
      ENDSELECT.

*     IF sy-subrc <> 0.
*       me->zif_integracao_agriq~set_mensagem( '15' ).
*     ENDIF.

      CHECK SY-SUBRC = 0.

      CHECK W_0218-CHAVE_PDF_ASSINADO IS NOT INITIAL.
      CHECK W_0218-CHAVE_ASSINATURA   IS NOT INITIAL.

*---------------------------
*-- montar campos API
*---------------------------
*     lw_envio-metodo_envio                 = 'GET'.
*     lw_envio-endpoint_get_pdf_assinado    = w_0218-chave_pdf_assinado.
*     lw_envio-id_referencia                = w_0218-receitakey.

*-----------------------------------------------------
*-- Envio BRY - OBTER documento assinado -------------
*-----------------------------------------------------
*     TRY.
*         CREATE OBJECT lo_envio
*           EXPORTING
*             it_anexos     = t_anexos
*             i_dados_envio = lw_envio.
*
*         lo_envio->zif_integracao_bry_adigital~enviar_bry( IMPORTING e_jsonx = l_responsex ).
*
*         IF l_responsex IS INITIAL.
*           me->zif_integracao_agriq~set_mensagem( '16' ).
*         ENDIF.
*
**--------------------------
**------- montar PDF format
**--------------------------
*         w_pdf_files-tipo_doc  = '07'.
*         w_pdf_files-filename  = 'R.A. Assinadas'.
*         w_pdf_files-data      = l_responsex.
*         w_pdf_files-len       = xstrlen( l_responsex ).
*         APPEND w_pdf_files   TO t_pdf_files.
*
*       CATCH zcx_integracao INTO DATA(ex_int).
*         me->zif_integracao_agriq~set_mensagem( '16' ).
*
*       CATCH zcx_error      INTO DATA(ex_erro).
*         me->zif_integracao_agriq~set_mensagem( '16' ).
*     ENDTRY.

*-----------------------------------------------------
*-- montar campos API - relatorio assinaturas
*-----------------------------------------------------
*     CLEAR lw_envio.
*
*     lw_envio-metodo_envio                 = 'GET'.
*     lw_envio-endpoint_get_pdf_rel_assina  = w_0218-chave_assinatura.
*     lw_envio-id_referencia                = w_0218-receitakey.
*
**-----------------------------------------------------
**-- Envio BRY - OBTER relatorio assinaturas ----------
**-----------------------------------------------------
*     TRY.
*         CREATE OBJECT lo_envio
*           EXPORTING
*             it_anexos     = t_anexos
*             i_dados_envio = lw_envio.
*
*         lo_envio->zif_integracao_bry_adigital~enviar_bry( IMPORTING e_jsonx = l_responsex ).
*
*         IF l_responsex IS INITIAL.
*           me->zif_integracao_agriq~set_mensagem( '16' ).
*         ENDIF.
*
**---------------------------
**------- montar PDF format
**---------------------------
*         w_pdf_files-tipo_doc  = '08'.
*         w_pdf_files-filename  = 'Rel.Assinaturas'.
*         w_pdf_files-data      = l_responsex.
*         w_pdf_files-len       = xstrlen( l_responsex ).
*         APPEND w_pdf_files   TO t_pdf_files.
*
*       CATCH zcx_integracao INTO ex_int.
*         me->zif_integracao_agriq~set_mensagem( '16' ).
*
*       CATCH zcx_error      INTO ex_erro.
*         me->zif_integracao_agriq~set_mensagem( '16' ).
*     ENDTRY.

*---------------------------
*---- obtem PDF documento assinado AgriQ
*---------------------------
      W_PDF_FILES-TIPO_DOC  = '08'.
      W_PDF_FILES-FILENAME  = W_0218-URL_PDF_ASSINADO.

*     IF w_0218-doc_pdf_assinado IS NOT INITIAL.
*       w_pdf_files-data    = w_0218-doc_pdf_assinado.
*       w_pdf_files-len     = xstrlen( w_0218-doc_pdf_assinado ).
*     ELSE.
      TRY .
          ZCL_FATURAMENTO=>ZIF_FATURAMENTO~GET_INSTANCE(
           )->GET_DATA_URL( EXPORTING I_FILENAME   = W_0218-URL_PDF_ASSINADO
                                      I_PULA_CHECK = ABAP_FALSE
                            IMPORTING E_DATA       = W_PDF_FILES-DATA
                                      E_LEN        = W_PDF_FILES-LEN ).
        CATCH ZCX_FATURAMENTO.
        CATCH ZCX_ERROR.
      ENDTRY.
*     ENDIF.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          BUFFER        = W_PDF_FILES-DATA
        IMPORTING
          OUTPUT_LENGTH = W_PDF_FILES-LEN
        TABLES
          BINARY_TAB    = T_TABBIN.

      APPEND LINES OF T_TABBIN[] TO T_TABBIN[].

      W_PDF_FILES-LEN = W_PDF_FILES-LEN + W_PDF_FILES-LEN.

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          INPUT_LENGTH = W_PDF_FILES-LEN
        IMPORTING
          BUFFER       = W_PDF_FILES-DATA
        TABLES
          BINARY_TAB   = T_TABBIN
        EXCEPTIONS
          FAILED       = 1
          OTHERS       = 2.

      APPEND W_PDF_FILES   TO T_PDF_FILES.

    ENDLOOP.

    IF T_PDF_FILES[] IS INITIAL.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '18' ).
    ENDIF.

    T_PDF_DOCTO_ASSINADO[] = T_PDF_FILES[].

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_ATUALIZA_NFE_RECEITA.

    DATA: L_ZCX_INTEGRA TYPE CHAR1,
          L_ZCX_ERROR   TYPE CHAR1,
          L_TENTA       TYPE I.

*-------------------------------
*-- instancia atributos
*-------------------------------
    R_IF_INTEGRACAO_AGRIQ = ME.

*-#130114-20.12.2023-JT-inicio
    SELECT SINGLE MODEL
             INTO @DATA(_MODEL)
             FROM J_1BNFDOC
            WHERE DOCNUM = @I_DOCNUM.

    IF SY-SUBRC = 0 AND _MODEL = '57'.
      EXIT.
    ENDIF.
*-#130114-20.12.2023-JT-fim

*-------------------------------
*-- Busca NF
*-------------------------------
    FREE: L_TENTA.

    DO.
      L_TENTA = L_TENTA + 1.

      IF L_TENTA > 200.
        EXIT.
      ENDIF.

      SELECT *
        INTO @DATA(W_0001)
        FROM ZSDT0001
          UP TO 1 ROWS
       WHERE NRO_NF_PROD = @I_DOCNUM.
      ENDSELECT.

      IF SY-SUBRC <> 0.
        WAIT UP TO 5 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    IF SY-SUBRC <> 0.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '17' ).
    ENDIF.

*-------------------------------
*-- verifica se receita existe
*-------------------------------
    SELECT *
      FROM ZSDT0298
      INTO TABLE @DATA(T_0298)
     WHERE NRO_CGD       = @W_0001-NRO_CG
       AND CH_REFERENCIA = @W_0001-CH_REFERENCIA
       AND CANCELADO     = @ABAP_FALSE.

    IF SY-SUBRC <> 0.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '12' ).
    ENDIF.

*-------------------------------
*-- Todos IDs Receita da Carga
*-------------------------------
    LOOP AT T_0298 INTO DATA(W_0298).

*-------------------------------
*---- Setar ID Referencia
*-------------------------------
      ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_REFERENCIA  = W_0298-CH_REFERENCIA.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA = W_0298-ID.

      FREE: L_ZCX_INTEGRA, L_ZCX_ERROR.

*-------------------------------
*---- Monta JSON
*-------------------------------
      ME->ZIF_INTEGRACAO_AGRIQ~SET_JSON_ATUALIZA_NFE( I_DOCNUM ).

*-------------------------------
*---- Executa API
*-------------------------------
      TRY .
          ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
             )->SET_EXEC_AGRIQ( EXPORTING I_METODO = 'ATUALIZA_NFE' ).

        CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
          DATA(LX_INTEGRA) = EX_INTEGRA.
          L_ZCX_INTEGRA    = ABAP_TRUE.

        CATCH ZCX_ERROR INTO DATA(EX_ERROR).
          DATA(LX_ERROR)   = EX_ERROR.
          L_ZCX_ERROR      = ABAP_TRUE.
      ENDTRY.

*-------------------------------
*---- Retorno Erro
*-------------------------------
      CASE ABAP_TRUE.

        WHEN L_ZCX_INTEGRA.
          RAISE EXCEPTION TYPE ZCX_INTEGRACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = LX_INTEGRA->MSGID
                                MSGNO = LX_INTEGRA->MSGNO
                                ATTR1 = CONV #( LX_INTEGRA->MSGV1 )
                                ATTR2 = CONV #( LX_INTEGRA->MSGV2 )
                                ATTR3 = CONV #( LX_INTEGRA->MSGV3 )
                                ATTR4 = CONV #( LX_INTEGRA->MSGV4 ) )
              MSGID  = LX_INTEGRA->MSGID
              MSGNO  = LX_INTEGRA->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( LX_INTEGRA->MSGV1 )
              MSGV2  = CONV #( LX_INTEGRA->MSGV2 )
              MSGV3  = CONV #( LX_INTEGRA->MSGV3 )
              MSGV4  = CONV #( LX_INTEGRA->MSGV4 ).

        WHEN L_ZCX_ERROR.
          RAISE EXCEPTION TYPE ZCX_ERROR
            EXPORTING
              TEXTID = VALUE #( MSGID = LX_ERROR->MSGID
                                MSGNO = LX_ERROR->MSGNO
                                ATTR1 = CONV #( LX_ERROR->MSGV1 )
                                ATTR2 = CONV #( LX_ERROR->MSGV2 )
                                ATTR3 = CONV #( LX_ERROR->MSGV3 )
                                ATTR4 = CONV #( LX_ERROR->MSGV4 ) )
              MSGID  = LX_ERROR->MSGID
              MSGNO  = LX_ERROR->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( LX_ERROR->MSGV1 )
              MSGV2  = CONV #( LX_ERROR->MSGV2 )
              MSGV3  = CONV #( LX_ERROR->MSGV3 )
              MSGV4  = CONV #( LX_ERROR->MSGV4 ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_BAIXAR_RECEITAS_CONTA.

    DATA: L_DATA_STR    TYPE STRING,
          L_DATA_INI    TYPE DATUM,
          L_ZCX_INTEGRA TYPE CHAR1,
          L_ZCX_ERROR   TYPE CHAR1.

    R_IF_INTEGRACAO_AGRIQ                     = ME.

*-------------------------------
*-- data inicio busca
*-------------------------------
    L_DATA_INI = COND #( WHEN I_DATA_FIM IS INITIAL THEN I_DATA_INICIO - 5
                                                    ELSE I_DATA_INICIO - 3 ).
    ME->ZIF_INTEGRACAO_AGRIQ~SET_FORMATA_DATA( EXPORTING I_DATA = L_DATA_INI
                                               IMPORTING E_DATA = L_DATA_STR ).

*-------------------------------
*-- instancia atributos
*-------------------------------
    ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_INICIO   = L_DATA_STR.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_FIM      = I_DATA_FIM.

*-------------------------------
*---- Executa API
*-------------------------------
    TRY .
        ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
           )->SET_EXEC_AGRIQ( EXPORTING I_METODO = 'BAIXAR_RECEITAS' ).

      CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
        DATA(LX_INTEGRA) = EX_INTEGRA.
        L_ZCX_INTEGRA    = ABAP_TRUE.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).
        DATA(LX_ERROR)   = EX_ERROR.
        L_ZCX_ERROR      = ABAP_TRUE.
    ENDTRY.

*-------------------------------
*---- Retorno Erro
*-------------------------------
    CASE ABAP_TRUE.

      WHEN L_ZCX_INTEGRA.
        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = LX_INTEGRA->MSGID
                              MSGNO = LX_INTEGRA->MSGNO
                              ATTR1 = CONV #( LX_INTEGRA->MSGV1 )
                              ATTR2 = CONV #( LX_INTEGRA->MSGV2 )
                              ATTR3 = CONV #( LX_INTEGRA->MSGV3 )
                              ATTR4 = CONV #( LX_INTEGRA->MSGV4 ) )
            MSGID  = LX_INTEGRA->MSGID
            MSGNO  = LX_INTEGRA->MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( LX_INTEGRA->MSGV1 )
            MSGV2  = CONV #( LX_INTEGRA->MSGV2 )
            MSGV3  = CONV #( LX_INTEGRA->MSGV3 )
            MSGV4  = CONV #( LX_INTEGRA->MSGV4 ).

      WHEN L_ZCX_ERROR.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = LX_ERROR->MSGID
                              MSGNO = LX_ERROR->MSGNO
                              ATTR1 = CONV #( LX_ERROR->MSGV1 )
                              ATTR2 = CONV #( LX_ERROR->MSGV2 )
                              ATTR3 = CONV #( LX_ERROR->MSGV3 )
                              ATTR4 = CONV #( LX_ERROR->MSGV4 ) )
            MSGID  = LX_ERROR->MSGID
            MSGNO  = LX_ERROR->MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( LX_ERROR->MSGV1 )
            MSGV2  = CONV #( LX_ERROR->MSGV2 )
            MSGV3  = CONV #( LX_ERROR->MSGV3 )
            MSGV4  = CONV #( LX_ERROR->MSGV4 ).

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_CANCELAR_SOL_RA.

    DATA: L_ZCX_INTEGRA     TYPE CHAR1,
          L_ZCX_ERROR       TYPE CHAR1,
          L_CANCELA_RECEITA TYPE CHAR1,
          L_ERRO            TYPE CHAR1,
          L_QUANT           TYPE I,
          L_LINES           TYPE I.

*-------------------------------
*-- instancia atributos
*-------------------------------
    R_IF_INTEGRACAO_AGRIQ                     = ME.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD       = I_NRO_CGD.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA = I_CH_REFERENCIA.

*-------------------------------
*-- verifica se Solicitacao existe
*-------------------------------
    SELECT *
      FROM ZSDT0298
      INTO TABLE @DATA(T_0298)
     WHERE NRO_CGD       = @I_NRO_CGD
       AND CH_REFERENCIA = @I_CH_REFERENCIA
       AND CANCELADO     = @ABAP_FALSE.

    IF SY-SUBRC <> 0.
*     me->zif_integracao_agriq~set_elimina_zsdt0302( ).
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '12' ).
    ENDIF.

*-------------------------------
*-- atualizar Statua das RAs
*-- consulta solicitacao receitas
*-------------------------------
    TRY.
        ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
           )->SET_CONSULTAR_SOL_RA( EXPORTING I_NRO_CGD       = I_NRO_CGD
                                              I_CH_REFERENCIA = I_CH_REFERENCIA ).

      CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
        L_ERRO = ABAP_TRUE.
      CATCH ZCX_ERROR INTO DATA(EX_ERROR).
        L_ERRO = ABAP_TRUE.
    ENDTRY.

    IF L_ERRO = ABAP_TRUE.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '10' ).
    ENDIF.

*-------------------------------
*-- recupera status atualizado
*-------------------------------
    SELECT *
      FROM ZSDT0298
      INTO TABLE T_0298
     WHERE NRO_CGD       = I_NRO_CGD
       AND CH_REFERENCIA = I_CH_REFERENCIA
       AND CANCELADO     = ABAP_FALSE.

*-------------------------------
*-- eliminar os ja cancelados
*-------------------------------
    DELETE T_0298 WHERE STATUS = 3.

    IF T_0298[] IS INITIAL.
*     me->zif_integracao_agriq~set_elimina_zsdt0302( ).
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '12' ).
    ENDIF.
*CS2021000218 -r ajustes na mont. carga - BG #109923 - inicio
*-------------------------------
*-- verifica se ja há algum documento assinado
*-------------------------------
*    LOOP AT t_0298 INTO DATA(w_0298).
*      SELECT chave_pdf_assinado
*        FROM zsdt0218
*        INTO @DATA(l_chave_pdf_assinado)
*          UP TO 1 ROWS
*       WHERE receitakey = @w_0298-receitakey
*         AND cancelada  = @abap_off.
*      ENDSELECT.
*
*      IF sy-subrc = 0 AND l_chave_pdf_assinado IS NOT INITIAL.
*        me->zif_integracao_agriq~set_mensagem( '19' ).
*      ENDIF.
*    ENDLOOP.


*-------------------------------
*-- verifica se ja tem NF vinculada
*-------------------------------
    LOOP AT T_0298 INTO DATA(W_0298).

      SELECT SINGLE * FROM ZSDT0001 INTO @DATA(W_ZSDT0001)
        WHERE CH_REFERENCIA EQ @W_0298-CH_REFERENCIA.

      IF SY-SUBRC IS INITIAL AND W_ZSDT0001-FATURA_PROD IS NOT INITIAL.
        IF W_ZSDT0001-FATURA_PROD IS NOT INITIAL.
          SELECT SINGLE * FROM J_1BNFLIN
            INTO @DATA(W_J_1BNFLIN)
            WHERE REFKEY EQ @W_ZSDT0001-FATURA_PROD.

          IF SY-SUBRC IS INITIAL.
            SELECT SINGLE * FROM ZSDT0212 INTO @DATA(W_ZSDT0212)
              WHERE DOCNUM EQ @W_J_1BNFLIN-DOCNUM.

            IF SY-SUBRC IS INITIAL AND W_ZSDT0212-ID_INDEA IS NOT INITIAL.
              ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '20' ).
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
*CS2021000218 -r ajustes na mont. carga - BG #109923 - Fim
*---------------------------------------------------------------
*-- Cancelar solicitacao receitas ------------------------------
*---------------------------------------------------------------
    LOOP AT T_0298 INTO W_0298 WHERE STATUS = '0'
                                  OR STATUS = '1'
                                  OR STATUS = '2'.

*-------------------------------
*---- Setar ID Referencia
*-------------------------------
      ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_REFERENCIA  = W_0298-CH_REFERENCIA.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD        = W_0298-NRO_CGD.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA  = W_0298-CH_REFERENCIA.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA = W_0298-ID.

      FREE: L_ZCX_INTEGRA, L_ZCX_ERROR.

*-------------------------------
*---- Executa API
*-------------------------------
      TRY .
          ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
             )->SET_EXEC_AGRIQ( EXPORTING I_METODO = 'CANCELAR_RA' ).

        CATCH ZCX_INTEGRACAO INTO EX_INTEGRA.
          DATA(LX_INTEGRA) = EX_INTEGRA.
          L_ZCX_INTEGRA    = ABAP_TRUE.

        CATCH ZCX_ERROR INTO EX_ERROR.
          DATA(LX_ERROR)   = EX_ERROR.
          L_ZCX_ERROR      = ABAP_TRUE.
      ENDTRY.

*-------------------------------
*---- Retorno Erro
*-------------------------------
      CASE ABAP_TRUE.

        WHEN L_ZCX_INTEGRA.
          RAISE EXCEPTION TYPE ZCX_INTEGRACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = LX_INTEGRA->MSGID
                                MSGNO = LX_INTEGRA->MSGNO
                                ATTR1 = CONV #( LX_INTEGRA->MSGV1 )
                                ATTR2 = CONV #( LX_INTEGRA->MSGV2 )
                                ATTR3 = CONV #( LX_INTEGRA->MSGV3 )
                                ATTR4 = CONV #( LX_INTEGRA->MSGV4 ) )
              MSGID  = LX_INTEGRA->MSGID
              MSGNO  = LX_INTEGRA->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( LX_INTEGRA->MSGV1 )
              MSGV2  = CONV #( LX_INTEGRA->MSGV2 )
              MSGV3  = CONV #( LX_INTEGRA->MSGV3 )
              MSGV4  = CONV #( LX_INTEGRA->MSGV4 ).

        WHEN L_ZCX_ERROR.
          RAISE EXCEPTION TYPE ZCX_ERROR
            EXPORTING
              TEXTID = VALUE #( MSGID = LX_ERROR->MSGID
                                MSGNO = LX_ERROR->MSGNO
                                ATTR1 = CONV #( LX_ERROR->MSGV1 )
                                ATTR2 = CONV #( LX_ERROR->MSGV2 )
                                ATTR3 = CONV #( LX_ERROR->MSGV3 )
                                ATTR4 = CONV #( LX_ERROR->MSGV4 ) )
              MSGID  = LX_ERROR->MSGID
              MSGNO  = LX_ERROR->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( LX_ERROR->MSGV1 )
              MSGV2  = CONV #( LX_ERROR->MSGV2 )
              MSGV3  = CONV #( LX_ERROR->MSGV3 )
              MSGV4  = CONV #( LX_ERROR->MSGV4 ).

      ENDCASE.
    ENDLOOP.

*---------------------------------------------------------------
*-- Cancelar RECEITA -------------------------------------------
*---------------------------------------------------------------
    LOOP AT T_0298 INTO W_0298  WHERE STATUS = '4'.

*-------------------------------
*---- Setar ID Referencia
*-------------------------------
      ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_REFERENCIA  = W_0298-CH_REFERENCIA.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD        = W_0298-NRO_CGD.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA  = W_0298-CH_REFERENCIA.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA = W_0298-ID.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_RECEITAKEY     = W_0298-RECEITAKEY.

      FREE: L_ZCX_INTEGRA, L_ZCX_ERROR.

*-------------------------------
*---- Executa API
*-------------------------------
      TRY .
          ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
             )->SET_EXEC_AGRIQ( EXPORTING I_METODO = 'CANCELAR_RECEITA' ).

        CATCH ZCX_INTEGRACAO INTO EX_INTEGRA.
          LX_INTEGRA       = EX_INTEGRA.
          L_ZCX_INTEGRA    = ABAP_TRUE.

        CATCH ZCX_ERROR INTO EX_ERROR.
          LX_ERROR         = EX_ERROR.
          L_ZCX_ERROR      = ABAP_TRUE.
      ENDTRY.

*-------------------------------
*---- Retorno Erro
*-------------------------------
      CASE ABAP_TRUE.

        WHEN L_ZCX_INTEGRA.
          RAISE EXCEPTION TYPE ZCX_INTEGRACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = LX_INTEGRA->MSGID
                                MSGNO = LX_INTEGRA->MSGNO
                                ATTR1 = CONV #( LX_INTEGRA->MSGV1 )
                                ATTR2 = CONV #( LX_INTEGRA->MSGV2 )
                                ATTR3 = CONV #( LX_INTEGRA->MSGV3 )
                                ATTR4 = CONV #( LX_INTEGRA->MSGV4 ) )
              MSGID  = LX_INTEGRA->MSGID
              MSGNO  = LX_INTEGRA->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( LX_INTEGRA->MSGV1 )
              MSGV2  = CONV #( LX_INTEGRA->MSGV2 )
              MSGV3  = CONV #( LX_INTEGRA->MSGV3 )
              MSGV4  = CONV #( LX_INTEGRA->MSGV4 ).

        WHEN L_ZCX_ERROR.
          RAISE EXCEPTION TYPE ZCX_ERROR
            EXPORTING
              TEXTID = VALUE #( MSGID = LX_ERROR->MSGID
                                MSGNO = LX_ERROR->MSGNO
                                ATTR1 = CONV #( LX_ERROR->MSGV1 )
                                ATTR2 = CONV #( LX_ERROR->MSGV2 )
                                ATTR3 = CONV #( LX_ERROR->MSGV3 )
                                ATTR4 = CONV #( LX_ERROR->MSGV4 ) )
              MSGID  = LX_ERROR->MSGID
              MSGNO  = LX_ERROR->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( LX_ERROR->MSGV1 )
              MSGV2  = CONV #( LX_ERROR->MSGV2 )
              MSGV3  = CONV #( LX_ERROR->MSGV3 )
              MSGV4  = CONV #( LX_ERROR->MSGV4 ).

      ENDCASE.
    ENDLOOP.

*-------------------------------
*-- Ajustar tabela ZSDT0302
*-------------------------------
    IF I_ELIMINA_REG = ABAP_TRUE.
      SELECT *
        FROM ZSDT0298
        INTO TABLE T_0298
       WHERE NRO_CGD       = I_NRO_CGD
         AND CH_REFERENCIA = I_CH_REFERENCIA
         AND CANCELADO     = ABAP_FALSE.

      IF T_0298[] IS INITIAL.
        ME->ZIF_INTEGRACAO_AGRIQ~SET_ELIMINA_ZSDT0302( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_CONSULTAR_RECEITA_SIAGRI.

    DATA: L_DATA_STR    TYPE STRING,
          L_DATA_INI    TYPE STRING,
          L_DATA_FIM    TYPE STRING,
          L_DATA_AUX1   TYPE DATUM,
          L_DATA_AUX2   TYPE DATUM,
          L_ZCX_INTEGRA TYPE CHAR1,
          L_ZCX_ERROR   TYPE CHAR1.

    R_IF_INTEGRACAO_AGRIQ                     = ME.

    FREE: E_RECEITAS.

*-------------------------------
*-- data inicio busca
*-------------------------------
    IF I_DATA_INICIO IS NOT INITIAL AND I_DATA_FIM IS NOT INITIAL.
      L_DATA_INI = I_DATA_INICIO+0(4) && '-' && I_DATA_INICIO+4(2) && '-' && I_DATA_INICIO+6(2).
      L_DATA_FIM = I_DATA_FIM+0(4)    && '-' && I_DATA_FIM+4(2)    && '-' && I_DATA_FIM+6(2).
    ELSE.
      L_DATA_AUX1 = SY-DATUM - 15.
      L_DATA_AUX2 = SY-DATUM.
      L_DATA_INI = L_DATA_AUX1+0(4)   && '-' && L_DATA_AUX1+4(2)   && '-' && L_DATA_AUX1+6(2).
      L_DATA_FIM = L_DATA_AUX2+0(4)   && '-' && L_DATA_AUX2+4(2)   && '-' && L_DATA_AUX2+6(2).
    ENDIF.

*-------------------------------
*-- instancia atributos
*-------------------------------
    ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_INICIO   = L_DATA_INI.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_FINAL    = L_DATA_FIM.

*-------------------------------
*---- Executa API
*-------------------------------
    TRY .
        ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
           )->SET_EXEC_SIAGRI( EXPORTING I_METODO   = 'GET_SIAGRI_RECEITA'
                               IMPORTING E_RECEITAS = E_RECEITAS ).

      CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
        DATA(LX_INTEGRA) = EX_INTEGRA.
        L_ZCX_INTEGRA    = ABAP_TRUE.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).
        DATA(LX_ERROR)   = EX_ERROR.
        L_ZCX_ERROR      = ABAP_TRUE.
    ENDTRY.

*-------------------------------
*---- Retorno Erro
*-------------------------------
    CASE ABAP_TRUE.

      WHEN L_ZCX_INTEGRA.
        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = LX_INTEGRA->MSGID
                              MSGNO = LX_INTEGRA->MSGNO
                              ATTR1 = CONV #( LX_INTEGRA->MSGV1 )
                              ATTR2 = CONV #( LX_INTEGRA->MSGV2 )
                              ATTR3 = CONV #( LX_INTEGRA->MSGV3 )
                              ATTR4 = CONV #( LX_INTEGRA->MSGV4 ) )
            MSGID  = LX_INTEGRA->MSGID
            MSGNO  = LX_INTEGRA->MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( LX_INTEGRA->MSGV1 )
            MSGV2  = CONV #( LX_INTEGRA->MSGV2 )
            MSGV3  = CONV #( LX_INTEGRA->MSGV3 )
            MSGV4  = CONV #( LX_INTEGRA->MSGV4 ).

      WHEN L_ZCX_ERROR.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = LX_ERROR->MSGID
                              MSGNO = LX_ERROR->MSGNO
                              ATTR1 = CONV #( LX_ERROR->MSGV1 )
                              ATTR2 = CONV #( LX_ERROR->MSGV2 )
                              ATTR3 = CONV #( LX_ERROR->MSGV3 )
                              ATTR4 = CONV #( LX_ERROR->MSGV4 ) )
            MSGID  = LX_ERROR->MSGID
            MSGNO  = LX_ERROR->MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( LX_ERROR->MSGV1 )
            MSGV2  = CONV #( LX_ERROR->MSGV2 )
            MSGV3  = CONV #( LX_ERROR->MSGV3 )
            MSGV4  = CONV #( LX_ERROR->MSGV4 ).

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_CONSULTAR_RTC_SIAGRI.

    DATA: L_ZCX_INTEGRA TYPE CHAR1,
          L_ZCX_ERROR   TYPE CHAR1.

    R_IF_INTEGRACAO_AGRIQ                     = ME.

    FREE: E_RTC.

*-------------------------------
*-- instancia atributos
*-------------------------------
    ME->ZIF_INTEGRACAO_AGRIQ~AT_ATIVO         = I_ATIVO.

*-------------------------------
*---- Executa API
*-------------------------------
    TRY .
        ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
           )->SET_EXEC_SIAGRI( EXPORTING I_METODO   = 'GET_SIAGRI_RTC'
                               IMPORTING E_RTC      = E_RTC ).

      CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
        DATA(LX_INTEGRA) = EX_INTEGRA.
        L_ZCX_INTEGRA    = ABAP_TRUE.

      CATCH ZCX_ERROR INTO DATA(EX_ERROR).
        DATA(LX_ERROR)   = EX_ERROR.
        L_ZCX_ERROR      = ABAP_TRUE.
    ENDTRY.

*-------------------------------
*---- Retorno Erro
*-------------------------------
    CASE ABAP_TRUE.

      WHEN L_ZCX_INTEGRA.
        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = LX_INTEGRA->MSGID
                              MSGNO = LX_INTEGRA->MSGNO
                              ATTR1 = CONV #( LX_INTEGRA->MSGV1 )
                              ATTR2 = CONV #( LX_INTEGRA->MSGV2 )
                              ATTR3 = CONV #( LX_INTEGRA->MSGV3 )
                              ATTR4 = CONV #( LX_INTEGRA->MSGV4 ) )
            MSGID  = LX_INTEGRA->MSGID
            MSGNO  = LX_INTEGRA->MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( LX_INTEGRA->MSGV1 )
            MSGV2  = CONV #( LX_INTEGRA->MSGV2 )
            MSGV3  = CONV #( LX_INTEGRA->MSGV3 )
            MSGV4  = CONV #( LX_INTEGRA->MSGV4 ).

      WHEN L_ZCX_ERROR.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = LX_ERROR->MSGID
                              MSGNO = LX_ERROR->MSGNO
                              ATTR1 = CONV #( LX_ERROR->MSGV1 )
                              ATTR2 = CONV #( LX_ERROR->MSGV2 )
                              ATTR3 = CONV #( LX_ERROR->MSGV3 )
                              ATTR4 = CONV #( LX_ERROR->MSGV4 ) )
            MSGID  = LX_ERROR->MSGID
            MSGNO  = LX_ERROR->MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( LX_ERROR->MSGV1 )
            MSGV2  = CONV #( LX_ERROR->MSGV2 )
            MSGV3  = CONV #( LX_ERROR->MSGV3 )
            MSGV4  = CONV #( LX_ERROR->MSGV4 ).

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_CONSULTAR_SOL_RA.

    DATA: L_ZCX_INTEGRA TYPE CHAR1,
          L_ZCX_ERROR   TYPE CHAR1.

*-------------------------------
*-- instancia atributos
*-------------------------------
    R_IF_INTEGRACAO_AGRIQ = ME.

*-------------------------------
*-- verifica se Solicitacao ewxiste
*-------------------------------
    SELECT *
      FROM ZSDT0298
      INTO TABLE @DATA(T_0298)
     WHERE NRO_CGD       = @I_NRO_CGD
       AND CH_REFERENCIA = @I_CH_REFERENCIA
       AND CANCELADO     = @ABAP_FALSE.

    IF SY-SUBRC <> 0.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '12' ).
    ENDIF.

*-------------------------------
*-- Todos IDs Receita da Carga
*-------------------------------
    LOOP AT T_0298 INTO DATA(W_0298).

*-------------------------------
*---- Setar ID Referencia
*-------------------------------
      ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_REFERENCIA  = W_0298-CH_REFERENCIA.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD        = W_0298-NRO_CGD.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA  = W_0298-CH_REFERENCIA.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA = W_0298-ID.

      FREE: L_ZCX_INTEGRA, L_ZCX_ERROR.

*-------------------------------
*---- Executa API
*-------------------------------
      TRY .
          ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
             )->SET_EXEC_AGRIQ( EXPORTING I_METODO = 'CONSULTAR_RA' ).

        CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
          DATA(LX_INTEGRA) = EX_INTEGRA.
          L_ZCX_INTEGRA    = ABAP_TRUE.

        CATCH ZCX_ERROR INTO DATA(EX_ERROR).
          DATA(LX_ERROR)   = EX_ERROR.
          L_ZCX_ERROR      = ABAP_TRUE.
      ENDTRY.

*-------------------------------
*---- Retorno Erro
*-------------------------------
      CASE ABAP_TRUE.

        WHEN L_ZCX_INTEGRA.
          RAISE EXCEPTION TYPE ZCX_INTEGRACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = LX_INTEGRA->MSGID
                                MSGNO = LX_INTEGRA->MSGNO
                                ATTR1 = CONV #( LX_INTEGRA->MSGV1 )
                                ATTR2 = CONV #( LX_INTEGRA->MSGV2 )
                                ATTR3 = CONV #( LX_INTEGRA->MSGV3 )
                                ATTR4 = CONV #( LX_INTEGRA->MSGV4 ) )
              MSGID  = LX_INTEGRA->MSGID
              MSGNO  = LX_INTEGRA->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( LX_INTEGRA->MSGV1 )
              MSGV2  = CONV #( LX_INTEGRA->MSGV2 )
              MSGV3  = CONV #( LX_INTEGRA->MSGV3 )
              MSGV4  = CONV #( LX_INTEGRA->MSGV4 ).

        WHEN L_ZCX_ERROR.
          RAISE EXCEPTION TYPE ZCX_ERROR
            EXPORTING
              TEXTID = VALUE #( MSGID = LX_ERROR->MSGID
                                MSGNO = LX_ERROR->MSGNO
                                ATTR1 = CONV #( LX_ERROR->MSGV1 )
                                ATTR2 = CONV #( LX_ERROR->MSGV2 )
                                ATTR3 = CONV #( LX_ERROR->MSGV3 )
                                ATTR4 = CONV #( LX_ERROR->MSGV4 ) )
              MSGID  = LX_ERROR->MSGID
              MSGNO  = LX_ERROR->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( LX_ERROR->MSGV1 )
              MSGV2  = CONV #( LX_ERROR->MSGV2 )
              MSGV3  = CONV #( LX_ERROR->MSGV3 )
              MSGV4  = CONV #( LX_ERROR->MSGV4 ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_DS_DATA.

    R_IF_INTEGRACAO_AGRIQ = ME.

*---------------------------------------
*---types
*---------------------------------------
    TYPES BEGIN OF TY_RETORNO.
    TYPES: ACCESS_TOKEN TYPE STRING.
    TYPES: EXPIRES_IN TYPE STRING.
    TYPES: TOKEN_TYPE TYPE STRING.
    TYPES END OF TY_RETORNO.

*---------------------------------------
*---workarea
*---------------------------------------
    DATA: L_ACCESS_TOKEN TYPE STRING,
          L_TOKEN_TYPE   TYPE STRING,
          L_EXPIRES_IN   TYPE STRING,
          L_TOKEN        TYPE STRING,
          LC_RETORNO     TYPE TY_RETORNO.

    FREE: ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

    CASE ME->ZIF_INTEGRACAO_AGRIQ~AT_METODO.
      WHEN 'TOKEN'.
        APPEND VALUE #( NAME = 'User'             VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-USERNAME ) TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Password'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-PASSWORD ) TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-CONTENT_TYPE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'POST'.

      WHEN 'TOKEN_SIAGRI'.
        SELECT SINGLE *
          FROM ZAUTH_WS_0001
          INTO @DATA(WA_ZAUTH_WS_0001)
         WHERE SERVICE = 'SIAGRI_TOKEN'.

        APPEND VALUE #( HEADER_FIELD = 'content-disposition' HEADER_VALUE = 'form-data;name=client_id'      VALUE = WA_ZAUTH_WS_0001-USERNAME ) TO ME->ZIF_INTEGRACAO_AGRIQ~AT_MULTIPART.
        APPEND VALUE #( HEADER_FIELD = 'content-disposition' HEADER_VALUE = 'form-data;name=client_secret'  VALUE = WA_ZAUTH_WS_0001-ADD01 )    TO ME->ZIF_INTEGRACAO_AGRIQ~AT_MULTIPART.
        APPEND VALUE #( HEADER_FIELD = 'content-disposition' HEADER_VALUE = 'form-data;name=scope'          VALUE = 'apiV2' )                   TO ME->ZIF_INTEGRACAO_AGRIQ~AT_MULTIPART.
        APPEND VALUE #( HEADER_FIELD = 'content-disposition' HEADER_VALUE = 'form-data;name=grant_type'     VALUE = 'client_credentials' )      TO ME->ZIF_INTEGRACAO_AGRIQ~AT_MULTIPART.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = 'multipart/form-data'.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'POST'.

      WHEN 'GET_ALL_PROD' OR 'GET_MAPA'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-CONTENT_TYPE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'GET'.

      WHEN 'GERAR_RA'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-CONTENT_TYPE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY         = ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'POST'.

      WHEN 'CONSULTAR_RA' OR 'LER_PDF'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-CONTENT_TYPE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'GET'.

      WHEN 'CANCELAR_RA'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-CONTENT_TYPE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'POST'.

      WHEN 'CANCELAR_RECEITA'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-CONTENT_TYPE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'POST'.

      WHEN 'ATUALIZA_NFE'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-CONTENT_TYPE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY         = ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'PUT'.

      WHEN 'BAIXAR_RECEITAS'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-CONTENT_TYPE.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY         = ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'GET'.

      WHEN 'GET_SIAGRI_RECEITA'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = 'application/json'.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY         = ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'GET'.

      WHEN 'GET_SIAGRI_RTC'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INTEGRACAO-DS_DATA_RETORNO CHANGING DATA = LC_RETORNO ).

        L_ACCESS_TOKEN = LC_RETORNO-ACCESS_TOKEN.
        L_TOKEN_TYPE   = LC_RETORNO-TOKEN_TYPE.
        L_EXPIRES_IN   = LC_RETORNO-EXPIRES_IN.
        L_TOKEN        = |{ L_TOKEN_TYPE } { L_ACCESS_TOKEN }|.

        APPEND VALUE #( NAME = 'Authorization'    VALUE = L_TOKEN )                                         TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'Subscription-Key' VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD02 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
        APPEND VALUE #( NAME = 'X-Tenant'         VALUE = ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE-ADD01 )    TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE = 'application/json'.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY         = ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON.
        ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO       = 'GET'.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_DS_DATA_AGRIQ.

    DATA: I_INBOUND1      TYPE ZDE_API_RECEITA_AGRIQ,
          I_INBOUND2      TYPE ZINC_BRY_IN_COLETA,
          I_INBOUND3      TYPE ZDE_API_AGRIQ_ASSINADA,
          T_ELEMENT_ARRAY	TYPE ZDE_ELEMENT_ARRAY_T.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_AGRIQ = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO     = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_INBOUND.

    "Verificar a Função de Cada requisição
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = I_FUNCAO_PROCESSA.

    CASE ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA.
      WHEN 'servico_receita_agriq'.
        ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AGRIQ.
      WHEN 'servico_receita_agriq_assinada'.
        ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AGRIQ.
      WHEN 'bry_set_coleta_finalizada'.
        ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE  = '062'.
    ENDCASE.

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

        CASE ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA.
          WHEN 'servico_receita_agriq'.
            /UI2/CL_JSON=>DESERIALIZE(
              EXPORTING
                JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_INFO-DS_BODY I_ELEMENT_ARRAY = T_ELEMENT_ARRAY )
              CHANGING
                DATA = I_INBOUND1
            ).

          WHEN 'servico_receita_agriq_assinada'.
            /UI2/CL_JSON=>DESERIALIZE(
              EXPORTING
                JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_INFO-DS_BODY I_ELEMENT_ARRAY = T_ELEMENT_ARRAY )
              CHANGING
                DATA = I_INBOUND3
            ).

          WHEN 'bry_set_coleta_finalizada'.
            /UI2/CL_JSON=>DESERIALIZE(
              EXPORTING
                JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_INFO-DS_BODY I_ELEMENT_ARRAY = T_ELEMENT_ARRAY )
              CHANGING
                DATA = I_INBOUND2
            ).
        ENDCASE.

      WHEN 'JSON'.
        CASE ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA.
          WHEN 'servico_receita_agriq'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = I_INBOUND1 ).
          WHEN 'servico_receita_agriq_assinada'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = I_INBOUND3 ).
          WHEN 'bry_set_coleta_finalizada'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = I_INBOUND2 ).
        ENDCASE.

    ENDCASE.

    CASE ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA.
      WHEN 'servico_receita_agriq'.
        ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = I_INBOUND1-KEY.
      WHEN 'servico_receita_agriq_assinada'.
        ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = I_INBOUND3-ID.
      WHEN 'bry_set_coleta_finalizada'.
        ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = I_INBOUND2-CHAVE.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_DS_URL.

    R_IF_INTEGRACAO_AGRIQ = ME.

    DATA: L_URL     TYPE STRING,
          L_DATA    TYPE STRING,
          L_PAGE    TYPE STRING,
          L_ATIVO   TYPE STRING,
          L_SERVICO TYPE STRING.

    CASE I_METODO.
      WHEN 'TOKEN'.
        L_SERVICO = 'AGRIQ_TOKEN'.
      WHEN 'TOKEN_SIAGRI'.
        L_SERVICO = 'SIAGRI_TOKEN'.
      WHEN 'GET_ALL_PROD' OR 'GET_MAPA'.
        L_SERVICO = 'AGRIQ_PRODUTOS'.
      WHEN 'GERAR_RA'.
        L_SERVICO = 'AGRIQ_GERAR_SOL_RECEITA'.
      WHEN 'CONSULTAR_RA'.
        L_SERVICO = 'AGRIQ_CONSULTAR_SOL_RECEITA'.
      WHEN 'CANCELAR_RA'.
        L_SERVICO = 'AGRIQ_CANCELAR_SOL_RECEITA'.
      WHEN 'CANCELAR_RECEITA'.
        L_SERVICO = 'AGRIQ_CANCELAR_RECEITA'.
      WHEN 'LER_PDF'.
        L_SERVICO = 'AGRIQ_BAIXAR_PDF_BASE64'.
      WHEN 'ATUALIZA_NFE'.
        L_SERVICO = 'AGRIQ_ATUALIZAR_RECEITA'.
      WHEN 'BAIXAR_RECEITAS'.
        L_SERVICO = 'AGRIQ_BAIXAR_RECEITAS_POR_CONTA'.
      WHEN 'GET_SIAGRI_RECEITA'.
        L_SERVICO = 'SIAGRI_RECEITAS'.
      WHEN 'GET_SIAGRI_RTC'.
        L_SERVICO = 'AGRIQ_BAIXAR_RTC_POR_CONTA'.
    ENDCASE.

    SELECT SINGLE *
             FROM ZAUTH_WEBSERVICE
             INTO @DATA(WA_WEBSERVICE)
            WHERE SERVICE = @L_SERVICO.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
                            ATTR1 = 'T'
                            ATTR2 = 'TC' )
          MSGID  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
          MSGTY  = 'E'
          MSGV1  = 'T'
          MSGV2  = 'TC'.
    ENDIF.

    ME->ZIF_INTEGRACAO_AGRIQ~AT_WEBSERVICE = WA_WEBSERVICE.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_METODO     = I_METODO.

    CASE I_METODO.
      WHEN 'TOKEN'.
        L_URL = WA_WEBSERVICE-URL_TOKEN.

      WHEN 'TOKEN_SIAGRI'.
        L_URL = WA_WEBSERVICE-URL.

      WHEN 'GET_ALL_PROD'.
        IF I_DATA IS INITIAL.
          L_DATA = '1900-01-01T01:00:00.00'.
        ELSE.
          L_DATA = I_DATA(4) && '-' && I_DATA+4(2) && '-' && I_DATA+6(2) && 'T01:00:00.00'.
        ENDIF.

        L_PAGE = '&Page=' && ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE.

        IF I_ATIVO = ABAP_TRUE.
          L_ATIVO = '&Ativo=true'.
        ENDIF.

        L_URL = WA_WEBSERVICE-URL && '?Date='      && L_DATA && L_ATIVO && L_PAGE.

      WHEN 'GET_MAPA'.
        IF I_ATIVO = ABAP_TRUE.
          L_ATIVO = '&Ativo=true'.
        ENDIF.

        L_URL = WA_WEBSERVICE-URL && '?CodigoMapa=' && I_CODMAPA && L_ATIVO.

      WHEN 'GERAR_RA' OR 'ATUALIZA_NFE'.
        L_URL = WA_WEBSERVICE-URL.

      WHEN 'CONSULTAR_RA' OR 'CANCELAR_RA'.
        L_URL = WA_WEBSERVICE-URL && '?id='         && ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA.

      WHEN 'CANCELAR_RECEITA' OR 'LER_PDF'.
        L_URL = WA_WEBSERVICE-URL && '?receitaKey=' && ME->ZIF_INTEGRACAO_AGRIQ~AT_RECEITAKEY.

      WHEN 'BAIXAR_RECEITAS'.
        L_PAGE = ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE.
        L_URL  = WA_WEBSERVICE-URL && '?Date='        && ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_INICIO &&
                                      '&contaID='     && WA_WEBSERVICE-USERNAME &&
                                      '&page='        && L_PAGE                 &&
                                      '&size='        && '1000'.

      WHEN 'GET_SIAGRI_RECEITA'.
        L_URL  = WA_WEBSERVICE-URL && '?dataInicial=' && ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_INICIO
                                   && '&dataFinal='   && ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_FINAL.

      WHEN 'GET_SIAGRI_RTC'.
        IF     ME->ZIF_INTEGRACAO_AGRIQ~AT_ATIVO IS INITIAL.
          L_URL = WA_WEBSERVICE-URL.
        ELSEIF ME->ZIF_INTEGRACAO_AGRIQ~AT_ATIVO = 'S'.
          L_URL = WA_WEBSERVICE-URL && '?Ativo=true'.
        ELSEIF ME->ZIF_INTEGRACAO_AGRIQ~AT_ATIVO = 'N'.
          L_URL = WA_WEBSERVICE-URL && '?Ativo=false'.
        ENDIF.
    ENDCASE.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO          = 'JSON'.
*   me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN        = WA_WEBSERVICE-URL_TOKEN.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL              = L_URL.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ABAP_OFF.
    ME->ZIF_INTEGRACAO_AGRIQ~SET_ID_REFERENCIA( ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_ELIMINA_ZSDT0302.

    R_IF_INTEGRACAO_AGRIQ = ME.

    DELETE FROM ZSDT0302 WHERE NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
                           AND CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA.

    COMMIT WORK AND WAIT. " STEFANINI - 2000016980 - IR193546 - 10.09.2024

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_ENVIAR_ASSINATURA.

    DATA: LO_OBJECT     TYPE REF TO ZCL_AGRIQ_DOCUMENTS_STEP_01,
          T_RANGE       TYPE ZINC_REF_RANGE,
          W_RANGE       TYPE ZINS_REF_RANGE,
          L_ZCX_INTEGRA TYPE CHAR1,
          L_ZCX_ERROR   TYPE CHAR1.

    FREE: T_RANGE.

*-------------------------------
*-- instancia atributos
*-------------------------------
    R_IF_INTEGRACAO_AGRIQ = ME.

*-------------------------------
*-- verifica se receita existe
*-------------------------------
    SELECT SINGLE *
      FROM ZSDT0218
      INTO @DATA(W_0218)
     WHERE RECEITAKEY = @I_RECEITAKEY
       AND CANCELADA  = @ABAP_OFF.

    IF SY-SUBRC <> 0.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '15' ).
    ENDIF.

*-------------------------------
*-- atualiza status ZSDT0298
*-------------------------------
    SELECT SINGLE *
      FROM ZSDT0298
      INTO @DATA(W_0298)
     WHERE RECEITAKEY    = @I_RECEITAKEY.

    IF SY-SUBRC <> 0.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '15' ).
    ENDIF.

    TRY.
        ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
           )->SET_CONSULTAR_SOL_RA( EXPORTING I_NRO_CGD       = W_0298-NRO_CGD
                                              I_CH_REFERENCIA = W_0298-CH_REFERENCIA ).

      CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
      CATCH ZCX_ERROR INTO DATA(EX_ERROR).
    ENDTRY.

*-------------------------------
*-- monta range
*-------------------------------
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_0218-receitakey ) TO t_range.

*-------------------------------
*-- Executa API Bry
*-------------------------------
*    TRY.
*        CREATE OBJECT lo_object.
*        lo_object->process_contracts( t_range[] ).
*
*      CATCH zcx_integracao INTO ex_integra.
*        DATA(lx_integra) = ex_integra.
*        l_zcx_integra    = abap_true.
*
*      CATCH zcx_error      INTO ex_error.
*        DATA(lx_error)   = ex_error.
*        l_zcx_error      = abap_true.
*    ENDTRY.

*-------------------------------
*-- Retorno Erro
*-------------------------------
*    CASE abap_true.
*
*      WHEN l_zcx_integra.
*        RAISE EXCEPTION TYPE zcx_integracao
*          EXPORTING
*            textid = VALUE #( msgid = lx_integra->msgid
*                              msgno = lx_integra->msgno
*                              attr1 = CONV #( lx_integra->msgv1 )
*                              attr2 = CONV #( lx_integra->msgv2 )
*                              attr3 = CONV #( lx_integra->msgv3 )
*                              attr4 = CONV #( lx_integra->msgv4 ) )
*            msgid  = lx_integra->msgid
*            msgno  = lx_integra->msgno
*            msgty  = 'E'
*            msgv1  = CONV #( lx_integra->msgv1 )
*            msgv2  = CONV #( lx_integra->msgv2 )
*            msgv3  = CONV #( lx_integra->msgv3 )
*            msgv4  = CONV #( lx_integra->msgv4 ).
*
*      WHEN l_zcx_error.
*        RAISE EXCEPTION TYPE zcx_error
*          EXPORTING
*            textid = VALUE #( msgid = lx_error->msgid
*                              msgno = lx_error->msgno
*                              attr1 = CONV #( lx_error->msgv1 )
*                              attr2 = CONV #( lx_error->msgv2 )
*                              attr3 = CONV #( lx_error->msgv3 )
*                              attr4 = CONV #( lx_error->msgv4 ) )
*            msgid  = lx_error->msgid
*            msgno  = lx_error->msgno
*            msgty  = 'E'
*            msgv1  = CONV #( lx_error->msgv1 )
*            msgv2  = CONV #( lx_error->msgv2 )
*            msgv3  = CONV #( lx_error->msgv3 )
*            msgv4  = CONV #( lx_error->msgv4 ).
*
*    ENDCASE.
*
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_EXEC_AGRIQ.

*---------------------------------------
*---types
*---------------------------------------
    TYPES: BEGIN OF TY_METODO,
             METODO TYPE STRING.
    TYPES: END   OF TY_METODO.

*---------------------------------------
*---workarea
*---------------------------------------
    DATA: LC_INTEGRACAO  TYPE ZINTEGRACAO,
          LC_TABPROD     TYPE ZSDE0015,
          LC_SOLRA       TYPE ZSDE0016,
          LC_CONSRA      TYPE ZSDE0017,
          LC_DATA_PDF    TYPE ZSDE0018,
          LC_DADOS_COMPL TYPE ZSDE0019,
          T_TABPROD      TYPE ZSDS068_T,
          T_ZSDT0299     TYPE TABLE OF ZSDT0299,
          W_TABPROD      TYPE ZSDS068,
          W_DADOS_COMPL  TYPE ZSDS075,
          W_ITENS        TYPE ZSDS076,
          W_ZSDT0298     TYPE ZSDT0298,
          W_ZSDT0299     TYPE ZSDT0299,
          W_ZSDT0218     TYPE ZSDT0218,
          T_ZSDT0219     TYPE TABLE OF ZSDT0219,
          W_ZSDT0219     TYPE ZSDT0219,
          W_ZSDT0132     TYPE ZSDT0132,
          W_ZSDT0134     TYPE ZSDT0134,
          L_CODIGOMAPA   TYPE ZSDT0219-CODIGOMAPA,
          L_DATA_EMISSAO TYPE DATUM,
          L_PRODUTOID    TYPE ZSDT0299-PRODUTOID,
          L_ERROR        TYPE C,
          L_TABIX        TYPE SY-TABIX,
          L_PAGS_TOT     TYPE I,
          T_QUANT        TYPE TABLE OF RGSB4,
          W_QUANT        TYPE RGSB4,

          T_METODO       TYPE TABLE OF TY_METODO,
          W_METODO       TYPE TY_METODO.

*---------------------------------------
*---inicio processo
*---------------------------------------
    R_IF_INTEGRACAO_AGRIQ = ME.

    FREE: T_METODO,
          LC_INTEGRACAO,
          L_ERROR,
          ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS,
          ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP,
          ME->ZIF_INTEGRACAO_AGRIQ~AT_PDF_RECEITA_AGRIQ,
          ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE,
          ME->ZIF_INTEGRACAO_AGRIQ~AT_MULTIPART,
          E_PDF_RECEITA_AGRIQ.

    ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE           = 0.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_OUTBOUND.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_RECEITAKEY     = COND #( WHEN I_RECEITAKEY IS NOT INITIAL
                                                              THEN I_RECEITAKEY
                                                              ELSE ME->ZIF_INTEGRACAO_AGRIQ~AT_RECEITAKEY ).

*---------------------------------------
*---Quantidade de paginas a consultar produtos
*---------------------------------------
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        CLASS         = '0000'
        SETNR         = 'ZSD_PAGES_PRODS_AGRIQ'
      TABLES
        SET_VALUES    = T_QUANT
      EXCEPTIONS
        SET_NOT_FOUND = 1
        OTHERS        = 2.

    READ TABLE T_QUANT INTO W_QUANT INDEX 1.
    L_PAGS_TOT = W_QUANT-FROM.

*---------------------------------------
*---metodos chamada
*---------------------------------------
    DO.

      FREE: T_METODO.

      W_METODO-METODO   = 'TOKEN'.
      APPEND W_METODO  TO T_METODO.
      W_METODO-METODO   = I_METODO.
      APPEND W_METODO  TO T_METODO.
*---------------------------------------

*---------------------------------------
*---buscar token / metodo
*---------------------------------------
      LOOP AT T_METODO INTO W_METODO.

        L_TABIX = SY-TABIX.

        IF W_METODO-METODO = 'GET_ALL_PROD' OR
           W_METODO-METODO = 'BAIXAR_RECEITAS'.
          ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE = ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE + 1.
        ENDIF.

        TRY.

            ME->ZIF_INTEGRACAO_AGRIQ~SET_DS_URL(
                               EXPORTING I_METODO        = W_METODO-METODO
                                         I_ATIVO         = I_ATIVO
                                         I_DATA          = I_DATA
                                         I_CODMAPA       = I_CODMAPA
              )->SET_DS_DATA(  EXPORTING I_INTEGRACAO    = LC_INTEGRACAO
              )->SET_SEND_MSG( IMPORTING E_ID_INTEGRACAO = DATA(LC_ID_INTEGRACAO)
                                         E_INTEGRACAO	   = LC_INTEGRACAO
              ).

          CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
            L_ERROR = ABAP_TRUE.

            RAISE EXCEPTION TYPE ZCX_INTEGRACAO
              EXPORTING
                TEXTID = VALUE #( MSGID = EX_INTEGRA->MSGID
                                  MSGNO = EX_INTEGRA->MSGNO
                                  ATTR1 = CONV #( EX_INTEGRA->MSGV1 )
                                  ATTR2 = CONV #( EX_INTEGRA->MSGV2 )
                                  ATTR3 = CONV #( EX_INTEGRA->MSGV3 )
                                  ATTR4 = CONV #( EX_INTEGRA->MSGV4 ) )
                MSGID  = EX_INTEGRA->MSGID
                MSGNO  = EX_INTEGRA->MSGNO
                MSGTY  = 'E'
                MSGV1  = CONV #( EX_INTEGRA->MSGV1 )
                MSGV2  = CONV #( EX_INTEGRA->MSGV2 )
                MSGV3  = CONV #( EX_INTEGRA->MSGV3 )
                MSGV4  = CONV #( EX_INTEGRA->MSGV4 ).

          CATCH ZCX_ERROR      INTO DATA(EX_ERROR).    "  "
            L_ERROR = ABAP_TRUE.

            RAISE EXCEPTION TYPE ZCX_ERROR
              EXPORTING
                TEXTID = VALUE #( MSGID = EX_ERROR->MSGID
                                  MSGNO = EX_ERROR->MSGNO
                                  ATTR1 = CONV #( EX_ERROR->MSGV1 )
                                  ATTR2 = CONV #( EX_ERROR->MSGV2 )
                                  ATTR3 = CONV #( EX_ERROR->MSGV3 )
                                  ATTR4 = CONV #( EX_ERROR->MSGV4 ) )
                MSGID  = EX_ERROR->MSGID
                MSGNO  = EX_ERROR->MSGNO
                MSGTY  = 'E'
                MSGV1  = CONV #( EX_ERROR->MSGV1 )
                MSGV2  = CONV #( EX_ERROR->MSGV2 )
                MSGV3  = CONV #( EX_ERROR->MSGV3 )
                MSGV4  = CONV #( EX_ERROR->MSGV4 ).
        ENDTRY.

        CHECK L_ERROR = ABAP_FALSE.

*---------------------------------------
*---avalia retorno JSON
*---------------------------------------
        CASE W_METODO-METODO.

          WHEN 'GET_ALL_PROD' OR 'GET_MAPA'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                       CHANGING  DATA = LC_TABPROD ).

            T_TABPROD[] = LC_TABPROD-DATA[].

*         IF t_tabprod[] IS INITIAL.
*           RAISE EXCEPTION TYPE zcx_error
*             EXPORTING
*               textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
*                                 msgno = zcx_error=>zcx_erro_geral-msgno
*                                 attr1 = CONV #( 'Não encontrado Produto no portal AgriQ.' ) )
*               msgid  = zcx_error=>zcx_erro_geral-msgid
*               msgno  = zcx_error=>zcx_erro_geral-msgno
*               msgty  = 'E'
*               msgv1  = CONV #( 'Não encontrado Produto no portal AgriQ.' ).
*         ENDIF.

*----------------------------
* ----- atualiza tabela
*----------------------------
            LOOP AT T_TABPROD             INTO W_TABPROD.
              MOVE-CORRESPONDING W_TABPROD  TO W_ZSDT0299.
              MOVE SY-MANDT                 TO W_ZSDT0299-MANDT.
              MOVE SY-UNAME                 TO W_ZSDT0299-USER_DOWNLOAD.
              MOVE SY-DATUM                 TO W_ZSDT0299-DATA_DOWNLOAD.
              MOVE SY-UZEIT                 TO W_ZSDT0299-HORA_DOWNLOAD.
              MODIFY ZSDT0299             FROM W_ZSDT0299.
            ENDLOOP.

            COMMIT WORK AND WAIT.

          WHEN 'GERAR_RA'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                       CHANGING  DATA = LC_SOLRA ).
*----------------------------
* ----- atualiza tabela
*----------------------------
            TRANSLATE LC_SOLRA-DATA-RECEITAKEY TO UPPER CASE.

            CLEAR W_ZSDT0298.
            W_ZSDT0298-MANDT           = SY-MANDT.
            W_ZSDT0298-NRO_CGD         = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD.
            W_ZSDT0298-CH_REFERENCIA   = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA.
            W_ZSDT0298-ID              = LC_SOLRA-DATA-ID.
            W_ZSDT0298-RECEITAKEY      = LC_SOLRA-DATA-RECEITAKEY.
            W_ZSDT0298-RECEITAID       = ABAP_OFF.
            W_ZSDT0298-STATUS          = LC_SOLRA-DATA-STATUS.
            W_ZSDT0298-TIPO_ASSINATURA = ME->ZIF_INTEGRACAO_AGRIQ~AT_TIPO_ASSINATURA.
            W_ZSDT0298-USNAM           = SY-UNAME.
            W_ZSDT0298-DATA_ATUAL      = SY-DATUM.
            W_ZSDT0298-HORA_ATUAL      = SY-UZEIT.
            MODIFY ZSDT0298       FROM W_ZSDT0298.

            COMMIT WORK AND WAIT.

          WHEN 'CONSULTAR_RA'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                       CHANGING  DATA = LC_CONSRA ).
*----------------------------
* ----- atualiza tabela
*----------------------------
            CLEAR W_ZSDT0298.

            SELECT SINGLE *
              FROM ZSDT0298
              INTO W_ZSDT0298
             WHERE NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
               AND CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA
               AND ID            = ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA.

            IF SY-SUBRC <> 0.
              W_ZSDT0298-USNAM       = SY-UNAME.
              W_ZSDT0298-DATA_ATUAL  = SY-DATUM.
              W_ZSDT0298-HORA_ATUAL  = SY-UZEIT.
            ENDIF.

            TRANSLATE LC_CONSRA-RECEITAKEY TO UPPER CASE.

            W_ZSDT0298-MANDT         = SY-MANDT.
            W_ZSDT0298-NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD.
            W_ZSDT0298-CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA.
            W_ZSDT0298-ID            = ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA.
            W_ZSDT0298-RECEITAKEY    = LC_CONSRA-RECEITAKEY.
            W_ZSDT0298-RECEITAID     = LC_CONSRA-RECEITAID.
            W_ZSDT0298-STATUS        = LC_CONSRA-STATUS.

            IF W_ZSDT0298-STATUS = 3 AND W_ZSDT0298-CANCELADO = ABAP_FALSE.
              W_ZSDT0298-CANCELADO   = ABAP_TRUE.
              W_ZSDT0298-USER_CANC   = SY-UNAME.
              W_ZSDT0298-DT_CANC     = SY-DATUM.
              W_ZSDT0298-HR_CANC     = SY-UZEIT.
            ENDIF.

            MODIFY ZSDT0298       FROM W_ZSDT0298.

            COMMIT WORK AND WAIT.

          WHEN 'CANCELAR_RA'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                       CHANGING  DATA = LC_SOLRA ).
*----------------------------
* ----- atualiza tabela
*----------------------------
            CLEAR W_ZSDT0298.

            SELECT SINGLE *
              FROM ZSDT0298
              INTO W_ZSDT0298
             WHERE NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
               AND CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA
               AND ID            = ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA.

            IF SY-SUBRC <> 0.
              W_ZSDT0298-RECEITAKEY  = LC_SOLRA-DATA-RECEITAKEY.
              W_ZSDT0298-USNAM       = SY-UNAME.
              W_ZSDT0298-DATA_ATUAL  = SY-DATUM.
              W_ZSDT0298-HORA_ATUAL  = SY-UZEIT.
            ENDIF.

            W_ZSDT0298-MANDT         = SY-MANDT.
            W_ZSDT0298-NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD.
            W_ZSDT0298-CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA.
            W_ZSDT0298-ID            = ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA.
            W_ZSDT0298-STATUS        = LC_SOLRA-DATA-STATUS.
            W_ZSDT0298-CANCELADO     = ABAP_TRUE.
            W_ZSDT0298-USER_CANC     = SY-UNAME.
            W_ZSDT0298-DT_CANC       = SY-DATUM.
            W_ZSDT0298-HR_CANC       = SY-UZEIT.
            MODIFY ZSDT0298       FROM W_ZSDT0298.

            COMMIT WORK AND WAIT.

          WHEN 'CANCELAR_RECEITA'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                       CHANGING  DATA = LC_SOLRA ).

*----------------------------
* ----- atualiza tabela
*----------------------------
            CLEAR W_ZSDT0298.

            SELECT SINGLE *
              FROM ZSDT0298
              INTO W_ZSDT0298
             WHERE NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
               AND CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA
               AND ID            = ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA.

            IF SY-SUBRC <> 0.
              W_ZSDT0298-RECEITAKEY  = LC_SOLRA-DATA-RECEITAKEY.
              W_ZSDT0298-USNAM       = SY-UNAME.
              W_ZSDT0298-DATA_ATUAL  = SY-DATUM.
              W_ZSDT0298-HORA_ATUAL  = SY-UZEIT.
            ENDIF.

            W_ZSDT0298-MANDT         = SY-MANDT.
            W_ZSDT0298-NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD.
            W_ZSDT0298-CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA.
            W_ZSDT0298-ID            = ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA.
            W_ZSDT0298-STATUS        = '3'. "Cancelado
            W_ZSDT0298-CANCELADO     = ABAP_TRUE.
            W_ZSDT0298-USER_CANC     = SY-UNAME.
            W_ZSDT0298-DT_CANC       = SY-DATUM.
            W_ZSDT0298-HR_CANC       = SY-UZEIT.
            MODIFY ZSDT0298       FROM W_ZSDT0298.

            COMMIT WORK AND WAIT.

          WHEN 'LER_PDF'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                       CHANGING  DATA = LC_DATA_PDF ).

            CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
              EXPORTING
                INPUT  = LC_DATA_PDF-DATA
              IMPORTING
                OUTPUT = E_PDF_RECEITA_AGRIQ.

          WHEN 'BAIXAR_RECEITAS'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                       CHANGING  DATA = LC_DADOS_COMPL ).

*----------------------------
* ----- atualiza tabela
*----------------------------
            LOOP AT LC_DADOS_COMPL-DATA INTO W_DADOS_COMPL.

              CLEAR: W_ZSDT0218, W_ZSDT0298, W_ZSDT0132, W_ZSDT0134.

              TRANSLATE W_DADOS_COMPL-RECEITAKEY TO UPPER CASE.
              CONDENSE  W_DADOS_COMPL-RECEITAKEY.

              IF ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_FIM IS NOT INITIAL.
                L_DATA_EMISSAO = W_DADOS_COMPL-DATAEMISSAO(4) && W_DADOS_COMPL-DATAEMISSAO+5(2) && W_DADOS_COMPL-DATAEMISSAO+8(2).
                IF L_DATA_EMISSAO > ME->ZIF_INTEGRACAO_AGRIQ~AT_DATA_FIM.
                  CONTINUE.
                ENDIF.
              ENDIF.

              SELECT *
                FROM ZSDT0298
                INTO W_ZSDT0298
                  UP TO 1 ROWS
               WHERE RECEITAKEY = W_DADOS_COMPL-RECEITAKEY.
              ENDSELECT.

              SELECT *
                FROM ZSDT0134
                INTO W_ZSDT0134
                  UP TO 1 ROWS
               WHERE NRO_CG        = W_ZSDT0298-NRO_CGD.
              ENDSELECT.

              SELECT *
                FROM ZSDT0132
                INTO W_ZSDT0132
                  UP TO 1 ROWS
               WHERE NR_ROT        = W_ZSDT0134-NR_ROT.
              ENDSELECT.

              SELECT *
                FROM ZSDT0218
                INTO W_ZSDT0218
                  UP TO 1 ROWS
               WHERE RECEITAKEY = W_DADOS_COMPL-RECEITAKEY
                 AND CANCELADA  = ABAP_FALSE.
              ENDSELECT.

              IF SY-SUBRC = 0.
                W_ZSDT0218-CODIGOINDEAPROPRIEDADE   = W_ZSDT0132-ID_PROPRIEDADE. "w_dados_compl-codpropriedade.
                W_ZSDT0218-NUMERONF                 = W_DADOS_COMPL-NUMERONOTAFISCAL.
                W_ZSDT0218-CREA                     = W_DADOS_COMPL-VISTOCREA.
                W_ZSDT0218-USNAM                    = SY-UNAME.
                W_ZSDT0218-DATA_ATUAL               = SY-DATUM.
                W_ZSDT0218-HORA_ATUAL               = SY-UZEIT.
                MODIFY ZSDT0218                  FROM W_ZSDT0218.
              ENDIF.

              SELECT *
                FROM ZSDT0219
                INTO TABLE T_ZSDT0219
               WHERE RECEITAKEY = W_DADOS_COMPL-RECEITAKEY.

              IF T_ZSDT0219[] IS NOT INITIAL.

                LOOP AT W_DADOS_COMPL-ITENS  INTO W_ITENS.

                  CLEAR L_PRODUTOID.

                  SELECT PRODUTOID
***Stefanini - IR234210 - 15/04/2025 - GGARAUJO1 - Início de alteração
***                  INTO l_produtoid
                    INTO TABLE @DATA(LT_PRODUTOID)
                    FROM ZSDT0299
***                    UP TO 1 ROWS
                   WHERE REGISTROMINISTERIO = @W_ITENS-CODPRODUTOINDEA.
***                ENDSELECT.

                  LOOP AT LT_PRODUTOID INTO DATA(LS_PRODUTOID).
* Correção Atualização de Dados do Codigo MAPA Projeto Renova Insumos 08/07/2025 WBARBOSA
                    L_PRODUTOID = LS_PRODUTOID.

                    LOOP AT T_ZSDT0219 INTO W_ZSDT0219 WHERE PRODUTO_ID_AGRIQ = LS_PRODUTOID-PRODUTOID.
                      L_CODIGOMAPA        = W_ZSDT0219-CODIGOMAPA.

                      UPDATE ZSDT0219 SET CODIGOMAPA               = W_ITENS-CODPRODUTOINDEA
                                          CODIGOINDEACULTURA       = W_ITENS-CODCULTURA
                                          CODIGOINDEAPRAGA         = W_ITENS-CODPRAGA
                                          CODIGOINDEATIPOAPLICACAO = W_ITENS-CODTIPOAPLICACAO
                                          CODIGOINDEAUNIDADEMEDIDA = W_ITENS-CODUNIDADEMEDIDA
                                          USNAM                    = SY-UNAME
                                          DATA_ATUAL               = SY-DATUM
                                          HORA_ATUAL               = SY-UZEIT
                                    WHERE NUMERORECEITA            = W_ZSDT0219-NUMERORECEITA
                                      AND NUMEROPEDIDO             = W_ZSDT0219-NUMEROPEDIDO
                                      AND CPFRT                    = W_ZSDT0219-CPFRT
                                      AND CODIGOMAPA               = L_CODIGOMAPA
                                      AND RECEITAKEY               = W_ZSDT0219-RECEITAKEY
                                      AND PRODUTO_ID_AGRIQ         = L_PRODUTOID.

                    ENDLOOP.
                  ENDLOOP.
***Stefanini - IR234210 - 15/04/2025 - GGARAUJO1 - Fim de alteração
                ENDLOOP.
              ENDIF.
            ENDLOOP.

            COMMIT WORK AND WAIT.
        ENDCASE.

      ENDLOOP.

      IF     W_METODO-METODO = 'GET_ALL_PROD'.
        IF ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE > L_PAGS_TOT OR LC_TABPROD-DATA[]     IS INITIAL.
          EXIT.
        ENDIF.
      ELSEIF W_METODO-METODO = 'BAIXAR_RECEITAS'.
        IF ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE > L_PAGS_TOT OR LC_DADOS_COMPL-DATA[] IS INITIAL.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_EXEC_SIAGRI.

*---------------------------------------
*---types
*---------------------------------------
    TYPES: BEGIN OF TY_METODO,
             METODO TYPE STRING.
    TYPES: END   OF TY_METODO.

*---------------------------------------
*---workarea
*---------------------------------------
    DATA: LC_INTEGRACAO  TYPE ZINTEGRACAO,
          LC_TABSIAGRI   TYPE ZSDE0070,
          LC_SOLRA       TYPE ZSDE0016,
          LC_CONSRA      TYPE ZSDE0017,
          LC_DATA_PDF    TYPE ZSDE0018,
          LC_DADOS_COMPL TYPE ZSDE0019,
          T_TABPROD      TYPE ZSDS068_T,
          T_ZSDT0299     TYPE TABLE OF ZSDT0299,
          W_TABPROD      TYPE ZSDS068,
          W_DADOS_COMPL  TYPE ZSDS075,
          W_ITENS        TYPE ZSDS076,
          W_ZSDT0298     TYPE ZSDT0298,
          W_ZSDT0299     TYPE ZSDT0299,
          W_ZSDT0218     TYPE ZSDT0218,
          T_ZSDT0219     TYPE TABLE OF ZSDT0219,
          W_ZSDT0219     TYPE ZSDT0219,
          W_ZSDT0132     TYPE ZSDT0132,
          W_ZSDT0134     TYPE ZSDT0134,
          L_CODIGOMAPA   TYPE ZSDT0219-CODIGOMAPA,
          L_DATA_EMISSAO TYPE DATUM,
          L_PRODUTOID    TYPE ZSDT0299-PRODUTOID,
          L_ERROR        TYPE C,
          L_TABIX        TYPE SY-TABIX,
          L_PAGS_TOT     TYPE I,
          T_QUANT        TYPE TABLE OF RGSB4,
          W_QUANT        TYPE RGSB4,
*
          T_METODO       TYPE TABLE OF TY_METODO,
          W_METODO       TYPE TY_METODO.

*---------------------------------------
*---inicio processo
*---------------------------------------
    R_IF_INTEGRACAO_AGRIQ = ME.

    FREE: T_METODO,
          E_RECEITAS,
          LC_INTEGRACAO,
          L_ERROR,
          ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS,
          ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP,
          ME->ZIF_INTEGRACAO_AGRIQ~AT_PDF_RECEITA_AGRIQ,
          ME->ZIF_INTEGRACAO_AGRIQ~AT_PAGE,
          ME->ZIF_INTEGRACAO_AGRIQ~AT_MULTIPART.

    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_OUTBOUND.

*---------------------------------------
*---metodos chamada
*---------------------------------------
    FREE: T_METODO.

    W_METODO-METODO   = COND #( WHEN I_METODO = 'GET_SIAGRI_RTC' THEN 'TOKEN'
                                                                 ELSE 'TOKEN_SIAGRI' ).
    APPEND W_METODO  TO T_METODO.
    W_METODO-METODO   = I_METODO.
    APPEND W_METODO  TO T_METODO.
*---------------------------------------

*---------------------------------------
*---buscar token / metodo
*---------------------------------------
    LOOP AT T_METODO INTO W_METODO.

      TRY.
          ME->ZIF_INTEGRACAO_AGRIQ~SET_DS_URL(
                             EXPORTING I_METODO        = W_METODO-METODO
            )->SET_DS_DATA(  EXPORTING I_INTEGRACAO    = LC_INTEGRACAO
            )->SET_SEND_MSG( IMPORTING E_ID_INTEGRACAO = DATA(LC_ID_INTEGRACAO)
                                       E_INTEGRACAO	   = LC_INTEGRACAO
            ).

        CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
          L_ERROR = ABAP_TRUE.

          RAISE EXCEPTION TYPE ZCX_INTEGRACAO
            EXPORTING
              TEXTID = VALUE #( MSGID = EX_INTEGRA->MSGID
                                MSGNO = EX_INTEGRA->MSGNO
                                ATTR1 = CONV #( EX_INTEGRA->MSGV1 )
                                ATTR2 = CONV #( EX_INTEGRA->MSGV2 )
                                ATTR3 = CONV #( EX_INTEGRA->MSGV3 )
                                ATTR4 = CONV #( EX_INTEGRA->MSGV4 ) )
              MSGID  = EX_INTEGRA->MSGID
              MSGNO  = EX_INTEGRA->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( EX_INTEGRA->MSGV1 )
              MSGV2  = CONV #( EX_INTEGRA->MSGV2 )
              MSGV3  = CONV #( EX_INTEGRA->MSGV3 )
              MSGV4  = CONV #( EX_INTEGRA->MSGV4 ).

        CATCH ZCX_ERROR      INTO DATA(EX_ERROR).    "  "
          L_ERROR = ABAP_TRUE.

          RAISE EXCEPTION TYPE ZCX_ERROR
            EXPORTING
              TEXTID = VALUE #( MSGID = EX_ERROR->MSGID
                                MSGNO = EX_ERROR->MSGNO
                                ATTR1 = CONV #( EX_ERROR->MSGV1 )
                                ATTR2 = CONV #( EX_ERROR->MSGV2 )
                                ATTR3 = CONV #( EX_ERROR->MSGV3 )
                                ATTR4 = CONV #( EX_ERROR->MSGV4 ) )
              MSGID  = EX_ERROR->MSGID
              MSGNO  = EX_ERROR->MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( EX_ERROR->MSGV1 )
              MSGV2  = CONV #( EX_ERROR->MSGV2 )
              MSGV3  = CONV #( EX_ERROR->MSGV3 )
              MSGV4  = CONV #( EX_ERROR->MSGV4 ).
      ENDTRY.

      CHECK L_ERROR = ABAP_FALSE.

*---------------------------------------
*---avalia retorno JSON
*---------------------------------------
      CASE W_METODO-METODO.
        WHEN 'GET_SIAGRI_RECEITA'.
          /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                     CHANGING  DATA = E_RECEITAS ).

        WHEN 'GET_SIAGRI_RTC'.
          /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = LC_INTEGRACAO-DS_DATA_RETORNO
                                     CHANGING  DATA = E_RTC ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_FORMATA_DATA.

    R_IF_INTEGRACAO_AGRIQ = ME.

    DATA L_DATA  TYPE STRING.

    L_DATA = '&1-&2-&3T&4:&5:&6-04:00'.

    REPLACE '&1' IN L_DATA WITH I_DATA(4).
    REPLACE '&2' IN L_DATA WITH I_DATA+4(2).
    REPLACE '&3' IN L_DATA WITH I_DATA+6(2).
    REPLACE '&4' IN L_DATA WITH SY-UZEIT(2).
    REPLACE '&5' IN L_DATA WITH I_DATA+2(2).
    REPLACE '&6' IN L_DATA WITH I_DATA+4(2).

    CONDENSE L_DATA NO-GAPS.

    E_DATA = L_DATA.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_GERAR_SOL_RA.

*-------------------------------
*-- Types
*-------------------------------
    TYPES: BEGIN OF TY_GRP_MAT,
             MATNR TYPE MATNR,
             GRP   TYPE NUMC3.
    TYPES: END OF TY_GRP_MAT.

*-------------------------------
*-- Locais
*-------------------------------
    DATA: W_ZSDT0302      TYPE ZSDT0302,
          T_0001_ITEM     TYPE TABLE OF ZSDT0001_ITEM,
          T_0001_ITEM_GRP TYPE TABLE OF ZSDT0001_ITEM,
          T_LISTA         TYPE TABLE OF SPOPLI,
          T_VALUE         TYPE TABLE OF RGSB4,
          T_GRP_MAT       TYPE TABLE OF TY_GRP_MAT,
          W_GRP_MAT       TYPE TY_GRP_MAT,
          T_GRP_MAT_AUX   TYPE TABLE OF TY_GRP_MAT,
          W_GRP_MAT_AUX   TYPE TY_GRP_MAT,
          L_SET_MAX_MAT   TYPE NUMC3,
          L_QUANT_MAT     TYPE P DECIMALS 5,
          L_QUANT         TYPE NUMC3,
          L_IND           TYPE NUMC3,
          L_GRP           TYPE NUMC3,
          L_MATNR         TYPE MATNR,
          L_AGRIQ_ANT     TYPE ZSDT0210-ID_MATNR_AGRIQ,
          L_MATNR_ANT     TYPE ZSDT0210-MATNR,
          L_ERRO          TYPE CHAR1.

    FREE: T_VALUE, T_LISTA.

*-------------------------------
*-- instancia atributos
*-------------------------------
    R_IF_INTEGRACAO_AGRIQ                     = ME.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD       = I_NRO_CGD.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA = I_CH_REFERENCIA.
    ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_REFERENCIA = I_CH_REFERENCIA.

*-------------------------------
*-- verifica se CArga ja tem RA emitida
*-------------------------------
    SELECT SINGLE *
      FROM ZSDT0298
      INTO @DATA(W_0298)
     WHERE NRO_CGD       = @ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
       AND CH_REFERENCIA = @ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA
       AND CANCELADO     = @ABAP_FALSE.

    IF SY-SUBRC = 0.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '11' ).
    ENDIF.

*-------------------------------
*-- Cabecalho carga insumos
*-------------------------------
    SELECT SINGLE TIPO_RTC, CPF_RTC
      FROM ZSDT0139
      INTO @DATA(W_0139)
     WHERE NRO_CGD = @I_NRO_CGD.

    IF SY-SUBRC <> 0.
      ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '01' ).
    ENDIF.

    SELECT SINGLE ASS_ELETRONICA
      FROM ZSDT0259
      INTO @DATA(L_ASS_ELETRONICA)
     WHERE CPF = @W_0139-CPF_RTC.

    IF SY-SUBRC = 0.
      IF L_ASS_ELETRONICA = ABAP_OFF.
        ME->ZIF_INTEGRACAO_AGRIQ~AT_TIPO_ASSINATURA = '0'.  "Assinatura Manual
      ELSE.
        ME->ZIF_INTEGRACAO_AGRIQ~AT_TIPO_ASSINATURA = '1'.  "Assinatura Eletrônica
      ENDIF.
    ELSE.
      ME->ZIF_INTEGRACAO_AGRIQ~AT_TIPO_ASSINATURA   = '0'.
    ENDIF.
*-------------------------------
*-- tipo RTC
*-------------------------------
    CASE W_0139-TIPO_RTC.
      WHEN 'T'.
        ME->ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302( I_GERA_RA = ABAP_OFF I_QTD_RA = 0 ).
        ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '02' ).

      WHEN 'P'.
        SELECT SINGLE *
          INTO @DATA(W_0134)
          FROM ZSDT0134
         WHERE NRO_CG        = @I_NRO_CGD
           AND CH_REFERENCIA = @I_CH_REFERENCIA.

        IF SY-SUBRC <> 0.
          ME->ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302( I_GERA_RA = ABAP_ON I_QTD_RA = 0 ).
          ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '03' ).
        ENDIF.

        SELECT SINGLE *
          INTO @DATA(W_VBAK)
          FROM VBAK
         WHERE VBELN = @W_0134-VBELN.

        IF SY-SUBRC <> 0.
          ME->ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302( I_GERA_RA = ABAP_ON I_QTD_RA = 0 ).
          ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '04' ).
        ENDIF.

        SELECT SINGLE *
          INTO @DATA(W_0216)
          FROM ZSDT0216
         WHERE BUKRS = @W_VBAK-VKORG
           AND KUNNR = @W_VBAK-KUNNR
           AND SETOR_ATIVIDADE = 'A' "Agrotoxico
           AND TIPO            = 'C' "Cliente
           AND REVENDA         = 'S'.

        IF SY-SUBRC = 0.  "Cliente e revenda
          ME->ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302( I_GERA_RA = ABAP_OFF I_QTD_RA = 0 ).
          ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '05' ).
        ENDIF.

*-------------------------------
*------ itens carga
*-------------------------------
        SELECT ZSDT0001_ITEM~*
          INTO TABLE @T_0001_ITEM
          FROM ZSDT0001_ITEM
         INNER JOIN MARA ON MARA~MATNR     = ZSDT0001_ITEM~MATNR
         WHERE ZSDT0001_ITEM~CH_REFERENCIA = @I_CH_REFERENCIA
           AND MARA~IHIVI                  = @ABAP_FALSE.

        IF T_0001_ITEM[] IS INITIAL.
          ME->ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302( I_GERA_RA = ABAP_OFF I_QTD_RA = 0 ).
          ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '06' ).
        ENDIF.

*-------------------------------
*------ ID Agriq
*-------------------------------
        SELECT *
          INTO TABLE @DATA(T_0210)
          FROM ZSDT0210
           FOR ALL ENTRIES IN @T_0001_ITEM
         WHERE MATNR  = @T_0001_ITEM-MATNR.

        LOOP AT T_0001_ITEM INTO DATA(W_0001_ITEM).
          READ TABLE T_0210 INTO DATA(W_0210) WITH KEY MATNR = W_0001_ITEM-MATNR.
          IF SY-SUBRC <> 0 OR W_0210-ID_MATNR_AGRIQ IS INITIAL.
            L_MATNR = W_0001_ITEM-MATNR.
            SHIFT L_MATNR LEFT DELETING LEADING '0'.
            APPEND VALUE #( SELFLAG = ABAP_OFF VAROPTION = L_MATNR INACTIVE = ABAP_OFF ) TO T_LISTA.
          ENDIF.
        ENDLOOP.

*-------------------------------
*------ Lista com materiais sem DE/PARA AgriQ
*-------------------------------
        IF T_LISTA[] IS NOT INITIAL.
          SORT T_LISTA BY VAROPTION.
          DELETE ADJACENT DUPLICATES FROM T_LISTA
                                COMPARING VAROPTION.

          IF I_EXIBE_POPUP = ABAP_TRUE.
*           CALL FUNCTION 'ISH_POPUP_TO_DECIDE_LIST'
            CALL FUNCTION 'POPUP_TO_DECIDE_LIST'  "*-Equalização RISE x PRD - 19.07.2023 - JT
              EXPORTING
                CURSORLINE   = 1
                START_COL    = 70
                START_ROW    = 08
*               show_row     = 12
                TEXTLINE1    = 'Materiais sem De/Para AgriQ'
                TITEL        = 'Erro ao Gerar Solicitação de R.A.'
                DISPLAY_ONLY = ABAP_TRUE
              TABLES
*               im_ex_liste  = t_lista
                T_SPOPLI     = T_LISTA
              EXCEPTIONS
                NO_ENTRIES   = 1
                OTHERS       = 2.
          ENDIF.
          ME->ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302( I_GERA_RA = ABAP_ON I_QTD_RA = 0 ).
          ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '08' ).
        ENDIF.

*-------------------------------
*------ verifica se ha cod material agriq duplicado
*-------------------------------
        SORT T_0210 BY ID_MATNR_AGRIQ.

        L_AGRIQ_ANT = ABAP_OFF.
        L_MATNR_ANT = ABAP_OFF.

        LOOP AT T_0210 INTO W_0210.
          IF W_0210-ID_MATNR_AGRIQ = L_AGRIQ_ANT.
            SHIFT W_0210-MATNR          LEFT DELETING LEADING '0'.
            SHIFT W_0210-ID_MATNR_AGRIQ LEFT DELETING LEADING '0'.
            DATA(L_LINHA1) = |{ W_0210-MATNR } / { W_0210-ID_MATNR_AGRIQ }|.
            APPEND VALUE #( SELFLAG = ABAP_OFF VAROPTION = L_LINHA1 INACTIVE = ABAP_OFF ) TO T_LISTA.

            SHIFT L_MATNR_ANT           LEFT DELETING LEADING '0'.
            SHIFT L_AGRIQ_ANT           LEFT DELETING LEADING '0'.
            DATA(L_LINHA2) = |{ L_MATNR_ANT  } / { L_AGRIQ_ANT }|.
            APPEND VALUE #( SELFLAG = ABAP_OFF VAROPTION = L_LINHA2 INACTIVE = ABAP_OFF ) TO T_LISTA.
          ENDIF.
          L_MATNR_ANT = W_0210-MATNR.
          L_AGRIQ_ANT = W_0210-ID_MATNR_AGRIQ.
        ENDLOOP.

*-------------------------------
*------ Lista com materiais duplicados
*-------------------------------
        IF T_LISTA[] IS NOT INITIAL.
          SORT T_LISTA BY VAROPTION.
          DELETE ADJACENT DUPLICATES FROM T_LISTA
                                COMPARING VAROPTION.

          IF I_EXIBE_POPUP = ABAP_TRUE.
*           CALL FUNCTION 'ISH_POPUP_TO_DECIDE_LIST'
            CALL FUNCTION 'POPUP_TO_DECIDE_LIST'  "*-Equalização RISE x PRD - 19.07.2023 - JT
              EXPORTING
                CURSORLINE   = 1
                START_COL    = 70
                START_ROW    = 08
*               show_row     = 12
                TEXTLINE1    = 'Materiais Duplicados AgriQ'
                TITEL        = 'Erro ao Gerar Solicitação de R.A.'
*               coltitle     = 'Cód.Material / Cód.Material AgriQ'
                DISPLAY_ONLY = ABAP_TRUE
              TABLES
*               im_ex_liste  = t_lista
                T_SPOPLI     = T_LISTA
              EXCEPTIONS
                NO_ENTRIES   = 1
                OTHERS       = 2.
          ENDIF.
          ME->ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302( I_GERA_RA = ABAP_ON I_QTD_RA = 0 ).
          ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '09' ).
        ENDIF.

*-------------------------------
*------ Quantidade maxima de solic por RA
*-------------------------------
        CALL FUNCTION 'G_SET_GET_ALL_VALUES'
          EXPORTING
            SETNR           = 'QUANTIDADE_SOL_RECEITA'
            CLASS           = '0000'
            NO_DESCRIPTIONS = ''
          TABLES
            SET_VALUES      = T_VALUE
          EXCEPTIONS
            SET_NOT_FOUND   = 1
            OTHERS          = 2.

        READ TABLE T_VALUE INTO DATA(W_VALUE) INDEX 1.
        IF SY-SUBRC <> 0.
          L_SET_MAX_MAT = 4.
        ELSE.
          L_SET_MAX_MAT = W_VALUE-FROM.
        ENDIF.

*-------------------------------
*------ Quantidade de solicitacoes na carga
*-------------------------------
        T_0001_ITEM_GRP[] = T_0001_ITEM[].
        SORT T_0001_ITEM_GRP BY MATNR.
        DELETE ADJACENT DUPLICATES FROM T_0001_ITEM_GRP
                              COMPARING MATNR.

        DESCRIBE TABLE T_0001_ITEM_GRP LINES DATA(L_LINES).

        L_QUANT_MAT = L_LINES / L_SET_MAX_MAT.
        L_QUANT_MAT = CEIL( L_QUANT_MAT ).
        L_QUANT     = L_QUANT_MAT.

*       IF l_quant  > l_set_max_mat.
*         zif_integracao_agriq~set_mensagem( '07' ).
*       ENDIF.

*-------------------------------
*------ gravar zsdt0302
*-------------------------------
        ME->ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302( I_GERA_RA = ABAP_ON  I_QTD_RA = L_QUANT ).

*-------------------------------
*------ Agrupar materiais para envio ao Portal
*-------------------------------
        FREE:  T_GRP_MAT, L_IND.

        L_GRP = 1.

        LOOP AT T_0001_ITEM_GRP INTO W_0001_ITEM.
          L_IND = L_IND + 1.

          IF L_IND > L_SET_MAX_MAT.
            L_GRP = L_GRP + 1.
            L_IND = 1.
          ENDIF.

          CLEAR W_GRP_MAT.
          W_GRP_MAT-MATNR   = W_0001_ITEM-MATNR.
          W_GRP_MAT-GRP     = L_GRP.
          APPEND W_GRP_MAT TO T_GRP_MAT.
        ENDLOOP.

        T_GRP_MAT_AUX[] = T_GRP_MAT[].
        SORT T_GRP_MAT_AUX BY GRP.
        DELETE ADJACENT DUPLICATES FROM T_GRP_MAT_AUX
                              COMPARING GRP.

*-------------------------------
*-------Gerar Solicitacao RA para cada agrupamento de materiais
*-------------------------------
        FREE: L_ERRO.

        LOOP AT T_GRP_MAT_AUX INTO W_GRP_MAT_AUX.

          FREE: ME->ZIF_INTEGRACAO_AGRIQ~AT_ZSDT0001_ITEM.

          LOOP AT T_GRP_MAT     INTO W_GRP_MAT WHERE GRP = W_GRP_MAT_AUX-GRP.
            LOOP AT T_0001_ITEM INTO W_0001_ITEM WHERE MATNR = W_GRP_MAT-MATNR.
              APPEND W_0001_ITEM  TO ME->ZIF_INTEGRACAO_AGRIQ~AT_ZSDT0001_ITEM.
            ENDLOOP.
          ENDLOOP.

*-------------------------------
*---------Monta JSON
*-------------------------------
          ME->ZIF_INTEGRACAO_AGRIQ~SET_JSON_SOLIC_RA( ).

*-------------------------------
*-------- Executa API
*-------------------------------
          TRY .
              ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
                 )->SET_EXEC_AGRIQ( EXPORTING I_METODO = 'GERAR_RA' ).

            CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRA).
              L_ERRO = ABAP_TRUE.
              EXIT.
            CATCH ZCX_ERROR INTO DATA(EX_ERROR).
              L_ERRO = ABAP_TRUE.
              EXIT.
          ENDTRY.
        ENDLOOP.

        IF L_ERRO = ABAP_TRUE.
*-------------------------------
*-------- Cancela demais RA
*-------------------------------
          TRY.
              ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
                 )->SET_CANCELAR_SOL_RA( EXPORTING I_NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
                                                   I_CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA ).

            CATCH ZCX_INTEGRACAO INTO EX_INTEGRA.
            CATCH ZCX_ERROR      INTO EX_ERROR.
          ENDTRY.

          ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '13' ).
        ENDIF.

*-------------------------------
*------ Avalia se devemos cancelar o RA
*-------------------------------
        SELECT *
          FROM ZSDT0298
          INTO TABLE @DATA(T_0298)
         WHERE NRO_CGD       = @ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
           AND CH_REFERENCIA = @ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA
           AND CANCELADO     = @ABAP_FALSE.

        SELECT SINGLE *
          FROM ZSDT0302
          INTO @DATA(W_0302)
         WHERE NRO_CGD       = @ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
           AND CH_REFERENCIA = @ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA.

        DESCRIBE TABLE T_0298 LINES DATA(L_LINES_298).

        IF L_LINES_298 <> W_0302-QTD_SOLICITACAO_RA OR L_ERRO = ABAP_TRUE.
*-------------------------------
*-------- Cancelar RAs
*-------------------------------
          TRY.
              ZCL_INTEGRACAO_AGRIQ=>ZIF_INTEGRACAO_AGRIQ~GET_INSTANCE(
                 )->SET_CANCELAR_SOL_RA( EXPORTING I_NRO_CGD       = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
                                                   I_CH_REFERENCIA = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA ).

            CATCH ZCX_INTEGRACAO INTO EX_INTEGRA.
            CATCH ZCX_ERROR      INTO EX_ERROR.
          ENDTRY.

          ME->ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM( '13' ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_GRAVA_ZSDT0302.

    DATA: W_ZSDT0302  TYPE ZSDT0302.

    R_IF_INTEGRACAO_AGRIQ = ME.

    CLEAR W_ZSDT0302.
    W_ZSDT0302-MANDT               = SY-MANDT.
    W_ZSDT0302-NRO_CGD             = ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD.
    W_ZSDT0302-CH_REFERENCIA       = ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA.
    W_ZSDT0302-GERA_SOLICITACAO_RA = I_GERA_RA.
    W_ZSDT0302-QTD_SOLICITACAO_RA  = I_QTD_RA.
    MODIFY ZSDT0302             FROM W_ZSDT0302.
    COMMIT WORK AND WAIT. " STEFANINI - 2000016980 - IR193546 - 10.09.2024

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_HEADER.

    R_IF_INTEGRACAO_AGRIQ = ME.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_ID_REFERENCIA.

    "Incluir Chave de agriq
    R_IF_INTEGRACAO_AGRIQ = ME.
    ME->ZIF_INTEGRACAO_AGRIQ~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_JSON_ATUALIZA_NFE.

    DATA: L_CHAVE_NFE TYPE ZDE_CHAVE_NFE,
          L_SERIES    TYPE NUMC3,
          ZCL_UTIL    TYPE REF TO ZCL_UTIL.

    R_IF_INTEGRACAO_AGRIQ = ME.

    CREATE OBJECT ZCL_UTIL.

    FREE: ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON, AT_JSON.

*---------------------------------------------
*-- acessar NFe
*---------------------------------------------
    SELECT SINGLE DOCNUM, NFENUM, SERIES
      INTO @DATA(W_JDOC)
      FROM J_1BNFDOC
     WHERE DOCNUM = @I_DOCNUM.

    IF SY-SUBRC <> 0.
      CLEAR W_JDOC.
    ENDIF.

    SELECT SINGLE DOCNUM, CANCEL
      INTO @DATA(W_ACTIVE)
      FROM J_1BNFE_ACTIVE
     WHERE DOCNUM = @I_DOCNUM.

    L_SERIES      = W_JDOC-SERIES.
    W_JDOC-SERIES = L_SERIES.

    IF W_ACTIVE-CANCEL = ABAP_TRUE.
      W_JDOC-NFENUM = ABAP_OFF.
      W_JDOC-SERIES = ABAP_OFF.
    ENDIF.

*---------------------------------------------
*-- monta chave NFE
*---------------------------------------------
    L_CHAVE_NFE = ZCL_UTIL->GET_CHAVE_NFE( I_DOCNUM ).

*--------------------------------
*-- montar JSON
*--------------------------------
    SET_JSON( P1 = '{' ).
    SET_JSON( P1 = 'Solicitacaokey'   P2 = CONV #( ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_SOL_RECEITA ) ).
    SET_JSON( P1 = 'NumeroNf'         P2 = CONV #( W_JDOC-NFENUM ) ).
    SET_JSON( P1 = 'SerieNf'          P2 = CONV #( W_JDOC-SERIES ) ).
    SET_JSON( P1 = 'TipoDocumentoNf'  P2 = CONV #( 'NF' ) ).
    SET_JSON( P1 = 'ChaveDocumentoNf' P2 = CONV #( L_CHAVE_NFE ) ).
    SET_JSON( P1 = '}' ).

    ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON = AT_JSON.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_JSON_SOLIC_RA.

    DATA: T_0001_ITEM TYPE TABLE OF ZSDT0001_ITEM,
          T_0001_GRP  TYPE TABLE OF ZSDT0001_ITEM,
          W_JSON      TYPE ZSDE0060,
          W_PRODU     TYPE ZSDE0064,
          T_PRODU     TYPE ZSDE0064_T,
          L_NAME1     TYPE KNA1-NAME1,
          L_TIPOP     TYPE STRING,
          L_CPF       TYPE STRING,
          L_TOT_LFIMG TYPE ZSDT0001_ITEM-LFIMG.

    R_IF_INTEGRACAO_AGRIQ = ME.

    FREE: ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON, AT_JSON.

    T_0001_ITEM[] = ME->ZIF_INTEGRACAO_AGRIQ~AT_ZSDT0001_ITEM[].

    SELECT SINGLE *
      INTO @DATA(W_0134)
      FROM ZSDT0134
     WHERE NRO_CG        = @ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD
       AND CH_REFERENCIA = @ME->ZIF_INTEGRACAO_AGRIQ~AT_CH_REFERENCIA.

    IF SY-SUBRC <> 0.
      CLEAR W_0134.
    ENDIF.

    SELECT SINGLE *
      FROM ZSDT0139
      INTO @DATA(W_0139)
     WHERE NRO_CGD = @ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD.

    IF SY-SUBRC <> 0.
      CLEAR W_0139.
    ENDIF.

    SELECT SINGLE *
      FROM ZSDT0140
      INTO @DATA(W_0140)
     WHERE NRO_CGD = @ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD.

    IF SY-SUBRC <> 0.
      CLEAR W_0140.
    ENDIF.

    SELECT SINGLE *
      FROM ZSDT0132
      INTO @DATA(W_0132)
     WHERE NR_ROT = @W_0134-NR_ROT.

    IF SY-SUBRC <> 0.
      CLEAR W_0132.
    ENDIF.
*"// WBARBOSA BUG-185648 18/07/25 AGRIQ
    IF W_0132-LZONE IS NOT INITIAL.
      SELECT SINGLE *
        FROM TZONE
        INTO @DATA(LS_TZONE)
      WHERE ZONE1 EQ @W_0132-LZONE.
    ENDIF.
*"// WBARBOSA BUG-185648 18/07/25 AGRIQ

    READ TABLE T_0001_ITEM INTO DATA(W_0001_ITEM) INDEX 1.

    SELECT SINGLE *
      FROM VBAP
      INTO @DATA(W_VBAP)
     WHERE VBELN = @W_0001_ITEM-VBELN.

    IF SY-SUBRC <> 0.
      CLEAR W_VBAP.
    ENDIF.

    SELECT SINGLE *
      FROM KNA1
      INTO @DATA(W_KNA1)
     WHERE KUNNR = @W_0140-KUNNR.

    IF SY-SUBRC <> 0.
      CLEAR W_KNA1.
    ENDIF.

    SELECT SINGLE *
      FROM LFA1
      INTO @DATA(W_LFA1)
     WHERE LIFNR = @W_0139-COD_CE.

    IF SY-SUBRC <> 0.
      CLEAR W_LFA1.
    ENDIF.

    SELECT *
      INTO TABLE @DATA(T_0210)
      FROM ZSDT0210
       FOR ALL ENTRIES IN @T_0001_ITEM
     WHERE MATNR  = @T_0001_ITEM-MATNR.

    IF T_0210[] IS NOT INITIAL.
      SELECT *
        INTO TABLE @DATA(T_0299)
        FROM ZSDT0299
         FOR ALL ENTRIES IN @T_0210
       WHERE PRODUTOID = @T_0210-ID_MATNR_AGRIQ.
    ENDIF.

*--------------------------------
*-- tratamento cliente
*--------------------------------
    IF W_KNA1-STKZN = ABAP_OFF.
      L_TIPOP = 'Juridica'.
      L_NAME1 = ABAP_OFF.
      L_CPF   = W_KNA1-STCD1.
    ELSE.
      L_NAME1 = W_KNA1-NAME1.
      L_TIPOP = 'Fisica'.
      L_CPF   = W_KNA1-STCD2.
    ENDIF.

*--------------------------------
*-- agrupamento produtos
*--------------------------------
    T_0001_GRP[] = T_0001_ITEM[].
    SORT T_0001_GRP BY MATNR.
    DELETE ADJACENT DUPLICATES FROM T_0001_GRP
                          COMPARING MATNR.

    FREE: W_JSON.

*--------------------------------
*-- montar JSON
*--------------------------------
    W_JSON-CPFRESPONSAVELTECNICO                    = W_0139-CPF_RTC.
    W_JSON-NUMEROPEDIDO                             = W_VBAP-WERKS.
    W_JSON-NUMERONF                                 = ABAP_OFF.
    W_JSON-TIPODOCUMENTONF                          = ABAP_OFF.
    W_JSON-PRODUTOSDEFINIDOS                        = ABAP_TRUE.
    W_JSON-CHAVEDOCUMENTONF                         = ABAP_OFF.

    W_JSON-CLIENTE-ID                               = W_KNA1-KUNNR.
    W_JSON-CLIENTE-NOMEFANTASIA                     = W_KNA1-NAME1.
    W_JSON-CLIENTE-RAZAOSOCIAL                      = L_NAME1.
    W_JSON-CLIENTE-CPFCNPJ                          = L_CPF.
    W_JSON-CLIENTE-ENDERECO                         = W_KNA1-STRAS && '-' && W_KNA1-ORT02.
    W_JSON-CLIENTE-CEP                              = W_KNA1-PSTLZ.
    W_JSON-CLIENTE-NOMECIDADE                       = W_KNA1-ORT01.
    W_JSON-CLIENTE-SIGLAESTADO                      = W_KNA1-REGIO.
    W_JSON-CLIENTE-TELEFONE                         = ABAP_OFF.
    W_JSON-CLIENTE-TIPOPESSOA                       = L_TIPOP.

    W_JSON-CLIENTE-LOCALAPLICACAO-DESCRICAO         = W_0132-ROT_DESC.
    W_JSON-CLIENTE-LOCALAPLICACAO-INSCRICAOESTADUAL = W_KNA1-STCD3.
    W_JSON-CLIENTE-LOCALAPLICACAO-NOMECIDADE        = W_KNA1-ORT01.
    W_JSON-CLIENTE-LOCALAPLICACAO-SIGLAESTADO       = W_KNA1-REGIO.
    W_JSON-CLIENTE-LOCALAPLICACAO-CPFCNPJ           = W_KNA1-STCD2.
    W_JSON-CLIENTE-LOCALAPLICACAO-CODIGOEXTERNO     = W_0134-NR_ROT.

*"// WBARBOSA BUG-185648 18/07/25 AGRIQ
    W_JSON-CLIENTE-LOCALAPLICACAO-LAT               = LS_TZONE-ZLATITUDE.
    W_JSON-CLIENTE-LOCALAPLICACAO-LON               = LS_TZONE-ZLONGITUDE.
*"// WBARBOSA BUG-185648 18/07/25 AGRIQ

    W_JSON-UNIDADERECEBIMENTO-DESCRICAO             = W_LFA1-NAME1 && W_LFA1-NAME2.
    W_JSON-UNIDADERECEBIMENTO-ENDERECO              = W_LFA1-STRAS && '-' && W_LFA1-ORT02.
    W_JSON-UNIDADERECEBIMENTO-NOMECIDADE            = W_LFA1-ORT01.
    W_JSON-UNIDADERECEBIMENTO-SIGLAESTADO           = W_LFA1-REGIO.
    W_JSON-UNIDADERECEBIMENTO-CEP                   = W_LFA1-PSTLZ.
    W_JSON-UNIDADERECEBIMENTO-TELEFONE              = W_LFA1-TELF1.

*-----------------------------------
*-- elimina carac espeial
*-----------------------------------
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = W_JSON-CLIENTE-NOMEFANTASIA
      IMPORTING
        OUTTEXT           = W_JSON-CLIENTE-NOMEFANTASIA
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = W_JSON-CLIENTE-RAZAOSOCIAL
      IMPORTING
        OUTTEXT           = W_JSON-CLIENTE-RAZAOSOCIAL
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = W_JSON-CLIENTE-ENDERECO
      IMPORTING
        OUTTEXT           = W_JSON-CLIENTE-ENDERECO
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = W_JSON-CLIENTE-NOMECIDADE
      IMPORTING
        OUTTEXT           = W_JSON-CLIENTE-NOMECIDADE
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = W_JSON-UNIDADERECEBIMENTO-DESCRICAO
      IMPORTING
        OUTTEXT           = W_JSON-UNIDADERECEBIMENTO-DESCRICAO
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = W_JSON-UNIDADERECEBIMENTO-NOMECIDADE
      IMPORTING
        OUTTEXT           = W_JSON-UNIDADERECEBIMENTO-NOMECIDADE
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = W_JSON-UNIDADERECEBIMENTO-ENDERECO
      IMPORTING
        OUTTEXT           = W_JSON-UNIDADERECEBIMENTO-ENDERECO
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = W_JSON-CLIENTE-LOCALAPLICACAO-NOMECIDADE
      IMPORTING
        OUTTEXT           = W_JSON-CLIENTE-LOCALAPLICACAO-NOMECIDADE
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

*--------------------------------
*-- produtos
*--------------------------------
    LOOP AT T_0001_GRP INTO DATA(W_0001_GRP).

      CLEAR W_PRODU.

      READ TABLE T_0210 INTO DATA(W_0210) WITH KEY MATNR = W_0001_GRP-MATNR.
      IF SY-SUBRC <> 0.
        CLEAR W_0210.
      ENDIF.

      READ TABLE T_0299 INTO DATA(W_0299) WITH KEY PRODUTOID = W_0210-ID_MATNR_AGRIQ.
      IF SY-SUBRC <> 0.
        CLEAR W_0299.
      ENDIF.

      CLEAR L_TOT_LFIMG.
      LOOP AT T_0001_ITEM INTO W_0001_ITEM WHERE MATNR = W_0001_GRP-MATNR.
        L_TOT_LFIMG = L_TOT_LFIMG + W_0001_ITEM-LFIMG.
      ENDLOOP.

      W_PRODU-QUANTIDADE                            = L_TOT_LFIMG.
      W_PRODU-OBSERVACOES                           = ABAP_OFF.
      W_PRODU-PRODUTOID                             = W_0210-ID_MATNR_AGRIQ.
*     w_produ-codigomapa                            = w_0299-registroministerio.
      W_PRODU-CULTURAID                             = ABAP_OFF.

      APPEND W_PRODU                                TO T_PRODU.
    ENDLOOP.

    W_JSON-PRODUTOS[] = T_PRODU[].

*-----------------------------------
*-- monta JSON
*-----------------------------------
    AT_JSON = ZIF_INTEGRACAO_AGRIQ~AT_JSON = /UI2/CL_JSON=>SERIALIZE( DATA        = W_JSON
                                                                      PRETTY_NAME = /UI2/CL_JSON=>PRETTY_MODE-LOW_CASE ).

    ME->ZIF_INTEGRACAO_AGRIQ~AT_JSON = AT_JSON.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_MENSAGEM.

    R_IF_INTEGRACAO_AGRIQ = ME.

    CASE I_COD_MSG.

      WHEN '01'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Não encontrado Cabeçalho da Carga ' )
                              ATTR2 = CONV #( 'de Entrega Defensivos.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Não encontrado Cabeçalho da Carga ' )
            MSGV2  = CONV #( 'de Entrega Defensivos.' ).

      WHEN '02'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'RTC definido com Terceiro, logo a Solicitação ' )
                              ATTR2 = CONV #( 'de Receita não será gerada.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'W'
            MSGV1  = CONV #( 'RTC definido com Terceiro, logo a Solicitação ' )
            MSGV2  = CONV #( 'de Receita não será gerada.' ).

      WHEN '03'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Carga ' )
                              ATTR2 = CONV #( ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD )
                              ATTR3 = CONV #( ' não encontrada em ZSDT0134.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Carga ' )
            MSGV2  = CONV #( ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD )
            MSGV3  = CONV #( ' não encontrada em ZSDT0134.' ).

      WHEN '04'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Carga ' )
                              ATTR2 = CONV #( ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD )
                              ATTR3 = CONV #( ' não encontrada em Ordem Venda.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Carga ' )
            MSGV2  = CONV #( ME->ZIF_INTEGRACAO_AGRIQ~AT_NRO_CGD )
            MSGV3  = CONV #( ' não encontrada em Ordem Venda.' ).

      WHEN '05'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'O Cliente da Carga está Parametrizado como Revenda ' )
                              ATTR2 = CONV #( 'na Transação ZSDT0154, logo a Solicitação ' )
                              ATTR3 = CONV #( 'de Receita não será gerada.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'W'
            MSGV1  = CONV #( 'O Cliente da Carga está Parametrizado como Revenda ' )
            MSGV2  = CONV #( 'na Transação ZSDT0154, logo a Solicitação ' )
            MSGV3  = CONV #( 'de Receita não será gerada.' ).

      WHEN '06'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Não encontrado Itens na Carga.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Não encontrado Itens na Carga.' ).

      WHEN '07'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Quantidade Solicitação Ultrapassou limite.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Quantidade Solicitação Ultrapassou limite.' ).

      WHEN '08'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Material na Carga sem seu Cód.AGRIQ!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Material na Carga sem seu Cód.AGRIQ!' ).

      WHEN '09'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Há Cod.Material AgriQ duplicados!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Há Cod.Material AgriQ duplicados!' ).

      WHEN '10'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Ocorreu um erro na Integração. ' )
                              ATTR2 = CONV #( 'Verifique Log!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Ocorreu um erro na Integração. ' )
            MSGV2  = CONV #( 'Verifique Log!' ).

      WHEN '11'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Já há Solic. RA emitida para esta Carga.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Já há Solic. RA emitida para esta Carga.' ).

      WHEN '12'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Não localizada Solicitações de Receita ' )
                              ATTR2 = CONV #( 'para a Carga Informada!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'W'
            MSGV1  = CONV #( 'Não localizada Solicitações de Receita ' )
            MSGV2  = CONV #( 'para a Carga Informada!' ).

      WHEN '13'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Erro ao Gerar a Soicitações de Receita.' )
                              ATTR2 = CONV #( 'Solicitação Cancelada!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Erro ao Gerar a Soicitações de Receita.' )
            MSGV2  = CONV #( 'Solicitação Cancelada!' ).

      WHEN '14'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Não foi possível receber PDF ' )
                              ATTR2 = CONV #( 'da Receita informada!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Não foi possível receber PDF ' )
            MSGV2  = CONV #( 'da Receita informada!' ).

      WHEN '15'.
        "Recepção RA do Agriq sem estar no SAP - #135617 BG
*        RAISE EXCEPTION TYPE zcx_error
*          EXPORTING
*            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
*                              msgno = zcx_error=>zcx_erro_geral-msgno
*                              attr1 = CONV #( 'Receita não foi Localizada!' ) )
*            msgid  = zcx_error=>zcx_erro_geral-msgid
*            msgno  = zcx_error=>zcx_erro_geral-msgno
*            msgty  = 'E'
*            msgv1  = CONV #( 'Receita não foi Localizada!' ).

      WHEN '16'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Não foi possivel baixar PDF da Bry.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Não foi possivel baixar PDF da Bry.' ).

      WHEN '17'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Nro.NF não localizada na tabela ZSDT0001.' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Nro.NF não localizada na tabela ZSDT0001.' ).

      WHEN '18'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Não há Documentos assinados!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Não há Documentos assinados!' ).

      WHEN '19'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'Já há documentos assinados nesta carga!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'Já há documentos assinados nesta carga!' ).

      WHEN '20'.
        RAISE EXCEPTION TYPE ZCX_ERROR
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                              MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1 = CONV #( 'A NFe associada à receita já foi enviada ao Indea!' ) )
            MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( 'A NFe associada à receita já foi enviada ao Indea!' ).

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_SEND_MSG.

    TYPES BEGIN OF TY_RETORNO.
    TYPES: ACCESS_TOKEN TYPE STRING.
    TYPES: EXPIRES_IN TYPE STRING.
    TYPES: TOKEN_TYPE TYPE STRING.
    TYPES END OF TY_RETORNO.

    DATA: LC_INTEGRAR    TYPE REF TO ZCL_INTEGRACAO,
          LC_RETORNO     TYPE TY_RETORNO,
          L_ACCESS_TOKEN TYPE STRING,
          L_TOKEN_TYPE   TYPE STRING,
          L_EXPIRES_IN   TYPE STRING,
          L_FORCE        TYPE CHAR01.

    R_IF_INTEGRACAO_AGRIQ = ME.

    CREATE OBJECT LC_INTEGRAR.

    LC_INTEGRAR->ZIF_INTEGRACAO~AT_MULTIPART = ME->ZIF_INTEGRACAO_AGRIQ~AT_MULTIPART.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG(
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = E_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_AGRIQ~SET_SEND_MSG_AGRIQ.

    DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.

    CLEAR: E_ZINTEGRACAO_LOG.

    R_IF_INTEGRACAO_AGRIQ = ME.

    "Verificar a Função de Cada requisição
*   me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = 'servico_receita_agriq'.

    CREATE OBJECT LC_INTEGRACAO.

    LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_AGRIQ~AT_ID_INTEGRACAO
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


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.

*    DATA: l_id_integracao  TYPE zde_id_integracao,
*          l_access_token  TYPE string,
*          l_token_type    TYPE string,
*          l_expires_in    TYPE string.
*
**----------------------------
**---token
**----------------------------
*    TRY.
*        me->zif_integracao_agriq~set_ds_url(
*                           EXPORTING i_metodo        = 'TOKEN'
*          )->set_ds_data(
*          )->set_send_msg( IMPORTING e_id_integracao = DATA(lc_id_integracao)
*                                     e_header_fields = DATA(e_header_fields)
*          ).
*
*        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).
*
*      CATCH zcx_error INTO DATA(ex_erro).
*
*        RAISE EXCEPTION TYPE zcx_integracao
*          EXPORTING
*            textid = VALUE #( msgid = ex_erro->zif_error~msgid
*                              msgno = ex_erro->zif_error~msgno
*                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
*                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
*                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
*                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
*            msgid  = ex_erro->zif_error~msgid
*            msgno  = ex_erro->zif_error~msgno
*            msgty  = 'E'
*            msgv1  = ex_erro->zif_error~msgv1
*            msgv2  = ex_erro->zif_error~msgv2
*            msgv3  = ex_erro->zif_error~msgv3
*            msgv4  = ex_erro->zif_error~msgv4.
*
*    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: I_INBOUND1      TYPE ZDE_API_RECEITA_AGRIQ,
          I_INBOUND2      TYPE ZINC_BRY_IN_COLETA,
          I_INBOUND3      TYPE ZDE_API_AGRIQ_ASSINADA,
          W_DOCASSIN      TYPE ZINS_BRY_IN_DOC_ASSINADOS,
*
          T_ELEMENT_ARRAY TYPE ZDE_ELEMENT_ARRAY_T,
          W_ZSDT0218      TYPE ZSDT0218,
          W_ZSDT0219      TYPE ZSDT0219,
          T_PROD          TYPE ZSDS074_T,
          W_PROD          TYPE ZSDS074,
          L_DATA_EMISSAO  TYPE DATUM,
          L_TIPO_PESSOA   TYPE CHAR2.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO              = ABAP_FALSE.
    E_NM_CODE              = ABAP_FALSE.

    APPEND 'det'       TO T_ELEMENT_ARRAY.
    APPEND 'detPag'    TO T_ELEMENT_ARRAY.
    APPEND 'NFref'     TO T_ELEMENT_ARRAY.
    APPEND 'DI'        TO T_ELEMENT_ARRAY.
    APPEND 'adi'       TO T_ELEMENT_ARRAY.
    APPEND 'detExport' TO T_ELEMENT_ARRAY.
    APPEND 'med'       TO T_ELEMENT_ARRAY.
    APPEND 'arma'      TO T_ELEMENT_ARRAY.
    APPEND 'comb'      TO T_ELEMENT_ARRAY.
    APPEND 'vol'       TO T_ELEMENT_ARRAY.
    APPEND 'lacres'    TO T_ELEMENT_ARRAY.
    APPEND 'dup'       TO T_ELEMENT_ARRAY.
    APPEND 'pag'       TO T_ELEMENT_ARRAY.
    APPEND 'procRef'   TO T_ELEMENT_ARRAY.
    APPEND 'obsCont'   TO T_ELEMENT_ARRAY.
    APPEND 'obsFisco'  TO T_ELEMENT_ARRAY.

*--------------------------------------------------------------------------
*-- Este metodo recebe dois servicos:
*-- 1.Webhook da AgriQ quando a receita é gerada
*-- 2.Webhook da Bry, quando assinatuda digital é concluída
*--------------------------------------------------------------------------

    CASE ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA.
      WHEN 'servico_receita_agriq'.
*--------------------------------------------------------------------------
*------ 1.Webhook da AgriQ quando a receita é gerada
*--------------------------------------------------------------------------
        CASE ZCL_STRING=>UPPER( CONV #( I_MSG_COMPLETA-DS_FORMATO ) ).
          WHEN 'XML'.
            /UI2/CL_JSON=>DESERIALIZE(
              EXPORTING
                JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_MSG_INBOUND I_ELEMENT_ARRAY = T_ELEMENT_ARRAY )
              CHANGING
                DATA = I_INBOUND1 ).
          WHEN 'JSON'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = I_INBOUND1 ).
        ENDCASE.

*--------------------------------------
*------ tratar sensitive key
*--------------------------------------
        TRANSLATE I_INBOUND1-KEY TO UPPER CASE.
        CONDENSE  I_INBOUND1-KEY.

*--------------------------------------
*------ Processar
*--------------------------------------
        SELECT NRO_CGD, CH_REFERENCIA, ID, TIPO_ASSINATURA
          INTO @DATA(W_0298)
          FROM ZSDT0298
            UP TO 1 ROWS
         WHERE RECEITAKEY = @I_INBOUND1-KEY.
        ENDSELECT.

        IF SY-SUBRC <> 0.
*Recepção RA do Agriq sem estar no SAP - #135617 BG
          E_NM_CODE      = '200'.
          E_MSG_ERRO     = 'Receita não foi Localizada no SAP!'.
          E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                              '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
          E_SUCESSO      = ABAP_TRUE.
          EXIT.
        ENDIF.

        SELECT SINGLE CPF_RTC
          FROM ZSDT0139
          INTO @DATA(L_CPF_RTC)
         WHERE NRO_CGD = @W_0298-NRO_CGD.

        IF SY-SUBRC <> 0.
          E_NM_CODE      = '400'.
          E_MSG_ERRO     = 'CPF Agronomo não foi Localizado!'.
          E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                              '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
          E_SUCESSO      = ABAP_TRUE.
          EXIT.
          CLEAR L_CPF_RTC.
        ENDIF.

        SELECT BRANCH
          INTO @DATA(L_BRANCH)
          FROM ZSDT0001
            UP TO 1 ROWS
         WHERE CH_REFERENCIA = @W_0298-CH_REFERENCIA.
        ENDSELECT.

        IF SY-SUBRC <> 0.
          CLEAR L_BRANCH.
        ENDIF.

        L_DATA_EMISSAO = I_INBOUND1-DATAEMISSAO(4) && I_INBOUND1-DATAEMISSAO+5(2) && I_INBOUND1-DATAEMISSAO+8(2).

        TRANSLATE I_INBOUND1-CLIENTE-TIPOPESSOA TO UPPER CASE.
        CONDENSE I_INBOUND1-CLIENTE-TIPOPESSOA.
        L_TIPO_PESSOA  = COND #( WHEN I_INBOUND1-CLIENTE-TIPOPESSOA(3) = 'JUR' THEN 'PJ'
                                                                               ELSE 'PF' ).

*--------------------------------------
*------ gravar receituario agronomico
*--------------------------------------
        CLEAR W_ZSDT0218.

        W_ZSDT0218-MANDT               = SY-MANDT.
        W_ZSDT0218-NUMERORECEITA       = I_INBOUND1-RECEITANUMERO.
        W_ZSDT0218-NUMEROPEDIDO        = L_BRANCH.
        W_ZSDT0218-CPFRT               = L_CPF_RTC.
        W_ZSDT0218-RECEITAKEY          = I_INBOUND1-KEY.
        W_ZSDT0218-DATAEMISSAO         = L_DATA_EMISSAO.
        W_ZSDT0218-NUMEROART           = I_INBOUND1-NUMEROART.
        W_ZSDT0218-CLIENTE_ID_AGRIQ    = I_INBOUND1-CLIENTE-ID.
        W_ZSDT0218-NOMECLIENTE         = I_INBOUND1-CLIENTE-NOMEFANTASIA.
        W_ZSDT0218-RAZAOSOCIAL         = I_INBOUND1-CLIENTE-RAZAOSOCIAL.
        W_ZSDT0218-CPFCNPJCLIENTE      = I_INBOUND1-CLIENTE-CPFCNPJ.
        W_ZSDT0218-TIPOPESSOA          = L_TIPO_PESSOA.
        W_ZSDT0218-CANCELADA           = COND #( WHEN I_INBOUND1-ATIVO = ABAP_TRUE THEN ABAP_OFF
                                                                                   ELSE ABAP_TRUE ).
        W_ZSDT0218-URL_PDF             = I_INBOUND1-URLPDF.
        W_ZSDT0218-NR_RECEITA_COMPLETA = I_INBOUND1-NRRECEITACOMPLETA.
        W_ZSDT0218-TIPO_ASSINATURA     = W_0298-TIPO_ASSINATURA.
        W_ZSDT0218-USNAM               = SY-UNAME.
        W_ZSDT0218-DATA_ATUAL          = SY-DATUM.
        W_ZSDT0218-HORA_ATUAL          = SY-UZEIT.
        MODIFY ZSDT0218             FROM W_ZSDT0218.

*--------------------------------------
*------ itens receituario agronomico
*--------------------------------------
        T_PROD[] = I_INBOUND1-PRODUTOS[].

        LOOP AT T_PROD INTO W_PROD.

          CLEAR W_ZSDT0219.

          W_ZSDT0219-MANDT                = SY-MANDT.
          W_ZSDT0219-NUMERORECEITA        = I_INBOUND1-RECEITANUMERO.
          W_ZSDT0219-NUMEROPEDIDO         = L_BRANCH.
          W_ZSDT0219-CPFRT                = L_CPF_RTC.
          W_ZSDT0219-RECEITAKEY           = I_INBOUND1-KEY.
          W_ZSDT0219-PRODUTO_ID_AGRIQ     = W_PROD-PRODUTOID.
          W_ZSDT0219-CULTURA_ID_AGRIQ     = W_PROD-CULTURAID.
          W_ZSDT0219-AREA                 = W_PROD-AREAAPLICACAO.
          W_ZSDT0219-DOSE                 = W_PROD-DOSE.
          W_ZSDT0219-QUANTIDADE           = W_PROD-QUANTIDADE.
          W_ZSDT0219-QTDSEMENTES          = W_PROD-QTDSEMENTES.
          W_ZSDT0219-QUANTIDADEAPLICACOES = W_PROD-NUMEROAPLICACOES. "BUG 107322
          W_ZSDT0219-USNAM                = SY-UNAME.
          W_ZSDT0219-DATA_ATUAL           = SY-DATUM.
          W_ZSDT0219-HORA_ATUAL           = SY-UZEIT.
          MODIFY ZSDT0219              FROM W_ZSDT0219.
        ENDLOOP.

*--------------------------------------
*------ stualiza status receita
*--------------------------------------
        UPDATE ZSDT0298 SET STATUS        = '4'
                      WHERE NRO_CGD       = W_0298-NRO_CGD
                        AND CH_REFERENCIA = W_0298-CH_REFERENCIA
                        AND ID            = W_0298-ID.

        E_NM_CODE      = '200'.
        E_MSG_ERRO     = 'Processo finalizado com Sucesso.'.
        E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                            '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
        E_SUCESSO      = ABAP_TRUE.

        COMMIT WORK AND WAIT. " STEFANINI - 2000016980 - IR193546 - 10.09.2024

*       IF w_0298-tipo_assinatura = '1'.  "Assinatura eletronica
**---------------------------
**------- efetua assinatura digital BRY
**---------------------------
*         TRY .
*             zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
*                )->set_enviar_assinatura( EXPORTING i_receitakey  = w_zsdt0218-receitakey ).
*
*           CATCH zcx_integracao INTO DATA(ex_integra).
*             e_nm_code      = '400'.
*             e_msg_erro     = 'Erro na Integração com a Bry.'.
*             e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
*                                '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
*             e_sucesso      = abap_true.
*             EXIT.
*           CATCH zcx_error INTO DATA(ex_error).    "  "
*             e_nm_code      = '400'.
*             e_msg_erro     = 'Erro na Integração com a Bry.'.
*             e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
*                                 '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
*             e_sucesso      = abap_true.
*             EXIT.
*         ENDTRY.
*
**---------------------------
**------- msg retorno
**---------------------------
*         e_nm_code      = '200'.
*         e_msg_erro     = 'Assinatura enviada a Bry com Sucesso.'.
*         e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
*                             '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
*         e_sucesso      = abap_true.
*       ELSE.
**---------------------------
**------- msg retorno
**---------------------------
*         e_nm_code      = '200'.
*         e_msg_erro     = 'Processo finalizado com Sucesso.'.
*         e_msg_outbound = '{ "erro" : "'         && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
*                             '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
*         e_sucesso      = abap_true.
*       ENDIF.

      WHEN 'servico_receita_agriq_assinada'.
*--------------------------------------------------------------------------
*------ 1.Webhook da AgriQ quando a receita é gerada
*--------------------------------------------------------------------------
        CASE ZCL_STRING=>UPPER( CONV #( I_MSG_COMPLETA-DS_FORMATO ) ).
          WHEN 'XML'.
            /UI2/CL_JSON=>DESERIALIZE(
              EXPORTING
                JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_MSG_INBOUND I_ELEMENT_ARRAY = T_ELEMENT_ARRAY )
              CHANGING
                DATA = I_INBOUND3 ).
          WHEN 'JSON'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = I_INBOUND3 ).
        ENDCASE.

*--------------------------------------
*------ tratar sensitive key
*--------------------------------------
        TRANSLATE I_INBOUND3-ID TO UPPER CASE.
        CONDENSE  I_INBOUND3-ID.

*--------------------------------------
*------ Processar
*--------------------------------------
        SELECT *
          INTO @DATA(W_0298_A)
          FROM ZSDT0298
            UP TO 1 ROWS
         WHERE RECEITAKEY = @I_INBOUND3-ID.
        ENDSELECT.

        IF SY-SUBRC <> 0.
          "Recepção RA do Agriq sem estar no SAP - #135617 BG
          E_NM_CODE      = '200'.
          E_MSG_ERRO     = 'Receita não foi Localizada no SAP!'.
          E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                              '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
          E_SUCESSO      = ABAP_TRUE.
          EXIT.
        ENDIF.

        SELECT *
          FROM ZSDT0218
          INTO @DATA(W_0218_A)
            UP TO 1 ROWS
         WHERE RECEITAKEY = @W_0298_A-RECEITAKEY
           AND CANCELADA  = @ABAP_FALSE.
        ENDSELECT.

        IF SY-SUBRC <> 0.
          E_NM_CODE      = '400'.
          E_MSG_ERRO     = 'Receita não foi Localizada na ZSDT0218!'.
          E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                              '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
          E_SUCESSO      = ABAP_TRUE.
          EXIT.
        ENDIF.

        UPDATE ZSDT0218 SET URL_PDF_ASSINADO   = I_INBOUND3-URLDOCUMENTO
*                           doc_pdf_assinado   = i_inbound3-base64receita
                            CHAVE_ASSINATURA   = I_INBOUND3-HASH
                            CHAVE_PDF_ASSINADO = I_INBOUND3-HASH
                            TIPO_ASSINATURA    = '1'
                            USNAM              = SY-UNAME
                            DATA_ATUAL         = SY-DATUM
                            HORA_ATUAL         = SY-UZEIT
                      WHERE NUMERORECEITA      = W_0218_A-NUMERORECEITA
                        AND NUMEROPEDIDO       = W_0218_A-NUMEROPEDIDO
                        AND CPFRT              = W_0218_A-CPFRT
                        AND RECEITAKEY         = W_0218_A-RECEITAKEY.

        E_NM_CODE      = '200'.
        E_MSG_ERRO     = 'Processo finalizado com Sucesso.'.
        E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                            '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
        E_SUCESSO      = ABAP_TRUE.

        COMMIT WORK AND WAIT. " STEFANINI - 2000016980 - IR193546 - 10.09.2024

      WHEN 'bry_set_coleta_finalizada'.
*--------------------------------------------------------------------------
*------ 2.Webhook da Bry, quando assinatuda digital é concluída
*--------------------------------------------------------------------------
        CASE ZCL_STRING=>UPPER( CONV #( I_MSG_COMPLETA-DS_FORMATO ) ).
          WHEN 'XML'.
            /UI2/CL_JSON=>DESERIALIZE(
              EXPORTING
                JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_MSG_INBOUND I_ELEMENT_ARRAY = T_ELEMENT_ARRAY )
              CHANGING
                DATA = I_INBOUND2 ).
          WHEN 'JSON'.
            /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = I_INBOUND2 ).
        ENDCASE.

        IF I_INBOUND2-STATUS <> 'CONCLUIDO'.
          E_NM_CODE      = '400'.
          E_MSG_ERRO     = 'Coleta recusada na BRY'.
          E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                              '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
          E_SUCESSO      = ABAP_TRUE.
          EXIT.
        ENDIF.

        IF I_INBOUND2-CHAVE IS INITIAL.
          E_NM_CODE      = '400'.
          E_MSG_ERRO     = 'Chave de Coleta em branco'.
          E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                              '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
          E_SUCESSO      = ABAP_TRUE.
          EXIT.
        ENDIF.

*       SELECT *
*         INTO @DATA(w_0218)
*         FROM zsdt0218
*           UP TO 1 ROWS
*        WHERE chave_workflow = @i_inbound2-chave.
*       ENDSELECT.

        SELECT NUMERORECEITA, NUMEROPEDIDO, CPFRT, RECEITAKEY, CHAVE_WORKFLOW
          INTO TABLE @DATA(T_0218)
          FROM ZSDT0218.

        READ TABLE T_0218 INTO DATA(W_0218) WITH KEY CHAVE_WORKFLOW = I_INBOUND2-CHAVE.

        IF SY-SUBRC <> 0.
          E_NM_CODE      = '400'.
          E_MSG_ERRO     = 'Chave Workflow nao localizada em ZSDT0218'.
          E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                              '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
          E_SUCESSO      = ABAP_TRUE.
          EXIT.
        ENDIF.

        READ TABLE I_INBOUND2-DOCUMENTOSASSINADOS INTO W_DOCASSIN INDEX 1.

*       w_0218-chave_pdf_assinado       = w_docassin-chave.
*       w_0218-url_pdf_assinado         = w_docassin-link.
*       w_0218-url_relatorio_assinatura = i_inbound2-protocoloassinatura-linkrelatorio.
*       MODIFY zsdt0218              FROM w_0218.

        UPDATE ZSDT0218 SET CHAVE_PDF_ASSINADO       = W_DOCASSIN-CHAVE
                            URL_PDF_ASSINADO         = W_DOCASSIN-LINK
                            URL_RELATORIO_ASSINATURA = I_INBOUND2-PROTOCOLOASSINATURA-LINKRELATORIO
                            USNAM                    = SY-UNAME
                            DATA_ATUAL               = SY-DATUM
                            HORA_ATUAL               = SY-UZEIT
                      WHERE NUMERORECEITA            = W_0218-NUMERORECEITA
                        AND NUMEROPEDIDO             = W_0218-NUMEROPEDIDO
                        AND CPFRT                    = W_0218-CPFRT
                        AND RECEITAKEY               = W_0218-RECEITAKEY.

*----------------------------
*------ msg retorno
*----------------------------
        E_NM_CODE      = '200'.
        E_MSG_ERRO     = 'Documento Assinado recebido com Sucesso.'.
        E_MSG_OUTBOUND = '{ "erro" : "'         && E_MSG_ERRO   && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ',' &&
                            '"status_code" : "' && E_NM_CODE    && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
        E_SUCESSO      = ABAP_TRUE.

        COMMIT WORK AND WAIT. " STEFANINI - 2000016980 - IR193546 - 10.09.2024
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.
ENDCLASS.
