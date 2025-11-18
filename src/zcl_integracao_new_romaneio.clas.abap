class ZCL_INTEGRACAO_NEW_ROMANEIO definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_NEW_ROMANEIO .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_NEW_ROMANEIO IMPLEMENTATION.


  method CONSTRUCTOR.
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_grc_new_doc.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_referencia-tp_referencia = 'SAP_NEW_ROMANEIO'.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
  R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_FALSE.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  r_if_integracao_inject = me.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
     "DATA: I_INBOUND       TYPE ZDE_PROCESSO,
          " I_INBOUND       TYPE TABLE OF ZSDS001,
          "T_ELEMENT_ARRAY  TYPE ZDE_ELEMENT_ARRAY_T.

    E_SUCESSO = ABAP_FALSE.
    R_IF_INTEGRACAO_INJECT = ME.

*
*    APPEND 'det' TO T_ELEMENT_ARRAY.
*    APPEND 'detPag' TO T_ELEMENT_ARRAY.
*    APPEND 'NFref' TO T_ELEMENT_ARRAY.
*    APPEND 'DI' TO T_ELEMENT_ARRAY.
*    APPEND 'adi' TO T_ELEMENT_ARRAY.
*    APPEND 'detExport' TO T_ELEMENT_ARRAY.
*    APPEND 'med' TO T_ELEMENT_ARRAY.
*    APPEND 'arma' TO T_ELEMENT_ARRAY.
*    APPEND 'comb' TO T_ELEMENT_ARRAY.
*    APPEND 'vol' TO T_ELEMENT_ARRAY.
*    APPEND 'lacres' TO T_ELEMENT_ARRAY.
*    APPEND 'dup' TO T_ELEMENT_ARRAY.
*    APPEND 'pag' TO T_ELEMENT_ARRAY.
*    APPEND 'procRef' TO T_ELEMENT_ARRAY.
*    APPEND 'obsCont' TO T_ELEMENT_ARRAY.
*    APPEND 'obsFisco' TO T_ELEMENT_ARRAY.
*
*    CASE ZCL_STRING=>UPPER( CONV #( I_MSG_COMPLETA-DS_FORMATO ) ).
*      WHEN 'XML'.
*        /UI2/CL_JSON=>DESERIALIZE(
*          EXPORTING
*            JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_MSG_INBOUND  I_ELEMENT_ARRAY = T_ELEMENT_ARRAY )
*          CHANGING
*            DATA = I_INBOUND ).
*      WHEN 'JSON'.
*        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = I_INBOUND ).
*    ENDCASE.
*
*    CALL FUNCTION 'ZSD_INBOUND_REMESSA'
*      TABLES
*        IB_ROMANEIO = I_INBOUND.
*
*    COMMIT WORK.

*    SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231
*      FROM ZSDT0231
*     WHERE OBJ_KEY EQ @I_INBOUND-PROCESSO-INFOSISTEMAORIGEM-ID.

*    IF SY-SUBRC IS INITIAL AND ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM IS NOT INITIAL.
*
*      SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
*        FROM J_1BNFDOC
*       WHERE DOCNUM EQ @ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM.
*
*      IF SY-SUBRC IS NOT INITIAL.
*        DELETE FROM ZSDT0231
*         WHERE OBJ_KEY EQ I_INBOUND-PROCESSO-INFOSISTEMAORIGEM-ID
*           AND OBJ_KEY NE SPACE.
*        SY-SUBRC = 1.
*      ENDIF.
*
*    ENDIF.

*    IF SY-SUBRC IS INITIAL.
*
*      SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFDOC
*        FROM J_1BNFDOC
*       WHERE DOCNUM EQ @ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM.
*
*      SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE
*        FROM J_1BNFE_ACTIVE
*       WHERE DOCNUM EQ @ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM.
*
*      E_SUCESSO = ABAP_TRUE.
*
*    ENDIF.

*    IF E_SUCESSO NE ABAP_TRUE.
*      TRY .
*          ME->ZIF_INTEGRACAO_GRC_NEW_NFE~SET_NEW_DOC_DISCAL(
*             EXPORTING
*               I_DADOS              = I_INBOUND
*             IMPORTING
*              E_DOCUMENTO           = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFDOC
*              E_INFO_DOC_ELETRONICO = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE ).
*
*          E_SUCESSO = ABAP_TRUE.
*          DATA(LC_REGISTRO_CRIADO) = ABAP_TRUE.
*
*        CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRACAO).
*          DATA(MS_ERRO) = EX_INTEGRACAO->ZIF_ERROR~GET_MSG_ERRO( ).
*        CATCH ZCX_ERROR INTO DATA(EX_ERRO).
*          MS_ERRO = EX_ERRO->ZIF_ERROR~GET_MSG_ERRO( ).
*        CATCH CX_ROOT INTO DATA(EX_ROOT).
*          MS_ERRO = EX_ROOT->GET_TEXT( ).
*      ENDTRY.
*    ELSE.
*      LC_REGISTRO_CRIADO = ABAP_FALSE.
*    ENDIF.
*
*    CASE E_SUCESSO.
*      WHEN ABAP_TRUE.
*
*        IF LC_REGISTRO_CRIADO = ABAP_FALSE .
*
*          "Verificar Número Não Determinado """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*          TRY .
*              ZCL_NFE=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM
*                )->SET_REGISTRO(
*                     EXPORTING
*                       I_DOCNUM = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM
*                       I_SEM_BLOQUEIO = ABAP_TRUE
*                )->GET_CK_DETERMINAR_NUMERO(
*                )->SET_DET_NUMERO(
*                )->GET_REGISTRO(
*                     IMPORTING
*                       E_DOCUMENTO = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFDOC
*                       E_INFO_DOC_ELETRONICO = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE
*                ).
*
*            CATCH ZCX_DOC_ELETRONICO INTO DATA(EX_DOC_ELETRONICO).
*
*              TRY .
*                  ZCL_NFE=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM
*                    )->SET_REGISTRO(
*                         EXPORTING
*                           I_DOCNUM = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM
*                           I_SEM_BLOQUEIO = ABAP_TRUE
*                    )->SET_AUTORIZAR(
*                    )->GET_REGISTRO(
*                         IMPORTING
*                           E_DOCUMENTO = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFDOC
*                           E_INFO_DOC_ELETRONICO = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE
*                    ).
*                CATCH ZCX_DOC_ELETRONICO.
*                CATCH CX_ROOT.
*              ENDTRY.
*            CATCH CX_ROOT.
*          ENDTRY.
*
*        ENDIF.
*        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*        DATA(E_CHAVE) = ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-REGIO &&
*                        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-NFYEAR &&
*                        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-NFMONTH &&
*                        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-STCD1 &&
*                        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-MODEL &&
*                        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-SERIE &&
*                        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-NFNUM9 &&
*                        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-DOCNUM9 &&
*                        ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-CDV.

*        E_MSG_OUTBOUND = '{ "protocolo" : "' &&
*                                ZCL_STRING=>LPAD( I_STR  = CONV #( ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-PROTOCOLO ) I_QTD  = 15 I_CHAR = '0'  ) &&  '" ,'
*                                && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*             ' "docnum" : "' && ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFDOC-DOCNUM &&  '" ,' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*             ' "nfenum" : "' && ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-NFNUM9 &&  '" ,  ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*             ' "serie"  : "' && ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-SERIE &&  '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*             ' "model"  : "' && ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-MODEL &&  '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*             ' "chave"  : "' && E_CHAVE && '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*             ' "protocolo_autorizacao" : "' && ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-AUTHCOD &&  '", ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*             ' "cnpj"   : "' && ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_J_1BNFE_ACTIVE-STCD1 &&  '" ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*             ' }'.
*
*        E_NM_CODE  = '200'.
*        E_MSG_ERRO = 'Ok'.
*
*      WHEN ABAP_FALSE.
*        MS_ERRO = ZCL_STRING=>TIRA_ACENTOS( MS_ERRO ).
*        MS_ERRO = ZCL_STRING=>CONVERT_TO_UTF8( MS_ERRO ).
*
*        E_NM_CODE  = '400'.
*        E_MSG_ERRO = 'Bad Request'.
*
*        E_MSG_OUTBOUND = '{ ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '"protocolo" : " ' &&
*                               ZCL_STRING=>LPAD( I_STR  = CONV #( ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-PROTOCOLO ) I_QTD  = 15 I_CHAR = '0'  ) &&  '", ' &&
*                            ' "erro" : "' && MS_ERRO && '"' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
*                          '}'.
*
*        SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231
*          FROM ZSDT0231
*         WHERE OBJ_KEY EQ @I_INBOUND-PROCESSO-INFOSISTEMAORIGEM-ID.
*
*        IF SY-SUBRC IS INITIAL AND ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM IS NOT INITIAL.
*
*          SELECT SINGLE * INTO @WA_J_1BNFDOC
*            FROM J_1BNFDOC
*           WHERE DOCNUM EQ @ME->ZIF_INTEGRACAO_GRC_NEW_NFE~AT_ZSDT0231-DOCNUM.
*
*          IF SY-SUBRC IS NOT INITIAL.
*            DELETE FROM ZSDT0231
*             WHERE OBJ_KEY EQ I_INBOUND-PROCESSO-INFOSISTEMAORIGEM-ID
*               AND OBJ_KEY NE SPACE.
*          ENDIF.
*
*        ENDIF.
*
*    ENDCASE.
*
*    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
     E_SUCESSO = ABAP_TRUE.

    R_IF_INTEGRACAO_INJECT = ME.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
        R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_NEW_ROMANEIO~GET_INSTANCE.

     IF zif_integracao_new_romaneio~at_if_integracao_new_romaneio IS NOT BOUND.
      CREATE OBJECT zif_integracao_new_romaneio~at_if_integracao_new_romaneio TYPE zcl_integracao_new_romaneio.
    ENDIF.
    R_IF_INTEGRACAO_NEW_ROMANEIO = zif_integracao_new_romaneio~at_if_integracao_new_romaneio.

  endmethod.


  method ZIF_INTEGRACAO_NEW_ROMANEIO~GET_VALIDAR_DADOS.

    DATA: WA_ZFIWRT0009 TYPE LINE OF ZFIWRT0009_T.

    R_IF_INTEGRACAO_NEW_ROMANEIO = ME.

  endmethod.


  METHOD zif_integracao_new_romaneio~set_ds_data.


    "Incluir Texto JSON para integração
    r_if_integracao_new_romaneio = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

    DATA: i_inbound       TYPE zsds001. "ZSDt001.


    "Validar Json
    /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = i_inbound ).

    IF id_referencia IS NOT INITIAL.
      me->zif_integracao_new_romaneio~at_recebido = id_referencia.
    ELSE.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZSEQ_RECEB'
        IMPORTING
          number                  = me->zif_integracao_new_romaneio~at_recebido
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
    ENDIF.

    IF i_inbound IS NOT INITIAL.
      me->zif_integracao_new_romaneio~at_zsdt001 = i_inbound.
      me->zif_integracao_inject~at_referencia-id_referencia = me->zif_integracao_new_romaneio~at_recebido.
    ENDIF.

    CHECK i_inbound IS INITIAL.

    RAISE EXCEPTION TYPE zcx_integracao
      EXPORTING
        textid = VALUE #( msgid = zcx_integracao=>zcx_erro_body_recebido-msgid
                          msgno = zcx_integracao=>zcx_erro_body_recebido-msgno )
        msgid  = zcx_integracao=>zcx_erro_body_recebido-msgid
        msgno  = zcx_integracao=>zcx_erro_body_recebido-msgno
        msgty  = 'E'.

  ENDMETHOD.


  METHOD zif_integracao_new_romaneio~set_new_romaneio.
    DATA: t_romaneio  TYPE zsdt001.

    r_if_integracao_new_romaneio = me.

    APPEND me->zif_integracao_new_romaneio~at_zsdt001 TO t_romaneio[].
    "t_romaneio[] = me->zif_integracao_new_romaneio~at_zsdt001.

* CALL FUNCTION 'ZSD_INBOUND_REMESSA'
*      TABLES
*        IB_ROMANEIO = t_romaneio.

    CALL FUNCTION 'ZSD_INBOUND_REMESSA_BSA'
      TABLES
        ib_romaneio = t_romaneio.

    COMMIT WORK.

  ENDMETHOD.


  method ZIF_INTEGRACAO_NEW_ROMANEIO~SET_SEND_MSG.
     DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.

      CLEAR: E_ZINTEGRACAO_LOG.

      r_if_integracao_new_romaneio = ME.

      "Verificar a Função de Cada requisição
      ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = '/sap/newromaneio'. " mudar isso aqui

      CREATE OBJECT LC_INTEGRACAO.

      LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
        )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->zif_integracao_new_romaneio~AT_ID_INTEGRACAO
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
  endmethod.
ENDCLASS.
