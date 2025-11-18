class ZCL_INTEGRACAO_GRC_NEW_NFE_CA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_GRC_NEW_NFE_CA .
  interfaces ZIF_INTEGRACAO_INJECT .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_GRC_NEW_NFE_CA IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_grc_can_doc.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_referencia-tp_referencia = 'NFE_SAP_GRC'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE_CA~GET_INSTANCE.

    IF ZIF_INTEGRACAO_GRC_NEW_NFE_CA~AT_IF_INTEGRACAO_GRC_CAN_NFE IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_GRC_NEW_NFE_CA~AT_IF_INTEGRACAO_GRC_CAN_NFE TYPE ZCL_INTEGRACAO_GRC_NEW_NFE_CA.
    ENDIF.
    R_IF_INTEGRACAO_GRC_CAN_NFE = ZIF_INTEGRACAO_GRC_NEW_NFE_CA~AT_IF_INTEGRACAO_GRC_CAN_NFE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE_CA~SET_DS_DATA.

    DATA: I_INBOUND TYPE ZDE_PROCESSO_CAN.

    R_IF_INTEGRACAO_GRC_CAN_NFE = ME.

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

    IF I_INBOUND-I_XJUST IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_EMPTY_JUSTIFY-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_EMPTY_JUSTIFY-MSGNO )
          MSGID  = ZCX_INTEGRACAO=>ZCX_EMPTY_JUSTIFY-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_EMPTY_JUSTIFY-MSGNO
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
                            ATTR1 = 'Data Emissão' )
          MSGID  = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_DATA_ERRADA-MSGNO
          MSGTY  = 'E'
          MSGV1  = 'Data Emissão'.
    ENDIF.

    SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_GRC_NEW_NFE_CA~AT_ZSDT0231
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


  METHOD ZIF_INTEGRACAO_GRC_NEW_NFE_CA~SET_SEND_MSG.

    DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_GRC_CAN_NFE = ME.

    "Verificar a Função de Cada requisição
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = '/nfe/cannfwriter'.

    CREATE OBJECT LC_INTEGRACAO.

    LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_GRC_NEW_NFE_CA~AT_ID_INTEGRACAO
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


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: I_INBOUND TYPE ZDE_PROCESSO_CAN.

    E_SUCESSO = ABAP_FALSE.

    R_IF_INTEGRACAO_INJECT = ME.

    CASE ZCL_STRING=>UPPER( CONV #( I_MSG_COMPLETA-DS_FORMATO ) ).
      WHEN 'XML'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = ZCL_STRING=>XML_TO_JSON( I_XML = I_MSG_INBOUND  ) CHANGING DATA = I_INBOUND ).
      WHEN 'JSON'.
        /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = I_INBOUND ).
    ENDCASE.

    TRY .

        SELECT SINGLE * INTO @ME->ZIF_INTEGRACAO_GRC_NEW_NFE_CA~AT_ZSDT0231
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

        TRY .
            DATA(LC_NFE) = ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = ME->ZIF_INTEGRACAO_GRC_NEW_NFE_CA~AT_ZSDT0231-DOCNUM
              )->SET_REGISTRO( EXPORTING I_DOCNUM = ME->ZIF_INTEGRACAO_GRC_NEW_NFE_CA~AT_ZSDT0231-DOCNUM  I_SEM_BLOQUEIO = ABAP_TRUE
              )->SET_CANCELAR( EXPORTING I_DS_MOTIVO = I_INBOUND-I_XJUST
              )->SET_LIBERAR_REGISTRO(
              )->SET_CLEAR_LOG_ERRO(
              ).

            E_SUCESSO = ABAP_TRUE.
            CLEAR: LC_NFE.

          CATCH ZCX_DOC_ELETRONICO INTO DATA(EX_DOC_ELETRONICO).

            LC_NFE->SET_LIBERAR_REGISTRO( ).
            CLEAR: LC_NFE.
            DATA(MS_ERRO) = EX_DOC_ELETRONICO->GET_LONGTEXT( ).
        ENDTRY.

        CASE E_SUCESSO.
          WHEN ABAP_TRUE.

            E_NM_CODE  = '200'.
            E_MSG_ERRO = 'Ok'.

            E_MSG_OUTBOUND = '{ ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '"protocolo" : " ' && ZCL_STRING=>LPAD( I_STR  = CONV #( I_MSG_COMPLETA-ID_INTEGRACAO ) I_QTD  = 15 I_CHAR = '0'  ) &&  '" '
                                   && CL_ABAP_CHAR_UTILITIES=>NEWLINE && ' }'.
          WHEN ABAP_FALSE.

            E_NM_CODE  = '400'.
            E_MSG_ERRO = 'Bad Request'.

            MS_ERRO = ZCL_STRING=>TIRA_ACENTOS( MS_ERRO ).
            MS_ERRO = ZCL_STRING=>CONVERT_TO_UTF8( MS_ERRO ).
            E_MSG_OUTBOUND = '{ ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '"protocolo" : " ' &&
                                   ZCL_STRING=>LPAD( I_STR  = CONV #( I_MSG_COMPLETA-ID_INTEGRACAO ) I_QTD  = 15 I_CHAR = '0'  ) &&  '", ' &&
                                ' "erro" : "' && MS_ERRO && '"'
                                && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
        ENDCASE.

      CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRACAO).

        E_NM_CODE  = '400'.
        E_MSG_ERRO = 'Bad Request'.

        MS_ERRO = EX_INTEGRACAO->ZIF_ERROR~GET_MSG_ERRO( ).
        E_MSG_OUTBOUND = '{ ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '"protocolo" : " ' &&
                               ZCL_STRING=>LPAD( I_STR  = CONV #( I_MSG_COMPLETA-ID_INTEGRACAO ) I_QTD  = 15 I_CHAR = '0'  ) &&  '", ' &&
                            ' "erro" : "' && MS_ERRO && '"'
                            && CL_ABAP_CHAR_UTILITIES=>NEWLINE && '}'.
    ENDTRY.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.
ENDCLASS.
