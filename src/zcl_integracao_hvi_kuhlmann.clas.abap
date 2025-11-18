class ZCL_INTEGRACAO_HVI_KUHLMANN definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_HVI_KUHLMANN .
  interfaces ZIF_INTEGRACAO_INJECT .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_HVI_KUHLMANN IMPLEMENTATION.


  METHOD constructor.
    me->zif_integracao_hvi_kuhlmann~set_servico( i_servico = i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_kuhlmann_ana.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    "ME->ZIF_INTEGRACAO_HVI_KUHLMANN~SET_DS_URL( ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_HVI_KUHLMANN~GET_ID_REFERENCIA.

    R_IF_INTEGRACAO_HVI_KUHLMANN = ME.

    E_REFERENCIA-TP_REFERENCIA = 'KUHLMANN_HVI'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_USUARIO.

  ENDMETHOD.


  METHOD zif_integracao_hvi_kuhlmann~get_instance.

    IF zif_integracao_hvi_kuhlmann~at_if_integracao_hvi_kuhlmann IS NOT BOUND.
      CREATE OBJECT zif_integracao_hvi_kuhlmann~at_if_integracao_hvi_kuhlmann
        TYPE zcl_integracao_hvi_kuhlmann
        EXPORTING
          i_servico = i_servico.
*        CATCH zcx_integracao.    " .
    ENDIF.
    IF i_servico IS NOT INITIAL.
      zif_integracao_hvi_kuhlmann~at_servico = i_servico.
    ENDIF.

    r_if_integracao_hvi_kuhlmann = zif_integracao_hvi_kuhlmann~at_if_integracao_hvi_kuhlmann.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_HVI_KUHLMANN~GET_JSON.
    R_IF_INTEGRACAO_HVI_KUHLMANN = ME.

    CHECK ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_CLIENTE_ID IS NOT INITIAL.

    E_JSON = '{' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             ' "cliente_id" : ' && ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_CLIENTE_ID && ' ' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
             '}'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_HVI_KUHLMANN~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_HVI_KUHLMANN = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  ENDMETHOD.


  METHOD zif_integracao_hvi_kuhlmann~set_ds_url.

    r_if_integracao_hvi_kuhlmann = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ 'K'
       AND servico EQ @me->zif_integracao_hvi_kuhlmann~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_integracao_hvi_kuhlmann~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_integracao_hvi_kuhlmann~at_servico ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token    = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && zif_integracao_hvi_kuhlmann=>at_fc_end_point.

    "IF ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_CLIENTE_ID IS NOT INITIAL.
    "  ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL = |{ ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL }?cliente_id={ ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_CLIENTE_ID }|.
    "ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_integracao_hvi_kuhlmann=>at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_hvi_kuhlmann~set_id_referencia( ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_HVI_KUHLMANN~SET_HVI_BUSCAR.

    R_IF_INTEGRACAO_HVI_KUHLMANN = ME.

    ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_CLIENTE_ID = I_CLIENTE_ID.
    "ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_SENHA = I_SENHA.
    "ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_USUARIO = I_USUARIO.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_HVI_KUHLMANN~SET_DS_URL(
      )->GET_JSON( IMPORTING E_JSON = DATA(LC_JSON)
      )->SET_DS_DATA( EXPORTING I_JSON = LC_JSON
      )->SET_DS_URL(
      )->SET_SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO E_INTEGRACAO	= E_INTEGRACAO
      ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_HVI_KUHLMANN~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_INTEGRACAO_HVI_KUHLMANN = ME.
    ME->ZIF_INTEGRACAO_HVI_KUHLMANN~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_HVI_KUHLMANN~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_HVI_KUHLMANN = ME.

    CREATE OBJECT LC_INTEGRAR.

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


  METHOD zif_integracao_hvi_kuhlmann~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_integracao_hvi_kuhlmann~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_integracao_token_kuhlmann(
               zcl_integracao_token_kuhlmann=>zif_integracao_token_kuhlmann~get_instance( i_servico = CONV #( me->zif_integracao_hvi_kuhlmann~at_servico )
                 )->set_ds_url(
                 )->set_usuario_senha( "EXPORTING I_SENHA = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_SENHA I_USUARIO = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_USUARIO
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


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.

    DATA: ORDEM_SERVICO TYPE ZDE_KUHLMANN_HVI_RETORNO.

    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).

    CHECK ORDEM_SERVICO-OS_ID IS NOT INITIAL.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ZCL_INTEGRACAO_COF_KUHLMANN=>ZIF_INTEGRACAO_COF_KUHLMANN~GET_INSTANCE(
      )->SET_HVI_CONFIRMAR( EXPORTING
        "I_USUARIO = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_USUARIO
        "I_SENHA   = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_SENHA
        I_OS_ID   = ORDEM_SERVICO-OS_ID
      ).
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.

    DATA: ORDEM_SERVICO TYPE ZDE_KUHLMANN_HVI_RETORNO.

    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).

    CHECK ORDEM_SERVICO-OS_ID IS NOT INITIAL.

    DATA: HVI    TYPE ZPP_KUHLMANN_HVI,
          IT_HVI TYPE TABLE OF ZPP_KUHLMANN_HVI.

    CLEAR: IT_HVI[].

    LOOP AT ORDEM_SERVICO-FARDOS INTO DATA(WA_FARDO).
      CLEAR: HVI.
      HVI-FARDO  = WA_FARDO-FARDO.
      HVI-OS_ID  = ORDEM_SERVICO-OS_ID.
      HVI-OS_NR  = ORDEM_SERVICO-NUMERO_OS.
      HVI-ROMANEIO = ORDEM_SERVICO-ROMANEIO.
      HVI-LABORATORIO = ORDEM_SERVICO-LABORATORIO.

      DATA(DT_ENTRADA) = ORDEM_SERVICO-DATA_ENTRADA(10).
      REPLACE ALL OCCURRENCES OF '-' IN DT_ENTRADA WITH ''.
      HVI-DATA_ENTRADA = DT_ENTRADA.

      DATA(DT_ANALISE) = ORDEM_SERVICO-DATA_ANALISE(10).
      REPLACE ALL OCCURRENCES OF '-' IN DT_ANALISE WITH ''.
      HVI-DATA_ANALISE = DT_ANALISE.

      HVI-HVI_MIC      = WA_FARDO-MIC.
      HVI-HVI_POL      = WA_FARDO-POL.
      HVI-HVI_LEN      = WA_FARDO-LEN.
      HVI-HVI_STR      = WA_FARDO-STR.
      HVI-HVI_UNF      = WA_FARDO-UNF.
      HVI-HVI_ELG      = WA_FARDO-ELG.
      HVI-HVI_MAT      = WA_FARDO-MAT.
      HVI-HVI_RD       = WA_FARDO-RD.
      HVI-HVI_B        = WA_FARDO-B.
      HVI-HVI_CG       = WA_FARDO-CG.
      HVI-HVI_LEAF     = WA_FARDO-LEAF.
      HVI-HVI_ARE      = WA_FARDO-ARE.
      HVI-HVI_COUNT    = WA_FARDO-COUNT.
      HVI-HVI_SFI      = WA_FARDO-SFI.
      HVI-HVI_CSP      = WA_FARDO-CSP.
      HVI-HVI_SCI      = WA_FARDO-SCI.
      HVI-HVI_MAT_PORC = WA_FARDO-MAT_PORC.
      HVI-HVI_POL_FRAC = WA_FARDO-POL_FRAC.
      HVI-HVI_LOTE     = WA_FARDO-LOTE.
      HVI-HVI_PADRAO   = WA_FARDO-PADRAO.
      APPEND HVI TO IT_HVI.
    ENDLOOP.

    IF IT_HVI[] IS NOT INITIAL.
      MODIFY ZPP_KUHLMANN_HVI FROM TABLE IT_HVI.
      COMMIT WORK AND WAIT.
    ENDIF.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.
ENDCLASS.
