CLASS zcl_integracao_estrat_aprov DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_estrat_aprov .

    METHODS constructor
      IMPORTING
        !i_servico TYPE ztipowebserv
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_ESTRAT_APROV IMPLEMENTATION.


  METHOD constructor.
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_estrat_aprov.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD zif_integracao_estrat_aprov~get_instance.
    IF zif_integracao_estrat_aprov~at_if_integracao_estrat_aprov IS NOT BOUND.
      CREATE OBJECT zif_integracao_estrat_aprov~at_if_integracao_estrat_aprov
        TYPE zcl_integracao_estrat_aprov
        EXPORTING
          i_servico = i_servico. " Tipo de Serviço WebService
*        CATCH zcx_integracao.    " .
    ENDIF.

    r_if_integracao_estrat_aprov = zif_integracao_estrat_aprov~at_if_integracao_estrat_aprov.

  ENDMETHOD.


  METHOD zif_integracao_estrat_aprov~set_ds_data.

    r_if_integracao_estrat_aprov = me.

    me->zif_integracao_inject~at_info_request_http = i_info.
    /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = me->zif_integracao_estrat_aprov~at_consulta ).

*    RAISE EXCEPTION TYPE zcx_integracao
*      EXPORTING
*        textid = VALUE #( msgid = zcx_integracao=>zcx_erro_body_recebido-msgid
*                          msgno = zcx_integracao=>zcx_erro_body_recebido-msgno )
*        msgid  = zcx_integracao=>zcx_erro_body_recebido-msgid
*        msgno  = zcx_integracao=>zcx_erro_body_recebido-msgno
*        msgty  = 'E'.
  ENDMETHOD.


  METHOD zif_integracao_estrat_aprov~set_send_msg.
    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_if_integracao_estrat_aprov = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = '/ctb/estdoccontabil'.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_estrat_aprov~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

*    r_if_integracao_inject = me.
*
*    TRY .
*        CAST zcl_integracao_token_kuhlmann(
*               zcl_integracao_token_kuhlmann=>zif_integracao_token_kuhlmann~get_instance(
*                   i_servico = CONV #( me->zif_integracao_cli_kuhlmann~at_servico )
*                 )->set_ds_url(
*                 )->set_usuario_senha( "EXPORTING I_SENHA = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_SENHA I_USUARIO = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO
*                 )
*             )->zif_integracao_inject~get_header_request_http(
*          IMPORTING
*            e_header_fields = DATA(e_header_fields) ).
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


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.
    DATA: i_inbound  TYPE zde_cns_estrat_aprov,
          l_centro   TYPE werks,
          l_tipo     TYPE atwrt,
          l_kostl    TYPE kostl,
          t_return   TYPE TABLE OF zmme0111_t,
          l_username TYPE cdhdr-username,
          e_retorno  TYPE string,
          e_erro     TYPE string,
          t_result   TYPE zmme0111_t.

    TYPES: lr_bukrs TYPE RANGE OF bukrs,
           lr_werks TYPE fre_range_t_werks,
           lr_depar TYPE RANGE OF zdep_resp,
           lr_escri TYPE RANGE OF vkbur,
           lr_tvend TYPE zrange_t_atwrt,
           lr_kostl TYPE fagl_range_t_kostl.

    FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
                   <t_data_line> TYPE ANY TABLE,
                   <w_data>      TYPE any,
                   <w_data_line> TYPE any.

    DATA: l_data            TYPE REF TO data,
          l_data_line       TYPE REF TO data,
          l_data_descr      TYPE REF TO cl_abap_datadescr,
          l_data_line_descr TYPE REF TO cl_abap_datadescr.

    CONSTANTS l_dataate TYPE dats VALUE '99991231'.

    e_sucesso              = abap_false.
    e_nm_code              = abap_false.
    r_if_integracao_inject = me.


    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = i_inbound ).
    ENDIF.

    DATA(s_werks)  = VALUE lr_werks( sign = 'I' option = 'EQ' ( low = i_inbound-centrode ) ).
    IF i_inbound-centroate IS NOT INITIAL.
      s_werks  = VALUE lr_werks( sign = 'I' option = 'BT' ( low = i_inbound-centrode high = i_inbound-centroate ) ).
    ENDIF.

    DATA(s_bukrs)  = VALUE lr_bukrs( sign = 'I' option = 'EQ' ( low = i_inbound-empresa ) ).

    DATA(s_depart) = VALUE lr_depar( sign = 'I' option = 'EQ' ( low = i_inbound-departamento ) ).
    DATA(s_kostl) = VALUE lr_kostl( sign = 'I' option = 'EQ' ( low = i_inbound-centrocusto ) ).

    DATA(s_esc)    = VALUE lr_escri( sign = 'I' option = 'EQ' ( low = i_inbound-escritovendade ) ).

    DATA(s_tvenda) = VALUE lr_tvend( sign = 'I' option = 'EQ' ( low = i_inbound-tipovend ) ).


    CASE i_inbound-visao.
      WHEN 'ZMM0071'.

        CALL FUNCTION 'ZMMF0071_ESTRATEGIA_APROVACAO'
          EXPORTING
            i_centro = s_werks
            i_tipo   = s_tvenda
            i_ccusto = s_kostl
          IMPORTING
            t_result = t_result
            e_erro   = e_erro.

        IF t_result IS NOT INITIAL.
          e_msg_outbound = zcl_fmcall_base=>abap2json( EXPORTING abap_data = t_result ).
          e_sucesso      = abap_true.
        ENDIF.

      WHEN OTHERS.
        IF <t_data> IS ASSIGNED.
          CLEAR: <t_data>.
        ENDIF.

        IF <t_data_line> IS ASSIGNED.
          CLEAR: <t_data_line>.
        ENDIF.

        FREE: l_data,  l_data_line, l_data_descr,  l_data_line_descr.

        cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                                metadata = abap_false
                                                data     = abap_true ).


        CASE i_inbound-visao.
          WHEN 'ZIMP56'.
            SUBMIT zfir0071 WITH zimp = abap_true
                            WITH s_bukrs  IN s_bukrs
                            WITH s_depart IN s_depart
                            WITH s_werks  IN s_werks
                            WITH s_dataat = l_dataate AND RETURN.

          WHEN 'ZGL019'.
            SUBMIT zfir0071 WITH zimp = abap_false
                            WITH zgl = abap_true
                            WITH s_bukrs  IN s_bukrs
                            WITH s_depart IN s_depart
                            WITH s_werks  IN s_werks
                            WITH s_dataat = l_dataate AND RETURN.

          WHEN 'ZFI0019'.
            SUBMIT zfir0071 WITH zimp = abap_false
                            WITH inv = abap_true
                            WITH s_bukrs  IN s_bukrs
                            WITH s_esc    IN s_esc
                            WITH s_werks  IN s_werks
                            WITH s_dataat = l_dataate AND RETURN.

          WHEN 'ZFI0031'.
            SUBMIT zfir0071 WITH zimp = abap_false
                            WITH admt = abap_true
                            WITH s_bukrs  IN s_bukrs
                            WITH s_depart IN s_depart
                            WITH s_werks  IN s_werks
                            WITH s_dataat = l_dataate AND RETURN.
          WHEN 'ZSDT0117'.
            SUBMIT zfir0071 WITH zimp = abap_false
                            WITH lov = abap_true
                            WITH s_bukrs  IN s_bukrs
                            WITH s_esc    IN s_esc
                            WITH s_dataat = l_dataate AND RETURN.

          WHEN 'ZSDT0127'.
            SUBMIT zfir0071 WITH zimp = abap_false
                            WITH cred = abap_true
                            WITH s_bukrs  IN s_bukrs
                            WITH s_werks  IN s_werks
                            WITH s_dataat = l_dataate AND RETURN.

          WHEN 'ZLES0155'.
            SUBMIT zfir0071 WITH zimp = abap_false
                            WITH fret = abap_true
                            WITH s_bukrs  IN s_bukrs
                            WITH s_dataat = l_dataate AND RETURN.
          WHEN 'ZSDT0138'.
            SUBMIT zfir0071 WITH zimp = abap_false
                            WITH ovs = abap_true
                            WITH s_bukrs  IN s_bukrs
                            WITH s_tvenda IN s_tvenda
                            WITH s_esc    IN s_esc
                            WITH s_dataat = l_dataate AND RETURN.
          WHEN OTHERS.

        ENDCASE.
        TRY.
            cl_salv_bs_runtime_info=>get_data_ref(
            IMPORTING r_data_descr  = l_data_descr
                      r_data_line_descr = l_data_line_descr ).

            CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

            CREATE DATA l_data      TYPE HANDLE  l_data_descr.
            CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

            ASSIGN l_data->* TO <t_data>.

            cl_salv_bs_runtime_info=>get_data( IMPORTING t_data  = <t_data> ).

            cl_salv_bs_runtime_info=>clear_all( ).

            ASSIGN l_data->*        TO <w_data>.

            IF <t_data> IS ASSIGNED.
              e_msg_outbound = zcl_fmcall_base=>abap2json( EXPORTING abap_data = <t_data> ).
            ENDIF.
          CATCH cx_salv_bs_sc_runtime_info.
        ENDTRY.

    ENDCASE.
    IF e_erro IS NOT INITIAL.
      e_msg_outbound = '{ "mensagem" : "'     && e_erro   && '"' && cl_abap_char_utilities=>newline && ',' && '"ID_MSG" : "' && '404'    && '"' && cl_abap_char_utilities=>newline && '}'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
