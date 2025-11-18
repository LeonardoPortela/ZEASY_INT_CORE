class ZCL_INTEGRACAO_INSUMOS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INSUMOS .
  interfaces ZIF_INTEGRACAO_INBOUND .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_INSUMOS IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_insumos.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'INSUMOS'.

  ENDMETHOD.


  METHOD zif_integracao_inbound~configure_server.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

  ENDMETHOD.


  METHOD zif_integracao_inbound~processar_requisicao.

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

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~set_data.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_inbound.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: w_data_inbound  TYPE zsdt0314_sigam.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'POST'.
      r_msg_erro = 'Metodo informado não reconhecido!'.
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = w_data_inbound ).

*--------------------------------------
*-- validar
*--------------------------------------
    SELECT nr_doc_gerado
      INTO @DATA(w_0310)
      FROM zsdt0310
        UP TO 1 ROWS
     WHERE nr_doc_gerado = @w_data_inbound-nr_doc_gerado.
    ENDSELECT.

    IF sy-subrc <> 0.
      r_msg_erro = 'Documento SIGAM não foi existe no SAP!'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: t_data_inbound TYPE zsdt0314_sigam_t,
          w_data_inbound TYPE zsdt0314_sigam,
          w_zsdt0314     TYPE zsdt0314,
          l_status       TYPE zstatus_doc,
          l_tipo_msg     TYPE bapi_mtype,
          l_totregs      TYPE i,
          l_seq          TYPE i,
          t_partic       TYPE zsde0036_t,
          w_partic       TYPE zsde0036,
          l_mesg         TYPE string.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = w_data_inbound ).
    ENDIF.

    e_msg_erro = me->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound  ).

    IF e_msg_erro IS NOT INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

*--------------------------------------
*-- Processar
*--------------------------------------
    CHECK w_data_inbound-nr_doc_gerado IS NOT INITIAL.

    SELECT *
      INTO @DATA(w_0310)
      FROM zsdt0310
        UP TO 1 ROWS
     WHERE nr_doc_gerado = @w_data_inbound-nr_doc_gerado.
    ENDSELECT.

    APPEND w_0310  TO  me->zif_integracao_insumos~at_t_zsdt0310.

*-------------------------------
*---- atualizar status
*-------------------------------
    IF w_data_inbound-status = '1'.
      l_status = COND #( WHEN w_0310-tipo_doc_digital = 'N' THEN '05'
                                                            ELSE '02' ).

      UPDATE zsdt0310  SET status                = l_status
                           assinatura_sequencial = w_data_inbound-assinatura_sequencial
                           proibir_rejeicao      = w_data_inbound-proibir_rejeicao
                           data_doc_gerado       = sy-datum
                           hora_doc_gerado       = sy-uzeit
                     WHERE nr_venda              = w_0310-nr_venda
                       AND tipo_doc              = w_0310-tipo_doc
                       AND id_documento          = w_0310-id_documento.
    ELSE.
      UPDATE zsdt0310  SET status                = '11'
*                          tipo_doc_digital      = abap_off
*                          nr_doc_gerado         = abap_off
                           data_doc_gerado       = sy-datum
                           hora_doc_gerado       = sy-uzeit
                     WHERE nr_venda              = w_0310-nr_venda
                       AND tipo_doc              = w_0310-tipo_doc
                       AND id_documento          = w_0310-id_documento.
    ENDIF.

*-------------------------------
*-- atualizar participantes
*-------------------------------
    FREE: l_totregs, l_seq.

    t_partic[] = w_data_inbound-participantes[].

*-------------------------------
*-- gravar participantes
*-------------------------------
    me->zif_integracao_insumos~set_gravar_participantes( EXPORTING t_zsdt0310      = me->zif_integracao_insumos~at_t_zsdt0310
                                                                   t_participantes = t_partic ).

*-------------------------------
*-- gerar log
*-------------------------------
    CONDENSE w_data_inbound-log_mensagem.

    w_data_inbound-log_mensagem = COND #( WHEN w_data_inbound-log_mensagem IS INITIAL
                                                                           THEN 'Documento Gerado no SIGAM foi recepcionado pelo sistema SAP'
                                                                           ELSE w_data_inbound-log_mensagem ).
    l_mesg                      = COND #( WHEN w_data_inbound-status = '1' THEN |{ 'Sucesso: ' } { w_data_inbound-log_mensagem }|
                                                                           ELSE |{ 'Erro: '    } { w_data_inbound-log_mensagem }| ).
    l_tipo_msg                  = COND #( WHEN w_data_inbound-status = '1' THEN 'S'
                                                                           ELSE 'E' ).

    me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_0310-nr_venda
                                                         i_tipo_doc     = w_0310-tipo_doc
                                                         i_id_documento = w_0310-id_documento
                                                         i_tipo_msg     = l_tipo_msg
                                                         i_mensagem     = l_mesg ).

*-------------------------------
*-- atualizar tabela com PDF
*-------------------------------
    CLEAR w_zsdt0314.

    w_zsdt0314-nr_doc_gerado     = w_data_inbound-nr_doc_gerado.
    w_zsdt0314-id_doc_agrupador  = w_0310-id_documento.
    w_zsdt0314-pdf_doc_original  = w_data_inbound-pdf_doc_original.
    w_zsdt0314-data              = sy-datum.
    w_zsdt0314-hora              = sy-uzeit.

    MODIFY zsdt0314           FROM w_zsdt0314.

    COMMIT WORK AND WAIT.

*--------------------------------------
*-- Retorno processamento
*--------------------------------------
    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                     '   "status_code" : "' && e_nm_code                                             &&
                     '" '                   && cl_abap_char_utilities=>newline &&
                     ' }'.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_parametro.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_insumos~get_baixar_pdf_assinado.

    DATA: lo_envio   TYPE REF TO zcl_integracao_bry_adigital,
          lw_envio   TYPE zins_dados_bry_dados,
          w_zsdt0314 TYPE zsdt0314,
          t_anexos   TYPE zins_adigital_attachments.

    FREE: e_pdf_assinado.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_integracao_insumos = me.

    SELECT SINGLE *
             INTO w_zsdt0314
             FROM zsdt0314
            WHERE nr_doc_gerado    = i_nr_doc_gerado
              AND id_doc_agrupador = i_id_doc_agrupador.

    CHECK sy-subrc = 0.
    CHECK w_zsdt0314-chave_pdf_assinado IS NOT INITIAL.

*---------------------------
*-- montar campos API
*---------------------------
    lw_envio-metodo_envio                 = 'GET'.
    lw_envio-endpoint_get_pdf_assinado    = w_zsdt0314-chave_pdf_assinado.
    lw_envio-id_referencia                = w_zsdt0314-id_doc_agrupador.

*-----------------------------------------------------
*-- Envio BRY - OBTER documento assinado -------------
*-----------------------------------------------------
    TRY.
        CREATE OBJECT lo_envio
          EXPORTING
            it_anexos     = t_anexos
            i_dados_envio = lw_envio.

        lo_envio->zif_integracao_bry_adigital~enviar_bry( IMPORTING e_jsonx = e_pdf_assinado ).


      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error      INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

*-------------------------------
*-- atualiza PDF assinado
*-------------------------------
    UPDATE zsdt0314 SET doc_pdf_assinado = e_pdf_assinado
                  WHERE nr_doc_gerado    = i_nr_doc_gerado
                    AND id_doc_agrupador = i_id_doc_agrupador.

*-------------------------------
*-- atualiza status
*-------------------------------
    SELECT SINGLE status
      INTO @DATA(l_status)
      FROM zsdt0310
     WHERE id_documento = @i_id_doc_agrupador.

    IF l_status <> '13'. "cancelado
      UPDATE zsdt0310 SET status           = '06'
                    WHERE id_documento     = i_id_doc_agrupador.
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_integracao_insumos~get_id_referencia.

    r_if_integracao_insumos    = me.
    e_referencia-tp_referencia = 'INSUMOS'.
    e_referencia-id_referencia = me->zif_integracao_insumos~at_id_referencia.

  ENDMETHOD.


  METHOD zif_integracao_insumos~get_instance.

    IF zif_integracao_insumos~at_if_integracao_insumos IS NOT BOUND.
      CREATE OBJECT zif_integracao_insumos~at_if_integracao_insumos
        TYPE zcl_integracao_insumos.
    ENDIF.
    r_if_integracao_insumos = zif_integracao_insumos~at_if_integracao_insumos.

  ENDMETHOD.


  METHOD zif_integracao_insumos~get_metodo.
  ENDMETHOD.


  METHOD zif_integracao_insumos~get_obter_participantes.

    DATA: l_erro TYPE char1.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_integracao_insumos                     = me.
    me->zif_integracao_insumos~at_nr_venda      = i_nr_venda.
    me->zif_integracao_insumos~at_tipo_doc      = i_tipo_doc.
    me->zif_integracao_insumos~at_id_documento  = i_id_documento.

*-------------------------------
*-- dados gerais
*-------------------------------
    SELECT *
      FROM zsdt0310
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0310
     WHERE nr_venda      = me->zif_integracao_insumos~at_nr_venda
       AND tipo_doc      = me->zif_integracao_insumos~at_tipo_doc
       AND id_documento  = me->zif_integracao_insumos~at_id_documento
       AND status       <> me->zif_integracao_insumos~at_cancelado.

    IF sy-subrc <> 0.
      me->zif_integracao_insumos~set_mensagem( '01' ).
    ENDIF.

*-------------------------------
*-- dados emitentes
*-------------------------------
    SELECT *
      FROM zsdt0311
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0311
       FOR ALL ENTRIES IN me->zif_integracao_insumos~at_t_zsdt0310
     WHERE id_documento  = me->zif_integracao_insumos~at_t_zsdt0310-id_documento.

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
           )->set_exec_insumos( EXPORTING i_metodo = 'OBTER_PARTICIPANTES' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_insumos~get_obter_pdf_assinado.

    DATA: l_status     TYPE char30,
          l_mesg       TYPE string,
          l_tabix      TYPE sy-tabix,
          w_docassin   TYPE zins_bry_in_doc_assinados,
          t_assinantes TYPE zsde0075_t,
          w_assinantes TYPE zsde0075.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_integracao_insumos = me.

*-------------------------------
*-- obtem pdf documento
*-------------------------------
    SELECT id_doc_agrupador, nr_doc_gerado, chave_workflow
      INTO TABLE @DATA(t_0314)
      FROM zsdt0314.

    READ TABLE t_0314 INTO DATA(w_0314) WITH KEY chave_workflow = i_data-chave.

    CHECK sy-subrc = 0.

*-------------------------------
*-- obtem pdf assinado e atualiza
*-------------------------------
    READ TABLE i_data-documentosassinados     INTO w_docassin INDEX 1.

    SELECT SINGLE *
      INTO @DATA(w_0310)
      FROM zsdt0310
     WHERE id_documento  = @w_0314-id_doc_agrupador
       AND status       <> '10'.

    l_status = COND #( WHEN i_data-status(4) = 'CONC' THEN '04'
                       WHEN i_data-status(4) = 'REJE' THEN '13'
                       WHEN i_data-status(4) = 'CANC' THEN '10' ).

    UPDATE zsdt0314 SET chave_pdf_assinado       = w_docassin-chave
                        url_pdf_assinado         = w_docassin-link
                        url_relatorio_assinatura = i_data-protocoloassinatura-linkrelatorio
                  WHERE nr_doc_gerado            = w_0314-nr_doc_gerado
                    AND id_doc_agrupador         = w_0314-id_doc_agrupador.

    UPDATE zsdt0310 SET status                   = l_status
                  WHERE id_documento             = w_0310-id_documento.

*-----------------------------
*-- mensagem start
*-----------------------------
    l_mesg = COND #( WHEN l_status = '04' THEN 'Recebido Documento Assinado.'
                                          ELSE 'Rejeição de Coleta por algum Assinante' ).

    me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_0310-nr_venda
                                                         i_tipo_doc     = w_0310-tipo_doc
                                                         i_id_documento = w_0310-id_documento
                                                         i_tipo_msg     = 'S'
                                                         i_mensagem     = l_mesg ).

*-----------------------------
* Obter status assinantess
*-----------------------------
    TRY .
        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
           )->get_obter_status_assinatura( EXPORTING i_id_documento       = w_0310-id_documento
                                           IMPORTING t_status_assinaturas = t_assinantes ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        l_mesg = 'Não foi possivel obter Status das Assinaturas dos Participantes!'.
        me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_0310-nr_venda
                                                             i_tipo_doc     = w_0310-tipo_doc
                                                             i_id_documento = w_0310-id_documento
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg ).

      CATCH zcx_error INTO DATA(ex_error).
        l_mesg = 'Não foi possivel obter Status das Assinaturas dos Participantes!'.
        me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_0310-nr_venda
                                                             i_tipo_doc     = w_0310-tipo_doc
                                                             i_id_documento = w_0310-id_documento
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg ).
    ENDTRY.

*-----------------------------
*-- Atualizar status assinantes
*-----------------------------
    SELECT *
      INTO TABLE @DATA(t_0316)
      FROM zsdt0316
     WHERE nr_doc_gerado     = @w_0314-nr_doc_gerado
       AND id_doc_agrupador  = @w_0314-id_doc_agrupador.

    LOOP AT t_assinantes INTO w_assinantes.

      CHECK w_assinantes-situacaoassinatura-chave IS NOT INITIAL.

      LOOP AT t_0316 INTO DATA(w_0316) WHERE codigo = w_assinantes-codigo.
        l_tabix = sy-tabix.

        UPDATE zsdt0316   SET status_assinatura = w_assinantes-situacaoassinatura-chave
                        WHERE nr_doc_gerado     = w_0316-nr_doc_gerado
                          AND id_doc_agrupador  = w_0316-id_doc_agrupador
                          AND sequenciador      = w_0316-sequenciador.

        DELETE t_0316 INDEX l_tabix.
      ENDLOOP.

*-----------------------------
*---- registrar log
*-----------------------------
      l_mesg = |{ 'Status Assinatura' } { w_assinantes-nome } : { w_assinantes-situacaoassinatura-chave }|.

      me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_0310-nr_venda
                                                           i_tipo_doc     = w_0310-tipo_doc
                                                           i_id_documento = w_0310-id_documento
                                                           i_tipo_msg     = 'S'
                                                           i_mensagem     = l_mesg ).
    ENDLOOP.

*-----------------------------
*-- mensagem start
*-----------------------------
    l_mesg = COND #( WHEN l_status = '04' THEN 'Processo de Assinatura Finalizado.'
                                          ELSE 'Coleta deverá ser Cancelada.' ).

    me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_0310-nr_venda
                                                         i_tipo_doc     = w_0310-tipo_doc
                                                         i_id_documento = w_0310-id_documento
                                                         i_tipo_msg     = 'S'
                                                         i_mensagem     = l_mesg ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_integracao_insumos~get_obter_status_assinatura.

    DATA: lo_envio             TYPE REF TO zcl_integracao_bry_adigital,
          lw_envio             TYPE zins_dados_bry_dados,
          w_zsdt0310           TYPE zsdt0310,
          w_zsdt0314           TYPE zsdt0314,
          l_status_assinaturas TYPE string,
          t_anexos             TYPE zins_adigital_attachments.

    FREE: t_status_assinaturas.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_integracao_insumos = me.

    SELECT SINGLE *
             INTO w_zsdt0310
             FROM zsdt0310
            WHERE id_documento = i_id_documento.

    IF sy-subrc <> 0.
      me->zif_integracao_insumos~set_mensagem( '01' ).
    ENDIF.

    SELECT SINGLE *
             INTO w_zsdt0314
             FROM zsdt0314
            WHERE nr_doc_gerado    = w_zsdt0310-nr_doc_gerado
              AND id_doc_agrupador = w_zsdt0310-id_documento.

    IF sy-subrc <> 0.
      me->zif_integracao_insumos~set_mensagem( '01' ).
    ENDIF.

    IF w_zsdt0314-chave_workflow IS INITIAL.
      me->zif_integracao_insumos~set_mensagem( '02' ).
    ENDIF.

*---------------------------
*-- montar campos API
*---------------------------
    lw_envio-metodo_envio                 = 'GET'.
    lw_envio-endpoint_get_participantes   = w_zsdt0314-chave_workflow.
    lw_envio-id_referencia                = w_zsdt0314-id_doc_agrupador.

*-----------------------------------------------------
*-- Envio BRY - OBTER documento assinado -------------
*-----------------------------------------------------
    TRY.
        CREATE OBJECT lo_envio
          EXPORTING
            it_anexos     = t_anexos
            i_dados_envio = lw_envio.

        l_status_assinaturas = lo_envio->zif_integracao_bry_adigital~enviar_bry( ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error      INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

*---------------------------------------
*---obter partucipantes / ststus assinaturas
*---------------------------------------
    /ui2/cl_json=>deserialize( EXPORTING json = l_status_assinaturas
                               CHANGING  data = t_status_assinaturas ).

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_cancelar_coleta.

    DATA: lo_envio   TYPE REF TO zcl_integracao_bry_adigital,
          lw_envio   TYPE zins_dados_bry_dados,
          w_zsdt0310 TYPE zsdt0310,
          w_zsdt0314 TYPE zsdt0314,
          l_mesg     TYPE string,
          l_status   TYPE string,
          l_retorno  TYPE zsde0077,
          t_anexos   TYPE zins_adigital_attachments.

    FREE: l_status.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_integracao_insumos = me.

    SELECT SINGLE *
             INTO w_zsdt0310
             FROM zsdt0310
            WHERE id_documento = i_id_documento.

    IF sy-subrc <> 0.
      me->zif_integracao_insumos~set_mensagem( '01' ).
    ENDIF.

    SELECT SINGLE *
             INTO w_zsdt0314
             FROM zsdt0314
            WHERE nr_doc_gerado    = w_zsdt0310-nr_doc_gerado
              AND id_doc_agrupador = w_zsdt0310-id_documento.

    IF sy-subrc <> 0.
      me->zif_integracao_insumos~set_mensagem( '01' ).
    ENDIF.

*   IF w_zsdt0314-chave_workflow IS INITIAL.
*     me->zif_integracao_insumos~set_mensagem( '02' ).
*   ENDIF.

    CHECK w_zsdt0314-chave_workflow IS NOT INITIAL.

    l_mesg = 'Cancelamento da Coleta foi Solicitada.'.
    me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_zsdt0310-nr_venda
                                                         i_tipo_doc     = w_zsdt0310-tipo_doc
                                                         i_id_documento = w_zsdt0310-id_documento
                                                         i_tipo_msg     = 'S'
                                                         i_mensagem     = l_mesg ).

*---------------------------
*-- montar campos API
*---------------------------
    lw_envio-metodo_envio                 = 'POST'.
    lw_envio-endpoint_cancelar_coletas    = w_zsdt0314-chave_workflow.
    lw_envio-id_referencia                = w_zsdt0314-id_doc_agrupador.

*-----------------------------------------------------
*-- Envio BRY - OBTER documento assinado -------------
*-----------------------------------------------------
    TRY.
        CREATE OBJECT lo_envio
          EXPORTING
            it_anexos     = t_anexos
            i_dados_envio = lw_envio.

        l_status = lo_envio->zif_integracao_bry_adigital~enviar_bry( ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        l_mesg = 'Não foi possível efetuar o Cancelamento da Coleta.'.
        me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_zsdt0310-nr_venda
                                                             i_tipo_doc     = w_zsdt0310-tipo_doc
                                                             i_id_documento = w_zsdt0310-id_documento
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg ).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error      INTO DATA(ex_error).
        l_mesg = 'Não foi possível efetuar o Cancelamento da Coleta.'.
        me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_zsdt0310-nr_venda
                                                             i_tipo_doc     = w_zsdt0310-tipo_doc
                                                             i_id_documento = w_zsdt0310-id_documento
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg ).

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

*---------------------------------------
*-- retorno cncelamento
*---------------------------------------
    /ui2/cl_json=>deserialize( EXPORTING json = l_status
                               CHANGING  data = l_retorno ).

    IF l_status IS INITIAL.
      l_mesg = 'Não foi possível efetuar o Cancelamento da Coleta.'.
      me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_zsdt0310-nr_venda
                                                           i_tipo_doc     = w_zsdt0310-tipo_doc
                                                           i_id_documento = w_zsdt0310-id_documento
                                                           i_tipo_msg     = 'E'
                                                           i_mensagem     = l_mesg ).
    ELSE.
*---------------------------------------
*---- atualiza status coleta
*---------------------------------------
      IF i_cancelar_documento = abap_off.
        UPDATE zsdt0310  SET status           = '00'
                             tipo_doc_digital = abap_off
                             nr_doc_gerado    = abap_off
                             usnam_canc       = sy-uname
                             data_canc        = sy-datum
                             hora_canc        = sy-uzeit
                             hora_doc_gerado  = sy-uzeit
                       WHERE id_documento     = i_id_documento.
      ENDIF.

      l_mesg = 'Cancelamento da Coleta efetuado com Sucesso!'.
      me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = w_zsdt0310-nr_venda
                                                           i_tipo_doc     = w_zsdt0310-tipo_doc
                                                           i_id_documento = w_zsdt0310-id_documento
                                                           i_tipo_msg     = 'S'
                                                           i_mensagem     = l_mesg ).
    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_cancelar_doc_sigam.

    DATA: l_erro TYPE char1,
          l_mesg TYPE string.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_integracao_insumos                     = me.
    me->zif_integracao_insumos~at_id_documento  = i_id_documento.

*-------------------------------
*-- dados gerais
*-------------------------------
    SELECT *
      FROM zsdt0310
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0310
     WHERE id_documento  = me->zif_integracao_insumos~at_id_documento.

    IF sy-subrc <> 0.
      me->zif_integracao_insumos~set_mensagem( '01' ).
    ENDIF.

    READ TABLE me->zif_integracao_insumos~at_t_zsdt0310 INTO DATA(w_0310) INDEX 1.

    CHECK w_0310-nr_doc_gerado IS NOT INITIAL.

    me->zif_integracao_insumos~at_nr_venda      = w_0310-nr_venda.
    me->zif_integracao_insumos~at_tipo_doc      = w_0310-tipo_doc.
    me->zif_integracao_insumos~at_id_documento  = w_0310-id_documento.

*-------------------------------
*-- monta json
*-------------------------------
    me->zif_integracao_insumos~at_json = '{"nr_documento":' && w_0310-nr_doc_gerado && '}'.

    l_mesg        = | { 'Iniciando Cancelamento no SIGAM.' } |.
    me->zif_integracao_insumos~set_gravar_log(           EXPORTING i_nr_venda      = me->zif_integracao_insumos~at_nr_venda
                                                                   i_tipo_doc      = me->zif_integracao_insumos~at_tipo_doc
                                                                   i_id_documento  = me->zif_integracao_insumos~at_id_documento
                                                                   i_mensagem      = l_mesg ).
*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
           )->set_exec_insumos( EXPORTING i_metodo = 'CANCELA_DOC_SIGAM' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_criar_documento.
*&-------------------------------------------------------------------&*
*&                    Histórico de Modificações                      &*
*& Autor ABAP |Request    |Data       |Descrição                     &*
*&-------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações p/ geração Contrato&*
*&                                    |Compra.                       &*
*&                                    |Chamado: 168919 2ª Parte.     &*
*&-------------------------------------------------------------------&*

    DATA: l_erro TYPE char1.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_integracao_insumos                     = me.
    me->zif_integracao_insumos~at_nr_venda      = i_nr_venda.
    me->zif_integracao_insumos~at_tipo_doc      = i_tipo_doc.
    me->zif_integracao_insumos~at_id_documento  = i_id_documento.

*-------------------------------
*-- dados gerais
*-------------------------------
    SELECT *
      FROM zsdt0310
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0310
     WHERE nr_venda      = me->zif_integracao_insumos~at_nr_venda
       AND tipo_doc      = me->zif_integracao_insumos~at_tipo_doc
       AND id_documento  = me->zif_integracao_insumos~at_id_documento
       AND status       <> me->zif_integracao_insumos~at_cancelado.

    IF sy-subrc <> 0.
      me->zif_integracao_insumos~set_mensagem( '01' ).
    ENDIF.

*-------------------------------
*-- dados emitentes
*-------------------------------
    SELECT *
      FROM zsdt0311
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0311
       FOR ALL ENTRIES IN me->zif_integracao_insumos~at_t_zsdt0310
     WHERE id_documento  = me->zif_integracao_insumos~at_t_zsdt0310-id_documento.

*-------------------------------
*-- dados itens
*-------------------------------
    SELECT *
      FROM zsdt0312
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0312
       FOR ALL ENTRIES IN me->zif_integracao_insumos~at_t_zsdt0310
     WHERE id_documento  = me->zif_integracao_insumos~at_t_zsdt0310-id_documento.
**<<<------"168919 - NMS - INI------>>>
*-------------------------------
*-- dados parcelas
*-------------------------------
    SELECT *
      FROM zsdt0013
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0013
       FOR ALL ENTRIES IN me->zif_integracao_insumos~at_t_zsdt0310
     WHERE id_documento EQ me->zif_integracao_insumos~at_t_zsdt0310-id_documento.

*-------------------------------
*-- dados royalties
*-------------------------------
    SELECT *
      FROM zsdt0014
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0014
       FOR ALL ENTRIES IN me->zif_integracao_insumos~at_t_zsdt0310
     WHERE id_documento EQ me->zif_integracao_insumos~at_t_zsdt0310-id_documento.

*-------------------------------
*-- dados testemunhas
*-------------------------------
    SELECT *
      FROM zsdt0015
      INTO TABLE me->zif_integracao_insumos~at_t_zsdt0015
       FOR ALL ENTRIES IN me->zif_integracao_insumos~at_t_zsdt0310
     WHERE id_documento EQ me->zif_integracao_insumos~at_t_zsdt0310-id_documento.
**<<<------"168919 - NMS - FIM------>>>
*-------------------------------
*-- Monta JSON
*-------------------------------
    me->zif_integracao_insumos~set_json_criar_docto( ).

*-------------------------------
*-- Executa API
*-------------------------------
    TRY .
        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
           )->set_exec_insumos( EXPORTING i_metodo = 'CRIAR_DOCTO' ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error INTO DATA(ex_error).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_ds_data.

    r_if_integracao_insumos = me.

*---------------------------------------
*---types
*---------------------------------------
    TYPES BEGIN OF ty_retorno.
    TYPES: access TYPE string.
    TYPES: expiresin TYPE string.
    TYPES: userid TYPE string.
    TYPES END OF ty_retorno.

*---------------------------------------
*---workarea
*---------------------------------------
    DATA: l_access_token TYPE string,
          l_token_type   TYPE string,
          l_expires_in   TYPE string,
          l_token        TYPE string,
          lc_retorno     TYPE ty_retorno.

    FREE: me->zif_integracao_inject~at_header_fields.

    CASE me->zif_integracao_insumos~at_metodo.
      WHEN 'TOKEN'.
        me->zif_integracao_inject~at_info_request_http-ds_body         = '{"module" : "'  && me->zif_integracao_insumos~at_webservice-add01    && '",' &&
                                                                         '"username" : "' && me->zif_integracao_insumos~at_webservice-username && '",' &&
                                                                         '"password" : "' && me->zif_integracao_insumos~at_webservice-password && '"}'.
        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

      WHEN 'CRIAR_DOCTO'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access.
        l_expires_in   = lc_retorno-expiresin.
        l_token        = |{ 'Bearer' } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )                                           TO me->zif_integracao_inject~at_header_fields.
*       APPEND VALUE #( name = 'Subscription-Key' value = me->zif_integracao_insumos~at_webservice-add02 )    TO me->zif_integracao_inject~at_header_fields.
*       APPEND VALUE #( name = 'X-Tenant'         value = me->zif_integracao_insumos~at_webservice-add01 )    TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_insumos~at_webservice-content_type.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_integracao_insumos~at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

      WHEN 'OBTER_PARTICIPANTES_CLIENTE' OR 'OBTER_PARTICIPANTES_FILIAL'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access.
        l_expires_in   = lc_retorno-expiresin.
        l_token        = |{ 'Bearer' } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )                                           TO me->zif_integracao_inject~at_header_fields.
*       APPEND VALUE #( name = 'Subscription-Key' value = me->zif_integracao_insumos~at_webservice-add02 )    TO me->zif_integracao_inject~at_header_fields.
*       APPEND VALUE #( name = 'X-Tenant'         value = me->zif_integracao_insumos~at_webservice-add01 )    TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_insumos~at_webservice-content_type.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_integracao_insumos~at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'GET'.

      WHEN 'CANCELA_DOC_SIGAM'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        l_access_token = lc_retorno-access.
        l_expires_in   = lc_retorno-expiresin.
        l_token        = |{ 'Bearer' } { l_access_token }|.

        APPEND VALUE #( name = 'Authorization'    value = l_token )                                           TO me->zif_integracao_inject~at_header_fields.
*       APPEND VALUE #( name = 'Subscription-Key' value = me->zif_integracao_insumos~at_webservice-add02 )    TO me->zif_integracao_inject~at_header_fields.
*       APPEND VALUE #( name = 'X-Tenant'         value = me->zif_integracao_insumos~at_webservice-add01 )    TO me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_insumos~at_webservice-content_type.
        me->zif_integracao_inject~at_info_request_http-ds_body         = me->zif_integracao_insumos~at_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_ds_url.

    r_if_integracao_insumos = me.

    DATA: l_url     TYPE string,
          l_data    TYPE string,
          l_ativo   TYPE string,
          l_servico TYPE string.

    CASE i_metodo.
      WHEN 'TOKEN'.
        l_servico = 'SIGAM_TOKEN_DOCS_ISUMOS'.
      WHEN 'CRIAR_DOCTO'.
        l_servico = 'SIGAM_CRIA_DOCS_INSUMOS'.
      WHEN 'OBTER_PARTICIPANTES_CLIENTE' OR 'OBTER_PARTICIPANTES_FILIAL'.
        l_servico = 'SIGAM_PARTICIPANTES_DOCS_ISUMOS'.
      WHEN 'CANCELA_DOC_SIGAM'.
        l_servico = 'SIGAM_CANCELAR_DOCS_INSUMOS'.
    ENDCASE.

    SELECT SINGLE *
             FROM zauth_webservice
             INTO @DATA(wa_webservice)
            WHERE service = @l_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'T'
                            attr2 = 'TC' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'T'
          msgv2  = 'TC'.
    ENDIF.

    me->zif_integracao_insumos~at_webservice = wa_webservice.
    me->zif_integracao_insumos~at_metodo     = i_metodo.

    CASE i_metodo.
      WHEN 'TOKEN'.
        l_url = wa_webservice-url_token.

      WHEN 'CRIAR_DOCTO'.
        l_url = wa_webservice-url && '?nr_documento=1'.

      WHEN 'OBTER_PARTICIPANTES_CLIENTE'.
        READ TABLE me->zif_integracao_insumos~at_t_zsdt0310 INTO DATA(w_0310) INDEX 1.
        READ TABLE me->zif_integracao_insumos~at_t_zsdt0311 INTO DATA(w_0311) WITH KEY tp_emitente = 'EMITENTE'.
        l_url = wa_webservice-url && '?cod_Modelo=' && wa_webservice-add01 && '&id_filial_sap=' && w_0310-vkbur && '&id_cliente_sap=' && w_0311-kunnr.

      WHEN 'OBTER_PARTICIPANTES_FILIAL'.
        READ TABLE me->zif_integracao_insumos~at_t_zsdt0310 INTO      w_0310  INDEX 1.
        l_url = wa_webservice-url && '?cod_Modelo=691&id_filial_sap='                           && w_0310-vkbur && '&id_cliente_sap='.

      WHEN 'CANCELA_DOC_SIGAM'.
        l_url = wa_webservice-url.

    ENDCASE.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
*   me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token        = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url              = l_url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_insumos~set_id_referencia( ).

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_exec_insumos.

*---------------------------------------
* types
*---------------------------------------
    TYPES: BEGIN OF ty_metodo,
             metodo TYPE string.
    TYPES: END   OF ty_metodo.

*-#159957-02.12.2024-JT-inicio
    TYPES BEGIN OF ty_retorno.
    TYPES: access TYPE string.
    TYPES: expiresin TYPE string.
    TYPES: userid TYPE string.
    TYPES END OF ty_retorno.
*-#159957-02.12.2024-JT-fim

*---------------------------------------
* workarea
*---------------------------------------
    DATA: lc_integracao       TYPE zintegracao,
          lc_integracao_token TYPE zintegracao,
          l_error             TYPE c,
          l_mesg              TYPE string,
          l_token             TYPE string,           "*-#159957-02.12.2024-JT-inicio
          lv_token            TYPE zintegracao0001,  "*-#159957-02.12.2024-JT-inicio
          lc_retorno          TYPE ty_retorno,       "*-#159957-02.12.2024-JT-inicio
          l_segundos          TYPE i,                "*-#159957-02.12.2024-JT-inicio
          lc_segundos         TYPE char100,          "*-#159957-02.12.2024-JT-inicio
          lv_timestamp        TYPE timestamp,        "*-#159957-02.12.2024-JT-inicio
          lv_data_atual       TYPE sy-datum,         "*-#159957-02.12.2024-JT-inicio
          lv_hora_atual       TYPE sy-uzeit,         "*-#159957-02.12.2024-JT-inicio
          l_totregs           TYPE i,
          l_seq               TYPE i,
          t_metodo            TYPE TABLE OF ty_metodo,
          w_metodo            TYPE ty_metodo,
          lc_docto            TYPE zsde0047,
          w_zsdt0316          TYPE zsdt0316,
*         t_partic            TYPE zsde0036_t,
          w_partic            TYPE zsde0078. "36zsde0036.

*---------------------------------------
* inicio processo
*---------------------------------------
    r_if_integracao_insumos = me.

    FREE: t_metodo,
          lc_integracao,
          l_error,
          me->zif_integracao_inject~at_header_fields,
          me->zif_integracao_inject~at_info_request_http.

    me->zif_integracao_inject~at_tp_integracao  = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_insumos~at_id_referencia = me->zif_integracao_insumos~at_nr_venda && '-' &&  me->zif_integracao_insumos~at_tipo_doc.

*---------------------------------------
* metodos chamada
*---------------------------------------
    w_metodo-metodo     = 'TOKEN'.
    APPEND w_metodo    TO t_metodo.

    IF i_metodo = 'OBTER_PARTICIPANTES'.
      w_metodo-metodo   = 'OBTER_PARTICIPANTES_CLIENTE'.
      APPEND w_metodo  TO t_metodo.
*     w_metodo-metodo   = 'OBTER_PARTICIPANTES_FILIAL'.
*     APPEND w_metodo  TO t_metodo.
    ELSE.
      w_metodo-metodo   = i_metodo.
      APPEND w_metodo  TO t_metodo.
    ENDIF.

*---------------------------------------

*---------------------------------------
*---buscar token / metodo
*---------------------------------------
    LOOP AT t_metodo INTO w_metodo.

      TRY.
*-#159957-02.12.2024-JT-inicio
          IF w_metodo-metodo = 'TOKEN'.
            DATA(lt_header) = zcl_gestao_token=>get_token_valido( i_id_token = '0007' ).

            IF lt_header[] IS NOT INITIAL.
              READ TABLE lt_header INTO DATA(wl_header) INDEX 1.
              SPLIT wl_header-value AT abap_off INTO DATA(lc_constant) l_token.
              lc_integracao-ds_data_retorno = '{"access":"' && l_token && '"}'.
              lc_integracao_token           = lc_integracao.
              CONTINUE.
            ENDIF.
          ENDIF.
*-#159957-02.12.2024-JT-fim

          me->zif_integracao_insumos~set_ds_url(
                             EXPORTING i_metodo        = w_metodo-metodo
            )->set_ds_data(  EXPORTING i_integracao    = lc_integracao
            )->set_send_msg( IMPORTING e_id_integracao = DATA(lc_id_integracao)
                                       e_integracao	   = lc_integracao
            ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_error = abap_true.
          l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.
          me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = me->zif_integracao_insumos~at_nr_venda
                                                               i_tipo_doc     = me->zif_integracao_insumos~at_tipo_doc
                                                               i_id_documento = me->zif_integracao_insumos~at_id_documento
                                                               i_tipo_msg     = 'E'
                                                               i_mensagem     = l_mesg ).

          RAISE EXCEPTION TYPE zcx_integracao
            EXPORTING
              textid = VALUE #( msgid = ex_integra->msgid
                                msgno = ex_integra->msgno
                                attr1 = CONV #( ex_integra->msgv1 )
                                attr2 = CONV #( ex_integra->msgv2 )
                                attr3 = CONV #( ex_integra->msgv3 )
                                attr4 = CONV #( ex_integra->msgv4 ) )
              msgid  = ex_integra->msgid
              msgno  = ex_integra->msgno
              msgty  = 'E'
              msgv1  = CONV #( ex_integra->msgv1 )
              msgv2  = CONV #( ex_integra->msgv2 )
              msgv3  = CONV #( ex_integra->msgv3 )
              msgv4  = CONV #( ex_integra->msgv4 ).

        CATCH zcx_error      INTO DATA(ex_error).    "  "
          l_error = abap_true.
          l_mesg  = ex_error->msgv1 && '-' && ex_error->msgv2 && '-' && ex_error->msgv3 && '-' && ex_error->msgv4.
          me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = me->zif_integracao_insumos~at_nr_venda
                                                               i_tipo_doc     = me->zif_integracao_insumos~at_tipo_doc
                                                               i_id_documento = me->zif_integracao_insumos~at_id_documento
                                                               i_tipo_msg     = 'E'
                                                               i_mensagem     = l_mesg ).

          RAISE EXCEPTION TYPE zcx_error
            EXPORTING
              textid = VALUE #( msgid = ex_error->msgid
                                msgno = ex_error->msgno
                                attr1 = CONV #( ex_error->msgv1 )
                                attr2 = CONV #( ex_error->msgv2 )
                                attr3 = CONV #( ex_error->msgv3 )
                                attr4 = CONV #( ex_error->msgv4 ) )
              msgid  = ex_error->msgid
              msgno  = ex_error->msgno
              msgty  = 'E'
              msgv1  = CONV #( ex_error->msgv1 )
              msgv2  = CONV #( ex_error->msgv2 )
              msgv3  = CONV #( ex_error->msgv3 )
              msgv4  = CONV #( ex_error->msgv4 ).
      ENDTRY.

      CHECK l_error = abap_false.

*---------------------------------------
*---avalia retorno JSON
*---------------------------------------
      CASE w_metodo-metodo.

        WHEN 'TOKEN'.
          lc_integracao_token = lc_integracao.

*-#159957-02.12.2024-JT-inicio
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno CHANGING data = lc_retorno ).

*-------- Data/hora expiracao token JSON (UTC Brasilia)
          DATA(lv_data_expir) = lc_retorno-expiresin(4)    && lc_retorno-expiresin+5(2)  && lc_retorno-expiresin+8(2).
          DATA(lv_hora_expir) = lc_retorno-expiresin+11(2) && lc_retorno-expiresin+14(2) && lc_retorno-expiresin+17(2).

*-------- Data/hora atual (UTC Brasilia)
          GET TIME STAMP FIELD lv_timestamp.
          CONVERT TIME STAMP lv_timestamp TIME ZONE 'BRAZIL'  INTO DATE lv_data_atual  TIME lv_hora_atual.

          cl_abap_tstmp=>td_subtract( EXPORTING date1    = CONV #( lv_data_expir )
                                                time1    = CONV #( lv_hora_expir )
                                                date2    = CONV #( lv_data_atual )
                                                time2    = CONV #( lv_hora_atual )
                                      IMPORTING res_secs = l_segundos ).

          lc_segundos = abs( l_segundos ).
          CONDENSE lc_segundos.

          lv_token-id_token     = '0007'.
          lv_token-access_token = lc_retorno-access.
          lv_token-token_type   = 'Bearer'.
          lv_token-expires_in   = lc_segundos.
          zcl_gestao_token=>update_token( i_token = lv_token ).
*-#159957-02.12.2024-JT-fim

        WHEN 'CRIAR_DOCTO'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = lc_docto ).

*         IF lc_docto-success           IS INITIAL OR
*            lc_docto-data-nr_documento IS INITIAL.
          IF lc_docto-nr_documento IS INITIAL.
            l_mesg  = 'Número documento no SIGAM não foi gerado.'.
            me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = me->zif_integracao_insumos~at_nr_venda
                                                                 i_tipo_doc     = me->zif_integracao_insumos~at_tipo_doc
                                                                 i_id_documento = me->zif_integracao_insumos~at_id_documento
                                                                 i_tipo_msg     = 'E'
                                                                 i_mensagem     = l_mesg ).

            RAISE EXCEPTION TYPE zcx_error
              EXPORTING
                textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                                  msgno = zcx_error=>zcx_erro_geral-msgno
                                  attr1 = CONV #( l_mesg ) )
                msgid  = zcx_error=>zcx_erro_geral-msgid
                msgno  = zcx_error=>zcx_erro_geral-msgno
                msgty  = 'E'
                msgv1  = CONV #( l_mesg ).
          ENDIF.

          UPDATE zsdt0310  SET nr_doc_gerado   = lc_docto-nr_documento
                               data_doc_gerado = sy-datum
                               hora_doc_gerado = sy-uzeit
                               status          = '01'
                         WHERE nr_venda        = me->zif_integracao_insumos~at_nr_venda
                           AND tipo_doc        = me->zif_integracao_insumos~at_tipo_doc
                           AND id_documento    = me->zif_integracao_insumos~at_id_documento.

          l_mesg = | { 'Recebidos dados para gerar o Documento número' } { lc_docto-nr_documento } |.

          me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = me->zif_integracao_insumos~at_nr_venda
                                                               i_tipo_doc     = me->zif_integracao_insumos~at_tipo_doc
                                                               i_id_documento = me->zif_integracao_insumos~at_id_documento
                                                               i_mensagem     = l_mesg ).
          COMMIT WORK AND WAIT.

        WHEN 'OBTER_PARTICIPANTES_CLIENTE' OR 'OBTER_PARTICIPANTES_FILIAL'.
          /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                     CHANGING  data = w_partic ).

          lc_integracao = lc_integracao_token.
          l_mesg        = | { 'Participantes Gravados com Sucesso.' } |.

          UPDATE zsdt0310  SET assinatura_sequencial = w_partic-assinatura_sequencial
                               proibir_rejeicao      = w_partic-proibir_rejeicao
                         WHERE nr_venda              = me->zif_integracao_insumos~at_nr_venda
                           AND tipo_doc              = me->zif_integracao_insumos~at_tipo_doc
                           AND id_documento          = me->zif_integracao_insumos~at_id_documento.

          me->zif_integracao_insumos~set_gravar_participantes( EXPORTING t_zsdt0310      = me->zif_integracao_insumos~at_t_zsdt0310
                                                                         t_participantes = w_partic-participantes
                                                                         i_metodo        = w_metodo-metodo ).

          me->zif_integracao_insumos~set_gravar_log(           EXPORTING i_nr_venda      = me->zif_integracao_insumos~at_nr_venda
                                                                         i_tipo_doc      = me->zif_integracao_insumos~at_tipo_doc
                                                                         i_id_documento  = me->zif_integracao_insumos~at_id_documento
                                                                         i_mensagem      = l_mesg ).

        WHEN 'CANCELA_DOC_SIGAM'.
          l_mesg        = | { 'Documento Cancelado no SIGAM.' } |.
          me->zif_integracao_insumos~set_gravar_log(           EXPORTING i_nr_venda      = me->zif_integracao_insumos~at_nr_venda
                                                                         i_tipo_doc      = me->zif_integracao_insumos~at_tipo_doc
                                                                         i_id_documento  = me->zif_integracao_insumos~at_id_documento
                                                                         i_mensagem      = l_mesg ).

          COMMIT WORK AND WAIT.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_formata_data.

    DATA l_data  TYPE string.

    l_data = '&1-&2-&3T&4:&5:&6'. "-04:00'.

    REPLACE '&1' IN l_data WITH i_data(4).
    REPLACE '&2' IN l_data WITH i_data+4(2).
    REPLACE '&3' IN l_data WITH i_data+6(2).
    REPLACE '&4' IN l_data WITH sy-uzeit(2).
    REPLACE '&5' IN l_data WITH i_data+2(2).
    REPLACE '&6' IN l_data WITH i_data+4(2).

    CONDENSE l_data NO-GAPS.

    e_data = l_data.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_gravar_log.

    DATA: l_time     TYPE timestampl,
          w_zsdt0313 TYPE zsdt0313.

    r_if_integracao_insumos = me.

*-----------------------------------
*-- busca menor id
*-----------------------------------
    SELECT *
      FROM zsdt0310
      INTO TABLE @DATA(t_0310)
     WHERE nr_venda     = @i_nr_venda
       AND tipo_doc     = @i_tipo_doc
       AND id_documento = @i_id_documento.

    SORT t_0310 BY id_documento.
    READ TABLE t_0310 INTO DATA(w_0310) INDEX 1.

    DATA(l_id_documento) = w_0310-id_documento.

*-----------------------------------
*-- atualiza id agrupador
*-----------------------------------
    LOOP AT t_0310         INTO w_0310 WHERE id_doc_agrupador IS INITIAL.
      w_0310-id_doc_agrupador = l_id_documento.
      MODIFY zsdt0310      FROM w_0310.
    ENDLOOP.

*-----------------------------------
*-- atualiza log
*-----------------------------------
    DO.
      GET TIME STAMP FIELD l_time.

      SELECT SINGLE *
        FROM zsdt0313
        INTO @DATA(w_0313)
       WHERE id_doc_agrupador      = @l_id_documento
         AND id_seq                = @l_time.

      CHECK sy-subrc <> 0.

      CLEAR w_zsdt0313.
      w_zsdt0313-mandt             = sy-mandt.
      w_zsdt0313-id_doc_agrupador  = l_id_documento.
      w_zsdt0313-id_seq            = l_time.
      w_zsdt0313-id_log            = 1.
      w_zsdt0313-tipo_msg          = COND #( WHEN i_tipo_msg = abap_off THEN 'S'
                                                                        ELSE i_tipo_msg ).
      w_zsdt0313-mensagem          = i_mensagem.
      w_zsdt0313-usname            = sy-uname.
      w_zsdt0313-data              = sy-datum.
      w_zsdt0313-hora              = sy-uzeit.
      MODIFY zsdt0313           FROM w_zsdt0313.

      EXIT.
    ENDDO.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_gravar_participantes.

    DATA: l_totregs  TYPE i,
          l_seq      TYPE i,
          w_zsdt0316 TYPE zsdt0316,
          w_partic   TYPE zsde0036.

    r_if_integracao_insumos = me.

    CHECK t_participantes[] IS NOT INITIAL.

    READ TABLE t_zsdt0310 INTO DATA(w_0310) INDEX 1.

    SELECT COUNT( * )
      INTO l_totregs
      FROM zsdt0316
     WHERE nr_doc_gerado    = w_0310-nr_doc_gerado
       AND id_doc_agrupador = w_0310-id_documento.

    LOOP AT t_participantes INTO w_partic.

      CLEAR w_zsdt0316.

      REPLACE ALL OCCURRENCES OF '.' IN w_partic-codigo WITH abap_off.
      REPLACE ALL OCCURRENCES OF '-' IN w_partic-codigo WITH abap_off.
      CONDENSE w_partic-codigo NO-GAPS.

      SELECT SINGLE *
        INTO w_zsdt0316
        FROM zsdt0316
       WHERE nr_doc_gerado    = w_0310-nr_doc_gerado
         AND id_doc_agrupador = w_0310-id_documento
         AND codigo           = w_partic-codigo
         AND id_grupo         = abap_off.

      IF sy-subrc = 0.
*      l_seq       = w_zsdt0316-sequenciador.
      ELSE.
        IF w_partic-idgrupo IS NOT INITIAL.
          SELECT SINGLE *
            INTO w_zsdt0316
            FROM zsdt0316
           WHERE nr_doc_gerado    = w_0310-nr_doc_gerado
             AND id_doc_agrupador = w_0310-id_documento
             AND id_grupo         = w_partic-idgrupo.
        ELSE.
          sy-subrc  = 4.
        ENDIF.

        IF sy-subrc = 0.
*        l_seq     = w_zsdt0316-sequenciador.
        ELSE.
*        l_totregs = l_totregs + 1.
*        l_seq     = l_totregs.
        ENDIF.
      ENDIF.

*------------------------------
      l_totregs                   = l_totregs + 1.
      l_seq                       = l_totregs.
*------------------------------

      w_zsdt0316-nr_doc_gerado    = w_0310-nr_doc_gerado.
      w_zsdt0316-id_doc_agrupador = w_0310-id_documento.
      w_zsdt0316-sequenciador     = l_seq.
      w_zsdt0316-ordem            = w_partic-ordem.
      w_zsdt0316-codigo           = w_partic-codigo.
      w_zsdt0316-nome             = w_partic-nome.
      w_zsdt0316-email            = w_partic-email.
      w_zsdt0316-id_grupo         = w_partic-idgrupo.
      w_zsdt0316-qtd_assinantes   = w_partic-quantidadeassinantes.
      w_zsdt0316-revisor          = w_partic-revisor.
      w_zsdt0316-assinante        = w_partic-assinante.
      w_zsdt0316-tipo_assinatura  = w_partic-tipoassinatura.
      w_zsdt0316-cliente          = COND #( WHEN w_partic-tipoemitente = 'EMITENTE' THEN abap_true
                                                                                    ELSE abap_off ).
      w_zsdt0316-data             = sy-datum.
      w_zsdt0316-hora             = sy-uzeit.

      MODIFY zsdt0316          FROM w_zsdt0316.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_header.
  ENDMETHOD.


  METHOD zif_integracao_insumos~set_id_referencia.

    "Incluir Chave insumos
    r_if_integracao_insumos = me.
    me->zif_integracao_insumos~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_json_criar_docto.
*&-------------------------------------------------------------------&*
*&                    Histórico de Modificações                      &*
*& Autor ABAP |Request    |Data       |Descrição                     &*
*&-------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações p/ geração Contrato&*
*&                                    |Compra.                       &*
*&                                    |Chamado: 168919 2ª Parte.     &*
*&-------------------------------------------------------------------&*

    DATA: w_zsdt0310     TYPE zsdt0310,
          w_zsdt0311     TYPE zsdt0311,
          w_zsdt0312     TYPE zsdt0312,
          w_zsdt0312_aux TYPE zsdt0312,
          w_itens        TYPE zsde0031,
          w_emitentes    TYPE zsde0032,
          w_json         TYPE zsde0030,
          t_itens        TYPE zsde0031_t,
          t_emitentes    TYPE zsde0032_t.

    r_if_integracao_insumos = me.

*----------------------------------------------
*-- cultura
*----------------------------------------------
    SELECT *
      FROM zsdt0038
      INTO TABLE @DATA(t_0038).

    FREE: zif_integracao_insumos~at_json.

*----------------------------------------------
*-- montar json
*----------------------------------------------
    LOOP AT zif_integracao_insumos~at_t_zsdt0310 INTO w_zsdt0310.

      FREE: w_json,  w_zsdt0311, w_zsdt0312,
            t_itens, t_emitentes.

      READ TABLE me->zif_integracao_insumos~at_t_zsdt0311 INTO w_zsdt0311   WITH KEY id_documento = w_zsdt0310-id_documento.
      READ TABLE me->zif_integracao_insumos~at_t_zsdt0312 INTO w_zsdt0312   WITH KEY id_documento = w_zsdt0310-id_documento.
      READ TABLE t_0038                                   INTO DATA(w_0038) WITH KEY cultura      = w_zsdt0310-tpcult.

      SHIFT  w_zsdt0310-conta   LEFT DELETING LEADING '0'.
      SHIFT  w_zsdt0310-agencia LEFT DELETING LEADING '0'.

      w_json-nr_documento          = COND #( WHEN w_zsdt0310-nr_doc_gerado IS INITIAL THEN 0
                                                                                      ELSE w_zsdt0310-nr_doc_gerado ).
      w_json-nr_doc_simulacao      = w_zsdt0310-nr_venda.
*     w_json-id_cliente_sap        = COND #( WHEN w_zsdt0311-tp_emitente = 'EMITENTE' THEN w_zsdt0311-kunnr
*                                                                                     ELSE abap_off ).
      w_json-id_filial_sap         = w_zsdt0310-vkbur.
**<<<------"168919 - NMS - INI------>>>
*      w_json-dt_vencimento         = zif_integracao_insumos~set_formata_data( i_data = w_zsdt0310-dt_vencimento ).
*      w_json-nm_fazenda            = w_zsdt0310-fazenda.
      w_json-dt_vencimento         = COND #( WHEN w_zsdt0310-dt_vencimento IS NOT INITIAL THEN zif_integracao_insumos~set_formata_data( i_data = w_zsdt0310-dt_vencimento ) ELSE space ).
      w_json-nm_fazenda            = COND #( WHEN w_zsdt0310-tipo_doc NE 'CTC' THEN w_zsdt0310-fazenda ).
**<<<------"168919 - NMS - FIM------>>>
      w_json-tp_frete              = w_zsdt0312-inco1.
      w_json-nr_area               = w_zsdt0310-area_ha.
      w_json-tp_moeda              = w_zsdt0310-waerk.
      w_json-vr_cotacao            = w_zsdt0310-kursf.
      w_json-tp_cultura            = w_0038-descricao. "w_zsdt0310-tpcult.
      w_json-ds_safra              = w_zsdt0310-safra.
      w_json-dt_emissao            = zif_integracao_insumos~set_formata_data( i_data = w_zsdt0310-data ).
      w_json-qt_area_penhor        = w_zsdt0310-area_penhor.
      w_json-vr_total_dolar        = w_zsdt0310-vlrtot_usd.
      w_json-vr_juros_mora         = w_zsdt0310-juros_ano. "163113 CS2019001753 Mel. na Ger. Documentos( CTR / OVD ) PSA
*     w_json-id_fiador_01          = abap_off.
*     w_json-id_fiador_02          = abap_off.
      w_json-ps_pag_prorrogado     = 'N'. "w_zsdt0310-pag_prorrogado.
      w_json-dt_atual              = zif_integracao_insumos~set_formata_data( i_data = sy-datum ).
      w_json-st_documento          = '1'. "w_zsdt0310-status.
      w_json-tp_documento          = w_zsdt0310-tipo_doc.
      w_json-tp_documento_digital  = w_zsdt0310-tipo_doc_digital.
      w_json-nr_conta_corrente     = w_zsdt0310-conta.
      w_json-nm_agencia            = w_zsdt0310-agencia.
      w_json-nm_banco              = w_zsdt0310-banco.
      w_json-nm_usuario            = w_zsdt0310-usname.
      w_json-vr_total_real         = w_zsdt0310-vlrtot.
*-US 130070-26.12.2023-JT-inicio
      w_json-ps_valor_imp          = w_zsdt0310-imp_valor_unit.
      w_json-tp_distribuicao       = w_zsdt0310-canal_distribuicao.
      w_json-tp_operacao           = w_zsdt0312-tp_ov. "w_zsdt0310-tp_simulador.
      w_json-ds_observacao         = w_zsdt0310-observacao.
**<<<------"168919 - NMS - INI------>>>
      w_json-tp_condicao_pag       = w_zsdt0310-tp_simulador.
      w_json-st_enviado_sap        = space.
      w_json-nr_pedido             = COND #( WHEN w_zsdt0310-tipo_doc EQ 'CTC' THEN w_zsdt0310-fazenda ).
      w_json-tp_forma_pag          = w_zsdt0310-forma_pagamento.
      w_json-vr_multa_compradora   = w_zsdt0310-multa_compradora.
      w_json-vr_multa_vendedora    = w_zsdt0310-multa_vendedora.
      w_json-ps_royalty            = COND #( WHEN w_zsdt0310-royalties EQ abap_on THEN 'S' ELSE 'N' ).
      w_json-tp_royalty            = w_zsdt0310-alocacao_royalties.
      w_json-qt_prazo              = w_zsdt0310-dias_prazo.
**<<<------"168919 - NMS - FIM------>>>
*-----------------------------------
*-- elimina carac espeial
*-----------------------------------
      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = w_json-ds_observacao
        IMPORTING
          outtext           = w_json-ds_observacao
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.
*-US 130070-26.12.2023-JT-fim

*     w_json-ds_log                = abap_off.
*     w_json-id_assinatura         = abap_off.

      READ TABLE me->zif_integracao_insumos~at_t_zsdt0312 INTO w_zsdt0312_aux  WITH KEY id_documento = w_zsdt0310-id_documento
                                                                                        tp_insumo    = 'SEM'.
*     w_json-dt_entrega_sem        = COND #( WHEN sy-subrc = 0 THEN zif_integracao_insumos~set_formata_data( i_data = w_zsdt0312_aux-dt_entrega )
*                                                              ELSE 'null' ).

      READ TABLE me->zif_integracao_insumos~at_t_zsdt0312 INTO w_zsdt0312_aux  WITH KEY id_documento = w_zsdt0310-id_documento
                                                                                        tp_insumo    = 'DEF'.
*     w_json-dt_entrega_def        = COND #( WHEN sy-subrc = 0 THEN zif_integracao_insumos~set_formata_data( i_data = w_zsdt0312_aux-dt_entrega )
*                                                              ELSE 'null' ).

      READ TABLE me->zif_integracao_insumos~at_t_zsdt0312 INTO w_zsdt0312_aux  WITH KEY id_documento = w_zsdt0310-id_documento
                                                                                        tp_insumo    = 'FET'.
*     w_json-dt_entrega_fet        = COND #( WHEN sy-subrc = 0 THEN zif_integracao_insumos~set_formata_data( i_data = w_zsdt0312_aux-dt_entrega )
*                                                              ELSE 'null' ).

*-----------------------------------
*---- itens
*-----------------------------------
      LOOP AT me->zif_integracao_insumos~at_t_zsdt0312 INTO w_zsdt0312 WHERE id_documento = w_zsdt0310-id_documento.

        CLEAR w_itens.
        w_itens-nr_documento        = 0.
        w_itens-nr_doc_simulacao    = w_zsdt0310-nr_venda.
        w_itens-nr_posicao          = w_zsdt0312-posnr.
        w_itens-tp_insumo           = w_zsdt0312-tp_insumo.
        w_itens-tp_frete            = w_zsdt0312-inco1.
        w_itens-cd_material         = w_zsdt0312-matnr.
        w_itens-nr_posicao          = w_zsdt0312-posnr.
        w_itens-dt_vencimento       = zif_integracao_insumos~set_formata_data( i_data = w_zsdt0310-dt_vencimento ).
        w_itens-id_filial_sap       = w_zsdt0312-werks. "w_zsdt0310-vkbur.
        w_itens-qt_insumo           = w_zsdt0312-zmeng.
        w_itens-tp_unidade          = w_zsdt0312-descricao_um.
        w_itens-vr_unitario         = w_zsdt0312-vlr_unit.
        w_itens-vr_total            = w_zsdt0312-vlr_total.
        w_itens-nr_ordem_venda      = w_zsdt0312-vbeln.
        w_itens-vr_frete            = w_zsdt0312-valor_frete.
        w_itens-tp_cultura          = w_zsdt0312-cultura_aplicacao.
        w_itens-ds_insumo           = w_zsdt0312-desc_matnr.
        w_itens-ds_safra_aplicacao  = w_zsdt0312-safra_aplicacao.
        w_itens-nm_fazenda          = w_zsdt0310-fazenda.
        w_itens-ds_endereco         = w_zsdt0312-endereco_entrega.
**<<<------"168919 - NMS - INI------>>>
*        w_itens-dt_final_entrega    = zif_integracao_insumos~set_formata_data( i_data = w_zsdt0312-dt_entrega ).
**-US 130070-26.12.2023-JT-inicio
*      w_json-tp_condicao_pag       = w_zsdt0310-tp_simulador. "w_zsdt0312-tp_ov.
**-US 130070-26.12.2023-JT-fim
        w_itens-dt_inicio_entrega   = COND #( WHEN w_zsdt0310-tipo_doc EQ 'CTC' THEN zif_integracao_insumos~set_formata_data( i_data = w_zsdt0312-dt_entrega ) ).
        w_itens-dt_final_entrega    = COND #( WHEN w_zsdt0310-tipo_doc EQ 'CTC' THEN zif_integracao_insumos~set_formata_data( i_data = w_zsdt0312-dt_entrega_ate )
                                                                                ELSE zif_integracao_insumos~set_formata_data( i_data = w_zsdt0312-dt_entrega ) ).
**<<<------"168919 - NMS - FIM------>>>
        APPEND w_itens             TO t_itens.

      ENDLOOP.

*-----------------------------------
*---- emitente
*-----------------------------------
      LOOP AT me->zif_integracao_insumos~at_t_zsdt0311 INTO w_zsdt0311 WHERE id_documento = w_zsdt0310-id_documento.

        CLEAR w_emitentes.
        w_emitentes-nr_documento    = 0.
        w_emitentes-id_cliente_sap  = w_zsdt0311-kunnr.
        w_emitentes-nr_ordem        = w_zsdt0311-seq_kunnr.
        w_emitentes-tp_emitente     = w_zsdt0311-tp_emitente.
        APPEND w_emitentes         TO t_emitentes.

      ENDLOOP.

      w_json-itens                  = t_itens[].
      w_json-emitentes              = t_emitentes[].
**<<<------"168919 - NMS - INI------>>>
*-----------------------------------
*---parcelas
*-----------------------------------
      LOOP AT me->zif_integracao_insumos~at_t_zsdt0013 INTO DATA(el_zsdt0013) WHERE id_documento EQ w_zsdt0310-id_documento.
        APPEND INITIAL LINE TO w_json-parcelas ASSIGNING FIELD-SYMBOL(<fs_parcelas>).
        <fs_parcelas>-nr_documento    = 0.
        <fs_parcelas>-vr_parc_dolar   = el_zsdt0013-valor_usd.
        <fs_parcelas>-vr_parc_real    = el_zsdt0013-valor.
        <fs_parcelas>-dt_vencimento   = COND #( WHEN el_zsdt0013-dt_vcto IS NOT INITIAL THEN zif_integracao_insumos~set_formata_data( i_data = el_zsdt0013-dt_vcto ) ELSE space ).
        <fs_parcelas>-tp_condicao_pag = el_zsdt0013-zterm.
        <fs_parcelas>-vr_percentual   = el_zsdt0013-percentual.
        <fs_parcelas>-tp_modo_pag     = el_zsdt0013-modo_pagamento.

      ENDLOOP.
*-----------------------------------
*---royalties
*-----------------------------------
      LOOP AT me->zif_integracao_insumos~at_t_zsdt0014 INTO DATA(el_zsdt0014) WHERE id_documento EQ w_zsdt0310-id_documento.
        APPEND INITIAL LINE TO w_json-royalties ASSIGNING FIELD-SYMBOL(<fs_royalties>).
        <fs_royalties>-nr_documento      = 0.
        <fs_royalties>-id_fornecedor_sap = el_zsdt0014-cod_obtentora.
        <fs_royalties>-vr_total_dolar    = el_zsdt0014-valor_royalties_usd.
        <fs_royalties>-vr_total_real     = el_zsdt0014-valor_royalties.
        <fs_royalties>-nr_conta_corrente = el_zsdt0014-conta.
        <fs_royalties>-nm_agencia        = el_zsdt0014-agencia.
        <fs_royalties>-nm_banco          = el_zsdt0014-banco.
        <fs_royalties>-nr_mes            = el_zsdt0014-mes.
        <fs_royalties>-nr_ano            = el_zsdt0014-ano.

      ENDLOOP.
*-----------------------------------
*---testemunhas
*-----------------------------------
      LOOP AT me->zif_integracao_insumos~at_t_zsdt0015 INTO DATA(el_zsdt0015) WHERE id_documento EQ w_zsdt0310-id_documento.
        APPEND INITIAL LINE TO w_json-testemunhas ASSIGNING FIELD-SYMBOL(<fs_testemunha>).
        <fs_testemunha>-nr_documento  = 0.
        <fs_testemunha>-nr_cpf        = el_zsdt0015-cpf.
        <fs_testemunha>-nm_testemunha = el_zsdt0015-nome.
        <fs_testemunha>-ds_email      = el_zsdt0015-email.
        <fs_testemunha>-nr_ordem      = 0.

      ENDLOOP.
**<<<------"168919 - NMS - FIM------>>>
    ENDLOOP.

*-----------------------------------
*---monta JSON
*-----------------------------------
    zif_integracao_insumos~at_json = /ui2/cl_json=>serialize( data        = w_json
                                                              pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    REPLACE ALL OCCURRENCES OF '"null"' IN zif_integracao_insumos~at_json WITH 'null'.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_mensagem.

    r_if_integracao_insumos = me.

    CASE i_cod_msg.

      WHEN '01'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Não encontrados Dados Gerais ' )
                              attr2 = CONV #( 'do Gerador Documentos' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Não encontrados Dados Gerais ' )
            msgv2  = CONV #( 'do Gerador Documentos' ).

      WHEN '02'.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                              msgno = zcx_error=>zcx_erro_geral-msgno
                              attr1 = CONV #( 'Chave do Documento não gerada.' ) )
            msgid  = zcx_error=>zcx_erro_geral-msgid
            msgno  = zcx_error=>zcx_erro_geral-msgno
            msgty  = 'E'
            msgv1  = CONV #( 'Chave do Documento não gerada.' ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_send_msg.

    TYPES BEGIN OF ty_retorno.
    TYPES: access_token TYPE string.
    TYPES: expires_in TYPE string.
    TYPES: token_type TYPE string.
    TYPES END OF ty_retorno.

    DATA: lc_integrar    TYPE REF TO zcl_integracao,
          lc_retorno     TYPE ty_retorno,
          l_access_token TYPE string,
          l_token_type   TYPE string,
          l_expires_in   TYPE string,
          l_force        TYPE char01.

    r_if_integracao_insumos = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

  ENDMETHOD.


  METHOD zif_integracao_insumos~set_solicitar_assinatura.

    DATA: lo_object     TYPE REF TO zcl_insumos_documents_step_01,
          l_mesg        TYPE string,
          t_range       TYPE zinc_ref_range,
          w_range       TYPE zins_ref_range,
          l_zcx_integra TYPE char1,
          l_zcx_error   TYPE char1.

    FREE: t_range.

*-------------------------------
*-- instancia atributos
*-------------------------------
    r_if_integracao_insumos                     = me.
    me->zif_integracao_insumos~at_nr_venda      = i_nr_venda.
    me->zif_integracao_insumos~at_tipo_doc      = i_tipo_doc.
    me->zif_integracao_insumos~at_id_documento  = i_id_documento.

*-------------------------------
*-- dados gerais
*-------------------------------
    SELECT SINGLE *
      FROM zsdt0310
      INTO @DATA(w_zsdt0310)
     WHERE nr_venda      = @me->zif_integracao_insumos~at_nr_venda
       AND tipo_doc      = @me->zif_integracao_insumos~at_tipo_doc
       AND id_documento  = @me->zif_integracao_insumos~at_id_documento
       AND status       <> @me->zif_integracao_insumos~at_cancelado.

    IF sy-subrc <> 0.
      me->zif_integracao_insumos~set_mensagem( '01' ).
    ENDIF.

*-------------------------------
*-- monta range
*-------------------------------
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_zsdt0310-id_documento ) TO t_range.

*-------------------------------
*-- Executa API Bry
*-------------------------------
    TRY.
        CREATE OBJECT lo_object.
        lo_object->process_contracts( t_range[] ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        DATA(lx_integra) = ex_integra.
        l_zcx_integra    = abap_true.

      CATCH zcx_error      INTO DATA(ex_error).
        DATA(lx_error)   = ex_error.
        l_zcx_error      = abap_true.
    ENDTRY.

*-------------------------------
*-- Retorno Erro
*-------------------------------
    CASE abap_true.

      WHEN l_zcx_integra.
        l_mesg  = ex_integra->msgv1 && '-' && ex_integra->msgv2 && '-' && ex_integra->msgv3 && '-' && ex_integra->msgv4.
        me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = me->zif_integracao_insumos~at_nr_venda
                                                             i_tipo_doc     = me->zif_integracao_insumos~at_tipo_doc
                                                             i_id_documento = me->zif_integracao_insumos~at_id_documento
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg ).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = lx_integra->msgid
                              msgno = lx_integra->msgno
                              attr1 = CONV #( lx_integra->msgv1 )
                              attr2 = CONV #( lx_integra->msgv2 )
                              attr3 = CONV #( lx_integra->msgv3 )
                              attr4 = CONV #( lx_integra->msgv4 ) )
            msgid  = lx_integra->msgid
            msgno  = lx_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( lx_integra->msgv1 )
            msgv2  = CONV #( lx_integra->msgv2 )
            msgv3  = CONV #( lx_integra->msgv3 )
            msgv4  = CONV #( lx_integra->msgv4 ).

      WHEN l_zcx_error.
        l_mesg  = ex_error->msgv1 && '-' && ex_error->msgv2 && '-' && ex_error->msgv3 && '-' && ex_error->msgv4.
        me->zif_integracao_insumos~set_gravar_log( EXPORTING i_nr_venda     = me->zif_integracao_insumos~at_nr_venda
                                                             i_tipo_doc     = me->zif_integracao_insumos~at_tipo_doc
                                                             i_id_documento = me->zif_integracao_insumos~at_id_documento
                                                             i_tipo_msg     = 'E'
                                                             i_mensagem     = l_mesg ).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = lx_error->msgid
                              msgno = lx_error->msgno
                              attr1 = CONV #( lx_error->msgv1 )
                              attr2 = CONV #( lx_error->msgv2 )
                              attr3 = CONV #( lx_error->msgv3 )
                              attr4 = CONV #( lx_error->msgv4 ) )
            msgid  = lx_error->msgid
            msgno  = lx_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( lx_error->msgv1 )
            msgv2  = CONV #( lx_error->msgv2 )
            msgv3  = CONV #( lx_error->msgv3 )
            msgv4  = CONV #( lx_error->msgv4 ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
