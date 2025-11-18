class ZCL_INTEGRACAO_PLAN_PRODUCE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_PLAN_PRODUCE .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional
      value(I_REQ) type ZMMC_DADOS_INT_COUPA_EBAN optional
      value(I_FORNECEDOR_SAP) type LIFNR optional
    raising
      ZCX_INTEGRACAO .
protected section.

  data MO_SERVICE type ref to ZCL_PP_SERVICES .
  data SERVICE_CX type ref to ZCX_PP_SERVICES .
  data CONTAINER_MESSAGE type ref to /IWBEP/IF_MESSAGE_CONTAINER .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_PLAN_PRODUCE IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface  = zif_integracao=>at_id_interface_plan_produce.
    me->zif_integracao_inject~at_tp_integracao = zif_integracao=>at_tp_integracao_inbound.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'Plan Produce'.
    me->zif_integracao_inject~at_tp_canal = zif_integracao=>at_tp_canal_comunica_http.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_form_fields = me->zif_integracao_inject~at_form_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    r_if_integracao_inject = me.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields = i_form_fields.
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

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    "Metodo para processoamento de InBound

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_plan_produce~get_instance.

    IF zif_integracao_plan_produce~at_if_integracao_plan_produce IS NOT BOUND.
      CREATE OBJECT zif_integracao_plan_produce~at_if_integracao_plan_produce TYPE zcl_integracao_plan_produce.
    ENDIF.
    r_if_integracao_plan_produce = zif_integracao_plan_produce~at_if_integracao_plan_produce.

  ENDMETHOD.


  METHOD zif_integracao_plan_produce~set_ds_data.

*##################################################
*#AT_PLAN_PRODUCE_OC_CANCEL       'cancelar'      #
*#AT_PLAN_PRODUCE_OC_PESO_VAZIO   'pesagemvazia'  #
*#AT_PLAN_PRODUCE_OC_MOD  Constant 'modordemprod' #
*##################################################

    r_if_integracao_plan_produce = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_info-ds_body
      CHANGING
        data = me->zif_integracao_plan_produce~at_ordem_carregamento.

  ENDMETHOD.


  METHOD zif_integracao_plan_produce~set_oc_cancelar.

*# AT_PLAN_PRODUCE_OC_CANCEL        'cancelar'    #

    DATA: wl_retorno TYPE zppe0004,
          it_mensage TYPE bapiret2_t.

    r_if_integracao_plan_produce = me.

    CREATE OBJECT mo_service.

    wl_retorno-ordemcarregamento = me->zif_integracao_plan_produce~at_ordem_carregamento-ordemcarregamento.
    wl_retorno-status = '1'.
    wl_retorno-message = 'Ordem de Carregamento Cancelada'.

    me->zif_integracao_inject~at_referencia =
    VALUE #(
              tp_referencia = 'Plan Produce - Cancelar OC'
              id_referencia = wl_retorno-ordemcarregamento
           ).

    CALL METHOD mo_service->select_shipment
      EXPORTING
        ordem_carregamento = CONV #( wl_retorno-ordemcarregamento )
      EXCEPTIONS
        shipment_not_found = 4.

    TRY.
        CALL METHOD mo_service->select_ordem_carregamento
          EXPORTING
            ordem_carregamento = CONV #( wl_retorno-ordemcarregamento ).

        CALL METHOD mo_service->process_order_is_locked
          EXPORTING
            order = mo_service->get_shipment( )-aufnr.

        CALL METHOD mo_service->storno_process_order
          EXPORTING
            ordem_carregamento = CONV #( wl_retorno-ordemcarregamento ).

        DELETE FROM zppt0009 WHERE ordem_carreg = wl_retorno-ordemcarregamento.

      CATCH zcx_pp_services INTO service_cx.
        CALL METHOD mo_service->set_message
          EXPORTING
            type   = 'E'
            id     = service_cx->if_t100_message~t100key-msgid
            number = service_cx->if_t100_message~t100key-msgno
            v1     = service_cx->if_t100_message~t100key-attr1
            v2     = service_cx->if_t100_message~t100key-attr2
            v3     = service_cx->if_t100_message~t100key-attr3
            v4     = service_cx->if_t100_message~t100key-attr4.
    ENDTRY.

    IF mo_service->has_messages( ) EQ abap_true.

      it_mensage = mo_service->get_messages( ).

      wl_retorno-status = '2'.
      CLEAR wl_retorno-message.

      LOOP AT it_mensage INTO DATA(wa_mensage).

        MESSAGE ID wa_mensage-id
            TYPE wa_mensage-type
            NUMBER wa_mensage-number
            WITH wa_mensage-message_v1
                 wa_mensage-message_v2
                 wa_mensage-message_v3
                 wa_mensage-message_v4
            INTO wl_retorno-message.

        wl_retorno-message =
        COND #( WHEN wl_retorno-message IS INITIAL
                    THEN wa_mensage-message
                    ELSE |{ wl_retorno-message }; { wa_mensage-message }| ).
      ENDLOOP.

    ENDIF.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = wl_retorno
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      RECEIVING
        r_json      = me->zif_integracao_plan_produce~at_response.

  ENDMETHOD.


  METHOD zif_integracao_plan_produce~set_oc_modificar.

*###################################################
*# AT_PLAN_PRODUCE_OC_MOD  Constant 'modordemprod' #
*###################################################

    TYPES: BEGIN OF ty_retorno.
             INCLUDE TYPE zppe0004.  "// Base
             INCLUDE TYPE zppe0005.  "// Complemento
           TYPES: END OF ty_retorno.

    DATA: wl_retorno TYPE ty_retorno.

    r_if_integracao_plan_produce = me.
    CREATE OBJECT mo_service.

    REPLACE ALL OCCURRENCES OF '/' IN me->zif_integracao_plan_produce~at_ordem_carregamento-data WITH space.

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external = me->zif_integracao_plan_produce~at_ordem_carregamento-data
      IMPORTING
        date_internal = me->zif_integracao_plan_produce~at_ordem_carregamento-data.

    MOVE-CORRESPONDING me->zif_integracao_plan_produce~at_ordem_carregamento TO wl_retorno.


    me->zif_integracao_inject~at_referencia =
    VALUE #(
              tp_referencia = 'Plan Produce - Modificar OC'
              id_referencia = wl_retorno-ordemcarregamento
           ).

    CALL METHOD mo_service->select_shipment
      EXPORTING
        ordem_carregamento = CONV #( wl_retorno-ordemcarregamento )
      EXCEPTIONS
        shipment_not_found = 4.

    IF sy-subrc IS INITIAL.
      wl_retorno-status = '2'.
      wl_retorno-message = |Ordem de Carregamento { wl_retorno-ordemcarregamento } não pode ser Modificada|.
    ELSE.

      TRY .

*          CALL METHOD mo_service->select_ordem_carregamento
*            EXPORTING
*              ordem_carregamento = CONV #( wl_retorno-ordemcarregamento ).

          SELECT SINGLE *
            FROM zppt0009
            INTO @DATA(wa_zppt0009)
            WHERE ordem_carreg EQ @wl_retorno-ordemcarregamento.

          IF sy-subrc IS INITIAL.
            UPDATE zppt0009 SET quantidade = wl_retorno-quantidade
                               dt_modificacao =  sy-datum
                               hr_modificacao = sy-uzeit
                   WHERE ordem_carreg EQ wl_retorno-ordemcarregamento.
          IF sy-subrc IS INITIAL.
            wl_retorno-status = '1'.
            wl_retorno-message = 'Processado com Sucesso, Alterado quantidade!'.
          ELSE.
            wl_retorno-status = '2'.
            wl_retorno-message = 'Erro na Alteração da Quantidade!'.
          ENDIF.
          ELSE.
            wa_zppt0009-ordem_carreg =  wl_retorno-ordemcarregamento.
            wa_zppt0009-placa =  wl_retorno-placa.
            wa_zppt0009-posnr =  wl_retorno-item.
            wa_zppt0009-vbeln =  wl_retorno-ordemvenda.
            wa_zppt0009-quantidade =  wl_retorno-quantidade.
            wa_zppt0009-data =  wl_retorno-data.

            MODIFY zppt0009 FROM wa_zppt0009.
          ENDIF.

          IF sy-subrc IS INITIAL.
            wl_retorno-status = '1'.
            wl_retorno-message = 'Processado com Sucesso,Criado na tabela zppt0009  '.
          ELSE.
            wl_retorno-status = '2'.
            wl_retorno-message = 'Erro na Criação da Ordem de Carregamento!'.
          ENDIF.

        CATCH zcx_pp_services INTO service_cx.
          CALL METHOD mo_service->set_message
            EXPORTING
              type   = 'E'
              id     = service_cx->if_t100_message~t100key-msgid
              number = service_cx->if_t100_message~t100key-msgno
              v1     = service_cx->if_t100_message~t100key-attr1
              v2     = service_cx->if_t100_message~t100key-attr2
              v3     = service_cx->if_t100_message~t100key-attr3
              v4     = service_cx->if_t100_message~t100key-attr4.
      ENDTRY.

    ENDIF.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = wl_retorno
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      RECEIVING
        r_json      = me->zif_integracao_plan_produce~at_response.

  ENDMETHOD.


  METHOD zif_integracao_plan_produce~set_oc_pesovazio.

*###################################################
*# AT_PLAN_PRODUCE_OC_PESO_VAZIO    'pesagemvazia' #
*###################################################

    r_if_integracao_plan_produce = me.

    DATA: wl_retorno TYPE zppe0004,
          it_mensage TYPE bapiret2_t.

    r_if_integracao_plan_produce = me.

    CREATE OBJECT mo_service.

    wl_retorno-ordemcarregamento = me->zif_integracao_plan_produce~at_ordem_carregamento-ordemcarregamento.
    wl_retorno-status = '1'.
    wl_retorno-message = 'OK'.

    me->zif_integracao_inject~at_referencia =
    VALUE #(
              tp_referencia = 'Plan Produce - Peso Vazio OC'
              id_referencia = wl_retorno-ordemcarregamento
           ).

    CALL METHOD mo_service->select_shipment
      EXPORTING
        ordem_carregamento = CONV #( wl_retorno-ordemcarregamento )
      EXCEPTIONS
        shipment_not_found = 4.

    TRY.

        CALL METHOD mo_service->select_ordem_carregamento
          EXPORTING
            ordem_carregamento = CONV #( wl_retorno-ordemcarregamento ).

        CALL METHOD mo_service->process_order_is_locked
          EXPORTING
            order = mo_service->get_shipment( )-aufnr.  " Order number

        CALL METHOD mo_service->storno_process_order
          EXPORTING
            ordem_carregamento = CONV #( wl_retorno-ordemcarregamento )
            psva               = abap_true.

        DELETE FROM zppt0009 WHERE ordem_carreg = wl_retorno-ordemcarregamento.

      CATCH zcx_pp_services INTO service_cx.
        CALL METHOD mo_service->set_message
          EXPORTING
            type   = 'E'
            id     = service_cx->if_t100_message~t100key-msgid
            number = service_cx->if_t100_message~t100key-msgno
            v1     = service_cx->if_t100_message~t100key-attr1
            v2     = service_cx->if_t100_message~t100key-attr2
            v3     = service_cx->if_t100_message~t100key-attr3
            v4     = service_cx->if_t100_message~t100key-attr4.
    ENDTRY.


    IF mo_service->has_messages( ) EQ abap_true.

      it_mensage = mo_service->get_messages( ).

      wl_retorno-status = '2'.
      CLEAR wl_retorno-message.

      LOOP AT it_mensage INTO DATA(wa_mensage).

        MESSAGE ID wa_mensage-id
            TYPE wa_mensage-type
            NUMBER wa_mensage-number
            WITH wa_mensage-message_v1
                 wa_mensage-message_v2
                 wa_mensage-message_v3
                 wa_mensage-message_v4
            INTO wl_retorno-message.

        wl_retorno-message =
        COND #( WHEN wl_retorno-message IS INITIAL
                    THEN wa_mensage-message
                    ELSE |{ wl_retorno-message }; { wa_mensage-message }| ).
      ENDLOOP.

    ENDIF.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = wl_retorno
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      RECEIVING
        r_json      = me->zif_integracao_plan_produce~at_response.

  ENDMETHOD.


  METHOD zif_integracao_plan_produce~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    r_if_integracao_plan_produce = me.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_plan_produce~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
           IMPORTING
             e_data_retorno =  DATA(e_data_retorno)
             e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    e_msg = me->zif_integracao_plan_produce~at_response.

    e_integracao-ds_data_retorno = e_msg.
    MODIFY zintegracao FROM e_integracao.
    COMMIT WORK.

    e_protocolo = zcl_string=>lpad( i_str  = CONV #( e_integracao-id_integracao ) i_qtd  = 15 i_char = '0'  ).

    CLEAR: lc_integracao.

  ENDMETHOD.
ENDCLASS.
