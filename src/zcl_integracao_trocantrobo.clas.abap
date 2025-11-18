class ZCL_INTEGRACAO_TROCANTROBO definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_TROCANTROBO .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_TROCANTROBO IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface  = zif_integracao=>at_id_interface_trocantrobo.
    me->zif_integracao_inject~at_tp_integracao = zif_integracao=>at_tp_integracao_inbound.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'Troca NT - Itinerario'.
    me->zif_integracao_inject~at_tp_canal = zif_integracao=>at_tp_canal_comunica_http.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_FALSE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    E_SUCESSO = ABAP_TRUE.

    R_IF_INTEGRACAO_INJECT = ME.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    "Metodo para Integrar InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    E_SUCESSO = ABAP_TRUE.

    R_IF_INTEGRACAO_INJECT = ME.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    "Metodo para processoamento de InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_integracao_trocantrobo~get_instance.

    IF zif_integracao_trocantrobo~at_if_integracao_trocantrobo IS NOT BOUND.
      CREATE OBJECT zif_integracao_trocantrobo~at_if_integracao_trocantrobo TYPE zcl_integracao_trocantrobo.
    ENDIF.
    r_if_integracao_trocantrobo = zif_integracao_trocantrobo~at_if_integracao_trocantrobo.

  ENDMETHOD.


  METHOD zif_integracao_trocantrobo~get_itinerario.

    DATA: r_ordem TYPE RANGE OF vbap-vbeln,
          r_item  TYPE RANGE OF vbap-posnr,
          xroute  TYPE vbap-route.

    r_if_integracao_trocantrobo = me.

    IF me->zif_integracao_trocantrobo~at_request-ordem IS NOT INITIAL.

      r_ordem =
      VALUE #(
                (
                  sign = 'I'
                  option = 'EQ'
                  low = |{ me->zif_integracao_trocantrobo~at_request-ordem ALPHA = IN }|
                 )
             ).
    ELSE.
      EXIT.
    ENDIF.

    IF me->zif_integracao_trocantrobo~at_request-item IS NOT INITIAL.

      r_item =
      VALUE #(
                (
                  sign = 'I'
                  option = 'EQ'
                  low = |{ me->zif_integracao_trocantrobo~at_request-item ALPHA = IN }|
                 )
             ).

    ENDIF.

    SELECT SINGLE route
      FROM vbap
      INTO @xroute
      WHERE vbeln IN @r_ordem
        AND posnr IN @r_item.

    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE route
        FROM ekpv
        INTO xroute
        WHERE ebeln IN r_ordem
          AND ebelp IN r_item.

    ENDIF.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE *
      FROM tvro
      INTO @DATA(wa_tvro)
      WHERE route EQ @xroute.

    SELECT SINGLE *
      FROM tvrot
      INTO @DATA(wa_tvrot)
      WHERE route EQ @xroute
        AND spras EQ @sy-langu.

    SELECT SINGLE azone, lzone
      FROM trolz
      INTO @DATA(wa_trolz)
      WHERE route EQ @xroute
        AND aland EQ 'BR'
        AND vsbed EQ '01'
        AND tragr EQ '0001'.

    SELECT SINGLE *
      FROM tzont
      INTO @DATA(wa_tzonta)
      WHERE spras EQ @sy-langu
        AND land1 EQ 'BR'
        AND zone1 EQ @wa_trolz-azone.

    SELECT SINGLE *
      FROM tzont
      INTO @DATA(wa_tzontl)
      WHERE spras EQ @sy-langu
        AND land1 EQ 'BR'
        AND zone1 EQ @wa_trolz-lzone.

    CLEAR me->zif_integracao_trocantrobo~at_response.

    me->zif_integracao_trocantrobo~at_response =
    VALUE #(
            ordem      = me->zif_integracao_trocantrobo~at_request-ordem
            item       = me->zif_integracao_trocantrobo~at_request-item
            route      = xroute
            distz      = wa_tvro-distz
            medst      = wa_tvro-medst
            bezei      = wa_tvrot-bezei
            traztd     = wa_tvro-traztd
            azone      = wa_trolz-azone
            desc_azone = wa_tzonta-vtext
            lzone      = wa_trolz-lzone
            desc_lzone = wa_tzontl-vtext

    ).

    CLEAR: wa_tvro, wa_tvrot, wa_trolz, wa_tzonta, wa_tzontl, xroute.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_TROCANTROBO~SET_DS_DATA.

    R_IF_INTEGRACAO_TROCANTROBO = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = ME->ZIF_INTEGRACAO_TROCANTROBO~AT_REQUEST ).

  ENDMETHOD.


  METHOD zif_integracao_trocantrobo~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    r_if_integracao_trocantrobo = me.

    CREATE OBJECT lc_integracao.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = me->zif_integracao_trocantrobo~at_response
        pretty_name = abap_true
      RECEIVING
        r_json      = e_msg.


    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_trocantrobo~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
           IMPORTING
             e_data_retorno =  DATA(e_data_retorno)
             e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    e_integracao-ds_data_retorno = e_msg.
    MODIFY zintegracao FROM e_integracao.
    COMMIT WORK.

    e_protocolo = zcl_string=>lpad( i_str  = CONV #( e_integracao-id_integracao ) i_qtd  = 15 i_char = '0'  ).

    CLEAR: lc_integracao.

  ENDMETHOD.
ENDCLASS.
