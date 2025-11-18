class ZCL_INTEGRACAO_APP_PRODUTOR definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_APP_PRODUTOR .

  methods CONSTRUCTOR .
  class-methods SEND_ACEITE
    importing
      !ACEITE type INDTYP
      !FORNECEDOR_SAP type LIFNR .
  methods GET_PROTOCOLO
    importing
      !I_FORNECEDOR_SAP type LIFNR
    returning
      value(E_PROTOCOLO) type ZMME0005 .
protected section.
private section.

  class-data AT_PROTOCOLO type STRING .

  methods SET_PROTOCOLO
    importing
      !ZMME0005 type ZMME0005 .
ENDCLASS.



CLASS ZCL_INTEGRACAO_APP_PRODUTOR IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface  = zif_integracao=>at_id_interface_app_produtor.
    me->zif_integracao_inject~at_tp_integracao = zif_integracao=>at_tp_integracao_inbound.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'APP PRODUTOR'.
    me->zif_integracao_inject~at_tp_canal = zif_integracao=>at_tp_canal_comunica_http.

  ENDMETHOD.


  METHOD get_protocolo.

    DATA: vl_lifnr TYPE lifnr.
    vl_lifnr = |{ i_fornecedor_sap ALPHA = IN }|.
    IMPORT protocolo TO e_protocolo FROM  MEMORY ID vl_lifnr.
*
    IF e_protocolo IS NOT INITIAL.
      FREE MEMORY ID vl_lifnr.
    ENDIF.

  ENDMETHOD.


  METHOD send_aceite.

*    DATA o_cdata TYPE string.

*    zcl_integracao_app_produtor=>zif_integracao_app_produtor~get_instance(
**            )->set_ds_data( i_info = i_info
*            )->set_envia_al5( aceite = aceite fornecedor_sap = fornecedor_sap
*            )->set_send_msg( IMPORTING e_msg = o_cdata
*         ).

    DATA retorno_aceite TYPE zmms007.

    SELECT SINGLE *
      FROM lfa1
      INTO @DATA(wa_lfa1)
      WHERE lifnr = @fornecedor_sap.

    IF wa_lfa1-adrnr IS NOT INITIAL.
      SELECT SINGLE *
        FROM adr6
        INTO @DATA(wa_adr6)
        WHERE addrnumber = @wa_lfa1-adrnr.

      SELECT *
        FROM adr2
        INTO TABLE @DATA(it_adr2)
        WHERE addrnumber = @wa_lfa1-adrnr.

      SELECT SINGLE *
        FROM adrc
        INTO @DATA(wa_adrc)
        WHERE addrnumber = @wa_lfa1-adrnr.

    ENDIF.

    retorno_aceite-nome             = wa_lfa1-name1.
*    retorno_aceite-data_nascimento  = wa_lfa1-gbdat.
    retorno_aceite-cpf              = wa_lfa1-stcd2.
    retorno_aceite-rg               = wa_lfa1-stcd3.

    IF wa_adrc IS NOT INITIAL.
      retorno_aceite-endereco = |{ wa_adrc-street } { wa_adrc-house_num1 }, { wa_adrc-city2 }, { wa_adrc-city1 }-{ wa_adrc-region }|.
    ENDIF.

    READ TABLE it_adr2 INTO DATA(wa_adr2) WITH KEY addrnumber = wa_lfa1-adrnr home_flag = abap_true.
    IF sy-subrc IS INITIAL.
      retorno_aceite-telefone1 = wa_adr2-telnr_long.
    ENDIF.

    READ TABLE it_adr2 INTO wa_adr2 WITH KEY addrnumber = wa_lfa1-adrnr home_flag = abap_false.
    IF sy-subrc IS INITIAL.
      retorno_aceite-telefone2 = wa_adr2-telnr_long.
    ENDIF.

    retorno_aceite-email           = wa_adr6-smtp_addr.
    retorno_aceite-grupo_economico = wa_lfa1-konzs.



    DATA(obj_webservice) = NEW zcl_webservice( ).
    DATA: lv_string TYPE string.
    DATA: lv_name TYPE string,
          i_json  TYPE string,
          r_json  TYPE string.


    cl_http_client=>create_by_url(
         EXPORTING
           url                = 'https://desenvolveapim.azure-api.net/integrationtest/v1/.auth/login/custom'
         IMPORTING
           client             = DATA(e_http)
         EXCEPTIONS
           argument_not_found = 1
           plugin_not_active  = 2
           internal_error     = 3
           OTHERS             = 4 ).

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json'.

    CALL METHOD e_http->request->set_header_field
      EXPORTING
        name  = 'Ocp-Apim-Subscription-Key'
        value = 'bae9a95f684a420685bc3977a70deebb'.

    i_json = '{ "login": "admin", "password": "3e90c73665cd50920af5e2f3bd0eb49f9b268e8a", "clientId": "ADMIN", "tenantCode": "amaggi" }'.

    obj_webservice->zif_webservice~consultar(
      EXPORTING
        i_http                     = e_http
        i_xml                      = i_json
        i_not_content_length       = abap_true
      IMPORTING
        e_reason                   = DATA(e_reason)
      RECEIVING
        e_resultado                = r_json
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

  ENDMETHOD.


  METHOD SET_PROTOCOLO.
    DATA: VL_LIFNR TYPE LIFNR.

    "// wbarbosa BUG-165542 03/02/2025
    VL_LIFNR = COND #( WHEN ZMME0005-FORNECEDOR IS NOT INITIAL THEN |{ ZMME0005-FORNECEDOR ALPHA = IN }| ELSE |{ ZMME0005-CLIENTE ALPHA = IN }| ).
    "// wbarbosa BUG-165542 03/02/2025

    EXPORT PROTOCOLO FROM ZMME0005 TO MEMORY ID VL_LIFNR.

  ENDMETHOD.


  METHOD zif_integracao_app_produtor~get_instance.

    IF zif_integracao_app_produtor~at_if_integracao_app_produtor IS NOT BOUND.
      CREATE OBJECT zif_integracao_app_produtor~at_if_integracao_app_produtor TYPE zcl_integracao_app_produtor.
    ENDIF.
    r_if_integracao_app_produtor = zif_integracao_app_produtor~at_if_integracao_app_produtor.

  ENDMETHOD.


  METHOD zif_integracao_app_produtor~get_ordem_vendas.

    r_if_integracao_app_produtor = me.

    TRY.
        CALL FUNCTION 'ZSD_PORTAL_PRODUTOR'
          EXPORTING
            tipo    = me->zif_integracao_app_produtor~at_tipo
            at_tcon = me->zif_integracao_app_produtor~at_tcon
            at_cont = me->zif_integracao_app_produtor~at_cont
            at_orgv = me->zif_integracao_app_produtor~at_orgv
            at_cdis = me->zif_integracao_app_produtor~at_cdis
            at_sati = me->zif_integracao_app_produtor~at_sati
            at_fatu = me->zif_integracao_app_produtor~at_fatu
            at_clie = me->zif_integracao_app_produtor~at_clie
            at_date = me->zif_integracao_app_produtor~at_date
          TABLES
            at_item = me->zif_integracao_app_produtor~at_retorno_item.

         DELETE me->zif_integracao_app_produtor~at_retorno_item WHERE ordem NOT IN me->zif_integracao_app_produtor~at_ordens.

      CATCH cx_root INTO DATA(erro).
        APPEND INITIAL LINE TO me->zif_integracao_app_produtor~at_retorno_remessa.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_app_produtor~get_remessas.

    DATA: it_saida TYPE zsde009_t.
    DATA: it_remessa TYPE zsde010_t.

    r_if_integracao_app_produtor = me.

    TRY .

        CALL FUNCTION 'ZSD_GET_DADOS_ZSDT0051'
          EXPORTING
            r_tpcont  = me->zif_integracao_app_produtor~at_tcon
            r_cont    = me->zif_integracao_app_produtor~at_cont
            r_posnr   = me->zif_integracao_app_produtor~at_posnr
            r_orgven  = me->zif_integracao_app_produtor~at_orgv
            r_cdist   = me->zif_integracao_app_produtor~at_cdis
            r_sativ   = me->zif_integracao_app_produtor~at_sati
            r_clien   = me->zif_integracao_app_produtor~at_clie
            r_datent  = me->zif_integracao_app_produtor~at_date
            r_fatuv   = me->zif_integracao_app_produtor~at_fatu
          TABLES
            t_remessa = me->zif_integracao_app_produtor~at_retorno_remessa.
      CATCH cx_root INTO DATA(erro).
        APPEND INITIAL LINE TO me->zif_integracao_app_produtor~at_retorno_remessa.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_integracao_app_produtor~get_solicitacoes.

    r_if_integracao_app_produtor = me.
    TRY .

        CALL FUNCTION 'ZSD_PORTAL_PRODUTOR'
          EXPORTING
            tipo            = me->zif_integracao_app_produtor~at_tipo
            at_tcon         = me->zif_integracao_app_produtor~at_tcon
            at_cont         = me->zif_integracao_app_produtor~at_cont
            at_orgv         = me->zif_integracao_app_produtor~at_orgv
            at_cdis         = me->zif_integracao_app_produtor~at_cdis
            at_sati         = me->zif_integracao_app_produtor~at_sati
            at_fatu         = me->zif_integracao_app_produtor~at_fatu
            at_clie         = me->zif_integracao_app_produtor~at_clie
            at_date         = me->zif_integracao_app_produtor~at_date
            at_inscestadual = me->zif_integracao_app_produtor~at_inscestadual
          TABLES
            at_header       = me->zif_integracao_app_produtor~at_retorno_header.

      CATCH cx_root INTO DATA(erro).
        APPEND INITIAL LINE TO me->zif_integracao_app_produtor~at_retorno_header.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_app_produtor~set_ds_data.

    DATA: solicitacao     TYPE zsde003,
          ov              TYPE zsde007,
          tipo            TYPE zsde008,
          remessa         TYPE zsde011,
          termo           TYPE zsde012,
          t_head          TYPE TABLE OF zsdt0040,
          t_item          TYPE TABLE OF zsdt0041,
          t_aditivo       TYPE TABLE OF zsdt0090,

*Range
          r_doc_simulacao TYPE RANGE OF zsdt0040-doc_simulacao,
          r_safra         TYPE RANGE OF zsdt0040-safra,

          r_stcd1         TYPE RANGE OF kna1-stcd1,
          r_stcd2         TYPE RANGE OF kna1-stcd2,
          r_stcd3         TYPE RANGE OF kna1-stcd3,
          "r_vbeln         TYPE RANGE OF vbak-vbeln,


          r_kunnr         TYPE RANGE OF zsdt0040-kunnr,
          r_cultura       TYPE RANGE OF zsdt0040-cultura,
          r_vbeln         TYPE RANGE OF vbap-vbeln,
          r_posnr         TYPE RANGE OF vbap-posnr.

    r_if_integracao_app_produtor = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

    FREE me->zif_integracao_app_produtor~at_rsparams[].

    /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = tipo ).
    me->zif_integracao_app_produtor~at_tipo = tipo.

    CASE me->zif_integracao_app_produtor~at_tipo.
      WHEN '1'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = solicitacao ).
      WHEN '2'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = ov ).
      WHEN '3'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = remessa ).
      WHEN '4'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = termo ).
      WHEN OTHERS.
        EXIT.
    ENDCASE.

    IF termo-fornecedor IS NOT INITIAL OR termo-cliente IS NOT INITIAL.
      me->zif_integracao_app_produtor~at_fornecedor_sap = termo-fornecedor.
      me->zif_integracao_app_produtor~at_aceite = termo-aceite.
      me->zif_integracao_app_produtor~at_protocolo = termo-protocolo.

*-US 135191-18-06-2024-#135191-RJF-inicio
      me->zif_integracao_app_produtor~at_tipo    = termo-tipo.
      me->zif_integracao_app_produtor~at_cliente = termo-cliente.
      me->zif_integracao_app_produtor~at_origem  = termo-origem.
*-US 135191-18-06-2024-#135191-RJF-fim
    ENDIF.

    IF ov-contrato IS NOT INITIAL.
      APPEND VALUE #(
                    sign = 'I'
                    option = 'EQ'
                    low = ov-contrato
          ) TO r_doc_simulacao.
    ENDIF.

    LOOP AT ov-ordens INTO DATA(lwa_ordem).
      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = lwa_ordem ) TO me->zif_integracao_app_produtor~at_ordens.
    ENDLOOP.

    IF remessa-ordem IS NOT INITIAL.
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = remessa-ordem
      ) TO r_vbeln.
    ENDIF.

    IF remessa-item IS NOT INITIAL.
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = remessa-item
      ) TO r_posnr.

      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = remessa-item
                high = sy-datum
      ) TO me->zif_integracao_app_produtor~at_posnr.

    ENDIF.

    LOOP AT solicitacao-contrato INTO DATA(w_contrato).
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = w_contrato
      ) TO r_doc_simulacao.
    ENDLOOP.

    LOOP AT solicitacao-safra INTO DATA(w_safra).
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = w_safra
      ) TO r_safra.
    ENDLOOP.

    LOOP AT solicitacao-produtor INTO DATA(w_produto).
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = w_produto
      ) TO r_kunnr.
    ENDLOOP.

    LOOP AT solicitacao-produtor_cnpj INTO DATA(w_produto_cnpj).
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = w_produto_cnpj
      ) TO r_stcd1.
    ENDLOOP.

    LOOP AT solicitacao-produtor_cpf INTO DATA(w_produto_cpf).
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = w_produto_cpf
      ) TO r_stcd2.
    ENDLOOP.

    LOOP AT solicitacao-inscestadual INTO DATA(w_produto_inscestadual).
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = w_produto_inscestadual
      ) TO me->zif_integracao_app_produtor~at_inscestadual.
    ENDLOOP.

    IF r_stcd1[] IS NOT INITIAL OR r_stcd2[] IS NOT INITIAL.
      SELECT kunnr
        FROM kna1 INTO TABLE @DATA(lit_kna1_kunnr)
       WHERE stcd1 IN @r_stcd1
         AND stcd2 IN @r_stcd2.


      SORT lit_kna1_kunnr.
      DELETE ADJACENT DUPLICATES FROM lit_kna1_kunnr.

      LOOP AT lit_kna1_kunnr INTO DATA(lwa_kna1_kunnr).
        APPEND VALUE #( sign = 'I'
                        option = 'EQ'
                        low = lwa_kna1_kunnr ) TO r_kunnr.
      ENDLOOP.
    ENDIF.

*    "Adicionar Filtro de Cliente Automaticamente
*    CASE me->zif_integracao_app_produtor~at_tipo.
*      WHEN '1'. "Solicitação
*        IF ( r_kunnr[] IS INITIAL ) AND ( r_doc_simulacao[] IS NOT INITIAL ) .
*          SELECT KUNNR
*            FROM ZSDT0040 INTO TABLE @DATA(LIT_ZSDT0040_KUNNR)
*           WHERE doc_simulacao IN @r_doc_simulacao.
*
*          LOOP AT LIT_ZSDT0040_KUNNR INTO DATA(lwa_zsdt0040_kunnr).
*            APPEND VALUE #( sign = 'I'
*                            option = 'EQ'
*                             low = lwa_zsdt0040_kunnr-kunnr ) TO r_kunnr.
*          ENDLOOP.
*        ENDIF.
*      WHEN OTHERS.
*    ENDCASE.

    LOOP AT solicitacao-cultura INTO DATA(w_cultura).
      APPEND VALUE #(
                sign = 'I'
                option = 'EQ'
                low = w_cultura
      ) TO r_cultura.
    ENDLOOP.

    CASE me->zif_integracao_app_produtor~at_tipo.
      WHEN '1'.
        CHECK r_kunnr IS NOT INITIAL OR r_doc_simulacao IS NOT INITIAL .
      WHEN '2'.
        CHECK r_doc_simulacao IS NOT INITIAL.
      WHEN '3'.
        CHECK r_vbeln IS NOT INITIAL.
        CHECK r_posnr IS NOT INITIAL.

        SELECT 'I' AS sign,
               'EQ' AS option,
                b~doc_simulacao AS low
         FROM vbap AS a
          INNER JOIN zsdt0041 AS b ON a~vbeln EQ b~vbeln
         INTO TABLE @r_doc_simulacao
         WHERE a~vbeln IN @r_vbeln
           AND a~posnr IN @r_posnr.

        CHECK r_doc_simulacao IS NOT INITIAL.
      WHEN '4'.
        CHECK me->zif_integracao_app_produtor~at_fornecedor_sap IS NOT INITIAL.
        EXIT. "Saindo do metodo aqui devido não precisar realizar as demais seleções na sequencia
    ENDCASE.

    SELECT *
      FROM zsdt0040
      INTO TABLE t_head
      WHERE doc_simulacao IN r_doc_simulacao
        AND safra   IN r_safra
        AND kunnr   IN r_kunnr
        AND cultura IN r_cultura.

    CHECK sy-subrc IS INITIAL.

    SELECT *
      FROM zsdt0041
      INTO TABLE t_item
      FOR ALL ENTRIES IN t_head
      WHERE doc_simulacao EQ t_head-doc_simulacao.

    CHECK sy-subrc IS INITIAL.

    SELECT *
      FROM zsdt0090
      INTO TABLE t_aditivo
      FOR ALL ENTRIES IN t_head
      WHERE doc_simulacao EQ t_head-doc_simulacao.

    LOOP AT t_head INTO DATA(w_head).
*   "Cliente
      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_head-kunnr ) TO me->zif_integracao_app_produtor~at_clie.
    ENDLOOP.

    LOOP AT t_item INTO DATA(w_item).
*   " Tipo de Contrato
      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_item-auart ) TO me->zif_integracao_app_produtor~at_tcon.

*   " Contrato
      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_item-vbeln ) TO me->zif_integracao_app_produtor~at_cont.

    ENDLOOP.

    LOOP AT t_aditivo INTO DATA(w_aditivo).
*   " Tipo de Contrato
      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_aditivo-auart ) TO me->zif_integracao_app_produtor~at_tcon.

      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_aditivo-auartv ) TO me->zif_integracao_app_produtor~at_tcon.

*   " Contrato
      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_aditivo-vbeln ) TO me->zif_integracao_app_produtor~at_cont.

      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_aditivo-vbelv ) TO me->zif_integracao_app_produtor~at_cont.

*   "Cliente
      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_aditivo-kunnr ) TO me->zif_integracao_app_produtor~at_clie.

*   "Cliente
      APPEND VALUE #(
          sign = 'I'
          option = 'EQ'
          low = w_aditivo-kunnrv ) TO me->zif_integracao_app_produtor~at_clie.
    ENDLOOP.

    SORT me->zif_integracao_app_produtor~at_clie.
    DELETE ADJACENT DUPLICATES FROM me->zif_integracao_app_produtor~at_clie COMPARING ALL FIELDS.
    DELETE me->zif_integracao_app_produtor~at_clie WHERE low IS INITIAL.

    SORT me->zif_integracao_app_produtor~at_tcon.
    DELETE ADJACENT DUPLICATES FROM me->zif_integracao_app_produtor~at_tcon COMPARING ALL FIELDS.
    DELETE me->zif_integracao_app_produtor~at_tcon WHERE low IS INITIAL.

    SORT me->zif_integracao_app_produtor~at_cont.
    DELETE ADJACENT DUPLICATES FROM me->zif_integracao_app_produtor~at_cont COMPARING ALL FIELDS.
    DELETE me->zif_integracao_app_produtor~at_cont WHERE low IS INITIAL.

*   "Organização de Vendas
    APPEND VALUE #(
        sign = 'I'
        option = 'CP'
        low = '*' ) TO me->zif_integracao_app_produtor~at_orgv.

*   "Canal Distribuição
    APPEND VALUE #(
        sign = 'I'
        option = 'CP'
        low = '*' ) TO me->zif_integracao_app_produtor~at_cdis.

*   "Setor Atividade
    APPEND VALUE #(
        sign = 'I'
        option = 'CP'
        low = '*' ) TO me->zif_integracao_app_produtor~at_sati.

*   "Data de Entrada
    APPEND VALUE #(
        sign = 'I'
        option = 'BT'
        low = '20000101'
        high = sy-datum ) TO me->zif_integracao_app_produtor~at_date.

*   "Data de Faturamento
    APPEND VALUE #(
        sign = 'I'
        option = 'BT'
        low = '20000101'
        high = sy-datum ) TO me->zif_integracao_app_produtor~at_fatu.

  ENDMETHOD.


  METHOD zif_integracao_app_produtor~set_envia_al5.

    r_if_integracao_app_produtor = me.

    SELECT SINGLE *
      FROM lfa1
      INTO @DATA(wa_lfa1)
      WHERE lifnr = @fornecedor_sap.

    IF wa_lfa1-adrnr IS NOT INITIAL.
      SELECT SINGLE *
        FROM adr6
        INTO @DATA(wa_adr6)
        WHERE addrnumber = @wa_lfa1-adrnr.

      SELECT *
        FROM adr2
        INTO TABLE @DATA(it_adr2)
        WHERE addrnumber = @wa_lfa1-adrnr.

      SELECT SINGLE *
        FROM adrc
        INTO @DATA(wa_adrc)
        WHERE addrnumber = @wa_lfa1-adrnr.

    ENDIF.

    me->zif_integracao_app_produtor~at_retorno_aceite-nome     = wa_lfa1-name1.
    me->zif_integracao_app_produtor~at_retorno_aceite-data_nascimento  = wa_lfa1-gbdat.
    me->zif_integracao_app_produtor~at_retorno_aceite-cpf      = wa_lfa1-stcd2.
    me->zif_integracao_app_produtor~at_retorno_aceite-rg       = wa_lfa1-stcd3.

    IF wa_adrc IS NOT INITIAL.
      me->zif_integracao_app_produtor~at_retorno_aceite-endereco = |{ wa_adrc-street } { wa_adrc-house_num1 }, { wa_adrc-city2 }, { wa_adrc-city1 }-{ wa_adrc-region }|.
    ENDIF.

    READ TABLE it_adr2 INTO DATA(wa_adr2) WITH KEY addrnumber = wa_lfa1-adrnr home_flag = abap_true.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_app_produtor~at_retorno_aceite-telefone1 = wa_adr2-telnr_long.
    ENDIF.

    READ TABLE it_adr2 INTO wa_adr2 WITH KEY addrnumber = wa_lfa1-adrnr home_flag = abap_false.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_app_produtor~at_retorno_aceite-telefone2 = wa_adr2-telnr_long.
    ENDIF.

    me->zif_integracao_app_produtor~at_retorno_aceite-email           = wa_adr6-smtp_addr.
    me->zif_integracao_app_produtor~at_retorno_aceite-grupo_economico = wa_lfa1-konzs.


  ENDMETHOD.


  METHOD zif_integracao_app_produtor~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    r_if_integracao_app_produtor = me.

    CREATE OBJECT lc_integracao.

    CASE me->zif_integracao_app_produtor~at_tipo.
      WHEN '1'.

        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data        = me->zif_integracao_app_produtor~at_retorno_header
            pretty_name = abap_true
          RECEIVING
            r_json      = e_msg.

      WHEN '2'.

        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data        = me->zif_integracao_app_produtor~at_retorno_item
            pretty_name = abap_true
          RECEIVING
            r_json      = e_msg.

      WHEN '3'.

        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data        = me->zif_integracao_app_produtor~at_retorno_remessa
            pretty_name = abap_true
          RECEIVING
            r_json      = e_msg.

      WHEN '4'.

        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data        = me->zif_integracao_app_produtor~at_retorno_termo
            pretty_name = abap_true
          RECEIVING
            r_json      = e_msg.

        me->zif_integracao_inject~at_referencia =
            VALUE #(
                      tp_referencia = 'APP Produtor - Termo'
                      id_referencia = me->zif_integracao_app_produtor~at_protocolo
                   ).

    ENDCASE.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_app_produtor~at_id_integracao
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


METHOD zif_integracao_app_produtor~set_termos_aceite.
*  "// Rodar o MCHB da XK02

  DATA: tg_bdc                 TYPE TABLE OF bdcdata,
        opt                    TYPE ctu_params,
        vl_modo                TYPE char1 VALUE 'N',
        wa_data_share          TYPE zmme0005,
        lva_j_1kftind          TYPE lfa1-j_1kftind,
        lva_j_1kfrepre         TYPE lfa1-j_1kfrepre, "*-US 135191-18-06-2024-#135191-RJF
        lv_xk02_force_cadastro TYPE c,
        lv_xk02_termo_al5bank  TYPE c,
        tg_msg                 TYPE TABLE OF bdcmsgcoll.

  r_if_integracao_app_produtor = me.

  me->zif_integracao_app_produtor~at_retorno_termo-tipo      = me->zif_integracao_app_produtor~at_tipo. "'4'.
  me->zif_integracao_app_produtor~at_retorno_termo-aceite    = me->zif_integracao_app_produtor~at_aceite.
  me->zif_integracao_app_produtor~at_retorno_termo-protocolo = me->zif_integracao_app_produtor~at_protocolo.
  me->zif_integracao_app_produtor~at_retorno_termo-cliente   = me->zif_integracao_app_produtor~at_cliente.
  me->zif_integracao_app_produtor~at_retorno_termo-origem    = me->zif_integracao_app_produtor~at_origem.

  wa_data_share =
  VALUE #(
            protocolo  = me->zif_integracao_app_produtor~at_protocolo
            fornecedor = me->zif_integracao_app_produtor~at_fornecedor_sap
            aceite     = me->zif_integracao_app_produtor~at_aceite
            cliente    = me->zif_integracao_app_produtor~at_cliente
            origem     = me->zif_integracao_app_produtor~at_origem
  ).

  me->set_protocolo( wa_data_share ).

  IF me->zif_integracao_app_produtor~at_fornecedor_sap IS INITIAL AND me->zif_integracao_app_produtor~at_cliente IS INITIAL.
    me->zif_integracao_app_produtor~at_retorno_termo-fornecedor = '???????'.
    me->zif_integracao_app_produtor~at_retorno_termo-nr_msg = '999'.
    me->zif_integracao_app_produtor~at_retorno_termo-tp_msg = 'E'.
    me->zif_integracao_app_produtor~at_retorno_termo-vl_msg = 'Não foi informado o codigo do fornecedor/cliente!'.
    me->zif_integracao_app_produtor~at_retorno_termo-protocolo = me->zif_integracao_app_produtor~at_protocolo.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM lfa1 INTO @DATA(lwa_lfa1_check)
   WHERE lifnr EQ @me->zif_integracao_app_produtor~at_fornecedor_sap.

  IF sy-subrc NE 0.

*-US 135191-18-06-2024-#135191-RJF-inicio
    SELECT SINGLE *
      FROM kna1 INTO @DATA(lwa_kna1_check)
     WHERE kunnr EQ @me->zif_integracao_app_produtor~at_cliente.
    IF sy-subrc NE 0.
*-US 135191-18-06-2024-#135191-RJF-fim

      me->zif_integracao_app_produtor~at_retorno_termo-fornecedor = me->zif_integracao_app_produtor~at_fornecedor_sap.
      me->zif_integracao_app_produtor~at_retorno_termo-nr_msg = '999'.
      me->zif_integracao_app_produtor~at_retorno_termo-tp_msg = 'E'.
      me->zif_integracao_app_produtor~at_retorno_termo-vl_msg = 'Não existe cadastro de fornecedor/cliente para o código:' && me->zif_integracao_app_produtor~at_fornecedor_sap && ' !'.
      me->zif_integracao_app_produtor~at_retorno_termo-protocolo = me->zif_integracao_app_produtor~at_protocolo.
      EXIT.
*-US 135191-18-06-2024-#135191-RJF-inicio
    ENDIF.
*-US 135191-18-06-2024-#135191-RJF-fim
  ENDIF.

*-------------------------------------------------------------------------------------------------*
* Consistir CNPJ/CPF Duplicado
*-------------------------------------------------------------------------------------------------*
*  SELECT SINGLE *
*    from lfa1 INTO @DATA(lwa_lfa1)
*   WHERE lifnr EQ @me->zif_integracao_app_produtor~at_fornecedor_sap.
*
*  IF SY-SUBRC EQ 0.
*
*    if lwa_lfa1-stcd2 IS NOT INITIAL.
*
*      SELECT SINGLE valfrom INTO @DATA(lwa_setleaf_dup1)
*       FROM setleaf
*      WHERE setname EQ 'Z_VERIF_CPF'
*        AND valfrom EQ @lwa_lfa1-ktokk.
*
*      if sy-subrc eq 0.
*
*        SELECT SINGLE *
*          FROM lfa1 INTO @DATA(lwa_lfa1_dup)
*         WHERE stcd2 EQ @lwa_lfa1-stcd2
*           AND lifnr NE @lwa_lfa1-lifnr
*           AND ktokk NE 'ZMOT'
*           AND sperr NE 'X'
*           AND loevm NE 'X'.
*
*        IF SY-SUBRC EQ 0.
*          me->zif_integracao_app_produtor~at_retorno_termo-fornecedor = lwa_lfa1-lifnr.
*          me->zif_integracao_app_produtor~at_retorno_termo-nr_msg     = '999'.
*          me->zif_integracao_app_produtor~at_retorno_termo-tp_msg     = 'E'.
*          me->zif_integracao_app_produtor~at_retorno_termo-vl_msg     = 'Existe mais de um cadastro com o mesmo CPF do produtor informado!'.
*          me->zif_integracao_app_produtor~at_retorno_termo-protocolo  = me->zif_integracao_app_produtor~at_protocolo.
*          EXIT.
*        ENDIF.
*
*      ENDIF.
*
*    ELSEIF lwa_lfa1-stcd1 IS NOT INITIAL .
*
*      SELECT SINGLE valfrom
*        FROM setleaf INTO @DATA(lwa_setleaf_dup)
*       WHERE setname = 'MAGGI_FORN_DUPL'
*         AND valfrom = @lwa_lfa1-regio.
*
*      IF SY-SUBRC NE 0.
*        SELECT SINGLE *
*          FROM lfa1 INTO lwa_lfa1_dup
*         WHERE stcd1 EQ lwa_lfa1-stcd1
*           AND lifnr NE lwa_lfa1-lifnr
*           AND sperr NE 'X'
*           AND loevm NE 'X'.
*
*        IF SY-SUBRC EQ 0.
*          me->zif_integracao_app_produtor~at_retorno_termo-fornecedor = lwa_lfa1-lifnr.
*          me->zif_integracao_app_produtor~at_retorno_termo-nr_msg     = '999'.
*          me->zif_integracao_app_produtor~at_retorno_termo-tp_msg     = 'E'.
*          me->zif_integracao_app_produtor~at_retorno_termo-vl_msg     = 'Existe mais de um cadastro com o mesmo CNPJ do produtor informado!'.
*          me->zif_integracao_app_produtor~at_retorno_termo-protocolo  = me->zif_integracao_app_produtor~at_protocolo.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.

*-------------------------------------------------------------------------------------------------*
* Consistir CNPJ/CPF Duplicado - Fim
*-------------------------------------------------------------------------------------------------*

  lva_j_1kftind = COND #( WHEN me->zif_integracao_app_produtor~at_aceite EQ 'S' THEN 'AL5BAN-A' ELSE 'AL5BAN-R' ).

*-US 135191-18-06-2024-#135191-RJF-inicio
**  tg_bdc = VALUE #(
**
**  ( dynbegin = abap_true  program = 'SAPMF02K'       dynpro = '0101'         )
**
**  ( dynbegin = abap_false fnam    = 'BDC_CURSOR'     fval = 'RF02K-D0120'    )
**  ( dynbegin = abap_false fnam    = 'BDC_OKCODE'     fval = '/00'            )
**  ( dynbegin = abap_false fnam    = 'RF02K-LIFNR'    fval = me->zif_integracao_app_produtor~at_fornecedor_sap   )
**  ( dynbegin = abap_false fnam    = 'RF02K-D0120'    fval = abap_true        )
**
**
**  ( dynbegin = abap_true  program = 'SAPMF02K'       dynpro = '0120'         )
**
**  ( dynbegin = abap_false fnam    = 'BDC_CURSOR'     fval = 'LFA1-J_1KFTIND' )
**  ( dynbegin = abap_false fnam    = 'BDC_OKCODE'     fval = '=UPDA'          )
**  ( dynbegin = abap_false fnam    = 'LFA1-J_1KFTIND' fval = lva_j_1kftind )
**
**   ).
**
***  opt = VALUE #( dismode = 'N' defsize = 'X').
**  opt = VALUE #(
**                  dismode  = 'S'
**                  racommit = abap_true
**                  nobinpt  = abap_true
**               ).
**
**  DATA(vl_mode) = 'N'.
**
**  lv_xk02_force_cadastro = abap_true.
**  EXPORT lv_xk02_force_cadastro FROM lv_xk02_force_cadastro TO MEMORY ID 'XK02_FORCE_CADASTRO'.
**
**  lv_xk02_termo_al5bank = abap_true.
**  EXPORT lv_xk02_termo_al5bank FROM lv_xk02_termo_al5bank TO MEMORY ID 'XK02_TERMO_AL5BANK'.
*-US 135191-18-06-2024-#135191-RJF-fim

* ---> S4 Migration - 29/06/2023 - JS
*  CALL TRANSACTION 'XK02' USING  tg_bdc
*                          OPTIONS FROM opt
*                          "MODE   vl_mode
*                          "UPDATE 'S'
*                          MESSAGES INTO tg_msg.

  DATA: lt_bdc    TYPE bdcdata_tab,
        lt_bdcmsg TYPE tab_bdcmsgcoll,
        wa_lfa1   TYPE lfa1,
        wa_kna1   TYPE kna1. "*-US 135191-18-06-2024-#135191-RJF

  DATA: lo_migbp TYPE REF TO /mignow/cl_migbp.

*  lt_bdc = CONV #( tg_bdc[] ). "*-US 135191-18-06-2024-#135191-RJF -comentado

  CREATE OBJECT lo_migbp
    EXPORTING
      im_test  = abap_false
      im_tcode = 'BP'.
*-US 135191-18-06-2024-#135191-RJF-inicio
*      it_bdcdata = lt_bdc.

*  CALL METHOD lo_migbp->mt_bp_process_old_shdb(
*    CHANGING
*      ct_bdcmsg = lt_bdcmsg ).

  IF me->zif_integracao_app_produtor~at_fornecedor_sap IS NOT INITIAL.
    wa_lfa1-lifnr = me->zif_integracao_app_produtor~at_fornecedor_sap. "lwa_lfa1_check-lifnr.
    wa_lfa1-j_1kftind = lva_j_1kftind.
    wa_lfa1-j_1kfrepre = zif_integracao_app_produtor~at_origem.

  DATA(lv_af_termo_al5bank) = abap_true.
  EXPORT lv_af_termo_al5bank FROM lv_af_termo_al5bank TO MEMORY ID 'AF_TERMO_AL5BANK'.

  ELSE.
    IF me->zif_integracao_app_produtor~at_cliente IS NOT INITIAL.
      wa_kna1 = lwa_kna1_check.
      wa_kna1-kunnr = me->zif_integracao_app_produtor~at_cliente. "lwa_kna1_check-kunnr.
      wa_kna1-j_1kftind = lva_j_1kftind.
      wa_kna1-j_1kfrepre = zif_integracao_app_produtor~at_origem.

  DATA(lv_ac_termo_al5bank) = abap_true.
  EXPORT lv_ac_termo_al5bank FROM lv_ac_termo_al5bank TO MEMORY ID 'AC_TERMO_AL5BANK'.

    ENDIF.
  ENDIF.

  CALL METHOD lo_migbp->mt_set_data_directly(
    EXPORTING
      is_lfa1 = wa_lfa1
      is_kna1 = wa_kna1 ).
*-US 135191-18-06-2024-#135191-RJF-fim

  CALL METHOD lo_migbp->mt_bp_process_data( CHANGING ct_bdcmsg = lt_bdcmsg ).

  tg_msg = CONV #( lt_bdcmsg[] ).
* <--- S4 Migration - 29/06/2023 - JS

  DATA(_sucess_update) = abap_false.
  DO 4 TIMES.

    IF me->zif_integracao_app_produtor~at_fornecedor_sap IS NOT INITIAL. " RJF - CS2024000164 CLIENTES e FORNECEDORES
      SELECT SINGLE *
        FROM lfa1 INTO lwa_lfa1_check
       WHERE lifnr EQ me->zif_integracao_app_produtor~at_fornecedor_sap.

      IF sy-subrc EQ 0 AND ( lwa_lfa1_check-j_1kftind EQ lva_j_1kftind ).
        _sucess_update = abap_true.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
      ENDIF.
*-US 135191-18-06-2024-#135191-RJF-inicio
    ELSE.
      IF me->zif_integracao_app_produtor~at_cliente IS NOT INITIAL.
        SELECT SINGLE *
          FROM kna1 INTO lwa_kna1_check
         WHERE kunnr EQ me->zif_integracao_app_produtor~at_cliente.

        IF sy-subrc EQ 0 AND ( lwa_kna1_check-j_1kftind EQ lva_j_1kftind ).
          _sucess_update = abap_true.
          EXIT.
        ELSE.
          WAIT UP TO 2 SECONDS.
        ENDIF.
      ENDIF.
*-US 135191-18-06-2024-#135191-RJF-fim

*      ELSE.
*        WAIT UP TO 2 SECONDS.
    ENDIF.
  ENDDO.


  IF _sucess_update EQ abap_true.
    me->zif_integracao_app_produtor~at_retorno_termo-fornecedor = lwa_lfa1_check-lifnr.
    me->zif_integracao_app_produtor~at_retorno_termo-nr_msg     = '200'.
    me->zif_integracao_app_produtor~at_retorno_termo-tp_msg     = 'S'.
    me->zif_integracao_app_produtor~at_retorno_termo-vl_msg     = 'Cadastro fornecedor/cliente atualizado com sucesso!'.
    me->zif_integracao_app_produtor~at_retorno_termo-protocolo  = me->zif_integracao_app_produtor~at_protocolo.
  ELSE.
    me->zif_integracao_app_produtor~at_retorno_termo-fornecedor = lwa_lfa1_check-lifnr.
    me->zif_integracao_app_produtor~at_retorno_termo-nr_msg     = '999'.
    me->zif_integracao_app_produtor~at_retorno_termo-tp_msg     = 'E'.
    me->zif_integracao_app_produtor~at_retorno_termo-vl_msg     = 'Não foi possivel realizar a alteração no cadastro do fornecedor/cliente!'.
    me->zif_integracao_app_produtor~at_retorno_termo-protocolo  = me->zif_integracao_app_produtor~at_protocolo.
  ENDIF.

*  READ TABLE tg_msg INTO DATA(wa_msg) INDEX 1.
*
*  me->zif_integracao_app_produtor~at_retorno_termo-tipo = '4'.
*  me->zif_integracao_app_produtor~at_retorno_termo-fornecedor = me->zif_integracao_app_produtor~at_fornecedor_sap.
*  me->zif_integracao_app_produtor~at_retorno_termo-nr_msg = wa_msg-msgnr.
*  me->zif_integracao_app_produtor~at_retorno_termo-tp_msg = wa_msg-msgtyp.
*
*  CALL FUNCTION 'FORMAT_MESSAGE'
*    EXPORTING
*      id        = wa_msg-msgid
*      lang      = sy-langu
*      no        = wa_msg-msgnr
*      v1        = wa_msg-msgv1
*      v2        = wa_msg-msgv2
*      v3        = wa_msg-msgv3
*      v4        = wa_msg-msgv4
*    IMPORTING
*      msg       = me->zif_integracao_app_produtor~at_retorno_termo-vl_msg
*    EXCEPTIONS
*      not_found = 1
*      OTHERS    = 2.

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
ENDCLASS.
