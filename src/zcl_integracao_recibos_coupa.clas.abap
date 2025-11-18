class ZCL_INTEGRACAO_RECIBOS_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_RECIBOS_COUPA .

  data AT_TAXA_CAMBIO_STRUCT type ZMMS_DADOS_TX_CAMBIO_COUPA .

  methods CONSTRUCTOR
    importing
      !I_SERVICO type ZTIPOWEBSERV optional .
protected section.
private section.

  types:
    type_t_bapiesllc TYPE TABLE OF bapiesllc .
  types:
    type_t_bapiesklc TYPE TABLE OF bapiesklc .
  types:
    type_t_bapiesknc TYPE TABLE OF bapiesknc .
  types:
    type_t_bdcmsgcoll TYPE TABLE OF bdcmsgcoll .

  data AT_BAPIESLLC type TYPE_T_BAPIESLLC .
  data AT_BAPIESKLC type TYPE_T_BAPIESKLC .
  data AT_BAPIESKNC type TYPE_T_BAPIESKNC .
  data AT_BAPIESSRC type BAPIESSRC .
  data AT_MSG type TYPE_T_BDCMSGCOLL .

  methods CRIA_FOLHA_SERVICO .
  methods COMMIT_WORK .
  methods RETORNA_COUPA .
  methods BUSCA_ITENS_DETALHES
    importing
      !I_PO_NUMBER type EBELN .
  methods PROCESSO_CRIACAO_FOLHA
    changing
      !CWA_SERVICO type ZCOUPA_SERVICOS .
ENDCLASS.



CLASS ZCL_INTEGRACAO_RECIBOS_COUPA IMPLEMENTATION.


  METHOD busca_itens_detalhes.

    DATA: lwa_poheader  TYPE bapimepoheader,
          lwa_bapiesllc TYPE bapiesllc,
          lwa_bapiesklc TYPE bapiesklc.

    DATA: lit_return            TYPE TABLE OF bapiret2,
          lit_poservices        TYPE TABLE OF bapiesllc,
          lit_posrvaccessvalues TYPE TABLE OF bapiesklc.

    DATA: lva_lineno   TYPE srv_line_no,
          lva_ext_line TYPE srv_line_no,
          lva_packno   TYPE packno.

*---> 01/07/2023 - Migração S4 - EJ
    CALL FUNCTION 'BAPI_PO_GETDETAIL1'        "#EC CI_USAGE_OK[2438131]
*<--- 01/07/2023 - Migração S4 - EJ
      EXPORTING
        purchaseorder     = i_po_number
        services          = 'X'
      IMPORTING
        poheader          = lwa_poheader
      TABLES
        return            = lit_return
        poservices        = lit_poservices
        posrvaccessvalues = lit_posrvaccessvalues.

    CLEAR: lva_lineno, lva_ext_line, lva_packno.

    lva_packno = 1.

    lwa_bapiesllc-pckg_no = '1'.
    lwa_bapiesllc-line_no = '1'.
    lwa_bapiesllc-outl_ind = 'X'.
    lwa_bapiesllc-subpckg_no = '2'.
    APPEND lwa_bapiesllc TO me->at_bapiesllc.

    LOOP AT lit_poservices INTO DATA(lwa_poservices).
      IF sy-tabix = 1.
        CONTINUE.
      ENDIF.

      DATA(lv_tabix) = sy-tabix.

      MOVE-CORRESPONDING lwa_poservices TO lwa_bapiesllc.

      CLEAR: lwa_bapiesllc-pln_pckg,
             lwa_bapiesllc-pln_line,
             lwa_bapiesllc-ext_line.

      IF at_bapiessrc-short_text IS INITIAL.
        at_bapiessrc-short_text = lwa_bapiesllc-short_text.
      ENDIF.

      lwa_bapiesllc-pckg_no   = lv_tabix.
      lwa_bapiesllc-line_no   = lv_tabix.

      APPEND lwa_bapiesllc TO me->at_bapiesllc.
    ENDLOOP.

    CLEAR: lva_lineno, lva_ext_line.
    lva_lineno = 1.

    lva_packno   = 2.

    LOOP AT lit_posrvaccessvalues INTO DATA(lwa_posrvaccessvalues).

      lva_lineno = lva_lineno + 1.

      MOVE-CORRESPONDING lwa_posrvaccessvalues TO lwa_bapiesklc.

      lwa_bapiesklc-pckg_no = lva_packno.
      lwa_bapiesklc-line_no = lva_lineno.

      APPEND lwa_bapiesklc TO at_bapiesklc.

      lva_ext_line = lva_ext_line + 1.
    ENDLOOP.

  ENDMETHOD.


  METHOD commit_work.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    CALL FUNCTION 'DB_COMMIT'.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_recibos.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

    " Move as informacoes da taxa de cambio
*    me->at__struct = i_taxa_cambio.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_recibos_coupa~at_auth_ws
        WHERE service = 'COUPA_RECIBOS'.

    SELECT SINGLE * FROM zauth_webservice
      INTO me->zif_integracao_recibos_coupa~at_token_ws
        WHERE service = 'COUPA_TOKEN'.

    IF me->zif_integracao_recibos_coupa~at_auth_ws  IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD cria_folha_servico.

    DATA: lit_return             TYPE TABLE OF bapiret2,
          lit_servicos           TYPE TABLE OF zcoupa_servicos,
          lit_entrysheetservices TYPE TABLE OF bapiesllc.

    DATA: lwa_entrysheetheader   TYPE bapiessrc,
          lwa_entrysheetservices TYPE bapiesllc,
          lwa_retorno_coupa      TYPE zcoupa_servicos_retorno,
          lwa_zintegrcoupa01     TYPE zintegrcoupa01.

    DATA: lva_entrysheet    TYPE bapiessr-sheet_no,
          lva_servico_coupa TYPE c LENGTH 255,
          lva_servico_sap   TYPE c LENGTH 255,
          lva_offset        TYPE i,
          lva_ebeln         TYPE ekpo-ebeln,
          lva_ebelp         TYPE ekpo-ebelp,
          vg_return         TYPE string,
          wa_zob_mensagem   TYPE zob_mensagem,
          it_zob_mensagem   TYPE TABLE OF zob_mensagem.

    lit_servicos[] = me->zif_integracao_recibos_coupa~at_servicos[].

    LOOP AT lit_servicos INTO DATA(lwa_servicos).

      TRANSLATE lwa_servicos-status TO UPPER CASE.

      IF lwa_servicos-status <> 'VOIDED'.
        REFRESH: at_msg.

        me->processo_criacao_folha( CHANGING cwa_servico = lwa_servicos ).

        LOOP AT at_msg INTO DATA(lwa_msg) WHERE msgv1 IS NOT INITIAL
                                          AND msgtyp = 'S'.
          " Informações do log
          lwa_zintegrcoupa01-id_integr  = lwa_servicos-id.
          lwa_zintegrcoupa01-ident_proc	=	'FS'.
          lwa_zintegrcoupa01-fields     = lwa_msg-msgv1.

          lwa_zintegrcoupa01-dt_atual	  =	sy-datum.
          lwa_zintegrcoupa01-hr_atual	  =	sy-uzeit.
          MODIFY zintegrcoupa01 FROM lwa_zintegrcoupa01.

          me->commit_work( ).

          lwa_retorno_coupa-exported  = 'TRUE'.
          lwa_retorno_coupa-id        = lwa_servicos-id.
          lwa_retorno_coupa-num_folha = lwa_msg-msgv1.
          APPEND lwa_retorno_coupa TO me->zif_integracao_recibos_coupa~at_tt_retorno.
          EXIT.
        ENDLOOP.
        "
        REFRESH: it_zob_mensagem.
        LOOP AT at_msg INTO lwa_msg.
          WRITE :/ lwa_msg-msgtyp,lwa_msg-msgspra,lwa_msg-msgid,lwa_msg-msgnr,lwa_msg-msgv1,lwa_msg-msgv2.
          CALL FUNCTION 'MESSAGE_PREPARE'
            EXPORTING
              msg_id                 = lwa_msg-msgid
              msg_no                 = lwa_msg-msgnr
              msg_var1               = lwa_msg-msgv1(50)
              msg_var2               = lwa_msg-msgv2(50)
              msg_var3               = lwa_msg-msgv3(50)
              msg_var4               = lwa_msg-msgv4(50)
            IMPORTING
              msg_text               = vg_return
            EXCEPTIONS
              function_not_completed = 1
              message_not_found      = 2
              OTHERS                 = 3.
          "
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZOB_MENSG'
            IMPORTING
              number                  = wa_zob_mensagem-seq_registro
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.

          CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
            EXPORTING
              i_date   = sy-datum
              i_time   = sy-uzeit
            IMPORTING
              e_tstamp = wa_zob_mensagem-timestamp.


          wa_zob_mensagem-obj_key         = lwa_servicos-id.
          wa_zob_mensagem-interface       = '15'.
          wa_zob_mensagem-dt_atualizacao  = sy-datum.
          wa_zob_mensagem-hr_atualizacao  = sy-uzeit.
          wa_zob_mensagem-type            = lwa_msg-msgtyp.
          wa_zob_mensagem-id              = 'FS'.
          wa_zob_mensagem-num             = '899'.
          wa_zob_mensagem-message         = vg_return.
          APPEND wa_zob_mensagem TO it_zob_mensagem.
        ENDLOOP.
        IF it_zob_mensagem[] IS NOT INITIAL.
          MODIFY zob_mensagem FROM TABLE it_zob_mensagem.
          COMMIT WORK.
        ENDIF.


        IF at_msg[] IS INITIAL.
          lwa_retorno_coupa-exported  = 'TRUE'.
          lwa_retorno_coupa-id        = lwa_servicos-id.
          lwa_retorno_coupa-num_folha = lwa_msg-msgv1.
          APPEND lwa_retorno_coupa TO me->zif_integracao_recibos_coupa~at_tt_retorno.
        ENDIF.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lwa_servicos-order_line-order_header_number
          IMPORTING
            output = lva_ebeln.

        lva_ebelp = lwa_servicos-order_line-line_num.
        MULTIPLY lva_ebelp BY 10.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lva_ebelp
          IMPORTING
            output = lva_ebelp.

        SELECT SINGLE *
          FROM essr
          INTO @DATA(lwa_essr)
          WHERE lblne = @lwa_servicos-id
            AND ebeln = @lva_ebeln
            AND ebelp = @lva_ebelp
            AND loekz NE 'X'.
        IF sy-subrc IS INITIAL.
          "----------------------------------------
          " DELETE SERV.ENTRYSHEET
          "----------------------------------------
          IF  lwa_essr-kzabn EQ 'X'.
            lwa_zintegrcoupa01-id_integr  = lwa_servicos-id.
            lwa_zintegrcoupa01-ident_proc	=	'FS'.
            lwa_zintegrcoupa01-status = 'F'.
            lwa_zintegrcoupa01-dt_atual    = sy-datum.
            lwa_zintegrcoupa01-hr_atual	  =	sy-uzeit.
            MODIFY zintegrcoupa01 FROM lwa_zintegrcoupa01.
            me->commit_work( ).

            lwa_retorno_coupa-id     = lwa_servicos-id.
            lwa_retorno_coupa-exported = 'FALSE'.
            CLEAR lwa_retorno_coupa-num_folha.
            APPEND lwa_retorno_coupa TO me->zif_integracao_recibos_coupa~at_tt_retorno.
            "
            REFRESH: lit_return, lit_entrysheetservices.
            CLEAR: lwa_entrysheetheader, lwa_entrysheetservices, lva_entrysheet.
            CONTINUE.
          ELSEIF sy-subrc = 0.
            CALL FUNCTION 'BAPI_ENTRYSHEET_DELETE'
              EXPORTING
                entrysheet = lwa_essr-lblni
              TABLES
                return     = lit_return.
          ENDIF.
        ELSE.
          lwa_retorno_coupa-exported  = 'TRUE'.
          lwa_retorno_coupa-id        = lwa_servicos-id.
          lwa_retorno_coupa-num_folha = lwa_msg-msgv1.
          APPEND lwa_retorno_coupa TO me->zif_integracao_recibos_coupa~at_tt_retorno.
        ENDIF.


*      CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
*        EXPORTING
*          entrysheetheader          = at_bapiessrc
*        IMPORTING
*          entrysheet                = lva_entrysheet
*        TABLES
*          entrysheetservices        = me->at_bapiesllc
*          entrysheetsrvaccassvalues = me->at_bapiesklc
*          return                    = lit_return.



        " Trata retorno
        READ TABLE lit_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          lwa_retorno_coupa-id     = lwa_servicos-id.
          lwa_retorno_coupa-exported = 'FALSE'.

          lwa_zintegrcoupa01-status = 'F'.
        ELSE.

          lwa_zintegrcoupa01-status = 'T'.

          me->commit_work( ).

          lwa_retorno_coupa-id     = lwa_servicos-id.

          " Verifica se esta sendo excluido ou criado
          " para que possa ser tratado o retorno
          " VOID -> Processo de exclusão.
          IF lwa_servicos-status <> 'VOIDED'.
            lwa_retorno_coupa-exported = 'TRUE'.
          ELSE.
            CLEAR: lwa_retorno_coupa-exported.
          ENDIF.

          APPEND lwa_retorno_coupa TO me->zif_integracao_recibos_coupa~at_tt_retorno.

          " Informações do log
          lwa_zintegrcoupa01-id_integr  = lwa_servicos-id.
          lwa_zintegrcoupa01-ident_proc	=	'FS'.
          lwa_zintegrcoupa01-dt_atual	  =	sy-datum.
          lwa_zintegrcoupa01-hr_atual	  =	sy-uzeit.
          MODIFY zintegrcoupa01 FROM lwa_zintegrcoupa01.

          me->commit_work( ).

          REFRESH: lit_return, lit_entrysheetservices.

          CLEAR: lwa_entrysheetheader, lwa_entrysheetservices, lva_entrysheet.

        ENDIF.
      ENDIF.
    ENDLOOP.

    me->retorna_coupa( ).

  ENDMETHOD.


  METHOD processo_criacao_folha.

    DATA:

      lt_po_items    TYPE TABLE OF bapiekpo,
      lt_po_services TYPE TABLE OF bapiesll,
      lt_ekkn        TYPE TABLE OF bapiekkn,
      lt_po_acc      TYPE TABLE OF bapieskl,
      lt_po_return   TYPE TABLE OF bapireturn,
      "BAPI
      lt_bapi_eskn   TYPE TABLE OF bapiesknc,
      lt_bapi_esll   TYPE TABLE OF bapiesllc,
      lt_bapi_eskl   TYPE TABLE OF bapiesklc,
      lt_bapi_return TYPE TABLE OF bapiret2,
      wt_msg         LIKE LINE OF at_msg.

    DATA:
      lw_po_header   TYPE bapiekkol,
      lw_po_items    TYPE bapiekpo,
      lw_po_services TYPE bapiesll,
      lw_po_return   TYPE bapireturn,
      "BAPI
      lw_bapi_header TYPE bapiessrc,
      lw_bapi_esll   TYPE bapiesllc,
      lw_bapi_return TYPE bapiret2,
      lw_out_return  TYPE bapireturn1,
      "BalLog
      lw_log_handle  TYPE balloghndl.
    DATA:
      lv_mess           TYPE string,
      v_lblni           TYPE lblni,
      lv_strlen         TYPE i,
      lv_line_no        TYPE bapiesllc-line_no,
      lv_pckg_no        TYPE bapiesll-pckg_no,
      lv_subpckg_no     TYPE bapiesll-subpckg_no,
      lv_gross_val_unit TYPE bapiesll-gross_val,
      lv_ref_doc_no     TYPE bapiessrc-ref_doc_no,
      lv_acceptance     TYPE bapiessr-acceptance.



    " Estruturas
    DATA: lwa_poheader  TYPE bapimepoheader.

    " Variaveis
    DATA: lva_ebeln     TYPE ebeln,
          lva_ebelp     TYPE ebelp,
          lva_descricao TYPE string,
          lva_quantity  TYPE c LENGTH 20,
          lv_menge      TYPE menge_d.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cwa_servico-order_line-order_header_number
      IMPORTING
        output = lva_ebeln.

    lva_ebelp = cwa_servico-order_line-line_num.
    MULTIPLY lva_ebelp BY 10.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lva_ebelp
      IMPORTING
        output = lva_ebelp.


    CALL FUNCTION 'BAPI_PO_GETDETAIL' "#EC CI_USAGE_OK[2438131]
      EXPORTING                       "#EC CI_USAGE_OK[1803189]
        purchaseorder              = lva_ebeln
        items                      = 'X'
        account_assignment         = 'X'
        services                   = 'X'
      IMPORTING
        po_header                  = lw_po_header
      TABLES
        po_items                   = lt_po_items
        po_item_account_assignment = lt_ekkn
        po_item_services           = lt_po_services
        po_item_srv_accass_values  = lt_po_acc
        return                     = lt_po_return.


    READ TABLE lt_po_items INTO lw_po_items WITH KEY po_item = lva_ebelp.
    READ TABLE lt_po_services INTO lw_po_services WITH KEY pckg_no = lw_po_items-pckg_no.
    DELETE lt_po_services WHERE pckg_no NE lw_po_services-pckg_no AND pckg_no NE lw_po_services-subpckg_no .
    DELETE lt_po_acc      WHERE pckg_no NE lw_po_services-pckg_no AND pckg_no NE lw_po_services-subpckg_no .
    "
    lw_bapi_header-po_number    = lw_po_items-po_number.
    lw_bapi_header-po_item      = lw_po_items-po_item.


    lw_bapi_header-acceptance   = ' '.

    lw_bapi_header-doc_date     = sy-datum.
    lw_bapi_header-post_date    = sy-datum.

    IF lw_po_items-acctasscat = 'U'.
      lw_bapi_header-accasscat  = 'K'.
    ELSE.
      lw_bapi_header-accasscat  = lw_po_items-acctasscat.
    ENDIF.

    lw_bapi_header-pckg_no      = 1."lw_po_items-pckg_no.

    lw_bapi_header-ref_doc_no   = cwa_servico-id.
    lw_bapi_header-ext_number   = cwa_servico-id.

    " novo
    DATA(tabix) = sy-tabix.
    SORT lt_po_services BY line_no.
    LOOP AT lt_po_services INTO lw_po_services.
      tabix = sy-tabix.

      MOVE-CORRESPONDING lw_po_services TO lw_bapi_esll.
      MOVE cwa_servico-total TO lva_quantity.
      lv_menge = lva_quantity.
      FREE: lva_quantity.
      lw_bapi_esll-quantity = lv_menge.


      IF lw_bapi_header-short_text IS INITIAL.
        lw_bapi_header-short_text = lw_po_services-short_text.
      ENDIF.


      lw_bapi_esll-pckg_no = tabix.
      lw_bapi_esll-pln_line = 0.
      lw_bapi_esll-pln_pckg = 0.

      IF lw_bapi_esll-pckg_no = 1.
        lw_bapi_esll-subpckg_no = 2.
      ENDIF.

      APPEND lw_bapi_esll TO lt_bapi_esll.

    ENDLOOP.

    lt_bapi_eskl = CORRESPONDING #( lt_po_acc ).


    IF lt_bapi_eskl IS NOT INITIAL.

      LOOP AT lt_bapi_eskl ASSIGNING FIELD-SYMBOL(<fs_eskl>).
        <fs_eskl>-pckg_no = 2.
      ENDLOOP.

      LOOP AT lt_po_acc ASSIGNING FIELD-SYMBOL(<fs_acc_po>).

        APPEND INITIAL LINE TO lt_bapi_eskn ASSIGNING FIELD-SYMBOL(<fs_eskn>).

        <fs_eskn>-pckg_no = 2.

        <fs_eskn>-serial_no = <fs_acc_po>-serial_no.

        READ TABLE lt_ekkn ASSIGNING FIELD-SYMBOL(<fs_ekkn>) WITH KEY po_item = lva_ebelp
                                                                      serial_no = <fs_acc_po>-serial_no.

        CHECK sy-subrc EQ 0.

        <fs_eskn>-gl_account  = <fs_ekkn>-g_l_acct.
        <fs_eskn>-bus_area    = <fs_ekkn>-bus_area.
        <fs_eskn>-costcenter  = <fs_ekkn>-cost_ctr.
        <fs_eskn>-co_area     = <fs_ekkn>-co_area.
        <fs_eskn>-profit_ctr  = <fs_ekkn>-profit_ctr.
        <fs_eskn>-order       = <fs_ekkn>-order_no.
        <fs_eskn>-routing_no  = <fs_ekkn>-routing_no.

        SELECT SINGLE *
               INTO @DATA(wl_ekkn)
               FROM ekkn
               WHERE ebeln EQ @lva_ebeln
                 AND ebelp EQ @lva_ebelp.

        IF wl_ekkn-aufpl IS NOT INITIAL AND wl_ekkn-aplzl IS NOT INITIAL.
          SELECT SINGLE vornr
                 INTO @DATA(lv_vornr)
                 FROM afvc
                 WHERE aufpl EQ @wl_ekkn-aufpl
                   AND aplzl EQ @wl_ekkn-aplzl.
          IF lv_vornr IS NOT INITIAL.
            <fs_eskn>-activity  =  lv_vornr.
            CLEAR lv_vornr.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

    CLEAR v_lblni.
    CALL FUNCTION 'BAPI_ENTRYSHEET_CREATE'
      EXPORTING
        entrysheetheader            = lw_bapi_header
        testrun                     = ''
      IMPORTING
        entrysheet                  = v_lblni
      TABLES
        entrysheetaccountassignment = lt_bapi_eskn
        entrysheetservices          = lt_bapi_esll
        entrysheetsrvaccassvalues   = lt_bapi_eskl
        return                      = lt_bapi_return.
    READ TABLE lt_bapi_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc NE 0 AND v_lblni IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
    "
    REFRESH  at_msg.
    LOOP AT lt_bapi_return INTO DATA(wa_bapi_return).
      wt_msg-msgtyp    = wa_bapi_return-type.
      wt_msg-msgspra   = 'P'.
      wt_msg-msgid     = wa_bapi_return-id.
      wt_msg-msgnr     = wa_bapi_return-number.
      wt_msg-msgv1     = wa_bapi_return-message_v1.
      wt_msg-msgv2     = wa_bapi_return-message_v2.
      wt_msg-msgv3     = wa_bapi_return-message_v3.
      wt_msg-msgv4     = wa_bapi_return-message_v4.
      APPEND wt_msg TO at_msg.
    ENDLOOP.


  ENDMETHOD.


  METHOD retorna_coupa.

    DATA: lva_metodo   TYPE string,
          lva_http_url TYPE string,
          lva_xml      TYPE string,
          lva_id       TYPE zde_id_integracao.

    DATA: lwa_rif_ex   TYPE REF TO cx_root,
          lwa_var_text TYPE string VALUE 'Error in Imput XML File'.

    DATA: e_integracao_return TYPE zintegracao_log.

    TRY .

        lva_metodo = 'PUT'.

        LOOP AT me->zif_integracao_recibos_coupa~at_tt_retorno INTO DATA(lwa_retorno).

          DATA(o_coupa_r) = NEW zcl_integracao_recibos_coupa_r( i_wa_valores = lwa_retorno )..

            IF lwa_retorno-exported IS INITIAL.
              lva_http_url = '/receiving_transactions/' && lwa_retorno-id && '/void?fields=["id"]'.
            ELSEIF lwa_retorno-exported = 'TRUE'.
              lva_http_url = '/receiving_transactions/' && lwa_retorno-id && '?exported=true&fields=["id"]'.
            ELSEIF lwa_retorno-exported = 'FALSE'.
              lva_http_url = '/receiving_transactions/' && lwa_retorno-id && '?exported=false&fields=["id"]'.
            ENDIF.

          o_coupa_r->zif_integracao_recibos_coupa_r~get_xml( IMPORTING e_xml = me->zif_integracao_recibos_coupa~at_xml ).

          o_coupa_r->zif_integracao_recibos_coupa_r~set_ds_data( i_xml = me->zif_integracao_recibos_coupa~at_xml ).

          CLEAR: lva_id.
          lva_id = lwa_retorno-id.

          o_coupa_r->zif_integracao_recibos_coupa_r~set_ds_url( e_http_url = lva_http_url e_metodo = lva_metodo
            )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao)
                                       e_integracao    = DATA(e_integracao) ).

          o_coupa_r->zif_integracao_recibos_coupa_r~get_retorno( IMPORTING e_retorno = DATA(e_retorno) ).

          FREE: o_coupa_r.
        ENDLOOP.

      CATCH cx_root INTO lwa_rif_ex.
        MESSAGE lwa_var_text TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_true.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.

    TRY .
        CAST zcl_integracao_token_coupa(
               zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                 )->get_token( )
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

    APPEND VALUE #( name = 'Accept' value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    DATA: lit_result_xml TYPE abap_trans_resbind_tab.

    DATA: lwa_result_xml TYPE abap_trans_resbind,
          lwa_servicos   TYPE zcoupa_servicos,
          lwa_rif_ex     TYPE REF TO cx_root,
          lwa_var_text   TYPE string VALUE 'Error in Imput XML File'.

    DATA: lva_retorno TYPE string.

    IF i_msg_completa-qt_erro GT 0.
      RETURN.
    ENDIF.

    CLEAR: lit_result_xml,
           lit_result_xml[],
           lwa_servicos,
           me->zif_integracao_recibos_coupa~at_servicos[].

    GET REFERENCE OF me->zif_integracao_recibos_coupa~at_servicos INTO lwa_result_xml-value.
    lwa_result_xml-name = 'INVENTORY-TRANSACTION'.
    APPEND lwa_result_xml TO lit_result_xml.

    TRY.
        CALL TRANSFORMATION zt_coupa_servico
        SOURCE XML i_msg_retorno
        RESULT (lit_result_xml).

        me->cria_folha_servico( ).

      CATCH cx_root INTO lwa_rif_ex.
        MESSAGE lwa_var_text TYPE 'E'.
    ENDTRY.

    me->zif_integracao_recibos_coupa~at_retorno = e_sucesso.

  ENDMETHOD.


  METHOD zif_integracao_recibos_coupa~get_id_referencia.

    r_if_integracao_recibos_coupa = me.

    e_referencia-id_referencia = sy-datum.
    e_referencia-tp_referencia = 'COUPA_SERVICO'.

  ENDMETHOD.


  METHOD zif_integracao_recibos_coupa~get_retorno.

    r_if_integracao_recibos_coupa = me.

    e_retorno = me->zif_integracao_recibos_coupa~at_retorno.

  ENDMETHOD.


  METHOD zif_integracao_recibos_coupa~get_xml.

    DATA: lv_full_xml TYPE string.


    e_xml = lv_full_xml.
  ENDMETHOD.


  METHOD zif_integracao_recibos_coupa~set_ds_data.

    r_if_integracao_recibos_coupa = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_xml.

  ENDMETHOD.


  METHOD zif_integracao_recibos_coupa~set_ds_url.

    r_if_integracao_recibos_coupa = me.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_recibos_coupa~at_auth_ws-url &&
                                                            e_http_url.
*    me->zif_integracao_inject~at_info_request_http-ds_body_xstring

    me->zif_integracao_inject~at_info_request_http-ds_metodo = e_metodo.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_recibos_coupa~set_id_referencia( ).
  ENDMETHOD.


  method ZIF_INTEGRACAO_RECIBOS_COUPA~SET_ID_REFERENCIA.

    r_if_integracao_recibos_coupa = me.

    me->zif_integracao_recibos_coupa~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  METHOD zif_integracao_recibos_coupa~set_recibo_buscar.

    r_if_integracao_recibos_coupa = me.

  ENDMETHOD.


  METHOD zif_integracao_recibos_coupa~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    DATA: e_integracao_log TYPE zintegracao_log.

    r_if_integracao_recibos_coupa = me.

    CREATE OBJECT lc_integrar.

    TRY.
        lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
          )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
          )->set_outbound_msg(
          )->set_processar_retorno(
          )->set_integrar_retorno(
          )->get_registro( IMPORTING e_integracao = e_integracao
          )->free( ).

        CLEAR: lc_integrar.
      CATCH zcx_error INTO DATA(lo_error).

        e_retorno_integracao = e_integracao_log.

        DATA(lc_msgid) = lo_error->zif_error~msgid.
        DATA(lc_msgno) = lo_error->zif_error~msgno.
        DATA(lc_msgv1) = lo_error->zif_error~msgv1.
        DATA(lc_msgv2) = lo_error->zif_error~msgv2.
        DATA(lc_msgv3) = lo_error->zif_error~msgv3.
        DATA(lc_msgv4) = lo_error->zif_error~msgv4.


        "Propagar erro de Comunicação para Fora
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = lc_msgid
                              msgno = lc_msgno
                              attr1 = CONV #( lc_msgv1 )
                              attr2 = CONV #( lc_msgv2 )
                              attr3 = CONV #( lc_msgv3 )
                              attr4 = CONV #( lc_msgv4 ) )
            msgid  = lc_msgid
            msgno  = lc_msgno
            msgty  = 'E'
            msgv1  = lc_msgv1
            msgv2  = lc_msgv2
            msgv3  = lc_msgv3
            msgv4  = lc_msgv4.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_integracao_recibos_coupa~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_integracao_recibos_coupa~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_RECIBOS_COUPA~SET_USER_BUSCAR.
  endmethod.
ENDCLASS.
