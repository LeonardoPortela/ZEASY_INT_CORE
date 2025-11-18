class ZCL_INTEGRACAO_COUPA_PED_COMP definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COUPA_PED_COMP .

  types:
    tt_xml_header TYPE STANDARD TABLE OF zstped_header WITH EMPTY KEY .
  types:
    tt_xml_lines  TYPE STANDARD TABLE OF zst_ped_lines .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional
    raising
      ZCX_INTEGRACAO .
  methods BUSCA_DADOS .
  methods REALIZA_QUEBRA_XML
    exporting
      !ET_XML_HEADER type TT_XML_HEADER
      !ET_XML_LINES type TT_XML_LINES .
  class-methods BAPI_PO_CHANGE
    importing
      !IV_TEST type XFELD optional
      !IW_POHEADER type BAPIMEPOHEADER
      !IV_NO_PRICE_FROM_PO type BAPIFLAG-BAPIFLAG optional
    exporting
      !EV_EBELN type EBELN
    changing
      !CT_POLIMITS type BAPIESUHC_TP optional
      !CT_POSRVACCESSVALUES type BAPIESKLC_TP optional
      !CT_POITEM type BAPIMEPOITEM_TP
      !CT_POCOND type BAPIMEPOCOND_TP optional
      !CT_POPARTNER type BAPIEKKOP_TP optional
      !CT_BAPIMEPOTEXTHEADER type BAPIMEPOTEXTHEADER_TP optional
      !CT_ACCOUNT type BAPIMEPOACCOUNT_TP optional
      !CT_SCHEDULE type BAPIMEPOSCHEDULE_TP optional
      !CT_BAPIESLLC type BAPIESLLC_TP optional
      !CT_POCONDHEADER type BAPIMEPOCONDHEADER_TP optional
      !CT_EXTENSION type BAPIPAREX_TP optional
    returning
      value(RT_RETURN) type BAPIRET2_T .
  methods GET_XML_HEADER
    returning
      value(RT_XML_HEADER) type TT_XML_HEADER .
  methods DETERMINA_CRIACAO_MODIFICACAO
    importing
      !IV_TEST type FLAG
    changing
      !CT_XML_HEADER type TT_XML_HEADER .
  methods FORMATA_TXT
    importing
      !IV_IN type STRING
    returning
      value(RV_OUT) type STRING .
  methods ATUALIZA_IVA_PEDIDO_COUPA
    importing
      !IT_XML_LINES type TT_XML_LINES .
  methods ATUALIZA_EXPORTED_PEDIDO_IVA
    importing
      !IW_XML_HEADER type ZSTPED_HEADER .
  PROTECTED SECTION.
private section.

  data LO_INTEGRACAO type ref to ZCL_INTEGRACAO .
  data T_XML_HEADER type TT_XML_HEADER .
  data T_XML_LINES type TT_XML_LINES .
  data O_INTEGRACAO_COUPA_PED_COMP type ref to ZCL_INTEGRACAO_COUPA_PED_COMP .
  data T_ESLL type ESLL_TT .
  data T_EBAN type MEOUT_T_EBAN .

  methods ATUALIZA_MENSAGEM_PEDIDO_COUPA
    importing
      !IW_XML_HEADER type ZSTPED_HEADER .
  methods ATUALIZA_EXPORTED_PEDIDO_COUPA
    importing
      !IW_XML_HEADER type ZSTPED_HEADER .
  methods REALIZA_MODIFICACAO_PO
    importing
      !IV_TEST type FLAG optional
    changing
      !CT_XML_HEADER type TT_XML_HEADER .
  methods REALIZA_LANCAMENTO_PO
    importing
      !IV_TEST type FLAG optional
    changing
      !CT_XML_HEADER type TT_XML_HEADER .
  methods DETERMINA_TIPO_PO
    importing
      !IW_XML_HEADER type ZSTPED_HEADER
    returning
      value(RV_BSART) type BSART .
  methods BAPI_PO_CREATE
    importing
      !IV_TEST type XFELD optional
      !IW_POHEADER type BAPIMEPOHEADER
      !IV_NO_PRICE_FROM_PO type BAPIFLAG-BAPIFLAG optional
    exporting
      !EV_EBELN type EBELN
    changing
      !CT_POCONDHEADER type BAPIMEPOCONDHEADER_TP optional
      !CT_POITEM type BAPIMEPOITEM_TP
      !CT_POCOND type BAPIMEPOCOND_TP optional
      !CT_POPARTNER type BAPIEKKOP_TP optional
      !CT_BAPIMEPOTEXTHEADER type BAPIMEPOTEXTHEADER_TP optional
      !CT_ACCOUNT type BAPIMEPOACCOUNT_TP optional
      !CT_SCHEDULE type BAPIMEPOSCHEDULE_TP optional
      !CT_BAPIESLLC type BAPIESLLC_TP optional
      !CT_POLIMITS type BAPIESUHC_TP optional
      !CT_POSRVACCESSVALUES type BAPIESKLC_TP optional
    returning
      value(RT_RETURN) type BAPIRET2_T
    exceptions
      FAILURE .
  class-methods SET_FIELDS_X
    importing
      !IW_ITEM type ANY
      !IV_REUSE_COMPONENTS type FLAG optional
    changing
      !CW_ITEMX type ANY .
  class-methods GET_STRUCTDESCR
    importing
      !I_DATA type ANY
    returning
      value(RT_COMPONENTES) type ABAP_COMPONENT_TAB .
  methods ATUALIZA_PEDIDO_COUPA
    importing
      !IV_ID type STRING
      !IV_TIPO type STRING optional .
  methods SELECIONA_SERVICO_REQUISICAO .
  methods VERIFICA_SERVICO_REQUISICAO
    importing
      !IV_BANFN type BANFN
      !IV_BNFPO type BNFPO
      value(IV_PRICE) type STRING optional
      value(IV_UNIT) type EPEIN optional
    changing
      !ET_POSERVICES type BAPIESLLC_TP
      !ET_POLIMITS type BAPIESUHC_TP
      !ET_POSRVACCESSVALUES type BAPIESKLC_TP
      !CV_SERIALNO type BAPIMEPOACCOUNT-SERIAL_NO
      !CV_PCKGNO type BAPIESUHC-PCKG_NO
      !CV_LINENO type SRV_LINE_NO
    returning
      value(RV_SERVICO) type FLAG .
  methods MANIPULA_PO_ACCOUNT
    importing
      !IV_EBELP type EBELP
      !IV_ACCTASSCAT type STRING
      !IW_RATEIO_ACCOUNT type ZST_PED_ACCOUNT_ALLOCATIONS optional
    changing
      !CT_POACCOUNT type BAPIMEPOACCOUNT_TP
      !CW_POITEM type BAPIMEPOITEM
      !CV_SERIAL_NO type BAPIMEPOACCOUNT-SERIAL_NO optional .
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_PED_COMP IMPLEMENTATION.


  METHOD atualiza_exported_pedido_coupa.

    IF ( o_integracao_coupa_ped_comp IS NOT BOUND ).
      CREATE OBJECT o_integracao_coupa_ped_comp
        EXPORTING
          i_servico = 'COUPA_INT_STATUS_PED_COMPRA'.
    ENDIF.

    o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_ds_url(
                                                          e_metodo     = 'PUT'
                                                          e_integracao = 'EXPORTED'
                                                          e_id_po      = iw_xml_header-id ).

    o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_send_msg(
                    IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                              e_integracao    = DATA(e_integracao_post) ).

  ENDMETHOD.


  METHOD atualiza_mensagem_pedido_coupa.
    DATA: lv_string     TYPE string.
    DATA: lt_filter     TYPE zif_integracao_coupa_ped_comp=>tt_filter.

    LOOP AT iw_xml_header-tt_return REFERENCE INTO DATA(lo_wa_return).
      IF ( lv_string IS NOT INITIAL ).
        CONCATENATE lv_string lo_wa_return->message INTO lv_string SEPARATED BY '/'.
      ELSE.
        lv_string = lo_wa_return->message.
      ENDIF.

      ENDLOOP.

      IF ( lv_string IS NOT INITIAL ).
        APPEND INITIAL LINE TO lt_filter REFERENCE INTO DATA(lo_wa_filter).
        lo_wa_filter->value = lv_string.
        lo_wa_filter->field = 'status-da-integrao'.
      endif.

      IF ( o_integracao_coupa_ped_comp IS NOT BOUND ).
        CREATE OBJECT o_integracao_coupa_ped_comp
          EXPORTING
            i_servico = 'COUPA_INT_STATUS_PED_COMPRA'.
      ENDIF.

      o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_ds_url(
                                                            e_metodo     = 'PUT'
                                                            e_integracao = 'PEDIDO'
                                                            e_id_po      = iw_xml_header-id
                                                            it_filter    = lt_filter ).

      o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_send_msg(
                      IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                                e_integracao    = DATA(e_integracao_post) ).

    ENDMETHOD.


  METHOD atualiza_pedido_coupa.

*    EXIT.  "Não vai mais mandar

    IF ( o_integracao_coupa_ped_comp IS NOT BOUND ).
      CREATE OBJECT o_integracao_coupa_ped_comp
        EXPORTING
          i_servico = 'COUPA_INT_STATUS_PED_COMPRA'.
    ENDIF.
*-CS1117561-#RIMINI-14.07.2023-BEGIN
    IF iv_tipo EQ 'MOD'.
       exit.
*      o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_ds_url(
*                                                            e_metodo     = 'PUT'
*                                                            e_integracao = 'ISSUE_WITHOUT_SEND'
*                                                            e_id_po      = iv_id ).
    ELSE.
*-CS1117561-#RIMINI-14.07.2023-END
    o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_ds_url(
                                                          e_metodo     = 'PUT'
                                                          e_integracao = 'ISSUED'
                                                          e_id_po      = iv_id ).
*-CS1117561-#RIMINI-14.07.2023-BEGIN
    ENDIF.
*-CS1117561-#RIMINI-14.07.2023-END
    o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_send_msg(
                    IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                              e_integracao    = DATA(e_integracao_post) ).

  ENDMETHOD.


  METHOD bapi_po_change.
    " Local structures
    DATA:
      lw_pocondheaderx TYPE bapimepocondheaderx,
      lw_poheaderx     TYPE bapimepoheaderx,
      lw_poitemx       TYPE bapimepoitemx,
      lw_pocondx       TYPE bapimepocondx,
      lw_accountx      TYPE bapimepoaccountx,
      lw_schedulex     TYPE bapimeposchedulx.


    " Local internal tables.
    DATA:
      lt_pocondheaderx TYPE STANDARD TABLE OF bapimepocondheaderx,
      lt_poitemx       TYPE STANDARD TABLE OF bapimepoitemx,
      lt_pocondx       TYPE STANDARD TABLE OF bapimepocondx,
      lt_accountx      TYPE STANDARD TABLE OF bapimepoaccountx,
      lt_schedulex     TYPE STANDARD TABLE OF bapimeposchedulx.

    CLEAR: lw_poheaderx.
    set_fields_x(
      EXPORTING
        iw_item  = iw_poheader
      CHANGING
        cw_itemx = lw_poheaderx
    ).

    IF iw_poheader-doc_type IS NOT INITIAL.
      lw_poheaderx-item_intvl = 'X'.
    ENDIF.

    DATA(_lines) = lines( ct_popartner ).
    IF iw_poheader-diff_inv   IS INITIAL AND _lines > 0.
      lw_poheaderx-diff_inv   = 'X'.
      lw_poheaderx-suppl_vend = 'X'.
    ENDIF.


    LOOP AT ct_pocondheader[] ASSIGNING FIELD-SYMBOL(<lfs_pocondheader>).
      CLEAR lw_pocondheaderx.
      set_fields_x(
        EXPORTING
          iw_item  = <lfs_pocondheader>
        CHANGING
          cw_itemx = lw_pocondheaderx
      ).
      INSERT lw_pocondheaderx INTO TABLE lt_pocondheaderx.
    ENDLOOP.

    LOOP AT ct_poitem[] ASSIGNING FIELD-SYMBOL(<lfs_poitem>).
      CLEAR lw_poitemx.
      set_fields_x(
        EXPORTING
          iw_item  = <lfs_poitem>
        CHANGING
          cw_itemx = lw_poitemx
      ).

      IF iw_poheader-doc_type IS NOT INITIAL.
        IF iw_poheader-doc_type = 'YCEF' OR iw_poheader-doc_type = 'YSEF'.
          lw_poitemx-gr_basediv = 'X'.
          CLEAR <lfs_poitem>-gr_basediv.
        ELSE.
          lw_poitemx-gr_basediv = 'X'.
          <lfs_poitem>-gr_basediv = 'X'.
        ENDIF.
      ELSE.
        CLEAR lw_poitemx-gr_basediv .
      ENDIF.

      CLEAR: lw_poitemx-prio_requirement, lw_poitemx-prio_urgency. "25.11.2022 ALRS
      INSERT lw_poitemx INTO TABLE lt_poitemx.
      IF <lfs_poitem>-no_more_gr = 'Z'.
        CLEAR <lfs_poitem>-no_more_gr.
      ENDIF.
      IF  <lfs_poitem>-final_inv = 'Z'.
        CLEAR <lfs_poitem>-final_inv.
      ENDIF.
    ENDLOOP.

    LOOP AT ct_pocond[] ASSIGNING FIELD-SYMBOL(<lfs_pocond>).
      CLEAR lw_pocondx.
      set_fields_x(
        EXPORTING
          iw_item  = <lfs_pocond>
        CHANGING
          cw_itemx = lw_pocondx
      ).

      lw_pocondx-condition_no = <lfs_pocond>-condition_no.

      INSERT lw_pocondx INTO TABLE lt_pocondx.
    ENDLOOP.

    LOOP AT ct_account[] ASSIGNING FIELD-SYMBOL(<lfs_account>).
      CLEAR lw_accountx.
      set_fields_x(
        EXPORTING
          iw_item  = <lfs_account>
        CHANGING
          cw_itemx = lw_accountx
      ).
      INSERT lw_accountx INTO TABLE lt_accountx.
    ENDLOOP.

    LOOP AT ct_schedule[] ASSIGNING FIELD-SYMBOL(<lfs_schedule>).
      CLEAR lw_schedulex.
      set_fields_x(
        EXPORTING
          iw_item  = <lfs_schedule>
        CHANGING
          cw_itemx = lw_schedulex
      ).
      INSERT lw_schedulex INTO TABLE lt_schedulex.
    ENDLOOP.

    LOOP AT ct_poitem ASSIGNING FIELD-SYMBOL(<po_item>).
      IF <po_item>-calctype = 'B'.
        CLEAR <po_item>-calctype.
      ELSE.
        <po_item>-calctype = 'G'.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_poitemx ASSIGNING FIELD-SYMBOL(<po_item_x>).
      READ TABLE ct_poitem INTO DATA(w_item) WITH KEY po_item = <po_item_x>-po_item.
      IF sy-subrc = 0 AND
         w_item-calctype IS INITIAL.
        CLEAR <po_item_x>-calctype.
      ELSE.
        <po_item_x>-calctype = 'X'.
      ENDIF.
    ENDLOOP.

    LOOP AT ct_pocond ASSIGNING FIELD-SYMBOL(<po_cond>).
      IF <po_cond>-cond_type EQ 'PBXX' OR <po_cond>-cond_type EQ 'PB00'.
        <po_cond>-change_id = 'U'. "Update.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_pocondx ASSIGNING FIELD-SYMBOL(<po_cond_x>).
      <po_cond_x>-change_id = 'X'. "Update.
    ENDLOOP.



    CALL FUNCTION 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        purchaseorder     = iw_poheader-po_number
        poheader          = iw_poheader
        poheaderx         = lw_poheaderx
        no_price_from_po  = iv_no_price_from_po
      TABLES
        return            = rt_return
        pocondheader      = ct_pocondheader
        pocondheaderx     = lt_pocondheaderx
        poitem            = ct_poitem
        poitemx           = lt_poitemx
        pocond            = ct_pocond
        pocondx           = lt_pocondx
        popartner         = ct_popartner
        poaccount         = ct_account
        poaccountx        = lt_accountx
        poschedule        = ct_schedule
        poschedulex       = lt_schedulex
        potextheader      = ct_bapimepotextheader
        poservices        = ct_bapiesllc
        polimits          = ct_polimits
        posrvaccessvalues = ct_posrvaccessvalues
        EXTENSIONIN       = ct_extension.

    IF iv_test IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.

      READ TABLE rt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        "
        IF _lines > 0.  "forçar parceiro
          REFRESH rt_return.
          CALL FUNCTION 'BAPI_PO_CHANGE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              purchaseorder = iw_poheader-po_number
              poheader      = iw_poheader
              poheaderx     = lw_poheaderx
            TABLES
              return        = rt_return
              popartner     = ct_popartner.

          READ TABLE rt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.
*            WAIT UP TO 3 SECONDS.
          ENDIF.
        ENDIF.
      ENDIF.
*-CS1114935-#RIMINI-07.10.2023-BEGIN
*- Excluir mensagem de warning "Erro durante a transferência dos dados
*  ExtensionIn para a ampliação CI_EKPODB"
      DELETE rt_return WHERE type   EQ 'W'
                         AND id     EQ 'ME'
                         AND number EQ '887'.
*-CS1114935-#RIMINI-07.10.2023-END
    ENDIF.


  ENDMETHOD.


  method bapi_po_create.
    " Local structures
    data:
      lw_pocondheaderx type bapimepocondheaderx,
      lw_poheaderx     type bapimepoheaderx,
      lw_poitemx       type bapimepoitemx,
      lw_pocondx       type bapimepocondx,
      lw_accountx      type bapimepoaccountx,
      lw_schedulex     type bapimeposchedulx.

    " Local internal tables.
    data:
      lt_pocondheaderx type standard table of bapimepocondheaderx,
      lt_poitemx       type standard table of bapimepoitemx,
      lt_pocondx       type standard table of bapimepocondx,
      lt_accountx      type standard table of bapimepoaccountx,
      lt_schedulex     type standard table of bapimeposchedulx.


    clear: lw_poheaderx.
    set_fields_x(
      exporting
        iw_item  = iw_poheader
      changing
        cw_itemx = lw_poheaderx
    ).

    loop at ct_pocondheader[] assigning field-symbol(<lfs_pocondheader>).
      clear lw_pocondheaderx.
      set_fields_x(
        exporting
          iw_item  = <lfs_pocondheader>
        changing
          cw_itemx = lw_pocondheaderx
      ).
      insert lw_pocondheaderx into table lt_pocondheaderx.
    endloop.

    lw_poheaderx-item_intvl = 'X'.

    loop at ct_poitem[] assigning field-symbol(<lfs_poitem>).
      clear lw_poitemx.
      set_fields_x(
        exporting
          iw_item  = <lfs_poitem>
        changing
          cw_itemx = lw_poitemx
      ).

      if iw_poheader-doc_type = 'YDBP'. "ALRS 05.10.2023
        lw_poitemx-gr_non_val = 'X'.
      endif.
      if ( 'YFTE_YEFI' cs iw_poheader-doc_type ).
        <lfs_poitem>-conf_ctrl = '0003'.
      else.
        clear: <lfs_poitem>-conf_ctrl,<lfs_poitem>-ackn_reqd.
      endif.
      lw_poitemx-conf_ctrl = 'X'.
      lw_poitemx-ackn_reqd = 'X'.

*** Stefanini - IR234900 - 30/06/2025 - FINC - Início de Alteração
      IF iw_poheader-doc_type IS NOT INITIAL.
        IF iw_poheader-doc_type = 'YEFI' OR iw_poheader-doc_type = 'YSEF' OR iw_poheader-doc_type = 'YCEF'.
          lw_poitemx-gr_basediv = 'X'.
          CLEAR <lfs_poitem>-gr_basediv.
        ELSE.
          lw_poitemx-gr_basediv = 'X'.
          <lfs_poitem>-gr_basediv = 'X'.
        ENDIF.
      ELSE.
        CLEAR lw_poitemx-gr_basediv.
      ENDIF.
*** Stefanini - IR234900 - 30/06/2025 - FINC - Fim de Alteração

      insert lw_poitemx into table lt_poitemx.
    endloop.

    loop at ct_pocond[] assigning field-symbol(<lfs_pocond>).
      clear lw_pocondx.
      set_fields_x(
        exporting
          iw_item  = <lfs_pocond>
        changing
          cw_itemx = lw_pocondx
      ).
      insert lw_pocondx into table lt_pocondx.
    endloop.

    loop at ct_account[] assigning field-symbol(<lfs_account>).
      clear lw_accountx.
      set_fields_x(
        exporting
          iw_item  = <lfs_account>
        changing
          cw_itemx = lw_accountx
      ).
      insert lw_accountx into table lt_accountx.
    endloop.

    loop at ct_schedule[] assigning field-symbol(<lfs_schedule>).
      clear lw_schedulex.
      set_fields_x(
        exporting
          iw_item  = <lfs_schedule>
        changing
          cw_itemx = lw_schedulex
      ).
      insert lw_schedulex into table lt_schedulex.
    endloop.
    read table ct_bapimepotextheader into data(w_bapimepotextheader) with key text_id = 'F01'.
    if sy-subrc = 0 and
      ( w_bapimepotextheader-text_line = 'ERRORATEIO' or w_bapimepotextheader-text_line = 'ERRO99' ).
      append initial line to rt_return reference into data(lo_return).
      lo_return->type     = 'E'.
      lo_return->id       = 'Z01'.
      lo_return->number   = '004'.
      if w_bapimepotextheader-text_line = 'ERRO99'.
        lo_return->message_v1 = 'Rateio unitário > 99 linhas'.
        lo_return->message    = 'Rateio unitário > 99 linhas'.
      else.
        lo_return->message_v1 = 'Rateio unitário 100%'.
        lo_return->message    = 'Rateio unitário não pode ser 100%'.
      endif.
    else.
      call function 'BAPI_PO_CREATE1' "#EC CI_USAGE_OK[2438131]
        exporting
          poheader          = iw_poheader
          poheaderx         = lw_poheaderx
          no_price_from_po  = iv_no_price_from_po
        importing
          exppurchaseorder  = ev_ebeln
        tables
          pocondheader      = ct_pocondheader
          pocondheaderx     = lt_pocondheaderx
          return            = rt_return
          poitem            = ct_poitem
          poitemx           = lt_poitemx
          pocond            = ct_pocond
          pocondx           = lt_pocondx
          popartner         = ct_popartner
          poaccount         = ct_account
          poaccountx        = lt_accountx
          poschedule        = ct_schedule
          poschedulex       = lt_schedulex
          potextheader      = ct_bapimepotextheader
          poservices        = ct_bapiesllc
          polimits          = ct_polimits
          posrvaccessvalues = ct_posrvaccessvalues.

      if iv_test is not initial.
        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else.
        read table rt_return transporting no fields with key type = 'E'.
        if sy-subrc is initial.
          call function 'BAPI_TRANSACTION_ROLLBACK'.
        else.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = abap_true.
        endif.
*-CS1114935-#RIMINI-07.10.2023-BEGIN
*- Excluir mensagem de warning "Erro durante a transferência dos dados
*  ExtensionIn para a ampliação CI_EKPODB"
        delete rt_return where type   eq 'W'
                           and id     eq 'ME'
                           and number eq '887'.
*-CS1114935-#RIMINI-07.10.2023-END
      endif.
    endif.

  endmethod.


  method BUSCA_DADOS.

    me->zif_integracao_coupa_ped_comp~set_ds_url( e_metodo   = 'GET' ).
    me->zif_integracao_coupa_ped_comp~set_send_msg(
                    IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                              e_integracao    = DATA(e_integracao_post) ).


  endmethod.


  METHOD CONSTRUCTOR.

    me->zif_integracao_coupa_ped_comp~set_servico( i_servico = i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_coupa_ped_comp.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    "ME->ZIF_INTEGRACAO_HVI_KUHLMANN~SET_DS_URL( ).

  ENDMETHOD.


  METHOD determina_criacao_modificacao.
    DATA: lt_xml_header_create TYPE zcl_integracao_coupa_ped_comp=>tt_xml_header.
    DATA: lt_xml_header_update TYPE zcl_integracao_coupa_ped_comp=>tt_xml_header.

    LOOP AT ct_xml_header REFERENCE INTO DATA(lo_wa_xml_header).

      IF ( lo_wa_xml_header->pedido_sap NE '@01@' ).
        APPEND lo_wa_xml_header->* TO lt_xml_header_create.
      ELSE.
        APPEND lo_wa_xml_header->* TO lt_xml_header_update.
      ENDIF.
    ENDLOOP.

    IF ( lt_xml_header_create IS NOT INITIAL ).
      me->realiza_lancamento_po(
        EXPORTING
          iv_test       = iv_test
        CHANGING
          ct_xml_header = lt_xml_header_create ).

      LOOP AT lt_xml_header_create REFERENCE INTO DATA(lo_wa_xml_header_local).
        READ TABLE ct_xml_header REFERENCE INTO lo_wa_xml_header WITH KEY id        = lo_wa_xml_header_local->id
                                                                          po_number = lo_wa_xml_header_local->po_number.
        IF ( sy-subrc IS INITIAL ).
          lo_wa_xml_header->* = lo_wa_xml_header_local->*.

          "Se gerou  serviço S0- força alterar
          SELECT SINGLE *
            INTO @DATA(_ekpo)
            FROM ekpo
            WHERE ebeln EQ @lo_wa_xml_header->po_number
            AND   mwskz = 'S0'.
          IF sy-subrc = 0.
            APPEND lo_wa_xml_header->* TO lt_xml_header_update. "Troca S0 por S1 se necessario
          ENDIF.
          "Se gerou valor inconsistente - força alterar
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF ( lt_xml_header_update IS NOT INITIAL ).
      me->realiza_modificacao_po(
        EXPORTING
          iv_test       = iv_test
        CHANGING
          ct_xml_header = lt_xml_header_update ).

      LOOP AT lt_xml_header_update REFERENCE INTO lo_wa_xml_header_local.
        READ TABLE ct_xml_header REFERENCE INTO lo_wa_xml_header WITH KEY id        = lo_wa_xml_header_local->id
                                                                          po_number = lo_wa_xml_header_local->po_number.
        IF ( sy-subrc IS INITIAL ).
          lo_wa_xml_header->* = lo_wa_xml_header_local->*.
        ENDIF.
      ENDLOOP.

    ENDIF.


  ENDMETHOD.


  METHOD determina_tipo_po.

    CASE iw_xml_header-tipo_pedido.
      WHEN 'IC' .
        rv_bsart = 'YCI'.
      WHEN 'CP|RE' OR 'CP|RP' .
        IF ( iw_xml_header-entrega_futura EQ abap_true ).
          rv_bsart = 'YSEF'.
        ELSEIF ( iw_xml_header-tipo_fornecedor EQ 'ZFEX' ).
          rv_bsart = 'YCSI'.
        ELSE.
          rv_bsart = 'YCS'.
        ENDIF.

      WHEN 'RD|NN' OR 'RD|RR' OR 'RD|RD' OR 'DA' OR 'SCAE'.
        IF ( iw_xml_header-entrega_futura EQ abap_true ).
          rv_bsart = 'YCEF'.
        ELSEIF ( iw_xml_header-tipo_fornecedor EQ 'ZFEX' ).
          rv_bsart = 'YCEI'.
        ELSE.
          rv_bsart = 'YCE'.
        ENDIF.

      WHEN 'DP|DPPR' OR 'DP|DPRR' OR 'DP|DPCP' OR 'DP|DP'  OR 'DP'.
        rv_bsart = 'YDBP'.
**********************************************************************"150179 CS2024000780 Inclusão de pedidos de Calcário no COUPA (ZFTE, ZEFI) PSA
      WHEN 'CP|FE' .
        rv_bsart = 'YFTE'.
      WHEN 'CP|EF' .
        rv_bsart = 'YEFI'.
**********************************************************************
    ENDCASE.

  ENDMETHOD.


  method GET_STRUCTDESCR.

    DATA: lo_structdescr TYPE REF TO cl_abap_structdescr.

    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( i_data ).
    rt_componentes = lo_structdescr->get_components( ).

  endmethod.


  method GET_XML_HEADER.

    rt_xml_header = t_xml_header.

  endmethod.


  method manipula_po_account.
    data:  lv_costcenter         type bapimepoaccount-costcenter.

    split iv_acctasscat at '-' into data(lv_cod_centro_custo)
                                    data(lv_cod_obj_contabil)
                                    data(lv_tipo)
                                    data(lv_centro_custo)
                                    data(lv_obj_contabil)
                                    data(lv_nd)
                                    data(lv_conta_razao)
                                    data(lv_conta_descricao).

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = lv_centro_custo
      importing
        output = lv_costcenter.

    if ( iw_rateio_account is initial ).
      read table ct_poaccount reference into data(lo_wa_poaccount) with key po_item = iv_ebelp.
    endif.

    case lv_tipo.
      when 'K'.
        cw_poitem-acctasscat = 'K'.
        if ( lo_wa_poaccount is not bound ).
          append initial line to ct_poaccount reference into lo_wa_poaccount.
        endif.
        lo_wa_poaccount->po_item = iv_ebelp.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv_conta_razao
          importing
            output = lo_wa_poaccount->gl_account.

        lo_wa_poaccount->costcenter   = lv_costcenter.

        if ( iw_rateio_account is not initial ).
          add 1 to cv_serial_no.
          lo_wa_poaccount->serial_no = cv_serial_no.
          lo_wa_poaccount->distr_perc = iw_rateio_account-pct.
          lo_wa_poaccount->quantity = iw_rateio_account-amount.
        endif.

      when 'FM' or 'FI'.
        cw_poitem-acctasscat = 'F'.
        if ( lo_wa_poaccount is not bound ).
          append initial line to ct_poaccount reference into lo_wa_poaccount.
        endif.
        lo_wa_poaccount->po_item = iv_ebelp.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv_conta_razao
          importing
            output = lo_wa_poaccount->gl_account.

        lo_wa_poaccount->costcenter   = lv_costcenter.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv_obj_contabil
          importing
            output = lo_wa_poaccount->orderid.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv_nd
          importing
            output = lo_wa_poaccount->activity.
        "
        if ( iw_rateio_account is not initial ).
          add 1 to cv_serial_no.
          lo_wa_poaccount->serial_no = cv_serial_no.
          lo_wa_poaccount->distr_perc = iw_rateio_account-pct.
          lo_wa_poaccount->quantity = iw_rateio_account-amount.
        endif.
      when 'A'.
        cw_poitem-acctasscat = 'A'.
        if ( lo_wa_poaccount is not bound ).
          append initial line to ct_poaccount reference into lo_wa_poaccount.
        endif.
        lo_wa_poaccount->po_item    = iv_ebelp.
        lo_wa_poaccount->quantity   = cw_poitem-quantity.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv_obj_contabil
          importing
            output = lo_wa_poaccount->asset_no.

        if ( lv_nd is not initial ).
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = lv_nd
            importing
              output = lo_wa_poaccount->sub_number.
        endif.

        lo_wa_poaccount->gr_rcpt = lv_centro_custo.

      when 'H'.
        cw_poitem-acctasscat = 'H'.
        if ( lo_wa_poaccount is not bound ).
          append initial line to ct_poaccount reference into lo_wa_poaccount.
        endif.
        lo_wa_poaccount->po_item    = iv_ebelp.
        lo_wa_poaccount->asset_no   = lv_cod_obj_contabil.
      when 'E'.
        cw_poitem-acctasscat = abap_false.
        cw_poitem-stge_loc   = lv_obj_contabil.
    endcase.


  endmethod.


  method realiza_lancamento_po.

    data: wa_poheader           type bapimepoheader,
          lt_pocondheader       type standard table of bapimepocondheader,
          lt_poitem             type standard table of bapimepoitem,
          lt_poitem_a           type standard table of bapimepoitem,
          lt_pocond             type standard table of bapimepocond,
          lt_poaccount          type standard table of bapimepoaccount,
          lt_poschedule         type standard table of bapimeposchedule,
          lt_bapimepotextheader type standard table of bapimepotextheader,
          lt_popartner          type standard table of bapiekkop,
          lt_poservices         type standard table of bapiesllc,
          lt_polimits           type standard table of bapiesuhc,
          lt_posrvaccessvalues  type standard table of bapiesklc,
          lv_ebeln              type ebeln,
          lv_poitem             type ebelp,
          lv_pckg_no            type bapiesuhc-pckg_no,
          lv_subpckg_no         type bapiesuhc-pckg_no,
          lv_line_no            type srv_line_no,
          lv_serial_no          type bapimepoaccount-serial_no,
          lv_costcenter         type bapimepoaccount-costcenter,
          lt_zmmt0158           type table of zmmt0158,
          wa_zmmt0158           type zmmt0158,
          lv_length             type i,
          v_price               type bapiesllc-quantity,
          v_price_neg           type bapicurext,
          v_price_des           type bapicurext,
          v_price_sup           type bapicurext,
          v_matnr               type zppet016-id_material_sap, ""mara-matnr,
          v_prices              type string,
          ld_htype              type dd01v-datatype,
          v_por_erro(1).

    data: ws_ekpo        type ekpo,
          tg_ekpo        type table of ekpo,
          vg_perc_reid   type char10,
          tg_item_pedido type table of ekpo.

    data: vg_taxa_char type char10,
          vg_taxa_num  type p decimals 4.

    data: lv_saknr      type ska1-saknr,
          iv_acctasscat type string.

    me->seleciona_servico_requisicao( ).
    refresh lt_zmmt0158.

    loop at ct_xml_header reference into data(wa_xml_header).

      clear:
       wa_poheader,
       lv_ebeln.

      free:
        lt_pocondheader,
        lt_poitem,
        lt_poitem_a,
        lt_pocond,
        lt_popartner,
        lt_bapimepotextheader,
        lt_poaccount,
        lt_poschedule,
        lt_poservices,
        lt_polimits  ,
        lt_posrvaccessvalues,
        tg_item_pedido.
      check wa_xml_header->bsart is not initial.

      clear wa_poheader.

*      me->determina_tipo_po( iw_xml_header = wa_xml_header->* ).
      wa_poheader-doc_type = wa_xml_header->bsart.

      wa_poheader-po_number  = wa_xml_header->po_number.
      wa_poheader-pmnttrms   = wa_xml_header->payment_term.
      wa_poheader-our_ref    = '58'.
      wa_poheader-pur_group  = 'C03'.
      wa_poheader-purch_org  = 'OC01'.
      wa_poheader-purch_org  = 'OC01'.
      wa_poheader-created_by = sy-uname.
*      wa_poheader-doc_date   = wa_xml_header->created_a(4) && wa_xml_header->created_a+5(2) && wa_xml_header->created_a+8(2).
      wa_poheader-quot_date   = wa_xml_header->created_a(4) && wa_xml_header->created_a+5(2) && wa_xml_header->created_a+8(2). "108615 CS2023000262 Ret Check data Em Pedi vs. No - PSA
      wa_poheader-doc_date   = sy-datum.
      wa_poheader-langu      = sy-langu.
      wa_poheader-currency   = wa_xml_header->currency.
      wa_poheader-comp_code  = wa_xml_header->empresa.
      wa_poheader-quotation   = wa_xml_header->contrato. "108615 CS2023000262 Ret Check data Em Pedi vs. No - PSA

      clear v_por_erro.
      loop at t_xml_lines into data(wa_xml_lines2) where id = wa_xml_header->id
                                                    and po_number = wa_xml_header->po_number.
        if wa_xml_lines2-price_unit = 0 and wa_xml_lines2-status ne 'cancelled'.
          v_por_erro = 'X'.
          exit.
        endif.
        clear lv_serial_no.
                                                            "US 157652
        if ( wa_xml_lines2-account_allocations is not initial ).
          loop at wa_xml_lines2-account_allocations reference into data(lo_wa_account_allocation2).
            iv_acctasscat     = lo_wa_account_allocation2->code.
            split iv_acctasscat at '-' into data(lv_cod_centro_custo2)
                                            data(lv_cod_obj_contabil2)
                                            data(lv_tipo2)
                                            data(lv_centro_custo2)
                                            data(lv_obj_contabil2)
                                            data(lv_nd2)
                                            data(lv_conta_razao2)
                                            data(lv_conta_descricao2).
            case lv_tipo2.
              when 'K' or 'FM' or 'FI'.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = lv_conta_razao2
                  importing
                    output = lv_saknr.

                select count(*)
                  from ska1
                  where saknr = lv_saknr.

                if sy-subrc ne 0.
                  v_por_erro = 'X'.
                  exit.
                endif.

                if strlen( lv_nd2 ) gt 4.
                  v_por_erro = 'X'.
                  exit.
                endif.

            endcase.
          endloop.
          if v_por_erro = 'X'.
            exit.
          endif.
        else.
          iv_acctasscat = wa_xml_lines2-acctasscat.
          split iv_acctasscat at '-' into data(lv_cod_centro_custo)
                                      data(lv_cod_obj_contabil)
                                      data(lv_tipo)
                                      data(lv_centro_custo)
                                      data(lv_obj_contabil)
                                      data(lv_nd)
                                      data(lv_conta_razao)
                                      data(lv_conta_descricao).
          case lv_tipo.
            when 'K' or 'FM' or 'FI'.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = lv_conta_razao
                importing
                  output = lv_saknr.

              select count(*)
                from ska1
                where saknr = lv_saknr.

              if sy-subrc ne 0.
                v_por_erro = 'X'.
                exit.
              endif.

              if strlen( lv_nd ) gt 4.
                v_por_erro = 'X'.
                exit.
              endif.

          endcase.
        endif.
                                                            "US 157652
        "
      endloop.
      "
      if v_por_erro = 'X'.
        me->atualiza_exported_pedido_coupa( iw_xml_header = wa_xml_header->* ).
        continue.
      endif.


      call function 'NUMERIC_CHECK'
        exporting
          string_in  = wa_xml_header->taxa_d_cmbio
        importing
          string_out = wa_xml_header->taxa_d_cmbio
          htype      = ld_htype.





      if ld_htype = 'CHAR'.
        replace ',' in wa_xml_header->taxa_d_cmbio with '.'.

        if sy-subrc eq 0.
          condense wa_xml_header->taxa_d_cmbio no-gaps.
          if wa_xml_header->taxa_d_cmbio > 0.
            try.
                wa_poheader-exch_rate  = wa_xml_header->taxa_d_cmbio.
              catch cx_sy_conversion_overflow.
                me->atualiza_exported_pedido_coupa( iw_xml_header = wa_xml_header->* ).
                continue.
            endtry.
          else.
            wa_poheader-exch_rate  = 1.
          endif.
        else.
          wa_poheader-exch_rate  = 1.
        endif.
      else.
        wa_poheader-exch_rate  = 1.
      endif.



      if wa_xml_header->taxa_fixa = 'false'.
        wa_poheader-ex_rate_fx = ' '.
      else.
        wa_poheader-ex_rate_fx = 'X'.
      endif.

      wa_poheader-incoterms1 = wa_xml_header->incoterms1.
      wa_poheader-incoterms2 = wa_xml_header->incoterms1.
      wa_poheader-ref_1      = wa_xml_header->safra. "US184512

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_xml_header->vendor
        importing
          output = wa_poheader-vendor.

      if ( wa_xml_header->codio_pagamento is not initial ).
        append initial line to lt_bapimepotextheader reference into data(lo_headertext).
        lo_headertext->po_number = wa_xml_header->po_number.
        lo_headertext->text_id   = 'F07'.
        lo_headertext->text_form = '*'.
        lo_headertext->text_line = wa_xml_header->codio_pagamento.
      endif.

      lv_length = strlen( wa_xml_header->justificativa ). "RJF

      if lv_length gt 0 and lv_length le 132.

        if ( wa_xml_header->justificativa is not initial ).
          append initial line to lt_bapimepotextheader reference into lo_headertext.
          lo_headertext->po_number = wa_xml_header->po_number.
          lo_headertext->text_id   = 'F01'.
          lo_headertext->text_form = '*'.
          lo_headertext->text_line = wa_xml_header->justificativa.
        endif.

      elseif lv_length gt 132. " RJF
        data: lv_ini   type i,
              lv_cont  type i,
              lv_qtd   type p decimals 2,
              lv_qtdl  type i,
              lv_fim   type i,
              lv_contx type i.
        lv_ini = 0.
        lv_cont = 132.
        lv_qtd   = lv_length / 132.
        lv_qtdl  = ceil( lv_qtd ).

        if lv_qtdl lt 3.
          lv_fim = lv_length - 132.
        else.
          lv_fim = ( ( lv_length ) - ( 132 * ( lv_qtdl - 1 ) ) ).
        endif.

        free: lv_contx.

        do lv_qtdl times.
          lv_contx = lv_contx + 1.

          if lv_contx gt 1.
            lv_ini = lv_ini + 132.
          endif.

          append initial line to lt_bapimepotextheader reference into lo_headertext.
          lo_headertext->po_number = wa_xml_header->po_number.
          lo_headertext->text_id   = 'F01'.
          if lv_contx eq 1.
            lo_headertext->text_form = '*'.
            lo_headertext->text_line = wa_xml_header->justificativa+lv_ini(lv_cont).
          elseif lv_contx eq lv_qtdl.
            lo_headertext->text_form = '='.
            lo_headertext->text_line = wa_xml_header->justificativa+lv_ini(lv_fim).
          else.
            lo_headertext->text_form = '='.
            lo_headertext->text_line = wa_xml_header->justificativa+lv_ini(lv_cont).
          endif.
        enddo.
      endif.

      if ( wa_xml_header->partner is not initial ).
        wa_poheader-diff_inv   = wa_xml_header->partner.
        wa_poheader-suppl_vend = wa_xml_header->partner.
      endif.

      clear: v_price_sup, v_price_des.

      loop at t_xml_lines into data(wa_xml_lines) where id = wa_xml_header->id
                                                    and po_number = wa_xml_header->po_number.
        v_price_neg = wa_xml_lines-price * wa_xml_lines-quantity.
        if v_price_neg  < 0.
          multiply v_price_neg by -1.
          add v_price_neg to v_price_des.
        endif.

        split wa_xml_lines-description  at '-' into data(lv2_matnr)
                                                    data(lv2_matnr_des)
                                                    data(lv2_service).
        if ( wa_xml_lines-service_type eq abap_true ).
          "
          split wa_xml_lines-description  at ' ' into lv2_matnr
                                                      lv2_matnr_des.
        endif.

        if ( lv2_matnr ca sy-abcde ).
          split lv2_matnr  at ' ' into lv2_matnr
                                       lv2_matnr_des.
        endif.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv2_matnr
          importing
            output = v_matnr.
        if v_matnr = '000000000000999992'.
          add v_price_neg to   v_price_sup.
        endif.

      endloop.

      "Desconto
      if ( wa_xml_header->desconto is not initial ) or v_price_des > 0.
        append initial line to lt_pocondheader reference into data(lo_pocondheader).
        lo_pocondheader->cond_type  = 'HB01'.
        if v_price_des > 0.
          lo_pocondheader->cond_value = v_price_des.
        else.
          lo_pocondheader->cond_value = wa_xml_header->desconto.
        endif.
        multiply lo_pocondheader->cond_value by -1.
        lo_pocondheader->currency   = wa_xml_header->currency.
        lo_pocondheader->change_id  = 'I'.
      endif.

      "Frete
      if ( wa_xml_header->frete is not initial ) or v_price_sup > 0.
        append initial line to lt_pocondheader reference into lo_pocondheader.
*        lo_pocondheader->itm_number =  0.
        lo_pocondheader->cond_type  = 'HB00'.
        if v_price_sup > 0.
          lo_pocondheader->cond_value = v_price_sup.
        else.
          lo_pocondheader->cond_value = wa_xml_header->frete.
        endif.
*        lo_pocondheader->cond_unit  = 'UN'.
        lo_pocondheader->currency   = wa_xml_header->currency.
        lo_pocondheader->change_id  = 'I'.
      endif.


      free: lv_poitem, lv_pckg_no, lv_serial_no, lv_line_no.

      loop at t_xml_lines into wa_xml_lines where id = wa_xml_header->id
                                                  and po_number = wa_xml_header->po_number.

        if wa_xml_lines-status = 'cancelled'.
          continue.
        endif.
        v_price_neg = wa_xml_lines-price * wa_xml_lines-quantity.
        if v_price_neg < 0. "Ignora linhas negativas
          continue.
        endif.
        split wa_xml_lines-description  at '-' into lv2_matnr
                                                    lv2_matnr_des
                                                    lv2_service.
        if ( wa_xml_lines-service_type eq abap_true ).

          split wa_xml_lines-description  at ' ' into lv2_matnr
                                                      lv2_matnr_des.
        endif.

        if ( lv2_matnr ca sy-abcde ).
          split lv2_matnr  at ' ' into lv2_matnr
                                       lv2_matnr_des.
        endif.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv2_matnr
          importing
            output = v_matnr.
        if v_matnr = '000000000000999992'. "Ignora esse material usado como acrescimo /suplemento de preço
          continue.
        endif.

        lv_poitem = wa_xml_lines-line_num * 10.

        append initial line to lt_poitem    reference into data(lo_wa_poitem).

        lo_wa_poitem->po_item    = lv_poitem.
        lo_wa_poitem->preq_name  =
        lo_wa_poitem->plant      = wa_xml_lines-plant.
        lo_wa_poitem->quantity   = wa_xml_lines-quantity.
* "US184512
          lo_wa_poitem->incoterms1 = wa_xml_header->incoterms1.
          lo_wa_poitem->incoterms2 = wa_xml_header->incoterms1.
*"US184512

        lo_wa_poitem->preq_name  = wa_xml_header->login. " wa_xml_lines-requested_by. "Requesitante.

        if ( wa_xml_lines-trackingno is not initial ).
*          DATA(lv_strlen) = strlen( wa_xml_lines-trackingno ).
*          IF ( lv_strlen GT 8 ).
*            lv_strlen = 8.
*          ENDIF.
*          lo_wa_poitem->trackingno = wa_xml_lines-trackingno(lv_strlen).
          lo_wa_poitem->trackingno = wa_xml_lines-trackingno.
        endif.
        "
        "GRAVA SEMPRE S0 para serviço, depois altera
        if wa_xml_lines-tax_code = 'S1'.
          lo_wa_poitem->tax_code   = 'S0'.
        else.
          lo_wa_poitem->tax_code   = wa_xml_lines-tax_code.
        endif.

        lo_wa_poitem->price_unit = wa_xml_lines-price_unit.
        if ( lo_wa_poitem->price_unit is initial ).
          lo_wa_poitem->price_unit = wa_xml_lines-price.
        else.
          lo_wa_poitem->net_price = wa_xml_lines-price * wa_xml_lines-price_unit.
        endif.

        if wa_xml_lines-status = 'soft_closed_for_invoicing' or
          wa_xml_lines-status = 'closed'.  " RJF - 2023.05.15 IR128497 - pedidos COUPA status 'closed/fechado'
          lo_wa_poitem->no_more_gr = 'X'.
          lo_wa_poitem->final_inv = 'X'.
        endif.
        if wa_xml_lines-status = 'soft_closed_for_receiving'.
          lo_wa_poitem->no_more_gr = 'X'.
        endif.

        lo_wa_poitem->batch = wa_xml_lines-charg.
                                                            "US150179
        if wa_poheader-our_ref    = '58' and ( wa_xml_header->bsart eq 'YFTE' or wa_xml_header->bsart eq 'YEFI' ).
          wa_poheader-our_ref = wa_xml_lines-charg+5(4).
        endif.
                                                            "US150179

        if ( wa_xml_lines-banfn is not initial ).
          lo_wa_poitem->preq_no = wa_xml_lines-banfn.
          lo_wa_poitem->preq_item = wa_xml_lines-bnfpo.
        endif.

        clear lv_serial_no.
        if ( wa_xml_lines-account_allocations is not initial ).
          if wa_xml_header->justificativa ne 'ERRORATEIO'.
            loop at wa_xml_lines-account_allocations reference into data(lo_wa_account_allocation).
              at first.
                lo_wa_poitem->distrib = '2'.
                clear lv_serial_no.
              endat.

              me->manipula_po_account(
                exporting
                  iv_ebelp          = lv_poitem
                  iv_acctasscat     = lo_wa_account_allocation->code
                  iw_rateio_account = lo_wa_account_allocation->*
                changing
                  ct_poaccount      = lt_poaccount    " Tipo de tabela para BAPIMEPOACCOUNT
                  cw_poitem         = lo_wa_poitem->*    " Item do pedido
                  cv_serial_no      = lv_serial_no
              ).

            endloop.
          endif.
        else.
          me->manipula_po_account(
            exporting
              iv_ebelp      = lv_poitem
              iv_acctasscat = wa_xml_lines-acctasscat
*             iw_rateio_account = lo_wa_account_allocation->*
            changing
              ct_poaccount  = lt_poaccount    " Tipo de tabela para BAPIMEPOACCOUNT
              cw_poitem     = lo_wa_poitem->*    " Item do pedido
          ).
        endif.


        split wa_xml_lines-description  at '-' into data(lv_matnr)
                                                    data(lv_matnr_des)
                                                    data(lv_service).
        if ( wa_xml_lines-service_type eq abap_true ).
          replace ' ' in lv_matnr_des with ''.
          split lv_matnr_des  at ' ' into lv_service
                                          lv_matnr_des.
          if ( lv_service is initial ).
            split lv_matnr_des  at ' ' into lv_service
                                            lv_matnr_des.
          endif.
          split wa_xml_lines-description  at ' ' into lv_matnr
                                                      lv_matnr_des.
        endif.

        if ( lv_matnr ca sy-abcde ).
          split lv_matnr  at ' ' into lv_matnr
                                      lv_matnr_des.
        endif.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = lv_matnr
          importing
            output = lo_wa_poitem->material.

        if ( wa_xml_lines-service_type is not initial ) and wa_xml_header->bsart ne 'YDBP'.
*          FREE: lo_wa_poitem->material.
          lo_wa_poitem->short_text = lv_matnr_des.
          lo_wa_poitem->matl_group = '153310'.
          if ( lo_wa_poitem->tax_code is initial ).
*            lo_wa_poitem->tax_code = 'S1'.
            lo_wa_poitem->tax_code = 'S0'.
          endif.
          add 10 to lv_pckg_no.
          lo_wa_poitem->item_cat = 'D'.
          lo_wa_poitem->pckg_no     = lv_pckg_no.

                                                            "US164105
          lo_wa_poitem->srv_based_iv = 'X'.
                                                            "US164105

          if ( wa_xml_lines-account_allocations is initial ).
            read table lt_poaccount reference into data(lo_wa_poaccount) with key po_item = lv_poitem.
            if ( lo_wa_poaccount is bound ).
              if lo_wa_poaccount->serial_no = '00'.
                lo_wa_poaccount->quantity = 1.
              endif.
            endif.
          endif.

          add 1 to lv_line_no.
          append initial line to lt_poservices reference into data(lo_wa_poservices).
          lo_wa_poservices->pckg_no  = lv_pckg_no.
          lo_wa_poservices->line_no  = lv_line_no.
          lo_wa_poservices->pln_line = lv_line_no.

          "incrementa novamente
          call function 'NUMBER_GET_NEXT'
            exporting
              nr_range_nr             = '01'
              object                  = 'SERVICE'
            importing
              number                  = lo_wa_poservices->subpckg_no
            exceptions
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              others                  = 8.

          lv_subpckg_no = lo_wa_poservices->subpckg_no.
          append initial line to lt_poservices reference into lo_wa_poservices.
          add 1 to lv_line_no.
          lo_wa_poservices->pckg_no = lv_subpckg_no.
          lo_wa_poservices->line_no = lv_line_no.

          condense lv_service no-gaps.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = lv_service
            importing
              output = lo_wa_poservices->service.
          lo_wa_poservices->quantity    = lo_wa_poitem->net_price * wa_xml_lines-quantity.
          lo_wa_poservices->price_unit  = wa_xml_lines-price_unit.
          lo_wa_poservices->gr_price    = 1.
          "
          "Neste caso o standar força qtde = 1
          lo_wa_poitem->price_unit      = 1.
          lo_wa_poitem->quantity        = 1.
*          lo_wa_poitem->net_price       = lo_wa_poitem->net_price * wa_xml_lines-quantity.
          lo_wa_poitem->net_price       = wa_xml_lines-price * wa_xml_lines-quantity.
          "
          "Valores originais
          wa_zmmt0158-ebeln  = wa_poheader-po_number.
          wa_zmmt0158-ebelp  = lo_wa_poitem->po_item.
          wa_zmmt0158-srvpos = lo_wa_poservices->service.
          wa_zmmt0158-menge  = wa_xml_lines-quantity.
          wa_zmmt0158-netwr  = lo_wa_poitem->net_price.
          append wa_zmmt0158 to lt_zmmt0158.
          "Valores originais

          append initial line to lt_polimits reference into data(lo_wa_polimits).
          lo_wa_polimits->pckg_no   = lv_subpckg_no.
          lo_wa_polimits->no_limit  = abap_true.
          lo_wa_polimits->exp_value = lo_wa_poitem->net_price.

          if ( lo_wa_poaccount is bound ).
            if ( lo_wa_poaccount->serial_no is initial ).
              add 1 to lv_serial_no.
              lo_wa_poaccount->serial_no = lv_serial_no.
            else.
              lv_serial_no = lo_wa_poaccount->serial_no.
            endif.
          endif.

          if ( wa_xml_lines-account_allocations is not initial ).
            loop at wa_xml_lines-account_allocations reference into lo_wa_account_allocation.
              at first.
                clear lv_serial_no.
              endat.
              add 1 to lv_serial_no.
              append initial line to lt_posrvaccessvalues reference into data(lo_wa_valuesr).
              lo_wa_valuesr->pckg_no    = lv_subpckg_no.
              lo_wa_valuesr->line_no    = lv_line_no.
              lo_wa_valuesr->serno_line = lv_serial_no.
              lo_wa_valuesr->percentage = lo_wa_account_allocation->pct.
              lo_wa_valuesr->serial_no  = lv_serial_no.
              lo_wa_valuesr->quantity   = lo_wa_account_allocation->amount.
              lo_wa_valuesr->net_value  = 1.


            endloop.
          else.
            append initial line to lt_posrvaccessvalues reference into data(lo_wa_values).
            lo_wa_values->pckg_no    = lv_subpckg_no.
            lo_wa_values->line_no    = lv_line_no.
            lo_wa_values->serno_line = '01'.
            lo_wa_values->percentage = 100.
            lo_wa_values->serial_no  = lv_serial_no.
*            lo_wa_values->quantity   = lo_wa_poitem->net_price * wa_xml_lines-quantity.
            lo_wa_values->quantity   = wa_xml_lines-price * wa_xml_lines-quantity.
            lo_wa_values->net_value  = 1.
          endif.

*          lo_wa_values->quantity   = wa_xml_lines-quantity.
*          lo_wa_values->net_value  = lo_wa_poitem->net_price.

        elseif wa_xml_header->bsart ne 'YDBP'.
          if ( wa_xml_lines-banfn is not initial ).
            v_price   = wa_xml_lines-price * wa_xml_lines-quantity.
            v_prices =  v_price.
            data(lv_servico) = me->verifica_servico_requisicao( exporting iv_banfn             = wa_xml_lines-banfn
                                                                          iv_bnfpo             = wa_xml_lines-bnfpo
                                                                          iv_price             = v_prices
                                                                          iv_unit              = wa_xml_lines-price_unit
                                                                changing  et_poservices        = lt_poservices
                                                                          et_polimits          = lt_polimits
                                                                          et_posrvaccessvalues = lt_posrvaccessvalues
                                                                          cv_pckgno            = lv_pckg_no
                                                                          cv_serialno          = lv_serial_no
                                                                          cv_lineno            = lv_line_no ).
            if ( lv_servico is not initial ).
              lo_wa_poitem->item_cat = 'D'.
              lo_wa_poitem->pckg_no  = lv_pckg_no.

              if ( lo_wa_poaccount is bound ).
                lo_wa_poaccount->serial_no = lv_serial_no.
              endif.

            endif.
          endif.
        elseif wa_xml_header->bsart = 'YDBP'.
          lo_wa_poitem->gr_non_val = 'X'. "Entrada de mercadoria não avaliada
        endif.

        append initial line to lt_poschedule reference into data(lo_wa_poschedule).
        lo_wa_poschedule->po_item       = lv_poitem.
        if wa_xml_lines-service_type = 'X' and  wa_xml_header->bsart ne 'YDBP'.
          lo_wa_poschedule->quantity      = 1. "erro, altera a quantidade do item
        else.
          lo_wa_poschedule->quantity      = wa_xml_lines-quantity.
        endif.
        lo_wa_poschedule->sched_line    = 1.

        split wa_xml_lines-need_by_date at 'T' into data(lv_data_text)
                                                    data(lv_data_rest).
        if lv_data_text is not initial.

          lo_wa_poschedule->delivery_date = |{ lv_data_text(4)   }| &&
                                            |{ lv_data_text+5(2) }| &&
                                            |{ lv_data_text+8(2) }| .

        endif.
        if wa_xml_lines-percentual_reid is not initial.
          clear: vg_perc_reid.
          vg_perc_reid = wa_xml_lines-percentual_reid.
          condense vg_perc_reid no-gaps.
          translate vg_perc_reid using ',.'.
          append value #( aliquota = vg_perc_reid ebelp = lo_wa_poitem->po_item ebeln = wa_xml_header->po_number reidi = vg_perc_reid ) to tg_item_pedido.
        endif.

      endloop.

      "alterado em 29.09.2025
      me->atualiza_exported_pedido_coupa( iw_xml_header = wa_xml_header->* ).
      "alterado em 29.09.2025
      try .
*==========================================================Inicio ajuste DUMP IR177296 / AOENNING.
          data(lt_return) = me->bapi_po_create(
            exporting
              iw_poheader           = wa_poheader              " Pedido dds.cabeçalho
              iv_test               = iv_test
              iv_no_price_from_po   = abap_true
            importing
              ev_ebeln              = lv_ebeln
            changing
              ct_pocondheader       = lt_pocondheader
              ct_poitem             = lt_poitem                " Tipo de tabela para BAPIMEPOITEM
              ct_pocond             = lt_pocond                " Tipo de tabela para BAPIMEPOCOND
              ct_popartner          = lt_popartner             " Tipo de tabela para BAPIEKKOP
              ct_bapimepotextheader = lt_bapimepotextheader    " Tipo de tabela para BAPIMEPOTEXTHEADER
              ct_account            = lt_poaccount
              ct_schedule           = lt_poschedule
              ct_bapiesllc          = lt_poservices
              ct_polimits           = lt_polimits
              ct_posrvaccessvalues  = lt_posrvaccessvalues
          ).
        catch cx_root.
      endtry.
*      me->bapi_po_create(
*        EXPORTING
*          iw_poheader           = wa_poheader              " Pedido dds.cabeçalho
*          iv_test               = iv_test
*          iv_no_price_from_po   = abap_true
*        IMPORTING
*          ev_ebeln              = lv_ebeln
*        CHANGING
*          ct_pocondheader       = lt_pocondheader
*          ct_poitem             = lt_poitem                " Tipo de tabela para BAPIMEPOITEM
*          ct_pocond             = lt_pocond                " Tipo de tabela para BAPIMEPOCOND
*          ct_popartner          = lt_popartner             " Tipo de tabela para BAPIEKKOP
*          ct_bapimepotextheader = lt_bapimepotextheader    " Tipo de tabela para BAPIMEPOTEXTHEADER
*          ct_account            = lt_poaccount
*          ct_schedule           = lt_poschedule
*          ct_bapiesllc          = lt_poservices
*          ct_polimits           = lt_polimits
*          ct_posrvaccessvalues  = lt_posrvaccessvalues
*        RECEIVING
*          rt_return             = DATA(lt_return)           " Tabela de retorno
*        EXCEPTIONS
*          failure               = 1                " Error
*          OTHERS                = 2
*      ).
*      IF sy-subrc <> 0.
**         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*==========================================================Fim ajuste DUMP IR177296 / AOENNING.


      if lv_ebeln is not initial.
        wa_xml_header->exported = 'True'.
        if lt_zmmt0158[] is not initial.
          modify zmmt0158 from table lt_zmmt0158.
          commit work.
        endif.
      endif.

      wa_xml_header->icon_result = '@DH@'.
      wa_xml_header->tt_return = lt_return.

      if ( line_exists( wa_xml_header->tt_return[ type = 'E' ] ) ).
        wa_xml_header->icon = '@5C@'.
      else.
*||==========================================================CS2023000312 / AOENNING.
        select * from ekpo into table tg_ekpo where ebeln eq lv_ebeln.
        if sy-subrc eq 0.
          loop at tg_ekpo assigning field-symbol(<ls_ekpo>).
            read table tg_item_pedido into data(ws_item) with key ebeln = <ls_ekpo>-ebeln ebelp = <ls_ekpo>-ebelp.
            if sy-subrc eq 0 and ws_item-aliquota is not initial.
              <ls_ekpo>-aliquota = ws_item-aliquota.
              <ls_ekpo>-reidi    = 'S'.
            else.
              <ls_ekpo>-reidi    = ''.
            endif.
          endloop.
          modify ekpo from table tg_ekpo.
          commit work.
          clear: ws_item.
        endif.
*||==========================================================CS2023000312 / AOENNING.

        if ( iv_test is not initial ).
          wa_xml_header->icon = '@01@'.
        else.
          wa_xml_header->icon = '@DF@'.
          wa_xml_header->pedido_sap = '@01@'.
*-CS1117561-#RIMINI-14.07.2023-BEGIN
*          me->atualiza_pedido_coupa( iv_id = wa_xml_header->id ).
          me->atualiza_pedido_coupa(
            iv_id   = wa_xml_header->id
            iv_tipo = 'INC' ).
*-CS1117561-#RIMINI-14.07.2023-END
          "
          "Faz o recalculo do imposto
          do 5 times.
            refresh lt_poitem_a.
            loop at lt_poitem into data(lo_wa_poitem_a).
              data(_item) = lo_wa_poitem_a-po_item.
              clear lo_wa_poitem_a.
              lo_wa_poitem_a-po_item = _item.
              lo_wa_poitem_a-calctype = 'G'. "Aceitar comp.preço sem modif., calcular novamente imposto
              append lo_wa_poitem_a to lt_poitem_a.
            endloop.
            lt_return = zcl_integracao_coupa_ped_comp=>bapi_po_change(
              exporting
                iv_test     = iv_test
                iw_poheader = value bapimepoheader( po_number = lv_ebeln )
              changing
                ct_poitem   = lt_poitem_a ).
          enddo.

        endif.
      endif.

      if ( iv_test is initial ).
        me->atualiza_mensagem_pedido_coupa( iw_xml_header = wa_xml_header->* ).
        me->atualiza_exported_pedido_coupa( iw_xml_header = wa_xml_header->* ).
      endif.

      clear:
        wa_poheader,
        lv_ebeln.

      free:
        lt_pocondheader,
        lt_poitem,
        lt_poitem_a,
        lt_pocond,
        lt_popartner,
        lt_bapimepotextheader,
        lt_poaccount,
        lt_poschedule,
        lt_poservices,
        lt_polimits  ,
        lt_posrvaccessvalues.
    endloop.

  endmethod.


  method realiza_modificacao_po.

    data: wa_poheader           type bapimepoheader,
          lt_pocondheader       type standard table of bapimepocondheader,
          lt_poitem             type standard table of bapimepoitem,
          lt_poitem_a           type standard table of bapimepoitem,
          lt_poitem_b           type standard table of bapimepoitem,
          lt_pocond             type standard table of bapimepocond,
          lt_poaccount          type standard table of bapimepoaccount,
          lt_poschedule         type standard table of bapimeposchedule,
          lt_bapimepotextheader type standard table of bapimepotextheader,
          lt_popartner          type standard table of bapiekkop,
          lt_poservices         type standard table of bapiesllc,
          lt_polimits           type standard table of bapiesuhc,
          lt_posrvaccessvalues  type standard table of bapiesklc,
          lt_extension          type table of bapiparex,
          "

          lv_length             type i,   "*-Equalização RISE x PRD - 19.07.2023 - JT
          lv_ebeln              type ebeln,
          lv_poitem             type ebelp,
          lv_pckg_no            type bapiesuhc-pckg_no,
          lv_subpckg_no         type bapiesuhc-pckg_no,
          lv_subpckg_no2        type bapiesuhc-pckg_no,
          lv_line_no            type srv_line_no,
          lv_serial_no          type bapimepoaccount-serial_no,
          lv_costcenter         type bapimepoaccount-costcenter,
          lv_delete             type flag,
          lt_zmmt0158           type table of zmmt0158,
          wa_zmmt0158           type zmmt0158,
          v_netpr               type ekpo-netpr value '0.10',
          v_price               type bapiesllc-quantity,
          v_prices              type string,
          v_price_neg           type bapicurext,
          v_price_des           type bapicurext,
          v_price_sup           type bapicurext,
          v_matnr               type zppet016-id_material_sap, "mara-matnr,
          ld_htype              type dd01v-datatype,
          vg_ebelp              type ebelp,
          v_por_erro(1).

    data: ws_ekpo        type ekpo,
          tg_ekpo        type table of ekpo,
          vg_perc_reid   type char10,
          tg_item_pedido type table of ekpo.

    data: lv_saknr      type ska1-saknr,
          iv_acctasscat type string.

    refresh lt_zmmt0158.
    me->seleciona_servico_requisicao( ).
    loop at ct_xml_header reference into data(wa_xml_header).
      data(_fimloop)  = 'N'.
      data(_loop)  = 0.
      data(_altera) = 'N'.
      refresh: lt_poitem_b.
      while _fimloop = 'N' and _loop lt 3. "loop 1-erro preço liquido 2 - trocs iva 3-iva original
        add 1 to _loop.
        clear: wa_poheader, lv_delete.
        clear:
        wa_poheader,
        lv_ebeln.

        free:
          lt_pocondheader,
          lt_poitem,
          lt_pocond,
          lt_popartner,
          lt_bapimepotextheader,
          lt_poaccount,
          lt_poschedule,
          lt_poservices,
          lt_polimits  ,
          lt_posrvaccessvalues,
          tg_item_pedido.
        call function 'BAPI_PO_GETDETAIL1' "#EC CI_USAGE_OK[2438131]
          exporting
            purchaseorder      = wa_xml_header->po_number
            account_assignment = abap_true
            services           = abap_true
            header_text        = abap_true
          importing
            poheader           = wa_poheader
          tables
            poitem             = lt_poitem
            poschedule         = lt_poschedule
            poaccount          = lt_poaccount
            pocond             = lt_pocond
            polimits           = lt_polimits
*           POCONTRACTLIMITS   =
            poservices         = lt_poservices
            posrvaccessvalues  = lt_posrvaccessvalues
            potextheader       = lt_bapimepotextheader
            popartner          = lt_popartner.
*-CS1119615-#RIMINI-20.07.2023-BEGIN
*- Prevalecer apenas texto do COUPA
        clear lt_bapimepotextheader[].
*-CS1119615-#RIMINI-20.07.2023-END
        if ( wa_poheader-pmnttrms ne wa_xml_header->payment_term ).

          wa_poheader-pmnttrms   = wa_xml_header->payment_term.
          free: wa_poheader-dscnt1_to, wa_poheader-dscnt2_to, wa_poheader-dscnt3_to.
        endif.
*        wa_poheader-doc_type = wa_xml_header->bsart.
        wa_poheader-our_ref    = '58'.
        wa_poheader-pur_group  = 'C03'.
        wa_poheader-purch_org  = 'OC01'.
*      wa_poheader-created_by = sy-uname.
*      wa_poheader-doc_date   = wa_xml_header->created_a(4) && wa_xml_header->created_a+5(2) && wa_xml_header->created_a+8(2).
        wa_poheader-quot_date   = wa_xml_header->created_a(4) && wa_xml_header->created_a+5(2) && wa_xml_header->created_a+8(2). "108615 CS2023000262 Ret Check data Em Pedi vs. No - PSA
        wa_poheader-langu      = sy-langu.
        wa_poheader-currency   = wa_xml_header->currency.
        wa_poheader-comp_code  = wa_xml_header->empresa.
        wa_poheader-quotation   = wa_xml_header->contrato. "108615 CS2023000262 Ret Check data Em Pedi vs. No - PSA
        clear v_por_erro.
        loop at t_xml_lines into data(wa_xml_lines2) where id = wa_xml_header->id
                                                      and po_number = wa_xml_header->po_number.
          if wa_xml_lines2-price_unit = 0 and wa_xml_lines2-status ne 'cancelled'.
            v_por_erro = 'X'.
            exit.
          endif.

                                                            "US157652
          if ( wa_xml_lines2-account_allocations is not initial ).
            loop at wa_xml_lines2-account_allocations reference into data(lo_wa_account_allocation2).
              iv_acctasscat     = lo_wa_account_allocation2->code.
              split iv_acctasscat at '-' into data(lv_cod_centro_custo2)
                                              data(lv_cod_obj_contabil2)
                                              data(lv_tipo2)
                                              data(lv_centro_custo2)
                                              data(lv_obj_contabil2)
                                              data(lv_nd2)
                                              data(lv_conta_razao2)
                                              data(lv_conta_descricao2).
              case lv_tipo2.
                when 'K' or 'FM' or 'FI'.
                  call function 'CONVERSION_EXIT_ALPHA_INPUT'
                    exporting
                      input  = lv_conta_razao2
                    importing
                      output = lv_saknr.

                  select count(*)
                    from ska1
                    where saknr = lv_saknr.

                  if sy-subrc ne 0.
                    v_por_erro = 'X'.
                    exit.
                  endif.

                  if strlen( lv_nd2 ) gt 4.
                    v_por_erro = 'X'.
                    exit.
                  endif.

              endcase.
            endloop.
            if v_por_erro = 'X'.
              exit.
            endif.
          else.
            iv_acctasscat = wa_xml_lines2-acctasscat.
            split iv_acctasscat at '-' into data(lv_cod_centro_custo)
                                    data(lv_cod_obj_contabil)
                                    data(lv_tipo)
                                    data(lv_centro_custo)
                                    data(lv_obj_contabil)
                                    data(lv_nd)
                                    data(lv_conta_razao)
                                    data(lv_conta_descricao).
            case lv_tipo.
              when 'K' or 'FM' or 'FI'.
                call function 'CONVERSION_EXIT_ALPHA_INPUT'
                  exporting
                    input  = lv_conta_razao
                  importing
                    output = lv_saknr.

                select count(*)
                  from ska1
                  where saknr = lv_saknr.

                if sy-subrc ne 0.
                  v_por_erro = 'X'.
                  exit.
                endif.

                if strlen( lv_nd ) gt 4.
                  v_por_erro = 'X'.
                  exit.
                endif.

            endcase.
          endif.
                                                            "US157652

        endloop.
        "
        if v_por_erro = 'X'.
          me->atualiza_exported_pedido_coupa( iw_xml_header = wa_xml_header->* ).
          _fimloop = 'S'.
          continue.
        endif.


        call function 'NUMERIC_CHECK'
          exporting
            string_in  = wa_xml_header->taxa_d_cmbio
          importing
            string_out = wa_xml_header->taxa_d_cmbio
            htype      = ld_htype.

        if ld_htype = 'CHAR'.
          replace ',' in wa_xml_header->taxa_d_cmbio with '.'.

          if sy-subrc eq 0.
            condense wa_xml_header->taxa_d_cmbio no-gaps.

            if wa_xml_header->taxa_d_cmbio > 0.
              try.
                  wa_poheader-exch_rate  = wa_xml_header->taxa_d_cmbio.
                catch cx_sy_conversion_overflow.
                  me->atualiza_exported_pedido_coupa( iw_xml_header = wa_xml_header->* ).
                  continue.
              endtry.
            else.
              wa_poheader-exch_rate  = 1.
            endif.
          else.
            wa_poheader-exch_rate  = 1.
          endif.
        else.
          wa_poheader-exch_rate  = 1.
        endif.

        if wa_xml_header->taxa_fixa = 'false'.
          wa_poheader-ex_rate_fx = ' '.
        else.
          wa_poheader-ex_rate_fx = 'X'.
        endif.

        wa_poheader-incoterms1 = wa_xml_header->incoterms1.
        wa_poheader-incoterms2 = wa_xml_header->incoterms1.
        wa_poheader-ref_1      = wa_xml_header->safra.      "US184512

        if ( wa_xml_header->codio_pagamento is not initial ).
          read table lt_bapimepotextheader reference into data(lo_headertext) index 1.
          if ( sy-subrc is not initial ).

            append initial line to lt_bapimepotextheader reference into lo_headertext.
          endif.

          lo_headertext->po_number = wa_xml_header->po_number.
*     lo_headertext->PO_ITEM
          lo_headertext->text_id   = 'F07'.
          lo_headertext->text_form = '*'.
          lo_headertext->text_line = wa_xml_header->codio_pagamento.
        endif.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
        lv_length = strlen( wa_xml_header->justificativa ). "RJF

        if lv_length gt 0 and lv_length le 132.

          if ( wa_xml_header->justificativa is not initial ).
            append initial line to lt_bapimepotextheader reference into lo_headertext.
            lo_headertext->po_number = wa_xml_header->po_number.
            lo_headertext->text_id   = 'F01'.
            lo_headertext->text_form = '*'.
            lo_headertext->text_line = wa_xml_header->justificativa.
          endif.

        elseif lv_length gt 132. " RJF
          data: lv_ini   type i,
                lv_cont  type i,
                lv_qtd   type p decimals 2,
                lv_qtdl  type i,
                lv_fim   type i,
                lv_contx type i.
          lv_ini = 0.
          lv_cont = 132.
          lv_qtd   = lv_length / 132.
          lv_qtdl  = ceil( lv_qtd ).

          if lv_qtdl lt 3.
            lv_fim = lv_length - 132.
          else.
            lv_fim = ( ( lv_length ) - ( 132 * ( lv_qtdl - 1 ) ) ).
          endif.

          free: lv_contx.

          do lv_qtdl times.
            lv_contx = lv_contx + 1.

            if lv_contx gt 1.
              lv_ini = lv_ini + 132.
            endif.

            append initial line to lt_bapimepotextheader reference into lo_headertext.
            lo_headertext->po_number = wa_xml_header->po_number.
            lo_headertext->text_id   = 'F01'.
            if lv_contx eq 1.
              lo_headertext->text_form = '*'.
              lo_headertext->text_line = wa_xml_header->justificativa+lv_ini(lv_cont).
            elseif lv_contx eq lv_qtdl.
              lo_headertext->text_form = '='.
              lo_headertext->text_line = wa_xml_header->justificativa+lv_ini(lv_fim).
            else.
              lo_headertext->text_form = '='.
              lo_headertext->text_line = wa_xml_header->justificativa+lv_ini(lv_cont).
            endif.
          enddo.
        endif.
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

        if ( wa_xml_header->partner is not initial ).
          wa_poheader-diff_inv   = wa_xml_header->partner.
          wa_poheader-suppl_vend = wa_xml_header->partner.
        elseif wa_poheader-diff_inv is not initial.
          clear: wa_poheader-diff_inv, wa_poheader-suppl_vend.
        endif.

        if ( wa_xml_header->status eq 'cancelled' ).
          lv_delete = abap_true.
        endif.


        clear: v_price_sup, v_price_des.
        loop at t_xml_lines into data(wa_xml_lines) where id = wa_xml_header->id
                                                      and po_number = wa_xml_header->po_number.

          lv_poitem = wa_xml_lines-line_num * 10.

          if wa_xml_lines-percentual_reid is not initial.
            append initial line to lt_extension assigning field-symbol(<fs_extension>).
            <fs_extension>-structure = 'EKPO'.
            <fs_extension>-valuepart1 = 'PERCENTUAL-REID'.
            <fs_extension>-valuepart2 = lv_poitem.
            <fs_extension>-valuepart3 = wa_xml_lines-percentual_reid.
          endif.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
          if wa_xml_lines-status = 'cancelled'.
            continue.
          endif.
*-Equalização RISE x PRD - 19.07.2023 - JT - fim
          v_price_neg = wa_xml_lines-price * wa_xml_lines-quantity.
          if v_price_neg  < 0.
            multiply v_price_neg by -1.
            add v_price_neg to v_price_des.
          endif.
          split wa_xml_lines-description  at '-' into data(lv2_matnr)
                                                    data(lv2_matnr_des)
                                                    data(lv2_service).
          if ( wa_xml_lines-service_type eq abap_true ).

            split wa_xml_lines-description  at ' ' into lv2_matnr
                                                        lv2_matnr_des.
          endif.

          if ( lv2_matnr ca sy-abcde ).
            split lv2_matnr  at ' ' into lv2_matnr
                                         lv2_matnr_des.
          endif.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = lv2_matnr
            importing
              output = v_matnr.
          if v_matnr = '000000000000999992'.
            add v_price_neg to   v_price_sup.
          endif.

        endloop.


        select single knumv
           from ekko
          into @data(v_knumv)
          where ebeln = @wa_xml_header->po_number.

        select count(*)
           from prcd_elements
           into @data(_count)
           where knumv = @v_knumv
           and   kschl = 'HB01'.

        append initial line to lt_pocondheader reference into data(lo_pocondheader).
        refresh lt_pocondheader.
        "Desconto
        if ( wa_xml_header->desconto is not initial ) or v_price_des > 0.
          append initial line to lt_pocondheader reference into lo_pocondheader.
          lo_pocondheader->cond_type  = 'HB01'.
          if v_price_des > 0.
            lo_pocondheader->cond_value = v_price_des.
          else.
            lo_pocondheader->cond_value = wa_xml_header->desconto.
          endif.
          multiply lo_pocondheader->cond_value by -1.
          lo_pocondheader->currency   = wa_xml_header->currency.
          if _count ge 1.
            lo_pocondheader->change_id  = 'U'.
          else.
            lo_pocondheader->change_id  = 'I'.
          endif.
        elseif _count ge 1.
          append initial line to lt_pocondheader reference into lo_pocondheader.
          lo_pocondheader->cond_type  = 'HB01'.
          lo_pocondheader->cond_value = 0.
          multiply lo_pocondheader->cond_value by -1.
          lo_pocondheader->currency   = wa_xml_header->currency.
          lo_pocondheader->change_id  = 'D'.
        endif.

        select count(*)
         from prcd_elements
         into @data(_count2)
         where knumv = @v_knumv
         and   kschl = 'HB00'.
        "Frete
        if ( wa_xml_header->frete is not initial )  or v_price_sup > 0.
          append initial line to lt_pocondheader reference into lo_pocondheader.
          lo_pocondheader->cond_type  = 'HB00'.
          if v_price_sup > 0.
            lo_pocondheader->cond_value = v_price_sup.
          else.
            lo_pocondheader->cond_value = wa_xml_header->frete.
          endif.
          lo_pocondheader->currency   = wa_xml_header->currency.
          if _count2 ge 1.
            lo_pocondheader->change_id  = 'U'.
          else.
            lo_pocondheader->change_id  = 'I'.
          endif.
        elseif _count2 ge 1.
          append initial line to lt_pocondheader reference into lo_pocondheader.
          lo_pocondheader->cond_type  = 'HB00'.
          lo_pocondheader->cond_value = 0.
          lo_pocondheader->currency   = wa_xml_header->currency.
          lo_pocondheader->change_id  = 'D'.
        endif.

        free: lv_poitem, lv_pckg_no, lv_serial_no, lv_line_no.
        loop at t_xml_lines into wa_xml_lines where id = wa_xml_header->id
                                              and po_number = wa_xml_header->po_number.

          v_price_neg = wa_xml_lines-price * wa_xml_lines-quantity.
          if v_price_neg < 0. "Ignora linhas negativas
            continue.
          endif.
          "
          split wa_xml_lines-description  at '-' into lv2_matnr
                                                   lv2_matnr_des
                                                   lv2_service.
          if ( wa_xml_lines-service_type eq abap_true ).

            split wa_xml_lines-description  at ' ' into lv2_matnr
                                                        lv2_matnr_des.
          endif.

          if ( lv2_matnr ca sy-abcde ).
            split lv2_matnr  at ' ' into lv2_matnr
                                         lv2_matnr_des.
          endif.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = lv2_matnr
            importing
              output = v_matnr.
          if v_matnr = '000000000000999992'. "Ignora esse material usado como acrescimo /suplemento de preço
            continue.
          endif.


          lv_poitem = wa_xml_lines-line_num * 10.

          read table lt_poitem reference into data(lo_wa_poitem) with key po_item = lv_poitem.

          if ( sy-subrc is not initial ). "
            continue.
          endif.
*        lo_wa_poitem->po_item    = lv_poitem.
          lo_wa_poitem->plant      = wa_xml_lines-plant.
          lo_wa_poitem->quantity   = wa_xml_lines-quantity.
* "US184512
          lo_wa_poitem->incoterms1 = wa_xml_header->incoterms1.
          lo_wa_poitem->incoterms2 = wa_xml_header->incoterms1.
*"US184512

          if wa_xml_lines-status = 'cancelled' or lv_delete = abap_true.
            lo_wa_poitem->delete_ind = abap_true.
          endif.

          if wa_xml_lines-status = 'soft_closed_for_invoicing'
           or wa_xml_lines-status = 'closed'. " RJF - 2023.05.15 IR128497 - pedidos COUPA status 'closed/fechado'
            lo_wa_poitem->no_more_gr = 'X'.
            lo_wa_poitem->final_inv = 'X'.
          endif.
          "
          if wa_xml_lines-status = 'soft_closed_for_receiving'.
            lo_wa_poitem->no_more_gr = 'X'.
            if lo_wa_poitem->final_inv = 'X'.
              lo_wa_poitem->final_inv = 'Z'.
            endif.
          endif.

          if wa_xml_lines-status <> 'soft_closed_for_invoicing' and
             wa_xml_lines-status <> 'soft_closed_for_receiving' and
             wa_xml_lines-status <> 'closed'. " RJF - 2023.05.15 IR128497 - pedidos COUPA status 'closed/fechado'.
            if lo_wa_poitem->no_more_gr = 'X'.
              lo_wa_poitem->no_more_gr = 'Z'.
            endif.
            "
            if lo_wa_poitem->final_inv = 'X'.
              lo_wa_poitem->final_inv = 'Z'.
            endif.
          endif.

          if ( wa_xml_lines-trackingno is not initial ).
            lo_wa_poitem->trackingno = wa_xml_lines-trackingno.
          endif.

          lo_wa_poitem->tax_code   = wa_xml_lines-tax_code.

          lo_wa_poitem->batch = wa_xml_lines-charg.

          lo_wa_poitem->price_unit = wa_xml_lines-price_unit.
          if ( lo_wa_poitem->price_unit is initial ).
            lo_wa_poitem->price_unit = wa_xml_lines-price.
          else.
*          lo_wa_poitem->net_price = wa_xml_lines-price.
            lo_wa_poitem->net_price = wa_xml_lines-price * wa_xml_lines-price_unit.
          endif.

          if ( wa_xml_lines-banfn is not initial ).
            lo_wa_poitem->preq_no = wa_xml_lines-banfn.
            lo_wa_poitem->preq_item = wa_xml_lines-bnfpo.
          endif.

          clear lv_serial_no.
          if ( wa_xml_lines-account_allocations is not initial ).
            delete lt_poaccount where po_item eq lv_poitem.
            loop at wa_xml_lines-account_allocations reference into data(lo_wa_account_allocation).
              at first.
                lo_wa_poitem->distrib = '2'.
                clear lv_serial_no.
              endat.

              me->manipula_po_account(
                exporting
                  iv_ebelp          = lv_poitem
                  iv_acctasscat     = lo_wa_account_allocation->code
                  iw_rateio_account = lo_wa_account_allocation->*
                changing
                  ct_poaccount      = lt_poaccount    " Tipo de tabela para BAPIMEPOACCOUNT
                  cw_poitem         = lo_wa_poitem->*    " Item do pedido
                  cv_serial_no      = lv_serial_no
              ).

            endloop.
          else.
            me->manipula_po_account(
              exporting
                iv_ebelp      = lv_poitem
                iv_acctasscat = wa_xml_lines-acctasscat
*               iw_rateio_account = lo_wa_account_allocation->*
              changing
                ct_poaccount  = lt_poaccount    " Tipo de tabela para BAPIMEPOACCOUNT
                cw_poitem     = lo_wa_poitem->*    " Item do pedido
            ).
          endif.

          split wa_xml_lines-description  at '-' into data(lv_matnr)
                                                      data(lv_matnr_des)
                                                      data(lv_service).
          if ( wa_xml_lines-service_type eq abap_true ).
            replace ' ' in lv_matnr_des with ''.
            split lv_matnr_des  at ' ' into lv_service
                                            lv_matnr_des.
            if ( lv_service is initial ).
              split lv_matnr_des  at ' ' into lv_service
                                              lv_matnr_des.
            endif.
            split wa_xml_lines-description  at ' ' into lv_matnr
                                                        lv_matnr_des.
          endif.

          if ( lv_matnr ca sy-abcde ).
            split lv_matnr  at ' ' into lv_matnr
                                        lv_matnr_des.
          endif.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = lv_matnr
            importing
              output = lo_wa_poitem->material.

          if ( wa_xml_lines-service_type is not initial ) and wa_xml_header->bsart ne 'YDBP'.
*          FREE: lo_wa_poitem->material.
            lo_wa_poitem->short_text = lv_matnr_des.
            lo_wa_poitem->matl_group = '153310'.
            if ( lo_wa_poitem->tax_code is initial ).
              lo_wa_poitem->tax_code = 'S0'.
*            lo_wa_poitem->tax_code = 'S1'.
            endif.
            lv_pckg_no = lo_wa_poitem->pckg_no.
*          lo_wa_poitem->item_cat = 'D'.
*          lo_wa_poitem->pckg_no     = lv_pckg_no.

            read table lt_poaccount reference into data(lo_wa_poaccount) with key po_item = lv_poitem.
            if ( lo_wa_poaccount is bound ).
              if lo_wa_poaccount->serial_no = '00'.
                lo_wa_poaccount->quantity = 1.
              endif.
            endif.

            lv_subpckg_no2 = 0.
            loop at lt_poservices reference into data(lo_wa_poservices) where pckg_no  = lv_pckg_no
                                                                        and   subpckg_no is not initial.

              lv_subpckg_no2 = lo_wa_poservices->subpckg_no.
            endloop.
            lv_pckg_no = lv_subpckg_no2.


            loop at lt_poservices reference into data(lo_wa_poservices2) where  pckg_no  = lv_pckg_no
                                                                         and    subpckg_no is initial.


              lo_wa_poservices2->quantity    = lo_wa_poitem->net_price * wa_xml_lines-quantity.
              lo_wa_poservices2->price_unit  = wa_xml_lines-price_unit.
              lo_wa_poservices2->gr_price    = 1.

              "Neste caso o standar força qtde = 1
              lo_wa_poitem->price_unit      = 1.
              lo_wa_poitem->quantity        = 1.
              lo_wa_poitem->net_price       = wa_xml_lines-price * wa_xml_lines-quantity.
              "

              "Valores originais
              wa_zmmt0158-ebeln  = wa_poheader-po_number.
              wa_zmmt0158-ebelp  = lo_wa_poitem->po_item.
              wa_zmmt0158-srvpos = lo_wa_poservices2->service.
              wa_zmmt0158-menge  = wa_xml_lines-quantity.
              wa_zmmt0158-netwr  = lo_wa_poitem->net_price.
              append wa_zmmt0158 to lt_zmmt0158.
              "Valores originais
            endloop.

            read table lt_polimits reference into data(lo_wa_polimits) with key pckg_no = lv_pckg_no.
            if ( sy-subrc is initial ).
*          APPEND INITIAL LINE TO lt_polimits REFERENCE INTO DATA(lo_wa_polimits).
*          lo_wa_polimits->pckg_no   = lv_subpckg_no.
              lo_wa_polimits->no_limit  = abap_true.
              lo_wa_polimits->exp_value = lo_wa_poitem->net_price.
            endif.

            if ( wa_xml_lines-account_allocations is not initial ).
              delete lt_posrvaccessvalues where pckg_no = lv_pckg_no
                                          and   line_no = lo_wa_poservices2->line_no.
              loop at wa_xml_lines-account_allocations reference into lo_wa_account_allocation.
                at first.
                  clear lv_serial_no.
                endat.
                add 1 to lv_serial_no.
                append initial line to lt_posrvaccessvalues reference into data(lo_wa_valuesr).
                lo_wa_valuesr->pckg_no    = lv_pckg_no. "lv_subpckg_no.
                lo_wa_valuesr->line_no    = lo_wa_poservices2->line_no. "lv_line_no.
                lo_wa_valuesr->serno_line = lv_serial_no.
                lo_wa_valuesr->percentage = lo_wa_account_allocation->pct.
                lo_wa_valuesr->serial_no  = lv_serial_no.
                lo_wa_valuesr->quantity   = lo_wa_account_allocation->amount.
                lo_wa_valuesr->net_value  = 1.


              endloop.
            else.
              read table lt_posrvaccessvalues reference into data(lo_wa_values) with key pckg_no = lv_pckg_no
                                                                                         line_no = lo_wa_poservices2->line_no.
              if ( sy-subrc is initial ).
                lo_wa_values->serno_line = '01'.
                lo_wa_values->percentage = 100.
                lo_wa_values->serial_no  = lo_wa_poaccount->serial_no.
*              lo_wa_values->quantity   = lo_wa_poitem->net_price * wa_xml_lines-quantity.
                lo_wa_values->quantity   = wa_xml_lines-price * wa_xml_lines-quantity.
                lo_wa_values->net_value  = 1.
              endif.
            endif.
          elseif wa_xml_header->bsart ne 'YDBP'.
            if ( wa_xml_lines-banfn is not initial ).
              v_price   = wa_xml_lines-price * wa_xml_lines-quantity.
              v_prices =  v_price.
              data(lv_servico) = me->verifica_servico_requisicao( exporting iv_banfn             = wa_xml_lines-banfn
                                                                            iv_bnfpo             = wa_xml_lines-bnfpo
                                                                            iv_price             = v_prices
                                                                            iv_unit              = wa_xml_lines-price_unit
                                                                  changing  et_poservices        = lt_poservices
                                                                            et_polimits          = lt_polimits
                                                                            et_posrvaccessvalues = lt_posrvaccessvalues
                                                                            cv_pckgno            = lv_pckg_no
                                                                            cv_serialno          = lv_serial_no
                                                                            cv_lineno            = lv_line_no ).
              if ( lv_servico is not initial ).
                lo_wa_poitem->item_cat = 'D'.
                lo_wa_poitem->pckg_no  = lv_pckg_no.

                if ( lo_wa_poaccount is bound ).
*            ADD 1 TO lv_serial_no.
                  lo_wa_poaccount->serial_no = lv_serial_no.
                endif.
              endif.
            endif.
          endif.

          read table lt_poschedule reference into data(lo_wa_poschedule) with key po_item = lv_poitem.
          if ( sy-subrc is initial ).
            if wa_xml_lines-service_type = 'X' and wa_xml_header->bsart ne 'YDBP'.
              lo_wa_poschedule->quantity      = 1. "erro,. altera a quantidade do item
            else.
              lo_wa_poschedule->quantity      = wa_xml_lines-quantity.
            endif.
            lo_wa_poschedule->sched_line    = 1.

            split wa_xml_lines-need_by_date at 'T' into data(lv_data_text)
                                                        data(lv_data_rest).
            if lv_data_text is not initial.

              lo_wa_poschedule->delivery_date = |{ lv_data_text(4)   }| &&
                                                |{ lv_data_text+5(2) }| &&
                                                |{ lv_data_text+8(2) }| .

            endif.
          endif.

          if wa_xml_lines-percentual_reid is not initial.
            clear: vg_perc_reid.
            vg_perc_reid = wa_xml_lines-percentual_reid.
            condense vg_perc_reid no-gaps.
            translate vg_perc_reid using ',.'.
            append value #( aliquota = vg_perc_reid ebelp = lv_poitem ebeln = wa_xml_lines-po_number reidi = vg_perc_reid ) to tg_item_pedido.
          endif.
        endloop.

        "  Linhas  NOVAS
        free: lv_poitem, lv_pckg_no, lv_serial_no, lv_line_no.
        loop at t_xml_lines into wa_xml_lines where id = wa_xml_header->id
                                                      and po_number = wa_xml_header->po_number.

          v_price_neg = wa_xml_lines-price * wa_xml_lines-quantity.
          if v_price_neg < 0. "Ignora linhas negativas
            continue.
          endif.

          split wa_xml_lines-description  at '-' into lv2_matnr
                                                   lv2_matnr_des
                                                   lv2_service.
          if ( wa_xml_lines-service_type eq abap_true ).

            split wa_xml_lines-description  at ' ' into lv2_matnr
                                                        lv2_matnr_des.
          endif.

          if ( lv2_matnr ca sy-abcde ).
            split lv2_matnr  at ' ' into lv2_matnr
                                         lv2_matnr_des.
          endif.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = lv2_matnr
            importing
              output = v_matnr.
          if v_matnr = '000000000000999992'. "Ignora esse material usado como acrescimo /suplemento de preço
            continue.
          endif.

          lv_poitem = wa_xml_lines-line_num * 10.
          read table lt_poitem reference into lo_wa_poitem with key po_item = lv_poitem.

          if ( sy-subrc is initial ) or  wa_xml_lines-status = 'cancelled' or lv_delete = abap_true. "
            continue.
          endif.

          append initial line to lt_poitem    reference into lo_wa_poitem.

          lo_wa_poitem->po_item    = lv_poitem.
          lo_wa_poitem->plant      = wa_xml_lines-plant.
          lo_wa_poitem->quantity   = wa_xml_lines-quantity.
          lo_wa_poitem->calctype   = 'B'.

          if ( wa_xml_lines-trackingno is not initial ).
            lo_wa_poitem->trackingno = wa_xml_lines-trackingno.
          endif.

          lo_wa_poitem->tax_code   = wa_xml_lines-tax_code.

          lo_wa_poitem->batch = wa_xml_lines-charg.

          lo_wa_poitem->price_unit = wa_xml_lines-price_unit.
          if ( lo_wa_poitem->price_unit is initial ).
            lo_wa_poitem->price_unit = wa_xml_lines-price.
          else.
*          lo_wa_poitem->net_price = wa_xml_lines-price.
            lo_wa_poitem->net_price = wa_xml_lines-price * wa_xml_lines-price_unit.
          endif.

          if ( wa_xml_lines-banfn is not initial ).
            lo_wa_poitem->preq_no = wa_xml_lines-banfn.
            lo_wa_poitem->preq_item = wa_xml_lines-bnfpo.
          endif.

          if wa_xml_lines-status = 'soft_closed_for_invoicing' or
            wa_xml_lines-status = 'closed'. " RJF - 2023.05.15 IR128497 - pedidos COUPA status 'closed/fechado'
            lo_wa_poitem->no_more_gr = 'X'.
            lo_wa_poitem->final_inv = 'X'.
          endif.

          if wa_xml_lines-status = 'soft_closed_for_receiving'.
            lo_wa_poitem->no_more_gr = 'X'.
            if lo_wa_poitem->final_inv = 'X'.
              lo_wa_poitem->final_inv = 'Z'.
            endif.
          endif.

          clear lv_serial_no.
          if ( wa_xml_lines-account_allocations is not initial ).
            loop at wa_xml_lines-account_allocations reference into lo_wa_account_allocation.
              at first.
                lo_wa_poitem->distrib = '2'.
                clear lv_serial_no.
              endat.

              me->manipula_po_account(
                exporting
                  iv_ebelp          = lv_poitem
                  iv_acctasscat     = lo_wa_account_allocation->code
                  iw_rateio_account = lo_wa_account_allocation->*
                changing
                  ct_poaccount      = lt_poaccount    " Tipo de tabela para BAPIMEPOACCOUNT
                  cw_poitem         = lo_wa_poitem->*    " Item do pedido
                  cv_serial_no      = lv_serial_no
              ).

            endloop.
          else.
            me->manipula_po_account(
              exporting
                iv_ebelp      = lv_poitem
                iv_acctasscat = wa_xml_lines-acctasscat
*               iw_rateio_account = lo_wa_account_allocation->*
              changing
                ct_poaccount  = lt_poaccount    " Tipo de tabela para BAPIMEPOACCOUNT
                cw_poitem     = lo_wa_poitem->*    " Item do pedido
            ).
          endif.


          split wa_xml_lines-description  at '-' into lv_matnr
                                                      lv_matnr_des
                                                      lv_service.
          if ( wa_xml_lines-service_type eq abap_true ).
            replace ' ' in lv_matnr_des with ''.
            split lv_matnr_des  at ' ' into lv_service
                                            lv_matnr_des.
            if ( lv_service is initial ).
              split lv_matnr_des  at ' ' into lv_service
                                              lv_matnr_des.
            endif.
            split wa_xml_lines-description  at ' ' into lv_matnr
                                                        lv_matnr_des.
          endif.

          if ( lv_matnr ca sy-abcde ).
            split lv_matnr  at ' ' into lv_matnr
                                        lv_matnr_des.
          endif.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = lv_matnr
            importing
              output = lo_wa_poitem->material.

          if ( wa_xml_lines-service_type is not initial ) and wa_xml_header->bsart ne 'YDBP'.
*          FREE: lo_wa_poitem->material.
            lo_wa_poitem->short_text = lv_matnr_des.
            lo_wa_poitem->matl_group = '153310'.
            if ( lo_wa_poitem->tax_code is initial ).
*            lo_wa_poitem->tax_code = 'S1'.
              lo_wa_poitem->tax_code = 'S0'.
            endif.
            add 10 to lv_pckg_no.
            lo_wa_poitem->item_cat = 'D'.
            lo_wa_poitem->pckg_no     = lv_pckg_no.

            if ( wa_xml_lines-account_allocations is initial ).
              read table lt_poaccount reference into lo_wa_poaccount with key po_item = lv_poitem.
              if ( lo_wa_poaccount is bound ).
                if lo_wa_poaccount->serial_no = '00'.
                  lo_wa_poaccount->quantity = 1.
                endif.
              endif.
            endif.

            add 1 to lv_line_no.
            append initial line to lt_poservices reference into lo_wa_poservices.
            lo_wa_poservices->pckg_no  = lv_pckg_no.
            lo_wa_poservices->line_no  = lv_line_no.
            lo_wa_poservices->pln_line = lv_line_no.

            "incrementa novamente
            call function 'NUMBER_GET_NEXT'
              exporting
                nr_range_nr             = '01'
                object                  = 'SERVICE'
              importing
                number                  = lo_wa_poservices->subpckg_no
              exceptions
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                others                  = 8.

            lv_subpckg_no = lo_wa_poservices->subpckg_no.
            append initial line to lt_poservices reference into lo_wa_poservices.
            add 1 to lv_line_no.
            lo_wa_poservices->pckg_no = lv_subpckg_no.
            lo_wa_poservices->line_no = lv_line_no.

            condense lv_service no-gaps.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = lv_service
              importing
                output = lo_wa_poservices->service.
            lo_wa_poservices->quantity    = lo_wa_poitem->net_price * wa_xml_lines-quantity.
            lo_wa_poservices->price_unit  = wa_xml_lines-price_unit.
            lo_wa_poservices->gr_price    = 1.

            "Neste caso o standar força qtde = 1
            lo_wa_poitem->price_unit      = 1.
            lo_wa_poitem->quantity        = 1.
*          lo_wa_poitem->net_price       = lo_wa_poitem->net_price * wa_xml_lines-quantity.
            lo_wa_poitem->net_price       = wa_xml_lines-price  * wa_xml_lines-quantity.
            "

            "Valores originais
            wa_zmmt0158-ebeln  = wa_poheader-po_number.
            wa_zmmt0158-ebelp  = lo_wa_poitem->po_item.
            wa_zmmt0158-srvpos = lo_wa_poservices->service.
            wa_zmmt0158-menge  = wa_xml_lines-quantity.
            wa_zmmt0158-netwr  = lo_wa_poitem->net_price.
            append wa_zmmt0158 to lt_zmmt0158.
            "Valores originais

            append initial line to lt_polimits reference into lo_wa_polimits.
            lo_wa_polimits->pckg_no   = lv_subpckg_no.
            lo_wa_polimits->no_limit  = abap_true.
            lo_wa_polimits->exp_value = lo_wa_poitem->net_price.

            if ( lo_wa_poaccount is bound ).
              if ( lo_wa_poaccount->serial_no is initial ).
                add 1 to lv_serial_no.
                lo_wa_poaccount->serial_no = lv_serial_no.
              else.
                lv_serial_no = lo_wa_poaccount->serial_no.
              endif.
            endif.

            if ( wa_xml_lines-account_allocations is not initial ).
              loop at wa_xml_lines-account_allocations reference into lo_wa_account_allocation.
                at first.
                  clear lv_serial_no.
                endat.
                add 1 to lv_serial_no.
                append initial line to lt_posrvaccessvalues reference into lo_wa_valuesr.
                lo_wa_valuesr->pckg_no    = lv_subpckg_no.
                lo_wa_valuesr->line_no    = lv_line_no.
                lo_wa_valuesr->serno_line = lv_serial_no.
                lo_wa_valuesr->percentage = lo_wa_account_allocation->pct.
                lo_wa_valuesr->serial_no  = lv_serial_no.
                lo_wa_valuesr->quantity   = lo_wa_account_allocation->amount.
                lo_wa_valuesr->net_value  = 1.
              endloop.
            else.
              append initial line to lt_posrvaccessvalues reference into lo_wa_values.
              lo_wa_values->pckg_no    = lv_subpckg_no.
              lo_wa_values->line_no    = lv_line_no.
              lo_wa_values->serno_line = '01'.
              lo_wa_values->percentage = 100.
              lo_wa_values->serial_no  = lv_serial_no.
*            lo_wa_values->quantity   = lo_wa_poitem->net_price * wa_xml_lines-quantity.
              lo_wa_values->quantity   = wa_xml_lines-price * wa_xml_lines-quantity.
              lo_wa_values->net_value  = 1.
            endif.
          elseif wa_xml_header->bsart ne 'YDBP'.
            if ( wa_xml_lines-banfn is not initial ).
              v_price   = wa_xml_lines-price * wa_xml_lines-quantity.
              v_prices =  v_price.
              lv_servico = me->verifica_servico_requisicao( exporting iv_banfn             = wa_xml_lines-banfn
                                                                      iv_bnfpo             = wa_xml_lines-bnfpo
                                                                      iv_price             = v_prices
                                                                      iv_unit              = wa_xml_lines-price_unit
                                                            changing  et_poservices        = lt_poservices
                                                                      et_polimits          = lt_polimits
                                                                      et_posrvaccessvalues = lt_posrvaccessvalues
                                                                      cv_pckgno            = lv_pckg_no
                                                                      cv_serialno          = lv_serial_no
                                                                      cv_lineno            = lv_line_no ).
              if ( lv_servico is not initial ).
                lo_wa_poitem->item_cat = 'D'.
                lo_wa_poitem->pckg_no  = lv_pckg_no.

                if ( lo_wa_poaccount is bound ).
*            ADD 1 TO lv_serial_no.
                  lo_wa_poaccount->serial_no = lv_serial_no.
                endif.
              endif.
            endif.
          endif.

          append initial line to lt_poschedule reference into lo_wa_poschedule.
          lo_wa_poschedule->po_item       = lv_poitem.

          if wa_xml_lines-service_type = 'X'.
            lo_wa_poschedule->quantity      = 1. "erro, altera a quantidade do item
          else.
            lo_wa_poschedule->quantity      = wa_xml_lines-quantity.
          endif.
          lo_wa_poschedule->sched_line    = 1.

          split wa_xml_lines-need_by_date at 'T' into lv_data_text
                                                      lv_data_rest.
          if lv_data_text is not initial.

            lo_wa_poschedule->delivery_date = |{ lv_data_text(4)   }| &&
                                              |{ lv_data_text+5(2) }| &&
                                              |{ lv_data_text+8(2) }| .

          endif.

        endloop.
        "  Linhas  NOVAS
        delete  lt_pocond where cond_type  ne 'PBXX' and cond_type  ne 'PB00'. "25.11.2022 ALRS
        "
        loop at lt_poitem into data(ls_po_item).
          read table lt_pocond assigning field-symbol(<pocond>) with key itm_number = ls_po_item-po_item
                                                                         cond_type  = 'PBXX'.
          if sy-subrc is initial.
            <pocond>-cond_value = ls_po_item-net_price.
            <pocond>-cond_p_unt = ls_po_item-price_unit.
          else.
            read table lt_pocond assigning field-symbol(<pocond2>) with key itm_number = ls_po_item-po_item
                                                                            cond_type  = 'PB00'.
            if sy-subrc is initial.
              <pocond2>-cond_value = ls_po_item-net_price.
              <pocond2>-cond_p_unt = ls_po_item-price_unit.
            endif.
          endif.
        endloop.

        loop at lt_poschedule assigning field-symbol(<poschedule>).
          data(_tam) = strlen( <poschedule>-delivery_date ).
          if _tam = 8.
            concatenate <poschedule>-delivery_date+6(2) <poschedule>-delivery_date+4(2) <poschedule>-delivery_date+0(4) into <poschedule>-delivery_date separated by '.'.
          endif.
        endloop.

        if _altera = 'S'.
          "troca pelo IVA e ajusta pra calcular preço 1,00
          loop at  lt_poitem_b into data(_item_b).
            read table lt_poitem assigning field-symbol(<itemb>) with key po_item = _item_b-po_item.
            <itemb>-tax_code   = _item_b-tax_code.
            <itemb>-net_price  = 1.
            <itemb>-price_unit = 1.

            read table lt_pocond assigning field-symbol(<pocond4>) with key itm_number = <itemb>-po_item
                                                                            cond_type  = 'PBXX'.
            if sy-subrc is initial.
              <pocond4>-cond_value =  <itemb>-net_price.
              <pocond4>-cond_p_unt =  <itemb>-price_unit.
            endif.
          endloop.
          refresh lt_poitem_b.
        endif.

        "alterado em 29.09.2025
        me->atualiza_exported_pedido_coupa( iw_xml_header = wa_xml_header->* ).
        "alterado em 29.09.2025

        data(lt_return) = bapi_po_change(
          exporting
            iv_test               = iv_test     " Campo de seleção
            iw_poheader           = wa_poheader     " Pedido dds.cabeçalho
            iv_no_price_from_po   = abap_false
          changing
            ct_pocondheader       = lt_pocondheader
            ct_poitem             = lt_poitem     " Tipo de tabela para BAPIMEPOITEM
            ct_pocond             = lt_pocond    " Tipo de tabela para BAPIMEPOCOND
            ct_popartner          = lt_popartner    " Tipo de tabela para BAPIEKKOP
            ct_bapimepotextheader = lt_bapimepotextheader    " Tipo de tabela para BAPIMEPOTEXTHEADER
            ct_account            = lt_poaccount   " Campos de classificação contábil do pedido
            ct_schedule           = lt_poschedule    " Tipo de tabela para BAPIMEPOSCHEDULE
            ct_bapiesllc          = lt_poservices
            ct_polimits           = lt_polimits
            ct_posrvaccessvalues  = lt_posrvaccessvalues
            ct_extension          = lt_extension ).

        wa_xml_header->icon_result = '@DH@'.
        wa_xml_header->tt_return = lt_return.
        wa_xml_header->exported  = 'True'.

        if ( line_exists( wa_xml_header->tt_return[ type = 'E' ] ) ).
          wa_xml_header->icon = '@5C@'.
          if _altera = 'S'.
            _fimloop = 'S'.
          else.

            loop at lt_return into data(wa_return) where  type = 'E'
                                                   and    id   = '06'
                                                   and    number = '214'. " Preço negativo
              vg_ebelp = wa_return-row.
              multiply vg_ebelp by 10.
              read table lt_poitem assigning field-symbol(<item>) with key po_item = vg_ebelp.
              if sy-subrc = 0.
                case  <item>-tax_code .
                  when 'C0'.
                    <item>-tax_code = 'C1'.
                    _altera = 'S'.
                  when 'C1'.
                    <item>-tax_code = 'C0'.
                    _altera = 'S'.
                  when 'I0'.
                    <item>-tax_code = 'I1'.
                    _altera = 'S'.
                  when 'I1'.
                    <item>-tax_code = 'I0'.
                    _altera = 'S'.
                  when 'Y1'.
                    <item>-tax_code = 'Y2'.
                    _altera = 'S'.
                  when 'Y2'.
                    <item>-tax_code = 'Y1'.
                    _altera = 'S'.
                  when 'Y3'.
                    <item>-tax_code = 'Y1'.
                    _altera = 'S'.
                  when 'Y5'.
                    <item>-tax_code = 'Y1'.
                    _altera = 'S'.
                endcase.
                if _altera = 'S'.
                  append <item> to lt_poitem_b.
                endif.
              endif.
            endloop.
            if _altera = 'N'.
              _fimloop = 'S'.
              clear wa_return.
            endif.
          endif.

        else.


*||==========================================================CS2023000312 / AOENNING.
          select * from ekpo into table tg_ekpo where ebeln eq wa_xml_header->po_number  .
          if sy-subrc eq 0.
            loop at tg_ekpo assigning field-symbol(<ls_ekpo>).
              read table tg_item_pedido into data(ws_item) with key ebeln = <ls_ekpo>-ebeln ebelp = <ls_ekpo>-ebelp.
              if sy-subrc eq 0 and ws_item-aliquota is not initial.
                <ls_ekpo>-aliquota = ws_item-aliquota.
                <ls_ekpo>-reidi    = 'S'.
              else.
                <ls_ekpo>-reidi    = ''.
              endif.
            endloop.
            modify ekpo from table tg_ekpo.
            commit work.
            clear: ws_item.
          endif.
*||==========================================================CS2023000312 / AOENNING.



          if _altera = 'N'.
            _fimloop = 'S'.
          endif.
          _altera = 'N'.
          if ( iv_test is not initial ).
            wa_xml_header->icon = '@01@'.
          else.
            if lt_zmmt0158[] is not initial.
              modify zmmt0158 from table lt_zmmt0158.
              commit work.
            endif.
            wa_xml_header->icon = '@DF@'.
            if ( lv_delete is initial ).
              wa_xml_header->pedido_sap = '@0Z@'.
            else.
              wa_xml_header->pedido_sap = '@11@'.
            endif.
*-CS1117561-#RIMINI-14.07.2023-BEGIN
*            me->atualiza_pedido_coupa( iv_id = wa_xml_header->id ).
            me->atualiza_pedido_coupa(
              iv_id   = wa_xml_header->id
              iv_tipo = 'MOD' ).
*-CS1117561-#RIMINI-14.07.2023-END
            "Faz o recalculo do imposto
            do 5 times.
              refresh lt_poitem_a.
              loop at lt_poitem into data(lo_wa_poitem_a).
                data(_item) = lo_wa_poitem_a-po_item.
                clear lo_wa_poitem_a.
                lo_wa_poitem_a-po_item = _item.
                lo_wa_poitem_a-calctype = 'G'. "Aceitar comp.preço sem modif., calcular novamente imposto
                append lo_wa_poitem_a to lt_poitem_a.
              endloop.
              lt_return = zcl_integracao_coupa_ped_comp=>bapi_po_change(
                exporting
                  iv_test     = iv_test
                  iw_poheader = value bapimepoheader( po_number = wa_poheader-po_number )
                changing
                  ct_poitem   = lt_poitem_a ).
            enddo.
          endif.
        endif.

        if ( iv_test is initial ).
          me->atualiza_mensagem_pedido_coupa( iw_xml_header = wa_xml_header->* ).
          me->atualiza_exported_pedido_coupa( iw_xml_header = wa_xml_header->* ).
        endif.

        clear:
          wa_poheader,
          lv_ebeln.

        free:
          lt_pocondheader,
          lt_poitem,
          lt_pocond,
          lt_popartner,
          lt_bapimepotextheader,
          lt_poaccount,
          lt_poschedule,
          lt_poservices,
          lt_polimits  ,
          lt_posrvaccessvalues.
      endwhile.

    endloop.

  endmethod.


  method realiza_quebra_xml.
    types: begin of lty_cnpj,
             cnpj_xml type zstped_header-cnpj,
             cnpj     type lfa1-stcd1,
           end of lty_cnpj.
    data: lt_xml_header type tt_xml_header,
          lt_xml_lines  type tt_xml_lines.

    data: lva_xml_xstring type xstring,
          lt_xml_table    type standard table of smum_xmltb,
          lt_xml_info     type table of smum_xmltb,
          lt_return       type standard table of bapiret2,
          lt_cnpj         type standard table of lty_cnpj,
          lo_xml_ped      type ref to cl_xml_document,
          wa_xml_header   type zstped_header,
          wa_xml_lines    type zst_ped_lines,
          l_tabix         type sy-tabix,
          l_line_tag      type string,
          lv_con_lin      type string.

    data: vperc(6)   type p decimals 1,
          vvproz     type vproz,
          vcount     type i,
          vok(1),
          vajuste(6) type p decimals 1.

    lo_integracao->zif_integracao~get_registro(
      importing
        e_integracao    = data(e_integracao_post) ).

    create object lo_xml_ped.

*    lv_subrc = lo_xml_ped->parse_string( e_integracao_post-ds_data_retorno ).

    call function 'SCMS_STRING_TO_XSTRING'
      exporting
        text   = e_integracao_post-ds_data_retorno
      importing
        buffer = lva_xml_xstring
      exceptions
        failed = 1
        others = 2.

    if sy-subrc = 0.

      call function 'SMUM_XML_PARSE'
        exporting
          xml_input = lva_xml_xstring
        tables
          xml_table = lt_xml_table[]  " XML Table structure used for retreive and output XML doc
          return    = lt_return[].    " XML Table structure used for retreive and output XML doc

      lt_xml_info[] = corresponding #( lt_xml_table[] ).
      loop at lt_xml_info assigning field-symbol(<fs_info>).

        l_tabix = sy-tabix.
        translate <fs_info>-cname to upper case.

        case <fs_info>-cname.
          when 'ORDER-HEADER'.
            l_line_tag = 'REQUISITION-HEADER'.
*            IF wa_xml_header-id IS NOT INITIAL.
            append initial line to lt_xml_header reference into data(lo_wa_header).
*              APPEND wa_xml_header TO t_xml_header.
*              APPEND wa_xml_lines  TO t_xml_lines.
            clear: wa_xml_header, wa_xml_lines,
*-CS1126179-#RIMINI-03.08.2023-BEGIN
                   lv_con_lin.
*-CS1126179-#RIMINI-03.08.2023-END
*            ENDIF.


          when 'ORDER-LINE'.
*            IF wa_xml_lines-description IS NOT INITIAL.
            append initial line to lt_xml_lines reference into data(lo_wa_lines).
*              APPEND wa_xml_lines TO t_xml_lines.

*              CLEAR wa_xml_lines.
            lo_wa_lines->id         = lo_wa_header->id.

            lo_wa_lines->po_number  = lo_wa_header->po_number.
            lo_wa_lines->plant      = wa_xml_lines-plant.
            lo_wa_lines->description = wa_xml_lines-description.
            l_line_tag = 'ORDER-LINE'.

          when 'ACCOUNT-ALLOCATION'.
            if ( lo_wa_lines is bound ).
              append initial line to lo_wa_lines->account_allocations reference into data(lo_wa_account_allocation).
              l_line_tag = 'ACCOUNT-ALLOCATION'.
            endif.

          when 'ID'.
            if l_line_tag eq 'REQUISITION-HEADER' and lo_wa_header->id is initial.
*            wa_xml_header-id = <fs_info>-cvalue.
              lo_wa_header->id = <fs_info>-cvalue.
*            wa_xml_lines-id  = <fs_info>-cvalue.
*              CLEAR l_line_tag.
            elseif l_line_tag eq 'REQUISITION-HEADER'.
              lo_wa_header->requisition_id = <fs_info>-cvalue.
              clear l_line_tag.
            elseif l_line_tag eq 'ORDER-LINE'.
              lo_wa_lines->id_line  = <fs_info>-cvalue.
            elseif l_line_tag eq 'ACCOUNT-ALLOCATION'.
              if ( lo_wa_account_allocation is bound ).
                lo_wa_account_allocation->id = <fs_info>-cvalue.
              endif.
            endif.

          when 'REQUISITION-HEADER'.
            l_line_tag = 'REQUISITION-HEADER'.

          when 'SHIPPING-TERM'.
            l_line_tag = 'SHIPPING-TERM'.

          when 'CNPJ'.
            lo_wa_header->cnpj = <fs_info>-cvalue.

            if ( <fs_info>-cvalue is not initial ).
              read table lt_cnpj transporting no fields with key cnpj_xml = <fs_info>-cvalue.
              if ( sy-subrc is not initial ).
                append initial line to lt_cnpj reference into data(lo_wa_cnpj).
                lo_wa_cnpj->cnpj_xml =  <fs_info>-cvalue.
                call function 'CONVERSION_EXIT_CGCBR_INPUT'
                  exporting
                    input     = lo_wa_cnpj->cnpj_xml
                  importing
                    output    = lo_wa_cnpj->cnpj
                  exceptions
                    not_valid = 1
                    others    = 2.
              endif.
            endif.

          when 'IE'.
            lo_wa_header->ie = <fs_info>-cvalue.

*          WHEN 'SERVICE-TYPE'.
*            IF ( <fs_info>-cvalue NE 'non_service' ).
*              lo_wa_lines->service_type = abap_true.
*            ENDIF.
          when 'ITEM-TYPE'.
            if ( <fs_info>-cvalue eq 'Service Quantity' ).
              lo_wa_lines->service_type = abap_true.
            endif.

          when 'ACCOUNT'.
            l_line_tag = 'ACCOUNT'.

          when 'CREATED-AT'.
            lo_wa_header->created_a = <fs_info>-cvalue.

          when 'PO-NUMBER'.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = <fs_info>-cvalue
              importing
                output = lo_wa_header->po_number.
*          wa_xml_lines-po_number = <fs_info>-cvalue.

          when 'RISCO-DE-TERCEIROS'.
            free: l_line_tag.
          when 'IVA'.
            l_line_tag = 'IVA'.
          when 'CURRENCY'.
            l_line_tag = 'CURRENCY'.
          when 'PAYMENT-TERM'.
            l_line_tag = 'PAYMENT-TERM'.
          when 'CODE'.
            if l_line_tag eq 'SHIPPING-TERM'.
              lo_wa_header->incoterms1 = <fs_info>-cvalue.
              clear l_line_tag.

            elseif l_line_tag eq 'ACCOUNT'.

              if ( lo_wa_account_allocation is bound ).
                if lo_wa_account_allocation->code is initial.
                  lo_wa_account_allocation->code = <fs_info>-cvalue.
                else.
                  lo_wa_lines->acctasscat = <fs_info>-cvalue.
                endif.
              else.
                lo_wa_lines->acctasscat = <fs_info>-cvalue.
              endif.
              clear l_line_tag.

            elseif l_line_tag eq 'CURRENCY'.
              lo_wa_header->currency = <fs_info>-cvalue.
              free: l_line_tag.
            elseif  l_line_tag eq 'PAYMENT-TERM'.
              lo_wa_header->payment_term = <fs_info>-cvalue.
*              FREE: l_line_tag.
            elseif ( l_line_tag eq 'ACCOUNT-ALLOCATION'
                 and lo_wa_account_allocation is bound ).
              lo_wa_account_allocation->code = <fs_info>-cvalue.
              free: l_line_tag.
            endif.

          when 'STATUS'.
            if  l_line_tag eq 'REQUISITION-HEADER'.
              lo_wa_header->status = <fs_info>-cvalue.
            elseif l_line_tag = 'ORDER-LINE'.
              lo_wa_lines->status = <fs_info>-cvalue.
            endif.
          when 'EXPORTED'.
            lo_wa_header->exported = <fs_info>-cvalue.
          when 'ANO-SAFRA'.                                "US184512
            if <fs_info>-type = 'V'.
              lo_wa_header->safra = <fs_info>-cvalue.
            endif.

          when 'TIPO-DE-FORNECEDOR'.
            l_line_tag = 'TIPO-DE-FORNECEDOR'.
*            lo_wa_header->tipo_fornecedor = <fs_info>-cvalue.

          when 'ENTREGA-FUTURA'.
            if ( <fs_info>-cvalue = 'true' ).
              lo_wa_header->entrega_futura = abap_true.
            endif.

          when 'TIPO-DE-PEDIDO'.
            l_line_tag = 'TIPO-DE-PEDIDO'.
          when 'NUMBER'.
            if lo_wa_header->vendor is initial.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = <fs_info>-cvalue(10)
                importing
                  output = lo_wa_header->vendor.

              select count(*)
                from lfa1
                where lifnr = lo_wa_header->vendor.
              if sy-subrc is not initial.
                free: lo_wa_header->vendor.
              endif.
            endif.

          when 'CD-FORNECEDOR-PARCEIRO'.
            if lo_wa_header->partner is initial.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = <fs_info>-cvalue(10)
                importing
                  output = lo_wa_header->partner.

              select count(*)
                from lfa1
                where lifnr = lo_wa_header->partner.
              if sy-subrc is not initial.
                free: lo_wa_header->partner.
              endif.
            endif.


          when 'EXTERNAL-REF-CODE'.
            case  l_line_tag.
              when 'IVA'.
                lo_wa_lines->tax_code = <fs_info>-cvalue.
                free: l_line_tag.
              when 'TIPO-DE-PEDIDO'.
                lo_wa_header->tipo_pedido = <fs_info>-cvalue.
                free: l_line_tag.
              when 'TIPO-DE-FORNECEDOR'.
                lo_wa_header->tipo_fornecedor = <fs_info>-cvalue.
            endcase.

          when 'EMPRESA'.
            lo_wa_header->empresa = <fs_info>-cvalue.

          when 'TAXA-D-CMBIO'.
            if <fs_info>-cvalue eq 'Não'.
              lo_wa_header->taxa_d_cmbio = '1'.
            else.
              lo_wa_header->taxa_d_cmbio = <fs_info>-cvalue.
            endif.

          when 'TAXA-FIXA'.
            lo_wa_header->taxa_fixa = <fs_info>-cvalue.

          when 'DESCRIPTION'.
            if ( l_line_tag = 'ORDER-LINE' ).
              lo_wa_lines->description = <fs_info>-cvalue.
            elseif  l_line_tag eq 'PAYMENT-TERM'.
              if ( lo_wa_header->payment_term is initial ).
                lo_wa_header->payment_term = <fs_info>-cvalue.
              endif.
              free: l_line_tag.
            endif.
          when 'LINE-NUM'.
            lo_wa_lines->line_num = <fs_info>-cvalue.
          when 'LOTE'.
            lo_wa_lines->charg = <fs_info>-cvalue.
          when 'PRICE'.
            lo_wa_lines->price = <fs_info>-cvalue.

          when 'QUANTITY'.
            lo_wa_lines->quantity = <fs_info>-cvalue.

          when 'LOCATION-CODE'.
            wa_xml_lines-plant = <fs_info>-cvalue.

          when 'REQUESTED-BY'.
*            wa_xml_lines-requested_by = <fs_info>-cvalue.
          when 'N-ACOMPANHAMENTO'.
            lo_wa_lines->trackingno = <fs_info>-cvalue.

          when 'PRECO-POR'.
            try.
                lo_wa_lines->price_unit = <fs_info>-cvalue.
              catch cx_sy_conversion_overflow.
                clear lo_wa_lines->price_unit.
            endtry.

          when 'NEED-BY-DATE'.
            lo_wa_lines->need_by_date = <fs_info>-cvalue.
          when 'REQUISICAO-SAP'.
            if ( <fs_info>-cvalue is not initial ).
              split <fs_info>-cvalue  at '/' into lo_wa_lines->banfn
                                                  lo_wa_lines->bnfpo.
              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = lo_wa_lines->banfn
                importing
                  output = lo_wa_lines->banfn.
            endif.
          when 'CONDIO-DE-PAGAMENTO'.
            lo_wa_header->codio_pagamento = <fs_info>-cvalue.
          when 'FRETE'.
            lo_wa_header->frete = <fs_info>-cvalue.
          when 'DESCONTO'.
            lo_wa_header->desconto = <fs_info>-cvalue.
          when 'JUSTIFICATIVA-PARA-COMPRA-EMERGENCIAL'.
*            lo_wa_header->justificativa = <fs_info>-cvalue.
            lv_con_lin = lv_con_lin && <fs_info>-cvalue.
            lo_wa_header->justificativa = lv_con_lin.
*            DATA: lv_string type string.
*            lv_string = <fs_info>-cvalue.
*            lo_wa_header->justificativa = me->formata_txt( lv_string ).
*          WHEN 'FORNECEDOR-DETERMINADO'.
*            CLEAR lv_con_lin.
*            lv_con_lin = lv_con_lin && <fs_info>-cvalue.
*            CONDENSE lv_con_lin NO-GAPS.
*            lo_wa_header->pedido = lv_con_lin.
          when 'SOLICITADO-POR' . "'LOGIN'.
            lo_wa_header->login = <fs_info>-cvalue.
          when 'AMOUNT'.
            if ( l_line_tag eq 'ACCOUNT-ALLOCATION'
              and lo_wa_account_allocation is bound ).
              lo_wa_account_allocation->amount = <fs_info>-cvalue.
            endif.
          when 'PCT'.
            if ( l_line_tag eq 'ACCOUNT-ALLOCATION'
              and lo_wa_account_allocation is bound ).
              lo_wa_account_allocation->pct = <fs_info>-cvalue.
            endif.

**********************************************************************
* 108615 CS2023000262 Retomar Check data Emissão Pedido vs. Nota c/ Aperfeiçoamento - PSA
**********************************************************************
          when 'CONTRACT'.
            read table lt_xml_info into data(lt_xml_info_contract) index l_tabix + 1. "Lê a proxima linha onde esta o valor do id doc contrato
            translate lt_xml_info_contract-cname to upper case.
            if lt_xml_info_contract-cname = 'ID'.
              lo_wa_header->contrato = 'CONTRATO'.
            endif.

          when 'PERCENTUAL-REID'.
            l_line_tag = 'PERCENTUAL-REID'.
            lo_wa_lines->percentual_reid = <fs_info>-cvalue.
            lo_wa_lines->reidi = 'S'.
            lo_wa_header->percentual_reid = <fs_info>-cvalue.
            lo_wa_header->reidi = 'S'.

          when 'CUSTOM-FIELDS'.
*            IF l_line_tag EQ 'PERCENTUAL-REID' AND lo_wa_lines->percentual_reid IS NOT INITIAL.
*              lo_wa_lines->percentual_reid = <fs_info>-cvalue.
*              lo_wa_lines->REIDI = 'S'.
*            ENDIF.

*          WHEN 'EXTERNAL-REF-NUM'.
*            IF l_line_tag EQ 'PERCENTUAL-REIDI' AND lo_wa_lines->percentual_reid IS NOT INITIAL.
*              lo_wa_lines->percentual_reid = <fs_info>-cvalue.
*              lo_wa_lines->REIDI = 'S'.
*            ENDIF.
        endcase.
      endloop.

    endif.

    data(lt_xml_header_aux) = lt_xml_header.
    sort lt_xml_header_aux by vendor.
    delete adjacent duplicates from lt_xml_header_aux comparing vendor.
    delete lt_xml_header_aux where vendor is initial.

    if ( lt_xml_header_aux is not initial ).
      select lifnr, name1
        appending table @data(lt_lfa1_name)
        from lfa1
        for all entries in @lt_xml_header_aux
        where lifnr eq @lt_xml_header_aux-vendor.
    endif.

    lt_xml_header_aux = lt_xml_header.
    sort lt_xml_header_aux by po_number.
    delete adjacent duplicates from lt_xml_header_aux comparing po_number.
    delete lt_xml_header_aux where po_number is initial.

    if ( lt_xml_header_aux is not initial ).
      select ebeln
        appending table @data(lt_ekko)
        from ekko
        for all entries in @lt_xml_header_aux
        where ebeln eq @lt_xml_header_aux-po_number.
    endif.

    data(lt_xml_lines_aux) = lt_xml_lines.
    sort lt_xml_lines_aux by plant.
    delete adjacent duplicates from lt_xml_lines_aux comparing plant.
    delete lt_xml_lines_aux where plant is initial.

    if ( lt_xml_lines_aux is not initial ).
      select werks, name1
        appending table @data(lt_t001w)
        from t001w
        for all entries in @lt_xml_lines_aux
        where werks eq @lt_xml_lines_aux-plant.
    endif.

    sort: lt_cnpj by cnpj_xml.
*    SORT: lt_lfa1 BY stcd1.
    sort: lt_lfa1_name by lifnr.
    sort: lt_t001w by werks.
    sort: lt_ekko by ebeln.
    sort: lt_xml_lines by id po_number.

    loop at lt_xml_header reference into lo_wa_header.
      lo_wa_header->bsart = me->determina_tipo_po( iw_xml_header = lo_wa_header->* ).

      read table lt_lfa1_name reference into data(lo_wa_lfa1_name) with key lifnr = lo_wa_header->vendor binary search.
      if ( sy-subrc is initial ).
        lo_wa_header->name   = lo_wa_lfa1_name->name1.
      endif.
*      ENDIF.

      read table lt_ekko transporting no fields with key ebeln = lo_wa_header->po_number binary search.
      if ( sy-subrc is initial ).
        lo_wa_header->pedido_sap = '@01@'.
      else.
        lo_wa_header->pedido_sap = '@02@'.
      endif.

      lo_wa_header->icon = '@Q3@'.

      read table lt_xml_lines reference into lo_wa_lines with key id  = lo_wa_header->id
                                                                  po_number = lo_wa_header->po_number binary search.
      if ( sy-subrc is initial ).
        read table lt_t001w reference into data(lo_wa_t001w) with key werks = lo_wa_lines->plant binary search.
        if ( sy-subrc is initial ).
          lo_wa_header->werks   = lo_wa_t001w->werks.
          lo_wa_header->name1   = lo_wa_t001w->name1.
        endif.

        loop at lt_xml_lines reference into lo_wa_lines where id  = lo_wa_header->id
                                                          and po_number = lo_wa_header->po_number.
          lo_wa_header->preco = lo_wa_header->preco + ( lo_wa_lines->quantity * lo_wa_lines->price  ).
          "
          "Arredondar rateio
          if ( lo_wa_lines->account_allocations is not initial ).
            loop at lo_wa_lines->account_allocations reference into data(wa_account_allocation).
              at first.
                clear: vperc, vcount.
              endat.
              add 1 to vcount.
              vajuste = wa_account_allocation->pct.
              if vajuste lt 100.
                vvproz = wa_account_allocation->pct.
                wa_account_allocation->pct = vvproz.
                add  vvproz to vperc.
              else.
                vperc = 100.
                lo_wa_header->justificativa = 'ERRORATEIO'.
                exit.
              endif.
            endloop.
            if vcount gt 99.
              lo_wa_header->justificativa = 'ERRO99'.
            endif.
            if vperc ne 100.
              vajuste = 100 - vperc.
              vok = 'N'.
              loop at lo_wa_lines->account_allocations reference into wa_account_allocation.
                if vajuste lt 0.
                  if abs( vajuste ) ge wa_account_allocation->pct.
                    continue.
                  endif.
                endif.
                vok = 'S'.
                wa_account_allocation->pct = wa_account_allocation->pct + vajuste.
                exit.
              endloop.
              if vok = 'N'.
                loop at lo_wa_lines->account_allocations reference into wa_account_allocation.
                  if vajuste lt 0.
                    if abs( vajuste ) gt wa_account_allocation->pct.
                      vajuste = ( wa_account_allocation->pct + vajuste ) - '0.1'.
                      wa_account_allocation->pct = '0.1'.
                    else.
                      wa_account_allocation->pct = wa_account_allocation->pct + vajuste.
                      exit.
                    endif.
                  endif.
                endloop.
              endif.
            endif.
          endif.
          "Arredondar rateio
        endloop.
      endif.

    endloop.

    et_xml_header = lt_xml_header.
    et_xml_lines  = lt_xml_lines.

    append lines of lt_xml_header to t_xml_header.
    append lines of lt_xml_lines to t_xml_lines.
    delete et_xml_header where status = 'cancelled'
                            or status = 'closed'.

  endmethod.


  METHOD seleciona_servico_requisicao.

    DATA(lt_requisicao) = t_xml_lines.
    SORT lt_requisicao BY banfn.
    DELETE ADJACENT DUPLICATES FROM lt_requisicao COMPARING banfn.
    DELETE lt_requisicao WHERE banfn IS INITIAL.

    IF ( lt_requisicao IS NOT INITIAL ).

      SELECT *
        INTO TABLE @t_eban
        FROM eban
        FOR ALL ENTRIES IN @lt_requisicao
        WHERE banfn EQ @lt_requisicao-banfn
          AND pstyp EQ '9'
          AND packno NE @abap_false.

      IF ( sy-subrc IS INITIAL ).
        SELECT *
          INTO TABLE @t_esll
          FROM esll
          FOR ALL ENTRIES IN @t_eban
          WHERE packno EQ @t_eban-packno.

        IF ( sy-subrc IS INITIAL ).
          DATA(lt_esll) = t_esll.
          SORT lt_esll BY sub_packno.
          DELETE ADJACENT DUPLICATES FROM lt_esll COMPARING sub_packno.
          DELETE lt_esll WHERE sub_packno IS INITIAL.

          IF ( lt_esll IS NOT INITIAL ).
            SELECT *
              APPENDING TABLE @t_esll
              FROM esll
              FOR ALL ENTRIES IN @lt_esll
              WHERE packno EQ @lt_esll-sub_packno.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    SORT t_eban BY banfn bnfpo.
    SORT t_esll BY packno.

  ENDMETHOD.


  method SET_FIELDS_X.

    STATICS: lt_components TYPE abap_component_tab.

    IF ( iv_reuse_components = abap_false )
    OR ( iv_reuse_components = abap_true
    AND  lt_components[]     IS INITIAL ).
      lt_components = get_structdescr( cw_itemx ).
    ENDIF.

    LOOP AT lt_components[] REFERENCE INTO DATA(lw_componentes).

      CASE lw_componentes->name.
        WHEN 'PREQ_ITEMX' OR 'PO_ITEMX' OR 'UPDATEFLAG'.
          ASSIGN COMPONENT lw_componentes->name OF STRUCTURE cw_itemx TO FIELD-SYMBOL(<lfs_pritemx>).
          IF ( <lfs_pritemx> IS ASSIGNED ).
            <lfs_pritemx> = abap_true.
          ENDIF.

          IF ( lw_componentes->name EQ 'UPDATEFLAG' ).
            <lfs_pritemx> = 'I'.
          ENDIF.

          CONTINUE.

        WHEN 'PREQ_ITEM' OR 'PO_ITEM' OR 'SCHED_LINE' OR 'ITM_NUMBER' OR 'ITEM_NO' OR 'SERIAL_NO'.
          DATA(lv_valor) = abap_true.
        WHEN OTHERS.
          lv_valor = abap_false.

      ENDCASE.

      ASSIGN COMPONENT lw_componentes->name OF STRUCTURE iw_item TO FIELD-SYMBOL(<lfs_pritem>).
      IF ( <lfs_pritem> IS ASSIGNED
      AND  <lfs_pritem> IS NOT INITIAL ).

        ASSIGN COMPONENT lw_componentes->name OF STRUCTURE cw_itemx TO <lfs_pritemx>.
        IF ( <lfs_pritemx> IS ASSIGNED ).
          IF ( lv_valor IS INITIAL ).
            <lfs_pritemx> = abap_true.
          ELSE.
            <lfs_pritemx> = <lfs_pritem>.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

  endmethod.


  METHOD verifica_servico_requisicao.
    DATA:
*          lv_pckg_no    TYPE bapiesuhc-pckg_no,
*          lv_line_no    TYPE srv_line_no,
          lv_subpckg_no TYPE bapiesuhc-pckg_no.
*          lv_serial_no  TYPE bapimepoaccount-serial_no.

    READ TABLE t_eban REFERENCE INTO DATA(lo_eban) WITH KEY banfn = iv_banfn
                                                            bnfpo = iv_bnfpo BINARY SEARCH.

    IF ( sy-subrc IS INITIAL ).
      READ TABLE t_esll REFERENCE INTO DATA(lo_esll) WITH KEY packno = lo_eban->packno BINARY SEARCH.
      IF ( sy-subrc IS INITIAL ).
        READ TABLE t_esll REFERENCE INTO DATA(lo_esll_sub) WITH KEY packno = lo_esll->sub_packno BINARY SEARCH.
        IF ( sy-subrc IS INITIAL ).
          ADD 10 TO cv_pckgno.
          ADD 1 TO cv_lineno.

          APPEND INITIAL LINE TO et_poservices REFERENCE INTO DATA(lo_wa_poservices).
          lo_wa_poservices->pckg_no  = cv_pckgno.
          lo_wa_poservices->line_no  = cv_lineno.
          lo_wa_poservices->pln_line = cv_lineno.

          "incrementa novamente
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'SERVICE'
            IMPORTING
              number                  = lo_wa_poservices->subpckg_no
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.

          lv_subpckg_no = lo_wa_poservices->subpckg_no.
          APPEND INITIAL LINE TO et_poservices REFERENCE INTO lo_wa_poservices.
          ADD 1 TO cv_lineno.
          lo_wa_poservices->pckg_no   = lv_subpckg_no.
          lo_wa_poservices->line_no   = cv_lineno.
          lo_wa_poservices->service   = lo_esll_sub->srvpos.

          IF iv_banfn IS INITIAL.
            lo_wa_poservices->quantity  = lo_esll_sub->menge.
            lo_wa_poservices->gr_price  = lo_esll_sub->brtwr.
          ELSE.
            IF lo_wa_poservices->service IS NOT INITIAL.
              lo_wa_poservices->quantity  = iv_price.
              lo_wa_poservices->price_unit = iv_unit.
              lo_wa_poservices->gr_price  = 1.
            ENDIF.
          ENDIF.


          APPEND INITIAL LINE TO et_polimits REFERENCE INTO DATA(lo_wa_polimits).
          lo_wa_polimits->pckg_no   = lv_subpckg_no.
          lo_wa_polimits->no_limit  = abap_true.
          lo_wa_polimits->exp_value = lo_esll_sub->brtwr.

*          IF ( lo_wa_poaccount IS BOUND ).
          ADD 1 TO cv_serialno.
*            lo_wa_poaccount->serial_no = lv_serial_no.
*          ENDIF.

          APPEND INITIAL LINE TO et_posrvaccessvalues REFERENCE INTO DATA(lo_wa_values).
          lo_wa_values->pckg_no    = lv_subpckg_no.
          lo_wa_values->line_no    = cv_lineno.
          lo_wa_values->serno_line = '01'.
          lo_wa_values->percentage = 100.
          lo_wa_values->serial_no  = cv_serialno.
          lo_wa_values->quantity   = iv_price.
          lo_wa_values->net_value  = 1.
*          lo_wa_values->quantity   = lo_esll_sub->menge.
*          lo_wa_values->net_value  = lo_esll_sub->brtwr.

          rv_servico = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  method ZIF_INTEGRACAO_COUPA_PED_COMP~GET_INSTANCE.
  endmethod.


  method ZIF_INTEGRACAO_COUPA_PED_COMP~GET_XML.
  endmethod.


  method ZIF_INTEGRACAO_COUPA_PED_COMP~SET_DS_DATA.
  endmethod.


  METHOD zif_integracao_coupa_ped_comp~set_ds_url.

    DATA: lv_token    TYPE string,
          lv_http_url TYPE string,
          lv_index    type i.

    SELECT  SINGLE *
      FROM zauth_webservice
        INTO @DATA(l_webservise)
          WHERE service = @zif_integracao_coupa_ped_comp~at_servico.

    CHECK sy-subrc IS INITIAL.

    IF e_metodo = 'PUT'.

      CASE e_integracao.
        WHEN 'EXPORTED'. " Atualizar Campo Exported do Pedidos de Compra
          lv_http_url = l_webservise-url && e_id_po && |?| && |exported=true&fields=["id"]|.
          clear me->zif_integracao_inject~at_info_request_http-ds_body.
        WHEN 'NO_EXPORTED'. " Atualizar Campo Exported do Pedidos de Compra
          lv_http_url = l_webservise-url && e_id_po && |?| && |exported=false&fields=["id"]|.
        WHEN 'ISSUED'.   " Atualizar Status dos Pedidos de Compra para Issued
          lv_http_url = l_webservise-url && e_id_po && |/issue?fields=["id"]|.
*-CS1117561-#RIMINI-14.07.2023-BEGIN
        WHEN 'ISSUE_WITHOUT_SEND'.   " Atualizar Status dos Pedidos de Compra para Issue Without Send
          lv_http_url = l_webservise-url && e_id_po && |/issue_without_send?fields=["id"]|.
*-CS1117561-#RIMINI-14.07.2023-END
        WHEN 'PEDIDO'.   " Atualizar Campos do Pedido de Compra
          lv_http_url = l_webservise-url && e_id_po && |/?fields=["id"]|.

          me->zif_integracao_inject~at_info_request_http-ds_body = '<?xml version="1.0" encoding="utf-8"?> <order-header> <custom-fields>'.
          LOOP AT it_filter REFERENCE INTO DATA(lo_wa_filter).
            CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_body '<' lo_wa_filter->field '>'
            lo_wa_filter->value '</' lo_wa_filter->field '>'
            INTO me->zif_integracao_inject~at_info_request_http-ds_body.
          ENDLOOP.
          CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_body '</custom-fields> </order-header>'
           INTO me->zif_integracao_inject~at_info_request_http-ds_body.
        WHEN 'PEDIDO_IVA'.   " Atualizar Campos do Pedido de Compra
          lv_http_url = l_webservise-url && e_id_po && |/?fields=["id"]|.
          "
          DESCRIBE TABLE it_filter LINES DATA(v_linhas).
          lv_index = 0.
          me->zif_integracao_inject~at_info_request_http-ds_body = '<?xml version="1.0" encoding="utf-8"?> <order-header> <transmission-method-override>do_not_transmit</transmission-method-override> <order-lines>'.
          DO v_linhas TIMES.
            add 1 to lv_index.
            READ TABLE it_filter REFERENCE INTO lo_wa_filter INDEX lv_index.
            DATA(num) = lv_index MOD 2.
            IF num = 1.
              CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_body '<order-line>'
                     '<' lo_wa_filter->field '>'
                     lo_wa_filter->value '</' lo_wa_filter->field '>'
                     INTO me->zif_integracao_inject~at_info_request_http-ds_body.
            ELSE.
              CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_body '<custom-fields> <iva>'
                     INTO me->zif_integracao_inject~at_info_request_http-ds_body  .

              CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_body '<' lo_wa_filter->field '>'
               lo_wa_filter->value '</' lo_wa_filter->field '>'
              INTO me->zif_integracao_inject~at_info_request_http-ds_body.

              CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_body '</iva></custom-fields> </order-line>'
               INTO me->zif_integracao_inject~at_info_request_http-ds_body.
            ENDIF.
          ENDDO.
          CONCATENATE me->zif_integracao_inject~at_info_request_http-ds_body '</order-lines> </order-header>'
          INTO me->zif_integracao_inject~at_info_request_http-ds_body.
        WHEN OTHERS.
      ENDCASE.

    ELSEIF e_metodo = 'GET'.
      lv_http_url = l_webservise-url.
      CONDENSE lv_http_url NO-GAPS.
      LOOP AT it_filter REFERENCE INTO lo_wa_filter.
        lv_http_url = lv_http_url && |&| && lo_wa_filter->field && |=| && lo_wa_filter->value.
      ENDLOOP.
      CONDENSE lv_http_url NO-GAPS.
    ENDIF.

    SELECT  SINGLE add01
      FROM zauth_webservice
        INTO lv_token
          WHERE service = 'COUPA_TOKEN'.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml;charset=UTF-8'.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = lv_token.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = e_metodo.
    me->zif_integracao_inject~at_info_request_http-ds_url = lv_http_url.
    me->zif_integracao_inject~at_info_request_http-ds_url = lv_http_url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_coupa_ped_comp~set_id_referencia( ).
  ENDMETHOD.


  method ZIF_INTEGRACAO_COUPA_PED_COMP~SET_ID_REFERENCIA.
  endmethod.


  METHOD zif_integracao_coupa_ped_comp~set_send_msg.

*    DATA lc_integrar TYPE REF TO zcl_integracao.

    TRY .


    r_if_integracao_ped_comp_coupa = me.

    CREATE OBJECT lo_integracao.

    "Cria MSG para Integração via HTTP
    lo_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao ).
*      )->free(
*      ).

*    CLEAR: lc_integrar.

    CATCH ZCX_ERROR.

    ENDTRY.
  ENDMETHOD.


  METHOD zif_integracao_coupa_ped_comp~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_integracao_coupa_ped_comp~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

*    r_if_integracao_inject = me.
*
*    DATA lt_header_fields  TYPE zde_header_field_t.
*
*    APPEND INITIAL LINE TO lt_header_fields ASSIGNING FIELD-SYMBOL(<fs_field>).
*    <fs_field>-name = 'x-coupa-api-key'.
*    <fs_field>-value = me->zif_integracao_inject~at_info_request_http-ds_url_token.
*
*    APPEND INITIAL LINE TO lt_header_fields ASSIGNING <fs_field>.
*    <fs_field>-name = 'Accept'.
*    <fs_field>-value = 'application/xml'.
*
*    me->zif_integracao_inject~set_header_request_http( i_header_fields = lt_header_fields ).


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

*    DATA: ordem_servico TYPE retorno.


*    "/UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ordem_servico-os_id IS NOT INITIAL.

*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    ZCL_INTEGRACAO_USER_COUPA=>ZIF_INTEGRACAO_USER_COUPA~GET_INSTANCE(
*      )->SET_CONFIRMAR( EXPORTING
*        "I_USUARIO = ME->ZIF_INTEGRACAO_USER_COUPA~AT_USUARIO
*
*      ).
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    r_if_integracao_inject = me.

*    DATA: ORDEM_SERVICO TYPE ZDE_KUHLMANN_HVI_RETORNO.
*
*    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
*    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ORDEM_SERVICO-OS_ID IS NOT INITIAL.
*
*    DATA: HVI    TYPE ZPP_KUHLMANN_HVI,
*          IT_HVI TYPE TABLE OF ZPP_KUHLMANN_HVI.
*
*    CLEAR: IT_HVI[].
*
*    LOOP AT ORDEM_SERVICO-FARDOS INTO DATA(WA_FARDO).
*      CLEAR: HVI.
*      HVI-FARDO  = WA_FARDO-FARDO.
*      HVI-OS_ID  = ORDEM_SERVICO-OS_ID.
*      HVI-OS_NR  = ORDEM_SERVICO-NUMERO_OS.
*      HVI-ROMANEIO = ORDEM_SERVICO-ROMANEIO.
*      HVI-LABORATORIO = ORDEM_SERVICO-LABORATORIO.
*
*      DATA(DT_ENTRADA) = ORDEM_SERVICO-DATA_ENTRADA(10).
*      REPLACE ALL OCCURRENCES OF '-' IN DT_ENTRADA WITH ''.
*      HVI-DATA_ENTRADA = DT_ENTRADA.
*
*      DATA(DT_ANALISE) = ORDEM_SERVICO-DATA_ANALISE(10).
*      REPLACE ALL OCCURRENCES OF '-' IN DT_ANALISE WITH ''.
*      HVI-DATA_ANALISE = DT_ANALISE.
*
*      HVI-HVI_MIC      = WA_FARDO-MIC.
*      HVI-HVI_POL      = WA_FARDO-POL.
*      HVI-HVI_LEN      = WA_FARDO-LEN.
*      HVI-HVI_STR      = WA_FARDO-STR.
*      HVI-HVI_UNF      = WA_FARDO-UNF.
*      HVI-HVI_ELG      = WA_FARDO-ELG.
*      HVI-HVI_MAT      = WA_FARDO-MAT.
*      HVI-HVI_RD       = WA_FARDO-RD.
*      HVI-HVI_B        = WA_FARDO-B.
*      HVI-HVI_CG       = WA_FARDO-CG.
*      HVI-HVI_LEAF     = WA_FARDO-LEAF.
*      HVI-HVI_ARE      = WA_FARDO-ARE.
*      HVI-HVI_COUNT    = WA_FARDO-COUNT.
*      HVI-HVI_SFI      = WA_FARDO-SFI.
*      HVI-HVI_CSP      = WA_FARDO-CSP.
*      HVI-HVI_SCI      = WA_FARDO-SCI.
*      HVI-HVI_MAT_PORC = WA_FARDO-MAT_PORC.
*      HVI-HVI_POL_FRAC = WA_FARDO-POL_FRAC.
*      HVI-HVI_LOTE     = WA_FARDO-LOTE.
*      HVI-HVI_PADRAO   = WA_FARDO-PADRAO.
*      APPEND HVI TO IT_HVI.
*    ENDLOOP.
*
*    IF IT_HVI[] IS NOT INITIAL.
*      MODIFY ZPP_KUHLMANN_HVI FROM TABLE IT_HVI.
*      COMMIT WORK AND WAIT.
*    ENDIF.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ATUALIZA_EXPORTED_PEDIDO_IVA.

    IF ( o_integracao_coupa_ped_comp IS NOT BOUND ).
      CREATE OBJECT o_integracao_coupa_ped_comp
        EXPORTING
          i_servico = 'COUPA_INT_STATUS_PED_COMPRA'.
    ENDIF.

    o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_ds_url(
                                                          e_metodo     = 'PUT'
                                                          e_integracao = 'NO_EXPORTED'
                                                          e_id_po      = iw_xml_header-id ).

    o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_send_msg(
                    IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                              e_integracao    = DATA(e_integracao_post) ).

  ENDMETHOD.


  METHOD atualiza_iva_pedido_coupa.
    DATA: lv_string     TYPE string.
    DATA: lt_filter     TYPE zif_integracao_coupa_ped_comp=>tt_filter.
    REFRESH lt_filter.

    LOOP AT it_xml_lines REFERENCE INTO  DATA(wa_xml_lines).
      lv_string = wa_xml_lines->id_line.
      APPEND INITIAL LINE TO lt_filter REFERENCE INTO DATA(lo_wa_filter).
      lo_wa_filter->value = lv_string.
      lo_wa_filter->field = 'id'.

      lv_string = wa_xml_lines->tax_code.
      APPEND INITIAL LINE TO lt_filter REFERENCE  INTO lo_wa_filter.
      lo_wa_filter->value = lv_string.
      lo_wa_filter->field = 'external-ref-num'.


    ENDLOOP.
    IF ( o_integracao_coupa_ped_comp IS NOT BOUND ).
      CREATE OBJECT o_integracao_coupa_ped_comp
        EXPORTING
          i_servico = 'COUPA_INT_STATUS_PED_COMPRA'.
    ENDIF.

    o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_ds_url(
                                                      e_metodo     = 'PUT'
                                                      e_integracao = 'PEDIDO_IVA'
                                                      e_id_po      = wa_xml_lines->id
                                                      it_filter    = lt_filter ).

    o_integracao_coupa_ped_comp->zif_integracao_coupa_ped_comp~set_send_msg(
                    IMPORTING e_id_integracao = DATA(e_id_integracao_post)
                              e_integracao    = DATA(e_integracao_post) ).

  ENDMETHOD.


  method FORMATA_TXT.

    CHECK iv_in IS NOT INITIAL.

    rv_out = iv_in.

    REPLACE ALL OCCURRENCES OF REGEX `[\t\v\n\r]` IN rv_out WITH ';'.
    REPLACE ALL OCCURRENCES OF ',' IN rv_out WITH '#'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_out WITH space.

    WHILE rv_out CP '*;;*'.
      REPLACE ALL OCCURRENCES OF ';;' IN rv_out WITH ';'.

      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

    ENDWHILE.

    WHILE rv_out CP '*##*'.

      REPLACE ALL OCCURRENCES OF '##' IN rv_out WITH '#'.


      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

    ENDWHILE.

    DATA(lv_tam) = strlen( rv_out ).

    lv_tam = lv_tam - 1.

    CHECK lv_tam > 0.

    IF rv_out+lv_tam(1) NE ';'.

      rv_out = rv_out && ';'.

    ENDIF.

  endmethod.
ENDCLASS.
