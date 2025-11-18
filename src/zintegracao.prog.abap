*&---------------------------------------------------------------------*
*& Report  ZINTEGRACAO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zintegracao MESSAGE-ID zintegra.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar2 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler2 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

TYPES: BEGIN OF ty_integracao_proc,
         id_integracao TYPE zintegracao-id_integracao,
         dt_registro   TYPE zintegracao-dt_registro,
         hr_registro   TYPE zintegracao-hr_registro,
       END OF ty_integracao_proc.

*alteradro por Guilherme Rabelo inicio 20.06.2023
TYPES:BEGIN OF y_interface,
        id TYPE zintegracao-id_interface,
      END OF y_interface.

*alteradro por Guilherme Rabelo  20.06.2023

DATA: ok_code             TYPE sy-ucomm,
      dg_splitter         TYPE REF TO cl_gui_splitter_container,
      dg_splitter2        TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer2    TYPE REF TO cl_gui_container,
      ctl_cccontainer3    TYPE REF TO cl_gui_container,
      ctl_cccontainer4    TYPE REF TO cl_gui_container,
      ctl_cccontainer5    TYPE REF TO cl_gui_container,
      ctl_alv             TYPE REF TO cl_gui_alv_grid,
      ctl_alv2            TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog     TYPE lvc_t_fcat,
      it_fieldcatalog2    TYPE lvc_t_fcat,
      gs_variant          TYPE disvariant,
      gs_variant2         TYPE disvariant,
      gs_layout           TYPE lvc_s_layo,
      gs_layout2          TYPE lvc_s_layo,
      obg_toolbar         TYPE REF TO lcl_alv_toolbar,
      obj_toolbarmanager  TYPE REF TO cl_alv_grid_toolbar_manager,
      obg_toolbar2        TYPE REF TO lcl_alv_toolbar2,
      obj_toolbarmanager2 TYPE REF TO cl_alv_grid_toolbar_manager,
      event_handler       TYPE REF TO lcl_event_handler,
      event_handler2      TYPE REF TO lcl_event_handler2.

DATA: gs_scroll_col    TYPE lvc_s_col,
      gs_scroll_row    TYPE lvc_s_row,
      gs_scroll_row_id TYPE lvc_s_roid.

DATA: gs_scroll_col2    TYPE lvc_s_col,
      gs_scroll_row2    TYPE lvc_s_row,
      gs_scroll_row_id2 TYPE lvc_s_roid.

DATA: lc_id_selecionado TYPE zde_id_integracao,
      it_mensagens      TYPE TABLE OF zintegracao_alv,
      wa_mensagens      TYPE zintegracao_alv,
      it_mensagens_sel  TYPE TABLE OF zintegracao_alv,
      it_mensagens_log  TYPE TABLE OF zintegracao_log_alv,
      git_list_integra  TYPE TABLE OF ty_integracao_proc,
      wa_mensagens_log  TYPE zintegracao_log_alv,
      lc_data_view      TYPE string,
      lc_open_browser   TYPE char01.

CONSTANTS: c_processar TYPE char50 VALUE 'PROCESSAR',
           c_integrar  TYPE char50 VALUE 'INTEGRAR',
           c_enviar    TYPE char50 VALUE 'ENVIAR'.

CONSTANTS: c_01(02) TYPE c VALUE '01',
           c_02(02) TYPE c VALUE '02',
           c_03(02) TYPE c VALUE '03',
           c_04(02) TYPE c VALUE '04',
           c_05(02) TYPE c VALUE '05'.

CLASS lcl_alv_toolbar IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar TYPE stb_button.

*    "Separador
    CLEAR ty_toolbar.
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_execute_object.
    ty_toolbar-function  = 'CONSULTAR'.
    ty_toolbar-quickinfo = TEXT-004.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

*    "Separador
    CLEAR ty_toolbar.
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_refresh.
    ty_toolbar-function  = 'ATUALIZAR'.
    "TY_TOOLBAR-TEXT      = TEXT-001.
    ty_toolbar-quickinfo = TEXT-001.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

*    "Separador
    CLEAR ty_toolbar.
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.

*    "Marcar Todos os Documentos
    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_outbox.
    ty_toolbar-function  = 'ENVIAR'.
    ty_toolbar-quickinfo = TEXT-101.
    ty_toolbar-text      = TEXT-101.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_operation.
    ty_toolbar-function  = 'PROCESSAR'.
    ty_toolbar-quickinfo = TEXT-102.
    ty_toolbar-text      = TEXT-102.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_wf_link.
    ty_toolbar-function  = 'INTEGRAR'.
    ty_toolbar-quickinfo = TEXT-103.
    ty_toolbar-text      = TEXT-103.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

*    "Separador
    CLEAR ty_toolbar.
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.

*    "Marcar Todos os Documentos
    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_outbox.
    ty_toolbar-function  = 'ENVIAR_FORCE'.
    ty_toolbar-quickinfo = TEXT-104.
    ty_toolbar-text      = TEXT-104.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CALL METHOD obj_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_mensagens_sel, it_mensagens_sel[].

    IF e_ucomm NE 'ATUALIZAR' AND e_ucomm NE 'CONSULTAR'.
      ctl_alv->get_selected_rows( IMPORTING et_index_rows = DATA(et_index_rows) ).
      IF et_index_rows[] IS NOT INITIAL.
        LOOP AT et_index_rows INTO DATA(wa_index_rows).
          READ TABLE it_mensagens INTO DATA(wa_mensagem) INDEX wa_index_rows-index.
          APPEND wa_mensagem TO it_mensagens_sel.
        ENDLOOP.
      ENDIF.
      CHECK it_mensagens_sel[] IS NOT INITIAL.
    ENDIF.



*alterado por Guilherme Rabelo inicio 20.06.2023
    DATA: lv_user_aux  TYPE usr02-bname,
          ls_vla       TYPE ust12-von,
          lv_text(255) TYPE c.

    lv_user_aux = sy-uname.
    CASE e_ucomm.

      WHEN 'CONSULTAR'.
        ls_vla = c_01.
      WHEN 'ATUALIZAR'.
        ls_vla = c_01.
      WHEN 'ENVIAR_FORCE'.
        ls_vla = c_05.

      WHEN 'ENVIAR'.
        ls_vla = c_02.

      WHEN 'PROCESSAR'.
        ls_vla = c_03.

      WHEN 'INTEGRAR'.
        ls_vla = c_04.

    ENDCASE.

    CALL FUNCTION 'AUTHORITY_CHECK'
      EXPORTING
        user                = lv_user_aux
        object              = 'ZWS0004'
        field1              = 'ZDM_ID_ACA'
        value1              = ls_vla
      EXCEPTIONS
        user_dont_exist     = 1
        user_is_authorized  = 2
        user_not_authorized = 3
        user_is_locked      = 4
        OTHERS              = 5.

    IF sy-subrc = 2 OR ( sy-tcode NE 'ZWS0004' AND ls_vla NE c_05 ).

*fim da alteração

      CASE e_ucomm.
        WHEN 'CONSULTAR'.
          PERFORM selecao_mensagens.
        WHEN 'ATUALIZAR'.
          PERFORM atualizar_registros.
        WHEN 'ENVIAR_FORCE'.
          PERFORM enviar_mensagens USING it_mensagens_sel abap_true.
          PERFORM atualizar_registros.
        WHEN 'ENVIAR'.
          PERFORM enviar_mensagens USING it_mensagens_sel abap_false.
          PERFORM atualizar_registros.
        WHEN 'PROCESSAR'.
          PERFORM processar_mensagens USING it_mensagens_sel.
          PERFORM atualizar_registros.
        WHEN 'INTEGRAR'.
          PERFORM integrar_mensagens USING it_mensagens_sel.
          PERFORM atualizar_registros.
      ENDCASE.

    ELSE.

      MESSAGE i000 WITH 'Usuário sem autorização para essa ação' DISPLAY LIKE 'I'.

    ENDIF.

    ctl_alv->refresh_table_display( i_soft_refresh = abap_true ).

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

CLASS lcl_alv_toolbar2 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager2
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    CLEAR ty_toolbar.
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_close.
    ty_toolbar-function  = 'FECHAR'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CALL METHOD obj_toolbarmanager2->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'FECHAR'.
        CLEAR: it_mensagens_log[], lc_data_view.
        LEAVE TO SCREEN 0100.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler2 IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click2 USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_double_click.
    PERFORM handle_double_click2 USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

"Informações de Status de Documentos
SELECTION-SCREEN BEGIN OF BLOCK int01 WITH FRAME TITLE TEXT-901.
  SELECT-OPTIONS: sidint FOR wa_mensagens-id_integracao,
                  sidinf FOR wa_mensagens-id_interface,
                  sidref FOR wa_mensagens-id_referencia,
                  sdtreg FOR wa_mensagens-dt_registro DEFAULT sy-datum,
                  shrreg FOR wa_mensagens-hr_registro,
                  susreg FOR wa_mensagens-us_registro.
SELECTION-SCREEN END OF BLOCK int01 .

SELECTION-SCREEN BEGIN OF BLOCK int02 WITH FRAME TITLE TEXT-902.
  PARAMETERS: sckret TYPE zde_ck_retornou   AS CHECKBOX,
              sckpro TYPE zde_ck_processado AS CHECKBOX,
              sckint TYPE zde_ck_integrado  AS CHECKBOX,
              sckall TYPE zde_ck_integrado  AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK int02 .

SELECT-OPTIONS: stpref FOR wa_mensagens-tp_referencia NO-DISPLAY.
SELECT-OPTIONS: idinte FOR wa_mensagens-id_integracao NO-DISPLAY.

INITIALIZATION.

  IF sy-batch EQ abap_true.
    SELECT SINGLE *
      FROM setleaf INTO @DATA(lwa_conf_0004)
     WHERE setname = 'ZINTEGRACAO_CONF_0004'
       AND valfrom = @sy-uname.

    IF sy-subrc EQ 0.
      DO 100 TIMES.
        PERFORM f_clear_log USING '2'.
        WAIT UP TO 2 SECONDS.
      ENDDO.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  IF sy-batch EQ abap_true.

    DATA: lva_job                TYPE i,
          lva_jobnm              TYPE btcjob,
          lva_stepc              TYPE btcstepcnt,
          lc_data_delete         TYPE sy-datum,
          lva_interfaces_del_001 TYPE string,
          lva_interfaces_del_002 TYPE string.

    CONSTANTS: c_zintegracao_enviar    TYPE c LENGTH 50 VALUE 'ZINTEGRACAO_ENVIAR',
               c_zintegracao_processar TYPE c LENGTH 50 VALUE 'ZINTEGRACAO_PROCESSAR',
               c_zintegracao_integrar  TYPE c LENGTH 50 VALUE 'ZINTEGRACAO_INTEGRAR'.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(lwa_conf_0005)
     WHERE setname = 'ZINTEGRACAO_CONF_0005'
       AND valfrom = 'STOP_JOB'.

    IF sy-subrc EQ 0.
      LEAVE PROGRAM.
    ENDIF.

    DATA(lva_proc_paralelo) = abap_false.

    SELECT SINGLE *
      FROM setleaf INTO lwa_conf_0004
     WHERE setname = 'ZINTEGRACAO_CONF_0004'
       AND valfrom = 'PROC_PARALELO'.

    IF sy-subrc EQ 0.
      lva_proc_paralelo = abap_true.
    ENDIF.

    CASE lva_proc_paralelo.
      WHEN abap_true.

        CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
          IMPORTING
            jobname         = lva_jobnm
            stepcount       = lva_stepc
          EXCEPTIONS
            no_runtime_info = 1
            OTHERS          = 2.

        CASE lva_jobnm.
          WHEN c_zintegracao_enviar.

            SELECT SINGLE COUNT(*) INTO lva_job
              FROM tbtco
             WHERE jobname EQ c_zintegracao_enviar
               AND status  EQ 'R'.

          WHEN c_zintegracao_processar.

            SELECT SINGLE COUNT(*) INTO lva_job
              FROM tbtco
             WHERE jobname EQ c_zintegracao_processar
               AND status  EQ 'R'.

          WHEN c_zintegracao_integrar.

            SELECT SINGLE COUNT(*) INTO lva_job
              FROM tbtco
             WHERE jobname EQ c_zintegracao_integrar
               AND status  EQ 'R'.

          WHEN OTHERS.
            LEAVE PROGRAM.
        ENDCASE.

        IF lva_job NE 1.
          LEAVE PROGRAM.
        ENDIF.

      WHEN abap_false.

        TRY.
            zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
          CATCH zcx_job.
        ENDTRY.

        IF e_qtd GT 1.
          LEAVE PROGRAM.
        ENDIF.

    ENDCASE.

    CASE lva_proc_paralelo.
      WHEN abap_true.

        CASE lva_jobnm.
          WHEN c_zintegracao_enviar.

            PERFORM: f_exec_a_enviar.

          WHEN c_zintegracao_processar.

            PERFORM: f_exec_a_processar.

          WHEN c_zintegracao_integrar.

            PERFORM: f_exec_a_integrar.

            PERFORM f_clear_log USING '1'.

          WHEN OTHERS.
            LEAVE PROGRAM.
        ENDCASE.

      WHEN abap_false.

        PERFORM f_clear_log USING '1'.

        PERFORM: f_exec_a_enviar,
                 f_exec_a_processar,
                 f_exec_a_integrar.

    ENDCASE.

    LEAVE PROGRAM.

  ENDIF.

START-OF-SELECTION.

  CLEAR: gs_scroll_col, gs_scroll_row, gs_scroll_col2, gs_scroll_row2, gs_scroll_row_id, gs_scroll_row_id2.
  PERFORM selecao_mensagens.

END-OF-SELECTION.

  CALL SCREEN '0100'.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.


  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  PERFORM cria_splitter.

  PERFORM cria_alv_principal USING ctl_cccontainer2.

  PERFORM cria_alv_log USING ctl_cccontainer4.

  PERFORM cria_html_view USING ctl_cccontainer5.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.

  CLEAR ok_code.
  PERFORM limpa_objetos.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZINTEGRACAO_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  DELETE it_fieldcatalog WHERE fieldname = 'DTHR_REGISTRO'.

  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.
    IF <fs_cat>-fieldname(4) = 'ICO_'.
      <fs_cat>-icon    = abap_true.
      <fs_cat>-just    = 'C'.
    ENDIF.

    IF <fs_cat>-fieldname = 'ICO_ST_MSG'    OR
       <fs_cat>-fieldname = 'ICO_MSG_ENVIO' OR
       <fs_cat>-fieldname = 'ICO_MSG_RETORNO' OR
       <fs_cat>-fieldname = 'ICO_MSG_HEADER'.
      <fs_cat>-hotspot = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

  gs_layout-sel_mode     = 'A'.
  gs_layout-info_fname   = 'LINE_COLOR'.
  gs_layout-stylefname   = 'STYLE'.
  gs_layout-ctab_fname   = 'COLOR_CELL'.
  gs_layout-zebra        = abap_false.
  gs_layout-cwidth_opt   = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECAO_MENSAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecao_mensagens .

  CLEAR: it_mensagens, it_mensagens[].

  RANGES: lc_status_ret FOR wa_mensagens-ck_retornou.
  RANGES: lc_status_pro FOR wa_mensagens-ck_processado.
  RANGES: lc_status_int FOR wa_mensagens-ck_integrado.

  CASE sckall.
    WHEN abap_false.
      lc_status_ret[] = VALUE #( sign = 'I' option = 'EQ' ( low = sckret high = sckret ) ).
      lc_status_pro[] = VALUE #( sign = 'I' option = 'EQ' ( low = sckpro high = sckpro ) ).
      lc_status_int[] = VALUE #( sign = 'I' option = 'EQ' ( low = sckint high = sckint ) ).
  ENDCASE.

  SELECT * INTO TABLE @DATA(it_mensagens_banco)
    FROM zintegracao
   WHERE id_integracao IN @sidint
     AND id_interface  IN @sidinf
     AND id_integracao IN @idinte
     AND id_referencia IN @sidref
     AND dt_registro   IN @sdtreg
     AND hr_registro   IN @shrreg
     AND us_registro   IN @susreg
     AND ck_retornou   IN @lc_status_ret
     AND ck_processado IN @lc_status_pro
     AND ck_integrado  IN @lc_status_int
     AND tp_referencia IN @stpref.


*alterado por Guilherme Rabelo inicio 20.06.2023
  DATA: lv_user      TYPE usr02-bname,
        lt_id        TYPE TABLE OF y_interface,
        ls_id        TYPE y_interface,
        ls_value     TYPE ust12-von,
        lv_text(255) TYPE c,
        lv_msg(255)  TYPE c.


  lv_user = sy-uname.

  LOOP  AT it_mensagens_banco INTO DATA(wa_men).

    IF sy-tcode = 'ZWS0004'.
      ls_value = wa_men-id_interface.

      CALL FUNCTION 'AUTHORITY_CHECK'
        EXPORTING
          user                = lv_user
          object              = 'ZWS0004'
          field1              = 'ZDM_ID_INT'
          value1              = ls_value
        EXCEPTIONS
          user_dont_exist     = 1
          user_is_authorized  = 2
          user_not_authorized = 3
          user_is_locked      = 4
          OTHERS              = 5.


      IF sy-subrc <> 2.

        DELETE it_mensagens_banco WHERE id_interface = wa_men-id_interface.

        ls_id-id = wa_men-id_interface.
        APPEND ls_id TO lt_id.
        CLEAR ls_id.

      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lt_id COMPARING ALL FIELDS.

  CLEAR:lv_text.

  LOOP AT lt_id INTO ls_id.

    CONCATENATE lv_text ls_id-id ',' INTO lv_text.
  ENDLOOP.

  IF lv_text <> ' '.

    DATA: answer(1) TYPE c.

    CONCATENATE 'Sem acesso aos ID Interface: ' lv_text 'caso necessário procure o suporte.sap' INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'I'.

  ENDIF.

*alterado por Guilherme Rabelo fim 20.06.2023

  LOOP AT it_mensagens_banco INTO DATA(wa_mensagens).
    TRY .
        zcl_integracao=>zif_integracao~get_instance(
          )->get_msg_alv( EXPORTING i_integracao = wa_mensagens IMPORTING e_integracao_alv = DATA(e_integracao_alv)
          ).
        APPEND e_integracao_alv TO it_mensagens.
      CATCH zcx_integracao INTO DATA(ex_erro).
        ex_erro->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

  SORT it_mensagens BY dthr_registro DESCENDING.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_REGISTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualizar_registros .

  CHECK it_mensagens[] IS NOT INITIAL.

  LOOP AT it_mensagens ASSIGNING FIELD-SYMBOL(<fs_msg>).
    TRY .

        zcl_integracao=>zif_integracao~get_instance(
          )->set_registro( i_id_integracao = <fs_msg>-id_integracao
          )->get_msg_alv( IMPORTING e_integracao_alv = <fs_msg>
          )->free(
          ).

      CATCH zcx_integracao INTO DATA(ex_erro).    " .
        ex_erro->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO DATA(ex_erro2).
        ex_erro2->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_MENSAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MENSAGENS_SEL  text
*----------------------------------------------------------------------*
FORM enviar_mensagens  USING p_sel TYPE zintegracao_alv_t  cx_force TYPE char01.

  CHECK it_mensagens[] IS NOT INITIAL.

  LOOP AT it_mensagens_sel ASSIGNING FIELD-SYMBOL(<fs_msg>).

    IF <fs_msg>-tp_sincronia NE zif_integracao=>at_tp_sincronia_assincrona AND cx_force EQ abap_false.
      CONTINUE.
    ENDIF.

    TRY .
        zcl_integracao=>zif_integracao~get_instance(
          )->set_registro( i_id_integracao = <fs_msg>-id_integracao
          )->set_outbound_msg( i_force = cx_force
          )->set_inbound_msg(
          )->free(
          ).

      CATCH zcx_integracao INTO DATA(ex_erro).    " .
        ex_erro->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO DATA(ex_error).    " .
        ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_log .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZINTEGRACAO_LOG_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog2.

  DELETE it_fieldcatalog2 WHERE fieldname = 'DTHR_REGISTRO'.
  DELETE it_fieldcatalog2 WHERE fieldname = 'DTHR_RESPOSTA'.

  LOOP AT it_fieldcatalog2 ASSIGNING <fs_cat>.
    IF <fs_cat>-fieldname(4) = 'ICO_'.
      <fs_cat>-icon    = abap_true.
      <fs_cat>-just    = 'C'.
    ENDIF.

    IF <fs_cat>-fieldname = 'ICO_MSG_RETORNO'.
      <fs_cat>-hotspot = abap_true.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_log .

  gs_variant2-report      = sy-repid.
  gs_variant2-handle      = '0101'.
  gs_variant2-log_group   = abap_false.
  gs_variant2-username    = abap_false.
  gs_variant2-variant     = abap_false.
  gs_variant2-text        = abap_false.
  gs_variant2-dependvars  = abap_false.

  gs_layout2-sel_mode   = 'A'.
  gs_layout2-info_fname = 'LINE_COLOR'.
  gs_layout2-stylefname = 'STYLE'.
  gs_layout2-ctab_fname = 'COLOR_CELL'.
  gs_layout2-zebra      = abap_false.
  gs_layout2-cwidth_opt = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CONTAINER  text
*----------------------------------------------------------------------*
FORM cria_alv_log  USING container TYPE REF TO cl_gui_container.

  IF ctl_alv2 IS INITIAL AND container IS NOT INITIAL.

    CREATE OBJECT ctl_alv2
      EXPORTING
        i_parent = container.

    PERFORM fill_it_fieldcatalog_log.
    PERFORM fill_gs_variant_log.

    CREATE OBJECT obg_toolbar2
      EXPORTING
        io_alv_grid = ctl_alv2.

    SET HANDLER obg_toolbar2->on_toolbar FOR ctl_alv2.
    SET HANDLER obg_toolbar2->handle_user_command FOR ctl_alv2.

    CALL METHOD ctl_alv2->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout2
        is_variant      = gs_variant2
        i_save          = 'A'
        it_except_qinfo = zcl_integracao=>zif_integracao~get_qinfo_alv( )
      CHANGING
        it_fieldcatalog = it_fieldcatalog2
        it_outtab       = it_mensagens_log[].

    CREATE OBJECT event_handler2.
    SET HANDLER event_handler2->handle_hotspot_click FOR ctl_alv2.
    SET HANDLER event_handler2->handle_double_click  FOR ctl_alv2.

  ENDIF.

  IF ctl_alv2 IS NOT INITIAL.
    ctl_alv2->refresh_table_display( ).

    ctl_alv2->set_scroll_info_via_id(
          EXPORTING
            is_row_info = gs_scroll_row2
            is_col_info = gs_scroll_col2
            is_row_no   = gs_scroll_row_id2
        ).

  ENDIF.


ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_PRINCIPAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CTL_CCCONTAINER2  text
*----------------------------------------------------------------------*
FORM cria_alv_principal  USING container TYPE REF TO cl_gui_container.

  IF ctl_alv IS INITIAL AND container IS NOT INITIAL.
    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = container.

    PERFORM fill_it_fieldcatalog.

    PERFORM fill_gs_variant.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = ctl_alv.

    SET HANDLER obg_toolbar->on_toolbar FOR ctl_alv.
    SET HANDLER obg_toolbar->handle_user_command FOR ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
        it_except_qinfo = zcl_integracao=>zif_integracao~get_qinfo_alv( )
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_mensagens[].

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv.
    SET HANDLER event_handler->handle_double_click  FOR ctl_alv.

  ENDIF.

  IF ctl_alv IS NOT INITIAL .
    ctl_alv->refresh_table_display(
      EXPORTING
        is_stable      = VALUE #( row = abap_true col = abap_true )
        i_soft_refresh = abap_true ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: lc_row TYPE lvc_t_row,
        p_row  TYPE lvc_s_row.

  READ TABLE it_mensagens INDEX row_id INTO wa_mensagens.

  p_row-index = row_id.
  gs_scroll_row_id-row_id = row_id.
  APPEND p_row TO lc_row.

  CASE fieldname.
    WHEN 'ICO_MSG_RETORNO'.

      TRY .
          zcl_integracao=>zif_integracao~get_instance(
           )->set_registro( i_id_integracao = wa_mensagens-id_integracao
           )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
           )->free(
           ).
          lc_data_view = e_integracao-ds_data_retorno.
          CHECK lc_data_view IS NOT INITIAL.
          PERFORM seleciona_msg USING wa_mensagens-id_integracao.
          LEAVE TO SCREEN 0100.
        CATCH zcx_integracao INTO DATA(ex_integra).    "
          ex_integra->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_error INTO DATA(ex_error).
          ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN 'ICO_MSG_HEADER'.

      TRY .
          zcl_integracao=>zif_integracao~get_instance(
           )->set_registro( i_id_integracao = wa_mensagens-id_integracao
           )->get_registro( IMPORTING e_integracao = e_integracao
           )->free(
           ).
          lc_data_view = e_integracao-ds_header.
          CHECK lc_data_view IS NOT INITIAL.
          PERFORM seleciona_msg USING wa_mensagens-id_integracao.
          LEAVE TO SCREEN 0100.

        CATCH zcx_integracao INTO ex_integra.    "
          ex_integra->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_error INTO ex_error.
          ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.


    WHEN 'ICO_MSG_ENVIO'.

      TRY .
          zcl_integracao=>zif_integracao~get_instance(
           )->set_registro( i_id_integracao = wa_mensagens-id_integracao
           )->get_registro( IMPORTING e_integracao = e_integracao
           )->free(
           ).
          lc_data_view = e_integracao-ds_body.
          CHECK lc_data_view IS NOT INITIAL.
          PERFORM seleciona_msg USING wa_mensagens-id_integracao.
          LEAVE TO SCREEN 0100.
        CATCH zcx_integracao INTO ex_integra.    "
          ex_integra->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_error INTO ex_error.
          ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN 'ICO_ST_MSG'.
      PERFORM seleciona_msg USING wa_mensagens-id_integracao.
      PERFORM mostra_log_msg USING wa_mensagens-id_integracao.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click2
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: lc_row TYPE lvc_t_row,
        p_row  TYPE lvc_s_row.

  READ TABLE it_mensagens_log INDEX row_id INTO wa_mensagens_log.

  p_row-index = row_id.
  gs_scroll_row_id-row_id = row_id.
  APPEND p_row TO lc_row.

  CASE fieldname.
    WHEN 'ICO_MSG_RETORNO'.

      TRY .
          zcl_integracao=>zif_integracao~get_instance(
           )->get_registro_log(
            EXPORTING
              i_id_integracao  = wa_mensagens_log-id_integracao    " Id. de Integração
              i_dt_registro    = wa_mensagens_log-dt_registro    " Data de Registro
              i_hr_registro    = wa_mensagens_log-hr_registro    " Hora de Registro
            IMPORTING
              e_integracao_log = DATA(e_integracao_log)
           ).
          lc_data_view = e_integracao_log-ds_data_retorno.
          CHECK lc_data_view IS NOT INITIAL.
          LEAVE TO SCREEN 0100.
        CATCH zcx_integracao.
      ENDTRY.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click  USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  IF p_row-rowtype IS INITIAL.
    gs_scroll_row_id-row_id = p_row-index.
    APPEND p_row TO lc_row.
    ctl_alv->set_selected_rows( EXPORTING it_index_rows = lc_row ).
    READ TABLE it_mensagens INDEX p_row-index INTO wa_mensagens .
    PERFORM seleciona_msg USING wa_mensagens-id_integracao.
    PERFORM mostra_log_msg USING wa_mensagens-id_integracao.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK


*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click2  USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  IF p_row-rowtype IS INITIAL.
    READ TABLE it_mensagens_log INDEX p_row-index INTO wa_mensagens_log.
    PERFORM mostra_log_msg USING wa_mensagens-id_integracao.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK



*&---------------------------------------------------------------------*
*&      Form  MOSTRA_LOG_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MENSAGENS_ID_INTEGRACAO  text
*----------------------------------------------------------------------*
FORM mostra_log_msg  USING p_id_integracao TYPE zde_id_integracao.

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  CLEAR: it_mensagens_log[].

  SELECT * INTO TABLE @DATA(it_log)
    FROM zintegracao_log
   WHERE id_integracao EQ @p_id_integracao.

  LOOP AT it_log INTO DATA(wa_log).
    TRY .
        zcl_integracao=>zif_integracao~get_instance(
         )->get_msg_log_alv(
          EXPORTING
            i_integracao_log     = wa_log
          IMPORTING
            e_integracao_log_alv = wa_mensagens_log
        ).

        APPEND wa_mensagens_log TO it_mensagens_log.
      CATCH zcx_integracao.    "
    ENDTRY.
  ENDLOOP.

  CHECK it_mensagens_log[] IS NOT INITIAL.

  SORT it_mensagens_log BY dthr_registro DESCENDING.

  READ TABLE it_mensagens_log INTO wa_mensagens_log INDEX 1.
  READ TABLE it_log INTO wa_log
    WITH KEY dt_registro = wa_mensagens_log-dt_registro
             hr_registro = wa_mensagens_log-hr_registro .
  IF sy-subrc IS INITIAL.
    lc_data_view = wa_log-ds_data_retorno.
  ENDIF.

  LEAVE TO SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPA_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_objetos .

  IF lc_open_browser = abap_true.
    cl_abap_browser=>close_browser( ).
    lc_open_browser = abap_false.
  ENDIF.

  IF ctl_alv IS NOT INITIAL.
    ctl_alv->free( ).
  ENDIF.

  IF ctl_alv2 IS NOT INITIAL.
    ctl_alv2->free( ).
  ENDIF.

  IF ctl_cccontainer5 IS NOT INITIAL.
    ctl_cccontainer5->free( ).
  ENDIF.

  IF ctl_cccontainer4 IS NOT INITIAL.
    ctl_cccontainer4->free( ).
  ENDIF.

  IF dg_splitter2 IS NOT INITIAL.
    dg_splitter2->free( ).
  ENDIF.

  IF ctl_cccontainer3 IS NOT INITIAL.
    ctl_cccontainer3->free( ).
  ENDIF.

  IF ctl_cccontainer2 IS NOT INITIAL.
    ctl_cccontainer2->free( ).
  ENDIF.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->free( ).
  ENDIF.

  CLEAR: event_handler, ctl_alv, ctl_alv2, obg_toolbar, obj_toolbarmanager,
        obg_toolbar2, obj_toolbarmanager2,
        ctl_cccontainer2, ctl_cccontainer3, ctl_cccontainer4, ctl_cccontainer5, dg_splitter, dg_splitter2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_SPLITTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_splitter .

  DATA: qtd_rows   TYPE i.
  DATA: qtd_coluns TYPE i.

  qtd_rows   = 1.

  IF it_mensagens_log[] IS NOT INITIAL OR lc_data_view IS NOT INITIAL.
    ADD 1 TO qtd_rows.
  ENDIF.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->get_rows( IMPORTING result = DATA(qtd_rows_criadas) ).
    IF qtd_rows_criadas NE qtd_rows.
      PERFORM limpa_objetos.
    ENDIF.
  ENDIF.

  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = qtd_rows
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = ctl_cccontainer2.

    IF qtd_rows GT 1.

      CALL METHOD dg_splitter->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = ctl_cccontainer3.

      CALL METHOD dg_splitter->set_row_height
        EXPORTING
          id     = 2
          height = 20.

    ENDIF.

  ENDIF.

  qtd_coluns = 0.
  IF it_mensagens_log[] IS NOT INITIAL OR lc_data_view IS NOT INITIAL.
    IF it_mensagens_log[] IS NOT INITIAL.
      ADD 1 TO qtd_coluns.
    ENDIF.
    IF lc_data_view IS NOT INITIAL.
      ADD 1 TO qtd_coluns.
    ENDIF.
  ENDIF.

  IF dg_splitter2 IS NOT INITIAL.
    dg_splitter2->get_columns( IMPORTING result = DATA(qtd_rows_obj) ).
    IF qtd_rows_obj NE qtd_coluns.
      PERFORM limpa_objetos_splitter2.
    ENDIF.
  ENDIF.

  IF qtd_rows GT 1 AND dg_splitter2 IS INITIAL.
    CREATE OBJECT dg_splitter2
      EXPORTING
        parent  = ctl_cccontainer3
        rows    = 1
        columns = qtd_coluns.

    DATA(lc_colunm_splitt) = 0.

    IF it_mensagens_log[] IS NOT INITIAL.
      ADD 1 TO lc_colunm_splitt.
      CALL METHOD dg_splitter2->get_container
        EXPORTING
          row       = 1
          column    = lc_colunm_splitt
        RECEIVING
          container = ctl_cccontainer4.
    ENDIF.

    IF lc_data_view IS NOT INITIAL.
      ADD 1 TO lc_colunm_splitt.
      CALL METHOD dg_splitter2->get_container
        EXPORTING
          row       = 1
          column    = lc_colunm_splitt
        RECEIVING
          container = ctl_cccontainer5.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MENSAGENS_ID_INTEGRACAO  text
*----------------------------------------------------------------------*
FORM seleciona_msg  USING  p_id_integracao TYPE zde_id_integracao.

  IF lc_id_selecionado IS NOT INITIAL.
    READ TABLE it_mensagens WITH KEY id_integracao = lc_id_selecionado ASSIGNING FIELD-SYMBOL(<fs_msg>).
    IF sy-subrc IS INITIAL.
      <fs_msg>-line_color = zcl_integracao=>zif_integracao~get_line_color(
                              i_tp_sincronia = <fs_msg>-tp_sincronia
                              i_status = <fs_msg>-ico_st_msg
                              i_selected = abap_false
                              i_anonimo  = <fs_msg>-ck_anonimo ).
    ENDIF.
  ENDIF.

  READ TABLE it_mensagens WITH KEY id_integracao = p_id_integracao ASSIGNING <fs_msg>.
  IF sy-subrc IS INITIAL.
    <fs_msg>-line_color = zcl_integracao=>zif_integracao~get_line_color(
                            i_tp_sincronia = <fs_msg>-tp_sincronia
                            i_status = <fs_msg>-ico_st_msg
                            i_selected = abap_true
                            i_anonimo = <fs_msg>-ck_anonimo ).
    lc_id_selecionado  = p_id_integracao.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESSAR_MENSAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MENSAGENS_SEL  text
*----------------------------------------------------------------------*
FORM processar_mensagens  USING p_sel TYPE zintegracao_alv_t.

  CHECK it_mensagens[] IS NOT INITIAL.

  LOOP AT it_mensagens_sel ASSIGNING FIELD-SYMBOL(<fs_msg>).

    IF <fs_msg>-tp_sincronia NE zif_integracao=>at_tp_sincronia_assincrona.
      CONTINUE.
    ENDIF.

    TRY .
        zcl_integracao=>zif_integracao~get_instance(
          )->set_registro( i_id_integracao = <fs_msg>-id_integracao
          )->set_processar_retorno(
          )->free(
          ).
      CATCH zcx_integracao INTO DATA(ex_erro).    " .
        ex_erro->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO DATA(ex_error).    " .
        ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESSAR_MENSAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MENSAGENS_SEL  text
*----------------------------------------------------------------------*
FORM integrar_mensagens  USING p_sel TYPE zintegracao_alv_t.

  CHECK it_mensagens[] IS NOT INITIAL.

  LOOP AT it_mensagens_sel ASSIGNING FIELD-SYMBOL(<fs_msg>).

    IF <fs_msg>-tp_sincronia NE zif_integracao=>at_tp_sincronia_assincrona.
      CONTINUE.
    ENDIF.

    TRY .
        zcl_integracao=>zif_integracao~get_instance(
          )->set_registro( i_id_integracao = <fs_msg>-id_integracao
          )->set_integrar_retorno(
          )->free(
          ).
      CATCH zcx_integracao INTO DATA(ex_erro).    " .
        ex_erro->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO DATA(ex_error).    " .
        ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CTL_CCCONTAINER5  text
*----------------------------------------------------------------------*
FORM cria_html_view  USING container TYPE REF TO cl_gui_container.

  CHECK lc_data_view IS NOT INITIAL.
  CHECK container IS NOT INITIAL.

*  IF ZCL_STRING=>LENGTH( TEXT = LC_DATA_VIEW ) GE 14.
*
*    IF LC_DATA_VIEW(14) NE '<!DOCTYPE html'.
*
*      DATA(HTML_VIEW) = '<!DOCTYPE html>' &&
*                        '<html>' &&
*
*                        '<style>' &&
*                        'body {' &&
*                        '   background-color: white;' &&
*                        '   margin: 10px;' &&
*                        '   }' &&
*                        'p {' &&
*                        '   background-color: mintcream;' &&
*                        '   border: 1px solid silver;' &&
*                        '   padding: 1px 1px;' &&
*                        '   }' &&
*                        '.json-key {' &&
*                        '   color: brown;' &&
*                        '   }' &&
*                        '.json-value {' &&
*                        '   color: navy;' &&
*                        '   }' &&
*                        '.json-boolean {' &&
*                        '   color: teal;' &&
*                        '   }' &&
*                        '.json-string {' &&
*                        '   color: olive;' &&
*                        '   }' &&
*                        '</style>' &&
*
*                        '<body>' &&
*                        '<p id="text_json"></p>' &&
*                        '<script>' &&
*                        'function replacer(match, p1, p2, p3, p4) { ' &&
*                        '   const part = { indent: p1, key: p2, value: p3, end: p4 }; ' &&
*                        '   const key =  ''<span class=json-key>''; ' &&
*                        '   const val =  ''<span class=json-value>''; ' &&
*                        '   const bool = ''<span class=json-boolean>''; ' &&
*                        '   const str =  ''<span class=json-string>''; ' &&
*                        '   const isBool = [''true'', ''false''].includes(part.value); ' &&
*                        '   const valSpan = /^"/.test(part.value) ? str : isBool ? bool : val; ' &&
*                        '   const findName = /"([\w]+)": |(.*): /; ' &&
*                        '   const indentHtml = part.indent || ''''; ' &&
*                        '   const keyHtml =    part.key ? key + part.key.replace(findName, ''$1$2'') + ''</span>: '' : ''''; ' &&
*                        '   const valueHtml =  part.value ? valSpan + part.value + ''</span>'' : ''''; ' &&
*                        '   const endHtml =    part.end || ''''; ' &&
*                        '   return indentHtml + keyHtml + valueHtml + endHtml; ' &&
*                        '   }; ' &&
*
*                        'const jsonLine = /^( *)("[^"]+": )?("[^"]*"|[\w.+-]*)?([{}[\],]*)?$/mg; ' &&
*                        'var myJSONtexto = ''' && ZCL_STRING=>CONVERT_TXT_STRING_TO_JSON( LC_DATA_VIEW ) && ''';' &&
*                        'var myObj = JSON.parse(myJSONtexto); '  &&
*                        "'var obj = ' && LC_DATA_VIEW && ';' &&
*                        "'var myJSON = JSON.stringify(obj)' &&
*                        'var myJSON = JSON.stringify(myObj)' &&
*                        "'var myJSON = JSON.stringify(myObj,null,3)' &&
*                        '.replace(/&/g, ''&amp;'') ' && SPACE &&
*                        '.replace(/\\"/g, ''&quot;'')' && SPACE &&
*                        '.replace(/</g, ''&lt;'') ' &&
*                        '.replace(/>/g, ''&gt;'') ' &&
*                        '.replace(jsonLine, replacer);' &&
*                        ';' &&
*                        'document.getElementById("text_json").innerHTML = myJSON;' &&
*                        '</script>' &&
*                        '</body>' &&
*                        '</html>' .
*    ELSE.
*      HTML_VIEW = LC_DATA_VIEW.
*    ENDIF.
*  ELSE.
*  HTML_VIEW = LC_DATA_VIEW.
*  ENDIF.

*-US 140617-30.12.2024-#140617-JT-inicio
  CASE wa_mensagens-id_interface.
    WHEN '254' OR '255' OR '256' OR '257' OR '258' OR '263'.
      IF lc_data_view(19) <> '<?xml version="1.0"' AND lc_data_view(01) <> '['.
        lc_data_view = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' && lc_data_view.
      ENDIF.
  ENDCASE.
*-US 140617-30.12.2024-#140617-JT-fim

  IF lc_open_browser = abap_true.
    cl_abap_browser=>close_browser( ).
    lc_open_browser = abap_false.
  ENDIF.

  lc_open_browser = abap_true.

  cl_abap_browser=>show_html(
   EXPORTING
     html_string = lc_data_view
     modal       = abap_false
     format      = cl_abap_browser=>landscape
     size        = cl_abap_browser=>small
     container   = container ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIMPA_OBJETOS_SPLITTER2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_objetos_splitter2 .

  IF lc_open_browser = abap_true.
    cl_abap_browser=>close_browser( ).
    lc_open_browser = abap_false.
  ENDIF.

  IF ctl_alv2 IS NOT INITIAL.
    ctl_alv2->free( ).
  ENDIF.

  IF ctl_cccontainer5 IS NOT INITIAL.
    ctl_cccontainer5->free( ).
  ENDIF.

  IF ctl_cccontainer4 IS NOT INITIAL.
    ctl_cccontainer4->free( ).
  ENDIF.

  IF dg_splitter2 IS NOT INITIAL.
    dg_splitter2->free( ).
  ENDIF.

  CLEAR: ctl_alv2, ctl_cccontainer4, ctl_cccontainer5, dg_splitter2, obg_toolbar2, obj_toolbarmanager2.

ENDFORM.

FORM f_exec_a_enviar.

  DATA: it_integra TYPE TABLE OF ty_integracao_proc.

  "Enviar Documentos Não Enviados

  DATA(_proc_paralelo)       = abap_false.
  DATA(_times_processamento) = 1.
  PERFORM f_get_config_exec_paralelo USING c_enviar CHANGING _proc_paralelo _times_processamento.

  DO _times_processamento TIMES.

    CLEAR: it_integra[].

    DO 6 TIMES.

      SELECT id_integracao dt_registro hr_registro INTO TABLE it_integra
        FROM zintegracao
       WHERE ck_retornou   EQ abap_false
         AND qt_erro       LE 5
         AND ck_cancelou   EQ abap_false
         AND tp_integracao EQ zif_integracao=>at_tp_integracao_outbound
         AND tp_sincronia  EQ zif_integracao=>at_tp_sincronia_assincrona
         AND ck_anonimo    EQ abap_false.

      IF it_integra[] IS NOT INITIAL.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.

    ENDDO.

    IF it_integra[] IS INITIAL.
      EXIT.
    ENDIF.

    SORT it_integra BY dt_registro hr_registro.

    CASE _proc_paralelo.
      WHEN abap_false.

        LOOP AT it_integra INTO DATA(wa_integra) .
          TRY .
              zcl_integracao=>zif_integracao~get_instance(
               )->set_registro( i_id_integracao = wa_integra-id_integracao
               )->set_outbound_msg(
               )->free(
               ).
            CATCH zcx_integracao INTO DATA(ex_integra).
              ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            CATCH zcx_error INTO DATA(ex_error).    " .
              ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          ENDTRY.
        ENDLOOP.

      WHEN abap_true.
        PERFORM f_exec_paralela TABLES it_integra USING c_enviar.
    ENDCASE.

  ENDDO.

ENDFORM.

FORM f_exec_a_processar.

  DATA: it_integra TYPE TABLE OF ty_integracao_proc.

  "Processar Documentos Recebidos e não Processados
  DATA(_proc_paralelo)       = abap_false.
  DATA(_times_processamento) = 1.
  PERFORM f_get_config_exec_paralelo USING c_processar CHANGING _proc_paralelo _times_processamento.

  DO _times_processamento TIMES.

    CLEAR: it_integra[].

    DO 6 TIMES.

      SELECT id_integracao dt_registro hr_registro INTO TABLE it_integra
        FROM zintegracao
       WHERE ck_retornou   EQ abap_true
         AND ck_processado EQ abap_false
         AND qt_erro       LE 5
         AND ck_cancelou   EQ abap_false
         AND tp_integracao EQ zif_integracao=>at_tp_integracao_outbound
         AND tp_sincronia  EQ zif_integracao=>at_tp_sincronia_assincrona
         AND ck_anonimo    EQ abap_false.

      SELECT id_integracao dt_registro hr_registro APPENDING TABLE it_integra
        FROM zintegracao
       WHERE ck_retornou   EQ abap_false
         AND ck_processado EQ abap_false
         AND qt_erro       LE 5
         AND ck_cancelou   EQ abap_false
         AND tp_integracao EQ zif_integracao=>at_tp_integracao_inbound
         AND tp_sincronia  EQ zif_integracao=>at_tp_sincronia_assincrona
         AND ck_anonimo    EQ abap_false.

      IF it_integra[] IS NOT INITIAL.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.

    ENDDO.

    IF it_integra[] IS INITIAL.
      EXIT.
    ENDIF.

    SORT it_integra BY dt_registro hr_registro.

    CASE _proc_paralelo.
      WHEN abap_false.

        LOOP AT it_integra INTO DATA(wa_integra).
          TRY .
              zcl_integracao=>zif_integracao~get_instance(
               )->set_registro( i_id_integracao = wa_integra-id_integracao
               )->set_processar_retorno(
               )->free(
               ).
            CATCH zcx_integracao INTO DATA(ex_integra).
              ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            CATCH zcx_error INTO DATA(ex_error).
              ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          ENDTRY.
        ENDLOOP.

      WHEN abap_true.
        PERFORM f_exec_paralela TABLES it_integra USING c_processar.
    ENDCASE.

  ENDDO.

ENDFORM.

FORM f_exec_a_integrar.

  DATA: it_integra TYPE TABLE OF ty_integracao_proc.

  "Integrar Documentos Recebidos, Processados e não Integrados

  DATA(_proc_paralelo)       = abap_false.
  DATA(_times_processamento) = 1.
  PERFORM f_get_config_exec_paralelo USING c_integrar CHANGING _proc_paralelo _times_processamento.

  DO _times_processamento TIMES.

    CLEAR: it_integra[].

    DO 6 TIMES.
      SELECT id_integracao dt_registro hr_registro INTO TABLE it_integra
        FROM zintegracao
       WHERE ck_retornou   EQ abap_true
         AND ck_processado EQ abap_true
         AND ck_integrado  EQ abap_false
         AND qt_erro       LE 5
         AND ck_cancelou   EQ abap_false
         AND tp_integracao EQ zif_integracao=>at_tp_integracao_outbound
         AND tp_sincronia  EQ zif_integracao=>at_tp_sincronia_assincrona
         AND ck_anonimo    EQ abap_false.

      SELECT id_integracao dt_registro hr_registro APPENDING TABLE it_integra
        FROM zintegracao
       WHERE ck_retornou   EQ abap_false
         AND ck_processado EQ abap_true
         AND ck_integrado  EQ abap_false
         AND qt_erro       LE 5
         AND ck_cancelou   EQ abap_false
         AND tp_integracao EQ zif_integracao=>at_tp_integracao_inbound
         AND tp_sincronia  EQ zif_integracao=>at_tp_sincronia_assincrona
         AND ck_anonimo    EQ abap_false.

      IF it_integra[] IS NOT INITIAL.
        EXIT.
      ELSE.
        WAIT UP TO 10 SECONDS.
      ENDIF.

    ENDDO.

    IF it_integra[] IS INITIAL.
      EXIT.
    ENDIF.

    SORT it_integra BY dt_registro hr_registro.

    CASE _proc_paralelo.
      WHEN abap_false.

        LOOP AT it_integra INTO DATA(wa_integra).
          TRY .
              zcl_integracao=>zif_integracao~get_instance(
               )->set_registro( i_id_integracao = wa_integra-id_integracao
               )->set_integrar_retorno(
               )->free(
               ).
            CATCH zcx_integracao INTO DATA(ex_integra).
              ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            CATCH zcx_error INTO DATA(ex_error).
              ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          ENDTRY.
        ENDLOOP.

      WHEN abap_true.
        PERFORM f_exec_paralela TABLES it_integra USING c_integrar.
    ENDCASE.

  ENDDO.


ENDFORM.

FORM f_clear_log USING p_origem.

  TYPES: BEGIN OF ty_zintegracao01,
           mandt         TYPE zintegracao01-mandt,
           id_integracao TYPE zintegracao01-id_integracao,
           id_form       TYPE zintegracao01-id_form,
         END OF ty_zintegracao01.

  DATA: lit_zintegracao01 TYPE TABLE OF ty_zintegracao01.

  DATA: lva_qtde_registro TYPE string.

  CLEAR: lva_interfaces_del_001, lva_interfaces_del_002, lc_data_delete.

  MESSAGE 'Iniciando limpeza Logs...' TYPE 'I'.

  CASE p_origem.
    WHEN '2'.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(lwa_conf_0006)
       WHERE setname = 'ZINTEGRACAO_CONF_0006'. "Qtdes Registros Deletar

      IF sy-subrc EQ 0 AND lwa_conf_0006-valfrom IS NOT INITIAL.
        lva_qtde_registro = lwa_conf_0006-valfrom.
      ELSE.
        lva_qtde_registro = '600000'.
      ENDIF.

    WHEN OTHERS.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(lwa_conf_0004)
       WHERE setname = 'ZINTEGRACAO_CONF_0004'
         AND valfrom = 'STOP_CLEAR'.

      CHECK sy-subrc NE 0.

      lva_qtde_registro = '5000'.
  ENDCASE.


  SELECT *
    FROM setleaf INTO TABLE @DATA(lit_conf_0001)
   WHERE setname = 'ZINTEGRACAO_CONF_0001'. "Interface deletar registro dia anterior

  LOOP AT lit_conf_0001 INTO DATA(lwa_conf_0001) WHERE valfrom IS NOT INITIAL.

    IF lwa_conf_0001-valfrom IS INITIAL.
      CONCATENATE '''' '''' INTO lwa_conf_0001-valfrom SEPARATED BY space.
    ELSE.
      CONCATENATE '''' lwa_conf_0001-valfrom '''' INTO lwa_conf_0001-valfrom.
    ENDIF.

    IF lva_interfaces_del_001 IS INITIAL.
      lva_interfaces_del_001 = lwa_conf_0001-valfrom.
    ELSE.
      CONCATENATE lva_interfaces_del_001 ',' lwa_conf_0001-valfrom INTO lva_interfaces_del_001.
    ENDIF.
  ENDLOOP.

  SELECT *
    FROM setleaf INTO TABLE @DATA(lit_conf_0002)
   WHERE setname = 'ZINTEGRACAO_CONF_0002'. "Interface deletar registro com dias retenção parametrizado

  LOOP AT lit_conf_0002 INTO DATA(lwa_conf_0002).

    IF lwa_conf_0002-valfrom IS INITIAL.
      CONCATENATE '''' '''' INTO lwa_conf_0002-valfrom SEPARATED BY space.
    ELSE.
      CONCATENATE '''' lwa_conf_0002-valfrom '''' INTO lwa_conf_0002-valfrom.
    ENDIF.

    IF lva_interfaces_del_002 IS INITIAL.
      lva_interfaces_del_002 = lwa_conf_0002-valfrom.
    ELSE.
      CONCATENATE lva_interfaces_del_002 ','  lwa_conf_0002-valfrom INTO lva_interfaces_del_002.
    ENDIF.
  ENDLOOP.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(lwa_conf_0003)
   WHERE setname = 'ZINTEGRACAO_CONF_0003'. "Dias retenção

  IF sy-subrc EQ 0 AND lwa_conf_0003-valfrom IS NOT INITIAL.
    lc_data_delete = sy-datum - lwa_conf_0003-valfrom.
  ELSE.
    lc_data_delete = sy-datum - 90.
  ENDIF.

  IF lva_interfaces_del_001 IS NOT INITIAL.
    DATA(lc_delete_02) = |DELETE FROM SAPHANADB.ZINTEGRACAO WHERE MANDT = '{ sy-mandt }' AND ID_INTERFACE IN ( { lva_interfaces_del_001 } ) AND DT_REGISTRO < '{ sy-datum }'|.  "AND ROWNUM <= { lva_qtde_registro } |.
    NEW cl_sql_statement( )->execute_update( statement = lc_delete_02 ).
    COMMIT WORK.
  ENDIF.

  IF lva_interfaces_del_002 IS NOT INITIAL.
    DATA(lc_delete_05) = |DELETE FROM SAPHANADB.ZINTEGRACAO WHERE MANDT = '{ sy-mandt }' AND ID_INTERFACE IN ( { lva_interfaces_del_002 } ) AND DT_REGISTRO < '{ lc_data_delete }'|. "AND ROWNUM <= { lva_qtde_registro } |.
    NEW cl_sql_statement( )->execute_update( statement = lc_delete_05 ).
    COMMIT WORK.
  ENDIF.

  DATA(lc_delete_04) = |DELETE FROM SAPHANADB.ZINTEGRACAO_LOG WHERE MANDT = '{ sy-mandt }' AND DT_REGISTRO < '{ lc_data_delete }'|. "AND ROWNUM <=  { lva_qtde_registro } |.
  NEW cl_sql_statement( )->execute_update( statement = lc_delete_04 ).
  COMMIT WORK.

  CLEAR: lit_zintegracao01[].
  SELECT mandt id_integracao id_form
    FROM zintegracao01 AS a INTO TABLE lit_zintegracao01
     UP TO lva_qtde_registro ROWS
   WHERE NOT EXISTS ( SELECT id_integracao
                        FROM zintegracao AS b
                     WHERE b~id_integracao = a~id_integracao ).

  IF lit_zintegracao01[] IS NOT INITIAL.
    DELETE zintegracao01 FROM TABLE lit_zintegracao01.
  ENDIF.


ENDFORM.

FORM f_get_qtde_program_exec_proc USING p_show_msg
                                        p_tipo TYPE char50
                               CHANGING c_quantidade TYPE i.

  DATA: lc_quantidade_int TYPE i,
        lc_quantidade_str TYPE c LENGTH 10.

  CLEAR: c_quantidade.

  DATA: it_status TYPE zde_btcstatus_t.

  APPEND 'S' TO it_status.
  APPEND 'R' TO it_status.
  APPEND 'P' TO it_status.


  CASE p_tipo.
    WHEN c_processar.
      DATA(_progname) = 'ZINTEGRACAO_PROCESSAR'.
    WHEN c_integrar.
      _progname       = 'ZINTEGRACAO_INTEGRAR'.
    WHEN c_enviar.
      _progname       = 'ZINTEGRACAO_ENVIAR'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.


  zcl_job=>get_job_programa_execucao(
    EXPORTING
      i_progname   = CONV #( _progname )   " Nome de um programa em uma etapa (p.ex. report)
      i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
      i_status     = it_status   " Status de Jobs
    IMPORTING
      e_quantidade = lc_quantidade_int ).

  ADD lc_quantidade_int TO c_quantidade.

  WRITE lc_quantidade_int TO lc_quantidade_str.
  CONDENSE lc_quantidade_str NO-GAPS.

  IF p_show_msg EQ abap_true.
    MESSAGE s000 WITH 'Quantidade de Jobs p/ Programa' 'ZINTEGRACAO_PROCESSAR:' lc_quantidade_str.
  ENDIF.


ENDFORM.


FORM f_check_jobs_erro_escal_proc USING p_tipo TYPE char50.

  DATA: lit_jobs        TYPE /sdf/tbtco_tt,
        lit_status_scal TYPE zde_btcstatus_t,
        lit_tbtco_del   TYPE TABLE OF tbtco.

  DATA: lva_init_date    TYPE btcsdldate.

  CLEAR: lit_jobs[], lit_status_scal[], lit_tbtco_del[].

  lva_init_date = sy-datum - 1.

  APPEND 'P' TO lit_status_scal.

*-----------------------------------------------------------------------------*
* Get Jobs do programa ZMMR019 e ZMMR020( Criação Registros ) com status Escalonado
*-----------------------------------------------------------------------------*

  CASE p_tipo.
    WHEN c_processar.
      DATA(_job_name_cp) = 'ZINTEGRACAO_PROC_*'.
    WHEN c_integrar.
      _job_name_cp = 'ZINTEGRACAO_INT_*'.
    WHEN c_enviar.
      _job_name_cp = 'ZINTEGRACAO_ENV_*'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  zcl_job=>get_job_programa_execucao(
    EXPORTING
      i_job_name_cp   = CONV #( _job_name_cp )     " Nome de um programa em uma etapa (p.ex. report)
      i_sdldate_init  = lva_init_date     " Data de escalonamento de job ou etapa
      i_status        = lit_status_scal   " Status de Jobs
    IMPORTING
      e_jobs          = lit_jobs ).

  CHECK lit_jobs[] IS NOT INITIAL.

  DATA(_qtde_jobs_esc) = lines( lit_jobs[] ).

  WAIT UP TO 10 SECONDS.

  SELECT *
    FROM tbtco INTO TABLE lit_tbtco_del
     FOR ALL ENTRIES IN lit_jobs
   WHERE jobname  EQ lit_jobs-jobname
     AND jobcount EQ lit_jobs-jobcount.

  LOOP AT lit_tbtco_del INTO DATA(lwa_tbtco_del).

    CALL FUNCTION 'BP_JOB_DELETE'
      EXPORTING
        jobcount                 = lwa_tbtco_del-jobcount
        jobname                  = lwa_tbtco_del-jobname
      EXCEPTIONS
        cant_delete_event_entry  = 1
        cant_delete_job          = 2
        cant_delete_joblog       = 3
        cant_delete_steps        = 4
        cant_delete_time_entry   = 5
        cant_derelease_successor = 6
        cant_enq_predecessor     = 7
        cant_enq_successor       = 8
        cant_enq_tbtco_entry     = 9
        cant_update_predecessor  = 10
        cant_update_successor    = 11
        commit_failed            = 12
        jobcount_missing         = 13
        jobname_missing          = 14
        job_does_not_exist       = 15
        job_is_already_running   = 16
        no_delete_authority      = 17
        OTHERS                   = 18.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

FORM f_exec_paralela TABLES t_integra LIKE git_list_integra
                      USING p_tipo TYPE char50.

  DATA: it_integra_group TYPE zintegracao_t.
  DATA: lva_qtde_reg_group TYPE i,
        lva_group_job      TYPE string.

  DATA: lva_jobs_in_exec  TYPE i,
        lc_qtde_times     TYPE i,
        lc_show_msg       TYPE c,
        lva_job_number    TYPE tbtcjob-jobcount,
        lva_job_name      TYPE tbtcjob-jobname,
        lva_job_name_like TYPE tbtcjob-jobname,
        print_parameters  TYPE pri_params.

  DATA(lc_user_job) = zcl_job=>get_user_job( ).
  lva_jobs_in_exec = 0.

  DATA(_qtde_jobs_paralelo) = 1.
  PERFORM f_get_qtde_jobs_paralelo USING p_tipo CHANGING _qtde_jobs_paralelo.

  lva_qtde_reg_group = lines( t_integra ) / _qtde_jobs_paralelo.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(lwa_conf_0011)
   WHERE setname = 'ZINTEGRACAO_CONF_0011'.

  IF sy-subrc EQ 0.
    lva_qtde_reg_group = lwa_conf_0011-valfrom.
  ENDIF.

  "Desmembrar Filas e Escalonar jobs paralelos
  DO _qtde_jobs_paralelo TIMES.

    lva_group_job = sy-index.

    CLEAR: it_integra_group[].

    DATA(_qtde_registros_append) = 0.
    LOOP AT t_integra INTO DATA(wa_integra).

      ADD 1 TO _qtde_registros_append.

      APPEND INITIAL LINE TO it_integra_group ASSIGNING FIELD-SYMBOL(<fs_integra_group>).
      <fs_integra_group>-id_integracao = wa_integra-id_integracao.

      DELETE t_integra WHERE id_integracao = wa_integra-id_integracao.

      "IF lva_group_job NE _qtde_jobs_paralelo. "No ultimo grupo de registros, devem processar o restante da fila
      IF _qtde_registros_append >= lva_qtde_reg_group.
        EXIT.
      ENDIF.
      "ENDIF.
    ENDLOOP.

    CHECK it_integra_group[] IS NOT INITIAL.

    CASE p_tipo.
      WHEN c_processar.
        CONCATENATE 'ZINTEGRACAO_PROC' lva_group_job INTO lva_job_name SEPARATED BY '_'.
      WHEN c_integrar.
        CONCATENATE 'ZINTEGRACAO_INT' lva_group_job INTO lva_job_name SEPARATED BY '_'.
      WHEN c_enviar.
        CONCATENATE 'ZINTEGRACAO_ENV' lva_group_job INTO lva_job_name SEPARATED BY '_'.
      WHEN OTHERS.
        EXIT.
    ENDCASE.

    CONDENSE lva_job_name NO-GAPS.

    "Escalonar JOB
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lva_job_name
      IMPORTING
        jobcount         = lva_job_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.

      CASE p_tipo.
        WHEN c_enviar.

          SUBMIT zintegracao_enviar TO SAP-SPOOL SPOOL PARAMETERS print_parameters
          WITHOUT SPOOL DYNPRO VIA JOB lva_job_name NUMBER lva_job_number
          WITH pint_t EQ it_integra_group
          USER lc_user_job
           AND RETURN.

        WHEN c_processar.

          SUBMIT zintegracao_processar TO SAP-SPOOL SPOOL PARAMETERS print_parameters
          WITHOUT SPOOL DYNPRO VIA JOB lva_job_name NUMBER lva_job_number
          WITH pint_t EQ it_integra_group
          USER lc_user_job
           AND RETURN.

        WHEN c_integrar.

          SUBMIT zintegracao_integrar TO SAP-SPOOL SPOOL PARAMETERS print_parameters
          WITHOUT SPOOL DYNPRO VIA JOB lva_job_name NUMBER lva_job_number
          WITH pint_t EQ it_integra_group
          USER lc_user_job
           AND RETURN.

      ENDCASE.


      ADD 1 TO lva_jobs_in_exec.

      MESSAGE s000 WITH 'Programado Job:' lva_job_name.

      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lva_job_number
            jobname              = lva_job_name
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.

        IF sy-subrc IS NOT INITIAL.
          DATA(ck_erro) = abap_true.

          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              jobcount                 = lva_job_number
              jobname                  = lva_job_name
            EXCEPTIONS
              cant_delete_event_entry  = 1
              cant_delete_job          = 2
              cant_delete_joblog       = 3
              cant_delete_steps        = 4
              cant_delete_time_entry   = 5
              cant_derelease_successor = 6
              cant_enq_predecessor     = 7
              cant_enq_successor       = 8
              cant_enq_tbtco_entry     = 9
              cant_update_predecessor  = 10
              cant_update_successor    = 11
              commit_failed            = 12
              jobcount_missing         = 13
              jobname_missing          = 14
              job_does_not_exist       = 15
              job_is_already_running   = 16
              no_delete_authority      = 17
              OTHERS                   = 18.
          IF sy-subrc IS NOT INITIAL.
            ck_erro = abap_false.
          ENDIF.
        ELSE.

        ENDIF.
      ELSE.
        ck_erro = abap_true.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
        CALL FUNCTION 'BP_JOB_DELETE'
          EXPORTING
            jobcount                 = lva_job_number
            jobname                  = lva_job_name
          EXCEPTIONS
            cant_delete_event_entry  = 1
            cant_delete_job          = 2
            cant_delete_joblog       = 3
            cant_delete_steps        = 4
            cant_delete_time_entry   = 5
            cant_derelease_successor = 6
            cant_enq_predecessor     = 7
            cant_enq_successor       = 8
            cant_enq_tbtco_entry     = 9
            cant_update_predecessor  = 10
            cant_update_successor    = 11
            commit_failed            = 12
            jobcount_missing         = 13
            jobname_missing          = 14
            job_does_not_exist       = 15
            job_is_already_running   = 16
            no_delete_authority      = 17
            OTHERS                   = 18.
        IF sy-subrc IS NOT INITIAL.
          ck_erro = abap_false.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDDO.

  WHILE lva_jobs_in_exec > 0.

    PERFORM f_check_jobs_erro_escal_proc USING p_tipo.

    WAIT UP TO 2 SECONDS.

    lc_show_msg = abap_false.
    IF lc_qtde_times EQ 300.
      lc_qtde_times = 0.
      lc_show_msg   = abap_true.
    ENDIF.

    PERFORM f_get_qtde_program_exec_proc USING lc_show_msg    "Exibir Mensagem no JOB
                                               p_tipo
                                      CHANGING lva_jobs_in_exec.

    ADD 1 TO lc_qtde_times.
  ENDWHILE.




ENDFORM.

FORM f_get_config_exec_paralelo  USING p_tipo          TYPE char50
                              CHANGING c_proc_paralelo TYPE c
                                       c_times_proc    TYPE i.

  c_proc_paralelo = abap_false.
  c_times_proc    = 1.

  DATA(_qtde_jobs_paralelo) = 1.
  PERFORM f_get_qtde_jobs_paralelo USING p_tipo CHANGING _qtde_jobs_paralelo.
  IF _qtde_jobs_paralelo > 1.
    c_proc_paralelo = abap_true.
  ENDIF.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(lwa_conf_0008)
   WHERE setname = 'ZINTEGRACAO_CONF_0008'.

  IF sy-subrc EQ 0.
    c_times_proc = lwa_conf_0008-valfrom.
  ENDIF.

ENDFORM.

FORM f_get_qtde_jobs_paralelo USING p_tipo TYPE char50
                           CHANGING c_qtde_jobs_paralelo TYPE i.

  c_qtde_jobs_paralelo = 1.

  CASE p_tipo.
    WHEN c_enviar.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(lwa_conf_0010)
       WHERE setname = 'ZINTEGRACAO_CONF_0010'.

      IF sy-subrc EQ 0.
        c_qtde_jobs_paralelo = lwa_conf_0010-valfrom.
      ENDIF.

    WHEN c_processar.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(lwa_conf_0007)
       WHERE setname = 'ZINTEGRACAO_CONF_0007'.

      IF sy-subrc EQ 0.
        c_qtde_jobs_paralelo = lwa_conf_0007-valfrom.
      ENDIF.

    WHEN c_integrar.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(lwa_conf_0009)
       WHERE setname = 'ZINTEGRACAO_CONF_0009'.

      IF sy-subrc EQ 0.
        c_qtde_jobs_paralelo = lwa_conf_0009-valfrom.
      ENDIF.

  ENDCASE.

ENDFORM.
