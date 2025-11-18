*----------------------------------------------------------------------*
***INCLUDE LZPMT0065I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CUSTOM_BUTTON_FERRAMENTAS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE custom_button_ferramentas INPUT.

  CASE function.
    WHEN 'CAD_CAIXA'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action                       = 'U'
          view_name                    = 'ZPMT0066'
        EXCEPTIONS
          client_reference             = 1
          foreign_lock                 = 2
          invalid_action               = 3
          no_clientindependent_auth    = 4
          no_database_function         = 5
          no_editor_function           = 6
          no_show_auth                 = 7
          no_tvdir_entry               = 8
          no_upd_auth                  = 9
          only_show_allowed            = 10
          system_failure               = 11
          unknown_field_in_dba_sellist = 12
          view_not_found               = 13
          maintenance_prohibited       = 14
          OTHERS                       = 15.
  ENDCASE.

ENDMODULE.
