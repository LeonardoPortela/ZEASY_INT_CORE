*&---------------------------------------------------------------------*
*& Report  ZINTEGRACAO_PROCESSAR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZINTEGRACAO_ENVIAR.


PARAMETERS pint_t TYPE ZINTEGRACAO_T NO-DISPLAY.

START-OF-SELECTION.

  LOOP AT pint_t INTO DATA(wa_integra).
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
