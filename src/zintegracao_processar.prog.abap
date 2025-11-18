*&---------------------------------------------------------------------*
*& Report  ZINTEGRACAO_PROCESSAR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZINTEGRACAO_PROCESSAR.


PARAMETERS pint_t TYPE ZINTEGRACAO_T NO-DISPLAY.

START-OF-SELECTION.

  LOOP AT pint_t INTO DATA(wa_integra).
    TRY .
        zcl_integracao=>zif_integracao~get_instance(
         )->set_registro( i_id_integracao = wa_integra-id_integracao
         )->set_processar_retorno(
         )->free(
         ).
      CATCH zcx_integracao INTO DATA(ex_integra).
      CATCH zcx_error INTO DATA(ex_error).
    ENDTRY.
  ENDLOOP.
