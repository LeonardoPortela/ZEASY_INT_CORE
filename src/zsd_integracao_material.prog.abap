*&---------------------------------------------------------------------*
*& Include ZSD_INTEGRACAO_MATERIAL
*&---------------------------------------------------------------------*

DATA: lc_workarea TYPE mara.

*IF tablename = 'MARA'.
*  lc_workarea = workarea_new.
*
*  IF lc_workarea IS NOT INITIAL.
*    DATA(l_task) = 'INTEGRAR_MATERIAL' && lc_workarea-matnr.
*
*    CALL FUNCTION 'ZSD_INT_OB_INTEGRA_MATERIAL' STARTING NEW TASK l_task
*      EXPORTING
*        i_matnr = lc_workarea-matnr.
*  ENDIF.
*ENDIF.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
