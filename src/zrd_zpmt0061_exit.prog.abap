*&---------------------------------------------------------------------*
*& Report  ZRD_zpmt0061_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zpmt0061_exit.

FORM f_exit_zpmt0061_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zpmt0061 TYPE zpmt0061.
  CLEAR:  wa_zpmt0061.

  MOVE-CORRESPONDING p_registro_manter TO wa_zpmt0061.

  CLEAR: p_error.
  IF p_error IS INITIAL.
    IF wa_zpmt0061-uname IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo usuário obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zpmt0061-werks IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Centro obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zpmt0061-werks IS NOT INITIAL.
      SELECT SINGLE werks
          INTO wa_zpmt0061-werks
             FROM t001w
        WHERE werks EQ wa_zpmt0061-werks.
      IF sy-subrc NE 0.
        MESSAGE 'Centro não existe!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_exit_zpmt0061_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zpmt0061 TYPE zpmt0061.
  MOVE-CORRESPONDING p_registro_manter TO wa_zpmt0061.
*  wa_zpmt0061-usnam_cad = sy-uname.
*  wa_zpmt0061-dt_cad = sy-datum.
*  wa_zpmt0061-hr_cad = sy-uzeit.
  MOVE-CORRESPONDING wa_zpmt0061 TO p_registro_manter.

ENDFORM.

FORM f_exit_zpmt0061_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_zpmt0061 TYPE zpmt0061.
  MOVE-CORRESPONDING p_registro_manter TO wa_zpmt0061.
  MOVE-CORRESPONDING wa_zpmt0061 TO p_registro_manter.

ENDFORM.

FORM  f_exit_zpmt0061_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  IF p_db_tab = 'ZPMT0061'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
