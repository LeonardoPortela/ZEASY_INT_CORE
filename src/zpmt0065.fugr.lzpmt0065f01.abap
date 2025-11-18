*----------------------------------------------------------------------*
***INCLUDE LZPMT0065F01.
*----------------------------------------------------------------------*

FORM f_atualiza_info.

  zpmt0065-criado_por = sy-uname.

ENDFORM.

FORM f_atualiza_modificador.

  IF zpmt0065-criado_por IS NOT INITIAL.
    zpmt0065-modificado_por = sy-uname.
  ENDIF.

ENDFORM.
