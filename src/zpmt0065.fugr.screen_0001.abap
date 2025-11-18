PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zpmt0065 CURSOR nextline.
    MODULE liste_show_liste.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zpmt0065-werks .
      FIELD zpmt0065-pernr .
      FIELD zpmt0065-caixa .
      FIELD zpmt0065-criado_por .
      FIELD zpmt0065-modificado_por .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zpmt0065-werks .
      FIELD zpmt0065-pernr .
      FIELD zpmt0065-caixa .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.

  MODULE custom_button_ferramentas.
