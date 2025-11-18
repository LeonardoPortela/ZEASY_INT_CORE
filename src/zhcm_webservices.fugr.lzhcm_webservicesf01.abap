*----------------------------------------------------------------------*
***INCLUDE LZHCM_WEBSERVICESF01.
*----------------------------------------------------------------------*

DATA: CK_INFORMOU_SENHA TYPE CHAR01,
      DS_SENHA_01       TYPE ZDE_SENHA_30,
      DS_SENHA_02       TYPE ZDE_SENHA_30,
      OK_CODE           TYPE SY-UCOMM.

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_SENHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_E_SENHA  text
*----------------------------------------------------------------------*
FORM BUSCAR_SENHA  CHANGING E_SENHA TYPE ZDE_SENHA_30.

  CK_INFORMOU_SENHA = ABAP_FALSE.

  CALL SCREEN 0100 STARTING AT 10 08.

  IF CK_INFORMOU_SENHA EQ ABAP_TRUE.
    E_SENHA = DS_SENHA_01.
  ELSE.
    CLEAR E_SENHA.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'DS_SENHA_01' OR SCREEN-NAME EQ 'DS_SENHA_02' .
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  CLEAR: DS_SENHA_01, DS_SENHA_02, CK_INFORMOU_SENHA.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK_CODE.
    WHEN 'CONFIRMAR'.

      IF DS_SENHA_01 NE DS_SENHA_02.
        MESSAGE S013 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF STRLEN( DS_SENHA_01 ) LT 6.
        MESSAGE S014 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CK_INFORMOU_SENHA = ABAP_TRUE.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
