CLASS zcx_error_eudr DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES zif_error .

    ALIASES msgid
      FOR zif_error~msgid .
    ALIASES msgno
      FOR zif_error~msgno .
    ALIASES msgty
      FOR zif_error~msgty .
    ALIASES msgv1
      FOR zif_error~msgv1 .
    ALIASES msgv2
      FOR zif_error~msgv2 .
    ALIASES msgv3
      FOR zif_error~msgv3 .
    ALIASES msgv4
      FOR zif_error~msgv4 .
    ALIASES transacao
      FOR zif_error~transacao .

    CONSTANTS:
      BEGIN OF zcx_erro_geral,
        msgty TYPE symsgty VALUE 'E',
        msgid TYPE symsgid VALUE 'ZAPIEUDR',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF zcx_erro_geral .

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !msgid     TYPE syst_msgid OPTIONAL
        !msgno     TYPE syst_msgno OPTIONAL
        !msgty     TYPE syst_msgty OPTIONAL
        !msgv1     TYPE syst_msgv OPTIONAL
        !msgv2     TYPE syst_msgv OPTIONAL
        !msgv3     TYPE syst_msgv OPTIONAL
        !msgv4     TYPE syst_msgv OPTIONAL
        !transacao TYPE tcode OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ERROR_EUDR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGID = MSGID .
me->MSGNO = MSGNO .
me->MSGTY = MSGTY .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->TRANSACAO = TRANSACAO .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD ZIF_ERROR~GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_ERROR
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD ZIF_ERROR~GET_MSG_ERRO.

    MESSAGE ID ME->MSGID TYPE ME->MSGTY NUMBER ME->MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 INTO R_MSG_TEXTO.

  ENDMETHOD.


  METHOD ZIF_ERROR~PUBLISHED_ERRO.

    DATA: P_MSGTY	        TYPE SYST_MSGTY,
          P_MSGTY_DISPLAY	TYPE SYST_MSGTY.


    IF I_MSGTY IS NOT INITIAL.
      P_MSGTY = I_MSGTY.
    ELSE.
      P_MSGTY = ME->MSGTY.
    ENDIF.

    IF I_MSGTY_DISPLAY IS NOT INITIAL.
      P_MSGTY_DISPLAY = I_MSGTY_DISPLAY.
    ELSE.
      P_MSGTY_DISPLAY = ME->MSGTY.
    ENDIF.

    IF ME->TRANSACAO IS NOT INITIAL.
      IF ME->MSGV1 IS INITIAL.
        ME->MSGV1 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV2 IS INITIAL.
        ME->MSGV2 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV3 IS INITIAL.
        ME->MSGV3 = ME->TRANSACAO.
      ENDIF.
      IF ME->MSGV4 IS INITIAL.
        ME->MSGV4 = ME->TRANSACAO.
      ENDIF.
    ENDIF.

    MESSAGE ID ME->MSGID TYPE P_MSGTY NUMBER ME->MSGNO WITH ME->MSGV1 ME->MSGV2 ME->MSGV3 ME->MSGV4 DISPLAY LIKE P_MSGTY_DISPLAY.

  ENDMETHOD.
ENDCLASS.
