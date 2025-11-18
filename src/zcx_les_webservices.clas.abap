class ZCX_LES_WEBSERVICES definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_ERRO_GERAL,
      msgid type symsgid value 'ZLES',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_ERRO_GERAL .
  data MSGTY type SYST_MSGTY .
  data MSGNO type SYST_MSGNO .
  data MSGV1 type SYST_MSGV .
  data MSGV2 type SYST_MSGV .
  data MSGV3 type SYST_MSGV .
  data MSGV4 type SYST_MSGV .
  data MSGID type SYST_MSGID .
  data TRANSACAO type TCODE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGTY type SYST_MSGTY optional
      !MSGNO type SYST_MSGNO optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional
      !MSGID type SYST_MSGID optional
      !TRANSACAO type TCODE optional .
  methods PUBLISHED_ERRO
    importing
      !I_MSGTY type SYST_MSGTY optional
      !I_MSGTY_DISPLAY type SYST_MSGTY optional .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_SOL_MOBILE_RH .
protected section.
private section.
ENDCLASS.



CLASS ZCX_LES_WEBSERVICES IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGTY = MSGTY .
me->MSGNO = MSGNO .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->MSGID = MSGID .
me->TRANSACAO = TRANSACAO .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.

    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_LES_WEBSERVICES
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_LES_WEBSERVICES=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_LES_WEBSERVICES=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_LES_WEBSERVICES=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_LES_WEBSERVICES=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD PUBLISHED_ERRO.

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
