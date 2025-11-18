class ZCX_WEBSERVICE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

*"* public components of class ZCX_WEBSERVICE
*"* do not include other source files here!!!
public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of SERVICO_NAO_ENCONTRADO,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SERVICO_NAO_ENCONTRADO .
  constants:
    begin of URL_NAO_ENCONTRADO,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of URL_NAO_ENCONTRADO .
  constants:
    begin of ARGUMENTO_NAO_ENCONTRADO,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ARGUMENTO_NAO_ENCONTRADO .
  constants:
    begin of PLUGIN_NAO_ATIVO,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PLUGIN_NAO_ATIVO .
  constants:
    begin of ERRO_INTERNO,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERRO_INTERNO .
  constants:
    begin of AUTENTICACAO_NAO_ENCONTRADO,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of AUTENTICACAO_NAO_ENCONTRADO .
  constants:
    begin of ERRO_NO_XML,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERRO_NO_XML .
  constants:
    begin of SEM_ROTA_UPDATE,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEM_ROTA_UPDATE .
  constants:
    begin of ERRO_GRAVAR_DB,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERRO_GRAVAR_DB .
  constants:
    begin of ERRO_VLR_COTACAO,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERRO_VLR_COTACAO .
  constants:
    begin of ZCX_ERRO_GERAL,
      msgid type symsgid value 'ZWEBSERVICE',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_GERAL .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_WEBSERVICE .
protected section.
*"* protected components of class ZCX_WEBSERVICE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_WEBSERVICE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_WEBSERVICE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
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

    RAISE EXCEPTION TYPE ZCX_WEBSERVICE
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_WEBSERVICE=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_WEBSERVICE=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) ).

  ENDMETHOD.
ENDCLASS.
