class ZCX_INTEGRACAO definition
  public
  inheriting from ZCX_ERROR
  final
  create public .

public section.

  constants:
    begin of ZCX_ID_INTEGRACAO_NAO_ECONTRAD,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ID_INTEGRACAO_NAO_ECONTRAD .
  constants:
    begin of ZCX_SERVICO_HTTP_CONFIG,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SERVICO_HTTP_CONFIG .
  constants:
    begin of ZCX_SEM_CIDADE_IDIOMA,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_CIDADE_IDIOMA .
  constants:
    begin of ZCX_ERRO_CAD_VEICULO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_CAD_VEICULO .
  constants:
    begin of ZCX_LOTE_FRETE_NOT_FOUND,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LOTE_FRETE_NOT_FOUND .
  constants:
    begin of ZCX_LOTE_FRETE_OV,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_LOTE_FRETE_OV .
  constants:
    begin of ZCX_MSG_DEPENDENTE,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MSG_DEPENDENTE .
  constants:
    begin of ZCX_DEPARA_MATERIAL,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of ZCX_DEPARA_MATERIAL .
  constants:
    begin of ZCX_DEPARA_UNDGOVERNACIONAL,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DEPARA_UNDGOVERNACIONAL .
  constants:
    begin of ZCX_DATA_NASC_MOTORISTA,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DATA_NASC_MOTORISTA .
  constants:
    begin of ZCX_ERRO_BODY_RECEBIDO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_BODY_RECEBIDO .
  constants:
    begin of ZCX_ERRO_CAD_PROPRIETARIO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_CAD_PROPRIETARIO .
  constants:
    begin of ZCX_ERRO_OV_INCOTERMO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_OV_INCOTERMO .
  constants:
    begin of ZCX_SEM_INFO_FRETE_ENT,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '016',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_SEM_INFO_FRETE_ENT .
  constants:
    begin of ZCX_ORDEM_CAR_NOT_EXISTS,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '017',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ORDEM_CAR_NOT_EXISTS .
  constants:
    begin of ZCX_DATA_ERRADA,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '019',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DATA_ERRADA .
  constants:
    begin of ZCX_MATERIAL_NAO_ENCONTRADO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MATERIAL_NAO_ENCONTRADO .
  constants:
    begin of ZCX_DOC_NAO_GERADO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '021',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_DOC_NAO_GERADO .
  constants:
    begin of ZCX_EMPTY_PROTOCOL,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '022',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EMPTY_PROTOCOL .
  constants:
    begin of ZCX_EMPTY_DOCNUM,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '023',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EMPTY_DOCNUM .
  constants:
    begin of ZCX_EMPTY_JUSTIFY,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '024',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EMPTY_JUSTIFY .
  constants:
    begin of ZCX_EMPTY_DT_EMISSAO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '025',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EMPTY_DT_EMISSAO .
  constants:
    begin of ZCX_NOT_IDENTIFY_NF_WRITER,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '026',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_NOT_IDENTIFY_NF_WRITER .
  constants:
    begin of ZCX_EMPTY_PRC_EXTERNAL,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '027',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_EMPTY_PRC_EXTERNAL .
  constants:
    begin of ZCX_ERRO_DT_EMISSAO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '028',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_DT_EMISSAO .
  constants:
    begin of ZCX_ERRO_DT_ESTORNO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '029',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ERRO_DT_ESTORNO .
  constants:
    begin of ZCX_TREINAMENTO_CPF_OBRIGA,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '033',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TREINAMENTO_CPF_OBRIGA .
  constants:
    begin of ZCX_TREINAMENTO_CPF_PADRAO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '034',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TREINAMENTO_CPF_PADRAO .
  constants:
    begin of ZCX_TREINAMENTO_CONTA_ERRO,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '035',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TREINAMENTO_CONTA_ERRO .
  constants:
    begin of ZCX_TREINAMENTO_CURSO_CPF,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '036',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TREINAMENTO_CURSO_CPF .
  constants:
    begin of ZCX_TREINAMENTO_COD_TREINA,
      msgid type symsgid value 'ZINTEGRA',
      msgno type symsgno value '037',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_TREINAMENTO_COD_TREINA .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGID type SYST_MSGID optional
      !MSGNO type SYST_MSGNO optional
      !MSGTY type SYST_MSGTY optional
      !MSGV1 type SYST_MSGV optional
      !MSGV2 type SYST_MSGV optional
      !MSGV3 type SYST_MSGV optional
      !MSGV4 type SYST_MSGV optional
      !TRANSACAO type TCODE optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_INTEGRACAO IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGID = MSGID
MSGNO = MSGNO
MSGTY = MSGTY
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
TRANSACAO = TRANSACAO
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
