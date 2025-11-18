class ZCL_INTEGRACAO_ORD_CARREGA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_ORD_CARREGA .

  data T_BDCDATA type BDCDATA_TAB .
  data W_BDCDATA type BDCDATA .
  data CTUMODE type CTU_MODE value 'N' ##NO_TEXT.
  data CUPDATE type CTU_UPDATE value 'S' ##NO_TEXT.
  data T_MESSTAB type TAB_BDCMSGCOLL .
  data W_MESSTAB type BDCMSGCOLL .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  methods SET_CAD_FORNECEDOR_FRETE
    importing
      !I_FORNECEDOR type ZDE_CAD_FONECEDOR
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    exporting
      !E_PARCEIRO type J_1BPARID
      !E_OUTBOUND_FORNE type ZFIE_VENDOR
      !E_MSG type STRING
      !E_MSG_VALIDACAO type STRING
    returning
      value(R_INTEGRACAO_ORD_CARREGA) type ref to ZCL_INTEGRACAO_ORD_CARREGA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_CAD_MOTORISTA
    importing
      !I_FORNECEDOR type ZDE_CAD_FONECEDOR
    exporting
      !E_PARCEIRO type J_1BPARID
      !E_OUTBOUND_FORNE type ZFIE_VENDOR
      !E_MSG_VALIDACAO type STRING
      !E_MSG type STRING
    returning
      value(R_INTEGRACAO_ORD_CARREGA) type ref to ZCL_INTEGRACAO_ORD_CARREGA
    raising
      ZCX_INTEGRACAO
      ZCX_SHDB
      ZCX_ERROR .
  methods SET_CAD_VEICULO
    importing
      !I_VEICULO type ZDE_CAD_VEICULO
      !I_PROPRIETARIO type LIFNR
    exporting
      !E_VEICULO type ZLEST0002
      !E_MSG type STRING
      !E_MSG_VALIDACAO type STRING
    returning
      value(R_INTEGRACAO_ORD_CARREGA) type ref to ZCL_INTEGRACAO_ORD_CARREGA
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  class-methods GET_TXT_HTML_VIAGEM
    importing
      !I_VIAGEM_ID type ZDE_VIAGEM_ID
    returning
      value(R_STRING_HTML) type STRING .
  methods SET_CRIAR_CONDICAO_TK11
    importing
      !I_LOTE_FRETE type ZLEST0181 optional
      !I_VIAGEM_ID type ZLEST0185-VIAGEM_ID optional
      !I_PLACA_VEIC_TRACAO type ZPC_VEICULO optional
      !I_PAGA_TRECHO_TERCEIRO type ZDE_CARGUERO_REQ_DATA-PAGA_TRECHO_TERCEIRO optional
      !I_VALOR_FRETE type ZDE_CARGUERO_VALOR_FRETE_VIA optional
    exporting
      !E_MSG_ERRO type STRING
    raising
      ZCX_ORDEM_VENDA
      ZCX_VEICULOS
      ZCX_INTEGRACAO .
  methods SET_BDC_DYNPRO
    importing
      !I_PROGRAM type CHAR40
      !I_DYNPRO type CHAR40 .
  methods SET_BDC_FIELD
    importing
      !I_FNAM type CHAR40
      !I_FVAL type CHAR40 .
  methods SET_BDC_TRANSACTION
    importing
      !I_TCODE type CHAR20
    raising
      ZCX_INTEGRACAO .
  methods SET_CRIAR_CARGA_ENTRADA
    importing
      !I_ZLEST0185 type ZLEST0185
      !I_LOTE_FRETE type ZLEST0181
      !I_PARCEIRO_TERC type LIFNR
      !I_MOTORISTA_LIFNR type LIFNR
      !I_INBOUND type ZDE_CARGUERO_REQUISICAO
      !IT_VEICULOS type ZDE_MSG_INBOUND_ORDEM_CARREGA
    exporting
      value(E_MSG_ERROR) type STRING
      !E_NRO_CG type ZNRO_CG
    returning
      value(R_SUCESSO) type CHAR01 .
  PROTECTED SECTION.

private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_ORD_CARREGA IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE   = ZIF_INTEGRACAO=>AT_ID_INTERFACE_ORD_CARREGAMEN.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO  = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_INBOUND.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_OPUS = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_OPUS_SIM.
    ME->ZIF_INTEGRACAO_INJECT~AT_SEND_AUTENTICAO = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_SEND_NAO.
    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-TP_REFERENCIA = 'CARGUERO_ORDEM_CARREGAMENTO'.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
  ENDMETHOD.


  METHOD GET_TXT_HTML_VIAGEM.

    DATA: LC_INBOUND TYPE ZDE_CARGUERO_REQUISICAO.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0185TX)
      FROM ZLEST0185TX
     WHERE VIAGEM_ID EQ @I_VIAGEM_ID.

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = WA_ZLEST0185TX-TX_JSON_ENTRADA CHANGING DATA = LC_INBOUND ).

    DATA(LC_LISTA_PROCESSAMENTO) = ZCL_STRING=>HTML_STRING_TO_TABLE_LIST( I_TABLE_STRING = ZCL_STRING=>SPLIT( WA_ZLEST0185TX-TX_PROCESSAMENTO ) I_TIPO_LISTA = '07' ).
    DATA(LC_LISTA_VIAGEM)        = ZCL_STRING=>HTML_STRING_TO_TABLE_LIST( I_TABLE_STRING = ZCL_STRING=>SPLIT( WA_ZLEST0185TX-TX_VALIDACAO_VIAGEM ) I_TIPO_LISTA = '07' ).
    DATA(LC_LISTA_PROP)          = ZCL_STRING=>HTML_STRING_TO_TABLE_LIST( I_TABLE_STRING = ZCL_STRING=>SPLIT( WA_ZLEST0185TX-TX_VALIDACAO_PROP ) I_TIPO_LISTA = '07' ).
    DATA(LC_LISTA_VEICULOS)      = ZCL_STRING=>HTML_STRING_TO_TABLE_LIST( I_TABLE_STRING = ZCL_STRING=>SPLIT( WA_ZLEST0185TX-TX_VALIDACAO_VEICULOS ) I_TIPO_LISTA = '07' ).
    DATA(LC_LISTA_MOT)           = ZCL_STRING=>HTML_STRING_TO_TABLE_LIST( I_TABLE_STRING = ZCL_STRING=>SPLIT( WA_ZLEST0185TX-TX_VALIDACAO_MOT ) I_TIPO_LISTA = '07' ).
    DATA(LC_LISTA_OC)            = ZCL_STRING=>HTML_STRING_TO_TABLE_LIST( I_TABLE_STRING = ZCL_STRING=>SPLIT( WA_ZLEST0185TX-TX_VALIDACAO_OC ) I_TIPO_LISTA = '07' ).
    DATA(LC_LISTA_PRECO)         = ZCL_STRING=>HTML_STRING_TO_TABLE_LIST( I_TABLE_STRING = ZCL_STRING=>SPLIT( WA_ZLEST0185TX-TX_VALIDACAO_PRECO ) I_TIPO_LISTA = '07' ).

    DATA(LC_STYLE) =
      '<style type="text/css">' && SPACE &&
      ' *, *:after, *:before { box-sizing: border-box; } ' && SPACE &&
      ' body{ margin: 0; padding: 0; font-family: Calibri, Tahoma, Arial; line-height: 2.0; color: #444; font-size: 14px; } ' && SPACE &&
      ' p{ padding: 0 10px; font-size: 12px; } ' && SPACE &&
      ' .tabs-container { position: relative; height: 360px; max-width: 98%; margin: 0 auto; }' && SPACE &&
      ' .tabs-container p{ margin: 0; padding: 0; }' && SPACE &&
      ' .tabs-container:after { content: "."; display: block; clear: both; height: 0; font-size: 0; line-height: 0; visibility: none; } ' && SPACE &&
      ' input.tabs { display: none; } ' && SPACE &&
      ' input.tabs + label { line-height: 40px; padding: 0 20px; float: left; background: #444; color: #fff; cursor: pointer; transition: background ease-in-out .3s; } ' &&
      ' input.tabs:checked + label { color: #000; background: #eee; } ' && SPACE &&
      ' input.tabs + label + div { width: 98%; opacity: 0; position: absolute; background: #eee; top: 40px; left: 0; padding: 10px; z-index: -1; transition: opacity ease-in-out .3s; } ' && SPACE &&
      ' input.tabs:checked + label + div { opacity: 1; z-index: 1000; } ' && SPACE &&
      '.table_borda1, .th_borda1, .td_borda1 { border: 1px solid black; border-collapse: collapse; }' && SPACE &&

      ' </style> '.

    DATA(LC_TEXTO_VIAGEM) =
      '<div>' &&
        '<form>' &&
        '  <table style="width:100%">' &&
        '    <tr>' &&
        '      <td> Viagem ID: </td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-VIAGEM_ID && '">' &&
        '      </td>' &&
        '      <td> Status:</td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-STATUS && '">' &&
        '      </td>' &&
        '    </tr>' &&
        '    <tr>' &&
        '      <td> Validade da Ordem de Carregamento: </td>' &&
        '      <td>' &&
        '         <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-VALIDADE_ORDEM_CARREGAMENTO && '">' &&
        '      </td>' &&
        '      <td>Prazo Solicitação Emissão NF Transito: </td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-PRAZO_SOLICITACAO_EMISSAO_NFT && '">' &&
        '      </td>' &&
        '    </tr>' &&
        '  </table>' &&
        '</form>' &&
      '</div>' .

    DATA(LC_TEXTO_CONFIGURA) =
      '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Configuração:</legend>' &&
        '  <table style="width:100%">' &&
        '    <tr>' &&
        '      <td>' &&
        '      <input type="checkbox" readonly="true" name="vehicle1" value="X" ' && SPACE && COND STRING( WHEN LC_INBOUND-DATA-TROCA_NOTA = ABAP_TRUE THEN 'checked') &&'> Troca Nota' &&
        '      </td>' &&
        '      <td>' &&
        '      <input type="checkbox" readonly="true" name="vehicle1" value="X" ' && SPACE && COND STRING( WHEN LC_INBOUND-DATA-PAGA_TRECHO_TERCEIRO = ABAP_TRUE THEN 'checked') &&'> Paga Trecho Terceiro' &&
        '      </td>' &&
        '      <td>' &&
        '      <input type="checkbox" readonly="true" name="vehicle1" value="X" ' && SPACE && COND STRING( WHEN LC_INBOUND-DATA-EMBARQUE_TERCEIRO = ABAP_TRUE THEN 'checked') &&'> Embarque Terceiro' &&
        '      </td>' &&
        '      <td>' &&
        '      <input type="checkbox" readonly="true" name="vehicle1" value="X" ' && SPACE && COND STRING( WHEN LC_INBOUND-DATA-EMITIR_NF_TRANSITO = ABAP_TRUE THEN 'checked') &&'> Emitir Nota Transito' &&
        '      </td>' &&
        '    </tr>' &&
        '  </table>' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>'.

    DATA(LC_TEXTO_OBSERVACOES) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Observações:</legend>' &&
        '  <textarea name="observacoes" readonly="true" cols=100%>' && LC_INBOUND-DATA-OBSERVACOES && '</textarea> ' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>' .

    DATA(LC_TEXTO_EMBARCADOR) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Lote Embarcador:</legend>' &&
        '  id:' &&
        '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-LOTE_EMBARCADOR-ID && '">' &&
        '  referencia_integracao:' &&
        '  <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOTE_EMBARCADOR-REFERENCIA_INTEGRACAO && '">' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>'.

    DATA(LC_TEXTO_TRANSPORTADOR) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Lote Transportador:</legend>' &&
        '  id:' &&
        '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-LOTE_TRANSPORTADOR-ID && '">' &&
        '  referencia_integracao:' &&
        '  <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOTE_TRANSPORTADOR-REFERENCIA_INTEGRACAO && '">' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>'.

    DATA(LC_TEXTO_VALORES) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Valores de Frete:</legend>' &&
        '<table style="width:100%">' &&
        '<tr>' &&
        '  <td>' &&
              '  Modalidade de Pagamento:' &&
        '  </td>' &&
        '  <td>' &&
              '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-VALOR_FRETE-MODALIDADE_PAGAMENTO_FRETE && '">' &&
        '  </td>' &&
        '  <td>' &&
              '  Vlr. Frete Empresa:' &&
        '  </td>' &&
        '  <td>' &&
              '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-VALOR_FRETE-VALOR_FRETE_EMPRESA && '">' &&
        '  </td>' &&
        '  <td>' &&
              '  Vlr. Trecho Fiscal:' &&
        '  </td>' &&
        '  <td>' &&
              '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-VALOR_FRETE-VALOR_FRETE_TRECHO_FISCAL && '">' &&
        '  </td>' &&
        '</tr>' &&
        '<tr>' &&
        '  <td>' &&
        '  Vlr. Trecho Terceiro:' &&
        '  </td>' &&
        '  <td>' &&
        '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-VALOR_FRETE-VALOR_FRETE_TRECHO_TERCEIRO && '">' &&
        '  </td>' &&
        '  <td>' &&
        '  Vlr. Frete Motorista:' &&
        '  </td>' &&
        '  <td>' &&
        '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-VALOR_FRETE-VALOR_FRETE_MOTORISTA && '">' &&
        '  </td>' &&
        '  <td>' &&
        '  </td>' &&
        '</tr>' &&
        '</table>' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>' .

    DATA(LC_TEXTO_PRODUTO) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Produto:</legend>' &&
        '  id:' &&
        '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-LOTE_TRANSPORTADOR-ID && '">' &&
        '  referencia_integracao:' &&
        '  <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOTE_TRANSPORTADOR-REFERENCIA_INTEGRACAO && '">' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>'.

    DATA(LC_TEXTO_PESO_ESTIMADO) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Peso Estimado:</legend>' &&
        '  Quantidade:' &&
        '  <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-PESO_ESTIMADO-QUANTIDADE && '">' &&
        '  Unidade de Medida:' &&
        '  <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-PESO_ESTIMADO-UNIDADE_MEDIDA && '">' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>'.

    DATA(LC_TEXTO_LOCAL_COLETA) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Local de Coleta:</legend>' &&
        '   <table style="width:100%">' &&
        '    <tr>' &&
        '      <td> ID: </td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-LOCAL_COLETA-ID && '">' &&
        '      </td>' &&
        '      <td> referencia_integracao: </td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOCAL_COLETA-REFERENCIA_INTEGRACAO && '">' &&
        '      </td>' &&
        '    </tr>' &&
        '    <tr>' &&
        '      <td>UF:</td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOCAL_COLETA-UF && '">' &&
        '      </td>' &&
        '      <td>Nome:</td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOCAL_COLETA-NOME && '">' &&
        '      </td>' &&
        '    </tr>' &&
        '   </table>' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>'.

    DATA(LC_TEXTO_LOCAL_DESCARGA) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Local de Descarga:</legend>' &&
        '   <table style="width:100%">' &&
        '    <tr>' &&
        '      <td> ID: </td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-LOCAL_DESCARGA-ID && '">' &&
        '      </td>' &&
        '      <td> referencia_integracao: </td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOCAL_DESCARGA-REFERENCIA_INTEGRACAO && '">' &&
        '      </td>' &&
        '    </tr>' &&
        '    <tr>' &&
        '      <td>UF:</td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOCAL_DESCARGA-UF && '">' &&
        '      </td>' &&
        '      <td>Nome:</td>' &&
        '      <td>' &&
        '        <input type="text" readonly="true" name="lastname" value="' && LC_INBOUND-DATA-LOCAL_DESCARGA-NOME && '">' &&
        '      </td>' &&
        '    </tr>' &&
        '   </table>' &&
        '  </fieldset>' &&
        '</form>' &&
      '</div>'.

    DATA(LC_RG_DESCRICAO) =
    ZCL_STRING=>CONCAT(  S1 = LC_INBOUND-DATA-MOTORISTA-RG-NUMERO
                         S2 = ZCL_STRING=>CONCAT( S1 = LC_INBOUND-DATA-MOTORISTA-RG-ORGAO_EMISSOR-NOME
                                                  S2 = LC_INBOUND-DATA-MOTORISTA-RG-ORGAO_EMISSOR-UF
                                                  SP = '/' )
                         SP = ' - ' ).


    DATA(LC_CNH_DESCRICAO) =
    ZCL_STRING=>CONCAT( S1 =
    ZCL_STRING=>CONCAT( S1 =
    ZCL_STRING=>CONCAT( S1 =
    ZCL_STRING=>CONCAT( S1 = LC_INBOUND-DATA-MOTORISTA-CNH-NUMERO
                        S2 = ZCL_STRING=>CONCAT( S1 = LC_INBOUND-DATA-MOTORISTA-CNH-UF S2 = LC_INBOUND-DATA-MOTORISTA-CNH-MUNICIPIO-NOME SP = '/' )
                        SP = '-' )
                        S2 = ZCL_STRING=>CONCAT( S1 = 'Categoria:' S2 = LC_INBOUND-DATA-MOTORISTA-CNH-CATEGORIA SP = SPACE )
                        SP = '-' )
                        S2 = ZCL_STRING=>CONCAT( S1 = 'Dt.Emissão:' S2 = LC_INBOUND-DATA-MOTORISTA-CNH-DATA_EMISSAO SP = SPACE )
                        SP = '-' )
                        S2 = ZCL_STRING=>CONCAT( S1 = 'Dt. Validade:' S2 = LC_INBOUND-DATA-MOTORISTA-CNH-DATA_VALIDADE SP = SPACE )
                        SP = '-' ).

    DATA: LC_MOTORISTA_CONTATOS TYPE STRING.

    IF LC_INBOUND-DATA-MOTORISTA-CONTATOS[] IS NOT INITIAL.
      LC_MOTORISTA_CONTATOS =
        '<table style="width:100%" class="table_borda1">' && SPACE &&
        '  <caption>Contatos</caption>' && SPACE &&
        '  <tr>' && SPACE &&
        '    <th class="th_borda1">Tipo</th>' && SPACE &&
        '    <th class="th_borda1">Nome</th>' && SPACE &&
        '    <th class="th_borda1">Telefone</th>' && SPACE &&
        '  </tr>' && SPACE.
      LOOP AT LC_INBOUND-DATA-MOTORISTA-CONTATOS INTO DATA(WA_CONTATO).
        LC_MOTORISTA_CONTATOS = LC_MOTORISTA_CONTATOS &&
          '  <tr>' && SPACE &&
          '    <td class="td_borda1">' && WA_CONTATO-TIPO     && '</td>' && SPACE &&
          '    <td class="td_borda1">' && WA_CONTATO-NOME     && '</td>' && SPACE &&
          '    <td class="td_borda1">' && WA_CONTATO-TELEFONE && '</td>' && SPACE &&
          '  </tr>'.
      ENDLOOP.
      LC_MOTORISTA_CONTATOS = LC_MOTORISTA_CONTATOS && '</table>'.
    ENDIF.

    DATA(LC_TEXTO_MOTORISTA) =
    '<div>' &&
        '<form>' &&
        '  <fieldset>' &&
        '  <legend>Dados do Motorista:</legend>' &&
        '   <table style="width:100%">' &&

        '    <tr>' &&
        '      <td>CPF:</td>' &&
        '      <td> <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-MOTORISTA-CPF && '"> </td>' &&
        '      <td>Nome:</td>' &&
        '      <td colspan="3"> <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-MOTORISTA-NOME && '"> </td>' &&
        '    </tr>' &&

        '    <tr>' &&
        '      <td>Data Nascimento:</td>' &&
        '      <td> <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-MOTORISTA-DATA_NASCIMENTO && '"> </td>' &&
        '      <td>Nome Pai:</td>' &&
        '      <td colspan="3"> <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-MOTORISTA-FILIACAO-NOME_PAI && '"> </td>' &&
        '    </tr>' &&

        '    <tr>' &&
        '      <td>Sexo:</td>' &&
        '      <td> <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-MOTORISTA-SEXO && '"> </td>' &&
        '      <td>Nome Mãe:</td>' &&
        '      <td colspan="3"> <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-MOTORISTA-FILIACAO-NOME_MAE && '"> </td>' &&
        '    </tr>' &&

        '    <tr>' &&
        '      <td>Fone:</td>' &&
        '      <td> <input type="text" readonly="true" name="firstname" value="' && WA_CONTATO-TELEFONE && '"> </td>' && SPACE &&
        '      <td>Email:</td>' &&
        '      <td colspan="3"> <input type="text" readonly="true" name="firstname" value="' && LC_INBOUND-DATA-MOTORISTA-EMAIL && '"> </td>' &&
        '    </tr>' &&

        '    <tr>' && SPACE &&
        '      <td>Logradouro:</td>' && SPACE &&
        '      <td><input type="text" readonly="true" name="logradouro" value="' && LC_INBOUND-DATA-MOTORISTA-ENDERECO-LOGRADOURO && '"></td>' && SPACE &&
        '      <td>Número:</td>' && SPACE &&
        '      <td><input type="text" readonly="true" name="numero" value="' && LC_INBOUND-DATA-MOTORISTA-ENDERECO-NUMERO && '"></td>' && SPACE &&
        '      <td>Bairro:</td>' && SPACE &&
        '      <td><input type="text" readonly="true" name="bairro" value="' && LC_INBOUND-DATA-MOTORISTA-ENDERECO-BAIRRO && '"></td>  ' && SPACE &&
        '    </tr>' && SPACE &&
        '    ' && SPACE &&
        '    <tr>' && SPACE &&
        '      <td>CEP:</td>' && SPACE &&
        '      <td><input type="text" readonly="true" name="cep" value="' && LC_INBOUND-DATA-MOTORISTA-ENDERECO-CEP && '"></td>    ' && SPACE &&
        '      <td>UF:</td>' && SPACE &&
        '      <td><input type="text" readonly="true" name="uf" value="' && LC_INBOUND-DATA-MOTORISTA-ENDERECO-UF && '"></td>    ' && SPACE &&
        '      <td>Cidade:</td>' && SPACE &&
        '      <td><input type="text" readonly="true" name="cidade" value="' && LC_INBOUND-DATA-MOTORISTA-ENDERECO-MUNICIPIO-NOME && SPACE && '-' && SPACE && LC_INBOUND-DATA-MOTORISTA-ENDERECO-MUNICIPIO-CODIGO_IBGE && '"></td>    ' && SPACE &&
        '    </tr> ' && SPACE &&

        '    <tr>' &&
        '      <td>RG:</td>' &&
        '      <td colspan="5"> <input type="text" readonly="true" name="firstname" value="' && LC_RG_DESCRICAO  && '"> </td>' &&
        '    </tr>' &&

        '    <tr>' &&
        '      <td>CNH:</td>' &&
        '      <td colspan="5"> <input type="text" readonly="true" name="firstname" value="' && LC_CNH_DESCRICAO  && '"> </td>' &&
        '    </tr>' &&

        '   </table>' &&

        LC_MOTORISTA_CONTATOS &&

        '  </fieldset>' &&
        '</form>' &&
      '</div>'.

    DATA(LC_TEXT_CONJ_TRANSP) =
      '<div>' && SPACE &&
      '<form>  ' && SPACE &&
      '<fieldset>  ' && SPACE &&
      '<legend>Conjunto Transportador:</legend>  ' && SPACE &&
      '<table style="width:100%">' && SPACE &&
      '<tr>      ' && SPACE &&
      ' <td>id:</td>      ' && SPACE &&
      ' <td> <input type="text" readonly="true" name="id" value="' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-ID && '"> </td>' && SPACE &&
      ' <td>referencia_integracao:</td>      ' && SPACE &&
      ' <td> <input type="text" readonly="true" name="referencia_integracao" value="' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-REFERENCIA_INTEGRACAO && '"> </td>' && SPACE &&
      '</tr>' && SPACE &&
      '<tr>' && SPACE &&
      ' <td>Qtd. Eixos:</td>' && SPACE &&
      ' <td> <input type="text" readonly="true" name="dnit_eixos" value="' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-EIXOS && '"> </td>' && SPACE &&
      ' <td>Unidades Veiculares:</td>' && SPACE &&
      ' <td> <input type="text" readonly="true" name="dnit_und_veicu" value="' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-UNIDADES_VEICULARES && '"> </td>' && SPACE &&
      '</tr>' && SPACE &&
      '<tr>' && SPACE &&
      ' <td>Comp.Máximo:</td>' && SPACE &&
      ' <td> <input type="text" readonly="true" name="dnit_eixos" value="' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-COMPRIMENTO_MAXIMO && '"> </td> ' && SPACE &&
      ' <td></td>' && SPACE &&
      ' <td></td>' && SPACE &&
      '</tr>' && SPACE &&
      '</table>' && SPACE &&
      '<table style="width:100%">' && SPACE &&
      '<caption>Dados do DNIT</caption>' && SPACE &&
      '<tr>' && SPACE &&
      ' <td>Código:</td>' && SPACE &&
      ' <td> <input type="text" readonly="true" name="dnit_codigo" value="' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-DNIT-CODIGO && '"> </td> ' && SPACE &&
      ' <td>Classe:</td>' && SPACE &&
      '<td> <input type="text" readonly="true" name="dnit_classe" value="' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-DNIT-CLASSE && '"> </td>' && SPACE &&
      '</tr>' && SPACE &&
      '<tr>' && SPACE &&
      ' <td>Caracterização:</td>' && SPACE &&
      ' <td colspan="3">' && SPACE &&
      ' <textarea name="dnit_caract" readonly="true" cols=100%>' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-DNIT-CARACTERIZACAO && '</textarea>' && SPACE &&
      '</tr>' && SPACE &&
      '<tr>' && SPACE &&
      ' <td>Nome:</td>' && SPACE &&
      ' <td colspan="3"> <textarea name="dnit_nome" readonly="true" cols=100%>' && LC_INBOUND-DATA-CONJUNTO_TRANSPORTADOR-DNIT-NOME && '</textarea> </td>' && SPACE &&
      '</tr>' && SPACE &&
      '</table>' && SPACE &&
      '</fieldset>' && SPACE &&
      '</form>' && SPACE &&
      '</div>'.


    DATA(LC_TEXT_TIPO_CARR) =
      '<div>' && SPACE &&
      '<form>' && SPACE &&
      '<fieldset>' && SPACE &&
      '<legend>Tipo da Carroceria:</legend>' && SPACE &&
      '<table style="width:100%">' && SPACE &&
      '<tr>' && SPACE &&
      '<td>id:</td>' && SPACE &&
      '<td> <input type="text" readonly="true" name="id" value="' && LC_INBOUND-DATA-TIPO_CARROCERIA-ID && '"> </td>' && SPACE &&
      '<td>referencia_integracao:</td>' && SPACE &&
      '<td> <input type="text" readonly="true" name="referencia_integracao" value="' && LC_INBOUND-DATA-TIPO_CARROCERIA-REFERENCIA_INTEGRACAO && '"> </td>' && SPACE &&
      '<td>Nome:</td>' && SPACE &&
      '<td> <input type="text" readonly="true" name="nome" value="' && LC_INBOUND-DATA-TIPO_CARROCERIA-NOME && '"> </td>' && SPACE &&
      '</tr>' && SPACE &&
      '</table>' && SPACE &&
      '</fieldset>' && SPACE &&
      '</form>' && SPACE &&
      '</div>'.

    DATA(LC_TEXT_BODY) =
      '<div class="tabs-container"> ' && SPACE &&
      '<input type="radio" name="tabs" class="tabs" id="tab1" checked>' &&
      '<label for="tab1">Mensagens</label>' &&
      '<div>' &&
      '  <p><b>Mensagem Processamento</b></p>' &&
      '  <p>'  && COND STRING( WHEN WA_ZLEST0185TX-TX_PROCESSAMENTO IS INITIAL THEN 'Sem Mensagem de Processamento' ELSE LC_LISTA_PROCESSAMENTO ) &&  '</p>' &&
      '  <br/>' &&
      '  <p><b>Mensagem Viagem</b></p>' &&
      '  <p>'  && COND STRING( WHEN WA_ZLEST0185TX-TX_VALIDACAO_VIAGEM IS INITIAL THEN 'Sem Mensagem de Processamento de Viagem' ELSE LC_LISTA_VIAGEM ) &&  '</p>' &&
      '  <br/>' &&
      '  <p><b>Mensagem Proprietário</b></p>' &&
      '  <p>'  && COND STRING( WHEN WA_ZLEST0185TX-TX_VALIDACAO_PROP IS INITIAL THEN 'Sem Mensagem de Processamento do Proprietário do(s) Veículo(s)' ELSE LC_LISTA_PROP ) &&  '</p>' &&
      '  <br/>' &&
      '  <p><b>Mensagem Veículos</b></p>' &&
      '  <p>'  && COND STRING( WHEN WA_ZLEST0185TX-TX_VALIDACAO_VEICULOS IS INITIAL THEN 'Sem Mensagem de Processamento do(s) Veículo(s)' ELSE LC_LISTA_VEICULOS ) &&  '</p>' &&
      '  <br/>' &&
      '  <p><b>Mensagem Motorista</b></p>' &&
      '  <p>'  && COND STRING( WHEN WA_ZLEST0185TX-TX_VALIDACAO_MOT IS INITIAL THEN 'Sem Mensagem de Processamento do Motorista' ELSE LC_LISTA_MOT ) &&  '</p>' &&
      '  <br/>' &&
      '  <p><b>Mensagem Preço de Frete</b></p>' &&
      '  <p>'  && COND STRING( WHEN WA_ZLEST0185TX-TX_VALIDACAO_PRECO IS INITIAL THEN 'Sem Mensagem de Processamento do Preço de Frete' ELSE LC_LISTA_PRECO ) &&  '</p>' &&
      '  <br/>' &&
      '  <p><b>Mensagem Solicitação Ordem Carregamento OPUS</b></p>' &&
      '  <p>'  && COND STRING( WHEN WA_ZLEST0185TX-TX_VALIDACAO_OC IS INITIAL THEN 'Sem Mensagem de Processamento de Solicitação Ordem Carregamento OPUS' ELSE LC_LISTA_OC ) &&  '</p>' &&
      '</div>' &&

      '<input type="radio" name="tabs" class="tabs" id="tab2" >' &&
      '<label for="tab2">Cadastro</label>' &&
      '<div>' &&
        LC_TEXTO_VIAGEM &&
        LC_TEXTO_CONFIGURA &&
        LC_TEXTO_OBSERVACOES &&
        LC_TEXTO_EMBARCADOR &&
        LC_TEXTO_TRANSPORTADOR &&
        LC_TEXTO_PRODUTO &&
        LC_TEXTO_VALORES &&
        LC_TEXTO_PESO_ESTIMADO &&
        LC_TEXTO_LOCAL_COLETA &&
        LC_TEXTO_LOCAL_DESCARGA &&
        LC_TEXTO_MOTORISTA &&
        LC_TEXT_CONJ_TRANSP &&
        LC_TEXT_TIPO_CARR &&

      '</div>' &&
      '</div>'.

    R_STRING_HTML = ZCL_STRING=>HTML_STRING_INTO_HTML( I_STR = LC_TEXT_BODY I_TXT_HEADER_TITLE = 'Mensagens Viagens CARGUERO' I_TXT_HEADER_STYLE = LC_STYLE ).

  ENDMETHOD.


  METHOD set_bdc_dynpro.

    CLEAR w_bdcdata.
    w_bdcdata-program  = i_program.
    w_bdcdata-dynpro   = i_dynpro.
    w_bdcdata-dynbegin = 'X'.
    APPEND w_bdcdata  TO t_bdcdata.

  ENDMETHOD.


  METHOD set_bdc_field.

    IF i_fval <> '/'.
      CLEAR w_bdcdata.
      w_bdcdata-fnam    = i_fnam.
      w_bdcdata-fval    = i_fval.
      APPEND w_bdcdata TO t_bdcdata.
    ENDIF.

  ENDMETHOD.


  METHOD set_bdc_transaction.

    DATA: l_msgno    TYPE syst_msgno.


    FREE: t_messtab,
          w_messtab.

    CALL TRANSACTION i_tcode USING t_bdcdata
                              MODE ctumode
                              UPDATE cupdate
                     MESSAGES INTO t_messtab.

    READ TABLE t_messtab INTO w_messtab WITH KEY msgtyp = 'E'.

    IF sy-subrc = 0.
      l_msgno    =  w_messtab-msgnr.

      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = w_messtab-msgid
                            msgno = l_msgno
                            attr1 = conv #( w_messtab-msgv1 )
                            attr2 = conv #( w_messtab-msgv2 )
                            attr3 = conv #( w_messtab-msgv3 )
                            attr4 = conv #( w_messtab-msgv4 )
                            )
          msgid  = w_messtab-msgid
          msgno  = l_msgno
          msgv1  = conv #( w_messtab-msgv1 )
          msgv2  = conv #( w_messtab-msgv2 )
          msgv3  = conv #( w_messtab-msgv3 )
          msgv4  = conv #( w_messtab-msgv4 )
          msgty  = 'E'.
    ENDIF.


  ENDMETHOD.


  METHOD set_cad_fornecedor_frete.

    DATA: lc_fornecedor TYPE zde_info_vendor_create,
          lc_msg_erro   TYPE string,
          lc_mensagem   TYPE scx_attrname,
          lc_ktokk      TYPE zde_ktok_range_t.  "*-CS2021000253-06.02.2025-#165752-JT-inicio

    e_msg_validacao = ''.

    r_integracao_ord_carrega = me.

    lc_fornecedor-bukrs = i_bukrs.
    SELECT SINGLE * INTO @DATA(wa_t024w) FROM t024w WHERE werks EQ @i_branch.
    IF sy-subrc IS NOT INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Não encontrado Organizações de compra admissíveis em relação ao centro ' && i_branch sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    lc_fornecedor-name1      = i_fornecedor-ds_razao_social.
    lc_fornecedor-post_code1 = i_fornecedor-ds_logradouro_cep.
    lc_fornecedor-region     = i_fornecedor-ds_logradouro_uf.
    lc_fornecedor-remark     = i_fornecedor-ds_observacao.
    lc_fornecedor-sortl      = 'TRANSPORTE'.
*-CS2021000253-06.02.2025-#165752-JT-inicio
    lc_ktokk               = COND #( WHEN i_fornecedor-ds_doc_cnpj IS NOT INITIAL

                                                                   "THEN VALUE #( BASE lc_ktokk ( sign = 'I' option = 'EQ' ktokk_low = 'ZFFJ' )
                                                                   "                            ( sign = 'I' option = 'EQ' ktokk_low = 'ZFIC' ) )

                                                                   THEN VALUE #( BASE lc_ktokk ( sign = 'I' option = 'NE' ktokk_low = 'ZMOT' ) )

                                                                   ELSE VALUE #( BASE lc_ktokk ( sign = 'I' option = 'EQ' ktokk_low = 'ZFFF' ) )
                                    ).
*-CS2021000253-06.02.2025-#165752-JT-fim

    TRY .
        zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro_cnpj_cpf_ie(
              EXPORTING i_cnpj = CONV #( i_fornecedor-ds_doc_cnpj )
                        i_cpf  = CONV #( i_fornecedor-ds_doc_cpf  )
                        "I_INSC_ESTATUAL = CONV #( I_FORNECEDOR-DS_DOC_IE )
                        i_forne_frete = abap_true                                                                      "*-CS2021000253-06.02.2025-#165752-JT-fim
                        i_ktokk       = lc_ktokk                                                                       "*-CS2021000253-06.02.2025-#165752-JT-fim
*                       i_ktokk       = VALUE #( sign = 'I' option = 'NE' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) ) "*-CS2021000253-06.02.2025-#165752-JT-fim
                        i_agnorar_bloqueio = abap_false  "abap_true                                                    "*-CS2021000253-06.02.2025-#165752-JT-fim
          )->get_id_parceiro( IMPORTING e_parceiro = lc_fornecedor-lifnr ).
      CATCH zcx_parceiros.
    ENDTRY.

    TRY .
        zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro_cnpj_cpf_ie(
                EXPORTING i_cnpj = CONV #( i_fornecedor-ds_doc_cnpj )
                          i_cpf  = CONV #( i_fornecedor-ds_doc_cpf  )
                          "I_INSC_ESTATUAL = CONV #( I_FORNECEDOR-DS_DOC_IE )
            )->get_id_parceiro( IMPORTING e_parceiro = lc_fornecedor-kunnr ).
      CATCH zcx_parceiros.
    ENDTRY.

    IF i_fornecedor-ds_doc_cnpj IS INITIAL AND i_fornecedor-ds_doc_cpf IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado o CPF ou CNPJ do Proprietário do Veículo!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_doc_rntrc IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado o RNTRC do Proprietário do Veículo!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_logradouro_rua IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado a Rua do Logradouro do Proprietário do Veículo!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_logradouro_bairro IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado o Bairro do Logradouro do Proprietário do Veículo!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_logradouro_uf IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado a UF do Logradouro do Proprietário do Veículo!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_logradouro_cidade_ibge IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado o Código do IBGE do Logradouro do Proprietário do Veículo!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

*    IF I_FORNECEDOR-DS_TELEFONE IS INITIAL.
*      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( S1 = E_MSG_VALIDACAO S2 = 'Deve ser Informado o Telefone de Contato do Proprietário do Veículo!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
*    ENDIF.

    IF zcl_string=>length( i_fornecedor-dt_nascimento ) GE 1.

      "Validar Qtd Caracteres
      IF zcl_string=>length( i_fornecedor-dt_nascimento ) LT 10.
        e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Data de Nascimento deve estar no padrão yyyy-mm-dd!' sp = zcl_string=>at_separador_padrao ).
      ENDIF.

      "Validar Padrão de Data
      FIND REGEX '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' IN i_fornecedor-dt_nascimento(10).
      IF sy-subrc IS NOT INITIAL.
        e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Data de Nascimento deve estar no padrão yyyy-mm-dd!' sp = zcl_string=>at_separador_padrao ).
      ENDIF.

    ENDIF.

    IF e_msg_validacao IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_cad_proprietario-msgid
                            msgno = zcx_integracao=>zcx_erro_cad_proprietario-msgno )
          msgid  = zcx_integracao=>zcx_erro_cad_proprietario-msgid
          msgno  = zcx_integracao=>zcx_erro_cad_proprietario-msgno
          msgty  = 'E'.
    ENDIF.

    lc_fornecedor-akont = '0000212003'.
    lc_fornecedor-bahns = i_fornecedor-ds_doc_rntrc.

    IF i_fornecedor-ds_doc_cnpj IS NOT INITIAL.
      lc_fornecedor-ktokk      = 'ZFFJ'.
      lc_fornecedor-stcd1      = i_fornecedor-ds_doc_cnpj.
      lc_fornecedor-stkzn      = abap_false.
      lc_fornecedor-title_medi = 'Empresa'.
    ELSE.
      lc_fornecedor-ktokk = 'ZFFF'.
      lc_fornecedor-stcd2 = i_fornecedor-ds_doc_cpf.
      "LC_FORNECEDOR-STCD4 = WA_VEICULOS-PROPRIETARIO-DS_DOC_RG.
      lc_fornecedor-stenr = i_fornecedor-ds_doc_pis.
      lc_fornecedor-kraus = i_fornecedor-ds_doc_pis. "Comentado 19/06/2023 - Publicar Versao
      lc_fornecedor-stcd5 = i_fornecedor-ds_stcd5.
      IF i_fornecedor-dt_nascimento IS NOT INITIAL.
        "1979-01-30
        lc_fornecedor-gbdat = i_fornecedor-dt_nascimento(4) && i_fornecedor-dt_nascimento+5(2) && i_fornecedor-dt_nascimento+8(2).
      ENDIF.
      lc_fornecedor-profs = i_fornecedor-ds_nome_mae.
      lc_fornecedor-sexkz = COND string( WHEN i_fornecedor-ds_sexo = 'F' THEN '2' ELSE '1' ).
      IF i_fornecedor-ds_doc_rg_uf_emissor IS NOT INITIAL AND i_fornecedor-ds_doc_rg_orgao_emissor IS NOT INITIAL.
        lc_fornecedor-gbort = i_fornecedor-ds_doc_rg_uf_emissor && '-' && i_fornecedor-ds_doc_rg_orgao_emissor.
      ENDIF.

      lc_fornecedor-stkzn = abap_true.
      CASE lc_fornecedor-sexkz.
        WHEN '1'.
          lc_fornecedor-title_medi = 'Senhor'.
        WHEN '2'.
          lc_fornecedor-title_medi = 'Senhora'.
        WHEN OTHERS.
          lc_fornecedor-title_medi = 'Senhor'.
      ENDCASE.

    ENDIF.

    DATA(ds_ie) = i_fornecedor-ds_doc_ie.
    TRANSLATE ds_ie TO UPPER CASE.
    CONDENSE ds_ie NO-GAPS.
    IF ds_ie NE 'ISENTO'.
      lc_fornecedor-stcd3 = i_fornecedor-ds_doc_ie.
        "158549 CS2024001090 BP Valid de dados fiscais PSA - Comentado 11-03-25

        SELECT a~descript,b~valfrom
        FROM setlinet AS a
        inner JOIN setleaf AS b ON b~setname = a~setname
        WHERE a~setname = 'MAGGI_FRETE_INSC_MAX'
        INTO TABLE @DATA(it_set_lines).

        IF sy-subrc = 0.
          IF it_set_lines IS NOT INITIAL.
            READ TABLE it_set_lines INTO DATA(wa_set_lines) WITH KEY VALFROM = lc_fornecedor-region.
            IF sy-subrc = 0.
              IF lc_fornecedor-stcd3 IS NOT INITIAL.
                CONDENSE lc_fornecedor-stcd3 NO-GAPS.
                lc_fornecedor-stcd3 = |{ lc_fornecedor-stcd3 PAD = '0' WIDTH = wa_set_lines-descript ALIGN = RIGHT }|.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        "158549 CS2024001090 BP Valid de dados fiscais PSA - Fim

    ENDIF.
    lc_fornecedor-country    = 'BR'.
    lc_fornecedor-ekorg      = wa_t024w-ekorg.
    lc_fornecedor-dlgrp      = '0001'.

    lc_fornecedor-house_num1 = i_fornecedor-ds_logradouro_nro.
    lc_fornecedor-street     = i_fornecedor-ds_logradouro_rua.
    lc_fornecedor-city2      = i_fornecedor-ds_logradouro_bairro.
    CONCATENATE i_fornecedor-ds_logradouro_uf i_fornecedor-ds_logradouro_cidade_ibge INTO lc_fornecedor-txjcd SEPARATED BY space.
    lc_fornecedor-tel_number = i_fornecedor-ds_telefone.
    lc_fornecedor-mob_number = i_fornecedor-ds_mobile.
    lc_fornecedor-smtp_addr  = i_fornecedor-ds_email.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = 'PT'
      IMPORTING
        output           = lc_fornecedor-langu
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.

    IF sy-subrc IS NOT INITIAL.
      lc_fornecedor-langu = sy-langu.
    ENDIF.
    lc_fornecedor-scacd = '9999'.
    lc_fornecedor-fdgrv = 'A8'.  "A8 F-Fretes F-Fretes
    lc_fornecedor-waers = 'BRL'. "Moeda de Pedido
    lc_fornecedor-zterm = 'Z001'.
    lc_fornecedor-zuawa = '001'.
    lc_fornecedor-zwels = 'ACDEHMSTUV'.
    lc_fornecedor-zgrup = '01'.
    lc_fornecedor-frgrp = '0001'.

*-CS2021000253-26.04.2024-#148923-JT-BUG 148923-inicio
*--------------------------------------------------
* verifica se expande fornecedor
*--------------------------------------------------
    IF lc_fornecedor-lifnr IS NOT INITIAL.
      SELECT SINGLE *
        INTO @DATA(w_lfa1)
        FROM lfa1
       WHERE lifnr = @lc_fornecedor-lifnr.

      SELECT SINGLE *
        INTO @DATA(w_lfb1)
        FROM lfb1
       WHERE lifnr = @lc_fornecedor-lifnr
         AND bukrs = @lc_fornecedor-bukrs.

      IF sy-subrc <> 0.
        "fornecedor empresa modelo """""""""""""""""""""""""""""""""""""""""""""""""""""""
        SELECT SINGLE *
          INTO @DATA(w_lfb1_modelo)
          FROM lfb1
         WHERE lifnr = @lc_fornecedor-lifnr.

        IF sy-subrc <> 0.
          CLEAR w_lfb1_modelo.
          w_lfb1_modelo-lifnr = lc_fornecedor-lifnr.
          w_lfb1_modelo-akont = lc_fornecedor-akont.
          w_lfb1_modelo-fdgrv = lc_fornecedor-fdgrv.
          w_lfb1_modelo-waers = lc_fornecedor-waers.
          w_lfb1_modelo-zterm = lc_fornecedor-zterm.
          w_lfb1_modelo-zuawa = lc_fornecedor-zuawa.
          w_lfb1_modelo-zwels = lc_fornecedor-zwels.
          w_lfb1_modelo-zgrup = lc_fornecedor-zgrup.
          w_lfb1_modelo-frgrp = lc_fornecedor-frgrp.
        ENDIF.

        "Expandir fornecedor """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        zcl_mestre_fornecedor=>get_instance(
               )->expandir_dados_fornecedor( EXPORTING  i_lfb1           = w_lfb1_modelo                   " Mestre de fornecedores (empresa)
                                                        i_lfa1           = w_lfa1                          " Mestre de fornecedores (parte geral)
                                                        i_bukrs          = lc_fornecedor-bukrs             " Empresa
                                             IMPORTING  gs_err_messages  = DATA(w_err_messages_prop)       " Indicador de erro e mensagens do sistema
                                                        gs_succ_messages = DATA(w_succ_messages_prop) ).   " Indicador de sucesso e mensagens do sistema

        IF w_err_messages_prop-is_error = abap_true.
          lc_msg_erro = 'Erro expansão Fornecedor:'.
          LOOP AT w_err_messages_prop-messages INTO DATA(w_message_prop).
            lc_msg_erro = lc_msg_erro &&  w_message_prop-message.
          ENDLOOP.
          zcx_integracao=>zif_error~gera_erro_geral( i_texto = lc_msg_erro ).
        ENDIF.

        "informacoes fornecedor """"""""""""""""""""""""""""""""""""""""""""""""""""
        zcl_fornecedores=>zif_parceiros~get_instance(
           )->set_parceiro(     EXPORTING i_parceiro = lc_fornecedor-lifnr
           )->get_id_parceiro(  IMPORTING e_parceiro = e_parceiro
           )->get_outbound_msg( EXPORTING i_bukrs    = lc_fornecedor-bukrs
                                IMPORTING e_msg      = e_outbound_forne
           ).

        "-------------------------------------------------
        "---Ajustar Impostos Transportadores PF
        "-------------------------------------------------
        zcl_mestre_fornecedor=>get_instance(
               )->ajustar_impostos_fornecedor( EXPORTING i_bukrs    = i_bukrs
                                                         i_lifnr    = CONV #( lc_fornecedor-lifnr )
                                               IMPORTING e_msg_erro = lc_msg_erro
               ).

        IF lc_msg_erro IS NOT INITIAL.
          lc_mensagem = lc_msg_erro.

          RAISE EXCEPTION TYPE zcx_integracao
            EXPORTING
              textid = VALUE #( attr1 = lc_mensagem )
              msgid  = zcx_integracao=>zcx_erro_cad_proprietario-msgid
              msgno  = zcx_integracao=>zcx_erro_cad_proprietario-msgno
              msgty  = 'E'.
        ENDIF.

        RETURN.
      ENDIF.
    ENDIF.
*-CS2021000253-26.04.2024-#148923-JT-BUG 148923-fim

    TRY .

        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_create_partiner( EXPORTING i_data        = lc_fornecedor
                                            i_forne_frete = abap_true              "*-CS2021000253-06.02.2025-#165752-JT-fim
                                  IMPORTING e_msg         = e_msg e_outbound_forne = e_outbound_forne
          )->get_id_parceiro(     IMPORTING e_parceiro    = e_parceiro
          ).

      CATCH zcx_parceiros INTO DATA(ex_parceiros).    "
        e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = ex_parceiros->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = ex_parceiros->zif_error~get_msg_erro( ) ).
      CATCH zcx_error INTO DATA(ex_error).    " .
        e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = ex_error->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = ex_error->zif_error~get_msg_erro( ) ).
    ENDTRY.

*-CS2022000015 - 22.03.2022 - JT - inicio
*-------------------------------------------------
*---Ajustar Impostos Transportadores PF
*-------------------------------------------------
    zcl_mestre_fornecedor=>get_instance(
           )->ajustar_impostos_fornecedor( EXPORTING i_bukrs    = i_bukrs
                                                     i_lifnr    = e_parceiro
                                           IMPORTING e_msg_erro = lc_msg_erro
           ).

    IF lc_msg_erro IS NOT INITIAL.
      lc_mensagem = lc_msg_erro.

      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( attr1 = lc_mensagem )
          msgid  = zcx_integracao=>zcx_erro_cad_proprietario-msgid
          msgno  = zcx_integracao=>zcx_erro_cad_proprietario-msgno
          msgty  = 'E'.
    ENDIF.
*-CS2022000015 - 22.03.2022 - JT - fim

  ENDMETHOD.


  METHOD set_cad_motorista.

    DATA: lc_fornecedor TYPE zde_info_vendor_create,
          lc_msg_erro   TYPE string.  "*-CS2021000253-26.04.2024-#148923-JT-BUG 148923

    r_integracao_ord_carrega = me.

    lc_fornecedor-bukrs = '0001'.
    DATA(lc_werks)      = '0101'.

    SELECT SINGLE * INTO @DATA(wa_t024w) FROM t024w WHERE werks EQ @lc_werks.
    lc_fornecedor-name1      = i_fornecedor-ds_razao_social.
    lc_fornecedor-post_code1 = i_fornecedor-ds_logradouro_cep.
    lc_fornecedor-region     = i_fornecedor-ds_logradouro_uf.
    lc_fornecedor-remark     = i_fornecedor-ds_observacao.
    lc_fornecedor-sortl      = 'MOTORISTA'.

    TRY .
        zcl_fornecedores=>zif_parceiros~get_instance( )->set_parceiro_cnpj_cpf_ie(
              EXPORTING i_cnpj  = CONV #( i_fornecedor-ds_doc_cnpj )
                        i_cpf   = CONV #( i_fornecedor-ds_doc_cpf  )
                        i_insc_estatual = CONV #( i_fornecedor-ds_doc_ie )
                        i_ktokk = VALUE #( sign = 'I' option = 'EQ' ( ktokk_low = 'ZMOT' ktokk_high = 'ZMOT' ) )
                        i_agnorar_bloqueio = abap_true
          )->get_id_parceiro( IMPORTING e_parceiro = lc_fornecedor-lifnr ).
      CATCH zcx_parceiros.
    ENDTRY.

    TRY .
        zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro_cnpj_cpf_ie(
                EXPORTING i_cnpj = CONV #( i_fornecedor-ds_doc_cnpj )
                          i_cpf  = CONV #( i_fornecedor-ds_doc_cpf  )
                          i_insc_estatual = CONV #( i_fornecedor-ds_doc_ie )
            )->get_id_parceiro( IMPORTING e_parceiro = lc_fornecedor-kunnr ).
      CATCH zcx_parceiros.
    ENDTRY.


    IF i_fornecedor-ds_doc_cpf IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado o CPF do Motorista!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_logradouro_rua IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado a Rua do Logradouro do Motorista!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_logradouro_bairro IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado o Bairro do Logradouro do Motorista!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_logradouro_uf IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado a UF do Logradouro do Motorista!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_logradouro_cidade_ibge IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado o Código do IBGE do Logradouro do Motorista!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF i_fornecedor-ds_telefone IS INITIAL.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado o Telefone de Contato do Motorista!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF zcl_string=>length( i_fornecedor-dt_nascimento ) GE 1.

      "Validar Qtd Caracteres
      IF zcl_string=>length( i_fornecedor-dt_nascimento ) LT 10.
        e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Data de Nascimento do Motorista, deve estar no padrão yyyy-mm-dd!' sp = zcl_string=>at_separador_padrao ).
      ENDIF.

      "Validar Padrão de Data
      FIND REGEX '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' IN i_fornecedor-dt_nascimento(10).
      IF sy-subrc IS NOT INITIAL.
        e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Data de Nascimento do Motorista, deve estar no padrão yyyy-mm-dd!' sp = zcl_string=>at_separador_padrao ).
      ENDIF.
    ELSE.
      e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = 'Deve ser Informado a Data de Nascimento do Motorista!' sp = zcl_string=>at_separador_padrao ).
    ENDIF.

    IF e_msg_validacao IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_cad_proprietario-msgid
                            msgno = zcx_integracao=>zcx_erro_cad_proprietario-msgno )
          msgid  = zcx_integracao=>zcx_erro_cad_proprietario-msgid
          msgno  = zcx_integracao=>zcx_erro_cad_proprietario-msgno
          msgty  = 'E'.
    ENDIF.

    lc_fornecedor-akont = '0000212003'.
    lc_fornecedor-bahns = i_fornecedor-ds_doc_rntrc.
    lc_fornecedor-ktokk = 'ZMOT'.
    lc_fornecedor-stcd2 = i_fornecedor-ds_doc_cpf.
    lc_fornecedor-stcd3 = i_fornecedor-ds_doc_rg.
    lc_fornecedor-stcd4 = i_fornecedor-ds_doc_cnh.
    lc_fornecedor-stenr = i_fornecedor-ds_doc_pis.
    lc_fornecedor-kraus = i_fornecedor-ds_doc_pis. "Comentado 19/06/2023 - CS2023000160 - Publicar PRD depois
    "1979-01-30
    lc_fornecedor-gbdat = i_fornecedor-dt_nascimento(4) && i_fornecedor-dt_nascimento+5(2) && i_fornecedor-dt_nascimento+8(2).
    "I_FORNECEDOR-DT_NASCIMENTO(2) && I_FORNECEDOR-DT_NASCIMENTO+3(2) && I_FORNECEDOR-DT_NASCIMENTO+6(4).
    lc_fornecedor-profs = i_fornecedor-ds_nome_mae.
    lc_fornecedor-sexkz = COND string( WHEN i_fornecedor-ds_sexo = 'F' THEN '2' ELSE '1' ).
    lc_fornecedor-gbort = i_fornecedor-ds_doc_rg_uf_emissor && '-' && i_fornecedor-ds_doc_rg_orgao_emissor.
    lc_fornecedor-stkzn = abap_true.
    CASE lc_fornecedor-sexkz.
      WHEN '1'.
        lc_fornecedor-title_medi = 'Senhor'.
      WHEN '2'.
        lc_fornecedor-title_medi = 'Senhora'.
      WHEN OTHERS.
        lc_fornecedor-title_medi = 'Senhor'.
    ENDCASE.

    DATA(ds_ie) = i_fornecedor-ds_doc_ie.
    TRANSLATE ds_ie TO UPPER CASE.
    CONDENSE ds_ie NO-GAPS.
    IF ds_ie NE 'ISENTO'.
      lc_fornecedor-stcd3 = i_fornecedor-ds_doc_ie.
    ENDIF.

    lc_fornecedor-country    = 'BR'.
    lc_fornecedor-ekorg      = wa_t024w-ekorg.
    lc_fornecedor-dlgrp      = '0001'.

    lc_fornecedor-house_num1 = i_fornecedor-ds_logradouro_nro.
    lc_fornecedor-street     = i_fornecedor-ds_logradouro_rua.
    lc_fornecedor-city2      = i_fornecedor-ds_logradouro_bairro.
    CONCATENATE i_fornecedor-ds_logradouro_uf i_fornecedor-ds_logradouro_cidade_ibge INTO lc_fornecedor-txjcd SEPARATED BY space.
    lc_fornecedor-tel_number = i_fornecedor-ds_telefone.
    lc_fornecedor-mob_number = i_fornecedor-ds_mobile.
    lc_fornecedor-smtp_addr  = i_fornecedor-ds_email.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input            = 'PT'
      IMPORTING
        output           = lc_fornecedor-langu
      EXCEPTIONS
        unknown_language = 1
        OTHERS           = 2.

    IF sy-subrc IS NOT INITIAL.
      lc_fornecedor-langu = sy-langu.
    ENDIF.
    "LC_FORNECEDOR-SCACD = '9999'.
    lc_fornecedor-fdgrv = 'A8'.  "A8 F-Fretes F-Fretes
    lc_fornecedor-waers = 'BRL'. "Moeda de Pedido
    lc_fornecedor-zterm = 'Z001'.
    lc_fornecedor-eikto = i_fornecedor-ds_logradouro_uf.
    lc_fornecedor-zsabe = i_fornecedor-ds_logradouro_uf.
    lc_fornecedor-zuawa = '001'.
    lc_fornecedor-zwels = 'ACDEHMSTUV'.
    lc_fornecedor-zgrup = '01'.
    lc_fornecedor-frgrp = '0001'.

*-CS2021000253-26.04.2024-#148923-JT-BUG 148923-inicio
*--------------------------------------------------
* verifica se expande fornecedor
*--------------------------------------------------
    IF lc_fornecedor-lifnr IS NOT INITIAL.
      SELECT SINGLE *
        INTO @DATA(w_lfa1)
        FROM lfa1
       WHERE lifnr = @lc_fornecedor-lifnr.

      SELECT SINGLE *
        INTO @DATA(w_lfb1)
        FROM lfb1
       WHERE lifnr = @lc_fornecedor-lifnr
         AND bukrs = @lc_fornecedor-bukrs.

      IF sy-subrc <> 0.
        "fornecedor empresa modelo """""""""""""""""""""""""""""""""""""""""""""""""""""""
        SELECT SINGLE *
          INTO @DATA(w_lfb1_modelo)
          FROM lfb1
         WHERE lifnr = @lc_fornecedor-lifnr.

        IF sy-subrc <> 0.
          CLEAR w_lfb1_modelo.
          w_lfb1_modelo-lifnr = lc_fornecedor-lifnr.
          w_lfb1_modelo-akont = lc_fornecedor-akont.
          w_lfb1_modelo-fdgrv = lc_fornecedor-fdgrv.
          w_lfb1_modelo-waers = lc_fornecedor-waers.
          w_lfb1_modelo-zterm = lc_fornecedor-zterm.
          w_lfb1_modelo-zuawa = lc_fornecedor-zuawa.
          w_lfb1_modelo-zwels = lc_fornecedor-zwels.
          w_lfb1_modelo-zgrup = lc_fornecedor-zgrup.
          w_lfb1_modelo-frgrp = lc_fornecedor-frgrp.
        ENDIF.

        "Expandir fornecedor """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        zcl_mestre_fornecedor=>get_instance(
               )->expandir_dados_fornecedor( EXPORTING  i_lfb1           = w_lfb1_modelo                   " Mestre de fornecedores (empresa)
                                                        i_lfa1           = w_lfa1                          " Mestre de fornecedores (parte geral)
                                                        i_bukrs          = lc_fornecedor-bukrs             " Empresa
                                             IMPORTING  gs_err_messages  = DATA(w_err_messages_prop)       " Indicador de erro e mensagens do sistema
                                                        gs_succ_messages = DATA(w_succ_messages_prop) ).   " Indicador de sucesso e mensagens do sistema

        IF w_err_messages_prop-is_error = abap_true.
          lc_msg_erro = 'Erro expansão Fornecedor:'.
          LOOP AT w_err_messages_prop-messages INTO DATA(w_message_prop).
            lc_msg_erro = lc_msg_erro &&  w_message_prop-message.
          ENDLOOP.
          zcx_integracao=>zif_error~gera_erro_geral( i_texto = lc_msg_erro ).
        ENDIF.

        "informacoes fornecedor """"""""""""""""""""""""""""""""""""""""""""""""""""
        zcl_fornecedores=>zif_parceiros~get_instance(
           )->set_parceiro(     EXPORTING i_parceiro = lc_fornecedor-lifnr
           )->get_id_parceiro(  IMPORTING e_parceiro = e_parceiro
           )->get_outbound_msg( EXPORTING i_bukrs    = lc_fornecedor-bukrs
                                IMPORTING e_msg      = e_outbound_forne
           ).

        RETURN.
      ENDIF.
    ENDIF.
*-CS2021000253-26.04.2024-#148923-JT-BUG 148923-fim

    TRY .
        zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_create_partiner( EXPORTING i_data = lc_fornecedor IMPORTING e_msg = e_msg e_outbound_forne = e_outbound_forne
          )->get_id_parceiro( IMPORTING e_parceiro = e_parceiro
          ).

        IF e_outbound_forne-id_fornecedor IS NOT INITIAL AND e_outbound_forne-empresa IS INITIAL.

          SELECT SINGLE * INTO @DATA(wa_lfb1)
            FROM lfb1
           WHERE lifnr EQ @e_outbound_forne-id_fornecedor.

          IF sy-subrc IS INITIAL.
            e_outbound_forne-empresa           = wa_lfb1-bukrs.
            e_outbound_forne-cn_reconciliacao  = wa_lfb1-akont.
            e_outbound_forne-ch_ordenacao      = wa_lfb1-zuawa.
            e_outbound_forne-gr_administracao  = wa_lfb1-fdgrv.
            e_outbound_forne-id_sigam          = wa_lfb1-altkn.
            e_outbound_forne-ch_pagamento      = wa_lfb1-zterm.
            e_outbound_forne-fr_pagamento      = wa_lfb1-zwels.
            e_outbound_forne-bl_empresa        = wa_lfb1-sperr.
            e_outbound_forne-el_empresa        = wa_lfb1-loevm.
            e_outbound_forne-el_empresa_dados  = wa_lfb1-nodel.
          ENDIF.

        ENDIF.

      CATCH zcx_parceiros INTO DATA(ex_parceiros).    "
        e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = ex_parceiros->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = ex_parceiros->zif_error~get_msg_erro( ) ).
      CATCH zcx_shdb INTO DATA(ex_shdb).

        RAISE EXCEPTION TYPE zcx_shdb
          EXPORTING
            textid = VALUE #( msgid = ex_shdb->msgid
                              msgno = ex_shdb->msgno
                              attr1 = CONV #( ex_shdb->msgv1 )
                              attr2 = CONV #( ex_shdb->msgv2 )
                              attr3 = CONV #( ex_shdb->msgv3 )
                              attr4 = CONV #( ex_shdb->msgv4 ) )
            msgid  = ex_shdb->msgid
            msgno  = ex_shdb->msgno
            msgty  = 'E'
            msgv1  = ex_shdb->msgv1
            msgv2  = ex_shdb->msgv2
            msgv3  = ex_shdb->msgv3
            msgv4  = ex_shdb->msgv4.

      CATCH zcx_error INTO DATA(ex_error).    " .
        e_msg_validacao = zcl_string=>concat( s1 = e_msg_validacao s2 = ex_error->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
        zcx_integracao=>zif_error~gera_erro_geral( i_texto = ex_error->zif_error~get_msg_erro( ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD SET_CAD_VEICULO.

    R_INTEGRACAO_ORD_CARREGA = ME.

    CLEAR: E_VEICULO, E_MSG_VALIDACAO.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0002)
      FROM ZLEST0002
     WHERE PC_VEICULO EQ @I_VEICULO-DS_PLACA.

    DATA(LC_EXISTE) = COND STRING( WHEN SY-SUBRC IS INITIAL THEN ABAP_TRUE ELSE ABAP_FALSE ).
    DATA(WA_ZLEST0002_ALTEROU) = WA_ZLEST0002.
    WA_ZLEST0002-PC_VEICULO   = I_VEICULO-DS_PLACA.
    WA_ZLEST0002-PROPRIETARIO = I_PROPRIETARIO.
    CONCATENATE I_VEICULO-DS_UF I_VEICULO-DS_CIDADE_IBGE INTO WA_ZLEST0002-TAXJURCODE SEPARATED BY SPACE.
    WA_ZLEST0002-KALSM        = 'TAXBRA'.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        INPUT            = 'PT'
      IMPORTING
        OUTPUT           = WA_ZLEST0002-SPRAS
      EXCEPTIONS
        UNKNOWN_LANGUAGE = 1
        OTHERS           = 2.

    IF SY-SUBRC IS NOT INITIAL.
      WA_ZLEST0002-SPRAS = SY-LANGU.
    ENDIF.

    SELECT SINGLE * INTO @DATA(WA_J_1BTXJURT)
      FROM J_1BTXJURT
     WHERE SPRAS      EQ @WA_ZLEST0002-SPRAS
       AND COUNTRY    EQ 'BR'
       AND TAXJURCODE EQ @WA_ZLEST0002-TAXJURCODE.

    IF SY-SUBRC IS NOT INITIAL.
      CONCATENATE 'Cidade' WA_ZLEST0002-TAXJURCODE 'não encontrada para o proprietário do veículo' I_VEICULO-DS_PLACA
             INTO DATA(TX_CIDADE_VEICULO) SEPARATED BY SPACE.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = TX_CIDADE_VEICULO SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    WA_ZLEST0002-CD_RENAVAM = I_VEICULO-DS_RENAVAM.
    WA_ZLEST0002-CD_CIDADE  = WA_J_1BTXJURT-TEXT.
    WA_ZLEST0002-ANO        = I_VEICULO-DS_ANO.
    WA_ZLEST0002-CD_UF      = WA_J_1BTXJURT-TAXJURCODE(2).
    WA_ZLEST0002-CHASSI     = I_VEICULO-DS_CHASSI.
    WA_ZLEST0002-TARA       = I_VEICULO-DS_TARA.
    WA_ZLEST0002-CAP_KG     = I_VEICULO-DS_CAP_KG.
    WA_ZLEST0002-CAP_M3     = I_VEICULO-DS_CAP_M3.
    WA_ZLEST0002-QT_EIXO    = I_VEICULO-DS_QTD_EIXOS.
    WA_ZLEST0002-ERDAT      = SY-DATUM.
    WA_ZLEST0002-ERZET      = SY-UZEIT.
    WA_ZLEST0002-ERNAM      = SY-UNAME.
    WA_ZLEST0002-COUNTRY    = 'BR'.
    WA_ZLEST0002-PSTLZ      = I_VEICULO-DS_CEP.

    "0 - Não / 1 - Sim

    WA_ZLEST0002-CTO_COMODATO    = I_VEICULO-DS_COMODATO.
    "CTO_COMODATO
    "1  Sim
    "2  Não

    "0 - Não / 1 - Sim
    WA_ZLEST0002-AGREGADO        = COND STRING( WHEN I_VEICULO-DS_AGREGADO = '0' THEN '2' ELSE '1' ).
    "AGREGADO
    "1  Sim
    "2  Não

    "0 - Tracao / 1 - Reboque
    WA_ZLEST0002-TP_VEICULO      = COND STRING( WHEN I_VEICULO-DS_TP_VEICULO = '0' THEN '0' ELSE '1' ).
    "TP_VEICULO
    "0  Tração
    "1  Reboque

    WA_ZLEST0002-CT_VEICULO      = COND STRING( WHEN I_VEICULO-DS_TP_VEICULO = '0' THEN '1' ELSE '2' ).
    "CT_VEICULO 2
    "1  Veiculo(Trator)
    "2  Carreta(Reboque)
    "3  Empurradores(Navegação)
    "4  Doli

    "00 Não aplicável / 01  Aberta / 02 Fechada/Baú / 03  Granelera / 04  Porta Container / 05  Sider / 06 tanque
    WA_ZLEST0002-TP_CARROCERIA   = COND STRING( WHEN I_VEICULO-DS_TP_CARROCERIA = '03' THEN '2'
                                                when I_VEICULO-DS_TP_CARROCERIA = '06' THEN '3'
                                                ELSE '1' ).
    "TP_CARROCERIA
    "1  Basculante
    "2  Graneleiro
    "3  Tanque

    WA_ZLEST0002-TP_CARROCERIA2 = I_VEICULO-DS_TP_CARROCERIA.
    IF I_VEICULO-DS_TP_CARROCERIA EQ '06'. "Tanque
      WA_ZLEST0002-TP_CARROCERIA2 = '00'.
    ENDIF.

    "TP_CARROCERIA2 00
    "00	Não aplicável
    "01	Aberta
    "02	Fechada/Baú
    "03	Granelera
    "04	Porta Container
    "05	Sider

    IF WA_ZLEST0002-TP_VEICULO = '0'.
      WA_ZLEST0002-TP_RODADO = I_VEICULO-DS_TP_RODADO.
    ELSE.
      WA_ZLEST0002-TP_RODADO = '00'.
    ENDIF.
    "TP_RODADO  00
    "00	Não aplicável
    "01	Truck
    "02	Toco
    "03	Cavalo Mecânico
    "04	Van
    "05	Utilitario
    "06	Outros

    WA_ZLEST0002-TP_PROPRIETARIO = I_VEICULO-DS_TIPO_PROPRIETARIO.
    "TP_PROPRIETARIO
    ""0  CTC 1 ETC 2 ETC-EQUIPARADO 3  TAC

    FIND REGEX '[A-Z]{3}[0-9]{1}[A-Z]{1}[0-9]{2}' IN WA_ZLEST0002-PC_VEICULO.
    IF SY-SUBRC IS NOT INITIAL.
      FIND REGEX '[A-Z]{3}[0-9]{4}' IN WA_ZLEST0002-PC_VEICULO.
      IF SY-SUBRC IS NOT INITIAL.
        E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'Placa precisa ter o formato LLLNLNN ou LLLNNNN! Onde L é letra e N é número.' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
      ENDIF.
    ENDIF.

    IF WA_ZLEST0002-PC_VEICULO IS INITIAL.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'A placa do veículo deve ser informada!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF WA_ZLEST0002-PROPRIETARIO IS INITIAL.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'O proprietário deve ser informado!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF WA_ZLEST0002-PROPRIETARIO IS NOT INITIAL AND WA_ZLEST0002-PC_VEICULO IS NOT INITIAL.
      ZCL_WEBSERVICE_TIPCARD=>CONS_SITUACAO_TRANSPORTADOR(
        EXPORTING
          I_PARTINER       = WA_ZLEST0002-PROPRIETARIO    " Contratado
          I_PLACA          = WA_ZLEST0002-PC_VEICULO    " Placa veículo
        RECEIVING
          E_CONSULTAS      = DATA(E_CONSULTAS)   " Tabela de Consultas Transportador
        EXCEPTIONS
          ERRO             = 1
          WEBSERVICE       = 2
          OTHERS           = 3 ).

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO DATA(MSG_TEXTO).
        E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = MSG_TEXTO SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
      ELSE.
        READ TABLE E_CONSULTAS INDEX 1 INTO DATA(WA_CONSULTAS).
        IF WA_CONSULTAS-CK_RNTRC_ATIVO EQ ABAP_FALSE AND SY-SUBRC IS INITIAL.
          SY-MSGV1 = WA_CONSULTAS-DS_MSG_TRANSPORTADOR+000(50).
          SY-MSGV2 = WA_CONSULTAS-DS_MSG_TRANSPORTADOR+050(50).
          SY-MSGV3 = WA_CONSULTAS-DS_MSG_TRANSPORTADOR+100(50).
          SY-MSGV4 = WA_CONSULTAS-DS_MSG_TRANSPORTADOR+150(50).
          MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO MSG_TEXTO.
          E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = MSG_TEXTO SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
        ENDIF.

        IF SY-SUBRC IS INITIAL.
          IF WA_ZLEST0002-QT_EIXO NE WA_CONSULTAS-QT_EIXOS AND WA_CONSULTAS-QT_EIXOS IS NOT INITIAL.
            MESSAGE S138(ZLES) INTO MSG_TEXTO.
            E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = MSG_TEXTO SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
      EXPORTING
        P_KOART      = 'K'
        P_FORNECEDOR = WA_ZLEST0002-PROPRIETARIO
      EXCEPTIONS
        ERROR        = 1
        OTHERS       = 2.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO MSG_TEXTO.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = MSG_TEXTO SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF WA_ZLEST0002-COUNTRY IS INITIAL.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'O país deve ser informado!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF WA_ZLEST0002-CHASSI IS INITIAL.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'O Numero do Chassi deve ser informado!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF WA_ZLEST0002-CD_RENAVAM IS INITIAL.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'O Código do Renavam deve ser informado!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF WA_ZLEST0002-CD_UF IS INITIAL.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'A Região deve ser informada!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF WA_ZLEST0002-CTO_COMODATO IS INITIAL.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'O Campo Contrato de Comodato deve ser informada!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF WA_ZLEST0002-TP_PROPRIETARIO IS INITIAL.
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'O Campo Tipo do Proprietário deve ser informada!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF NOT ( WA_ZLEST0002-CT_VEICULO IS INITIAL ).
      CASE WA_ZLEST0002-CT_VEICULO.
        WHEN: '1'.
          IF ( WA_ZLEST0002-TP_VEICULO NE '0' ).
            E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'O tipo do veículo deve ser Tração!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
          ENDIF.
        WHEN: '2'.
          IF ( WA_ZLEST0002-TP_VEICULO NE '1' ).
            E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'O tipo do veículo deve ser Reboque!' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
          ENDIF.
      ENDCASE.
    ENDIF.

    IF ( WA_ZLEST0002-QT_EIXO > 4 ).
      E_MSG_VALIDACAO = ZCL_STRING=>CONCAT( EXPORTING S1 = E_MSG_VALIDACAO S2 = 'Quantidade de eixos incorreto.' SP = ZCL_STRING=>AT_SEPARADOR_PADRAO ).
    ENDIF.

    IF E_MSG_VALIDACAO IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ERRO_CAD_VEICULO-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_ERRO_CAD_VEICULO-MSGNO
                            ATTR1 = CONV #( WA_ZLEST0002-PC_VEICULO ) )
          MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_CAD_VEICULO-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_CAD_VEICULO-MSGNO
          MSGV1  = CONV #( WA_ZLEST0002-PC_VEICULO )
          MSGTY  = 'E'.
    ENDIF.

    IF LC_EXISTE EQ ABAP_TRUE.
      IF WA_ZLEST0002 = WA_ZLEST0002_ALTEROU.
        CONCATENATE 'Sem alteração no Veículo' WA_ZLEST0002-PC_VEICULO INTO E_MSG SEPARATED BY SPACE.
        CONCATENATE E_MSG '!' INTO E_MSG.
        E_VEICULO = WA_ZLEST0002.
        EXIT.
      ENDIF.
      WA_ZLEST0002-DT_MODIFICACAO  = SY-DATUM.
      WA_ZLEST0002-USR_MODIFICACAO = SY-UNAME.
    ENDIF.

    MODIFY ZLEST0002 FROM WA_ZLEST0002.
    COMMIT WORK.
    E_VEICULO = WA_ZLEST0002.
    IF LC_EXISTE EQ ABAP_FALSE.
      CONCATENATE 'Veículo' WA_ZLEST0002-PC_VEICULO ' incluído com sucesso!' INTO E_MSG SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'Veículo' WA_ZLEST0002-PC_VEICULO ' alterado com sucesso!' INTO E_MSG SEPARATED BY SPACE.
    ENDIF.

  ENDMETHOD.


  METHOD SET_CRIAR_CONDICAO_TK11.

    DATA: L_FRETE                     TYPE ZDE_INFO_FRETE,
          L_ADD01(4)                  TYPE N,
          L_VALOR                     TYPE CHAR20,
          L_VALOR_FRETE_TERCEIRO      TYPE KBETR_KOND,
          L_VALOR_FRETE_TRECHO_FISCAL TYPE KBETR_KOND,
          L_TO(2)                     TYPE C,
          L_ATIVA                     TYPE C.

    DATA: LIT_A942_SEL TYPE TABLE OF A942.

    FREE: T_BDCDATA,
          L_FRETE,
          L_TO,
          E_MSG_ERRO,
          L_ATIVA,
          LIT_A942_SEL[].

*-----------------------------------------------
* Ativa/desativa EXIT
*-----------------------------------------------
    SELECT HIGH
      INTO L_ATIVA
      FROM TVARVC
        UP TO 1 ROWS
     WHERE NAME = 'ZMM_CAD_FRETE_CARGUERO'
       AND LOW  = I_LOTE_FRETE-BUKRS.
    ENDSELECT.

    IF SY-SUBRC <> 0.
      L_ATIVA = ABAP_TRUE.
    ENDIF.

    CHECK L_ATIVA = ABAP_TRUE.

*--------------------------------------
* --ordem de venda - Tipo de transporte
*--------------------------------------
    IF I_LOTE_FRETE-VBELN IS NOT INITIAL.

      ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~GET_INSTANCE(
         )->SET_ORDEM_VENDA(
            EXPORTING
              I_VBELN           = I_LOTE_FRETE-VBELN
         )->GET_TIPO_TRANSPORTE(
            EXPORTING
              I_VSART           = '01'
            IMPORTING
              E_TIPO_TRANSPORTE = L_FRETE-SHTYP
         ).

*--------------------------------------
* --pedido de compras - Tipo de transporte
*--------------------------------------
    ELSEIF I_LOTE_FRETE-EBELN IS NOT INITIAL.

      ZCL_PEDIDO_COMPRA=>GET_INSTANCE(
         )->SET_PEDIDO_PC(
            EXPORTING
              I_EBELN           = I_LOTE_FRETE-EBELN
         )->GET_TIPO_TRANSPORTE(
            EXPORTING
              I_VSART           = '01'
            IMPORTING
              E_TIPO_TRANSPORTE = L_FRETE-SHTYP ).
*--------------------------------------
* -- Solicitação de Compra - Tipo de transporte
*--------------------------------------
    ELSEIF I_LOTE_FRETE-NRO_SOL IS NOT INITIAL.

      ZCL_PEDIDO_COMPRA=>GET_INSTANCE(
         )->GET_SOLICITACAO(
            EXPORTING
              E_SOLICITACAO = I_LOTE_FRETE-NRO_SOL
            IMPORTING
              I_PEDIDO = DATA(I_EBELN)
         )->SET_PEDIDO_PC(
            EXPORTING
              I_EBELN           = I_EBELN
         )->GET_TIPO_TRANSPORTE(
            EXPORTING
              I_VSART           = '01'
            IMPORTING
              E_TIPO_TRANSPORTE = L_FRETE-SHTYP ).

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    ELSEIF I_LOTE_FRETE-NRO_CG_SAI_IN IS NOT INITIAL.

      SELECT SINGLE *
        FROM ZSDT0129 INTO @DATA(LWA_ZSDT0129)
       WHERE NRO_CG EQ @I_LOTE_FRETE-NRO_CG_SAI_IN.

      IF SY-SUBRC EQ 0.
        SELECT SINGLE *
          FROM ZSDT0131 INTO @DATA(LWA_ZSDT0131)
         WHERE NRO_LOTE EQ @LWA_ZSDT0129-NRO_LOTE.

        IF SY-SUBRC EQ 0.
          ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~GET_INSTANCE(
             )->SET_ORDEM_VENDA(
                EXPORTING
                  I_VBELN           = LWA_ZSDT0131-VBELN
             )->GET_TIPO_TRANSPORTE(
                EXPORTING
                  I_VSART           = '01'
                IMPORTING
                  E_TIPO_TRANSPORTE = L_FRETE-SHTYP
             ).
        ENDIF.

      ENDIF.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    ENDIF.

*--------------------------------------
*-- Tipo do Contrato
*--------------------------------------
    TRY.
        ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE(
           )->SET_VEICULO(
              EXPORTING
                I_PLACA         = I_PLACA_VEIC_TRACAO
           )->GET_TIPO_CONTRATO(
              IMPORTING
                E_TIPO_CONTRATO = L_FRETE-ADD01
           ).

      CATCH ZCX_VEICULOS INTO DATA(EX_VEICULO).    "
        EX_VEICULO->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_GERAL-MSGID
                              MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1  = CONV #( SY-MSGV1 ) ATTR2  = CONV #( SY-MSGV2 ) ATTR3  = CONV #( SY-MSGV3 ) ATTR4  = CONV #( SY-MSGV4 ) )
            MSGTY  = 'E'
            MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_GERAL-MSGNO
            MSGV1  = SY-MSGV1
            MSGV2  = SY-MSGV2
            MSGV3  = SY-MSGV3
            MSGV4  = SY-MSGV4.
    ENDTRY.

*"// US-169528 - ZLES0136 - WBARBOSA
    CASE I_VALOR_FRETE-MODALIDADE_PAGAMENTO_FRETE.
      WHEN 1.
*--------------------------------------
*-- cadastra frete trecho terceiro - TK11
*--------------------------------------
        IF I_PAGA_TRECHO_TERCEIRO = ABAP_TRUE.
          L_VALOR_FRETE_TERCEIRO = I_VALOR_FRETE-VALOR_FRETE_TRECHO_TERCEIRO.

          IF L_VALOR_FRETE_TERCEIRO IS NOT INITIAL.
            L_ADD01 = L_FRETE-ADD01.
            L_VALOR = I_VALOR_FRETE-VALOR_FRETE_TRECHO_TERCEIRO.

            REPLACE ALL OCCURRENCES OF '.' IN L_VALOR WITH ','.

            IF L_ADD01 = '0001'.
              L_TO = 'TO'.
            ENDIF.

*--------------------------------------
*---- monta bdc
*--------------------------------------
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '0100' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'RV13A-KSCHL' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'RV13A-KSCHL'        I_FVAL   = 'ZFRE' ).
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPLV14A'           I_DYNPRO = '0100' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'RV130-SELKZ(01)' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '=WEIT' ).
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KOMG-SDABW' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SHTYP'         I_FVAL   = 'Z021' ). "CONV #( l_frete-shtyp ) ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SDABW'         I_FVAL   = CONV #( L_ADD01 ) ).
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KONP-KMEIN(01)' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SHTYP'         I_FVAL   = 'Z021' ). "CONV #( l_frete-shtyp ) ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SDABW'         I_FVAL   = CONV #( L_ADD01 ) ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-ID_VIAGEM(01)' I_FVAL   = CONV #( I_VIAGEM_ID ) ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KBETR(01)'     I_FVAL   = CONV #( L_VALOR ) ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KONWA(01)'     I_FVAL   = 'BRL' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KPEIN(01)'     I_FVAL   = '    1' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KMEIN(01)'     I_FVAL   = CONV #( L_TO ) ).
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KOMG-ID_VIAGEM(01)' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '=SICH' ).

*--------------------------------------
*---- executa TK11
*--------------------------------------
            TRY.
                ME->SET_BDC_TRANSACTION(
                  EXPORTING
                    I_TCODE = 'TK11'
                ).

              CATCH ZCX_INTEGRACAO INTO DATA(EX_INTEGRACAO).
                E_MSG_ERRO = EX_INTEGRACAO->ZIF_ERROR~GET_MSG_ERRO( ).

                RAISE EXCEPTION TYPE ZCX_INTEGRACAO
                  EXPORTING
                    TEXTID = VALUE #( MSGID  = SY-MSGID
                                      MSGNO  = SY-MSGNO
                                      ATTR1  = CONV #( SY-MSGV1 ) ATTR2  = CONV #( SY-MSGV2 ) ATTR3  = CONV #( SY-MSGV3 ) ATTR4  = CONV #( SY-MSGV4 ) )
                    MSGTY  = 'E'
                    MSGID  = SY-MSGID
                    MSGNO  = SY-MSGNO
                    MSGV1  = SY-MSGV1
                    MSGV2  = SY-MSGV2
                    MSGV3  = SY-MSGV3
                    MSGV4  = SY-MSGV4.
            ENDTRY.

            APPEND VALUE #( ID_VIAGEM = I_VIAGEM_ID SHTYP = 'Z021' ) TO LIT_A942_SEL.

          ENDIF.
        ENDIF.

*--------------------------------------
*-- cadastra frete trecho fiscal - TK11
*--------------------------------------
        L_VALOR_FRETE_TRECHO_FISCAL = I_VALOR_FRETE-VALOR_FRETE_TRECHO_FISCAL.

        IF L_VALOR_FRETE_TRECHO_FISCAL IS NOT INITIAL.
          FREE: T_BDCDATA,
                L_TO,
                E_MSG_ERRO.

          L_ADD01 = L_FRETE-ADD01.
          L_VALOR = I_VALOR_FRETE-VALOR_FRETE_TRECHO_FISCAL.

          REPLACE ALL OCCURRENCES OF '.' IN L_VALOR WITH ','.

          IF L_ADD01 = '0001'.
            L_TO = 'TO'.
          ENDIF.

*--------------------------------------
*-- monta bdc
*--------------------------------------
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '0100' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'RV13A-KSCHL' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'RV13A-KSCHL'        I_FVAL   = 'ZFRE' ).
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPLV14A'           I_DYNPRO = '0100' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'RV130-SELKZ(01)' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '=WEIT' ).
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KOMG-SDABW' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SHTYP'         I_FVAL   = CONV #( L_FRETE-SHTYP ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SDABW'         I_FVAL   = CONV #( L_ADD01 ) ).
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KONP-KMEIN(01)' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SHTYP'         I_FVAL   = CONV #( L_FRETE-SHTYP ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SDABW'         I_FVAL   = CONV #( L_ADD01 ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-ID_VIAGEM(01)' I_FVAL   = CONV #( I_VIAGEM_ID ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KBETR(01)'     I_FVAL   = CONV #( L_VALOR ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KONWA(01)'     I_FVAL   = 'BRL' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KPEIN(01)'     I_FVAL   = '    1' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KMEIN(01)'     I_FVAL   = CONV #( L_TO ) ).
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KOMG-ID_VIAGEM(01)' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '=SICH' ).

*--------------------------------------
*-- executa TK11
*--------------------------------------
          TRY.
              ME->SET_BDC_TRANSACTION(
                EXPORTING
                  I_TCODE = 'TK11'
              ).

            CATCH ZCX_INTEGRACAO INTO EX_INTEGRACAO.
              E_MSG_ERRO = EX_INTEGRACAO->ZIF_ERROR~GET_MSG_ERRO( ).

              RAISE EXCEPTION TYPE ZCX_INTEGRACAO
                EXPORTING
                  TEXTID = VALUE #( MSGID  = SY-MSGID
                                    MSGNO  = SY-MSGNO
                                    ATTR1  = CONV #( SY-MSGV1 ) ATTR2  = CONV #( SY-MSGV2 ) ATTR3  = CONV #( SY-MSGV3 ) ATTR4  = CONV #( SY-MSGV4 ) )
                  MSGTY  = 'E'
                  MSGID  = SY-MSGID
                  MSGNO  = SY-MSGNO
                  MSGV1  = SY-MSGV1
                  MSGV2  = SY-MSGV2
                  MSGV3  = SY-MSGV3
                  MSGV4  = SY-MSGV4.
          ENDTRY.

          APPEND VALUE #( ID_VIAGEM = I_VIAGEM_ID SHTYP = L_FRETE-SHTYP ) TO LIT_A942_SEL.

        ENDIF.

      WHEN 2. "// Cadastro TK de Frete Montante Fixo

*--------------------------------------
*-- cadastra frete trecho terceiro - TK11
*--------------------------------------
        IF I_PAGA_TRECHO_TERCEIRO = ABAP_TRUE.
          L_VALOR_FRETE_TERCEIRO = I_VALOR_FRETE-VALOR_FRETE_TRECHO_TERCEIRO.

          IF L_VALOR_FRETE_TERCEIRO IS NOT INITIAL.
            L_ADD01 = L_FRETE-ADD01.
            L_VALOR = I_VALOR_FRETE-VALOR_FRETE_TRECHO_TERCEIRO.

            REPLACE ALL OCCURRENCES OF '.' IN L_VALOR WITH ','.

*--------------------------------------
*---- monta bdc
*--------------------------------------
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '0100' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'RV13A-KSCHL' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'RV13A-KSCHL'        I_FVAL   = 'ZFRE' ).
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPLV14A'           I_DYNPRO = '0100' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'RV130-SELKZ(01)' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '=WEIT' ).
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KOMG-SDABW' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SHTYP'         I_FVAL   = 'Z021' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SDABW'         I_FVAL   = '0002' ). "// Frete Lotação
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KONP-KMEIN(01)' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SHTYP'         I_FVAL   = 'Z021' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SDABW'         I_FVAL   = '0002' ). "// Frete Lotação
            ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-ID_VIAGEM(01)' I_FVAL   = CONV #( I_VIAGEM_ID ) ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KBETR(01)'     I_FVAL   = CONV #( L_VALOR ) ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KONWA(01)'     I_FVAL   = 'BRL' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'RV13A-KRECH(01)'    I_FVAL   = 'B' ). "// Montante fixo
            ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KOMG-ID_VIAGEM(01)' ).
            ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '=SICH' ).

*--------------------------------------
*---- executa TK11
*--------------------------------------
            TRY.
                ME->SET_BDC_TRANSACTION(
                  EXPORTING
                    I_TCODE = 'TK11'
                ).

              CATCH ZCX_INTEGRACAO INTO EX_INTEGRACAO.
                E_MSG_ERRO = EX_INTEGRACAO->ZIF_ERROR~GET_MSG_ERRO( ).

                RAISE EXCEPTION TYPE ZCX_INTEGRACAO
                  EXPORTING
                    TEXTID = VALUE #( MSGID  = SY-MSGID
                                      MSGNO  = SY-MSGNO
                                      ATTR1  = CONV #( SY-MSGV1 ) ATTR2  = CONV #( SY-MSGV2 ) ATTR3  = CONV #( SY-MSGV3 ) ATTR4  = CONV #( SY-MSGV4 ) )
                    MSGTY  = 'E'
                    MSGID  = SY-MSGID
                    MSGNO  = SY-MSGNO
                    MSGV1  = SY-MSGV1
                    MSGV2  = SY-MSGV2
                    MSGV3  = SY-MSGV3
                    MSGV4  = SY-MSGV4.
            ENDTRY.

            APPEND VALUE #( ID_VIAGEM = I_VIAGEM_ID SHTYP = 'Z021' ) TO LIT_A942_SEL.

          ENDIF.
        ENDIF.

*--------------------------------------
*-- cadastra frete trecho fiscal - TK11
*--------------------------------------
        L_VALOR_FRETE_TRECHO_FISCAL = I_VALOR_FRETE-VALOR_FRETE_TRECHO_FISCAL.

        IF L_VALOR_FRETE_TRECHO_FISCAL IS NOT INITIAL.
          FREE: T_BDCDATA,
                L_TO,
                E_MSG_ERRO.

          L_ADD01 = L_FRETE-ADD01.
          L_VALOR = I_VALOR_FRETE-VALOR_FRETE_TRECHO_FISCAL.

          REPLACE ALL OCCURRENCES OF '.' IN L_VALOR WITH ','.

*--------------------------------------
*-- monta bdc
*--------------------------------------
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '0100' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'RV13A-KSCHL' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'RV13A-KSCHL'        I_FVAL   = 'ZFRE' ).
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPLV14A'           I_DYNPRO = '0100' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'RV130-SELKZ(01)' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '=WEIT' ).
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KOMG-SDABW' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SHTYP'         I_FVAL   = CONV #( L_FRETE-SHTYP ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SDABW'         I_FVAL   = '0002' ).  "// Frete Lotação
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KONP-KMEIN(01)' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '/00' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SHTYP'         I_FVAL   = CONV #( L_FRETE-SHTYP ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-SDABW'         I_FVAL   = '0002' ).  "// Frete Lotação
          ME->SET_BDC_FIELD(  I_FNAM    = 'KOMG-ID_VIAGEM(01)' I_FVAL   = CONV #( I_VIAGEM_ID ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KBETR(01)'     I_FVAL   = CONV #( L_VALOR ) ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'KONP-KONWA(01)'     I_FVAL   = 'BRL' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'RV13A-KRECH(01)'    I_FVAL   = 'B' ). "// Montante fixo
          ME->SET_BDC_DYNPRO( I_PROGRAM = 'SAPMV13A'           I_DYNPRO = '1942' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_CURSOR'         I_FVAL   = 'KOMG-ID_VIAGEM(01)' ).
          ME->SET_BDC_FIELD(  I_FNAM    = 'BDC_OKCODE'         I_FVAL   = '=SICH' ).

*--------------------------------------
*-- executa TK11
*--------------------------------------
          TRY.
              ME->SET_BDC_TRANSACTION(
                EXPORTING
                  I_TCODE = 'TK11'
              ).

            CATCH ZCX_INTEGRACAO INTO EX_INTEGRACAO.
              E_MSG_ERRO = EX_INTEGRACAO->ZIF_ERROR~GET_MSG_ERRO( ).

              RAISE EXCEPTION TYPE ZCX_INTEGRACAO
                EXPORTING
                  TEXTID = VALUE #( MSGID  = SY-MSGID
                                    MSGNO  = SY-MSGNO
                                    ATTR1  = CONV #( SY-MSGV1 ) ATTR2  = CONV #( SY-MSGV2 ) ATTR3  = CONV #( SY-MSGV3 ) ATTR4  = CONV #( SY-MSGV4 ) )
                  MSGTY  = 'E'
                  MSGID  = SY-MSGID
                  MSGNO  = SY-MSGNO
                  MSGV1  = SY-MSGV1
                  MSGV2  = SY-MSGV2
                  MSGV3  = SY-MSGV3
                  MSGV4  = SY-MSGV4.
          ENDTRY.

          APPEND VALUE #( ID_VIAGEM = I_VIAGEM_ID SHTYP = L_FRETE-SHTYP ) TO LIT_A942_SEL.

        ENDIF.

      WHEN OTHERS.
    ENDCASE.
*"// US-169528 - ZLES0136 - WBARBOSA

    "WAIT UP TO 5 SECONDS.

    "Checa se condições foram gravados na TK11
    LOOP AT LIT_A942_SEL INTO DATA(LWA_A942_SEL).

      SELECT SINGLE *
        FROM A942 INTO @DATA(LWA_A942)
       WHERE ID_VIAGEM EQ @LWA_A942_SEL-ID_VIAGEM
         AND SHTYP     EQ @LWA_A942_SEL-SHTYP.

      IF SY-SUBRC NE 0.

        SY-MSGV1 = 'Não foi possivel cadastrar o preço na TK11.'.
        CONCATENATE 'Id. Viagem:'         LWA_A942_SEL-ID_VIAGEM INTO SY-MSGV2 SEPARATED BY SPACE.
        CONCATENATE 'Tipo de Transporte:' LWA_A942_SEL-SHTYP     INTO SY-MSGV3 SEPARATED BY SPACE.

        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_GERAL-MSGID
                              MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_GERAL-MSGNO
                              ATTR1  = CONV #( SY-MSGV1 ) ATTR2  = CONV #( SY-MSGV2 ) ATTR3  = CONV #( SY-MSGV3 ) )
            MSGTY  = 'E'
            MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_GERAL-MSGID
            MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_GERAL-MSGNO
            MSGV1  = SY-MSGV1
            MSGV2  = SY-MSGV2
            MSGV3  = SY-MSGV3.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.

*    SELECT SINGLE * INTO @DATA(WA_VIAGEM)
*      FROM ZLEST0185
*     WHERE VIAGEM_ID EQ @I_INTEGRACAO-ID_REFERENCIA.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGID
*                            MSGNO = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGNO
*                            ATTR1 = CONV #( I_INTEGRACAO-ID_REFERENCIA ) )
*          MSGID  = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGID
*          MSGNO  = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGNO
*          MSGTY  = 'E'
*          MSGV1  = CONV #( I_INTEGRACAO-ID_REFERENCIA ).
*    ENDIF.
*
*    TRY .
*        CAST ZCL_INTEGRACAO_TOKEN(
*               ZCL_INTEGRACAO_TOKEN=>ZIF_INTEGRACAO_TOKEN~GET_INSTANCE(
*                 )->SET_EMPRESA_TOKEN( EXPORTING I_BUKRS = WA_VIAGEM-BUKRS
*                 )
*             )->ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP(
*          IMPORTING
*            E_HEADER_FIELDS = DATA(E_HEADER_FIELDS) ).
*
*        ME->ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP( I_HEADER_FIELDS = E_HEADER_FIELDS ).
*
*      CATCH ZCX_ERROR INTO DATA(EX_ERRO).
*
*        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
*          EXPORTING
*            TEXTID = VALUE #( MSGID = EX_ERRO->ZIF_ERROR~MSGID
*                              MSGNO = EX_ERRO->ZIF_ERROR~MSGNO
*                              ATTR1 = CONV #( EX_ERRO->ZIF_ERROR~MSGV1 )
*                              ATTR2 = CONV #( EX_ERRO->ZIF_ERROR~MSGV2 )
*                              ATTR3 = CONV #( EX_ERRO->ZIF_ERROR~MSGV3 )
*                              ATTR4 = CONV #( EX_ERRO->ZIF_ERROR~MSGV4 ) )
*            MSGID  = EX_ERRO->ZIF_ERROR~MSGID
*            MSGNO  = EX_ERRO->ZIF_ERROR~MSGNO
*            MSGTY  = 'E'
*            MSGV1  = EX_ERRO->ZIF_ERROR~MSGV1
*            MSGV2  = EX_ERRO->ZIF_ERROR~MSGV2
*            MSGV3  = EX_ERRO->ZIF_ERROR~MSGV3
*            MSGV4  = EX_ERRO->ZIF_ERROR~MSGV4.
*
*    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: i_inbound   TYPE zde_carguero_requisicao,
          lc_aprovar  TYPE REF TO zcl_integracao_viagem_aprovar,
          lc_rejeitar TYPE REF TO zcl_integracao_viagem_rejeita.

    r_if_integracao_inject = me.

    CLEAR: e_msg_outbound, e_sucesso.

    CLEAR: CAST zif_integracao_ord_carrega( me )->at_msg_inbound.

    " Cadastrar/Alterar/Deletar Placa
    /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound  CHANGING data = i_inbound ).

    SELECT SINGLE * INTO @DATA(wa_zlest0185)
      FROM zlest0185
     WHERE viagem_id EQ @i_inbound-data-viagem_id.

    CHECK sy-subrc IS INITIAL.

    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
*    SELECT SINGLE * INTO @DATA(wa_zlest0181_frete)
*      FROM zlest0181
*    WHERE id_lote_frete = @wa_zlest0185-id_lote_frete.
*
*    CHECK sy-subrc EQ 0.
*
*    IF wa_zlest0181_frete-viagem_aprovacao_rejeicao_manu EQ abap_true AND wa_zlest0185-ck_processada_com_sucesso EQ abap_true.
*      e_sucesso = ABAP_TRUE. "Viagem será aprovada pela transação ZSDT0112
*      EXIT.
*    ENDIF.
    "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676

    CASE wa_zlest0185-ck_autorizada.
      WHEN abap_true.

        "Autorizar Viagem
        CREATE OBJECT lc_aprovar.
        lc_aprovar->zif_integracao_viagem_aprovar~set_new_viagem_aprovar(
              EXPORTING i_viagem_id = wa_zlest0185-viagem_id IMPORTING e_id_integracao = wa_zlest0185-id_integracao_aprovar
          ).

        CLEAR: lc_aprovar.
        e_sucesso = abap_true.
        UPDATE zlest0185
           SET id_integracao_aprovar = wa_zlest0185-id_integracao_aprovar
               ck_integrada           = abap_true
         WHERE viagem_id EQ wa_zlest0185-viagem_id.
        COMMIT WORK.

      WHEN abap_false.

        "Rejeitar Viagem
        CREATE OBJECT lc_rejeitar.
        lc_rejeitar->zif_integracao_viagem_rejeitar~set_new_viagem_rejeitar(
              EXPORTING i_viagem_id = wa_zlest0185-viagem_id IMPORTING e_id_integracao = wa_zlest0185-id_integracao_rejeitar
          ).

        CLEAR: lc_rejeitar.
        e_sucesso = abap_true.
        UPDATE zlest0185
           SET id_integracao_rejeitar = wa_zlest0185-id_integracao_rejeitar
               ck_integrada           = abap_true
         WHERE viagem_id EQ wa_zlest0185-viagem_id.
        COMMIT WORK.

    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    "Metodo para Integrar InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
*&---------------------------------------------------------------------------&*
*&                    Histórico de Modificações                              &*
*& Autor ABAP |Request    |Data       |Descrição                             &*
*&---------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2LTT |27/06/2025 |Implementação Status de processamento &*
*&                                    |da Carga - INSUMOS. Chamado: 169508.  &*
*&---------------------------------------------------------------------------&*

    TYPES BEGIN OF ty_messagem.
    TYPES: message TYPE string.
    TYPES END OF ty_messagem.

    DATA: it_veiculos              TYPE TABLE OF zlest0002,
          it_fornecedores          TYPE TABLE OF zfie_vendor,
          i_inbound                TYPE zde_carguero_requisicao,
          local                    TYPE zde_msg_inbound_ordem_carrega,
          lc_veiculos              TYPE zde_cad_veiculo,
          lc_proprietario          TYPE zde_cad_fonecedor,
          lc_transp                TYPE zde_cad_fonecedor,  "*-CS2021000253-26.04.2024-#59941-JT
          lc_agente_transp         TYPE lifnr,              "*-CS2021000253-28.11.2024-#159662-JT-fim    "*-CS2021000253-26.04.2024-#59941-JT
          lc_transportador         TYPE zde_carguero_transportador2,
          e_msg_veiculos           TYPE string,
          e_msg_proprietarios      TYPE string,
          "LC_SEPRADOR         TYPE STRING,
          lc_erro                  TYPE char01,
          lva_cpf_prop             TYPE string,
          lva_cpf_transp           TYPE string,
          wa_tx                    TYPE zlest0185tx,
          lc_texto                 TYPE string,
          lc_auart                 TYPE vbak-auart,           "*-CS2023000070-21.03.2023-#103477-JT
          lc_bsart                 TYPE ekko-bsart,           "*-CS2023000070-21.03.2023-#103477-JT
          lc_matkl                 TYPE mara-matkl,           "*-CS2023000070-21.03.2023-#103477-JT
          lc_lifnr                 TYPE lfa1-lifnr,           "*-CS2023000070-21.03.2023-#103477-JT
          lc_lifnr_motorista       TYPE lfa1-lifnr,           "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
          lc_data                  TYPE datum,                "*-CS2023000070-21.03.2023-#103477-JT
          lc_peso_ok               TYPE char1,                "*-CS2023000070-21.03.2023-#103477-JT
          t_0001od                 TYPE TABLE OF zsdt0001od,  "*-CS2023000070-21.03.2023-#103477-JT
          w_0001od                 TYPE zsdt0001od,           "*-CS2023000070-21.03.2023-#103477-JT
          lt_split_safra           TYPE TABLE OF char40,
          lc_sap_opus              TYPE zde_json_oc_sap_to_pus_ov,
          lc_emp_join_vt           TYPE tvarvc-low,
          lr_ktokk                 TYPE RANGE OF lfa1-ktokk,  "*-ISSUE #172761-01.04.2025-JT
          lc_ktokk                 TYPE lfa1-ktokk,           "*-CS2021000253-26.04.2024-#59941-JT
          lc_valor_frete_motorista TYPE string,               "*-CS2021000253-15.08.2024-#148932-JT-BUG 148932-inicio
          lc_outbound_forne        TYPE zfie_vendor.          "*-IR 219949-29.01.2025-#165179-JT-inicio

    DATA: rg_vgtyp TYPE RANGE OF lips-vgtyp,
          wa_vgtyp LIKE LINE  OF rg_vgtyp.

    "LC_SEPRADOR =  ' ->'.

    r_if_integracao_inject = me.

    CLEAR: e_msg_proprietarios, e_msg_veiculos, e_sucesso, lr_ktokk.  "*-ISSUE #172761-01.04.2025-JT


    " Ler JSON Recebido """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Cadastrar/Alterar/Deletar Placa
    CLEAR: CAST zif_integracao_ord_carrega( me )->at_msg_inbound.
    /ui2/cl_json=>deserialize( EXPORTING json = i_msg CHANGING data = i_inbound ).

    "Criar Cadastro da Viagem no SAP """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(wa_zlest0185)
      FROM zlest0185
     WHERE viagem_id EQ @i_inbound-data-viagem_id.

    IF sy-subrc IS NOT INITIAL.
      wa_zlest0185-dt_registro     = sy-datum.
      wa_zlest0185-hr_registro     = sy-uzeit.
      wa_zlest0185-us_registro     = sy-uname.
      wa_zlest0185-viagem_id       = i_inbound-data-viagem_id.
      wa_zlest0185-id_localizador  = i_inbound-data-codigo_localizador. "WPP 16.05.2025  Integração SOlicitação Recebimento US 184481
      wa_zlest0185-ck_integrada    = abap_false.
      wa_zlest0185-ck_processada   = abap_false.
      wa_zlest0185-id_integracao   = i_id_integracao.
*-CS2021000508 - 27.05.2021 - JT - inicio
      wa_zlest0185-base_url        = i_inbound-metadata-storage-base_url.
      wa_zlest0185-blob_path       = i_inbound-metadata-storage-blob_path.
*-CS2021000508 - 27.05.2021 - JT - fim
      MODIFY zlest0185 FROM wa_zlest0185.
      COMMIT WORK.
    ENDIF.


    wa_tx-tx_json_entrada = i_msg.
    wa_tx-tx_validacao_viagem   = ''.
    wa_tx-tx_validacao_prop     = ''.
    wa_tx-tx_validacao_veiculos = ''.
    wa_tx-tx_validacao_mot      = ''.
    wa_tx-tx_validacao_preco    = ''.
    wa_tx-tx_validacao_oc       = ''.

    "Validar Lote Embarcador """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    SELECT SINGLE * INTO @DATA(wa_lote_frete)
      FROM zlest0181
     WHERE id_carguero EQ @i_inbound-data-lote_embarcador-id.

    IF sy-subrc IS NOT INITIAL.
      lc_erro = abap_true.
      wa_tx-tx_validacao_viagem = 'Lote Embarcador' && space && i_inbound-data-lote_embarcador-id && space && 'não encontrada no sistema de destino!'.
    ELSE.

      wa_zlest0185-id_lote_frete  = wa_lote_frete-id_lote_frete.
      wa_zlest0185-vbeln          = wa_lote_frete-vbeln.
      wa_zlest0185-posnr          = wa_lote_frete-posnr.
      wa_zlest0185-ebeln          = wa_lote_frete-ebeln.
      wa_zlest0185-ebelp          = wa_lote_frete-ebelp.

      wa_zlest0185-nro_sol        = wa_lote_frete-nro_sol. "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
      wa_zlest0185-nro_cg_sai_in  = wa_lote_frete-nro_cg_sai_in. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
      wa_zlest0185-matkl          = wa_lote_frete-matkl. "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676

      wa_zlest0185-bukrs          = wa_lote_frete-bukrs.
      wa_zlest0185-branch         = wa_lote_frete-branch.
      "wa_zlest0185-id_localizador = i_inbound-data-codigo_localizador. "WPP 16.05.2025  Integração SOlicitação Recebimento US 184481
*-CS2021000508 - 27.05.2021 - JT - inicio
      wa_zlest0185-base_url       = i_inbound-metadata-storage-base_url.
      wa_zlest0185-blob_path      = i_inbound-metadata-storage-blob_path.
*-CS2021000508 - 27.05.2021 - JT - fim

      "Validar Produto
      SELECT * INTO TABLE @DATA(it_material_depara)
        FROM zlest0183
       WHERE id_mt_carguero EQ @i_inbound-data-produto-id
         AND bukrs EQ @wa_zlest0185-bukrs.

      IF sy-subrc IS NOT INITIAL OR i_inbound-data-produto-id IS INITIAL.
        lc_texto = 'Produto informado na viagem, não encontrado no sistema destino'.
        wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
        lc_erro = abap_true.
      ELSEIF sy-subrc IS INITIAL.
        READ TABLE it_material_depara WITH KEY matnr = wa_lote_frete-matnr tp_produto = wa_lote_frete-tp_produto matkl = wa_lote_frete-matkl TRANSPORTING NO FIELDS. "//wbarbosa US-171426 080525
        IF sy-subrc IS NOT INITIAL.
          lc_texto = 'Produto informado na viagem, diferente do produto e tipo do produto encontrado no sistema destino'.
          wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
          lc_erro = abap_true.
        ENDIF.
      ENDIF.

      "Validar Local de Coleta
      IF zcl_string=>length( i_inbound-data-local_coleta-referencia_integracao ) NE 11 OR i_inbound-data-local_coleta-referencia_integracao+1(10) NE wa_lote_frete-id_parceiro_cole.
        lc_texto = 'Local de Coleta informado na viagem, diferente do local de sistema destino'.
        wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
        lc_erro = abap_true.
      ENDIF.

      IF lc_erro EQ abap_false.
        TRY .
            CASE wa_lote_frete-tp_parceiro_cole.
              WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
                zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = wa_lote_frete-id_parceiro_cole
                  )->ck_ativo_empresa( i_empresa = wa_lote_frete-bukrs
                  )->ck_ativo(
                  ).
              WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
                zcl_clientes=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = wa_lote_frete-id_parceiro_cole
                  )->ck_ativo_empresa( i_empresa = wa_lote_frete-bukrs
                  )->ck_ativo(
                  ).
            ENDCASE.
          CATCH zcx_parceiros INTO DATA(ex_parceiro).
            lc_erro = abap_true.
            wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = ex_parceiro->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
        ENDTRY.
      ENDIF.

      "Validar Local de Descarga
      IF zcl_string=>length( i_inbound-data-local_descarga-referencia_integracao ) NE 11 OR i_inbound-data-local_descarga-referencia_integracao+1(10) NE wa_lote_frete-id_parceiro_entr.
        lc_texto = 'Local de Descarga informado na viagem, diferente do local de sistema destino'.
        wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
        lc_erro = abap_true.
      ENDIF.

      IF lc_erro EQ abap_false.
        TRY .
            CASE wa_lote_frete-tp_parceiro_entr.
              WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
                zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = wa_lote_frete-id_parceiro_entr
                  )->ck_ativo_empresa( i_empresa = wa_lote_frete-bukrs
                  )->ck_ativo(
                  ).
              WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
                zcl_clientes=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = wa_lote_frete-id_parceiro_entr
                  )->ck_ativo_empresa( i_empresa = wa_lote_frete-bukrs
                  )->ck_ativo(
                  ).
            ENDCASE.
          CATCH zcx_parceiros INTO ex_parceiro.
            lc_erro = abap_true.
            wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = ex_parceiro->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
        ENDTRY.
      ENDIF.

*-CS2021000253-15.08.2024-#148932-JT-BUG 148999-inicio - descomentado em 02.10.2024
*-CS2021000253-28.11.2024-#159662-JT-inicio
*-----Versao comentada em 03.12.2024
*-CS2021000253-10.12.2024-#160631-JT-inicio
      SELECT SINGLE low
        INTO @DATA(_low)
        FROM tvarvc
       WHERE name = 'STRADA_LOG_VALIDA_CPT'
         AND low  = @abap_true.

      IF sy-subrc = 0.
        "Validar Viagem Strada Log - Terceiros
        IF i_inbound-data-transportador-documentos-cpf_cnpj IS NOT INITIAL.
          SELECT SINGLE lifnr
            INTO @DATA(lc_parceiro_terc)
            FROM lfa1
           WHERE stcd1 = @i_inbound-data-transportador-documentos-cpf_cnpj
             AND loevm = @abap_off
             AND sperr = @abap_off
             AND sperm = @abap_off.

          IF sy-subrc = 0.
            TRY .
                zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = lc_parceiro_terc
                  )->ck_parceiro_local_negocio( IMPORTING e_j_1bbranch = DATA(lc_branch)
                  ).
                DATA(lc_intercompany) = abap_true.
              CATCH zcx_parceiros.
                lc_intercompany       = abap_false.
            ENDTRY.

            IF     wa_lote_frete-tp_frete = zif_carga=>st_tp_frete_cif.
              IF lc_intercompany = abap_false.
                lc_texto = 'Incoterms do Lote é CIF, porém Parceiro Frete é Terceiro!'.
                wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
                lc_erro = abap_true.
              ENDIF.
            ELSEIF wa_lote_frete-tp_frete = zif_carga=>st_tp_frete_cpt.
              IF lc_intercompany = abap_true.
                lc_texto = 'Incoterms do Lote é CPT, porém Parceiro Frete é Intercompany!'.
                wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
                lc_erro = abap_true.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*-CS2021000253-10.12.2024-#160631-JT-fim
*-CS2021000253-15.08.2024-#148932-JT-BUG 148999-fim
*-CS2021000253-28.11.2024-#159662-JT-fim

*-CS2021000253-10.12.2024-#160631-JT-inicio
*==== Comentado em 10.12.2024, por carguero envia informacao "#--" no campo referencia_integracao ======================================
      "Validar Transportador
*      IF i_inbound-data-transportador-referencia_integracao IS NOT INITIAL AND wa_lote_frete-tp_frete EQ zif_carga=>st_tp_frete_cif.
*        IF zcl_string=>length( i_inbound-data-transportador-referencia_integracao ) NE 11 OR
*           ( i_inbound-data-transportador-referencia_integracao+1(10) NE wa_lote_frete-id_parceiro_frete AND wa_lote_frete-id_parceiro_frete IS NOT INITIAL ).
*          lc_texto = 'Transportador informado na viagem, diferente do transportador de sistema destino'.
*          wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
*          lc_erro = abap_true.
*        ENDIF.
*        IF lc_erro EQ abap_false.
*          TRY .
*              CASE wa_lote_frete-tp_parceiro_frete.
*                WHEN zif_integracao_lote_frete=>at_tp_parceiro_fornecedor.
*                  zcl_fornecedores=>zif_parceiros~get_instance(
*                    )->set_parceiro( i_parceiro = wa_lote_frete-id_parceiro_frete
*                    )->ck_ativo_empresa( i_empresa = wa_lote_frete-bukrs
*                    )->ck_ativo(
*                    ).
*                WHEN zif_integracao_lote_frete=>at_tp_parceiro_cliente.
*                  zcl_clientes=>zif_parceiros~get_instance(
*                    )->set_parceiro( i_parceiro = wa_lote_frete-id_parceiro_frete
*                    )->ck_ativo_empresa( i_empresa = wa_lote_frete-bukrs
*                    )->ck_ativo(
*                    ).
*              ENDCASE.
*            CATCH zcx_parceiros INTO ex_parceiro.
*              lc_erro = abap_true.
*              wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = ex_parceiro->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
*          ENDTRY.
*        ENDIF.
*      ENDIF.
*-CS2021000253-10.12.2024-#160631-JT-fim
*==================================================================================================

      "Validar Preço de Frete
      IF i_inbound-data-valor_frete-valor_frete_empresa IS INITIAL OR i_inbound-data-valor_frete-valor_frete_empresa = '0'.
        lc_texto = 'Não foi informado o valor do frete empresa'.
        wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
        lc_erro = abap_true.
      ENDIF.

      lc_valor_frete_motorista = i_inbound-data-valor_frete-valor_frete_motorista.  "*-CS2021000253-15.08.2024-#148932-JT-BUG 148932

      IF i_inbound-data-valor_frete-valor_frete_motorista IS INITIAL OR i_inbound-data-valor_frete-valor_frete_motorista = '0' AND wa_lote_frete-tp_frete EQ zif_carga=>st_tp_frete_cif.
        lc_texto = 'Não foi informado o valor do frete motorista'.
        wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
        lc_erro = abap_true.
      ELSEIF wa_lote_frete-tp_frete EQ zif_carga=>st_tp_frete_cpt OR
             wa_lote_frete-tp_frete EQ zif_carga=>st_tp_frete_cfr.
        i_inbound-data-valor_frete-valor_frete_motorista = i_inbound-data-valor_frete-valor_frete_empresa.
      ENDIF.


      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      IF wa_lote_frete-nro_cg_sai_in IS NOT INITIAL AND lc_erro EQ abap_false.

        SELECT SINGLE *
          FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
         WHERE id_lote_frete EQ @wa_lote_frete-id_lote_frete
           AND user_canc     EQ @space  "Não cancelada
           AND viagem_id     NE @space. "Viagem Gerada

        IF sy-subrc EQ 0.
          lc_texto = |Já possui uma autorização de Embarque numero: { lwa_zsdt0133-nro_cg } para esse Lote de Embarcador!|.
          wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
          lc_erro = abap_true.
        ENDIF.

      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    ENDIF.

    "Mapear Campos CARGUERO para Interface MAGGI """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    local-motorista-ds_nome_fantazia          = i_inbound-data-motorista-nome.
    local-motorista-ds_razao_social           = i_inbound-data-motorista-nome.
    local-motorista-ds_logradouro_rua         = i_inbound-data-motorista-endereco-logradouro.
    local-motorista-ds_logradouro_nro         = i_inbound-data-motorista-endereco-numero.
    local-motorista-ds_logradouro_bairro      = i_inbound-data-motorista-endereco-bairro.

    "Validar CEP
    FIND REGEX '[0-9]{8}' IN i_inbound-data-motorista-endereco-cep.
    IF sy-subrc IS INITIAL.
      local-motorista-ds_logradouro_cep         = i_inbound-data-motorista-endereco-cep(5) && '-' && i_inbound-data-motorista-endereco-cep+5(3).
    ENDIF.

    local-motorista-ds_logradouro_uf          = i_inbound-data-motorista-endereco-uf.
    local-motorista-ds_logradouro_cidade      = i_inbound-data-motorista-endereco-municipio-nome.
    local-motorista-ds_logradouro_cidade_ibge = i_inbound-data-motorista-endereco-municipio-codigo_ibge.
    local-motorista-ds_email                  = i_inbound-data-motorista-email.
    local-motorista-ds_doc_cpf                = i_inbound-data-motorista-cpf.
    local-motorista-ds_doc_pis                = i_inbound-data-motorista-pis.
    local-motorista-ds_doc_rg                 = i_inbound-data-motorista-rg-numero.
    local-motorista-ds_doc_rg_orgao_emissor   = i_inbound-data-motorista-rg-orgao_emissor-nome.
    local-motorista-ds_doc_rg_uf_emissor      = i_inbound-data-motorista-rg-orgao_emissor-uf.
    local-motorista-dt_nascimento             = i_inbound-data-motorista-data_nascimento.
    local-motorista-ds_sexo                   = i_inbound-data-motorista-sexo.
    local-motorista-ds_doc_cnh                = i_inbound-data-motorista-cnh-numero.
    local-motorista-ds_nome_mae               = i_inbound-data-motorista-filiacao-nome_mae.

    LOOP AT i_inbound-data-motorista-contatos INTO DATA(wa_contatos_moto).
      IF wa_contatos_moto-tipo  EQ '1'.
        local-motorista-ds_telefone = wa_contatos_moto-telefone.
      ENDIF.
      IF wa_contatos_moto-tipo EQ '2' .
        IF local-motorista-ds_telefone IS INITIAL.
          local-motorista-ds_telefone = wa_contatos_moto-telefone.
        ENDIF.
        local-motorista-ds_mobile = wa_contatos_moto-telefone.
      ENDIF.
    ENDLOOP.

    IF i_inbound-data-unidades_veiculares IS NOT INITIAL. "174424 Int. Strada Log criação viagem validar JSON PSA

      LOOP AT i_inbound-data-unidades_veiculares INTO DATA(lc_unidade_veicular).

        CLEAR: lva_cpf_prop,lva_cpf_transp, lc_veiculos.

        lc_veiculos-ds_placa             = lc_unidade_veicular-placa. "1 Tipo    STRING  0 0 Placa
        lc_veiculos-ds_chassi            = lc_unidade_veicular-chassi. "1 Tipo   STRING  0 0 Chassi
        lc_veiculos-ds_renavam           = lc_unidade_veicular-renavam. "1 Tipo    STRING  0 0 RENAVAN
        lc_veiculos-ds_uf                = lc_unidade_veicular-licenciamento-uf. "1 Tipo   STRING  0 0 UF
        lc_veiculos-ds_cidade_ibge       = lc_unidade_veicular-licenciamento-municipio-codigo_ibge. "1 Tipo    STRING  0 0 Código da Cidade do IBGE
        lc_veiculos-ds_ano               = lc_unidade_veicular-licenciamento-ano_exercicio. "1 Tipo    STRING  0 0 Ano
        lc_veiculos-ds_qtd_eixos         = lc_unidade_veicular-eixos. "1 Tipo    STRING  0 0 Qtde de eixos
        lc_veiculos-ds_ordem             = lc_unidade_veicular-ordem.

        "0  CTC 1 ETC 2 ETC-EQUIPARADO 3  TAC
        CONDENSE lc_unidade_veicular-proprietario_arredantario-tipo-descricao NO-GAPS.
        lc_veiculos-ds_tipo_proprietario = COND string( WHEN lc_unidade_veicular-proprietario_arredantario-tipo-descricao = 'CTC' THEN '0'
                                                        WHEN lc_unidade_veicular-proprietario_arredantario-tipo-descricao = 'ETC' THEN '1'
                                                        WHEN lc_unidade_veicular-proprietario_arredantario-tipo-descricao = 'ETC-EQUIPARADO' THEN '2'
                                                        WHEN lc_unidade_veicular-proprietario_arredantario-tipo-descricao = 'TAC' THEN '3' ).

        lc_veiculos-ds_tara              = lc_unidade_veicular-peso_tara_estimado. "1 Tipo   STRING  0 0 Tara

        CASE zcl_string=>upper( lc_unidade_veicular-capacidade-unidade_medida ).
          WHEN 'KG'.
            lc_veiculos-ds_cap_kg = lc_unidade_veicular-capacidade-quantidade. "1 Tipo   STRING  0 0 Capacidade em KG
          WHEN 'M3'.
            lc_veiculos-ds_cap_m3 = lc_unidade_veicular-capacidade-quantidade. "1 Tipo   STRING  0 0 Capacidade M3
        ENDCASE.

        "LC_VEICULOS-DS_MARCA         "1 Tipo   STRING  0 0 Marca
        "LC_VEICULOS-DS_MODELO        "1 Tipo   STRING  0 0 Modelo
        "LC_VEICULOS-DS_COR           "1 Tipo   STRING  0 0 Cor
        lc_veiculos-ds_tp_veiculo     = COND string( WHEN lc_unidade_veicular-cavalo = abap_true THEN '0' ELSE '1' )."0 - Tração / 1 - Reboque

        "LC_VEICULOS-DS_TP_RODADO      = LC_UNIDADE_VEICULAR-TP_RODADO. "1 Tipo   STRING  0 0 03 - Cavalo Mecânico (0-Tração) / 00 - Não Ap. (1-Reboque)
        IF zcl_string=>length( text = i_inbound-data-conjunto_transportador-referencia_integracao ) GE 2.
          lc_veiculos-ds_tp_rodado = i_inbound-data-conjunto_transportador-referencia_integracao(2).
        ELSEIF lc_unidade_veicular-cavalo EQ abap_true.
          lc_veiculos-ds_tp_rodado = '03'. "03 - Cavalo Mecânico (0-Tração)
        ELSEIF lc_unidade_veicular-cavalo EQ abap_false.
          lc_veiculos-ds_tp_rodado = '00'. "00 - Não Ap. (1-Reboque)
        ENDIF.

        "LC_VEICULOS-DS_TP_CARROCERIA  = LC_UNIDADE_VEICULAR-TP_CARROCERIA. "1 Tipo   STRING  0 0 Tipo da Carroceria
        IF zcl_string=>length( text = i_inbound-data-tipo_carroceria-referencia_integracao ) GE 2.
          lc_veiculos-ds_tp_carroceria  = i_inbound-data-tipo_carroceria-referencia_integracao(2). "1 Tipo   STRING  0 0 Tipo da Carroceria
        ENDIF.

        "LC_VEICULOS-DS_AGREGADO      "1 Tipo   STRING  0 0 1 - Sim / 2 - Não (Placa é Agregada)

        lc_veiculos-ds_comodato = COND string( WHEN lc_unidade_veicular-proprietario_arredantario-proprietario = abap_true THEN '2' ELSE '1' )."1 - Sim / 2 - Não (Contrato de Comodato)

        IF zcl_string=>length( text = lc_unidade_veicular-proprietario_arredantario-documentos-cpf_cnpj ) GT 11.
          lc_veiculos-ds_doc_cnpj = lc_unidade_veicular-proprietario_arredantario-documentos-cpf_cnpj.
        ELSE.
          lc_veiculos-ds_doc_cpf  = lc_unidade_veicular-proprietario_arredantario-documentos-cpf_cnpj.
        ENDIF.
        APPEND lc_veiculos TO local-veiculos.

        "IF LC_UNIDADE_VEICULAR-CAVALO EQ ABAP_TRUE.
        CLEAR: lc_proprietario.
        IF zcl_string=>length( text = lc_unidade_veicular-proprietario_arredantario-documentos-cpf_cnpj ) GT 11.
          lc_proprietario-ds_doc_cnpj = lc_unidade_veicular-proprietario_arredantario-documentos-cpf_cnpj.
        ELSE.
          lc_proprietario-ds_doc_cpf  = lc_unidade_veicular-proprietario_arredantario-documentos-cpf_cnpj.
          lva_cpf_prop = lc_proprietario-ds_doc_cpf.
        ENDIF.
        lc_proprietario-ds_razao_social           = lc_unidade_veicular-proprietario_arredantario-documentos-razao_social.
        lc_proprietario-ds_nome_fantazia          = lc_unidade_veicular-proprietario_arredantario-documentos-nome_fantasia.
        lc_proprietario-ds_doc_ie                 = lc_unidade_veicular-proprietario_arredantario-documentos-inscricao_estadual.
        lc_proprietario-ds_doc_rntrc              = lc_unidade_veicular-proprietario_arredantario-documentos-rntrc.
        lc_proprietario-ds_doc_pis                = lc_unidade_veicular-proprietario_arredantario-documentos-pis_transportador_tac.
        lc_proprietario-ds_logradouro_rua         = lc_unidade_veicular-proprietario_arredantario-endereco-logradouro.
        lc_proprietario-ds_logradouro_nro         = lc_unidade_veicular-proprietario_arredantario-endereco-numero.
        lc_proprietario-ds_logradouro_bairro      = lc_unidade_veicular-proprietario_arredantario-endereco-bairro.
        lc_proprietario-ds_placa_cavalo           = lc_unidade_veicular-cavalo.  "*-IR 219949-29.01.2025-#165179-JT-inicio

        "-CS2023000160 - 28.03.2023 - Inicio
*      IF lc_veiculos-ds_tipo_proprietario EQ '3'.
*        lc_proprietario-ds_doc_pis              = local-motorista-ds_doc_pis.
*      ENDIF.

        "-CS2023000160 - 28.03.2023 - Inicio

        IF zcl_string=>length( text =  i_inbound-data-transportador-documentos-cpf_cnpj ) LE 11.
          lva_cpf_transp  = i_inbound-data-transportador-documentos-cpf_cnpj.
        ENDIF.

*      IF lva_cpf_prop EQ lva_cpf_transp AND lva_cpf_transp is NOT INITIAL.
*        lc_proprietario-ds_doc_pis = i_inbound-data-transportador-documentos-pis_transportador_tac.
*      ENDIF.

        CASE lc_unidade_veicular-proprietario_arredantario-tipo-descricao.
          WHEN 'CTC'.
            lc_proprietario-ds_indtyp  = 'Z3'.
          WHEN 'TAC'.
            IF lva_cpf_prop IS NOT INITIAL.
              lc_proprietario-ds_stcd5 = '782510/712'.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

        "-CS2023000160 - 28.03.2023 - Fim
        "Validar CEP
        FIND REGEX '[0-9]{8}' IN lc_unidade_veicular-proprietario_arredantario-endereco-cep.
        IF sy-subrc IS INITIAL.
          lc_proprietario-ds_logradouro_cep         = lc_unidade_veicular-proprietario_arredantario-endereco-cep(5) && '-' && lc_unidade_veicular-proprietario_arredantario-endereco-cep+5(3).
        ENDIF.
        lc_proprietario-ds_logradouro_uf          = lc_unidade_veicular-proprietario_arredantario-endereco-uf.
        lc_proprietario-ds_logradouro_cidade      = lc_unidade_veicular-proprietario_arredantario-endereco-municipio-nome.
        lc_proprietario-ds_logradouro_cidade_ibge = lc_unidade_veicular-proprietario_arredantario-endereco-municipio-codigo_ibge.
        lc_proprietario-ds_email                  = lc_unidade_veicular-proprietario_arredantario-contatos-email.
        lc_proprietario-ds_telefone               = lc_unidade_veicular-proprietario_arredantario-contatos-telefone.
        lc_proprietario-dt_nascimento             = lc_unidade_veicular-proprietario_arredantario-data_nascimento.
        lc_proprietario-ds_sexo                   = lc_unidade_veicular-proprietario_arredantario-sexo.
        lc_proprietario-ds_nome_mae               = lc_unidade_veicular-proprietario_arredantario-filiacao-nome_mae.
        "ENDIF.

        READ TABLE local-proprietario WITH KEY ds_doc_cnpj = lc_proprietario-ds_doc_cnpj ds_doc_cpf = lc_proprietario-ds_doc_cpf TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          APPEND lc_proprietario TO local-proprietario.
        ENDIF.

      ENDLOOP.

    ELSE. "174424 Int. Strada Log criação viagem validar JSON PSA
      lc_texto = 'Unidades Veiculares devem conter dados. Verificar com o time do Strada.'.
      wa_tx-tx_validacao_viagem = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_viagem s2 = lc_texto sp = zcl_string=>at_separador_padrao ).
      lc_erro = abap_true.
    ENDIF.

    LOOP AT local-proprietario INTO DATA(wa_proprietario).

      " Cadastro de Fornecedor de Frete """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF lc_erro NE abap_true.

        DATA(lc_erro_proprietario) = abap_false.
        IF local-proprietario IS NOT INITIAL.
          TRY.
              me->set_cad_fornecedor_frete(
                EXPORTING
                  i_bukrs          = wa_lote_frete-bukrs
                  i_branch         = wa_lote_frete-branch
                  i_fornecedor     = wa_proprietario
                IMPORTING
                  e_parceiro       = DATA(e_parceiro_prop)
                  e_msg            = DATA(e_msg_proprietario)
                  e_msg_validacao  = DATA(e_msg_val_prop)
                  e_outbound_forne = DATA(e_outbound_forne)
              ).
            CATCH zcx_integracao INTO DATA(ex_integracao).
              lc_erro_proprietario = abap_true.
              DATA(e_msg_erro) = ex_integracao->zif_error~get_msg_erro( ).
            CATCH zcx_error INTO DATA(ex_error).
              lc_erro_proprietario = abap_true.
              e_msg_erro = ex_error->zif_error~get_msg_erro( ).
          ENDTRY.

          wa_tx-tx_validacao_prop = zcl_string=>concat( s1 = e_msg_val_prop s2 = e_msg_erro sp = zcl_string=>at_separador_padrao ).

          IF e_outbound_forne IS NOT INITIAL.
            APPEND e_outbound_forne TO it_fornecedores.
*-IR 219949-29.01.2025-#165179-JT-inicio
            IF wa_proprietario-ds_placa_cavalo = abap_true.
              MOVE e_outbound_forne           TO lc_outbound_forne.
              MOVE wa_proprietario-ds_doc_pis TO lc_outbound_forne-id_pis.
            ENDIF.
*-IR 219949-29.01.2025-#165179-JT-fim
          ENDIF.

          MESSAGE e_msg_proprietario TYPE 'S'.

          IF e_msg_proprietarios IS INITIAL.
            e_msg_proprietarios = e_msg_proprietario.
          ELSE.
            CONCATENATE e_msg_proprietarios e_msg_proprietario INTO e_msg_proprietarios SEPARATED BY ' --- '.
          ENDIF.
        ELSE.
          lc_erro_proprietario = abap_true.
          wa_tx-tx_validacao_prop = 'Não foi informado o proprietário do conjunto veicular!'.
          e_msg_proprietarios = wa_tx-tx_validacao_prop.
        ENDIF.

        IF lc_erro EQ abap_false.
          lc_erro = lc_erro_proprietario.
        ENDIF.
      ENDIF.

      " Cadastro de Veículo """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF lc_erro NE abap_true.
        DATA(lc_erro_veiculo) = abap_false.
        IF local-veiculos[] IS NOT INITIAL.

          LOOP AT local-veiculos INTO DATA(wa_veiculo) WHERE ds_doc_cnpj EQ wa_proprietario-ds_doc_cnpj
                                                         AND ds_doc_cpf  EQ wa_proprietario-ds_doc_cpf.
            IF lc_erro_veiculo EQ abap_true.
              CONTINUE.
            ENDIF.

            TRY .
                "Cadastra Veículo
                me->set_cad_veiculo(
                  EXPORTING
                    i_veiculo       = wa_veiculo
                    i_proprietario  = e_parceiro_prop
                  IMPORTING
                    e_msg           = DATA(e_msg_veiculo)
                    e_msg_validacao = DATA(e_msg_val_veic)
                    e_veiculo       = DATA(e_veiculo)
                ).

                MESSAGE e_msg_veiculo TYPE 'S'.

                IF e_veiculo IS NOT INITIAL.
                  APPEND e_veiculo TO it_veiculos.
                ENDIF.

              CATCH zcx_integracao INTO ex_integracao.
                lc_erro_veiculo = abap_true.
                DATA(e_msg_erro_veiculo) = ex_integracao->zif_error~get_msg_erro( ).
              CATCH zcx_error INTO ex_error.
                lc_erro_veiculo = abap_true.
                e_msg_erro_veiculo = ex_error->zif_error~get_msg_erro( ).
            ENDTRY.

            wa_tx-tx_validacao_veiculos = zcl_string=>concat( s1 = e_msg_val_veic s2 = e_msg_erro_veiculo sp = zcl_string=>at_separador_padrao ).

            IF e_msg_veiculos IS INITIAL.
              e_msg_veiculos = e_msg_veiculo.
            ELSE.
              CONCATENATE e_msg_veiculos e_msg_veiculo INTO e_msg_veiculos SEPARATED BY ' --- '.
            ENDIF.

          ENDLOOP.

        ELSE.
          lc_erro_veiculo = abap_true.
          wa_tx-tx_validacao_veiculos = 'Não foi informado veículo(s) para o transporte!'.
          e_msg_proprietarios = wa_tx-tx_validacao_prop.
        ENDIF.

        IF lc_erro EQ abap_false.
          lc_erro = lc_erro_veiculo.
        ENDIF.
      ENDIF.

    ENDLOOP.

*-CS2021000253-26.04.2024-#59941-JT-inicio
*=======================================================================
* Valida / criacao Transportador
*=======================================================================
    IF lc_erro <> abap_true.
      DATA(lc_cadastra_transp) = abap_off.
      DATA(lc_expandir_transp) = abap_off.
      DATA(lc_erro_rodoviario) = abap_off.
      DATA(lc_erro_ov)         = abap_off.
      DATA(lc_erro_pc)         = abap_off.

*-CS2021000253-26.04.2024-#59941-JT-BUG 145002-inicio
      SELECT SINGLE *
        INTO @DATA(w_lfa1)
        FROM lfa1
       WHERE stcd1 = @i_inbound-data-transportador-documentos-cpf_cnpj
         AND ktokk = 'ZFIC'
         AND loevm = @abap_off
         AND sperr = @abap_off
         AND sperm = @abap_off.

      IF sy-subrc = 0.
        lc_cadastra_transp = abap_false.
        lc_expandir_transp = abap_false.
*-CS2021000253-26.04.2024-#59941-JT-BUG 145002-fim
      ELSE.
        SELECT SINGLE *
          INTO w_lfa1
          FROM lfa1
         WHERE stcd1 = i_inbound-data-transportador-documentos-cpf_cnpj
           AND ktokk = 'ZFFJ'
           AND loevm = abap_off
           AND sperr = abap_off
           AND sperm = abap_off.

        IF sy-subrc <> 0.
          lc_cadastra_transp = abap_true.
        ELSE.
          TRY .
              zcl_fornecedores=>zif_parceiros~get_instance(
                )->set_parceiro( i_parceiro = w_lfa1-lifnr
                )->ck_servico_frete(
                ).
            CATCH zcx_parceiros INTO DATA(ex_cad_transp).
              lc_erro_rodoviario = abap_true.
          ENDTRY.

          IF lc_erro_rodoviario = abap_true.
            lc_cadastra_transp = abap_true.
          ELSE.
            "Verificar Org.venda da OV ou pedido compras =======================================
            IF     wa_zlest0185-vbeln IS NOT INITIAL.
              TRY .
                  zcl_ordem_venda=>zif_ordem_venda~get_instance(
                    )->set_ordem_venda( EXPORTING i_vbeln       = wa_lote_frete-vbeln
                    )->get_ordem_venda( IMPORTING e_ordem_venda = DATA(e_ov) ).
                CATCH zcx_ordem_venda INTO DATA(ex_ov).    "
                  lc_erro_ov = abap_true.
              ENDTRY.
            ELSEIF wa_zlest0185-ebeln IS NOT INITIAL.
              TRY.
                  zcl_pedido_compra=>get_instance(
                    )->set_pedido_pc( EXPORTING i_ebeln         = wa_lote_frete-ebeln
                    )->get_pedido_pc( IMPORTING e_pedido_compra = DATA(e_pc) ).
                CATCH zcx_pedido_compra INTO DATA(ex_pc).    "
                  lc_erro_pc = abap_true.
              ENDTRY.
            ENDIF.

            IF ( wa_zlest0185-vbeln IS NOT INITIAL AND lc_erro_ov = abap_false ) OR              "#172365-26.03.2025-#172365-JT
               ( wa_zlest0185-ebeln IS NOT INITIAL AND lc_erro_pc = abap_false ).                "#172365-26.03.2025-#172365-JT
              DATA(lc_vkorg) = COND #( WHEN wa_zlest0185-vbeln IS NOT INITIAL THEN e_ov-vkorg
                                                                              ELSE e_pc-bukrs ). "e_pc-ekorg ). #172365-26.03.2025-#172365-JT
              TRY .
                  zcl_fornecedores=>zif_parceiros~get_instance(
                    )->set_parceiro(     i_parceiro = w_lfa1-lifnr
                    )->ck_ativo_empresa( i_empresa  = lc_vkorg
                    )->ck_ativo(
                    ).
                CATCH zcx_parceiros INTO ex_cad_transp.
                  lc_expandir_transp = abap_true.
              ENDTRY.
            ENDIF .
          ENDIF.
        ENDIF.
      ENDIF.

      IF lc_cadastra_transp = abap_true.
        "Criar fornecedor """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        CLEAR: lc_transp.

        IF zcl_string=>length( text = i_inbound-data-transportador-documentos-cpf_cnpj ) GT 11.
          lc_transp-ds_doc_cnpj = i_inbound-data-transportador-documentos-cpf_cnpj.
        ELSE.
          lc_transp-ds_doc_cpf  = i_inbound-data-transportador-documentos-cpf_cnpj.
          lva_cpf_transp        = lc_transp-ds_doc_cpf.
        ENDIF.

        lc_transp-ds_razao_social           = i_inbound-data-transportador-documentos-razao_social.
        lc_transp-ds_nome_fantazia          = i_inbound-data-transportador-documentos-nome_fantasia.
        lc_transp-ds_doc_ie                 = i_inbound-data-transportador-documentos-inscricao_estadual.
        lc_transp-ds_doc_rntrc              = i_inbound-data-transportador-documentos-rntrc.
        lc_transp-ds_doc_pis                = i_inbound-data-transportador-documentos-pis_transportador_tac.
        lc_transp-ds_logradouro_rua         = i_inbound-data-transportador-endereco-logradouro.
        lc_transp-ds_logradouro_nro         = i_inbound-data-transportador-endereco-numero.
        lc_transp-ds_logradouro_bairro      = i_inbound-data-transportador-endereco-bairro.

        CASE i_inbound-data-transportador-tipo-descricao.
          WHEN 'CTC'.
            lc_transp-ds_indtyp  = 'Z3'.
          WHEN 'TAC'.
            IF lva_cpf_transp IS NOT INITIAL.
              lc_transp-ds_stcd5 = '782510/712'.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

        "Validar CEP
        FIND REGEX '[0-9]{8}' IN i_inbound-data-transportador-endereco-cep.
        IF sy-subrc IS INITIAL.
          lc_transp-ds_logradouro_cep = i_inbound-data-transportador-endereco-cep(5) && '-' && i_inbound-data-transportador-endereco-cep+5(3).
        ENDIF.

        lc_transp-ds_logradouro_uf          = i_inbound-data-transportador-endereco-uf.
        lc_transp-ds_logradouro_cidade      = i_inbound-data-transportador-endereco-municipio-nome.
        lc_transp-ds_logradouro_cidade_ibge = i_inbound-data-transportador-endereco-municipio-codigo_ibge.
        lc_transp-ds_email                  = i_inbound-data-transportador-contatos-email.
        lc_transp-ds_telefone               = i_inbound-data-transportador-contatos-telefone.
        lc_transp-dt_nascimento             = i_inbound-data-transportador-data_nascimento.
        lc_transp-ds_sexo                   = i_inbound-data-transportador-sexo.
        lc_transp-ds_nome_mae               = i_inbound-data-transportador-filiacao-nome_mae.

        DATA(lc_erro_transp) = abap_false.

        TRY.
            me->set_cad_fornecedor_frete(
              EXPORTING
                i_bukrs          = wa_lote_frete-bukrs
                i_branch         = wa_lote_frete-branch
                i_fornecedor     = lc_transp
              IMPORTING
                e_parceiro       = DATA(e_parceiro_transp)
                e_msg            = DATA(e_msg_transp)
                e_msg_validacao  = DATA(e_msg_val_transp)
                e_outbound_forne = DATA(e_outbound_forne_transp)
            ).
          CATCH zcx_integracao INTO DATA(ex_integracao_transp).
            lc_erro_transp = abap_true.
            DATA(e_msg_erro_transp) = ex_integracao_transp->zif_error~get_msg_erro( ).
          CATCH zcx_error INTO DATA(ex_error_transp).
            lc_erro_transp = abap_true.
            e_msg_erro_transp = ex_error_transp->zif_error~get_msg_erro( ).
        ENDTRY.

        wa_tx-tx_validacao_prop = zcl_string=>concat( s1 = e_msg_val_transp s2 = e_msg_erro_transp sp = zcl_string=>at_separador_padrao ).

        IF e_outbound_forne_transp IS NOT INITIAL.
          APPEND e_outbound_forne_transp TO it_fornecedores.
        ENDIF.

        IF lc_erro = abap_false.
          lc_erro = lc_erro_transp.
        ENDIF.

        MESSAGE e_msg_transp TYPE 'S'.

      ELSEIF lc_expandir_transp = abap_true.
        "Expandir fornecedor """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        SELECT *
          INTO @DATA(w_lfb1)
          FROM lfb1
            UP TO 1 ROWS
         WHERE lifnr = @w_lfa1-lifnr.
        ENDSELECT.

        zcl_mestre_fornecedor=>get_instance(
               )->expandir_dados_fornecedor(
          EXPORTING
            i_lfb1           = w_lfb1                     " Mestre de fornecedores (empresa)
            i_lfa1           = w_lfa1                     " Mestre de fornecedores (parte geral)
            i_bukrs          = lc_vkorg                   " Empresa
          IMPORTING
            gs_err_messages  = DATA(w_err_messages)       " Indicador de erro e mensagens do sistema
            gs_succ_messages = DATA(w_succ_messages) ).   " Indicador de sucesso e mensagens do sistema

        IF w_err_messages-is_error = abap_true.
          lc_erro_transp = abap_true.
          LOOP AT w_err_messages-messages INTO DATA(w_message).
            e_msg_erro_transp = 'Erro expansão Fornecedor:' && w_message-message.
            wa_tx-tx_validacao_prop = zcl_string=>concat( s1 = 'Erro expansão Fornecedor:' s2 = CONV #( w_message-message ) sp = zcl_string=>at_separador_padrao ).
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
*-CS2021000253-26.04.2024-#59941-JT-fim

    "Ordernar Placas do Conjunto Veicular """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(it_veiculos_aux)  = it_veiculos[].
    DATA(it_veiculos_sort) = it_veiculos[].

    CLEAR: it_veiculos_sort[].

    SORT local-veiculos BY ds_ordem.
    LOOP AT local-veiculos INTO wa_veiculo.
      READ TABLE it_veiculos_aux INTO DATA(lwa_veiculos_aux) WITH KEY pc_veiculo = wa_veiculo-ds_placa.
      IF sy-subrc EQ 0.
        APPEND lwa_veiculos_aux TO it_veiculos_sort.
      ENDIF.
    ENDLOOP.

    IF lines( it_veiculos_sort[] ) EQ lines( it_veiculos[] ).
      it_veiculos[] = it_veiculos_sort[].
    ENDIF.

    " Cadastro de Motorista """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF lc_erro NE abap_true.
      DATA(lc_erro_motorista) = abap_false.
      TRY .
          me->set_cad_motorista(
            EXPORTING
              i_fornecedor     = local-motorista
            IMPORTING
              e_parceiro       = DATA(e_parceiro_moto)
              e_msg_validacao  = DATA(e_msg_val_mot)
              e_msg            = DATA(e_msg_motorista)
              e_outbound_forne = DATA(e_outbound_forne_mot) ).
          MESSAGE e_msg_motorista TYPE 'S'.
          IF e_outbound_forne_mot IS NOT INITIAL.
            MOVE local-motorista-ds_doc_pis TO e_outbound_forne_mot-id_pis. "*-IR 219949-29.01.2025-#165179-JT
            APPEND e_outbound_forne_mot     TO it_fornecedores.
          ENDIF.

*          SELECT SINGLE lifnr
*            FROM lfa1 INTO lc_lifnr_motorista
*           WHERE stcd2 = local-motorista-ds_doc_cpf
*             AND ktokk = 'ZMOT'.
*
*          IF sy-subrc NE 0.
*            lc_lifnr_motorista.
*          ENDIF.

          CLEAR: e_msg_motorista.
        CATCH zcx_integracao INTO ex_integracao.

          lc_erro_motorista = abap_true.
          e_msg_erro = ex_integracao->zif_error~get_msg_erro( ).
          e_msg_motorista = e_msg_erro.

        CATCH zcx_shdb INTO DATA(ex_shdb).

          lc_erro_motorista = abap_true.
          e_msg_erro = ex_shdb->zif_error~get_msg_erro( ).
          e_msg_motorista = e_msg_erro.

        CATCH zcx_error INTO ex_error.

          lc_erro_motorista = abap_true.
          e_msg_erro = ex_error->zif_error~get_msg_erro( ).
          e_msg_motorista = e_msg_erro.

      ENDTRY.
      wa_tx-tx_validacao_mot = zcl_string=>concat( s1 = e_msg_val_mot s2 = e_msg_motorista sp = zcl_string=>at_separador_padrao ).
      IF lc_erro EQ abap_false.
        lc_erro = lc_erro_motorista.
      ENDIF.
    ENDIF.


*-CS2023000070-21.03.2023-#103477-JT-inicio
    " verificar se ordem / pedido estao paramerizados bloqueio OC """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF lc_erro NE abap_true AND wa_lote_frete-sem_oc_opus NE 'X'. "WPP 16.05.2025  Integração SOlicitação Recebimento US 169676
      FREE: t_0001od, lc_auart, lc_bsart, lc_matkl, lc_lifnr.

      lc_data = sy-datum - 90.

      IF     wa_zlest0185-vbeln IS NOT INITIAL.
        SELECT SINGLE auart
          INTO lc_auart
          FROM vbak
         WHERE vbeln = wa_zlest0185-vbeln.
      ELSEIF wa_zlest0185-ebeln IS NOT INITIAL.
        SELECT SINGLE bsart
          INTO lc_bsart
          FROM ekko
         WHERE ebeln = wa_zlest0185-ebeln.
      ENDIF.

*--------------------------------
*---- parametro OC
*--------------------------------
* Início - SD - CS2023000070 - Bloqueio de Oc - Correções #117578 RSA
*      SELECT *
*        INTO @DATA(w_zlest0230)
*        FROM zlest0230
*          UP TO 1 ROWS
*       WHERE auart       = @lc_auart
*         AND bsart       = @lc_bsart
*         AND valida      = '1'
*         AND parceiro_lr = @wa_lote_frete-id_parceiro_entr.
*      ENDSELECT.
*
*      IF sy-subrc = 0.
      " FIm - SD - CS2023000070 - Bloqueio de Oc - Correções #117578 RSA
      SELECT SINGLE matkl
        INTO lc_matkl
        FROM mara
       WHERE matnr = wa_lote_frete-matnr.

      SELECT SINGLE *
        FROM tvarvc
        INTO @DATA(lc_tvarvc)
       WHERE name = 'MAGGI_GR_GRAOS'
         AND low  = @lc_matkl.

      IF sy-subrc = 0.
        SELECT lifnr
          INTO lc_lifnr
            UP TO 1 ROWS
          FROM lfa1
         WHERE stcd2 = local-motorista-ds_doc_cpf
           AND ktokk = 'ZMOT'.
        ENDSELECT.

*-----------------------------------
*-------- Identificar as ordens de carregamento emitidas
*-------- nos últimos 60 dias para o motorista
*-----------------------------------
        SELECT *
          INTO TABLE t_0001od
          FROM zsdt0001od
         WHERE id_motorista = lc_lifnr
           AND dt_emissao  >= lc_data
           AND tp_status    = 'FE'.

*-----------------------------------
*------ Identificar as ordens de carregamento emitidas nos
*------ últimos 60 dias para a placa cavalo e demais placas
*-----------------------------------
        LOOP AT local-veiculos INTO wa_veiculo.
          IF     wa_veiculo-ds_tp_veiculo  = '0'.  "tracao
            SELECT *
         APPENDING TABLE t_0001od
              FROM zsdt0001od
             WHERE ds_placa_trator    = wa_veiculo-ds_placa
               AND dt_emissao        >= lc_data
               AND tp_status          = 'FE'.

          ELSEIF wa_veiculo-ds_tp_veiculo  = '1'.  "reboque
            SELECT *
         APPENDING TABLE t_0001od
              FROM zsdt0001od
             WHERE ( ds_placa_reboq_1 = wa_veiculo-ds_placa
                OR   ds_placa_reboq_2 = wa_veiculo-ds_placa
                OR   ds_placa_reboq_3 = wa_veiculo-ds_placa )
               AND   dt_emissao      >= lc_data
               AND   tp_status        = 'FE'.
          ENDIF.
        ENDLOOP.

        IF t_0001od[] IS NOT INITIAL.
          SELECT vttk~id_ordem,    vttk~tknum,           vttp~vbeln,
                 zlest0039~docnum, zlest0039~pesotransb, zlest0039~pesochegada
            INTO TABLE @DATA(t_zlest0039)
            FROM vttk
           INNER JOIN vttp       ON vttp~tknum      = vttk~tknum
           INNER JOIN zlest0039  ON zlest0039~vbeln = vttp~vbeln
                                AND tp_baixa        = @abap_off
             FOR ALL ENTRIES IN @t_0001od
           WHERE vttk~id_ordem = @t_0001od-id_ordem.

          lc_peso_ok = abap_true.

          LOOP AT t_zlest0039 INTO DATA(w_zlest0039).
            IF w_zlest0039-pesotransb  IS INITIAL AND
               w_zlest0039-pesochegada IS INITIAL.
              lc_peso_ok = abap_false.
              EXIT.
            ENDIF.
          ENDLOOP.
          " Início - SD - CS2023000070 - Bloqueio de Oc - Correções #117578 RSA
          SELECT *
                 INTO @DATA(w_zlest0230)
                 FROM zlest0230
                 UP TO 1 ROWS
                 "WHERE auart       = @lc_auart "CS2023000070 - DEVK9A1RRV
                 "AND   bsart       = @lc_bsart "CS2023000070 - DEVK9A1RRV
                 WHERE valida      = '1'
                 AND   parceiro_lr = @wa_lote_frete-id_parceiro_entr.
          ENDSELECT.
          " FIm - SD - CS2023000070 - Bloqueio de Oc - Correções #117578 RSA
          IF sy-subrc = 0.
            IF lc_peso_ok = abap_off.
              READ TABLE t_0001od INTO w_0001od WITH KEY id_ordem = w_zlest0039-id_ordem.
              lc_erro = abap_true.
              wa_tx-tx_validacao_viagem = |{ 'Faturamento pendente de descarga. Docnum:' } { w_zlest0039-docnum } { '- OC: ' } { w_0001od-nr_ordem }|.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
      "ENDIF. "Início - SD - CS2023000070 - Bloqueio de Oc - Correções #117578 RSA

      "Início - SD - CS2023000070 - Bloqueio de Oc - Correções #117578 RSA
      IF NOT t_zlest0039[] IS INITIAL AND lc_erro NE abap_true.

        wa_vgtyp-sign   = 'I'.
        wa_vgtyp-option = 'EQ'.
        wa_vgtyp-low    = 'C'.
        APPEND wa_vgtyp TO rg_vgtyp.
        wa_vgtyp-low    = 'V'.
        APPEND wa_vgtyp TO rg_vgtyp.

        SELECT vbeln, vgbel
               FROM lips
               INTO TABLE @DATA(t_lips)
               FOR ALL ENTRIES IN @t_zlest0039
               WHERE vbeln EQ @t_zlest0039-vbeln
               AND   vgtyp IN @rg_vgtyp.

        SELECT vbeln, kunnr
               FROM vbpa
               INTO TABLE @DATA(t_vbpa)
               FOR ALL ENTRIES IN @t_zlest0039
               WHERE vbeln  EQ @t_zlest0039-vbeln
               AND   parvw  EQ 'LR'.

        SORT t_vbpa BY vbeln.

        SELECT *
               FROM tvarvc
               INTO TABLE @DATA(t_emp_joint_vt)
               WHERE name = 'EMPRESAS_JOINT_VENTURE'.

        LOOP AT t_lips INTO DATA(w_lips).

          IF lc_erro = abap_true.
            CONTINUE.
          ENDIF.

          SELECT SINGLE auart, bukrs_vf
                 FROM vbak
                 INTO @DATA(w_vbak)
                 WHERE vbeln EQ @w_lips-vgbel.

          lc_emp_join_vt = w_vbak-bukrs_vf.
          READ TABLE t_emp_joint_vt WITH KEY low = lc_emp_join_vt TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            READ TABLE t_vbpa INTO DATA(w_vbpa) WITH KEY vbeln = w_lips-vbeln BINARY SEARCH.
            IF sy-subrc EQ 0.
              SELECT SINGLE parceiro_lr
                     FROM zlest0230
                     INTO @DATA(w_zlest0230_par_lr)
                     WHERE parceiro_lr EQ @w_vbpa-kunnr.
              IF sy-subrc EQ 0.
                READ TABLE t_zlest0039 INTO w_zlest0039 WITH KEY vbeln = w_lips-vbeln.
                READ TABLE t_0001od    INTO w_0001od WITH KEY id_ordem = w_zlest0039-id_ordem.
                lc_erro = abap_true.
                wa_tx-tx_validacao_viagem = |{ 'Faturamento pendente de descarga. Docnum:' } { w_zlest0039-docnum } { '- OC: ' } { w_0001od-nr_ordem }|.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ENDIF.

      "Fim - SD - CS2023000070 - Bloqueio de Oc - Correções #117578 RSA

    ENDIF.
*-CS2023000070-21.03.2023-#103477-JT-fim

    "Validar Valor do Frete Empresa e Motorista """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF lc_erro NE abap_true.
      DATA(lc_erro_preco) = abap_false.
      DATA: lva_msg_erro_preco TYPE string.

      READ TABLE local-veiculos INTO DATA(wa_tracao) WITH KEY ds_tp_veiculo = '0'.

*-----CS2019001158 - Jaime Tassoni - 16.11.2020 - inicio
      DO 10 TIMES.

        CLEAR: lva_msg_erro_preco.

        TRY.
            me->set_criar_condicao_tk11(
              EXPORTING
                i_lote_frete           = wa_lote_frete
                i_viagem_id            = wa_zlest0185-viagem_id
                i_placa_veic_tracao    = CONV #( wa_tracao-ds_placa )
                i_paga_trecho_terceiro = i_inbound-data-paga_trecho_terceiro
                i_valor_frete          = CONV #( i_inbound-data-valor_frete )
              IMPORTING
                e_msg_erro             = lva_msg_erro_preco
            ).

            EXIT.

          CATCH zcx_integracao INTO ex_integracao.
            lva_msg_erro_preco = ex_integracao->zif_error~get_msg_erro( ).
            IF lva_msg_erro_preco IS INITIAL.
              lva_msg_erro_preco = 'Erro desconhecido no Cadastro de Preco TK11'.
            ENDIF.

            WAIT UP TO 6 SECONDS.

        ENDTRY.
      ENDDO.

      IF lva_msg_erro_preco IS NOT INITIAL.
        lc_erro_preco            = abap_true.
        wa_tx-tx_validacao_preco = lva_msg_erro_preco && '-(Cadastro Preco TK11)'.
      ENDIF.

*-----CS2019001158 - Jaime Tassoni - 16.11.2020 - fim

      IF     wa_lote_frete-vbeln IS NOT INITIAL AND
             lc_erro_preco       IS INITIAL.
        TRY .
            zcl_ordem_venda=>zif_ordem_venda~get_instance(
              )->set_ordem_venda( i_vbeln = wa_lote_frete-vbeln
              )->get_ordem_venda( IMPORTING e_ordem_venda = DATA(e_ordem_venda)
              )->get_item( IMPORTING e_vbap = DATA(e_vbap)
              )->get_dados_comerciais( IMPORTING e_vbkd = DATA(e_vbkd)
              )->get_ck_valor_frete(
                  EXPORTING
                    i_valor_frete_empresa   = CONV #( i_inbound-data-valor_frete-valor_frete_empresa )
                    i_valor_frete_motorista = CONV #( i_inbound-data-valor_frete-valor_frete_motorista )
                    i_placa_veic_tracao     = CONV #( wa_tracao-ds_placa )
                    i_zlest0181             = wa_lote_frete
                    i_viagem_id             = wa_zlest0185-viagem_id
                    i_paga_trecho_terceiro  = i_inbound-data-paga_trecho_terceiro
                  IMPORTING
                    e_parametros_empresa     = DATA(e_parametros_empresa)
                    e_parametros_motorista   = DATA(e_parametros_motorista)
                    e_valor_frete_entrada    = DATA(e_valor_frete_entrada)
                    e_valor_frete_motorista  = DATA(e_valor_frete_motorista)
              )->get_parceiros( IMPORTING e_vbpa = DATA(e_vbpa)
              ).

            wa_zlest0185-nm_vr_frete_empresa   = CONV #( i_inbound-data-valor_frete-valor_frete_empresa ).
            wa_zlest0185-nm_vr_frete_motorista_entrada = e_valor_frete_entrada.
            wa_zlest0185-nm_vr_frete_motorista         = e_valor_frete_motorista.

          CATCH zcx_ordem_venda INTO DATA(ex_ordem_venda).    "
            lc_erro_preco = abap_true.
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = e_parametros_empresa s2 = e_parametros_motorista sp = zcl_string=>at_separador_padrao ).
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = wa_tx-tx_validacao_preco s2 = ex_ordem_venda->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
            DATA(lc_tx_ordem_venda) = ex_ordem_venda->zif_error~get_msg_erro( ).
          CATCH zcx_error INTO ex_error.    " .
            lc_erro_preco = abap_true.
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = e_parametros_empresa s2 = e_parametros_motorista sp = zcl_string=>at_separador_padrao ).
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = wa_tx-tx_validacao_preco s2 = ex_error->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
          CATCH zcx_calc_frete INTO DATA(ex_calc_frete).
            lc_erro_preco = abap_true.
            ex_calc_frete->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = e_parametros_empresa s2 = e_parametros_motorista sp = zcl_string=>at_separador_padrao ).
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = wa_tx-tx_validacao_preco s2 = mtext sp = zcl_string=>at_separador_padrao ).
        ENDTRY.
*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
      ELSEIF wa_lote_frete-ebeln IS NOT INITIAL AND
             lc_erro_preco       IS INITIAL.
        TRY.
            zcl_pedido_compra=>get_instance(
              )->set_pedido_pc( i_ebeln = wa_lote_frete-ebeln
              )->get_pedido_pc( IMPORTING e_pedido_compra = DATA(e_pedido_compra)
              )->get_item_pc( IMPORTING e_ekpo = DATA(e_ekpo)
                                        e_ekpv = DATA(e_ekpv)
                                        e_eket = DATA(e_eket)
              )->get_ck_valor_frete(
                  EXPORTING
                    i_valor_frete_empresa   = CONV #( i_inbound-data-valor_frete-valor_frete_empresa )
                    i_valor_frete_motorista = CONV #( i_inbound-data-valor_frete-valor_frete_motorista )
                    i_placa_veic_tracao     = CONV #( wa_tracao-ds_placa )
                    i_zlest0181             = wa_lote_frete
                    i_viagem_id             = wa_zlest0185-viagem_id
                    i_paga_trecho_terceiro  = i_inbound-data-paga_trecho_terceiro
                  IMPORTING
                    e_parametros_empresa     = e_parametros_empresa
                    e_parametros_motorista   = e_parametros_motorista
                    e_valor_frete_entrada    = e_valor_frete_entrada
                    e_valor_frete_motorista  = e_valor_frete_motorista
               )->get_parceiros( IMPORTING e_ekpa = DATA(e_ekpa)
               ).

            wa_zlest0185-nm_vr_frete_empresa   = CONV #( i_inbound-data-valor_frete-valor_frete_empresa ).
            wa_zlest0185-nm_vr_frete_motorista_entrada = e_valor_frete_entrada.
            wa_zlest0185-nm_vr_frete_motorista         = e_valor_frete_motorista.

          CATCH zcx_pedido_compra INTO DATA(ex_pedido_compra).    "
            lc_erro_preco = abap_true.
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = e_parametros_empresa s2 = e_parametros_motorista sp = zcl_string=>at_separador_padrao ).
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = wa_tx-tx_validacao_preco s2 = ex_pedido_compra->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
            DATA(lc_tx_pedido_compra) = ex_pedido_compra->zif_error~get_msg_erro( ).
          CATCH zcx_error INTO ex_error.    " .
            lc_erro_preco = abap_true.
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = e_parametros_empresa s2 = e_parametros_motorista sp = zcl_string=>at_separador_padrao ).
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = wa_tx-tx_validacao_preco s2 = ex_error->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
          CATCH zcx_calc_frete INTO ex_calc_frete.
            lc_erro_preco = abap_true.
            ex_calc_frete->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = e_parametros_empresa s2 = e_parametros_motorista sp = zcl_string=>at_separador_padrao ).
            wa_tx-tx_validacao_preco = zcl_string=>concat( s1 = wa_tx-tx_validacao_preco s2 = mtext sp = zcl_string=>at_separador_padrao ).
        ENDTRY.
*---CS2020000704 - JTASSONI - 01.10.2020 - fim
      ENDIF.

      IF lc_erro EQ abap_false.
        lc_erro = lc_erro_preco.
      ENDIF.
    ENDIF.

*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
    IF     wa_lote_frete-vbeln IS NOT INITIAL.
      READ TABLE e_vbpa INTO DATA(e_parceiro_ov_sp) WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_frete_ov.
      READ TABLE e_vbpa INTO DATA(e_parceiro_ov_pc) WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_cole_ov.
      READ TABLE e_vbpa INTO DATA(e_parceiro_ov_lr) WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_entr_ov.
      READ TABLE e_vbpa INTO DATA(e_parceiro_ov_z1) WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_porto_ov.
    ELSE.
      READ TABLE e_ekpa INTO DATA(e_parceiro_pc_sp) WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_frete_ov.
      READ TABLE e_ekpa INTO DATA(e_parceiro_pc_pc) WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_cole_pc.
      READ TABLE e_ekpa INTO DATA(e_parceiro_pc_lr) WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_entr_pc.
      READ TABLE e_ekpa INTO DATA(e_parceiro_pc_z1) WITH KEY parvw = zif_integracao_lote_frete=>at_tp_parceiro_porto_ov.
    ENDIF.
*---CS2020000704 - JTASSONI - 01.10.2020 - fim

    "Retornar por Interface os Veículos Criados """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF it_veiculos[] IS NOT INITIAL.
*--> 25.08.2023 16:16:45 - Migração S4 – ML - Início
*      CALL FUNCTION 'Z_LES_OUTBOUND_VEICULO' IN BACKGROUND TASK
*        DESTINATION 'XI_VEICULO'
*        AS SEPARATE UNIT
*        TABLES
*          it_veiculo = it_veiculos.

      DATA: lv_rfc TYPE rfcdest.

      CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_LES_OUTBOUND_VEICULO'.

      CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
        EXPORTING
          i_fm          = c_fm
        IMPORTING
          e_rfc         = lv_rfc
        EXCEPTIONS
          no_rfc        = 1
          no_rfc_config = 2
          OTHERS        = 3.

      IF sy-subrc EQ 0.
        CALL FUNCTION c_fm IN BACKGROUND TASK
          DESTINATION lv_rfc
          AS SEPARATE UNIT
          TABLES
            it_veiculo = it_veiculos.
      ELSE.
        CALL FUNCTION c_fm IN BACKGROUND TASK
          TABLES
            it_veiculo = it_veiculos.
      ENDIF.
*<-- 25.08.2023 16:16:45 - Migração S4 – ML – Fim
    ENDIF.

    "BS #169676 - inicio
    IF wa_lote_frete-sem_oc_opus <> 'X'.
      "BS #169676 - fim
      IF wa_zlest0185-vbeln IS NOT INITIAL.
        "Disparar Integração de Ordem de Carregamento no OPUS """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF lc_erro NE abap_true.
          DATA(lc_erro_oc) = abap_false.
          lc_sap_opus-numeroordemvenda      = wa_lote_frete-vbeln.
          lc_sap_opus-liberacarregamento    = 'C'.
**<<<------"188424 - NMS - INI------>>>
          SELECT SINGLE * FROM vbak INTO @DATA(el_vbak) WHERE vbeln EQ @wa_zlest0185-vbeln.

          IF el_vbak-auart EQ 'ZTER'.
            lc_sap_opus-safra                 = el_vbak-erdat(4).

          ELSE.
**<<<------"188424 - NMS - FIM------>>>
            lc_sap_opus-safra                 = e_vbap-charg.
            "US67959 WPP
            SELECT SINGLE *
              FROM tvarvc INTO @DATA(lwa_tvarvc)
             WHERE name = 'MAGGI_GR_FERTILIZANTES'
               AND low  = @e_vbap-matkl.

            IF sy-subrc EQ 0.
              CLEAR: lt_split_safra[].
              SPLIT e_vbap-charg AT '/' INTO TABLE lt_split_safra.

              IF lines( lt_split_safra ) EQ 2.
                READ TABLE lt_split_safra INTO DATA(lwa_safra) INDEX 2.
                IF strlen( lwa_safra ) EQ 4.
                  lc_sap_opus-safra = lwa_safra.
                ENDIF.
              ELSEIF e_vbap-charg IS INITIAL.

                DATA: lva_doc_simulador TYPE zsdt0041-doc_simulacao.

                "Busca Safra do Simulador
                CLEAR: lva_doc_simulador.

                SELECT SINGLE *
                  FROM zsdt0041 INTO @DATA(lwa_zsdt0041)
                 WHERE vbeln EQ @wa_lote_frete-vbeln.

                IF sy-subrc EQ 0.
                  lva_doc_simulador = lwa_zsdt0041-doc_simulacao.
                ELSE.
                  SELECT SINGLE *
                    FROM zsdt0090 INTO @DATA(lwa_zsdt0090)
                   WHERE vbeln EQ @wa_lote_frete-vbeln.

                  IF sy-subrc EQ 0 .
                    lva_doc_simulador = lwa_zsdt0090-doc_simulacao.
                  ENDIF.
                ENDIF.

                IF lva_doc_simulador IS NOT INITIAL.
                  SELECT SINGLE *
                    FROM zsdt0040 INTO @DATA(lwa_zsdt0040)
                   WHERE doc_simulacao EQ @lva_doc_simulador.

                  IF sy-subrc EQ 0 AND lwa_zsdt0040-safra IS NOT INITIAL.
                    lc_sap_opus-safra = lwa_zsdt0040-safra.
                  ENDIF.
                ENDIF.

*            SELECT SINGLE mtart
*              from mara INTO @DATA(lva_mtart)
*             WHERE matnr eq @e_vbap-matnr.
*
*            IF ( sy-subrc eq 0 ) AND ( e_vbap-matnr is NOT INITIAL ) AND ( lva_mtart EQ 'ZFER' ). "Regra para OV Fertilizantes Fabrica
*              LC_SAP_OPUS-SAFRA = SY-DATUM(4). "Definido em Reuniao 08.07.22 com Mauricio Schimit  Jaqueline Denardini e Arianne Queiroz
*            ENDIF.
              ENDIF.
            ENDIF.
            "US67959 WPP
**<<<------"188424 - NMS - INI------>>>
          ENDIF.
**<<<------"188424 - NMS - FIM------>>>
*-CS2021000253-26.04.2024-#59941-JT-inicio
*-CS2021000253-28.11.2024-#159662-JT-inicio
*=======================================================================
*------ fornecedor transporte
*=======================================================================
*       IF e_vbkd-inco1 = 'CIF' OR e_vbkd-inco1 = 'CPT'.  "*-CS2021000253-22.08.2024-#149374-JT
          IF e_vbkd-inco1 = 'CPT'.
            CLEAR w_lfa1.

*-ISSUE #172761-01.04.2025-JT-inicio
            CASE e_vbkd-inco1.
*           WHEN 'CIF'.
*             lc_ktokk = 'ZFIC'.
              WHEN 'CPT'.
                lr_ktokk = VALUE #( ( sign = 'E' option = 'EQ' low = 'ZFIC' high = abap_off )
                                    ( sign = 'E' option = 'EQ' low = 'ZMOT' high = abap_off ) ).
*             lc_ktokk = 'ZFFJ'.
            ENDCASE.
*-ISSUE #172761-01.04.2025-JT-fim

            SELECT SINGLE *
              INTO w_lfa1
              FROM lfa1
             WHERE stcd1  = i_inbound-data-transportador-documentos-cpf_cnpj
               AND ktokk IN lr_ktokk  "lc_ktokk '"*-ISSUE #172761-01.04.2025-JT
               AND loevm  = abap_off
               AND sperr  = abap_off
               AND sperm  = abap_off.

            lc_sap_opus-transportadoraterceiro-id_fornecedor = w_lfa1-lifnr.
            lc_sap_opus-transportadoraterceiro-descricao     = w_lfa1-name1.
            lc_sap_opus-transportadoraterceiro-cnpj          = i_inbound-data-transportador-documentos-cpf_cnpj.
            lc_agente_transp                                 = w_lfa1-lifnr.
          ENDIF.
*-CS2021000253-26.04.2024-#59941-JT-fim
*-CS2021000253-28.11.2024-#159662-JT-fim

          lc_sap_opus-empresa               = e_ordem_venda-vkorg.
          lc_sap_opus-filialresponsavel     = e_vbap-werks.
          lc_sap_opus-tipofrete             = e_vbkd-inco1.
          lc_sap_opus-fornecedorlocalcoleta = e_parceiro_ov_pc-lifnr.
          lc_sap_opus-dataemissao           = sy-datum.
          lc_sap_opus-dataemissao           = lc_sap_opus-dataemissao(4) && '-' && lc_sap_opus-dataemissao+4(2) && '-' && lc_sap_opus-dataemissao+6(2).

          IF zcl_string=>length( i_inbound-data-validade_ordem_carregamento ) GE 1.

            "Validar Qtd Caracteres
            IF zcl_string=>length( i_inbound-data-validade_ordem_carregamento ) LT 10.
              wa_tx-tx_validacao_oc = zcl_string=>concat( s1 = wa_tx-tx_validacao_oc s2 = 'Data de Validade de Ordem de Carregamento deve estar no padrão yyyy-mm-dd!' sp = zcl_string=>at_separador_padrao ).
              lc_erro_oc = abap_true.
            ENDIF.

            "Validar Padrão de Data
            FIND REGEX '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' IN i_inbound-data-validade_ordem_carregamento(10).
            IF sy-subrc IS NOT INITIAL.
              wa_tx-tx_validacao_oc = zcl_string=>concat( s1 = wa_tx-tx_validacao_oc s2 = 'Data de Validade de Ordem de Carregamento deve estar no padrão yyyy-mm-dd!' sp = zcl_string=>at_separador_padrao ).
              lc_erro_oc = abap_true.
            ELSE.
              lc_sap_opus-datavalidade = i_inbound-data-validade_ordem_carregamento(10).
            ENDIF.

          ENDIF.

          IF lc_erro_oc EQ abap_false.

            lc_sap_opus-numeromaterialsap = e_vbap-matnr.

            TRY .
                zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = e_parceiro_ov_sp-lifnr
                  )->ck_parceiro_local_negocio( IMPORTING e_j_1bbranch = DATA(e_j_1bbranch)
                  ).
                ")->CK_PARCEIRO_INTERCOMPANY( EXPORTING I_EMPRESA = E_ORDEM_VENDA-VKORG IMPORTING E_J_1BBRANCH = DATA(E_J_1BBRANCH)
                lc_sap_opus-filialemissora = e_j_1bbranch-branch.
              CATCH zcx_parceiros.
                lc_sap_opus-filialemissora = e_vbap-werks.
            ENDTRY.

*         MOVE-CORRESPONDING  e_outbound_forne TO lc_sap_opus-fornecedorproprietarioveiculos.         "*-IR 219949-29.01.2025-#165179-JT-inicio
            MOVE-CORRESPONDING lc_outbound_forne TO lc_sap_opus-fornecedorproprietarioveiculos.         "*-IR 219949-29.01.2025-#165179-JT-inicio

            READ TABLE it_veiculos WITH KEY tp_veiculo = '0' INTO DATA(wa_zlest0002).
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_zlest0002 TO lc_sap_opus-cavalo.
            ENDIF.

            DATA(it_veiculos_careta) = it_veiculos[].
            DELETE it_veiculos_careta WHERE tp_veiculo = '0'.

            READ TABLE it_veiculos_careta INDEX 1 INTO wa_zlest0002.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_zlest0002 TO lc_sap_opus-carreta1.
            ENDIF.

            READ TABLE it_veiculos_careta INDEX 2 INTO wa_zlest0002.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_zlest0002 TO lc_sap_opus-carreta2.
            ENDIF.

            READ TABLE it_veiculos_careta INDEX 3 INTO wa_zlest0002.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_zlest0002 TO lc_sap_opus-carreta3.
            ENDIF.

            MOVE-CORRESPONDING e_outbound_forne_mot TO lc_sap_opus-motorista.

            lc_sap_opus-clientelocaldescarga   = e_parceiro_ov_lr-kunnr.
            lc_sap_opus-fornecedorlocaldestino = e_parceiro_ov_z1-lifnr.
            IF zcl_string=>upper( i_inbound-data-peso_estimado-unidade_medida ) = 'KG'.
              lc_sap_opus-numeropesoalvo    = i_inbound-data-peso_estimado-quantidade.
            ELSEIF zcl_string=>upper( i_inbound-data-peso_estimado-unidade_medida ) = 'TO'.
              DATA(valor) = zcl_string=>to_float( i_inbound-data-peso_estimado-quantidade ) * 1000.
              MOVE valor TO lc_sap_opus-numeropesoalvo.
            ENDIF.
            lc_sap_opus-numeroquantidade     = lc_sap_opus-numeropesoalvo.
*-CS2021000253-15.08.2024-#148932-JT-BUG 148932-inicio
*         lc_sap_opus-numerofretecombinado = i_inbound-data-valor_frete-valor_frete_empresa.
            lc_sap_opus-numerofretecombinado = COND #( WHEN e_vbkd-inco1 = 'CPT' THEN lc_valor_frete_motorista
                                                                                 ELSE i_inbound-data-valor_frete-valor_frete_empresa ).
*-CS2021000253-15.08.2024-#148932-JT-BUG 148932-fim
            lc_sap_opus-storage              = i_inbound-metadata-storage-blob_path.
            lc_sap_opus-base_url             = i_inbound-metadata-storage-base_url.

            TRY .
                lc_sap_opus-numeropercentualadiantamentofr = zcl_calc_frete=>get_valor_adiantamento( EXPORTING i_bukrs  = e_ordem_venda-vkorg i_branch = e_vbap-werks i_lifnr  = e_parceiro_prop ).
              CATCH zcx_calc_frete.
                MOVE 0 TO lc_sap_opus-numeropercentualadiantamentofr.
            ENDTRY.

            TRY .

                DATA: lc_out_od      TYPE zde_resp_ord_carregamento,
                      lc_out_od_gene TYPE ty_messagem,
                      lc_out_od_erro TYPE zde_resp_ord_carregamento_erro.

                zcl_solicita_oc_opus=>zif_solicita_oc_opus~get_instance(
                  )->set_solicita_ordem_carrega(
                  EXPORTING
                    i_zlest0185                 = wa_zlest0185
                    i_zde_json_oc_sap_to_pus_ov = lc_sap_opus
                  IMPORTING
                    e_id_integracao             = wa_zlest0185-id_integracao_sol_oc
                    e_integracao                = DATA(e_integracao)
                  ).

                /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od ).

                wa_zlest0185-id_ordem = lc_out_od-idordem.

              CATCH zcx_integracao INTO ex_integracao.    "

                REPLACE ALL OCCURRENCES OF: '<b>'   IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '</b>'  IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '<br>'  IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '</br>' IN e_integracao-ds_data_retorno WITH space.

                lc_erro_oc = abap_true.

                "Se chegou e enviar
                IF e_integracao IS NOT INITIAL.

                  "Origem
                  "Erro
                  /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od_erro ).

                  "Se Tem Msg de Retorno
                  IF lc_out_od_erro-error IS NOT INITIAL.
                    DATA(lc_msg_erro_oc) = zcl_string=>concat( s1 = lc_out_od_erro-origem s2 = lc_out_od_erro-error sp = ':' ).
                    wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = lc_msg_erro_oc sp = zcl_string=>at_separador_padrao ).
                  ELSE.

                    "Message
                    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od_gene ).
                    IF lc_out_od_gene-message IS NOT INITIAL.
                      wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = lc_out_od_gene-message sp = zcl_string=>at_separador_padrao ).
                    ELSE.
                      wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = ex_integracao->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
                    ENDIF.

                  ENDIF.

                ELSE.
                  wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = ex_integracao->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
                ENDIF.

              CATCH zcx_error INTO ex_error.    "

                REPLACE ALL OCCURRENCES OF: '<b>'   IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '</b>'  IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '<br>'  IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '</br>' IN e_integracao-ds_data_retorno WITH space.

                lc_erro_oc = abap_true.

                "Se chegou e enviar
                IF e_integracao IS NOT INITIAL.

                  "Origem
                  "Erro
                  /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od_erro ).
                  "Se Tem Msg de Retorno
                  IF lc_out_od_erro-error IS NOT INITIAL.
                    lc_msg_erro_oc = zcl_string=>concat( s1 = lc_out_od_erro-origem s2 = lc_out_od_erro-error sp = ':' ).
                    wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = lc_msg_erro_oc sp = zcl_string=>at_separador_padrao ).
                  ELSE.

                    "Message
                    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od_gene ).
                    IF lc_out_od_gene-message IS NOT INITIAL.
                      wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = lc_out_od_gene-message sp = zcl_string=>at_separador_padrao ).
                    ELSE.
                      wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = ex_error->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
                    ENDIF.

                  ENDIF.
                ELSE.
                  wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = ex_error->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
                ENDIF.

            ENDTRY.

          ENDIF.

          IF lc_erro EQ abap_false.
            lc_erro = lc_erro_oc.
          ENDIF.

        ENDIF.

      ELSEIF wa_zlest0185-ebeln IS NOT INITIAL.
*---CS2020000704 - JTASSONI - 01.10.2020 - inicio
        "Pedido de Compra ZUB
        "Disparar Integração de Ordem de Carregamento no OPUS """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF lc_erro NE abap_true.

          lc_erro_oc = abap_false.
          lc_sap_opus-numeroordemvenda      = wa_lote_frete-ebeln.
          lc_sap_opus-liberacarregamento    = 'C'.


          "US67959 WPP
          DATA: lva_charg_ret TYPE eket-charg.

          CALL FUNCTION 'ZMM_GET_SAFRA_PEDIDO_FOR_OPUS'
            EXPORTING
              i_ekko  = e_pedido_compra
              i_ekpo  = e_ekpo
              i_eket  = e_eket
              i_ebeln = e_pedido_compra-ebeln
            IMPORTING
              e_safra = lva_charg_ret.

          lc_sap_opus-safra = lva_charg_ret.

*        DATA(_fertilizantes) = abap_false.
*
*        IF 'ZNB_ZFTE' CS e_pedido_compra-bsart.
*          LC_SAP_OPUS-SAFRA = e_pedido_compra-unsez.
*        ENDIF.
*
*        IF LC_SAP_OPUS-SAFRA IS INITIAL.
*          SELECT SINGLE *
*            FROM tvarvc INTO lwa_tvarvc
*           WHERE name = 'MAGGI_GR_FERTILIZANTES'
*             AND LOW  = e_ekpo-matkl.
*
*          if sy-subrc eq 0 AND e_eket-charg IS NOT INITIAL.
*            CLEAR: LT_SPLIT_SAFRA[].
*            SPLIT e_eket-charg AT '/' INTO TABLE LT_SPLIT_SAFRA.
*
*            IF LINES( LT_SPLIT_SAFRA ) EQ 2.
*              READ TABLE LT_SPLIT_SAFRA INTO LWA_SAFRA INDEX 2.
*              IF STRLEN( LWA_SAFRA ) EQ 4.
*                LC_SAP_OPUS-SAFRA = LWA_SAFRA.
*              ENDIF.
*            ENDIF.
*          endif.
*        ENDIF.
*
*        IF LC_SAP_OPUS-SAFRA IS INITIAL.
*          lc_sap_opus-safra = e_eket-charg.
*        ENDIF.
          "US67959 WPP

*-CS2021000253-26.04.2024-#59941-JT-inicio
*-CS2021000253-28.11.2024-#159662-JT-inicio
*=======================================================================
*------ fornecedor transporte
*=======================================================================
*       IF e_pedido_compra-inco1 = 'CIF' OR e_pedido_compra-inco1 = 'CPT'. "*-CS2021000253-22.08.2024-#149374-JT

          IF e_pedido_compra-inco1 IS INITIAL.    "#172365-26.03.2025-#172365-JT
            e_pedido_compra-inco1 = e_ekpo-inco1. "#172365-26.03.2025-#172365-JT
          ENDIF.                                  "#172365-26.03.2025-#172365-JT

          IF e_pedido_compra-inco1 = 'CPT'.
            CLEAR w_lfa1.

*-ISSUE #172761-01.04.2025-JT-inicio
            CASE e_pedido_compra-inco1.
*           WHEN 'CIF'.
*             lc_ktokk = 'ZFIC'.
              WHEN 'CPT'.
                lr_ktokk = VALUE #( ( sign = 'E' option = 'EQ' low = 'ZFIC' high = abap_off )
                                    ( sign = 'E' option = 'EQ' low = 'ZMOT' high = abap_off ) ).
*             lc_ktokk = 'ZFFJ'.
            ENDCASE.
*-ISSUE #172761-01.04.2025-JT-fim

            SELECT SINGLE *
              INTO w_lfa1
              FROM lfa1
             WHERE stcd1  = i_inbound-data-transportador-documentos-cpf_cnpj
               AND ktokk IN lr_ktokk "lc_ktokk "*-ISSUE #172761-01.04.2025-JT
               AND loevm  = abap_off
               AND sperr  = abap_off
               AND sperm  = abap_off.

            lc_sap_opus-transportadoraterceiro-id_fornecedor = w_lfa1-lifnr.
            lc_sap_opus-transportadoraterceiro-descricao     = w_lfa1-name1.
            lc_sap_opus-transportadoraterceiro-cnpj          = i_inbound-data-transportador-documentos-cpf_cnpj.
            lc_agente_transp                                 = w_lfa1-lifnr.
          ENDIF.
*-CS2021000253-26.04.2024-#59941-JT-fim
*-CS2021000253-28.11.2024-#159662-JT-fim

          lc_sap_opus-empresa               = e_pedido_compra-bukrs.

          IF e_pedido_compra-bsart EQ 'ZUB'.
            lc_sap_opus-filialresponsavel  = e_ekpv-vstel.
          ELSE.
            SELECT SINGLE *
              FROM j_1bbranch INTO @DATA(lwa_branch_pedido)
             WHERE branch = @e_ekpo-werks.

            IF sy-subrc EQ 0.
              lc_sap_opus-filialresponsavel  = e_ekpo-werks.
            ELSE.
              SELECT SINGLE *
                FROM zsdt_depara_cen INTO @DATA(lwa_zsdt_depara_cen)
               WHERE centrov_1 EQ @e_ekpo-werks.

              IF ( sy-subrc EQ 0 ) AND ( lwa_zsdt_depara_cen-centro_real IS NOT INITIAL ).
                lc_sap_opus-filialresponsavel = lwa_zsdt_depara_cen-centro_real.
              ELSE.
                lc_sap_opus-filialresponsavel = e_ekpo-werks.
              ENDIF.
            ENDIF.
          ENDIF.

          lc_sap_opus-tipofrete             = e_ekpo-inco1.
          lc_sap_opus-fornecedorlocalcoleta = e_parceiro_pc_pc-lifn2.
          lc_sap_opus-dataemissao           = sy-datum.
          lc_sap_opus-dataemissao           = lc_sap_opus-dataemissao(4) && '-' && lc_sap_opus-dataemissao+4(2) && '-' && lc_sap_opus-dataemissao+6(2).

          IF zcl_string=>length( i_inbound-data-validade_ordem_carregamento ) GE 1.

            "Validar Qtd Caracteres
            IF zcl_string=>length( i_inbound-data-validade_ordem_carregamento ) LT 10.
              wa_tx-tx_validacao_oc = zcl_string=>concat( s1 = wa_tx-tx_validacao_oc s2 = 'Data de Validade de Ordem de Carregamento deve estar no padrão yyyy-mm-dd!' sp = zcl_string=>at_separador_padrao ).
              lc_erro_oc = abap_true.
            ENDIF.

            "Validar Padrão de Data
            FIND REGEX '[0-9]{4}[-]{1}[0-9]{2}[-]{1}[0-9]{2}' IN i_inbound-data-validade_ordem_carregamento(10).
            IF sy-subrc IS NOT INITIAL.
              wa_tx-tx_validacao_oc = zcl_string=>concat( s1 = wa_tx-tx_validacao_oc s2 = 'Data de Validade de Ordem de Carregamento deve estar no padrão yyyy-mm-dd!' sp = zcl_string=>at_separador_padrao ).
              lc_erro_oc = abap_true.
            ELSE.
              lc_sap_opus-datavalidade = i_inbound-data-validade_ordem_carregamento(10).
            ENDIF.

          ENDIF.

          IF lc_erro_oc EQ abap_false.

            lc_sap_opus-numeromaterialsap = e_ekpo-matnr.

            TRY .
                zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = e_parceiro_pc_sp-lifn2
                  )->ck_parceiro_local_negocio( IMPORTING e_j_1bbranch = e_j_1bbranch
                  ).
                ")->CK_PARCEIRO_INTERCOMPANY( EXPORTING I_EMPRESA = E_ORDEM_VENDA-VKORG IMPORTING E_J_1BBRANCH = DATA(E_J_1BBRANCH)
                lc_sap_opus-filialemissora = e_j_1bbranch-branch.
              CATCH zcx_parceiros.
                lc_sap_opus-filialemissora = e_ekpo-werks.
            ENDTRY.

*         MOVE-CORRESPONDING  e_outbound_forne TO lc_sap_opus-fornecedorproprietarioveiculos.         "*-IR 219949-29.01.2025-#165179-JT-inicio
            MOVE-CORRESPONDING lc_outbound_forne TO lc_sap_opus-fornecedorproprietarioveiculos.         "*-IR 219949-29.01.2025-#165179-JT-inicio

            READ TABLE it_veiculos WITH KEY tp_veiculo = '0' INTO wa_zlest0002.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_zlest0002 TO lc_sap_opus-cavalo.
            ENDIF.

            it_veiculos_careta = it_veiculos[].
            DELETE it_veiculos_careta WHERE tp_veiculo = '0'.

            READ TABLE it_veiculos_careta INDEX 1 INTO wa_zlest0002.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_zlest0002 TO lc_sap_opus-carreta1.
            ENDIF.

            READ TABLE it_veiculos_careta INDEX 2 INTO wa_zlest0002.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_zlest0002 TO lc_sap_opus-carreta2.
            ENDIF.

            READ TABLE it_veiculos_careta INDEX 3 INTO wa_zlest0002.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING wa_zlest0002 TO lc_sap_opus-carreta3.
            ENDIF.

            MOVE-CORRESPONDING e_outbound_forne_mot TO lc_sap_opus-motorista.

            lc_sap_opus-clientelocaldescarga   = e_parceiro_pc_lr-lifn2.
            lc_sap_opus-fornecedorlocaldestino = e_parceiro_pc_z1-lifn2.
            IF zcl_string=>upper( i_inbound-data-peso_estimado-unidade_medida ) = 'KG'.
              lc_sap_opus-numeropesoalvo    = i_inbound-data-peso_estimado-quantidade.
            ELSEIF zcl_string=>upper( i_inbound-data-peso_estimado-unidade_medida ) = 'TO'.
              valor = zcl_string=>to_float( i_inbound-data-peso_estimado-quantidade ) * 1000.
              MOVE valor TO lc_sap_opus-numeropesoalvo.
            ENDIF.
            lc_sap_opus-numeroquantidade     = lc_sap_opus-numeropesoalvo.
*-CS2021000253-15.08.2024-#148932-JT-BUG 148932-inicio
*         lc_sap_opus-numerofretecombinado = i_inbound-data-valor_frete-valor_frete_empresa.
            lc_sap_opus-numerofretecombinado = COND #( WHEN e_pedido_compra-inco1 = 'CPT' THEN lc_valor_frete_motorista
                                                                                          ELSE i_inbound-data-valor_frete-valor_frete_empresa ).
*-CS2021000253-15.08.2024-#148932-JT-BUG 148932-fim
            lc_sap_opus-storage              = i_inbound-metadata-storage-blob_path.
            lc_sap_opus-base_url             = i_inbound-metadata-storage-base_url.

            TRY .
                lc_sap_opus-numeropercentualadiantamentofr = zcl_calc_frete=>get_valor_adiantamento(
                  EXPORTING
                    i_bukrs  = e_pedido_compra-bukrs
                    i_branch = e_ekpo-werks
                    i_lifnr  = e_parceiro_prop ).
              CATCH zcx_calc_frete.
                MOVE 0 TO lc_sap_opus-numeropercentualadiantamentofr.
            ENDTRY.

            TRY .

*             DATA: lc_out_od      TYPE zde_resp_ord_carregamento,
*                   lc_out_od_gene TYPE ty_messagem,
*                   lc_out_od_erro TYPE zde_resp_ord_carregamento_erro.

                zcl_solicita_oc_opus=>zif_solicita_oc_opus~get_instance(
                  )->set_solicita_ordem_carrega(
                  EXPORTING
                    i_zlest0185                 = wa_zlest0185
                    i_zde_json_oc_sap_to_pus_ov = lc_sap_opus
                  IMPORTING
                    e_id_integracao             = wa_zlest0185-id_integracao_sol_oc
                    e_integracao                = e_integracao
                  ).

                /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od ).

                wa_zlest0185-id_ordem = lc_out_od-idordem.

              CATCH zcx_integracao INTO ex_integracao.    "

                REPLACE ALL OCCURRENCES OF: '<b>'   IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '</b>'  IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '<br>'  IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '</br>' IN e_integracao-ds_data_retorno WITH space.

                lc_erro_oc = abap_true.

                "Se chegou e enviar
                IF e_integracao IS NOT INITIAL.

                  "Origem
                  "Erro
                  /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od_erro ).

                  "Se Tem Msg de Retorno
                  IF lc_out_od_erro-error IS NOT INITIAL.
                    lc_msg_erro_oc = zcl_string=>concat( s1 = lc_out_od_erro-origem s2 = lc_out_od_erro-error sp = ':' ).
                    wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = lc_msg_erro_oc sp = zcl_string=>at_separador_padrao ).
                  ELSE.

                    "Message
                    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od_gene ).
                    IF lc_out_od_gene-message IS NOT INITIAL.
                      wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = lc_out_od_gene-message sp = zcl_string=>at_separador_padrao ).
                    ELSE.
                      wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = ex_integracao->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
                    ENDIF.

                  ENDIF.

                ELSE.
                  wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = ex_integracao->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
                ENDIF.

              CATCH zcx_error INTO ex_error.    "

                REPLACE ALL OCCURRENCES OF: '<b>'   IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '</b>'  IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '<br>'  IN e_integracao-ds_data_retorno WITH space.
                REPLACE ALL OCCURRENCES OF  '</br>' IN e_integracao-ds_data_retorno WITH space.

                lc_erro_oc = abap_true.

                "Se chegou e enviar
                IF e_integracao IS NOT INITIAL.

                  "Origem
                  "Erro
                  /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od_erro ).
                  "Se Tem Msg de Retorno
                  IF lc_out_od_erro-error IS NOT INITIAL.
                    lc_msg_erro_oc = zcl_string=>concat( s1 = lc_out_od_erro-origem s2 = lc_out_od_erro-error sp = ':' ).
                    wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = lc_msg_erro_oc sp = zcl_string=>at_separador_padrao ).
                  ELSE.

                    "Message
                    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od_gene ).
                    IF lc_out_od_gene-message IS NOT INITIAL.
                      wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = lc_out_od_gene-message sp = zcl_string=>at_separador_padrao ).
                    ELSE.
                      wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = ex_error->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
                    ENDIF.

                  ENDIF.
                ELSE.
                  wa_tx-tx_validacao_oc = zcl_string=>concat( EXPORTING s1 = wa_tx-tx_validacao_oc s2 = ex_error->zif_error~get_msg_erro( ) sp = zcl_string=>at_separador_padrao ).
                ENDIF.

            ENDTRY.

          ENDIF.

          IF lc_erro EQ abap_false.
            lc_erro = lc_erro_oc.
          ENDIF.

        ENDIF.
*---CS2020000704 - JTASSONI - 01.10.2020 - fim

      ELSE.
        lc_erro = abap_true.
        wa_tx-tx_processamento = 'Lote Embarcador sem Ordem de Venda ou Pedido de Compra!'.

      ENDIF.
      "BS #169676 - inicio
    ENDIF.

    "WPP #169676 - inicio
    IF wa_lote_frete-nro_sol IS NOT INITIAL OR
       wa_lote_frete-nro_cg_sai_in IS NOT INITIAL. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP

      IF i_inbound-data-transportador-documentos-cpf_cnpj IS NOT INITIAL.

        SELECT SINGLE *
          FROM lfa1 INTO @DATA(lwa_lfa1_transp)
         WHERE stcd1  = @i_inbound-data-transportador-documentos-cpf_cnpj
           AND loevm  = @abap_off
           AND sperr  = @abap_off
           AND sperm  = @abap_off.

        IF sy-subrc EQ 0 AND lwa_lfa1_transp-ktokk NE 'ZFIC'.
          lc_agente_transp = lwa_lfa1_transp-lifnr.
        ENDIF.
      ENDIF.

    ENDIF.
    "WPP #169676 - fim


    "WPP #169676 - Inicio
    IF wa_lote_frete-nro_sol IS NOT INITIAL AND lc_erro EQ abap_false.

      me->set_criar_carga_entrada(
        EXPORTING
          i_zlest0185       =  wa_zlest0185       " Tabela de Viagens Criadas
          i_lote_frete      =  wa_lote_frete      " Tabela de Lotes de Demanda de Frete - CARGUERO
          i_parceiro_terc   =  lwa_lfa1_transp-lifnr   " Nº Parceiro Terceiro
          i_motorista_lifnr =  CONV #( e_parceiro_moto )          " Nº Motorista
          i_inbound         =  i_inbound          " Estrutura de Requisição de Ordem de Carregamento
          it_veiculos       =  local              " Estrutura para InBound de Ordem de Carregamento
       IMPORTING
         e_msg_error        = DATA(lva_msg_erro_carga_in)
         e_nro_cg           = DATA(lva_nro_carga_in)
       RECEIVING
         r_sucesso          = DATA(lva_sucesso_carga_in)
      ).

      IF lva_sucesso_carga_in EQ abap_false OR lva_nro_carga_in IS INITIAL.
        lc_erro = abap_true.

        IF lva_msg_erro_carga_in IS NOT INITIAL.
          wa_tx-tx_validacao_viagem = lva_msg_erro_carga_in.
        ELSE.
          wa_tx-tx_validacao_viagem = 'Não foi possível criar a Carga de entrada!'.
        ENDIF.

      ELSE.

        zcl_carga_entrada_insumos=>gerar_autorizacao_embarque(
          EXPORTING
            i_nro_cg                     = lva_nro_carga_in
            i_background                 = abap_true
            i_envia_autorizacao_carguero = abap_true
          IMPORTING
            e_msg_error                  = DATA(lva_msg_error_aut)
          RECEIVING
            r_sucesso                    = DATA(lva_sucesso_aut)
        ).

        IF lva_sucesso_aut EQ abap_false. "Se não conseguiu enviar a autorização de embarque para o storage do carguero, cancela a carga criada
          lc_erro = abap_true.
          wa_tx-tx_validacao_viagem = |Não foi possível gerar a autorização de embarque e enviar ao Carguero/Strada! Msg: { lva_msg_error_aut }|.
          UPDATE zmmt0201 SET cancel      = abap_true
                              user_cancel = sy-uname
                              date_cancel = sy-datum
                              time_cancel = sy-uzeit
           WHERE nro_cg = lva_nro_carga_in.

          COMMIT WORK.
        ENDIF.
      ENDIF.

    ENDIF.
    "WPP #169676 - fim

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
    IF wa_lote_frete-nro_cg_sai_in IS NOT INITIAL AND lc_erro EQ abap_false.

      DATA: lwa_dados_logistico TYPE zsds382.

      CLEAR: lwa_dados_logistico.

      lwa_dados_logistico-nro_cg              = wa_lote_frete-nro_cg_sai_in.
      lwa_dados_logistico-cod_transportadora  = lwa_lfa1_transp-lifnr.
      CASE lwa_lfa1_transp-ktokk.
        WHEN 'ZFIC'.
          lwa_dados_logistico-inco1 = 'CIF'.
        WHEN OTHERS.
          lwa_dados_logistico-inco1 = 'CPT'.
      ENDCASE.
      lwa_dados_logistico-motorista           = e_parceiro_moto.
      lwa_dados_logistico-preco_frete         = i_inbound-data-valor_frete-valor_frete_empresa.
      lwa_dados_logistico-viagem_id           = wa_zlest0185-viagem_id.
      lwa_dados_logistico-ds_conjunto_transp  = i_inbound-data-conjunto_transportador-nome.

      CASE i_inbound-data-valor_frete-modalidade_pagamento_frete.
        WHEN '1'. "Tonelada
          lwa_dados_logistico-frete_por_t = abap_true.
        WHEN '2'. "Viagem
          lwa_dados_logistico-frete_por_v = abap_true.
      ENDCASE.

      LOOP AT local-veiculos INTO DATA(ls_veiculo).
        CASE ls_veiculo-ds_tp_veiculo .
          WHEN '0'.
            lwa_dados_logistico-placa_cav = ls_veiculo-ds_placa.
          WHEN '1'.
            IF lwa_dados_logistico-placa_car1 IS INITIAL.
              lwa_dados_logistico-placa_car1 = ls_veiculo-ds_placa.
            ELSEIF lwa_dados_logistico-placa_car2 IS INITIAL.
              lwa_dados_logistico-placa_car2 = ls_veiculo-ds_placa.
            ELSE.
              lwa_dados_logistico-placa_car3 = ls_veiculo-ds_placa.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      DATA(lva_msg_error_atualizacao) = zcl_carga_saida_insumos=>atualizar_dados_logistico( i_header   = lwa_dados_logistico ).

      IF lva_msg_error_atualizacao IS NOT INITIAL.
        lc_erro = abap_true.
        wa_tx-tx_validacao_viagem = |Não foi possível atualizar os dados logisticos da Carga! Msg: { lva_msg_error_atualizacao }|.
      ELSE.

        zcl_carga_saida_insumos=>gerar_autorizacao_embarque(
          EXPORTING
            i_nro_cg                     = wa_lote_frete-nro_cg_sai_in
            i_viagem_id                  = wa_zlest0185-viagem_id
            i_background                 = abap_true
            i_envia_autorizacao_carguero = abap_true
          IMPORTING
            e_msg_error                  = lva_msg_error_aut
          RECEIVING
            r_sucesso                    = lva_sucesso_aut
        ).

        IF lva_sucesso_aut EQ abap_false. "Se não conseguiu gerar a autorização de embarque e enviar para o storage do carguero e CD, cancela remove os dados logisticos da Carga
          lc_erro = abap_true.

          "Apagar Dados Logistico Carga
          CLEAR: lwa_dados_logistico.
          lwa_dados_logistico-nro_cg = wa_lote_frete-nro_cg_sai_in.
          zcl_carga_saida_insumos=>atualizar_dados_logistico( i_header = lwa_dados_logistico i_remove_data_aut_emb = abap_true ).

          wa_tx-tx_validacao_viagem = |Não foi possível gerar a autorização de embarque! Msg: { lva_msg_error_aut }|.
        ENDIF.
      ENDIF.

    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

    "Marca Criar Viagem como Processada """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    wa_zlest0185-ck_processada  = abap_true.
    wa_zlest0185-agente_frete   = lc_agente_transp. "*-CS2021000253-28.11.2024-#159662-JT-inicio  "*-CS2021000253-22.08.2024-#149374-JT-COMENTADO / descomentado 13.11.2024    "*-CS2021000253-26.04.2024-#59941-JT

    "BS #169676 - inicio
*    IF wa_lote_frete-viagem_aprovacao_rejeicao_manu EQ abap_true AND lc_erro EQ abap_false.
*
*      wa_zlest0185-ck_processada_com_sucesso = abap_true.
*
*    ELSE.

    CASE lc_erro.
      WHEN abap_true.
        wa_zlest0185-ck_autorizada    = abap_false.
        wa_tx-tx_processamento = 'Processada com Erro!'.
      WHEN abap_false.
        wa_zlest0185-ck_autorizada    = abap_true.
        wa_tx-tx_processamento = 'Processada com Sucesso!'.
    ENDCASE.

    "    ENDIF.
    "BS #169676 - fim


*    IF WA_TX-TX_VALIDACAO_VIAGEM IS NOT INITIAL.
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = 'Mensagem Viagem' SP = '-' ).
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = WA_TX-TX_VALIDACAO_VIAGEM SP = '/' ).
*    ENDIF.
*
*    IF WA_TX-TX_VALIDACAO_PROP IS NOT INITIAL.
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = 'Mensagem Proprietário' SP = '-' ).
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = WA_TX-TX_VALIDACAO_PROP SP = '/' ).
*    ENDIF.
*
*    IF WA_TX-TX_VALIDACAO_VEICULOS IS NOT INITIAL.
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = 'Mensagem Veículos'         SP = '-' ).
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = WA_TX-TX_VALIDACAO_VEICULOS SP = '/' ).
*    ENDIF.
*
*    IF WA_TX-TX_VALIDACAO_MOT IS NOT INITIAL.
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = 'Mensagem Motorista'   SP = '-' ).
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = WA_TX-TX_VALIDACAO_MOT SP = '/' ).
*    ENDIF.
*
*    IF LC_TX_ORDEM_VENDA IS NOT INITIAL.
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = 'Mensagem Preço de Frete' SP = '-' ).
*      WA_TX-TX_PROCESSAMENTO = ZCL_STRING=>CONCAT( EXPORTING S1 = WA_TX-TX_PROCESSAMENTO S2 = LC_TX_ORDEM_VENDA         SP = '/' ).
*    ENDIF.

    wa_tx-viagem_id = wa_zlest0185-viagem_id.

    MODIFY zlest0185 FROM wa_zlest0185.
    MODIFY zlest0185tx FROM wa_tx.
    COMMIT WORK.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    "Metodo para processoamento de InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_ORD_CARREGA~GET_INSTANCE.

    IF ZIF_INTEGRACAO_ORD_CARREGA~AT_IF_INTEGRACAO_ORD_CARREGA IS NOT BOUND.
      CREATE OBJECT ZIF_INTEGRACAO_ORD_CARREGA~AT_IF_INTEGRACAO_ORD_CARREGA TYPE ZCL_INTEGRACAO_ORD_CARREGA.
    ENDIF.
    R_IF_INTEGRACAO_ORD_CARREGA = ZIF_INTEGRACAO_ORD_CARREGA~AT_IF_INTEGRACAO_ORD_CARREGA.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_ORD_CARREGA~SET_DS_DATA.

    DATA: I_INBOUND TYPE ZDE_CARGUERO_REQUISICAO.

    "Incluir Texto JSON para integração
    R_IF_INTEGRACAO_ORD_CARREGA = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP = I_INFO.

    "Validar Json
    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_INFO-DS_BODY CHANGING DATA = I_INBOUND ).
    ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA = I_INBOUND-DATA-VIAGEM_ID.
    CHECK I_INBOUND-DATA-VIAGEM_ID IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_INTEGRACAO
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
                          MSGNO = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO )
        MSGID  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGID
        MSGNO  = ZCX_INTEGRACAO=>ZCX_ERRO_BODY_RECEBIDO-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_ORD_CARREGA~SET_SEND_MSG.

    DATA: LC_INTEGRACAO TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_ORD_CARREGA = ME.

    "Verificar a Função de Cada requisição
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = '/notificacoes/viagem-criada'.

    SELECT SINGLE * INTO @DATA(WA_INTEGRACAO)
      FROM ZINTEGRACAO
     WHERE TP_REFERENCIA EQ @ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-TP_REFERENCIA
       AND ID_REFERENCIA EQ @ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA-ID_REFERENCIA.

    CHECK SY-SUBRC IS NOT INITIAL.

    CREATE OBJECT LC_INTEGRACAO.

    LC_INTEGRACAO->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = ME->ZIF_INTEGRACAO_ORD_CARREGA~AT_ID_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRACAO.

  ENDMETHOD.


  METHOD set_criar_carga_entrada.

    DATA: ls_carga     TYPE zmmt0201,
          ls_frete     TYPE zsdt0391,
          lt_cotacao   TYPE zsdtt0346,
          ls_cotacao   TYPE zsdt0346,
          lv_seq       TYPE zsdt0346-seq_cotacao,
          lv_lzone_pc  TYPE lfa1-lzone,
          lv_lzone_le  TYPE lfa1-lzone,
          lv_route     TYPE trolz-route,
          lv_nro_carga TYPE zsdt0346-nro_carga.

    DATA: lt_lfa1        TYPE TABLE OF lfa1,
          lt_itens_carga TYPE zmmtt0202.

    CLEAR: r_sucesso, e_msg_error, e_nro_cg.

    " Preenchimento da ZMMT0201
    CLEAR: ls_carga.

    ls_carga-viagem_id            = i_zlest0185-viagem_id.
    ls_carga-nro_sol              = i_lote_frete-nro_sol.
    ls_carga-bukrs                = i_zlest0185-bukrs.
    ls_carga-cod_transportadora   = i_parceiro_terc.
    ls_carga-cod_motorista        = i_motorista_lifnr.
    ls_carga-nome_motorista       = i_inbound-data-motorista-nome.
    ls_carga-inco1                = i_lote_frete-tp_frete.
    ls_carga-ponto_coleta         = i_lote_frete-id_parceiro_cole.
    ls_carga-local_entrega        = i_lote_frete-id_parceiro_entr.
    ls_carga-dt_prevista_embarque = sy-datum.

    " Quantidade Total
    IF zcl_string=>upper( i_inbound-data-peso_estimado-unidade_medida ) = 'KG'.
      ls_carga-qtd_total_kg = i_inbound-data-peso_estimado-quantidade.
    ELSEIF zcl_string=>upper( i_inbound-data-peso_estimado-unidade_medida ) = 'TO'.
      ls_carga-qtd_total_kg = zcl_string=>to_float( i_inbound-data-peso_estimado-quantidade ) * 1000.
    ENDIF.

    " Buscar troca de nota na ZMMT0196
    SELECT SINGLE troca_nota INTO ls_carga-transf_no_fornecedor
      FROM zmmt0196
     WHERE nro_sol = i_lote_frete-nro_sol.

    " Placas
    LOOP AT it_veiculos-veiculos INTO DATA(ls_veiculo).
      CASE ls_veiculo-ds_tp_veiculo .
        WHEN '0'.
          ls_carga-placa_cav = ls_veiculo-ds_placa.
        WHEN '1'.
          IF ls_carga-placa_car1 IS INITIAL.
            ls_carga-placa_car1 = ls_veiculo-ds_placa.
          ELSEIF ls_carga-placa_car2 IS INITIAL.
            ls_carga-placa_car2 = ls_veiculo-ds_placa.
          ELSE.
            ls_carga-placa_car3 = ls_veiculo-ds_placa.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    ls_carga-user_create = sy-uname.
    ls_carga-date_create = sy-datum.
    ls_carga-time_create = sy-uzeit.

    " Preenchimento da ZSDT0391
    CLEAR: ls_frete.
    ls_frete-modalidade_pag_frete  = i_inbound-data-valor_frete-modalidade_pagamento_frete(1).
    ls_frete-preco_total_frete     = i_inbound-data-valor_frete-valor_frete_empresa.
    ls_frete-moeda                 = 'BRL'.
    ls_frete-dt_contratacao_frete  = sy-datum.
    ls_frete-dt_cotacao_frete      = sy-datum.
    ls_frete-user_create           = sy-uname.
    ls_frete-date_create           = sy-datum.
    ls_frete-time_create           = sy-uzeit.


    CLEAR: ls_cotacao.
    ls_cotacao-id_operacao      = '00000001'.
    ls_cotacao-ponto_coleta     = i_lote_frete-id_parceiro_cole.
    ls_cotacao-local_entrega    = i_lote_frete-id_parceiro_entr.
    ls_cotacao-unidade_condicao = i_zlest0185-waers.
    ls_cotacao-valor            = i_inbound-data-valor_frete-valor_frete_empresa.

    SELECT SINGLE tipo_transporte
        INTO @DATA(lv_tp_trans)
        FROM zsdt0345 WHERE id = @ls_cotacao-id_operacao.

    IF sy-subrc = 0.
      ls_cotacao-tipo_transporte = lv_tp_trans.
    ENDIF.

    " Buscar ZONA do parceiro coletador
    SELECT SINGLE lzone
      INTO lv_lzone_pc
      FROM lfa1
     WHERE lifnr = i_lote_frete-id_parceiro_cole.

    " Buscar ZONA do parceiro entregador
    SELECT SINGLE lzone
      INTO lv_lzone_le
      FROM lfa1
     WHERE lifnr = i_lote_frete-id_parceiro_entr.

    " Itinerário
    IF lv_lzone_pc IS NOT INITIAL AND lv_lzone_le IS NOT INITIAL.
      SELECT SINGLE route
        INTO lv_route
        FROM trolz
       WHERE azone = lv_lzone_pc
         AND lzone = lv_lzone_le.
    ENDIF.

    ls_cotacao-itinerario = lv_route.

    " Unidade de medida
    CASE ls_frete-modalidade_pag_frete.
      WHEN '1'.
        ls_cotacao-unidade_medida = 'TO'.
      WHEN '2'.
        ls_cotacao-unidade_medida = ''.
    ENDCASE.

    ls_cotacao-usuario = sy-uname.
    ls_cotacao-data    = sy-datum.
    ls_cotacao-hora    = sy-uzeit.
    APPEND ls_cotacao TO lt_cotacao.

    zcl_carga_entrada_insumos=>gravar_carga_core(
      EXPORTING
        i_determina_itens_auto = abap_true
      IMPORTING
        e_nro_carga   = lv_nro_carga
      CHANGING
        i_zmmt0201    = ls_carga
        i_zsdt0391    = ls_frete
        i_zsdt0346    = lt_cotacao
      RECEIVING
        r_msg_error   = e_msg_error
    ).

    CHECK e_msg_error IS INITIAL.

    IF lv_nro_carga IS INITIAL.
      e_msg_error = 'Não foi possível determinar a numeração para a Carga de entrada!'.
      RETURN.
    ENDIF.

    COMMIT WORK.

    e_nro_cg = lv_nro_carga.

    r_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
