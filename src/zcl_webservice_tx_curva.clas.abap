class ZCL_WEBSERVICE_TX_CURVA definition
  public
  inheriting from ZCL_WEBSERVICE
  create public .

*"* public components of class ZCL_WEBSERVICE_TX_CURVA
*"* do not include other source files here!!!
public section.

  methods BUSCAR_TAXA
    importing
      !I_DATA type DATUM
      !I_DATA_LIB type DATUM optional
      !I_TIPO type CHAR01 default 'C'
    returning
      value(E_COTACAO) type KURRF
    raising
      ZCX_WEBSERVICE .
  methods EXECUTAR
    importing
      !I_NUMERO type ZSDED013
      !I_TIPO type CHAR03 optional
      !I_FIXACAO type POSNR optional
      !I_UCOMM type SYUCOMM optional
      !I_STATUS type C optional
      !I_TCODE type SYTCODE optional
      !I_VBELN type VBELN optional
      !I_AUART type AUART optional
      !I_VBAP type VBAP_T optional .
  class-methods HEDGE_INSUMOS
    importing
      !I_NUMERO type ZSDED003 optional
      !I_ACAO type SY-UCOMM optional
      !I_TIPO type CHAR3 optional
      !I_ITENS type STANDARD TABLE optional
      !I_0090 type ZSDT0090 optional
      !I_VBELN type VBELN optional
      !I_DIR type BEZEI30 optional
      !I_SEQ type NUMC4 optional
      !I_TAXA_BOLETA type UKURSP optional
      !I_0090_MANUAL type FLAG optional .
  class-methods HEDGE_AQUAVIARIO
    importing
      !_VBRK type VBRK
      !_VBRP type VBRPVB optional
      !_CODE type SYTCODE
      !_AUART type AUART optional .
  PROTECTED SECTION.
*"* protected components of class ZCL_WEBSERVICE_TX_CURVA
*"* do not include other source files here!!!
private section.

*"* private components of class ZCL_WEBSERVICE_TX_CURVA
*"* do not include other source files here!!!
  methods MONTA_XML_TAXA
    importing
      !I_DATA type SYDATUM
      !I_DATA_LIB type DATUM optional
      !I_TIPO type CHAR01
    returning
      value(E_XML) type STRING .
  methods LER_XML_TAXA
    importing
      !I_XML type STRING
    returning
      value(E_COTACAO) type KURRF
    raising
      ZCX_WEBSERVICE .
ENDCLASS.



CLASS ZCL_WEBSERVICE_TX_CURVA IMPLEMENTATION.


  METHOD buscar_taxa.

    DATA: var_msg  TYPE string, "Variavel para mostrar a Mensagem texto da exception.
          var_http TYPE REF TO if_http_client. "Interface HTTP Client

    DATA: cx_exception TYPE REF TO zcx_webservice. "Referencia para a Classe de Exception.

    DATA: gw_xml      TYPE string, "String para guardar informações do XML
          gw_xml_taxa TYPE string. "String para guardar informações do XML da Rota.

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "TX = Taxa Curva.
        me->set_servico( EXPORTING i_servico = 'TX' ).

      CATCH zcx_webservice INTO cx_exception .
        var_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH var_msg.
    ENDTRY.

    "Atribui o Tipo de Serviço
    "A = atualização.
    me->set_tipo( EXPORTING i_tipo = 'C').

    TRY .

        "Atribui as Informações do HTTP Client para consultar o WebService.
        var_http = me->url( ).

      CATCH zcx_webservice INTO cx_exception .
        var_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH var_msg.
    ENDTRY.

    "Abrir a conexão com o serviço.
    me->zif_webservice~abrir_conexao( var_http ).
    gw_xml = monta_xml_taxa( i_data     = i_data
                             i_data_lib = i_data_lib
                             i_tipo = i_tipo ).

    "Envia para Consultar a rota com as informações preenchidas acima.
    "O retorno é de um arquivo XML com todas as rotas que precisam ser atualizadas.
* Modificação - RIM-SKM - IR127835 - Inicio
*    GW_XML_TAXA = ME->ZIF_WEBSERVICE~CONSULTAR( I_HTTP = VAR_HTTP
*                                                I_XML  = GW_XML
*                                                ).
    me->zif_webservice~consultar( EXPORTING
                                    i_http = var_http
                                    i_xml  = gw_xml
                                  RECEIVING
                                    e_resultado = gw_xml_taxa
                                  EXCEPTIONS
                                    http_communication_failure = 1
                                    http_invalid_state = 2
                                    http_processing_failed = 3
                                    http_invalid_timeout = 4
                                ).
    IF sy-subrc NE 0.
      DATA: w_zsdt0094_log TYPE zsdt0094_log.
      w_zsdt0094_log-data_registro = sy-datum.
      w_zsdt0094_log-hora_registro = sy-uzeit.
      w_zsdt0094_log-programa = sy-cprog.
      w_zsdt0094_log-ernam = sy-uname.
      w_zsdt0094_log-data = i_data.
      w_zsdt0094_log-data_lib = i_data_lib.
      w_zsdt0094_log-tipo = i_tipo.
      w_zsdt0094_log-erro = sy-subrc.
      MODIFY zsdt0094_log FROM w_zsdt0094_log.
    ENDIF.
* Modificação - RIM-SKM - IR127835 - Fim

    e_cotacao = ler_xml_taxa( gw_xml_taxa ).


  ENDMETHOD.


  method executar.

    data: gobj_taxa_curva_db type ref to zcl_taxa_curva_db.
    data: lw_zsdt0094 type zsdt0094.
    data: lw_setleaf  type setleaf.

    create object gobj_taxa_curva_db.

    "Caso encontre qualquer registro referente ao número de solicitação de venda que tenha o status inicializado como 'X'
    "não deverá ser feito nenhum tipo de registro na ZSDT0094.
    select single * from zsdt0094 into lw_zsdt0094 where nro_sol_ov eq i_numero
                                                     and edicao     eq 'X'.
    if ( sy-subrc ne 0 ).

      "Caso a solicitação seja encontrada, não pode fazer o lançamento  novamente, porque se trata de uma SOV antiga.
      select single * from setleaf into lw_setleaf where setname = 'MAGGI_SOV_ANTIGA'
                                                     and valfrom = i_numero.
      if ( sy-subrc ne 0 ).
        case i_tipo.
          when: 'LIB'. "Liberação de OV.
            gobj_taxa_curva_db->liberar_ov( i_numero  = i_numero
                                            i_tcode   = i_tcode
                                            i_fixacao = i_fixacao ).
          when: 'FRA'."Frame
            gobj_taxa_curva_db->frame( i_numero = i_numero
                                       i_ucomm  = i_ucomm
                                       i_tcode  = i_tcode
                                       i_vbeln  = i_vbeln
                                       i_auart  = i_auart
                                       ).
          when: 'FRE'. "Frete.
            gobj_taxa_curva_db->frete( i_numero  = i_numero
                                       i_fixacao = i_fixacao
                                       i_status  = i_status
                                       i_tcode   = i_tcode
                                       i_vbeln   = i_vbeln
                                       i_auart   = i_auart
                                       ).

          when: 'EDI' or 'DEL' . "Edição / Delete.
            gobj_taxa_curva_db->edicao( i_numero  = i_numero
                                        i_fixacao = i_fixacao
                                        i_ucomm   = i_ucomm
                                        i_vbeln   = i_vbeln
                                        i_tcode   = i_tcode
                                        ).

          when: 'EDF'."Edição de Frame
            gobj_taxa_curva_db->frame_edicao( i_numero  = i_numero
                                              i_fixacao = i_fixacao
                                              i_vbeln   = i_vbeln
                                              i_tcode   = i_tcode
                                              i_auart   = i_auart
                                              i_tipo    = i_tipo
                                              i_status  = i_status ).

          when: 'ENC'.  "Encerramento de Venda Simples

            "Repetir essa merda porque o usuário não sabe o que quer da vida.
            gobj_taxa_curva_db->encerramento( i_numero  = i_numero
                                              i_tcode   = i_tcode
                                              i_fixacao = i_fixacao
                                              i_auart   = i_auart
                                              i_status  = i_status
                                              i_vbeln   = i_vbeln
                                              i_vbap    = i_vbap ). "ajuste Bug Solto 149379 / aoenning& / 22-08-2024 -----&*.


          when: 'LOG'.  "Atualiza a Aba logista, das vendas que não dispara o Hedge

            gobj_taxa_curva_db->calcula_frete( i_numero  = i_numero
                                               i_fixacao = i_fixacao
                                               i_ucomm   = i_ucomm
                                               i_vbeln   = i_vbeln
                                               i_tcode   = i_tcode ).


        endcase.
      endif.
    endif.

  endmethod.


  METHOD hedge_aquaviario.

    DATA(obj_tx_curva)    = NEW zcl_taxa_curva( ).
    DATA(obj_tx_curva_db) = NEW zcl_taxa_curva_db( ).

    DATA(r_aqv) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_AQV' ).
    DATA(r_spt) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_SPT' ).
    DATA(r_tbo) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_TB' ).

    DATA(r_geral) = r_aqv.
    APPEND LINES OF r_spt TO r_geral.
    APPEND LINES OF r_tbo TO r_geral.

    DATA(r_burks) = obj_tx_curva->get_auart( 'RV60AFZZ_HEDGE_BUKRS' ).

    DATA: v_aquav_ini TYPE bsak-augdt.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_zfi0064_ini_aquav)
     WHERE setname = 'ZFI0064_INI_AQUAV'.

    IF ( sy-subrc EQ 0 ) AND ( _wl_zfi0064_ini_aquav-valfrom IS NOT INITIAL ).
      v_aquav_ini = _wl_zfi0064_ini_aquav-valfrom.

      "// Verifica se a data que está no Set é menor ou igual a data de hoje, para fazer fazer o disparo.
      IF v_aquav_ini LE sy-datum.

        CHECK r_geral IS NOT INITIAL.
        CHECK r_burks IS NOT INITIAL.

        CHECK _vbrk-fkart IS NOT INITIAL.
        CHECK _vbrk-vkorg IS NOT INITIAL.

        CHECK _vbrk-vkorg IN r_burks.
        CHECK _vbrk-waerk EQ 'BRL'.

*   "// Condição para contemplar a ZLES0077
        DATA(code) = _code.
        code = COND #( WHEN _vbrk-fkart EQ 'S1' THEN 'VF11' ELSE code ).

        CASE code.
          WHEN 'VF01' OR 'ZLES0077' OR 'ZNFW0005' OR 'ZSDT0158' OR 'ZSDT0200' OR 'ZSDT0201'.

            IF _auart IS NOT INITIAL.
              CHECK _auart IN r_geral.
            ELSE.
              CHECK _vbrk-fkart IN r_geral.
            ENDIF.

            obj_tx_curva_db->frete_aqv(
                                        _vbrk  = _vbrk
                                        _vbrp  = _vbrp
                                        _auart = _auart
                                      ).

          WHEN 'VF11'.
            obj_tx_curva_db->estorno_aqv( _vbrk-sfakn ).
        ENDCASE.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD hedge_insumos.

    DATA: obj_taxa   TYPE REF TO zcl_taxa_curva_db.
    CREATE OBJECT obj_taxa.

    CASE i_tipo.

      WHEN 'VDI'. "Venda mercado Interno
**************************************************************************************************
*        Dispara o HEDGE VENDA na Transação ZSDT0044 quando:
*        Aprova uma Solicitação
*        Inclui um desconto Absoluto.
*        e na Transação ZSDT0087 quando:
*        Altera Quantidade da Ordem
*        Traca de Materiais
*        Encerramento
*        Trava Cambio
*        Redistribuição de Quantidade
**************************************************************************************************
        obj_taxa->venda_in( i_numero = i_numero
                            i_tipo   = i_tipo
                            i_acao   = i_acao
                            i_0090   = i_0090
                            t_itens  = i_itens
                            i_dir    = i_dir
                            i_taxa_boleta = i_taxa_boleta ).

      WHEN 'FRI'. "Frete Mercado interno
**************************************************************************************************
*        Dispara o HEDGE FRETE na Transação ZSDT0044 quando:
*        Aprova uma Solicitação
*        Inclui um desconto Absoluto.
*        e na Transação ZSDT0087 quando:
*        Altera Quantidade da Ordem
*        Traca de Materiais
*        Encerramento
*        Redistribuição de quantiodade
**************************************************************************************************
        obj_taxa->frete_in( i_numero = i_numero
                            i_tipo   = i_tipo
                            i_acao   = i_acao
                            i_0090   = i_0090
                            t_itens  = i_itens
                            i_dir    = i_dir
                            i_taxa_boleta = i_taxa_boleta ).

      WHEN 'EST'. "Estorno dos lançamentos disparados
**************************************************************************************************
*        Dispara o HEDGE quando é REPROVADO uma Simulação na ZSDT0044
*        ou quando Estorna uma OV na ZSDT0087
*        I_NUMERO -> Documento de Simulação
*        I_TIPO   -> Parametro de Identificação
*        I_VBELN  -> Documento da Ordem
*        I-DIR    -> Direção da VF11
**************************************************************************************************
        obj_taxa->estorno_in( i_numero = i_numero
                              i_tipo   = i_tipo
                              i_0090   = i_0090
                              i_vbeln  = i_vbeln
                              i_dir    = i_dir
                              i_seq    = i_seq
                              I_0090_MANUAL = I_0090_MANUAL ).

      WHEN 'INV'.
**************************************************************************************************
*
*
*        I_NUMERO -> Documento de Simulação
*        I_TIPO   -> Parametro de Identificação
*        I_VBELN  -> Documento da Ordem
*
**************************************************************************************************
        obj_taxa->reversao_frete_in( i_numero = i_numero
                                     i_tipo   = i_tipo
                                     i_0090   = i_0090
                                     i_vbeln  = i_vbeln
                                     i_dir    = i_dir
                                     i_seq    = i_seq ).
    ENDCASE.
  ENDMETHOD.


  METHOD LER_XML_TAXA.

    TYPES: BEGIN OF TY_TAXA,
             COTACAO TYPE C LENGTH 10,
             STATUS  TYPE C LENGTH 1,
             MESSAGE TYPE C LENGTH 50,
           END OF TY_TAXA.

    DATA: IF_XML           TYPE REF TO IF_IXML,
          IF_STREAMFACTORY TYPE REF TO IF_IXML_STREAM_FACTORY,
          IF_STREAM        TYPE REF TO IF_IXML_ISTREAM,
          IF_XML_PARSER    TYPE REF TO IF_IXML_PARSER,
          IF_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,

          IF_NODE          TYPE REF TO IF_IXML_NODE,
          IF_MAP           TYPE REF TO IF_IXML_NAMED_NODE_MAP,
          IF_ATTR          TYPE REF TO IF_IXML_NODE,

          ITERATOR         TYPE REF TO IF_IXML_NODE_ITERATOR,
          TAG_NAME         TYPE STRING,
          NAME_DOM         TYPE STRING,
          COUNT_DOM        TYPE I,
          INDEX_DOM        TYPE I,
          PREFIX_DOM       TYPE STRING,
          VALOR_DOM        TYPE STRING,

          NODE_FILHO       TYPE REF TO IF_IXML_NODE,
          VALOR_FILHO      TYPE STRING.

    DATA: GT_TAXA TYPE TABLE OF TY_TAXA,
          GW_TAXA TYPE TY_TAXA.

    FIELD-SYMBOLS: <FS_TAXA> TYPE TY_TAXA.


    IF_XML           = CL_IXML=>CREATE( ).
    IF_DOCUMENT      = IF_XML->CREATE_DOCUMENT( ).
    IF_STREAMFACTORY = IF_XML->CREATE_STREAM_FACTORY( ).
    IF_STREAM        = IF_STREAMFACTORY->CREATE_ISTREAM_STRING( I_XML ).

    IF_XML_PARSER    = IF_XML->CREATE_PARSER(  STREAM_FACTORY = IF_STREAMFACTORY
                                               ISTREAM        = IF_STREAM
                                               DOCUMENT       = IF_DOCUMENT ).

    IF_XML_PARSER->PARSE( ).

    IF_NODE ?= IF_DOCUMENT->GET_ROOT_ELEMENT( ).


    IF NOT ( IF_NODE IS INITIAL ).

      ITERATOR = IF_NODE->CREATE_ITERATOR( ).
      IF_NODE = ITERATOR->GET_NEXT( ).

      WHILE NOT IF_NODE IS INITIAL.


        CASE IF_NODE->GET_TYPE( ).

          WHEN: IF_IXML_NODE=>CO_NODE_ELEMENT.

            TAG_NAME = IF_NODE->GET_NAME( ).
            IF_MAP   = IF_NODE->GET_ATTRIBUTES( ).

            IF NOT ( IF_MAP IS INITIAL ).

              COUNT_DOM = IF_MAP->GET_LENGTH( ).

              DO COUNT_DOM TIMES.

                INDEX_DOM  = SY-INDEX - 1.
                IF_ATTR    = IF_MAP->GET_ITEM( INDEX_DOM ).
                NAME_DOM   = IF_ATTR->GET_NAME( ).
                PREFIX_DOM = IF_ATTR->GET_NAMESPACE_PREFIX( ).
                VALOR_DOM  = IF_ATTR->GET_VALUE( ).

              ENDDO.

              CASE TAG_NAME.
                WHEN: 'cotacao'.
                  GW_TAXA-COTACAO = IF_NODE->GET_VALUE( ).
                  E_COTACAO = GW_TAXA-COTACAO.
                WHEN: 'status'.
                  GW_TAXA-STATUS = IF_NODE->GET_VALUE( ).
                WHEN: 'message'.
                  GW_TAXA-MESSAGE = IF_NODE->GET_VALUE( ).

              ENDCASE.
            ENDIF.
        ENDCASE.
        IF_NODE = ITERATOR->GET_NEXT( ).
      ENDWHILE.
    ENDIF.




  ENDMETHOD.


  METHOD MONTA_XML_TAXA.
    CLEAR: E_XML. "Limpar a variavel de retorno.

    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    DATA: OBJ_ZCL_UTIL TYPE REF TO ZCL_UTIL.
    DATA: VAR_DATA     TYPE C LENGTH 10.
    DATA: VAR_DATA_LIB TYPE C LENGTH 10.

    FREE: OBJ_ZCL_UTIL.

    CREATE OBJECT OBJ_ZCL_UTIL.
    OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = I_DATA
                                             I_OPCAO = '.'
                                   RECEIVING E_DATA = VAR_DATA ).

    OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = I_DATA_LIB
                                             I_OPCAO = '.'
                                   RECEIVING E_DATA = VAR_DATA_LIB ).

    CONC_XML '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:tem="http://tempuri.org/">'.
    CONC_XML  '<soap:Header/>'.
    CONC_XML '<soap:Body>'.

    IF ( SY-CPROG EQ 'ZCARGA').
      CONC_XML       '<tem:GetHedge>'.
      CONC_XML          '<tem:dataRef>'.
      CONC_XML           VAR_DATA_LIB.
      CONC_XML         '</tem:dataRef>'.
      CONC_XML          '<tem:dataVencimento>'.
      CONC_XML           VAR_DATA.
      CONC_XML         '</tem:dataVencimento>'.
    ELSE.
      CONC_XML       '<tem:Get>'.
      CONC_XML          '<tem:data>'.
      CONC_XML           VAR_DATA.
      CONC_XML         '</tem:data>'.
    ENDIF.

    CONC_XML          '<tem:tipo>'.
    CONC_XML           I_TIPO.
    CONC_XML       '</tem:tipo>'.

    IF ( SY-CPROG EQ 'ZCARGA').
      CONC_XML       '</tem:GetHedge>'.
    ELSE.
      CONC_XML       '</tem:Get>'.
    ENDIF.

    CONC_XML    '</soap:Body>'.
    CONC_XML  '</soap:Envelope>'.


  ENDMETHOD.
ENDCLASS.
