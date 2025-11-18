class ZCL_WEBSERVICE_ORD_CAR definition
  public
  inheriting from ZCL_WEBSERVICE
  final
  create public .

*"* public components of class ZCL_WEBSERVICE_ORD_CAR
*"* do not include other source files here!!!
public section.

  methods BUSCAR_TRANSPORTE
    importing
      value(I_NR_ORDEM) type STRING
      value(I_SAFRA) type STRING
      value(I_FILIAL) type WERKS_D optional
    returning
      value(E_ORDEM_CAR) type ref to ZCL_ORDEM_CAR .
protected section.
*"* protected components of class ZCL_WEBSERVICE_ORD_CAR
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_WEBSERVICE_ORD_CAR
*"* do not include other source files here!!!

  methods MONTA_XML_TRANSP
    importing
      !I_NR_ORDEM type STRING
      !I_SAFRA type STRING
    returning
      value(E_XML) type STRING .
  methods LER_XML_TRANSP
    importing
      !I_XML type STRING
    returning
      value(E_ORDEM_CAR) type ref to ZCL_ORDEM_CAR .
ENDCLASS.



CLASS ZCL_WEBSERVICE_ORD_CAR IMPLEMENTATION.


METHOD buscar_transporte.

  DATA: var_msg  TYPE string, "Variavel para mostrar a Mensagem texto da exception.
        var_http TYPE REF TO if_http_client. "Interface HTTP Client

  DATA: cx_exception TYPE REF TO zcx_webservice. "Referencia para a Classe de Exception.

  DATA: gw_xml        TYPE string, "String para guardar informações do XML
        gw_xml_transp TYPE string, "String para guardar informações do XML do Transp.
        lc_nr_ordem   TYPE zsdt0001od-nr_ordem,  "*-CS2024000522-12.09.2024-JT-#152417-inicio
        lc_nr_safra   TYPE zsdt0001od-nr_safra,  "*-CS2024000522-12.09.2024-JT-#152417-inicio
        lra_filial    TYPE RANGE OF  WERKS_D."IR246503 - Correção busca OC #184471 - BG

  lc_nr_ordem = i_nr_ordem.  "*-CS2024000522-12.09.2024-JT-#152417-inicio
  lc_nr_safra = i_safra.     "*-CS2024000522-12.09.2024-JT-#152417-inicio

  APPEND VALUE #( sign = 'I' option = 'EQ' low = I_FILIAL ) TO lra_filial. "IR246503 - Correção busca OC #184471 - BG

  SELECT SINGLE * INTO @DATA(wa_zsdt0001od)
    FROM zsdt0001od
   WHERE nr_ordem  EQ @lc_nr_ordem  "@i_nr_ordem
     AND nr_safra  EQ @lc_nr_safra "@i_safra.
     AND ID_BRANCH IN @lra_filial "IR246503 - Correção busca OC #184471 - BG
     AND TP_STATUS EQ 'AB'.       "IR246503 - Correção busca OC #184471 - BG

  IF sy-subrc IS NOT INITIAL.

    CREATE OBJECT e_ordem_car.
    e_ordem_car->set_mensagem_ret( 'Ordem de Carregamento não encontrada!' ).

*    TRY .
*        "Atribui o serviço que precisa ser consultado.
*        "OC = Ordem Carrramento.
*        ME->SET_SERVICO( EXPORTING I_SERVICO = 'OC' ).
*
*      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
*        VAR_MSG  = CX_EXCEPTION->GET_TEXT( ).
*        MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
*    ENDTRY.
*
*    "Atribui o Tipo de Serviço
*    "A = atualização.
*    ME->SET_TIPO( EXPORTING I_TIPO = 'C').
*
*    TRY .
*
*        "Atribui as Informações do HTTP Client para consultar o WebService.
*        VAR_HTTP = ME->URL( ).
*
*      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
*        VAR_MSG  = CX_EXCEPTION->GET_TEXT( ).
*        MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
*    ENDTRY.
*
*    "Abrir a conexão com o serviço.
*    ME->ZIF_WEBSERVICE~ABRIR_CONEXAO( VAR_HTTP ).
*
*    GW_XML = MONTA_XML_TRANSP( I_NR_ORDEM = I_NR_ORDEM
*                               I_SAFRA    = I_SAFRA ).
*
*    "Envia para Consultar os dados transporte com as informações preenchidas acima.
*    "O retorno é de um arquivo XML com todas os dados do Transporte(Placas e Motorista).
*    GW_XML_TRANSP = ME->ZIF_WEBSERVICE~CONSULTAR( I_HTTP = VAR_HTTP
*                                                  I_XML  = GW_XML
*                                                ).
*
*    ME->LER_XML_TRANSP( EXPORTING
*                          I_XML = GW_XML_TRANSP
*                        RECEIVING
*                          E_ORDEM_CAR = E_ORDEM_CAR  ).
  ELSE.

    CREATE OBJECT e_ordem_car.
    e_ordem_car->set_motorista( i_motorista = wa_zsdt0001od-id_motorista ).
    e_ordem_car->set_placa_cav( i_placa_cav = wa_zsdt0001od-ds_placa_trator ).
    e_ordem_car->set_placa1( i_placa1 = wa_zsdt0001od-ds_placa_reboq_1 ).
    e_ordem_car->set_placa2( i_placa2 = wa_zsdt0001od-ds_placa_reboq_2 ).
    e_ordem_car->set_placa3( i_placa3 = wa_zsdt0001od-ds_placa_reboq_3 ).
    e_ordem_car->at_zsdt0001od = wa_zsdt0001od.
    e_ordem_car->set_mensagem_ret( 'Sucesso' ).

  ENDIF.


ENDMETHOD.


method LER_XML_TRANSP.

  DATA: OBJ_ORDEM_CAR    TYPE REF TO ZCL_ORDEM_CAR.

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

        NODE_FILHO      TYPE REF TO IF_IXML_NODE,
        VALOR_FILHO     TYPE STRING,

        VL_MOTORISTA    TYPE ZMOTORISTA,
        VL_PLACA_CAV    TYPE ZPLACA,
        VL_PLACA1       TYPE ZPLACA,
        VL_PLACA2       TYPE ZPLACA,
        VL_MENSAGEM     TYPE STRING.

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

    FREE: OBJ_ORDEM_CAR.
    CREATE OBJECT OBJ_ORDEM_CAR.

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
              WHEN: 'codigoSapMotorista'.
                VL_MOTORISTA = IF_NODE->GET_VALUE( ).
                OBJ_ORDEM_CAR->SET_MOTORISTA( VL_MOTORISTA ).
              WHEN: 'placaCavalo'.
                VL_PLACA_CAV = IF_NODE->GET_VALUE( ).
                OBJ_ORDEM_CAR->SET_PLACA_CAV( VL_PLACA_CAV ).
              WHEN: 'placaCarreta1'.
                VL_PLACA1 = IF_NODE->GET_VALUE( ).
                OBJ_ORDEM_CAR->SET_PLACA1( VL_PLACA1 ).
              WHEN: 'placaCarreta2'.
                VL_PLACA2 = IF_NODE->GET_VALUE( ).
                OBJ_ORDEM_CAR->SET_PLACA2( VL_PLACA2 ).
              WHEN: 'mensagem'.
                VL_MENSAGEM = IF_NODE->GET_VALUE( ).
                OBJ_ORDEM_CAR->SET_MENSAGEM_RET( VL_MENSAGEM ).

            ENDCASE.
          ENDIF.
      ENDCASE.
      IF_NODE = ITERATOR->GET_NEXT( ).
    ENDWHILE.

    E_ORDEM_CAR = OBJ_ORDEM_CAR.


  ENDIF.


endmethod.


method MONTA_XML_TRANSP.

  CLEAR: E_XML. "Limpar a variavel de retorno.

  DEFINE CONC_XML.
    CONCATENATE E_XML &1 INTO E_XML.
  END-OF-DEFINITION.

  DATA: VAR_NR_ORDEM   TYPE STRING,
        VAR_SAFRA      TYPE STRING.

  VAR_NR_ORDEM   = I_NR_ORDEM.
  VAR_SAFRA      = I_SAFRA.

  CONC_XML '<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:tem="http://tempuri.org/">'.
  CONC_XML    '<soap:Header/>'.
  CONC_XML    '<soap:Body>'.
  CONC_XML       '<tem:Get>'.

  CONC_XML         '<tem:numeroOrdem>'.
  CONC_XML            VAR_NR_ORDEM.
  CONC_XML         '</tem:numeroOrdem>'.

  CONC_XML          '<tem:safra>'.
  CONC_XML            I_SAFRA.
  CONC_XML         '</tem:safra>'.

  CONC_XML       '</tem:Get>'.
  CONC_XML    '</soap:Body>'.
  CONC_XML '</soap:Envelope>'.

endmethod.
ENDCLASS.
