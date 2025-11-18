class ZCL_INTEGRACAO_DOC_CONT_FORN_S definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  types:
    BEGIN OF ty_erros,
        mensagem TYPE char100,
      END OF ty_erros .

      data:
    tl_erro TYPE  TABLE OF ty_erros .
  types:
    BEGIN OF ty_erro,
        erros LIKE tl_erro,
      END OF ty_erro .


  data:
    BEGIN OF zde_data_request,
        num_ov           TYPE zfit0026-vbeln,
        objkey_sigam     TYPE zfit0026-objkey_sigam,
        DOC_CONTAB_FOR         TYPE zfit0026-docnum,
      END OF zde_data_request .
  data:
    tb_erro TYPE TABLE OF string .
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '228' ##NO_TEXT.
  data:
    BEGIN OF zde_data_response,
        erros TYPE TABLE OF ty_erro,
      END OF zde_data_response .
  data:
    BEGIN OF zde_data_response_doc,
        doc_compensacao TYPE ZIB_CONTABIL_CHV-belnr,
      END OF zde_data_response_doc .

  methods CONSTRUCTOR
    exceptions
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_DOC_CONT_FORN_S IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  METHOD ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.

    DATA: w_data_inbound   TYPE zsdt0325_inb_cliente,
          lit_data_inbound TYPE zsdt0325_inbcli_t,
          w_tx             TYPE zsdt0327tx,
          lva_kna1         TYPE kna1,
          lva_tabix        TYPE sy-tabix,
          lva_duplic_flg   TYPE c,
          lva_kunnret      TYPE kna1-kunnr,
          lva_erro         TYPE string,
          lva_type         TYPE dd01v-datatype,
          l_erro(1).

    DATA: lwa_data_response_erro LIKE zde_data_response.
    DATA: lwa_data_request LIKE zde_data_request,
          lwa_data_response_doc  LIKE zde_data_response_doc.


    TYPES:
      BEGIN OF ty_erro,
        mensagem TYPE char100,
      END OF ty_erro.

    DATA: it_mensagem TYPE TABLE OF  ty_erro,
          wa_mensagem TYPE  ty_erro.

    CLEAR: r_msg_erro.

    IF ( me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'POST' AND  me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'PUT' ).
      r_msg_erro = 'Metodo informado não reconhecido!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_data_inbound ).
    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

    "FIELD-SYMBOLS: <fs_erro> TYPE lit_data_inbound.
    APPEND INITIAL LINE TO lwa_data_response_erro-erros ASSIGNING FIELD-SYMBOL(<fs_erro>).

    IF lwa_data_request-num_ov IS INITIAL.
      wa_mensagem = 'O num_ov é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
    ENDIF.

    IF lwa_data_request-DOC_CONTAB_FOR IS INITIAL.
      wa_mensagem =  'A Doc Compensação é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
    ENDIF.

    IF lwa_data_request-objkey_sigam IS INITIAL.
      wa_mensagem =  'O objKey_sigam é um campo obrigatório.'.
      APPEND wa_mensagem TO <fs_erro>-erros.
      l_erro = 'X'.
    ENDIF.

    IF l_erro IS NOT INITIAL.
      r_msg_erro = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response_erro ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

    DATA: LWA_DATA_REQUEST       LIKE ZDE_DATA_REQUEST,
          LWA_DATA_RESPONSE_ERRO LIKE ZDE_DATA_RESPONSE,
          LWA_DATA_RESPONSE_DOC  LIKE ZDE_DATA_RESPONSE_DOC.

    TYPES: BEGIN OF TY_msg,
             TCODE   TYPE BDC_TCODE,
             DYNAME  TYPE BDC_MODULE,
             DYNUMB  TYPE BDC_DYNNR,
             MSGTYP  TYPE BDC_MART,
             MSGSPRA TYPE BDC_SPRAS,
             MSGID   TYPE BDC_MID,
             MSGNR   TYPE BDC_MNR,
             MSGV1   TYPE BDC_VTEXT1,
             MSGV2   TYPE BDC_VTEXT1,
             MSGV3   TYPE BDC_VTEXT1,
             MSGV4   TYPE BDC_VTEXT1,
             ENV     TYPE BDC_AKT,
             FLDNAME TYPE FNAM_____4,
           END OF TY_msg.

    DATA: IT_MSG           TYPE TABLE OF TY_msg,
          WA_MSG           TYPE TY_msg,
          WL_MODE(1),
          WG_DOCUMENTO(10),
          L_AUGLV          TYPE T041A-AUGLV   VALUE 'UMBUCHNG', "Posting with Clearing
          L_TCODE          TYPE SY-TCODE      VALUE 'F-51',     "You get an error with any other value
          L_SGFUNCT        TYPE RFIPI-SGFUNCT VALUE 'C',        "Post immediately
          VDATA(10),
          WL_TAXA(7),
          P_MODE           TYPE RFPDO-ALLGAZMD,
          TI_BDCDATA       TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
          WA_BDCDATA       LIKE LINE OF TI_BDCDATA,
          L_ERRO(1),
          WA_BSIK_AUX      TYPE BSIK,
          TG_BSIK_AUX      TYPE TABLE OF BSIK,
          WL_LFBK          TYPE LFBK,
          WA_ZFIT0026      TYPE ZFIT0026,
          LVA_MSG_ERRO     TYPE STRING,
          V_COMPENSAR(1).
    CONSTANTS: C_MSGID LIKE WA_msg-MSGID VALUE 'F5',
               C_MSGNR LIKE WA_msg-MSGNR VALUE '312',
               C_MSGNE LIKE WA_msg-MSGNR VALUE '539'.
    R_IF_INTEGRACAO_INJECT = ME.

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = LWA_DATA_REQUEST ).

    R_IF_INTEGRACAO_INJECT = ME.

    CLEAR: E_MSG_ERRO, E_SUCESSO, E_NM_CODE, E_MSG_OUTBOUND.", lwa_data_response.

    IF I_MSG_INBOUND IS NOT INITIAL.
      /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = LWA_DATA_REQUEST ).
    ENDIF.

    ME->ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND( EXPORTING I_DATA_INBOUND =  I_MSG_INBOUND IMPORTING E_STATUS_CODE  =  DATA(_STATUS_CODE)  RECEIVING R_MSG_ERRO = LVA_MSG_ERRO ).

    IF LVA_MSG_ERRO IS NOT INITIAL.

      IF _STATUS_CODE IS INITIAL .
        _STATUS_CODE = '400'. "Bad Request
      ENDIF.

      E_SUCESSO      = ABAP_TRUE.
      E_NM_CODE      = _STATUS_CODE.
      E_MSG_OUTBOUND = LVA_MSG_ERRO.

      RETURN.
    ENDIF.

    IF LWA_DATA_REQUEST IS NOT INITIAL."1
      APPEND INITIAL LINE TO LWA_DATA_RESPONSE_ERRO-ERROS ASSIGNING FIELD-SYMBOL(<FS_ERRO>).

      SELECT * FROM VBAK INTO TABLE @DATA(IT_VBAK)
              WHERE VBELN = @LWA_DATA_REQUEST-NUM_OV.

      CHECK NOT IT_VBAK[] IS INITIAL.

      SELECT SINGLE * INTO WA_ZFIT0026 FROM ZFIT0026 WHERE OBJKEY_SIGAM = LWA_DATA_REQUEST-OBJKEY_SIGAM AND VBELN EQ LWA_DATA_REQUEST-NUM_OV.

      IF SY-SUBRC IS INITIAL. "2
        CONCATENATE  WA_ZFIT0026-DATA_PGTO+6(2) WA_ZFIT0026-DATA_PGTO+4(2) WA_ZFIT0026-DATA_PGTO(4) INTO VDATA SEPARATED BY '.'.

        IF WA_ZFIT0026-DOCNUM_FORN IS INITIAL."2.1
          V_COMPENSAR = 'X'.
        ELSE. "2.1

          SELECT SINGLE *
          FROM BSEG
          INTO @DATA(WA_BSEG)
          WHERE BUKRS EQ @WA_ZFIT0026-BUKRS AND
                BELNR EQ @WA_ZFIT0026-DOCNUM AND
                GJAHR EQ @WA_ZFIT0026-DATA_PGTO(4) AND
                BSCHL EQ '09'.

          IF SY-SUBRC IS INITIAL AND WA_BSEG-AUGBL IS NOT INITIAL. "2.2

            LWA_DATA_RESPONSE_DOC-DOC_COMPENSACAO = WA_BSEG-AUGBL.

          ELSE. "tem na tabela zfit0026 mas está estornado   "2.2
            V_COMPENSAR = 'X'.
*            CONCATENATE 'Não foi encontrado o Doc de compensação para o ' LWA_DATA_REQUEST-DOC_CONTAB_FOR '. Verifique se houve estorno.' INTO WA_MSG SEPARATED BY SPACE.
*            APPEND WA_Msg TO <FS_ERRO>-ERROS.
*            L_ERRO = 'X'.
          ENDIF. "2.2
        ENDIF."2.1

        IF V_COMPENSAR IS NOT INITIAL."3

          UPDATE ZFIT0026
          SET DOCNUM_FORN  = LWA_DATA_REQUEST-DOC_CONTAB_FOR
              Status       =  'G'
          WHERE VBELN  =  LWA_DATA_REQUEST-NUM_OV  AND
                OBJKEY_SIGAM = LWA_DATA_REQUEST-OBJKEY_SIGAM.

          IF SY-SUBRC IS INITIAL.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.

          "***************************************************************************
          "                          GERAR COMPENSACAO                               *
          "***************************************************************************


          CLEAR: WA_BSIK_AUX, TG_BSIK_AUX.
          SELECT SINGLE *
             FROM BSIK
             INTO WA_BSIK_AUX
             WHERE BUKRS EQ WA_ZFIT0026-BUKRS
             AND   BELNR EQ LWA_DATA_REQUEST-DOC_CONTAB_FOR.

          CONCATENATE  WA_ZFIT0026-DATA_PGTO+6(2) WA_ZFIT0026-DATA_PGTO+4(2) WA_ZFIT0026-DATA_PGTO(4) INTO VDATA SEPARATED BY '.'.
          READ TABLE IT_VBAK INTO DATA(WA_VBAK) INDEX 1.

          WL_TAXA = WA_ZFIT0026-TAXA.
          REPLACE '.' WITH ',' INTO WL_TAXA.
          CLEAR WA_BDCDATA.

          WA_BDCDATA-PROGRAM = 'SAPMF05A'.
          WA_BDCDATA-DYNPRO =  '0122'.
          WA_BDCDATA-DYNBEGIN = 'X'.
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL = ''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.

          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =  'BKPF-WAERS'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.

          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =  '/00'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BLDAT'.
          WA_BDCDATA-FVAL =  VDATA.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BLART'.
          WA_BDCDATA-FVAL =  'TA'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BUKRS'.
          WA_BDCDATA-FVAL = WA_BSIK_AUX-BUKRS.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BUDAT'.
          WA_BDCDATA-FVAL =   VDATA .

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-MONAT'.
          WA_BDCDATA-FVAL = WA_ZFIT0026-dATA_pgto+4(2).

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-WAERS'.
          WA_BDCDATA-FVAL =  WA_ZFIT0026-MOEDA.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM =  ''       .
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'FS006-DOCID'.
          WA_BDCDATA-FVAL =  '*'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPMF05A'.
          WA_BDCDATA-DYNPRO = '0122'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =  ''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =  'BKPF-KURSF'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =  '/00'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BLDAT'.
          WA_BDCDATA-FVAL =  VDATA .

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BLART'.
          WA_BDCDATA-FVAL = 'TA'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BUKRS'.
          WA_BDCDATA-FVAL = WA_BSIK_AUX-BUKRS.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BUDAT'.
          WA_BDCDATA-FVAL =  VDATA .

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-MONAT'.
          WA_BDCDATA-FVAL = WA_ZFIT0026-dATA_pgto+4(2).

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-WAERS'.
          WA_BDCDATA-FVAL = WA_ZFIT0026-MOEDA.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-KURSF'.
          WA_BDCDATA-FVAL = WL_TAXA.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-WWERT'.
          WA_BDCDATA-FVAL =  VDATA .

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


*WA_BDCDATA-PROGRAM = ''.
*WA_BDCDATA-DYNPRO = ''    .
*WA_BDCDATA-DYNBEGIN = ''.
*WA_BDCDATA-FNAM = 'BKPF-AWSYS'.
*WA_BDCDATA-FVAL = QASCLNT300'.
*
*append wa_bdcdata to ti_bdcdata.
*clear wa_bdcdata.
*
*
          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'FS006-DOCID'.
          WA_BDCDATA-FVAL =      	'*'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPMF05A'.
          WA_BDCDATA-DYNPRO = '0122'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =    	''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =   'RF05A-NEWBS'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =   '=SL'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BLDAT'.
          WA_BDCDATA-FVAL =    VDATA .

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BLART'.
          WA_BDCDATA-FVAL =   'TA'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BUKRS'.
          WA_BDCDATA-FVAL =  WA_BSIK_AUX-BUKRS.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-BUDAT'.
          WA_BDCDATA-FVAL =    VDATA .

          APPEND WA_BDCDATA TO TI_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-WAERS'.
          WA_BDCDATA-FVAL =   WA_ZFIT0026-MOEDA.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-KURSF'.
          WA_BDCDATA-FVAL =  WL_TAXA.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BKPF-WWERT'.
          WA_BDCDATA-FVAL =    VDATA .

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


*WA_BDCDATA-PROGRAM = ''.
*WA_BDCDATA-DYNPRO = ''    .
*WA_BDCDATA-DYNBEGIN = ''.
*WA_BDCDATA-FNAM = 'BKPF-AWSYS'.
*WA_BDCDATA-FVAL =   'QASCLNT300'.
*
*append wa_bdcdata to ti_bdcdata.
*clear wa_bdcdata.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = '' 	 .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'FS006-DOCID'.
          WA_BDCDATA-FVAL =   '*'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPMF05A'.
          WA_BDCDATA-DYNPRO = '0710'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =  	''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =   'RF05A-XPOS1(03)'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =   '/00'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-AGBUK'.
          WA_BDCDATA-FVAL =   WA_BSIK_AUX-BUKRS.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-AGKON'.
          WA_BDCDATA-FVAL =   WA_BSIK_AUX-LIFNR.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-AGKOA'.
          WA_BDCDATA-FVAL =   'K'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-AGUMS'.
          WA_BDCDATA-FVAL =   'AZKL'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-XNOPS'.
          WA_BDCDATA-FVAL =   'X'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-XPOS1(01)'.
          WA_BDCDATA-FVAL =   ''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-XPOS1(03)'.
          WA_BDCDATA-FVAL =   'X'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPMF05A'.
          WA_BDCDATA-DYNPRO = '0731'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =   ''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''     .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =   'RF05A-SEL01(01)'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''     .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =   '=SLK'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''     .
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-SEL01(01)'.
          WA_BDCDATA-FVAL =   LWA_DATA_REQUEST-DOC_CONTAB_FOR.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPMF05A'.
          WA_BDCDATA-DYNPRO = '0710'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =  	''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =   'RF05A-XPOS1(03)'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =   '/00'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-AGBUK'.
          WA_BDCDATA-FVAL =   WA_VBAK-BUKRS_VF.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.

          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-AGKON'.
          WA_BDCDATA-FVAL =  WA_VBAK-KUNNR.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.

          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-AGKOA'.
          WA_BDCDATA-FVAL =   'D'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-AGUMS'.
          WA_BDCDATA-FVAL =   'AZKL'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-XNOPS'.
          WA_BDCDATA-FVAL =   'X'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-XPOS1(01)'.
          WA_BDCDATA-FVAL =   ''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-XPOS1(03)'.
          WA_BDCDATA-FVAL =   'X'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPMF05A'.
          WA_BDCDATA-DYNPRO = '0731'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =  	''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM =  'BDC_CURSOR'.
          WA_BDCDATA-FVAL =   'RF05A-SEL01(01)'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM =  'BDC_OKCODE'.
          WA_BDCDATA-FVAL =   '=PA'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM =  'RF05A-SEL01(01)'.
          WA_BDCDATA-FVAL =   WA_ZFIT0026-DOCNUM.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPDF05X'.
          WA_BDCDATA-DYNPRO = '3100'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =  ''	.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =  '=PI'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_SUBSCR'.
          WA_BDCDATA-FVAL =  'SAPDF05X    6102PAGE'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =  'DF05B-PSBET(02)'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-ABPOS'.
          WA_BDCDATA-FVAL =  '1'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPDF05X'.
          WA_BDCDATA-DYNPRO = '3100'  .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =  ''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =  '=PI'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_SUBSCR'.
          WA_BDCDATA-FVAL =  'SAPDF05X    6102PAGE'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =  'DF05B-PSBET(04)'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-ABPOS'.
          WA_BDCDATA-FVAL =  '1'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPDF05X'.
          WA_BDCDATA-DYNPRO = '3100'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =  '' .

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =  '=BS'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_SUBSCR'.
          WA_BDCDATA-FVAL =  'SAPDF05X  6102PAGE'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =  'DF05B-PSBET(04)'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'RF05A-ABPOS'.
          WA_BDCDATA-FVAL =  '1'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = 'SAPMF05A'.
          WA_BDCDATA-DYNPRO = '0700'   .
          WA_BDCDATA-DYNBEGIN = 'X'  .
          WA_BDCDATA-FNAM = ''.
          WA_BDCDATA-FVAL =  ''.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNPRO = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_CURSOR'.
          WA_BDCDATA-FVAL =  'RF05A-NEWBS'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.


          WA_BDCDATA-PROGRAM = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-DYNBEGIN = ''.
          WA_BDCDATA-FNAM = 'BDC_OKCODE'.
          WA_BDCDATA-FVAL =  '=BU'.

          APPEND WA_BDCDATA TO TI_BDCDATA.
          CLEAR WA_BDCDATA.



          REFRESH IT_MSG.

          WL_MODE = 'N'.

          CALL TRANSACTION L_TCODE USING TI_BDCDATA
                MODE WL_MODE
                MESSAGES INTO IT_MSG.

          READ TABLE IT_MSG TRANSPORTING NO FIELDS WITH KEY MSGTYP = 'A'  .
          IF SY-SUBRC = 0.
            L_erro = 'X'.
          ELSE.
            READ TABLE IT_MSG TRANSPORTING NO FIELDS WITH KEY MSGTYP = 'E'.
            IF SY-SUBRC = 0.
              L_erro = 'X'.
            ENDIF.
          ENDIF.

          CLEAR WG_DOCUMENTO.
          READ TABLE IT_MSG  INTO DATA(WAA_MSG) WITH KEY MSGID = C_MSGID
                                     MSGNR = C_MSGNR
                                     MSGTYP = 'S'.

          IF SY-SUBRC = 0.
            MOVE WAA_MSG-MSGV1 TO WG_DOCUMENTO.
          ENDIF.

          IF  WG_DOCUMENTO IS INITIAL.

            LOOP AT IT_MSG INTO DATA(WW_MSG) WHERE MSGTYP = 'E'.
              WA_MSG = WW_MSG-MSGV1.
              APPEND WA_Msg TO <FS_ERRO>-ERROS.

            ENDLOOP.
            L_ERRO = 'X'.
            UPDATE ZFIT0026
                SET DOCNUM_FORN  = ' '
                    Status       =  'S'
                WHERE VBELN  =  LWA_DATA_REQUEST-NUM_OV
                AND OBJKEY_SIGAM = LWA_DATA_REQUEST-oBJKEY_SIGAM.
            IF SY-SUBRC IS INITIAL.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.
          ELSE.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WG_DOCUMENTO
              IMPORTING
                OUTPUT = WG_DOCUMENTO.
            LWA_DATA_RESPONSE_DOC = WG_DOCUMENTO.
          ENDIF.
        ENDIF. "3
      ENDIF."2
    ENDIF."1

    IF L_ERRO IS NOT INITIAL.


      E_SUCESSO   = ABAP_FALSE.
      E_MSG_OUTBOUND = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DATA_RESPONSE_ERRO ).
    ELSE.
      E_SUCESSO   = ABAP_TRUE.
      E_MSG_OUTBOUND = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DATA_RESPONSE_DOC ).
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.
