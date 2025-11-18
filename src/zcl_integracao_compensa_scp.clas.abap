class ZCL_INTEGRACAO_COMPENSA_SCP definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COMPENSA_SCP .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_COMPENSA_SCP IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_compensa_scp.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_referencia-tp_referencia = 'COMPENSA_SCP'.

  ENDMETHOD.


  METHOD zif_integracao_compensa_scp~get_instance.

    IF zif_integracao_compensa_scp~at_if_integracao_compensa_scp IS NOT BOUND.
      CREATE OBJECT zif_integracao_compensa_scp~at_if_integracao_compensa_scp TYPE zcl_integracao_compensa_scp.
    ENDIF.
    r_if_integracao_compensa_scp = zif_integracao_compensa_scp~at_if_integracao_compensa_scp.

  ENDMETHOD.


  METHOD zif_integracao_compensa_scp~set_ds_data.

    DATA: i_inbound       TYPE zde_compensa_scp,
          t_element_array	TYPE zde_element_array_t.

    "Incluir Texto JSON para integração
    r_if_integracao_compensa_scp = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

    APPEND 'det' TO t_element_array.
    APPEND 'detPag' TO t_element_array.
    APPEND 'NFref' TO t_element_array.
    APPEND 'DI' TO t_element_array.
    APPEND 'adi' TO t_element_array.
    APPEND 'detExport' TO t_element_array.
    APPEND 'med' TO t_element_array.
    APPEND 'arma' TO t_element_array.
    APPEND 'comb' TO t_element_array.
    APPEND 'vol' TO t_element_array.
    APPEND 'lacres' TO t_element_array.
    APPEND 'dup' TO t_element_array.
    APPEND 'pag' TO t_element_array.
    APPEND 'procRef' TO t_element_array.
    APPEND 'obsCont' TO t_element_array.
    APPEND 'obsFisco' TO t_element_array.

    CASE zcl_string=>upper( CONV #( i_info-ds_formato ) ).
      WHEN 'XML'.
        "Validar Json
        /ui2/cl_json=>deserialize(
           EXPORTING
             json = zcl_string=>xml_to_json( i_xml = i_info-ds_body i_element_array =  t_element_array )
           CHANGING data = i_inbound
        ).
      WHEN 'JSON'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = i_inbound ).
    ENDCASE.

    me->zif_integracao_inject~at_referencia-id_referencia = i_inbound-empresa   && '-' &&
                                                            i_inbound-documento && '-' &&
                                                            i_inbound-ano       && '-' &&
                                                            i_inbound-linha.

  ENDMETHOD.


  METHOD zif_integracao_compensa_scp~set_send_msg.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_if_integracao_compensa_scp = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = 'compensacao_scp'.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_compensa_scp~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
           IMPORTING
             e_data_retorno = DATA(e_data_retorno)
             e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    e_msg = e_data_retorno.
    e_protocolo = zcl_string=>lpad( i_str  = CONV #( e_integracao-id_integracao ) i_qtd  = 15 i_char = '0'  ).

    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: i_inbound       TYPE zde_compensa_scp,
          t_element_array	TYPE zde_element_array_t,
*
          t_bkdf          TYPE TABLE OF bkdf,
          t_bkpf          TYPE TABLE OF bkpf,
          t_bsec          TYPE TABLE OF bsec,
          t_bsed          TYPE TABLE OF bsed,
          t_bseg          TYPE TABLE OF bseg,
          t_bset          TYPE TABLE OF bset,
          t_xbseg         TYPE TABLE OF fbseg,
          w_xbseg         TYPE fbseg,
          t_ybseg         TYPE TABLE OF fbseg,
          w_ybseg         TYPE fbseg,
*
          l_bukrs         TYPE numc4,
          l_belnr         TYPE numc10,
          l_gjahr         TYPE numc4,
          l_buzei         TYPE numc3,
*
          l_objectid      TYPE cdhdr-objectid,
          l_tcode         TYPE cdhdr-tcode,
          l_utime         TYPE cdhdr-utime,
          l_udate         TYPE cdhdr-udate,
          l_username      TYPE cdhdr-username.



    FREE: t_bkdf,  t_bkpf, t_bsec,
          t_bsed,  t_bseg, t_bset,
          t_xbseg, t_ybseg.

    e_sucesso              = abap_false.
    e_nm_code              = abap_false.
    r_if_integracao_inject = me.

    APPEND 'det'       TO t_element_array.
    APPEND 'detPag'    TO t_element_array.
    APPEND 'NFref'     TO t_element_array.
    APPEND 'DI'        TO t_element_array.
    APPEND 'adi'       TO t_element_array.
    APPEND 'detExport' TO t_element_array.
    APPEND 'med'       TO t_element_array.
    APPEND 'arma'      TO t_element_array.
    APPEND 'comb'      TO t_element_array.
    APPEND 'vol'       TO t_element_array.
    APPEND 'lacres'    TO t_element_array.
    APPEND 'dup'       TO t_element_array.
    APPEND 'pag'       TO t_element_array.
    APPEND 'procRef'   TO t_element_array.
    APPEND 'obsCont'   TO t_element_array.
    APPEND 'obsFisco'  TO t_element_array.

    CASE zcl_string=>upper( CONV #( i_msg_completa-ds_formato ) ).
      WHEN 'XML'.
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = zcl_string=>xml_to_json( i_xml = i_msg_inbound  i_element_array = t_element_array )
          CHANGING
            data = i_inbound ).
      WHEN 'JSON'.
        /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = i_inbound ).
    ENDCASE.

*----------------------------
*-- formatacao campos
*----------------------------
    CONDENSE i_inbound-empresa      NO-GAPS.
    CONDENSE i_inbound-documento    NO-GAPS.
    CONDENSE i_inbound-ano          NO-GAPS.
    CONDENSE i_inbound-linha        NO-GAPS.

    l_bukrs = i_inbound-empresa.
    l_belnr = i_inbound-documento.
    l_gjahr = i_inbound-ano.
    l_buzei = i_inbound-linha.

*----------------------------
*-- procura documento
*----------------------------
    SELECT *
      FROM bkpf
      INTO TABLE t_bkpf
     WHERE bukrs = l_bukrs
       AND belnr = l_belnr
       AND gjahr = l_gjahr.

    IF sy-subrc <> 0.
      e_nm_code      = '400'.
      e_msg_erro     = 'Cabecalho do documento não encontrado.'.
      e_msg_outbound = '{ "mensagem" : "'     && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso      = abap_true.
      RETURN.
    ENDIF.

*----------------------------
*-- linha do documento
*----------------------------
    DATA RLDNR_L101C4R4084 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L101C4R4084
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L101C4R4084
    I_BUKRS = L_BUKRS
    I_BELNR = L_BELNR
    I_GJAHR = L_GJAHR
    I_BUZEI = L_BUZEI
  IMPORTING
    ET_BSEG = T_BSEG
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC <> 0 OR LINES( T_BSEG ) = 0.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ELSE.
  SY-DBCNT = LINES( T_BSEG ).
ENDIF.


    IF sy-subrc <> 0.
      e_nm_code      = '400'.
      e_msg_erro     = 'Linha do documento não encontrada.'.
      e_msg_outbound = '{ "mensagem" : "'     && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso      = abap_true.
      RETURN.
    ENDIF.

*----------------------------
*-- atribuicao
*----------------------------
    READ TABLE t_bseg       INTO DATA(w_bseg) INDEX 1.

    MOVE w_bseg-bukrs         TO w_ybseg-bukrs.
    MOVE w_bseg-belnr         TO w_ybseg-belnr.
    MOVE w_bseg-gjahr         TO w_ybseg-gjahr.
    MOVE w_bseg-buzei         TO w_ybseg-buzei.
    MOVE w_bseg-zuonr         TO w_ybseg-zuonr.
    APPEND w_ybseg            TO t_ybseg.

    MOVE w_bseg-bukrs         TO w_xbseg-bukrs.
    MOVE w_bseg-belnr         TO w_xbseg-belnr.
    MOVE w_bseg-gjahr         TO w_xbseg-gjahr.
    MOVE w_bseg-buzei         TO w_xbseg-buzei.
    MOVE i_inbound-atribuicao TO w_xbseg-zuonr.
    APPEND w_xbseg            TO t_xbseg.

    MOVE i_inbound-atribuicao TO w_bseg-zuonr.
    MODIFY t_bseg           FROM w_bseg       INDEX 1.

*----------------------------
*-- modifica documento
*----------------------------
    CALL FUNCTION 'CHANGE_DOCUMENT'
      TABLES
        t_bkdf = t_bkdf
        t_bkpf = t_bkpf
        t_bsec = t_bsec
        t_bsed = t_bsed
        t_bseg = t_bseg
        t_bset = t_bset.

    IF sy-subrc <> 0.
      e_nm_code      = '400'.
      e_msg_erro     = 'Atribuição do Documento não pode ser efetuada!'.
      e_msg_outbound = '{ "mensagem" : "'     && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                          '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
      e_sucesso      = abap_true.
      RETURN.
    ENDIF.

*----------------------------
*-- atualizar historico
*----------------------------
    l_objectid = sy-mandt && l_bukrs && l_belnr && l_gjahr.
    l_tcode    = 'API_SCP'. "sy-tcode.
    l_utime    = sy-uzeit.
    l_udate    = sy-datum.
    l_username = sy-uname.

    CALL FUNCTION 'BELEG_WRITE_DOCUMENT'
      IN UPDATE TASK
      EXPORTING
        objectid                = l_objectid
        tcode                   = l_tcode
        utime                   = l_utime
        udate                   = l_udate
        username                = l_username
        object_change_indicator = 'U'
        upd_bseg                = 'U'
      TABLES
        xbseg                   = t_xbseg
        ybseg                   = t_ybseg.

*----------------------------
*-- commit
*----------------------------
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*----------------------------
*-- msg retorno
*----------------------------
    e_nm_code      = '200'.
    e_msg_erro     = 'Documento modificado com Sucesso'.
    e_msg_outbound = '{ "mensagem" : "'     && e_msg_erro   && '"' && cl_abap_char_utilities=>newline && ',' &&
                        '"status_code" : "' && e_nm_code    && '"' && cl_abap_char_utilities=>newline && '}'.
    e_sucesso      = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.

    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
  endmethod.
ENDCLASS.
