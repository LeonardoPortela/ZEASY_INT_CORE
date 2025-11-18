class ZCL_INTEGRACAO_IPORTSOLUTION definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_IPORTSOLUTION .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type /UI2/SERVICE_NAME optional
      value(I_REQ) type ZMMC_DADOS_INT_COUPA_EBAN optional
      value(I_PARAMETROS) type ZHCME_PY_0004 optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_IPORTSOLUTION IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_iportsolution~set_servico( i_servico = i_servico ).
    me->zif_integracao_iportsolution~set_parametros( i_parametros = i_parametros ).

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_iportsolution.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    IF i_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_iportsolution~at_auth_ws
          WHERE service = i_servico.

    ENDIF.

    SELECT SINGLE *
      FROM zauth_webservice
      INTO me->zif_integracao_iportsolution~at_token_ws
        WHERE service = 'TGG_REFEITORIO_TOKEN'.

    IF me->zif_integracao_iportsolution~at_auth_ws  IS INITIAL OR
       me->zif_integracao_iportsolution~at_token_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_form_fields = me->zif_integracao_inject~at_form_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_integracao_token_iportso(
               zcl_integracao_token_iportso=>zif_integracao_token_iportso~get_instance(
                 )->get_token( )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields = i_form_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

*    DATA: ordem_servico TYPE zde_kuhlmann_hvi_retorno.
*break abap.
*    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
*    "/UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ordem_servico-os_id IS NOT INITIAL.

*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    ZCL_INTEGRACAO_COF_KUHLMANN=>ZIF_INTEGRACAO_COF_KUHLMANN~GET_INSTANCE(
*      )->SET_HVI_CONFIRMAR( EXPORTING
*        "I_USUARIO = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_USUARIO
*        "I_SENHA   = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_SENHA
*        I_OS_ID   = ORDEM_SERVICO-OS_ID
*      ).
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
*    break abap.
*    DATA: ORDEM_SERVICO TYPE ZDE_KUHLMANN_HVI_RETORNO.
*
*    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
*    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ORDEM_SERVICO-OS_ID IS NOT INITIAL.
*
*    DATA: HVI    TYPE ZPP_KUHLMANN_HVI,
*          IT_HVI TYPE TABLE OF ZPP_KUHLMANN_HVI.
*
*    CLEAR: IT_HVI[].
*
*    LOOP AT ORDEM_SERVICO-FARDOS INTO DATA(WA_FARDO).
*      CLEAR: HVI.
*      HVI-FARDO  = WA_FARDO-FARDO.
*      HVI-OS_ID  = ORDEM_SERVICO-OS_ID.
*      HVI-OS_NR  = ORDEM_SERVICO-NUMERO_OS.
*      HVI-ROMANEIO = ORDEM_SERVICO-ROMANEIO.
*      HVI-LABORATORIO = ORDEM_SERVICO-LABORATORIO.
*
*      DATA(DT_ENTRADA) = ORDEM_SERVICO-DATA_ENTRADA(10).
*      REPLACE ALL OCCURRENCES OF '-' IN DT_ENTRADA WITH ''.
*      HVI-DATA_ENTRADA = DT_ENTRADA.
*
*      DATA(DT_ANALISE) = ORDEM_SERVICO-DATA_ANALISE(10).
*      REPLACE ALL OCCURRENCES OF '-' IN DT_ANALISE WITH ''.
*      HVI-DATA_ANALISE = DT_ANALISE.
*
*      HVI-HVI_MIC      = WA_FARDO-MIC.
*      HVI-HVI_POL      = WA_FARDO-POL.
*      HVI-HVI_LEN      = WA_FARDO-LEN.
*      HVI-HVI_STR      = WA_FARDO-STR.
*      HVI-HVI_UNF      = WA_FARDO-UNF.
*      HVI-HVI_ELG      = WA_FARDO-ELG.
*      HVI-HVI_MAT      = WA_FARDO-MAT.
*      HVI-HVI_RD       = WA_FARDO-RD.
*      HVI-HVI_B        = WA_FARDO-B.
*      HVI-HVI_CG       = WA_FARDO-CG.
*      HVI-HVI_LEAF     = WA_FARDO-LEAF.
*      HVI-HVI_ARE      = WA_FARDO-ARE.
*      HVI-HVI_COUNT    = WA_FARDO-COUNT.
*      HVI-HVI_SFI      = WA_FARDO-SFI.
*      HVI-HVI_CSP      = WA_FARDO-CSP.
*      HVI-HVI_SCI      = WA_FARDO-SCI.
*      HVI-HVI_MAT_PORC = WA_FARDO-MAT_PORC.
*      HVI-HVI_POL_FRAC = WA_FARDO-POL_FRAC.
*      HVI-HVI_LOTE     = WA_FARDO-LOTE.
*      HVI-HVI_PADRAO   = WA_FARDO-PADRAO.
*      APPEND HVI TO IT_HVI.
*    ENDLOOP.
*
*    IF IT_HVI[] IS NOT INITIAL.
*      MODIFY ZPP_KUHLMANN_HVI FROM TABLE IT_HVI.
*      COMMIT WORK AND WAIT.
*    ENDIF.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_iportsolution~get_acesso.


    DATA: dataminima   TYPE c LENGTH 10,
          datamaxima   TYPE c LENGTH 10,
          empresa_tgg  TYPE pa0001-bukrs VALUE '0048',
          lc_retorno   TYPE zhcme_py_0001,
          it_0025      TYPE TABLE OF zhcmt_py_0025,
          seq          TYPE numc15,
          i_parametros TYPE zhcme_py_0004,
          cpf14        TYPE char14,
          cpf11        TYPE char11,
          vl_url       TYPE string.

    me->zif_integracao_iportsolution~set_ds_url( ).

    i_parametros = zif_integracao_iportsolution~at_parametros.

    dataminima = |{ i_parametros-dataminima(4) }/{ i_parametros-dataminima+4(2) }/{ i_parametros-dataminima+6(2) }|.
    datamaxima = |{ i_parametros-datamaxima(4) }/{ i_parametros-datamaxima+4(2) }/{ i_parametros-datamaxima+6(2) }|.

    vl_url = |{ vl_url }?localEspecifico=1|.

    CASE abap_true.
      WHEN i_parametros-apenasfuncionario.
        vl_url = |{ vl_url }&apenasFuncionarios=1|.
      WHEN OTHERS.
        vl_url = |{ vl_url }&apenasFuncionarios=2|.
    ENDCASE.

    IF dataminima IS NOT INITIAL.
      vl_url = |{ vl_url }&dataMinima={ dataminima }|.
    ENDIF.

    IF datamaxima IS NOT INITIAL.
      vl_url = |{ vl_url }&dataMaxima={ datamaxima }|.
    ENDIF.

    IF i_parametros-documento IS NOT INITIAL.

      cpf14 = i_parametros-documento.
      REPLACE ALL OCCURRENCES OF '.' IN cpf14 WITH ''.
      REPLACE ALL OCCURRENCES OF '-' IN cpf14 WITH ''.
      cpf11 = cpf14.

      vl_url = |{ vl_url }&documento={ cpf11 }|.

    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_url =
    me->zif_integracao_iportsolution~at_auth_ws-url && vl_url.

    me->zif_integracao_inject~at_referencia =
    VALUE #(
             tp_referencia = 'Acesso Periodo'
             id_referencia = |{ i_parametros-dataminima+4(2) }/{ i_parametros-dataminima(4) }|
           ).

    me->zif_integracao_iportsolution~set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao) e_integracao  = e_integracao ).

    CHECK e_integracao-nm_code EQ 200.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json        = e_integracao-ds_data_retorno
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = lc_retorno.

    LOOP AT lc_retorno-value INTO DATA(w_value).

      IF w_value-documento IS NOT INITIAL.
        CLEAR cpf14.

        cpf11 = w_value-documento.

        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = cpf11   " CPF in internal format (NUMC 11)
          IMPORTING
            output = cpf14.  " CPF in screen format (999.999.999-99)

      ENDIF.

      SELECT COUNT(*)
        FROM zhcmt_py_0025
      WHERE anopr  EQ i_parametros-datamaxima(4)
        AND mespr  EQ i_parametros-datamaxima+4(2)
        AND nr_cpf EQ cpf14
        AND status EQ abap_true.

      CHECK sy-subrc IS NOT INITIAL.

      SELECT *
        FROM pa0465
        INTO TABLE @DATA(it_pernr)
        WHERE cpf_nr EQ @cpf14.

      IF sy-subrc IS INITIAL.

        SELECT *
          FROM pa0000
          INTO TABLE @DATA(it_pa0000)
          FOR ALL ENTRIES IN @it_pernr
          WHERE pernr EQ @it_pernr-pernr
            AND endda >= @sy-datum
            AND stat2 NE '0'.

        IF sy-subrc IS INITIAL.

          SELECT *
            FROM pa0001
            INTO TABLE @DATA(it_pa0001)
            FOR ALL ENTRIES IN @it_pa0000
            WHERE pernr EQ @it_pa0000-pernr
              AND endda >= @sy-datum
              AND bukrs EQ @empresa_tgg.

        ENDIF.
      ENDIF.

      READ TABLE it_pa0001 INTO DATA(wa_pa0001) INDEX 1.

      APPEND
      VALUE #(
                nr_cpf       = cpf14
                anopr        = i_parametros-datamaxima(4)
                mespr        = i_parametros-datamaxima+4(2)
                cod_retorno  = w_value-codigoretorno
                msg_retorno  = w_value-mensagemretorno
                bukrs        = wa_pa0001-bukrs
                werks        = wa_pa0001-werks
                pernr        = wa_pa0001-pernr
                cname        = |{ w_value-nome CASE = UPPER }|
                perfil       = w_value-perfilacesso
                qtde_acessos = w_value-qtdeacesso
                user_name    = sy-uname
                data         = sy-datum
                hora         = sy-uzeit
             ) TO it_0025.

      CLEAR: wa_pa0001, cpf11.
      FREE: it_pa0001, it_pa0000, it_pernr.

    ENDLOOP.

    MODIFY zhcmt_py_0025 FROM TABLE it_0025.
    COMMIT WORK.


  ENDMETHOD.


  METHOD zif_integracao_iportsolution~get_id_referencia.
    r_if_integracao_iportsolution = me.
  ENDMETHOD.


  method ZIF_INTEGRACAO_IPORTSOLUTION~GET_INSTANCE.
  endmethod.


  METHOD zif_integracao_iportsolution~set_ausencia.

    DATA lv_text2 TYPE char100.
    DATA: empresa_tgg TYPE pa0001-bukrs VALUE '0048'.
    DATA lo_xml_ret TYPE REF TO cl_xml_document.

    CREATE OBJECT lo_xml_ret.

    DATA: t_set     TYPE TABLE OF rgsb4,
          data_fire TYPE sy-datum,
          r_subty   TYPE zrsdsselopts,
          wa_set    TYPE rgsb4.

    DATA: cpf14      TYPE char14.
    DATA: cpf11      TYPE char11.

*   "// Demissão
    SELECT pernr
      FROM pa0001
      INTO TABLE @DATA(it_pa0001)
        WHERE bukrs EQ @empresa_tgg
          AND endda >= @sy-datum
          AND abkrs NE 'BA'.

    IF sy-subrc IS INITIAL.

      SELECT pernr
        FROM pa0000
        INTO TABLE @DATA(it_pa0000)
        FOR ALL ENTRIES IN @it_pa0001
          WHERE pernr EQ @it_pa0001-pernr
            AND stat2 EQ '0'
            AND ( begda >= @sy-datum OR
                  aedtm EQ @sy-datum ).

      IF it_pa0000 IS NOT INITIAL.

        SELECT *
          FROM pa0465
          INTO TABLE @DATA(it_pa0465)
          FOR ALL ENTRIES IN @it_pa0000
            WHERE pernr EQ @it_pa0000-pernr
              AND subty EQ '0001'
              AND endda >= @sy-datum.

      ENDIF.

    ENDIF.

    LOOP AT it_pa0465 INTO DATA(wa_pa0465).

      CALL FUNCTION 'RP_GET_FIRE_DATE'
        EXPORTING
          persnr   = wa_pa0465-pernr
        IMPORTING
          firedate = data_fire.

      cpf14 = wa_pa0465-cpf_nr.
      REPLACE ALL OCCURRENCES OF '.' IN cpf14 WITH ''.
      REPLACE ALL OCCURRENCES OF '-' IN cpf14 WITH ''.
      cpf11 = cpf14.

      APPEND VALUE #(
                      _documento = cpf11
                      _restricao_requisito = '9999'
                      _data_inicio         = |{ data_fire(4) }/{ data_fire+4(2) }/{ data_fire+6(2) }|
                    ) TO me->zif_integracao_iportsolution~at_zhcme_py_0003.


    ENDLOOP.

*   "// Ausencia
    SELECT pernr
       FROM pa0001
       INTO TABLE @it_pa0001
         WHERE bukrs EQ '0048'
           AND endda >= @sy-datum
      AND plans NE '99999999'
           AND abkrs NE 'BA'.

    IF sy-subrc IS INITIAL.

      SELECT *
        FROM pa0465
        INTO TABLE @it_pa0465
        FOR ALL ENTRIES IN @it_pa0001
          WHERE pernr EQ @it_pa0001-pernr
            AND subty EQ '0001'
            AND endda >= @sy-datum.


      SELECT 'I'   AS valsign,
             'EQ'  AS valoption,
             valfrom AS low
        FROM setleaf
          INTO TABLE @r_subty
            WHERE setname = 'MAGGI_AUSENCIA_REF_TTG'.

      SELECT *
        FROM pa2001
        INTO TABLE @DATA(it_pa2001)
        FOR ALL ENTRIES IN @it_pa0001
          WHERE pernr EQ @it_pa0001-pernr
            AND subty IN @r_subty
            AND endda >= @sy-datum
            AND begda <= @sy-datum.

      SELECT *
        FROM pa2001
        APPENDING TABLE it_pa2001
        FOR ALL ENTRIES IN it_pa0001
          WHERE pernr EQ it_pa0001-pernr
            AND subty IN r_subty
            AND aedtm EQ sy-datum.

    ENDIF.

    SORT it_pa2001 BY pernr.
    DELETE ADJACENT DUPLICATES FROM it_pa2001 COMPARING pernr.
    LOOP AT it_pa2001 INTO DATA(wa_pa2001).

      READ TABLE it_pa0465 INTO wa_pa0465 WITH KEY pernr = wa_pa2001-pernr.

      cpf14 = wa_pa0465-cpf_nr.
      REPLACE ALL OCCURRENCES OF '.' IN cpf14 WITH ''.
      REPLACE ALL OCCURRENCES OF '-' IN cpf14 WITH ''.
      cpf11 = cpf14.

      APPEND VALUE #(
                      _documento           = cpf11
                      _restricao_requisito = wa_pa2001-subty
                      _data_inicio         = |{ wa_pa2001-begda(4) }/{ wa_pa2001-begda+4(2) }/{ wa_pa2001-begda+6(2) }|
                      _data_termino        = |{ wa_pa2001-endda(4) }/{ wa_pa2001-endda+4(2) }/{ wa_pa2001-endda+6(2) }|
                    ) TO me->zif_integracao_iportsolution~at_zhcme_py_0003.

    ENDLOOP.

    LOOP AT me->zif_integracao_iportsolution~at_zhcme_py_0003 INTO me->zif_integracao_iportsolution~al_zhcme_py_0003.

      me->zif_integracao_inject~at_referencia =
          VALUE #(
                   tp_referencia = 'Ausencia'
                   id_referencia = me->zif_integracao_iportsolution~al_zhcme_py_0003-_documento
                 ).

      me->zif_integracao_iportsolution~set_ds_url(
        )->set_ds_data(
        )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao) e_integracao  = DATA(e_integracao) ).

    ENDLOOP.


  ENDMETHOD.


  METHOD zif_integracao_iportsolution~set_ds_data.

    DATA: wa_zhcme_py_0005 TYPE zhcme_py_0005.

    MOVE-CORRESPONDING me->zif_integracao_iportsolution~al_zhcme_py_0003 TO wa_zhcme_py_0005.

    IF me->zif_integracao_iportsolution~al_zhcme_py_0003-_restricao_requisito EQ '9999'
    OR me->zif_integracao_iportsolution~al_zhcme_py_0003-_data_termino(4)     EQ '9999'.

      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data        = wa_zhcme_py_0005
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        RECEIVING
          r_json      = me->zif_integracao_inject~at_info_request_http-ds_body.

    ELSE.

      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data        = me->zif_integracao_iportsolution~al_zhcme_py_0003
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        RECEIVING
          r_json      = me->zif_integracao_inject~at_info_request_http-ds_body.

    ENDIF.

    me->zif_integracao_iportsolution~at_xml = me->zif_integracao_inject~at_info_request_http-ds_body.

    r_if_integracao_iportsolution = me.

  ENDMETHOD.


  METHOD zif_integracao_iportsolution~set_ds_url.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = me->zif_integracao_iportsolution~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url              = me->zif_integracao_iportsolution~at_auth_ws-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo           = me->zif_integracao_iportsolution~at_auth_ws-method.

    me->zif_integracao_iportsolution~set_id_referencia( ).

    r_if_integracao_iportsolution = me.

  ENDMETHOD.


  METHOD zif_integracao_iportsolution~set_id_referencia.

    r_if_integracao_iportsolution = me.
    me->zif_integracao_iportsolution~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_iportsolution~set_parametros.
    IF i_parametros IS NOT INITIAL.
      zif_integracao_iportsolution~at_parametros = i_parametros.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_iportsolution~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_iportsolution = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

  ENDMETHOD.


  METHOD zif_integracao_iportsolution~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_integracao_iportsolution~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD zif_integracao_iportsolution~set_xml.
    r_if_integracao_iportsolution = me.
  ENDMETHOD.
ENDCLASS.
