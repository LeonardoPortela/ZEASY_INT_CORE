*&---------------------------------------------------------------------*
*& Report ZINTEGRACAO_TRIBUTUM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zintegracao_tributum.

TABLES: j_1bbranch.

DATA: git_empresas TYPE zmms_response_tributum_001_t.
RANGES: gra_status_replace_edoc  FOR edobrgateprocst-process_status.

DATA: gva_total_empresas TYPE i.

DATA: lva_job   TYPE i,
      lva_jobnm TYPE btcjob,
      lva_stepc TYPE btcstepcnt.

CONSTANTS: c_zintegracao_tributum_nfe TYPE c LENGTH 100 VALUE 'ZINTEGRACAO_TRIBUTUM_NFE',
           c_zintegracao_tributum_cte TYPE c LENGTH 100 VALUE 'ZINTEGRACAO_TRIBUTUM_CTE'.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_cnpj   FOR j_1bbranch-stcd1.
  PARAMETERS: p_nfe  TYPE char01 AS CHECKBOX.
  PARAMETERS: p_cte  TYPE char01 AS CHECKBOX.

  PARAMETERS: p_nfe_ev  TYPE char01 AS CHECKBOX USER-COMMAND bbb."SMC 01-10-2025 - #192108
  PARAMETERS: p_cte_ev  TYPE char01 AS CHECKBOX.


  PARAMETERS: p_all  TYPE char01 AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b1.

*  Novos filtros condicionais - SMC 01-10-2025 - #192108
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_data FOR sy-datum NO-EXTENSION MODIF ID eee.
  PARAMETERS: tp_evto TYPE c LENGTH 10 MODIF ID eee.
SELECTION-SCREEN: END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'EEE'.
      IF p_nfe_ev = abap_false.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*  Novos filtros condicionais - SMC 01-10-2025 - #192108



START-OF-SELECTION.

  IF sy-batch EQ abap_true.
    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobname         = lva_jobnm
        stepcount       = lva_stepc
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.

    SELECT SINGLE COUNT(*) INTO lva_job
      FROM tbtco
     WHERE jobname EQ lva_jobnm
       AND status  EQ 'R'.

* "US #173081 - SMC - 15-09-2025
*    if lva_job ne 1 and sy-sysid = 'QAS'.
*      lva_job = '1'.
*     endif.
* "US #173081 - SMC - 15-09-2025

    IF lva_job NE 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  PERFORM f_get_stvarv.
  PERFORM f_get_empresas_tributum CHANGING git_empresas.

  CHECK git_empresas[] IS NOT INITIAL.



*-----------------------------------------------------------------------------*
*  "XML de NF-e e CT-e
*-----------------------------------------------------------------------------*

  IF p_nfe EQ abap_true OR p_all EQ abap_true.
    PERFORM: f_processar_nfe.
  ENDIF.

  IF p_cte EQ abap_true OR p_all EQ abap_true.
    PERFORM: f_processar_cte.
  ENDIF.

*-----------------------------------------------------------------------------*
*  XML de Eventos de NF-e e CT-e
*-----------------------------------------------------------------------------*

  IF p_nfe_ev EQ abap_true OR p_all EQ abap_true.
    PERFORM: f_processar_nfe_eventos.
  ENDIF.

  IF p_cte_ev EQ abap_true OR p_all EQ abap_true.
    PERFORM: f_processar_cte_eventos.
  ENDIF.


FORM f_get_empresas_tributum CHANGING t_empresas TYPE zmms_response_tributum_001_t.

  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  CLEAR: t_empresas.

  lwa_req_tributum-tp_requisicao = '01'. "Lista Empresas

  TRY.
      zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum
                                                                                           IMPORTING e_integracao = DATA(lwa_integracao) ).
      CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

      /ui2/cl_json=>deserialize( EXPORTING json = lwa_integracao-ds_data_retorno CHANGING data = t_empresas ).

      "DELETE t_empresas WHERE cnpj NE '77294254007792'. "Teste NF-e
      "DELETE t_empresas WHERE CNPJ NE '77294254004343'. "Teste CT-e

      DELETE t_empresas WHERE cnpj NOT IN s_cnpj.


    CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
      MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
    CATCH zcx_error INTO DATA(zcx_error).
      MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
  ENDTRY.


  gva_total_empresas = lines( t_empresas ).

ENDFORM.

FORM f_processar_nfe.

  DATA: lit_nfes TYPE zmms_response_tributum_002_t.

  PERFORM f_listar_nfes CHANGING lit_nfes.

  CHECK lit_nfes[] IS NOT INITIAL.

  PERFORM f_descartar_nfes_recebidas CHANGING lit_nfes.

  CHECK lit_nfes[] IS NOT INITIAL.

  PERFORM f_download_nfes CHANGING lit_nfes.

ENDFORM.


FORM f_listar_nfes CHANGING t_nfes TYPE zmms_response_tributum_002_t.

  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  DATA: lva_data_inicial TYPE erdat,
        lva_data_final   TYPE erdat.

  DATA: lit_nfes TYPE zmms_response_tributum_002_t.

  CLEAR: t_nfes.

  lva_data_inicial = sy-datum - 1.
  lva_data_final   = sy-datum + 1.

  LOOP AT git_empresas INTO DATA(lwa_empresa_tributum).

    DATA(_tabix) = sy-tabix.
    MESSAGE |Listando NF-es CNPJ: { lwa_empresa_tributum-cnpj }! Registro { _tabix }/{ gva_total_empresas }...| TYPE 'S'.

    CLEAR: lit_nfes.

    lwa_req_tributum-tp_requisicao           = '02'. "Lista NF-es
    lwa_req_tributum-lista_nfes-cnpj         = lwa_empresa_tributum-cnpj.
    lwa_req_tributum-lista_nfes-data_inicial = lva_data_inicial(4) && '-' &&  lva_data_inicial+4(2) && '-' && lva_data_inicial+6(2) && 'T00:00:00.000-04:00'.
    lwa_req_tributum-lista_nfes-data_final   = lva_data_final(4)   && '-' &&  lva_data_final+4(2)   && '-' && lva_data_final+6(2)   && 'T23:59:59.000-04:00'.
    lwa_req_tributum-lista_nfes-tipo_data    = 'chegada'.
    lwa_req_tributum-lista_nfes-tipo_nfe     = 'destinadas'.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum
                                                                                             IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lwa_integracao-ds_data_retorno CHANGING data = lit_nfes ).

        APPEND LINES OF lit_nfes TO t_nfes.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.

ENDFORM.

FORM f_descartar_nfes_recebidas CHANGING t_nfes TYPE zmms_response_tributum_002_t.

  CHECK t_nfes[] IS NOT INITIAL.

  SELECT accesskey, edoc_guid
    FROM edobrincoming INTO TABLE @DATA(lit_edobrincoming)
    FOR ALL ENTRIES IN @t_nfes
    WHERE accesskey EQ @t_nfes-infprot-chnfe.

  CHECK lit_edobrincoming[] IS NOT INITIAL.

  SELECT edoc_guid, proc_status, created_by
    FROM edocument INTO TABLE @DATA(lit_edocument)
    FOR ALL ENTRIES IN @lit_edobrincoming
    WHERE edoc_guid EQ @lit_edobrincoming-edoc_guid.

  DELETE lit_edocument WHERE proc_status IN gra_status_replace_edoc AND created_by NE 'JOBADM'.

  CHECK lit_edocument[] IS NOT INITIAL.

  SORT lit_edobrincoming BY accesskey.
  SORT lit_edocument BY edoc_guid.

  LOOP AT t_nfes ASSIGNING FIELD-SYMBOL(<fs_nfe>).
    READ TABLE lit_edobrincoming INTO DATA(lwa_edobrincoming) WITH KEY accesskey = <fs_nfe>-infprot-chnfe BINARY SEARCH .
    CHECK sy-subrc EQ 0.

    READ TABLE lit_edocument INTO DATA(lwa_edocument) WITH KEY edoc_guid = lwa_edobrincoming-edoc_guid BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    CLEAR: <fs_nfe>-infprot-chnfe.
  ENDLOOP.

  DELETE t_nfes WHERE infprot-chnfe IS INITIAL.

ENDFORM.


FORM f_download_nfes CHANGING t_nfes TYPE zmms_response_tributum_002_t.

  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  DATA(_total_registros) = lines( t_nfes ).

  LOOP AT t_nfes INTO DATA(lwa_nfe).

    DATA(_tabix) = sy-tabix.
    MESSAGE |Baixando NF-e: { lwa_nfe-infprot-chnfe }! Registro { _tabix }/{ _total_registros }...| TYPE 'S'.

    lwa_req_tributum-tp_requisicao                      = '03'. "Download do XML da NFe
    lwa_req_tributum-download_xml_nfe-codigo_download   = lwa_nfe-codigo_download.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        DATA(_xml_base64) = zcl_string=>string_to_base64( i_texto = lwa_integracao-ds_data_retorno ).

        "Criar EDocument
        CALL FUNCTION 'ZEDOC_BR_RECEIVE_XML'
          EXPORTING
            iv_xml         = _xml_base64
          EXCEPTIONS
            write_error    = 1
            log_save_error = 2
            OTHERS         = 3.

        IF sy-subrc NE 0.
          MESSAGE |Erro ao criar o Edocument do NF-e { lwa_nfe-infprot-chnfe }| TYPE 'I'.
        ENDIF.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.

ENDFORM.

FORM f_processar_cte.

  DATA: lit_ctes TYPE zmms_response_tributum_003_t.

  PERFORM f_listar_ctes CHANGING lit_ctes.

  CHECK lit_ctes[] IS NOT INITIAL.

  PERFORM f_descartar_ctes_recebidos CHANGING lit_ctes.

  CHECK lit_ctes[] IS NOT INITIAL.

  PERFORM f_download_ctes CHANGING lit_ctes.

ENDFORM.


FORM f_listar_ctes CHANGING t_ctes TYPE zmms_response_tributum_003_t.

  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  DATA: lva_data_inicial TYPE erdat,
        lva_data_final   TYPE erdat.

  DATA: lit_ctes TYPE zmms_response_tributum_003_t.

  CLEAR: t_ctes.

  lva_data_inicial = sy-datum - 1.
  lva_data_final   = sy-datum + 1.

  LOOP AT git_empresas INTO DATA(lwa_empresa_tributum).

    DATA(_tabix) = sy-tabix.
    MESSAGE |Listando CT-es CNPJ: { lwa_empresa_tributum-cnpj }! Registro { _tabix }/{ gva_total_empresas }...| TYPE 'S'.

    CLEAR: lit_ctes.

    lwa_req_tributum-tp_requisicao           = '07'. "Listar CTes
    lwa_req_tributum-lista_ctes-cnpj         = lwa_empresa_tributum-cnpj.
    lwa_req_tributum-lista_ctes-data_inicial = lva_data_inicial(4) && '-' &&  lva_data_inicial+4(2) && '-' && lva_data_inicial+6(2) && 'T00:00:00.000-04:00'.
    lwa_req_tributum-lista_ctes-data_final   = lva_data_final(4)   && '-' &&  lva_data_final+4(2)   && '-' && lva_data_final+6(2)   && 'T23:59:59.000-04:00'.
    lwa_req_tributum-lista_ctes-tipo_data    = 'chegada'.
    lwa_req_tributum-lista_ctes-tipo_cte     = 'tomados'.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum
                                                                                             IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lwa_integracao-ds_data_retorno CHANGING data = lit_ctes ).

        APPEND LINES OF lit_ctes TO t_ctes.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.

ENDFORM.

FORM f_descartar_ctes_recebidos CHANGING t_ctes TYPE zmms_response_tributum_003_t.

  CHECK t_ctes[] IS NOT INITIAL.

  SELECT accesskey, edoc_guid
    FROM edobrcteincoming INTO TABLE @DATA(lit_edobrcteincoming)
    FOR ALL ENTRIES IN @t_ctes
    WHERE accesskey EQ @t_ctes-infprot-chcte.

  CHECK lit_edobrcteincoming[] IS NOT INITIAL.

  SELECT edoc_guid, proc_status, created_by
    FROM edocument INTO TABLE @DATA(lit_edocument)
    FOR ALL ENTRIES IN @lit_edobrcteincoming
    WHERE edoc_guid EQ @lit_edobrcteincoming-edoc_guid.

  DELETE lit_edocument WHERE proc_status IN gra_status_replace_edoc AND created_by NE 'JOBADM'.

  CHECK lit_edocument[] IS NOT INITIAL.

  SORT lit_edobrcteincoming BY accesskey.
  SORT lit_edocument BY edoc_guid.

  LOOP AT t_ctes ASSIGNING FIELD-SYMBOL(<fs_cte>).
    READ TABLE lit_edobrcteincoming INTO DATA(lwa_edobrcteincoming) WITH KEY accesskey = <fs_cte>-infprot-chcte BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    READ TABLE lit_edocument INTO DATA(lwa_edocument) WITH KEY edoc_guid = lwa_edobrcteincoming-edoc_guid BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    CLEAR: <fs_cte>-infprot-chcte.
  ENDLOOP.

  DELETE t_ctes WHERE infprot-chcte IS INITIAL.

ENDFORM.


FORM f_download_ctes CHANGING t_ctes TYPE zmms_response_tributum_003_t.

  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  DATA(_total_registros) = lines( t_ctes ).

  LOOP AT t_ctes INTO DATA(lwa_cte).

    DATA(_tabix) = sy-tabix.
    MESSAGE |Baixando CT-e: { lwa_cte-infprot-chcte }! Registro { _tabix }/{ _total_registros }...| TYPE 'S'.

    lwa_req_tributum-tp_requisicao                      = '08'. "Download do XML da CTe
    lwa_req_tributum-download_xml_cte-codigo_download   = lwa_cte-codigo_download.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        DATA(_xml_base64) = zcl_string=>string_to_base64( i_texto = lwa_integracao-ds_data_retorno ).

        "Criar EDocument
        CALL FUNCTION 'ZEDOC_BR_RECEIVE_XML'
          EXPORTING
            iv_xml         = _xml_base64
          EXCEPTIONS
            write_error    = 1
            log_save_error = 2
            OTHERS         = 3.

        IF sy-subrc NE 0.
          MESSAGE |Erro ao criar o Edocument do CT-e { lwa_cte-infprot-chcte }| TYPE 'I'.
        ENDIF.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.

ENDFORM.


FORM f_processar_cte_eventos.

  DATA: lit_ctes_eventos TYPE zmms_response_tributum_005_t.

  PERFORM f_listar_ctes_eventos CHANGING lit_ctes_eventos.

  CHECK lit_ctes_eventos[] IS NOT INITIAL.

  PERFORM f_descartar_ctes_eve_recebidos CHANGING lit_ctes_eventos.

  CHECK lit_ctes_eventos[] IS NOT INITIAL.

  PERFORM f_download_ctes_eventos CHANGING lit_ctes_eventos.

ENDFORM.


FORM f_download_ctes_eventos CHANGING t_ctes_eventos TYPE zmms_response_tributum_005_t.

  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  DATA(_total_registros) = lines( t_ctes_eventos ).

  LOOP AT t_ctes_eventos INTO DATA(lwa_cte_evento).

    DATA(_tabix) = sy-tabix.
    MESSAGE |Baixando Eventos CT-e: { lwa_cte_evento-eventocte-infevento-chcte }! Registro { _tabix }/{ _total_registros }...| TYPE 'S'.

    lwa_req_tributum-tp_requisicao                            = '11'. "Download XML do Evento de CTe
    lwa_req_tributum-download_xml_evento_cte-codigo_download  = lwa_cte_evento-codigo_download.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        DATA(_xml_base64) = zcl_string=>string_to_base64( i_texto = lwa_integracao-ds_data_retorno ).

        "Criar EDocument
        CALL FUNCTION 'ZEDOC_BR_RECEIVE_XML'
          EXPORTING
            iv_xml         = _xml_base64
          EXCEPTIONS
            write_error    = 1
            log_save_error = 2
            OTHERS         = 3.

        IF sy-subrc NE 0.
          MESSAGE |Erro ao criar o Edocument do Evento da NF-e { lwa_cte_evento-eventocte-infevento-chcte }| TYPE 'I'.
        ENDIF.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.

ENDFORM.


FORM f_descartar_ctes_eve_recebidos CHANGING t_ctes_eventos TYPE zmms_response_tributum_005_t.

  DELETE t_ctes_eventos WHERE eventocte-infevento-tpevento NE '110111'. "Descartar eventos diferente de "Cancelamento"

  CHECK t_ctes_eventos[] IS NOT INITIAL.

  SELECT accesskey, edoc_guid
    FROM edobrcteincoming AS a INTO TABLE @DATA(lit_edobrincoming)
    FOR ALL ENTRIES IN @t_ctes_eventos
    WHERE accesskey EQ @t_ctes_eventos-eventocte-infevento-chcte
     AND EXISTS ( SELECT edoc_guid
                    FROM edocument AS b
                   WHERE b~edoc_guid = a~edoc_guid
                     AND b~edoc_type IN ( 'BR_INCTECP' )
                     AND b~created_by EQ 'JOBADM' ).

  CHECK lit_edobrincoming[] IS NOT INITIAL.

  SORT lit_edobrincoming BY accesskey.

  LOOP AT t_ctes_eventos ASSIGNING FIELD-SYMBOL(<fs_cte_evento>).
    READ TABLE lit_edobrincoming INTO DATA(lwa_edobrincoming) WITH KEY accesskey = <fs_cte_evento>-eventocte-infevento-chcte BINARY SEARCH .
    CHECK sy-subrc EQ 0.

    CLEAR: <fs_cte_evento>-eventocte-infevento-chcte.
  ENDLOOP.

  DELETE t_ctes_eventos WHERE eventocte-infevento-chcte IS INITIAL.

ENDFORM.


FORM f_listar_ctes_eventos CHANGING t_ctes_eventos TYPE zmms_response_tributum_005_t.

  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  DATA: lva_data_inicial TYPE erdat,
        lva_data_final   TYPE erdat.

  DATA: lit_nfes_eventos TYPE zmms_response_tributum_005_t.

  CLEAR: t_ctes_eventos.

  lva_data_inicial = sy-datum - 1.
  lva_data_final   = sy-datum + 1.

  LOOP AT git_empresas INTO DATA(lwa_empresa_tributum).

    DATA(_tabix) = sy-tabix.
    MESSAGE |Listando Eventos NF-es do CNPJ: { lwa_empresa_tributum-cnpj }! Registro { _tabix }/{ gva_total_empresas }...| TYPE 'S'.

    CLEAR: lit_nfes_eventos.

    lwa_req_tributum-tp_requisicao                   = '13'. "Lista CT-es Eventos
    lwa_req_tributum-lista_ctes_eventos-cnpj         = lwa_empresa_tributum-cnpj.
    lwa_req_tributum-lista_ctes_eventos-data_inicial = lva_data_inicial(4) && '-' &&  lva_data_inicial+4(2) && '-' && lva_data_inicial+6(2) && 'T00:00:00.000-04:00'.
    lwa_req_tributum-lista_ctes_eventos-data_final   = lva_data_final(4)   && '-' &&  lva_data_final+4(2)   && '-' && lva_data_final+6(2)   && 'T23:59:59.000-04:00'.
    lwa_req_tributum-lista_ctes_eventos-tipo_data    = 'chegada'.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum
                                                                                             IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lwa_integracao-ds_data_retorno CHANGING data = lit_nfes_eventos ).

        APPEND LINES OF lit_nfes_eventos TO t_ctes_eventos.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.

ENDFORM.

FORM f_processar_nfe_eventos.

  DATA: lit_nfes_eventos TYPE zmms_response_tributum_004_t.

  PERFORM f_listar_nfes_eventos CHANGING lit_nfes_eventos.

  CHECK lit_nfes_eventos[] IS NOT INITIAL.

  PERFORM f_descartar_nfes_eve_recebidos CHANGING lit_nfes_eventos.

  CHECK lit_nfes_eventos[] IS NOT INITIAL.

  PERFORM f_download_nfes_eventos CHANGING lit_nfes_eventos.

ENDFORM.


FORM f_download_nfes_eventos CHANGING t_nfes_eventos TYPE zmms_response_tributum_004_t.

  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  DATA(_total_registros) = lines( t_nfes_eventos ).

  LOOP AT t_nfes_eventos INTO DATA(lwa_nfe_evento).

    DATA(_tabix) = sy-tabix.
    MESSAGE |Baixando Eventos NF-e: { lwa_nfe_evento-evento-infevento-chnfe }! Registro { _tabix }/{ _total_registros }...| TYPE 'S'.

    lwa_req_tributum-tp_requisicao                            = '06'. "Download XML do Evento de NFe
    lwa_req_tributum-download_xml_evento_nfe-codigo_download  = lwa_nfe_evento-codigo_download.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        DATA(_xml_base64) = zcl_string=>string_to_base64( i_texto = lwa_integracao-ds_data_retorno ).

        "Criar EDocument
        CALL FUNCTION 'ZEDOC_BR_RECEIVE_XML'
          EXPORTING
            iv_xml         = _xml_base64
          EXCEPTIONS
            write_error    = 1
            log_save_error = 2
            OTHERS         = 3.

        IF sy-subrc NE 0.
          MESSAGE |Erro ao criar o Edocument do Evento da NF-e { lwa_nfe_evento-evento-infevento-chnfe }| TYPE 'I'.
        ENDIF.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.

ENDFORM.


FORM f_descartar_nfes_eve_recebidos CHANGING t_nfes_eventos TYPE zmms_response_tributum_004_t.


*** US #173081 - MMSILVA - 03.07.2025 - Ini ***
  DATA: lva_event_sequence TYPE edobrevent-event_sequence. "

  " Inicio Comentário - US #173081
*  DELETE t_nfes_eventos WHERE evento-infevento-tpevento NE '110111'. "Descartar eventos diferente de "Cancelamento"
  " Fim Comentário - US #173081

  DATA: lt_event_type TYPE RANGE OF edobrevtdesc-event_type.
*  Novos filtros condicionais - SMC 01-10-2025 - #192108
  DATA: it_edobrevtdesc TYPE TABLE OF edobrevtdesc.

  IF tp_evto IS NOT INITIAL.
    " Se filtro de tipo evento foi preenchido
    SELECT * FROM edobrevtdesc
         INTO TABLE @it_edobrevtdesc
          WHERE event_type = @tp_evto.
  ELSE.
    " Se não foi preenchido, usar lógica atual
    SELECT * FROM edobrevtdesc INTO TABLE @it_edobrevtdesc.
  ENDIF.

  IF it_edobrevtdesc[] IS INITIAL AND tp_evto IS NOT INITIAL.
    " Tipo de evento não encontrado na tabela
    MESSAGE 'Tipo de evento não encontrado' TYPE 'W'.
    RETURN.
  ENDIF.

*  select event_type from edobrevtdesc into table @data(it_edobrevtdesc).

*  Novos filtros condicionais - SMC 01-10-2025 - #192108

  LOOP AT it_edobrevtdesc ASSIGNING FIELD-SYMBOL(<fs_event>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_event>-event_type ) TO lt_event_type.
  ENDLOOP.

  DELETE t_nfes_eventos WHERE evento-infevento-tpevento NOT IN lt_event_type.
*** US #173081 - MMSILVA - 03.07.2025 - Fim ***

  CHECK t_nfes_eventos[] IS NOT INITIAL.

  SELECT accesskey, edoc_guid
    FROM edobrincoming AS a INTO TABLE @DATA(lit_edobrincoming_cancel)
    FOR ALL ENTRIES IN @t_nfes_eventos
    WHERE accesskey EQ @t_nfes_eventos-evento-infevento-chnfe
     AND EXISTS ( SELECT edoc_guid
                    FROM edocument AS b
                   WHERE b~edoc_guid = a~edoc_guid
                     AND b~edoc_type IN ( 'BR_IN_CP' )
                     AND b~created_by EQ 'JOBADM'
                     ).

*** US #173081 - MMSILVA - 03.07.2025 - Ini ***
  SELECT accesskey, edoc_guid, event_sequence
    FROM edobrevent AS c
    INTO TABLE @DATA(lit_edobrincoming_eventos)
    FOR ALL ENTRIES IN @t_nfes_eventos
    WHERE c~accesskey = @t_nfes_eventos-evento-infevento-chnfe.

  LOOP AT lit_edobrincoming_eventos ASSIGNING FIELD-SYMBOL(<fs_edobrevent>).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_edobrevent>-event_sequence
      IMPORTING
        output = <fs_edobrevent>-event_sequence.
  ENDLOOP.
*** US #173081 - MMSILVA - 03.07.2025 - Fim ***

  CHECK lit_edobrincoming_cancel[] IS NOT INITIAL OR lit_edobrincoming_eventos[] IS NOT INITIAL . " US #173081 - MMSILVA - 03.07.2025

  SORT: lit_edobrincoming_cancel  BY accesskey,
        lit_edobrincoming_eventos BY accesskey event_sequence.  "US #173081 - MMSILVA - 03.07.2025 - Ini ***

  LOOP AT t_nfes_eventos ASSIGNING FIELD-SYMBOL(<fs_nfe>).

    CASE <fs_nfe>-evento-infevento-tpevento.
      WHEN '110111'. "Cancelamento

        READ TABLE lit_edobrincoming_cancel INTO DATA(lwa_edobrincoming) WITH KEY accesskey = <fs_nfe>-evento-infevento-chnfe BINARY SEARCH .
        IF sy-subrc EQ 0.
          CLEAR: <fs_nfe>-evento-infevento-chnfe.
        ENDIF.

      WHEN OTHERS.

        lva_event_sequence  = <fs_nfe>-evento-infevento-nseqevento.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lva_event_sequence
          IMPORTING
            output = lva_event_sequence.

        READ TABLE lit_edobrincoming_eventos INTO DATA(lwa_edobrevent) WITH KEY accesskey      = <fs_nfe>-evento-infevento-chnfe
                                                                                event_sequence = lva_event_sequence   BINARY SEARCH .
        IF sy-subrc EQ 0.
          CLEAR: <fs_nfe>-evento-infevento-chnfe.
        ENDIF.

    ENDCASE.

  ENDLOOP.

  DELETE t_nfes_eventos WHERE evento-infevento-chnfe IS INITIAL.

ENDFORM.

FORM f_listar_nfes_eventos CHANGING t_nfes_eventos TYPE zmms_response_tributum_004_t.



  DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

  DATA: lva_data_inicial TYPE erdat,
        lva_data_final   TYPE erdat.

  DATA: lit_nfes_eventos TYPE zmms_response_tributum_004_t.

  CLEAR: t_nfes_eventos.

*  Novos filtros condicionais - SMC 01-10-2025 - #192108
*  lva_data_inicial = sy-datum - 1.
*  lva_data_final   = sy-datum + 1.

  IF s_data[] IS NOT INITIAL.
    " Se filtro de data foi preenchido
    lva_data_inicial = s_data-low.
    lva_data_final = s_data-high.

    " Se só data inicial foi preenchida, usar a mesma data para ambas
    IF s_data-high IS INITIAL.
      lva_data_final = s_data-low.
    ENDIF.
  ELSE.
    " Se não foi preenchido, usar lógica atual
    lva_data_inicial = sy-datum - 1.
    lva_data_final = sy-datum + 1.
  ENDIF.
*  Novos filtros condicionais - SMC 01-10-2025 - #192108


  LOOP AT git_empresas INTO DATA(lwa_empresa_tributum).

    DATA(_tabix) = sy-tabix.
    MESSAGE |Listando Eventos NF-es do CNPJ: { lwa_empresa_tributum-cnpj }! Registro { _tabix }/{ gva_total_empresas }...| TYPE 'S'.

    CLEAR: lit_nfes_eventos.

    lwa_req_tributum-tp_requisicao                   = '12'. "Lista NF-es Eventos

    lwa_req_tributum-lista_nfes_eventos-cnpj         = lwa_empresa_tributum-cnpj.
    lwa_req_tributum-lista_nfes_eventos-data_inicial = lva_data_inicial(4) && '-' &&  lva_data_inicial+4(2) && '-' && lva_data_inicial+6(2) && 'T00:00:00.000-04:00'.
    lwa_req_tributum-lista_nfes_eventos-data_final   = lva_data_final(4)   && '-' &&  lva_data_final+4(2)   && '-' && lva_data_final+6(2)   && 'T23:59:59.000-04:00'.
    lwa_req_tributum-lista_nfes_eventos-tipo_data    = 'chegada'.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum
                                                                                             IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lwa_integracao-ds_data_retorno CHANGING data = lit_nfes_eventos ).

        APPEND LINES OF lit_nfes_eventos TO t_nfes_eventos.

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
    ENDTRY.

  ENDLOOP.

ENDFORM.


FORM f_set_xml USING p_tipo CHANGING p_xml.

  CLEAR: p_xml.

  CASE p_tipo.
    WHEN '01'.
      p_xml = p_xml && '['.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457000195",'.
      p_xml = p_xml && '"codigo": "414407a83554a4c36c3a9f57205fb7d8",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:02.997032+00:00",'.
      p_xml = p_xml && '"razao": "1501-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457000276",'.
      p_xml = p_xml && '"codigo": "6c882afbb5e040cba60fb4b776d96edb",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:02.998922+00:00",'.
      p_xml = p_xml && '"razao": "1502-SM-03"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457000357",'.
      p_xml = p_xml && '"codigo": "cfdd82ca7106eb9347d153073c98c6c1",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.000631+00:00",'.
      p_xml = p_xml && '"razao": "1503-SM-05"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457000780",'.
      p_xml = p_xml && '"codigo": "efc8f59039928225a1686b5dc97e5a09",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.002397+00:00",'.
      p_xml = p_xml && '"razao": "1507-TUCUNARÉ 01"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457000861",'.
      p_xml = p_xml && '"codigo": "14c50dc420331206eb869e53bc88b3a4",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.004161+00:00",'.
      p_xml = p_xml && '"razao": "1508-TUCUNARÉ 02"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457000942",'.
      p_xml = p_xml && '"codigo": "1e702d1e376c580a274c6fb74864d2b7",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.005859+00:00",'.
      p_xml = p_xml && '"razao": "1509-SM-01"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001086",'.
      p_xml = p_xml && '"codigo": "3023b025ecd4186f7ae1c10288cf1b3f",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.007617+00:00",'.
      p_xml = p_xml && '"razao": "1510-TRANSPORTES"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001167",'.
      p_xml = p_xml && '"codigo": "23a8b7ec7ba42c67db9f8abc599b9964",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.009288+00:00",'.
      p_xml = p_xml && '"razao": "1511-SM-02"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001248",'.
      p_xml = p_xml && '"codigo": "4ac83ec37edb74ecf0cda688bd96fc85",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.010996+00:00",'.
      p_xml = p_xml && '"razao": "1512-FAZ. PONTE PEDRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001329",'.
      p_xml = p_xml && '"codigo": "4406e98f07ebe0ebfe32f11c3f60a6e4",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.012696+00:00",'.
      p_xml = p_xml && '"razao": "1513-FAZ. UIRAPURU"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001400",'.
      p_xml = p_xml && '"codigo": "6e1d2bea15541efa2e49142962a34358",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.014404+00:00",'.
      p_xml = p_xml && '"razao": "1514-SM-04"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001590",'.
      p_xml = p_xml && '"codigo": "3a9dbe5118b962dc9504297d0c1280e6",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.016107+00:00",'.
      p_xml = p_xml && '"razao": "1515-PORTO VELHO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001671",'.
      p_xml = p_xml && '"codigo": "04aa149960be0bdabaa9a087019e2713",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:02.995153+00:00",'.
      p_xml = p_xml && '"razao": "1516-SM-06"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001752",'.
      p_xml = p_xml && '"codigo": "4c777f63327b4665984717ec7e7be092",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.018037+00:00",'.
      p_xml = p_xml && '"razao": "1517-FAZ. SM3 B"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001833",'.
      p_xml = p_xml && '"codigo": "73062411e1745c809447e9b88c2680df",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.019273+00:00",'.
      p_xml = p_xml && '"razao": "1518-SEMENTEIRO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457001914",'.
      p_xml = p_xml && '"codigo": "eee2fbeffadd98cdc2d46f720b853d98",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.020401+00:00",'.
      p_xml = p_xml && '"razao": "1519-FAZ. TANGURO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457002058",'.
      p_xml = p_xml && '"codigo": "af6d42dcc58149b892d5671812c1e374",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.021506+00:00",'.
      p_xml = p_xml && '"razao": "1520-FAZ. ÁGUA QUENTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457002139",'.
      p_xml = p_xml && '"codigo": "2fd11b077aa5ea5d11355b36ae324716",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.022695+00:00",'.
      p_xml = p_xml && '"razao": "1521-FAZ. ITAMARATI"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457002210",'.
      p_xml = p_xml && '"codigo": "46176a9c84a15eee0ff2d2429532bbb6",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.023559+00:00",'.
      p_xml = p_xml && '"razao": "1522-FAZ. VALE DO ARAGUAIA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457002481",'.
      p_xml = p_xml && '"codigo": "fe433719f652eff6e571eba9ff519d2a",'.
      p_xml = p_xml && '"data_criacao": "2022-08-02T20:15:21.287663+00:00",'.
      p_xml = p_xml && '"razao": "AGROPECUARIA MAGGI LTDA - CORUMBIARA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457002562",'.
      p_xml = p_xml && '"codigo": "1d42bb7f4d723deabe3710b706e357c7",'.
      p_xml = p_xml && '"data_criacao": "2022-09-02T12:15:42.319551+00:00",'.
      p_xml = p_xml && '"razao": "AGROPECUARIA MAGGI LTDA - SAPEZAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457002643",'.
      p_xml = p_xml && '"codigo": "0a4031372345f2a2927d75661d7a2a49",'.
      p_xml = p_xml && '"data_criacao": "2022-12-21T11:16:50.581960+00:00",'.
      p_xml = p_xml && '"razao": "1526"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "00315457002724",'.
      p_xml = p_xml && '"codigo": "1a687e680b6d3b5928dc2925abb28ce6",'.
      p_xml = p_xml && '"data_criacao": "2022-12-20T17:44:42.372697+00:00",'.
      p_xml = p_xml && '"razao": "1527"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "03908754000132",'.
      p_xml = p_xml && '"codigo": "9161d4d065433ac062959d2c2f53f288",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.024397+00:00",'.
      p_xml = p_xml && '"razao": "2201-RONDONOPOLIS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "03908754000302",'.
      p_xml = p_xml && '"codigo": "02a75f2a7cfc76f6e7cf647228f8f7dd",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.025233+00:00",'.
      p_xml = p_xml && '"razao": "2203-ITACOATIARA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "03908754000485",'.
      p_xml = p_xml && '"codigo": "a3001e02d4b3b1c6a87b37e3d4dc79ae",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.026086+00:00",'.
      p_xml = p_xml && '"razao": "2204-SAPEZAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "03908754000566",'.
      p_xml = p_xml && '"codigo": "477e88ada1f673020ffdfdd2efb640de",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.026958+00:00",'.
      p_xml = p_xml && '"razao": "2205-CUIABA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "03908754000647",'.
      p_xml = p_xml && '"codigo": "6c929a8c7c44905db4467aabdc17637b",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.027786+00:00",'.
      p_xml = p_xml && '"razao": "2206-SOROCABA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "03908754000728",'.
      p_xml = p_xml && '"codigo": "f5f082c67b5262605b5bfda788d85f1a",'.
      p_xml = p_xml && '"data_criacao": "2021-01-07T11:39:14.061338+00:00",'.
      p_xml = p_xml && '"razao": "MAGGI ENERGIA S A"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "03908754000809",'.
      p_xml = p_xml && '"codigo": "174593c1cc57449eb58da0be70529431",'.
      p_xml = p_xml && '"data_criacao": "2021-01-07T11:39:14.061338+00:00",'.
      p_xml = p_xml && '"razao": "MAGGI ENERGIA S A"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05527694000133",'.
      p_xml = p_xml && '"codigo": "8d37847d6e8fe0349c623032c283ea0e",'.
      p_xml = p_xml && '"data_criacao": "2021-09-01T19:30:45.515234+00:00",'.
      p_xml = p_xml && '"razao": "TERMINAL DE GRANEIS DO GUARUJA S A"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277000180",'.
      p_xml = p_xml && '"codigo": "f6e6b4f11674b526acf3023872335098",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5001-TELHAR - MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277000503",'.
      p_xml = p_xml && '"codigo": "9ca1850a96b678778768f2b7dd60d479",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5005-TELHAR -FAZENDA PRIMAVERA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277000694",'.
      p_xml = p_xml && '"codigo": "21b38cee447c98dd42ab04edfba47bdb",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5006-TELHAR -FAZENDA NOVA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277000937",'.
      p_xml = p_xml && '"codigo": "233e7228a7f2e762b428bbdc9e720ef2",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5009-TELHAR -FAZENDA RIO BONITO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277001070",'.
      p_xml = p_xml && '"codigo": "c77f319725f0bfc5658db870a9b3e8ea",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5010-TELHAR -FAZENDA SETE LAGOAS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277001402",'.
      p_xml = p_xml && '"codigo": "79ee5eb838495d1cf548334040e32621",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5014-TELHAR -FAZENDA PIRAPO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277003103",'.
      p_xml = p_xml && '"codigo": "78d1fbcfb268bb300573c0bcb4bb8287",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5031-TELHAR -COMERCIAL EXPORTADORA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277003367",'.
      p_xml = p_xml && '"codigo": "101d01ddf49fe15fc93e8a3a35159d92",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5033-TELHAR -FAZENDA LEONARDO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277003600",'.
      p_xml = p_xml && '"codigo": "a54ca0b9d64a595be58fc87fd2182aa9",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5036-TELHAR -FAZENDA PRIMAVERA - ARMAZENS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277003790",'.
      p_xml = p_xml && '"codigo": "9bc4940d8c908c2670b4266cc2bdeaad",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5037-TELHAR -FAZENDA PIRAPÓ - ARMAZENS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277003952",'.
      p_xml = p_xml && '"codigo": "168302f10256a991ddc28f0ee0906462",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5039-TELHAR -FAZENDA AGROPECUÁRIA RIO VERDE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277004096",'.
      p_xml = p_xml && '"codigo": "40fc78c1f90961157f11315a6a00a872",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5040-TELHAR -FAZENDA INDEPENDÊNCIA - ARMAZENS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277004177",'.
      p_xml = p_xml && '"codigo": "195111c93c2579b3602e81505ca0d9d0",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5041-TELHAR -FAZENDA PRIMAVERA - ALGODOEIRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277004339",'.
      p_xml = p_xml && '"codigo": "a8b20768a76a9e5d55301bfcabfd98ff",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5043-TELHAR -FAZENDA PASCOAL/GUENO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277004410",'.
      p_xml = p_xml && '"codigo": "1b3de63b47f4ed5c1feaf2ad4da362e2",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5044-TELHAR -FAZENDA RIO VERDE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277004509",'.
      p_xml = p_xml && '"codigo": "decda43c5af152f9a0846fb31ddf94e8",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5045-TELHAR -FAZENDA HORIZONTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277004681",'.
      p_xml = p_xml && '"codigo": "1ea65a964285f45929aef4fec3cb2860",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5046-TELHAR -FAZENDA AGROPECUARIA FLOR DO CAMPO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277004843",'.
      p_xml = p_xml && '"codigo": "0638e80ea1bdfe7bc1411d5d29dfc0ad",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5048-TELHAR -FAZENDA INDEPENDENCIA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277004924",'.
      p_xml = p_xml && '"codigo": "242d7cf36c0e07505866d3cc35ab8cc0",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5049-TELHAR -FAZENDA ANGENITA I"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277005068",'.
      p_xml = p_xml && '"codigo": "03c181469fc20af78f26ae7118dcc5d3",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5050-TELHAR -FAZENDA NOVA - ARMAZENS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277005149",'.
      p_xml = p_xml && '"codigo": "d165bd09ec5ca20069746461dc6e20a8",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5051-TELHAR -FAZENDA CORRENTAO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277005220",'.
      p_xml = p_xml && '"codigo": "c70714858833eea0314bca8251016774",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "0152-TELHAR -FAZENDA PASSO FUNDO - ARMAZENS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277005300",'.
      p_xml = p_xml && '"codigo": "39aba2de24e8d3128c11852adfc7f67b",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5053-TELHAR -FAZENDA INDEPENDENCIA - ALGODOEIRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277005491",'.
      p_xml = p_xml && '"codigo": "3143e10731ab604a54147a30a25ec7ee",'.
      p_xml = p_xml && '"data_criacao": "2021-05-19T17:40:58.131349+00:00",'.
      p_xml = p_xml && '"razao": "5054-TELHAR -FAZENDA PIRAPÓ - ALGODOEIRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277005572",'.
      p_xml = p_xml && '"codigo": "ff1cf800afb83689bd5d006535e2f441",'.
      p_xml = p_xml && '"data_criacao": "2022-08-02T20:15:33.551006+00:00",'.
      p_xml = p_xml && '"razao": "O TELHAR AGROPECUARIA LTDA  - SANTO ANTONIO DO LESTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "05683277005653",'.
      p_xml = p_xml && '"codigo": "30abc4ad4b94b6c7179184203bc081bb",'.
      p_xml = p_xml && '"data_criacao": "2022-08-12T20:26:07.355568+00:00",'.
      p_xml = p_xml && '"razao": "TELHAR - NOVO SAO JOAQUIM"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "08918031000138",'.
      p_xml = p_xml && '"codigo": "f84e9c5afd99c287c072c6ec545c956e",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.028456+00:00",'.
      p_xml = p_xml && '"razao": "2401-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "08918031000219",'.
      p_xml = p_xml && '"codigo": "81e9ea47e076de1c960f1c74fbd59cd7",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.029129+00:00",'.
      p_xml = p_xml && '"razao": "2402-SAPEZAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "08936794000101",'.
      p_xml = p_xml && '"codigo": "3250596010b1e76ef9eb97d8404b5a7e",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.029805+00:00",'.
      p_xml = p_xml && '"razao": "2501-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "08936794000292",'.
      p_xml = p_xml && '"codigo": "4c0d715c81178e837b0eff161d2dee8b",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.030490+00:00",'.
      p_xml = p_xml && '"razao": "2502-SAPEZAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "08936816000133",'.
      p_xml = p_xml && '"codigo": "b1b8d73a73cd3fb4ab6d2bd749ee13ba",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.031158+00:00",'.
      p_xml = p_xml && '"razao": "2601-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "08936816000214",'.
      p_xml = p_xml && '"codigo": "0e49c7317a8d4be86120d2b362227450",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.031903+00:00",'.
      p_xml = p_xml && '"razao": "2602-SAPEZAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "09137338000164",'.
      p_xml = p_xml && '"codigo": "5f3d29a473eabe3bd2c549d4230df233",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.032652+00:00",'.
      p_xml = p_xml && '"razao": "3601-RIO MADEIRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10431501000186",'.
      p_xml = p_xml && '"codigo": "4e7cc34fdae16aa21d84bd3dd26f89db",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.033261+00:00",'.
      p_xml = p_xml && '"razao": "2701-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10431501000267",'.
      p_xml = p_xml && '"codigo": "3c09429a6cd7ff8624a397f24cdb3971",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.033856+00:00",'.
      p_xml = p_xml && '"razao": "2702-SAPEZAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000135",'.
      p_xml = p_xml && '"codigo": "5260139413a57289f039ca6b86c3baec",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.035549+00:00",'.
      p_xml = p_xml && '"razao": "3501-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000216",'.
      p_xml = p_xml && '"codigo": "c984692f6de05dd9f0468fd8a5d4f450",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.036172+00:00",'.
      p_xml = p_xml && '"razao": "3502-CORRENTINA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000305",'.
      p_xml = p_xml && '"codigo": "e1e5e307211a9ce0f9e5274fb737c5d0",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.036735+00:00",'.
      p_xml = p_xml && '"razao": "3503-BALSAS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000488",'.
      p_xml = p_xml && '"codigo": "a079f568033d70f6f2a8cad950e816df",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.037316+00:00",'.
      p_xml = p_xml && '"razao": "3504-GUARAI"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000569",'.
      p_xml = p_xml && '"codigo": "062132303ad11b520fdb9063997bc3d9",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.037854+00:00",'.
      p_xml = p_xml && '"razao": "3505-LEM - ESCRITÓRIO CENTRAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000640",'.
      p_xml = p_xml && '"codigo": "5ca0545b08036df70cfe6dff1a92794c",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.038376+00:00",'.
      p_xml = p_xml && '"razao": "3506-PIAUI"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000720",'.
      p_xml = p_xml && '"codigo": "ce7e784ceced7e7fb64bd8485cda7b19",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.038871+00:00",'.
      p_xml = p_xml && '"razao": "3507-COACERAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000801",'.
      p_xml = p_xml && '"codigo": "7bc169424ed68b34fa1ae9bd4f4b0f1f",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.039350+00:00",'.
      p_xml = p_xml && '"razao": "3508-PDG"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697000992",'.
      p_xml = p_xml && '"codigo": "6864e025f467ce4f1c520c0c01b13167",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.039835+00:00",'.
      p_xml = p_xml && '"razao": "3509-PALMAS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697001026",'.
      p_xml = p_xml && '"codigo": "2b60dc367f48f9c74c4ba9c18ddf5a87",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.040335+00:00",'.
      p_xml = p_xml && '"razao": "3510-URUÇUÍ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697001107",'.
      p_xml = p_xml && '"codigo": "83f659a4b54e5aef9cb8533116f2482e",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.040816+00:00",'.
      p_xml = p_xml && '"razao": "3511-FIGUEIROPOLIS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697001379",'.
      p_xml = p_xml && '"codigo": "2c17eb6f4a0c53127304f9d9ab772d59",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.034430+00:00",'.
      p_xml = p_xml && '"razao": "3513-TRANSPORTES LEM"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697001450",'.
      p_xml = p_xml && '"codigo": "bad3951d9f5689833db44509865cb2ee",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.034986+00:00",'.
      p_xml = p_xml && '"razao": "3514-TRANSPORTES BALSAS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697001530",'.
      p_xml = p_xml && '"codigo": "a72624c0f126ed0abcffa39be3cf1b19",'.
      p_xml = p_xml && '"data_criacao": "2022-10-24T14:57:20.575439+00:00",'.
      p_xml = p_xml && '"razao": "3515 - SAO LUIS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697001611",'.
      p_xml = p_xml && '"codigo": "c46092b03054343e790ab8969110d256",'.
      p_xml = p_xml && '"data_criacao": "2022-10-24T14:57:31.721459+00:00",'.
      p_xml = p_xml && '"razao": "3516 – LUIS EDUARDO MAGALHAES"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697001700",'.
      p_xml = p_xml && '"codigo": "8afcecb7a2e2ef9cf1e5f7a203e0b3b0",'.
      p_xml = p_xml && '"data_criacao": "2022-10-24T14:57:42.190300+00:00",'.
      p_xml = p_xml && '"razao": "3517 – PORTO NACIONAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "10962697001883",'.
      p_xml = p_xml && '"codigo": "1d26d70848b59f107ad74aa19d8b71c8",'.
      p_xml = p_xml && '"data_criacao": "2023-06-13T14:44:15.752024+00:00",'.
      p_xml = p_xml && '"razao": "AMAGGI LOUIS DREYFUS ZEN-NOH GRAOS S A  FILIAL 3518"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "11338257000174",'.
      p_xml = p_xml && '"codigo": "b71096450aab3a6ff8062620d1563ea3",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.041396+00:00",'.
      p_xml = p_xml && '"razao": "3901-BARCARENA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "11338257000255",'.
      p_xml = p_xml && '"codigo": "66f370fdf9557eff3c998c9bfd6cc7df",'.
      p_xml = p_xml && '"data_criacao": "2022-08-17T01:00:47.164489+00:00",'.
      p_xml = p_xml && '"razao": "NAVEGACOES UNIDAS TAPAJOS S/A"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "11338257000417",'.
      p_xml = p_xml && '"codigo": "07ba73034bd015f893da2c4e38e4a998",'.
      p_xml = p_xml && '"data_criacao": "2022-08-17T01:00:47.164489+00:00",'.
      p_xml = p_xml && '"razao": "NAVEGACOES UNIDAS TAPAJOS S/A"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "15143827000121",'.
      p_xml = p_xml && '"codigo": "a94927cee84bc1dd8e97e771007e686b",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.041884+00:00",'.
      p_xml = p_xml && '"razao": "3801-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "15143827000202",'.
      p_xml = p_xml && '"codigo": "cd481eab864661259f6e249fdbba3c46",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.042381+00:00",'.
      p_xml = p_xml && '"razao": "3802-SÃO LUIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "16587133000146",'.
      p_xml = p_xml && '"codigo": "4bb749bc6ede057d49ff51796377a90f",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.042830+00:00",'.
      p_xml = p_xml && '"razao": "2801-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "23771214000167",'.
      p_xml = p_xml && '"codigo": "1c6853905c74a1451be2d2b77cba0fa4",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.043265+00:00",'.
      p_xml = p_xml && '"razao": "4101-BARCARENA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "23771214000248",'.
      p_xml = p_xml && '"codigo": "58cc441a399cea1f588a99e132f6c39d",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.043689+00:00",'.
      p_xml = p_xml && '"razao": "4102-ITAITUBA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "32101789000163",'.
      p_xml = p_xml && '"codigo": "4ea3169ab02d3df43997711f2b7939d0",'.
      p_xml = p_xml && '"data_criacao": "2021-09-01T19:31:02.487829+00:00",'.
      p_xml = p_xml && '"razao": "CRISTALINA ENERGIA LTDA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "45946418000124",'.
      p_xml = p_xml && '"codigo": "39def39d0ca63ce5f422aefb7f7aac7e",'.
      p_xml = p_xml && '"data_criacao": "2022-10-21T13:07:49.143703+00:00",'.
      p_xml = p_xml && '"razao": "Tropical Farms SA -"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "51573237001506",'.
      p_xml = p_xml && '"codigo": "1d9e67a4302e5b1fe39ee903ae4afe20",'.
      p_xml = p_xml && '"data_criacao": "2023-09-18T14:08:53.192437+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1315 – Sinop"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254000194",'.
      p_xml = p_xml && '"codigo": "63b71c2e2642980f795db742cb1a7697",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.044938+00:00",'.
      p_xml = p_xml && '"razao": "0101-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254000275",'.
      p_xml = p_xml && '"codigo": "a70b35a0c28697fd36931dad8845730b",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.045349+00:00",'.
      p_xml = p_xml && '"razao": "0102-IPIRANGA DO NORTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254000356",'.
      p_xml = p_xml && '"codigo": "589265126485d5d3acdcb4a696074519",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.045769+00:00",'.
      p_xml = p_xml && '"razao": "0103-MARINGÁ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254000437",'.
      p_xml = p_xml && '"codigo": "2e14cd5aaa1b44383c759514e41ee4b5",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.046183+00:00",'.
      p_xml = p_xml && '"razao": "0104-ITIQUIRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254000780",'.
      p_xml = p_xml && '"codigo": "2dabdfef47bdd24c334ca5b57dfa5035",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.046598+00:00",'.
      p_xml = p_xml && '"razao": "0107-CAMPO NOVO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001085",'.
      p_xml = p_xml && '"codigo": "dee86d85a76704166b8fd41ca3a0db3a",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.047009+00:00",'.
      p_xml = p_xml && '"razao": "0110-CAMPOS DE JÚLIO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001166",'.
      p_xml = p_xml && '"codigo": "5d3aed981109d8aad35a2c35a3bb3498",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.047434+00:00",'.
      p_xml = p_xml && '"razao": "0111-SAPEZAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001247",'.
      p_xml = p_xml && '"codigo": "244295ddd7552bca2e05db0b3a1a9c14",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.047830+00:00",'.
      p_xml = p_xml && '"razao": "0112-SANTOS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001328",'.
      p_xml = p_xml && '"codigo": "8d792114fdd11c897e2ae3a0a027866a",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.048198+00:00",'.
      p_xml = p_xml && '"razao": "0113-CAMPO VERDE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001409",'.
      p_xml = p_xml && '"codigo": "78d543c1c0822cf0ea3342a2ba02f357",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.048582+00:00",'.
      p_xml = p_xml && '"razao": "0114-PVA DO LESTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001590",'.
      p_xml = p_xml && '"codigo": "14df4612eaaa1147e8b05214118e47d9",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.048952+00:00",'.
      p_xml = p_xml && '"razao": "0115-PONTE PEDRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001670",'.
      p_xml = p_xml && '"codigo": "bbfbc16aef6d482a8bfe67392a61d920",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.049319+00:00",'.
      p_xml = p_xml && '"razao": "0116-TRANSPORTADORA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001832",'.
      p_xml = p_xml && '"codigo": "041ad140c396fdcee56b8a0e1619cd59",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.049689+00:00",'.
      p_xml = p_xml && '"razao": "0118-ÁGUA BOA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254001913",'.
      p_xml = p_xml && '"codigo": "b27ee70aeabd7fc5beb6530af34340a5",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.050064+00:00",'.
      p_xml = p_xml && '"razao": "0119-PORTO VELHO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002057",'.
      p_xml = p_xml && '"codigo": "3be6332c8a3736d89f1fa92e8bbd2578",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.050436+00:00",'.
      p_xml = p_xml && '"razao": "0120-PARANAGUÁ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002138",'.
      p_xml = p_xml && '"codigo": "6abbb00ce0a6d19ce1700871415dd130",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.050803+00:00",'.
      p_xml = p_xml && '"razao": "0121-ITACOATIARA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002219",'.
      p_xml = p_xml && '"codigo": "aec9dda241cf3d2d9a0a5ca3fb96bb73",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.051171+00:00",'.
      p_xml = p_xml && '"razao": "0122-CEREJEIRAS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002308",'.
      p_xml = p_xml && '"codigo": "f97dfcc5a1404b5293f1a2538f06a77c",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.051536+00:00",'.
      p_xml = p_xml && '"razao": "0123-BARREIRO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002480",'.
      p_xml = p_xml && '"codigo": "39506183df4f0f32abdb5f6db1ed4589",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.051903+00:00",'.
      p_xml = p_xml && '"razao": "0124-SINOP"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002561",'.
      p_xml = p_xml && '"codigo": "dfce25c3dffb31a061b3582ddf68c526",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.052278+00:00",'.
      p_xml = p_xml && '"razao": "0125-VILHENA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002642",'.
      p_xml = p_xml && '"codigo": "0d11e98991c94088c7094b65a5bfb146",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.052662+00:00",'.
      p_xml = p_xml && '"razao": "0126-NOVO HORIZONTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002723",'.
      p_xml = p_xml && '"codigo": "acb4ae70a1574b35e4fb52a40fe829c2",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.052993+00:00",'.
      p_xml = p_xml && '"razao": "0127-TUCUNARÉ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002804",'.
      p_xml = p_xml && '"codigo": "09e3cac565c42e24908866050d18b8a9",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.053328+00:00",'.
      p_xml = p_xml && '"razao": "0128-CARAJÁS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254002995",'.
      p_xml = p_xml && '"codigo": "3ef37ccd1fb87b43e2495e95f8a5eecc",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.053659+00:00",'.
      p_xml = p_xml && '"razao": "0129-TEIXEIRA JÚNIOR"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254003029",'.
      p_xml = p_xml && '"codigo": "0bf3075f352feae303f51a93db17e57f",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.054093+00:00",'.
      p_xml = p_xml && '"razao": "0130-SORRISO ESCRITÓRIO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254003100",'.
      p_xml = p_xml && '"codigo": "77832dacdede707429722d967990c546",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.054425+00:00",'.
      p_xml = p_xml && '"razao": "0131-BRASNORTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254003290",'.
      p_xml = p_xml && '"codigo": "d1ff8bcfc0b9f5d8a73adb1e9abf5f01",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.054756+00:00",'.
      p_xml = p_xml && '"razao": "0132-STA RITA DO TRIVELATO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254004181",'.
      p_xml = p_xml && '"codigo": "af93c77d297b693040fb096a8918ec0a",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.055086+00:00",'.
      p_xml = p_xml && '"razao": "0141-ITAMARATI"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254004262",'.
      p_xml = p_xml && '"codigo": "10748b43d45f9573c1621d3d7192b6fc",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.055417+00:00",'.
      p_xml = p_xml && '"razao": "0142-NOVA MUTUM"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254004343",'.
      p_xml = p_xml && '"codigo": "ca763f6ee85721e92aaae0d314805c8d",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.055748+00:00",'.
      p_xml = p_xml && '"razao": "0143-BOA ESPERANÇA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254004777",'.
      p_xml = p_xml && '"codigo": "68c130a17474693e67a689286e3adcda",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.056077+00:00",'.
      p_xml = p_xml && '"razao": "0147-VERA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254004858",'.
      p_xml = p_xml && '"codigo": "e7e1af86d76d6e1ee799e42c8b24ffc1",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.056410+00:00",'.
      p_xml = p_xml && '"razao": "0148-AGRO SAM"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254004939",'.
      p_xml = p_xml && '"codigo": "7d4fb0059f13f5e7eb953336ec477d2a",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.056741+00:00",'.
      p_xml = p_xml && '"razao": "0149-TANGURO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005072",'.
      p_xml = p_xml && '"codigo": "f77652cb046d108efaf2463e174a262a",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.057093+00:00",'.
      p_xml = p_xml && '"razao": "0150-CARAMORI"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005153",'.
      p_xml = p_xml && '"codigo": "a6598996c47c61af5d38f750b5517b26",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.057436+00:00",'.
      p_xml = p_xml && '"razao": "0151-CAMPOS DE JÚLIO AGD"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005234",'.
      p_xml = p_xml && '"codigo": "79f47eadf58424b457aaee69e03fc13e",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.057773+00:00",'.
      p_xml = p_xml && '"razao": "0152-ANA TERRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005315",'.
      p_xml = p_xml && '"codigo": "605e5b0117193a443d6e8deb0abc6d57",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.058100+00:00",'.
      p_xml = p_xml && '"razao": "0153-QUERENCIA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005404",'.
      p_xml = p_xml && '"codigo": "eff8c49cb8cb61ab2d5f9a56708fde49",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.058413+00:00",'.
      p_xml = p_xml && '"razao": "0154-RONDONÓPOLIS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005587",'.
      p_xml = p_xml && '"codigo": "64bc00ff1d8e454d97aaa5c5e87fafb5",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.058735+00:00",'.
      p_xml = p_xml && '"razao": "0155-LUCAS - FÁBRICA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005668",'.
      p_xml = p_xml && '"codigo": "cd058ab0ea389e8e732ce7e1f478404c",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.059036+00:00",'.
      p_xml = p_xml && '"razao": "0156-LUCAS - REFLORESTAMENTO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005749",'.
      p_xml = p_xml && '"codigo": "f5ff8cd9a4e474ca2c89b5aa7e298a02",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.059334+00:00",'.
      p_xml = p_xml && '"razao": "0157-NOVA UBIRATA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005820",'.
      p_xml = p_xml && '"codigo": "62a42b2bb4d8586e9363bf90716af2c7",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.059634+00:00",'.
      p_xml = p_xml && '"razao": "0158-DIAMANTINO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254005900",'.
      p_xml = p_xml && '"codigo": "fde7c132eb1c708c27c6c07ee632baa3",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.059932+00:00",'.
      p_xml = p_xml && '"razao": "0159-CASCAVEL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006044",'.
      p_xml = p_xml && '"codigo": "c40b302b88d5d30eb909d9a7d5960686",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.060235+00:00",'.
      p_xml = p_xml && '"razao": "0160-PASSO FUNDO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006125",'.
      p_xml = p_xml && '"codigo": "a982612caa68ccb37e1cf588bcc688c4",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.060535+00:00",'.
      p_xml = p_xml && '"razao": "0161-PORTOCHUELO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006206",'.
      p_xml = p_xml && '"codigo": "73fa88d7da08a7c5d3cf4eb2d0fcb89f",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.060835+00:00",'.
      p_xml = p_xml && '"razao": "0162-SÃO FRANCISCO DO SUL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006397",'.
      p_xml = p_xml && '"codigo": "86b49164de1a09853ef7487e7edce8f0",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.061135+00:00",'.
      p_xml = p_xml && '"razao": "0163-TAPURAH"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006478",'.
      p_xml = p_xml && '"codigo": "73355a3344243dae7591979016a60097",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.061434+00:00",'.
      p_xml = p_xml && '"razao": "0164-ROO - ESCRITORIO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006559",'.
      p_xml = p_xml && '"codigo": "1e9af1ca4988481b096ca1e00774632c",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.061738+00:00",'.
      p_xml = p_xml && '"razao": "0165-FAZENDA BOM SUCESSO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006630",'.
      p_xml = p_xml && '"codigo": "5e6b97eb521fdf0b5ecf8d4f8be4fd3a",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.062041+00:00",'.
      p_xml = p_xml && '"razao": "0166-FAZENDA NOVA ESPERANÇA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006800",'.
      p_xml = p_xml && '"codigo": "9651c3b378c9af773a7a70900c209226",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.062339+00:00",'.
      p_xml = p_xml && '"razao": "0168-FAZENDA STO ANDRÉ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254006982",'.
      p_xml = p_xml && '"codigo": "29fac09469cbf8a6699afc1ae59fea6c",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.062659+00:00",'.
      p_xml = p_xml && '"razao": "0169-MATUPÁ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007016",'.
      p_xml = p_xml && '"codigo": "4f89818567659fb38f5343074bcb4096",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.062958+00:00",'.
      p_xml = p_xml && '"razao": "0170-ESPIGÃO DO LESTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007105",'.
      p_xml = p_xml && '"codigo": "c5c3760a7c9720e27512a78f01e0b153",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.063257+00:00",'.
      p_xml = p_xml && '"razao": "0171-CONFRESA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007288",'.
      p_xml = p_xml && '"codigo": "8407066f21a7843b5bcecff8f8a8b939",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.063557+00:00",'.
      p_xml = p_xml && '"razao": "0172-SANTIAGO DO NORTE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007369",'.
      p_xml = p_xml && '"codigo": "600be9b53e699ccb644287fe8f28445d",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.063863+00:00",'.
      p_xml = p_xml && '"razao": "0173-TANGARÁ DA SERRA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007440",'.
      p_xml = p_xml && '"codigo": "740b7973d3106d1b4b91d5a662b6e792",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.064230+00:00",'.
      p_xml = p_xml && '"razao": "0174-RIO VERDE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007520",'.
      p_xml = p_xml && '"codigo": "9448972f9f2e4c4ab679a99d5fcb2a4d",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.064529+00:00",'.
      p_xml = p_xml && '"razao": "0175-COMODORO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007601",'.
      p_xml = p_xml && '"codigo": "2b36d6cfec0c439c6b2c0270060a7061",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.064826+00:00",'.
      p_xml = p_xml && '"razao": "0176-SÃO JOSÉ DO XINGU"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007792",'.
      p_xml = p_xml && '"codigo": "d9c9b234b9402d2c80c98853088c5b7f",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.044111+00:00",'.
      p_xml = p_xml && '"razao": "0177-LUFT"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007873",'.
      p_xml = p_xml && '"codigo": "0fefb56ccd161d34b0fb4525fed2e3ee",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.044524+00:00",'.
      p_xml = p_xml && '"razao": "0178-ARIQUEMES"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254007954",'.
      p_xml = p_xml && '"codigo": "1d6571d3948b23ba2ebc26db055ae951",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.065145+00:00",'.
      p_xml = p_xml && '"razao": "0179-BOA VISTA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254008098",'.
      p_xml = p_xml && '"codigo": "5c7269a9ba723e6bb675776dbee4d2ef",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.065443+00:00",'.
      p_xml = p_xml && '"razao": "0180-ITAITUBA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254008179",'.
      p_xml = p_xml && '"codigo": "debb58be9f9e7ddac4dbc958ac482903",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.065748+00:00",'.
      p_xml = p_xml && '"razao": "0181-BARCARENA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254008250",'.
      p_xml = p_xml && '"codigo": "e0f0108909e189d73eda268aef78388b",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.066049+00:00",'.
      p_xml = p_xml && '"razao": "0182-TRANSPORTADORA ITAITUBA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254008330",'.
      p_xml = p_xml && '"codigo": "c8e12c8beb49a62917b505e918161882",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.070755+00:00",'.
      p_xml = p_xml && '"razao": "0183-PARAGOMINAS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254008411",'.
      p_xml = p_xml && '"codigo": "eed371357536c78237a8961c65e41412",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.070451+00:00",'.
      p_xml = p_xml && '"razao": "0184-CAMPO VERDE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254008500",'.
      p_xml = p_xml && '"codigo": "77a83175a923470a84791b2a5abe22d0",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.070151+00:00",'.
      p_xml = p_xml && '"razao": "0185-SAPEZAL"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254008764",'.
      p_xml = p_xml && '"codigo": "a9c2bc79fef7f485b1bc3056d182f1ed",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.071355+00:00",'.
      p_xml = p_xml && '"razao": "0187-PONTES E LACERDA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254008926",'.
      p_xml = p_xml && '"codigo": "c1b9ec8daf41ef7fa4c6f7d9abbe6995",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.071657+00:00",'.
      p_xml = p_xml && '"razao": "0189-TRANSPORTADORA FROTA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009060",'.
      p_xml = p_xml && '"codigo": "acc0ce007fb247240e81da17abd1bb84",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.071054+00:00",'.
      p_xml = p_xml && '"razao": "0190-REDENCAO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009140",'.
      p_xml = p_xml && '"codigo": "2e07826842bfc07d5d866979bab77953",'.
      p_xml = p_xml && '"data_criacao": "2022-08-02T20:15:45.016701+00:00",'.
      p_xml = p_xml && '"razao": "AMAGGI EXPORTACAO E IMPORTACAO LTDA - VILHENA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009221",'.
      p_xml = p_xml && '"codigo": "4d353db7413fbf8ec6c27854e625ae92",'.
      p_xml = p_xml && '"data_criacao": "2022-08-02T20:15:55.936302+00:00",'.
      p_xml = p_xml && '"razao": "AMAGGI EXPORTACAO E IMPORTACAO LTDA - MATUPA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009302",'.
      p_xml = p_xml && '"codigo": "ce370e4b19506f33f7e6c8b2da51416c",'.
      p_xml = p_xml && '"data_criacao": "2021-11-17T20:55:27.746588+00:00",'.
      p_xml = p_xml && '"razao": "0193 Boa Vista ARMZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009493",'.
      p_xml = p_xml && '"codigo": "cd56d090dac73e3dd07a6cb197851515",'.
      p_xml = p_xml && '"data_criacao": "2022-08-02T20:16:08.417792+00:00",'.
      p_xml = p_xml && '"razao": "AMAGGI EXPORTACAO E IMPORTACAO LTDA - QUERENCIA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009574",'.
      p_xml = p_xml && '"codigo": "3a7a2d777efeb536891fc2c485afb96a",'.
      p_xml = p_xml && '"data_criacao": "2022-08-02T20:16:22.263829+00:00",'.
      p_xml = p_xml && '"razao": "AMAGGI EXPORTACAO E IMPORTACAO LTDA - MATUPA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009655",'.
      p_xml = p_xml && '"codigo": "e05e581abeafb014affe56fa000c2ea1",'.
      p_xml = p_xml && '"data_criacao": "2022-08-02T20:16:33.650779+00:00",'.
      p_xml = p_xml && '"razao": "AMAGGI EXPORTACAO E IMPORTACAO LTDA - RIO VERDE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009736",'.
      p_xml = p_xml && '"codigo": "aee78556562c3575d8230f1ab4675ebd",'.
      p_xml = p_xml && '"data_criacao": "2023-02-14T12:16:24.317904+00:00",'.
      p_xml = p_xml && '"razao": "PARANATINGA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254009906",'.
      p_xml = p_xml && '"codigo": "b7cb24f13e4e708ca3d2e2770677954f",'.
      p_xml = p_xml && '"data_criacao": "2023-09-13T12:11:01.186019+00:00",'.
      p_xml = p_xml && '"razao": "Filial 0199 - CD CAMPO NOVO DOS PARECIS"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010076",'.
      p_xml = p_xml && '"codigo": "0b471814651cb6bc0d8864b5e40f3aff",'.
      p_xml = p_xml && '"data_criacao": "2023-09-13T12:11:19.735419+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1100 - CD ARIQUEMES"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010157",'.
      p_xml = p_xml && '"codigo": "49c7677ce06d2d0ff7a09a2397133d9e",'.
      p_xml = p_xml && '"data_criacao": "2023-09-13T12:10:44.076963+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1101 – Pato Branco"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010238",'.
      p_xml = p_xml && '"codigo": "b035f85be2c1ef39c38c9df5b6b482cd",'.
      p_xml = p_xml && '"data_criacao": "2023-10-27T14:17:55.883640+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1102 - PARANATINGA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010319",'.
      p_xml = p_xml && '"codigo": "929ef1d38bfdff631f3c5d697b21ec73",'.
      p_xml = p_xml && '"data_criacao": "2023-11-01T11:19:00.570007+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1103 - CD APARECIDA DE GOIANIA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010408",'.
      p_xml = p_xml && '"codigo": "e1588b5d72a5315f1800aa87acb72bdc",'.
      p_xml = p_xml && '"data_criacao": "2023-10-26T11:19:42.261904+00:00",'.
      p_xml = p_xml && '"razao": "1104 – ALTO FLORESTA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010580",'.
      p_xml = p_xml && '"codigo": "891d11eda554bf36ffbd037c43a00ac4",'.
      p_xml = p_xml && '"data_criacao": "2023-12-22T12:53:47.149498+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1105 - CD MARABÁ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010661",'.
      p_xml = p_xml && '"codigo": "8bfde91f159a48d2dd14615a4a2eb7d5",'.
      p_xml = p_xml && '"data_criacao": "2024-02-14T11:16:11.014809+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1105 – CD CARAZINHO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010742",'.
      p_xml = p_xml && '"codigo": "3a343e658ff20fe1526a40ba2b28b660",'.
      p_xml = p_xml && '"data_criacao": "2023-11-01T11:58:06.467777+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1107 – Armazenadora - PB"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010823",'.
      p_xml = p_xml && '"codigo": "0c34440018c14ee49c9a0d7f84f8ff99",'.
      p_xml = p_xml && '"data_criacao": "2023-10-10T14:50:39.346935+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1108 - CD BARUERI"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254010904",'.
      p_xml = p_xml && '"codigo": "5507709a5a38145340335c06171a6086",'.
      p_xml = p_xml && '"data_criacao": "2023-10-10T14:50:59.384854+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1109 – Passo da Ilha"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "77294254011048",'.
      p_xml = p_xml && '"codigo": "8464bd31f8e9476c1449874e5786b51b",'.
      p_xml = p_xml && '"data_criacao": "2024-03-21T17:31:03.936760+00:00",'.
      p_xml = p_xml && '"razao": "Filial 1110 – UBERLÂNDIA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "84590892000118",'.
      p_xml = p_xml && '"codigo": "f2681fd38e4c526dc0ff0439ac447dc6",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.069847+00:00",'.
      p_xml = p_xml && '"razao": "1001-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "84590892000207",'.
      p_xml = p_xml && '"codigo": "f27d85f514c8759872f64c5c9aab3629",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.066348+00:00",'.
      p_xml = p_xml && '"razao": "1002-ENERGIA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "84590892000380",'.
      p_xml = p_xml && '"codigo": "243e7b796622aca4eeb703421600731b",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.066646+00:00",'.
      p_xml = p_xml && '"razao": "1003-PORTO VELHO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "84590892000541",'.
      p_xml = p_xml && '"codigo": "009b26d8ac5bd5cff0189b440c7d0aa4",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.066949+00:00",'.
      p_xml = p_xml && '"razao": "1005-BELEM"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "84590892000622",'.
      p_xml = p_xml && '"codigo": "d14596c4bdd7cdc9af4130ea93d82b49",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.067247+00:00",'.
      p_xml = p_xml && '"razao": "1006-CUIABÁ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "84590892000894",'.
      p_xml = p_xml && '"codigo": "3bcc20b23a54ad842f0e1a95a6de507d",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.067558+00:00",'.
      p_xml = p_xml && '"razao": "1008-IND. ESMAGADORA"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "84590892000975",'.
      p_xml = p_xml && '"codigo": "62e229a0ef68cd8c54193cfb05d07714",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.067883+00:00",'.
      p_xml = p_xml && '"razao": "1009-ITAC. REFLORESTAMENTO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "84590892001351",'.
      p_xml = p_xml && '"codigo": "4f2a3d29f46b43a14f0b1af9b1be21e9",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.068185+00:00",'.
      p_xml = p_xml && '"razao": "1013-PORTOCHUELO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792000187",'.
      p_xml = p_xml && '"codigo": "c8c4d501658f2dbeb7b618a7f69f8b7a",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.068484+00:00",'.
      p_xml = p_xml && '"razao": "3201-MATRIZ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792000268",'.
      p_xml = p_xml && '"codigo": "44645c29930be20eb903443687b96b7b",'.
      p_xml = p_xml && '"data_criacao": "2021-11-17T20:56:04.065461+00:00",'.
      p_xml = p_xml && '"razao": "3202 Fazenda SM6"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792000420",'.
      p_xml = p_xml && '"codigo": "cf8b2af2b4cb51fbfd90413c9a7046ef",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.068827+00:00",'.
      p_xml = p_xml && '"razao": "3204-FAZ. SM02"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792000500",'.
      p_xml = p_xml && '"codigo": "a86f68df2603097f2bb47e2bff5df3dc",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.069196+00:00",'.
      p_xml = p_xml && '"razao": "3205-FAZ. TUCUNARÉ"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792000691",'.
      p_xml = p_xml && '"codigo": "c6269b940e63be7760683cc685c208f0",'.
      p_xml = p_xml && '"data_criacao": "2020-03-15T17:07:03.069496+00:00",'.
      p_xml = p_xml && '"razao": "3206-FAZ. ITAMARATI"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792000934",'.
      p_xml = p_xml && '"codigo": "326375c65eb741df9d9566d36e073c5e",'.
      p_xml = p_xml && '"data_criacao": "2020-11-03T13:37:30.955409+00:00",'.
      p_xml = p_xml && '"razao": "3209 - SORRISO"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792001078",'.
      p_xml = p_xml && '"codigo": "be5f4f96c652860d8935aadcb6709f27",'.
      p_xml = p_xml && '"data_criacao": "2020-11-03T13:37:52.996957+00:00",'.
      p_xml = p_xml && '"razao": "3210 - SINOP"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792001159",'.
      p_xml = p_xml && '"codigo": "c056d1c1529265fc7f75be164ade12fe",'.
      p_xml = p_xml && '"data_criacao": "2020-11-03T13:38:17.095250+00:00",'.
      p_xml = p_xml && '"razao": "3211- LUCAS DO RIO VERDE"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792001230",'.
      p_xml = p_xml && '"codigo": "1474c86fdf3163919bc34eee737243e1",'.
      p_xml = p_xml && '"data_criacao": "2021-11-17T20:55:52.819613+00:00",'.
      p_xml = p_xml && '"razao": "3212 Fazenda Nova"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792001310",'.
      p_xml = p_xml && '"codigo": "b8c2e5a4638f52952337eabbda6b9450",'.
      p_xml = p_xml && '"data_criacao": "2021-11-17T20:55:41.547206+00:00",'.
      p_xml = p_xml && '"razao": "3213 Fazenda Independência"'.
      p_xml = p_xml && '},'.
      p_xml = p_xml && '{'.
      p_xml = p_xml && '"ativa": true,'.
      p_xml = p_xml && '"cnpj": "85509792001400",'.
      p_xml = p_xml && '"codigo": "eacd2c51577f745b8b3e422003cb157a",'.
      p_xml = p_xml && '"data_criacao": "2022-08-02T20:16:45.537873+00:00",'.
      p_xml = p_xml && '"razao": "AMAGGI PECUARIA LTDA - RONDONOPOLI"'.
      p_xml = p_xml && '}'.
      p_xml = p_xml && ']'.

    WHEN '02'. "Lista NF-e
      p_xml = p_xml && '['.
      p_xml = p_xml && '    {'.
      p_xml = p_xml && '        "codigo_download": "2024-06-27-50240612923609000111550010003059781000846669",'.
      p_xml = p_xml && '        "infNFe": {'.
      p_xml = p_xml && '            "cobr": {'.
      p_xml = p_xml && '                "dup": {'.
      p_xml = p_xml && '                    "dVenc": "2024-07-30",'.
      p_xml = p_xml && '                    "nDup": "001",'.
      p_xml = p_xml && '                    "vDup": "90574.00"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "fat": {'.
      p_xml = p_xml && '                    "nFat": "305978",'.
      p_xml = p_xml && '                    "vLiq": "90574.00",'.
      p_xml = p_xml && '                    "vOrig": "90574.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "dest": {'.
      p_xml = p_xml && '                "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                "IE": "4210093090",'.
      p_xml = p_xml && '                "email": "nfe@rhall.com.br",'.
      p_xml = p_xml && '                "enderDest": {'.
      p_xml = p_xml && '                    "CEP": "87013190",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4115200",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "nro": "901",'.
      p_xml = p_xml && '                    "xBairro": "ZONA 01",'.
      p_xml = p_xml && '                    "xCpl": "SALA 03, EDIF. DIAMANTE DE GOU",'.
      p_xml = p_xml && '                    "xLgr": "RUA VEREADOR BASILIO SAUTCHUK",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xPais": "BRASIL"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "indIEDest": "1",'.
      p_xml = p_xml && '                "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "det": ['.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "@nItem": "1",'.
      p_xml = p_xml && '                    "imposto": {'.
      p_xml = p_xml && '                        "COFINS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "ICMS": {'.
      p_xml = p_xml && '                            "CST": "41",'.
      p_xml = p_xml && '                            "orig": "0"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "PIS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        }'.
      p_xml = p_xml && '                    },'.
      p_xml = p_xml && '                    "prod": {'.
      p_xml = p_xml && '                        "CFOP": "6502",'.
      p_xml = p_xml && '                        "NCM": "12019000",'.
      p_xml = p_xml && '                        "cEAN": "SEM GTIN",'.
      p_xml = p_xml && '                        "cEANTrib": "SEM GTIN",'.
      p_xml = p_xml && '                        "cProd": "36",'.
      p_xml = p_xml && '                        "indTot": "1",'.
      p_xml = p_xml && '                        "qCom": "39380.000",'.
      p_xml = p_xml && '                        "qTrib": "39.380",'.
      p_xml = p_xml && '                        "uCom": "KG",'.
      p_xml = p_xml && '                        "uTrib": "TON",'.
      p_xml = p_xml && '                        "vProd": "90574.00",'.
      p_xml = p_xml && '                        "vUnCom": "2.3000000000",'.
      p_xml = p_xml && '                        "vUnTrib": "2300.0000000000",'.
      p_xml = p_xml && '                        "xProd": "SOJA COMERCIAL"'.
      p_xml = p_xml && '                    }'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            ],'.
      p_xml = p_xml && '            "emit": {'.
      p_xml = p_xml && '                "CNPJ": "12923609000111",'.
      p_xml = p_xml && '                "CRT": "3",'.
      p_xml = p_xml && '                "IE": "283627441",'.
      p_xml = p_xml && '                "enderEmit": {'.
      p_xml = p_xml && '                    "CEP": "79849899",'.
      p_xml = p_xml && '                    "UF": "MS",'.
      p_xml = p_xml && '                    "cMun": "5003702",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "fone": "6734167600",'.
      p_xml = p_xml && '                    "nro": "0",'.
      p_xml = p_xml && '                    "xBairro": "ZONA RURAL",'.
      p_xml = p_xml && '                    "xCpl": "KM 07 ARMAZ 02",'.
      p_xml = p_xml && '                    "xLgr": "RODOVIA ESTADUAL MS 162",'.
      p_xml = p_xml && '                    "xMun": "DOURADOS",'.
      p_xml = p_xml && '                    "xPais": "BRASIL"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "xFant": "ITAHUM EXPORT",'.
      p_xml = p_xml && '                "xNome": "ITAHUM EXPORT COMERCIO DE CEREAIS S.A."'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "entrega": {'.
      p_xml = p_xml && '                "CEP": "87065006",'.
      p_xml = p_xml && '                "CNPJ": "06078755000195",'.
      p_xml = p_xml && '                "IE": "9034277440",'.
      p_xml = p_xml && '                "UF": "PR",'.
      p_xml = p_xml && '                "cMun": "4115200",'.
      p_xml = p_xml && '                "cPais": "1058",'.
      p_xml = p_xml && '                "email": "nfe@rhall.com.br",'.
      p_xml = p_xml && '                "nro": "847",'.
      p_xml = p_xml && '                "xBairro": "PARQUE INDUSTRIAL II",'.
      p_xml = p_xml && '                "xCpl": "ANEXO PATIO FERROVIARIO",'.
      p_xml = p_xml && '                "xLgr": "AVENIDA MARCELO MESSIAS BUSIQUIA",'.
      p_xml = p_xml && '                "xMun": "MARINGA",'.
      p_xml = p_xml && '                "xNome": "RHALL TERMINAIS LTDA",'.
      p_xml = p_xml && '                "xPais": "BRASIL"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "ide": {'.
      p_xml = p_xml && '                "cDV": "9",'.
      p_xml = p_xml && '                "cMunFG": "5003702",'.
      p_xml = p_xml && '                "cNF": "00084666",'.
      p_xml = p_xml && '                "cUF": "50",'.
      p_xml = p_xml && '                "dhEmi": "2024-06-27T15:51:57-04:00",'.
      p_xml = p_xml && '                "dhSaiEnt": "2024-06-27T15:51:57-04:00",'.
      p_xml = p_xml && '                "finNFe": "1",'.
      p_xml = p_xml && '                "idDest": "2",'.
      p_xml = p_xml && '                "indFinal": "1",'.
      p_xml = p_xml && '                "indPres": "0",'.
      p_xml = p_xml && '                "mod": "55",'.
      p_xml = p_xml && '                "nNF": "305978",'.
      p_xml = p_xml && '                "natOp": "REMESSA MERC.ADQ./REC.TERC.C/FIM DE EXPORTACAO",'.
      p_xml = p_xml && '                "procEmi": "0",'.
      p_xml = p_xml && '                "serie": "1",'.
      p_xml = p_xml && '                "tpAmb": "1",'.
      p_xml = p_xml && '                "tpEmis": "1",'.
      p_xml = p_xml && '                "tpImp": "1",'.
      p_xml = p_xml && '                "tpNF": "1",'.
      p_xml = p_xml && '                "verProc": "1.0"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "infAdic": {'.
      p_xml = p_xml && '                "infAdFisco": "FUNDERSUL SOBRE VENDA - NOTA.: 944,92FUNDEMS SOBRE VENDA - NOTA.: 53,78",'.
      p_xml = p_xml && '                "infCpl": "Usuario: ESF Local Entrega: RHALL TERMINAIS LTDA - MARINGA/PR Local Emb/Ret: COAPUA ARMAZEM - CAMAPUA/MS Nr. Lanc'.
      p_xml = p_xml && ' to: 84666 Vendedor: 2-ED. Valor Aproximado dos Tributos (Cod. Item/Valor): 36/Vlr R$ 998,'.
      p_xml = p_xml && ' 70 . Total R$'.
      p_xml = p_xml && ' R$ 0,00 COFINS Vlr: R$ 0,00 / FRETE CIF R$"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "infRespTec": {'.
      p_xml = p_xml && '                "CNPJ": "03298453000134",'.
      p_xml = p_xml && '                "email": "rafael.mascarello@maxiconsystems.com.br",'.
      p_xml = p_xml && '                "fone": "4533784525",'.
      p_xml = p_xml && '                "xContato": "RAFAEL MASCARELLO"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "pag": {'.
      p_xml = p_xml && '                "detPag": {'.
      p_xml = p_xml && '                    "indPag": "1",'.
      p_xml = p_xml && '                    "tPag": "90",'.
      p_xml = p_xml && '                    "vPag": "0.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "retirada": {'.
      p_xml = p_xml && '                "CEP": "79420000",'.
      p_xml = p_xml && '                "CNPJ": "04502188000127",'.
      p_xml = p_xml && '                "IE": "283186020",'.
      p_xml = p_xml && '                "UF": "MS",'.
      p_xml = p_xml && '                "cMun": "5002605",'.
      p_xml = p_xml && '                "cPais": "1058",'.
      p_xml = p_xml && '                "nro": "213",'.
      p_xml = p_xml && '                "xBairro": "ZONA RURAL",'.
      p_xml = p_xml && '                "xLgr": "RODOVIA BR 060",'.
      p_xml = p_xml && '                "xMun": "CAMAPUA",'.
      p_xml = p_xml && '                "xNome": "COOPERATIVA DOS PRODUTORES AGROP DE CAMAPUA E REGIAO",'.
      p_xml = p_xml && '                "xPais": "BRASIL"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "total": {'.
      p_xml = p_xml && '                "ICMSTot": {'.
      p_xml = p_xml && '                    "vBC": "0.00",'.
      p_xml = p_xml && '                    "vBCST": "0.00",'.
      p_xml = p_xml && '                    "vCOFINS": "0.00",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vFCP": "0.00",'.
      p_xml = p_xml && '                    "vFCPST": "0.00",'.
      p_xml = p_xml && '                    "vFCPSTRet": "0.00",'.
      p_xml = p_xml && '                    "vFrete": "0.00",'.
      p_xml = p_xml && '                    "vICMS": "0.00",'.
      p_xml = p_xml && '                    "vICMSDeson": "0.00",'.
      p_xml = p_xml && '                    "vII": "0.00",'.
      p_xml = p_xml && '                    "vIPI": "0.00",'.
      p_xml = p_xml && '                    "vIPIDevol": "0.00",'.
      p_xml = p_xml && '                    "vNF": "90574.00",'.
      p_xml = p_xml && '                    "vOutro": "0.00",'.
      p_xml = p_xml && '                    "vPIS": "0.00",'.
      p_xml = p_xml && '                    "vProd": "90574.00",'.
      p_xml = p_xml && '                    "vST": "0.00",'.
      p_xml = p_xml && '                    "vSeg": "0.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "transp": {'.
      p_xml = p_xml && '                "modFrete": "0",'.
      p_xml = p_xml && '                "transporta": {'.
      p_xml = p_xml && '                    "CNPJ": "08024112000194",'.
      p_xml = p_xml && '                    "IE": "283387440",'.
      p_xml = p_xml && '                    "UF": "MS",'.
      p_xml = p_xml && '                    "xEnder": "AV.:R GUSTAVO ADOLFO PAVEL,1135",'.
      p_xml = p_xml && '                    "xMun": "DOURADOS",'.
      p_xml = p_xml && '                    "xNome": "CARGA PESADA TRANSPORTES LTDA"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "vol": {'.
      p_xml = p_xml && '                    "esp": "KG",'.
      p_xml = p_xml && '                    "pesoB": "39380.000",'.
      p_xml = p_xml && '                    "pesoL": "39380.000",'.
      p_xml = p_xml && '                    "qVol": "39380"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            }'.
      p_xml = p_xml && '        },'.
      p_xml = p_xml && '        "infNFeSupl": null,'.
      p_xml = p_xml && '        "infProt": {'.
      p_xml = p_xml && '            "@Id": "ID150240025544565",'.
      p_xml = p_xml && '            "cStat": "100",'.
      p_xml = p_xml && '            "chNFe": "50240612923609000111550010003059781000846669",'.
      p_xml = p_xml && '            "dhRecbto": "2024-06-27T15:52:03-04:00",'.
      p_xml = p_xml && '            "digVal": "frVL+iePIFcEkIktQY/b6yWh0nk=",'.
      p_xml = p_xml && '            "nProt": "150240025544565",'.
      p_xml = p_xml && '            "tpAmb": "1",'.
      p_xml = p_xml && '            "verAplic": "MS_6.2.10",'.
      p_xml = p_xml && '            "xMotivo": "Autorizado o uso da NF-e"'.
      p_xml = p_xml && '        }'.
      p_xml = p_xml && '    },'.
      p_xml = p_xml && '    {'.
      p_xml = p_xml && '        "codigo_download": "2024-06-27-41240676108349000103550020002991971776939574",'.
      p_xml = p_xml && '        "infNFe": {'.
      p_xml = p_xml && '            "cobr": {'.
      p_xml = p_xml && '                "dup": {'.
      p_xml = p_xml && '                    "dVenc": "2024-06-27",'.
      p_xml = p_xml && '                    "nDup": "001",'.
      p_xml = p_xml && '                    "vDup": "81696.00"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "fat": {'.
      p_xml = p_xml && '                    "nFat": "0091352209",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vLiq": "81696.00",'.
      p_xml = p_xml && '                    "vOrig": "81696.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "dest": {'.
      p_xml = p_xml && '                "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                "IE": "4210093090",'.
      p_xml = p_xml && '                "enderDest": {'.
      p_xml = p_xml && '                    "CEP": "87013190",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4115200",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "fone": "4535651379",'.
      p_xml = p_xml && '                    "nro": "901",'.
      p_xml = p_xml && '                    "xBairro": "ZONA 01",'.
      p_xml = p_xml && '                    "xCpl": "SALA 01, Z",'.
      p_xml = p_xml && '                    "xLgr": "RUA VEREADOR BASILI",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xPais": "Brasil"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "indIEDest": "1",'.
      p_xml = p_xml && '                "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "det": ['.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "@nItem": "1",'.
      p_xml = p_xml && '                    "imposto": {'.
      p_xml = p_xml && '                        "COFINS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "ICMS": {'.
      p_xml = p_xml && '                            "CST": "41",'.
      p_xml = p_xml && '                            "orig": "0"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "IPI": {'.
      p_xml = p_xml && '                            "CST": "53",'.
      p_xml = p_xml && '                            "cEnq": "999"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "PIS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "vTotTrib": "26510.35"'.
      p_xml = p_xml && '                    },'.
      p_xml = p_xml && '                    "prod": {'.
      p_xml = p_xml && '                        "CFOP": "5502",'.
      p_xml = p_xml && '                        "NCM": "12019000",'.
      p_xml = p_xml && '                        "cBenef": "PR800002",'.
      p_xml = p_xml && '                        "cEAN": "SEM GTIN",'.
      p_xml = p_xml && '                        "cEANTrib": "SEM GTIN",'.
      p_xml = p_xml && '                        "cProd": "000000001200000076",'.
      p_xml = p_xml && '                        "indTot": "1",'.
      p_xml = p_xml && '                        "qCom": "36800.0000",'.
      p_xml = p_xml && '                        "qTrib": "36.8000",'.
      p_xml = p_xml && '                        "uCom": "KG",'.
      p_xml = p_xml && '                        "uTrib": "TON",'.
      p_xml = p_xml && '                        "vProd": "81696.00",'.
      p_xml = p_xml && '                        "vUnCom": "2.2200000000",'.
      p_xml = p_xml && '                        "vUnTrib": "2220.0000000000",'.
      p_xml = p_xml && '                        "xProd": "SOJA GMO INTACTA IPRO"'.
      p_xml = p_xml && '                    }'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            ],'.
      p_xml = p_xml && '            "emit": {'.
      p_xml = p_xml && '                "CNAE": "4623199",'.
      p_xml = p_xml && '                "CNPJ": "76108349000103",'.
      p_xml = p_xml && '                "CRT": "3",'.
      p_xml = p_xml && '                "IE": "2020033110",'.
      p_xml = p_xml && '                "IM": "14559",'.
      p_xml = p_xml && '                "enderEmit": {'.
      p_xml = p_xml && '                    "CEP": "84196200",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4104907",'.
      p_xml = p_xml && '                    "fone": "4232348000",'.
      p_xml = p_xml && '                    "nro": "S/N",'.
      p_xml = p_xml && '                    "xBairro": "COLÔNIA CASTROLANDA",'.
      p_xml = p_xml && '                    "xLgr": "ROD. PR 340 KM 195 MAIS 490 MTS",'.
      p_xml = p_xml && '                    "xMun": "CASTRO",'.
      p_xml = p_xml && '                    "xPais": "Brasil"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "xFant": "CASTROLANDA COOP. AGROIND.",'.
      p_xml = p_xml && '                "xNome": "Castrolanda Cooperativa Agroindustrial LTDA"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "ide": {'.
      p_xml = p_xml && '                "cDV": "4",'.
      p_xml = p_xml && '                "cMunFG": "4104907",'.
      p_xml = p_xml && '                "cNF": "77693957",'.
      p_xml = p_xml && '                "cUF": "41",'.
      p_xml = p_xml && '                "dhEmi": "2024-06-27T17:32:54-03:00",'.
      p_xml = p_xml && '                "finNFe": "1",'.
      p_xml = p_xml && '                "idDest": "1",'.
      p_xml = p_xml && '                "indFinal": "0",'.
      p_xml = p_xml && '                "indIntermed": "0",'.
      p_xml = p_xml && '                "indPres": "1",'.
      p_xml = p_xml && '                "mod": "55",'.
      p_xml = p_xml && '                "nNF": "299197",'.
      p_xml = p_xml && '                "natOp": "Rem. merc. adq. rec. terc. fim esp. exportação",'.
      p_xml = p_xml && '                "procEmi": "0",'.
      p_xml = p_xml && '                "serie": "2",'.
      p_xml = p_xml && '                "tpAmb": "1",'.
      p_xml = p_xml && '                "tpEmis": "1",'.
      p_xml = p_xml && '                "tpImp": "1",'.
      p_xml = p_xml && '                "tpNF": "1",'.
      p_xml = p_xml && '                "verProc": "SAP NF-E 10.0"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "infRespTec": {'.
      p_xml = p_xml && '                "CNPJ": "74544297000192",'.
      p_xml = p_xml && '                "email": "responsavel.tecnico@sap.com",'.
      p_xml = p_xml && '                "fone": "00001155032400",'.
      p_xml = p_xml && '                "xContato": "SAP Product Engineering"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "pag": {'.
      p_xml = p_xml && '                "detPag": {'.
      p_xml = p_xml && '                    "tPag": "01",'.
      p_xml = p_xml && '                    "vPag": "81696.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "total": {'.
      p_xml = p_xml && '                "ICMSTot": {'.
      p_xml = p_xml && '                    "vBC": "0.00",'.
      p_xml = p_xml && '                    "vBCST": "0.00",'.
      p_xml = p_xml && '                    "vCOFINS": "0.00",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vFCP": "0.00",'.
      p_xml = p_xml && '                    "vFCPST": "0.00",'.
      p_xml = p_xml && '                    "vFCPSTRet": "0.00",'.
      p_xml = p_xml && '                    "vFrete": "0.00",'.
      p_xml = p_xml && '                    "vICMS": "0.00",'.
      p_xml = p_xml && '                    "vICMSDeson": "0.00",'.
      p_xml = p_xml && '                    "vII": "0.00",'.
      p_xml = p_xml && '                    "vIPI": "0.00",'.
      p_xml = p_xml && '                    "vIPIDevol": "0.00",'.
      p_xml = p_xml && '                    "vNF": "81696.00",'.
      p_xml = p_xml && '                    "vOutro": "0.00",'.
      p_xml = p_xml && '                    "vPIS": "0.00",'.
      p_xml = p_xml && '                    "vProd": "81696.00",'.
      p_xml = p_xml && '                    "vST": "0.00",'.
      p_xml = p_xml && '                    "vSeg": "0.00",'.
      p_xml = p_xml && '                    "vTotTrib": "26510.35"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "transp": {'.
      p_xml = p_xml && '                "modFrete": "1",'.
      p_xml = p_xml && '                "transporta": {'.
      p_xml = p_xml && '                    "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                    "IE": "4210093090",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "xEnder": "RUA VEREADOR BASILI 901, SALA 01, Z",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "vol": {'.
      p_xml = p_xml && '                    "pesoB": "36800.000",'.
      p_xml = p_xml && '                    "pesoL": "36800.000"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            }'.
      p_xml = p_xml && '        },'.
      p_xml = p_xml && '        "infNFeSupl": null,'.
      p_xml = p_xml && '        "infProt": {'.
      p_xml = p_xml && '            "@Id": "ID141240190863536",'.
      p_xml = p_xml && '            "cStat": "100",'.
      p_xml = p_xml && '            "chNFe": "41240676108349000103550020002991971776939574",'.
      p_xml = p_xml && '            "dhRecbto": "2024-06-27T17:33:32-03:00",'.
      p_xml = p_xml && '            "digVal": "+wIUUcylJAGghw8I6sbPQZHJuyw=",'.
      p_xml = p_xml && '            "nProt": "141240190863536",'.
      p_xml = p_xml && '            "tpAmb": "1",'.
      p_xml = p_xml && '            "verAplic": "PR-v4_8_46",'.
      p_xml = p_xml && '            "xMotivo": "Autorizado o uso da NF-e"'.
      p_xml = p_xml && '        }'.
      p_xml = p_xml && '    },'.
      p_xml = p_xml && '    {'.
      p_xml = p_xml && '        "codigo_download": "2024-06-27-41240677890846004247550100001930001556732873",'.
      p_xml = p_xml && '        "infNFe": {'.
      p_xml = p_xml && '            "cobr": {'.
      p_xml = p_xml && '                "dup": {'.
      p_xml = p_xml && '                    "dVenc": "2024-06-27",'.
      p_xml = p_xml && '                    "nDup": "001",'.
      p_xml = p_xml && '                    "vDup": "106554.00"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "fat": {'.
      p_xml = p_xml && '                    "nFat": "0091130274",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vLiq": "106554.00",'.
      p_xml = p_xml && '                    "vOrig": "106554.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "dest": {'.
      p_xml = p_xml && '                "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                "IE": "4210093090",'.
      p_xml = p_xml && '                "enderDest": {'.
      p_xml = p_xml && '                    "CEP": "87013190",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4115200",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "fone": "4432244880",'.
      p_xml = p_xml && '                    "nro": "901",'.
      p_xml = p_xml && '                    "xBairro": "ZONA 01",'.
      p_xml = p_xml && '                    "xLgr": "VEREADOR BASILIO SAUTCHUK",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xPais": "Brasil"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "indIEDest": "1",'.
      p_xml = p_xml && '                "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "det": ['.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "@nItem": "1",'.
      p_xml = p_xml && '                    "imposto": {'.
      p_xml = p_xml && '                        "COFINS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "ICMS": {'.
      p_xml = p_xml && '                            "CST": "51",'.
      p_xml = p_xml && '                            "modBC": "3",'.
      p_xml = p_xml && '                            "orig": "0",'.
      p_xml = p_xml && '                            "pDif": "0.0000",'.
      p_xml = p_xml && '                            "pICMS": "0.0000",'.
      p_xml = p_xml && '                            "pRedBC": "100.0000",'.
      p_xml = p_xml && '                            "vBC": "0.00",'.
      p_xml = p_xml && '                            "vICMS": "0.00",'.
      p_xml = p_xml && '                            "vICMSDif": "0.00",'.
      p_xml = p_xml && '                            "vICMSOp": "0.00"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "IPI": {'.
      p_xml = p_xml && '                            "CST": "54",'.
      p_xml = p_xml && '                            "cEnq": "002"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "PIS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        }'.
      p_xml = p_xml && '                    },'.
      p_xml = p_xml && '                    "infAdProd": "Lote: EXPUG2024",'.
      p_xml = p_xml && '                    "prod": {'.
      p_xml = p_xml && '                        "CFOP": "5501",'.
      p_xml = p_xml && '                        "NCM": "12019000",'.
      p_xml = p_xml && '                        "cBenef": "PR830078",'.
      p_xml = p_xml && '                        "cEAN": "SEM GTIN",'.
      p_xml = p_xml && '                        "cEANTrib": "SEM GTIN",'.
      p_xml = p_xml && '                        "cProd": "000000000020000165",'.
      p_xml = p_xml && '                        "indTot": "1",'.
      p_xml = p_xml && '                        "qCom": "49.5600",'.
      p_xml = p_xml && '                        "qTrib": "49.5600",'.
      p_xml = p_xml && '                        "rastro": {'.
      p_xml = p_xml && '                            "dFab": "2023-09-27",'.
      p_xml = p_xml && '                            "dVal": "2026-12-30",'.
      p_xml = p_xml && '                            "nLote": "EXPUG2024",'.
      p_xml = p_xml && '                            "qLote": "49.560"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "uCom": "TO",'.
      p_xml = p_xml && '                        "uTrib": "TON",'.
      p_xml = p_xml && '                        "vProd": "106554.00",'.
      p_xml = p_xml && '                        "vUnCom": "2150.0000000000",'.
      p_xml = p_xml && '                        "vUnTrib": "2150.0000000000",'.
      p_xml = p_xml && '                        "xProd": "SOJA GRAOS NC TON"'.
      p_xml = p_xml && '                    }'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            ],'.
      p_xml = p_xml && '            "emit": {'.
      p_xml = p_xml && '                "CNPJ": "77890846004247",'.
      p_xml = p_xml && '                "CRT": "3",'.
      p_xml = p_xml && '                "IE": "9058152614",'.
      p_xml = p_xml && '                "IM": "33.920-2",'.
      p_xml = p_xml && '                "enderEmit": {'.
      p_xml = p_xml && '                    "CEP": "85031350",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4109401",'.
      p_xml = p_xml && '                    "fone": "4236258000",'.
      p_xml = p_xml && '                    "nro": "211",'.
      p_xml = p_xml && '                    "xBairro": "Vassoural",'.
      p_xml = p_xml && '                    "xCpl": "KM 354",'.
      p_xml = p_xml && '                    "xLgr": "ROD BR-277",'.
      p_xml = p_xml && '                    "xMun": "Guarapuava",'.
      p_xml = p_xml && '                    "xPais": "Brasil"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "xFant": "Agrária óleo e farelo",'.
      p_xml = p_xml && '                "xNome": "Cooperativa Agrária Agroindustrial"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "ide": {'.
      p_xml = p_xml && '                "cDV": "3",'.
      p_xml = p_xml && '                "cMunFG": "4109401",'.
      p_xml = p_xml && '                "cNF": "55673287",'.
      p_xml = p_xml && '                "cUF": "41",'.
      p_xml = p_xml && '                "dhEmi": "2024-06-27T16:07:43-03:00",'.
      p_xml = p_xml && '                "dhSaiEnt": "2024-06-27T16:17:43-03:00",'.
      p_xml = p_xml && '                "finNFe": "1",'.
      p_xml = p_xml && '                "idDest": "1",'.
      p_xml = p_xml && '                "indFinal": "0",'.
      p_xml = p_xml && '                "indIntermed": "0",'.
      p_xml = p_xml && '                "indPres": "9",'.
      p_xml = p_xml && '                "mod": "55",'.
      p_xml = p_xml && '                "nNF": "193000",'.
      p_xml = p_xml && '                "natOp": "Rem.prod.estab. com fim específico de exportação",'.
      p_xml = p_xml && '                "procEmi": "0",'.
      p_xml = p_xml && '                "serie": "10",'.
      p_xml = p_xml && '                "tpAmb": "1",'.
      p_xml = p_xml && '                "tpEmis": "1",'.
      p_xml = p_xml && '                "tpImp": "1",'.
      p_xml = p_xml && '                "tpNF": "1",'.
      p_xml = p_xml && '                "verProc": "SAP NFE 4,00"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "infRespTec": {'.
      p_xml = p_xml && '                "CNPJ": "74544297000192",'.
      p_xml = p_xml && '                "email": "responsavel.tecnico@sap.com",'.
      p_xml = p_xml && '                "fone": "1155032400",'.
      p_xml = p_xml && '                "xContato": "SAP Product Engineering"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "pag": {'.
      p_xml = p_xml && '                "detPag": {'.
      p_xml = p_xml && '                    "tPag": "99",'.
      p_xml = p_xml && '                    "vPag": "106554.00",'.
      p_xml = p_xml && '                    "xPag": "Pagamento em Carteira"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "total": {'.
      p_xml = p_xml && '                "ICMSTot": {'.
      p_xml = p_xml && '                    "vBC": "0.00",'.
      p_xml = p_xml && '                    "vBCST": "0.00",'.
      p_xml = p_xml && '                    "vCOFINS": "0.00",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vFCP": "0.00",'.
      p_xml = p_xml && '                    "vFCPST": "0.00",'.
      p_xml = p_xml && '                    "vFCPSTRet": "0.00",'.
      p_xml = p_xml && '                    "vFrete": "0.00",'.
      p_xml = p_xml && '                    "vICMS": "0.00",'.
      p_xml = p_xml && '                    "vICMSDeson": "0.00",'.
      p_xml = p_xml && '                    "vII": "0.00",'.
      p_xml = p_xml && '                    "vIPI": "0.00",'.
      p_xml = p_xml && '                    "vIPIDevol": "0.00",'.
      p_xml = p_xml && '                    "vNF": "106554.00",'.
      p_xml = p_xml && '                    "vOutro": "0.00",'.
      p_xml = p_xml && '                    "vPIS": "0.00",'.
      p_xml = p_xml && '                    "vProd": "106554.00",'.
      p_xml = p_xml && '                    "vST": "0.00",'.
      p_xml = p_xml && '                    "vSeg": "0.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "transp": {'.
      p_xml = p_xml && '                "modFrete": "1",'.
      p_xml = p_xml && '                "transporta": {'.
      p_xml = p_xml && '                    "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                    "IE": "4210093090",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "xEnder": "VEREADOR BASILIO SAUTCHUK 901",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "veicTransp": {'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "placa": "IKP4H96"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "vol": {'.
      p_xml = p_xml && '                    "esp": "Kilograma",'.
      p_xml = p_xml && '                    "nVol": "49560.000",'.
      p_xml = p_xml && '                    "pesoB": "49560.000",'.
      p_xml = p_xml && '                    "pesoL": "49560.000",'.
      p_xml = p_xml && '                    "qVol": "49560"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            }'.
      p_xml = p_xml && '        },'.
      p_xml = p_xml && '        "infNFeSupl": null,'.
      p_xml = p_xml && '        "infProt": {'.
      p_xml = p_xml && '            "@Id": "ID141240190702220",'.
      p_xml = p_xml && '            "cStat": "100",'.
      p_xml = p_xml && '            "chNFe": "41240677890846004247550100001930001556732873",'.
      p_xml = p_xml && '            "dhRecbto": "2024-06-27T16:08:21-03:00",'.
      p_xml = p_xml && '            "digVal": "vG5Oxu6ffrA8Qu/8fitMNCNqa50=",'.
      p_xml = p_xml && '            "nProt": "141240190702220",'.
      p_xml = p_xml && '            "tpAmb": "1",'.
      p_xml = p_xml && '            "verAplic": "PR-v4_8_46",'.
      p_xml = p_xml && '            "xMotivo": "Autorizado o uso da NF-e"'.
      p_xml = p_xml && '        }'.
      p_xml = p_xml && '    },'.
      p_xml = p_xml && '    {'.
      p_xml = p_xml && '        "codigo_download": "2024-06-27-50240637185812000169550010000171941000240465",'.
      p_xml = p_xml && '        "infNFe": {'.
      p_xml = p_xml && '            "autXML": ['.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "CPF": "60045566100"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "CNPJ": "09250717000166"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            ],'.
      p_xml = p_xml && '            "cobr": {'.
      p_xml = p_xml && '                "dup": {'.
      p_xml = p_xml && '                    "dVenc": "2024-07-01",'.
      p_xml = p_xml && '                    "nDup": "001",'.
      p_xml = p_xml && '                    "vDup": "74162.67"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "fat": {'.
      p_xml = p_xml && '                    "nFat": "017194",'.
      p_xml = p_xml && '                    "vLiq": "74162.67",'.
      p_xml = p_xml && '                    "vOrig": "74162.67"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "dest": {'.
      p_xml = p_xml && '                "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                "IE": "4210093090",'.
      p_xml = p_xml && '                "email": "nfe.fiscal@amaggi.com.br",'.
      p_xml = p_xml && '                "enderDest": {'.
      p_xml = p_xml && '                    "CEP": "87013190",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4115200",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "fone": "44998298950",'.
      p_xml = p_xml && '                    "nro": "901",'.
      p_xml = p_xml && '                    "xBairro": "ZONA 01",'.
      p_xml = p_xml && '                    "xLgr": "RUA VEREADOR BASILIO SAUTCHUK, SALA 03",'.
      p_xml = p_xml && '                    "xMun": "Maringa",'.
      p_xml = p_xml && '                    "xPais": "BRASIL"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "indIEDest": "1",'.
      p_xml = p_xml && '                "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "det": ['.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "@nItem": "1",'.
      p_xml = p_xml && '                    "imposto": {'.
      p_xml = p_xml && '                        "COFINS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "ICMS": {'.
      p_xml = p_xml && '                            "CST": "41",'.
      p_xml = p_xml && '                            "orig": "0"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "PIS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        }'.
      p_xml = p_xml && '                    },'.
      p_xml = p_xml && '                    "infAdProd": "Valor Aprox. dos Tributos = R$ 0.00",'.
      p_xml = p_xml && '                    "prod": {'.
      p_xml = p_xml && '                        "CFOP": "6502",'.
      p_xml = p_xml && '                        "NCM": "12019000",'.
      p_xml = p_xml && '                        "cEAN": "SEM GTIN",'.
      p_xml = p_xml && '                        "cEANTrib": "SEM GTIN",'.
      p_xml = p_xml && '                        "cProd": "21",'.
      p_xml = p_xml && '                        "indTot": "1",'.
      p_xml = p_xml && '                        "qCom": "32480.0000",'.
      p_xml = p_xml && '                        "qTrib": "32.4800",'.
      p_xml = p_xml && '                        "uCom": "KG",'.
      p_xml = p_xml && '                        "uTrib": "TON",'.
      p_xml = p_xml && '                        "vProd": "74162.67",'.
      p_xml = p_xml && '                        "vUnCom": "2.2833333333",'.
      p_xml = p_xml && '                        "vUnTrib": "2283.3333333000",'.
      p_xml = p_xml && '                        "xProd": "SOJA EM GRAOS"'.
      p_xml = p_xml && '                    }'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            ],'.
      p_xml = p_xml && '            "emit": {'.
      p_xml = p_xml && '                "CNPJ": "37185812000169",'.
      p_xml = p_xml && '                "CRT": "3",'.
      p_xml = p_xml && '                "IE": "284548448",'.
      p_xml = p_xml && '                "enderEmit": {'.
      p_xml = p_xml && '                    "CEP": "79950000",'.
      p_xml = p_xml && '                    "UF": "MS",'.
      p_xml = p_xml && '                    "cMun": "5005707",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "fone": "6734613908",'.
      p_xml = p_xml && '                    "nro": "SN",'.
      p_xml = p_xml && '                    "xBairro": "Zona Rural",'.
      p_xml = p_xml && '                    "xLgr": "AVENIDA MATO GROSSO- FAZENDA GAUCHA I",'.
      p_xml = p_xml && '                    "xMun": "Navirai",'.
      p_xml = p_xml && '                    "xPais": "BRASIL"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "xFant": "Nova Prata Comercio de Graos e Armazens Gerais",'.
      p_xml = p_xml && '                "xNome": "Nova Prata Com. de Graos e Armazens Gerais Ltda"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "entrega": {'.
      p_xml = p_xml && '                "CEP": "87065006",'.
      p_xml = p_xml && '                "CNPJ": "06078755000195",'.
      p_xml = p_xml && '                "IE": "9034277440",'.
      p_xml = p_xml && '                "UF": "PR",'.
      p_xml = p_xml && '                "cMun": "4115200",'.
      p_xml = p_xml && '                "cPais": "1058",'.
      p_xml = p_xml && '                "email": "estoques@rhall.com.br",'.
      p_xml = p_xml && '                "fone": "4430273344",'.
      p_xml = p_xml && '                "nro": "937",'.
      p_xml = p_xml && '                "xBairro": "BAIRRO: PARQUE INDUSTRIAL",'.
      p_xml = p_xml && '                "xLgr": "AV : MARCELO MESSIAS BUSIQUIA",'.
      p_xml = p_xml && '                "xMun": "Maringa",'.
      p_xml = p_xml && '                "xNome": "RHALL TERMINAIS LTDA",'.
      p_xml = p_xml && '                "xPais": "Brasil"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "ide": {'.
      p_xml = p_xml && '                "cDV": "5",'.
      p_xml = p_xml && '                "cMunFG": "5005707",'.
      p_xml = p_xml && '                "cNF": "00024046",'.
      p_xml = p_xml && '                "cUF": "50",'.
      p_xml = p_xml && '                "dhEmi": "2024-06-27T16:31:45-04:00",'.
      p_xml = p_xml && '                "dhSaiEnt": "2024-06-27T16:31:45-04:00",'.
      p_xml = p_xml && '                "finNFe": "1",'.
      p_xml = p_xml && '                "idDest": "2",'.
      p_xml = p_xml && '                "indFinal": "0",'.
      p_xml = p_xml && '                "indPres": "0",'.
      p_xml = p_xml && '                "mod": "55",'.
      p_xml = p_xml && '                "nNF": "17194",'.
      p_xml = p_xml && '                "natOp": "6502 - VENDA COM FIM ESPECIFICO DE EXPORTACAO",'.
      p_xml = p_xml && '                "procEmi": "0",'.
      p_xml = p_xml && '                "serie": "1",'.
      p_xml = p_xml && '                "tpAmb": "1",'.
      p_xml = p_xml && '                "tpEmis": "1",'.
      p_xml = p_xml && '                "tpImp": "1",'.
      p_xml = p_xml && '                "tpNF": "1",'.
      p_xml = p_xml && '                "verProc": "CONTROLSOFT"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "infRespTec": {'.
      p_xml = p_xml && '                "CNPJ": "02081082000171",'.
      p_xml = p_xml && '                "email": "controlsoft@controlsoft.com.br",'.
      p_xml = p_xml && '                "fone": "06635459400",'.
      p_xml = p_xml && '                "xContato": "Joao Nestor Mayer"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "pag": {'.
      p_xml = p_xml && '                "detPag": {'.
      p_xml = p_xml && '                    "indPag": "1",'.
      p_xml = p_xml && '                    "tPag": "99",'.
      p_xml = p_xml && '                    "vPag": "74162.67",'.
      p_xml = p_xml && '                    "xPag": "Outros"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "retirada": {'.
      p_xml = p_xml && '                "CEP": "79804970",'.
      p_xml = p_xml && '                "CNPJ": "01646273000170",'.
      p_xml = p_xml && '                "IE": "282978011",'.
      p_xml = p_xml && '                "UF": "MS",'.
      p_xml = p_xml && '                "cMun": "5003702",'.
      p_xml = p_xml && '                "cPais": "1058",'.
      p_xml = p_xml && '                "fone": "6734271300",'.
      p_xml = p_xml && '                "nro": "S/N",'.
      p_xml = p_xml && '                "xBairro": "ZONA RURAL",'.
      p_xml = p_xml && '                "xLgr": "ROD BR 162",'.
      p_xml = p_xml && '                "xMun": "Dourados",'.
      p_xml = p_xml && '                "xNome": "COOPASOL COOP AGROP SULMATOGROSSENSE",'.
      p_xml = p_xml && '                "xPais": "Brasil"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "total": {'.
      p_xml = p_xml && '                "ICMSTot": {'.
      p_xml = p_xml && '                    "vBC": "0.00",'.
      p_xml = p_xml && '                    "vBCST": "0.00",'.
      p_xml = p_xml && '                    "vCOFINS": "0.00",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vFCP": "0.00",'.
      p_xml = p_xml && '                    "vFCPST": "0.00",'.
      p_xml = p_xml && '                    "vFCPSTRet": "0.00",'.
      p_xml = p_xml && '                    "vFrete": "0.00",'.
      p_xml = p_xml && '                    "vICMS": "0.00",'.
      p_xml = p_xml && '                    "vICMSDeson": "0.00",'.
      p_xml = p_xml && '                    "vII": "0.00",'.
      p_xml = p_xml && '                    "vIPI": "0.00",'.
      p_xml = p_xml && '                    "vIPIDevol": "0.00",'.
      p_xml = p_xml && '                    "vNF": "74162.67",'.
      p_xml = p_xml && '                    "vOutro": "0.00",'.
      p_xml = p_xml && '                    "vPIS": "0.00",'.
      p_xml = p_xml && '                    "vProd": "74162.67",'.
      p_xml = p_xml && '                    "vST": "0.00",'.
      p_xml = p_xml && '                    "vSeg": "0.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "transp": {'.
      p_xml = p_xml && '                "modFrete": "0",'.
      p_xml = p_xml && '                "transporta": {'.
      p_xml = p_xml && '                    "CNPJ": "05220925000595",'.
      p_xml = p_xml && '                    "IE": "283554673",'.
      p_xml = p_xml && '                    "UF": "MS",'.
      p_xml = p_xml && '                    "xEnder": "ROD BR 163 KM 05 S/N",'.
      p_xml = p_xml && '                    "xMun": "Dourados",'.
      p_xml = p_xml && '                    "xNome": "TRANSPORTES TRANSVIDAL LTDA"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "vol": {'.
      p_xml = p_xml && '                    "pesoB": "32480.000",'.
      p_xml = p_xml && '                    "pesoL": "32480.000",'.
      p_xml = p_xml && '                    "qVol": "1"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            }'.
      p_xml = p_xml && '        },'.
      p_xml = p_xml && '        "infNFeSupl": null,'.
      p_xml = p_xml && '        "infProt": {'.
      p_xml = p_xml && '            "@Id": "ID150240025555856",'.
      p_xml = p_xml && '            "cStat": "100",'.
      p_xml = p_xml && '            "chNFe": "50240637185812000169550010000171941000240465",'.
      p_xml = p_xml && '            "dhRecbto": "2024-06-27T16:31:47-04:00",'.
      p_xml = p_xml && '            "digVal": "LuYSC2dbXbjdW5hJUAbPBHCUDcc=",'.
      p_xml = p_xml && '            "nProt": "150240025555856",'.
      p_xml = p_xml && '            "tpAmb": "1",'.
      p_xml = p_xml && '            "verAplic": "MS_6.2.10",'.
      p_xml = p_xml && '            "xMotivo": "Autorizado o uso da NF-e"'.
      p_xml = p_xml && '        }'.
      p_xml = p_xml && '    },'.
      p_xml = p_xml && '    {'.
      p_xml = p_xml && '        "codigo_download": "2024-06-27-50240603902129003441551340000052561781285483",'.
      p_xml = p_xml && '        "infNFe": {'.
      p_xml = p_xml && '            "cobr": {'.
      p_xml = p_xml && '                "dup": {'.
      p_xml = p_xml && '                    "dVenc": "2024-07-12",'.
      p_xml = p_xml && '                    "nDup": "001",'.
      p_xml = p_xml && '                    "vDup": "72512.62"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "fat": {'.
      p_xml = p_xml && '                    "nFat": "5256",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vLiq": "72512.62",'.
      p_xml = p_xml && '                    "vOrig": "72512.62"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "dest": {'.
      p_xml = p_xml && '                "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                "IE": "4210093090",'.
      p_xml = p_xml && '                "email": "nfe.fiscal@amaggi.com.br",'.
      p_xml = p_xml && '                "enderDest": {'.
      p_xml = p_xml && '                    "CEP": "87013190",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4115200",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "nro": "901",'.
      p_xml = p_xml && '                    "xBairro": "ZONA 01",'.
      p_xml = p_xml && '                    "xCpl": "SALA 03, SOBRE LOJA EDIF DIAMANTE DE GOULD",'.
      p_xml = p_xml && '                    "xLgr": "RUA VEREADOR BASILIO SAUTCHUK",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xPais": "BRASIL"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "indIEDest": "1",'.
      p_xml = p_xml && '                "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "det": ['.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "@nItem": "1",'.
      p_xml = p_xml && '                    "imposto": {'.
      p_xml = p_xml && '                        "COFINS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "ICMS": {'.
      p_xml = p_xml && '                            "CST": "41",'.
      p_xml = p_xml && '                            "orig": "0"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "IPI": {'.
      p_xml = p_xml && '                            "CST": "53",'.
      p_xml = p_xml && '                            "cEnq": "999"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "PIS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        }'.
      p_xml = p_xml && '                    },'.
      p_xml = p_xml && '                    "infAdProd": "Federal R$ Estadual R$ Municipal R$ Fonte: IBPT",'.
      p_xml = p_xml && '                    "prod": {'.
      p_xml = p_xml && '                        "CFOP": "6502",'.
      p_xml = p_xml && '                        "NCM": "12019000",'.
      p_xml = p_xml && '                        "cEAN": "SEM GTIN",'.
      p_xml = p_xml && '                        "cEANTrib": "SEM GTIN",'.
      p_xml = p_xml && '                        "cProd": "GRA100035",'.
      p_xml = p_xml && '                        "indTot": "1",'.
      p_xml = p_xml && '                        "qCom": "30640.0000",'.
      p_xml = p_xml && '                        "qTrib": "30.6400",'.
      p_xml = p_xml && '                        "uCom": "KG",'.
      p_xml = p_xml && '                        "uTrib": "TON",'.
      p_xml = p_xml && '                        "vProd": "72512.62",'.
      p_xml = p_xml && '                        "vUnCom": "2.3666000000",'.
      p_xml = p_xml && '                        "vUnTrib": "2366.6000000000",'.
      p_xml = p_xml && '                        "xProd": "SOJA EM GRAOS"'.
      p_xml = p_xml && '                    }'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            ],'.
      p_xml = p_xml && '            "emit": {'.
      p_xml = p_xml && '                "CNPJ": "03902129003441",'.
      p_xml = p_xml && '                "CRT": "3",'.
      p_xml = p_xml && '                "IE": "284774790",'.
      p_xml = p_xml && '                "enderEmit": {'.
      p_xml = p_xml && '                    "CEP": "79950000",'.
      p_xml = p_xml && '                    "UF": "MS",'.
      p_xml = p_xml && '                    "cMun": "5005707",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "nro": "S/N",'.
      p_xml = p_xml && '                    "xBairro": "Zona Rural",'.
      p_xml = p_xml && '                    "xCpl": "KM31",'.
      p_xml = p_xml && '                    "xLgr": "Rodovia MS-489",'.
      p_xml = p_xml && '                    "xMun": "NAVIRAI",'.
      p_xml = p_xml && '                    "xPais": "BRASIL"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "xNome": "COPASUL COOPERATIVA AGRICOLA SUL MATOGROSSENSE"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "entrega": {'.
      p_xml = p_xml && '                "CEP": "83221565",'.
      p_xml = p_xml && '                "CNPJ": "84046101028101",'.
      p_xml = p_xml && '                "IE": "1180354294",'.
      p_xml = p_xml && '                "UF": "PR",'.
      p_xml = p_xml && '                "cMun": "4118204",'.
      p_xml = p_xml && '                "cPais": "1058",'.
      p_xml = p_xml && '                "nro": "250",'.
      p_xml = p_xml && '                "xBairro": "DOM PEDRO II",'.
      p_xml = p_xml && '                "xCpl": "NC",'.
      p_xml = p_xml && '                "xLgr": "AVENIDA BENTO ROCHA",'.
      p_xml = p_xml && '                "xMun": "PARANAGUA",'.
      p_xml = p_xml && '                "xNome": "BUNGE ALIMENTOS S/A",'.
      p_xml = p_xml && '                "xPais": "BRASIL"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "ide": {'.
      p_xml = p_xml && '                "cDV": "3",'.
      p_xml = p_xml && '                "cMunFG": "5005707",'.
      p_xml = p_xml && '                "cNF": "78128548",'.
      p_xml = p_xml && '                "cUF": "50",'.
      p_xml = p_xml && '                "dhEmi": "2024-06-27T15:04:30+03:00",'.
      p_xml = p_xml && '                "dhSaiEnt": "2024-06-27T15:04:30+03:00",'.
      p_xml = p_xml && '                "finNFe": "1",'.
      p_xml = p_xml && '                "idDest": "2",'.
      p_xml = p_xml && '                "indFinal": "1",'.
      p_xml = p_xml && '                "indIntermed": "0",'.
      p_xml = p_xml && '                "indPres": "2",'.
      p_xml = p_xml && '                "mod": "55",'.
      p_xml = p_xml && '                "nNF": "5256",'.
      p_xml = p_xml && '                "natOp": "6502- REMESSA DE MERC. ADQ. OU RECEBIDA DE TERCEIROS, COM FI",'.
      p_xml = p_xml && '                "procEmi": "0",'.
      p_xml = p_xml && '                "serie": "134",'.
      p_xml = p_xml && '                "tpAmb": "1",'.
      p_xml = p_xml && '                "tpEmis": "1",'.
      p_xml = p_xml && '                "tpImp": "1",'.
      p_xml = p_xml && '                "tpNF": "1",'.
      p_xml = p_xml && '                "verProc": "SynchroDFe_3.7.0.7"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "infRespTec": {'.
      p_xml = p_xml && '                "CNPJ": "67185306000300",'.
      p_xml = p_xml && '                "email": "suportedfe@synchro.com.br",'.
      p_xml = p_xml && '                "fone": "1925129200",'.
      p_xml = p_xml && '                "xContato": "SYNCHRO Solucao Fiscal Brasil"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "pag": {'.
      p_xml = p_xml && '                "detPag": {'.
      p_xml = p_xml && '                    "indPag": "1",'.
      p_xml = p_xml && '                    "tPag": "90",'.
      p_xml = p_xml && '                    "vPag": "0.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "total": {'.
      p_xml = p_xml && '                "ICMSTot": {'.
      p_xml = p_xml && '                    "vBC": "0.00",'.
      p_xml = p_xml && '                    "vBCST": "0",'.
      p_xml = p_xml && '                    "vCOFINS": "0",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vFCP": "0",'.
      p_xml = p_xml && '                    "vFCPST": "0.00",'.
      p_xml = p_xml && '                    "vFCPSTRet": "0.00",'.
      p_xml = p_xml && '                    "vFrete": "0.00",'.
      p_xml = p_xml && '                    "vICMS": "0.00",'.
      p_xml = p_xml && '                    "vICMSDeson": "0",'.
      p_xml = p_xml && '                    "vII": "0",'.
      p_xml = p_xml && '                    "vIPI": "0.00",'.
      p_xml = p_xml && '                    "vIPIDevol": "0.00",'.
      p_xml = p_xml && '                    "vNF": "72512.62",'.
      p_xml = p_xml && '                    "vOutro": "0.00",'.
      p_xml = p_xml && '                    "vPIS": "0",'.
      p_xml = p_xml && '                    "vProd": "72512.62",'.
      p_xml = p_xml && '                    "vST": "0",'.
      p_xml = p_xml && '                    "vSeg": "0.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "transp": {'.
      p_xml = p_xml && '                "modFrete": "0",'.
      p_xml = p_xml && '                "transporta": {'.
      p_xml = p_xml && '                    "CNPJ": "76302157001024",'.
      p_xml = p_xml && '                    "IE": "282731814",'.
      p_xml = p_xml && '                    "UF": "MS",'.
      p_xml = p_xml && '                    "xEnder": "RODOVIA BR-163",'.
      p_xml = p_xml && '                    "xMun": "DOURADOS",'.
      p_xml = p_xml && '                    "xNome": "TRANSPORTES RODOVIARIOS VALE DO PIQUIRI LTDA"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "vol": {'.
      p_xml = p_xml && '                    "pesoB": "30640.000",'.
      p_xml = p_xml && '                    "pesoL": "30640.000"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            }'.
      p_xml = p_xml && '        },'.
      p_xml = p_xml && '        "infNFeSupl": null,'.
      p_xml = p_xml && '        "infProt": {'.
      p_xml = p_xml && '            "@Id": "ID150240025530797",'.
      p_xml = p_xml && '            "cStat": "100",'.
      p_xml = p_xml && '            "chNFe": "50240603902129003441551340000052561781285483",'.
      p_xml = p_xml && '            "dhRecbto": "2024-06-27T15:05:11-04:00",'.
      p_xml = p_xml && '            "digVal": "Q5441TyWwvhrYEadcTuvGzfkI5k=",'.
      p_xml = p_xml && '            "nProt": "150240025530797",'.
      p_xml = p_xml && '            "tpAmb": "1",'.
      p_xml = p_xml && '            "verAplic": "MS_6.2.10",'.
      p_xml = p_xml && '            "xMotivo": "Autorizado o uso da NF-e"'.
      p_xml = p_xml && '        }'.
      p_xml = p_xml && '    },'.
      p_xml = p_xml && '    {'.
      p_xml = p_xml && '        "codigo_download": "2024-06-27-41240677890846004247550100001930041057714925",'.
      p_xml = p_xml && '        "infNFe": {'.
      p_xml = p_xml && '            "cobr": {'.
      p_xml = p_xml && '                "dup": {'.
      p_xml = p_xml && '                    "dVenc": "2024-06-27",'.
      p_xml = p_xml && '                    "nDup": "001",'.
      p_xml = p_xml && '                    "vDup": "108016.00"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "fat": {'.
      p_xml = p_xml && '                    "nFat": "0091130291",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vLiq": "108016.00",'.
      p_xml = p_xml && '                    "vOrig": "108016.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "dest": {'.
      p_xml = p_xml && '                "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                "IE": "4210093090",'.
      p_xml = p_xml && '                "enderDest": {'.
      p_xml = p_xml && '                    "CEP": "87013190",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4115200",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "fone": "4432244880",'.
      p_xml = p_xml && '                    "nro": "901",'.
      p_xml = p_xml && '                    "xBairro": "ZONA 01",'.
      p_xml = p_xml && '                    "xLgr": "VEREADOR BASILIO SAUTCHUK",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xPais": "Brasil"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "indIEDest": "1",'.
      p_xml = p_xml && '                "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "det": ['.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "@nItem": "1",'.
      p_xml = p_xml && '                    "imposto": {'.
      p_xml = p_xml && '                        "COFINS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "ICMS": {'.
      p_xml = p_xml && '                            "CST": "51",'.
      p_xml = p_xml && '                            "modBC": "3",'.
      p_xml = p_xml && '                            "orig": "0",'.
      p_xml = p_xml && '                            "pDif": "0.0000",'.
      p_xml = p_xml && '                            "pICMS": "0.0000",'.
      p_xml = p_xml && '                            "pRedBC": "100.0000",'.
      p_xml = p_xml && '                            "vBC": "0.00",'.
      p_xml = p_xml && '                            "vICMS": "0.00",'.
      p_xml = p_xml && '                            "vICMSDif": "0.00",'.
      p_xml = p_xml && '                            "vICMSOp": "0.00"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "IPI": {'.
      p_xml = p_xml && '                            "CST": "54",'.
      p_xml = p_xml && '                            "cEnq": "002"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "PIS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        }'.
      p_xml = p_xml && '                    },'.
      p_xml = p_xml && '                    "infAdProd": "Lote: EXPUG2024",'.
      p_xml = p_xml && '                    "prod": {'.
      p_xml = p_xml && '                        "CFOP": "5501",'.
      p_xml = p_xml && '                        "NCM": "12019000",'.
      p_xml = p_xml && '                        "cBenef": "PR830078",'.
      p_xml = p_xml && '                        "cEAN": "SEM GTIN",'.
      p_xml = p_xml && '                        "cEANTrib": "SEM GTIN",'.
      p_xml = p_xml && '                        "cProd": "000000000020000165",'.
      p_xml = p_xml && '                        "indTot": "1",'.
      p_xml = p_xml && '                        "qCom": "50.2400",'.
      p_xml = p_xml && '                        "qTrib": "50.2400",'.
      p_xml = p_xml && '                        "rastro": {'.
      p_xml = p_xml && '                            "dFab": "2023-09-27",'.
      p_xml = p_xml && '                            "dVal": "2026-12-30",'.
      p_xml = p_xml && '                            "nLote": "EXPUG2024",'.
      p_xml = p_xml && '                            "qLote": "50.240"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "uCom": "TO",'.
      p_xml = p_xml && '                        "uTrib": "TON",'.
      p_xml = p_xml && '                        "vProd": "108016.00",'.
      p_xml = p_xml && '                        "vUnCom": "2150.0000000000",'.
      p_xml = p_xml && '                        "vUnTrib": "2150.0000000000",'.
      p_xml = p_xml && '                        "xProd": "SOJA GRAOS NC TON"'.
      p_xml = p_xml && '                    }'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            ],'.
      p_xml = p_xml && '            "emit": {'.
      p_xml = p_xml && '                "CNPJ": "77890846004247",'.
      p_xml = p_xml && '                "CRT": "3",'.
      p_xml = p_xml && '                "IE": "9058152614",'.
      p_xml = p_xml && '                "IM": "33.920-2",'.
      p_xml = p_xml && '                "enderEmit": {'.
      p_xml = p_xml && '                    "CEP": "85031350",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4109401",'.
      p_xml = p_xml && '                    "fone": "4236258000",'.
      p_xml = p_xml && '                    "nro": "211",'.
      p_xml = p_xml && '                    "xBairro": "Vassoural",'.
      p_xml = p_xml && '                    "xCpl": "KM 354",'.
      p_xml = p_xml && '                    "xLgr": "ROD BR-277",'.
      p_xml = p_xml && '                    "xMun": "Guarapuava",'.
      p_xml = p_xml && '                    "xPais": "Brasil"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "xFant": "Agrária óleo e farelo",'.
      p_xml = p_xml && '                "xNome": "Cooperativa Agrária Agroindustrial"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "ide": {'.
      p_xml = p_xml && '                "cDV": "5",'.
      p_xml = p_xml && '                "cMunFG": "4109401",'.
      p_xml = p_xml && '                "cNF": "05771492",'.
      p_xml = p_xml && '                "cUF": "41",'.
      p_xml = p_xml && '                "dhEmi": "2024-06-27T16:40:32-03:00",'.
      p_xml = p_xml && '                "dhSaiEnt": "2024-06-27T16:50:32-03:00",'.
      p_xml = p_xml && '                "finNFe": "1",'.
      p_xml = p_xml && '                "idDest": "1",'.
      p_xml = p_xml && '                "indFinal": "0",'.
      p_xml = p_xml && '                "indIntermed": "0",'.
      p_xml = p_xml && '                "indPres": "9",'.
      p_xml = p_xml && '                "mod": "55",'.
      p_xml = p_xml && '                "nNF": "193004",'.
      p_xml = p_xml && '                "natOp": "Rem.prod.estab. com fim específico de exportação",'.
      p_xml = p_xml && '                "procEmi": "0",'.
      p_xml = p_xml && '                "serie": "10",'.
      p_xml = p_xml && '                "tpAmb": "1",'.
      p_xml = p_xml && '                "tpEmis": "1",'.
      p_xml = p_xml && '                "tpImp": "1",'.
      p_xml = p_xml && '                "tpNF": "1",'.
      p_xml = p_xml && '                "verProc": "SAP NFE 4,00"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "infRespTec": {'.
      p_xml = p_xml && '                "CNPJ": "74544297000192",'.
      p_xml = p_xml && '                "email": "responsavel.tecnico@sap.com",'.
      p_xml = p_xml && '                "fone": "1155032400",'.
      p_xml = p_xml && '                "xContato": "SAP Product Engineering"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "pag": {'.
      p_xml = p_xml && '                "detPag": {'.
      p_xml = p_xml && '                    "tPag": "99",'.
      p_xml = p_xml && '                    "vPag": "108016.00",'.
      p_xml = p_xml && '                    "xPag": "Pagamento em Carteira"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "total": {'.
      p_xml = p_xml && '                "ICMSTot": {'.
      p_xml = p_xml && '                    "vBC": "0.00",'.
      p_xml = p_xml && '                    "vBCST": "0.00",'.
      p_xml = p_xml && '                    "vCOFINS": "0.00",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vFCP": "0.00",'.
      p_xml = p_xml && '                    "vFCPST": "0.00",'.
      p_xml = p_xml && '                    "vFCPSTRet": "0.00",'.
      p_xml = p_xml && '                    "vFrete": "0.00",'.
      p_xml = p_xml && '                    "vICMS": "0.00",'.
      p_xml = p_xml && '                    "vICMSDeson": "0.00",'.
      p_xml = p_xml && '                    "vII": "0.00",'.
      p_xml = p_xml && '                    "vIPI": "0.00",'.
      p_xml = p_xml && '                    "vIPIDevol": "0.00",'.
      p_xml = p_xml && '                    "vNF": "108016.00",'.
      p_xml = p_xml && '                    "vOutro": "0.00",'.
      p_xml = p_xml && '                    "vPIS": "0.00",'.
      p_xml = p_xml && '                    "vProd": "108016.00",'.
      p_xml = p_xml && '                    "vST": "0.00",'.
      p_xml = p_xml && '                    "vSeg": "0.00"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "transp": {'.
      p_xml = p_xml && '                "modFrete": "1",'.
      p_xml = p_xml && '                "transporta": {'.
      p_xml = p_xml && '                    "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                    "IE": "4210093090",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "xEnder": "VEREADOR BASILIO SAUTCHUK 901",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "veicTransp": {'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "placa": "ABY7B67"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "vol": {'.
      p_xml = p_xml && '                    "esp": "Kilograma",'.
      p_xml = p_xml && '                    "nVol": "50240.000",'.
      p_xml = p_xml && '                    "pesoB": "50240.000",'.
      p_xml = p_xml && '                    "pesoL": "50240.000",'.
      p_xml = p_xml && '                    "qVol": "50240"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            }'.
      p_xml = p_xml && '        },'.
      p_xml = p_xml && '        "infNFeSupl": null,'.
      p_xml = p_xml && '        "infProt": {'.
      p_xml = p_xml && '            "@Id": "ID141240190767296",'.
      p_xml = p_xml && '            "cStat": "100",'.
      p_xml = p_xml && '            "chNFe": "41240677890846004247550100001930041057714925",'.
      p_xml = p_xml && '            "dhRecbto": "2024-06-27T16:41:21-03:00",'.
      p_xml = p_xml && '            "digVal": "d6Blu1vv9avkV4qGAH3K+N9mpu4=",'.
      p_xml = p_xml && '            "nProt": "141240190767296",'.
      p_xml = p_xml && '            "tpAmb": "1",'.
      p_xml = p_xml && '            "verAplic": "PR-v4_8_46",'.
      p_xml = p_xml && '            "xMotivo": "Autorizado o uso da NF-e"'.
      p_xml = p_xml && '        }'.
      p_xml = p_xml && '    },'.
      p_xml = p_xml && '    {'.
      p_xml = p_xml && '        "codigo_download": "2024-06-27-35240679114450023530550010000959561381004356",'.
      p_xml = p_xml && '        "infNFe": {'.
      p_xml = p_xml && '            "autXML": {'.
      p_xml = p_xml && '                "CNPJ": "04055601000233"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "cobr": {'.
      p_xml = p_xml && '                "dup": {'.
      p_xml = p_xml && '                    "dVenc": "2024-06-27",'.
      p_xml = p_xml && '                    "nDup": "001",'.
      p_xml = p_xml && '                    "vDup": "84291.17"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "fat": {'.
      p_xml = p_xml && '                    "nFat": "1",'.
      p_xml = p_xml && '                    "vLiq": "84291.17",'.
      p_xml = p_xml && '                    "vOrig": "84291.17"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "compra": null,'.
      p_xml = p_xml && '            "dest": {'.
      p_xml = p_xml && '                "CNPJ": "77294254000356",'.
      p_xml = p_xml && '                "IE": "4210093090",'.
      p_xml = p_xml && '                "email": "MARINGA.PORTO@AMAGGI.COM.BR,NFE.FISCAL@AMAGGI.COM.BR",'.
      p_xml = p_xml && '                "enderDest": {'.
      p_xml = p_xml && '                    "CEP": "87013190",'.
      p_xml = p_xml && '                    "UF": "PR",'.
      p_xml = p_xml && '                    "cMun": "4115200",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "fone": "4432244880",'.
      p_xml = p_xml && '                    "nro": "3",'.
      p_xml = p_xml && '                    "xBairro": "ZONA 01",'.
      p_xml = p_xml && '                    "xLgr": "RUA VEREADOR BASILIO SAUTCHUK, 901-SL",'.
      p_xml = p_xml && '                    "xMun": "MARINGA",'.
      p_xml = p_xml && '                    "xPais": "BRASIL"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "indIEDest": "1",'.
      p_xml = p_xml && '                "xNome": "AMAGGI EXPORTACAO E IMPORTACAO LTDA"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "det": ['.
      p_xml = p_xml && '                {'.
      p_xml = p_xml && '                    "@nItem": "1",'.
      p_xml = p_xml && '                    "imposto": {'.
      p_xml = p_xml && '                        "COFINS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "ICMS": {'.
      p_xml = p_xml && '                            "CST": "41",'.
      p_xml = p_xml && '                            "orig": "0"'.
      p_xml = p_xml && '                        },'.
      p_xml = p_xml && '                        "PIS": {'.
      p_xml = p_xml && '                            "CST": "08"'.
      p_xml = p_xml && '                        }'.
      p_xml = p_xml && '                    },'.
      p_xml = p_xml && '                    "infAdProd": "CN=3838318;; PRECO UNITARIO DO ITEM IMPRESSO C/4 CASAS DECIMAIS MAS CALCULADO C/10 CASAS DECIMAIS, PRECO = R$ 2,1083334167.",'.
      p_xml = p_xml && '                    "prod": {'.
      p_xml = p_xml && '                        "CFOP": "6502",'.
      p_xml = p_xml && '                        "NCM": "12019000",'.
      p_xml = p_xml && '                        "cEAN": "SEM GTIN",'.
      p_xml = p_xml && '                        "cEANTrib": "SEM GTIN",'.
      p_xml = p_xml && '                        "cProd": "595420",'.
      p_xml = p_xml && '                        "indTot": "1",'.
      p_xml = p_xml && '                        "qCom": "39980.0000",'.
      p_xml = p_xml && '                        "qTrib": "39.9800",'.
      p_xml = p_xml && '                        "uCom": "KG",'.
      p_xml = p_xml && '                        "uTrib": "TON",'.
      p_xml = p_xml && '                        "vProd": "84291.17",'.
      p_xml = p_xml && '                        "vUnCom": "2.1083334167",'.
      p_xml = p_xml && '                        "vUnTrib": "2108.3334167000",'.
      p_xml = p_xml && '                        "xProd": "SOJA COMERCIAL"'.
      p_xml = p_xml && '                    }'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            ],'.
      p_xml = p_xml && '            "emit": {'.
      p_xml = p_xml && '                "CNPJ": "79114450023530",'.
      p_xml = p_xml && '                "CRT": "3",'.
      p_xml = p_xml && '                "IE": "501034074111",'.
      p_xml = p_xml && '                "enderEmit": {'.
      p_xml = p_xml && '                    "CEP": "19978899",'.
      p_xml = p_xml && '                    "UF": "SP",'.
      p_xml = p_xml && '                    "cMun": "3535309",'.
      p_xml = p_xml && '                    "cPais": "1058",'.
      p_xml = p_xml && '                    "fone": "4432213007",'.
      p_xml = p_xml && '                    "nro": "SN",'.
      p_xml = p_xml && '                    "xBairro": "AREA RURAL DE PALMITAL",'.
      p_xml = p_xml && '                    "xCpl": "KM 418",'.
      p_xml = p_xml && '                    "xLgr": "ROD RAPOSO TAVARES",'.
      p_xml = p_xml && '                    "xMun": "PALMITAL",'.
      p_xml = p_xml && '                    "xPais": "BRASIL"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "xFant": "PALMITAL - SP",'.
      p_xml = p_xml && '                "xNome": "COCAMAR COOPERATIVA AGROINDUSTRIAL"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "ide": {'.
      p_xml = p_xml && '                "cDV": "6",'.
      p_xml = p_xml && '                "cMunFG": "3535309",'.
      p_xml = p_xml && '                "cNF": "38100435",'.
      p_xml = p_xml && '                "cUF": "35",'.
      p_xml = p_xml && '                "dhEmi": "2024-06-27T17:28:02-03:00",'.
      p_xml = p_xml && '                "dhSaiEnt": "2024-06-27T17:28:02-03:00",'.
      p_xml = p_xml && '                "finNFe": "1",'.
      p_xml = p_xml && '                "idDest": "2",'.
      p_xml = p_xml && '                "indFinal": "0",'.
      p_xml = p_xml && '                "indPres": "0",'.
      p_xml = p_xml && '                "mod": "55",'.
      p_xml = p_xml && '                "nNF": "95956",'.
      p_xml = p_xml && '                "natOp": "REM.MERC.ADQ.REC.TERC.P/EXPORT",'.
      p_xml = p_xml && '                "procEmi": "0",'.
      p_xml = p_xml && '                "serie": "1",'.
      p_xml = p_xml && '                "tpAmb": "1",'.
      p_xml = p_xml && '                "tpEmis": "1",'.
      p_xml = p_xml && '                "tpImp": "1",'.
      p_xml = p_xml && '                "tpNF": "1",'.
      p_xml = p_xml && '                "verProc": "4.00"'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "pag": {'.
      p_xml = p_xml && '                "detPag": {'.
      p_xml = p_xml && '                    "indPag": "1",'.
      p_xml = p_xml && '                    "tPag": "16",'.
      p_xml = p_xml && '                    "vPag": "84291.17"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "total": {'.
      p_xml = p_xml && '                "ICMSTot": {'.
      p_xml = p_xml && '                    "vBC": "0.00",'.
      p_xml = p_xml && '                    "vBCST": "0.00",'.
      p_xml = p_xml && '                    "vCOFINS": "0.00",'.
      p_xml = p_xml && '                    "vDesc": "0.00",'.
      p_xml = p_xml && '                    "vFCP": "0.00",'.
      p_xml = p_xml && '                    "vFCPST": "0.00",'.
      p_xml = p_xml && '                    "vFCPSTRet": "0.00",'.
      p_xml = p_xml && '                    "vFrete": "0.00",'.
      p_xml = p_xml && '                    "vICMS": "0.00",'.
      p_xml = p_xml && '                    "vICMSDeson": "0.00",'.
      p_xml = p_xml && '                    "vII": "0.00",'.
      p_xml = p_xml && '                    "vIPI": "0.00",'.
      p_xml = p_xml && '                    "vIPIDevol": "0.00",'.
      p_xml = p_xml && '                    "vNF": "84291.17",'.
      p_xml = p_xml && '                    "vOutro": "0.00",'.
      p_xml = p_xml && '                    "vPIS": "0.00",'.
      p_xml = p_xml && '                    "vProd": "84291.17",'.
      p_xml = p_xml && '                    "vST": "0.00",'.
      p_xml = p_xml && '                    "vSeg": "0.00"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "retTrib": null'.
      p_xml = p_xml && '            },'.
      p_xml = p_xml && '            "transp": {'.
      p_xml = p_xml && '                "modFrete": "1",'.
      p_xml = p_xml && '                "transporta": {'.
      p_xml = p_xml && '                    "CNPJ": "07569161000735",'.
      p_xml = p_xml && '                    "UF": "SP",'.
      p_xml = p_xml && '                    "xEnder": "V.ANHANGUERA, S/N. KM 150.FERRAO",'.
      p_xml = p_xml && '                    "xMun": "LIMEIRA",'.
      p_xml = p_xml && '                    "xNome": "G10 - TRANSPORTES LTDA"'.
      p_xml = p_xml && '                },'.
      p_xml = p_xml && '                "vol": {'.
      p_xml = p_xml && '                    "pesoB": "58340.000",'.
      p_xml = p_xml && '                    "pesoL": "39980.000",'.
      p_xml = p_xml && '                    "qVol": "39980"'.
      p_xml = p_xml && '                }'.
      p_xml = p_xml && '            }'.
      p_xml = p_xml && '        },'.
      p_xml = p_xml && '        "infNFeSupl": null,'.
      p_xml = p_xml && '        "infProt": {'.
      p_xml = p_xml && '            "@Id": "Id135241371655628",'.
      p_xml = p_xml && '            "cStat": "100",'.
      p_xml = p_xml && '            "chNFe": "35240679114450023530550010000959561381004356",'.
      p_xml = p_xml && '            "dhRecbto": "2024-06-27T17:28:14-03:00",'.
      p_xml = p_xml && '            "digVal": "AljL+4TkxjTVE8Gl+ErYkTowETI=",'.
      p_xml = p_xml && '            "nProt": "135241371655628",'.
      p_xml = p_xml && '            "tpAmb": "1",'.
      p_xml = p_xml && '            "verAplic": "SP_NFE_PL009_V4",'.
      p_xml = p_xml && '            "xMotivo": "Autorizado o uso da NF-e"'.
      p_xml = p_xml && '        }'.
      p_xml = p_xml && '    }'.
      p_xml = p_xml && ' ]'.
    WHEN '03'.
      p_xml = p_xml && '<nfeProc versao="4.00" xmlns="http://www.portalfiscal.inf.br/nfe">'.
      p_xml = p_xml && '<NFe xmlns="http://www.portalfiscal.inf.br/nfe">'.
      p_xml = p_xml && '<infNFe versao="4.00" Id="NFe41240600050091760925551242706200013467262754">'.
      p_xml = p_xml && '<ide>'.
      p_xml = p_xml && '<cUF>41</cUF>'.
      p_xml = p_xml && '<cNF>46726275</cNF>'.
      p_xml = p_xml && '<natOp>Venda</natOp>'.
      p_xml = p_xml && '<mod>55</mod>'.
      p_xml = p_xml && '<serie>124</serie>'.
      p_xml = p_xml && '<nNF>270620001</nNF>'.
      p_xml = p_xml && '<dhEmi>2024-06-27T09:30:52-03:00</dhEmi>'.
      p_xml = p_xml && '<tpNF>1</tpNF>'.
      p_xml = p_xml && '<idDest>1</idDest>'.
      p_xml = p_xml && '<cMunFG>4107801</cMunFG>'.
      p_xml = p_xml && '<tpImp>0</tpImp>'.
      p_xml = p_xml && '<tpEmis>3</tpEmis>'.
      p_xml = p_xml && '<cDV>4</cDV>'.
      p_xml = p_xml && '<tpAmb>1</tpAmb>'.
      p_xml = p_xml && '<finNFe>1</finNFe>'.
      p_xml = p_xml && '<indFinal>0</indFinal>'.
      p_xml = p_xml && '<indPres>9</indPres>'.
      p_xml = p_xml && '<indIntermed>0</indIntermed>'.
      p_xml = p_xml && '<procEmi>3</procEmi>'.
      p_xml = p_xml && '<verProc>1.00</verProc>'.
      p_xml = p_xml && '</ide>'.
      p_xml = p_xml && '<emit>'.
      p_xml = p_xml && '<CPF>50091760925</CPF>'.
      p_xml = p_xml && '<xNome>EUCLIDES FAVORETO</xNome>'.
      p_xml = p_xml && '<enderEmit>'.
      p_xml = p_xml && '<xLgr>Lote 195: A, B, G1, E, G, H,</xLgr>'.
      p_xml = p_xml && '<nro>0</nro>'.
      p_xml = p_xml && '<xCpl>Lado direio</xCpl>'.
      p_xml = p_xml && '<xBairro>zona rural</xBairro>'.
      p_xml = p_xml && '<cMun>4107801</cMun>'.
      p_xml = p_xml && '<xMun>Floraí</xMun>'.
      p_xml = p_xml && '<UF>PR</UF>'.
      p_xml = p_xml && '<CEP>87185000</CEP>'.
      p_xml = p_xml && '<cPais>1058</cPais>'.
      p_xml = p_xml && '<xPais>Brasil</xPais>'.
      p_xml = p_xml && '</enderEmit>'.
      p_xml = p_xml && '<IE>9513452109</IE>'.
      p_xml = p_xml && '<CRT>3</CRT>'.
      p_xml = p_xml && '</emit>'.
      p_xml = p_xml && '<dest>'.
      p_xml = p_xml && '<CNPJ>77294254000356</CNPJ>'.
      p_xml = p_xml && '<xNome>AMAGGI EXP E IMP LTDA</xNome>'.
      p_xml = p_xml && '<enderDest>'.
      p_xml = p_xml && '<xLgr>RUA VEREADOR BASILIO SAUTCHUK</xLgr>'.
      p_xml = p_xml && '<nro>901</nro>'.
      p_xml = p_xml && '<xCpl>SL 03SOBRE LOJA,ED DIAMANTE DE GOULD</xCpl>'.
      p_xml = p_xml && '<xBairro>ZONA 01</xBairro>'.
      p_xml = p_xml && '<cMun>4115200</cMun>'.
      p_xml = p_xml && '<xMun>Maringá</xMun>'.
      p_xml = p_xml && '<UF>PR</UF>'.
      p_xml = p_xml && '<CEP>87013190</CEP>'.
      p_xml = p_xml && '</enderDest>'.
      p_xml = p_xml && '<indIEDest>1</indIEDest>'.
      p_xml = p_xml && '<IE>4210093090</IE>'.
      p_xml = p_xml && '</dest>'.
      p_xml = p_xml && '<det nItem="1">'.
      p_xml = p_xml && '<prod>'.
      p_xml = p_xml && '<cProd>002</cProd>'.
      p_xml = p_xml && '<cEAN>SEM GTIN</cEAN>'.
      p_xml = p_xml && '<xProd>Milho</xProd>'.
      p_xml = p_xml && '<NCM>10059090</NCM>'.
      p_xml = p_xml && '<cBenef>PR830060</cBenef>'.
      p_xml = p_xml && '<CFOP>5101</CFOP>'.
      p_xml = p_xml && '<uCom>Kg</uCom>'.
      p_xml = p_xml && '<qCom>22500.0000</qCom>'.
      p_xml = p_xml && '<vUnCom>0.9666</vUnCom>'.
      p_xml = p_xml && '<vProd>21748.50</vProd>'.
      p_xml = p_xml && '<cEANTrib>SEM GTIN</cEANTrib>'.
      p_xml = p_xml && '<uTrib>Kg</uTrib>'.
      p_xml = p_xml && '<qTrib>22500.0000</qTrib>'.
      p_xml = p_xml && '<vUnTrib>0.9666</vUnTrib>'.
      p_xml = p_xml && '<indTot>1</indTot>'.
      p_xml = p_xml && '<infProdNFF>'.
      p_xml = p_xml && '<cProdFisco>001110070-0198</cProdFisco>'.
      p_xml = p_xml && '<cOperNFF>63</cOperNFF>'.
      p_xml = p_xml && '</infProdNFF>'.
      p_xml = p_xml && '</prod>'.
      p_xml = p_xml && '<imposto>'.
      p_xml = p_xml && '<ICMS>'.
      p_xml = p_xml && '<ICMS51>'.
      p_xml = p_xml && '<orig>0</orig>'.
      p_xml = p_xml && '<CST>51</CST>'.
      p_xml = p_xml && '<modBC>3</modBC>'.
      p_xml = p_xml && '<pRedBC>0.00</pRedBC>'.
      p_xml = p_xml && '<vBC>21748.50</vBC>'.
      p_xml = p_xml && '<pICMS>12.00</pICMS>'.
      p_xml = p_xml && '<vICMSOp>2609.82</vICMSOp>'.
      p_xml = p_xml && '<pDif>100.00</pDif>'.
      p_xml = p_xml && '<vICMSDif>2609.82</vICMSDif>'.
      p_xml = p_xml && '<vICMS>0.00</vICMS>'.
      p_xml = p_xml && '</ICMS51>'.
      p_xml = p_xml && '</ICMS>'.
      p_xml = p_xml && '<PIS>'.
      p_xml = p_xml && '<PISOutr>'.
      p_xml = p_xml && '<CST>49</CST>'.
      p_xml = p_xml && '<vBC>0.00</vBC>'.
      p_xml = p_xml && '<pPIS>0.00</pPIS>'.
      p_xml = p_xml && '<vPIS>0.00</vPIS>'.
      p_xml = p_xml && '</PISOutr>'.
      p_xml = p_xml && '</PIS>'.
      p_xml = p_xml && '<COFINS>'.
      p_xml = p_xml && '<COFINSOutr>'.
      p_xml = p_xml && '<CST>49</CST>'.
      p_xml = p_xml && '<vBC>0.00</vBC>'.
      p_xml = p_xml && '<pCOFINS>0.00</pCOFINS>'.
      p_xml = p_xml && '<vCOFINS>0.00</vCOFINS>'.
      p_xml = p_xml && '</COFINSOutr>'.
      p_xml = p_xml && '</COFINS>'.
      p_xml = p_xml && '</imposto>'.
      p_xml = p_xml && '<infAdProd>Operação NFF Venda de milho para indústria ou comércio operação interna CFOP 5.101 DIFERIDO'.
      p_xml = p_xml && '</infAdProd>'.
      p_xml = p_xml && '</det>'.
      p_xml = p_xml && '<det nItem="2">'.
      p_xml = p_xml && '<prod>'.
      p_xml = p_xml && '<cProd>002</cProd>'.
      p_xml = p_xml && '<cEAN>SEM GTIN</cEAN>'.
      p_xml = p_xml && '<xProd>Milho</xProd>'.
      p_xml = p_xml && '<NCM>10059090</NCM>'.
      p_xml = p_xml && '<cBenef>PR830060</cBenef>'.
      p_xml = p_xml && '<CFOP>5101</CFOP>'.
      p_xml = p_xml && '<uCom>Kg</uCom>'.
      p_xml = p_xml && '<qCom>22760.0000</qCom>'.
      p_xml = p_xml && '<vUnCom>0.9666</vUnCom>'.
      p_xml = p_xml && '<vProd>21999.82</vProd>'.
      p_xml = p_xml && '<cEANTrib>SEM GTIN</cEANTrib>'.
      p_xml = p_xml && '<uTrib>Kg</uTrib>'.
      p_xml = p_xml && '<qTrib>22760.0000</qTrib>'.
      p_xml = p_xml && '<vUnTrib>0.9666</vUnTrib>'.
      p_xml = p_xml && '<indTot>1</indTot>'.
      p_xml = p_xml && '<infProdNFF>'.
      p_xml = p_xml && '<cProdFisco>001110070-0198</cProdFisco>'.
      p_xml = p_xml && '<cOperNFF>63</cOperNFF>'.
      p_xml = p_xml && '</infProdNFF>'.
      p_xml = p_xml && '</prod>'.
      p_xml = p_xml && '<imposto>'.
      p_xml = p_xml && '<ICMS>'.
      p_xml = p_xml && '<ICMS51>'.
      p_xml = p_xml && '<orig>0</orig>'.
      p_xml = p_xml && '<CST>51</CST>'.
      p_xml = p_xml && '<modBC>3</modBC>'.
      p_xml = p_xml && '<pRedBC>0.00</pRedBC>'.
      p_xml = p_xml && '<vBC>21999.81</vBC>'.
      p_xml = p_xml && '<pICMS>12.00</pICMS>'.
      p_xml = p_xml && '<vICMSOp>2639.97</vICMSOp>'.
      p_xml = p_xml && '<pDif>100.00</pDif>'.
      p_xml = p_xml && '<vICMSDif>2639.97</vICMSDif>'.
      p_xml = p_xml && '<vICMS>0.00</vICMS>'.
      p_xml = p_xml && '</ICMS51>'.
      p_xml = p_xml && '</ICMS>'.
      p_xml = p_xml && '<PIS>'.
      p_xml = p_xml && '<PISOutr>'.
      p_xml = p_xml && '<CST>49</CST>'.
      p_xml = p_xml && '<vBC>0.00</vBC>'.
      p_xml = p_xml && '<pPIS>0.00</pPIS>'.
      p_xml = p_xml && '<vPIS>0.00</vPIS>'.
      p_xml = p_xml && '</PISOutr>'.
      p_xml = p_xml && '</PIS>'.
      p_xml = p_xml && '<COFINS>'.
      p_xml = p_xml && '<COFINSOutr>'.
      p_xml = p_xml && '<CST>49</CST>'.
      p_xml = p_xml && '<vBC>0.00</vBC>'.
      p_xml = p_xml && '<pCOFINS>0.00</pCOFINS>'.
      p_xml = p_xml && '<vCOFINS>0.00</vCOFINS>'.
      p_xml = p_xml && '</COFINSOutr>'.
      p_xml = p_xml && '</COFINS>'.
      p_xml = p_xml && '</imposto>'.
      p_xml = p_xml && '<infAdProd>Operação NFF Venda de milho para indústria ou comércio operação interna CFOP 5.101 DIFERIDO'.
      p_xml = p_xml && '</infAdProd>'.
      p_xml = p_xml && '</det>'.
      p_xml = p_xml && '<total>'.
      p_xml = p_xml && '<ICMSTot>'.
      p_xml = p_xml && '<vBC>43748.31</vBC>'.
      p_xml = p_xml && '<vICMS>0.00</vICMS>'.
      p_xml = p_xml && '<vICMSDeson>0.00</vICMSDeson>'.
      p_xml = p_xml && '<vFCP>0.00</vFCP>'.
      p_xml = p_xml && '<vBCST>0.00</vBCST>'.
      p_xml = p_xml && '<vST>0.00</vST>'.
      p_xml = p_xml && '<vFCPST>0.00</vFCPST>'.
      p_xml = p_xml && '<vFCPSTRet>0.00</vFCPSTRet>'.
      p_xml = p_xml && '<vProd>43748.32</vProd>'.
      p_xml = p_xml && '<vFrete>0.00</vFrete>'.
      p_xml = p_xml && '<vSeg>0.00</vSeg>'.
      p_xml = p_xml && '<vDesc>0.00</vDesc>'.
      p_xml = p_xml && '<vII>0.00</vII>'.
      p_xml = p_xml && '<vIPI>0.00</vIPI>'.
      p_xml = p_xml && '<vIPIDevol>0.00</vIPIDevol>'.
      p_xml = p_xml && '<vPIS>0.00</vPIS>'.
      p_xml = p_xml && '<vCOFINS>0.00</vCOFINS>'.
      p_xml = p_xml && '<vOutro>0.00</vOutro>'.
      p_xml = p_xml && '<vNF>43748.32</vNF>'.
      p_xml = p_xml && '</ICMSTot>'.
      p_xml = p_xml && '</total>'.
      p_xml = p_xml && '<transp>'.
      p_xml = p_xml && '<modFrete>1</modFrete>'.
      p_xml = p_xml && '<transporta>'.
      p_xml = p_xml && '<CNPJ>54881059000165</CNPJ>'.
      p_xml = p_xml && '<xNome>ARIEL E FILHOS TRANSPORTE LTDA</xNome>'.
      p_xml = p_xml && '<IE>9106898607</IE>'.
      p_xml = p_xml && '<UF>PR</UF>'.
      p_xml = p_xml && '</transporta>'.
      p_xml = p_xml && '<veicTransp>'.
      p_xml = p_xml && '<placa>QAY8G24</placa>'.
      p_xml = p_xml && '</veicTransp>'.
      p_xml = p_xml && '</transp>'.
      p_xml = p_xml && '<pag>'.
      p_xml = p_xml && '<detPag>'.
      p_xml = p_xml && '<tPag>99</tPag>'.
      p_xml = p_xml && '<xPag>Regime NFF</xPag>'.
      p_xml = p_xml && '<vPag>43748.32</vPag>'.
      p_xml = p_xml && '</detPag>'.
      p_xml = p_xml && '</pag>'.
      p_xml = p_xml && '<infAdic>'.
      p_xml = p_xml && '<infCpl>xBenef Diferimento previsto no item 53 do caput do art. 31 do Anexo VIII do RICMS/2017</infCpl>'.
      p_xml = p_xml && '</infAdic>'.
      p_xml = p_xml && '            '.
      p_xml = p_xml && '</infNFe>'.
      p_xml = p_xml && '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">'.
      p_xml = p_xml && '<SignedInfo>'.
      p_xml = p_xml && '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />'.
      p_xml = p_xml && '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />'.
      p_xml = p_xml && '<Reference URI="#NFe41240600050091760925551242706200013467262754">'.
      p_xml = p_xml && '<Transforms>'.
      p_xml = p_xml && '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />'.
      p_xml = p_xml && '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />'.
      p_xml = p_xml && '</Transforms>'.
      p_xml = p_xml && '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />'.
      p_xml = p_xml && '<DigestValue>/xp9EzouNdDKV3clrOWP2MDyNB4=</DigestValue>'.
      p_xml = p_xml && '</Reference>'.
      p_xml = p_xml && '</SignedInfo>'.
      p_xml = p_xml && '            '.
      p_xml = p_xml && '</Signature>'.
      p_xml = p_xml && '</NFe>'.
      p_xml = p_xml && '<protNFe versao="4.00" xmlns="http://www.portalfiscal.inf.br/nfe">'.
      p_xml = p_xml && '<infProt>'.
      p_xml = p_xml && '<tpAmb>1</tpAmb>'.
      p_xml = p_xml && '<verAplic>SVRS2405142121DR</verAplic>'.
      p_xml = p_xml && '<chNFe>41240600050091760925551242706200013467262754</chNFe>'.
      p_xml = p_xml && '<dhRecbto>2024-06-27T09:30:54-03:00</dhRecbto>'.
      p_xml = p_xml && '<nProt>241240002941228</nProt>'.
      p_xml = p_xml && '<digVal>/xp9EzouNdDKV3clrOWP2MDyNB4=</digVal>'.
      p_xml = p_xml && '<cStat>100</cStat>'.
      p_xml = p_xml && '<xMotivo>Autorizado o uso da NF-e</xMotivo>'.
      p_xml = p_xml && '</infProt>'.
      p_xml = p_xml && '</protNFe>'.
      p_xml = p_xml && '</nfeProc>'.
    WHEN OTHERS.
  ENDCASE.


ENDFORM.

FORM f_get_stvarv.

  SELECT *
    FROM tvarvc INTO TABLE @DATA(lit_status_replace_edoc)
   WHERE name = 'DRC_EDOC_STATUS_REPLACE_XML'.

  LOOP AT lit_status_replace_edoc INTO DATA(lwa_status_replace).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_status_replace-low ) TO gra_status_replace_edoc.
  ENDLOOP.


ENDFORM.
