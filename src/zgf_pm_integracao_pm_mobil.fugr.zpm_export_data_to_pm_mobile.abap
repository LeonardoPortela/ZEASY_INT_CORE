FUNCTION zpm_export_data_to_pm_mobile.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(VIEW) TYPE  STRING
*"     REFERENCE(STATUS) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(FILIAL) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(MES_INICIO) TYPE  CHAR02 OPTIONAL
*"     REFERENCE(MES_FIM) TYPE  CHAR02 OPTIONAL
*"     REFERENCE(ANO) TYPE  GJAHR OPTIONAL
*"     REFERENCE(TP_OBJECT) TYPE  EQART OPTIONAL
*"     REFERENCE(CT_EQPTO) TYPE  EQTYP OPTIONAL
*"     REFERENCE(EQPTO) TYPE  EQUNR OPTIONAL
*"     REFERENCE(MODELO) TYPE  TYPBZ OPTIONAL
*"     REFERENCE(USER_AD) TYPE  ZDE_USUARIO OPTIONAL
*"     REFERENCE(UPDATE_AT) TYPE  TIMESTAMPL OPTIONAL
*"     REFERENCE(CREATE_AT) TYPE  TIMESTAMPL OPTIONAL
*"     REFERENCE(BODY) TYPE  STRING OPTIONAL
*"     REFERENCE(USUARIO_ORC_ORDER) TYPE  USALIAS OPTIONAL
*"     REFERENCE(MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(MAKTX) TYPE  MAKTX OPTIONAL
*"     REFERENCE(WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(PERNR) TYPE  PERSNO OPTIONAL
*"     REFERENCE(ORDEM) TYPE  AUFNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  STRING
*"----------------------------------------------------------------------

  TYPES:
    BEGIN OF ty_return,
      aufnr    TYPE aufnr,
      mensagem TYPE string,
    END OF ty_return,

    BEGIN OF ty_estorno,
      rueck     TYPE co_rueck,
      rmzhl     TYPE co_rmzhl,
      estornado TYPE c,
      msg_erro  TYPE bapi_msg,
    END OF ty_estorno,

    tt_str_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY,

    BEGIN OF ts_data,
      matricula                TYPE string,
      data                     TYPE string,
      inicio_jornada           TYPE beguz,
      inicio_intervalo         TYPE pabeg,
      fim_intervalo            TYPE paend,
      fim_jornada              TYPE enduz,
      total_jornada_registrada TYPE string,
      total_jornada_esperada   TYPE string,
      hora_calculo             TYPE string,
      batida                   TYPE tt_str_tab,
    END OF ts_data,

    BEGIN OF ty_jornada,
      matricula(8)          TYPE  c,
      nome(80)              TYPE  c,
      cod_cargo(8)          TYPE  c,
      desc_cargo(40)        TYPE  c,
      cod_filial(4)         TYPE  c,
      desc_filial(30)       TYPE  c,
      situacao(40)          TYPE  c,
      data_saida(10)        TYPE  c,
      cod_motivo_saida(2)   TYPE  c,
      desc_motivo_saida(30) TYPE  c,
      jornada               TYPE retext,
    END OF ty_jornada.

  TYPES: ty_t352 TYPE RANGE OF t352c-rbnr.

  DATA: i_data_de TYPE sy-datum.
  DATA: i_data_ate TYPE sy-datum.
  DATA: _data TYPE p DECIMALS 2.

  DATA: data_de  TYPE sy-datum,
        data_ate TYPE sy-datum.

  DATA: _date_interval TYPE rsis_t_range.".

  DATA:t_usermd         TYPE STANDARD TABLE OF  rgsb4,
       _create_at       TYPE timestampl,
       date             TYPE sy-datum,
       time             TYPE sy-uzeit,
       lt_ordem         TYPE TABLE OF ztpm_orc_ordem,
       lw_return        TYPE bapiret2,
       lt_return        TYPE TABLE OF bapiret2,
       lv_ok            TYPE c,
       gt_methods       TYPE TABLE OF bapi_alm_order_method,
       gt_header        TYPE TABLE OF bapi_alm_order_headers_i,
       lt_ordem_man     TYPE TABLE OF ztpm_d_m_ordem,
       lt_return_lib    TYPE TABLE OF ty_return,
       lt_componentes   TYPE TABLE OF bapi_alm_order_component_e,
       lt_mensagem      TYPE ztpm_mensagem,
       lt_component2    TYPE TABLE OF bapi_alm_order_component,
       lt_operations    TYPE TABLE OF bapi_alm_order_operation_e,
       lt_operations2   TYPE TABLE OF bapi_alm_order_operation,
       lt_consult_mat   TYPE TABLE OF ztpm_d_m_material,
       lt_nota          TYPE TABLE OF zepm_d_nota,
       lv_status        TYPE bapi2080_notadt-systatus,
       lw_data          TYPE bapi2080_notsti,
       lt_perfil        TYPE TABLE OF ztpm_p_perfcat,
       lt_retorno_nota  TYPE TABLE OF ztpm_retorno_nota,
       ls_retorno_nota  TYPE ztpm_retorno_nota,
       lt_obrig         TYPE TABLE OF zepm_campos_obrigatorios,
       lt_nota_autom    TYPE TABLE OF ztpm_par_t_nota_aut,
       lt_solic_mat     TYPE TABLE OF ztpm_d_m_material,
       lt_solic_mat_ret TYPE TABLE OF ztpm_transa_vw_solic_mat,
       lt_component     TYPE TABLE OF bapi_alm_order_component,
       lv_aufnr         TYPE ztpm_d_m_ordem-aufnr,
       ls_retorno_ordem TYPE ztpm_transa_exp_ordem,
       lt_atrib_ordem   TYPE ztpm_atrib_nota_ordem,
       lt_ordem_orc     TYPE ztpm_t_orc_ordem,
       lv_id            TYPE belnr_d,
       lv_prox_aprov    TYPE string,
       lv_bname         TYPE xubname,
       lt_estorno       TYPE TABLE OF ty_estorno,
       lt_olist         TYPE TABLE OF bapi_alm_order_objectlist,
       lt_jornada       TYPE TABLE OF ty_jornada.

  CLEAR: _create_at.
  FREE: t_usermd.

  IF create_at IS NOT INITIAL.
*    _create_at = create_at.
*    REPLACE ALL OCCURRENCES OF '-' IN _create_at WITH space.
*    REPLACE ALL OCCURRENCES OF '-' IN _create_at WITH space.
*    REPLACE ALL OCCURRENCES OF '/' IN _create_at WITH space.

    CALL FUNCTION 'Z_CONV_TIMESTAMP_MIL_TO_DATE'
      EXPORTING
        i_date = create_at  " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)
      IMPORTING
        e_date = date
        e_time = time.

    i_data_de = date.
    i_data_ate = sy-datum.
  ELSE.
******  Pega data base para selação de dados no SET.
*           Buscando empresa TABELA SETLEAF.
*            CLEAR: _OBRA.
    SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
      WHERE setname EQ 'MAGI_PM_APP_MOBILE'
        AND valfrom EQ @filial.


    IF i_data IS NOT INITIAL.

      SELECT SINGLE *
        FROM setlinet
        INTO  @DATA(ls_setline)
        WHERE setclass = @i_data-setclass
          AND subclass = @i_data-subclass
          AND setname  = @i_data-setname
          AND langu    = 'P'
          AND lineid   = @i_data-lineid.
*      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*        EXPORTING
*          class           = '0000'
*          setnr           = 'MAGI_PM_APP_MOBILE'
*          no_descriptions = ' '
*        TABLES
*          set_values      = t_usermd
*        EXCEPTIONS
*          set_not_found   = 1
*          OTHERS          = 2.

      IF ls_setline IS NOT INITIAL.
*        READ TABLE lt_setline INTO DATA(wa_usermd) WITH KEY from = filial.
*        IF sy-subrc  EQ 0.
        _data = CONV #( ls_setline-descript ). "Periodo em dias.
        i_data_de = sy-datum - _data.
        i_data_ate = sy-datum.
      ELSE.
        _data = 360. "Periodo de 360 dias.
        i_data_de = sy-datum - _data.
        i_data_ate = sy-datum.
      ENDIF.
*      ENDIF.
    ELSE.
      _data = 360. "Periodo de 360 dias.
      i_data_de = sy-datum - _data.
      i_data_ate = sy-datum.
    ENDIF.
  ENDIF.

  data_de   = i_data_de.
  data_ate  = i_data_ate.


  APPEND VALUE #( sign = 'I' option = 'EQ'  low = i_data_de high = i_data_ate ) TO _date_interval.

  _date_interval = cl_main_app=>get_date_range(
    from = data_de
    to   = data_ate
  ).

  DATA(_main_app) = NEW cl_main_app( ).

  DATA(_view) = |{ view CASE = LOWER }|.
*
  IF usuario_orc_order IS NOT INITIAL.

    SELECT bname
      FROM usrefus
      UP TO 1 ROWS
      INTO lv_bname
      WHERE useralias = usuario_orc_order.
    ENDSELECT.

  ENDIF.

  CASE _view.
    WHEN 'vw_m_equipamento'.
*      "//ZPM_EXPORT_M_VEIC_TO_PM_MOBILE

      CALL METHOD _main_app->get_veiculos
        EXPORTING
          filial   = filial
          situacao = status
        RECEIVING
          veiculos = DATA(_veiculos).
      "PM-14.03.19 - Projeto PM_MOBILE [CS2019*531]- AO
      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _veiculos ).

    WHEN 'vw_m_local'.
*      "//ZPM_EXPORT_M_VEIC_TO_PM_MOBILE

      CALL METHOD _main_app->get_local
        EXPORTING
          filial   = filial
          situacao = status
        RECEIVING
          local    = DATA(_local).
      "PM-14.03.19 - Projeto PM_MOBILE [CS2019*531]- AO
      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _local ).

    WHEN 'vw_m_catalogo'.
      "//ZPM_EXPORT_CAT_TO_PM_MOBILE

      CALL METHOD _main_app->get_catalogo
        RECEIVING
          catalogo = DATA(_catalogo).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _catalogo ).

    WHEN 'vw_p_nota'.
      "//ZPM_EXPORT_T_NOTA_TO_PM_MOBILE

      CALL METHOD _main_app->get_tipo_notas
        RECEIVING
          tip_nota = DATA(_tipo_nota).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _tipo_nota ).

    WHEN 'vw_p_ordem'.
      "//ZPM_EXPORT_T_ORDE_TO_PM_MOBILE
*
      CALL METHOD _main_app->get_tipo_ordem
        RECEIVING
          tip_ordem = DATA(_tipo_ordem).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _tipo_ordem ).

    WHEN 'vw_ordem_centro'.
      "//ZPM_EXPORT_P_ORDE_TO_PM_MOBILE
*
      CALL METHOD _main_app->get_p_ordem
        EXPORTING
          filial  = filial
        RECEIVING
          p_ordem = DATA(_p_ordem).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _p_ordem ).

    WHEN 'vw_p_atividade'.
      "//ZPM_EXPORT_T_ATIV_TO_PM_MOBILE
*
      CALL METHOD _main_app->get_tipo_atividade
        RECEIVING
          tip_atividade = DATA(_tipo_ativ).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _tipo_ativ ).

    WHEN 'vw_p_prioridade'.
      "//ZPM_EXPORT_T_PRIO_TO_PM_MOBILE
*
      CALL METHOD _main_app->get_tipo_prior
        RECEIVING
          tip_prior = DATA(_tipo_prior).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _tipo_prior ).


    WHEN 'vw_p_grp'.
      "//ZPM_EXPORT_T_GRP_TO_PM_MOBILE
*
      CALL METHOD _main_app->get_tipo_grp
        EXPORTING
          filial  = filial
        RECEIVING
          tip_grp = DATA(_tipo_grp).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _tipo_grp ).


    WHEN 'vw_p_centro'.
      "//ZPM_EXPORT_T_CENT_TO_PM_MOBILE
*
      CALL METHOD _main_app->get_tipo_cent
        RECEIVING
          tip_cent = DATA(_tipo_cent).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _tipo_cent ).

    WHEN 'vw_p_ctrabalho'.
      "//ZPM_EXPORT_T_CTRA_TO_PM_MOBILE
      CALL METHOD _main_app->get_tipo_ctrab
        EXPORTING
          filial    = filial
        RECEIVING
          tip_ctrab = DATA(_tipo_ctrab).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _tipo_ctrab ).

    WHEN 'vw_d_ordem'.
*    "//ZPM_EXPORT_D_ORDEM_PM_MOBILE
      CALL METHOD _main_app->get_d_ordem
        EXPORTING
          filial    = filial
          create_at = _date_interval
          update_at = update_at
          ordem     = ordem
        RECEIVING
          d_ordem   = DATA(_d_ordem).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_ordem ).

    WHEN 'vw_d_nota'.
**    "//ZPM_EXPORT_D_NOTA_PM_MOBILE
      CALL METHOD _main_app->get_d_nota
        EXPORTING
          filial = filial
          data   = _date_interval
        RECEIVING
          d_nota = DATA(_d_nota).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_nota ).

    WHEN 'vw_d_status'.
      "//ZPM_EXPORT_D_STATUS_PM_MOBILE
      CALL METHOD _main_app->get_d_status
        RECEIVING
          d_status = DATA(_d_status).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_status ).

    WHEN 'vw_d_operacao_ordem'.
      "//ZPM_EXPORT_D_OPER_PM_MOBILE
      CALL METHOD _main_app->get_d_operacao
        EXPORTING
          filial     = filial
          data       = _date_interval
        RECEIVING
          d_operacao = DATA(_d_operacao).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_operacao ).

    WHEN 'vw_d_usuario' OR 'vw_d_usuario_id'.
      "//ZPM_EXPORT_D_USUARIO_PM_MOBILE
      CALL METHOD _main_app->get_d_usuario
        EXPORTING
          filial    = filial
          user_ad   = user_ad
          pernr     = pernr
        RECEIVING
          d_usuario = DATA(_d_usuario).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_usuario ).

    WHEN 'vw_p_desvio'.

      "//ZPM_EXPORT_P_DESVIO_PM_MOBILE
      CALL METHOD _main_app->get_p_desvio
        EXPORTING
          filial   = filial
        RECEIVING
          p_desvio = DATA(_p_desvio).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _p_desvio ).

    WHEN 'vw_d_equipe'.

      "//ZPM_EXPORT_P_DESVIO_PM_MOBILE
      CALL METHOD _main_app->get_d_equipe
        RECEIVING
          d_equipe = DATA(_p_equipe).

      SORT _p_equipe BY centro.
      DELETE _p_equipe WHERE centro NE filial.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _p_equipe ).


    WHEN 'vw_d_apont'.

      "//ZPM_EXPORT_D_APONT_PM_MOBILE
      CALL METHOD _main_app->get_d_apont
        EXPORTING
          ordem   = ordem
        RECEIVING
          d_apont = DATA(_d_apont).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_apont ).


    WHEN 'vw_d_custo_eqpto'.
      "//Exporta dados de custo do eqpto
      CALL METHOD _main_app->get_custo_eqpto
        EXPORTING
          filial       = filial
          mes_inicio   = mes_inicio
          mes_fim      = mes_fim
          ano          = ano
          tp_object    = tp_object
          ct_eqpto     = ct_eqpto
          eqpto        = eqpto
          modelo       = modelo
        RECEIVING
          t_cust_eqpto = DATA(_d_cust_eqpto).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_cust_eqpto ).


    WHEN 'vw_custo_modelo'.
      "//Exporta dados de custo do eqpto
      CALL METHOD _main_app->get_custo_eqpto
        EXPORTING
          filial       = filial
          mes_inicio   = mes_inicio
          mes_fim      = mes_fim
          ano          = ano
          tp_object    = tp_object
          ct_eqpto     = ct_eqpto
          eqpto        = eqpto
          modelo       = modelo
        RECEIVING
          t_cust_eqpto = DATA(_d_cust_modelo).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_cust_modelo ).

    WHEN 'vw_custo_tp_objeto'.
      "//Exporta dados de custo do eqpto
      CALL METHOD _main_app->get_custo_eqpto
        EXPORTING
          filial       = filial
          mes_inicio   = mes_inicio
          mes_fim      = mes_fim
          ano          = ano
          tp_object    = tp_object
          ct_eqpto     = ct_eqpto
          eqpto        = eqpto
          modelo       = modelo
        RECEIVING
          t_cust_eqpto = DATA(_d_cust_categoria).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_cust_categoria ).

    WHEN 'vw_custo_manutencao'.
      "//Exporta dados de custo do eqpto
      CALL METHOD _main_app->get_custo_eqpto
        EXPORTING
          filial       = filial
          mes_inicio   = mes_inicio
          mes_fim      = mes_fim
          ano          = ano
          tp_object    = tp_object
          ct_eqpto     = ct_eqpto
          eqpto        = eqpto
          modelo       = modelo
        RECEIVING
          t_cust_eqpto = DATA(_d_cust_manutencao).

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_cust_manutencao ).

*** Inicio - Rubenilson Pereira - 23.06.2022 - CS2020000572 API Aprovação de Orçamento via APP - 81248
    WHEN 'vw_order_orc'.

      CALL FUNCTION 'ZPM_BUSCA_ORDENS_ORCAM'
        EXPORTING
          i_user       = lv_bname
        IMPORTING
          e_ordens_orc = lt_ordem_orc.


      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_ordem_orc ).

    WHEN 'vw_lib_orc_ordem'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_ordem ).

      LOOP AT lt_ordem ASSIGNING FIELD-SYMBOL(<fs_ordem>).

        CALL FUNCTION 'ZPM_APROVAR_PERMIT'
          EXPORTING
            i_aufnr      = <fs_ordem>-aufnr
            i_aprovador  = lv_bname
          IMPORTING
            return       = lw_return
            id_orcamento = lv_id
            e_prox_aprov = lv_prox_aprov.

        IF lv_id IS NOT INITIAL.
          <fs_ordem>-id = lv_id.
        ENDIF.

        IF lw_return-message IS NOT INITIAL.

          <fs_ordem>-mensagem = lw_return-message.
        ELSE.

          <fs_ordem>-mensagem = 'Não foi possível realizar a aprovação!'.
        ENDIF.

**  Begin of    #XXXXX  FF
        IF lw_return-type = 'S'.
          <fs_ordem>-success    = abap_true.
          <fs_ordem>-prox_aprov = lv_prox_aprov.
        ENDIF.
** End of FF

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_ordem ).

    WHEN 'vw_recusa_ordem'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_ordem ).

      LOOP AT lt_ordem ASSIGNING <fs_ordem>.

        CALL FUNCTION 'ZPM_REPROVAR_PERMIT'
          EXPORTING
            ordem       = <fs_ordem>-aufnr
            motivo      = <fs_ordem>-observacao
            usuario     = lv_bname
          IMPORTING
            ok          = lv_ok
            msg_retorno = <fs_ordem>-mensagem.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_ordem ).
*** Fim - Rubenilson Pereira - 23.06.2022 - CS2020000572 API Aprovação de Orçamento via APP - 81248

    WHEN 'vw_supl_ordem'.

*      CALL METHOD _main_app->get_orc_ordem
*        EXPORTING
*          usuario     = usuario_orc_order
*          supl_ordem  = abap_true
*        RECEIVING
*          t_orc_ordem = _d_orc_ordem.
*
*      result = zcl_fmcall_pm_mobile=>abap2json( abap_data  = _d_orc_ordem ).

    WHEN 'vw_lib_supl_ordem'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_ordem ).

      LOOP AT lt_ordem ASSIGNING <fs_ordem>.

        CALL FUNCTION 'ZPM_APROVAR_SUPLEMENTACAO'
          EXPORTING
            ordem        = <fs_ordem>-aufnr
            usuario      = lv_bname
          IMPORTING
            id_orcamento = lv_id
            e_prox_aprov = lv_prox_aprov
          TABLES
            errors       = lt_return.

        <fs_ordem>-id = lv_id.
        IF lt_return IS INITIAL.
          <fs_ordem>-mensagem = 'Suplementação liberada!'.

**  Begin of    #XXXXX  FF
          <fs_ordem>-success    = abap_true.
          <fs_ordem>-prox_aprov = lv_prox_aprov.
** End of FF

        ENDIF.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_ordem ).

    WHEN 'vw_rej_supl_ordem'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_ordem ).

      LOOP AT lt_ordem ASSIGNING <fs_ordem>.

        CALL FUNCTION 'ZPM_REPROVAR_SUPLEMENTACAO'
          EXPORTING
            ordem       = <fs_ordem>-aufnr
            motivo      = <fs_ordem>-observacao
            usuario     = lv_bname
          IMPORTING
            ok          = lv_ok
            msg_retorno = <fs_ordem>-mensagem.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_ordem ).

    WHEN 'vw_libera_ordem'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_ordem_man ).

      LOOP AT lt_ordem_man ASSIGNING FIELD-SYMBOL(<fs_ordem_man>).

        APPEND VALUE #(
                            refnumber  = 1
                            objecttype = 'HEADER'
                            method     = 'RELEASE'
                            objectkey  = <fs_ordem_man>-aufnr
                          ) TO gt_methods.

        APPEND VALUE #(
                        refnumber  = 1
                        objecttype = ''
                        method     = 'SAVE'
                        objectkey  = <fs_ordem_man>-aufnr
                      ) TO gt_methods.

        APPEND VALUE #(
                        orderid =  <fs_ordem_man>-aufnr
                       ) TO gt_header.

        CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
          TABLES
            it_methods = gt_methods
            it_header  = gt_header
            return     = lt_return.

** Gravar logs da operação de liberação da ordem
        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          TABLES
            t_return = lt_return.

        IF NOT line_exists( lt_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          ls_retorno_ordem-sucess = abap_true.
          ls_retorno_ordem-data-nr_ordem = <fs_ordem_man>-aufnr.

          CONCATENATE 'Ordem' <fs_ordem_man>-aufnr 'liberada com sucesso' INTO ls_retorno_ordem-data-msg_ordem SEPARATED BY space.

          ls_retorno_ordem-data-status = '1'.

        ELSE.

          SORT lt_return BY type.

          READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return>) INDEX 1.
          IF sy-subrc IS INITIAL.

*            APPEND INITIAL LINE TO ls_retorno_ordem-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).

            MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number
               WITH <fs_return>-message_v1 <fs_return>-message_v2 INTO ls_retorno_ordem-erros .

            ls_retorno_ordem-data-nr_ordem = <fs_ordem_man>-aufnr.
          ENDIF.

        ENDIF.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_ordem ).

    WHEN 'vw_encerra_ordem'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_ordem_man ).

      LOOP AT lt_ordem_man ASSIGNING <fs_ordem_man>.

        APPEND VALUE #(
                            refnumber  = 1
                            objecttype = 'HEADER'
                            method     = 'TECOMPLETE'
                            objectkey  = <fs_ordem_man>-aufnr
                          ) TO gt_methods.

        APPEND VALUE #(
                        refnumber  = 1
                        objecttype = ''
                        method     = 'SAVE'
                        objectkey  = <fs_ordem_man>-aufnr
                      ) TO gt_methods.

        APPEND VALUE #(
                        orderid =  <fs_ordem_man>-aufnr
                       ) TO gt_header.

        CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
          TABLES
            it_methods = gt_methods
            it_header  = gt_header
            return     = lt_return.

** Gravar logs da operação de liberação da ordem
        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          TABLES
            t_return = lt_return.

        IF NOT line_exists( lt_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          ls_retorno_ordem-sucess = abap_true.
          ls_retorno_ordem-data-nr_ordem = <fs_ordem_man>-aufnr.

          CONCATENATE 'Ordem' <fs_ordem_man>-aufnr 'encerrada com sucesso' INTO ls_retorno_ordem-data-msg_ordem SEPARATED BY space.

          ls_retorno_ordem-data-status = '2'.

        ELSE.

          SORT lt_return BY type.

          READ TABLE lt_return ASSIGNING <fs_return> INDEX 1.
          IF sy-subrc IS INITIAL.

            MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number
               WITH <fs_return>-message_v1 <fs_return>-message_v2 INTO ls_retorno_nota-erros-msg_erros .

            ls_retorno_ordem-data-nr_ordem = <fs_ordem_man>-aufnr.

          ENDIF.

        ENDIF.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_ordem ).

    WHEN 'vw_elim_op_ordem'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_ordem_man ).

      LOOP AT lt_ordem_man ASSIGNING <fs_ordem_man>.

        REFRESH: lt_mensagem,
                 lt_operations2.

        IF <fs_ordem_man>-delete IS NOT INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = <fs_ordem_man>-aufnr
            IMPORTING
              output = <fs_ordem_man>-aufnr.


          CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
            EXPORTING
              number        = <fs_ordem_man>-aufnr
            TABLES
              et_components = lt_componentes
              et_operations = lt_operations
              return        = lt_return.

          IF lt_componentes IS NOT INITIAL.
            SORT lt_componentes BY activity.

            READ TABLE lt_componentes ASSIGNING FIELD-SYMBOL(<fs_componentes>)
            WITH KEY activity = <fs_ordem_man>-vornr.
            IF sy-subrc IS INITIAL.

              LOOP AT lt_componentes ASSIGNING <fs_componentes> FROM sy-tabix.
                IF <fs_ordem_man>-vornr <> <fs_componentes>-activity.
                  EXIT.
                ENDIF.

                IF <fs_componentes>-withd_quan <> 0.

                  APPEND INITIAL LINE TO lt_mensagem ASSIGNING FIELD-SYMBOL(<fs_mensagem>).
                  <fs_mensagem>-mensagem = TEXT-e01.
                  <fs_ordem_man>-mensagem = lt_mensagem.
                  EXIT.
                ELSEIF <fs_componentes>-deliv_qty <> 0.

                  APPEND INITIAL LINE TO lt_mensagem ASSIGNING <fs_mensagem>.
                  <fs_mensagem>-mensagem = TEXT-e01.
                  <fs_ordem_man>-mensagem = lt_mensagem.
                  EXIT.

                ELSEIF <fs_componentes>-purchase_order_exists IS NOT INITIAL.

                  APPEND INITIAL LINE TO lt_mensagem ASSIGNING <fs_mensagem>.
                  <fs_mensagem>-mensagem = TEXT-e01.
                  <fs_ordem_man>-mensagem = lt_mensagem.
                  EXIT.

                ELSE.

                  SELECT SINGLE frgzu
                    FROM eban
                    INTO @DATA(lv_frgzu)
                    WHERE banfn = @<fs_componentes>-preq_no
                      AND bnfpo = @<fs_componentes>-preq_item.
                  IF sy-subrc IS INITIAL.

                    IF lv_frgzu IS NOT INITIAL.

                      APPEND INITIAL LINE TO lt_mensagem ASSIGNING <fs_mensagem>.
                      <fs_mensagem>-mensagem = TEXT-e01.
                      <fs_ordem_man>-mensagem = lt_mensagem.
                      EXIT.

                    ENDIF.

                  ENDIF.

                ENDIF.

              ENDLOOP.

            ENDIF.

          ENDIF.

          IF lt_mensagem IS INITIAL.


            APPEND VALUE #(
                                refnumber  = 1
                                objecttype = 'OPERATION'
                                method     = 'DELETE'
                                objectkey  = <fs_ordem_man>-aufnr
                              ) TO gt_methods.

            APPEND VALUE #(
                            refnumber  = 1
                            objecttype = ''
                            method     = 'SAVE'
                            objectkey  = <fs_ordem_man>-aufnr
                          ) TO gt_methods.

            APPEND VALUE #(
                            orderid =  <fs_ordem_man>-aufnr
                           ) TO gt_header.

            READ TABLE lt_operations ASSIGNING FIELD-SYMBOL(<fs_operations>)
            WITH KEY activity = <fs_ordem_man>-vornr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              APPEND INITIAL LINE TO lt_operations2 ASSIGNING FIELD-SYMBOL(<fs_operations2>).
              MOVE-CORRESPONDING <fs_operations> TO <fs_operations2>.

            ENDIF.

            CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
              TABLES
                it_methods   = gt_methods
                it_header    = gt_header
                it_operation = lt_operations2
                return       = lt_return.

** Gravar logs da operação de liberação da ordem
            CALL FUNCTION 'Z_GRAVA_LOG_PM'
              TABLES
                t_return = lt_return.

            IF NOT line_exists( lt_return[ type = 'E' ] ).

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              <fs_ordem_man>-delete = abap_true.

            ELSE.

              CLEAR <fs_ordem_man>-delete.

            ENDIF.

            LOOP AT lt_return ASSIGNING <fs_return>.

              APPEND INITIAL LINE TO lt_mensagem ASSIGNING <fs_mensagem>.
              <fs_mensagem>-mensagem = <fs_return>-message.

            ENDLOOP.

            IF lt_mensagem IS NOT INITIAL.
              <fs_ordem_man>-mensagem = lt_mensagem.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_ordem_man ).

    WHEN 'vw_consult_mat'.

      CALL METHOD _main_app->get_consult_mat
        EXPORTING
          matnr     = matnr
          maktx     = maktx
          werks     = werks
        RECEIVING
          t_consult = lt_consult_mat.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_consult_mat ).

    WHEN 'vw_libera_nota'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_nota ).

      LOOP AT lt_nota ASSIGNING FIELD-SYMBOL(<fs_nota>).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_nota>-qmnum
          IMPORTING
            output = <fs_nota>-qmnum.

        CALL FUNCTION 'BAPI_ALM_NOTIF_PUTINPROGRESS'
          EXPORTING
            number       = <fs_nota>-qmnum
          IMPORTING
            systemstatus = lv_status
          TABLES
            return       = lt_return.
        IF lt_return IS INITIAL.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

          ls_retorno_nota-sucess = abap_true.
          ls_retorno_nota-data-nr_nota = <fs_nota>-qmnum.

          CONCATENATE 'Nota' <fs_nota>-qmnum 'liberada com sucesso' INTO ls_retorno_nota-data-msg_nota SEPARATED BY space.

          ls_retorno_nota-data-status = '1'.

        ELSE.

          SORT lt_return BY type.

          READ TABLE lt_return ASSIGNING <fs_return> INDEX 1.
          IF sy-subrc IS INITIAL.

            MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number
               WITH <fs_return>-message_v1 <fs_return>-message_v2 INTO ls_retorno_nota-erros-msg_erros .

            ls_retorno_nota-data-nr_nota = <fs_nota>-qmnum.
          ENDIF.

        ENDIF.

        CLEAR lv_status.
        REFRESH lt_return.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_nota ).

    WHEN 'vw_encerra_nota'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_nota ).

      LOOP AT lt_nota ASSIGNING <fs_nota>.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_nota>-qmnum
          IMPORTING
            output = <fs_nota>-qmnum.

        lw_data-langu = sy-langu.
        lw_data-refdate = sy-datum.
        lw_data-reftime = sy-uzeit.

        CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
          EXPORTING
            number       = <fs_nota>-qmnum
            syststat     = lw_data
          IMPORTING
            systemstatus = lv_status
          TABLES
            return       = lt_return.
        IF lt_return IS INITIAL.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

          ls_retorno_nota-sucess = abap_true.
          ls_retorno_nota-data-nr_nota = <fs_nota>-qmnum.

          CONCATENATE 'Nota' <fs_nota>-qmnum 'encerrada com sucesso' INTO ls_retorno_nota-data-msg_nota SEPARATED BY space.

          ls_retorno_nota-data-status = '2'.

        ELSE.

          SORT lt_return BY type.

          READ TABLE lt_return ASSIGNING <fs_return> INDEX 1.
          IF sy-subrc IS INITIAL.

            MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number
               WITH <fs_return>-message_v1 <fs_return>-message_v2 INTO ls_retorno_nota-erros-msg_erros .

            ls_retorno_nota-data-nr_nota = <fs_nota>-qmnum.

          ENDIF.

        ENDIF.

        CLEAR lv_status.
        REFRESH lt_return.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_nota ).

    WHEN 'vw_perfil_usuario'.

      DATA(lr_rbnr) = VALUE ty_t352(
        ( sign = 'I' option = 'CP' low = 'PM*' )
        ( sign = 'I' option = 'CP' low = 'SPM*' )
        ).

      SELECT *
        FROM t352c
        INTO TABLE @DATA(lt_t352c)
        WHERE rbnr IN @lr_rbnr.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_t352c ASSIGNING FIELD-SYMBOL(<fs_352c>).

          APPEND INITIAL LINE TO lt_perfil ASSIGNING FIELD-SYMBOL(<fs_perfil>).

          MOVE-CORRESPONDING <fs_352c> TO <fs_perfil>.

          <fs_perfil>-id = <fs_352c>-rbnr.

        ENDLOOP.
      ENDIF.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_perfil ).
      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_perfil ).


    WHEN 'vw_campo_obrigatorio'.

      SELECT *
        FROM zpmt0064
        INTO TABLE @DATA(lt_zpmt0064).
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING lt_zpmt0064 TO lt_obrig.

        result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_obrig ).
      ENDIF.


    WHEN 'vw_nota_autom'.

      SELECT auart,
             notdat,
             qmart
        FROM t350
        INTO TABLE @DATA(lt_t350).
      IF sy-subrc IS INITIAL.

        LOOP AT lt_t350 ASSIGNING FIELD-SYMBOL(<fs_t350>).

          APPEND INITIAL LINE TO lt_nota_autom ASSIGNING FIELD-SYMBOL(<fs_nota_autom>).
          MOVE-CORRESPONDING <fs_t350> TO <fs_nota_autom>.
          <fs_nota_autom>-id = <fs_t350>-auart.

          IF <fs_t350>-auart  IS NOT INITIAL AND
             <fs_t350>-notdat IS NOT INITIAL AND
             <fs_t350>-qmart IS NOT INITIAL.

            <fs_nota_autom>-zcria = '1'.

          ELSE.

            <fs_nota_autom>-zcria = '0'.

          ENDIF.

        ENDLOOP.

        result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_nota_autom ).
      ENDIF.

    WHEN 'vw_solic_mat'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_solic_mat ).

      READ TABLE lt_solic_mat ASSIGNING FIELD-SYMBOL(<fs_solic_mat>) INDEX 1.
      IF sy-subrc IS INITIAL.

        lv_aufnr = <fs_solic_mat>-aufnr.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_aufnr
          IMPORTING
            output = lv_aufnr.

        CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
          EXPORTING
            number        = lv_aufnr
          TABLES
            et_operations = lt_operations
            return        = lt_return.
        IF sy-subrc IS INITIAL.
          SORT lt_operations BY activity.
        ENDIF.

      ENDIF.

      LOOP AT lt_solic_mat ASSIGNING <fs_solic_mat>.

        READ TABLE lt_operations ASSIGNING <fs_operations>
        WITH KEY activity = <fs_solic_mat>-vornr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          IF <fs_operations>-complete IS NOT INITIAL.
            <fs_solic_mat>-mensagem = 'Operação já está completa, não é permitido inserir material!'.
            CONTINUE.
          ENDIF.
        ENDIF.

        REFRESH: gt_methods,
                 gt_header,
                 lt_component,
                 lt_return.

        APPEND VALUE #(
                         refnumber  = 1
                         objecttype = 'COMPONENT'
                         method     = 'CREATE'
                         objectkey  = <fs_solic_mat>-aufnr
                       ) TO gt_methods.

        APPEND VALUE #(
                        refnumber  = 1
                        objecttype = ''
                        method     = 'SAVE'
                        objectkey  = <fs_solic_mat>-aufnr
                      ) TO gt_methods.

        APPEND VALUE #(
                orderid =  <fs_solic_mat>-aufnr
               ) TO gt_header.

        APPEND INITIAL LINE TO lt_component ASSIGNING FIELD-SYMBOL(<fs_component>).

        <fs_component>-activity             = <fs_solic_mat>-vornr.
        <fs_component>-material             = <fs_solic_mat>-matnr.
        <fs_component>-stge_loc             = <fs_solic_mat>-lgort.
        <fs_component>-requirement_quantity = <fs_solic_mat>-menge.
        <fs_component>-req_date             = <fs_solic_mat>-bdter.
        <fs_component>-unload_pt            = <fs_solic_mat>-ablad.
        <fs_component>-gr_rcpt              = <fs_solic_mat>-wempf.

        CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
          TABLES
            it_methods   = gt_methods
            it_header    = gt_header
            it_operation = lt_operations2
            it_component = lt_component
            return       = lt_return.

        IF NOT line_exists( lt_return[ type = 'E' ] ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          <fs_solic_mat>-mensagem = 'Material incluído com sucesso!'.

**  Begin of    #106408   FF  10.03.2023

**  Begin of    #106408   FF  20.03.2023
          IF lt_solic_mat[] IS NOT INITIAL.
            SELECT * FROM resb
              INTO TABLE @DATA(lt_resb)
              FOR ALL ENTRIES IN @lt_solic_mat
              WHERE aufnr = @lt_solic_mat-aufnr
                AND werks = @lt_solic_mat-werks
                AND matnr = @lt_solic_mat-matnr.
          ENDIF.
**  End of    #106408   FF  20.03.2023

          SORT lt_resb BY matnr rspos DESCENDING.

          LOOP AT lt_component ASSIGNING FIELD-SYMBOL(<fs_comp>).
            APPEND INITIAL LINE TO lt_solic_mat_ret ASSIGNING FIELD-SYMBOL(<fs_solic_mat_ret>).

            <fs_solic_mat_ret>-sucess         = abap_true.
            <fs_solic_mat_ret>-data-nr_ordem  = <fs_solic_mat>-aufnr.
            <fs_solic_mat_ret>-data-matnr     = <fs_comp>-material.

            READ TABLE lt_resb ASSIGNING FIELD-SYMBOL(<fs_resb>) WITH KEY matnr = <fs_comp>-material BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_solic_mat_ret>-data-reserv_no = <fs_resb>-rsnum.
              <fs_solic_mat_ret>-data-res_item  = <fs_resb>-rspos.
            ENDIF.

          ENDLOOP.
** End of FF  10.03.2023

        ELSE.

          SORT lt_return BY type.

          READ TABLE lt_return ASSIGNING <fs_return> INDEX 1.
          IF sy-subrc IS INITIAL.
**  Begin of    #106408   FF  10.03.2023
            APPEND INITIAL LINE TO lt_solic_mat_ret ASSIGNING <fs_solic_mat_ret>.

*            MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number WITH <fs_return>-message_v1
*                                                                                           <fs_return>-message_v2
*                                                                                           <fs_return>-message_v3
*                                                                                           <fs_return>-message_v4 INTO <fs_solic_mat>-mensagem.
            MESSAGE ID <fs_return>-id TYPE <fs_return>-type NUMBER <fs_return>-number WITH <fs_return>-message_v1
                                                                                           <fs_return>-message_v2
                                                                                           <fs_return>-message_v3
                                                                                           <fs_return>-message_v4 INTO <fs_solic_mat_ret>-erros.
            <fs_solic_mat_ret>-data-nr_ordem  = <fs_solic_mat>-aufnr.

** End of FF  10.03.2023
          ENDIF.

        ENDIF.

      ENDLOOP.

*      result = zcl_fmcall_pm_mobile=>abap2json( abap_data  = lt_solic_mat ).
      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_solic_mat_ret )."  #106408  FF  10.03.2023

    WHEN 'vw_atribui_nota_ordem'.

      DATA: t_methods         TYPE bapi_alm_order_method_t,
            t_objectlist      TYPE TABLE OF bapi_alm_order_objectlist,
            lv_ordem          TYPE objidext,
            t_header          TYPE TABLE OF bapi_alm_order_headers_i,
            ls_header         TYPE bapi_alm_order_header_e,
            lt_oprol3         TYPE TABLE OF bapi_alm_olist_relation,
            lt_notas          TYPE zpmt0020_t,
            lv_refnumber      TYPE sy-tabix,
            lv_desvinc        TYPE c,
            lt_header         TYPE TABLE OF bapi_alm_order_headers_i,
            lv_excluir_header TYPE c,
            lt_methods        TYPE bapi_alm_order_method_t.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_atrib_ordem ).

      READ TABLE lt_atrib_ordem ASSIGNING FIELD-SYMBOL(<fs_atrib_ordem>) INDEX 1.
      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_atrib_ordem>-aufnr
          IMPORTING
            output = <fs_atrib_ordem>-aufnr.

        CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
          EXPORTING
            number        = <fs_atrib_ordem>-aufnr
          IMPORTING
            es_header     = ls_header
          TABLES
            et_oprol      = lt_oprol3
            et_operations = lt_operations
            et_olist      = lt_olist
            return        = lt_return.
        IF sy-subrc IS INITIAL.

          SELECT *
            FROM t350
            INTO @DATA(ls_t350)
            UP TO 1 ROWS
            WHERE auart = @ls_header-order_type.
          ENDSELECT.
          IF sy-subrc IS INITIAL.
            IF ls_t350-lngtxt IS NOT INITIAL AND ls_t350-notdat IS NOT INITIAL AND ls_t350-extended_ol EQ '2' .

              DELETE lt_operations INDEX 1.

              IF lt_olist IS NOT INITIAL.
                LOOP AT lt_olist ASSIGNING FIELD-SYMBOL(<fs_olist>).
                  DATA(lv_tabix2) = sy-tabix.

                  APPEND VALUE #( refnumber = sy-tabix objecttype = 'OBJECTLIST' method = 'CHANGE'  objectkey = <fs_atrib_ordem>-aufnr ) TO t_methods.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = <fs_olist>-notif_no
                    IMPORTING
                      output = <fs_olist>-notif_no.

                  APPEND
                  VALUE #(
                           counter = <fs_olist>-counter notif_no = <fs_olist>-notif_no
                         ) TO t_objectlist.

                ENDLOOP.

*                LOOP AT lt_operations ASSIGNING <fs_operations>.
*
*                  APPEND VALUE #( refnumber = sy-tabix objecttype = 'OPERATION' method = 'CHANGE'  objectkey = <fs_atrib_ordem>-aufnr ) TO t_methods.
*
*                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                    EXPORTING
*                      input  = <fs_olist>-notif_no
*                    IMPORTING
*                      output = <fs_olist>-notif_no.
*
*                ENDLOOP.
              ENDIF.

              SORT lt_oprol3 BY counter.
              SORT t_methods BY objecttype refnumber.
              LOOP AT <fs_atrib_ordem>-notas ASSIGNING FIELD-SYMBOL(<fs_notas>).

                lv_ordem = <fs_atrib_ordem>-aufnr.
                IF <fs_notas>-desvincular EQ abap_true.

                  IF <fs_notas>-qmnum EQ ls_header-notif_no.
                    lv_excluir_header = abap_true.
                  ENDIF.

                  lv_desvinc = abap_true.
                  READ TABLE lt_olist ASSIGNING <fs_olist>
                                     WITH KEY notif_no = <fs_notas>-qmnum.
                  IF sy-subrc IS INITIAL.
                    READ TABLE t_methods ASSIGNING FIELD-SYMBOL(<fs_methods>)
                    WITH KEY objecttype = 'OBJECTLIST'
                             refnumber  = sy-tabix
                    BINARY SEARCH.
                    IF sy-subrc IS INITIAL.
                      <fs_methods>-method = 'DELETE'.

                      READ TABLE lt_oprol3 ASSIGNING FIELD-SYMBOL(<fs_oprol>)
                      WITH KEY counter = <fs_olist>-counter
                      BINARY SEARCH.
                      IF sy-subrc IS INITIAL.
                        READ TABLE lt_operations TRANSPORTING NO FIELDS
                        WITH KEY notif_no = <fs_notas>-qmnum.
                        IF sy-subrc IS INITIAL.
                          ADD 1 TO lv_refnumber.
                          APPEND VALUE #( refnumber = lv_refnumber objecttype = 'OPERATION' method = 'DELETE'  objectkey = <fs_atrib_ordem>-aufnr ) TO t_methods.
                        ENDIF.

                      ENDIF.

                    ENDIF.

                  ENDIF.

                ELSE.
                  APPEND INITIAL LINE TO lt_notas ASSIGNING FIELD-SYMBOL(<fs_notas_aux>).

                  <fs_notas_aux>-qmnum = <fs_notas>-qmnum.

                ENDIF.

              ENDLOOP.

              IF lt_notas IS NOT INITIAL.

                CALL FUNCTION 'ZPM_ATUALIZA_OBJETOS_ORDEM' IN BACKGROUND TASK AS SEPARATE UNIT
                  EXPORTING
                    i_aufnr = <fs_atrib_ordem>-aufnr
                    i_notas = lt_notas
                  TABLES
                    i_oprol = lt_oprol3.

                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

              ENDIF.

              IF lv_desvinc IS NOT INITIAL.

                APPEND VALUE #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = lv_ordem ) TO t_methods.

                MOVE-CORRESPONDING lt_operations TO lt_operations2.

                APPEND INITIAL LINE TO lt_header ASSIGNING FIELD-SYMBOL(<fs_header>).
                MOVE-CORRESPONDING ls_header TO <fs_header>.

                IF lv_excluir_header IS NOT INITIAL.
                  CLEAR <fs_header>-notif_no.
                ENDIF.

                CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
                  TABLES
                    it_header     = lt_header
                    it_methods    = t_methods
                    it_operation  = lt_operations2
                    it_objectlist = t_objectlist
                    return        = lt_return.
                IF NOT line_exists( lt_return[ type = 'E' ] ).

                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

                  ls_retorno_ordem-sucess = abap_true.
                  ls_retorno_ordem-data-nr_ordem = lv_ordem.
                  ls_retorno_ordem-data-msg_ordem = 'Processo realizado com sucesso!'.
                  ls_retorno_ordem-data-status = '2'.

                  result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_ordem ).

                ELSE.

                  ls_retorno_ordem-data-nr_ordem = <fs_notas>-qmnum.

                  READ TABLE lt_return ASSIGNING <fs_return> INDEX 1.

                  ls_retorno_ordem-erros = <fs_return>-message.

                  result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_ordem ).

                ENDIF.

              ELSE.

                ls_retorno_ordem-sucess = abap_true.
                ls_retorno_ordem-data-nr_ordem = lv_ordem.
                ls_retorno_ordem-data-msg_ordem = 'Processo realizado com sucesso!'.
                ls_retorno_ordem-data-status = '2'.

                result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_ordem ).

              ENDIF.

            ELSE.

              LOOP AT lt_atrib_ordem ASSIGNING <fs_atrib_ordem>.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = <fs_atrib_ordem>-aufnr
                  IMPORTING
                    output = <fs_atrib_ordem>-aufnr.

                CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
                  EXPORTING
                    number   = <fs_atrib_ordem>-aufnr
                  TABLES
                    et_olist = lt_olist
                    return   = lt_return.

                IF lt_olist IS NOT INITIAL.
                  LOOP AT lt_olist ASSIGNING <fs_olist>.
                    lv_tabix2 = sy-tabix.

                    APPEND VALUE #( refnumber = sy-tabix objecttype = 'OBJECTLIST' method = 'CHANGE'  objectkey = <fs_atrib_ordem>-aufnr ) TO t_methods.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                      EXPORTING
                        input  = <fs_olist>-notif_no
                      IMPORTING
                        output = <fs_olist>-notif_no.

                    APPEND
                    VALUE #(
                             counter = <fs_olist>-counter notif_no = <fs_olist>-notif_no
                           ) TO t_objectlist.

                  ENDLOOP.

                  DATA(lt_olist2) = lt_olist.

                  SORT lt_olist2 BY counter DESCENDING .

                  READ TABLE lt_olist2 ASSIGNING <fs_olist> INDEX 1.
                  IF sy-subrc IS INITIAL.
                    DATA(lv_tabix) = <fs_olist>-counter.
                  ENDIF.

                ENDIF.

                LOOP AT <fs_atrib_ordem>-notas ASSIGNING <fs_notas>.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = <fs_notas>-qmnum
                    IMPORTING
                      output = <fs_notas>-qmnum.


                  lv_tabix = lv_tabix + 1.

                  lv_ordem = <fs_atrib_ordem>-aufnr.

                  IF <fs_notas>-desvincular IS NOT INITIAL.

                    IF <fs_notas>-qmnum EQ ls_header-notif_no.
                      lv_excluir_header = abap_true.
                    ENDIF.

                    READ TABLE lt_olist ASSIGNING <fs_olist>
                    WITH KEY notif_no = <fs_notas>-qmnum.
                    IF sy-subrc IS INITIAL.
                      READ TABLE t_methods ASSIGNING <fs_methods> INDEX sy-tabix.
                      IF sy-subrc IS INITIAL.
                        <fs_methods>-method = 'DELETE'.
                      ENDIF.

                    ENDIF.

                  ELSE.

                    lv_tabix2 = lv_tabix2 + 1.

                    APPEND VALUE #( refnumber = lv_tabix2 objecttype = 'OBJECTLIST' method = 'CREATE'  objectkey = lv_ordem ) TO t_methods.

                    APPEND
                    VALUE #(
                           counter = lv_tabix notif_no = <fs_notas>-qmnum
                         ) TO t_objectlist.

                  ENDIF.

                ENDLOOP.

              ENDLOOP.

              APPEND VALUE #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = lv_ordem ) TO t_methods.

              MOVE-CORRESPONDING lt_operations TO lt_operations2.

              FREE: lt_header.

              APPEND INITIAL LINE TO lt_header ASSIGNING <fs_header>.
              MOVE-CORRESPONDING ls_header TO <fs_header>.

              IF lv_excluir_header IS NOT INITIAL.
                CLEAR <fs_header>-notif_no.

                APPEND VALUE #( refnumber = lv_tabix2 objecttype = 'HEADER' method = 'CHANGE'  objectkey = lv_ordem ) TO lt_methods.
                APPEND VALUE #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = lv_ordem ) TO lt_methods.

                CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
                  TABLES
                    it_methods = lt_methods
                    it_header  = lt_header
                    return     = lt_return.

                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.

              ENDIF.

              CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
                TABLES
                  it_methods    = t_methods
                  it_header     = lt_header
                  it_operation  = lt_operations2
                  it_objectlist = t_objectlist
*                 it_text       = t_text
*                 it_text_lines = t_text_lines
                  return        = lt_return.
              IF NOT line_exists( lt_return[ type = 'E' ] ).

                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

                ls_retorno_ordem-sucess = abap_true.
                ls_retorno_ordem-data-nr_ordem = lv_ordem.
                ls_retorno_ordem-data-msg_ordem = 'Processo realizado com sucesso!'.
                ls_retorno_ordem-data-status = '2'.

                result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_ordem ).

              ELSE.

                ls_retorno_ordem-data-nr_ordem = <fs_notas>-qmnum.

                READ TABLE lt_return ASSIGNING <fs_return> INDEX 1.

                ls_retorno_ordem-erros = <fs_return>-message.

                result = zcl_fmcall_pm_mobile=>abap2json( abap_data = ls_retorno_ordem ).

              ENDIF.


            ENDIF.

          ENDIF.

        ENDIF.



      ENDIF.

    WHEN 'vw_estorno_apont'.

      /ui2/cl_json=>deserialize( EXPORTING json = body CHANGING data = lt_estorno ).

      LOOP AT lt_estorno ASSIGNING FIELD-SYMBOL(<fs_apont>).

        CALL FUNCTION 'BAPI_ALM_CONF_CANCEL'
          EXPORTING
            confirmation        = <fs_apont>-rueck
            confirmationcounter = <fs_apont>-rmzhl
          IMPORTING
            return              = lw_return.
        IF lw_return-type <> 'E' .

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          <fs_apont>-estornado = abap_true.

        ELSE.

          <fs_apont>-msg_erro = lw_return-message.

        ENDIF.

      ENDLOOP.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_estorno ).

    WHEN 'vw_get_jornada_user'.

      CALL METHOD _main_app->get_jornada_user
        EXPORTING
          pernr     = pernr
        RECEIVING
          t_jornada = lt_jornada.

      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = lt_jornada ).


*"FF - 23.02.24 - #131818 - inicio
*    WHEN 'vw_d_apont_zpm0102'.
*
*      CALL METHOD _main_app->apont_zpm0102
*        EXPORTING
*
*        RECEIVING
*          d_ordem   = DATA(_d_ordem).
*
*      result = zcl_fmcall_pm_mobile=>abap2json( abap_data = _d_ordem ).
*"FF - 23.02.24 - #131818 - fim




  ENDCASE.

ENDFUNCTION.
