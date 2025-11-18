FUNCTION-POOL zgf_pm_integracao_pm_mobil.   "MESSAGE-ID ..

*INCLUDE LZGF_PM_INTEGRACAO_PM_MOB.    " Local class definition

*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_INTEGRACAO_SAAFD01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class CL_MAIN_APP
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*

CLASS cl_main_app DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF status_veiculo,
        active   VALUE '1',
        inactive VALUE '0',
      END OF status_veiculo.



    TYPES:
      BEGIN OF ty_equipment,
        iwerk TYPE equz-iwerk,
        equnr TYPE equz-equnr,
        eqart TYPE equi-eqart,
      END OF ty_equipment,


      BEGIN OF ty_catalog,
        codigo TYPE qpcd-code,
        texto  TYPE qpct-kurztext,
        grupo  TYPE qpcd-codegruppe,
      END OF ty_catalog,

      BEGIN OF ty_jest,
        objnr TYPE jest-objnr,
        stat  TYPE jest-stat,
        txt04 TYPE tj02t-txt04,
        inact TYPE jest-inact,
      END OF ty_jest,

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

      END OF ty_jornada,

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

      tb_equipment       TYPE TABLE OF ty_equipment      WITH KEY equnr,
      tb_veiculo         TYPE TABLE OF ztpm_m_veic_mobile  WITH EMPTY KEY,
      tb_local           TYPE TABLE OF ztpm_m_local_mobile  WITH EMPTY KEY,
      tb_catalogo        TYPE TABLE OF ztpm_p_catal WITH EMPTY KEY,
      tb_tip_notas       TYPE TABLE OF ztpm_par_t_nota WITH EMPTY KEY,
      tb_tip_ordem       TYPE TABLE OF ztpm_par_t_ordem WITH EMPTY KEY,
      tb_p_ordem         TYPE TABLE OF ztpm_p_t_ordem WITH EMPTY KEY,
      tb_tip_atividade   TYPE TABLE OF ztpm_par_t_ativ WITH EMPTY KEY,
      tb_tip_prior       TYPE TABLE OF ztpm_par_t_prior WITH EMPTY KEY,
      tb_tip_grp         TYPE TABLE OF ztpm_par_t_grp WITH EMPTY KEY,
      tb_tip_desvio      TYPE TABLE OF ztpm_par_t_desvio WITH EMPTY KEY,
      tb_custo_eqpto     TYPE TABLE OF zpme0066 WITH EMPTY KEY,
      tb_d_equipe        TYPE TABLE OF ztpm_d_equipe WITH EMPTY KEY,
      tb_tip_cent        TYPE TABLE OF ztpm_par_t_centro WITH EMPTY KEY,
      tb_tip_ctrab       TYPE TABLE OF ztpm_par_t_ctrab WITH EMPTY KEY,
      tb_d_ordem         TYPE TABLE OF ztpm_d_m_ordem WITH EMPTY KEY,
      tb_d_nota          TYPE TABLE OF zepm_d_nota WITH EMPTY KEY,
      tb_d_status        TYPE TABLE OF ztpm_d_m_status WITH EMPTY KEY,
      tb_d_apont         TYPE TABLE OF ztpm_d_m_apont WITH EMPTY KEY,
      tb_d_operacao      TYPE TABLE OF ztpm_d_m_operacao2 WITH EMPTY KEY,
      tb_d_usuario       TYPE TABLE OF ztpm_d_m_usuario WITH EMPTY KEY,
      tb_ztpm_m_veic_mob TYPE TABLE OF ztpm_m_veic_mob WITH EMPTY KEY,
      tb_orc_ordem       TYPE TABLE OF ztpm_orc_ordem WITH EMPTY KEY,
      tb_consult_mat     TYPE TABLE OF ztpm_d_m_material WITH EMPTY KEY,
*      tb_d_apont_zpm0102 TYPE  ztpm_d_m_apont_102 , "FF - 23.02.24 - #131818
      tb_jornada         TYPE TABLE OF ty_jornada WITH EMPTY KEY.

    "// Note we can't define a range with header line in Classes
    "// Define a Ranges as Class Attribute
    CLASS-DATA status_range TYPE RANGE OF  stat.

    "// Define a Ranges as Instance Attribute
    DATA instance_range TYPE RANGE OF stat.


    CLASS-METHODS get_date_range
      IMPORTING
        from         TYPE sy-datum
        to           TYPE sy-datum
      RETURNING
        VALUE(table) TYPE rsis_t_range.

    CLASS-METHODS get_time_range
      IMPORTING
        from         TYPE cduzeit
        to           TYPE cduzeit
      RETURNING
        VALUE(table) TYPE rsis_t_range.

    CLASS-METHODS convert_date
      IMPORTING
        data          TYPE char16
      RETURNING
        VALUE(return) TYPE sy-datum.

    CLASS-METHODS convert_time
      IMPORTING
        time          TYPE char16
      RETURNING
        VALUE(return) TYPE cduzeit.

    METHODS select_equipments
      IMPORTING
        filial            TYPE iwerk
        situacao          TYPE c
      RETURNING
        VALUE(equipments) TYPE tb_equipment.


    METHODS get_veiculos
      IMPORTING
        filial          TYPE iwerk
        situacao        TYPE c
      RETURNING
        VALUE(veiculos) TYPE tb_veiculo.

    METHODS get_local
      IMPORTING
        filial       TYPE iwerk
        situacao     TYPE c
      RETURNING
        VALUE(local) TYPE tb_local.

    METHODS get_catalogo
      RETURNING
        VALUE(catalogo) TYPE tb_catalogo.


    METHODS get_tipo_notas
      RETURNING
        VALUE(tip_nota) TYPE tb_tip_notas.

    METHODS get_tipo_ordem
      RETURNING
        VALUE(tip_ordem) TYPE tb_tip_ordem.


    METHODS get_p_ordem " Tipo de ordem por centro de planejamento.
      IMPORTING
        filial         TYPE iwerk OPTIONAL
      RETURNING
        VALUE(p_ordem) TYPE tb_p_ordem.

    METHODS get_tipo_atividade
      RETURNING
        VALUE(tip_atividade) TYPE tb_tip_atividade.

    METHODS get_tipo_prior
      RETURNING
        VALUE(tip_prior) TYPE tb_tip_prior.

    METHODS get_tipo_grp
      IMPORTING
        filial         TYPE iwerk
      RETURNING
        VALUE(tip_grp) TYPE tb_tip_grp.

    METHODS get_tipo_cent
      RETURNING
        VALUE(tip_cent) TYPE tb_tip_cent.

    METHODS get_tipo_ctrab
      IMPORTING
        filial           TYPE iwerk
      RETURNING
        VALUE(tip_ctrab) TYPE tb_tip_ctrab.

    METHODS get_d_ordem
      IMPORTING
        filial         TYPE iwerk
        create_at      TYPE rsis_t_range
        update_at      TYPE timestampl
        ordem          TYPE aufnr OPTIONAL
      RETURNING
        VALUE(d_ordem) TYPE tb_d_ordem.

    METHODS get_d_nota
      IMPORTING
        filial        TYPE iwerk
        data          TYPE rsis_t_range
      RETURNING
        VALUE(d_nota) TYPE tb_d_nota.

    METHODS get_d_status
      RETURNING
        VALUE(d_status) TYPE tb_d_status.

    METHODS get_d_apont
      IMPORTING
        ordem          TYPE aufnr OPTIONAL
      RETURNING
        VALUE(d_apont) TYPE tb_d_apont.

    METHODS get_d_operacao
      IMPORTING
        filial            TYPE iwerk
        data              TYPE rsis_t_range
      RETURNING
        VALUE(d_operacao) TYPE tb_d_operacao.

    "===============================================
    METHODS get_d_usuario_id
      IMPORTING
        user_ad          TYPE zde_usuario
      RETURNING
        VALUE(d_usuario) TYPE tb_d_usuario.

    "===============================================
    METHODS get_d_usuario
      IMPORTING
        filial           TYPE iwerk
        user_ad          TYPE zde_usuario
        pernr            TYPE persno
      RETURNING
        VALUE(d_usuario) TYPE tb_d_usuario.

    METHODS get_d_equipe
      RETURNING
        VALUE(d_equipe) TYPE tb_d_equipe.


    METHODS get_custo_eqpto
      IMPORTING
        filial              TYPE iwerk
        mes_inicio          TYPE char02
        mes_fim             TYPE char02
        ano                 TYPE gjahr
        tp_object           TYPE eqart
        ct_eqpto            TYPE eqtyp
        eqpto               TYPE equnr
        modelo              TYPE typbz
      RETURNING
        VALUE(t_cust_eqpto) TYPE tb_custo_eqpto.

    METHODS get_p_desvio
      IMPORTING
        filial          TYPE iwerk
      RETURNING
        VALUE(p_desvio) TYPE tb_tip_desvio.

    METHODS get_inactive_status
      RETURNING VALUE(table) TYPE rsis_t_range.
*** Inicio - Rubenilson Pereira - 23.06.2022 - CS2020000572 API Aprovação de Orçamento via APP - 81248
    METHODS get_orc_ordem
      IMPORTING
        usuario            TYPE sy-uname
        supl_ordem         TYPE c
      RETURNING
        VALUE(t_orc_ordem) TYPE tb_orc_ordem.

*** Fim - Rubenilson Pereira - 23.06.2022 - CS2020000572 API Aprovação de Orçamento via APP - 81248

    METHODS get_consult_mat
      IMPORTING
        matnr            TYPE mara-matnr
        maktx            TYPE makt-maktx
        werks            TYPE marc-werks
      RETURNING
        VALUE(t_consult) TYPE tb_consult_mat.

    METHODS get_jornada_user
      IMPORTING
        pernr            TYPE persno
      RETURNING
        VALUE(t_jornada) TYPE tb_jornada.
***FF - 23.02.24 - #131818 - inicio
**    METHODS apont_zpm0102
**      IMPORTING
**        i_dados_apontamento    TYPE tb_d_apont_zpm0102
**      RETURNING
**        VALUE(e_retorno_apont) TYPE ztpm_d_m_apont_zpm0102_retorno.
***FF - 23.02.24 - #131818 - fim

    CLASS-METHODS check_sets_authorization
      IMPORTING input         TYPE werks_d
      RETURNING VALUE(return) TYPE sy-subrc.

  PRIVATE SECTION.
    DATA system_status TYPE TABLE OF bapi_itob_status.
    DATA user_status   TYPE TABLE OF bapi_itob_status.
ENDCLASS.

CLASS cl_main_app IMPLEMENTATION.

  METHOD get_veiculos.

*** Inicio - Rubenilson Pereira 10.06.2022 - 79444 - Trecho descomentado
    DATA classif_obj   TYPE TABLE OF sclass.
    DATA object_data   TYPE TABLE OF clobjdat.
    DATA veiculo       TYPE ztpm_m_veic_mobile.
    DATA general_data  TYPE bapi_itob.
    DATA point         TYPE diimpt.
    DATA km_real_ch    TYPE c LENGTH 30.
    DATA doc_medition  TYPE imrg.
    DATA _matnr        TYPE matnr.
    DATA w_equi TYPE v_equi.
    DATA tb_equipment       TYPE TABLE OF ty_equipment.
    DATA: lv_line    TYPE bsvx-sttxt,
          lv_created TYPE timestampl,
          lv_updated TYPE timestampl,
          lv_string  TYPE string.

*    CALL METHOD me->select_equipments
*      EXPORTING
*        filial     = filial
*        situacao   = situacao
*      RECEIVING
*        equipments = DATA(_equipments).

    FREE: veiculos, tb_equipment.

    DATA(_inact_status) = me->get_inactive_status( ).

    "//Get all active vehicles;
    SELECT DISTINCT b~iwerk a~equnr a~eqart
      INTO CORRESPONDING FIELDS OF TABLE tb_equipment
      FROM equi AS a
     INNER JOIN equz AS b ON b~equnr = a~equnr
     WHERE b~iwerk EQ filial
       AND b~datbi EQ '99991231'.

    SORT tb_equipment ASCENDING BY equnr.
    DELETE ADJACENT DUPLICATES FROM tb_equipment.

    IF tb_equipment IS NOT INITIAL.
      SELECT a~mandt e~tplnr e~pltxt a~equnr a~erdat a~aedat b~eqtyp b~eqart b~objnr a~iwerk a~datbi a~hequi c~eqktx a~rbnr
         FROM equz AS a
         INNER JOIN equi AS b ON b~equnr EQ a~equnr
         INNER JOIN eqkt AS c ON c~equnr EQ b~equnr
         INNER JOIN iloa AS d ON d~iloan EQ a~iloan
         INNER JOIN iflotx AS e ON e~tplnr EQ d~tplnr
         INTO CORRESPONDING FIELDS OF TABLE veiculos
        FOR ALL ENTRIES IN tb_equipment
           WHERE a~equnr EQ tb_equipment-equnr
            AND  a~datbi EQ '99991231'
            AND  c~spras EQ sy-langu.
*** Inicio - Rubenilson Pereira 10.06.2022 - 79444 - NOVA LÓGICA
      IF sy-subrc IS INITIAL.
        LOOP AT veiculos ASSIGNING FIELD-SYMBOL(<fs_veiculos>).
          <fs_veiculos>-id         = <fs_veiculos>-equnr.
*          <fs_veiculos>-created_at = <fs_veiculos>-erdat.
*          <fs_veiculos>-updated_at = <fs_veiculos>-aedat.

*** Inicio - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
          IF <fs_veiculos>-objnr IS NOT INITIAL.

            CALL FUNCTION 'STATUS_TEXT_EDIT'
              EXPORTING
                client           = sy-mandt
                objnr            = <fs_veiculos>-objnr
                spras            = sy-langu
              IMPORTING
                line             = lv_line
              EXCEPTIONS
                object_not_found = 1
                OTHERS           = 2.
            IF sy-subrc = 0.
              IF lv_line CS 'INAT' OR lv_line CS 'MREL'.
                <fs_veiculos>-istat = '0'.
              ELSE.
                <fs_veiculos>-istat = '1'.
              ENDIF.
            ENDIF.

          ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

          IF <fs_veiculos>-erdat IS NOT INITIAL.
            CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
              EXPORTING
                i_date = <fs_veiculos>-erdat    " Data
*               i_time = ''  " Hora
              IMPORTING
                e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

            lv_string = lv_created.
            REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
            CONDENSE lv_string NO-GAPS.

            <fs_veiculos>-created_at = lv_string(13).

          ENDIF.

          IF <fs_veiculos>-aedat IS NOT INITIAL.
            CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
              EXPORTING
                i_date = <fs_veiculos>-aedat    " Data
*               i_time = ''  " Hora
              IMPORTING
                e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

            lv_string = lv_updated.
            REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
            CONDENSE lv_string NO-GAPS.

            <fs_veiculos>-updated_at = lv_string(13).

          ENDIF.
        ENDLOOP.
      ENDIF.
*** Fim - Rubenilson Pereira 10.06.2022 - 79444 - NOVA LÓGICA

*      IF NOT ( _equipments IS INITIAL ).
*
*

*      LOOP AT _equipments INTO DATA(_equipment).

*        SELECT SINGLE a~mandt e~tplnr e~pltxt a~equnr a~erdat a~aedat b~eqtyp b~eqart b~objnr a~iwerk a~datbi a~hequi c~eqktx
*        FROM equz AS a
*        INNER JOIN equi AS b ON b~equnr EQ a~equnr
*        INNER JOIN eqkt AS c ON c~equnr EQ b~equnr
*        INNER JOIN iloa AS d ON d~iloan EQ a~iloan
*        INNER JOIN iflotx AS e ON e~tplnr EQ d~tplnr
*        INTO CORRESPONDING FIELDS OF veiculo
*          WHERE a~equnr EQ _equipment-equnr
*           AND  a~datbi EQ '99991231'
*           AND  c~spras EQ sy-langu.
*
*        APPEND veiculo TO veiculos.
*        CLEAR: veiculo.
*      ENDLOOP.


*
*      DESCRIBE TABLE IT_SAIDA_EQUI_DISPONIVEIS LINES data(VEICULOS-quantidade).
*      DESCRIBE TABLE IT_SAIDA_EQUI_DISPONIVEIS LINES data(_LT_ZTPM_M_VEIC_MOB-quantidade).

    ENDIF.
*** Fim - Rubenilson Pereira 10.06.2022 - 79444 - TRECHO DESCOMENTADO

*** Inicio - Rubenilson Pereira 10.06.2022 - 79444 - TRECHO COMENTADO
    "Seleciona daados equipamento/veiculos.
*    SELECT * FROM zpmt0047 INTO CORRESPONDING FIELDS OF TABLE veiculos WHERE iwerk EQ filial.
*** Inicio - Rubenilson Pereira 10.06.2022 - 79444 - TRECHO COMENTADO
  ENDMETHOD.

  METHOD get_local.

*** Inicio - Rubenilson Pereira - 10.06.2022 - 79444 - TRECHO DESCOMENTADO
    DATA: r_stat     TYPE rsis_t_range,
          date       TYPE syst_datum,
          hora       TYPE syst_uzeit,
          lw_local   TYPE ztpm_m_local_mobile,
          lv_line    TYPE bsvx-sttxt,
          lv_created TYPE timestampl,
          lv_updated TYPE timestampl,
          lv_string  TYPE string.

    CONSTANTS: c_stat TYPE jest-stat VALUE 'I0098'.

*    DATA: R_STAT LIKE RANGE OF JEST-STAT.
    DATA line_range LIKE LINE OF status_range.
    DATA(lib) =  'I0098'.


    APPEND VALUE #( sign = 'I'  option = 'EQ' low = lib  high = ' ' )  TO r_stat.

    IF filial IS NOT INITIAL.
      SELECT *
      FROM iflo AS a
      INNER JOIN iflotx AS b ON b~tplnr EQ a~tplnr
*** Inicio - Rubenilson Pereira - 10.06.2022 - 79444 - NOVA LÓGICA
*        INTO CORRESPONDING FIELDS OF TABLE local
        INTO TABLE @DATA(lt_iflo)
*** Fim - Rubenilson Pereira - 10.06.2022 - 79444 - NOVA LÓGICA
        WHERE swerk EQ @filial
        AND ( EXISTS ( SELECT * FROM jest
                     WHERE objnr EQ a~objnr
                       AND inact NE @abap_true
                       AND stat  EQ @c_stat ) ).
*** Inicio - Rubenilson Pereira - 10.06.2022 - 79444 - NOVA LÓGICA
      IF sy-subrc IS INITIAL.

        LOOP AT lt_iflo ASSIGNING FIELD-SYMBOL(<fs_iflo>).

          MOVE-CORRESPONDING <fs_iflo>-a TO lw_local.
          MOVE-CORRESPONDING <fs_iflo>-b TO lw_local.

          lw_local-id         = <fs_iflo>-a-iloan.
*          lw_local-created_at = <fs_iflo>-a-erdat.
*          lw_local-updated_at = <fs_iflo>-a-aedat.

*** Inicio - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

          IF <fs_iflo>-a-objnr IS NOT INITIAL.

            CALL FUNCTION 'STATUS_TEXT_EDIT'
              EXPORTING
                client           = sy-mandt
                objnr            = <fs_iflo>-a-objnr
                spras            = sy-langu
              IMPORTING
                line             = lv_line
              EXCEPTIONS
                object_not_found = 1
                OTHERS           = 2.
            IF sy-subrc = 0.
              IF lv_line CS 'INAT' OR lv_line CS 'MREL'.
                lw_local-istat = '0'.
              ELSE.
                lw_local-istat = '1'.
              ENDIF.
            ENDIF.

          ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

          IF <fs_iflo>-a-erdat IS NOT INITIAL.
            CLEAR: date, hora.
            date = <fs_iflo>-a-erdat.
            hora = space.
            CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
              EXPORTING
                i_date = date     " Data
                i_time = hora  " Hora
              IMPORTING
                e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

            lv_string = lv_created.
            REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
            CONDENSE lv_string NO-GAPS.

            lw_local-created_at = lv_string(13).

          ENDIF.

          IF <fs_iflo>-a-aedat IS NOT INITIAL.
            CLEAR: date, hora.
            date = <fs_iflo>-a-aedat.
            hora = space.
            CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
              EXPORTING
                i_date = date    " Data
                i_time = hora  " Hora
              IMPORTING
                e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

            lv_string = lv_updated.
            REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
            CONDENSE lv_string NO-GAPS.

            lw_local-updated_at = lv_string(13).

          ENDIF.


          APPEND lw_local TO local.
          CLEAR lw_local.

        ENDLOOP.
      ENDIF.
*** Fim - Rubenilson Pereira - 10.06.2022 - 79444 - NOVA LÓGICA
    ENDIF.
*** Fim - Rubenilson Pereira - 10.06.2022 - 79444 - TRECHO DESCOMENTADO

*** Inicio - Rubenilson Pereira - 10.06.2022 - 79444 - TRECHO COMENTADO
    "Seleciona dados de locais de instalação.
*    SELECT * FROM zpmt0048 INTO CORRESPONDING FIELDS OF TABLE local WHERE swerk EQ filial.
*** Fim - Rubenilson Pereira - 10.06.2022 - 79444 - TRECHO COMENTADO

  ENDMETHOD.

  METHOD get_date_range.
    IF to IS INITIAL AND from IS NOT INITIAL.
      table = VALUE rsis_t_range( ( low  = from sign = 'I' option = 'EQ'  ) ).
    ELSEIF from IS NOT INITIAL AND to IS NOT INITIAL.
      table = VALUE rsis_t_range( ( low  = from high = to sign = 'I' option = 'BT'  ) ).
    ENDIF.
  ENDMETHOD.

  METHOD get_time_range.
    IF to IS INITIAL AND from IS NOT INITIAL.
      table = VALUE rsis_t_range( ( low  = from sign = 'I' option = 'EQ'  ) ).
    ELSEIF from IS NOT INITIAL AND to IS NOT INITIAL.
      table = VALUE rsis_t_range( ( low  = from high = to sign = 'I' option = 'BT'  ) ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_date.
    return = |{ data(4) }{ data+5(2) }{ data+8(2) }|.
  ENDMETHOD.

  METHOD convert_time.
    return = |{ time+11(2) }{ time+14(2) }|.
  ENDMETHOD.

  METHOD select_equipments.
*    DATA(_PARAMETERS)   = ME->GET_PARAMETERS( PARAM_KEY = PARAM IS_ABASTEC = COND #( WHEN FLAG IS INITIAL THEN ABAP_TRUE ELSE ABAP_FALSE ) ).
*    DATA(_PARAMETERS)   = ME->GET_PARAMETERS( PARAM_KEY = 'TP_OBJ' IS_ABASTEC = ABAP_TRUE ).
    DATA(_inact_status) = me->get_inactive_status( ).


*DATA(wHERE_A) = ''
    CASE situacao.
      WHEN 'A'.
        "//Get all active vehicles;
        SELECT DISTINCT b~iwerk a~equnr a~eqart
          INTO CORRESPONDING FIELDS OF TABLE equipments
          FROM equi AS a
         INNER JOIN equz AS b ON b~equnr = a~equnr
         WHERE b~iwerk EQ filial
           AND b~datbi EQ '99991231'
           AND NOT EXISTS ( SELECT *
                             FROM jest
                             WHERE stat IN _inact_status
                              AND inact EQ abap_false
                              AND objnr EQ a~objnr
                           ).

      WHEN 'I'.
        "//Get inactive vehicles;
        SELECT DISTINCT b~iwerk a~equnr a~eqart
          APPENDING TABLE equipments
          FROM equi AS a
         INNER JOIN equz AS b ON b~equnr = a~equnr
         INNER JOIN jcds AS c ON c~objnr = a~objnr
         WHERE c~stat  IN _inact_status
           AND c~inact EQ abap_false
           AND b~iwerk EQ filial
           AND b~datbi EQ '99991231'.

    ENDCASE.

    SORT equipments ASCENDING BY equnr.
    DELETE ADJACENT DUPLICATES FROM equipments.
  ENDMETHOD.

  METHOD get_inactive_status.
    table = VALUE #( ( low = 'I0076' option = 'EQ' sign = 'I' )
                     ( low = 'I0320' option = 'EQ' sign = 'I' )
                   ).
  ENDMETHOD.


  METHOD check_sets_authorization.

    SELECT COUNT(*)
      FROM setleaf AS a
      INNER JOIN setlinet AS b ON a~setname EQ b~setname
                              AND a~lineid  EQ b~lineid
     WHERE a~setname  EQ 'MAGGI_CENTROS_MODULO_PM'
       AND a~valfrom  EQ input
       AND b~descript EQ 'SAAF'.

    return = sy-subrc.

  ENDMETHOD.

  METHOD get_catalogo.

*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444
    DATA: lw_catalogo TYPE ztpm_p_catal,
          lv_created  TYPE timestampl,
          lv_updated  TYPE timestampl,
          lv_string   TYPE string.
*** Fim - Rubenilson Pereira - 13.06.2022 - 79444

    SELECT *
    FROM qpgr AS a
    INNER JOIN qpgt AS b ON b~katalogart EQ a~katalogart AND b~codegruppe EQ a~codegruppe
    INNER JOIN tq15t AS c ON c~katalogart EQ a~katalogart
*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444
*            APPENDING CORRESPONDING FIELDS OF TABLE  catalogo
*** Fim - Rubenilson Pereira - 13.06.2022 - 79444
    INTO TABLE @DATA(lt_qpgr)
      WHERE b~katalogart IN ('2', '5', '8', 'A', 'B', 'C', 'D') " '8', 'A'
       AND  b~inaktiv    EQ @abap_false
       AND  b~sprache    EQ @sy-langu
       AND  c~sprache    EQ @sy-langu.

    SELECT *
    FROM qpct
    INTO TABLE @DATA(t_qpct)
*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444
*                FOR ALL ENTRIES IN @catalogo
      FOR ALL ENTRIES IN @lt_qpgr
*                WHERE katalogart EQ @catalogo-katalogart
      WHERE katalogart EQ @lt_qpgr-a-katalogart
*                  AND codegruppe EQ @catalogo-codegruppe
        AND codegruppe EQ @lt_qpgr-a-codegruppe
*** Fim - Rubenilson Pereira - 13.06.2022 - 79444
        AND sprache EQ @sy-langu.

*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444
*                CHECK catalogo IS NOT INITIAL.
    CHECK lt_qpgr IS NOT INITIAL.
*                SORT catalogo ASCENDING BY katalogart codegruppe.

*                LOOP AT catalogo ASSIGNING FIELD-SYMBOL(<_catalogo>).
    LOOP AT lt_qpgr ASSIGNING FIELD-SYMBOL(<fs_qpgr>).
*                  LOOP AT t_qpct INTO DATA(w_qpct) WHERE katalogart EQ <_catalogo>-katalogart
      LOOP AT t_qpct INTO DATA(w_qpct) WHERE katalogart EQ <fs_qpgr>-a-katalogart
*                                                    AND  codegruppe EQ <_catalogo>-codegruppe.
                                        AND  codegruppe EQ <fs_qpgr>-a-codegruppe.

        MOVE-CORRESPONDING <fs_qpgr>-a TO lw_catalogo.
        MOVE-CORRESPONDING <fs_qpgr>-b TO lw_catalogo.
        MOVE-CORRESPONDING <fs_qpgr>-c TO lw_catalogo.

        lw_catalogo-id          = <fs_qpgr>-a-katalogart.
*        lw_catalogo-created_at  = <fs_qpgr>-a-e_datum.
*        lw_catalogo-updated_at  =  <fs_qpgr>-a-a_datum.

        IF <fs_qpgr>-a-e_datum IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = <fs_qpgr>-a-e_datum    " Data
*             i_time = ''  " Hora
            IMPORTING
              e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_created.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          lw_catalogo-created_at = lv_string(13).

        ENDIF.

        IF <fs_qpgr>-a-a_datum IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = <fs_qpgr>-a-a_datum    " Data
*             i_time = ''  " Hora
            IMPORTING
              e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_updated.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          lw_catalogo-updated_at = lv_string(13).

        ENDIF.
*** Fim - Rubenilson Pereira - 13.06.2022 - 79444

        APPEND  VALUE #(
                        code     = w_qpct-code
*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444
*                                    kurztext = w_qpct-kurztext   ) TO <_catalogo>-item_code.
                        kurztext = w_qpct-kurztext   ) TO lw_catalogo-item_code.

        APPEND lw_catalogo TO catalogo.
        CLEAR lw_catalogo.
*** Fim - Rubenilson Pereira - 13.06.2022 - 79444

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_tipo_notas.



    SELECT *
    FROM tq80 AS a
    INNER JOIN tq80_t AS b ON b~qmart EQ a~qmart
    INTO CORRESPONDING FIELDS OF TABLE tip_nota
      WHERE spras EQ 'P'
       AND qmtyp EQ '01'.

*  Selecionando tipo de catalogo / code.
    CHECK tip_nota IS NOT INITIAL.
    SELECT *
    FROM t352c
    INTO TABLE @DATA(_t352dc)
      FOR ALL ENTRIES IN @tip_nota
      WHERE rbnr EQ @tip_nota-rbnr.

    IF _t352dc IS NOT INITIAL.

      SELECT *
      FROM qpcd
      INTO TABLE @DATA(t_qpcd)
        FOR ALL ENTRIES IN @_t352dc
        WHERE katalogart EQ @_t352dc-qkatart
          AND codegruppe EQ @_t352dc-qcodegrp.

    ENDIF.

*    DATA(T_CODE) = TIP_NOTA.

    LOOP AT tip_nota ASSIGNING FIELD-SYMBOL(<w_nota>).

      SELECT SINGLE *
          FROM tqscr
          INTO @DATA(_tqscr)
            WHERE qmart EQ @<w_nota>-qmart
              AND qmtyp EQ '01'
              AND tabcd EQ '10\TAB00'.

      IF _tqscr-sub03 IS NOT INITIAL AND _tqscr-sub03 EQ '035'.
        <w_nota>-avaria = abap_true.
      ELSE.
        <w_nota>-avaria = ' '.
      ENDIF.
      CLEAR _tqscr.


*     Catalogo codificação
      IF <w_nota>-sakat IS NOT INITIAL.
        READ TABLE _t352dc INTO DATA(_condf) WITH KEY rbnr = <w_nota>-rbnr
                                                   qkatart = <w_nota>-sakat.

        IF <w_nota>-sakat EQ _condf-qkatart.
          <w_nota>-qmgrp = _condf-qcodegrp.
        ENDIF.
      ENDIF.


*     Catalogo parte do objeto
      IF <w_nota>-otkat IS NOT INITIAL.
        READ TABLE _t352dc INTO DATA(_part) WITH KEY rbnr = <w_nota>-rbnr
                                                  qkatart = <w_nota>-otkat.


        IF <w_nota>-otkat EQ _part-qkatart.
          <w_nota>-otgrp = _part-qcodegrp.
        ENDIF.
      ENDIF.


*     Catalogo tipo de problema/Erro
      IF <w_nota>-fekat IS NOT INITIAL.
        READ TABLE _t352dc INTO DATA(_prob) WITH KEY rbnr = <w_nota>-rbnr
                                                  qkatart = <w_nota>-fekat.

        IF <w_nota>-fekat EQ _prob-qkatart.
          <w_nota>-fegrp = _prob-qcodegrp.
        ENDIF.
      ENDIF.

*     Catalogo causas
      IF <w_nota>-urkat IS NOT INITIAL.
        READ TABLE _t352dc INTO DATA(_caus) WITH KEY rbnr = <w_nota>-rbnr
                                                     qkatart = <w_nota>-urkat.

        IF <w_nota>-urkat EQ _caus-qkatart.
          <w_nota>-urgrp = _caus-qcodegrp.
        ENDIF.
      ENDIF.

*     Catalogo atividade
      IF <w_nota>-mnkat IS NOT INITIAL.
        READ TABLE _t352dc INTO DATA(_ativ) WITH KEY rbnr = <w_nota>-rbnr
                                                     qkatart = <w_nota>-mnkat.

        IF <w_nota>-mnkat EQ _ativ-qkatart.
          <w_nota>-mngrp = _ativ-qcodegrp.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD get_tipo_ordem.

    SELECT *
    FROM v_auart
    INTO CORRESPONDING FIELDS OF TABLE tip_ordem
    WHERE spras EQ 'P'
      AND autyp EQ '30'.


  ENDMETHOD.

  METHOD get_p_ordem.

    DATA: r_filial     TYPE RANGE OF werks_d.

    IF filial IS NOT INITIAL.
      r_filial = VALUE #( ( sign = 'I' option = 'EQ' low = filial ) ).
    ENDIF.

    SELECT *
      FROM t003o
      INTO TABLE @DATA(_t003o)
      WHERE autyp EQ '30'.
*
    SELECT *
    FROM t350w
    INTO CORRESPONDING FIELDS OF TABLE p_ordem
      FOR ALL ENTRIES IN _t003o
      WHERE auart EQ _t003o-auart
        AND iwerk IN r_filial.

    SELECT *
     FROM t003p
     INTO TABLE @DATA(_t003p)
       FOR ALL ENTRIES IN @p_ordem
       WHERE auart EQ @p_ordem-auart
       AND spras EQ 'P'.

    CHECK p_ordem IS NOT INITIAL.

    LOOP AT p_ordem ASSIGNING FIELD-SYMBOL(<s_ordem>).
      READ TABLE _t003o INTO DATA(s_t003o) WITH KEY auart = <s_ordem>-auart.
      IF sy-subrc EQ 0.
        <s_ordem>-autyp = s_t003o-autyp.
      ENDIF.

      CLEAR: s_t003o.
    ENDLOOP.
    FREE: s_t003o.
  ENDMETHOD.



  METHOD get_tipo_atividade.

    SELECT *
    FROM v_d_t350i
    INTO CORRESPONDING FIELDS OF TABLE tip_atividade
    WHERE spras EQ 'P'.
  ENDMETHOD.

  METHOD get_tipo_prior.

    SELECT *
    FROM t356_t
    INTO CORRESPONDING FIELDS OF TABLE tip_prior
    WHERE spras EQ 'P'
      AND artpr EQ 'PM'.
  ENDMETHOD.



  METHOD get_tipo_grp.

    SELECT *
       FROM t024i
       INTO CORRESPONDING FIELDS OF TABLE tip_grp
       WHERE iwerk EQ filial.

  ENDMETHOD.

  METHOD get_tipo_cent.
    SELECT *
    FROM t001w
    INTO CORRESPONDING FIELDS OF TABLE tip_cent
    WHERE spras EQ 'P'.
  ENDMETHOD.

  METHOD get_tipo_ctrab.

    DATA: lw_ctrab   TYPE ztpm_par_t_ctrab,
          lv_created TYPE timestampl,
          lv_updated TYPE timestampl,
          lv_string  TYPE string.

    SELECT *
    FROM crhd AS a
    INNER JOIN crtx AS b ON b~objid EQ a~objid
*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444
*    INTO CORRESPONDING FIELDS OF TABLE tip_ctrab
    INTO TABLE @DATA(lt_crhd)
*** Fim - Rubenilson Pereira - 13.06.2022 - 79444
    WHERE a~werks EQ @filial
      AND a~verwe EQ '0005'
     AND  b~spras EQ @sy-langu.

    IF sy-subrc IS INITIAL.
      LOOP AT lt_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>).
*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444
        MOVE-CORRESPONDING <fs_crhd>-a TO lw_ctrab.
        MOVE-CORRESPONDING <fs_crhd>-b TO lw_ctrab.

        lw_ctrab-id         = <fs_crhd>-a-arbpl.
*        lw_ctrab-created_at = <fs_crhd>-a-begda.
*        lw_ctrab-updated_at = <fs_crhd>-a-aedat_grnd.

*** Inicio - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
        IF <fs_crhd>-a-lvorm IS NOT INITIAL OR <fs_crhd>-a-xsprr IS NOT INITIAL.
          lw_ctrab-istat = '1'.
        ELSE.
          lw_ctrab-istat = '0'.
        ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

        IF <fs_crhd>-a-begda IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = <fs_crhd>-a-begda    " Data
*             i_time = ''  " Hora
            IMPORTING
              e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_created.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          lw_ctrab-created_at = lv_string(13).
        ENDIF.


        IF <fs_crhd>-a-aedat_grnd IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = <fs_crhd>-a-aedat_grnd   " Data
*             i_time = ''  " Hora
            IMPORTING
              e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_updated.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          lw_ctrab-updated_at = lv_string(13).
        ENDIF.

        APPEND lw_ctrab TO tip_ctrab.
        CLEAR lw_ctrab.
*** Fim - Rubenilson Pereira - 13.06.2022 - 79444
      ENDLOOP.
    ENDIF.



  ENDMETHOD.

  METHOD get_d_ordem.

    TYPES:
      BEGIN OF ty_objnr,
        objnr TYPE bpge-objnr,
      END OF ty_objnr.

    DATA: gt_conf        TYPE TABLE OF afru,
          d_operacao     TYPE TABLE OF ztpm_d_m_operacao WITH EMPTY KEY,
          lt_componentes TYPE TABLE OF bapi_alm_order_component_e,
          lt_mensagem    TYPE ztpm_mensagem,
          lt_component2  TYPE TABLE OF bapi_alm_order_component,
          lt_operations  TYPE TABLE OF bapi_alm_order_operation_e,
          lt_operations2 TYPE TABLE OF bapi_alm_order_operation,
          lt_return      TYPE TABLE OF bapiret2,
          lv_created     TYPE timestampl,
          lv_updated     TYPE timestampl,
          lv_sobra       TYPE string,
          lv_string      TYPE string,
          lw_header      TYPE bapi_alm_order_header_e,
          lv_line        TYPE bsvx-sttxt,
          lt_messages    TYPE bal_t_msg,
          lt_resbd       TYPE TABLE OF resbd,
*          lt_components  TYPE TABLE OF bapi_alm_order_component_e,
          lt_materiais   TYPE ztpm_d_m_material_t,
          lt_objnr       TYPE TABLE OF ty_objnr.

*  Carregando ordem de manutenção.
    DATA: gt_jest TYPE TABLE OF ty_jest.
    DATA: w_jest TYPE ty_jest.
    DATA: r_stat TYPE rsis_t_range.
    DATA: _update_date TYPE syst_datum,
          _update_time TYPE syst_uzeit,
          r_filial     TYPE RANGE OF werks_d,
          lr_objnr     TYPE RANGE OF bpge-objnr,
          lv_objnr     TYPE bpge-objnr,
          lv_posit     TYPE bpge-posit,
          lv_versn     TYPE bpge-versn VALUE '000',
          lv_lednr     TYPE bpge-lednr VALUE '0003',
          lv_geber     TYPE bpge-geber,
          lr_ordem     TYPE RANGE OF aufnr.

*    DATA: R_STAT LIKE RANGE OF JEST-STAT.
    DATA line_range LIKE LINE OF status_range.
    DATA(aber) = 'I0001'.
    DATA(lib) =  'I0002'.
    DATA(lv_ordem) = ordem.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_ordem
      IMPORTING
        output = lv_ordem.


    IF filial IS NOT INITIAL.
      r_filial = VALUE #( ( sign = 'I' option = 'EQ' low = filial ) ).
    ENDIF.

    IF lv_ordem IS NOT INITIAL.
      lr_ordem = VALUE #( ( sign = 'I' option = 'EQ' low =  lv_ordem ) ).
    ENDIF.

*    APPEND VALUE #( sign = 'I'  option = 'EQ' low = lib  high = ' ' )  TO r_stat.

    IF update_at IS NOT INITIAL.
*      _update_at = update_at.
*      REPLACE ALL OCCURRENCES OF '-' IN _update_at WITH space.
*      REPLACE ALL OCCURRENCES OF '-' IN _update_at WITH space.
*      REPLACE ALL OCCURRENCES OF '/' IN _update_at WITH space.

      CALL FUNCTION 'Z_CONV_TIMESTAMP_MIL_TO_DATE'
        EXPORTING
          i_date = update_at  " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)
        IMPORTING
          e_date = _update_date
          e_time = _update_time.


      SELECT *
      FROM viaufkst AS a
      INTO CORRESPONDING FIELDS OF TABLE d_ordem
      WHERE a~aufnr  IN lr_ordem
        AND a~werks  IN r_filial
        AND a~erdat  IN create_at
        AND a~aedat  GT _update_date "Maior que data de referencia
        AND a~aezeit GT _update_time
        AND a~auart  NOT IN ( 'ZPM6', 'ID06', 'HE06' , 'ZPM5' )
        AND a~autyp  EQ '30'
        AND ( EXISTS ( SELECT * FROM jest
        WHERE objnr EQ a~objnr
        AND inact NE abap_true ) ).
    ELSE.
      SELECT *
            FROM viaufkst
            INTO CORRESPONDING FIELDS OF TABLE d_ordem
            WHERE aufnr IN lr_ordem
              AND werks IN r_filial
              AND erdat IN create_at
              AND auart NOT IN ( 'ZPM6', 'ID06', 'HE06' , 'ZPM5' )
              AND autyp EQ '30'
              AND ( EXISTS ( SELECT * FROM jest
              WHERE objnr EQ viaufkst~objnr
              AND inact NE abap_true ) ).

    ENDIF.

    CHECK d_ordem IS NOT INITIAL.

*Coletando descrição da prioridade.
    SELECT *
    FROM t356_t
    INTO TABLE @DATA(s_t356_t)
    FOR ALL ENTRIES IN @d_ordem
      WHERE priok EQ @d_ordem-priok
        AND artpr EQ 'PM'
        AND spras EQ 'P'.

*   Coletando descrição da local de instalação.
    SELECT *
   FROM iflo
   INTO TABLE @DATA(s_iflo)
      FOR ALL ENTRIES IN @d_ordem
     WHERE tplnr EQ @d_ordem-tplnr.

*   Coletando descrição da atividade.
    SELECT *
    FROM t353i_t
    INTO TABLE @DATA(s_t353i_t)
      FOR ALL ENTRIES IN @d_ordem
      WHERE ilart EQ @d_ordem-ilart
      AND   spras EQ 'P'.


*   Coletando descrição do tipo de ordem.
    SELECT *
    FROM v_auart
    INTO TABLE @DATA(s_v_auart)
      FOR ALL ENTRIES IN @d_ordem
    WHERE auart EQ @d_ordem-auart
    AND  spras EQ 'P'.

*   Coletando descrição do equipamento.
    SELECT *
    FROM eqkt
    INTO TABLE @DATA(s_eqkt)
      FOR ALL ENTRIES IN @d_ordem
      WHERE equnr EQ @d_ordem-equnr.

* Coletando centro de trabalho responsável.
    SELECT *
    FROM crhd
    INTO TABLE @DATA(s_crhd)
      FOR ALL ENTRIES IN @d_ordem
      WHERE objid = @d_ordem-gewrk.

    IF d_ordem IS NOT INITIAL.

      SELECT *
      FROM zpmt0022
      INTO TABLE @DATA(_equip)
      FOR ALL ENTRIES IN @d_ordem
      WHERE aufnr EQ @d_ordem-aufnr.

      SORT _equip BY aufnr.

    ENDIF.
*    SORT: d_ordem BY objnr.
*    "Seleção status da ordem.
*    SELECT * FROM jest INTO TABLE @DATA(t_jest) FOR ALL ENTRIES IN @d_ordem WHERE  objnr EQ @d_ordem-objnr AND inact EQ @abap_true.
*    IF sy-subrc EQ 0.
*
*      "Desc. status.
*      SELECT * FROM tj02t INTO TABLE @DATA(t_tj02t) FOR ALL ENTRIES IN @t_jest WHERE  istat EQ @t_jest-stat.
*    ENDIF.

*
*    SORT: t_jest  BY objnr.

*    DELETE d_ordem WHERE marc_status NE abap_true.


    "Preencher dados da operação.
    DATA: _d_ordem TYPE TABLE OF aufk.
    SELECT *
    FROM afko AS a
    INNER JOIN afvc AS b ON b~aufpl EQ a~aufpl
    INNER JOIN afvv AS c ON c~aufpl EQ b~aufpl AND c~aplzl EQ b~aplzl
    INTO CORRESPONDING FIELDS OF TABLE d_operacao
      FOR ALL ENTRIES IN d_ordem
      WHERE a~aufnr EQ d_ordem-aufnr
       AND b~werks EQ filial
       AND b~loekz EQ abap_false.
    SORT d_operacao ASCENDING BY aufnr vornr.

    FREE gt_conf.
    SELECT *
    FROM afru
    INTO TABLE gt_conf
      FOR ALL ENTRIES IN d_operacao
      WHERE aufnr EQ d_operacao-aufnr
        AND vornr EQ d_operacao-vornr.

    SORT gt_conf ASCENDING BY aufnr vornr aueru.
*    DELETE gt_conf WHERE aueru NE abap_true.
    IF gt_conf IS NOT INITIAL.

      DATA(lt_conf2) = gt_conf.
      SORT lt_conf2 BY aufnr vornr stzhl.

      DATA(lt_afru_aux) = gt_conf.

      SORT lt_afru_aux BY aufpl vornr.
      DELETE ADJACENT DUPLICATES FROM lt_afru_aux COMPARING aufpl vornr.

      SELECT aufpl, vornr, arbid
        FROM afvc
        INTO TABLE @DATA(lt_afvc)
        FOR ALL ENTRIES IN @lt_afru_aux
        WHERE aufpl = @lt_afru_aux-aufpl
          AND vornr = @lt_afru_aux-vornr.
      IF sy-subrc IS INITIAL.
        SORT lt_afvc BY aufpl vornr.

        DATA(lt_afvc_aux) = lt_afvc.
        SORT lt_afvc_aux BY arbid.
        DELETE ADJACENT DUPLICATES FROM lt_afvc_aux COMPARING arbid.

        SELECT objid, arbpl
          FROM crhd
          INTO TABLE @DATA(lt_crhd)
          FOR ALL ENTRIES IN @lt_afvc_aux
          WHERE objid = @lt_afvc_aux-arbid.
        IF sy-subrc IS INITIAL.
          SORT lt_crhd BY objid.
        ENDIF.
      ENDIF.

    ENDIF.

    SORT d_operacao BY aufnr.

    DATA(lt_ordem) = d_ordem.
    SORT lt_ordem BY aufnr.
    DELETE ADJACENT DUPLICATES FROM lt_ordem COMPARING aufnr.

    SELECT aufnr,rsnum
      FROM afko
      INTO TABLE @DATA(lt_afko)
      FOR ALL ENTRIES IN @lt_ordem
      WHERE aufnr = @lt_ordem-aufnr.
    IF sy-subrc IS INITIAL.
      SORT lt_afko BY aufnr.

      DATA(lt_afko_aux) = lt_afko.
      SORT lt_afko_aux BY rsnum.
      DELETE ADJACENT DUPLICATES FROM lt_afko_aux COMPARING rsnum.

      SELECT *
        FROM resb
        INTO TABLE @DATA(lt_resb)
        FOR ALL ENTRIES IN @lt_afko_aux
        WHERE rsnum = @lt_afko_aux-rsnum.
      IF sy-subrc IS INITIAL.
        SORT lt_resb BY rsnum.

        DATA(lt_resb_aux) = lt_resb.
        SORT lt_resb_aux BY ebeln.
        DELETE ADJACENT DUPLICATES FROM lt_resb_aux COMPARING ebeln.

        SELECT ebeln
          FROM ekko
          INTO TABLE @DATA(lt_ekko)
          FOR ALL ENTRIES IN @lt_resb_aux
          WHERE ebeln = @lt_resb_aux-ebeln
            AND bstyp = 'F'.
        IF sy-subrc IS INITIAL.
          SORT lt_ekko BY ebeln.
        ENDIF.
      ENDIF.

*      SELECT rsnum,ebeln,ebelp
*        FROM rsdb
*        INTO TABLE @DATA(lt_rsdb)
*        FOR ALL ENTRIES IN @lt_afko_aux
*        WHERE rsnum = @lt_afko_aux-rsnum.
*        IF sy-subrc is INITIAL.
*          sort lt_rsdb by rsnum.
*
*          data(lt_rsbd_aux) = lt_rsdb.
*
*          sort lt_rsdb_aux by ebeln ebelp.
*          delete ADJACENT DUPLICATES FROM lt_rsdb_aux COMPARING ebeln ebelp.
*
*          select ebeln,ebelp,
*        ENDIF.

    ENDIF.

    LOOP AT d_ordem ASSIGNING FIELD-SYMBOL(<fs_ordem>).
      APPEND INITIAL LINE TO lt_objnr ASSIGNING FIELD-SYMBOL(<fs_objnr>).

      <fs_objnr>-objnr = 'OR' && <fs_ordem>-aufnr.
    ENDLOOP.

    IF lt_objnr IS NOT INITIAL.

      SELECT objnr,wtgev,wlges
        FROM bpge
        INTO TABLE @DATA(lt_bpge)
        FOR ALL ENTRIES IN @lt_objnr
        WHERE objnr = @lt_objnr-objnr
          AND wrttp EQ '42'
          AND vorga EQ 'KBFC'.
      IF sy-subrc IS INITIAL.
        SORT lt_bpge BY objnr.
      ENDIF.

    ENDIF.

    DATA(lt_ordem_aux) = d_ordem.
    SORT lt_ordem_aux BY aufnr.
    DELETE ADJACENT DUPLICATES FROM lt_ordem_aux COMPARING aufnr.

*    SELECT *
*      FROM resb
*      INTO TABLE @DATA(lt_components)
*      FOR ALL ENTRIES IN @lt_ordem_aux
*      WHERE aufnr = @lt_ordem_aux-aufnr.
*    IF sy-subrc IS INITIAL.


    SORT lt_resb BY aufnr vornr.

    DATA(lt_components_aux) = lt_resb.
    SORT lt_components_aux BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_components_aux COMPARING matnr.

    SELECT *
      FROM makt
      INTO TABLE @DATA(lt_mara)
      FOR ALL ENTRIES IN @lt_components_aux
      WHERE matnr = @lt_components_aux-matnr
        AND spras = @sy-langu.
    IF sy-subrc IS INITIAL.
      SORT lt_mara BY matnr.
    ENDIF.
*    ENDIF.

    lt_ordem_aux = d_ordem.
    SORT lt_ordem_aux BY aufnr.
    DELETE ADJACENT DUPLICATES FROM lt_ordem_aux COMPARING aufnr.

    SELECT *
      FROM viqmel
      INTO TABLE @DATA(lt_viqmel)
      FOR ALL ENTRIES IN @lt_ordem_aux
      WHERE aufnr = @lt_ordem_aux-aufnr.
    IF sy-subrc IS INITIAL.
      SORT lt_viqmel BY aufnr.
    ENDIF.

    LOOP AT d_ordem ASSIGNING FIELD-SYMBOL(<ls_ordem>).
      IF <ls_ordem>-pernr EQ '00000000'.
        CLEAR <ls_ordem>-pernr.
      ENDIF.

      READ TABLE lt_viqmel ASSIGNING FIELD-SYMBOL(<fs_viqmel>)
      WITH KEY aufnr = <ls_ordem>-aufnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_viqmel ASSIGNING <fs_viqmel> FROM sy-tabix.

          IF <ls_ordem>-aufnr <> <fs_viqmel>-aufnr.
            EXIT.
          ENDIF.

          APPEND INITIAL LINE TO <ls_ordem>-notas ASSIGNING FIELD-SYMBOL(<fs_qmnum>).

          <fs_qmnum>-qmnum = <fs_viqmel>-qmnum.
        ENDLOOP.
      ENDIF.

      CONCATENATE 'OR' <ls_ordem>-aufnr INTO lv_objnr.

      READ TABLE lt_bpge ASSIGNING FIELD-SYMBOL(<fs_bpge>)
      WITH KEY objnr = lv_objnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <ls_ordem>-orcamento_disp = <fs_bpge>-wtgev - <fs_bpge>-wlges.
      ENDIF.

      "Status da ordem.

      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          objnr            = <ls_ordem>-objnr
          spras            = 'P'
        IMPORTING
          line             = lv_line
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc = 0.

        IF lv_line CS 'ABER'.
          <ls_ordem>-istat = '0'.
        ELSEIF lv_line CS 'LIB'.
          <ls_ordem>-istat = '1'.
        ELSEIF lv_line CS 'ENTE' OR
               lv_line CS 'ENCE' .
          <ls_ordem>-istat = '2'.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.


      READ TABLE  s_t356_t INTO DATA(_t356_t) WITH KEY priok = <ls_ordem>-priok.
      IF sy-subrc EQ 0.
        <ls_ordem>-artpr  = _t356_t-artpr.
        <ls_ordem>-priokx = _t356_t-priokx.
      ENDIF.

      READ TABLE  s_iflo INTO DATA(_iflo) WITH KEY tplnr = <ls_ordem>-tplnr.
      IF sy-subrc EQ 0.
        <ls_ordem>-pltxt = _iflo-pltxt.
      ENDIF.

      READ TABLE  s_t353i_t INTO DATA(_t353i_t) WITH KEY ilart = <ls_ordem>-ilart.
      IF sy-subrc EQ 0.
        <ls_ordem>-ilatx = _t353i_t-ilatx.
      ENDIF.

      READ TABLE  s_v_auart INTO DATA(_v_auart) WITH KEY auart = <ls_ordem>-auart.
      IF sy-subrc EQ 0.
        <ls_ordem>-txt = _v_auart-txt.
      ENDIF.

      IF <ls_ordem>-equnr IS NOT INITIAL.
        READ TABLE  s_eqkt INTO DATA(_eqkt) WITH KEY equnr = <ls_ordem>-equnr.
        IF sy-subrc EQ 0.
          <ls_ordem>-eqktx = _eqkt-eqktx.
        ENDIF.
      ENDIF.

      READ TABLE  s_crhd INTO DATA(_crhd) WITH KEY objid = <ls_ordem>-gewrk.
      IF sy-subrc EQ 0.
        <ls_ordem>-cr_objty = _crhd-objty.
        <ls_ordem>-arbpl    = _crhd-arbpl.
      ENDIF.

      READ TABLE _equip INTO DATA(_eqp) WITH KEY aufnr = <ls_ordem>-aufnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <ls_ordem>-idequipe = _eqp-idequipe.
        <ls_ordem>-dsequipe = _eqp-dsequipe.
      ENDIF.

      IF <ls_ordem>-erdat IS NOT INITIAL AND <ls_ordem>-erfzeit IS NOT INITIAL.
        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <ls_ordem>-erdat    " Data
            i_time = <ls_ordem>-erfzeit  " Hora
          IMPORTING
            e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_created.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <ls_ordem>-created_at = lv_string(13).

      ENDIF.

      IF <ls_ordem>-aedat IS NOT INITIAL AND <ls_ordem>-aezeit IS NOT INITIAL.
        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <ls_ordem>-aedat    " Data
            i_time = <ls_ordem>-aezeit  " Hora
          IMPORTING
            e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string  = lv_updated.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <ls_ordem>-updated_at = lv_string(13).

      ENDIF.

      <ls_ordem>-id          = <ls_ordem>-aufnr.
*      <ls_ordem>-txt04       = 'LIB'.
      <ls_ordem>-marc_status = abap_true.
*      <ls_ordem>-istat       = '1'.


*      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
*        EXPORTING
*          number        = <ls_ordem>-aufnr
*        TABLES
*          et_components = lt_components
*          return        = lt_return.

*      SORT lt_components BY activity.

      READ TABLE d_operacao TRANSPORTING NO FIELDS
        WITH KEY aufnr = <ls_ordem>-aufnr
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT d_operacao ASSIGNING FIELD-SYMBOL(<_operacao>) FROM sy-tabix.

          IF <ls_ordem>-aufnr <> <_operacao>-aufnr.
            EXIT.
          ENDIF.

          IF <_operacao>-pernr EQ '00000000'.
            CLEAR <_operacao>-pernr.
          ENDIF.

          READ TABLE lt_resb TRANSPORTING NO FIELDS
          WITH KEY aufnr = <_operacao>-aufnr
                   vornr = <_operacao>-vornr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            LOOP AT lt_resb ASSIGNING FIELD-SYMBOL(<fs_components>) FROM sy-tabix.
              IF <fs_components>-vornr <> <_operacao>-vornr OR <_operacao>-aufnr <> <fs_components>-aufnr.
                EXIT.
              ENDIF.

              APPEND INITIAL LINE TO lt_materiais ASSIGNING FIELD-SYMBOL(<fs_materiais>).

              <fs_materiais>-aufnr = <_operacao>-aufnr.
              <fs_materiais>-id    = <fs_components>-matnr.
              <fs_materiais>-matnr = <fs_components>-matnr.
              <fs_materiais>-menge = <fs_components>-bdmng.
              <fs_materiais>-rsnum = <fs_components>-rsnum.
              <fs_materiais>-rspos = <fs_components>-rspos.
              <fs_materiais>-werks = <fs_components>-werks.
              <fs_materiais>-ablad = <fs_components>-ablad.
              <fs_materiais>-wempf = <fs_components>-wempf.
              <fs_materiais>-enmng = <fs_components>-enmng.
              <fs_materiais>-lgort = <fs_components>-lgort.
              <fs_materiais>-postp = <fs_components>-postp.

              READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
              WITH KEY matnr = <fs_components>-matnr
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                <fs_materiais>-maktx = <fs_mara>-maktx.
              ENDIF.

              <fs_materiais>-xloek = <fs_components>-xloek.

            ENDLOOP.

          ENDIF.

**  Begin of    #106408   FF  10.03.2023
*          IF <_operacao>-steus EQ 'PM01'.
*            <_operacao>-steus = '1'.
*          ELSE.
*            <_operacao>-steus = '0'.
*          ENDIF.
** End of FF  10.03.2023

          SELECT SINGLE *
          FROM crhd
          INTO @DATA(w_crhd)
            WHERE objid EQ @<_operacao>-arbid.
          IF sy-subrc EQ 0.
            <_operacao>-objty = w_crhd-objty.
            <_operacao>-arbpl = w_crhd-arbpl.
          ENDIF.

          LOOP AT gt_conf INTO DATA(_conf) WHERE aufnr = <_operacao>-aufnr
                                            AND  vornr = <_operacao>-vornr.

            CHECK sy-subrc EQ 0 AND _conf-stzhl IS INITIAL.

            APPEND INITIAL LINE TO <_operacao>-apontamentos ASSIGNING FIELD-SYMBOL(<fs_apontamento>).
            MOVE-CORRESPONDING _conf TO <fs_apontamento>.

            READ TABLE lt_conf2 ASSIGNING FIELD-SYMBOL(<fs_conf2>)
            WITH KEY aufnr = _conf-aufnr
                     vornr = _conf-vornr
                     stzhl = _conf-rmzhl
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_apontamento>-stokz = abap_true.
            ENDIF.

            READ TABLE lt_afvc ASSIGNING FIELD-SYMBOL(<fs_afvc>)
            WITH KEY aufpl = _conf-aufpl
                     vornr = _conf-vornr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              READ TABLE lt_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>)
              WITH KEY objid = <fs_afvc>-arbid
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                <fs_apontamento>-arbpl = <fs_crhd>-arbpl.
              ENDIF.
            ENDIF.

            <fs_apontamento>-id         = _conf-rmzhl.
**        <fs_d_apont>-created_at = <fs_afru>-ersda.
*            <fs_apontamento>-updated_at = _conf-laeda.

            IF _conf-ersda IS NOT INITIAL.
              CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
                EXPORTING
                  i_date = _conf-ersda   " Data
*                 i_time = ''  " Hora
                IMPORTING
                  e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

              lv_string = lv_created.
              REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
              CONDENSE lv_string NO-GAPS.

              <fs_apontamento>-created_at = lv_string(13).

            ENDIF.

            IF _conf-laeda IS NOT INITIAL.
              CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
                EXPORTING
                  i_date = _conf-laeda   " Data
*                 i_time = ''  " Hora
                IMPORTING
                  e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

              lv_string = lv_updated.
              REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
              CONDENSE lv_string NO-GAPS.

              <fs_apontamento>-updated_at = lv_string(13).

            ENDIF.

            IF _conf-aueru EQ abap_true AND _conf-stokz IS INITIAL. "#96115  FF
              <_operacao>-confnl = _conf-aueru.
              CLEAR <_operacao>-phflg.                      "#96115  FF

            ELSE.

              CLEAR <_operacao>-confnl.                     "#96115  FF
              CONTINUE.
            ENDIF.
          ENDLOOP.

          <_operacao>-materiais = lt_materiais.
          REFRESH lt_materiais.

          APPEND  <_operacao> TO <ls_ordem>-operacao.

        ENDLOOP.

      ENDIF.

      READ TABLE lt_afko ASSIGNING FIELD-SYMBOL(<fs_afko>)
      WITH KEY aufnr = <ls_ordem>-aufnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        READ TABLE lt_resb TRANSPORTING NO FIELDS
        WITH KEY rsnum = <fs_afko>-rsnum
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_resb ASSIGNING FIELD-SYMBOL(<fs_resbd>) FROM sy-tabix.
            IF <fs_afko>-rsnum <> <fs_resbd>-rsnum.
              EXIT.
            ENDIF.

            IF <fs_resbd>-enmng <> 0.

              READ TABLE <ls_ordem>-operacao ASSIGNING FIELD-SYMBOL(<fs_oper>)
              WITH KEY vornr = <fs_resbd>-vornr.
              IF sy-subrc IS INITIAL.
                <fs_oper>-resreq = abap_true.
              ENDIF.

            ELSEIF <fs_resbd>-ebeln IS NOT INITIAL. "<fs_resbd>-flg_purs IS NOT INITIAL.

              READ TABLE lt_ekko TRANSPORTING NO FIELDS
              WITH KEY ebeln = <fs_resbd>-ebeln
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                READ TABLE <ls_ordem>-operacao ASSIGNING <fs_oper>
                WITH KEY vornr = <fs_resbd>-vornr.
                IF sy-subrc IS INITIAL.
                  <fs_oper>-resreq = abap_true.
                ENDIF.
              ENDIF.

            ELSE.

              IF <fs_resbd>-banfn IS NOT INITIAL AND
                 <fs_resbd>-bnfpo IS NOT INITIAL.

                SELECT SINGLE frgzu
                  FROM eban
                  INTO @DATA(lv_frgzu)
                  WHERE banfn = @<fs_resbd>-banfn
                    AND bnfpo = @<fs_resbd>-bnfpo.
                IF sy-subrc IS INITIAL.

                  IF lv_frgzu IS NOT INITIAL.

                    READ TABLE <ls_ordem>-operacao ASSIGNING <fs_oper>
                    WITH KEY vornr = <fs_resbd>-vornr.
                    IF sy-subrc IS INITIAL.
                      <fs_oper>-resreq = abap_true.
                    ENDIF.

                  ENDIF.

                ENDIF.

              ENDIF.

            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDIF.
**   Get components to order (all operations)
*      CALL FUNCTION 'CO_BC_RESBD_TAB_TO_ORDER_GET'
*        EXPORTING
*          aufnr_imp          = is_caufvd-aufnr
*          flg_check_log_loe  = space
*          flg_check_vbkz_del = abap_true
*          rsnum_imp          = is_caufvd-rsnum
*        TABLES
*          resbd_tab          = lt_resbd
*        EXCEPTIONS
*          error_message      = 1
*          OTHERS             = 2.
** read order components
*      CALL FUNCTION 'IBAPI_ALM_ORDER_COMP_READ'
*        EXPORTING
*          iv_orderid    = <ls_ordem>-aufnr
*        TABLES
*          et_components = lt_componentes
*          et_messages   = lt_messages.
*
**      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
**        EXPORTING
**          number        = <w_ordem>-aufnr
**        TABLES
**          et_components = lt_componentes
**          et_operations = lt_operations
**          return        = lt_return.
*
*      IF lt_componentes IS NOT INITIAL.
*        SORT lt_componentes BY activity.
*
*        LOOP AT lt_componentes ASSIGNING FIELD-SYMBOL(<fs_componentes>).
*
*          IF <fs_componentes>-withd_quan <> 0.
*
*            READ TABLE <ls_ordem>-operacao ASSIGNING FIELD-SYMBOL(<fs_oper>)
*            WITH KEY vornr = <fs_componentes>-activity.
*            IF sy-subrc IS INITIAL.
*              <fs_oper>-resreq = abap_true.
*            ENDIF.
*
*          ELSEIF <fs_componentes>-deliv_qty <> 0.
*
*            READ TABLE <ls_ordem>-operacao ASSIGNING <fs_oper>
*            WITH KEY vornr = <fs_componentes>-activity.
*            IF sy-subrc IS INITIAL.
*              <fs_oper>-resreq = abap_true.
*            ENDIF.
*
*          ELSEIF <fs_componentes>-purchase_order_exists IS NOT INITIAL.
*
*            READ TABLE <ls_ordem>-operacao ASSIGNING <fs_oper>
*            WITH KEY vornr = <fs_componentes>-activity.
*            IF sy-subrc IS INITIAL.
*              <fs_oper>-resreq = abap_true.
*            ENDIF.
*
*          ELSE.
*
*            IF <fs_componentes>-preq_no IS NOT INITIAL AND
*              <fs_componentes>-preq_item IS NOT INITIAL.
*
*              SELECT SINGLE frgzu
*                FROM eban
*                INTO @DATA(lv_frgzu)
*                WHERE banfn = @<fs_componentes>-preq_no
*                  AND bnfpo = @<fs_componentes>-preq_item.
*              IF sy-subrc IS INITIAL.
*
*                IF lv_frgzu IS NOT INITIAL.
*
*                  READ TABLE <ls_ordem>-operacao ASSIGNING <fs_oper>
*                  WITH KEY vornr = <fs_componentes>-activity.
*                  IF sy-subrc IS INITIAL.
*                    <fs_oper>-resreq = abap_true.
*                  ENDIF.
*
*                ENDIF.
*
*              ENDIF.
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDLOOP.
*
*        REFRESH: lt_componentes.
*
*      ENDIF.

      CLEAR: _t356_t, _iflo, _t353i_t, _v_auart, _eqkt, _crhd.
    ENDLOOP.

*    DELETE d_ordem WHERE marc_status NE abap_true.
*
*
*    "Preencher dados da operação.
*    DATA: _d_ordem TYPE TABLE OF aufk.
*    SELECT *
*    FROM afko AS a
*    INNER JOIN afvc AS b ON b~aufpl EQ a~aufpl
*    INNER JOIN afvv AS c ON c~aufpl EQ b~aufpl AND c~aplzl EQ b~aplzl
*    INTO CORRESPONDING FIELDS OF TABLE d_operacao
*      FOR ALL ENTRIES IN d_ordem
*      WHERE a~aufnr EQ d_ordem-aufnr
*       AND b~werks EQ filial
*       AND b~loekz EQ abap_false.
*    SORT d_operacao ASCENDING BY aufnr vornr.
*
*    FREE gt_conf.
*    SELECT *
*    FROM afru
*    INTO TABLE gt_conf
*      FOR ALL ENTRIES IN d_operacao
*      WHERE aufnr EQ d_operacao-aufnr
*        AND vornr EQ d_operacao-vornr.
*
*    SORT gt_conf ASCENDING BY aufnr vornr aueru.
*    DELETE gt_conf WHERE aueru NE abap_true.
*
*    SORT d_operacao BY aufnr.

*    LOOP AT d_ordem ASSIGNING FIELD-SYMBOL(<w_ordem>).
*      READ TABLE d_operacao TRANSPORTING NO FIELDS
*      WITH KEY aufnr = <w_ordem>-aufnr
*      BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*
*        LOOP AT d_operacao ASSIGNING FIELD-SYMBOL(<_operacao>) FROM sy-tabix.
*
*          IF <w_ordem>-aufnr <> <_operacao>-aufnr.
*            EXIT.
*          ENDIF.
*
*          IF <_operacao>-steus EQ 'PM01'.
*            <_operacao>-steus = '1'.
*          ELSE.
*            <_operacao>-steus = '0'.
*          ENDIF.
*
*          CLEAR: s_crhd.
*          SELECT SINGLE *
*          FROM crhd
*          INTO @DATA(w_crhd)
*            WHERE objid EQ @<_operacao>-arbid.
*          IF sy-subrc EQ 0.
*            <_operacao>-objty = w_crhd-objty.
*          ENDIF.
*          CLEAR s_crhd.
*
*          LOOP AT gt_conf INTO DATA(_conf) WHERE aufnr = <_operacao>-aufnr
*                                            AND  vornr = <_operacao>-vornr.
*
*            CHECK sy-subrc EQ 0.
*
*            IF _conf-aueru EQ abap_true.
*              <_operacao>-confnl = _conf-aueru.
*            ELSE.
*              CONTINUE.
*            ENDIF.
*          ENDLOOP.
*
*          APPEND  <_operacao> TO <w_ordem>-operacao.
*        ENDLOOP.
*
*      ENDIF.
*
** read order components
*      CALL FUNCTION 'IBAPI_ALM_ORDER_COMP_READ'
*        EXPORTING
*          iv_orderid    = <w_ordem>-aufnr
*        TABLES
*          et_components = lt_componentes
*          et_messages   = lt_messages.
*
**      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
**        EXPORTING
**          number        = <w_ordem>-aufnr
**        TABLES
**          et_components = lt_componentes
**          et_operations = lt_operations
**          return        = lt_return.
*
*      IF lt_componentes IS NOT INITIAL.
*        SORT lt_componentes BY activity.
*
*        LOOP AT lt_componentes ASSIGNING FIELD-SYMBOL(<fs_componentes>).
*
*          IF <fs_componentes>-withd_quan <> 0.
*
*            READ TABLE <w_ordem>-operacao ASSIGNING FIELD-SYMBOL(<fs_oper>)
*            WITH KEY vornr = <fs_componentes>-activity.
*            IF sy-subrc IS INITIAL.
*              <fs_oper>-resreq = abap_true.
*            ENDIF.
*
*          ELSEIF <fs_componentes>-deliv_qty <> 0.
*
*            READ TABLE <w_ordem>-operacao ASSIGNING <fs_oper>
*            WITH KEY vornr = <fs_componentes>-activity.
*            IF sy-subrc IS INITIAL.
*              <fs_oper>-resreq = abap_true.
*            ENDIF.
*
*          ELSEIF <fs_componentes>-purchase_order_exists IS NOT INITIAL.
*
*            READ TABLE <w_ordem>-operacao ASSIGNING <fs_oper>
*            WITH KEY vornr = <fs_componentes>-activity.
*            IF sy-subrc IS INITIAL.
*              <fs_oper>-resreq = abap_true.
*            ENDIF.
*
*          ELSE.
*
*            IF <fs_componentes>-preq_no IS NOT INITIAL AND
*              <fs_componentes>-preq_item IS NOT INITIAL.
*
*              SELECT SINGLE frgzu
*                FROM eban
*                INTO @DATA(lv_frgzu)
*                WHERE banfn = @<fs_componentes>-preq_no
*                  AND bnfpo = @<fs_componentes>-preq_item.
*              IF sy-subrc IS INITIAL.
*
*                IF lv_frgzu IS NOT INITIAL.
*
*                  READ TABLE <w_ordem>-operacao ASSIGNING <fs_oper>
*                  WITH KEY vornr = <fs_componentes>-activity.
*                  IF sy-subrc IS INITIAL.
*                    <fs_oper>-resreq = abap_true.
*                  ENDIF.
*
*                ENDIF.
*
*              ENDIF.
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDLOOP.
*
*      ENDIF.
*    ENDLOOP.

    "Seleciona dados da ordem de manutenção.
*    SELECT * FROM zpmt0049 INTO CORRESPONDING FIELDS OF TABLE d_ordem WHERE  iwerk EQ filial AND erdat IN data.

  ENDMETHOD.

  METHOD get_d_nota.

    DATA: r_objid  TYPE RANGE OF objid,
          r_filial TYPE RANGE OF werks_d.



    TYPES:
      BEGIN OF ty_equipment,
        iwerk TYPE equz-iwerk,
        equnr TYPE equz-equnr,
        eqart TYPE equi-eqart,
      END OF ty_equipment,


      BEGIN OF ty_catalog,
        codigo TYPE qpcd-code,
        texto  TYPE qpct-kurztext,
        grupo  TYPE qpcd-codegruppe,
      END OF ty_catalog,

      BEGIN OF ty_jest,
        objnr TYPE jest-objnr,
        stat  TYPE jest-stat,
        txt04 TYPE tj02t-txt04,
        inact TYPE jest-inact,
      END OF ty_jest,

      BEGIN OF ty_objid,
        objid TYPE crhd-objid,
      END OF ty_objid.


    DATA: gt_tq80 TYPE TABLE OF tq80.
    DATA: gt_jest TYPE TABLE OF ty_jest.
    DATA: w_jest TYPE ty_jest.
    DATA: w_qmur TYPE qmur.
    DATA: gt_catal TYPE TABLE OF zepm_d_nota.
    DATA: wa_catal TYPE ztpm_d_n_catal,
          l_name   TYPE thead-tdname.

    DATA: t_d_nota      TYPE TABLE OF zepm_d_not,
          t_sort_d_nota TYPE SORTED TABLE OF zepm_d_not WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
          t_tqscr       TYPE TABLE OF tqscr,
          t_sort_tqscr  TYPE SORTED TABLE OF tqscr WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
          t_qmfe        TYPE TABLE OF qmfe,
          t_sort_qmfe   TYPE TABLE OF qmfe,
          t_qmur        TYPE TABLE OF qmur,
          t_sort_qmur   TYPE SORTED TABLE OF qmur WITH NON-UNIQUE DEFAULT KEY, "WITH HEADER LINE,
          t_viqmma      TYPE TABLE OF viqmma,
          t_sort_viqmma TYPE TABLE OF viqmma,
          t_texto       TYPE TABLE OF tline,
          w_texto       TYPE tline,
          lv_created    TYPE timestampl,
          lv_updated    TYPE timestampl,
          lv_sobra      TYPE string,
          lv_string     TYPE string,
          lw_header     TYPE bapi2080_nothdre,
          lv_line       TYPE bsvx-sttxt,
          lr_objid      TYPE RANGE OF crhd-objid,
          lt_nota_aux   TYPE TABLE OF zepm_d_not,
          lt_objid      TYPE TABLE OF ty_objid.

    DATA: t_notlongtxt TYPE TABLE OF bapi2080_notfulltxte,
          it_return    TYPE TABLE OF bapiret2.

    READ TABLE data INTO DATA(w_date) INDEX 1.
    IF sy-subrc EQ 0.
      DATA(dt_inic) = w_date-low.
      DATA(dt_fim) = w_date-high.
    ENDIF.

    IF filial IS NOT INITIAL.
      r_filial = VALUE #( ( sign = 'I' option = 'EQ' low = filial ) ).
    ENDIF.


    SELECT *
     FROM viqmel AS a
     INNER JOIN tq80 AS b ON b~qmart = a~qmart
     INTO CORRESPONDING FIELDS OF TABLE t_d_nota
      WHERE a~qmdat IN data
        AND a~iwerk IN r_filial
*     WHERE a~qmdat BETWEEN dt_inic AND dt_fim
       AND b~qmtyp EQ '01'
*       AND a~qmart IN ( 'Y1', 'Y2', 'Y3', 'MI', 'M2', 'M3', 'P1', 'P2', 'P3' )
       AND ( EXISTS ( SELECT * FROM jest
                       WHERE objnr EQ a~objnr
                         AND inact NE abap_true ) ).

    CHECK t_d_nota IS NOT INITIAL.
    SORT t_d_nota[] BY qmnum ASCENDING.

    lt_nota_aux = t_d_nota.
    SORT lt_nota_aux BY arbpl.
    DELETE ADJACENT DUPLICATES FROM lt_nota_aux COMPARING arbpl.

    LOOP AT lt_nota_aux ASSIGNING FIELD-SYMBOL(<fs_nota_aux>).

      APPEND INITIAL LINE TO lt_objid ASSIGNING FIELD-SYMBOL(<fs_objid>).
      <fs_objid>-objid = <fs_nota_aux>-arbpl.

    ENDLOOP.

    IF lt_objid IS NOT INITIAL.
      SELECT objid,arbpl
        FROM crhd
        INTO TABLE @DATA(lt_crhd)
        FOR ALL ENTRIES IN @lt_objid
        WHERE objid = @lt_objid-objid.
      IF sy-subrc IS INITIAL.
        SORT lt_crhd BY objid.
      ENDIF.
    ENDIF.

    SELECT *
    FROM qmfe
    INTO TABLE t_qmfe
      FOR ALL ENTRIES IN t_d_nota
      WHERE qmnum EQ t_d_nota-qmnum.
    SORT t_qmfe BY otkat otgrp oteil fekat fegrp fecod.
    DELETE t_qmfe
    WHERE otkat EQ space
    AND otgrp EQ space
    AND oteil EQ space
    AND fekat EQ space
    AND fegrp EQ space
    AND  fecod EQ space.
    SORT t_qmfe BY qmnum.

    IF t_qmfe IS NOT INITIAL.
      SORT t_qmfe[] BY qmnum fenum ASCENDING.
      SELECT *
      FROM qmur
      INTO TABLE t_qmur
        FOR ALL ENTRIES IN t_qmfe
        WHERE qmnum EQ t_qmfe-qmnum
         AND  fenum EQ t_qmfe-fenum.
      SORT t_qmur[] BY urkat urgrp urcod ASCENDING.
      DELETE t_qmur[] WHERE urkat EQ space AND urgrp EQ space AND urcod EQ space.
      SORT t_qmur[] BY qmnum fenum ASCENDING.
    ENDIF.

    SELECT *
    FROM viqmma
    INTO TABLE t_viqmma
      FOR ALL ENTRIES IN t_d_nota
      WHERE qmnum EQ t_d_nota-qmnum.
    SORT t_viqmma[] BY mnkat mngrp  mncod ASCENDING.
    DELETE t_viqmma[] WHERE mnkat EQ space AND mngrp EQ space AND mncod EQ space.
    SORT t_viqmma[] BY qmnum ASCENDING.

    SELECT *
    FROM tqscr
    INTO TABLE t_tqscr
      FOR ALL ENTRIES IN t_d_nota
      WHERE qmart EQ t_d_nota-qmart
        AND qmtyp EQ '01'
        AND tabcd EQ '10\TAB00'.
    SORT t_tqscr[] BY sub03 ASCENDING.
    DELETE t_tqscr[] WHERE sub03 NE '035'.
    SORT t_tqscr[] BY qmart qmtyp tabcd ASCENDING.

    SORT t_d_nota BY qmnum.
    SORT t_viqmma BY qmnum.

    t_sort_d_nota[] = t_d_nota[].
    t_sort_tqscr[]  = t_tqscr[].
    t_sort_qmur[]   = t_qmur[].
    t_sort_qmfe[]   = t_qmfe[].

    SORT t_sort_qmfe BY qmnum.

    t_sort_viqmma[] = t_viqmma[].

    SORT t_sort_viqmma BY qmnum.

    LOOP AT t_d_nota ASSIGNING FIELD-SYMBOL(<_nota>).

      FREE: t_texto.

      READ TABLE lt_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>)
      WITH KEY objid = <_nota>-arbpl
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <_nota>-arbpl = <fs_crhd>-arbpl.
      ENDIF.
*      <_nota>-txt04 = 'LIB'.
      <_nota>-marc_status = abap_true.

      <_nota>-iwerk = <_nota>-swerk.
*      <_nota>-arjty = 'A'.
*      <_nota>-arjid = CONV #( <_nota>-arbpl ).
      CLEAR t_notlongtxt.

      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          objnr            = <_nota>-objnr
          spras            = 'P'
        IMPORTING
          line             = lv_line
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc = 0.

        IF lv_line CS 'MSPR'.
          <_nota>-istat = '1'.
        ELSEIF lv_line CS 'MSEN'.
          <_nota>-istat = '2'.
        ELSEIF lv_line CS 'MSPN'.
          <_nota>-istat = '0'.
        ENDIF.
      ENDIF.


*-IR054443 - 14.04.2021 - JT - inicio
      l_name = <_nota>-qmnum.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'LTXT'
          language                = sy-langu
          name                    = l_name
          object                  = 'QMEL'
        TABLES
          lines                   = t_texto
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      LOOP AT t_texto ASSIGNING FIELD-SYMBOL(<_text_l>).
        IF <_nota>-txtnt IS INITIAL.
          <_nota>-txtnt =  | ->{ <_text_l>-tdline }| .
        ELSE.
          <_nota>-txtnt = |{ <_nota>-txtnt } ->{ <_text_l>-tdline }|.
        ENDIF.
      ENDLOOP.

*     CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
*       EXPORTING
*         number     = <_nota>-qmnum
*       TABLES
*         notlongtxt = t_notlongtxt
*         return     = it_return.

*     LOOP AT t_notlongtxt ASSIGNING FIELD-SYMBOL(<_text>).
*       IF <_nota>-txtnt IS INITIAL.
*         <_nota>-txtnt =  | ->{ <_text>-text_line }| .
*       ELSE.
*         <_nota>-txtnt = |{ <_nota>-txtnt } ->{ <_text>-text_line }|.
*       ENDIF.
*     ENDLOOP.
*-IR054443 - 14.04.2021 - JT - fim

      READ TABLE t_tqscr ASSIGNING FIELD-SYMBOL(<_tqscr>) WITH KEY qmart = <_nota>-qmart
                                                                        qmtyp = '01'
                                                                        tabcd = '10\TAB00' BINARY SEARCH.


      IF sy-subrc EQ 0.
        <_nota>-avaria = abap_true.

        READ TABLE t_sort_qmfe TRANSPORTING NO FIELDS
        WITH KEY qmnum = <_nota>-qmnum
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          LOOP AT t_sort_qmfe ASSIGNING FIELD-SYMBOL(<w_qmfe>) FROM sy-tabix.

            IF <_nota>-qmnum <> <w_qmfe>-qmnum.
              EXIT.
            ENDIF.

            wa_catal-numero_item = <w_qmfe>-fenum.
            wa_catal-otkat = <w_qmfe>-otkat.
            wa_catal-otgrp = <w_qmfe>-otgrp.
            wa_catal-oteil = <w_qmfe>-oteil.
            wa_catal-fekat = <w_qmfe>-fekat.
            wa_catal-fegrp = <w_qmfe>-fegrp.
            wa_catal-fecod = <w_qmfe>-fecod.
            wa_catal-fetxt = <w_qmfe>-fetxt.
            IF <w_qmfe>-kzloesch EQ 'X' .

              wa_catal-eliminado = '1'.

            ELSE.

              wa_catal-eliminado = '0'.

            ENDIF.

            READ TABLE t_qmur INTO w_qmur WITH KEY qmnum = <w_qmfe>-qmnum
                                                   fenum = <w_qmfe>-fenum BINARY SEARCH.

            IF sy-subrc EQ 0.
              wa_catal-urkat  = w_qmur-urkat.
              wa_catal-urgrp  = w_qmur-urgrp.
              wa_catal-urcod  = w_qmur-urcod.
              wa_catal-urstx  = w_qmur-urtxt.
            ENDIF.

            IF wa_catal IS NOT INITIAL.
              APPEND wa_catal TO <_nota>-part_objnr.
              CLEAR: wa_catal, w_qmur.
            ENDIF.
          ENDLOOP.

        ENDIF.

      ELSE.
        <_nota>-avaria = ''.

        READ TABLE t_sort_viqmma TRANSPORTING NO FIELDS
        WITH KEY qmnum = <_nota>-qmnum
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          LOOP AT t_sort_viqmma ASSIGNING FIELD-SYMBOL(<w_qmma>) FROM sy-tabix.

            IF <_nota>-qmnum <> <w_qmma>-qmnum.
              EXIT.
            ENDIF.

            wa_catal-numero_item  = <w_qmma>-manum.
            wa_catal-mnkat =  <w_qmma>-mnkat.
            wa_catal-mngrp =  <w_qmma>-mngrp.
            wa_catal-mncod =  <w_qmma>-mncod.

            IF <w_qmma>-kzloesch EQ 'X'.
              wa_catal-eliminado = '1'.
            ELSE.
              wa_catal-eliminado = '0'.
            ENDIF.

            IF wa_catal IS NOT INITIAL.
              APPEND wa_catal TO <_nota>-part_objnr.
              CLEAR wa_catal.
            ENDIF.
          ENDLOOP.

        ENDIF.

      ENDIF.

      <_nota>-id         = <_nota>-qmnum.
*      lw_d_nota-created_at = <fs_d_nota>-erdat.
*      lw_d_nota-updated_at = <fs_d_nota>-aedat.

      IF <_nota>-erdat IS NOT INITIAL.
        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <_nota>-erdat   " Data
*           i_time = ''  " Hora
          IMPORTING
            e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_created.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <_nota>-created_at = lv_string(13).

      ENDIF.

      IF <_nota>-aedat IS NOT INITIAL.
        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <_nota>-aedat   " Data
*           i_time = ''  " Hora
          IMPORTING
            e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_updated.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <_nota>-updated_at = lv_string(13).

      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING t_d_nota TO d_nota.

*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444 - NOVA LÓGICA

*** Fim - Rubenilson Pereira - 13.06.2022 - 79444 - NOVA LÓGICA

*** Fim - Rubenilson Pereira - 13.06.2022 - 79444 - TRECHO DESCOMENTADO

*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444 - TRECHO COMENTADO
*    DELETE T_D_NOTA WHERE MARC_STATUS NE ABAP_TRUE.
*    MOVE-CORRESPONDING t_d_nota TO d_nota.



*    "Selecionar daddos da nota.
*    SELECT *
*    FROM zpmt0045
*    INTO CORRESPONDING FIELDS OF TABLE d_nota
*      WHERE iwerk EQ filial
*       AND  qmdat IN data.
*
*    IF d_nota IS NOT INITIAL.
*      SELECT *
*    FROM zpmt0046
*    INTO TABLE t_zpmt0046
*      FOR ALL ENTRIES IN d_nota
*      WHERE qmnum EQ d_nota-qmnum.
*
*
*      LOOP AT  d_nota ASSIGNING FIELD-SYMBOL(<w_nota>).
*        LOOP AT t_zpmt0046 INTO DATA(catalogo) WHERE qmnum EQ <w_nota>-qmnum.
*          APPEND VALUE #( otkat = catalogo-otkat
*                          otgrp = catalogo-otgrp
*                          oteil = catalogo-oteil
*                          fekat = catalogo-fekat
*                          fegrp = catalogo-fegrp
*                          fecod = catalogo-fecod
*                          urkat = catalogo-urkat
*                          urgrp = catalogo-urgrp
*                          urcod = catalogo-urcod
*                          mnkat = catalogo-mnkat
*                          mngrp = catalogo-mngrp
*                          mncod = catalogo-mncod ) TO <w_nota>-part_objnr.
*
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*** Inicio - Rubenilson Pereira - 13.06.2022 - 79444 - TRECHO COMENTADO
  ENDMETHOD.

  METHOD: get_d_status.

    SELECT *
    FROM tj02t
    INTO CORRESPONDING FIELDS OF TABLE d_status
      WHERE spras EQ sy-langu.
    SORT  d_status ASCENDING BY istat.

  ENDMETHOD.


  METHOD: get_d_operacao.
    DATA: gt_conf TYPE TABLE OF afru.

*    CALL METHOD get_d_ordem
*      EXPORTING
*        filial  = filial
*        data    = data
*      RECEIVING
*        d_ordem = DATA(_d_ordem).

    SELECT * FROM zpmt0051
    INTO CORRESPONDING FIELDS OF TABLE d_operacao
    WHERE werks EQ filial.
    SORT d_operacao ASCENDING BY aufnr vornr.


*    IF _d_ordem IS NOT INITIAL.
*      SELECT * FROM zpmt0051
*      INTO CORRESPONDING FIELDS OF TABLE d_operacao
*      FOR ALL ENTRIES IN _d_ordem
*      WHERE aufnr EQ _d_ordem-aufnr
*       AND werks EQ filial.
*      SORT d_operacao ASCENDING BY aufnr vornr.
*    ENDIF.


*    DATA: _D_ORDEM TYPE TABLE OF AUFK.
*    SELECT *
*    FROM afko AS a
*    INNER JOIN afvc AS b ON b~aufpl EQ a~aufpl
*    INNER JOIN afvv AS c ON c~aufpl EQ b~aufpl AND c~aplzl EQ b~aplzl
*    INTO CORRESPONDING FIELDS OF TABLE d_operacao
*      FOR ALL ENTRIES IN _d_ordem
*      WHERE a~aufnr EQ _d_ordem-aufnr
*       AND b~werks EQ filial
*       AND b~loekz EQ abap_false.
*    SORT d_operacao ASCENDING BY aufnr vornr.
*
*    FREE gt_conf.
*    SELECT *
*    FROM afru
*    INTO TABLE gt_conf
*      FOR ALL ENTRIES IN d_operacao
*      WHERE aufnr EQ d_operacao-aufnr
*        AND vornr EQ d_operacao-vornr.
*
*    SORT gt_conf ASCENDING BY aufnr vornr aueru.
*    DELETE gt_conf WHERE aueru NE abap_true.
*
*    LOOP AT d_operacao ASSIGNING FIELD-SYMBOL(<_operacao>).
*      IF <_operacao>-steus EQ 'PM01'.
*        <_operacao>-steus = '1'.
*      ELSE.
*        <_operacao>-steus = '0'.
*      ENDIF.
*
*      SELECT SINGLE *
*      FROM crhd
*      INTO @DATA(s_crhd)
*        WHERE objid EQ @<_operacao>-arbid.
*      IF sy-subrc EQ 0.
*        <_operacao>-objty = s_crhd-objty.
*      ENDIF.
*      CLEAR s_crhd.
*
*      LOOP AT gt_conf INTO DATA(_conf) WHERE aufnr = <_operacao>-aufnr
*                                        AND  vornr = <_operacao>-vornr.
*
*        CHECK sy-subrc EQ 0.
*
*        IF _conf-aueru EQ abap_true.
*          <_operacao>-confnl = _conf-aueru.
*        ELSE.
*          CONTINUE.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.

  ENDMETHOD.
  METHOD get_d_usuario_id.



*    CALL FUNCTION 'RP_GET_FIRE_DATE'.
    DATA: gt_empregado      TYPE TABLE OF ztpm_d_m_empreg,
          it_sort_empregado TYPE SORTED TABLE OF ztpm_d_m_empreg WITH NON-UNIQUE DEFAULT KEY. "WITH HEADER LINE,
    DATA: gw_empregado TYPE TABLE OF ztpm_d_m_empreg.
    DATA: gt_user TYPE TABLE OF ztpm_d_usuario.
    DATA: gw_usuario TYPE ztpm_d_usuario.
    DATA: gw_perf TYPE TABLE OF zpmt0012.
    DATA: gs_perf TYPE ztpm_d_m_perfil.
    DATA: gs_ctrab TYPE ztpm_d_m_centrab.
    DATA: gt_usuario TYPE TABLE OF ztpm_d_m_usuario.
    DATA: t_usuario TYPE TABLE OF pa0002.
    DATA: t_depart TYPE TABLE OF zhcmt0007.
    DATA: w_usuario    TYPE pad_cname.


    CLEAR d_usuario.
    SELECT *
    FROM pa0465 AS a
    INNER JOIN pa0002 AS b ON b~pernr EQ a~pernr
    LEFT  JOIN zhcmt0007 AS c ON c~pernr EQ a~pernr
    INTO CORRESPONDING FIELDS OF TABLE @d_usuario
      WHERE a~tpdoc EQ '0001'
        AND c~samaccountname EQ @user_ad.

*
*    CLEAR gt_usuario.
*    SELECT *
*    FROM pa0465
*    INTO CORRESPONDING FIELDS OF TABLE gt_usuario
*      WHERE tpdoc EQ '0001'.

    CHECK d_usuario IS NOT INITIAL.

*Consultado cadastro de empregado ativos.
    SELECT *
    FROM pa0000
    INTO TABLE @DATA(s_pa0000)
      FOR ALL ENTRIES IN @d_usuario
      WHERE pernr EQ @d_usuario-pernr
       AND  stat2  EQ '0'.

*    SELECT *
*     FROM pa0002
*     INTO CORRESPONDING FIELDS OF TABLE t_usuario
*      FOR ALL ENTRIES IN d_usuario
*       WHERE pernr EQ d_usuario-pernr.

*
*    SELECT *
*     FROM zhcmt0007
*     INTO CORRESPONDING FIELDS OF TABLE t_depart
*      FOR ALL ENTRIES IN d_usuario
*       WHERE pernr EQ d_usuario-pernr.



*Consulta cadastro de empregado.
*    SELECT * FROM zpmt0050 INTO CORRESPONDING FIELDS OF TABLE d_usuario.

*    CHECK d_usuario IS NOT INITIAL.

    SELECT a~objid a~objty b~kapid c~sobid d~pernr a~arbpl a~werks "F~KTEXT D~SNAME
    FROM crhd AS a
    INNER JOIN crca    AS b ON b~objid = a~objid
    INNER JOIN hrp1001 AS c ON c~objid = b~kapid
    INNER JOIN pa0001  AS d ON d~pernr = c~sobid
    INTO CORRESPONDING FIELDS OF TABLE gt_empregado
    FOR ALL ENTRIES IN d_usuario
    WHERE d~pernr EQ d_usuario-pernr
      AND c~otype EQ 'KA'.
    SORT gt_empregado BY pernr ASCENDING.


    SELECT  *
      FROM ztpm_d_usuario
      INTO CORRESPONDING FIELDS OF TABLE gt_user
      FOR ALL ENTRIES IN d_usuario
        WHERE pernr EQ d_usuario-pernr.
    SORT gt_user BY pernr ASCENDING.


*Atualizando a tabela deixando só empregados ativos.
    LOOP AT d_usuario ASSIGNING FIELD-SYMBOL(<w_usuario>).
      READ TABLE s_pa0000 INTO DATA(_pa0000) WITH KEY pernr = <w_usuario>-pernr.
      IF sy-subrc = 0.
        <w_usuario>-ativo = abap_true.
      ENDIF.
    ENDLOOP.

*     Excluindo empregados desativados.
    DELETE d_usuario WHERE ativo EQ abap_true.


    IF d_usuario IS NOT INITIAL.
      it_sort_empregado[] = gt_empregado[].


      LOOP AT d_usuario ASSIGNING FIELD-SYMBOL(<_funcao>).
        READ TABLE gt_user INTO DATA(w_user) WITH KEY pernr = <_funcao>-pernr BINARY SEARCH.
        IF sy-subrc EQ 0.
          <_funcao>-cpf_nr = w_user-cpf_nr.
          <_funcao>-login = w_user-login.
          <_funcao>-ativo = abap_true.
        ENDIF.

        LOOP AT it_sort_empregado ASSIGNING FIELD-SYMBOL(<c_trab>) WHERE pernr EQ <_funcao>-pernr.
*          GS_CTRAB-WERKS = <C_TRAB>-WERKS.
          gs_ctrab-objty = <c_trab>-objty.
          gs_ctrab-arbpl = <c_trab>-arbpl.
          <_funcao>-ativo = abap_true.
          APPEND gs_ctrab TO <_funcao>-cent_trab.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    DELETE d_usuario WHERE ativo EQ space.

*    Verificar se usuario esta em algum centro de trabalho ou se tem cadastro de login para acessar o sistema.
*    LOOP AT gt_usuario INTO DATA(_d_usuario).
**      READ TABLE gt_empregado INTO DATA(w_empregado) WITH KEY pernr = _d_usuario-pernr.
*
*      READ TABLE gt_user INTO gw_usuario WITH KEY pernr = _d_usuario-pernr.
*
*      READ TABLE t_usuario INTO DATA(ls_usua) WITH KEY pernr =  _d_usuario-pernr.
*      IF sy-subrc EQ 0.
*        w_usuario = ls_usua-cname.
*      ENDIF.
*
*      READ TABLE t_depart INTO DATA(ls_dep) WITH KEY pernr =  _d_usuario-pernr.
*      IF sy-subrc EQ 0.
*        DATA(w_departamento)   = ls_dep-departamento.
*        DATA(w_funcao)         = ls_dep-funcao.
*      ENDIF.
**
*      APPEND VALUE #(
*                      pernr           = _d_usuario-pernr
*                      cname           = w_usuario
*                      cpf_nr          = gw_usuario-cpf_nr
*                     departamento     = w_departamento
*                      funcao          = w_funcao
*                      login           = gw_usuario-login
*                      ) TO d_usuario.
*
*      CLEAR: gw_usuario, w_usuario, w_departamento, w_funcao.
*    ENDLOOP.



*Coletando login dos usuarios.
    SELECT *
    FROM zpmt0012
    INTO TABLE gw_perf
      FOR ALL ENTRIES IN d_usuario
      WHERE pernr EQ d_usuario-pernr.
*
    IF gw_perf IS NOT INITIAL.  "Comentado por não esta utlizando as informações no APP.
      LOOP AT  d_usuario ASSIGNING FIELD-SYMBOL(<_usuario>).
        LOOP AT gw_perf INTO DATA(_perf) WHERE pernr EQ <_usuario>-pernr.
          gs_perf-iwerk     =  _perf-iwerk.
          gs_perf-transacao =  _perf-transacao.
          gs_perf-exibir    =  _perf-exibir.
          gs_perf-modificar =  _perf-modificar.
          gs_perf-criar     =  _perf-criar.
          gs_perf-liberar   =  _perf-liberar.
          APPEND gs_perf TO <_usuario>-perfil.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_d_usuario.

*    CALL FUNCTION 'RP_GET_FIRE_DATE'.
    DATA: gt_empregado      TYPE TABLE OF ztpm_d_m_empreg,
          it_sort_empregado TYPE SORTED TABLE OF ztpm_d_m_empreg WITH NON-UNIQUE DEFAULT KEY. "WITH HEADER LINE,
    DATA: gw_empregado TYPE TABLE OF ztpm_d_m_empreg.
    DATA: gt_user TYPE TABLE OF ztpm_d_usuario.
    DATA: gw_usuario TYPE ztpm_d_usuario.
    DATA: gw_perf TYPE TABLE OF zpmt0012.
    DATA: gs_perf TYPE ztpm_d_m_perfil.
    DATA: gs_ctrab TYPE ztpm_d_m_centrab.
    DATA: gt_usuario TYPE TABLE OF ztpm_d_m_usuario.
    DATA: t_usuario TYPE TABLE OF pa0002.
    DATA: t_depart TYPE TABLE OF zhcmt0007.
    DATA: w_usuario TYPE pad_cname,
          r_user_ad TYPE RANGE OF zde_usuario,
          r_filial  TYPE RANGE OF werks_d,
          r_pernr   TYPE RANGE OF p_pernr.


    IF user_ad IS NOT INITIAL.
      r_user_ad = VALUE #( ( option = 'EQ' sign = 'I' low = user_ad ) ).
    ENDIF.

    IF filial IS NOT INITIAL.
      r_filial = VALUE #( ( option = 'EQ' sign = 'I' low = filial ) ).
    ENDIF.

    IF pernr IS NOT INITIAL.
      r_pernr = VALUE #( ( option = 'EQ' sign = 'I' low = pernr ) ).
    ENDIF.

    FREE d_usuario.

    SELECT b~pernr
           b~cname
           c~cpf_nr
           c~departamento
           c~funcao
           c~samaccountname
           c~email_ad
           c~bukrs
           c~werks
           c~bname "change of FF
    FROM pa0465 AS a
    INNER JOIN pa0002 AS b ON b~pernr EQ a~pernr
    INNER JOIN zhcmt0007 AS c ON c~pernr EQ a~pernr
    INTO CORRESPONDING FIELDS OF TABLE d_usuario
      WHERE a~pernr IN r_pernr
      AND tpdoc EQ '0001'
      AND c~samaccountname IN r_user_ad
      AND c~werks IN r_filial.

    CHECK d_usuario IS NOT INITIAL.

*Consultado cadastro de empregado ativos.
    SELECT *
    FROM pa0000
    INTO TABLE @DATA(s_pa0000)
      FOR ALL ENTRIES IN @d_usuario
      WHERE pernr EQ @d_usuario-pernr
       AND  stat2  EQ '0'.

*    SELECT *
*     FROM pa0002
*     INTO CORRESPONDING FIELDS OF TABLE t_usuario
*      FOR ALL ENTRIES IN d_usuario
*       WHERE pernr EQ d_usuario-pernr.

*
*    SELECT *
*     FROM zhcmt0007
*     INTO CORRESPONDING FIELDS OF TABLE t_depart
*      FOR ALL ENTRIES IN d_usuario
*       WHERE pernr EQ d_usuario-pernr.



*Consulta cadastro de empregado.
*    SELECT * FROM zpmt0050 INTO CORRESPONDING FIELDS OF TABLE d_usuario.

*    CHECK d_usuario IS NOT INITIAL.

    SELECT a~objid a~objty b~kapid c~sobid d~pernr a~arbpl a~werks "F~KTEXT D~SNAME
    FROM crhd AS a
    INNER JOIN crca    AS b ON b~objid = a~objid
    INNER JOIN hrp1001 AS c ON c~objid = b~kapid
    INNER JOIN pa0001  AS d ON d~pernr = c~sobid
    INTO CORRESPONDING FIELDS OF TABLE gt_empregado
    FOR ALL ENTRIES IN d_usuario
    WHERE d~pernr EQ d_usuario-pernr
      AND c~otype EQ 'KA'.
    SORT gt_empregado BY pernr ASCENDING.


    SELECT  *
      FROM ztpm_d_usuario
      INTO CORRESPONDING FIELDS OF TABLE gt_user
      FOR ALL ENTRIES IN d_usuario
        WHERE pernr EQ d_usuario-pernr.
    SORT gt_user BY pernr ASCENDING.


*Atualizando a tabela deixando só empregados ativos.
    LOOP AT d_usuario ASSIGNING FIELD-SYMBOL(<w_usuario>).
      READ TABLE s_pa0000 INTO DATA(_pa0000) WITH KEY pernr = <w_usuario>-pernr.
      IF sy-subrc = 0.
        <w_usuario>-ativo = abap_true.
      ENDIF.
    ENDLOOP.

*     Excluindo empregados desativados.
    DELETE d_usuario WHERE ativo EQ abap_true.


    IF d_usuario IS NOT INITIAL.
      it_sort_empregado[] = gt_empregado[].


      LOOP AT d_usuario ASSIGNING FIELD-SYMBOL(<_funcao>).

        READ TABLE gt_user INTO DATA(w_user) WITH KEY pernr = <_funcao>-pernr BINARY SEARCH.
        IF sy-subrc EQ 0.
          <_funcao>-cpf_nr = w_user-cpf_nr.
          <_funcao>-login = w_user-login.
          <_funcao>-ativo = abap_true.
        ENDIF.

        LOOP AT it_sort_empregado ASSIGNING FIELD-SYMBOL(<c_trab>) WHERE pernr EQ <_funcao>-pernr.
          gs_ctrab-iwerk = <c_trab>-werks.
          gs_ctrab-objty = <c_trab>-objty.
          gs_ctrab-arbpl = <c_trab>-arbpl.
          <_funcao>-ativo = abap_true.
          APPEND gs_ctrab TO <_funcao>-cent_trab.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    DELETE d_usuario WHERE ativo EQ space.

*    Verificar se usuario esta em algum centro de trabalho ou se tem cadastro de login para acessar o sistema.
*    LOOP AT gt_usuario INTO DATA(_d_usuario).
**      READ TABLE gt_empregado INTO DATA(w_empregado) WITH KEY pernr = _d_usuario-pernr.
*
*      READ TABLE gt_user INTO gw_usuario WITH KEY pernr = _d_usuario-pernr.
*
*      READ TABLE t_usuario INTO DATA(ls_usua) WITH KEY pernr =  _d_usuario-pernr.
*      IF sy-subrc EQ 0.
*        w_usuario = ls_usua-cname.
*      ENDIF.
*
*      READ TABLE t_depart INTO DATA(ls_dep) WITH KEY pernr =  _d_usuario-pernr.
*      IF sy-subrc EQ 0.
*        DATA(w_departamento)   = ls_dep-departamento.
*        DATA(w_funcao)         = ls_dep-funcao.
*      ENDIF.
**
*      APPEND VALUE #(
*                      pernr           = _d_usuario-pernr
*                      cname           = w_usuario
*                      cpf_nr          = gw_usuario-cpf_nr
*                     departamento     = w_departamento
*                      funcao          = w_funcao
*                      login           = gw_usuario-login
*                      ) TO d_usuario.
*
*      CLEAR: gw_usuario, w_usuario, w_departamento, w_funcao.
*    ENDLOOP.



*Coletando login dos usuarios.
    SELECT *
    FROM zpmt0012
    INTO TABLE gw_perf
      FOR ALL ENTRIES IN d_usuario
      WHERE pernr EQ d_usuario-pernr.
*
    IF gw_perf IS NOT INITIAL.  "Comentado por não esta utlizando as informações no APP.
      LOOP AT  d_usuario ASSIGNING FIELD-SYMBOL(<_usuario>).
        LOOP AT gw_perf INTO DATA(_perf) WHERE pernr EQ <_usuario>-pernr.
          gs_perf-iwerk     =  _perf-iwerk.
          gs_perf-transacao =  _perf-transacao.
          gs_perf-exibir    =  _perf-exibir.
          gs_perf-modificar =  _perf-modificar.
          gs_perf-criar     =  _perf-criar.
          gs_perf-liberar   =  _perf-liberar.
          gs_perf-encerrar   =  _perf-encerrar.
          gs_perf-aprovar_orc = _perf-aprovar_orc.
          APPEND gs_perf TO <_usuario>-perfil.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

* Seleção dados equipe de execução de atividade.
  METHOD get_d_equipe.

    SELECT *
    FROM zpmt0021
    INTO CORRESPONDING FIELDS OF TABLE d_equipe.

  ENDMETHOD.

  METHOD get_custo_eqpto.

    DATA: t_zpmt0057  TYPE TABLE OF zpmt0057,
          it_zpmt0057 TYPE TABLE OF zpmt0057,
          r_equnr     TYPE RANGE OF equnr,
          r_eqart     TYPE RANGE OF eqart,
          r_eqtyp     TYPE RANGE OF eqtyp,
          r_gjahr     TYPE RANGE OF gjahr,
          r_swerk     TYPE RANGE OF swerk,
          r_typbz     TYPE RANGE OF typbz,
          r_perio     TYPE RANGE OF coep-perio,
          d_equnr     TYPE equnr,
          p_inicio    TYPE coep-perio,
          p_fim       TYPE coep-perio,
          lv_line     TYPE bsvx-sttxt.

    "formatar mes.
    CLEAR: p_inicio, p_fim.
    p_inicio = mes_inicio.
    p_fim    = mes_fim.
    p_inicio = |{ p_inicio ALPHA = IN }|.
    p_fim    = |{ p_fim ALPHA = IN }|.
    CLEAR: d_equnr.


    r_perio = VALUE #( ( sign = 'I' option = 'BT' low = p_inicio high = p_fim ) ).

    IF eqpto IS NOT INITIAL.
      d_equnr = |{ eqpto ALPHA = IN }|.
      r_equnr = VALUE #( ( sign = 'I' option = 'EQ'  low = d_equnr ) ).
    ENDIF.

    IF tp_object IS NOT INITIAL.
      r_eqart = VALUE #( ( sign = 'I' option = 'EQ'  low = tp_object ) ).
    ENDIF.

    IF ct_eqpto IS NOT INITIAL.
      r_eqtyp = VALUE #( ( sign = 'I' option = 'EQ'  low = ct_eqpto ) ).
    ENDIF.

    IF filial IS NOT INITIAL.
      r_swerk = VALUE #( ( sign = 'I' option = 'EQ'  low = filial ) ).
    ENDIF.

    IF modelo IS NOT INITIAL.
      r_typbz = VALUE #( ( sign = 'I' option = 'EQ'  low = modelo ) ).
    ENDIF.


    "seleção de dados custo equipamento.
    SELECT * FROM zpmt0057
    INTO TABLE t_zpmt0057
      WHERE ano EQ ano
        AND swerk IN r_swerk
        AND eqart IN r_eqart
        AND eqtyp IN r_eqtyp
        AND tipbz IN r_typbz
        AND equnr IN r_equnr
        AND perio IN r_perio.

    IF t_zpmt0057 IS NOT INITIAL.
      it_zpmt0057 = t_zpmt0057.
      SORT it_zpmt0057 BY equnr.
      DELETE ADJACENT DUPLICATES FROM it_zpmt0057 COMPARING equnr.

*** Inicio - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
      SELECT equnr,objnr
        FROM equi
        INTO TABLE @DATA(lt_equi)
        FOR ALL ENTRIES IN @it_zpmt0057
        WHERE equnr = @it_zpmt0057-equnr.
      IF sy-subrc IS INITIAL.
        SORT lt_equi BY equnr.
      ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

      LOOP AT it_zpmt0057 ASSIGNING FIELD-SYMBOL(<ws_zpmt0057>).
        <ws_zpmt0057>-vl_cont = ''.
        <ws_zpmt0057>-vl_abast = ''.
        <ws_zpmt0057>-custo_hr = ''.
        <ws_zpmt0057>-cons_med = ''.
        <ws_zpmt0057>-vl_comb  = ''.
        <ws_zpmt0057>-vl_fl_lb = ''.
        <ws_zpmt0057>-vl_fl_lb = ''.
        <ws_zpmt0057>-vl_peca  = ''.
        <ws_zpmt0057>-vl_pneu = ''.
        <ws_zpmt0057>-vl_mate = ''.
        <ws_zpmt0057>-vl_terc = ''.
        <ws_zpmt0057>-vl_total = ''.


        LOOP AT t_zpmt0057 ASSIGNING FIELD-SYMBOL(<w_zpmt0057>) WHERE equnr = <ws_zpmt0057>-equnr.
          ADD  <w_zpmt0057>-vl_cont  TO    <ws_zpmt0057>-vl_cont.
          ADD  <w_zpmt0057>-vl_abast TO    <ws_zpmt0057>-vl_abast.
          ADD  <w_zpmt0057>-vl_comb  TO    <ws_zpmt0057>-vl_comb.
          ADD  <w_zpmt0057>-vl_fl_lb TO    <ws_zpmt0057>-vl_fl_lb.
          ADD  <w_zpmt0057>-vl_peca  TO    <ws_zpmt0057>-vl_peca.
          ADD  <w_zpmt0057>-vl_pneu  TO    <ws_zpmt0057>-vl_pneu.
          ADD  <w_zpmt0057>-vl_mate  TO    <ws_zpmt0057>-vl_mate.
          ADD  <w_zpmt0057>-vl_terc  TO    <ws_zpmt0057>-vl_terc.
          ADD  <w_zpmt0057>-vl_total  TO    <ws_zpmt0057>-vl_total.
        ENDLOOP.
        <ws_zpmt0057>-perio = ''.

        TRY.
            IF <ws_zpmt0057>-recdu = 'H'.
              IF <ws_zpmt0057>-vl_total IS NOT INITIAL AND <ws_zpmt0057>-vl_cont IS NOT INITIAL.
                <ws_zpmt0057>-custo_hr = <ws_zpmt0057>-vl_total / <ws_zpmt0057>-vl_cont.
              ENDIF.
              IF <ws_zpmt0057>-vl_abast IS NOT INITIAL AND <ws_zpmt0057>-vl_cont IS NOT INITIAL.
                <ws_zpmt0057>-cons_med = <ws_zpmt0057>-vl_abast / <ws_zpmt0057>-vl_cont.
              ENDIF.
            ELSE.
              IF <ws_zpmt0057>-vl_total IS NOT INITIAL AND <ws_zpmt0057>-vl_cont IS NOT INITIAL.
                <ws_zpmt0057>-custo_hr = <ws_zpmt0057>-vl_cont / <ws_zpmt0057>-vl_total.
              ENDIF.
              IF <ws_zpmt0057>-vl_abast IS NOT INITIAL AND <ws_zpmt0057>-vl_cont IS NOT INITIAL.
                <ws_zpmt0057>-cons_med = <ws_zpmt0057>-vl_cont / <ws_zpmt0057>-vl_abast.
              ENDIF.
            ENDIF.
          CATCH cx_root.
        ENDTRY.
      ENDLOOP.

      MOVE-CORRESPONDING it_zpmt0057 TO t_cust_eqpto.

      LOOP AT t_cust_eqpto ASSIGNING FIELD-SYMBOL(<fs_custo>).
        <fs_custo>-id         = <fs_custo>-equnr.

        READ TABLE lt_equi ASSIGNING FIELD-SYMBOL(<fs_equi>)
        WITH KEY equnr = <fs_custo>-equnr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
*** Inicio - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
          IF <fs_equi>-objnr IS NOT INITIAL.

            CALL FUNCTION 'STATUS_TEXT_EDIT'
              EXPORTING
                client           = sy-mandt
                objnr            = <fs_equi>-objnr
                spras            = sy-langu
              IMPORTING
                line             = lv_line
              EXCEPTIONS
                object_not_found = 1
                OTHERS           = 2.
            IF sy-subrc = 0.
              IF lv_line CS 'INAT' OR lv_line CS 'MREL'.
                <fs_custo>-istat = '0'.
              ELSE.
                <fs_custo>-istat = '1'.
              ENDIF.
            ENDIF.

          ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_p_desvio.

    IF filial IS NOT INITIAL.
*  Selecionando causas dos desvios por centro de planejamento.
      SELECT *
        FROM trugt
        INTO CORRESPONDING FIELDS OF TABLE p_desvio
        WHERE werks EQ filial
          AND spras EQ 'P'.
    ELSE.

      SELECT *
       FROM trugt
       INTO CORRESPONDING FIELDS OF TABLE p_desvio
       WHERE spras EQ 'P'.
    ENDIF.
  ENDMETHOD.

  METHOD get_d_apont.

    TYPES: BEGIN OF ty_afru,
             ersda TYPE afru-ersda,
             laeda TYPE afru-laeda,
             aufpl TYPE afru-aufpl.
             INCLUDE TYPE ztpm_d_m_apont.
  TYPES END OF ty_afru.

    DATA: quant_dias TYPE p DECIMALS 1 VALUE 180.
    DATA: data_dia TYPE sy-datum.
    DATA: data_proc  TYPE sy-datum,
          lt_afru    TYPE TABLE OF ty_afru,
          lv_created TYPE timestampl,
          lv_updated TYPE timestampl,
          lv_sobra   TYPE string,
          lv_string  TYPE string,
          lr_aufnr   TYPE RANGE OF afru-aufnr.

    data_dia = sy-datum.
    data_proc = ( data_dia - quant_dias ).

    DATA(lv_ordem) = ordem.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_ordem
      IMPORTING
        output = lv_ordem.

    IF lv_ordem IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_ordem ) TO lr_aufnr.
    ENDIF.


    SELECT *
    FROM afru
*    INTO CORRESPONDING FIELDS OF TABLE d_apont
    INTO CORRESPONDING FIELDS OF TABLE lt_afru
      WHERE isdd BETWEEN data_proc AND sy-datum
       AND stokz NE abap_true
       AND stzhl EQ ' '
       AND orind EQ '3'
       AND aufnr IN lr_aufnr.
    IF sy-subrc IS INITIAL.

      DATA(lt_afru_aux) = lt_afru.
      SORT lt_afru_aux BY aufpl vornr.
      DELETE ADJACENT DUPLICATES FROM lt_afru_aux COMPARING aufpl vornr.

      SELECT aufpl, vornr, arbid
        FROM afvc
        INTO TABLE @DATA(lt_afvc)
        FOR ALL ENTRIES IN @lt_afru_aux
        WHERE aufpl = @lt_afru_aux-aufpl
          AND vornr = @lt_afru_aux-vornr.
      IF sy-subrc IS INITIAL.
        SORT lt_afvc BY aufpl vornr.

        DATA(lt_afvc_aux) = lt_afvc.
        SORT lt_afvc_aux BY arbid.
        DELETE ADJACENT DUPLICATES FROM lt_afvc_aux COMPARING arbid.

        SELECT objid, arbpl
          FROM crhd
          INTO TABLE @DATA(lt_crhd)
          FOR ALL ENTRIES IN @lt_afvc_aux
          WHERE objid = @lt_afvc_aux-arbid.
        IF sy-subrc IS INITIAL.
          SORT lt_crhd BY objid.
        ENDIF.
      ENDIF.

      LOOP AT lt_afru ASSIGNING FIELD-SYMBOL(<fs_afru>).

        APPEND INITIAL LINE TO d_apont ASSIGNING FIELD-SYMBOL(<fs_d_apont>).

        MOVE-CORRESPONDING <fs_afru> TO <fs_d_apont>.

        READ TABLE lt_afvc ASSIGNING FIELD-SYMBOL(<fs_afvc>)
        WITH KEY aufpl = <fs_afru>-aufpl
                 vornr = <fs_afru>-vornr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>)
          WITH KEY objid = <fs_afvc>-arbid
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_d_apont>-arbpl = <fs_crhd>-arbpl.
          ENDIF.
        ENDIF.

        <fs_d_apont>-id         = <fs_afru>-pernr.
*        <fs_d_apont>-created_at = <fs_afru>-ersda.
        <fs_d_apont>-updated_at = <fs_afru>-laeda.

        IF <fs_afru>-ersda IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = <fs_afru>-ersda   " Data
*             i_time = ''  " Hora
            IMPORTING
              e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_created.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          <fs_d_apont>-created_at = lv_string(13).

        ENDIF.


        IF <fs_afru>-laeda IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = <fs_afru>-laeda   " Data
*             i_time = ''  " Hora
            IMPORTING
              e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_updated.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          <fs_d_apont>-updated_at = lv_string(13).

        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.

*** Inicio - Rubenilson Pereira - 23.06.2022 - CS2020000572 API Aprovação de Orçamento via APP - 81248
  METHOD get_orc_ordem.

    TYPES: BEGIN OF ty_ordens.
             INCLUDE TYPE zpmr0003.
    TYPES:   rowcolor(4) TYPE c,
             cell_color  TYPE lvc_t_scol,    " Cor da Célula
             sttxt       TYPE caufvd-sttxt,
             asttx       TYPE caufvd-asttx,
             icon        TYPE c LENGTH 4.
    TYPES : END OF ty_ordens,

    BEGIN OF ty_resb,
      werks   TYPE resb-werks,
      aufnr   TYPE resb-aufnr,
      matnr   TYPE resb-matnr,
      bdmng   TYPE resb-bdmng,
      rsnum   TYPE resb-rsnum,
      preis   TYPE afvc-preis,
      vprsv   TYPE mbew-vprsv,
      p_valor TYPE p LENGTH 13 DECIMALS 2,
    END OF ty_resb,

    BEGIN OF ty_afvc,             "Selecão de informação operação da ordem
      aufpl TYPE afvc-aufpl,
      objnr TYPE afvc-objnr,
      vornr TYPE afvc-vornr,
      werks TYPE afvc-werks,
      banfn TYPE afvc-banfn,
      preis TYPE afvc-preis,
      aufnr TYPE afko-aufnr,
    END OF ty_afvc,

    BEGIN OF ty_saida_0120.
      INCLUDE TYPE zpmr0006.
    TYPES: rowcolor(4) TYPE c,
      sttxt       TYPE caufvd-sttxt,
      asttx       TYPE caufvd-asttx,
      icon1       TYPE c LENGTH 4,
      icon        TYPE c LENGTH 4,
      cellcolor   TYPE lvc_t_scol,
      style       TYPE lvc_t_styl.
    TYPES END OF ty_saida_0120.

    DATA: lv_lines TYPE numc2,
          vlres    TYPE p DECIMALS 2,
          vlreq    TYPE p DECIMALS 2.

    DATA: esheader  TYPE bapi_alm_order_header_e,
          it_return TYPE TABLE OF bapiret2,
          it_olist  TYPE TABLE OF bapi_alm_order_objectlist,
          isheader  TYPE TABLE OF bapi_alm_order_headers_i.

    DATA: gt_ordens     TYPE TABLE OF ty_ordens,
          gt_ordens_aux TYPE TABLE OF zpmr0003,
          it_resb       TYPE TABLE OF ty_resb,
          it_afvc       TYPE TABLE OF ty_afvc,
          ls_orc_order  TYPE ztpm_orc_ordem.

    DATA: lv_aufnr       TYPE aufnr,
          lv_valor_aprov TYPE dmbtr,
          lo_util        TYPE REF TO zcl_util.

    DATA: gt_zpmr0006   TYPE TABLE OF zpmr0006,
          gt_saida_0120 TYPE TABLE OF ty_saida_0120,
          gt_fieldname  TYPE TABLE OF zstyle_fieldname,
          gw_saida_0120 TYPE ty_saida_0120,
          lr_objnr      TYPE RANGE OF bpge-objnr,
          lv_objnr      TYPE bpge-objnr.

    CLEAR: gt_zpmr0006,
           gt_saida_0120,
           gt_fieldname.

    FREE gt_ordens.

    IF supl_ordem IS NOT INITIAL.

      CALL FUNCTION 'ZPM_LISTAR_SUPLEMENTOS1'
        EXPORTING
          username      = usuario
        TABLES
          t_suplementos = gt_zpmr0006.

      CREATE OBJECT lo_util.


      LOOP AT gt_zpmr0006 INTO DATA(_zpmr0006).

        APPEND INITIAL LINE TO t_orc_ordem ASSIGNING FIELD-SYMBOL(<fs_ordem_supl>).

        MOVE-CORRESPONDING _zpmr0006 TO gw_saida_0120.

        gw_saida_0120-icon1 = '@DH@'.

        IF gw_saida_0120-obs_reprov IS NOT INITIAL.
          gw_saida_0120-icon = '@DH@'.
        ELSE.
          gw_saida_0120-icon = abap_false.
        ENDIF.

        SELECT SINGLE *
           FROM aufk
           INTO @DATA(w_aufk)
           WHERE aufnr EQ @_zpmr0006-aufnr
             AND werks EQ @_zpmr0006-werks.

*-> reread order status text in print language
        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            flg_user_stat    = abap_true
            objnr            = w_aufk-objnr
            only_active      = abap_true
            spras            = sy-langu
          IMPORTING
            line             = gw_saida_0120-sttxt
            user_line        = gw_saida_0120-asttx
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.

        IF gw_saida_0120-asttx IS NOT INITIAL.
          SELECT SINGLE * FROM  tj30t INTO @DATA(w_tj30t)
                 WHERE  stsma       = 'ZPM00010'
                 AND    txt04       = @gw_saida_0120-asttx
                 AND    spras       = @sy-langu.

          IF w_tj30t IS NOT INITIAL.
            gw_saida_0120-asttx = |{ gw_saida_0120-asttx } - { w_tj30t-txt30 }| .
          ENDIF.
        ENDIF.

*      "Inicio USER STORY 75882 - Anderson Oenning - 20/05/2022
        "Selecionar dados local / Centro de trabalho.

        CLEAR: esheader.
        FREE: it_return, it_olist.
        CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
          EXPORTING
            number    = _zpmr0006-aufnr
          IMPORTING
            es_header = esheader
          TABLES
            return    = it_return
            et_olist  = it_olist
          EXCEPTIONS
            OTHERS    = 01.

        IF sy-subrc EQ 0.
          gw_saida_0120-tplnr = esheader-funct_loc.
          gw_saida_0120-arbpl = esheader-mn_wk_ctr.
        ENDIF.


*      "Fim USER STORY 75882

        APPEND gw_saida_0120 TO gt_saida_0120.

        <fs_ordem_supl>-id            = gw_saida_0120-aufnr.
        <fs_ordem_supl>-aufnr         = gw_saida_0120-aufnr.
        <fs_ordem_supl>-iwerk         = gw_saida_0120-werks.
        <fs_ordem_supl>-arbpl         = gw_saida_0120-arbpl.
        <fs_ordem_supl>-short_text    = gw_saida_0120-observacao.
        <fs_ordem_supl>-tplnr         = gw_saida_0120-tplnr.
        <fs_ordem_supl>-equnr         = esheader-equipment.
        <fs_ordem_supl>-observacao    = gw_saida_0120-obs_reprov.
        IF <fs_ordem_supl>-equnr IS NOT INITIAL.

          SELECT SINGLE eqktx
            FROM eqkt
            INTO <fs_ordem_supl>-eqktx
            WHERE equnr = <fs_ordem_supl>-equnr
              AND spras = sy-langu.

        ENDIF.
        <fs_ordem_supl>-istat         = gw_saida_0120-asttx.
        <fs_ordem_supl>-erdat         = gw_saida_0120-dt_solicitacao.
        <fs_ordem_supl>-vlr_estimado  = gw_saida_0120-vlr_estimado.

      ENDLOOP.

    ELSE.

      CALL FUNCTION 'ZPM_LISTAR_PERMITS1'
        EXPORTING
          i_user   = usuario
        IMPORTING
          e_lines  = lv_lines
        TABLES
          t_ordens = gt_ordens_aux.

      MOVE-CORRESPONDING gt_ordens_aux TO gt_ordens.
      IF gt_ordens IS NOT INITIAL.
        LOOP AT gt_ordens ASSIGNING FIELD-SYMBOL(<ls_ordens>).

          SELECT *
          FROM afko AS a
          INNER JOIN resb AS b ON b~rsnum = a~rsnum
          INTO CORRESPONDING FIELDS OF TABLE it_resb
          WHERE a~aufnr EQ <ls_ordens>-aufnr
            AND b~werks EQ <ls_ordens>-werks.


          SELECT SINGLE *
           FROM aufk
           INTO w_aufk
           WHERE aufnr EQ <ls_ordens>-aufnr
             AND werks EQ <ls_ordens>-werks.

*-> reread order status text in print language
          CALL FUNCTION 'STATUS_TEXT_EDIT'
            EXPORTING
              flg_user_stat    = abap_true
              objnr            = w_aufk-objnr
              only_active      = abap_true
              spras            = sy-langu
            IMPORTING
              line             = <ls_ordens>-sttxt
              user_line        = <ls_ordens>-asttx
            EXCEPTIONS
              object_not_found = 1
              OTHERS           = 2.

*        IF <ls_ordens>-asttx IS NOT INITIAL.
*          SELECT SINGLE * FROM  tj30t INTO @DATA(w_tj30t)
*                 WHERE  stsma       = 'ZPM00010'
*                 AND    txt04       = @<ls_ordens>-asttx
*                 AND    spras       = @sy-langu.
*
*          IF w_tj30t IS NOT INITIAL.
*            <ls_ordens>-asttx =  w_tj30t-txt30 .
*          ENDIF.
*        ENDIF.


          IF it_resb IS NOT INITIAL.
            LOOP AT it_resb ASSIGNING FIELD-SYMBOL(<ls_resb>) WHERE aufnr EQ <ls_ordens>-aufnr.
              SELECT SINGLE *
              INTO @DATA(ls_mbew)
              FROM mbew
              WHERE matnr EQ @<ls_resb>-matnr
                AND bwkey EQ @<ls_resb>-werks.


              IF sy-subrc = 0.
                <ls_resb>-vprsv = ls_mbew-vprsv.

                IF <ls_resb>-vprsv = 'V'.
                  <ls_resb>-preis = ls_mbew-stprs.
                ELSE.
                  <ls_resb>-preis = ls_mbew-verpr.
                ENDIF.

                <ls_resb>-p_valor = ( <ls_resb>-bdmng * <ls_resb>-preis ).
              ENDIF.

              ADD <ls_resb>-p_valor TO <ls_ordens>-wert2.
            ENDLOOP.
          ENDIF.

          SELECT *
          FROM afvc AS a
          INNER JOIN afko AS b ON b~aufpl EQ a~aufpl AND b~aufnr EQ <ls_ordens>-aufnr
          INTO CORRESPONDING FIELDS OF TABLE it_afvc.

          LOOP AT it_afvc ASSIGNING FIELD-SYMBOL(<ls_afvc>) WHERE aufnr EQ <ls_ordens>-aufnr.
            IF sy-subrc = 0.
              ADD <ls_afvc>-preis TO <ls_ordens>-wert2.
            ENDIF.
          ENDLOOP.

          CLEAR: esheader.
          FREE: it_return, it_olist.
          CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
            EXPORTING
              number    = <ls_ordens>-aufnr
            IMPORTING
              es_header = esheader
            TABLES
              return    = it_return
              et_olist  = it_olist
            EXCEPTIONS
              OTHERS    = 01.

          IF sy-subrc EQ 0.
            <ls_ordens>-tplnr = esheader-funct_loc.
            <ls_ordens>-arbpl = esheader-mn_wk_ctr.
          ENDIF.

        ENDLOOP.

        DATA(lt_ordens) = gt_ordens.
        SORT lt_ordens BY aufnr.
        DELETE ADJACENT DUPLICATES FROM lt_ordens COMPARING aufnr.

        SELECT aufnr,obs_reprov,belnr
          FROM zpmr0006
          INTO TABLE @DATA(lt_0006)
          FOR ALL ENTRIES IN @lt_ordens
          WHERE aufnr EQ @lt_ordens-aufnr.
        IF sy-subrc IS INITIAL.
          SORT lt_0006 BY aufnr.
        ENDIF.

        LOOP AT gt_ordens ASSIGNING <ls_ordens>.

          ls_orc_order-id           = <ls_ordens>-aufnr.
          ls_orc_order-aufnr        = <ls_ordens>-aufnr.
          ls_orc_order-iwerk        = <ls_ordens>-werks.
          ls_orc_order-arbpl        = <ls_ordens>-arbpl.
          ls_orc_order-ktext        = <ls_ordens>-ktext.

          READ TABLE lt_0006 ASSIGNING FIELD-SYMBOL(<fs_0006>)
          WITH KEY aufnr = <ls_ordens>-aufnr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_orc_order-short_text = <fs_0006>-obs_reprov.
          ENDIF.

          ls_orc_order-user4        = <ls_ordens>-user4.
          ls_orc_order-cost_plan    = <ls_ordens>-wert2.
          ls_orc_order-tplnr        = <ls_ordens>-tplnr.

          CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
            EXPORTING
              number    = <ls_ordens>-aufnr
            IMPORTING
              es_header = esheader
            TABLES
              return    = it_return
              et_olist  = it_olist
            EXCEPTIONS
              OTHERS    = 01.
          IF sy-subrc IS INITIAL.
            ls_orc_order-equnr        = esheader-equipment.

            IF ls_orc_order-equnr IS NOT INITIAL.

              SELECT SINGLE eqktx
                FROM eqkt
                INTO ls_orc_order-eqktx
                WHERE equnr = ls_orc_order-equnr
                  AND spras = sy-langu.

            ENDIF.

          ENDIF.

          ls_orc_order-istat        = <ls_ordens>-asttx.
          ls_orc_order-erdat        = <ls_ordens>-erdat.

          APPEND ls_orc_order TO t_orc_ordem.
          CLEAR ls_orc_order.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.
*** Fim - Rubenilson Pereira - 23.06.2022 - CS2020000572 API Aprovação de Orçamento via APP - 81248

  METHOD get_consult_mat.

    TYPES: lr_range TYPE RANGE OF maktx.

    DATA: lt_consult_mat TYPE TABLE OF ztpm_d_m_material,
          lv_mtart       TYPE mara-mtart,
          lv_matkl       TYPE mara-matkl,
          lt_param       TYPE TABLE OF ztparam,
          lv_begru       TYPE mara-begru,
          lt_tpmat       TYPE TABLE OF string,
          ls_consult     TYPE ztpm_d_m_material.

    CALL FUNCTION 'Z_ACHA_PARAM_PM'
      EXPORTING
        i_param = 'MOBMAN'
        i_const = 'MATERIAL'
      TABLES
        t_param = lt_param.
    IF lt_param IS NOT INITIAL.
      SORT lt_param BY zindex.

      READ TABLE lt_param ASSIGNING FIELD-SYMBOL(<fs_param>)
      WITH KEY zindex = 'GRME'
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lv_matkl = <fs_param>-zval.
      ENDIF.

      READ TABLE lt_param ASSIGNING <fs_param>
      WITH KEY zindex = 'GRAU'
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lv_begru = <fs_param>-zval.
      ENDIF.

    ENDIF.

    IF matnr IS NOT INITIAL.

      DATA(lv_matnr1) = matnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_matnr1
        IMPORTING
          output = lv_matnr1.

      SELECT SINGLE matnr,meins,matkl
        FROM mara
        INTO @DATA(ls_matnr)
        WHERE matnr = @lv_matnr1
*          AND matkl = @lv_matkl
          AND mstae EQ @abap_false.
      IF sy-subrc IS INITIAL.

        SELECT SINGLE *
          FROM t023
          INTO @DATA(ls_t023)
         WHERE matkl = @ls_matnr-matkl
          AND  begru = @lv_begru.
        IF sy-subrc IS NOT INITIAL.
          APPEND INITIAL LINE TO t_consult ASSIGNING FIELD-SYMBOL(<fs_consult_mat>).
          <fs_consult_mat>-mensagem = 'Material não possue centro cadastrado!'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          FROM t006a
          INTO @DATA(ls_t006a)
          WHERE spras = @sy-langu
            AND msehi = @ls_matnr-meins.

        SELECT SINGLE matnr,maktx
          FROM makt
          INTO @DATA(ls_maktx)
          WHERE matnr = @lv_matnr1
            AND spras = @sy-langu.
        IF sy-subrc IS INITIAL.
          ls_matnr-matnr = ls_maktx-matnr.
        ENDIF.

        SELECT matnr,werks
         FROM marc
         INTO TABLE @DATA(lt_marc)
         WHERE matnr = @lv_matnr1.
        IF sy-subrc IS NOT INITIAL.
          APPEND INITIAL LINE TO t_consult ASSIGNING <fs_consult_mat>.
          <fs_consult_mat>-mensagem = 'Material não possue centro cadastrado!'.

        ELSE.

          DATA(lt_marc_aux1) = lt_marc.

          SORT lt_marc_aux1 BY matnr werks.
          DELETE ADJACENT DUPLICATES FROM lt_marc_aux1 COMPARING matnr werks.

          SELECT matnr,bwkey,verpr
            FROM mbew
            INTO TABLE @DATA(lt_mbew)
            FOR ALL ENTRIES IN @lt_marc_aux1
            WHERE matnr = @lt_marc_aux1-matnr
              AND bwkey = @lt_marc_aux1-werks.
          IF sy-subrc IS INITIAL.
            SORT lt_mbew BY matnr bwkey.
          ENDIF.

          SELECT matnr,
                 werks,
                 lgort
            FROM mard
            INTO TABLE @DATA(lt_mard)
            FOR ALL ENTRIES IN @lt_marc_aux1
            WHERE matnr = @lt_marc_aux1-matnr
              AND werks = @lt_marc_aux1-werks.
          IF sy-subrc IS INITIAL.
            SORT lt_mard BY matnr werks.
          ENDIF.

          LOOP AT lt_marc ASSIGNING FIELD-SYMBOL(<fs_marc>).

            READ TABLE lt_mard ASSIGNING FIELD-SYMBOL(<fs_mard>)
            WITH KEY matnr = <fs_marc>-matnr
                     werks = <fs_marc>-werks
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              LOOP AT lt_mard ASSIGNING <fs_mard> FROM sy-tabix.
                IF <fs_marc>-matnr <> <fs_mard>-matnr OR
                   <fs_marc>-werks <> <fs_mard>-werks.
                  EXIT.
                ENDIF.

                APPEND INITIAL LINE TO t_consult ASSIGNING FIELD-SYMBOL(<fs_consult>).

                <fs_consult>-matnr = <fs_marc>-matnr.
                <fs_consult>-maktx = ls_maktx-maktx.
                <fs_consult>-werks = <fs_marc>-werks.
                <fs_consult>-mseh3 = ls_t006a-mseh3.


                READ TABLE lt_mbew ASSIGNING FIELD-SYMBOL(<fs_mbew>)
                WITH KEY matnr = <fs_marc>-matnr
                         bwkey = <fs_marc>-werks
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  <fs_consult>-verpr = <fs_mbew>-verpr.
                ENDIF.

                <fs_consult>-lgort = <fs_mard>-lgort.
              ENDLOOP.

            ENDIF.

          ENDLOOP.

        ENDIF.


      ELSE.
        APPEND INITIAL LINE TO t_consult ASSIGNING <fs_consult_mat>.
        <fs_consult_mat>-mensagem = 'Material inválido!'.

      ENDIF.

    ELSEIF maktx IS NOT INITIAL.


      DATA(lr_maktx) = VALUE lr_range( sign   = 'I'
                                       option = 'CP'
                                       ( low    = maktx ) ).

      SELECT a~matnr,a~maktx,b~meins,b~matkl
         FROM makt AS a
         INNER JOIN mara AS b
         ON a~matnr = b~matnr
         INTO TABLE @DATA(lt_maktx)
         WHERE a~maktx IN @lr_maktx
           AND a~spras = @sy-langu
*           AND b~matkl = @lv_matkl
           AND b~mstae = @abap_false.
      IF sy-subrc IS NOT INITIAL.

        DATA(lt_maktx_aux) = lt_maktx.

        SORT lt_maktx_aux BY meins.
        DELETE ADJACENT DUPLICATES FROM lt_maktx_aux COMPARING meins.

        APPEND INITIAL LINE TO t_consult ASSIGNING <fs_consult_mat>.
        <fs_consult_mat>-mensagem = 'Material inválido!'.

      ELSE.

        SELECT *
          FROM t023
          INTO TABLE @DATA(lt_t023)
          FOR ALL ENTRIES IN @lt_maktx
         WHERE matkl = @lt_maktx-matkl
          AND  begru = @lv_begru.
        IF sy-subrc IS INITIAL.
          SORT lt_t023 BY matkl.
        ENDIF.

        SORT lt_maktx BY matnr.

        lt_maktx_aux = lt_maktx.

        DELETE ADJACENT DUPLICATES FROM lt_maktx_aux COMPARING matnr.

        SELECT matnr werks
          FROM marc
          INTO TABLE lt_marc
          FOR ALL ENTRIES IN lt_maktx_aux
          WHERE matnr = lt_maktx_aux-matnr.
        IF sy-subrc IS INITIAL.
          SORT lt_marc BY matnr.

          lt_marc_aux1 = lt_marc.

          SORT lt_marc_aux1 BY matnr werks.
          DELETE ADJACENT DUPLICATES FROM lt_marc_aux1 COMPARING matnr werks.

          SELECT matnr
                 werks
                 lgort
            FROM mard
            INTO TABLE lt_mard
            FOR ALL ENTRIES IN lt_marc_aux1
            WHERE matnr = lt_marc_aux1-matnr
              AND werks = lt_marc_aux1-werks.
          IF sy-subrc IS INITIAL.
            SORT lt_mard BY matnr werks.
          ENDIF.

          SELECT matnr bwkey verpr
            FROM mbew
            INTO TABLE lt_mbew
            FOR ALL ENTRIES IN lt_marc_aux1
            WHERE matnr = lt_marc_aux1-matnr
              AND bwkey = lt_marc_aux1-werks.
          IF sy-subrc IS INITIAL.
            SORT lt_mbew BY matnr bwkey.
          ENDIF.

          SELECT matnr,
                 meins
            FROM mara
            INTO TABLE @DATA(lt_mara)
            FOR ALL ENTRIES IN @lt_marc
            WHERE matnr = @lt_marc-matnr.
          IF sy-subrc IS INITIAL.
            DATA(lt_mara_aux) = lt_mara.

            SORT lt_mara_aux BY meins.
            DELETE ADJACENT DUPLICATES FROM lt_mara_aux COMPARING meins.

            SELECT *
              FROM t006a
              INTO TABLE @DATA(lt_t006a)
              FOR ALL ENTRIES IN @lt_mara_aux
              WHERE spras = @sy-langu
                AND msehi = @lt_mara_aux-meins.
            IF sy-subrc IS INITIAL.
              SORT lt_t006a BY msehi.
            ENDIF.
          ENDIF.
        ENDIF.

        LOOP AT lt_marc ASSIGNING <fs_marc>.

          READ TABLE lt_mard ASSIGNING <fs_mard>
                      WITH KEY matnr = <fs_marc>-matnr
                               werks = <fs_marc>-werks
                      BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            LOOP AT lt_mard ASSIGNING <fs_mard> FROM sy-tabix.
              IF <fs_marc>-matnr <> <fs_mard>-matnr OR
                 <fs_marc>-werks <> <fs_mard>-werks.
                EXIT.
              ENDIF.

              APPEND INITIAL LINE TO t_consult ASSIGNING <fs_consult>.

              <fs_consult>-matnr = <fs_marc>-matnr.

              <fs_consult>-lgort = <fs_mard>-lgort.
              READ TABLE lt_maktx ASSIGNING FIELD-SYMBOL(<fs_maktx>)
                        WITH KEY matnr = <fs_marc>-matnr
                        BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                READ TABLE lt_t023 ASSIGNING FIELD-SYMBOL(<fs_t023>)
                WITH KEY matkl = <fs_maktx>-matkl
                BINARY SEARCH.
                IF sy-subrc IS NOT INITIAL.
                  CONTINUE.
                ENDIF.

                <fs_consult>-maktx = <fs_maktx>-maktx.
                <fs_consult>-werks = <fs_marc>-werks.

                READ TABLE lt_mbew ASSIGNING <fs_mbew>
                WITH KEY matnr = <fs_marc>-matnr
                         bwkey = <fs_marc>-werks
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  <fs_consult>-verpr = <fs_mbew>-verpr.
                ENDIF.

                READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
                WITH KEY matnr = <fs_maktx>-matnr
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  READ TABLE lt_t006a ASSIGNING FIELD-SYMBOL(<fs_t006a>)
                  WITH KEY msehi = <fs_mara>-meins
                  BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    <fs_consult>-mseh3 = <fs_t006a>-mseh3.
                  ENDIF.
                ENDIF.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ELSEIF werks IS NOT INITIAL.

      SELECT matnr werks
       FROM marc
       INTO TABLE lt_marc
       WHERE werks = werks.
      IF sy-subrc IS INITIAL.

        lt_marc_aux1 = lt_marc.

        SORT lt_marc_aux1 BY matnr werks.
        DELETE ADJACENT DUPLICATES FROM lt_marc_aux1 COMPARING matnr werks.

        SELECT matnr
               werks
               lgort
          FROM mard
          INTO TABLE lt_mard
          FOR ALL ENTRIES IN lt_marc_aux1
          WHERE matnr = lt_marc_aux1-matnr
            AND werks = lt_marc_aux1-werks.
        IF sy-subrc IS INITIAL.
          SORT lt_mard BY matnr werks.
        ENDIF.

        SELECT matnr bwkey verpr
          FROM mbew
          INTO TABLE lt_mbew
          FOR ALL ENTRIES IN lt_marc_aux1
          WHERE matnr = lt_marc_aux1-matnr
            AND bwkey = lt_marc_aux1-werks.
        IF sy-subrc IS INITIAL.
          SORT lt_mbew BY matnr bwkey.
        ENDIF.

        SORT lt_marc BY matnr.

        DATA(lt_marc_aux) = lt_marc.
        SORT lt_marc_aux BY matnr.
        DELETE ADJACENT DUPLICATES FROM lt_marc_aux COMPARING matnr.

        SELECT a~matnr a~maktx b~meins b~matkl
           FROM makt AS a
           INNER JOIN mara AS b
           ON a~matnr = b~matnr
           INTO TABLE lt_maktx
          FOR ALL ENTRIES IN lt_marc_aux
           WHERE a~matnr = lt_marc_aux-matnr
             AND a~spras = sy-langu
*             AND b~matkl = lv_matkl
             AND b~mstae = abap_false.
        IF sy-subrc IS INITIAL.
          SORT lt_maktx BY matnr.

          lt_maktx_aux =  lt_maktx.

          SORT lt_maktx_aux BY matkl.
          DELETE ADJACENT DUPLICATES FROM lt_maktx_aux COMPARING matkl.

          SELECT *
            FROM t023
            INTO TABLE lt_t023
            FOR ALL ENTRIES IN lt_maktx_aux
           WHERE matkl = lt_maktx_aux-matkl
            AND  begru = lv_begru.
          IF sy-subrc IS INITIAL.
            SORT lt_t023 BY matkl.
          ENDIF.

          lt_maktx_aux =  lt_maktx.

          SORT lt_maktx_aux BY meins.
          DELETE ADJACENT DUPLICATES FROM lt_maktx_aux COMPARING meins.

          SELECT *
            FROM t006a
            INTO TABLE lt_t006a
            FOR ALL ENTRIES IN lt_maktx_aux
            WHERE spras = sy-langu
              AND msehi = lt_maktx_aux-meins.
          IF sy-subrc IS INITIAL.
            SORT lt_t006a BY msehi.
          ENDIF.

          LOOP AT lt_marc ASSIGNING <fs_marc>.

            READ TABLE lt_mard ASSIGNING <fs_mard>
                                 WITH KEY matnr = <fs_marc>-matnr
                                          werks = <fs_marc>-werks
                                 BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              LOOP AT lt_mard ASSIGNING <fs_mard> FROM sy-tabix.
                IF <fs_marc>-matnr <> <fs_mard>-matnr OR
                   <fs_marc>-werks <> <fs_mard>-werks.
                  EXIT.
                ENDIF.

                READ TABLE lt_maktx ASSIGNING <fs_maktx>
                WITH KEY matnr = <fs_marc>-matnr
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  ls_consult-maktx = <fs_maktx>-maktx.

                  READ TABLE lt_t023 ASSIGNING <fs_t023>
                  WITH KEY matkl = <fs_maktx>-matkl
                  BINARY SEARCH.
                  IF sy-subrc IS NOT INITIAL.
                    CONTINUE.
                  ENDIF.

                  READ TABLE lt_t006a ASSIGNING <fs_t006a>
                  WITH KEY msehi = <fs_maktx>-meins
                  BINARY SEARCH.
                  IF sy-subrc IS INITIAL.
                    ls_consult-mseh3 = <fs_t006a>-mseh3.
                  ENDIF.
                ELSE.
                  CONTINUE.
                ENDIF.


                ls_consult-matnr = <fs_marc>-matnr.

                ls_consult-werks = <fs_marc>-werks.
                ls_consult-lgort = <fs_mard>-lgort.

                READ TABLE lt_mbew ASSIGNING <fs_mbew>
                WITH KEY matnr = <fs_marc>-matnr
                         bwkey = <fs_marc>-werks
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  ls_consult-verpr = <fs_mbew>-verpr.
                ENDIF.

                APPEND ls_consult TO t_consult.
                CLEAR ls_consult.
              ENDLOOP.

            ENDIF.

          ENDLOOP.

        ELSE.

          APPEND INITIAL LINE TO t_consult ASSIGNING <fs_consult_mat>.
          <fs_consult_mat>-mensagem = 'Nenhum material encontrado para centro informado!'.

        ENDIF.

      ELSE.

        APPEND INITIAL LINE TO t_consult ASSIGNING <fs_consult_mat>.
        <fs_consult_mat>-mensagem = 'Centro não encontrado!'.

      ENDIF.

    ELSE.

      APPEND INITIAL LINE TO t_consult ASSIGNING <fs_consult_mat>.
      <fs_consult_mat>-mensagem = 'Favor inserir Nº Material e/ou Descrição e/ou Centro!'.

    ENDIF.

  ENDMETHOD.

  METHOD get_jornada_user.

    DATA: lwa_data_request  TYPE zde_rfo_par_faltas.
    DATA: lv_id_referencia TYPE zsdt0001-id_referencia.

    DATA: lit_saida     TYPE TABLE OF zhcms_batida_list,
          lit_saida_aux TYPE TABLE OF zhcms_batida_list,
          lit_objid     TYPE TABLE OF zhcms_objid,
          lit_pernr     TYPE TABLE OF zhcms_ret_pernr,
          lwa_pernr     LIKE LINE OF lit_pernr.

    DATA: lit_horarios   TYPE TABLE OF zhcms_horarios_excecao.

    DATA: lva_dtbatida(10) TYPE c.


    DATA:
      lwa_time1  TYPE t,
      lwa_time2  TYPE t,
      lwa_result TYPE t,
      lwa_int1   TYPE i,
      lwa_int2   TYPE i.

    DATA: lo_sap_hcm TYPE REF TO zcl_hcm_util.
    CREATE OBJECT lo_sap_hcm.

    DATA: r_subty TYPE RANGE OF pa0030-subty.
    DATA: r_pernr TYPE RANGE OF persno.

    " DATA: git_saida  TYPE TABLE OF ty_saida.
    " DATA: gwa_saida LIKE LINE OF git_saida .

    DATA: git_saida TYPE TABLE OF ts_data,
          gwa_saida TYPE ts_data.


    DATA: lit_text_tab_local  TYPE hrpad_text_tab,
          lwa_text_tab_local  LIKE LINE OF lit_text_tab_local,
          lva_message_handler TYPE REF TO  if_hrpa_message_handler,
          lva_no_auth_check   TYPE  boole_d,
          lva_is_ok           TYPE  boole_d.


    DATA: lt_saida  TYPE TABLE OF ty_jornada.
    DATA: lw_saida TYPE ty_jornada .
    TYPES lr_pernr_type TYPE RANGE OF p_pernr.
    DATA: lva_dtsaida  TYPE sy-datum,
          lit_return   TYPE zhcm_rfo_dados,
          lt_func_list TYPE TABLE OF zhcms_func_list.

*
*    /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).

*    IF e_msg_erro IS NOT INITIAL.
*
*      IF _status_code IS INITIAL .
*        _status_code = '400'. "Bad Request
*      ENDIF.
*
*      e_sucesso      = abap_true.
*      e_nm_code      = _status_code.
*      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
*                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
*                       '}'.
*      RETURN.
*    ENDIF.


    IF pernr IS NOT INITIAL.
      r_pernr = VALUE #( ( option = 'EQ' sign = 'I' low = pernr ) ).
    ENDIF.

* Ativos
    SELECT a~pernr,
           a~stell,
           a~gsber,
           b~stat2,
           c~cname
      FROM pa0001 AS a
      INNER JOIN pa0000 AS b
        ON a~pernr EQ b~pernr
      INNER JOIN pa0002 AS c
        ON a~pernr = c~pernr
       INTO TABLE @DATA(git_pa0001_atv)
        WHERE a~pernr IN @r_pernr
*          AND a~stell IN @r_stell
          AND a~endda >= @sy-datum
          AND b~endda >= @sy-datum
          AND b~stat2 EQ  '3'
          AND c~endda >= @sy-datum .

    IF git_pa0001_atv IS NOT INITIAL..

      LOOP AT git_pa0001_atv INTO DATA(lwa_pa0001_atv).

* Busca desc. situação
        SELECT SINGLE text1
           INTO @DATA(lva_text1)
           FROM t529u
          WHERE sprsl = @sy-langu
            AND statn = '2'
            AND statv = @lwa_pa0001_atv-stat2.

* Busca nome cargo
        SELECT SINGLE stext
             INTO @DATA(lva_stext)
             FROM hrp1000
            WHERE plvar = '01'
              AND otype = 'C'
              AND endda >= @sy-datum
              AND langu =  @sy-langu
              AND objid =  @lwa_pa0001_atv-stell.


* Busca nome filial
        SELECT SINGLE name1
           INTO @DATA(lva_name1)
           FROM  t001w
          WHERE werks = @lwa_pa0001_atv-gsber.


        lw_saida-matricula         =  lwa_pa0001_atv-pernr.
        lw_saida-nome              =  lwa_pa0001_atv-cname.
        lw_saida-cod_cargo         =  lwa_pa0001_atv-stell.
        lw_saida-desc_cargo        =  lva_stext.
        lw_saida-cod_filial        =  lwa_pa0001_atv-gsber.
        lw_saida-desc_filial       =  lva_name1.
        lw_saida-situacao          =  lva_text1.

        CALL FUNCTION 'ZHCMF_RETURN_FUNC'
          EXPORTING
*           stat2   = stat2
            pernr   = pernr
*           cname   = cname
          TABLES
            t_saida = lt_func_list.

        READ TABLE lt_func_list ASSIGNING FIELD-SYMBOL(<fs_func_list>) INDEX 1.
        IF sy-subrc IS INITIAL.
          lw_saida-jornada = <fs_func_list>-horario.
        ENDIF.

        APPEND lw_saida TO lt_saida.
        CLEAR:lw_saida, lva_stext, lva_name1.

      ENDLOOP.
    ENDIF.

* Inativos
    SELECT a~pernr,
           a~stell,
           a~gsber,
           b~begda,
           b~endda,
           b~massn,
           b~massg,
           b~stat2,
           c~cname
      FROM pa0001 AS a
      INNER JOIN pa0000 AS b
        ON a~pernr EQ b~pernr
      INNER JOIN pa0002 AS c
        ON a~pernr = c~pernr
       INTO TABLE @DATA(git_pa0001_ina)
        WHERE a~pernr IN @r_pernr
*          AND a~stell IN @r_stell
          AND a~endda >= @sy-datum
          AND b~begda >= '20220902'
          AND b~endda >= @sy-datum
          AND b~stat2 EQ  '0'
          AND c~endda >= @sy-datum .

    IF git_pa0001_ina IS NOT INITIAL..

      LOOP AT git_pa0001_ina INTO DATA(lwa_pa0001_ina).

* Busca desc. situação
        SELECT SINGLE text1
           INTO lva_text1
           FROM t529u
          WHERE sprsl = sy-langu
            AND statn = '2'
            AND statv = lwa_pa0001_ina-stat2.

* Busca nome cargo
        SELECT SINGLE stext
             INTO lva_stext
             FROM hrp1000
            WHERE plvar = '01'
              AND otype = 'C'
              AND endda >= sy-datum
              AND langu = sy-langu
              AND objid = lwa_pa0001_ina-stell.


* Busca nome filial
        SELECT SINGLE name1
           INTO lva_name1
           FROM  t001w
          WHERE werks = lwa_pa0001_ina-gsber.

* Busca desc. motivo saída
        SELECT SINGLE mgtxt
           INTO @DATA(lva_mgtxt)
           FROM  t530t
          WHERE massn = @lwa_pa0001_ina-massn
           AND  massg = @lwa_pa0001_ina-massg.

        lw_saida-matricula         =  lwa_pa0001_ina-pernr.
        lw_saida-nome              =  lwa_pa0001_ina-cname.
        lw_saida-cod_cargo         =  lwa_pa0001_ina-stell.
        lw_saida-desc_cargo        =  lva_stext.
        lw_saida-cod_filial        =  lwa_pa0001_ina-gsber.
        lw_saida-desc_filial       =  lva_name1.
        lw_saida-situacao          =  lva_text1. "lwa_pa0001_ina-stat2.

        CALL FUNCTION 'ZHCMF_RETURN_FUNC'
          EXPORTING
*           stat2   = stat2
            pernr   = pernr
*           cname   = cname
          TABLES
            t_saida = lt_func_list.

        READ TABLE lt_func_list ASSIGNING <fs_func_list> INDEX 1.
        IF sy-subrc IS INITIAL.
          lw_saida-jornada = <fs_func_list>-horario.
        ENDIF.

        lva_dtsaida =  ( lwa_pa0001_ina-begda - 1 ).

        CONCATENATE lva_dtsaida+6(2) '/'  lva_dtsaida+4(2) '/'  lva_dtsaida+0(4) INTO lw_saida-data_saida .


        lw_saida-cod_motivo_saida  =  lwa_pa0001_ina-massg .
        lw_saida-desc_motivo_saida =  lva_mgtxt.

        APPEND lw_saida TO lt_saida.
        CLEAR:lw_saida, lva_text1, lva_stext, lva_name1.
      ENDLOOP.
    ENDIF.

    IF  lt_saida  IS NOT INITIAL.
      MOVE lt_saida[] TO lit_return.
    ENDIF.

*    lo_sap_hcm->get_func_rem_operadores( EXPORTING
*                                                 ir_pernr  = lwa_data_request-pernr
*                                       IMPORTING et_return = DATA(lit_return) ).
*    IF  lit_return IS NOT INITIAL.
*
*      "LOOP AT lit_return INTO DATA(lwa_return).
*
**      LOOP AT lit_return INTO DATA(lwa_return).
**        lwa_pernr-pernr = lwa_return-matricula.
**        APPEND lwa_pernr TO lit_pernr.
**        CLEAR: lwa_pernr.
**      ENDLOOP.
*
**          t_objid  = t_objid[]
**          t_arearh = t_arearh[]
**          t_kostl  = t_kostl[]
***-CS2021001101 - 08.03.2022 - JT - inicio
**          t_abkrs  = t_abkrs[].
**      CALL FUNCTION 'ZHCMF_RETURN_BATIDAS'
**        EXPORTING
**          pernr   = pernr
**          begda   = lwa_data_request-begda
**          endda   = lwa_data_request-endda
**        TABLES
**          t_saida = lit_saida
**          t_objid = lit_objid
**          t_pernr = lit_pernr.
*
*      IF lit_saida IS NOT  INITIAL.
*
*        SORT   lit_saida     BY pernr data_batida.
*        LOOP AT lit_saida INTO DATA(lwa_saida).
*
*          gwa_saida-matricula     = lwa_saida-pernr.
*
*          CONCATENATE  lwa_saida-data_batida+6(2) '/'  lwa_saida-data_batida+4(2) '/' lwa_saida-data_batida+0(4) INTO lva_dtbatida .
*
*          gwa_saida-data = lva_dtbatida.
*
*          CALL FUNCTION 'ZHCMF_RETURN_HORARIOS_FUNC'
*            EXPORTING
*              pernr   = lwa_saida-pernr
*              begda   = lwa_saida-data_batida
*              endda   = lwa_saida-data_batida
*            TABLES
*              t_saida = lit_horarios.      " Tabela Horários Calendário - Ponto Exceção
*
*          IF ( lit_horarios[] IS NOT INITIAL ).
*            READ TABLE  lit_horarios INTO DATA(lwa_horarios) INDEX 1.
*            gwa_saida-inicio_jornada   = lwa_horarios-hora_inicio.
*            gwa_saida-inicio_intervalo = lwa_horarios-intervalo_ini.
*            gwa_saida-fim_intervalo    = lwa_horarios-intervalo_fim.
*            gwa_saida-fim_jornada      = lwa_horarios-hora_fim.
*          ENDIF.
*
** Soma abono + hora trabalhada: lwa_saida-htrab + lwa_saida-abono
*
*          REPLACE ALL OCCURRENCES OF ':' IN lwa_saida-htrab WITH space.
*          CONDENSE lwa_saida-htrab NO-GAPS.
*
*          REPLACE ALL OCCURRENCES OF ':' IN lwa_saida-abono WITH space.
*          CONDENSE lwa_saida-abono NO-GAPS.
*
*          IF lwa_saida-htrab IS NOT INITIAL.
*            CONCATENATE lwa_saida-htrab+0(2) lwa_saida-htrab+2(2) '00' INTO lwa_time1 .
*          ENDIF.
*          IF  lwa_saida-abono IS NOT INITIAL.
*            CONCATENATE lwa_saida-abono+0(2) lwa_saida-abono+2(2) '00' INTO lwa_time2 .
*          ENDIF.
*
*          CALL FUNCTION 'C14B_ADD_TIME'
*            EXPORTING
*              i_starttime = lwa_time1
*              i_startdate = sy-datum
*              i_addtime   = lwa_time2
*            IMPORTING
*              e_endtime   = lwa_result.
*
*          CONCATENATE lwa_result+0(2) ':' lwa_result+2(2) INTO gwa_saida-total_jornada_registrada.
*
*          gwa_saida-total_jornada_esperada   = lwa_saida-base .
*
*          IF lwa_saida-htrab >=  lwa_saida-base .
*            gwa_saida-hora_calculo = lwa_saida-htrab.
*          ELSE.
*            gwa_saida-hora_calculo = lwa_saida-base.
*          ENDIF.
*
*          IF lwa_saida-ent1 IS NOT INITIAL.
*            APPEND  lwa_saida-ent1 TO gwa_saida-batida.
*          ENDIF.
*          IF lwa_saida-sai1 IS NOT INITIAL.
*            APPEND  lwa_saida-sai1 TO gwa_saida-batida.
*          ENDIF.
*          IF lwa_saida-ent2 IS NOT INITIAL.
*            APPEND  lwa_saida-ent2 TO gwa_saida-batida.
*          ENDIF.
*          IF lwa_saida-sai2 IS NOT INITIAL.
*            APPEND  lwa_saida-sai2 TO gwa_saida-batida.
*          ENDIF.
*          IF lwa_saida-ent3 IS NOT INITIAL.
*            APPEND  lwa_saida-ent3 TO gwa_saida-batida.
*          ENDIF.
*          IF lwa_saida-sai3 IS NOT INITIAL.
*            APPEND  lwa_saida-sai3 TO gwa_saida-batida.
*          ENDIF.
*
*          CLEAR: lwa_saida, lva_dtbatida.
*
*          APPEND gwa_saida TO git_saida.
*          CLEAR: gwa_saida,
*                 lwa_time1,
*                 lwa_time2,
*                 lwa_result,
*                 lwa_int1,
*                 lwa_int2,
*                 lit_horarios,
*                 lva_dtbatida.
*
*        ENDLOOP.
*      ENDIF.
**      CLEAR: lwa_return,
**              lit_saida,
**              lit_objid.
*
*      "ENDLOOP.
*      CLEAR: lit_return.
*    ENDIF.

    IF lt_saida IS NOT INITIAL.
      t_jornada = lt_saida.
    ENDIF.
  ENDMETHOD.

***FF - 23.02.24 - #131818 - inicio
**  METHOD apont_zpm0102.
**
***Os dados virão na i_dados_apontamento
**
**
**    SELECT cpf_nr, login
**    FROM ztpm_d_usuario
**    INTO TABLE @DATA(lt_usuario)
**    WHERE pernr = @i_dados_apontamento-id.
**
**    IF sy-subrc <> 0.
**
**      CLEAR lt_usuario.
**
**      e_retorno_apont-msg_error = | { 'CPF e senha não encontrado para o empregado: '  }  && { i_dados_apontamento-id } |.
**
**    ENDIF.
**
**
**
**
**
**  ENDMETHOD.
***FF - 23.02.24 - #131818 - fim

ENDCLASS.
