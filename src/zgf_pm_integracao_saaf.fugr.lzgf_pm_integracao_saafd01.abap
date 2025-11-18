*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_INTEGRACAO_SAAFD01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class CL_MAIN_APP
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
*CLASS CL_MAIN_APP DEFINITION ABSTRACT.
*  PUBLIC SECTION.
*    CLASS-METHODS GET_PARAMETERS
*      IMPORTING
*        PARAM_KEY    TYPE ZPARAM
*        IS_ABASTEC   TYPE ABAP_BOOL
*      RETURNING
*        VALUE(TABLE) TYPE RSIS_T_RANGE.
*
*    CLASS-METHODS GET_DATE_RANGE
*      IMPORTING
*        FROM         TYPE SY-DATUM
*        TO           TYPE SY-DATUM
*      RETURNING
*        VALUE(TABLE) TYPE RSIS_T_RANGE.
*ENDCLASS.
*
*CLASS CL_MAIN_APP IMPLEMENTATION.
*  METHOD GET_PARAMETERS.
*    SELECT ZVAL, CONST
*      FROM ZTPARAM
*      INTO TABLE @DATA(_PARAMETERS)
*     WHERE PARAM   EQ @PARAM_KEY
*       AND ABASTEC EQ @IS_ABASTEC.
*
*    LOOP AT _PARAMETERS INTO DATA(_PARAMETER).
*      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = _PARAMETER-ZVAL ) TO TABLE.
*    ENDLOOP.
*  ENDMETHOD.
*
*  METHOD GET_DATE_RANGE.
*    IF TO IS INITIAL AND FROM IS NOT INITIAL.
*      TABLE = VALUE RSIS_T_RANGE( ( LOW  = FROM SIGN = 'I' OPTION = 'EQ'  ) ).
*    ELSEIF FROM IS NOT INITIAL AND TO IS NOT INITIAL.
*      TABLE = VALUE RSIS_T_RANGE( ( LOW  = FROM HIGH = TO SIGN = 'I' OPTION = 'BT'  ) ).
*    ENDIF.
*  ENDMETHOD.
*ENDCLASS.

CLASS cl_main_app DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF status_veiculo,
        active   VALUE '1',
        inactive VALUE '0',
      END OF status_veiculo.

    TYPES:
      BEGIN OF ty_equipment,
        bukrs TYPE itob-bukrs,
        swerk TYPE itob-swerk,
        equnr TYPE itob-equnr,
        herst TYPE itob-herst,
        herld TYPE itob-herld,
        eqart TYPE equi-eqart,
        typbz TYPE itob-typbz,
        objnr TYPE itob-objnr,
      END OF ty_equipment,

      BEGIN OF ty_employee,
        stell    TYPE pa0001-stell,
        begda    TYPE pa0001-begda,
        pernr    TYPE pa0001-pernr,
        werks    TYPE pa0001-werks,
        cname    TYPE pa0002-cname,
        gbdat    TYPE pa0002-gbdat,
        gesch    TYPE pa0002-gesch,
        cpf_nr   TYPE pa0465-cpf_nr,
        ident_nr TYPE pa0465-ident_nr,
      END OF ty_employee,

      BEGIN OF ty_compartimento,
        matnr TYPE mara-matnr,
        maktx TYPE makt-maktx,
        maktg TYPE makt-maktg,
      END OF ty_compartimento,

      BEGIN OF ty_local_instalacao,
        tplnr TYPE iflo-tplnr,
        msgrp TYPE iflo-msgrp,
        pltxt TYPE iflo-pltxt,
      END OF ty_local_instalacao,

      BEGIN OF ty_catalog,
        codigo TYPE qpcd-code,
        texto  TYPE qpct-kurztext,
        grupo  TYPE qpcd-codegruppe,
      END OF ty_catalog,

      BEGIN OF ty_nf,
        ebeln   TYPE ekbe-ebeln,
        xblnr   TYPE ekbe-xblnr,
        lifnr   TYPE ekko-lifnr,
        werks   TYPE ekbe-werks,
        budat   TYPE ekbe-budat,
        bldat   TYPE ekbe-bldat,
        cputm   TYPE ekbe-cputm,
        matnr   TYPE ekbe-matnr,
        menge   TYPE ekbe-menge,
        lgort   TYPE ekpo-lgort,
        station TYPE t370fld_stn-station,
        id_tq   TYPE zpmt006-id_tq,
      END OF ty_nf,

      ty_t_equipment    TYPE TABLE OF ty_equipment      WITH KEY equnr,
      ty_t_veiculo      TYPE TABLE OF ztpm_m_veic_saaf  WITH EMPTY KEY,
      ty_t_implementos  TYPE TABLE OF ztpm_m_imple_saaf  WITH EMPTY KEY,
      ty_t_diimpt       TYPE TABLE OF diimpt            WITH EMPTY KEY,
      ty_t_veiculo_comp TYPE TABLE OF ztpm_c_veic_saaf  WITH EMPTY KEY,
      ty_t_employee     TYPE TABLE OF ty_employee       WITH EMPTY KEY,
      ty_t_users        TYPE TABLE OF ztpm_m_user_saaf  WITH EMPTY KEY,
      ty_t_produtos     TYPE TABLE OF ztpm_m_prod_saaf  WITH EMPTY KEY,
      ty_t_planos_op    TYPE TABLE OF ztpm_b_plan_saaf  WITH EMPTY KEY,
      ty_t_locais       TYPE TABLE OF ztpm_loc_ab_saaf  WITH EMPTY KEY,
      ty_t_operacao     TYPE TABLE OF ztpm_r_ve_op_saaf WITH EMPTY KEY,
      ty_t_centros      TYPE TABLE OF ztpm_b_cent_saaf  WITH EMPTY KEY,
      ty_t_causas       TYPE TABLE OF ztpm_b_caus_saaf  WITH EMPTY KEY,
      ty_t_material     TYPE TABLE OF ztpm_b_comp_saaf  WITH EMPTY KEY,
      ty_t_nota_fiscal  TYPE TABLE OF ztpm_m_nf_saaf    WITH EMPTY KEY.

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
        data              TYPE rsis_t_range
        hora              TYPE rsis_t_range
      RETURNING
        VALUE(equipments) TYPE ty_t_equipment.

    METHODS get_measuring_points
      IMPORTING
        equipment     TYPE equi-equnr
      RETURNING
        VALUE(points) TYPE ty_t_diimpt.

    METHODS get_veiculos
      IMPORTING
        filial          TYPE iwerk
        situacao        TYPE c
        data            TYPE rsis_t_range
        hora            TYPE rsis_t_range
      RETURNING
        VALUE(veiculos) TYPE ty_t_veiculo.

    METHODS select_implementos
      IMPORTING
        filial            TYPE iwerk
        situacao          TYPE c
        data              TYPE rsis_t_range
        hora              TYPE rsis_t_range
      RETURNING
        VALUE(equipments) TYPE ty_t_equipment.

    METHODS get_implementos
      IMPORTING
        filial             TYPE iwerk
        situacao           TYPE c
        data               TYPE rsis_t_range
        hora               TYPE rsis_t_range
      RETURNING
        VALUE(implementos) TYPE ty_t_implementos.

    METHODS get_veiculo_compartimentos
      IMPORTING
        filial                TYPE iwerk
        situacao              TYPE c
        data                  TYPE rsis_t_range
        hora                  TYPE rsis_t_range
      RETURNING
        VALUE(compartimentos) TYPE ty_t_veiculo_comp.

    METHODS get_veiculo_operacoes
      IMPORTING
        filial           TYPE iwerk
        situacao         TYPE c
        data             TYPE rsis_t_range
        hora             TYPE rsis_t_range
      RETURNING
        VALUE(operacoes) TYPE ty_t_operacao.

    METHODS select_employees
      IMPORTING
        situacao         TYPE char1
        filial           TYPE pa0001-werks
        data             TYPE rsis_t_range
      RETURNING
        VALUE(employees) TYPE ty_t_employee.

    METHODS get_employees
      IMPORTING
        situacao         TYPE char1
        filial           TYPE pa0001-werks
        data             TYPE rsis_t_range
      RETURNING
        VALUE(employees) TYPE ty_t_users.

    METHODS get_compartimentos
      IMPORTING
        situacao              TYPE char1
        filial                TYPE pa0001-werks
        data                  TYPE rsis_t_range
      RETURNING
        VALUE(compartimentos) TYPE ty_t_material.

    METHODS get_centros_custos
      IMPORTING
        situacao       TYPE char1
        filial         TYPE pa0001-werks
        data           TYPE rsis_t_range
      RETURNING
        VALUE(centros) TYPE ty_t_centros.

    METHODS get_locais_abastecimento
      IMPORTING
        situacao      TYPE char1
        filial        TYPE pa0001-werks
        data          TYPE rsis_t_range
        hora          TYPE rsis_t_range
      RETURNING
        VALUE(locais) TYPE ty_t_locais.

    METHODS get_parameters
      IMPORTING
        param_key    TYPE zparam
        is_abastec   TYPE abap_bool
      RETURNING
        VALUE(table) TYPE rsis_t_range.

    METHODS get_produtos
      RETURNING VALUE(produtos) TYPE ty_t_produtos.

    METHODS get_causa_manutencoes
      IMPORTING
        situacao      TYPE char1
        filial        TYPE pa0001-werks
        data          TYPE rsis_t_range
      RETURNING
        VALUE(causas) TYPE ty_t_causas.

    METHODS get_entrada_nota
      IMPORTING
        situacao     TYPE char1
        filial       TYPE pa0001-werks
        data         TYPE rsis_t_range
      RETURNING
        VALUE(notas) TYPE ty_t_nota_fiscal.

    METHODS get_planos_operacoes
      IMPORTING
        situacao      TYPE char1
        filial        TYPE pa0001-werks
        data          TYPE rsis_t_range
      RETURNING
        VALUE(planos) TYPE ty_t_planos_op.

    METHODS get_inactive_status
      RETURNING VALUE(table) TYPE rsis_t_range.

    METHODS formatar_placa
      IMPORTING i_placa      TYPE license_num
      RETURNING VALUE(placa) TYPE license_num.

    METHODS get_atnam
      IMPORTING i_equnr       TYPE equnr
                i_eqart       TYPE eqart
                range         TYPE rsis_t_range
      RETURNING VALUE(return) TYPE char2.

    CLASS-METHODS check_sets_authorization
      IMPORTING input         TYPE werks_d
      RETURNING VALUE(return) TYPE sy-subrc.

  PRIVATE SECTION.
    DATA system_status TYPE TABLE OF bapi_itob_status.
    DATA user_status   TYPE TABLE OF bapi_itob_status.
ENDCLASS.

CLASS cl_main_app IMPLEMENTATION.
  METHOD get_parameters.

    SELECT zval, const
      FROM ztparam
      INTO TABLE @DATA(_parameters)
     WHERE param   EQ @param_key
       AND abastec EQ @is_abastec.

    LOOP AT _parameters INTO DATA(_parameter).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = _parameter-zval ) TO table.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_veiculos.

    DATA classif_obj   TYPE TABLE OF sclass.
    DATA object_data   TYPE TABLE OF clobjdat.
    DATA veiculo       TYPE ztpm_m_veic_saaf.
    DATA general_data  TYPE bapi_itob.
    DATA point         TYPE diimpt.
    DATA km_real_ch    TYPE c LENGTH 30.
    DATA doc_medition  TYPE imrg.
    DATA _matnr        TYPE char18.
*---> 01/06/2023 - Migração S4 - JS
    "DATA W_EQUI TYPE V_EQUI.
    DATA w_equi TYPE equi.
*<--- 01/06/2023 - Migração S4 - JS


    CALL METHOD me->select_equipments
      EXPORTING
        filial     = filial
        situacao   = situacao
        data       = data
        hora       = hora
*       PARAM      = 'TP_OBJ'
      RECEIVING
        equipments = DATA(_equipments).

    IF NOT ( _equipments IS INITIAL ).

      DATA(_tp_implem)   = me->get_parameters( param_key = 'TP_IMPLEM' is_abastec = abap_false ).

      SELECT objnr, license_num, fuel_pri, fuel_sec, div1, div2, div3, tq_combustivel_1, tq_combustivel_2, tq_combustivel_3
        INTO TABLE @DATA(_tech_equipment_informations)
        FROM fleet FOR ALL ENTRIES IN @_equipments
       WHERE objnr = @_equipments-objnr.

      LOOP AT _equipments INTO DATA(_equipment).
*
        "//Select equipment description
        SELECT SINGLE eqktu
          FROM eqkt
          INTO veiculo-s_descricao
         WHERE equnr = _equipment-equnr.

        veiculo-s_desc_reduzida = veiculo-s_descricao.

        "//Select city
        SELECT SINGLE city1
          INTO veiculo-s_cidade_veiculo
          FROM adrc AS a
          INNER JOIN j_1bbranch AS b ON a~addrnumber = b~adrnr
         WHERE b~bukrs  = _equipment-bukrs
           AND b~branch = _equipment-swerk.

        TRY.
            DATA(_tech_equipment_information) = _tech_equipment_informations[ objnr = _equipment-objnr ].

          CATCH cx_sy_itab_line_not_found.
            CLEAR _tech_equipment_information.
        ENDTRY.

        veiculo-i_capac_tanque_1 = COND #( WHEN _tech_equipment_information-tq_combustivel_1 IS INITIAL THEN '0' ELSE _tech_equipment_information-tq_combustivel_1 ).
        veiculo-i_capac_tanque_2 = COND #( WHEN _tech_equipment_information-tq_combustivel_2 IS INITIAL THEN '0' ELSE _tech_equipment_information-tq_combustivel_2 ).
        veiculo-i_capac_tanque_3 = COND #( WHEN _tech_equipment_information-tq_combustivel_3 IS INITIAL THEN '0' ELSE _tech_equipment_information-tq_combustivel_3 ).

        CONDENSE veiculo-i_capac_tanque_1.
        CONDENSE veiculo-i_capac_tanque_2.
        CONDENSE veiculo-i_capac_tanque_3.

*        VEICULO-S_DIV_1 = COND #( WHEN _TECH_EQUIPMENT_INFORMATION-DIV1 IS INITIAL THEN '0' ELSE _TECH_EQUIPMENT_INFORMATION-DIV1 ).
*        VEICULO-S_DIV_2 = COND #( WHEN _TECH_EQUIPMENT_INFORMATION-DIV2 IS INITIAL THEN '0' ELSE _TECH_EQUIPMENT_INFORMATION-DIV2 ).
*        VEICULO-S_DIV_3 = COND #( WHEN _TECH_EQUIPMENT_INFORMATION-DIV3 IS INITIAL THEN '0' ELSE _TECH_EQUIPMENT_INFORMATION-DIV3 ).

        veiculo-s_div_1 = _tech_equipment_information-div1.
        veiculo-s_div_2 = _tech_equipment_information-div2.
        veiculo-s_div_3 = _tech_equipment_information-div3.

        CONDENSE veiculo-s_div_1.
        CONDENSE veiculo-s_div_2.
        CONDENSE veiculo-s_div_3.

        veiculo-s_placa = me->formatar_placa( _tech_equipment_information-license_num ).

        CLEAR w_equi.
        SELECT SINGLE *
        FROM equi
        INTO w_equi
          WHERE equnr EQ _equipment-equnr.

        IF w_equi-eqtyp NE 'M'.
          veiculo-i_tipo_telemetria    = me->get_atnam( i_equnr = _equipment-equnr
                                                        i_eqart = _equipment-eqart
                                                        range   = _tp_implem
                                                       ). " 01 = nenhum, 02 Hodometro, 03 Horimetro.
          IF veiculo-i_tipo_telemetria IS INITIAL. " Verifica se existe uma das 3 opções acima senão existir não adiciona o equipamento
            CONTINUE.
          ENDIF.

        ELSE.
          SELECT *
          FROM v_equi
          INTO TABLE @DATA(t_equi)
          WHERE equnr EQ @_equipment-equnr
            AND daufn NE ' '.

          IF t_equi IS NOT INITIAL.
            SORT t_equi DESCENDING BY datbi.
            READ TABLE t_equi INTO DATA(_equi) INDEX 1.
            IF _equi-daufn IS NOT INITIAL.
              veiculo-i_tipo_telemetria = '1'.
            ELSE.
              CONTINUE.
            ENDIF.

          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.

        veiculo-i_num_digi_hodometro = 8.
        veiculo-ib_telemetria_ativa  = 0.
*      IT_M_VEIC_SAAF-I_ID_TELEMETRIA      = '0'.
        veiculo-ib_associar_comboio  = 0.
        veiculo-i_imprime_ticket     = 0.
        veiculo-i_tipo_tanque_1      = 0.
        veiculo-i_tipo_tanque_2      = 0.
        veiculo-i_tipo_tanque_3      = 0.
        veiculo-s_barcode            = '0'.
        veiculo-ib_solicita_cotista  = '0'.
        veiculo-i_tipo_autorizacao   = '1'.

        IF w_equi-eqtyp NE 'M'.
          veiculo-ib_controla_km       = '1'.
        ELSE.
          veiculo-ib_controla_km       = '0'.
          veiculo-f_horimetro_inicial  = '0'.
          veiculo-f_km_inicial = '0'.
        ENDIF.

        veiculo-s_grupo_combustivel  = 'COMBUSTIVEIS'.
        veiculo-s_cod_veiculo        = |{ _equipment-equnr ALPHA = OUT }|.
        veiculo-s_modelo_veiculo     = _equipment-typbz.
        veiculo-s_marca_veiculo      = _equipment-herst.
        veiculo-s_pais_veiculo       = _equipment-herld.
        veiculo-ib_imprime_ticket    = '1'.
        veiculo-ib_veiculo_ativo     = SWITCH #( situacao WHEN 'A' THEN me->status_veiculo-active ELSE me->status_veiculo-inactive ).

* ---> S4 Migração - 20/06/2023 - FC - Inicio
        "DATA(_OBJECT) = CONV OBJNUM( _EQUIPMENT-EQUNR ).
        DATA(_object) = CONV ausp-objek( _equipment-equnr ).
* <--- S4 Migração - 20/06/2023 - FC - Fim

        CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
          EXPORTING
            classtype          = '002'
            object             = _object
            key_date           = sy-datum
          TABLES
            t_class            = classif_obj
            t_objectdata       = object_data
          EXCEPTIONS
            no_classification  = 1
            no_classtypes      = 2
            invalid_class_type = 3
            OTHERS             = 4.

        TRY.
            veiculo-s_plano_veiculo = classif_obj[ 1 ]-class.

          CATCH cx_sy_itab_line_not_found.
            CLEAR veiculo-s_plano_veiculo.
        ENDTRY.

        DATA(_measuring_points) = me->get_measuring_points( veiculo-s_cod_veiculo ).

        IF NOT ( _measuring_points IS INITIAL ).

          TRY.
              veiculo-s_cod_comb_principal = |{ _measuring_points[ atnam = 'COMBUSTIVEL' ]-locas ALPHA = OUT }|.
              veiculo-s_cod_comb_principal = |{ veiculo-s_cod_comb_principal ALPHA = OUT }|.
              _matnr = |{ veiculo-s_cod_comb_principal ALPHA = IN }|.

              SELECT SINGLE maktx
                FROM makt
                INTO veiculo-s_desc_comb_principal
               WHERE matnr = _matnr.

            CATCH cx_sy_itab_line_not_found.
              CLEAR veiculo-s_cod_comb_principal.
          ENDTRY.

          TRY.
              point = _measuring_points[ psort = 'HORIMETRO' ].
            CATCH cx_sy_itab_line_not_found.

              TRY.
                  point = _measuring_points[ psort = 'ODOMETRO'  ].
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

          ENDTRY.
*
          IF NOT ( point-point IS INITIAL ).

            CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST_BUF'
              EXPORTING
                point          = point-point
              IMPORTING
                imrg_wa        = doc_medition
              EXCEPTIONS
                imrg_not_found = 4.

            IF ( sy-subrc IS INITIAL ).

              "//Converte para KM;
              CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
                EXPORTING
                  char_unit       = doc_medition-recdu
                  fltp_value_si   = doc_medition-cntrr
                  indicator_value = abap_true
                  masc_symbol     = ' '
                IMPORTING
                  char_value      = km_real_ch
                EXCEPTIONS
                  no_unit_given   = 1
                  unit_not_found  = 2
                  OTHERS          = 3.

              IF ( sy-subrc IS INITIAL ).
                TRANSLATE km_real_ch USING '.,'.

                "//Esta é a quilometragem atual;
                IF ( point-psort = 'HORIMETRO' ).
                  veiculo-f_horimetro_inicial = km_real_ch.
                  CONDENSE veiculo-f_horimetro_inicial.
                ELSE.
                  veiculo-f_km_inicial = km_real_ch.
                  CONDENSE veiculo-f_km_inicial.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND veiculo TO veiculos.
        CLEAR: veiculo, point, doc_medition, _measuring_points, classif_obj.
      ENDLOOP.
    ENDIF.
*
  ENDMETHOD.

  METHOD get_measuring_points.
    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        i_equnr   = equipment
      TABLES
        et_diimpt = points.
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
    DATA(_parameters)   = me->get_parameters( param_key = 'TP_OBJ' is_abastec = abap_true ).
    DATA(_inact_status) = me->get_inactive_status( ).

*DATA(wHERE_A) = ''
    CASE situacao.
      WHEN 'A'.
        IF ( data IS INITIAL ).
          "//Get all active vehicles;
          SELECT DISTINCT b~bukrs b~swerk a~equnr a~herst a~herld a~eqart a~typbz a~objnr
            INTO TABLE equipments
            FROM equi AS a
           INNER JOIN itob AS b ON b~equnr = a~equnr
           WHERE a~eqtyp IN _parameters
*             AND A~EQART IN _PARAMETERS
             AND b~iwerk EQ filial
             AND NOT EXISTS ( SELECT *
                               FROM jest
                               WHERE stat IN _inact_status
                                AND inact EQ abap_false
                                AND objnr EQ a~objnr
                             ).
        ELSE.
          "//Get active vehicles by date period;
          SELECT DISTINCT b~bukrs b~swerk a~equnr a~herst a~herld a~eqart a~typbz a~objnr
            INTO TABLE equipments
            FROM equi AS a
           INNER JOIN itob AS b ON b~equnr = a~equnr
           WHERE a~eqtyp IN _parameters
*             AND A~EQART IN _PARAMETERS
             AND a~erdat IN data
             AND b~iwerk EQ filial
             AND NOT EXISTS ( SELECT *
                                FROM jest
                               WHERE stat IN _inact_status
                                 AND inact = abap_false
                                 AND objnr = a~objnr
                            )
    .

          SELECT DISTINCT b~bukrs b~swerk a~equnr a~herst a~herld a~eqart a~typbz a~objnr
       APPENDING CORRESPONDING FIELDS OF TABLE equipments
            FROM equi AS a
           INNER JOIN itob AS b ON b~equnr = a~equnr
           WHERE a~eqtyp IN _parameters
*             AND A~EQART IN _PARAMETERS
             AND a~aedat IN data
             AND b~iwerk EQ filial
             AND NOT EXISTS ( SELECT *
                                FROM jest
                               WHERE stat IN _inact_status
                                 AND inact = abap_false
                                 AND objnr = a~objnr
                            ).
        ENDIF.

      WHEN 'I'.
        "//Get inactive vehicles;
        SELECT DISTINCT b~bukrs b~swerk a~equnr a~herst a~herld a~eqart a~typbz a~objnr
          APPENDING TABLE equipments
          FROM equi AS a
         INNER JOIN itob AS b ON b~equnr = a~equnr
         INNER JOIN jcds AS c ON c~objnr = b~objnr
         WHERE c~stat  IN _inact_status
           AND c~inact EQ abap_false
           AND c~udate IN data
           AND c~utime IN hora
           AND b~iwerk EQ filial
           AND a~eqtyp IN _parameters."A~EQART IN _PARAMETERS."
    ENDCASE.

    SORT equipments ASCENDING BY equnr.
    DELETE ADJACENT DUPLICATES FROM equipments.
  ENDMETHOD.

  METHOD select_implementos.
    DATA(_parameters)   = me->get_parameters( param_key = 'TP_IMPLEM' is_abastec = abap_false ).
    DATA(_inact_status) = me->get_inactive_status( ).

    CASE situacao.
      WHEN 'A'.
        IF ( data IS INITIAL ).
          "//Get all active vehicles;
          SELECT DISTINCT b~bukrs b~swerk a~equnr a~herst a~herld a~eqart a~typbz a~objnr
            INTO TABLE equipments
            FROM equi AS a
           INNER JOIN itob AS b ON b~equnr = a~equnr
           WHERE a~eqart IN _parameters
             AND b~iwerk EQ filial
             AND NOT EXISTS ( SELECT *
                               FROM jest
                               WHERE stat IN _inact_status
                                AND inact EQ abap_false
                                AND objnr EQ a~objnr
                             ).
        ELSE.
          "//Get active vehicles by date period;
          SELECT DISTINCT b~bukrs b~swerk a~equnr a~herst a~herld a~eqart a~typbz a~objnr
            INTO TABLE equipments
            FROM equi AS a
           INNER JOIN itob AS b ON b~equnr = a~equnr
           WHERE a~eqart IN _parameters
             AND a~erdat IN data
             AND b~iwerk EQ filial
             AND NOT EXISTS ( SELECT *
                                FROM jest
                               WHERE stat IN _inact_status
                                 AND inact = abap_false
                                 AND objnr = a~objnr
                            )
    .

          SELECT DISTINCT b~bukrs b~swerk a~equnr a~herst a~herld a~eqart a~typbz a~objnr
       APPENDING CORRESPONDING FIELDS OF TABLE equipments
            FROM equi AS a
           INNER JOIN itob AS b ON b~equnr = a~equnr
           WHERE a~eqart IN _parameters
             AND a~aedat IN data
             AND b~iwerk EQ filial
             AND NOT EXISTS ( SELECT *
                                FROM jest
                               WHERE stat IN _inact_status
                                 AND inact = abap_false
                                 AND objnr = a~objnr
                            ).
        ENDIF.

      WHEN 'I'.
        "//Get inactive vehicles;
        SELECT DISTINCT b~bukrs b~swerk a~equnr a~herst a~herld a~eqart a~typbz a~objnr
          APPENDING TABLE equipments
          FROM equi AS a
         INNER JOIN itob AS b ON b~equnr = a~equnr
         INNER JOIN jcds AS c ON c~objnr = b~objnr
         WHERE c~stat  IN _inact_status
           AND c~inact EQ abap_false
           AND c~udate IN data
           AND c~utime IN hora
           AND b~iwerk EQ filial
           AND a~eqtyp IN _parameters.
    ENDCASE.

    SORT equipments ASCENDING BY equnr.
    DELETE ADJACENT DUPLICATES FROM equipments.
  ENDMETHOD.

  METHOD get_implementos.

    DATA implemento       TYPE ztpm_m_imple_saaf.

    CALL METHOD me->select_equipments
      EXPORTING
        filial     = filial
        situacao   = situacao
        data       = data
        hora       = hora
*       PARAM      = 'TP_IMPLEM'
*       FLAG       = ABAP_TRUE
      RECEIVING
        equipments = DATA(_equipments).

    IF NOT ( _equipments IS INITIAL ).

      LOOP AT _equipments INTO DATA(_equipment).
*
        "//Select equipment description
        SELECT SINGLE eqktu
          FROM eqkt
          INTO implemento-s_implemento
         WHERE equnr = _equipment-equnr.

        implemento-s_cod_implemento = _equipment-equnr.
        implemento-ib_vincular_comboio = 1.

        APPEND implemento TO implementos.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD get_veiculo_compartimentos.

    CALL METHOD me->select_equipments
      EXPORTING
        filial     = filial
        situacao   = situacao
        data       = data
        hora       = hora
*       PARAM      = 'TP_OBJ'
      RECEIVING
        equipments = DATA(_equipments).

    DATA(_tp_implem)   = me->get_parameters( param_key = 'TP_IMPLEM' is_abastec = abap_false ).

    LOOP AT _equipments INTO DATA(_equipment).


      IF me->get_atnam( i_equnr = _equipment-equnr
                        i_eqart = _equipment-eqart
                        range   = _tp_implem ) IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(_measuring_points) =
        me->get_measuring_points( _equipment-equnr ).

      DELETE _measuring_points WHERE locas = abap_false.
      DELETE _measuring_points WHERE inact = abap_true.

      IF NOT ( _measuring_points IS INITIAL ).

        DATA(_compartimentos) =
        VALUE ty_t_veiculo_comp( FOR ls IN _measuring_points WHERE ( mptyp = 'F' OR mptyp = 'H' ) (
                                  s_cod_veiculo       = |{ _equipment-equnr ALPHA = OUT }|
                                  s_cod_compartimento = ls-locas
                                ) ).

        APPEND LINES OF _compartimentos TO compartimentos.

      ENDIF.
    ENDLOOP.

    SORT compartimentos ASCENDING BY s_cod_veiculo s_cod_compartimento.
  ENDMETHOD.

  METHOD select_employees.
    CASE situacao.
      WHEN 'A'.
        IF ( data IS INITIAL ).
          "//Select all hired employees
          SELECT DISTINCT a~stell a~begda a~pernr a~werks b~cname b~gbdat b~gesch c~cpf_nr c~ident_nr
            FROM pa0001 AS a
      INNER JOIN pa0002 AS b ON b~pernr = a~pernr
      INNER JOIN pa0465 AS c ON c~pernr = a~pernr
            INTO CORRESPONDING FIELDS OF TABLE employees
           WHERE a~gsber EQ filial
             AND a~endda EQ '99991231'
             AND c~subty EQ '0001'.
        ELSE.
          "//Select changed and hired employees by date
          SELECT DISTINCT a~stell a~begda a~pernr a~werks b~cname b~gbdat b~gesch c~cpf_nr c~ident_nr
            FROM pa0001 AS a
      INNER JOIN pa0002 AS b ON b~pernr = a~pernr
      INNER JOIN pa0465 AS c ON c~pernr = a~pernr
            INTO CORRESPONDING FIELDS OF TABLE employees
           WHERE a~gsber EQ filial
             AND a~begda IN data
             AND a~endda EQ '99991231'
             AND c~subty EQ '0001'.

          SELECT DISTINCT a~stell a~begda a~pernr a~werks b~cname b~gbdat b~gesch c~cpf_nr c~ident_nr
            FROM pa0001 AS a
      INNER JOIN pa0002 AS b ON b~pernr = a~pernr
      INNER JOIN pa0465 AS c ON c~pernr = a~pernr
       APPENDING TABLE employees
           WHERE a~gsber EQ filial
             AND a~aedtm IN data
             AND a~endda EQ '99991231'
             AND c~subty EQ '0001'.
        ENDIF.

      WHEN 'I'.
        "//Select all fired employees by date
        SELECT DISTINCT a~stell a~begda a~pernr a~werks b~cname b~gbdat b~gesch c~cpf_nr c~ident_nr
          FROM pa0001 AS a
    INNER JOIN pa0002 AS b ON b~pernr EQ a~pernr
    INNER JOIN pa0465 AS c ON c~pernr EQ a~pernr
    INNER JOIN pa0661 AS d ON d~pernr EQ a~pernr
          INTO CORRESPONDING FIELDS OF TABLE employees
         WHERE a~gsber EQ filial
           AND d~endda IN data
           AND c~subty EQ '0001'.
    ENDCASE.
  ENDMETHOD.

  METHOD get_veiculo_operacoes.

    CALL METHOD me->select_equipments
      EXPORTING
        filial     = filial
        situacao   = situacao
        data       = data
        hora       = hora
*       PARAM      = 'TP_OBJ'
      RECEIVING
        equipments = DATA(_equipments).

    SELECT *
      INTO TABLE @DATA(_vehicles_combinations)
      FROM ztpm_vei_op_saaf
   FOR ALL ENTRIES IN @_equipments
     WHERE eqart = @_equipments-eqart.

    SORT _vehicles_combinations ASCENDING BY eqart.

    DATA(_tp_implem)   = me->get_parameters( param_key = 'TP_IMPLEM' is_abastec = abap_false ).

    LOOP AT _equipments INTO DATA(_equipment).

      IF me->get_atnam( i_equnr = _equipment-equnr
                        i_eqart = _equipment-eqart
                        range   = _tp_implem ) IS INITIAL.
        CONTINUE.
      ENDIF.

      LOOP AT _vehicles_combinations INTO DATA(vehicles_) WHERE eqart = _equipment-eqart.
        TRY.
            APPEND VALUE #(
              s_cod_operacao      = |{ vehicles_-code ALPHA = OUT }|
              ib_vincular_comboio = '1'
              s_cod_veiculo       = |{ _equipment-equnr ALPHA = OUT }| ) TO operacoes.

          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_locais_abastecimento.
    DATA category(1)    VALUE 'C'.
    DATA locais_abastec TYPE TABLE OF ty_local_instalacao.

    DATA(_inact_status) = me->get_inactive_status( ).

    CASE situacao.
      WHEN 'A'.
        IF ( data IS INITIAL ).

          SELECT *
            INTO CORRESPONDING FIELDS OF TABLE locais_abastec
            FROM iflo AS a
           WHERE a~spras EQ sy-langu
             AND a~fltyp EQ category
             AND a~iwerk EQ filial
             AND NOT EXISTS ( SELECT *
                                FROM jest
                               WHERE stat IN _inact_status
                                 AND inact = abap_false
                                 AND objnr = a~objnr
                            ).
        ELSE.

          SELECT *
            INTO CORRESPONDING FIELDS OF TABLE locais_abastec
            FROM iflo AS a
           WHERE a~spras EQ sy-langu
             AND a~fltyp EQ category
             AND a~iwerk EQ filial
             AND a~erdat IN data
             AND NOT EXISTS ( SELECT *
                                FROM jest
                               WHERE stat IN _inact_status
                                 AND inact = abap_false
                                 AND objnr = a~objnr
                            ).

          SELECT *
       APPENDING CORRESPONDING FIELDS OF TABLE locais_abastec
            FROM iflo AS a
           WHERE a~spras EQ sy-langu
             AND a~fltyp EQ category
             AND a~iwerk EQ filial
             AND a~aedat IN data
             AND NOT EXISTS ( SELECT *
                                FROM jest
                               WHERE stat IN _inact_status
                                 AND inact = abap_false
                                 AND objnr = a~objnr
                            ).

        ENDIF.

      WHEN 'I'.

        SELECT *
     APPENDING CORRESPONDING FIELDS OF TABLE locais_abastec
          FROM iflo AS a
    INNER JOIN jcds AS b ON b~objnr = a~objnr
         WHERE a~spras EQ sy-langu
           AND a~fltyp EQ category
           AND a~iwerk EQ filial
           AND b~udate IN data
           AND b~utime IN hora
           AND b~stat  IN _inact_status
           AND b~inact EQ abap_false.

      WHEN OTHERS.
    ENDCASE.



    SORT locais_abastec ASCENDING BY tplnr.
*
*    REFRESH IT_LOC_AB_SAAF.
*
    LOOP AT locais_abastec INTO DATA(_installation_local).
*      CLEAR IT_LOC_AB_SAAF.
*
*      LV_LANGU = SY-LANGU.
*      LV_TPLNR = IT_IFLO-S_COD_LOCAL.

*      CALL FUNCTION 'BAPI_FUNCLOC_GETSTATUS'
*        EXPORTING
*          FUNCTLOCATION = _INSTALLATION_LOCAL-TPLNR
*        TABLES
*          SYSTEM_STATUS = ME->SYSTEM_STATUS
*          USER_STATUS   = ME->USER_STATUS.

*      IF NOT
*         ( LINE_EXISTS( USER_STATUS[ STATUS = 'I0076' ] ) OR LINE_EXISTS( USER_STATUS[ STATUS = 'I0320' ] ) ).

      APPEND VALUE #( s_cod_local         = _installation_local-msgrp
                      s_desc_local        = _installation_local-pltxt
                      ib_vincular_comboio = '1'
                    ) TO locais.
*      ENDIF.
*
*      READ TABLE IT_STATUS WITH KEY STATUS = LC_I0076.  "Inativo
*      IF SY-SUBRC IS INITIAL.
*        CONTINUE.
*      ENDIF.
**
*      READ TABLE IT_STATUS WITH KEY STATUS = LC_I0320.  "Inativo
*      IF SY-SUBRC IS INITIAL.
*        CONTINUE.
*      ENDIF.
*

*      APPEND IT_LOC_AB_SAAF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_employees.
    DATA employee TYPE ztpm_m_user_saaf.
    DATA: lt_nota_fiscal TYPE TABLE OF ty_nf.
    DATA  lw_ztpm_m_terc_saaf TYPE ztpm_m_terc_saaf.

*    Verificar se existe fornecedor por entrada de nota de combustivel para cadastrar na tabela de fornecedor.
    CASE situacao.
      WHEN 'A'.
        DATA(_range) =
            VALUE rsis_t_range(
                                ( low  = '101' sign = 'I' option = 'EQ'  )
                                ( low  = '123' sign = 'I' option = 'EQ'  )
                               ).

      WHEN 'I'.
        _range =
        VALUE rsis_t_range(
                             ( low  = '102' sign = 'I' option = 'EQ'  )
                             ( low  = '122' sign = 'I' option = 'EQ'  )
                           ).
    ENDCASE.

* Tabela de Materias para ser Enviado para o SAAF
    SELECT *
      FROM zpmt004
      INTO TABLE @DATA(it_matnr).

    CHECK it_matnr IS NOT INITIAL.

    DELETE it_matnr WHERE matnr = '' OR
                          matnr = '0'.

    DATA(_matnr) =
            VALUE rsis_t_range(
                                FOR ls1 IN it_matnr
                                  ( low  = ls1-matnr sign = 'I' option = 'EQ' )
                              ).

    SELECT a~ebeln a~xblnr b~lifnr a~werks a~budat a~bldat a~cputm a~matnr a~menge c~lgort d~station
      INTO TABLE lt_nota_fiscal
      FROM ekbe AS a
      INNER JOIN ekko AS b ON b~ebeln EQ a~ebeln
      INNER JOIN ekpo AS c ON c~ebeln EQ a~ebeln
      INNER JOIN t370fld_stn AS d ON d~plant EQ a~werks AND d~storage EQ c~lgort
        WHERE b~bstyp EQ 'F'
        AND a~ebeln EQ c~ebeln
        AND a~bewtp EQ 'E'
        AND a~werks EQ filial
        AND a~budat IN data
        AND a~matnr IN _matnr
        AND a~bwart IN _range.

    SORT  lt_nota_fiscal ASCENDING BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_nota_fiscal COMPARING lifnr.

    CLEAR lw_ztpm_m_terc_saaf.
    LOOP AT lt_nota_fiscal INTO DATA(ls_nota).

      SELECT SINGLE *
      FROM ztpm_m_terc_saaf
      INTO @DATA(_fornecedor)
        WHERE lifnr EQ @ls_nota-lifnr.

      IF _fornecedor IS INITIAL.
        SELECT SINGLE *
        FROM lfa1
        INTO @DATA(_lfa1)
          WHERE lifnr EQ @ls_nota-lifnr.

        MOVE-CORRESPONDING _lfa1 TO lw_ztpm_m_terc_saaf.
        MODIFY ztpm_m_terc_saaf FROM lw_ztpm_m_terc_saaf.
        COMMIT WORK.
      ENDIF.
    ENDLOOP.

*Veficando cadastro de usuario.
    CALL METHOD me->select_employees
      EXPORTING
        situacao  = situacao
        filial    = filial
        data      = data
      RECEIVING
        employees = DATA(_employees_data).

    LOOP AT _employees_data INTO DATA(_employee_data).
      employee-dt_data_nascimento = _employee_data-gbdat.
      employee-s_rg               = _employee_data-ident_nr.
      employee-s_cpf              = _employee_data-cpf_nr.
      employee-s_id_teclado       = _employee_data-pernr.
      employee-s_nome             = _employee_data-cname.
      employee-s_sexo             = SWITCH #( _employee_data-gesch WHEN 1 THEN 'M' WHEN 2 THEN 'F' ).
      employee-s_lotacao          = zcl_hrst_commons=>get_descr_area( _employee_data-werks ).
      employee-ib_ativo           = 1.
      employee-i_usuario_tipo     = 0.


      CALL METHOD zcl_hrst_commons=>get_descr_cargo
        EXPORTING
          iv_stell = _employee_data-stell
          iv_date  = _employee_data-begda
          iv_sprsl = sy-langu
        RECEIVING
          rv_stltx = employee-s_perfil.

      APPEND employee TO employees.
      CLEAR employee.
    ENDLOOP.


    SELECT *
      FROM ztpm_m_terc_saaf
      INTO TABLE @DATA(lt_fornecedor).

    CHECK lt_fornecedor IS NOT INITIAL.

    LOOP AT lt_fornecedor INTO DATA(ls).
      employee-s_id_teclado    = ls-lifnr.
      employee-s_nome          = ls-name1.
      employee-i_usuario_tipo  = 1.
      employee-ib_ativo        = 1.

      APPEND employee TO employees.
      CLEAR employee.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_compartimentos.
    DATA material_kind(4) VALUE 'IBAU'.
    DATA materiais   TYPE TABLE OF ty_compartimento.

    CASE situacao.
      WHEN 'A'.
        IF data IS INITIAL.
          SELECT *
            INTO CORRESPONDING FIELDS OF TABLE materiais
            FROM mara AS a
      INNER JOIN makt AS b ON b~matnr EQ a~matnr
           WHERE a~mtart = material_kind
             AND a~lvorm = abap_false.
        ELSE.
          SELECT *
            INTO CORRESPONDING FIELDS OF TABLE materiais
            FROM mara AS a
      INNER JOIN makt AS b ON b~matnr EQ a~matnr
           WHERE a~mtart EQ material_kind
             AND a~lvorm EQ abap_false
             AND a~laeda IN data.

          SELECT *
       APPENDING CORRESPONDING FIELDS OF TABLE materiais
            FROM mara AS a
      INNER JOIN makt AS b ON b~matnr EQ a~matnr
           WHERE a~mtart EQ material_kind
             AND a~lvorm EQ abap_false
             AND a~ersda IN data.
        ENDIF.

      WHEN 'I'.
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE materiais
          FROM mara AS a
    INNER JOIN makt AS b ON b~matnr EQ a~matnr
         WHERE a~mtart EQ material_kind
           AND a~lvorm EQ abap_true
           AND a~laeda IN data.

      WHEN OTHERS.
    ENDCASE.

    SORT materiais ASCENDING BY matnr.
*
*    REFRESH IT_B_COMP_SAAF.

    DELETE materiais WHERE matnr(1) NE 'F'.

    LOOP AT materiais INTO DATA(_material).
*      CLEAR IT_B_COMP_SAAF.

      APPEND VALUE #( s_cod_compartimento = _material-matnr
                      s_desc_red          = _material-maktx
                      s_desc              = _material-maktg
                    ) TO compartimentos.

*      IT_B_COMP_SAAF-.
*      IT_B_COMP_SAAF-.
*      IT_B_COMP_SAAF-.
*      APPEND IT_B_COMP_SAAF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_centros_custos.

    DATA(_parameters) =
      me->get_parameters( param_key  = 'TP_OBJ' is_abastec = abap_true ).

*    SELECT ZVAL CONST
*      FROM ZTPARAM
*      INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
*     WHERE PARAM EQ LC_TP_OBJ
*       AND ABASTEC EQ LC_X.
*
*    LOOP AT IT_PARAM.
*      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = IT_PARAM-ZVAL ) TO EQUIPAMENTOS_ABAST.
**    IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
**    APPEND IT_ZVAL.
*    ENDLOOP.
*
*  SELECT EQUNR HERST HERLD TYPBZ OBJNR INTO TABLE IT_EQUI
*    FROM ITOB FOR ALL ENTRIES IN IT_ZVAL
*    WHERE EQTYP   = IT_ZVAL-ZVAL.
*  SORT IT_EQUI ASCENDING BY S_COD_VEICULO.

*    REFRESH IT_B_CENT_SAAF.

    DATA cost_area(4) VALUE 'MAGI'.
    DATA r_values     TYPE rsis_t_range.

    CASE situacao.
      WHEN 'A'.
        IF ( data IS INITIAL ).
          "//Select active cost centers;
          SELECT a~kostl b~ltext b~ktext
            INTO TABLE centros
            FROM csks AS a
      INNER JOIN cskt AS b ON b~kokrs EQ a~kokrs
                          AND b~kostl EQ a~kostl
                          AND b~datbi EQ a~datbi
           WHERE a~kokrs EQ cost_area
             AND a~datbi GT sy-datum
             AND a~gsber EQ filial
             AND a~spras EQ sy-langu.
        ELSE.
          "//Get created cost centers by date;
          SELECT a~kostl b~ltext b~ktext
            INTO TABLE centros
            FROM csks AS a
      INNER JOIN cskt AS b ON b~kokrs EQ a~kokrs AND b~kostl EQ a~kostl AND b~datbi EQ a~datbi
           WHERE a~kokrs EQ cost_area
             AND a~ersda IN data
             AND b~datbi GT sy-datum
             AND a~gsber EQ filial
             AND a~spras EQ sy-langu.

          "//Get changed cost centers by date;
          SELECT *
            FROM cdhdr
            INTO TABLE @DATA(_changed_centers)
           WHERE objectclas EQ 'KOSTL'
             AND tcode      EQ 'KS02'
             AND udate      IN @data
             AND langu      EQ @sy-langu
             AND change_ind EQ 'U'.

          LOOP AT _changed_centers INTO DATA(_changed_center).
            REPLACE cost_area IN _changed_center-objectid WITH space.

            APPEND VALUE #( sign   = 'I'
                            option = 'EQ'
                            low    = _changed_center-objectid
                          ) TO r_values.
          ENDLOOP.

          SELECT a~kostl b~ltext b~ktext
       APPENDING TABLE centros
            FROM csks AS a
      INNER JOIN cskt AS b ON b~kokrs EQ a~kokrs AND b~kostl EQ a~kostl AND b~datbi EQ a~datbi
           WHERE a~kokrs EQ cost_area
             AND a~kostl IN r_values
             AND b~datbi GT sy-datum
             AND a~gsber EQ filial
             AND a~spras EQ sy-langu.
        ENDIF.

      WHEN 'I'.
        "//Select deleted cost centers by date;
        SELECT *
          FROM cdhdr
          INTO TABLE @DATA(_deleted_centers)
         WHERE objectclas EQ 'KOSTL'
           AND tcode      EQ 'KS04'
           AND udate      IN @data
           AND langu      EQ @sy-langu
           AND change_ind EQ 'U'.

        LOOP AT _deleted_centers INTO DATA(_deleted_center).
          REPLACE cost_area IN _deleted_center-objectid WITH space.
          APPEND VALUE #( s_cod_centro_custo = _deleted_center-objectid ) TO centros.
        ENDLOOP.


    ENDCASE.


  ENDMETHOD.

  METHOD get_produtos.
    DATA produto LIKE LINE OF produtos.
    DATA: vg_matnr TYPE char18.

    SELECT *
      INTO TABLE @DATA(_materiais_aux)
      FROM ztftpm_lubri.

    SORT _materiais_aux ASCENDING BY code_mat.

    LOOP AT _materiais_aux INTO DATA(_material_aux).

      SELECT SINGLE *
        FROM makt
        INTO @DATA(_material)
       WHERE matnr = @_material_aux-conjunto.

     CLEAR: vg_matnr.
     _material-matnr = |{ _material-matnr ALPHA = OUT }|.
     vg_matnr = |{ _material-matnr ALPHA = IN }|.
     _material-matnr = vg_matnr.

      SELECT SINGLE a~wgbez
        FROM t023t AS a
  INNER JOIN mara AS b ON b~matkl = a~matkl
        INTO produto-s_grupo_produto
       WHERE b~matnr = _material-matnr.

      produto-i_tipo               = 0.
      produto-ib_filtro            = SWITCH #( _material_aux-categoria WHEN 'F' THEN 1 ELSE 0 ).
      produto-ib_lubrificante      = SWITCH #( _material_aux-categoria WHEN 'C' THEN 1 ELSE 0 ).
      produto-s_cod_produto        = |{ _material_aux-conjunto ALPHA = OUT }|.
      produto-s_descricao          = _material-maktx.
      produto-s_descricao_reduzida = _material-maktx.
      APPEND produto TO produtos.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_causa_manutencoes.
    DATA catalogos TYPE TABLE OF ty_catalog.
    DATA catalog(10) VALUE 'R'.

    IF data IS INITIAL.
      SELECT a~code b~kurztext a~codegruppe
        INTO TABLE catalogos
        FROM qpcd AS a
  INNER JOIN qpct AS b ON b~katalogart = a~katalogart
                      AND b~codegruppe = a~codegruppe
                      AND b~code       = a~code
       WHERE a~katalogart EQ catalog
         AND a~codegruppe IN ('F-MOTIVO','H-MOTIVO')
         AND a~inaktiv    EQ abap_false.

    ELSE.

      SELECT a~code b~kurztext a~codegruppe
        INTO TABLE catalogos
        FROM qpcd AS a
  INNER JOIN qpct AS b ON b~katalogart = a~katalogart
                      AND b~codegruppe = a~codegruppe
                      AND b~code       = a~code
       WHERE a~katalogart EQ catalog
         AND a~codegruppe IN ('F-MOTIVO','H-MOTIVO')
         AND a~inaktiv    EQ abap_false
         AND a~e_datum    IN data.

      SELECT a~code b~kurztext a~codegruppe
   APPENDING TABLE catalogos
        FROM qpcd AS a
  INNER JOIN qpct AS b ON b~katalogart = a~katalogart
                      AND b~codegruppe = a~codegruppe
                      AND b~code       = a~code
       WHERE a~katalogart EQ catalog
         AND a~codegruppe IN ('F-MOTIVO','H-MOTIVO')
         AND a~inaktiv    EQ abap_false
         AND a~a_datum    IN data.
    ENDIF.

    SORT catalogos ASCENDING BY codigo.
    DELETE ADJACENT DUPLICATES FROM catalogos.

    LOOP AT catalogos INTO DATA(_catalog).
      APPEND VALUE #( s_cod_causa_manut   = _catalog-codigo
                      s_desc              = _catalog-texto
                      ib_manut_filtro     = SWITCH #( _catalog-grupo(1) WHEN 'H' THEN '0' ELSE '1' )
                      ib_vincular_comboio = '1'
                    ) TO causas.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_entrada_nota.

    DATA: _nota_fiscal TYPE TABLE OF ty_nf.
    DATA  lw_ztpm_m_terc_saaf TYPE ztpm_m_terc_saaf.

    CASE situacao.
      WHEN 'A'.
        DATA(_range) =
        VALUE rsis_t_range(
                            ( low  = '101' sign = 'I' option = 'EQ'  )
                            ( low  = '123' sign = 'I' option = 'EQ'  )
                           ).

      WHEN 'I'.
        _range =
        VALUE rsis_t_range(
                             ( low  = '102' sign = 'I' option = 'EQ'  )
                             ( low  = '122' sign = 'I' option = 'EQ'  )
                           ).
    ENDCASE.

* Tabela de Materias para ser Enviado para o SAAF
    SELECT *
      FROM zpmt004
      INTO TABLE @DATA(it_matnr).

    CHECK it_matnr IS NOT INITIAL.

    DELETE it_matnr WHERE matnr = '' OR
                          matnr = '0'.

    DATA(_matnr) =
            VALUE rsis_t_range(
                                FOR ls1 IN it_matnr
                                  ( low  = ls1-matnr sign = 'I' option = 'EQ' )
                              ).

    SELECT a~ebeln a~xblnr b~lifnr a~werks a~budat a~bldat a~cputm a~matnr a~menge c~lgort d~station
      INTO TABLE _nota_fiscal
      FROM ekbe AS a
      INNER JOIN ekko AS b ON b~ebeln EQ a~ebeln
      INNER JOIN ekpo AS c ON c~ebeln EQ a~ebeln
      INNER JOIN t370fld_stn AS d ON d~plant EQ a~werks AND d~storage EQ c~lgort
        WHERE b~bstyp EQ 'F'
        AND a~ebeln EQ c~ebeln
        AND a~bewtp EQ 'E'
        AND a~werks EQ filial
        AND a~budat IN data
        AND a~matnr IN _matnr
        AND a~bwart IN _range.



*    SELECT * "SINGLE STATION
*    FROM T370FLD_STN
*    INTO TABLE @DATA(T_DEPOSITO)
*      FOR ALL ENTRIES IN @_NOTA_FISCAL
*    WHERE STORAGE EQ @_NOTA_FISCAL-LGORT
*    AND PLANT EQ @_NOTA_FISCAL-WERKS.

    IF _nota_fiscal IS NOT INITIAL.
      LOOP AT _nota_fiscal ASSIGNING FIELD-SYMBOL(<_deposito>).
        IF <_deposito>-matnr IS NOT INITIAL.
          SELECT SINGLE *
          FROM zpmt006
          INTO @DATA(_zpmt006)
            WHERE matnr EQ @<_deposito>-matnr
              AND iwerk EQ @<_deposito>-werks.
          <_deposito>-id_tq = |{ _zpmt006-id_tq ALPHA = OUT }|.

        ENDIF.
      ENDLOOP.
    ENDIF.


    notas = VALUE #(
            FOR ls IN _nota_fiscal
                                  (
                                    s_numero_nota      = ls-xblnr
                                    s_cod_fornecedor   = |{ ls-lifnr ALPHA = OUT }|
                                    s_cod_planta       = ls-werks
                                    dt_emissao_nota    = |{ ls-bldat+6(2) }/{ ls-bldat+4(2) }/{ ls-bldat(4) } { ls-cputm(2) }:{ ls-cputm+2(2) }:{ ls-cputm+4(2) }|
                                    dt_entrada         = |{ ls-bldat+6(2) }/{ ls-bldat+4(2) }/{ ls-bldat(4) } 00:01:00|
                                    s_cfop             = ''
                                    s_desc_cfop        = ''
                                    f_base_calc_icms   = 0
                                    f_valor_icms       = 0
                                    f_base_icms_subst  = 0
                                    f_valor_icms_subst = 0
                                    f_valor_frete      = 0
                                    f_valor_seguro     = 0
                                    f_outras_desp      = 0
                                    f_valor_ipi        = 0
                                    f_valor_desconto   = 0
                                    f_total_nota       = 0
                                    i_usuario_tipo     = 1
                                    items              = VALUE #(
                                                                   s_cod_prod_combust = |{ ls-matnr ALPHA = OUT }|
                                                                   f_quantidade       = ls-menge
                                                                   f_valor_unitario   = 0
                                                                   f_valor_total      = 0
                                                                   f_aliquota_icms    = 0
                                                                   tanquesdestino     = VALUE #(
                                                                                                  s_cod_tanque_destino    = ls-id_tq"LS-LGORT
                                                                                                  f_quantidade_tanque     = ls-menge
                                                                                               )
                                                                )

                                  )
                   ).


  ENDMETHOD.

  METHOD get_planos_operacoes.
    DATA catalogos TYPE TABLE OF ty_catalog.

    DATA catalog(1)  VALUE 'S'.
    DATA code_grp(8) VALUE 'FPN-0010'.

    IF ( data IS INITIAL ).
      SELECT a~code b~kurztext a~codegruppe
        INTO TABLE catalogos
        FROM qpcd AS a
  INNER JOIN qpct AS b ON b~katalogart = a~katalogart
                      AND b~codegruppe = a~codegruppe
                      AND b~code       = a~code
       WHERE a~katalogart EQ catalog
         AND a~codegruppe EQ code_grp
         AND a~inaktiv    EQ abap_false.

    ELSE.
      SELECT a~code b~kurztext a~codegruppe
        INTO TABLE catalogos
        FROM qpcd AS a
  INNER JOIN qpct AS b ON b~katalogart = a~katalogart
                      AND b~codegruppe = a~codegruppe
                      AND b~code       = a~code
       WHERE a~katalogart EQ catalog
         AND a~codegruppe EQ code_grp
         AND a~inaktiv    EQ abap_false
         AND a~e_datum    IN data.

      SELECT a~code b~kurztext a~codegruppe
   APPENDING TABLE catalogos
        FROM qpcd AS a
  INNER JOIN qpct AS b ON b~katalogart = a~katalogart
                      AND b~codegruppe = a~codegruppe
                      AND b~code       = a~code
       WHERE a~katalogart EQ catalog
         AND a~codegruppe EQ code_grp
         AND a~inaktiv    EQ abap_false
         AND a~a_datum    IN data.
    ENDIF.

*    SORT CATALOGOS BY CODIGO.
*    DELETE ADJACENT DUPLICATES FROM CATALOGOS.

    LOOP AT catalogos INTO DATA(_catalog).
      APPEND VALUE #( s_cod_operacao  = |{ _catalog-codigo ALPHA = OUT }|
                      s_desc_operacao = _catalog-texto
                      s_desc_reduzida = _catalog-texto
                      ib_vincular_comboio = '1'
                    ) TO planos.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_inactive_status.
    table = VALUE #( ( low = 'I0076' option = 'EQ' sign = 'I' )
                     ( low = 'I0320' option = 'EQ' sign = 'I' )
                     ( low = 'I0100' option = 'EQ' sign = 'I' )

                   ).
  ENDMETHOD.

  METHOD formatar_placa.
    placa = i_placa.

    REPLACE ALL OCCURRENCES OF '-' IN placa WITH space.
    REPLACE ALL OCCURRENCES OF '.' IN placa WITH space.
    REPLACE ALL OCCURRENCES OF '/' IN placa WITH space.
    REPLACE ALL OCCURRENCES OF ',' IN placa WITH space.

    CHECK NOT placa IS INITIAL.
    placa = placa(3) && '-' && placa+3.
  ENDMETHOD.

  METHOD get_atnam.

    DATA it_dimpt TYPE TABLE OF diimpt.
    DATA r_atnam  TYPE RANGE OF atnam.

    r_atnam = VALUE #(
                       ( sign = 'I' option = 'EQ' low = 'HORIMETRO' )
                       ( sign = 'I' option = 'EQ' low = 'ODOMETRO' )
                     ).

    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        i_equnr   = i_equnr
      TABLES
        et_diimpt = it_dimpt.

    IF i_eqart IN range.
      DELETE it_dimpt WHERE atnam NOT IN r_atnam.
    ELSE.
      DELETE it_dimpt WHERE atnam NOT IN r_atnam OR indtr IS NOT INITIAL.
    ENDIF.

    IF it_dimpt IS NOT INITIAL.
      return = SWITCH #( it_dimpt[ 1 ]-atnam
                         WHEN 'ODOMETRO' THEN '2'
                         WHEN 'HORIMETRO' THEN '3'
                         ELSE '1' ).
    ENDIF.
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
*
*  METHOD INSERIR_MENSAGEM.
*
*    IF NOT NUMBER EQ 999.
*
*      MESSAGE ID 'ZPPM001'
*              TYPE 'E'
*              NUMBER NUMBER
*              WITH   SPACE
*                     SPACE
*                     SPACE
*                     SPACE
*              INTO DATA(MESSAGE_).
*    ENDIF.
*
*    APPEND VALUE #(
*                     TYPE       =  'E'
*                     ID         =  'ZPPM001'
*                     NUMBER     =  NUMBER
*                     MESSAGE_V1 =  SPACE
*                     MESSAGE_V2 =  SPACE
*                     MESSAGE_V3 =  SPACE
*                     MESSAGE_V4 =  SPACE
*                     FIELD      =  FIELD
*                  ) TO RETURN.
*
*  ENDMETHOD.
*
*  METHOD CONVERT_DATE_TO_INT.
*
*    TRY .
*        CL_ABAP_DATFM=>CONV_DATE_EXT_TO_INT(
*                                            EXPORTING IM_DATEXT = DATA
*                                            IMPORTING EX_DATINT = RETURN
*                                           ).
*      CATCH CX_ABAP_DATFM_NO_DATE.
*      CATCH CX_ABAP_DATFM_INVALID_DATE.
*      CATCH CX_ABAP_DATFM_FORMAT_UNKNOWN.
*      CATCH CX_ABAP_DATFM_AMBIGUOUS.
*    ENDTRY.
*
*  ENDMETHOD.
*
*  METHOD TRANSFERE_TANQUE.
*
*    DATA: _SWERK    TYPE SWERK,
*          _T001K    TYPE  T001K,
*          _DT_PONTO TYPE SY-DATUM.
*
*    DATA:
*      IT_ITEM   TYPE STANDARD TABLE OF BAPI2017_GM_ITEM_CREATE,
*      IT_RETURN TYPE STANDARD TABLE OF BAPIRET2,
*      _HEADER   TYPE BAPI2017_GM_HEAD_01,
*      _CODE     TYPE BAPI2017_GM_CODE,
*      _LOG      TYPE ZLOGAPONT,
*      _YEAR     TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
*      _MATER    TYPE BAPI2017_GM_HEAD_RET-MAT_DOC.
*
*    DATA: _DATA TYPE SY-DATUM,
*          _HORA TYPE SY-UZEIT.
*
** verificar se período está aberto
*    _SWERK = INPUT-S_COD_PLANTA_CENTRO_VEIC.
*
*    CALL FUNCTION 'AIP01_PLANT_DETERMINE'
*      EXPORTING
*        I_WERKS  = _SWERK
*      IMPORTING
*        ES_T001K = _T001K
*      EXCEPTIONS
*        OTHERS   = 1.
*
*    _DT_PONTO          = INPUT-DT_INICIO_MOV.
*    _HEADER-PSTNG_DATE = _DT_PONTO.
*    _HEADER-DOC_DATE   = _DT_PONTO.
*
*    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
*      EXPORTING
*        P_DATA_ENT     = _DT_PONTO
*        P_BUKRS        = _T001K-BUKRS
*        P_VAL_FI       = ABAP_TRUE
*        P_VAL_MM       = ABAP_TRUE
*      EXCEPTIONS
*        DATA_FI_MM_NAO = 1
*        OTHERS         = 2.
*
*    IF SY-SUBRC IS NOT INITIAL.
*
*      APPEND LINES OF ME->INSERIR_MENSAGEM(
*                                             NUMBER  = 999
*                                             MESSAGE = |A data informada esta em um período bloqueado para a empresa { _T001K-BUKRS }.|
*                                             FIELD   = CONV #( INPUT-I_COD_TRANSA_SAAF )
*                                          ) TO ME->_RETURN.
*
*      ME->_ERRO = ABAP_TRUE.
*    ENDIF.
*
**   verificar qual é o material
*    APPEND VALUE #(
*                      MATERIAL     = |{ COND #( WHEN INPUT-S_COD_COMBUST IS NOT INITIAL
*                                                THEN INPUT-S_COD_COMBUST
*                                                ELSE INPUT-S_COD_PRODUTO )
*                                      ALPHA = IN }|
*
*                      PLANT        = INPUT-S_COD_PLANTA_CENTRO_VEIC
*                      MOVE_PLANT   = INPUT-S_COD_PLANTA_CENTRO_VEIC
*                      MOVE_TYPE    = '311'
*                      ENTRY_QNT    = COND #( WHEN INPUT-F_QTDE_ABAST IS NOT INITIAL
*                                             THEN INPUT-F_QTDE_ABAST
*                                             ELSE INPUT-F_QTDE_PRODUTO )
*
*                      STGE_LOC     = INPUT-S_ID_TANQUE_ORIGEM
*                      MOVE_STLOC   = INPUT-S_ID_TANQUE_DESTINO
*                      ACTIVITY     = ME->GET_ACTIVITY( INPUT-IB_ORDEM )
*                   ) TO IT_ITEM.
*
*    _CODE = '06'.
*    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*      EXPORTING
*        GOODSMVT_HEADER  = _HEADER
*        GOODSMVT_CODE    = _CODE
*      IMPORTING
*        MATERIALDOCUMENT = _MATER
*        MATDOCUMENTYEAR  = _YEAR
*      TABLES
*        GOODSMVT_ITEM    = IT_ITEM
*        RETURN           = IT_RETURN.
*
*    CLEAR: _HEADER, _CODE, IT_ITEM[].
*
*    IF SY-SUBRC IS INITIAL AND IT_RETURN IS INITIAL.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          WAIT = ABAP_TRUE.
*
*      APPEND VALUE #( MESSAGE = |{ TEXT-053 } { _MATER }{ TEXT-042 }| ) TO IT_RETMED.
*
*      _HORA = SY-UZEIT.
*      _DATA = SY-DATUM.
*    ELSE.
*
*      ME->_ERRO = ABAP_TRUE.
*
*      LOOP AT IT_RETURN INTO DATA(LS_RETURN).
*        APPEND LINES OF ME->INSERIR_MENSAGEM(
*                                               NUMBER     = 999
*                                               MESSAGE    = LS_RETURN-MESSAGE
*                                               FIELD   = CONV #( INPUT-I_COD_TRANSA_SAAF )
*                                            ) TO ME->_RETURN.
*      ENDLOOP.
*    ENDIF.
*
*    LOOP AT IT_RETMED INTO DATA(_RETMED).
*      _LOG = VALUE #(
*                        ID         = ME->GERA_ID( )
*                        HORA       = _HORA
*                        DATA       = _DATA
*                        USUARIO    = SY-UNAME
*                        MENSAGEM   = _RETMED-MESSAGE
*       ).
*      INSERT INTO ZLOGAPONT VALUES _LOG.
*    ENDLOOP.
*
*  ENDMETHOD.
*
*  METHOD GET_ACTIVITY.
*
*    SELECT SINGLE VORNR
*      FROM AFVC AS A
*     INNER JOIN AFKO AS B ON A~AUFPL = B~AUFPL
*      INTO RETURN
*     WHERE B~AUFNR = INPUT.
*
*  ENDMETHOD.
*
*  METHOD GERA_ID.
*    SELECT COUNT(*) INTO RETURN FROM ZLOGAPONT.
*    ADD 1 TO RETURN.
*  ENDMETHOD.

ENDCLASS.
