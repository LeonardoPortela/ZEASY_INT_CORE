FUNCTION zpm_export_data_to_saaf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(VIEW) TYPE  STRING
*"     REFERENCE(STATUS) TYPE  CHAR1
*"     REFERENCE(FILIAL) TYPE  WERKS_D
*"     REFERENCE(DATA_DE) TYPE  CHAR16
*"     REFERENCE(DATA_ATE) TYPE  CHAR16
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  STRING
*"----------------------------------------------------------------------
  DATA(_main_app)      = NEW cl_main_app( ).

  DATA(_view) = |{ view CASE = LOWER }|.

  IF cl_main_app=>check_sets_authorization( filial ) IS NOT INITIAL.
    result = zcl_fmcall_handler=>abap2xml( abap_data  = |Acesso não Autorizado para a filial { filial }!| name = _view ).
    EXIT.
  ENDIF.


  DATA(_date_interval) =
      cl_main_app=>get_date_range(
                                   from = cl_main_app=>convert_date( data_de )
                                   to   = cl_main_app=>convert_date( data_ate )
                                 ).

  DATA(_time_interval) =
      cl_main_app=>get_time_range(
                                   from = cl_main_app=>convert_time( data_de )
                                   to   = cl_main_app=>convert_time( data_ate )
                                 ).


  CASE _view.
    WHEN 'vw_m_veiculo_imp_terceiro'.
      "//ZPM_EXPORT_M_VEIC_TO_SAAF

*      CLEAR: _date_interval, _time_interval.

      CALL METHOD _main_app->get_veiculos
        EXPORTING
          filial   = filial
          situacao = status
          data     = _date_interval
          hora     = _time_interval
        RECEIVING
          veiculos = DATA(_veiculos).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _veiculos name = _view ).

    WHEN 'vw_r_veiculo_compartimento'.
      "//ZPM_EXPORT_C_VEIC_TO_SAAF

      CALL METHOD _main_app->get_veiculo_compartimentos
        EXPORTING
          filial         = filial
          situacao       = status
          data           = _date_interval
          hora           = _time_interval
        RECEIVING
          compartimentos = DATA(_veiculo_compartimentos).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _veiculo_compartimentos name = _view ).

    WHEN 'vw_r_veiculo_plano_operacao'.
      "//ZPM_EXPORT_R_VE_OP_TO_SAAF

      CALL METHOD _main_app->get_veiculo_operacoes
        EXPORTING
          filial    = filial
          situacao  = status
          data      = _date_interval
          hora      = _time_interval
        RECEIVING
          operacoes = DATA(_operacoes).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _operacoes name = _view ).

    WHEN 'vw_m_usuario_imp_terceiro'.
      "//ZPM_EXPORT_M_USER_TO_SAAF

      CALL METHOD _main_app->get_employees
        EXPORTING
          situacao  = status
          filial    = filial
          data      = _date_interval
        RECEIVING
          employees = DATA(_employees).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _employees name = _view ).

    WHEN 'vw_m_prod_serv_imp_terceiro'.
      "//ZPM_EXPORT_M_PROD_TO_SAAF

      CALL METHOD _main_app->get_produtos
        RECEIVING
          produtos = DATA(_produtos).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _produtos name = _view ).

    WHEN 'vw_b_plano_operacao'.
      "//ZPM_EXPORT_B_PLAN_TO_SAAF

      CALL METHOD _main_app->get_planos_operacoes
        EXPORTING
          situacao = status
          filial   = filial
          data     = _date_interval
        RECEIVING
          planos   = DATA(_planos).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _planos name = _view ).

    WHEN 'vw_b_local_abastecimento'.
      "//ZPM_EXPORT_LOC_AB_TO_SAAF

      CALL METHOD _main_app->get_locais_abastecimento
        EXPORTING
          situacao = status
          filial   = filial
          data     = _date_interval
          hora     = _time_interval
        RECEIVING
          locais   = DATA(_locais).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _locais name = _view ).

    WHEN 'vw_b_compartimentos'.
      "//ZPM_EXPORT_B_COMP_TO_SAAF

      CALL METHOD _main_app->get_compartimentos
        EXPORTING
          situacao       = status
          filial         = filial
          data           = _date_interval
        RECEIVING
          compartimentos = DATA(_compartimentos).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _compartimentos name = _view ).

    WHEN 'vw_b_centro_custo'.
      "//ZPM_EXPORT_B_CENT_TO_SAAF

      CALL METHOD _main_app->get_centros_custos
        EXPORTING
          situacao = status
          filial   = filial
          data     = _date_interval
        RECEIVING
          centros  = DATA(_centros_custos).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _centros_custos name = _view ).

    WHEN 'vw_b_causa_manutencao'.
      "//ZPM_EXPORT_B_CAUS_TO_SAAF

      CALL METHOD _main_app->get_causa_manutencoes
        EXPORTING
          situacao = status
          filial   = filial
          data     = _date_interval
        RECEIVING
          causas   = DATA(_causas).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _causas name = _view ).

    WHEN 'vw_m_implementos'.

      CALL METHOD _main_app->get_implementos
        EXPORTING
          filial      = filial
          situacao    = status
          data        = _date_interval
          hora        = _time_interval
        RECEIVING
          implementos = DATA(_implementos).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _implementos name = _view ).

    WHEN 'vw_m_entrada_nf'.

      IF  _date_interval  IS INITIAL.
        _date_interval =
         cl_main_app=>get_date_range(
                                      from = cl_main_app=>convert_date( '2018.11.08' )
                                      to   = cl_main_app=>convert_date( '9999.12.31' )
                                    ).
      ENDIF.

      CALL METHOD _main_app->get_entrada_nota
        EXPORTING
          situacao = status
          filial   = filial
          data     = _date_interval
        RECEIVING
          notas    = DATA(_nota_fiscal).

      result = zcl_fmcall_handler=>abap2xml( abap_data  = _nota_fiscal name = _view ).

  ENDCASE.
ENDFUNCTION.
