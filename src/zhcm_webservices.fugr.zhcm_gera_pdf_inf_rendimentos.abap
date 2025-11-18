FUNCTION zhcm_gera_pdf_inf_rendimentos .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TOKEN) TYPE  STRING
*"     REFERENCE(I_PERNR) TYPE  P_PERNR OPTIONAL
*"     REFERENCE(I_CPF) TYPE  CHAR14 OPTIONAL
*"     REFERENCE(I_ANOPR) TYPE  PBR_ANO
*"  EXPORTING
*"     REFERENCE(E_PDF) TYPE  XSTRING
*"  RAISING
*"      ZCX_HCM
*"      CX_SHM_EXCLUSIVE_LOCK_ACTIVE
*"      CX_SHM_VERSION_LIMIT_EXCEEDED
*"      CX_SHM_CHANGE_LOCK_ACTIVE
*"      CX_SHM_PARAMETER_ERROR
*"      CX_SHM_PENDING_LOCK_REMOVED
*"      CX_SHM_ATTACH_ERROR
*"      ZCX_SOL_MOBILE_RH
*"----------------------------------------------------------------------

  DATA: "PNPPERNR TYPE RANGE OF PERNR-PERNR,
    "LC_PERNR  TYPE PERNR-PERNR,
    it_saida  TYPE TABLE OF zhcms_func_list,
    "LC_CPF   TYPE STRING,
    lc_cpf_nr TYPE  char14,
    it_pa     TYPE TABLE OF zhcms_func_list_pa,
    it_bukrs  TYPE TABLE OF t001.

  DATA: lc_endda LIKE p0001-endda.

  DATA: handle   TYPE REF TO zcl_memory_variaveis_area,
        root     TYPE REF TO zcl_memory_variaveis,
        oref     TYPE REF TO zcl_memory_variaveis,
        it_pernr TYPE TABLE OF zhcms_ret_pernr.

  DATA: e_text      TYPE  string,
        e_text_x    TYPE  xstring,
        e_otf_geral TYPE  tt_itcoo,
        e_otf       TYPE  tt_itcoo.

  IF i_pernr IS INITIAL AND i_cpf IS NOT INITIAL.

    "Retorna Todos os PERNR do CPF """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'ZHCMF_RET_PERNR'
      EXPORTING
        cpf_nr  = i_cpf
      TABLES
        t_saida = it_pernr.

    IF it_pernr[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_hcm
        EXPORTING
          textid = VALUE #( msgid = zcx_hcm=>zcx_cpf_nao_encontrado-msgid msgno = zcx_hcm=>zcx_cpf_nao_encontrado-msgno attr1 = CONV #( i_cpf ) )
          msgid  = zcx_hcm=>zcx_cpf_nao_encontrado-msgid
          msgno  = zcx_hcm=>zcx_cpf_nao_encontrado-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_cpf ).
    ENDIF.

    lc_cpf_nr = i_cpf.

  ELSEIF i_pernr IS NOT INITIAL .

    "Retorna Informações do PERNR """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "LC_PERNR = I_PERNR.

    CALL FUNCTION 'ZHCMF_RETURN_FUNC'
      EXPORTING
        pernr   = i_pernr
      TABLES
        t_saida = it_saida.

    IF it_saida[] IS INITIAL.
      RAISE EXCEPTION TYPE zcx_hcm
        EXPORTING
          textid = VALUE #( msgid = zcx_hcm=>zcx_cpf_nao_encontrado-msgid msgno = zcx_hcm=>zcx_cpf_nao_encontrado-msgno attr1 = CONV #( i_pernr ) )
          msgid  = zcx_hcm=>zcx_cpf_nao_encontrado-msgid
          msgno  = zcx_hcm=>zcx_cpf_nao_encontrado-msgno
          msgty  = 'E'
          msgv1  = CONV #( i_pernr ).
    ENDIF.

    "Retorna Todos os PERNR do CPF """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    READ TABLE it_saida INDEX 1 INTO DATA(wa_saida).
    lc_cpf_nr = wa_saida-cpf_nr.

    CALL FUNCTION 'ZHCMF_RET_PERNR'
      EXPORTING
        cpf_nr  = lc_cpf_nr
      TABLES
        t_saida = it_pernr.

  ELSE.
    RAISE EXCEPTION TYPE zcx_hcm
      EXPORTING
        textid = VALUE #( msgid = zcx_hcm=>zcx_informar_pernr_cpf-msgid msgno = zcx_hcm=>zcx_informar_pernr_cpf-msgno )
        msgid  = zcx_hcm=>zcx_informar_pernr_cpf-msgid
        msgno  = zcx_hcm=>zcx_informar_pernr_cpf-msgno
        msgty  = 'E'.
  ENDIF.

  SELECT *
    FROM setleaf
    INTO TABLE @DATA(it_block)
    WHERE setname = 'BUKRS_NAOGERA_IRRF'.

  DATA(it_pernr_aux) = it_pernr[].

  lc_endda = sy-datum.

  LOOP AT it_pernr INTO DATA(wa_pernr).

    CLEAR: it_pa.

    CALL FUNCTION 'ZHCMF_DADOS_FUNCIONAIS_PA'
      EXPORTING
        pernr   = wa_pernr-pernr
        endda   = lc_endda
      TABLES
        t_saida = it_pa.

    READ TABLE it_pa INDEX 1 INTO DATA(wa_pa).
    IF sy-subrc IS INITIAL.
      TRANSLATE wa_pa-situacao TO UPPER CASE.
      CONDENSE wa_pa-situacao NO-GAPS.
      IF wa_pa-situacao NE 'ATIVO'.
        DELETE it_pernr_aux WHERE pernr EQ wa_pernr-pernr.
      ENDIF.
    ELSE.
      DELETE it_pernr_aux WHERE pernr EQ wa_pernr-pernr.
    ENDIF.

  ENDLOOP.

  IF it_pernr_aux[] IS INITIAL.
    RAISE EXCEPTION TYPE zcx_hcm
      EXPORTING
        textid = VALUE #( msgid  = zcx_hcm=>zcx_cpf_nao_encontrado-msgid
                          msgno  = zcx_hcm=>zcx_cpf_nao_encontrado-msgno
                          attr1  = CONV #( i_cpf ) )
        msgid  = zcx_hcm=>zcx_cpf_nao_encontrado-msgid
        msgno  = zcx_hcm=>zcx_cpf_nao_encontrado-msgno
        msgty  = 'E'
        msgv1  = CONV #( i_cpf ).
  ENDIF.

  zcl_sol_mobile_rh=>zif_sol_mobile_rh~get_verifica_token( i_cpf = CONV #( lc_cpf_nr ) i_token = i_token ).

  CLEAR: e_otf_geral[].

  LOOP AT it_pernr_aux INTO wa_pernr.

    CLEAR: it_saida[].

    CALL FUNCTION 'ZHCMF_RETURN_FUNC'
      EXPORTING
        pernr   = wa_pernr-pernr
      TABLES
        t_saida = it_saida.

    IF it_saida[] IS INITIAL.
      CONTINUE.
    ENDIF.

    DATA(lc_inicio) = i_anopr && '0101'.
    DATA(lc_final)  = i_anopr && '1231'.

    SELECT * INTO TABLE @DATA(t_pa01)
      FROM pa0001
     WHERE pernr EQ @wa_pernr-pernr.

    LOOP AT t_pa01 INTO DATA(wa_pa01).

      IF NOT ( i_anopr BETWEEN wa_pa01-begda(4) AND wa_pa01-endda(4) ).
        CONTINUE.
      ENDIF.

      "//   Bloqueia a Empresa que estiver no SET para não gerar o Rendimentos.
      READ TABLE it_block WITH KEY valfrom = wa_pa01-bukrs TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE it_bukrs WITH KEY bukrs = wa_pa01-bukrs TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.
      APPEND VALUE #( bukrs = wa_pa01-bukrs ) TO it_bukrs.

      CLEAR: e_otf[].

      READ TABLE it_saida INTO wa_saida INDEX 1.

      SELECT SINGLE * INTO @DATA(wa_zsped005)
        FROM zsped005
       WHERE bukrs     EQ @wa_pa01-bukrs
         AND cod_assin EQ '900'.

      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CONCATENATE 'PDF_Informe_Rendimentos' wa_pernr-pernr INTO DATA(nm_instance).

      handle = zcl_memory_variaveis_area=>attach_for_write( inst_name = CONV #( nm_instance ) ).
      CREATE OBJECT root AREA HANDLE handle.
      handle->set_root( root ).
      CREATE OBJECT root AREA HANDLE handle TYPE zcl_memory_variaveis.
      handle->set_root( root ).
      handle->detach_commit( ).

      DATA: lc_ano TYPE pbr_ano.

      lc_ano = i_anopr.
      ADD 1 TO lc_ano.

      SUBMIT hbrcced0
        WITH pnptimr6     EQ abap_true
        WITH pnppernr-low EQ wa_pernr-pernr
        WITH pnpbukrs-low EQ wa_pa01-bukrs
        WITH pnpbegda     EQ lc_inicio
        WITH pnpendda     EQ lc_final
        WITH respcc       EQ wa_zsped005-ident_nom
        WITH ano          EQ lc_ano
        WITH titulo1      EQ 'ABONO PECUNIARIO'
        WITH complem1     EQ '*ATENÇÃO* PARA INFORMAÇÃO DO BRADESCO VIDA E PREVIDENCIA SA'
        WITH complem2     EQ 'INFORME O CNPJ 51.990.695-0001/37 VALORES QUADRO 3 LINHA 3'
        WITH datacc       EQ '20220228'
         AND RETURN.

      handle = zcl_memory_variaveis_area=>attach_for_read( inst_name = CONV #( nm_instance ) ).
      oref ?= handle->root.
      oref->get_texto_string(  IMPORTING e_string  = e_text
         )->get_texto_xstring( IMPORTING e_xstring = e_text_x
         )->get_texto_otf( IMPORTING e_otf = e_otf
         ).
      CLEAR oref.
      handle->detach( ).
      handle->free_area( ).

      CALL FUNCTION 'ZSMARTFORMS_MERGE_OTF'
        EXPORTING
          otf_01     = e_otf_geral
          otf_02     = e_otf
        IMPORTING
          otf_result = e_otf_geral.

    ENDLOOP.
  ENDLOOP.

  IF e_otf_geral[] IS NOT INITIAL.

    CALL FUNCTION 'ZSMARTFORMS_OTF_XSTRING'
      EXPORTING
        otf             = e_otf_geral
      IMPORTING
        ls_pdf_string_x = e_text_x.

  ENDIF.

  IF e_text_x IS NOT INITIAL.
    e_pdf = e_text_x.
  ELSE.
    RAISE EXCEPTION TYPE zcx_hcm
      EXPORTING
        textid = VALUE #( msgid  = zcx_hcm=>zcx_sem_inf_rendimento-msgid
                          msgno  = zcx_hcm=>zcx_sem_inf_rendimento-msgno
                          attr1  = i_anopr )
        msgid  = zcx_hcm=>zcx_sem_inf_rendimento-msgid
        msgno  = zcx_hcm=>zcx_sem_inf_rendimento-msgno
        msgty  = 'E'
        msgv1  = CONV #( i_anopr ).
  ENDIF.

ENDFUNCTION.
