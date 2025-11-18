FUNCTION zhcm_gera_pdf_folha_pagamento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TOKEN) TYPE  STRING
*"     REFERENCE(I_PERNR) TYPE  P_PERNR OPTIONAL
*"     REFERENCE(I_CPF) TYPE  CHAR14 OPTIONAL
*"     REFERENCE(I_ANOPR) TYPE  ZED_ANOPR
*"     REFERENCE(I_MESPR) TYPE  ZED_MESPR
*"     VALUE(I_NORMAL) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_DECIMO) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(I_PPR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_ALL) TYPE  CHAR01 DEFAULT 'X'
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

  DATA: lc_pernr TYPE pernr-pernr,
        it_pernr TYPE TABLE OF zhcms_ret_pernr,
        it_saida TYPE TABLE OF zhcms_func_list,
        lc_cpf   TYPE string,
        it_pa    TYPE TABLE OF zhcms_func_list_pa.

  DATA: lc_endda LIKE p0001-endda.

  DATA: handle TYPE REF TO zcl_memory_variaveis_area,
        root   TYPE REF TO zcl_memory_variaveis,
        oref   TYPE REF TO zcl_memory_variaveis.

  DATA: e_text       TYPE string,
        e_text_x     TYPE xstring,
        e_otf        TYPE tt_itcoo,
        e_otf01      TYPE tt_itcoo,
        e_otf02      TYPE tt_itcoo,
        e_otf03      TYPE tt_itcoo,
        pdf_tab      TYPE TABLE OF tline,
        bin_filesize TYPE i.

  DATA: lva_ts2(25)    TYPE c,
        lva_ts         TYPE timestampl,
        lva_dat1       TYPE date,
        lva_tim        TYPE time,
        lva_tz         TYPE timezone,
        lva_t1(10)     TYPE c,
        lva_t2(13)     TYPE c,
        lva_time_stamp TYPE char25.


  "ZHCMF_RET_LABORE_HOLERITE

  IF i_pernr IS INITIAL AND i_cpf IS NOT INITIAL.
    CALL FUNCTION 'ZHCMF_RET_PERNR'
      EXPORTING
        cpf_nr  = i_cpf
      TABLES
        t_saida = it_pernr.

    IF it_pernr[] IS INITIAL.
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
        IF wa_pa-situacao EQ 'ATIVO'.
          lc_pernr = wa_pernr-pernr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lc_pernr IS INITIAL.
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

    lc_cpf = i_cpf.

  ELSEIF i_pernr IS NOT INITIAL .

    lc_pernr = i_pernr.

    CALL FUNCTION 'ZHCMF_RETURN_FUNC'
      EXPORTING
        pernr   = lc_pernr
      TABLES
        t_saida = it_saida.

    READ TABLE it_saida INTO DATA(wa_saida) INDEX 1.
    lc_cpf = wa_saida-cpf_nr.

  ELSE.
    RAISE EXCEPTION TYPE zcx_hcm
      EXPORTING
        textid = VALUE #( msgid  = zcx_hcm=>zcx_informar_pernr_cpf-msgid
                          msgno  = zcx_hcm=>zcx_informar_pernr_cpf-msgno )
        msgid  = zcx_hcm=>zcx_informar_pernr_cpf-msgid
        msgno  = zcx_hcm=>zcx_informar_pernr_cpf-msgno
        msgty  = 'E'.
  ENDIF.

  zcl_sol_mobile_rh=>zif_sol_mobile_rh~get_verifica_token( i_cpf = lc_cpf i_token = i_token ).


  DATA(lva_instance_ts) = abap_false.
  CLEAR: lva_time_stamp.

  SELECT SINGLE *
    FROM setleaf INTO @DATA(lwa_confi_demo_folha)
   WHERE setname = 'ZHCM_GERA_PDF_FOLHA_PAG'
     AND valfrom = 'ALL_PERNR'.

  IF sy-subrc EQ 0.
    lva_instance_ts = abap_true.
    i_normal = abap_true.
    CLEAR: i_all.
  ELSE.
    SELECT SINGLE *
      FROM setleaf INTO lwa_confi_demo_folha
     WHERE setname = 'ZHCM_GERA_PDF_FOLHA_PAG'
       AND valfrom = lc_pernr.

    IF sy-subrc EQ 0.
      lva_instance_ts = abap_true.
      i_normal = abap_true.
      CLEAR: i_all.
    ENDIF.
  ENDIF.

  IF lva_instance_ts EQ abap_true.
    GET TIME STAMP FIELD lva_ts.

    MOVE lva_ts TO lva_ts2.

    CONDENSE lva_ts2.

    CONVERT TIME STAMP lva_ts TIME ZONE sy-zonlo INTO DATE lva_dat1 TIME lva_tim.
    CONCATENATE lva_dat1+0(4) lva_dat1+4(2) lva_dat1+6(2) INTO lva_t1.
    CONCATENATE lva_tim+0(2) lva_tim+2(2) lva_tim+4(2) lva_ts2+15(3) INTO lva_t2.
    CONCATENATE lva_t1 lva_t2 INTO lva_time_stamp.

    CONCATENATE 'PDF_Folha' lc_pernr i_anopr i_mespr lva_time_stamp INTO DATA(nm_instance).
  ELSE.
    CONCATENATE 'PDF_Folha' lc_pernr i_anopr i_mespr INTO nm_instance.
  ENDIF.



  CASE i_all.
    WHEN abap_false.

      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        TRY.
            handle = zcl_memory_variaveis_area=>attach_for_write( inst_name = CONV #( nm_instance ) ).
            CREATE OBJECT root AREA HANDLE handle.
            handle->set_root( root ).
            CREATE OBJECT root AREA HANDLE handle TYPE zcl_memory_variaveis.
            handle->set_root( root ).
            handle->detach_commit( ).
            EXIT.
          CATCH cx_shm_attach_error INTO DATA(lva_attach_error).
        ENDTRY.
      ENDDO.



      SUBMIT zhcmr_py0038 WITH ppernr   EQ lc_pernr
                          WITH panopr   EQ i_anopr
                          WITH pmespr   EQ i_mespr
                          WITH ptfnor   EQ i_normal
                          WITH ptf13    EQ i_decimo
                          WITH ptfprd   EQ i_ppr
                          WITH ptimestp EQ lva_time_stamp
                          WITH pxstring EQ abap_true  AND RETURN.

      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        TRY.
            handle = zcl_memory_variaveis_area=>attach_for_read( inst_name = CONV #( nm_instance ) ).
            oref ?= handle->root.
            oref->get_texto_string(  IMPORTING e_string  = e_text
               )->get_texto_xstring( IMPORTING e_xstring = e_text_x
               )->get_texto_otf( IMPORTING e_otf = e_otf
               )->get_id_autenticacao( IMPORTING e_cd_autenticacao = DATA(e_cd_autenticacao)
               ).
            CLEAR oref.
            handle->detach( ).
            handle->free_area( ).
            EXIT.
          CATCH cx_shm_attach_error INTO lva_attach_error.
        ENDTRY.
      ENDDO.

    WHEN abap_true.

      "Normal
      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        TRY.
            handle = zcl_memory_variaveis_area=>attach_for_write( inst_name = CONV #( nm_instance ) ).
            CREATE OBJECT root AREA HANDLE handle.
            handle->set_root( root ).
            CREATE OBJECT root AREA HANDLE handle TYPE zcl_memory_variaveis.
            handle->set_root( root ).
            handle->detach_commit( ).
            EXIT.
          CATCH cx_shm_attach_error INTO lva_attach_error.
        ENDTRY.
      ENDDO.

      SUBMIT zhcmr_py0038 WITH ppernr   EQ lc_pernr
                          WITH panopr   EQ i_anopr
                          WITH pmespr   EQ i_mespr
                          WITH ptfnor   EQ abap_true
                          WITH ptf13    EQ abap_false
                          WITH ptfprd   EQ abap_false
                          WITH ptimestp EQ lva_time_stamp
                          WITH pxstring EQ abap_true
                          WITH pnoauten EQ abap_false AND RETURN.

      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        TRY.
            handle = zcl_memory_variaveis_area=>attach_for_read( inst_name = CONV #( nm_instance ) ).
            oref ?= handle->root.
            oref->get_texto_string(  IMPORTING e_string  = e_text
               )->get_texto_xstring( IMPORTING e_xstring = e_text_x
               )->get_texto_otf( IMPORTING e_otf = e_otf01
               )->get_autenticacao( IMPORTING e_ds_autenticacao = DATA(e_ds_autenticacao)
               )->get_id_autenticacao( IMPORTING e_cd_autenticacao = e_cd_autenticacao
               ).
            CLEAR oref.
            handle->detach( ).
            handle->free_area( ).
            EXIT.
          CATCH cx_shm_attach_error INTO lva_attach_error.
        ENDTRY.
      ENDDO.



      "Décimo
      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        TRY.
            handle = zcl_memory_variaveis_area=>attach_for_write( inst_name = CONV #( nm_instance ) ).
            CREATE OBJECT root AREA HANDLE handle.
            handle->set_root( root ).
            CREATE OBJECT root AREA HANDLE handle TYPE zcl_memory_variaveis.
            handle->set_root( root ).
            handle->detach_commit( ).
            EXIT.
          CATCH cx_shm_attach_error INTO lva_attach_error.
        ENDTRY.
      ENDDO.

      DATA(pnoauten) = COND string( WHEN e_ds_autenticacao IS INITIAL THEN abap_false ELSE abap_true ).

      SUBMIT zhcmr_py0038 WITH ppernr   EQ lc_pernr
                          WITH panopr   EQ i_anopr
                          WITH pmespr   EQ i_mespr
                          WITH ptfnor   EQ abap_false
                          WITH ptf13    EQ abap_true
                          WITH ptfprd   EQ abap_false
                          WITH ptimestp EQ lva_time_stamp
                          WITH pxstring EQ abap_true
                          WITH pnoauten EQ pnoauten
                          WITH pautenti EQ e_ds_autenticacao
                          WITH pcodauti EQ e_cd_autenticacao AND RETURN.

      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        TRY.
            handle = zcl_memory_variaveis_area=>attach_for_read( inst_name = CONV #( nm_instance ) ).
            oref ?= handle->root.
            oref->get_texto_string(  IMPORTING e_string  = e_text
               )->get_texto_xstring( IMPORTING e_xstring = e_text_x
               )->get_texto_otf( IMPORTING e_otf = e_otf02
               ).
            CLEAR oref.
            handle->detach( ).
            handle->free_area( ).
            EXIT.
          CATCH cx_shm_attach_error INTO lva_attach_error.
        ENDTRY.
      ENDDO.


      "PPR
      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        TRY.
            handle = zcl_memory_variaveis_area=>attach_for_write( inst_name = CONV #( nm_instance ) ).
            CREATE OBJECT root AREA HANDLE handle.
            handle->set_root( root ).
            CREATE OBJECT root AREA HANDLE handle TYPE zcl_memory_variaveis.
            handle->set_root( root ).
            handle->detach_commit( ).
            EXIT.
          CATCH cx_shm_attach_error INTO lva_attach_error.
        ENDTRY.
      ENDDO.

      pnoauten = COND string( WHEN e_ds_autenticacao IS INITIAL THEN abap_false ELSE abap_true ).

      SUBMIT zhcmr_py0038 WITH ppernr   EQ lc_pernr
                          WITH panopr   EQ i_anopr
                          WITH pmespr   EQ i_mespr
                          WITH ptfnor   EQ abap_false
                          WITH ptf13    EQ abap_false
                          WITH ptfprd   EQ abap_true
                          WITH ptimestp EQ lva_time_stamp
                          WITH pxstring EQ abap_true
                          WITH pnoauten EQ pnoauten
                          WITH pautenti EQ e_ds_autenticacao
                          WITH pcodauti EQ e_cd_autenticacao AND RETURN.

      DO 5 TIMES.
        WAIT UP TO 2 SECONDS.
        TRY.
            handle = zcl_memory_variaveis_area=>attach_for_read( inst_name = CONV #( nm_instance ) ).
            oref ?= handle->root.
            oref->get_texto_string(  IMPORTING e_string  = e_text
               )->get_texto_xstring( IMPORTING e_xstring = e_text_x
               )->get_texto_otf( IMPORTING e_otf = e_otf03
               ).
            CLEAR oref.
            handle->detach( ).
            handle->free_area( ).
            EXIT.
          CATCH cx_shm_attach_error INTO lva_attach_error.
        ENDTRY.
      ENDDO.



      SELECT SINGLE *
        FROM setleaf INTO lwa_confi_demo_folha
       WHERE setname = 'ZHCM_GERA_PDF_FOLHA_PAG'
         AND valfrom = 'FREE_INSTANCE'.

      IF sy-subrc EQ 0.
        handle->free_instance( ).
      ENDIF.

      CALL FUNCTION 'ZSMARTFORMS_MERGE_OTF'
        EXPORTING
          otf_01     = e_otf01
          otf_02     = e_otf02
        IMPORTING
          otf_result = e_otf.

      CALL FUNCTION 'ZSMARTFORMS_MERGE_OTF'
        EXPORTING
          otf_01     = e_otf
          otf_02     = e_otf03
        IMPORTING
          otf_result = e_otf.

      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
          max_linewidth         = 132
        IMPORTING
          bin_filesize          = bin_filesize
          bin_file              = e_text_x
        TABLES
          otf                   = e_otf[]
          lines                 = pdf_tab
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          err_bad_otf           = 4
          OTHERS                = 5.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(mtext).
        zcl_sol_mobile_rh=>zif_sol_mobile_rh~gera_erro_geral( i_texto = mtext ).
      ENDIF.

  ENDCASE.

  IF e_text_x IS NOT INITIAL.

    CALL FUNCTION 'ZSMARTFORMS_SET_XSTRING'
      EXPORTING
        i_cd_autenticacao = e_cd_autenticacao
        i_xstring         = e_text_x.

    e_pdf = e_text_x.
  ELSE.
    RAISE EXCEPTION TYPE zcx_hcm
      EXPORTING
        textid = VALUE #( msgid  = zcx_hcm=>zcx_sem_folha_pagamento-msgid
                          msgno  = zcx_hcm=>zcx_sem_folha_pagamento-msgno )
        msgid  = zcx_hcm=>zcx_sem_folha_pagamento-msgid
        msgno  = zcx_hcm=>zcx_sem_folha_pagamento-msgno
        msgty  = 'E'.
  ENDIF.

ENDFUNCTION.
