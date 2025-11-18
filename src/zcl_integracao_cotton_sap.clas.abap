class ZCL_INTEGRACAO_COTTON_SAP definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COTTON_SAP .

  methods CONSTRUCTOR .
  class-methods ESTORNO_04
    importing
      !ID_COTTON type XBLNR .
  class-methods GET_QTDE_PROGRAM_EXEC
    importing
      !I_TIPO type CHAR1 optional
      !I_SHOW_MSG type CHAR1
    returning
      value(E_QUANTIDADE) type I .
  class-methods LOCK_OBJETO
    importing
      !I_OBJETO type CHAR50
    returning
      value(E_ERRO) type CHAR1 .
  class-methods UNLOCK_OBJETO
    importing
      !I_OBJETO type CHAR50 .
  class-methods AGUARDAR_JOB
    importing
      !I_TIPO type CHAR1 optional .
  class-methods VERIFICA_BLOQUEAR_JOB
    returning
      value(E_BLOQUEADO) type CHAR1 .
  class-methods ARMAZENAR_FILA_PROCESSAMENTO
    importing
      !I_ZPPT0002 type ZPPT0002 .
  class-methods CHECK_REGISTRO_PROCESSAMENTO
    importing
      !I_ZPPT0002 type ZPPT0002
    returning
      value(R_ZPPT0002_PROCESSING) type ZPPT0002 .
protected section.
private section.

  methods GET_DOC_GERADO_04
    importing
      !ACHARG type CHARG_D
      !ID_COTTON type XBLNR
      !STATUS_ENTRADA type CHAR03 optional
      !STATUS_ESTORNO type CHAR03 optional
    returning
      value(DOC_EXISTE) type CHAR1 .
  methods JOB_ENVIO
    importing
      !ID_REFERENCIA type CHAR11 optional
      !S_COTTON type ZRSDSSELOPTS optional
      !ALGODOEIRA type CHAR2 optional
      !ESTORNO type CHAR1 optional
      !ID_COTTON type XBLNR optional .
  methods GET_DOC_GERADO_02
    importing
      !ACHARG type CHARG_D
      !ID_COTTON type XBLNR
      !STATUS_ENTRADA type CHAR03
      !STATUS_ESTORNO type CHAR03
    returning
      value(DOC_EXISTE) type CHAR1 .
  methods ADD_ZPPT0007
    importing
      !I_0007 type ZPPT0007 .
  methods JOB_ENVIO_ESTORNO
    importing
      !ID_REFERENCIA type CHAR11 optional
      !ESTORNO type CHAR1 optional
      !IT_ZPPT0002 type ZPPT0002_T optional .
  methods FILL_DADOS_HEADER_ZPPT0002
    importing
      !I_DADOS_GERAIS type ZPPS0002
    changing
      !C_ZPPT0002 type ZPPT0002 .
  methods SET_ID_MOVIMENTACAO_INTERNO
    importing
      !I_ID_MOV_SISTEMA_ORIGEM type ZDE_ID_MOV_SISTEMA_ORIGEM
    changing
      !C_ZPPT0002 type ZPPT0002
    returning
      value(R_ID_GERADO) type CHAR01 .
  methods CHECK_MOV_AND_FORCE_RETORNO
    importing
      !I_ZPPT0002 type ZPPT0002
    exporting
      !E_ERROR type CHAR01
      !E_MOV_MARCADA_RETORNO type CHAR01 .
ENDCLASS.



CLASS ZCL_INTEGRACAO_COTTON_SAP IMPLEMENTATION.


  METHOD add_zppt0007.

    DATA: w_0007 TYPE zppt0007,
          seq    TYPE numc11.

    MOVE-CORRESPONDING i_0007 TO w_0007.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQ0007'
      IMPORTING
        number                  = seq
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    w_0007-seqnum   = seq.
    w_0007-data_reg = sy-datum.
    w_0007-hora_reg = sy-uzeit.

    MODIFY zppt0007 FROM w_0007.
    COMMIT WORK.

    CLEAR w_0007.

  ENDMETHOD.


  METHOD aguardar_job.

    DATA: lc_lim_jobs_exec TYPE i,
          lc_qtde_times    TYPE i,
          lc_show_msg      TYPE c,
          e_quantidade     TYPE i.

    lc_lim_jobs_exec = 100.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(wl_set_job_est_gr_lim_exec)
     WHERE setname = 'JOB_TRACE_COTTON_LIM'.

    IF sy-subrc EQ 0.
      IF wl_set_job_est_gr_lim_exec-valfrom > 0.
        lc_lim_jobs_exec = wl_set_job_est_gr_lim_exec-valfrom.
      ENDIF.
    ENDIF.

*-------------------------------
*---- checar quantidade de jobs em execucao
*-------------------------------
    e_quantidade = zcl_integracao_cotton_sap=>get_qtde_program_exec( i_tipo     = i_tipo
                                                                     i_show_msg = abap_true ).

    lc_qtde_times = 0.
    WHILE e_quantidade >= lc_lim_jobs_exec.
      WAIT UP TO 1 SECONDS.
      lc_show_msg = abap_false.
      IF lc_qtde_times = 10.
        lc_qtde_times = 0.
        lc_show_msg   = abap_true.
      ENDIF.
      e_quantidade = zcl_integracao_cotton_sap=>get_qtde_program_exec( i_tipo     = i_tipo
                                                                       i_show_msg = lc_show_msg ).
      ADD 1 TO lc_qtde_times.
    ENDWHILE.

  ENDMETHOD.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface  = zif_integracao=>at_id_interface_cotton.
    me->zif_integracao_inject~at_tp_integracao = zif_integracao=>at_tp_integracao_inbound.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'TraceCotton X SAP'.
    me->zif_integracao_inject~at_tp_canal = zif_integracao=>at_tp_canal_comunica_http.

    FREE: zif_integracao_cotton_sap~at_zppt0002,
          zif_integracao_cotton_sap~at_retorno,
          zif_integracao_cotton_sap~at_recebido.

  ENDMETHOD.


  METHOD estorno_04.

    TYPES: BEGIN OF ty_mseg,
             charg TYPE mseg-charg,
             mblnr TYPE mseg-mblnr,
             smbln TYPE mseg-smbln,
             werks TYPE mseg-werks,
           END   OF ty_mseg.

    DATA: it_mseg           TYPE TABLE OF ty_mseg,
          wa_return         TYPE bapiret2,
          v_confirmation_es TYPE bapi_rm_datkey-cancconfirmation.

    CHECK id_cotton IS NOT INITIAL.

    SELECT *
      FROM zppt0002
      INTO TABLE @DATA(it_0002)
      WHERE id_cotton EQ @id_cotton.

    CHECK it_0002 IS NOT INITIAL.

    LOOP AT it_0002 INTO DATA(wa_0002).

      SELECT *
        FROM mseg
        INTO CORRESPONDING FIELDS OF TABLE it_mseg
        WHERE charg EQ wa_0002-acharg
          AND werks EQ wa_0002-werks
          AND bwart IN ('131', '132').

      LOOP AT it_mseg ASSIGNING FIELD-SYMBOL(<f_mseg>).
        READ TABLE it_mseg INTO DATA(wa_mseg) WITH KEY charg = <f_mseg>-charg smbln = <f_mseg>-mblnr.
        IF sy-subrc IS INITIAL.
          <f_mseg>-smbln = wa_mseg-mblnr.
        ENDIF.
      ENDLOOP.

      DELETE it_mseg WHERE smbln IS NOT INITIAL.

      CHECK it_mseg IS NOT INITIAL.
      CHECK lines( it_mseg ) NE 1.
      SORT it_mseg BY mblnr DESCENDING.

      DELETE it_mseg INDEX 1.

      SELECT *
        FROM blpp
        INTO TABLE @DATA(it_blpp)
        FOR ALL ENTRIES IN @it_mseg
        WHERE belnr EQ @it_mseg-mblnr
          AND prtps EQ '0001'.

      LOOP AT it_blpp INTO DATA(wa_blpp).

        CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
          EXPORTING
            confirmation     = wa_blpp-prtnr
          IMPORTING
            cancconfirmation = v_confirmation_es
            return           = wa_return.

        IF wa_return-type EQ 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_doc_gerado_02.

    DATA check TYPE char1.

*   Pega os Logs de Processamento para verificar se já esta com DOC Gerado.
    SELECT *
      FROM zppt0006
      INTO TABLE @DATA(it_0006)
    WHERE id_cotton  EQ @id_cotton
      AND status_msg EQ 'S'
      ORDER BY data, hora.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE status_registro
      FROM zppt0006
      INTO @DATA(vl_status_registro)
    WHERE id_cotton EQ @id_cotton
      AND status_msg EQ 'S'
      AND id IN ( SELECT MAX( id )
                   FROM zppt0006
                  WHERE id_cotton    EQ @id_cotton
                    AND status_msg EQ 'S' ).

    CHECK vl_status_registro EQ status_entrada.

*   Marca como W todos os status 04/02 ate o estorno 05/06
*   Caso encontre um 05 todos os 04/02 W são transformados em Y
*   e no final deletamos todos os Y
    LOOP AT it_0006 ASSIGNING FIELD-SYMBOL(<f_006>).

      IF <f_006>-status_registro EQ status_entrada.
        <f_006>-flag_envio = 'W'.
      ENDIF.

      IF <f_006>-status_registro EQ status_estorno.
        <f_006>-flag_envio = 'Y'.
        MODIFY it_0006 FROM <f_006> TRANSPORTING flag_envio WHERE flag_envio EQ 'W'.
      ENDIF.

    ENDLOOP.

    DELETE it_0006 WHERE flag_envio EQ 'Y'.
    DELETE it_0006 WHERE status_registro NE '02'.

    SORT it_0006 BY acharg.
    DELETE ADJACENT DUPLICATES FROM it_0006 COMPARING acharg.

    CHECK lines( it_0006 ) EQ 1.

    LOOP AT it_0006 INTO DATA(wa_006) WHERE status_registro = status_entrada.

      UPDATE zppt0006 SET flag_envio = 'R'
      WHERE charg  EQ wa_006-charg
        AND acharg EQ wa_006-acharg
        AND werks  EQ wa_006-werks
        AND id     EQ wa_006-id
        AND id_cotton EQ wa_006-id_cotton.

      doc_existe = abap_true.

      SELECT *
        FROM zppt0002
        INTO TABLE @DATA(it_002)
        WHERE werks  EQ @wa_006-werks
          AND id_cotton EQ @wa_006-id_cotton.

      IF sy-subrc IS INITIAL.

        SORT it_002 BY id_cotton.
        DELETE ADJACENT DUPLICATES FROM it_002 COMPARING id_cotton.

        LOOP AT it_002 INTO DATA(w_002).

          IF w_002-mblnr02 IS INITIAL AND check IS INITIAL.

            UPDATE zppt0002 SET mblnr02 = wa_006-cd_mensagem+27(10)
            WHERE werks  EQ wa_006-werks
              AND id_cotton EQ wa_006-id_cotton.

            check = abap_true.

          ENDIF.

        ENDLOOP.
      ENDIF.

      check = abap_false.

    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD get_doc_gerado_04.

    DATA check TYPE char1.

*   Pega os Logs de Processamento para verificar se já esta com DOC Gerado.
    SELECT *
      FROM zppt0006
      INTO TABLE @DATA(it_0006)
    WHERE acharg     EQ @acharg
      AND status_msg EQ 'S'
      ORDER BY data, hora.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE status_registro
      FROM zppt0006
      INTO @DATA(vl_status_registro)
    WHERE acharg     EQ @acharg
      AND status_msg EQ 'S'
      AND id IN ( SELECT MAX( id )
                   FROM zppt0006
                  WHERE acharg    EQ @acharg
                    AND status_msg EQ 'S' ).

    CHECK vl_status_registro EQ status_entrada.

*   Marca como W todos os status 04/02 ate o estorno 05/06
*   Caso encontre um 05 todos os 04/02 W são transformados em Y
*   e no final deletamos todos os Y
    LOOP AT it_0006 ASSIGNING FIELD-SYMBOL(<f_006>).

      IF <f_006>-status_registro EQ status_entrada.
        <f_006>-flag_envio = 'W'.
      ENDIF.

      IF <f_006>-status_registro EQ status_estorno.
        <f_006>-flag_envio = 'Y'.
        MODIFY it_0006 FROM <f_006> TRANSPORTING flag_envio WHERE flag_envio EQ 'W'.
      ENDIF.

    ENDLOOP.

    DELETE it_0006 WHERE flag_envio EQ 'Y'.
    SORT it_0006 BY acharg.
    DELETE ADJACENT DUPLICATES FROM it_0006 COMPARING acharg.

    CHECK lines( it_0006 ) EQ 1.

    LOOP AT it_0006 INTO DATA(wa_006) WHERE status_registro = status_entrada.

      UPDATE zppt0006 SET flag_envio = 'R'
      WHERE charg  EQ wa_006-charg
        AND acharg EQ wa_006-acharg
        AND werks  EQ wa_006-werks
        AND id     EQ wa_006-id
        AND id_cotton EQ wa_006-id_cotton.

      doc_existe = abap_true.

      SELECT *
        FROM zppt0002
        INTO TABLE @DATA(it_002)
        WHERE werks  EQ @wa_006-werks
          AND id_cotton EQ @wa_006-id_cotton
          AND acharg    EQ @wa_006-acharg.

      IF sy-subrc IS INITIAL.

        LOOP AT it_002 INTO DATA(w_002).

          IF w_002-mblnr IS INITIAL AND check IS INITIAL.

            UPDATE zppt0002 SET mblnr = wa_006-cd_mensagem+29(10)
            WHERE werks  EQ wa_006-werks
              AND id_cotton EQ wa_006-id_cotton
              AND acharg    EQ wa_006-acharg
              AND werks     EQ wa_006-werks.

            check = abap_true.

          ENDIF.

        ENDLOOP.
      ENDIF.

      check = abap_false.

    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD get_qtde_program_exec.

    DATA: lc_quantidade_int    TYPE i,
          lc_quantidade_str_19 TYPE c LENGTH 10,
          lc_quantidade_str_20 TYPE c LENGTH 10,
          t_status             TYPE zde_btcstatus_t.

    FREE: e_quantidade, t_status.

    APPEND 'S' TO t_status.
    APPEND 'R' TO t_status.
    APPEND 'P' TO t_status.

*-----------------------------------------------------------------------------*
* Get Jobs do programa ZMMR030( Criação Registros ) em Execução
*-----------------------------------------------------------------------------*
    CASE i_tipo.
      WHEN '1'. "Proc Normal
        TRY.
            zcl_job=>get_job_programa_execucao(
              EXPORTING
                i_progname   = zif_integracao_cotton_sap~c_zmmr0030   " Nome de um programa em uma etapa (p.ex. report)
                i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
                i_status     = t_status   " Status de Jobs
              IMPORTING
                e_quantidade = lc_quantidade_int ).
          CATCH zcx_job.
        ENDTRY.

        ADD lc_quantidade_int TO e_quantidade.

        WRITE lc_quantidade_int TO lc_quantidade_str_19.
        CONDENSE lc_quantidade_str_19 NO-GAPS.

      WHEN '2'.  "Proc Estorno
        TRY.
            zcl_job=>get_job_programa_execucao(
              EXPORTING
                i_progname   = zif_integracao_cotton_sap~c_zmmr0031   " Nome de um programa em uma etapa (p.ex. report)
                i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
                i_status     = t_status   " Status de Jobs
              IMPORTING
                e_quantidade = lc_quantidade_int ).
          CATCH zcx_job.
        ENDTRY.

        ADD lc_quantidade_int TO e_quantidade.

        WRITE lc_quantidade_int TO lc_quantidade_str_20.
        CONDENSE lc_quantidade_str_20 NO-GAPS.
    ENDCASE.

    IF i_show_msg EQ abap_true.
      MESSAGE s007(zjob) WITH zif_integracao_cotton_sap~c_zmmr0030
                              lc_quantidade_str_19
                              zif_integracao_cotton_sap~c_zmmr0031
                              lc_quantidade_str_20.
    ENDIF.

  ENDMETHOD.


  METHOD job_envio.

    CONSTANTS: c_zmmr0040_job         TYPE btcprog VALUE 'ZMMR0040_JOB'.

    DATA: number           TYPE tbtcjob-jobcount,
          name             TYPE tbtcjob-jobname,
          t_0002           TYPE TABLE OF zppt0002,
          wa_zppt0026      TYPE zppt0026,
          user             TYPE sy-uname,
          l_number         TYPE char15,
          print_parameters TYPE pri_params,
          w_zppt0030       TYPE zppt0030,
          l_quantidade_int TYPE i.

*-----------------------------------------------------------------------------------------------------
*   Job
*---------------------------------------------------------------------------------
    CLEAR: wa_zppt0026.

    name = |JOB_TRACE_COTTON|.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQ0008'
      IMPORTING
        number                  = wa_zppt0026-seq
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    wa_zppt0026-jobname       = name.
    wa_zppt0026-id_referencia = me->zif_integracao_cotton_sap~at_protocolo_recebimento.
    wa_zppt0026-dt_ini        = sy-datum.
    wa_zppt0026-hr_ini        = sy-uzeit.

    MODIFY zppt0026 FROM wa_zppt0026.

*-------------------------------------------------
*   Criar blocos de processamento
*-------------------------------------------------
    LOOP AT me->zif_integracao_cotton_sap~at_proc_zppt0002_t INTO DATA(lwa_zppt0002).

      CLEAR: w_zppt0030.

      IF lwa_zppt0002-id_sessao IS NOT INITIAL.
        w_zppt0030-chave_quebra      = lwa_zppt0002-id_sessao.
      ELSEIF lwa_zppt0002-lgort IS NOT INITIAL.
        w_zppt0030-chave_quebra      = lwa_zppt0002-lgort.
      ENDIF.

      w_zppt0030-chave_lock         = lwa_zppt0002-werks && '9999999999'.
      w_zppt0030-id_referencia      = me->zif_integracao_cotton_sap~at_protocolo_recebimento.
      w_zppt0030-werks              = lwa_zppt0002-werks.
      w_zppt0030-matnr              = lwa_zppt0002-matnr.
      w_zppt0030-cd_classificacao   = lwa_zppt0002-cd_classificacao.
      w_zppt0030-status_registro    = lwa_zppt0002-status_registro.
      w_zppt0030-processamento      = '01'.
      w_zppt0030-data_proc          = sy-datum.
      w_zppt0030-hora_proc          = sy-uzeit.

      MODIFY zppt0030            FROM w_zppt0030.
    ENDLOOP.

    COMMIT WORK.


  ENDMETHOD.


  METHOD job_envio_estorno.

    CONSTANTS: c_zmmr0040_job         TYPE btcprog VALUE 'ZMMR0040_JOB'.

    DATA: number           TYPE tbtcjob-jobcount,
          name             TYPE tbtcjob-jobname,
          wa_zppt0026      TYPE zppt0026,
          user             TYPE sy-uname,
          l_number         TYPE char15,
          l_start_imed     TYPE char1,
          l_jobname_pred   TYPE tbtcjob-jobname,
          w_zppt0030       TYPE zppt0030,
          print_parameters TYPE pri_params,
          t_status         TYPE zde_btcstatus_t,
          l_quantidade_int TYPE i.

*-------------------------------------------------
* Job
*-------------------------------------------------
    FREE: t_status.

    APPEND 'S' TO t_status.
    APPEND 'R' TO t_status.
    APPEND 'P' TO t_status.

    name = |JOB_TRACE_COTTON|.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQ0008'
      IMPORTING
        number                  = wa_zppt0026-seq
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    wa_zppt0026-jobname       = name.
    wa_zppt0026-id_referencia = me->zif_integracao_cotton_sap~at_protocolo_recebimento.
    wa_zppt0026-dt_ini        = sy-datum.
    wa_zppt0026-hr_ini        = sy-uzeit.

    MODIFY zppt0026 FROM wa_zppt0026.

*-------------------------------------------------
* - criar blocos de processamento
*-------------------------------------------------
    LOOP AT me->zif_integracao_cotton_sap~at_proc_zppt0002_t INTO DATA(lwa_zppt0002).

      w_zppt0030-mandt             = sy-mandt.
      w_zppt0030-id_referencia     = me->zif_integracao_cotton_sap~at_protocolo_recebimento.
      w_zppt0030-werks             = lwa_zppt0002-werks.

      IF lwa_zppt0002-id_sessao IS NOT INITIAL.
        w_zppt0030-chave_quebra      = lwa_zppt0002-id_sessao.
      ELSEIF lwa_zppt0002-lgort IS NOT INITIAL.
        w_zppt0030-chave_quebra      = lwa_zppt0002-lgort.
      ENDIF.

      w_zppt0030-chave_lock        = '9999999999'.
      w_zppt0030-matnr             = lwa_zppt0002-matnr.
      w_zppt0030-cd_classificacao  = lwa_zppt0002-cd_classificacao.
      w_zppt0030-status_registro   = lwa_zppt0002-status_registro.
      w_zppt0030-processamento     = '02'.
      w_zppt0030-data_proc         = sy-datum.
      w_zppt0030-hora_proc         = sy-uzeit.

      MODIFY zppt0030          FROM w_zppt0030.
    ENDLOOP.

    COMMIT WORK.

*-------------------------------------------------
* - Escalonar JOB
*-------------------------------------------------
*---verifica se job ja esta sendo executado
*    TRY.
*        zcl_job=>get_job_programa_execucao(
*          EXPORTING
*            i_progname   = c_zmmr0040_job   " Nome de um programa em uma etapa (p.ex. report)
*            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
*            i_status     = t_status   " Status de Jobs
*          IMPORTING
*            e_quantidade = l_quantidade_int ).
*      CATCH zcx_job.
*    ENDTRY.
*
*    CHECK l_quantidade_int = 0.
*
*    name = |JOB_TRACE_COTTON|.
*    user = sy-uname.
*
*    CALL FUNCTION 'JOB_OPEN'
*      EXPORTING
*        jobname          = name
*        sdlstrtdt        = sy-datum
*        sdlstrttm        = sy-uzeit
*      IMPORTING
*        jobcount         = number
*      EXCEPTIONS
*        cant_create_job  = 1
*        invalid_job_data = 2
*        jobname_missing  = 3
*        OTHERS           = 4.
*
*    IF sy-subrc IS INITIAL.
*
*      sy-uname = 'JOBADM'.
*
*      SUBMIT zmmr0040_job VIA JOB    name
*                              NUMBER number
*                          AND RETURN.
*
*      IF sy-subrc IS INITIAL.
*        CALL FUNCTION 'JOB_CLOSE'
*          EXPORTING
*            jobcount             = number
*            jobname              = name
*            strtimmed            = abap_true
*          EXCEPTIONS
*            cant_start_immediate = 1
*            invalid_startdate    = 2
*            jobname_missing      = 3
*            job_close_failed     = 4
*            job_nosteps          = 5
*            job_notex            = 6
*            lock_failed          = 7
*            OTHERS               = 8.
*      ENDIF.
*
*      sy-uname = user.
*    ENDIF.
*
  ENDMETHOD.


  METHOD lock_objeto.

    DATA: w_zppt0031        TYPE zppt0031.

    FREE: e_erro.

    CALL FUNCTION 'ENQUEUE_EZPPT0031'
      EXPORTING
        mode_zppt0031  = 'E'
        mandt          = sy-mandt
        chave_quebra   = i_objeto
        _scope         = '2'
        _wait          = abap_true
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      e_erro = abap_true.
      EXIT.
    ENDIF.

    w_zppt0031-mandt            = sy-mandt.
    w_zppt0031-chave_quebra     = i_objeto.
    w_zppt0031-data_fim         = sy-datum.
    w_zppt0031-hora_fim         = sy-uzeit.
    MODIFY zppt0031          FROM w_zppt0031.

    COMMIT WORK.

  ENDMETHOD.


  METHOD unlock_objeto.

    DATA: w_zppt0031        TYPE zppt0031.

    CALL FUNCTION 'DEQUEUE_EZPPT0031'
      EXPORTING
        mode_zppt0031 = 'E'
        mandt         = sy-mandt
        chave_quebra  = i_objeto
        _scope        = '3'.
*       _synchron     = abap_true.

    w_zppt0031-mandt            = sy-mandt.
    w_zppt0031-chave_quebra     = i_objeto.
    w_zppt0031-data_fim         = sy-datum.
    w_zppt0031-hora_fim         = sy-uzeit.
    MODIFY zppt0031          FROM w_zppt0031.

    COMMIT WORK.

  ENDMETHOD.


  METHOD verifica_bloquear_job.

    DATA: l_datum     TYPE sy-datum,
          l_uzeit     TYPE sy-uzeit,
          l_data_str1 TYPE char20,
          l_data_str2 TYPE char20,
          l_data_str3 TYPE char20.

    e_bloqueado = abap_false.

*-----------------------
*-- simulacao data
*-----------------------
    SELECT SINGLE *
      INTO @DATA(w_tvarv)
      FROM tvarvc
     WHERE name = 'ZPPT0030_DATA_SIMULA'.
    IF sy-subrc = 0.
      l_datum = w_tvarv-low+6(4) && w_tvarv-low+3(2) && w_tvarv-low(2).
    ELSE.
      l_datum = sy-datum.
    ENDIF.

*-----------------------
*-- simulacao hora
*-----------------------
    SELECT SINGLE *
      INTO w_tvarv
      FROM tvarvc
     WHERE name = 'ZPPT0030_HORA_SIMULA'.
    IF sy-subrc = 0.
      l_uzeit = w_tvarv-low(2) && w_tvarv-low+3(2) && w_tvarv-low+6(2).
    ELSE.
      l_uzeit = sy-uzeit.
    ENDIF.

*-----------------------
*-- historivo
*-----------------------
    SELECT *
      FROM zppt0036
      INTO TABLE @DATA(t_0036)
     WHERE data_bloq_fim >= @l_datum.

*-----------------------
*-- verifica se esta blqoueado
*-----------------------
    LOOP AT t_0036 INTO DATA(w_0036).
      l_data_str1     = w_0036-data_bloq_ini && w_0036-hora_bloq_ini.
      l_data_str2     = w_0036-data_bloq_fim && w_0036-hora_bloq_fim.
      l_data_str3     = l_datum              && l_uzeit.

      IF l_data_str1 <= l_data_str3  AND
         l_data_str2 >= l_data_str3.
        e_bloqueado = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_integracao_cotton_sap~get_check_job.

    DATA: wa_zppe0002 TYPE zppe0002,
          t1          TYPE t,
          t2          TYPE t,
          def         TYPE p.

    DATA: hh1     TYPE n LENGTH 2,
          mm1     TYPE n LENGTH 2,
          ss1     TYPE n LENGTH 2,
          hhs1    TYPE i,
          mms1    TYPE i,
          sss1    TYPE i,
          tmp1    TYPE i,
          hh2     TYPE n LENGTH 2,
          mm2     TYPE n LENGTH 2,
          ss2     TYPE n LENGTH 2,
          hhs2    TYPE i,
          mms2    TYPE i,
          sss2    TYPE i,
          tmp2    TYPE i,
          tmp3    TYPE i,
          tmp4    TYPE i,
          calculo TYPE p DECIMALS 5.

    r_if_integracao_cotton_sap = me.

    SELECT COUNT(*)
      FROM zppt0026
      INTO me->zif_integracao_cotton_sap~at_status-qtd_solicitacao_em_fila
    WHERE dt_fim  = '00000000'.

    SELECT SUM( qtd_item )
      FROM zppt0026
      INTO me->zif_integracao_cotton_sap~at_status-qtd_registro_em_fila
    WHERE dt_fim  = '00000000'
       AND qtd_item <> '999999'.

    SELECT COUNT(*)
      FROM zppt0026
      INTO me->zif_integracao_cotton_sap~at_status-qtd_solicitacao_processada_dia
    WHERE dt_fim  = sy-datum.

    SELECT SUM( qtd_item )
      FROM zppt0026
      INTO me->zif_integracao_cotton_sap~at_status-qtd_registro_processado_dia
    WHERE dt_fim  = sy-datum
      AND dt_done <> '00000000'.

    SELECT *
      FROM zppt0026
      INTO TABLE @DATA(it_0026)
    WHERE dt_fim  = @sy-datum
      AND dt_done <> '00000000'.

    LOOP AT it_0026 INTO DATA(wa_0026).

      CLEAR: hhs1, mms1, sss1, tmp1, hhs2, mms2, sss2, tmp2, tmp3.

      hh1 = wa_0026-hr_fim(2).
      mm1 = wa_0026-hr_fim+2(2).
      ss1 = wa_0026-hr_fim+4(2).

      hhs1 = hh1 * 3600.
      mms1 = mm1 * 60.
      sss1 = ss1.

      ADD hhs1 TO tmp1.
      ADD mms1 TO tmp1.
      ADD sss1 TO tmp1.

      hh2 = wa_0026-hr_done(2).
      mm2 = wa_0026-hr_done+2(2).
      ss2 = wa_0026-hr_done+4(2).

      hhs2 = hh2 * 3600.
      mms2 = mm2 * 60.
      sss2 = ss2.

      ADD hhs2 TO tmp2.
      ADD mms2 TO tmp2.
      ADD sss2 TO tmp2.

      tmp3 = tmp2 - tmp1.

      ADD tmp3 TO tmp4.

    ENDLOOP.

    calculo = tmp4 / me->zif_integracao_cotton_sap~at_status-qtd_registro_processado_dia.
    me->zif_integracao_cotton_sap~at_status-time_medio_registro_processado = calculo.

    CONDENSE me->zif_integracao_cotton_sap~at_status-time_medio_registro_processado NO-GAPS.

    SELECT COUNT(*)
      FROM tbtco
      WHERE jobname LIKE 'JOB_FILA_%'
      AND status = 'R'.

    IF sy-subrc IS INITIAL.
      me->zif_integracao_cotton_sap~at_status-sts_job_pri_ativo = abap_true.
    ELSE.
      me->zif_integracao_cotton_sap~at_status-sts_job_pri_ativo = abap_false.
    ENDIF.

    SELECT COUNT(*)
      FROM tbtco
      WHERE jobname LIKE 'JOB_COTTON_0%'
      AND status = 'R'.

    IF sy-subrc IS INITIAL.
      me->zif_integracao_cotton_sap~at_status-sts_job_sec_ativo = abap_true.
    ELSE.
      me->zif_integracao_cotton_sap~at_status-sts_job_sec_ativo = abap_false.
    ENDIF.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = me->zif_integracao_cotton_sap~at_status
        pretty_name = abap_true
      RECEIVING
        r_json      = e_msg.

  ENDMETHOD.


  METHOD zif_integracao_cotton_sap~get_double_check.
    r_if_integracao_cotton_sap = me.
  ENDMETHOD.


  METHOD zif_integracao_cotton_sap~get_instance.

    IF zif_integracao_cotton_sap~at_if_integracao_cotton_sap IS NOT BOUND.
      CREATE OBJECT zif_integracao_cotton_sap~at_if_integracao_cotton_sap TYPE zcl_integracao_cotton_sap.
    ENDIF.
    r_if_integracao_cotton_sap = zif_integracao_cotton_sap~at_if_integracao_cotton_sap.

  ENDMETHOD.


  METHOD zif_integracao_cotton_sap~set_ds_data.

    "Projeto Restruturação Algodao 2024 - WPP
    DATA: lva_id_protocolo_cotton TYPE zde_protocolo_cotton.

    DATA: lwa_inbound  TYPE zpps0001.

    r_if_integracao_cotton_sap = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

    CLEAR: me->zif_integracao_cotton_sap~at_retorno.

    /ui2/cl_json=>deserialize( EXPORTING json =   CONV #( me->zif_integracao_inject~at_info_request_http-ds_body ) CHANGING data = lwa_inbound ).

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZPROCOTTON'
      IMPORTING
        number                  = lva_id_protocolo_cotton
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc NE 0 OR lva_id_protocolo_cotton IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Objeto numeração ZPROCOTTON sem parametro'
                            )
          msgid  = zcx_integracao=>zcx_erro_body_recebido-msgid
          msgno  = zcx_integracao=>zcx_erro_body_recebido-msgno
          msgv1  = 'Objeto numeração ZPROCOTTON sem parametro'
          msgty  = 'E'.
    ENDIF.

    me->zif_integracao_cotton_sap~at_protocolo_recebimento = 'C' && sy-datum(4) && sy-datum+4(2) && lva_id_protocolo_cotton && '-' && lwa_inbound-dadosgerais-tp_integracao.


*    DATA: i_inbound TYPE zppt016.
*
*    "Incluir Texto JSON para integração
*    r_if_integracao_cotton_sap = me.
*    me->zif_integracao_inject~at_info_request_http = i_info. "
*
*    "Validar Json
*    /ui2/cl_json=>deserialize( EXPORTING jsonx = CONV #( i_info-ds_body ) CHANGING data = i_inbound ).
*    /ui2/cl_json=>deserialize( EXPORTING json = i_info-ds_body CHANGING data = i_inbound ).
*
*    IF id_referencia IS NOT INITIAL.
*      me->zif_integracao_cotton_sap~at_recebido = id_referencia.
*    ELSE.
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*        EXPORTING
*          nr_range_nr             = '01'
*          object                  = 'ZSEQ_RECEB'
*        IMPORTING
*          number                  = me->zif_integracao_cotton_sap~at_recebido
*        EXCEPTIONS
*          interval_not_found      = 1
*          number_range_not_intern = 2
*          object_not_found        = 3
*          quantity_is_0           = 4
*          quantity_is_not_1       = 5
*          interval_overflow       = 6
*          buffer_overflow         = 7
*          OTHERS                  = 8.
*    ENDIF.
*
*    IF i_inbound IS NOT INITIAL.
*      me->zif_integracao_cotton_sap~at_zppt0002 = i_inbound.
*      me->zif_integracao_inject~at_referencia-id_referencia = me->zif_integracao_cotton_sap~at_recebido.
*    ENDIF.
*
*    CHECK i_inbound IS INITIAL.
*
*    RAISE EXCEPTION TYPE zcx_integracao
*      EXPORTING
*        textid = VALUE #( msgid = zcx_integracao=>zcx_erro_body_recebido-msgid
*                          msgno = zcx_integracao=>zcx_erro_body_recebido-msgno )
*        msgid  = zcx_integracao=>zcx_erro_body_recebido-msgid
*        msgno  = zcx_integracao=>zcx_erro_body_recebido-msgno
*        msgty  = 'E'.

    "Projeto Restruturação Algodao 2024 - WPP

  ENDMETHOD.


  METHOD zif_integracao_cotton_sap~set_estorna_dados.

    "Projeto Restruturação Algodao 2024 - WPP - Ini
*    DATA: lwa_inbound  TYPE zpps0001,
*          lit_zppt0002 TYPE TABLE OF zppt0002.
*
*    DATA: lwa_zppt0002 TYPE zppt0002.
*
*    r_if_integracao_cotton_sap = me.
*
*    /ui2/cl_json=>deserialize( EXPORTING json =   CONV #( me->zif_integracao_inject~at_info_request_http-ds_body ) CHANGING data = lwa_inbound ).
*
**-------------------------------------------------------------------------------------------------------------------------*
**   Validações - Ini
**-------------------------------------------------------------------------------------------------------------------------*
*    DATA(lwa_dados_gerais) = lwa_inbound-dadosgerais.
*    DATA(lit_detalhamento) = lwa_inbound-detalhamento.
*
*    IF lwa_inbound IS INITIAL.
*      me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Requisição sem dados!'.
*      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
*      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
*      RETURN.
*    ENDIF.
*
*    IF lwa_inbound-dadosgerais-id_movimentacao IS INITIAL.
*      me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Id. Movimentação não informado!'.
*      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
*      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
*      RETURN.
*    ENDIF.
*
*    "Tratativas formato Campos
*    LOOP AT lit_detalhamento INTO DATA(lwa_detalhamento).
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lwa_detalhamento-id_material_sap
*        IMPORTING
*          output = lwa_detalhamento-id_material_sap.
*    ENDLOOP.
*
*
**-------------------------------------------------------------------------------------------------------------------------*
**   Validações - Fim
**-------------------------------------------------------------------------------------------------------------------------*
*
*    "Estornar Dados Sessão
*    CLEAR: lwa_zppt0002.
*
*    "Informações a Nivel de cabecalho
*    me->fill_dados_header_zppt0002(
*      EXPORTING
*        i_dados_gerais = lwa_dados_gerais
*      CHANGING
*        c_zppt0002     = lwa_zppt0002 ).
*
*    APPEND lwa_zppt0002 TO lit_zppt0002.
*
*
*    "Estornar Blocos
*    LOOP AT lit_detalhamento INTO lwa_detalhamento.
*
*      CLEAR: lwa_zppt0002.
*
*      "Informações a Nivel de cabecalho
*      me->fill_dados_header_zppt0002(
*        EXPORTING
*          i_dados_gerais = lwa_dados_gerais
*        CHANGING
*          c_zppt0002     = lwa_zppt0002 ).
*
*      "Itens
*      CLEAR: lwa_zppt0002-id_sessao.
*
*      lwa_zppt0002-lgort                           = lwa_detalhamento-bloco.
*      lwa_zppt0002-qtd_bloco                       = lwa_detalhamento-capacidade_bloco.
*      lwa_zppt0002-verid                           = lwa_detalhamento-versao_producao.
*      lwa_zppt0002-matnr                           = lwa_detalhamento-id_material_sap.
*      lwa_zppt0002-peso_bruto                      = lwa_detalhamento-peso_bruto_fardos.
*      lwa_zppt0002-peso_liquido                    = lwa_detalhamento-peso_liquido_fardos.
*      lwa_zppt0002-qtd_fardinhos_bloco             = lwa_detalhamento-qtd_fardinhos.
*      lwa_zppt0002-cd_classificacao                = lwa_detalhamento-cd_classificacao.
*      lwa_zppt0002-dt_classificacao                = |{ lwa_detalhamento-dt_envio_classificacao+6(4) }{ lwa_detalhamento-dt_envio_classificacao+3(2) }{ lwa_detalhamento-dt_envio_classificacao(2) }|.
*      lwa_zppt0002-bloco_destino                   = lwa_detalhamento-bloco_destino-bloco.
*      lwa_zppt0002-cd_classificacao_bloco_destino  = lwa_detalhamento-bloco_destino-cd_classificacao.
*
*
*      APPEND lwa_zppt0002 TO lit_zppt0002.
*
*    ENDLOOP.
*
*   LOOP AT lit_zppt0002 INTO lwa_zppt0002.
*
*      SELECT SINGLE *
*        FROM zppt0002 INTO @DATA(lwa_zppt0002_exists)
*       WHERE acharg     = @lwa_zppt0002-acharg
*         AND werks      = @lwa_zppt0002-werks
*         AND id_sessao  = @lwa_zppt0002-id_sessao
*         AND lgort      = @lwa_zppt0002-lgort.
*
*      CHECK sy-subrc eq 0.
*
*      "Informações Cabeçalho
*      lwa_zppt0002_exists-status_processamento            = 'P'. "Processamento Pendente
*      lwa_zppt0002_exists-status_ret_sistema_origem       = 'P'. "Retorno Pendente
*      lwa_zppt0002_exists-id_referencia                   = lwa_zppt0002-id_referencia.
*      lwa_zppt0002_exists-status_registro                 = lwa_zppt0002-status_registro.
*      lwa_zppt0002_exists-bldat                           = lwa_zppt0002-bldat.
*      lwa_zppt0002_exists-budat                           = lwa_zppt0002-budat.
*      lwa_zppt0002_exists-qtd_fardinhos_sessao            = lwa_zppt0002-qtd_fardinhos_sessao.
*      lwa_zppt0002_exists-cd_safra                        = lwa_zppt0002-cd_safra.
*      lwa_zppt0002_exists-peso_algodao_caroco             = lwa_zppt0002-peso_algodao_caroco.
*      lwa_zppt0002_exists-menge                           = lwa_zppt0002-menge.
*      lwa_zppt0002_exists-peso_caroco                     = lwa_zppt0002-peso_caroco.
*      lwa_zppt0002_exists-peso_fibrilha                   = lwa_zppt0002-peso_fibrilha.
*      lwa_zppt0002_exists-id_mov_sistema_origem           = lwa_zppt0002-id_mov_sistema_origem.
*      lwa_zppt0002_exists-status                          = lwa_zppt0002-status.
*      lwa_zppt0002_exists-laeda                           = lwa_zppt0002-laeda.
*      lwa_zppt0002_exists-laehr                           = lwa_zppt0002-laehr.
*
*      "Informações Detalhamento
*      lwa_zppt0002_exists-lgort                           = lwa_zppt0002-lgort.
*      lwa_zppt0002_exists-qtd_bloco                       = lwa_zppt0002-qtd_bloco.
*      lwa_zppt0002_exists-verid                           = lwa_zppt0002-verid.
*      lwa_zppt0002_exists-matnr                           = lwa_zppt0002-matnr.
*      lwa_zppt0002_exists-peso_bruto                      = lwa_zppt0002-peso_bruto.
*      lwa_zppt0002_exists-peso_liquido                    = lwa_zppt0002-peso_liquido.
*      lwa_zppt0002_exists-qtd_fardinhos_bloco             = lwa_zppt0002-qtd_fardinhos_bloco.
*      lwa_zppt0002_exists-cd_classificacao                = lwa_zppt0002-cd_classificacao.
*      lwa_zppt0002_exists-dt_classificacao                = lwa_zppt0002-dt_classificacao.
*      lwa_zppt0002_exists-bloco_destino                   = lwa_zppt0002-bloco_destino.
*      lwa_zppt0002_exists-cd_classificacao_bloco_destino  = lwa_zppt0002-cd_classificacao_bloco_destino.
*
*      MODIFY zppt0002 FROM lwa_zppt0002_exists.
*
*    ENDLOOP.
*
*    COMMIT WORK.
*
*    me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Solicitação Recebida com sucesso!'.
*    me->zif_integracao_cotton_sap~at_retorno-protocolo  = me->zif_integracao_cotton_sap~at_protocolo_recebimento.
*    me->zif_integracao_cotton_sap~at_retorno-statuscode = '200'.
*    RETURN.
*
*    me->zif_integracao_cotton_sap~at_proc_zppt0002_t[] = lit_zppt0002[].
*
*    CALL METHOD me->job_envio_estorno.


*    DATA: w_0002     TYPE zppt0002,
*          t_0002     TYPE TABLE OF zppt0002,
*          t_0002_job TYPE TABLE OF zppt0002,
*          in_fardos  TYPE zrsdsselopts,
*          w_0007     TYPE zppt0007,
*          embarcado  TYPE c,
*          l_erro     TYPE c.
*
*    r_if_integracao_cotton_sap = me.
*
*    LOOP AT me->zif_integracao_cotton_sap~at_zppt0002 INTO DATA(_0002).
*
**---------------------------------
**---- existe fardo?
**---------------------------------
*      SELECT *
*        INTO TABLE @DATA(t_zppt0002)
*        FROM zppt0002
*       WHERE id_cotton = @_0002-nr_fardo_origem.
*
*      IF sy-subrc <> 0.
*        _0002-id_referencia = _0002-nr_fardo_origem.
*        _0002-cd_mensagem   = |ID_COTTON nao existe, verificar!|.
*        APPEND _0002       TO me->zif_integracao_cotton_sap~at_retorno[].
*        CLEAR: w_0007, w_0002.
*        CONTINUE.
*      ENDIF.
*
**---------------------------------
**---- esta em processamento?
**---------------------------------
*      READ TABLE t_zppt0002 INTO DATA(w_zppt0002)
*                                 WITH KEY status          = 'R'
*                                          status_registro = '99'.
*      IF sy-subrc = 0.
*        _0002-id_referencia = _0002-nr_fardo_origem.
*        _0002-cd_mensagem   = |Existem fardos ainda em processamento de estorno!|.
*        APPEND _0002 TO me->zif_integracao_cotton_sap~at_retorno[].
*
*        MOVE-CORRESPONDING w_zppt0002 TO w_0007.
*        w_0007-cd_mensagem             = _0002-cd_mensagem.
*        w_0007-seq_recebido            = me->zif_integracao_cotton_sap~at_recebido.
*
*        me->add_zppt0007( w_0007 ).
*
*        CLEAR: w_0007, w_0002.
*        CONTINUE.
*      ENDIF.
*
**---------------------------------
**---- ver saldo
**---------------------------------
*      SELECT *
*        INTO TABLE @DATA(t_mchb)
*        FROM mchb
*         FOR ALL ENTRIES IN @t_zppt0002
*       WHERE charg = @t_zppt0002-acharg
*         AND werks = @t_zppt0002-werks.
*
*      DATA(t_mchb_aux) = t_mchb.
*      DELETE t_mchb_aux WHERE clabs <> 0.
*
*      DELETE t_mchb WHERE clabs <= 0.
*
*      SORT t_mchb BY charg.
*      DELETE ADJACENT DUPLICATES FROM t_mchb
*                            COMPARING charg.
*
**---------------------------------
**---- montar tabela para cada fardinho
**---------------------------------
*      FREE l_erro.
*
*      LOOP AT t_zppt0002 INTO w_zppt0002.
*
*        READ TABLE t_mchb INTO DATA(w_mchb) WITH KEY charg = w_zppt0002-acharg
*                                            BINARY SEARCH.
*
*        MOVE w_zppt0002           TO w_0002.
*        w_0002-id_referencia       = me->zif_integracao_cotton_sap~at_recebido.
*
*        APPEND w_0002             TO t_0002.
*        MOVE-CORRESPONDING w_0002 TO w_0007.
*        w_0007-seq_recebido        = me->zif_integracao_cotton_sap~at_recebido.
*        w_0007-status_registro = '99'.
*
*        CLEAR w_0002.
*
*        me->add_zppt0007( w_0007 ).
*      ENDLOOP.
*
*      IF t_0002[] IS INITIAL OR l_erro = abap_true.
*        _0002-id_referencia = _0002-nr_fardo_origem.
*        _0002-cd_mensagem   = |Impossivel estornar os fardinhos associados.|.
*
*        APPEND _0002       TO me->zif_integracao_cotton_sap~at_retorno[].
*
*        MOVE-CORRESPONDING w_zppt0002 TO w_0007.
*        w_0007-cd_mensagem  = _0002-cd_mensagem.
*        w_0007-seq_recebido = me->zif_integracao_cotton_sap~at_recebido.
*
*        me->add_zppt0007( w_0007 ).
*        CLEAR w_0002.
*
*        DELETE t_0002 WHERE id_cotton = _0002-nr_fardo_origem.
*        CONTINUE.
*      ELSE.
*        _0002-id_referencia = _0002-nr_fardo_origem.
*        APPEND _0002  TO me->zif_integracao_cotton_sap~at_retorno[].
*      ENDIF.
*
*    ENDLOOP.
*
*    SORT t_0002 BY id_cotton.
*    FREE in_fardos.
*
*    LOOP AT t_0002 INTO w_0002.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = w_0002-id_cotton ) TO in_fardos.
*
*      UPDATE zppt0002 SET status           = 'R',    "// Estorno
*                          status_registro  = '99',
*                          id_referencia    = @me->zif_integracao_cotton_sap~at_recebido
*                    WHERE acharg           = @w_0002-acharg
*                      AND werks            = @w_0002-werks.
*    ENDLOOP.
*
*    COMMIT WORK.
*
*    t_0002_job[] = t_0002[].
*    SORT t_0002_job BY id_cotton.
*    DELETE ADJACENT DUPLICATES FROM t_0002_job
*                          COMPARING id_cotton.
*
**-------------------------
**-- FArdos a serem processados
**-------------------------
*    me->zif_integracao_cotton_sap~at_proc_zppt0002_t[] = t_0002[].
*
**---------------------------------
**-- enviar fardoes para processamento
**---------------------------------
*    CALL METHOD me->job_envio_estorno
*      EXPORTING
*        it_zppt0002   = t_0002_job
*        estorno       = abap_true
*        id_referencia = me->zif_integracao_cotton_sap~at_recebido.

   "Projeto Restruturação Algodao 2024 - WPP - Fim

  ENDMETHOD.


  METHOD zif_integracao_cotton_sap~set_processa_dados.

    "Projeto Restruturação Algodao 2024 - WPP - Ini
    DATA: lwa_inbound           TYPE zpps0001,
          lit_zppt0002          TYPE TABLE OF zppt0002,
          lwa_zppt0002_wait     TYPE zppt0002_wait,
          lit_zppt0002_aux      TYPE TABLE OF zppt0002,
          lit_zppt0006_aux      TYPE TABLE OF zppt0006,
          lit_zppt0002_wait_aux TYPE TABLE OF zppt0002_wait.

    DATA: lva_peso_aux TYPE menge_d.
    DATA: lva_qtde_aux TYPE menge_d.

    DATA: lwa_zppt0002      TYPE zppt0002.

    r_if_integracao_cotton_sap = me.

    /ui2/cl_json=>deserialize( EXPORTING json =   CONV #( me->zif_integracao_inject~at_info_request_http-ds_body ) CHANGING data = lwa_inbound ).

*-------------------------------------------------------------------------------------------------------------------------*
*   Validações Dados Request
*-------------------------------------------------------------------------------------------------------------------------*
    DATA(lwa_dados_gerais) = lwa_inbound-dadosgerais.
    DATA(lit_detalhamento) = lwa_inbound-detalhamento.

    IF lwa_inbound IS INITIAL.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Requisição sem dados!'.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.

    IF lwa_dados_gerais-tp_integracao IS INITIAL.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Tp. Integração não informado!'.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.

    IF lwa_dados_gerais-tp_integracao NE '00' AND "Homologação Sessao
       lwa_dados_gerais-tp_integracao NE '03' AND "Classificação
       lwa_dados_gerais-tp_integracao NE '07' AND "Troca Bloco/Troca Material
       lwa_dados_gerais-tp_integracao NE '99'.    "Estorno

      me->zif_integracao_cotton_sap~at_retorno-mensagem   = |Tp. Integração { lwa_dados_gerais-tp_integracao } não previsto!|.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.

    ENDIF.

    IF lwa_dados_gerais-id_filial_algodoeira IS INITIAL.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Id. Filial Algodoeira não informado!'.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.


    IF lwa_dados_gerais-safra IS INITIAL.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Safra não informada!'.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.


    IF lwa_dados_gerais-id_movimentacao IS INITIAL.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Id. Movimentação não informado!'.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.

    IF strlen( lwa_dados_gerais-id_movimentacao ) GT 50.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Id. Movimentação com tamanho superior a 50 digitos!'.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.

    CASE lwa_dados_gerais-tp_integracao.
      WHEN '00' OR "Homologação Sessão
           '99'.   "Estorno Sessão

        IF lwa_dados_gerais-id_sessao IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Id. Sessao não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_dados_gerais-dt_inicio_sessao IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Data Inicio Sessao não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_dados_gerais-qtde_fardinhos_sessao IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Quantidade Fardinhos Sessao não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_dados_gerais-dt_envio_homologacao IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Data Envio Homologação não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        lva_peso_aux = lwa_dados_gerais-peso_rolinhos.
        IF lva_peso_aux IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Peso Rolinhos não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        lva_peso_aux = lwa_dados_gerais-peso_pluma.
        IF lva_peso_aux IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Peso Pluma não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        lva_peso_aux = lwa_dados_gerais-peso_caroco.
        IF lva_peso_aux IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Peso Caroco não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        lva_peso_aux = lwa_dados_gerais-peso_fibrilha.
        IF lva_peso_aux IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Peso Fibrilha não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_dados_gerais-versao_producao IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Versao Producao não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_dados_gerais-id_material_sap IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Id Material Sap não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

    ENDCASE.

    IF lwa_dados_gerais-tp_integracao NE '00'. "Não é Homologação Sessão, deve ter detalhamento

      LOOP AT lit_detalhamento INTO DATA(lwa_detalhamento).

        IF lwa_dados_gerais-tp_integracao = '99'. "Se for estorno sessão, só validar os volumes já classificados para processar o estorno
          CHECK lwa_detalhamento-bloco IS NOT INITIAL.
        ENDIF.

        IF lwa_detalhamento-bloco IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Bloco não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_detalhamento-capacidade_bloco IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Capacidade Bloco não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_detalhamento-tamanho_fardo IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Tamanho Fardo Bloco não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_detalhamento-versao_producao IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Versao Producao não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_detalhamento-id_material_sap IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Id Material Sap não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        lva_peso_aux = lwa_detalhamento-peso_bruto_fardos.
        IF lva_peso_aux IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Peso Bruto Fardos não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        lva_peso_aux = lwa_detalhamento-peso_liquido_fardos.
        IF lva_peso_aux IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Peso Liquido Fardos não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_dados_gerais-tp_integracao NE '07'." Na troca, nao é obrigatorio ter fardinhos no bloco origem, pois pode haver troca do bloco inteiro e no bloco origem nao ficar nenhum fardo

          lva_peso_aux = lwa_detalhamento-qtd_fardinhos.
          IF lva_peso_aux IS INITIAL.
            me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Quantidade Fardinhos não informado!'.
            me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
            me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
            RETURN.
          ENDIF.

        ENDIF.

        IF lwa_detalhamento-cd_classificacao IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Cd. Classificação não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        IF lwa_detalhamento-dt_envio_classificacao IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Data Envio Classificação não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        lva_qtde_aux = lwa_detalhamento-qtd_fardinhos.
        lva_peso_aux = lwa_detalhamento-peso_liquido_atual_bloco.
        IF lva_qtde_aux > 0 AND lva_peso_aux IS INITIAL.
          me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Peso Liquido Atual Bloco não informado!'.
          me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
          me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
          RETURN.
        ENDIF.

        CASE lwa_dados_gerais-tp_integracao.
          WHEN '07'. "Troca Bloco/Material
            IF lwa_detalhamento-bloco_destino-bloco IS INITIAL.
              me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Bloco Destino não informado!'.
              me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
              me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
              RETURN.
            ENDIF.

            IF lwa_detalhamento-bloco_destino-cd_classificacao IS INITIAL.
              me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Cd. Classificação Bloco Destino não informado!'.
              me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
              me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
              RETURN.
            ENDIF.

            IF lwa_detalhamento-bloco_destino-capacidade_bloco IS INITIAL.
              me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Capacidade Bloco Destino não informada!'.
              me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
              me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
              RETURN.
            ENDIF.

            IF lwa_detalhamento-bloco_destino-tamanho_fardo IS INITIAL.
              me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Tamanho Fardo Bloco Destino não informada!'.
              me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
              me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
              RETURN.
            ENDIF.

            lva_peso_aux = lwa_detalhamento-bloco_destino-qtd_fardinhos.
            IF lva_peso_aux IS INITIAL.
              me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Quantidade Fardinhos Bloco Destino não informado!'.
              me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
              me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
              RETURN.
            ENDIF.

            lva_qtde_aux = lwa_detalhamento-bloco_destino-qtd_fardinhos.
            lva_peso_aux = lwa_detalhamento-bloco_destino-peso_liquido_atual_bloco.
            IF lva_qtde_aux > 0 AND lva_peso_aux IS INITIAL.
              me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Peso Liquido Atual Bloco Destino não informado!'.
              me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
              me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
              RETURN.
            ENDIF.

        ENDCASE.
      ENDLOOP.

    ENDIF.

    CLEAR: lit_zppt0002_aux[], lit_zppt0006_aux[], lit_zppt0002_wait_aux[].
    SELECT *
      FROM zppt0002 INTO TABLE lit_zppt0002_aux
     WHERE id_mov_sistema_origem EQ lwa_dados_gerais-id_movimentacao.

    SELECT *
      FROM zppt0002_wait INTO TABLE lit_zppt0002_wait_aux
     WHERE id_mov_sistema_origem EQ lwa_dados_gerais-id_movimentacao.

    SELECT *
      FROM zppt0006 INTO TABLE lit_zppt0006_aux
     WHERE id_mov_sistema_origem EQ lwa_dados_gerais-id_movimentacao.

    IF lit_zppt0002_aux[] IS NOT INITIAL OR lit_zppt0006_aux[] IS NOT INITIAL OR lit_zppt0002_wait_aux[] IS NOT INITIAL.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = |Registros já recebidos para o Id. Movimentação { lwa_dados_gerais-id_movimentacao } |.
      READ TABLE lit_zppt0006_aux INTO DATA(lwa_zppt006_aux) INDEX 1.
      IF ( sy-subrc EQ 0 ) AND ( lwa_zppt006_aux-prot_retorno_sistema_origem IS NOT INITIAL ) OR ( lwa_zppt006_aux-flag_envio = 'P' ).
        me->zif_integracao_cotton_sap~at_retorno-mensagem = |{ me->zif_integracao_cotton_sap~at_retorno-mensagem } - Status: Já retornado para o sistema origem! Protocolo: { lwa_zppt006_aux-prot_retorno_sistema_origem }|.
      ELSE.
        me->zif_integracao_cotton_sap~at_retorno-mensagem = |{ me->zif_integracao_cotton_sap~at_retorno-mensagem } - Status: Em processamento... !|.
      ENDIF.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.

    "Tratativas formato Campos
    LOOP AT lit_detalhamento INTO lwa_detalhamento.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_detalhamento-id_material_sap
        IMPORTING
          output = lwa_detalhamento-id_material_sap.
    ENDLOOP.

*-------------------------------------------------------------------------------------------------------------------------*
*   Preparar dados para gravar em tabela
*-------------------------------------------------------------------------------------------------------------------------*

    CASE lwa_dados_gerais-tp_integracao..
      WHEN '00' OR "Homologação Sessão
           '99'.   "Estorno Sessão

        CLEAR: lwa_zppt0002.

        "Informações a Nivel de cabecalho
        me->fill_dados_header_zppt0002(
          EXPORTING
            i_dados_gerais = lwa_dados_gerais
          CHANGING
            c_zppt0002     = lwa_zppt0002 ).

        APPEND lwa_zppt0002 TO lit_zppt0002.

    ENDCASE.

    IF lwa_dados_gerais-tp_integracao NE '00'. "Não é Homologação Sessão, deve ter detalhamento

      LOOP AT lit_detalhamento INTO lwa_detalhamento.

        IF lwa_dados_gerais-tp_integracao = '99'. "Se for estorno sessão, só receber os volumes já classificados para processar o estorno
          CHECK lwa_detalhamento-bloco IS NOT INITIAL.
        ENDIF.

        CLEAR: lwa_zppt0002.

        "Informações a Nivel de cabecalho
        me->fill_dados_header_zppt0002(
          EXPORTING
            i_dados_gerais = lwa_dados_gerais
          CHANGING
            c_zppt0002     = lwa_zppt0002 ).

        "Itens
        CLEAR: lwa_zppt0002-id_sessao, lwa_zppt0002-peso_algodao_caroco, lwa_zppt0002-menge, lwa_zppt0002-peso_caroco, lwa_zppt0002-peso_fibrilha, lwa_zppt0002-verid, lwa_zppt0002-matnr.

        lwa_zppt0002-lgort                           = lwa_detalhamento-bloco.
        lwa_zppt0002-qtd_bloco                       = lwa_detalhamento-capacidade_bloco.
        lwa_zppt0002-tipo_fardo                      = lwa_detalhamento-tamanho_fardo.
        lwa_zppt0002-peso_liq_atual_bloco            = lwa_detalhamento-peso_liquido_atual_bloco.
        lwa_zppt0002-verid                           = lwa_detalhamento-versao_producao.
        lwa_zppt0002-matnr                           = lwa_detalhamento-id_material_sap.
        lwa_zppt0002-peso_bruto                      = lwa_detalhamento-peso_bruto_fardos.
        lwa_zppt0002-peso_liquido                    = lwa_detalhamento-peso_liquido_fardos.
        lwa_zppt0002-menge                           = lwa_detalhamento-peso_liquido_fardos.
        lwa_zppt0002-qtd_fardinhos_bloco             = lwa_detalhamento-qtd_fardinhos.
        lwa_zppt0002-cd_classificacao                = lwa_detalhamento-cd_classificacao.
        lwa_zppt0002-dt_classificacao                = lwa_detalhamento-dt_envio_classificacao+6(4) && lwa_detalhamento-dt_envio_classificacao+3(2) && lwa_detalhamento-dt_envio_classificacao(2).

        "Dados Bloco Destino
        lwa_zppt0002-bloco_destino                   = lwa_detalhamento-bloco_destino-bloco.
        lwa_zppt0002-capacidade_bloco_destino        = lwa_detalhamento-bloco_destino-capacidade_bloco.
        lwa_zppt0002-tipo_fardo_bloco_destino        = lwa_detalhamento-bloco_destino-tamanho_fardo.
        lwa_zppt0002-peso_liq_atual_bloco_destino    = lwa_detalhamento-bloco_destino-peso_liquido_atual_bloco.
        lwa_zppt0002-cd_classificacao_bloco_destino  = lwa_detalhamento-bloco_destino-cd_classificacao.
        lwa_zppt0002-qtde_fardinhos_bloco_destino    = lwa_detalhamento-bloco_destino-qtd_fardinhos.

        APPEND lwa_zppt0002 TO lit_zppt0002.
      ENDLOOP.

    ENDIF.

    LOOP AT lit_zppt0002 ASSIGNING FIELD-SYMBOL(<fs_zppt0002>).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fs_zppt0002>-matnr
        IMPORTING
          output = <fs_zppt0002>-matnr.

      DATA(_id_gerado) = me->set_id_movimentacao_interno( EXPORTING i_id_mov_sistema_origem = CONV #( lwa_dados_gerais-id_movimentacao )
                                                          CHANGING  c_zppt0002              = <fs_zppt0002> ).

      IF _id_gerado EQ abap_false OR <fs_zppt0002>-id_mov_sistema_origem_ref_int IS INITIAL.
        me->zif_integracao_cotton_sap~at_retorno-mensagem   = 'Houve um erro ao gerar o Id. Movimentação interno!'.
        me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
        me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
        RETURN.
      ENDIF.

    ENDLOOP.

*-------------------------------------------------------------------------------------------------------------------------*
*   Gravar dados em tabela
*-------------------------------------------------------------------------------------------------------------------------*

    LOOP AT lit_zppt0002 INTO lwa_zppt0002.

*-------------------------------------------------------------------------------------------------------------------------*
*     Verificar se registro esta em processamento
*-------------------------------------------------------------------------------------------------------------------------*

      CASE lwa_dados_gerais-tp_integracao.
        WHEN '03'. "Classificação não checa se já tem registro em processamento. Recebe e já coloca na fila

          me->armazenar_fila_processamento( i_zppt0002 = lwa_zppt0002 ).

          IF sy-subrc NE 0.
            ROLLBACK WORK.
            me->zif_integracao_cotton_sap~at_retorno-mensagem   = |Houve um erro ao gravar os dados de movimentação gerada. ZPPT0002 Id: { lwa_zppt0002-id_mov_sistema_origem_ref_int }!|.
            me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
            me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
            RETURN.
          ENDIF.

        WHEN OTHERS.

          DATA(_zppt0002_processing) = me->check_registro_processamento( lwa_zppt0002 ).

          IF _zppt0002_processing IS NOT INITIAL.

            ROLLBACK WORK.

            IF _zppt0002_processing-id_sessao IS NOT INITIAL.
              me->zif_integracao_cotton_sap~at_retorno-mensagem   = | Sessão: { _zppt0002_processing-id_sessao } esta sendo processada no Id. Mov.: { _zppt0002_processing-id_mov_sistema_origem } - Protocolo: { _zppt0002_processing-id_referencia } !|.
            ELSE.
              me->zif_integracao_cotton_sap~at_retorno-mensagem   = | Bloco: { _zppt0002_processing-lgort } esta sendo processado no Id. Mov.: { _zppt0002_processing-id_mov_sistema_origem } - Protocolo: { _zppt0002_processing-id_referencia } !|.
            ENDIF.

            me->zif_integracao_cotton_sap~at_retorno-mensagem = |{ me->zif_integracao_cotton_sap~at_retorno-mensagem } Aguardar retorno desse processamento! |.

            me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
            me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
            RETURN.

          ENDIF.

          me->armazenar_fila_processamento( i_zppt0002 = lwa_zppt0002 ).

          IF sy-subrc NE 0.
            ROLLBACK WORK.
            me->zif_integracao_cotton_sap~at_retorno-mensagem   = |Houve um erro ao gravar os dados de movimentação gerada. ZPPT0002 Id: { lwa_zppt0002-id_mov_sistema_origem_ref_int }!|.
            me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
            me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
            RETURN.
          ENDIF.

      ENDCASE.

    ENDLOOP.

    COMMIT WORK.

    me->zif_integracao_cotton_sap~at_retorno-mensagem     = 'Solicitação Recebida com sucesso!'.
    me->zif_integracao_cotton_sap~at_retorno-protocolo    = me->zif_integracao_cotton_sap~at_protocolo_recebimento.
    me->zif_integracao_cotton_sap~at_retorno-statuscode   = '200'.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'BENEFICIAMENTO_ALG_REQUEST'.
    me->zif_integracao_inject~at_referencia-id_referencia = me->zif_integracao_cotton_sap~at_protocolo_recebimento.

    RETURN.


*    DATA: w_0002         TYPE zppt0002,
*          t_0002         TYPE TABLE OF zppt0002,
*          v_material_sap TYPE zppet016-id_material_sap,
*          t_cotton       TYPE zppt016,
*          id_cotton      TYPE zppt0002-id_cotton,
*          in_fardos      TYPE zrsdsselopts,
*          w_0007         TYPE zppt0007,
*          seq            TYPE numc11,
*          algodoeira     TYPE char2,
*          numc           TYPE int4.
*
*    CLEAR algodoeira.
*
*    r_if_integracao_cotton_sap = me.
*
*    t_cotton[] = me->zif_integracao_cotton_sap~at_zppt0002[].
*
*    LOOP AT me->zif_integracao_cotton_sap~at_zppt0002 INTO DATA(_0002).
*
*      DATA(qtd_fardos) = REDUCE int4( INIT x = 0 FOR ls IN t_cotton WHERE ( nr_fardo_origem = _0002-nr_fardo_origem ) NEXT x = x + 1 ).
*
*      IF _0002-rg_atualizado EQ 0.
*
*        w_0002-acharg           = _0002-nr_fardo.
*        w_0002-werks            = _0002-id_filial_sap.
*        w_0002-verid            = _0002-nr_maquina.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = _0002-id_material_sap
*          IMPORTING
*            output = v_material_sap.
*
*        w_0002-matnr = v_material_sap.
*        w_0002-menge            = _0002-quantidade.
*        w_0002-id_cotton        = _0002-nr_fardo_origem.
*        w_0002-budat            = |{ _0002-dt_lancamento+6(4) }{ _0002-dt_lancamento+3(2) }{ _0002-dt_lancamento(2) }|.
*        w_0002-bldat            = |{ _0002-dt_documento+6(4)  }{ _0002-dt_documento+3(2)  }{ _0002-dt_documento(2)  }|.
*        w_0002-dt_fabricacao    = |{ _0002-dt_fabricacao+6(4) }{ _0002-dt_fabricacao+3(2) }{ _0002-dt_fabricacao(2) }|.
*        w_0002-hr_fabricacao    = _0002-hr_fabricacao.
*        w_0002-status           = _0002-status = 'R'.
*        w_0002-usnam            = _0002-usuario.
*        w_0002-cd_safra         = _0002-safra.
*        w_0002-id_fardinho      = _0002-id_fardinho.
*        w_0002-cd_sai           = _0002-cd_sai.
*        w_0002-peso_bruto       = _0002-peso_bruto.
*        w_0002-peso_liquido     = _0002-peso_liquido.
*        w_0002-id_fardao        = _0002-id_fardao.
*        w_0002-cd_classificacao = _0002-cd_classificacao.
*        w_0002-lgort            = _0002-deposito.
*        w_0002-mblnr            = _0002-doc_material_fardinho.
*        w_0002-mblnr02          = _0002-doc_material_fardao.
*        w_0002-ernam            = _0002-nome_responsavel.
*        w_0002-status_registro  = COND #( WHEN _0002-status_registro EQ '00' THEN '01' ELSE _0002-status_registro ).
*        w_0002-qtd_fardinhos    = _0002-qtd_fardinhos.
*        w_0002-dt_classificacao = |{ _0002-dt_classificacao+6(4) }{ _0002-dt_classificacao+3(2) }{ _0002-dt_classificacao(2) }|.
*        w_0002-peso_caroco      = _0002-peso_caroco.
*        w_0002-peso_fibrilha    = _0002-peso_fibrilha.
*        w_0002-cd_mensagem      = _0002-cd_mensagem.
*        w_0002-qtd_bloco        = _0002-qtd_bloco.
*        w_0002-laeda            = sy-datum.
*        w_0002-laehr            = sy-uzeit.
*        w_0002-id_referencia    = me->zif_integracao_cotton_sap~at_recebido.
*
*        IF algodoeira IS INITIAL.
*          algodoeira = _0002-nr_fardo_origem(2).
*        ENDIF.
*
*        SELECT SINGLE *
*          FROM zpps_ximfbf_log
*          INTO @DATA(w_ximfbf)
*         WHERE id_cotton  = @w_0002-id_cotton
*           AND processado = 'S'.
*
*        IF ( sy-subrc <> 0 ) OR ( sy-subrc = 0 AND w_ximfbf-charg IS INITIAL ).
*          SELECT SINGLE *
*            FROM zmmt0006 AS m1
*            INTO @DATA(w_0006)
*           WHERE id_cotton     = @w_0002-id_cotton
*             AND ch_referencia = ( SELECT MAX( m2~ch_referencia ) FROM zmmt0006 AS m2 WHERE m2~id_cotton = m1~id_cotton ).
*
*          IF ( sy-subrc <> 0 ) OR ( sy-subrc = 0 AND w_0006-batch_d IS INITIAL ).
*            SELECT SINGLE *
*              FROM mseg
*              INTO @DATA(w_mseg)
*             WHERE xblnr_mkpf EQ @w_0002-id_cotton
*               AND smbln      EQ @abap_off
*               AND xauto      EQ @abap_true.
*
*            IF ( sy-subrc <> 0 ) OR ( sy-subrc = 0 AND w_mseg-charg IS INITIAL ).
*              _0002-cd_mensagem = |Não existe entrada de prod. p/ o rolo { w_0002-id_cotton }|.
*              APPEND _0002 TO me->zif_integracao_cotton_sap~at_retorno[].
*
*              MOVE-CORRESPONDING w_0002 TO w_0007.
*              w_0007-cd_mensagem      = _0002-cd_mensagem.
*              w_0007-seq_recebido     = me->zif_integracao_cotton_sap~at_recebido.
*
*              me->add_zppt0007( w_0007 ).
*
*              CLEAR: w_0007, w_0002.
*              CONTINUE.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*        IF qtd_fardos  NE _0002-qtd_fardinhos
*           AND ( _0002-status_registro EQ '00'
*              OR _0002-status_registro EQ '01'
*               ).
*
*          _0002-cd_mensagem = |Qtd de fardinhos { qtd_fardos } <> { _0002-qtd_fardinhos }|.
*          APPEND _0002 TO me->zif_integracao_cotton_sap~at_retorno[].
*
*          MOVE-CORRESPONDING w_0002 TO w_0007.
*          w_0007-cd_mensagem      = _0002-cd_mensagem.
*          w_0007-seq_recebido     = me->zif_integracao_cotton_sap~at_recebido.
*
*          me->add_zppt0007( w_0007 ).
*
*          CLEAR: w_0007, w_0002.
*
*          CONTINUE.
*        ENDIF.
*
*        APPEND w_0002 TO t_0002.
*        APPEND _0002 TO me->zif_integracao_cotton_sap~at_retorno[].
*
*        MOVE-CORRESPONDING w_0002 TO w_0007.
*        w_0007-seq_recebido     = me->zif_integracao_cotton_sap~at_recebido.
*
*        CLEAR w_0002.
*
*        me->add_zppt0007( w_0007 ).
*
*      ENDIF.
*
*    ENDLOOP.
*
*    SORT t_0002 BY charg acharg.
*    FREE in_fardos.
*
*    LOOP AT t_0002 INTO w_0002.
*
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = w_0002-id_cotton ) TO in_fardos.
*
*      numc = 0.
*
*      SELECT COUNT(*)
*        FROM zppt0002
*        INTO numc
*        WHERE acharg EQ w_0002-acharg
*          AND werks  EQ w_0002-werks.
*
*      IF numc EQ 0.
**       "// Processo Novo
*
*        MODIFY zppt0002 FROM w_0002.
*
*      ELSEIF w_0002-status_registro EQ '03'.
*
**       "// Classificação da Pluma
*        UPDATE zppt0002
*           SET status = 'R',
*               status_registro  = @w_0002-status_registro,
*               cd_classificacao = @w_0002-cd_classificacao,
*               lgort            = @w_0002-lgort,
*               dt_classificacao = @w_0002-dt_classificacao,
*               laeda            = @sy-datum,
*               laehr            = @sy-uzeit,
*               id_referencia    = @me->zif_integracao_cotton_sap~at_recebido
*        WHERE acharg EQ @w_0002-acharg
*          AND werks  EQ @w_0002-werks.
*
*      ELSEIF _0002-status_registro EQ '01' OR _0002-status_registro EQ '00'.
*
*
**       "//Inicio de Processo
*        UPDATE zppt0002
*           SET status           = 'R',
*               status_registro  = @w_0002-status_registro,
*               budat            = @w_0002-budat,
*               verid            = @w_0002-verid,
*               matnr            = @w_0002-matnr,
*               menge            = @w_0002-menge,
*               charg            = @w_0002-charg,
*               bldat            = @w_0002-bldat,
*               dt_fabricacao    = @w_0002-dt_fabricacao,
*               hr_fabricacao    = @w_0002-hr_fabricacao,
*               cd_safra         = @w_0002-cd_safra,
*               id_fardinho      = @w_0002-id_fardinho,
*               cd_sai           = @w_0002-cd_sai,
*               peso_bruto       = @w_0002-peso_bruto,
*               peso_liquido     = @w_0002-peso_liquido,
*               id_fardao        = @w_0002-id_fardao,
*               qtd_fardinhos    = @w_0002-qtd_fardinhos,
*               peso_caroco      = @w_0002-peso_caroco,
*               peso_fibrilha    = @w_0002-peso_fibrilha,
*               id_cotton        = @w_0002-id_cotton,
*               laeda            = @sy-datum,
*               laehr            = @sy-uzeit,
*               id_referencia    = @me->zif_integracao_cotton_sap~at_recebido
*         WHERE acharg EQ @w_0002-acharg
*           AND werks  EQ @w_0002-werks.
*
*      ELSEIF _0002-status_registro EQ '07'.
**       "//Troca de Deposito
*
*        UPDATE zppt0002
*           SET status           = 'R',
*               status_registro  = @w_0002-status_registro,
*               lgort            = @w_0002-lgort,
*               laeda            = @sy-datum,
*               laehr            = @sy-uzeit,
*               id_referencia    = @me->zif_integracao_cotton_sap~at_recebido
*         WHERE acharg EQ @w_0002-acharg
*           AND werks  EQ @w_0002-werks.
*
*      ELSEIF _0002-status_registro EQ '09'.
**       "//Troca de Material e Deposito
*
*        IF w_0002-lgort IS INITIAL.
*          UPDATE zppt0002
*             SET status           = 'R',
*                 status_registro  = @w_0002-status_registro,
*                 matnr            = @w_0002-matnr,
*                 cd_classificacao = @w_0002-cd_classificacao,
*                 laeda            = @sy-datum,
*                 laehr            = @sy-uzeit,
*                 id_referencia    = @me->zif_integracao_cotton_sap~at_recebido
*           WHERE acharg EQ @w_0002-acharg
*             AND werks  EQ @w_0002-werks.
*        ELSE.
*          UPDATE zppt0002
*             SET status           = 'R',
*                 status_registro  = @w_0002-status_registro,
*                 lgort            = @w_0002-lgort,
*                 matnr            = @w_0002-matnr,
*                 cd_classificacao = @w_0002-cd_classificacao,
*                 laeda            = @sy-datum,
*                 laehr            = @sy-uzeit,
*                 id_referencia    = @me->zif_integracao_cotton_sap~at_recebido
*         WHERE acharg EQ @w_0002-acharg
*             AND werks  EQ @w_0002-werks.
*        ENDIF.
*
*      ENDIF.
*    ENDLOOP.
*
*    COMMIT WORK.
*
**-------------------------
**-- FArdos a serem processados
**-------------------------
*    me->zif_integracao_cotton_sap~at_proc_zppt0002_t[] = t_0002[].
*
*    CALL METHOD me->job_envio
*      EXPORTING
*        id_referencia = me->zif_integracao_cotton_sap~at_recebido   " Caractere comprimento 11
*        s_cotton      = in_fardos.  " Tabela de OPÇÔES-SELECIONAR

    "Projeto Restruturação Algodao 2024 - WPP - Fim

  ENDMETHOD.


  METHOD zif_integracao_cotton_sap~set_send_msg.

    DATA: i_inbound TYPE zppt0002_t.
    DATA: lc_integracao TYPE REF TO zcl_integracao.

    r_if_integracao_cotton_sap = me.

    CREATE OBJECT lc_integracao.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = me->zif_integracao_cotton_sap~at_retorno
        pretty_name = abap_true
      RECEIVING
        r_json      = e_msg.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_cotton_sap~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno(
           IMPORTING
             e_data_retorno =  DATA(e_data_retorno)
             e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    e_integracao-ds_data_retorno = e_msg.
    MODIFY zintegracao FROM e_integracao.
    COMMIT WORK.

    e_protocolo = zcl_string=>lpad( i_str  = CONV #( e_integracao-id_integracao ) i_qtd  = 15 i_char = '0'  ).

    CLEAR: lc_integracao.




  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_FALSE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    E_SUCESSO = ABAP_TRUE.

    R_IF_INTEGRACAO_INJECT = ME.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    "Metodo para Integrar InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    E_SUCESSO = ABAP_TRUE.

    R_IF_INTEGRACAO_INJECT = ME.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    "Metodo para processoamento de InBound

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ARMAZENAR_FILA_PROCESSAMENTO.

    DATA: lwa_zppt0002_wait TYPE zppt0002_wait.

    CLEAR: lwa_zppt0002_wait.

    MOVE-CORRESPONDING I_ZPPT0002 TO lwa_zppt0002_wait.

    lwa_zppt0002_wait-status_processamento      = 'P'. "Processamento Pendente
    lwa_zppt0002_wait-status_ret_sistema_origem = 'P'. "Retorno Pendente

    GET TIME STAMP FIELD lwa_zppt0002_wait-timestampl.

    MODIFY zppt0002_wait FROM lwa_zppt0002_wait.

  endmethod.


  METHOD check_mov_and_force_retorno.

    DATA: lva_id_log   TYPE zppt0006-id.

    CLEAR: e_mov_marcada_retorno, e_error.

    CHECK i_zppt0002 IS NOT INITIAL.

    "Se Movimentação já foi gerada no SAP, somente marcar o registro para retornar novamente para o sistema Origem.
    SELECT SINGLE *
      FROM zppt0002 INTO @DATA(lwa_zppt0002_mov_exists)
     WHERE id_mov_sistema_origem_ref_int EQ @i_zppt0002-id_mov_sistema_origem_ref_int.

    CHECK ( sy-subrc EQ 0 ) AND ( lwa_zppt0002_mov_exists-status_processamento EQ 'C' ). "Processamento Concluido

    SELECT SINGLE *
      FROM zppt0006 INTO @DATA(lwa_zppt0006_mov_exists)
     WHERE id_mov_sistema_origem_ref_int EQ @i_zppt0002-id_mov_sistema_origem_ref_int.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      e_error = abap_true.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = |Houve um erro ao localizar os dados de movimentação gerada. Id: { i_zppt0002-id_mov_sistema_origem_ref_int }!|.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.

    lwa_zppt0002_mov_exists-id_referencia                   = i_zppt0002-id_referencia.
    lwa_zppt0002_mov_exists-status_ret_sistema_origem       = 'P'. "Retorno Pendente
    lwa_zppt0002_mov_exists-cd_mensagem                     = | { lwa_zppt0002_mov_exists-cd_mensagem } - [ Processado anteriormente ]|.
    MODIFY zppt0002 FROM lwa_zppt0002_mov_exists.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      e_error = abap_true.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = |Houve um erro ao gravar os dados de movimentação gerada. ZPPT0002 Id: { i_zppt0002-id_mov_sistema_origem_ref_int }!|.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.

    CLEAR: lva_id_log.
    SELECT MAX( id ) INTO lva_id_log
      FROM zppt0006
     WHERE werks      = lwa_zppt0006_mov_exists-werks
       AND charg      = lwa_zppt0006_mov_exists-charg
       AND acharg     = lwa_zppt0006_mov_exists-acharg
       AND id_sessao  = lwa_zppt0006_mov_exists-id_sessao
       AND lgort      = lwa_zppt0006_mov_exists-lgort.

    ADD 1 TO lva_id_log.

    lwa_zppt0006_mov_exists-id             = lva_id_log.
    lwa_zppt0006_mov_exists-cd_mensagem    =  | { lwa_zppt0002_mov_exists-cd_mensagem } - [ Processado anteriormente ]|.
    lwa_zppt0006_mov_exists-flag_envio     = 'R'.
    lwa_zppt0006_mov_exists-id_referencia2 = i_zppt0002-id_referencia.
    MODIFY zppt0006 FROM lwa_zppt0006_mov_exists.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      e_error = abap_true.
      me->zif_integracao_cotton_sap~at_retorno-mensagem   = |Houve um erro ao gravar os dados de movimentação gerada. ZPPT0006 Id: { i_zppt0002-id_mov_sistema_origem_ref_int }!|.
      me->zif_integracao_cotton_sap~at_retorno-protocolo  = ''.
      me->zif_integracao_cotton_sap~at_retorno-statuscode = '400'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD check_registro_processamento.

    DATA: lwa_zppt0002_process  TYPE zppt0002.

    DATA(_process_new) = abap_false.

    CLEAR: r_zppt0002_processing.

    SELECT SINGLE *
      FROM zppt0002_wait INTO CORRESPONDING FIELDS OF lwa_zppt0002_process
     WHERE acharg                 = i_zppt0002-acharg
       AND werks                  = i_zppt0002-werks
       AND id_sessao              = i_zppt0002-id_sessao
       AND lgort                  = i_zppt0002-lgort
       AND liberado_processamento = abap_false.

    IF sy-subrc EQ 0.
      r_zppt0002_processing = lwa_zppt0002_process.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zppt0002 INTO CORRESPONDING FIELDS OF lwa_zppt0002_process
     WHERE acharg                 = i_zppt0002-acharg
       AND werks                  = i_zppt0002-werks
       AND id_sessao              = i_zppt0002-id_sessao
       AND lgort                  = i_zppt0002-lgort.

    IF sy-subrc EQ 0 AND lwa_zppt0002_process-status_ret_sistema_origem NE 'C'. "Pendente de retorno sistema Origem.
      r_zppt0002_processing = lwa_zppt0002_process.
      RETURN.
    ENDIF.

    IF i_zppt0002-bloco_destino IS NOT INITIAL.  "Checar processamento em andamento para o bloco destino

      SELECT SINGLE *
        FROM zppt0002_wait INTO CORRESPONDING FIELDS OF lwa_zppt0002_process
       WHERE acharg                 = i_zppt0002-acharg
         AND werks                  = i_zppt0002-werks
         AND id_sessao              = i_zppt0002-id_sessao
         AND lgort                  = i_zppt0002-bloco_destino
         AND liberado_processamento = abap_false.

      IF sy-subrc EQ 0.
        r_zppt0002_processing = lwa_zppt0002_process.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zppt0002 INTO CORRESPONDING FIELDS OF lwa_zppt0002_process
       WHERE acharg                 = i_zppt0002-acharg
         AND werks                  = i_zppt0002-werks
         AND id_sessao              = i_zppt0002-id_sessao
         AND lgort                  = i_zppt0002-bloco_destino.

      IF sy-subrc EQ 0 AND lwa_zppt0002_process-status_ret_sistema_origem NE 'C'. "Pendente de retorno sistema Origem.
        r_zppt0002_processing = lwa_zppt0002_process.
        RETURN.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD fill_dados_header_zppt0002.

    c_zppt0002-id_referencia          = me->zif_integracao_cotton_sap~at_protocolo_recebimento.
    c_zppt0002-id_sessao              = i_dados_gerais-id_sessao.  "Id. Sessão
    c_zppt0002-status_registro        = COND #( WHEN i_dados_gerais-tp_integracao EQ '00' THEN '01' ELSE i_dados_gerais-tp_integracao ).
    c_zppt0002-werks                  = i_dados_gerais-id_filial_algodoeira.

    IF i_dados_gerais-dt_inicio_sessao IS NOT INITIAL.
      c_zppt0002-bldat                  = i_dados_gerais-dt_inicio_sessao+6(4) && i_dados_gerais-dt_inicio_sessao+3(2) && i_dados_gerais-dt_inicio_sessao(2) .
    ENDIF.

    IF i_dados_gerais-dt_envio_homologacao IS NOT INITIAL.
      c_zppt0002-budat                  = i_dados_gerais-dt_envio_homologacao+6(4) && i_dados_gerais-dt_envio_homologacao+3(2) && i_dados_gerais-dt_envio_homologacao(2).
    ENDIF.

    c_zppt0002-qtd_fardinhos_sessao   = i_dados_gerais-qtde_fardinhos_sessao.
    c_zppt0002-cd_safra               = i_dados_gerais-safra.
    c_zppt0002-peso_algodao_caroco    = i_dados_gerais-peso_rolinhos.  "Peso Algodao Caroco
    c_zppt0002-menge                  = i_dados_gerais-peso_pluma.     "Peso Algodao em Pluma
    c_zppt0002-peso_caroco            = i_dados_gerais-peso_caroco.    "Peso Caroco
    c_zppt0002-peso_fibrilha          = i_dados_gerais-peso_fibrilha. "Peso Fibrilha
    c_zppt0002-id_mov_sistema_origem  = i_dados_gerais-id_movimentacao.
    c_zppt0002-charg                  = i_dados_gerais-safra && '_' && i_dados_gerais-id_filial_algodoeira.
    c_zppt0002-verid                  = i_dados_gerais-versao_producao.
    c_zppt0002-matnr                  = i_dados_gerais-id_material_sap.
    c_zppt0002-status                 = 'R'.
    c_zppt0002-laeda                  = sy-datum.
    c_zppt0002-laehr                  = sy-uzeit.


  ENDMETHOD.


  METHOD set_id_movimentacao_interno.

    DATA: lva_id_mov_interno TYPE zppt0039-id_movimento_interno.

    CLEAR: r_id_gerado.

    CHECK i_id_mov_sistema_origem IS NOT INITIAL.

    CHECK strlen( i_id_mov_sistema_origem ) LE 50.

    SELECT SINGLE *
      FROM zppt0039 INTO @DATA(lwa_zppt0039)
     WHERE id_mov_sistema_origem EQ @i_id_mov_sistema_origem.

    IF ( sy-subrc NE 0 ) OR ( lwa_zppt0039-id_movimento_interno IS INITIAL ).

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZPP0001'
        IMPORTING
          number                  = lva_id_mov_interno
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      CHECK sy-subrc EQ 0 AND lva_id_mov_interno IS NOT INITIAL.

      CLEAR: lwa_zppt0039.
      lwa_zppt0039-id_mov_sistema_origem    = i_id_mov_sistema_origem.
      lwa_zppt0039-id_movimento_interno     = lva_id_mov_interno.
      lwa_zppt0039-dt_registro              = sy-datum.
      lwa_zppt0039-hr_registro              = sy-uzeit.

      INSERT zppt0039 FROM lwa_zppt0039.
      CHECK sy-subrc EQ 0.
      COMMIT WORK.

    ENDIF.

    SELECT SINGLE *
      FROM zppt0039 INTO @DATA(lwa_zppt0039_check)
     WHERE id_mov_sistema_origem EQ @i_id_mov_sistema_origem.

    CHECK ( sy-subrc EQ 0 ) AND ( lwa_zppt0039_check-id_movimento_interno IS NOT INITIAL ).

    IF c_zppt0002-id_sessao IS NOT INITIAL.
      c_zppt0002-id_mov_sistema_origem_ref_int =  'CT' && lwa_zppt0039_check-id_movimento_interno && 'SESSAO'.
    ELSE.
      c_zppt0002-id_mov_sistema_origem_ref_int =  'CT' && lwa_zppt0039_check-id_movimento_interno && 'BL' && c_zppt0002-lgort.
    ENDIF.

    r_id_gerado = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_COTTON_SAP~CONFIGURE_SERVER.

    DATA: lva_reason TYPE string,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_cotton_sap~at_retorno-statuscode IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_integracao_cotton_sap~at_retorno-statuscode
        IMPORTING
          output = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code        = lva_code
        IMPORTING
          e_desc_status = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = CONV #( lva_code )
          reason = CONV #( lva_reason )
       ).

    ENDIF.

  endmethod.
ENDCLASS.
