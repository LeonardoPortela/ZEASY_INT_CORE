FUNCTION z_conv_timestamp_mil_to_date.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATE) TYPE  TIMESTAMPL
*"  EXPORTING
*"     VALUE(E_DATE) TYPE  SYST_DATUM
*"     VALUE(E_TIME) TYPE  SYST_UZEIT
*"----------------------------------------------------------------------

  DATA:
    lva_date           TYPE sy-datum,
    lva_days_i         TYPE i,
    lva_timestamp      TYPE timestampl,
    lva_date_i         TYPE sy-datum,
    lva_time_i         TYPE sy-uzeit,
    lva_seconds        TYPE p,
    date               TYPE sy-datum,
    hora               TYPE      sy-uzeit,
    id_timestmp        TYPE rke_hzstmp,
    cd_timestmp_output TYPE tspas4,
    i_tzone            TYPE sy-tzone,

    lc_day_in_sec      TYPE i.

  lc_day_in_sec = 86400.
  IF i_date IS NOT INITIAL.

    CLEAR: lva_date,
           lva_days_i,
           lva_timestamp,
           lva_seconds,
           lva_date_i.

    "Data inicio.
    i_tzone = sy-tzone.
    lva_seconds = 1000.
    data(vg_tzone) = ( i_tzone + 3600 ).

    lva_timestamp = ( i_date / lva_seconds ) + vg_tzone.   "timestamp in seconds

* One day has 86400 seconds: Timestamp in days

    lva_days_i    = lva_timestamp DIV lc_day_in_sec.
    lva_date      = '19700101'.
    lva_date_i    = lva_date + lva_days_i.
    date          = lva_date_i.
    e_date        =  date.

    "Hora inicio.
    e_time    = ( lva_timestamp MOD lc_day_in_sec ).
  ENDIF.
ENDFUNCTION.
