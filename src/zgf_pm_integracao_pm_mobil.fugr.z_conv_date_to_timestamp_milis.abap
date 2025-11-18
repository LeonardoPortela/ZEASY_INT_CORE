FUNCTION Z_CONV_DATE_TO_TIMESTAMP_MILIS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATE) TYPE  SYST_DATUM OPTIONAL
*"     REFERENCE(I_TIME) TYPE  SYST_UZEIT OPTIONAL
*"  EXPORTING
*"     VALUE(E_DATE) TYPE  TIMESTAMPL
*"----------------------------------------------------------------------

  CONSTANTS: lc_day_in_sec TYPE i VALUE 86400.

  DATA: days TYPE i.
  DATA: utc_time TYPE i.
  DATA: seconds_per_day TYPE i.
  DATA: days_bevor_utc_start TYPE i.
  DATA: utc_start TYPE d VALUE '19700101'.
  DATA: uhrzeit       LIKE  sy-uzeit.

  IF i_date IS NOT INITIAL.

    GET TIME.
    days = i_date.
*    uhrzeit = i_time - sy-tzone.
    uhrzeit = i_time.

    days_bevor_utc_start = utc_start.
    SUBTRACT days_bevor_utc_start FROM days.
    utc_time = days * lc_day_in_sec + uhrzeit.
    e_date = utc_time * 1000.
  ENDIF.
ENDFUNCTION.
