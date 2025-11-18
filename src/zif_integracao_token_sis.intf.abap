INTERFACE zif_integracao_token_sis
  PUBLIC .


  CLASS-DATA at_if_integracao_token TYPE REF TO zif_integracao_token_sis.

  CLASS-METHODS get_instance
    RETURNING
      VALUE(r_if_integracao_token) TYPE REF TO zif_integracao_token_sis
    RAISING
      zcx_integracao
      zcx_error .
  METHODS get_token
    RETURNING
      VALUE(r_if_integracao_token) TYPE REF TO zif_integracao_token_sis
    RAISING
      zcx_integracao
      zcx_error .
  METHODS set_ds_url
    RETURNING
      VALUE(r_if_integracao_token) TYPE REF TO zif_integracao_token_sis .
  METHODS set_send_msg
    EXPORTING
      !e_access_token              TYPE string
      !e_token_type                TYPE string
      !e_expires_in                TYPE string
    RETURNING
      VALUE(r_if_integracao_token) TYPE REF TO zif_integracao_token_sis
    RAISING
      zcx_integracao
      zcx_error .
ENDINTERFACE.
