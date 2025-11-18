class ZCL_INTEGRACAO_INJECT definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .

  class-data AT_INTEGRACAO_INJECT type ref to ZCL_INTEGRACAO_INJECT .

  class-methods GET_INSTANCE
    returning
      value(R_INTEGRACAO_INJECT) type ref to ZCL_INTEGRACAO_INJECT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INTEGRACAO_INJECT IMPLEMENTATION.


  METHOD GET_INSTANCE.

    IF ZCL_INTEGRACAO_INJECT=>AT_INTEGRACAO_INJECT IS NOT BOUND.
      CREATE OBJECT ZCL_INTEGRACAO_INJECT=>AT_INTEGRACAO_INJECT.
    ENDIF.
    R_INTEGRACAO_INJECT = ZCL_INTEGRACAO_INJECT=>AT_INTEGRACAO_INJECT.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    E_SUCESSO = ABAP_FALSE.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    E_SUCESSO = ABAP_FALSE.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    E_SUCESSO = ABAP_FALSE.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    E_SUCESSO = ABAP_FALSE.
    R_IF_INTEGRACAO_INJECT = ME.
  ENDMETHOD.
ENDCLASS.
