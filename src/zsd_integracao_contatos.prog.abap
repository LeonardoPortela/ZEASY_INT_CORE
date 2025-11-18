*&---------------------------------------------------------------------*
*& Include ZSD_INTEGRACAO_CONTATOS
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_parceiro,
         parceiro TYPE bu_partner.
TYPES: END   OF ty_parceiro.

DATA: t_customer TYPE TABLE OF ibupacustomer,
      t_supplier TYPE TABLE OF ibpsupplier,
      t_parceiro TYPE TABLE OF ty_parceiro,
      w_parceiro TYPE ty_parceiro.

FREE: t_parceiro.

SELECT *
  INTO TABLE t_customer
  FROM ibupacustomer
 WHERE businesspartner = bus_joel_main-change_number.

SELECT *
  INTO TABLE t_supplier
  FROM ibpsupplier
 WHERE businesspartner = bus_joel_main-change_number.

LOOP AT t_customer INTO DATA(_customer).
  DATA(l_task_cust) = 'INTEGRAR_CUSTOMER' && _customer-customer.

  CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS' STARTING NEW TASK l_task_cust
    EXPORTING
      i_parceiro         = _customer-customer
      i_outros_enderecos = abap_false
    EXCEPTIONS
      erro_integracao    = 1
      OTHERS             = 2.

  w_parceiro-parceiro = _customer-customer.
  APPEND w_parceiro  TO t_parceiro.
ENDLOOP.

LOOP AT t_supplier INTO DATA(_supplier).

  READ TABLE t_parceiro INTO w_parceiro WITH KEY parceiro = _supplier-supplier.
  CHECK sy-subrc <> 0.

  DATA(l_task_supl) = 'INTEGRAR_SUPPLIER' && _supplier-supplier.

  CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS' STARTING NEW TASK l_task_supl
    EXPORTING
      i_parceiro         = _supplier-supplier
      i_outros_enderecos = abap_false
    EXCEPTIONS
      erro_integracao    = 1
      OTHERS             = 2.
ENDLOOP.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
