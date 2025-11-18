FUNCTION ZPM_EXPORT_M_VEIC_TO_SAAF .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DATA_DE) TYPE  CHAR16
*"     REFERENCE(DATA_ATE) TYPE  CHAR16
*"     REFERENCE(STATUS) TYPE  C
*"     REFERENCE(FILIAL) TYPE  IWERK
*"  TABLES
*"      RESULT STRUCTURE  ZTPM_M_VEIC_SAAF
*"  EXCEPTIONS
*"      UNKNOWN_ERROR
*"----------------------------------------------------------------------
*&                 AMAGGI - Projeto Abaco
*&---------------------------------------------------------------------*
*& Criado por:  José Godoy ( JAP ) - Ábaco Consultores
*& Data      : 18/05/2017
*& Pedido por: Cleudo Ferreira
*& Chamado/Descrição :xx Interface Exportação Dds.Mestres para SAAF
*& Request: DEVK971109 - PM - 25.05.2017 - Interface SAP X SAAF [  ] - JAP
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------

  NEW CL_MAIN_APP( )->GET_VEICULOS(
    EXPORTING
      FILIAL   = FILIAL
      SITUACAO = STATUS
      DATA     = CL_MAIN_APP=>GET_DATE_RANGE( FROM = CL_MAIN_APP=>CONVERT_DATE( DATA = DATA_DE ) TO = CL_MAIN_APP=>CONVERT_DATE( DATA_ATE ) )
      HORA     = CL_MAIN_APP=>GET_TIME_RANGE( FROM = CL_MAIN_APP=>CONVERT_TIME( TIME = DATA_DE ) TO = CL_MAIN_APP=>CONVERT_TIME( DATA_ATE ) )
    RECEIVING
      VEICULOS = RESULT[]
  ).

* Verifica se tabela de entrada está vazia
*  SELECT * INTO TABLE IT_EXP_P_SAAF
*     FROM ZTPM_EXP_P_SAAF.
**
** View de Veículos
*  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_M_VEIC_SAAF.
*  IF NOT SY-SUBRC IS INITIAL.
*    PERFORM ZF_INSERIR_TODA_TABELA_03.
**
*    IF NOT IT_M_VEIC_SAAF[] IS INITIAL.
*      RESULT[] = IT_M_VEIC_SAAF[].
*    ENDIF.
**
*  ELSE.
*    PERFORM ZF_INSERIR_NOVOS_TABELA_03.
**
*    IF NOT IT_M_VEIC_SAAF[] IS INITIAL.
*      RESULT[] = IT_M_VEIC_SAAF[].
*    ENDIF.
*
*  ENDIF.

*WIT_M_VEIC_SAAF

*  DATA IT_GENERAL TYPE REF TO DATA.
*  FIELD-SYMBOLS <IT_GENERAL_TAB> TYPE ANY TABLE.

*  CREATE DATA IT_GENERAL TYPE TABLE OF ZTPM_M_VEIC_SAAF.
*  ASSIGN IT_GENERAL->* TO <IT_GENERAL_TAB>.

*  TAB_DATA = IT_M_VEIC_SAAF[].

****
ENDFUNCTION.
