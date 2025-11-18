FUNCTION ZPM_EXPORT_B_CAUS_TO_SAAF .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(STATUS) TYPE  CHAR1
*"     REFERENCE(FILIAL) TYPE  WERKS_D
*"     REFERENCE(DATA_DE) TYPE  SY-DATUM
*"     REFERENCE(DATA_ATE) TYPE  SY-DATUM
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      RESULT STRUCTURE  ZTPM_B_CAUS_SAAF
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

  NEW CL_MAIN_APP( )->GET_CAUSA_MANUTENCOES(
    EXPORTING
      SITUACAO = STATUS
      FILIAL   = FILIAL
      DATA     = CL_MAIN_APP=>GET_DATE_RANGE( FROM = DATA_DE TO = DATA_ATE )
    RECEIVING
      CAUSAS   = RESULT[]
  ).

* Verifica se tabela de entrada está vazia
*  SELECT * INTO TABLE IT_EXP_P_SAAF
*     FROM ZTPM_EXP_P_SAAF.
*****
** View de Causa Manutenção
*  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_B_CAUS_SAAF.
*  IF NOT SY-SUBRC IS INITIAL.
*    PERFORM ZF_INSERIR_TODA_TABELA_16.
**
*    IF NOT IT_B_CAUS_SAAF[] IS INITIAL.
*      WIT_B_CAUS_SAAF[] = IT_B_CAUS_SAAF[].
*    ENDIF.
*  ENDIF.
****
ENDFUNCTION.
