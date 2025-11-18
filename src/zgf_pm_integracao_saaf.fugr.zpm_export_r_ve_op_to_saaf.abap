FUNCTION ZPM_EXPORT_R_VE_OP_TO_SAAF .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DATA_DE) TYPE  CHAR16
*"     REFERENCE(DATA_ATE) TYPE  CHAR16
*"     REFERENCE(STATUS) TYPE  C
*"     REFERENCE(FILIAL) TYPE  IWERK
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      RESULT STRUCTURE  ZTPM_R_VE_OP_SAAF
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

  NEW CL_MAIN_APP( )->GET_VEICULO_OPERACOES(
    EXPORTING
      FILIAL    = FILIAL
      SITUACAO  = STATUS
*      DATA      = CL_MAIN_APP=>GET_DATE_RANGE( FROM = DATA_DE TO = DATA_ATE )
      DATA           = CL_MAIN_APP=>GET_DATE_RANGE( FROM = CL_MAIN_APP=>CONVERT_DATE( DATA_DE ) TO = CL_MAIN_APP=>CONVERT_DATE( DATA_ATE ) )
      HORA           = CL_MAIN_APP=>GET_TIME_RANGE( FROM = CL_MAIN_APP=>CONVERT_TIME( DATA_DE ) TO = CL_MAIN_APP=>CONVERT_TIME( DATA_ATE ) )
    RECEIVING
      OPERACOES = RESULT[]
  ).

* Verifica se tabela de entrada está vazia
*  SELECT * INTO TABLE IT_EXP_P_SAAF
*     FROM ZTPM_EXP_P_SAAF.
*****
** View de Veículo X Operação
*  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_R_VE_OP_SAAF.
*  IF NOT SY-SUBRC IS INITIAL.
*    PERFORM ZF_INSERIR_TODA_TABELA_10.
**
*    IF NOT IT_R_VE_OP_SAAF[] IS INITIAL.
*      WIT_R_VE_OP_SAAF[] = IT_R_VE_OP_SAAF[].
*    ENDIF.
*  ENDIF.
****
ENDFUNCTION.
