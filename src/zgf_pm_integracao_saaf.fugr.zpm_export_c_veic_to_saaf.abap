FUNCTION ZPM_EXPORT_C_VEIC_TO_SAAF .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(STATUS) TYPE  CHAR1
*"     REFERENCE(FILIAL) TYPE  WERKS_D
*"     REFERENCE(DATA_DE) TYPE  CHAR16
*"     REFERENCE(DATA_ATE) TYPE  CHAR16
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      RESULT STRUCTURE  ZTPM_C_VEIC_SAAF
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

  NEW CL_MAIN_APP( )->GET_VEICULO_COMPARTIMENTOS(
    EXPORTING
      FILIAL         = FILIAL
      SITUACAO       = STATUS
      DATA           = CL_MAIN_APP=>GET_DATE_RANGE( FROM = CL_MAIN_APP=>CONVERT_DATE( DATA_DE ) TO = CL_MAIN_APP=>CONVERT_DATE( DATA_ATE ) )
      HORA           = CL_MAIN_APP=>GET_TIME_RANGE( FROM = CL_MAIN_APP=>CONVERT_TIME( DATA_DE ) TO = CL_MAIN_APP=>CONVERT_TIME( DATA_ATE ) )
    RECEIVING
      COMPARTIMENTOS = RESULT[]
  ).

* Verifica se tabela de entrada está vazia
*  SELECT * INTO TABLE IT_EXP_P_SAAF
*     FROM ZTPM_EXP_P_SAAF.
*****
** View de Veículo Compartimento
*  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_C_VEIC_SAAF.
*  IF NOT SY-SUBRC IS INITIAL.
*   PERFORM ZF_INSERIR_TODA_TABELA_18.
**
*    IF NOT IT_C_VEIC_SAAF[] IS INITIAL.
*      WIT_C_VEIC_SAAF[] = IT_C_VEIC_SAAF[].
*    ENDIF.
*  ENDIF.
****
ENDFUNCTION.
