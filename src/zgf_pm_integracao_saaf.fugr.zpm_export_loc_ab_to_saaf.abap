FUNCTION ZPM_EXPORT_LOC_AB_TO_SAAF .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(STATUS) TYPE  CHAR1
*"     REFERENCE(FILIAL) TYPE  WERKS_D
*"     REFERENCE(DATA_DE) TYPE  CHAR16
*"     REFERENCE(DATA_ATE) TYPE  CHAR16
*"  TABLES
*"      RESULT STRUCTURE  ZTPM_LOC_AB_SAAF
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

  NEW CL_MAIN_APP( )->GET_LOCAIS_ABASTECIMENTO(
    EXPORTING
      SITUACAO = STATUS
      FILIAL   = FILIAL
*      DATA     = CL_MAIN_APP=>GET_DATE_RANGE( FROM = DATA_DE TO = DATA_ATE )
      DATA           = CL_MAIN_APP=>GET_DATE_RANGE( FROM = CL_MAIN_APP=>CONVERT_DATE( DATA_DE ) TO = CL_MAIN_APP=>CONVERT_DATE( DATA_ATE ) )
      HORA           = CL_MAIN_APP=>GET_TIME_RANGE( FROM = CL_MAIN_APP=>CONVERT_TIME( DATA_DE ) TO = CL_MAIN_APP=>CONVERT_TIME( DATA_ATE ) )
    RECEIVING
      LOCAIS   = RESULT[]
  ).

* Verifica se tabela de entrada está vazia
*  SELECT * INTO TABLE IT_EXP_P_SAAF
*     FROM ZTPM_EXP_P_SAAF.
*****
** View de Local de Abastecimento
*  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_LOC_AB_SAAF.
*  IF NOT SY-SUBRC IS INITIAL.
*    PERFORM ZF_INSERIR_TODA_TABELA_09.
**
*    IF NOT IT_LOC_AB_SAAF[] IS INITIAL.
*      WIT_LOC_AB_SAAF[] = IT_LOC_AB_SAAF[].
*    ENDIF.
*  ENDIF.
****
ENDFUNCTION.
