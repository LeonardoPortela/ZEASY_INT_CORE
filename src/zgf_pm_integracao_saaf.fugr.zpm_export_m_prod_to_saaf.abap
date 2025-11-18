FUNCTION ZPM_EXPORT_M_PROD_TO_SAAF .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      RESULT STRUCTURE  ZTPM_M_PROD_SAAF
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

  RESULT[] = NEW CL_MAIN_APP( )->GET_PRODUTOS( ).

* Verifica se tabela de entrada está vazia
*  SELECT * INTO TABLE IT_EXP_P_SAAF
*     FROM ZTPM_EXP_P_SAAF.
*****
** View de Produtos
*  READ TABLE IT_EXP_P_SAAF WITH KEY TABELA = LC_ZTPM_M_PROD_SAAF.
*  IF NOT SY-SUBRC IS INITIAL.
*    PERFORM ZF_INSERIR_TODA_TABELA_05.
**
*    IF NOT IT_M_PROD_SAAF[] IS INITIAL.
*      WIT_M_PROD_SAAF[] = IT_M_PROD_SAAF[].
*    ENDIF.
*  ENDIF.
****
ENDFUNCTION.
