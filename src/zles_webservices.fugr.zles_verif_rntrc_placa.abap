FUNCTION ZLES_VERIF_RNTRC_PLACA .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_PLACA) TYPE  ZPC_VEICULO
*"     REFERENCE(I_CNPJ_CPF) TYPE  STRING OPTIONAL
*"     REFERENCE(I_RNTRC) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     REFERENCE(I_SUCESSO) TYPE  CHAR01
*"     REFERENCE(I_TEXTO) TYPE  STRING
*"  RAISING
*"      ZCX_LES_WEBSERVICES
*"----------------------------------------------------------------------

  DATA: LC_CNPJ	 TYPE STCD1,
        LC_RNTRC TYPE STCD3.


  I_SUCESSO = '1'.

  TRY .

      IF I_CNPJ_CPF IS INITIAL.

        ZCL_WEBSERVICE_TIPCARD=>CONS_SITUACAO_TRANSPORTADOR(
          EXPORTING
            I_PLACA          = I_PLACA    " Placa veículo
          RECEIVING
            E_CONSULTAS      = DATA(E_CONSULTAS)    " Tabela de Consultas Transportador
          EXCEPTIONS
            ERRO             = 1
            WEBSERVICE       = 2
            OTHERS           = 3 ).

      ELSEIF I_CNPJ_CPF IS NOT INITIAL AND I_RNTRC IS  NOT INITIAL.

        LC_CNPJ  = I_CNPJ_CPF.
        LC_RNTRC = I_RNTRC.

        ZCL_WEBSERVICE_TIPCARD=>CONS_SITUACAO_TRANSPORTADOR(
          EXPORTING
            I_PLACA          = I_PLACA    " Placa veículo
            I_PESQUISA_LIVRE = ABAP_TRUE
            I_CNPJ           = LC_CNPJ    " Nº ID fiscal 1
            I_RNTRC          = LC_RNTRC    " Nº identificação fiscal 3
          RECEIVING
            E_CONSULTAS      = E_CONSULTAS    " Tabela de Consultas Transportador
          EXCEPTIONS
            ERRO             = 1
            WEBSERVICE       = 2
            OTHERS           = 3 ).

      ELSE.
        SY-SUBRC = 1.
      ENDIF.

      IF SY-SUBRC EQ 1.
        I_SUCESSO = '0'.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO I_TEXTO.
        EXIT.
      ENDIF.

      CHECK SY-SUBRC IS INITIAL.

      READ TABLE E_CONSULTAS INDEX 1 INTO DATA(WA_CONSULTAS).

      CHECK SY-SUBRC IS INITIAL.

      IF WA_CONSULTAS-CK_RNTRC_ATIVO EQ ABAP_FALSE.
        I_SUCESSO = '0'.
        I_TEXTO = WA_CONSULTAS-DS_RAZAO_SOCIAL && ' com RNTRC não ativo! ' && WA_CONSULTAS-DS_MSG_TRANSPORTADOR.
      ELSE.
        I_SUCESSO = '1'.
        I_TEXTO = WA_CONSULTAS-DS_MSG_TRANSPORTADOR.
      ENDIF.

    CATCH CX_ROOT.
      I_SUCESSO = '1'.
  ENDTRY.

ENDFUNCTION.
