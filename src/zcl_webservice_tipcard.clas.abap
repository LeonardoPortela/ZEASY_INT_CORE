class ZCL_WEBSERVICE_TIPCARD definition
  public
  inheriting from ZCL_WEBSERVICE
  create public .

*"* public components of class ZCL_WEBSERVICE_TIPCARD
*"* do not include other source files here!!!
public section.

  data CK_SALVAR_XML_LOCAL type CHAR01 .

  methods AUTENTICACAO
    importing
      !I_HTTP type ref to IF_HTTP_CLIENT optional
      !I_OPERADORA type CHAR3
      !I_GRUPO type ZDE_DS_GRUPO_EMPRESA
    returning
      value(E_CHAVE) type CHAR32
    raising
      ZCX_WEBSERVICE .
  methods BUSCAR_ROTAS
    importing
      !I_GRUPO type ZDE_DS_GRUPO_EMPRESA
    raising
      ZCX_WEBSERVICE .
  methods LER_XML_ROTAS
    importing
      !I_GRUPO type ZDE_DS_GRUPO_EMPRESA
      !I_XML type STRING
    raising
      ZCX_WEBSERVICE .
  methods CHAVE_SEGURANCA
    importing
      !I_GRUPO type ZDE_DS_GRUPO_EMPRESA
    returning
      value(E_CHAVE) type CHAR32
    raising
      ZCX_WEBSERVICE .
  methods ATUALIZAR_VALORES
    importing
      !I_GRUPO type ZDE_DS_GRUPO_EMPRESA .
  methods XML_AUTENTICA
    importing
      !I_USUARIO type ZUSE_WEB
      !I_SENHA type ZPASS_WEB
    returning
      value(E_XML) type STRING .
  methods XML_CONSULTA_ROTA .
  methods XML_ATUALIZA_ROTA
    importing
      !I_CHAVE type CHAR32
      !I_CNPJCONTRATANTE type SETVALMIN
    returning
      value(E_XML) type STRING .
  methods CONSULTAR_ARQUIVO_COBRANCA
    importing
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_FATURA type ZPFE_NR_LOTE_ADM optional
    exporting
      !E_MSG type CHAR255
      value(R_LINKARQUIVO) type STRING
      value(R_LINKARQUIVO_PEDAGIO) type STRING
      value(R_CONTENT_ARQUIVO) type STRING
      value(R_CONTENT_ARQUIVO_PEDAGIO) type STRING
    exceptions
      ZWEBSERVICE .
  methods CONSULTAR_ARQUIVO_CONFERENCIA
    importing
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_DT_INICIAL type DATUM
      !I_DT_FINAL type DATUM
    exporting
      !E_MSG type CHAR255
    returning
      value(R_LINKARQUIVO) type STRING
    exceptions
      ZWEBSERVICE .
  methods CONSULTAR_ROTA
    importing
      !I_ROTA type ZDE_ID_ROTA
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    exporting
      !E_MSG type CHAR255
    exceptions
      ZWEBSERVICE .
  methods SOLICITA_ROTA
    importing
      !I_ROTA type ZDE_ID_ROTA
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    exporting
      !E_MSG type CHAR255
      !E_ID_ROTA_ADM type ZDE_ID_ROTA_ADM
    exceptions
      ZWEBSERVICE .
  methods ATUALIZAR_ROTA
    importing
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    exporting
      !E_MSG type CHAR255
    exceptions
      ZWEBSERVICE .
  class-methods CONS_SITUACAO_TRANSPORTADOR
    importing
      !I_BUKRS type BUKRS default '0001'
      !I_BRANCH type J_1BBRANC_ default '0116'
      !I_PARTINER type J_1BPARID optional
      !I_PLACA type ZPC_VEICULO optional
      !I_PESQUISA_LIVRE type CHAR01 default ' '
      !I_CNPJ type STCD1 optional
      !I_RNTRC type STCD3 optional
      !I_CK_CONSULTA type ZDE_CK_RNTRC_CS optional
    returning
      value(E_CONSULTAS) type ZLEST0135_T
    exceptions
      ERRO
      WEBSERVICE .
  methods SALVA_XML
    importing
      !I_NAME_FILE type STRING
      !I_XML type STRING .
  methods XML_CONSULTAR_ARQ_COBRANCA
    importing
      !I_CHAVE type CHAR32
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_FATURA type ZPFE_NR_LOTE_ADM optional
    returning
      value(E_XML) type STRING .
  methods XML_CONSULTAR_ARQ_CONFERENCIA
    importing
      !I_CHAVE type CHAR32
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_DT_INICIAL type DATUM
      !I_DT_FINAL type DATUM
    returning
      value(E_XML) type STRING .
  class-methods PROCESSAR_ARQUIVO_COBRANCA
    importing
      !I_FILENAME type STRING
      !I_FILENAME_PEDAGIO type STRING
      !I_CONTENT_FILENAME type STRING optional
      !I_CONTENT_FILENAME_PEDAGIO type STRING optional
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    exporting
      !E_LOTES type ZPFE_NUMERO_LOTE_T
    raising
      ZCX_CADASTRO
      ZCX_ERRO_ARQUIVO .
  class-methods PROCESSAR_ARQUIVO_CONFERENCIA
    importing
      !I_FILENAME type STRING
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
      !I_DT_INICIAL type DATUM
      !I_DT_FINAL type DATUM
    raising
      ZCX_CADASTRO
      ZCX_ERRO_ARQUIVO .
  methods CHECK_DATA_ENVIO_EMAIL
    importing
      !E_CONSULTAS type ZLEST0135 .
  methods CONS_STATUS_PARCEIRO
    importing
      !I_BUKRS type BUKRS default '0001'
      !I_BRANCH type J_1BBRANC_ default '0116'
      !I_PARTINER type J_1BPARID optional
      !I_PLACA type ZPC_VEICULO optional
    returning
      value(E_CONSULTAS) type ZLEST0135_T
    exceptions
      ERRO
      WEBSERVICE .
  methods XML_CONSULTA_STATUS_PARCEIRO
    importing
      !I_CHAVE type CHAR32
      !I_CONSULTA type ZDE_CONSULTA_PARCEIRO
    returning
      value(E_XML) type STRING .
  methods ENVIAR_EMAIL_TIP
    importing
      !E_CONSULTAS type ZLEST0135 .
protected section.

*"* protected components of class ZCL_WEBSERVICE_TIPCARD
*"* do not include other source files here!!!
  methods XML_ATUALIZAR_ROTA
    importing
      !I_CHAVE type CHAR32
      !I_BUKRS type BUKRS
      !I_BRANCH type J_1BBRANC_
    returning
      value(E_XML) type STRING .
  methods XML_CONSULTAR_ROTA
    importing
      !I_CHAVE type CHAR32
      !I_ZLEST0101 type ZLEST0101
    returning
      value(E_XML) type STRING .
  methods XML_CRIAR_ROTA
    importing
      !I_CHAVE type CHAR32
      !I_ZLEST0101 type ZLEST0101
      !I_ZLEST0107 type ZLEST0107_T
    returning
      value(E_XML) type STRING .
  methods LER_XML_ATUALIZAR_ROTA
    importing
      !I_XML type STRING
    exporting
      !E_MSG type CHAR255
      !E_ROTAS type ZLEST0101_T
      !E_PRACAS type ZLEST0102_T .
  methods LER_XML_CONSULTAR_ROTA
    importing
      !I_XML type STRING
    exporting
      !E_ROTAS type ZLEST0101_T
      !E_PRACAS type ZLEST0102_T
      !E_MSG type CHAR255 .
  methods LER_XML_CRIAR_ROTA
    importing
      !I_XML type STRING
    exporting
      !E_MSG type CHAR255
      !E_ID_ROTA_ADM type ZDE_ID_ROTA_ADM .
  methods XML_CONSULTA_TRANSPORTADOR
    importing
      !I_CHAVE type CHAR32
      !I_CONSULTA_RNTRC type ZDE_CONSULTA_RNTRC
    returning
      value(E_XML) type STRING .
  methods LER_XML_CONSULTAR_ARQ_COBRANCA
    importing
      !I_XML type STRING
    exporting
      !E_MSG type CHAR255
      value(R_LINKARQUIVO) type STRING
      value(R_LINKARQUIVO_PEDAGIO) type STRING
      value(R_CONTENT_ARQUIVO) type STRING
      value(R_CONTENT_ARQUIVO_PEDAGIO) type STRING .
  methods LER_XML_CONSULTAR_ARQ_CONFERE
    importing
      !I_XML type STRING
    exporting
      !E_MSG type CHAR255
      !E_CODIGO type STRING
    returning
      value(R_LINKARQUIVO) type STRING .
  methods LER_XML_SITUACAO_TRANSPORTADOR
    importing
      !I_XML type STRING
    returning
      value(E_ZLEST0135) type ZLEST0135 .
private section.

  methods LER_XML_STATUS_PARCEIRO
    importing
      !I_XML type STRING
    returning
      value(E_ZLEST0135) type ZLEST0135 .
*"* private components of class ZCL_WEBSERVICE_TIPCARD
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_WEBSERVICE_TIPCARD IMPLEMENTATION.


  METHOD atualizar_rota.

    DATA: e_chave      TYPE char32,
          lc_msg       TYPE string,
          lc_msg_adm   TYPE string,
          cx_exception TYPE REF TO zcx_webservice,
          lc_xml       TYPE string,
          lc_http      TYPE REF TO if_http_client,
          e_rotas	     TYPE	zlest0101_t,
          e_pracas     TYPE zlest0102_t,
          it_zlest0101 TYPE TABLE OF zlest0101,
          it_zlest0102 TYPE TABLE OF zlest0102,
          it_zlest0084 TYPE TABLE OF zlest0084,
          it_zlest0091 TYPE TABLE OF zlest0091,
          wa_zlest0101 TYPE zlest0101,
          wa_zlest010a TYPE zlest0101,
          wa_zlest0102 TYPE zlest0102,
          wa_zlest0084 TYPE zlest0084,
          wa_zlest0091 TYPE zlest0091,
          cd_id_rota   TYPE zde_id_rota.

    FIELD-SYMBOLS: <z101> TYPE zlest0101,
                   <z102> TYPE zlest0102,
                   <z084> TYPE zlest0084.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs  EQ @i_bukrs
       AND branch EQ @i_branch.

    TRY .
        e_chave = me->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    lc_xml = xml_atualizar_rota( i_chave = e_chave i_bukrs = i_bukrs i_branch = i_branch ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RO - Criar Rota.
        me->set_servico( EXPORTING i_servico = 'RA' ).

      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->set_tipo( EXPORTING i_tipo = 'R').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        lc_http = me->url( ).

      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->zif_webservice~abrir_conexao( lc_http ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD me->zif_webservice~consultar
      EXPORTING
        i_http                     = lc_http
        i_xml                      = lc_xml
      RECEIVING
        e_resultado                = lc_msg_adm
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zwebservice.
    ENDIF.

    CLEAR: e_msg.

    CALL METHOD me->ler_xml_atualizar_rota
      EXPORTING
        i_xml    = lc_msg_adm
      IMPORTING
        e_msg    = e_msg
        e_rotas  = e_rotas
        e_pracas = e_pracas.

    IF e_msg NE 'Sucesso'.

      MESSAGE |Falha ao consultar Rotas Empresa: { i_bukrs } Filial: { i_branch } | TYPE 'S'.
      MESSAGE |Mensagem Retorno Serviço: { e_msg } Data: { sy-datum } Horas: { sy-uzeit } | TYPE 'S'.

      MESSAGE e_msg TYPE 'E' RAISING zwebservice.
    ELSE.

      SELECT * INTO TABLE it_zlest0101
        FROM zlest0101
       WHERE bukrs  EQ i_bukrs
         AND branch EQ i_branch.

      SELECT * INTO TABLE it_zlest0091
        FROM zlest0091.

      SELECT * INTO TABLE it_zlest0084
        FROM zlest0084
       WHERE bukrs  EQ i_bukrs
         AND branch EQ i_branch.

      IF it_zlest0101 IS NOT INITIAL.
        SELECT * INTO TABLE it_zlest0102
          FROM zlest0102
           FOR ALL ENTRIES IN it_zlest0101
         WHERE id_rota EQ it_zlest0101-id_rota
           AND bukrs   EQ it_zlest0101-bukrs
           AND branch  EQ it_zlest0101-branch.
      ENDIF.

      SORT e_rotas      BY bukrs branch country cd_cid_origem cd_cid_destino tp_rota_perc id_rota_adm.
      SORT it_zlest0101 BY bukrs branch country cd_cid_origem cd_cid_destino tp_rota_perc id_rota_adm.

      SELECT MAX( id_rota ) INTO cd_id_rota
        FROM zlest0101.

      IF cd_id_rota IS INITIAL OR cd_id_rota EQ 0.
        MOVE 1 TO cd_id_rota.
      ELSE.
        ADD 1 TO cd_id_rota.
      ENDIF.

      "Marcar Rota como Ativa/Desativa
      LOOP AT it_zlest0101 ASSIGNING <z101>.
        READ TABLE e_rotas INTO wa_zlest010a
                           WITH KEY bukrs          = <z101>-bukrs
                                    branch         = <z101>-branch
                                    country        = <z101>-country
                                    cd_cid_origem  = <z101>-cd_cid_origem
                                    cd_cid_destino = <z101>-cd_cid_destino
                                    tp_rota_perc   = <z101>-tp_rota_perc
                                    id_rota_adm    = <z101>-id_rota_adm.
        IF sy-subrc IS NOT INITIAL.
          CLEAR: <z101>-st_rota.
        ELSE.
          <z101>-st_rota = abap_true.
        ENDIF.
      ENDLOOP.

      "Incluir Nova Rota
      LOOP AT e_rotas ASSIGNING <z101>.
        READ TABLE it_zlest0101 INTO wa_zlest010a
                            WITH KEY bukrs          = <z101>-bukrs
                                     branch         = <z101>-branch
                                     country        = <z101>-country
                                     cd_cid_origem  = <z101>-cd_cid_origem
                                     cd_cid_destino = <z101>-cd_cid_destino
                                     tp_rota_perc   = <z101>-tp_rota_perc
                                     id_rota_adm    = <z101>-id_rota_adm.
        IF sy-subrc IS NOT INITIAL.
          "Nova Rota
          CLEAR: <z101>-st_rota.
          <z101>-id_rota = cd_id_rota.
          <z101>-st_rota = abap_true.
          LOOP AT e_pracas ASSIGNING <z102> WHERE id_rota_adm EQ <z101>-id_rota_adm.
            <z102>-id_rota   = <z101>-id_rota.
            <z102>-bukrs     = <z101>-bukrs.
            <z102>-branch    = <z101>-branch.
            <z102>-st_praca  = abap_true.
            <z102>-vl_eixo1  = <z102>-vl_eixo1 / 100.
            <z102>-vl_eixo2  = <z102>-vl_eixo2 / 100.
            <z102>-vl_eixo3  = <z102>-vl_eixo3 / 100.
            <z102>-vl_eixo4  = <z102>-vl_eixo4 / 100.
            <z102>-vl_eixo5  = <z102>-vl_eixo5 / 100.
            <z102>-vl_eixo6  = <z102>-vl_eixo6 / 100.
            <z102>-vl_eixo7  = <z102>-vl_eixo7 / 100.
            <z102>-vl_eixo8  = <z102>-vl_eixo8 / 100.
            <z102>-vl_eixo9  = <z102>-vl_eixo9 / 100.
            <z102>-vl_eixo10 = <z102>-vl_eixo10 / 100.
*** Inicio - Rubenilson - 17.12.24 - US160867
            <z102>-vl_tageixo1  = <z102>-vl_tageixo1 / 100.
            <z102>-vl_tageixo2  = <z102>-vl_tageixo2 / 100.
            <z102>-vl_tageixo3  = <z102>-vl_tageixo3 / 100.
            <z102>-vl_tageixo4  = <z102>-vl_tageixo4 / 100.
            <z102>-vl_tageixo5  = <z102>-vl_tageixo5 / 100.
            <z102>-vl_tageixo6  = <z102>-vl_tageixo6 / 100.
            <z102>-vl_tageixo7  = <z102>-vl_tageixo7 / 100.
            <z102>-vl_tageixo8  = <z102>-vl_tageixo8 / 100.
            <z102>-vl_tageixo9  = <z102>-vl_tageixo9 / 100.
            <z102>-vl_tageixo10 = <z102>-vl_tageixo10 / 100.
***Fim - Rubenilson - 17.12.24 - US160867
            APPEND <z102> TO it_zlest0102.
          ENDLOOP.
          APPEND <z101> TO it_zlest0101.
          ADD 1 TO cd_id_rota.
        ELSE.
          LOOP AT e_pracas ASSIGNING <z102> WHERE id_rota_adm EQ wa_zlest010a-id_rota_adm.
            <z102>-id_rota   = wa_zlest010a-id_rota.
            <z102>-bukrs     = wa_zlest010a-bukrs.
            <z102>-branch    = wa_zlest010a-branch.
            <z102>-st_praca  = abap_true.
            <z102>-vl_eixo1  = <z102>-vl_eixo1 / 100.
            <z102>-vl_eixo2  = <z102>-vl_eixo2 / 100.
            <z102>-vl_eixo3  = <z102>-vl_eixo3 / 100.
            <z102>-vl_eixo4  = <z102>-vl_eixo4 / 100.
            <z102>-vl_eixo5  = <z102>-vl_eixo5 / 100.
            <z102>-vl_eixo6  = <z102>-vl_eixo6 / 100.
            <z102>-vl_eixo7  = <z102>-vl_eixo7 / 100.
            <z102>-vl_eixo8  = <z102>-vl_eixo8 / 100.
            <z102>-vl_eixo9  = <z102>-vl_eixo9 / 100.
            <z102>-vl_eixo10 = <z102>-vl_eixo10 / 100.
*** Inicio - Rubenilson - 17.12.24 - US160867
            <z102>-vl_tageixo1  = <z102>-vl_tageixo1 / 100.
            <z102>-vl_tageixo2  = <z102>-vl_tageixo2 / 100.
            <z102>-vl_tageixo3  = <z102>-vl_tageixo3 / 100.
            <z102>-vl_tageixo4  = <z102>-vl_tageixo4 / 100.
            <z102>-vl_tageixo5  = <z102>-vl_tageixo5 / 100.
            <z102>-vl_tageixo6  = <z102>-vl_tageixo6 / 100.
            <z102>-vl_tageixo7  = <z102>-vl_tageixo7 / 100.
            <z102>-vl_tageixo8  = <z102>-vl_tageixo8 / 100.
            <z102>-vl_tageixo9  = <z102>-vl_tageixo9 / 100.
            <z102>-vl_tageixo10 = <z102>-vl_tageixo10 / 100.
*** Fim - Rubenilson - 17.12.24 - US160867
            READ TABLE it_zlest0102 INTO wa_zlest0102
                                    WITH KEY id_rota_adm = wa_zlest010a-id_rota_adm
                                             id_praca    = <z102>-id_praca.
            IF sy-subrc IS INITIAL.
              MODIFY it_zlest0102 INDEX sy-tabix FROM <z102>.
            ELSE.
              APPEND <z102> TO it_zlest0102.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      LOOP AT it_zlest0102 ASSIGNING <z102>.
        READ TABLE e_pracas INTO wa_zlest0102
                            WITH KEY id_rota_adm = <z102>-id_rota_adm
                                     id_praca    = <z102>-id_praca.
        IF sy-subrc IS NOT INITIAL.
          <z102>-st_praca = abap_false.
        ENDIF.
      ENDLOOP.

      LOOP AT it_zlest0101 ASSIGNING <z101> WHERE st_rota = abap_true.
        <z101>-vl_eixo1  = 0.
        <z101>-vl_eixo2  = 0.
        <z101>-vl_eixo3  = 0.
        <z101>-vl_eixo4  = 0.
        <z101>-vl_eixo5  = 0.
        <z101>-vl_eixo6  = 0.
        <z101>-vl_eixo7  = 0.
        <z101>-vl_eixo8  = 0.
        <z101>-vl_eixo9  = 0.
        <z101>-vl_eixo10 = 0.
*** Inicio - Rubenilson - 17.12.24 - US160867
        <z101>-vl_tageixo1  = 0.
        <z101>-vl_tageixo2  = 0.
        <z101>-vl_tageixo3  = 0.
        <z101>-vl_tageixo4  = 0.
        <z101>-vl_tageixo5  = 0.
        <z101>-vl_tageixo6  = 0.
        <z101>-vl_tageixo7  = 0.
        <z101>-vl_tageixo8  = 0.
        <z101>-vl_tageixo9  = 0.
        <z101>-vl_tageixo10 = 0.
*** Fim - Rubenilson - 17.12.24 - US160867
        LOOP AT it_zlest0102 INTO wa_zlest0102 WHERE st_praca = abap_true AND id_rota EQ <z101>-id_rota.
          ADD wa_zlest0102-vl_eixo1  TO <z101>-vl_eixo1.
          ADD wa_zlest0102-vl_eixo2  TO <z101>-vl_eixo2.
          ADD wa_zlest0102-vl_eixo3  TO <z101>-vl_eixo3.
          ADD wa_zlest0102-vl_eixo4  TO <z101>-vl_eixo4.
          ADD wa_zlest0102-vl_eixo5  TO <z101>-vl_eixo5.
          ADD wa_zlest0102-vl_eixo6  TO <z101>-vl_eixo6.
          ADD wa_zlest0102-vl_eixo7  TO <z101>-vl_eixo7.
          ADD wa_zlest0102-vl_eixo8  TO <z101>-vl_eixo8.
          ADD wa_zlest0102-vl_eixo9  TO <z101>-vl_eixo9.
          ADD wa_zlest0102-vl_eixo10 TO <z101>-vl_eixo10.
*** Inicio - Rubenilson - 17.12.24 - US160867
          ADD wa_zlest0102-vl_tageixo1  TO <z101>-vl_tageixo1.
          ADD wa_zlest0102-vl_tageixo2  TO <z101>-vl_tageixo2.
          ADD wa_zlest0102-vl_tageixo3  TO <z101>-vl_tageixo3.
          ADD wa_zlest0102-vl_tageixo4  TO <z101>-vl_tageixo4.
          ADD wa_zlest0102-vl_tageixo5  TO <z101>-vl_tageixo5.
          ADD wa_zlest0102-vl_tageixo6  TO <z101>-vl_tageixo6.
          ADD wa_zlest0102-vl_tageixo7  TO <z101>-vl_tageixo7.
          ADD wa_zlest0102-vl_tageixo8  TO <z101>-vl_tageixo8.
          ADD wa_zlest0102-vl_tageixo9  TO <z101>-vl_tageixo9.
          ADD wa_zlest0102-vl_tageixo10 TO <z101>-vl_tageixo10.
*** Fim - Rubenilson - 17.12.24 - US160867
        ENDLOOP.
      ENDLOOP.

      SORT it_zlest0101 BY id_rota_adm.

      LOOP AT it_zlest0084 ASSIGNING <z084>.
        READ TABLE it_zlest0101 INTO wa_zlest0101 WITH KEY id_rota_adm = <z084>-id_rota.
        IF sy-subrc IS NOT INITIAL.
          <z084>-st_rota = abap_false.
        ELSE.
          READ TABLE it_zlest0091 INTO wa_zlest0091 WITH KEY categoria = <z084>-cat_veiculo.
          <z084>-st_rota  = wa_zlest0101-st_rota.
          CASE wa_zlest0091-qtd_eixo.
            WHEN 1.
              <z084>-vlr_pedagio    = wa_zlest0101-vl_eixo1.
              <z084>-nv_vl_pedagio  = wa_zlest0101-vl_eixo1.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo1."Rubenilson - 17.12.24 - US160867
            WHEN 2.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo2.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo2.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo2."Rubenilson - 17.12.24 - US160867
            WHEN 3.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo3.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo3.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo3."Rubenilson - 17.12.24 - US160867
            WHEN 4.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo4.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo4.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo4."Rubenilson - 17.12.24 - US160867
            WHEN 5.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo5.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo5.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo5."Rubenilson - 17.12.24 - US160867
            WHEN 6.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo6.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo6.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo6."Rubenilson - 17.12.24 - US160867
            WHEN 7.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo7.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo7.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo7."Rubenilson - 17.12.24 - US160867
            WHEN 8.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo8.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo8.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo8."Rubenilson - 17.12.24 - US160867
            WHEN 9.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo9.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo9.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo9."Rubenilson - 17.12.24 - US160867
            WHEN 10.
              <z084>-vlr_pedagio   = wa_zlest0101-vl_eixo10.
              <z084>-nv_vl_pedagio = wa_zlest0101-vl_eixo10.
              <z084>-tag_vl_pedagio =  wa_zlest0101-vl_tageixo10."Rubenilson - 17.12.24 - US160867
          ENDCASE.
        ENDIF.
      ENDLOOP.

      SORT it_zlest0084 BY id_rota cat_veiculo.

      LOOP AT it_zlest0101 INTO wa_zlest0101.
        LOOP AT it_zlest0091 INTO wa_zlest0091.
          READ TABLE it_zlest0084 INTO wa_zlest0084 WITH KEY id_rota     = wa_zlest0101-id_rota_adm
                                                             bukrs       = wa_zlest0101-bukrs
                                                             branch      = wa_zlest0101-branch
                                                             cat_veiculo = wa_zlest0091-categoria.
          IF sy-subrc IS INITIAL.
            wa_zlest0084-id_rota       = wa_zlest0101-id_rota_adm.

            CASE wa_zlest0091-qtd_eixo.
              WHEN 1.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo1.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo1.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo1."Rubenilson - 17.12.24 - US160867
              WHEN 2.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo2.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo2.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo2."Rubenilson - 17.12.24 - US160867
              WHEN 3.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo3.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo3.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo3."Rubenilson - 17.12.24 - US160867
              WHEN 4.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo4.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo4.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo4."Rubenilson - 17.12.24 - US160867
              WHEN 5.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo5.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo5.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo5."Rubenilson - 17.12.24 - US160867
              WHEN 6.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo6.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo6.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo6."Rubenilson - 17.12.24 - US160867
              WHEN 7.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo7.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo7.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo7."Rubenilson - 17.12.24 - US160867
              WHEN 8.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo8.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo8.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo8."Rubenilson - 17.12.24 - US160867
              WHEN 9.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo9.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo9.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo9."Rubenilson - 17.12.24 - US160867
              WHEN 10.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo10.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo10.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo10."Rubenilson - 17.12.24 - US160867
            ENDCASE.

            wa_zlest0084-st_rota       = wa_zlest0101-st_rota.
            MODIFY it_zlest0084 INDEX sy-tabix FROM wa_zlest0084.
          ELSE.
            CLEAR: wa_zlest0084.
            wa_zlest0084-id_rota       = wa_zlest0101-id_rota_adm.
            wa_zlest0084-bukrs         = wa_zlest0101-bukrs.
            wa_zlest0084-branch        = wa_zlest0101-branch.
            wa_zlest0084-cat_veiculo   = wa_zlest0091-categoria.
            wa_zlest0084-munic_origem  = wa_zlest0101-cd_cid_origem+3(7).
            wa_zlest0084-munic_destino = wa_zlest0101-cd_cid_destino+3(7).

            CASE wa_zlest0091-qtd_eixo.
              WHEN 1.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo1.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo1.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo1."Rubenilson - 17.12.24 - US160867
              WHEN 2.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo2.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo2.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo2."Rubenilson - 17.12.24 - US160867
              WHEN 3.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo3.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo3.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo3."Rubenilson - 17.12.24 - US160867
              WHEN 4.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo4.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo4.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo4."Rubenilson - 17.12.24 - US160867
              WHEN 5.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo5.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo5.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo5."Rubenilson - 17.12.24 - US160867
              WHEN 6.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo6.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo6.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo6."Rubenilson - 17.12.24 - US160867
              WHEN 7.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo7.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo7.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo7."Rubenilson - 17.12.24 - US160867
              WHEN 8.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo8.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo8.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo8."Rubenilson - 17.12.24 - US160867
              WHEN 9.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo9.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo9.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo9."Rubenilson - 17.12.24 - US160867
              WHEN 10.
                wa_zlest0084-vlr_pedagio   = wa_zlest0101-vl_eixo10.
                wa_zlest0084-nv_vl_pedagio = wa_zlest0101-vl_eixo10.
                wa_zlest0084-tag_vl_pedagio =  wa_zlest0101-vl_tageixo10."Rubenilson - 17.12.24 - US160867
            ENDCASE.

            wa_zlest0084-st_rota       = wa_zlest0101-st_rota.
            APPEND wa_zlest0084 TO it_zlest0084.
            SORT it_zlest0084 BY id_rota cat_veiculo.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      MODIFY zlest0101 FROM TABLE it_zlest0101.
      MODIFY zlest0102 FROM TABLE it_zlest0102.
      MODIFY zlest0084 FROM TABLE it_zlest0084.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD ATUALIZAR_VALORES.
*****************
*  Descrição: Método responsavel por atualizar os novos valores para o pedágio e sua data de vigencia.
*  Data: 05.05.2014 14:43:10
*****************

    DATA: OBJ_ZCL_ROTA_DB TYPE REF TO ZCL_ROTA_DB.

    FREE OBJ_ZCL_ROTA_DB.

    CREATE OBJECT OBJ_ZCL_ROTA_DB.
    OBJ_ZCL_ROTA_DB->SELECIONA_NOVOS( I_GRUPO = I_GRUPO ).

  ENDMETHOD.


  METHOD autenticacao.
    "Método para Autenticação com a Tipcard.

    "Esse item trata do serviço de autenticação para consumir os serviços
    "disponibilizados pelo WSConvenio. Na requisição é informado um usuário e
    "uma senha. E na reposta é retornada uma chave válida para consumo dos
    "serviços.

    "Estrutura
    "<autenticacao>
    "  <usuario>XXXXXXXXXXXX</usuario>
    "  <senha>XXXXXXXXXXXXXX</senha>
    "</autenticacao>

    DATA: gw_zlest0085  TYPE zlest0085, "Tabela de Cadastro de Usuário e Senha para autenticação TIPCARD.
          xml_ret       TYPE REF TO cl_xml_document,
          xml_node      TYPE REF TO if_ixml_node,
          xml_autentica TYPE string. "Montagem do XML Para Autenticação.

    DATA: var_resultado TYPE string, "Resultado do XML de Autenticação.
          var_tam       TYPE i, "Variavel para guardar o tamanho do XML de retorno.
          var_valor     TYPE string. "Valor da TAG.

    DATA: zcl_rota_db TYPE REF TO zcl_rota_db. "Classe para Persistencia no Banco de Dados.


    "Limpar as variaveis, work areas e objetos.
    CLEAR: xml_autentica, var_resultado, var_tam,
           gw_zlest0085,  xml_ret, xml_node, e_chave.

    "Clear no Objeto.
    FREE: zcl_rota_db.

    "Selecionar o usuário e senha de autenticação da tipcard.
    SELECT SINGLE * FROM zlest0085 INTO gw_zlest0085
     WHERE operadora EQ i_operadora
       AND ds_grupo  EQ i_grupo.

    IF ( sy-subrc EQ 0 ).

      "Monta o arquivo
      xml_autentica = me->xml_autentica(
                                         i_usuario = gw_zlest0085-usuario
                                         i_senha   = gw_zlest0085-senha
                                       ).

      me->zif_webservice~abrir_conexao( i_http = i_http ).

      CALL METHOD me->zif_webservice~consultar
        EXPORTING
          i_http                     = i_http
          i_xml                      = xml_autentica
        RECEIVING
          e_resultado                = var_resultado
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5.

      IF sy-subrc IS NOT INITIAL.
*-168836-26.02.2025-JT-inicio
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(msg_texto).
*       zcx_webservice=>gera_erro_geral( i_texto = msg_texto ).
*-168836-26.02.2025-JT-fim
      ENDIF.

      CREATE OBJECT xml_ret.

      CALL METHOD xml_ret->parse_string
        EXPORTING
          stream  = var_resultado
        RECEIVING
          retcode = var_tam.

      CALL METHOD xml_ret->find_node
        EXPORTING
          name = 'msg'
        RECEIVING
          node = xml_node.

      IF ( sy-subrc EQ 0 ) AND ( NOT xml_node IS INITIAL ).

        CALL METHOD xml_node->get_value
          RECEIVING
            rval = var_valor.

        CASE var_valor.
          WHEN: 'ok' OR 'OK'.

            CLEAR: var_valor, xml_node.

            CALL METHOD xml_ret->find_node
              EXPORTING
                name = 'chave'
              RECEIVING
                node = xml_node.

            IF ( sy-subrc EQ 0 ) AND ( NOT xml_node IS INITIAL ).

              CALL METHOD xml_node->get_value
                RECEIVING
                  rval = var_valor.

              CREATE OBJECT zcl_rota_db.

              zcl_rota_db->gravar_chave( i_chave = var_valor i_grupo = i_grupo ). "Método para gravar a chave no banco de dados e manter o seu controle.

              e_chave = var_valor. "Retorna o Valor da nova CHAVE

            ELSE.
              RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>erro_no_xml.
            ENDIF.

          WHEN: OTHERS.
            MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH 'Ocorreu um erro no retorno do XML de autenticação'
                                                             var_valor.
        ENDCASE.
      ELSE.
        RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>erro_no_xml.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>autenticacao_nao_encontrado.
    ENDIF.

  ENDMETHOD.


  METHOD BUSCAR_ROTAS.

    CONSTANTS: VAR_SET_CONTRATANTE TYPE SETNAMENEW VALUE 'MAGGI_PEDAGIO_AG_PROP'. "Constante para o SET Agentes Proprio.

    DATA: GT_SETLEAF TYPE TABLE OF SETLEAF, "Internal Table para o SET
          GW_SETLEAF TYPE SETLEAF. "Work Area para o SET

    DATA: GW_XML      TYPE STRING, "String para guardar informações do XML
          GW_XML_ROTA TYPE STRING. "String para guardar informações do XML da Rota.

    DATA: VAR_CHAVE TYPE C LENGTH 32, "Variavel para guardar a CHAVE
          VAR_MSG   TYPE STRING, "Variavel para mostrar a Mensagem texto da exception.
          VAR_HTTP  TYPE REF TO IF_HTTP_CLIENT. "Interface HTTP Client

    DATA: CX_EXCEPTION TYPE REF TO ZCX_WEBSERVICE. "Referencia para a Classe de Exception.

    "Limpar tabela internal e work area.
    REFRESH: GT_SETLEAF.
    CLEAR: GW_SETLEAF.

    "Selecionar o SET para o Agente Proprio.
    SELECT * FROM SETLEAF
      INTO TABLE GT_SETLEAF
    WHERE SETNAME EQ VAR_SET_CONTRATANTE.

    IF ( SY-SUBRC EQ 0 ).

      LOOP AT GT_SETLEAF INTO GW_SETLEAF.

        TRY .

            VAR_CHAVE = ME->CHAVE_SEGURANCA( I_GRUPO = I_GRUPO ). "Recuperar a Chave de Segurança.

          CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
            VAR_MSG  = CX_EXCEPTION->GET_TEXT( ).
            MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
        ENDTRY.

        "Caso a chave não seja encontrada o loop deve continuar.
        IF ( VAR_CHAVE IS INITIAL ).
          CLEAR: GW_SETLEAF.
          CONTINUE.
        ELSE.

          "Chama o Método XML_ATUALIZ_ROTA onde é feito a montagem do arquivo
          "XML e para que possamos enviar para a TIPCARD e consultar toda as
          "rotas que precisam ser atualizadas referente ao CNPJ do Contratante.
          GW_XML = XML_ATUALIZA_ROTA( I_CHAVE = VAR_CHAVE
                                      I_CNPJCONTRATANTE = GW_SETLEAF-VALFROM
                                    ).

          TRY .

              "Atribui o serviço que precisa ser consultado.
              "PE = Pedagio.
              ME->SET_SERVICO( EXPORTING I_SERVICO = 'PE' ).

            CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
              VAR_MSG  = CX_EXCEPTION->GET_TEXT( ).
              MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
          ENDTRY.

          "Atribui o Tipo de Serviço
          "A = atualização.
          ME->SET_TIPO( EXPORTING I_TIPO = 'A').

          TRY .

              "Atribui as Informações do HTTP Client para consultar o WebService.
              VAR_HTTP = ME->URL( ).


            CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
              VAR_MSG  = CX_EXCEPTION->GET_TEXT( ).
              MESSAGE E007(ZWEBSERVICE) WITH VAR_MSG.
          ENDTRY.

          ME->ZIF_WEBSERVICE~ABRIR_CONEXAO( VAR_HTTP ).

        ENDIF.

        "Envia para Consultar a rota com as informações preenchidas acima.
        "O retorno é de um arquivo XML com todas as rotas que precisam ser atualizadas.
        GW_XML_ROTA = ME->ZIF_WEBSERVICE~CONSULTAR( I_HTTP = VAR_HTTP
                                                    I_XML  = GW_XML
                                                   ).

        ME->LER_XML_ROTAS( I_GRUPO = I_GRUPO I_XML = GW_XML_ROTA ).

        CLEAR: GW_SETLEAF, GW_XML, GW_XML_ROTA.
        CLEAR: VAR_CHAVE, VAR_HTTP, VAR_MSG.
      ENDLOOP.

    ELSE.
      "Quando não existir parametrização no SET para o contratante a mensagem abaixo devera
      "ser exibida.
      MESSAGE E007(ZWEBSERVICE) WITH 'Nenhum contratante foi cadastrado no SET' 'MAGGI_PEDAGIO_AG_PROP'.
    ENDIF.
  ENDMETHOD.


  METHOD CHAVE_SEGURANCA.
*****************
*  Descrição: Método para retornar a chave de Segurança da Tipcard.
*  Data: 07.04.2014 14:09:41
*  Developer: Victor Hugo Souza Nunes
*****************

    DATA: GW_ZLEST0086 TYPE ZLEST0086, "Controle da Chave de Autenticação TIPCARD
          CALC_HORA    TYPE I,         "Variavel para calcular a hora.
          VAR_HTTP     TYPE REF TO IF_HTTP_CLIENT,
          CX_EXCEPTION TYPE REF TO ZCX_WEBSERVICE, "Classe da Exception
          MSG          TYPE STRING. "String de Mensagem.

    CLEAR: GW_ZLEST0086, CALC_HORA.

    "Selecionar na tabela de Controle de Chave a chave atual.
    SELECT SINGLE * INTO GW_ZLEST0086
      FROM ZLEST0086
     WHERE DS_GRUPO EQ I_GRUPO.

    IF ( SY-SUBRC EQ 0 ).

      CALC_HORA = ( ( SY-UZEIT - GW_ZLEST0086-HORA ) / 60 ). "Transformar a hora cadastrada em minutos.
      IF ( CALC_HORA < 0 ).
        CALC_HORA = CALC_HORA * -1.
      ENDIF.



      "Caso a hora seja maior ou igual a 50 minutos
      "Solicitar uma nova chave ao serviço de Autenticação da Tipcard.
      IF 1 EQ 1."( CALC_HORA >= 50 ) OR ( GW_ZLEST0086-DATA NE SY-DATUM ).
        "IF ( CALC_HORA >= 50 ) OR ( GW_ZLEST0086-DATA NE SY-DATUM ).

        "TRY .

        "Atribui o serviço que precisa ser consultado.
        "PE = Pedagio.
        "TRY .
        ME->SET_SERVICO( I_SERVICO = 'VI' ).
        "  CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
        "    MSG = CX_EXCEPTION->GET_TEXT( ).
        "    MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH MSG.
        "ENDTRY.

        "Atribui o Tipo de Serviço
        "A = atualização.
        ME->SET_TIPO( I_TIPO = 'C').

        VAR_HTTP = ME->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.

        "Recuperar a Chave de Acesso cas não tenha registro na tabela de controle
        "ou o seu tempo de autorização esteja ultrapassado.

        E_CHAVE = ME->AUTENTICACAO(
                                   I_HTTP      = VAR_HTTP
                                   I_OPERADORA = 'TIP'
                                   I_GRUPO     = I_GRUPO
                                   ).


        "   CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
        "     MSG = CX_EXCEPTION->GET_TEXT( ).
        "     MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH MSG.
        " ENDTRY.
      ELSE.
        "Retornar a chave que esta cadastrada caso a mesma ainda seja menor que 50 minutos.
        E_CHAVE = GW_ZLEST0086-CHAVE.
      ENDIF.
    ELSE.
      "RAISE EXCEPTION TYPE ZCX_WEBSERVICE EXPORTING TEXTID = ZCX_WEBSERVICE=>AUTENTICACAO_NAO_ENCONTRADO.
      "TRY .
      "Atribui o serviço que precisa ser consultado.
      "PE = Pedagio.
      "TRY .
      ME->SET_SERVICO( I_SERVICO = 'VI' ).
      "  CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
      "    MSG = CX_EXCEPTION->GET_TEXT( ).
      "    MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH MSG.
      "ENDTRY.

      "Atribui o Tipo de Serviço
      "A = atualização.
      ME->SET_TIPO( I_TIPO = 'C').

      VAR_HTTP = ME->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.

      "Recuperar a Chave de Acesso cas não tenha registro na tabela de controle
      "ou o seu tempo de autorização esteja ultrapassado.

      E_CHAVE = ME->AUTENTICACAO( I_HTTP = VAR_HTTP I_OPERADORA = 'TIP' I_GRUPO = I_GRUPO ).

      "   CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
      "     MSG = CX_EXCEPTION->GET_TEXT( ).
      "     MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH MSG.
      " ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD consultar_arquivo_cobranca.

    DATA: cx_exception TYPE REF TO zcx_webservice.

    CLEAR: r_linkarquivo, e_msg.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs  EQ @i_bukrs
       AND branch EQ @i_branch.

    TRY .
        DATA(e_chave) = me->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zcx_webservice INTO cx_exception .
        DATA(lc_msg)  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    DATA(lc_xml) = me->xml_consultar_arq_cobranca( i_chave = e_chave i_bukrs = i_bukrs i_branch = i_branch  i_fatura = i_fatura ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RC - Consultar Rota.
        me->set_servico( EXPORTING i_servico = 'CB' ).

      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->set_tipo( EXPORTING i_tipo = 'B').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        DATA(lc_http) = me->url( ).
      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->zif_webservice~abrir_conexao( lc_http ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD me->zif_webservice~consultar
      EXPORTING
        i_http                     = lc_http
        i_xml                      = lc_xml
      RECEIVING
        e_resultado                = DATA(lc_msg_adm)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zwebservice.
    ENDIF.

    CLEAR: e_msg.

    me->ler_xml_consultar_arq_cobranca( EXPORTING i_xml                     = lc_msg_adm
                                        IMPORTING e_msg                     = e_msg
                                                  r_linkarquivo             = r_linkarquivo
                                                  r_linkarquivo_pedagio     = r_linkarquivo_pedagio
                                                  r_content_arquivo         = r_content_arquivo            "*-US 130492-08.04.2024-JT
                                                  r_content_arquivo_pedagio = r_content_arquivo_pedagio ). "*-US 130492-08.04.2024-JT

    IF e_msg NE 'SUCESSO'.
      MESSAGE e_msg TYPE 'E' RAISING zwebservice.
    ENDIF.

  ENDMETHOD.


  METHOD CONSULTAR_ARQUIVO_CONFERENCIA.

    DATA: CX_EXCEPTION TYPE REF TO ZCX_WEBSERVICE.

    CLEAR: R_LINKARQUIVO, E_MSG.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0160)
      FROM ZLEST0160
     WHERE BUKRS   EQ @I_BUKRS
       AND BRANCH  EQ @I_BRANCH.

    TRY .
        DATA(E_CHAVE) = ME->CHAVE_SEGURANCA( I_GRUPO = WA_ZLEST0160-DS_GRUPO ).
      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
        DATA(LC_MSG)  = CX_EXCEPTION->GET_TEXT( ).
        MESSAGE E007(ZWEBSERVICE) WITH LC_MSG.
    ENDTRY.

    DATA(LC_XML) = ME->XML_CONSULTAR_ARQ_CONFERENCIA( I_CHAVE = E_CHAVE I_BUKRS = I_BUKRS I_BRANCH = I_BRANCH I_DT_INICIAL = I_DT_INICIAL I_DT_FINAL = I_DT_FINAL ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RC - Consultar Rota.
        ME->SET_SERVICO( EXPORTING I_SERVICO = 'CA' ).

      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
        LC_MSG  = CX_EXCEPTION->GET_TEXT( ).
        MESSAGE E007(ZWEBSERVICE) WITH LC_MSG.
    ENDTRY.

    ME->SET_TIPO( EXPORTING I_TIPO = 'Q').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        DATA(LC_HTTP) = ME->URL( ).
      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
        LC_MSG  = CX_EXCEPTION->GET_TEXT( ).
        MESSAGE E007(ZWEBSERVICE) WITH LC_MSG.
    ENDTRY.

    ME->ZIF_WEBSERVICE~ABRIR_CONEXAO( LC_HTTP ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD ME->ZIF_WEBSERVICE~CONSULTAR
      EXPORTING
        I_HTTP                     = LC_HTTP
        I_XML                      = LC_XML
      RECEIVING
        E_RESULTADO                = DATA(LC_MSG_ADM)
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ZWEBSERVICE.
    ENDIF.

    CLEAR: E_MSG.

    R_LINKARQUIVO = ME->LER_XML_CONSULTAR_ARQ_CONFERE( EXPORTING I_XML = LC_MSG_ADM IMPORTING E_MSG = E_MSG ).

    IF E_MSG NE 'SUCESSO'.
      MESSAGE E_MSG TYPE 'E' RAISING ZWEBSERVICE.
    ENDIF.

  ENDMETHOD.


  METHOD consultar_rota.

    DATA: e_chave      TYPE char32,
          lc_msg       TYPE string,
          lc_msg_adm   TYPE string,
          cx_exception TYPE REF TO zcx_webservice,
          lc_xml       TYPE string,
          lc_http      TYPE REF TO if_http_client,
          wa_zlest0101 TYPE zlest0101,
          e_rotas	     TYPE	zlest0101_t,
          wa_rotas     TYPE	zlest0101,
          e_pracas     TYPE zlest0102_t,
          it_zlest0091 TYPE TABLE OF zlest0091,
          wa_zlest0091 TYPE zlest0091,
          it_zlest0102 TYPE TABLE OF zlest0102,
          wa_zlest0102 TYPE zlest0102,
          it_zlest0084 TYPE TABLE OF zlest0084,
          wa_zlest0084 TYPE zlest0084,
          lc_divisaor  TYPE i.

    FIELD-SYMBOLS: <z101> TYPE zlest0101,
                   <z102> TYPE zlest0102,
                   <z084> TYPE zlest0084.

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs  EQ @i_bukrs
       AND branch EQ @i_branch.

    SELECT SINGLE * INTO wa_zlest0101
      FROM zlest0101
     WHERE id_rota EQ i_rota
       AND bukrs   EQ i_bukrs
       AND branch  EQ i_branch.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e088(zles) WITH i_rota RAISING zwebservice.
      "elseif wa_zlest0101-id_rota_adm is not initial.
      "  message e087(zles) raising zwebservice.
    ENDIF.

    TRY .
        e_chave = me->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    lc_xml = xml_consultar_rota( i_chave = e_chave i_zlest0101 = wa_zlest0101 ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RC - Consultar Rota.
        me->set_servico( EXPORTING i_servico = 'RC' ).

      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->set_tipo( EXPORTING i_tipo = 'R').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        lc_http = me->url( ).

      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg.
    ENDTRY.

    me->zif_webservice~abrir_conexao( lc_http ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD me->zif_webservice~consultar
      EXPORTING
        i_http                     = lc_http
        i_xml                      = lc_xml
      RECEIVING
        e_resultado                = lc_msg_adm
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING zwebservice.
    ENDIF.

    CLEAR: e_msg.

    CALL METHOD me->ler_xml_consultar_rota
      EXPORTING
        i_xml    = lc_msg_adm
      IMPORTING
        e_rotas  = e_rotas
        e_pracas = e_pracas
        e_msg    = e_msg.

    IF e_msg NE 'Sucesso'.
      MESSAGE e_msg TYPE 'E' RAISING zwebservice.
    ELSE.

      SELECT SINGLE * INTO wa_zlest0101
        FROM zlest0101
       WHERE id_rota EQ i_rota
         AND bukrs   EQ i_bukrs
         AND branch  EQ i_branch.

      SELECT * INTO TABLE it_zlest0091
        FROM zlest0091.

      IF sy-subrc IS INITIAL.
        SELECT * INTO TABLE it_zlest0102
          FROM zlest0102
         WHERE id_rota EQ wa_zlest0101-id_rota
           AND bukrs   EQ wa_zlest0101-bukrs
           AND branch  EQ wa_zlest0101-branch.

        SELECT * INTO TABLE it_zlest0084
          FROM zlest0084
         WHERE id_rota EQ wa_zlest0101-id_rota_adm
           AND bukrs   EQ wa_zlest0101-bukrs
           AND branch  EQ wa_zlest0101-branch.
      ENDIF.

      READ TABLE e_rotas INDEX 1 INTO wa_rotas.
      IF sy-subrc IS INITIAL.
        IF wa_rotas-bukrs          EQ wa_zlest0101-bukrs   AND
           wa_rotas-branch         EQ wa_zlest0101-branch  AND
           wa_rotas-country        EQ wa_zlest0101-country AND
           wa_rotas-cd_cid_origem  EQ wa_zlest0101-cd_cid_origem  AND
           wa_rotas-cd_cid_destino EQ wa_zlest0101-cd_cid_destino AND
           wa_rotas-tp_rota_perc   EQ wa_zlest0101-tp_rota_perc AND
           wa_rotas-id_rota_adm    EQ wa_zlest0101-id_rota_adm.

          wa_rotas-st_rota = abap_true.
          wa_rotas-id_rota = wa_zlest0101-id_rota.

          "Atualiza Praças
          LOOP AT e_pracas ASSIGNING <z102> WHERE id_rota_adm EQ wa_zlest0101-id_rota_adm.

            IF <z102>-vl_precisao EQ 0.
              lc_divisaor = 1.
            ELSE.
              lc_divisaor = 0.
              DO <z102>-vl_precisao TIMES.
                IF lc_divisaor EQ 0.
                  lc_divisaor = 10.
                ELSE.
                  lc_divisaor = 10 * lc_divisaor.
                ENDIF.
              ENDDO.
            ENDIF.
            <z102>-bukrs     = wa_zlest0101-bukrs.
            <z102>-branch    = wa_zlest0101-branch.
            <z102>-id_rota   = wa_zlest0101-id_rota.
            <z102>-st_praca  = abap_true.
            <z102>-vl_eixo1  = <z102>-vl_eixo1  / lc_divisaor.
            <z102>-vl_eixo2  = <z102>-vl_eixo2  / lc_divisaor.
            <z102>-vl_eixo3  = <z102>-vl_eixo3  / lc_divisaor.
            <z102>-vl_eixo4  = <z102>-vl_eixo4  / lc_divisaor.
            <z102>-vl_eixo5  = <z102>-vl_eixo5  / lc_divisaor.
            <z102>-vl_eixo6  = <z102>-vl_eixo6  / lc_divisaor.
            <z102>-vl_eixo7  = <z102>-vl_eixo7  / lc_divisaor.
            <z102>-vl_eixo8  = <z102>-vl_eixo8  / lc_divisaor.
            <z102>-vl_eixo9  = <z102>-vl_eixo9  / lc_divisaor.
            <z102>-vl_eixo10 = <z102>-vl_eixo10 / lc_divisaor.
*** Inicio - Rubenilson - 17.12.24 - US160867
            <z102>-vl_tageixo1  = <z102>-vl_tageixo1 / lc_divisaor.
            <z102>-vl_tageixo2  = <z102>-vl_tageixo2 / lc_divisaor.
            <z102>-vl_tageixo3  = <z102>-vl_tageixo3 / lc_divisaor.
            <z102>-vl_tageixo4  = <z102>-vl_tageixo4 / lc_divisaor.
            <z102>-vl_tageixo5  = <z102>-vl_tageixo5 / lc_divisaor.
            <z102>-vl_tageixo6  = <z102>-vl_tageixo6 / lc_divisaor.
            <z102>-vl_tageixo7  = <z102>-vl_tageixo7 / lc_divisaor.
            <z102>-vl_tageixo8  = <z102>-vl_tageixo8 / lc_divisaor.
            <z102>-vl_tageixo9  = <z102>-vl_tageixo9 / lc_divisaor.
            <z102>-vl_tageixo10 = <z102>-vl_tageixo10 / lc_divisaor.
***Fim - Rubenilson - 17.12.24 - US160867
            READ TABLE it_zlest0102 INTO wa_zlest0102
                                    WITH KEY id_rota_adm = wa_zlest0101-id_rota_adm
                                             id_praca    = <z102>-id_praca.
            IF sy-subrc IS INITIAL.
              MODIFY it_zlest0102 INDEX sy-tabix FROM <z102>.
            ELSE.
              APPEND <z102> TO it_zlest0102.
            ENDIF.
          ENDLOOP.

          "Desativa Praças
          LOOP AT it_zlest0102 ASSIGNING <z102>.
            READ TABLE e_pracas INTO wa_zlest0102
                                WITH KEY id_rota_adm = <z102>-id_rota_adm
                                         id_praca    = <z102>-id_praca.
            IF sy-subrc IS NOT INITIAL.
              <z102>-st_praca = abap_false.
            ENDIF.
          ENDLOOP.

          wa_rotas-vl_eixo1  = 0.
          wa_rotas-vl_eixo2  = 0.
          wa_rotas-vl_eixo3  = 0.
          wa_rotas-vl_eixo4  = 0.
          wa_rotas-vl_eixo5  = 0.
          wa_rotas-vl_eixo6  = 0.
          wa_rotas-vl_eixo7  = 0.
          wa_rotas-vl_eixo8  = 0.
          wa_rotas-vl_eixo9  = 0.
          wa_rotas-vl_eixo10 = 0.
*** Inicio - Rubenilson - 17.12.24 - US160867
          wa_rotas-vl_tageixo1  = 0.
          wa_rotas-vl_tageixo2  = 0.
          wa_rotas-vl_tageixo3  = 0.
          wa_rotas-vl_tageixo4  = 0.
          wa_rotas-vl_tageixo5  = 0.
          wa_rotas-vl_tageixo6  = 0.
          wa_rotas-vl_tageixo7  = 0.
          wa_rotas-vl_tageixo8  = 0.
          wa_rotas-vl_tageixo9  = 0.
          wa_rotas-vl_tageixo10 = 0.
*** Fim - Rubenilson - 17.12.24 - US160867
          LOOP AT it_zlest0102 INTO wa_zlest0102 WHERE st_praca = abap_true AND id_rota EQ wa_rotas-id_rota.
            ADD wa_zlest0102-vl_eixo1 TO wa_rotas-vl_eixo1.
            ADD wa_zlest0102-vl_eixo2  TO wa_rotas-vl_eixo2.
            ADD wa_zlest0102-vl_eixo3  TO wa_rotas-vl_eixo3.
            ADD wa_zlest0102-vl_eixo4  TO wa_rotas-vl_eixo4.
            ADD wa_zlest0102-vl_eixo5  TO wa_rotas-vl_eixo5.
            ADD wa_zlest0102-vl_eixo6  TO wa_rotas-vl_eixo6.
            ADD wa_zlest0102-vl_eixo7  TO wa_rotas-vl_eixo7.
            ADD wa_zlest0102-vl_eixo8  TO wa_rotas-vl_eixo8.
            ADD wa_zlest0102-vl_eixo9  TO wa_rotas-vl_eixo9.
            ADD wa_zlest0102-vl_eixo10 TO wa_rotas-vl_eixo10.
*** Início - Rubenilson - 17.12.24 - US160867
            ADD wa_zlest0102-vl_tageixo1 TO wa_rotas-vl_tageixo1.
            ADD wa_zlest0102-vl_tageixo2  TO wa_rotas-vl_tageixo2.
            ADD wa_zlest0102-vl_tageixo3  TO wa_rotas-vl_tageixo3.
            ADD wa_zlest0102-vl_tageixo4  TO wa_rotas-vl_tageixo4.
            ADD wa_zlest0102-vl_tageixo5  TO wa_rotas-vl_tageixo5.
            ADD wa_zlest0102-vl_tageixo6  TO wa_rotas-vl_tageixo6.
            ADD wa_zlest0102-vl_tageixo7  TO wa_rotas-vl_tageixo7.
            ADD wa_zlest0102-vl_tageixo8  TO wa_rotas-vl_tageixo8.
            ADD wa_zlest0102-vl_tageixo9  TO wa_rotas-vl_tageixo9.
            ADD wa_zlest0102-vl_tageixo10 TO wa_rotas-vl_tageixo10.
*** Fim - Rubenilson - 17.12.24 - US160867
          ENDLOOP.

          LOOP AT it_zlest0084 ASSIGNING <z084>.
            READ TABLE it_zlest0091 INTO wa_zlest0091 WITH KEY categoria = <z084>-cat_veiculo.
            <z084>-st_rota  = wa_rotas-st_rota.
            CASE wa_zlest0091-qtd_eixo.
              WHEN 1.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo1.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo1.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo1."Rubenilson - 17.12.24 - US160867
              WHEN 2.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo2.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo2.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo2."Rubenilson - 17.12.24 - US160867
              WHEN 3.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo3.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo3.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo3."Rubenilson - 17.12.24 - US160867
              WHEN 4.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo4.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo4.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo4."Rubenilson - 17.12.24 - US160867
              WHEN 5.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo5.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo5.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo5."Rubenilson - 17.12.24 - US160867
              WHEN 6.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo6.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo6.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo6."Rubenilson - 17.12.24 - US160867
              WHEN 7.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo7.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo7.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo7."Rubenilson - 17.12.24 - US160867
              WHEN 8.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo8.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo8.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo8."Rubenilson - 17.12.24 - US160867
              WHEN 9.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo9.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo9.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo9."Rubenilson - 17.12.24 - US160867
              WHEN 10.
                <z084>-vlr_pedagio   = wa_rotas-vl_eixo10.
                <z084>-nv_vl_pedagio = wa_rotas-vl_eixo10.
                <z084>-tag_vl_pedagio =  wa_rotas-vl_tageixo10."Rubenilson - 17.12.24 - US160867
            ENDCASE.
          ENDLOOP.

          SORT it_zlest0084 BY id_rota cat_veiculo.

          LOOP AT it_zlest0091 INTO wa_zlest0091.
            READ TABLE it_zlest0084 INTO wa_zlest0084 WITH KEY id_rota     = wa_rotas-id_rota_adm
                                                               cat_veiculo = wa_zlest0091-categoria.
            IF sy-subrc IS INITIAL.
              wa_zlest0084-id_rota       = wa_rotas-id_rota_adm.

              CASE wa_zlest0091-qtd_eixo.
                WHEN 1.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo1.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo1.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo1."Rubenilson - 17.12.24 - US160867
                WHEN 2.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo2.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo2.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo2."Rubenilson - 17.12.24 - US160867
                WHEN 3.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo3.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo3.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo3."Rubenilson - 17.12.24 - US160867
                WHEN 4.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo4.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo4.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo4."Rubenilson - 17.12.24 - US160867
                WHEN 5.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo5.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo5.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo5."Rubenilson - 17.12.24 - US160867
                WHEN 6.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo6.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo6.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo6."Rubenilson - 17.12.24 - US160867
                WHEN 7.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo7.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo7.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo7."Rubenilson - 17.12.24 - US160867
                WHEN 8.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo8.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo8.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo8."Rubenilson - 17.12.24 - US160867
                WHEN 9.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo9.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo9.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo9."Rubenilson - 17.12.24 - US160867
                WHEN 10.
                  wa_zlest0084-vlr_pedagio   = wa_rotas-vl_eixo10.
                  wa_zlest0084-nv_vl_pedagio = wa_rotas-vl_eixo10.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo10."Rubenilson - 17.12.24 - US160867
              ENDCASE.

              wa_zlest0084-st_rota       = wa_rotas-st_rota.
              MODIFY it_zlest0084 INDEX sy-tabix FROM wa_zlest0084.
            ELSE.
              CLEAR: wa_zlest0084.
              wa_zlest0084-id_rota       = wa_rotas-id_rota_adm.
              wa_zlest0084-cat_veiculo   = wa_zlest0091-categoria.
              wa_zlest0084-munic_origem  = wa_rotas-cd_cid_origem+3(7).
              wa_zlest0084-munic_destino = wa_rotas-cd_cid_destino+3(7).
              wa_zlest0084-bukrs  = wa_zlest0101-bukrs.
              wa_zlest0084-branch = wa_zlest0101-branch.

              CASE wa_zlest0091-qtd_eixo.
                WHEN 1.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo1.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo1.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo1."Rubenilson - 17.12.24 - US160867
                WHEN 2.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo2.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo2.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo2."Rubenilson - 17.12.24 - US160867
                WHEN 3.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo3.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo3.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo3."Rubenilson - 17.12.24 - US160867
                WHEN 4.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo4.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo4.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo4."Rubenilson - 17.12.24 - US160867
                WHEN 5.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo5.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo5.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo5."Rubenilson - 17.12.24 - US160867
                WHEN 6.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo6.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo6.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo6."Rubenilson - 17.12.24 - US160867
                WHEN 7.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo7.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo7.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo7."Rubenilson - 17.12.24 - US160867
                WHEN 8.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo8.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo8.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo8."Rubenilson - 17.12.24 - US160867
                WHEN 9.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo9.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo9.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo9."Rubenilson - 17.12.24 - US160867
                WHEN 10.
                  wa_zlest0084-vlr_pedagio    = wa_rotas-vl_eixo10.
                  wa_zlest0084-nv_vl_pedagio  = wa_rotas-vl_eixo10.
                  wa_zlest0084-tag_vl_pedagio =  wa_rotas-vl_tageixo10."Rubenilson - 17.12.24 - US160867
              ENDCASE.

              wa_zlest0084-st_rota       = wa_rotas-st_rota.
              APPEND wa_zlest0084 TO it_zlest0084.
              SORT it_zlest0084 BY id_rota cat_veiculo.
            ENDIF.
          ENDLOOP.

          MODIFY zlest0101 FROM wa_rotas.
          MODIFY zlest0102 FROM TABLE it_zlest0102.
          MODIFY zlest0084 FROM TABLE it_zlest0084.
          COMMIT WORK.

          MESSAGE e_msg TYPE 'S'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD cons_situacao_transportador.

    DATA: lc_webservice TYPE REF TO zcl_webservice_tipcard.

    DATA: e_chave       TYPE char32,
          cx_exception  TYPE REF TO zcx_webservice,
          lc_msg        TYPE string,
          lc_xml        TYPE string,
          lc_msg_adm    TYPE string,
          it_consultas  TYPE TABLE OF zde_consulta_rntrc,
          wa_consultas  TYPE zde_consulta_rntrc,
          wa_j_1bbranch TYPE j_1bbranch,
          wa_lfa1       TYPE lfa1,
          it_lfa1       TYPE TABLE OF lfa1,
          lc_http       TYPE REF TO if_http_client,
          wa_zlest0135  TYPE zlest0135,
          it_zlest0135  TYPE TABLE OF zlest0135,
          it_veiculos   TYPE TABLE OF zlest0002,
          wa_veiculos   TYPE zlest0002,
          wa_lifnr      TYPE range_lifnr,
          it_lifnr      TYPE TABLE OF range_lifnr,
          wa_placa      TYPE zde_placa_range,
          it_placa      TYPE TABLE OF zde_placa_range,
          i_name_file   TYPE string,
          wa_zlest0137  TYPE zlest0137,
          i_parceiro    TYPE j_1bparid.

    SELECT SINGLE * INTO wa_zlest0137 FROM zlest0137.

    IF wa_zlest0137-ck_consulta IS NOT INITIAL OR i_ck_consulta IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_zlest0160)
        FROM zlest0160
       WHERE bukrs   EQ @i_bukrs
         AND branch  EQ @i_branch.

      CREATE OBJECT lc_webservice.

      TRY .
          e_chave = lc_webservice->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
        CATCH zcx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.

      SELECT SINGLE * INTO wa_j_1bbranch FROM j_1bbranch
        WHERE bukrs  EQ i_bukrs
          AND branch EQ i_branch.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE e112(zles) WITH i_bukrs i_branch RAISING erro.

        IF wa_j_1bbranch IS INITIAL.

          i_parceiro = wa_j_1bbranch-branch.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = i_parceiro
            IMPORTING
              output = i_parceiro.

          SELECT SINGLE * INTO @DATA(wa_lfa1_01)
            FROM lfa1
           WHERE lifnr EQ @i_parceiro.

          wa_j_1bbranch-stcd1 = wa_lfa1_01-stcd1.
        ENDIF.
      ENDIF.

      IF ( i_partiner IS INITIAL OR i_placa IS INITIAL ) AND ( i_pesquisa_livre IS INITIAL ).

        CLEAR: it_lifnr.

        IF i_partiner IS NOT INITIAL.
          wa_lifnr-sign   = 'I'.
          wa_lifnr-option = 'EQ'.
          wa_lifnr-low    = i_partiner.
          wa_lifnr-high   = i_partiner.
          APPEND wa_lifnr TO it_lifnr.
        ENDIF.

        IF i_placa IS NOT INITIAL.
          wa_placa-sign   = 'I'.
          wa_placa-option = 'EQ'.
          wa_placa-low    = i_placa.
          wa_placa-high   = i_placa.
          APPEND wa_placa TO it_placa.
        ENDIF.

        SELECT * INTO TABLE it_veiculos
          FROM zlest0002
         WHERE proprietario IN it_lifnr
           AND pc_veiculo   IN it_placa.

        IF sy-subrc IS NOT INITIAL AND i_placa IS INITIAL AND i_partiner IS NOT INITIAL.
          MESSAGE e118(zles) WITH i_partiner RAISING erro.
        ENDIF.

        IF sy-subrc IS NOT INITIAL AND i_placa IS NOT INITIAL AND i_partiner IS NOT INITIAL.
          MESSAGE e117(zles) WITH i_placa i_partiner RAISING erro.
        ENDIF.

        IF sy-subrc IS NOT INITIAL AND i_placa IS NOT INITIAL AND i_partiner IS INITIAL.
          MESSAGE e116(zles) WITH i_placa RAISING erro.
        ENDIF.

        CHECK it_veiculos IS NOT INITIAL.

        SELECT * INTO TABLE it_lfa1
          FROM lfa1
           FOR ALL ENTRIES IN it_veiculos
         WHERE lifnr EQ it_veiculos-proprietario.

        SORT it_lfa1 BY lifnr.

        LOOP AT it_veiculos INTO wa_veiculos.

          CLEAR: wa_consultas, wa_zlest0135.

          wa_consultas-cd_transportador = wa_veiculos-proprietario.
          wa_consultas-ds_placa         = wa_veiculos-pc_veiculo.

          "Fornecedor
          READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_veiculos-proprietario BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE e113(zles) WITH i_partiner RAISING erro.
          ENDIF.

          "RNTRC
          IF ( wa_lfa1-bahns IS INITIAL ) AND ( wa_zlest0135-ds_msg_transportador IS INITIAL ).
            MESSAGE e114(zles) WITH i_partiner RAISING erro.
          ENDIF.

          IF wa_zlest0135-ds_msg_transportador IS INITIAL.
            wa_consultas-cnpj_contratante = wa_j_1bbranch-stcd1.
            CASE wa_lfa1-stkzn."sfrgr. "Ajuste realizado U.S #117449 AOENNING.
              WHEN abap_true.
                wa_consultas-cnpj_contratado  = wa_lfa1-stcd2.
              WHEN abap_false.
                wa_consultas-cnpj_contratado  = wa_lfa1-stcd1.
            ENDCASE.
            wa_consultas-rntrc            = wa_lfa1-bahns.
            APPEND wa_consultas TO it_consultas.
          ELSE.
            "Não fará consulta WebService
            APPEND wa_zlest0135 TO it_zlest0135.
          ENDIF.

        ENDLOOP.

      ELSE.
        IF i_pesquisa_livre IS INITIAL.
          SELECT SINGLE * INTO wa_lfa1 FROM lfa1 WHERE lifnr EQ i_partiner.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE e113(zles) WITH i_partiner RAISING erro.
          ENDIF.

          IF wa_lfa1-bahns IS INITIAL.
            MESSAGE e114(zles) WITH i_partiner RAISING erro.
          ENDIF.
          wa_consultas-cd_transportador = i_partiner.
          wa_consultas-ds_placa         = i_placa.
          wa_consultas-cnpj_contratante = wa_j_1bbranch-stcd1.

          CASE wa_lfa1-sfrgr.
            WHEN abap_true.
              wa_consultas-cnpj_contratado  = wa_lfa1-stcd2.
            WHEN abap_false.
              wa_consultas-cnpj_contratado  = wa_lfa1-stcd1.
          ENDCASE.

          wa_consultas-rntrc            = wa_lfa1-bahns.
          APPEND wa_consultas TO it_consultas.
        ELSE.
          wa_consultas-ds_placa         = i_placa.
          wa_consultas-cnpj_contratante = wa_j_1bbranch-stcd1.
          wa_consultas-cnpj_contratado  = i_cnpj.
          wa_consultas-rntrc            = i_rntrc.
          APPEND wa_consultas TO it_consultas.
        ENDIF.

      ENDIF.

      TRY .
          lc_webservice->set_servico( EXPORTING i_servico = 'ST' ).
        CATCH zcx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.

      lc_webservice->set_tipo( EXPORTING i_tipo = 'S').

      TRY .
          lc_http = lc_webservice->url( ).
        CATCH zcx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.

      LOOP AT it_consultas INTO wa_consultas.

        CLEAR: lc_msg_adm.

        lc_webservice->zif_webservice~abrir_conexao( lc_http ).

        lc_xml = lc_webservice->xml_consulta_transportador( EXPORTING i_chave = e_chave i_consulta_rntrc = wa_consultas ).

        IF lc_webservice->ck_salvar_xml_local EQ abap_true.
          i_name_file = 'C:\Maggi\TipFrete\consTransportadorReq.xml'.
          CALL METHOD lc_webservice->salva_xml
            EXPORTING
              i_name_file = i_name_file
              i_xml       = lc_xml.
        ENDIF.

        CALL METHOD lc_webservice->zif_webservice~consultar
          EXPORTING
            i_http                     = lc_http
            i_xml                      = lc_xml
          RECEIVING
            e_resultado                = lc_msg_adm
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING webservice.
        ENDIF.

        IF lc_webservice->ck_salvar_xml_local EQ abap_true.
          i_name_file = 'C:\Maggi\TipFrete\consTransportadorResp.xml'.
          CALL METHOD lc_webservice->salva_xml
            EXPORTING
              i_name_file = i_name_file
              i_xml       = lc_msg_adm.
        ENDIF.

        wa_zlest0135 = lc_webservice->ler_xml_situacao_transportador( EXPORTING i_xml = lc_msg_adm ).
        wa_zlest0135-cd_transportador = wa_consultas-cd_transportador.
        wa_zlest0135-ds_placa         = wa_consultas-ds_placa.
        wa_zlest0135-dt_atualizacao   = sy-datum.
        wa_zlest0135-hr_atualizacao   = sy-uzeit.
        APPEND wa_zlest0135 TO it_zlest0135.

      ENDLOOP.

      IF it_zlest0135 IS NOT INITIAL AND i_pesquisa_livre IS INITIAL.
        MOVE it_zlest0135[] TO e_consultas[].
        MODIFY zlest0135 FROM TABLE it_zlest0135.
        "COMMIT WORK.
      ELSE.
        MOVE it_zlest0135[] TO e_consultas[].
      ENDIF.
    ELSE.
      wa_zlest0135-ck_rntrc_ativo = abap_true.
      wa_zlest0135-ck_sem_parar   = abap_true.

      IF ( i_placa IS NOT INITIAL ).

        wa_placa-sign   = 'I'.
        wa_placa-option = 'EQ'.
        wa_placa-low    = i_placa.
        wa_placa-high   = i_placa.
        APPEND wa_placa TO it_placa.

        SELECT * INTO TABLE it_veiculos
          FROM zlest0002
         WHERE pc_veiculo IN it_placa.

        IF sy-subrc IS INITIAL.
          READ TABLE it_veiculos INTO wa_veiculos INDEX 1.
          wa_zlest0135-cd_transportador = wa_veiculos-proprietario.
          wa_zlest0135-ds_placa         = wa_veiculos-pc_veiculo.
        ENDIF.

      ENDIF.

      APPEND wa_zlest0135 TO e_consultas.
    ENDIF.

  ENDMETHOD.


  METHOD ler_xml_atualizar_rota.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_document      TYPE REF TO if_ixml_document,

          if_node          TYPE REF TO if_ixml_node,
          if_map           TYPE REF TO if_ixml_named_node_map,
          if_attr          TYPE REF TO if_ixml_node,

          iterator         TYPE REF TO if_ixml_node_iterator,
          iterator_pracas  TYPE REF TO if_ixml_node_iterator,
          iterator_pracas2 TYPE REF TO if_ixml_node_iterator,
          tag_name         TYPE string,
          name_dom         TYPE string,
          count_dom        TYPE i,
          index_dom        TYPE i,
          prefix_dom       TYPE string,
          valor_dom        TYPE string,

          filho            TYPE REF TO if_ixml_node_list,
          if_node_filho    TYPE REF TO if_ixml_node,
          if_map_filho     TYPE REF TO if_ixml_named_node_map,
          count_dom_filho  TYPE i,
          valor_filho      TYPE string,

          filho2           TYPE REF TO if_ixml_node_list,
          if_node_filho2   TYPE REF TO if_ixml_node,
          if_map_filho2    TYPE REF TO if_ixml_named_node_map,
          count_dom_filho2 TYPE i,
          valor_filho2     TYPE string,

          wa_rotas         TYPE zlest0101,
          wa_pracas        TYPE zlest0102,
          wa_j_1bbranch    TYPE j_1bbranch,
          wa_j_1btxjur     TYPE j_1btxjur.

    CLEAR: e_rotas, e_pracas.

    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).

    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory
                                               istream        = if_stream
                                               document       = if_document ).
    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).

    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node  = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.

        CASE if_node->get_type( ).

          WHEN: if_ixml_node=>co_node_element.

            tag_name = if_node->get_name( ).
            if_map   = if_node->get_attributes( ).

            IF NOT ( if_map IS INITIAL ).

              count_dom = if_map->get_length( ).

              DO count_dom TIMES.

                index_dom  = sy-index - 1.
                if_attr    = if_map->get_item( index_dom ).
                name_dom   = if_attr->get_name( ).
                prefix_dom = if_attr->get_namespace_prefix( ).
                valor_dom  = if_attr->get_value( ).

                "VALOR_DOM.
              ENDDO.

              CASE tag_name.
                WHEN: 'msg'.
                  e_msg = if_node->get_value( ).
                WHEN: 'cnpjContratante'.
                  CLEAR: wa_rotas.
                  valor_dom = if_node->get_value( ).
                  SELECT SINGLE * INTO wa_j_1bbranch
                    FROM j_1bbranch
                   WHERE stcd1 EQ valor_dom.
                  IF sy-subrc IS INITIAL.
                    wa_rotas-bukrs  = wa_j_1bbranch-bukrs.
                    wa_rotas-branch = wa_j_1bbranch-branch.
                  ENDIF.
                WHEN: 'codigoRota'.
                  wa_rotas-id_rota_adm = if_node->get_value( ).
                WHEN: 'codigoCidadeOrigem'.
                  valor_dom = if_node->get_value( ).
                  CONCATENATE '%' valor_dom INTO valor_dom.
                  SELECT SINGLE * INTO wa_j_1btxjur
                    FROM j_1btxjur AS a
                   WHERE country EQ 'BR'
                     AND taxjurcode LIKE valor_dom
                     AND EXISTS ( SELECT taxjurcode
                                    FROM j_1btreg_city AS b
                                   WHERE b~taxjurcode = a~taxjurcode ).
                  IF sy-subrc IS INITIAL.
                    wa_rotas-country       = wa_j_1btxjur-country.
                    wa_rotas-cd_cid_origem = wa_j_1btxjur-taxjurcode.
                  ENDIF.
                WHEN: 'codigoCidadeDestino'.
                  valor_dom = if_node->get_value( ).
                  CONCATENATE '%' valor_dom INTO valor_dom.
                  SELECT SINGLE * INTO wa_j_1btxjur
                    FROM j_1btxjur AS a
                   WHERE country EQ 'BR'
                     AND taxjurcode LIKE valor_dom
                     AND EXISTS ( SELECT taxjurcode
                                    FROM j_1btreg_city AS b
                                   WHERE b~taxjurcode = a~taxjurcode ).
                  IF sy-subrc IS INITIAL.
                    wa_rotas-country        = wa_j_1btxjur-country.
                    wa_rotas-cd_cid_destino = wa_j_1btxjur-taxjurcode.
                  ENDIF.
                WHEN: 'retornoOrigem'.
                  wa_rotas-tp_rota_perc = if_node->get_value( ).
                  APPEND wa_rotas TO e_rotas.
                WHEN: 'pracasPedagio'.
                  filho           = if_node->get_children( ).
                  iterator_pracas = filho->create_iterator( ).
                  if_node_filho   = iterator_pracas->get_next( ).

                  WHILE NOT if_node_filho IS INITIAL.
                    filho2           = if_node_filho->get_children( ).
                    iterator_pracas2 = filho2->create_iterator( ).
                    if_node_filho2   = iterator_pracas2->get_next( ).

                    WHILE NOT if_node_filho2 IS INITIAL.
                      tag_name = if_node_filho2->get_name( ).
                      CASE if_node_filho2->get_type( ).
                        WHEN: if_ixml_node=>co_node_element.
                          tag_name     = if_node_filho2->get_name( ).
                          if_map_filho2 = if_node_filho2->get_attributes( ).
                          IF NOT ( if_map_filho2 IS INITIAL ).
                            CASE tag_name.
                              WHEN: 'codigoPracaAilog'.
                                valor_dom = if_node_filho2->get_value( ).
                                CLEAR: wa_pracas.
                                wa_pracas-id_rota_adm = wa_rotas-id_rota_adm.
                                MOVE valor_dom TO wa_pracas-id_praca.
                              WHEN: 'codigoPracaSemParar'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-id_praca_sem_par.
                              WHEN: 'nomePraca'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-nm_praca.
                              WHEN: 'eixo1'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo1.
                              WHEN: 'eixo2'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo2.
                              WHEN: 'eixo3'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo3.
                              WHEN: 'eixo4'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo4.
                              WHEN: 'eixo5'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo5.
                              WHEN: 'eixo6'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo6.
                              WHEN: 'eixo7'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo7.
                              WHEN: 'eixo8'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo8.
                              WHEN: 'eixo9'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo9.
                              WHEN: 'eixo10'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo10.
                              WHEN: 'precisaoEixo'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_precisao.
                                APPEND wa_pracas TO e_pracas.
*** Inicio - Rubenilson - 17.12.24 - US160867
                              WHEN: 'tageixo1'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo1.
                              WHEN: 'tageixo2'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo2.
                              WHEN: 'tageixo3'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo3.
                              WHEN: 'tageixo4'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo4.
                              WHEN: 'tageixo5'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo5.
                              WHEN: 'tageixo6'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo6.
                              WHEN: 'tageixo7'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo7.
                              WHEN: 'tageixo8'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo8.
                              WHEN: 'tageixo9'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo9.
                              WHEN: 'tageixo10'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo10.
*** Fim - Rubenilson - 17.12.24 - US160867
                            ENDCASE.
                          ENDIF.
                      ENDCASE.
                      if_node_filho2 = iterator_pracas2->get_next( ).
                    ENDWHILE.
                    if_node_filho = iterator_pracas->get_next( ).
                  ENDWHILE.
              ENDCASE.
            ENDIF.
        ENDCASE.
        if_node = iterator->get_next( ).
      ENDWHILE.
    ENDIF.

*<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
*<retornoAtualizaRota>
*    <msg>Sucesso</msg>
*    <rotas>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>523</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1002</codigoRota>
*            <codigoCidadeOrigem>2915353</codigoCidadeOrigem>
*            <codigoCidadeDestino>3550308</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*            <pracasPedagio>
*                <praca>
*                    <codigoPraca>886</codigoPraca>
*                    <nomePraca>Vitória da Conquista</nomePraca>
*                    <eixo1>340</eixo1>
*                    <eixo2>680</eixo2>
*                    <eixo3>1020</eixo3>
*                    <eixo4>1360</eixo4>
*                    <eixo5>1700</eixo5>
*                    <eixo6>2040</eixo6>
*                    <eixo7>2380</eixo7>
*                    <eixo8>2720</eixo8>
*                    <eixo9>3060</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>772</codigoPraca>
*                    <nomePraca>Itatiaiaçú</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>771</codigoPraca>
*                    <nomePraca>Carmópolis de Minas</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>770</codigoPraca>
*                    <nomePraca>Santo Antonio do Amparo</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>769</codigoPraca>
*                    <nomePraca>Carmo da Cachoeira</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>768</codigoPraca>
*                    <nomePraca>São Gonçalo do Sapucaí</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>767</codigoPraca>
*                    <nomePraca>Cambuí</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>766</codigoPraca>
*                    <nomePraca>Vargem</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*                <praca>
*                    <codigoPraca>765</codigoPraca>
*                    <nomePraca>Mairiporã</nomePraca>
*                    <eixo1>160</eixo1>
*                    <eixo2>320</eixo2>
*                    <eixo3>480</eixo3>
*                    <eixo4>640</eixo4>
*                    <eixo5>800</eixo5>
*                    <eixo6>960</eixo6>
*                    <eixo7>1120</eixo7>
*                    <eixo8>1280</eixo8>
*                    <eixo9>1440</eixo9>
*                </praca>
*            </pracasPedagio>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1003</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1004</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1005</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*        <rota>
*            <cnpjContratante>77294254001670</cnpjContratante>
*            <codigoRota>1006</codigoRota>
*            <codigoCidadeOrigem>5107602</codigoCidadeOrigem>
*            <codigoCidadeDestino>5106372</codigoCidadeDestino>
*            <retornoOrigem>0</retornoOrigem>
*        </rota>
*    </rotas>
*</retornoAtualizaRota>


  ENDMETHOD.


  METHOD ler_xml_consultar_arq_cobranca.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_document      TYPE REF TO if_ixml_document,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_node          TYPE REF TO if_ixml_node,
          iterator         TYPE REF TO if_ixml_node_iterator.

    DATA: tag_name      TYPE string,
          valor_dom     TYPE string,
          filho         TYPE REF TO if_ixml_node_list,
          iterator2     TYPE REF TO if_ixml_node_iterator,
          if_node_filho TYPE REF TO if_ixml_node,
*-US 130492-08.04.2024-JT-inicio
          xml_ret       TYPE REF TO cl_xml_document,
          xml_ret_itm   TYPE REF TO cl_xml_document,
          xml_node_root TYPE REF TO if_ixml_node,
          xml_node      TYPE REF TO if_ixml_node,
          v_tamanhoi    TYPE i.
*-US 130492-08.04.2024-JT-fim

    CLEAR: r_linkarquivo, r_linkarquivo_pedagio, r_content_arquivo, r_content_arquivo_pedagio.

    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).
    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory istream = if_stream document = if_document ).
    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).

    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node  = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.
        CASE if_node->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            tag_name = if_node->get_name( ).

            CASE tag_name.
              WHEN 'retornoArquivoCobrancaPedagio'.
                filho         = if_node->get_children( ).
                iterator2     = filho->create_iterator( ).
                if_node_filho = iterator2->get_next( ).

                WHILE NOT if_node_filho IS INITIAL.
                  tag_name  = if_node_filho->get_name( ).
                  valor_dom = if_node_filho->get_value( ).
                  CASE tag_name.
                    WHEN 'mensagem'.
                      TRANSLATE valor_dom TO UPPER CASE.
                      e_msg = valor_dom.
*-US 130492-08.04.2024-JT-inicio
                    WHEN 'linkArquivoCobranca'.
                      r_linkarquivo = valor_dom.
                    WHEN 'linkArquivoPedagio'.
                      r_linkarquivo_pedagio = valor_dom.
*-US 130492-08.04.2024-JT-fim
                  ENDCASE.
                  if_node_filho = iterator2->get_next( ).
                ENDWHILE.
            ENDCASE.
        ENDCASE.

        if_node = iterator->get_next( ).
      ENDWHILE.

*-US 130492-08.04.2024-JT-inicio ----------------------------------------
      CREATE OBJECT xml_ret.

      IF r_linkarquivo IS NOT INITIAL AND r_linkarquivo(4) <> 'http'.
        CALL METHOD xml_ret->parse_string
          EXPORTING
            stream  = i_xml
          RECEIVING
            retcode = v_tamanhoi.

        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'arquivoCobranca'
          RECEIVING
            node = xml_node.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
          iterator = xml_node->create_iterator( ).
          xml_node = iterator->get_next( ).
          WHILE NOT xml_node IS INITIAL.
            CASE xml_node->get_type( ).
              WHEN: if_ixml_node=>co_node_element.
                tag_name = xml_node->get_name( ).
                IF tag_name EQ 'nome'.
                  r_linkarquivo = xml_node->get_value( ).
                ENDIF.
                IF tag_name EQ 'dados'.
                  r_content_arquivo = xml_node->get_value( ).
                ENDIF.
            ENDCASE.
            xml_node = iterator->get_next( ).
          ENDWHILE.
        ENDIF.
      ENDIF.

      IF r_linkarquivo_pedagio IS NOT INITIAL AND r_linkarquivo_pedagio(4) <> 'http'.
        CALL METHOD xml_ret->find_node
          EXPORTING
            name = 'arquivoPedagio'
          RECEIVING
            node = xml_node.

        IF ( sy-subrc IS INITIAL ) AND ( NOT xml_node IS INITIAL ).
          iterator = xml_node->create_iterator( ).
          xml_node = iterator->get_next( ).
          WHILE NOT xml_node IS INITIAL.
            CASE xml_node->get_type( ).
              WHEN: if_ixml_node=>co_node_element.
                tag_name = xml_node->get_name( ).
                IF tag_name EQ 'nome'.
                  r_linkarquivo_pedagio = xml_node->get_value( ).
                ENDIF.
                IF tag_name EQ 'dados'.
                  r_content_arquivo_pedagio = xml_node->get_value( ).
                ENDIF.
            ENDCASE.
            xml_node = iterator->get_next( ).
          ENDWHILE.
        ENDIF.
      ENDIF.
*-US 130492-08.04.2024-JT-fim -------------------------------------------

    ENDIF.

  ENDMETHOD.


  METHOD LER_XML_CONSULTAR_ARQ_CONFERE.

    DATA: IF_XML           TYPE REF TO IF_IXML,
          IF_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,
          IF_STREAMFACTORY TYPE REF TO IF_IXML_STREAM_FACTORY,
          IF_STREAM        TYPE REF TO IF_IXML_ISTREAM,
          IF_XML_PARSER    TYPE REF TO IF_IXML_PARSER,
          IF_NODE          TYPE REF TO IF_IXML_NODE,
          ITERATOR         TYPE REF TO IF_IXML_NODE_ITERATOR.

    DATA: TAG_NAME      TYPE STRING,
          VALOR_DOM     TYPE STRING,
          FILHO         TYPE REF TO IF_IXML_NODE_LIST,
          ITERATOR2     TYPE REF TO IF_IXML_NODE_ITERATOR,
          IF_NODE_FILHO TYPE REF TO IF_IXML_NODE.

    CLEAR: R_LINKARQUIVO.

    IF_XML           = CL_IXML=>CREATE( ).
    IF_DOCUMENT      = IF_XML->CREATE_DOCUMENT( ).
    IF_STREAMFACTORY = IF_XML->CREATE_STREAM_FACTORY( ).
    IF_STREAM        = IF_STREAMFACTORY->CREATE_ISTREAM_STRING( I_XML ).
    IF_XML_PARSER    = IF_XML->CREATE_PARSER(  STREAM_FACTORY = IF_STREAMFACTORY ISTREAM = IF_STREAM DOCUMENT = IF_DOCUMENT ).
    IF_XML_PARSER->PARSE( ).

    IF_NODE ?= IF_DOCUMENT->GET_ROOT_ELEMENT( ).

    IF NOT ( IF_NODE IS INITIAL ).

      ITERATOR = IF_NODE->CREATE_ITERATOR( ).
      IF_NODE  = ITERATOR->GET_NEXT( ).

      WHILE NOT IF_NODE IS INITIAL.
        CASE IF_NODE->GET_TYPE( ).
          WHEN: IF_IXML_NODE=>CO_NODE_ELEMENT.
            TAG_NAME = IF_NODE->GET_NAME( ).

            CASE TAG_NAME.
              WHEN 'respostaArquivoConferencia'.
                FILHO         = IF_NODE->GET_CHILDREN( ).
                ITERATOR2     = FILHO->CREATE_ITERATOR( ).
                IF_NODE_FILHO = ITERATOR2->GET_NEXT( ).

                WHILE NOT IF_NODE_FILHO IS INITIAL.
                  TAG_NAME  = IF_NODE_FILHO->GET_NAME( ).
                  VALOR_DOM = IF_NODE_FILHO->GET_VALUE( ).
                  CASE TAG_NAME.
                    WHEN 'codigoMensagem'.
                      E_CODIGO = VALOR_DOM.
                    WHEN 'mensagem'.
                      TRANSLATE VALOR_DOM TO UPPER CASE.
                      E_MSG = VALOR_DOM.
                    WHEN 'linkArquivo'.
                      R_LINKARQUIVO = VALOR_DOM.
                  ENDCASE.
                  IF_NODE_FILHO = ITERATOR2->GET_NEXT( ).
                ENDWHILE.
            ENDCASE.
        ENDCASE.

        IF_NODE = ITERATOR->GET_NEXT( ).
      ENDWHILE.

    ENDIF.

  ENDMETHOD.


  METHOD ler_xml_consultar_rota.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_document      TYPE REF TO if_ixml_document,

          if_node          TYPE REF TO if_ixml_node,
          if_map           TYPE REF TO if_ixml_named_node_map,
          if_attr          TYPE REF TO if_ixml_node,

          iterator         TYPE REF TO if_ixml_node_iterator,
          iterator_pracas  TYPE REF TO if_ixml_node_iterator,
          iterator_pracas2 TYPE REF TO if_ixml_node_iterator,
          tag_name         TYPE string,
          name_dom         TYPE string,
          count_dom        TYPE i,
          index_dom        TYPE i,
          prefix_dom       TYPE string,
          valor_dom        TYPE string,

          filho            TYPE REF TO if_ixml_node_list,
          if_node_filho    TYPE REF TO if_ixml_node,
          if_map_filho     TYPE REF TO if_ixml_named_node_map,
          count_dom_filho  TYPE i,
          valor_filho      TYPE string,

          filho2           TYPE REF TO if_ixml_node_list,
          if_node_filho2   TYPE REF TO if_ixml_node,
          if_map_filho2    TYPE REF TO if_ixml_named_node_map,
          count_dom_filho2 TYPE i,
          valor_filho2     TYPE string,

          wa_rotas         TYPE zlest0101,
          wa_pracas        TYPE zlest0102,
          wa_j_1bbranch    TYPE j_1bbranch,
*          wa_j_1btreg_city TYPE j_1btreg_city. "Incluindo nova seleção na tabela devido estar buscando mais de um valor domicio fiscal. US 109147 / AOENNING.
          wa_j_1btxjur     TYPE j_1btxjur. "Comentado devido altaração na seleção esta buscando mais de um valor domicio fiscal. US 109147 / AOENNING.
    CLEAR: e_rotas, e_pracas.

    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).

    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory
                                               istream        = if_stream
                                               document       = if_document ).
    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).

    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node  = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.

        CASE if_node->get_type( ).

          WHEN: if_ixml_node=>co_node_element.

            tag_name = if_node->get_name( ).
            if_map   = if_node->get_attributes( ).

            IF NOT ( if_map IS INITIAL ).

              count_dom = if_map->get_length( ).

              DO count_dom TIMES.

                index_dom  = sy-index - 1.
                if_attr    = if_map->get_item( index_dom ).
                name_dom   = if_attr->get_name( ).
                prefix_dom = if_attr->get_namespace_prefix( ).
                valor_dom  = if_attr->get_value( ).

                "VALOR_DOM.
              ENDDO.

              CASE tag_name.
                WHEN: 'msg'.
                  e_msg = if_node->get_value( ).
                WHEN: 'cnpjContratante'.
                  CLEAR: wa_rotas.
                  valor_dom = if_node->get_value( ).
                  SELECT SINGLE * INTO wa_j_1bbranch
                    FROM j_1bbranch
                   WHERE stcd1 EQ valor_dom.
                  IF sy-subrc IS INITIAL.
                    wa_rotas-bukrs  = wa_j_1bbranch-bukrs.
                    wa_rotas-branch = wa_j_1bbranch-branch.
                  ENDIF.
                WHEN: 'codigoRota'.
                  wa_rotas-id_rota_adm = if_node->get_value( ).
                WHEN: 'codigoCidadeOrigem'.
                  valor_dom = if_node->get_value( ).
                  CONCATENATE '%' valor_dom INTO valor_dom.
                  CLEAR: wa_j_1btxjur.
                  SELECT SINGLE * INTO wa_j_1btxjur
                    FROM j_1btxjur AS a
                   WHERE country EQ 'BR'
                     AND taxjurcode LIKE valor_dom
                     AND EXISTS ( SELECT taxjurcode
                                    FROM j_1btreg_city AS b
                                   WHERE b~taxjurcode = a~taxjurcode ).
                  IF sy-subrc IS INITIAL.
                    wa_rotas-country       = wa_j_1btxjur-country.
                    wa_rotas-cd_cid_origem = wa_j_1btxjur-taxjurcode.
                  ENDIF.
                WHEN: 'codigoCidadeDestino'.
                  valor_dom = if_node->get_value( ).
                  CONCATENATE '%' valor_dom INTO valor_dom.
                  CLEAR: wa_j_1btxjur.
                  SELECT SINGLE * INTO wa_j_1btxjur
                    FROM j_1btxjur AS a
                   WHERE country EQ 'BR'
                     AND taxjurcode LIKE valor_dom
                     AND EXISTS ( SELECT taxjurcode
                                    FROM j_1btreg_city AS b
                                   WHERE b~taxjurcode = a~taxjurcode ).
                  IF sy-subrc IS INITIAL.
                    wa_rotas-country        = wa_j_1btxjur-country.
                    wa_rotas-cd_cid_destino = wa_j_1btxjur-taxjurcode.
                  ENDIF.
                WHEN: 'retornoOrigem'.
                  wa_rotas-tp_rota_perc = if_node->get_value( ).
                  APPEND wa_rotas TO e_rotas.
                WHEN: 'pracasPedagio'.
                  filho           = if_node->get_children( ).
                  iterator_pracas = filho->create_iterator( ).
                  if_node_filho   = iterator_pracas->get_next( ).

                  WHILE NOT if_node_filho IS INITIAL.
                    filho2           = if_node_filho->get_children( ).
                    iterator_pracas2 = filho2->create_iterator( ).
                    if_node_filho2   = iterator_pracas2->get_next( ).

                    WHILE NOT if_node_filho2 IS INITIAL.
                      tag_name = if_node_filho2->get_name( ).
                      CASE if_node_filho2->get_type( ).
                        WHEN: if_ixml_node=>co_node_element.
                          tag_name     = if_node_filho2->get_name( ).
                          if_map_filho2 = if_node_filho2->get_attributes( ).
                          IF NOT ( if_map_filho2 IS INITIAL ).
                            CASE tag_name.
                              WHEN: 'codigoPracaAilog'.
                                valor_dom = if_node_filho2->get_value( ).
                                CLEAR: wa_pracas.
                                wa_pracas-id_rota_adm = wa_rotas-id_rota_adm.
                                MOVE valor_dom TO wa_pracas-id_praca.
                              WHEN 'codigoPracaSemParar'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-id_praca_sem_par.
                              WHEN: 'nomePraca'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-nm_praca.
                              WHEN: 'eixo1'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo1.
                              WHEN: 'eixo2'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo2.
                              WHEN: 'eixo3'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo3.
                              WHEN: 'eixo4'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo4.
                              WHEN: 'eixo5'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo5.
                              WHEN: 'eixo6'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo6.
                              WHEN: 'eixo7'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo7.
                              WHEN: 'eixo8'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo8.
                              WHEN: 'eixo9'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo9.
                              WHEN: 'eixo10'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_eixo10.
                              WHEN: 'precisaoEixo'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_precisao.
                                APPEND wa_pracas TO e_pracas.
*** Inicio - Rubenilson - 17.12.24 - US160867
                              WHEN: 'tageixo1'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo1.
                              WHEN: 'tageixo2'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo2.
                              WHEN: 'tageixo3'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo3.
                              WHEN: 'tageixo4'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo4.
                              WHEN: 'tageixo5'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo5.
                              WHEN: 'tageixo6'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo6.
                              WHEN: 'tageixo7'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo7.
                              WHEN: 'tageixo8'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo8.
                              WHEN: 'tageixo9'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo9.
                              WHEN: 'tageixo10'.
                                valor_dom = if_node_filho2->get_value( ).
                                MOVE valor_dom TO wa_pracas-vl_tageixo10.
*** Fim - Rubenilson - 17.12.24 - US160867
                            ENDCASE.
                          ENDIF.
                      ENDCASE.
                      if_node_filho2 = iterator_pracas2->get_next( ).
                    ENDWHILE.
                    if_node_filho = iterator_pracas->get_next( ).
                  ENDWHILE.
              ENDCASE.
            ENDIF.
        ENDCASE.
        if_node = iterator->get_next( ).
      ENDWHILE.
    ENDIF.

*<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
*<retornoConsultaRota>
*    <msg>Sucesso</msg>
*    <cnpjContratante>77294254001670</cnpjContratante>
*    <codigoRota>1007</codigoRota>
*    <codigoCidadeOrigem>5107925</codigoCidadeOrigem>
*    <codigoCidadeDestino>5107602</codigoCidadeDestino>
*    <retornoOrigem>0</retornoOrigem>
*    <pracasPedagio>
*        <praca>
*            <codigoPraca>849</codigoPraca>
*            <nomePraca>Lucas do Rio Verde</nomePraca>
*            <eixo1>430</eixo1>
*            <eixo2>860</eixo2>
*            <eixo3>1290</eixo3>
*            <eixo4>1720</eixo4>
*            <eixo5>2150</eixo5>
*            <eixo6>2580</eixo6>
*            <eixo7>3010</eixo7>
*            <eixo8>3440</eixo8>
*            <eixo9>3870</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>850</codigoPraca>
*            <nomePraca>Nova Mutum</nomePraca>
*            <eixo1>330</eixo1>
*            <eixo2>660</eixo2>
*            <eixo3>990</eixo3>
*            <eixo4>1320</eixo4>
*            <eixo5>1650</eixo5>
*            <eixo6>1980</eixo6>
*            <eixo7>2310</eixo7>
*            <eixo8>2640</eixo8>
*            <eixo9>2970</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>851</codigoPraca>
*            <nomePraca>Diamantino</nomePraca>
*            <eixo1>410</eixo1>
*            <eixo2>820</eixo2>
*            <eixo3>1230</eixo3>
*            <eixo4>1640</eixo4>
*            <eixo5>2050</eixo5>
*            <eixo6>2460</eixo6>
*            <eixo7>2870</eixo7>
*            <eixo8>3280</eixo8>
*            <eixo9>3690</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>852</codigoPraca>
*            <nomePraca>Jangada</nomePraca>
*            <eixo1>490</eixo1>
*            <eixo2>980</eixo2>
*            <eixo3>1470</eixo3>
*            <eixo4>1960</eixo4>
*            <eixo5>2450</eixo5>
*            <eixo6>2940</eixo6>
*            <eixo7>3430</eixo7>
*            <eixo8>3920</eixo8>
*            <eixo9>4410</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>828</codigoPraca>
*            <nomePraca>Rondonópolis</nomePraca>
*            <eixo1>450</eixo1>
*            <eixo2>900</eixo2>
*            <eixo3>1350</eixo3>
*            <eixo4>1800</eixo4>
*            <eixo5>2250</eixo5>
*            <eixo6>2700</eixo6>
*            <eixo7>3150</eixo7>
*            <eixo8>3600</eixo8>
*            <eixo9>4050</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>829</codigoPraca>
*            <nomePraca>Campo Verde</nomePraca>
*            <eixo1>370</eixo1>
*            <eixo2>740</eixo2>
*            <eixo3>1110</eixo3>
*            <eixo4>1480</eixo4>
*            <eixo5>1850</eixo5>
*            <eixo6>2220</eixo6>
*            <eixo7>2590</eixo7>
*            <eixo8>2960</eixo8>
*            <eixo9>3330</eixo9>
*        </praca>
*        <praca>
*            <codigoPraca>830</codigoPraca>
*            <nomePraca>Santo Antônio de Leverger</nomePraca>
*            <eixo1>360</eixo1>
*            <eixo2>720</eixo2>
*            <eixo3>1080</eixo3>
*            <eixo4>1440</eixo4>
*            <eixo5>1800</eixo5>
*            <eixo6>2160</eixo6>
*            <eixo7>2520</eixo7>
*            <eixo8>2880</eixo8>
*            <eixo9>3240</eixo9>
*        </praca>
*    </pracasPedagio>
*</retornoConsultaRota>



  ENDMETHOD.


  METHOD LER_XML_CRIAR_ROTA.

    DATA: XML_RET    TYPE REF TO CL_XML_DOCUMENT,
          XML_NODE   TYPE REF TO IF_IXML_NODE,
          V_TAMANHOI TYPE I,
          V_VALOR    TYPE STRING.

    CREATE OBJECT XML_RET.

    V_TAMANHOI = XML_RET->PARSE_STRING( I_XML ).

    CALL METHOD XML_RET->FIND_NODE
      EXPORTING
        NAME = 'msg'
      RECEIVING
        NODE = XML_NODE.

    IF ( SY-SUBRC IS INITIAL ) AND ( NOT XML_NODE IS INITIAL ).
      CALL METHOD XML_NODE->GET_VALUE
        RECEIVING
          RVAL = V_VALOR.
      MOVE V_VALOR TO E_MSG.
    ENDIF.

    CALL METHOD XML_RET->FIND_NODE
      EXPORTING
        NAME = 'codigoRota'
      RECEIVING
        NODE = XML_NODE.

    IF ( SY-SUBRC IS INITIAL ) AND ( NOT XML_NODE IS INITIAL ).
      CALL METHOD XML_NODE->GET_VALUE
        RECEIVING
          RVAL = V_VALOR.
      MOVE V_VALOR TO E_ID_ROTA_ADM.
    ENDIF.

  ENDMETHOD.


  METHOD LER_XML_ROTAS.

    TYPES: BEGIN OF TY_ROTAS,
             BUKRS         TYPE ZLEST0084-BUKRS,
             BRANCH        TYPE ZLEST0084-BRANCH,
             ID_ROTA       TYPE ZLEST0084-ID_ROTA,
             CAT_VEICULO   TYPE ZLEST0084-CAT_VEICULO,
             DT_VIGENCIA   TYPE ZLEST0084-DT_VIGENCIA,
             MUNIC_ORIGEM  TYPE ZLEST0084-MUNIC_ORIGEM,
             MUNIC_DESTINO TYPE ZLEST0084-MUNIC_DESTINO,
             DISTANCIA     TYPE ZLEST0084-DISTANCIA,
             VLR_PEDAGIO   TYPE ZLEST0084-VLR_PEDAGIO,
             DESCR_ROTA    TYPE ZLEST0084-DESCR_ROTA,
           END OF TY_ROTAS.

    DATA: IF_XML           TYPE REF TO IF_IXML,
          IF_STREAMFACTORY TYPE REF TO IF_IXML_STREAM_FACTORY,
          IF_STREAM        TYPE REF TO IF_IXML_ISTREAM,
          IF_XML_PARSER    TYPE REF TO IF_IXML_PARSER,
          IF_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,

          IF_NODE          TYPE REF TO IF_IXML_NODE,
          IF_MAP           TYPE REF TO IF_IXML_NAMED_NODE_MAP,
          IF_ATTR          TYPE REF TO IF_IXML_NODE,

          ITERATOR         TYPE REF TO IF_IXML_NODE_ITERATOR,
          TAG_NAME         TYPE STRING,
          NAME_DOM         TYPE STRING,
          COUNT_DOM        TYPE I,
          INDEX_DOM        TYPE I,
          PREFIX_DOM       TYPE STRING,
          VALOR_DOM        TYPE STRING,

          NODE_FILHO       TYPE REF TO IF_IXML_NODE,
          VALOR_FILHO      TYPE STRING.


    DATA: GT_ROTAS     TYPE TABLE OF TY_ROTAS,
          GW_ROTAS     TYPE TY_ROTAS,
          GW_ZLEST0084 TYPE ZLEST0084.

    DATA: OBJ_ZCL_ROTA    TYPE REF TO ZCL_ROTA,
          OBJ_ZCL_ROTA_DB TYPE REF TO ZCL_ROTA_DB.



    FIELD-SYMBOLS: <FS_ROTAS> TYPE TY_ROTAS.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0159)
      FROM ZLEST0159
     WHERE DS_GRUPO EQ @I_GRUPO.

    IF_XML           = CL_IXML=>CREATE( ).
    IF_DOCUMENT      = IF_XML->CREATE_DOCUMENT( ).
    IF_STREAMFACTORY = IF_XML->CREATE_STREAM_FACTORY( ).
    IF_STREAM        = IF_STREAMFACTORY->CREATE_ISTREAM_STRING( I_XML ).

    IF_XML_PARSER    = IF_XML->CREATE_PARSER(  STREAM_FACTORY = IF_STREAMFACTORY
                                               ISTREAM        = IF_STREAM
                                               DOCUMENT       = IF_DOCUMENT ).

    IF_XML_PARSER->PARSE( ).

    IF_NODE ?= IF_DOCUMENT->GET_ROOT_ELEMENT( ).

    IF NOT ( IF_NODE IS INITIAL ).

      ITERATOR = IF_NODE->CREATE_ITERATOR( ).
      IF_NODE = ITERATOR->GET_NEXT( ).

      WHILE NOT IF_NODE IS INITIAL.


        CASE IF_NODE->GET_TYPE( ).

          WHEN: IF_IXML_NODE=>CO_NODE_ELEMENT.

            TAG_NAME = IF_NODE->GET_NAME( ).
            IF_MAP   = IF_NODE->GET_ATTRIBUTES( ).

            IF NOT ( IF_MAP IS INITIAL ).

              COUNT_DOM = IF_MAP->GET_LENGTH( ).

              DO COUNT_DOM TIMES.

                INDEX_DOM  = SY-INDEX - 1.
                IF_ATTR    = IF_MAP->GET_ITEM( INDEX_DOM ).
                NAME_DOM   = IF_ATTR->GET_NAME( ).
                PREFIX_DOM = IF_ATTR->GET_NAMESPACE_PREFIX( ).
                VALOR_DOM  = IF_ATTR->GET_VALUE( ).

                GW_ROTAS-ID_ROTA = VALOR_DOM.

                IF NOT ( GW_ROTAS-ID_ROTA IS INITIAL ).
                  APPEND GW_ROTAS TO GT_ROTAS.
                ENDIF.

              ENDDO.

              CASE TAG_NAME.
                WHEN: 'categoriaVeiculo'.

                  IF NOT ( GW_ROTAS-ID_ROTA IS INITIAL ).
                    LOOP AT GT_ROTAS ASSIGNING <FS_ROTAS> WHERE ID_ROTA EQ GW_ROTAS-ID_ROTA.
                      <FS_ROTAS>-CAT_VEICULO = IF_NODE->GET_VALUE( ).
                    ENDLOOP.
                    UNASSIGN <FS_ROTAS>.
                  ENDIF.

                WHEN: 'valorPedagio'.

                  IF NOT ( GW_ROTAS-ID_ROTA IS INITIAL ).
                    LOOP AT GT_ROTAS ASSIGNING <FS_ROTAS> WHERE ID_ROTA EQ GW_ROTAS-ID_ROTA.
                      <FS_ROTAS>-VLR_PEDAGIO = IF_NODE->GET_VALUE( ).
                    ENDLOOP.

                    UNASSIGN <FS_ROTAS>.
                  ENDIF.


                WHEN: 'percurso'.

                  IF NOT ( GW_ROTAS-ID_ROTA IS INITIAL ).
                    LOOP AT GT_ROTAS ASSIGNING <FS_ROTAS> WHERE ID_ROTA EQ GW_ROTAS-ID_ROTA.
                      <FS_ROTAS>-DESCR_ROTA = IF_NODE->GET_VALUE( ).
                    ENDLOOP.
                    UNASSIGN <FS_ROTAS>.
                  ENDIF.


                WHEN: 'distanciaPercurso'.

                  IF NOT ( GW_ROTAS-ID_ROTA IS INITIAL ).
                    LOOP AT GT_ROTAS ASSIGNING <FS_ROTAS> WHERE ID_ROTA EQ GW_ROTAS-ID_ROTA.
                      <FS_ROTAS>-DISTANCIA = IF_NODE->GET_VALUE( ).
                    ENDLOOP.
                  ENDIF.

                WHEN: 'inicioVigencia'.

                  IF NOT ( GW_ROTAS-ID_ROTA IS INITIAL ).
                    LOOP AT GT_ROTAS ASSIGNING <FS_ROTAS> WHERE ID_ROTA EQ GW_ROTAS-ID_ROTA.
                      <FS_ROTAS>-DT_VIGENCIA = IF_NODE->GET_VALUE( ).
                    ENDLOOP.
                    UNASSIGN <FS_ROTAS>.
                  ENDIF.

                WHEN: 'municipios'.

                  IF NOT ( GW_ROTAS-ID_ROTA IS INITIAL ).

                    NODE_FILHO    = IF_NODE->GET_FIRST_CHILD( ).
                    VALOR_FILHO   = NODE_FILHO->GET_VALUE( ).

                    LOOP AT GT_ROTAS ASSIGNING <FS_ROTAS> WHERE ID_ROTA EQ GW_ROTAS-ID_ROTA.
                      <FS_ROTAS>-MUNIC_ORIGEM = VALOR_FILHO.
                    ENDLOOP.
                    UNASSIGN <FS_ROTAS>.

                    CLEAR: NODE_FILHO, VALOR_FILHO.

                    NODE_FILHO  = IF_NODE->GET_LAST_CHILD( ).
                    VALOR_FILHO = NODE_FILHO->GET_VALUE( ).

                    LOOP AT GT_ROTAS ASSIGNING <FS_ROTAS> WHERE ID_ROTA EQ GW_ROTAS-ID_ROTA.
                      <FS_ROTAS>-MUNIC_DESTINO = VALOR_FILHO.
                    ENDLOOP.
                    UNASSIGN <FS_ROTAS>.

                    CLEAR: VALOR_FILHO.
                  ENDIF.
              ENDCASE.
            ENDIF.
        ENDCASE.

        IF_NODE = ITERATOR->GET_NEXT( ).
      ENDWHILE.
    ENDIF.

    IF NOT ( GT_ROTAS[] IS INITIAL ).

      LOOP AT GT_ROTAS INTO GW_ROTAS.

        FREE: OBJ_ZCL_ROTA,
              OBJ_ZCL_ROTA_DB.

        CREATE OBJECT: OBJ_ZCL_ROTA,
                       OBJ_ZCL_ROTA_DB.

        SELECT SINGLE * FROM ZLEST0084 INTO GW_ZLEST0084 WHERE ID_ROTA     EQ GW_ROTAS-ID_ROTA
                                                           AND CAT_VEICULO EQ GW_ROTAS-CAT_VEICULO
                                                           AND BUKRS       EQ GW_ROTAS-BUKRS
                                                           AND BRANCH      EQ GW_ROTAS-BRANCH.
        CASE SY-SUBRC.
          WHEN: 0.

            OBJ_ZCL_ROTA->SET_BUKRS( WA_ZLEST0159-BUKRS ).
            OBJ_ZCL_ROTA->SET_BRANCH( WA_ZLEST0159-BRANCH ).
            OBJ_ZCL_ROTA->SET_ID_ROTA( GW_ROTAS-ID_ROTA ).
            OBJ_ZCL_ROTA->SET_CAT_VEICULO( GW_ROTAS-CAT_VEICULO ).
            OBJ_ZCL_ROTA->SET_DT_VIGENCIA( GW_ROTAS-DT_VIGENCIA ).
            OBJ_ZCL_ROTA->SET_VLR_PEDAGIO( GW_ROTAS-VLR_PEDAGIO ).
            OBJ_ZCL_ROTA->SET_DESCR_ROTA( GW_ROTAS-DESCR_ROTA ).

            "Atualizar as Informações no Banco de Dados.
            OBJ_ZCL_ROTA_DB->ZIF_ROTA_DB~ATUALIZAR( OBJ_ZCL_ROTA ).

          WHEN OTHERS.

            OBJ_ZCL_ROTA->SET_BUKRS( WA_ZLEST0159-BUKRS ).
            OBJ_ZCL_ROTA->SET_BRANCH( WA_ZLEST0159-BRANCH ).
            OBJ_ZCL_ROTA->SET_ID_ROTA( GW_ROTAS-ID_ROTA  ).
            OBJ_ZCL_ROTA->SET_CAT_VEICULO( GW_ROTAS-CAT_VEICULO ).
            OBJ_ZCL_ROTA->SET_DT_VIGENCIA( GW_ROTAS-DT_VIGENCIA ).
            OBJ_ZCL_ROTA->SET_MUNIC_ORIGEM( GW_ROTAS-MUNIC_ORIGEM ).
            OBJ_ZCL_ROTA->SET_MUNIC_DESTINO( GW_ROTAS-MUNIC_DESTINO ).
            OBJ_ZCL_ROTA->SET_DISTANCIA( GW_ROTAS-DISTANCIA ).
            OBJ_ZCL_ROTA->SET_VLR_PEDAGIO( GW_ROTAS-VLR_PEDAGIO ).
            OBJ_ZCL_ROTA->SET_DESCR_ROTA( GW_ROTAS-DESCR_ROTA ).

            "Inserir no Banco de Dados.
            OBJ_ZCL_ROTA_DB->ZIF_ROTA_DB~INSERIR( OBJ_ZCL_ROTA ).


        ENDCASE.
      ENDLOOP.

    ELSE.
      "RAISE EXCEPTION TYPE ZCX_WEBSERVICE EXPORTING TEXTID = ZCX_WEBSERVICE=>SEM_ROTA_UPDATE.
    ENDIF.

  ENDMETHOD.


  METHOD LER_XML_SITUACAO_TRANSPORTADOR.

    DATA: IF_XML           TYPE REF TO IF_IXML,
          IF_DOCUMENT      TYPE REF TO IF_IXML_DOCUMENT,
          IF_STREAMFACTORY TYPE REF TO IF_IXML_STREAM_FACTORY,
          IF_STREAM        TYPE REF TO IF_IXML_ISTREAM,
          IF_XML_PARSER    TYPE REF TO IF_IXML_PARSER,
          IF_NODE          TYPE REF TO IF_IXML_NODE,
          ITERATOR         TYPE REF TO IF_IXML_NODE_ITERATOR.

    DATA: TAG_NAME      TYPE STRING,
          VALOR_DOM     TYPE STRING,
          FILHO         TYPE REF TO IF_IXML_NODE_LIST,
          ITERATOR2     TYPE REF TO IF_IXML_NODE_ITERATOR,
          IF_NODE_FILHO TYPE REF TO IF_IXML_NODE.

    CLEAR: E_ZLEST0135.

    IF_XML           = CL_IXML=>CREATE( ).
    IF_DOCUMENT      = IF_XML->CREATE_DOCUMENT( ).
    IF_STREAMFACTORY = IF_XML->CREATE_STREAM_FACTORY( ).
    IF_STREAM        = IF_STREAMFACTORY->CREATE_ISTREAM_STRING( I_XML ).
    IF_XML_PARSER    = IF_XML->CREATE_PARSER(  STREAM_FACTORY = IF_STREAMFACTORY ISTREAM = IF_STREAM DOCUMENT = IF_DOCUMENT ).
    IF_XML_PARSER->PARSE( ).

    IF_NODE ?= IF_DOCUMENT->GET_ROOT_ELEMENT( ).

    IF NOT ( IF_NODE IS INITIAL ).

      ITERATOR = IF_NODE->CREATE_ITERATOR( ).
      IF_NODE  = ITERATOR->GET_NEXT( ).

      WHILE NOT IF_NODE IS INITIAL.
        CASE IF_NODE->GET_TYPE( ).
          WHEN: IF_IXML_NODE=>CO_NODE_ELEMENT.
            TAG_NAME = IF_NODE->GET_NAME( ).

            CASE TAG_NAME.
              WHEN 'transportador'.
                FILHO         = IF_NODE->GET_CHILDREN( ).
                ITERATOR2     = FILHO->CREATE_ITERATOR( ).
                IF_NODE_FILHO = ITERATOR2->GET_NEXT( ).

                WHILE NOT IF_NODE_FILHO IS INITIAL.
                  TAG_NAME  = IF_NODE_FILHO->GET_NAME( ).
                  VALOR_DOM = IF_NODE_FILHO->GET_VALUE( ).
                  CASE TAG_NAME.
                    WHEN 'razaoSocial'.
                      E_ZLEST0135-DS_RAZAO_SOCIAL = VALOR_DOM.
                    WHEN 'tipo'.
                      TRANSLATE VALOR_DOM TO UPPER CASE.
                      CASE VALOR_DOM.
                        WHEN 'TAC'.
                          E_ZLEST0135-TP_TRANSPORTADOR = '1'.
                        WHEN 'ETC'.
                          E_ZLEST0135-TP_TRANSPORTADOR = '2'.
                        WHEN 'CTC'.
                          E_ZLEST0135-TP_TRANSPORTADOR = '3'.
                      ENDCASE.
                    WHEN 'validadeRntrc'.
                      IF VALOR_DOM IS NOT INITIAL.
                        CONCATENATE VALOR_DOM+6(4) VALOR_DOM+3(2) VALOR_DOM(2) INTO E_ZLEST0135-DT_VALIDADE_RNTRC.
                      ENDIF.
                    WHEN 'rntrcAtivo'.
                      TRANSLATE VALOR_DOM TO UPPER CASE.
                      CASE VALOR_DOM.
                        WHEN 'FALSE'.
                          E_ZLEST0135-CK_RNTRC_ATIVO = ABAP_FALSE.
                        WHEN 'TRUE'.
                          E_ZLEST0135-CK_RNTRC_ATIVO = ABAP_TRUE.
                      ENDCASE.
                    WHEN 'equiparadoTac'.
                      TRANSLATE VALOR_DOM TO UPPER CASE.
                      CASE VALOR_DOM.
                        WHEN 'FALSE'.
                          E_ZLEST0135-CK_ETC_EQUIPARADO = ABAP_FALSE.
                        WHEN 'TRUE'.
                          E_ZLEST0135-CK_ETC_EQUIPARADO = ABAP_TRUE.
                      ENDCASE.
                    WHEN 'msg'.
                      E_ZLEST0135-DS_MSG_TRANSPORTADOR = VALOR_DOM.
                  ENDCASE.
                  IF_NODE_FILHO = ITERATOR2->GET_NEXT( ).
                ENDWHILE.
              WHEN 'veiculoTransportador'.
                FILHO     = IF_NODE->GET_CHILDREN( ).
                ITERATOR2 = FILHO->CREATE_ITERATOR( ).
                IF_NODE_FILHO = ITERATOR2->GET_NEXT( ).

                WHILE NOT IF_NODE_FILHO IS INITIAL.
                  TAG_NAME  = IF_NODE_FILHO->GET_NAME( ).
                  VALOR_DOM = IF_NODE_FILHO->GET_VALUE( ).
                  CASE TAG_NAME.
                    WHEN 'descricao'.
                      E_ZLEST0135-DS_VEICULO = VALOR_DOM.
                    WHEN 'eixo'.
                      E_ZLEST0135-QT_EIXOS = VALOR_DOM.
                    WHEN 'proprietario'.
                      E_ZLEST0135-DS_PROPRIETARIO = VALOR_DOM.
                    WHEN 'tag'.
                      E_ZLEST0135-NR_TAG = VALOR_DOM.
                    WHEN 'msg'.
                      E_ZLEST0135-DS_MSG_VEICULO = VALOR_DOM.
                      TRANSLATE VALOR_DOM TO UPPER CASE.
                      IF VALOR_DOM = '[WS SEMPARAR] - SUCESSO'.
                        E_ZLEST0135-CK_SEM_PARAR = ABAP_TRUE.
                      ELSE.
                        E_ZLEST0135-CK_SEM_PARAR = ABAP_FALSE.
                      ENDIF.
                  ENDCASE.
                  IF_NODE_FILHO = ITERATOR2->GET_NEXT( ).
                ENDWHILE.
            ENDCASE.
        ENDCASE.

        IF_NODE = ITERATOR->GET_NEXT( ).
      ENDWHILE.

    ENDIF.

  ENDMETHOD.


   METHOD processar_arquivo_cobranca.

     TYPES: BEGIN OF ty_lines,
              line(500),
            END OF ty_lines.

     TYPES BEGIN OF ty_valores.
     TYPES: cd_ciot   TYPE zciot,
            docnum    TYPE j_1bdocnum,
            vl_perda  TYPE kwert,
            vl_quebra TYPE kwert.
     TYPES END OF ty_valores.

     DATA: it_lines_arq       TYPE zde_linha_txt_1000_t,
           it_lines_arq_ped   TYPE zde_linha_txt_1000_t,
           it_lines           TYPE STANDARD TABLE OF ty_lines,
           wa_0062            TYPE zlest0062,
           it_0062            TYPE TABLE OF zlest0062,
           arquivo            TYPE REF TO zcl_tip_frete_aq_cobranca,
           "LC_NR_LOTE_ADM    TYPE ZPFE_NR_LOTE_ADM,
           it_reg_cabecalho   TYPE TABLE OF zpfe_lote,
           it_reg_itens       TYPE TABLE OF zpfe_lote_item,
           it_reg_itens_ad    TYPE TABLE OF zpfe_lote_item,
           it_reg_itens_aux   TYPE TABLE OF zpfe_lote_item,
           it_arquivo	        TYPE TABLE OF zpfe_arquivo,
           vg_lote            TYPE  i,
           vg_nm_lote_item    TYPE zpfe_numero_lote,
           vg_tipcontabil     TYPE ztipcontabil,
           vg_tipcontabil_itm TYPE ztipcontabil,
           st_lote            TYPE zpfe_numero_lote,
           st_lote_aux        TYPE zpfe_numero_lote,
           wa_valores         TYPE ty_valores,
           it_valores         TYPE TABLE OF ty_valores,
           vg_diferenca       TYPE j_1bnetqty,
           vg_toleravel       TYPE j_1bnetqty.

     DEFINE insert_line_log.

       CLEAR: wa_0062.
       wa_0062-nr_lote_adm = &1.
       wa_0062-nr_lote     = &2.
       wa_0062-chvid       = &3.
       wa_0062-nucontrato  = &4.
       wa_0062-id_tipo     = &5.
       wa_0062-msg_erro    = &6.
       wa_0062-linha       = &7.
       APPEND wa_0062 TO it_0062.

       IF &8 EQ 'E'.
         MODIFY zlest0062 FROM wa_0062.
       ENDIF.

     END-OF-DEFINITION.

     CLEAR: it_reg_cabecalho, it_reg_cabecalho[], it_reg_itens, it_reg_itens[], e_lotes[].

     DATA: url TYPE string.
     DATA: t_url TYPE string.
     DATA: client TYPE REF TO if_http_client.
     DATA: c_xml TYPE string.
     DATA: c_separador TYPE c LENGTH 1.

     IF i_filename IS NOT INITIAL.
*-US 130492-08.04.2024-JT-inicio
       IF i_filename(4) = 'http'.
         url = i_filename.

         cl_http_client=>create_by_url(
           EXPORTING
             url                = url
*           PROXY_HOST         =
*           PROXY_SERVICE      =
*           SSL_ID             =
*           SAP_USERNAME       =
*           SAP_CLIENT         =
           IMPORTING
             client             = client
           EXCEPTIONS
             argument_not_found = 1
             plugin_not_active  = 2
             internal_error     = 3
             OTHERS             = 4 ).

         IF sy-subrc IS NOT INITIAL.
           RAISE EXCEPTION TYPE zcx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
         ENDIF.

         client->send(
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             http_invalid_timeout       = 4
             OTHERS                     = 5
         ).

         IF sy-subrc IS NOT INITIAL.
           RAISE EXCEPTION TYPE zcx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
         ENDIF.

         client->receive(
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             OTHERS                     = 4
         ).

         IF sy-subrc IS NOT INITIAL.
           RAISE EXCEPTION TYPE zcx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
         ENDIF.

         c_xml = client->response->get_cdata( ).
         client->close( ).
       ELSE.
         c_xml = zcl_string=>base64_to_string( i_texto = CONV #( i_content_filename ) ).
       ENDIF.
*-US 130492-08.04.2024-JT-fim

       SPLIT c_xml AT cl_abap_char_utilities=>newline INTO: TABLE it_lines_arq.
     ENDIF.

     IF i_filename_pedagio IS NOT INITIAL.
*-US 130492-08.04.2024-JT-inicio
       IF i_filename_pedagio(4) = 'http'.
         url = i_filename_pedagio.

         cl_http_client=>create_by_url(
           EXPORTING
             url                = url
*           PROXY_HOST         =
*           PROXY_SERVICE      =
*           SSL_ID             =
*           SAP_USERNAME       =
*           SAP_CLIENT         =
           IMPORTING
             client             = client
           EXCEPTIONS
             argument_not_found = 1
             plugin_not_active  = 2
             internal_error     = 3
             OTHERS             = 4 ).

         IF sy-subrc IS NOT INITIAL.
           RAISE EXCEPTION TYPE zcx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
         ENDIF.

         client->send(
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             http_invalid_timeout       = 4
             OTHERS                     = 5
         ).

         IF sy-subrc IS NOT INITIAL.
           RAISE EXCEPTION TYPE zcx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
         ENDIF.

         client->receive(
           EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state         = 2
             http_processing_failed     = 3
             OTHERS                     = 4
         ).

         IF sy-subrc IS NOT INITIAL.
           RAISE EXCEPTION TYPE zcx_erro_arquivo
             EXPORTING
               textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
               msgid  = sy-msgid
               msgno  = sy-msgno
               msgty  = 'E'
               msgv1  = sy-msgv1
               msgv2  = sy-msgv2
               msgv3  = sy-msgv3
               msgv4  = sy-msgv4.
         ENDIF.

         c_xml = client->response->get_cdata( ).
         client->close( ).
       ELSE.
         c_xml = zcl_string=>base64_to_string( i_texto = CONV #( i_content_filename_pedagio ) ).
       ENDIF.
*-US 130492-08.04.2024-JT-fim

       SPLIT c_xml AT cl_abap_char_utilities=>newline INTO: TABLE it_lines_arq_ped.
     ENDIF.

     LOOP AT it_lines_arq INTO DATA(wa_line).
       APPEND wa_line TO it_arquivo.
     ENDLOOP.

     vg_lote = 0.

     CALL FUNCTION 'Z_PFE_ARQUIVO_REGISTOS'
       TABLES
         t_arquivo           = it_arquivo
         t_reg_cabecalho     = it_reg_cabecalho
         t_reg_itens         = it_reg_itens
       CHANGING
         vg_lote             = vg_lote
       EXCEPTIONS
         nao_administradora  = 1
         nao_local_negocio   = 2
         nao_chave_historico = 3
         nao_chave_hist_info = 4
         nao_ciot            = 5
         nao_ciot_info       = 6
         OTHERS              = 7.

     IF sy-subrc IS NOT INITIAL.
       RAISE EXCEPTION TYPE zcx_erro_arquivo
         EXPORTING
           textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
           msgid  = sy-msgid
           msgno  = sy-msgno
           msgty  = 'E'
           msgv1  = sy-msgv1
           msgv2  = sy-msgv2
           msgv3  = sy-msgv3
           msgv4  = sy-msgv4.
     ENDIF.

     CHECK it_reg_itens IS NOT INITIAL.

     SELECT * INTO TABLE @DATA(it_zlest0025)
       FROM zlest0025.

     MOVE it_reg_itens[] TO it_reg_itens_ad[].
     DELETE it_reg_itens    WHERE chvid EQ '1'.
     DELETE it_reg_itens_ad WHERE chvid NE '1'.
     MOVE it_reg_itens[] TO it_reg_itens_aux[].

     SORT it_reg_cabecalho BY nr_lote_adm.

     DATA(it_reg_copia) = it_reg_cabecalho[].
     SORT it_reg_copia BY nr_lote_adm.
     DELETE ADJACENT DUPLICATES FROM it_reg_copia COMPARING nr_lote_adm.
     LOOP AT it_reg_copia INTO DATA(wa_reg_copia).
       SELECT SINGLE * INTO @DATA(wa_zpfe_lote) FROM zpfe_lote WHERE nr_lote_adm EQ @wa_reg_copia-nr_lote_adm.
       IF sy-subrc IS INITIAL.
         DELETE it_reg_cabecalho WHERE nr_lote_adm = wa_reg_copia-nr_lote_adm .
       ENDIF.
     ENDLOOP.


     "Adiantamento
     vg_nm_lote_item = 1.
     LOOP AT it_reg_cabecalho INTO DATA(wa_reg_cabecalho).

       READ TABLE it_arquivo INDEX 1 INTO DATA(wa_arquivo).
       CLEAR: vg_tipcontabil.

       READ TABLE it_reg_itens_ad WITH KEY nm_lote = wa_reg_cabecalho-nm_lote TRANSPORTING NO FIELDS.
       IF NOT sy-subrc IS INITIAL.
         CONTINUE.
       ENDIF.

       wa_reg_cabecalho-vl_total_lote = 0.
       wa_reg_cabecalho-vl_confi_lote = 0.

       CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
         EXPORTING
           p_dt_posicao  = wa_reg_cabecalho-dt_posicao
         IMPORTING
           p_tipcontabil = vg_tipcontabil.

       CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
           nr_range_nr             = '01'
           object                  = 'ZPFELOTE'
         IMPORTING
           number                  = st_lote
         EXCEPTIONS
           interval_not_found      = 1
           number_range_not_intern = 2
           object_not_found        = 3
           quantity_is_0           = 4
           quantity_is_not_1       = 5
           interval_overflow       = 6
           buffer_overflow         = 7
           OTHERS                  = 8.

       IF sy-subrc IS NOT INITIAL.
         RAISE EXCEPTION TYPE zcx_erro_arquivo
           EXPORTING
             textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
             msgid  = sy-msgid
             msgno  = sy-msgno
             msgty  = 'E'
             msgv1  = sy-msgv1
             msgv2  = sy-msgv2
             msgv3  = sy-msgv3
             msgv4  = sy-msgv4.
       ENDIF.

       st_lote_aux = wa_reg_cabecalho-nm_lote.

       wa_reg_cabecalho-status  = 'I'.
       wa_reg_cabecalho-nm_lote = st_lote.
       IF it_reg_itens_ad[] IS NOT INITIAL.

         SELECT *
           FROM zpfe_lote_item
           INTO TABLE it_reg_itens_aux
            FOR ALL ENTRIES IN it_reg_itens_ad
            WHERE nucontrato EQ it_reg_itens_ad-nucontrato
              AND chvid      EQ it_reg_itens_ad-chvid.

         SELECT  *
           FROM zcte_ciot
           INTO TABLE @DATA(it_zcte_ciot)
            FOR ALL ENTRIES IN @it_reg_itens_ad
            WHERE nucontrato EQ @it_reg_itens_ad-nucontrato.

         IF sy-subrc IS INITIAL.
           SELECT  *
             FROM zcte_identifica
             INTO TABLE @DATA(it_zcte_identifica)
             FOR ALL ENTRIES IN @it_zcte_ciot
              WHERE docnum EQ @it_zcte_ciot-docnum.
         ENDIF.
         SORT: it_reg_itens_aux BY nucontrato chvid.
         SORT: it_zcte_ciot BY nucontrato.
         SORT: it_zcte_identifica BY docnum.
       ENDIF.

       LOOP AT it_reg_itens_ad INTO DATA(wa_reg_itens) WHERE nm_lote EQ st_lote_aux.
         DATA(lc_tabix) = sy-tabix.
         ADD 1 TO lc_tabix.
         READ TABLE it_arquivo INDEX lc_tabix INTO wa_arquivo.

         READ TABLE it_reg_itens_aux INTO DATA(wa_reg_itens_aux)
         WITH KEY nucontrato = wa_reg_itens-nucontrato chvid = wa_reg_itens-chvid BINARY SEARCH.

         IF sy-subrc IS INITIAL.
           insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de existência de chave já importada'
                           wa_arquivo-linha
                           'E'.

           "Erro de existência de chave já importada
           CONTINUE.
         ENDIF.

         wa_reg_itens-nm_lote      = st_lote.
         wa_reg_itens-status       = 'I'.
         wa_reg_itens-nm_lote_item = vg_nm_lote_item.

*        SELECT SINGLE * INTO WA_ZCTE_CIOT
*          FROM ZCTE_CIOT
*         WHERE NUCONTRATO EQ WA_REG_ITENS-NUCONTRATO.
         READ TABLE it_zcte_ciot INTO DATA(wa_zcte_ciot) WITH KEY nucontrato = wa_reg_itens-nucontrato BINARY SEARCH.

         IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).
           IF wa_reg_itens-vl_transacao NE wa_zcte_ciot-vlr_adiantamento.
             insert_line_log wa_reg_cabecalho-nr_lote_adm
                             space
                             wa_reg_itens-chvid
                             wa_reg_itens-nucontrato
                             'F'
                             'Erro no adiantamento, valor da transação diferente do valor da cte'
                             wa_arquivo-linha
                             'E'.
             CONTINUE.
           ENDIF.

           READ TABLE it_zcte_identifica INTO DATA(wa_zcte_identifica) WITH KEY docnum = wa_zcte_ciot-docnum BINARY SEARCH.

           wa_reg_itens-tp_plano_administradora = wa_zcte_ciot-tp_plano_administradora.
           wa_reg_itens-cd_ciot                 = wa_zcte_ciot-cd_ciot.
           wa_reg_itens-nr_ciot                 = wa_zcte_ciot-nr_ciot.
           wa_reg_itens-docnum                  = wa_zcte_ciot-docnum.
           wa_reg_itens-tknum                   = wa_zcte_ciot-tknum.
           wa_reg_itens-ctenum                  = wa_zcte_identifica-nct.
           wa_reg_itens-cteserie                = wa_zcte_identifica-serie.
           wa_reg_cabecalho-vl_total_lote       = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

           wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
           wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
           wa_reg_itens-ck_conferido    = ''.
           wa_reg_itens-ds_usuario_conf = sy-uname.

           wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_transacao.

           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = wa_reg_itens-nm_lote_item
             IMPORTING
               output = wa_reg_itens-nm_lote_item.

           CASE wa_reg_itens-tp_plano_administradora.
             	WHEN zcl_ciot=>st_tp_plano_pre_pago.
               vg_tipcontabil_itm = 'FP'.
             	WHEN OTHERS.
               vg_tipcontabil_itm = vg_tipcontabil.
           ENDCASE.

           MODIFY zpfe_lote_item FROM wa_reg_itens.
           vg_nm_lote_item = vg_nm_lote_item + 1.

           insert_line_log wa_reg_itens-nr_lote_adm
                           wa_reg_itens-nm_lote
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'A linha do arquivo foi importado com sucesso'
                           space
                           'S'.
         ELSE.
           insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de Contrato não encontrado'
                           wa_arquivo-linha
                           'E'.
           "Erro de Contrato não encontrado

         ENDIF.
       ENDLOOP.

       wa_reg_cabecalho-tplote = 'A'.

       MODIFY zpfe_lote FROM wa_reg_cabecalho.
       APPEND wa_reg_cabecalho-nm_lote TO e_lotes.

       insert_line_log wa_reg_cabecalho-nr_lote_adm
                       wa_reg_cabecalho-nm_lote
                       space
                       space
                       'F'
                       'A linha do cabeçalho do arquivo foi importado com sucesso'
                       space
                       'S'.

     ENDLOOP.

     IF it_reg_itens[] IS NOT INITIAL.
       SELECT * INTO TABLE it_reg_itens_aux
         FROM zpfe_lote_item
          FOR ALL ENTRIES IN it_reg_itens
        WHERE nucontrato EQ it_reg_itens-nucontrato
          AND chvid      EQ it_reg_itens-chvid.

       SELECT * INTO TABLE it_zcte_ciot
         FROM zcte_ciot
          FOR ALL ENTRIES IN it_reg_itens
        WHERE nucontrato EQ it_reg_itens-nucontrato.

       IF sy-subrc IS INITIAL.
         SELECT * INTO TABLE it_zcte_identifica
           FROM zcte_identifica
            FOR ALL ENTRIES IN it_zcte_ciot
          WHERE docnum EQ it_zcte_ciot-docnum.
       ENDIF.

       SORT: it_reg_itens_aux BY nucontrato chvid.
       SORT: it_zcte_ciot BY nucontrato.
       SORT: it_zcte_identifica BY docnum.
     ENDIF.

     "Resto de Chaves
     vg_nm_lote_item = 1.
     LOOP AT it_reg_cabecalho INTO wa_reg_cabecalho.

       CLEAR: vg_tipcontabil.

       READ TABLE it_reg_itens WITH KEY nm_lote = wa_reg_cabecalho-nm_lote TRANSPORTING NO FIELDS.
       IF NOT sy-subrc IS INITIAL.
         CONTINUE.
       ENDIF.

       wa_reg_cabecalho-vl_total_lote = 0.
       wa_reg_cabecalho-vl_confi_lote = 0.

       CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
         EXPORTING
           p_dt_posicao  = wa_reg_cabecalho-dt_posicao
         IMPORTING
           p_tipcontabil = vg_tipcontabil.

       CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
           nr_range_nr             = '01'
           object                  = 'ZPFELOTE'
         IMPORTING
           number                  = st_lote
         EXCEPTIONS
           interval_not_found      = 1
           number_range_not_intern = 2
           object_not_found        = 3
           quantity_is_0           = 4
           quantity_is_not_1       = 5
           interval_overflow       = 6
           buffer_overflow         = 7
           OTHERS                  = 8.

       IF sy-subrc IS NOT INITIAL.
         RAISE EXCEPTION TYPE zcx_erro_arquivo
           EXPORTING
             textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
             msgid  = sy-msgid
             msgno  = sy-msgno
             msgty  = 'E'
             msgv1  = sy-msgv1
             msgv2  = sy-msgv2
             msgv3  = sy-msgv3
             msgv4  = sy-msgv4.
       ENDIF.

       st_lote_aux = wa_reg_cabecalho-nm_lote.

       wa_reg_cabecalho-status  = 'I'.
       wa_reg_cabecalho-nm_lote = st_lote.

       "Verificações de Saldo de Frete
       LOOP AT it_reg_itens INTO wa_reg_itens WHERE nm_lote EQ st_lote_aux AND chvid EQ '2'.

         CASE wa_reg_itens-tp_plano_administradora.
           	WHEN zcl_ciot=>st_tp_plano_pre_pago.
             vg_tipcontabil_itm = 'FP'.
           WHEN OTHERS.
             vg_tipcontabil_itm = vg_tipcontabil.
         ENDCASE.

         lc_tabix = sy-tabix.
         ADD 1 TO lc_tabix.
         READ TABLE it_arquivo INDEX lc_tabix INTO wa_arquivo.
         READ TABLE it_zlest0025 WITH KEY chvid = wa_reg_itens-chvid INTO DATA(wa_zlest0025).
         IF ( wa_reg_itens-chvid IS INITIAL ) OR ( NOT sy-subrc IS INITIAL ).
           CONTINUE.
         ENDIF.

         READ TABLE it_reg_itens_aux INTO wa_reg_itens_aux
         WITH KEY nucontrato = wa_reg_itens-nucontrato chvid = wa_reg_itens-chvid BINARY SEARCH.

         IF sy-subrc IS INITIAL.
           insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de existência de chave já importada'
                           wa_arquivo-linha
                           'E'.
           "Erro de existência de chave já importada
           CONTINUE.
         ENDIF.

         wa_reg_itens-nm_lote      = st_lote.
         wa_reg_itens-status       = 'I'.
         wa_reg_itens-nm_lote_item = vg_nm_lote_item.

         READ TABLE it_zcte_ciot INTO wa_zcte_ciot
         WITH KEY nucontrato = wa_reg_itens-nucontrato BINARY SEARCH.

         IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).

           READ TABLE it_zcte_identifica INTO wa_zcte_identifica WITH KEY docnum = wa_zcte_ciot-docnum BINARY SEARCH.

           wa_valores-cd_ciot   = wa_zcte_ciot-cd_ciot.
           wa_valores-docnum    = wa_zcte_ciot-docnum.
           wa_valores-vl_quebra = 0.
           wa_valores-vl_perda  = 0.

           wa_reg_itens-cd_ciot     = wa_zcte_ciot-cd_ciot.
           wa_reg_itens-nr_ciot     = wa_zcte_ciot-nr_ciot.
           wa_reg_itens-docnum       = wa_zcte_ciot-docnum.
           wa_reg_itens-tknum       = wa_zcte_ciot-tknum.
           wa_reg_itens-peso_origem = wa_zcte_ciot-quantidade.
           wa_reg_itens-ctenum      = wa_zcte_identifica-nct.
           wa_reg_itens-cteserie    = wa_zcte_identifica-serie.
           wa_reg_cabecalho-vl_total_lote = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

           IF vg_tipcontabil_itm EQ 'FC'.
             wa_reg_itens-peso_chegada = wa_reg_itens-peso_importado.
             wa_zcte_ciot-vlr_frete = wa_zcte_ciot-vlr_frete - wa_zcte_ciot-vlr_adiantamento - wa_zcte_ciot-vlr_seguro - wa_zcte_ciot-vlr_impostos.

             IF wa_reg_itens-peso_chegada GT wa_reg_itens-peso_origem.
               wa_reg_itens-peso_chegada = wa_reg_itens-peso_origem.
             ENDIF.
             wa_reg_itens-vl_pago_lote = wa_zcte_ciot-vlr_frete.

             IF ( wa_reg_itens-peso_chegada LT wa_reg_itens-peso_origem ) OR
                ( wa_reg_itens-vl_transacao GT wa_zcte_ciot-vlr_frete ).
               vg_diferenca = wa_reg_itens-peso_origem - wa_reg_itens-peso_chegada.

               "Valor de Quebra
               wa_valores-vl_quebra  = ( vg_diferenca * ( wa_zcte_ciot-vlr_unit_frete / 1000 ) ).
               "Valor da Perda
               vg_toleravel          = wa_reg_itens-peso_origem * ( wa_zcte_ciot-perc_tolerancia / 100 ).
               IF vg_diferenca GT vg_toleravel.
                 wa_valores-vl_perda = ( ( vg_diferenca - vg_toleravel ) * wa_zcte_ciot-vlr_unit_merc  ).
               ENDIF.
             ENDIF.
           ELSEIF vg_tipcontabil_itm EQ 'FS' OR vg_tipcontabil_itm EQ 'FP'.
             wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
             wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
             wa_reg_itens-peso_chegada    = wa_reg_itens-peso_importado.
             wa_reg_itens-ck_conferido    = 'X'.
             wa_reg_itens-ds_usuario_conf = sy-uname.
           ENDIF.

           APPEND wa_valores TO it_valores.
           IF wa_zlest0025-naturezachvid EQ 'S'.
             wa_reg_itens-vl_pago_lote = wa_reg_itens-vl_pago_lote * ( -1 ).
           ENDIF.

           wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.

           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = wa_reg_itens-nm_lote_item
             IMPORTING
               output = wa_reg_itens-nm_lote_item.

           MODIFY zpfe_lote_item FROM wa_reg_itens.
           vg_nm_lote_item = vg_nm_lote_item + 1.
           insert_line_log wa_reg_itens-nr_lote_adm
                           wa_reg_itens-nm_lote
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'A linha do arquivo foi importado com sucesso'
                           space
                           'S'.
         ELSE.
           insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de Contrato não encontrado'
                           wa_arquivo-linha
                           'E'.
         ENDIF.
       ENDLOOP.

       LOOP AT it_reg_itens INTO wa_reg_itens WHERE nm_lote EQ st_lote_aux AND chvid NE '2'.
         lc_tabix = sy-tabix.
         ADD 1 TO lc_tabix.
         READ TABLE it_arquivo INDEX lc_tabix INTO wa_arquivo.

         READ TABLE it_zlest0025 WITH KEY chvid = wa_reg_itens-chvid INTO wa_zlest0025.
         IF ( wa_reg_itens-chvid IS INITIAL ) OR ( NOT sy-subrc IS INITIAL ).
           CONTINUE.
         ENDIF.

         READ TABLE it_reg_itens_aux INTO wa_reg_itens_aux
         WITH KEY nucontrato = wa_reg_itens-nucontrato chvid = wa_reg_itens-chvid BINARY SEARCH.

         IF sy-subrc IS INITIAL.
           insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de existência de chave já importada'
                           wa_arquivo-linha
                           'E'.
           "Erro de existência de chave já importada
           CONTINUE.
         ENDIF.

         wa_reg_itens-nm_lote      = st_lote.
         wa_reg_itens-status       = 'I'.
         wa_reg_itens-nm_lote_item = vg_nm_lote_item.

         READ TABLE it_zcte_ciot INTO wa_zcte_ciot WITH KEY nucontrato = wa_reg_itens-nucontrato BINARY SEARCH.

         IF ( sy-subrc IS INITIAL ) AND ( wa_reg_itens-nucontrato IS NOT INITIAL ).

           READ TABLE it_zcte_identifica INTO wa_zcte_identifica WITH KEY docnum = wa_zcte_ciot-docnum BINARY SEARCH.
           wa_reg_itens-cd_ciot     = wa_zcte_ciot-cd_ciot.
           wa_reg_itens-nr_ciot     = wa_zcte_ciot-nr_ciot.
           wa_reg_itens-docnum      = wa_zcte_ciot-docnum.
           wa_reg_itens-tknum       = wa_zcte_ciot-tknum.
           wa_reg_itens-peso_origem = wa_zcte_ciot-quantidade.
           wa_reg_itens-ctenum      = wa_zcte_identifica-nct.
           wa_reg_itens-cteserie    = wa_zcte_identifica-serie.
           wa_reg_itens-tp_plano_administradora = wa_zcte_ciot-tp_plano_administradora.

           IF wa_zlest0025-naturezachvid EQ 'S'.
             wa_reg_itens-vl_transacao = wa_reg_itens-vl_transacao * ( -1 ).
           ENDIF.
           wa_reg_cabecalho-vl_total_lote = wa_reg_cabecalho-vl_total_lote + wa_reg_itens-vl_transacao.

           CASE wa_reg_itens-tp_plano_administradora.
             	WHEN zcl_ciot=>st_tp_plano_pre_pago.
               vg_tipcontabil_itm = 'FP'.
             WHEN OTHERS.
               vg_tipcontabil_itm = vg_tipcontabil.
           ENDCASE.

           IF vg_tipcontabil_itm EQ 'FC'.
             wa_reg_itens-vl_diferenca  = 0.

             CASE wa_reg_itens-chvid.
               WHEN '30'.
                 READ TABLE it_valores INTO wa_valores
                 WITH KEY cd_ciot = wa_zcte_ciot-cd_ciot
                          docnum  = wa_zcte_ciot-docnum.
                 IF sy-subrc IS INITIAL.
                   DATA(vg_tabix) = sy-tabix.
                   wa_reg_itens-vl_pago_lote = wa_valores-vl_quebra * -1.
                   wa_valores-vl_quebra      = 0.
                   MODIFY it_valores FROM wa_valores INDEX vg_tabix TRANSPORTING vl_quebra.
                 ENDIF.
                 "Quebra
               WHEN '31'.
                 "Perda
                 READ TABLE it_valores INTO wa_valores
                 WITH KEY cd_ciot = wa_zcte_ciot-cd_ciot
                          docnum  = wa_zcte_ciot-docnum.

                 IF sy-subrc IS INITIAL.
                   vg_tabix = sy-tabix.
                   wa_reg_itens-vl_pago_lote = wa_valores-vl_perda * -1.
                   wa_valores-vl_perda       = 0.
                   MODIFY it_valores FROM wa_valores INDEX vg_tabix TRANSPORTING vl_perda.
                 ENDIF.
               WHEN OTHERS.
                 wa_reg_itens-vl_pago_lote = wa_reg_itens-vl_transacao.
             ENDCASE.

           ELSEIF vg_tipcontabil_itm EQ 'FS' OR vg_tipcontabil_itm EQ 'FP'.
             wa_reg_itens-vl_conferido    = wa_reg_itens-vl_transacao.
             wa_reg_itens-vl_pago_lote    = wa_reg_itens-vl_transacao.
             wa_reg_itens-ck_conferido    = 'X'.
             wa_reg_itens-ds_usuario_conf = sy-uname.
           ENDIF.

           wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.

           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               input  = wa_reg_itens-nm_lote_item
             IMPORTING
               output = wa_reg_itens-nm_lote_item.

           MODIFY zpfe_lote_item FROM wa_reg_itens.
           vg_nm_lote_item = vg_nm_lote_item + 1.
           insert_line_log wa_reg_itens-nr_lote_adm
                           wa_reg_itens-nm_lote
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'A linha do arquivo foi importado com sucesso'
                           space
                           'S'.
         ELSE.
           insert_line_log wa_reg_cabecalho-nr_lote_adm
                           space
                           wa_reg_itens-chvid
                           wa_reg_itens-nucontrato
                           'F'
                           'Erro de Contrato não encontrado'
                           wa_arquivo-linha
                           'E'.
           "Erro de Contrato não encontrado
         ENDIF.
       ENDLOOP.

       IF vg_tipcontabil EQ 'FC'.
         IF it_valores[] IS NOT INITIAL.
           SELECT *
             FROM zcte_ciot
             INTO TABLE it_zcte_ciot
              FOR ALL ENTRIES IN it_valores
              WHERE cd_ciot EQ it_valores-cd_ciot.

           SELECT *
             FROM zcte_identifica
             INTO TABLE it_zcte_identifica
             FOR ALL ENTRIES IN it_valores
              WHERE docnum EQ it_valores-docnum.

           SORT it_zcte_ciot BY cd_ciot.
           SORT it_zcte_identifica BY docnum.
         ENDIF.

         LOOP AT it_valores INTO wa_valores WHERE ( vl_quebra GT 0 OR vl_perda GT 0 ) .

           wa_reg_itens-nm_lote = st_lote.
           wa_reg_itens-status  = 'I'.

           READ TABLE it_zcte_ciot INTO wa_zcte_ciot WITH KEY cd_ciot = wa_valores-cd_ciot BINARY SEARCH.

           READ TABLE it_zcte_identifica INTO wa_zcte_identifica WITH KEY docnum = wa_valores-docnum BINARY SEARCH.

           wa_reg_itens-cd_ciot       = wa_zcte_ciot-cd_ciot.
           wa_reg_itens-nr_ciot       = wa_zcte_ciot-nr_ciot.
           wa_reg_itens-docnum         = wa_zcte_ciot-docnum.
           wa_reg_itens-tknum         = wa_zcte_ciot-tknum.
           wa_reg_itens-ctenum        = wa_zcte_identifica-nct.
           wa_reg_itens-cteserie      = wa_zcte_identifica-serie.

           wa_reg_itens-vl_pago_lote  = 0.
           wa_reg_itens-vl_transacao  = 0.
           wa_reg_itens-vl_diferenca  = 0.

           IF wa_valores-vl_quebra GT 0.
             READ TABLE it_zlest0025 WITH KEY chvid = '30' INTO wa_zlest0025.
             wa_reg_itens-vl_pago_lote = wa_valores-vl_quebra * ( -1 ).
             wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.
             wa_reg_itens-chvid = '30'.
             wa_reg_itens-nm_lote_item = vg_nm_lote_item.

             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 input  = wa_reg_itens-nm_lote_item
               IMPORTING
                 output = wa_reg_itens-nm_lote_item.

             MODIFY zpfe_lote_item FROM wa_reg_itens.
             vg_nm_lote_item = vg_nm_lote_item + 1.
           ENDIF.
           IF wa_valores-vl_perda GT 0.
             READ TABLE it_zlest0025 WITH KEY chvid = '31' INTO wa_zlest0025.
             wa_reg_itens-vl_pago_lote = wa_valores-vl_perda * ( -1 ).
             wa_reg_cabecalho-vl_confi_lote = wa_reg_cabecalho-vl_confi_lote + wa_reg_itens-vl_pago_lote.
             wa_reg_itens-chvid = '31'.
             wa_reg_itens-nm_lote_item = vg_nm_lote_item.

             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 input  = wa_reg_itens-nm_lote_item
               IMPORTING
                 output = wa_reg_itens-nm_lote_item.

             MODIFY zpfe_lote_item FROM wa_reg_itens.
             vg_nm_lote_item = vg_nm_lote_item + 1.
             insert_line_log wa_reg_itens-nr_lote_adm
                             wa_reg_itens-nm_lote
                             wa_reg_itens-chvid
                             wa_reg_itens-nucontrato
                             'F'
                             'A linha do arquivo foi importado com sucesso'
                             space
                             'S'.
           ENDIF.
         ENDLOOP.
       ENDIF.

       CLEAR: it_valores[].

       wa_reg_cabecalho-tplote = 'S'.
       MODIFY zpfe_lote FROM wa_reg_cabecalho.
       APPEND wa_reg_cabecalho-nm_lote TO e_lotes.

       insert_line_log wa_reg_cabecalho-nr_lote_adm
                       wa_reg_cabecalho-nm_lote
                       space
                       space
                       'F'
                       'A linha do cabeçalho do arquivo foi importado com sucesso'
                       space
                       'S'.
     ENDLOOP.

     COMMIT WORK AND WAIT.

     LOOP AT it_reg_cabecalho INTO wa_reg_cabecalho.
       CREATE OBJECT arquivo.
       arquivo->set_bukrs( i_bukrs = i_bukrs ).
       arquivo->set_branch( i_branch = i_branch  ).
       arquivo->set_ds_filename( i_ds_filename = i_filename ).
       arquivo->set_ds_filename_ped( i_ds_filename = i_filename_pedagio ).
       arquivo->set_corpo_arquivo( i_corpo_arquivo = it_lines_arq ).
       arquivo->set_corpo_arquivo_ped( i_corpo_arquivo = it_lines_arq_ped ).
       arquivo->set_nr_lote_adm( i_nr_lote_adm = wa_reg_cabecalho-nr_lote_adm ).
       arquivo->zif_cadastro~gravar_registro( ).
       CLEAR: arquivo.
     ENDLOOP.

   ENDMETHOD.


   METHOD PROCESSAR_ARQUIVO_CONFERENCIA.

     TYPES: BEGIN OF TY_LINES,
              LINE(500),
            END OF TY_LINES.

     DATA: IT_LINES_ARQ        TYPE ZDE_LINHA_TXT_1000_T,
           IT_LINES            TYPE STANDARD TABLE OF TY_LINES,
           WA_ZPFE_LOTE_ITEM   TYPE ZPFE_LOTE_ITEM,
           WA_ZPFE_LOTE_ITEM_C TYPE ZPFE_LOTE_ITEM,
           IT_ZPFE_LOTE_ITEM   TYPE TABLE OF ZPFE_LOTE_ITEM,
           IT_ZPFE_LOTE_ITEM_C TYPE TABLE OF ZPFE_LOTE_ITEM,
           WA_CHVID_ADM(10),
           WA_0062             TYPE ZLEST0062,
           IT_0062             TYPE TABLE OF ZLEST0062,
           LC_SUCESSO          TYPE C LENGTH 1,
           ARQUIVO             TYPE REF TO ZCL_TIP_FRETE_AQ_CONF,
           LC_NR_LOTE_ADM	     TYPE ZPFE_NR_LOTE_ADM.

     DEFINE INSERT_LINE_LOG.

       CLEAR: WA_0062.
       WA_0062-NR_LOTE_ADM = &1.
       WA_0062-NR_LOTE     = &2.
       WA_0062-CHVID       = &3.
       WA_0062-NUCONTRATO  = &4.
       WA_0062-ID_TIPO     = &5.
       WA_0062-MSG_ERRO    = &6.
       WA_0062-LINHA       = &7.
       APPEND WA_0062 TO IT_0062.

       IF &8 EQ 'E'.
         MODIFY ZLEST0062 FROM WA_0062.
       ENDIF.

     END-OF-DEFINITION.

     CLEAR: IT_ZPFE_LOTE_ITEM_C, IT_ZPFE_LOTE_ITEM, IT_ZPFE_LOTE_ITEM_C[], IT_ZPFE_LOTE_ITEM[].

     DATA: URL TYPE STRING.
     DATA: T_URL TYPE STRING.
     DATA: CLIENT TYPE REF TO IF_HTTP_CLIENT.
     DATA: C_XML TYPE STRING.
     DATA: C_SEPARADOR TYPE C LENGTH 1.

     URL = I_FILENAME.

     CL_HTTP_CLIENT=>CREATE_BY_URL(
       EXPORTING
         URL                = URL
*           PROXY_HOST         =
*           PROXY_SERVICE      =
*           SSL_ID             =
*           SAP_USERNAME       =
*           SAP_CLIENT         =
       IMPORTING
         CLIENT             = CLIENT
       EXCEPTIONS
         ARGUMENT_NOT_FOUND = 1
         PLUGIN_NOT_ACTIVE  = 2
         INTERNAL_ERROR     = 3
         OTHERS             = 4 ).

     IF SY-SUBRC IS NOT INITIAL.
       RAISE EXCEPTION TYPE ZCX_ERRO_ARQUIVO
         EXPORTING
           TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
           MSGID  = SY-MSGID
           MSGNO  = SY-MSGNO
           MSGTY  = 'E'
           MSGV1  = SY-MSGV1
           MSGV2  = SY-MSGV2
           MSGV3  = SY-MSGV3
           MSGV4  = SY-MSGV4.
     ENDIF.

     CLIENT->SEND(
       EXCEPTIONS
         HTTP_COMMUNICATION_FAILURE = 1
         HTTP_INVALID_STATE         = 2
         HTTP_PROCESSING_FAILED     = 3
         HTTP_INVALID_TIMEOUT       = 4
         OTHERS                     = 5
     ).

     IF SY-SUBRC IS NOT INITIAL.
       RAISE EXCEPTION TYPE ZCX_ERRO_ARQUIVO
         EXPORTING
           TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
           MSGID  = SY-MSGID
           MSGNO  = SY-MSGNO
           MSGTY  = 'E'
           MSGV1  = SY-MSGV1
           MSGV2  = SY-MSGV2
           MSGV3  = SY-MSGV3
           MSGV4  = SY-MSGV4.
     ENDIF.

     CLIENT->RECEIVE(
       EXCEPTIONS
         HTTP_COMMUNICATION_FAILURE = 1
         HTTP_INVALID_STATE         = 2
         HTTP_PROCESSING_FAILED     = 3
         OTHERS                     = 4
     ).

     IF SY-SUBRC IS NOT INITIAL.
       RAISE EXCEPTION TYPE ZCX_ERRO_ARQUIVO
         EXPORTING
           TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
           MSGID  = SY-MSGID
           MSGNO  = SY-MSGNO
           MSGTY  = 'E'
           MSGV1  = SY-MSGV1
           MSGV2  = SY-MSGV2
           MSGV3  = SY-MSGV3
           MSGV4  = SY-MSGV4.
     ENDIF.

     C_XML = CLIENT->RESPONSE->GET_CDATA( ).

     SPLIT C_XML AT CL_ABAP_CHAR_UTILITIES=>NEWLINE INTO: TABLE IT_LINES_ARQ.

*     CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
*       EXPORTING
*         I_STRING         = C_XML
*         I_TABLINE_LENGTH = 1000
*       TABLES
*         ET_TABLE         = IT_LINES_ARQ.

     LOOP AT IT_LINES_ARQ INTO DATA(WA_LINES_ARQ).
       CLEAR: WA_ZPFE_LOTE_ITEM,
              WA_ZPFE_LOTE_ITEM_C.

       SPLIT WA_LINES_ARQ AT '|' INTO TABLE IT_LINES.
       READ TABLE IT_LINES INDEX 1 INTO DATA(WA_LINES).

       CASE WA_LINES-LINE.
         WHEN '1'.

           READ TABLE IT_LINES INDEX 4 INTO WA_LINES.
           CONDENSE WA_LINES-LINE NO-GAPS.

           CONCATENATE WA_LINES-LINE+4(4) WA_LINES-LINE+2(2) WA_LINES-LINE(2) INTO DATA(WL_DATA).
           WA_ZPFE_LOTE_ITEM-DT_CONF_ADM = WL_DATA.

           READ TABLE IT_LINES INDEX 7 INTO WA_LINES.

           CONDENSE WA_LINES-LINE NO-GAPS.
           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
               INPUT  = WA_LINES-LINE
             IMPORTING
               OUTPUT = WA_ZPFE_LOTE_ITEM-NR_LOTE_ADM.

           LC_NR_LOTE_ADM = WA_ZPFE_LOTE_ITEM-NR_LOTE_ADM.

         WHEN '2'.

           READ TABLE IT_LINES INDEX 12 INTO WA_LINES.
           CONDENSE WA_LINES-LINE NO-GAPS.
           IF WA_LINES-LINE EQ 'A'.
             CLEAR: WA_LINES.
             READ TABLE IT_LINES INDEX 13 INTO WA_LINES.
             CONDENSE WA_LINES-LINE NO-GAPS.
             IF WA_LINES-LINE IS INITIAL.
**          Inclusao de nova linha na tabela e lote itens
               WA_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM = WA_ZPFE_LOTE_ITEM-NR_LOTE_ADM.
               READ TABLE IT_LINES INDEX 8 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.
               WA_ZPFE_LOTE_ITEM_C-NR_CIOT = WA_LINES-LINE.

               READ TABLE IT_LINES INDEX 2 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.
               WA_ZPFE_LOTE_ITEM_C-NUCONTRATO = WA_LINES-LINE.

               READ TABLE IT_LINES INDEX 5 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.
               WA_ZPFE_LOTE_ITEM_C-CTENUM = WA_LINES-LINE.

               READ TABLE IT_LINES INDEX 3 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.

               CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                 EXPORTING
                   INPUT  = WA_LINES-LINE
                 IMPORTING
                   OUTPUT = WA_CHVID_ADM.

               SELECT SINGLE * INTO @DATA(WA_ZPFE_LOTE_HIS_DE)
                 FROM ZPFE_LOTE_HIS_DE
                WHERE CHVID_ADM EQ @WA_CHVID_ADM.

               IF SY-SUBRC IS INITIAL.
                 WA_ZPFE_LOTE_ITEM_C-CHVID = WA_ZPFE_LOTE_HIS_DE-CHVID_EMP.
               ENDIF.

               READ TABLE IT_LINES INDEX 4 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.
               CONCATENATE WA_LINES-LINE+4(4) WA_LINES-LINE+2(2) WA_LINES-LINE(2) INTO WA_ZPFE_LOTE_ITEM_C-DT_TRANSACAO.

               READ TABLE IT_LINES INDEX 9 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.
               WA_ZPFE_LOTE_ITEM_C-PESO_ORIGEM = WA_LINES-LINE.

               READ TABLE IT_LINES INDEX 10 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.
               WA_ZPFE_LOTE_ITEM_C-PESO_CHEGADA = WA_ZPFE_LOTE_ITEM_C-PESO_CONF_ADM = WA_LINES-LINE.

               READ TABLE IT_LINES INDEX 11 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.
               CONCATENATE WA_LINES-LINE+4(4) WA_LINES-LINE+2(2) WA_LINES-LINE(2) INTO  WA_ZPFE_LOTE_ITEM_C-DT_CHEGADA.

               WA_ZPFE_LOTE_ITEM_C-DT_BAIXA = WA_ZPFE_LOTE_ITEM_C-DT_CONF_ADM = WA_ZPFE_LOTE_ITEM-DT_CONF_ADM.

               READ TABLE IT_LINES INDEX 7 INTO WA_LINES.
               CONDENSE WA_LINES-LINE NO-GAPS.
               IF NOT WA_LINES-LINE IS INITIAL.
                 DATA(LC_CONT) = STRLEN( WA_LINES-LINE ).
                 SUBTRACT 2 FROM LC_CONT.
                 CONCATENATE WA_LINES-LINE(LC_CONT) '.' WA_LINES-LINE+LC_CONT INTO WA_LINES-LINE.
               ELSE.
                 WA_LINES-LINE  = '0.00'.
               ENDIF.

               WA_ZPFE_LOTE_ITEM_C-VL_CONF_ADM = WA_ZPFE_LOTE_ITEM_C-VL_AJUS_ADM = WA_LINES-LINE.

               SELECT SINGLE * INTO @DATA(WA_0025)
                 FROM ZLEST0025
                WHERE CHVID EQ @WA_ZPFE_LOTE_ITEM_C-CHVID.

               IF WA_0025-NATUREZACHVID EQ 'S'.
                 MULTIPLY WA_ZPFE_LOTE_ITEM_C-VL_AJUS_ADM BY -1.
               ENDIF.
               WA_ZPFE_LOTE_ITEM_C-CK_CONF_ADM = 'X'.

               APPEND WA_ZPFE_LOTE_ITEM_C TO IT_ZPFE_LOTE_ITEM_C.
             ENDIF.
           ELSE.
             READ TABLE IT_LINES INDEX 2 INTO WA_LINES.
             CONDENSE WA_LINES-LINE NO-GAPS.
             WA_ZPFE_LOTE_ITEM-NUCONTRATO = WA_LINES-LINE.

             READ TABLE IT_LINES INDEX 3 INTO WA_LINES.
             CONDENSE WA_LINES-LINE NO-GAPS.
             SHIFT WA_LINES-LINE LEFT DELETING LEADING '0'.

             CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
               EXPORTING
                 INPUT  = WA_LINES-LINE
               IMPORTING
                 OUTPUT = WA_CHVID_ADM.

             SELECT SINGLE * INTO WA_ZPFE_LOTE_HIS_DE
               FROM ZPFE_LOTE_HIS_DE
              WHERE CHVID_ADM EQ WA_CHVID_ADM.

             IF SY-SUBRC IS INITIAL.
               WA_ZPFE_LOTE_ITEM-CHVID = WA_ZPFE_LOTE_HIS_DE-CHVID_EMP.
             ENDIF.

             READ TABLE IT_LINES INDEX 7 INTO WA_LINES.
             CONDENSE WA_LINES-LINE NO-GAPS.

             CLEAR: LC_CONT.
             LC_CONT = STRLEN( WA_LINES-LINE ).
             IF LC_CONT GT 2.
               SUBTRACT 2 FROM LC_CONT.
               CONCATENATE WA_LINES-LINE(LC_CONT) '.' WA_LINES-LINE+LC_CONT(2) INTO WA_LINES-LINE.
               WA_ZPFE_LOTE_ITEM-VL_CONF_ADM = WA_LINES-LINE.
             ENDIF.

             READ TABLE IT_LINES INDEX 10 INTO WA_LINES.
             CONDENSE WA_LINES-LINE NO-GAPS.
             WA_ZPFE_LOTE_ITEM-PESO_CONF_ADM = WA_LINES-LINE.

             APPEND WA_ZPFE_LOTE_ITEM TO IT_ZPFE_LOTE_ITEM.
           ENDIF.
       ENDCASE.

     ENDLOOP.

     IF IT_ZPFE_LOTE_ITEM_C IS NOT INITIAL.

       SELECT * INTO TABLE @DATA(IT_LOTE)
         FROM ZPFE_LOTE
          FOR ALL ENTRIES IN @IT_ZPFE_LOTE_ITEM_C
        WHERE NR_LOTE_ADM EQ @IT_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM
          AND TPLOTE      EQ 'S'.

       READ TABLE IT_LOTE INDEX 1 INTO DATA(WA_LOTE).

       SELECT * INTO TABLE @DATA(IT_ITENS)
         FROM ZPFE_LOTE_ITEM
          FOR ALL ENTRIES IN @IT_ZPFE_LOTE_ITEM_C
        WHERE NR_LOTE_ADM EQ @IT_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM
          AND NM_LOTE     EQ @WA_LOTE-NM_LOTE.

       SELECT * INTO TABLE @DATA(IT_ZCTE_CIOT)
         FROM ZCTE_CIOT
          FOR ALL ENTRIES IN @IT_ZPFE_LOTE_ITEM_C
        WHERE NUCONTRATO EQ @IT_ZPFE_LOTE_ITEM_C-NUCONTRATO.

       SORT IT_ITENS BY NM_LOTE_ITEM DESCENDING.

       LOOP AT IT_ZPFE_LOTE_ITEM_C INTO WA_ZPFE_LOTE_ITEM_C.

         READ TABLE IT_ITENS INTO DATA(WA_ITENS) WITH KEY NR_LOTE_ADM = WA_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM.
         IF SY-SUBRC IS NOT INITIAL.
           CLEAR: WA_ITENS.
         ENDIF.

         READ TABLE IT_ZCTE_CIOT INTO DATA(WA_ZCTE_CIOT) WITH KEY NUCONTRATO = WA_ZPFE_LOTE_ITEM_C-NUCONTRATO.
         IF SY-SUBRC IS NOT INITIAL.
           CLEAR: WA_ZCTE_CIOT.
         ENDIF.

         WA_ZPFE_LOTE_ITEM_C-NM_LOTE_ITEM = WA_ITENS-NM_LOTE_ITEM + 1.
         WA_ZPFE_LOTE_ITEM_C-NM_LOTE      = WA_ITENS-NM_LOTE.
         WA_ZPFE_LOTE_ITEM_C-CD_CIOT      = WA_ZCTE_CIOT-CD_CIOT.
         WA_ZPFE_LOTE_ITEM_C-DOCNUM       = WA_ZCTE_CIOT-DOCNUM.
         WA_ZPFE_LOTE_ITEM_C-TKNUM        = WA_ZCTE_CIOT-TKNUM.

         CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
             INPUT  = WA_ZPFE_LOTE_ITEM_C-NM_LOTE_ITEM
           IMPORTING
             OUTPUT = WA_ZPFE_LOTE_ITEM_C-NM_LOTE_ITEM.

         LC_SUCESSO = 'X'.
         INSERT_LINE_LOG  WA_ZPFE_LOTE_ITEM_C-NR_LOTE_ADM
                          WA_ZPFE_LOTE_ITEM_C-NM_LOTE
                          WA_ZPFE_LOTE_ITEM_C-CHVID
                          WA_ZPFE_LOTE_ITEM_C-NUCONTRATO
                          'C'
                          'A linha do arquivo foi importado com sucesso'
                          SPACE
                          'S'.

         MODIFY ZPFE_LOTE_ITEM FROM WA_ZPFE_LOTE_ITEM_C.
       ENDLOOP.
       REFRESH: IT_ITENS.
     ENDIF.

     IF IT_ZPFE_LOTE_ITEM IS NOT INITIAL.

       SELECT * INTO TABLE IT_ITENS
         FROM ZPFE_LOTE_ITEM
          FOR ALL ENTRIES IN IT_ZPFE_LOTE_ITEM
        WHERE NR_LOTE_ADM EQ IT_ZPFE_LOTE_ITEM-NR_LOTE_ADM
          AND NUCONTRATO  EQ IT_ZPFE_LOTE_ITEM-NUCONTRATO
          AND CHVID       EQ IT_ZPFE_LOTE_ITEM-CHVID.

       IF IT_ITENS IS NOT INITIAL.
         SELECT * INTO TABLE @DATA(IT_ZLEST0025)
           FROM ZLEST0025
            FOR ALL ENTRIES IN @IT_ITENS
          WHERE CHVID EQ @IT_ITENS-CHVID.
       ENDIF.

       LOOP AT IT_ZPFE_LOTE_ITEM INTO WA_ZPFE_LOTE_ITEM.
         DATA(LC_TABIX) = SY-TABIX.
         ADD 1 TO LC_TABIX.

         READ TABLE IT_LINES_ARQ INDEX LC_TABIX INTO WA_LINES_ARQ.
         READ TABLE IT_ITENS INTO WA_ITENS
           WITH KEY NR_LOTE_ADM = WA_ZPFE_LOTE_ITEM-NR_LOTE_ADM
                    NUCONTRATO  = WA_ZPFE_LOTE_ITEM-NUCONTRATO
                    CHVID       = WA_ZPFE_LOTE_ITEM-CHVID.

         IF SY-SUBRC IS INITIAL.
           IF WA_ITENS-CK_CONF_ADM IS INITIAL.
             LC_TABIX = SY-TABIX.

             READ TABLE IT_ZLEST0025 INTO DATA(WA_ZLEST0025) WITH KEY CHVID = WA_ITENS-CHVID.
             IF SY-SUBRC IS INITIAL.
               MOVE: WA_ZPFE_LOTE_ITEM-DT_CONF_ADM   TO WA_ITENS-DT_CONF_ADM,
                     WA_ZPFE_LOTE_ITEM-VL_CONF_ADM   TO WA_ITENS-VL_CONF_ADM,
                     WA_ZPFE_LOTE_ITEM-PESO_CONF_ADM TO WA_ITENS-PESO_CONF_ADM,
                     'X'                             TO WA_ITENS-CK_CONF_ADM.

               IF WA_ZLEST0025-NATUREZACHVID EQ 'S'.
                 MULTIPLY WA_ITENS-VL_CONF_ADM  BY -1.
               ENDIF.

               MODIFY IT_ITENS INDEX LC_TABIX FROM WA_ITENS.
               LC_SUCESSO = 'X'.
               INSERT_LINE_LOG WA_ITENS-NR_LOTE_ADM
                               WA_ITENS-NM_LOTE
                               WA_ITENS-CHVID
                               WA_ITENS-NUCONTRATO
                               'C'
                               'A linha do arquivo foi importado com sucesso'
                               SPACE
                               'S'.
             ELSE.
               INSERT_LINE_LOG WA_ITENS-NR_LOTE_ADM
                               WA_ITENS-NM_LOTE
                               WA_ITENS-CHVID
                               WA_ITENS-NUCONTRATO
                               'C'
                               'A chave de identificação nao foi encontrada na tabela de "Controle chave de identificação de lote" ZLEST0025'
                               WA_LINES_ARQ
                               'E'.
             ENDIF.
           ELSE.
             INSERT_LINE_LOG WA_ITENS-NR_LOTE_ADM
                             WA_ITENS-NM_LOTE
                             WA_ITENS-CHVID
                             WA_ITENS-NUCONTRATO
                             'C'
                             'A linha já foi importada anteriormente. linha não importada'
                             WA_LINES_ARQ
                             'E'.

           ENDIF .
         ELSE.

           INSERT_LINE_LOG WA_ZPFE_LOTE_ITEM-NR_LOTE_ADM
                           SPACE
                           WA_ZPFE_LOTE_ITEM-CHVID
                           WA_ZPFE_LOTE_ITEM-NUCONTRATO
                           'C'
                           'A linha do documento financeiro não foi encontrado.'
                           WA_LINES_ARQ
                           'E'.
         ENDIF .

       ENDLOOP.

       IF LC_SUCESSO IS NOT INITIAL.
         IF IT_ITENS[] IS NOT INITIAL.
           MODIFY ZPFE_LOTE_ITEM FROM TABLE IT_ITENS.
         ENDIF.
       ENDIF.

       CREATE OBJECT ARQUIVO.
       ARQUIVO->SET_BUKRS( I_BUKRS = I_BUKRS ).
       ARQUIVO->SET_BRANCH( I_BRANCH = I_BRANCH  ).
       ARQUIVO->SET_DT_INICIAL( I_DT_INICIAL = I_DT_INICIAL ).
       ARQUIVO->SET_DT_FINAL( I_DT_FINAL = I_DT_FINAL ).
       ARQUIVO->SET_DS_FILENAME( I_DS_FILENAME = I_FILENAME ).
       ARQUIVO->SET_CORPO_ARQUIVO( I_CORPO_ARQUIVO = IT_LINES_ARQ ).
       ARQUIVO->SET_NR_LOTE_ADM( I_NR_LOTE_ADM = LC_NR_LOTE_ADM ).
       ARQUIVO->ZIF_CADASTRO~GRAVAR_REGISTRO( ).
     ENDIF.

   ENDMETHOD.


  METHOD SALVA_XML.

    TYPES: BEGIN OF TY_XML_VIAGEM.
    TYPES:   XML TYPE STRING.
    TYPES: END OF TY_XML_VIAGEM.

    DATA: WA_XML TYPE TY_XML_VIAGEM,
          IT_XML TYPE STANDARD TABLE OF TY_XML_VIAGEM.

    CLEAR: IT_XML.
    WA_XML-XML = I_XML.
    APPEND WA_XML TO IT_XML.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME                = I_NAME_FILE
      TABLES
        DATA_TAB                = IT_XML
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 22.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD SOLICITA_ROTA.

    DATA: E_CHAVE       TYPE CHAR32,
          LC_MSG        TYPE STRING,
          LC_MSG_ADM    TYPE STRING,
          CX_EXCEPTION  TYPE REF TO ZCX_WEBSERVICE,
          LC_XML        TYPE STRING,
          LC_HTTP       TYPE REF TO IF_HTTP_CLIENT,
          WA_ZLEST0101  TYPE ZLEST0101,
          IT_ZLEST0107  TYPE TABLE OF ZLEST0107,
          WA_ZLEST0107  TYPE ZLEST0107,
          IT_ZLEST0107B TYPE ZLEST0107_T.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0160)
      FROM ZLEST0160
     WHERE BUKRS  EQ @I_BUKRS
       AND BRANCH EQ @I_BRANCH.

    SELECT SINGLE * INTO WA_ZLEST0101
      FROM ZLEST0101
     WHERE ID_ROTA EQ I_ROTA
       AND BUKRS   EQ I_BUKRS
       AND BRANCH  EQ I_BRANCH.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E088(ZLES) WITH I_ROTA RAISING ZWEBSERVICE.
    ELSEIF WA_ZLEST0101-ID_ROTA_ADM IS NOT INITIAL.
      MESSAGE E087(ZLES) RAISING ZWEBSERVICE.
    ENDIF.

    SELECT * INTO TABLE IT_ZLEST0107
      FROM ZLEST0107
     WHERE ID_ROTA EQ I_ROTA
       AND BUKRS   EQ I_BUKRS
       AND BRANCH  EQ I_BRANCH
     ORDER BY ID_ROTA_ITEM.

    LOOP AT IT_ZLEST0107 INTO WA_ZLEST0107.
      APPEND WA_ZLEST0107 TO IT_ZLEST0107B.
    ENDLOOP.

    TRY .
        E_CHAVE = ME->CHAVE_SEGURANCA( I_GRUPO = WA_ZLEST0160-DS_GRUPO ).
      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
        LC_MSG  = CX_EXCEPTION->GET_TEXT( ).
        MESSAGE E007(ZWEBSERVICE) WITH LC_MSG.
    ENDTRY.

    LC_XML = XML_CRIAR_ROTA( I_CHAVE = E_CHAVE I_ZLEST0101 = WA_ZLEST0101 I_ZLEST0107 = IT_ZLEST0107B ).

    TRY .
        "Atribui o serviço que precisa ser consultado.
        "RO - Criar Rota.
        ME->SET_SERVICO( EXPORTING I_SERVICO = 'RO' ).

      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
        LC_MSG  = CX_EXCEPTION->GET_TEXT( ).
        MESSAGE E007(ZWEBSERVICE) WITH LC_MSG.
    ENDTRY.

    ME->SET_TIPO( EXPORTING I_TIPO = 'R').

    TRY .
        "Atribui as Informações do HTTP Client para consultar o WebService.
        LC_HTTP = ME->URL( ).

      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION .
        LC_MSG  = CX_EXCEPTION->GET_TEXT( ).
        MESSAGE E007(ZWEBSERVICE) WITH LC_MSG.
    ENDTRY.

    ME->ZIF_WEBSERVICE~ABRIR_CONEXAO( LC_HTTP ).

    "Envia para Criar Rota.
    "O retorno é de um arquivo XML com o Código da Rota Administradora.
    CALL METHOD ME->ZIF_WEBSERVICE~CONSULTAR
      EXPORTING
        I_HTTP                     = LC_HTTP
        I_XML                      = LC_XML
      RECEIVING
        E_RESULTADO                = LC_MSG_ADM
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING ZWEBSERVICE.
    ENDIF.

    "LC_MSG_ADM = ME->ZIF_WEBSERVICE~CONSULTAR( I_HTTP = LC_HTTP I_XML = LC_XML ).

    CLEAR: E_ID_ROTA_ADM, E_MSG.

    CALL METHOD ME->LER_XML_CRIAR_ROTA
      EXPORTING
        I_XML         = LC_MSG_ADM
      IMPORTING
        E_MSG         = E_MSG
        E_ID_ROTA_ADM = E_ID_ROTA_ADM.

    IF E_ID_ROTA_ADM IS NOT INITIAL.
      WA_ZLEST0101-ID_ROTA_ADM = E_ID_ROTA_ADM.
      MODIFY ZLEST0101 FROM WA_ZLEST0101.
      COMMIT WORK.
    ELSE.
      MESSAGE E_MSG TYPE 'E' RAISING ZWEBSERVICE.
    ENDIF.

  ENDMETHOD.


  METHOD XML_ATUALIZAR_ROTA.

    DATA: WA_J_1BBRANCH TYPE J_1BBRANCH.

    CLEAR: E_XML. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO WA_J_1BBRANCH
      FROM J_1BBRANCH
     WHERE BUKRS   EQ I_BUKRS
       AND BRANCH  EQ I_BRANCH.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<atualizaRota>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<tipoConsulta>T</tipoConsulta>'.

    CONC_XML '</atualizaRota>'.

  ENDMETHOD.


  METHOD XML_ATUALIZA_ROTA.

    CLEAR: E_XML. "Limpar a variavel de retorno.

    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    "Montar o arquivo XML para consultar das Rotas.
    CONC_XML '<atualizaRota>'.
    CONC_XML   '<chave>'.
    CONC_XML     I_CHAVE.
    CONC_XML   '</chave>'.
    CONC_XML   '<cnpjContratante>'.
    CONC_XML    I_CNPJCONTRATANTE.
    CONC_XML   '</cnpjContratante>'.
    CONC_XML   '<tipoConsulta>'.
    CONC_XML   'A'.
    CONC_XML   '</tipoConsulta>'.
    CONC_XML '</atualizaRota>'.

  ENDMETHOD.


  METHOD XML_AUTENTICA.
*****************
*  Descrição: Método para Montar a estrutura do arquivo XML de Autenticação.
*  Data: 07.04.2014 14:29:28
*  Developer: Victor Hugo Souza Nunes
*****************

    CLEAR: E_XML. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    "Montar o Arquivo XML da autenticação.
    CONC_XML '<autenticacao>'.
    CONC_XML  '<usuario>'.
    CONC_XML   I_USUARIO.
    CONC_XML  '</usuario>'.
    CONC_XML  '<senha>'.
    CONC_XML   I_SENHA.
    CONC_XML  '</senha>'.
    CONC_XML '</autenticacao>'.

  ENDMETHOD.


  METHOD XML_CONSULTAR_ARQ_COBRANCA.

    DATA: WA_J_1BBRANCH   TYPE J_1BBRANCH.

    CLEAR: E_XML. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO WA_J_1BBRANCH
      FROM J_1BBRANCH
     WHERE BUKRS  EQ I_BUKRS
       AND BRANCH EQ I_BRANCH.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<arquivoCobrancaPedagio>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<codigoFatura>'.
    CONC_XML I_FATURA.
    CONC_XML '</codigoFatura>'.

    CONC_XML '</arquivoCobrancaPedagio>'.

  ENDMETHOD.


  METHOD XML_CONSULTAR_ARQ_CONFERENCIA.

    DATA: WA_J_1BBRANCH   TYPE J_1BBRANCH,
          LC_DATA_INICIAL TYPE C LENGTH 8,
          LC_DATA_FINAL   TYPE C LENGTH 8.

    CLEAR: E_XML. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO WA_J_1BBRANCH
      FROM J_1BBRANCH
     WHERE BUKRS  EQ I_BUKRS
       AND BRANCH EQ I_BRANCH.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<requisicaoArquivoConferencia>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONCATENATE I_DT_INICIAL+6(2) I_DT_INICIAL+4(2) I_DT_INICIAL(4) INTO LC_DATA_INICIAL.
    CONCATENATE I_DT_FINAL+6(2)   I_DT_FINAL+4(2)   I_DT_FINAL(4)   INTO LC_DATA_FINAL.

    CONC_XML '<dataConferenciaInicio>'.
    CONC_XML LC_DATA_INICIAL.
    CONC_XML '</dataConferenciaInicio>'.

    CONC_XML '<dataConferenciaFim>'.
    CONC_XML LC_DATA_FINAL.
    CONC_XML '</dataConferenciaFim>'.

    CONC_XML '</requisicaoArquivoConferencia>'.

  ENDMETHOD.


  METHOD XML_CONSULTAR_ROTA.

    DATA: WA_J_1BBRANCH TYPE J_1BBRANCH.

    CLEAR: E_XML. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO WA_J_1BBRANCH
      FROM J_1BBRANCH
     WHERE BUKRS   EQ I_ZLEST0101-BUKRS
       AND BRANCH  EQ I_ZLEST0101-BRANCH.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<consultaRota>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<codigoRota>'.
    CONC_XML I_ZLEST0101-ID_ROTA_ADM.
    CONC_XML '</codigoRota>'.

    CONC_XML '</consultaRota>'.

  ENDMETHOD.


  METHOD XML_CONSULTA_ROTA.

  ENDMETHOD.


  METHOD XML_CONSULTA_TRANSPORTADOR.

    CLEAR: E_XML. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<situacaoTransportador>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML I_CONSULTA_RNTRC-CNPJ_CONTRATANTE.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<cnpj>'.
    CONC_XML I_CONSULTA_RNTRC-CNPJ_CONTRATADO.
    CONC_XML '</cnpj>'.

    CONC_XML '<rntrc>'.
    CONC_XML I_CONSULTA_RNTRC-RNTRC.
    CONC_XML '</rntrc>'.

    CONC_XML '<placa>'.
    CONC_XML I_CONSULTA_RNTRC-DS_PLACA.
    CONC_XML '</placa>'.

    CONC_XML '</situacaoTransportador>'.

  ENDMETHOD.


  METHOD XML_CRIAR_ROTA.

    DATA: WA_J_1BBRANCH TYPE J_1BBRANCH,
          WA_ZLEST0107  TYPE ZLEST0107.

    CLEAR: E_XML. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    SELECT SINGLE * INTO WA_J_1BBRANCH
      FROM J_1BBRANCH
     WHERE BUKRS   EQ I_ZLEST0101-BUKRS
       AND BRANCH  EQ I_ZLEST0101-BRANCH.

    "Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<rota>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML WA_J_1BBRANCH-STCD1.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<retornoOrigem>'.
    CONC_XML I_ZLEST0101-TP_ROTA_PERC.
    CONC_XML '</retornoOrigem>'.

    "Cidades """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CONC_XML '<cidades>'.
    CONC_XML '<codigoCidade>'.
    CONC_XML I_ZLEST0101-CD_CID_ORIGEM+3(7).
    CONC_XML '</codigoCidade>'.

    LOOP AT I_ZLEST0107 INTO WA_ZLEST0107.
      CONC_XML '<codigoCidade>'.
      CONC_XML WA_ZLEST0107-CD_CIDADE+3(7).
      CONC_XML '</codigoCidade>'.
    ENDLOOP.

    CONC_XML '<codigoCidade>'.
    CONC_XML I_ZLEST0101-CD_CID_DESTINO+3(7).
    CONC_XML '</codigoCidade>'.
    CONC_XML '</cidades>'.
    "Cidades """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CONC_XML '</rota>'.

  ENDMETHOD.


  METHOD check_data_envio_email.

    DATA: it_zlest0135 TYPE TABLE OF zlest0135,
          enviar_email TYPE char01,
          lv_data_lim  TYPE char14,
          lv_data_cons TYPE char14,
          lv_ret_date   TYPE tvpod-rudat,
          lv_ret_time   TYPE tvpod-rutim.


    DATA: lwa_zlest0135 TYPE zlest0135.

    lwa_zlest0135 = e_consultas.


*--------------------------------------------------------------------------------------------------------*
*  consultar a ultima atualização de envio email.
*--------------------------------------------------------------------------------------------------------*

    CLEAR: enviar_email.
    FREE: it_zlest0135.
    SELECT *
    FROM zlest0135 INTO TABLE it_zlest0135
    WHERE dt_tipbank_email EQ sy-datum.

    IF it_zlest0135 IS NOT INITIAL.
      SORT it_zlest0135 BY dt_tipbank_email hr_tipbank_email DESCENDING.
      READ TABLE it_zlest0135 INTO DATA(ws_zlest0135) INDEX 1.
      IF ws_zlest0135-dt_tipbank_email IS NOT INITIAL.

        "Verificar se hora que esta na tabela é maior que 6 hora, se for maior devera consultar novamente a API.
        CALL FUNCTION 'TSTR_CALC_TIME'
          EXPORTING
            iv_begin_datelocal_req   = sy-datum
            iv_begin_timelocal_req   = sy-uzeit
            iv_duration_integer      = 1800 "Data atual menos 30 minutos.
            iv_direction             = '-'
          IMPORTING
            ev_end_datelocal         = lv_ret_date
            ev_end_timelocal         = lv_ret_time
          EXCEPTIONS
            fatal_error              = 1
            time_invalid             = 2
            time_missing             = 3
            tstream_not_loadable     = 4
            tstream_generation_error = 5
            parameter_error          = 6
            unspecified_error        = 7
            OTHERS                   = 8.


        lv_data_lim   = |{ lv_ret_date }{ lv_ret_time }|.
        lv_data_cons  = |{ ws_zlest0135-dt_atualizacao }{ ws_zlest0135-hr_atualizacao }|.


        IF  lv_data_cons > lv_data_lim.
          enviar_email = abap_false.
        ELSE.
          enviar_email = abap_true.
        ENDIF.

      ELSE.
        enviar_email = abap_true.
      ENDIF.

    ELSE.
      enviar_email = abap_true.
    ENDIF.


*--------------------------------------------------------------------------------------------------------*
*   "Envio email/ Registrar ultimo envio.
*--------------------------------------------------------------------------------------------------------*
    IF enviar_email EQ abap_true.

      lwa_zlest0135-dt_tipbank_email = sy-datum.
      lwa_zlest0135-hr_tipbank_email = sy-uzeit.

      MODIFY  zlest0135 FROM lwa_zlest0135.
      COMMIT WORK.

      me->enviar_email_tip( e_consultas = lwa_zlest0135 ).

    ENDIF.

    CLEAR: ws_zlest0135, enviar_email.

  ENDMETHOD.


  METHOD cons_status_parceiro.

    DATA: lc_webservice TYPE REF TO zcl_webservice_tipcard.


    TYPES: BEGIN OF ty_dados,
             cd_transportador TYPE  lifnr,
             ds_placa         TYPE  zpc_veiculo,
             cnpj_contratante TYPE  stcd1,
             cnpj_contratado  TYPE  stcd1,
             cpf_contratado   TYPE  stcd1.
    TYPES END OF ty_dados.

    DATA: ws_dados         TYPE zde_consulta_parceiro,
          it_dados         TYPE TABLE OF zde_consulta_parceiro,
          e_chave          TYPE char32,
          cx_exception     TYPE REF TO zcx_webservice,
          lc_msg           TYPE string,
          lc_xml           TYPE string,
          lc_msg_adm       TYPE string,
          i_name_file      TYPE string,
          it_consultas     TYPE TABLE OF zde_consulta_rntrc,
          wa_consultas     TYPE zde_consulta_rntrc,
          wa_j_1bbranch    TYPE j_1bbranch,
          wa_lfa1          TYPE lfa1,
          it_lfa1          TYPE TABLE OF lfa1,
          lc_http          TYPE REF TO if_http_client,
          wa_zlest0135     TYPE zlest0135,
          wa_zlest0135_aux TYPE zlest0135,
          it_zlest0135     TYPE TABLE OF zlest0135,
          i_parceiro       TYPE j_1bparid.


    FREE: it_zlest0135.

*--------------------------------------------------------------------------------------------------------*
*   "Dados de acesso token.
*--------------------------------------------------------------------------------------------------------*

    SELECT SINGLE * INTO @DATA(wa_zlest0160)
      FROM zlest0160
     WHERE bukrs   EQ @i_bukrs
       AND branch  EQ @i_branch.

    CREATE OBJECT lc_webservice.

    TRY .
        e_chave = lc_webservice->chave_seguranca( i_grupo = wa_zlest0160-ds_grupo ).
      CATCH zcx_webservice INTO cx_exception .
        lc_msg  = cx_exception->get_text( ).
        MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
    ENDTRY.

*--------------------------------------------------------------------------------------------------------*
*   "Dados da empresa contratante.
*--------------------------------------------------------------------------------------------------------*

    SELECT SINGLE * INTO wa_j_1bbranch FROM j_1bbranch
      WHERE bukrs  EQ i_bukrs
        AND branch EQ i_branch.

    IF sy-subrc EQ 0.

*--------------------------------------------------------------------------------------------------------*
*   "Dados da empresa contratada / proprietario do veiculo.
*--------------------------------------------------------------------------------------------------------*

      CLEAR: wa_lfa1.
      SELECT SINGLE * FROM lfa1 AS a
      INNER JOIN zlest0002 AS b ON b~proprietario EQ a~lifnr
      INTO CORRESPONDING FIELDS OF wa_lfa1
      WHERE pc_veiculo EQ i_placa.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE e113(zles) WITH i_placa RAISING erro.
      ENDIF.

      ws_dados-cd_transportador = wa_lfa1-lifnr.
      ws_dados-ds_placa         = i_placa.
      ws_dados-cnpj_contratante = wa_j_1bbranch-stcd1.


      CASE wa_lfa1-stkzn.
        WHEN abap_true.
          ws_dados-cpf_contratado  = wa_lfa1-stcd2.
        WHEN abap_false.
          ws_dados-cnpj_contratado  = wa_lfa1-stcd1.
      ENDCASE.

      APPEND ws_dados TO it_dados.

*--------------------------------------------------------------------------------------------------------*
*   Dados do endpoint para realizar a consulta na API.
*--------------------------------------------------------------------------------------------------------*

      TRY .
          lc_webservice->set_servico( EXPORTING i_servico = 'SP' ).
        CATCH zcx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.

      lc_webservice->set_tipo( EXPORTING i_tipo = 'S').

      TRY .
          lc_http = lc_webservice->url( ).
        CATCH zcx_webservice INTO cx_exception .
          lc_msg  = cx_exception->get_text( ).
          MESSAGE e007(zwebservice) WITH lc_msg RAISING erro.
      ENDTRY.


*--------------------------------------------------------------------------------------------------------*
*  Consultar a API.
*--------------------------------------------------------------------------------------------------------*
      CLEAR: ws_dados.
      LOOP AT it_dados INTO ws_dados.

        CLEAR: lc_msg_adm, wa_zlest0135_aux,  wa_zlest0135.
        SELECT SINGLE * FROM zlest0135 INTO wa_zlest0135
          WHERE ds_placa EQ ws_dados-ds_placa
            AND cd_transportador EQ ws_dados-cd_transportador.
        IF sy-subrc NE 0.
          wa_zlest0135-cd_transportador  = ws_dados-cd_transportador.
          wa_zlest0135-ds_placa          = ws_dados-ds_placa.
        ENDIF.

        lc_webservice->zif_webservice~abrir_conexao( lc_http ).

        lc_xml = lc_webservice->xml_consulta_status_parceiro( EXPORTING i_chave = e_chave i_consulta = ws_dados ).

        IF lc_webservice->ck_salvar_xml_local EQ abap_true.
          i_name_file = 'C:\Maggi\TipFrete\consultaParceiroReq.xml'.
          CALL METHOD lc_webservice->salva_xml
            EXPORTING
              i_name_file = i_name_file
              i_xml       = lc_xml.
        ENDIF.


        CALL METHOD lc_webservice->zif_webservice~consultar
          EXPORTING
            i_http                     = lc_http
            i_xml                      = lc_xml
          RECEIVING
            e_resultado                = lc_msg_adm
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING webservice.
        ENDIF.

*--------------------------------------------------------------------------------------------------------*
*  Retorno da API.
*--------------------------------------------------------------------------------------------------------*
        IF lc_webservice->ck_salvar_xml_local EQ abap_true.
          i_name_file = 'C:\Maggi\TipFrete\consultaParceiroReq.xml'.
          CALL METHOD lc_webservice->salva_xml
            EXPORTING
              i_name_file = i_name_file
              i_xml       = lc_msg_adm.
        ENDIF.

        wa_zlest0135_aux = lc_webservice->ler_xml_status_parceiro( EXPORTING i_xml = lc_msg_adm ).

        IF wa_zlest0135_aux-tp_transportador IS NOT INITIAL.
          CASE wa_zlest0135_aux-tp_transportador.
            WHEN '1'.
              wa_zlest0135-tp_transportador  = '1'.
              wa_zlest0135-ck_etc_equiparado = ''.  "vazio
            WHEN '2'.
              wa_zlest0135-tp_transportador  = '1'.
              wa_zlest0135-ck_etc_equiparado = 'X'.  "
            WHEN '3'.
              wa_zlest0135-tp_transportador  = '2'.
              wa_zlest0135-ck_etc_equiparado = ''.  "Vazio
            WHEN '4'.
              wa_zlest0135-tp_transportador  = '2'.
              wa_zlest0135-ck_etc_equiparado = 'X'.  "
            WHEN '5'.
              wa_zlest0135-tp_transportador  = '3'.
              wa_zlest0135-ck_etc_equiparado = ''.  "vazio
            WHEN '6'.
              wa_zlest0135-tp_transportador  = '3'.
              wa_zlest0135-ck_etc_equiparado = 'X'.  "
            WHEN OTHERS.
          ENDCASE.

          wa_zlest0135-dt_atualizacao  = sy-datum.
          wa_zlest0135-hr_atualizacao  = sy-uzeit.
          wa_zlest0135-consulta_base_tipbank  = abap_true.

          APPEND wa_zlest0135 TO it_zlest0135.
          CLEAR: wa_zlest0135, ws_dados.
        ELSE.
          APPEND VALUE #(
          cd_transportador  = ws_dados-cd_transportador
          ds_placa          = ws_dados-ds_placa ) TO e_consultas.
        ENDIF.
      ENDLOOP.

      IF it_zlest0135[] IS NOT INITIAL.
        MOVE it_zlest0135[] TO e_consultas[].
        MODIFY zlest0135 FROM TABLE it_zlest0135.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD enviar_email_tip.

    DATA: t_html TYPE string,
          t_host TYPE sy-host.

    DATA: lt_to        TYPE crmt_email_recipients,
          lt_copy      TYPE crmt_email_recipients,
          ls_recep     TYPE crms_email_recipient,
          lt_mail_body TYPE crmt_email_mime_struc,
          ls_mail_body TYPE crms_email_mime_struc,
          dt           TYPE char10,
          hr           TYPE char8,
          lv_activity  TYPE sysuuid_x.

    DATA: lo_create_mail TYPE REF TO cl_crm_email_data.
    CREATE OBJECT lo_create_mail.

    CLEAR: dt, hr.

    "Verificar endereço email.
    SELECT *
    FROM tvarvc INTO TABLE @DATA(it_stvarv)
    WHERE name = 'SIT_TRANSP_EMAIL'. "Sit_Transp_email

    CHECK it_stvarv IS NOT INITIAL.

    dt = |{ e_consultas-dt_tipbank_email+6(2) }.{ e_consultas-dt_tipbank_email+4(2) }.{ e_consultas-dt_tipbank_email+0(4) }|.
    hr = |{ e_consultas-hr_tipbank_email+0(2) }:{ e_consultas-hr_tipbank_email+2(2) }:{ e_consultas-hr_tipbank_email+4(2) }|.

    CLEAR t_html.
    CONCATENATE t_html '<html>'  INTO t_html.
    CONCATENATE t_html '<head>'  INTO t_html.
    CONCATENATE t_html '</head>' INTO t_html.
    CONCATENATE t_html '<body>'  INTO t_html.
    TRANSLATE t_host TO UPPER CASE.
    CONCATENATE t_html '<div><font face=Verdana size=4>' t_host '</font></div>'            INTO t_html.
    CONCATENATE t_html '<div><font face=Verdana size=3>Sistema de consulta da ANTT</font></div>' INTO t_html.
    CONCATENATE t_html '<br><br>'                               INTO t_html.
    CONCATENATE t_html '<align=left>&nbsp;</div>'               INTO t_html.
    CONCATENATE t_html '<div><p>O Sistema de consulta da ANTT entrou em contingencia desde' dt  'as ' hr '</p></div>' INTO t_html SEPARATED BY space.
    CONCATENATE t_html '<div><p>As consultas atuais serão em base ao histórico da TIPBANK, ate o retorno do Serviço.</p></div>' INTO t_html.
    CONCATENATE t_html '</body>'  INTO t_html.
    CONCATENATE t_html '</html>'  INTO t_html.

    lo_create_mail->subject =  'Sistema de consulta da ANTT'.

    CLEAR ls_mail_body.
    ls_mail_body-content_ascii = t_html.
    ls_mail_body-mime_type     = 'text/html'.
    APPEND ls_mail_body TO lt_mail_body.

    MOVE lt_mail_body TO lo_create_mail->body.

    LOOP AT it_stvarv INTO DATA(ws_end_email).
      CLEAR ls_recep.
      ls_recep-address = ws_end_email-low.
      APPEND ls_recep TO lt_to.
    ENDLOOP.

    MOVE lt_to TO lo_create_mail->to.

    ls_recep-address = 'suporte.sap@amaggi.com.br'.
    APPEND ls_recep TO lt_copy.

    MOVE lt_copy TO lo_create_mail->copy.

    CLEAR ls_recep.
    ls_recep-address = ''.
    MOVE ls_recep TO lo_create_mail->from.

    CALL METHOD cl_crm_email_utility_base=>send_email
      EXPORTING
        iv_mail_data       = lo_create_mail
      RECEIVING
        ev_send_request_id = lv_activity.

    COMMIT WORK.
  ENDMETHOD.


  METHOD ler_xml_status_parceiro.

    DATA: if_xml           TYPE REF TO if_ixml,
          if_document      TYPE REF TO if_ixml_document,
          if_streamfactory TYPE REF TO if_ixml_stream_factory,
          if_stream        TYPE REF TO if_ixml_istream,
          if_xml_parser    TYPE REF TO if_ixml_parser,
          if_node          TYPE REF TO if_ixml_node,
          iterator         TYPE REF TO if_ixml_node_iterator.

    DATA: tag_name      TYPE string,
          valor_dom     TYPE string,
          filho         TYPE REF TO if_ixml_node_list,
          iterator2     TYPE REF TO if_ixml_node_iterator,
          if_node_filho TYPE REF TO if_ixml_node.

    CLEAR: e_zlest0135.


    if_xml           = cl_ixml=>create( ).
    if_document      = if_xml->create_document( ).
    if_streamfactory = if_xml->create_stream_factory( ).
    if_stream        = if_streamfactory->create_istream_string( i_xml ).
    if_xml_parser    = if_xml->create_parser(  stream_factory = if_streamfactory istream = if_stream document = if_document ).
    if_xml_parser->parse( ).

    if_node ?= if_document->get_root_element( ).

    IF NOT ( if_node IS INITIAL ).

      iterator = if_node->create_iterator( ).
      if_node  = iterator->get_next( ).

      WHILE NOT if_node IS INITIAL.
        CASE if_node->get_type( ).
          WHEN: if_ixml_node=>co_node_element.
            tag_name = if_node->get_name( ).

            CASE tag_name.
              WHEN 'consultaParceiroRetorno'.
                filho         = if_node->get_children( ).
                iterator2     = filho->create_iterator( ).
                if_node_filho = iterator2->get_next( ).

                WHILE NOT if_node_filho IS INITIAL.
                  tag_name  = if_node_filho->get_name( ).
                  valor_dom = if_node_filho->get_value( ).
                  CASE tag_name.
                    WHEN 'tipoContratado'.
                      e_zlest0135-tp_transportador = valor_dom.
                  ENDCASE.
                  if_node_filho = iterator2->get_next( ).
                ENDWHILE.
            ENDCASE.
        ENDCASE.

        if_node = iterator->get_next( ).
      ENDWHILE.

    ENDIF.
  ENDMETHOD.


  method XML_CONSULTA_STATUS_PARCEIRO.

    CLEAR: E_XML. "Limpar o retorno.

    "Criar uma MACRO para concatenar as informações do arquivo XML.
    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

*    Montar o Arquivo XML da Solicitação de Nova Rota
    CONC_XML '<consultaParceiro>'.

    CONC_XML '<chave>'.
    CONC_XML I_CHAVE.
    CONC_XML '</chave>'.

    CONC_XML '<cnpjContratante>'.
    CONC_XML I_CONSULTA-CNPJ_CONTRATANTE.
    CONC_XML '</cnpjContratante>'.

    CONC_XML '<numeroCNPJ>'.
    CONC_XML I_CONSULTA-CNPJ_CONTRATADO.
    CONC_XML '</numeroCNPJ>'.

    CONC_XML '<numeroCPF>'.
    CONC_XML I_CONSULTA-CPF_CONTRATADO.
    CONC_XML '</numeroCPF>'.

    CONC_XML '<tipo>'.
    CONC_XML '02'.
    CONC_XML '</tipo>'.

    CONC_XML '</consultaParceiro>'.
  endmethod.
ENDCLASS.
