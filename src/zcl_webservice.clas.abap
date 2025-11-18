CLASS zcl_webservice DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZCL_WEBSERVICE
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES zif_webservice .

    DATA at_url TYPE string .
    DATA at_url_token TYPE string .

    METHODS constructor .
    METHODS destructor .
    METHODS set_servico
      IMPORTING
        !i_servico TYPE ztipowebserv
      RAISING
        zcx_webservice .
    METHODS get_servico
      RETURNING
        VALUE(e_servico) TYPE ztipowebserv .
    METHODS set_tipo
      IMPORTING
        !i_tipo TYPE ztipowebadm .
    METHODS get_tipo
      RETURNING
        VALUE(e_tipo) TYPE ztipowebadm .
    METHODS get_senha
      RETURNING
        VALUE(e_senha) TYPE zde_ui_src_password60
      RAISING
        zcx_webservice .
    METHODS get_uri
      RETURNING
        VALUE(e_uri) TYPE zurlwebadm
      RAISING
        zcx_webservice .
    METHODS get_usuario
      RETURNING
        VALUE(e_usuario) TYPE zde_usuario
      RAISING
        zcx_webservice .
    METHODS url
      IMPORTING
        !i_url        TYPE zurlwebadm OPTIONAL
        !i_add_url    TYPE string OPTIONAL
      RETURNING
        VALUE(e_http) TYPE REF TO if_http_client
      RAISING
        zcx_webservice .
    METHODS set_senha
      IMPORTING
        !i_senha TYPE string .
    METHODS set_usuario
      IMPORTING
        !i_usuario TYPE string .
  PROTECTED SECTION.
*"* protected components of class ZCL_WEBSERVICE
*"* do not include other source files here!!!
private section.

  aliases AUTENTICA_MODULE
    for ZIF_WEBSERVICE~AUTENTICA_MODULE .

*"* private components of class ZCL_WEBSERVICE
*"* do not include other source files here!!!
  data AT_SERVICO type ZTIPOWEBSERV .
  data AT_TIPO type ZTIPOWEBADM .
  data AT_HTTP type ref to IF_HTTP_CLIENT .
  data AT_CONTENT_TYPE type ZDE_WEB_SERV_CTX .
  data AT_METHOD type STRING .
  data AT_USUARIO type STRING .
  data AT_SENHA type STRING .
ENDCLASS.



CLASS ZCL_WEBSERVICE IMPLEMENTATION.


  METHOD constructor.
    "Limpar os atributos quando o objeto for criado.
    CLEAR: me->at_servico,
           me->at_tipo,
           me->at_http,
           me->at_content_type,
           me->zif_webservice~autentica_opus,
           me->zif_webservice~nm_auth_webservice,
           me->zif_webservice~ck_usa_auth_webservice.

    me->zif_webservice~autentica_opus = abap_false.
    me->zif_webservice~ck_usa_auth_webservice = abap_false.

  ENDMETHOD.


  METHOD destructor.



  ENDMETHOD.


  METHOD get_senha.

    IF at_senha IS NOT INITIAL.
      e_senha = at_senha.
    ELSE.

      CASE me->zif_webservice~ck_usa_auth_webservice.
        WHEN abap_false.

          DATA(var_servico) = me->get_servico( ). "Atribuir o Serviço que vai ser selecionado.
          DATA(var_tipo)    = me->get_tipo( ).    "Atribuir o Tipo de Serviço que vai ser selecionado.

          IF NOT ( var_servico IS INITIAL ) AND NOT ( var_tipo IS INITIAL ).
            "Selecionar na tabela o serviço e tipo de serviço que vai ser utilizado.
            SELECT SINGLE * FROM zciot_webservice
              INTO @DATA(gw_webservice)
             WHERE servico EQ @var_servico
               AND tipo    EQ @var_tipo.

            IF ( sy-subrc IS INITIAL ). "Caso o endereço seja encontrado.
              e_senha = gw_webservice-senha. "Atribuir o Endereço URL cadastrado.
            ELSE.
              RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>url_nao_encontrado.
            ENDIF.
          ENDIF.

        WHEN abap_true.

          SELECT SINGLE * INTO @DATA(wa_zauth_webservice)
            FROM zauth_webservice
           WHERE service EQ @me->zif_webservice~nm_auth_webservice.

          IF sy-subrc IS NOT INITIAL.
            RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>url_nao_encontrado.
          ENDIF.

          e_senha = wa_zauth_webservice-password.

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD get_servico.
************************************
*  Método de Acesso
*  Retorno: E_SERVICO
*  Descrição: Método para capturar o serviço que retornar do cadastro ZLEST0096.
*  Developer: Victor Hugo Souza Nunes
*  03.04.2014 10:09:28
************************************
    e_servico = me->at_servico.
  ENDMETHOD.


  METHOD get_tipo.
************************************
*  Método de Acesso
*  Atributo: AT_TIPO
*  Parâmetro:
*  Retorno: E_TIPO
*  Descrição: Método para capturar o tipo de serviço que vai ser utilizado na seleção da URL (endereço do webservice).
*  Developer: Victor Hugo Souza Nunes
*  03.04.2014 10:16:54
************************************
    e_tipo = me->at_tipo.
  ENDMETHOD.


  METHOD get_uri.

    CASE me->zif_webservice~ck_usa_auth_webservice.
      WHEN abap_false.

        DATA(var_servico) = me->get_servico( ). "Atribuir o Serviço que vai ser selecionado.
        DATA(var_tipo)    = me->get_tipo( ).    "Atribuir o Tipo de Serviço que vai ser selecionado.

        IF NOT ( var_servico IS INITIAL ) AND NOT ( var_tipo IS INITIAL ).
          "Selecionar na tabela o serviço e tipo de serviço que vai ser utilizado.
          SELECT SINGLE * FROM zciot_webservice
            INTO @DATA(gw_webservice)
           WHERE servico EQ @var_servico
             AND tipo    EQ @var_tipo.

          IF ( sy-subrc IS INITIAL ). "Caso o endereço seja encontrado.
            me->at_url = gw_webservice-url.
            me->set_senha( i_senha = CONV #( gw_webservice-senha ) ).
            me->set_usuario( i_usuario = CONV #( gw_webservice-usuario ) ).
            me->at_content_type = gw_webservice-content_type.
            "ME->AT_METHOD = GW_WEBSERVICE-METHOD.
            me->at_url_token = gw_webservice-url_token.
            e_uri = gw_webservice-url. "Atribuir o Endereço URL cadastrado.
          ELSE.
            RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>url_nao_encontrado.
          ENDIF.
        ENDIF.

      WHEN abap_true.

        SELECT SINGLE * INTO @DATA(wa_zauth_webservice)
          FROM zauth_webservice
         WHERE service EQ @me->zif_webservice~nm_auth_webservice.

        IF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>url_nao_encontrado.
        ENDIF.

        me->at_url = wa_zauth_webservice-url.
        me->set_senha( i_senha = CONV #( wa_zauth_webservice-password ) ).
        me->set_usuario( i_usuario = CONV #( wa_zauth_webservice-username ) ).
        me->at_content_type = wa_zauth_webservice-content_type.
        me->at_method = wa_zauth_webservice-method.
        me->at_url_token = wa_zauth_webservice-url_token.
        e_uri = wa_zauth_webservice-url.

    ENDCASE.

  ENDMETHOD.


  METHOD get_usuario.

    IF at_usuario IS NOT INITIAL.
      e_usuario = at_usuario.
    ELSE.

      CASE me->zif_webservice~ck_usa_auth_webservice.
        WHEN abap_false.

          DATA(var_servico) = me->get_servico( ). "Atribuir o Serviço que vai ser selecionado.
          DATA(var_tipo)    = me->get_tipo( ).    "Atribuir o Tipo de Serviço que vai ser selecionado.

          IF NOT ( var_servico IS INITIAL ) AND NOT ( var_tipo IS INITIAL ).
            "Selecionar na tabela o serviço e tipo de serviço que vai ser utilizado.
            SELECT SINGLE * FROM zciot_webservice
              INTO @DATA(gw_webservice)
             WHERE servico EQ @var_servico
               AND tipo    EQ @var_tipo.

            IF ( sy-subrc IS INITIAL ). "Caso o endereço seja encontrado.
              e_usuario = gw_webservice-usuario. "Atribuir o Endereço URL cadastrado.
            ELSE.
              RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>url_nao_encontrado.
            ENDIF.
          ENDIF.

        WHEN abap_true.

          SELECT SINGLE * INTO @DATA(wa_zauth_webservice)
            FROM zauth_webservice
           WHERE service EQ @me->zif_webservice~nm_auth_webservice.

          IF sy-subrc IS NOT INITIAL.
            RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>url_nao_encontrado.
          ENDIF.

          e_usuario = wa_zauth_webservice-username.

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD set_senha.
    me->at_senha = i_senha.
  ENDMETHOD.


  METHOD set_servico.
************************************
*  Método de Configuração
*  Atributo: AT_SERVICO
*  Parâmetro: I_SERVICO
*  Descrição: Este método tem como objetivo a configuração do atributo de serviço e a verificação se o mesmo já esta cadastrado na ZLES0096.
*  Developer: Victor Hugo Souza Nunes
*  03.04.2014 10:04:23
************************************

    DATA:  gw_webservice TYPE zciot_webservice. "Work Area para guardar informações selecionadas da tabela ZCIOT_WEBSERVICE.
    CLEAR: gw_webservice. "Limpar a Work Area.

    "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
    SELECT SINGLE * FROM zciot_webservice INTO gw_webservice WHERE servico EQ i_servico.

    "Caso o registro seja encontrado atribuir ao atributor AT_SERVICO o serviço selecionado.
    IF ( sy-subrc EQ 0 ).
      me->at_servico      = gw_webservice-servico.
      IF gw_webservice-content_type IS NOT INITIAL.
        me->at_content_type = gw_webservice-content_type.
      ELSE.
        me->at_content_type = 'text/xml; charset=UTF-8'.
      ENDIF.
    ELSE. "Se o serviço não for encontrado no cadastro enviar uma Exception para ser tratada.
      RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>servico_nao_encontrado.
    ENDIF.

  ENDMETHOD.


  METHOD set_tipo.
************************************
*  Método de Configuração
*  Atributo: AT_TIPO
*  Parâmetro: I_TIPO
*  Descrição: Método para configurar o tipo de serviço que vai ser utilizado na seleção da URL (endereço do webservice).
*  Developer: Victor Hugo Souza Nunes
*  03.04.2014 10:11:35
************************************
    me->at_tipo = i_tipo.
  ENDMETHOD.


  METHOD set_usuario.
    me->at_usuario = i_usuario.
  ENDMETHOD.


  METHOD url.
************************************
*  Método de Acesso
*  Atributo:
*  Parâmetro:
*  Retorno: E_HTTP
*  Descrição: Método utilizado para retornar a URL que vai ser consumida no WebService.
*  Developer: Victor Hugo Souza Nunes
*  03.04.2014 10:27:56
************************************

    DATA: gw_webservice TYPE zciot_webservice. "Tabela de WebServices para Viagem Adiministradora
    DATA: var_servico TYPE ztipowebserv, "Tipo de Serviço WebService
          var_tipo    TYPE ztipowebadm, "Tipo de WebService
          var_url     TYPE string. "Endereço do WebService.

    IF i_url IS INITIAL.

      CASE me->zif_webservice~ck_usa_auth_webservice.
        WHEN abap_false.

          CLEAR: var_servico, var_tipo.

          var_servico = me->get_servico( ). "Atribuir o Serviço que vai ser selecionado.
          var_tipo    = me->get_tipo( ).    "Atribuir o Tipo de Serviço que vai ser selecionado.

          IF NOT ( var_servico IS INITIAL ) AND NOT ( var_tipo IS INITIAL ).

            "Selecionar na tabela o serviço e tipo de serviço que vai ser utilizado.
            SELECT SINGLE * FROM zciot_webservice INTO gw_webservice WHERE servico EQ var_servico
                                                                       AND tipo    EQ var_tipo.

            IF ( sy-subrc EQ 0 ). "Caso o endereço seja encontrado.

              var_url = gw_webservice-url && i_add_url. "Atribuir o Endereço URL cadastrado.
              me->at_url = var_url.
              me->at_url_token = gw_webservice-url_token.
              me->at_content_type = gw_webservice-content_type.

              "Criar um novo Objeto HTTP Client.
              "Passando o endereço URL cadastrado na ZLES0096 para consumir o WebService.
              "O retorno é um HTTP Client.
              cl_http_client=>create_by_url(
                                             EXPORTING url    = var_url
                                             IMPORTING client = e_http
                                             EXCEPTIONS argument_not_found = 1
                                                        plugin_not_active  = 2
                                                        internal_error     = 3
                                           ).
              CASE sy-subrc.
                WHEN: 1.
                  "Este erro lança uma exception de "Argumento não encontrado" Caso a URL não seja informada.
                  "na passagem de parametro da CREATE_BY_URL.
                  RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>argumento_nao_encontrado.
                WHEN: 2.
                  "Este erro lança uma expcetion de "Plugin não ativo", quando a comunicação HTTP não esteja ativa no SAP.
                  RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>plugin_nao_ativo.
                WHEN: 3.
                  "Este erro lança uma exception generica "Erro Interno" do SAP.
                  RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>erro_interno.
              ENDCASE.

            ELSE.
              "Quando uma URL não for encontrada no cadastro da ZLES0096 (tabela: ZCIOT_WEBSERVICE)
              "Este erro vai lançar uma exception informando ao sistema que não foi encontrado o endereço URL.
              RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>url_nao_encontrado.
            ENDIF.
          ENDIF.

        WHEN abap_true.

          SELECT SINGLE * INTO @DATA(wa_zauth_webservice)
            FROM zauth_webservice
           WHERE service EQ @me->zif_webservice~nm_auth_webservice.

          IF sy-subrc IS NOT INITIAL.
            RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>url_nao_encontrado.
          ENDIF.

          var_url = wa_zauth_webservice-url && i_add_url.
          me->at_url = var_url.
          me->set_senha( i_senha = CONV #( wa_zauth_webservice-password ) ).
          me->set_usuario( i_usuario = CONV #( wa_zauth_webservice-username ) ).
          me->at_content_type = wa_zauth_webservice-content_type.
          me->at_method = wa_zauth_webservice-method.
          me->at_url_token = wa_zauth_webservice-url_token.

          cl_http_client=>create_by_url(
                                         EXPORTING url    = var_url
                                         IMPORTING client = e_http
                                         EXCEPTIONS argument_not_found = 1
                                                    plugin_not_active  = 2
                                                    internal_error     = 3
                                       ).
          CASE sy-subrc.
            WHEN: 1.
              "Este erro lança uma exception de "Argumento não encontrado" Caso a URL não seja informada.
              "na passagem de parametro da CREATE_BY_URL.
              RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>argumento_nao_encontrado.
            WHEN: 2.
              "Este erro lança uma expcetion de "Plugin não ativo", quando a comunicação HTTP não esteja ativa no SAP.
              RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>plugin_nao_ativo.
            WHEN: 3.
              "Este erro lança uma exception generica "Erro Interno" do SAP.
              RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>erro_interno.
          ENDCASE.

      ENDCASE.

    ELSE.
      var_url = i_url. "Atribuir o Endereço URL cadastrado.

      me->at_url = i_url.

      "Criar um novo Objeto HTTP Client.
      "Passando o endereço URL cadastrado na ZLES0096 para consumir o WebService.
      "O retorno é um HTTP Client.
      cl_http_client=>create_by_url( EXPORTING url    = var_url
                                     IMPORTING client = e_http
                                     EXCEPTIONS argument_not_found = 1
                                                plugin_not_active  = 2
                                                internal_error     = 3 ).
      CASE sy-subrc.
        WHEN: 1.
          "Este erro lança uma exception de "Argumento não encontrado" Caso a URL não seja informada.
          "na passagem de parametro da CREATE_BY_URL.
          RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>argumento_nao_encontrado.
        WHEN: 2.
          "Este erro lança uma expcetion de "Plugin não ativo", quando a comunicação HTTP não esteja ativa no SAP.
          RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>plugin_nao_ativo.
        WHEN: 3.
          "Este erro lança uma exception generica "Erro Interno" do SAP.
          RAISE EXCEPTION TYPE zcx_webservice EXPORTING textid = zcx_webservice=>erro_interno.
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD zif_webservice~abrir_conexao.
*****************
*  Descrição: Método para atribuir e abrir a conexão com o HTTP, assim comunicando com o WebService.
*             utilizando da interface Standard IF_HTTP_CLIENT
*  Data: 03.04.2014 13:09:29
*****************

    DATA: lc_content_type TYPE string.

    "Adicionar no cabeçalho da requisição qual é o tipo da mesma.
    "No caso sera utilizado POST sempre.
    "POST - Submits data to be processed to a specified resource
    "Documentação: http://www.w3schools.com/tags/ref_httpmethods.asp
    CALL METHOD i_http->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = 'POST'.

    "Adicionar no cabeçalho da requisição qual é o tipo de protocolo.
    "Neste caso o protocolo a ser utilizado é o HTTP versão 1.1
    "Documentação: http://www.w3.org/Protocols/rfc2616/rfc2616.html
    CALL METHOD i_http->request->set_header_field
      EXPORTING
        name  = '~server_protocol'
        value = 'HTTP/1.1'.

    "Adicionar no cabeçalho qual é o Tipo de documento que vai ser vinculado.
    "Neste caso vai esta passando o tipo XML com CHARSET UTF 8.
    "http://www.w3.org/TR/xhtml-media-types/
    "https://www.w3.org/International/O-charset.pt-br.php
    MOVE me->at_content_type TO lc_content_type.

    CALL METHOD i_http->request->set_header_field
      EXPORTING
        name  = 'Content-Type'
        value = lc_content_type.

    IF me->at_method IS NOT INITIAL.
      CASE me->at_method.
        WHEN if_http_request=>co_request_method_get.
          i_http->request->set_method( method = if_http_request=>co_request_method_get ).
        WHEN if_http_request=>co_request_method_post.
          i_http->request->set_method( method = if_http_request=>co_request_method_post ).
        WHEN OTHERS.
          i_http->request->set_method( method = me->at_method ).
      ENDCASE.

    ENDIF.

    IF i_autenticar EQ abap_true.
      i_http->authenticate( username = me->get_usuario( ) password = me->get_senha( ) ).
    ENDIF.

    i_http->request->set_header_field(
      EXPORTING
        name  = 'Accept-Charset'
        value = 'utf-8' ).

  ENDMETHOD.


  METHOD zif_webservice~add_token_api_ad_http_cliente.

    zcl_webservice=>zif_webservice~get_token_api_ad(
     EXPORTING
       i_url_destino              = i_url_destino
       i_url_token                = i_url_token
       i_autentica_module         = i_autentica_module
     IMPORTING
       e_token                    = DATA(e_token)
     EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state         = 2
       http_processing_failed     = 3
       http_invalid_timeout       = 4
       OTHERS                     = 5
   ).

    CASE sy-subrc.
      WHEN 1 OR 5.
        MESSAGE e028(zsimetrya) WITH 'Send WebService' RAISING http_communication_failure.
      WHEN 2.
        MESSAGE e029(zsimetrya) WITH 'Send WebService' RAISING http_invalid_state.
      WHEN 3.
        MESSAGE e030(zsimetrya) WITH 'Send WebService' RAISING http_processing_failed.
      WHEN 4.
        MESSAGE e031(zsimetrya) WITH 'Send WebService' RAISING http_invalid_timeout.
    ENDCASE.

    i_http->request->set_header_field( EXPORTING name  = 'Authorization' value = e_token ).

  ENDMETHOD.


  METHOD zif_webservice~add_token_opus_http_cliente.

    zcl_webservice=>zif_webservice~get_token_opus(
      EXPORTING
        i_url_destino              = i_url_destino
        i_url_token                = i_url_token
      IMPORTING
        e_token                    = DATA(e_token)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    CASE sy-subrc.
      WHEN 1 OR 5.
        MESSAGE e028(zsimetrya) WITH 'Send WebService' RAISING http_communication_failure.
      WHEN 2.
        MESSAGE e029(zsimetrya) WITH 'Send WebService' RAISING http_invalid_state.
      WHEN 3.
        MESSAGE e030(zsimetrya) WITH 'Send WebService' RAISING http_processing_failed.
      WHEN 4.
        MESSAGE e031(zsimetrya) WITH 'Send WebService' RAISING http_invalid_timeout.
    ENDCASE.

    i_http->request->set_header_field( EXPORTING name  = 'Authorization' value = e_token ).

  ENDMETHOD.


  METHOD zif_webservice~consultar.

    DATA: tam_xml_s   TYPE string, "Tamanho do Arquivo XML.
          tam_xml_i   TYPE i,
          return_code TYPE i.

    CLEAR: tam_xml_s, tam_xml_i.

    IF i_xml IS NOT INITIAL.
      tam_xml_i = strlen( i_xml ). "Verifica o tamanho do arquivo XML.
      tam_xml_s = tam_xml_i.

      IF i_not_content_length IS INITIAL.
        CALL METHOD i_http->request->set_header_field
          EXPORTING
            name  = 'Content-Length'
            value = tam_xml_s.
      ENDIF.

      CALL METHOD i_http->request->set_cdata
        EXPORTING
          data   = i_xml
          offset = 0
          length = tam_xml_i.
    ENDIF.

    IF me->zif_webservice~autentica_opus EQ abap_true.

      me->zif_webservice~get_token_opus(
        EXPORTING
          i_url_destino              = me->at_url
          i_url_token                = me->at_url_token
        IMPORTING
          e_token                    = DATA(e_token)
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5
      ).

      CASE sy-subrc.
        WHEN 1 OR 5.
          MESSAGE e028(zsimetrya) WITH 'Send WebService' RAISING http_communication_failure.
        WHEN 2.
          MESSAGE e029(zsimetrya) WITH 'Send WebService' RAISING http_invalid_state.
        WHEN 3.
          MESSAGE e030(zsimetrya) WITH 'Send WebService' RAISING http_processing_failed.
        WHEN 4.
          MESSAGE e031(zsimetrya) WITH 'Send WebService' RAISING http_invalid_timeout.
      ENDCASE.

      i_http->request->set_header_field( EXPORTING name  = 'Authorization' value = e_token ).

    ELSEIF me->zif_webservice~autentica_api_ad EQ abap_true.

      me->zif_webservice~get_token_api_ad(
        EXPORTING
          i_url_destino              = me->at_url
          i_url_token                = me->at_url_token
          i_autentica_module         = me->autentica_module
        IMPORTING
          e_token                    = e_token
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5
      ).

      CASE sy-subrc.
        WHEN 1 OR 5.
          MESSAGE e028(zsimetrya) WITH 'Send WebService' RAISING http_communication_failure.
        WHEN 2.
          MESSAGE e029(zsimetrya) WITH 'Send WebService' RAISING http_invalid_state.
        WHEN 3.
          MESSAGE e030(zsimetrya) WITH 'Send WebService' RAISING http_processing_failed.
        WHEN 4.
          MESSAGE e031(zsimetrya) WITH 'Send WebService' RAISING http_invalid_timeout.
      ENDCASE.

      i_http->request->set_header_field( EXPORTING name  = 'Authorization' value = e_token ).

    ENDIF.

    CALL METHOD i_http->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE e028(zsimetrya) WITH 'Send WebService' RAISING http_communication_failure.
      WHEN 2.
        MESSAGE e029(zsimetrya) WITH 'Send WebService' RAISING http_invalid_state.
      WHEN 3.
        MESSAGE e030(zsimetrya) WITH 'Send WebService' RAISING http_processing_failed.
      WHEN 4.
        MESSAGE e031(zsimetrya) WITH 'Send WebService' RAISING http_invalid_timeout.
    ENDCASE.

    CALL METHOD i_http->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE e028(zsimetrya) WITH 'Receive WebService' RAISING http_communication_failure.
      WHEN 2.
        MESSAGE e029(zsimetrya) WITH 'Receive WebService' RAISING http_invalid_state.
      WHEN 3.
        MESSAGE e030(zsimetrya) WITH 'Receive WebService' RAISING http_processing_failed.
    ENDCASE.

    i_http->response->get_status( IMPORTING code = return_code ).

    e_resultado = i_http->response->get_cdata( ).
    e_data      = i_http->response->get_data( ).

    i_http->response->get_status( IMPORTING code = e_code reason = e_reason ).

    i_http->close( ).

  ENDMETHOD.


  METHOD zif_webservice~get_token_api_ad.

    DATA: ex_webservice_token TYPE REF TO zcl_webservice,
          lva_json_output     TYPE string,
          lr_data             TYPE REF TO data,
          lva_token_number    TYPE string.

    TYPES BEGIN OF ty_retorno_msg.
    TYPES message TYPE string.
    TYPES END OF ty_retorno_msg.

    DATA: lc_mensagem TYPE ty_retorno_msg.

    CREATE OBJECT ex_webservice_token.

    ex_webservice_token->zif_webservice~nm_auth_webservice = 'URL_TOKEN_API_AD'.

    SELECT SINGLE * INTO @DATA(wa_zauth_webservice)
        FROM zauth_webservice
       WHERE service EQ @ex_webservice_token->zif_webservice~nm_auth_webservice.

    IF ( sy-subrc = 0 ).

      DATA(lwa_body) = VALUE zde_token_api_body(
          ip       = 'aroeira.corp'
*          module   = |SAP_{ sy-sysid }|
          module   = i_autentica_module
          username = wa_zauth_webservice-username
          password = wa_zauth_webservice-password
      ).

      ex_webservice_token->at_method = wa_zauth_webservice-method.
      ex_webservice_token->at_url = wa_zauth_webservice-url_token.

      DATA(var_http) = ex_webservice_token->url( i_url = CONV #( ex_webservice_token->at_url ) ).
      ex_webservice_token->zif_webservice~abrir_conexao( i_http = var_http ).

      lva_json_output = /ui2/cl_json=>serialize(
                  data = lwa_body
                  compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      CALL METHOD var_http->request->set_header_field
        EXPORTING
          name  = '~server_protocol'
          value = 'HTTP/1.1'.

      CALL METHOD var_http->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'application/json; charset=UTF-8'.

      ex_webservice_token->zif_webservice~consultar(
        EXPORTING
          i_http                     = var_http
          i_xml                      = lva_json_output
        IMPORTING
          e_code                     = DATA(e_code)
          e_reason                   = DATA(e_reason)
        RECEIVING
          e_resultado                = DATA(json_retorno)
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).

      CASE sy-subrc.
        WHEN 1 OR 5.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_communication_failure.
        WHEN 2.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_invalid_state.
        WHEN 3.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_processing_failed.
        WHEN 4.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_invalid_timeout.
      ENDCASE.

      IF e_code NE 200.

        DATA: lc_texto TYPE c LENGTH 200.

        CALL METHOD /ui2/cl_json=>deserialize
          EXPORTING
            json = json_retorno
          CHANGING
            data = lc_mensagem.

        lc_texto = lc_mensagem-message.
        sy-msgv1 = lc_texto+000(50).
        sy-msgv2 = lc_texto+050(50).
        sy-msgv3 = lc_texto+100(50).
        sy-msgv4 = lc_texto+150(50).

        MESSAGE e028(zsimetrya) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_communication_failure.

      ELSE.

        lr_data = /ui2/cl_json=>generate( json = json_retorno ).

        /ui2/cl_data_access=>create( ir_data = lr_data
                         iv_component = 'access' )->value(
                            IMPORTING ev_data = lva_token_number ).

        CONDENSE lva_token_number.

        e_token = |bearer { lva_token_number }|.

      ENDIF.

      CLEAR: ex_webservice_token.

    ENDIF.


  ENDMETHOD.


  METHOD zif_webservice~get_token_opus.

    DATA: ex_webservice_token TYPE REF TO zcl_webservice,
          lc_endereco         TYPE string.

    TYPES BEGIN OF ty_retorno_msg.
    TYPES message TYPE string.
    TYPES END OF ty_retorno_msg.

    TYPES BEGIN OF ty_json_retorno.
    TYPES: access_token TYPE string.
    TYPES: token_type   TYPE string.
    TYPES: expires_in   TYPE string.  "*-#160251-03.12.2024-#160251-JT-inicio
    TYPES END OF ty_json_retorno.

    DATA: lc_mensagem TYPE ty_retorno_msg,
          lc_retorno  TYPE ty_json_retorno,
          lv_token    TYPE zintegracao0001.  ""*-#160251-03.12.2024-#160251-JT-inicio

    CREATE OBJECT ex_webservice_token.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
    DATA(_gestao_token_api_amaggi) = abap_false.
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_gestao_token_api_amaggi)
     WHERE name EQ 'GESTAO_TOKEN_API_AMAGGI'.
    IF sy-subrc EQ 0.
      _gestao_token_api_amaggi = abap_true.
    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--


*-#160251-03.12.2024-#160251-JT-inicio
    IF i_servico = 'RV' OR _gestao_token_api_amaggi EQ abap_true.
      DATA(lt_header_auth) = zcl_gestao_token=>get_token_valido( i_id_token = '0008' ).

      IF lt_header_auth[] IS NOT INITIAL.
        READ TABLE lt_header_auth INTO DATA(wl_header_auth) INDEX 1.
        e_token = wl_header_auth-value.
        CLEAR ex_webservice_token.
        RETURN.
      ENDIF.
    ENDIF.
*-#160251-03.12.2024-#160251-JT-fim

    ex_webservice_token->zif_webservice~ck_usa_auth_webservice = abap_true.
    ex_webservice_token->zif_webservice~nm_auth_webservice = 'URL_TOKEN_OPUS_API'.


    DATA(_ambiente_qas) = abap_false.
    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_stvarv_mock_prd)
     WHERE name = 'MOCK_PRD'
       AND low  = @abap_true.

    IF sy-subrc EQ '0'.
      _ambiente_qas = abap_true.
    ENDIF.

    IF ( sy-sysid NE 'PRD' ) OR ( _ambiente_qas EQ abap_true ).

      SELECT SINGLE * INTO @DATA(wa_zauth_webservice)
        FROM zauth_webservice
       WHERE service EQ @ex_webservice_token->zif_webservice~nm_auth_webservice.

      ex_webservice_token->set_senha( i_senha = CONV #( wa_zauth_webservice-password ) ).
      ex_webservice_token->set_usuario( i_usuario = CONV #( wa_zauth_webservice-username ) ).
      ex_webservice_token->at_content_type = wa_zauth_webservice-content_type.
      ex_webservice_token->at_method = wa_zauth_webservice-method.

      IF i_url_token IS INITIAL .
        ex_webservice_token->at_url = wa_zauth_webservice-url.

        "Montar Dominio Destino
        SPLIT i_url_destino AT '//' INTO: DATA(str_http) DATA(str2).
        SPLIT str2 AT '/' INTO TABLE DATA(itab).
        READ TABLE itab INDEX 1 INTO DATA(str3).
        DELETE itab INDEX 1.
        SPLIT str3 AT ':' INTO DATA(str_host) DATA(str2_porta).
        IF str2_porta IS NOT INITIAL.
          lc_endereco = |{ str_http }//{ str_host }:{ str2_porta }|.
        ELSE.
          lc_endereco = |{ str_http }//{ str_host }|.
        ENDIF.

        "Montar URI com Base no Domio Destino
        SPLIT wa_zauth_webservice-url AT '//' INTO: DATA(str_http_token) DATA(str2_token).
        SPLIT str2_token AT '/' INTO TABLE DATA(itab_token).
        DELETE itab_token INDEX 1.

        LOOP AT itab_token INTO DATA(diretorio_token).
          IF sy-tabix EQ 1.
            READ TABLE itab INDEX 1 INTO DATA(wa_tabix).
            lc_endereco = lc_endereco && '/' && wa_tabix.
          ELSE.
            lc_endereco = lc_endereco && '/' && diretorio_token.
          ENDIF.
        ENDLOOP.

        ex_webservice_token->at_url = lc_endereco.

      ELSE.
        ex_webservice_token->at_url = i_url_token.
      ENDIF.

      DATA(var_http) = ex_webservice_token->url( i_url = CONV #( ex_webservice_token->at_url ) ).
    ELSE.
      var_http = ex_webservice_token->url( ).
    ENDIF.

    ex_webservice_token->zif_webservice~abrir_conexao( i_http = var_http ).

    var_http->request->set_form_field( EXPORTING name = 'grant_type' value = 'password' ).
    var_http->request->set_form_field( EXPORTING name = 'username' value = ex_webservice_token->get_usuario( ) ).
    var_http->request->set_form_field( EXPORTING name = 'password' value = ex_webservice_token->get_senha( ) ).

    DATA(text_form) = |grant_type=password&username={ ex_webservice_token->get_usuario( ) }&password={ ex_webservice_token->get_senha( ) }|.

    ex_webservice_token->zif_webservice~consultar(
      EXPORTING
        i_http                     = var_http
        i_xml                      = text_form
      IMPORTING
        e_code                     = DATA(e_code)
        e_reason                   = DATA(e_reason)
      RECEIVING
        e_resultado                = DATA(json_retorno)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    CASE sy-subrc.
      WHEN 1 OR 5.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_communication_failure.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_invalid_state.
      WHEN 3.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_processing_failed.
      WHEN 4.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_invalid_timeout.
    ENDCASE.

    IF e_code NE 200.

      DATA: lc_texto TYPE c LENGTH 200.

      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json = json_retorno
        CHANGING
          data = lc_mensagem.

      lc_texto = lc_mensagem-message.
      sy-msgv1 = lc_texto+000(50).
      sy-msgv2 = lc_texto+050(50).
      sy-msgv3 = lc_texto+100(50).
      sy-msgv4 = lc_texto+150(50).

      MESSAGE e028(zsimetrya) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING http_communication_failure.

    ELSE.

      /ui2/cl_json=>deserialize(
         EXPORTING
           json = json_retorno
           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING data = lc_retorno
      ).

      e_token = |{ lc_retorno-token_type } { lc_retorno-access_token }|.

*-#160251-03.12.2024-#160251-JT-inicio
      IF i_servico = 'RV' OR _gestao_token_api_amaggi EQ abap_true.
        lv_token-id_token     = '0008'.
        lv_token-access_token = lc_retorno-access_token.
        lv_token-token_type   = lc_retorno-token_type.
        lv_token-expires_in   = lc_retorno-expires_in.
        zcl_gestao_token=>update_token( i_token = lv_token ).
      ENDIF.
*-#160251-03.12.2024-#160251-JT-fim

    ENDIF.

    CLEAR: ex_webservice_token.

  ENDMETHOD.


  METHOD zif_webservice~tela_exception.
*****************
*  Descrição: Método para criação da tela de Exception.
*  Parâmetro: I_TIPO      = TIPO DE MENSAGEM
*             I_CLASS_MSG = Nome da Classe de Mensagem
*             I_NR_MSG    = Número da Mensagem da Classe.
*             I_MSG       = Mensagem do Usuário.
*  Developer: Victor Hugo Souza Nunes
*  Data: 03.04.2014 14:27:08
*****************

    DATA: gw_t100  TYPE t100.  "Tabela de Mensagens
    DATA: var_tipo TYPE char1. "Tipo de Mensage.

    "Tipos de Mensagem:
    "I_TIPO = I (Informação).
    "I_TIPO = E (Erro).
    "I_TIPO = W (Aviso).

    "Colocar o tipo que for passado para o parâmetro como maiúsculo .
    var_tipo = i_tipo.
    TRANSLATE var_tipo TO UPPER CASE.

    "Caso a I_CLASS_MSG e I_NR_MSG seja informado
    "Selecionar na tabela de Mensagem (T100) a mensagme referente aos parametros.
    IF NOT ( i_class_msg IS INITIAL ) AND NOT ( i_nr_msg IS INITIAL ).

      "Seleção da tabela de Mensagens.
      SELECT SINGLE * FROM t100
        INTO gw_t100
      WHERE arbgb EQ i_class_msg
        AND msgnr EQ i_nr_msg.

      IF ( sy-subrc EQ 0 ).

        CASE var_tipo.

          WHEN: 'I'.

            IF NOT ( i_msg IS INITIAL ).
              CONCATENATE gw_t100-text i_msg INTO gw_t100-text SEPARATED BY space.
            ENDIF.

            CALL FUNCTION 'POPUP_FOR_INTERACTION'
              EXPORTING
                headline = 'Exception - Framework WebService Maggi'
                text1    = gw_t100-text
                ticon    = var_tipo
                button_1 = 'Fechar'.

          WHEN: 'E'.

            IF NOT ( i_msg IS INITIAL ).
              CONCATENATE gw_t100-text i_msg INTO gw_t100-text SEPARATED BY space.
            ENDIF.

            CALL FUNCTION 'POPUP_FOR_INTERACTION'
              EXPORTING
                headline = 'Exception - Framework WebService Maggi'
                text1    = gw_t100-text
                ticon    = var_tipo
                button_1 = 'Fechar'.


          WHEN: 'W'.

            IF NOT ( i_msg IS INITIAL ).
              CONCATENATE gw_t100-text i_msg INTO gw_t100-text SEPARATED BY space.
            ENDIF.


            CALL FUNCTION 'POPUP_FOR_INTERACTION'
              EXPORTING
                headline = 'Exception - Framework WebService Maggi'
                text1    = gw_t100-text
                ticon    = var_tipo
                button_1 = 'Fechar'.

        ENDCASE.

      ELSE.

        CALL FUNCTION 'POPUP_FOR_INTERACTION'
          EXPORTING
            headline = 'Exception - Framework WebService Maggi'
            text1    = 'Mensagem não cadastrada'
            ticon    = 'I'
            button_1 = 'Fechar'.
      ENDIF.

    ELSE.

      IF NOT ( i_msg IS INITIAL ).

        CALL FUNCTION 'POPUP_FOR_INTERACTION'
          EXPORTING
            headline = 'Exception - Framework WebService Maggi'
            text1    = i_msg
            ticon    = 'I'
            button_1 = 'Fechar'.

      ENDIF.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
