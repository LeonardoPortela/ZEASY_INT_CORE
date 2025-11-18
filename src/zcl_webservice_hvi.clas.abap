class ZCL_WEBSERVICE_HVI definition
  public
  create public .

*"* public components of class ZCL_WEBSERVICE_HVI
*"* do not include other source files here!!!
public section.

  types:
    BEGIN OF TY_OAUTH,
        TOKEN      TYPE STRING,
        IDENTIDADE TYPE STRING,
        IDUSUARIO  TYPE STRING,
        USUARIO    TYPE STRING,
        SUCCESS    TYPE STRING,
      END OF TY_OAUTH .
  types:
    BEGIN OF TY_RESULTADO_FARDO,
        NR_FARDO       TYPE STRING,
        VL_UHML        TYPE STRING,
        VL_ML          TYPE STRING,
        VL_UI          TYPE STRING,
        VL_STR         TYPE STRING,
        VL_ELG         TYPE STRING,
        VL_MIC         TYPE STRING,
        VL_AMT         TYPE STRING,
        VL_RD          TYPE STRING,
        VL_MB          TYPE STRING,
        VL_CG          TYPE STRING,
        VL_TCNT        TYPE STRING,
        VL_TAREA       TYPE STRING,
        VL_LEAF        TYPE STRING,
        VL_MR          TYPE STRING,
        VL_SFI         TYPE STRING,
        VL_MST         TYPE STRING,
        SCI            TYPE STRING,
        CSP            TYPE STRING,
        MALA           TYPE STRING,
        DT_FINALIZACAO TYPE STRING,
        HR_FINALIZACAO TYPE STRING,
        OS             TYPE STRING,
        NR_LOTE        TYPE STRING,
      END OF TY_RESULTADO_FARDO .
  types:
    TY_T_RESULTADO_FARDO TYPE TABLE OF TY_RESULTADO_FARDO WITH EMPTY KEY .
  types:
    BEGIN OF TY_ROOT_RESULTADO_FARDO,
        SUCCESS TYPE STRING,
        RESULT  TYPE TY_T_RESULTADO_FARDO,
      END OF TY_ROOT_RESULTADO_FARDO .

  methods BUSCA_MALA_HVI
    importing
      !I_LOGIN type STRING
      !I_SENHA type STRING
      !I_DATAI type DATUM
      !I_DATAF type DATUM
    returning
      value(RESULT) type TY_ROOT_RESULTADO_FARDO .
  methods AUTHENTICATION
    importing
      !I_LOGIN type STRING
      !I_SENHA type STRING
    returning
      value(RESULT) type TY_OAUTH .
  PROTECTED SECTION.
*"* protected components of class ZCL_WEBSERVICE_HVI
*"* do not include other source files here!!!
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WEBSERVICE_HVI IMPLEMENTATION.


  METHOD AUTHENTICATION.
    "//Call service
    CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
      EXPORTING
        URL                = CONV #( 'http://kerpweb.kuhlmann.agr.br/rest/usuarios/login' )
      IMPORTING
        CLIENT             = DATA(HTTP_CLIENT)
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~request_method'
        VALUE = 'POST'.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = '~server_protocol'
        VALUE = 'HTTP/1.1'.

    CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
      EXPORTING
        NAME  = 'Content-Type'
        VALUE = 'application/x-www-form-urlencoded'.

    HTTP_CLIENT->REQUEST->SET_FORM_FIELD(
      EXPORTING
        NAME  = 'login'
        VALUE = I_LOGIN
    ).

    HTTP_CLIENT->REQUEST->SET_FORM_FIELD(
      EXPORTING
        NAME  = 'senha'
        VALUE = I_SENHA
    ).

    HTTP_CLIENT->SEND( ).

    CALL METHOD HTTP_CLIENT->RECEIVE
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        OTHERS                     = 4.

    DATA(_RESULT) = HTTP_CLIENT->RESPONSE->GET_CDATA( ).
    DATA(_JSON_DESERIALIZER) = NEW CL_TREX_JSON_DESERIALIZER( ).

    /UI2/CL_JSON=>DESERIALIZE(
      EXPORTING
        JSON        = _RESULT
      CHANGING
        DATA        = RESULT
    ).

  ENDMETHOD.


  METHOD BUSCA_MALA_HVI.
    DATA: V_AUTHOR TYPE TY_OAUTH.
    DATA: OBJ_ZCL_UTIL TYPE REF TO ZCL_UTIL.
    DATA: VAR_DATAI     TYPE STRING. " C LENGTH 10.
    DATA: VAR_DATAF TYPE STRING. " C LENGTH 10.

    FREE: OBJ_ZCL_UTIL.

    CREATE OBJECT OBJ_ZCL_UTIL.
    OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = I_DATAI
                                             I_OPCAO = '/'
                                   RECEIVING E_DATA = VAR_DATAI ).

    OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = I_DATAF
                                             I_OPCAO = '/'
                                   RECEIVING E_DATA = VAR_DATAF ).

    V_AUTHOR = AUTHENTICATION( I_LOGIN = I_LOGIN
                               I_SENHA = I_SENHA ).

    IF V_AUTHOR-TOKEN IS NOT INITIAL.
      "//Call service
      CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
        EXPORTING
          URL                = CONV #( 'http://kerpweb.kuhlmann.agr.br/rest/hvi-fardos/list-fardos-by-cliente' )
        IMPORTING
          CLIENT             = DATA(HTTP_CLIENT)
        EXCEPTIONS
          ARGUMENT_NOT_FOUND = 1
          PLUGIN_NOT_ACTIVE  = 2
          INTERNAL_ERROR     = 3
          OTHERS             = 4.

      CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
        EXPORTING
          NAME  = '~request_method'
          VALUE = 'POST'.

      CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
        EXPORTING
          NAME  = '~server_protocol'
          VALUE = 'HTTP/1.1'.

      CALL METHOD HTTP_CLIENT->REQUEST->SET_HEADER_FIELD
        EXPORTING
          NAME  = 'Content-Type'
          VALUE = 'application/x-www-form-urlencoded'.

      HTTP_CLIENT->REQUEST->SET_FORM_FIELD(
        EXPORTING
          NAME  = 'token'
          VALUE = V_AUTHOR-TOKEN
      ).

      HTTP_CLIENT->REQUEST->SET_FORM_FIELD(
        EXPORTING
          NAME  = 'data_inicial'
          VALUE = VAR_DATAI
      ).

      HTTP_CLIENT->REQUEST->SET_FORM_FIELD(
        EXPORTING
          NAME  = 'data_final'
          VALUE = VAR_DATAF
      ).

      HTTP_CLIENT->SEND( ).

      CALL METHOD HTTP_CLIENT->RECEIVE
        EXCEPTIONS
          HTTP_COMMUNICATION_FAILURE = 1
          HTTP_INVALID_STATE         = 2
          HTTP_PROCESSING_FAILED     = 3
          OTHERS                     = 4.

      DATA(_RESULT) = HTTP_CLIENT->RESPONSE->GET_CDATA( ).
      DATA(_JSON_DESERIALIZER) = NEW CL_TREX_JSON_DESERIALIZER( ).

      /UI2/CL_JSON=>DESERIALIZE(
        EXPORTING
          JSON        = _RESULT
        CHANGING
          DATA        = RESULT
      ).

    ELSE.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
