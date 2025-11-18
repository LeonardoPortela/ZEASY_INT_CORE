*"* components of interface ZIF_WEBSERVICE
interface ZIF_WEBSERVICE
  public .


  data AUTENTICA_OPUS type CHAR01 .
  data CK_USA_AUTH_WEBSERVICE type CHAR01 .
  data NM_AUTH_WEBSERVICE type /UI2/SERVICE_NAME .
  data AUTENTICA_API_AD type CHAR01 .
  data AUTENTICA_MODULE type ZDE_AUTENTICA_MODULE .

  class-methods ADD_TOKEN_OPUS_HTTP_CLIENTE
    importing
      !I_URL_DESTINO type STRING
      !I_URL_TOKEN type STRING optional
    changing
      value(I_HTTP) type ref to IF_HTTP_CLIENT
    exceptions
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT .
  class-methods GET_TOKEN_API_AD
    importing
      !I_URL_DESTINO type STRING
      !I_URL_TOKEN type STRING
      !I_AUTENTICA_MODULE type ZDE_AUTENTICA_MODULE optional
    exporting
      !E_TOKEN type STRING
    exceptions
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT .
  class-methods GET_TOKEN_OPUS
    importing
      !I_URL_DESTINO type STRING
      !I_URL_TOKEN type STRING
      !I_SERVICO type CHAR02 optional
    exporting
      !E_TOKEN type STRING
    exceptions
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT .
  class-methods ADD_TOKEN_API_AD_HTTP_CLIENTE
    importing
      !I_URL_DESTINO type STRING
      !I_URL_TOKEN type STRING optional
      !I_AUTENTICA_MODULE type ZDE_AUTENTICA_MODULE optional
    changing
      value(I_HTTP) type ref to IF_HTTP_CLIENT
    exceptions
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT .
  methods ABRIR_CONEXAO
    importing
      !I_HTTP type ref to IF_HTTP_CLIENT
      !I_AUTENTICAR type CHAR01 default ' ' .
  methods CONSULTAR
    importing
      !I_HTTP type ref to IF_HTTP_CLIENT
      !I_XML type STRING optional
      !I_NOT_CONTENT_LENGTH type CHAR01 optional
    exporting
      value(E_DATA) type XSTRING
      !E_CODE type I
      !E_REASON type STRING
    returning
      value(E_RESULTADO) type STRING
    exceptions
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT .
  methods TELA_EXCEPTION
    importing
      !I_TIPO type CHAR01
      !I_CLASS_MSG type ARBGB optional
      !I_NR_MSG type MSGNR optional
      !I_MSG type STRING optional
    raising
      ZCX_WEBSERVICE .
endinterface.
