class ZCL_DATA_HORA definition
  public
  final
  create public .

public section.

  class-methods GET_DATA_HOTA_TO_TIMESTAMP
    importing
      !I_DATA type SY-DATUM default SY-DATUM
      !I_HORA type SY-UZEIT default SY-UZEIT
    returning
      value(R_TIMESTAMP) type TZNTSTMPS .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DATA_HORA IMPLEMENTATION.


  METHOD GET_DATA_HOTA_TO_TIMESTAMP.

    CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_INTO'
      EXPORTING
        IV_DATE          = I_DATA
        IV_TIME          = I_HORA
      IMPORTING
        EV_TIMESTAMP     = R_TIMESTAMP
      EXCEPTIONS
        CONVERSION_ERROR = 1
        OTHERS           = 2.

  ENDMETHOD.
ENDCLASS.
