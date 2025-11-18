"Name: \FU:SD_SHIPMENT_STATUS_REGISTRATED\SE:END\EI
ENHANCEMENT 0 Z_DETERMINA_DATA_HORA.
*
  CALL FUNCTION 'Z_LES_AJUSTA_DATA_HORA'
    EXPORTING
      I_ERNAM       = c_xvttk_wa-ernam
    CHANGING
      I_DATA        = c_xvttk_wa-dareg
      I_HORA        = c_xvttk_wa-uareg.

ENDENHANCEMENT.
