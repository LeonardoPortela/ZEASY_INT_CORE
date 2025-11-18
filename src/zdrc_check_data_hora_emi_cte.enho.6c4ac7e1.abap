"Name: \TY:CL_NFE_CLOUD_CTE_MAP_HEADER\ME:FORMAT_DATE_TIME_FIELDS\SE:END\EI
ENHANCEMENT 0 ZDRC_CHECK_DATA_HORA_EMI_CTE.

   IF ( is_document_header-docdat NE lv_local_date ) AND
      ( lv_local_date IS NOT INITIAL ) AND
      ( is_document_header-cretim IS  NOT INITIAL ) AND
      ( is_document_header-docdat IS NOT INITIAL ).

     rv_result = mo_map_helper->format_date_time_field( iv_local_date = CONV #( is_document_header-docdat )
                                                        iv_local_time = CONV #( is_document_header-cretim )
                                                        iv_time_zone  = lv_time_zone ).

   ENDIF.

ENDENHANCEMENT.
