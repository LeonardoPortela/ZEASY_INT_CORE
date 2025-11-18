class ZCL_INTEGRACAO_COUPA_MATERIAL definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_COUPA_MATERIAL .

  methods ENVIAR_MAT_COUPA
    importing
      !I_ITEM type ZMMS_INT_MAT_COUPA_ITEM .
  methods CONSTRUCTOR .
protected section.
private section.

  methods ENVIAR_MAT_FORNEC_COUPA
    importing
      !I_ITEM_STR type ZMMS_INT_MAT_COUPA_ITEM_STR
      !I_INT_MATERIAL type ZINTEGRCOUPA01
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods ITEM_GET_COUPA
    importing
      !I_ITEM type ZMMS_INT_MAT_COUPA_ITEM
    returning
      value(E_RET) type ZINTEGRCOUPA01 .
  methods EXCLUIR_MAT_FORNEC_COUPA
    importing
      !I_ITEM_STR type ZMMS_INT_MAT_COUPA_ITEM_STR
      !I_INT_MATERIAL type ZINTEGRCOUPA01
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods ITEM_GET_COUPA_BY_NAME
    importing
      !I_ITEM type ZMMS_INT_MAT_COUPA_ITEM
      !I_ITEM_STR type ZMMS_INT_MAT_COUPA_ITEM_STR
    returning
      value(E_RET) type ZINTEGRCOUPA01 .
  methods BLOQUEAR_MATNR
    importing
      !IV_MATNR type MATNR
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods DESBLOQUEAR_MATNR
    importing
      !IV_MATNR type MATNR .
ENDCLASS.



CLASS ZCL_INTEGRACAO_COUPA_MATERIAL IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_coupa_material~at_service = 'COUPA_INT_ENVIA_MATERIAL'.
    me->zif_integracao_inject~at_id_interface    =  zif_integracao=>at_id_interface_coupa_material.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    IF me->zif_integracao_coupa_material~at_service IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_coupa_material~at_auth_ws
          WHERE service = me->zif_integracao_coupa_material~at_service .

    ENDIF.

    IF me->zif_integracao_coupa_material~at_auth_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD enviar_mat_coupa.

    DATA lr_mmsta TYPE RANGE OF mmsta.
    DATA lv_method TYPE zde_http_metodo VALUE 'POST'.

    DATA lw_errors TYPE zmms_int_mat_coupa_errors.
    DATA lv_mess TYPE string.
    DATA lv_body TYPE string.
    DATA lv_param TYPE string.
    DATA lw_integ TYPE zintegrcoupa01.
    DATA lv_mat_ok TYPE flag.
    DATA lw_ret_item TYPE zmms_int_mat_coupa_ret.
    DATA lv_asnum TYPE asnum.
    DATA lv_matnr TYPE matnr.

    DATA lw_item_str TYPE zmms_int_mat_coupa_item_str.

    " 05.07.2023 - 117573 - RBL --------->
    CHECK me->bloquear_matnr( i_item-matnr ) = 0.
    " 05.07.2023 - 117573 - RBL ---------<

    APPEND 'IEQ01' TO lr_mmsta.
    APPEND 'IEQ02' TO lr_mmsta.
    APPEND 'IEQ03' TO lr_mmsta.
    APPEND 'IEQ04' TO lr_mmsta.
    APPEND 'IEQ05' TO lr_mmsta.

    CLEAR me->zif_integracao_coupa_material~at_bapiret2_tab.

    CALL FUNCTION 'ZMM_CONVERTE_MAT_COUPA_ITEM'
      EXPORTING
        i_item = i_item
      IMPORTING
        e_item = lw_item_str.

    lw_integ = me->item_get_coupa_by_name( i_item = i_item i_item_str = lw_item_str ).

    IF lw_integ-fields IS INITIAL.
      lv_param = 'items/?fields=["id","name","item-number"]'.
    ELSE.

      IF lw_integ-fields IS NOT INITIAL.

        lv_param = 'items/' && lw_integ-fields && '?fields=["id","name","item-number"]'.

        lv_method = 'PUT'.

      ELSE.

        lv_param = 'items/?fields=["id","name","item-number"]'.

      ENDIF.

    ENDIF.

    IF i_item-mtart = 'ZDIE'.

      lv_body = '<?xml version="1.0" encoding="UTF-8"?><item><description>#01#</description>' &&
                '<commodity><name>#02#</name></commodity><name>' &&
                '#04#</name><active type="boolean">#05#</active><image-url></image-url><item-type>' &&
                '#06#</item-type><custom-fields><ncm>#07#</ncm><tipo-de-material>#08#</tipo-de-material>' &&
                '<cdigo-do-servio>#09#</cdigo-do-servio></custom-fields><uom><code>#10#</code></uom></item>'.

    ELSE.

      lv_body = '<?xml version="1.0" encoding="UTF-8"?><item><description>#01#</description>' &&
                '<commodity><name>#02#</name></commodity><item-number>#03#</item-number><name>' &&
                '#04#</name><active type="boolean">#05#</active><image-url></image-url><item-type>' &&
                '#06#</item-type><custom-fields><ncm>#07#</ncm><tipo-de-material>#08#</tipo-de-material>' &&
                '<cdigo-do-servio>#09#</cdigo-do-servio></custom-fields><uom><code>#10#</code></uom></item>'.

    ENDIF.

*-CS1067612-#RIMINI-03.27.2023-BEGIN
    REPLACE ALL OCCURRENCES OF REGEX '#' IN lw_item_str-description WITH ''.
*-CS1067612-#RIMINI-03.27.2023-END

*-CS1083608-#RIMINI-04.20.2023-BEGIN
    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lw_item_str-description
      IMPORTING
        outtext           = lw_item_str-description
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
*-CS1083608-#RIMINI-04.20.2023-END

    REPLACE '#01#' IN lv_body WITH lw_item_str-description.
    REPLACE '#02#' IN lv_body WITH lw_item_str-name.
    REPLACE '#03#' IN lv_body WITH lw_item_str-number.
    REPLACE '#04#' IN lv_body WITH lw_item_str-item_name.
    REPLACE '#05#' IN lv_body WITH lw_item_str-active.
    REPLACE '#06#' IN lv_body WITH lw_item_str-item_type.
    REPLACE '#07#' IN lv_body WITH lw_item_str-ncm.
    REPLACE '#08#' IN lv_body WITH lw_item_str-tipo_material.
    REPLACE '#09#' IN lv_body WITH lw_item_str-cod_servico.
    REPLACE '#10#' IN lv_body WITH lw_item_str-uom.

    TRY.

        DATA(lw_int) = me->zif_integracao_coupa_material~execute_service( i_params = lv_param i_method = lv_method i_body = lv_body ).

        IF lw_int-nm_code < 206." OR lw_int-ds_data_retorno CS 'already'.

          zcl_string2=>xml_to_table( EXPORTING i_xml = lw_int-ds_data_retorno IMPORTING r_data = lw_ret_item ).

          IF i_item-mtart = 'ZDIE'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = i_item-asnum
              IMPORTING
                output = lv_asnum.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = i_item-matnr
              IMPORTING
                output = lv_matnr.

            lw_integ-id_integr  = lv_asnum && lv_matnr && i_item-werks. "i_item-asnum && i_item-werks. "BUG 99160
            lw_integ-ident_proc	=	'SV'.
            lw_integ-dt_atual	=	sy-datum.
            lw_integ-hr_atual	=	sy-uzeit.
            lw_integ-status	=	'S'.
            lw_integ-fields = lw_ret_item-item-id.

          ELSE.

            lw_integ-id_integr  = i_item-matnr && i_item-werks.
            lw_integ-ident_proc	=	'MT'.
            lw_integ-dt_atual	=	sy-datum.
            lw_integ-hr_atual	=	sy-uzeit.
            lw_integ-status	=	'S'.
            lw_integ-fields = lw_ret_item-item-id.

          ENDIF.

          lv_mat_ok = 'X'.

          "MODIFY zintegrcoupa01 FROM lw_integ.

        ELSEIF lw_int-ds_data_retorno CS 'already'.

          lw_integ = me->item_get_coupa( i_item ).

          lv_mat_ok = 'X'.

        ENDIF.

        IF lw_int-nm_code > 300 AND  lv_mat_ok IS INITIAL.

          zcl_string2=>xml_to_table( EXPORTING i_xml = lw_int-ds_data_retorno IMPORTING r_data = lw_errors ).
          zcx_error=>zif_error~gera_erro_geral( i_texto = lw_errors-errors-error ).

        ENDIF.

        " somente se for criação
        IF lw_item_str-active = 'true' AND lv_mat_ok = 'X'.

          zcl_string2=>xml_to_table( EXPORTING i_xml = lw_int-ds_data_retorno IMPORTING r_data = lw_ret_item ).

          IF lw_integ-fields IS NOT INITIAL.

            IF i_item-mmsta IN lr_mmsta.
              me->excluir_mat_fornec_coupa( EXPORTING i_item_str = lw_item_str i_int_material = lw_integ ).
            ELSE.
              me->enviar_mat_fornec_coupa( EXPORTING i_item_str = lw_item_str i_int_material = lw_integ ).
            ENDIF.

          ENDIF.

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).

        me->zif_integracao_coupa_material~set_message( ex_int->get_longtext( ) ).

        " 05.07.2023 - 117573 - RBL --------->
        me->desbloquear_matnr( i_item-matnr ).
        " 05.07.2023 - 117573 - RBL ---------<

        EXIT.

      CATCH zcx_error INTO DATA(ex_erro).

        me->zif_integracao_coupa_material~set_message( ex_erro->get_longtext( ) ).

        " 05.07.2023 - 117573 - RBL --------->
        me->desbloquear_matnr( i_item-matnr ).
        " 05.07.2023 - 117573 - RBL ---------<

        EXIT.

    ENDTRY.

    CHECK lw_integ IS NOT INITIAL.

    MODIFY zintegrcoupa01 FROM lw_integ.

    " 05.07.2023 - 117573 - RBL --------->
    me->desbloquear_matnr( i_item-matnr ).
    " 05.07.2023 - 117573 - RBL ---------<

    lv_mess = `Envio realizado ID COUPA: ` && lw_integ-fields.

    me->zif_integracao_coupa_material~set_message( EXPORTING i_message = lv_mess i_msgty = 'S' ).

  ENDMETHOD.


  METHOD enviar_mat_fornec_coupa.

    DATA lw_errors TYPE zmms_int_mat_coupa_errors.
    DATA lv_body TYPE string.
    DATA lv_param TYPE string.
    DATA lv_ctr_param TYPE string.
    DATA lo_xml TYPE REF TO cl_xml_document.
    DATA lv_contrato TYPE string.

    lv_param = 'supplier_items?fields=["id","supplier-part-num","supplier-aux-part-num",' &&
               '{"supplier":["id","name","number"]},{"item": ' &&
               '["id","name","item-number","active"]},{"custom_fields": {}}]'.

    " 06.06.2023 - 109042 - RBL --------->
*    lv_body = '<?xml version="1.0" encoding="UTF-8"?><supplier-item><price type="decimal">#01#</price>' &&
*              '<preferred type="boolean">#02#</preferred><currency><code>#03#</code></currency><supplier>' &&
*              '<name>#04#</name></supplier><contract><number>#05#</number></contract><item><id>#06#</id>' &&
*              '</item><custom-fields><preo-por type="decimal">#07#</preo-por><benefcios-fiscais>#08#' &&
*              '</benefcios-fiscais></custom-fields></supplier-item>'.

    lv_body = '<?xml version="1.0" encoding="UTF-8"?><supplier-item><price type="decimal">#01#</price>' &&
              '<preferred type="boolean">#02#</preferred><currency><code>#03#</code></currency><supplier>' &&
              '<name>#04#</name></supplier><contract><id>#05#</id></contract><item><id>#06#</id>' &&
              '</item><custom-fields><preo-por type="decimal">#07#</preo-por><benefcios-fiscais>#08#' &&
              '</benefcios-fiscais></custom-fields></supplier-item>'.

    TRY .

        lv_ctr_param = 'contracts?status=published&number=' && i_item_str-werks && '&fields=["id","number","name"]'.

        DATA(lw_int_contrato) = me->zif_integracao_coupa_material~execute_service( i_params = lv_ctr_param i_method = 'GET').

        IF lw_int_contrato-nm_code < 206.

          CREATE OBJECT lo_xml.

          IF lo_xml->parse_string( lw_int_contrato-ds_data_retorno ) = 0.

            lv_contrato = lo_xml->find_simple_element( 'id' ).

          ENDIF.

        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int1).
        me->zif_integracao_coupa_material~set_message( ex_int1->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro1).
        me->zif_integracao_coupa_material~set_message( ex_erro1->get_longtext( ) ).

    ENDTRY.


    " 06.06.2023 - 109042 - RBL ---------<

    REPLACE '#01#' IN lv_body WITH '0.01'.
    REPLACE '#02#' IN lv_body WITH 'false'.
    REPLACE '#03#' IN lv_body WITH 'BRL'.
    REPLACE '#04#' IN lv_body WITH 'A definir'."i_item_str-name1.
    "REPLACE '#05#' IN lv_body WITH i_item_str-werks. " 06.06.2023 - 109042 - RBL
    REPLACE '#05#' IN lv_body WITH lv_contrato.

    REPLACE '#06#' IN lv_body WITH  i_int_material-fields.
    REPLACE '#07#' IN lv_body WITH '1.0'.
    REPLACE '#08#' IN lv_body WITH '1'.

    TRY.

        DATA(lw_int) = me->zif_integracao_coupa_material~execute_service( i_params = lv_param i_body = lv_body ).

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_coupa_material~set_message( ex_int->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_coupa_material~set_message( ex_erro->get_longtext( ) ).
    ENDTRY.

    IF lw_int-nm_code < 206 OR lw_int-ds_data_retorno CS 'already' OR lw_int-ds_data_retorno CS 'Contract and Catalog must be unique'.

    ELSE.

      zcl_string2=>xml_to_table( EXPORTING i_xml = lw_int-ds_data_retorno IMPORTING r_data = lw_errors ).

      zcx_error=>zif_error~gera_erro_geral( i_texto = lw_errors-errors-error ).

    ENDIF.

  ENDMETHOD.


  METHOD excluir_mat_fornec_coupa.

    DATA lo_xml TYPE REF TO cl_xml_document.

    DATA lw_errors TYPE zmms_int_mat_coupa_errors.
    DATA lv_body TYPE string.
    DATA lv_param TYPE string.

    lv_param = 'supplier_items?item[id=' && i_int_material-fields && '&fields=["id"]'.

    TRY.

        DATA(lw_int) = me->zif_integracao_coupa_material~execute_service( i_params = lv_param i_body = lv_body i_method = 'GET' ).

        CREATE OBJECT lo_xml.

        IF lo_xml->parse_xstring( lw_int-ds_data_xstring ) = 0.
          DATA(lv_id_item) = lo_xml->find_simple_element( 'id' ).
        ENDIF.

        CHECK lv_id_item IS NOT INITIAL.

        lv_param = 'supplier_items/' && lv_id_item.

        lw_int = me->zif_integracao_coupa_material~execute_service( i_params = lv_param i_method = 'DELETE' ).

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_coupa_material~set_message( ex_int->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_coupa_material~set_message( ex_erro->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD item_get_coupa.

    DATA lv_id_integr TYPE zcoupa_id_integr.
    DATA lw_item TYPE zmms_int_mat_coupa_ret2.
    DATA lv_param TYPE string.
    DATA lv_mat_str TYPE c LENGTH 20.

    CHECK i_item-matnr IS NOT INITIAL.

    lv_id_integr = i_item-matnr && i_item-werks.

    SELECT SINGLE * FROM zintegrcoupa01
      INTO e_ret
        WHERE id_integr = lv_id_integr.

    CHECK sy-subrc NE 0.

    WRITE i_item-matnr TO lv_mat_str LEFT-JUSTIFIED NO-ZERO.

    lv_param ='items/?item-number=' && lv_mat_str && '&fields=["id","name","active"]'.

    TRY.

        DATA(lw_int) = me->zif_integracao_coupa_material~execute_service( i_params = lv_param i_method = 'GET' ).

        IF lw_int-nm_code < 206.

          zcl_string2=>xml_to_table( EXPORTING i_xml = lw_int-ds_data_retorno IMPORTING r_data = lw_item ).

          CHECK sy-subrc EQ 0.

          e_ret-id_integr  = i_item-matnr && i_item-werks.
          e_ret-ident_proc  = 'MT'.
          e_ret-dt_atual  = sy-datum.
          e_ret-hr_atual  = sy-uzeit.
          e_ret-status  = 'S'.
          e_ret-fields = lw_item-items-item-id.

          "MODIFY zintegrcoupa01 FROM e_ret.
        ELSEIF lw_int-nm_code = 404.
          RETURN.
        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_coupa_material~set_message( ex_int->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_coupa_material~set_message( ex_erro->get_longtext( ) ).
    ENDTRY.


  ENDMETHOD.


  METHOD item_get_coupa_by_name.

    DATA t_element_array  TYPE zde_element_array_t.
    DATA lv_id_integr TYPE zcoupa_id_integr.
    DATA lw_item TYPE zmms_int_mat_coupa_ret3.
    DATA lv_param TYPE string.
    DATA lv_asnum TYPE asnum.
    DATA lv_matnr TYPE matnr.
    "DATA lv_mat_str TYPE c LENGTH 20.

    CHECK i_item-matnr IS NOT INITIAL.

    IF i_item-mtart = 'ZDIE'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = i_item-asnum
        IMPORTING
          output = lv_asnum.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = i_item-matnr
        IMPORTING
          output = lv_matnr.

      lv_id_integr = lv_asnum && lv_matnr && i_item-werks.  "BUG 99160

      SELECT SINGLE * FROM zintegrcoupa01
        INTO e_ret
          WHERE id_integr = lv_id_integr
            AND ident_proc = 'SV'.

    ELSE.

      lv_id_integr = i_item-matnr && i_item-werks.

      SELECT SINGLE * FROM zintegrcoupa01
        INTO e_ret
          WHERE id_integr = lv_id_integr
            AND ident_proc = 'MT'.

    ENDIF.

    CHECK sy-subrc NE 0

    " 23.11.2022 - RAMON - CORREÇÃO QUANDO FIELDS VEM VAZIO -->>
    OR e_ret-fields IS INITIAL.
    " 23.11.2022 - RAMON - CORREÇÃO QUANDO FIELDS VEM VAZIO --<<

    "WRITE i_item-matnr TO lv_mat_str LEFT-JUSTIFIED NO-ZERO.

    IF i_item-mtart = 'ZDIE'.                                                                                                         "2000003259 - IR166008
      DATA(lv_escape_url) = cl_http_utility=>if_http_utility~escape_url( EXPORTING unescaped =  CONV #( i_item_str-cod_servico ) ).   "2000003259 - IR166008
      lv_param ='items/?custom-fields[cdigo-do-servio]=' && lv_escape_url && '&fields=["id","name","active","item-number"]'.          "2000003259 - IR166008
    ELSE.                                                                                                                             "2000003259 - IR166008
      lv_escape_url = cl_http_utility=>if_http_utility~escape_url( EXPORTING unescaped =  CONV #( i_item_str-item_name ) ).

      "lv_param ='items/?item-number=' && lv_mat_str && '&fields=["id","name","active"]'.
      lv_param ='items/?name=' && lv_escape_url && '&fields=["id","name","active","item-number"]'.
    ENDIF.                                                                                                                            "2000003259 - IR166008

    TRY.

        DATA(lw_int) = me->zif_integracao_coupa_material~execute_service( i_params = lv_param i_method = 'GET' ).

        IF lw_int-nm_code < 206.

          APPEND 'item' TO t_element_array.

          zcl_string2=>xml_to_table( EXPORTING i_xml = lw_int-ds_data_retorno i_element_array = t_element_array IMPORTING r_data = lw_item ).

          LOOP AT lw_item-items-item ASSIGNING FIELD-SYMBOL(<fs_itens>).

            IF i_item-mtart = 'ZDIE'. " é um serviço?, entao pega os que item-number vazio

              IF <fs_itens>-itemnumber IS INITIAL.

                e_ret-id_integr  = lv_asnum && lv_matnr && i_item-werks. "i_item-asnum && i_item-werks. "BUG 99160
                e_ret-ident_proc  = 'SV'.
                e_ret-dt_atual  = sy-datum.
                e_ret-hr_atual  = sy-uzeit.
                e_ret-status  = 'S'.
                e_ret-fields = <fs_itens>-id.
                EXIT.

              ENDIF.

              " então pega os que tem item number
            ELSE.

              IF <fs_itens>-itemnumber IS NOT INITIAL.

                e_ret-id_integr  = i_item-matnr && i_item-werks.
                e_ret-ident_proc  = 'MT'.
                e_ret-dt_atual  = sy-datum.
                e_ret-hr_atual  = sy-uzeit.
                e_ret-status  = 'S'.
                e_ret-fields = <fs_itens>-id.

              ENDIF.

            ENDIF.

          ENDLOOP.

          "MODIFY zintegrcoupa01 FROM e_ret.
        ELSEIF lw_int-nm_code = 404.
          RETURN.
        ENDIF.

      CATCH zcx_integracao INTO DATA(ex_int).
        me->zif_integracao_coupa_material~set_message( ex_int->get_longtext( ) ).
      CATCH zcx_error INTO DATA(ex_erro).
        me->zif_integracao_coupa_material~set_message( ex_erro->get_longtext( ) ).
    ENDTRY.


  ENDMETHOD.


  method ZIF_INTEGRACAO_COUPA_MATERIAL~AFTER_EXECUTE.
  endmethod.


  method ZIF_INTEGRACAO_COUPA_MATERIAL~BEFORE_EXECUTE.
  endmethod.


  METHOD zif_integracao_coupa_material~execute_service.


    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->zif_integracao_coupa_material~at_service IS NOT INITIAL.

      IF me->zif_integracao_coupa_material~at_auth_ws IS INITIAL.

        SELECT SINGLE * FROM zauth_webservice
          INTO me->zif_integracao_coupa_material~at_auth_ws
            WHERE service = me->zif_integracao_coupa_material~at_service.

      ENDIF.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    IF me->zif_integracao_coupa_material~at_auth_ws  IS INITIAL.
      lv_msgtx = `Serviço ` && me->zif_integracao_coupa_material~at_service && ` não está configurado na ZAUTH_WEBSERVICE`.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_coupa_material~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_coupa_material~at_auth_ws-url && i_params.
    CLEAR me->zif_integracao_inject~at_multipart_fields.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_coupa_material~before_execute( ).

    "me->zif_integracao_coupa_material~gui_indicator_exe( ).

    CREATE OBJECT lc_integrar.

    IF i_body IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_body = i_body.
    ELSE.
      CLEAR me->zif_integracao_inject~at_info_request_http-ds_body.
    ENDIF.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = r_integracao
      )->free(
      ).

    FREE lc_integrar.

    me->zif_integracao_coupa_material~after_execute( r_integracao ).


  ENDMETHOD.


  METHOD zif_integracao_coupa_material~get_instance.

    IF zif_integracao_coupa_material~at_instance IS NOT BOUND.

      zif_integracao_coupa_material~at_instance = NEW zcl_integracao_coupa_material( ).

    ENDIF.

    r_object = zif_integracao_coupa_material~at_instance.

  ENDMETHOD.


  METHOD zif_integracao_coupa_material~get_messages.

    r_ret = me->zif_integracao_coupa_material~at_bapiret2_tab.

  ENDMETHOD.


  METHOD zif_integracao_coupa_material~set_message.

    DATA(lv_msgty) = i_msgty.

    CHECK i_message IS NOT INITIAL.

    IF lv_msgty IS INITIAL.
      lv_msgty = 'E'.
    ENDIF.

    APPEND INITIAL LINE TO me->zif_integracao_coupa_material~at_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_message>).

    DATA: lt_trtexts     TYPE trtexts,
          lw_trtexts     TYPE trtext,
          lv_texto(4000).

    DATA lv_msg1 TYPE sy-msgv1.
    DATA lv_msg2 TYPE sy-msgv1.
    DATA lv_msg3 TYPE sy-msgv1.
    DATA lv_msg4 TYPE sy-msgv1.

    DATA(lv_string) = i_message.

    REPLACE ALL OCCURRENCES OF '&' IN lv_string WITH space.
    REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
    CONDENSE lv_string.

    "REPLACE `    ` IN lv_string WITH space.

    lv_texto = lv_string.

    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text  = lv_texto
        iv_len   = 30
      IMPORTING
        et_lines = lt_trtexts.

    LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

      CASE sy-tabix.
        WHEN 1.
          <fs_message>-message_v1 = <fs_line>.
        WHEN 2.
          <fs_message>-message_v2 = <fs_line>.
        WHEN 3.
          <fs_message>-message_v3 = <fs_line>.
        WHEN 4.
          <fs_message>-message_v4 = <fs_line>.
      ENDCASE.

    ENDLOOP.

    <fs_message>-id = 'DS'.
    <fs_message>-type = lv_msgty.
    <fs_message>-number = '016'.

    MESSAGE ID <fs_message>-id TYPE <fs_message>-type NUMBER <fs_message>-number
      WITH <fs_message>-message_v1 <fs_message>-message_v2
           <fs_message>-message_v3 <fs_message>-message_v4 INTO <fs_message>-message.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_integracao_token_coupa(
               zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                 )->get_token( )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.

    APPEND VALUE #( name = 'Accept' value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD bloquear_matnr.

    CALL FUNCTION 'ENQUEUE_EZMATNR'
      EXPORTING
        mode_mara      = 'X'
        mandt          = sy-mandt
        matnr          = iv_matnr
        _scope         = '2'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.

      rv_subrc = sy-subrc.

      me->zif_integracao_coupa_material~set_message( i_msgty = 'E' i_message = `Material ` && |{ iv_matnr ALPHA = OUT }| && ` está bloqueado para processamento` ).

    ENDIF.

  ENDMETHOD.


  METHOD desbloquear_matnr.

    CALL FUNCTION 'DEQUEUE_EZMATNR'
      EXPORTING
        mode_mara = 'X'
        mandt     = sy-mandt
        matnr     = iv_matnr
        _scope    = '2'.

    me->zif_integracao_coupa_material~set_message( i_msgty = 'S' i_message = `Material ` && |{ iv_matnr ALPHA = OUT }| && ` desbloqueado` ).

  ENDMETHOD.
ENDCLASS.
