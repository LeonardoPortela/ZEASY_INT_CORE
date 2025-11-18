class ZCL_INTEGRACAO_BRY_ADIGITAL definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_BRY_ADIGITAL .

  methods CONSTRUCTOR
    importing
      !IT_ANEXOS type ZINS_ADIGITAL_ATTACHMENTS
      !I_DADOS_ENVIO type ZINS_DADOS_BRY_DADOS
    raising
      ZCX_INTEGRACAO .
protected section.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INTEGRACAO_BRY_ADIGITAL IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_bry_adigital~at_servico = 'BRY_INT_ASSINATURA_DIGITAL'.
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_bry_adigital.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_bry_adigital~at_dados = i_dados_envio.

    me->zif_integracao_bry_adigital~at_anexos = it_anexos.

*-CS2019001753-20.06.2023-#103050-JT-inicio
    me->zif_integracao_bry_adigital~at_json_grupo_parti_template = '{  "idGrupo": "#01#", "quantidadeAssinantes": "#02#", "ordem": "#03#",' &&
                                                                      '"assinante": #04#, "tipoAssinatura": "#05#" } '.

    " 08.11.2024 - RAMON - 154395 -->
    me->zif_integracao_bry_adigital~at_json_parti_template = '{  "codigo": "#01#", "nome": "#02#", "email": "#03#",' &&
                                                             '"assinante": "#06#","revisor": "#07#",'  &&
                                                             '"tipoAssinatura": "#04#" ,"ordem": "#05#"  } '.

*    me->zif_integracao_bry_adigital~at_json_parti_template = '{  "codigo": "#01#", "nome": "#02#", "email": "#03#",' &&
*                                                             '"assinante": "true","revisor": "false",'  &&
*                                                             '"tipoAssinatura": "#04#" ,"ordem": "#05#"  } '.

    " 08.11.2024 - RAMON - 154395 --<

*   me->zif_integracao_bry_adigital~at_json_parti_template = '{  "codigo": "#01#", "nome": "#02#", "email": "#03#",' &&
*                                                            '"assinante": "true","revisor": "false",	"tipoAssinatura": "#04#" } '.
*-CS2019001753-20.06.2023-#103050-JT-fim

*-CS2021000218-10.11.2022-JT-#92371-inicio
    me->zif_integracao_inject~at_referencia        = me->zif_integracao_bry_adigital~at_dados-id_referencia.

    IF     me->zif_integracao_bry_adigital~at_dados-endpoint_coletas            IS NOT INITIAL.
      me->zif_integracao_bry_adigital~at_end_point = 'coletas/#01#/documentos'.
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_posicao_assina     IS NOT INITIAL.
      me->zif_integracao_bry_adigital~at_end_point = 'coleta/#01#/definir-locais-assinaturas'. "// BUG-174950 WBARBOSA 23/04/25
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_assinado   IS NOT INITIAL.
      me->zif_integracao_bry_adigital~at_end_point = 'documento/assinado/#01#'. "// BUG-174950 WBARBOSA 23/04/25
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_rel_assina IS NOT INITIAL.
      me->zif_integracao_bry_adigital~at_end_point = 'documentos/#01#/relatorio-assinaturas'.
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_get_participantes  IS NOT INITIAL.
      me->zif_integracao_bry_adigital~at_end_point = 'coletas/#01#/participantes'.
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_cancelar_coletas   IS NOT INITIAL.
      me->zif_integracao_bry_adigital~at_end_point = 'coletas/#01#/cancelar'.
    ELSE.
      me->zif_integracao_bry_adigital~at_end_point = 'coletas/cadastrar?nomecoleta=#01#&dataLimite=#02#&' && "// BUG-174950 WBARBOSA 23/04/25
                                                     'descricao=#03#&padraoAssinatura=#04#&exigirDownload=#05#&proibirRejeicao=#06#&' &&
                                                     'assinaturaSequencial=#07#&agruparDocumentos=#08#&configuracaoLocalAssinatura=#09#&origemAssinatura=SAP#10#'.
    ENDIF.
*-CS2021000218-10.11.2022-JT-#92371-fim

    IF me->zif_integracao_bry_adigital~at_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_integracao_bry_adigital~at_auth_ws
          WHERE service = me->zif_integracao_bry_adigital~at_servico .

    ENDIF.

    IF me->zif_integracao_bry_adigital~at_auth_ws  IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

*-CS2021000218-10.11.2022-JT-#92371-inicio
    IF ( me->zif_integracao_bry_adigital~at_dados-endpoint_coletas            IS INITIAL  AND
         me->zif_integracao_bry_adigital~at_dados-endpoint_posicao_assina     IS INITIAL  AND
         me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_assinado   IS INITIAL  AND
         me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_rel_assina IS INITIAL  AND
         me->zif_integracao_bry_adigital~at_dados-endpoint_get_participantes  IS INITIAL  AND
         me->zif_integracao_bry_adigital~at_dados-endpoint_cancelar_coletas   IS INITIAL  AND
         me->zif_integracao_bry_adigital~at_anexos                            IS INITIAL ) OR
         me->zif_integracao_bry_adigital~at_dados                             IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.
*-CS2021000218-10.11.2022-JT-#92371-fim

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital~enviar_bry.

    DATA lv_mess TYPE string.

    me->zif_integracao_bry_adigital~set_ds_url(
          )->set_ds_data(
          )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao) e_integracao  = DATA(e_integracao)  ).

    "ENDTRY.

    IF ( e_integracao-nm_code NE 200 AND e_integracao-nm_code NE 201 AND e_integracao-nm_code NE 202 ).

      IF e_integracao-nm_code >= 500.

        "zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro 500(Internal Server Error): Servidor BRY esta fora' ).

      ELSEIF e_integracao-nm_code >= 400.

        "zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro 400: API ( MAGGI -> BRY ) está fora' ).

      ELSE.

        "zcx_error=>zif_error~gera_erro_geral( i_texto = 'Erro 400: API ( MAGGI -> BRY ) está fora' ).

      ENDIF.

    ENDIF.

    e_json  = e_integracao-ds_data_retorno.
    e_jsonx = e_integracao-ds_data_xstring.  "*-CS2021000218-10.11.2022-JT-#92371

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital~get_grupo_participantes.

*    DATA lt_assinantes TYPE TABLE OF zinc_dados_partici_form.
*    DATA lw_values TYPE zinc_dados_partici_form.
*    DATA lv_ret TYPE string.
*
*    CLEAR r_json.
*
**-CS2019001753-20.06.2023-#103050-JT-inicio
**   CHECK  me->zif_integracao_bry_adigital~at_dados-relacao_assinantes IS NOT INITIAL.
**   SPLIT me->zif_integracao_bry_adigital~at_dados-relacao_assinantes AT ';' INTO TABLE lt_assinantes.
*
*    IF me->zif_integracao_bry_adigital~at_dados-relacao_assinantes IS NOT INITIAL.
*      SPLIT me->zif_integracao_bry_adigital~at_dados-relacao_assinantes AT ';' INTO TABLE lt_assinantes.
*    ELSEIF me->zif_integracao_bry_adigital~at_dados-relacao_grupo_assinantes IS NOT INITIAL.
*      SPLIT me->zif_integracao_bry_adigital~at_dados-relacao_grupo_assinantes AT ';' INTO TABLE lt_assinantes.
*    ELSE.
*      EXIT.
*    ENDIF.
**-CS2019001753-20.06.2023-#103050-JT-fim
*
*    SORT lt_assinantes.
*
*    DELETE lt_assinantes WHERE value1 IS INITIAL.
*
*    DELETE ADJACENT DUPLICATES FROM lt_assinantes.
*
*    DATA(lv_lines) = lines( lt_assinantes ).
*
*    LOOP AT lt_assinantes ASSIGNING FIELD-SYMBOL(<fs_assinantes>).
*
*      DATA(lv_tabix) = sy-tabix.
*
*      SPLIT <fs_assinantes>-value1 AT '#' INTO lw_values-value1 lw_values-value2 lw_values-value3 lw_values-value4.
*
*      CONDENSE lw_values-value1.
*      CONDENSE lw_values-value2.
*      CONDENSE lw_values-value3.
*
*      " NÃO É PARA MANDAR DEFAULT
**      IF lw_values-value4 IS INITIAL.
**        lw_values-value4 = 'ASSINATURA_ELETRONICA'.
**      ENDIF.
**
**      REPLACE space IN lw_values-value4 WITH '_'.
*
*      CONDENSE lw_values-value4.
*
**-CS2019001753-20.06.2023-#103050-JT-inicio
**     lv_ret = lv_ret && me->zif_integracao_bry_adigital~at_json_parti_template.
*      IF me->zif_integracao_bry_adigital~at_dados-relacao_assinantes IS NOT INITIAL.
*        lv_ret = lv_ret && me->zif_integracao_bry_adigital~at_json_parti_template.
*      ELSE.
*        lv_ret = lv_ret && me->zif_integracao_bry_adigital~at_json_grupo_parti_template.
*      ENDIF.
**-CS2019001753-20.06.2023-#103050-JT-fim
*
*      REPLACE '#01#' IN lv_ret WITH lw_values-value1.
*      REPLACE '#02#' IN lv_ret WITH lw_values-value2.
*      REPLACE '#03#' IN lv_ret WITH lw_values-value3.
*      REPLACE '#04#' IN lv_ret WITH lw_values-value4.
*
*      IF lv_tabix NE lv_lines.
*        lv_ret = lv_ret && ','.
*      ENDIF.
*
*    ENDLOOP.
*
*    CHECK lv_ret IS NOT INITIAL.
*
**-CS2019001753-20.06.2023-#103050-JT-inicio
*    IF me->zif_integracao_bry_adigital~at_dados-relacao_assinantes IS NOT INITIAL.
*      CONCATENATE lv_ret ',' '{"codigo":"77294254000194", "assinante":"false", "revisor":"false"}' INTO lv_ret. "ALRS 26.12.2022 PART. CONVIDADO (URGENTE)
*    ENDIF.
**-CS2019001753-20.06.2023-#103050-JT-fim
*
*    r_json = '[' && lv_ret && ']'.

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital~get_instance.

    IF zif_integracao_bry_adigital~at_instance IS NOT BOUND.
      "zif_integracao_bry_adigital~at_instance = NEW zcl_integracao_bry_adigital( ).
    ENDIF.

    "r_if_int_bry_adigital = zif_integracao_bry_adigital~at_instance.

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital~get_participantes.

    DATA lt_assinantes TYPE TABLE OF zinc_dados_partici_form.
    DATA lw_values TYPE zinc_dados_partici_form.
    DATA lv_ret TYPE string.

    CLEAR r_json.

*-CS2019001753-20.06.2023-#103050-JT-inicio
*   CHECK  me->zif_integracao_bry_adigital~at_dados-relacao_assinantes IS NOT INITIAL.
    IF me->zif_integracao_bry_adigital~at_dados-relacao_assinantes IS NOT INITIAL.
*-CS2019001753-20.06.2023-#103050-JT-fim
      SPLIT me->zif_integracao_bry_adigital~at_dados-relacao_assinantes AT ';' INTO TABLE lt_assinantes.

      SORT lt_assinantes.

      DELETE lt_assinantes WHERE value1 IS INITIAL.

      DELETE ADJACENT DUPLICATES FROM lt_assinantes.

      DATA(lv_lines) = lines( lt_assinantes ).

      LOOP AT lt_assinantes ASSIGNING FIELD-SYMBOL(<fs_assinantes>).

        DATA(lv_tabix) = sy-tabix.

        SPLIT <fs_assinantes>-value1 AT '#' INTO lw_values-value1 lw_values-value2
          lw_values-value3 lw_values-value4 lw_values-value5
          " 08.11.2024 - RAMON - 154395 -->
          lw_values-value6 lw_values-value7.
        " 08.11.2024 - RAMON - 154395 --<

        CONDENSE lw_values-value1.
        CONDENSE lw_values-value2.
        CONDENSE lw_values-value3.

        " NÃO É PARA MANDAR DEFAULT
*      IF lw_values-value4 IS INITIAL.
*        lw_values-value4 = 'ASSINATURA_ELETRONICA'.
*      ENDIF.
*
*      REPLACE space IN lw_values-value4 WITH '_'.

        CONDENSE lw_values-value4.

*-CS2019001753-20.06.2023-#103050-JT-inicio
        CONDENSE lw_values-value5.
*-CS2019001753-20.06.2023-#103050-JT-fim

        " 08.11.2024 - RAMON - 154395 -->
        " é passado ponto para depois apagar, no coupa nao vai precisar
        IF lw_values-value5 = '.'.
          CLEAR lw_values-value5.
        ENDIF.
        " 08.11.2024 - RAMON - 154395 --<

        lv_ret = lv_ret && me->zif_integracao_bry_adigital~at_json_parti_template.

        REPLACE '#01#' IN lv_ret WITH lw_values-value1.
        REPLACE '#02#' IN lv_ret WITH lw_values-value2.
        REPLACE '#03#' IN lv_ret WITH lw_values-value3.
        REPLACE '#04#' IN lv_ret WITH lw_values-value4.
        REPLACE '#05#' IN lv_ret WITH lw_values-value5.  "*-CS2019001753-20.06.2023-#103050-JT

        " 08.11.2024 - RAMON - 154395 -->

        " ( quando é initial passa true como default, era o valor que passava antes da alteração )
        IF lw_values-value6 IS INITIAL .
          REPLACE '#06#' IN lv_ret WITH 'true'."lw_values-value6.
        ELSE.
          REPLACE '#06#' IN lv_ret WITH lw_values-value6.
        ENDIF.

        " ( quando é initial passa true como default, era o valor que passava antes da alteração )
        IF lw_values-value7 IS INITIAL .
          REPLACE '#07#' IN lv_ret WITH 'false'."lw_values-value7.
        ELSE.
          REPLACE '#07#' IN lv_ret WITH lw_values-value7.
        ENDIF.

        " 08.11.2024 - RAMON - 154395 --<

        IF lv_tabix NE lv_lines.
          lv_ret = lv_ret && ','.
        ENDIF.

      ENDLOOP.

*     CHECK lv_ret IS NOT INITIAL.

      IF lv_ret IS NOT INITIAL.
        CONCATENATE lv_ret ',' '{"codigo":"77294254000194", "assinante":"false", "revisor":"false"}' INTO lv_ret. "ALRS 26.12.2022 PART. CONVIDADO (URGENTE)
      ENDIF.

    ENDIF.

*-CS2019001753-20.06.2023-#103050-JT-inicio
    IF me->zif_integracao_bry_adigital~at_dados-relacao_grupo_assinantes IS NOT INITIAL.

      IF lv_ret IS NOT INITIAL.
        lv_ret = lv_ret && ','.
      ENDIF.

      SPLIT me->zif_integracao_bry_adigital~at_dados-relacao_grupo_assinantes AT ';' INTO TABLE lt_assinantes.

      SORT lt_assinantes.

      DELETE lt_assinantes WHERE value1 IS INITIAL.

      DELETE ADJACENT DUPLICATES FROM lt_assinantes.

      lv_lines = lines( lt_assinantes ).

      LOOP AT lt_assinantes ASSIGNING FIELD-SYMBOL(<fs_assinantes2>).

        lv_tabix = sy-tabix.

        SPLIT <fs_assinantes2>-value1 AT '#' INTO lw_values-value1 lw_values-value2 lw_values-value3 lw_values-value4 lw_values-value5.

        CONDENSE lw_values-value1.
        CONDENSE lw_values-value2.
        CONDENSE lw_values-value3.
        CONDENSE lw_values-value4.
        CONDENSE lw_values-value5.

        lv_ret = lv_ret && me->zif_integracao_bry_adigital~at_json_grupo_parti_template.

        REPLACE '#01#' IN lv_ret WITH lw_values-value1.
        REPLACE '#02#' IN lv_ret WITH lw_values-value2.
        REPLACE '#03#' IN lv_ret WITH lw_values-value3.
        REPLACE '#04#' IN lv_ret WITH lw_values-value4.
        REPLACE '#05#' IN lv_ret WITH lw_values-value5.

        IF lv_tabix NE lv_lines.
          lv_ret = lv_ret && ','.
        ENDIF.

      ENDLOOP.
    ENDIF.
*-CS2019001753-20.06.2023-#103050-JT-fim

    r_json = '[' && lv_ret && ']'.

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital~set_ds_data.

    DATA lv_partici TYPE string.

*    DATA(lv_partici) = '[{"codigo":"03122221144", "nome":"Wellington Pereira", "email":"ramon.lima@reclike.com.br",' &&
*                       '"assinante":"true", "revisor":"false", "tipoAssinatura":"ASSINATURA_ELETRONICA"}]'.
*
*    IF lv_partici IS INITIAL.
    lv_partici = zif_integracao_bry_adigital~get_participantes( ).
*    ENDIF.

*-CS2021000218-10.11.2022-JT-#92371-inicio
    IF me->zif_integracao_bry_adigital~at_dados-endpoint_coletas            IS INITIAL AND
       me->zif_integracao_bry_adigital~at_dados-endpoint_posicao_assina     IS INITIAL AND
       me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_assinado   IS INITIAL AND
       me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_rel_assina IS INITIAL AND
       me->zif_integracao_bry_adigital~at_dados-endpoint_get_participantes  IS INITIAL AND
       me->zif_integracao_bry_adigital~at_dados-endpoint_cancelar_coletas   IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_bry_adigital~at_auth_ws-content_type.
    ELSE.
      me->zif_integracao_inject~at_info_request_http-ds_content_type = abap_off.
    ENDIF.
*-CS2021000218-10.11.2022-JT-#92371-fim

*-CS2021000218-10.11.2022-JT-#92371-inicio
    IF me->zif_integracao_bry_adigital~at_dados-metodo_envio IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    ELSE.
      me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_integracao_bry_adigital~at_dados-metodo_envio.
    ENDIF.
*-CS2021000218-10.11.2022-JT-#92371-fim

    LOOP AT me->zif_integracao_bry_adigital~at_anexos-attachments-attachment ASSIGNING FIELD-SYMBOL(<fs_anexos>).

      DATA(lv_value) = 'form-data; name="documento"; filename="' && <fs_anexos>-file_name && '"'.

      APPEND VALUE #( header_field = 'Content-Type' header_value = <fs_anexos>-application_type  ) TO me->zif_integracao_inject~at_multipart_fields.
      APPEND VALUE #( header_field = 'content-disposition' header_value = lv_value xvalue = <fs_anexos>-xfile ) TO me->zif_integracao_inject~at_multipart_fields.

    ENDLOOP.

*-CS2021000218-10.11.2022-JT-#92371-inicio
    IF     me->zif_integracao_bry_adigital~at_dados-endpoint_posicao_assina_json IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_body = me->zif_integracao_bry_adigital~at_dados-endpoint_posicao_assina_json.
    ELSEIF lv_partici IS NOT INITIAL AND
         ( me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_assinado  IS INITIAL AND  "*-CS2019001753-18.12.2023-#657230-JT
           me->zif_integracao_bry_adigital~at_dados-endpoint_get_participantes IS INITIAL ).
      APPEND VALUE #( header_field = 'Content-Type' header_value = 'application/text'   ) TO me->zif_integracao_inject~at_multipart_fields.
      APPEND VALUE #( header_field = 'content-disposition' header_value = 'form-data; name="participante"' value = lv_partici ) TO me->zif_integracao_inject~at_multipart_fields.
    ENDIF.
*-CS2021000218-10.11.2022-JT-#92371-fim

    r_object = me.

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital~set_ds_url.

    me->zif_integracao_bry_adigital~set_end_point( ).

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    "me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_integracao_bry_adigital~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_bry_adigital~at_auth_ws-url && me->zif_integracao_bry_adigital~at_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_integracao_bry_adigital~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.

    r_if_object = me.

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital~set_end_point.

    DATA lv_responsavel TYPE string.

*-CS2021000218-10.11.2022-JT-#92371-inicio
    IF     me->zif_integracao_bry_adigital~at_dados-endpoint_coletas            IS NOT INITIAL.
      REPLACE '#01#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-endpoint_coletas.
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_posicao_assina     IS NOT INITIAL.
      REPLACE '#01#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-endpoint_posicao_assina.
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_assinado   IS NOT INITIAL.
      REPLACE '#01#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_assinado.
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_rel_assina IS NOT INITIAL.
      REPLACE '#01#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-endpoint_get_pdf_rel_assina.
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_get_participantes  IS NOT INITIAL.
      REPLACE '#01#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-endpoint_get_participantes.
    ELSEIF me->zif_integracao_bry_adigital~at_dados-endpoint_cancelar_coletas   IS NOT INITIAL.
      REPLACE '#01#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-endpoint_cancelar_coletas.
    ELSE.
 "// BUG-174950 WBARBOSA 23/04/25  REMOVIDO
*-CS2021000218-10.11.2022-JT-#92371-fim

*** Stefanini - IR217194 - 20/01/2025 - GGARAUJO1 - Início de Alteração
*      CALL FUNCTION 'Z_CONVERTE_CARACTERES'
*       EXPORTING
*         IV_STRING       = me->zif_integracao_bry_adigital~at_dados-nomecoleta
*       IMPORTING
*         EV_STRING       = me->zif_integracao_bry_adigital~at_dados-nomecoleta.
*
*      CALL FUNCTION 'Z_CONVERTE_CARACTERES'
*       EXPORTING
*         IV_STRING       = me->zif_integracao_bry_adigital~at_dados-descricao
*       IMPORTING
*         EV_STRING       = me->zif_integracao_bry_adigital~at_dados-descricao
                .
*** Stefanini - IR217194 - 20/01/2025 - GGARAUJO1 - Fim de Alteração
 "// BUG-174950 WBARBOSA 23/04/25  REMOVIDO
      REPLACE '#01#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-nomecoleta.
      REPLACE '#02#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-datalimite.
      REPLACE '#03#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-descricao.
      REPLACE '#04#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-padraoassinatura.
      REPLACE '#05#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-exigirdownload.
      REPLACE '#06#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-proibirrejeicao.
      REPLACE '#07#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-assinaturasequencial.
      REPLACE '#08#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-agrupardocumentos.
      REPLACE '#09#' IN me->zif_integracao_bry_adigital~at_end_point WITH me->zif_integracao_bry_adigital~at_dados-local_assinatura.


      IF me->zif_integracao_bry_adigital~at_dados-codigoresponsavel IS INITIAL.

        REPLACE '#10#' IN me->zif_integracao_bry_adigital~at_end_point WITH space.

      ELSE.

        lv_responsavel = '&codigoResponsavel=' && me->zif_integracao_bry_adigital~at_dados-codigoresponsavel.
        "&& '&configuracaoLocalAssinatura=' && me->zif_integracao_bry_adigital~at_dados-configuracaolocalassinatura.

        REPLACE '#10#' IN me->zif_integracao_bry_adigital~at_end_point WITH lv_responsavel.

      ENDIF.

    ENDIF.  "*-CS2021000218-10.11.2022-JT-#92371-fim

  ENDMETHOD.


  METHOD zif_integracao_bry_adigital~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_object = me.

    CREATE OBJECT lc_integrar.

    lc_integrar->zif_integracao~at_multipart = me->zif_integracao_inject~at_multipart_fields.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

*    IF .
*
*    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    DATA LT_HEADER_FIELDS  TYPE ZDE_HEADER_FIELD_T.

    "APPEND VALUE #( name = '~enctype' value = 'multipart/form-data' ) TO lt_header_fields.
    "APPEND VALUE #( name = 'Content-Type' value = 'multipart/form-data' ) TO lt_header_fields.
    "APPEND VALUE #( name = 'Accept-Encoding' value = 'gzip, deflate, br' ) TO lt_header_fields.
    "APPEND VALUE #( name = 'Accept' value = '*/*' ) TO lt_header_fields.

    APPEND VALUE #( NAME = 't' VALUE = ME->ZIF_INTEGRACAO_BRY_ADIGITAL~AT_AUTH_WS-ADD01 ) TO LT_HEADER_FIELDS.
    APPEND VALUE #( NAME = 'Authorization' VALUE = |Basic { ME->ZIF_INTEGRACAO_BRY_ADIGITAL~AT_AUTH_WS-TOKEN }| ) TO LT_HEADER_FIELDS. "// BUG-174950 WBARBOSA 23/04/25

    ME->ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP( I_HEADER_FIELDS = LT_HEADER_FIELDS ).

    R_IF_INTEGRACAO_INJECT = ME.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

*    DATA: ordem_servico TYPE zde_kuhlmann_hvi_retorno.
*break abap.
*    "Implementar Confirmação de Leitura """""""""""""""""""""""""""""""""""""
*    "/UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_RETORNO CHANGING DATA = ORDEM_SERVICO ).
*
*    CHECK ordem_servico-os_id IS NOT INITIAL.

*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    ZCL_INTEGRACAO_COF_KUHLMANN=>ZIF_INTEGRACAO_COF_KUHLMANN~GET_INSTANCE(
*      )->SET_HVI_CONFIRMAR( EXPORTING
*        "I_USUARIO = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_USUARIO
*        "I_SENHA   = ME->ZIF_INTEGRACAO_HVI_KUHLMANN~AT_SENHA
*        I_OS_ID   = ORDEM_SERVICO-OS_ID
*      ).
*    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
