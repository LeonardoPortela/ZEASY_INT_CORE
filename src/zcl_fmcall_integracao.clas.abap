class ZCL_FMCALL_INTEGRACAO definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools JS .

*"* public components of class ZCL_FMCALL_INTEGRACAO
*"* do not include other source files here!!!
  interfaces IF_HTTP_EXTENSION .
  interfaces ZIF_FMCALL_APP_MOBILE .

  constants XNL type ABAP_CHAR1 value %_NEWLINE ##NO_TEXT.
  constants XCRLF type ABAP_CR_LF value %_CR_LF ##NO_TEXT.
  data MY_SERVICE type STRING .
  data MY_URL type STRING .

  class-methods ABAP2JSON
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
      !UPCASE type XFELD optional
      !CAMELCASE type XFELD optional
    returning
      value(JSON_STRING) type STRING
    exceptions
      ERROR_IN_DATA_DESCRIPTION .
  class-methods ABAP2PDF
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
    returning
      value(PDF_XSTRING) type XSTRING
    exceptions
      ERROR_IN_DATA_DESCRIPTION .
  class-methods ABAP2PERL
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
      !UPCASE type XFELD optional
    returning
      value(PERL_STRING) type STRING
    exceptions
      ERROR_IN_DATA_DESCRIPTION .
  class-methods ABAP2XML
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
      !WITH_XML_HEADER type ABAP_BOOL default ABAP_FALSE
      !UPCASE type XFELD optional
      !NAME_ATR type STRING optional
    returning
      value(XML_STRING) type STRING .
  class-methods ABAP2YAML
    importing
      !ABAP_DATA type DATA
      !NAME type STRING optional
      !UPCASE type XFELD optional
      !Y_LEVEL type I default 0
      !S_INDEX type I default 0
      !FIRST_ROW type XFELD optional
      !DONT_INDENT type XFELD optional
    returning
      value(YAML_STRING) type STRING
    exceptions
      ERROR_IN_DATA_DESCRIPTION .
  class-methods BUILD_PARAMS
    importing
      !FUNCTION_NAME type RS38L_FNAM
    exporting
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB
      !PARAMS type ANY
    exceptions
      INVALID_FUNCTION
      UNSUPPORTED_PARAM_TYPE .
  class-methods JSON2ABAP
    importing
      !JSON_STRING type STRING optional
      !VAR_NAME type STRING optional
      !PROPERTY_PATH type STRING default 'json_obj'
    exporting
      value(PROPERTY_TABLE) type JS_PROPERTY_TAB
    changing
      !JS_OBJECT type ref to CL_JAVA_SCRIPT optional
      value(ABAP_DATA) type ANY optional
    raising
      ZCX_JSON .
  class-methods JSON_DESERIALIZE
    importing
      !JSON type STRING
    changing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
    raising
      ZCX_JSON .
  methods NOTES
    returning
      value(TEXT) type STRING .
  class-methods SERIALIZE_JSON
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !CAMELCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !O_STRING type STRING .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods GERA_ERRO_GERAL_SYS
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods SERIALIZE_PDF
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !CAMELCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !O_XSTRING type XSTRING
      !O_NAME type STRING .
  class-methods SERIALIZE_PERL
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !FUNCNAME type RS38L_FNAM
    exporting
      !PERL_STRING type STRING .
  class-methods SERIALIZE_XML
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !FUNCNAME type RS38L_FNAM
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !FORMAT type STRING optional
    exporting
      !O_STRING type STRING .
  class-methods SERIALIZE_YAML
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !YAML_STRING type STRING .
  class-methods DESERIALIZE_ID
    importing
      !JSON type STRING
    changing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
    raising
      ZCX_JSON .
  class-methods SERIALIZE_ID
    importing
      !PARAMTAB type ABAP_FUNC_PARMBIND_TAB
      !PARAMS type ANY optional
      !EXCEPTAB type ABAP_FUNC_EXCPBIND_TAB optional
      !SHOW_IMPP type ABAP_BOOL optional
      !JSONP type STRING optional
      !LOWERCASE type ABAP_BOOL default ABAP_FALSE
      !FORMAT type STRING default 'JSON'
      !FUNCNAME type RS38L_FNAM optional
      !CAMELCASE type ABAP_BOOL default ABAP_FALSE
    exporting
      !O_STRING type STRING
    raising
      ZCX_JSON .
  class-methods CONVERT_TO_UTF8
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
  class-methods LOGIN
    importing
      value(I_SERVER) type ref to IF_HTTP_SERVER
    exporting
      !E_OUT_LOGIN type ZDE_OUT_LOGIN_APP_MOBILE_OUT
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZCL_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods LOGOFF
    importing
      value(I_SERVER) type ref to IF_HTTP_SERVER
    returning
      value(R_INSTANCE) type ref to ZCL_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods VALIDA_TOKEN
    importing
      value(I_SERVER) type ref to IF_HTTP_SERVER
    exporting
      !E_OUT_LOGIN type ZDE_OUT_LOGIN_APP_MOBILE_OUT
      !E_JSON type STRING
    returning
      value(R_INSTANCE) type ref to ZCL_FMCALL_APP_MOBILE
    raising
      ZCX_FMCALL_APP_MOBILE .
  class-methods CONVERT_TXT_JSON_TO_STRING
    importing
      !I_TEXTO type STRING
    returning
      value(R_TEXTO) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FMCALL_INTEGRACAO IMPLEMENTATION.


METHOD ABAP2JSON.
*/**********************************************/*
*/ This method takes any ABAP data variable and /*
*/ returns a string representing its value in   /*
*/ JSON format.                                 /*
*/ ABAP references are always de-referenced and /*
*/ treated as normal variables.                 /*
*/**********************************************/*

  TYPE-POOLS: ABAP.

  CONSTANTS:
    C_COMMA TYPE C VALUE ',',
    C_COLON TYPE C VALUE ':',
    C_QUOTE TYPE C VALUE '"'.

  DATA:
    DONT_QUOTE      TYPE XFELD,
    JSON_FRAGMENTS  TYPE TABLE OF STRING,
    REC_JSON_STRING TYPE STRING,
    L_TYPE          TYPE C,
    S_TYPE          TYPE C,
    L_COMPS         TYPE I,
    L_LINES         TYPE I,
    L_INDEX         TYPE I,
    L_VALUE         TYPE STRING,
    L_NAME          TYPE STRING,
    L_STRUDESCR     TYPE REF TO CL_ABAP_STRUCTDESCR.

  FIELD-SYMBOLS:
    <ABAP_DATA> TYPE ANY,
    <ITAB>      TYPE ANY TABLE,
    <STRU>      TYPE ANY TABLE,
    <COMP>      TYPE ANY,
    <ABAPCOMP>  TYPE ABAP_COMPDESCR.

  DEFINE GET_SCALAR_VALUE.
    " &1 : assigned var
    " &2 : abap data
    " &3 : abap type
    &1 = &2.
****************************************************
* Adapt some basic ABAP types (pending inclusion of all basic abap types?)
* Feel free to customize this for your needs
    case &3.
*       1. ABAP numeric types
      when 'I'. " Integer
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.
        dont_quote = 'X'.

      when 'F'. " Float
        condense &1.
        dont_quote = 'X'.

      when 'P'. " Packed number (used in quantities or currency, for example)
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.
        dont_quote = 'X'.

      when 'X'. " Hexadecimal
        condense &1.
        concatenate '0x' &1 into &1.
*        dont_quote = 'X'.
*        "Quote it, as JSON doesn't support Hex or Octal as native types.

*       2. ABAP char types
      when 'D'. " Date type
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO &1.

      when 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.

      when 'N'. " Numeric text field
*           condense &1.

      when 'C' or 'g'. " Char sequences and Strings
* Put safe chars
        replace all occurrences of '\' in &1 with '\\' .
        replace all occurrences of '"' in &1 with '\"' .
        replace all occurrences of cl_abap_char_utilities=>cr_lf in &1 with '\r\n' .
        replace all occurrences of cl_abap_char_utilities=>newline in &1 with '\n' .
        replace all occurrences of cl_abap_char_utilities=>horizontal_tab in &1 with '\t' .
        replace all occurrences of cl_abap_char_utilities=>backspace in &1 with '\b' .
        replace all occurrences of cl_abap_char_utilities=>form_feed in &1 with '\f' .

      when 'y'.  " XSTRING
* Put the XSTRING in Base64
        &1 = cl_http_utility=>ENCODE_X_BASE64( &2 ).

      when others.
* Don't hesitate to add and modify scalar abap types to suit your taste.

    endcase.
** End of scalar data preparing.

* Enclose value in quotes (or not)
    if dont_quote ne 'X'.
      concatenate c_quote &1 c_quote into &1.
    endif.
    clear dont_quote.
  END-OF-DEFINITION.

***************************************************
*  Prepare field names, JSON does quote names!!   *
*  You must be strict in what you produce.        *
***************************************************
  IF NAME IS NOT INITIAL.
    CONCATENATE C_QUOTE NAME C_QUOTE C_COLON INTO REC_JSON_STRING.
    APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
    CLEAR REC_JSON_STRING.
  ENDIF.

**
* Get ABAP data type
  DESCRIBE FIELD ABAP_DATA TYPE L_TYPE COMPONENTS L_COMPS.

***************************************************
*  Get rid of data references
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.
    ASSIGN ABAP_DATA->* TO <ABAP_DATA>.
    IF SY-SUBRC NE 0.
      APPEND '{}' TO JSON_FRAGMENTS.
      CONCATENATE LINES OF JSON_FRAGMENTS INTO JSON_STRING.
      EXIT.
    ENDIF.
  ELSE.
    ASSIGN ABAP_DATA TO <ABAP_DATA>.
  ENDIF.

* Get ABAP data type again and start
  DESCRIBE FIELD <ABAP_DATA> TYPE L_TYPE COMPONENTS L_COMPS.

***************************************************
*  Tables
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE.
* '[' JSON table opening bracket
    APPEND '[' TO JSON_FRAGMENTS.
    ASSIGN <ABAP_DATA> TO <ITAB>.
    L_LINES = LINES( <ITAB> ).
    LOOP AT <ITAB> ASSIGNING <COMP>.
      ADD 1 TO L_INDEX.
*> Recursive call for each table row:
      REC_JSON_STRING = ABAP2JSON( ABAP_DATA = <COMP> UPCASE = UPCASE CAMELCASE = CAMELCASE ).
      APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
      CLEAR REC_JSON_STRING.
      IF L_INDEX < L_LINES.
        APPEND C_COMMA TO JSON_FRAGMENTS.
      ENDIF.
    ENDLOOP.
    APPEND ']' TO JSON_FRAGMENTS.
* ']' JSON table closing bracket


***************************************************
*  Structures
***************************************************
  ELSE.
    IF L_COMPS IS NOT INITIAL.
* '{' JSON object opening curly brace
      APPEND '{' TO JSON_FRAGMENTS.
      L_STRUDESCR ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ).
      LOOP AT L_STRUDESCR->COMPONENTS ASSIGNING <ABAPCOMP>.
        L_INDEX = SY-TABIX .
        ASSIGN COMPONENT <ABAPCOMP>-NAME OF STRUCTURE <ABAP_DATA> TO <COMP>.
        L_NAME = <ABAPCOMP>-NAME.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        IF UPCASE NE 'X'.
          " translate l_name to lower case.
          L_NAME = TO_LOWER( L_NAME ).
        ENDIF.
        IF CAMELCASE EQ 'X'.
          L_NAME = TO_MIXED( VAL = L_NAME  CASE = 'a' ).
        ENDIF.
        DESCRIBE FIELD <COMP> TYPE S_TYPE.
        IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF OR
           S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT1 OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT2.
*> Recursive call for non-scalars:
          REC_JSON_STRING = ABAP2JSON( ABAP_DATA = <COMP> NAME = L_NAME UPCASE = UPCASE CAMELCASE = CAMELCASE ).
        ELSE.
          IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_OREF OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_IREF.
            REC_JSON_STRING = '"REF UNSUPPORTED"'.
          ELSE.
            GET_SCALAR_VALUE REC_JSON_STRING <COMP> S_TYPE.
          ENDIF.
          CONCATENATE C_QUOTE L_NAME C_QUOTE C_COLON REC_JSON_STRING INTO REC_JSON_STRING.
        ENDIF.
        APPEND REC_JSON_STRING TO JSON_FRAGMENTS.
        CLEAR REC_JSON_STRING. CLEAR L_NAME.
        IF L_INDEX < L_COMPS.
          APPEND C_COMMA TO JSON_FRAGMENTS.
        ENDIF.
      ENDLOOP.
      APPEND '}' TO JSON_FRAGMENTS.
* '}' JSON object closing curly brace


****************************************************
*                  - Scalars -                     *
****************************************************
    ELSE.
      GET_SCALAR_VALUE L_VALUE <ABAP_DATA> L_TYPE.
      APPEND L_VALUE TO JSON_FRAGMENTS.

    ENDIF.
* End of structure/scalar IF block.
***********************************

  ENDIF.
* End of main IF block.
**********************

* Use a loop in older releases that don't support concatenate lines.
  CONCATENATE LINES OF JSON_FRAGMENTS INTO JSON_STRING.

ENDMETHOD.


METHOD ABAP2PDF.
*/**********************************************/*
*/ This method takes any ABAP data variable and /*
*/ returns a string representing its value in   /*
*/ JSON format.                                 /*
*/ ABAP references are always de-referenced and /*
*/ treated as normal variables.                 /*
*/**********************************************/*

  TYPE-POOLS: ABAP.

  CONSTANTS:
    C_COMMA TYPE C VALUE ',',
    C_COLON TYPE C VALUE ':',
    C_QUOTE TYPE C VALUE '"'.

  DATA:
    PDF_FRAGMENTS TYPE TABLE OF XSTRING,
    L_TYPE        TYPE C,
    L_COMPS       TYPE I,
    L_VALUE       TYPE XSTRING.

  FIELD-SYMBOLS:
    <ABAP_DATA> TYPE ANY,
    <ITAB>      TYPE ANY TABLE.

* Get ABAP data type
  DESCRIBE FIELD ABAP_DATA TYPE L_TYPE COMPONENTS L_COMPS.

***************************************************
*  Get rid of data references
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.
    ASSIGN ABAP_DATA->* TO <ABAP_DATA>.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
  ELSE.
    ASSIGN ABAP_DATA TO <ABAP_DATA>.
  ENDIF.

* Get ABAP data type again and start
  DESCRIBE FIELD <ABAP_DATA> TYPE L_TYPE COMPONENTS L_COMPS.

  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE.

    ASSIGN <ABAP_DATA> TO <ITAB>.
    LOOP AT <ITAB> ASSIGNING FIELD-SYMBOL(<COMP>).
      L_VALUE = <COMP>.
      APPEND L_VALUE TO PDF_FRAGMENTS.
    ENDLOOP.

  ELSEIF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_XSTRING.

    L_VALUE = <ABAP_DATA>.
    PDF_XSTRING = L_VALUE.
    APPEND L_VALUE TO PDF_FRAGMENTS.

  ELSE.

    L_VALUE = <ABAP_DATA>.
    APPEND L_VALUE TO PDF_FRAGMENTS.

  ENDIF.

ENDMETHOD.


method ABAP2PERL.
*/**********************************************/*
*/ This method takes any ABAP data variable and /*
*/ returns a string representing its value in   /*
*/ Perl Data::Dumper format, ready to be evaled /*
*/ in a Perl program.                           /*
*/**********************************************/*

  type-pools: abap.

  constants:
    c_comma type c value ',',
    c_colon type c value ':',
    c_quote type c value ''''.

  data:
    perl_hash_assign type string,
    dont_quote type xfeld,
    perl_fragments type table of string,
    rec_perl_string type string,
    l_type  type c,
    s_type  type c,
    l_comps type i,
    l_lines type i,
    l_index type i,
    l_value type string,
    l_name  type string,
    l_typedescr type ref to cl_abap_structdescr.

  field-symbols:
    <abap_data> type any,
    <itab> type any table,
    <stru> type any table,
    <comp> type any,
    <abapcomp> type abap_compdescr.

  concatenate space '=>' space into perl_hash_assign respecting blanks.

  define get_scalar_value.
    " &1 : assigned var
    " &2 : abap data
    " &3 : abap type
    &1 = &2.
****************************************************
* Adapt some basic ABAP types (pending inclusion of all basic abap types?)
* Feel free to customize this for your needs
    case &3.
*       1. ABAP numeric types
      when 'I'. " Integer
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.
        dont_quote = 'X'.

      when 'F'. " Float
        condense &1.
        dont_quote = 'X'.

      when 'P'. " Packed number (used in quantities, for example)
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.
        dont_quote = 'X'.

      when 'X'. " Hexadecimal
        condense &1.
        concatenate '0x' &1 into &1.
        dont_quote = 'X'.

*       2. ABAP char types
      when 'D'. " Date type
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO &1.

      when 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.

      when 'N'. " Numeric text field
*           condense &1.

      when 'C' or 'g'. " Char sequences and Strings
* Put safe chars
        replace all occurrences of '''' in &1 with '\''' .

      when 'y'.  " XSTRING
* Put the XSTRING in Base64
        &1 = cl_http_utility=>ENCODE_X_BASE64( &2 ).

      when others.
* Don't hesitate to add and modify abap types to suit your taste.

    endcase.
** End of scalar data preparing.

* Enclose value in quotes (or not)
    if dont_quote ne 'X'.
      concatenate c_quote &1 c_quote into &1.
    endif.
    clear dont_quote.

  end-of-definition.



***************************************************
*  Prepare field names, we use single quotes.     *
*  You must be strict in what you produce.        *
***************************************************
  if name is not initial.
    concatenate c_quote name c_quote perl_hash_assign into rec_perl_string respecting blanks.
    append rec_perl_string to perl_fragments.
    clear rec_perl_string.
  endif.

**
* Get ABAP data type
  describe field abap_data type l_type components l_comps.

***************************************************
*  Get rid of data references
***************************************************
  if l_type eq cl_abap_typedescr=>typekind_dref.
    assign abap_data->* to <abap_data>.
    if sy-subrc ne 0.
      append '{}' to perl_fragments.
      concatenate lines of perl_fragments into perl_string.
      exit.
    endif.
  else.
    assign abap_data to <abap_data>.
  endif.


* Get ABAP data type again and start
  describe field <abap_data> type l_type components l_comps.

***************************************************
*  Tables
***************************************************
  if l_type eq cl_abap_typedescr=>typekind_table.
* '[' Table opening bracket
    append '[' to perl_fragments.
    assign <abap_data> to <itab>.
    l_lines = lines( <itab> ).
    loop at <itab> assigning <comp>.
      add 1 to l_index.
*> Recursive call here
      rec_perl_string = abap2perl( abap_data = <comp> upcase = upcase ).
      append rec_perl_string to perl_fragments.
      clear rec_perl_string.
      if l_index < l_lines.
        append c_comma to perl_fragments.
      endif.
    endloop.
    append ']' to perl_fragments.
* ']' Table closing bracket


***************************************************
*  Structures
***************************************************
  else .
    if l_comps is not initial.
* '{' Object opening curly brace
      append '{' to perl_fragments .
      l_typedescr ?= cl_abap_typedescr=>describe_by_data( <abap_data> ) .
      loop at l_typedescr->components assigning <abapcomp> .
        l_index = sy-tabix .
        assign component <abapcomp>-name of structure <abap_data> to <comp>.
        l_name = <abapcomp>-name.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        if upcase ne 'X'.
          translate l_name to lower case.
        endif.
        describe field <comp> type s_type.
        if s_type eq cl_abap_typedescr=>typekind_table or s_type eq cl_abap_typedescr=>typekind_dref or
           s_type eq cl_abap_typedescr=>typekind_struct1 or s_type eq cl_abap_typedescr=>typekind_struct2.
*> Recursive call for non-scalars:
          rec_perl_string = abap2perl( abap_data = <comp> name = l_name upcase = upcase ).
        else.
          if s_type eq cl_abap_typedescr=>TYPEKIND_OREF or s_type eq cl_abap_typedescr=>TYPEKIND_IREF.
            rec_perl_string = '"REF UNSUPPORTED"'.
          else.
            get_scalar_value rec_perl_string <comp> s_type.
          endif.
          concatenate c_quote l_name c_quote perl_hash_assign rec_perl_string into rec_perl_string.
        endif.

        append rec_perl_string to perl_fragments.
        clear rec_perl_string.
        if l_index < l_comps.
          append c_comma to perl_fragments.
        endif.
      endloop.
      append '}' to perl_fragments.
* '}' Object closing curly brace


****************************************************
*                  - Scalars -                     *
****************************************************
    else.

      get_scalar_value l_value <abap_data> l_type.
      append l_value to perl_fragments.

    endif.
* End of structure/scalar IF block.
***********************************


  endif.
* End of main IF block.
**********************


* Use a loop in older releases that don't support concatenate lines.
  concatenate lines of perl_fragments into perl_string.

endmethod.


METHOD ABAP2XML.
*
*/ Look at method serialize_id for a new way of doing XML.

  TYPE-POOLS: ABAP.

  CONSTANTS:
    XML_HEAD TYPE STRING VALUE '<?xml version="1.0" encoding="utf-8"?>',
    ITEM_ATR TYPE STRING VALUE 'idx="#"'.

  DATA:
    XML_FRAGMENTS  TYPE TABLE OF STRING,
    REC_XML_STRING TYPE STRING,
    L_TYPE         TYPE C,
    S_TYPE         TYPE C,
    L_COMPS        TYPE I,
    L_VALUE        TYPE STRING,
    T_STRING       TYPE STRING,
    L_ITEM_ATR     TYPE STRING,
    L_ITEM_STR     TYPE STRING,
    L_NAME         TYPE STRING,
    L_IDX          TYPE STRING,
    L_TYPEDESCR    TYPE REF TO CL_ABAP_STRUCTDESCR,
    L_LINEDESCR    TYPE REF TO CL_ABAP_DATADESCR,
    L_TABLEDESCR   TYPE REF TO CL_ABAP_TABLEDESCR.

  FIELD-SYMBOLS:
    <ABAP_DATA> TYPE ANY,
    <ITAB>      TYPE ANY TABLE,
    <STRU>      TYPE ANY TABLE,
    <COMP>      TYPE ANY,
    <ABAPCOMP>  TYPE ABAP_COMPDESCR.

  DEFINE GET_SCALAR_VALUE.
    " &1 : assigned var
    " &2 : abap data
    " &3 : abap type
    " &4 : scape
    &1 = &2.
****************************************************
* Adapt some basic ABAP types (pending inclusion of all basic abap types?)
* Feel free to customize this for your needs
    case &3.
*       1. ABAP numeric types
      when 'I'. " Integer
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.

      when 'F'. " Float
        condense &1.

      when 'P'. " Packed number (used in quantities, for example)
        condense &1.
        if sign( &1 ) < 0.
          shift &1 by 1 places right circular.
        endif.

      when 'X'. " Hexadecimal
        condense &1.
        concatenate '0x' &1 into &1.

*       2. ABAP char types
      when 'D'. " Date type
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO &1.

      when 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.

      when 'N'. " Numeric text field
*           condense &1.

      when 'C' or 'g'. " Char sequences and Strings
* Put safe chars
        t_string = &2.

        if &4 = abap_true.
        &1 = cl_http_utility=>escape_html( t_string ).
        endif.

      when 'y'.  " XSTRING
* Put the XSTRING in Base64
        &1 = cl_http_utility=>ENCODE_X_BASE64( &2 ).

      when others.
* Don't hesitate to add and modify abap types to suit your taste.

    endcase.
** End of scalar data preparing.

  END-OF-DEFINITION.



*******************************
* Put XML header if requested *
*******************************
  IF WITH_XML_HEADER EQ ABAP_TRUE.
    APPEND XML_HEAD TO XML_FRAGMENTS.
  ENDIF.

***************************************************
*  Open XML tag                                   *
*  <          >                                   *
***************************************************
  IF NAME IS NOT INITIAL.
    L_NAME = NAME.
    IF NAME_ATR IS NOT INITIAL.
      CONCATENATE NAME NAME_ATR INTO L_NAME SEPARATED BY SPACE.
    ENDIF.
    CONCATENATE '<' L_NAME '>' INTO REC_XML_STRING.
    APPEND REC_XML_STRING TO XML_FRAGMENTS.
    CLEAR REC_XML_STRING.
  ENDIF.

**
* Get ABAP data type
  DESCRIBE FIELD ABAP_DATA TYPE L_TYPE COMPONENTS L_COMPS .

***************************************************
*  Get rid of data references
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.
    ASSIGN ABAP_DATA->* TO <ABAP_DATA>.
    IF SY-SUBRC NE 0.
      IF NAME IS NOT INITIAL.
        CONCATENATE '<' NAME '/>' INTO XML_STRING.
      ELSE.
        CLEAR XML_STRING.
      ENDIF.
      EXIT.
    ENDIF.
  ELSE.
    ASSIGN ABAP_DATA TO <ABAP_DATA>.
  ENDIF.


* Get ABAP data type again and start
  DESCRIBE FIELD <ABAP_DATA> TYPE L_TYPE COMPONENTS L_COMPS.


***************************************************
*  Tables
***************************************************
  IF L_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE.
    L_TABLEDESCR ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ).
    L_LINEDESCR = L_TABLEDESCR->GET_TABLE_LINE_TYPE( ).
    L_ITEM_STR = L_LINEDESCR->GET_RELATIVE_NAME( ).
    ASSIGN <ABAP_DATA> TO <ITAB>.
    LOOP AT <ITAB> ASSIGNING <COMP>.
      L_IDX = SY-TABIX.
      CONDENSE L_IDX.
      L_ITEM_ATR = ITEM_ATR.
      REPLACE '#' IN L_ITEM_ATR WITH L_IDX.
      IF UPCASE NE 'X'.
        TRANSLATE L_ITEM_STR TO LOWER CASE.
      ENDIF.
*> Recursive call for line items here:
      REC_XML_STRING = ABAP2XML( ABAP_DATA = <COMP> UPCASE = UPCASE NAME = L_ITEM_STR NAME_ATR = L_ITEM_ATR ).
      APPEND REC_XML_STRING TO XML_FRAGMENTS.
      CLEAR REC_XML_STRING.
    ENDLOOP.


***************************************************
*  Structures
***************************************************
  ELSE .
    IF L_COMPS IS NOT INITIAL.
      L_TYPEDESCR ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( <ABAP_DATA> ).
      LOOP AT L_TYPEDESCR->COMPONENTS ASSIGNING <ABAPCOMP> .
        ASSIGN COMPONENT <ABAPCOMP>-NAME OF STRUCTURE <ABAP_DATA> TO <COMP>.
        L_NAME = <ABAPCOMP>-NAME. " l_value justs holds the name here.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        IF UPCASE NE 'X'.
          TRANSLATE L_NAME TO LOWER CASE.
        ENDIF.
        DESCRIBE FIELD <COMP> TYPE S_TYPE.
        IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_TABLE OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_DREF OR
           S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT1 OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_STRUCT2.
*> Recursive call for non-scalars:
          REC_XML_STRING = ABAP2XML( ABAP_DATA = <COMP> NAME = L_NAME UPCASE = UPCASE ).
        ELSE.
          IF S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_OREF OR S_TYPE EQ CL_ABAP_TYPEDESCR=>TYPEKIND_IREF.
            REC_XML_STRING = 'REF UNSUPPORTED'.
          ELSE.
            GET_SCALAR_VALUE REC_XML_STRING <COMP> S_TYPE ABAP_TRUE.
          ENDIF.
          CONCATENATE '<' L_NAME '>' REC_XML_STRING '</' L_NAME '>' INTO REC_XML_STRING.
        ENDIF.
        APPEND REC_XML_STRING TO XML_FRAGMENTS.
        CLEAR REC_XML_STRING.
      ENDLOOP.



****************************************************
*                  - Scalars -                     *
****************************************************
    ELSE.

      GET_SCALAR_VALUE L_VALUE <ABAP_DATA> L_TYPE ABAP_FALSE.
      APPEND L_VALUE TO XML_FRAGMENTS.

    ENDIF.
* End of structure/scalar IF block.
***********************************


  ENDIF.
* End of main IF block.
**********************


*****************
* Close XML tag *
*****************
  IF NAME IS NOT INITIAL.
    CONCATENATE '</' NAME '>' INTO REC_XML_STRING.
    APPEND REC_XML_STRING TO XML_FRAGMENTS.
    CLEAR REC_XML_STRING.
  ENDIF.

* Use a loop in older releases that don't support concatenate lines.
  CONCATENATE LINES OF XML_FRAGMENTS INTO XML_STRING.

ENDMETHOD.


method ABAP2YAML.
*********************
* ABAP goes to YAML *
*********************

  type-pools: abap.

  constants:
    c_comma     type c value ',',
    c_space     type c value ' ',
    c_colon     type c value ':',
    c_quote     type c value '"',
    c_squot     type c value '''',
    c_colo2(2)  type c value ': ',
    c_indt2     type i value 2,
    c_hyph      type c value '-'.

  data:
  ly_level type i,
  l_dont_indent type xfeld,
  dec_level type i value 0,
  dont_quote type xfeld,
  yaml_fragments type table of string,
  rec_yaml_string type string,
  l_type  type c ,
  l_comps type i ,
  l_lines type i ,
  l_index type i ,
  l_value type string,
  l_name type string.
  field-symbols:
    <abap_data> type any,
    <itab> type any table,
    <stru> type any table,
    <comp> type any.
  data l_typedescr type ref to cl_abap_structdescr .
  field-symbols <abapcomp> type abap_compdescr .

  ly_level = y_level.

**
* Get ABAP data type
  describe field abap_data type l_type components l_comps .

***************************************************
*  First of all, get rid of data references
***************************************************
  if l_type eq cl_abap_typedescr=>typekind_dref.
    assign abap_data->* to <abap_data>.
    if sy-subrc ne 0.
      yaml_string = space. " pasamos de poner nada si falla...
      exit.
    endif.
  else.
    assign abap_data to <abap_data>.
  endif.


* Get ABAP data type again and start
  describe field <abap_data> type l_type components l_comps.

***************************************************
*  Prepare field names, YAML does not quote names *
***************************************************
* Put hyphens...
  if name is initial and y_level gt 0.
    concatenate c_hyph space into rec_yaml_string respecting blanks.
    l_dont_indent = 'X'.
  endif.

  if name is not initial.
    concatenate name c_colon c_space into rec_yaml_string respecting blanks.
  endif.

* do indent
  if dont_indent ne 'X'.
    do  ly_level  times.
      shift rec_yaml_string right by c_indt2 places.
    enddo.
  endif.

  append rec_yaml_string to yaml_fragments.
  clear rec_yaml_string.




***************************************************
*  Tables
***************************************************
  if l_type eq cl_abap_typedescr=>TYPEKIND_TABLE.
    assign <abap_data> to <itab>.
    l_lines = lines( <itab> ).
    clear l_index.
    if l_lines eq 0.
      move '[]' to rec_yaml_string.
      append rec_yaml_string to yaml_fragments.
      clear rec_yaml_string.
      append xnl to yaml_fragments.
    else.
      if name is not initial.
        append xnl to yaml_fragments.
      endif.
      add 1 to ly_level.
      loop at <itab> assigning <comp>.
        add 1 to l_index.
*> Recursive call here
        rec_yaml_string = abap2yaml( abap_data = <comp> upcase = upcase y_level = ly_level s_index = l_index ).
        append rec_yaml_string to yaml_fragments.
        clear rec_yaml_string.
      endloop.
    endif.
* YAML table ends *
*******************


***************************************************
*  Structures
***************************************************
  else .
    if l_comps is not initial.
      if name is not initial.
        append xnl to yaml_fragments.
      endif.
      add 1 to ly_level.
* Loop for structure elements
      l_typedescr ?= cl_abap_typedescr=>describe_by_data( <abap_data> ) .
      clear l_index.
      loop at l_typedescr->components assigning <abapcomp>.
        add 1 to l_index.
        assign component <abapcomp>-name of structure <abap_data> to <comp>.
        l_name = <abapcomp>-name.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        if upcase ne 'X'.
          translate l_name to lower case.
        endif.
*> Recursive call here
        rec_yaml_string = abap2yaml( abap_data = <comp> name = l_name upcase = upcase y_level = ly_level s_index = l_index dont_indent = l_dont_indent ).
        clear l_dont_indent. " it is only used once
        append rec_yaml_string to yaml_fragments.
        clear rec_yaml_string.
      endloop.

* YAML structure ends *
***********************


***************************************************
*  Scalars and others...
***************************************************
    else.
      if l_type eq cl_abap_typedescr=>TYPEKIND_OREF or l_type eq cl_abap_typedescr=>TYPEKIND_IREF.
        l_value = 'REF UNSUPPORTED'.
      else.
        l_value = <abap_data>.
      endif.

* Adapt some basic ABAP types (pending inclusion of all basic abap types)
* Feel free to customize this for your needs
      case l_type.
*       1. ABAP numeric types
        when 'I'. " Integer
          condense l_value.
          if sign( l_value ) < 0.
            shift l_value by 1 places right circular.
          endif.
          dont_quote = 'X'.

        when 'F'. " Float
          condense l_value.
          dont_quote = 'X'.

        when 'P'. " Packed number (used in quantities, for example)
          condense l_value.
          if sign( l_value ) < 0.
            shift l_value by 1 places right circular.
          endif.
          dont_quote = 'X'.

        when 'X'. " Hexadecimal
          condense l_value.
          concatenate '0x' l_value into l_value.
          dont_quote = 'X'.

*       2. ABAP char types
        when 'D'. " Date type
          CONCATENATE l_value(4) '-' l_value+4(2) '-' l_value+6(2) INTO l_value.

        when 'T'. " Time representation
          CONCATENATE l_value(2) ':' l_value+2(2) ':' l_value+4(2) INTO l_value.

        when 'N'. " Numeric text field
*           condense l_value.

        when 'C' or 'g'. " Chars and Strings
* Put safe chars
          replace all occurrences of '\' in l_value with '\\' .
          replace all occurrences of '"' in l_value with '\"' .
          replace all occurrences of cl_abap_char_utilities=>cr_lf in l_value with '\r\n' .
          replace all occurrences of cl_abap_char_utilities=>newline in l_value with '\n' .
          replace all occurrences of cl_abap_char_utilities=>horizontal_tab in l_value with '\t' .
          replace all occurrences of cl_abap_char_utilities=>backspace in l_value with '\b' .
          replace all occurrences of cl_abap_char_utilities=>form_feed in l_value with '\f' .

        when 'y'.  " XSTRING
* Put the XSTRING in Base64
*          l_value = cl_http_utility=>ENCODE_X_BASE64( <abap_data> ).
          l_value = 'XSTRING not supported in YAML yet!'.

        when others.
* Don't hesitate to add and modify abap types to suit your taste.

      endcase.

* We use YAML scalars double quoted
      if dont_quote ne 'X'.
        concatenate c_quote l_value c_quote into l_value.
      else.
        clear dont_quote.
      endif.

      append l_value to yaml_fragments.

      append xnl to yaml_fragments.

    endif. " is structure or scalar

  endif. " main typekind sentence



* Use a loop in older releases that don't support concatenate lines.
  concatenate lines of yaml_fragments into yaml_string respecting blanks.

endmethod.


method BUILD_PARAMS.

  type-pools: ABAP.

  data defval type RS38L_DEFO.
  data dataname type string.
  data waref type ref to data.

  field-symbols:
    <wa> type any,
    <temp> type any.

  data len type i.
  data excnt type i value 1.

  data paramline  type line  of ABAP_FUNC_PARMBIND_TAB.
  data exceptline type line  of ABAP_FUNC_EXCPBIND_TAB.
  data t_params_p type table of RFC_FINT_P.
  data params_p   type RFC_FINT_P.

  define remove_enclosing_quotes.
    " Remove enclosing single quotes
    if &2 gt 1.
      subtract 1 from &2.
      if &1+&2 eq ''''.
        &1+&2 = space.
      endif.
      if &1(1) eq ''''.
        shift &1 left.
      endif.
      &2 = strlen( &1 ).
    endif.
  end-of-definition.


* do we have the rfc name?
  call function 'RFC_GET_FUNCTION_INTERFACE_P'
    EXPORTING
      funcname      = function_name
      language      = 'E'       "'D'  "sy-langu
    TABLES
      params_p      = t_params_p
    EXCEPTIONS
      fu_not_found  = 1
      nametab_fault = 2
      others        = 3.

  if sy-subrc <> 0.
    raise INVALID_FUNCTION.
  endif.


* Build params table
  loop at t_params_p into params_p.

    unassign <wa>.
    unassign <temp>.
    clear paramline.

    case params_p-paramclass.

      when 'I' or 'E' or 'C'.

        paramline-name = params_p-parameter.

        if params_p-paramclass = 'E'.
          paramline-kind = ABAP_FUNC_IMPORTING.
        elseif params_p-paramclass = 'I'.
          paramline-kind = ABAP_FUNC_EXPORTING.
        else.
          paramline-kind = ABAP_FUNC_CHANGING.
        endif.

        if params_p-fieldname is initial.
          dataname = params_p-tabname.
        else.
          concatenate params_p-tabname params_p-fieldname into
              dataname separated by '-'.
        endif.

* Assign default values
        defval = params_p-default.
        if dataname is initial.
           dataname = 'STRING'.  " use a STRING for this cases (see CONVERT_DATE_TO_EXTERNAL).
        endif.
        create data waref type (dataname).
        assign waref->* to <wa>.
        len = strlen( defval ).
        remove_enclosing_quotes defval len.
        if defval = 'SPACE'.
          <wa> = space.
        elseif len > 3 and defval+0(3) = 'SY-'.
          assign (defval) to <temp>.
          <wa> = <temp>.
          unassign <temp>.
        else.
          if defval is not initial.
            <wa> = defval.
          endif.
        endif.
        unassign <wa>.
        paramline-value = waref.
        insert paramline into table paramtab.

      when 'T'.
        paramline-name = params_p-parameter.
        paramline-kind = ABAP_FUNC_TABLES.
        if params_p-exid eq 'h'.
          create data waref type (params_p-tabname).
        else.
          create data waref type standard table of (params_p-tabname).
        endif.
        paramline-value = waref.
        insert paramline into table paramtab.

      when 'X'.
        exceptline-name = params_p-parameter.
        exceptline-value = excnt.
        data messg type ref to data.
        create data messg type string.
        assign messg->* to <temp>.
        <temp> = params_p-paramtext.
        exceptline-message = messg.
        insert exceptline into table exceptab.
        add 1 to excnt.

      when others.
        raise UNSUPPORTED_PARAM_TYPE.

    endcase.

  endloop.


* add in the catch all exception
  exceptline-name = 'OTHERS'.
  exceptline-value = excnt.
  insert exceptline into table exceptab.


* return
  params = t_params_p.

*********************************
******* Remaining from 2006 *****
******* end of build_params *****
*********************************
endmethod.


  METHOD CONVERT_TO_UTF8.

    DATA: LC_LENGTH TYPE I.
    DATA: LC_BUFFER TYPE XSTRING.
    DATA: LT_TEXTO  TYPE TABLE OF CHAR80.
    DATA: TAMANHO   TYPE I.

    TRY .
        LC_LENGTH = STRLEN( I_TEXTO ).
        CL_ABAP_CONV_OUT_CE=>CREATE( ENCODING = 'UTF-8' )->CONVERT( EXPORTING DATA = I_TEXTO N = LC_LENGTH IMPORTING BUFFER = LC_BUFFER ).

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            BUFFER        = LC_BUFFER
          IMPORTING
            OUTPUT_LENGTH = TAMANHO
          TABLES
            BINARY_TAB    = LT_TEXTO.

        CALL FUNCTION 'SCMS_BINARY_TO_STRING'
          EXPORTING
            INPUT_LENGTH = TAMANHO
          IMPORTING
            TEXT_BUFFER  = R_TEXTO
          TABLES
            BINARY_TAB   = LT_TEXTO
          EXCEPTIONS
            FAILED       = 1
            OTHERS       = 2.

        IF SY-SUBRC IS NOT INITIAL.
          R_TEXTO = I_TEXTO.
        ENDIF.

      CATCH CX_PARAMETER_INVALID_RANGE.
        R_TEXTO = I_TEXTO.
      CATCH CX_SY_CODEPAGE_CONVERTER_INIT.
        R_TEXTO = I_TEXTO.
      CATCH CX_SY_CONVERSION_CODEPAGE.
        R_TEXTO = I_TEXTO.
      CATCH CX_PARAMETER_INVALID_TYPE.
        R_TEXTO = I_TEXTO.
    ENDTRY.

    REPLACE ALL OCCURRENCES OF '\' IN R_TEXTO WITH '\\' .
    REPLACE ALL OCCURRENCES OF '"' IN R_TEXTO WITH '\"' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>CR_LF IN R_TEXTO WITH '\r\n' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>NEWLINE IN R_TEXTO WITH '\n' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB IN R_TEXTO WITH '\t' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>BACKSPACE IN R_TEXTO WITH '\b' .
    REPLACE ALL OCCURRENCES OF CL_ABAP_CHAR_UTILITIES=>FORM_FEED IN R_TEXTO WITH '\f' .

  ENDMETHOD.


  METHOD CONVERT_TXT_JSON_TO_STRING.

    R_TEXTO = I_TEXTO.
    REPLACE ALL OCCURRENCES OF '\u00e1' IN R_TEXTO WITH 'á' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e0' IN R_TEXTO WITH 'à' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e2' IN R_TEXTO WITH 'â' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e3' IN R_TEXTO WITH 'ã' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e4' IN R_TEXTO WITH 'ä' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c1' IN R_TEXTO WITH 'Á' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c0' IN R_TEXTO WITH 'À' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c2' IN R_TEXTO WITH 'Â' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c3' IN R_TEXTO WITH 'Ã' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c4' IN R_TEXTO WITH 'Ä' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e9' IN R_TEXTO WITH 'é' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e8' IN R_TEXTO WITH 'è' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ea' IN R_TEXTO WITH 'ê' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ea' IN R_TEXTO WITH 'ê' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c9' IN R_TEXTO WITH 'É' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c8' IN R_TEXTO WITH 'È' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ca' IN R_TEXTO WITH 'Ê' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00cb' IN R_TEXTO WITH 'Ë' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ed' IN R_TEXTO WITH 'í' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ec' IN R_TEXTO WITH 'ì' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ee' IN R_TEXTO WITH 'î' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ef' IN R_TEXTO WITH 'ï' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00cd' IN R_TEXTO WITH 'Í' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00cc' IN R_TEXTO WITH 'Ì' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00ce' IN R_TEXTO WITH 'Î' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00cf' IN R_TEXTO WITH 'Ï' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f3' IN R_TEXTO WITH 'ó' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f2' IN R_TEXTO WITH 'ò' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f4' IN R_TEXTO WITH 'ô' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f5' IN R_TEXTO WITH 'õ' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f6' IN R_TEXTO WITH 'ö' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d3' IN R_TEXTO WITH 'Ó' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d2' IN R_TEXTO WITH 'Ò' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d4' IN R_TEXTO WITH 'Ô' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d5' IN R_TEXTO WITH 'Õ' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d6' IN R_TEXTO WITH 'Ö' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00fa' IN R_TEXTO WITH 'ú' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f9' IN R_TEXTO WITH 'ù' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00fb' IN R_TEXTO WITH 'û' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00fc' IN R_TEXTO WITH 'ü' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00da' IN R_TEXTO WITH 'Ú' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d9' IN R_TEXTO WITH 'Ù' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00db' IN R_TEXTO WITH 'Û' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00e7' IN R_TEXTO WITH 'ç' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00c7' IN R_TEXTO WITH 'Ç' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00f1' IN R_TEXTO WITH 'ñ' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u00d1' IN R_TEXTO WITH 'Ñ' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u0026' IN R_TEXTO WITH '&' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '\u0027' IN R_TEXTO WITH '''' IGNORING CASE.

  ENDMETHOD.


METHOD DESERIALIZE_ID.
*/***********************************************************/*
*/ New method using the built-in transformation              /*
*/ included in releases 7.02 and 7.03/7.31 (Kernelpatch 116) /*
*/***********************************************************/*

  TYPE-POOLS: ABAP.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  DATA:
    RTAB       TYPE ABAP_TRANS_RESBIND_TAB,
    RLIN       TYPE ABAP_TRANS_RESBIND,
    OEXCP      TYPE REF TO CX_ROOT,
    ETEXT      TYPE STRING,
    JSON_XTEXT TYPE XSTRING.

  FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.

  IF JSON IS INITIAL. EXIT. ENDIF.  " exit method if there is nothing to parse

  " build rtab table for transformation id

  LOOP AT PARAMTAB ASSIGNING <PARM>.
    IF <PARM>-KIND EQ ABAP_FUNC_IMPORTING. "" va al revés, cuidado!!!
      CONTINUE.
    ENDIF.
    RLIN-NAME  = <PARM>-NAME.
    RLIN-VALUE = <PARM>-VALUE.
    APPEND RLIN TO RTAB.
  ENDLOOP.

  " Convert input JSON variable names to uppercase

  JSON_XTEXT = CL_ABAP_CODEPAGE=>CONVERT_TO( JSON ).
  DATA(READER) = CL_SXML_STRING_READER=>CREATE( JSON_XTEXT ).
  DATA(WRITER) = CAST IF_SXML_WRITER( CL_SXML_STRING_WRITER=>CREATE( TYPE = IF_SXML=>CO_XT_JSON ) ).
  DO.
    DATA(NODE) = READER->READ_NEXT_NODE( ).
    IF NODE IS INITIAL.
      EXIT.
    ENDIF.
    IF NODE->TYPE = IF_SXML_NODE=>CO_NT_ELEMENT_OPEN.
      DATA(ATTRIBUTES)  = CAST IF_SXML_OPEN_ELEMENT( NODE )->GET_ATTRIBUTES( ).
      LOOP AT ATTRIBUTES ASSIGNING FIELD-SYMBOL(<ATTRIBUTE>).
        IF <ATTRIBUTE>->QNAME-NAME = 'name'.
          <ATTRIBUTE>->SET_VALUE(
            TO_UPPER( <ATTRIBUTE>->GET_VALUE( ) ) ).
        ENDIF.
      ENDLOOP.
    ENDIF.
    WRITER->WRITE_NODE( NODE ).
  ENDDO.
  JSON_XTEXT = CAST CL_SXML_STRING_WRITER( WRITER )->GET_OUTPUT( ) .

  TRY.
      CALL TRANSFORMATION ID SOURCE XML JSON_XTEXT RESULT (RTAB).
    CATCH CX_ROOT INTO OEXCP.

      ETEXT = OEXCP->IF_MESSAGE~GET_TEXT( ).
      RAISE EXCEPTION TYPE ZCX_JSON
        EXPORTING
          MESSAGE = ETEXT.

  ENDTRY.

ENDMETHOD.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GERA_ERRO_GERAL_SYS.

    RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


METHOD IF_HTTP_EXTENSION~HANDLE_REQUEST.

  DATA: PATH_INFO          TYPE STRING,
        P_INFO_TAB         TYPE TABLE OF STRING,
        ACTION             TYPE STRING,
        "I_CONTENT_TYPE  TYPE STRING,
        I_CDATA            TYPE STRING,
        O_CDATA            TYPE STRING,
        O_NAME             TYPE STRING,
        O_AUX              TYPE STRING,
        LC_FORMAT          TYPE STRING,
        O_DATA             TYPE XSTRING,
        JSONP_CALLBACK     TYPE STRING,
        SHOW_IMPORT_PARAMS TYPE ABAP_BOOL VALUE ABAP_FALSE,
        T_PARAMS_P         TYPE STANDARD TABLE OF RFC_FINT_P,
        PARAMTAB           TYPE ABAP_FUNC_PARMBIND_TAB,
        EXCEPTAB           TYPE ABAP_FUNC_EXCPBIND_TAB,
        LOWERCASE          TYPE ABAP_BOOL VALUE ABAP_FALSE,
        CAMELCASE          TYPE ABAP_BOOL VALUE ABAP_FALSE,
        EXCEPTHEADER       TYPE STRING,
        ETEXT              TYPE STRING,
        OEXCP              TYPE REF TO CX_ROOT,
        ETEXT2             TYPE STRING,
        STR_ITEM           TYPE STRING,
        HTTP_CODE          TYPE I,
        HTTP_STATUS        TYPE STRING,
        FUNCNAME           TYPE RS38L_FNAM,
        FUNCNAME2          TYPE STRING.

  DATA: LC_ERRO   TYPE STRING.

  DEFINE HTTP_ERROR.

    lc_erro = &3.
    IF lc_erro IS NOT INITIAL.
      LC_ERRO = ME->CONVERT_TO_UTF8( I_TEXTO = LC_ERRO ).
    ENDIF.
    server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).
    http_code = &1.
    server->response->set_status( code = http_code  reason = &2 ).
    concatenate '{"ERROR_CODE":"' &1 '","ERROR_MESSAGE":"' lc_erro '"}' into etext.
    server->response->set_cdata( etext ).
    exit.

  END-OF-DEFINITION.

* Get Server Info:

  SERVER->GET_LOCATION( IMPORTING HOST = DATA(LC_HOST)  PORT = DATA(LC_PORT)  OUT_PROTOCOL = DATA(LC_PROTO) ).
  CONCATENATE LC_PROTO '://' LC_HOST ':' LC_PORT INTO ME->MY_URL.

  ME->MY_SERVICE = SERVER->REQUEST->GET_HEADER_FIELD( NAME = '~script_name' ).
  ACTION         = SERVER->REQUEST->GET_HEADER_FIELD( 'action' ).

  TRANSLATE ACTION TO UPPER CASE.

  IF ACTION EQ 'LOGIN'.
    TRY .
        ME->LOGIN( EXPORTING I_SERVER = SERVER IMPORTING E_JSON = O_CDATA ).
        SERVER->RESPONSE->SET_COMPRESSION( ).
        SERVER->RESPONSE->SET_CDATA( DATA = O_CDATA ).
      CATCH ZCX_FMCALL_APP_MOBILE INTO DATA(EX_APP_MOBILE).
        EX_APP_MOBILE->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        HTTP_ERROR '401' 'Server' MTEXT.
    ENDTRY.
    EXIT.
  ENDIF.

  IF ACTION EQ 'LOGOFF'.
    TRY .
        ME->LOGOFF( EXPORTING I_SERVER = SERVER ).
      CATCH ZCX_FMCALL_APP_MOBILE INTO EX_APP_MOBILE.
        EX_APP_MOBILE->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        HTTP_ERROR '401' 'Server' MTEXT.
    ENDTRY.
    EXIT.
  ENDIF.

*  TRY .
*      ME->VALIDA_TOKEN( EXPORTING I_SERVER = SERVER ).
*    CATCH ZCX_FMCALL_APP_MOBILE INTO EX_APP_MOBILE.
*      EX_APP_MOBILE->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      HTTP_ERROR '402' 'Server' MTEXT.
*  ENDTRY.

* Get function name from PATH_INFO
  PATH_INFO = SERVER->REQUEST->GET_HEADER_FIELD( NAME = '~path_info' ).
  SPLIT PATH_INFO AT '/' INTO TABLE P_INFO_TAB.
  READ TABLE P_INFO_TAB INDEX 2 INTO FUNCNAME.
  READ TABLE P_INFO_TAB INDEX 3 INTO FUNCNAME2.
  IF SY-SUBRC EQ 0.
    CONCATENATE '//' FUNCNAME '/' FUNCNAME2 INTO FUNCNAME.
    CONDENSE FUNCNAME.
  ENDIF.

  CHECK FUNCNAME IS NOT INITIAL.
  TRANSLATE FUNCNAME TO UPPER CASE.

  "Funçãoes Gerais
  TRY .
      CASE FUNCNAME.
        WHEN ZIF_FMCALL_APP_MOBILE~ST_GET_QTD_TAREFAS_USUARIO.
          ME->ZIF_FMCALL_APP_MOBILE~GET_QTD_TAREFAS_USUARIO( IMPORTING E_JSON = O_CDATA ).
      ENDCASE.
    CATCH ZCX_FMCALL_APP_MOBILE INTO EX_APP_MOBILE.
      EX_APP_MOBILE->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      HTTP_ERROR '405' 'Server' MTEXT.
  ENDTRY.

  DATA FILIAL TYPE WERKS_D.
  DATA STATUS TYPE CHAR1.
  DATA _VIEW TYPE STRING.
  DATA: FATURA TYPE CHAR10.
  DATA: CNPJ   TYPE STCD1.
  DATA: I_INFO TYPE ZDE_INTEGRACAO_HTTP_CONFIG.
  DATA: SERVER_BASE TYPE REF TO CL_HTTP_SERVER.

  SERVER_BASE = CAST #( SERVER ).

  FATURA = SERVER->REQUEST->GET_FORM_FIELD( 'fatura' ).
  CNPJ   = SERVER->REQUEST->GET_FORM_FIELD( 'cnpj' ).
  FILIAL = SERVER->REQUEST->GET_FORM_FIELD( 'filial' ).
  _VIEW = FUNCNAME.


  LC_FORMAT = SERVER->REQUEST->GET_FORM_FIELD( 'format' ).
  IF LC_FORMAT IS INITIAL.
    LC_FORMAT = SERVER->REQUEST->GET_FORM_FIELD( '$format' ).
  ENDIF.

  I_INFO-DS_BODY = SERVER->REQUEST->GET_CDATA( ).
  I_INFO-DS_CONTENT_TYPE = SERVER->REQUEST->GET_CONTENT_TYPE( ).
  I_INFO-DS_FORMATO = CONV #( LC_FORMAT ).
  I_INFO-DS_IP_ORIGEM = SERVER_BASE->C_CALLER_IP.
  I_INFO-DS_METODO = SERVER->REQUEST->GET_METHOD( ).
  I_INFO-DS_SERVER_PROTOCOLO = CONV #( SERVER_BASE->M_PROTOCOL_VERSION ).

  DATA(LC_URL) = ME->MY_URL && ME->MY_SERVICE && PATH_INFO.

  I_INFO-DS_URL = CONV #( LC_URL ).

  DATA(_ORDEM) = NEW ZCL_PM_ORDEM( ).
  DATA(_FATURA) = NEW ZCL_WEBSERVIC_PROTHEUS( ).

  TRY.
      IF _VIEW IS NOT INITIAL.

        CASE _VIEW.
          WHEN 'CRIARLOTECOMPENSACAO' .
            CALL FUNCTION 'ZMMF_ATUALIZA_ZFIT0156'
              EXPORTING
                JSON_RESULT   = I_INFO-DS_BODY
              IMPORTING
                JSON_RESPONSE = O_CDATA.

          WHEN 'CRIARLOTEATRIBUICAO' .
            CALL FUNCTION 'ZMMF_ATUALIZA_ZFIT0155'
              EXPORTING
                JSON_RESULT   = I_INFO-DS_BODY
              IMPORTING
                JSON_RESPONSE = O_CDATA.

        ENDCASE.


      ENDIF.

    CATCH ZCX_FMCALL_APP_MOBILE INTO EX_APP_MOBILE.
      EX_APP_MOBILE->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO MTEXT WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      HTTP_ERROR '405' 'Server' MTEXT.
  ENDTRY.

  SERVER->RESPONSE->SET_COMPRESSION( ).
  SERVER->RESPONSE->SET_CDATA( DATA = O_CDATA ).

ENDMETHOD.


method JSON2ABAP.
*/************************************************/*
*/ Input any abap data and this method tries to   /*
*/ fill it with the data in the JSON string.      /*
*/  Thanks to Juan Diaz for helping here!!        /*
*/************************************************/*

  type-pools: abap, js.

  data:
    js_script         type string,
    js_started        type i value 0,
    l_json_string     type string,
    js_property_table type   js_property_tab,
    js_property       type line of js_property_tab,
    l_property_path   type string,
    compname          type string,
    item_path         type string.

  data:
    l_type   type c,
    l_value  type string,
    linetype type string,
    l_comp   type line of ABAP_COMPDESCR_TAB.

  data:
    datadesc type ref to CL_ABAP_TYPEDESCR,
    drefdesc type ref to CL_ABAP_TYPEDESCR,
    linedesc type ref to CL_ABAP_TYPEDESCR,
    strudesc type ref to CL_ABAP_STRUCTDESCR,
    tabldesc type ref to CL_ABAP_TABLEDESCR.

  data newline type ref to data.

  field-symbols:
    <abap_data> type any,
    <itab>      type any table,
    <comp>      type any,
    <jsprop>    type line of js_property_tab,
    <abapcomp>  type abap_compdescr.


  define assign_scalar_value.
    "   &1   <abap_data>
    "   &2   js_property-value
    describe field &1 type l_type.
    l_value = &2.
* convert or adapt scalar values to ABAP.
    case l_type.
      when 'D'. " date type
        if l_value cs '-'.
          replace all occurrences of '-' in l_value with space.
          condense l_value no-gaps.
        endif.
      when 'T'. " time type
        if l_value cs ':'.
          replace all occurrences of ':' in l_value with space.
          condense l_value no-gaps.
        endif.
      when others.
        " may be other conversions or checks could be implemented here.
    endcase.
    &1 = l_value.
  end-of-definition.


  if js_object is not bound.

    if json_string is initial. exit. endif. " exit method if there is nothing to parse

    l_json_string = json_string.
    " js_object = cl_java_script=>create( STACKSIZE = 16384 ).
    js_object = cl_java_script=>create( STACKSIZE = 16384 HEAPSIZE = 960000 ).

***************************************************
*  Parse JSON using JavaScript                    *
***************************************************
    js_object->bind( exporting name_obj = 'abap_data' name_prop = 'json_string'    changing data = l_json_string ).
    js_object->bind( exporting name_obj = 'abap_data' name_prop = 'script_started' changing data = js_started ).

* We use the JavaScript engine included in ABAP to read the JSON string.
* We simply use the recommended way to eval a JSON string as specified
* in RFC 4627 (http://www.ietf.org/rfc/rfc4627.txt).
*
* Security considerations:
*
*   Generally there are security issues with scripting languages.  JSON
*   is a subset of JavaScript, but it is a safe subset that excludes
*   assignment and invocation.
*
*   A JSON text can be safely passed into JavaScript's eval() function
*   (which compiles and executes a string) if all the characters not
*   enclosed in strings are in the set of characters that form JSON
*   tokens.  This can be quickly determined in JavaScript with two
*   regular expressions and calls to the test and replace methods.
*
*      var my_JSON_object = !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
*             text.replace(/"(\\.|[^"\\])*"/g, ''))) &&
*         eval('(' + text + ')');

    concatenate

         'var json_obj; '
         'var json_text; '

         'function start() { '
         '  if(abap_data.script_started) { return; } '
         '  json_text = abap_data.json_string;'
         '  json_obj = !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test( '
         '      json_text.replace(/"(\\.|[^"\\])*"/g, ''''))) && '
         '    eval(''('' + json_text + '')''); '
         '  abap_data.script_started = 1; '
         '} '

         'if(!abap_data.script_started) start(); '


       into js_script respecting blanks separated by xnl.

    js_object->compile( script_name = 'json_parser'     script = js_script ).
    js_object->execute( script_name = 'json_parser' ).

    if js_object->last_error_message is not initial.
      RAISE EXCEPTION type ZCX_JSON
        EXPORTING
          message = js_object->last_error_message.
    endif.

  endif.
** End of JS processing.

**
  if var_name is not initial.
    concatenate property_path var_name into l_property_path separated by '.'.
  else.
    l_property_path = property_path.
  endif.
**
**
  js_property_table = js_object->get_properties_scope_global( property_path = l_property_path ).
  property_table = js_property_table.

* Exit if abap_data is not supplied, normally when called
* from json_deserialize to get top level properties
  if abap_data is not supplied.
    exit.
  endif. "***

*
* Get ABAP data type, dereference if necessary and start
  datadesc = cl_abap_typedescr=>DESCRIBE_BY_DATA( abap_data ).
  if datadesc->kind eq cl_abap_typedescr=>kind_ref.
    assign abap_data->* to <abap_data>.
  else.
    assign abap_data to <abap_data>.
  endif.
  datadesc = cl_abap_typedescr=>DESCRIBE_BY_DATA( <abap_data> ).


  case datadesc->kind.

    when cl_abap_typedescr=>kind_elem.
* Scalar: process ABAP elements. Assume no type conversions for the moment.
      if var_name is initial.
        RAISE EXCEPTION type ZCX_JSON
          EXPORTING
            message = 'VAR_NAME is required for scalar values.'.
      endif.
      js_property_table = js_object->get_properties_scope_global( property_path = property_path ).
      read table js_property_table with key name = var_name into js_property.
      if sy-subrc eq 0.
        assign_scalar_value <abap_data> js_property-value.
      endif.


    when cl_abap_typedescr=>kind_struct.
* Process ABAP structures
      strudesc ?= datadesc.
      loop at js_property_table assigning <jsprop>.
        compname = <jsprop>-name.
        translate compname to upper case.
        read table strudesc->COMPONENTS with key name = compname into l_comp.
        if sy-subrc eq 0.
          assign component l_comp-name of structure <abap_data> to <comp>.
          case l_comp-type_kind.
            when    cl_abap_typedescr=>TYPEKIND_STRUCT1  " 'v'
                 or cl_abap_typedescr=>TYPEKIND_STRUCT2  " 'u'
                 or cl_abap_typedescr=>TYPEKIND_TABLE.   " 'h' (may need a different treatment one day)
              concatenate l_property_path <jsprop>-name into item_path separated by '.'.
*> Recursive call here
              json2abap( exporting property_path = item_path changing abap_data = <comp> js_object = js_object ).

            when others.
* Process scalars in structures (same as the kind_elem above)
              assign_scalar_value <comp> <jsprop>-value.

          endcase.
        endif.
      endloop.

    when cl_abap_typedescr=>kind_table.
* Process ABAP tables
      if js_property_table is not initial.
        tabldesc ?= datadesc.
        linedesc = tabldesc->get_table_line_type( ).
        linetype = linedesc->get_relative_name( ).
        assign <abap_data> to <itab>.
        loop at js_property_table into js_property where name NE 'length'. " the JS object length
          create data newline type (linetype).
          assign newline->* to <comp>.
          case js_property-kind.
            when 'O'.
              concatenate l_property_path js_property-name into item_path separated by '.'.
              condense item_path.
*> Recursive call here
              json2abap( exporting property_path = item_path changing abap_data = newline js_object = js_object ).
            when others. " Assume scalars, 'S', 'I', or other JS types
              " Process scalars in plain table components(same as the kind_elem above)
              assign_scalar_value <comp> js_property-value.
          endcase.
          insert <comp> into table <itab>.
          free newline.
        endloop.
      endif.

    when others. " kind_class, kind_intf
      " forget it.

  endcase.


endmethod.


method JSON_DESERIALIZE.

  type-pools: ABAP, JS.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  data paramname   type string.
  data js_obj      type ref to cl_java_script.
  data js_prop_tab type js_property_tab.

  field-symbols <js_prop> type line of js_property_tab.
  field-symbols <parm>    type abap_func_parmbind.

  if json is initial. exit. endif.

  json2abap( exporting json_string = json  importing property_table = js_prop_tab  changing js_object = js_obj ).

  loop at js_prop_tab assigning <js_prop>.
    paramname = <js_prop>-name.
    translate paramname to upper case.
    read table paramtab with key name = paramname assigning <parm>.
    if sy-subrc eq 0.
      if <parm>-kind ne abap_func_importing. "" va al revés, cuidado!!!
        json2abap( exporting var_name = <js_prop>-name  changing abap_data = <parm>-value js_object = js_obj ).
      endif.
    endif.
  endloop.

endmethod.


  METHOD LOGIN.

    I_SERVER->GET_XSRF_TOKEN( IMPORTING TOKEN = E_OUT_LOGIN-TOKEN
      EXCEPTIONS
        INTERNAL_ERROR           = 1
        CALLED_BY_PUBLIC_SERVICE = 2
        OTHERS                   = 3 ).

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    E_OUT_LOGIN-SESSION_ID = I_SERVER->SESSION_ID.

    SELECT SINGLE * INTO @DATA(WA_ZHCMT0007)
      FROM ZHCMT0007
     WHERE BNAME EQ @SY-UNAME.

    IF SY-SUBRC IS INITIAL.
      E_OUT_LOGIN-CPF          = WA_ZHCMT0007-CPF_NR.
      E_OUT_LOGIN-DEPARTAMENTO = WA_ZHCMT0007-DEPARTAMENTO.
      E_OUT_LOGIN-FUNCAO       = WA_ZHCMT0007-FUNCAO.
      E_OUT_LOGIN-NOME         = WA_ZHCMT0007-CNAME.
    ENDIF.

    E_JSON = '{' &&
                 '"e_sucesso" : "SIM",' &&
                 '"e_token" : "' && E_OUT_LOGIN-TOKEN && '",' &&
                 '"e_nome" : "' && E_OUT_LOGIN-NOME && '",' &&
                 '"e_departamento" : "' && E_OUT_LOGIN-DEPARTAMENTO && '",' &&
                 '"e_funcao" : "' && E_OUT_LOGIN-FUNCAO && '",' &&
                 '"e_cpf" : "' && E_OUT_LOGIN-CPF && '"' &&
              '}'.

    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'x-csrf-token'  VALUE = E_OUT_LOGIN-TOKEN ).
    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Session_Id'    VALUE = E_OUT_LOGIN-SESSION_ID ).
    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).

  ENDMETHOD.


  METHOD LOGOFF.

    I_SERVER->LOGOFF(
      EXCEPTIONS
        LOGOFF_NOT_POSSIBLE       = 1
        OTHERS                    = 2
    ).

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

  ENDMETHOD.


method NOTES.

data location type string.

concatenate me->my_url me->my_service '/RFC_SYSTEM_INFO' into location.

concatenate

'<html><head><title>JSON (NEW) handler notes</title></head><body>'

'<h4>About this service...</h4>'
'This is the ABAP implementation of a conversion program that'
' tranforms ABAP data into a <a href="http://www.json.org">JSON</a> representation.'
'<p>'
'It provides a user interface in the form of a ICF service that '
'allows web invocation of ABAP function modules. It doesn''t matter if they are RFC enabled or not.'
'<p>In this system this service has '
'been assigned to ICF service <a href="' me->my_url me->my_service '">' me->my_service '</a>.'
'<p>'
'In order to invoke a function module, just put its name in the PATH_INFO '
'of the service URL, as is shown in the following examples.'

'<p>Try the following link to do the default call in JSON format:<pre><a href="' location '?format=json">'
location
'?format=json</a></pre>'

'<p>A simple syntax allows to get the output in different formats.<p>'

'The following gets the output in <a href="http://yaml.org">YAML</a> format:'
'<pre><a href="' location '?format=yaml">'
location
'?format=yaml</a></pre>'
''
'<p>And this will get the output in a basic XML representation: <pre><a href="' location '?format=xml">'
location
'?format=xml</a></pre>'

'<p>And, just for fun, getting it into Perl format could be handy: <pre><a href="' location '?format=perl">'
location
'?format=perl</a></pre>'

'<p>Finnally, you can add a callback to get the JSON response enclosed in a javascript function call,'
' in order to allow a <a href="http://en.wikipedia.org/wiki/JSONP">JSONP</a> style response: '
'<pre><a href="'
location '?format=json&callback=callMe">'
location '?format=json&callback=callMe</a></pre>'

'<hr><h4>WARNING</h4>This is work in progress and may not be suitable for use in productive '
'systems. The interface is somewhat unstable. Please feel free to test it and report  '
'any bug and improvement you may find.'
'<p>Use it at your own risk!'
'<p>For more information: <a href="https://cw.sdn.sap.com/cw/groups/json-adapter-for-abap-function-modules">'
'https://cw.sdn.sap.com/cw/groups/json-adapter-for-abap-function-modules</a>'
'<p>'
'If you have any questions, please contact me at <a href="mailto:cesar.martin@sap.com">'
'cesar.martin@sap.com</a>'
'<p>'


'<hr></body></html>'


into text RESPECTING BLANKS.


endmethod.


method SERIALIZE_ID.
*/***********************************************************/*
*/ New method using the built-in transformation              /*
*/ included in releases 7.02 and 7.03/7.31 (Kernelpatch 116) /*
*/ Generates both JSON and XML formats!!
*/***********************************************************/*
*/
** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  type-pools: ABAP.

  data:
    stab type ABAP_TRANS_SRCBIND_TAB,
    slin type ABAP_TRANS_SRCBIND,
    oexcp type ref to cx_root,
    etext type string,
    adata type ref to data,
    json_writer type ref to cl_sxml_string_writer.

  field-symbols <parm> type abap_func_parmbind.
*  field-symbols <excep> type abap_func_excpbind.


  loop at paramtab assigning <parm>.
    if show_impp ne 'X'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      continue.
    endif.
    slin-name  = <parm>-name.
    slin-value = <parm>-value.
    append slin to stab. clear slin.
  endloop.

  if exceptab is not initial.
    slin-name  = 'EXCEPTION'.
    get reference of exceptab into adata.
    slin-value = adata.
    append slin to stab. clear slin.
  endif.


  json_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

  try.

      case format.

        when 'XML'.

          call transformation id options data_refs = 'embedded'
                                         initial_components = 'include'
                                 source (stab)
                                 result xml o_string.


        when others.

          call transformation id options data_refs = 'embedded'
                                         initial_components = 'include'
                                 source (stab)
                                 result xml json_writer.

          o_string = cl_abap_codepage=>convert_from( json_writer->get_output( ) ).
*  json_string = json_writer->get_output( ).

          if jsonp is not initial.
            concatenate jsonp '(' o_string ');' into o_string.
          endif.

      endcase.


    catch cx_root into oexcp.

      etext = oexcp->if_message~get_text( ).
      RAISE EXCEPTION type ZCX_JSON
        EXPORTING
          message = etext.

  endtry.


endmethod.


method SERIALIZE_JSON.
* ABAP based JSON serializer for function modules (January 2013).
  type-pools: ABAP.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  data json_fragments type table of string.
  data rec_json_string type string.
  data paramname type string.
  data l_lines type i.
  data l_index type i.
  data upcase type xfeld value 'X'.
  field-symbols <parm> type abap_func_parmbind.
  field-symbols <excep> type abap_func_excpbind.

  if jsonp is not initial.
    append jsonp to json_fragments.
    append '(' to json_fragments.
  endif.

  rec_json_string = '{'.
  append rec_json_string to json_fragments.
  clear rec_json_string.

  clear l_index.
  l_lines = lines( paramtab ).

  loop at paramtab assigning <parm>.
    if show_impp ne 'X'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      subtract 1 from l_lines.
      continue.
    endif.
    add 1 to l_index.
    paramname = <parm>-name.
    if lowercase eq abap_true.
      translate paramname to lower case.
      " paramname = to_lower( paramname ).
      upcase = space.
    endif.
    if camelcase eq abap_true.
      paramname = to_mixed( val = paramname  case = 'a').
    endif.
    rec_json_string = abap2json( abap_data = <parm>-value  name = paramname  upcase = upcase camelcase = camelcase ).
    append rec_json_string to json_fragments.
    clear rec_json_string.
    if l_index < l_lines.
      append ',' to json_fragments .
    endif .
  endloop.

  if exceptab is not initial.
    if l_lines gt 0.
      append ',' to json_fragments.
    endif.
    rec_json_string = abap2json( abap_data = exceptab upcase = 'X' name = 'EXCEPTION').
    append rec_json_string to json_fragments.
    clear rec_json_string.
  endif.

  rec_json_string = '}'.
  append rec_json_string to json_fragments.
  clear rec_json_string.

  if jsonp is not initial.
    append ');' to json_fragments.
  endif.

  concatenate lines of json_fragments into o_string.

endmethod.


METHOD SERIALIZE_PDF.

  TYPE-POOLS: ABAP.

** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  DATA REC_PDF_XSTRING TYPE XSTRING.
  DATA PARAMNAME TYPE STRING.
  DATA L_LINES TYPE I.
  DATA L_INDEX TYPE I.
  DATA UPCASE TYPE XFELD VALUE 'X'.

  FIELD-SYMBOLS: <ABAP_DATA> TYPE ANY.

  FIELD-SYMBOLS <PARM> TYPE ABAP_FUNC_PARMBIND.
  FIELD-SYMBOLS <EXCEP> TYPE ABAP_FUNC_EXCPBIND.

  DATA: L_TYPE  TYPE C,
        L_COMPS TYPE I.

  CLEAR: REC_PDF_XSTRING.

  CLEAR L_INDEX.
  L_LINES = LINES( PARAMTAB ).

  LOOP AT PARAMTAB ASSIGNING <PARM>.

    IF SHOW_IMPP NE 'X' AND <PARM>-KIND EQ ABAP_FUNC_EXPORTING. "" va al revés, cuidado!!!
      SUBTRACT 1 FROM L_LINES.
      CONTINUE.
    ENDIF.

    ADD 1 TO L_INDEX.
    PARAMNAME = <PARM>-NAME.

    IF LOWERCASE EQ ABAP_TRUE.
      TRANSLATE PARAMNAME TO LOWER CASE.
      UPCASE = SPACE.
    ENDIF.

    IF CAMELCASE EQ ABAP_TRUE.
      PARAMNAME = TO_MIXED( VAL = PARAMNAME  CASE = 'a').
    ENDIF.

    DESCRIBE FIELD <PARM>-VALUE TYPE L_TYPE COMPONENTS L_COMPS.

    CASE L_TYPE.
      WHEN CL_ABAP_TYPEDESCR=>TYPEKIND_XSTRING OR CL_ABAP_TYPEDESCR=>TYPEKIND_DREF.

        CASE <PARM>-NAME.
          WHEN 'E_NAME'.
            ASSIGN <PARM>-VALUE->* TO <ABAP_DATA>.
            O_NAME = <ABAP_DATA>.
          WHEN OTHERS.
            REC_PDF_XSTRING = ABAP2PDF( ABAP_DATA = <PARM>-VALUE  NAME = PARAMNAME ).
            O_XSTRING = REC_PDF_XSTRING.
        ENDCASE.

      WHEN CL_ABAP_TYPEDESCR=>TYPEKIND_CHAR.
        ASSIGN <PARM>-VALUE TO <ABAP_DATA>.
        O_NAME = <ABAP_DATA>.
    ENDCASE.

  ENDLOOP.


ENDMETHOD.


method SERIALIZE_PERL.
* Just for fun, generate data in Perl Data::Dumper format.

  type-pools: ABAP.

**Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  data perl_fragments type table of string.
  data rec_perl_string type string.
  data paramname type string.
  data l_lines type i.
  data l_index type i.
  data upcase type xfeld value 'X'.
  data perl_var type string.
  field-symbols <parm> type abap_func_parmbind.
  field-symbols <excep> type abap_func_excpbind.

  if jsonp is not initial.
    perl_var = jsonp.
  else.
    perl_var = funcname.
  endif.
  concatenate '$' perl_var ' = {' into rec_perl_string.
  append rec_perl_string to perl_fragments.
  clear rec_perl_string.

  clear l_index.
  l_lines = lines( paramtab ).

  loop at paramtab assigning <parm>.
    if show_impp ne 'X'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      subtract 1 from l_lines.
      continue.
    endif.
    add 1 to l_index.
    paramname = <parm>-name.
    if lowercase eq abap_true.
      translate paramname to lower case.
      upcase = space.
    endif.
    rec_perl_string = abap2perl( abap_data = <parm>-value  name = paramname  upcase = upcase ).
    append rec_perl_string to perl_fragments.
    clear rec_perl_string.
    if l_index < l_lines.
      append ',' to perl_fragments .
    endif .
  endloop.

  if exceptab is not initial.
    if l_lines gt 0.
      append ',' to perl_fragments.
    endif.
    rec_perl_string = abap2perl( abap_data = exceptab upcase = 'X' name = 'EXCEPTION').
    append rec_perl_string to perl_fragments.
    clear rec_perl_string.
  endif.

  rec_perl_string = '};'.
  append rec_perl_string to perl_fragments.
  clear rec_perl_string.

  concatenate lines of perl_fragments into perl_string.

endmethod.


method SERIALIZE_XML.
* serialize function data into simple xml
*/ look at method serialize_id for a new way of doing xml.

  type-pools: abap.

** remember function parameter types
***constants:
***  abap_func_exporting type abap_func_parmbind-kind value 10,
***  abap_func_importing type abap_func_parmbind-kind value 20,
***  abap_func_tables    type abap_func_parmbind-kind value 30,
***  abap_func_changing  type abap_func_parmbind-kind value 40.

  data rec_xml_string type string.
  data xml_fragments type table of string.
  data l_funcname type string.
  data paramname type string.
  field-symbols <parm> type abap_func_parmbind.
  field-symbols <excep> type abap_func_excpbind.
  data upcase type xfeld value 'x'.

  constants:
     xml_head type string value '<?xml version="1.0" encoding="utf-8"?>'.

  append xml_head to xml_fragments.

  l_funcname = funcname.
  if lowercase eq abap_true.
    translate l_funcname to lower case.
    upcase = space.
  endif.

  concatenate '<' l_funcname '>' into rec_xml_string.
  append rec_xml_string to xml_fragments.

  loop at paramtab assigning <parm>.
    if show_impp ne 'x'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      continue.
    endif.
    paramname = <parm>-name.
    if lowercase eq abap_true.
      translate paramname to lower case.
    endif.
    rec_xml_string = abap2xml( name = paramname abap_data = <parm>-value upcase = upcase ).
    append rec_xml_string to xml_fragments.
  endloop.

  if exceptab is not initial.
    rec_xml_string = abap2xml( name = 'exception' abap_data = exceptab  upcase = upcase ).
    append rec_xml_string to xml_fragments.
  endif.

  concatenate '</' l_funcname '>' into rec_xml_string.
  append rec_xml_string to xml_fragments.

  concatenate lines of xml_fragments into o_string.

endmethod.


method SERIALIZE_YAML.
* Now, go and represent function data in YAML (http://yaml.org)

  type-pools: ABAP.
** Remember function parameter types
**constants:
**  abap_func_exporting type abap_func_parmbind-kind value 10,
**  abap_func_importing type abap_func_parmbind-kind value 20,
**  abap_func_tables    type abap_func_parmbind-kind value 30,
**  abap_func_changing  type abap_func_parmbind-kind value 40.

  data yaml_fragments type table of string.
  data rec_yaml_string type string.
  data rec_yaml_table type table of string.
  data paramname type string.
  field-symbols <parm> type abap_func_parmbind.
  field-symbols <excep> type abap_func_excpbind.
  data upcase type xfeld value 'X'.
  data yaml_head type string value '--- #YAML:1.0'.

  concatenate yaml_head xnl into rec_yaml_string.
  append rec_yaml_string to yaml_fragments.
  clear rec_yaml_string.

  loop at paramtab assigning <parm>.
    if show_impp ne 'X'
          and <parm>-kind eq abap_func_exporting. "" va al revés, cuidado!!!
      continue.
    endif.
    paramname = <parm>-name.
    if lowercase eq abap_true.
       translate paramname to lower case.
       upcase = space.
    endif.
    rec_yaml_string = abap2yaml( abap_data = <parm>-value  name = paramname upcase = upcase ).
    append rec_yaml_string to yaml_fragments.
    clear rec_yaml_string.
  endloop.

  if exceptab is not initial.
    rec_yaml_string = abap2yaml( abap_data = exceptab name = 'EXCEPTION' upcase = 'X' ).
    append rec_yaml_string to yaml_fragments.
    clear rec_yaml_string.
  endif.

*  append xnl to yaml_fragments.

  concatenate lines of yaml_fragments into yaml_string.

*  if jsonp is not initial.
*     concatenate jsonp '(' yaml_string ');' into yaml_string.
*  endif.

endmethod.


  METHOD VALIDA_TOKEN.

    E_OUT_LOGIN-TOKEN = I_SERVER->REQUEST->GET_HEADER_FIELD( 'token' ).

    I_SERVER->VALIDATE_XSRF_TOKEN(
      EXPORTING
        TOKEN                    = E_OUT_LOGIN-TOKEN
      IMPORTING
        SUCCESSFUL               = DATA(LC_SUCCESSFUL)
      EXCEPTIONS
        TOKEN_NOT_FOUND          = 1
        COOKIE_NOT_FOUND         = 2
        INTERNAL_ERROR           = 3
        CALLED_BY_PUBLIC_SERVICE = 4
        OTHERS                   = 5 ).

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(MTEXT) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDIF.

    IF LC_SUCCESSFUL EQ ABAP_FALSE.
      RAISE EXCEPTION TYPE ZCX_FMCALL_APP_MOBILE
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_TOKEN-MSGID MSGNO = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_TOKEN-MSGNO )
          MSGID  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_TOKEN-MSGID
          MSGNO  = ZCX_FMCALL_APP_MOBILE=>ZCX_ERRO_TOKEN-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    E_OUT_LOGIN-SESSION_ID = I_SERVER->SESSION_ID.

    SELECT SINGLE * INTO @DATA(WA_ZHCMT0007)
      FROM ZHCMT0007
     WHERE BNAME EQ @SY-UNAME.

    IF SY-SUBRC IS INITIAL.
      E_OUT_LOGIN-CPF          = WA_ZHCMT0007-CPF_NR.
      E_OUT_LOGIN-DEPARTAMENTO = WA_ZHCMT0007-DEPARTAMENTO.
      E_OUT_LOGIN-FUNCAO       = WA_ZHCMT0007-FUNCAO.
      E_OUT_LOGIN-NOME         = WA_ZHCMT0007-CNAME.
    ENDIF.

    E_JSON = '{' &&
                 '"e_sucesso" : "SIM",' &&
                 '"e_token" : "' && E_OUT_LOGIN-TOKEN && '",' &&
                 '"e_nome" : "' && E_OUT_LOGIN-NOME && '",' &&
                 '"e_departamento" : "' && E_OUT_LOGIN-DEPARTAMENTO && '",' &&
                 '"e_funcao" : "' && E_OUT_LOGIN-FUNCAO && '",' &&
                 '"e_cpf" : "' && E_OUT_LOGIN-CPF && '"' &&
              '}'.

    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'x-csrf-token' VALUE = E_OUT_LOGIN-TOKEN ).
    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Session_Id'   VALUE = I_SERVER->SESSION_ID ).
    I_SERVER->RESPONSE->SET_HEADER_FIELD( NAME = 'Content-Type' VALUE = 'application/json; charset=UTF-8' ).

  ENDMETHOD.


  METHOD ZIF_FMCALL_APP_MOBILE~GET_QTD_TAREFAS_MM.

    DATA: ITEMS TYPE TABLE OF SWWWLHEAD.

    DATA(LC_USER) = SY-UNAME.

    CALL FUNCTION 'RH_INBOX_VIEW_CREATE'
      EXPORTING
*       SEARCH_DATE       = SY-DATUM
*       READ_OBJECT_TEXT  =
*       NO_WI_SELECTION   =
*       NO_HEADER_SELECTION       =
        INBOX_USER        = LC_USER
*       IV_DO_COMMIT      = 'X'
      TABLES
*       INBOX_VIEW        =
        WI_HEAD           = ITEMS
*       WI_STATUS         =
*       TASK_FILTER       =
      EXCEPTIONS
        NO_ACTIVE_PLVAR   = 1
        NO_TASKS_FOUND    = 2
        USER_NOT_DEFINED  = 3
        NO_WORKITEM_FOUND = 4
        OTHERS            = 5.

    IF SY-SUBRC IS NOT INITIAL.
      ZCL_FMCALL_APP_MOBILE=>GERA_ERRO_GERAL_SYS( ).
    ENDIF.

    "Pedido de compra
    DELETE ITEMS WHERE WI_RH_TASK NE 'TS20000166'.
    "Reservas
    DELETE ITEMS WHERE WI_RH_TASK NE 'TS99900017'.
    "Requisições
    DELETE ITEMS WHERE WI_RH_TASK NE 'TS00007986'.

  ENDMETHOD.
ENDCLASS.
