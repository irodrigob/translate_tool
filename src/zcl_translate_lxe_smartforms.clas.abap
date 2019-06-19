CLASS zcl_translate_lxe_smartforms DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_lxe
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_LXE_SMARTFORMS
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_lxe_type
        REDEFINITION .
    METHODS has_proposed_text
        REDEFINITION .
    METHODS load_text
        REDEFINITION .
    METHODS set_check_params
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_LXE_SMARTFORMS
*"* do not include other source files here!!!

    DATA mv_olang_r3 TYPE spras .
    DATA mv_tlang_r3 TYPE spras .
    DATA mv_objtype TYPE trobjtype VALUE 'SSF' ##NO_TEXT.
    DATA ms_lwrkobj TYPE lwrkobj .
  PRIVATE SECTION.
*"* private components of class ZCL_TRANSLATE_LXE_SMARTFORMS
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_lxe_smartforms IMPLEMENTATION.


  METHOD get_lxe_type.
    APPEND 'SSFO' TO rt_objects.
  ENDMETHOD.


  METHOD has_proposed_text.
    rv_has = abap_false.
  ENDMETHOD.


  METHOD load_text.
    FIELD-SYMBOLS <ls_tline_source> TYPE LINE OF tline_tab.
    FIELD-SYMBOLS <ls_tline_target> TYPE LINE OF tline_tab.
    DATA lt_tline_source TYPE tline_tab.
    DATA lt_tline_target TYPE tline_tab.

    CALL FUNCTION 'SSFTR_GET_TEXT'
      EXPORTING
        sourcelang       = mv_olang_r3
        targetlang       = mv_tlang_r3
      TABLES
        sourcetext       = lt_tline_source
        targettext       = lt_tline_target
      CHANGING
        tlwrkobj         = ms_lwrkobj
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.




  ENDMETHOD.


  METHOD set_check_params.
    DATA ls_colob TYPE lxe_colob.
    DATA ls_prefs TYPE lxe_upsets.
    DATA ld_lxe_objname TYPE lxeobjname.

* Guardo los parametros pasados a variables globales.
    d_object = iv_object.
    d_obj_name = iv_obj_name.
    d_olang = iv_olang.
    d_tlang = iv_tlang.

* Para recuperar el texto hay que pasar el idioma en R/3, por eso se realiza
* la conversion de los dos idiomas.
    CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
      EXPORTING
        language           = d_olang
      IMPORTING
        o_r3_lang          = mv_olang_r3
      EXCEPTIONS
        language_not_in_cp = 1
        unknown            = 2
        OTHERS             = 3.

    CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
      EXPORTING
        language           = d_tlang
      IMPORTING
        o_r3_lang          = mv_tlang_r3
      EXCEPTIONS
        language_not_in_cp = 1
        unknown            = 2
        OTHERS             = 3.

* Llamo a la funcion que me devolverá las preferencia de usuario.
* Acceso a esta funcion para saber el área de traduccion. Por regla general
* es '999999'.
    CALL FUNCTION 'LXE_SE63_TRANSLATION_PREFS_GET'
      EXPORTING
        uname                   = sy-uname
      CHANGING
        preferences_worklist    = ls_prefs
      EXCEPTIONS
        no_default_preset_found = 1
        OTHERS                  = 2.

    IF sy-subrc = 0.
      ls_colob-custmnr =  ls_prefs-custmnr.
    ELSE.
      ls_colob-custmnr = '999999'.
    ENDIF.
    ld_lxe_objname = iv_obj_name.
    CALL FUNCTION 'LXE_OBJ_CONVERT_OL_WLB'
      EXPORTING
        in_custmnr = ls_colob-custmnr
        in_objtype = ls_colob-objtype
        in_objname = ld_lxe_objname
      IMPORTING
        objname    = ls_colob-objname.
    ls_colob-objtype = mv_objtype.
    APPEND ls_colob TO it_colob.


* Relleno la estructura del objeto a traducir que se usará
* tanto para leer como para grabar.
    ms_lwrkobj-targetlang = mv_tlang_r3.
    ms_lwrkobj-objtype = mv_objtype.
    ms_lwrkobj-objname = d_obj_name.

  ENDMETHOD.
ENDCLASS.
