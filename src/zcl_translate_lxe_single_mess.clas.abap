CLASS zcl_translate_lxe_single_mess DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_lxe
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_LXE_SINGLE_MESS
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_lxe_type
        REDEFINITION .
    METHODS has_proposed_text
        REDEFINITION .
    METHODS set_check_params
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_LXE_SINGLE_MESS
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF ty_message_key,
        message_id TYPE symsgid,
        message_nr TYPE t100-msgnr,
      END OF ty_message_key .

    METHODS get_object_text
        REDEFINITION .
  PRIVATE SECTION.
*"* private components of class ZZC_CA02707
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_lxe_single_mess IMPLEMENTATION.


  METHOD get_lxe_type.
    APPEND 'MESS' TO rt_objects.
  ENDMETHOD.


  METHOD get_object_text.
    r_obj_text = 'MESS'.
  ENDMETHOD.


  METHOD has_proposed_text.
    rv_has = abap_true.
  ENDMETHOD.


  METHOD set_check_params.
    DATA ld_leng TYPE i.
    DATA ld_obj_name TYPE sobj_name.
    DATA ls_message_key TYPE ty_message_key.

* El nombre del mensaje requiere de un ajuste para poderlo traducir.
* En el nombre que se pasa los tres ultimos digitos es el numero de mensaje. Tengo que separarlos en dos
* para pasarlo a la estructura.
* Calculo la longitud total quitandole el espacio del numero de mensaje
    ld_leng = strlen( iv_obj_name ) - 3.

* Hago un offset para recuperar los valores.
    ls_message_key-message_id = iv_obj_name(ld_leng).
    ls_message_key-message_nr = iv_obj_name+ld_leng(3).

* Finalmente muevo la estructura a una variable.
    ld_obj_name = ls_message_key.

* Llamo al método de la clase padre para que se encargue del resto.
    CALL METHOD super->set_check_params
      EXPORTING
        iv_object        = iv_object
        iv_obj_name      = ld_obj_name
        iv_olang         = iv_olang
        iv_tlang         = iv_tlang
      EXCEPTIONS
        object_not_valid = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RAISE object_not_valid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
