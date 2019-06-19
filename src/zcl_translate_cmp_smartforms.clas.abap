CLASS zcl_translate_cmp_smartforms DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_cmp
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_CMP_SMARTFORMS
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_components
        REDEFINITION .
    METHODS get_objects_type
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_CMP_SMARTFORMS
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_TRANSLATE_CMP_SMARTFORMS
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_cmp_smartforms IMPLEMENTATION.


  METHOD get_components.
    DATA ls_components TYPE LINE OF tt_components.

    CLEAR et_components.

    ls_components-object = mv_object.
    ls_components-obj_name = mv_obj_name.
    APPEND ls_components TO et_components.

  ENDMETHOD.


  METHOD get_objects_type.
*  APPEND 'SSFO' TO r_objects.
  ENDMETHOD.
ENDCLASS.
