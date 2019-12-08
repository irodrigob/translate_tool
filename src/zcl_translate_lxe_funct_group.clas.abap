CLASS zcl_translate_lxe_funct_group DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_lxe
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_LXE_FUNCT_GROUP
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_lxe_type
        REDEFINITION .
    METHODS has_proposed_text
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_LXE_FUNCT_GROUP
*"* do not include other source files here!!!

    METHODS get_object_text
        REDEFINITION .
  PRIVATE SECTION.
*"* private components of class ZCL_TRANSLATE_LXE_FUNCT_GROUP
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_lxe_funct_group IMPLEMENTATION.


  METHOD get_lxe_type.
* Function group
    APPEND 'FUGR' TO rt_objects.
  ENDMETHOD.


  METHOD get_object_text.
* Tipo de objeto de texto del objeto pasado.
    rv_obj_text = 'REPT'.
  ENDMETHOD.


  METHOD has_proposed_text.
    rv_has = abap_true.
  ENDMETHOD.
ENDCLASS.
