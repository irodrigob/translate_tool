CLASS zcl_translate_lxe_classes DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_lxe
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_LXE_CLASSES
*"* do not include other source files here!!!
public section.

  methods GET_LXE_TYPE
    redefinition .
  methods HAS_PROPOSED_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_TRANSLATE_LXE_CLASSES
*"* do not include other source files here!!!

  methods GET_OBJECT_TEXT
    redefinition .
private section.
*"* private components of class ZZC_CA02705
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_TRANSLATE_LXE_CLASSES IMPLEMENTATION.


method GET_LXE_TYPE.
* Clases
  APPEND 'CLAS' TO rt_objects.
endmethod.


method GET_OBJECT_TEXT.
  DATA ld_objtyp   TYPE e071-object.
  DATA ld_pgmid    TYPE e071-pgmid.
  DATA ld_obj_name TYPE e071-obj_name.

  PERFORM determine_lock_key IN PROGRAM SAPLSEUQ USING    space
                                        'TEXT'
                                        i_obj_name
                               CHANGING ld_pgmid
                                        ld_objtyp
                                        ld_obj_name.

* Tipo de objeto de texto del objeto pasado.
  r_obj_text = ld_objtyp.

endmethod.


method HAS_PROPOSED_TEXT.
  rv_has = abap_true.
endmethod.
ENDCLASS.
