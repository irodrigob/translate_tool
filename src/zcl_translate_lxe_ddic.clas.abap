CLASS zcl_translate_lxe_ddic DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_lxe
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_LXE_DDIC
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_lxe_type
        REDEFINITION .
    METHODS has_proposed_text
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_LXE_DDIC
*"* do not include other source files here!!!

    METHODS get_object_text
        REDEFINITION .
  PRIVATE SECTION.
*"* private components of class ZCL_TRANSLATE_LXE_DDIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_lxe_ddic IMPLEMENTATION.


  METHOD get_lxe_type.
* Tables
    APPEND 'TABL' TO rt_objects.
* Data elements
    APPEND 'DTEL' TO rt_objects.
* Domain
    APPEND 'DOMA' TO rt_objects.
* Structure
    APPEND 'STRU' TO rt_objects.

  ENDMETHOD.


  METHOD get_object_text.
    DATA ls_ddenq TYPE ddenqs.
    DATA ld_objtyp   TYPE e071-object.
    DATA ld_pgmid    TYPE e071-pgmid.
    DATA ld_obj_name TYPE e071-obj_name.

* Se pasa los datos a una estructura para obtener el objeto de texto.
    ls_ddenq-objname = i_obj_name.
    ls_ddenq-objtype = i_object.


    PERFORM determine_lock_key IN PROGRAM saplseuq USING    space
                                          'DICT'
                                          ls_ddenq
                                 CHANGING ld_pgmid
                                          ld_objtyp
                                          ld_obj_name.

* Tipo de objeto de texto del objeto pasado.
    r_obj_text = ld_objtyp.

  ENDMETHOD.


  METHOD has_proposed_text.
    rv_has = abap_true.
  ENDMETHOD.
ENDCLASS.
