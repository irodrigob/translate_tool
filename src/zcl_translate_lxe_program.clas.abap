CLASS zcl_translate_lxe_program DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_lxe
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_LXE_PROGRAM
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_lxe_type
        REDEFINITION .
    METHODS has_proposed_text
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_LXE_PROGRAM
*"* do not include other source files here!!!

    METHODS get_object_text
        REDEFINITION .
  PRIVATE SECTION.
*"* private components of class ZZC_CA02705
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_lxe_program IMPLEMENTATION.


  METHOD get_lxe_type.
* Programas
    APPEND 'PROG' TO rt_objects.

* Status de pantalla
    APPEND 'CUAD' TO rt_objects.
  ENDMETHOD.


  METHOD get_object_text.
    DATA ls_ddenq TYPE ddenqs.
    DATA ld_objtyp   TYPE e071-object.
    DATA ld_class   TYPE e071-object.
    DATA ld_pgmid    TYPE e071-pgmid.
    DATA ld_obj_name TYPE e071-obj_name.

* Se pasa los datos a una estructura para obtener el objeto de texto.
    ls_ddenq-objname = iv_obj_name.
    ls_ddenq-objtype = iv_object.

* La clase para todo es PROG menos para los PFSTATUS que es CUAD
    CASE iv_object.
      WHEN 'CUAD'. " PF Status
        ld_class = 'CUAD'.
      WHEN OTHERS.
        ld_class = 'PROG'.
    ENDCASE.

    PERFORM determine_lock_key IN PROGRAM saplseuq USING    space
                                          ld_class
                                          ls_ddenq
                                 CHANGING ld_pgmid
                                          ld_objtyp
                                          ld_obj_name.

* Tipo de objeto de texto del objeto pasado.
    rv_obj_text = ld_objtyp.

  ENDMETHOD.


  METHOD has_proposed_text.
    rv_has = abap_true.
  ENDMETHOD.
ENDCLASS.
