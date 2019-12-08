CLASS zcl_translate_lxe_transaction DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_translate_lxe
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_lxe_type
        REDEFINITION .
    METHODS has_proposed_text
        REDEFINITION .
        METHODS set_check_params REDEFINITION.
  PROTECTED SECTION.
    METHODS get_object_text
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TRANSLATE_LXE_TRANSACTION IMPLEMENTATION.


  METHOD get_lxe_type.
    APPEND 'TRAN' TO rt_objects.
  ENDMETHOD.


  METHOD get_object_text.
    rv_obj_text = 'TRAN'.
  ENDMETHOD.


  METHOD has_proposed_text.
    rv_has = abap_true.
  ENDMETHOD.


  METHOD set_check_params.
 DATA ls_e071 TYPE LINE OF tr_objects.
    DATA lt_e071_text TYPE tr_objects .
    DATA lt_e071k_text TYPE tr_keys .

* Guardo los parametros pasados a variables globales.
    mv_object = iv_object.
    mv_obj_name = iv_obj_name.
    mv_olang = iv_olang.
    mv_tlang = iv_tlang.

    mv_object_text = get_object_text( iv_object = iv_object
                                   iv_obj_name = iv_obj_name ).

* Si no se puede determinar el tipo de objeto del texto se lanza una
* excepcion.
    IF mv_object_text IS NOT INITIAL.

* Obtengo el idioma original del objeto. Se usará cuando no hay textos en el idioma de origen.
      get_masterlang( ).

* Se rellena una tabla interna (e071 la que contiene los objetos en las tareas) para poder la
* informacion para el transporte.
* En la transacciónes a diferencia del resto de objeto hay que pasarle R3TR y lo LIMU, que son como objetos secundarios.
      ls_e071-pgmid = 'R3TR'.
      ls_e071-object = mv_object_text.
      ls_e071-obj_name = iv_obj_name.
      APPEND ls_e071 TO lt_e071_text.

* Se obtiene la informacion para obtener los datos traduccion en base a los datos de la tabla de transporte.
      CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT'
        TABLES
          in_e071  = lt_e071_text
          in_e071k = lt_e071k_text
          ex_colob = mt_colob.

      IF mt_colob IS INITIAL.
        RAISE object_not_valid.
      ENDIF.

    ELSE.
      RAISE object_not_valid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
