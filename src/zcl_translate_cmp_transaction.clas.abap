CLASS zcl_translate_cmp_transaction DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_translate_cmp
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_components
        REDEFINITION .
    METHODS get_objects_type
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_translate_cmp_transaction IMPLEMENTATION.
  METHOD get_components.
    " La transacción se valida que al menos exista
    SELECT SINGLE @abap_true INTO @DATA(lv_exist)
           FROM tstc
           WHERE tcode = @mv_obj_name.
    IF sy-subrc = 0.
      INSERT VALUE #( object = mv_object obj_name = mv_obj_name obj_name_int = mv_obj_name ) INTO TABLE et_components.
    ENDIF.
  ENDMETHOD.

  METHOD get_objects_type.
    APPEND 'TRAN' TO rt_objects.
  ENDMETHOD.

ENDCLASS.
