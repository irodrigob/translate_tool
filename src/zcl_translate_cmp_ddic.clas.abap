CLASS zcl_translate_cmp_ddic DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_cmp
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_CMP_DDIC
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_components
        REDEFINITION .
    METHODS get_objects_type
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_CMP_DDIC
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_TRANSLATE_CMP_DDIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_cmp_ddic IMPLEMENTATION.


  METHOD get_components.
    FIELD-SYMBOLS <ls_refs> TYPE LINE OF zif_ref_data=>tt_list_refs.
    DATA lt_refs TYPE zif_ref_data=>tt_list_refs.
    DATA lo_refs TYPE REF TO zcl_ref_object.
    DATA ls_components TYPE LINE OF tt_components.

    CLEAR et_components.

    CREATE OBJECT lo_refs.

    CALL METHOD lo_refs->search_refs
      EXPORTING
        iv_object             = mv_obj_name
        iv_type               = mv_object
        iv_level_depth_max    = ms_params_comp-depth_refs
      IMPORTING
        et_refs               = lt_refs
      EXCEPTIONS
        type_object_not_valid = 1
        OTHERS                = 2.

    IF sy-subrc = 0.

* Leo el primer registro de la tabla de ref
* Se pasa el componente pasado
      ls_components-object = mv_object.
      ls_components-obj_name = mv_obj_name.
      APPEND ls_components TO et_components.


* Se pasan las referencia del objeto a la tabla de componentes
* Uso la estructura base para añadir los campos comunes. Esto simplifica el codigo.
      LOOP AT lt_refs ASSIGNING <ls_refs>.
        ls_components-object = <ls_refs>-type_ref.
*      IF <ls_refs>-internal_name_ref IS NOT INITIAL.
*        ls_components-obj_name = <ls_refs>-internal_name_ref.
*      ELSE.
        ls_components-obj_name = <ls_refs>-object_ref.
*      ENDIF.

* Como nombre interno dejo siempre el "bueno" es decir, sin añadidos que se usan en clases o en funciones.
        ls_components-obj_name_int = <ls_refs>-object_ref.

        APPEND ls_components TO et_components.
      ENDLOOP.

      FREE lo_refs.

    ENDIF.
  ENDMETHOD.


  METHOD get_objects_type.
* Tablas
    APPEND zif_ref_data=>cs_types-table TO rt_objects.
* Estructuras
    APPEND zif_ref_data=>cs_types-struc TO rt_objects.
* Elemento de datos
    APPEND zif_ref_data=>cs_types-dataelem TO rt_objects.
* Dominio
    APPEND zif_ref_data=>cs_types-domain TO rt_objects.
* Tipo tabla
    APPEND zif_ref_data=>cs_types-tabltype TO rt_objects.
* Ayuda para búsqued
    APPEND zif_ref_data=>cs_types-seahlp TO rt_objects.
* Vistas
    APPEND zif_ref_data=>cs_types-view TO rt_objects.


  ENDMETHOD.
ENDCLASS.
