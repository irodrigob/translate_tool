*----------------------------------------------------------------------*
*       CLASS ZTRANSLATE_CMP DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_translate_cmp DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_CMP
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS abap .

    TYPES:
      BEGIN OF ts_object_list,
        pgmid	    TYPE pgmid,
        object    TYPE trobjtype,
        text      TYPE ko100-text,
        class     TYPE seoclsname,
        ref_class TYPE REF TO zcl_translate_cmp,
      END OF ts_object_list .
    TYPES:
      tt_object_list TYPE STANDARD TABLE OF ts_object_list .
    TYPES:
      BEGIN OF ts_components,
        object       TYPE trobjtype,
        obj_name     TYPE sobj_name,
        obj_name_int TYPE sobj_name,
      END OF ts_components .
    TYPES:
      tt_components TYPE STANDARD TABLE OF ts_components .

    CLASS-METHODS get_objectlist
      EXPORTING
        !et_object_list TYPE tt_object_list .
    METHODS get_components
      EXPORTING
        !et_components TYPE tt_components .
    METHODS get_objects_type
      RETURNING
        VALUE(rt_objects) TYPE objtyptable .
    METHODS check_object_exists
      IMPORTING
        !iv_pgmid       TYPE pgmid
        !iv_object      TYPE trobjtype
        !iv_obj_name    TYPE sobj_name
      RETURNING
        VALUE(rv_exist) TYPE sap_bool .
    METHODS set_params
      IMPORTING
        !iv_depth_refs TYPE i DEFAULT 2 .
  PROTECTED SECTION.
*"* protected components of class ZZC_CA02703
*"* do not include other source files here!!!
    TYPES: BEGIN OF ts_params_comp,
             depth_refs TYPE i, "Nivel de búsqueda de objetos a traducir
           END OF ts_params_comp.
    DATA mv_object TYPE trobjtype .
    DATA mv_obj_name TYPE sobj_name .
    DATA ms_params_comp TYPE ts_params_comp.
  PRIVATE SECTION.
*"* private components of class ZZC_CA02703
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_cmp IMPLEMENTATION.


  METHOD check_object_exists.
    rv_exist = abap_false.

* Como la mayoria de objetos (por no decir todos) que se traducen existen en la TADIR, la validacion
* se pone en la clase padre. Para casos concretos se hará sobrecarga.
    CLEAR: mv_object, mv_obj_name.

    SELECT SINGLE object obj_name INTO (mv_object, mv_obj_name)
           FROM tadir
           WHERE pgmid = iv_pgmid
                 AND object = iv_object
                 AND obj_name = iv_obj_name.
    IF sy-subrc = 0.
      rv_exist = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_components.
  ENDMETHOD.


  METHOD get_objectlist.
    FIELD-SYMBOLS <ls_classlist> TYPE LINE OF seo_inheritances.
    FIELD-SYMBOLS <ls_objects> TYPE LINE OF objtyptable.
    FIELD-SYMBOLS <ls_types_out> TYPE ko100.
    DATA lt_classlist TYPE seo_inheritances.
    DATA ls_object_list TYPE LINE OF tt_object_list.
    DATA lt_objects TYPE objtyptable.
    DATA lt_types_in TYPE STANDARD TABLE OF ko105.
    DATA ls_types_in TYPE ko105.
    DATA lt_types_out TYPE STANDARD TABLE OF ko100.

    CLEAR et_object_list.

* Leo todas las clases que heredan de la clase principal de componentes.
    SELECT * FROM vseoextend INTO TABLE lt_classlist
      WHERE refclsname = 'ZCL_TRANSLATE_CMP'
      AND version = '1'.

    IF sy-subrc = 0.
      LOOP AT lt_classlist ASSIGNING <ls_classlist>.
        CLEAR ls_object_list.

        ls_object_list-class = <ls_classlist>-clsname.
* Cualquier excepcion al instanciar la clase o al llamar de sus métodos hará que no se
* tenga en cuenta la clase.
        TRY.
* Instancio la clase que obtendrá los componentes
            CREATE OBJECT ls_object_list-ref_class TYPE (<ls_classlist>-clsname).

* Llamo al método para que me devuelva los objetos asociados al componente. Por ejemplo:
* para la clase para los componentes del diccionario devolverá: DTEL, DOMA, TABL, etc..
            lt_objects = ls_object_list-ref_class->get_objects_type( ).

            LOOP AT lt_objects ASSIGNING <ls_objects>.
              CLEAR: lt_types_in, lt_types_out, lt_types_in.
* Recupero la descripción del objeto para pasarla a la tabla principal. También servirá
* para validar que el objeto exista.
              ls_types_in-object = <ls_objects>.
              APPEND ls_types_in TO lt_types_in.
              CALL FUNCTION 'TRINT_OBJECT_TABLE'
                TABLES
                  tt_types_in  = lt_types_in
                  tt_types_out = lt_types_out.

              READ TABLE lt_types_out ASSIGNING <ls_types_out> INDEX 1.
              IF sy-subrc = 0.
                MOVE-CORRESPONDING <ls_types_out> TO ls_object_list.
                APPEND ls_object_list TO et_object_list.
              ENDIF..

            ENDLOOP.

            CLEAR lt_objects.

          CATCH cx_root.
        ENDTRY.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_objects_type.
  ENDMETHOD.


  METHOD set_params.

* Nivel de búsqueda de objetos a traducir a partir del objeto principal.
    ms_params_comp-depth_refs = iv_depth_refs.
  ENDMETHOD.
ENDCLASS.
