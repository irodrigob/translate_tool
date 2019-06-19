CLASS zcl_translate_cmp_program DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_cmp
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_CMP_PROGRAM
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_components
        REDEFINITION .
    METHODS get_objects_type
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_CMP_PROGRAM
*"* do not include other source files here!!!

    METHODS get_pf_status
      IMPORTING
        !iv_obj_name   TYPE sobj_name
      CHANGING
        !ct_components TYPE tt_components .
  PRIVATE SECTION.
*"* private components of class ZCL_TRANSLATE_CMP_PROGRAM
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_cmp_program IMPLEMENTATION.


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

* Obtengo el nombre interno, necesario para encontrar las traduccion en clases.
      CALL METHOD zcl_ref_source=>get_internal_name
        EXPORTING
          iv_object             = mv_obj_name
          iv_type               = mv_object
        IMPORTING
          ev_internal_name      = ls_components-obj_name
        EXCEPTIONS
          type_object_not_valid = 1
          OTHERS                = 2.
      IF sy-subrc NE 0.
        ls_components-obj_name = mv_obj_name.
      ENDIF.
      APPEND ls_components TO et_components.

* Los includes de los programas no se procesan ya que los textos pertenecen al programa padre.
      DELETE lt_refs WHERE type = 'PROG'
                           AND fullname_ref CS '\IC:'.

* Las clases de mensajes también se eliminan, ya que se capturan los mensajes de manera inidividual.
      DELETE lt_refs WHERE type_ref = zif_ref_data=>cs_types-messclas.

* Los status de pantalla/títulos no se leen en las referencias pero se han de tener en cuenta para
* las traducciones.
      get_pf_status( EXPORTING iv_obj_name = mv_obj_name
                     CHANGING ct_components = et_components ).

* Se pasan las referencia del objeto a la tabla de componentes
* Uso la estructura base para añadir los campos comunes. Esto simplifica el codigo.
      LOOP AT lt_refs ASSIGNING <ls_refs>.
        ls_components-object = <ls_refs>-type_ref.



* Devuelvo el nombre interno porque para funciones y clases se ha de usar el interno para recuperar los extos.
        IF <ls_refs>-internal_name_ref IS NOT INITIAL.
          ls_components-obj_name = <ls_refs>-internal_name_ref.
        ELSE.
          ls_components-obj_name = <ls_refs>-object_ref.
        ENDIF.

* Como nombre interno dejo siempre el "bueno" es decir, sin añadidos que se usan en clases o en funciones.
        ls_components-obj_name_int = <ls_refs>-object_ref.

        APPEND ls_components TO et_components.
      ENDLOOP.

      FREE lo_refs.

    ENDIF.

  ENDMETHOD.


  METHOD get_objects_type.

* Programas
    APPEND 'PROG' TO rt_objects.
* Clases
    APPEND 'CLAS' TO rt_objects.

  ENDMETHOD.


  METHOD get_pf_status.
    FIELD-SYMBOLS <ls_ista> TYPE rsmpe_stat.
    FIELD-SYMBOLS <ls_itit> TYPE rsmpe_titt.

    DATA: ista TYPE TABLE OF rsmpe_stat,
          ifun TYPE TABLE OF rsmpe_funt,
          imen TYPE TABLE OF rsmpe_men,
          imtx TYPE TABLE OF rsmpe_mnlt,
          iact TYPE TABLE OF rsmpe_act,
          ibut TYPE TABLE OF rsmpe_but,
          ipfk TYPE TABLE OF rsmpe_pfk,
          iset TYPE TABLE OF rsmpe_staf,
          idoc TYPE TABLE OF rsmpe_atrt,
          itit TYPE TABLE OF rsmpe_titt,
          ibiv TYPE TABLE OF rsmpe_buts.
    DATA ld_program TYPE trdir-name.
    DATA ls_component TYPE LINE OF tt_components.

    ld_program = iv_obj_name.
    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = ld_program
        language        = sy-langu
      TABLES
        sta             = ista
        fun             = ifun
        men             = imen
        mtx             = imtx
        act             = iact
        but             = ibut
        pfk             = ipfk
        set             = iset
        doc             = idoc
        tit             = itit
        biv             = ibiv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.

    IF sy-subrc = 0.

* Si hay status de pantalla o títulos lo indico en los componentes.
      IF ista IS NOT INITIAL OR itit IS NOT INITIAL.
        ls_component-object = 'CUAD'.
        ls_component-obj_name = mv_obj_name.
        INSERT ls_component INTO TABLE ct_components.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
