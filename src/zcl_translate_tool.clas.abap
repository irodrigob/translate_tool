CLASS zcl_translate_tool DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZTRANSLATE_TOOL
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS abap .
    TYPE-POOLS trwbo .

    DATA mv_object TYPE trobjtype READ-ONLY .
    DATA mv_obj_name TYPE sobj_name READ-ONLY .
    CONSTANTS mc_field_txt_lang TYPE bsstring VALUE 'FIELD_'. "#EC NOTEXT
    CONSTANTS mc_field_ctrl_lang TYPE bsstring VALUE 'UPDKZ_'. "#EC NOTEXT
    CONSTANTS mc_struc_main_fields TYPE tabname VALUE 'ZTRANSLATE_MAIN_FIELDS'. "#EC NOTEXT
    CONSTANTS mc_field_style TYPE fieldname VALUE 'FIELD_STYLE'. "#EC NOTEXT
    CONSTANTS mc_style_wo_trans TYPE raw4 VALUE '0000000F'. "#EC NOTEXT
    CONSTANTS mc_style_prop_wo_conf TYPE raw4 VALUE '0000000C'. "#EC NOTEXT
    CONSTANTS mc_style_prop_conf TYPE raw4 VALUE '0000000E'. "#EC NOTEXT
    CONSTANTS mc_style_text_changed TYPE raw4 VALUE '0000000A'. "#EC NOTEXT

    METHODS constructor .
    METHODS get_languages .
    METHODS check_obj_2_trans
      IMPORTING
        !iv_object      TYPE trobjtype
        !iv_obj_name    TYPE sobj_name
      RETURNING
        VALUE(rv_exist) TYPE sap_bool .
    METHODS load_object_texts .
    METHODS set_params_selscreen
      IMPORTING
        !iv_olang      TYPE lxeisolang
        !it_tlang      TYPE lxe_tt_lxeisolang
        !iv_trkorr     TYPE trkorr OPTIONAL
        !iv_depth_refs TYPE i DEFAULT 2 .
    METHODS get_data
      RETURNING
        VALUE(r_data) TYPE REF TO data .
    METHODS set_data
      IMPORTING
        !it_data TYPE REF TO data .
    METHODS get_fcat
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat .
    METHODS save_data
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS transport_mod_obj
      EXPORTING
        VALUE(es_return) TYPE bapiret2 .
    METHODS get_allowed_objects
      RETURNING
        VALUE(rt_objects) TYPE tr_object_texts .
    METHODS get_tlang
      RETURNING
        VALUE(rt_tlang) TYPE lxe_tt_lxeisolang .
    METHODS get_name_field_text
      IMPORTING
        !iv_language        TYPE lxeisolang
      RETURNING
        VALUE(rv_fieldname) TYPE fieldname .
  PROTECTED SECTION.

    TYPES:
*"* protected components of class ZTRANSLATE_TOOL
*"* do not include other source files here!!!
      tt_lxe_t002 TYPE STANDARD TABLE OF lxe_t002 .
    TYPES:
      tt_lxe_t002t TYPE STANDARD TABLE OF lxe_t002t .
    TYPES:
      BEGIN OF ts_mngt_text,
        object       TYPE trobjtype,
        obj_name     TYPE sobj_name,
        tlang        TYPE lxeisolang,
        oobject      TYPE REF TO zcl_translate_lxe,
        data_changed TYPE sap_bool,
      END OF ts_mngt_text .
    TYPES:
      tt_mngt_text TYPE SORTED TABLE OF ts_mngt_text
                     WITH NON-UNIQUE KEY object obj_name tlang .
    TYPES:
      BEGIN OF ts_main_fields,
        object      TYPE  trobjtype,
        obj_name    TYPE  sobj_name,
        objtype     TYPE  lxeobjtype,
        id_text     TYPE  lxetextkey,
        txt_olang   TYPE  string, "ztranslate_e_olang,
        field_style TYPE  lvc_t_styl,
      END OF ts_main_fields .
    TYPES:
      tt_main_fields TYPE STANDARD TABLE OF ts_main_fields .

    DATA mt_lxe_t002 TYPE tt_lxe_t002 .
    DATA mt_lxe_t002t TYPE tt_lxe_t002t .
    DATA mt_tlang TYPE lxe_tt_lxeisolang .
    DATA mv_olang TYPE lxeisolang .
    DATA mo_it_data TYPE REF TO data .
    DATA mo_wa_data TYPE REF TO data .
    DATA mt_fcat TYPE lvc_t_fcat .
    DATA mt_mngt_text TYPE tt_mngt_text .
    DATA mv_trkorr TYPE e070-trkorr .
    DATA mt_components TYPE tt_main_fields .
    DATA mt_object_list TYPE zcl_translate_cmp=>tt_object_list .
    DATA mt_lxe_list TYPE zcl_translate_lxe=>tt_lxe_list .
    DATA mv_depth_refs TYPE i .

    METHODS update_text_object .
    METHODS copy_1_of_read_process_texts .
    METHODS read_process_texts .
    METHODS change_text_fcat
      IMPORTING
        !i_text TYPE any
      CHANGING
        !c_fcat TYPE lvc_s_fcat .
    METHODS get_components
      EXPORTING
        VALUE(e_components) TYPE tt_main_fields .
    METHODS create_it_fcat .
    METHODS get_name_field_ctrl
      IMPORTING
        !i_language        TYPE lxeisolang
      RETURNING
        VALUE(r_fieldname) TYPE fieldname .
    CLASS-METHODS fill_return
      IMPORTING
        !i_type         TYPE any
        !i_number       TYPE any
        !i_message_v1   TYPE any OPTIONAL
        !i_message_v2   TYPE any OPTIONAL
        !i_message_v3   TYPE any OPTIONAL
        !i_message_v4   TYPE any OPTIONAL
        !i_id           TYPE any OPTIONAL
      RETURNING
        VALUE(r_return) TYPE bapiret2 .
    METHODS get_ref_text_object
      IMPORTING
        !i_object   TYPE trobjtype
        !i_obj_name TYPE sobj_name
        !i_tlang    TYPE lxeisolang
      EXPORTING
        !e_object   TYPE REF TO zcl_translate_lxe .
    METHODS proposal_text
      IMPORTING
        !i_tlang        TYPE lxeisolang
        !is_texts       TYPE zcl_translate_lxe=>ts_texts
        !io_object_text TYPE REF TO zcl_translate_lxe
      CHANGING
        !cs_wa          TYPE any .
  PRIVATE SECTION.
*"* private components of class ZTRANSLATE_TOOL
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_tool IMPLEMENTATION.


  METHOD change_text_fcat.
    c_fcat-scrtext_l = i_text.
    c_fcat-scrtext_s = i_text.
    c_fcat-scrtext_m = i_text.
    c_fcat-reptext = i_text.
  ENDMETHOD.


  METHOD check_obj_2_trans.
    FIELD-SYMBOLS <ls_object_list> LIKE LINE OF mt_object_list.

* Aprovecho para guardar el objeto y el nombre del mismo. Se utilizará
* en otros puntos del programa
    mv_object = iv_object.
    mv_obj_name = iv_obj_name.

    READ TABLE mt_object_list ASSIGNING <ls_object_list> WITH KEY object = iv_object.
    IF sy-subrc = 0.
      rv_exist = <ls_object_list>-ref_class->check_object_exists( iv_pgmid = <ls_object_list>-pgmid
                                                                 iv_object   = iv_object
                                                                 iv_obj_name = iv_obj_name ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

* Carga de los idioma del entorno de traduccion
    get_languages( ).

* Se obtiene por separado los objetos que pueden ser traducidos y las clases que permiten
* traducir dichos objetos.
* El motivo de hacerlo separado es que muchos objetos se traducen de la misma manera: programas, funciones,
* clases, etc.. Por eso de la separacion.

* Obtengo los objetos que: 1) pueden ser traducidos 2) podemos obtener componentes.
    zcl_translate_cmp=>get_objectlist( IMPORTING et_object_list = mt_object_list ).

* Obtengo los objetos cuyo proceso de traduccion esta implementado.
    zcl_translate_lxe=>get_lxelist( IMPORTING et_lxe_list = mt_lxe_list  ).

  ENDMETHOD.


  METHOD copy_1_of_read_process_texts.
    FIELD-SYMBOLS <tbl> TYPE table.
    FIELD-SYMBOLS <wa> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <field_style> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_tlang> LIKE LINE OF mt_tlang.
    FIELD-SYMBOLS <ls_texts> TYPE zcl_translate_lxe=>ts_texts.
    FIELD-SYMBOLS <ls_lxe_list> LIKE LINE OF mt_lxe_list.
    DATA ls_main_fields TYPE LINE OF tt_main_fields.
    DATA lt_main_fields TYPE tt_main_fields.
    DATA lo_data TYPE REF TO data.
    DATA ls_mngt_text LIKE LINE OF mt_mngt_text.
    DATA ld_tabix TYPE sytabix.
    DATA lt_texts TYPE zcl_translate_lxe=>tt_texts.
    DATA ld_primer_source TYPE sap_bool.
    DATA ld_field_text TYPE fieldname.
    DATA ls_field_style TYPE LINE OF lvc_t_styl.
    DATA ld_tabix_mngt TYPE sytabix.
    DATA ld_text_object TYPE REF TO zcl_translate_lxe.

    ASSIGN mo_it_data->* TO <tbl>.

    CLEAR <tbl>.

    LOOP AT mt_components INTO ls_main_fields.

* En el primer texto informaré tanto el idioma de origen como el destino.
* a partir del segundo idioma de destino solo leerá el de destino.
      ld_primer_source = abap_true.

* Se recorre la tabla de idioma a traducir.
      LOOP AT mt_tlang ASSIGNING <ls_tlang>.
        CLEAR: ls_mngt_text, lt_texts.

* Se recupera el objeto de texto para obtener las traducciones
        CALL METHOD get_ref_text_object
          EXPORTING
            i_object   = ls_main_fields-object
            i_obj_name = ls_main_fields-obj_name
            i_tlang    = <ls_tlang>
          IMPORTING
            e_object   = ld_text_object.


        IF ld_text_object IS BOUND.

* Miro si el objeto ya ha sido instancio previamente. Si no es así, se crea.
          READ TABLE mt_mngt_text INTO ls_mngt_text
                                  WITH TABLE KEY object = ls_main_fields-object
                                                 obj_name = ls_main_fields-obj_name
                                                 tlang = <ls_tlang>.
          IF sy-subrc NE 0.
* Pongo la posicion cero para que se inserte el registro.
            ld_tabix_mngt = 0.
* Se pasa los datos a la tabla que contendrá el objetos de textos en cada
* idioma para cada objeto.
            ls_mngt_text-tlang = <ls_tlang>.
            ls_mngt_text-object = ls_main_fields-object.
            ls_mngt_text-obj_name = ls_main_fields-obj_name.

* Recupero la clase que servirá para traducir el objeto.
            READ TABLE mt_lxe_list ASSIGNING <ls_lxe_list> WITH KEY object = ls_main_fields-object.
            IF sy-subrc = 0.
* Instancio el objeto que hará la traduccion
              CREATE OBJECT ls_mngt_text-oobject TYPE (<ls_lxe_list>-class).

* Valido que el objeto sea valido.
              CALL METHOD ls_mngt_text-oobject->set_check_params
                EXPORTING
                  iv_object        = ls_main_fields-object
                  iv_obj_name      = ls_main_fields-obj_name
                  iv_olang         = mv_olang
                  iv_tlang         = <ls_tlang>
                EXCEPTIONS
                  object_not_valid = 1
                  OTHERS           = 2.
              IF sy-subrc = 0.
* Leo los datos
                ls_mngt_text-oobject->load_text( ).

* Recupero los textos para pasarlos a la tabla de datos
                ls_mngt_text-oobject->get_texts( IMPORTING et_texts = lt_texts ).
              ENDIF.
            ENDIF.
          ELSE.
* Me guardo la posición para despues actualizarla.
            ld_tabix_mngt = sy-tabix.
* Vuelvo a cargar los datos
            ls_mngt_text-oobject->load_text( ).
* Recupero los textos.
            ls_mngt_text-oobject->get_texts( IMPORTING et_texts = lt_texts ).
          ENDIF.

* Solo se tienen en cuanta los objetos con textos.
          IF lt_texts IS NOT INITIAL.

* Recorro la tabla de textos para pasarla a la de datos.
            LOOP AT lt_texts ASSIGNING <ls_texts>.
              ls_main_fields-id_text = <ls_texts>-textkey. " Id del texto
              ls_main_fields-objtype = <ls_texts>-objtype. " Tipo de objeto

              IF ld_primer_source = abap_true.
                ls_main_fields-txt_olang = <ls_texts>-s_text.
              ENDIF.

* Leo si el para el objeto e id de texto esta en la tabla que guarda de manera temporal lo mismo(campos principales) que la
* tabla global dinámica. Esta tabla permite evitar duplicados o más cuando hay varios idiomas a traducir para un mismo objeto.
              READ TABLE lt_main_fields TRANSPORTING NO FIELDS WITH KEY object = ls_main_fields-object
                                                                        obj_name = ls_main_fields-obj_name
                                                                        id_text = ls_main_fields-id_text
                                                                        objtype = ls_main_fields-objtype.
              IF sy-subrc = 0.
* Me guardo la posicion donde se ha encontrado.
                ld_tabix = sy-tabix.
* Los registros de la tabla loca y temporal siempre coinciden porque se añaden los mismos datos.
                READ TABLE <tbl> ASSIGNING <wa> INDEX ld_tabix.
              ELSE.
                CLEAR ld_tabix.
* Reasigno la cabecera para limpiar valores previos.
                ASSIGN mo_wa_data->* TO <wa>.
                APPEND ls_main_fields TO lt_main_fields.
* Paso los datos comunes a la cabecera de la tabla de datos
                MOVE-CORRESPONDING ls_main_fields TO <wa>.
              ENDIF.

* Construyo el campo donde se pondra el texto destino
              ld_field_text = get_name_field_text( <ls_tlang> ).
              ASSIGN COMPONENT ld_field_text OF STRUCTURE <wa> TO <field>.
              IF sy-subrc = 0.
                <field> = <ls_texts>-t_text.
                IF <field> IS INITIAL.
* Recupero la mejor propuesta para el campo
                  CALL METHOD ls_mngt_text-oobject->get_best_text_proposal
                    EXPORTING
                      iv_textkey   = <ls_texts>-textkey
                      iv_objtype   = <ls_texts>-objtype
                    IMPORTING
                      ev_best_text = <field>.
                ENDIF.

* Determino el estilo según el valor del campo
                ASSIGN COMPONENT mc_field_style OF STRUCTURE <wa> TO <field_style>.
                IF sy-subrc = 0.
                  ls_field_style-fieldname = ld_field_text.
* Si no hay texto ni por propuesta ni por origen se pone el color de no hay traduccion
                  IF <field> IS INITIAL.
                    ls_field_style-style = mc_style_wo_trans.
* Si hay texto por la propuesta pero el original no lo tenia, se pone el texto de pdte de confirmacion.
                  ELSEIF <ls_texts>-t_text IS INITIAL.
                    ls_field_style-style = mc_style_prop_wo_conf.
                  ELSE.
* Si el texto esta informado compruebo si el texto esta dentro de las propuestas
* para el texto. Según el resultado el color de la celda varia.
                    IF ls_mngt_text-oobject->is_text_in_proposal( iv_text = <field>
                                                                  iv_textkey = <ls_texts>-textkey
                                                                  iv_objtype = <ls_texts>-objtype ) = abap_true.
                      ls_field_style-style = mc_style_prop_conf.
                    ELSE.
                      ls_field_style-style = mc_style_prop_wo_conf.
                    ENDIF.
                  ENDIF.
                  INSERT ls_field_style INTO TABLE <field_style>.
                  CLEAR ls_field_style.
                ENDIF.
              ENDIF.

* Si el objeto e id de texto no esta en la tabla temporal, muevo los campos principales
* a la cabecera y añado los datos.
              IF ld_tabix IS INITIAL.
                APPEND <wa> TO <tbl>.
                CLEAR <wa>.
              ENDIF.

            ENDLOOP.

* Segun el valor de la variable ld_tabix_mngt se sabe si hay que insertar o modificar.
            IF ld_tabix_mngt IS INITIAL.
              INSERT ls_mngt_text INTO TABLE mt_mngt_text.
            ELSE.
              MODIFY mt_mngt_text FROM ls_mngt_text INDEX ld_tabix_mngt.
            ENDIF.

* Marco para que el texto de origen no se vuelva a pasar porque ya se ha hecho con el primer idioma.
            ld_primer_source = abap_false.

          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_it_fcat.
    FIELD-SYMBOLS <ls_dlang> LIKE LINE OF mt_tlang.
    FIELD-SYMBOLS <ls_fcat> TYPE LINE OF lvc_t_fcat.
    FIELD-SYMBOLS <ls_lxe_t002t> LIKE LINE OF mt_lxe_t002t.
    DATA ls_fcat TYPE LINE OF lvc_t_fcat.
    DATA lo_main_fields TYPE REF TO data.
    DATA lt_fcat_aux TYPE lvc_t_fcat.
    DATA ld_col_pos TYPE i VALUE 1.

    FREE: mo_it_data, mo_wa_data.

* Se crea el objeto temporal con los campos base.
    CALL METHOD ztranslate_utilities=>create_wa_from_struc
      EXPORTING
        i_struc    = mc_struc_main_fields
      IMPORTING
        e_workarea = lo_main_fields.

* Recupero el catalogo de la tabla de campos
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = mc_struc_main_fields
        i_bypassing_buffer     = 'X'
      CHANGING
        ct_fieldcat            = mt_fcat[]
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

* Se ajusta el catalogo de campos principal
    LOOP AT mt_fcat ASSIGNING <ls_fcat>.
      CASE <ls_fcat>-fieldname.
        WHEN 'TXT_OLANG'. " Texto origen
* Se pone el texto del idioma de origen
          READ TABLE mt_lxe_t002t ASSIGNING <ls_lxe_t002t> WITH KEY language = mv_olang.
          IF sy-subrc = 0.
            CALL METHOD change_text_fcat(
              EXPORTING
                i_text = <ls_lxe_t002t>-sptxt
              CHANGING
                c_fcat = <ls_fcat> ).
          ENDIF.
          <ls_fcat>-col_opt = abap_true.
      ENDCASE.

* Los campos principales son fijos para que se pueden ver en todo momento.
      <ls_fcat>-fix_column = abap_true.
      ADD 1 TO ld_col_pos.
    ENDLOOP.

* Construyo los campos que se añadirán a la tabla base para crear la principal
* Por cada idioma se ponen dos campos: 1) campo con el texto 2) indicador que se ha modificado el campo
    LOOP AT mt_tlang ASSIGNING <ls_dlang>.

* Campo con el texto
      CLEAR ls_fcat.
      ls_fcat-fieldname = get_name_field_text( <ls_dlang> ).
*    ls_fcat-rollname = 'LXEUNITLIN'.
      ls_fcat-inttype = 'C'.
      ls_fcat-intlen = '255'.
      ls_fcat-lowercase = abap_true.
      ls_fcat-edit = abap_true.
      ls_fcat-col_opt = abap_true.
      ls_fcat-col_pos = ld_col_pos.

* Texto del campo
      READ TABLE mt_lxe_t002t ASSIGNING <ls_lxe_t002t> WITH KEY language = <ls_dlang>.
      IF sy-subrc = 0.
        CALL METHOD change_text_fcat(
          EXPORTING
            i_text = <ls_lxe_t002t>-sptxt
          CHANGING
            c_fcat = ls_fcat ).
      ELSE.
        CALL METHOD change_text_fcat(
          EXPORTING
            i_text = <ls_dlang>
          CHANGING
            c_fcat = ls_fcat ).
      ENDIF.
      APPEND ls_fcat TO lt_fcat_aux.
      ADD 1 TO ld_col_pos.

* Campo de control
      CLEAR ls_fcat.
      CONCATENATE mc_field_ctrl_lang <ls_dlang> INTO ls_fcat-fieldname.
      TRANSLATE ls_fcat-fieldname TO UPPER CASE.
      ls_fcat-rollname = 'SAP_BOOL'.
      ls_fcat-tech = abap_true.
      ls_fcat-col_pos = ld_col_pos.
      APPEND ls_fcat TO lt_fcat_aux.
      ADD 1 TO ld_col_pos.
    ENDLOOP.

* Se añaden los campos de idioma al catalogo de campos principal
    APPEND LINES OF lt_fcat_aux TO mt_fcat.

* Se crea la tabla interna en base a los campos principales + los de idioma
    CALL METHOD ztranslate_utilities=>create_it_fields_base_ref
      EXPORTING
        i_base_fields = lo_main_fields
        i_new_fields  = lt_fcat_aux
      IMPORTING
        e_table       = mo_it_data
        e_wa          = mo_wa_data.

  ENDMETHOD.


  METHOD fill_return.

    CLEAR r_return.

    r_return-type = i_type.

* Se no se pasa una clase de mensaje se pone la generica de la aplicación.
    IF i_id IS NOT SUPPLIED.
      r_return-id = 'ZTRANSLATE_TOOL'.
    ELSE.
      r_return-id = i_id.
    ENDIF.
    r_return-number = i_number.
    r_return-message_v1 = i_message_v1.
    r_return-message_v2 = i_message_v2.
    r_return-message_v3 = i_message_v3.
    r_return-message_v4 = i_message_v4.


    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = r_return-id
        number     = r_return-number
        language   = sy-langu
        textformat = 'ASC'
        message_v1 = r_return-message_v1
        message_v2 = r_return-message_v2
        message_v3 = r_return-message_v3
        message_v4 = r_return-message_v4
      IMPORTING
        message    = r_return-message.

  ENDMETHOD.


  METHOD get_allowed_objects.
    FIELD-SYMBOLS <ls_object_list> LIKE LINE OF mt_object_list.
    DATA ls_objects TYPE LINE OF tr_object_texts.

    CLEAR rt_objects.
    LOOP AT mt_object_list ASSIGNING <ls_object_list>.
      MOVE-CORRESPONDING <ls_object_list> TO ls_objects.
      APPEND ls_objects TO rt_objects.
      CLEAR ls_objects.
    ENDLOOP.

    SORT rt_objects BY pgmid object.

  ENDMETHOD.


  METHOD get_components.
    FIELD-SYMBOLS <ls_components> TYPE LINE OF zcl_translate_cmp=>tt_components.
    FIELD-SYMBOLS <ls_object_list> LIKE LINE OF mt_object_list.
    DATA ls_main_fields TYPE LINE OF tt_main_fields.
    DATA lt_components TYPE zcl_translate_cmp=>tt_components.


    READ TABLE mt_object_list ASSIGNING <ls_object_list> WITH KEY object = mv_object.
    IF sy-subrc = 0.

* Paso los parámetros para condicionar la búsqueda.
      <ls_object_list>-ref_class->set_params( EXPORTING iv_depth_refs = mv_depth_refs ).

* Obtengo el componentes del objeto a traducir.
      <ls_object_list>-ref_class->get_components( IMPORTING et_components = lt_components ).

* Paso los components al parámetro de salida
      LOOP AT lt_components ASSIGNING <ls_components>.
        MOVE-CORRESPONDING <ls_components> TO ls_main_fields.
        APPEND ls_main_fields TO e_components.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_data.
    r_data = mo_it_data.
  ENDMETHOD.


  METHOD get_fcat.
    rt_fcat = mt_fcat.
  ENDMETHOD.


  METHOD get_languages.

    CALL FUNCTION 'LXE_T002_GET_LANGUAGES'
      EXPORTING
        r3_lang_only = abap_true
      TABLES
        lt_lang      = mt_lxe_t002[]
        lt_langt     = mt_lxe_t002t[].

  ENDMETHOD.


  METHOD get_name_field_ctrl.
    CONCATENATE mc_field_ctrl_lang i_language INTO r_fieldname.
    TRANSLATE r_fieldname TO UPPER CASE.
  ENDMETHOD.


  METHOD get_name_field_text.
    CONCATENATE mc_field_txt_lang iv_language INTO rv_fieldname.
    TRANSLATE rv_fieldname TO UPPER CASE.
  ENDMETHOD.


  METHOD get_ref_text_object.
    FIELD-SYMBOLS <ls_mngt_text> LIKE LINE OF mt_mngt_text.
    FIELD-SYMBOLS <ls_lxe_list> LIKE LINE OF mt_lxe_list.
    DATA ls_mngt_text LIKE LINE OF mt_mngt_text.

* Miro si el objeto ya ha sido instancio previamente. Si no es así, se crea.
    READ TABLE mt_mngt_text ASSIGNING <ls_mngt_text>
                            WITH TABLE KEY object = i_object
                                           obj_name = i_obj_name
                                           tlang = i_tlang.
    IF sy-subrc NE 0.
* Se pasa los datos a la tabla que contendrá el objetos de textos en cada
* idioma para cada objeto.
      ls_mngt_text-tlang = i_tlang.
      ls_mngt_text-object = i_object.
      ls_mngt_text-obj_name = i_obj_name.

* Recupero la clase que servirá para traducir el objeto.
      READ TABLE mt_lxe_list ASSIGNING <ls_lxe_list> WITH KEY object = i_object.
      IF sy-subrc = 0.
* Instancio el objeto que hará la traduccion
        CREATE OBJECT ls_mngt_text-oobject TYPE (<ls_lxe_list>-class).

* Valido que el objeto sea valido.
        CALL METHOD ls_mngt_text-oobject->set_check_params
          EXPORTING
            iv_object        = i_object
            iv_obj_name      = i_obj_name
            iv_olang         = mv_olang
            iv_tlang         = i_tlang
          EXCEPTIONS
            object_not_valid = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
* Se añade el nuevo registro
          INSERT ls_mngt_text INTO TABLE mt_mngt_text.

* Y se devuelve el objeto.
          e_object = ls_mngt_text-oobject.
        ENDIF.
      ENDIF.
    ELSE.
      e_object = <ls_mngt_text>-oobject.
    ENDIF.

* Si el puntero esta asignado devuelvo el objeto de textos al parámetro de salida.
* Esto que permite:
* 1) Los datos del objeto se actualizarán solo debido a que devuelvo
* una referencia a memoria que apunta al registro de la tabla donde esta que también
* es un puntero.
* 2) Simplifica el codigo que llama a dicho método. Ya que no hay porque preocuparse
* de actualizar el registro en it_mngt_text
    IF <ls_mngt_text> IS ASSIGNED.

    ENDIF.
  ENDMETHOD.


  METHOD get_tlang.
    rt_tlang = mt_tlang.
  ENDMETHOD.


  METHOD load_object_texts.
    CLEAR mt_components.

* Se crea la tabla interna en base al catalogo de campos y textos a traduccion
    create_it_fcat( ).

* Componentes del objeto.
    get_components( IMPORTING e_components = mt_components ).

* Lectura y proceso de los textos
    read_process_texts( ).

  ENDMETHOD.


  METHOD proposal_text.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <field_style> TYPE ANY TABLE.
    DATA ld_field_text TYPE fieldname.
    DATA ls_field_style TYPE LINE OF lvc_t_styl.

    ld_field_text = get_name_field_text( i_tlang ).
    ASSIGN COMPONENT ld_field_text OF STRUCTURE cs_wa TO <field>.
    IF sy-subrc = 0.

      IF <field> IS INITIAL.
* Recupero la mejor propuesta para el campo
        CALL METHOD io_object_text->get_best_text_proposal
          EXPORTING
            iv_textkey   = is_texts-textkey
            iv_objtype   = is_texts-objtype
          IMPORTING
            ev_best_text = <field>.
      ENDIF.

* Determino el estilo según el valor del campo
      ASSIGN COMPONENT mc_field_style OF STRUCTURE cs_wa TO <field_style>.
      IF sy-subrc = 0.
        ls_field_style-fieldname = ld_field_text.
* Si no hay texto ni por propuesta ni por origen se pone el color de no hay traduccion
        IF <field> IS INITIAL.
          ls_field_style-style = mc_style_wo_trans.
* Si hay texto por la propuesta pero el original no lo tenia, se pone el texto de pdte de confirmacion.
        ELSEIF is_texts-t_text IS INITIAL.
          ls_field_style-style = mc_style_prop_wo_conf.
        ELSE.
* Si el texto esta informado compruebo si el texto esta dentro de las propuestas
* para el texto. Según el resultado el color de la celda varia.
          IF io_object_text->is_text_in_proposal( iv_text = <field>
                                                        iv_textkey = is_texts-textkey
                                                        iv_objtype = is_texts-objtype ) = abap_true.
            ls_field_style-style = mc_style_prop_conf.
          ELSE.
            ls_field_style-style = mc_style_prop_wo_conf.
          ENDIF.
        ENDIF.
        INSERT ls_field_style INTO TABLE <field_style>.
        CLEAR ls_field_style.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD read_process_texts.
    FIELD-SYMBOLS <tbl> TYPE table.
    FIELD-SYMBOLS <wa> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <ls_tlang> LIKE LINE OF mt_tlang.
    FIELD-SYMBOLS <ls_texts> TYPE zcl_translate_lxe=>ts_texts.
    DATA ls_main_fields TYPE LINE OF tt_main_fields.
    DATA lt_main_fields TYPE tt_main_fields.
    DATA lo_data TYPE REF TO data.
    DATA ld_tabix TYPE sytabix.
    DATA lt_texts TYPE zcl_translate_lxe=>tt_texts.
    DATA ld_primer_source TYPE sap_bool.
    DATA ld_field_text TYPE fieldname.
    DATA lo_text_object TYPE REF TO zcl_translate_lxe.

    ASSIGN mo_it_data->* TO <tbl>.

    CLEAR <tbl>.

    LOOP AT mt_components INTO ls_main_fields.

* En el primer texto informaré tanto el idioma de origen como el destino.
* a partir del segundo idioma de destino solo leerá el de destino.
      ld_primer_source = abap_true.

* Se recorre la tabla de idioma a traducir.
      LOOP AT mt_tlang ASSIGNING <ls_tlang>.
        CLEAR: lt_texts.

* Se recupera el objeto de texto para obtener las traducciones
        CALL METHOD get_ref_text_object
          EXPORTING
            i_object   = ls_main_fields-object
            i_obj_name = ls_main_fields-obj_name
            i_tlang    = <ls_tlang>
          IMPORTING
            e_object   = lo_text_object.

        IF lo_text_object IS BOUND.

* Carga de textos y recuperacion de los textos
          lo_text_object->load_text( ).
          lo_text_object->get_texts( IMPORTING et_texts = lt_texts ).

* Solo se tienen en cuanta los objetos con textos.
          IF lt_texts IS NOT INITIAL.

* Recorro la tabla de textos para pasarla a la de datos.
            LOOP AT lt_texts ASSIGNING <ls_texts>.
              ls_main_fields-id_text = <ls_texts>-textkey. " Id del texto
              ls_main_fields-objtype = <ls_texts>-objtype. " Tipo de objeto

              IF ld_primer_source = abap_true.
                ls_main_fields-txt_olang = <ls_texts>-s_text.
              ENDIF.

* Leo si el para el objeto e id de texto esta en la tabla que guarda de manera temporal lo mismo(campos principales) que la
* tabla global dinámica. Esta tabla permite evitar duplicados o más cuando hay varios idiomas a traducir para un mismo objeto.
              READ TABLE lt_main_fields TRANSPORTING NO FIELDS WITH KEY object = ls_main_fields-object
                                                                        obj_name = ls_main_fields-obj_name
                                                                        id_text = ls_main_fields-id_text
                                                                        objtype = ls_main_fields-objtype.
              IF sy-subrc = 0.
* Me guardo la posicion donde se ha encontrado.
                ld_tabix = sy-tabix.
* Los registros de la tabla local y temporal siempre coinciden porque se añaden los mismos datos.
                READ TABLE <tbl> ASSIGNING <wa> INDEX ld_tabix.
              ELSE.
                CLEAR ld_tabix.
* Reasigno la cabecera para limpiar valores previos.
                ASSIGN mo_wa_data->* TO <wa>.
                APPEND ls_main_fields TO lt_main_fields.
* Paso los datos comunes a la cabecera de la tabla de datos
                MOVE-CORRESPONDING ls_main_fields TO <wa>.
              ENDIF.

* Construyo el campo donde se pondra el texto destino
              ld_field_text = get_name_field_text( <ls_tlang> ).
              ASSIGN COMPONENT ld_field_text OF STRUCTURE <wa> TO <field>.
              IF sy-subrc = 0.
                <field> = <ls_texts>-t_text.

* Si el objeto tiene propuesta de textos se rellena los datos con la propuesta del
* texto. Si no hay texto y hay propuesta se pone el de la propuesta.
* Hay objetos que no tienen propuesta de texto como los formularios.
                IF lo_text_object->has_proposed_text( ) = abap_true.
                  CALL METHOD proposal_text
                    EXPORTING
                      i_tlang        = <ls_tlang>
                      is_texts       = <ls_texts>
                      io_object_text = lo_text_object
                    CHANGING
                      cs_wa          = <wa>.
                ENDIF.

* Si el objeto e id de texto no esta en la tabla temporal, muevo los campos principales
* a la cabecera y añado los datos.
                IF ld_tabix IS INITIAL.
                  APPEND <wa> TO <tbl>.
                  CLEAR <wa>.
                ENDIF.

              ENDIF.

            ENDLOOP.

* Marco para que el texto de origen no se vuelva a pasar porque ya se ha hecho con el primer idioma.
            ld_primer_source = abap_false.

          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD save_data.
    FIELD-SYMBOLS <ls_mngt_text> TYPE LINE OF tt_mngt_text.


    CLEAR rs_return.

* Primero se actualizan los textos en la tabla interna donde contiene los objetos de textos
* de cada objeto.
    update_text_object( ).

* Se recorre la tabla donde contiene la gestion de los propios textos para grabarla.
* Eso si, solo aquellos que se han modificados.
    LOOP AT mt_mngt_text ASSIGNING <ls_mngt_text> WHERE data_changed = abap_true.

* Solo se graban los datos modificados.
      CALL METHOD <ls_mngt_text>-oobject->save_text
        EXCEPTIONS
          error_save = 1
          OTHERS     = 2.

* Cualquier error se almacena en a estructura de salida y se sale.
      IF sy-subrc NE 0.
        rs_return = fill_return( i_type = 'S'
                                i_number = '006'
                                i_message_v1 = <ls_mngt_text>-object
                                i_message_v2 = <ls_mngt_text>-obj_name ).
        EXIT.
      ENDIF.

    ENDLOOP.

* Si no hay errores en la grabación devuelvo un mensaje indicado del
* éxito de la operación.
    IF rs_return IS INITIAL.
      rs_return = fill_return( i_type = 'S'
                              i_number = '007' ).

* Se vuelven a leer los textos por dos motivos:
* 1) En la SE63, Un campo nuevo en la pantalla de seleccion de un programa que
* referencia al diccionario aparece a traducir. Pero una vez realizada la
* traducción "desaparece".
* 2) Ajuste de estilos en base a las propuestas.
* Sobretodo por el punto 1 y casos parecidos que puedan ocurrir, lo mejor es leer de nuevo
* y refrescar contenido.
      read_process_texts( ).

    ENDIF.

  ENDMETHOD.


  METHOD set_data.
    mo_it_data = it_data.
  ENDMETHOD.


  METHOD set_params_selscreen.

    mv_olang = iv_olang. " Idioma origen
    mt_tlang = it_tlang. " Idioma destino.
    mv_trkorr = iv_trkorr. " Orden de transporte
    " Nivel de búsqueda de objetos a traducir a partir del objeto principal-
    mv_depth_refs = iv_depth_refs.

  ENDMETHOD.


  METHOD transport_mod_obj.
    FIELD-SYMBOLS <ls_mngt_text> TYPE LINE OF tt_mngt_text.

    CLEAR es_return.

    IF mv_trkorr IS NOT INITIAL.

* Solo se transportan los objetos modificados
      LOOP AT mt_mngt_text ASSIGNING <ls_mngt_text> WHERE data_changed = abap_true.
* Una vez grabado en la orden de transporte quito la marca de modificado.
        <ls_mngt_text>-data_changed = abap_false.

* El transporte se realiza desde la propia clase que gestiona los textos.
        CALL METHOD <ls_mngt_text>-oobject->transport_translate
          EXPORTING
            iv_trkorr           = mv_trkorr
          EXCEPTIONS
            error_insert_trkorr = 1
            OTHERS              = 2.
        IF sy-subrc <> 0.
          es_return = fill_return( i_type = 'W'
                                  i_id = sy-msgid
                                  i_number = sy-msgno
                                  i_message_v1 = sy-msgv1
                                  i_message_v2 = sy-msgv2
                                  i_message_v3 = sy-msgv3
                                  i_message_v4 = sy-msgv4  ).
          EXIT.

        ENDIF.
      ENDLOOP.
      IF sy-subrc = 0.
* Si hay datos y no hay errores saco un mensaje informativo de que todo ha ido bien.
        IF es_return IS INITIAL.
          es_return = fill_return( i_type = 'S'
                                  i_number = '008'
                                  i_message_v1 = mv_trkorr ).

* Para evitar que se van acumulando los mismos objetos en la orden, llamo a la funcion
* que ordena y clasifica (elimina duplicados) de la orden/tarea pasada.
* NOTA: Creo que no es necesario porque al hacer pruebas no ha habido duplicado, pero
* lo dejo por si se meten entradas manuales o de otra forma haciendo duplicados.
          CALL FUNCTION 'TR_SORT_AND_COMPRESS_COMM'
            EXPORTING
              iv_trkorr                      = mv_trkorr
            EXCEPTIONS
              trkorr_not_found               = 1
              order_released                 = 2
              error_while_modifying_obj_list = 3
              tr_enqueue_failed              = 4
              no_authorization               = 5
              OTHERS                         = 6.

        ENDIF.
* Si no se han modificados datos también lo aviso.
      ELSE.
        es_return = fill_return( i_type = 'S'
                                i_number = '011' ).

      ENDIF.
    ELSE.
* Si no hay orden se devuelve un mensaje adviertiendolo
      es_return = fill_return( i_type = 'S'
                              i_number = '009' ).
    ENDIF.
  ENDMETHOD.


  METHOD update_text_object.
    FIELD-SYMBOLS <tbl> TYPE ANY TABLE.
    FIELD-SYMBOLS <wa> TYPE any.
    FIELD-SYMBOLS <field> TYPE any.
    FIELD-SYMBOLS <ls_tlang> TYPE any.
    FIELD-SYMBOLS <ls_mngt_text> TYPE LINE OF tt_mngt_text.
    DATA ls_main_fields TYPE ts_main_fields.
    DATA ld_fieldname TYPE fieldname.

    ASSIGN mo_it_data->* TO <tbl>.
    ASSIGN mo_wa_data->* TO <wa>.

    LOOP AT <tbl> ASSIGNING <wa>.

* Paso los datos a una estructura base para poder simplificar el codigo.
      MOVE-CORRESPONDING <wa> TO ls_main_fields.

* Por cada registro leo los lenguajes a traducir para ver cual de ellos ha sido modificado.
      LOOP AT mt_tlang ASSIGNING <ls_tlang>.

* Recupero el campo del control del idioma para ver si se ha modificado.
        ld_fieldname = get_name_field_ctrl( <ls_tlang> ).
        ASSIGN COMPONENT ld_fieldname OF STRUCTURE <wa> TO <field>.
        IF sy-subrc = 0.
          IF <field> = abap_true.

* Quito la marca de campo modificado para que no vuelva a entrar si se cambia otro campo.
            <field> = abap_false.

* Recupero el texto del idioma para informa a la tabla donde están los textos
            ld_fieldname = get_name_field_text( <ls_tlang> ).
            ASSIGN COMPONENT ld_fieldname OF STRUCTURE <wa> TO <field>.
            IF sy-subrc = 0.

* Busqueda de del objeto del texto
              READ TABLE mt_mngt_text ASSIGNING <ls_mngt_text>
                                      WITH TABLE KEY object = ls_main_fields-object
                                                     obj_name = ls_main_fields-obj_name
                                                     tlang = <ls_tlang>.
              IF sy-subrc = 0.

* Indico que los datos se han actualizado para después saber si se han de grabar
                <ls_mngt_text>-data_changed = abap_true.
* Actualizacion
                CALL METHOD <ls_mngt_text>-oobject->set_text
                  EXPORTING
                    iv_id_text         = ls_main_fields-id_text
                    iv_objtype         = ls_main_fields-objtype
                    iv_text            = <field>
                  EXCEPTIONS
                    id_text_dont_exist = 1
                    OTHERS             = 2.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
