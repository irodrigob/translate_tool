*----------------------------------------------------------------------*
***INCLUDE Z_CA02701_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EVT_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM evt_initialization .
  DATA ls_allowed_object LIKE LINE OF mt_allowed_object.

* Se instancia la clase que gestionara todo la herramientra de traduccion.
  CREATE OBJECT mo_proces.

* Se recupera los objetos permitidos en base a las clases de componentes
* parametrizadas.
  mt_allowed_object = mo_proces->get_allowed_objects( ).

* Idioma fuente por defecto
  CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
    EXPORTING
      r3_lang    = sy-langu
    IMPORTING
      o_language = p_olang.

* Indico que los refrescos de los ALV no se moverán de filas y columna
  ms_stable-row = abap_true.
  ms_stable-col = abap_true.

* Nombre del programa para variantes.
  ms_variant-report = sy-cprog.
ENDFORM.                    " EVT_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  F4_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_object .
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJECT'
      window_title    = 'Allowed Plugins'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'P_OBJECT'
      value_org       = 'S'
    TABLES
      value_tab       = mt_allowed_object
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.                    " F4_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CHECK_BL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_bl1 .
  IF p_object IS INITIAL.
    MESSAGE e001.
  ELSE.
* Compruebo si el objeto es valido
    READ TABLE mt_allowed_object TRANSPORTING NO FIELDS
                                 WITH KEY object = p_object.
    IF sy-subrc = 0.
* Compruebo si el objeto existe
      IF mo_proces->check_obj_2_trans( iv_object = p_object iv_obj_name = p_oname ) = abap_false.
        MESSAGE e004 WITH p_object p_oname.
      ENDIF.
    ELSE.
      MESSAGE e012 WITH p_object.
    ENDIF.

  ENDIF.
ENDFORM.                    " CHECK_BL1
*&---------------------------------------------------------------------*
*&      Form  LOAD_OBJECT_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM load_object_texts .

* Se leen los textos del objeto y sus componentes.
  mo_proces->load_object_texts( ).

* Se recuperan el texto del objeto con sus componentes.
  mo_it_data = mo_proces->get_data( ).
  ASSIGN mo_it_data->* TO <it_datos>.

ENDFORM.                    " LOAD_OBJECT_TEXTS
*&---------------------------------------------------------------------*
*&      Form  F4_OLANG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_olang .
  CALL FUNCTION 'LXE_T002_SELECT_LANGUAGE'
    EXPORTING
      dynpprog        = 'Z_CA02701'
      dynpnr          = '1000'
      dynpfield       = 'P_OLANG'
    IMPORTING
      language        = p_olang
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      no_selection    = 3
      OTHERS          = 4.

ENDFORM.                    " F4_OLANG
*&---------------------------------------------------------------------*
*&      Form  PBO_PS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pbo_ps .
* Texto informativo para el campo de niveles de profundidad.
  text1 = TEXT-i01.

  LOOP AT SCREEN.
    CASE screen-name.
* El idioma de origen, objeto a traducir y la orden se desactiva la entrada de datos.
* Se usará el matchcode para hacerlo.
* Se hace para que usen la ayuda para búsqueda y no se introduzcan cosas que no deben.
      WHEN 'P_OLANG' OR 'P_TRKORR'.
        screen-input = 0.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " PBO_PS
*&---------------------------------------------------------------------*
*&      Form  F4_SPRAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_spras .

  CALL FUNCTION 'LXE_T002_SELECT_LANGUAGE'
    EXPORTING
      dynpprog        = 'Z_CA02701'
      dynpnr          = '1000'
      dynpfield       = 'S_TLANG-LOW'
    IMPORTING
      language        = s_tlang-low
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      no_selection    = 3
      OTHERS          = 4.

ENDFORM.                    " F4_SPRAS
*&---------------------------------------------------------------------*
*&      Form  check_tlang
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_tlang.

* Se borra el idioma de origen en el ranges de idiomas de destino.
* no tiene sentido poner los mismos.
  DELETE s_tlang WHERE low = p_olang.

* La búsqueda por patrones no esta permitida, y no se puede marcar la opción
* de exclusion.
  LOOP AT s_tlang TRANSPORTING NO FIELDS WHERE option = 'CP'
                                               OR sign = 'E'.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE e002.
  ELSE.

    IF s_tlang-low IS NOT INITIAL.
      CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
        EXPORTING
          language           = s_tlang-low
        EXCEPTIONS
          language_not_in_cp = 1
          unknown            = 2
          OTHERS             = 3.
      IF sy-subrc > 1.
        MESSAGE e003(lxe_trans) WITH s_tlang-low. " Idioma no existe.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "check_tlang
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMS_SELSCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_params_selscreen .
  DATA lt_tlang TYPE lxe_tt_lxeisolang.

* El ranges de los idiomas se pasa a una tabla normal
  LOOP AT s_tlang.
    APPEND s_tlang-low TO lt_tlang.
  ENDLOOP.

* Se eliminan posibles duplicados
  SORT lt_tlang.
  DELETE ADJACENT DUPLICATES FROM lt_tlang.

  CALL METHOD mo_proces->set_params_selscreen
    EXPORTING
      iv_olang      = p_olang
      it_tlang      = lt_tlang
      iv_trkorr     = p_trkorr
      iv_depth_refs = p_drefs.

ENDFORM.                    " SET_PARAMS_SELSCREEN
*&---------------------------------------------------------------------*
*&      Form  SHOW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_data .
  CALL SCREEN 9000.
ENDFORM.                    " SHOW_DATA
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed  CHANGING ps_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  FIELD-SYMBOLS <ls_modif> TYPE lvc_s_modi.
  FIELD-SYMBOLS <wa> TYPE any.
  FIELD-SYMBOLS <field> TYPE any.
  FIELD-SYMBOLS <lt_field_style> TYPE lvc_t_styl.
  FIELD-SYMBOLS <ls_field_style> TYPE LINE OF lvc_t_styl.
  DATA ld_fieldname TYPE fieldname.
  DATA ls_field_style TYPE LINE OF lvc_t_styl.


  LOOP AT ps_data_changed->mt_mod_cells ASSIGNING <ls_modif>.
* En el primer registro se marca que los datos han sido modificados
    AT FIRST.
      mv_datos_modif = abap_true.
    ENDAT.
* Registro cada vez que se modifica una fila.
    AT NEW row_id.
* Leo el registro modificado.
      READ TABLE <it_datos> ASSIGNING <wa> INDEX <ls_modif>-row_id.
    ENDAT.

* Paso el texto modificado al campo que toca.
    ASSIGN COMPONENT <ls_modif>-fieldname OF STRUCTURE <wa> TO <field>.
    IF sy-subrc = 0.
      <field> = <ls_modif>-value.
    ENDIF.

* Pongo el nombre del campo en una variable para despues cambiarlo para actualizar
* el campo de control.
    ld_fieldname = <ls_modif>-fieldname.

* Para actualizar el campo de control tengo que reemplazar el texto del nombre del campo
* por el de control.
    REPLACE mo_proces->cs_fields_itab-txt_lang IN ld_fieldname
            WITH mo_proces->cs_fields_itab-ctrl_lang.

* Actualizo el campo de control
    ASSIGN COMPONENT ld_fieldname OF STRUCTURE <wa> TO <field>.
    IF sy-subrc = 0.
      <field> = abap_true.
    ENDIF.

* Cambio el color del campo de texto. No se puede actualizar directamente el estilo
* en la tabla porque ira bien para el primer ENTER. Pero si luego se modifica y se vuelve
* a pulsar ENTER se pierde el estilo de la primera modificacion. Vamos, que solo
* hay color para los últimos cambios.
    CALL METHOD ps_data_changed->modify_style
      EXPORTING
        i_row_id    = <ls_modif>-row_id
        i_fieldname = <ls_modif>-fieldname
        i_style     = mo_proces->mc_style_text_changed.

  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  BUTTONS_EXCLUDE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM buttons_exclude_alv .
*mt_excluding
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO mt_excluding.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo  TO mt_excluding.
ENDFORM.                    " BUTTONS_EXCLUDE_ALV
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_data.
  DATA ls_return TYPE bapiret2.

  IF mv_datos_modif = abap_true.

* Se pasa los datos a la clase de control
    mo_proces->set_data( mo_it_data ).

* Se graban los datos
    ls_return = mo_proces->save_data( ).

    CASE ls_return-type.
* Los mensajes de error se muestran como informativos pero de tipo error.
      WHEN 'E' OR 'X' OR 'A'.
        MESSAGE ID ls_return-id TYPE 'S'
                       NUMBER ls_return-number
                       WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
                       DISPLAY LIKE 'E'.
      WHEN OTHERS.
* Una vez grabado sin error se marca que los datos no están modificados.
        mv_datos_modif = abap_false.

        MESSAGE ID ls_return-id TYPE ls_return-type
                       NUMBER ls_return-number
                       WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ENDCASE.

* Refresco los datos para que se van los cambios en la tabla interna de datos.
* No es necesario recuperar los datos porque ya se han hecho con anterioridad a través
* de punteros. Por lo que todo se haga en la clase o programa se ve reflejando en ambos sitios
* sin necesidad de traspasar de nuevo los datos. Viva los punteros :-)
    CALL METHOD mo_alv->refresh_table_display( EXPORTING is_stable = ms_stable ).

  ELSE.
    MESSAGE s010.
  ENDIF.

ENDFORM.                    "save_data
*&---------------------------------------------------------------------*
*&      Form  F4_TRKORR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_trkorr .
  DATA ld_order TYPE trkorr.
  DATA ld_task TYPE trkorr.
  DATA ld_trkorr TYPE trkorr.
  DATA lt_dyntab TYPE STANDARD TABLE OF dynpread.
  DATA ls_dyntab TYPE dynpread.

  CALL FUNCTION 'TR_ORDER_CHOICE_CORRECTION'
    EXPORTING
      iv_category = 'SYST'
    IMPORTING
      ev_order    = ld_order
      ev_task     = ld_task
    EXCEPTIONS
      OTHERS      = 3.
  IF sy-subrc = 0.

* La tarea si, se devuelve, tiene más prioridad. Esto se hace para
* operacion en la orden que se harán al transportar (como clasificar y comprimir).
    IF ld_task IS NOT INITIAL.
      ld_trkorr = ld_task.
    ELSEIF ld_order IS NOT INITIAL.
      ld_trkorr = ld_order.
    ENDIF.

* Si hay orden actualizo el campo de la pantalla.
    IF ld_trkorr IS NOT INITIAL.
      ls_dyntab-fieldname = 'P_TRKORR'.
      ls_dyntab-fieldvalue = ld_trkorr.
      APPEND ls_dyntab TO lt_dyntab.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = 'Z_CA02701'
          dynumb               = '1000'
        TABLES
          dynpfields           = lt_dyntab
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          undefind_error       = 7
          OTHERS               = 8.
    ENDIF.

  ENDIF.
ENDFORM.                    " F4_TRKORR
*&---------------------------------------------------------------------*
*&      Form  CHECK_TRKORR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_trkorr .
  FIELD-SYMBOLS <ls_req_head> TYPE trwbo_request_header.
  DATA ld_trfunction TYPE e070-trfunction.
  DATA lt_req_head TYPE trwbo_request_headers.
  DATA lt_req TYPE trwbo_requests.
  DATA ld_order TYPE e070-trkorr.

  IF p_trkorr IS NOT INITIAL.
    mo_proces->get_task_from_order( EXPORTING iv_order = p_trkorr
                                    IMPORTING es_return = DATA(ls_return)
                                              ev_task = p_trkorr ).
    IF ls_return IS NOT INITIAL.
      MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
    ENDIF.
** Se las tareas de las ordenes para ver si hay alguna valida.
*    CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
*      EXPORTING
*        iv_trkorr          = p_trkorr
*      IMPORTING
*        et_request_headers = lt_req_head
*        et_requests        = lt_req
*      EXCEPTIONS
*        invalid_input      = 1
*        OTHERS             = 2.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
** Se mira si hay alguna tarea valida para el usuario.
*    LOOP AT lt_req_head ASSIGNING <ls_req_head> WHERE trfunction = 'S'
*                                                      AND trstatus = 'D'
*                                                      AND as4user = sy-uname.
*      EXIT.
*    ENDLOOP.
*    IF sy-subrc = 0.
*      p_trkorr = <ls_req_head>-trkorr.
*    ELSE.
** En caso de no haberla se lee la primera posicion se indique la orden padre para leer
** algunos datos y crear la tarea.
*      LOOP AT lt_req_head ASSIGNING <ls_req_head> WHERE strkorr IS NOT INITIAL.
*        EXIT.
*      ENDLOOP.
*      IF sy-subrc = 0.
*
*        CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
*          EXPORTING
*            wi_kurztext       = <ls_req_head>-as4text
*            wi_trfunction     = 'S'
*            iv_username       = sy-uname
*            wi_strkorr        = <ls_req_head>-strkorr
*            wi_client         = sy-mandt
*          IMPORTING
*            we_trkorr         = p_trkorr
*          EXCEPTIONS
*            no_systemname     = 1
*            no_systemtype     = 2
*            no_authorization  = 3
*            db_access_error   = 4
*            file_access_error = 5
*            enqueue_error     = 6
*            number_range_full = 7
*            invalid_input     = 8
*            OTHERS            = 9.
*        IF sy-subrc <> 0.
*          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  ENDIF.

ENDFORM.                    " CHECK_TRKORR
*&---------------------------------------------------------------------*
*&      Form  TOOLBAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toolbar_alv CHANGING ps_object TYPE REF TO cl_alv_event_toolbar_set
                          ps_interactive TYPE char01.
  DATA ls_toolbar  TYPE stb_button.

* Se añade un separador para separar los botones estándar de los propios.
  CLEAR ls_toolbar.
  MOVE 3 TO ls_toolbar-butn_type.
  APPEND ls_toolbar TO ps_object->mt_toolbar.

* Boton de transporte
  CLEAR ls_toolbar.
  MOVE mc_transport TO ls_toolbar-function.
  MOVE icon_transport TO ls_toolbar-icon.
  MOVE TEXT-q01 TO ls_toolbar-quickinfo.
  MOVE 1 TO ls_toolbar-butn_type. " Menu + opcion por defecto
  IF p_trkorr IS NOT INITIAL.
    MOVE space TO ls_toolbar-disabled.
  ELSE.
    MOVE abap_true TO ls_toolbar-disabled .
  ENDIF.
  APPEND ls_toolbar TO ps_object->mt_toolbar.

* Botón de confirmar traducciones
  CLEAR ls_toolbar.
  MOVE mc_pprosal TO ls_toolbar-function.
*  MOVE icon_transport TO ls_toolbar-icon.
*  MOVE text-q02 TO ls_toolbar-quickinfo.
  MOVE TEXT-q02 TO ls_toolbar-text.
  MOVE 1 TO ls_toolbar-butn_type. " Menu + opcion por defecto
  APPEND ls_toolbar TO ps_object->mt_toolbar.

ENDFORM.                    " TOOLBAR_ALV
*&---------------------------------------------------------------------*
*&      Form  MENU_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM menu_toolbar  CHANGING ps_object TYPE REF TO cl_ctmenu
                            ps_ucomm TYPE sy-ucomm.

* Se desplega el menú en base a la opción escogida.
  CASE ps_ucomm.
    WHEN mc_transport.
      ps_object->add_function( fcode = mc_transport
                               text = TEXT-t01 " Transport changes
                               icon =  icon_import_transport_request ).

      ps_object->add_function( fcode = mc_trans_obj
                               text = TEXT-t02 " Selective transport
                               icon  = icon_transport_proposal
                               disabled = abap_true ).
    WHEN mc_pprosal.
      ps_object->add_function( fcode = mc_pprosal
                             text = TEXT-t03 " Confirm all
                             icon =  icon_import_transport_request ).

      ps_object->add_function( fcode = mc_ok_pprosal
                               text = TEXT-f01 " Confirm proposal field
                               icon  = icon_transport_proposal ).
  ENDCASE.
ENDFORM.                    " MENU_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_toolbar  CHANGING ps_ucomm TYPE sy-ucomm.

  CASE ps_ucomm.
    WHEN mc_transport. " Transport all objects
      PERFORM transport_mod_obj.
    WHEN mc_trans_obj. " Selective transport
    WHEN mc_ok_pprosal. " Confirm proposal
      PERFORM button_confirm_pprosal.
    WHEN mc_pprosal. " Confirm all proposal
      PERFORM confirm_all_pprosal.
  ENDCASE.

ENDFORM.                    " USER_COMMAND_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  TRANSPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM transport_mod_obj .
  DATA ls_return TYPE bapiret2.

  CALL METHOD mo_proces->transport_mod_obj
    IMPORTING
      es_return = ls_return.

  CASE ls_return-type.
* Los errores se muestran como informativo pero de tipo error, de esta
* manera no se sale del programa.
    WHEN 'E' OR 'X' OR 'A'.
      MESSAGE ID ls_return-id TYPE 'S' NUMBER ls_return-number
             WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
             DISPLAY LIKE 'E'.
    WHEN OTHERS.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
           WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
  ENDCASE.

ENDFORM.                    " TRANSPORT
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM init_data .
* Los datos no se han modificado.
  mv_datos_modif = abap_false.
ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  CONTEXT_MENU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM context_menu  USING  pe_object TYPE REF TO cl_ctmenu.
  FIELD-SYMBOLS <wa> TYPE any.
  FIELD-SYMBOLS <field_style> TYPE lvc_t_styl.
  DATA ls_col TYPE lvc_s_col.
  DATA ls_row TYPE lvc_s_row.
  DATA ld_mostrar_menu TYPE sap_bool.
* Códigos de funcion para menu contextual
  DATA lt_func_menu_context TYPE ui_functions.
* Opciones de menu contextual estándar que no se mostrarán
  DATA lt_func_std_menu_context TYPE ui_functions.

* Carga de las opciones de los menus contextuales.
  PERFORM load_func_menu_context CHANGING lt_func_menu_context
                                          lt_func_std_menu_context.

* Obtengo los datos de la columna donde se ha pulsado el menú contextual.
  CALL METHOD mo_alv->get_current_cell
    IMPORTING
      es_col_id = ls_col
      es_row_id = ls_row.
  CALL METHOD cl_gui_cfw=>flush.

  ld_mostrar_menu = abap_false.

* El menu contextual solo se permite para las columnas dinámicas con los textos
  IF ls_col-fieldname CS zcl_spt_translate_tool=>cs_fields_itab-txt_lang.

* El menú contextual saldrá
    READ TABLE <it_datos> ASSIGNING <wa> INDEX ls_row-index.
    IF sy-subrc = 0.

      ASSIGN COMPONENT zcl_spt_translate_tool=>mc_field_style OF STRUCTURE <wa> TO <field_style>.
      IF sy-subrc = 0.

* Miro el estilo es de propuesta no confirmada. Si es así se muestra el menu.
        READ TABLE <field_style> TRANSPORTING NO FIELDS
                                  WITH KEY fieldname = ls_col-fieldname
                                           style = mo_proces->mc_style_prop_wo_conf.
        IF sy-subrc = 0.
          ld_mostrar_menu = abap_true.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.

  IF ld_mostrar_menu = abap_true.
* Añade las opciones al menu contextual
    PERFORM assign_func_menu_context USING pe_object.

* Enseño los códigos funcion
    CALL METHOD pe_object->show_functions
      EXPORTING
        fcodes = lt_func_menu_context.

* Oculto los estándar
    CALL METHOD pe_object->hide_functions
      EXPORTING
        fcodes = lt_func_std_menu_context.
  ELSE.
* Enseño los códigos funcion
    CALL METHOD pe_object->hide_functions
      EXPORTING
        fcodes = lt_func_std_menu_context.

* Oculto los estándar
    CALL METHOD pe_object->show_functions
      EXPORTING
        fcodes = lt_func_std_menu_context.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " CONTEXT_MENU
*&---------------------------------------------------------------------*
*&      Form  LOAD_FUNC_MENU_CONTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM load_func_menu_context  CHANGING ps_func TYPE ui_functions
                                      ps_func_std TYPE ui_functions.

* Confirmacion propuesta traduccion
  APPEND mc_ok_pprosal TO ps_func.

* Codigo estándar para que no se muestren.
  APPEND cl_gui_alv_grid=>mc_fc_col_optimize TO ps_func_std.
  APPEND cl_gui_alv_grid=>mc_fc_help TO ps_func_std.
  APPEND cl_gui_alv_grid=>mc_fc_col_invisible TO ps_func_std.
  APPEND cl_gui_alv_grid=>mc_fc_delete_filter TO ps_func_std.

ENDFORM.                    " LOAD_FUNC_MENU_CONTEXT
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_FUNC_MENU_CONTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assign_func_menu_context  USING  pe_object TYPE REF TO cl_ctmenu.

  pe_object->add_separator( ).

  CALL METHOD pe_object->add_function
    EXPORTING
      fcode = mc_ok_pprosal
      text  = TEXT-f01.

ENDFORM.                    " ASSIGN_FUNC_MENU_CONTEXT
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_PPROSAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM confirm_pprosal_field USING pe_fieldname
                            CHANGING ps_wa TYPE any.
  FIELD-SYMBOLS <wa> TYPE any.
  FIELD-SYMBOLS <field> TYPE any.
  FIELD-SYMBOLS <field_style> TYPE lvc_t_styl.
  FIELD-SYMBOLS <wa_field_style> TYPE LINE OF lvc_t_styl.
  DATA ld_fieldname TYPE fieldname.

  ASSIGN COMPONENT zcl_spt_translate_tool=>mc_field_style OF STRUCTURE ps_wa TO <field_style>.
  IF sy-subrc = 0.

* Miro el estilo es de propuesta no confirmada. Si es así se cambia a confirmada
    READ TABLE <field_style> ASSIGNING <wa_field_style>
                              WITH KEY fieldname = pe_fieldname
                                       style = mo_proces->mc_style_prop_wo_conf.
    IF sy-subrc = 0.
      <wa_field_style>-style = mo_proces->mc_style_text_changed.

* Se marca que el campo ha sido modificado para que se tenga en cuenta en la grabacion.
      ld_fieldname = pe_fieldname.
* Para actualizar el campo de control tengo que reemplazar el texto del nombre del campo
* por el de control.
      REPLACE mo_proces->cs_fields_itab-txt_lang IN ld_fieldname
              WITH mo_proces->cs_fields_itab-ctrl_lang.

* Actualizo el campo de control
      ASSIGN COMPONENT ld_fieldname OF STRUCTURE ps_wa TO <field>.
      IF sy-subrc = 0.
        <field> = abap_true.

* A nivel global establezco que los datos han sido modificados.
        mv_datos_modif = abap_true.

      ENDIF.

    ENDIF.

  ENDIF.


ENDFORM.                    " CONFIRM_PPROSAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  BUTTON_CONFIRM_PPROSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM button_confirm_pprosal .
  FIELD-SYMBOLS <wa> TYPE any.
  DATA ls_col TYPE lvc_s_col.
  DATA ls_row TYPE lvc_s_row.

* Obtengo los datos de la columna donde se ha pulsado el menú contextual.
  CALL METHOD mo_alv->get_current_cell
    IMPORTING
      es_col_id = ls_col
      es_row_id = ls_row.
  CALL METHOD cl_gui_cfw=>flush.


* El menú contextual saldrá
  READ TABLE <it_datos> ASSIGNING <wa> INDEX ls_row-index.
  IF sy-subrc = 0.
    PERFORM confirm_pprosal_field USING ls_col-fieldname
                                  CHANGING <wa>.

    CALL METHOD mo_alv->refresh_table_display( EXPORTING is_stable = ms_stable ).

    CALL METHOD cl_gui_cfw=>flush.

  ENDIF.

ENDFORM.                    " BUTTON_CONFIRM_PPROSAL
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_ALL_PPROSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM confirm_all_pprosal .
  FIELD-SYMBOLS <wa> TYPE any.
  FIELD-SYMBOLS <ls_tlang> TYPE LINE OF lxe_tt_lxeisolang.
  DATA lt_tlang TYPE lxe_tt_lxeisolang.
  DATA ld_fieldname TYPE fieldname.

* Recupero los idiomas de destino.
  lt_tlang = mo_proces->get_tlang( ).

* Por cada registro se recorre cada columna de idioma y se llama al
* procedimiento que acepta las propuestas por cada idioma.
  LOOP AT <it_datos> ASSIGNING <wa>.

    LOOP AT lt_tlang ASSIGNING <ls_tlang>.

      ld_fieldname = mo_proces->get_name_field_text( <ls_tlang> ).
      PERFORM confirm_pprosal_field USING ld_fieldname
                                   CHANGING <wa>.

    ENDLOOP.

  ENDLOOP.

  CALL METHOD mo_alv->refresh_table_display( EXPORTING is_stable = ms_stable ).

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " CONFIRM_ALL_PPROSAL
