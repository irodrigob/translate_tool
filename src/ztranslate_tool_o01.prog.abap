*----------------------------------------------------------------------*
***INCLUDE Z_CA02701_O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'P9000'.
  SET TITLEBAR 'T9000' WITH go_proces->d_object go_proces->d_obj_name.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ALV_VIEW  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alv_view OUTPUT.
  DATA ld_repid TYPE sy-repid.
  DATA ld_dynnr TYPE sy-dynnr.
  DATA lt_fcat TYPE lvc_t_fcat.

  IF go_container IS NOT BOUND.

    ld_repid = sy-repid.
    ld_dynnr = sy-dynnr.

    CREATE OBJECT go_container
      EXPORTING
        repid = ld_repid
        dynnr = ld_dynnr
        side  = cl_gui_docking_container=>dock_at_left
        ratio = 95.

* Creo el ALV
    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_container.

* Botones del ALV a excluir
    PERFORM buttons_exclude_alv.

* Layout
    et_layout-col_opt = abap_true.
    et_layout-cwidth_opt = abap_true.
    et_layout-stylefname = go_proces->dc_field_style.

* Catalogo de campos
    lt_fcat = go_proces->get_fcat( ).

* La verificación cuando se haga enter
    CALL METHOD go_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter. " mc_evt_modified

* Mostrar campos
    CALL METHOD go_alv->set_table_for_first_display
      EXPORTING
        i_bypassing_buffer   = 'X'
        is_layout            = et_layout
        is_variant           = et_variant
        it_toolbar_excluding = it_excluding
      CHANGING
        it_fieldcatalog      = lt_fcat
        it_outtab            = <it_datos>.
*        it_filter            = it_filters.

* Activo el estado de edicion del ALV
    CALL METHOD go_alv->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

* Activo el evento de validación de datos
    CREATE OBJECT go_event_receiver_alv.
    SET HANDLER go_event_receiver_alv->handle_data_changed FOR go_alv.
    SET HANDLER go_event_receiver_alv->handle_user_command FOR go_alv.
    SET HANDLER go_event_receiver_alv->handle_toolbar FOR go_alv.
    SET HANDLER go_event_receiver_alv->handle_context_menu FOR go_alv.
    SET HANDLER go_event_receiver_alv->handle_menu_button FOR go_alv.

* Lanzo el evento para construir/modificar la barra de herramientas del ALV
    go_alv->set_toolbar_interactive( ).

  ELSE.
    CALL METHOD go_alv->refresh_table_display( EXPORTING is_stable = et_stable ).
  ENDIF.
ENDMODULE.                 " ALV_VIEW  OUTPUT
