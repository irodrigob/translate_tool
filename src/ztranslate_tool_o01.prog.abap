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
  SET TITLEBAR 'T9000' WITH mo_proces->mv_object mo_proces->mv_obj_name.

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

  IF mo_container IS NOT BOUND.

    ld_repid = sy-repid.
    ld_dynnr = sy-dynnr.

    CREATE OBJECT mo_container
      EXPORTING
        repid = ld_repid
        dynnr = ld_dynnr
        side  = cl_gui_docking_container=>dock_at_left
        ratio = 95.

* Creo el ALV
    CREATE OBJECT mo_alv
      EXPORTING
        i_parent = mo_container.

* Botones del ALV a excluir
    PERFORM buttons_exclude_alv.

* Layout
    ms_layout-col_opt = abap_true.
    ms_layout-cwidth_opt = abap_true.
    ms_layout-stylefname = mo_proces->mc_field_style.

* Catalogo de campos
    lt_fcat = mo_proces->get_fcat( ).

* La verificación cuando se haga enter
    CALL METHOD mo_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter. " mc_evt_modified

* Mostrar campos
    CALL METHOD mo_alv->set_table_for_first_display
      EXPORTING
        i_bypassing_buffer   = 'X'
        is_layout            = ms_layout
        is_variant           = ms_variant
        it_toolbar_excluding = mt_excluding
      CHANGING
        it_fieldcatalog      = lt_fcat
        it_outtab            = <it_datos>.
*        it_filter            = mt_filters.

* Activo el estado de edicion del ALV
    CALL METHOD mo_alv->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

* Activo el evento de validación de datos
    CREATE OBJECT mo_event_receiver_alv.
    SET HANDLER mo_event_receiver_alv->handle_data_changed FOR mo_alv.
    SET HANDLER mo_event_receiver_alv->handle_user_command FOR mo_alv.
    SET HANDLER mo_event_receiver_alv->handle_toolbar FOR mo_alv.
    SET HANDLER mo_event_receiver_alv->handle_context_menu FOR mo_alv.
    SET HANDLER mo_event_receiver_alv->handle_menu_button FOR mo_alv.

* Lanzo el evento para construir/modificar la barra de herramientas del ALV
    mo_alv->set_toolbar_interactive( ).

  ELSE.
    CALL METHOD mo_alv->refresh_table_display( EXPORTING is_stable = ms_stable ).
  ENDIF.
ENDMODULE.                 " ALV_VIEW  OUTPUT
