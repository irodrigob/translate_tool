*&---------------------------------------------------------------------*
*&  Include           Z_CA02701_C01
*&---------------------------------------------------------------------*
CLASS lcl_event_alv DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,
    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,
    handle_menu_button
        FOR EVENT menu_button OF cl_gui_alv_grid
            IMPORTING e_object e_ucomm,
    handle_context_menu
        FOR EVENT context_menu_request OF cl_gui_alv_grid
            IMPORTING e_object.

ENDCLASS.                    "lcl_event_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_alv IMPLEMENTATION.
  METHOD handle_data_changed.
    PERFORM data_changed CHANGING er_data_changed.
  ENDMETHOD.                    "handle_data_changed

  METHOD handle_toolbar.
    PERFORM toolbar_alv CHANGING e_object e_interactive.
  ENDMETHOD.                    "handle_toolbar
  METHOD handle_user_command.
    perform user_command_toolbar CHANGING e_ucomm.
  ENDMETHOD.                    "handle_user_command
  METHOD handle_menu_button.
    PERFORM menu_toolbar CHANGING e_object e_ucomm.
  ENDMETHOD.                    "handle_menu_button
  METHOD handle_context_menu.
    PERFORM context_menu USING e_object.
  ENDMETHOD.                    "handle_context_menu
ENDCLASS.                    "lcl_event_alv IMPLEMENTATION
