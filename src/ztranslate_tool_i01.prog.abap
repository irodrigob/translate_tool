*----------------------------------------------------------------------*
***INCLUDE Z_CA02701_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SALIR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE salir INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " SALIR  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA ld_valid TYPE sap_bool.
  CALL METHOD mo_alv->check_changed_data( IMPORTING e_valid = ld_valid ).
  IF ld_valid = abap_true.
    CASE d_okcode.
      WHEN 'BU'.
        PERFORM save_data.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
