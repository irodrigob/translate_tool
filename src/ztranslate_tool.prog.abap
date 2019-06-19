*&---------------------------------------------------------------------*
*& Report  ZTRANS_TOOL
*&
*&---------------------------------------------------------------------*
*& Description: tool for translate objects
*& Author: Iván Rodrigo(Stratesys) 23/12/2013
*&---------------------------------------------------------------------*

REPORT  ztranslate_tool MESSAGE-ID ztranslate_tool.

INCLUDE ztranslate_tool_top.
INCLUDE ztranslate_tool_c01.


*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-bl1.
PARAMETERS p_object TYPE trobjtype OBLIGATORY.
PARAMETERS p_oname TYPE tadir-obj_name.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-bl2.
PARAMETERS p_olang TYPE lxe_log-targlng OBLIGATORY.
SELECT-OPTIONS: s_tlang FOR lxe_log-targlng OBLIGATORY NO INTERVALS.
SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-bl3.
PARAMETERS p_trkorr TYPE e070-trkorr.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(49) text1.
PARAMETERS p_drefs TYPE i DEFAULT 2.
SELECTION-SCREEN END OF BLOCK bl3.

*----------------------------------------------------------------------*
* initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM evt_initialization.

*----------------------------------------------------------------------*
* Events of screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_object.
  PERFORM f4_object.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_olang.
  PERFORM f4_olang.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tlang-low.
  PERFORM f4_spras.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_trkorr.
  PERFORM f4_trkorr.

AT SELECTION-SCREEN ON s_tlang. " validacion idioma destino
  PERFORM check_tlang.

AT SELECTION-SCREEN ON BLOCK bl1.
  PERFORM check_bl1.

AT SELECTION-SCREEN ON BLOCK bl3.
  PERFORM check_trkorr.

AT SELECTION-SCREEN OUTPUT.
  PERFORM pbo_ps.

*----------------------------------------------------------------------*
* Select data
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Inicializacion de datos
  PERFORM init_data.

* Se transfieren los datos de la pantalla de seleccion a la clase
  PERFORM set_params_selscreen.

* Lectura de textos del objeto
  PERFORM load_object_texts.

*----------------------------------------------------------------------*
* Display data
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF <it_datos> IS NOT INITIAL.
    PERFORM show_data.
  ELSE.
    MESSAGE s005.
  ENDIF.

  INCLUDE ztranslate_tool_f01.
  INCLUDE ztranslate_tool_o01.
  INCLUDE ztranslate_tool_i01.
