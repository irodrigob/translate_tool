*&---------------------------------------------------------------------*
*&  Include           Z_CA02701_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Dictionary tables
*----------------------------------------------------------------------*
TABLES: tadir, t002, lxe_log.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPE-POOLS: trwbo, icon, abap.

*----------------------------------------------------------------------*
*   Constants
*----------------------------------------------------------------------*
* Se acepta propuesta
CONSTANTS dc_ok_pprosal TYPE ui_func VALUE 'OK_PPROSAL'.

* Confirmar propuestas
CONSTANTS dc_pprosal TYPE ui_func VALUE 'PPROSAL'.

* Transport data modified
CONSTANTS dc_transport TYPE ui_func VALUE 'TRANSPORT'.

* Selective transport
CONSTANTS dc_trans_obj TYPE ui_func VALUE 'TRANS_OBJ'.

*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
DATA go_proces TYPE REF TO ZCL_TRANSLATE_TOOL. " Clase que gestiona el proceso
DATA go_it_data TYPE REF TO data. " Tabla interna con los datos.
FIELD-SYMBOLS <it_datos> TYPE table. " Puntero a la tabla interna de datos
DATA d_datos_modif TYPE sap_bool. " Controla si los datos han cambiado.

*----------------------------------------------------------------------*
* Internal tables
*----------------------------------------------------------------------*
* Objetos que pueden ser traducidos
DATA it_allowed_object TYPE tr_object_texts.

*----------------------------------------------------------------------*
* Declaración para los ALV
*----------------------------------------------------------------------*

* Catalogo de campos de la vista
DATA it_fieldcat TYPE lvc_t_fcat.

* Catalogo de campos clave de la vista
DATA it_fieldcat_key TYPE lvc_t_fcat.

* Lista de campos para las ordenes de transporte
DATA it_fieldlist TYPE ddfields.

* Funciones de la barra del ALV que se excluiran
DATA it_excluding TYPE ui_functions.
DATA et_stable TYPE lvc_s_stbl.
DATA et_layout TYPE lvc_s_layo.
DATA et_variant TYPE disvariant.
DATA it_filters TYPE lvc_t_filt.
DATA go_alv TYPE REF TO cl_gui_alv_grid.
DATA go_container TYPE REF TO cl_gui_docking_container.
CLASS lcl_event_alv DEFINITION DEFERRED.
DATA go_event_receiver_alv TYPE REF TO lcl_event_alv.
DATA d_okcode TYPE syucomm.
