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
CONSTANTS mc_ok_pprosal TYPE ui_func VALUE 'OK_PPROSAL'.

* Confirmar propuestas
CONSTANTS mc_pprosal TYPE ui_func VALUE 'PPROSAL'.

* Transport data modified
CONSTANTS mc_transport TYPE ui_func VALUE 'TRANSPORT'.

* Selective transport
CONSTANTS mc_trans_obj TYPE ui_func VALUE 'TRANS_OBJ'.

*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
DATA mo_proces TYPE REF TO zcl_spt_translate_tool. " Clase que gestiona el proceso
DATA mo_it_data TYPE REF TO data. " Tabla interna con los datos.
FIELD-SYMBOLS <it_datos> TYPE table. " Puntero a la tabla interna de datos
DATA mv_datos_modif TYPE sap_bool. " Controla si los datos han cambiado.

*----------------------------------------------------------------------*
* Internal tables
*----------------------------------------------------------------------*
* Objetos que pueden ser traducidos
DATA mt_allowed_object TYPE tr_object_texts.

*----------------------------------------------------------------------*
* Declaración para los ALV
*----------------------------------------------------------------------*

* Catalogo de campos de la vista
DATA mt_fieldcat TYPE lvc_t_fcat.

* Catalogo de campos clave de la vista
DATA mt_fieldcat_key TYPE lvc_t_fcat.

* Lista de campos para las ordenes de transporte
DATA mt_fieldlist TYPE ddfields.

* Funciones de la barra del ALV que se excluiran
DATA mt_excluding TYPE ui_functions.
DATA ms_stable TYPE lvc_s_stbl.
DATA ms_layout TYPE lvc_s_layo.
DATA ms_variant TYPE disvariant.
DATA mt_filters TYPE lvc_t_filt.
DATA mo_alv TYPE REF TO cl_gui_alv_grid.
DATA mo_container TYPE REF TO cl_gui_docking_container.
CLASS lcl_event_alv DEFINITION DEFERRED.
DATA mo_event_receiver_alv TYPE REF TO lcl_event_alv.
DATA d_okcode TYPE syucomm.
