CLASS zcl_translate_cmp_messages DEFINITION
  PUBLIC
  INHERITING FROM zcl_translate_cmp
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_CMP_MESSAGES
*"* do not include other source files here!!!
  PUBLIC SECTION.

    METHODS get_components
        REDEFINITION .
    METHODS get_objects_type
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_CMP_MESSAGES
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF ts_message_key,
        message_id TYPE sy-msgid,
        message_nr TYPE t100-msgnr,
      END OF ts_message_key .
  PRIVATE SECTION.
*"* private components of class ZCL_TRANSLATE_CMP_MESSAGES
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_cmp_messages IMPLEMENTATION.


  METHOD get_components.
    FIELD-SYMBOLS <ls_t100> TYPE t100.
    DATA ls_message_key TYPE ts_message_key.
    DATA lt_t100 TYPE STANDARD TABLE OF t100.
    DATA ls_components TYPE LINE OF tt_components.
    DATA ld_masterlang TYPE masterlang.

    CLEAR et_components.

* Busco el idioma original de la clase de mensajes
    SELECT SINGLE masterlang INTO ld_masterlang
           FROM tadir
           WHERE object = mv_object
                 AND obj_name = mv_obj_name.

    IF sy-subrc = 0.

* Se buscan los mensajes en la tabla T100 de la clase de mensajes y con
* el idioma original.
      SELECT * INTO TABLE lt_t100
             FROM t100
             WHERE sprsl = ld_masterlang
                   AND arbgb = mv_obj_name.
      IF sy-subrc = 0.

* Se añade cada mensaje individual a la lista de objetos.
        LOOP AT lt_t100 ASSIGNING <ls_t100>.

* Paso a una estructura los datos para construir la clave.
          ls_message_key-message_id = mv_obj_name.
          ls_message_key-message_nr = <ls_t100>-msgnr.

* Añado el mensaje
          ls_components-object = 'MESS'. " Mensaje individual
          ls_components-obj_name = ls_message_key.
* El nombre interno será el mismo que el externo que se va.
          ls_components-obj_name_int = ls_components-obj_name.
          APPEND ls_components TO et_components.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_objects_type.
    APPEND 'MSAG' TO rt_objects.
  ENDMETHOD.
ENDCLASS.
