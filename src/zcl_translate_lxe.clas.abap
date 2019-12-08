*----------------------------------------------------------------------*
*       CLASS ZTRANSLATE_LXE DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_translate_lxe DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class ZCL_TRANSLATE_LXE
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS abap .


    TYPES:
      BEGIN OF ts_lxe_list,
        object TYPE trobjtype,
        class  TYPE seoclsname,
      END OF ts_lxe_list .
    TYPES:
      tt_lxe_list TYPE STANDARD TABLE OF ts_lxe_list .
    TYPES: BEGIN OF ts_texts.
        INCLUDE TYPE lxe_pcx_s1.
    TYPES: objtype TYPE  lxeobjtype,
           END OF ts_texts.
    TYPES:
      tt_texts TYPE STANDARD TABLE OF ts_texts .

    METHODS get_texts
      EXPORTING
        VALUE(et_texts) TYPE tt_texts .
    METHODS set_text
      IMPORTING
        !iv_id_text TYPE lxetextkey
        !iv_objtype TYPE lxeobjtype
        !iv_text    TYPE any
      EXCEPTIONS
        id_text_dont_exist .
    METHODS save_text
      EXCEPTIONS
        error_save .
    METHODS transport_translate
      IMPORTING
        !iv_trkorr TYPE trkorr
      EXCEPTIONS
        error_insert_trkorr .
    METHODS get_best_text_proposal
      IMPORTING
        !iv_textkey   TYPE lxetextkey
        !iv_objtype   TYPE lxeobjtype
      EXPORTING
        !ev_best_text TYPE any .
    METHODS is_text_in_proposal
      IMPORTING
        !iv_text        TYPE any
        !iv_textkey     TYPE lxetextkey
        !iv_objtype     TYPE lxeobjtype
      RETURNING
        VALUE(rv_exist) TYPE sap_bool .
    CLASS-METHODS get_lxelist
      EXPORTING
        !et_lxe_list TYPE tt_lxe_list .
    METHODS get_lxe_type
      RETURNING
        VALUE(rt_objects) TYPE objtyptable .
    METHODS set_check_params
      IMPORTING
        !iv_object   TYPE trobjtype
        !iv_obj_name TYPE sobj_name
        !iv_olang    TYPE lxeisolang
        !iv_tlang    TYPE lxeisolang
      EXCEPTIONS
        object_not_valid .
    METHODS load_text .
    METHODS has_proposed_text
      RETURNING
        VALUE(rv_has) TYPE sap_bool .
  PROTECTED SECTION.
*"* protected components of class ZCL_TRANSLATE_LXE
*"* do not include other source files here!!!
    TYPES:
      BEGIN OF ts_proposal.
        INCLUDE TYPE lxe_pcx_s2.
    TYPES: objtype TYPE lxeobjtype,
           END OF ts_proposal .
    TYPES:
      tt_proposal TYPE STANDARD TABLE OF ts_proposal .
    TYPES:
      tt_colob TYPE STANDARD TABLE OF lxe_colob .
    TYPES:
      tt_pcx_s1 TYPE STANDARD TABLE OF lxe_pcx_s1 .
    TYPES:
      tt_pcx_s2 TYPE STANDARD TABLE OF lxe_pcx_s2 .

    CONSTANTS cv_status_proposal TYPE lxepp_stat VALUE 09.  "#EC NOTEXT
    DATA mv_object_text TYPE trobjtype .
    DATA mv_object TYPE trobjtype .
    DATA mv_obj_name TYPE sobj_name .
    DATA mv_olang TYPE lxeisolang .
    DATA mv_tlang TYPE lxeisolang .
    DATA mt_texts TYPE tt_texts .
    DATA mt_proposal TYPE tt_proposal .
    DATA mt_proposal_best TYPE tt_proposal .
    DATA mt_colob TYPE tt_colob .
    DATA ms_info_object_old TYPE lxe_colob .
    DATA mv_masterlang TYPE lxeisolang .

    METHODS get_masterlang .
    METHODS read_proposal
      IMPORTING
        !it_pcx_s1      TYPE tt_pcx_s1
        !is_colob         TYPE lxe_colob
      EXPORTING
        !et_best_proposal TYPE tt_pcx_s2
        !et_proposal      TYPE tt_pcx_s2 .
    METHODS pp_create_hash
      IMPORTING
        !iv_language TYPE lxeisolang
        !iv_text     TYPE lxeunitlin
      EXPORTING
        !ev_hash     TYPE lxe_pphash
        !ev_enqueue  TYPE sap_bool
      CHANGING
        !cv_pstatus  TYPE lxestatprc .
    METHODS get_object_text
      IMPORTING
        !iv_object         TYPE trobjtype
        !iv_obj_name       TYPE sobj_name
      RETURNING
        VALUE(rv_obj_text) TYPE trobjtype .
    METHODS load_proposal .
    METHODS save_proposal
      IMPORTING
        !is_colob TYPE lxe_colob .
    METHODS read_single_text
      IMPORTING
        !is_colob TYPE lxe_colob
      EXPORTING
        !et_text  TYPE tt_pcx_s1 .
  PRIVATE SECTION.
*"* private components of class ZCL_TRANSLATE_LXE
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_translate_lxe IMPLEMENTATION.


  METHOD get_best_text_proposal.
    FIELD-SYMBOLS <ls_proposal> TYPE LINE OF tt_proposal.

    CLEAR ev_best_text.

    READ TABLE mt_proposal_best ASSIGNING <ls_proposal>
         WITH KEY textkey = iv_textkey
                  objtype = iv_objtype.
    IF sy-subrc = 0.
      ev_best_text = <ls_proposal>-best_prop.
    ENDIF.

  ENDMETHOD.


  METHOD get_lxelist.
    FIELD-SYMBOLS <ls_objects> TYPE LINE OF objtyptable.
    FIELD-SYMBOLS <ls_classlist> TYPE LINE OF seo_inheritances.
    DATA lt_classlist TYPE seo_inheritances.
    DATA ls_lxelist TYPE LINE OF tt_lxe_list.
    DATA lt_objects TYPE objtyptable.
    DATA lo_ref TYPE REF TO zcl_translate_lxe.

    CLEAR et_lxe_list.

* Leo todas las clases que heredan de la clase principal de traducciones.
    SELECT * FROM vseoextend INTO TABLE lt_classlist
      WHERE refclsname = 'ZCL_TRANSLATE_LXE'
      AND version = '1'.
    IF sy-subrc = 0.
      LOOP AT lt_classlist ASSIGNING <ls_classlist>.
        ls_lxelist-class = <ls_classlist>-clsname.

* Cualquier excepcion al instanciar la clase o al llamar de sus métodos hará que no se
* tenga en cuenta la clase.
        TRY.

* Se instancia la clase para saber los objetos que puede traducir.
            CREATE OBJECT lo_ref TYPE (ls_lxelist-class).

            lt_objects = lo_ref->get_lxe_type( ).
            LOOP AT lt_objects ASSIGNING <ls_objects>.
              ls_lxelist-object = <ls_objects>.
              APPEND ls_lxelist TO et_lxe_list.
            ENDLOOP.

          CATCH cx_root.
        ENDTRY.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_lxe_type.
  ENDMETHOD.


  METHOD get_masterlang.
    DATA ld_object TYPE trobjtype.
    DATA ld_obj_name TYPE sobj_name.
    DATA ld_masterlang TYPE masterlang.

* Cambio el tipo de objeto cuando es necesario.
    CASE mv_object.
      WHEN 'CUAD'. " PF-Status
        ld_object = 'PROG'. " El idioma será el del programa original.
      WHEN 'MESS'. " Mensaje
        ld_object = 'MSAG'. " será el de la clase de mensajes.
      WHEN OTHERS.
        ld_object = mv_object.
    ENDCASE.

* Para el nombre del programa elimino partes identificadoras
    ld_obj_name = mv_obj_name.

    CASE mv_object.
      WHEN 'FUGR'.
* Funciones y pools de subrutinas
        REPLACE ALL OCCURRENCES OF 'SAPL' IN ld_obj_name WITH space.
      WHEN 'CLAS'.
* Clases
* Con este primer reemplazo me aseguro que no quito nada del nombre de la clase. Solo
* la parte del nombre completo.
        REPLACE ALL OCCURRENCES OF '=CP' IN ld_obj_name WITH space.
        REPLACE ALL OCCURRENCES OF '=' IN ld_obj_name WITH space.
      WHEN 'MESS'.
* Pongo el primer digito ya que en el DO no se tiene en cuenta. Y al menos
* siempre tendrá un digito la clase de mensajes.
        ld_obj_name = mv_obj_name(1).
* Me quedo con el nombre. al primer espacio significa que luego vendrá el numero de mensaje y en ese
* momento salgo del bucle.
        DO.
          IF mv_obj_name+sy-index(1) = space.
            EXIT.
          ELSE.
            ld_obj_name+sy-index(1) = mv_obj_name+sy-index(1).
          ENDIF.
        ENDDO.
    ENDCASE.

* Ahora busco el idioma en la TADIR
    SELECT SINGLE masterlang INTO ld_masterlang
           FROM tadir
           WHERE pgmid = 'R3TR'
                 AND object = ld_object
                 AND obj_name = ld_obj_name.
    IF sy-subrc = 0.
      CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
        EXPORTING
          r3_lang    = ld_masterlang
        IMPORTING
          o_language = mv_masterlang.
    ELSE.
* Si no encuentro el idioma maestro pongo que es el idioma de origen.
      mv_masterlang = mv_olang.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_text.
*DATA ls_ddenq TYPE ddenqs.
*  DATA ls_trkey TYPE trkey.
*  DATA ld_object_class TYPE c LENGTH 10.
*
*  CLEAR r_obj_text.

* Se determina la clase del objeto.
*  CASE i_object.
*    WHEN zif_ref_data~dc_type_program
*         OR zif_ref_data~dc_type_class
*         OR zif_ref_data~dc_type_interface
*         OR zif_ref_data~dc_type_function
*         OR zif_ref_data~dc_type_webdynpro.
*    WHEN zif_ref_data~dc_type_table
*            OR zif_ref_data~dc_type_struc
*            OR zif_ref_data~dc_type_dataelem
*            OR zif_ref_data~dc_type_domain
*            OR zif_ref_data~dc_type_tabltype
*            OR zif_ref_data~dc_type_seahlp
*            OR zif_ref_data~dc_type_view.
*
** Se pasa los datos a una estructura para obtener
*      ls_ddenq-objname = i_obj_name.
*      ls_ddenq-objtype = i_object.
*
*      CALL FUNCTION 'RS_CORR_CHECK'
*        EXPORTING
*          object          = ls_ddenq
*          object_class    = 'DICT'
*          suppress_dialog = 'X'
*        IMPORTING
*          transport_key   = ls_trkey
*        EXCEPTIONS
*          OTHERS          = 1.
*  ENDCASE.

* Tipo de objeto de texto del objeto pasado.
*  r_obj_text = ls_trkey-sub_type.

  ENDMETHOD.


  METHOD get_texts.
    et_texts = mt_texts.
  ENDMETHOD.


  METHOD has_proposed_text.
  ENDMETHOD.


  METHOD is_text_in_proposal.
    FIELD-SYMBOLS <ls_proposal> TYPE LINE OF tt_proposal.
    rv_exist = abap_false.

    READ TABLE mt_proposal ASSIGNING <ls_proposal> WITH KEY textkey = iv_textkey
                                                            objtype = iv_objtype
                                                            best_prop = iv_text.
    IF sy-subrc = 0.
* El status '00' significa que hay una propuesta pero no esta confirmada para el texto.
      IF <ls_proposal>-status NE '00'.
        rv_exist = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD load_proposal.
*  FIELD-SYMBOLS <ls_texts> LIKE LINE OF it_texts.
*  FIELD-SYMBOLS <ls_pp> TYPE lxe_pp___1.
*  DATA ld_status TYPE lxestatprc.
*  DATA ls_proposal TYPE LINE OF TY_T_PROPOSAL.
*  DATA ld_ohash TYPE lxe_pphash.
*  DATA ld_thash TYPE lxe_pphash.
*  DATA lt_pp TYPE STANDARD TABLE OF lxe_pp___1.
*  DATA lt_ppa TYPE STANDARD TABLE OF lxe_ppa__1.
*  DATA ls_hash TYPE lxe_pphash.
*
*  CLEAR: it_proposal, it_proposal_best.
*
** Los textos tienen que estar para poder leer la propuesta.
*  IF it_texts IS NOT INITIAL.
** Obtengo la mejor propuesta de los textos.
*      CALL FUNCTION 'LXE_PP1_PROPOSALS_GET'
*        EXPORTING
*          s_lang    = d_olang
*          t_lang    = d_tlang
*          custmnr   = et_info_object-custmnr
*          objtype   = et_info_object-objtype
*          domatyp   = d_domatyp
*          domanam   = d_domanam
*        IMPORTING
*          pstatus   = ld_status
*        TABLES
*          lt_pcx_s1 = it_texts
*          lt_pcx_s2 = it_proposal_best.
*
*
** De cada texto obtengo sus propuestas. No hay una funcion que sea como la anterior que
** devuelva todas las propuestas bien formateadas. Por ello, tengo que replicarlo como hace SAP
** pero ajustando a la tabla de mejores propuestas.
*      LOOP AT it_texts ASSIGNING <ls_texts>.
*        CLEAR lt_pp.
*
*        CALL FUNCTION 'LXE_PP1_HASH_CREATE'
*          EXPORTING
*            language       = d_olang
*            text           = <ls_texts>-s_text
*          IMPORTING
*            hash           = ld_ohash
*          EXCEPTIONS
*            internal_error = 1
*            OTHERS         = 2.
*
*        CALL FUNCTION 'LXE_PP1_PP_READ_DB_APPLIC'
*          EXPORTING
*            s_lang         = d_olang
*            t_lang         = d_tlang
*            domatyp        = '*'
*            domanam        = '* '
*            hash           = ld_ohash
*            no_zero        = ''
*          TABLES
*            t_pp           = lt_pp
*            t_ppa          = lt_ppa
*          EXCEPTIONS
*            internal_error = 1
*            OTHERS         = 2.
*
*        LOOP AT lt_pp ASSIGNING <ls_pp>.
*          ls_proposal-status = <ls_pp>-status.
*          ls_proposal-textkey = <ls_texts>-textkey.
*          ls_proposal-cnt_prop = <ls_pp>-var_cnt.
*
*          ls_hash-language = d_tlang.
*          ls_hash-hash_1 = <ls_pp>-t_hash_1.
*          ls_hash-hash_2 = <ls_pp>-t_hash_2.
*          ls_hash-hash_3 = <ls_pp>-t_hash_3.
*          ls_hash-hash_4 = <ls_pp>-t_hash_4.
*          ls_hash-hash_5 = <ls_pp>-t_hash_5.
*          ls_hash-hash_6 = <ls_pp>-t_hash_6.
*
*          CALL FUNCTION 'LXE_PP1_TEXT_GET'
*            EXPORTING
*              hash      = ls_hash
*            IMPORTING
*              text      = ls_proposal-best_prop
*            EXCEPTIONS
*              not_found = 1
*              OTHERS    = 2.
*
*          IF sy-subrc = 0.
*            APPEND ls_proposal TO it_proposal.
*            CLEAR ls_proposal.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.
*
*  ENDIF.

  ENDMETHOD.


  METHOD load_text.
    FIELD-SYMBOLS <ls_colob> TYPE lxe_colob.
    FIELD-SYMBOLS <ls_lxe_texts> TYPE lxe_pcx_s1.
    FIELD-SYMBOLS <ls_pcx_s2> TYPE LINE OF tt_pcx_s2.
    DATA lt_lxe_texts TYPE STANDARD TABLE OF lxe_pcx_s1.
    DATA ls_texts TYPE LINE OF tt_texts.
    DATA lt_best_pcx_s2 TYPE tt_pcx_s2.
    DATA lt_pcx_s2 TYPE tt_pcx_s2.
    DATA ls_proposal TYPE LINE OF tt_proposal.

    CLEAR: mt_texts, mt_proposal, mt_proposal_best.

* Un objeto puede tener tipos de textos a traducir.
    LOOP AT mt_colob ASSIGNING <ls_colob>.

* Se convierte, para objetos antiguos, el objeto a traducir
      CALL FUNCTION 'LXE_OBJ_CONVERT_OL_WLB'
        EXPORTING
          in_custmnr = <ls_colob>-custmnr
          in_objtype = <ls_colob>-objtype
          in_objname = <ls_colob>-objname
        IMPORTING
          custmnr    = <ls_colob>-custmnr
          objtype    = <ls_colob>-objtype
          objname    = <ls_colob>-objname.

* Lectura del texto según el registro que se lee la tabla de tipos de textos
* del objeto
      CALL METHOD read_single_text
        EXPORTING
          is_colob = <ls_colob>
        IMPORTING
          et_text  = lt_lxe_texts.

* Paso los textos leídos de SAP a la tabla general.
      LOOP AT lt_lxe_texts ASSIGNING <ls_lxe_texts>.
        MOVE-CORRESPONDING <ls_lxe_texts> TO ls_texts.
        ls_texts-objtype = <ls_colob>-objtype.
        APPEND ls_texts TO mt_texts.
        CLEAR ls_texts.
      ENDLOOP.
      IF sy-subrc = 0.

* Leo la propuestas de los textos obtenidos
        CALL METHOD read_proposal
          EXPORTING
            it_pcx_s1      = lt_lxe_texts
            is_colob         = <ls_colob>
          IMPORTING
            et_best_proposal = lt_best_pcx_s2
            et_proposal      = lt_pcx_s2.

* Se añaden las propuestas a las tablas globales.
        LOOP AT lt_best_pcx_s2 ASSIGNING <ls_pcx_s2>.
          MOVE-CORRESPONDING <ls_pcx_s2> TO ls_proposal.
          ls_proposal-objtype = <ls_colob>-objtype.
          APPEND ls_proposal TO mt_proposal_best.
          CLEAR ls_proposal.
        ENDLOOP.
        LOOP AT lt_pcx_s2 ASSIGNING <ls_pcx_s2>.
          MOVE-CORRESPONDING <ls_pcx_s2> TO ls_proposal.
          ls_proposal-objtype = <ls_colob>-objtype.
          APPEND ls_proposal TO mt_proposal.
          CLEAR ls_proposal.
        ENDLOOP.
      ENDIF.

      CLEAR: lt_lxe_texts, lt_pcx_s2, lt_best_pcx_s2.

    ENDLOOP.

  ENDMETHOD.


  METHOD pp_create_hash.
    CLEAR: ev_hash.
    ev_enqueue = abap_false.
    cv_pstatus = 'S'. " Ok

    CALL FUNCTION 'LXE_PP1_HASH_CREATE'
      EXPORTING
        language       = iv_language
        text           = iv_text
        insert_db      = 'X'
      IMPORTING
        hash           = ev_hash
        enq_commit     = ev_enqueue
      EXCEPTIONS
        internal_error = 1
        OTHERS         = 2.
    IF sy-subrc NE 0.
      cv_pstatus = 'F'.
    ELSE.
      IF ev_enqueue = abap_true.
        COMMIT WORK.
      ELSE.
        CALL FUNCTION 'ENQUEUE_E_LXETXT'
          EXPORTING
            mode_lxetxt0020 = 'E'
            language        = ev_hash-language
            hash_1          = ev_hash-hash_1
            hash_2          = ev_hash-hash_2
            hash_3          = ev_hash-hash_3
            hash_4          = ev_hash-hash_4
            hash_5          = ev_hash-hash_5
            _wait           = 'X'
          EXCEPTIONS
            foreign_lock    = 1
            system_failure  = 2
            OTHERS          = 3.
        IF sy-subrc <> 0.
          cv_pstatus = 'F'.
        ELSE.
          ev_enqueue = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD read_proposal.
    FIELD-SYMBOLS <ls_texts> TYPE lxe_pcx_s1.
    FIELD-SYMBOLS <ls_pp> TYPE lxe_pp___1.
    DATA ld_domatyp TYPE lxedomatyp .
    DATA ld_domanam TYPE lxedomanam .
    DATA ld_status TYPE lxestatprc.
    DATA ls_proposal TYPE LINE OF tt_proposal.
    DATA ld_ohash TYPE lxe_pphash.
    DATA ld_thash TYPE lxe_pphash.
    DATA lt_pp TYPE STANDARD TABLE OF lxe_pp___1.
    DATA lt_ppa TYPE STANDARD TABLE OF lxe_ppa__1.
    DATA ls_hash TYPE lxe_pphash.


    CLEAR: et_best_proposal, et_proposal.

* Los textos tienen que estar para poder leer la propuesta.
    IF it_pcx_s1 IS NOT INITIAL.

* Leo los atributos del objeto para obtener la propuesta.
      CALL FUNCTION 'LXE_OBJ_GET_TECH_INFO'
        EXPORTING
          custmnr       = is_colob-custmnr
          objtype       = is_colob-objtype
          objname       = is_colob-objname
          bypass_buffer = 'X'
        IMPORTING
          domatyp       = ld_domatyp
          domanam       = ld_domanam.

* Obtengo la mejor propuesta de los textos.
      CALL FUNCTION 'LXE_PP1_PROPOSALS_GET'
        EXPORTING
          s_lang   = mv_olang
          t_lang   = mv_tlang
          custmnr  = is_colob-custmnr
          objtype  = is_colob-objtype
          domatyp  = ld_domatyp
          domanam  = ld_domanam
        IMPORTING
          pstatus  = ld_status
        TABLES
          t_pcx_s1 = it_pcx_s1[]
          t_pcx_s2 = et_best_proposal[].

* De cada texto obtengo sus propuestas. No hay una funcion que sea como la anterior que
* devuelva todas las propuestas bien formateadas. Por ello, tengo que replicarlo como hace SAP
* pero ajustando a la tabla de mejores propuestas.
      LOOP AT it_pcx_s1 ASSIGNING <ls_texts>.
        CLEAR lt_pp.

        CALL FUNCTION 'LXE_PP1_HASH_CREATE'
          EXPORTING
            language       = mv_olang
            text           = <ls_texts>-s_text
          IMPORTING
            hash           = ld_ohash
          EXCEPTIONS
            internal_error = 1
            OTHERS         = 2.

        CALL FUNCTION 'LXE_PP1_PP_READ_DB_APPLIC'
          EXPORTING
            s_lang         = mv_olang
            t_lang         = mv_tlang
            domatyp        = '*'
            domanam        = '* '
            hash           = ld_ohash
            no_zero        = ''
          TABLES
            t_pp           = lt_pp
            t_ppa          = lt_ppa
          EXCEPTIONS
            internal_error = 1
            OTHERS         = 2.

        LOOP AT lt_pp ASSIGNING <ls_pp>.
          ls_proposal-status = <ls_pp>-status.
          ls_proposal-textkey = <ls_texts>-textkey.
          ls_proposal-cnt_prop = <ls_pp>-var_cnt.

          ls_hash-language = mv_tlang.
          ls_hash-hash_1 = <ls_pp>-t_hash_1.
          ls_hash-hash_2 = <ls_pp>-t_hash_2.
          ls_hash-hash_3 = <ls_pp>-t_hash_3.
          ls_hash-hash_4 = <ls_pp>-t_hash_4.
          ls_hash-hash_5 = <ls_pp>-t_hash_5.
          ls_hash-hash_6 = <ls_pp>-t_hash_6.

          CALL FUNCTION 'LXE_PP1_TEXT_GET'
            EXPORTING
              hash      = ls_hash
            IMPORTING
              text      = ls_proposal-best_prop
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.

          IF sy-subrc = 0.
            APPEND ls_proposal TO et_proposal.
            CLEAR ls_proposal.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD read_single_text.
    FIELD-SYMBOLS <ls_text> TYPE LINE OF tt_pcx_s1.
    CLEAR et_text.

* Se llama a la función que cubre el 80% o de traducciones. Para objetos
* especificos se sobrecarga el método y se cambia.
    CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
      EXPORTING
        t_lang    = mv_tlang
        s_lang    = mv_olang
        custmnr   = is_colob-custmnr
        objtype   = is_colob-objtype
        objname   = is_colob-objname
        read_only = space
      TABLES
        lt_pcx_s1 = et_text[].

* Si E_TEXT esta en blanco significa que el objeto no esta traducido. Por lo que los leo del idioma maestro del
* objeto, y limpio el texto de destino que sera el mismo que el de origen. De esta manera, se podra traducir.
    IF et_text IS INITIAL.
      CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
        EXPORTING
          t_lang    = mv_masterlang
          s_lang    = mv_masterlang
          custmnr   = is_colob-custmnr
          objtype   = is_colob-objtype
          objname   = is_colob-objname
          read_only = space
        TABLES
          lt_pcx_s1 = et_text[].

      LOOP AT et_text ASSIGNING <ls_text>.
        CLEAR <ls_text>-t_text.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD save_proposal.
    FIELD-SYMBOLS <ls_texts> LIKE LINE OF mt_texts.
    DATA ld_pstatus TYPE lxestatprc.
    DATA ls_t_hash TYPE lxe_pphash.
    DATA ld_t_enq TYPE c.
    DATA ls_s_hash TYPE lxe_pphash.
    DATA ld_s_enq TYPE c.
    DATA ld_enq TYPE c.
    DATA ls_proposal TYPE LINE OF tt_proposal.
    DATA ld_domatyp TYPE lxedomatyp .
    DATA ld_domanam TYPE lxedomanam .

* Solo se graban como propuestas aquellos textos que no existen
* en la tabla de propuestas
    LOOP AT mt_texts ASSIGNING <ls_texts> WHERE t_text IS NOT INITIAL
                                                AND objtype = is_colob-objtype.

      READ TABLE mt_proposal TRANSPORTING NO FIELDS WITH KEY textkey = <ls_texts>-textkey
                                                             objtype = <ls_texts>-objtype
                                                              best_prop = <ls_texts>-t_text.
      IF sy-subrc NE 0.

* Añado el nuevo texto a la tabla de propuestas
        ls_proposal-textkey = <ls_texts>-textkey.
        ls_proposal-objtype = <ls_texts>-objtype.
        ls_proposal-best_prop = <ls_texts>-t_text.
        ls_proposal-status = cv_status_proposal.
        APPEND ls_proposal TO mt_proposal.

* Leo los atributos del objeto para poder grabar la propuesta de texto
        CALL FUNCTION 'LXE_OBJ_GET_TECH_INFO'
          EXPORTING
            custmnr       = is_colob-custmnr
            objtype       = is_colob-objtype
            objname       = is_colob-objname
            bypass_buffer = 'X'
          IMPORTING
            domatyp       = ld_domatyp
            domanam       = ld_domanam.

        ld_pstatus = 'S'. " Proceso correcto.

* Se crea el HASH en base de datos del texto destino.
        CALL METHOD pp_create_hash
          EXPORTING
            iv_language = mv_tlang
            iv_text     = <ls_texts>-t_text
          IMPORTING
            ev_hash     = ls_t_hash
            ev_enqueue  = ld_t_enq
          CHANGING
            cv_pstatus  = ld_pstatus.


        IF ld_pstatus = 'S'. " Si todo correcto se continua el proceso.

* Lo mismo para el texto de origen
          CALL METHOD pp_create_hash
            EXPORTING
              iv_language = mv_olang
              iv_text     = <ls_texts>-s_text
            IMPORTING
              ev_hash     = ls_s_hash
              ev_enqueue  = ld_s_enq
            CHANGING
              cv_pstatus  = ld_pstatus.

          IF ld_pstatus = 'S'. " Si todo correcto se continua.
* Finalmente se crea la propuesta con el status '09'.
            CALL FUNCTION 'LXE_PP1_APPLIC_CREATE'
              EXPORTING
                t_lang         = mv_tlang
                s_lang         = mv_olang
                domatyp        = ld_domatyp
                domanam        = ld_domanam
                s_hash         = ls_s_hash
                t_hash         = ls_t_hash
                status         = cv_status_proposal
              IMPORTING
                enq_commit     = ld_enq
              EXCEPTIONS
                internal_error = 1
                OTHERS         = 2.
            IF sy-subrc = 0 AND ld_enq = abap_true.
              COMMIT WORK.
              CALL FUNCTION 'DEQUEUE_E_LXE_PP'
                EXPORTING
                  t_lang   = mv_tlang
                  s_lang   = mv_olang
                  s_hash_1 = ls_s_hash-hash_1
                  s_hash_2 = ls_s_hash-hash_2
                  s_hash_3 = ls_s_hash-hash_3
                  s_hash_4 = ls_s_hash-hash_4
                  s_hash_5 = ls_s_hash-hash_5.
            ENDIF.
          ENDIF.
        ENDIF.

* Se desbloquean los HASH de los textos
        IF ld_t_enq = abap_true.
          CALL FUNCTION 'DEQUEUE_E_LXETXT'
            EXPORTING
              language = ls_t_hash-language
              hash_1   = ls_t_hash-hash_1
              hash_2   = ls_t_hash-hash_2
              hash_3   = ls_t_hash-hash_3
              hash_4   = ls_t_hash-hash_4
              hash_5   = ls_t_hash-hash_5.
        ENDIF.
        IF ld_s_enq = abap_true.
          CALL FUNCTION 'DEQUEUE_E_LXETXT'
            EXPORTING
              language = ls_s_hash-language
              hash_1   = ls_s_hash-hash_1
              hash_2   = ls_s_hash-hash_2
              hash_3   = ls_s_hash-hash_3
              hash_4   = ls_s_hash-hash_4
              hash_5   = ls_s_hash-hash_5.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD save_text.
    FIELD-SYMBOLS <ls_colob> TYPE lxe_colob.
    FIELD-SYMBOLS <ls_texts> TYPE LINE OF tt_texts.
    DATA ld_status TYPE lxestatprc.
    DATA lt_lxe_texts TYPE STANDARD TABLE OF lxe_pcx_s1.
    DATA ls_lxe_texts TYPE lxe_pcx_s1.

* La tabla de textos principal contiene todos los posibles
* textos que puede tener un objeto. Para poder grabar hay
* hacerlo por cada tipo. Por eso hay que separalos.
    LOOP AT mt_colob ASSIGNING <ls_colob>.

      CLEAR lt_lxe_texts.
      LOOP AT mt_texts ASSIGNING <ls_texts> WHERE objtype = <ls_colob>-objtype.
        MOVE-CORRESPONDING <ls_texts> TO ls_lxe_texts.
        APPEND ls_lxe_texts TO lt_lxe_texts.
      ENDLOOP.
      IF sy-subrc = 0.

* Antes de grabar hay que leer de nuevo los objetos para que se carguen tablas intermedias
* del grupo de funciones estándar.
        read_single_text( is_colob = <ls_colob> ).

        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
          EXPORTING
            t_lang    = mv_tlang
            s_lang    = mv_olang
            custmnr   = <ls_colob>-custmnr
            objtype   = <ls_colob>-objtype
            objname   = <ls_colob>-objname
          IMPORTING
            pstatus   = ld_status
          TABLES
            lt_pcx_s1 = lt_lxe_texts.

        IF ld_status = 'F'.
          RAISE error_save.
        ENDIF.

* Se graban las propuestas
        save_proposal( is_colob = <ls_colob> ).

      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD set_check_params.
    DATA ls_e071 TYPE LINE OF tr_objects.
    DATA lt_e071_text TYPE tr_objects .
    DATA lt_e071k_text TYPE tr_keys .

* Guardo los parametros pasados a variables globales.
    mv_object = iv_object.
    mv_obj_name = iv_obj_name.
    mv_olang = iv_olang.
    mv_tlang = iv_tlang.

    mv_object_text = get_object_text( iv_object = iv_object
                                   iv_obj_name = iv_obj_name ).

* Si no se puede determinar el tipo de objeto del texto se lanza una
* excepcion.
    IF mv_object_text IS NOT INITIAL.

* Obtengo el idioma original del objeto. Se usará cuando no hay textos en el idioma de origen.
      get_masterlang( ).

* Se rellena una tabla interna (e071 la que contiene los objetos en las tareas) para poder la
* informacion para el transporte
      ls_e071-pgmid = 'LIMU'.
      ls_e071-object = mv_object_text.
      ls_e071-obj_name = iv_obj_name.
      APPEND ls_e071 TO lt_e071_text.

* Se obtiene la informacion para obtener los datos traduccion en base a los datos de la tabla de transporte.
      CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT'
        TABLES
          in_e071  = lt_e071_text
          in_e071k = lt_e071k_text
          ex_colob = mt_colob.

      IF mt_colob IS INITIAL.
        RAISE object_not_valid.
      ENDIF.

    ELSE.
      RAISE object_not_valid.
    ENDIF.

  ENDMETHOD.


  METHOD set_text.
    FIELD-SYMBOLS <ls_texts> TYPE LINE OF tt_texts.

    READ TABLE mt_texts ASSIGNING <ls_texts> WITH KEY textkey = iv_id_text
                                                      objtype = iv_objtype.
    IF sy-subrc = 0.
      <ls_texts>-t_text = iv_text.
    ELSE.
      RAISE id_text_dont_exist.
    ENDIF.
  ENDMETHOD.


  METHOD transport_translate.
    FIELD-SYMBOLS <ls_colob> TYPE lxe_colob.
    DATA lt_e071k TYPE STANDARD TABLE OF e071k.
    DATA ls_e071k TYPE e071k.
    DATA lt_e071 TYPE STANDARD TABLE OF e071.
    DATA ls_e071 TYPE e071.
    DATA ld_lang TYPE spras.

* Se convierte el idioma del sistema de traduccio a idioma R/3
* para ponerlo en la orden.
    CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
      EXPORTING
        language           = mv_tlang
      IMPORTING
        o_r3_lang          = ld_lang
      EXCEPTIONS
        language_not_in_cp = 1
        unknown            = 2
        OTHERS             = 3.

    LOOP AT mt_colob ASSIGNING <ls_colob>.

* Llamo a la funcion que pasandole el objeto me genera las entradas en las tablas
* E071 y E071K para pasarla a la orden de transporte.
      CALL FUNCTION 'LXE_OBJ_CREATE_TRANSPORT_SE63'
        EXPORTING
          language = ld_lang
          custmnr  = <ls_colob>-custmnr
          objtype  = <ls_colob>-objtype
          objname  = <ls_colob>-objname
        TABLES
          ex_e071  = lt_e071[]
          ex_e071k = lt_e071k[].

      IF lt_e071 IS NOT INITIAL.

        CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
          EXPORTING
            wi_simulation                  = ' '
            wi_suppress_key_check          = ' '
            wi_trkorr                      = iv_trkorr
          TABLES
            wt_e071                        = lt_e071[]
            wt_e071k                       = lt_e071k[]
          EXCEPTIONS
            key_char_in_non_char_field     = 1
            key_check_keysyntax_error      = 2
            key_inttab_table               = 3
            key_longer_field_but_no_generc = 4
            key_missing_key_master_fields  = 5
            key_missing_key_tablekey       = 6
            key_non_char_but_no_generic    = 7
            key_no_key_fields              = 8
            key_string_longer_char_key     = 9
            key_table_has_no_fields        = 10
            key_table_not_activ            = 11
            key_unallowed_key_function     = 12
            key_unallowed_key_object       = 13
            key_unallowed_key_objname      = 14
            key_unallowed_key_pgmid        = 15
            key_without_header             = 16
            ob_check_obj_error             = 17
            ob_devclass_no_exist           = 18
            ob_empty_key                   = 19
            ob_generic_objectname          = 20
            ob_ill_delivery_transport      = 21
            ob_ill_lock                    = 22
            ob_ill_parts_transport         = 23
            ob_ill_source_system           = 24
            ob_ill_system_object           = 25
            ob_ill_target                  = 26
            ob_inttab_table                = 27
            ob_local_object                = 28
            ob_locked_by_other             = 29
            ob_modif_only_in_modif_order   = 30
            ob_name_too_long               = 31
            ob_no_append_of_corr_entry     = 32
            ob_no_append_of_c_member       = 33
            ob_no_consolidation_transport  = 34
            ob_no_original                 = 35
            ob_no_shared_repairs           = 36
            ob_no_systemname               = 37
            ob_no_systemtype               = 38
            ob_no_tadir                    = 39
            ob_no_tadir_not_lockable       = 40
            ob_privat_object               = 41
            ob_repair_only_in_repair_order = 42
            ob_reserved_name               = 43
            ob_syntax_error                = 44
            ob_table_has_no_fields         = 45
            ob_table_not_activ             = 46
            tr_enqueue_failed              = 47
            tr_errors_in_error_table       = 48
            tr_ill_korrnum                 = 49
            tr_lockmod_failed              = 50
            tr_lock_enqueue_failed         = 51
            tr_not_owner                   = 52
            tr_no_systemname               = 53
            tr_no_systemtype               = 54
            tr_order_not_exist             = 55
            tr_order_released              = 56
            tr_order_update_error          = 57
            tr_wrong_order_type            = 58
            ob_invalid_target_system       = 59
            tr_no_authorization            = 60
            ob_wrong_tabletyp              = 61
            ob_wrong_category              = 62
            ob_system_error                = 63
            ob_unlocal_objekt_in_local_ord = 64
            tr_wrong_client                = 65
            ob_wrong_client                = 66
            key_wrong_client               = 67
            OTHERS                         = 68.

        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error_insert_trkorr.

        ENDIF.
      ENDIF.
      CLEAR: lt_e071, lt_e071k.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
