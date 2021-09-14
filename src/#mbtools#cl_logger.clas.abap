*----------------------------------------------------------------------*
*       CLASS ZCL_LOGGER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS /mbtools/cl_logger DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS /mbtools/cl_logger_factory.

  PUBLIC SECTION.
*"* public components of class ZCL_LOGGER
*"* do not include other source files here!!!


    INTERFACES /mbtools/if_logger.
    ALIASES: add FOR /mbtools/if_logger~add,
             a FOR /mbtools/if_logger~a,
             e FOR /mbtools/if_logger~e,
             w FOR /mbtools/if_logger~w,
             i FOR /mbtools/if_logger~i,
             s FOR /mbtools/if_logger~s,
             has_errors FOR /mbtools/if_logger~has_errors,
             has_warnings FOR /mbtools/if_logger~has_warnings,
             is_empty FOR /mbtools/if_logger~is_empty,
             length FOR /mbtools/if_logger~length,
             save FOR /mbtools/if_logger~save,
             export_to_table FOR /mbtools/if_logger~export_to_table,
             fullscreen FOR /mbtools/if_logger~fullscreen,
             popup FOR /mbtools/if_logger~popup,
             handle FOR /mbtools/if_logger~handle,
             db_number FOR /mbtools/if_logger~db_number,
             header FOR /mbtools/if_logger~header.

    "! Starts a new log.
    "! For backwards compatibility only! Use ZCL_LOGGER_FACTORY instead.
    CLASS-METHODS new
      IMPORTING
        !object         TYPE csequence OPTIONAL
        !subobject      TYPE csequence OPTIONAL
        !desc           TYPE csequence OPTIONAL
        !context        TYPE simple OPTIONAL
        !auto_save      TYPE abap_bool OPTIONAL
        !second_db_conn TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_log)    TYPE REF TO /mbtools/cl_logger.

    "! Reopens an already existing log.
    "! For backwards compatibility only! Use ZCL_LOGGER_FACTORY instead.
    CLASS-METHODS open
      IMPORTING
        !object                   TYPE csequence
        !subobject                TYPE csequence
        !desc                     TYPE csequence OPTIONAL
        !create_if_does_not_exist TYPE abap_bool DEFAULT abap_false
        !auto_save                TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(r_log)              TYPE REF TO /mbtools/cl_logger.

  PROTECTED SECTION.
*"* protected components of class ZCL_LOGGER
*"* do not include other source files here!!!
  PRIVATE SECTION.
* Local type for hrpad_message as it is not available in an ABAP Development System
    TYPES: BEGIN OF hrpad_message_field_list_alike,
             scrrprfd TYPE scrrprfd.
    TYPES: END OF hrpad_message_field_list_alike.

    TYPES: BEGIN OF hrpad_message_alike,
             cause(32)    TYPE c,                          "original: hrpad_message_cause
             detail_level TYPE ballevel.
             INCLUDE TYPE symsg.
    TYPES: field_list   TYPE STANDARD TABLE OF hrpad_message_field_list_alike
               WITH NON-UNIQUE KEY scrrprfd.
    TYPES: END OF hrpad_message_alike.

*"* private components of class ZCL_LOGGER
*"* do not include other source files here!!!
    DATA sec_connection           TYPE abap_bool.
    DATA sec_connect_commit       TYPE abap_bool.
    DATA settings                 TYPE REF TO /mbtools/if_logger_settings.

    METHODS:
      "! Safety limit for previous exception drill down
      drill_down_into_exception
        IMPORTING
          exception                      TYPE REF TO cx_root
          type                           TYPE symsgty OPTIONAL
          importance                     TYPE balprobcl OPTIONAL
        RETURNING
          VALUE(rt_exception_data_table) TYPE tty_exception_data,
get_message_handles
        IMPORTING
          msgtype                   TYPE symsgty OPTIONAL
        RETURNING
          VALUE(rt_message_handles) TYPE bal_t_msgh,
add_structure
        IMPORTING
          obj_to_log    TYPE any OPTIONAL
          context       TYPE simple OPTIONAL
          callback_form TYPE csequence OPTIONAL
          callback_prog TYPE csequence OPTIONAL
          callback_fm   TYPE csequence OPTIONAL
          type          TYPE symsgty OPTIONAL
          importance    TYPE balprobcl OPTIONAL
            PREFERRED PARAMETER obj_to_log
        RETURNING
          VALUE(self)   TYPE REF TO /mbtools/if_logger.

    METHODS save_log.
ENDCLASS.


CLASS /mbtools/cl_logger IMPLEMENTATION.


  METHOD drill_down_into_exception.
    DATA: i                  TYPE i VALUE 2,
          previous_exception TYPE REF TO cx_root,
          exceptions         TYPE tty_exception.

    FIELD-SYMBOLS <ex> LIKE LINE OF exceptions.
    FIELD-SYMBOLS <ret> LIKE LINE OF rt_exception_data_table.
    APPEND INITIAL LINE TO exceptions ASSIGNING <ex>.
    <ex>-level = 1.
    <ex>-exception = exception.

    previous_exception = exception.

    WHILE i <= settings->get_max_exception_drill_down( ).
      IF previous_exception->previous IS NOT BOUND.
        EXIT.
      ENDIF.

      previous_exception ?= previous_exception->previous.

      APPEND INITIAL LINE TO exceptions ASSIGNING <ex>.
      <ex>-level = i.
      <ex>-exception = previous_exception.
      i = i + 1.
    ENDWHILE.


    SORT exceptions BY level DESCENDING.                   "Display the deepest exception first
    LOOP AT exceptions ASSIGNING <ex>.
      APPEND INITIAL LINE TO rt_exception_data_table ASSIGNING <ret>.
      <ret>-exception = <ex>-exception.
      <ret>-msgty     = type.
      <ret>-probclass = importance.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_message_handles.

    DATA: log_handle TYPE bal_t_logh,
          filter     TYPE bal_s_mfil.

    FIELD-SYMBOLS <f> LIKE LINE OF filter-msgty.

    INSERT handle INTO TABLE log_handle.

    IF msgtype IS NOT INITIAL.
      APPEND INITIAL LINE TO filter-msgty ASSIGNING <f>.
      <f>-sign   = 'I'.
      <f>-option = 'EQ'.
      <f>-low    = msgtype.
    ENDIF.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = log_handle
        i_s_msg_filter = filter
      IMPORTING
        e_t_msg_handle = rt_message_handles
      EXCEPTIONS
        msg_not_found  = 0.

  ENDMETHOD.


  METHOD new.

    IF auto_save IS SUPPLIED.
      r_log ?= /mbtools/cl_logger_factory=>create_log(
        object = object
        subobject = subobject
        desc = desc
        context = context
        settings = /mbtools/cl_logger_factory=>create_settings(
          )->set_usage_of_secondary_db_conn( second_db_conn
          )->set_autosave( auto_save )
      ).
    ELSE.
      r_log ?= /mbtools/cl_logger_factory=>create_log(
        object = object
        subobject = subobject
        desc = desc
        context = context
        settings = /mbtools/cl_logger_factory=>create_settings(
          )->set_usage_of_secondary_db_conn( second_db_conn )
      ).
    ENDIF.

  ENDMETHOD.


  METHOD open.

    IF auto_save IS SUPPLIED.
      r_log ?= /mbtools/cl_logger_factory=>open_log(
        object = object
        subobject = subobject
        desc = desc
        create_if_does_not_exist = create_if_does_not_exist
        settings = /mbtools/cl_logger_factory=>create_settings(
          )->set_autosave( auto_save )
      ).
    ELSE.
      r_log ?= /mbtools/cl_logger_factory=>open_log(
        object = object
        subobject = subobject
        desc = desc
        create_if_does_not_exist = create_if_does_not_exist
      ).
    ENDIF.

  ENDMETHOD.


  METHOD /mbtools/if_logger~a.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'A'
      importance    = importance ).
  ENDMETHOD.


  METHOD /mbtools/if_logger~add.

    DATA: detailed_msg         TYPE bal_s_msg,
          exception_data_table TYPE tty_exception_data,
          free_text_msg        TYPE char200,
          ctx_type             TYPE REF TO cl_abap_typedescr,
          ctx_ddic_header      TYPE x030l,
          msg_type             TYPE REF TO cl_abap_typedescr,
          msg_table_type       TYPE REF TO cl_abap_tabledescr,
          log_numbers          TYPE bal_t_lgnm,
          log_handles          TYPE bal_t_logh,
          log_number           TYPE bal_s_lgnm,
          formatted_context    TYPE bal_s_cont,
          formatted_params     TYPE bal_s_parm,
          message_type         TYPE symsgty.

    FIELD-SYMBOLS: <table_of_messages> TYPE ANY TABLE,
                   <message_line>      TYPE any,
                   <symsg>             TYPE symsg,
                   <bapiret1_msg>      TYPE bapiret1,
                   <bapi_msg>          TYPE bapiret2,
                   <bapi_coru_msg>     TYPE bapi_coru_return,
                   <bdc_msg>           TYPE bdcmsgcoll,
                   <hrpad_msg>         TYPE hrpad_message_alike,
                   <context_val>       TYPE any.
      "Solution manager doens't have BAPI_ORDER_RETURN, RCOMP, PROTT. Therefore avoid using these concrete types
*                   <bapi_order_msg>    type bapi_order_return,
*                   <rcomp_msg>         type rcomp,
*                   <prott_msg>         type prott,
    DATA replacement_bapi_order_return TYPE bapiret2.
    FIELD-SYMBOLS <exception_data> LIKE LINE OF exception_data_table.

    IF context IS NOT INITIAL.
      ASSIGN context TO <context_val>.
      formatted_context-value = <context_val>.
      ctx_type                = cl_abap_typedescr=>describe_by_data( context ).

      ctx_type->get_ddic_header( RECEIVING p_header = ctx_ddic_header EXCEPTIONS not_found = 1 no_ddic_type = 2 OTHERS = 3 ).
      IF sy-subrc = 0.
        formatted_context-tabname = ctx_ddic_header-tabname.
      ENDIF.
    ENDIF.

    IF callback_fm IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_fm.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = 'F'.
    ELSEIF callback_form IS NOT INITIAL.
      formatted_params-callback-userexitf = callback_form.
      formatted_params-callback-userexitp = callback_prog.
      formatted_params-callback-userexitt = ' '.
    ENDIF.

    msg_type = cl_abap_typedescr=>describe_by_data( obj_to_log ).

    IF obj_to_log IS NOT SUPPLIED.
      detailed_msg-msgty = sy-msgty.
      detailed_msg-msgid = sy-msgid.
      detailed_msg-msgno = sy-msgno.
      detailed_msg-msgv1 = sy-msgv1.
      detailed_msg-msgv2 = sy-msgv2.
      detailed_msg-msgv3 = sy-msgv3.
      detailed_msg-msgv4 = sy-msgv4.
    ELSEIF msg_type->absolute_name = '\TYPE=SYMSG'.
      ASSIGN obj_to_log TO <symsg>.
      detailed_msg-msgty = <symsg>-msgty.
      detailed_msg-msgid = <symsg>-msgid.
      detailed_msg-msgno = <symsg>-msgno.
      detailed_msg-msgv1 = <symsg>-msgv1.
      detailed_msg-msgv2 = <symsg>-msgv2.
      detailed_msg-msgv3 = <symsg>-msgv3.
      detailed_msg-msgv4 = <symsg>-msgv4.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPIRET1'.
      ASSIGN obj_to_log TO <bapiret1_msg>.
      detailed_msg-msgty = <bapiret1_msg>-type.
      detailed_msg-msgid = <bapiret1_msg>-id.
      detailed_msg-msgno = <bapiret1_msg>-number.
      detailed_msg-msgv1 = <bapiret1_msg>-message_v1.
      detailed_msg-msgv2 = <bapiret1_msg>-message_v2.
      detailed_msg-msgv3 = <bapiret1_msg>-message_v3.
      detailed_msg-msgv4 = <bapiret1_msg>-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPIRET2'.
      ASSIGN obj_to_log TO <bapi_msg>.
      detailed_msg-msgty = <bapi_msg>-type.
      detailed_msg-msgid = <bapi_msg>-id.
      detailed_msg-msgno = <bapi_msg>-number.
      detailed_msg-msgv1 = <bapi_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_msg>-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPI_CORU_RETURN'.
      ASSIGN obj_to_log TO <bapi_coru_msg>.
      detailed_msg-msgty = <bapi_coru_msg>-type.
      detailed_msg-msgid = <bapi_coru_msg>-id.
      detailed_msg-msgno = <bapi_coru_msg>-number.
      detailed_msg-msgv1 = <bapi_coru_msg>-message_v1.
      detailed_msg-msgv2 = <bapi_coru_msg>-message_v2.
      detailed_msg-msgv3 = <bapi_coru_msg>-message_v3.
      detailed_msg-msgv4 = <bapi_coru_msg>-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      ASSIGN obj_to_log TO <bdc_msg>.
      detailed_msg-msgty = <bdc_msg>-msgtyp.
      detailed_msg-msgid = <bdc_msg>-msgid.
      detailed_msg-msgno = <bdc_msg>-msgnr.
      detailed_msg-msgv1 = <bdc_msg>-msgv1.
      detailed_msg-msgv2 = <bdc_msg>-msgv2.
      detailed_msg-msgv3 = <bdc_msg>-msgv3.
      detailed_msg-msgv4 = <bdc_msg>-msgv4.
    ELSEIF msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.
      ASSIGN obj_to_log TO <hrpad_msg>.
      detailed_msg-msgty = <hrpad_msg>-msgty.
      detailed_msg-msgid = <hrpad_msg>-msgid.
      detailed_msg-msgno = <hrpad_msg>-msgno.
      detailed_msg-msgv1 = <hrpad_msg>-msgv1.
      detailed_msg-msgv2 = <hrpad_msg>-msgv2.
      detailed_msg-msgv3 = <hrpad_msg>-msgv3.
      detailed_msg-msgv4 = <hrpad_msg>-msgv4.
    ELSEIF msg_type->absolute_name = '\TYPE=BAPI_ORDER_RETURN'.
      "Solution manager doens't have BAPI_ORDER_RETURN. Therefore avoid using the concrete type
      MOVE-CORRESPONDING obj_to_log TO replacement_bapi_order_return.
      detailed_msg-msgty = replacement_bapi_order_return-type.
      detailed_msg-msgid = replacement_bapi_order_return-id.
      detailed_msg-msgno = replacement_bapi_order_return-number.
      detailed_msg-msgv1 = replacement_bapi_order_return-message_v1.
      detailed_msg-msgv2 = replacement_bapi_order_return-message_v2.
      detailed_msg-msgv3 = replacement_bapi_order_return-message_v3.
      detailed_msg-msgv4 = replacement_bapi_order_return-message_v4.
    ELSEIF msg_type->absolute_name = '\TYPE=RCOMP'.
      "Solution manager doens't have RCOMP. Therefore avoid using the concrete type
      MOVE-CORRESPONDING obj_to_log TO detailed_msg.
    ELSEIF msg_type->absolute_name = '\TYPE=PROTT'.
      "Solution manager doens't have PROTT. Therefore avoid using the concrete type
      MOVE-CORRESPONDING obj_to_log TO detailed_msg.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_oref.
      IF type IS INITIAL.
        message_type = if_msg_output=>msgtype_error.
      ELSE.
        message_type = type.
      ENDIF.
      exception_data_table = me->drill_down_into_exception(
          exception   = obj_to_log
          type        = message_type
          importance  = importance
          ).
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN obj_to_log TO <table_of_messages>.
      LOOP AT <table_of_messages> ASSIGNING <message_line>.
        IF sy-tabix = 1.
          /mbtools/if_logger~add(
              obj_to_log    = <message_line>
              context       = context ).
        ELSE.
          /mbtools/if_logger~add( <message_line> ).
        ENDIF.
      ENDLOOP.
    ELSEIF msg_type->type_kind = cl_abap_typedescr=>typekind_struct1   "flat structure
        OR msg_type->type_kind = cl_abap_typedescr=>typekind_struct2.  "deep structure (already when string is used)
      add_structure(
        EXPORTING
          obj_to_log    = obj_to_log
          context       = context
          callback_form = callback_form
          callback_prog = callback_prog
          callback_fm   = callback_fm
          type          = type
          importance    = importance
        RECEIVING
          self          = self
      ).
    ELSE.
      free_text_msg = obj_to_log.
    ENDIF.

    IF free_text_msg IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle = me->handle
          i_msgty      = type
          i_probclass  = importance
          i_text       = free_text_msg
          i_s_context  = formatted_context
          i_s_params   = formatted_params.
    ELSEIF exception_data_table IS NOT INITIAL.

      LOOP AT exception_data_table ASSIGNING <exception_data>.
        CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
          EXPORTING
            i_log_handle = me->handle
            i_s_exc      = <exception_data>.
      ENDLOOP.
    ELSEIF detailed_msg IS NOT INITIAL.
      detailed_msg-context   = formatted_context.
      detailed_msg-params    = formatted_params.
      detailed_msg-probclass = importance.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = me->handle
          i_s_msg      = detailed_msg.
    ENDIF.

    IF me->settings->get_autosave( ) = abap_true.
      save_log( ).
    ENDIF.
    self = me.
  ENDMETHOD.


  METHOD add_structure.
    DATA: msg_type        TYPE REF TO cl_abap_typedescr,
          msg_struct_type TYPE REF TO cl_abap_structdescr,
          components      TYPE cl_abap_structdescr=>component_table,
          component       LIKE LINE OF components,
          string_to_log   TYPE string.
    FIELD-SYMBOLS: <component>   TYPE any.

    msg_struct_type ?= cl_abap_typedescr=>describe_by_data( obj_to_log ).
    components = msg_struct_type->get_components( ).
    add( '--- Begin of structure ---' ).
    LOOP AT components INTO component.
      ASSIGN COMPONENT component-name OF STRUCTURE obj_to_log TO <component>.
      IF sy-subrc = 0.
        msg_type = cl_abap_typedescr=>describe_by_data( <component> ).
        IF msg_type->kind = cl_abap_typedescr=>kind_elem.
          string_to_log = |{ to_lower( component-name ) } = { <component> }|.
          add( string_to_log ).
        ELSEIF msg_type->kind = cl_abap_typedescr=>kind_struct.
          add_structure(
            EXPORTING
              obj_to_log    = <component>
              context       = context
              callback_form = callback_form
              callback_prog = callback_prog
              callback_fm   = callback_fm
              type          = type
              importance    = importance
            RECEIVING
              self          = self
          ).
        ENDIF.
      ENDIF.
    ENDLOOP.
    add( '--- End of structure ---' ).
  ENDMETHOD.


  METHOD /mbtools/if_logger~e.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'E'
      importance    = importance ).
  ENDMETHOD.


  METHOD /mbtools/if_logger~export_to_table.
    DATA: message_handles TYPE bal_t_msgh,
          message         TYPE bal_s_msg,
          bapiret2        TYPE bapiret2.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    message_handles = get_message_handles( ).

    LOOP AT message_handles ASSIGNING <msg_handle>.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <msg_handle>
        IMPORTING
          e_s_msg        = message
        EXCEPTIONS
          OTHERS         = 3.
      IF sy-subrc IS INITIAL.
        MESSAGE ID message-msgid
                TYPE message-msgty
                NUMBER message-msgno
                INTO bapiret2-message
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4.

        bapiret2-type       = message-msgty.
        bapiret2-id         = message-msgid.
        bapiret2-number     = message-msgno.
        bapiret2-log_no     = <msg_handle>-log_handle.     "last 2 chars missing!!
        bapiret2-log_msg_no = <msg_handle>-msgnumber.
        bapiret2-message_v1 = message-msgv1.
        bapiret2-message_v2 = message-msgv2.
        bapiret2-message_v3 = message-msgv3.
        bapiret2-message_v4 = message-msgv4.
        bapiret2-system     = sy-sysid.
        APPEND bapiret2 TO rt_bapiret.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD /mbtools/if_logger~fullscreen.

    DATA:
      profile        TYPE bal_s_prof,
      lt_log_handles TYPE bal_t_logh.

    APPEND me->handle TO lt_log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = profile
        i_t_log_handle      = lt_log_handles.

  ENDMETHOD.


  METHOD /mbtools/if_logger~has_errors.

    rv_yes = boolc( lines( get_message_handles( msgtype = 'E' ) ) > 0 ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~has_warnings.

    rv_yes = boolc( lines( get_message_handles( msgtype = 'W' ) ) > 0 ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~i.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'I'
      importance    = importance ).
  ENDMETHOD.


  METHOD /mbtools/if_logger~is_empty.

    rv_yes = boolc( length( ) = 0 ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~length.

    rv_length = lines( get_message_handles( ) ).

  ENDMETHOD.


  METHOD /mbtools/if_logger~popup.
* See SBAL_DEMO_04_POPUP for ideas
    DATA relevant_profile TYPE bal_s_prof.
    DATA lt_log_handles TYPE bal_t_logh.

    APPEND me->handle TO lt_log_handles.

    IF profile IS SUPPLIED AND profile IS NOT INITIAL.
      relevant_profile = profile.
    ELSE.
      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = relevant_profile.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = relevant_profile
        i_t_log_handle      = lt_log_handles.
  ENDMETHOD.


  METHOD /mbtools/if_logger~s.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'S'
      importance    = importance ).
  ENDMETHOD.


  METHOD /mbtools/if_logger~save.
    CHECK me->settings->get_autosave( ) = abap_false.
    save_log( ).
  ENDMETHOD.


  METHOD /mbtools/if_logger~w.
    self = add(
      obj_to_log    = obj_to_log
      context       = context
      callback_form = callback_form
      callback_prog = callback_prog
      callback_fm   = callback_fm
      type          = 'W'
      importance    = importance ).
  ENDMETHOD.


  METHOD save_log.
    DATA log_handles TYPE bal_t_logh.
    DATA log_numbers TYPE bal_t_lgnm.
    DATA log_number TYPE bal_s_lgnm.

    INSERT me->handle INTO TABLE log_handles.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = log_handles
        i_2th_connection     = me->sec_connection
        i_2th_connect_commit = me->sec_connect_commit
      IMPORTING
        e_new_lognumbers     = log_numbers.
    IF me->db_number IS INITIAL.
      READ TABLE log_numbers INDEX 1 INTO log_number.
      me->db_number = log_number-lognumber.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
