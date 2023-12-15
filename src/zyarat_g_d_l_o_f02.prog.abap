
*---------------------------------------------------------------------*
*       FORM initialize                                               *
*---------------------------------------------------------------------*
FORM initialize.
  DATA: lv_offset TYPE i.

  CHECK gv_mode IS INITIAL.

  lv_offset = strlen( sy-tcode ) - 2.

  GET PARAMETER ID 'ZHVL_FI_MODE' FIELD gv_mode.
  IF sy-subrc NE 0 OR gv_mode IS INITIAL.
    gv_mode = sy-tcode+lv_offset(2).
  ELSE.
    SET PARAMETER ID 'ZHVL_FI_MODE' FIELD space.
    gv_listflag = 'X'.
  ENDIF.

*-- Onaylama algılaması
  CLEAR gv_onay_flag.
  IF gv_mode EQ '06'.
    gv_mode = '02'.
    gv_onay_flag = 'X'.
  ENDIF.

  PERFORM set_title.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM set_title                                                *
*---------------------------------------------------------------------*
FORM set_title.
  SEARCH sy-title FOR '- '.
  IF sy-subrc EQ 0.
    sy-title = sy-title(sy-fdpos).
  ENDIF.

  CASE gv_mode.
    WHEN gc_create.
      CONCATENATE sy-title TEXT-008
                  INTO sy-title
                  SEPARATED BY space.
    WHEN gc_change.
      CONCATENATE sy-title TEXT-009
                  INTO sy-title
                  SEPARATED BY space.
    WHEN gc_display.
      CONCATENATE sy-title TEXT-010
                  INTO sy-title
                  SEPARATED BY space.
    WHEN gc_delete.
      CONCATENATE sy-title TEXT-011
                  INTO sy-title
                  SEPARATED BY space.
    WHEN OTHERS.
      LEAVE PROGRAM.
  ENDCASE.

  gv_title = sy-title.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM user_command_exit                                        *
*---------------------------------------------------------------------*
FORM user_command_exit.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      PERFORM exit_program USING gv_mode
                                 space.
    WHEN gc_list.
      PERFORM submit_list.
    WHEN gc_create
      OR gc_change
      OR gc_display
      OR gc_delete.
      PERFORM exit_program USING sy-ucomm
                                 space.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM popup_to_confirm                                         *
*---------------------------------------------------------------------*
FORM popup_to_confirm USING p_popup_type TYPE icon-name
                            p_title      TYPE text60
                            p_text       TYPE text132.

  DATA:
    lv_answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING titlebar              = p_title
              text_question         = p_text
              default_button        = '1'
              display_cancel_button = space
              popup_type            = p_popup_type
    IMPORTING answer                = lv_answer.

  IF lv_answer EQ '1'.
    sy-subrc = 0.
  ELSE.
    sy-subrc = 4.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM check_keys                                               *
*---------------------------------------------------------------------*
FORM check_keys.
  PERFORM get_customizing.

  CASE gv_mode.
    WHEN gc_create.
      PERFORM check_values.
*      PERFORM authority_check .
      PERFORM set_document_default_values USING 'H'.
    WHEN gc_change OR gc_display OR gc_delete.
      PERFORM get_document.
  ENDCASE.

  PERFORM authority_check.

  IF gv_mode NE gc_display.
    PERFORM lock.
  ENDIF.

  PERFORM calculate_screen_values.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM check_field_value                                        *
*---------------------------------------------------------------------*
FORM check_field_value.
  PERFORM get_isk_h_text.
  PERFORM calculate_screen_values.
  IF sy-mandt EQ '200'.
*    PERFORM check_kostl.
  ENDIF.
  PERFORM anln1_help.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  check_error
*&---------------------------------------------------------------------*
FORM check_error.
  PERFORM check_document.
ENDFORM.                    " check_all

*&---------------------------------------------------------------------*
*&      Form  CHECK_VALUES
*&---------------------------------------------------------------------*
FORM check_values.
  CASE gv_mode.
    WHEN gc_create.
    WHEN gc_change OR gc_display OR gc_delete.
  ENDCASE.
ENDFORM.
