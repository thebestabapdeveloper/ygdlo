
*&---------------------------------------------------------------------*
*&      Form  set_table_control
*&---------------------------------------------------------------------*
FORM set_table_control.
  CASE sy-dynnr.
    WHEN '0210'.
      DESCRIBE TABLE gt_item LINES tc_item-lines.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM set_item_line                                            *
*---------------------------------------------------------------------*
FORM set_item_line.
  CASE sy-dynnr.
    WHEN '0210'.
      IF tc_item-current_line > tc_item-lines.
        EXIT FROM STEP-LOOP.
      ENDIF.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM check_item_input                                         *
*---------------------------------------------------------------------*
FORM check_item_input.
*-- Satır uygunluk kontrolü
  CASE sy-dynnr.
    WHEN '0210'.
      IF sy-ucomm = 'DELE'.
        RETURN.
      ENDIF.
      IF sy-ucomm = 'OPT2'.
        RETURN.
      ENDIF.
      READ TABLE gt_item WITH KEY anln1 = *zhvl_fi_dv_isk_i_scr-anln1
                                  anln2 = *zhvl_fi_dv_isk_i_scr-anln2.
      IF sy-subrc EQ 0 AND sy-tabix NE tc_item-current_line.
        IF gv_excel_data = 'X'.
          MESSAGE i006 WITH sy-tabix.
        ELSE.
          MESSAGE e006 WITH sy-tabix.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM set_line_input                                           *
*---------------------------------------------------------------------*
FORM set_line_input.
  CASE sy-dynnr.
    WHEN '0210'.
      MOVE-CORRESPONDING *zhvl_fi_dv_isk_i_scr TO gt_item.
      MODIFY gt_item INDEX tc_item-current_line.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  tabstrip_active_tab_get
*&---------------------------------------------------------------------*
FORM tabstrip_active_tab_get.
  CASE sy-ucomm.
    WHEN gc_tab1.
      gs_tabstrip-active_tab = gc_tab1.
      gs_tabstrip-subscreen  = '0210'.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM hide_coloumns                                            *
*---------------------------------------------------------------------*
FORM hide_coloumns CHANGING pc LIKE tc_item.
  DATA: lf_wa TYPE cxtab_column.

  LOOP AT pc-cols INTO lf_wa.
    IF gv_onay_flag EQ space AND lf_wa-screen-group2 EQ 'ONY'.
      lf_wa-invisible = 'X'.
    ENDIF.
*    IF lf_wa-screen-group3 NE space .
*      IF *zhvl_fi_dv_dvth-vsart EQ gc_harici AND NOT 'H' CO lf_wa-screen-group3 .
*        lf_wa-invisible = 'X' .
*      ELSEIF *zhvl_fi_dv_dvth-vsart EQ gc_dahili AND NOT 'D' CO lf_wa-screen-group3 .
*        lf_wa-invisible = 'X' .
*      ELSEIF *zhvl_fi_dv_dvth-vsart EQ gc_musterek AND NOT 'M' CO lf_wa-screen-group3 .
*        lf_wa-invisible = 'X' .
*      ELSE .
*        lf_wa-invisible = space .
*      ENDIF .
*    ENDIF .

    MODIFY pc-cols FROM lf_wa.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  add_item
*&---------------------------------------------------------------------*
FORM add_item USING pv_count TYPE i.
  DO pv_count TIMES.
    CLEAR gt_item.
    PERFORM set_document_default_values USING 'I'.
    APPEND gt_item.
  ENDDO.
ENDFORM.
