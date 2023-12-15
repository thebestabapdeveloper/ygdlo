*&---------------------------------------------------------------------*
*&  Include           ZHVL_FI_DV_I_ISK_LIST_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_DB
*&---------------------------------------------------------------------*
FORM read_db.
  DATA:
    lt_isk_h     LIKE zhvl_fi_dv_isk_h OCCURS 0 WITH HEADER LINE,
    lt_isk_i     LIKE zhvl_fi_dv_isk_i OCCURS 0 WITH HEADER LINE,
    ls_isk_h_scr TYPE zhvl_fi_dv_isk_h_scr,
    ls_isk_i_scr TYPE zhvl_fi_dv_isk_i_scr,
    lv_lines     TYPE i,
    lv_lines_col TYPE i.

  DATA: BEGIN OF lt_anlz OCCURS 0,
          anln1 TYPE anlz-anln1,
          anln2 TYPE anlz-anln2,
*          kostl TYPE anlz-kostl,
        END OF lt_anlz.

  DATA: BEGIN OF lt_isk_i_col OCCURS 0,
          kostl TYPE anlz-kostl,
        END OF lt_isk_i_col.

  SELECT *
    FROM zhvl_fi_dv_isk_h
    INTO TABLE lt_isk_h
    WHERE bukrs EQ p_bukrs
      AND gjahr EQ p_gjahr
      AND belnr IN s_belnr
*      AND ggsbe IN s_ggsbe
*      AND agsbe IN s_agsbe
      AND ernam IN s_ernam
      AND erdat IN s_erdat
      AND aenam IN s_aenam
      AND aedat IN s_aedat.
*  AND kostl_gon IN s_gkostl
*  AND kostl_alan IN s_akostl .

* Yetki kontrolü
* -

*-- Header
  LOOP AT lt_isk_h .
    CLEAR gt_list .
    MOVE-CORRESPONDING lt_isk_h TO gt_list .

    CLEAR ls_isk_h_scr.
*    CALL FUNCTION 'ZHVL_FI_DV_ISK_GET_isk_h_TEXT'
*      EXPORTING
*        is_isk_h     = lt_isk_h
*      CHANGING
*        cs_isk_h_scr = ls_isk_h_scr.

    MOVE-CORRESPONDING lt_isk_h TO ls_isk_h_scr.

    REFRESH lt_isk_i.
    SELECT *
      FROM zhvl_fi_dv_isk_i
      INTO TABLE lt_isk_i
      WHERE bukrs EQ lt_isk_h-bukrs
        AND gjahr EQ lt_isk_h-gjahr
        AND belnr EQ lt_isk_h-belnr
    AND anln1 IN s_anln1
    AND anln2 IN s_anln2 .

    CHECK sy-subrc EQ 0 .

*********************************************************************
*Başlık seçili olduğunda eğer lt_isk_i tek satır değilse             *
*tüm kostl lerin aynı olup olmadığı kontrol edilecek. Eğer aynı ise *
*kostl_alan alanına bu değer yazılacak, değilse boş bırakılacak     *
*********************************************************************
    FREE lt_isk_i_col.                                              "*
    LOOP AT lt_isk_i.                                               "*
*      lt_isk_i_col-kostl = lt_isk_i-kostl.                         "*
      COLLECT lt_isk_i_col.                                         "*
    ENDLOOP.                                                        "*
    "*
    DESCRIBE TABLE lt_isk_i_col LINES lv_lines_col.                 "*
*******************************************************************"*
    SELECT anln1 anln2 "kostl
      FROM anlz
      INTO TABLE lt_anlz
      FOR ALL ENTRIES IN lt_isk_i
      WHERE anln1 = lt_isk_i-anln1
        AND anln2 = lt_isk_i-anln2
        AND bdatu = '99991231'.

*    CLEAR lv_lines.
*    DESCRIBE TABLE lt_isk_i LINES lv_lines.
*    LOOP AT lt_isk_i.
*      CLEAR lt_anlz.
*      READ TABLE lt_anlz
*        WITH KEY anln1 = lt_isk_i-anln1
*                 anln2 = lt_isk_i-anln2.
*      IF sy-subrc EQ 0.
*        lt_isk_h-kostl_gon = lt_anlz-kostl.
*      ENDIF.
*      IF p_rkalem = 'X' OR
*         lv_lines EQ 1  OR
*         lv_lines_col = 1.
*        lt_isk_h-kostl_alan = lt_isk_i-kostl.
*      ELSE.
*        CLEAR lt_isk_h-kostl_alan.
*      ENDIF.
*      ls_isk_h_scr-kostl_alan = lt_isk_h-kostl_alan.
*      ls_isk_h_scr-kostl_gon = lt_isk_h-kostl_gon.
*    ENDLOOP.

    CALL FUNCTION 'ZHVL_FI_DV_ISK_CALCULATE'
      TABLES
        it_isk_i     = lt_isk_i
      CHANGING
        cs_isk_h     = lt_isk_h
        cs_isk_h_scr = ls_isk_h_scr.

    MOVE-CORRESPONDING ls_isk_h_scr TO gt_list .

    IF p_rbasli EQ 'X' .
      APPEND gt_list .
    ELSEIF p_rkalem EQ 'X' .
      LOOP AT lt_isk_i.
        MOVE-CORRESPONDING lt_isk_i TO ls_isk_i_scr.
        CALL FUNCTION 'ZHVL_FI_DV_ISK_GET_ISKI_TEXT'
          EXPORTING
            is_isk_h     = lt_isk_h
          CHANGING
            cs_isk_i_scr = ls_isk_i_scr.

        gt_list-anln1 = lt_isk_i-anln1 .
        gt_list-anln2 = lt_isk_i-anln2 .
        gt_list-menge = lt_isk_i-menge .
        gt_list-meins = ls_isk_i_scr-meins .
        gt_list-gja_bchwrt = lt_isk_i-gja_bchwrt.
        gt_list-anlhtxt = ls_isk_i_scr-anlhtxt .
        APPEND gt_list.
      ENDLOOP.
    ENDIF .
  ENDLOOP.
ENDFORM.                    " READ_DB
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
FORM show_alv.
  DATA:
    ls_layout   TYPE slis_layout_alv,
    ls_print    TYPE slis_print_alv,
    ls_settings TYPE lvc_s_glay,
    ls_variant  LIKE disvariant.

  PERFORM prepare_field_catalog .
  PERFORM define_events .

  ls_layout-zebra = 'X' .
  ls_layout-colwidth_optimize = 'X' .
  ls_print-no_print_listinfos = 'X' .

  IF p_rbasli EQ 'X' .
    CONCATENATE sy-cprog '_BASLIK' INTO ls_variant-report .
  ELSEIF p_rkalem EQ 'X' .
    CONCATENATE sy-cprog '_KALEM' INTO ls_variant-report .
  ENDIF .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-cprog
      i_callback_user_command = 'USER_COMMAND_ALV'
*     i_callback_pf_status_set = 'STATUS_SET_ALV'
      it_events               = gt_events
      is_layout               = ls_layout
      it_fieldcat             = gt_fieldcat[]
      is_print                = ls_print
      i_grid_settings         = ls_settings
      is_variant              = ls_variant
      i_save                  = 'A'
    TABLES
      t_outtab                = gt_list.
ENDFORM.                    " SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM prepare_field_catalog.
  DATA ls_fieldcat LIKE LINE OF gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_structure_name       = 'ZHVL_FI_DV_S_ISK_LIST'
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = gt_fieldcat.

  LOOP AT gt_fieldcat INTO ls_fieldcat.
    CASE ls_fieldcat-fieldname.
      WHEN 'BELNR'.
        ls_fieldcat-hotspot = 'X'.
*      WHEN 'KOSTL_GON'.
*        IF sy-mandt EQ '100'.
*          ls_fieldcat-no_out = 'X'.
*        ENDIF.
*      WHEN 'KOSTL_ALAN'.
*        IF sy-mandt EQ '100'.
*          ls_fieldcat-no_out = 'X'.
*        ENDIF.
      WHEN 'ANLN1'   OR
           'ANLN2'   OR
           'ANLHTXT' OR
           'MENGE'   OR
           'MEINS'   OR
           'GJA_BCHWRT'.
        IF p_rbasli EQ abap_true.
          ls_fieldcat-no_out = 'X'.
        ELSE.
          ls_fieldcat-no_out = ' '.
        ENDIF.
    ENDCASE.

    MODIFY gt_fieldcat FROM ls_fieldcat.
  ENDLOOP.
ENDFORM.                    " PREPARE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  DEFINE_EVENTS
*&---------------------------------------------------------------------*
FORM define_events.
  DATA ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events.

* TOP_OF_PAGE
  DELETE gt_events WHERE name EQ slis_ev_top_of_page.
  ls_event-name = slis_ev_top_of_page.
  ls_event-form = 'TOP_OF_PAGE_ALV'.
  APPEND ls_event TO gt_events.
ENDFORM.                    " DEFINE_EVENTS
*---------------------------------------------------------------------*
*       FORM USER_COMMAND_ALV                                         *
*---------------------------------------------------------------------*
FORM user_command_alv USING ip_ucomm LIKE sy-ucomm
                            ip_selfield TYPE kkblo_selfield.
                                                            "#EC CALLED
  DATA:
    lv_index TYPE sy-tabix,
    ls_list  LIKE gt_list.

  CASE ip_ucomm .
    WHEN '&IC1' .
      READ TABLE gt_list INTO ls_list INDEX ip_selfield-tabindex .
      IF sy-subrc EQ 0 .
        lv_index = sy-tabix .
        PERFORM display_doc USING ls_list.
        MODIFY gt_list FROM ls_list INDEX lv_index .
      ENDIF .
  ENDCASE .

  CLEAR sy-ucomm.

  ip_selfield-refresh = 'X'.
  ip_selfield-col_stable = 'X'.
  ip_selfield-row_stable = 'X'.
ENDFORM .                    "user_command_alv
*---------------------------------------------------------------------*
*       FORM status_set_alv                                           *
*---------------------------------------------------------------------*
FORM status_set_alv USING is_extab TYPE kkblo_t_extab.
  DATA lt_tcode TYPE sy-tcode OCCURS 0 WITH HEADER LINE .

  SET PF-STATUS 'STANDARD' EXCLUDING lt_tcode.
ENDFORM.                    "status_set_alv
*---------------------------------------------------------------------*
*       FORM TOP_OF_LIST_ALV                                          *
*---------------------------------------------------------------------*
FORM top_of_page_alv.                                       "#EC CALLED
  DATA: lt_textpool    TYPE textpool OCCURS 0 WITH HEADER LINE,
        lt_vari_tab    TYPE rsparams OCCURS 0 WITH HEADER LINE,
        ls_line        TYPE slis_listheader,
        lt_top_of_page TYPE slis_t_listheader,
        lv_datum(10).

*-- Başlık
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title .
  APPEND ls_line TO lt_top_of_page .

*-- FM Alanı
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = TEXT-001 .
  ls_line-info =  p_bukrs .
  APPEND ls_line TO lt_top_of_page .

*-- Mali Yıl
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = TEXT-002 .
  ls_line-info =  p_gjahr .
  APPEND ls_line TO lt_top_of_page .

*-- Seçimlik
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = sy-cprog
    TABLES
      selection_table = lt_vari_tab
    EXCEPTIONS
      not_found       = 1
      no_report       = 2
      OTHERS          = 3.

  IF sy-subrc EQ 0 .
    READ TABLE lt_vari_tab WITH KEY low = 'X' .
    IF sy-subrc EQ 0 .
      READ TEXTPOOL sy-cprog INTO lt_textpool LANGUAGE sy-langu .
      IF sy-subrc EQ 0 .
        READ TABLE lt_textpool WITH KEY key = lt_vari_tab-selname .
        IF sy-subrc EQ 0 .
          CLEAR ls_line .
          ls_line-typ  = 'S' .
          ls_line-key  = TEXT-003 .
          ls_line-info = lt_textpool-entry+8 .
          APPEND ls_line TO lt_top_of_page .
        ENDIF .
      ENDIF .
    ENDIF .
  ENDIF .

*-- Raporun tarihi
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = TEXT-004 .
  WRITE sy-datum TO lv_datum .
  WRITE sy-uzeit TO ls_line-info .
  CONCATENATE lv_datum '/' ls_line-info
         INTO ls_line-info
    SEPARATED BY space .
  APPEND ls_line TO lt_top_of_page .

*--
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.
ENDFORM.                    "top_of_page_alv
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DOC
*&---------------------------------------------------------------------*
FORM display_doc USING ps_list LIKE gt_list .
  DATA:
    ls_isk_h     TYPE zhvl_fi_dv_isk_h,
    ls_isk_h_scr TYPE zhvl_fi_dv_isk_h_scr.

  IF gv_onay_flag EQ 'X' .
    p_mode = '06' .
  ENDIF .

  SET PARAMETER ID 'BUK' FIELD ps_list-bukrs .
  SET PARAMETER ID 'GJR' FIELD ps_list-gjahr .
  SET PARAMETER ID 'ZHVL_FI_DV_ISK_BELNR' FIELD ps_list-belnr .
  SET PARAMETER ID 'ZHVL_FI_MODE' FIELD p_mode .
  CALL TRANSACTION 'ZHVL_FI_DV_ISK_03'
                                  AND SKIP FIRST SCREEN . "#EC CI_CALLTA

  IF gv_onay_flag EQ 'X' .
    SELECT SINGLE dstat
      FROM zhvl_fi_dv_isk_h
      INTO ps_list-dstat
      WHERE bukrs EQ ps_list-bukrs
    AND gjahr EQ ps_list-gjahr
    AND belnr EQ ps_list-belnr .

    MOVE-CORRESPONDING ps_list TO ls_isk_h .

    CALL FUNCTION 'ZHVL_FI_DV_ISK_CALCULATE'
      CHANGING
        cs_isk_h     = ls_isk_h
        cs_isk_h_scr = ls_isk_h_scr.

    ps_list-dstatt = ls_isk_h_scr-dstatt .
  ENDIF .
ENDFORM.                    " DISPLAY_DOC
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM authority_check .

  DATA : lt_gsber TYPE gsber OCCURS 0 WITH HEADER LINE .

* Muhasebe belgesi: Şirket kodları için yetki
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'BUKRS' FIELD p_bukrs
           ID 'ACTVT' FIELD '03' .
  IF sy-subrc NE 0 .
* Şirket kodu & için & işlem yetkiniz yok!
    MESSAGE e039 WITH p_bukrs '03' .
  ENDIF .

*  CLEAR : lt_gsber , lt_gsber[] .
*  SELECT gsber
*    FROM tgsb
*  INTO TABLE lt_gsber
*  WHERE gsber IN s_ggsbe .
*
*  LOOP AT lt_gsber .
*    AUTHORITY-CHECK OBJECT 'F_BKPF_GSB'
*      ID 'ACTVT' FIELD '03'
*      ID 'GSBER' FIELD lt_gsber .
*
*    IF sy-subrc NE 0 .
** İş alanı & için & işlem yetkiniz yok!
*      MESSAGE e038 WITH lt_gsber '03'.
*      EXIT .
*    ENDIF.
*  ENDLOOP .

ENDFORM.                    " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_KOSTL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_kostl .
*  SELECT *
*    FROM csks
*    INTO TABLE @DATA(lt_data_gon)
*      UP TO 1 ROWS
*   WHERE kokrs = '1000'
*     AND kostl IN @s_gkostl
*  AND gsber IN @s_ggsbe
*  ORDER BY datbi DESCENDING, datab DESCENDING.
*  IF sy-subrc NE 0.
*    MESSAGE e040 .
*  ENDIF .
*
*  IF s_agsbe IS NOT INITIAL.
*    SELECT *
*      FROM csks
*      INTO TABLE @DATA(lt_data_alan)
*        UP TO 1 ROWS
*     WHERE kokrs = '1000'
*       AND kostl IN @s_akostl
*    AND gsber IN @s_agsbe
*    ORDER BY datbi DESCENDING, datab DESCENDING.
*    IF sy-subrc NE 0.
*      MESSAGE e041 .
*    ENDIF .
*  ENDIF.
ENDFORM.

FORM init .
  IF sy-tcode CP '*ONAY' .
    gv_onay_flag = 'X' .
    sy-title = TEXT-005 .
  ENDIF .
ENDFORM.
