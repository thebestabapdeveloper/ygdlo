
FORM user_command.
  CALL FUNCTION 'ZHVL_FI_GEN_BATCH_MODE'
    IMPORTING
      ev_batchmode = gv_batchmode.

  CASE sy-dynnr.
*---------------------------------------------------------------------
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN space.
          CALL SCREEN '0200'.
      ENDCASE.
*---------------------------------------------------------------------
    WHEN '0205'.
      CASE sy-ucomm.
        WHEN 'CANC' OR 'OK'.
          LEAVE TO SCREEN 0.
      ENDCASE.
*---------------------------------------------------------------------
    WHEN '0200'.
      CASE sy-ucomm.
        WHEN gc_list.
          PERFORM submit_list.
        WHEN gc_save.
          PERFORM save USING space
                             space.
        WHEN gc_pick.
          PERFORM display_doc.
        WHEN gc_check.
          PERFORM check_error.
        WHEN gc_header.
          CALL SCREEN '0205' STARTING AT 3 2 ENDING AT 46 8.
        WHEN gc_print.
*          SUBMIT zhvl_fi_dv_dvt_prnt
*                  WITH p_belnr = *zhvl_fi_dv_isk_h-belnr
*                  WITH p_bukrs = *zhvl_fi_dv_isk_h-bukrs
*                  WITH p_gjahr = *zhvl_fi_dv_isk_h-gjahr
*                  AND RETURN .
      ENDCASE.
*---------------------------------------------------------------------
    WHEN '0210'.
      CASE sy-ucomm.
        WHEN gc_crea.
          PERFORM add_item USING 1.
        WHEN gc_sele.
          gt_item-mark = 'X'.
          MODIFY gt_item TRANSPORTING mark WHERE mark EQ space.
        WHEN gc_dese.
          gt_item-mark = space.
          MODIFY gt_item TRANSPORTING mark WHERE mark EQ 'X'.
        WHEN gc_dele.
          DELETE gt_item WHERE mark EQ 'X'.
        WHEN gc_pick.
          PERFORM display_doc.
        WHEN gc_onayla.
          PERFORM create_dv_transfer.
        WHEN gc_upload.
          PERFORM upload_list.
      ENDCASE.
*---------------------------------------------------------------------
  ENDCASE.

*--
  CLEAR sy-ucomm.
ENDFORM.

FORM exit_program USING VALUE(pv_mode)
                        pv_ask_data_lost TYPE xfeld.

  DATA: lv_tcode TYPE tcode.

  IF sy-dynnr EQ '0200'.
    IF gv_datachanged EQ 'X'.
      IF pv_ask_data_lost EQ space.
        PERFORM popup_to_confirm USING gc_icon_warning
                                       TEXT-003
                                       TEXT-006.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF     sy-ucomm NE gc_create AND sy-ucomm NE gc_change
     AND sy-ucomm NE gc_display
     AND sy-ucomm NE gc_delete
     AND (    sy-ucomm    EQ 'EXIT'
           OR sy-dynnr    EQ '0100'
           OR gv_listflag EQ 'X' ).
    LEAVE PROGRAM.
  ELSE.
    CONCATENATE 'ZHVL_FI_DV_ISK_' pv_mode INTO lv_tcode.
    LEAVE TO TRANSACTION lv_tcode.
  ENDIF.
ENDFORM.

FORM set_status.
  DATA: lt_excluding TYPE rsmpe-func OCCURS 0 WITH HEADER LINE,
        lv_title     LIKE gv_title.

  lt_excluding = gv_mode.
  APPEND lt_excluding.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE gv_mode.
        WHEN gc_create.
          lt_excluding = gc_list.
          APPEND lt_excluding.
      ENDCASE.
    WHEN '0200' OR '0210'.
      IF gv_datachanged IS NOT INITIAL.
        lt_excluding = gc_print.
        APPEND lt_excluding.
      ENDIF.
      IF gv_mode EQ gc_create.
        lt_excluding = gc_onayla.
        APPEND lt_excluding.
        lt_excluding = gc_header.
        APPEND lt_excluding.
        lt_excluding = gc_print.
        APPEND lt_excluding.
      ENDIF.
      IF gv_mode EQ gc_display.
        lt_excluding = gc_save.
        APPEND lt_excluding.
      ENDIF.
      IF gv_mode EQ gc_display OR gv_mode EQ gc_delete.
        lt_excluding = gc_onayla.
        APPEND lt_excluding.
        lt_excluding = gc_check.
        APPEND lt_excluding.
      ENDIF.
      IF gv_mode EQ gc_change AND (    gv_onay_flag            EQ space
                                    OR *zhvl_fi_dv_isk_h-dstat EQ gc_iskat_durum-onayandi ).
        lt_excluding = gc_onayla.
        APPEND lt_excluding.
      ENDIF.
  ENDCASE.

  SET PF-STATUS sy-dynnr EXCLUDING lt_excluding.

  IF *zhvl_fi_dv_isk_h-dstat EQ gc_iskat_durum-ters_kayit AND sy-dynnr EQ '0200'.
    CONCATENATE gv_title TEXT-012 INTO lv_title
                SEPARATED BY space.
    SET TITLEBAR sy-dynnr WITH lv_title.
  ELSE.
    SET TITLEBAR sy-dynnr WITH gv_title.
  ENDIF.
ENDFORM.

FORM get_document.
*-- Başlık okunur
  SELECT SINGLE * FROM zhvl_fi_dv_isk_h
    INTO *zhvl_fi_dv_isk_h
    WHERE bukrs EQ *zhvl_fi_dv_isk_h-bukrs
      AND gjahr EQ *zhvl_fi_dv_isk_h-gjahr
      AND belnr EQ *zhvl_fi_dv_isk_h-belnr.

  IF sy-subrc NE 0.
    MESSAGE e000.
*   Belge bulunamadı
  ENDIF.

  PERFORM authority_check.

*-- Silindi ise
  IF *zhvl_fi_dv_isk_h-dstat EQ gc_iskat_durum-ters_kayit.
    IF gv_mode NE gc_display.
      gv_mode = gc_display.
      PERFORM set_title.
    ENDIF.
    MESSAGE w001 WITH *zhvl_fi_dv_isk_h-belnr.
*   & belgesi silinmiştir
  ENDIF.

*-- Kalemler okunur
  SELECT * FROM zhvl_fi_dv_isk_i
    INTO CORRESPONDING FIELDS OF TABLE gt_item
    WHERE bukrs EQ *zhvl_fi_dv_isk_h-bukrs
      AND gjahr EQ *zhvl_fi_dv_isk_h-gjahr
      AND belnr EQ *zhvl_fi_dv_isk_h-belnr.

  LOOP AT gt_item INTO *zhvl_fi_dv_isk_i_scr.
    PERFORM get_isk_i_text.
    MODIFY gt_item FROM *zhvl_fi_dv_isk_i_scr.
  ENDLOOP.

*-- Tanımlar okunur
  PERFORM get_isk_h_text.
ENDFORM.

FORM lock.
  IF *zhvl_fi_dv_isk_h-belnr IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EZHVL_FI_DV_ISKH'
    EXPORTING
      mode_zhvl_fi_dv_isk_h = 'E'
      mandt                 = sy-mandt
      bukrs                 = *zhvl_fi_dv_isk_h-bukrs
      gjahr                 = *zhvl_fi_dv_isk_h-gjahr
      belnr                 = *zhvl_fi_dv_isk_h-belnr
    EXCEPTIONS
      foreign_lock          = 1
      system_failure        = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM unlock.
  CALL FUNCTION 'DEQUEUE_EZHVL_FI_DV_ISKH'
    EXPORTING
      mode_zhvl_fi_dv_isk_h = 'E'
      mandt                 = sy-mandt
      bukrs                 = *zhvl_fi_dv_isk_h-bukrs
      gjahr                 = *zhvl_fi_dv_isk_h-gjahr
      belnr                 = *zhvl_fi_dv_isk_h-belnr.
ENDFORM.

FORM modify_screen.
  LOOP AT SCREEN.
    IF screen-name = 'TAB1'.
      CONTINUE.
    ENDIF.

    CASE sy-dynnr.
      WHEN '0100'.
        CASE gv_mode.
          WHEN gc_create.
            IF screen-group1 EQ 'C'.
              screen-active = 0.
            ENDIF.
          WHEN OTHERS.
            IF screen-group1 EQ 'D'.
              screen-active = 0.
            ENDIF.
        ENDCASE.
      WHEN '0200' OR '0210'.
        IF sy-mandt EQ '100'.
          IF screen-group4 EQ 'KOS'.
            screen-invisible = 1.
            screen-input     = 0.
          ENDIF.
        ENDIF.
        CASE gv_mode.
          WHEN gc_create.
          WHEN gc_change.
            IF gv_onay_flag EQ 'X' AND screen-group2 EQ 'CLS'.
              screen-input = 0.
            ENDIF.
            IF sy-mandt EQ '200' AND *zhvl_fi_dv_isk_h-dstat EQ 'O' AND screen-group3 EQ 'DIS'.
              screen-input = 0.
            ENDIF.
*            IF *zhvl_fi_dv_isk_i_scr-awref IS NOT INITIAL AND screen-group2 EQ 'ONY' .
*              screen-input = 0 .
*            ENDIF .
            IF sy-mandt EQ '200'.
*              IF screen-name EQ '*ZHVL_FI_DV_ISK_H-KOSTL_ALAN' AND sy-tcode NE 'ZHVL_FI_DV_ISK_02'.
*                screen-required = 1.
*              ENDIF.
            ENDIF.

          WHEN gc_display OR gc_delete.
            screen-input = 0.
        ENDCASE.
    ENDCASE.

*-- Görünür olup olmaması kontrolü için group3 kullanılır
*    IF screen-group3 NE space .
*      IF *zhvl_fi_dv_isk_h-vsart EQ gc_harici AND NOT 'H' CO screen-group3 .
*        screen-active = 0 .
*      ENDIF .
*      IF *zhvl_fi_dv_isk_h-vsart EQ gc_dahili AND NOT 'D' CO screen-group3 .
*        screen-active = 0 .
*      ENDIF .
*      IF *zhvl_fi_dv_isk_h-vsart EQ gc_musterek AND NOT 'M' CO screen-group3 .
*        screen-active = 0 .
*      ENDIF .
*    ENDIF .
*
**-- Girişe açık olup olmaması kontrolü için group1 kullanılır
*    IF sy-dynnr NE '0100' .
*      IF screen-group1 NE space .
*        IF *zhvl_fi_dv_isk_h-vsart EQ gc_harici AND 'H' CA screen-group1 .
*          screen-input = 0 .
*        ENDIF .
*        IF *zhvl_fi_dv_isk_h-vsart EQ gc_dahili AND 'D' CA screen-group1 .
*          screen-input = 0 .
*        ENDIF .
*        IF *zhvl_fi_dv_isk_h-vsart EQ gc_musterek AND 'M' CA screen-group1 .
*          screen-input = 0 .
*        ENDIF .
*      ENDIF .
*    ENDIF .
*
**-- Zorunlu olup olmaması kontrolü için group4 kullanılır
*    IF sy-dynnr NE '0100' .
*      IF screen-group4 NE space .
*        IF *zhvl_fi_dv_isk_h-vsart EQ gc_harici AND 'H' CA screen-group4 .
*          screen-required = 0 .
*        ENDIF .
*        IF *zhvl_fi_dv_isk_h-vsart EQ gc_dahili AND 'D' CA screen-group4 .
*          screen-required = 0 .
*        ENDIF .
*        IF *zhvl_fi_dv_isk_h-vsart EQ gc_musterek AND 'M' CA screen-group4 .
*          screen-required = 0 .
*        ENDIF .
*      ENDIF .
*    ENDIF .

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

FORM save USING p_no_ask     TYPE xfeld
                p_no_control TYPE xfeld.

  DATA: ls_isk_i TYPE zhvl_fi_dv_isk_i.

*-- Ters kayıt
  IF gv_mode EQ gc_delete.
    PERFORM reverse.
    EXIT.
  ENDIF.

*-- Kontrol
  IF p_no_control EQ space.
    PERFORM check_document.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDIF.

*-- Onay
  IF p_no_ask EQ space.
    PERFORM popup_to_confirm USING gc_icon_question
                                   TEXT-003
                                   TEXT-007.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDIF.

*- Başlangı durumu
  IF *zhvl_fi_dv_isk_h-dstat IS INITIAL.
    *zhvl_fi_dv_isk_h-dstat = gc_iskat_durum-talep_olusturuldu.
  ENDIF.

*-- Son değiştiren
  *zhvl_fi_dv_isk_h-aenam = sy-uname.
  *zhvl_fi_dv_isk_h-aedat = sy-datum.

*-- Belge numarası
  IF gv_mode EQ gc_create.
*-- Yaratan ve belge tarihleri
*     *zhvl_fi_dv_isk_h-bldat = sy-datum .
    *zhvl_fi_dv_isk_h-ernam = sy-uname.
    *zhvl_fi_dv_isk_h-erdat = sy-datum.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZHVL_FI_09'
        quantity                = '1'
        subobject               = *zhvl_fi_dv_isk_h-bukrs
        toyear                  = *zhvl_fi_dv_isk_h-gjahr
      IMPORTING
        number                  = *zhvl_fi_dv_isk_h-belnr
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.
  ENDIF.

*-- DB kayıt işlemi
  MODIFY zhvl_fi_dv_isk_h FROM *zhvl_fi_dv_isk_h.
  IF sy-subrc EQ 0.
*-- Kalemler
    DELETE FROM zhvl_fi_dv_isk_i
          WHERE bukrs EQ *zhvl_fi_dv_isk_h-bukrs
            AND gjahr EQ *zhvl_fi_dv_isk_h-gjahr
            AND belnr EQ *zhvl_fi_dv_isk_h-belnr.

    CLEAR ls_isk_i.
    LOOP AT gt_item.
      MOVE-CORRESPONDING gt_item TO ls_isk_i.
      MOVE-CORRESPONDING *zhvl_fi_dv_isk_h TO ls_isk_i.

*      IF sy-mandt EQ '200' AND *zhvl_fi_dv_isk_h-kostl_alan IS NOT INITIAL.
*        ls_isk_i-kostl = *zhvl_fi_dv_isk_h-kostl_alan.
*      ENDIF.

** Sıra no alınması
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*        EXPORTING
*          nr_range_nr             = '01'
*          object                  = 'ZFI_LINID'
*        IMPORTING
*          number                  = ls_isk_i-linid
*        EXCEPTIONS
*          interval_not_found      = 1
*          number_range_not_intern = 2
*          object_not_found        = 3
*          quantity_is_0           = 4
*          quantity_is_not_1       = 5
*          interval_overflow       = 6
*          buffer_overflow         = 7
*          OTHERS                  = 8.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*
*        EXIT.
*      ENDIF.

      MODIFY zhvl_fi_dv_isk_i FROM ls_isk_i.
    ENDLOOP.

    CLEAR: gv_datachanged,
           gv_excel_data.

    MESSAGE s002 WITH *zhvl_fi_dv_isk_h-belnr.
*   & beglesi kaydedildi
  ELSE.
    MESSAGE w003 WITH *zhvl_fi_dv_isk_h-belnr.
*   & belgesi kaydedilemedi!
  ENDIF.

  SET PARAMETER ID 'ZHVL_FI_DV_ISK_BELNR' FIELD *zhvl_fi_dv_isk_h-belnr.
  COMMIT WORK AND WAIT.

  IF p_no_ask EQ space.
    PERFORM exit_program USING gv_mode
                               'X'.
  ELSE.
    PERFORM calculate_screen_values.
  ENDIF.

  sy-subrc = 0.
ENDFORM.

FORM reverse.
*-- Onay
  PERFORM popup_to_confirm USING gc_icon_critical
                                 TEXT-003
                                 TEXT-013.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*-- Muhasebe belgelerinin ters kaydı alınır
  IF gv_onay_flag EQ 'X'.
    PERFORM reverse_dv_transfer.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDIF.

*-- Silme işlemi
  UPDATE zhvl_fi_dv_isk_h SET dstat = gc_iskat_durum-ters_kayit
                              aenam = sy-uname
                              aedat = sy-datum
   WHERE bukrs EQ *zhvl_fi_dv_isk_h-bukrs
     AND gjahr EQ *zhvl_fi_dv_isk_h-gjahr
     AND belnr EQ *zhvl_fi_dv_isk_h-belnr.

  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.

    MESSAGE s004 WITH *zhvl_fi_dv_isk_h-belnr.
*   & belgesi ters kaydı alındı
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  LEAVE TO SCREEN 0.
ENDFORM.

FORM submit_list.
  SUBMIT zhvl_fi_dv_r_isk_list VIA SELECTION-SCREEN AND RETURN
    WITH p_mode EQ gv_mode .
ENDFORM.

*
FORM get_isk_h_text.
*  CALL FUNCTION 'ZHVL_FI_DV_DVT_GET_isk_h_TEXT'
*    EXPORTING
*      is_isk_h     = *zhvl_fi_dv_isk_h
*    CHANGING
*      cs_isk_h_scr = *zhvl_fi_dv_isk_h_scr.
ENDFORM.

FORM get_isk_i_text.
  CALL FUNCTION 'ZHVL_FI_DV_ISK_GET_ISKI_TEXT'
    EXPORTING
      is_isk_h     = *zhvl_fi_dv_isk_h
    CHANGING
      cs_isk_i_scr = *zhvl_fi_dv_isk_i_scr.
ENDFORM.

FORM activate_service.
*  STATICS:
*    sc_object TYPE REF TO cl_gos_manager .
*  DATA:
*    lv_mode TYPE sgs_rwmod ,
*    ls_object TYPE borident ,
*    lt_service TYPE tgos_sels ,
*    ls_service TYPE sgos_sels ,
*    lc_object TYPE REF TO cl_gos_manager .
*
*  CHECK sc_object IS NOT BOUND .
*
*  ls_object-objtype = 'ZHVL_FI_01' .
*
*  CONCATENATE *zhvl_fi_dv_isk_h-bukrs
*              *zhvl_fi_dv_isk_h-gjahr
*              *zhvl_fi_dv_isk_h-belnr INTO ls_object-objkey .
*
*  ls_service-sign   = 'E'.
*  ls_service-option = 'CP'.
*  ls_service-low = 'WF_*' .
*  APPEND ls_service TO lt_service .
*  ls_service-low = 'SO_*' .
*  APPEND ls_service TO lt_service .
*  ls_service-low = 'MYO*' .
*  APPEND ls_service TO lt_service .
*  ls_service-option = 'EQ'.
*  ls_service-low = 'INFO_SERVICE' .
*  APPEND ls_service TO lt_service .
*  ls_service-low = 'BARCODE' .
*  APPEND ls_service TO lt_service .
*  ls_service-low = 'SRELATIONS' .
*  APPEND ls_service TO lt_service .
*  ls_service-low = 'ARL_LINK' .
*  APPEND ls_service TO lt_service .
*
*  CALL METHOD cl_gos_publication=>unpublish .
*
*  IF gv_mode EQ gc_display OR gv_mode EQ gc_delete .
*    lv_mode = 'D' .
*  ELSE .
*    lv_mode = 'E' .
*  ENDIF .
*
*  CREATE OBJECT sc_object
*    EXPORTING
*      is_object            = ls_object
*      it_service_selection = lt_service
*      ip_mode              = lv_mode
*      ip_no_commit         = space
*    EXCEPTIONS
*      object_invalid       = 1
*      callback_invalid     = 2
*      OTHERS               = 3.
*
*  IF sy-subrc <> 0 .
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
*  ENDIF .
ENDFORM.

*---------------------------------------------------------------------*
*       FORM display_doc                                              *
*---------------------------------------------------------------------*
FORM display_doc.
  DATA: lv_cursor_field TYPE c LENGTH 30,
        lv_value        TYPE d021s-fnam,
        ls_item         LIKE gt_item,
        lv_line         TYPE i,
        lv_cursor_line  TYPE i.

  GET CURSOR FIELD lv_cursor_field VALUE lv_value.

  IF lv_value IS INITIAL.
    RETURN.
  ENDIF.

  CASE lv_cursor_field.
    WHEN '*zhvl_fi_dv_isk_i_SCR-ANLN1'.
      SET PARAMETER ID 'AN1' FIELD lv_value.
      SET PARAMETER ID 'AN2' FIELD '0'.
      SET PARAMETER ID 'BUK' FIELD *zhvl_fi_dv_isk_h-bukrs.
      CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA
    WHEN '*zhvl_fi_dv_isk_i_SCR-AWREF'.
      GET CURSOR LINE lv_line.
      lv_cursor_line = tc_item-top_line + lv_line - 1.
      READ TABLE gt_item INTO ls_item INDEX lv_cursor_line.
      SET PARAMETER ID 'AN1' FIELD ls_item-anln1.
      SET PARAMETER ID 'AN2' FIELD ls_item-anln2.
      SET PARAMETER ID 'GJR' FIELD *zhvl_fi_dv_isk_h-bldat(4).
      SET PARAMETER ID 'BUK' FIELD *zhvl_fi_dv_isk_h-bukrs.
      CALL TRANSACTION 'AB03' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA

  ENDCASE.
ENDFORM.

FORM authority_check.
* Muhasebe belgesi: Şirket kodları için yetki
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                  ID 'BUKRS' FIELD *zhvl_fi_dv_isk_h-bukrs
                  ID 'ACTVT' FIELD gv_mode.
  IF sy-subrc NE 0.
* Şirket kodu & için & işlem yetkiniz yok!
    MESSAGE e039 WITH *zhvl_fi_dv_isk_h-bukrs gv_mode.
  ENDIF.
ENDFORM.

FORM calculate_screen_values.
  CALL FUNCTION 'ZHVL_FI_DV_ISK_CALCULATE'
    TABLES
      it_isk_i_scr = gt_item
    CHANGING
      cs_isk_h     = *zhvl_fi_dv_isk_h
      cs_isk_h_scr = *zhvl_fi_dv_isk_h_scr.
  *zhvl_fi_dv_isk_h_scr-dstatt = *zhvl_fi_dv_isk_h_scr-dstatt(3) && '@'.
ENDFORM.

FORM get_customizing.
*-- Şirket kodları
  SELECT SINGLE * FROM t001
    WHERE bukrs EQ *zhvl_fi_dv_isk_h-bukrs.

  PERFORM get_isk_h_text.
ENDFORM.

FORM set_document_default_values USING pv_what_for TYPE char1.
  CASE pv_what_for.
    WHEN 'H'. " Başlık
      IF gv_mode EQ gc_create.
*       *zhvl_fi_dv_isk_h-waers = t001-waers .
      ENDIF.
    WHEN 'I'. " Kalem
      MOVE-CORRESPONDING *zhvl_fi_dv_isk_h TO gt_item.
  ENDCASE.
ENDFORM.

FORM check_document.
  CALL FUNCTION 'ZHVL_FI_DV_ISK_CHECK'
    EXPORTING
      is_isk_h        = *zhvl_fi_dv_isk_h
      is_isk_h_scr    = *zhvl_fi_dv_isk_h_scr
      iv_show_message = 'X'
    TABLES
      it_isk_i_scr    = gt_item
    EXCEPTIONS
      error           = 1
      OTHERS          = 2.
ENDFORM.

FORM check_data_changed.
  gv_datachanged = 'X'.
ENDFORM.

FORM create_dv_transfer.
  DATA:
    lv_error       TYPE c LENGTH 1,
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    ls_item        LIKE gt_item,
*    lv_index       TYPE sy-tabix,
*    lt_message     TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
    lt_message_all TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  CLEAR gv_error.
*-- Kontrol
  PERFORM check_document.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*-- Seçim kontrolü
  LOOP AT gt_item INTO ls_item WHERE mark EQ 'X'.
*                                 AND kostl IS NOT INITIAL
*                                 AND awref IS INITIAL .
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE w009.
    EXIT.
  ENDIF.

*-- Onay
  PERFORM popup_to_confirm USING gc_icon_warning
                                 TEXT-003
                                 TEXT-005.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*-- BATCH
*  lv_error = 'X' .
*  LOOP AT gt_item INTO ls_item WHERE mark EQ 'X'.
**                                 AND kostl IS NOT INITIAL
**                                 AND awref IS INITIAL .
*    lv_index = sy-tabix .
*
*    REFRESH lt_message .
*    CALL FUNCTION 'ZHVL_FI_DV_DVT_TRANSFER_CREATE'
*      EXPORTING
*        is_isk_h      = *zhvl_fi_dv_isk_h
*        iv_batchmode = gv_batchmode
*      TABLES
*        et_message   = lt_message
*      CHANGING
*        cs_isk_i_scr  = ls_item
*      EXCEPTIONS
*        error        = 1
*        OTHERS       = 2.

*    IF sy-subrc EQ 0 .
*      CLEAR lv_error .
*      READ TABLE lt_message WITH KEY msgid = 'F5' msgnr = '201'.
*      IF sy-subrc NE 0.
*        MODIFY gt_item FROM ls_item INDEX lv_index TRANSPORTING awref .
*      ELSE.
*        lv_error = 'X' .
*      ENDIF.
*      LOOP AT lt_message.
*        IF lt_message-msgtyp = 'E'.
*          lv_error = 'X' .
*        ENDIF.
*      ENDLOOP.
*    ENDIF .
*
*    APPEND LINES OF lt_message TO lt_message_all .
*  ENDLOOP .

  IF lv_error EQ 'X'.
    gv_error = lv_error.
    CALL FUNCTION 'ZHVL_FI_GEN_SHOW_MESSAGES'
      TABLES
        it_message = lt_message_all.

    MESSAGE w010.
  ELSE.
    PERFORM calculate_screen_values.
    PERFORM save USING 'X'
                       'X'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    MESSAGE s008.
*   Transfer belgesi oluşturulmuştur
  ENDIF.

*  CALL FUNCTION 'ZHVL_FI_GEN_SHOW_MESSAGES'
*    TABLES
*      it_message = lt_message_all.
ENDFORM.

FORM reverse_dv_transfer.
  DATA:
    lv_belnr TYPE zhvl_fi_si_docf-belnr_rf.

*    CALL FUNCTION 'ZHVL_FI_DV_DVT_TRANSFER_REVRSE'
*      EXPORTING
*        iv_bukrs = ls_item-bukrs_rf
*        iv_gjahr = ls_item-gjahr_rf
*        iv_belnr = ls_item-belnr_rf
*      IMPORTING
*        ev_belnr = lv_belnr.

  IF lv_belnr IS INITIAL.
    ROLLBACK WORK.
    sy-subrc = 4.
  ELSE.
    sy-subrc = 0.
  ENDIF.
ENDFORM.

FORM upload_list.
*  DATA : lv_file_path LIKE rlgrap-filename,
*         lt_data      TYPE TABLE OF alsmex_tabline WITH HEADER LINE.
*
*  CONSTANTS:
*    lc_scol TYPE i VALUE '1',
*    lc_srow TYPE i VALUE '2',
*    lc_ecol TYPE i VALUE '4',
*    lc_erow TYPE i VALUE '100'.
*
*  PERFORM select_file USING lv_file_path.
*
*  PERFORM get_from_excel
*          TABLES lt_data
*          USING  lv_file_path
*                 lc_scol
*                 lc_srow
*                 lc_ecol
*                 lc_erow.
*
*  PERFORM append_to_itab TABLES lt_data
*                         USING  lc_scol.
ENDFORM.

FORM select_file USING iv_file_path.
  DATA: it_tab   TYPE STANDARD TABLE OF file_table,
        lw_file  LIKE LINE OF it_tab,
        gd_subrc TYPE i.

  cl_gui_frontend_services=>file_open_dialog( EXPORTING window_title     = 'Lütfen Bir excel dosyası seçiniz'
                                                        default_filename = '.XLS'
                                                        multiselection   = ' '
                                              CHANGING  file_table       = it_tab
                                                        rc               = gd_subrc ).

  READ TABLE it_tab INTO lw_file INDEX 1.

  iv_file_path = lw_file-filename.
ENDFORM.

FORM get_from_excel
  TABLES it_data  STRUCTURE alsmex_tabline
  USING  iv_fname TYPE rlgrap-filename
         iv_scol  TYPE i
         iv_srow  TYPE i
         iv_ecol  TYPE i
         iv_erow  TYPE i.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = iv_fname
      i_begin_col             = iv_scol
      i_begin_row             = iv_srow
      i_end_col               = iv_ecol
      i_end_row               = iv_erow
    TABLES
      intern                  = it_data
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM append_to_itab TABLES p_lt_data STRUCTURE alsmex_tabline
                    USING  lc_scol.

  DATA : lv_index LIKE sy-tabix.
  DATA : lv_col LIKE sy-tabix.
  DATA : lv_col_number TYPE i.
  DATA : lv_anln2 TYPE num4.

  SORT p_lt_data BY row
                    col.
* hangi aralıkla table'a append yapılacak( column sayısı)
  PERFORM get_col_number TABLES   p_lt_data
                         CHANGING lv_col_number.
* excelden alınan değerler internal tabloya yerleştirilir

  CLEAR: gt_item,
         gv_exc_first_line.

  DESCRIBE TABLE gt_item LINES gv_exc_first_line.
  gv_exc_first_line = gv_exc_first_line + 1.

  LOOP AT p_lt_data.

    lv_index = sy-tabix MOD lv_col_number.
    lv_col = p_lt_data-col + lc_scol - 1.

    CASE lv_col.

      WHEN 1. gt_item-anln1 = p_lt_data-value.
      WHEN 2.
        lv_anln2 = p_lt_data-value.
        WRITE lv_anln2 TO gt_item-anln2.

      WHEN 3. gt_item-menge = p_lt_data-value.
      WHEN 4. gt_item-gja_bchwrt = p_lt_data-value.
*      when 5.  move p_lt_data-value to gt_item-.
*      when 6.  move p_lt_data-value to gt_item-.
*      when 7.  move p_lt_data-value to gt_item-.
*      when 8 . move p_lt_data-value to gt_item-.

    ENDCASE.

    IF lv_index EQ 0.
* her kayıtta bulunacak alanlar buraya eklenebilir(excelden almadığı)
      PERFORM set_document_default_values USING 'I'.
      gt_item-mandt = sy-mandt.
      APPEND gt_item.
      gv_excel_data = 'X'.
      CLEAR: gt_item.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM get_col_number TABLES   it_data       STRUCTURE alsmex_tabline
                    CHANGING ev_col_number.

  CLEAR : ev_col_number.
* column number
  LOOP AT it_data WHERE row = 1.
    ev_col_number = ev_col_number + 1.
  ENDLOOP.
ENDFORM.

FORM set_excel_data.
  IF tc_item-current_line >= gv_exc_first_line.
*    PERFORM check_item_input .
    PERFORM check_data_changed.
    PERFORM get_isk_i_text.
  ENDIF.
ENDFORM.

*FORM check_kostl .
*  SELECT *
*    FROM csks
*    INTO TABLE @DATA(lt_data_gon)
*      UP TO 1 ROWS
*   WHERE kokrs = '1000'
*     AND kostl = @*zhvl_fi_dv_isk_h-kostl_gon
*     AND gsber = @*zhvl_fi_dv_isk_h-ggsbe
*   ORDER BY datbi DESCENDING, datab DESCENDING.
*  IF sy-subrc NE 0.
*    MESSAGE e040 .
*  ENDIF .
*
*  IF *zhvl_fi_dv_isk_h-kostl_alan IS NOT INITIAL.
*    SELECT *
*      FROM csks
*      INTO TABLE @DATA(lt_data_alan)
*        UP TO 1 ROWS
*     WHERE kokrs = '1000'
*       AND kostl = @*zhvl_fi_dv_isk_h-kostl_alan
*       AND gsber = @*zhvl_fi_dv_isk_h-agsbe
*     ORDER BY datbi DESCENDING, datab DESCENDING.
*    IF sy-subrc NE 0.
*      MESSAGE e041 .
*    ENDIF .
*    PERFORM set_item_kostl.
*  ENDIF.
*ENDFORM.

FORM anln1_help.
*  DATA: lv_kostl TYPE kostl,
*  DATA: lv_gsber TYPE gsber,
  DATA: lv_bukrs TYPE bukrs.

  lv_bukrs = *zhvl_fi_dv_isk_h-bukrs.
*  lv_kostl = *zhvl_fi_dv_isk_h-kostl_gon.
*  lv_gsber = *zhvl_fi_dv_isk_h-ggsbe.
  EXPORT lv_bukrs TO MEMORY ID 'ZHVL_SAPM_FI_DV_ISK_BUKRSTO_ZHVL_FI_SH_EXT_AANL'.
*  EXPORT lv_kostl TO MEMORY ID 'ZHVL_SAPM_FI_DV_ISK_KOSTL_GONTO_ZHVL_FI_SH_EXT_AANL'.
*  EXPORT lv_gsber TO MEMORY ID 'ZHVL_SAPM_FI_DV_ISK_GSBERTO_ZHVL_FI_SH_EXT_AANL'.
ENDFORM.

FORM check_item_kostl.
  SELECT SINGLE * FROM anlz
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    INTO @DATA(ls_anlz)
    WHERE anln1 EQ @*zhvl_fi_dv_isk_i_scr-anln1.
*  IF ls_anlz-kostl NE *zhvl_fi_dv_isk_h-kostl_gon.
*    MESSAGE e042 WITH *zhvl_fi_dv_isk_i_scr-anln1.
*  ENDIF.
ENDFORM.

FORM set_item_kostl.
*  IF sy-mandt EQ '200'.
*    LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
*      <fs_item>-kostl = *zhvl_fi_dv_isk_h-kostl_alan.
*    ENDLOOP.
*    REFRESH CONTROL 'TC_ITEM' FROM SCREEN 0210.
*  ENDIF.
ENDFORM.
