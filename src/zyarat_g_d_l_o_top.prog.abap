
PROGRAM zhvl_sapm_fi_dv_dvt MESSAGE-ID zhvl_fi_dv.

TABLES:
*----------------------------------------------------------------------
  t001,
  *zhvl_fi_dv_isk_h,
  *zhvl_fi_dv_isk_h_scr,
  *zhvl_fi_dv_isk_i_scr,

*-- Aşağıdaki alanlar kullanılmaz! Ekran tutarlılığı için kullanıldı!
  zhvl_fi_dv_isk_h ##NEEDED,
  zhvl_fi_dv_isk_i_scr ##NEEDED.

CONTROLS:
*----------------------------------------------------------------------
  ts_control TYPE TABSTRIP,
  tc_item    TYPE TABLEVIEW USING SCREEN 0210.

CONSTANTS:
*----------------------------------------------------------------------
  gc_create  TYPE char2     VALUE '01',
  gc_change  LIKE gc_create VALUE '02',
  gc_display LIKE gc_create VALUE '03',
  gc_delete  LIKE gc_create VALUE '05',

  gc_save    LIKE sy-ucomm  VALUE 'SAVE',
  gc_onayla  LIKE sy-ucomm  VALUE 'ONAYLA',
  gc_upload  LIKE sy-ucomm  VALUE 'UPLOAD',

  gc_crea    LIKE sy-ucomm  VALUE 'CREA',
  gc_dele    LIKE sy-ucomm  VALUE 'DELE',
  gc_sele    LIKE sy-ucomm  VALUE 'SELE',
  gc_dese    LIKE sy-ucomm  VALUE 'DESE',

  gc_list    LIKE sy-ucomm  VALUE 'LIST',
  gc_check   LIKE sy-ucomm  VALUE 'CHECK',
  gc_header  LIKE sy-ucomm  VALUE 'HEADER',
  gc_print   LIKE sy-ucomm  VALUE 'PRINT',

  gc_pick    LIKE sy-ucomm  VALUE 'PICK',

  gc_tab1    TYPE sy-ucomm  VALUE 'TAB1'.

DATA:
*----------------------------------------------------------------------
  gv_mode           LIKE gc_create,
  gv_title          TYPE sy-title,
  gv_datachanged    TYPE c LENGTH 1,
  gv_excel_data     TYPE c LENGTH 1,
  gv_listflag       TYPE c LENGTH 1,

  gv_onay_flag      TYPE c LENGTH 1,
  gv_error          TYPE c LENGTH 1,
  gv_batchmode      TYPE c LENGTH 1 VALUE 'N',
  gv_exc_first_line TYPE int4,

  gt_item           TYPE zhvl_fi_dv_isk_i_scr OCCURS 0 WITH HEADER LINE,

  BEGIN OF gs_tabstrip,
    subscreen  TYPE sy-dynnr VALUE '0210',
    active_tab TYPE sy-ucomm VALUE gc_tab1,
  END OF gs_tabstrip.
