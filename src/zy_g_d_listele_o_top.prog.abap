*&---------------------------------------------------------------------*
*&  Include           ZHVL_FI_DV_I_ISK_LIST_TOP
*&---------------------------------------------------------------------*

*- Tip havuzları
TYPE-POOLS: slis, kkblo .

*- Sözlük yapıları
TABLES: zhvl_fi_dv_isk_h ,
        zhvl_fi_dv_isk_i .

*- İç tablolar
DATA: gt_list     TYPE zhvl_fi_dv_s_isk_list OCCURS 0 WITH HEADER LINE,
      gt_events   TYPE slis_t_event,
      gt_fieldcat TYPE slis_t_fieldcat_alv.

*- Değişkenler
DATA: gv_onay_flag.
