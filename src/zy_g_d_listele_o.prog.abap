*&---------------------------------------------------------------------*
*& Report  ZHVL_FI_DV_ISK_LIST
*----------------------------------------------------------------------*
* Tanım       : Duran Varlık Iskat ve Onay Listesi                     *
* Geliştiren  : Tuna CENGİZKANER                                       *
* Tarih       : 12.12.2023                                             *
* Tasarlayan  : Ayşe Gül YÜZÜAK & Hepşen AKIN                          *
* İşlem       : 8000019382                                             *
*&---------------------------------------------------------------------*
REPORT zy_g_d_listele_o MESSAGE-ID zhvl_fi_dv .

INCLUDE ZY_G_D_LISTELE_O_TOP.
*INCLUDE: zhvl_fi_dv_i_isk_list_top. " Master program tanımlamaları
INCLUDE ZY_G_D_LISTELE_O_SEL.
*INCLUDE: zhvl_fi_dv_i_isk_list_sel. " Seçim ekranları
INCLUDE ZY_G_D_LISTELE_O_F01.
*INCLUDE: zhvl_fi_dv_i_isk_list_f01. " Alt-rutinler

*---Programın başlatılması---------------------------------------------*
INITIALIZATION.
  PERFORM init.

*---Seçimin başlangıcı-------------------------------------------------*
START-OF-SELECTION.
  PERFORM read_db.

*---Seçimin bitişi-----------------------------------------------------*
END-OF-SELECTION.
  IF gt_list[] IS INITIAL.
    MESSAGE s016.
  ELSE.
    PERFORM show_alv.
  ENDIF.
*----------------------------------------------------------------------*
