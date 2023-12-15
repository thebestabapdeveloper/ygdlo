*&---------------------------------------------------------------------*
*&  Include           ZHVL_FI_DV_I_ISK_LIST_SEL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK s100 WITH FRAME TITLE TEXT-100 .
PARAMETERS:
  p_bukrs TYPE zhvl_fi_dv_isk_h-bukrs OBLIGATORY MEMORY ID buk VALUE CHECK,
  p_gjahr TYPE zhvl_fi_dv_isk_h-gjahr OBLIGATORY MEMORY ID gjr.
SELECTION-SCREEN END OF BLOCK s100 .

SELECTION-SCREEN BEGIN OF BLOCK s200 WITH FRAME TITLE TEXT-100 .
SELECT-OPTIONS:
  s_belnr FOR zhvl_fi_dv_isk_h-belnr .
*  s_ggsbe FOR zhvl_fi_dv_isk_h-ggsbe ,
*  s_gkostl FOR zhvl_fi_dv_isk_h-kostl_gon MODIF ID kos,
*  s_agsbe FOR zhvl_fi_dv_isk_h-agsbe ,
*  s_akostl FOR zhvl_fi_dv_isk_h-kostl_alan MODIF ID kos.


SELECTION-SCREEN END OF BLOCK s200 .

SELECTION-SCREEN BEGIN OF BLOCK s300 WITH FRAME TITLE TEXT-100 .
SELECT-OPTIONS:
  s_anln1 FOR zhvl_fi_dv_isk_i-anln1 ,
  s_anln2 FOR zhvl_fi_dv_isk_i-anln2 .
SELECTION-SCREEN END OF BLOCK s300 .

SELECTION-SCREEN BEGIN OF BLOCK s500 WITH FRAME TITLE TEXT-100 .
SELECT-OPTIONS:
  s_ernam FOR zhvl_fi_dv_isk_h-ernam ,
  s_erdat FOR zhvl_fi_dv_isk_h-erdat ,
  s_aenam FOR zhvl_fi_dv_isk_h-aenam ,
  s_aedat FOR zhvl_fi_dv_isk_h-aedat .
SELECTION-SCREEN END OF BLOCK s500 .

SELECTION-SCREEN BEGIN OF BLOCK s400 WITH FRAME TITLE TEXT-100 .
PARAMETERS:
  p_rbasli RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND a,
  p_rkalem RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK s400 .

PARAMETERS:
  p_mode(2) NO-DISPLAY.
*----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.
  IF sy-mandt EQ '100'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'KOS'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN  .
*----------------------------------------------------------------------
  PERFORM authority_check.
  IF sy-mandt EQ '200'.
    PERFORM check_kostl.
  ENDIF.
