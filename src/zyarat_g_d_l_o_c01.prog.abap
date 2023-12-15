
CONSTANTS:
*----------------------------------------------------------------------
  BEGIN OF gc_iskat_durum,
    talep_olusturuldu TYPE zhvl_fi_dv_isk_h-dstat VALUE 'TO',
    onaya_gonderildi  TYPE zhvl_fi_dv_isk_h-dstat VALUE 'OG',
    onayandi          TYPE zhvl_fi_dv_isk_h-dstat VALUE 'ON',
    iskat_edildi      TYPE zhvl_fi_dv_isk_h-dstat VALUE 'IE',
    ters_kayit        TYPE zhvl_fi_dv_isk_h-dstat VALUE 'TK',
  END OF gc_iskat_durum,

  gc_icon_question    TYPE icon-name VALUE 'ICON_MESSAGE_QUESTION',
  gc_icon_information TYPE icon-name VALUE 'ICON_MESSAGE_INFORMATION',
  gc_icon_warning     TYPE icon-name VALUE 'ICON_MESSAGE_WARNING',
  gc_icon_error       TYPE icon-name VALUE 'ICON_MESSAGE_ERROR',
  gc_icon_critical    TYPE icon-name VALUE 'ICON_MESSAGE_CRITICAL'.
