
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
MODULE status OUTPUT.
  PERFORM set_status.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  user_command  INPUT
*&---------------------------------------------------------------------*
MODULE user_command INPUT.
  PERFORM user_command.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  PERFORM modify_screen.
ENDMODULE.                 " set_fields_on_off  OUTPUT
*---------------------------------------------------------------------*
*       MODULE initialize OUTPUT                                      *
*---------------------------------------------------------------------*
MODULE initialize OUTPUT.
  PERFORM initialize.
ENDMODULE.                 " set_initilize  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  check_belnr  OUTPUT
*&---------------------------------------------------------------------*
MODULE check_keys INPUT.
  PERFORM check_keys.
ENDMODULE.                 " check_belnr  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_exit  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  PERFORM user_command_exit.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  check_field_value  INPUT
*&---------------------------------------------------------------------*
MODULE check_field_value INPUT.
  PERFORM check_field_value.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  activate_service  OUTPUT
*&---------------------------------------------------------------------*
MODULE activate_service OUTPUT.
  PERFORM activate_service.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  set_item_line  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_item_line OUTPUT.
  PERFORM set_item_line.
ENDMODULE.
*----------------------------------------------------------------------*
*  MODULE get_isk_i_text INPUT
*----------------------------------------------------------------------*
MODULE get_isk_i_text INPUT.
  PERFORM get_isk_i_text.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  check_item_input  INPUT
*&---------------------------------------------------------------------*
MODULE check_item_input INPUT.
  PERFORM check_item_input.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  set_line_input  INPUT
*&---------------------------------------------------------------------*
MODULE set_line_input INPUT.
  PERFORM set_line_input.
  PERFORM calculate_screen_values.
ENDMODULE.

MODULE set_line_input OUTPUT.
  PERFORM set_line_input.
  IF gv_error NE 'X'.
    PERFORM calculate_screen_values.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  tabstrip_active_tab_set  OUTPUT
*&---------------------------------------------------------------------*
MODULE tabstrip_active_tab_set OUTPUT.
  ts_control-activetab = gs_tabstrip-active_tab.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  tabstrip_active_tab_get  INPUT
*&---------------------------------------------------------------------*
MODULE tabstrip_active_tab_get INPUT.
  PERFORM tabstrip_active_tab_get.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  hide_coloumns  OUTPUT
*&---------------------------------------------------------------------*
MODULE hide_coloumns OUTPUT.
  CASE sy-dynnr.
    WHEN '0210'.
      PERFORM hide_coloumns CHANGING tc_item.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_TABLE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_table_control OUTPUT.
  PERFORM set_table_control.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA_CHANGED  INPUT
*&---------------------------------------------------------------------*
MODULE check_data_changed INPUT.
  PERFORM check_data_changed.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_EXCEL_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_excel_data INPUT.

  CHECK gv_excel_data EQ 'X'.
  PERFORM set_excel_data.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_EXCEL_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_excel_data OUTPUT.

*  CHECK gv_excel_data EQ 'X'.
  PERFORM set_excel_data.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CLEAR_GV_EXCEL_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*module CLEAR_GV_EXCEL_DATA output.
*   CLEAR: gv_excel_data.
*endmodule.                 " CLEAR_GV_EXCEL_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_GV_EXCEL_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_gv_excel_data INPUT.
  CLEAR: gv_excel_data.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ANLN1_HELP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE anln1_help INPUT.
  PERFORM anln1_help.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_ITEM_KOSTL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_item_kostl INPUT.
  IF sy-mandt EQ '200'.
    PERFORM check_item_kostl.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_ITEM_KOSTL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_item_kostl INPUT.
  IF sy-mandt EQ '200'.
    PERFORM set_item_kostl.
  ENDIF.
ENDMODULE.
