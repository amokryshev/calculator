MODULE CALC_LOGIC_0001 INPUT.

  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
        c_controller->push_the_button( OK_CODE ).
  ENDCASE.

  clear OK_CODE.

 ENDMODULE.
