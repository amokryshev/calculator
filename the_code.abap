REPORT ZTEST_AMOKRYSHEV.
*INTERFACES AND ERRORS

CLASS cx_method_is_not_implemented DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS cx_state_not_found DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS cx_forbidden_symbol_name DEFINITION INHERITING FROM cx_static_check.
   PUBLIC SECTION.
    CONSTANTS: hold_the_state TYPE string VALUE 'hold_the_state'.
ENDCLASS.
CLASS cx_unknown_button_symbol DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

INTERFACE i_fsm_state.
  METHODS:
          mem_clear
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          mem_recover
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          mem_save
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          mem_plus
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          mem_minus
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          backspace
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          clear_last
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          clear_all
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          negative
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          sqrt
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_sy_arg_out_of_domain
                   cx_method_is_not_implemented
                   cx_state_not_found,
          divide
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          percent
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          multiply
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          one_to_ex
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          minus
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          plus
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          point
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          equals
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found,
          number
            IMPORTING inputed_number TYPE i
            RETURNING VALUE(fsm_symbol) TYPE string
            RAISING
                   cx_method_is_not_implemented
                   cx_state_not_found.
ENDINTERFACE.

*CALCULATION KERNEL, MODELS AND VIEW.

CLASS cl_sem_tree_node DEFINITION.
  PUBLIC SECTION.
    TYPES:
        BEGIN OF ts_tokens,
              line_id TYPE i,
              t_class TYPE string,
              token TYPE string,
              priority TYPE i,
              start_symbol TYPE i,
              end_symbol TYPE i,
        END OF ts_tokens.
    TYPES ts_tokens_tab TYPE SORTED TABLE OF ts_tokens WITH UNIQUE KEY line_id.
    METHODS:
        parse_from_str
          IMPORTING formula_string TYPE string,
        parse_from_arr
          IMPORTING tokens_tab TYPE ts_tokens_tab,
        calculate,
        get_string
          RETURNING VALUE(formula_string) TYPE string,
        get_tokens
          RETURNING VALUE(tokens_tab) TYPE ts_tokens_tab,
        get_result
          RETURNING VALUE(res) TYPE f,
        windows_calc_mode_on,
        excel_mode_on.

    PROTECTED SECTION.
      DATA:
          raw_string TYPE string,
          tokens TYPE SORTED TABLE OF ts_tokens WITH UNIQUE KEY line_id,
          action TYPE string,
          l_branch TYPE REF TO cl_sem_tree_node,
          r_branch TYPE REF TO cl_sem_tree_node,
          result TYPE f,
          calc_mode TYPE i VALUE 0.
      METHODS:
        append_token
          IMPORTING token_class TYPE string
                    token_symbol TYPE string,
        prioritize
          RETURNING VALUE(max_p_index) TYPE i.
ENDCLASS.
CLASS cl_sem_tree_node IMPLEMENTATION.
  METHOD append_token.
    DATA new_token TYPE ts_tokens.
    new_token = VALUE #( line_id = lines( me->tokens ) + 1 t_class = token_class token = token_symbol ).
    APPEND new_token TO me->tokens.
  ENDMETHOD.
  METHOD windows_calc_mode_on.
    me->calc_mode = 0.
  ENDMETHOD.
  METHOD excel_mode_on.
    me->calc_mode = 1.
  ENDMETHOD.
  METHOD prioritize.
    DATA:
          max_p TYPE i VALUE 0,
          token TYPE ts_tokens.
    max_p_index = 1.
    LOOP AT me->tokens INTO token.
        CASE token-t_class.
          WHEN '+' OR '-'.
            IF calc_mode = 1.
              token-priority = 3.
            ELSEIF calc_mode = 0.
              token-priority = 2.
            ENDIF.
            IF max_p <= token-priority.
              max_p = token-priority.
              max_p_index = token-line_id.
            ENDIF.
          WHEN '*' OR '/'.
            token-priority = 2.
            IF max_p <= token-priority.
              max_p = token-priority.
              max_p_index = token-line_id.
            ENDIF.
          WHEN 'b' OR 'r' OR 's'.
            token-priority = 1.
            IF max_p <= token-priority.
              max_p = token-priority.
              max_p_index = token-line_id.
            ENDIF.
          WHEN 'n'.
            token-priority = 0.
         ENDCASE.
       MODIFY TABLE me->tokens FROM token.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_from_str.

    me->raw_string = formula_string.

    DATA: counter TYPE i,
          curr_counter TYPE i,
          len TYPE i,
          init TYPE i VALUE 0,
          curr_str TYPE string,
          curr_token TYPE string,
          token_len TYPE i,
          token TYPE ts_tokens,
          tokens_temp TYPE ts_tokens_tab.

      curr_str = formula_string+1(1).
      len = strlen( formula_string ).
      counter = 0.
      curr_counter = 1.
      init = 0.
      WHILE counter < len.
        curr_str = formula_string+init(curr_counter).
        CASE curr_str.
          WHEN ' +'.
            me->append_token( token_class = '+' token_symbol = curr_str ).
            counter = counter + 1.
            init = counter.
            curr_counter = 1.
          WHEN ' -'.
            me->append_token( token_class = '-' token_symbol = curr_str ).
            counter = counter + 1.
            init = counter.
            curr_counter = 1.
          WHEN ' *'.
            me->append_token( token_class = '*' token_symbol = curr_str ).
            counter = counter + 1.
            init = counter.
            curr_counter = 1.
          WHEN ' /'.
            me->append_token( token_class = '/' token_symbol = curr_str ).
            counter = counter + 1.
            init = counter.
            curr_counter = 1.
          WHEN ' ('.
            curr_counter = len - init.
            curr_str = formula_string+init(curr_counter).
            curr_token = match( val = curr_str regex = '\s\([\w,\(,\),\+,\-,\*,\/,\s]+\)' occ = 1 ).
            token_len = strlen( curr_token ) - 3.
            curr_token = curr_token+2(token_len).
            me->append_token( token_class = 'b' token_symbol = curr_token ).
            counter = init + token_len + 3.
            init = counter.
            curr_counter = 1.
          WHEN ' reciproc('.
            curr_counter = len - init.
            curr_str = formula_string+init(curr_counter).
            curr_token = match( val = curr_str regex = '\sreciproc\([\w,\(,\)]+\)' occ = 1 ).
            token_len = strlen( curr_token ) - 11.
            curr_token = curr_token+10(token_len).
            CONCATENATE '1 /' curr_token INTO curr_token SEPARATED BY space.
            me->append_token( token_class = 'b' token_symbol = curr_token ).
            counter = init + token_len + 11.
            init = counter.
            curr_counter = 1.
          WHEN 'reciproc('.
            curr_counter = len - init.
            curr_str = formula_string+init(curr_counter).
            curr_token = match( val = curr_str regex = 'reciproc\([\w,\(,\)]+\)' occ = 1 ).
            token_len = strlen( curr_token ) - 10.
            curr_token = curr_token+9(token_len).
            CONCATENATE '1 /' curr_token INTO curr_token SEPARATED BY space.
            me->append_token( token_class = 'b' token_symbol = curr_token ).
            counter = init + token_len + 10.
            init = counter.
            curr_counter = 1.
          WHEN ' sqrt('.
            curr_counter = len - init.
            curr_str = formula_string+init(curr_counter).
*           Если бы ABAP позволял ленивую квантификацию, то этот скрипт парсил бы куда гибче: \ssqrt\([\w,\(,\),\+,\-,\*,\\,\s]+?\)(?=\s)|\ssqrt\([\w,\(,\),\+,\-,\*,\\,\s]+\)
            curr_token = match( val = curr_str regex = '\ssqrt\([\w,\(,\)]+\)' occ = 1 ).
            token_len = strlen( curr_token ) - 7.
            curr_token = curr_token+6(token_len).
            me->append_token( token_class = 's' token_symbol = curr_token ).
            counter = init + token_len + 7.
            init = counter.
            curr_counter = 1.
          WHEN 'sqrt('.
            curr_counter = len - init.
            curr_str = formula_string+init(curr_counter).
*           Если бы ABAP позволял ленивую квантификацию, то этот скрипт парсил бы куда гибче: sqrt\([\w,\(,\),\+,\-,\*,\\,\s]+?\)(?=\s)|\ssqrt\([\w,\(,\),\+,\-,\*,\\,\s]+\)
            curr_token = match( val = curr_str regex = 'sqrt\([\w,\(,\)]+\)' occ = 1 ).
            token_len = strlen( curr_token ) - 6.
            curr_token = curr_token+5(token_len).
            me->append_token( token_class = 's' token_symbol = curr_token ).
            counter = init + token_len + 6.
            init = counter.
            curr_counter = 1.
          WHEN OTHERS.
            curr_token = match( val = curr_str regex = '[-]?[0-9]+\,?[0-9]*\s' occ = 1 ).
            IF strlen( curr_token ) > 0.
              me->append_token( token_class = 'n' token_symbol = curr_token ).
              init = counter.
              curr_counter = 1.
            ELSE.
              curr_counter = curr_counter + 1.
              counter = counter + 1.
            ENDIF.
        ENDCASE.
      ENDWHILE.
      APPEND LINES OF me->tokens TO tokens_temp.
      me->parse_from_arr( tokens_tab = tokens_temp ).
  ENDMETHOD.

  METHOD parse_from_arr.

    CLEAR me->tokens.
    me->tokens = tokens_tab.

    DATA:
         tokens_divider TYPE i VALUE 1,
         left_branch_tokens TYPE ts_tokens_tab,
         right_branch_tokens TYPE ts_tokens_tab,
         curr_token TYPE ts_tokens,
         iter_token TYPE ts_tokens,
         new_token TYPE ts_tokens,
         len_of_token TYPE i,
         l_counter TYPE i VALUE 1,
         r_counter TYPE i VALUE 1.

    IF lines( me->tokens ) > 1.

      tokens_divider = me->prioritize( ).
      READ TABLE me->tokens WITH TABLE KEY line_id = tokens_divider INTO curr_token.
      me->action = curr_token-t_class.
      LOOP AT me->tokens INTO iter_token.
         IF iter_token-line_id < tokens_divider.
          new_token = VALUE #( line_id = l_counter t_class = iter_token-t_class token = iter_token-token ).
          APPEND new_token TO left_branch_tokens.
          l_counter = l_counter + 1.
         ELSEIF iter_token-line_id > tokens_divider.
          new_token = VALUE #( line_id = r_counter t_class = iter_token-t_class token = iter_token-token ).
          APPEND new_token TO right_branch_tokens.
          r_counter = r_counter + 1.
         ENDIF.
      ENDLOOP.
      CREATE OBJECT me->l_branch.
      CREATE OBJECT me->r_branch.
      me->l_branch->parse_from_arr( tokens_tab = left_branch_tokens ).
      me->r_branch->parse_from_arr( tokens_tab = right_branch_tokens ).
    ELSE.
      READ TABLE me->tokens WITH TABLE KEY line_id = tokens_divider INTO curr_token.
      me->action = curr_token-t_class.
      CASE curr_token-t_class.
        WHEN 'b' OR 's'.
          CREATE OBJECT me->l_branch.
          len_of_token = strlen( curr_token-token ) - 1.
          IF NOT curr_token-token+len_of_token(1) = ')'.
            CONCATENATE curr_token-token '' INTO curr_token-token SEPARATED BY space.
          ENDIF.
          me->l_branch->parse_from_str( curr_token-token ).
        WHEN 'n'.
          REPLACE ALL OCCURRENCES OF REGEX `\,` IN curr_token-token  WITH `.`.
          MOVE curr_token-token TO me->result.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
  METHOD calculate.

    IF NOT me->l_branch IS INITIAL.
      me->l_branch->calculate( ).
    ENDIF.
    IF NOT me->r_branch IS INITIAL.
      me->r_branch->calculate( ).
    ENDIF.
    CASE me->action.
      WHEN '+'.
        me->result = me->l_branch->result + me->r_branch->result.
      WHEN '-'.
        me->result = me->l_branch->result - me->r_branch->result.
      WHEN '*'.
        me->result = me->l_branch->result * me->r_branch->result.
      WHEN '/'.
        me->result = me->l_branch->result / me->r_branch->result.
      WHEN 's'.
        me->result = sqrt( me->l_branch->result ).
      WHEN 'r'.
        me->result = 1 / me->l_branch->result.
      WHEN 'b'.
        me->result = me->l_branch->result.
    ENDCASE.
  ENDMETHOD.
  METHOD get_string.
    formula_string = me->raw_string.
  ENDMETHOD.
  METHOD get_tokens.
    tokens_tab = me->tokens.
  ENDMETHOD.
  METHOD get_result.
    res = me->result.
  ENDMETHOD.
ENDCLASS.

CLASS cl_calc_memory DEFINITION.

  PUBLIC SECTION.
    METHODS:
        update_current
          IMPORTING val TYPE f,
        read_current
          RETURNING VALUE(res) TYPE f,
        update_memory
          IMPORTING val TYPE f,
        read_memory
          RETURNING VALUE(res) TYPE f,
        update_last
          IMPORTING val TYPE string,
        read_last
          RETURNING VALUE(res) TYPE string,
        add_to_formula
          IMPORTING val TYPE string,
        read_formula
          RETURNING VALUE(res) TYPE string,
        clear_formula,
        rise_after_point_factor,
        decrease_after_point_factor,
        reset_after_point_factor,
        read_after_point_factor
          RETURNING VALUE(res) TYPE f,
        update_minus_factor,
        read_minus_factor
          RETURNING VALUE(res) TYPE i,
        rise_is_droped,
        reset_is_droped,
        read_is_droped
          RETURNING VALUE(res) TYPE i.


    PRIVATE SECTION.

      DATA:
        current TYPE f,
        last TYPE string,
        memory TYPE f,
        formula TYPE string,
        after_point_factor TYPE f VALUE 1,
        minus_factor TYPE i VALUE 1,
        is_droped TYPE i VALUE 0.

ENDCLASS.
CLASS cl_calc_memory IMPLEMENTATION.

  METHOD update_current.
    current = val.
  ENDMETHOD.
  METHOD read_current.
    res = current.
  ENDMETHOD.
  METHOD update_last.
    last = val.
  ENDMETHOD.
  METHOD read_last.
    res = last.
  ENDMETHOD.
  METHOD update_memory.
    memory = val.
  ENDMETHOD.
  METHOD read_memory.
    res = memory.
  ENDMETHOD.
  METHOD add_to_formula.
    IF strlen( me->formula ) > 0.
      CONCATENATE me->formula val INTO me->formula SEPARATED BY space.
    ELSE.
      me->formula = val.
    ENDIF.
  ENDMETHOD.
  METHOD read_formula.
    res = formula.
  ENDMETHOD.
  METHOD clear_formula.
    formula = ''.
  ENDMETHOD.
  METHOD rise_after_point_factor.
      me->after_point_factor = me->after_point_factor / 10.
  ENDMETHOD.
  METHOD decrease_after_point_factor.
    me->after_point_factor = me->after_point_factor * 10.
  ENDMETHOD.
  METHOD reset_after_point_factor.
      me->after_point_factor = 1.
  ENDMETHOD.
  METHOD read_after_point_factor.
   res = me->after_point_factor.
  ENDMETHOD.
  METHOD update_minus_factor.
    IF me->minus_factor = 1.
      me->minus_factor = -1.
    ELSE.
      me->minus_factor = 1.
    ENDIF.
  ENDMETHOD.
  METHOD read_minus_factor.
    res = me->minus_factor.
  ENDMETHOD.
  METHOD rise_is_droped.
    me->is_droped = 1.
  ENDMETHOD.
  METHOD reset_is_droped.
    me->is_droped = 0.
  ENDMETHOD.
  METHOD read_is_droped.
    res = me->is_droped.
  ENDMETHOD.

ENDCLASS.

CLASS cl_calc_screen DEFINITION.
  PUBLIC SECTION.
    METHODS:
      CONSTRUCTOR
        IMPORTING
          value(curr_textbox) TYPE REF TO string
          value(formula_textbox) TYPE REF TO string,
      float_to_char
        IMPORTING val TYPE f
        RETURNING VALUE(res) TYPE string,
      char_to_float
        IMPORTING val TYPE string
        RETURNING VALUE(res) TYPE f,
      update_screen
        IMPORTING
          current TYPE f
          last TYPE string
          formula TYPE string,
      turn_mem_on,
      turn_mem_off,
      rise_sqrt_on_negative,
      reset_sqrt_on_negative,
      rise_divide_on_zero,
      reset_divide_on_zero.
  PRIVATE SECTION.
    DATA: mem_flag TYPE c VALUE '',
          sqrt_on_negative_flag TYPE i VALUE 0,
          divide_on_zero_flag TYPE i VALUE 0,
          screen_tb TYPE REF TO string,
          formula_tb TYPE REF TO string.
ENDCLASS.
CLASS cl_calc_screen IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    CALL METHOD super->constructor.
    me->screen_tb = curr_textbox.
    me->formula_tb = formula_textbox.
  ENDMETHOD.

  METHOD float_to_char.
    DATA:
          temp_str TYPE c LENGTH 27.

    CALL FUNCTION 'FLTP_CHAR_CONVERSION'
      EXPORTING
        decim = 14
        expon = 0
        input = val
        ivalu = ' '
        maskn = ' '
      IMPORTING
        flstr = temp_str.
    res = temp_str.


    SHIFT res RIGHT DELETING TRAILING ' '.
    SHIFT res RIGHT DELETING TRAILING '0'.
    SHIFT res RIGHT DELETING TRAILING ','.
    CONDENSE res.
  ENDMETHOD.
  METHOD char_to_float.

    DATA:
          temp_float TYPE f,
          temp_str TYPE c LENGTH 27.

    MOVE val to temp_str.

    CALL FUNCTION 'CHAR_FLTP_CONVERSION'
      EXPORTING
        string =  temp_str
      IMPORTING
        flstr = temp_float.

    res = temp_float.

  ENDMETHOD.


  METHOD update_screen.
    DATA:
          current_str TYPE string,
          last_str TYPE string,
          formula_str TYPE string,
          temp_argument_v_str TYPE string,
          temp_str TYPE string,
          str_len TYPE i,
          string_of_spaces(29) TYPE c,
          count_of_spaces TYPE i.

    IF me->divide_on_zero_flag = 0 AND me->sqrt_on_negative_flag = 0.
      current_str = me->float_to_char( current ).
    ELSEIF me->divide_on_zero_flag = 1.
      current_str = 'Делить на ноль нельзя!'.
      me->reset_divide_on_zero( ).
    ELSEIF me->sqrt_on_negative_flag = 1.
      current_str = 'Отр. числа не имеют корня!'.
      me->reset_sqrt_on_negative( ).
    ENDIF.
    count_of_spaces = 29 - strlen( mem_flag ) - strlen( current_str ).

    CONCATENATE  mem_flag current_str INTO screen_tb->* SEPARATED BY string_of_spaces(count_of_spaces).
    str_len = strlen( formula ) + strlen( last ).
    IF strlen( formula ) > 0.
      CONCATENATE formula last INTO formula_str SEPARATED BY space.
    ELSE.
      formula_str = last.
    ENDIF.
    IF str_len > 29.
      str_len = str_len - 26.
      formula_str = formula_str+str_len.
      CONCATENATE '<<' formula_str INTO formula_str.
      formula_tb->* = formula_str.
    ELSEIF str_len = 29.
      formula_tb->* = formula_str.
    ELSEIF str_len < 29.
      count_of_spaces = 28 - str_len.
      CONCATENATE '' formula_str INTO formula_str SEPARATED BY string_of_spaces(count_of_spaces).
      formula_tb->* = formula_str.

    ENDIF.
  ENDMETHOD.
  METHOD turn_mem_on.
    mem_flag = 'M'.
  ENDMETHOD.
  METHOD turn_mem_off.
    mem_flag = ''.
  ENDMETHOD.
  METHOD rise_divide_on_zero.
    divide_on_zero_flag = 1.
  ENDMETHOD.
  METHOD reset_divide_on_zero.
    divide_on_zero_flag = 0.
  ENDMETHOD.
  METHOD rise_sqrt_on_negative.
    sqrt_on_negative_flag = 1.
  ENDMETHOD.
  METHOD reset_sqrt_on_negative.
    sqrt_on_negative_flag = 0.
  ENDMETHOD.
ENDCLASS.

*FINITE STATE MACHINE

CLASS cl_fsm_mem_initial DEFINITION.
  PUBLIC SECTION.
    INTERFACES i_fsm_state.
    METHODS:
            CONSTRUCTOR
              IMPORTING
                screen TYPE REF TO cl_calc_screen
                memory TYPE REF TO cl_calc_memory.
   PROTECTED SECTION.
      DATA:
           screen TYPE REF TO cl_calc_screen,
           memory TYPE REF TO cl_calc_memory.
ENDCLASS.
CLASS cl_fsm_mem_initial IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    me->memory = memory.
    me->screen = screen.
  ENDMETHOD.
  METHOD i_fsm_state~mem_clear.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~mem_recover.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~mem_save.
    me->memory->update_memory( me->memory->read_current( ) ).
    me->screen->turn_mem_on( ).
    me->memory->rise_is_droped( ).
    fsm_symbol = 'saved'.
  ENDMETHOD.
  METHOD i_fsm_state~mem_plus.
    me->memory->update_memory( me->memory->read_current( ) ).
    me->screen->turn_mem_on( ).
    me->memory->rise_is_droped( ).
    fsm_symbol = 'saved'.
  ENDMETHOD.
  METHOD i_fsm_state~mem_minus.
    me->memory->update_memory( me->memory->read_current( ) * -1 ).
    me->screen->turn_mem_on( ).
    me->memory->rise_is_droped( ).
    fsm_symbol = 'saved'.
  ENDMETHOD.
  METHOD i_fsm_state~backspace.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~clear_last.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~clear_all.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~negative.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~sqrt.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~divide.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~percent.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~multiply.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~one_to_ex.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~minus.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~plus.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~point.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~equals.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.

  METHOD i_fsm_state~number.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.
ENDCLASS.

CLASS cl_fsm_mem_saved DEFINITION INHERITING FROM cl_fsm_mem_initial.
  PUBLIC SECTION.
    METHODS:
            i_fsm_state~mem_clear REDEFINITION,
            i_fsm_state~mem_recover REDEFINITION,
            i_fsm_state~mem_save REDEFINITION,
            i_fsm_state~mem_plus REDEFINITION,
            i_fsm_state~mem_minus REDEFINITION.

ENDCLASS.
CLASS cl_fsm_mem_saved IMPLEMENTATION.
  METHOD i_fsm_state~mem_clear.
    me->memory->update_memory( 0 ).
    me->screen->turn_mem_off( ).
    fsm_symbol = 'initial'.
  ENDMETHOD.
  METHOD i_fsm_state~mem_recover.
    me->memory->update_current( me->memory->read_memory( ) ).
    me->memory->rise_is_droped( ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~mem_save.
    me->memory->update_memory( me->memory->read_current( ) ).
    me->memory->rise_is_droped( ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~mem_plus.
    me->memory->update_memory( me->memory->read_memory( ) + me->memory->read_current( ) ).
    me->memory->rise_is_droped( ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~mem_minus.
    me->memory->update_memory( me->memory->read_memory( ) - me->memory->read_current( ) ).
    me->memory->rise_is_droped( ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
ENDCLASS.

CLASS cl_fsm_calc_state DEFINITION.
  PUBLIC SECTION.
    INTERFACES i_fsm_state.
    METHODS:
        CONSTRUCTOR
          IMPORTING
            screen TYPE REF TO cl_calc_screen
            memory TYPE REF TO cl_calc_memory.
  PROTECTED SECTION.
    DATA:
         screen TYPE REF TO cl_calc_screen,
         memory TYPE REF TO cl_calc_memory.
    METHODS:
        working_equals
          RETURNING VALUE(fsm_symbol) TYPE string
          RAISING
                 cx_method_is_not_implemented
                 cx_state_not_found,
        working_percent
          IMPORTING
            mode TYPE i
          RETURNING VALUE(fsm_symbol) TYPE string
          RAISING
                 cx_method_is_not_implemented
                 cx_state_not_found,
        calculate
          IMPORTING
            mode TYPE i
          RETURNING VALUE(res) TYPE REF TO cl_sem_tree_node,
        read_suffix
          RETURNING VALUE(res) TYPE string.
ENDCLASS.
CLASS cl_fsm_calc_state IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    me->memory = memory.
    me->screen = screen.
  ENDMETHOD.
  METHOD i_fsm_state~mem_clear.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.
  METHOD i_fsm_state~mem_recover.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.
  METHOD i_fsm_state~mem_save.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.
  METHOD i_fsm_state~mem_plus.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.
  METHOD i_fsm_state~mem_minus.
    RAISE EXCEPTION TYPE cx_method_is_not_implemented.
  ENDMETHOD.
  METHOD i_fsm_state~backspace.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~clear_last.
    me->memory->update_current( 0 ).
    me->memory->reset_after_point_factor( ).
    IF me->memory->read_minus_factor( ) = -1.
      me->memory->update_minus_factor( ).
    ENDIF.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~clear_all.
    me->i_fsm_state~clear_last( ).
    me->memory->update_last( '' ).
    me->memory->clear_formula( ).
    fsm_symbol = 'clear_all'.
  ENDMETHOD.
  METHOD i_fsm_state~negative.
    me->memory->update_minus_factor( ).
    me->memory->update_current( me->memory->read_current( ) * -1 ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~sqrt.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~divide.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~percent.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~multiply.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~one_to_ex.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~minus.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~plus.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~point.
    IF me->memory->read_after_point_factor( ) = 1.
      me->memory->rise_after_point_factor( ).
    ENDIF.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~equals.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~number.
    IF me->memory->read_is_droped( ) = 0.
      IF me->memory->read_after_point_factor( ) = 1.
        me->memory->update_current(  me->memory->read_current( ) * 10 + inputed_number * me->memory->read_minus_factor( ) ).
      ELSE.
        me->memory->update_current(  me->memory->read_current( ) + inputed_number * me->memory->read_minus_factor( ) * me->memory->read_after_point_factor( ) ).
        me->memory->rise_after_point_factor( ).
      ENDIF.
    ELSE.
      me->memory->update_current( inputed_number + 1 / 10 - 1 / 10 ).
      me->memory->reset_is_droped( ).
      me->memory->reset_after_point_factor( ).
    ENDIF.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD working_equals.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 0 ).
    me->memory->update_current( calc_tree->get_result( ) ).
    me->memory->clear_formula( ).
    me->memory->update_last('').
    me->memory->rise_is_droped( ).
    fsm_symbol = 'clear_all'.
  ENDMETHOD.
  METHOD working_percent.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node,
         result TYPE f,
         check_last_str TYPE string,
         length_of_last_str TYPE i,
         last_str TYPE string.

    IF mode = 0.
      last_str = me->memory->read_last( ).
    ELSEIF mode = 1.
      last_str = me->memory->read_formula( ).
    ENDIF.
    length_of_last_str = strlen( last_str ) - 1.
    check_last_str = last_str+length_of_last_str.
    IF check_last_str = '/'.
      CONCATENATE last_str '1' '' INTO last_str SEPARATED BY space.
    ELSEIF length_of_last_str = 0.
      me->memory->update_last('').
      check_last_str = me->read_suffix( ).
      me->memory->update_last( last_str ).
      last_str = me->memory->read_formula( ).
    ELSE.
      CONCATENATE last_str '0' '' INTO last_str SEPARATED BY space.
    ENDIF.
    CREATE OBJECT calc_tree.
    calc_tree->parse_from_str( last_str ).
    calc_tree->calculate( ).
    result = calc_tree->get_result( ) * ( me->memory->read_current( ) * ( 1 / 100 ) ).
    IF mode = 0.
      me->memory->add_to_formula( me->memory->read_last( ) ).
    ENDIF.
    me->memory->update_last('').
    me->memory->update_current( result ).
    fsm_symbol = 'enter_formula'.
  ENDMETHOD.
  METHOD calculate.
    DATA:
         curr_str TYPE string,
         last_str TYPE string,
         formula_str TYPE string,
         calc_tree TYPE REF TO cl_sem_tree_node.
    formula_str = me->memory->read_formula( ).
    last_str = me->memory->read_last( ).
    curr_str = me->screen->float_to_char( me->memory->read_current( ) ).
    IF mode = 0.
      CONCATENATE formula_str last_str curr_str '' INTO formula_str SEPARATED BY space.
    ELSEIF mode = 1.
      CONCATENATE formula_str last_str '' INTO formula_str SEPARATED BY space.
    ENDIF.
    CREATE OBJECT calc_tree.
    calc_tree->parse_from_str( formula_str ).
    calc_tree->calculate( ).
    me->memory->add_to_formula( me->memory->read_last( ) ).
    me->memory->update_last('').
    res = calc_tree.
  ENDMETHOD.
  METHOD read_suffix.
    IF strlen( me->memory->read_last( ) ) > 0.
      res = me->memory->read_last( ).
    ELSE.
      res = me->screen->float_to_char( me->memory->read_current( ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS cl_fsm_calc_error DEFINITION INHERITING FROM cl_fsm_calc_state.
  PUBLIC SECTION.
    METHODS:
            i_fsm_state~clear_last REDEFINITION,
            i_fsm_state~negative REDEFINITION,
            i_fsm_state~point REDEFINITION,
            i_fsm_state~number REDEFINITION.
ENDCLASS.
CLASS cl_fsm_calc_error IMPLEMENTATION.
  METHOD i_fsm_state~clear_last.
    fsm_symbol = super->i_fsm_state~clear_last( ).
    me->memory->update_last( '' ).
    me->memory->clear_formula( ).
    fsm_symbol = 'clear_all'.
  ENDMETHOD.
  METHOD i_fsm_state~negative.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~point.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~number.
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
ENDCLASS.

CLASS cl_fsm_calc_initial DEFINITION INHERITING FROM cl_fsm_calc_state.
  PUBLIC SECTION.
    METHODS:
            i_fsm_state~sqrt REDEFINITION,
            i_fsm_state~divide REDEFINITION,
            i_fsm_state~percent REDEFINITION,
            i_fsm_state~multiply REDEFINITION,
            i_fsm_state~one_to_ex REDEFINITION,
            i_fsm_state~minus REDEFINITION,
            i_fsm_state~plus REDEFINITION,
            i_fsm_state~number REDEFINITION.
ENDCLASS.
CLASS cl_fsm_calc_initial IMPLEMENTATION.

  METHOD i_fsm_state~sqrt.
    me->memory->update_current( 0 ).
    me->memory->update_last('sqrt(0)').
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_formula'.
  ENDMETHOD.

  METHOD i_fsm_state~divide.
    me->memory->update_current( 0 ).
    me->memory->update_last('0 /').
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_symbol'.
  ENDMETHOD.

  METHOD i_fsm_state~percent.
    me->memory->update_current( 0 ).
    me->memory->update_last('0').
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_formula'.
  ENDMETHOD.

  METHOD i_fsm_state~multiply.
    me->memory->update_current( 0 ).
    me->memory->update_last('0 *').
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_symbol'.
  ENDMETHOD.

  METHOD i_fsm_state~one_to_ex.
    me->memory->update_current( 0 ).
    me->memory->update_last('reciproc(0)').
    me->memory->rise_is_droped( ).
    me->screen->rise_divide_on_zero( ).
    fsm_symbol = 'enter_formula'.
  ENDMETHOD.

  METHOD i_fsm_state~minus.
    me->memory->update_current( 0 ).
    me->memory->update_last('0 -').
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_symbol'.
  ENDMETHOD.

  METHOD i_fsm_state~plus.
    me->memory->update_current( 0 ).
    me->memory->update_last('0 +').
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_symbol'.
  ENDMETHOD.

  METHOD i_fsm_state~number.
    super->i_fsm_state~number( inputed_number ).
    fsm_symbol = 'enter_first'.
  ENDMETHOD.

ENDCLASS.

CLASS cl_fsm_calc_first DEFINITION INHERITING FROM cl_fsm_calc_initial.
  PUBLIC SECTION.
    METHODS:
            i_fsm_state~backspace REDEFINITION,
            i_fsm_state~sqrt REDEFINITION,
            i_fsm_state~one_to_ex REDEFINITION,
            i_fsm_state~divide REDEFINITION,
            i_fsm_state~multiply REDEFINITION,
            i_fsm_state~minus REDEFINITION,
            i_fsm_state~plus REDEFINITION,
            i_fsm_state~number REDEFINITION.

ENDCLASS.
CLASS cl_fsm_calc_first IMPLEMENTATION.
  METHOD i_fsm_state~backspace.
   DATA:
        str_len TYPE i,
        curr_str TYPE string.
   curr_str = me->screen->float_to_char( me->memory->read_current( ) ).
   str_len = strlen( curr_str ).
   IF str_len > 0.
    str_len = str_len - 1.
    curr_str = curr_str(str_len).
    IF NOT curr_str = '-'.
      me->memory->update_current( me->screen->char_to_float( curr_str ) ).
    ELSE.
      me->memory->update_current( 0 ).
      me->memory->update_minus_factor( ).
    ENDIF.
   ENDIF.
   IF me->memory->read_after_point_factor( ) < 1.
     me->memory->decrease_after_point_factor( ).
   ENDIF.
   IF me->memory->read_after_point_factor( ) = 1 / 10.
     me->memory->reset_after_point_factor( ).
   ENDIF.
   fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~sqrt.
    DATA suffix TYPE string.
    suffix = me->read_suffix( ).
    CONCATENATE 'sqrt(' suffix ')' INTO suffix.
    me->memory->update_last( suffix ).
    TRY.
      me->memory->update_current( sqrt( me->memory->read_current( ) ) ).
      me->memory->rise_is_droped( ).
      fsm_symbol = cx_forbidden_symbol_name=>hold_the_state..
    CATCH cx_sy_arg_out_of_domain.
      me->screen->rise_sqrt_on_negative( ).
      fsm_symbol = 'fall_to_error'.
    ENDTRY.
  ENDMETHOD.

  METHOD i_fsm_state~one_to_ex.
    DATA suffix TYPE string.
    suffix = me->read_suffix( ).
    CONCATENATE 'reciproc(' suffix ')' INTO suffix.
    me->memory->update_last( suffix ).
    TRY.
      me->memory->update_current( 1 / me->memory->read_current( ) ).
      me->memory->rise_is_droped( ).
      fsm_symbol = cx_forbidden_symbol_name=>hold_the_state..
    CATCH cx_sy_zerodivide.
      me->memory->update_last('reciproc(0)').
      me->screen->rise_divide_on_zero( ).
      fsm_symbol = 'fall_to_error'.
    ENDTRY.
  ENDMETHOD.

  METHOD i_fsm_state~divide.
    DATA suffix TYPE string.
    suffix = me->read_suffix( ).
    CONCATENATE suffix ' /' INTO suffix.
    me->memory->update_last( suffix ).
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_symbol'.
  ENDMETHOD.


  METHOD i_fsm_state~multiply.
    DATA suffix TYPE string.
    suffix = me->read_suffix( ).
    CONCATENATE suffix ' *' INTO suffix.
    me->memory->update_last( suffix ).
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_symbol'.
  ENDMETHOD.


  METHOD i_fsm_state~minus.
    DATA suffix TYPE string.
    suffix = me->read_suffix( ).
    CONCATENATE suffix ' -' INTO suffix.
    me->memory->update_last( suffix ).
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_symbol'.
  ENDMETHOD.

  METHOD i_fsm_state~plus.
    DATA suffix TYPE string.
    suffix = me->read_suffix( ).
    CONCATENATE suffix ' +' INTO suffix.
    me->memory->update_last( suffix ).
    me->memory->rise_is_droped( ).
    fsm_symbol = 'enter_symbol'.
  ENDMETHOD.

  METHOD i_fsm_state~number.
    super->i_fsm_state~number( inputed_number ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
ENDCLASS.

CLASS cl_fsm_calc_symbol DEFINITION INHERITING FROM cl_fsm_calc_first.
  PUBLIC SECTION.
    METHODS:
            i_fsm_state~sqrt REDEFINITION,
            i_fsm_state~one_to_ex REDEFINITION,
            i_fsm_state~percent REDEFINITION,
            i_fsm_state~divide REDEFINITION,
            i_fsm_state~multiply REDEFINITION,
            i_fsm_state~minus REDEFINITION,
            i_fsm_state~plus REDEFINITION,
            i_fsm_state~equals REDEFINITION,
            i_fsm_state~number REDEFINITION.

ENDCLASS.
CLASS cl_fsm_calc_symbol IMPLEMENTATION.
  METHOD i_fsm_state~sqrt.
    me->memory->add_to_formula( me->memory->read_last( ) ).
    me->memory->update_last('').
    fsm_symbol = super->i_fsm_state~sqrt( ).
  ENDMETHOD.
  METHOD i_fsm_state~one_to_ex.
    me->memory->add_to_formula( me->memory->read_last( ) ).
    me->memory->update_last('').
    fsm_symbol = super->i_fsm_state~one_to_ex( ).
  ENDMETHOD.
  METHOD i_fsm_state~percent.
    fsm_symbol = me->working_percent( 0 ).
    me->memory->update_last( me->read_suffix( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~divide.
    me->memory->update_last('').
    super->i_fsm_state~divide( ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~multiply.
    me->memory->update_last('').
    super->i_fsm_state~multiply( ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~minus.
    me->memory->update_last('').
    super->i_fsm_state~minus( ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~plus.
    me->memory->update_last('').
    super->i_fsm_state~plus( ).
    fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
  ENDMETHOD.
  METHOD i_fsm_state~equals.
    fsm_symbol = me->working_equals( ).
  ENDMETHOD.
  METHOD i_fsm_state~number.
    super->i_fsm_state~number( inputed_number ).
    fsm_symbol = 'enter_next'.
  ENDMETHOD.

ENDCLASS.

CLASS cl_fsm_calc_next DEFINITION INHERITING FROM cl_fsm_calc_first.
  PUBLIC SECTION.
    METHODS:
            i_fsm_state~sqrt REDEFINITION,
            i_fsm_state~one_to_ex REDEFINITION,
            i_fsm_state~percent REDEFINITION,
            i_fsm_state~divide REDEFINITION,
            i_fsm_state~multiply REDEFINITION,
            i_fsm_state~minus REDEFINITION,
            i_fsm_state~plus REDEFINITION,
            i_fsm_state~equals REDEFINITION.
ENDCLASS.
CLASS cl_fsm_calc_next IMPLEMENTATION.
  METHOD i_fsm_state~sqrt.
    me->memory->add_to_formula( me->memory->read_last( ) ).
    me->memory->update_last('').
    super->i_fsm_state~sqrt( ).
    fsm_symbol = 'enter_formula'.
  ENDMETHOD.
  METHOD i_fsm_state~one_to_ex.
    me->memory->add_to_formula( me->memory->read_last( ) ).
    me->memory->update_last('').
    super->i_fsm_state~one_to_ex( ).
    fsm_symbol = 'enter_formula'.
  ENDMETHOD.
  METHOD i_fsm_state~percent.
    fsm_symbol = me->working_percent( 0 ).
    me->memory->update_last( me->read_suffix( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~divide.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 0 ).
    fsm_symbol = super->i_fsm_state~divide( ).
    me->memory->update_current( calc_tree->get_result( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~multiply.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 0 ).

    fsm_symbol = super->i_fsm_state~multiply( ).
    me->memory->update_current( calc_tree->get_result( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~minus.
   DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 0 ).
    fsm_symbol = super->i_fsm_state~minus( ).
    me->memory->update_current( calc_tree->get_result( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~plus.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 0 ).
    fsm_symbol = super->i_fsm_state~plus( ).
    me->memory->update_current( calc_tree->get_result( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~equals.
    fsm_symbol = me->working_equals( ).
  ENDMETHOD.
ENDCLASS.

CLASS cl_fsm_calc_after_formula DEFINITION INHERITING FROM cl_fsm_calc_first.
  PUBLIC SECTION.
    METHODS:
            i_fsm_state~percent REDEFINITION,
            i_fsm_state~divide REDEFINITION,
            i_fsm_state~multiply REDEFINITION,
            i_fsm_state~minus REDEFINITION,
            i_fsm_state~plus REDEFINITION,
            i_fsm_state~equals REDEFINITION,
            i_fsm_state~number REDEFINITION.
ENDCLASS.
CLASS cl_fsm_calc_after_formula IMPLEMENTATION.
  METHOD i_fsm_state~percent.
    me->memory->update_last('').
    fsm_symbol = me->working_percent( 1 ).
    me->memory->update_last( me->read_suffix( ) ).
  ENDMETHOD.
   METHOD i_fsm_state~divide.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 1 ).
    me->memory->update_last( '/' ).
    fsm_symbol = 'enter_symbol'.
    me->memory->update_current( calc_tree->get_result( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~multiply.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 1 ).
    me->memory->update_last( '*' ).
    fsm_symbol = 'enter_symbol'.
    me->memory->update_current( calc_tree->get_result( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~minus.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 1 ).
    me->memory->update_last( '-' ).
    fsm_symbol = 'enter_symbol'.
    me->memory->update_current( calc_tree->get_result( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~plus.
    DATA:
         calc_tree TYPE REF TO cl_sem_tree_node.
    calc_tree = me->calculate( 1 ).
    me->memory->update_last( '+' ).
    fsm_symbol = 'enter_symbol'.
    me->memory->update_current( calc_tree->get_result( ) ).
  ENDMETHOD.
  METHOD i_fsm_state~equals.
    me->memory->update_last('').
    fsm_symbol = me->working_equals( ).
  ENDMETHOD.
  METHOD i_fsm_state~number.
    me->memory->update_last('').
    super->i_fsm_state~number( inputed_number ).
    fsm_symbol = 'enter_next'.
  ENDMETHOD.
ENDCLASS.

CLASS cl_calc_fsm DEFINITION ABSTRACT.
  PUBLIC SECTION.
        METHODS:
          CONSTRUCTOR,
          add_state
            IMPORTING
                  fsm_symbol TYPE string
                  fsm_transition TYPE REF TO i_fsm_state
            RAISING cx_forbidden_symbol_name,
          remove_state
            IMPORTING
                  fsm_symbol TYPE string,
          clear_fsm.

  PROTECTED SECTION.
    TYPES: BEGIN OF fsm_table,
             symbol TYPE string,
             transition TYPE REF TO i_fsm_state,
           END OF fsm_table.
    DATA fsm TYPE HASHED TABLE OF fsm_table WITH UNIQUE KEY symbol.
    METHODS:
            get_state
              IMPORTING req_fsm_symbol TYPE string
              RETURNING VALUE(fsm_transition) TYPE REF TO i_fsm_state
              RAISING cx_state_not_found.
ENDCLASS.
CLASS cl_calc_fsm IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD add_state.

    IF fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
      RAISE EXCEPTION TYPE cx_forbidden_symbol_name.
    ENDIF.

    DATA:
         fsm_str TYPE fsm_table,
         new_str TYPE fsm_table,
         is_found TYPE i VALUE 0.

    LOOP AT me->fsm INTO fsm_str.
      IF fsm_str-symbol = fsm_symbol.
        fsm_str-transition = fsm_transition.
        MODIFY TABLE me->fsm FROM fsm_str.
        is_found = 1.
      ENDIF.
    ENDLOOP.
    IF is_found = 0.
      new_str = VALUE #( symbol = fsm_symbol transition = fsm_transition ).
      INSERT new_str INTO TABLE fsm.
    ENDIF.
  ENDMETHOD.
  METHOD remove_state.
    DELETE me->fsm WHERE symbol = fsm_symbol.
  ENDMETHOD.
  METHOD get_state.
    DATA:
          fsm_str TYPE fsm_table,
          is_found TYPE i VALUE 0.
    LOOP AT me->fsm INTO fsm_str.
      IF fsm_str-symbol = req_fsm_symbol.
        fsm_transition = fsm_str-transition.
        is_found = 1.
      ENDIF.
    ENDLOOP.
    IF is_found = 0.
      RAISE EXCEPTION TYPE cx_state_not_found.
    ENDIF.
  ENDMETHOD.
  METHOD clear_fsm.
    CLEAR fsm.
  ENDMETHOD.
ENDCLASS.

*Calculation controller based on FSM
CLASS cl_calc_controller DEFINITION INHERITING FROM cl_calc_fsm.

    PUBLIC SECTION.
      METHODS:
              CONSTRUCTOR
                IMPORTING
                  calc_screen TYPE REF TO cl_calc_screen
                  calc_memory TYPE REF TO cl_calc_memory
                RAISING
                       cx_method_is_not_implemented,
                       set_curr_mem_state
                IMPORTING
                      fsm_symbol TYPE string
                RAISING
                       cx_state_not_found,
                       set_curr_calc_state
                IMPORTING
                         fsm_symbol TYPE string
                RAISING
                       cx_state_not_found,
                       push_the_button
                IMPORTING
                         button_symbol TYPE sy-ucomm
                RAISING
                       cx_state_not_found
                       cx_unknown_button_symbol
                       cx_method_is_not_implemented.

    PRIVATE SECTION.
      DATA:
           curr_mem_state TYPE REF TO i_fsm_state,
           curr_calc_state TYPE REF TO i_fsm_state,
           screen TYPE REF TO cl_calc_screen,
           memory TYPE REF TO cl_calc_memory.


ENDCLASS.
CLASS cl_calc_controller IMPLEMENTATION.

  METHOD CONSTRUCTOR.
    CALL METHOD super->CONSTRUCTOR( ).
    me->screen = calc_screen.
    me->memory = calc_memory.
  ENDMETHOD.

  METHOD set_curr_mem_state.
    IF NOT fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
      me->curr_mem_state = get_state( req_fsm_symbol = fsm_symbol ).
    ENDIF.
  ENDMETHOD.
  METHOD set_curr_calc_state.
    IF NOT fsm_symbol = cx_forbidden_symbol_name=>hold_the_state.
      me->curr_calc_state = get_state( req_fsm_symbol = fsm_symbol ).
    ENDIF.
  ENDMETHOD.
  METHOD push_the_button.
    CASE button_symbol.
      WHEN 'MC'.
        me->set_curr_mem_state( fsm_symbol = me->curr_mem_state->mem_clear( ) ).
      WHEN 'MR'.
        me->set_curr_mem_state( me->curr_mem_state->mem_recover( ) ).
      WHEN 'MS'.
        me->set_curr_mem_state( me->curr_mem_state->mem_save( ) ).
      WHEN 'M_PLUS'.
        me->set_curr_mem_state( me->curr_mem_state->mem_plus( ) ).
      WHEN 'M_MINUS'.
        me->set_curr_mem_state( me->curr_mem_state->mem_minus( ) ).
      WHEN 'BACKSPACE'.
        me->set_curr_calc_state( me->curr_calc_state->backspace( ) ).
      WHEN 'CE'.
        me->set_curr_calc_state( me->curr_calc_state->clear_last( ) ).
      WHEN 'C'.
        me->set_curr_calc_state( me->curr_calc_state->clear_all( ) ).
      WHEN 'NEGATIVE'.
        me->set_curr_calc_state( me->curr_calc_state->negative( ) ).
      WHEN 'DIVIDE'.
        me->set_curr_calc_state( me->curr_calc_state->divide( ) ).
      WHEN 'MULTY'.
        me->set_curr_calc_state( me->curr_calc_state->multiply( ) ).
      WHEN 'MINUS'.
        me->set_curr_calc_state( me->curr_calc_state->minus( ) ).
      WHEN 'PLUS'.
        me->set_curr_calc_state( me->curr_calc_state->plus( ) ).
      WHEN 'SQRT'.
        me->set_curr_calc_state( me->curr_calc_state->sqrt( ) ).
      WHEN 'PERCENT'.
        me->set_curr_calc_state( me->curr_calc_state->percent( ) ).
      WHEN 'ONE_TO_EX'.
        me->set_curr_calc_state( me->curr_calc_state->one_to_ex( ) ).
      WHEN 'EQUALS'.
        me->set_curr_calc_state( me->curr_calc_state->equals( ) ).
      WHEN 'POINT'.
        me->set_curr_calc_state( me->curr_calc_state->point( ) ).
      WHEN 'ONE'.
        me->set_curr_calc_state( me->curr_calc_state->number( 1 ) ).
      WHEN 'TWO'.
        me->set_curr_calc_state( me->curr_calc_state->number( 2 ) ).
      WHEN 'THREE'.
        me->set_curr_calc_state( me->curr_calc_state->number( 3 ) ).
      WHEN 'FOUR'.
        me->set_curr_calc_state( me->curr_calc_state->number( 4 ) ).
      WHEN 'FIVE'.
        me->set_curr_calc_state( me->curr_calc_state->number( 5 ) ).
      WHEN 'SIX'.
        me->set_curr_calc_state( me->curr_calc_state->number( 6 ) ).
      WHEN 'SEVEN'.
        me->set_curr_calc_state( me->curr_calc_state->number( 7 ) ).
      WHEN 'EIGHT'.
        me->set_curr_calc_state( me->curr_calc_state->number( 8 ) ).
      WHEN 'NINE'.
        me->set_curr_calc_state( me->curr_calc_state->number( 9 ) ).
      WHEN 'ZERO'.
        me->set_curr_calc_state( me->curr_calc_state->number( 0 ) ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_unknown_button_symbol.
    ENDCASE.
    me->screen->update_screen( current = me->memory->read_current( )
                               last = me->memory->read_last( )
                               formula = me->memory->read_formula( ) ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

DATA:
     ok_code TYPE sy-ucomm,
     infinite_loop_flag TYPE i VALUE 0,
     SCREEN_TEXT type string,
     SCREEN_FORMULA type string,
     ref_str_a TYPE REF TO string,
     ref_str_b TYPE REF TO string,
     c_screen TYPE REF TO cl_calc_screen,
     c_memory TYPE REF TO cl_calc_memory,
     c_controller TYPE REF TO cl_calc_controller,
     test_tr TYPE REF TO i_fsm_state.

GET REFERENCE OF SCREEN_TEXT INTO ref_str_a.
GET REFERENCE OF SCREEN_FORMULA INTO ref_str_b.

CREATE OBJECT c_screen EXPORTING curr_textbox = ref_str_a formula_textbox = ref_str_b.
CREATE OBJECT c_memory.
CREATE OBJECT c_controller EXPORTING calc_screen = c_screen calc_memory = c_memory.

 DATA:
      m_init_state TYPE REF TO cl_fsm_mem_initial,
      m_saved_state TYPE REF TO cl_fsm_mem_saved,
      c_initial_state TYPE REF TO cl_fsm_calc_initial,
      c_first_state TYPE REF TO cl_fsm_calc_first,
      c_symbol_state TYPE REF TO cl_fsm_calc_symbol,
      c_next_state TYPE REF TO cl_fsm_calc_next,
      c_after_formula TYPE REF TO cl_fsm_calc_after_formula,
      c_error TYPE REF TO cl_fsm_calc_error.

CREATE OBJECT m_init_state EXPORTING screen = c_screen memory = c_memory.
CREATE OBJECT m_saved_state EXPORTING screen = c_screen memory = c_memory.
CREATE OBJECT c_initial_state EXPORTING screen = c_screen memory = c_memory.
CREATE OBJECT c_first_state EXPORTING screen = c_screen memory = c_memory.
CREATE OBJECT c_symbol_state EXPORTING screen = c_screen memory = c_memory.
CREATE OBJECT c_next_state EXPORTING screen = c_screen memory = c_memory.
CREATE OBJECT c_after_formula EXPORTING screen = c_screen memory = c_memory.
CREATE OBJECT c_error EXPORTING screen = c_screen memory = c_memory.

CALL METHOD c_controller->add_state( fsm_symbol = 'initial' fsm_transition = m_init_state ).
CALL METHOD c_controller->add_state( fsm_symbol = 'saved' fsm_transition = m_saved_state ).
CALL METHOD c_controller->add_state( fsm_symbol = 'clear_all' fsm_transition = c_initial_state ).
CALL METHOD c_controller->add_state( fsm_symbol = 'enter_first' fsm_transition = c_first_state ).
CALL METHOD c_controller->add_state( fsm_symbol = 'enter_symbol' fsm_transition = c_symbol_state ).
CALL METHOD c_controller->add_state( fsm_symbol = 'enter_next' fsm_transition = c_next_state ).
CALL METHOD c_controller->add_state( fsm_symbol = 'enter_formula' fsm_transition = c_after_formula ).
CALL METHOD c_controller->add_state( fsm_symbol = 'fall_to_error' fsm_transition = c_error ).

CALL METHOD c_controller->set_curr_mem_state( fsm_symbol = 'initial' ).
CALL METHOD c_controller->set_curr_calc_state( fsm_symbol = 'clear_all' ).

CALL SCREEN 0001.


INCLUDE ztest_amokryshev_test_0001i01.
