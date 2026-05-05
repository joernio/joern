CLASS zcl_simple DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS greet
      IMPORTING
        iv_name TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS add
      IMPORTING
        iv_a TYPE i
        iv_b TYPE i
      RETURNING
        VALUE(rv_sum) TYPE i.

    METHODS run
      IMPORTING
        iv_input TYPE string.
ENDCLASS.

CLASS zcl_simple IMPLEMENTATION.
  METHOD greet.
    DATA: lv_greeting TYPE string.
    lv_greeting = iv_name.
    rv_result = lv_greeting.
    me->run( iv_input = lv_greeting ).
  ENDMETHOD.

  METHOD add.
    rv_sum = iv_a.
  ENDMETHOD.

  METHOD run.
    zcl_simple=>greet( iv_name = iv_input ).
  ENDMETHOD.
ENDCLASS.
