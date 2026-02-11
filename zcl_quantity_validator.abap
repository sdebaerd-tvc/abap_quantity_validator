*&---------------------------------------------------------------------*
*& Class ZCL_QUANTITY_VALIDATOR
*&---------------------------------------------------------------------*
*& ABAP Quantity Validator
*&---------------------------------------------------------------------*
class ZCL_QUANTITY_VALIDATOR definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_validation_result,
        is_valid        TYPE abap_bool,
        value_type      TYPE string,
        numeric_value   TYPE p LENGTH 15 DECIMALS 3,
        error_message   TYPE string,
        warning_message TYPE string,
      END OF ty_validation_result .

  class-methods VALIDATE
    importing
      !IV_VALUE type ANY
      !IV_ALLOW_NEGATIVE type ABAP_BOOL default ABAP_TRUE
      !IV_ALLOW_ZERO type ABAP_BOOL default ABAP_TRUE
      !IV_MAX_DECIMALS type I default 3
    returning
      value(RS_RESULT) type TY_VALIDATION_RESULT .
  class-methods VALIDATE_STRING
    importing
      !IV_STRING type STRING
      !IV_ALLOW_NEGATIVE type ABAP_BOOL default ABAP_TRUE
      !IV_ALLOW_ZERO type ABAP_BOOL default ABAP_TRUE
      !IV_MAX_DECIMALS type I default 3
    returning
      value(RS_RESULT) type TY_VALIDATION_RESULT .
  class-methods IS_QUANTITY
    importing
      !IV_VALUE type ANY
    returning
      value(RV_RESULT) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS check_numeric_string
      IMPORTING
        iv_string       TYPE string
      RETURNING
        VALUE(rv_valid) TYPE abap_bool.

    CLASS-METHODS convert_to_quantity
      IMPORTING
        iv_string          TYPE string
      EXPORTING
        ev_too_long        TYPE abap_bool
      RETURNING
        VALUE(rv_quantity) TYPE ty_validation_result-numeric_value.

    CLASS-METHODS count_decimals
      IMPORTING
        iv_value        TYPE any
      RETURNING
        VALUE(rv_count) TYPE i.

ENDCLASS.



CLASS ZCL_QUANTITY_VALIDATOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_QUANTITY_VALIDATOR=>CHECK_NUMERIC_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STRING                      TYPE        STRING
* | [<-()] RV_VALID                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_numeric_string.
    DATA: lv_string      TYPE string,
          lv_char        TYPE c LENGTH 1,
          lv_has_digit   TYPE abap_bool,
          lv_has_decimal TYPE abap_bool,
          lv_has_sign    TYPE abap_bool,
          lv_len         TYPE i.

    rv_valid = abap_false.
    lv_string = iv_string.
    CONDENSE lv_string.

    lv_len = strlen( lv_string ).

    IF lv_len = 0.
      RETURN.
    ENDIF.

    DO lv_len TIMES.
      DATA(lv_index) = sy-index - 1.
      lv_char = lv_string+lv_index(1).

      CASE lv_char.
        WHEN '0' OR '1' OR '2' OR '3' OR '4' OR
             '5' OR '6' OR '7' OR '8' OR '9'.
          lv_has_digit = abap_true.

        WHEN '.'.
          " Decimal point - only one allowed
          IF lv_has_decimal = abap_true.
            RETURN.  " Second decimal point found
          ENDIF.
          lv_has_decimal = abap_true.

        WHEN ',' OR ' '.
          " Thousands separator or space - check if in valid position
          " Allow but ignore for now
          CONTINUE.

        WHEN '-' OR '+'.
          " Sign - must be first character
          IF sy-index > 1.
            RETURN.  " Sign not at start
          ENDIF.
          IF lv_has_sign = abap_true.
            RETURN.  " Multiple signs
          ENDIF.
          lv_has_sign = abap_true.

        WHEN OTHERS.
          " Invalid character
          RETURN.
      ENDCASE.
    ENDDO.

    " Must have at least one digit
    IF lv_has_digit = abap_true.
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_QUANTITY_VALIDATOR=>CONVERT_TO_QUANTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STRING                      TYPE        STRING
* | [<---] EV_TOO_LONG                    TYPE        ABAP_BOOL
* | [<-()] RV_QUANTITY                    TYPE        TY_VALIDATION_RESULT-NUMERIC_VALUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_to_quantity.
    DATA: lv_string TYPE string.

    lv_string = iv_string.

    " Remove thousands separators and spaces
    REPLACE ALL OCCURRENCES OF ',' IN lv_string WITH ''.
    CONDENSE lv_string NO-GAPS.
    " Check for reasonable range (QUAN is typically 12 digits + 3 decimals)
    IF abs( lv_string ) >= '999999999999.999'.
      ev_too_long = abap_true.
    ENDIF.
    " Convert to quantity
    TRY.
        rv_quantity = lv_string.
      CATCH cx_root.
        rv_quantity = 0.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_QUANTITY_VALIDATOR=>COUNT_DECIMALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [<-()] RV_COUNT                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD count_decimals.
    DATA: lv_string      TYPE string,
          lv_decimal_pos TYPE i,
          lv_fractional  TYPE string.

    rv_count = 0.

    " Convert to string
    lv_string = iv_value.
    SHIFT lv_string RIGHT DELETING TRAILING '-'.
    SHIFT lv_string RIGHT DELETING TRAILING '0'.
* also delete trailing '.', if possible
    SHIFT lv_string RIGHT DELETING TRAILING'.'.
    CONDENSE lv_string NO-GAPS.

    " Find decimal point
    FIND '.' IN lv_string MATCH OFFSET lv_decimal_pos.

    IF sy-subrc = 0.
      " Get fractional part
      lv_decimal_pos = lv_decimal_pos + 1.
      lv_fractional = lv_string+lv_decimal_pos.
      rv_count = strlen( lv_fractional ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_QUANTITY_VALIDATOR=>IS_QUANTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [<-()] RV_RESULT                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_quantity.
    DATA: ls_result TYPE ty_validation_result.

    ls_result = validate( iv_value ).
    rv_result = ls_result-is_valid.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_QUANTITY_VALIDATOR=>VALIDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE                       TYPE        ANY
* | [--->] IV_ALLOW_NEGATIVE              TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_ALLOW_ZERO                  TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_MAX_DECIMALS                TYPE        I (default =3)
* | [<-()] RS_RESULT                      TYPE        TY_VALIDATION_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate.
    DATA: lv_string TYPE string,
          lv_type   TYPE c LENGTH 1.

    " Initialize result
    CLEAR rs_result.
    rs_result-is_valid = abap_false.

    " Get type of input
    DESCRIBE FIELD iv_value TYPE lv_type.
    IF lv_type <> 'F'.
      " Check decimal places
      DATA(lv_decimal_count) = count_decimals( iv_value ).
      IF lv_decimal_count > iv_max_decimals.
        rs_result-is_valid = abap_false.
        rs_result-error_message = |Too many dec. ({ lv_decimal_count }), max: { iv_max_decimals }|.
        RETURN.
      ENDIF.
    ENDIF.

    CASE lv_type.
      WHEN 'P'.  " Packed number (QUAN, DEC, etc.)
        " Check for reasonable range (QUAN is typically 12 digits + 3 decimals)
        IF abs( iv_value ) >= '999999999999.999'.
          rs_result-error_message = 'Value is too large'.
          RETURN.
        ENDIF.
        rs_result-value_type = 'QUAN/PACKED'.
        rs_result-numeric_value = iv_value.
        rs_result-is_valid = abap_true.

      WHEN 'I' OR 'b' OR 's'.  " Integer types
        " Check for reasonable range (QUAN is typically 12 digits + 3 decimals)
        IF abs( iv_value ) >= '999999999999.999'.
          rs_result-error_message = 'Value is too large'.
          RETURN.
        ENDIF.
        rs_result-value_type = 'INTEGER'.
        rs_result-numeric_value = iv_value.
        rs_result-is_valid = abap_true.

      WHEN 'F'.  " Float
        " Check for reasonable range (QUAN is typically 12 digits + 3 decimals)
        IF abs( iv_value ) >= '999999999999.999'.
          rs_result-error_message = 'Value is too large'.
          RETURN.
        ENDIF.
        rs_result-value_type = 'FLOAT'.
        rs_result-numeric_value = iv_value.
        rs_result-is_valid = abap_true.
        rs_result-warning_message = 'Float type used - precision may be lost'.
      WHEN 'C' OR 'g'.  " Character or String
        lv_string = iv_value.
        CONDENSE lv_string.

        IF lv_string IS INITIAL.
          rs_result-error_message = 'Empty value'.
          RETURN.
        ENDIF.

        " Check if it's a valid numeric string
        IF check_numeric_string( lv_string ) = abap_false.
          rs_result-error_message = |Invalid format: '{ lv_string }'|.
          RETURN.
        ENDIF.
        DATA: lv_too_long TYPE abap_bool.
        " Convert to quantity
        rs_result-numeric_value = convert_to_quantity( EXPORTING iv_string = lv_string IMPORTING ev_too_long = lv_too_long ).
        IF lv_too_long = abap_true.
          rs_result-error_message = 'Value is too large'.
          RETURN.
        ENDIF.
        rs_result-value_type = 'STRING'.
        rs_result-is_valid = abap_true.

      WHEN OTHERS.
        rs_result-error_message = |Unsupp. data type: { lv_type }|.
        RETURN.
    ENDCASE.

    " Additional validations
    IF rs_result-is_valid = abap_true.

      " Check for negative values
      IF iv_allow_negative = abap_false AND rs_result-numeric_value < 0.
        rs_result-is_valid = abap_false.
        rs_result-error_message = 'Neg. values not allowed'.
        RETURN.
      ENDIF.

      " Check for zero
      IF iv_allow_zero = abap_false AND rs_result-numeric_value = 0.
        rs_result-is_valid = abap_false.
        rs_result-error_message = 'Zero value not allowed'.
        RETURN.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_QUANTITY_VALIDATOR=>VALIDATE_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STRING                      TYPE        STRING
* | [--->] IV_ALLOW_NEGATIVE              TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_ALLOW_ZERO                  TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_MAX_DECIMALS                TYPE        I (default =3)
* | [<-()] RS_RESULT                      TYPE        TY_VALIDATION_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD validate_string.
    DATA: lv_string TYPE string.

    lv_string = iv_string.
    CONDENSE lv_string.

    " Use generic validate method
    rs_result = validate(
      iv_value          = lv_string
      iv_allow_negative = iv_allow_negative
      iv_allow_zero     = iv_allow_zero
      iv_max_decimals   = iv_max_decimals ).

  ENDMETHOD.
ENDCLASS.