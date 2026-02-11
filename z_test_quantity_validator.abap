*&---------------------------------------------------------------------*
*& Report Z_TEST_QUANTITY_VALIDATOR
*&---------------------------------------------------------------------*
*& Test program for ABAP Quantity Validator
*&---------------------------------------------------------------------*
REPORT z_test_quantity_validator.

DATA: ls_result TYPE zcl_quantity_validator=>ty_validation_result.

WRITE: / 'ABAP Quantity Validator - Test Results'.
WRITE: / '========================================'.
SKIP.

" Test 1: Valid packed quantity
WRITE: / 'Test 1: Valid QUAN (packed) value'.
WRITE: / '-----------------------------------'.
DATA: lv_quan TYPE quan VALUE '123.456'.
ls_result = zcl_quantity_validator=>validate( lv_quan ).
PERFORM display_result USING ls_result 'QUAN: 123.456'.
SKIP.

" Test 2: Valid integer
WRITE: / 'Test 2: Valid integer value'.
WRITE: / '-----------------------------------'.
DATA: lv_int TYPE i VALUE 100.
ls_result = zcl_quantity_validator=>validate( lv_int ).
PERFORM display_result USING ls_result 'INTEGER: 100'.
SKIP.

" Test 3: Valid string quantity
WRITE: / 'Test 3: Valid string quantity'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( '250.75' ).
PERFORM display_result USING ls_result 'STRING: "250.75"'.
SKIP.

" Test 4: Negative quantity (allowed by default)
WRITE: / 'Test 4: Negative quantity (allowed)'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( '-50.25' ).
PERFORM display_result USING ls_result 'STRING: "-50.25"'.
SKIP.

" Test 5: Negative quantity (not allowed)
WRITE: / 'Test 5: Negative quantity (not allowed)'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string(
  iv_string = '-50.25'
  iv_allow_negative = abap_false ).
PERFORM display_result USING ls_result 'STRING: "-50.25" (negative not allowed)'.
SKIP.

" Test 6: Zero value (allowed)
WRITE: / 'Test 6: Zero value (allowed)'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( '0' ).
PERFORM display_result USING ls_result 'STRING: "0"'.
SKIP.

" Test 7: Zero value (not allowed)
WRITE: / 'Test 7: Zero value (not allowed)'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string(
  iv_string = '0'
  iv_allow_zero = abap_false ).
PERFORM display_result USING ls_result 'STRING: "0" (zero not allowed)'.
SKIP.

" Test 8: Too many decimals
WRITE: / 'Test 8: Too many decimal places'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string(
  iv_string = '123.45678'
  iv_max_decimals = 3 ).
PERFORM display_result USING ls_result 'STRING: "123.45678" (max 3 decimals)'.
SKIP.

" Test 9: Invalid string format
WRITE: / 'Test 9: Invalid string format'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( 'ABC123' ).
PERFORM display_result USING ls_result 'STRING: "ABC123"'.
SKIP.

" Test 10: String with thousand separators
WRITE: / 'Test 10: String with thousand separators'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( '1,234,567.89' ).
PERFORM display_result USING ls_result 'STRING: "1,234,567.89"'.
SKIP.

" Test 11: Empty string
WRITE: / 'Test 11: Empty string'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( '' ).
PERFORM display_result USING ls_result 'STRING: ""'.
SKIP.

" Test 12: String with spaces
WRITE: / 'Test 12: String with spaces'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( '  123.45  ' ).
PERFORM display_result USING ls_result 'STRING: "  123.45  "'.
SKIP.

" Test 13: Very large number
WRITE: / 'Test 13: Very large number'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( '9999999999999.999' ).
PERFORM display_result USING ls_result 'STRING: "9999999999999.999"'.
SKIP.

" Test 14: Float value (with warning)
WRITE: / 'Test 14: Float value'.
WRITE: / '-----------------------------------'.
DATA: lv_float TYPE f VALUE '123.456'.
ls_result = zcl_quantity_validator=>validate( lv_float ).
PERFORM display_result USING ls_result 'FLOAT: 123.456'.
SKIP.

" Test 15: Quick check using is_quantity
WRITE: / 'Test 15: Quick validation using is_quantity'.
WRITE: / '-----------------------------------'.
IF zcl_quantity_validator=>is_quantity( '500.25' ) = abap_true.
  WRITE: / '  "500.25" is a valid quantity ✓'.
ELSE.
  WRITE: / '  "500.25" is NOT a valid quantity ✗'.
ENDIF.

IF zcl_quantity_validator=>is_quantity( 'INVALID' ) = abap_true.
  WRITE: / '  "INVALID" is a valid quantity ✓'.
ELSE.
  WRITE: / '  "INVALID" is NOT a valid quantity ✗'.
ENDIF.
SKIP.

" Test 16: Decimal-only values
WRITE: / 'Test 16: Decimal-only values'.
WRITE: / '-----------------------------------'.
ls_result = zcl_quantity_validator=>validate_string( '.5' ).
PERFORM display_result USING ls_result 'STRING: ".5"'.
SKIP.

ls_result = zcl_quantity_validator=>validate_string( '0.999' ).
PERFORM display_result USING ls_result 'STRING: "0.999"'.
SKIP.

WRITE: / '========================================'.
WRITE: / 'All tests completed!'.

*&---------------------------------------------------------------------*
*& Form display_result
*&---------------------------------------------------------------------*
FORM display_result USING ps_result TYPE zcl_quantity_validator=>ty_validation_result
                          pv_input TYPE string.

  WRITE: / |Input: { pv_input }|.

  IF ps_result-is_valid = abap_true.
    WRITE: / '  Status: ✓ VALID'.
    WRITE: / |  Type: { ps_result-value_type }|.
    WRITE: / |  Numeric value: { ps_result-numeric_value }|.

    IF ps_result-warning_message IS NOT INITIAL.
      WRITE: / |  ⚠ Warning: { ps_result-warning_message }|.
    ENDIF.
  ELSE.
    WRITE: / '  Status: ✗ INVALID'.
    WRITE: / |  Error: { ps_result-error_message }|.
  ENDIF.

ENDFORM.
