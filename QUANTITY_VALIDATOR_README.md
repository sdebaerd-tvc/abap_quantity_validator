# ABAP Quantity Validator

## Overview
A comprehensive validation tool to check if a value is a valid ABAP quantity (QUAN type). Supports multiple data types and provides detailed validation results with configurable options.

## Features
- ✅ Validates QUAN (packed number) types
- ✅ Validates integers (I, b, s)
- ✅ Validates floats (F) with precision warnings
- ✅ Validates string representations of quantities
- ✅ Handles thousand separators and spaces
- ✅ Configurable negative value handling
- ✅ Configurable zero value handling
- ✅ Decimal place validation
- ✅ Range checking for typical QUAN limits
- ✅ Detailed error and warning messages

## Installation

1. Create the class `ZCL_QUANTITY_VALIDATOR` using the code in `zcl_quantity_validator.abap`
2. Create the test report `Z_TEST_QUANTITY_VALIDATOR` using the code in `z_test_quantity_validator.abap`
3. Activate both objects

## Usage

### Method 1: Full Validation with Options

```abap
DATA: ls_result TYPE zcl_quantity_validator=>ty_validation_result.

ls_result = zcl_quantity_validator=>validate(
  iv_value          = lv_quantity
  iv_allow_negative = abap_false  " Don't allow negative values
  iv_allow_zero     = abap_true   " Allow zero
  iv_max_decimals   = 2           " Maximum 2 decimal places
).

IF ls_result-is_valid = abap_true.
  WRITE: / 'Valid quantity:', ls_result-numeric_value.
ELSE.
  WRITE: / 'Error:', ls_result-error_message.
ENDIF.
```

### Method 2: String Validation

```abap
DATA: ls_result TYPE zcl_quantity_validator=>ty_validation_result.

ls_result = zcl_quantity_validator=>validate_string(
  iv_string         = '1,234.56'
  iv_allow_negative = abap_true
  iv_allow_zero     = abap_true
  iv_max_decimals   = 3
).

IF ls_result-is_valid = abap_true.
  " Use the converted numeric value
  DATA(lv_amount) = ls_result-numeric_value.
ENDIF.
```

### Method 3: Quick Boolean Check

```abap
IF zcl_quantity_validator=>is_quantity( lv_value ) = abap_true.
  " Process as quantity
ELSE.
  " Handle invalid quantity
ENDIF.
```

## Parameters

### validate / validate_string Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `iv_value` / `iv_string` | ANY / STRING | - | Value to validate |
| `iv_allow_negative` | ABAP_BOOL | ABAP_TRUE | Allow negative values |
| `iv_allow_zero` | ABAP_BOOL | ABAP_TRUE | Allow zero values |
| `iv_max_decimals` | I | 3 | Maximum decimal places |

### Result Structure (ty_validation_result)

| Field | Type | Description |
|-------|------|-------------|
| `is_valid` | ABAP_BOOL | Whether the value is valid |
| `value_type` | STRING | Type of input (QUAN/PACKED, INTEGER, FLOAT, STRING) |
| `numeric_value` | P(16,3) | Converted numeric value |
| `error_message` | STRING | Error description (if invalid) |
| `warning_message` | STRING | Warning message (if applicable) |

## Supported Data Types

### Fully Supported
- **QUAN** - Packed number quantity type
- **P** - Packed number (DEC)
- **I** - Integer (INT4)
- **b** - Integer (INT1)
- **s** - Integer (INT2)
- **C** - Character string
- **STRING** - String type

### Supported with Warnings
- **F** - Float (precision warning issued)

### String Formats Supported
- Simple numbers: `"123"`, `"456.78"`
- Negative numbers: `"-123.45"`
- Decimal numbers: `"0.5"`, `".5"`
- With thousand separators: `"1,234,567.89"`
- With spaces: `"  123.45  "`

## Validation Rules

### Numeric Format
- Must contain at least one digit
- Only one decimal point allowed
- Sign (+ or -) only at the beginning
- Thousand separators (,) are allowed and ignored
- Spaces are allowed and ignored

### Value Constraints
- Negative values: Configurable via `iv_allow_negative`
- Zero values: Configurable via `iv_allow_zero`
- Decimal places: Configurable via `iv_max_decimals`
- Maximum value: Warning if >= 9,999,999,999,999.999

## Examples

### Example 1: Validate User Input from Screen

```abap
DATA: lv_input  TYPE string,
      ls_result TYPE zcl_quantity_validator=>ty_validation_result.

PARAMETERS: p_qty TYPE string.

lv_input = p_qty.

ls_result = zcl_quantity_validator=>validate_string(
  iv_string         = lv_input
  iv_allow_negative = abap_false
  iv_allow_zero     = abap_false
  iv_max_decimals   = 2
).

IF ls_result-is_valid = abap_false.
  MESSAGE ls_result-error_message TYPE 'E'.
ELSE.
  " Use ls_result-numeric_value for processing
  DATA(lv_quantity) = ls_result-numeric_value.
ENDIF.
```

### Example 2: Validate Stock Quantity

```abap
DATA: lv_stock  TYPE quan,
      ls_result TYPE zcl_quantity_validator=>ty_validation_result.

SELECT SINGLE labst INTO lv_stock
  FROM mard
  WHERE matnr = '000000000000000001'
    AND werks = '1000'.

ls_result = zcl_quantity_validator=>validate(
  iv_value          = lv_stock
  iv_allow_negative = abap_false  " Stock cannot be negative
  iv_allow_zero     = abap_true   " Zero stock is valid
).

IF ls_result-is_valid = abap_true.
  WRITE: / 'Valid stock quantity:', ls_result-numeric_value.
ENDIF.
```

### Example 3: Validate Order Quantity with Decimal Restrictions

```abap
DATA: lv_order_qty TYPE string VALUE '100.567',
      ls_result    TYPE zcl_quantity_validator=>ty_validation_result.

" Order quantities limited to 2 decimals
ls_result = zcl_quantity_validator=>validate_string(
  iv_string       = lv_order_qty
  iv_max_decimals = 2
).

IF ls_result-is_valid = abap_false.
  MESSAGE ls_result-error_message TYPE 'E'.
  " Output: "Too many decimal places (3), maximum allowed: 2"
ENDIF.
```

### Example 4: Batch Validation

```abap
DATA: lt_quantities TYPE TABLE OF string,
      ls_result     TYPE zcl_quantity_validator=>ty_validation_result,
      lv_qty        TYPE string.

lt_quantities = VALUE #(
  ( '100' )
  ( '250.50' )
  ( 'INVALID' )
  ( '-50' )
  ( '1,234.56' )
).

LOOP AT lt_quantities INTO lv_qty.
  ls_result = zcl_quantity_validator=>validate_string( lv_qty ).
  
  WRITE: / |Quantity: { lv_qty WIDTH = 15 }|.
  
  IF ls_result-is_valid = abap_true.
    WRITE: |✓ Valid ({ ls_result-numeric_value })|.
  ELSE.
    WRITE: |✗ Invalid - { ls_result-error_message }|.
  ENDIF.
ENDLOOP.
```

### Example 5: File Upload Validation

```abap
DATA: lt_upload_data TYPE TABLE OF string,
      lv_line        TYPE string,
      ls_result      TYPE zcl_quantity_validator=>ty_validation_result,
      lt_errors      TYPE TABLE OF string.

" Upload file
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING filename = 'C:\quantities.txt'
  TABLES data_tab = lt_upload_data.

LOOP AT lt_upload_data INTO lv_line.
  ls_result = zcl_quantity_validator=>validate_string(
    iv_string         = lv_line
    iv_allow_negative = abap_false
    iv_max_decimals   = 3
  ).
  
  IF ls_result-is_valid = abap_false.
    APPEND |Line { sy-tabix }: { ls_result-error_message }| TO lt_errors.
  ENDIF.
ENDLOOP.

IF lt_errors IS NOT INITIAL.
  " Display errors
  LOOP AT lt_errors INTO DATA(lv_error).
    WRITE: / lv_error.
  ENDLOOP.
ENDIF.
```

## Error Messages

| Error | Description |
|-------|-------------|
| Empty value | Input is empty or blank |
| Invalid quantity format | Contains non-numeric characters or invalid format |
| Negative values not allowed | Value is negative but not permitted |
| Zero value not allowed | Value is zero but not permitted |
| Too many decimal places | Exceeds maximum allowed decimals |
| Unsupported data type | Input type not supported |

## Warnings

| Warning | Description |
|---------|-------------|
| Float type used - precision may be lost | Float types may have precision issues |
| Value is very large | Value >= 9,999,999,999,999.999 |

## Test Cases

Run `Z_TEST_QUANTITY_VALIDATOR` to see all test cases:

1. ✓ Valid QUAN (packed) value
2. ✓ Valid integer value
3. ✓ Valid string quantity
4. ✓ Negative quantity (allowed)
5. ✗ Negative quantity (not allowed)
6. ✓ Zero value (allowed)
7. ✗ Zero value (not allowed)
8. ✗ Too many decimal places
9. ✗ Invalid string format
10. ✓ String with thousand separators
11. ✗ Empty string
12. ✓ String with spaces
13. ⚠ Very large number
14. ⚠ Float value (precision warning)
15. ✓ Quick validation
16. ✓ Decimal-only values

## Best Practices

1. **Always validate user input** before processing quantities
2. **Use appropriate constraints** based on business rules
3. **Handle errors gracefully** with clear messages to users
4. **Consider decimal precision** requirements for your use case
5. **Validate early** in the data flow to catch errors quickly

## Integration with Standard ABAP

### Use in Function Modules

```abap
FUNCTION z_create_order.
  IMPORTING iv_quantity TYPE string.
  
  DATA: ls_result TYPE zcl_quantity_validator=>ty_validation_result.
  
  ls_result = zcl_quantity_validator=>validate_string( iv_quantity ).
  
  IF ls_result-is_valid = abap_false.
    RAISE quantity_invalid.
  ENDIF.
  
  " Process with ls_result-numeric_value
ENDFUNCTION.
```

### Use in Class Methods

```abap
CLASS zcl_order_processor DEFINITION.
  METHODS: validate_order_item
    IMPORTING iv_quantity TYPE any
    RAISING   zcx_invalid_quantity.
ENDCLASS.

CLASS zcl_order_processor IMPLEMENTATION.
  METHOD validate_order_item.
    DATA(ls_result) = zcl_quantity_validator=>validate( iv_quantity ).
    
    IF ls_result-is_valid = abap_false.
      RAISE EXCEPTION TYPE zcx_invalid_quantity
        EXPORTING textid = zcx_invalid_quantity=>invalid_format
                  value  = |{ iv_quantity }|
                  reason = ls_result-error_message.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

## Technical Details

### Type Detection
Uses ABAP's `DESCRIBE FIELD` to identify input type and apply appropriate validation logic.

### Number Conversion
- Removes thousand separators and spaces
- Converts to packed number (P type) with 16 digits and 3 decimals
- Handles all standard ABAP numeric formats

### Decimal Counting
- Converts value to string
- Locates decimal point
- Counts non-zero digits after decimal point
- Ignores trailing zeros

## Limitations

- Maximum precision: 16 digits + 3 decimal places
- Does not validate against specific unit of measure rules
- Does not perform currency-specific validations
- String conversion may lose precision for very large numbers

## License

Free to use and modify for your ABAP projects.
