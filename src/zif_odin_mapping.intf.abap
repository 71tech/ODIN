INTERFACE zif_odin_mapping
  PUBLIC .


  INTERFACES if_badi_interface .

  METHODS before
    CHANGING
      !external TYPE zodin_external_t
    RAISING
      zcx_odin .
  METHODS after
    IMPORTING
      VALUE(external)  TYPE zodin_external_t
      VALUE(key_value) TYPE zodin_key_value_t
    CHANGING
      !internal        TYPE zodin_internal_t
    RAISING
      zcx_odin .
ENDINTERFACE.
