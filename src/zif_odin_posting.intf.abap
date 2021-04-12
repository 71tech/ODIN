INTERFACE zif_odin_posting
  PUBLIC .


  INTERFACES if_badi_interface .

  METHODS before
    IMPORTING
      VALUE(document)    TYPE zodin_internal_t
    CHANGING
      !documentheader    TYPE bapiache09
      !customercpd       TYPE bapiacpa09
      !accountgl         TYPE bapiacgl09_tab
      !accountreceivable TYPE bapiacar09_tab
      !accountpayable    TYPE bapiacap09_tab
      !accounttax        TYPE bapiactx09_tab
      !currencyamount    TYPE bapiaccr09_tab
      !extension2        TYPE bapiparex_tab_ac
      !accountwt         TYPE bapiacwt09_tab
    RAISING
      zcx_odin .
  METHODS after_pre_commit
    IMPORTING
      VALUE(obj_key)     TYPE awkey
      VALUE(simulation)  TYPE abap_bool
      VALUE(path_local)  TYPE localfile
      VALUE(hex_file)    TYPE xstring
      VALUE(path_server) TYPE string
    CHANGING
      !document          TYPE zodin_internal_t
    RAISING
      zcx_odin .
  METHODS after_post_commit
    IMPORTING
      VALUE(obj_key)     TYPE awkey
      VALUE(path_local)  TYPE localfile
      VALUE(hex_file)    TYPE xstring
      VALUE(path_server) TYPE string
    CHANGING
      !document          TYPE zodin_internal_t
    RAISING
      zcx_odin .
ENDINTERFACE.
