*&---------------------------------------------------------------------*
*& Report Z_ODIN_MULTIFILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zodin_multifile LINE-SIZE 1023.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-bl1.
PARAMETERS: p_srdir  TYPE eps2filnam DEFAULT '/location/of/input/files/on/server' LOWER CASE,
            p_bkdir  TYPE string LOWER CASE,
            p_sfname TYPE string LOWER CASE DEFAULT '*' OBLIGATORY.
SELECTION-SCREEN COMMENT /1(70) TEXT-c02.
SELECTION-SCREEN COMMENT /1(70) TEXT-c03.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-bl0.
PARAMETERS: p_skprow TYPE i DEFAULT 0.
SELECTION-SCREEN COMMENT 50(70) TEXT-c01 FOR FIELD p_skprow.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-bl2.
PARAMETERS: p_sep(1)   DEFAULT ';',
            p_delim(1) DEFAULT '"',
            p_1000(1)  DEFAULT '.',
            p_dec(1)   DEFAULT ','.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
  WRITE: / 'PROTOCOL'.
  DATA error TYPE string.
  DATA files TYPE TABLE OF eps2fili.
  CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
    EXPORTING
      iv_dir_name            = p_srdir
    TABLES
      dir_list               = files
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        error = 'invalid_eps_subdir'.
      WHEN 2.
        error = 'sapgparam_failed'.
      WHEN 3.
        error = 'build_directory_failed'.
      WHEN 4.
        error = 'no_authorization'.
      WHEN 5.
        error = 'read_directory_failed'.
      WHEN 6.
        error = 'too_many_read_errors'.
      WHEN 7.
        error = 'empty_directory_list'.
      WHEN 8.
        error = 'other_error'.
    ENDCASE.
    IF sy-batch IS NOT INITIAL.
      MESSAGE s006(zodin) WITH error.
    ELSE.
      MESSAGE s006(zodin) WITH error DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
  DATA srpath TYPE string.
  DATA bkpath TYPE string.
  DATA info TYPE string.
  LOOP AT files INTO DATA(files_s).
    DATA(tabix) = sy-tabix.
    IF files_s-name NP p_sfname.
      info = |Ignoring { files_s-name } because it doesn't cover pattern { p_sfname }|.
      IF sy-batch = abap_true.
        MESSAGE info TYPE 'S'.
      ELSE.
        WRITE: / info.
      ENDIF.
      CONTINUE.
    ENDIF.
    srpath = p_srdir && '/' && files_s-name.
    bkpath = p_bkdir && '/' && files_s-name.
    info = |Processing { srpath }...|.
    MESSAGE info TYPE 'S'.
    IF sy-batch = abap_false.
      FORMAT INTENSIFIED ON.
      WRITE / info.
      FORMAT INTENSIFIED OFF.
    ENDIF.
    TRY.
        DATA(cl_fin_doc) = NEW zcl_odin_posting(
          csv_separator = p_sep
          csv_delimiter = p_delim
          csv_decimal_separator = p_dec
          csv_thousands_separator = p_1000
          path_server = srpath
          path_server_backup = bkpath
          simulation = abap_false
          skip_rows = p_skprow ).
      CATCH zcx_odin INTO DATA(cx_odin).
        info = cx_odin->get_text( ).
        IF sy-batch = abap_true.
          MESSAGE info TYPE 'S'.
        ELSE.
          WRITE: / info.
        ENDIF.
        CONTINUE.
    ENDTRY.
    TRY.
        cl_fin_doc->post( ).
      CATCH zcx_odin INTO cx_odin.
        info = cx_odin->get_text( ).
        IF sy-batch = abap_true.
          MESSAGE info TYPE 'S'.
        ELSE.
          WRITE / info.
        ENDIF.
    ENDTRY.
    cl_fin_doc->display( report = 'ZODIN' pfstatus = 'ALV2' ).
  ENDLOOP.
