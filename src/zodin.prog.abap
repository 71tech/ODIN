*&---------------------------------------------------------------------*
*& Report Z_ODIN
*&---------------------------------------------------------------------*
*MIT License
*
*Copyright (c) 2021 71TECH
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
*&---------------------------------------------------------------------*

REPORT zodin.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-bl1.
PARAMETERS: p_infile TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-bl3.
PARAMETERS: p_srpath TYPE string LOWER CASE,
            p_bkpath TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-bl0.
PARAMETERS: p_batch TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN COMMENT 50(70) TEXT-c00 FOR FIELD p_batch.
PARAMETERS: p_skprow TYPE i DEFAULT 0.
SELECTION-SCREEN COMMENT 50(70) TEXT-c01 FOR FIELD p_skprow.
SELECTION-SCREEN END OF BLOCK b0.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-bl2.
PARAMETERS: p_sep(1)   DEFAULT ';',
            p_delim(1) DEFAULT '"',
            p_1000(1)  DEFAULT '.',
            p_dec(1)   DEFAULT ','.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_infile.
  DATA: filetab   TYPE filetable,
        filetab_s LIKE LINE OF filetab.
  DATA rc TYPE i.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select a file'
      file_filter             = 'Excel or csv (*.xlsx;*.xlsm;*.xls;*.csv)|*.xlsx;*.xlsm;*.xls;*.csv'
    CHANGING
      file_table              = filetab
      rc                      = rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0.
    READ TABLE filetab INTO filetab_s INDEX 1.
    p_infile = filetab_s-filename.
  ENDIF.

START-OF-SELECTION.
  TRY.
      IF sy-batch = abap_true OR p_batch = abap_true.
        DATA(simulation) = abap_false.
      ELSE.
        simulation = abap_true.
      ENDIF.
      DATA(cl_fin_doc) = NEW zcl_odin_posting(
        path_local = p_infile
*        hex_file = p_hexfil
        csv_separator = p_sep
        csv_delimiter = p_delim
        csv_decimal_separator = p_dec
        csv_thousands_separator = p_1000
        path_server = p_srpath
        path_server_backup = p_bkpath
        simulation = simulation
        skip_rows = p_skprow ).
    CATCH zcx_ODIN INTO DATA(cx_odin).
      DATA(error) = cx_odin->get_text( ).
      IF sy-batch = abap_false.
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        MESSAGE error TYPE 'E'.
      ENDIF.
  ENDTRY.
  TRY.
      cl_fin_doc->post( ).
    CATCH zcx_ODIN INTO cx_odin.
      error = cx_odin->get_text( ).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
  cl_fin_doc->display( report = sy-repid pfstatus = 'ALV2' ).
