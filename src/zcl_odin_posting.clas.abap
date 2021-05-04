class ZCL_ODIN_POSTING definition
  public
  create public .

public section.

  types:
    type_component_tab TYPE TABLE OF string .

  data POSTED type ABAP_BOOL read-only .
  data PATH_LOCAL type LOCALFILE read-only .
  data STRATEGY type STRING read-only .
  data EXTERNAL type ZODIN_EXTERNAL_T read-only .
  data INTERNAL type ZODIN_INTERNAL_T read-only .
  data ALV type ref to CL_SALV_TABLE read-only .
  data SIMULATION type ABAP_BOOL read-only .
  data ACTIVE_COMPONENTS type TYPE_COMPONENT_TAB read-only .
  constants:
    BEGIN OF state,
        "For now let's not use unicode symbols with respect to non-unicode systems
*        ok    VALUE '✓',
*        ready VALUE '❓',
*        error VALUE '✗',
        ok    TYPE string VALUE 'OK',
        ready TYPE string VALUE 'READY',
        error TYPE string VALUE 'ERROR',
        unknown TYPE string VALUE 'UNKNOWN',
      END OF state .
  data HEX_FILE type XSTRING read-only .
  constants ABAP2XLSX type STRING value 'abap2xlsx' ##NO_TEXT.
  constants CSV type STRING value 'csv' ##NO_TEXT.
  data INPUT_MODE type STRING read-only .
  data CSV_SEPARATOR type CHAR1 read-only .
  data CSV_DELIMITER type CHAR1 read-only .
  data CSV_DECIMAL_SEPARATOR type CHAR1 read-only .
  data CSV_THOUSANDS_SEPARATOR type CHAR1 read-only .
  data PATH_SERVER type STRING read-only .
  data PATH_SERVER_BACKUP type STRING read-only .
  constants ODIN_EXTENSION_STRUCTURE type STRING value 'ZODIN_EXTENSION' ##NO_TEXT.

  methods ON_USER_COMMAND
    for event ADDED_FUNCTION of CL_SALV_EVENTS_TABLE
    importing
      !E_SALV_FUNCTION .
  class-methods HAS_DATE_TYPE
    importing
      !EXTERNAL type ANY
    returning
      value(RESULT) type ABAP_BOOL .
  methods ON_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
  class-methods HAS_NUMERIC_TYPE
    importing
      value(INPUT) type ANY
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods TO_KEY_VALUE
    importing
      value(T_EXTERNAL) type ZODIN_EXTERNAL_T
    exporting
      value(T_KEY_VALUE) type ZODIN_KEY_VALUE_T .
  methods CONSTRUCTOR
    importing
      value(PATH_LOCAL) type LOCALFILE optional
      value(SIMULATION) type ABAP_BOOL default 'X'
      value(DATA_STRING) type STRING optional
      value(DATA_TYPE) type STRING optional
      value(HEX_FILE) type XSTRING optional
      value(CSV_SEPARATOR) type CHAR1 default ';'
      value(CSV_DELIMITER) type CHAR1 default '"'
      value(CSV_DECIMAL_SEPARATOR) type CHAR1 default ','
      value(CSV_THOUSANDS_SEPARATOR) type CHAR1 default '.'
      value(PATH_SERVER) type STRING optional
      value(PATH_SERVER_BACKUP) type STRING optional
      value(SKIP_ROWS) type INT4 optional
    raising
      ZCX_ODIN .
  methods DISPLAY
    importing
      !REPORT type SYREPID
      !PFSTATUS type SYPFKEY .
  methods POST
    raising
      ZCX_ODIN .
  methods SET_SIMULATION
    importing
      !SIMULATION type ABAP_BOOL .
protected section.

  methods POST_DOCUMENT
    importing
      value(DOCUMENT) type ZODIN_INTERNAL_T
    exporting
      value(BELNR) type BELNR_D
      value(BUKRS) type BUKRS_D
      value(GJAHR) type GJAHR
      value(OBJ_KEY) type BAPIACHE09-OBJ_KEY
    raising
      ZCX_ODIN .
  methods LOAD
    importing
      value(SKIP_ROWS) type INT4 optional
    raising
      ZCX_ODIN .
  methods MAP
    raising
      ZCX_ODIN .
  methods DISPLAY_GUI
    importing
      value(REPORT) type SYREPID
      value(PFSTATUS) type SYPFKEY
    raising
      CX_SALV_MSG
      CX_SALV_NOT_FOUND
      CX_SALV_WRONG_CALL .
  methods DISPLAY_BATCH .
private section.
ENDCLASS.



CLASS ZCL_ODIN_POSTING IMPLEMENTATION.


  METHOD constructor.
    me->path_local = path_local.
    me->simulation = simulation.
    me->csv_separator = csv_separator.
    me->csv_delimiter = csv_delimiter.
    me->csv_decimal_separator = csv_decimal_separator.
    me->csv_thousands_separator = csv_thousands_separator.
    me->path_server = path_server.
    me->path_server_backup = path_server_backup.
    me->hex_file = hex_file.
    me->load( skip_rows = skip_rows ).
    me->map( ).
  ENDMETHOD.


  METHOD display.
    display_gui( report   = report
                 pfstatus = pfstatus ).
    IF sy-batch = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD display_batch.
    DATA internal_s TYPE zodin_internal.
    DATA strdesc TYPE REF TO cl_abap_structdescr.
    DATA t_active_components TYPE TABLE OF sy-index.
    FIELD-SYMBOLS: <fs> TYPE any.
    LOOP AT me->internal INTO DATA(s_internal).
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE s_internal TO <fs>.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
        IF <fs> IS NOT INITIAL.
          READ TABLE t_active_components WITH KEY table_line = sy-index TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            APPEND sy-index TO t_active_components.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
    SORT t_active_components BY table_line ASCENDING.
    strdesc ?= cl_abap_structdescr=>describe_by_data( internal_s ).
    DATA(comps) = strdesc->get_components( ).
    DATA output TYPE string.
    LOOP AT comps INTO DATA(comp).
      READ TABLE t_active_components WITH KEY table_line = sy-tabix TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        output = output && '|' && comp-name.
      ENDIF.
    ENDLOOP.
    SHIFT output LEFT BY 1 PLACES.
    WRITE / output.
    LOOP AT me->internal INTO s_internal.
      CLEAR output.
      LOOP AT t_active_components INTO DATA(active_component).
        ASSIGN COMPONENT active_component OF STRUCTURE s_internal TO <fs>.
        output = output && '|' && <fs>.
      ENDLOOP.
      SHIFT output LEFT BY 1 PLACES.
      WRITE / output.
    ENDLOOP.
  ENDMETHOD.


  METHOD display_gui.
    DATA internal_s TYPE zodin_internal.
    cl_salv_table=>factory(
*      EXPORTING
*        list_display   = if_salv_c_bool_sap=>false " ALV Displayed in List Mode
*        r_container    =                           " Abstract Container for GUI Controls
*        container_name =
      IMPORTING
        r_salv_table   = me->alv                          " Basis Class Simple ALV Tables
      CHANGING
        t_table        = me->internal
    ).
*    CATCH cx_salv_msg. " ALV: General Error Class with Message
    DATA(cols) = alv->get_columns( ).
    DATA(col) = cols->get_column( 'COUNTER' ).
    col->set_short_text( 'Counter'(000)  ).
*    CLEAR col.
*    col = cols->get_column( 'STRATEGY'  ).
*    col->set_short_text( 'Strategy'(001) ).
    CLEAR col.
    col = cols->get_column( 'STATE' ).
    col->set_short_text( 'Status'(002) ).
    CLEAR col.
    col = cols->get_column( 'MSG' ).
    col->set_short_text( 'Message'(003) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM1' ).
    col->set_short_text( 'Custom1'(004) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM2' ).
    col->set_short_text( 'Custom2'(005) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM3' ).
    col->set_short_text( 'Custom3'(006) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM4' ).
    col->set_short_text( 'Custom4'(007) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM5' ).
    col->set_short_text( 'Custom5'(008) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM6' ).
    col->set_short_text( 'Custom6'(009) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM7' ).
    col->set_short_text( 'Custom7'(010) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM8' ).
    col->set_short_text( 'Custom8'(011) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM9' ).
    col->set_short_text( 'Custom9'(012) ).
    CLEAR col.
    col = cols->get_column( 'CUSTOM10' ).
    col->set_short_text( 'Custom10'(013) ).
    alv->set_screen_status(
      EXPORTING
        report        =  report
        pfstatus      =  pfstatus
        set_functions = alv->c_functions_all
    ).
    IF me->simulation = abap_false.
      DATA(functions) = alv->get_functions( ).
      TRY.
          functions->set_function(
            EXPORTING
              name    = 'EXEC'
              boolean = abap_false
          ).
        CATCH cx_salv_not_found.
          MESSAGE s005(zodin) DISPLAY LIKE 'W'.
      ENDTRY.
    ENDIF.
    DATA(layout) = me->alv->get_layout( ).
    DATA key TYPE salv_s_layout_key.
    key-report = report.
    layout->set_key( key ).
    layout->set_save_restriction(
        value = if_salv_c_layout=>restrict_none
    ).
    APPEND 'STATE' TO active_components.
    APPEND 'MSG' TO active_components.
    APPEND 'BELNR' TO active_components.
    APPEND 'GJAHR' TO active_components.
    APPEND 'BUKRS' TO active_components.
    DATA columns_s LIKE LINE OF me->internal.
    DATA strdesc TYPE REF TO cl_abap_structdescr.
    strdesc ?= cl_abap_structdescr=>describe_by_data( columns_s ).
    DATA(all_components) = strdesc->get_components( ).
    FIELD-SYMBOLS <fs> TYPE any.
    DATA supplied TYPE abap_bool.
    LOOP AT all_components INTO DATA(component).
      CLEAR supplied.
      READ TABLE active_components WITH KEY table_line = component-name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      LOOP AT me->internal INTO internal_s .
        ASSIGN COMPONENT component-name OF STRUCTURE internal_s TO <fs>.
        IF <fs> IS NOT INITIAL.
          supplied = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF supplied = abap_false.
        CLEAR col.
        col = cols->get_column( CONV #( component-name ) ).
        col->set_visible( abap_false ).
      ENDIF.
    ENDLOOP.
    DATA(event) = alv->get_event( ).
    SET HANDLER me->on_user_command  FOR event.
    SET HANDLER me->on_double_click  FOR event.
    cols->set_optimize( abap_true ).
    alv->display( ).
  ENDMETHOD.


  METHOD has_date_type.
    DATA(desc) = cl_abap_typedescr=>describe_by_data( external ).
    IF desc->type_kind = desc->typekind_date.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD has_numeric_type.
    DATA(desc) = cl_abap_typedescr=>describe_by_data( input ).
    CASE desc->type_kind.
      WHEN desc->typekind_decfloat
        OR desc->typekind_decfloat16
        OR desc->typekind_decfloat34
        OR desc->typekind_float
        OR desc->typekind_int
        OR desc->typekind_int1
        OR desc->typekind_int2
        OR desc->typekind_num
        OR desc->typekind_packed
        OR desc->typekind_numeric.
        result = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD load.
    DATA path TYPE string.
    DATA string_t TYPE TABLE OF string.
    DATA string   LIKE LINE OF string_t.
    DATA flag_server TYPE abap_bool.
    IF me->path_local IS NOT INITIAL.
      path = me->path_local.
    ELSEIF me->path_server IS NOT INITIAL.
      path = me->path_server.
      flag_server = abap_true.
    ENDIF.
    IF path IS NOT INITIAL.
      DATA(pos) = strlen( path ).
      pos = pos - 4.
      IF path+pos(4) CP '.xls'.
        input_mode = abap2xlsx.
      ELSEIF path+pos(4) CP '.csv'.
        input_mode = csv.
      ELSE.
        pos = pos - 1.
        IF path+pos(5) CP '.xlsx' OR path+pos(5) CP '.xlsm'.
          input_mode = abap2xlsx.
        ELSE.
          RAISE EXCEPTION TYPE zcx_odin EXPORTING text = 'File type not supported. Allowed extensions: .csv, .xls, .xlsx, .xlsm'.
        ENDIF.
      ENDIF.
      DATA xlsx_reader TYPE REF TO object.
      DATA ws TYPE REF TO object.
      DATA excel TYPE REF TO object.
      IF input_mode = abap2xlsx.
        TRY.
            "dynamic calls of abap2xlsx classes, because if component abap2xlsx is missing the project should nevertheless compile
            CREATE OBJECT xlsx_reader TYPE ('ZCL_EXCEL_READER_2007').
          CATCH cx_root.
            RAISE EXCEPTION TYPE zcx_odin EXPORTING text = 'For an Excel upload abap2xlsx must be installed https://github.com/sapmentors/abap2xlsx'.
        ENDTRY.
        TRY.
            CALL METHOD xlsx_reader->('ZIF_EXCEL_READER~LOAD_FILE') EXPORTING  i_filename = path i_from_applserver = flag_server RECEIVING r_excel = excel.
            CALL METHOD excel->('GET_ACTIVE_WORKSHEET') RECEIVING eo_worksheet = ws.
            CALL METHOD ws->('GET_TABLE') EXPORTING iv_skipped_rows = skip_rows IMPORTING et_table = external.
          CATCH zcx_excel INTO DATA(cx_excel).
            RAISE EXCEPTION TYPE zcx_odin EXPORTING previous = cx_excel text = cx_excel->if_message~get_text( ) .
        ENDTRY.
      ELSEIF input_mode = me->csv.
        cl_rsda_csv_converter=>create(
          EXPORTING
            i_delimiter      = me->csv_delimiter
            i_separator      = me->csv_separator
          RECEIVING
            r_r_conv         = DATA(converter)
        ).
        IF flag_server = abap_false.
          cl_gui_frontend_services=>gui_upload(
            EXPORTING
              filename                = path             " Name der Datei
*            filetype                = 'ASC'            " Dateityp (Ascii, Binär)
*            has_field_separator     = space            " Spalten durch TAB getrennt bei ASCII Upload
*            header_length           = 0                " Länge des Headers bei Binärdaten
*            read_by_line            = 'X'              " Die Datei wird zeilenweise in die interne Tabelle geschriebe
*            dat_mode                = space            " Zahl- und Datumsfelder werden im 'DAT' Format des ws_downloa
*            codepage                =                  " Zeichenrepräsentation für Ausgabe
*            ignore_cerr             = abap_true        " Gibt an, ob Fehler bei der Zeichensatzkonvertierung ignorier
*            replacement             = '#'              " Ersatzzeichen für nicht-konvertierbare Zeichen.
*            virus_scan_profile      =                  " Viren-Scan-Profil
*          IMPORTING
*            filelength              =                  " Dateilänge
*            header                  =                  " Header der Datei bei binärem Upload
            CHANGING
              data_tab                = string_t                 " Übergabetabelle für Datei-Inhalt
*            isscanperformed         = space            " File ist bereits gescannt
          EXCEPTIONS
            file_open_error         = 1                " Datei nicht vorhanden, kann nicht geöffnet werde
            file_read_error         = 2                " Fehler beim Lesen der Datei
            no_batch                = 3                " Frontend-Funktion im Batch nicht ausführbar.
            gui_refuse_filetransfer = 4                " Falsches Frontend oder Fehler im Frontend
            invalid_type            = 5                " Falscher Parameter FILETYPE
            no_authority            = 6                " Keine Berechtigung für Upload
            unknown_error           = 7                " Unbekannter Fehler
            bad_data_format         = 8                " Daten in der Datei können nicht interpretiert werden.
            header_not_allowed      = 9                " Header ist nicht zulässig.
            separator_not_allowed   = 10               " Separator ist nicht zulässig.
            header_too_long         = 11               " Die Headerinformation ist zur Zeit auf maximal 1023 Bytes be
            unknown_dp_error        = 12               " Fehler beim Aufruf des Dataprovider
            access_denied           = 13               " Zugriff auf Datei nicht erlaubt.
            dp_out_of_memory        = 14               " Nicht genug Speicher im Dataprovider
            disk_full               = 15               " Speichermedium ist voll.
            dp_timeout              = 16               " Timeout des Dataproviders
            not_supported_by_gui    = 17               " Nicht unterstützt von GUI
            error_no_gui            = 18               " GUI nicht verfügbar
            OTHERS                  = 19
          ).
          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.
                DATA(text) = `file_open_error`.
              WHEN 2.
                text = `file_read_error`.
              WHEN 3.
                text = `no_batch`.
              WHEN 4.
                text = `gui_refuse_filetransfer`.
              WHEN 5.
                text = `invalid_type`.
              WHEN 6.
                text = `no_authority`.
              WHEN 7.
                text = `unknown_error`.
              WHEN 8.
                text = `bad_data_format`.
              WHEN 9.
                text = `header_not_allowed`.
              WHEN 10.
                text = `separator_not_allowed`.
              WHEN 11.
                text = `header_too_long`.
              WHEN 12.
                text = `unknown_dp_error`.
              WHEN 13.
                text = `access_denied`.
              WHEN 14.
                text = `dp_out_of_memory`.
              WHEN 15.
                text = `disk_full`.
              WHEN 16.
                text = `dp_timeout`.
              WHEN 17.
                text = `not_supported_by_gui`.
              WHEN 18.
                text = `error_no_gui`.
              WHEN 19.
                text = `other error`.
            ENDCASE.
            RAISE EXCEPTION TYPE zcx_odin EXPORTING text = `Upload error: ` && text.
          ENDIF.
        ELSE.
          OPEN DATASET path FOR INPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE zcx_odin EXPORTING text = |Could not open file { path }|.
          ENDIF.
          DATA subrc LIKE sy-subrc.
          DATA byom(4) TYPE x.
          FIELD-SYMBOLS: <fs> TYPE c.
          WHILE subrc = 0.
            READ DATASET path INTO string.
            subrc = sy-subrc.
            REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN string WITH ''.
            REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN string WITH ''.
            REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1) IN string WITH ''.
            byom = cl_abap_char_utilities=>byte_order_mark_little.
            ASSIGN byom TO <fs> CASTING.
            REPLACE ALL OCCURRENCES OF <fs>(1) IN string WITH ''.
            byom = cl_abap_char_utilities=>byte_order_mark_big.
            ASSIGN byom TO <fs> CASTING.
            REPLACE ALL OCCURRENCES OF <fs>(1) IN string WITH ''.
            byom = cl_abap_char_utilities=>byte_order_mark_utf8.
            ASSIGN byom TO <fs> CASTING.
            REPLACE ALL OCCURRENCES OF <fs>(1) IN string WITH ''.
            APPEND string TO string_t.
          ENDWHILE.
          CLOSE DATASET path.
        ENDIF.
        DATA external_s LIKE LINE OF external.
        DATA csv_input TYPE zodin_input_csv.
        LOOP AT string_t INTO string.
          CHECK sy-tabix GT skip_rows.
          CLEAR csv_input.
          converter->csv_to_structure(
            EXPORTING
              i_data   = string                " I_DATA
            IMPORTING
              e_s_data = csv_input                 " E_S_DATA
          ).
          IF csv_input IS NOT INITIAL.
            CLEAR external_s.
            MOVE-CORRESPONDING csv_input TO external_s.
            APPEND external_s TO external.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSEIF me->hex_file IS NOT INITIAL.
      me->input_mode = abap2xlsx. "Only abap2xlsx supported for xstring for now
      TRY.
          "abap2xlsx calls dynamically, because if component abap2xlsx is missing the project should nevertheless compile
          CREATE OBJECT xlsx_reader TYPE ('ZCL_EXCEL_READER_2007').
        CATCH cx_root.
          RAISE EXCEPTION TYPE zcx_odin EXPORTING text = 'For an Excel upload abap2xlsx must be installed https://github.com/sapmentors/abap2xlsx'.
      ENDTRY.
      TRY.
          CALL METHOD xlsx_reader->('ZIF_EXCEL_READER~LOAD') EXPORTING i_excel2007 = me->hex_file RECEIVING r_excel = excel.
          CALL METHOD excel->('GET_ACTIVE_WORKSHEET') RECEIVING eo_worksheet = ws.
          CALL METHOD ws->('GET_TABLE') IMPORTING et_table = external.
        CATCH zcx_excel INTO cx_excel.
          RAISE EXCEPTION TYPE zcx_odin EXPORTING previous = cx_excel text = cx_excel->if_message~get_text( ) .
      ENDTRY.
    ELSE.
      RAISE EXCEPTION TYPE zcx_odin EXPORTING text = 'File path or hex string must be supplied'.
    ENDIF.
    LOOP AT external INTO external_s.
      IF external_s IS INITIAL.
        DELETE external.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD map.
    zcl_odin_posting=>to_key_value(
      EXPORTING
        t_external     = me->external
      IMPORTING
        t_key_value = DATA(t_key_value)
    ).
    READ TABLE t_key_value INTO DATA(line) WITH KEY key = 'STRATEGY'.
    me->strategy = line-value.
    IF me->strategy IS NOT INITIAL.
      DATA mapping_badi TYPE REF TO zodin_mapping.
      GET BADI mapping_badi
        FILTERS
          strategy = me->strategy.
      CALL BADI mapping_badi->before
        CHANGING
          external = me->external.
      CLEAR t_key_value.
      zcl_odin_posting=>to_key_value(
        EXPORTING
          t_external     = me->external
        IMPORTING
          t_key_value = t_key_value
      ).
    ENDIF.
    FIELD-SYMBOLS <fs> TYPE any.
    DATA s_internal TYPE zodin_internal.
    DATA state LIKE s_internal-state VALUE me->state-unknown.
    DATA msg LIKE s_internal-msg.
    DATA tmp_float TYPE f.
    DATA component TYPE string.
    LOOP AT t_key_value INTO line.
      CLEAR component.
      IF line-key IS NOT INITIAL.
        SELECT SINGLE field_name
          FROM zodin_alias
          INTO component
          WHERE alias_name = line-key
            AND strategy = me->strategy.
        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE field_name
            FROM zodin_alias
            INTO component
            WHERE alias_name = line-key
              AND strategy = ''.
          IF sy-subrc IS NOT INITIAL.
            component = line-key.
          ENDIF.
        ENDIF.
        ASSIGN COMPONENT component OF STRUCTURE s_internal TO <fs>.
        IF sy-subrc = 0.
          IF zcl_odin_posting=>has_numeric_type( <fs> ) = abap_true.
            TRY.
                IF me->input_mode = me->abap2xlsx.
                  <fs> = line-value.
                ELSEIF me->input_mode = me->csv.
                  REPLACE ALL OCCURRENCES OF me->csv_thousands_separator IN line-value WITH ''.
                  IF me->csv_decimal_separator NE '.'.
                    REPLACE ALL OCCURRENCES OF me->csv_decimal_separator IN line-value WITH '.'.
                  ENDIF.
                  <fs> = line-value.
                ENDIF.
              CATCH cx_root INTO DATA(cx_root).
                "Hack for exponential notation with conversion to float first
                TRY.
                    CLEAR tmp_float.
                    tmp_float = line-value.
                    <fs> = tmp_float.
                  CATCH cx_root INTO cx_root.
                    state = me->state-error.
                    msg = |Could not convert { line-value } into number: { cx_root->if_message~get_text( ) }|.
                ENDTRY.
            ENDTRY.
          ELSEIF zcl_odin_posting=>has_date_type( <fs> ) = abap_true.
            IF input_mode = me->abap2xlsx.
              TRY.
                  IF line-value IS NOT INITIAL.
                    CALL METHOD zcl_excel_common=>('EXCEL_STRING_TO_DATE') EXPORTING ip_value = line-value RECEIVING ep_value = <fs>.
                  ENDIF.
                CATCH zcx_excel INTO DATA(cx_excel).
                  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                    EXPORTING
                      date_external            = line-value
*                     ACCEPT_INITIAL_DATE      =
                    IMPORTING
                      date_internal            = <fs>
                    EXCEPTIONS
                      date_external_is_invalid = 1
                      OTHERS                   = 2.
                  IF sy-subrc <> 0.
                    state = me->state-error.
                    msg = |Could not convert { line-value } into date: { cx_excel->if_message~get_text( ) }|.
                  ENDIF.
              ENDTRY.
            ELSE.
              CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                EXPORTING
                  date_external            = line-value
*                 ACCEPT_INITIAL_DATE      =
                IMPORTING
                  date_internal            = <fs>
                EXCEPTIONS
                  date_external_is_invalid = 1
                  OTHERS                   = 2.
              IF sy-subrc <> 0.
                state = me->state-error.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO msg.
              ENDIF.
            ENDIF.
          ELSE.
            <fs> = line-value.
            IF component = 'BUKRS' OR component = 'BWART' OR component = 'LIFNR' OR component = 'KUNNR' OR component = 'HKONT' OR component = 'AUFNR'
              OR component = 'KOSTL' OR component = 'VBEL2' OR component = 'EBELN' OR component = 'KUNNR_GL' OR component = 'LIFNR_GL'.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs>
                IMPORTING
                  output = <fs>.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      AT END OF line.
        s_internal-state = state.
        s_internal-msg = msg.
        APPEND s_internal TO me->internal.
        CLEAR: s_internal, msg.
        state = me->state-unknown.
      ENDAT.
    ENDLOOP.
    SORT me->internal BY counter ASCENDING.
    LOOP AT me->internal INTO s_internal
      WHERE counter IS INITIAL.
      DELETE me->internal.
    ENDLOOP.
    IF mapping_badi IS BOUND.
      CALL BADI mapping_badi->after
        EXPORTING
          external  = external
          key_value = t_key_value
        CHANGING
          internal  = me->internal.
    ENDIF.
    DATA error_counters LIKE TABLE OF s_internal-counter.
    LOOP AT me->internal INTO s_internal
      WHERE state = me->state-error.
      APPEND s_internal-counter TO error_counters.
    ENDLOOP.
    IF sy-subrc = 0.
      SORT error_counters ASCENDING.
      DELETE ADJACENT DUPLICATES FROM error_counters.
      CLEAR s_internal.
      s_internal-state = me->state-error.
      LOOP AT error_counters INTO DATA(error_counter).
        MODIFY me->internal FROM s_internal TRANSPORTING state WHERE counter = error_counter.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD on_double_click.
    READ TABLE me->internal INDEX row INTO DATA(s_internal).
    IF s_internal-belnr IS NOT INITIAL.
      SET PARAMETER ID 'BLN' FIELD s_internal-belnr.
      SET PARAMETER ID 'BUK' FIELD s_internal-bukrs.
      SET PARAMETER ID 'GJR' FIELD s_internal-budat(4).
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDMETHOD.


  METHOD on_user_command.
    IF e_salv_function EQ 'EXEC'.
      me->set_simulation( abap_false ).
      TRY.
          me->post(  ).
        CATCH zcx_ODIN INTO DATA(cx_odin).
          CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
              titel = 'Error'
              txt1  = cx_odin->get_text( )
              txt2  = ''.
      ENDTRY.
      DATA(cols) = me->alv->get_columns( ).
      cols->set_optimize( abap_true ).
      DATA(functions) = alv->get_functions( ).
      TRY.
          functions->set_function(
            EXPORTING
              name    = 'EXEC'
              boolean = abap_false
          ).
        CATCH cx_salv_not_found.
          MESSAGE s005(zodin) DISPLAY LIKE 'W'.
      ENDTRY.
      me->alv->refresh( ).
    ENDIF.
  ENDMETHOD.


  METHOD post.
    IF me->posted = abap_true.
      RAISE EXCEPTION TYPE zcx_odin EXPORTING text = 'Posting allowed only once'.
    ENDIF.
    IF me->simulation = abap_false.
      me->posted = abap_true.
    ENDIF.
    READ TABLE me->internal WITH KEY state = me->state-error TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      IF me->simulation = abap_false.
        RAISE EXCEPTION TYPE zcx_odin EXPORTING text = 'No posting/simulation is made. Please check for errors and re-read data'.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    DATA document LIKE me->internal.
    DATA posting_badi TYPE REF TO zodin_posting.
    DATA commited TYPE abap_bool.
    DATA commited_all TYPE abap_bool VALUE abap_true.
    DATA err_msg TYPE string.
    GET BADI posting_badi
      FILTERS
        strategy = me->strategy.
    LOOP AT me->internal INTO DATA(s_internal).
      APPEND s_internal TO document.
      AT END OF counter.
        CLEAR: commited, err_msg.
        DATA(counter) = s_internal-counter.

        TRY.
            me->post_document( EXPORTING document = document
                               IMPORTING bukrs = DATA(bukrs) belnr = DATA(belnr) gjahr = DATA(gjahr) obj_key = DATA(obj_key) ).
          CATCH zcx_odin INTO DATA(cx_odin).
            err_msg = cx_odin->get_text( ).
        ENDTRY.

        IF posting_badi IS BOUND AND err_msg IS INITIAL.
          TRY.
              CALL BADI posting_badi->after_pre_commit
                EXPORTING
                  obj_key     = obj_key
                  simulation  = me->simulation
                  path_local  = me->path_local
                  hex_file    = me->hex_file
                  path_server = me->path_server
                CHANGING
                  document    = document.
            CATCH zcx_odin INTO cx_odin.
              err_msg = cx_odin->get_text( ).
          ENDTRY.
        ENDIF.

        IF me->simulation IS INITIAL AND err_msg IS INITIAL.
          COMMIT WORK AND WAIT.
          IF sy-subrc NE 0.
            err_msg = 'Update failed'.
          ELSE.
            commited = abap_true.
          ENDIF.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

        IF commited = abap_false.
          commited_all = abap_false.
        ENDIF.

        IF posting_badi IS BOUND AND me->simulation IS INITIAL AND err_msg IS INITIAL.
          TRY.
              CALL BADI posting_badi->after_post_commit
                EXPORTING
                  obj_key     = obj_key
                  path_local  = me->path_local
                  hex_file    = me->hex_file
                  path_server = me->path_server
                CHANGING
                  document    = document.
            CATCH zcx_odin INTO cx_odin.
              err_msg = cx_odin->get_text( ).
          ENDTRY.
        ENDIF.

        LOOP AT me->internal INTO DATA(s_internal2) WHERE counter = counter.
          IF me->simulation = abap_false.
            s_internal2-belnr = belnr.
            s_internal2-bukrs = bukrs.
            s_internal2-gjahr = gjahr.
            IF err_msg IS INITIAL.
              s_internal2-state = me->state-ok.
            ELSE.
              s_internal2-state = me->state-error.
              s_internal2-msg = err_msg.
            ENDIF.
          ELSE.
            IF err_msg IS INITIAL.
              s_internal2-state = me->state-ready.
            ELSE.
              s_internal2-state = me->state-error.
              s_internal2-msg = err_msg.
            ENDIF.
          ENDIF.
          MODIFY me->internal FROM s_internal2.
        ENDLOOP.

        CLEAR document.
      ENDAT.
    ENDLOOP.

    IF commited_all = abap_true AND me->path_server_backup IS NOT INITIAL AND me->path_server IS NOT INITIAL. "Move file
      DATA file_content TYPE xstring.
      TRY.
          OPEN DATASET me->path_server FOR INPUT IN BINARY MODE.
          IF sy-subrc = 0.
            READ DATASET me->path_server INTO file_content.
            CLOSE DATASET me->path_server.
            DELETE DATASET me->path_server_backup.
            OPEN DATASET me->path_server_backup FOR OUTPUT IN BINARY MODE.
            IF sy-subrc = 0.
              TRANSFER file_content TO me->path_server_backup.
              CLOSE DATASET me->path_server_backup.
              DELETE DATASET me->path_server.
            ENDIF.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD post_document.
    DATA: header         TYPE bapiache09,
          customercpd    TYPE  bapiacpa09,
          glaccount      TYPE TABLE OF bapiacgl09,
          s_glaccount    TYPE bapiacgl09,
          vendor         TYPE TABLE OF bapiacap09,
          s_vendor       TYPE bapiacap09,
          customer       TYPE TABLE OF bapiacar09,
          s_customer     LIKE LINE OF customer,
          amount         TYPE TABLE OF bapiaccr09,
          s_amount       TYPE bapiaccr09,
          extension2     TYPE TABLE OF bapiparex,
          s_extension2   LIKE LINE OF extension2,
          odin_extension TYPE zodin_extension,
          wht            TYPE TABLE OF bapiacwt09,
          bapiret        TYPE TABLE OF bapiret2,
          s_bapiret      TYPE bapiret2,
          itemno         TYPE posnr_acc,
          bapitax        TYPE TABLE OF bapiactx09,
          s_bapitax      TYPE bapiactx09,
          wrbtr          TYPE bseg-wrbtr,
          t_mwdat        TYPE TABLE OF rtax1u15,
          tax            TYPE bset-fwste,
          itemno_tax     LIKE itemno VALUE 9999999999,
          segments       TYPE i,
          msg            TYPE string.

    FIELD-SYMBOLS <fs> TYPE any.

    LOOP AT document INTO DATA(s_document).

      CLEAR: s_glaccount, s_amount, s_vendor, wrbtr, t_mwdat, segments, tax, s_extension2, odin_extension.
      ADD 1 TO itemno.

      IF header IS INITIAL.
        header-pstng_date = s_document-budat.
        header-doc_date = s_document-bldat.
        header-doc_type = s_document-blart.
        header-comp_code = s_document-bukrs.
        header-ref_doc_no = s_document-xblnr.
        header-header_txt = s_document-bktxt.
        header-username = sy-uname.
        header-fisc_year = s_document-gjahr.
        header-fis_period = s_document-monat.
        IF s_document-doc_status IS NOT INITIAL.
          ASSIGN COMPONENT 'DOC_STATUS' OF STRUCTURE header TO <fs>. "Using field symbol for downwards compatibility (component doc_status might lack in older releases)
          IF sy-subrc = 0.
            <fs> = s_document-doc_status.
*            IF <fs> CA '23AB'.
*              header-bus_act = 'RFBV'.
*            ENDIF.
          ENDIF.
        ENDIF.
        header-bus_act = s_document-glvor.
      ENDIF.

      MOVE-CORRESPONDING s_document TO odin_extension.
      IF odin_extension IS NOT INITIAL.
        odin_extension-posnr = itemno.
        s_extension2-structure = me->odin_extension_structure.
        s_extension2-valuepart1 = odin_extension.
        APPEND s_extension2 TO extension2.
      ENDIF.

      IF s_document-wrbtr_h IS NOT INITIAL.
        wrbtr = s_document-wrbtr_h * -1.
      ELSEIF s_document-wrbtr_s IS NOT INITIAL.
        wrbtr = s_document-wrbtr_s.
      ENDIF.

      IF s_document-hkont IS NOT INITIAL.
        ADD 1 TO segments.
      ENDIF.
      IF s_document-lifnr IS NOT INITIAL.
        ADD 1 TO segments.
      ENDIF.
      IF s_document-kunnr IS NOT INITIAL.
        ADD 1 TO segments.
      ENDIF.
      IF segments NE 1.
        RAISE EXCEPTION TYPE zcx_odin EXPORTING text = 'Please specify exactly one of the following fields: lifnr, kunnr, hkont'.
      ENDIF.

      IF s_document-hkont IS NOT INITIAL.
        s_glaccount-itemno_acc = itemno.
        s_glaccount-gl_account = s_document-hkont.
        s_glaccount-orderid = s_document-aufnr.
        s_glaccount-item_text = s_document-sgtxt.
        s_glaccount-tax_code = s_document-mwskz.
        s_glaccount-alloc_nmbr = s_document-budat.
        s_glaccount-ref_key_1 = s_document-xref1.
        s_glaccount-ref_key_2 = s_document-xref2.
        s_glaccount-ref_key_3 = s_document-xref3.
        s_glaccount-cs_trans_t = s_document-bewar.
        s_glaccount-costcenter = s_document-kostl.
        s_glaccount-wbs_element = s_document-projk.
        s_glaccount-sales_ord = s_document-vbel2.
        s_glaccount-s_ord_item = s_document-posn2.
        s_glaccount-co_busproc = s_document-prznr.
        s_glaccount-trade_id = s_document-vbund.
        s_glaccount-po_number = s_document-ebeln.
        s_glaccount-po_item = s_document-ebelp.
        s_glaccount-quantity = s_document-menge.
        s_glaccount-base_uom = s_document-meins.
        s_glaccount-alloc_nmbr = s_document-zuonr.
        s_glaccount-customer = s_document-kunnr_gl.
        s_glaccount-vendor_no = s_document-lifnr_gl.
        APPEND s_glaccount TO glaccount.

        IF s_document-mwskz IS NOT INITIAL.
          CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
            EXPORTING
              i_bukrs                 = s_document-bukrs
              i_mwskz                 = s_document-mwskz
*             I_TXJCD                 = ' '
              i_waers                 = s_document-waers
              i_wrbtr                 = wrbtr
*             I_ZBD1P                 = 0
*             I_PRSDT                 =
*             I_PROTOKOLL             =
*             I_TAXPS                 =
*             I_ACCNT_EXT             =
*             I_ACCDATA               =
*             IS_ENHANCEMENT          =
*             I_PRICING_REFRESH_TX    = ' '
            IMPORTING
*             E_FWNAV                 =
*             E_FWNVV                 =
              e_fwste                 = tax
*             E_FWAST                 =
            TABLES
              t_mwdat                 = t_mwdat
            EXCEPTIONS
              bukrs_not_found         = 1
              country_not_found       = 2
              mwskz_not_defined       = 3
              mwskz_not_valid         = 4
              account_not_found       = 5
              different_discount_base = 6
              different_tax_base      = 7
              txjcd_not_valid         = 8
              not_found               = 9
              ktosl_not_found         = 10
              kalsm_not_found         = 11
              parameter_error         = 12
              knumh_not_found         = 13
              kschl_not_found         = 14
              unknown_error           = 15
              OTHERS                  = 16.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO msg.
            RAISE EXCEPTION TYPE zcx_odin EXPORTING text = msg.
          ENDIF.
        ENDIF.

        s_amount-itemno_acc = itemno.
        s_amount-currency = s_document-waers.
        s_amount-amt_doccur = wrbtr - tax.
        s_amount-exch_rate = s_document-kursf.
        s_amount-exch_rate_v = s_document-kursf_m.
        APPEND s_amount TO amount.

        DATA modified TYPE abap_bool.
        LOOP AT t_mwdat INTO DATA(s_mwdat).
          CLEAR: modified.
          LOOP AT bapitax INTO s_bapitax WHERE tax_code = s_document-mwskz AND acct_key = s_mwdat-ktosl.
            CLEAR: s_amount.
            READ TABLE amount WITH KEY itemno_acc = s_bapitax-itemno_acc INTO s_amount.
            "To be on the safe side check equality of signs before tax position is added to existing position, this might not be necessary because we     have already compared acct_key
            IF sy-subrc = 0 AND ( ( s_amount-amt_doccur GE 0 AND s_mwdat-wmwst GE 0 ) OR ( s_amount-amt_doccur LT 0 AND s_mwdat-wmwst LT 0 ) ).
              s_amount-amt_base = s_amount-amt_base + s_mwdat-kawrt.
              s_amount-amt_doccur = s_amount-amt_doccur + s_mwdat-wmwst.
              MODIFY amount FROM s_amount INDEX sy-tabix.
              modified = abap_true.
            ENDIF.
          ENDLOOP.
          IF modified = abap_false. "New tax position
            CLEAR: s_amount, s_bapitax.
            s_bapitax-itemno_acc = itemno_tax.
            s_bapitax-tax_code = s_document-mwskz.
            s_bapitax-gl_account = s_mwdat-hkont.
            s_bapitax-acct_key = s_mwdat-ktosl.
            APPEND s_bapitax TO bapitax.
            s_amount-itemno_acc = itemno_tax.
            s_amount-amt_base = s_mwdat-kawrt.
            s_amount-amt_doccur = s_mwdat-wmwst.
            s_amount-currency = s_document-waers.
            s_amount-exch_rate = s_document-kursf.
            s_amount-exch_rate_v = s_document-kursf_m.
            APPEND s_amount TO amount.
            SUBTRACT 1 FROM itemno_tax.
          ENDIF.
        ENDLOOP.

      ENDIF.

      IF s_document-lifnr IS NOT INITIAL.
        s_vendor-itemno_acc = itemno.
        s_vendor-vendor_no = s_document-lifnr.
        s_vendor-item_text = s_document-sgtxt.
        s_vendor-pmnttrms = s_document-zterm.
        s_vendor-pmnt_block = s_document-zlspr.
        s_vendor-ref_key_1 = s_document-xref1.
        s_vendor-ref_key_2 = s_document-xref2.
        s_vendor-ref_key_3 = s_document-xref3.
        s_vendor-alloc_nmbr = s_document-zuonr.
        s_vendor-bline_date = s_document-zfbdt.
        s_vendor-w_tax_code = s_document-qsskz.
        s_vendor-scbank_ind = s_document-lzbkz.
        s_vendor-supcountry = s_document-landl.
        s_vendor-sp_gl_ind = s_document-umskz.
        s_vendor-partner_bk = s_document-bvtyp.
        s_vendor-tax_code = s_document-mwskz.
        APPEND s_vendor TO vendor.
        s_amount-itemno_acc = itemno.
        s_amount-currency = s_document-waers.
        s_amount-amt_doccur = wrbtr.
        s_amount-exch_rate = s_document-kursf.
        s_amount-exch_rate_v = s_document-kursf_m.
        APPEND s_amount TO amount.
      ENDIF.

      IF s_document-kunnr IS NOT INITIAL.
        s_customer-itemno_acc = itemno.
        s_customer-customer = s_document-kunnr.
        s_customer-item_text = s_document-sgtxt.
        s_customer-pmnttrms = s_document-zterm.
        s_customer-pmnt_block = s_document-zlspr.
        s_customer-ref_key_1 = s_document-xref1.
        s_customer-ref_key_2 = s_document-xref2.
        s_customer-ref_key_3 = s_document-xref3.
        s_customer-alloc_nmbr = s_document-zuonr.
        s_customer-bline_date = s_document-zfbdt.
        s_customer-dunn_block = s_document-mansp.
        s_customer-scbank_ind = s_document-lzbkz.
        s_customer-supcountry = s_document-landl.
        s_customer-sp_gl_ind = s_document-umskz.
        s_customer-partner_bk = s_document-bvtyp.
        s_customer-tax_code = s_document-mwskz.
        APPEND s_customer TO customer.
        s_amount-itemno_acc = itemno.
        s_amount-currency = s_document-waers.
        s_amount-amt_doccur = wrbtr.
        s_amount-exch_rate = s_document-kursf.
        s_amount-exch_rate_v = s_document-kursf_m.
        APPEND s_amount TO amount.
      ENDIF.

    ENDLOOP.

    IF me->strategy IS NOT INITIAL.
      DATA posting_badi TYPE REF TO zodin_posting.
      GET BADI posting_badi
        FILTERS
          strategy = me->strategy.
      CALL BADI posting_badi->before
        EXPORTING
          document          = document
        CHANGING
          documentheader    = header
          customercpd       = customercpd
          accountgl         = glaccount
          accountreceivable = customer
          accountpayable    = vendor
          accounttax        = bapitax
          currencyamount    = amount
          extension2        = extension2
          accountwt         = wht.
    ENDIF.

    IF me->simulation EQ abap_true.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = header
          customercpd       = customercpd
        TABLES
          accountgl         = glaccount
          accountreceivable = customer
          accountpayable    = vendor
          accounttax        = bapitax
          currencyamount    = amount
          extension2        = extension2
          accountwt         = wht
          return            = bapiret.
    ELSE.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = header
          customercpd       = customercpd
        IMPORTING
          obj_key           = obj_key
        TABLES
          accountgl         = glaccount
          accountreceivable = customer
          accountpayable    = vendor
          accounttax        = bapitax
          currencyamount    = amount
          extension2        = extension2
          accountwt         = wht
          return            = bapiret.
    ENDIF.
    LOOP AT bapiret INTO s_bapiret WHERE type = 'E' OR type = 'A'.
      MESSAGE ID s_bapiret-id TYPE s_bapiret-type NUMBER s_bapiret-number WITH s_bapiret-message_v1 s_bapiret-message_v2 s_bapiret-message_v3 s_bapiret-message_v4 INTO msg.
      IF sy-tabix = 2.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      IF me->simulation NE abap_true.
        belnr = obj_key(10).
        bukrs = obj_key+10(4).
        gjahr = obj_key+14(4).
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_odin EXPORTING text = msg.
    ENDIF.
  ENDMETHOD.


  METHOD set_simulation.
    me->simulation = simulation.
  ENDMETHOD.


  METHOD to_key_value.
    DATA: s_external      LIKE LINE OF t_external,
          ls_fieldnames LIKE s_external,
          l_index       TYPE sy-index,
          ls_key_value  LIKE LINE OF t_key_value,
          l_line        TYPE i.
    FIELD-SYMBOLS: <fieldname> TYPE any, <value> TYPE any.

    "Get data format
    READ TABLE t_external INTO ls_fieldnames INDEX 1.
    DELETE t_external INDEX 1.

    LOOP AT t_external INTO s_external.

      ADD 1 TO l_line.

      DO.
        CLEAR: ls_key_value.
        UNASSIGN: <fieldname>, <value>.
        l_index = sy-index.
        ASSIGN COMPONENT l_index OF STRUCTURE ls_fieldnames TO <fieldname>.
        IF sy-subrc IS NOT INITIAL.
          EXIT.
        ENDIF.
        IF <fieldname> IS NOT INITIAL.
          ASSIGN COMPONENT l_index OF STRUCTURE s_external TO <value>.
          ls_key_value-key = <fieldname>.
          ls_key_value-value = <value>.
          ls_key_value-line = l_line.
          APPEND ls_key_value TO t_key_value.
        ENDIF.
      ENDDO.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
