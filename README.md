# Open Document Interface (ODIN)

ODIN is a flexible, extensible, easy to use, multi-purpose import interface for *SAP FI documents*.

It is suitable for postings as they can be entered in the dialog transaction fb01, such as incoming and outgoing invoices or G/L account postings.

It accepts different input formats:

- Excel
- csv
- REST/JSON (planned)
- SOAP/XML (planned)

and has a flexible record structure.

It can be used as a dialog or batch application. In addition, e-mail input processing is planned.

## Key features

- Can be used in frontend or as background job
- Flexible dataset structure
- Suitable for mass processing
- Highly extensible with BAdI enhancements
- Various input interfaces

## Prerequisites

If Excel files are processed, [abap2xlsx](https://github.com/sapmentors/abap2xlsx) must be installed. For processing of csv files, abap2xlsx is not required and doesn't need to be installed.

ABAP releases older than 7.40 will not work. Release 7.40 might be ok, however it has not been tested. Release 7.50 and later should be fine. S/4 releases should work as well, however test is missing. Feedback at that point is very welcome.

## Available fields
Many common fields are available, see [wiki](https://github.com/71tech/ODIN/wiki/Available-fields) for details. If something is missing you can open an issue or make a [contribution](#contribution).

## Dataset structure

At the moment the interface can process Excel and csv files, further input options are planned.

For Excel and csv the following applies:

- The first row must contain the column names as under [Available fields](#_Available_fields) (alias identifiers are not yet available, but planned)
- The order of the columns is arbitrary
- From row two on, the user data follows, which must be arranged in the same way as in the first row
- One FI document line item corresponds to one row in the input data. Several rows are combined into one document via the COUNTER field
- There is no separate line for header fields from table bkpf, such as BUTXT, instead they are to be repeated redundantly in each row

### Example (csv)

COUNTER;STRATEGY;BUKRS;BUTXT;XBLNR;BLDAT;BUDAT;BLART;SGTXT;LIFNR;KUNNR;HKONT;WRBTR\_S;WRBTR\_H;WAERS;MWSKZ;CUSTOM1

 1;MY\_STRATEGY;81;Example for incoming invoice;INVOICE-001;26.03.2020;26.03.2020;KR;Text position;;;154200;119,00;;EUR;V2;Some custom value
 
 1;MY\_STRATEGY;81;Example for incoming invoice;INVOICE-001;26.03.2020;26.03.2020;KR;Text position;802236;;;;119,00;EUR;V2;Some custom value
 
 2;MY\_STRATEGY;81;Another example;INVOICE-002;27.03.2020;26.03.2020;KR;Text position;;;154200;;59,50;EUR;V2;Another custom value
 
 2;MY\_STRATEGY;81;Another example;INVOICE-002;27.03.2020;26.03.2020;KR;Text position;;;154200;;59,50;EUR;V2;Another custom value
 
 2;MY\_STRATEGY;81;Another example;INVOICE-002;27.03.2020;26.03.2020;KR;Text position;802236;;;119,00;;EUR;V2;Another custom value

### Example (Excel)

[Simple inputfile](https://github.com/71tech/ODIN/blob/294bfdc74bf3c868d08407ae47897aa92b1d5bdb/examples/ODIN_simple.xlsx).

## Import program

Use sa38 > ZODIN or Transaction ZODIN, see [wiki](https://github.com/71tech/ODIN/wiki/ZODIN).

## Server side processing of multiple files

Use sa38 > ZODIN\_MULTIFILE, see [wiki](https://github.com/71tech/ODIN/wiki/ZODIN_MULTIFILE).

## BAdI Reference

Enhancement Spot (se18): ZODIN\_EXTENSION. The filter value in your BAdI implementation must match the field value of STRATEGY in your input data. Find a list of available extensions below.

Note that ODIN uses FMs BAPI\_ACC\_DOCUMENT\_POST / BAPI\_ACC\_DOCUMENT\_CHECK for posting, hence SAP standard extension BADI\_ACC\_DOCUMENT is also available.

| **BAdI Definition / Interface** | **Method** | **Time** | **Recommended Use** | **Effect when Exception zcx\_odin is thrown** |
| --- | --- | --- | --- | --- |
| ZODIN\_MAPPING /ZODIN\_INTF\_MAPPING | BEFORE | Before Mapping; called one time per file on test run and live run, see [Batch mode](#_Batch_mode) | Manipulate external data before mapping, e.g. translate VAT in % to SAP tax code | Display error message, return to selection screen |
| ZODIN\_MAPPING /ZODIN\_INTF\_MAPPING | AFTER | After Mapping; called one time per file on test run and live run, see [Batch mode](#_Batch_mode) | Manipulate internal data after mapping has been done | Display error message, return to selection screen.If you still want to display the ALV list, don&#39;t use an exception, but set error state in erroneous line as suggested here: [Snippet: after mapping, error for ALV list](#_Snippet_after_mapping,) |
| ZODIN\_POSTING / ZODIN\_INTF\_POSTING | BEFORE | Before Call of Function BAPI\_ACC\_DOCUMENT\_POSTresp. BAPI\_ACC\_DOCUMENT\_POST; called one time per FI document on test run and live run, see [Batch mode](#_Batch_mode) | Check and/or manipulate field values of BAPI structures and tables | Show error message in ALV Grid table for corresponding line items |
| ZODIN\_POSTING / ZODIN\_INTF\_POSTING | AFTER\_PRE\_
 COMMIT | After Call of Function BAPI\_ACC\_DOCUMENT\_CHECK resp. BAPI\_ACC\_DOCUMENT\_POST and before potential database commit; called one time per FI document on test run and live run, see [Batch mode](#_Batch_mode) | Perform custom action that doesn&#39;t require the FI document written on DB, e.g. connect the input file via archive link | Show error message in ALV Grid table for corresponding line items. Rollback will be performed (in case of live run) |
| ZODIN\_POSTING / ZODIN\_INTF\_POSTING | AFTER\_POST\_
 COMMIT | After Call of Function BAPI\_ACC\_DOCUMENT\_POST and after database commit; called one time per FI document on live run only, see [Batch mode](#_Batch_mode) | Perform custom action that needs to happen after writing on DB is completed, i.e. SELECT from bkpf, bseg, acdoca etc. of new the document can be done | Show error message in ALV Grid table for corresponding line items. Note that booking of FI document has been done and cannot be rolled back |

### Example: before mapping

  METHOD zodin\_intf\_mapping~before.
     _&quot;Truncate the first 4 characters of the external invoice no. because_
     _&quot;it is too long for the XBLNR field,_
     _&quot;but still unique without the first 4 digits_
     FIELD-SYMBOLS \&lt;fs\&gt; TYPE any.
     READ TABLE external INTO DATA(input\_s) INDEX 1.
     DO.
       ASSIGN COMPONENT sy-index OF STRUCTURE input\_s TO \&lt;fs\&gt;.
       IF sy-subrc NE 0.
         EXIT.
       ENDIF.
       IF \&lt;fs\&gt; = &#39;XBLNR&#39;.
         DATA(component) = sy-index.
         EXIT.
       ENDIF.
     ENDDO.
     IF component IS NOT INITIAL.
       LOOP AT external INTO input\_s.
         IF sy-tabix = 1.
           CONTINUE.
         ENDIF.
         ASSIGN COMPONENT component OF STRUCTURE input\_s TO \&lt;fs\&gt;.
         TRY.
             \&lt;fs\&gt; = \&lt;fs\&gt;+4.
             MODIFY external FROM input\_s.
           CATCH cx\_root.
         ENDTRY.
       ENDLOOP.
     ENDIF.
   ENDMETHOD.

### Example: after mapping

  METHOD zodin\_intf\_mapping~after.
     _&quot;Check invoice number (xblnr) is unique in file_
     DATA(output\_tmp) = internal.
     DATA output\_s\_tmp LIKE LINE OF internal.
     DELETE ADJACENT DUPLICATES FROM output\_tmp COMPARING counter.
     SORT output\_tmp BY xblnr.
     DATA(lines\_before) = lines( output\_tmp ).
     DELETE ADJACENT DUPLICATES FROM output\_tmp COMPARING xblnr.
     DATA(lines\_after) = lines( output\_tmp ).
     IF lines\_before NE lines\_after.
       RAISE EXCEPTION TYPE zcx\_odin EXPORTING text = &#39;xblnr (Invoice Number) must be unique within a file&#39;.
     ENDIF.
     _&quot;Derive document type (blart) from vendor master data (field begru)_
     LOOP AT internal INTO DATA(output\_s).
       output\_s-zlspr = &#39;R&#39;.
       IF output\_s-lifnr IS NOT INITIAL AND output\_s-blart IS INITIAL.
         SELECT SINGLE begru INTO @DATA(begru) FROM lfa1 WHERE lifnr = @output\_s-lifnr.
         IF sy-subrc = 0.
           CASE begru.
             WHEN &#39;HONO&#39;.
               DATA(blart) = &#39;KH&#39;.
             WHEN &#39;0001&#39;. _&quot;Additional distinction depending on debit/credit required_
               IF output\_s-wrbtr\_h IS NOT INITIAL.
                 blart = &#39;KR&#39;.
               ELSE.
                 blart = &#39;KG&#39;.
               ENDIF.
             WHEN OTHERS.
               RAISE EXCEPTION TYPE zcx\_odin EXPORTING text = |Could not derive BLART from vendor { output\_s-lifnr }, BEGRU must be HONO or 0001|.
           ENDCASE.
         ENDIF.
       ENDIF.
       MODIFY internal FROM output\_s TRANSPORTING zlspr.
       AT END OF counter.
         IF blart IS NOT INITIAL.
           output\_s\_tmp-blart = blart.
           MODIFY internal FROM output\_s\_tmp TRANSPORTING blart WHERE counter = output\_s-counter.
           CLEAR blart.
         ENDIF.
       ENDAT.
     ENDLOOP.
   ENDMETHOD.

### Snippet: after mapping, error for ALV list

    LOOP AT internal INTO DATA(internal\_s).
       _&quot;..._
       output\_s-state = zodin\_cl\_posting=\&gt;state-error.
       output\_s-msg = &#39;Optional error text&#39;.
       MODIFY internal FROM internal\_s TRANSPORTING state msg.
     ENDLOOP.

### Example: before posting

  METHOD zodin\_intf\_posting~before.
     _&quot;Check IBAN of vendor against field CUSTOM1_
     DATA iban TYPE iban.
     LOOP AT document INTO DATA(document\_s).
       CHECK document\_s-lifnr IS NOT INITIAL.
       iban = document\_s-custom1.
       CONDENSE iban NO-GAPS.
       SELECT COUNT(\*) FROM lfbk
         INNER JOIN tiban ON lfbk~bankl = tiban~bankl AND lfbk~bankn = lfbk~bankn AND lfbk~banks = lfbk~banks
         UP TO 1 ROWS
         WHERE lifnr = @document\_s-lifnr
           AND iban = @iban.
       IF sy-subrc NE 0.
         RAISE EXCEPTION TYPE zcx\_odin EXPORTING text = |IBAN { iban } could not be found within masterdata of vendor { document\_s-lifnr }|.
       ENDIF.
     ENDLOOP.
   ENDMETHOD.

## Contribution

Suggestions: Aliases for good old (german) bkpf/bseg field names, REST/JSON Interface, SOAP/XML Interface, add fields, enable parked documents, enable CPDs, implement default strategies e.g. for DATEV format.

Please make sure to keep downwards compatibility to ABAP release 7.40.
