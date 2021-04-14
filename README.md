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

## Quick start
Clone the ODIN repo using [abapgit](https://github.com/abapGit/abapGit). Create a package first, we recommend ZODIN. You can utilize this [simple Excel template](https://github.com/71tech/ODIN/blob/a18d07e11fa689ccd2c212c3ca3b8c320953de90/examples/ODIN_simple.xlsx) for your first postings. Of course some IDs such as company code, g/l account or vendor number need to be adjusted to your environment. Go to transaction ZODIN, select the local file, hit "Execute" and an ALV Grid List with your posting data should appear. If the status says  "READY" and everything looks fine hit "Post" and a FI document will be created. Double click on a row in order to jump to transaction fb03.

## Dataset structure

At the moment the interface can process Excel and csv files, further input options are planned.

For Excel and csv the following applies:

- The first row must contain the column names as under [Available fields](#available-fields) (alias identifiers are not yet available, but planned)
- The order of the columns is arbitrary
- From row two on, the user data follows, which must be arranged in the same way as in the first row
- One FI document line item corresponds to one row in the input data. Several rows are combined into one document via the COUNTER field
- There is no separate line for header fields from table bkpf, such as BUTXT, instead they are to be repeated redundantly in each row

### Example (csv)
```
COUNTER;STRATEGY;BUKRS;BUTXT;XBLNR;BLDAT;BUDAT;BLART;SGTXT;LIFNR;KUNNR;HKONT;WRBTR_S;WRBTR_H;WAERS;MWSKZ;CUSTOM1
1;MY_OPTIONAL_STRATEGY;1234;Example for incoming invoice;INVOICE-001;26.03.2020;26.03.2020;KR;Text position;;;154200;119,00;;EUR;V2;Some custom value
1;MY_OPTIONAL_STRATEGY;1234;Example for incoming invoice;INVOICE-001;26.03.2020;26.03.2020;KR;Text position;802236;;;;119,00;EUR;V2;Some custom value
2;MY_OPTIONAL_STRATEGY;1234;Another example;INVOICE-002;27.03.2020;26.03.2020;KR;Text position;;;154200;;59,50;EUR;V2;Another custom value
2;MY_OPTIONAL_STRATEGY;1234;Another example;INVOICE-002;27.03.2020;26.03.2020;KR;Text position;;;154200;;59,50;EUR;V2;Another custom value
2;MY_OPTIONAL_STRATEGY;1234;Another example;INVOICE-002;27.03.2020;26.03.2020;KR;Text position;802236;;;119,00;;EUR;V2;Another custom value
```
## Import program

Use sa38 > ZODIN or Transaction ZODIN, see [wiki](https://github.com/71tech/ODIN/wiki/ZODIN).

## Server side processing of multiple files

Use sa38 > ZODIN\_MULTIFILE, see [wiki](https://github.com/71tech/ODIN/wiki/ZODIN_MULTIFILE).

## BAdI reference (extensions)

See [wiki](https://github.com/71tech/ODIN/wiki/BAdI-Reference-(Extensions)).

## Contribution

Your [contribution](https://docs.abapgit.org/guide-contributing.html) is very welcome!

Suggestions: Aliases for good old (german) bkpf/bseg field names, REST/JSON Interface, SOAP/XML Interface, add fields, enable CPDs, Interface for DATEV format.

## Feedback

Please open an issue for comments, suggestions, questions etc.

