class ZCX_ODIN definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants ZCX_FDOC type SOTR_CONC value '005056977EF71EEB9AF9D255B6CC868D' ##NO_TEXT.
  data TEXT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !TEXT type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ODIN IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_FDOC .
 ENDIF.
me->TEXT = TEXT .
  endmethod.
ENDCLASS.
