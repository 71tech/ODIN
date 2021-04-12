class ZCL_ODIN_ACC definition
  public
  final
  create public .

*"* public components of class ZCL_ODIN_ACC
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ACC_DOCUMENT .
protected section.
*"* protected components of class ZCL_ODIN_ACC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_ODIN_ACC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_ODIN_ACC IMPLEMENTATION.


METHOD if_ex_acc_document~change .

  DATA: wa_extension           TYPE bapiparex,
        ext_value(960)         TYPE c,
        wa_accit               TYPE accit,
        l_ref                  TYPE REF TO data,
        l_rcl_abap_structdescr TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS: <l_struc> TYPE any,
                 <l_field> TYPE any.

  LOOP AT c_extension2 INTO wa_extension
    WHERE structure = zcl_odin_posting=>odin_extension_structure.
    CONCATENATE wa_extension-valuepart1 wa_extension-valuepart2
                wa_extension-valuepart3 wa_extension-valuepart4
           INTO ext_value.
    CLEAR l_ref.
    CREATE DATA l_ref TYPE (wa_extension-structure).
    ASSIGN l_ref->* TO <l_struc>.
    MOVE ext_value TO <l_struc>.
    ASSIGN COMPONENT 'POSNR' OF STRUCTURE <l_struc> TO <l_field>.
    READ TABLE c_accit WITH KEY posnr = <l_field> INTO wa_accit.
    IF sy-subrc IS INITIAL.
      DATA(tabix) = sy-tabix.
*      MOVE-CORRESPONDING <l_struc> TO wa_accit. "No simple MOVE-CORRESPONDING - ignore initials!
      l_rcl_abap_structdescr ?= cl_abap_typedescr=>describe_by_data( <l_struc> ).
      LOOP AT l_rcl_abap_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_str_component>).
        ASSIGN COMPONENT <fs_str_component>-name OF STRUCTURE wa_accit TO FIELD-SYMBOL(<fs_dest_field>).
        IF sy-subrc = 0.
          ASSIGN COMPONENT <fs_str_component>-name OF STRUCTURE <l_struc> TO FIELD-SYMBOL(<fs_source_field>).
          ASSERT sy-subrc = 0.
          IF <fs_source_field> IS NOT INITIAL.
            <fs_dest_field> = <fs_source_field>.
          ENDIF.
        ENDIF.
      ENDLOOP.
      MODIFY c_accit FROM wa_accit INDEX tabix.
    ENDIF.
  ENDLOOP.

ENDMETHOD.                    "IF_EX_ACC_DOCUMENT~CHANGE


method IF_EX_ACC_DOCUMENT~FILL_ACCIT.
endmethod.
ENDCLASS.
