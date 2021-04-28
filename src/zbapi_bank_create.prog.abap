*&---------------------------------------------------------------------*
*& Report ZBAPI_BANK_CREATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbapi_bank_create NO STANDARD PAGE HEADING.

DATA:BEGIN OF wa_data,
       bctry LIKE bapi1011_key-bank_ctry,
       bkey  LIKE bapi1011_key-bank_key.
    INCLUDE STRUCTURE bapi1011_address.
DATA END OF wa_data.


DATA lt_data LIKE TABLE OF wa_data.

DATA : wa_add LIKE bapi1011_address.

DATA: wa_return LIKE bapiret2.

*CALL FUNCTION 'UPLOAD'
*  EXPORTING
*    filetype = 'DAT '
*  TABLES
*    data_tab = lt_data.

*//Local Varible delaration
DATA:con_path TYPE string.

*// Selection screen design
PARAMETERS : p_path TYPE rlgrap-filename.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
    IMPORTING
      file_name     = p_path.

START-OF-SELECTION.
  con_path = p_path.
*//Uploading Data from Prsenetation Server
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = con_path
      filetype = 'ASC'
    TABLES
      data_tab = lt_data.

  LOOP AT lt_data INTO wa_data.
    MOVE-CORRESPONDING wa_data TO wa_add.
    CALL FUNCTION 'BAPI_BANK_CREATE'
      EXPORTING
        bank_ctry    = wa_data-bctry
        bank_key     = wa_data-bkey
        bank_address = wa_add
      IMPORTING
        return       = wa_return.


    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    IF NOT wa_return IS INITIAL.
      WRITE:/ wa_return-message.
    ENDIF.
  ENDLOOP.

  IF sy-subrc EQ 0.
    WRITE: 'SUCCESS'.
  ENDIF.
