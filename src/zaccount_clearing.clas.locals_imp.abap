*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*&---------------------------------------------------------------------*
*& Include          ZMAH_ACC_POST_F01
*&---------------------------------------------------------------------*
CLASS lcl_cc_pay_clear DEFINITION FINAL.
** Public Section
  PUBLIC SECTION.
** Public Types Declaration
** ALV Output
    TYPES: BEGIN OF lys_output,
             burks   TYPE bukrs,
             belnr   TYPE belnr_d,
             buzei   TYPE buzei,
             augdt   TYPE augdt,
             augbl   TYPE augbl,
             status  TYPE icon_d,
             waers   TYPE waers,
             dmbtr   TYPE dmbtr,
             zuonr   TYPE dzuonr,
             message TYPE string,
           END OF lys_output,
           lyt_output TYPE STANDARD TABLE OF lys_output,
** For the Cross company codes combination
           BEGIN OF lys_clearing_cc,
             vbukr TYPE bukrs,
             abukr TYPE bukrs,
           END OF lys_clearing_cc,
           lyt_clearing_cc TYPE STANDARD TABLE OF lys_clearing_cc,
** For Open Items from BSIS
           BEGIN OF lys_open_entries,
             bukrs         TYPE bukrs,
             hkont         TYPE hkont,
             zuonr         TYPE dzuonr,
             gjahr         TYPE gjahr,
             belnr         TYPE belnr_d,
             buzei         TYPE buzei,
             waers         TYPE waers,
             shkzg         TYPE shkzg,
             dmbtr         TYPE dmbtr,
             total_amt_lcl TYPE dmbtr,
             ind           TYPE shkzg,
           END OF lys_open_entries,
           lyt_open_entries TYPE STANDARD TABLE OF lys_open_entries WITH EMPTY KEY.

** Public Data
    DATA : lt_alv         TYPE lyt_output,          " Output ALV
           lt_cc_bukrs    TYPE RANGE OF bukrs,      "
           lt_clearing_cc TYPE lyt_clearing_cc.     "

** Public methods
    METHODS:
      check_clr_bukrs,                    " Validate Company codes from T001U
      perform_cc_clearing,                " Main Method
      display_output.                     " Display Final Output

** Private Section
  PRIVATE SECTION .
** Private Types Declaration

** Private Constants declaration
    CONSTANTS:
      lc_e TYPE syst_msgty  VALUE 'E',
      lc_w TYPE syst_msgty  VALUE 'W',
      lc_s TYPE syst_msgty  VALUE 'S'.

** Private Methods
    METHODS:
      fetch_open_entries EXPORTING et_open_entries TYPE lyt_open_entries,             " Fetch Open Items from BSIS based on Selection Screen
      post_cross_company_clearing IMPORTING is_debit_open  TYPE lys_open_entries      " Post the Cross Company Creaing using the FM
                                            is_credit_open TYPE lys_open_entries
                                            iv_doc_type    TYPE blart
                                  EXPORTING ev_debit_doc   TYPE belnr_d
                                            ev_credit_doc  TYPE belnr_d
                                            ev_msg         TYPE string
                                            ev_msg_type    TYPE syst_msgty,
      append_record_alv IMPORTING it_debit_open  TYPE lyt_open_entries                " Generate output ALV records
                                  it_credit_open TYPE lyt_open_entries
                                  iv_message     TYPE string
                                  iv_msg_type    TYPE syst_msgty
                                  iv_debit_doc   TYPE belnr_d OPTIONAL
                                  iv_credit_doc  TYPE belnr_d OPTIONAL.
ENDCLASS.

CLASS lcl_cc_pay_clear IMPLEMENTATION.

  METHOD perform_cc_clearing.
    DATA : lt_bsis_entries TYPE lyt_open_entries,
           lv_credit_found TYPE boolean,
           lv_cc           TYPE string,
           lv_blart        TYPE blart.
    CONSTANTS: lc_10268          TYPE char40   VALUE 'E_10268_FI',
               lc_techname_blart TYPE rollname VALUE 'BLART'.

** Fetch the Doucmnet type that needs to be created from the parameter table
*    CALL METHOD /chc/cl_fi_param_tab_utility=>get_values_param_cust
*      EXPORTING
*        iv_objectname  = lc_10268
*        iv_techname_1  = lc_techname_blart
*      IMPORTING
*        et_range       = DATA(lt_range)
*        ev_err_message = DATA(lv_err_msg).
*    IF lt_range IS NOT INITIAL AND lv_err_msg IS INITIAL.
*** Assigning the vlaue maintained Purchase org
*      ASSIGN lt_range[ 1 ] TO FIELD-SYMBOL(<ls_data>).
*      IF sy-subrc EQ 0 AND <ls_data> IS ASSIGNED.
*        lv_blart = CONV #( <ls_data>-low ).
*      ELSE.
*** Values for Docuemnt type not maintained in Parameter Table
*        MESSAGE e008(/chc/fi) WITH lc_10268 DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.
*    ELSE.
*** Values for Docuemnt type not maintained in Parameter Table
*      MESSAGE e008(/chc/fi) WITH lc_10268 DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.

** Fetch Open Entries from BSIS Table
    fetch_open_entries(
      IMPORTING
        et_open_entries = lt_bsis_entries ).

** If Entries are found Found
    IF lt_bsis_entries IS NOT INITIAL.
** Sepearating the Debit anc Credit Entries
      DATA(lt_open_items_debit) = lt_bsis_entries.
      DATA(lt_open_items_credit) = lt_bsis_entries.
      DELETE lt_open_items_debit WHERE ind EQ 'H'.
      SORT lt_open_items_debit ASCENDING BY bukrs hkont zuonr.
      DELETE ADJACENT DUPLICATES FROM lt_open_items_debit COMPARING bukrs hkont zuonr.
      DELETE lt_open_items_credit WHERE ind EQ 'S'.
      SORT lt_open_items_credit ASCENDING BY bukrs hkont zuonr.
      DELETE ADJACENT DUPLICATES FROM lt_open_items_credit COMPARING bukrs hkont zuonr.
    ENDIF.

** Cross company Payment is cleared in following manner
** Eg 2 company codes GB01 and GB04.
** GB01 has Total Positive amount and GB04 has Total negative amount and has same assignment number on both the documents
** The payments will be cleared for T001u entry VBUKR = GB01 and ABUKR = GB04

    IF lt_open_items_debit IS NOT INITIAL.
** Looping at all the Open Debit Items
      LOOP AT lt_open_items_debit ASSIGNING FIELD-SYMBOL(<ls_open_debit>).

** Building all the entries for BUKRS HKONT and ZUONR which is required in the output display
        DATA(lt_debit_open) = VALUE lyt_open_entries( FOR <ls_debit> IN lt_bsis_entries
                                                      WHERE ( bukrs = <ls_open_debit>-bukrs AND
                                                              hkont = <ls_open_debit>-hkont AND
                                                              zuonr = <ls_open_debit>-zuonr )
                                                      ( CORRESPONDING #( <ls_debit> ) ) ).
** Read the Company Code Which Is Being Posted To entry
** There can be a scenario where same company code has multiple clearing Against Company codes
** For that Looping the records instead of Read
        CLEAR : lv_credit_found.
        LOOP AT lt_clearing_cc ASSIGNING FIELD-SYMBOL(<ls_clearing_cc>) WHERE vbukr = <ls_open_debit>-bukrs. "#EC CI_STDSEQ "#EC CI_NESTED
** Checking for corresponding Credit entry
** for Company Code Which Will Be Cleared Against
          READ TABLE lt_open_items_credit ASSIGNING FIELD-SYMBOL(<ls_open_credit>) WITH KEY bukrs = <ls_clearing_cc>-abukr
                                                                                            hkont = <ls_open_debit>-hkont
                                                                                            zuonr = <ls_open_debit>-zuonr
                                                                                            BINARY SEARCH.
          IF sy-subrc EQ 0.
** Saving the index to delete later
            DATA(lv_tabix) = sy-tabix.
** Flaging the credit entry found
            lv_credit_found = abap_true.
** Building all the entries for BUKRS HKONT and ZUONR which is required in the output display
            DATA(lt_credit_open) = VALUE lyt_open_entries( FOR <ls_credit> IN lt_bsis_entries
                                                           WHERE ( bukrs = <ls_open_credit>-bukrs AND
                                                                   hkont = <ls_open_credit>-hkont AND
                                                                   zuonr = <ls_open_credit>-zuonr )
                                                           ( CORRESPONDING #( <ls_credit> ) ) ).
*            IF p_test IS NOT INITIAL OR p_prod IS NOT INITIAL.
** Method to post/simulate the clearing documents
              post_cross_company_clearing(
              EXPORTING
                  is_debit_open  = <ls_open_debit>        " Pasing the Debit entry which has VBUKR of T001U
                  is_credit_open = <ls_open_credit>       " Pasing the Credit entry which has ABUKR of T001U
                  iv_doc_type    = lv_blart               " Document type from parameter table
                IMPORTING
                  ev_debit_doc   = DATA(lv_debit_doc)     " Clearing Document Number for VBUKR of T001U
                  ev_credit_doc  = DATA(lv_credit_doc)    " Clearing Document Number for ABUKR of T001U
                  ev_msg         = DATA(lv_message)       " Error or Successful message
                  ev_msg_type    = DATA(lv_msg_typ)       " Message Type
              ).
** Building the final ALV output
              append_record_alv(
                EXPORTING
                it_debit_open  = lt_debit_open            " Entries for VBUKR HKONT and ZUONR which is required in the output display
                it_credit_open = lt_credit_open           " Entries for ABUKR HKONT and ZUONR which is required in the output display
                iv_message     = lv_message               " Message
                iv_msg_type    = lv_msg_typ               " Message Type
                iv_debit_doc   = lv_debit_doc             " Clearing Document Number for VBUKR of T001U
                iv_credit_doc  = lv_credit_doc ).         " Clearing Document Number for ABUKR of T001U
*            ENDIF.
** Deleting the already rad credit entries
            DELETE lt_open_items_credit INDEX lv_tabix.
** Exiting the LT_CLEARING_CC Loop once the credit entry if found
            EXIT.
          ENDIF.
        ENDLOOP.
** If no credit entry found then generating the error
        IF lv_credit_found IS INITIAL.
** If the corresponding Credit entry
** for Company Code Which Will Be Cleared Against is not found
          MESSAGE e003(/chc/fi) WITH <ls_clearing_cc>-abukr INTO lv_message.     "No Corresponding entry found for Clearing Against CompCode &
          MESSAGE e007(/chc/fi) INTO DATA(lv_msg).                               "Document Cannot be Cleared
          lv_message = lv_msg && `. ` && lv_message.
          lv_msg_typ = lc_e.
          CLEAR: lv_debit_doc, lv_credit_doc.
** Building the final ALV output
          append_record_alv(
            EXPORTING
            it_debit_open  = lt_debit_open
            it_credit_open = lt_credit_open
            iv_message     = lv_message
            iv_msg_type    = lv_msg_typ
            iv_debit_doc   = lv_debit_doc
            iv_credit_doc  = lv_credit_doc ).
        ENDIF.
        CLEAR : lt_debit_open, lt_credit_open.
      ENDLOOP.
    ENDIF.

** Checking if any of the credit open items are present and raising an error.
    LOOP AT lt_open_items_credit ASSIGNING <ls_open_credit>.
** There can be a scenario where same company code has multiple clearing Against Company codes
** For that Looping the records instead of Read
      LOOP AT lt_clearing_cc ASSIGNING <ls_clearing_cc> WHERE abukr = <ls_open_credit>-bukrs. "#EC CI_STDSEQ "#EC CI_NESTED
        IF lv_cc IS INITIAL.
          lv_cc = <ls_clearing_cc>-vbukr.
        ELSE.
          lv_cc = lv_cc && `/ ` && <ls_clearing_cc>-vbukr.
        ENDIF.
      ENDLOOP.
      MESSAGE e003(/chc/fi) WITH lv_cc INTO lv_message.     "No Corresponding entry found for Clearing Against CompCode
      MESSAGE e007(/chc/fi) INTO lv_msg.                    "Document Cannot be Cleared
      lv_message = lv_msg && `. ` && lv_message.
      lv_msg_typ = lc_e.
      CLEAR: lt_debit_open, lv_cc.
** Building all the entries for BUKRS HKONT and ZUONR which is required in the output display
      lt_credit_open = VALUE lyt_open_entries( FOR <ls_credit> IN lt_bsis_entries
                                               WHERE ( bukrs = <ls_open_credit>-bukrs AND
                                                       hkont = <ls_open_credit>-hkont AND
                                                       zuonr = <ls_open_credit>-zuonr )
                                                     ( CORRESPONDING #( <ls_credit> ) ) ).
** Building the final ALV output
      append_record_alv(
        EXPORTING
        it_debit_open  = lt_debit_open
        it_credit_open = lt_credit_open
        iv_message     = lv_message
        iv_msg_type    = lv_msg_typ
        iv_debit_doc   = lv_debit_doc
        iv_credit_doc  = lv_credit_doc ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_clr_bukrs.
    DATA :lv_cc       TYPE string.

** Selecting the company codes from T001 table for validation
    SELECT bukrs
        FROM t001
      INTO TABLE @DATA(lt_company_codes)
      WHERE bukrs = 'GB01'
      ORDER BY bukrs.
    IF sy-subrc EQ 0.
** Selecting the compnay codes maintianed for Clearing Between Company Codes
      SELECT vbukr,
             abukr
        FROM t001u
        INTO TABLE @lt_clearing_cc
        WHERE vbukr = 'GB01'
        AND abukr = 'GB01'            "" Added by MEHTARUS on 08.10.2020 for DF-1563
        ORDER BY vbukr.
      IF sy-subrc EQ 0.
** Builing a range table of Company codes relevant for Clearing Between Company Codes
        lt_cc_bukrs = VALUE #( FOR <ls_bukrs> IN lt_clearing_cc
                           ( sign = 'I' option = 'EQ' low = <ls_bukrs>-vbukr )
                           ( sign = 'I' option = 'EQ' low = <ls_bukrs>-abukr ) ).
        SORT lt_cc_bukrs ASCENDING BY low.
        DELETE ADJACENT DUPLICATES FROM lt_cc_bukrs COMPARING low.
        DELETE lt_company_codes WHERE bukrs IN lt_cc_bukrs.
** Deleting the entries fetched from T001 table
** if the values are found than raising error to change selection parameter.
        IF lt_company_codes IS NOT INITIAL.
          LOOP AT lt_company_codes ASSIGNING FIELD-SYMBOL(<ls_company_codes>).
            lv_cc = lv_cc && ` ` && <ls_company_codes>-bukrs.
          ENDLOOP.
          " Message  & Company codes are not valid for Cross Company clearing.
          MESSAGE e004(/chc/fi) WITH lv_cc DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
** if the values are not found in T001U found than raising error to change selection parameter.
        " Message Please enter valid pair of compay code for Cross Company clearing.
        MESSAGE e000(/chc/fi) WITH TEXT-007 TEXT-008 DISPLAY LIKE 'E'.                     "" Changed message by MEHTARUS on 08.10.2020 for DF-1563
      ENDIF.
    ELSE.
      " Message  All the entered Company codes are not valid for Cross Company clearing.
      MESSAGE e005(/chc/fi) DISPLAY LIKE 'E'.
    ENDIF.
** Populating a global table for Authority check
*    IF lt_cc_bukrs IS NOT INITIAL.
*      gt_cc_bukrs = lt_cc_bukrs.
*    ENDIF.
  ENDMETHOD.

  METHOD post_cross_company_clearing.
    CLEAR : ev_credit_doc, ev_debit_doc, ev_msg_type, ev_msg.
** Local Data Declaration
    DATA: lv_msgid   LIKE                   sy-msgid,
          lv_msgno   LIKE                   sy-msgno,
          lv_msgty   LIKE                   sy-msgty,
          lv_msgv1   LIKE                   sy-msgv1,
          lv_msgv2   LIKE                   sy-msgv2,
          lv_msgv3   LIKE                   sy-msgv3,
          lv_msgv4   LIKE                   sy-msgv4,
          lv_subrc   TYPE                   sy-subrc,

          ls_ftclear TYPE                   ftclear,
          ls_ftpost  TYPE                   ftpost,

          lt_blntab  TYPE STANDARD TABLE OF blntab,
          lt_ftclear TYPE STANDARD TABLE OF ftclear,
          lt_ftpost  TYPE STANDARD TABLE OF ftpost,
          lt_fttax   TYPE STANDARD TABLE OF fttax.

** Local Constant Declaration
    CONSTANTS:
      lc_auglv           TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
      lc_tcode           TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
      lc_sgfunct         TYPE rfipi-sgfunct VALUE 'C',        "Post immediately
      lc_stype_k         TYPE stype_pi VALUE 'K',
      lc_fnam_bkpf_bldat TYPE bdc_fnam VALUE 'BKPF-BLDAT',
      lc_fnam_bkpf_budat TYPE bdc_fnam VALUE 'BKPF-BUDAT',
      lc_fnam_bkpf_blart TYPE bdc_fnam VALUE 'BKPF-BLART',
*      lc_fval_sa         TYPE bdc_fval VALUE 'AB',
      lc_fnam_bkpf_bukrs TYPE bdc_fnam VALUE 'BKPF-BUKRS',
      lc_fnam_bkpf_waers TYPE bdc_fnam VALUE 'BKPF-WAERS',
      lc_agkoa_s         TYPE koart VALUE 'S',
      lc_selfd_zuonr     TYPE fld30_f05a VALUE 'ZUONR',
      lc_funcation_c     TYPE funct_pi VALUE 'C',
      lc_mode_n          TYPE allgazmd VALUE 'N',
      lc_update_s        TYPE allgvbmd VALUE 'S',
      lc_msgid_00        TYPE syst_msgid VALUE '00',
      lc_msgno_344       TYPE syst_msgno VALUE '344',
      lc_dynnr           TYPE bdc_dynnr VALUE '0733'.

    FIELD-SYMBOLS : <ls_msg> TYPE bdcmsgcoll.
*** begin of change by MEHTARUS on 24.06.2020
** Clearing date from the selection screen needs to passed as posting date for clearing document.
*    DATA(lv_date_posting) = |{ sy-datum DATE = USER }|.
    DATA(lv_date_posting) = |{ sy-datum DATE = USER }|.
*** End of change by MEHTARUS on 24.06.2020
*    IF p_clrcur IS NOT INITIAL.
      DATA(lv_waers) = 'USD'.
*    ELSE.
*      lv_waers = is_debit_open-waers.
*    ENDIF.

* Populating the header info/First screen in F-05
* Batch Input Values
    ls_ftpost-stype = lc_stype_k.             "K "Header
    ls_ftpost-count = 1.                      "number of Dynpro

    ls_ftpost-fnam = lc_fnam_bkpf_bldat.      "BKPF-BLDAT
    ls_ftpost-fval = lv_date_posting.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = lc_fnam_bkpf_budat.       "BKPF-BUDAT
    ls_ftpost-fval = lv_date_posting.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = lc_fnam_bkpf_blart.        "BKPF-BLART
    ls_ftpost-fval = iv_doc_type.                "Document type form parameter table
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = lc_fnam_bkpf_bukrs.        "BKPF-BUKRS
    ls_ftpost-fval = is_debit_open-bukrs.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = lc_fnam_bkpf_waers.        "BKPF-WAERS
    ls_ftpost-fval = lv_waers.
    APPEND ls_ftpost TO lt_ftpost.

*Documents to be cleared
    ls_ftclear-agkoa = lc_agkoa_s.               "S    "Account Type
    ls_ftclear-agbuk = is_debit_open-bukrs.      "Company code
    ls_ftclear-agkon = is_debit_open-hkont.      "Company code
    ls_ftclear-xnops = abap_true.
    ls_ftclear-selfd = lc_selfd_zuonr.           "ZUONR  "Selection Field
    ls_ftclear-selvon = is_debit_open-zuonr.
    APPEND ls_ftclear TO lt_ftclear.
*Documents to be cleared
    ls_ftclear-agkoa = lc_agkoa_s.               "S    "Account Type
    ls_ftclear-agbuk = is_credit_open-bukrs.      "Example company code
    ls_ftclear-agkon = is_credit_open-hkont.      "Example company code
    ls_ftclear-xnops = abap_true.
    ls_ftclear-selfd = lc_selfd_zuonr.           "ZUONR  "Selection Field
    ls_ftclear-selvon = is_credit_open-zuonr.
    APPEND ls_ftclear TO lt_ftclear.

**Initial information for internal accounting interface
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_client           = sy-mandt
        i_function         = lc_funcation_c
        i_mode             = lc_mode_n                      " No error in background mode of BDS
        i_update           = lc_update_s
        i_user             = sy-uname
      EXCEPTIONS
        client_incorrect   = 1
        function_invalid   = 2
        group_name_missing = 3
        mode_invalid       = 4
        update_invalid     = 5
        user_invalid       = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
** Error messages
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_msg.
      ev_msg_type = lc_e.
      RETURN.
    ENDIF.
** Post with clearing (FB05) using internal posting interface
    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = lc_auglv
        i_tcode                    = lc_tcode
        i_sgfunct                  = lc_sgfunct
        i_no_auth                  = ' '
        i_xsimu                    = 'X' "p_test
      IMPORTING
        e_msgid                    = lv_msgid
        e_msgno                    = lv_msgno
        e_msgty                    = lv_msgty
        e_msgv1                    = lv_msgv1
        e_msgv2                    = lv_msgv2
        e_msgv3                    = lv_msgv3
        e_msgv4                    = lv_msgv4
        e_subrc                    = lv_subrc
      TABLES
        t_blntab                   = lt_blntab
        t_ftclear                  = lt_ftclear
        t_ftpost                   = lt_ftpost
        t_fttax                    = lt_fttax
      EXCEPTIONS
        clearing_procedure_invalid = 1
        clearing_procedure_missing = 2
        table_t041a_empty          = 3
        transaction_code_invalid   = 4
        amount_format_error        = 5
        too_many_line_items        = 6
        company_code_invalid       = 7
        screen_not_found           = 8
        no_authorization           = 9
        OTHERS                     = 10.
    IF sy-subrc <> 0 OR lv_subrc <> 0.

** These messages are required for simutaion/test run of the program, if the transaction runs in productive mode the correct errors are automatically populated.

** The below static table is saved in the the form TRANSAKTION_BEENDEN of Function Module POSTING_INTERFACE_CLEARING.
** This table contains the message that is displayed on each and every screen
      IF  lv_msgty <> lc_e.
** Deleting the message where MSGID 00 and MSGNO 344 as those are message realted to No batch input data for dynpro & &
** Deleting the message for screen which are not correct errors
*        DELETE /chc/cl_fi_cross_comp_clear=>st_message_table WHERE ( msgid EQ lc_msgid_00  AND msgnr EQ lc_msgno_344 )
*                                                                    OR dynumb = lc_dynnr.
*        IF /chc/cl_fi_cross_comp_clear=>st_message_table IS NOT INITIAL.
*** Reading the last line of the message table which provides the correct error.
*** The Function Module calls the FB05 transaction internally, the error message on the screen are not displayed with correct message type
*** if the last message is of type warning, saving the type as 'W' or else 'E'
*          DATA(lv_lines) = lines( /chc/cl_fi_cross_comp_clear=>st_message_table ).
*          ASSIGN /chc/cl_fi_cross_comp_clear=>st_message_table[ lv_lines ] TO <ls_msg>.
*          IF sy-subrc EQ 0 AND <ls_msg> IS ASSIGNED.
*            MESSAGE ID <ls_msg>-msgid TYPE <ls_msg>-msgtyp NUMBER <ls_msg>-msgnr WITH <ls_msg>-msgv1 <ls_msg>-msgv2 <ls_msg>-msgv3 <ls_msg>-msgv4 INTO ev_msg.
*            IF <ls_msg>-msgtyp EQ lc_w.
*              ev_msg_type = lc_w.
*            ELSE.
*              ev_msg_type = lc_e.
*            ENDIF.
*          ENDIF.
*** if there are no error it means everything can be cleared successfully
*        ELSE.
*          MESSAGE e006(/chc/fi) INTO ev_msg.  "Document can be cleared
*          ev_msg_type = lc_s.
*        ENDIF.
      ELSE.
        IF lv_msgid <> lc_msgid_00  AND lv_msgno <> lc_msgno_344 .
** Error messages
          MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO ev_msg.
        ELSE.
          ev_msg = TEXT-005 && ` ` && TEXT-006. "Master Data configuration issue. Please try Clearing the documents manually using F-03
        ENDIF.
        ev_msg_type = lc_e.
      ENDIF.
    ENDIF.
** if the Document Number Table for Financial Accounting table is not initial
    IF lt_blntab IS NOT INITIAL.
      TRY .
** Populating the document numbers for VBUKR and ABUKR company codes
          ev_debit_doc = lt_blntab[ bukrs = is_debit_open-bukrs ]-belnr.
          ev_credit_doc = lt_blntab[ bukrs = is_credit_open-bukrs ]-belnr.
          ev_msg = TEXT-001.    " Clearing Document Successfully Posted
          ev_msg_type = lc_s.
        CATCH cx_sy_itab_line_not_found.
** if entry not found populating the message
          MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO ev_msg.
          ev_msg_type = lc_e.
      ENDTRY.
    ENDIF.
** Finish posting
    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXPORTING
        i_bdcimmed              = abap_true
      EXCEPTIONS
        session_not_processable = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
** Error messages
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_msg.
      ev_msg_type = lc_e.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD fetch_open_entries.
    CLEAR : et_open_entries.
** CTE to select the data and perform aggregation of selected data.
    WITH
      +bsis AS (
** Select data from BSIS
        SELECT bukrs, hkont, zuonr, gjahr, belnr, buzei, waers, shkzg,
                CASE shkzg
               	WHEN 'S' THEN CAST( dmbtr * 1 AS DEC( 23, 2 ) )
               	ELSE CAST( dmbtr * -1 AS DEC( 23, 2 ) )
                END AS dmbtr
               FROM bsis_view
               WHERE bukrs IN @lt_cc_bukrs
                 AND gjahr = '2020'
                 AND ( zuonr = '909' AND
                       zuonr IN ( SELECT zuonr FROM bsis_view WHERE  belnr = '1000001' AND budat = '20210101' ) )
                 AND hkont = '100010' ),
      +aggr AS (
** Perform Aggregation on selected data
       SELECT   +bsis~bukrs, +bsis~hkont, +bsis~zuonr,
                SUM( dmbtr ) AS total_amt_lcl, 'S' as ind
*                CASE
*                     WHEN
*                      dmbtr  GE 0 THEN 'S'
*                     ELSE 'H'
*                     END AS ind
                FROM +bsis
                GROUP BY bukrs, hkont, zuonr )
** Perform Aggregation on selected data

** Select the final result table required
        SELECT a~bukrs, a~hkont, a~zuonr, a~gjahr, a~belnr, a~buzei, a~waers, a~shkzg, a~dmbtr,
        b~total_amt_lcl, b~ind
        FROM +bsis AS a
        INNER JOIN +aggr AS b
        ON a~bukrs = b~bukrs
        AND a~hkont = b~hkont
        AND a~zuonr = b~zuonr
        ORDER BY a~bukrs, a~hkont, a~zuonr, a~gjahr, a~belnr, a~buzei
        INTO TABLE @et_open_entries.
    IF sy-subrc <> 0.
** No Entries found for given Selection Parameters
*      MESSAGE e002(/chc/fi) DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD append_record_alv.
    DATA: lv_status TYPE icon_d.
** CLearing date
*    IF p_clrdt IS INITIAL.
*      DATA(lv_augdt) = sy-datum.
*    ELSE.
    DATA: lv_augdt TYPE datum.
      lv_augdt = sy-datum.
*    ENDIF.
** Case for Icons
    CASE iv_msg_type.
      WHEN lc_e.
        lv_status = icon_red_light.
      WHEN lc_s.
        lv_status = icon_green_light.
      WHEN OTHERS.
        lv_status = icon_yellow_light.
    ENDCASE.
** Build ALV output for Debit Entries
    lt_alv = VALUE #( BASE lt_alv
                   FOR <ls_debit_open> IN it_debit_open
                   ( burks  = <ls_debit_open>-bukrs
                    belnr   =  <ls_debit_open>-belnr
                    buzei   =  <ls_debit_open>-buzei
                    status  =  lv_status
                    augdt   =  lv_augdt
                    augbl   =  iv_debit_doc
                    waers   =  <ls_debit_open>-waers
                    dmbtr   =  <ls_debit_open>-dmbtr
                    zuonr   =  <ls_debit_open>-zuonr
                    message =  iv_message               ) ).
** Build ALV output for Credit Entries
    lt_alv = VALUE #( BASE lt_alv
                   FOR <ls_credit_open> IN it_credit_open
                   ( burks   =  <ls_credit_open>-bukrs
                     belnr   =  <ls_credit_open>-belnr
                     buzei   =  <ls_credit_open>-buzei
                     status  =  lv_status
                     augdt   =  lv_augdt
                     augbl   =  iv_credit_doc
                     waers   =  <ls_credit_open>-waers
                     dmbtr   =  <ls_credit_open>-dmbtr
                     zuonr   =  <ls_credit_open>-zuonr
                     message =  iv_message               ) ).
  ENDMETHOD.

  METHOD display_output.

*    IF p_doccr IS INITIAL.
      DELETE lt_alv WHERE status EQ icon_green_light.
      DELETE lt_alv WHERE status EQ icon_yellow_light.
*    ENDIF.
*    IF p_docer IS INITIAL.
      DELETE lt_alv WHERE status EQ icon_red_light.
*    ENDIF.
    IF lt_alv IS NOT INITIAL.

** Create ALV grid instance
      TRY.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = DATA(lo_table)
            CHANGING
              t_table      = lt_alv.
        CATCH cx_salv_msg INTO DATA(lo_msg).
          DATA(lv_message) = lo_msg->get_text( ).
          MESSAGE s000(/chc/fi) WITH lv_message DISPLAY LIKE 'S'.
          RETURN.
      ENDTRY.

      lo_table->set_screen_status(
        EXPORTING
          report        = '/CHC/FI_A_10268_CC_PYMT_CLR_GO'              " ABAP Program: Current Master Program
          pfstatus      = 'STANDARD'                                    " Screens, Current GUI Status
      ).

** Set ALV Toolbars
      DATA(lo_functions) = lo_table->get_functions( ).
      lo_functions->set_all( ).

** Get Columns and Changes Properties
      DATA(lo_cols) = lo_table->get_columns( ).

      lo_cols->set_optimize( abap_true ).

      TRY.
          DATA(lo_col) = lo_cols->get_column( 'STATUS' ).
          lo_col->set_short_text( TEXT-004 ).             " Status
          lo_col->set_medium_text( TEXT-004 ).            " Status
          lo_col->set_long_text( TEXT-004 ).              " Status

          lo_col = lo_cols->get_column( 'MESSAGE' ).
*          IF p_errms IS INITIAL.
            lo_col->set_visible( value = if_salv_c_bool_sap=>false ).
*          ELSE.
            lo_col->set_short_text( TEXT-002 ).             " Message
            lo_col->set_medium_text( TEXT-002 ).           " Message
            lo_col->set_long_text( TEXT-002 ).            " Message
*          ENDIF.


        CATCH cx_salv_not_found INTO DATA(lo_msg1).
          lo_msg1->get_text( ).
        CATCH cx_salv_data_error INTO DATA(lo_msg2).
          lo_msg2->get_text( ).
      ENDTRY.
** Display ALV Grid
      lo_table->display( ).
    ELSE.
** No records to display
*      MESSAGE s000(/chc/fi) WITH TEXT-003 DISPLAY LIKE 'S'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
