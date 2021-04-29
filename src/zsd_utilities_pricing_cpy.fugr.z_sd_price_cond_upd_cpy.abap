FUNCTION Z_SD_PRICE_COND_UPD_CPY .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_KOMG) TYPE  E1KOMG
*"     REFERENCE(IS_KONH) TYPE  E1KONH
*"     REFERENCE(IS_KONP) TYPE  E1KONP
*"  EXPORTING
*"     REFERENCE(ET_RETURN) TYPE  BAPIRET2_T
*"     REFERENCE(EV_IDOCNUM) TYPE  EDIDC-DOCNUM
*"--------------------------------------------------------------------
************************************************************************
*  Program Title       : Create Pricing Condition record               *
*  Developer           : Raghu Radhakrishnan(C5232755)                 *
*  Description:        : Create Pricing Condition Record through       *
*                        Inbound IDOC CONDA_A04                        *
*  Type:               : Function Module                               *
*  Remarks             : This function module is developed as no Std.  *
*                        published API avaialbe to post conditon rec   *
************************************************************************
* MODIFICATION HISTORY                                                 *
*  Change Request #    :                                               *
*  Author              :                                               *
*  Changed By          :                                               *
*  Modification Date   :                                               *
*  Description         :                                               *
************************************************************************
*Data Declaration
  DATA:lv_msg          TYPE bdidocattr-message,
       lv_error        TYPE edi_slight,
       ls_idoc_ctrl    TYPE edidc,
       ls_idoc_data    TYPE edidd,
       ls_komg_seg     TYPE e1komg,
       ls_konh_seg     TYPE e1konh,
       ls_konp_seg     TYPE e1konp,
       lv_logsys       TYPE logsys,
       ls_process_data TYPE tede2,
       ls_return       TYPE bapiret2,
       lt_idoc_ctrl    TYPE STANDARD TABLE OF edidc,
       lt_idoc_data    TYPE STANDARD TABLE OF edidd.

  CLEAR:ev_idocnum,et_return.

* Get Logical system
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = lv_logsys
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.
  IF sy-subrc <> 0.
    ls_return-type = sy-msgty.
    ls_return-id = sy-msgid.
    ls_return-number = sy-msgno.
    ls_return-message_v1 = sy-msgv1.
    ls_return-message_v2 = sy-msgv2.
    ls_return-message_v3 = sy-msgv3.
    ls_return-message_v4 = sy-msgv4.
    APPEND ls_return TO et_return.
    CLEAR:ls_return.
    RETURN.
  ENDIF.

*Fill IDOC control inforamtion
*message type
  ls_idoc_ctrl-mestyp = gc_conda.          "     'COND_A'.
  ls_idoc_ctrl-direct = gc_2.              "      '2'.
*IDOC type
  ls_idoc_ctrl-idoctp = gc_conda04.        "     'COND_A04'.

*Receiver information
*Port
  CONCATENATE gc_sap sy-sysid(3) INTO ls_idoc_ctrl-rcvpor.
*Partner type
  ls_idoc_ctrl-rcvprt = gc_ls.             "     'LS'.
*Logical System
  ls_idoc_ctrl-rcvprn = lv_logsys.

*Sender information
*Port
  ls_idoc_ctrl-sndpor = ls_idoc_ctrl-rcvpor.
*Logical System
  ls_idoc_ctrl-sndprn = lv_logsys.
*Partner Type
  ls_idoc_ctrl-sndprt = gc_ls.


*Filling IDOC data
*--------------------------------------------------------------------*
*KOMG
*--------------------------------------------------------------------*
  ls_idoc_data-segnam    = gc_komg.            "   'E1KOMG'.
  ls_komg_seg = is_komg.
  ls_idoc_data-sdata     = ls_komg_seg.
  APPEND ls_idoc_data TO lt_idoc_data.
  CLEAR: ls_komg_seg,ls_idoc_data.

*--------------------------------------------------------------------*
*KONH
*--------------------------------------------------------------------*
  ls_idoc_data-segnam =  gc_konh.    "   'E1KONH'.
  ls_konh_seg = is_konh.
  ls_idoc_data-sdata = ls_konh_seg.
  APPEND ls_idoc_data TO lt_idoc_data.
  CLEAR: ls_idoc_data,ls_konh_seg.
*--------------------------------------------------------------------*
* KONP
*--------------------------------------------------------------------*
  ls_idoc_data-segnam = gc_konp.               "   'E1KONP'.
  ls_konp_seg = is_konp.
  ls_idoc_data-sdata = ls_konp_seg.
  APPEND ls_idoc_data TO lt_idoc_data.
  CLEAR: ls_idoc_data,ls_konp_seg.


*--------------------------------------------------------------------*
* Function Call for IDOC Generation
*--------------------------------------------------------------------*

  CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
    EXPORTING
      pi_do_handle_error      = abap_true
    IMPORTING
      pe_idoc_number          = ls_idoc_ctrl-docnum
      pe_inbound_process_data = ls_process_data
    TABLES
      t_data_records          = lt_idoc_data
    CHANGING
      pc_control_record       = ls_idoc_ctrl
    EXCEPTIONS
      idoc_not_saved          = 1
      OTHERS                  = 2.
  IF sy-subrc = 0.
* Implement suitable error handling here

    APPEND ls_idoc_ctrl TO lt_idoc_ctrl.
    ls_process_data-mandt  = sy-mandt .     "client
    ls_process_data-evcode = gc_cond.        "Process code
    ls_process_data-edivr2 = gc_6.

*Exporting the IDOC number
    ev_idocnum = ls_idoc_ctrl-docnum.

*--------------------------------------------------------------------*
* Function Call to Start inbound processing
*--------------------------------------------------------------------*

    CALL FUNCTION 'IDOC_START_INBOUND'
      EXPORTING
        pi_inbound_process_data       = ls_process_data
      TABLES
        t_control_records             = lt_idoc_ctrl
      EXCEPTIONS
        invalid_document_number       = 1
        error_before_call_application = 2
        inbound_process_not_possible  = 3
        old_wf_start_failed           = 4
        wf_task_error                 = 5
        serious_inbound_error         = 6
        OTHERS                        = 7.
    IF sy-subrc = 0.
*A COMMIT WORK must be dispatched in the calling program, otherwise the
*IDocs may not be dispatched.
      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.

*Getting the IDOC status(Error/Success/Warning)
      CALL FUNCTION 'ISU_IDOC_GET_STATUS_ICON'
        EXPORTING
          x_docnum   = ev_idocnum
        IMPORTING
          y_stalight = lv_error.
      CASE lv_error.
        WHEN '3'.
          ls_return-type = gc_error.
        WHEN '2'.
          ls_return-type = gc_success.
        WHEN '1'.
          ls_return-type = gc_warning.
      ENDCASE.

*Getting the IDOC message
      CALL FUNCTION 'IDOC_GET_MESSAGE_ATTRIBUTE'
        EXPORTING
          idoc_number  = ev_idocnum
        IMPORTING
          idoc_message = lv_msg.

      ls_return-message = lv_msg.
      APPEND ls_return TO et_return.
      CLEAR ls_return.

    ELSE.
      ls_return-type = sy-msgty.
      ls_return-id = sy-msgid.
      ls_return-number = sy-msgno.
      ls_return-message_v1 = sy-msgv1.
      ls_return-message_v2 = sy-msgv2.
      ls_return-message_v3 = sy-msgv3.
      ls_return-message_v4 = sy-msgv4.
      APPEND ls_return TO et_return.
      CLEAR:ls_return,ev_idocnum.
    ENDIF.
  ELSE.
    ls_return-type = gc_error.
    ls_return-message = TEXT-001."'IDOC not processed'.
    APPEND ls_return TO et_return.
    CLEAR ls_return.
  ENDIF.
ENDFUNCTION.
