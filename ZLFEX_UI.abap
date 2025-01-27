*&---------------------------------------------------------------------*
*& Report  ZLFEX_UI
*&---------------------------------------------------------------------*
*& LF 24.01.2025
*& Interfaccia utente per esercizio rifornimento stock
*&---------------------------------------------------------------------*
REPORT ZLFEX_UI.

*&---------------------------------------------------------------------*
* SELECT-OPTIONS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_matnr TYPE zlfex_matnr.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN : COMMENT 32(20) gv_text.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
PARAMETERS: p_qta   TYPE zlfex_order_qt,
            p_date  TYPE datum,
            p_valu  TYPE zlfex_price_curr DEFAULT 'EUR'.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: POSITION POS_LOW.
SELECTION-SCREEN: PUSHBUTTON (15) text-003
                  USER-COMMAND test1.
SELECTION-SCREEN: POSITION POS_HIGH.
SELECTION-SCREEN: PUSHBUTTON (15) text-004
                  USER-COMMAND test2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b02.

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*&---------------------------------------------------------------------*
* Inserire dati per i test da specifica
  IF sy-ucomm = 'TEST1'.
    p_matnr = 'MO_PH_17'.
    p_qta = 12.
    p_date = '20210901'.
  ELSEIF sy-ucomm = 'TEST2'.
    p_matnr = 'MO_PH_17'.
    p_qta = 12.
    p_date = '20211101'.
  ENDIF.

*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*&---------------------------------------------------------------------*
  IF p_matnr IS NOT INITIAL.
    SELECT SINGLE maktx FROM zlfex_mat INTO gv_text
            WHERE matnr = p_matnr.
  ELSE.
    CLEAR: gv_text.
  ENDIF.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  IF p_matnr IS INITIAL OR
     p_qta   IS INITIAL OR
     p_date  IS INITIAL.
    MESSAGE i003(zlfex_msg).
*   Valorizzare tutti i campi di selezione
  ELSE.
    PERFORM get_and_show_data USING p_matnr
                                    p_qta
                                    p_date.
  ENDIF.
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* FORM DEFINITIONS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      FORM get_and_show_data
*&---------------------------------------------------------------------*
FORM get_and_show_data USING u_matnr TYPE zlfex_matnr
                             u_qta   TYPE zlfex_order_qt
                             u_date  TYPE datum.

  DATA: lo_lfex_logic TYPE REF TO zcl_lfex_logic.

  CREATE OBJECT lo_lfex_logic.

  lo_lfex_logic->get_data(
            EXPORTING  i_matnr    = u_matnr
                       i_qty      = u_qta
                       i_ord_date = u_date
                       i_curr     = p_valu
            IMPORTING  et_out     = DATA(lt_alv)
            EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc = 1.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  PERFORM show_alv USING lt_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  show_alv
*&---------------------------------------------------------------------*
FORM show_alv USING ut_alv TYPE ztt_lfex_output_tab.
  DATA: li_alv        TYPE REF TO cl_salv_table,
        li_layout     TYPE REF TO cl_salv_layout,
        ls_layout_key TYPE salv_s_layout_key,
        li_funct      TYPE REF TO cl_salv_functions,
        li_sorts      TYPE REF TO cl_salv_sorts,
        li_columns    TYPE REF TO cl_salv_columns_table,
        li_column     TYPE REF TO cl_salv_column_table,
        lt_alv        TYPE STANDARD TABLE OF zlfex_output_tab_alv.

  MOVE-CORRESPONDING ut_alv[] TO lt_alv[].

* Evidenzio i fornitori più convenienti
  PERFORM color_cells CHANGING lt_alv.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = li_alv
        CHANGING
          t_table      = lt_alv.

*     Attiva la toolbar
      li_funct = li_alv->get_functions( ).
      li_funct->set_all( abap_true ).

*     Ottimizzazione larghezza colonne
      li_columns = li_alv->get_columns( ).
      li_columns->set_optimize( if_salv_c_bool_sap=>true ).

*     Colorazione celle
      li_columns->set_color_column( 'ALV_CELL_COLOUR' ).

      li_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*     Visualizzazione
      li_alv->display( ).

    CATCH cx_root INTO DATA(lx_root).
      MESSAGE 'Errore creando ALV Grid.' TYPE 'E'.
  ENDTRY.

ENDFORM.
*&--------------------------------------------------------------------*
*&      Form  color_cells
*&--------------------------------------------------------------------*
FORM color_cells CHANGING ct_alv TYPE ztt_lfex_output_tab_alv.
* Evidenzio di verde il prezzo (o i prezzi) più conventienti
  DATA: lv_last_price TYPE zlfex_total_price,
        ls_color      TYPE lvc_s_scol.

  CLEAR: lv_last_price.

  LOOP AT ct_alv ASSIGNING FIELD-SYMBOL(<falv>).
    IF lv_last_price IS INITIAL.
      lv_last_price = <falv>-total_price.
    ELSEIF lv_last_price NE <falv>-total_price.
      EXIT.
    ENDIF.
    ls_color-fname = 'LIFNR'.
    ls_color-color-col = 5.   "Verde
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    APPEND ls_color TO <falv>-alv_cell_colour.
    ls_color-fname = 'TOTAL_PRICE'.
    APPEND ls_color TO <falv>-alv_cell_colour.
  ENDLOOP.

ENDFORM.