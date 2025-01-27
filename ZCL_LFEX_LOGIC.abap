class ZCL_LFEX_LOGIC definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods GET_DATA
    importing
      value(I_MATNR) type MATNR
      value(I_QTY) type ZLFEX_ORDER_QT
      value(I_ORD_DATE) type DATUM
      value(I_CURR) type ZLFEX_PRICE_CURR default 'EUR'
    exporting
      value(ET_OUT) type ZTT_LFEX_OUTPUT_TAB
    exceptions
      NO_DATA
      BAD_QTY
      INVALID_DATE
      NO_CURR .
  methods CREATE_EXERCISE_DATA
    importing
      value(I_DELETE_OLD_DATA) type FLAG optional
      value(I_CREATE_DATA) type FLAG default 'X' .
protected section.
private section.

  methods GET_SUPPLIERS_STOCK_PRICE
    importing
      value(I_MATNR) type MATNR
      value(I_QTY) type ZLFEX_ORDER_QT
    exporting
      value(ET_OUT) type ZTT_LFEX_OUTPUT_TAB .
  methods GET_SUPPLIERS_DISCOUNTS
    importing
      value(IT_DATA_TAB) type ZTT_LFEX_OUTPUT_TAB
    exporting
      value(ET_OUT) type ZTT_LFEX_DISC .
  methods CALCULATE_TOTAL
    importing
      value(I_QTY) type ZLFEX_ORDER_QT
      value(I_ORD_DATE) type DATUM
      value(I_CURR) type ZLFEX_PRICE_CURR optional
    changing
      value(CT_DATA_TAB) type ZTT_LFEX_OUTPUT_TAB .
  methods APPLY_DISCOUNTS
    importing
      value(I_ORD_DATE) type DATUM
    changing
      value(CT_DISCOUNT) type ZTT_LFEX_DISC
      value(CT_DATA_TAB) type ZTT_LFEX_OUTPUT_TAB .
  methods CALCULATE_DISCOUNT
    importing
      value(I_DISC_PERC) type ZLFEX_DISC_PERC
    changing
      value(C_DATA) type ZLFEX_OUTPUT_TAB .
ENDCLASS.



CLASS ZCL_LFEX_LOGIC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_LFEX_LOGIC->APPLY_DISCOUNTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ORD_DATE                     TYPE        DATUM
* | [<-->] CT_DISCOUNT                    TYPE        ZTT_LFEX_DISC
* | [<-->] CT_DATA_TAB                    TYPE        ZTT_LFEX_OUTPUT_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method APPLY_DISCOUNTS.
*   Applica gli sconti al prezzo totale, se soddisfano i requisiti
*   dettati dalla tabella degli sconti fornitori.
*   Gli sconti non sono additivi, ed in caso di presenza di più
*   sconti, vanno applicati uno dopo l'altro.
    LOOP AT ct_data_tab ASSIGNING FIELD-SYMBOL(<fdata>).
*     Le condizioni sconto sono mutualmente esclusive, ma se una stessa
*     condizione ha due o più righe che soddisfano i requisiti devo
*     considerare solo lo sconto più alto.
*     Perciò ordino la tabella per il valore di sconto percentuale
*     e appena una condizione è soddisfatta passo alla prossima categoria.
      SORT ct_discount BY disc_perc DESCENDING.
*     Sconto su totale spesa minima
      LOOP AT ct_discount INTO DATA(ls_discount)
          WHERE lifnr  = <fdata>-lifnr
            AND min_tot IS NOT INITIAL.
        IF <fdata>-total_price >= ls_discount-min_tot.
*         condizione sconto soddisfatta
          me->calculate_discount(
                  EXPORTING i_disc_perc = ls_discount-disc_perc
                   CHANGING c_data      = <fdata> ).
          EXIT.
        ENDIF.
      ENDLOOP.
*     Sconto su quantità minima
      LOOP AT ct_discount INTO ls_discount
          WHERE lifnr  = <fdata>-lifnr
            AND min_qt IS NOT INITIAL.
        IF <fdata>-order_qty >= ls_discount-min_qt.
*         condizione sconto soddisfatta
          me->calculate_discount(
                  EXPORTING i_disc_perc = ls_discount-disc_perc
                   CHANGING c_data      = <fdata> ).
          EXIT.
        ENDIF.
      ENDLOOP.
*     Sconto in base alla data
      LOOP AT ct_discount INTO ls_discount
          WHERE lifnr  = <fdata>-lifnr
            AND disc_from IS NOT INITIAL
            AND disc_to   IS NOT INITIAL.
        IF i_ord_date >= ls_discount-disc_from AND
           i_ord_date <= ls_discount-disc_to.
*         condizione sconto soddisfatta
          me->calculate_discount(
                  EXPORTING i_disc_perc = ls_discount-disc_perc
                   CHANGING c_data      = <fdata> ).
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_LFEX_LOGIC->CALCULATE_DISCOUNT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DISC_PERC                    TYPE        ZLFEX_DISC_PERC
* | [<-->] C_DATA                         TYPE        ZLFEX_OUTPUT_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CALCULATE_DISCOUNT.
*   Calcola lo sconto
    DATA: lv_discount TYPE zlfex_discount.
    lv_discount = ( c_data-total_price / 100 ) * i_disc_perc.
    ADD lv_discount TO c_data-discount.
    SUBTRACT lv_discount FROM c_data-total_price.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_LFEX_LOGIC->CALCULATE_TOTAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_QTY                          TYPE        ZLFEX_ORDER_QT
* | [--->] I_ORD_DATE                     TYPE        DATUM
* | [--->] I_CURR                         TYPE        ZLFEX_PRICE_CURR(optional)
* | [<-->] CT_DATA_TAB                    TYPE        ZTT_LFEX_OUTPUT_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CALCULATE_TOTAL.
*   Calcola il prezzo totale in base alla quantità ordine e prezzi fornitore.
*   Esegue anche la conversione in una valuta comune.
    DATA: lv_date TYPE datum.

    IF i_ord_date > sy-datum.
      lv_date = sy-datum.
    ELSE.
      lv_date = i_ord_date.
    ENDIF.
    LOOP AT ct_data_tab ASSIGNING FIELD-SYMBOL(<fdata>).
      IF i_curr NE <fdata>-curr.
        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
          EXPORTING
            date                    = lv_date
            foreign_amount          = <fdata>-price
            foreign_currency        = <fdata>-curr
            local_currency          = i_curr
          IMPORTING
            LOCAL_AMOUNT            = <fdata>-price
          EXCEPTIONS
            NO_RATE_FOUND           = 1
            OVERFLOW                = 2
            NO_FACTORS_FOUND        = 3
            NO_SPREAD_FOUND         = 4
            DERIVED_2_TIMES         = 5
            OTHERS                  = 6.
        IF sy-subrc = 0.
          <fdata>-curr = i_curr.
        ENDIF.
*       Se la conversione non ha successo, lascio il valore
*       della valuta non convertito.
      ENDIF.
      <fdata>-order_qty = i_qty.
      <fdata>-total_price = <fdata>-price * i_qty.
    ENDLOOP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LFEX_LOGIC->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.
*   Se la tabella degli articoli è vuota, allora riempio le tabelle
*   con i dati di test.
    SELECT SINGLE * FROM zlfex_mat INTO @DATA(ls_mat).
    IF sy-subrc NE 0.
      me->create_exercise_data( i_create_data = 'X' ).
    ENDIF.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LFEX_LOGIC->CREATE_EXERCISE_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DELETE_OLD_DATA              TYPE        FLAG(optional)
* | [--->] I_CREATE_DATA                  TYPE        FLAG (default ='X')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CREATE_EXERCISE_DATA.
*   Creazione dei dati nella specifica dell'esercizio.
    DATA: lt_mat       TYPE STANDARD TABLE OF zlfex_mat,
          lt_supp      TYPE STANDARD TABLE OF zlfex_supp,
          lt_price     TYPE STANDARD TABLE OF zlfex_price,
          lt_disc      TYPE STANDARD TABLE OF zlfex_disc,
          lt_sup_stock TYPE STANDARD TABLE OF zlfex_sup_stock.

*   Se il flag I_DELETE_OLD_DATA è = 'X' cancello i dati precedenti.
    IF i_delete_old_data = 'X'.
      DELETE FROM zlfex_mat.
      DELETE FROM zlfex_supp.
      DELETE FROM zlfex_price.
      DELETE FROM zlfex_disc.
      DELETE FROM zlfex_sup_stock.
      COMMIT WORK AND WAIT.
    ENDIF.

*   Se il flag I_CREATE_DATA non è 'X' i dati non vengono creati, utile
*   per quando si vuole solo cancellare i dati già presenti.
    IF I_CREATE_DATA NE 'X'.
      RETURN.
    ENDIF.

*   Dalla specifica:
*   Supplier 1 has 8 pcs in stock at 120€ each, and offers 5% discount for purchases of
*     minimum 1000€. Min. days to ship order is 5
*   Supplier 2 has 15 pcs in stock at 128€ each, and offers a 3% discount if you order >5pcs and
*     5% discount if you order >10pcs. Min. days to ship order is 7
*   Supplier 3 has 23 pcs in stock at 129€ each, and offers a discount of 5% for orders over
*     1000€. It also offers an additional discount of 2% for orders placed in september. Min. days
*     to ship order is 4
*   Aggiungo anche qualche altro dato per altri test

**************************************************************
*   DATI MATERIALI
**************************************************************
    lt_mat = VALUE #(
      ( matnr = 'MO_PH_17' maktx = 'Philips monitor 17”' meins = 'ST' )
      ( matnr = 'MO_SM_18' maktx = 'Samsung monitor 18”' meins = 'ST' )
      ( matnr = 'MO_SM_16' maktx = 'Samsung monitor 16”' meins = 'ST' ) ).
**************************************************************
*   DATI FORNITORI
**************************************************************
    lt_supp = VALUE #(
      ( lifnr = '1' descr = 'Supplier 1' min_days = 5 )
      ( lifnr = '2' descr = 'Supplier 2' min_days = 7 )
      ( lifnr = '3' descr = 'Supplier 3' min_days = 4 ) ).
**************************************************************
*   DATI PREZZO FORNITORI
**************************************************************
    lt_price = VALUE #(
      ( lifnr = '1' matnr = 'MO_PH_17' price = 120 curr  = 'EUR' )
      ( lifnr = '2' matnr = 'MO_PH_17' price = 128 curr  = 'EUR' )
      ( lifnr = '3' matnr = 'MO_PH_17' price = 129 curr  = 'EUR' )
      ( lifnr = '1' matnr = 'MO_SM_16' price = 100 curr  = 'EUR' )
      ( lifnr = '2' matnr = 'MO_SM_16' price = 100 curr  = 'USD' )
      ( lifnr = '3' matnr = 'MO_SM_16' price = 129 curr  = 'EUR' ) ).
**************************************************************
*   DATI STOCK FORNITORI
**************************************************************
    lt_sup_stock = VALUE #(
      ( lifnr = '1' matnr = 'MO_PH_17' stock = 8 )
      ( lifnr = '2' matnr = 'MO_PH_17' stock = 15 )
      ( lifnr = '3' matnr = 'MO_PH_17' stock = 23 )
      ( lifnr = '1' matnr = 'MO_SM_16' stock = 11 )
      ( lifnr = '2' matnr = 'MO_SM_16' stock = 10 )
      ( lifnr = '3' matnr = 'MO_SM_16' stock = 20 ) ).
**************************************************************
*   DATI SCONTI FORNITORI
**************************************************************
    lt_disc = VALUE #(
      ( lifnr = '1' disc_code = '1' disc_perc = 5 min_tot = 1000 )
      ( lifnr = '2' disc_code = '1' disc_perc = 3 min_qt = 5 )
      ( lifnr = '2' disc_code = '2' disc_perc = 5 min_qt = 10 )
      ( lifnr = '3' disc_code = '1' disc_perc = 5 min_tot = '1000.01' )
      ( lifnr = '3' disc_code = '2' disc_perc = 2 disc_from = '20210901' disc_to = '20210930' ) ).

*   Inserisco i dati nel database
    MODIFY zlfex_mat FROM TABLE lt_mat.
    MODIFY zlfex_supp FROM TABLE lt_supp.
    MODIFY zlfex_price FROM TABLE lt_price.
    MODIFY zlfex_disc FROM TABLE lt_disc.
    MODIFY zlfex_sup_stock FROM TABLE lt_sup_stock.
    COMMIT WORK AND WAIT.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LFEX_LOGIC->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MATNR                        TYPE        MATNR
* | [--->] I_QTY                          TYPE        ZLFEX_ORDER_QT
* | [--->] I_ORD_DATE                     TYPE        DATUM
* | [--->] I_CURR                         TYPE        ZLFEX_PRICE_CURR (default ='EUR')
* | [<---] ET_OUT                         TYPE        ZTT_LFEX_OUTPUT_TAB
* | [EXC!] NO_DATA
* | [EXC!] BAD_QTY
* | [EXC!] INVALID_DATE
* | [EXC!] NO_CURR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_DATA.
******* CONTROLLI INPUT
*   Controllo sulla quantità
    IF i_qty <= 0.
      MESSAGE e000(zlfex_msg) RAISING bad_qty.
*     La quantità non può essere negativa o zero
    ENDIF.

*   Controllo sulla valuta
    SELECT SINGLE * FROM tcurc INTO @DATA(ls_tcurc)
            WHERE waers = @i_curr.
    IF sy-subrc NE 0.
      MESSAGE e004(zlfex_msg) WITH i_curr RAISING no_curr.
*     La valuta & non esiste
    ENDIF.

*   Il controllo sulla data è già eseguito dal data element,
*   ma è comunque possibile inviare dati errati
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                            = i_ord_date
      EXCEPTIONS
        PLAUSIBILITY_CHECK_FAILED       = 1
        OTHERS                          = 2.
    IF sy-subrc <> 0.
      MESSAGE e001(zlfex_msg) WITH i_ord_date RAISING invalid_date.
*     La data ordine & non è valida
    ENDIF.
******* END CONTROLLI INPUT

*   Estrazione dati iniziali
    me->get_suppliers_stock_price(
            EXPORTING i_matnr = i_matnr
                      i_qty   = i_qty
            IMPORTING et_out  = et_out ).

*   Se l'estrazione è vuota mando un errore
    IF et_out[] IS INITIAL.
      MESSAGE e002(zlfex_msg) RAISING no_data.
*     Nessun dato estratto
    ENDIF.

*   Estrazione sconti fornitori rilevanti, l'estrazione può
*   anche essere vuota.
    me->get_suppliers_discounts(
            EXPORTING it_data_tab = et_out
            IMPORTING et_out      = DATA(lt_discounts) ).

*   Calcolo il prezzo totale per ogni fornitore
    me->calculate_total(
            EXPORTING i_qty       = i_qty
                      i_ord_date  = i_ord_date
                      i_curr      = i_curr
             CHANGING ct_data_tab = et_out ).

    IF lt_discounts[] IS NOT INITIAL.
*     Applicazione sconti che soddisfano i requisiti
      me->apply_discounts(
              EXPORTING i_ord_date  = i_ord_date
               CHANGING ct_discount = lt_discounts
                        ct_data_tab = et_out ).
    ENDIF.

*   Il fornitore più conventiente è presentato all'inizio della lista
    SORT et_out BY total_price min_days lifnr ASCENDING.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_LFEX_LOGIC->GET_SUPPLIERS_DISCOUNTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DATA_TAB                    TYPE        ZTT_LFEX_OUTPUT_TAB
* | [<---] ET_OUT                         TYPE        ZTT_LFEX_DISC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_SUPPLIERS_DISCOUNTS.
*   Estrazione degli sconti fornitori
    SELECT * FROM zlfex_disc INTO TABLE @et_out
        FOR ALL ENTRIES IN @it_data_tab
            WHERE lifnr = @it_data_tab-lifnr.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_LFEX_LOGIC->GET_SUPPLIERS_STOCK_PRICE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MATNR                        TYPE        MATNR
* | [--->] I_QTY                          TYPE        ZLFEX_ORDER_QT
* | [<---] ET_OUT                         TYPE        ZTT_LFEX_OUTPUT_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_SUPPLIERS_STOCK_PRICE.
*   Estraggo i dati iniziali dalle tabelle di anagrafica.
*   In questo passaggio vengono esclusi automaticamente i fornitori
*   che non hanno abbastanza stock per soddisfare l'ordine.
    SELECT * FROM zlfex_mat as mat
       INNER JOIN zlfex_sup_stock as stock
               ON mat~matnr = stock~matnr
       INNER JOIN zlfex_supp as supp
               ON stock~lifnr = supp~lifnr
       INNER JOIN zlfex_price as price
               ON supp~lifnr = price~lifnr
              AND mat~matnr  = price~matnr
       INTO CORRESPONDING FIELDS OF TABLE @et_out
       WHERE mat~matnr = @i_matnr
         AND stock~stock >= @i_qty.
  endmethod.
ENDCLASS.