# 1. Guida avvio
Dopo aver importato la CR nel sistema SAP, il programma può essere avviato attraverso la transazione ZLFEX.

Per compilare in automatico i dati per eseguire i due test definiti in specifica, premere rispettivamente i tasti "Test 1" e "Test 2".

Per eseguire l'estrazione dati premere il tasto F8 della tastiera, oppure il tasto con l'icona di orologino in alto a sinistra.

# 2. Struttura programma
Questa architettura è così pensata in modo da rendere indipendente la logica e l'UI.

La transazione ZLFEX esegue il report ZLFEX_UI che fa da interfaccia utente, tutta la logica è gestita dalla classe ZCL_LFEX_LOGIC il cui metodo pubblico GET_DATA riceve come input:
- codice articolo
- quantità
- data ordine
- valuta (default €)
E come output:
- tabella con i dati, la prima riga è il fornitore più conveniente
Gli altri metodi privati eseguono operazioni atomiche, in modo da semplificare eventuali modifiche alla logica, nello specifico:
- GET_SUPPLIERS_STOCK_PRICE -> estrae i dati iniziali del materiale, stock e prezzo per i fornitori
- GET_SUPPLIERS_DISCOUNTS -> estrae i dati degli sconti relativi ai fornitori coinvolti
- CALCULATE_TOTAL -> calcola il prezzo totale senza sconti
- APPLY_DISCOUNTS -> verifica e applica gli sconti che soddisfano i requisiti
- CALCULATE_DISCOUNT -> calcola il valore degli sconti

Esiste anche il metodo CREATE_EXERCISE_DATA che permette di cancellare e creare i dati nelle varie tabelle database replicando le condizioni descritte nella specifica.

La classe ZCL_LFEX_LOGIC possiede anche un costruttore che chiama il metodo CREATE_EXERCISE_DATA se una tabella dati dell'esercizio risulta vuota.

I messaggi sono definiti nella classe messaggi ZLFEX_MSG

# 2.1 Naming convention
Il nome degli oggetti creati presentano il suffisso "LF", le iniziali del mio nome e cognome, ed "EX" che sta per exercise.

# 3. Struttura database
Le tabelle coinvolte sono 5:
- ZLFEX_MAT -> la lista degli articoli con dati di anagrafica
	- codice articolo (chiave)
	- descrizione
	- unità di misura di base
- ZLFEX_SUPP -> la lista dei fornitori con dati di anagrafica
	- codice fornitore (chiave)
	- descrizione
- ZLFEX_PRICE -> i prezzi articoli relativi ai fornitori
	- codice fornitore (chiave)
	- codice articolo (chiave)
	- prezzo unitario
	- Valuta del prezzo
- ZLFEX_DISC -> definisce gli sconti dei fornitori, un fornitore può avere da 0...N sconti
 	- codice fornitore (chiave)
	- codice sconto (chiave)
	- sconto in percentuale
	- minima spesa
	- minima qta
	- data inizio sconto
	- data fine sconto
- ZLFEX_SUP_STOCK -> lo stock disponibile dai fornitori
	- codice fornitore (chiave)
	- codice articolo (chiave)
	- qta in stock

# 3.1 Spiegazione tabella sconti
Uno sconto ha tre condizioni: spesa, quantità, data di validità, le tre condizioni sono mutualmente esclusive.

Se due sconti con la STESSA condizione sono soddisfatti, viene attivato solo lo sconto maggiore, ad esempio:

Se c'è uno sconto del 5% con una spesa minima di 100€ ed uno sconto del 10% con una spesa minima di 200€, a fronte di una spesa è di 200€ viene attivato solo lo sconto del 10%.

Le condizioni sono:
- minima spesa -> definisce la spesa minima in un ordine, se raggiunta, lo sconto viene attivato
- minima qta -> definisce la quantità minima in un ordine, se raggiunta, lo sconto viene attivato
- periodo -> definisce la data (GG.MM.AAAA) di validità dello sconto, se la data ordine è compresa tra i due estremi, lo sconto viene attivato

# 4. Scelte tecniche
La specifica non richiede la gestione dello stock del negozio, perciò non viene gestito.

Allo stesso modo, non è richiesta la gestione degli ordini nè il prelievo dai fornitori, perciò non viene modificato lo stock dei fornitori in database, anche per semplicità di ripetibilità dei test.
