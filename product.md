# MoneyPlan

## 1. Introduzione

Questa applicazione consente di monitorare il proprio capitale in base alle entrate e uscite ricorrenti mensili e fare simulazioni.
Non ha lo scopo di registrare ogni minima transazione ma consente una visione più ad alto livello.
Impostando le proprie entrate e uscite mensili, permette di conoscere il risparmio o le perdite mensili
e di pianificare eventuali uscite straordinarie simulando ciò che accadrebbe al proprio capitale.
I mesi sono di un anno generico.

## 2. Requisiti

1. Inserimento entrate per tipologia (stipendio, bonus, welfare, extra), mese e euro
2. Inserimento uscite per tipologia (alimentari, automobile, mutuo, scuola, sport, vacanze, svago, videogames), mese e euro
3. Inserimento capitale di partenza
4. Grafico a barre del totale entrate e uscite per mese
5. Box con valore di risparmio mensile e annuale (diffrenza tra totale entrate e totale uscite)
6. Calcolo capitale a fine anno
7. Simulazione entrate e uscite e calcolo risparmio e capitale in base ai dati simulati
8. Salvataggio dati su file in formato .feather

## 3. Framework

L'applicazione è implementata in linguaggio R con framework shiny/rhino.
Utilizza le seguenti librerie:

- bslib: componenti (es. card, box, ecc.)
- highcharter: grafici interattivi
- dplyr, tidyr: manipolazione dati
- feather: salvataggio dati
- shinyWidgets: componenti di input
- bs4Dash: tasti di inserimento/cancellazione
