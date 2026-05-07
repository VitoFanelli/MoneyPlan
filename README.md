# MoneyPlan

**Versione:** v1.0 — Maggio 2026

App Shiny per la pianificazione finanziaria personale. Permette di registrare entrate e uscite mensili, monitorare l'andamento del capitale nel corso dell'anno e simulare scenari alternativi.

## Funzionalità

- **Dashboard** — KPI mensili, grafico del capitale e tabella riepilogativa annuale per categoria
- **Entrate/Uscite** — inserimento e cancellazione di voci per mese e tipologia
- **Simulazione** — modifica i valori mensili per proiettare scenari futuri
- **Configurazione** — imposta il capitale iniziale e gestisci le tipologie di entrata/uscita

I dati vengono salvati in locale nella cartella `data/` in formato Feather.

## Requisiti

- [R](https://cran.r-project.org/) ≥ 4.0
- Pacchetto `renv` (installato automaticamente al primo avvio)

## Avvio

Esegui `run.bat` con doppio clic.

Al primo avvio `renv` installa automaticamente tutte le dipendenze (serve una connessione ad Internet). Le volte successive l'avvio è immediato e l'app funziona anche offline.

Per chiudere l'app usa il pulsante rosso in alto a destra nell'interfaccia.

NB: se non vedi tutti i dati caricati correttamente all'avvio dell'app, fai un
refresh dei dati tramite l'apposito tasto in alto a destra.
