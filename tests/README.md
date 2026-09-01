# Strategia di test di `flipmeta`

## Obiettivo

La suite verifica che `flipmeta` implementi correttamente il test score con
sign flipping e che i risultati restino affidabili quando vengono combinati
più modelli meta-analitici.

I test sono volutamente deterministici, brevi e separati per responsabilità.
Usano piccoli dataset e matrici di flip fissate in anticipo. Le simulazioni di
potenza e controllo dell'errore di tipo I restano analisi scientifiche separate:
sono utili per il paper, ma sarebbero lente e potenzialmente instabili durante
il normale controllo del pacchetto.

## Come eseguire i test

Dalla cartella principale del pacchetto:

```r
testthat::test_local()
```

Per verificare anche installazione, documentazione, dipendenze ed esempi:

```sh
R CMD build . --no-build-vignettes --no-manual
R CMD check flipmeta_0.0.1.tar.gz --no-manual --no-build-vignettes
```

## Validazione matematica

Il file `testthat/helper-mathematical-oracle.R` contiene un piccolo oracolo
indipendente dal codice del pacchetto. L'oracolo usa soltanto algebra matriciale
di base per calcolare:

1. la stima di eterogeneità sotto l'ipotesi nulla;
2. i pesi meta-analitici;
3. i residui del modello nullo;
4. la residualizzazione pesata del predittore testato;
5. i contributi individuali allo score;
6. score e varianze per ogni configurazione di flip;
7. statistiche standardizzate e p-value finale.

`testthat/test-mathematical-validation.R` confronta questi risultati con
`flipmeta` per i metodi `EE`, `DL`, `ML` e `REML`. Verifica inoltre:

- il caso in cui viene testata soltanto l'intercetta e non restano covariate
  nuisance;
- il mantenimento di un valore di `tau2` fissato nel modello originale;
- l'equivalenza con `flipscores` nel caso speciale con varianze uguali e
  modello equal-effects;
- la coerenza tra `scores`, `Tspace`, statistiche osservate e p-value riportati;
- l'invarianza delle statistiche standardizzate rispetto a un cambio coerente
  della scala di effect size e varianze;
- un errore esplicito quando una varianza flip-specifica è nulla e la
  standardizzazione non è definita.

Il confronto con `flipscores` è limitato al caso in cui i due modelli sono
realmente equivalenti. Non viene usato come oracolo generale per
meta-regressioni con varianze campionarie differenti.

## Identificatori e analisi multiverse

`testthat/test-observation-identifiers.R` verifica il ruolo dell'argomento
`id`, in particolare:

- modelli costruiti con le stesse osservazioni in ordine differente;
- specificazioni che escludono alcuni studi;
- specificazioni parzialmente sovrapposte e riordinate;
- assegnazione dello stesso segno allo stesso studio in tutti i modelli;
- contributo zero degli studi assenti da una specificazione;
- equivalenza tra il risultato congiunto e l'analisi diretta della singola
  specificazione con gli stessi flip;
- invarianza rispetto all'ordine delle colonne di una matrice di flip dotata di
  nomi;
- recupero corretto degli indici usati da `metafor::rma.uni(subset = ...)`;
- errori per identificatori mancanti, duplicati o non disponibili nei dati.

Questi controlli proteggono il punto più delicato di una multiverse
meta-analysis: uno studio deve mantenere la stessa trasformazione casuale in
ogni specificazione in cui compare.

## Correzioni multiple

`testthat/test-p-adjust.R` controlla la validazione degli input e confronta la
correzione max-T step-down con un calcolo manuale costruito direttamente da un
`Tspace` noto. Verifica anche le correzioni effettuate separatamente entro
famiglie definite con `by` e la proprietà `p.adj >= p`.

## API e casi limite

Gli altri file coprono senza replicare i controlli matematici:

- `test-input-validation.R`: classe del modello, numero di flip, matrici di
  flip, metodi, tolleranze, controlli numerici e metadati `extra`;
- `test-tested-coeffs.R`: selezione e validazione dei coefficienti nei modelli
  singoli e nelle liste;
- `test-model-names.R`: nomi mancanti, parziali o duplicati;
- `test-presentation.R`: metodi `print`, `summary` e `plot`, scelta automatica
  tra p-value grezzi e corretti e trasformazioni grafiche;
- `test-spec-curve.R`: costruzione della specification curve con o senza
  variabili descrittive aggiuntive e relativi errori informativi;
- `test-transf-p.R`: trasformazioni `raw`, `-log10`, `z` e funzioni definite
  dall'utente.

## Scelte intenzionali

- Nessun test dipende da internet o da un seed casuale.
- Non vengono confrontati grandi snapshot numerici: si confrontano quantità
  matematiche mirate, così un errore è facile da localizzare.
- Non vengono chiamate funzioni interne di `flipscores` tramite `:::`.
- Le simulazioni estese non fanno parte della suite ordinaria. Possono essere
  eseguite separatamente come validazione statistica periodica.
- Un valore di `tau2` fissato viene conservato nei modelli nulli quando non si
  richiede un metodo differente. Un override esplicito del metodo richiede
  invece un nuovo fit; `method = "EE"` fissa `tau2` a zero.
- Le varianze flip-specifiche numericamente degeneri producono un errore:
  eliminare quei flip cambierebbe silenziosamente il denominatore del p-value.

## Stato verificato

La suite contiene 40 gruppi di test. Al termine dell'implementazione:

- tutti i test `testthat` passano;
- il pacchetto viene costruito dal sorgente;
- `R CMD check` sul tarball termina con `Status: OK`.
