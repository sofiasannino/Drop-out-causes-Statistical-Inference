Regressione Logistica Senza Parametro Temporale
================
Alessandro Wiget, Sofia Sannino, Pietro Masini, Giulia Riccardi
2024-06-04

## Librerie

``` r
library(readxl)
library(dplyr)
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(BAS)
library(rgl)
library(corrplot)
library(pscl)
library(plm)
#library(glmulti)
library(AICcmodavg)
#library(glmtoolbox)
library(caret)
library(pROC)
library(e1071)
library(stats)
setwd("C:/Users/lenovo/OneDrive/Documenti/R")
```

## Il Dataset

Prima di tutto definiamo la working directory:

IMPORTANTE! Cambiare la directoy a seconda del pc.

Importiamo il Dataset, presente nella cartella `Dati/`:

``` r
setwd("C:/Users/lenovo/OneDrive/Documenti/R")
library(readxl)
df <- read_excel("Dropout20240226_IngMate.xlsx")
#View(df)
```

## Analisi esplorativa

- L’obiettivo è quello di prevedere se una persona lascerà l’università
  o meno, basandosi principalemente sui dati che caratterizzano la sua
  carriera al primo semestre del primo anno. E’ infatti ragionevole
  pensare che tali dati possano contenere elementi predittivi rilevanti,
  ai fini di un’azione mirata a limitare il dropout, il che avviene
  ragionevolemente all’inizio della carriera accademica.

- Dato che la variabile dipendente è bernoulliana
  $\hat{y}_i \in \{0,1\} \lor \hat{p}_i \in [0,1]$, sappiamo che dovremo
  costruire un modello di regressione logistica.

- Consideriamo perciò solo gli studenti con carriere terminate, cioè o
  che si sono laureati o che hanno abbandonato il corso di studio.

``` r
#togliamo covariate inutili, togliamo gli attivi dei quali non sappiamo ancora se hanno droppato o no.
df$career_anonymous_id <- NULL
df$career_time <- NULL
df$stud_career_degree_start_id <- NULL
df$stud_career_degree_changed <- NULL
df$stud_career_degree_name <- NULL
df$stud_ofa_flst <- NULL
df$stud_ofa_fltp <- NULL
df$stud_career_degree_area <- NULL
df$stud_career_degree_code <- NULL
df$stud_career_degree_code_CdS <-NULL
df$highschool_type <- NULL
hist(df$stud_career_admission_age,main="Età di ingresso degli studenti", xlab="Età", ylab="Numero studenti")
```

![](regres_log_con_commenti2---Copia--1-_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
df$stud_career_admission_age<-NULL

df$highschool_type_code <- NULL #abbiamo cancellato queste variabili perchè possiamo separare fra classico, scientifico e altro con un'altra variabile
df$stud_admis_convent_start_dt <- NULL
df$stud_career_end_ay <-NULL
#teniamo solo le persone che non sono attive e delle quali sappiamo già se hanno droppato o no
filtered_df <- df %>% filter(stud_career_status != 'A')
```

- Selezioniamo inizialmente dal dataset le variabili numeriche.

- In particolare togliamo la variabile `career_time` che indica i giorni
  per cui si è stati iscritti al poli, poichè risulta equivalente a
  livello interpretativo alla variabile bernoulliana del dropout, dando
  in questo modo problemi a livello di previsione, oltre ad essere in
  determinati casi fuori contesto per quanto riguarda lo scopo della
  ricerca.

``` r
#selezioniamo solo le variabili numeriche
numerical_vars <- sapply(filtered_df, is.numeric) 
numerical_df <- filtered_df[, numerical_vars]
numerical_df = na.omit(numerical_df)

#analisi esplorativa dei dati: notiamo che ci sono persone che non hanno iniziato il poli e in più ci sono persone che sono iscritte da 3 anni o più 
plot(numerical_df$exa_cfu_pass, numerical_df$career_time_conv,,xlab="cfu sostenuti",ylab="giorni di iscrizione")
abline( h=1200, lty = 2, col = 'red' )
```

![](regres_log_con_commenti2---Copia--1-_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#RITENIAMO A QUESTO PUNTO LA VARIABILE TEMPO NON RILEVANTE PERCHE' A NOI INTERESSA PREVEDERE IL DROPOUT ED EVITARLO, PROGETTANDO UN'AZIONE BASATA SU DATI DEL PRIMO SEMESTRE DEL PRIMO ANNO E PORTATA AVANTI IMMEDIATATAMENTE IL PRIMO ANNO STESSO. IL FATTO CHE LA CARRIERA DURI PIU O MENO E' FUORI CONTESTO: NOI VOGLIAMO CHE Y=0. 
numerical_df = numerical_df[which(!((numerical_df$career_time_conv > 1300)| numerical_df$career_time_conv<0)),] 
#numerical_df = numerical_df[which(!(numerical_df$career_time_conv<0)),]
numerical_df$career_time_conv <- NULL

#View(numerical_df)
```

Osserviamo se esistono correlazioni significative fra i dati numerici:

``` r
X = numerical_df[, -3]
X=X[,-1]
corrplot(cor(X), method='color')
```

![](regres_log_con_commenti2---Copia--1-_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Notiamo una correlazione importante tra `exa_cfu_pass` ovvero i cfu
sostenuti e `exa_grade_average` e tra media esami e tentativi medi
esami.

``` r
min(numerical_df$exa_cfu_pass)
```

    ## [1] 0

``` r
max(numerical_df$exa_cfu_pass)
```

    ## [1] 40

``` r
x  = c( 0, 7, 10, 17, 20, 27, 40) #supporto cfu

# Calcoliamo i punti medi degli intervalli che abbiamo creato
mid = c( ( x [ 2:7 ] + x [ 1:6 ] )/2 )

# Suddividiamo i dati nelle classi che abbiamo creato
GRAGE = cut( numerical_df$exa_cfu_pass, breaks = x, include.lowest = TRUE, right = FALSE )
GRAGE
```

    ##    [1] [0,7)   [7,10)  [27,40] [27,40] [17,20) [0,7)   [27,40] [0,7)   [27,40]
    ##   [10] [10,17) [0,7)   [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40] [27,40]
    ##   [19] [27,40] [10,17) [0,7)   [27,40] [27,40] [27,40] [27,40] [0,7)   [17,20)
    ##   [28] [27,40] [0,7)   [27,40] [27,40] [0,7)   [0,7)   [10,17) [0,7)   [27,40]
    ##   [37] [0,7)   [10,17) [0,7)   [27,40] [17,20) [17,20) [17,20) [27,40] [27,40]
    ##   [46] [20,27) [27,40] [27,40] [27,40] [0,7)   [0,7)   [0,7)   [17,20) [0,7)  
    ##   [55] [0,7)   [0,7)   [0,7)   [0,7)   [27,40] [27,40] [10,17) [27,40] [20,27)
    ##   [64] [27,40] [0,7)   [0,7)   [27,40] [0,7)   [17,20) [27,40] [0,7)   [0,7)  
    ##   [73] [0,7)   [0,7)   [0,7)   [27,40] [27,40] [0,7)   [0,7)   [0,7)   [27,40]
    ##   [82] [0,7)   [27,40] [0,7)   [27,40] [0,7)   [27,40] [17,20) [27,40] [0,7)  
    ##   [91] [17,20) [20,27) [0,7)   [27,40] [27,40] [27,40] [0,7)   [0,7)   [0,7)  
    ##  [100] [27,40] [0,7)   [27,40] [10,17) [27,40] [0,7)   [27,40] [0,7)   [27,40]
    ##  [109] [27,40] [10,17) [27,40] [0,7)   [20,27) [27,40] [0,7)   [27,40] [0,7)  
    ##  [118] [17,20) [27,40] [0,7)   [0,7)   [0,7)   [27,40] [0,7)   [27,40] [17,20)
    ##  [127] [0,7)   [7,10)  [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40]
    ##  [136] [27,40] [0,7)   [0,7)   [0,7)   [0,7)   [0,7)   [10,17) [27,40] [10,17)
    ##  [145] [17,20) [0,7)   [27,40] [17,20) [27,40] [27,40] [27,40] [0,7)   [27,40]
    ##  [154] [27,40] [27,40] [20,27) [0,7)   [27,40] [0,7)   [27,40] [0,7)   [27,40]
    ##  [163] [20,27) [0,7)   [20,27) [17,20) [17,20) [7,10)  [7,10)  [27,40] [0,7)  
    ##  [172] [0,7)   [0,7)   [0,7)   [0,7)   [0,7)   [0,7)   [0,7)   [0,7)   [27,40]
    ##  [181] [0,7)   [20,27) [0,7)   [0,7)   [27,40] [7,10)  [0,7)   [10,17) [0,7)  
    ##  [190] [0,7)   [27,40] [20,27) [27,40] [27,40] [17,20) [27,40] [0,7)   [0,7)  
    ##  [199] [27,40] [20,27) [20,27) [27,40] [27,40] [0,7)   [20,27) [27,40] [0,7)  
    ##  [208] [27,40] [27,40] [17,20) [27,40] [17,20) [0,7)   [17,20) [27,40] [0,7)  
    ##  [217] [27,40] [27,40] [20,27) [27,40] [7,10)  [27,40] [7,10)  [27,40] [27,40]
    ##  [226] [27,40] [10,17) [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40]
    ##  [235] [0,7)   [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40] [27,40] [20,27)
    ##  [244] [0,7)   [27,40] [10,17) [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)  
    ##  [253] [20,27) [27,40] [0,7)   [27,40] [20,27) [27,40] [0,7)   [10,17) [10,17)
    ##  [262] [0,7)   [27,40] [0,7)   [27,40] [17,20) [27,40] [0,7)   [27,40] [0,7)  
    ##  [271] [27,40] [27,40] [27,40] [0,7)   [27,40] [27,40] [0,7)   [0,7)   [27,40]
    ##  [280] [27,40] [0,7)   [27,40] [27,40] [27,40] [27,40] [7,10)  [0,7)   [27,40]
    ##  [289] [10,17) [0,7)   [20,27) [27,40] [27,40] [20,27) [27,40] [17,20) [17,20)
    ##  [298] [0,7)   [0,7)   [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40]
    ##  [307] [17,20) [27,40] [17,20) [27,40] [27,40] [0,7)   [27,40] [10,17) [27,40]
    ##  [316] [27,40] [27,40] [17,20) [20,27) [17,20) [17,20) [27,40] [27,40] [20,27)
    ##  [325] [27,40] [27,40] [17,20) [0,7)   [17,20) [27,40] [27,40] [20,27) [17,20)
    ##  [334] [27,40] [0,7)   [27,40] [27,40] [27,40] [27,40] [27,40] [10,17) [17,20)
    ##  [343] [0,7)   [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40] [27,40] [27,40]
    ##  [352] [27,40] [27,40] [27,40] [0,7)   [7,10)  [27,40] [0,7)   [27,40] [17,20)
    ##  [361] [27,40] [27,40] [0,7)   [27,40] [17,20) [0,7)   [0,7)   [27,40] [0,7)  
    ##  [370] [17,20) [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40] [27,40] [27,40]
    ##  [379] [17,20) [0,7)   [27,40] [27,40] [20,27) [27,40] [17,20) [27,40] [27,40]
    ##  [388] [27,40] [7,10)  [27,40] [0,7)   [27,40] [27,40] [27,40] [17,20) [27,40]
    ##  [397] [27,40] [27,40] [27,40] [0,7)   [0,7)   [10,17) [27,40] [0,7)   [27,40]
    ##  [406] [27,40] [17,20) [27,40] [17,20) [27,40] [10,17) [27,40] [27,40] [27,40]
    ##  [415] [27,40] [0,7)   [0,7)   [27,40] [27,40] [17,20) [27,40] [0,7)   [27,40]
    ##  [424] [0,7)   [17,20) [27,40] [27,40] [27,40] [0,7)   [20,27) [0,7)   [0,7)  
    ##  [433] [27,40] [27,40] [0,7)   [27,40] [27,40] [27,40] [0,7)   [17,20) [27,40]
    ##  [442] [27,40] [27,40] [0,7)   [27,40] [0,7)   [17,20) [7,10)  [0,7)   [27,40]
    ##  [451] [27,40] [27,40] [27,40] [0,7)   [27,40] [27,40] [0,7)   [27,40] [17,20)
    ##  [460] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [17,20)
    ##  [469] [27,40] [0,7)   [0,7)   [27,40] [17,20) [0,7)   [27,40] [0,7)   [27,40]
    ##  [478] [27,40] [27,40] [17,20) [27,40] [10,17) [17,20) [17,20) [27,40] [27,40]
    ##  [487] [17,20) [27,40] [7,10)  [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40]
    ##  [496] [20,27) [27,40] [27,40] [17,20) [17,20) [27,40] [27,40] [27,40] [27,40]
    ##  [505] [17,20) [27,40] [0,7)   [27,40] [27,40] [0,7)   [17,20) [27,40] [27,40]
    ##  [514] [27,40] [17,20) [27,40] [27,40] [17,20) [27,40] [27,40] [27,40] [27,40]
    ##  [523] [10,17) [27,40] [17,20) [17,20) [27,40] [27,40] [27,40] [27,40] [17,20)
    ##  [532] [0,7)   [17,20) [0,7)   [0,7)   [27,40] [27,40] [20,27) [27,40] [27,40]
    ##  [541] [7,10)  [10,17) [0,7)   [20,27) [17,20) [27,40] [27,40] [27,40] [27,40]
    ##  [550] [27,40] [27,40] [27,40] [27,40] [27,40] [17,20) [27,40] [27,40] [17,20)
    ##  [559] [27,40] [27,40] [0,7)   [0,7)   [27,40] [0,7)   [27,40] [27,40] [10,17)
    ##  [568] [10,17) [10,17) [0,7)   [20,27) [27,40] [27,40] [27,40] [0,7)   [27,40]
    ##  [577] [17,20) [17,20) [27,40] [27,40] [27,40] [0,7)   [27,40] [10,17) [7,10) 
    ##  [586] [0,7)   [0,7)   [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40]
    ##  [595] [27,40] [17,20) [27,40] [27,40] [27,40] [27,40] [10,17) [27,40] [27,40]
    ##  [604] [7,10)  [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [20,27)
    ##  [613] [17,20) [27,40] [27,40] [27,40] [7,10)  [27,40] [27,40] [27,40] [0,7)  
    ##  [622] [0,7)   [27,40] [17,20) [27,40] [27,40] [27,40] [17,20) [27,40] [27,40]
    ##  [631] [17,20) [7,10)  [27,40] [27,40] [27,40] [27,40] [0,7)   [17,20) [27,40]
    ##  [640] [0,7)   [0,7)   [27,40] [20,27) [0,7)   [27,40] [27,40] [0,7)   [7,10) 
    ##  [649] [17,20) [27,40] [0,7)   [27,40] [27,40] [27,40] [27,40] [10,17) [27,40]
    ##  [658] [0,7)   [27,40] [27,40] [10,17) [27,40] [27,40] [27,40] [17,20) [10,17)
    ##  [667] [27,40] [17,20) [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [20,27)
    ##  [676] [27,40] [0,7)   [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40]
    ##  [685] [0,7)   [27,40] [27,40] [27,40] [27,40] [17,20) [27,40] [0,7)   [27,40]
    ##  [694] [7,10)  [17,20) [0,7)   [27,40] [17,20) [27,40] [27,40] [0,7)   [0,7)  
    ##  [703] [27,40] [27,40] [10,17) [10,17) [20,27) [27,40] [17,20) [27,40] [20,27)
    ##  [712] [27,40] [27,40] [10,17) [27,40] [27,40] [17,20) [0,7)   [27,40] [20,27)
    ##  [721] [10,17) [17,20) [0,7)   [27,40] [10,17) [27,40] [27,40] [7,10)  [0,7)  
    ##  [730] [27,40] [17,20) [17,20) [0,7)   [27,40] [27,40] [7,10)  [27,40] [17,20)
    ##  [739] [17,20) [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)  
    ##  [748] [17,20) [27,40] [0,7)   [0,7)   [27,40] [27,40] [0,7)   [27,40] [17,20)
    ##  [757] [20,27) [0,7)   [27,40] [17,20) [27,40] [27,40] [17,20) [27,40] [7,10) 
    ##  [766] [27,40] [27,40] [27,40] [17,20) [27,40] [27,40] [0,7)   [17,20) [17,20)
    ##  [775] [0,7)   [27,40] [27,40] [17,20) [27,40] [27,40] [0,7)   [27,40] [27,40]
    ##  [784] [27,40] [17,20) [7,10)  [17,20) [0,7)   [27,40] [0,7)   [17,20) [27,40]
    ##  [793] [7,10)  [27,40] [0,7)   [7,10)  [10,17) [27,40] [27,40] [7,10)  [27,40]
    ##  [802] [27,40] [20,27) [27,40] [0,7)   [27,40] [17,20) [17,20) [0,7)   [17,20)
    ##  [811] [27,40] [17,20) [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)   [7,10) 
    ##  [820] [17,20) [10,17) [27,40] [27,40] [17,20) [27,40] [27,40] [17,20) [10,17)
    ##  [829] [27,40] [0,7)   [27,40] [27,40] [27,40] [20,27) [27,40] [10,17) [17,20)
    ##  [838] [27,40] [27,40] [17,20) [20,27) [17,20) [7,10)  [27,40] [0,7)   [27,40]
    ##  [847] [27,40] [17,20) [27,40] [27,40] [27,40] [0,7)   [0,7)   [27,40] [17,20)
    ##  [856] [27,40] [27,40] [27,40] [27,40] [27,40] [20,27) [27,40] [7,10)  [27,40]
    ##  [865] [27,40] [0,7)   [17,20) [17,20) [27,40] [27,40] [27,40] [0,7)   [27,40]
    ##  [874] [17,20) [10,17) [17,20) [0,7)   [27,40] [27,40] [10,17) [27,40] [27,40]
    ##  [883] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [17,20) [27,40]
    ##  [892] [7,10)  [27,40] [20,27) [27,40] [10,17) [17,20) [0,7)   [10,17) [27,40]
    ##  [901] [27,40] [20,27) [27,40] [10,17) [27,40] [20,27) [27,40] [17,20) [17,20)
    ##  [910] [10,17) [0,7)   [27,40] [27,40] [17,20) [10,17) [17,20) [17,20) [27,40]
    ##  [919] [17,20) [0,7)   [17,20) [17,20) [27,40] [17,20) [27,40] [27,40] [27,40]
    ##  [928] [27,40] [17,20) [7,10)  [27,40] [7,10)  [0,7)   [27,40] [27,40] [27,40]
    ##  [937] [0,7)   [10,17) [27,40] [17,20) [27,40] [27,40] [27,40] [20,27) [27,40]
    ##  [946] [0,7)   [27,40] [27,40] [0,7)   [17,20) [17,20) [17,20) [17,20) [27,40]
    ##  [955] [27,40] [17,20) [27,40] [27,40] [27,40] [17,20) [17,20) [0,7)   [17,20)
    ##  [964] [27,40] [17,20) [20,27) [27,40] [20,27) [0,7)   [10,17) [0,7)   [0,7)  
    ##  [973] [7,10)  [27,40] [10,17) [27,40] [0,7)   [10,17) [10,17) [27,40] [0,7)  
    ##  [982] [27,40] [27,40] [27,40] [17,20) [17,20) [27,40] [27,40] [0,7)   [27,40]
    ##  [991] [27,40] [27,40] [27,40] [17,20) [27,40] [27,40] [7,10)  [20,27) [27,40]
    ## [1000] [10,17) [17,20) [27,40] [0,7)   [17,20) [17,20) [10,17) [7,10)  [10,17)
    ## [1009] [27,40] [17,20) [27,40] [27,40] [0,7)   [27,40] [27,40] [27,40] [20,27)
    ## [1018] [0,7)   [7,10)  [17,20) [17,20) [27,40] [27,40] [27,40] [27,40] [27,40]
    ## [1027] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)   [17,20) [27,40]
    ## [1036] [7,10)  [27,40] [17,20) [27,40] [27,40] [27,40] [10,17) [27,40] [20,27)
    ## [1045] [17,20) [0,7)   [27,40] [10,17) [0,7)   [27,40] [17,20) [17,20) [7,10) 
    ## [1054] [0,7)   [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40]
    ## [1063] [7,10)  [0,7)   [27,40] [27,40] [27,40] [7,10)  [27,40] [27,40] [27,40]
    ## [1072] [27,40] [27,40] [27,40] [0,7)   [7,10)  [27,40] [27,40] [27,40] [17,20)
    ## [1081] [17,20) [27,40] [27,40] [27,40] [17,20) [27,40] [27,40] [20,27) [20,27)
    ## [1090] [27,40] [17,20) [0,7)   [27,40] [0,7)   [27,40] [10,17) [27,40] [17,20)
    ## [1099] [27,40] [7,10)  [10,17) [27,40] [0,7)   [27,40] [27,40] [27,40] [27,40]
    ## [1108] [27,40] [27,40] [10,17) [0,7)   [20,27) [7,10)  [27,40] [27,40] [27,40]
    ## [1117] [27,40] [0,7)   [27,40] [0,7)   [27,40] [27,40] [17,20) [17,20) [27,40]
    ## [1126] [27,40] [27,40] [10,17) [27,40] [17,20) [27,40] [17,20) [27,40] [27,40]
    ## [1135] [27,40] [27,40] [27,40] [27,40] [27,40] [20,27) [0,7)   [17,20) [27,40]
    ## [1144] [10,17) [27,40] [27,40] [27,40] [0,7)   [10,17) [27,40] [20,27) [27,40]
    ## [1153] [27,40] [27,40] [27,40] [0,7)   [27,40] [27,40] [27,40] [20,27) [0,7)  
    ## [1162] [20,27) [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40]
    ## [1171] [27,40] [0,7)   [20,27) [27,40] [0,7)   [27,40] [27,40] [7,10)  [10,17)
    ## [1180] [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40] [17,20) [27,40] [27,40]
    ## [1189] [27,40] [27,40] [27,40] [27,40] [0,7)   [7,10)  [27,40] [27,40] [17,20)
    ## [1198] [17,20) [17,20) [27,40] [0,7)   [27,40] [27,40] [27,40] [27,40] [17,20)
    ## [1207] [10,17) [27,40] [17,20) [27,40] [27,40] [27,40] [17,20) [27,40] [17,20)
    ## [1216] [27,40] [0,7)   [7,10)  [17,20) [27,40] [27,40] [0,7)   [27,40] [27,40]
    ## [1225] [27,40] [27,40] [27,40] [0,7)   [20,27) [27,40] [27,40] [27,40] [27,40]
    ## [1234] [20,27) [7,10)  [27,40] [0,7)   [17,20) [27,40] [27,40] [7,10)  [27,40]
    ## [1243] [0,7)   [27,40] [17,20) [0,7)   [27,40] [27,40] [17,20) [27,40] [27,40]
    ## [1252] [10,17) [20,27) [27,40] [27,40] [27,40] [20,27) [0,7)   [0,7)   [27,40]
    ## [1261] [27,40] [20,27) [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [17,20)
    ## [1270] [10,17) [17,20) [17,20) [27,40] [27,40] [17,20) [0,7)   [7,10)  [27,40]
    ## [1279] [17,20) [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40] [27,40] [27,40]
    ## [1288] [27,40] [27,40] [0,7)   [27,40] [27,40] [17,20) [27,40] [20,27) [27,40]
    ## [1297] [27,40] [27,40] [27,40] [27,40] [7,10)  [27,40] [27,40] [0,7)   [27,40]
    ## [1306] [10,17) [17,20) [27,40] [27,40] [0,7)   [27,40] [27,40] [27,40] [10,17)
    ## [1315] [27,40] [27,40] [17,20) [27,40] [27,40] [27,40] [17,20) [27,40] [27,40]
    ## [1324] [27,40] [27,40] [10,17) [17,20) [27,40] [27,40] [0,7)   [27,40] [20,27)
    ## [1333] [0,7)   [10,17) [10,17) [27,40] [0,7)   [27,40] [27,40] [27,40] [7,10) 
    ## [1342] [20,27) [20,27) [0,7)   [27,40] [27,40] [27,40] [27,40] [27,40] [27,40]
    ## [1351] [0,7)   [0,7)   [27,40] [27,40] [10,17) [27,40] [27,40] [27,40] [27,40]
    ## [1360] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [17,20) [27,40] [27,40]
    ## [1369] [27,40] [27,40] [17,20) [27,40] [17,20) [17,20) [27,40] [27,40] [20,27)
    ## [1378] [27,40] [17,20) [0,7)   [0,7)   [27,40] [27,40] [27,40] [27,40] [27,40]
    ## [1387] [20,27) [27,40] [27,40] [27,40] [27,40] [27,40] [7,10)  [7,10)  [20,27)
    ## [1396] [0,7)   [17,20) [17,20) [27,40] [27,40] [10,17) [17,20) [27,40] [27,40]
    ## [1405] [10,17) [27,40] [0,7)   [27,40] [27,40] [17,20) [27,40] [10,17) [27,40]
    ## [1414] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [10,17) [20,27)
    ## [1423] [27,40] [0,7)   [27,40] [10,17) [27,40] [27,40] [27,40] [27,40] [27,40]
    ## [1432] [20,27) [27,40] [27,40] [10,17) [27,40] [17,20) [0,7)   [0,7)   [27,40]
    ## [1441] [27,40] [0,7)   [27,40] [7,10)  [10,17) [27,40] [27,40] [27,40] [20,27)
    ## [1450] [10,17) [27,40] [27,40] [27,40] [17,20) [17,20) [27,40] [0,7)   [0,7)  
    ## [1459] [27,40] [27,40] [0,7)   [10,17) [0,7)   [27,40] [20,27) [27,40] [20,27)
    ## [1468] [0,7)   [27,40] [27,40] [0,7)   [10,17) [17,20) [27,40] [27,40] [27,40]
    ## [1477] [27,40] [0,7)   [27,40] [27,40] [27,40] [27,40] [17,20) [27,40] [17,20)
    ## [1486] [17,20) [27,40] [0,7)   [27,40] [10,17) [27,40] [27,40] [27,40] [27,40]
    ## [1495] [27,40] [0,7)   [27,40] [27,40] [20,27) [10,17) [27,40] [0,7)   [27,40]
    ## [1504] [27,40] [20,27) [27,40] [27,40] [20,27) [10,17) [17,20) [0,7)   [27,40]
    ## [1513] [0,7)   [27,40] [27,40] [27,40] [17,20) [27,40] [20,27) [10,17) [27,40]
    ## [1522] [7,10)  [27,40] [27,40] [27,40] [0,7)   [27,40] [20,27) [17,20) [27,40]
    ## [1531] [10,17) [27,40] [27,40] [27,40] [10,17) [17,20) [27,40] [27,40] [27,40]
    ## [1540] [27,40] [27,40] [10,17) [0,7)   [27,40] [10,17) [27,40] [0,7)   [27,40]
    ## [1549] [27,40] [17,20) [27,40] [27,40] [27,40] [0,7)   [20,27) [17,20) [27,40]
    ## [1558] [17,20) [0,7)   [27,40] [27,40] [20,27) [10,17) [0,7)   [17,20) [27,40]
    ## [1567] [27,40] [27,40] [20,27) [27,40] [17,20) [0,7)   [27,40] [27,40] [27,40]
    ## [1576] [17,20) [27,40] [27,40] [20,27) [20,27) [27,40] [27,40] [27,40] [27,40]
    ## [1585] [17,20) [20,27) [17,20) [27,40] [27,40] [27,40] [20,27) [27,40] [27,40]
    ## [1594] [27,40] [0,7)   [20,27) [0,7)   [27,40] [20,27) [10,17) [27,40] [10,17)
    ## [1603] [27,40] [27,40] [17,20) [27,40] [27,40] [27,40] [27,40] [0,7)   [27,40]
    ## [1612] [27,40] [27,40] [17,20) [27,40] [10,17) [27,40] [10,17) [27,40] [20,27)
    ## [1621] [17,20) [27,40] [27,40] [27,40] [27,40] [27,40] [0,7)   [20,27) [17,20)
    ## [1630] [27,40] [0,7)   [27,40] [27,40] [10,17) [0,7)   [20,27) [10,17) [27,40]
    ## [1639] [20,27) [20,27) [27,40] [27,40] [27,40] [10,17) [10,17) [27,40] [20,27)
    ## [1648] [20,27) [10,17) [27,40] [20,27) [0,7)   [27,40] [20,27) [10,17) [27,40]
    ## [1657] [27,40] [27,40] [27,40] [17,20) [10,17) [27,40] [10,17) [10,17) [27,40]
    ## [1666] [20,27) [10,17) [0,7)   [20,27) [27,40] [27,40] [0,7)   [10,17) [20,27)
    ## [1675] [27,40] [20,27) [27,40] [27,40] [27,40] [20,27) [0,7)   [7,10)  [17,20)
    ## [1684] [20,27) [17,20) [27,40] [20,27) [20,27) [10,17) [27,40] [27,40] [10,17)
    ## [1693] [20,27) [0,7)   [27,40] [27,40] [0,7)   [27,40] [10,17) [27,40] [27,40]
    ## [1702] [17,20) [27,40] [0,7)   [27,40] [10,17) [10,17) [27,40] [27,40] [27,40]
    ## [1711] [0,7)   [0,7)   [20,27) [10,17) [27,40] [20,27) [27,40] [27,40] [0,7)  
    ## [1720] [27,40] [27,40] [27,40] [27,40] [10,17) [27,40] [27,40] [27,40] [20,27)
    ## [1729] [27,40] [27,40] [17,20) [17,20) [20,27) [0,7)   [20,27) [0,7)   [10,17)
    ## [1738] [27,40] [27,40] [0,7)   [27,40] [20,27) [27,40] [0,7)   [27,40] [0,7)  
    ## [1747] [20,27) [0,7)   [27,40] [27,40] [27,40] [27,40] [27,40] [27,40] [10,17)
    ## [1756] [10,17) [27,40] [27,40] [0,7)   [0,7)   [20,27) [27,40] [27,40] [27,40]
    ## [1765] [10,17) [27,40] [17,20) [27,40] [10,17) [27,40] [10,17) [27,40] [27,40]
    ## [1774] [27,40] [20,27) [10,17) [10,17) [20,27) [20,27) [17,20) [10,17) [20,27)
    ## [1783] [20,27) [27,40] [27,40] [27,40] [27,40] [27,40] [20,27) [27,40] [27,40]
    ## [1792] [27,40] [17,20) [0,7)   [27,40] [10,17) [10,17) [27,40] [27,40] [17,20)
    ## [1801] [20,27) [27,40] [17,20) [10,17) [27,40] [27,40] [10,17) [0,7)   [27,40]
    ## [1810] [20,27) [0,7)   [17,20) [0,7)   [0,7)   [10,17) [27,40] [27,40] [10,17)
    ## [1819] [0,7)   [27,40] [27,40] [20,27) [27,40] [10,17) [27,40] [27,40] [27,40]
    ## [1828] [0,7)   [27,40] [20,27) [20,27) [27,40] [10,17) [10,17) [27,40] [27,40]
    ## [1837] [20,27) [27,40] [0,7)   [27,40] [0,7)   [27,40] [0,7)   [20,27) [27,40]
    ## [1846] [27,40] [0,7)   [27,40] [27,40] [20,27) [27,40] [27,40] [0,7)   [27,40]
    ## [1855] [27,40] [20,27) [27,40] [20,27) [27,40] [10,17) [0,7)   [10,17) [27,40]
    ## [1864] [0,7)   [0,7)   [10,17) [27,40] [0,7)   [17,20) [0,7)   [10,17) [20,27)
    ## [1873] [10,17) [27,40] [27,40] [27,40] [20,27) [10,17) [17,20) [17,20)
    ## Levels: [0,7) [7,10) [10,17) [17,20) [20,27) [27,40]

``` r
# Calcoliamo quindi la media della variabile AGE stratificata e sovrapponiamo 
# i valori di y al grafico precedente.

y = tapply( numerical_df$dropout, GRAGE, mean )
y
```

    ##      [0,7)     [7,10)    [10,17)    [17,20)    [20,27)    [27,40] 
    ## 0.96562500 0.39285714 0.54014599 0.10619469 0.11538462 0.02274975

``` r
#Aggiungere legenda
plot( numerical_df$exa_cfu_pass, numerical_df$dropout, pch = ifelse( numerical_df$dropout  == 1, 3, 4 ),
col = ifelse( numerical_df$dropout== 1, 'forestgreen', 'red' ),
xlab = 'cfu', ylab = 'dropout', main = 'cfu vs. dropout', lwd = 2, cex = 1.5 )
points( mid, y, col = "blue", pch = 16 )
```

![](regres_log_con_commenti2---Copia--1-_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Notiamo qualitativamente che al diminuire dei cfu acquisiti nel primo
semestre aumenta la probabilità di dropout.

## La Prima Regressione Logistica

Effettuiamo una prima regressione logistica fra le variabili numeriche
del dataset:

``` r
# Create a formula for linear model
formula_num <- as.formula(paste("dropout ~", paste(names(numerical_df[,-which(names(numerical_df) == "dropout")]), collapse = " + ")))

# Fit the linear model
model_init <- glm(formula_num, data = numerical_df, family=binomial( link = logit ))

# Print the summary of the model
summary(model_init)
```

    ## 
    ## Call:
    ## glm(formula = formula_num, family = binomial(link = logit), data = numerical_df)
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          41.129387  65.675483   0.626   0.5311    
    ## career_start_ay      -0.017726   0.032480  -0.546   0.5852    
    ## stud_admission_score -0.002410   0.010167  -0.237   0.8126    
    ## exa_cfu_pass         -0.168700   0.014922 -11.305  < 2e-16 ***
    ## exa_grade_average    -0.115663   0.019018  -6.082 1.19e-09 ***
    ## exa_avg_attempts      0.307325   0.238562   1.288   0.1977    
    ## highschool_grade     -0.019237   0.009355  -2.056   0.0398 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2107.76  on 1879  degrees of freedom
    ## Residual deviance:  816.45  on 1873  degrees of freedom
    ## AIC: 830.45
    ## 
    ## Number of Fisher Scoring iterations: 6

Le covariate con p-value rilevanti sono:

- exa_cfu_pass: il numero di cfu sostenuti nel primo semestre

- exa_grade_average: la media dei voti del primo semestre

- leggermente rilevante anche highschool_grade

Notiamo inoltre che il modello presenta molte covariate non
significative, pertanto operiamo una ricerca backward per eliminare
quelle non rilevanti.

Valutiamo l’Odd Ratio per un incremento di 10 cfu e di un punto di media
e valutiamo di quanto varia il rischio di dropout.

$\text{Aggiungere formula odds ratio}$

``` r
exp(10*coef(model_init)[5])
```

    ## exa_grade_average 
    ##         0.3145433

``` r
exp(coef(model_init)[6])
```

    ## exa_avg_attempts 
    ##         1.359782

Notiamo che il rischio di dropout diminuisce dell’ottanta percento
all’aumentare di 10 cfu e allo stesso modo diminuisce del 10 percento
all’aumentare di un punto di media.

``` r
covariate = paste("dropout ~", paste(names(numerical_df[,-which(names(numerical_df) == "dropout")]), collapse = " + "))

#Covariate rimosse durante la semplificazione, in ordine

formula_num <- as.formula(covariate)

# Fit the linear model
model_back <- glm(formula_num, data = numerical_df, family=binomial)

# Print the summary of the model
drop1(model_back, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ career_start_ay + stud_admission_score + exa_cfu_pass + 
    ##     exa_grade_average + exa_avg_attempts + highschool_grade
    ##                      Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                    816.45 830.45                      
    ## career_start_ay       1   816.74 828.74   0.297   0.58567    
    ## stud_admission_score  1   816.50 828.50   0.056   0.81266    
    ## exa_cfu_pass          1   962.74 974.74 146.293 < 2.2e-16 ***
    ## exa_grade_average     1   856.65 868.65  40.201 2.291e-10 ***
    ## exa_avg_attempts      1   818.11 830.11   1.660   0.19760    
    ## highschool_grade      1   820.63 832.63   4.185   0.04078 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
AIC(model_back)
```

    ## [1] 830.447

``` r
#--------------------------------
model_back = update(model_back, . ~ . -stud_admission_score )

# Print the summary of the model
drop1(model_back, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ career_start_ay + exa_cfu_pass + exa_grade_average + 
    ##     exa_avg_attempts + highschool_grade
    ##                   Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                 816.50 828.50                      
    ## career_start_ay    1   816.75 826.75   0.247   0.61949    
    ## exa_cfu_pass       1   965.82 975.82 149.313 < 2.2e-16 ***
    ## exa_grade_average  1   857.09 867.09  40.588  1.88e-10 ***
    ## exa_avg_attempts   1   818.27 828.27   1.771   0.18325    
    ## highschool_grade   1   820.82 830.82   4.312   0.03784 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
AIC(model_back)
```

    ## [1] 828.5032

``` r
#-----------------------------
model_back = update(model_back, . ~ . - career_start_ay)

# Print the summary of the model
drop1(model_back, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average + exa_avg_attempts + 
    ##     highschool_grade
    ##                   Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                 816.75 826.75                      
    ## exa_cfu_pass       1   966.24 974.24 149.492 < 2.2e-16 ***
    ## exa_grade_average  1   857.26 865.26  40.511 1.955e-10 ***
    ## exa_avg_attempts   1   818.49 826.49   1.736   0.18759    
    ## highschool_grade   1   821.27 829.27   4.516   0.03358 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
AIC(model_back)
```

    ## [1] 826.7497

``` r
#-------------------------------
model_back = update(model_back, . ~ . - stud_career_admission_age)

# Print the summary of the model
drop1(model_back, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average + exa_avg_attempts + 
    ##     highschool_grade
    ##                   Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                 816.75 826.75                      
    ## exa_cfu_pass       1   966.24 974.24 149.492 < 2.2e-16 ***
    ## exa_grade_average  1   857.26 865.26  40.511 1.955e-10 ***
    ## exa_avg_attempts   1   818.49 826.49   1.736   0.18759    
    ## highschool_grade   1   821.27 829.27   4.516   0.03358 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
AIC(model_back)
```

    ## [1] 826.7497

``` r
#-------------------------------
model_back = update(model_back, . ~ . - exa_avg_attempts )

# Print the summary of the model
drop1(model_back, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average + highschool_grade
    ##                   Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                 818.49 826.49                      
    ## exa_cfu_pass       1   967.13 973.13 148.641 < 2.2e-16 ***
    ## exa_grade_average  1   862.97 868.97  44.479 2.571e-11 ***
    ## highschool_grade   1   823.67 829.67   5.182   0.02283 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
AIC(model_back)
```

    ## [1] 826.4862

``` r
#-------------------------------
model_back_2 = update(model_back, . ~ . - highschool_grade)

# Print the summary of the model
drop1(model_back, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average + highschool_grade
    ##                   Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                 818.49 826.49                      
    ## exa_cfu_pass       1   967.13 973.13 148.641 < 2.2e-16 ***
    ## exa_grade_average  1   862.97 868.97  44.479 2.571e-11 ***
    ## highschool_grade   1   823.67 829.67   5.182   0.02283 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
AIC(model_back)
```

    ## [1] 826.4862

``` r
drop1(model_back_2, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average
    ##                   Df Deviance    AIC    LRT  Pr(>Chi)    
    ## <none>                 823.67 829.67                     
    ## exa_cfu_pass       1   986.72 990.72 163.05 < 2.2e-16 ***
    ## exa_grade_average  1   870.03 874.03  46.36 9.841e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
AIC(model_back)
```

    ## [1] 826.4862

``` r
#-------------------------------
```

Il modello risultante presenta due covariate numeriche significative: la
media esami relativa al primo semestre e i cfu sostenuti al primo
semestre. Abbiamo anche una significatività meno rilevante di
highschool_grade (al 5%).

``` r
anova(model_back_2, model_back, test="Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: dropout ~ exa_cfu_pass + exa_grade_average
    ## Model 2: dropout ~ exa_cfu_pass + exa_grade_average + highschool_grade
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
    ## 1      1877     823.67                       
    ## 2      1876     818.49  1   5.1816  0.02283 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Il test anova ci dà un p-value basso, perciò effettivamente i due
modelli sono diversi e il secondo, quello con una covariata in più,
porta una significatività intrinseca rilevante.

``` r
mod=glm("dropout~1+exa_cfu_pass+exa_grade_average+highschool_grade",data = numerical_df, family=binomial)
summary(mod)
```

    ## 
    ## Call:
    ## glm(formula = "dropout~1+exa_cfu_pass+exa_grade_average+highschool_grade", 
    ##     family = binomial, data = numerical_df)
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        5.473043   0.838854   6.524 6.83e-11 ***
    ## exa_cfu_pass      -0.167997   0.014626 -11.487  < 2e-16 ***
    ## exa_grade_average -0.104008   0.017211  -6.043 1.51e-09 ***
    ## highschool_grade  -0.021203   0.009246  -2.293   0.0218 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2107.76  on 1879  degrees of freedom
    ## Residual deviance:  818.49  on 1876  degrees of freedom
    ## AIC: 826.49
    ## 
    ## Number of Fisher Scoring iterations: 6

## Introduzione Interazioni fra Variabili Numeriche

Prima di procedere con l’analisi delle variabili categoriche cerchiamo
di comprendere se le aggiunte di interazioni fra variabili numeriche ci
permettono di migliorare il nostro modello. Osservando la matrice delle
covariate costruita all’inizio si rileva che `exa_cfu_pass` e
`exa_grade_average` sono molto correlate, pertanto aggiungo il termine
di interazione al modello e successivamente valuto l’equivalenza o meno
dei due modelli tramite il test ANOVA ($\chi^2$):

``` r
mod_int = update(mod, . ~ . + exa_cfu_pass*exa_grade_average)

summary(mod_int)
```

    ## 
    ## Call:
    ## glm(formula = dropout ~ exa_cfu_pass + exa_grade_average + highschool_grade + 
    ##     exa_cfu_pass:exa_grade_average, family = binomial, data = numerical_df)
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                     4.674151   0.859873   5.436 5.45e-08 ***
    ## exa_cfu_pass                   -0.037988   0.048964  -0.776   0.4378    
    ## exa_grade_average              -0.087164   0.016953  -5.141 2.73e-07 ***
    ## highschool_grade               -0.015864   0.009292  -1.707   0.0878 .  
    ## exa_cfu_pass:exa_grade_average -0.005669   0.002091  -2.712   0.0067 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2107.76  on 1879  degrees of freedom
    ## Residual deviance:  810.78  on 1875  degrees of freedom
    ## AIC: 820.78
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
anova(mod,mod_int)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: dropout ~ 1 + exa_cfu_pass + exa_grade_average + highschool_grade
    ## Model 2: dropout ~ exa_cfu_pass + exa_grade_average + highschool_grade + 
    ##     exa_cfu_pass:exa_grade_average
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
    ## 1      1876     818.49                        
    ## 2      1875     810.78  1   7.7043 0.005509 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Il p-value della covariata relativa a `exa_cfu_pass` è alto pertanto il
termine di interazione è da escludere in quanto non aggiunge nulla al
modello.

## Introduzione delle Variabili Categoriche

Rendiamo tutte le variabili del Dataset di tipo `factor` affinchè siano
utilizzabili nella regressione logistica.

``` r
MODEL = mod

filtered_df <- df %>% filter(stud_career_status != 'A')
filtered_df_no_na = na.omit(filtered_df)

#Partendo dal modello di ottimo trovato prima costruisco la matrice solo con quelle covariate:
cat_df <- filtered_df_no_na

cat_no_lev_df = cat_df[which(!((cat_df$career_time_conv > 1300)| cat_df$career_time_conv<0)),]
#cat_no_lev_df=cat_df[which(!(cat_df$career_time_conv<0)),]
cat_no_lev_df$career_time_conv <- NULL

cat_no_lev_df$stud_gender = factor(cat_no_lev_df$stud_gender, ordered=F)
cat_no_lev_df$previousStudies = factor(cat_no_lev_df$previousStudies, ordered=F)
cat_no_lev_df$origins = factor(cat_no_lev_df$origins, ordered=F)
cat_no_lev_df$income_bracket_normalized_on4 = factor(cat_no_lev_df$income_bracket_normalized_on4, ordered=F)


cat_no_lev_df$dropped_on_180<-NULL
cat_no_lev_df$stud_career_status <-NULL


#View(cat_no_lev_df)
```

Aggiorno il modello considerando le variabili categoriche, facendo un
test di backward. Consideriamo una soglia del 5 percento.

``` r
model_cat = glm("dropout ~ 1 + 
    exa_cfu_pass + exa_grade_average  +stud_gender + previousStudies + origins + income_bracket_normalized_on4+ highschool_grade", data=cat_no_lev_df, family=binomial)

drop1(model_cat, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ 1 + exa_cfu_pass + exa_grade_average + stud_gender + 
    ##     previousStudies + origins + income_bracket_normalized_on4 + 
    ##     highschool_grade
    ##                               Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                             801.76 829.76                      
    ## exa_cfu_pass                   1   941.66 967.66 139.909 < 2.2e-16 ***
    ## exa_grade_average              1   850.81 876.81  49.058 2.484e-12 ***
    ## stud_gender                    1   804.01 830.01   2.254   0.13326    
    ## previousStudies                3   809.16 831.16   7.405   0.06005 .  
    ## origins                        3   804.34 826.34   2.581   0.46091    
    ## income_bracket_normalized_on4  3   806.05 828.05   4.295   0.23128    
    ## highschool_grade               1   804.93 830.93   3.178   0.07461 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_cat = update(model_cat,  . ~ . - origins)
drop1(model_cat, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average + stud_gender + previousStudies + 
    ##     income_bracket_normalized_on4 + highschool_grade
    ##                               Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                             804.34 826.34                      
    ## exa_cfu_pass                   1   944.90 964.90 140.564 < 2.2e-16 ***
    ## exa_grade_average              1   853.02 873.02  48.685 3.006e-12 ***
    ## stud_gender                    1   806.67 826.67   2.330   0.12691    
    ## previousStudies                3   812.20 828.20   7.865   0.04889 *  
    ## income_bracket_normalized_on4  3   808.79 824.79   4.450   0.21678    
    ## highschool_grade               1   807.19 827.19   2.853   0.09121 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_cat = update(model_cat,  . ~ . - income_bracket_normalized_on4)
drop1(model_cat, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average + stud_gender + previousStudies + 
    ##     highschool_grade
    ##                   Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                 808.79 824.79                      
    ## exa_cfu_pass       1   951.75 965.75 142.961 < 2.2e-16 ***
    ## exa_grade_average  1   855.12 869.12  46.330 9.993e-12 ***
    ## stud_gender        1   811.28 825.28   2.497   0.11409    
    ## previousStudies    3   816.13 826.13   7.349   0.06158 .  
    ## highschool_grade   1   811.84 825.84   3.058   0.08035 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_cat = update(model_cat,  . ~ . - stud_gender)
drop1(model_cat, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average + previousStudies + 
    ##     highschool_grade
    ##                   Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                 811.28 825.28                      
    ## exa_cfu_pass       1   952.63 964.63 141.352 < 2.2e-16 ***
    ## exa_grade_average  1   856.75 868.75  45.471 1.549e-11 ***
    ## previousStudies    3   818.49 826.49   7.204   0.06568 .  
    ## highschool_grade   1   815.56 827.56   4.276   0.03865 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(model_cat,mod)
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: dropout ~ exa_cfu_pass + exa_grade_average + previousStudies + 
    ##     highschool_grade
    ## Model 2: dropout ~ 1 + exa_cfu_pass + exa_grade_average + highschool_grade
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
    ## 1      1873     811.28                       
    ## 2      1876     818.49 -3  -7.2036  0.06568 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model_cat = update(model_cat,  . ~ . - previousStudies)
drop1(model_cat, test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## dropout ~ exa_cfu_pass + exa_grade_average + highschool_grade
    ##                   Df Deviance    AIC     LRT  Pr(>Chi)    
    ## <none>                 818.49 826.49                      
    ## exa_cfu_pass       1   967.13 973.13 148.641 < 2.2e-16 ***
    ## exa_grade_average  1   862.97 868.97  44.479 2.571e-11 ***
    ## highschool_grade   1   823.67 829.67   5.182   0.02283 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(model_cat)
```

    ## 
    ## Call:
    ## glm(formula = dropout ~ exa_cfu_pass + exa_grade_average + highschool_grade, 
    ##     family = binomial, data = cat_no_lev_df)
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        5.473043   0.838854   6.524 6.83e-11 ***
    ## exa_cfu_pass      -0.167997   0.014626 -11.487  < 2e-16 ***
    ## exa_grade_average -0.104008   0.017211  -6.043 1.51e-09 ***
    ## highschool_grade  -0.021203   0.009246  -2.293   0.0218 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2107.76  on 1879  degrees of freedom
    ## Residual deviance:  818.49  on 1876  degrees of freedom
    ## AIC: 826.49
    ## 
    ## Number of Fisher Scoring iterations: 6

Le variabili categoriche non risultano rilevanti nel modello, pertanto
rimane il modello di partenza con le sole varibaili numeriche rilevate
in precedenza.

## Confusion Matrix e ROC Curve

Eseguiamo ora la classificazione.

- Computiamo la confusion matrix.

- Splittiamo il dataset in due parti: il training set che comprende
  l’80% dei dati, il test set il restante 20%. Rifittiamo il modello sul
  training set e facciamo predizione sul test set.

- Costruiamo la curva ROC e valutiamo la soglia ottimale.

``` r
# Assume you have a dataframe data with predictors and a response variable
# split the data into train and test
set.seed(123)
trainIndex <- createDataPartition(cat_no_lev_df$dropout, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

train <- cat_no_lev_df[ trainIndex,]
test  <- cat_no_lev_df[-trainIndex,]

# Fit the logistic regression model
fit <- glm("dropout ~ 1 + exa_cfu_pass + exa_grade_average + highschool_grade", data = train, family = binomial)

# Make predictions on the test set
predicted_probs <- predict(fit, newdata = test, type = "response")


# Compute the ROC curve
roc_obj <- roc(test$dropout, predicted_probs)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
# Plot the ROC curve
plot(roc_obj, print.auc = TRUE)
```

![](regres_log_con_commenti2---Copia--1-_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# Compute the AUC
auc <- auc(roc_obj)
print(auc)
```

    ## Area under the curve: 0.9471

``` r
#trovare soglia opt
best=coords(roc_obj, "best", ret="threshold", best.method="youden") #best circa 0.2
print(best)
```

    ##   threshold
    ## 1 0.2181341

``` r
#
test$dropout=as.factor(test$dropout)

# Compute the confusion matrix
predicted_classes <- ifelse(predicted_probs > as.numeric(best), 1, 0)
predicted_classes=as.factor(predicted_classes)

cm <- confusionMatrix(predicted_classes, test$dropout)
print(cm)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 266  11
    ##          1  24  75
    ##                                           
    ##                Accuracy : 0.9069          
    ##                  95% CI : (0.8729, 0.9343)
    ##     No Information Rate : 0.7713          
    ##     P-Value [Acc > NIR] : 5.045e-12       
    ##                                           
    ##                   Kappa : 0.7495          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.04252         
    ##                                           
    ##             Sensitivity : 0.9172          
    ##             Specificity : 0.8721          
    ##          Pos Pred Value : 0.9603          
    ##          Neg Pred Value : 0.7576          
    ##              Prevalence : 0.7713          
    ##          Detection Rate : 0.7074          
    ##    Detection Prevalence : 0.7367          
    ##       Balanced Accuracy : 0.8947          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

L’indice AUC, la sensitività, la specificità e la precisione sono tutti
valori più che soddisfacenti.

## Predizione sugli studenti in corso:

Costruiamo il dataset degli studenti attivi, quindi quelli ancora in
corso:

``` r
attivi_df <- df %>% filter(stud_career_status == 'A')
attivi_df$dropout <- NULL

attivi_df$stud_career_end_date <- NULL
attivi_df = na.omit(attivi_df)
#attivi_df = attivi_df[which(!((attivi_df$career_time_conv > 1000 & attivi_df$exa_cfu_pass==0) | attivi_df$career_start_ay!=2023 | attivi_df$career_time_conv<177)),]
attivi_df = attivi_df[which((attivi_df$career_start_ay==2023)|(attivi_df$career_start_ay==2022)|(attivi_df$career_start_ay==2021)),]
#attivi_df$stud_gender = factor(attivi_df$stud_gender, ordered=F)
#attivi_df$previousStudies = factor(attivi_df$previousStudies, ordered=F)

#View(attivi_df)
```

Il modello ottimale è il seguente:

``` r
model_opt = glm("dropout ~ 1 + exa_cfu_pass + exa_grade_average ", data = train, family=binomial)
summary(model_opt)
```

    ## 
    ## Call:
    ## glm(formula = "dropout ~ 1 + exa_cfu_pass + exa_grade_average ", 
    ##     family = binomial, data = train)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        4.21179    0.43195   9.751  < 2e-16 ***
    ## exa_cfu_pass      -0.17317    0.01621 -10.683  < 2e-16 ***
    ## exa_grade_average -0.12761    0.02173  -5.874 4.26e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1702.40  on 1503  degrees of freedom
    ## Residual deviance:  636.75  on 1501  degrees of freedom
    ## AIC: 642.75
    ## 
    ## Number of Fisher Scoring iterations: 6

Eseguiamo la predizione sugli studenti attivi presenti nel nostro
modello, utilizzando il valore di threshold precedentemente trovato:

``` r
predizione <- predict(model_opt, newdata = attivi_df, type = "response")

binary_output <- ifelse(predizione > round(as.numeric(best),1), 1, 0)

attivi_df$dropout_prediction = binary_output

sum(binary_output)/length(binary_output)
```

    ## [1] 0.2962963

``` r
#View(attivi_df)
```

La probabilità di dropout stimata è del 37% circa, che sovrastima quella
rilevata sugli studenti a carriera conclusa (mean(predicted_probs)=25.63
%). Questo è ragionevole considerando che l’obiettivo è prevenire il
dropout, quindi in un’ottica di prevenzione, una sovrastima è
accettabile.


## K-Fold cross Validation
``` r
set.seed(123) # For reproducibility
# Define number of folds
K <- 10
sensitivity_value1=rep(0,10)
sensitivity_value2=rep(0,10)
sensitivity_value3=rep(0,10)
sensitivity_value4=rep(0,10)
# Shuffle the data
cat_no_lev_df <- cat_no_lev_df[sample(nrow(cat_no_lev_df)),]
#View(cat_no_lev_df)
# Create K equally sized folds
folds <- cut(seq(1, nrow(cat_no_lev_df)), breaks=K, labels=FALSE)


# Perform K-fold cross-validation
for(i in 1:K) {
  # Segment the data by fold using the which() function
  test_indices <- which(folds == i, arr.ind=TRUE)
  test_data <- cat_no_lev_df[test_indices, ]
  train_data <- cat_no_lev_df[-test_indices, ]
  # Train your model (using a simple logistic regression for this example)
 model1 = glm("dropout ~ 1 + exa_cfu_pass + exa_grade_average ", data = train_data, family = binomial)
model2 = glm("dropout ~ 1 + exa_cfu_pass + exa_grade_average+highschool_grade", data = train_data, family = binomial)
model3 = glm("dropout ~ 1 + exa_cfu_pass + exa_grade_average + highschool_grade + exa_cfu_pass*exa_grade_average", data = train_data, family = binomial)
model4 = glm("dropout ~ 1 + exa_cfu_pass + exa_grade_average + previousStudies + highschool_grade", data=train_data, family=binomial)

  
  # Make predictions on the test data
  predictions1 <- predict(model1, test_data, type="response")
  predictions2 <- predict(model2, test_data, type="response")
  predictions3 <- predict(model3, test_data, type="response")
  predictions4 <- predict(model4, test_data, type="response")
  roc_obj1 <- roc(test_data$dropout, predictions1)
  b1=coords(roc_obj1, "best", ret="threshold", best.method="youden")
  roc_obj2 <- roc(test_data$dropout, predictions2)
  b2=coords(roc_obj2, "best", ret="threshold", best.method="youden")
  roc_obj3 <- roc(test_data$dropout, predictions3)
  b3=coords(roc_obj3, "best", ret="threshold", best.method="youden")
  roc_obj4 <- roc(test_data$dropout, predictions4)
  b4=coords(roc_obj4, "best", ret="threshold", best.method="youden")
  
  predicted_classes1 <- ifelse(as.numeric(predictions1) > as.numeric(b1), 1, 0)
  predicted_classes2 <- ifelse(as.numeric(predictions2) > as.numeric(b2), 1, 0)
  predicted_classes3 <- ifelse(as.numeric(predictions3) > as.numeric(b3), 1, 0)
  predicted_classes4 <- ifelse(as.numeric(predictions4) > as.numeric(b4), 1, 0)
  predicted_classes1=as.factor(predicted_classes1)
  predicted_classes2=as.factor(predicted_classes2)
  predicted_classes3=as.factor(predicted_classes3)
  predicted_classes4=as.factor(predicted_classes4)
  test_data$dropout=as.factor(test_data$dropout)
  cm1 <- confusionMatrix(predicted_classes1, test_data$dropout)
  cm2<- confusionMatrix(predicted_classes2, test_data$dropout)
  cm3<- confusionMatrix(predicted_classes3, test_data$dropout)
  cm4<- confusionMatrix(predicted_classes4, test_data$dropout)
  sensitivity_value1[i] <- cm1$byClass["Sensitivity"]
  sensitivity_value2[i] <- cm2$byClass["Sensitivity"]
  sensitivity_value3[i] <- cm3$byClass["Sensitivity"]
  sensitivity_value4[i] <- cm4$byClass["Sensitivity"]
}
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
boxplot(sensitivity_value1,sensitivity_value2,sensitivity_value3,sensitivity_value4,xlab="Modelli", ylab="Sensitività")
```

![](regres_log_con_commenti2---Copia--1-_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
