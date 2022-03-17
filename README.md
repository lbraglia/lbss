[![Build Status](https://travis-ci.org/lbraglia/lbss.svg)](https://travis-ci.org/lbraglia/lbss)

# ROADMAP

- inferenza
- google: "sample size software review" "sample size software comparison"
- installare tutti i software di sample size
  (pass, nqueryadvisor, machin, chow, altri maggiori)
  http://biostat.mc.vanderbilt.edu/wiki/Main/PowerSampleSize

# Feature di un buon software di sample size

- capillarità delle procedure coperte;
- software gratuito;
- disponibilità del codice (verifica);
- framework espandibile (senza la necessità di creare nuovi
  applicativi ex-novo per disegni specifici) potenzialmente da parte
  di chiunque;
- classificazione tag-based delle procedure (non gerarchica)
- validazione
- documentazione (manuali, esempi)

# Linguaggio scelto: R

- piu domestichezza io;
- piu domestichezza statistici che possono espandere forndendo plugin
  (a contrario di Python, magari più general purpose, ma molto più
  comune nella bioinformatica piuttosto che nella comunità statistica);
- si beneficia di una infrastruttura di distribuzione e installazione
  del sw già presente (e ben funzionante);
- layer sottostante risolve problemi di portabilità e uniformità su
   tutti i maggiori sistemi out of the box.
- relativamente semplice implementare un sistema
  base + plugin (similmente a Rcmdr)
- già numerosi pacchetti di sample size esistenti che possono essere wrappati
  in plugin

# Revisione package R

- https://cran.r-project.org/web/packages/rpanel/
- https://cran.r-project.org/web/packages/binomSamSize/
- https://cran.r-project.org/web/packages/samplesize/
- https://cran.r-project.org/web/packages/Sample.Size/
- https://cran.r-project.org/web/packages/SampleSize4ClinicalTrials/
- https://cran.r-project.org/web/packages/samplesize4surveys/
- https://cran.r-project.org/web/packages/samplesizelogisticcasecontrol/
- https://cran.r-project.org/web/packages/SampleSizeMeans/
- https://cran.r-project.org/web/packages/SampleSizeProportions/
- https://cran.r-project.org/web/packages/WMWssp/
- https://cran.r-project.org/web/packages/bivarRIpower/
- https://cran.r-project.org/web/packages/coprimary/
- https://cran.r-project.org/web/packages/CRTSize/
- https://cran.r-project.org/web/packages/easypower/
