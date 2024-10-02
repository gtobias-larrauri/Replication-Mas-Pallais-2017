This project consisted of replicating VAWA by Mas & Pallais 2017 (originally written in Stata). This paper estimated the whole distribution of compensating wage differentials in an experimental setting, e.g. how much of a paycut were employees willing to take in exchange for working at home. An interesting facet of this experimental setting is that people who chose a "dominated"(bad) option are assumed to be innatentive. To extend their paper I used DoubleML techinques to "improve" their observational study and then proceeded to change the innatentiveness assumption, which changed the policy implications of the paper under some marketplace assumptions.


The replication is split into 3 files:
Replication_work is the main file and it produces the tables and graphs included in the final write up.
Functions_replication has the functions that implement those tables.
Observational has the results from the observational study conducted with DoubleML

The .dta and csv files contain the microdata for this replication.

Please set your working directory in R to the REPLICATION folder.

The pdf contains the final writeup.
