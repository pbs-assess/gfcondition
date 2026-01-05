## gfcondition: Annual indices of average groundfish body condition 

This repository contains code used to generate sex and maturity specific indices 
of average body condition and associated density distributions for most species 
frequently sampled by Canadian Pacific bottom trawl surveys. 

These analyses are described in:

> English, P.A., Anderson, S.C., and Forrest, R.E. (2026) Body Condition
> as a Shared Response to Environment in a Commercially Important Demersal
> Fish Assemblage. Fish and Fisheries, in press.

All data retrieval, processing, and analysis scripts are found in the "analysis" 
folder and are numbered in the order that they must be run. Scripts starting with 
"00-" are for data preparation and are provided for reference, but do not to be run 
on their own. These files are either sourced by other files, or contain the original 
data retrieval and processing steps used to generate the data files provided. 
Scripts without numbers ("x-") are for plotting, supplementary tests, 
or summary statistic, and may need to be run after the main analysis is complete.
