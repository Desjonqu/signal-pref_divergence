# signal-pref_divergence
code and data associated to the manuscript titled "Interacting phenotypes and the origins of signal-preference divergence via social plasticity" by Camille Desjonquères, Bretta Speck, Sara Seidita, Lauren A. Cirino, Ignacio Escalante, Nathan W. Bailey, Rafael L. Rodríguez 

This repository contains 9 files:
- three data tables
- 6 R scripts

The two data tables are male_IGE_fulldataset.csv, spline_summaries_formated.csv, and median.csv. 
- male_IGE_fulldataset.csv contains the data for each individual male in the study
- spline_summaries_formated.csv contains the data for each individual female in the study
- medians.csv contains aggregated data for males and females by rearing plant

The six scripts are diagnostic_fcns.r, Figure1.R, Figure2_Table2_linearmodel.R, FigureS1.R, formatingdata.R and Table1_variance-comparisons_Ftests.R:
- diagnostic_fcns.r is a function to check diagnostics of the linear models written by **Roger Mundry**
- Figure1.R is a script to realise the hypothesis figure 1
- Figure2_Table2_linearmodel.R is a script to fit the linear model, draw Figure 2 and obtain the statistics in Table 2
- FigureS1.R is a script to draw Figure S1 showing preference function traits
- formatingdata.R is a script to obtain the formatted data frame medians.csv containing aggregated data for males and females by rearing plant
- Table1_variance-comparisons_Ftests.R is a script to obtain the F statistics reported in Table 1
