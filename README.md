# A nontechical primer for conducting a meta-analysis to synthesize correlational data

A companion R script implementing the analysis described in Quintana (in review). 

# Abstract

Meta-analysis synthesizes a body of research investigating a common research question. Outcomes from meta-analyses provide a more objective and transparent summary of a research area than traditional narrative reviews. Moreover, they are often used to support research grant applications, guide clinical practice and direct health policy. The aim of this article is to provide a practical and nontechnical guide for psychological scientists that outlines the steps involved in planning and performing a meta-analysis of correlational datasets. I provide a supplementary R script to demonstrate each analytical step described in the paper, which is readily adaptable for researchers to use for their analyses. I also emphasise the importance of meta-analysis protocols and pre-registration to improve transparency and help avoid unintended duplication. While the worked example is the analysis of correlational dataset, the general meta-analytic process described in this paper is applicable for all types of effect sizes. An improved understanding this tool will not only help scientists to conduct their own meta-analyses but also improve their evaluation of published meta-analyses.

# File descriptions

1. "script" - The supplementary R script. 
2. "dat_bias.csv" - A simulated “biased” dataset to demonstrate the trim and fill procedure. To follow the R script, this .csv file needs to be saved in the R working directory using the file name “dat_bias.csv”.
3. "dat_mes.csv" - A simulated dataset that aggregates the first 3 studies from the original dataset to demonstrate how to work with multiple effect sizes from a single study. To follow the R script, this .csv file needs to be saved in the R working directory using the file name “dat_mes.csv”.
