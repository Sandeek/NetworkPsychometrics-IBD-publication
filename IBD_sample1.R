# This R script is used for the data analysis for the following publication 
# Thomann, Anne & Knödler, Laura-Louise & Karthikeyan, Surya & Atanasova, Konstantina & Bernstein, Charles & Ebert, Matthias & Lis, 
# Stefanie & Reindl, Wolfgang. (2021).
# The Interplay of Biopsychosocial Factors and Quality of Life in Inflammatory Bowel Diseases: A Network Analysis.
# Journal of clinical gastroenterology. Publish Ahead of Print. 10.1097/MCG.0000000000001625. 
#########################################################################################
#########################################################################################
# ---------------------------------------------------------------------------------------
# ---------- 1. Load libaries -----------------------------------------------------------
# ---------------------------------------------------------------------------------------

library(foreign)
library(haven)
library(bootnet)
library(qgraph)
library(mgm)
library(dplyr)
library(summarytools)
library(ggcorrplot)
library(OpenMx)


# ---------------------------------------------------------------------------------------
# ---------- 2. Data preparation --------------------------------------------------------
# ---------------------------------------------------------------------------------------
# note: for the datasets please contact the authors.
data = read.spss("sample1_latest.sav", to.data.frame=TRUE)
data = data[,c(13,22,40,19,18,20,21,35,36,37,38)]
sample1_data = data;
nodes_labels = c("QoL","CT","ANX","DPR","VS","SE","LONELY","IID_RJ", 
                 "IID_ACC","IID_ENG","IID_ENR")

long_names = c("Quality of Life","Childhood Trauma","Anxiety","Depression",
               "Visceral Sensitivity","Self Esteem","Loneliness",
               "IIDRejection","IIDAcceptance","IIDEngulfment",
               "IIDEnrichment")

# ---------------------------------------------------------------------------------------
# ---------- 3. Network Estimation ------------------------------------------------------
# ---------------------------------------------------------------------------------------
## Sample 1 with all variables 

sample1_ibd_network_1<- estimateNetwork(data, 
                                 default="EBICglasso",
                                 corMethod = "cor",
                                 corArgs = list(method="spearman", 
                                                use = "pairwise.complete.obs"))

simulation_network <- estimateNetwork(data,default = "EBICglasso",
                                      corMethod = "cor",corArgs = list(method="spearman", 
                                                                       use = "pairwise.complete.obs"), 
                                      tuning= 0.5,refit = TRUE)

simRes <- netSimulator(simulation_network$graph,
                       default = 'EBICglasso',
                       nCases = c(80,100,250,500,1000,2500), tuning = 0.5,
                       nReps = 100,
                       nCores = 8)

plot(simRes)
plot(simRes,yvar = c('strength','closeness',
                     'betweenness'))


# # ---------------------------------------------------------------------------------------
# # ---------- 4. Bootstrap Edge Weights -------------------------------------------------
# # ---------------------------------------------------------------------------------------
# #Sample 1
network_accuracy_1 <- bootnet(ibd_network_1,
                               type = "nonparametric", nCores = 8,
                               nBoots = 1000, statistics =c("edge", "expectedInfluence", "strength"))
 
# # ---------------------------------------------------------------------------------------
# ---------- 6. Bootstrap Centrality Indices -------------------------------------------------
# ---------------------------------------------------------------------------------------
#Sample 1
network_stability_1 <- bootnet(ibd_network_1,  
                               type = "case", nCores = 8, 
                               nBoots =1000, 
                               statistics = "strength")

 plot(network_stability_1, statistics = "strength")
 plot(bootstrapped_strength2, statistics = "strength")
 plot(bootstrapped_strength3, statistics = "strength")
 
 corStability(bootstrapped_ei1)
 corStability(bootstrapped_ei2)
 corStability(bootstrapped_ei3)
 
# # ---------------------------------------------------------------------------------------
# # ----------7. Edge Weight Difference Tests -------------------------------------------
# # ---------------------------------------------------------------------------------------
 plot(network_accuracy_1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample",differenceEdgeColor = TRUE, differenceShowValue = TRUE)
 plot(network_accuracy_2, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample",differenceEdgeColor = TRUE, differenceShowValue = TRUE)
# # ---------------------------------------------------------------------------------------
# # ---------- 8. Centrality Difference Test  ------------------------------------------
# # ---------------------------------------------------------------------------------------
 plot(network_accuracy_1, "strength", plot = "difference", order = "sample")
 plot(network_accuracy_2, "strength", plot = "difference", order = "sample")
 plot(network_stability_1, "strength", order="sample", labels=TRUE)
 plot(network_stability_2, "strength", order="sample", labels=TRUE)