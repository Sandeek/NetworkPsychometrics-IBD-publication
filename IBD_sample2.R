
# This R script is used for the data analysis for the following publication 
# Thomann, Anne & Knödler, Laura-Louise & Karthikeyan, Surya & Atanasova, Konstantina & Bernstein, Charles & Ebert, Matthias & Lis, 
# Stefanie & Reindl, Wolfgang. (2021).
# The Interplay of Biopsychosocial Factors and Quality of Life in Inflammatory Bowel Diseases: A Network Analysis.
# Journal of clinical gastroenterology. Publish Ahead of Print. 10.1097/MCG.0000000000001625. 
##################################################################################################################################################################################
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
data = read.spss("sample2_latest.sav", to.data.frame=TRUE)
data = data[,c(20,33,35,36,38,43,44,39:42,34,116,114,115)]
# data2 = data2[,c(13,22,40,19,18,35,36,37,38,20,21)]
sample2_data = data;
nodes_labels = c("QoL","CT","ANX","DPR","VS","SE","LONELY","IID_RJ", "IID_ACC","IID_ENG","IID_ENR","FTG","Hb","TR_OBJ","TR_SUB")
long_names = c("Quality of Life","Childhood Trauma","Anxiety","Depression",
               "Visceral Sensitivity","Self Esteem","Loneliness","IIDRejection","IIDAcceptance","IIDEngulfment",
               "IIDEnrichment","Fatigue","Haemoglobin", "TR Objective","TR Subjective")

# ---------------------------------------------------------------------------------------
# ---------- 3. Network Estimation ------------------------------------------------------
# ---------------------------------------------------------------------------------------
## Sample 2 without last four variables - 84 dp
### GGM Model
ibd_network_1 <- estimateNetwork(data[,c(1:11)], 
                                 default="EBICglasso",
                                 corMethod = "cor",
                                 corArgs = list(method="spearman", 
                                                use = "pairwise.complete.obs")) 


simulation_network <- estimateNetwork(data[,c(1:11)],default = "EBICglasso",
                                      corMethod = "cor",corArgs = list(method="spearman", 
                                      use = "pairwise.complete.obs"), 
                                      tuning= 0.5,refit = TRUE)

simRes <- netSimulator(simulation_network$graph,
                       default = 'EBICglasso',
                       nCases = c(100,250,500,1000,2500), tuning = 0.5,
                       nReps = 100,
                       nCores = 8)

plot(simRes)
plot(simRes,yvar = c('strength','closeness',
                     'betweenness'))

ggcorrplot(ibd_network_1$graph,type = "lower", digits = 2, 
           show.legend = FALSE, lab = TRUE, 
           legend = "Edge Weights GGM Network")

#Network without therapy Response

ibd_network_2 <- estimateNetwork(data[,c(1:13)], 
                                  default="EBICglasso",
                                  corMethod = "cor",
                                  corArgs = list(method="spearman", 
                                                 use = "pairwise.complete.obs")) 
ggcorrplot(ibd_network_2$graph,type = "lower", digits = 2, 
           show.legend = FALSE, lab = TRUE, 
           legend = "Edge Weights GGM Network")
# Network Sample 1 Complete

ibd_network_3 <- estimateNetwork(data, 
                                   default="EBICglasso",
                                   corMethod = "cor",
                                   corArgs = list(method="spearman", use = "pairwise.complete.obs")) 
ggcorrplot(ibd_network_3$graph,type = "lower", digits = 2, 
           show.legend = FALSE, lab = TRUE, 
           legend = "Edge Weights GGM Network")

centralityPlot(ibd_network_1, include = "Strength", orderBy = "Strength", scale = "raw")
centralityPlot(ibd_network_2, include = "Strength", orderBy = "Strength", scale = "raw")
centralityPlot(ibd_network_3, include = "Strength", orderBy = "Strength", scale = "raw")

# # ---------------------------------------------------------------------------------------
# # ---------- 5. Bootstrap Edge Weights -------------------------------------------------
# # ---------------------------------------------------------------------------------------
# #Sample 1
network_accuracy_1 <- bootnet(ibd_network_1,
                       type = "nonparametric", nCores = 8,
                       nBoots = 1000, statistics =c("edge", "expectedInfluence", "strength"))
# #Sample 2
 network_accuracy_2 <- bootnet(ibd_network_2,
                       type = "nonparametric", nCores = 8,
                       nBoots = 1000, statistics =c("edge", "expectedInfluence", "strength"))
# 
# #Sample 1 complete
 network_accuracy_3 <- bootnet(ibd_network_3,
                       type = "nonparametric", nCores = 8,
                       nBoots = 1000, statistics =c("edge", "expectedInfluence", "strength"))
# 
# # #Plots 
plot(network_accuracy_1,  order = 'sample')
plot(network_accuracy_2,  order = 'sample')
plot(network_accuracy_3,  order = 'sample')

# ---------------------------------------------------------------------------------------
# ---------- 6. Bootstrap Centrality Indices -------------------------------------------------
# ---------------------------------------------------------------------------------------
#Sample 1
network_stability_1 <- bootnet(ibd_network_1,  
                                  type = "case", nCores = 8, 
                                  nBoots =1000, 
                                  statistics = "strength")
#Sample 2
network_stability_2 <- bootnet(ibd_network_2,  
                                  type = "case", nCores = 8, 
                                  nBoots =1000, 
                                  statistics = "strength")
#Sample 1 complete
network_stability_3 <- bootnet(ibd_network_3,  
                                  type = "case", nCores = 8, 
                                  nBoots =1000, 
                                  statistics = "strength")

corStability(network_stability_1)
corStability(network_stability_2)
corStability(network_stability_3)