library(NetworkComparisonTest)
NCT(na.omit(sample1_data), na.omit(sample2_data[,c(1:11)]),binary.data=FALSE)
plot(res, what = "network")
plot(res, what = "strength")

# res individual global strength of network 1 and network 2
res$glstrinv.sep

# res difference global strength between two networks
res$glstrinv.real


