tiff("sample2_network3.tiff", units="in", width=3.54, height=3.54, res=300)
ibd_network_3_layout = plot(ibd_network_3, labels = nodes_labels,
     layout = "spring")
dev.off()

L = averageLayout(ibd_network_3_layout)
tiff("sample2_network2.tiff", units="in", width=3.54, height=3.54, res=300)
plot(ibd_network_2,  
     layout = L[c(1:13),],labels = nodes_labels[1:13])
dev.off()

tiff("sample2_network1.tiff", units="in", width=3.54, height=3.54, res=300)
plot(ibd_network_1, 
     layout = L[c(1:11),],labels = nodes_labels[1:11] )
dev.off()

## Sample 1 
tiff("sample1_network1.tiff", units="in", width=3.54, height=3.54, res=300)
plot(sample1_ibd_network, labels = nodes_labels, 
     layout = L[c(1:11),])
dev.off()


tiff("sample2_network1.tiff", units="in", width=5, height=5, res=300)
ggcorrplot(ibd_network_1$graph,type = "lower", digits = 2, 
           show.legend = FALSE, lab = TRUE, 
           legend = "Edge Weights GGM Network")
dev.off()

tiff("sample2_network2.tiff", units="in", width=6, height=6, res=300)
ggcorrplot(ibd_network_2$graph,type = "lower", digits = 2, 
           show.legend = FALSE, lab = TRUE, 
           legend = "Edge Weights GGM Network")
dev.off()

tiff("sample2_network3.tiff", units="in", width=7, height=7, res=300)
ggcorrplot(ibd_network_3$graph,type = "lower", digits = 2, 
           show.legend = FALSE, lab = TRUE, 
           legend = "Edge Weights GGM Network")
dev.off()

tiff("sample1_network1.tiff", units="in", width=5, height=5, res=300)
ggcorrplot(sample1_ibd_network_1$graph,type = "lower", digits = 2, 
           show.legend = FALSE, lab = TRUE, 
           legend = "Edge Weights GGM Network")
dev.off()