# Plotting

# Catch of market share by destinations

catch <- colSums(S)
sel <- which(grepl("LEED|YOR|SHEF|BRAD|MEAD|HUDD|HALI|Gu", aLabs))
ggplot() + geom_bar(aes(x = aLabs[sel], y = catch[sel]))

# Flow by Distance

dVec <- as.numeric(c(dis))
dBins <- cut(dVec, breaks=c(0, 10, 20, 30, 40, 200))
dCats <- levels(dBins)
dCount <- data.frame(row.names=dCats)

for(i in 1:length(dCats)){
  dCount[i,1] <- sum(S[which(dBins == dCats[i])])
}
dCount
qplot(data = dCount, x = dCats, y = V1, geom = "bar", fill = dCats, stat = "identity")
p <- qplot(data = dCount, x = (cumsum(V1) + cumsum(c(0, V1[-length(V1)]))) / 2, 
      y = V1^0, geom = "bar", fill = dCats, stat = "identity", width = V1)
p + coord_polar()

theme_infog <- theme_classic() + theme(axis.line = element_blank(), axis.title = element_blank(), 
                                       axis.ticks = element_blank(), axis.text.y = element_blank())
last_plot() + theme_infog
