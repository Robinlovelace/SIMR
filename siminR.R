# Spatial interaction model in R

# Load input data

# demand, denoted by dem
demand <- read.csv("demand.csv", header=F) 
dem <- demand[,3] # vector of origins
nrow(dem); summary(dem)

# attractiveness 
attractiveness <- read.csv("attractiveness.csv")
aLabs <- colnames(attractiveness)[-1]
att <- t(attractiveness[4, -1]) # vector of destinations
nrow(att); summary(att)

# beta values (rate of exponential decay with distance)
beta <- 0.1

# distance
dis <- as.matrix(read.csv("distance.csv", header=F))

# Check the dimensions

nrow(dis) == length(dem) # should be TRUE
ncol(dis) == length(att) # should be TRUE

# Run model

# balancing factors
B <- exp(-beta * dis) 
B <- sweep(B, MARGIN = 2, STATS = att, FUN = "*" )
B[1:3, 1:3]

# flow rate
S <- sweep(B, MARGIN = 1, STATS = dem, FUN = "*")
S <- sweep(S, MARGIN = 1, STATS = rowSums(B), FUN = "/")

head(S)

