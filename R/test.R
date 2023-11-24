###Test of the different functions
data.path <- here::here('data','raw-data','example_data.csv')
birdID.path <- here::here('data','raw-data','birdsID.rds')
PEM.path <- here::here('data','raw-data','bird_eig400.rds')

data <- utils::read.csv(data.path)
birdID <- base::readRDS(birdID.path)
PEM <- base::readRDS(PEM.path)

birdID <- birdID[[2]]
PEM$ID <- rownames(PEM)
birdID$ID <- gsub(":",".", birdID$ID)
PEM <- merge(PEM, birdID, by="ID", all.x=T)
data <- data[,!(names(data)%in%c("X"))]
names(data)[names(data)=="binomial"] <- "targetTaxonName"
data$targetTaxonName <- gsub("_"," ", data$targetTaxonName)

#add phylo eigenvectors
PEM$ID <- NULL
names(PEM)[names(PEM)=="sp"] <- "binomial"
data <- merge(data, PEM, by.x ="targetTaxonName" ,by.y ="binomial", all.x=TRUE)
data <- data[complete.cases(data),] #all complete?

response_data <- colnames(data)[2]
species <- colnames(data)[1]
trait_data <- data[colnames(data)[c(3:14)]]
phylo_data <- data[colnames(data)[c(15:414)]]
method <- "regression"

RF <- opitmized_RF_function(data=data,species = species,trait_data = trait_data,
                            phylo_data = phylo_data,method='regression',)


data.trait <-
data.phylo <-

wgts <- weight_scenarii_list(data$response,0.5)
RF <- opitmized_RF_function()
