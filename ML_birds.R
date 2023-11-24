##################
'13/08/2023'
'PHILIPPE Martin'
'Can we predict the predation pressure of owned domestic cats on all birds in the United States of America?'
'Part 4 : Random Forest for prey prediction'

##################

#libraries
library(gtools)
library(ranger)
library(caTools)
library(caret)
library(tidyverse)
library(patchwork)
library(DHARMa)
library(renv)
#get some rf functions
source("all function for ranger mam_bird.R")
##use file.path to find the best path, eg for windows users file.path + here::here()
#get the data
catm <- read.csv("data/raw-data/example_data.csv")
ev400 <- readRDS("data/raw-data/bird_eig400.rds")
IDn <- readRDS("data/raw-data/birdsID.rds")


#fix the rownames and get rid of "_"
IDn <- IDn[[2]]
ev400$ID <- rownames(ev400)
IDn$ID <- gsub(":",".", IDn$ID)
ev400 <- merge(ev400, IDn, by="ID", all.x=T)
cat <- catm[,!(names(catm)%in%c("X"))]
names(cat)[names(cat)=="binomial"] <- "targetTaxonName"


#correlation
cat.corr <- cat |> dplyr::select(Hand.Wing.Index,ln.Mass,ln.Beak.Depth,ln.Beak.Length_Nares,ln.Clutch,Tail.Length)
hist(cat.corr$Hand.Wing.Index)
hist(cat.corr$Mass)
hist(cat.corr$Beak.depth)
hist(cat.corr$Beak.length.nares)
hist(cat.corr$Clutch.size)
hist(cat.corr$home_range)
hist(cat.corr$Tail.Length)
cat.corr$Beak.length.nares <- log(cat.corr$Beak.length.nares)
cat.corr$Mass <- log(cat.corr$Mass)
hist(cat.corr$Beak.length.nares)
hist(cat.corr$Mass)
require(corrplot)
M <- cor(cat.corr,method="spearman")
corrplot(M, method = 'number')
#correlation

cat <- cat |>  dplyr::select(-volant,-Habitat,-ln.Beak.Depth,-ln.Beak.Length_Nares)
all <- cat
all$targetTaxonName <- gsub("_"," ", all$targetTaxonName)

#add phylo eigenvectors
ev400$ID <- NULL
names(ev400)[names(ev400)=="sp"] <- "binomial"
all <- merge(all, ev400, by.x ="targetTaxonName" ,by.y ="binomial", all.x=TRUE)
all <- all[complete.cases(all),] #all complete
all$response <- all$response/145
# all <- all |>  dplyr::filter(home_range>0)

wgts <- all$response
wgts.0 <- rep(1,length(wgts))
ints <- length(wgts[wgts>0])
nons <- length(wgts[wgts==0])
wgts <- ifelse(wgts>0, 1, ints/nons)
wgts.1 <- wgts
for(i in 1:length(wgts)){
 if (all$response[i]>0.03){
  wgts[i] <- (ints/nons)*3
  }
  else if (all$response[i]>0 & all$response[i]<0.03){
   wgts[i] <- (ints/nons)*2
}
else{}
}
wgts.2 <- wgts
wgtlist <- list(wgts.0,wgts.1,wgts.2)

names(all) <- gsub(" ", "",names(all))

n_features <- dim(all)[2]-2 # minus two because name and interaction not included are predictors



# train a default random forest model
rf1 <- ranger(
  response ~ .-targetTaxonName,
  data = all,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123,
  case.weights    = wgts.0

)
#OOB prediction error: 25.73
# get OOB RMSE
(default_rmse <- sqrt(rf1$prediction.error))



# create hyperparameter grid for random forest (rf) to loop through i.e., the parameters values to compare
hyper_grid <- expand.grid(
  mtry_frac = c(.05, .15, .25, .333, .4, .6), #floor(n_features *
  min.node.size = c(1, 3, 5, 10, 20, 30, 50, 75, 100),
  replace = c(TRUE, FALSE),
  sample.fraction = c(.5, .6, .7),
  ntrees = seq(50,750,50),
  PEMs = c(5,10,20),
  mtry = NA,
  rmse = NA,
  wgt = c(1,2,3)
)

# execute full cartesian grid search, no weights assigned in this run
start_time <- Sys.time()

for(i in seq_len(nrow(hyper_grid))) {
  #adjust number of PEMs used
  names1 <- names(all)[names(all)%in%names(cat)]
  names2 <- paste("eig",1:hyper_grid$PEMs[i],sep="")
  allN <- all[,names(all)%in%c(names1,names2)]
  n_features <- dim(allN)[2]-2. #names and response
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = response ~ . -targetTaxonName,
    data            = allN,
    num.trees       = hyper_grid$ntrees[i],
    mtry            = round(hyper_grid$mtry_frac[i]*n_features),
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
    case.weights    = wgtlist[[hyper_grid$wgt[i]]]
  )
  # export OOB error
  hyper_grid$rmse[i] <- fit$prediction.error
  hyper_grid$mtry[i] <- round(hyper_grid$mtry_frac[i]*n_features)
  print(i)
}

end_time <- Sys.time()
end_time - start_time #6 hours

# assess top 10 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(1)

hyper_gridNW <- hyper_grid
#optimal parameters: mtry = 44, node = 100, replace = TRUE, sampFrac = 0.5, trees = 300, PEMs = 100
names1 <- names(all)[names(all)%in%names(cat)]
names2 <- paste("eig",1:10,sep="")
allN <- all[,names(all)%in%c(names1,names2)]
n_features <- dim(allN)[2]-2. #names and response

rf_opti <- ranger(
  response ~ .-targetTaxonName,
  data = allN,
  num.trees       = 50,
  mtry            = 8,
  min.node.size   =30,
  replace         = FALSE,
  sample.fraction = 0.7,
  verbose         = FALSE,
  seed            = 123,
  respect.unordered.factors = 'order',
  case.weights    = wgts.2,
  importance = 'permutation'
)






prediction.data <- allN %>% dplyr::select(-targetTaxonName,-response)
prediction <- predict(rf_opti,prediction.data)
predictions <- cbind(allN$targetTaxonName,allN$response,prediction$predictions)
colnames(predictions) <- c('binomial','Predation','RF.prediction')
predictions <- as.data.frame(predictions)
predictions$Predation <- as.numeric(predictions$Predation)
predictions$RF.prediction <- as.numeric(predictions$RF.prediction)
predictions <- predictions %>% dplyr::arrange(RF.prediction)



pred <- predictions$RF.prediction
min(pred)
max(pred)
mean(pred)
sd(pred)
pred <- pred[pred>=0.0066563318]
mean(pred)
sd(pred)
quantile(pred)

RAC_Italy_raw <- ggplot(data=predictions,aes(x=reorder(binomial,-Predation),y=Predation,fill=Predation,color=Predation))+
  geom_col()+
  theme_classic()+
  labs(x='species',y='Predation events per year per cat',title='A')+
  theme(legend.position = 'none', axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_fill_viridis_c(option='viridis',direction = 1)+
  scale_color_viridis_c(option='viridis',direction = 1)+
  ylim(0,0.65)

RAC_Italy_raw

RAC_Italy <- ggplot(data=predictions,aes(x=reorder(binomial,-RF.prediction),y=RF.prediction,fill=RF.prediction,color=RF.prediction))+
  geom_col()+
  theme_classic()+
  labs(x='species',y='Random Forest predation prediction',title='B')+
  theme(legend.position = 'none', axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_fill_viridis_c(option='viridis',direction = 1)+
  scale_color_viridis_c(option='viridis',direction = 1)+
  ylim(0,0.25)
RAC_Italy

threshold <- data.frame('Threshold predation prediction'=c('No predation','Low predation','Medium predation','High predation','Very high predation'),
                        'Random Forest predation prediction' = c(0.006716301,0.010832854,0.017001180,0.041731079,0.2045706))
threshold$Threshold.predation.prediction <- factor(threshold$Threshold.predation.prediction,levels=c('Very high predation','High predation','Medium predation','Low predation','No predation'))
threshold_italy <- ggplot(data = threshold,aes(y=Random.Forest.predation.prediction,x=reorder(Threshold.predation.prediction,-Random.Forest.predation.prediction),fill=Threshold.predation.prediction,color=Threshold.predation.prediction,label=round(Random.Forest.predation.prediction,4)))+
  geom_point(shape=23,size=10)+
  geom_text(color='black',vjust = -2)+
  scale_fill_viridis_d(option='viridis')+
  scale_color_viridis_d(option='viridis')+
  labs(x='',y='Random Forest predation prediction',title = "C")+
  theme_classic()+
  theme(legend.position='none')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ylim(0,0.25)
threshold_italy


PredvsTrue_T <- allN %>%
  dplyr::select(targetTaxonName,response) %>%
  dplyr::arrange(-response) %>%
  mutate(rank_T=c(1:length(allN$response)))

PredvsTrue_P <- predictions %>%
  dplyr::select(binomial,RF.prediction) %>%
  dplyr::arrange(-RF.prediction) %>%
  mutate(rank_P=c(1:length(predictions$RF.prediction)))


PredvsTrue <- merge(PredvsTrue_T,PredvsTrue_P,by.x='targetTaxonName',by.y='binomial',all=T)
PredvsTrue <- PredvsTrue %>% dplyr::mutate(accuracy_1 = case_when((rank_T-rank_P)==0~0,
                                                                  (rank_T-rank_P)!=0~1))%>%
  dplyr::mutate(accuracy_2 = rank_T-rank_P)


PredvsTrue_Pos <- PredvsTrue %>% dplyr::filter(response>0)
PredvsTrue_Pos$accuracy_3 <- ifelse(PredvsTrue_Pos$RF.prediction>=0.006716301,'TP','FP')

PredvsTrue_Null <- PredvsTrue %>% dplyr::filter(response==0)
PredvsTrue_Null$accuracy_3 <- ifelse(PredvsTrue_Null$RF.prediction<0.006716301,'TN','FN')

PredvsTrue <- rbind(PredvsTrue_Pos,PredvsTrue_Null)
classification_metrics <- PredvsTrue %>% dplyr::select(accuracy_3) %>% mutate(value=1) %>%group_by(accuracy_3)%>% summarise(sum_value=sum(value))
FN <- classification_metrics$sum_value[1]
FP <- classification_metrics$sum_value[2]
TN <- classification_metrics$sum_value[3]
TP <- classification_metrics$sum_value[4]
predicted <- predictions$RF.prediction
observed <- predictions$Predation


MAE <- mean(abs(predicted-observed))
accuracy <- (TP+TN)/(TP+TN+FP+FN)
TNR <- TN/(TN+FP)
TPR <- TP/(TP+FN)
F1.score <- TP/(TP+(0.5*(FP+FN)))
MCC <- ((TP*TN)-(FN*FP))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
accuracy
TNR
TPR
F1.score
MCC





# Obtain feature importance scores from the trained random forest model
importance_scores <- ranger::importance(rf_opti)

# Print the feature importance scores
print(importance_scores)
require(pdp)
partial_plot <- partial(rf_opti, pred.var = c("Mass", "home_range"))
plotPartial(partial_plot)

require(patchwork)
library(fastshap)
library(shapviz)
pfun <- function(object, newdata) {  # prediction wrapper
  unname(predict(object, data = newdata)$predictions)
}
set.seed(123)  # for reproducibility
X <- allN %>% dplyr::select(-targetTaxonName,-response)
ex.t1 <- explain(rf_opti, X = X, pred_wrapper = pfun, nsim = 100, adjust = TRUE,
                 shap_only = FALSE)
tibble::as_tibble(ex.t1$shapley_values)
shv.global <- shapviz(ex.t1)
importance_Italy <- sv_importance(shv.global, show_numbers = TRUE)
importance_Italy <- importance_Italy + theme_classic()+ labs(title='C')

italy_full <- (RAC_Italy_raw)|(RAC_Italy/importance_Italy)
italy_full

allN.corr <- allN %>% dplyr::select(Hand.Wing.Index,Mass,Clutch.size,home_range,Tail.Length,eig1,eig2,eig3,eig4,eig5)

par(mfrow=c(1,1))
M <- cor(allN.corr,method="spearman")
corrplot(M, method = 'number')





















#####

test_data_usa <- read.csv('data_usa_birds.csv')
test_data_usa <- test_data_usa %>% dplyr::select(-volant,-Beak.depth,-Beak.length.nares)
test_data_usa$scientificName <- gsub("_"," ", test_data_usa$scientificName)
ev400.usa <- ev400[,c(1,2,3,4,5,6,7,8,9,10,401)]
test_data_usa <- merge(test_data_usa, ev400.usa, by.x ="scientificName" ,by.y ="binomial", all.x=TRUE)
test_data_usa <- test_data_usa %>% dplyr::filter(home_range>0)
predict_data_usa <- test_data_usa %>% dplyr::select(-scientificName)
test_data_usa <- test_data_usa %>% na.omit()
predict_data_usa <- predict_data_usa %>% na.omit()##remove the insular endemic birds


prediction_usa <- predict(rf_opti,predict_data_usa)


test <- data.frame(scientificName=test_data_usa$scientificName,RF.prediction=prediction_usa$predictions)
pred <- test$RF.prediction
min(pred)
max(pred)
mean(pred)
sd(pred)
dev.off()
RAC_Usa <- ggplot(data=test,aes(x=reorder(scientificName,-RF.prediction),y=RF.prediction,fill=RF.prediction,color=RF.prediction))+
  geom_col()+
  theme_classic()+
  labs(x='species',y='Random Forest predation prediction',title='A')+
  theme(legend.position = 'none', axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_fill_viridis_c(option='viridis',direction = 1)+
  scale_color_viridis_c(option='viridis',direction = 1)+
  ylim(0,0.17)
RAC_Usa

threshold <- data.frame('Threshold predation prediction'=c('Max. no predation','Max. low predation','Max. medium predation','Max. high predation','Max. very high predation'),
                        'Random Forest predation prediction' = c(0.006716301,0.010832854,0.017001180,0.041731079,0.1422407))
threshold$Threshold.predation.prediction <- factor(threshold$Threshold.predation.prediction,levels=c('Max. no predation','Max. low predation','Max. medium predation','Max. high predation','Max. very high predation'))
threshold_usa <- ggplot(data = threshold,aes(y=Random.Forest.predation.prediction,x=reorder(Threshold.predation.prediction,-Random.Forest.predation.prediction),fill=Threshold.predation.prediction,color=Threshold.predation.prediction,label=round(Random.Forest.predation.prediction,4)))+
  geom_point(shape=23,size=10)+
  geom_text(color='black',vjust = -2)+
  scale_fill_viridis_d(option='viridis')+
  scale_color_viridis_d(option='viridis')+
  labs(x='',y='',title = "B")+
  theme_classic()+
  theme(legend.position='none')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ylim(0,0.17)
threshold_usa

usa <- RAC_Usa|threshold_usa
usa

compa <- read.csv('bird_database.csv')
compa <- compa %>% dplyr::filter(Cont=='N. America') %>%dplyr::select(binomial,Prey)
ACAD <- read.csv('ACAD.csv')
ACAD <- ACAD %>% dplyr::select(Scientific.Name,PS.g,BD.g,TB.c,PT.c,IUCN.Red.List.2018)

compa$binomial <- gsub("_"," ", compa$binomial)

test.bis <- merge(test,compa,by.x="scientificName",by.y="binomial",all.x=T)
test.bis <- merge(test.bis,ACAD,by.x="scientificName",by.y="Scientific.Name",all.x=T)
test.bis <- test.bis %>% pivot_longer(cols=c("PS.g","BD.g","TB.c","PT.c"))
test.bis$value <- as.factor(test.bis$value)
test.bis$Prey <- as.factor(test.bis$Prey)

library(tidyverse)
library(ggdist) # for shadeable density slabs
library(gghalves) # for half-half geoms
library(ggpp) # for position_dodge2nudge
library(cowplot) # for publication-ready themes
library(colorspace) # for lightening color palettes
library(gridExtra) # for grid.arrange

ggplot(data = test.bis,
       aes(y = RF.prediction,x = value,fill = value)) +
  # dots
  stat_dots(side = "left",scale = 1, show.legend = T,
            position = position_dodge(width = .3),aes(color = value)) +
  # dot-whisker for means
  stat_summary(fun = mean,
               geom = "pointrange",
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x),show.legend = T,size = .4,
               position = position_dodge2nudge(x=.1,width = .3)) +
  # styling
  theme_half_open() +
  facet_wrap(.~name)+
  theme(legend.position='none',axis.text.x = element_text(size = 7),axis.text.y = element_text(size = 7))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x='',y='Random Forest predation prediction')




test.bis.1 <- test.bis %>% filter(Prey=='1') %>% dplyr::distinct(scientificName,.keep_all = T) %>% mutate(SpeciesType='Prey species')
test.bis.0 <- test.bis %>% filter(Prey=='0') %>% dplyr::distinct(scientificName,.keep_all = T) %>% mutate(SpeciesType='Non-prey species')
min(test.bis.1$RF.prediction)
max(test.bis.1$RF.prediction)
mean(test.bis.1$RF.prediction)
sd(test.bis.1$RF.prediction)
min(test.bis.0$RF.prediction)
max(test.bis.0$RF.prediction)
mean(test.bis.0$RF.prediction)
sd(test.bis.0$RF.prediction)

predation_pressure <- rbind(test.bis.1,test.bis.0)
predation_pressure <- predation_pressure %>% select(RF.prediction,SpeciesType)
# Observed difference in means
observed_diff <- median(predation_pressure$RF.prediction[predation_pressure$SpeciesType == 'Prey species']) -
  median(predation_pressure$RF.prediction[predation_pressure$SpeciesType == 'Non-prey species'])

# Number of permutations
num_permutations <- 10000

# Permutation test
perm_diffs <- replicate(num_permutations, {
  # Permute SpeciesType labels
  permuted_species <- sample(predation_pressure$SpeciesType)

  # Calculate mean difference for each permutation
  perm_median_diff <- median(predation_pressure$RF.prediction[permuted_species == 'Prey species']) -
    median(predation_pressure$RF.prediction[permuted_species == 'Non-prey species'])

  return(perm_median_diff)
})

# Calculate the p-value
p_value <- length(perm_diffs[perm_diffs>observed_diff])/num_permutations
hist(perm_diffs)
mean(perm_diffs)
# Print the observed difference and p-value
print(paste("Observed Difference in Means: ", observed_diff))
print(paste("Permutation Test P-Value: ", p_value))
require('viridis')
colors <- viridis_pal(option = "D")(2)

# Calculate median values for each SpeciesType
median_values <- aggregate(RF.prediction ~ SpeciesType, data = predation_pressure, FUN = median)
significant <- TRUE
# Plotting
prey_plot <- ggplot(predation_pressure, aes(x = RF.prediction, fill = SpeciesType)) +
  geom_density(alpha = 0.4) +  # Density plots for each group
  geom_point(data = median_values, aes(x = RF.prediction, y = RF.prediction),
             color = "black", shape = 5, size = 3) +  # Median points
  labs(x = "RF.prediction", y = "Density", fill = "Species Type") +
  scale_fill_manual(values = colors) +  # Set color palette
  theme_classic() +
  labs(y='Density',x='Random Forest predation prediction',title='A')+
  scale_x_sqrt() +
  theme(legend.position = c(0.8, 0.8)) +
  geom_text(data = median_values, aes(x = RF.prediction, y = 0.005, label = "*"),
            size = 6, vjust = -0.5, hjust = 0.5, color = ifelse(significant, "black", "gray"))
prey_plot

predation_pressure <- predation_pressure %>% dplyr::mutate('Predation pressure'=case_when(RF.prediction<0.006716301~'No predation',
                                                           RF.prediction>=0.006716301 & RF.prediction<0.010832854~'Low predation',
                                                           RF.prediction>=0.010832854 & RF.prediction<0.017001180~'Medium predation',
                                                           RF.prediction>=0.017001180 & RF.prediction<0.041731079~'High predation',
                                                           RF.prediction>=0.041731079~'Very high predation',))
predation_pressure$`Predation pressure` <- factor(predation_pressure$`Predation pressure`,levels=c('No predation','Low predation','Medium predation','High predation','Very high predation'))

predation_pressure_sum <- predation_pressure %>% dplyr::mutate(occurence = 1 ) %>% group_by(SpeciesType,`Predation pressure`) %>% summarise('Number of species'=sum(occurence))

prey_plot_2 <- ggplot(data = predation_pressure_sum, aes(x = `Predation pressure` , y = `Number of species` , fill = `Predation pressure`,label = `Number of species` ))+
  facet_grid(~SpeciesType)+
  geom_col()+
  geom_text(vjust=-0.2)+
  scale_y_sqrt() +
  scale_fill_viridis_d()+
  scale_x_discrete(guide=guide_axis(n.dodge=2)) +
  theme_classic()+
  theme(legend.position = 'none')+
  labs(x='',y='Number of species',title='B')

prey_plot_2

prey_full <- prey_plot/prey_plot_2
prey_full

par(mfrow=c(1,1))


Conservation_birds <- na.omit(test.bis)
Conservation_birds$value <- as.factor(Conservation_birds$value)
unique(Conservation_birds$name)

Conservation_birds_PS <- Conservation_birds %>% dplyr::filter(name=="PS.g")
Conservation_birds_BD <- Conservation_birds %>% dplyr::filter(name=="BD.g")
Conservation_birds_TB <- Conservation_birds %>% dplyr::filter(name=="TB.c")
Conservation_birds_PT <- Conservation_birds %>% dplyr::filter(name=="PT.c")

model_PS <- aov(sqrt(RF.prediction) ~ value , data = Conservation_birds_PS)
TukeyHSD(model_PS)
simulationOutput <- simulateResiduals(fittedModel = model_PS)
plot(simulationOutput)
# Load required libraries
library(ggplot2)

# Provided data
diff_data <- data.frame(
  comparison = c("2-1", "3-1", "4-1", "5-1", "3-2", "4-2", "5-2", "4-3", "5-3", "5-4"),
  diff = c(-0.07458427, -0.11986797, -0.13251445, -0.14448389, -0.04528370, -0.05793018, -0.06989962, -0.01264648, -0.02461592, -0.01196944),
  lwr = c(-0.09946133, -0.14438897, -0.15867540, -0.17954341, -0.05945534, -0.07478103, -0.09868722, -0.02896711, -0.05309640, -0.04187351),
  upr = c(-0.049707212, -0.095346966, -0.106353501, -0.109424359, -0.031112052, -0.041079327, -0.041112007, 0.003674143, 0.003864567, 0.017934638),
  p_adj = c(0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.2129901, 0.1265955, 0.8093425)
)

# Function to get asterisks based on p-value
get_asterisks <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else if (p_value < 0.1) return("°")

  else return("")
}

# Create a new column with asterisks based on p-value
diff_data$asterisks <- sapply(diff_data$p_adj, get_asterisks)
require(ggrepel)
# Create ggplot with letters indicating significant differences
PS_plot <- ggplot(diff_data, aes(x = comparison, y = diff)) +
  geom_bar(stat = "identity", aes(fill = factor(ifelse(p_adj < 0.05, "Significant", "Not Significant"))), width = 0.7) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "black") +
  geom_label_repel(aes(label=round(p_adj,3)),fill='black',color='white',size=2.5) +
  labs(x = "", y = "Difference", fill = "Significance",title="Population size") +
  theme_classic() +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),legend.position = 'none')+
  geom_hline(yintercept = 0, linetype='dashed',color='black')



model_BD <- aov(sqrt(RF.prediction) ~ value , data = Conservation_birds_BD)
TukeyHSD(model_BD)
simulationOutput <- simulateResiduals(fittedModel = model_BD)
plot(simulationOutput)
# Load required libraries
library(ggplot2)

# Provided data
diff_data <- data.frame(
  comparison = c("2-1", "3-1", "4-1", "5-1", "3-2", "4-2", "5-2", "4-3", "5-3", "5-4"),
  diff = c(-0.008737354, -0.011686554, 0.001725653, -0.029060465, -0.002949199, 0.010463007, -0.020323111, 0.013412207, -0.017373911, -0.030786118),
  lwr = c(-0.02467712, -0.03403752, -0.02851879, -0.06481631, -0.02633917, -0.02055717, -0.05673747, -0.02134036, -0.05701600, -0.07535804),
  upr = c(0.007202414, 0.010664411, 0.031970101, 0.006695381, 0.020440770, 0.041483189, 0.016091249, 0.048164776, 0.022268175, 0.013785801),
  p_adj = c(0.5633993, 0.6084729, 0.9998668, 0.1724492, 0.9969612, 0.8882396, 0.5456260, 0.8291488, 0.7522316, 0.3241797)
)

# Function to get asterisks based on p-value
get_asterisks <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else if (p_value < 0.1) return("°")

  else return("")
}

# Create a new column with asterisks based on p-value
diff_data$asterisks <- sapply(diff_data$p_adj, get_asterisks)

# Create ggplot with letters indicating significant differences
BD_plot <- ggplot(diff_data, aes(x = comparison, y = diff)) +
  geom_bar(stat = "identity", aes(fill = factor(ifelse(p_adj < 0.05, "Significant", "Not Significant"))), width = 0.7) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "black") +
  geom_label_repel(aes(label=round(p_adj,3)),fill='black',color='white',size=2.5) +
  labs(x = "", y = "Difference", fill = "Significance",title='Breeding distribution') +
  theme_classic() +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),legend.position = 'none')+
  geom_hline(yintercept = 0, linetype='dashed',color='black')



model_TB <- aov(sqrt(RF.prediction) ~ value , data = Conservation_birds_TB)
TukeyHSD(model_TB)
simulationOutput <- simulateResiduals(fittedModel = model_TB)
plot(simulationOutput)
# Load required libraries
library(ggplot2)

# Provided data
diff_data <- data.frame(
  comparison = c("2-1", "3-1", "4-1", "5-1", "3-2", "4-2", "5-2", "4-3", "5-3", "5-4"),
  diff = c(-0.06810319, -0.08161092, -0.10653586, -0.09453311, -0.01350773, -0.03843267, -0.02642992, -0.02492494, -0.01292220, 0.01200275),
  lwr = c(-0.10619974, -0.11927632, -0.14755397, -0.16733814, -0.02833148, -0.06042235, -0.09047391, -0.04615891, -0.07671066, -0.05382104),
  upr = c(-0.030006644, -0.043945517, -0.065517757, -0.021728088, 0.001316025, -0.016442989, 0.037614071, -0.003690974, 0.050866270, 0.077826533),
  p_adj = c(0.0000124, 0.0000000, 0.0000000, 0.0037398, 0.0935489, 0.0000210, 0.7913689, 0.0120583, 0.9814362, 0.9874829)
)

# Function to get asterisks based on p-value
get_asterisks <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else if (p_value < 0.1) return("°")

  else return("")
}

# Create a new column with asterisks based on p-value
diff_data$asterisks <- sapply(diff_data$p_adj, get_asterisks)
require(ggrepel)
# Create ggplot with letters indicating significant differences
TB_plot<- ggplot(diff_data, aes(x = comparison, y = diff)) +
  geom_bar(stat = "identity", aes(fill = factor(ifelse(p_adj < 0.05, "Significant", "Not Significant"))), width = 0.7) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "black") +
  geom_label_repel(aes(label=round(p_adj,3)),fill='black',color='white',size=2.5) +
  labs(x = "", y = "Difference", fill = "Significance",title = 'Threats during breeding seasons') +
  theme_classic() +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),legend.position = 'none')+
  geom_hline(yintercept = 0, linetype='dashed',color='black')
TB_plot

model_PT <- aov(sqrt(RF.prediction) ~ value , data = Conservation_birds_PT)
TukeyHSD(model_PT)
simulationOutput <- simulateResiduals(fittedModel = model_PT)
plot(simulationOutput)
# Load required libraries
library(ggplot2)

# Provided data
diff_data <- data.frame(
  comparison = c("2-1", "3-1", "4-1", "5-1", "3-2", "4-2", "5-2", "4-3", "5-3", "5-4"),
  diff = c(0.012277359, -0.011085032, 0.017708453, -0.002623704, -0.023362391, 0.005431094, -0.014901063, 0.028793485, 0.008461328, -0.020332157),
  lwr = c(-0.010473705, -0.033796461, -0.003724300, -0.025296005, -0.045541645, -0.015436901, -0.037040248, 0.007968709, -0.013637125, -0.041114253),
  upr = c(0.0350284237, 0.0116263976, 0.0391412067, 0.0200485971, -0.0011831375, 0.0262990882, 0.0072381215, 0.0496182609, 0.0305597803, 0.0004499384),
  p_adj = c(0.5785924, 0.6695156, 0.1594366, 0.9978250, 0.0331996, 0.9538189, 0.3509366, 0.0015839, 0.8332209, 0.0586275)
)

# Function to get asterisks based on p-value
get_asterisks <- function(p_value) {
  if (p_value < 0.001) return("***")
  else if (p_value < 0.01) return("**")
  else if (p_value < 0.05) return("*")
  else if (p_value < 0.1) return("°")

  else return("")
}

# Create a new column with asterisks based on p-value
diff_data$asterisks <- sapply(diff_data$p_adj, get_asterisks)

# Create ggplot with letters indicating significant differences
PT_plot <- ggplot(diff_data, aes(x = comparison, y = diff)) +
  geom_bar(stat = "identity", aes(fill = factor(ifelse(p_adj < 0.05, "Significant", "Not Significant"))), width = 0.7) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "black") +
  geom_label_repel(aes(label=round(p_adj,3)),fill='black',color='white',size=2.5) +
  labs(x = "", y = "Difference", fill = "Significance",title='Population trend') +
  theme_classic() +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),legend.position = 'none')+
  geom_hline(yintercept = 0, linetype='dashed',color='black')
PT_plot

PS_plot / BD_plot / TB_plot  / PT_plot

#####
predcition.full.data.italy <- predictions %>%
  dplyr::rename(targetTaxonName=binomial) %>%
  dplyr::rename('Predicted predation pressure'=RF.prediction) %>%
  select(-Predation)
full.data.italy  <- merge(all,predcition.full.data.italy,by='targetTaxonName')
full.data.italy <- full.data.italy %>%
  dplyr::rename(scientificName=targetTaxonName) %>%
  dplyr::rename('Predation pressure'=response)
