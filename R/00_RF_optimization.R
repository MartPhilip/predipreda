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


