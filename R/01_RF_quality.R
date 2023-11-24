#' Evaluating the quality of the different random forest model according to
#' the hyperparameters select with optimized_RF_function
#'
#' @param data a dataframe
#' @param species a string
#' @param response_data a string
#' @param trait_data a dataframe
#' @param phylo_data a Boolean TRUE or FALSE
#' @param classification  a Boolean TRUE or FALSE
#' @param RF_optimized_dataset a dataframe
#' @param until_which_model a numeric
#'
#' @return a list
#' @export
#'
#' @examples
#' #' \dontrun{
#'XXX
#' }
random_forest_quality <- function(data,
                                  species,
                                  response_data,
                                  trait_data,
                                  phylo_data = NULL,
                                  classification, # binary or regression
                                  RF_optimized_dataset, #results of the optimized_RF_function()
                                  until_which_model=NULL){
  if(is.null(until_which_model)){until_which_model <- 1}
  RF_optimized_dataset <- RF_optimized_dataset[base::order(RF_optimized_dataset$rmse),]
  result_list <- list()
  for(i in c(1:until_which_model)){
    if(phylo_data == TRUE){

      names1 <- colnames(data)[colnames(data) %in% colnames(trait_data)]
      names2 <- paste("Eig", 1:RF_optimized_dataset$PEMs[i], sep = "")
      data_new <- data[c(species, response_data, names1, names2)]

    }

    if(phylo_data == FALSE){

      data_new <- data

    }
    formula <- stats::as.formula(paste0(response_data, " ~ .-", species))
    n_features_new <- dim(data_new)[2] - 2
    fit <- ranger::ranger(formula = formula,
                          data = data_new,
                          num.trees = RF_optimized_dataset$ntrees[i],
                          mtry = round(RF_optimized_dataset$mtry_frac[i]*n_features_new),
                          min.node.size = RF_optimized_dataset$min.node.size[i],
                          replace = RF_optimized_dataset$replace[i],
                          sample.fraction = RF_optimized_dataset$sample.fraction[i],
                          verbose = FALSE,
                          seed = 123,
                          respect.unordered.factors = 'order',
                          case.weights = weight[[RF_optimized_dataset$wgt[i]]])
  data_prediction <-  data_new[,!names(data_new) %in% c(species,response_data)]
  prediction <- predict(fit,data_prediction)
  Yobs <- data_new[,response_data]
  Ypred <- prediction[["predictions"]]
  Yobs_non_null <- Yobs[Yobs>0]
  threshold <- min(Yobs_non_null)
  if(classification){
    Ypred_binary <- ifelse(Ypred >= threshold, 1, 0)
    Ypred_binary <- as.factor(Ypred_binary)
    Yobs_binary <- as.factor(Yobs_binary)
    Ypred_binary <- factor(Ypred_binary,levels = c("0","1"))
    Yobs_binary <- factor(Yobs_binary,levels = c("0","1"))
    conf_matrix <- caret::confusionMatrix(reference = Yobs_binary, data = Ypred_binary)
    conf_matrix <- conf_matrix[["table"]]
    conf_matrix <- table(Observed = Yobs, Predicted = Ypred_binary)
   TP <- conf_matrix[2, 2]
   TN <- conf_matrix[1, 1]
   FP <- conf_matrix[1, 2]
   FN <- conf_matrix[2, 1]
   precision <- TP / (TP + FP)
   recall <- TP / (TP + FN)
   specificity <- TN / (TN + FP)
   f1_score <- 2 * (precision * recall) / (precision + recall)
   mcc = ((TP*TN)-(FP*FN)) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
   result <- c(True_positive_rate=(TP/(TP+FP)),
                True_negative_rate=(TN/(TN+FN)),
                Precision = precision,
                Recall = recall,
                Specificity = specificity,
                F1_Score = f1_score,
               MCC = mcc,
               rank = i)
   }
  else{
    Yobs_binary <- ifelse(Yobs>= threshold, 1, 0)
    Ypred_binary <- ifelse(Ypred >= threshold, 1, 0)
    Ypred_binary <- as.factor(Ypred_binary)
    Yobs_binary <- as.factor(Yobs_binary)
    Ypred_binary <- factor(Ypred_binary,levels = c("0","1"))
    Yobs_binary <- factor(Yobs_binary,levels = c("0","1"))
    conf_matrix <- caret::confusionMatrix(reference = Yobs_binary, data = Ypred_binary)
    conf_matrix <- conf_matrix[["table"]]
    TP <- conf_matrix[2, 2]
    TN <- conf_matrix[1, 1]
    FP <- conf_matrix[1, 2]
    FN <- conf_matrix[2, 1]
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    f1_score <- 2 * (precision * recall) / (precision + recall)
    mcc = ((TP*TN)-(FP*FN)) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    data_r_squared <- base::data.frame(Ypred=Ypred,Yobs=Yobs)
    model <- stats::lm(Ypred~Yobs,data=data_r_squared)
    r_squared <- summary(model)$r.squared
    result <- data.frame(True_positive_rate=(TP/(TP+FP)),
                True_negative_rate=(TN/(TN+FN)),
                Precision = precision,
                Recall = recall,
                Specificity = specificity,
                F1_Score = f1_score,
                MCC = mcc,
                R_squared = r_squared,
                rank = i)
   }
  result_list[[i]] <- result
  }
  return(result_list)
}

