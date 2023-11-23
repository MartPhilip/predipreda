#' Computing metric of random forest quality based on the training data used for optimization
#'
#' @param Yobs numeric vector
#' @param Ypred numeric vector
#' @param method 'regression' or 'classification'
#' @param threshold numeric value
#'
#' @return
#' @export
#'
#' @examples
random_forest_quality <- function(Yobs,Ypred,method,threshold=NULL){
  if(!is.null(threshold)){threshold <- 0.5}
  if(method=='classification'){
   Ypred_binary <- ifelse(Ypred > threshold, 1, 0)
   conf_matrix <- table(Actual = Yobs, Predicted = Ypred_binary)
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
               MCC = mcc)
   return(result)
  }
  if(method=='regression'){
    Yobs_binary <- ifelse(Yobs> threshold, 1, 0)
    Ypred_binary <- ifelse(Ypred > threshold, 1, 0)
    conf_matrix <- base::table(Actual = Yobs_binary, Predicted = Ypred_binary)
    TP <- conf_matrix[2, 2]
    TN <- conf_matrix[1, 1]
    FP <- conf_matrix[1, 2]
    FN <- conf_matrix[2, 1]
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    f1_score <- 2 * (precision * recall) / (precision + recall)
    mcc = ((TP*TN)-(FP*FN)) / sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    data <- base::data.frame(Ypred=Ypred,Yobs=Yobs)
    model <- stats::lm(Ypred~Yobs,data=data)
    r_squared <- summary(model)$r.squared
    result <- data.frame(True_positive_rate=(TP/(TP+FP)),
                True_negative_rate=(TN/(TN+FN)),
                Precision = precision,
                Recall = recall,
                Specificity = specificity,
                F1_Score = f1_score,
                MCC = mcc,
                R_squared = r_squared)
    return(result)
  }
}

