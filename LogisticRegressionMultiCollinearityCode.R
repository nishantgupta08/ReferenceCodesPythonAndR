# Forward Feature Selection Code by removing multicollinearity

var_list <- setdiff(names(data), c("DependentVariable"))

new_formula.lr <- paste0("DependentVariable", "~", var_list[1])
model_variables <- var_list[1]

for(var_index in (2:length(var_list))){
  var_name <- var_list[var_index]
  print(paste0("Testing variable : ", var_name))
  new_model_variables <- c(model_variables, var_name)
  rank_of_matrix <- rankMatrix(dtrain[ , (names(dtrain) %in% new_model_variables)])
    
  rankfindWarning <- tryCatch({rank_of_matrix <- rankMatrix(dtrain[ , (names(dtrain) %in% new_model_variables)]);0}, warning = function(w){
    print("nishant")
    print(w$message)
    if(w$message == "rankMatrix(<large sparse Matrix>, method = 'tolNorm2') coerces to dense matrix.\n Probably should rather use method = 'qr' !?"){
      1
    }})
  
  if(rankfindWarning == 1){
    print(paste0("Ignoring variable ", var_name, " due to qr Warning."))
    next
  }
  
  no_columns <- length(new_model_variables)
  print(paste0("Rank of matrix : ", rank_of_matrix, " total columns : ",no_columns))
  
  if(rank_of_matrix < no_columns){
    print(paste0("Ignoring variable ", var_name, " due to warning prediction from a rank-deficient fit may be misleading"))
    next
  }
  
  new_formula.lr <- paste0(new_formula.lr, "+" , var_name)
  print("New Regression Formula : ")
  print(new_formula.lr)
  
  glmfitWarning <- tryCatch({new_model_lr <- glm(as.formula(new_formula.lr), data=dtrain, family="binomial", y=FALSE, model=FALSE); 0}, warning = function(w){
    if(w$message == "glm.fit: fitted probabilities numerically 0 or 1 occurred"){
      new_model_lr <- NULL;
      1
    }})
  
  rankDeficitWarning <- tryCatch({lr_predict_score_train <- predict(new_model_lr, newdata = dtrain, type = "response"); 0}, warning = function(w){
    if(w$message == "prediction from a rank-deficient fit may be misleading"){
      new_model_lr <- NULL;
      lr_predict_score_train <- NULL;
      1
    }})
  
  
  if(rankDeficitWarning == 1){
    print(paste0("Ignoring variable ", var_name, " due to Rank Deficit Warning."))
    next
  }
  if(glmfitWarning == 1){
    print(paste0("Ignoring variable ", var_name, " due to Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred."))
    next
  }
  model_variables <- new_model_variables
  print("Finally Selected Model Varibales",model_variables)
  browser()
} 


#Finally you have got all features with no multicollinearity problem 
#Now one can build regularised logistic regresion modlel and check for statisticla significnace as well.

#Code to calcualte F1 Score 

getMetrics <- function (confMatrix, var, allVars) {
  allVarsWoNone <- allVars[which(!(allVars == "NA"))]
  otherVars <- allVars[which(!(allVarsWoNone == var))]
  p <- confMatrix[ c(var), c(var) ]  / sum ( confMatrix[ allVarsWoNone , c(var) ] )
  
  print("confusion matrix ")
  
  r <- confMatrix[ c(var), c(var) ]  / sum ( confMatrix[  c(var),   ] )
  f1 <- 2 * p * r / ( p + r )
  f0.5 <- 1.25 * p * r / ( 0.25*p + r )
  mcf1 <- sum( confMatrix[ c(var), otherVars ] ) / sum(  confMatrix[ c(var), allVars] )
  mcf2 <- sum( confMatrix[otherVars, c(var) ] ) / sum(  confMatrix[allVars , c(var) ] )
  
  return(c( p, r , f1 , f0.5 , mcf1 , mcf2 ))
}

modelPerformanceMetrics <- function (actual , predicted){
  
  ConfMatrix <- table ( actual, predicted )
  
  allVars <- c("cards",
               "carloan",
               "homeloan",
               "insurance",
               "loan",
               "save_invest",
               "travel",
               "Others")
  
  cards <- getMetrics(ConfMatrix,"cards", allVars)
  carloan <- getMetrics(ConfMatrix,"carloan", allVars)
  homeloan <- getMetrics(ConfMatrix,"homeloan", allVars)
  save_invest <- getMetrics(ConfMatrix,"insurance", allVars)
  insurance <- getMetrics(ConfMatrix,"loan", allVars)
  loan <- getMetrics(ConfMatrix,"save_invest", allVars)
  travel <- getMetrics(ConfMatrix,"travel", allVars)
  Others <- getMetrics(ConfMatrix,"Others", allVars)
  
  ModelPerf <- t ( data.frame (
    cards,
    carloan,
    homeloan,
    save_invest,
    insurance,
    travel,
    loan,
    Others
    ) )*100
  
  colnames ( ModelPerf ) <- c("Precision", "Recall", "F1Score", "F0.5Score", "Misclassification1", "Misclassification2")
  
  return ( ModelPerf )
}
