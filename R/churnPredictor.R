library("data.table")

churn_pred <- function(input, cust_id){
  if(!any(df==cust_id)){
    stop("error message")
  }

  #set columns to factors
  input$Exited <- as.factor(input$Exited)
  input$Gender <- as.factor(input$Gender)

  #creation of model for churn probability using logistic regression
  formula <- Exited~CreditScore+Gender+Age+Tenure+Balance+NumOfProducts+HasCrCard+IsActiveMember+EstimatedSalary
  glm.fit <- glm(formula, data = input, family = 'binomial')
  summary(glm.fit)

  #predict churn probability for each customer and add it as a new column 'Churn'
  input$Churn = predict(glm.fit, type = "response")

  churn<- input[input$CustomerId==cust_id, Churn]
  return(churn)
}

