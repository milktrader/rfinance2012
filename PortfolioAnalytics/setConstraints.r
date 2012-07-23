############## CUSTOM FUNCTIONS ############

pmean <- function(n=12, R, weights, geometric=TRUE){

  sum(Return.annualized(last(R, n)) * weights)
}
  
############## INITIALIZE CONSTRAINTS ######

constraintsAndObjectives  <-  constraint(

  assets         = colnames(indexes[,1:4]), 
  min            = 0.05, 
  max            = c(0.75, 0.45, 0.45, 0.15), 
  min_sum        = 0.99, 
  max_sum        = 1.01, 
  weight_seq     = generatesequence())

############## ADD OBJECTIVES #############

constraintsAndObjectives  <- add.objective(
                                           
  constraints    = constraintsAndObjectives, 
  type           = 'risk', 
  name           = 'CVaR',
  enabled        = TRUE, 
  arguments      = list(
                     p     = (1-1/12), 
                     clean = 'boudt'))  

constraintsAndObjectives  <- add.objective(
                                           
  constraints    = constraintsAndObjectives, 
  type           = 'return', 
  name           = 'pmean',
  enabled        = TRUE, 
  multiplier     = -1, 
  arguments      = list(
                     n     = 12))

############## SPECIFY SOLVER ############

myResult  <- optimize.portfolio(
  
  R               = indexes[, 1:4],
  constraints     = constraintsAndObjectives, 
  optimize_method = 'random', 
  search_size     = 100, 
  trace           = TRUE) 








