# ------ predict code ------

# ------ fcn ------
predict.fcn <- function(model, data) {
  # new data, transfer to matrix
  new.data <- matrix(as.numeric(data),ncol=2)
  output.layer_old <- sweep(new.data %*% model$W ,2, model$b, '+')
  output.layer <- SoftPlus(output.layer_old)
  return(output.layer)
}

# ------ nn2n ------
predict.nn2n <- function(model, data) {
  # new data, transfer to matrix
  new.data <- matrix(as.numeric(data),ncol=2)
  
  hidden.layer_old <- sweep(new.data %*% model$W1, 2, model$b1, "+")  ## Nx2 [[H1_1,H2_1],[H1_2,H2_2],...,[H1_N,H2_N]]
  hidden.layer <- SoftPlus(hidden.layer_old)
  
  output.layer_old <- sweep(hidden.layer %*% model$W2, 2, model$b2, "+")
  output.layer <- SoftPlus(output.layer_old)  ## Nx2 [[m_1,s_1],[m_2,s_2],...,[m_N,s_N]]
  
  return(output.layer)
}

# ------ nn4n ------
predict.nn4n <- function(model, data) {
  # new data, transfer to matrix
  new.data <- matrix(as.numeric(data),ncol=2)
  
  hidden.layer_old <- sweep(new.data %*% model$W1, 2, model$b1, "+")  ## Nx4 [[H1_1,..,H4_1],[H1_2,..,H4_2],...,[H1_N,..,H4_N]]
  hidden.layer <- SoftPlus(hidden.layer_old)
  
  output.layer_old <- sweep(hidden.layer %*% model$W2, 2, model$b2, "+")
  output.layer <- SoftPlus(output.layer_old)  ## Nx2 [[m_1,s_1],[m_2,s_2],...,[m_N,s_N]]
  
  return(output.layer)
}

# ------ nn3h4n ------
predict.nn3h4n <- function(model, data) {
  # new data, transfer to matrix
  new.data <- matrix(as.numeric(data),ncol=2)
  
  hidden1.layer_old <- sweep(new.data %*% model$W1, 2, model$b1, "+")  ## Nx4 [[H1_1,..,H4_1],[H1_2,..,H4_2],...,[H1_N,..,H4_N]]
  hidden1.layer <- SoftPlus(hidden1.layer_old)
  
  hidden2.layer_old <- sweep(hidden1.layer %*% model$W2, 2, model$b2, "+")  ## Nx4 [[H1_1,..,H4_1],[H1_2,..,H4_2],...,[H1_N,..,H4_N]]
  hidden2.layer <- SoftPlus(hidden2.layer_old)
  
  hidden3.layer_old <- sweep(hidden2.layer %*% model$W3, 2, model$b3, "+")  ## Nx4 [[H1_1,..,H4_1],[H1_2,..,H4_2],...,[H1_N,..,H4_N]]
  hidden3.layer <- SoftPlus(hidden3.layer_old)
  
  output.layer_old <- sweep(hidden3.layer %*% model$W4, 2, model$b4, "+")
  output.layer <- SoftPlus(output.layer_old)  ## Nx2 [[m_1,s_1],[m_2,s_2],...,[m_N,s_N]]
  
  return(output.layer)
}
