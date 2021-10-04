# ------ train code ------- 

# ------ fcn ------
train.fcn <- function(x=1:2, y=3, traindata, testdata,  ## x:1:2 (ensemble members col) / y:3 (obs col)
                      # max iteration steps
                      maxit=2000,
                      # delta loss 
                      abstol=1e-4,
                      # learning rate
                      lr = 1e-4,
                      # show results every 'display' step
                      display = 100,
                      # set.seed
                      random.seed = 1234,
                      # unequalweight for training data
                      unequalweight = NULL)
{
  # to make the case reproducible.
  set.seed(random.seed)
  
  # total number of training set
  N <- nrow(traindata)  ## 3
  
  if ((is.null(unequalweight)==TRUE) | (length(unique(unequalweight))==1)){
    
    # extract the data and label
    # don't need attribute 
    X <- unname(matrix(as.numeric(traindata[,x]),ncol=2))  ## mean & sd
    Y <- matrix(as.numeric(traindata[,y]),ncol=1)  ## obs
    # create index for both row and col
    Y_index <- cbind(1:N, Y)
    
  }else{
    
    ## given unequal weight to different training data
    unew <- as.vector(as.numeric(unequalweight))  ## length:N
    # extract the data and label
    # don't need attribute 
    X_old <- unname(matrix(as.numeric(traindata[,x]),ncol=2))  ## mean & sd
    X <- X_old*unew
    Y_old <- matrix(as.numeric(traindata[,y]),ncol=1)  ## obs
    Y <- Y_old*unew
    # create index for both row and col
    Y_index <- cbind(1:N, Y)
    
  }
  
  # number of input features
  D <- ncol(X)  ## D=2
  # number of second layer (?)
  K <- 2  ## K=2
  
  # create and init weights and bias 
  W <- 0.01*matrix(rnorm(D*K, sd=0.5), nrow=D, ncol=K)
  b <- matrix(0, nrow=1, ncol=K)
  
  # use all train data to update weights since it's a small dataset
  batchsize <- N
  
  # Training the network
  i <- 0
  while(i < maxit || loss < abstol) {
    
    # iteration index
    i <- i +1
    
    # forward ....
    output_old <- sweep(X %*% W, 2, b, "+")
    output <- SoftPlus(output_old)  ## §Æ±æ¯àÂÇ¥ÑSoftPlusÁ×§Ksd<0
    
    # compute the loss
    loss <- crps_norm(Y, mean=output[,1], sd=output[,2])
    
    # display results and update model
    # if( i %% display == 0) {
    #   if(!is.null(testdata)) {
    #     model <- list( D = D,
    #                    K = K,
    #                    # weights and bias
    #                    W = W,
    #                    b = b)
    #     testdata <- t(data.matrix(testdata))
    #     test_output <- predict.fcn(model, testdata[-y])
    #     meanCRPS <- mean(crps_norm(testdata[y], mean=test_output[,1], sd=test_output[,2]))
    #     cat("i=", i, "train_loss=", loss, "test_loss=", meanCRPS, "\n")  ## , "W=", W, "b=", b, "output=", output
    #   } else {
    #     cat("i=", i, "train_loss=", loss, "\n")
    #   }
    # }
    
    # backward ....
    dLdmu <- gradcrps_norm(Y, location=output[,1], scale=output[,2])[,1]
    dLdsd <- gradcrps_norm(Y, location=output[,1], scale=output[,2])[,2]
    dW <- matrix(c(dLdmu*diffSoftPlus(output_old[,1])*X[,1], dLdsd*diffSoftPlus(output_old[,2])*X[,1],
                   dLdmu*diffSoftPlus(output_old[,1])*X[,2], dLdsd*diffSoftPlus(output_old[,2])*X[,2]), ncol=4, byrow=F)  # dW1-dW4 Nx4
    db <- matrix(c(dLdmu*diffSoftPlus(output_old[,1])*1, dLdsd*diffSoftPlus(output_old[,2])*1), ncol=2, byrow=F)  # db1-b2 Nx2
    
    # update ....
    W <- W - lr*matrix(apply(dW, 2, sum), 2, byrow=T)
    b <- b - lr*matrix(apply(db, 2, sum), 1, byrow=T)
    if(is.nan(W)||is.nan(b)){print(i);print(output);print(loss);print(dW);print(db);print(W);print(b); break}
    
  }
  
  # final results
  model <- list( D = D,
                 K = K,
                 # weights and bias
                 W=W,
                 b=b)
  
  return(model)
}


# ------ nn2n ------
train.nn2n <- function(x=1:2, y=3, traindata, testdata,  ## x:1:2 (ensemble members col) / y:3 (obs col)
                     # max iteration steps
                     maxit=2000,
                     # delta loss 
                     abstol=1e-4,
                     # learning rate
                     lr = 1e-4,
                     # show results every 'display' step
                     display = 100,
                     # set.seed
                     random.seed = 1234,
                     # unequalweight for training data
                     unequalweight = NULL)
{
  # to make the case reproducible.
  set.seed(random.seed)
  
  # total number of training set
  N <- nrow(traindata)  ## 3
  
  if ((is.null(unequalweight)==TRUE) | (length(unique(unequalweight))==1)){
    
    # extract the data and label
    # don't need attribute 
    X <- unname(matrix(as.numeric(traindata[,x]),ncol=2))  ## mean & sd
    Y <- matrix(as.numeric(traindata[,y]),ncol=1)  ## obs
    # create index for both row and col
    Y_index <- cbind(1:N, Y)
    
  }else{
    
    ## given unequal weight to different training data
    unew <- as.vector(as.numeric(unequalweight))  ## length:N
    # extract the data and label
    # don't need attribute 
    X_old <- unname(matrix(as.numeric(traindata[,x]),ncol=2))  ## mean & sd
    X <- X_old*unew
    Y_old <- matrix(as.numeric(traindata[,y]),ncol=1)  ## obs
    Y <- Y_old*unew
    # create index for both row and col
    Y_index <- cbind(1:N, Y)
    
  }
  
  # number of input features
  D <- ncol(X)  ## D=2
  # number of hidden layer
  H <- 2
  # number of second layer (?)
  K <- 2  ## K=2
  
  # create and init weights and bias 
  W1 <- 0.01*matrix(rnorm(D*H, sd=0.5), nrow=D, ncol=H)  ## 2x2 [[w1.1,w1.2],[w1.3,w1.4]]
  b1 <- matrix(0, nrow=1, ncol=H)  ## 1x2 [b1.1,b1.2]
  
  W2 <- 0.01*matrix(rnorm(H*K, sd=0.5), nrow=H, ncol=K)  ## 2x2
  b2 <- matrix(0, nrow=1, ncol=K)  ## 1x2
  
  # use all train data to update weights since it's a small dataset
  batchsize <- N
  
  # Training the network
  i <- 0
  while(i < maxit || loss < abstol) {
    
    # iteration index
    i <- i +1
    
    # forward (hidden) ....
    hidden_old <- sweep(X %*% W1, 2, b1, "+")  ## Nx2 [[H1_1,H2_1],[H1_2,H2_2],...,[H1_N,H2_N]]
    hidden <- SoftPlus(hidden_old)
    
    output_old <- sweep(hidden %*% W2, 2, b2, "+")
    output <- SoftPlus(output_old)  ## Nx2 [[m_1,s_1],[m_2,s_2],...,[m_N,s_N]]
    
    # compute the loss
    loss <- crps_norm(Y, mean=output[,1], sd=output[,2])
    
    # backward ....
    dLdmu <- gradcrps_norm(Y, location=output[,1], scale=output[,2])[,1]
    dLdsd <- gradcrps_norm(Y, location=output[,1], scale=output[,2])[,2]
    
    dSPHdH <- diffSoftPlus(hidden_old)  ## Nx2, dSoftPlus(H1)d(H1) & dSoftPlus(H2)d(H2)
    dSPmsdms <- diffSoftPlus(output_old)  ## Nx2, dSoftPlus(mu)d(mu) & dSoftPlus(sd)d(sd)
    # ------------------------------------------------- #
    dmudW1 <- matrix(c(dSPmsdms[,1] * W2[1,1] * dSPHdH[,1] * X[,1],
                       dSPmsdms[,1] * W2[2,1] * dSPHdH[,2] * X[,1],
                       dSPmsdms[,1] * W2[1,1] * dSPHdH[,1] * X[,2],
                       dSPmsdms[,1] * W2[2,1] * dSPHdH[,2] * X[,2]), ncol=4, byrow=F)  ## Nx4
    dsddW1 <- matrix(c(dSPmsdms[,2] * W2[1,2] * dSPHdH[,1] * X[,1],
                       dSPmsdms[,2] * W2[2,2] * dSPHdH[,2] * X[,1],
                       dSPmsdms[,2] * W2[1,2] * dSPHdH[,1] * X[,2],
                       dSPmsdms[,2] * W2[2,2] * dSPHdH[,2] * X[,2]), ncol=4, byrow=F)  ## Nx4
    dW1 <- dLdmu*dmudW1 + dLdsd*dsddW1  ## Nx4
    # ------------------------------------------------- #
    dW2 <- matrix(c(dLdmu * dSPmsdms[,1] * hidden[,1],
                    dLdsd * dSPmsdms[,2] * hidden[,1],
                    dLdmu * dSPmsdms[,1] * hidden[,2],
                    dLdsd * dSPmsdms[,2] * hidden[,2]), ncol=4, byrow=F)  ## Nx4
    # ------------------------------------------------- #
    dmudb1 <- matrix(c(dSPmsdms[,1] * W2[1,1] * dSPHdH[,1],
                       dSPmsdms[,1] * W2[2,1] * dSPHdH[,2]), ncol=2, byrow=F)  ## Nx2
    dsddb1 <- matrix(c(dSPmsdms[,2] * W2[1,2] * dSPHdH[,1],
                       dSPmsdms[,2] * W2[2,2] * dSPHdH[,2]), ncol=2, byrow=F)  ## Nx2
    db1 <- dLdmu*dmudb1 + dLdsd*dsddb1  ## Nx2
    # ------------------------------------------------- #
    db2 <- matrix(c(dLdmu * dSPmsdms[,1],
                    dLdsd * dSPmsdms[,2]), ncol=2, byrow=F)
    # ------------------------------------------------- #
    
    
    # update ....
    W1 <- W1 - lr*matrix(apply(dW1, 2, sum), 2, byrow=T)
    W2 <- W2 - lr*matrix(apply(dW2, 2, sum), 2, byrow=T)
    b1 <- b1 - lr*matrix(apply(db1, 2, sum), 1, byrow=T)
    b2 <- b2 - lr*matrix(apply(db2, 2, sum), 1, byrow=T)
    
    if(is.nan(W1)||is.nan(W2)||is.nan(b1)||is.nan(b2)){print(i);print(hidden);print(output);print(loss);print(dW1);print(dW2);print(db1);print(db2);print(W1);print(W2);print(b1);print(b2); break}
    
  }
  
  # final results
  model <- list( D = D,
                 H = H,
                 K = K,
                 # weights and bias
                 W1=W1,
                 W2=W2,
                 b1=b1,
                 b2=b2)
  
  return(model)
}

# ------ nn4n ------
train.nn4n <- function(x=1:2, y=3, traindata, testdata,  ## x:1:2 (ensemble members col) / y:3 (obs col)
                       # max iteration steps
                       maxit=2000,
                       # delta loss 
                       abstol=1e-4,
                       # learning rate
                       lr = 1e-4,
                       # show results every 'display' step
                       display = 100,
                       # set.seed
                       random.seed = 1234,
                       # unequalweight for training data
                       unequalweight = NULL)
{
  # to make the case reproducible.
  set.seed(random.seed)
  
  # total number of training set
  N <- nrow(traindata)  ## 3
  
  if ((is.null(unequalweight)==TRUE) | (length(unique(unequalweight))==1)){
    
    # extract the data and label
    # don't need attribute 
    X <- unname(matrix(as.numeric(traindata[,x]),ncol=2))  ## mean & sd
    Y <- matrix(as.numeric(traindata[,y]),ncol=1)  ## obs
    # create index for both row and col
    Y_index <- cbind(1:N, Y)
    
  }else{
    
    ## given unequal weight to different training data
    unew <- as.vector(as.numeric(unequalweight))  ## length:N
    # extract the data and label
    # don't need attribute 
    X_old <- unname(matrix(as.numeric(traindata[,x]),ncol=2))  ## mean & sd
    X <- X_old*unew
    Y_old <- matrix(as.numeric(traindata[,y]),ncol=1)  ## obs
    Y <- Y_old*unew
    # create index for both row and col
    Y_index <- cbind(1:N, Y)
    
  }

  # number of input features
  D <- ncol(X)  ## D=2
  # number of hidden layer
  H <- 4
  # number of second layer (?)
  K <- 2  ## K=2
  
  # create and init weights and bias 
  W1 <- 0.01*matrix(rnorm(D*H, sd=0.5), nrow=D, ncol=H)  ## 2x4 [[w1.1,..,w1.4],[w1.5,..,w1.8]]
  b1 <- matrix(0, nrow=1, ncol=H)  ## 1x4 [b1.1,b1.2]
  
  W2 <- 0.01*matrix(rnorm(H*K, sd=0.5), nrow=H, ncol=K)  ## 4x2 [[w2.1,w2.2],[w2.3,w2.4],...[w2.7,w2.8]]
  b2 <- matrix(0, nrow=1, ncol=K)  ## 1x2
  
  # use all train data to update weights since it's a small dataset
  batchsize <- N
  
  # Training the network
  i <- 0
  while(i < maxit || loss < abstol) {
    
    # iteration index
    i <- i +1
    
    # forward (hidden) ....
    hidden_old <- sweep(X %*% W1, 2, b1, "+")  ## Nx4 [[H1_1,...,H4_1],[H1_2,...,H4_2],...,[H1_N,...,H4_N]]
    hidden <- SoftPlus(hidden_old)
    
    output_old <- sweep(hidden %*% W2, 2, b2, "+")  ## Nx2
    output <- SoftPlus(output_old)  ## Nx2 [[m_1,s_1],[m_2,s_2],...,[m_N,s_N]]
    
    # compute the loss
    loss <- crps_norm(Y, mean=output[,1], sd=output[,2])
    
    # backward ....
    dLdmu <- gradcrps_norm(Y, location=output[,1], scale=output[,2])[,1]
    dLdsd <- gradcrps_norm(Y, location=output[,1], scale=output[,2])[,2]
    
    dSPHdH <- diffSoftPlus(hidden_old)  ## Nx2, dSoftPlus(H1)d(H1) & dSoftPlus(H2)d(H2)
    dSPmsdms <- diffSoftPlus(output_old)  ## Nx2, dSoftPlus(mu)d(mu) & dSoftPlus(sd)d(sd)
    # ------------------------------------------------- #
    dmudW1 <- matrix(c(dSPmsdms[,1] * W2[1,1] * dSPHdH[,1] * X[,1],  # dmudw1_1
                       dSPmsdms[,1] * W2[2,1] * dSPHdH[,2] * X[,1],  # dmudw1_2
                       dSPmsdms[,1] * W2[3,1] * dSPHdH[,3] * X[,1],  # dmudw1_3
                       dSPmsdms[,1] * W2[4,1] * dSPHdH[,4] * X[,1],  # dmudw1_4
                       dSPmsdms[,1] * W2[1,1] * dSPHdH[,1] * X[,2],  # dmudw1_5
                       dSPmsdms[,1] * W2[2,1] * dSPHdH[,2] * X[,2],  # dmudw1_6
                       dSPmsdms[,1] * W2[3,1] * dSPHdH[,3] * X[,2],  # dmudw1_7
                       dSPmsdms[,1] * W2[4,1] * dSPHdH[,4] * X[,2]), ncol=8, byrow=F)  ## Nx8
    
    dsddW1 <- matrix(c(dSPmsdms[,2] * W2[1,2] * dSPHdH[,1] * X[,1],  # dsddw1_1
                       dSPmsdms[,2] * W2[2,2] * dSPHdH[,2] * X[,1],  # dsddw1_2
                       dSPmsdms[,2] * W2[3,2] * dSPHdH[,3] * X[,1],  # dsddw1_3
                       dSPmsdms[,2] * W2[4,2] * dSPHdH[,4] * X[,1],  # dsddw1_4
                       dSPmsdms[,2] * W2[1,2] * dSPHdH[,1] * X[,2],  # dsddw1_5
                       dSPmsdms[,2] * W2[2,2] * dSPHdH[,2] * X[,2],  # dsddw1_6
                       dSPmsdms[,2] * W2[3,2] * dSPHdH[,3] * X[,2],  # dsddw1_7
                       dSPmsdms[,2] * W2[4,2] * dSPHdH[,4] * X[,2]), ncol=8, byrow=F)  ## Nx8
    
    dW1 <- dLdmu*dmudW1 + dLdsd*dsddW1  ## Nx8
    # ------------------------------------------------- #
    dW2 <- matrix(c(dLdmu * dSPmsdms[,1] * hidden[,1],
                    dLdsd * dSPmsdms[,2] * hidden[,1],
                    dLdmu * dSPmsdms[,1] * hidden[,2],
                    dLdsd * dSPmsdms[,2] * hidden[,2],
                    dLdmu * dSPmsdms[,1] * hidden[,3],
                    dLdsd * dSPmsdms[,2] * hidden[,3],
                    dLdmu * dSPmsdms[,1] * hidden[,4],
                    dLdsd * dSPmsdms[,2] * hidden[,4]), ncol=8, byrow=F)  ## Nx8
    # ------------------------------------------------- #
    dmudb1 <- matrix(c(dSPmsdms[,1] * W2[1,1] * dSPHdH[,1],
                       dSPmsdms[,1] * W2[2,1] * dSPHdH[,2],
                       dSPmsdms[,1] * W2[3,1] * dSPHdH[,3],
                       dSPmsdms[,1] * W2[4,1] * dSPHdH[,4]), ncol=4, byrow=F)  ## Nx4
    dsddb1 <- matrix(c(dSPmsdms[,2] * W2[1,2] * dSPHdH[,1],
                       dSPmsdms[,2] * W2[2,2] * dSPHdH[,2],
                       dSPmsdms[,2] * W2[3,2] * dSPHdH[,3],
                       dSPmsdms[,2] * W2[4,2] * dSPHdH[,4]), ncol=4, byrow=F)  ## Nx4
    db1 <- dLdmu*dmudb1 + dLdsd*dsddb1  ## Nx4
    # ------------------------------------------------- #
    db2 <- matrix(c(dLdmu * dSPmsdms[,1],
                    dLdsd * dSPmsdms[,2]), ncol=2, byrow=F)
    # ------------------------------------------------- #
    
    
    # update ....
    W1 <- W1 - lr*matrix(apply(dW1, 2, sum), 2, byrow=T)
    W2 <- W2 - lr*matrix(apply(dW2, 2, sum), 4, byrow=T)
    b1 <- b1 - lr*matrix(apply(db1, 2, sum), 1, byrow=T)
    b2 <- b2 - lr*matrix(apply(db2, 2, sum), 1, byrow=T)
    
    if(is.nan(W1)||is.nan(W2)||is.nan(b1)||is.nan(b2)){print(i);print(hidden);print(output);print(loss);print(dW1);print(dW2);print(db1);print(db2);print(W1);print(W2);print(b1);print(b2); break}
    
  }
  
  # final results
  model <- list( D = D,
                 H = H,
                 K = K,
                 # weights and bias
                 W1=W1,
                 W2=W2,
                 b1=b1,
                 b2=b2)
  
  return(model)
}

# ------ nn3h4n ------
train.nn3h4n <- function(x=1:2, y=3, traindata, testdata,  ## x:1:2 (ensemble members col) / y:3 (obs col)
                         # max iteration steps
                         maxit=2000,
                         # delta loss 
                         abstol=1e-4,
                         # learning rate
                         lr = 1e-4,
                         # show results every 'display' step
                         display = 100,
                         # set.seed
                         random.seed = 1234,
                         # unequalweight for training data
                         unequalweight = NULL)
{
  # to make the case reproducible.
  set.seed(random.seed)
  
  # total number of training set
  N <- nrow(traindata)  ## 3
  
  if ((is.null(unequalweight)==TRUE) | (length(unique(unequalweight))==1)){
    
    # extract the data and label
    # don't need attribute 
    X <- unname(matrix(as.numeric(traindata[,x]),ncol=2))  ## mean & sd
    Y <- matrix(as.numeric(traindata[,y]),ncol=1)  ## obs
    # create index for both row and col
    Y_index <- cbind(1:N, Y)
    
  }else{
    
    ## given unequal weight to different training data
    unew <- as.vector(as.numeric(unequalweight))  ## length:N
    # extract the data and label
    # don't need attribute 
    X_old <- unname(matrix(as.numeric(traindata[,x]),ncol=2))  ## mean & sd
    X <- X_old*unew
    Y_old <- matrix(as.numeric(traindata[,y]),ncol=1)  ## obs
    Y <- Y_old*unew
    # create index for both row and col
    Y_index <- cbind(1:N, Y)
    
  }
  
  # number of input features
  D <- ncol(X)  ## D=2
  # number of hidden layer
  H1 <- 4
  H2 <- 4
  H3 <- 4
  # number of second layer (?)
  K <- 2  ## K=2
  
  # create and init weights and bias 
  W1 <- 0.01*matrix(rnorm(D*H1, sd=0.5), nrow=D, ncol=H1)  ## 2x4 [[w1.1,..,w1.4],[w1.5,..,w1.8]]
  b1 <- matrix(0, nrow=1, ncol=H1)  ## 1x4 [b1.1,b1.2]
  
  W2 <- 0.01*matrix(rnorm(H1*H2, sd=0.5), nrow=H1, ncol=H2)  ## 4x4 [[w2.1,..,w2.4],[w2.5,..,w2.8],...,[w2.13,..,w2.16]]
  b2 <- matrix(0, nrow=1, ncol=H2)  ## 1x2
  
  W3 <- 0.01*matrix(rnorm(H2*H3, sd=0.5), nrow=H2, ncol=H3)  ## 4x2 [[w3.1,..,w3.4],[w3.5,..,w3.8],...,[w3.13,..,w3.16]]
  b3 <- matrix(0, nrow=1, ncol=H3)  ## 1x2
  
  W4 <- 0.01*matrix(rnorm(H3*K, sd=0.5), nrow=H3, ncol=K)  ## 4x2 [[w4.1,w4.2],[w4.3,w4.4],...[w4.7,w4.8]]
  b4 <- matrix(0, nrow=1, ncol=K)  ## 1x2
  
  # use all train data to update weights since it's a small dataset
  batchsize <- N
  
  # Training the network
  i <- 0
  while(i < maxit || loss < abstol) {
    
    # iteration index
    i <- i +1
    
    # forward (hidden) ....
    hidden1_old <- sweep(X %*% W1, 2, b1, "+")  ## Nx4 [[H11_1,...,H14_1],[H11_2,...,H14_2],...,[H11_N,...,H14_N]]
    hidden1 <- SoftPlus(hidden1_old)
    
    hidden2_old <- sweep(hidden1 %*% W2, 2, b2, "+")  ## Nx4 [[H21_1,...,H24_1],[H21_2,...,H24_2],...,[H21_N,...,H24_N]]
    hidden2 <- SoftPlus(hidden2_old)
    
    hidden3_old <- sweep(hidden2 %*% W3, 2, b3, "+")  ## Nx4 [[H31_1,...,H34_1],[H31_2,...,H34_2],...,[H31_N,...,H34_N]]
    hidden3 <- SoftPlus(hidden3_old)
    
    output_old <- sweep(hidden3 %*% W4, 2, b4, "+")  ## Nx2
    output <- SoftPlus(output_old)  ## Nx2 [[m_1,s_1],[m_2,s_2],...,[m_N,s_N]]
    
    # compute the loss
    loss <- crps_norm(Y, mean=output[,1], sd=output[,2])
    
    # backward ....
    dLdmu <- gradcrps_norm(Y, location=output[,1], scale=output[,2])[,1]
    dLdsd <- gradcrps_norm(Y, location=output[,1], scale=output[,2])[,2]
    # ------------------------------------------------- #
    dSPmsdms <- diffSoftPlus(output_old)  ## Nx2, dSoftPlus(mu)d(mu) & dSoftPlus(sd)d(sd)
    # ------------------------------------------------- #
    dSPH1dH1 <- diffSoftPlus(hidden1_old)
    dSPH2dH2 <- diffSoftPlus(hidden2_old)
    dSPH3dH3 <- diffSoftPlus(hidden3_old)
    # ------------------------------------------------- #
    dW_f <- function(SubNo_dW, No_dW){  ## SubNo_dW:W1-W4, No_dW:W1_1-W1_8 or W2_1-W2-16...
      cutgroup <- as.numeric(cut(1:16, (ifelse(SubNo_dW!=4, 4, 8)), labels=c(1:(ifelse(SubNo_dW!=4, 4, 8)))))
      basematrix <- matrix(diag(4)[ifelse(No_dW%%4!=0, No_dW%%4, 4),], N, 4, byrow=T)
      ## dW1
      if (SubNo_dW==1){
        doldH1dW1 <- basematrix*matrix(X[,cutgroup[No_dW]], N, 4)  ## Nx4
        doldH2dW1 <- (dSPH1dH1 * doldH1dW1) %*% W2
        doldH3dW1 <- (dSPH2dH2 * doldH2dW1) %*% W3
        dW1 <- dLdmu * dSPmsdms[,1] * (dSPH3dH3 * doldH3dW1) %*% W4[,1] + 
          dLdsd * dSPmsdms[,2] * (dSPH3dH3 * doldH3dW1) %*% W4[,2]
        return(dW1)
      }else if (SubNo_dW==2){
        doldH2dW2 <- basematrix*matrix(hidden1[,cutgroup[No_dW]], N, 4)  ## Nx4
        doldH3dW2 <- (dSPH2dH2 * doldH2dW2) %*% W3
        dW2 <- dLdmu * dSPmsdms[,1] * (dSPH3dH3 * doldH3dW2) %*% W4[,1] + 
          dLdsd * dSPmsdms[,2] * (dSPH3dH3 * doldH3dW2) %*% W4[,2]
        return(dW2)
      }else if (SubNo_dW==3){
        doldH3dW3 <- basematrix*matrix(hidden2[,cutgroup[No_dW]], N, 4)  ## Nx4
        dW3 <- dLdmu * dSPmsdms[,1] * (dSPH3dH3 * doldH3dW3) %*% W4[,1] + 
          dLdsd * dSPmsdms[,2] * (dSPH3dH3 * doldH3dW3) %*% W4[,2]
        return(dW3)
      }else if (SubNo_dW==4){
        if (No_dW%%2==1){
          dW4 <- dLdmu * dSPmsdms[,1] * hidden3[,cutgroup[No_dW]]
        }else if (No_dW%%2==0){
          dW4 <- dLdsd * dSPmsdms[,2] * hidden3[,cutgroup[No_dW]]
        }
        return(dW4)
      }
    }
    # ------------------------------------------------- #
    dW1 <- cbind(dW_f(1,1),dW_f(1,2),dW_f(1,3),dW_f(1,4),
                 dW_f(1,5),dW_f(1,6),dW_f(1,7),dW_f(1,8))   ## Nx8
    dW2 <- cbind(dW_f(2,1),dW_f(2,2),dW_f(2,3),dW_f(2,4),
                 dW_f(2,5),dW_f(2,6),dW_f(2,7),dW_f(2,8),
                 dW_f(2,9),dW_f(2,10),dW_f(2,11),dW_f(2,12),
                 dW_f(2,13),dW_f(2,14),dW_f(2,15),dW_f(2,16))   ## Nx16
    dW3 <- cbind(dW_f(3,1),dW_f(3,2),dW_f(3,3),dW_f(3,4),
                 dW_f(3,5),dW_f(3,6),dW_f(3,7),dW_f(3,8),
                 dW_f(3,9),dW_f(3,10),dW_f(3,11),dW_f(3,12),
                 dW_f(3,13),dW_f(3,14),dW_f(3,15),dW_f(3,16))   ## Nx16
    dW4 <- cbind(dW_f(4,1),dW_f(4,2),dW_f(4,3),dW_f(4,4),
                 dW_f(4,5),dW_f(4,6),dW_f(4,7),dW_f(4,8))   ## Nx8
    # ------------------------------------------------- #
    db_f <- function(SubNo_db, No_db){  ## SubNo_dW:W1-W4, No_dW:W1_1-W1_8 or W2_1-W2-16...
      basematrix <- matrix(diag(4)[ifelse(No_db%%4!=0, No_db%%4, 4),], N, 4, byrow=T)
      ## dW1
      if (SubNo_db==1){
        doldH2db1 <- (dSPH1dH1 * basematrix) %*% W2
        doldH3db1 <- (dSPH2dH2 * doldH2db1) %*% W3
        db1 <- dLdmu * dSPmsdms[,1] * (dSPH3dH3 * doldH3db1) %*% W4[,1] + 
          dLdsd * dSPmsdms[,2] * (dSPH3dH3 * doldH3db1) %*% W4[,2]
        return(db1)
      }else if (SubNo_db==2){
        doldH3db2 <- (dSPH2dH2 * basematrix) %*% W3
        db2 <- dLdmu * dSPmsdms[,1] * (dSPH3dH3 * doldH3db2) %*% W4[,1] + 
          dLdsd * dSPmsdms[,2] * (dSPH3dH3 * doldH3db2) %*% W4[,2]
        return(db2)
      }else if (SubNo_db==3){
        db3 <- dLdmu * dSPmsdms[,1] * (dSPH3dH3 * basematrix) %*% W4[,1] + 
          dLdsd * dSPmsdms[,2] * (dSPH3dH3 * basematrix) %*% W4[,2]
        return(db3)
      }else if (SubNo_db==4){
        if (No_db==1){
          db4 <- dLdmu * dSPmsdms[,1]
        }else if (No_db==2){
          db4 <- dLdsd * dSPmsdms[,2]
        }
        return(db4)
      }
    }
    db1 <- cbind(db_f(1,1),db_f(1,2),db_f(1,3),db_f(1,4))   ## Nx4
    db2 <- cbind(db_f(2,1),db_f(2,2),db_f(2,3),db_f(2,4))   ## Nx4
    db3 <- cbind(db_f(3,1),db_f(3,2),db_f(3,3),db_f(3,4))   ## Nx4
    db4 <- cbind(db_f(4,1),db_f(4,2))   ## Nx2
    
    # ------------------------------------------------- #
    
    # update ....
    W1 <- W1 - lr*matrix(apply(dW1, 2, sum), 2, byrow=T)   ## 2x4
    W2 <- W2 - lr*matrix(apply(dW2, 2, sum), 4, byrow=T)   ## 4x4
    W3 <- W3 - lr*matrix(apply(dW3, 2, sum), 4, byrow=T)   ## 4x4
    W4 <- W4 - lr*matrix(apply(dW4, 2, sum), 4, byrow=T)   ## 4x2
    
    b1 <- b1 - lr*matrix(apply(db1, 2, sum), 1, byrow=T)
    b2 <- b2 - lr*matrix(apply(db2, 2, sum), 1, byrow=T)
    b3 <- b3 - lr*matrix(apply(db3, 2, sum), 1, byrow=T)
    b4 <- b4 - lr*matrix(apply(db4, 2, sum), 1, byrow=T)
    
    if(is.nan(W1)||is.nan(W2)||is.nan(W3)||is.nan(W4)||is.nan(b1)||is.nan(b2)||is.nan(b3)||is.nan(b4)){
      print(i);print(hidden1);print(hidden2);print(hidden3);print(output);print(loss);
      print(dW1);print(dW2);print(dW3);print(dW4);print(db1);print(db2);print(db3);print(db4);
      print(W1);print(W2);print(W3);print(W4);print(b1);print(b2);print(b3);print(b4);break
    }
  }
  
  # final results
  model <- list( D = D,
                 H1 = H1,
                 H2 = H2,
                 H3 = H3,
                 K = K,
                 # weights and bias
                 W1=W1,
                 W2=W2,
                 W3=W3,
                 W4=W4,
                 b1=b1,
                 b2=b2,
                 b3=b3,
                 b4=b4)
  
  return(model)
}
