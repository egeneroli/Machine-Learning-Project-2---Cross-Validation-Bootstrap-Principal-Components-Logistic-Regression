#################################### R version 3.4.0
#install.packages('readstata13')
#library(readstata13)
#library(stats)
#library(MASS)

rm(list = ls())

setwd('C:\\Users\\Evan Generoli\\Dropbox\\SaML_Project_2')

# read in data & explore it
dat <- read.dta13('voter_data_cleaned.dta')
dat <- as.data.frame(dat)
str(dat)
summary(dat)

# omit missing values for PCA & drop state & year
dat <- na.omit(dat[3:20])
summary(dat)

# create summary statistics table
table <- do.call(data.frame, 
                 list(mean = apply(dat, 2, mean),
                      sd = apply(dat, 2, sd),
                      median = apply(dat, 2, median),
                      min = apply(dat, 2, min),
                      max = apply(dat, 2, max),
                      n = apply(dat, 2, length)))
table

#PCA on only X data
pca1 <- prcomp(dat[-which(names(dat) == 'vote')], scale. = T)
names(pca1)
pca1$sdev #sqrt of eigen values
head(pca1$rotation) #loadings
head(pca1$x)#PCs
str(pca1$x)
dim(pca1$x)

# create pricipal component composite dataframe by combining the 
# transformed X matrix with the original y
dat_pc <- as.data.frame(cbind(
            as.data.frame(pca1$x), vote = unlist(dat['vote'])))

##################################################################
#split sample for cross-validation and analysis
k = 5
#randomly shuffle the data
set.seed(0)
Xy_pc <- dat_pc[sample(nrow(dat_pc)),]
Xy_std <- dat[sample(nrow(dat)),]

#create 5 equally size folds
folds <- cut(seq(1,nrow(Xy_pc)),breaks=k,labels=FALSE)

#loop over each of the 17 PCs
# this will run CV for models with each number of PCs
cv_mse_pc <- list()
for (p in 1:17){
  
  cv_mse_pc[[p]] <- numeric(k)
  cv_mse_std <- numeric(k)
  #perform 5 fold cross validation
  for(f in 1:k){
    #segement data by fold using the which() function 
    testIndexes <- which(folds==f, arr.ind=TRUE)
    Xy_pc_test <- Xy_pc[testIndexes, ]
    Xy_pc_train <- Xy_pc[-testIndexes, ]
    Xy_std_test <- Xy_std[testIndexes, ]
    Xy_std_train <- Xy_std[-testIndexes, ]
  
    #fit models
    y_col_num <- which(names(Xy_pc_train) == 'vote')
  
    logit_pc <- glm(vote ~ ., data = Xy_pc_train[ ,c(1:p, y_col_num)], 
                  family = 'binomial')  
  
    logit <- glm(vote ~ .-nchild-white, data = Xy_std_train, 
               family = 'binomial')  
  
    #generate predicted probabilities
    pred_pc <- predict(logit_pc, Xy_pc_test, type="response")
    pred_std <- predict(logit, Xy_std_test, type="response")
  
    #replace predicted probabilities with 0/1  
    for (i in 1:length(pred_pc)){   #PC models
      if (pred_pc[i] >= 0.5){
        pred_pc[i] <- 1
      } else {
        pred_pc[i] <- 0 
      }
    }
    for (i in 1:length(pred_std)){   #standard model
      if (pred_std[i] >= 0.5){
        pred_std[i] <- 1
      } else {
        pred_std[i] <- 0 
      }    
    }
    
    #save correct classification counts
    cv_mse_pc[[p]][f] <- sum(Xy_pc_test['vote'] == pred_pc)
    cv_mse_std[f] <- sum(Xy_std_test['vote'] == pred_std)
    
  }
  
  #average correct classification counts & convert to misclassification rate
  cv_mse_pc[[p]] <- 1 - mean(cv_mse_pc[[p]])/nrow(Xy_std_test['vote'])
  mpe_cv_std <- 1 - mean(cv_mse_std)/nrow(Xy_std_test['vote'])

}

#convert list to vector
mpe_cv_pc <- as.vector(unlist(cv_mse_pc, use.names=FALSE))

#print misclassification rates
mpe_cv_pc
mpe_cv_std


##################################################


#split data into training and test
set.seed(0)
index <- sample(nrow(dat), size = (.7)*nrow(dat), replace = F)

dat_std_train <- dat[index, ]
dat_pc_train <- dat_pc[index, ]

dat_std_test <- dat[-index, ]
dat_pc_test <- dat_pc[-index, ]

#########MODEL SELECTION

# fit model with each number of PCs and get BIC to identify candidate models
bic_pc <- numeric(ncol(dat_pc_train)-1)
for (i in 1:(ncol(dat_pc_train)-1)){
  y_col_num <- which(names(dat_pc_train) == 'vote')
  dat_pc_temp <- dat_pc_train[ ,c(1:i, y_col_num)]
  mod <- glm(vote ~ ., family = binomial(link = 'logit'), data = dat_pc_temp)
  bic_pc[i] <- BIC(mod)
}

#plot BIC vs number of PCs
plot(bic_pc, 
     xlab = 'Number of Components', 
     ylab = 'BIC', 
     main = 'BIC vs. Number of PCs')

# calculate percentage decrease in BIC on next point(#PCs,BIC)
bic_pc_per_chg <- numeric(length(bic_pc))
for (i in 1:length(bic_pc)){
  bic_pc_per_chg[i] <- (bic_pc[i] - bic_pc[i+1])/(bic_pc[i]) 
}
round(bic_pc_per_chg, 4)
which.max(bic_pc_per_chg)
# the highest percentage drop in BIC happens after the 5th PC (on the 6th PC)


# bootstrap - fit model on each bootstrapped resample of training set
# estimate error on original test set each time, average results
B <- 100
boot_mse_std <- numeric(B)
boot_mse_pc_6 <- numeric(B)
boot_mse_pc_8 <- numeric(B)
boot_mse_pc_10 <- numeric(B)
for (b in 1:B){
  #get indexes for bootstrap distribution
  index <- sample(nrow(dat), size = nrow(dat), replace = T)
  
  #create dataframes for each bootstrap resample of training data
  dat_boot_std <- dat_std_train[index, ]
  dat_boot_pc <- dat_pc_train[index, ]
  
  #get indexes for test data split
  testIndexes <- sample(nrow(dat), size = (1/3)*nrow(dat), replace = F)
  
  #fit models
  logit_pc_6 <- glm(vote ~ ., data = dat_boot_pc[ ,c(1:6, y_col_num)], 
                  family = 'binomial')  
  logit_pc_8 <- glm(vote ~ ., data = dat_boot_pc[ ,c(1:8, y_col_num)], 
                  family = 'binomial')  
  logit_pc_10 <- glm(vote ~ ., data = dat_boot_pc[ ,c(1:10, y_col_num)], 
                   family = 'binomial')  
  
  logit <- glm(vote ~ .-nchild-white, data = dat_boot_std, 
               family = 'binomial')  
  
  #generate predicted probabilities 
  pred_std <- predict(logit, dat_std_test, type="response")
  pred_pc_6 <- predict(logit_pc_6, dat_pc_test[ ,c(1:6, y_col_num)], type="response")
  pred_pc_8 <- predict(logit_pc_8, dat_pc_test[ ,c(1:8, y_col_num)], type="response")
  pred_pc_10 <- predict(logit_pc_10, dat_pc_test[ ,c(1:10, y_col_num)], type="response")
  
  #replace predicted probabilities with 0/1  
  for (i in 1:length(pred_pc_6)){ #6PC model
    if (pred_pc_6[i] >= 0.5){
      pred_pc_6[i] <- 1
    } else {
      pred_pc_6[i] <- 0 
    }
  }
  for (i in 1:length(pred_pc_8)){ #8PC model
    if (pred_pc_8[i] >= 0.5){
      pred_pc_8[i] <- 1
    } else {
      pred_pc_8[i] <- 0 
    }
  }
  for (i in 1:length(pred_pc_10)){ #8PC model
    if (pred_pc_10[i] >= 0.5){
      pred_pc_10[i] <- 1
    } else {
      pred_pc_10[i] <- 0 
    }
  }
  for (i in 1:length(pred_std)){ #standard model
    if (pred_std[i] >= 0.5){
      pred_std[i] <- 1
    } else {
      pred_std[i] <- 0 
    }    
  }
  
  #save correct classification counts to vector element
  boot_mse_std[b] <- sum(dat_std_test['vote'] == pred_std)
  boot_mse_pc_6[b] <- sum(dat_pc_test['vote'] == pred_pc_6)
  boot_mse_pc_8[b] <- sum(dat_pc_test['vote'] == pred_pc_8)
  boot_mse_pc_10[b] <- sum(dat_pc_test['vote'] == pred_pc_10)
  
}

# average vectors of correct classification counts from each bootstrap iteration
# & convert to misclassification rates
mpe_boot_std <- 1 - mean(boot_mse_std)/nrow(dat_pc_test['vote'])
mpe_boot_pc_6 <- 1 - mean(boot_mse_pc_6)/nrow(dat_pc_test['vote'])
mpe_boot_pc_8 <- 1 - mean(boot_mse_pc_8)/nrow(dat_pc_test['vote'])
mpe_boot_pc_10 <- 1 - mean(boot_mse_pc_10)/nrow(dat_pc_test['vote'])

#print misclassification rates
mpe_boot_std
mpe_boot_pc_6
mpe_boot_pc_8
mpe_boot_pc_10






