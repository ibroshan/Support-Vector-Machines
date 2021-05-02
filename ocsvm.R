##  INITIAL SETUP ##


# Set Working Directory
setwd("/home/dir")

# Setup library path
.libPaths("/R/library")

# Load required packages

libs <- c("data.table", "dtplyr", "dplyr", "e1071", "kernlab")

# Ensure Strings aren't converted to factors

lapply(libs, require, character.only = TRUE)


source("/dir/supportingfunctions.R") #Supporting functions

########## Load data set ###############

con_string <- "RtoTD"

con <- odbcConnect(con_string)

# Create a Table to hold data

qry <- #extract data from the database

# Extract data into data table

df <- as.data.table(sqlQuery(con, qry, believeNRows = FALSE))
index <- as.numeric(row.names(df))
df <- cbind(index, df)

df1 <- df[,5]

####################

X <- df1
X_prediction <- df1

str(X)
X <- as.data.frame(X)
X_prediction <- as.data.frame(X_prediction)


#################### Delete variables with zero variance ##############

Var0Variable = variables_zerovariance(X)

#rm = colnames(X) %in% names(Var0Variable)

if (length(Var0Variable) != 0) {
  X = X[,-Var0Variable]
  X_prediction = X_prediction[, -Var0Variable]
}

# Autoscale explanatory variable (X)
X <- scale(X, center = TRUE, scale = TRUE)

# Decide nu - ratio of outlier samples in data set
Nu = 0.05 

# Decide candidates of gamma - parameter in Gaussian kernel

CandidatesOfGamma <- 2^( -20:10 );

# Calculate gram matrix of Gaussian kernel and its variance for each gamma candidate
# Decide the optimal gamma with the maximum variance value

OptimalGamma = optimize_gamma_grammatrix( X, CandidatesOfGamma)

# tuned <- tune.svm(X, rep(1, length=nrow(X)), data = X,
#                   nu =  0.001:0.5,
#                   gamma = 10^(-2:0), 
#                   type='one-classification');

# Construct OCSVM

OCSVMResult <- svm(X, rep(1, length=nrow(X)) , type="one-classification", kernel="radial", gamma=OptimalGamma, nu=Nu, scale=FALSE)

# Estimate whether training samples are outliers or not, based on OCSVM in 6.

CalculatedY <- predict(OCSVMResult, X)
summary(CalculatedY)

# In prediction, subtract the mean in the autoscalling of X in 1. from X-variables, and then, divide X-variables by the standard deviation in the autoscalling of X in 1., for new samples

for (X_predictionNum in 1:nrow(X_prediction)) {
  X_prediction[X_predictionNum,] <- (X_prediction[X_predictionNum,]-attr(X,"scaled:center")) / attr(X,"scaled:scale")
}

# Estimate whether new samples are outliers or not, based on OCSVM in 6.

PredictedY <- predict(OCSVMResult, X_prediction)
summary(PredictedY)

################ Merge results back to data frame ###################


PredictedY <- as.data.frame(PredictedY)
PredictedY$index <- as.numeric(row.names(PredictedY))
result <- merge(df, PredictedY, by="index")

CalculatedY <- as.data.frame(CalculatedY)
CalculatedY$index <-  as.numeric(row.names(CalculatedY))
res <- merge(result, CalculatedY, by="index")

write.csv(res, file = paste("Testing/","res.csv", sep =""), row.names = FALSE)

####### Merge results back to reference data frame ################

result_false <- filter(result, result$PredictedY == 'FALSE')
result_true <-  filter(result, result$PredictedY == 'TRUE')

######### Plotting #############

plot(df$var1, type = 'l', col = "blue")


out_index = which(PredictedY[,1]==FALSE) ## true or false

df[out_index]

plot(df$var1, col="blue", type="l")

points(x=out_index, y=df$var1[out_index], pch=18, col="red")


#################### Extract data with all the variables ###################


qry2 <- #Extract data

dataset <- as.data.table(sqlQuery(con, qry2, believeNRows = FALSE))

All_results <- filter(dataset, dataset$ID %in% result$ID)

results <- filter(dataset, dataset$ID %in% result_true$ID)


############### Save results ###########################

write.csv(results, file = paste("Testing/","result.csv", sep =""), row.names = FALSE)

