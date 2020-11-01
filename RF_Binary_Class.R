#This Random Forest algorithm has been adapted from Advance Image Analysis Course material
# The code was used for entire classification of the AOI and assess the performance of RF in binary classification


#remove everything
rm(list=ls(all=TRUE))


#load libraries
library(raster)
install.packages("e1071")
library(randomForest)
library(sp)
library(rgdal)
library(ggplot2)
#library (VSURF)
#library(varSelRF)

#set working directory to data folder where your data is stored
setwd("C:/AIA/8. Project/Data")
infile <- stack('Image.tif')  


names(infile) <- c('B8', 'Texture_meanB3', 'Texture_meanB4', 'Texture_meanB2', 'Texture_HomogeneityB4',
                   'Texture_VarianceB4', 'Texture_HomogeneityB3', 'Texture_VarianceB3', 
                   'Texture_HomogeneityB2', 'Texture_VarianceB2', 'Texture_CorrelationB2', 
                   'Texture_CorrelationB3', 'Texture_CorrelationB4', 'Texture_DissimilarityB2',
                   'Texture_DissimilarityB3', 'Texture_DissimilarityB4', 'Texture_ContrastB4',
                   'Texture_EntropyB4', 'Texture_SecondMomentB4', 'Texture_ContrastB3',
                   'Texture_EntropyB3', 'Texture_SecondMomentB3', 'Texture_ContrastB2',
                   'Texture_EntropyB2', 'Texture_SecondMomentB2', 'Texture_SkewnessB4',
                   'Texture_SkewnessB3','Texture_SkewnessB2', 'Texture_DataRangeB4', 'Texture_DataRangeB3' ,
                   'Texture_DataRangeB2', 'B4', 'B3', 'B2', 'B12', 'B11', 'MNDWI', 'NDSSI')


#Common Poylgons to assess with other methods for validation -small part
ValidationPts<-read.csv("Validation.csv", header=TRUE, sep = ",")

# Training Points almost per pixel per mine area (small area in the AOI) and Randomly Points for polygons
#f NonMine
TrainingPts<-read.csv("Training.csv", header=TRUE, sep = ",")
ValidationPts
TrainingPts

# set number of samples for training and testing based on the size of the polygons for validation
NumToExcludeForValidation<-3066
#defines the number of training points to be extracted from all the training points randomnly
NumberForTraining<-4857
#number of times to run the classification (Given the size of the AOI and 38 bands used - only one iteration
#was performed given the computional cost for only one iteration)
NumIterations<-1
#selection of BANDS in image you want to use
selection<-c(1:38) 

#first subset training data to remove the points for validation points
coordinates(ValidationPts)=~x+y 
proj4string(ValidationPts)<-CRS("+proj=utm +zone=21S +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 


cumulative_errorTable = NULL    

for (i in 1:NumIterations){
  cat("Running classification iteration ", i)
  coordinates(TrainingPts)=~x+y
  proj4string(TrainingPts)<-CRS("+proj=utm +zone=21s +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  

  # Extract the training data 
  inraster = stack(infile)
  training_data = extract(inraster, TrainingPts)
  training_response = as.factor(TrainingPts$ClassID) 
  training_data
  training_response
  #csv file has a field called "ClassID" for names of each of the classes 
  
  training_predictors = training_data[,selection] 
  TrainingPts
  training_predictors
  
  
  ntree = 360    #number of trees based on Error v.s #trees - First CHART
  mtry = 10     # number of variables used   for splitting  based on oobErr v.s mtry -CHART
  r_forest_mtry = randomForest(training_predictors, y=training_response, mtry = mtry, ntree = ntree, keep.forest=TRUE, importance = TRUE, proximity=TRUE) 
  #r_forest = randomForest(training_predictors, y=training_response, ntree = ntree, keep.forest=TRUE, importance = TRUE, proximity=TRUE) 
  
  #===========================================================================================
  #Investigate the OOB (Out-Of-the bag) error
  #===========================================================================================
  r_forest <-r_forest_mtry  
  
  #===========================================================================================
  # Assessment of variable importance
  #===========================================================================================
  imp<-importance(r_forest)  #for ALL classes individually
  imp   #display importance output in console
  varImpPlot(r_forest)
  
  
  # assess the sensitivity of the model
  #=========================================================================================
  #Evaluate the impact of the ntree on the accuracy
  #========================================================================================
  tree_nr_evaluation <-data.frame(
    Trees=rep(1:nrow(r_forest$err.rate), times=3), 
    Type=rep(c("OOB", "0", "1"),
             each=nrow(r_forest$err.rate)),
    Error = c(r_forest$err.rate[, "OOB"],
              r_forest$err.rate[, "0"],
              r_forest$err.rate[, "1"]))
  #                        r_forest$err.rate[, "3"],
  #                        r_forest$err.rate[, "4"],
  #                        r_forest$err.rate[, "5"],
  #                        r_forest$err.rate[, "6"]))
  
  ggplot(data=tree_nr_evaluation, aes(x=Trees, y=Error)) + geom_line(aes(color=Type))
  
  
  # assess the sensitivity of the model for the number of the variables were splitted
  #=======================================================================================
  #Evaluate the impact of the mtry on the accuracy
  #========================================================================================
  mtry_evaluation <-vector (length=38)
  for(i in 1:38){
    test_model <- randomForest(training_predictors, y=training_response, mtry=i, ntree = ntree, keep.forest=TRUE, importance = TRUE, proximity=TRUE) 
    mtry_evaluation[i] <-test_model$err.rate[nrow(test_model$err.rate),1]
  }
  mtry_evaluation
  #==========================================================================================
  
  #set up the file name for the raster to print
  outraster = paste("rf_iteration", i, "NumberForTraining", NumberForTraining, ".pix", sep = "_")
  
  # Classify the whole image 
  predictor_data = subset(inraster, selection)	
  setwd("C:/AIA/8. Project/Data/Outputs") 
  predictions = predict(predictor_data, r_forest, filename=outraster, format="PCIDSK", overwrite=TRUE, progress="text", type="response") #write out the predicted classification to a raster file
  
  #do independent validation
  outraster=stack(outraster)	 
  #reads in the classification you just produced
  validation=extract(outraster, ValidationPts)  
  #extracts the value of the classified raster at the validation point locations
  
  ValidationJoin<-cbind(ValidationPts$ClassID, validation) 
  #joins the predicted with observed
  
  totalcorrect<-(subset(ValidationJoin,ValidationJoin[,1]==ValidationJoin[,2])) 
  #determines which samples were the same for observed and predicted
  
  indep_errorrate<-100 - (length(totalcorrect[,1])/length(validation)*100)
  #determines the independent error rate (%)
  
  rfOOB<-r_forest$err.rate[ntree,1]*100 #obtains the rfOOB error rate 
  differror<-rfOOB - indep_errorrate
  current_error<-cbind(names(outraster), rfOOB, indep_errorrate, differror)
  #binds all rfOOB, indep_errorrate, and differror together
  
  cumulative_errorTable<-rbind(current_error, cumulative_errorTable)	
  # populates a variable of cumulatively collected validation data within the loop
  
} #end of loop

#confussion matrix & Overall Accuracy
cm = table(prediction=validation,thruth=ValidationPts$ClassID)
oa =sum(diag(cm))/(length(ValidationPts$ClassID))
cm
oa

#comparison with other methods based on the cm

Precision = (1257)/(1257+393) 
Recall = (1257)/(1257+79)
F.score = (2*Precision*Recall)/(Precision + Recall)
F.score


tableName = paste("rf_iteration", i,"NumberForTraining", NumberForTraining, ".csv", sep = "_")
write.csv(cumulative_errorTable, file = tableName)	
#prints out a table with error from all classifications
warning()


