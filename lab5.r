colnames(glass) <- c('refractiveindex','Sodium','Magnesium', 'Aluminum', 'Silicon', 'Potassium','Calcium', 'Barium', 'Iron', 'Class')
library(neuralnet)
size.sample <- floor(1/2 * nrow(glass))
set.seed(107)

samples_id <- sample(1:nrow(glass), size.sample)
glasstrain <- glass[c(samples_id),]
glassvalidation <- glass[-c(samples_id),]

nnet_glasstrain <- glasstrain
nnet_glasstrain$building_windows_float_processed      <- glasstrain$Class == 'building_windows_float_processed'
nnet_glasstrain$building_windows_non_float_processed  <- glasstrain$Class == 'building_windows_non_float_processed'
nnet_glasstrain$containers                            <- glasstrain$Class == 'containers'
nnet_glasstrain$headlamps                             <- glasstrain$Class == 'headlamps'
nnet_glasstrain$tableware                             <- glasstrain$Class == 'tableware'
nnet_glasstrain$vehicle_windows_float_processed       <- glasstrain$Class == 'vehicle_windows_float_processed'

  nn <- neuralnet(building_windows_float_processed + building_windows_non_float_processed+ containers + headlamps + tableware + vehicle_windows_float_processed ~ 
                    refractiveindex+Sodium+Magnesium+Aluminum+Silicon+Potassium+Calcium+Barium+Iron,
                  data = nnet_glasstrain,
                  hidden =c(15), stepmax=1e+06)
  plot(nn)
  
mypredic <- compute(nn,glassvalidation)$net.result
maxidx <- function(arr){
  return(which(arr == max(arr)))
}



idx <- apply(mypredic, c(1), maxidx)
prediction <- c('building_windows_float_processed','building_windows_non_float_processed','containers','headlamps','tableware','vehicle_windows_float_processed')[idx]
table(prediction, glassvalidation$Class)

A<-as.matrix(table(prediction, glassvalidation$Class))
sum(diag(A))/sum(A) 


