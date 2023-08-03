library(dtw)

# a_vector <- c(1,2,4)
# b_vector <- c(2,3,4,5)

a_vector <- c(21, 25, 28)
b_vector <- c(23,24,36)


simple1 <- dtw(a_vector, b_vector, keep = TRUE, step = symmetric2)
lcm <- simple1$localCostMatrix; lcm
cm <- simple1$costMatrix;cm


###################### lcm ######################
image(x=1:nrow(lcm),y=1:ncol(lcm),lcm)
text(row(lcm),col(lcm),label=lcm)
lines(simple1$index1,simple1$index2)
simple1$localCostMatrix
write.csv(lcm, 'raw_data/Slides_figure/lcm_simple1.csv', row.names=TRUE)
simple1$stepPattern
simple1$directionMatrix
###################### cm ######################
image(x=1:nrow(cm),y=1:ncol(cm),cm)
text(row(cm),col(cm),label=cm)
lines(simple1$index1,simple1$index2)

write.csv(cm, 'raw_data/Slides_figure/cm_simple1.csv', row.names=TRUE)
#####################################################
model$normalizedDistance
model$localCostMatrix
model$stepPattern
model$directionMatrix

write.csv(localCostMatrix, 'raw_data/Slides_figure/lcm_simple1.csv', row.names=TRUE)

data("aami3a")

ref <- window(aami3a,start=0,end=2)
test <- window(aami3a,start=2.7,end=5)
alignment <- dtw(test,ref)
alignment$distance
alignment$index1
