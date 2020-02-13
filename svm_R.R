svm <- read.csv(file.choose())
svm1 <- svm[1:100,1:10]
View(svm)
write.csv(svm,file="svm.csv",col.names = F,row.names = F)
# For SVM all the features must be in numeric 
# All the feature values should be in same range 
# If not we should normalize 
# SVM model will perform Rescalling automatically 

# data is randomly arranged 
svm_train<-svm1[1:70,]
svm_test<-svm1[71:100,]
# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 

# kvsm() function uses gaussian RBF kernel 
# Building model 
library(kernlab)
library(caret)
model1<-ksvm(svm1 ~.,data = svm_train,kernel = "vanilladot")
model1

help(kvsm)
??kvsm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(svm1 ~.,data = svm_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=svm_test)
mean(pred_rfdot==svm_test) #0.66

# kernel = vanilladot
model_vanilla<-ksvm(svm1 ~.,data = svm_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=svm_test)
mean(pred_vanilla==svm_test) #0.56


# kernal = besseldot
model_besseldot<-ksvm(svm1 ~.,data = svm_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=svm_test)
mean(pred_bessel==svm_test)#0.44

# kernel = polydot

model_poly<-ksvm(svm1 ~.,data = svm_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = svm_test)
mean(pred_poly==svm_test) #0.40


##rfdot is the best model compare to other models
