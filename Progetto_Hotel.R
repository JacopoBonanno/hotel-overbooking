rm(list=ls())

library(GGally)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggcorrplot)
library(moments)
library(car)
library(pROC)
library(MASS)
library(factoextra)
library(ggplot2)
library(klaR)
library(stats)
library(caret)
library(rsample)
library(flextable)
library("e1071")
library(randomForest)
library(gridExtra)
library(cowplot)
library(grid)
library(ggplotify)
library(ggpubr)

df_oj = read.csv("Hotel Reservations.csv")
df_oj = df_oj[, -c(1, 6, 7, 8, 10, 12, 13, 14)]
df_oj$booking_status = as.factor(as.numeric(df_oj$booking_status == "Canceled"))

df = cbind(scale(df_oj[-c(6,11)]), df_oj[6],df_oj[11])

df_x=df_oj[-11]

df = as_tibble(df)
colSums(is.na(df))
df=drop_na(df)

dev.new()

ggpairs(df, aes(color=booking_status))

df_cor=df_x%>%as.matrix%>%cor()
ggcorrplot(df_cor)

mat_sk_ku = matrix(rep(0,20), ncol=2)
colnames(mat_sk_ku)=c("Skewness","Kurtosis")
row.names(mat_sk_ku)=colnames(df_x)
for (i in 1:10) {
  mat_sk_ku[i,1]=skewness(df_x[,i])
  mat_sk_ku[i,2]=kurtosis(df_x[,i])
}

plot<-list()
box_variables<-colnames(df_x)
for(i in box_variables){
  plot[[i]]<-ggplot(df_oj, aes_string(x="booking_status", y=i, 
                                     col="booking_status", fill="booking_status"))+
    geom_boxplot(alpha=0.2)+
    theme(legend.position = "none")+
    scale_color_manual(values=c("blue", "red"))+
    scale_fill_manual(values = c("blue", "red"))
  
}
do.call(grid.arrange, c(plot, nrow=2))

library(heplots)
boxm<-heplots::boxM(df_x, df$booking_status)
boxm


##################################################
# PCA
df_t = subset(df_x, arrival_month == 7)
df_t = df_t[,-10]

pc <-princomp(df_t, cor=TRUE, scores=TRUE)

fviz_eig(pc)
fviz_pca_ind(pc,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             label="none"
)
fviz_pca_var(pc,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE   
)

#################################################
# LDA

n_run = 5
pre = NULL
pre_mat = matrix(rep(0,57), ncol=3)
acc = NULL
acc_mat = matrix(rep(0,57), ncol=3)
sen = NULL
sen_mat = matrix(rep(0,57), ncol=3)
spe = NULL
spe_mat = matrix(rep(0,57), ncol=3)
a = 0
b = 0

for (mese in 6:8){
  b = b+1
  for (threshold in sequence(from = 5, 19, by = 5)){
    a = a+1
    for (i in 1:n_run){
      ds_t = subset(df, arrival_month == mese)
      ds_t = ds_t[,-10]
      
      split <- rsample::initial_split(ds_t, prop = 0.8, strata = booking_status)
      
      training <- rsample::training(split)
      test <- rsample::testing(split)
      
      #training_index = sample(nrow(ds_t), nrow(ds_t)*0.8)
      #training = ds_t[training_index,]       
      #test = ds_t[-training_index,]
      
      lda_mod = lda(booking_status ~ ., data = training)
      
      phat = predict(lda_mod, test)$posterior[,2]
      yhat = as.numeric(phat>(threshold/100))
      
      cf = confusionMatrix(data = as.factor(yhat), reference = test$booking_status, 
                           mode = "everything", positive = "1")
      
      #conf_mat<- table(Predicted=yhat, Actual=test$booking_status)
      
      #if (dim(conf_mat)[1]==2) {
       # pre[i] = conf_mat[[4]] / (conf_mat[[4]]+conf_mat[[2]])
        #acc[i] = sum(diag(conf_mat))/sum(conf_mat)
        #sen[i] = conf_mat[[4]] / (conf_mat[[4]]+conf_mat[[3]])
        #spe[i] = conf_mat[[1]] / (conf_mat[[1]]+conf_mat[[2]])
      #}
      #else {
       # pre[i] = NA
        #acc[i] = NA
        #sen[i] = NA
        #spe[i] = NA
      #}
      
      pre[i] = cf[["byClass"]][["Precision"]]
      acc[i] = cf[["overall"]][["Accuracy"]]
      sen[i] = cf[["byClass"]][["Sensitivity"]]
      spe[i] = cf[["byClass"]][["Specificity"]]
    }
    
    pre_mat[a,b] = mean(pre)
    pre = NULL
    acc_mat[a,b] = mean(acc)
    acc = NULL
    sen_mat[a,b] = mean(sen)
    sen = NULL
    spe_mat[a,b] = mean(spe)
    spe = NULL
  }
  a = 0
}

pre_mat = as.data.frame(pre_mat)
acc_mat = as.data.frame(acc_mat)
sen_mat = as.data.frame(sen_mat)
spe_mat = as.data.frame(spe_mat)
colnames(pre_mat) = c(6:8)
colnames(acc_mat) = c(6:8)
colnames(sen_mat) = c(6:8)
colnames(spe_mat) = c(6:8)
row.names(pre_mat) = sequence(from = 5, 19, by = 5)
row.names(acc_mat) = sequence(from = 5, 19, by = 5)
row.names(sen_mat) = sequence(from = 5, 19, by = 5)
row.names(spe_mat) = sequence(from = 5, 19, by = 5)
View(pre_mat)
View(acc_mat)
View(sen_mat)
View(spe_mat)

mat_mese = NULL
mat_mese[["Giugno"]]["Precision"] = pre_mat[1]
mat_mese[["Giugno"]]["Accuracy"] = acc_mat[1]
mat_mese[["Giugno"]]["Sensitivity"] = sen_mat[1]
mat_mese[["Giugno"]]["Specificity"] = spe_mat[1]
mat_mese[["Luglio"]]["Precision"] = pre_mat[2]
mat_mese[["Luglio"]]["Accuracy"] = acc_mat[2]
mat_mese[["Luglio"]]["Sensitivity"] = sen_mat[2]
mat_mese[["Luglio"]]["Specificity"] = spe_mat[2]
mat_mese[["Agosto"]]["Precision"] = pre_mat[3]
mat_mese[["Agosto"]]["Accuracy"] = acc_mat[3]
mat_mese[["Agosto"]]["Sensitivity"] = sen_mat[3]
mat_mese[["Agosto"]]["Specificity"] = spe_mat[3]

ft <- flextable(cbind(as.data.frame(mat_mese[["Giugno"]], col.names = c("a","b","c","j")),
                      as.data.frame(mat_mese[["Luglio"]], col.names = c("d","e","f","m")),
                      as.data.frame(mat_mese[["Agosto"]], col.names = c("g","h","i","n"))))
ft <- add_header_row(ft,
                     colwidths = c(4, 4, 4),
                     values = c("Giugno", "Luglio", "Agosto")
)
ft <- align_nottext_col(ft, align = "center")
ft

mat_mese_max = NULL
mat_mese_max[["Giugno"]]["Precision"] = list(c(max(pre_mat[1],na.rm = T),which(pre_mat[1]==max(drop_na(pre_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Accuracy"] = list(c(max(acc_mat[1],na.rm = T),which(acc_mat[1]==max(drop_na(acc_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Sensitivity"] = list(c(max(sen_mat[1],na.rm = T),which(sen_mat[1]==max(drop_na(sen_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Specificity"] = list(c(max(spe_mat[1],na.rm = T),which(spe_mat[1]==max(drop_na(spe_mat[1])))*0.05))
mat_mese_max[["Luglio"]]["Precision"] = list(c(max(pre_mat[2],na.rm = T),which(pre_mat[2]==max(drop_na(pre_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Accuracy"] = list(c(max(acc_mat[2],na.rm = T),which(acc_mat[2]==max(drop_na(acc_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Sensitivity"] = list(c(max(sen_mat[2],na.rm = T),which(sen_mat[2]==max(drop_na(sen_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Specificity"] = list(c(max(spe_mat[2],na.rm = T),which(spe_mat[2]==max(drop_na(spe_mat[2])))*0.05))
mat_mese_max[["Agosto"]]["Precision"] = list(c(max(pre_mat[3],na.rm = T),which(pre_mat[3]==max(drop_na(pre_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Accuracy"] = list(c(max(acc_mat[3],na.rm = T),which(acc_mat[3]==max(drop_na(acc_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Sensitivity"] = list(c(max(sen_mat[3],na.rm = T),which(sen_mat[3]==max(drop_na(sen_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Specificity"] = list(c(max(spe_mat[3],na.rm = T),which(spe_mat[3]==max(drop_na(spe_mat[3])))*0.05))


ft_max <- flextable(cbind(as.data.frame(mat_mese_max[["Giugno"]], col.names = c("a","b","c","j")),
                      as.data.frame(mat_mese_max[["Luglio"]], col.names = c("d","e","f","m")),
                      as.data.frame(mat_mese_max[["Agosto"]], col.names = c("g","h","i","n"))))
ft_max <- add_header_row(ft_max,
                     colwidths = c(4, 4, 4),
                     values = c("Giugno", "Luglio", "Agosto")
)
ft_max <- align_nottext_col(ft_max, align = "center")
ft_max
#################################################

ds_t = subset(df, arrival_month != 7)
ds_test = subset(df, arrival_month == 7)
ds_t = ds_t[,-10]
ds_test = ds_test[,-10]

split <- rsample::initial_split(ds_t, prop = 0.8, strata = booking_status)

training <- rsample::training(split)
test <- rsample::testing(split)

training_index = sample(nrow(ds_t), nrow(ds_t)*0.8)
training = ds_t[training_index,]       
test = ds_t[-training_index,]

lda_mod = lda(booking_status ~ ., data = ds_t)
plot(lda_mod)
partimat(booking_status ~  .,data=training,method="lda")

phat = predict(lda_mod, ds_test)$posterior[,2]
yhat = as.numeric(phat>(0.5/100))

yhat<-predict(lda_mod, ds_test)$class

cf = confusionMatrix(data = as.factor(yhat), reference = ds_test$booking_status, mode = "everything", positive = "1")

conf_mat<- table(Predicted=yhat, Actual=ds_test$booking_status)
conf_mat
sum(diag(conf_mat))/sum(conf_mat)

#################################################


df = subset(df, arrival_month == 8)
df = df[-10]

log.mod = glm(booking_status ~ . , family = binomial("logit"), data = df)
summary.glm(log.mod)

# Studio le collinearità
library(car)
vif(log.mod)

# Studio la significatività totale
Chi2_oss=log.mod$null.deviance-log.mod$deviance
gradi_lib=log.mod$df.null-log.mod$df.residual
alfa_oss=1-pchisq(Chi2_oss, df=gradi_lib)   
print(alfa_oss)

# Calcolo l'R quadro di McFadden
mf_r2=1-log.mod$deviance/log.mod$null.deviance
print(mf_r2)

library(aod)
library(lmtest)

df = df[-c(2,6,7)]

log.mod_rid = glm(booking_status ~ . , family = binomial("logit"), data = df)
summary.glm(log.mod_rid)

lrtest(log.mod,log.mod_rid)

#################################################
# LOGISTICA

n_run = 50
pre = NULL
pre_mat = matrix(rep(0,57), ncol=3)
acc = NULL
acc_mat = matrix(rep(0,57), ncol=3)
sen = NULL
sen_mat = matrix(rep(0,57), ncol=3)
spe = NULL
spe_mat = matrix(rep(0,57), ncol=3)
a = 0
b = 0

for (mese in 6:8){
  b = b+1
  for (threshold in sequence(from = 5, 19, by = 5)){
    a = a+1
    for (i in 1:n_run){
      ds_t = subset(df, arrival_month == mese)
      ds_t = ds_t[,-c(2,6,7,10)]
      
      split <- rsample::initial_split(ds_t, prop = 0.8, strata = booking_status)
      
      training <- rsample::training(split)
      test <- rsample::testing(split)

      log_mod = glm(booking_status ~ ., data = training, family = "binomial")
      
      phat = predict.glm(log_mod, test[-10], "response")
      yhat = as.numeric(phat>(threshold/100))
      
      cf = confusionMatrix(data = as.factor(yhat), reference = test$booking_status, 
                           mode = "everything", positive = "1")

      pre[i] = cf[["byClass"]][["Precision"]]
      acc[i] = cf[["overall"]][["Accuracy"]]
      sen[i] = cf[["byClass"]][["Sensitivity"]]
      spe[i] = cf[["byClass"]][["Specificity"]]
    }
    
    pre_mat[a,b] = mean(pre)
    pre = NULL
    acc_mat[a,b] = mean(acc)
    acc = NULL
    sen_mat[a,b] = mean(sen)
    sen = NULL
    spe_mat[a,b] = mean(spe)
    spe = NULL
  }
  a = 0
}

pre_mat = as.data.frame(pre_mat)
acc_mat = as.data.frame(acc_mat)
sen_mat = as.data.frame(sen_mat)
spe_mat = as.data.frame(spe_mat)
colnames(pre_mat) = c(6:8)
colnames(acc_mat) = c(6:8)
colnames(sen_mat) = c(6:8)
colnames(spe_mat) = c(6:8)
row.names(pre_mat) = sequence(from = 5, 19, by = 5)
row.names(acc_mat) = sequence(from = 5, 19, by = 5)
row.names(sen_mat) = sequence(from = 5, 19, by = 5)
row.names(spe_mat) = sequence(from = 5, 19, by = 5)
View(pre_mat)
View(acc_mat)
View(sen_mat)
View(spe_mat)

mat_mese = NULL
mat_mese[["Giugno"]]["Precision"] = pre_mat[1]
mat_mese[["Giugno"]]["Accuracy"] = acc_mat[1]
mat_mese[["Giugno"]]["Sensitivity"] = sen_mat[1]
mat_mese[["Giugno"]]["Specificity"] = spe_mat[1]
mat_mese[["Luglio"]]["Precision"] = pre_mat[2]
mat_mese[["Luglio"]]["Accuracy"] = acc_mat[2]
mat_mese[["Luglio"]]["Sensitivity"] = sen_mat[2]
mat_mese[["Luglio"]]["Specificity"] = spe_mat[2]
mat_mese[["Agosto"]]["Precision"] = pre_mat[3]
mat_mese[["Agosto"]]["Accuracy"] = acc_mat[3]
mat_mese[["Agosto"]]["Sensitivity"] = sen_mat[3]
mat_mese[["Agosto"]]["Specificity"] = spe_mat[3]

ft <- flextable(cbind(as.data.frame(mat_mese[["Giugno"]], col.names = c("a","b","c","j")),
                      as.data.frame(mat_mese[["Luglio"]], col.names = c("d","e","f","m")),
                      as.data.frame(mat_mese[["Agosto"]], col.names = c("g","h","i","n"))))
ft <- add_header_row(ft,
                     colwidths = c(4, 4, 4),
                     values = c("Giugno", "Luglio", "Agosto")
)
ft <- align_nottext_col(ft, align = "center")
ft

mat_mese_max = NULL
mat_mese_max[["Giugno"]]["Precision"] = list(c(max(pre_mat[1],na.rm = T),which(pre_mat[1]==max(drop_na(pre_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Accuracy"] = list(c(max(acc_mat[1],na.rm = T),which(acc_mat[1]==max(drop_na(acc_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Sensitivity"] = list(c(max(sen_mat[1],na.rm = T),which(sen_mat[1]==max(drop_na(sen_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Specificity"] = list(c(max(spe_mat[1],na.rm = T),which(spe_mat[1]==max(drop_na(spe_mat[1])))*0.05))
mat_mese_max[["Luglio"]]["Precision"] = list(c(max(pre_mat[2],na.rm = T),which(pre_mat[2]==max(drop_na(pre_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Accuracy"] = list(c(max(acc_mat[2],na.rm = T),which(acc_mat[2]==max(drop_na(acc_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Sensitivity"] = list(c(max(sen_mat[2],na.rm = T),which(sen_mat[2]==max(drop_na(sen_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Specificity"] = list(c(max(spe_mat[2],na.rm = T),which(spe_mat[2]==max(drop_na(spe_mat[2])))*0.05))
mat_mese_max[["Agosto"]]["Precision"] = list(c(max(pre_mat[3],na.rm = T),which(pre_mat[3]==max(drop_na(pre_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Accuracy"] = list(c(max(acc_mat[3],na.rm = T),which(acc_mat[3]==max(drop_na(acc_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Sensitivity"] = list(c(max(sen_mat[3],na.rm = T),which(sen_mat[3]==max(drop_na(sen_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Specificity"] = list(c(max(spe_mat[3],na.rm = T),which(spe_mat[3]==max(drop_na(spe_mat[3])))*0.05))


ft_max <- flextable(cbind(as.data.frame(mat_mese_max[["Giugno"]], col.names = c("a","b","c","j")),
                          as.data.frame(mat_mese_max[["Luglio"]], col.names = c("d","e","f","m")),
                          as.data.frame(mat_mese_max[["Agosto"]], col.names = c("g","h","i","n"))))
ft_max <- add_header_row(ft_max,
                         colwidths = c(4, 4, 4),
                         values = c("Giugno", "Luglio", "Agosto")
)
ft_max <- align_nottext_col(ft_max, align = "center")
ft_max

lista_beta = NULL
mat_beta = NULL
lista_aux = NULL

for (mese in 6:8) {
  for (i in 1:50) {
    ds_t = subset(df, arrival_month == mese)
    ds_t = ds_t[,-c(2,6,7,10)]
    
    split <- rsample::initial_split(ds_t, prop = 0.8, strata = booking_status)
    
    training <- rsample::training(split)
    test <- rsample::testing(split)
    
    log_mod = glm(booking_status ~ ., data = training, family = "binomial")
    summary.glm(log_mod)
    
    lista_beta = cbind(lista_beta, log_mod[["coefficients"]])
    lista_aux = cbind(lista_aux, log_mod[["coefficients"]])
  }
  mat_beta = cbind(mat_beta, rowMeans(lista_beta))
  lista_beta = NULL
}

#################################################
i = 2
ds_t = subset(df, arrival_month == 7)
ds_t = ds_t[,-c(2,6,10)]

split <- rsample::initial_split(ds_t, prop = 0.8, strata = booking_status)

training <- rsample::training(split)
test <- rsample::testing(split)

log_mod = glm(booking_status ~ ., data = ds_t, family = "binomial")

lista_beta = cbind(lista_beta, log_mod[["coefficients"]])
View(lista_beta)
mat_beta = cbind(mat_beta, rowMeans(lista_beta))
View(mat_beta)
#################################################
#KNN

n_run = 50
pre = NULL
pre_mat = matrix(rep(0,57), ncol=3)
acc = NULL
acc_mat = matrix(rep(0,57), ncol=3)
sen = NULL
sen_mat = matrix(rep(0,57), ncol=3)
spe = NULL
spe_mat = matrix(rep(0,57), ncol=3)
a = 0
b = 0

for (mese in 6:8){
  b = b+1
  for (threshold in sequence(from = 5, 19, by = 5)){
    a = a+1
    
    ds_t = subset(df, arrival_month == mese)
    ds_t = ds_t[,-10]
    
    split <- rsample::initial_split(ds_t, prop = 0.8, strata = booking_status)
    
    training <- rsample::training(split)
    test <- rsample::testing(split)
    
    train_index = createFolds(training$booking_status, k=10)
    
    KnnFit = train(booking_status ~ .,
                   method = "knn",
                   data=training,
                   #preProcess = "scale",
                   metric = "Accuracy",
                   tuneGrid = data.frame(k=1:20),
                   trControl = trainControl(method="cv",
                                            indexOut=train_index))
    
    phat=predict(KnnFit,test, "prob")[,2]
    yhat = as.numeric(phat>(threshold/100))
    cf = confusionMatrix(data = as.factor(yhat), reference = test$booking_status, 
                         mode = "everything", positive = "1")
    
    #pre[i] = cf[["byClass"]][["Precision"]]
    #acc[i] = cf[["overall"]][["Accuracy"]]
    #sen[i] = cf[["byClass"]][["Sensitivity"]]
    #spe[i] = cf[["byClass"]][["Specificity"]]
  
    pre_mat[a,b] = cf[["byClass"]][["Precision"]]
    #pre = NULL
    acc_mat[a,b] = cf[["overall"]][["Accuracy"]]
    #acc = NULL
    sen_mat[a,b] = cf[["byClass"]][["Sensitivity"]]
    #sen = NULL
    spe_mat[a,b] = cf[["byClass"]][["Specificity"]]
    #spe = NULL
  }
  a = 0
}

pre_mat = as.data.frame(pre_mat)
acc_mat = as.data.frame(acc_mat)
sen_mat = as.data.frame(sen_mat)
spe_mat = as.data.frame(spe_mat)
colnames(pre_mat) = c(6:8)
colnames(acc_mat) = c(6:8)
colnames(sen_mat) = c(6:8)
colnames(spe_mat) = c(6:8)
row.names(pre_mat) = sequence(from = 5, 19, by = 5)
row.names(acc_mat) = sequence(from = 5, 19, by = 5)
row.names(sen_mat) = sequence(from = 5, 19, by = 5)
row.names(spe_mat) = sequence(from = 5, 19, by = 5)
View(pre_mat)
View(acc_mat)
View(sen_mat)
View(spe_mat)

mat_mese = NULL
mat_mese[["Giugno"]]["Precision"] = pre_mat[1]
mat_mese[["Giugno"]]["Accuracy"] = acc_mat[1]
mat_mese[["Giugno"]]["Sensitivity"] = sen_mat[1]
mat_mese[["Giugno"]]["Specificity"] = spe_mat[1]
mat_mese[["Luglio"]]["Precision"] = pre_mat[2]
mat_mese[["Luglio"]]["Accuracy"] = acc_mat[2]
mat_mese[["Luglio"]]["Sensitivity"] = sen_mat[2]
mat_mese[["Luglio"]]["Specificity"] = spe_mat[2]
mat_mese[["Agosto"]]["Precision"] = pre_mat[3]
mat_mese[["Agosto"]]["Accuracy"] = acc_mat[3]
mat_mese[["Agosto"]]["Sensitivity"] = sen_mat[3]
mat_mese[["Agosto"]]["Specificity"] = spe_mat[3]

ft <- flextable(cbind(as.data.frame(mat_mese[["Giugno"]], col.names = c("a","b","c","j")),
                      as.data.frame(mat_mese[["Luglio"]], col.names = c("d","e","f","m")),
                      as.data.frame(mat_mese[["Agosto"]], col.names = c("g","h","i","n"))))
ft <- add_header_row(ft,
                     colwidths = c(4, 4, 4),
                     values = c("Giugno", "Luglio", "Agosto")
)
ft <- align_nottext_col(ft, align = "center")
ft

mat_mese_max = NULL
mat_mese_max[["Giugno"]]["Precision"] = list(c(max(pre_mat[1],na.rm = T),which(pre_mat[1]==max(drop_na(pre_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Accuracy"] = list(c(max(acc_mat[1],na.rm = T),which(acc_mat[1]==max(drop_na(acc_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Sensitivity"] = list(c(max(sen_mat[1],na.rm = T),which(sen_mat[1]==max(drop_na(sen_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Specificity"] = list(c(max(spe_mat[1],na.rm = T),which(spe_mat[1]==max(drop_na(spe_mat[1])))*0.05))
mat_mese_max[["Luglio"]]["Precision"] = list(c(max(pre_mat[2],na.rm = T),which(pre_mat[2]==max(drop_na(pre_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Accuracy"] = list(c(max(acc_mat[2],na.rm = T),which(acc_mat[2]==max(drop_na(acc_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Sensitivity"] = list(c(max(sen_mat[2],na.rm = T),which(sen_mat[2]==max(drop_na(sen_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Specificity"] = list(c(max(spe_mat[2],na.rm = T),which(spe_mat[2]==max(drop_na(spe_mat[2])))*0.05))
mat_mese_max[["Agosto"]]["Precision"] = list(c(max(pre_mat[3],na.rm = T),which(pre_mat[3]==max(drop_na(pre_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Accuracy"] = list(c(max(acc_mat[3],na.rm = T),which(acc_mat[3]==max(drop_na(acc_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Sensitivity"] = list(c(max(sen_mat[3],na.rm = T),which(sen_mat[3]==max(drop_na(sen_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Specificity"] = list(c(max(spe_mat[3],na.rm = T),which(spe_mat[3]==max(drop_na(spe_mat[3])))*0.05))


ft_max <- flextable(cbind(as.data.frame(mat_mese_max[["Giugno"]], col.names = c("a","b","c","j")),
                          as.data.frame(mat_mese_max[["Luglio"]], col.names = c("d","e","f","m")),
                          as.data.frame(mat_mese_max[["Agosto"]], col.names = c("g","h","i","n"))))
ft_max <- add_header_row(ft_max,
                         colwidths = c(4, 4, 4),
                         values = c("Giugno", "Luglio", "Agosto")
)
ft_max <- align_nottext_col(ft_max, align = "center")
ft_max

####################################################
#SVM

n_run = 50
pre = NULL
pre_mat = matrix(rep(0,57), ncol=3)
acc = NULL
acc_mat = matrix(rep(0,57), ncol=3)
sen = NULL
sen_mat = matrix(rep(0,57), ncol=3)
spe = NULL
spe_mat = matrix(rep(0,57), ncol=3)
a = 0
b = 0

for (mese in 6:8){
  b = b+1
  for (threshold in sequence(from = 5, 19, by = 5)){
    a = a+1
    
    ds_t = subset(df, arrival_month == mese)
    ds_t = ds_t[,-10]
    
    split <- rsample::initial_split(ds_t, prop = 0.8, strata = booking_status)
    
    training <- rsample::training(split)
    test <- rsample::testing(split)
    
    train_index = createFolds(training$booking_status, k=10)
    
    #svmFit = train(booking_status ~ .,
    #               method = "svmLinear",
    #               data=training,
    #               probability=T,
    #               trControl = trainControl(method="cv",
    #                                        indexOut=train_index))
    
    svmFit <- svm(booking_status ~ ., data = training, probability = TRUE)
    
    phat <- predict(svmFit,test,probability = TRUE,decision.values = FALSE)
    phat = attr(phat, "probabilities")[,2]
    #phat=predict(svmFit,test)
    yhat = as.numeric(phat>(threshold/100))
    cf = confusionMatrix(data = as.factor(yhat), reference = test$booking_status, 
                         mode = "everything", positive = "1")
    
    #pre[i] = cf[["byClass"]][["Precision"]]
    #acc[i] = cf[["overall"]][["Accuracy"]]
    #sen[i] = cf[["byClass"]][["Sensitivity"]]
    #spe[i] = cf[["byClass"]][["Specificity"]]
    
    pre_mat[a,b] = cf[["byClass"]][["Precision"]]
    #pre = NULL
    acc_mat[a,b] = cf[["overall"]][["Accuracy"]]
    #acc = NULL
    sen_mat[a,b] = cf[["byClass"]][["Sensitivity"]]
    #sen = NULL
    spe_mat[a,b] = cf[["byClass"]][["Specificity"]]
    #spe = NULL
  }
  a = 0
}

pre_mat = as.data.frame(pre_mat)
acc_mat = as.data.frame(acc_mat)
sen_mat = as.data.frame(sen_mat)
spe_mat = as.data.frame(spe_mat)
colnames(pre_mat) = c(6:8)
colnames(acc_mat) = c(6:8)
colnames(sen_mat) = c(6:8)
colnames(spe_mat) = c(6:8)
row.names(pre_mat) = sequence(from = 5, 19, by = 5)
row.names(acc_mat) = sequence(from = 5, 19, by = 5)
row.names(sen_mat) = sequence(from = 5, 19, by = 5)
row.names(spe_mat) = sequence(from = 5, 19, by = 5)
View(pre_mat)
View(acc_mat)
View(sen_mat)
View(spe_mat)

mat_mese = NULL
mat_mese[["Giugno"]]["Precision"] = pre_mat[1]
mat_mese[["Giugno"]]["Accuracy"] = acc_mat[1]
mat_mese[["Giugno"]]["Sensitivity"] = sen_mat[1]
mat_mese[["Giugno"]]["Specificity"] = spe_mat[1]
mat_mese[["Luglio"]]["Precision"] = pre_mat[2]
mat_mese[["Luglio"]]["Accuracy"] = acc_mat[2]
mat_mese[["Luglio"]]["Sensitivity"] = sen_mat[2]
mat_mese[["Luglio"]]["Specificity"] = spe_mat[2]
mat_mese[["Agosto"]]["Precision"] = pre_mat[3]
mat_mese[["Agosto"]]["Accuracy"] = acc_mat[3]
mat_mese[["Agosto"]]["Sensitivity"] = sen_mat[3]
mat_mese[["Agosto"]]["Specificity"] = spe_mat[3]

ft <- flextable(cbind(as.data.frame(mat_mese[["Giugno"]], col.names = c("a","b","c","j")),
                      as.data.frame(mat_mese[["Luglio"]], col.names = c("d","e","f","m")),
                      as.data.frame(mat_mese[["Agosto"]], col.names = c("g","h","i","n"))))
ft <- add_header_row(ft,
                     colwidths = c(4, 4, 4),
                     values = c("Giugno", "Luglio", "Agosto")
)
ft <- align_nottext_col(ft, align = "center")
ft

mat_mese_max = NULL
mat_mese_max[["Giugno"]]["Precision"] = list(c(max(pre_mat[1],na.rm = T),which(pre_mat[1]==max(drop_na(pre_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Accuracy"] = list(c(max(acc_mat[1],na.rm = T),which(acc_mat[1]==max(drop_na(acc_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Sensitivity"] = list(c(max(sen_mat[1],na.rm = T),which(sen_mat[1]==max(drop_na(sen_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Specificity"] = list(c(max(spe_mat[1],na.rm = T),which(spe_mat[1]==max(drop_na(spe_mat[1])))*0.05))
mat_mese_max[["Luglio"]]["Precision"] = list(c(max(pre_mat[2],na.rm = T),which(pre_mat[2]==max(drop_na(pre_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Accuracy"] = list(c(max(acc_mat[2],na.rm = T),which(acc_mat[2]==max(drop_na(acc_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Sensitivity"] = list(c(max(sen_mat[2],na.rm = T),which(sen_mat[2]==max(drop_na(sen_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Specificity"] = list(c(max(spe_mat[2],na.rm = T),which(spe_mat[2]==max(drop_na(spe_mat[2])))*0.05))
mat_mese_max[["Agosto"]]["Precision"] = list(c(max(pre_mat[3],na.rm = T),which(pre_mat[3]==max(drop_na(pre_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Accuracy"] = list(c(max(acc_mat[3],na.rm = T),which(acc_mat[3]==max(drop_na(acc_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Sensitivity"] = list(c(max(sen_mat[3],na.rm = T),which(sen_mat[3]==max(drop_na(sen_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Specificity"] = list(c(max(spe_mat[3],na.rm = T),which(spe_mat[3]==max(drop_na(spe_mat[3])))*0.05))


ft_max <- flextable(cbind(as.data.frame(mat_mese_max[["Giugno"]], col.names = c("a","b","c","j")),
                          as.data.frame(mat_mese_max[["Luglio"]], col.names = c("d","e","f","m")),
                          as.data.frame(mat_mese_max[["Agosto"]], col.names = c("g","h","i","n"))))
ft_max <- add_header_row(ft_max,
                         colwidths = c(4, 4, 4),
                         values = c("Giugno", "Luglio", "Agosto")
)
ft_max <- align_nottext_col(ft_max, align = "center")
ft_max

#######################################################
#RF

n_run = 50
pre = NULL
pre_mat = matrix(rep(0,57), ncol=3)
acc = NULL
acc_mat = matrix(rep(0,57), ncol=3)
sen = NULL
sen_mat = matrix(rep(0,57), ncol=3)
spe = NULL
spe_mat = matrix(rep(0,57), ncol=3)
a = 0
b = 0

for (mese in 6:8){
  b = b+1
  for (threshold in sequence(from = 5, 19, by = 5)){
    a = a+1
    
    ds_t = subset(df, arrival_month == mese)
    ds_t = ds_t[,-10]
    
    split <- rsample::initial_split(ds_t, prop = 0.8, strata = booking_status)
    
    training <- rsample::training(split)
    test <- rsample::testing(split)
    
    rfmod = randomForest(formula=booking_status ~ ., data=training, importance=T, ntree=1000)
    
    phat=predict(rfmod,test, "prob")[,2]
    yhat = as.numeric(phat>(threshold/100))
    cf = confusionMatrix(data = as.factor(yhat), reference = test$booking_status, 
                         mode = "everything", positive = "1")
    
    #pre[i] = cf[["byClass"]][["Precision"]]
    #acc[i] = cf[["overall"]][["Accuracy"]]
    #sen[i] = cf[["byClass"]][["Sensitivity"]]
    #spe[i] = cf[["byClass"]][["Specificity"]]
    
    pre_mat[a,b] = cf[["byClass"]][["Precision"]]
    #pre = NULL
    acc_mat[a,b] = cf[["overall"]][["Accuracy"]]
    #acc = NULL
    sen_mat[a,b] = cf[["byClass"]][["Sensitivity"]]
    #sen = NULL
    spe_mat[a,b] = cf[["byClass"]][["Specificity"]]
    #spe = NULL
  }
  a = 0
}

pre_mat = as.data.frame(pre_mat)
acc_mat = as.data.frame(acc_mat)
sen_mat = as.data.frame(sen_mat)
spe_mat = as.data.frame(spe_mat)
colnames(pre_mat) = c(6:8)
colnames(acc_mat) = c(6:8)
colnames(sen_mat) = c(6:8)
colnames(spe_mat) = c(6:8)
row.names(pre_mat) = sequence(from = 5, 19, by = 5)
row.names(acc_mat) = sequence(from = 5, 19, by = 5)
row.names(sen_mat) = sequence(from = 5, 19, by = 5)
row.names(spe_mat) = sequence(from = 5, 19, by = 5)
View(pre_mat)
View(acc_mat)
View(sen_mat)
View(spe_mat)

mat_mese = NULL
mat_mese[["Giugno"]]["Precision"] = pre_mat[1]
mat_mese[["Giugno"]]["Accuracy"] = acc_mat[1]
mat_mese[["Giugno"]]["Sensitivity"] = sen_mat[1]
mat_mese[["Giugno"]]["Specificity"] = spe_mat[1]
mat_mese[["Luglio"]]["Precision"] = pre_mat[2]
mat_mese[["Luglio"]]["Accuracy"] = acc_mat[2]
mat_mese[["Luglio"]]["Sensitivity"] = sen_mat[2]
mat_mese[["Luglio"]]["Specificity"] = spe_mat[2]
mat_mese[["Agosto"]]["Precision"] = pre_mat[3]
mat_mese[["Agosto"]]["Accuracy"] = acc_mat[3]
mat_mese[["Agosto"]]["Sensitivity"] = sen_mat[3]
mat_mese[["Agosto"]]["Specificity"] = spe_mat[3]

ft <- flextable(cbind(as.data.frame(mat_mese[["Giugno"]], col.names = c("a","b","c","j")),
                      as.data.frame(mat_mese[["Luglio"]], col.names = c("d","e","f","m")),
                      as.data.frame(mat_mese[["Agosto"]], col.names = c("g","h","i","n"))))
ft <- add_header_row(ft,
                     colwidths = c(4, 4, 4),
                     values = c("Giugno", "Luglio", "Agosto")
)
ft <- align_nottext_col(ft, align = "center")
ft

mat_mese_max = NULL
mat_mese_max[["Giugno"]]["Precision"] = list(c(max(pre_mat[1],na.rm = T),which(pre_mat[1]==max(drop_na(pre_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Accuracy"] = list(c(max(acc_mat[1],na.rm = T),which(acc_mat[1]==max(drop_na(acc_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Sensitivity"] = list(c(max(sen_mat[1],na.rm = T),which(sen_mat[1]==max(drop_na(sen_mat[1])))*0.05))
mat_mese_max[["Giugno"]]["Specificity"] = list(c(max(spe_mat[1],na.rm = T),which(spe_mat[1]==max(drop_na(spe_mat[1])))*0.05))
mat_mese_max[["Luglio"]]["Precision"] = list(c(max(pre_mat[2],na.rm = T),which(pre_mat[2]==max(drop_na(pre_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Accuracy"] = list(c(max(acc_mat[2],na.rm = T),which(acc_mat[2]==max(drop_na(acc_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Sensitivity"] = list(c(max(sen_mat[2],na.rm = T),which(sen_mat[2]==max(drop_na(sen_mat[2])))*0.05))
mat_mese_max[["Luglio"]]["Specificity"] = list(c(max(spe_mat[2],na.rm = T),which(spe_mat[2]==max(drop_na(spe_mat[2])))*0.05))
mat_mese_max[["Agosto"]]["Precision"] = list(c(max(pre_mat[3],na.rm = T),which(pre_mat[3]==max(drop_na(pre_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Accuracy"] = list(c(max(acc_mat[3],na.rm = T),which(acc_mat[3]==max(drop_na(acc_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Sensitivity"] = list(c(max(sen_mat[3],na.rm = T),which(sen_mat[3]==max(drop_na(sen_mat[3])))*0.05))
mat_mese_max[["Agosto"]]["Specificity"] = list(c(max(spe_mat[3],na.rm = T),which(spe_mat[3]==max(drop_na(spe_mat[3])))*0.05))


ft_max <- flextable(cbind(as.data.frame(mat_mese_max[["Giugno"]], col.names = c("a","b","c","j")),
                          as.data.frame(mat_mese_max[["Luglio"]], col.names = c("d","e","f","m")),
                          as.data.frame(mat_mese_max[["Agosto"]], col.names = c("g","h","i","n"))))
ft_max <- add_header_row(ft_max,
                         colwidths = c(4, 4, 4),
                         values = c("Giugno", "Luglio", "Agosto")
)
ft_max <- align_nottext_col(ft_max, align = "center")
ft_max

#################################################################################


#PCA con tutte le variabili per mese
pca_ds<-princomp(subset(df_x, arrival_month == 1)[,-6], cor=TRUE, scores=TRUE)
v1 = fviz_eig(pca_ds)
c1 = fviz_pca_ind(pca_ds,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             label = "none"
)
b1 = fviz_pca_biplot(pca_ds, repel = TRUE,
                 col.var = "#2E9FDF", 
                 col.ind = "#696969",
                 label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 2)[,-6], cor=TRUE, scores=TRUE)
v2 = fviz_eig(pca_ds)
c2 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b2 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 3)[,-6], cor=TRUE, scores=TRUE)
v3 = fviz_eig(pca_ds)
c3 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b3 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 4)[,-6], cor=TRUE, scores=TRUE)
v4 = fviz_eig(pca_ds)
c4 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b4 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 5)[,-6], cor=TRUE, scores=TRUE)
v5 = fviz_eig(pca_ds)
c5 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b5 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 6)[,-6], cor=TRUE, scores=TRUE)
v6 = fviz_eig(pca_ds)
c6 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b6 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 7)[,-6], cor=TRUE, scores=TRUE)
v7 = fviz_eig(pca_ds)
c7 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b7 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 8)[,-6], cor=TRUE, scores=TRUE)
v8 = fviz_eig(pca_ds)
c8 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b8 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 9)[,-6], cor=TRUE, scores=TRUE)
v9 = fviz_eig(pca_ds)
c9 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b9 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 10)[,-6], cor=TRUE, scores=TRUE)
v10 = fviz_eig(pca_ds)
c10 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b10 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 11)[,-6], cor=TRUE, scores=TRUE)
v11 = fviz_eig(pca_ds)
c11 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b11 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 12)[,-6], cor=TRUE, scores=TRUE)
v12 = fviz_eig(pca_ds)
c12 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b12 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

dev.new()
ggarrange(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12,  nrow = 3, ncol = 4)
dev.new()
ggarrange(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12,  nrow = 3, ncol = 4)
dev.new()
ggarrange(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12,  nrow = 3, ncol = 4)


#PCA senza le variabili "no_of_previous_bookings_not_canceled" "no_of_previous_cancellations" --> Situazione ottimale
pca_ds<-princomp(subset(df_x, arrival_month == 6)[,-c(6,7,8)], cor=TRUE, scores=TRUE)
v6 = fviz_eig(pca_ds)
c6 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b6 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 7)[,-c(6,7,8)], cor=TRUE, scores=TRUE)
v7 = fviz_eig(pca_ds)
c7 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b7 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 8)[,-c(6,7,8)], cor=TRUE, scores=TRUE)
v8 = fviz_eig(pca_ds)
c8 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b8 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)


dev.new()
ggarrange(v6, v7, v8,  nrow = 3, ncol = 1)
dev.new()
ggarrange(c6, c7, c8,  nrow = 3, ncol = 1)
dev.new()
ggarrange(b6, b7, b8,  nrow = 3, ncol = 1)




#PCA in cui oltre alle predenti variabili abbiamo anche eliminato "no_week_nights"
pca_ds<-princomp(subset(df_x, arrival_month == 6)[,-c(4,6,7,8)], cor=TRUE, scores=TRUE)
v6 = fviz_eig(pca_ds)
c6 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b6 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 7)[,-c(4,6,7,8)], cor=TRUE, scores=TRUE)
v7 = fviz_eig(pca_ds)
c7 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b7 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)

pca_ds<-princomp(subset(df_x, arrival_month == 8)[,-c(4,6,7,8)], cor=TRUE, scores=TRUE)
v8 = fviz_eig(pca_ds)
c8 = fviz_pca_ind(pca_ds,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  label = "none"
)
b8 = fviz_pca_biplot(pca_ds, repel = TRUE,
                     col.var = "#2E9FDF", 
                     col.ind = "#696969",
                     label = 'var'
)


dev.new()
ggarrange(v6, v7, v8,  nrow = 3, ncol = 1)
dev.new()
ggarrange(c6, c7, c8,  nrow = 3, ncol = 1)
dev.new()
ggarrange(b6, b7, b8,  nrow = 3, ncol = 1)


##################################################################################################
#Kmeans
df_x=df[-11]

dev.new()
fviz_nbclust(subset(df_x, arrival_month == 1)[,-c(6, 7, 10)], kmeans, method = "silhouette") #9
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 2)[,-c(6, 7, 10)], kmeans, method = "silhouette") #10
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 3)[,-c(6, 7, 10)], kmeans, method = "silhouette") #2
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 4)[,-c(6, 7, 10)], kmeans, method = "silhouette") #2
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 5)[,-c(6, 7, 10)], kmeans, method = "silhouette") #7
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 6)[,-c(6, 7, 10)], kmeans, method = "silhouette") #9
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 7)[,-c(6, 7, 10)], kmeans, method = "silhouette") #2
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 8)[,-c(6, 7, 10)], kmeans, method = "silhouette") #10
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 9)[,-c(6, 7, 10)], kmeans, method = "silhouette") #6
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 10)[,-c(6, 7, 10)], kmeans, method = "silhouette") #6
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 11)[,-c(6, 7, 10)], kmeans, method = "silhouette") #9
dev.new()
fviz_nbclust(subset(df_x, arrival_month == 12)[,-c(6, 7, 10)], kmeans, method = "silhouette") #9



km1 = fviz_cluster(kmeans(subset(df_x, arrival_month == 1)[,-c(6, 7, 10)], centers = 9),
                   data = subset(df_x, arrival_month == 1)[,-c(6, 7, 10)])
km2 = fviz_cluster(kmeans(subset(df_x, arrival_month == 2)[,-c(6, 7, 10)], centers = 10), 
                   data = subset(df_x, arrival_month == 2)[,-c(6, 7, 10)])
km3 = fviz_cluster(kmeans(subset(df_x, arrival_month == 3)[,-c(6, 7, 10)], centers = 2), 
                   data = subset(df_x, arrival_month == 3)[,-c(6, 7, 10)])
km4 = fviz_cluster(kmeans(subset(df_x, arrival_month == 4)[,-c(6, 7, 10)], centers = 2), 
                   data = subset(df_x, arrival_month == 4)[,-c(6, 7, 10)])
km5 = fviz_cluster(kmeans(subset(df_x, arrival_month == 5)[,-c(6, 7, 10)], centers = 7), 
                   data = subset(df_x, arrival_month == 5)[,-c(6, 7, 10)])
km6 = fviz_cluster(kmeans(subset(df_x, arrival_month == 6)[,-c(6, 7, 10)], centers = 9), 
                   data = subset(df_x, arrival_month == 6)[,-c(6, 7, 10)])
km7 = fviz_cluster(kmeans(subset(df_x, arrival_month == 7)[,-c(6, 7, 10)], centers = 2), 
                   data = subset(df_x, arrival_month == 7)[,-c(6, 7, 10)])
km8 = fviz_cluster(kmeans(subset(df_x, arrival_month == 8)[,-c(6, 7, 10)], centers = 10), 
                   data = subset(df_x, arrival_month == 8)[,-c(6, 7, 10)])
km9 = fviz_cluster(kmeans(subset(df_x, arrival_month == 9)[,-c(6, 7, 10)], centers = 6), 
                   data = subset(df_x, arrival_month == 9)[,-c(6, 7, 10)])
km10 = fviz_cluster(kmeans(subset(df_x, arrival_month == 10)[,-c(6, 7, 10)], centers = 6), 
                    data = subset(df_x, arrival_month == 10)[,-c(6, 7, 10)])
km11 = fviz_cluster(kmeans(subset(df_x, arrival_month == 11)[,-c(6, 7, 10)], centers = 9), 
                    data = subset(df_x, arrival_month == 11)[,-c(6, 7, 10)])
km12 = fviz_cluster(kmeans(subset(df_x, arrival_month == 12)[,-c(6, 7, 10)], centers = 9), 
                    data = subset(df_x, arrival_month == 12)[,-c(6, 7, 10)])

dev.new()
ggarrange(km1, km2, km3, km4, km5, km6, km7, km8, km9, km10, km11, km12, nrow = 3, ncol = 4)
dev.new()
ggarrange(km6, km7, km8, nrow = 3, ncol = 1)

km6 = kmeans(subset(df_x, arrival_month == 6)[,-c(6, 7, 10)], centers = 9)
km7 = kmeans(subset(df_x, arrival_month == 7)[,-c(6, 7, 10)], centers = 2)
km8 = kmeans(subset(df_x, arrival_month == 8)[,-c(6, 7, 10)], centers = 10)

km6 =subset(df_x, arrival_month == 6)[,-c(6, 7, 10)] %>%
      mutate(Cluster = km6$cluster) %>%
      group_by(Cluster) %>%
      summarise_all("mean")

km7 =subset(df_x, arrival_month == 7)[,-c(6, 7, 10)] %>%
    mutate(Cluster = km7$cluster) %>%
    group_by(Cluster) %>%
    summarise_all("mean")

km8 =subset(df_x, arrival_month == 8)[,-c(6, 7, 10)] %>%
    mutate(Cluster = km8$cluster) %>%
    group_by(Cluster) %>%
    summarise_all("mean")

View(km6)
View(km7)
View(km8)
