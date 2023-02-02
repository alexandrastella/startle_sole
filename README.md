# startle_sole
Code for Startle Experiments on English sole 

#PCA: Are the startle response variables correlated?

```{r}

#load packages
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)

#read file
getwd()
startle_sole<- read.csv("/Users/alexandrastella/Documents/Thesis/data/Startle_ES_Redo.csv", header= TRUE)
startle_sole

#plot the data 
c

#create dataframes 
pre= filter(.data = startle_sole, experiment=="pre")
post= filter(.data = startle_sole, experiment=="post")

#run PCA
#use scales if data not the same type of data or not in the same order of magnitude
#scale: PCA divides each variable by indv. variance 

help("prcomp")

#PCA for pre
PCA_pre= prcomp(~dist_total + vel_mean + vel_max, data= pre, scale=TRUE)
PCA_pre
summary(PCA_pre)


biplot(PCA_pre)
plot(PCA_pre$x[,c("PC1", "PC2")])


PCApre_box= ggplot(data = pre, aes(x= oxygen, y= PCA_pre)) + geom_boxplot(notch=FALSE)+xlab("Treatment Level")+ ylab("PCA Score")+ggtitle("PCA Results for Pre-Startle Behavior")
PCApre_box

#PCA for post
PCA_post= prcomp(~dist_total + vel_mean + vel_max + latency, data= post, scale=TRUE)
PCA_post
summary(PCA_post)


biplot(PCA_post)
plot(PCA_post$x[,c("PC1", "PC2")])

#PC1:new axis of data --> find direction where there is most variance 
#PC2: new line at 90 deg. angle to PC1
#variables are correlated so reduce down to one variable (PC1)
#PC1 could be used instead of original variables 
#PC1 contains more information than any of the one variables but less interpretable
#PC1: direction of most variance 
#PC1: all variables positive, all correlated 
#higher input, higher PC1 score because coefficients are positive 
```


```{r}
#evaluate PCA_pre
 library(factoextra)

get_eig(PCA_pre) #gives you table with eigenvalues and percent explained
fviz_eig(PCA_pre) #bar graph to visualize pc's

#plot loading matrix of PCA
mat_PCA=get_pca_var(PCA_pre)  #create a variable with information about the components
mat_PCA$coord #look at the loadings to understand what each components is telling us
fviz_pca_var(PCA_pre, col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#evaluate PCA_post
get_eig(PCA_post) #gives you table with eigenvalues and percent explained
fviz_eig(PCA_post) #bar graph to visualize pc's

#plot loading matrix of PCA
mat_PCA1=get_pca_var(PCA_post)  #create a variable with information about the components
mat_PCA1$coord #look at the loadings to understand what each components is telling us
fviz_pca_var(PCA_post, col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
```


#GLM: What is the relationship between response latency, total distance moved, average speed and maximum speed?
```{r}

#read file
startle_sole <- read.csv("/Users/alexandrastella/Documents/Thesis/data/Startle_ES_Redo.csv", header= TRUE)
startle_sole

#GLM for pre
pre1= startle_sole[startle_sole$experiment=="pre",]
pre1

pred_pre= lm(oxygen ~dist_total + vel_mean + vel_max, data= pre1)
summary(pred_pre)

#test variance with vif function
vif(pred_pre)

#GLM for post
post1= startle_sole [startle_sole$experiment=="post",]
post1

pred_post= lm(oxygen ~dist_total + vel_mean + vel_max + latency, data= post1)
summary(pred_post)

#test variance with vif function
vif(pred_post)

#vif creates new model where one of the input variable becomes the output, compares this variable to the other two input variables 
#vif= 1/ 1-R^2
#vif=2 --> moderately correlated but may not be sufficient cause to get rid of variables 
#if vif high, take out variable then rerun linear regression
#vif value suggests how well the variable can be distinguished from the other variables
```