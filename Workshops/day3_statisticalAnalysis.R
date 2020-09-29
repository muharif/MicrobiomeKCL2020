library(ggplot2)

#DataFrame import
df1 <- read.table("Data/DATA_SET_REFERENCE_3.csv",
                        sep=",",header=TRUE,row.names = 1)
colnames(df1)


plot(df1$Sugar_Consumption,df1$Exercise)
plot(df1$LDL_levels,df1$Exercise)

#Check Correlation
plot(df1$LDL_levels,df1$Sugar_Consumption)
cor.test(df1$LDL_levels,df1$Exercise)

cor1=cor.test(df1$LDL_levels,df1$Exercise)

cor.test(df1$LDL_levels,df1$Exercise,method="spearman")

#Check Data Normality
hist((df1[,"LDL_levels"]))
ks.test(scale(df1[,"LDL_levels"]),"pnorm")


hist((df1[,"Exercise"]))
exercise_norm=ks.test(scale(df1[,"Exercise"]),"pnorm")
exercise_norm$p.value


#Linear Regression
LMoutput<-lm(df1[,"Exercise"] ~  df1[,"LDL_levels"])
summary(LMoutput)
plot(LMoutput)

LMoutput2<-lm(df1[,"Sugar_Consumption"] ~  df1[,"LDL_levels"])
summary(LMoutput2)
plot(LMoutput2)

#############
#Exercises 1#
#############
df2 <- read.table("Data/DATA_SET_REFERENCE_4_A.csv",sep=",",header=TRUE,row.names = 1)

#Finding correlated genes with Gene1
temp=as.numeric(df2['Gene2',])
for(i in rownames(df2)){
  temp1=as.numeric(df2[i,])
  temp2=cor.test(temp,temp1)
  if(temp2$p.value < 0.01){
    print(i)
  }
}

#Find the gene with highest median (Remember the material yesterday?)

#Find correlated genes with Gene2, Gene203, and Gene119

#Plot 1 gene-pair that are significantly correlated (up to you) from the analysis above and see how correlated are they based on the plot

#Plot 1 gene-pair that are NOT significantly correlated (up to you) from the analysis above and see how correlated are they based on the plot

#Is there any genes that has non-normal distribution?

#Plot histogram of Gene2, Gene203, and Gene119, and comment on their normality

#Pick 3 pairs of genes (Up to you), and do Linear regression for them. Check if their significance.

#############
#Exercises 2#
#############

#Use df3 for this exercise
df3 <- read.table("Data/DATA_SET_REFERENCE_2.csv",
                  sep=",",header=TRUE,row.names = 1)

#Find the highest correlated variable with "LDL" level

#Check the normality of each numeric column, anything not following normal distribution?

#Do Linear Regression for LDL vs the rest of the variables. Note their significance levels