library(ggplot2)

#DataFrame import
df1 <- read.table("DATA_SET_REFERENCE_3.csv",
                        sep=",",header=TRUE,row.names = 1)
colnames(df1)# <- c('A','B','C', 'D','E')


plot(df1$Sugar_Consumption,df1$Exercise)
plot(df1$LDL_levels,df1$Sugar_Consumption)

plot(df1$LDL_levels,df1$Exercise)

ggplot(df1, aes(x=LDL_levels, y=Exercise)) + geom_point()

ggplot(df1,aes(x=Planet,y=LDL_levels, group=Planet)) + geom_boxplot() 
ggplot(df1,aes(x=Planet,y=LDL_levels, group=Planet)) + geom_boxplot(alpha=.5) 
ggplot(df1,aes(x=Planet,y=LDL_levels, group=Planet))  + geom_boxplot(alpha=.75) + geom_jitter(alpha=.25)

#Check Correlation
plot(df1$LDL_levels,df1$Exercise)
cor.test(df1$LDL_levels,df1$Exercise)

cor1 = cor.test(df1$LDL_levels,df1$Exercise)

cor.test(df1$LDL_levels,df1$Exercise)

plot(df1$LDL_levels,df1$Sugar_Consumption)
cor.test(df1$LDL_levels,df1$Sugar_Consumption)

plot(df1$Sugar_Consumption,df1$Exercise)
cor.test(df1$Exercise,df1$Sugar_Consumption,method="pearson")

cor.test(df1$Sugar_Consumption,df1$Exercise)

#Check Data Normality
hist((df1[,"LDL_levels"]))
ks.test(scale(df1[,"LDL_levels"]),"pnorm")

hist((df1[,"Exercise"]))
exercise_norm=ks.test(scale(df1[,"Exercise"]),"pnorm")
exercise_norm$p.value

hist((df1[,"Sugar_Consumption"]))
sugar_norm=ks.test(scale(df1[,"Sugar_Consumption"]),"pnorm")
sugar_norm$p.value

#Linear Regression

plot(df1$LDL_levels,df1$Exercise)
LMoutput<-lm(df1[,"Exercise"] ~  df1[,"LDL_levels"])
summary(LMoutput)

Exercise = -0.97578*LDL + 167.60554

plot(LMoutput)

plot(df1$LDL_levels,df1$Sugar_Consumption)

LMoutput2<-lm(df1[,"Sugar_Consumption"] ~  df1[,"LDL_levels"])
summary(LMoutput2)

Sugar = 0.22106*LDL

plot(LMoutput2)

#############
#Exercises 1#
#############
df2 <- read.table("DATA_SET_REFERENCE_4_A.csv",sep=",",header=TRUE,row.names = 1)

#Finding correlated genes with Gene2
temp=as.numeric(df2['Gene203',])

#p-value < 0.01

for(i in rownames(df2)){
  temp1 = as.numeric(df2[i,])
  cor1 = cor.test(temp,temp1)
  if(cor1$p.value < 0.01){
    print(i)
  }
}

for(i in rownames(df2)){
  temp1=as.numeric(df2[i,])
  temp2=cor.test(temp,temp1)
  if(temp2$p.value < 0.01){
    print(i)
  }
}

#Find the gene with highest mean (Remember the material yesterday?)

a = rowMeans(df2,na.rm = TRUE)
b = a[order(a,decreasing = FALSE)]
b
#Find correlated genes with Gene2, Gene203, and Gene119

temp=as.numeric(df2['Gene203',])

for(i in rownames(df2)){
  temp1 = as.numeric(df2[i,])
  cor1 = cor.test(temp,temp1)
  if(cor1$p.value < 0.01){
    print(i)
  }
}

temp=as.numeric(df2['Gene119',])

for(i in rownames(df2)){
  temp1 = as.numeric(df2[i,])
  cor1 = cor.test(temp,temp1)
  if(cor1$p.value < 0.01){
    print(i)
  }
}

plot(as.numeric(df2['Gene203',]), as.numeric(df2['Gene8',]))

#Plot 1 gene-pair that are significantly correlated (up to you) from the analysis above and see how correlated are they based on the plot

ggplot(as.data.frame(t(df2)), aes(x=Gene2, y=Gene203)) + geom_point()

#Plot 1 gene-pair that are NOT significantly correlated (up to you) from the analysis above and see how correlated are they based on the plot
ggplot(as.data.frame(t(df2)), aes(x=Gene2, y=Gene203)) + geom_point()

#Is there any genes that has non-normal distribution?
for(i in rownames(df2)){
  temp1 = as.numeric(df2[i,])
  cor1 = ks.test(scale(temp1),'pnorm')
  if(cor1$p.value > 0.01){
    print(i)
  }
}


#Plot histogram of Gene2, Gene203, and Gene119, and comment on their normality

hist((as.numeric(df2['Gene2',])))

exercise_norm=ks.test(scale(as.numeric(df2['Gene2',])),"pnorm")

exercise_norm$p.value


#Pick 3 pairs of genes (Up to you), and do Linear regression for them. Check if their significance.



LMoutput2 = lm(as.numeric(df2['Gene8',]) ~  as.numeric(df2['Gene203',]))
summary(LMoutput2)


#############
#Exercises 2#
#############

#Use df3 for this exercise
df3 <- read.table("Data/DATA_SET_REFERENCE_2.csv",
                  sep=",",header=TRUE,row.names = 1)
colnames(df3)
#Find the highest correlated variable with "LDL" level
cor.test(df3$LDL,df3$Minutes_Reading)

t <- as.numeric(df3[,"LDL"])
for(i in c('Hours_sun', 'Age', 'Weight', 'Sugar_blood', 'Sleep_hours', 'Hospital_times', 'Minutes_Reading')){
  t2 <- as.numeric(df3[,i])
  t3 <- cor.test(t, t2)
  print(paste(i,t3$p.value))
}

#Check the normality of each numeric column, anything not following normal distribution?

for(i in c('Hours_sun', 'Age', 'Weight', 'Sugar_blood', 'Sleep_hours', 'Hospital_times', 'Minutes_Reading','LDL')){
  t2 <- as.numeric(df3[,i])
  t3 <- ks.test(t2,'pnorm')
  print(paste(i,t3$p.value))
}

#Do Linear Regression for LDL vs the rest of the variables. Note their significance levels