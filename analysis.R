#+eval=FALSE

#Load libraries for Analysis
library(ggplot2)
library(dplyr)
library(corrplot)
library(moments)
library(reshape2)
library(psych)
library(scales)
library(gridExtra)
library(tidyverse)
library(readr)

#Import data
student<-read_delim(file="D:\\Users\\Maxine\\OneDrive - Asia Pacific University\\Documents\\Computer Science (Data Analysis)\\Programming For Data Analysis\\Assignment\\student.csv",
                    delim=";",
                    quote="")

tempcol=c()
student_perf=data.frame(index=1:nrow(student))

for(col in 1:length(student)){
  tempcol=c()
  for (row in student[,col]){
    temp=gsub("\"","",row)
    tempcol=c(tempcol,temp)
  }
  student_perf=cbind(student_perf,tempcol)
}

colname=c("index",names(student))
names(student_perf)=colname

student_perf
write.csv(student_perf,"D:\\Users\\Maxine\\OneDrive - Asia Pacific University\\Documents\\Computer Science (Data Analysis)\\Programming For Data Analysis\\Assignment\\student_perf.csv")
table(sapply(student_perf, class))

#Convert to correct data type
factor(student_perf[,17:24])
levels(factor(student_perf[,17:24]))
student_perf[,17:24]<-ifelse(student_perf[,17:24]=="yes",1,0)
student_perf[,17:24]<-sapply(student_perf[,17:24],as.logical)
student_perf[,c(4,8:9,14:16,25:34)]<-sapply(student_perf[,c(4,8:9,14:16,25:34)],as.integer)

table(sapply(student_perf, class))


#Question 1: Does the final score of each student remain stable throughout every period?
#Analysis 1-1: Find the distribution of G1, G2, and G3.

#Get Summary Statistics of score throughout three years
sum_score= summarise_at(student_perf, c("G1","G2","G3"),list(mean=mean,median=median,max=max,min=min,IQR=IQR,sd=sd))

#Add Range
sum_score=sum_score %>%  mutate(G1_range=G1_max-G1_min,G2_range=G2_max-G2_min,G3_range=G3_max-G3_min)

sum_score=matrix(c(sum_score),nrow=7,ncol=3,byrow=TRUE,dimnames=list(c("mean","median","max","min","IQR","sd","range"),c("G1","G2","G3")))
sum_score

#Create a new data frame of score and period to produce clustered histogram
student_score=data.frame(score=c(student_perf$G1,student_perf$G2,student_perf$G3),period=c(rep("G1",nrow(student_perf)),rep("G2",nrow(student_perf)),rep("G3",nrow(student_perf))))

#Histogram Plot
#Dodged
ggplot(student_score, aes(x=score,fill=period)) + geom_histogram() + facet_wrap(~period)

#Stacked
# ggplot(student_score) + geom_histogram(aes(x=score, fill=period ))+scale_x_continuous(breaks=0:20)

#Line Graph
ggplot(student_score, aes(x=score, color=period)) + geom_freqpoly(binwidth=1)+ scale_x_continuous(breaks=0:20)

#Analysis 1-2: Find the relationship between G1, G2, and G3.
#Following source code is modified from TPArrow (2015)
pairs(~ G1+G2+G3, data=student_perf, panel=function(x,y){
  points(x,y)
  abline(lm(y~x),col='red')
  text(15,5,labels = paste('r=',round(cor(x,y),2)) ,col='red' )
})

student_perf=mutate(student_perf,avg=rowMeans(select(student_perf[,32:34], starts_with("G")), na.rm = TRUE))

# Analysis 1-3: Exploration and Assumption of zero values in G2 and G3.
#Verify the conditional statement: If G2=0, then G3=0
filter(student_perf,student_perf$G2==0,student_perf$G3!=0)
dropout=c()
dropout=ifelse(student_perf$G3==0,TRUE,FALSE)
student_perf=mutate(student_perf,dropout)
filter(student_perf,student_perf$G3==0,student_perf$dropout==FALSE)



#Find out which year the students dropped out
assignDOYear<-function(G2,G3){
  if(G3>0){
    dropout_period='NA'
  }else{
    if(G2==0){
      dropout_period='Y2'
    }else{
      dropout_period='Y3'
    }
  }
  return (dropout_period)
}
dropout_period<-mapply(assignDOYear,student_perf$G2,student_perf$G3)
student_perf=mutate(student_perf,dropout_period)

#Following source code obtained from (Holtz, 2018)
student_perf$dropout_period <- factor(student_perf$dropout_period,levels=c("Y2", "Y3", "NA"))



#Question 2: What are the differences between drop out students and non-drop out students?

#Analysis 2-1:	Find drop out and non-drop out percentage of students.
#Dropout rate
pie(table(student_perf$dropout_period),labels = paste(c("NA  ", "Y2  ", "Y3  "),round(prop.table(table(student_perf$dropout_period))*100), "%", sep = ""), col=rainbow(3),main="Dropout Proportion") 


#Analysis 2-2: Find and compare the correlation matrix of both.

student_dropout=filter(student_perf,student_perf$dropout==TRUE)
student_ndo=filter(student_perf,student_perf$dropout==FALSE)

student_dropout_num=select_if(student_dropout, is.numeric)

par(mfrow=c(2,1))
corrplot(cor(student_dropout_num, method="s"), method = 'number')
corrplot(cor(student_ndo_num,method="s"), method = 'number')


#Analysis 2-3: Find and compare the summary statistics of both.

#Get Summary Statistics of all numeric variables of dropout students
sum_dropout= summarise_all(student_dropout_num, list(mean=mean,median=median,max=max,min=min,IQR=IQR,sd=sd))

#Get Summary Statistics of all numeric variables of non-dropout students
sum_ndo= summarise_all(student_ndo_num, list(mean=mean,median=median,max=max,min=min,IQR=IQR,sd=sd))

#Visualize summary statistics
sum_dropout=matrix(c(sum_dropout),nrow=6,ncol=18,byrow=TRUE,dimnames=list(c("mean","median","max","min","IQR","sd"),c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2","G3","avg")))

sum_ndo=matrix(c(sum_ndo),nrow=6,ncol=18,byrow=TRUE,dimnames=list(c("mean","median","max","min","IQR","sd"),c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2","G3","avg")))

sum_dropout

sum_ndo

#Special Feature 1: One-Way ANOVA.
#One-way Anova
#Following source code is modified from (Ura R-jp, 2012)

multi.tests <- function(fun = t.test, df, vars, group.var, ...) {
  sapply(simplify = FALSE, # sapply(simplify=T) better, elements named
         vars, # loop on vector of outcome variable names
         function(var) {
           formula <- as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
           fun(data = df, formula, ...)# perform test with a given fun, default t.test
         }
  )
}
``
res.multi.anova.do <-
  multi.tests(fun = oneway.test,
              df = student_perf,
              vars = c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2","G3"),
              group.var = "dropout",
              var.equal = TRUE)

arr.multi.anova.do=array(unlist(res.multi.anova.do),dim=c(6,18),dimnames=c(list(c("statistic (F)","num df","denom df","p.value","method","data.name"),c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2","G3","avg"))))
df.multi.anova.do=data.frame(arr.multi.anova.do)
df.multi.anova.do=as.data.frame(t(df.multi.anova.do))
df.multi.anova.do$p.value<-sapply(df.multi.anova.do$p.value,as.numeric)
df.multi.anova.do=arrange(df.multi.anova.do,df.multi.anova.do$p.value)
filter(df.multi.anova.do,p.value<=0.05)

#Preprocess Binary Data
isLogical<-sapply(student_perf,is.logical)
student_logical<-dplyr::select(student_perf, which(isLogical))

student_logical_temp=c()
student_logical_temp<-ifelse(student_perf[,2]=="GP",TRUE,FALSE)
student_logical<-mutate(student_logical,isGP=student_logical_temp)

student_logical_temp<-ifelse(student_perf[,3]=="F",TRUE,FALSE)
student_logical<-mutate(student_logical,isFemale=student_logical_temp)

student_logical_temp<-ifelse(student_perf[,5]=="U",TRUE,FALSE)
student_logical<-mutate(student_logical,isUrban=student_logical_temp)

student_logical_temp<-ifelse(student_perf[,6]=="GT3",TRUE,FALSE)
student_logical<-mutate(student_logical,isGT3=student_logical_temp)

student_logical_temp<-ifelse(student_perf[,7]=="T",TRUE,FALSE)
student_logical<-mutate(student_logical,isTogether=student_logical_temp)

#Special Feature 2: Phi-Test.
matrix_logical<-function(df,do,var){
  do_yes=subset(df , do == TRUE & var == TRUE)
  do_no=subset(df , do == TRUE & var == FALSE)
  ndo_yes=subset(df , do == FALSE & var == TRUE)
  ndo_no=subset(df , do == FALSE & var == FALSE)
  
  n_do_yes=nrow(do_yes)
  n_do_no=nrow(do_no)
  n_ndo_yes=nrow(ndo_yes)
  n_ndo_no=nrow(ndo_no)
  
  ntotal=nrow(student_perf)
  if(n_do_yes+n_do_no+n_ndo_yes+n_ndo_no==ntotal){
    mat<-matrix(c(n_do_yes,n_do_no,n_ndo_yes,n_ndo_no),nrow=2)
    colnames(mat)<-c("yes","no")
    rownames(mat)<-c("do","ndo")
    return(mat)
  }else{
    print(n_do_yes+n_do_no+n_ndo_yes+n_ndo_no)
    print("Data needs to be preprocessed")
  }
}

list_logical<-list()
c_phi<-array()
for(i in colnames(student_logical)){
  list_logical[[i]]<-matrix_logical(student_logical,student_logical$dropout,student_logical[[i]])
  c_phi[[i]]=phi(list_logical[[i]])
}

abs_phi=abs(c_phi)
abs_phi=sort(abs_phi,decreasing=TRUE)
rank_phi=dput(names(abs_phi))
c_phi=c_phi[rank_phi]
names(c_phi)=c("dropout","paid","higher","romantic","schoolsup","famsize","sex","Pstatus","activities","nursery","internet","address","famsup","school")
c_phi


#Special Feature 3: Binary-Binary Variables Plot Generator Function for Mosaic Plot and Bar Chart.

binary_binary_plot<-function(data,var2,var1,phi){
  #Following source code is obtained from (Lin, 2017)
  var1=as.name(var1)
  var2=as.name(var2)
  df1<-data %>%
    group_by(!!var1,!!var2) %>%
    summarise(count = n()) %>%
    mutate(var1.count = sum(count),
           prop = count/sum(count)) %>%
    ungroup()%>%
    select(v1=!!var1,prop,v2=!!var2,var1.count)
  
  mosaic_plot<-
    ggplot(data=df1,aes(x = v1, y = prop, width = var1.count, fill = v2)) +
    geom_bar(stat = "identity", position = "fill", colour = "black") +
    geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) + 
    facet_grid(~v1, scales = "free_x", space = "free_x") +
    labs( x = var1,fill=var2,title=paste("Phi coefficient between",var1,"and",var2,"is",phi))+
    scale_fill_brewer(palette = "RdYlGn")

  df2<-
    data %>%
    dplyr::select(v1=!!var1,v2=!!var2)
  
  bar_graph<-
    ggplot(x=v1,data=df2,aes(x=v1,fill=v2))+
    geom_bar()+
    labs( x = var1,fill=var2,title=paste("Phi coefficient between",var1,"\nand",var2,"is",phi))

  return(list(mosaic_plot,bar_graph))
}

#Analysis 2-4: Find the relationship between binary variables and dropout.
logical_plot2=list()
for(i in names(student_logical)){
  if(i!="dropout"){
    x=0
    if(i!="dropout_period"){
      x=c_phi[[i]]
    }
    logical_plot2[[i]]=binary_binary_plot(student_logical,i,"dropout",x)
  }
}

logical_mosaic2=list()
logical_bar2=list()
for(i in names(student_logical)){
  if(i!="dropout"){
    logical_mosaic2[[i]]=logical_plot2[[i]][[1]]
    logical_bar2[[i]]=logical_plot2[[i]][[2]]
  }
}

#Special feature: Save PNG of Graphs
png(file=paste("D:/plot/logical_mosaic_dropout_bb.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=logical_mosaic2,ncol=2,nrow=7,top="Dropout Binary Variables")
dev.off()
png(file=paste("D:/plot/logical_bar_dropout_bb.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=logical_bar2,ncol=2,nrow=7,top="Dropout Binary Variables")
dev.off()

#Analysis 2-5: Find the relationship between binary variables and dropout period.
#Special Feature x: Rank Variables
rank_logical=c(names(c_phi),"dropout_period")
student_logical=student_perf[,rank_logical]

logical_plot1=list()
for(i in names(student_logical)){
  if(i!="dropout_period"){
    logical_plot1[[i]]=binary_binary_plot(student_logical,i,"dropout_period",c_phi[[i]])
  }
}

logical_mosaic1=list()
logical_bar1=list()
for(i in names(student_logical)){
  if(i!="dropout_period"){
    logical_mosaic1[[i]]=logical_plot1[[i]][[1]]
    logical_bar1[[i]]=logical_plot1[[i]][[2]]
  }else{
    #Special feature x: Save PNG of Graphs to Local PC
    png(file=paste("D:/plot/logical_mosaic_dropout_period_bb.png",sep=""),width=1080,height=1440)
    grid.arrange(grobs=logical_mosaic1,ncol=2,nrow=7,top="Dropout Period Binary Variables")
    dev.off()
    png(file=paste("D:/plot/logical_bar_dropout_period_bb.png",sep=""),width=1080,height=1440)
    grid.arrange(grobs=logical_bar1,ncol=2,nrow=7,top="Dropout Period Binary Variables")
    dev.off()
  }
}



#Analysis 2-5: Find the relationship between categorical variables and dropout.
#Preprocess to factor
#Following source code is obtained from (Aizkalns, 2015)
student_factor<-student_perf %>%
  mutate_all(factor) %>% # just to change all columns to `factor` for testing
  select_if(~nlevels(.)  > 2)%>%
  select_if(~ nlevels(.) <=10)%>%
  mutate(dropout=as.factor(ifelse(dropout,"TRUE","FALSE")))

#Special Feature 3: Chi-square test
test=table(student_factor$age,student_factor$dropout)
test=dcast(student_factor,dropout~age)
list_factor<-list()
c_chi<-list()

#Convert to count observation
for(i in colnames(student_factor)){
  #Following source code is obtained from  (A5C1D2H2I1M1N2O1R2T1, 2013)
  if(i!="dropout"){
    list_factor[[i]]=table(student_factor$dropout,student_factor[[i]])
     c_chi[[i]]<-chisq.test(table(list_factor[[i]]))
  }}
#Following source code is obtained from (Cleland, 2018)
sorted_chi <- c_chi[order(sapply(c_chi,'[[','p.value'))]
head(sorted_chi,3)
print(sapply(sorted_chi,'[[','p.value')<0.05)

#Special Feature 4:  Factor-Factor Variables Plot Generator Function for Mosaic Plot and Histogram.
category_category_plot<-function(data,var2,var1,p){
  var1=as.name(var1)
  var2=as.name(var2)
  p=as.numeric(p)
  x=round(p,2)
  df1<-data %>%
    group_by(!!var1,!!var2) %>%
    summarise(count = n()) %>%
    mutate(var1.count = sum(count),
           prop = count/sum(count)) %>%
    ungroup()%>%
    select(v1=!!var1,prop,v2=!!var2,var1.count)
  
  if(is.numeric(df1$v2)){
    df1$v2=as.factor(df1$v2)
  }
  
  mosaic_plot<-
    ggplot(data=df1,aes(x = v1, y = prop, width = var1.count, fill = v2)) +
    geom_bar(stat = "identity", position = "fill", colour = "black") +
    geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) + # if labels are desired
    facet_grid(~v1, scales = "free_x", space = "free_x") +
    labs( x = var1,fill=var2,title=paste("P-val of chi-sqr between\n",var2,"and dropout is",x),cex.main=1)+
    scale_fill_brewer(palette = "RdYlGn")

  df2<-
    data %>%
    dplyr::select(v1=!!var1,v2=!!var2)
  
  if(is.numeric(df2$v2)){
    df2$v2=as.factor(df2$v2)
  }
  
  histogram<-ggplot(data=df2,aes(x=v1,colour=v2))+
    geom_histogram(stat='count',position='dodge')+
    labs( x = var1,colour=var2,title=paste("P-val of chi-sqr between\n",var2,"and dropout is",x),cex.main=1)

  return(list(mosaic_plot,histogram))
}



#Analysis 2-6: Find the relationship between categorical variables and dropout.
#Special Feature x: Rank Variables
rank_factor=c(names(sorted_chi),"dropout")
student_factor=student_perf[,rank_factor]
factor_plot=list()


for(i in names(student_factor)){
  if(i!="dropout"){
    factor_plot[[i]]=category_category_plot(student_factor,i,"dropout",sorted_chi[[i]][["p.value"]])
  }
}
factor_mosaic=list()
factor_histogram=list()
for(i in names(student_factor)){
  factor_mosaic[[i]]=factor_plot[[i]][[1]]
  factor_histogram[[i]]=factor_plot[[i]][[2]]
}

#Special feature x: Save PNG of Graphs

png(file=paste("D:/plot/factor_mosaic_dropout_cc.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=factor_mosaic,ncol=3,nrow=6,top="Dropout Categorical Variables")
dev.off()
png(file=paste("D:/plot/factor_histogram_dropout_cc.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=factor_histogram,ncol=3,nrow=6,top="Dropout Categorical Variables")
dev.off()


#Analysis 2-7: Find the relationship between categorical variables and dropout period.

for(i in names(student_factor)){
  if(i!="dropout_period"){
    factor_plot[[i]]=category_category_plot(student_factor,i,"dropout_period",sorted_chi[[i]][["p.value"]])
  }
}

factor_plot=list()
for(i in names(student_factor)){
  if(i!="dropout_period"){
    factor_plot[[i]]=category_category_plot(student_factor,i,"dropout_period",sorted_chi[[i]][["p.value"]])
  }
}


factor_mosaic=list()
factor_histogram=list()
for(i in names(student_factor)){
  factor_mosaic[[i]]=factor_plot[[i]][[1]]
  factor_histogram[[i]]=factor_plot[[i]][[2]]
}

#Special feature x: Save PNG of Graphs
png(file=paste("D:/plot/factor_mosaic_dropout_period_cc.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=factor_mosaic,ncol=3,nrow=6,top="Dropout Period Categorical Variables")
dev.off()
png(file=paste("D:/plot/factor_histogram_dropout_period_cc.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=factor_histogram,ncol=3,nrow=6,top="Dropout Period Categorical Variables")
dev.off()



# grid.arrange(grobs=factor_mosaic,ncol=floor(sqrt(length(factor_mosaic))))
# grid.arrange(grobs=factor_bar,ncol=floor(sqrt(length(factor_bar))))

#Question 3: What affects the score in first period, second period, final,
#and their average?

#Analysis 3-1: Find the difference of how variables affect score between student
#dropout and ndo.
par(mfrow=c(1,3))

student_perf%>%
  select_if(is.numeric)%>%
  cor(.)%>%
  corrplot(method = 'number')

student_dropout%>%
  select_if(is.numeric)%>%
  cor(.)%>%
  corrplot(method = 'number')

student_ndo%>%
  select_if(is.numeric)%>%
  cor(.)%>%
  corrplot(method = 'number')

#Not much, because sample size of drop out is small.

#Analysis 3-2: Find the difference of how variables affect score between
#student with different grades.
#A-15, B-10, C-5, D-0
grade<-function(score){
  if(score>=15){
    grade='A'
  }else if(score>=10){
    grade='B'
  }else if(score>=5){
    grade='C'
  }else if(score>=0){
    grade='D'
  }else{
    grade='O'
  }
  return (grade)
}

student_perf<-student_perf%>%
  mutate(gradeG1=sapply(G1, function(x) grade(x)))%>%
  mutate(gradeG2=sapply(G2, function(x) grade(x)))%>%
  mutate(gradeG3=sapply(G3, function(x) grade(x)))%>%
  mutate(gradeavg=sapply(avg, function(x) grade(x)))

student_ndo_factor<-student_perf %>%
  filter(dropout==FALSE) %>%
  mutate_all(factor) %>% # just to change all columns to `factor` for testing
  select_if(~nlevels(.)  >= 2)%>%
  select_if(~ nlevels(.) <=10)

student_ndo=filter(student_perf,student_perf$dropout==FALSE)


#Question 3: What affect the grades in non-dropout students?
#Analysis 3-3: Find the relationship between numeric variables and the grades


# #Analysis 4-: Find the relationship between numeric variables and the improvements.
# Special Feature: One-way Anova
multi_anova_var<-function(var){
  var=as.character(var)
  res.multi.anova.var <-
    multi.tests(fun = oneway.test,
                df = student_ndo,
                vars = c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences"),
                group.var = var,
                var.equal = TRUE)
  
  arr.multi.anova.var=array(unlist(res.multi.anova.var),dim=c(6,14),dimnames=c(list(c("statistic (F)","num df","denom df","p.value","method","data.name"),c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences"))))
  df.multi.anova.var=data.frame(arr.multi.anova.var)
  df.multi.anova.var=as.data.frame(t(df.multi.anova.var))
  df.multi.anova.var$p.value<-sapply(df.multi.anova.var$p.value,as.numeric)
  return(df.multi.anova.var)
}

multi_anova_ndo=list()
multi_anova_ndo_changes=matrix(nrow=3,ncol=14,dimnames=list(c("changes_p1","changes_p2","changes_all"),c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")),byrow=TRUE)

for(i in c("changes_p1","changes_p2","changes_all"))
{
  multi_anova_ndo[[i]]=as.data.frame(multi_anova_var(as.character(i)))
  p.value=as.data.frame(multi_anova_ndo[[i]][["p.value"]])
  colnames(p.value)[1]<-"p.value"
  rownames(p.value)<-c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")
  for(j in c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")){
    multi_anova_ndo_changes[as.character(i),j]=p.value[j,1]
  }
}

#Special feature x: Rank
multi_anova_ndo_changes<-t(multi_anova_ndo_changes)
multi_anova_ndo_changes<-data.frame(multi_anova_ndo_changes)


sorted_anova_score<-arrange(multi_anova_ndo_changes,changes_all)
student_ndo_num=student_perf[,c(row.names(sorted_anova_score),"dropout")]


#Analysis 3-4: Find the relationship between every variables
numeric_numeric_plot<-function(data,var2,var1,p){
  
  p=as.numeric(p)
  k=round(p,5)
  var1=as.name(var1)
  var2=as.name(var2)
  
  
  #https://stackoverflow.com/questions/57262084/how-can-i-easily-combine-the-output-of-grouped-summaries-with-an-overall-output
  sumstat=data.frame()
  
  sumstat<-data %>%
    filter(dropout==FALSE)%>%
    group_by(!!var1)%>%
    summarise_at(vars(!!var2),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))%>%
    bind_rows(.,
              data %>%
                group_by(dropout)%>%
                filter(dropout==TRUE)%>%
                summarise_at(vars(!!var2),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))
    )%>%
    bind_rows(.,
              data %>%
                summarise_at(vars(!!var2),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))
    )%>%
    ungroup()
  
  
  
  df<-data %>%
    select(v1=!!var1,v2=!!var2,dropout)%>%
    filter(dropout==FALSE)
  # https://stackoverflow.com/questions/47169639/saving-each-modified-facet-in-ggplot2
  box_plot<-ggplot(data=df,aes(x=v1,y=v2,color=v1))+
    geom_boxplot()+
    labs(x=var1,y=var2,color=var1,title=paste("P-val(Anova) between\n",var1," and",var2,"of ndo\nis",k),cex.main=1)
  # facet_wrap(~dropout)+
  # theme(strip.text.x = element_text(margin = margin(t = 10, b = 10)))
  
  df1<-data %>%
    group_by(!!var1,!!var2) %>%
    summarise(count = n()) %>%
    mutate(var1.count = sum(count),
           prop = count/sum(count)) %>%
    ungroup()%>%
    select(v1=!!var1,prop,v2=!!var2,var1.count)
  
  df1$v2=as.factor(df1$v2)
  bar_graph<-ggplot(x=v2,data=df1,aes(x=v2,y=prop,label=prop,fill=v1))+
    geom_bar(stat="identity",position="dodge")+
    # geom_text(aes(label=scales::percent(prop)),stat="identity",position=position_dodge(width=.9))+
    labs(x = var2,fill=var1,y="Probability",title=paste("P-val(Anova) between\n",var1," and",var2,"of ndo\nis",k),cex.main=1)
  
  
  histogram_plot<-ggplot(data=df,aes(x=v2))+
    geom_histogram()+
    labs(x = var2,paste("Histogram plot of",var2),cex.main=1)
  
  
  
  # var1=as.name("difference_G1_G2")
  # var2=as.name("age")
  var3=as.name(switch(as.character(var1),"changes_p1"="difference_G1_G2","changes_p2"="difference_G2_G3","changes_all"="difference_ALL"))
  var3=as.name(var3)
  df2<-student_perf %>%
    select(v1=!!var1,v2=!!var2,dropout,v3=!!var3)%>%
    filter(dropout==FALSE)
  
  
  #cor[v2,v3]
  scatter_plot<- ggplot(data=df2,aes(x=v2,y=v3,color=v1))+ geom_bin2d(bins=20) +
    scale_size_discrete(range = c(1, 10),guide=FALSE)+
    labs(x = var2,y=var3,color=var1,title=paste("Correlation of",var2,"and\n",var3,"by",var1,"is",round(cor(select_if(df2,is.numeric))[1,2],2),cex.main=1))
  
  return(list(sumstat,box_plot,bar_graph,histogram_plot,scatter_plot))
}
list_improvement=list()
for(i in c("changes_p1","changes_p2","changes_all")){
  for(j in c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")){
    x=0
    if(i!="dropout"){
      x=multi_anova_ndo_changes[[j,i]]
    }
    list_improvement[[j]][[i]]=numeric_numeric_plot(student_perf,j,i,x)
    #print(j)
    # print(list_improvement[[j]][[i]][1])
    # png(file=paste("D:/plot/","list_improvement_boxplot_",j,"_",i,".png",sep=""))
    # print(list_improvement[[j]][[i]][2])
    # dev.off()
    # png(file=paste("D:/plot/","list_improvement_bargraph_",j,"_",i,".png",sep=""))
    # print(list_improvement[[j]][[i]][3])
    # dev.off()
    # png(file=paste("D:/plot/","list_improvement_histogram_",j,".png",sep=""))
    # print(list_improvement[[j]][[i]][4])
    # dev.off()
    # png(file=paste("D:/plot/","list_improvement_scatterplot_",j,"_",i,".png",sep=""))
    # print(list_improvement[[j]][[i]][5])
    # dev.off()
    # png(file=paste("list_sum_grade","_",i,"_",j))
    # print(list_improvement[[i]][[j]][2])
    # dev.off()
    
    # factor_plot[[i]]=category_category_plot(student_factor,i,"dropout_year",sorted_chi[[i]][["p.value"]])
    # print(factor_plot[[i]][1])
    # print(factor_plot[[i]][2])
  }
  
}

# for(j in names(select_if(student_perf,is.numeric))){
#   for(i in c("changes_p1","changes_p2","changes_all")){
#     # list_improvement[[i]][[j]]=numeric_numeric_plot(student_perf,j,i)
#     print(i)
#     # print(list_improvement[[i]][[j]][1])
#     # https://stackoverflow.com/questions/7031935/r-using-previously-determined-variable-as-part-of-png-file-name
#     png(file=paste("D:/plot/","list_improvement_boxplot_",j,"_",i,".png",sep=""))
#     print(list_improvement[[j]][[i]][2])
#     dev.off()
# 
#     # factor_plot[[i]]=category_category_plot(student_factor,i,"dropout_year",sorted_chi[[i]][["p.value"]])
#     # print(factor_plot[[i]][1])
#     # print(factor_plot[[i]][2])
#   }
# 
#   #https://stackoverflow.com/questions/23570514/grid-arrange-using-list-of-plots
#   
#   print(j)
#   grade_boxplot=rbind(list_improvement[[j]][1:4])
#   png(file=paste("D:/plot/","list_improvement_boxplot_",j,".png",sep=""))
#   # do.call(grid.arrange, lapply(grade_boxplot, ggplotGrob))
#   #do.call(grid.arrange, grobs=grade_boxplot)
#   grid.arrange(grobs=grade_boxplot,ncol = 2,top=paste("P-val(Anova) between grade and average grade \n of non dropout is",df.multi.anova.score[[j,i]]))
#   dev.off()
#   
#}

for(j in c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")){
  list_boxplot=list()
  list_bargraph=list()
  list_scatterplot=list()
  
  for(i in c("changes_p1","changes_p2","changes_all")){
    list_boxplot[[i]]=list_improvement[[j]][[i]][[2]]
    list_bargraph[[i]]=list_improvement[[j]][[i]][[3]]
    list_scatterplot[[i]]=list_improvement[[j]][[i]][[5]]
  }
  # print(j)
  # png(file=paste("D:/plot/","list_improvement_boxplot_nn_",j,".png",sep=""))
  # grid.arrange(grobs=list_boxplot,ncol = 2,nrow=2,top=paste("Box plot of",j,"by score type"))
  # dev.off()
  # png(file=paste("D:/plot/","list_improvement_bargraph_nn_",j,".png",sep=""))
  # grid.arrange(grobs=list_bargraph,ncol = 2,nrow=2,top=paste("Bar graph of",j,"by score type"))
  # dev.off()
  # png(file=paste("D:/plot/","list_improvement_scatterplot_nn_",j,".png",sep=""))
  # grid.arrange(grobs=list_scatterplot,ncol = 2,nrow=2,top=paste("Scatterplot of",j,"by score type"))
  # dev.off()
}

list_boxplot1=list()
list_bargraph1=list()
list_scatterplot1=list()
list_boxplot2=list()
list_bargraph2=list()
list_scatterplot2=list()

for(j in c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")){
  index <- match(j, c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences"))
  if(index<=6){
    list_boxplot1[[j]]=list_improvement[[j]][[3]][[2]]
    list_bargraph1[[j]]=list_improvement[[j]][[3]][[3]]
    list_scatterplot1[[j]]=list_improvement[[j]][[3]][[5]]
  }else{
    list_boxplot2[[j]]=list_improvement[[j]][[3]][[2]]
    list_bargraph2[[j]]=list_improvement[[j]][[3]][[3]]
    list_scatterplot2[[j]]=list_improvement[[j]][[3]][[5]]
  }
  print(j)
}
png(file=paste("D:/plot/","list_improvement_boxplot1_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot1,ncol =2 ,nrow=3,top=paste("Box plot in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_bargraph1_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph1,ncol =2,nrow=3,top=paste("Bar graph in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_scatterplot1_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_scatterplot1,ncol =2,nrow=3,top=paste("Scatter plot in total changes"))
dev.off()

png(file=paste("D:/plot/","list_improvement_boxplot2_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot2,ncol =2 ,nrow=4,top=paste("Box plot in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_bargraph2_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph2,ncol =2,nrow=4,top=paste("Bar graph in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_scatterplot2_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_scatterplot2,ncol =2,nrow=4,top=paste("Scatter plot in total changes"))
dev.off()




#Analysis 3-2: Find the relationship between categorical variables and the grade
#Special Feature 1: One-way Anova

student_ndo_factor<-student_perf %>%
  filter(dropout==FALSE) %>%
  mutate_all(factor) %>% # just to change all columns to `factor` for testing
  select_if(~nlevels(.)  >= 2)%>%
  select_if(~ nlevels(.) <=10)%>%
  subset(select = -c(gradeG1,gradeG2,gradeG3,gradeavg))

multi_anova_var<-function(var){
  var=as.character(var)
  res.multi.anova.var <-
    multi.tests(fun = oneway.test,
                df = student_ndo,
                vars = c("G1","G2","G3","avg"),
                group.var = var,
                var.equal = TRUE)
  
  arr.multi.anova.var=array(unlist(res.multi.anova.var),dim=c(6,4),dimnames=c(list(c("statistic (F)","num df","denom df","p.value","method","data.name"),c("G1","G2","G3","avg"))))
  df.multi.anova.var=data.frame(arr.multi.anova.var)
  df.multi.anova.var=as.data.frame(t(df.multi.anova.var))
  df.multi.anova.var$p.value<-sapply(df.multi.anova.var$p.value,as.numeric)
  return(df.multi.anova.var)
}

multi_anova_ndo=list()
multi_anova_ndo_score=matrix(ncol=4,nrow=length(names(student_ndo_factor)),dimnames=list(names(student_ndo_factor),c("G1","G2","G3","avg")),byrow=TRUE)

for(i in names(student_ndo_factor))
{
  
  multi_anova_ndo[[i]]=as.data.frame(multi_anova_var(as.character(i)))
  p.value=as.data.frame(multi_anova_ndo[[i]][["p.value"]])
  colnames(p.value)[1]<-"p.value"
  rownames(p.value)<-c("G1","G2","G3","avg")
  multi_anova_ndo_score[as.character(i),"G1"]=p.value[1,1]
  multi_anova_ndo_score[as.character(i),"G2"]=p.value[2,1]
  multi_anova_ndo_score[as.character(i),"G3"]=p.value[3,1]
  multi_anova_ndo_score[as.character(i),"avg"]=p.value[4,1]
  
}

#Special feature x: Rank
multi_anova_ndo_score<-data.frame(multi_anova_ndo_score)
sorted_anova_score<-arrange(multi_anova_ndo_score,avg)
student_ndo_factor=student_perf[,c(row.names(sorted_anova_score),"dropout")]

#Special feature 6: Vategoric-Numeric Variables Plot Generator Function for Mosaic Plot and Histogram
category_numeric_plot<-function(data,var1,var2,p){
  
  p=as.numeric(p)
  k=round(p,5)
  var1=as.name(var1)
  var2=as.name(var2)
  
  
  sumstat=data.frame()
  print(var2)
  
  sumstat<-data %>%
    filter(dropout==FALSE)%>%
    group_by(!!var2)%>%
    summarise_at(vars(!!var1),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,sd=sd,skew=skewness))%>%
    bind_rows(.,
              data %>%
                group_by(dropout)%>%
                filter(dropout==TRUE)%>%
                summarise_at(vars(!!var1),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,sd=sd,skew=skewness))
    )%>%
    bind_rows(.,
              data %>%
                summarise_at(vars(!!var1),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,sd=sd,skew=skewness))
    )%>%
    ungroup()
  
  df<-data %>%
    select(v1=!!var1,v2=!!var2,dropout)%>%
    filter(dropout==FALSE)
  
  df$v2=as.factor(df$v2)
  
  box_plot<-ggplot(data=df,aes(x=v1,y=v2,color=v2))+
    geom_boxplot()+
    labs(x=var1,y=var2,color=var2,title=paste("P-val(Anova) between\n",var1," and",var2,"of ndo\nis",k),cex.main=1)
  
  
  bar_graph<-ggplot(x=v2,y=v1,data=df,aes(x=v2,y=v1,fill=v2))+
    geom_bar(stat="identity",position="dodge")+
    # geom_text(aes(label=scales::percent(prop)),stat="identity",position=position_dodge(width=.9))+
    labs(x = var2,fill=var1,y=var1,title=paste("P-val(Anova) between\n",var1," and",var2,"of ndo\nis",k),cex.main=1)
  
  return(list(sumstat,box_plot,bar_graph))
}

student_ndo_factor<-as.factor(student_ndo_factor)
list_grade=list()
for(i in names(student_ndo_factor)){
  for(j in c("G1","G2","G3","avg")){
    x=0
    if(i!="dropout"){
      x=multi_anova_ndo_score[[i,j]]
    }
    list_grade[[j]][[i]]=category_numeric_plot(student_perf,j,i,x)
    # print(j)
    
    # print(list_grade[[j]][[i]][1])
    # png(file=paste("D:/plot/","list_grade_boxplot_",i,"_",j,".png",sep=""))
    # print(list_grade[[j]][[i]][2])
    # dev.off()
    # png(file=paste("D:/plot/","list_grade_bargraph_",i,"_",j,".png",sep=""))
    # print(list_grade[[j]][[i]][3])
    # dev.off()
    # png(file=paste("list_sum_grade","_",i,"_",j))
    # print(list_grade[[i]][[j]][2])
    # dev.off()
    
    # factor_plot[[i]]=category_category_plot(student_factor,i,"dropout_year",sorted_chi[[i]][["p.value"]])
    # print(factor_plot[[i]][1])
    # print(factor_plot[[i]][2])
  }
  
}


#Special feature x: Save PNG of Graphs

for(j in names(student_ndo_factor)){
  list_boxplot=list()
  list_bargraph=list()
  list_scatterplot=list()
  for(i in c("G1","G2","G3","avg")){
    list_boxplot[[i]]=list_grade[[i]][[j]][[2]]
    list_bargraph[[i]]=list_grade[[i]][[j]][[3]]
  }
  # print(j)
  png(file=paste("D:/plot/","list_grade_boxplot_cn_",j,".png",sep=""))
  grid.arrange(grobs=list_boxplot,ncol = 2,nrow=2,top=paste("Box plot of",j,"and grade"))
  dev.off()
  png(file=paste("D:/plot/","list_grade_bargraph_cn_",j,".png",sep=""))
  grid.arrange(grobs=list_bargraph,ncol = 2,nrow=2,top=paste("Bar graph of",j,"and grade"))
  dev.off()
  
}

list_boxplot1=list()
list_boxplot2=list()
list_boxplot3=list()
list_bargraph1=list()
list_bargraph2=list()
list_bargraph3=list()

for(j in names(student_ndo_factor)){
  index <- match(j, names(student_ndo_factor))
  if(index<=12){
    list_boxplot1[[j]]=list_grade[[4]][[j]][[2]]
    list_bargraph1[[j]]=list_grade[[4]][[j]][[3]]
  }else if(index<=24){
    list_boxplot2[[j]]=list_grade[[4]][[j]][[2]]
    list_bargraph2[[j]]=list_grade[[4]][[j]][[3]]
  }else{
    list_boxplot3[[j]]=list_grade[[4]][[j]][[2]]
    list_bargraph3[[j]]=list_grade[[4]][[j]][[3]]
  }
  print(j)
}

png(file=paste("D:/plot/","list_grade_boxplot1_avg_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot1,ncol =3 ,nrow=4,top=paste("Box plot in average"))
dev.off()
png(file=paste("D:/plot/","list_grade_bargraph1_avg_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph1,ncol =3,nrow=4,top=paste("Bar graph in average"))
dev.off()

png(file=paste("D:/plot/","list_grade_boxplot2_avg_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot2,ncol =3 ,nrow=4,top=paste("Box plot in average"))
dev.off()
png(file=paste("D:/plot/","list_grade_bargraph2_avg_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph2,ncol =3,nrow=4,top=paste("Bar graph in average"))
dev.off()

png(file=paste("D:/plot/","list_grade_boxplot3_avg_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot3,ncol =3 ,nrow=4,top=paste("Box plot in average"))
dev.off()
png(file=paste("D:/plot/","list_grade_bargraph3_avg_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph3,ncol =3,nrow=4,top=paste("Bar graph in average"))
dev.off()


#Question 4: What affects the difference of scores?
#Analysis 4-1: Find the difference of how variables affect difference of scores between student dropout and non-dropouts.


student_perf<-student_perf%>%
  mutate(difference_G1_G2=G2-G1)%>%
  mutate(difference_G2_G3=G3-G2)%>%
  mutate(difference_ALL=difference_G1_G2+difference_G2_G3)


student_perf[,42:44] <- lapply(student_perf[,42:44], function (x) {as.numeric(as.character(x))})
student_ndo=filter(student_perf,student_perf$dropout==FALSE)

par(mfrow=c(1,2))

student_perf%>%
  select_if(is.numeric)%>%
  cor(.)%>%
  corrplot(method = 'number')

student_ndo%>%
  select_if(is.numeric)%>%
  cor(.)%>%
  corrplot(method = 'number')


if(student_perf$gradeG1!=student_perf$gradeG2){
  
}


student_ndo%>%
  arrange(desc(difference_ALL),desc(difference_G2_G3),desc(difference_G1_G2))

student_ndo%>%
  arrange(difference_ALL,difference_G2_G3,difference_G1_G2)


changes<-function(diff){
  if(diff>=3){
    improvement="A"
  }else if(diff>=1){
    improvement="B"
  }else if(diff==0){
    improvement="O"
  }else if(diff>=-1){
    improvement="Y"
  }else if(diff>=-3){
    improvement="Z"
  }else{
    improvement="?"
  }
  return (improvement)
}

student_perf<-student_perf%>%
  mutate(changes_p1=sapply(difference_G1_G2, function(x) changes(x)))%>%
  mutate(changes_p2=sapply(difference_G2_G3, function(x) changes(x)))%>%
  mutate(changes_all=sapply(difference_ALL, function(x) changes(x)))

student_ndo=filter(student_perf,student_perf$dropout==FALSE)


# #Analysis 4-3: Find the relationship between numeric variables and the improvements.
# Special Feature: One-way Anova
multi_anova_var<-function(var){
  var=as.character(var)
  res.multi.anova.var <-
    multi.tests(fun = oneway.test,
                df = student_ndo,
                vars = c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences"),
                group.var = var,
                var.equal = TRUE)
  
  arr.multi.anova.var=array(unlist(res.multi.anova.var),dim=c(6,14),dimnames=c(list(c("statistic (F)","num df","denom df","p.value","method","data.name"),c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences"))))
  df.multi.anova.var=data.frame(arr.multi.anova.var)
  df.multi.anova.var=as.data.frame(t(df.multi.anova.var))
  df.multi.anova.var$p.value<-sapply(df.multi.anova.var$p.value,as.numeric)
  return(df.multi.anova.var)
}

multi_anova_ndo=list()
multi_anova_ndo_changes=matrix(nrow=3,ncol=14,dimnames=list(c("changes_p1","changes_p2","changes_all"),c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")),byrow=TRUE)

for(i in c("changes_p1","changes_p2","changes_all"))
{
  multi_anova_ndo[[i]]=as.data.frame(multi_anova_var(as.character(i)))
  p.value=as.data.frame(multi_anova_ndo[[i]][["p.value"]])
  colnames(p.value)[1]<-"p.value"
  rownames(p.value)<-c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")
  for(j in c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")){
    multi_anova_ndo_changes[as.character(i),j]=p.value[j,1]
  }
}

#Special feature x: Rank
multi_anova_ndo_changes<-t(multi_anova_ndo_changes)
multi_anova_ndo_changes<-data.frame(multi_anova_ndo_changes)


sorted_anova_score<-arrange(multi_anova_ndo_changes,changes_all)
student_ndo_num=student_perf[,c(row.names(sorted_anova_score),"dropout")]


#Analysis 3-3: Find the relationship between every variables
numeric_numeric_plot<-function(data,var2,var1,p){
  
  p=as.numeric(p)
  k=round(p,5)
  var1=as.name(var1)
  var2=as.name(var2)
  
  
  #https://stackoverflow.com/questions/57262084/how-can-i-easily-combine-the-output-of-grouped-summaries-with-an-overall-output
  sumstat=data.frame()
  
  sumstat<-data %>%
    filter(dropout==FALSE)%>%
    group_by(!!var1)%>%
    summarise_at(vars(!!var2),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))%>%
    bind_rows(.,
              data %>%
                group_by(dropout)%>%
                filter(dropout==TRUE)%>%
                summarise_at(vars(!!var2),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))
    )%>%
    bind_rows(.,
              data %>%
                summarise_at(vars(!!var2),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))
    )%>%
    ungroup()
  
  
  
  df<-data %>%
    select(v1=!!var1,v2=!!var2,dropout)%>%
    filter(dropout==FALSE)
  # https://stackoverflow.com/questions/47169639/saving-each-modified-facet-in-ggplot2
  box_plot<-ggplot(data=df,aes(x=v1,y=v2,color=v1))+
    geom_boxplot()+
    labs(x=var1,y=var2,color=var1,title=paste("P-val(Anova) between\n",var1," and",var2,"of ndo\nis",k),cex.main=1)
  # facet_wrap(~dropout)+
  # theme(strip.text.x = element_text(margin = margin(t = 10, b = 10)))
  
  df1<-data %>%
    group_by(!!var1,!!var2) %>%
    summarise(count = n()) %>%
    mutate(var1.count = sum(count),
           prop = count/sum(count)) %>%
    ungroup()%>%
    select(v1=!!var1,prop,v2=!!var2,var1.count)
  
  df1$v2=as.factor(df1$v2)
  bar_graph<-ggplot(x=v2,data=df1,aes(x=v2,y=prop,label=prop,fill=v1))+
    geom_bar(stat="identity",position="dodge")+
    # geom_text(aes(label=scales::percent(prop)),stat="identity",position=position_dodge(width=.9))+
    labs(x = var2,fill=var1,y="Probability",title=paste("P-val(Anova) between\n",var1," and",var2,"of ndo\nis",k),cex.main=1)
  
  
  histogram_plot<-ggplot(data=df,aes(x=v2))+
    geom_histogram()+
    labs(x = var2,paste("Histogram plot of",var2),cex.main=1)
  
  
  
  # var1=as.name("difference_G1_G2")
  # var2=as.name("age")
  var3=as.name(switch(as.character(var1),"changes_p1"="difference_G1_G2","changes_p2"="difference_G2_G3","changes_all"="difference_ALL"))
  var3=as.name(var3)
  df2<-student_perf %>%
    select(v1=!!var1,v2=!!var2,dropout,v3=!!var3)%>%
    filter(dropout==FALSE)
  
  
  #cor[v2,v3]
  scatter_plot<- ggplot(data=df2,aes(x=v2,y=v3,color=v1))+ geom_bin2d(bins=20) +
    scale_size_discrete(range = c(1, 10),guide=FALSE)+
    labs(x = var2,y=var3,color=var1,title=paste("Correlation of",var2,"and\n",var3,"by",var1,"is",round(cor(select_if(df2,is.numeric))[1,2],2),cex.main=1))
  
  return(list(sumstat,box_plot,bar_graph,histogram_plot,scatter_plot))
}
list_improvement=list()
for(i in c("changes_p1","changes_p2","changes_all")){
  for(j in c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")){
    x=0
    if(i!="dropout"){
      x=multi_anova_ndo_changes[[j,i]]
    }
    list_improvement[[j]][[i]]=numeric_numeric_plot(student_perf,j,i,x)
    #print(j)
    # print(list_improvement[[j]][[i]][1])
    # png(file=paste("D:/plot/","list_improvement_boxplot_",j,"_",i,".png",sep=""))
    # print(list_improvement[[j]][[i]][2])
    # dev.off()
    # png(file=paste("D:/plot/","list_improvement_bargraph_",j,"_",i,".png",sep=""))
    # print(list_improvement[[j]][[i]][3])
    # dev.off()
    # png(file=paste("D:/plot/","list_improvement_histogram_",j,".png",sep=""))
    # print(list_improvement[[j]][[i]][4])
    # dev.off()
    # png(file=paste("D:/plot/","list_improvement_scatterplot_",j,"_",i,".png",sep=""))
    # print(list_improvement[[j]][[i]][5])
    # dev.off()
    # png(file=paste("list_sum_grade","_",i,"_",j))
    # print(list_improvement[[i]][[j]][2])
    # dev.off()
    
    # factor_plot[[i]]=category_category_plot(student_factor,i,"dropout_year",sorted_chi[[i]][["p.value"]])
    # print(factor_plot[[i]][1])
    # print(factor_plot[[i]][2])
  }
  
}

# for(j in names(select_if(student_perf,is.numeric))){
#   for(i in c("changes_p1","changes_p2","changes_all")){
#     # list_improvement[[i]][[j]]=numeric_numeric_plot(student_perf,j,i)
#     print(i)
#     # print(list_improvement[[i]][[j]][1])
#     # https://stackoverflow.com/questions/7031935/r-using-previously-determined-variable-as-part-of-png-file-name
#     png(file=paste("D:/plot/","list_improvement_boxplot_",j,"_",i,".png",sep=""))
#     print(list_improvement[[j]][[i]][2])
#     dev.off()
# 
#     # factor_plot[[i]]=category_category_plot(student_factor,i,"dropout_year",sorted_chi[[i]][["p.value"]])
#     # print(factor_plot[[i]][1])
#     # print(factor_plot[[i]][2])
#   }
# 
#   #https://stackoverflow.com/questions/23570514/grid-arrange-using-list-of-plots
#   
#   print(j)
#   grade_boxplot=rbind(list_improvement[[j]][1:4])
#   png(file=paste("D:/plot/","list_improvement_boxplot_",j,".png",sep=""))
#   # do.call(grid.arrange, lapply(grade_boxplot, ggplotGrob))
#   #do.call(grid.arrange, grobs=grade_boxplot)
#   grid.arrange(grobs=grade_boxplot,ncol = 2,top=paste("P-val(Anova) between grade and average grade \n of non dropout is",df.multi.anova.score[[j,i]]))
#   dev.off()
#   
#}

for(j in c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")){
  list_boxplot=list()
  list_bargraph=list()
  list_scatterplot=list()
  
  for(i in c("changes_p1","changes_p2","changes_all")){
    list_boxplot[[i]]=list_improvement[[j]][[i]][[2]]
    list_bargraph[[i]]=list_improvement[[j]][[i]][[3]]
    list_scatterplot[[i]]=list_improvement[[j]][[i]][[5]]
  }
  # print(j)
  # png(file=paste("D:/plot/","list_improvement_boxplot_nn_",j,".png",sep=""))
  # grid.arrange(grobs=list_boxplot,ncol = 2,nrow=2,top=paste("Box plot of",j,"by score type"))
  # dev.off()
  # png(file=paste("D:/plot/","list_improvement_bargraph_nn_",j,".png",sep=""))
  # grid.arrange(grobs=list_bargraph,ncol = 2,nrow=2,top=paste("Bar graph of",j,"by score type"))
  # dev.off()
  # png(file=paste("D:/plot/","list_improvement_scatterplot_nn_",j,".png",sep=""))
  # grid.arrange(grobs=list_scatterplot,ncol = 2,nrow=2,top=paste("Scatterplot of",j,"by score type"))
  # dev.off()
}

list_boxplot1=list()
list_bargraph1=list()
list_scatterplot1=list()
list_boxplot2=list()
list_bargraph2=list()
list_scatterplot2=list()

for(j in c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")){
  index <- match(j, c("index","age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences"))
  if(index<=6){
    list_boxplot1[[j]]=list_improvement[[j]][[3]][[2]]
    list_bargraph1[[j]]=list_improvement[[j]][[3]][[3]]
    list_scatterplot1[[j]]=list_improvement[[j]][[3]][[5]]
  }else{
    list_boxplot2[[j]]=list_improvement[[j]][[3]][[2]]
    list_bargraph2[[j]]=list_improvement[[j]][[3]][[3]]
    list_scatterplot2[[j]]=list_improvement[[j]][[3]][[5]]
  }
  print(j)
}
png(file=paste("D:/plot/","list_improvement_boxplot1_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot1,ncol =2 ,nrow=3,top=paste("Box plot in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_bargraph1_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph1,ncol =2,nrow=3,top=paste("Bar graph in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_scatterplot1_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_scatterplot1,ncol =2,nrow=3,top=paste("Scatter plot in total changes"))
dev.off()

png(file=paste("D:/plot/","list_improvement_boxplot2_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot2,ncol =2 ,nrow=4,top=paste("Box plot in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_bargraph2_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph2,ncol =2,nrow=4,top=paste("Bar graph in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_scatterplot2_tot_nn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_scatterplot2,ncol =2,nrow=4,top=paste("Scatter plot in total changes"))
dev.off()



#Analysis 4-3: Find the relationship between categorical variables and the difference of grades
#One-way Anova
student_ndo_factor<-student_perf %>%
  filter(dropout==FALSE) %>%
  mutate_all(factor) %>% # just to change all columns to `factor` for testing
  select_if(~nlevels(.)  >= 2)%>%
  select_if(~ nlevels(.) <=10)%>%
  subset(select = -c(gradeG1,gradeG2,gradeG3,gradeavg,difference_G1_G2,difference_G2_G3,difference_ALL))

multi_anova_var<-function(var){
  var=as.character(var)
  res.multi.anova.var <-
    multi.tests(fun = oneway.test,
                df = student_ndo,
                vars = c("difference_G1_G2","difference_G2_G3","difference_ALL"),
                group.var = var,
                var.equal = TRUE)
  
  arr.multi.anova.var=array(unlist(res.multi.anova.var),dim=c(6,3),dimnames=c(list(c("statistic (F)","num df","denom df","p.value","method","data.name"),c("difference_G1_G2","difference_G2_G3","difference_ALL"))))
  df.multi.anova.var=data.frame(arr.multi.anova.var)
  df.multi.anova.var=as.data.frame(t(df.multi.anova.var))
  df.multi.anova.var$p.value<-sapply(df.multi.anova.var$p.value,as.numeric)
  return(df.multi.anova.var)
}

multi_anova_ndo=list()

multi_anova_ndo_improvement=matrix(ncol=3,nrow=length(names(student_ndo_factor)),dimnames=list(names(student_ndo_factor),c("difference_G1_G2","difference_G2_G3","difference_ALL")),byrow=TRUE)

for(i in names(student_ndo_factor))
{
  multi_anova_ndo[[i]]=multi_anova_var(i)
  p.value=as.data.frame(multi_anova_ndo[[i]][["p.value"]])
  colnames(p.value)[1]<-"p.value"
  rownames(p.value)<-c("difference_G1_G2","difference_G2_G3","difference_ALL")
  multi_anova_ndo_improvement[as.character(i),"difference_G1_G2"]=p.value[1,1]
  multi_anova_ndo_improvement[as.character(i),"difference_G2_G3"]=p.value[2,1]
  multi_anova_ndo_improvement[as.character(i),"difference_ALL"]=p.value[3,1]
}

# multi_anova_ndo=list()
# multi_anova_ndo_score=matrix(ncol=3,nrow=length(names(student_ndo_factor)),dimnames=list(names(student_ndo_factor),c("difference_G1_G2","difference_G2_G3","difference_ALL")),byrow=TRUE)
# 
# for(i in names(student_ndo_factor))
# {
#   
#   multi_anova_ndo[[i]]=as.data.frame(multi_anova_var(as.character(i)))
#   p.value=as.data.frame(multi_anova_ndo[[i]][["p.value"]])
#   colnames(p.value)[1]<-"p.value"
#   rownames(p.value)<-c("difference_G1_G2","difference_G2_G3","difference_ALL")
#   multi_anova_ndo_score[as.character(i),"difference_G1_G2"]=p.value[1,1]
#   multi_anova_ndo_score[as.character(i),"difference_G2_G3"]=p.value[2,1]
#   multi_anova_ndo_score[as.character(i),"difference_ALL"]=p.value[3,1]
#   
# }

#Special feature x: Rank
multi_anova_ndo_improvement<-data.frame(multi_anova_ndo_improvement)
sorted_anova_improvement<-arrange(multi_anova_ndo_improvement,difference_ALL)

student_ndo_factor=student_perf[,c(row.names(sorted_anova_improvement),"dropout")]

# category_numeric_plot<-function(data,var2,var1,p){
#   
#   p=as.numeric(p)
#   k=round(p,5)
# 
#   var1=as.name(var1)
#   var2=as.name(var2)
#  
#   #https://stackoverflow.com/questions/57262084/how-can-i-easily-combine-the-output-of-grouped-summaries-with-an-overall-output
#   sumstat=data.frame()
#   
#   sumstat<-data %>%
#     filter(dropout==FALSE)%>%
#     group_by(!!var2)%>%
#     summarise_at(vars(!!var1),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))%>%
#     bind_rows(.,
#               data %>%
#                 group_by(dropout)%>%
#                 filter(dropout==TRUE)%>%
#                 summarise_at(vars(!!var1),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))
#     )%>%
#     bind_rows(.,
#               data %>%
#                 summarise_at(vars(!!var1),list(min=min,Q1=~quantile(., probs = 0.25),median=median,Q3=~quantile(., probs = 0.75),max=max,IQR=IQR,mean=mean,mode=mode,sd=sd,skew=skewness))
#     )%>%
#     ungroup()
#   
#   
#   
#   df<-data %>%
#     select(v1=!!var1,v2=!!var2,dropout)%>%
#     filter(dropout==FALSE)
#   
#   df$v2=as.factor(df$v2)
#   # https://stackoverflow.com/questions/47169639/saving-each-modified-facet-in-ggplot2
#   box_plot<-ggplot(data=df,aes(x=v1,y=v2,color=v1))+
#     geom_boxplot()+
#     labs(x=var1,y=var2,color=var1,title=paste("P-val(Anova) between\n",var1," and",var2,"of ndo\nis",k),cex.main=1)
#   # facet_wrap(~dropout)+
#   # theme(strip.text.x = element_text(margin = margin(t = 10, b = 10)))
#   
#   df1<-data %>%
#     group_by(!!var1,!!var2) %>%
#     summarise(count = n()) %>%
#     mutate(var1.count = sum(count),
#            prop = count/sum(count)) %>%
#     ungroup()%>%
#     select(v1=!!var1,prop,v2=!!var2,var1.count)
#   
#   
#   df1$v2=as.factor(df1$v2)
#   bar_graph<-ggplot(x=v2,data=df1,aes(x=v2,y=prop,label=prop,fill=v1))+
#     geom_bar(stat="identity",position="dodge")+
#     # geom_text(aes(label=scales::percent(prop)),stat="identity",position=position_dodge(width=.9))+
#     labs(x = var2,fill=var1,y="Probability",title=paste("P-val(Anova) between\n",var1," and",var2,"of ndo\nis",k),cex.main=1)
#   
#   return(list(sumstat,box_plot,bar_graph))
# }

student_ndo_factor<-as.factor(student_ndo_factor)
list_improvement=list()
for(i in names(student_ndo_factor)){
  for(j in c("difference_G1_G2","difference_G2_G3","difference_ALL")){
    x=0
    if(i!=j&&i!="dropout"){
      x=multi_anova_ndo_improvement[[i,j]]
      list_improvement[[j]][[i]]=category_numeric_plot(student_perf,j,i,x)
    }
    # print(j)
    # print(list_improvement[[j]][[i]][1])
    # png(file=paste("D:/plot/","list_improvement_boxplot_",i,"_",j,".png",sep=""))
    # print(list_improvement[[j]][[i]][2])
    # dev.off()
    # png(file=paste("D:/plot/","list_improvement_bargraph_",i,"_",j,".png",sep=""))
    # print(list_improvement[[j]][[i]][3])
    # dev.off()
    # png(file=paste("list_improvement_grade","_",i,"_",j))
    # print(list_improvement[[i]][[j]][2])
    # dev.off()
    
    # factor_plot[[i]]=category_category_plot(student_factor,i,"dropout_year",sorted_chi[[i]][["p.value"]])
    # print(factor_plot[[i]][1])
    # print(factor_plot[[i]][2])
  }
  
}



for(j in names(student_ndo_factor)){
  list_boxplot=list()
  list_bargraph=list()
  list_scatterplot=list()
  for(i in c("difference_G1_G2","difference_G2_G3","difference_ALL")){
    list_boxplot[[i]]=list_improvement[[i]][[j]][[2]]
    list_bargraph[[i]]=list_improvement[[i]][[j]][[3]]
  }
  print(j)
  # png(file=paste("D:/plot/","list_improvement_boxplot_cn_",j,".png",sep=""))
  # grid.arrange(grobs=list_boxplot,ncol = 2,nrow=2,top=paste("Box plot of",j,"and improvement"))
  # dev.off()
  # png(file=paste("D:/plot/","list_improvement_bargraph_cn_",j,".png",sep=""))
  # grid.arrange(grobs=list_bargraph,ncol = 2,nrow=2,top=paste("Bar graph of",j,"and improvement"))
  # dev.off()
  
}

list_boxplot1=list()
list_boxplot2=list()
list_boxplot3=list()
list_bargraph1=list()
list_bargraph2=list()
list_bargraph3=list()
for(j in names(student_ndo_factor)){
  index <- match(j, names(student_ndo_factor))
  if(index<=12){
    list_boxplot1[[j]]=list_improvement[[3]][[j]][[2]]
    list_bargraph1[[j]]=list_improvement[[3]][[j]][[3]]
  }else if(index<=24){
    list_boxplot2[[j]]=list_improvement[[3]][[j]][[2]]
    list_bargraph2[[j]]=list_improvement[[3]][[j]][[3]]
  }else{
    list_boxplot3[[j]]=list_improvement[[3]][[j]][[2]]
    list_bargraph3[[j]]=list_improvement[[3]][[j]][[3]]
  }
  print(j)
  
}
png(file=paste("D:/plot/","list_improvement_boxplot1_tot_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot1,ncol =3 ,nrow=4,top=paste("Box plot in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_bargraph1_tot_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph1,ncol =3,nrow=4,top=paste("Bar graph in total changes"))
dev.off()

png(file=paste("D:/plot/","list_improvement_boxplot2_tot_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot2,ncol =3 ,nrow=4,top=paste("Box plot of in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_bargraph2_tot_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph2,ncol =3,nrow=4,top=paste("Bar graph of in total changes"))
dev.off()

png(file=paste("D:/plot/","list_improvement_boxplot3_tot_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_boxplot3,ncol =3 ,nrow=4,top=paste("Box plot of in total changes"))
dev.off()
png(file=paste("D:/plot/","list_improvement_bargraph3_tot_cn.png",sep=""),width=1080,height=1440)
grid.arrange(grobs=list_bargraph3,ncol =3,nrow=4,top=paste("Bar graph of in total changes"))
dev.off()


#Refernces
# A5C1D2H2I1M1N2O1R2T1 (2013). time series - Convert categorical variable to events count variable in R. [online] Stack Overflow. Available at: https://stackoverflow.com/questions/18471847/convert-categorical-variable-to-events-count-variable-in-r [Accessed 23 Aug. 2021].
# 
# Aizkalns, J. (2015). r - How to select only columns (type=factor) with less than n levels with dplyr? [online] Stack Overflow. Available at: https://stackoverflow.com/questions/50357011/how-to-dplyr::select-only-columns-type-factor-with-less-than-n-levels-with-dplyr [Accessed 22 Aug. 2021].
# 
# Cleland (2018). Sort a list of lists based on an internal list variable in R. [online] Stack Overflow. Available at: https://stackoverflow.com/questions/53265425/sort-a-list-of-lists-based-on-an-internal-list-variable-in-r [Accessed 22 Aug. 2021].
# 
# Dan (2010). r - How to find the statistical mode? [online] Stack Overflow. Available at: https://stackoverflow.com/a/2547551 [Accessed 22 Aug. 2021].
# 
# Holtz, Y. (2018). Specific order for boxplot categories. [online] www.r-graph-gallery.com. Available at: https://www.r-graph-gallery.com/22-order-boxplot-labels-by-names.html [Accessed 22 Aug. 2021].
# 
# Lin, Z. (2017). r - How to create a Marimekko/Mosaic plot in ggplot2. [online] Stack Overflow. Available at: https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2 [Accessed 22 Aug. 2021].
# 
# sumshyftw (2019). r - How can I easily combine the output of grouped summaries with an overall output for the data. [online] Stack Overflow. Available at: https://stackoverflow.com/questions/57262084/how-can-i-easily-combine-the-output-of-grouped-summaries-with-an-overall-output [Accessed 23 Aug. 2021].
# 
# Ura R-jp (2012). RPubs - Performing multiple t-tests on different variables between the same two groups. [online] rpubs.com. Available at: https://rpubs.com/kaz_yos/1204 [Accessed 22 Aug. 2021].