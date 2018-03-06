#Lab 4

#Given the following transaction data, complete the frequent item function
#This function should print the frequent item sets as it finds them
#Your function should take two parameters: the transaction data and the support level.
#I've written the code to generate the candidate keys.  Look over this code.
#It takes a frequent item set and a value for n, and returns the candidate set.
#Set your support = 0.5


support=0.5
transactiondata=list(c(1,3,4),c(2,3,5),c(1,2,3,5),c(2,5),c(2,3))
#Hint: to access each transaction use: transactiondata[[1]], transactiondata[[2]]...
#unique(unlist(transactiondata)) will get the unique values in the transaction data


freqitem(transactiondata,support)

freqitem<-function(transactiondata,support){
  
  #Find the 1-item frequent set (fset) that meet the support value from the transaction data
  
  #Set k=2
  
  #While the frequent set is not null:
    #Find a candidate set using (fset) of size k
    #If no candidates are found, exit the loop
    #Initialize the newfset (the next iteration of fset) to be null
    #For each candidate:
      #For each transaction
        #Check if the candidate is present.  If it is, increment the count
    
      #If the count exceeds the support percentage, add the candidate to newfset
  
    #If newfset is null, exit the loop
    
    #Set fset=newfset and increment k
    
    #Print fset
  
}



#Practice using this function
#Make the function call: 
candidategen(cbind(1,2,2,4),2)
candidategen(cbind(c(1,2),c(3,4),c(4,3),c(1,4),c(4,2)),3)

candidategen<-function(fset, n){
 
  cset<-NULL
  if(length(fset[1,])==1){
    return(NULL)
  }
  for(i in 1:(length(fset[1,])-1)){
    for(j in (i+1):length(fset[1,])){
      temp<-list(unique(c(c(fset[,i],fset[,j]))))
      if(length(temp[[1]])==n){
        cset<-append(cset,temp)
      }
    }
  }
  if(is.null(cset)){
    return(NULL)
  }
  finalcset<-NULL
  for(i in 1:length(cset)){
    temp<-cset[[i]]
    
    sset<-combn(temp,n-1)
    for(j in 1:length(sset[1,])){
      flag=0
      for(k in 1:length(fset[1,])){
        if(all(sset[,j] %in% fset[,k])){
          flag=1
        }
      }
      if(flag!=1){
        break
      }
    }
    if(flag==1){
      finalcset<-cbind(finalcset,temp)
    }
  }
  if(is.null(finalcset)){
    return(NULL)
  }
  
  for(i in 1:length(finalcset[1,])){
    finalcset[,i]<-finalcset[order(finalcset[,i]),i]
  }
  finalcset<-finalcset[,!duplicated.array(finalcset,MARGIN = 2)]
  
  return(as.matrix(finalcset))
  
}




#Part 2:
#Download the hashtag data from Moodle.
#Load the hashtags and try different support and confidence scores.

#Why does the support values for this type of data need to be set to such a low value?


library(arules)

load("~/Downloads/hashtag.R")


res<-apriori(hashtag,parameter = list(supp = 0.002,target="frequent itemsets"))

inspect(res)

res<-apriori(hashtag,parameter = list(supp = 0.002,conf=.9,target="rules"))

inspect(res)