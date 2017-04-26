#Use naive_bayes_binary.csv 
# Objective:
# The data is for a 3 class classififcation problem with 10 binary variables.
# True class is the 11th column of the data file
# (a) Using the ???rst half of the data set, train a naive Bayes classi???er. 
# (b) Using the 2nd half of the data set, classify each vector and construct the confusion matrix.


X = read.csv("naive_bayes_binary.csv")
n = nrow(X)

# Randomly split the data into train and test
train_index = sample(1:n,size=0.5*n)
train = X[train_index,]
test = X[-train_index,]

p = matrix(0,nrow=3,ncol=11);   # p[c,j] will be estimate of P(X_j = 1 | Y = c)

for (c in 1:3) {  # for each class
  z = train[train$V11==(c),];  # pull out data corresponding to class c
  p[c,] = colSums(z)/nrow(z);  # p[c,j] = P(X_j = 1 | Y = c)
}

prior = rep(1/3,3);		# bayes classifier requires prior dist.  assume all classes ='ly likely

yhat = rep(0,nrow(test));	# the estimated class for each example
for (i in 1:nrow(test)) {  # for each example
  pc = prior;	     # begin with prior dist
  o = test[i,];	     # ith observation
  for (c in 1:3) {  # for each class
    for (j in 1:10) { # for each feature
      prob = ifelse(o[j], p[c,j] , 1-p[c,j]);	# if jth feat is 1 take p[c,j].  ow take 1-p[c,j]
      pc[c] = pc[c] * prob;      # accumulate prob in pc[c]
    }
  }
  yhat[i] = which.max(pc);  # take maximizing class
}
error_rate =  sum(yhat != test$V11)/nrow(test)
print(error_rate)  # error rate when testing on training. 

