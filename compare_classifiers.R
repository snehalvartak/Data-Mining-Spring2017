

# 4_9.csv gives classi???cation accuracy of 3 di???erent classi???cation techniques: 
# decision trees, naive Bayes, and support vector machines. Compare each pair of techniques on each data set, 
# deciding the comparision as a win, loss, or draw for the ???rst technique of the pair. 
# Produce a 3x3 table with rows labeled by the techniques and columns labeled by win/loss/draw, 
# counting the number of data sets that fall into each cell.

data = read.csv("4_9.csv")

# for 0.95 confiidence interval we can look up the z-score for normal distribution it comes out to 1.96
z_score =1.96

#compute the error for each method
data$decision_error = 1 -(data$decision_tree/100)
data$naive_error = 1 -(data$naive_bayes/100)
data$svm_error = 1 -(data$svm/100)

#compute the difference in errors for each pair of methods
data$decision_naive_diff = data$decision_error - data$naive_error
data$naive_svm_diff = data$naive_error - data$svm_error
data$svm_decision_diff = data$svm_error - data$decision_error

#compute the variance for each of the differences
data$decision_naive_var = (data$decision_error*(1-data$decision_error) + data$naive_error*(1-data$naive_error))/data$size

data$naive_svm_var = (data$naive_error*(1-data$naive_error) + data$svm_error*(1-data$svm_error))/data$size

data$svm_decision_var = (data$svm_error*(1-data$svm_error) + data$decision_error*(1-data$decision_error))/data$size


#compute w for each group
data$decision_naive_width = z_score * sqrt(data$decision_naive_var)
data$naive_svm_width = z_score * sqrt(data$naive_svm_var)
data$svm_decison_width = z_score * sqrt(data$svm_decision_var)

#Compute (d+w)*(d-w) , if its -ve we can say its a draw and when its not a draw but d>0 then zero it is a win for the technique with lower error

data$decision_naive = (data$decision_naive_diff - data$decision_naive_width) *  (data$decision_naive_diff + data$decision_naive_width)
data$naive_svm = (data$naive_svm_diff - data$naive_svm_width) *  (data$naive_svm_diff + data$naive_svm_width)
data$svm_decision = (data$svm_decision_diff - data$svm_decison_width) *  (data$svm_decision_diff + data$svm_decison_width)

draws_decision_naive = 0
draws_naive_svm = 0
draws_svm_decision =0
naive_wins_decision =0
decision_loss_naive =0
decision_wins_naive =0
naive_loss_decision =0
svm_win_naive =0
naive_loss_svm =0
naive_win_svm =0
svm_loss_naive =0
decision_wins_svm =0
svm_loss_decision =0
decision_loss_svm =0
svm_win_decision =0

#Calculation draws, wins and loss
for(i in 1:23){
  if(data[i,"decision_naive"] < 0) 
    draws_decision_naive = draws_decision_naive + 1
  else{
    if(data[i,"decision_naive_diff"] > 0){
      naive_wins_decision = naive_wins_decision +1
      decision_loss_naive = decision_loss_naive + 1
    }
    else{
      decision_wins_naive = decision_wins_naive +1
      naive_loss_decision = naive_loss_decision +1
    }
  }
   if(data[i,"naive_svm"] < 0) 
     draws_naive_svm = draws_naive_svm + 1
   else{
     if(data[i,"naive_svm_diff"] > 0){
       svm_win_naive = svm_win_naive +1
       naive_loss_svm = naive_loss_svm + 1
     }
     else{
       naive_win_svm = naive_win_svm +1
       svm_loss_naive = svm_loss_naive +1
     }
   }
   if(data[i,"svm_decision"] < 0) 
    draws_svm_decision = draws_svm_decision + 1
   else{
     if(data[i,"svm_decision_diff"] > 0){
       decision_wins_svm = decision_wins_svm +1
       svm_loss_decision = svm_loss_decision + 1
     }
     else{
       decision_loss_svm = decision_loss_svm +1
       svm_win_decision = svm_win_decision + 1
     }
   }
}

 
