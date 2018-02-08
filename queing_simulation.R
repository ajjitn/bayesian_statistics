#########################
## Simulating M/M/3 queue
#########################

#office open 7 hours
#3 doctors available to service patients
#time with patients ~ Unif(5,20)
#time until next arrival ~ Exp(1/10)


total_time = 7*60
num_cust = c()
num_wait_cust= c()
mean_q_time = c()
time_close = c()
for(j in 1:1000){
  interval = rexp(200,rate = 1/10)
  processing_time = runif(200, min=5, max=20)
  ncust_logi = cumsum(interval)<total_time
  
  interval = interval[ncust_logi]
  processing_time = processing_time[ncust_logi]
  arrival_time = cumsum(interval)
  n_cust = length(processing_time)
  q_time = c(rep(NA, n_cust))
  exit_time =  c(rep(NA, n_cust))
  doc_1 = c(rep(NA, n_cust))
  doc_2 = c(rep(NA, n_cust))
  doc_3 = c(rep(NA, n_cust))
  
  #setting q_time for first 3 cust = 0, and calculating thier exit times given that
  q_time[1:3] = 0
  exit_time[1:3] = processing_time[1:3]arrival_time[1:3]
  #calculating doc exit times for first three people
  doc_1[1] = arrival_time[1]processing_time[1]
  doc_2[2] = arrival_time[2]processing_time[2]
  doc_3[3] = arrival_time[3]processing_time[3]
  
  
  for( i in 4:(n_cust)){
    if(arrival_time[i] > max(doc_1, na.rm=T)){ #doc_1 is free
      q_time[i] = 0 
      exit_time[i] = arrival_time[i]processing_time[i]q_time[i]
      doc_1[i] = exit_time[i]
    }
    else if(arrival_time[i] > max(doc_2, na.rm=T)){ #doc_2 is free
      q_time[i] = 0 
      exit_time[i] = arrival_time[i]processing_time[i]q_time[i]
      doc_2[i] = exit_time[i]
    }
    else if(arrival_time[i] > max(doc_3, na.rm=T)){ #doc_3 is free
      q_time[i] = 0 
      exit_time[i] = arrival_time[i]processing_time[i]q_time[i]
      doc_3[i] = exit_time[i]
    } else{ #no doctors are free
      q_time[i] = min(max(doc_1, na.rm=T), max(doc_2, na.rm=T), max(doc_3, na.rm=T)) - arrival_time[i]
      exit_time[i] = arrival_time[i]processing_time[i]q_time[i]
      doc_service = which.min(c(max(doc_1, na.rm=T), max(doc_2, na.rm=T), max(doc_3, na.rm=T)))
      if(doc_service == 1){
        doc_1[i] = exit_time[i]
      }
      if(doc_service== 2){
        doc_2[i] = exit_time[i]  
      }
      if(doc_service== 3){
        doc_3[i] = exit_time[i]  
      }
    }
  }
  
  num_cust[j]      = n_cust
  num_wait_cust[j] = sum(q_time>0)
  mean_q_time[j]   =  mean(q_time)
  if(max(exit_time)<420){
    time_close[j] = total_time
  }else{
    time_close[j]= max(exit_time)
  }
}
