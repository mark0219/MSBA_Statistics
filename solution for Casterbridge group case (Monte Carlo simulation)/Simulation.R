rm(list = ls())

hist_anlst_demand <- c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)
excess_cost <- 6000
shortfall_cost <- 3600
base_contribution <- 4000
offers_to_make <- seq(10, 110, 1)
offers_to_make_tom <- 98
result_mtx_fix <- matrix(NA, ncol = 101)
result_mtx_tom <- matrix(NA, ncol = 101)
result_mtx_flex <- matrix(NA, ncol = 101)
result_mtx_fix_d <- matrix(NA, ncol = 101)
result_mtx_flex_d <- matrix(NA, ncol = 101)

for (k in 1:1000) {
  
  total_earnings_fixed_start <- c()
  total_earnings_tom <- c()
  total_earnings_flex <- c() 
  total_earnings_fix_d <- c()
  total_earnings_flex_d <- c()
  
  retention_rate_at_end_of_m <- c()
  noises <- c()
  unexpected_econ_grwth <- rnorm(1, 0, 0.05)             #Economic growth
  noises <- rnorm(15, 0, 0.1)                          
  retention_rate_at_end_of_m <- c(runif(2, 0.9, 1),
                                  runif(3, 0.95, 1),
                                  runif(1, 0.8, 1),
                                  runif(3, 0.9, 1),
                                  runif(1, 0.8, 1),
                                  runif(4, 0.9, 1),
                                  runif(1, 0.95, 1))
  
  for (i in 1:length(offers_to_make)) {
    
    analysts_at_start_of_m <- c()                     #Current available analysts in firm
    analysts_at_start_of_m_tom <- c()
    analysts_at_start_of_m_flex <- c()
    analysts_at_start_of_m_fix_d <- c()         
    analysts_at_start_of_m_flex_d <- c()        
    offers_accepted <- rbinom(1, offers_to_make[i], 0.7)
    offers_accepted_tom <- rbinom(1, offers_to_make_tom, 0.7)
    offers_accepted_flex_july <- 0.5 * offers_accepted
    offers_accepted_flex_sep <- offers_accepted_flex_july * runif(1, 0.7, 1)
    
    new_anlst_arr_at_end_of_m <- c(0,0,offers_accepted,0,0,0,0,0,0,0,0,0,0,0,0)          #Newly joinned recruits
    new_anlst_arr_at_end_of_m_tom <- c(0,0,offers_to_make_tom,0,0,0,0,0,0,0,0,0,0,0,0)
    new_anlst_arr_at_end_of_m_flex <- c(0,0,offers_accepted_flex_july,0,offers_accepted_flex_sep,0,0,0,0,0,0,0,0,0,0)
    
    for (j in 1:15) {                                                                  #Analysts at start of each month (Supply)
      if (j == 1) {                                          
        analysts_at_start_of_m[j] <- 63
        analysts_at_start_of_m_tom[j] <- 63
        analysts_at_start_of_m_flex[j] <- 63
        analysts_at_start_of_m_fix_d[j] <- 63
        analysts_at_start_of_m_flex_d[j] <- 63
      } else {
        analysts_at_start_of_m[j] <- round(analysts_at_start_of_m[j-1] * retention_rate_at_end_of_m[j-1]) + 
          new_anlst_arr_at_end_of_m[j-1]
        analysts_at_start_of_m_tom[j] <- round(analysts_at_start_of_m_tom[j-1] * retention_rate_at_end_of_m[j-1]) + 
          new_anlst_arr_at_end_of_m_tom[j-1]
        analysts_at_start_of_m_flex[j] <- round(analysts_at_start_of_m_flex[j-1] * retention_rate_at_end_of_m[j-1]) + 
          new_anlst_arr_at_end_of_m_flex[j-1]
        analysts_at_start_of_m_fix_d[j] <- round(analysts_at_start_of_m_fix_d[j-1] * retention_rate_at_end_of_m[j-1] + 
          new_anlst_arr_at_end_of_m[j-1])      #Still using fixed strategy arriving vector, so that we will know the shortfall between June and December. Will need to update analysts_at_start_of_m_fix_d later.
        analysts_at_start_of_m_flex_d[j] <- round(analysts_at_start_of_m_flex_d[j-1] * retention_rate_at_end_of_m[j-1] + 
          new_anlst_arr_at_end_of_m_flex[j-1]) #Still using flex strategy arriving vector, so that we will know the shortfall between June and December. Will need to update analysts_at_start_of_m_flex_d later.
      }                                   
    }
    actual_anlst_demand <- round(hist_anlst_demand * ((1 + unexpected_econ_grwth) * (1 + noises)))
    
    short_falls <- pmax((actual_anlst_demand - analysts_at_start_of_m), 0)              #Fix strategy
    excesses <- pmax((analysts_at_start_of_m - actual_anlst_demand), 0)
    
    short_falls_tom <- pmax((actual_anlst_demand - analysts_at_start_of_m_tom), 0)      #Tom's strategy
    excesses_tom <- pmax((analysts_at_start_of_m_tom - actual_anlst_demand), 0)
    
    short_falls_flex <- pmax((actual_anlst_demand - analysts_at_start_of_m_flex), 0)    #Flex strategy
    excesses_flex <- pmax((analysts_at_start_of_m_flex - actual_anlst_demand), 0)
    
    short_falls_fix_d <- pmax((actual_anlst_demand - analysts_at_start_of_m_fix_d), 0)    #Temporary short_falls vector for Fixed-December strategy, will need to update this later.
    
    short_falls_flex_d <- pmax((actual_anlst_demand - analysts_at_start_of_m_flex_d), 0)  #Temporary short_falls vector for Flex-December strategy, will need to update this later.
    
    demand_in_j_d <- round(mean(short_falls_fix_d[3:9]) / 0.7)
    offers_to_make_fix_d <- rbinom(1, demand_in_j_d, 0.7)
    new_anlst_arr_at_end_of_m_fix_d <- c(0,0,offers_accepted,0,0,0,0,0,offers_to_make_fix_d,0,0,0,0,0,0)  #For Fix-December strategy, planning offers for both June and December.
    
    demand_in_j_d_flex_d <- round(mean(short_falls_flex_d[3:9]) / 0.7)
    offers_to_make_flex_d <- rbinom(1, demand_in_j_d_flex_d, 0.7)
    new_anlst_arr_at_end_of_m_flex_d <- c(0,0,offers_accepted_flex_july,0,offers_accepted_flex_sep,0,0,0,offers_to_make_flex_d,0,0,0,0,0,0)  #For Flex-December strategy, planning offers for Both June and December (Flex).
    
    
    for (l in 1:15) {             #This loop computes strategies that involving December hiring only!!!
      if (l == 1) {
        analysts_at_start_of_m_fix_d[l] <- 63
        analysts_at_start_of_m_flex_d[l] <- 63
      } else {
        analysts_at_start_of_m_fix_d[l] <- round(analysts_at_start_of_m_fix_d[l-1] * retention_rate_at_end_of_m[l-1] + 
          new_anlst_arr_at_end_of_m_fix_d[l-1])
        analysts_at_start_of_m_flex_d[l] <- round(analysts_at_start_of_m_flex_d[l-1] * retention_rate_at_end_of_m[l-1] + 
          new_anlst_arr_at_end_of_m_flex_d[l-1])
      }
    }
    
    short_falls_fix_d <- pmax((actual_anlst_demand - analysts_at_start_of_m_fix_d), 0)    #This one is usable  (Fixed - December)
    excesses_fix_d <- pmax((analysts_at_start_of_m_fix_d - actual_anlst_demand), 0)
    
    short_falls_flex_d <- pmax((actual_anlst_demand - analysts_at_start_of_m_flex_d), 0)  #This one is usable  (Flex - December)
    excesses_flex_d <- pmax((analysts_at_start_of_m_flex_d - actual_anlst_demand), 0)
    
    contr_to_earnings_fixed_start <- actual_anlst_demand * base_contribution -        #Earnings for fix strategy
      (short_falls * shortfall_cost + excesses * excess_cost)

    contr_to_earnings_tom <- actual_anlst_demand * base_contribution -                #Earnings for Tom's strategy
      (short_falls_tom * shortfall_cost + excesses_tom * excess_cost)
    
    contr_to_earnings_flex <- actual_anlst_demand * base_contribution -               #Earnings for flex strategy
      (short_falls_flex * shortfall_cost + excesses_flex * excess_cost)
    
    contr_to_earnings_fix_d <- actual_anlst_demand * base_contribution -              #Earnings for Fix-December strategy
      (short_falls_fix_d * shortfall_cost + excesses_fix_d * excess_cost)
    
    contr_to_earnings_flex_d <- actual_anlst_demand * base_contribution -             #Earnings for Flex-December strategy
      (short_falls_flex_d * shortfall_cost + excesses_flex_d * excess_cost)
    
    total_earnings_fixed_start <- c(total_earnings_fixed_start, sum(contr_to_earnings_fixed_start))
    total_earnings_tom <- c(total_earnings_tom, sum(contr_to_earnings_tom))
    total_earnings_flex <- c(total_earnings_flex, sum(contr_to_earnings_flex))
    total_earnings_fix_d <- c(total_earnings_fix_d, sum(contr_to_earnings_fix_d)-22000)     #Fixed-December
    total_earnings_flex_d <- c(total_earnings_flex_d, sum(contr_to_earnings_flex_d)-22000)  #Flex-December
  }
  
  result_mtx_fix <-rbind(result_mtx_fix, total_earnings_fixed_start)
  result_mtx_tom <- rbind(result_mtx_tom, total_earnings_tom)
  result_mtx_flex <- rbind(result_mtx_flex, total_earnings_flex)
  result_mtx_fix_d <- rbind(result_mtx_fix_d, total_earnings_fix_d)               #Fixed-December
  result_mtx_flex_d <- rbind(result_mtx_flex_d, total_earnings_flex_d)            #Flex-December
}

result_mtx_fix <- result_mtx_fix[-1,]
result_mtx_tom <- result_mtx_tom[-1,]
result_mtx_flex <- result_mtx_flex[-1,]
result_mtx_fix_d <- result_mtx_fix_d[-1,]       #Fixed-December
result_mtx_flex_d <- result_mtx_flex_d[-1,]     #Flex-December

total_earnings_all_q_fixed <- c()
for (i in 1:ncol(result_mtx_fix)) {
  total_earnings_all_q_fixed[i] <- mean(result_mtx_fix[,i])
}

total_earnings_all_q_tom <- c()
for (i in 1:ncol(result_mtx_tom)) {
  total_earnings_all_q_tom[i] <- mean(result_mtx_tom[,i])
}

total_earnings_all_q_flex <- c()
for (i in 1:ncol(result_mtx_flex)) {
  total_earnings_all_q_flex[i] <- mean(result_mtx_flex[,i])
}

total_earnings_all_q_fix_d <- c()               #Fixed-December
for (i in 1:ncol(result_mtx_fix_d)) {
  total_earnings_all_q_fix_d[i] <- mean(result_mtx_fix_d[,i])
}

total_earnings_all_q_flex_d <- c()              #Flex-December
for (i in 1:ncol(result_mtx_flex_d)) {
  total_earnings_all_q_flex_d[i] <- mean(result_mtx_flex_d[,i])
}

expected_diff_fix_tom <- total_earnings_all_q_fixed[which.max(total_earnings_all_q_fixed)] -        #Fix - Tom diff
  total_earnings_all_q_tom[sample(length(total_earnings_all_q_tom), 1, replace = T)]
expected_diff_flex_fix <- total_earnings_all_q_flex[which.max(total_earnings_all_q_flex)] -         #Flex - Fix diff
  total_earnings_all_q_fixed[which.max(total_earnings_all_q_fixed)]
expected_diff_flex_d_fix_d <- total_earnings_all_q_flex_d[which.max(total_earnings_all_q_flex_d)] - #Flex-D - Fix-D diff
  total_earnings_all_q_fix_d[which.max(total_earnings_all_q_fix_d)]

#d:
print(paste('Given the fixed starting month strategy, the maximum contributions to earning:', 
            total_earnings_all_q_fixed[which.max(total_earnings_all_q_fixed)], 'is achieved by giving',
            which.max(total_earnings_all_q_fixed)+9, 'offers.'))
plot(total_earnings_all_q_fixed, xaxt = 'n', pch = 16, xlab = 'Number of Offers', ylab = 'Total Contribution to Earnings')       #Profit plot for fixed starting strategy
axis(1, at=1:101, labels=10:110)

print(paste('Given the strategy suggested by Tom, the contributions to earning is constant:',
            total_earnings_all_q_tom[sample(length(total_earnings_all_q_tom), 1, replace = T)]))
plot(total_earnings_all_q_tom, xaxt = 'n', pch = 16, type = 'b', xlab = 'Number of Offers', ylab = 'Total Contribution to Earnings')         #Profit plot for Tom's strategy
axis(1, at=1:101, labels=rep(98, 101))

print(paste('The expected difference in earnings between the fixed-start and Tom"s strategy is:', 
            expected_diff_fix_tom))
#e:
print(paste('Given the flexible starting month strategy, the maximum contributions to earning:', 
            total_earnings_all_q_flex[which.max(total_earnings_all_q_flex)], 'is achieved by giving',
            which.max(total_earnings_all_q_flex)+9, 'offers.'))
plot(total_earnings_all_q_flex, xaxt = 'n', pch = 16, type = 'b', xlab = 'Number of Offers', ylab = 'Total Contribution to Earnings')        #Profit plot for flex strategy
axis(1, at=1:101, labels=10:110)

print(paste('The expected difference in earnings between the Flexible-Start and Fixed-Start strategy is:', 
            expected_diff_flex_fix))

#f:
print(paste('Given the Fix-December mix strategy, the maximum contributions to earning:', 
            total_earnings_all_q_fix_d[which.max(total_earnings_all_q_fix_d)],
            'is achieved by giving', which.max(total_earnings_all_q_fix_d)+9,
            'offers in June and some number of offers in December. The actual number of offers for December should be determined by the sum of shortage from June through December after take the number of June offers into account.'))

print(paste('Given the Flexible-December mix strategy, the maximum contributions to earning:', 
            total_earnings_all_q_flex_d[which.max(total_earnings_all_q_flex_d)],
            'is achieved by giving', which.max(total_earnings_all_q_flex_d)+9,
            'offers in June and some number of offers in December. The actual number of offers for December should be determined by the sum of shortage from June through December after take the number of June offers into account.'))

print(paste('The expected difference in earnings between the Fixed-December and Flexible-December strategy is:', 
            expected_diff_flex_d_fix_d))

#Final words:
final_table <- t(as.matrix(data.frame(c(total_earnings_all_q_fixed[which.max(total_earnings_all_q_fixed)],
                                        which.max(total_earnings_all_q_fixed)+9),
                                      c(round(total_earnings_all_q_tom[sample(length(total_earnings_all_q_tom), 1, replace = T)], 2),
                                        98),
                                      c(total_earnings_all_q_flex[which.max(total_earnings_all_q_flex)],
                                        which.max(total_earnings_all_q_flex)+9),
                                      c(total_earnings_all_q_fix_d[which.max(total_earnings_all_q_fix_d)],
                                        which.max(total_earnings_all_q_fix_d)+9),
                                      c(total_earnings_all_q_flex_d[which.max(total_earnings_all_q_flex_d)],
                                        which.max(total_earnings_all_q_flex_d)+9))))
colnames(final_table) <- c('Expected Contributions to Profit',
                           'Number of Offers to Give in June')
rownames(final_table) <- c('Fixed-Starting',
                           'Tom"s Suggestion',
                           'Flexible-Starting',
                           'Fixed-December mix',
                           'Flexible-December mix')

final_table
print('By inspecting the table print-out, we can see that the Flexible-December mix strategy would produce the highest contributions to earnings. Therefore, the Flexible-December hiring strategy is suggested.')
