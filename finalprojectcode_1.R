# DECISION ANALYSIS FOR TAKING OR NOT TAKING FLU VACCINES



set.seed(123)
# data from CDC of vaccine effectiveness 
data_VE <-(0.01)*c(56,	60,	47,	49,	52,	19,	48,	40,	38,	29,	39,	36,	30,	44,	56)
#let us estimate the average vaccine effectiveness percentage
averageVE<-mean(data_VE)
varianceVE<-var(data_VE)
medianVE<-median(data_VE)

cat("Estimated average VE is", averageVE,"\n")

sim_num <- 10000
#estimate average cost of vaccine
data_price<-runif(sim_num,20,100)
common_price<-round(mean(data_price),2)
cat("Estimated price of Vaccine is", common_price,"\n")
# let's do flu probability which is between %5 and %20
sim_prob<-runif(sim_num,0.05,0.2)
common_prob<-median(sim_prob)
cat("Estimated probability of getting flu", common_prob,"\n")
# Inputs
admin_cost<-21.93
p_of_flu <- common_prob  # $21.93 adminsitration fee         
vac_eff<- averageVE             
cost_vaccine <- common_price + admin_cost         
cost_if_flu <- 273 #average treatment cost if get flu            
v_coverage <- 0.467 # vaccination coverage
inf_prob<-(1 - (v_coverage * vac_eff)) 
infection_rate <- p_of_flu * inf_prob #infection rate
# If take the vaccine scenario
flu_with_v <- infection_rate * (1 - vac_eff)  
got_flu_vax <- rbinom(sim_num,1, flu_with_v) #population of people who have vaccine and did or did not get flu
cost_v <- got_flu_vax * cost_if_flu + cost_vaccine
mean_cost_vax <- mean(cost_v)

##  Don't take the vaccine scenario
flu_without_v <- infection_rate
got_flu_novax <- rbinom(sim_num,1, flu_without_v )#population of people who don't have vaccine and did or did not get flu
cost_no_v <- got_flu_novax * cost_if_flu  # treatment cost
mean_cost_novax <- mean(cost_no_v)


cat("Expected cost of the population WITH vaccine:    $", round(mean_cost_vax, 2), "\n")
cat("Expected cost of the population WITHOUT vaccine: $", round(mean_cost_novax, 2), "\n")
if(round(mean_cost_vax, 2)>round(mean_cost_novax, 2)){print("Not Taking Vaccine is best option")}else{print(
                                                           "Taking Vaccine is best option")}
par(mfrow=c(2,2))
# Plot cost distribution for both strategies
hist(cost_novax, breaks=30, col="red",
     xlim=c(0, max(c(cost_novax, cost_vax))), xlab="Vaccine Cost",
     main="Cost Distribution: With vs Without Vaccine")
hist(cost_vax, breaks=30, col="lightblue", add=TRUE)
legend("topright", legend=c("No Vaccine", "Vaccine"),
       fill=c("red", "lightblue"))
# Density plot comparison
plot(density(cost_no_v),
     col="red", lwd=2,
     main="Density Plot: Cost With vs Without Vaccine",
     xlab="Cost", xlim=c(0, max(c(cost_no_v, cost_v))))
lines(density(cost_v), col="blue", lwd=2)
legend("topright", legend=c("No Vaccine", "Vaccine"),
       col=c("red", "blue"), lwd=2)
grid()
