#----------------------------------------------------------
# clear objects from workspace
#----------------------------------------------------------
rm(list = ls())

#----------------------------------------------------------
# Generic Function OrcaLifecycle for the 4 Scenarios
#----------------------------------------------------------
OrcaLifecycle <- function(enSD, enMu, scenario){
  # Declare an empty vector list for ages for females that stop breeding. 
  ages <- c() 
  # Declare an empty vector list for calves that become independent of their mothers.
  offsprings <- c() 
  # Declare an empty vector list for total number of offspring that reach recruitment from every female whale.
  recruits <- c() 
  
  # Create a for loop for 1000 observations.
  for (female_whales in 1:1000){ 
    #---------------------------------------------------------- 
    # Declare and initialize variables 
    #----------------------------------------------------------
    # rnorm is used to generate normal random numbers having number, mean and standard deviation as parameters.
    # Declare a variable cond for condition of each individual female with mean 100 and standard deviation 10
    co <- rnorm(1,100,10) 
    
    # Assign age when females become reproductively active to 15 
    age <- 15
    
    # Declare and Initialize variable alive 1 when female is alive and 0 if dead 
    alive <- 1
    
    # Set the probability for a whale survival to 0.8 
    sr <- 0.8 
   
    
    # Declare and initialize variables of linear predictor
    s0 <- -2 
    s1 <- 0.05 
    # Declare and intialize variable Calf with 0. 
    calf <- 0 
    # Declare the variable for the age of calf and initialize with 0. 
    calfage <- 0 
    # Declare variable offspring for tracking the number of calves that become independent of their mothers 
    offspring <- 0 
    # Declare and initialize variables of other linear predictors
    b0 <- -10 
    b1 <- 0.1 
    b <- 3 
    # Declare variable to decrement the female's condition, corresponding to annual maternal investment. 
    inv <- 10 
    # Assign the probability that a calf randomly to survive for up to 5 years. 
    probCalf_survives5 <- 0.8 
    # Assign the probability that a calf randomly to survive from 6 to 15 years. 
    probCalf_survives6to15 <- 0.98 
    
    #---------------------------------------------------------- 
    # Implement Conditions 
    #---------------------------------------------------------- 
    # Create a while loop for condition that female should be alive and be of reproductive age between 15 and 39. 
    while (alive == 1 && (age >= 15 && age <= 39)) 
    { 
      # Show status of female killer whales using Binomial Distribution, rbinom accepts number of observations, number of trials and probability of success.
      alive <- rbinom(1,1, sr) 
      # Increment age, if the female is alive 
      if (alive == 1) 
      { 
        age <- age+1 
      } 
      # Increase the condition of female yearly using a normal distribution. enMu, enSD are passed as parameters because they are unique for each scenarios 
      co <- co + rnorm(1, enMu, enSD) 
      # Make the probability of survival sr depends on the linear predictor with condition. 
      sr <- exp(s0+s1*co)/(1+exp(s0+s1*co)) 
      # if the female does not have a calf, 
      if (calf == 0) 
      { 
        # Get the probability of breeding 'b', using linear predictor
        b <- exp(b0+b1*co)/(1+exp(b0+b1*co)) 
        # Use probability (b) is used as part of a Bernoulli trial to simulate the birth event 
        # Binomial Distribution when value 1 with size 1 given the probability of breeding
        calf <- rbinom(1,1, b) 
      } 
      # else, the female has a calf, 
      else 
      { 
        # Decrement the condition of the female by 10. 
        co <- co-inv; 
        # Binomial Distribution when value 1 with size 1 and the given the probability that a calf survives up to 5 years which is 0.8
        calf <- rbinom(1,1, probCalf_survives5); 
        # check if the calf reaches the first 5 years, 
        if (calf == 1) 
        { 
          # Increment the age calfage by 1. 
          calfage <- calfage+1; 
          # Check if the calf has reached the age of independence i.e. greater than 5, 
          if (calfage > 5) 
          { 
            # Increment offspring
            offspring <- offspring+1; 
            # Re-initialize calf to 0. 
            calf <- 0; 
            # Re-initialize calfage to 0. 
            calfage <- 0; 
          } 
        } 
        # else, the calf has not reached independence 
        else 
        { 
          # Re-initialize calf to 0. 
          calfage <- 0; 
          # Re-initialize calfage to 0.
          calf <- 0; 
        } 
      } 
    }
    # While Loop ends here
    
    # Add the age to the list of ages 
    ages <- c(ages, age)
    # Add the offspring to the list of offsprings
    offsprings <- c(offsprings, offspring) 
    # Declare loop counter 
    iCounter <- 0 
    # Declare variable to check offspring reaching 15 or not 
    irecruits_offspring15 <- 0 
    # Declare year and initialize to 1 
    year <- 1 
    # check if calf is alive 
    calf_alive <- 0 
    # Create a while loop for to check the number of calfs that became independent. 
    while (iCounter < offspring) { 
      # Create a for loop randomly decides if the calf will survive from 6 up to 15 years or not, probability of survival defined as 0.98 
      for (years in 1:10) { 
        calf_alive <- rbinom (1,1, probCalf_survives6to15); 
        # Exit loop if calf dies within 10 years
        if (calf_alive == 0) { 
          break;
        } 
      } 
      # If Calf reaches the age of 15 and still alive, increase the recruited offspring by 1
      if (calf_alive == 1) { 
        irecruits_offspring15 <- irecruits_offspring15+1; 
      } 
      # Increment Counter
      iCounter <- iCounter+1 
    }
    # End While loop here
    # Recruited offsprings are added to the list of recruits 
    recruits <- c(recruits, irecruits_offspring15) 
  }
  # For loop ends here
  
  #---------------------------------------------------------- 
  # Print 
  #---------------------------------------------------------- 
  # Prints list of ages of death of females. 
  print(ages) 
  # Prints list of total number of offsprings 
  print(offsprings)
  # Prints list of number of recruited offsprings 
  print(recruits) 
  # Calculates average number of recruited offsprings 
  avg <- sum(recruits)/1000 
  # Prints total number of offspring 
  print(sum(offsprings)) 
  # Prints total number of recruited offsprings 
  print(sum(recruits)) 
  # Prints average number of recruited offsprings 
  print(avg) 
  
  #---------------------------------------------------------- 
  # Histogram plots 
  #---------------------------------------------------------- 
  # Plot Histogram to show age of Female Killer Whales that died
  hist(ages,axes = TRUE,break=50,main=paste(scenario, "Ages histogram", sep=" : "),xlab ="Age",ylab ="Frequency",col=rgb(0,0,1,0.5)) 
  # Plot Histogram of inclusive fitness Female Killer Whales 
  hist(recruits,axes = TRUE,break=50,main=paste(scenario, "Inclusive fitness histogram", sep=" : "),xlab ="Recruits  by each female",ylab ="Frequency",col=rgb(0,0,1,0.5)) 
  #---------------------------------------------------------- 
  # End 
  #----------------------------------------------------------
}

# Assign standard deviation (enSD) and mean (enMu) which represents the states of environment. Mean and the Standard Deviation changes for each scenario
# Scenario 1 : Rich and stable environment, set enMu=20 and enSD=1
enMu <- 20
enSD <- 1
OrcaLifecycle(enSD, enMu, "Scenario I")

# Scenario 2 : Poor and stable environment, set enMu=2 and enSD=1
enMu <- 2
enSD <- 1
OrcaLifecycle(enSD, enMu, "Scenario II")

# Scenario 3 : Rich and variable environment, Set enMu=20 and enSD=30
enMu <- 30
enSD <- 20
OrcaLifecycle(enSD, enMu, "scenario III")

# Scenario 4 : Poor and variable environment, Set enMu=2 and enSD=30
enMu <- 2
enSD <- 30
OrcaLifecycle(enSD, enMu, "Scenario IV")

