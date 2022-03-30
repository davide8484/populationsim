
# Description:
#
# This function uses Census, schools and business counts data to generate a synthetic population for a specified 
# region. It performs the following steps. 
#   1. Randomly generates a set of households of various (types, sizes) based on Census data for the region. 
#   2. Assigns ages to the individuals in each household using Census data on the distribution of ages 
#   for the household's (type, size) within the region, along with assumptions about the age structure 
#   within households (e.g. the age gaps between children). 
#   3. Assigns school IDs to the children in each household using ACARA data on schools' locations and student 
#   counts. 
#   4. Assigns workplace IDs to the adults in each household using Census data on the region's labour force
#   and ABS CABEE data on the distribution of business sizes (employee counts) across different industries in 
#   the region.
#
# Inputs:
#   1. rand_seed (integer): the seed for the random number generator (the generation of the IDs has random aspects).
#   2. region_level (character): the region level for which to generate the IDs. Choose one of {"SA2", SA3", "SA4", 
#   "GCCSA", "state"}.
#   3. region (character): the name of the region for which to generate the IDs. I.e. the name of the region_level.
# 
# Output is a list with 5 elements. The first four are vectors containing the ages, household IDs, school IDs, 
# and workplace IDs of the individuals living in the region. A school ID of NA indicates the individual does not 
# attend school; a workplace ID of NA indicates the individual does not have a workplace. The fifth element is a 
# data set containing the IDs along with other info which might be of use, such as labour force status and sector 
# and industry of employment. 

age.and.id.generator.generator <- function(rand_seed, region_level, region) {
  
  library(dplyr)
  
  # Load the required data:
  
  # (i) Data for creating the households:
  load("SA2s_list.RData") # Each list element is a vector containing the ages of the individuals in each SA2
  load("SA2s_prob_houses.RData") # Probabilities for drawing household types and sizes (differ by SA2)
  load("SA2s_prob_children.RData") # Conditional probs of age given household (type, size) (for children). Differ by SA2
  load("SA2s_prob_adults.RData") # The same as above, but for adults
  load("SA2s_prob_all.RData") # The same as above, but for all ages
  
  # (ii) Schools data:
  load("SA2s_draw_primary.RData") # CDFs for drawing primary schools (differ by SA2)
  load("SA2s_draw_secondary.RData") # Same as above, but for high schools
  
  # (iii) Workplaces data:
  load("public_id_list.RData") 
  load("business_id_list.RData")
  load("work_probs.RData") # CDFs (one for each SA4) from which to draw an individual's (lf status, sector, industry)
  
  # (iv) Correspondence between the geocodes:
  load("SA_corr.RData")
  
  # Set the inputs:
  max_child_age <- 30
  min_adult_age <- 18
  group_sig <- 5 # Standard deviation of ages in group household
  prop_thresh <- 0.98 # Proportion of population assigned to houses according to our rules 
  max_failures <- 10 # Number of failures allowed before household is randomly formed
  
  set.seed(rand_seed)
  
  # Extract the region labels (at the various levels):
  if (region_level=="SA2") {
    region_labs <- subset(SA_corr, SA2==region)
  } else if (region_level=="SA3") {
    region_labs <- subset(SA_corr, SA3==region)
  } else if (region_level=="SA4") {
    region_labs <- subset(SA_corr, SA4==region)
  } else if (region_level=="GCCSA") {
    region_labs <- subset(SA_corr, GCCSA==region) 
  } else if (region_level=="state") {
    region_labs <- subset(SA_corr, STATE==region)
  }
  
  SA2s <- unique(region_labs$SA2) # Get the relevant SA2s (those that make up the region)
  num_SA2s <- length(SA2s) # The number of SA2s
  store_SA2s <- vector(mode="list", length=num_SA2s) # Create the list to store each SA2's households
  counter_house_ids <- 0 # A counter used to make the household IDs unique across multiple SA2s
  
  # Generate the synthetic households by iterating through the SA2s.
  
  for (loc in 1:num_SA2s) {
    
    # Get the relevant CDFs and vectors for drawing ages and IDs for the SA2:
    SA2 <- SA2s[loc] # Set SA2
    children <- SA2s_prob_children[[SA2]] # CDF for children
    adults <- SA2s_prob_adults[[SA2]] # CDF for adults
    rest <- SA2s_prob_all[[SA2]] # CDF for all
    draw_from_primary <- SA2_draw_primary[[SA2]] # CDF for primary schools
    draw_from_secondary <- SA2_draw_secondary[[SA2]] # CDF for secondary schools
    all_ages <- SA2s_list[[SA2]] # Ages of all SA2 inhabitants
    char_ages <- as.character(all_ages)
    prob_long <- SA2s_list_houses[[SA2]] # CDF for drawing household (type, size)
    
    # Initialise a few objects for the alg:
    total_people <- 0 # Counts the total number of people assigned to households
    prop_assigned <- 0 # Tracks the proportion of the SA2's population assigned to households
    id <- 1  # Initialise ID of first household formed
    proceed <- TRUE # Indicates whether to proceed to forming the next household (TRUE if previous household successfully formed)
    failures <- 0 # Counts the number of failed attempts to form the household
    pop_size <- length(all_ages) # SA2's population size
    houses <- matrix(rep(0,7*pop_size), ncol=7) # Matrix to store household characteristics
    
    # Get the counts of individuals in the SA2 by age:
    count_age <- rep(0, length(unique(char_ages)))
    
    for (q in 1:length(unique(char_ages))) {
      q2 <- as.character(q-1)
      count_age[q] <- sum(char_ages==q2)
    }
    
    # Generate the households and the ages of the household members:
    while (prop_assigned < prop_thresh) {
      
      # (i) Randomly draw household type and size.
      
      if (proceed==TRUE) {
        ind <- sum(runif(1,0,1) > prob_long$Cum_prob) + 1 # Randomly draw the index
        household <- prob_long[ind,] # Get household type and size
        type <- household$Type
        num_people <- household$Num_people
        num_adults <- household$Num_adults
        num_children <- household$Num_children
        
        # Create a matrix with the household's characteristics (to which ages will be added):
        house <- matrix(rep(0, num_people*5), nrow=num_people)
        house[,1] <- rep(type, num_people)
        house[,2] <- rep(num_people, num_people)
        house[,3] <- rep(num_adults, num_people)
        house[,4] <- rep(num_children, num_people)
      }
      
      # (ii) Given household type and size, propose the ages of the family members.
      
      if (failures < max_failures) {
        
        # (a) One family household: one parent family.
        
        if (household$Type == "One family household: One parent family") {
          a <- subset(adults, Type==household$Type & Num_people==household$Num_people)
          ages <- rep(0, num_people)
          ages[1] <- sum(runif(1,0,1) > a[4:(length(a[1,]) - 1)]) + min_adult_age # Parent's age
          ages[2] <- round(ages[1] - rnorm(1, 32, 4)) # Age of eldest child 
          
          # If more than 1 child, generate the other children's ages:
          if (num_children > 1) {
            age_diff <- rpois(num_children - 1, 2) + 1 # Generate age intervals from shifted Poisson
            
            for (k in 2:num_children) {
              ages[(k+1)] <- ages[k] - age_diff[(k-1)] # Ages for other children
            }
          }
          
          house[,5] <- ages # Assign the ages to the house matrix
          
          # (b) One family household: couple family with children.
          
        } else if (household$Type == "One family household: Couple family with children") {
          a <- subset(adults, Type==household$Type & Num_people==household$Num_people)
          ages <- rep(0, num_people)
          mu <- sum(runif(1,0,1) > a[4:(length(a[1,]) - 1)]) + min_adult_age
          ages[1:2] <- round(rnorm(2, mu, 2.1)) # Generate ages of parents
          ages[3] <- round(ages[1] - rnorm(1, 32, 4)) # Generate age of eldest child
          
          # If more than 1 child, generate the other children's ages:
          if (num_children > 1) {
            age_diff <- rpois(num_children - 1, 2) + 1 # Generate age intervals from shifted Poisson
            
            for (k in 2:num_children) {
              ages[(k+2)] <- ages[(k+1)] - age_diff[(k-1)] # Ages for other children
            }
          }
          
          house[,5] <- ages
          
          # (c) One family household: couple with no children.
          
        } else if (household$Type == "One family household: Couple family with no children") {
          d <- subset(adults, Type==household$Type & Num_people==household$Num_people)
          mean_age <- sum(runif(1,0,1) > d[4:101]) + min_adult_age
          ages <- as.character(round(rnorm(num_people, mean_age, 2.1))) # Generate ages
          
          for (y in 1:num_people) {
            ages[y] <- as.character(min(max(as.numeric(ages[y]), 0), 115)) # Rounding
          }
          
          house[,5] <- ages
          
          # (d) Group household.
          
        } else if (household$Type == "Group household") {
          d <- subset(adults, Type==household$Type & Num_people==household$Num_people)
          mean_age <- sum(runif(1,0,1) > d[4:101])
          ages <- as.character(round(rnorm(num_people, mean_age, group_sig))) # Generate ages
          
          for (y in 1:num_people) {
            ages[y] <- as.character(min(max(as.numeric(ages[y]), min_adult_age), 115)) # Rounding
          }
          
          house[,5] <- ages
          
          # (e) Lone person household
          
        } else if (household$Type == "Lone person household") {
          d <- subset(adults, Type==household$Type & Num_people==household$Num_people)
          ages <- sum(runif(1,0,1) > d[4:101]) + min_adult_age
          house[,5] <- ages
        }
        
        # (f) If failed to form a household after max_failures attempts, or household type is 'other', 
        # then randomly draw ages from those remaining in the age vector.
        
      } else { 
        count_age_cdf <- cumsum(count_age) / sum(count_age) # Turn counts by age into CDF
        
        for (h in 1:num_people) {
          ages <- sum(runif(1,0,1) > count_age_cdf) ### REPLACES ABOVE
          house[h,5] <- ages
        }
      }
      
      # (iii) If the proposed ages are available, set them as the household ages. Otherwise, re-attempt
      # to form the household by drawing new ages.
      
      valid <- 0 # Count the number of valid (available) ages
      
      for (l in 1:num_people) {
        age3 <- as.numeric(house[l,5]) # Draw the age
        indic <- age3 + 1
        if (age3 >= 0 & length(count_age[indic] > 0)) {
          if (is.na(count_age[indic]) == FALSE) {
            if (count_age[indic] > 0) {
              valid <- valid + 1 
            }
          }
        }
      }
      
      # If household successfully formed:
      if (valid == num_people) { 
        mark_failure <- ifelse(failures==max_failures, 1, 0) # Set as 1 if failed to form household max_failures times, else set to 0
        houses[(total_people + 1):(total_people + num_people),7] <- mark_failure # Indicator of whether the household was formed with structure or randomly
        proceed <- TRUE # Move to creating the next household
        failures <- 0 # Reset the number of failed attempts to 0
        houses[(total_people + 1):(total_people + num_people),1:5] <- house # Put the house characteristics in houses object
        houses[(total_people + 1):(total_people + num_people),6] <- id # Assign household ID
        id <- id + 1 # Iterate ID for next household
        total_people <- total_people + num_people # Update total number of people assigned to households
        
        # If an age is drawn, decrease the count of that age in count_age by 1:
        for (pe in 1:num_people) {
          age3 <- as.numeric(house[pe,5]) # Draw the age
          indic <- age3 + 1
          count_age[indic] <- count_age[indic] - 1
        }
        
        # If household not formed:
      } else { 
        proceed <- FALSE # Propose new ages for household members
        failures <- failures + 1 # Count the number of failed attempts to form household
      }
      
      prop_assigned <- total_people/pop_size # Track proportion of SA2's people assigned to houses
      print(prop_assigned) 
    }
    
    # Assign the remaining population to households by copying previous households:
    houses <- houses[1:total_people,] 
    remainder <- pop_size - total_people 
    copied <- houses[1:remainder,] 
    copied[,6] <- as.numeric(copied[,6]) + as.numeric(id) - 1 
    houses <- rbind(houses, copied) 
    
    # Make the last household of type 'other' and inhabited only by adults for coherence:
    max_id <- houses[length(houses[,6]), 6] 
    size_last <- sum(houses[,6] == max_id)
    houses[(length(houses[,6]) - size_last + 1) : length(houses[,6]), 1] <- rep('Other', size_last)
    houses[(length(houses[,2]) - size_last + 1) : length(houses[,2]), 2] <- rep(size_last, size_last)
    houses[(length(houses[,2]) - size_last + 1) : length(houses[,2]), 3] <- rep(size_last, size_last)
    houses[(length(houses[,2]) - size_last + 1) : length(houses[,2]), 4] <- rep(0, size_last)
    houses[(length(houses[,2]) - size_last + 1) : length(houses[,2]), 5] <- round(runif(size_last, 18, 60))
    houses <- data.frame(houses)
    names(houses) <- c("Type", "Num_people", "Num_adults", "Num_children", "Age", "Household_id", "Randomly_formed")
    
    # Formatting:
    houses$Household_id <- as.character(houses$Household_id)
    houses$Age <- as.numeric(as.character(houses$Age))
    houses$Num_children <- as.numeric(as.character(houses$Num_children))
    houses$Num_people <- as.numeric(as.character(houses$Num_people))
    houses$school_id <- NA
    num_houses <- as.numeric(houses$Household_id[length(houses$Household_id)])
    
    #----------------------------------------------------------------------------------
    
    # Assign the school IDs. 
    
    house_ind <- 1 
    
    for (i in 1:num_houses) {
      id <- as.character(i) 
      house <- subset(houses, Household_id == id)
      house$school_id <- NA
      num_kids <- sum(house$Age > 4 & house$Age < 19)
      
      if (num_kids > 0) {
        # Set the primary and secondary school IDs for the household:
        ind <- sum(runif(1,0,1) > draw_from_primary$cum_prob) + 1 # Randomly draw index
        primary_school_id <- draw_from_primary$school_id[ind] # Get the corresponding primary school ID
        ind <- sum(runif(1,0,1) > draw_from_secondary$cum_prob) + 1 # Randomly draw index
        secondary_school_id <- draw_from_secondary$school_id[ind] # Get the corresponding secondary school ID
        
        # Assign the school IDs to the children based on their ages:
        for (j in 1:house$Num_people[1]) {
          if (house$Age[j] > 4 & house$Age[j] < 12) {
            house$school_id[j] <- primary_school_id
          } else if (house$Age[j] > 11 & house$Age[j] < 19) {
            house$school_id[j] <- secondary_school_id
          } else {
            house$school_id[j] <- NA
          }
        }
      }
      
      houses$school_id[house_ind:(house_ind + house$Num_people[1] - 1)] <- house$school_id
      house_ind <- house_ind + house$Num_people[1] # Update the first ind for generating the school IDs
    }
    
    #-------------------------------------------------------------------------------------------
    
    # Assign the workplace IDs.
    
    # Get the relevant SA4's CDF from which to sample labour force status:
    region_SA2 <- SA2
    region_SA4 <- subset(SA_corr, SA2==region_SA2)$SA4 
    region_dat <- subset(work_probs, SA4==region_SA4)
    
    # Assign workplace IDs and labour force status info to each person of working age:
    houses$workplace_id <- 0
    houses$lf_status <- 0
    houses$lf_industry <- 0
    houses$lf_sector <- 0
    
    for (i in 1:length(houses[,5])) {
      if(houses$Age[i] > 15 & houses$Age[i] < 71) { # If potential member of labour force
        
        # Sample a labour force status, sector, industry:
        ind <- as.numeric(sum(runif(1,0,1) > region_dat$cum_prob) + 1) 
        cat <- region_dat[ind,]
        houses$lf_status[i] <- cat$status 
        houses$lf_industry[i] <- cat$industry
        houses$lf_sector[i] <- cat$sector
        
        # Assign workplace ID and other lf info to the individual:
        if (cat$status == "not_in") {
          houses$workplace_id[i] <- NA
          houses$lf_industry[i] <- NA
          houses$lf_sector[i] <- NA
        } else if (cat$status != "not_in" & cat$sector=="public") {
          houses$workplace_id[i] <- sample(public_id_list[[region_SA4]], 1)
        } else if (cat$status != "not_in" & cat$sector=="private") {
          lab <- paste(cat$SA4, cat$industry, sep="_") # Get the SA4_industry label to pull item out of list
          houses$workplace_id[i] <- sample(business_id_list[[lab]], 1)
        }
        
      } else{
        houses$workplace_id[i] <- NA
        houses$lf_status[i] <- "not_in"
        houses$lf_industry[i] <- NA
        houses$lf_sector[i] <- NA
      }
      
    }
    
    houses$SA2 <- SA2
    houses$Household_id <- as.character(as.numeric(houses$Household_id) + counter_house_ids)
    counter_house_ids <- max(as.numeric(houses$Household_id))
    store_SA2s[[loc]] <- houses # Store SA2's households as list element
  }
  
  households_data <- bind_rows(store_SA2s, .id="column_label") # Concatenate list elements into data frame
  households_data <- merge(households_data, SA_corr, by="SA2", all.x=TRUE) # Merge on the other geocodes
  households_data$person_id <- c(1:length(households_data[,1])) # Create person ID
  
  # Keep only relevant variables:
  households_data <- households_data %>% select(person_id, Type, Age, Household_id, school_id, workplace_id, lf_status, lf_sector, lf_industry, SA2, SA3, SA4, GCCSA, STATE)
  names(households_data) <- c("person_id", "household_type", "age", "household_id", "school_id", "workplace_id", "lf_status", "lf_sector", "lf_industry", "SA2", "SA3", "SA4", "GCCSA", "STATE")
  
  # Get vectors of inhabitants ages, household IDs, school IDs, and workplace IDs:
  outputs <- list(vector.of.ages.specific.to.region <- households_data$age,
                  vector.of.ig.household.ids.specific.to.region <- households_data$household_id,
                  vector.of.ig.school.ids.specific.to.region <- households_data$school_id,
                  vector.of.ig.workplace.ids.specific.to.region <- households_data$workplace_id,
                  households.data <- households_data)
  
  names(outputs) <- c("age", "household_id", "school_id", "workplace_id", "household_data")
  
  return(outputs)
  
}

