
# This program generates populations for randomly selected SA2s within a region. 
# To set the region, load 'SA_corr.RData' and select the region of interest from 
# those listed. The region may be SA2, SA3, SA4, GCCSA, State or all of Aus. 

load("SA_corr.RData")

# Set params:
seed <- 123
n <- 2 # Number of SA2s to randomly sample
regionLevel <- 'SA2'

set.seed(seed)

# Randomly sample n SA2s from a specified region:
region <- subset(SA_corr, GCCSA=='Greater Sydney') # Set region from which to sample SA2s
n <- min(n, length(unique(region$SA2)))
SA2s_list <- sample(unique(SA_corr$SA2), n) # Randomly sample the SA2s

region.names <- SA2s_list
remaining.pop <- total.pop <- 100000
current.index <- 1
index <- 0
results <- list(household_id = NULL, school_id = NULL, workplace_id = NULL, uni_id = NULL, region_id = NULL, region_label = NULL)

cat('', file='failed-regions.txt', append=FALSE)

for (i in 1:length(region.names)) {
  
  region.name <- SA2s_list[i]
  
  cat(paste('doing:', region.name, '...'))
  
  ids <- try(age.and.id.generator.generator(seed, regionLevel, region.name))
  if(class(ids) == 'try-error') {
    cat(paste(region.name, '\n'), file='failed-regions.txt', append=TRUE)
    cat(paste('failed:', region.name, '\n'))
    next
  }
  
  ids$household_id <- as.numeric(ids$household_id)
  
  num_labels <- length(ids$household_id)
  
  if(i == 1) {
    results$age <- ids$age
    results$household_id <- ids$household_id
    results$workplace_id <- ids$workplace_id
    results$school_id <- ids$school_id
    results$uni_id <- ids$uni_id
    results$region_id <- rep(i, length(ids$household_id))
    results$region_label <- rep(region.name, length(ids$household_id))
    
  } else {
    results$age <- c(results$age, ids$age)
    results$household_id <- c(results$household_id, as.numeric(ids$household_id) + max(unique(results$household_id), na.rm=TRUE) + 1)
    results$workplace_id <- c(results$workplace_id, ids$workplace_id)
    results$school_id <- c(results$school_id, ids$school_id)
    results$uni_id <- c(results$uni_id, ids$uni_id)
    results$region_id <- c(results$region_id, rep(i, length(ids$age)))
    results$region_label <- c(results$region_label, rep(region.name, length(ids$age)))
    
  }
  
  cat(paste('remaining:', remaining.pop, '\n'))
  
}

results$household_id <- ifelse(is.na(results$household_id), -1, results$household_id)
results$workplace_id <- ifelse(is.na(results$workplace_id), -1, results$workplace_id)
results$school_id <- ifelse(is.na(results$school_id), -1, results$school_id)
results$uni_id <- ifelse(is.na(results$uni_id), -1, results$uni_id)
results$region_id <- ifelse(is.na(results$region_id), -1, results$region_id)
results$region_label <- ifelse(is.na(results$region_label), -1, results$region_label)

ids <- results

