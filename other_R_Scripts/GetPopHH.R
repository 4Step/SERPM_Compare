# R script to aggregate person and hh info at TAZ / MGRA Level
# Input: hh, pop and emp
# Output: Zero auto, hh by inc and per by age

library(dplyr)
library(tidyr)

  # user settings
  path <- "C:\\projects\\extract_SERPM_SEData" 
  in_dir <- "corr_2015R\\in"
  out_dir <- "corr_2015R\\out"

  # output
  outfile <- "2015_model_data.csv"  
    
  # Model Data
  hhInput  <- paste0(in_dir,"\\ctramp\\hhFile.csv")
  empInput <- paste0(in_dir,"\\ctramp\\maz_data.csv")
	hhFile   <- paste0(out_dir,"\\ctramp\\householdData_1.csv")
	perFile  <- paste0(out_dir,"\\ctramp\\personData_1.csv")
	
  ageBins <- c(-1, 17, 24, 34, 49, 64, 79, 120) 
	incBins <- c(-30000, 0, 25000, 50000, 75000, 100000, 1500001)
	  
	setwd(path)
    
	# read data
	hhinput <- read.csv(hhInput)
	emp <- read.csv(empInput)
	#hh_pop <- read.csv(hh_popFile)
	hh <- read.csv(hhFile)
	per <- read.csv(perFile)
	
	# Keep data from hhinput
	hhinput <- hhinput %>% select(HHID, TAZ)
	
  # recode age to in bins
	per <- per %>%
	  mutate(ageBin = cut(age, ageBins)) %>%
	  mutate(ageBin = paste(
	    as.numeric(sub("\\((.+),.*", "\\1", ageBin )),
	    as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", ageBin )),
	    sep=" to "))
		
	pop_by_age <- per %>% 
	     mutate(count = 1) %>%
	     select(hh_id, person_id, ageBin, count) %>%
	     spread(ageBin, count) %>%
	     mutate_each(funs(replace(.,is.na(.),0))) %>%
	     left_join(hhinput, by = c("hh_id" = "HHID")) %>%
	     group_by(TAZ) %>% 
	     summarise_each(funs(sum)) %>%
	     select(-hh_id, -person_id)
	
	# Get zero autos by taz
	zero_auto <- hh %>% 
       left_join(hhinput, by = c("hh_id" = "HHID")) %>%
       filter(autos == 0) %>%
       group_by(TAZ) %>% 
	     summarise(zero_auto = n())
	
	# Get pop, hh and emp
	hpe <- emp %>%
       group_by(TAZ) %>%
       summarise(hh = sum(hh),
                 pop = sum(pop),
                 emp = sum(emp_total)) %>%
       select(TAZ, hh, pop, emp)
	
	hpea <- hpe %>% left_join(zero_auto, by = "TAZ")
	
	hpea_age <- hpea %>% left_join(pop_by_age, by = "TAZ")
	 
	
	# recode income to inc bins
	hh <- hh %>%
	   mutate(incBin = cut(income, incBins)) %>%
	   mutate(incBin = paste(
	      as.numeric(sub("\\((.+),.*", "\\1", incBin )),
	      as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", incBin )),
	      sep=" to "))
	
	# compute hh by inc bins & TAZ
	hh_by_inc <- hh %>% 
	  mutate(count = 1) %>%
	  select(hh_id, incBin, count) %>%
	  spread(incBin, count) %>%
	  mutate_each(funs(replace(.,is.na(.),0))) %>%
	  left_join(hhinput, by = c("hh_id" = "HHID")) %>%
	  group_by(TAZ) %>% 
	  summarise_each(funs(sum)) %>%
	  select(-hh_id)
	
	
	# Add age and inc 
	hpea_age_inc <- hpea_age %>% 
	  left_join(hh_by_inc, by = "TAZ") %>%
	  mutate_each(funs(replace(.,is.na(.),0)))

	# colnames
	ageFields <- c("AGE0TO17",	"AGE18TO24",	"AGE25TO34",	"AGE35TO49",	"AGE50TO64",	"AGE65TO79",	"AGE80PLUS")
	incFields <- c("INCOME_NEG","INCOME_25K",	"INCOME_50K", 	"INCOME_75K",	"INCOME_100K", "INCOME_100KPLUS")
	fNames <- c("TAZ",	"hh",	"pop",	"emp",	"zero_auto", ageFields, incFields)
	            
	colnames(hpea_age_inc) <- fNames
	hpea_age_inc <- hpea_age_inc %>% select(- INCOME_NEG)	
	
	# Scale inc fields and age fields to account for total hhs and persons
	hpea_age_inc <-  hpea_age_inc %>% mutate(hh_inc = INCOME_25K +	INCOME_50K +	INCOME_75K +	INCOME_100K + 
                                              INCOME_100KPLUS,
	                                   scale_hh = hh / hh_inc )
	
	hpea_age_inc <- hpea_age_inc %>% mutate(INCOME_25K = INCOME_25K * scale_hh,
	                                         INCOME_50K = INCOME_50K * scale_hh,
	                                         INCOME_75K = INCOME_75K * scale_hh,
	                                         INCOME_100K = INCOME_100K * scale_hh,
	                                         INCOME_100KPLUS = hh - (INCOME_25K + 
	                                                                 INCOME_50K +
	                                                                 INCOME_75K + 
	                                                                 INCOME_100K )
	                                        )
	
	hpea_age_inc  <-  hpea_age_inc %>% mutate(per_age = (AGE0TO17 +	AGE18TO24 +	AGE25TO34 +	AGE35TO49 +	
	                                                    AGE50TO64 +	AGE65TO79 +	AGE80PLUS),
	                                          scale_per = pop / per_age)
	
	hpea_age_inc <- hpea_age_inc %>% mutate(AGE0TO17 = AGE0TO17 * scale_per,
	                                        AGE18TO24 = AGE18TO24 * scale_per,
	                                        AGE25TO34 = AGE25TO34 * scale_per,
	                                        AGE35TO49 = AGE35TO49 * scale_per,
	                                        AGE50TO64 = AGE50TO64 * scale_per,
	                                        AGE65TO79 = AGE65TO79 * scale_per,
	                                        AGE80PLUS = pop - (AGE0TO17 + 
	                                                             AGE18TO24 +
	                                                             AGE25TO34 + 
	                                                             AGE35TO49 +
	                                                             AGE50TO64 +
	                                                             AGE65TO79 )
	                                        )
	hpea_age_inc <- hpea_age_inc %>% select (-per_age, -scale_per, -scale_hh, -hh_inc)
		                                                               
	#Write output
	write.csv(hpea_age_inc, outfile)
	

	
	
	
  