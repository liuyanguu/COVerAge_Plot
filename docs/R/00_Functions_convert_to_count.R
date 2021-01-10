# borrowed step 1 from covid_age/R/00_Functions.R
# https://github.com/timriffe/covid_age/tree/master/R
# https://timriffe.github.io/covid_age/DataSteps.html
# If Metric is "Fraction", distribute totals to produce counts by age and/or sex
convert_to_count <- function(inputDB){
  require(dplyr, quietly = TRUE)
  inputDB %>% 
    filter(!(Age == "TOT" & Metric == "Fraction"),
           !(Age == "UNK" & Value == 0),
           !(Sex == "UNK" & Sex == 0)) %>% 
    mutate(AgeInt = as.integer(AgeInt))
  
  A <-
    inputDB %>% 
    filter(!(Age == "TOT" & Metric == "Fraction"),
           !(Age == "UNK" & Value == 0),
           !(Sex == "UNK" & Sex == 0)) %>% 
    group_by(Code, Measure) %>%
    # do_we_convert_fractions_all_sexes(chunk)
    do(convert_fractions_all_sexes(chunk = .data)) %>% 
    ungroup() %>% 
    group_by(Code, Sex, Measure) %>% 
    # do_we_convert_fractions_within_sex(chunk)
    do(convert_fractions_within_sex(chunk = .data))  
  return(A)
}

# TODO: write validation functions
# group_by(Code, Measure)
do_we_convert_fractions_all_sexes <- function(chunk){
  Fracs <-  chunk %>% pull(Metric) %>% '=='("Fraction") %>% sum()
  
  maybe <- Fracs > 0 
  if (maybe){
    Fracs      <- chunk %>% filter(Metric == "Fraction")
    have_sexes <- all(c("m","f") %in% Fracs$Sex)
    
    # Don't need explicit TOT b, Counts by age in b is enough
    yes_b_scalar <- chunk %>% 
      filter(Metric == "Count",
             Sex == "b") %>% 
      nrow() %>% 
      '>'(0)
    
    no_sex_scalars <- chunk %>% 
      filter(Sex %in% c("m","f")) %>% 
      pull(Metric) %>% 
      '=='("Count") %>% 
      sum() %>% 
      "=="(0)
    
    out <- have_sexes & yes_b_scalar & no_sex_scalars
  } else{
    out <- FALSE
  }
  out
}

convert_fractions_all_sexes <- function(chunk){
  do_this <- do_we_convert_fractions_all_sexes(chunk)
  if (!do_this){
    return(chunk)
  }
  
  # this might suggest a better way to check whether
  # to do this transformation
  b    <- chunk %>% filter(Sex == "b")
  rest <- chunk %>% filter(Sex != "b")
  
  # TR: this is a hard check to make sure the checker function
  # does the right thing
  stopifnot(all(rest$Metric == "Fraction"))
  
  # Console message
  cat("Fractions converted to Counts for",unique(chunk$Code),"\n")
  if (any(b$Age == "TOT")){
    BB <- b %>% filter(Age == "TOT") %>% pull(Value)
  } else {
    BB <- b %>% pull(Value) %>% sum()
  }
  
  out <-
    rest %>% 
    mutate(Value = Value * BB,
           Metric = "Count") %>% 
    bind_rows(b)
  
  out
}




# 1) convert fraction. Should be on 
# group_by(Code, Sex, Measure)

do_we_convert_fractions_within_sex <- function(chunk){
  have_fracs <- "Fraction" %in% chunk$Metric 
  scaleable  <- chunk %>% 
    filter(Metric == "Count",
           Age == "TOT")
  (nrow(scaleable) == 1) & have_fracs
}

convert_fractions_within_sex <- function(chunk){
  # subset should contain only Fractions and one Total Count
  
  do.this <- do_we_convert_fractions_within_sex(chunk)
  if (!do.this){
    return(chunk)
  }
  
  TOT <- chunk %>% 
    filter(Metric == "Count")
  
  stopifnot(TOT$Age == "TOT")
  # Console message
  cat("Fractions converted to Counts for",unique(chunk$Code),"\n")
  
  TOT <- TOT %>% pull(Value)
  
  out <- chunk %>% 
    filter(Metric == "Fraction") %>% 
    mutate(Value = Value / sum(Value),
           Value = Value * TOT,
           Metric = "Count")
  out
}