
# Function returns a character vector
#  with the name of the hospital that has the best (i.e. lowest) 30-day
# mortality for the specified outcome
#  in that state. The hospital name is the name provided in the Hospital.Name variable. 
# The outcomes can
#  be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have 
# data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

bestOrg <- function(state, outcome) {

  outcome = gsub('\\s+', '', outcome)
  print(outcome)
  outcomeList <- c("heartattack" , "heartfailure" ,"pneumonia")
  if (!(outcome %in% outcomeList))
  {
    stop("invalid outcome")
  }
  caremeasure <- read.csv("week3/outcome-of-care-measures.csv" ,colClasses = "character")
  states <- unique(caremeasure$State)
  if (!(state %in% states))
  {
    stop("invalid state")
  }
  names(caremeasure)[11] <- "heartattack"
  names(caremeasure)[17] <- "heartfailure"
  names(caremeasure)[23] <- "pneumonia"
  colNum <- 0
  if(outcome == "heartattack")
  {
    colNum <-11      
  }
  else if (outcome == "heartfailure")
  {
    colNum <-17
  }
  else if(outcome == "pneumonia")
  {
    colNum <- 23
  }  
  stateList <- caremeasure[which(caremeasure$State == state) , ]
  outComeFiltered <- as.numeric(stateList[ ,outcome])
  stateList[ ,outcome] <- outComeFiltered
  stateList <- subset(stateList, !is.na(stateList[outcome]))
  noOfHospitals = dim(stateList)[1]
  
  #https://www.statmethods.net/management/sorting.html
  #https://stackoverflow.com/questions/9156197/why-is-order-only-returning-first-row
  sStateList <- stateList[order(stateList[outcome],stateList["Hospital.Name"]),]
  print(sStateList[1:2 , c(2, 7 ,colNum) ])
  sStateList[1 , 2]
}

best <- function(state, outcome) {
  return(commonRanker(state , outcome))
}    

rankhospital <- function(state, outcome, num = "best") {
  return(commonRanker(state , outcome , num))
}

rankall <- function(outcome, num = "best") {
 
  outcome = gsub('\\s+', '', outcome)
  outcomeList <- c("heartattack" , "heartfailure" ,"pneumonia")
  if (!(outcome %in% outcomeList))
  {
    stop("invalid outcome")
  }
  rankedByState <- NULL
  rankedByState <- data.frame(state = character() , hospital = character())
  caremeasure <- read.csv("week3/outcome-of-care-measures.csv" ,colClasses = "character")
  states <- unique(caremeasure$State)
  stateList <- NULL
  for (stateName in states) {
    rankedHospitalInState <- commonRanker(stateName , outcome , num)
    stateList <- rbind(stateList, list(state = stateName ,hospital=rankedHospitalInState))
  }
  rankedByState <- rbind(rankedByState, stateList)
  return (rankedByState)
}

commonRanker <- function(state, outcome , num) {
  if(missing(num)){
    num <- 1
  }
  # print(class(num))
  outcome = gsub('\\s+', '', outcome)
  outcomeList <- c("heartattack" , "heartfailure" ,"pneumonia")
  if (!(outcome %in% outcomeList))
  {
    stop("invalid outcome")
  }
  caremeasure <- read.csv("week3/outcome-of-care-measures.csv" ,colClasses = "character")
  states <- unique(caremeasure$State)
  if (!(state %in% states))
  {
    stop("invalid state")
  }
  names(caremeasure)[11] <- "heartattack"
  names(caremeasure)[17] <- "heartfailure"
  names(caremeasure)[23] <- "pneumonia"
  colNum <- 0
  if(outcome == "heartattack")
  {
    colNum <-11      
  }
  else if (outcome == "heartfailure")
  {
    colNum <-17
  }
  else if(outcome == "pneumonia")
  {
    colNum <- 23
  }  
  stateList <- caremeasure[which(caremeasure$State == state) , ]
  outComeFiltered <- as.numeric(stateList[ ,outcome])
  stateList[ ,outcome] <- outComeFiltered
  stateList <- subset(stateList, !is.na(stateList[outcome]))
  noOfHospitals = dim(stateList)[1]
  returnRank <- 1
  invalidRank <- NA
  if(num=="best"){
    returnRank <- 1
  }
  else if(num=="worst")
  {
    returnRank <- noOfHospitals
  }
  else if(is.numeric(num))
  {
    num <- as.numeric(num)
      if(num > noOfHospitals)
      {
        print("Returning Invalid Rank.....")
        return(invalidRank);
      }
      returnRank <- num
     
  }
  #https://www.statmethods.net/management/sorting.html
  #https://stackoverflow.com/questions/9156197/why-is-order-only-returning-first-row
  sStateList <- stateList[order(stateList[outcome],stateList["Hospital.Name"]),]
  # print(sStateList[1:2 , c(2, 7 ,colNum) ])
  sStateList[returnRank , 2]
}

# outcome <- read.csv("week3/outcome-of-care-measures.csv", colClasses = "character")
#print(colnames(outcome))
#outcome[, 11] <- as.numeric(outcome[, 11])
#hg <- hist(outcome[, 11])
# print(best("TX" , "heart attack"))
# print(bestHospital)
# bestHospital <- best("SC", "heart attack")
# print(bestHospital)
# bestHospital <- best("DE", "heart attack")
# print(bestHospital)
# 
# print(rankhospital("DE", "heart attack" , "best"))
# print(rankhospital("DE", "heart attack" , "worst"))
# print(commonRanker("DE", "heart attack" , 3))
# print(commonRanker("DE", "heart attack" , 5000))
# print(commonRanker("DE", "heart attack" ))

# print(rankall("heart attack" , 3))
# print(rankall("heart attack" , 5000))
# print(rankall("heart attack" , "worst"))
# print(loopTest())
print("------------------ Question 1 ----------------")
print(best("SC", "heart attack"))
print("------------------ Question 2 ----------------")
print(best("NY", "pneumonia"))
print("------------------ Question 3 ----------------")
print(best("AK", "pneumonia"))
print("------------------ Question 4 ----------------")
print(rankhospital("NC", "heart attack", "worst"))
print("------------------ Question 5----------------")
print(rankhospital("WA", "heart attack", 7))
print("------------------ Question 6----------------")
print(rankhospital("TX", "pneumonia", 10))
print("------------------ Question 7----------------")
print(rankhospital("NY", "heart attack", 7))
print("------------------ Question 8----------------")
r <- rankall("heart attack", 4)
print(as.character(subset(r, state == "HI")$hospital))
print("------------------ Question 9----------------")
r <- rankall("pneumonia", "worst")
print(as.character(subset(r, state == "NJ")$hospital))
print("------------------ Question 10----------------")
r <- rankall("heart failure", 10)
print(as.character(subset(r, state == "NV")$hospital))

