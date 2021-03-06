##check if the two required packages are installed and then load them
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}
if(!require(stringi)){
  install.packages("stringi")
  library(stringi)
}


#set working directory
setwd("F:\\intern\\Result")

#this function is to extract information from MIPLIB2010 test engine
#filename: name of the .out file
#solver: "cbc", "cplex", "mipcl"
#summaryfile: name of the .res file
get.info <- function(filename, solver, summaryfile)
{
  if(solver == "cbc"){
    cbc.summary <- read.csv(summaryfile)
    test <- readLines(filename)
    result.pro <- grep("Problem",test, value=TRUE )
    result.cons <- as.numeric(str_extract(result.pro, pattern = "(?<=has).*(?=rows)"))
    result.cons <- result.cons[!is.na(result.cons)]
    result.vars <- as.numeric(str_extract(result.pro, pattern = "(?<=,).*(?=columns)"))
    result.vars <- result.vars[!is.na(result.vars)]
    result.mode <- grep("Result -", test, value = TRUE)
    result.solved.obj <- test[which(test %in% result.mode)+2] #include:1)infeasible solution proven 2)no feasible solution found in 7200s
    result.solved.obj <- as.numeric(str_extract(result.solved.obj, pattern = "(?<=value:).*"))
    #mode.optimal <- grep("Optimal solution found",test, value=TRUE)  #61
    #model.infe <- grep("Problem proven infeasible", test, value =  TRUE)  #2
    #mode.timeout <- grep("Result - Stopped on time limit",test, value=TRUE) #22
    mode.error <- grep("Aborted", test, value = TRUE) #2
    error.next <- test[which(test %in% mode.error)+11]
    error.next <- str_extract(error.next, pattern = "(?<=miplib2010/).*(?=.mps)")
    error.name <- as.character(cbc.summary$Name[which(cbc.summary$Name %in% error.next)-1])
    result.obj <- rep(0, 87)
    result.obj[which(cbc.summary$Name %in% error.next)-1] <- "aborted" 
    result.obj[which(result.obj == 0)] <- result.solved.obj
    result.df <- data.frame(Name = cbc.summary$Name, Constraints = result.cons, Variables = result.vars, cbc_obj = result.obj, cbc_time = cbc.summary$Time, cbc_status = cbc.summary$Status)
    
  }
  if(solver == "cplex"){
    cplex.summary <- read.csv(summaryfile)
    test <- readLines(filename)
    result.mode <- grep("CPLEX> MIP", test, value = TRUE)
    result.infe <- grep("Integer infeasible", result.mode, value = TRUE) #3
    result.optimal <- grep("Integer optimal", result.mode, value = TRUE) #83
    result.timeout <- grep("Time limit exceeded", result.mode, value = TRUE) #1
    result.obj <- as.numeric(str_extract(result.mode, pattern = "(?<=Objective = ).*"))
    result.df <- data.frame(Name = cplex.summary$Name, cplex_obj = result.obj, cplex_time = cplex.summary$Time, cplex_status = cplex.summary$Status)
  }
  if(solver == "mipcl"){
    test <- readLines(filename)
    result.name <- grep("NAME", test, value = TRUE)
    result.name <- str_extract(result.name, pattern = "(?<=NAME           ).*")
    result.time <- grep("Solution time", test, value = TRUE)
    result.time <- as.numeric(str_extract(result.time, pattern = "(?<=Solution time:).*"))
    result.mode <- test[which(test %in% grep("Solution time", test, value = TRUE))+2]
    result.optimal <- grep("optimality proven", result.mode, value = TRUE) #67
    result.infe <- grep("This problem is infeasible", result.mode, value = TRUE) #3
    result.timeout <- grep("Time limit reached", result.mode, value = TRUE) #17
    result.obj <- as.numeric(str_extract(result.mode, pattern = "(?<=Objective value:).*(?= - optimality)"))
    result.df <- data.frame(Name = result.name, mipcl_obj = result.obj, mipcl_time = result.time)
    result.df$mipcl_status[result.df$mipcl_time < 7199] <- "ok"
    result.df$mipcl_status[result.df$mipcl_time > 7199] <- "stopped"
    
  }
  return(result.df)
  
}

cbc.res4 <- get.info("cbc_results_4t.txt", "cbc", "cbc_summary_4t.csv")
cbc.res8 <- get.info("cbc_results_8t.txt", "cbc", "cbc_summary_8t.csv")
cplex.res <- get.info("cplex_results.txt", "cplex", "cplex_summary.csv")
mipcl.res <- get.info("mipcl_win_solved2.txt", "mipcl")