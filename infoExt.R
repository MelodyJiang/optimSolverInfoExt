##This is used to extract information form the output of several open-source optimization solvers

#install.packages("stringr")
#install.packages("stringi")
library(stringr)
library(stringi)

####get the name list of the .mps files (in a data frame)####
getNameList <- function(mypath, filename)
{
  setwd(mypath)
  namelist <- read.table(file = filename, col.names = "Name")
  namelist$Name <- gsub('.{4}$', '', namelist$Name)
  return(namelist)
}

lpList <- getNameList("F:\\testproblems\\test1\\MPS", "lp_namelist.txt")
mipList <- getNameList("F:\\testproblems\\miplib2010_51", "mip_namelist.txt")

setwd("F:\\Result")

####This function is for extracting information from .txt file####
#argurments: .txt file name, solver name, problem type (lp or mip)

ext_info <- function(test, solver, problemType)
  
{
  test <- readLines(test)
  
  if(solver == "clp" & problemType %in% c("lp", "mip")){
    
    result.name <- grep("NAME",test, value=TRUE )
    result.name <- str_extract(result.name, pattern = ("(?<=NAME).*"))
    result.pro <- grep("rows",test, value=TRUE )
    result.pro <- grep("Problem",result.pro, value=TRUE )
    result.cons <- as.numeric(str_extract(result.pro, pattern = "(?<=has).*(?=rows)"))
    result.vars <- as.numeric(str_extract(result.pro, pattern = "(?<=,).*(?=columns)"))
    result.info <- grep("time",test, value=TRUE)
    result.obj <- as.numeric(str_extract(result.info, pattern = ("(?<=objective).*(?=-)")))
    result.time <- str_extract(result.info, pattern = ("(?<=time).*"))
    result.time <- as.numeric(substr(result.time, 1, 6))
    result.df <- data.frame(Name = result.name, Constraints = result.cons, Variables = result.vars, clp_time = result.time,  clp_obj = result.obj, stringsAsFactors = FALSE)
    if(problemType == "lp"){
      result.df$Name <- lpList$Name
      result.df <- merge(lpList, result.df, by = "Name", all.x = TRUE)
    }
    if(problemType == "mip"){
      result.df 
    }
    
  }
  
  if(solver == "cplex" & problemType %in% c("lp", "mip")){
    
    result.name <- grep("Problem",test, value=TRUE )
    result.info <- grep("time",test, value=TRUE)
    result.info <- grep("Solution",result.info, value=TRUE)
    result.time <- as.numeric(str_extract(result.info, pattern = ("(?<==).*(?=sec)")))
    
    if(problemType == "lp"){
      
      result.name <- str_extract(result.name, pattern = ("(?<=').*(?=')"))
      result.name <- gsub('.{4}$', '', result.name)
      result.obj <- grep("Objective",test, value=TRUE)
      result.obj <- unique(as.numeric(str_extract(result.obj, pattern = ("(?<==).*"))))
    }
    if(problemType == "mip"){
      
      name_index <- which(test %in% result.name)
      result.name <- str_extract(result.name, pattern = ("(?<=').*(?=')"))
      result.name <- gsub('.{4}$', '', result.name)
      result.optimal <- grep("optimal", test, value = TRUE)
      result.obj <- grep("Objective", result.optimal, value = TRUE)
      result.obj <- as.numeric(str_extract(result.obj, pattern = ("(?<==).*")))
      result.infe <- grep("Integer infeasible", test, value = TRUE) 
      infe_index <- which(test %in% result.infe)
      
      if(length(result.infe) > 0){
        for (i in 1:length(name_index)) {
          if(infe_index > name_index[i] & infe_index < name_index[i+1])
            result.obj <- append(result.obj, "INFEASIBLE", after = i-1)
          
        }
        
      }
      
    }
    
    result.df <- data.frame(Name = result.name, cplex_time = result.time, cplex_obj = result.obj, stringsAsFactors = FALSE)
    if(problemType == "lp"){ 
      result.df <- merge(lpList, result.df, by = "Name", all.x = TRUE)
    }
    if(problemType == "mip"){
      result.df <- merge(mipList, result.df, by = "Name", all.x = TRUE)
    }
  }
  
  if(solver == "glpk" & problemType == "lp"){
    
    result.name <- grep("problem",test, value=TRUE )
    result.name <- str_extract(result.name, pattern = ("(?<=').*(?=')"))
    result.name <- gsub('.{4}$', '', result.name)
    result.error <- grep("must", test, value = TRUE)
    result.error <- str_extract(result.error, pattern = (".*(?=.mps)"))
    result.name <- result.name[!result.name %in% result.error]
    result.time <- grep("Time",test, value=TRUE)
    result.time <- as.numeric(str_extract(result.time, pattern = ("(?<=:).*(?=secs)")))
    result.obj <- test[which(test %in% grep("FOUND",test, value=TRUE)) -1]
    result.obj <- as.numeric(str_extract(result.obj, pattern = ("(?<==).*(?=inf)")))
    result.df <- data.frame(Name = result.name, glpk_time = result.time, glpk_obj = result.obj, stringsAsFactors = FALSE)
    result.df <- merge(lpList, result.df, by = "Name", all.x = TRUE)
  }
  if(solver == "glpk" & problemType == "mip"){
    result.name <- grep("problem",test, value=TRUE )
    result.name <- str_extract(result.name, pattern = ("(?<=').*(?=')"))
    result.name <- gsub('.{4}$', '', result.name)
    result.error <- grep("error", test, value = TRUE)
    result.error <- test[which(test %in% result.error)-1]
    result.error <- str_extract(result.error, pattern = ".*(?=.mps)")
    result.name <- result.name[!result.name %in% result.error]
    result.solved <- grep("FOUND", test, value = TRUE)
    result.integer <- grep("INTEGER", result.solved, value = TRUE)
    sol_index <- which(result.solved %in% result.integer)-1
    result.time <- rep(1200, length(result.name))
    solved.time <- as.numeric(str_extract(test[which(test %in% result.integer)+1], pattern = ("(?<=:).*(?=secs)")))
    result.time[sol_index] <- solved.time
    result.obj <- rep("Timeout", length(result.name))
    solved.obj <- test[which(test %in% result.integer)-1]
    result.obj[sol_index] <- as.numeric(str_extract(solved.obj, pattern = "(?<==).*(?=>)"))
    result.df <- data.frame(Name = result.name, glpk_time = result.time, glpk_obj = result.obj, stringsAsFactors = FALSE)
    result.df <- merge(mipList, result.df, by = "Name", all.x = TRUE)
  }
  if(solver == "lpsolve" & problemType == "lp"){
    
    result.num <- grep("Parsing", test, value = TRUE)
    pro_index <- which(test %in% result.num)
    test_sol <- test[pro_index+1]
    test_par <- grep("Parsing", test_sol, value = TRUE)
    result.name <- lpList[-c(which(test_sol %in% test_par)),]
    result.time <- grep("solving", test, value = TRUE)
    result.time <- str_extract(result.time, pattern = ("(?<=:).*(?=s)"))
    result.time <- as.numeric(substr(result.time, 1, 7))
    result.obj <- grep("objective", test, value = TRUE)
    result.obj <- as.numeric(str_extract(result.obj, pattern = ("(?<=:).*")))
    result.df <- data.frame(Name = result.name, lpsolve_time = result.time, lpsolve_obj = result.obj, stringsAsFactors = FALSE)
    result.df <- merge(lpList, result.df, by = "Name", all.x = TRUE)
    result.df$lpsolve_time[is.na(result.df$lpsolve_time)] <- 1200
    result.df$lpsolve_obj[is.na(result.df$lpsolve_obj)] <- "Timeout"
  }
  if(solver == "cbc" & problemType == "mip"){
    result.pro <- grep("Problem",test, value=TRUE )
    #result.name <- grep("jiang",test, value=TRUE )   #problem name, vars, cons
    result.name <- str_extract(result.pro, pattern = ("(?<=Problem).*(?=.has)"))
    result.infe <- grep("infeasible", test, value = TRUE)
    result.infe.name <- test[which(test %in% result.infe)+14]
    result.infe.name <- str_extract(result.infe.name, pattern = "(?<=cbc).*(?=.mps)")
    result.infe.name <- result.name[which(result.name %in% result.infe.name)-2]
    result.infe.time <- test[which(test %in% result.infe)+5]
    result.infe.time <- as.numeric(str_extract(result.infe.time, pattern = "(?<=:).*"))
    result.cons <- as.numeric(str_extract(result.pro, pattern = "(?<=has).*(?=rows)"))
    result.vars <- as.numeric(str_extract(result.pro, pattern = "(?<=,).*(?=columns)"))
    result.optimal <- grep("Optimal solution found",test, value=TRUE)  #solved problems &time
    result.solved <- test[which(test %in% result.optimal)+14]
    result.solved <- str_extract(result.solved, pattern = "(?<=cbc).*(?=.mps)")
    result.solved <- result.name[which(result.name %in% result.solved) - 1]
    result.solved <- c(result.solved, result.infe.name)
    result.time <- test[which(test %in% result.optimal)+5]
    result.time <- as.numeric(str_extract(result.time, pattern = "(?<=:).*"))
    result.time <- c(result.time, result.infe.time)
    result.obj <- test[which(test %in% result.optimal)+2]
    result.obj <- as.numeric(str_extract(result.obj, pattern = "(?<=:).*"))
    result.obj <- c(result.obj, "INFEASIBLE")
    result.info <- data.frame(Name = result.name, Constraints = result.cons, Variables = result.vars, stringsAsFactors = FALSE)
    result.info[26,] <- c("n3seq24", 6044, 11985)
    result.solved.df <- data.frame(Name = result.solved,cbc_time = result.time,  cbc_obj = result.obj, stringsAsFactors = FALSE)
    result.df <- merge(result.info, result.solved.df, by = "Name", all.x = TRUE)
    result.df$cbc_time[is.na(result.df$cbc_time)] <- 1200
    result.df$cbc_obj[is.na(result.df$cbc_obj)] <- "Timeout"
    result.df$Name <- str_replace_all(result.df$Name, pattern=" ", repl="")
    result.df$Constraints <- as.numeric(result.df$Constraints)
    result.df$Variables <- as.numeric(result.df$Variables)
    
    
  }
  
  return(result.df)
}