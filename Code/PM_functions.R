#Definition of functions for Phishy Mailbox Analysis

#loading and installing required packages.
req_init <- function(path=NULL){
  if(is.null(path)==T){
    stop()
    warning("You have not specified a working directory. Specify the path where your script and data are located on your computer. Windows documents parent folder is the default.")
  }
  #define dependencies
  pckg <- c("tidyverse", "ltm", "eRm", "difR", "psych", "sjmisc")
  
  #set path
  setwd(path)
  
  #check for dependencies in installed packages, install if not found, then execute.
  for(i in 1:length(pckg)){
    if(pckg[i] %in% installed.packages()){
      library(pckg[i], character.only = T)
    }else{
      install.packages(pckg[i])
      library(pckg[i], character.only = T)
    }
  }
}

#importing all data
import_data <- function(data= NULL){
  #Initial input check
  if(is.null(data)==T){
    stop()
    warning("No data was given as input. Make sure you give (a) sheet(s) as input.")
  }
  #importing and joining raw data.
  output <- data.frame()
  for (i in 1:length(data)){
    temp <- read_delim(data[i], delim= ",",escape_backslash = T)
    colnames(temp) <- c("code", "type", "at", "email_ID", "email_backoffice_ID", "event_ID", "from_folder", "to_folder", "scrolled_to", "url", "link_text")
    output <- rbind(output, temp)
  }
  return(output)
}

#Importing the codebook
import_codebook <- function(codebook = NULL){
  #input check
  if(is.null(codebook)==T){
    stop()
    warning("No codebook was given as input. Make sure you give a sheet as input here.")
  }
  #main code
  cb_frame <- read_csv(codebook)
  cb_frame <- cb_frame[,2:3]
  colnames(cb_frame) <- c("email", "pe")
  return(cb_frame)
}

#processing and filtering
process_data <- function(data = NULL){
  #input check
  if(is.null(data)==T){
    stop()
    warning("No data was given as input. Make sure you specify an input object.")
  }
  #change a variable that was given twice by mistake
  replacement <- grepl(" 13:", data$at) & data$code=="24"
  data$code[replacement] <- "25"
  
  #set up a filter that tells us which data sets are finished.
  f <- data |> filter(type=="finished") |> dplyr::select(code) |> as.vector()
  #filter finished datasets, drop NAs, select email moving data.
  output <- data[which(data$code %in% f$code),]|>  filter(type=="email-moved")
  #cut out: |> drop_na(to_folder)
  return(output)
}
add_vars <- function(data= NULL, codebook=NULL){
  if(is.null(data)==T){
    stop()
    warning("No data was given as input. Make sure you specify an input object.")
  }
  if(is.null(codebook)==T){
    stop()
    warning("No codebook was given as input. Make sure you specify an input object.")
  }
  #add variable that tells us if the given email was real or phishing
  output <- data |> rowwise() |> mutate(realOrPhish = as.character(codebook[which(codebook$email==email_backoffice_ID),"pe"]))
  
  #add a variable that tells us whether the person solved the individual email correctly.
  output <- output |> rowwise() |> mutate(solved= ifelse(realOrPhish=="P" & to_folder=="Phishing/Spam",1,ifelse(realOrPhish=="E" & to_folder=="No Phishing",1,0)))
  return(output)
}
model_ready <- function(data=NULL) {
  if(is.null(data)==T){
    stop()
    warning("No data was given as input. Make sure you specify an input object.")
  }
  #isolate phishing emails
  temp <- data |> filter(realOrPhish=="P")
  
  #isolate final mail categorization by dropping previous moving of the same email.
  joiner <- which(lag(temp$to_folder)==temp$from_folder)
  temp <- temp[-(joiner-1),]
  
  #remove duplicates that somehow are in there using a join.
  dups <- temp |> group_by(code) |> filter(duplicated(email_backoffice_ID))
  temp <-  temp |> anti_join(dups)
  
  #transform data to wide format.
  vars <- c("code", "email_backoffice_ID", "solved", "event_ID")
  temp <- temp|> dplyr::select(all_of(vars)) |> pivot_wider(id_cols = code, names_from = email_backoffice_ID, values_from = solved)
  
  #select only emails without NA values
  temp <- temp |> select_if(~!any(is.na(.)))
  output <- temp[,-1]
  cb <- data.frame(email_name = colnames(output), 
                   short_code = c(1:length(colnames(output))))
  write.csv(cb, "codebook_RaschModel.csv")
  print("A file called 'codebook_RaschModel.csv' has been saved to your working directory. It includes all emails with their assigned numerical codes. Some emails were cut out due to missing responses.")
  colnames(output) <- c(1:length(colnames(temp)))
  return(output)
}
#Modeling functions
model_rasch <- function(data){
  fit_rm <- RM(data)
  print("The following is a summary of a CML Rasch Model:")
  summary(fit_rm)
  ##difficulty parameters
  betas <- -coef(fit_rm)
  print("The following are the sorted difficulty parameters. They have been saved to your working directory as csv.")
  dp <- round(sort(betas),2)
  print(round(sort(betas),2))
  write.csv(dp, "difficulty_params_RaschModel.csv")
  return(fit_rm)
}
model_mml <- function(data){
  #MML Model
  fit_mml <- rasch(data)
  #summary
  print("The following is a summary of a MML rasch Model:")
  print(summary(fit_mml))
  return(fit_mml)
}
model_2pl <- function(data){
  #2PL Model
  fit_2pl <- ltm(data~z1)
  print("The following is a summary of a 2PL Model:")
  print(summary(fit_2pl))
  return(fit_2pl)
}
fscores_fit <- function(modelr, modelm, data){
  #infit and outfit
  print("The following shows the item infit and outfit statistics. Infit t around 0.9 or -0.9 is very good, anything within -1 and 1 is good.")
  modelr |> person.parameter() |> itemfit() |> print()
  #person parameters
  print("The following shows the estimated ability of every participant to detect phishing emails. Higher means better ability. Zero means average.")
  modelr |> person.parameter() |> coef() |> print()
  
  #expected posteriori (EAP) and empirical bayes (EB) estimations of factor scores.
  print("The following is a robustness check of factor scores (i.e. people's ability to detect phishing) regarding estimation algorithms. We use EB (Empirical Bayes) EAP(Expected a Posteriori) and correlate them with Maximum Likelihood Estimation (ML, here called Rasch). High correlation means the estimates are robust against change of estimator.")
  eb_fs <- factor.scores.rasch(modelm, method = "EB", resp.patterns = data)
  eap_fs <- factor.scores.rasch(modelm, method = "EAP", resp.patterns = data)
  tmp2 <- data.frame(Rasch = coef(person.parameter(modelr)), 
                     MAP = eb_fs$score.dat$z1, 
                     EAP = eap_fs$score.dat$z1)
  tmp2 <- round(cor(tmp2), 4)
  print(tmp2)
  return(tmp2)
}

plot_all <- function(model){
  for(i in 1:nrow({{model}}$X)){
    png(paste0("ICCplot_Email_",i,"_cml.png"), width = 866, height = 598, units = "px")
    plotICC(model, item.subset= i, cex=.6, main=paste0("ICC plot for Email ",i))
    abline(v=0, h=0.5, col = "grey")
    dev.off()
  }
  png("All_ICCs_Emails.png", width = 866, height = 598, units = "px")
  plotjointICC(fit_rm, cex=.6, legend=F, main = "ICC plots for all Emails")
  abline(v=0, h=0.5, col = "grey")
  dev.off()
  png("PIMap_cml.png", width = 866, height = 598, units = "px")
  plotPImap(model, cex.gen = .55, sorted = TRUE)
  dev.off()
  png("Pathway_Map_InfitCML.png", width = 866, height = 598, units = "px")
  plotPWmap(model)
  dev.off()
  print("All plots have been saved as png files to your working directory")
}
