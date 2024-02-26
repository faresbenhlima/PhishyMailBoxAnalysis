#Analysis Phishy Mailbox

#set path (where your data and functions script is located)
path <- '~/'

#initialize session
req_init(path)

#load functions to analyze
source("PM_functions.R")

#load data and email classification
dat <- import_data(data = c("export Rechner1.csv","export Rechner2.csv", "export Rechner3.csv"))
cb <- import_codebook(codebook = "unique_emails2.csv")

#processing data
dat <- process_data(dat)
dat <- add_vars(data = dat, codebook = cb)

#transform data into wide format, make it ready for models.
rmtab <- model_ready(data = dat)

#construct models
fit_rm <- model_rasch(rmtab)
fit_mml <- model_mml(rmtab)
fit_2pl <- model_2pl(rmtab)

cat("We will now compare the constructed models (CML, MML and 2PL).\n ")
anova(fit_mml, fit_2pl) #LRT test not significant --> scrap the 2PL model. 
print("This Likelihood-Ratio test (Andersen, 1973) is not significant (p>.05), which means that both models perform about the same for the data. Therefore, we can use the more restrictive model, which is the model we wanted to use anyway (Rasch Model, CML).\n ")
cor(coef(fit_mml)[,1], -coef(fit_rm))
print("Coefficients between MML and CML Rasch Models correlate highly, which makes them interchangeable. We will use the CML model, as this is what literature recommends; see here: https://escholarship.org/content/qt6m03p281/qt6m03p281.pdf \n")
fscores_df <- fscores_fit(fit_rm, fit_mml, data = rmtab)

#plot everything relevant
plot_all(model = fit_rm)
