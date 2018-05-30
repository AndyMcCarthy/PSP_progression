library(grid)
library(gtable)
library(dplyr)



pspnon <- readRDS("PSP_nosubset")
pspnon$sex<- "N/A"
pspnon$age<- 0

pspsexonly <- readRDS("PSP_sex_covariates")
pspsexonly$age<- 0

pspageonly <- readRDS("PSP_age_covariates")
pspageonly$sex<- "N/A"

pspcov <- readRDS("PSP_covariates")
psp <- rbind(pspnon,pspsexonly, pspageonly, pspcov)

psp$X_NAME_ <- factor(psp$X_NAME_, levels=c("Severe impairment","Moderate impairment", "Mild/No impairment"))
psp$Var <- factor(psp$Var, levels=c("Ocular","Gait", "History", "Limb", "Mentation", "Bulbar"))
psp$sex <- factor(psp$sex, levels=c("Female","Male", "N/A"))


cbdnon <- readRDS("CBD_nosubset")
cbdnon$sex<- "N/A"
cbdnon$age<- 0


cbdsexonly <- readRDS("CBD_sex_covariates")
cbdsexonly$age<- 0

cbdageonly <- readRDS("CBD_age_covariates")
cbdageonly$sex<- "N/A"

cbdcov <- readRDS("CBD_covariates")
cbd <- rbind(cbdnon, cbdageonly, cbdsexonly, cbdcov)

cbd$X_NAME_ <- factor(cbd$X_NAME_, levels=c("Severe impairment","Moderate impairment", "Mild/No impairment"))
cbd$Var <- factor(cbd$Var, levels=c("Ocular","Gait", "History", "Limb", "Mentation", "Bulbar"))
cbd$sex <- factor(cbd$sex, levels=c("Female","Male", "N/A"))


cginon <- readRDS("CGI_nosubset")
cginon$sex<- "N/A"
cginon$age<- 0


cgisexonly <- readRDS("CGI_sex_covariates")
cgisexonly$age<- 0

cgiageonly <- readRDS("CGI_age_covariates")
cgiageonly$sex<- "N/A"

cgicov <- readRDS("CGI_covariates")
cgi <- rbind(cginon, cgiageonly, cgisexonly, cgicov)

cgi$X_NAME_ <- factor(cgi$X_NAME_, levels=c("Severe impairment","Moderate impairment", "Mild/No impairment"))
cgi$Var <- factor(cgi$Var, levels=c("Ocular","Gait", "History", "Limb", "Mentation", "Bulbar"))
cgi$sex <- factor(cgi$sex, levels=c("Female","Male", "N/A"))


seadlnon  <- readRDS("SEADL_nosubset")
seadlnon$sex<- "N/A"
seadlnon$age<- 0


seadlsexonly <- readRDS("SEADL_sex_covariates")
seadlsexonly$age<- 0

seadlageonly <- readRDS("SEADL_age_covariates")
seadlageonly$sex<- "N/A"

seadlcov <- readRDS("SEADL_covariates")
seadl <- rbind(seadlnon,seadlageonly, seadlsexonly, seadlcov)

seadl$X_NAME_ <- factor(seadl$X_NAME_, levels=c("Severe impairment","Moderate impairment", "Mild/No impairment"))
seadl$Var <- factor(seadl$Var, levels=c("Ocular","Gait", "History", "Limb", "Mentation", "Bulbar"))
seadl$sex <- factor(seadl$sex, levels=c("Female","Male", "N/A"))


#MMRM
MMRMdataset <- readRDS("MMRMdataset")
