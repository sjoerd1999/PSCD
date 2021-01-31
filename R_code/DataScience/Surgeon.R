
install.packages("mice")
library(mice)
library(tidyr)

library(dplyr)


dat <- read.csv2(url("https://raw.githubusercontent.com/1997radhika/TS/main/surgical_case_durations.csv"), header=TRUE, sep=';', encoding='latin-1', dec = ',')
dat[dat == "NULL"] <- NA
dat[dat == "Onbekend"] <- NA
dat<- dat[-seq(nrow(dat),nrow(dat)-0),]

dat2 <- read.csv2(url("https://raw.githubusercontent.com/1997radhika/TS/main/surgical_case_durations.csv"), header=TRUE, sep=';', encoding='latin-1', dec = ',')
dat2[dat2 == "NULL"] <- NA
dat2[dat2 == "Onbekend"] <- NA
dat2<- dat2[-seq(nrow(dat2),nrow(dat2)-0),]

md.pattern(dat2)

library(VIM)
aggr_plot <- aggr(dat2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dat2), cex.axis=.4, gap=.1, ylab=c("Histogram of missing data","Pattern"))
marginplot(dat2[c(21,34)])

library(dplyr)

new_dat <- dat %>%
  group_by(Operatietype) %>%
  filter(n() > 18) %>%
  ungroup()

dat <- as.data.frame(new_dat)

print(dat)

sapply(dat, function(x) sum(is.na(x)))

library(dplyr) 
dat <- dat %>%
  transmute(
    Operatietype = as.factor(Operatietype),
    Chirurg = as.factor(Chirurg),
    Anesthesioloog = as.factor(Anesthesioloog),
    Benadering = as.factor(Benadering),
    OK = as.factor(OK),
    Casustype = as.factor(Casustype),
    Dagdeel = as.factor(Dagdeel),
    Geslacht = as.factor(Geslacht),
    AF = as.factor(AF),
    Chronischelongziekte = as.factor(Chronische.longziekte),
    Extracardialevaatpathie = as.factor(Extracardiale.vaatpathie),
    Eerderehartchirurgie = as.factor(Eerdere.hartchirurgie),
    Actieveendocarditis = as.factor(Actieve.endocarditis),
    Kritischepreoperatievestatus = as.factor(Kritische.preoperatieve.status),
    Myocardinfact90dagen = as.factor(Myocard.infact..90.dagen),
    Aortachirurgie = as.factor(Aorta.chirurgie),
    Pulmonalehypertensie = as.factor(Pulmonale.hypertensie),
    #Linkerventrikelfunctie = as.factor(Linker.ventrikel.functie),
    #Nierfunctie = as.factor(Nierfunctie),
    Slechtemobiliteit = as.factor(Slechte.mobiliteit),
    DM = as.factor(DM),
    Hypercholesterolemie = as.factor(Hypercholesterolemie),
    Hypertensie = as.factor(Hypertensie),
    Perifeervaatlijden = as.factor(Perifeer.vaatlijden),
    CCS = as.factor(CCS),
    NYHA = as.factor(NYHA),
    HLM = as.factor(HLM),
    Leeftijd = as.numeric(sub(",", ".", Leeftijd, fixed = TRUE)),
    Euroscore1 = as.numeric(sub(",", ".", Euroscore1, fixed = TRUE)),
    #Euroscore2 = as.numeric(sub(",", ".", Euroscore2, fixed = TRUE)),
    BMI = as.numeric(sub(",", ".", BMI, fixed = TRUE)),
    Aantalanastomosen = as.numeric(sub(",", ".", Aantal.anastomosen, fixed = TRUE)),
    Geplandeoperatieduur = as.numeric(sub(",", ".", Geplande.operatieduur, fixed = TRUE)),
    Operatieduur = as.numeric(sub(",", ".", Operatieduur, fixed = TRUE)),
    Ziekenhuisligduur = as.numeric(sub(",", ".", Ziekenhuis.ligduur, fixed = TRUE)),
    ICligduur = as.numeric(sub(",", ".", IC.ligduur, fixed = TRUE))
  )

str(dat)

init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("Hypercholesterolemie")] = "logreg"
meth[c("Leeftijd")] = "pmm"
meth[c("Euroscore1")] = "pmm"
#meth[c("Euroscore2")] = "pmm"
meth[c("BMI")] = "pmm"
meth[c("Aantalanastomosen")] = "pmm"
meth[c("Geplandeoperatieduur")] = "pmm"
meth[c("Operatieduur")] = "pmm"
meth[c("Ziekenhuisligduur")] = "pmm"
meth[c("ICligduur")] = "pmm"

meth[c("Operatietype")] = "polyreg"
meth[c("Chirurg")] = "polyreg"
meth[c("Anesthesioloog")] = "polyreg"
meth[c("Benadering")] = "polyreg"
meth[c("OK")] = "polyreg"
meth[c("Casustype")] = "polyreg"
meth[c("Dagdeel")] = "polyreg"
#meth[c("Nierfunctie")] = "polyreg"
meth[c("CCS")] = "polyreg"
meth[c("NYHA")] = "polyreg"
meth[c("Pulmonalehypertensie")] = "polyreg"
#meth[c("Linkerventrikelfunctie")] = "polyreg"

meth[c("Chronischelongziekte")] = "logreg"
meth[c("Extracardialevaatpathie")] = "logreg"
meth[c("Eerderehartchirurgie")] = "logreg"
meth[c("Actieveendocarditis")] = "logreg"
meth[c("Kritischepreoperatievestatus")] = "logreg"
meth[c("Myocardinfact90dagen")] = "logreg"
meth[c("Aortachirurgie")] = "logreg"
meth[c("Slechtemobiliteit")] = "logreg"
meth[c("Perifeervaatlijden")] = "logreg"
meth[c("Geslacht")] = "logreg"
meth[c("AF")] = "logreg"
meth[c("HLM")] = "logreg"
meth[c("Hypertensie")] = "logreg"
meth[c("Hypercholesterolemie")] = "logreg"
meth[c("DM")] = "logreg"


set.seed(103)
imputed3 = mice(dat, method=meth, predictorMatrix=predM, m=20, nnet.MaxNWts=84581)

imputed3 <- complete(imputed3)

sapply(imputed3, function(x) sum(is.na(x)))

str(imputed3)

imputed <- imputed3

# FAMD

suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))

df <-imputed

res.famd <- FAMD(df, 
                 sup.var = 31,  ## Set the target variable "Churn" as a supplementary variable
                 graph = FALSE, 
                 ncp=2)

## Inspect principal components
get_eigenvalue(res.famd)

plot.FAMD(res.famd)
print(res.famd$var)


## Import library
library(PCAmixdata)

## Split mixed dataset into quantitative and qualitative variables
## For now excluding the target variable "Churn", which will be added later as a supplementary variable
split <- splitmix(df[1:33])  

## FAMD
res.pcamix <- PCAmix(X.quanti=split$X.quanti,  
                     X.quali=split$X.quali, 
                     rename.level=TRUE, 
                     graph=FALSE, 
                     ndim=25)

## Inspect principal components
res.pcamix$var



# Correlation matrix

require(tidyverse)
require(rcompanion)


# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}
setwd("C:\Users\Sjoerd de Jong\Desktop")
sink(file='myoutput.txt')
options(max.print=1000000)
mixed_assoc(imputed)
sink()

# plot results
mixed_assoc(imputed) %>%
  ggplot(aes(x,y,fill=assoc))+
  geom_tile()+
  # geom_text(aes(x,y,label=assoc))+
  scale_fill_gradient(low="red", high="yellow")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
  
write.csv(imputed3,"C:/Users/Sjoerd de Jong/Desktop/imputed_csv5.csv", row.names = FALSE)

temp <- imputed
temp %>% mutate_at(vars(vnum1, vnum2), funs(round(., 1)))

set.seed(101)
sample <- sample.int(n = nrow(imputed), size = floor(.75*nrow(imputed)), replace = F)
train <- imputed[sample, ]
test  <- imputed[-sample, ]

train