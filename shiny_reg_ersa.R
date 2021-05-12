library(tidyverse)
library(ERSA)

load("els.Rdata")

els<-els%>%select(bynels2m,
                  bynels2r,
                  byses1,
                  urm,
                  female)

mod_1<-lm(bynels2m~.,data=els)

exploreReg(mod_1)
