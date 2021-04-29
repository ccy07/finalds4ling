library(dplyr)
library(tidyr)
library(lme4)
library(broom.mixed)
library(faux)

between <- list(dia = c(nor = "northern", 
                        sou = "southern"))
within <- list(con = c("s1", "s2", "s3", "s4"))
df <- sim_design(within, between, 
                 n = 50, mu = 1:8, sd = 1)
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}


df$s1[df$s1> 7] <- df$s1[df$s1 > 7] * 0 + 7 
df$s2[df$s2> 7] <- df$s2[df$s2 > 7] * 0 + 7 
df$s3[df$s3> 7] <- df$s3[df$s3 > 7] * 0 + 7 
df$s4[df$s4> 7] <- df$s4[df$s4 > 7] * 0 + 7 

df$s1[df$s1< 1] <- df$s1[df$s1 < 1] * 0 + 1 
df$s2[df$s2< 1] <- df$s2[df$s2 < 1] * 0 + 1 
df$s3[df$s3< 1] <- df$s3[df$s3 < 1] * 0 + 1 
df$s4[df$s4< 1] <- df$s4[df$s4 < 1] * 0 + 1
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

df<-round_df(df, digits=0)
write.csv(df,"~/Desktop/finalds4ling/Data/dataraw.csv")
