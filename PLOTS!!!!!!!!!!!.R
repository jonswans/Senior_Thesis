library(ggplot2)
library(dplyr)
library(lubridate)
library(topmodel)
library(DEoptim)
library(parallel)
library(XML)
library(curl)
library(dplyr)
library(devtools)
library(dataRetrieval)
library(EcoHydRology)
library(lubridate)
library(anytime)
library(fpp)
library(astsa)
library(DT)
library(dygraphs)
library(stringr)
library(withr)
library(waterData)
library(hydroTSM)
library(tibble)
library(hms)
library(ggplot2)
library(scales)
library(measurements)
setwd("C:/Users/Jonathan/Documents/Senior_Thesis/data")
ironton <- read.csv("no_dis03216070.csv")

ironton_mon = function(x){
  if(x %in% 1) return("January")
  if(x %in% 2) return("February")
  if(x %in% 3) return("March")
  if(x %in% 4) return("April")
  if(x %in% 5) return("May")
  if(x %in% 6) return("June")
  if(x %in% 7) return("July")
  if(x %in% 8) return("August")
  if(x %in% 9) return("September")
  if(x %in% 10) return("October")
  if(x %in% 11) return("November")
  if(x %in% 12) return("December")
}
ironton$Month = sapply(month(ironton$Date), ironton_mon)

Qnorm <- ironton$X_00060_00003[ironton$Month == "September"]
value <- ironton$X_99133_00003[ironton$Month == "September"]

Sept = ggplot(ironton[ironton$Month == "September", ], aes(x = Qnorm, y = value)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(color=Month, shape=Month),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=20)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()

ggsave(Sept, file=paste0("plot_norm_", siteNumber,".pdf"), width = 14, height = 10, units = "cm", useDingbats=FALSE)


