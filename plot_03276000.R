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
library(cowplot)
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

Qnorm9 <- ironton$X_00060_00003[ironton$Month == "September"]
value9 <- ironton$X_99133_00003[ironton$Month == "September"]

Qnorm10 <- ironton$X_00060_00003[ironton$Month == "October"]
value10 <- ironton$X_99133_00003[ironton$Month == "October"]

Qnorm11 <- ironton$X_00060_00003[ironton$Month == "November"]
value11 <- ironton$X_99133_00003[ironton$Month == "November"]

Qnorm12 <- ironton$X_00060_00003[ironton$Month == "December"]
value12 <- ironton$X_99133_00003[ironton$Month == "December"]

Qnorm1 <- ironton$X_00060_00003[ironton$Month == "January"]
value1 <- ironton$X_99133_00003[ironton$Month == "January"]

Qnorm2 <- ironton$X_00060_00003[ironton$Month == "February"]
value2 <- ironton$X_99133_00003[ironton$Month == "February"]

Qnorm3 <- ironton$X_00060_00003[ironton$Month == "March"]
value3 <- ironton$X_99133_00003[ironton$Month == "March"]

Qnorm4 <- ironton$X_00060_00003[ironton$Month == "April"]
value4 <- ironton$X_99133_00003[ironton$Month == "April"]

Qnorm5 <- ironton$X_00060_00003[ironton$Month == "May"]
value5 <- ironton$X_99133_00003[ironton$Month == "May"]

Qnorm6 <- ironton$X_00060_00003[ironton$Month == "June"]
value6 <- ironton$X_99133_00003[ironton$Month == "June"]

Qnorm7 <- ironton$X_00060_00003[ironton$Month == "July"]
value7 <- ironton$X_99133_00003[ironton$Month == "July"]

Qnorm8 <- ironton$X_00060_00003[ironton$Month == "August"]
value8 <- ironton$X_99133_00003[ironton$Month == "August"]

Sept = ggplot(ironton[ironton$Month == "September", ], aes(x = Qnorm9, y = value9)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()



Oct = ggplot(ironton[ironton$Month == "October", ], aes(x = Qnorm10, y = value10)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Nov = ggplot(ironton[ironton$Month == "November", ], aes(x = Qnorm11, y = value11)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Dec = ggplot(ironton[ironton$Month == "December", ], aes(x = Qnorm12, y = value12)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Jan = ggplot(ironton[ironton$Month == "January", ], aes(x = Qnorm1, y = value1)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Feb = ggplot(ironton[ironton$Month == "February", ], aes(x = Qnorm2, y = value2)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Mar = ggplot(ironton[ironton$Month == "March", ], aes(x = Qnorm3, y = value3)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Apr = ggplot(ironton[ironton$Month == "April", ], aes(x = Qnorm4, y = value4)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


May = ggplot(ironton[ironton$Month == "May", ], aes(x = Qnorm5, y = value5)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Jun = ggplot(ironton[ironton$Month == "June", ], aes(x = Qnorm6, y = value6)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Jul = ggplot(ironton[ironton$Month == "July", ], aes(x = Qnorm7, y = value7)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(aes(),
              method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()


Aug = ggplot(ironton[ironton$Month == "August", ], aes(x = Qnorm8, y = value8)) +
  geom_point() + geom_smooth(method=lm, se=FALSE)+
  geom_smooth(
    method=lm, se=FALSE, fullrange=TRUE) +
  #scale_color_hue(l=40,c=35)+
  #scale_color_viridis(discrete=TRUE, option="rainbow") +
  #scale_color_brewer(palette="Spectral") +
  #scale_color_gradient(low="blue",high="red")
  labs(x="Discharge (mm/hr)", y = "Nitrate (mg/L)") +
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(aspect.ratio = 1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^-0.5, 10^0.2)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)), limits=c(10^4.0, 10^6.0)) +
  annotation_logticks()

jan_mar = plot_grid(Jan,Feb,Mar,Apr, labels = c("Jan","Feb","Mar","Apr"),align = 'h', label_size = 15,
                    label_x = 0.7, label_y = 1)
apr_aug = plot_grid(May,Jun,Jul,Aug, labels = c("May","Jun","Jul","Aug"),align = 'h', label_size = 15,
                    label_x = 0.7, label_y = 1)
sept_dec = plot_grid(May,Jun,Jul,Aug, labels = c("May","Jun","Jul","Aug"),align = 'h', label_size = 15,
                     label_x = 0.7, label_y = 1)
ggsave(jan_mar, file=paste0("plot_norm_", "03216070_jan.pdf"), width = 14, height = 10, units = "cm", useDingbats=FALSE)
ggsave(apr_aug, file=paste0("plot_norm_", "03216070_apr.pdf"), width = 14, height = 10, units = "cm", useDingbats=FALSE)
ggsave(sept_dec, file=paste0("plot_norm_","03216070_sept.pdf"), width = 14, height = 10, units = "cm", useDingbats=FALSE)
