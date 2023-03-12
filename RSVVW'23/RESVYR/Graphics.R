#
# Creation of the graphics to the RSV poster to RSVVW23
# 
# Author: Charles NUTTENS - charles.nuttens@pfizer.com
#
# File creation: 08/02/2023
# Last modification: 09/02/2023

# WORKING DIRECTORY -------------------------------------------------------
setwd("C:/Users/NUTTEC/OneDrive - Pfizer/02 - RSV OA/Scientific projects/RESVYR/MANUSCRIPT AND ABSTRACT/RSVVW23 (ReSViNET conference)/Poster")

# PACKAGE -----------------------------------------------------------------

library(tidyverse)
library(readxl)

Sys.setlocale("LC_TIME", "English")

# LOAD DATA ---------------------------------------------------------------
df.hosp <- read_csv2("data/hospitalization_3_agegroups.csv",
                     col_types = cols(Age = col_factor(levels = c("50-64", "65-74", "75+"), ordered = TRUE))) %>%# Use factor for age groups
  mutate(Date = as.Date(Date))

df.proxy <- read_excel("data/Indicators.xlsx")

# FIGURE 1 ---------------------------------------------------------------

df <- df.hosp %>% filter(Cause == "Respiratory disease") #Filter to "respiratory cause"

pdf("Figure 1.pdf", width=8, height=4, paper = "a4", title = "RESVYR") #Generate the PDF
par(mar = c(5, 5, 3, 5), cex = 0.65, lwd = 0.8)

plot(filter(df, Age == "75+")$Date, #Plot with 75+ time serie
     filter(df, Age == "75+")$Incidence,
     ylim = c(0, 210),
     type = "l",
     col = "black", #"#E31A1C", Red
     ylab = "incidence rate (hospitalization per 100,000 population)",
     xlab = "",
     xaxt = "n",
     yaxt = "n"
     )

xaxt <- seq(min(filter(df, Age == "75+")$Date), max(filter(df, Age == "75+")$Date), by = "6 month") #Time break to display in x axis
axis.Date(1, xaxt, at = xaxt, format = "%b %Y", cex = 0.65, lwd = 0.8, las = 2) #x axis
axis(2, cex = 0.65, lwd = 0.8) #y axis

lines(filter(df, Age == "65-74")$Date,
      filter(df, Age == "65-74")$Incidence,
      type = "l",
      col = "grey40") #"#FF7F00")

lines(filter(df, Age == "50-64")$Date,
      filter(df, Age == "50-64")$Incidence,
      type = "l",
      col = "grey70") #"gold1")

## Legend
legend("topleft",
       col = c("black", "grey40", "grey70"),
       lty = 1,
       legend = c("75 years and older", "65-74 years", "50-64 years"),
       bty = "n",
       cex = 1,
       y.intersp = 1,
       x.intersp = 0.5,
       seg.len = 2.5)

dev.off()

# FIGURE 2 ---------------------------------------------------------------

df.influenza <- df.proxy %>% filter(Indicator == "Hospitalization for influenza\r\n(DP/DAS: J09-J10, J11)", Age == "65-XX") %>% arrange(Date)
df.rsv <- df.proxy %>% filter(Indicator == "Hospitalization for bronchiolitis <\r\n2 years (specified to VRS and\r\nunspecified)") %>% arrange(Date)

pdf("Figure 2.pdf", width=8, height=4, paper = "a4", title = "RESVYR")
par(mar = c(5, 5, 3, 5), cex = 0.65, lwd = 0.8)

plot(df.rsv$Date,
     df.rsv$Incidence,
     #ylim = c(0, 210),
     type = "l",
     col = "#E31A1C",
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n"
)

xaxt <- seq(min(df.influenza$Date), max(df.influenza$Date), by = "6 month")
axis(1, xaxt, format(xaxt, "%b %Y"), cex = 0.65, lwd = 0.8, las = 2)
axis(2, cex = 0.65, lwd = 0.8, col.axis= "#E31A1C")
mtext("incidence rate of RSV (hospitalization per 100,000 population)", side = 2, line = 3, col = "#E31A1C", cex = 0.65)

par(new = TRUE, cex = 0.65, lwd = 0.8)
plot(df.influenza$Date,
     df.influenza$Incidence,
     #ylim = c(0, 210),
     type = "l",
     col = "dodgerblue2",
     ylab = "",
     xlab = "",
     xaxt = "n",
     yaxt = "n"
)
axis(4, cex = 0.65, lwd = 0.8, col.axis= "dodgerblue2")
mtext("incidence rate of influenza (hospitalizations per 100,000 population)", side = 4, line = 3, col = "dodgerblue2", cex = 0.65)

## Legend
legend("topleft",
       col = c("#E31A1C", "dodgerblue2"),
       lty = 1,
       legend = c("RSV (below 2 years)", "influenza (65 years and older)"),
       bty = "n",
       cex = 1,
       y.intersp = 1,
       x.intersp = 0.5,
       seg.len = 2.5)

dev.off()
