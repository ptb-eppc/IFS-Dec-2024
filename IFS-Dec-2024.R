library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(srvyr)
library(haven)
library(forcats)
library(readr)
library(extrafont)
library(forcats)
library(showtext)
font_paths()
font_add_google("Lato", "Lato")
showtext_auto()
IFScolors <- c("#791672", "#16791D", "#164179", "#727916", "#1c1678")


elec_24 <- read_excel("Documents/R-2024/elec 24 11-16.xlsx", col_types = c("text", "numeric", "numeric", "text"))
USPS_abbrev <- read_excel("Documents/R-2024/USPS_abbrev.xlsx", col_types = c("text", "text", "numeric", "numeric", "text", "text"))
ACS_DP02 <- read_csv("Documents/R-2024/ACSDP5Y2022.DP02-Data.csv", skip = 1)  
House <- read_excel("Documents/R-2024/CTC-2025.xlsx", sheet = "House")
ACS_S1201_22 <- read_csv("Documents/R-2024/ACSST5Y2022.S1201-Data.csv", skip = 1)
dataverse <- read_csv("Documents/R-2024/1976-2022-house.csv")
S1301_Data12 <- read_excel("Documents/R-2024/ACSST5Y2012.S1301-Data.xls", sheet = "Sheet1")

election <- merge(elec_24, USPS_abbrev, by.x="Abbrev", by.y="USPS")
House24 <- merge(House, election, by="State")
House_24_data <- merge(House, ACS_S1201_22, by.x="Position", by.y="Geographic Area Name")
House_24_merged <- House_24_data |> distinct(.keep_all = TRUE)
H24stage3 <- merge(House_24_merged, House24, by=c("Position", "Member", "State", "Party", "Chamber", "Note", "Flip"))

H24condec <- H24stage3 |> 
  rename(ALLMALES = "Estimate!!Total!!LABOR FORCE PARTICIPATION!!Males 16 years and over") |>
  rename(MALESINFL = "Estimate!!Total!!LABOR FORCE PARTICIPATION!!Males 16 years and over!!In labor force") |>
  mutate(MALELFP = MALESINFL/ALLMALES)|>
  rename(ALLFEMALES = "Estimate!!Total!!LABOR FORCE PARTICIPATION!!Females 16 years and over") |>
  rename(FEMALESINFL = "Estimate!!Total!!LABOR FORCE PARTICIPATION!!Females 16 years and over!!In labor force") |>
  mutate(FEMALELFP = FEMALESINFL/ALLFEMALES)|>  
  rename(MARRIEDRATE = "Estimate!!Now married (except separated)!!Population 15 years and over...67") |>
  select(Position, State, Member, Party, Chamber, Geography, Division, Region, MALELFP, FEMALELFP, MARRIEDRATE, Abbrev, DEM., REP., DemxHouse.y, GOPxHouse.y, FIPS, Note, Flip)

H24stage4 <- merge(H24condec, ACS_DP02)

HOUSE2024 <- H24stage4 |>
  rename(TOTALHHs = "Estimate!!HOUSEHOLDS BY TYPE!!Total households") |>
  rename(MARRIEDwKIDS = "Estimate!!HOUSEHOLDS BY TYPE!!Total households!!Married-couple household!!With children of the householder under 18 years") |>
  rename(COHABITwKIDS = "Estimate!!HOUSEHOLDS BY TYPE!!Total households!!Cohabiting couple household!!With children of the householder under 18 years") |>
  rename(SINGDADwKIDS = "Estimate!!HOUSEHOLDS BY TYPE!!Total households!!Male householder, no spouse/partner present!!With children of the householder under 18 years" ) |>
  rename(SINGMOMwKIDS = "Estimate!!HOUSEHOLDS BY TYPE!!Total households!!Female householder, no spouse/partner present!!With children of the householder under 18 years") |>
  rename(KIDSPRESENTHH = "Percent!!HOUSEHOLDS BY TYPE!!Total households!!Households with one or more people under 18 years") |>  
  rename(ALLFERTILITY = "Estimate!!FERTILITY!!Number of women 15 to 50 years old who had a birth in the past 12 months") |>
  rename(UNMARRFERTILITY = "Estimate!!FERTILITY!!Number of women 15 to 50 years old who had a birth in the past 12 months!!Unmarried women (widowed, divorced, and never married)") |>
  rename(FERTILITYRATE = "Estimate!!FERTILITY!!Number of women 15 to 50 years old who had a birth in the past 12 months!!Per 1,000 women 15 to 50 years old") |>
  mutate(MARRIEDFERTILITY = (as.numeric(ALLFERTILITY)-as.numeric(UNMARRFERTILITY))) |>
  rename(MARRIEDwKIDSpct = "Percent!!HOUSEHOLDS BY TYPE!!Total households!!Married-couple household!!With children of the householder under 18 years") |>
  select(Position, State, Member, Party, Chamber, Geography, Region, Division, MALELFP, FEMALELFP, MARRIEDRATE, TOTALHHs, 
         MARRIEDwKIDS, COHABITwKIDS, SINGDADwKIDS, SINGMOMwKIDS, KIDSPRESENTHH, ALLFERTILITY, UNMARRFERTILITY,
         FERTILITYRATE, MARRIEDFERTILITY, MARRIEDwKIDSpct, Abbrev, DEM., REP., FIPS, DemxHouse.y, GOPxHouse.y, Note, Flip) |>
  distinct(.keep_all = TRUE)

write_delim(HOUSE2024, "Documents/R-2024/HOUSE2024.csv", delim = ",")


cds2012gop <- dataverse |> filter(year=="2012") |> mutate(voteshare = candidatevotes/totalvotes) |> filter(party=="REPUBLICAN")

election2012 <- merge(S1301_Data12, cds2012gop, by=c("state_po", "district"))
election2012house <- merge(election2012, USPS_abbrev, by.x="state_po", by.y="USPS")

write_delim(election2012house, "Documents/R-2024/election2012house.csv", delim = ",")


png("Documents/R-2024/IFS-Dec24-1.png", width = 1600, height = 900, units = 'px', res = 250)
HOUSE2024 |> 
  filter(GOPxHouse.y<.895) |> filter(GOPxHouse.y>.095) |>
  ggplot(aes(x=as.numeric(KIDSPRESENTHH), 
             y=as.numeric(GOPxHouse.y), group=1)) + geom_point(aes(color=Region)) + 
  geom_smooth(method = "lm", se = FALSE, color = "#1c1678") + 
  guides(alpha="none") +
  theme_minimal() +
  labs(x="Share of households with child(ren) under 18 present", 
       y="GOP share of House vote", 
       fill=" ", 
       title="2024 Congressional Elections: Presence of children",
       subtitle=" ",
       caption="Non-competitive districts (>90% or <10%) dropped. Chart by @PTBwrites.\nSource: Census Bureau; Reuters") +
  scale_color_manual(values=IFScolors) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(labels=scales::percent_format(scale = 1)) +
  theme(legend.position="bottom", 
        text = element_text(family = "Lato", color = "#1c1678"),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic"))
dev.off()


png("Documents/R-2024/IFS-Dec24-2.png", width = 1600, height = 900, units = 'px', res = 250)
HOUSE2024 |> 
  filter(GOPxHouse.y<.895) |> filter(GOPxHouse.y>.095) |>
  ggplot(aes(x=as.numeric(MARRIEDwKIDSpct), 
             y=as.numeric(GOPxHouse.y), group=1)) + geom_point(aes(color=Region)) + 
  geom_smooth(method = "lm", se = FALSE, color = "#1c1678") + 
  guides(alpha="none") +
  theme_minimal() +
  labs(x="Share of households which are married couples with kids (18 or under)", 
       y="GOP share of House vote", 
       fill=" ", 
       title="2024 Congressional Elections: Marriage and children",
       subtitle=" ",
       caption="Non-competitive districts (>90% or <10%) dropped. Chart by @PTBwrites.\nSource: Census Bureau; Reuters") +
  scale_color_manual(values=IFScolors) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(labels=scales::percent_format(scale = 1)) +
  theme(legend.position="bottom", 
        text = element_text(family = "Lato", color = "#1c1678"),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic"))
dev.off()  



png("Documents/R-2024/IFS-Dec24-3a.png", width = 1000, height = 900, units = 'px', res = 250)
election2012house |> 
  filter(voteshare<.895) |> filter(voteshare>.095) |>
  ggplot(aes(x=as.numeric(Marriedbirthshare), 
             y=as.numeric(voteshare), group=1)) + 
  geom_point(aes(color=Region)) + 
  geom_smooth(method = "lm", se = FALSE, color = "#1c1678") +
  guides(alpha="none") +
  theme_minimal() +
  labs(x="Share of births to married parents", 
       y="GOP share of House vote", 
       fill=" ", 
       title="2012 Congressional Elections",
       subtitle=" ",
       caption=" \nSource: Census Bureau; MIT Election Data Lab") +
  scale_color_manual(values=IFScolors) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(labels=scales::percent_format(scale = 1)) +
  theme(legend.position="bottom", 
        text = element_text(family = "Lato", color = "#1c1678"),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic"))
dev.off()  



png("Documents/R-2024/IFS-Dec24-3b.png", width = 1000, height = 900, units = 'px', res = 250)
HOUSE2024 |> 
  filter(GOPxHouse.y<.895) |> filter(GOPxHouse.y>.095) |>
  ggplot(aes(x=as.numeric(MARRIEDFERTILITY)/as.numeric(ALLFERTILITY),
             #x=as.numeric(MARRIEDwKIDS)/as.numeric(TOTALHHs), 
             y=as.numeric(GOPxHouse.y), group=1)) + geom_point(aes(color=Region)) + 
  geom_smooth(method = "lm", se = FALSE, color = "#1c1678") + 
  guides(alpha="none") +
  theme_minimal() +
  labs(x="Share of births to married parents", 
       y="GOP share of House vote", 
       fill=" ", 
       title="2024 Congressional Elections",
       subtitle=" ",
       caption="Non-competitive districts (>90% or <10%) dropped.\nSource: Census Bureau; Reuters") +
  scale_color_manual(values=IFScolors) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(labels=scales::percent_format(scale = 100)) +
  theme(legend.position="bottom", 
        text = element_text(family = "Lato", color = "#1c1678"),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic"))
dev.off()  