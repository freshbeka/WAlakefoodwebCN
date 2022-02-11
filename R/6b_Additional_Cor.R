## additional analysis for Erin.
## Using the wide form data 

TotAs_wide <- read.csv("data/As_orgs.csv")

ggplot(TotAs_wide, aes(x=littoral_sediment, y = periphyton)) +
  geom_point()
sed_per_lm<-lm(periphyton ~ littoral_sediment, TotAs_wide)
summary(sed_per_lm)
tidy(sed_per_lm)

ggplot(TotAs_wide, aes(x=littoral_sediment, y = phytoplankton)) +
  geom_point()
sed_phy_lm<-lm(phytoplankton ~ littoral_sediment, TotAs_wide)
summary(sed_phy_lm)
tidy(sed_phy_lm)

ggplot(TotAs_wide, aes(x=littoral_sediment, y = zooplankton)) +
  geom_point()
sed_zoo_lm<-lm(zooplankton ~ littoral_sediment, TotAs_wide)
summary(sed_zoo_lm)
tidy(sed_zoo_lm)

ggplot(TotAs_wide, aes(x=littoral_sediment, y = snail)) +
  geom_point()
sed_sn_lm<-lm(snail ~ littoral_sediment, TotAs_wide)
summary(sed_sn_lm)
tidy(sed_sn_lm)

ggplot(TotAs_wide, aes(x=littoral_sediment, y = sunfish)) +
  geom_point()
sed_sun_lm<-lm(sunfish ~ littoral_sediment, TotAs_wide)
summary(sed_sun_lm)
tidy(sed_sun_lm)
