library(tidyverse)
library(ggplot2)
library(readr)

options(pillar.sigfig = 5)

#######################   airsac volumes   ########################

volume_est <- read_csv("~/student_documents/UBC/Research/Malawi/data/larva size class measurements/volume_est.csv")
print(volume_est)

# above is long format, calcs are easier in wide
# correct for pH (bring down to approx pH 6 by reducing length dimensions 
# by (23.1%(increase in pH7) - 1.3% (increase from pH5 to in vivo size)) = 21.8% (from glam shot tube expt))
volume_est.wide <- volume_est %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(length_pH_vivo_corr = length * 0.782) # 21.8% reduction for vivo size

print(volume_est.wide, n= 20)


## volume calculation is different for tips vs. midsecs, so separating the data frames
#mid-section measures do still need tip values from other df (but at different row locations than the midsec we're calculating for in the main df)

# tips data frame
tip.df <- volume_est.wide %>%
  filter(region == "tip1" | region == "tip2") %>%
  group_by(indvd, sac, region) %>%
  mutate(volume = (((base/2)^2) * length * ((4/3) * pi)) / 2) %>%
  mutate(volume_corr = (((base/2)^2) * length_pH_vivo_corr * ((4/3) * pi)) / 2)

print(tip.df)  

# mid section (cylinders) data frame
midsec.df <- volume_est.wide %>%
  filter(region == "midsec") %>%
  mutate(tip1 = subset(tip.df, # tip1 base for mean hight in cylinder volume calc
                            region == "tip1", 
                            select = "base")) %>% 
  mutate(tip2 = subset(tip.df, # tip2 base for mean hight in cylinder volume calc
                            region == "tip2", 
                            select = "base")) 
# brute force rename (above mutates add "$base" for some reason, and this fucks up dplyr renaming/ other stuff) 
midsec.df[ , 7] <- midsec.df$tip1 
midsec.df[ , 8] <- midsec.df$tip2

midsec.df <- midsec.df %>%
  group_by(indvd, sac) %>%
  mutate(volume = (((mean(c(base, tip1, tip2)))/2)^2) * pi * length) %>%
  mutate(volume_corr = (((mean(c(base, tip1, tip2)))/2)^2) * pi * length_pH_vivo_corr)

print(midsec.df)


##################     make a dataframe of indvd and sac, 
##################     with region condensed so each sac appears once,
##################     and a column each for tip volume and midsec volume

# start by using the tip df
vol_df <- tip.df %>%
  select(-base, -length, -length_pH_vivo_corr) %>%
  group_by(indvd, sac) %>%
  mutate(sum_tip_vol = volume[1] + volume[2]) %>%
  mutate(sum_tip_vol_corr = volume_corr[1] + volume_corr[2]) %>%
  select(-volume, -volume_corr, -region) %>%
  unique()

# bring in the mid section info
vol_df$midsec_vol <- midsec.df$volume
vol_df$midsec_vol_corr <- midsec.df$volume_corr

# calculate sac volumes
vol_df <- vol_df %>%
  mutate(sac_volume_uL = sum_tip_vol + midsec_vol) %>%
  mutate(sac_volume_uL_corr = sum_tip_vol_corr + midsec_vol_corr)

print(vol_df)

# a spreadsheet with pH 7 (excised) as well as in vivo corrected sac volumes for each sac
write_csv(vol_df,
          "~/student_documents/UBC/Research/Malawi/data/larva size class measurements/sac_volume.csv")


# a data frame and spreadsheet with total larva air volume
total_air <- vol_df %>%
  group_by(indvd) %>%
  select(indvd, sac_volume_uL, sac_volume_uL_corr) %>%
  mutate(vol_total = sum(sac_volume_uL[1:4])) %>%
  mutate(vol_total_corr = sum(sac_volume_uL_corr[1:4])) %>%
  select(-sac_volume_uL, -sac_volume_uL_corr) %>%
  unique()

print(total_air)

write_csv(total_air,
          "~/student_documents/UBC/Research/Malawi/data/larva size class measurements/total_air.csv")


##################    asigning instar based on head_cap_len   #################

crush_raw <- crush_raw %>% 
  mutate(instar = replace(instar, 
                          head_cap_len > 0.88 & 
                            head_cap_len < 1.1, 
                          4)) %>%
  mutate(instar = replace(instar, 
                          head_cap_len > 0.58 & 
                            head_cap_len < 0.72, 
                          3)) %>%
  mutate(instar = replace(instar, 
                          head_cap_len > 0.3 & 
                            head_cap_len < 0.5, 
                          2)) %>%
  mutate(species = as_factor(species))

print(crush_raw)




