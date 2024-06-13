library(here)
library(tidyverse)
library("readxl")

list_years = seq(from = 2013, to = 2022, by = 1)

cbs_stat = data.frame(district_code = character(),
                        district_name = factor(),
                        municipality = factor(), 
                        pop_density = numeric() ,
                        pop = numeric(), 
                        land_ha = numeric(),
                        water_ha = numeric(),
                        tot_ha = numeric(),
                      # av_inc_inh = numeric(),
                        av_inc_rec = numeric(),
                        num_rec =  numeric(),
                        av_house_val = numeric(),
                        house_stock = numeric(),
                        year = numeric())

for(i in list_years){
  data = read_excel(here('raw_data', paste0('kwb-',i,'.xlsx')))
  # These data are StatLine's 'Kerncijfers wijken en buurten (KWB)' form 2013 to 2022 (https://www.cbs.nl/nl-nl/reeksen/publicatie/kerncijfers-wijken-en-buurten)
  data = data %>% 
    filter(recs == 'Wijk') %>% # only keep districts
    filter(gm_naam == "Amsterdam" | gm_naam == "Rotterdam" | gm_naam == "'s-Gravenhage" | gm_naam == "Utrecht"| gm_naam == "Groningen") %>% # only keep districts belonging to Amsterdam, The Hague, Rotterdam, and Utrecht
        select(gwb_code_10, # code 
           regio, # name
           gm_naam, # municipality
           # Population
           bev_dich, # Population density
           a_inw, # Number of inhabitants
           # Surface
           a_lan_ha,  # Surface area of land [ha]
           a_wat_ha,  # Surface area of water [ha]
           a_opp_ha,  # Total area [ha]

           # Income
           g_ink_po, # Average income per income recipient [x 1,000 euros]
         # g_ink_pi, # Average income per inhabitant [x 1,000 euros]
           a_inkont, # Number of income recipients g_ink_po:
             
           # Real estate
           a_woning, # Housing stock 
           g_woz # Average home value [x 1,000 euros]
           
           ) %>%  
    mutate(year = i) %>% 
    rename(district_code = gwb_code_10,    # Rename/translate column names
           district_name = regio,
           municipality = gm_naam,
            pop_density = bev_dich,
            pop = a_inw, 
            land_ha = a_lan_ha,
            water_ha = a_wat_ha,
            tot_ha = a_opp_ha,
            # av_inc_inh = g_ink_pi,
            av_inc_rec = g_ink_po,
            num_inc_rec = a_inkont,
            av_house_val = g_woz,
            house_stock = a_woning)
  
  # Consider '.' values as NAs
  data <- data %>% 
    mutate(
      av_house_val = ifelse(av_house_val =='.', NA, av_house_val),
      house_stock = ifelse(house_stock =='.', NA, house_stock),
      av_inc_rec = ifelse(av_inc_rec =='.', NA, av_inc_rec),
      # av_inc_inh = ifelse(av_inc_inh =='.', NA, av_inc_inh),
      pop_density = ifelse(pop_density =='.', NA, pop_density),
      num_inc_rec = ifelse(num_inc_rec =='.', NA, num_inc_rec)
      )
  
  # Clean and convert to numeric, handling commas
  data <- data %>% 
    mutate(
      av_house_val = as.numeric(gsub(",", ".", av_house_val)),
      house_stock = as.numeric(gsub(",", ".", house_stock)),
      av_inc_rec = as.numeric(gsub(",", ".", av_inc_rec)),
      # av_inc_inh = as.numeric(gsub(",", ".", av_inc_inh)),
      pop_density = as.numeric(gsub(",", ".", pop_density)),
      num_inc_rec = as.numeric(gsub(",", ".", num_inc_rec)),
      tot_ha = as.numeric(gsub(",", ".", tot_ha)),
      water_ha = as.numeric(gsub(",", ".", water_ha)),
      land_ha = as.numeric(gsub(",", ".", land_ha)),
      pop =  as.numeric(gsub(",", ".", pop))
    )
  
  data <- data %>% 
    mutate(
      district_name = as.factor(district_name),
      municipality = as.factor(municipality)
    )
  
  
cbs_stat = rbind(cbs_stat, data)

}

write.csv(cbs_stat,
          here("data", "cbs_statistics_wijk.csv"),
          row.names = FALSE)
