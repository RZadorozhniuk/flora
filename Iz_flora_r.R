library(tidyverse)
library(readxl)

setwd("/media/roman-zadorozhniuk/2DC83449178AD521/Drone/Izium_drone_2025/analize")

flora_list <- read.csv("data_imput/combined_clean_species.csv")

# view(flora_list)

head(flora_list)

flora_list <- flora_list %>%
  rename("coverage" = "Coverage_.")


###         Relative Abundance      ###

#Photo level

flora_list <- flora_list %>%
    group_by(Plot_ID, Photo_ID) %>%
    mutate(
      Tot_Cov_photo = sum(coverage, na.rm = TRUE),
      RA_photo = coverage / Tot_Cov_photo
    ) %>%
    ungroup()

# flora_list <- flora_list %>%
#   group_by(Plot_ID, Photo_ID) %>%
#   mutate(
#     Tot_Cov_photo = sum(coverage, na.rm = TRUE),
#     RA_photo = coverage / Tot_Cov_photo
#   ) %>%
#   ungroup() %>%
#   group_by(Plot_ID) %>%
#   mutate(
#     Tot_Cov_plot = sum(coverage, na.rm = TRUE),
#     RA_plot = coverage / Tot_Cov_plot
#   ) %>%
#   ungroup()

# Plot level
plot_species <- flora_list %>%
  group_by(Plot_ID, Species_std) %>%
  summarise(
    Cov_species_plot = sum(coverage, na.rm = TRUE),
    .groups = "drop"
  )
plot_species

#  рахуємо загальну суму покриття площі
plot_species <- plot_species %>%
  group_by(Plot_ID) %>%
  mutate(
    Tot_Cov_plot = sum(Cov_species_plot),
    RA_plot = Cov_species_plot / Tot_Cov_plot
  ) %>%
  ungroup()

head(plot_species)

flora_list

# some checking 
plot_species %>%
  group_by(Plot_ID) %>%
  summarise(RA_sum = sum(RA_plot),
            .groups = "drop") 



###       Species diversity parameters     ###

#     Shannon-Wiener Index (H')
shannon_plot <- plot_species %>%
  group_by(Plot_ID) %>%
  summarise(
    H_plot = -sum(RA_plot * log(RA_plot)),
    .groups = "drop"
  )

ggplot(shannon_plot, aes(H_plot)) +
  geom_boxplot() +
  ggtitle("Shannon-Wiener Index (H') per trancect")

shannon_photo <- flora_list %>%
  group_by(Plot_ID, Photo_ID) %>%
  summarise(
    H_photo = -sum(RA_photo * log(RA_photo)),
    .groups = "drop"
  )

ggplot(shannon_photo, aes(H_photo)) +
  geom_boxplot() +
  ggtitle("Shannon-Wiener Index (H') per  photo")

# Simpson’s Index (D)

simpson_plot <- plot_species %>%
  group_by(Plot_ID) %>%
  summarise(
    D_plot = sum(RA_plot^2),
    .groups = "drop"
  )

simpson_plot  
  
ggplot(simpson_plot, aes(D_plot)) +
  geom_boxplot() +
  ggtitle("Simpson’s Index (D) per transect")

simpson_photo <- flora_list %>%
  group_by(Plot_ID, Photo_ID) %>%
  summarise(
    D_photo = sum(RA_photo^2),
    .groups = "drop"
  )

# Number species per plot

n_sp_plot <- plot_species %>%
  group_by(Plot_ID) %>%
  summarise(
    n_sp_plot = length(unique(Species_std)),
    .groups = "drop")

head(n_sp_plot)

ggplot(n_sp_plot, aes(n_sp_plot)) +
  geom_boxplot() +
  ggtitle("Numbes species per transect")

n_sp_photo <- flora_list %>%
  group_by(Plot_ID, Photo_ID) %>%
  summarise(
    n_sp_photo = n(),
    .groups = "drop"
  )

# Gini-Simpson Index

# Interpretation: A value of 0 means all items belong to the 
# same type (homogeneity). A value approaching 1 indicates high diversity (heterogeneity). 

GS_plot <- plot_species %>%
  group_by(Plot_ID) %>%
  summarise(
    GS_plot = 1 - sum(RA_plot^2),
    .groups = "drop"
  )

GS_plot  

ggplot(GS_plot, aes(GS_plot)) +
  geom_boxplot() +
  ggtitle("Gini-Simpson Index")


# ENS (Effective Number of Species)
# Interpretation: If a community has an ENS of 5, it means the community is as 
# diverse as one with 5 equally common species.
head(shannon_plot)

shannon_plot <- shannon_plot %>%
  mutate(ENS = exp(H_plot))
  

ggplot(shannon_plot, aes(ENS)) +
  geom_boxplot() +
  ggtitle("ENS (Effective Number of Species) per transecta")

#   ENS_s simpson
simpson_plot <- simpson_plot %>%
  mutate(ENS_s = 1/D_plot)
  
ggplot(simpson_plot, aes(ENS_s)) +
  geom_boxplot() +
  ggtitle("ENS кількість ефективних домінантів")


###         Community-Weighted Proportion (CWP)
# What % of my community is shade-tolerant?")

head(flora_list)
plot_species

# Create table with alien_native classification
traits_unique <- flora_list %>%
  select(Species_std, native_alien_mod) %>%
  distinct()

plot_species <- plot_species %>%
  left_join(traits_unique, by = "Species_std")

# Calc CWP

CWP_plot <- plot_species %>%
  group_by(Plot_ID) %>%
  summarise(
    CWP_plot_native = sum(RA_plot * native_alien_mod, na.rm =  TRUE),
    .groups = "drop"
  )

CWP_plot

ggplot(CWP_plot, aes(CWP_plot_native)) +
  geom_boxplot() +
  ggtitle(" Community-Weighted Proportion of native species per transect")



          ####                            ####

###                 Metrics grouping                 ####

plot_species
plot_stat <- shannon_plot %>%
  # left_join() %>%
  left_join(simpson_plot, by = "Plot_ID") %>%
  left_join(GS_plot, by = "Plot_ID") %>%
  left_join(n_sp_plot, by = "Plot_ID") %>%
  left_join(CWP_plot, by = "Plot_ID")

head(plot_stat)

photo_stat <- shannon_photo %>%
  left_join(simpson_photo, by = c("Plot_ID", "Photo_ID")) %>%
  left_join(n_sp_photo, by = c("Plot_ID", "Photo_ID")) %>%
  mutate(
    GS_photo = 1 - D_photo,
    ENS_photo = exp(H_photo),
    ENS_s_photo = 1 / D_photo
  )




####                            ####

###                 Data grouping                 ####



# Add general table
path_general_table <- "data_imput/general_table.xlsx" 
general_table <- read_excel(path_general_table)

general_table <- general_table %>%
  rename("Plot_ID" = "R_code")

head(general_table)
colnames(general_table)


general_table <- general_table %>%
  left_join(plot_stat, by = "Plot_ID") %>%
  select(-comment_1, -TLU_RMZ, -some_coordinats, -Mis_code, 
         -time, -plot_number, - data) %>%
  mutate(
    chared_h_m_min = as.numeric(chared_h_m_min),
    chared_h_m_max = as.numeric(chared_h_m_max)
  ) %>%
  mutate(chared_h_mean_new = ifelse(Homogenious_chared == "yes", chared_h_m_mean, (chared_h_m_min + chared_h_m_max)/2))

# view(general_table)
# view(plot_stat)


###    Add Maxim`s dNBR data

path_nbr_file = "data_imput/Izium_transects_manual_dNBR.xlsx"
df_ndr <- read_excel(path_nbr_file)

head(df_ndr)
head(general_table)

# view(df_ndr)
# view(general_table)

df_ndr <- df_ndr %>%
  rename("sp_position" = "area") %>%
  rename("comment_mm" = "comment") %>%
  mutate(class_code = ifelse(class == "light", 1,
                             ifelse(class == "light-medium", 2, 3)))  %>%
  mutate(Plot_ID = paste0("Iz_", ID_transect))


print(df_ndr["class"])

general_table <- general_table %>%
  left_join(df_ndr, by = "Plot_ID")          # <-------- UNCOMENT after data will be obtained

colnames(general_table)
# view(general_table)


#         Did Not join wait for corrected data
# .............................. ....................... need to cantinue?



###    Lisvpor data
# path_lisvpor_table <- "data_imput/Export_lispor_data_08012026.xlsx" # <- old one
path_lisvpor_table <- "data_imput/Export_lospor_data_28022026.xlsx" # <- new one

df_lisvpor <- read_excel(path_lisvpor_table)

df_lisvpor <- df_lisvpor %>%
  rename("area_ha" = "Area")

print(df_lisvpor)
# view(df_lisvpor)

#Thinking how to group the data 
# Create table with duplicates
#Prepare script with test dataset and than run using maid dataset
dup_vidil <- df_lisvpor %>%
  group_by(Lisn_kode, Kvar, Vid, PidVid) %>%
  filter(n() > 1) %>%
  arrange(Lisn_kode, Kvar, Vid, PidVid)

# view(dup_vidil)

print(dup_vidil, n = nrow(dup_vidil))


stand_summary <- dup_vidil %>%
  group_by(Lisn_kode, Kvar, Vid, PidVid) %>%
  summarise(
    area_ha = first(area_ha),
    Bonitet = first(Bonitet),
    TLU = first(TLU),
    RS = first(RS),
    
    Zapas_total = sum(
      tapply(
        as.numeric(Zapas_ha),
        Jarus_nomer,
        function(x) ifelse(all(is.na(x)), 0, max(x, na.rm = TRUE))
      )
    ),
    
    # understory detection
    has_understory = ifelse(
      any(
        Jarus_nomer != 1 &
          !grepl("СУХОСТ", Jar_name) &
          !grepl("ПООДИНОК", Jar_name)
      ),
      1, 0
    ),
    
    # Other species inside firs forest "YARUS"
    #iMPORTANT
    #We do not calculate species compoosition NOW
    #Because we have almost clear PYSI forest
    # Other species constitute <5%
    
    has_sec_sp = ifelse(
      n_distinct(Species[Jarus_nomer == 1]) > 1,
      1, 0
    ),
    dom_species = Species[
      Jarus_nomer == 1 & Nomer_porody == 1
    ][1],
    
    dom_sp_age = as.numeric(Age[
      Jarus_nomer == 1 & Nomer_porody == 1
    ][1]),
    
    
    .groups = "drop"
  )

print(stand_summary, n = nrow(stand_summary))


### Run created function with our BIG data
stand_summary <- df_lisvpor %>%
  group_by(Lisn_kode, Kvar, Vid, PidVid) %>%
  summarise(
    area_ha = first(area_ha),
    Bonitet = first(Bonitet),
    TLU = first(TLU),
    RS = first(RS),
    
    Zapas_total = sum(
      tapply(
        as.numeric(Zapas_ha),
        Jarus_nomer,
        function(x) ifelse(all(is.na(x)), 0, max(x, na.rm = TRUE))
      )
    ),
    
    # understory detection
    has_understory = ifelse(
      any(
        Jarus_nomer != 1 &
          !grepl("СУХОСТ", Jar_name) &
          !grepl("ПООДИНОК", Jar_name)
      ),
      1, 0
    ),
    
    # Other species inside firs forest "YARUS"
    #iMPORTANT
    #We do not calculate species compoosition NOW
    #Because we have almost clear PYSI forest
    # Other species constitute <5%
    
    has_sec_sp = ifelse(
      n_distinct(Species[Jarus_nomer == 1]) > 1,
      1, 0
    ),
    dom_species = Species[
      Jarus_nomer == 1 & Nomer_porody == 1
    ][1],
    
    dom_sp_age = as.numeric(Age[
      Jarus_nomer == 1 & Nomer_porody == 1
    ][1]),
    
    
    .groups = "drop"
  )

print(stand_summary, n = nrow(stand_summary))

# stand_summary %>%
#   filter(Kvar %in% c(295, 382)) %>%
#   view()

# view(general_table)


general_table <- general_table %>%
  mutate(
    vid = as.numeric(vid)
  )

final_table <- general_table %>%
  left_join(
    stand_summary,
    by = c("kv" = "Kvar",
           "vid" = "Vid")
  )

colnames(final_table)


# Write manual values where we had 1 kv with a few vid
manual_data <- tibble(
  kv = c(295, 382),
  Lisn_kode      = c(3, 3),       # from lispor data
  PidVid         = c(0, 0),       # 
  area_ha        = c(20.2, 6.5),  # Calc manual
  Bonitet        = c(2, 1),       # dominant
  TLU            = c("В2", "В2"), # dominant
  RS             = c(0.9, 0.7),   #
  Zapas_total    = c(361, 340),  # calc manual
  has_understory = c(0, 0),     # 
  has_sec_sp     = c(1, 0),     # 
  dom_species    = c("СЗ", "СЗ"), # 
  dom_sp_age     = c(82, 70)      # calc manual
)

final_table <- final_table %>% mutate(kv = as.numeric(kv))
final_table_fixed <- final_table %>%
  rows_patch(manual_data, by = "kv")

final_table_fixed %>% 
  filter(kv %in% c(295, 382)) %>% 
  select(kv, area_ha, dom_sp_age, Zapas_total)

# view(final_table_fixed)

write_csv(final_table_fixed, "data_output/df_grouped.csv")


#### some plotting

ggplot(final_table_fixed, aes(H_plot, colour = TLU)) +
  geom_boxplot() +
  coord_flip()

ggplot(final_table_fixed, aes(x = TLU, y = H_plot, fill = TLU)) +
  geom_boxplot(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("Shannon Index")


ggplot(final_table_fixed, aes(x = TLU, y = ENS, fill = TLU)) +
  geom_boxplot(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("ENS (Effective Number of Species) per transecta")


ggplot(final_table_fixed, aes(x = TLU, y = ENS_s, fill = TLU)) +
  geom_boxplot(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("ENS кількість ефективних домінантів")

ggplot(final_table_fixed, aes(x = TLU, y = CWP_plot_native, fill = TLU)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("Частка природних видів")
  
  
colnames(final_table_fixed)
view(final_table_fixed)

ggplot(final_table_fixed, aes(x = class, y = CWP_plot_native, fill = class)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("Частка природних видів / ступінь порушення")
  

ggplot(final_table_fixed, aes(x = class, y = H_plot, fill = class)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("Шенон / ступінь порушення")
  
ggplot(final_table_fixed, aes(x = class, y = ENS, fill = class)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("ENS кількість ефективних домінантів / ступінь порушення")

# Bonitet 
ggplot(final_table_fixed, aes(x = as.factor(Bonitet), y = CWP_plot_native, fill = as.factor(Bonitet))) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("Частка природних видів / бонітет")


ggplot(final_table_fixed, aes(x = as.factor(Bonitet), y = H_plot, fill = as.factor(Bonitet))) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("Шенон / бонітет")

ggplot(final_table_fixed, aes(x = as.factor(Bonitet), y = ENS, fill = as.factor(Bonitet))) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 1.5) +
  theme_minimal() +
  ggtitle("ENS кількість ефективних домінантів / бонітет")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  