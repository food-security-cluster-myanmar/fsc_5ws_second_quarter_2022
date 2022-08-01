
# table-activities-transfer-values-quarter
fsc %>%
  filter(delivery_modality %in% c("CBT/CVA", "Hybrid (In-kind & CBT/CVA)")) %>%
  mutate(quarter = recode(quarter,
                          "q1" = "Quarter_1", 
                          "q2" = "Quarter_2")) %>%  
  filter(activity_red %out% c("FFS and farmer training", 
                              "vocational training", 
                              "IGA and small grants") & 
           usd_per_person <= 50) %>% 
  mutate(activity_red = fct_relevel(activity_red, 
                                    c("food distribution", 
                                      "multi-purpose cash transfer", 
                                      "food_cash for work_assets", 
                                      "livestock kits",
                                      "crop, vegetable and seed kits"))) %>% 
  ggplot(aes(x = usd_per_person, y = activity_red, fill = activity_red)) +
  geom_boxplot(outlier.alpha = .3) + 
  facet_wrap(~ quarter) + 
  theme(legend.position = "none", 
        strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_rect(fill = "#212121")) + 
  labs(x = "Average cash transfer value per household (USD)", 
       y = "", 
       title = "Average cash transfer values per activity")

fsc_disagg %>% 
  filter(new_beneficiaries > 0) %>% 
  mutate(pc_disagg = ben_sub / beneficiaries) %>% 
  group_by(org_code, disagg) %>% 
  mutate(mean = mean(pc_disagg), 
         sd = sd(pc_disagg, na.rm = TRUE),
         ben_sub = sum(ben_sub, na.rm = TRUE)) %>% 
  mutate(cat = ifelse(sd >= .05, "real", "fake"), 
         cat = ifelse(is.na(cat), "real", cat)) %>% 
  select(org_code, activity_red, disagg, ben_sub, cat, row_index) %>% 
  group_by(row_index, cat) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = cat, values_from = count, values_fill = 0) %>% 
  mutate(pc_fake = fake / (fake + real)) %>% 
  filter(pc_fake >= .5) %>%  
  write_csv("./data/row_index_fake_values.csv")

fsc %>% 
  mutate(pc_pwd = round(pwd_total / beneficiaries * 100, digits = 2), 
         pwd_fake = ifelse(pc_pwd == 2.50, "fake", "real")) %>%
  filter(pwd_fake == "fake") %>% 
  select(row_index, pwd_fake) %>% 
  write_csv("./data/pwd_fake.csv")

fsc %>% 
  group_by(org_code) %>%  
  # summarise(townships = n_distinct(admin3_pcode)) %>% 
  # summarise(mean = median(townships))
  summarise(beneficiaries = sum(new_beneficiaries)) %>% 
  summarise(mean = median(beneficiaries))

# How to determine which townships have not been reached 
pin %>% 
  left_join(fsc %>% 
              group_by(admin3_pcode_old) %>% 
              summarise(beneficiaries = sum(new_beneficiaries)), 
            by = c("admin3_pcode" = "admin3_pcode_old")) %>% 
  filter(is.na(beneficiaries))

pin %>% 
  right_join(pcode3_shape, by = "admin3_pcode") %>% 
  st_as_sf() %>%  
  ggplot() + 
  geom_sf(aes(fill = fs_pin), size = .1) + 
  scale_fill_viridis_c(direction = -1, trans = "log10",  
                       breaks = c(100, 1000, 10000, 100000, 500000),
                       limits = range(57, 801760), 
                       labels = comma) + 
  theme_void() + 
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.7, 'cm'), 
        plot.background = element_rect(fill = "white", colour = "white")) +
  labs(title = "2022 PIN by township",
       subtitle = "townships in grey have not yet had a PIN calculated", 
       fill = "PIN")

# ggsave("pin_by_tsp.png", dpi = 300, height = 18, width = 11, units = "in")


# maps-ben-target
pin %>% 
  select(admin3_pcode, target = fs_targeted) %>% 
  right_join(pcode3_shape, by = c("admin3_pcode")) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = target), size = .1) +
  scale_fill_viridis(direction = -1, trans = "log10", option = "mako",  
                     breaks = c(100, 1000, 10000, 100000, 500000), 
                     labels = comma) + 
  theme_void() + 
  theme(legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10), 
        legend.key.size = unit(.7, "cm"), 
        plot.background = element_rect(fill = "white", colour = "white")) + 
  labs(title = "2022 Target by township", 
       subtitle = "Townships in grey do not have any targets", 
       fill = "Target") + 
  
  fsc %>% 
  group_by(admin3_pcode_old) %>% 
  summarise(beneficiaries = sum(new_beneficiaries)) %>% 
  right_join(pcode3_shape, by = c("admin3_pcode_old" = "admin3_pcode")) %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = beneficiaries), size = .1) +
  scale_fill_viridis(direction = -1, trans = "log10", option = "mako",
                     breaks = c(100, 1000, 10000, 100000, 500000), 
                     labels = comma) + 
  theme_void() + 
  theme(legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10), 
        legend.key.size = unit(.7, "cm"), 
        plot.background = element_rect(fill = "white", colour = "white")) + 
  labs(title = "Beneficiaries by township, Q1 & Q2 2022", 
       subtitle = "Townships in grey do not have any partners present", 
       fill = "Beneficiaries")  

# New townships
fsc %>% 
  distinct(state, township, admin3_pcode) %>% 
  left_join(fsc %>% 
              filter(quarter == "q1") %>% 
              distinct(quarter, admin3_pcode), 
            by = c("admin3_pcode")) %>% 
  filter(is.na(quarter)) %>% 
  group_by(state) %>% 
  summarise(new_townships = n()) %>% 
  arrange(desc(new_townships)) %>% 
  adorn_totals("row") %>% 
  kable(caption = "Number of new townships in Q2 2022", 
        format = "html", 
        table.attr = "style='width:60%;'") %>% 
  kable_classic_2("striped")

# Comparison between 2021 and 2022 cash transfer values 
fsc %>%  
  filter(activity_red == "food distribution" & new_beneficiaries > 0) %>% 
  mutate(year = year(date)) %>% 
  select(usd_person_bin, beneficiaries, year) %>% 
  rbind(fsc_2021 %>%
          filter(activity_red == "food distribution" & unique_beneficiaries == "Yes") %>% 
          mutate(year = year(date)) %>% 
          select(usd_person_bin, beneficiaries, year)) %>% 
  filter(!is.na(usd_person_bin)) %>%  
  mutate(year = as.character(year)) %>% 
  group_by(year, usd_person_bin) %>% 
  summarise(beneficiaries = sum(beneficiaries)) %>% 
  group_by(year) %>% 
  mutate(pc_ben = beneficiaries / sum(beneficiaries),
         usd_person_bin = fct_relevel(usd_person_bin, c("<$2", ">=$2_<$4", 
                                                        ">=$4_<$6", ">=$6_<$8", 
                                                        ">=$8_<$10",">=$10_<$12",
                                                        ">=$12_<$14", ">=$14_<$16", 
                                                        ">=$16_<$18", ">=$18_<$20",
                                                        ">=20"))) %>% 
  ggplot(aes(y = fct_rev(usd_person_bin), x = pc_ben, fill = year)) + 
  geom_col(position = "dodge", width = .8) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, .1)) + 
  scale_fill_manual(values = c("#E65100", "#FFCC80"), 
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "% of beneficiaries", y = "", fill = "", 
       title = "Comparison between food distribution cash transfer values", 
       subtitle = "2021 vs 2022 (Q1 & Q2)")

```