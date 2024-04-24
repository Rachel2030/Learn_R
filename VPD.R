#Data cleaning and analysis for Verbal Autopsy data
Set directory
setwd(".")

# Load packages
pkgs <- c("haven", "tidyverse", "ggthemes", "viridis","finalfit","stringr", "writexl", "paletteer", "gtsummary", "table1", "VIM", "lubridate")
if(!"pacman" %in% installed.packages()[,1]){install.packages("pacman")}
pacman::p_load(pkgs, character.only = T, install = T)


#Load Data
VPD <- read_dta("Verbalautopsy_2002-2015 NUHDSS.dta") %>% as_factor()
labels <- VPD %>% extract_labels()
view(VPD)

#####filter######
filtered_data <- VPD %>% filter(vau_vadone=="yes")
filtered_data

filtered_data2 <- subset(filtered_data, select = c("vau_individualid_anon",
                                                    "vau_hhid_anon",
                                                    "vau_vadone",
                                                    "vau_round_intvw",
                                                    "vau_round_event",
                                                    "vau_slumarea",
                                                    "vau_gender",
                                                    "vau_datebirth",
                                                    "vau_datedeath",
                                                    "vau_agedeath_years",
                                                    "vau_agegroupdeath",
                                                    "vau_intvwresult",
                                                    "vau_respreltohhh",
                                                    "vau_respwascaretaker",
                                                    "vau_death_province",
                                                    "vau_death_district",
                                                    "vau_death_areatype",
                                                    "vau_death_village",
                                                    "vau_deathplace",
                                                    "vau_durofillorinjury",
                                                    "vau_carefrom_first",
                                                    "vau_injuryintended",
                                                    "vau_surv_immediate",
                                                    "vau_deathcertissued",
                                                    "pcod_under_final",
                                                    "pcodbroad_under_final",
                                                    "pcodgeneral_under_final",
                                                    "pagreement_under",
                                                    "pcod_immed_final",
                                                    "pcodbroad_immed_final",
                                                    "pcodgeneral_immed_final",
                                                    "pagreement_immed",
                                                    "icodbroad_1",
                                                    "icodgeneral_1"
))
filtered_data2

#drop unused levels from the gender variable
filtered_data3 <- filtered_data2 %>% 
  dplyr::select(vau_gender) %>%
  dplyr::mutate(across(c(vau_gender),  ~fct_drop(.x )))

#drop unused levels from the whole dataset
filtered_data4 <- filtered_data2 %>% 
  droplevels()
filtered_data4

filtered_data4 <- remove_missing_values(filtered_data4)

#check the duplicated variable causing the error
duplicated_names <- duplicated(names(filtered_data4))
duplicate_columns <- names(filtered_data4)[duplicated_names]
print(duplicate_columns) 




###########gender counts
gender_counts <- filtered_data4 %>%
  group_by(vau_individualid_anon, vau_gender) %>%
  summarize(n = n()) %>%
  group_by(vau_gender) %>%
  summarize(total = n())
print(gender_counts)



##############view missing values for each variable
colSums(is.na(filtered_data4))

#analysis
table(filtered_data4$vau_gender, filtered_data4$vau_slumarea) #crosstab

tbl_summary(filtered_data4 %>% dplyr::select(vau_gender))  #summary
################### table summary for distn of various xtics by gender
Table_sum <-filtered_data4 %>%
  tbl_summary(
    include = c(vau_slumarea,vau_agegroupdeath, icodbroad_1, vau_deathplace),
    by = vau_gender, 
    statistic = list(all_continuous() ~ "{median} ({IQR})", 
                     all_categorical() ~ "{n} ({p}%)"), 
    digits = all_categorical() ~ c(0,1)  # Specify 1 decimal place for proportions
  ) %>% 
  gtsummary::add_overall()%>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3))%>%
  modify_caption("Distribution of mortality by gender (N = {N})") %>%
  as_flex_table()
flextable::save_as_docx(Table_sum,
                        path = "Distribution of mortality by gender.docx",
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape")
                        )
)


table <- table1(~vau_gender + vau_slumarea + icodbroad_1 + vau_deathplace + vau_yeardeath ,filtered_data4) #table for descriptives per variabl
write_xlsx(as.data.frame(table), "descriptive_table.xlsx")



filtered_data4 <- filtered_data4[filtered_data4$vau_slumarea != "missing:impute", ]



####DESCRIPTIVES###

###Histogram##
#demographics
ggplot(filtered_data4, aes(x = vau_slumarea)) +
  geom_bar(stat = "count") + 
  labs(title = "Death distribution by slumarea")

ggplot(filtered_data4, aes(x = vau_agegroupdeath)) +
  geom_bar(stat = "count") + 
  labs(title = "Death distribution by age group")

ggplot(filtered_data4, aes(x = vau_gender)) +
  geom_bar(stat = "count") + 
  labs(title = "Death distribution by gender")

ggplot(filtered_data4, aes(x = vau_deathplace)) +
  geom_bar(stat = "count") + 
  labs(title = "Death distribution by deathplace")

ggplot(filtered_data4, aes(x = vau_death_province)) +
  geom_bar(stat = "count") + 
  labs(title = "Death distribution by province")

ggplot(filtered_data4, aes(x = vau_yeardeath)) +
  geom_bar(stat = "count") + 
  labs(title = "Year of death")

ggplot(filtered_data5, aes(x = vau_Age)) +
  geom_histogram(stat = "count") + 
  labs(title = "Histogram for Age at death")

ggplot(filtered_data4, aes(x = vau_deathcertissued)) +
  geom_bar(stat = "count") + 
  labs(title = "Death cerfificate issued")

####DEATHS########################
####extract year from date of death and merge into the original dataset###
filtered_deaths <- filtered_data4$vau_datedeath

years <- year(filtered_deaths)
year_strings <- strftime(filtered_deaths, "%Y")
year_strings
vau_yeardeath <- data.frame(year_strings)
vau_yeardeath
filtered_data5$vau_yeardeath <- vau_yeardeath$year_strings

#####proportions of deaths over the years####
deathyear <- round(prop.table(table(filtered_data4$vau_yeardeath)) * 100, 2)
data.frame(deathyear)
write.table(deathyear, file = "deathyear_proportions.docx", row.names = TRUE)



# #calculate age at death and extract months
# filtered_data5 <- filtered_data4 %>% mutate(
#   vau_Age = round(time_length(difftime(vau_datedeath, vau_datebirth, units = "auto"), unit = "year"),0
# ), 
# vau_monthdeath = lubridate::floor_date(vau_datedeath, unit = "month")
# 
# ) %>% 
#   group_by(vau_monthdeath, vau_gender) %>% 
#   count()
# 
# ggplot(filtered_data5, aes(vau_monthdeath, n)) + 
#          geom_line(aes(color = vau_gender))
# 

filtered_data5 <- filtered_data4 %>% mutate(
  vau_Age = round(time_length(difftime(vau_datedeath, vau_datebirth, units = "auto"), unit = "year"),0
  ), 
  vau_monthdeath = lubridate::floor_date(vau_datedeath, unit = "month"), 
  across(where(is.character), as.factor)
  
)


# Time series plot function
 plot1 <- sapply(c("vau_gender", "vau_slumarea"), function(x) {
  nn <- x 
  filtered_data5 <- filtered_data4 %>% mutate(
    vau_Age = round(time_length(difftime(vau_datedeath, vau_datebirth, units = "auto"), unit = "year"),0
    ), 
    vau_monthdeath = lubridate::floor_date(vau_datedeath, unit = "month"), 
    across(where(is.character), as.factor)
    
  )
  index_colour <- filtered_data5[[nn]]
  index <- filtered_data5[["vau_monthdeath"]]
  plot <- ggplot(filtered_data5, aes(index)) + 
    geom_line(aes(color = index_colour, group = index_colour), stat = "count") + 
    labs(title = "Time series plot for the distribution of deaths over the years", x = "Time (Years)", y = "Number of deaths") +
    scale_colour_brewer(palette = "Set1") + 
    scale_x_date(breaks = "1 year")
  
}, simplify = FALSE)
 print(plot1)
 ggsave(plot= plot1, height = 6, width = 10,
        filename = paste0("Timeseriesplot",".png"), bg='white')
 
 
 
#  plot1 <- sapply(c("vau_slumarea"), function(x) {  # Focus on "vau_slumarea"
#    nn <- x
#    filtered_data5 <- filtered_data4 %>% mutate(
#      vau_Age = round(time_length(difftime(vau_datedeath, vau_datebirth, units = "auto"), unit = "year"), 0),
#      vau_monthdeath = lubridate::floor_date(vau_datedeath, unit = "month"),
#      across(where(is.character), as.factor)
#    )
#    index_colour <- filtered_data5[[nn]]
#    index <- filtered_data5[["vau_monthdeath"]]
#    
#    plot <- ggplot(filtered_data5, aes(index)) +
#      geom_line(aes(color = index_colour, group = index_colour), stat = "count") +
#      labs(x = "Date of Death", y = "Number of Deaths", title = "Time Series plot for the number of Deaths by Slum Area") +
#      scale_x_date(breaks = "1 year") + 
#      scale_colour_brewer(palette = "Set1")
#    
#  }, simplify = FALSE) 
#  
# print(plot1)

#######OR
filtered_data5 <- filtered_data4 %>%
  mutate(
    vau_Age = round(time_length(difftime(vau_datedeath, vau_datebirth, units = "auto"), unit = "year"), 0),#calculate age
    vau_monthdeath = lubridate::floor_date(vau_datedeath, unit = "month")
  )
filtered_data5
plot_gender <- ggplot(filtered_data5, aes(vau_monthdeath)) +
  geom_line(aes(color = vau_gender, group = vau_gender), stat = "count")

plot_area <- ggplot(filtered_data5, aes(vau_monthdeath)) +
  geom_line(aes(color = vau_slumarea, group = vau_slumarea), stat = "count")

# visualize the plots
print(plot_gender)
print(plot_area)
# saving the plot
ggsave(plot= plot1, height = 6, width = 10,
       filename = paste0("Timeseriesplot",".png"), bg='white')



####bar plot for the immediate and broad causes of death

ggplot( filtered_data5,  aes(x = fct_infreq(icodbroad_1))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Deaths by immediate Causes",  
       x = "Immediate Cause of Death",  
       y = "Number of deaths") +  # Rename y-axis
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), breaks = seq(0, 3000, 250)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot( filtered_data4,  aes(x = fct_infreq(pcodgeneral_under_final))) +
  geom_bar(fill = "green") +
  labs(title = "General underlying Causes",  
       x = "Underlying Cause of Death",  
       y = "Number of deaths") +  # Rename y-axis
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), breaks = seq(0, 3000, 250)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#########Descriptives for leading causes of death by gender
leading_causes <- filtered_data5 %>%
  group_by(vau_gender, icodbroad_1) %>%
  # Count occurrences
  summarise(n = n()) %>%
  #top cause of death for each gender (based on count)
  group_by(vau_gender) %>%
  top_n(1, n) %>%   #top cause of death for each gender (based on count)
  rename(leading_cause = icodbroad_1, count = n)

gender_stats <- filtered_data5 %>%
  filter(icodbroad_1 == leading_causes$leading_cause) %>%
  group_by(vau_gender) %>%
  summarise(
    
    mean_age = mean(vau_Age),
    pct_total = n() / nrow(filtered_data4) * 100
  )

combined_results <- bind_cols(leading_causes, gender_stats)
print(combined_results)

  

# check for duplicate household IDs:
duplicate_ids <- duplicated(filtered_data5$vau_hhid_anon)
duplicate_rows <- filtered_data5[duplicate_ids, ]
if (nrow(duplicate_rows) > 0) {
  message <- paste("Warning: There are", nrow(duplicate_rows), "Households with repeated anonymizer IDs")
  print(message)
  print(duplicate_rows)  # Display the duplicated rows
} else {
  print("No duplicate household IDs found.")
}



averages <- filtered_data5 %>%
  group_by(vau_slumarea, vau_yeardeath) %>%
  summarise(counts = length(unique(vau_individualid_anon)))


ggplot(filtered_data5, aes(x = vau_agegroupdeath, y = count, fill = icodbroad_1)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Leading Cause of Death by Age Group", x = "Age Group", y = "Number of Deaths") +
  theme_bw()

#More visualizations
VP_Deaths1 <- VP_Deaths %>%
  mutate(
    vau_datebirth = ymd(vau_datebirth),
    vau_datedeath = ymd(vau_datedeath),
    vau_yeardeath1 = lubridate::year(vau_datedeath),
    vau_Age_years = round(time_length(difftime(vau_datedeath, vau_datebirth, 
                                               units = "auto"), unit = "year"),2),
    vau_Age_months = round(time_length(difftime(vau_datedeath, vau_datebirth, 
                                                units = "auto"), unit = "month"),2),
    vau_Age_group = if_else(vau_Age_years < 1, "Infant",
                            if_else(vau_Age_years < 15, "1-14",
                                    if_else(vau_Age_years < 35, "15-34",
                                            if_else(vau_Age_years < 65, "35-64", ">=65")))),
    vau_Age_group = if_else(vau_Age_years < 1 & vau_Age_months < 1, "Neonate", vau_Age_group),
    vau_Age_group = factor(vau_Age_group, levels = c("Neonate", "Infant", "1-14", "15-34", "35-64", ">=65")),
    vau_deathmonth = lubridate::floor_date(vau_datedeath, unit = "month")
  ) %>% #labeling variables from data dictionary 
  labelled::set_variable_labels( #creating labels for new variables
    HDSS_Site = "HDSS Site",
    vau_gender = "Gender",
    vau_Age_years = "Age (years)",
    vau_Age_group = "Age group",
    icod_1 = "Cause of death"
  )

## Descriptives

table_1 <- tbl_summary(VP_Deaths1 %>%
                         dplyr::select(HDSS_Site, vau_gender, vau_Age_years, vau_Age_group, icod_1),
                       by = HDSS_Site,
                       type = list(
                         all_dichotomous() ~ "categorical"
                         ,all_continuous() ~ "continuous2")
                       , statistic = list(
                         all_continuous(continuous2 = TRUE) ~ c(
                           "{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}" )
                       )
                       , digits = list(all_categorical() ~ c(0, 1)
                                       , all_continuous() ~ 1)
                       , missing = "ifany" #list missing data separately #ifany #no #always
                       ,missing_text = "Missing"
) %>% 
  modify_header(label = "**Descriptives**") %>% # update the column header
  bold_labels() %>%
  italicize_levels()%>%
  add_n( statistic = "{n}", col_label = "**n**", last = FALSE, footnote = FALSE
  )%>% # add column with total number of non-missing observations
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
  ) %>%
  modify_caption(caption = "") %>%
  modify_header(all_stat_cols() ~ "**{level}**, n = {n} ({style_percent(p)}%)") %>%
  as_flex_table() #covert gtsummary object to knitrkable object. also use as_flex_table() to maintain identation, footnotes, spanning headers

table_1

## Visualization

### Density plot of Age
plot1 <- ggplot(VP_Deaths1, 
                aes(x = vau_Age, colour = HDSS_Site)) +
  geom_density() +
  facet_wrap(~HDSS_Site,
             scales = "free") +
  labs(title = "Density plot for Age distribution", x = "Age")

plot1

ggsave(plot=plot1, height = 5, width = 9,
       filename = paste0("density_plot.png"), bg='white') 

### Stacked Bar plot - count


plot2 <- ggplot(data=VP_Deaths1, aes(x= HDSS_Site)) +
  geom_bar(aes(fill = vau_Age_group),
           position="fill", stat="count", show.legend = TRUE) +
  geom_text(aes(x= HDSS_Site, label = scales::percent(..count../sum(..count..)), group = vau_Age_group),
            stat = "count", position = position_fill())

plot2

### Stacked Bar plot - Age group identity

plot3 <- ggplot(data=VP_Deaths1 %>%
                  dplyr::select(HDSS_Site, vau_Age_group) %>%
                  group_by(HDSS_Site) %>%
                  mutate(total = n()) %>%
                  ungroup() %>%
                  group_by(HDSS_Site, vau_Age_group, total) %>%
                  summarise(count = n(), .groups = 'drop') %>%
                  mutate(prop = round(count/total, 3),
                         perc = scales::percent(prop, accuracy = .1)),
                aes(x= HDSS_Site, y= prop)) +
  geom_bar(aes(fill = vau_Age_group),
           position="stack", stat="identity", show.legend = TRUE) +
  scale_y_continuous(labels = scales::percent, n.breaks = 10) + 
  labs(title = "Death distribution by age group by site", fill = "Age group", 
       y = "Proportion")

plot3

plot4 <- ggarrange(plot1, plot3 + rremove("x.text"), 
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)





ggsave(plot=plot4, height = 5, width = 9,
       filename = paste0("Aaaaaaa.png"), bg='white') 


### Stacked Bar plot - Causes identity

plot4 <- ggplot(data=VP_Deaths1 %>%
                  dplyr::select(HDSS_Site, icod_1) %>%
                  group_by(HDSS_Site) %>%
                  mutate(total = n()) %>%
                  ungroup() %>%
                  group_by(HDSS_Site, icod_1, total) %>%
                  summarise(count = n(), .groups = 'drop') %>%
                  mutate(prop = round(count/total, 3),
                         perc = scales::percent(prop, accuracy = .1)),
                aes(x= HDSS_Site, y= prop)) +
  geom_bar(aes(fill = icod_1),
           position="stack", stat="identity", show.legend = TRUE) +
  geom_text(aes(x= HDSS_Site, y= prop, label = perc, group = icod_1), 
            position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent, n.breaks = 10) + 
  labs(title = "Leading causes of death by site")

plot4


ggsave(plot=plot4, height = 5, width = 9,
       filename = paste0("Causes_plot.png"), bg='white')


### Stacked Bar plot - Causes by age group identity

plot5 <- ggplot(data=VP_Deaths1 %>%
                  dplyr::select(HDSS_Site, icod_1, vau_Age_group) %>%
                  group_by(HDSS_Site, vau_Age_group) %>%
                  mutate(total = n()) %>%
                  ungroup() %>%
                  group_by(HDSS_Site, vau_Age_group, icod_1, total) %>%
                  summarise(count = n(), .groups = 'drop') %>%
                  mutate(prop = round(count/total, 3),
                         perc = scales::percent(prop, accuracy = .1)),
                aes(x= vau_Age_group, y= prop)) +
  geom_bar(aes(fill = icod_1),
           position="stack", stat="identity", show.legend = TRUE) +
  geom_text(aes(x= vau_Age_group, y= prop, label = perc, group = icod_1), 
            position = position_stack(vjust = 0.5)) +
  facet_wrap(~HDSS_Site) +
  scale_y_continuous(labels = scales::percent, n.breaks = 10) + 
  labs(title = "Causes of death by age groups", x = "Age group") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5


ggsave(plot=plot5, height = 5, width = 9,
       filename = paste0("Causes age group plot.png"), bg='white')


### Stacked Bar plot - Causes by gender identity

plot6 <- ggplot(data=VP_Deaths1 %>%
                  dplyr::select(HDSS_Site, icod_1, vau_gender) %>%
                  group_by(HDSS_Site, vau_gender) %>%
                  mutate(total = n()) %>%
                  ungroup() %>%
                  group_by(HDSS_Site, vau_gender, icod_1, total) %>%
                  summarise(count = n(), .groups = 'drop') %>%
                  mutate(prop = round(count/total, 3),
                         perc = scales::percent(prop, accuracy = .1)),
                aes(x= vau_gender, y= prop)) +
  geom_bar(aes(fill = icod_1),
           position="stack", stat="identity", show.legend = TRUE) +
  geom_text(aes(x= vau_gender, y= prop, label = perc, group = icod_1), 
            position = position_stack(vjust = 0.5)) +
  facet_wrap(~HDSS_Site) +
  scale_y_continuous(labels = scales::percent, n.breaks = 10)

plot6


ggsave(plot=plot6, height = 5, width = 9,
       filename = paste0("Causes gender plot.png"), bg='white')


### Line plot of total deaths

plot7 <- ggplot(data=VP_Deaths1, aes(x= vau_yeardeath1)) +
  geom_line(aes(colour = vau_gender),  stat="count", show.legend = TRUE) + 
  facet_wrap(~HDSS_Site, 
             scales = "free") + 
  labs(title = "Distribution of deaths by gender across years")


plot7















