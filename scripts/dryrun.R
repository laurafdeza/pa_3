library(tidyverse)
library(knitr)

# load data
vowels <- read.csv("./data/vowel_data.csv")

# calculate means and sd of formant centroids
descript_vowels <- vowels %>%
  summarize(f1_mean = mean(f1_cent),
            f2_mean = mean(f2_cent),
            f1_sd = sd(f1_cent),
            f2_sd = sd(f2_cent),
            tl_mean = mean(tl),
            tl_sd = sd(tl)) 

# subset data to manipulate
mean_val <- descript_vowels[c(1, 2)]
sd_val <- descript_vowels[c(3, 4)]
tl_val <- descript_vowels[c(5, 6)]

# gather columns in formant subsets to create column "formant"
mean_val <- gather(mean_val, key = formant, value = mean, f1_mean, f2_mean)
sd_val <- gather(sd_val, key = formant, value = sd, f1_sd, f2_sd) 

# rename values inside formant
mean_val$formant[mean_val$formant == "f1_mean"] <- "f1"
mean_val$formant[mean_val$formant == "f2_mean"] <- "f2"

sd_val$formant[sd_val$formant == "f1_sd"] <- "f1"
sd_val$formant[sd_val$formant == "f2_sd"] <- "f2"

# merge formant data frames
centroids <- merge(mean_val, sd_val, by = "formant")

# gather columns in trajectory length to have measures in ms
tl_val <- gather(tl_val, key = tl, value = ms, tl_mean, tl_sd)

# rename values inside tl
tl_val$tl[tl_val$tl == "tl_mean"] <- "mean"
tl_val$tl[tl_val$tl == "tl_sd"] <- "sd"

# print all measures

# formants
knitr::kable(centroids, col.names = c("Formant",
                                      "Centroid mean",
                                      "Centroid S.D."))

# trajectory length
knitr::kable(tl_val, col.names = c("Trajectory length",
                                   "ms"))

# # since data duplicate, drop duplicated observations
# descript_vowels <- descript_vowels[ which(descript_vowels$formant == "f1_mean" & descript_vowels$formant_sd == "f1_sd"), ]
# descript_vowels <- descript_vowels[ which(descript_vowels$formant == "f2_mean" & descript_vowels$formant_sd == "f2_sd"), ]
# 
# # change name to sd columns so they can later merge into formant column
# colnames(descript_vowels)[1:2] <- c("f1", "f2")
# 
# # consider formant a factor instead of strings
# descript_vowels$formant <- as.factor(descript_vowels$formant)
# 
# # reorder columns so variables appear in order: formant, centroid mean, centroid sd
# vowels_print <- descript_vowels[, c(2, 1, 3)]


# Let's plot!
# reorder vowel values so plots imitate vowel space distribution
vowels$vowel <- factor(vowels$vowel,levels = c("i", "a", "u"))

# Trajectory length as a function of vowel and language
ggplot(vowels, aes(vowel, tl, color = language)) +
  geom_point() +
  labs(x = "Vowel",
       y = "Trajectory length (ms)") +
  scale_color_discrete(name = "Language", labels = c("English", "Spanish")) +
  ggtitle("Trajectory length as a function\nof vowel and language") +
  theme(plot.title = element_text(hjust=0.5))

# F1 as a function of vowel and language
ggplot(vowels, aes(vowel, f1_cent, color = language)) +
  geom_point() +
  labs(x = "Vowel",
       y = "F1 centroid (Hz)") +
  scale_color_discrete(name = "Language", labels = c("English", "Spanish")) +
  ggtitle("F1 as a function\nof vowel and language") +
  theme(plot.title = element_text(hjust=0.5))

# F2 as a function of vowel and language
ggplot(vowels, aes(vowel, f2_cent, color = language)) +
  geom_point() +
  labs(x = "Vowel",
       y = "F2 centroid (Hz)") +
  scale_color_discrete(name = "Language", labels = c("English", "Spanish")) +
  ggtitle("F2 as a function\nof vowel and language") +
  theme(plot.title = element_text(hjust=0.5))

# Plot spectral centroids in F1/F2 vowel space
ggplot(vowels, aes(f1_cent, f2_cent, color = vowel)) +
  geom_point() +
  labs(x = "Mean F1 centroid (Hz)",
       y = "Mean F2 centroid (Hz)") +
  scale_color_discrete(name = "Vowel") +
  ggtitle("Spectral centroids as a function of vowel") +
  theme(plot.title = element_text(hjust=0.5))

# Plot 4 no idea what is required

# Plot both vowel spaces together
vowel_means <- vowels %>% 
  group_by(vowel, language) %>% 
  summarize(f1_cent = mean(f1_cent), f2_cent = mean(f2_cent)) %>% 
  ungroup() %>% 
  mutate(order = case_when(vowel == "i" ~ 1, vowel == "a" ~ 2, TRUE ~ 3), 
         vowel = forcats::fct_reorder2(vowel, vowel, order)) %>% 
  arrange(order)

vowels %>% 
  mutate(vowel = forcats::fct_relevel(vowel, "u", "a", "i")) %>% 
  ggplot(., aes(x = f2_cent, y = f1_cent, color = language, label = vowel)) + 
  geom_text(size = 3.5, alpha = 0.6, show.legend = F) + 
  geom_path(data = vowel_means, aes(group = language, lty = language), 
            color = "grey") + 
  geom_text(data = vowel_means, show.legend = F, size = 7) + 
  scale_y_reverse() + 
  scale_x_reverse() + 
  scale_color_brewer(palette = "Set1") + 
  labs(title = "Vowel space comparison", 
       subtitle = "Spectral centroids of English/Spanish cardinal vowels", 
       y = "F1 (Hz)", x = "F2 (Hz)") + 
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust=0.5))

# -----------------------------------------------------------------------------





# Plot trajectory length in the F1/F2 vowel space -----------------------------

### mean f

vowel_f_means <- vowels %>%
  group_by(language, vowel) %>%
  summarize(f_20 = (mean(f1_20) + mean(f2_20))/2,
            f_35 = (mean(f1_35) + mean(f2_35))/2,
            f_50 = (mean(f1_50) + mean(f2_50))/2,
            f_65 = (mean(f1_65) + mean(f2_65))/2,
            f_80 = (mean(f1_80) + mean(f2_80))/2) %>%
  pivot_longer(c(f_20:f_80), names_to = "percent", values_to = "mean_hz") %>%
  ungroup()

# remove mn from values and create columns formant and %
vowel_f_clean <- data.frame(lapply(vowel_f_means, function(x) {gsub("f_", "", x)}))
vowel_f_clean$mean_hz <- as.numeric(as.character(vowel_f_clean$mean_hz))

vowel_f_clean %>%
  mutate(., vowel = forcats::fct_relevel(vowel, "i", "a", "u")) %>%
  group_by(vowel) %>%
  ggplot(., aes(percent, mean_hz, color = language)) +
  scale_y_continuous(breaks=seq(0,1600,200)) +
  geom_point() +
  facet_grid(. ~ vowel) +
  ggtitle("Trajectory length in mean F1/F2 vowel space") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Percent into trajectory length") +
  ylab("Mean Hz") +
  scale_color_discrete(name = "Language", labels = c("English", "Spanish"))




### f1

vowel_f1_means <- vowels %>%
  group_by(language, vowel) %>%
  summarize(f_20 = mean(f1_20),
            f_35 = mean(f1_35),
            f_50 = mean(f1_50),
            f_65 = mean(f1_65),
            f_80 = mean(f1_80)) %>%
  pivot_longer(c(f_20:f_80), names_to = "percent", values_to = "mean_hz") %>%
  ungroup()

# remove mn from values and create columns formant and %
vowel_f1_clean <- data.frame(lapply(vowel_f1_means, function(x) {gsub("f_", "", x)}))
vowel_f1_clean$mean_hz <- as.numeric(as.character(vowel_f1_clean$mean_hz))

vowel_f1_clean %>%
  mutate(., vowel = forcats::fct_relevel(vowel, "i", "a", "u")) %>%
  group_by(vowel) %>%
  ggplot(., aes(percent, mean_hz, color = language)) +
  scale_y_continuous(breaks=seq(0,1000,100)) +
  geom_point() +
  facet_grid(. ~ vowel) +
  ggtitle("Trajectory length in F1 vowel space") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Percent into trajectory length") +
  ylab("Mean Hz") +
  scale_color_discrete(name = "Language", labels = c("English", "Spanish"))



### f2

vowel_f2_means <- vowels %>%
  group_by(language, vowel) %>%
  summarize(f_20 = mean(f2_20),
            f_35 = mean(f2_35),
            f_50 = mean(f2_50),
            f_65 = mean(f2_65),
            f_80 = mean(f2_80)) %>%
  pivot_longer(c(f_20:f_80), names_to = "percent", values_to = "mean_hz") %>%
  ungroup()

# remove mn from values and create columns formant and %
vowel_f2_clean <- data.frame(lapply(vowel_f2_means, function(x) {gsub("f_", "", x)}))
vowel_f2_clean$mean_hz <- as.numeric(as.character(vowel_f2_clean$mean_hz))

vowel_f2_clean %>%
  mutate(., vowel = forcats::fct_relevel(vowel, "i", "a", "u")) %>%
  group_by(vowel) %>%
  ggplot(., aes(percent, mean_hz, color = language)) +
  scale_y_continuous(breaks=seq(800,2800,200)) +
  geom_point() +
  facet_grid(. ~ vowel) +
  ggtitle("Trajectory length in F2 vowel space") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("Percent into trajectory length") +
  ylab("Mean Hz") +
  scale_color_discrete(name = "Language", labels = c("English", "Spanish"))
