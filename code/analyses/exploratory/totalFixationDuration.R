#Calculates the total time spent looking at the screen by different conditions
source("/Users/pdealcan/Documents/github/sabara/code/utils.R")
source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsProportion.R")
source("/Users/pdealcan/Documents/github/sabara/code/analyses/cutoffsVideoDuration.R")

library("stringr")
directory = "/Users/pdealcan/Documents/github/dataSabara/processedParticipantFINAL/"

setwd(directory)

files = list.files()
file_list = lapply(files, function(i){
                   a = fread(i)
                   a = a %>% select(!V1)
                   return(a)
            }
)

a = bind_rows(file_list)
a = a %>% mutate(Gaze.event.duration = Recording.time.end - Recording.time.begin)
a = a %>% mutate(trialsToFilter = paste(Presented.Stimulus.name, Recording.name, sep = ""))

a$tea = unlist(lapply(a$Recording.name, tagDiagnostico))

#Filter out participants with long/short video durations, and participants without at least 50% fixation
listToFilter075 = c(filterOutNames075$filterOutTrials, a$filterOutDurations)
listToFilter05 = c(filterOutNames05$filterOutTrials, a$filterOutDurations)
listToFilter025 = c(filterOutNames025$filterOutTrials, a$filterOutDurations)

a075 = a %>% filter(!trialsToFilter %in% listToFilter075)
a05 = a %>% filter(!trialsToFilter %in% listToFilter05)
a025 = a %>% filter(!trialsToFilter %in% listToFilter025)

#Filter participants with at least one trial per condition
a075 = a075 %>% 
  mutate(condition = substr(Presented.Stimulus.name, 1, 3)) %>%
  filter(!str_detect(condition, 'BL_')) %>%
  group_by(Recording.name) %>%
  mutate(nConditions = length(unique(condition))) %>%
  filter(nConditions == 2)

a05 = a05 %>% 
  mutate(condition = substr(Presented.Stimulus.name, 1, 3)) %>%
  filter(!str_detect(condition, 'BL_')) %>%
  group_by(Recording.name) %>%
  mutate(nConditions = length(unique(condition))) %>%
  filter(nConditions == 2)

a025 = a025 %>% 
  mutate(condition = substr(Presented.Stimulus.name, 1, 3)) %>%
  filter(!str_detect(condition, 'BL_')) %>%
  group_by(Recording.name) %>%
  mutate(nConditions = length(unique(condition))) %>%
  filter(nConditions == 2)


a075 = checkFocus(a075)
a05 = checkFocus(a05)
a025 = checkFocus(a025)

#Compute total fixation duration
a075 = a075 %>%
  select("Recording.name", "Presented.Stimulus.name", "Gaze.event.duration", "focus", "target", "condition") %>%
  group_by(Recording.name, Presented.Stimulus.name) %>%
  mutate(totalFixation = sum(Gaze.event.duration)) %>%
  group_by(Recording.name, Presented.Stimulus.name, focus) %>%
  summarise(condition = unique(condition),
            target = unique(target),
            totalFixationVariable = sum(Gaze.event.duration),
            totalFixation = unique(totalFixation)) %>%
  group_by(Recording.name, Presented.Stimulus.name, focus) %>%
  summarise(totalFixationProportion = totalFixationVariable/totalFixation,
            condition = unique(condition))

a05 = a05 %>%
  select("Recording.name", "Presented.Stimulus.name", "Gaze.event.duration", "focus", "target", "condition") %>%
  group_by(Recording.name, Presented.Stimulus.name) %>%
  mutate(totalFixation = sum(Gaze.event.duration)) %>%
  group_by(Recording.name, Presented.Stimulus.name, focus) %>%
  summarise(condition = unique(condition),
            target = unique(target),
            totalFixationVariable = sum(Gaze.event.duration),
            totalFixation = unique(totalFixation)) %>%
  group_by(Recording.name, Presented.Stimulus.name, focus) %>%
  summarise(totalFixationProportion = totalFixationVariable/totalFixation,
            condition = unique(condition))

a025 = a025 %>%
  select("Recording.name", "Presented.Stimulus.name", "Gaze.event.duration", "focus", "target", "condition") %>%
  group_by(Recording.name, Presented.Stimulus.name) %>%
  mutate(totalFixation = sum(Gaze.event.duration)) %>%
  group_by(Recording.name, Presented.Stimulus.name, focus) %>%
  summarise(condition = unique(condition),
            target = unique(target),
            totalFixationVariable = sum(Gaze.event.duration),
            totalFixation = unique(totalFixation)) %>%
  group_by(Recording.name, Presented.Stimulus.name, focus) %>%
  summarise(totalFixationProportion = totalFixationVariable/totalFixation,
            condition = unique(condition))

a075 = a075 %>% 
  ungroup() %>%
  select(Recording.name, Presented.Stimulus.name, condition, focus, totalFixationProportion)

a05 = a05 %>% 
  ungroup() %>%
  select(Recording.name, Presented.Stimulus.name, condition, focus, totalFixationProportion)

a025 = a025 %>% 
  ungroup() %>%
  select(Recording.name, Presented.Stimulus.name, condition, focus, totalFixationProportion)

#Adding diagnosis
a075$tea = unlist(lapply(a075$Recording.name, tagDiagnostico))
a05$tea = unlist(lapply(a05$Recording.name, tagDiagnostico))
a025$tea = unlist(lapply(a025$Recording.name, tagDiagnostico))

#Descriptives
descriptive075 = a075 %>%
  distinct(Recording.name, .keep_all = TRUE)

descriptive05 = a05 %>%
  distinct(Recording.name, .keep_all = TRUE)

descriptive025 = a025 %>%
  distinct(Recording.name, .keep_all = TRUE)

table(descriptive075$tea)
table(descriptive05$tea)
table(descriptive025$tea)

#Getting descriptive table
a05 %>%
  ungroup() %>%
  group_by(tea, condition, focus) %>%
  summarise(sd = sd(totalFixationProportion, na.rm = TRUE),
            totalFixationProportion = mean(totalFixationProportion, na.rm = TRUE)) %>%
  dvisu()

#Visualization
a05 %>%
  ggplot(aes(x = focus, y = totalFixationProportion, fill = tea)) +
    facet_wrap(~condition)+
    geom_boxplot()+
    ylab("Proporção de fixação")+
    xlab("")

ggsave("/Users/pdealcan/Documents/github/sabara/reports/report17/distribution.png")
