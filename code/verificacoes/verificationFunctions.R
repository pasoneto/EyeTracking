dif = function(x){
  difs = c()
  for(i in 1:(length(x)-1)){
    difs = c(difs, x[i+1]-x[i])
  }
  difs = c(difs, NA)
  return(difs)
}

samplingRate = function(x){
  name = unique(x$Recording.name)
  x %<>%
    filter(!is.na(Presented.Stimulus.name)) %>%
    filter(!str_detect(Presented.Stimulus.name, 'Eyetracker Calibration|BL_|bichinhocolorido|bola|fadinha|MASK')) %>%
    group_by(Presented.Stimulus.name)

  if(NROW(x) > 1){
    indexing = fixationIndexer(x$Presented.Stimulus.name)
    x$Presented.Stimulus.name = paste(x$Presented.Stimulus.name, indexing, sep = "_")
    x %<>%
      mutate(d = dif(Computer.timestamp)) %>%
      filter(!is.na(d))
    x$Recording.name = name
    return(x)
  } else {
    return(NULL)
  }
}

timeConsistency = function(a){
  name = unique(a$Recording.name)
  a %<>%
    select(Recording.name, Event, Event.value, Computer.timestamp) %>%
    filter(Event %in% c("VideoStimulusStart", "VideoStimulusEnd")) %>%
    filter(!str_detect(Event.value, 'Eyetracker Calibration|BL_|bichinhocolorido|bola|fadinha'))

  if(NROW(a) > 1){
    indexing = fixationIndexer(a$Event.value)
    a$Event.value = paste(a$Event.value, indexing, sep = "_")

    a %<>%
      group_by(Event.value) %>%
      distinct(Event, .keep_all = TRUE) %>%
      summarise(time = max(Computer.timestamp) - min(Computer.timestamp),
                N = length(Computer.timestamp),
                Recording.name = unique(Recording.name))
    a$Recording.name = name
    return(a)
  } else{
    return(NULL)
  }
}
