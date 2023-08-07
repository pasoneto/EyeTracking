df = df %>%
  filter(Recording.name %in% matchedParticipants)

df = computeProportions(df)
