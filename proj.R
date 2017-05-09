dat = read.csv("lyrics.csv")

#reduce to only Hip-Hop

dat = dat[dat$genre == "Hip-Hop",]

# remove unessesary cols
dat$index = NULL
dat$year = NULL
dat$genre = NULL

# only work with top 100 artists (via data set)

head(sort(table(dat$artist), decreasing=TRUE))

top = names(head(sort(table(dat$artist), decreasing=TRUE), 100))
artists = dat$artist

# remove all but the top 100 artists

dat = dat[is.element(artists, top),]

write.csv(file="lyrics_munged.csv", x=dat)

