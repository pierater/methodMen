# the purpose of this is to determine the difference between a song and an artist's profile

# [0, 1] feature scaling

scale = function(x) {
    (x - min(x))/(max(x) - min(x))
}

# euclidian distance

edist = function(profile, song) {
    sqrt(sum((profile - song)^2))
}

# fill for missing n-grams

fill = function(profile, song) {
    for(i in names(profile)) {
        if(is.na(song[i])) {
            song[i] = 0
        }
    }

    for(i in names(song)) {
        if(is.na(profile[i])) {
            profile[i] = 0
        }
    }

    return(list(profile, song))
}
# profile stores values for each n-gram to represent an artist
# song is essentially the same for a single song

profile0 = table(sample(1:5, 100, prob=c(.4, .2, .2, .12, .08), replace=TRUE))
profile1 = table(sample(3:9, 100, prob=c(.2, .15, .15, .3, .06, .04, .1), replace=TRUE))
song = table(sample(c(1:4, 7, 8), 20, prob=c(.3, .3, .1, .1, .1, .1), replace=TRUE))


profile0 = scale(profile0)
profile1 = scale(profile1)
song = scale(song)

x = fill(profile0, song)
y = fill(profile1, song)
if(edist(x[[1]], x[[2]]) < edist(y[[1]], y[[2]])) {
    print('The song is by artist0')
} else {
    print('The song is by artist1')
}
