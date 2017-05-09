dat = read.csv("lyrics_munged.csv")
eminem = dat[dat$artist == "eminem",]
gambino = dat[dat$artist == "childish-gambino",]

get_words = function(artist)
{
    artist = artist[!(artist$lyrics == ''),]
    words = scan(text = artist$lyrics, what='character', quote='')
    words = gsub('[^A-z]*', '', words)
    words = tolower(words)
    words = words[!(words == '')]
}

get_ngrams = function(words, n = 2)
{
    if(n == 0)
    {
        return(words)
    }
    return(get_ngrams(head(paste(words, words[n:length(words)]), -1), n = n-1))
}
