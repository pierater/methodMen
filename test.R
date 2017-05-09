dat = read.csv("lyrics_munged.csv", stringsAsFactors=FALSE)
eminem = dat[dat$artist == "eminem",]
gambino = dat[dat$artist == "childish-gambino",]

get_words = function(words)
{
    words = words[!(words == '')]
    words = scan(text = words, what='character', quote='')
    words = gsub('[^a-zA-Z]*', '', words)
    words = tolower(words)
    words = words[!(words == '')]
}

get_ngrams = function(words, n = 2)
{
    if(n == 1)
    {
        return(words)
    }
    return(get_ngrams(head(paste(words, words[n:length(words)]), -1), n = n-1))
}

get_top_grams = function(grams, n = 10)
{
    return(head(sort(table(grams), decreasing=TRUE), n))
}
