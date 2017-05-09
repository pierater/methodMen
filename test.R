dat = read.csv("lyrics_munged.csv", stringsAsFactors=FALSE)
eminem = dat[dat$artist == "eminem",]
gambino = dat[dat$artist == "childish-gambino",]

get_words = function(words)
{
    words = scan(text = words, what='character', quote='')
    words = gsub('[^a-zA-Z]*', '', words)
    words = tolower(words)
    words = words[!(words == '')]
}

get_top_grams = function(grams, n = 10)
{
    return(head(sort(table(grams), decreasing=TRUE), n))
}

get_bigrams = function(words)
{
    return(head(paste(words, words[2:length(words)]), -1))
}

generate_object = function(artist, n = 20)
{
    obj = list()
    all_lyrics = artist$lyrics
    for(i in 1:length(all_lyrics))
    {
        obj[[i]] = get_top_grams(get_bigrams(get_words(all_lyrics[i])), n)
    }
    return(obj)
}
