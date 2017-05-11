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

get_usage_of_grams_per_lyric = function(list) {
  l = c();
  for(r in 1:length(list)) {
    for (c in 1:length(list[[r]])) {
      word = names(list[[r]][c])
      if(is.null(l)) {
        l = c(1)
        names(l) = word
      } else if(is.na(l[word])) {
        l[word] = c(word)
        l[word] = 1
      } else {
        l[word] = sum(as.numeric(l[word]), 1)
      }
    }
  }
  return (l)
}

get_top_eighty = function(list) {
  countGrams = get_usage_of_grams_per_lyric(list)
  countGrams = sort(countGrams, decreasing = T)
  percentages = as.numeric(countGrams) / length(list)
  return (percentages)
}

normalize_list= function(percentages) {
    return ((percentages-min(percentages))/(max(percentages)-min(percentages)))
}

obj = generate_object(gambino, 40)


l = get_top_eighty(obj)

p = normalize_list(l)
