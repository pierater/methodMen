source("test.R")
source("dist.R")

# gambino generation
gambino_obj = generate_object(gambino, 20)
top_gambino = get_top_eighty(gambino_obj)
# normalized = normalize_list(top_percent)

# eminem generation
eminem_obj = generate_object(eminem, 20)
top_eminem = get_top_eighty(eminem_obj)


# profile generation
gambino_profile = scale(top_gambino)
eminem_profile = scale(top_eminem)

# generate profile for single song from eminem
song = eminem[1,]
song_obj = generate_object(song, 20)
top_song = 


