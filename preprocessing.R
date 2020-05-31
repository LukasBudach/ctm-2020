# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 13/05/2020

# read the data
data <- read_speeches('data/database_export_search_89.csv')
d_19 <- filter(data, 'p', min_period=19, max_period=19)


# read serialized results
res <- unserialize_results_text('data/speaker_p_17_19_co_200_Hofreiter_Hollnagel.txt')


# run wordfish for coal counts
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)     # filter to period 19 only
data <- filter(data, 'cc', threshold=3)                     # filter by coal count for >= 3 times coal per speech
visualization_copy <- data
data <- group_speeches(data, 'speaker')                    # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)                 # create the TDM
res <- run_wordfish(tdmat=mat,
                    repr_1=32, # representative for position 1
                    repr_2=6,  # representative for position 2
                    name='speaker_p_19_cc_3_Kotre_Beutin',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_19_cc_3_Kotre_Beutin.png')


# run wordfish for coal percentages
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=19, max_period=19)     # filter to period 19 only
data <- filter(data, 'cp', threshold=0.005)                 # filter by coal percentage >= 0.005 (0.5%)
visualization_copy <- data
data <- group_speeches(data, 'speaker')                    # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)                 # create the TDM
res <- run_wordfish(tdmat=mat,
                    repr_1=30,    # representative for position 1
                    repr_2=46,    # representative for position 2
                    name='speaker_p_19_cp_0005_Movassat_Wirth',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_19_cp_0005_Movassat_Wirth.png')


# run wordfish for periods 17-19 without any specific filtering, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
visualization_copy <- data
data <- group_speeches(data, 'party', multiple_periods=TRUE)   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)                     # create the TDM
res <- run_wordfish(tdmat=mat,
                    repr_1=7,   # representative for position 1
                    repr_2=9,   # representative for position 2
                    name='party_p_17_19_green18_cdu18',
                    tol=1e-7)
# visualization
party_speeches_by_party(raw=visualization_copy, res=res, filename='data/party_p_17_19_green18_cdu18.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/party_p_17_19_green18_cdu18_words.png')


# run wordfish for periods 17-19 without any specific filtering, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)           # filter to period 17-19
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)   # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=46, # representative for position 1
                    repr_2=30, # representative for position 2
                    name='speaker_p_17_19_Kießling_Roth',
                    tol=1e-7)
# visualization
draw_eiffel_tower_diagram(res=res, filename='data/speaker_p_17_19_Kießling_Roth.png')

# run wordfish for period 19 without any specific filtering, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)           # filter to period 17-19
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)   # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=164, # representative for position 1
                    repr_2=188, # representative for position 2
                    name='speaker_p_17_19_Hofreiter18_Laemmel18',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_17_19_Hofreiter18_Laemmel18.png', multiple_periods=TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/speaker_p_17_19_Hofreiter18_Laemmel18_words.png')


# run wordfish for periods 17-19 on only the text surrounding coal, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
data <- filter(data, 'co', chars_around=200)                    # filter speeches to contain only a 100 character space around the keyword coal
visualization_copy <- data
data <- group_speeches(data, 'party', multiple_periods=TRUE)   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999, stem_speeches=FALSE)
res <- run_wordfish(tdmat=mat,
                    repr_1=7, # representative for position 1
                    repr_2=9, # representative for position 2
                    name='party_p_17_19_co_200_cap_green18_cdu18',
                    tol=1e-7)
# visualization
party_speeches_by_party(raw=visualization_copy, res=res, filename='data/party_p_17_19_co_200_cap_green18_cdu18.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/party_p_17_19_co_200_cap_green18_cdu18_words.png')


# run wordfish for periods 17-19 on only the text surrounding coal, grouped by speaker
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=13, max_period=16)         # filter to period 17-19
data <- filter(data, 'co', chars_around=300)                    # filter speeches to contain only a 100 character space around the keyword coal
visualization_copy <- data
data <- group_speeches(data, 'party', multiple_periods=TRUE) # group the speeches by their speaker
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=1, # representative for position 1
                    repr_2=3, # representative for position 2
                    name='party_p_13_16_co_300_green13_cdu13',
                    tol=1e-7)
# visualization
party_speeches_by_party(raw=visualization_copy, res=res, filename='data/party_p_13_16_co_300_green13_cdu13.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/party_p_13_16_co_300_green13_cdu13_words.png')


# run wordfish for periods 17-19 on only the text that doesn't include coal, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
data <- filter(data, 'nc', chars_around=100)                    # filter speeches to exclude a 100 character space around the keyword coal
visualization_copy <- data
data <- group_speeches(data, 'speaker', multiple_periods=TRUE)   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=164, # representative for position 1
                    repr_2=188, # representative for position 2
                    name='speaker_p_17_19_nc_100_Hofreiter18_Laemmel18',
                    tol=1e-7)
# visualization
speaker_speeches_by_party(raw=visualization_copy, res=res, filename='data/speaker_p_17_19_nc_100_Hofreiter18_Laemmel18.png', multiple_periods=TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/speaker_p_17_19_nc_100_Hofreiter18_Laemmel18_words.png')


# run wordfish for periods 17-19 on only the text surrounding coal and having remove the parliament president, grouped by party
data <- read_speeches('data/database_export_search_89.csv')
data <- filter(data, 'p', min_period=17, max_period=19)         # filter to period 17-19
data <- filter(data, 'co', chars_around=150)                    # filter speeches to contain only a 100 character space around the keyword coal
data <- filter(data, 'np')                                      # remove speeches by the active Bundestagspräsident
visualization_copy <- data
data <- group_speeches(data, 'party', multiple_periods=TRUE)   # group the speeches by their party
mat <- get_frequency_matrix(data, sparse=0.9999)
res <- run_wordfish(tdmat=mat,
                    repr_1=7, # representative for position 1
                    repr_2=9, # representative for position 2
                    name='party_p_17_19_co_150_np_green18_cdu18',
                    tol=1e-7)
# visualization
party_speeches_by_party(raw=visualization_copy, res=res, filename='data/party_p_17_19_co_150_np_green18_cdu18.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/party_p_17_19_co_150_np_green18_cdu18_words.png')

