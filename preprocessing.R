# Title     : TODO
# Objective : TODO
# Created by: lukas
# Created on: 13/05/2020

# read the data
data <- read_speeches('data/database_export_search_89.csv')
d_19 <- filter_period(data, 19)

# read serialized results
res <- unserialize_results_text('data/long_result.txt')

# run wordfish for coal counts
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=19, max_period=19)
tdmat <- tdmat_coal_counts(data=filtered_data,
                           min_period=19, # only use period 19, so set both
                           max_period=19, # min and max to 19
                           concat_mode=1, # concat by speaker
                           coal_thresh=3, # filter out all speeches with less than 3 mentions of Kohle (or similar)
                           sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=32, # representative for position 1
                    repr_2=6,  # representative for position 2
                    name='speakers_cc_Kotre_Beutin_p19_t3',
                    tol=1e-7)
speaker_speeches_by_party(raw=filtered_data, res=res, filename='data/speakers_cc_Kotre_Beutin_p19_t3.png')


# run wordfish for coal percentages
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=19, max_period=19)
tdmat <- tdmat_coal_percent(data=filtered_data,
                            min_period=19, # only use period 19, so set both
                            max_period=19, # min and max to 19
                            concat_mode=1, # concat by speaker
                            coal_thresh=3, # filter out all speeches with less than 3 mentions of Kohle (or similar)
                            sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=32, # representative for position 1
                    repr_2=6,  # representative for position 2
                    name='speakers_cp_Wirth_Movassat_p19_t0_005',
                    tol=1e-7)
speaker_speeches_by_party(raw=filtered_data, res=res, filename='data/speakers_cp_Wirth_Movassat_p19_t0_005.png')


# run wordfish for periods 17-19 without any specific filtering, grouped by party
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=17, max_period=19)
tdmat <- tdmat_regular(data=filtered_data,
                       min_period=17,
                       max_period=19,
                       concat_mode=2, # concat by party
                       sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=7, # representative for position 1
                    repr_2=9, # representative for position 2
                    name='parties_r_green18_cdu18_p17_19',
                    tol=1e-7)
party_speeches_by_party(raw=filtered_data, res=res, filename='data/parties_r_green18_cdu18_p17_19.png', TRUE)


# run wordfish for period 19 without any specific filtering, grouped by speaker
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=19, max_period=19)
tdmat <- tdmat_regular(data=filtered_data,
                            min_period=19,
                            max_period=19,
                            concat_mode=1, # concat by speaker
                            sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=46, # representative for position 1
                    repr_2=30, # representative for position 2
                    name='speakers_r_Kießling_Roth_p19',
                    tol=1e-7)
draw_eiffel_tower_diagram(res=res, filename='data/speakers_r_Kießling_Roth_p19.png')

# run wordfish for periods 17-19 on only the text surrounding coal, grouped by party
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=17, max_period=19)
tdmat <- tdmat_coal_sections_only(data=filtered_data,
                                  min_period=17,
                                  max_period=19,
                                  concat_mode=2, # concat by party
                                  words_around=100,
                                  sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=7, # representative for position 1
                    repr_2=9, # representative for position 2
                    name='parties_co_green18_cdu18_p17_19_wa_100',
                    tol=1e-7)
party_speeches_by_party(raw=filtered_data, res=res, filename='data/parties_co_green18_cdu18_p17_19_wa_100.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/parties_co_green18_cdu18_p17_19_wa_100_words.png')


# run wordfish for periods 17-19 on only the text surrounding coal, grouped by party
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=17, max_period=19)
tdmat <- tdmat_coal_sections_only(data=filtered_data,
                                  min_period=17,
                                  max_period=19,
                                  concat_mode=2, # concat by party
                                  words_around=200,
                                  sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=7, # representative for position 1
                    repr_2=9, # representative for position 2
                    name='parties_co_green18_cdu18_p17_19_wa_200',
                    tol=1e-7)
party_speeches_by_party(raw=filtered_data, res=res, filename='data/parties_co_green18_cdu18_p17_19_wa_200.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/parties_co_green18_cdu18_p17_19_wa_200_words.png')


# run wordfish for periods 17-19 on only the text surrounding coal, grouped by party
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=17, max_period=19)
tdmat <- tdmat_coal_sections_only(data=filtered_data,
                                  min_period=17,
                                  max_period=19,
                                  concat_mode=2, # concat by party
                                  words_around=50,
                                  sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=7, # representative for position 1
                    repr_2=9, # representative for position 2
                    name='parties_co_green18_cdu18_p17_19_wa_50',
                    tol=1e-7)
party_speeches_by_party(raw=filtered_data, res=res, filename='data/parties_co_green18_cdu18_p17_19_wa_50.png', TRUE)
draw_eiffel_tower_diagram(res=res, filename='data/parties_co_green18_cdu18_p17_19_wa_50_words.png')


# run wordfish for period 19 on only the text surrounding coal, grouped by speaker
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=19, max_period=19)
tdmat <- tdmat_coal_sections_only(data=filtered_data,
                                  min_period=19,
                                  max_period=19,
                                  concat_mode=1, # concat by speaker
                                  words_around=100,
                                  sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=72, # representative for position 1
                    repr_2=163, # representative for position 2
                    name='speakers_co_Hofreiter_Hollnagel_p19_wa_100',
                    tol=1e-7)
speaker_speeches_by_party(raw=filtered_data, res=res, filename='data/speakers_co_Hofreiter_Hollnagel_p19_wa_100.png')
draw_eiffel_tower_diagram(res=res, filename='data/speakers_co_Hofreiter_Hollnagel_p19_wa_100_words.png')


# run wordfish for period 19 on only the text surrounding coal, grouped by speaker
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=19, max_period=19)
tdmat <- tdmat_coal_sections_only(data=filtered_data,
                                  min_period=19,
                                  max_period=19,
                                  concat_mode=1, # concat by speaker
                                  words_around=200,
                                  sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=72, # representative for position 1
                    repr_2=163, # representative for position 2
                    name='speakers_co_Hofreiter_Hollnagel_p19_wa_200',
                    tol=1e-7)
speaker_speeches_by_party(raw=filtered_data, res=res, filename='data/speakers_co_Hofreiter_Hollnagel_p19_wa_200.png')
draw_eiffel_tower_diagram(res=res, filename='data/speakers_co_Hofreiter_Hollnagel_p19_wa_200_words.png')


# run wordfish for period 19 on only the text surrounding coal, grouped by speaker
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=19, max_period=19)
tdmat <- tdmat_coal_sections_only(data=filtered_data,
                                  min_period=19,
                                  max_period=19,
                                  concat_mode=1, # concat by speaker
                                  words_around=50,
                                  sparse=0.9999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=72, # representative for position 1
                    repr_2=163, # representative for position 2
                    name='speakers_co_Hofreiter_Hollnagel_p19_wa_50',
                    tol=1e-7)
speaker_speeches_by_party(raw=filtered_data, res=res, filename='data/speakers_co_Hofreiter_Hollnagel_p19_wa_50.png')
draw_eiffel_tower_diagram(res=res, filename='data/speakers_co_Hofreiter_Hollnagel_p19_wa_50_words.png')

# run wordfish for period 19 on only the text that doesn't include coal, grouped by party
filtered_data <- load_periods('data/database_export_search_89.csv', min_period=19, max_period=19)

#### START TESTING: DELETE LATER
without_coal <- get_segments_without_coal(filtered_data, 100)
only_coal <- get_only_coal_segments(filtered_data, 100)
#### END TESTING

tdmat <- tdmat_without_coal_sections(data=filtered_data,
                                  min_period=19,
                                  max_period=19,
                                  concat_mode=2, # concat by party
                                  words_around=100,
                                  sparse=0.999)
res <- run_wordfish(tdmat=tdmat,
                    repr_1=72, # representative for position 1
                    repr_2=163, # representative for position 2
                    name='speakers_wc_pol1_pol2_p19_wa_100',
                    tol=1e-7)
speaker_speeches_by_party(raw=filtered_data, res=res, filename='data/speakers_wc_pol1_pol2_p19_wa_100.png')
draw_eiffel_tower_diagram(res=res, filename='data/speakers_wc_pol1_pol2_p19_wa_100_words.png')

