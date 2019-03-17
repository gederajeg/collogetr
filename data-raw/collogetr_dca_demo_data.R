# codes to generate output data for DCA in README
my_leipzig_path <- collogetr::leipzig_corpus_path[1]
dca_coll <- collogetr::colloc_leipzig(leipzig_path = my_leipzig_path,
                                     pattern = c("memperkuat", "menguatkan"),
                                     window = "r",
                                     span = 1,
                                     save_interim = FALSE)

# save into package data
usethis::use_data(dca_coll, internal = FALSE)

