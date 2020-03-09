context("Shiny app")
test_that("arguments parsed correctly", {
    expect_error(ovva_shiny(data_path = "blah"), "data_path must be a named character vector")
    expect_error(ovva_shiny(data_path = c(a = "blah")), "the directory 'blah' does not exist")
})
