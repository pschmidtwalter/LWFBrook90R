context("replace_vec")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
  expect_equal(2 * 2, 4)
  expect_equal(2 * 2, 4)

})


# dput zur ausgabe von objekten

test_that("replacement in vectors works", {
  expect_equal(replace_vecelements(1:10, varnms=c("a1", "a3"), vals=0),
               c(0, 2, 0, 4, 5, 6, 7, 8, 9, 10))
  expect_equal(replace_vecelements(1:10, varnms=c("a1", "b3"), vals=0),
               c(0, 2, 0, 4, 5, 6, 7, 8, 9, 10))
  expect_equal(replace_vecelements(1:10, varnms=c("1", "3"), vals=0),
               c(0, 2, 0, 4, 5, 6, 7, 8, 9, 10))
  expect_error(replace_vecelements(1:10, varnms=c("a1", "b"), vals=0))


})
