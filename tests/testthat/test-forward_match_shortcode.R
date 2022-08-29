test_that("forward match shortcode and hashref", {

  no_match <- "something else"

  match <- "some text `gh milesmcbain/datapasta#33` more text"

  two_match <- "some text `gh milesmcbain/datapasta#33` more text `gh milesmcbain/atcursor#1` yeah"

  expect_null(
    forward_match_shortcode(no_match, 4),
  )

  expect_equal(
    forward_match_shortcode(two_match, 2),
    "`gh milesmcbain/datapasta#33`"
  )

  expect_equal(
    forward_match_shortcode(two_match, 40),
    "`gh milesmcbain/atcursor#1`"
  )
  
  expect_null(
    forward_match_shortcode(two_match, 78),
  )

  expect_equal(
    forward_match_shortcode(match, 11),
    "`gh milesmcbain/datapasta#33`"
  )

  numbers <- "- Need to look at faceting section `gh hadley/r4ds#1035` whole game :soccer:"

  expect_equal(
    forward_match_shortcode(numbers, 10),
    "`gh hadley/r4ds#1035`"
  )

})
