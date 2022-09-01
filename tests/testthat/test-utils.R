test_that("utils work", {
  expect_true(
    is_gh_url(
      "https://github.com/milesmcbain/capsule/issues/10"
    )
  )
  expect_false(
    is_gh_url(
      "htptjpoasjdfjiojapsfdj1238903450.com"
    )
  )
  expect_false(
    is_gh_url(
      "htptjpoasjdfjiohttps://github.comjapsfdj1238903450.com"
    )
  )

  gh_issue_info <-
    extract_issue_info_from_gh_url(
      "https://github.com/milesmcbain/capsule/issues/10"
    )

    expect_equal(
      gh_issue_info$repo, "milesmcbain/capsule"
    )
    
    expect_equal(
      gh_issue_info$number, "10"
    )
})
