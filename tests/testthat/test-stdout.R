context("stdout and stderr")

test_that("test output for std_out equals TRUE/FALSE", {
  skip_if_not(packageVersion("base") >= "3.2.2", "skipping capture.output tests")
  is_windows <- identical("windows", tolower(Sys.info()[["sysname"]]))
  string <- "hello world"
  if(is_windows){
    output1 <- capture.output(res <- exec_wait('cmd', c('/C', 'echo', string)))
    output2 <- capture.output(res <- exec_wait('cmd', c('/C', 'echo', string), std_out = FALSE))
    output3 <- capture.output(res <- exec_wait('cmd', c('/C', 'echo', string, ">&2"), std_out = FALSE), type = 'message')
    output4 <- capture.output(res <- exec_wait('cmd', c('/C', 'echo', string, ">&2"), std_out = FALSE, std_err = FALSE), type = 'message')
  } else {
    output1 <- capture.output(res <- exec_wait('echo', string))
    output2 <- capture.output(res <- exec_wait('echo', string, std_out = FALSE))
    command <- sprintf("echo %s >&2", string)
    output3 <- capture.output(res <- exec_wait("sh", c("-c", command)), type = 'message')
    output4 <- capture.output(res <- exec_wait("sh", c("-c", command), std_err = FALSE), type = 'message')
  }
  expect_equal(sub("\\W+$", "", output1), string)
  expect_equal(output2, character())
  expect_equal(sub("\\W+$", "", output3), string)
  expect_equal(output4, character())
})
