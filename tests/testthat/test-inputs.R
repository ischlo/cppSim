#
# test_that("doubly constrained tested",{
#
#   ## error datas
#   error_dist <- cbind(cppSim::distance_test,1:ncol(cppSim::distance_test))
#
#   error_dist_neg <- cppSim::distance_test
#   error_dist_neg[100,150] <- -1
#
#   error_flow_neg <- cppSim::flows_test
#   error_flow_neg[543,57] <- -1
#   # errors
#   testthat::expect_error({
#     cppSim::run_model(flows = cppSim::flows_test
#                       ,distance = error_dist)
#   })
#
#   testthat::expect_error({
#     cppSim::run_model(flows = cppSim::flows_test
#                       ,distance = cppSim::distance_test
#                       ,beta = -1)
#   })
#
#   testthat::expect_error({
#     cppSim::run_model(flows=cppSim::flows_test
#                       ,distance = error_dist_neg#cppSim::distance_test
#                       )
#   })
#
#   testthat::expect_error({
#     cppSim::run_model(flows = error_flow_neg
#                       ,distance = cppSim::distance_test
#     )
#   })
#
#
#   #### Correct
#
#   testthat::expect_type({
#
#     cppSim::run_model(flows = cppSim::flows_test
#                       ,distance = cppSim::distance_test)
#
#   },"list")
#
# })
#
# test_that("singly constrained tested", {
#
#   ## error datas
#   error_dist <- cbind(cppSim::distance_test,1:ncol(cppSim::distance_test))
#
#   error_dist_neg <- cppSim::distance_test
#   error_dist_neg[100,150] <- -1
#
#   orig_flow <- error_flow_neg <- apply(cppSim::flows_test,FUN = sum,MARGIN = 1)
#
#   error_flow_neg[543] <- -1
#
#   # errors
#   # this test works locally, but does not work with checks...
#   # testthat::expect_error({
#   #   cppSim::run_model_single(flows = c(orig_flow,1,2)
#   #                            ,distance = error_dist)
#   # })
#
#   testthat::expect_error({
#     cppSim::run_model_single(flows = orig_flow
#                       ,distance = cppSim::distance_test
#                       ,beta = -1)
#   })
#
#   testthat::expect_error({
#     cppSim::run_model_single(flows = orig_flow
#                       ,distance = error_dist_neg #cppSim::distance_test
#     )
#   })
#
#   testthat::expect_error({
#     cppSim::run_model_single(flows = error_flow_neg
#                       ,distance = cppSim::distance_test
#     )
#   })
#
#   #### Correct
#
#   testthat::expect_type({
#
#     cppSim::run_model_single(flows = orig_flow
#                              ,distance = cppSim::distance_test)
#
#   },"list")
#
# })
