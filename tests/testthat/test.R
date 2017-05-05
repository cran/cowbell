library(cowbell)

test_that("Cowbell base functionality",
          {
            data("testA")
            concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1 , 7)
            test<-generateCowbell(concept, testA, 200)
            sum<-summary(test)
            expect_equal(sum$baseDefinition$minDepend, 1.96, tolerance=1e-2)
            expect_equal(sum$baseDefinition$maxDepend, 7.61, tolerance=1e-2)
            expect_equal(sum$baseDefinition$breakPointA, 4.36, tolerance=1e-2)
            expect_equal(sum$baseDefinition$breakPointB, 5.53, tolerance=1e-2)
            expect_equal(sum$baseDefinitionReduced$minDepend, 2.37, tolerance=1e-2)
            expect_equal(sum$baseDefinitionReduced$maxDepend, 9.0, tolerance=1e-2)
            expect_equal(sum$rSquared, 1.0, tolerance=1e-4)
            expect_equal(sum$rSquaredReduced, 0.93, tolerance=1e-2)
            expect_equal(sum$fstatistic$pval, 0.0, tolerance=1e-4)
            expect_equal(sum$fstatisticBreakpoint$pval, 0.0, tolerance=1e-4)
            expect_equal(residuals(test), rep(0.0, 1000), tolerance=1e-1)
          })

