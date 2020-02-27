# load the libraries
libraries <- "plyr,reshape2,data.table,optparse,futile.logger,jsonlite,parallel,dunn.test,lsr,survival,openxlsx,lattice"
libraries <- strsplit(libraries, ",")[[1]]
for (name in libraries) {
  suppressPackageStartupMessages(library(name, character.only=T, warn.conflicts=F, quietly=T, verbose=F))
}

# create a function to write the worksheet
writeWorksheet <- function(wb, sheetname, data, hs1, withFilter=F) {
  # add the worksheet
  addWorksheet(wb, sheetname) # add a worksheet
  # write out the data with filtering and bold headers
  writeData(wb, sheetname, data, withFilter=withFilter, headerStyle=createStyle(textDecoration = "bold"))
  # auto resize the columns
  setColWidths(wb, sheetname, cols=1:ncol(data), widths="auto") # autofit the columns
}

# function to compute the number of compute nodes to use based on the job number
getNodeNumber <- function(jobs, nodes=0, jobsPerNode=1000) {
  # automatic determination
  if (nodes == 0) {
    cores <- ceiling(detectCores()/2)
    flog.debug(paste("Running automatic process determination with", cores*2, "detected cores"))
    nodes <- ceiling(jobs / jobsPerNode)
    # upper bound it to the number of cores / 2
    nodes <- ifelse(nodes > cores, cores, nodes)
  }
  flog.debug(paste("Using", nodes, "for computations."))
  return(nodes)
}

# function to generate test data
generateTestData <- function(numerical_column=10, numerical_func=runif, categorical_column=10, categorical_column_min=3, categorical_column_max=10, level2_column=10, n=100) {
  o <- data.frame(id=1:n)
  if (numerical_column > 0) {
    for (i in 1:numerical_column) {
      o[[sprintf("n_%d", i)]] <- numerical_func(n)
    }
  }
  if (categorical_column > 0) {
    for (i in 1:categorical_column) {
      o[[sprintf("c_%d", i)]] <- sample(paste("C", 1:sample(categorical_column_min:categorical_column_max, 1), sep=""), n, replace=T)
    }
  }
  if (level2_column > 0) {
    for (i in 1:level2_column) {
      o[[sprintf("l2_%d", i)]] <- sample(c("A", "B"), n, replace=T)
    }
  }
  return(o)
}

# function to compute the summary statistics
computeSummaryStatistics <- function(d, numerical_min_unique_number=10, categorical_max_unique_number=10, categorical_min_level_n=3) {
  if (is.data.table(d)) {
    d <- as.data.frame(d)
  }
  o <- data.frame(name=colnames(d), stringsAsFactors=F, row.names=colnames(d))
  o$type <- sapply(o$name, function(x) ifelse(is.numeric(d[, x]), "numeric", "categorical"))
  o$unique_n <- sapply(o$name, function(x) length(unique(d[!is.na(d[, x]), x])))
  o$n <- sapply(o$name, function(x) sum(!is.na(d[, x])))
  o$missing_fraction <- 1 - (o$n / nrow(d))
  o$mean <- sapply(rownames(o), function(x) if(o[x, "type"]=="numeric") mean(d[, x], na.rm=T) else NA)
  o$median <- sapply(rownames(o), function(x) if(o[x, "type"]=="numeric") median(d[, x], na.rm=T) else NA)
  o$min <- sapply(rownames(o), function(x) if(o[x, "type"]=="numeric") min(d[, x], na.rm=T) else NA)
  o$max <- sapply(rownames(o), function(x) if(o[x, "type"]=="numeric") max(d[, x], na.rm=T) else NA)
  o$sd <- sapply(rownames(o), function(x) if(o[x, "type"]=="numeric") sd(d[, x], na.rm=T) else NA)
  o$shapiro_wilk_normal_pvalue <- sapply(rownames(o), function(x) ifelse(o[x, "type"]=="numeric", tryCatch(shapiro.test(d[!is.na(d[, x]), x])$p.value, error=function(e) NA), NA))
  # use as numerical
  o$use_as_numeric <- ifelse(o$type=="numeric" & o$unique_n >= numerical_min_unique_number, "yes", "no")
  # use as categorical
  o$use_as_categorical <- ifelse((o$type=="categorical" & o$unique_n <= categorical_max_unique_number) | (o$type=="numeric" & o$unique_n <= categorical_max_unique_number), "yes", "no")
  # compute number of levels which have at least 3 values in them
  o$num_min_count_group <- sapply(rownames(o), function(x) {
    if (o[x, "use_as_categorical"]=="yes") {
      x1 <- table(d[, x])
      return(length(x1[x1>=categorical_min_level_n]))
    }
    else {
      return(NA)
    }
  })
  return(o)
}

# function to compute the density plot data
computeDensityPlotData <- function(d) {
  o <- list()
  for (name in colnames(d)) {
    fit <- tryCatch(density(d[, name], na.rm=T), error=function(e) NULL)
    if (!is.null(fit)) {
      o[[name]] <- list(x=fit$x, y=fit$y)
    }
  }
  return(o)
}

# function to compute a data.frame with all possible tests from 2 sets of names
computeTestCombinations <- function(...) {
  arguments <- list(...)
  if (length(arguments)!=2) {
    msg <- "This function needs exactly 2 vectors."
    flog.error(msg)
    stop(msg)
  }
  else {
    config <- list()
    x <- as.character(arguments[[1]])
    y <- as.character(arguments[[2]])
    # have labels
    if(is.null(names(arguments))) {
      config[["x"]] <- rep(x, length(y))
      config[["y"]] <- rep(y, each=length(x))
    }
    else if (all(labels(arguments)!="")) {
      config[[labels(arguments)[1]]] <- rep(x, length(y))
      config[[labels(arguments)[2]]] <- rep(y, each=length(x))
    }
    else {
      msg <- "Both arguments need to have labels or no labels at all."
      flog.error(msg)
      stop(msg)
    }
    config <- as.data.frame(config, stringsAsFactors=F)
    config <- config[config[, 1]!=config[, 2], ]
    return(config)
  }
}

# function to create a pairwise comparison contrast matrix given a factor
createPairwiseComparisonContrastMatrix <- function(f1) {
  if (is.factor(f1) && length(levels(f1))>1) {
    comparisons <- t(combn(1:length(levels(f1)), 2))
    o <- apply(comparisons, 1, function(x) {
      o1 <- rep(0, length(levels(f1)))
      o1[x[1]] <- -1
      o1[x[2]] <- 1
      return(o1)
    })
    rownames(o) <- levels(f1)
    colnames(o) <- as.character(apply(comparisons, 1, function(x) {
      return(sprintf("%s vs %s", levels(f1)[x[2]], levels(f1)[x[1]]))
    }))
    return(o)
  }
  else {
    stop("Input is either not a factor or it does not have at least 2 levels.")
  }
}

# function to take a config and test func and run it in a parallel fashion with argument matching in the config file
runParallelJobArgMatch <- function(d, config, jobFunc, nodes=0, jobsPerNode=1000, clusterLogfile="") {
  # start time
  stime <- as.integer(as.POSIXct(Sys.time()))
  # compute the number of nodes to use
  nodes <- getNodeNumber(nrow(config), nodes)
  # check that the config has all the arguments need for the jobFunc
  if(length(colnames(config))!=length(names(formals(jobFunc))) || all(sort(colnames(config))!=sort(names(formals(jobFunc))))) {
    msg <- sprintf("Arguments in the config %s does not match the job function %s", paste(colnames(config), collapse=","), paste(names(formals(jobFunc)), collapse=","))
    flog.error(msg)
    stop(msg)
  }
  results <- data.table()
  if (nrow(config)>0) {
    # prepare the list object for parallelization
    flog.debug(paste("Computing the process configs."))
    # create the node config
    nconfig <- lapply(1:nodes, function(x) {
      selected <- which(rep(seq(from=1, to=nodes), length.out=nrow(config))==x)
      return(list(
        data=d,
        wd=getwd(),
        jobFunc=jobFunc,
        config=config[selected, ]
      ))
    })
    # run jobFunc in memory
    flog.debug(paste("Running", nrow(config), "tests with", nodes, "processes."))
    cl <- makeCluster(nodes, type="FORK", outfile=clusterLogfile)
    results <- rbindlist(parLapply(cl, nconfig, function(x) {
    #results <- rbindlist(lapply(nconfig, function(x) {
      library(dunn.test)
      library(lsr)
      library(plyr)
      library(reshape2)
      library(data.table)
      o <- rbindlist(lapply(1:nrow(x$config), function(y) {
        fd <- list()
        for (i in names(formals(x$jobFunc))) {
          fd[[i]] <- x$data[[x$config[y, ][[i]]]]
        }
        o1 <- tryCatch(do.call(x$jobFunc, fd), error=function(e) return(NULL))
        if (!is.null(o1)) {
          o1 <- cbind(x$config[y, ], o1)
          return(o1)
        } else {
          return(list())
        }
      }), fill=T)
    }), fill=T)
    stopCluster(cl)
  }
  etime <- as.integer(as.POSIXct(Sys.time()))
  flog.debug(paste("Time taken:", etime - stime, "seconds"))
  flog.debug(paste("Time taken per job:", (etime - stime) / nrow(config) * 1000, "ms"))
  return(results)
}

# function to run chisquare tests
runChisquareTest <- function(x, y) {
  o <- list(test="Chisquare test", status="")
  x <- as.character(x)
  y <- as.character(y)
  # remove the NA
  selected <- !is.na(x) & !is.na(y)
  x <- x[selected]
  y <- y[selected]
  if(length(unique(x))>1 && length(unique(y))>1) {
    tr <- chisq.test(x, y)
    o$pvalue <- tr$p.value
    o$n <- length(x)
    o$df <- tr$parameter[[1]]
    o$chisq <- tr$statistic[[1]]
    o$status <- "Test ok."
    # check for perfect association
    o$perfect_association <- ifelse(all(sapply(unique(x), function(i) length(unique(y[x==i])))==1) && all(sapply(unique(y), function(i) length(unique(x[y==i])))==1), "yes", "no")
  }
  else {
    o$status <- "Insufficient number of levels in either group."
  }
  return(o)
}

# function to run fisher or chisquare tests
runFisherChisquareTest <- function(x, y) {
  o <- list(test="Fisher/Chisquare test", status="")
  x <- as.character(x)
  y <- as.character(y)
  # remove the NA
  selected <- !is.na(x) & !is.na(y)
  x <- x[selected]
  y <- y[selected]
  if(length(unique(x))>1 && length(unique(y))>1) {
    tr <- chisq.test(x, y)
    o$chisq_pvalue <- tr$p.value
    o$n <- length(x)
    o$df <- tr$parameter[[1]]
    o$chisq <- tr$statistic[[1]]
    o$status <- "Test ok."
    o$cramer_v <- cramersV(x, y)
    # check for perfect association
    o$perfect_association <- ifelse(all(sapply(unique(x), function(i) length(unique(y[x==i])))==1) && all(sapply(unique(y), function(i) length(unique(x[y==i])))==1), "yes", "no")
    tr <- tryCatch(fisher.test(x, y, workspace=1000000, hybrid=T, simulate.p.value=T), error=function(e) return(NULL))
    if (!is.null(tr)) {
      o$fisher_pvalue <- tr$p.value
      o$pvalue <- o$fisher_pvalue
      o$test_value <- "Fisher Exact"
    }
    else {
      o$pvalue <- o$chisq_pvalue
      o$test_value <- "Chisquare"
    }
  }
  else {
    o$status <- "Insufficient number of levels in either group."
  }
  return(o)
}

computeChisquareTable <- function(dv1, dv2) {
  dv1 <- as.character(dv1)
  dv2 <- as.character(dv2)
  o1 <- as.data.table(table(dv1, dv2))
  colnames(o1) <- c("dv1_level", "dv2_level", "observed")
  o1 <- merge(o1, o1[, .(dv1_total=sum(observed)), .(dv1_level)], by="dv1_level")
  o1 <- merge(o1, o1[, .(dv2_total=sum(observed)), .(dv2_level)], by="dv2_level")
  o1$dv1_fraction <- o1$dv1_total / sum(o1$observed)
  o1$dv2_fraction <- o1$dv2_total / sum(o1$observed)
  o1$exp_fraction <- o1$dv1_fraction * o1$dv2_fraction
  o1$expected <- o1$exp_fraction * sum(o1$observed)
  o1$obs_over_exp <- o1$observed / o1$expected
  o1 <- as.data.frame(o1[order(o1$obs_over_exp, decreasing=T), ])
  return(o1[, c("dv1_level", "dv2_level", "observed", "expected", "obs_over_exp")])
}

# function to get the table to present the results of the chisquare fisher test
getChisquareTable <- function(d, config) {
  results <- data.frame()
  if (!("dv1" %in% colnames(config)) && "x" %in% colnames(config)) {
    config <- rename(config, c(x="dv1", y="dv2"))
  }
  for (i in 1:nrow(config)) {
    dv1 <- config[i, ]$dv1
    dv2 <- config[i, ]$dv2
    o1 <- computeChisquareTable(d[[dv1]], d[[dv2]])
    o1$dv1 <- dv1
    o1$dv2 <- dv2
    results <- rbind(results, o1)
  }
  return(results)
}

# function to run Spearman Rank correlation
runSpearmanRankCorrelation <- function(x, y) {
  o <- list(test="Spearman Rank correlation", status="")
  if (is.numeric(x) & is.numeric(y)) {
    # remove the NA
    selected <- !is.na(x) & !is.na(y)
    x <- x[selected]
    y <- y[selected]
    # check that we have enough points (3 or more)
    if (length(x) > 2) {
      tr <- tryCatch(cor.test(x, y, method="spearman"), error=function(e) return(NULL))
      if (!is.null(tr)) {
        o$pvalue <- tr$p.value
        o$n <- length(x)
        o$rho <- tr$estimate[[1]]
        o$rho2 <- tr$estimate[[1]]**2
        o$status <- "Test ok."
        # if the rho2 == 1 then check for perfect association (>0.99 since spearman does not give 1)
        if (o$rho2>0.99) {
          # the data is the same
          if (all(x==y)) {
            o$perfect_association <- "yes"
          }
          else {
            tr2 <- tryCatch(cor.test(x, y, method="pearson"), error=function(e) return(NULL))
            if (!is.null(tr2)) {
              o$perfect_association <- ifelse((tr2$estimate[[1]]**2)==1, "yes", "no")
            } else {
              o$perfect_association <- "no"
            }
          }
        } else {
          o$perfect_association <- "no"
        }
      } else {
        o$status <- "Test failed."
      }
    } else {
      o$status <- "Insufficient number of data points."
    }
  } else {
    o$status <- "Non-numerical data found."
  }
  return(o)
}

# function to run Pearson correlation
runPearsonCorrelation <- function(x, y) {
  o <- list(test="Pearson correlation", status="")
  if (is.numeric(x) & is.numeric(y)) {
    # remove the NA
    selected <- !is.na(x) & !is.na(y)
    x <- x[selected]
    y <- y[selected]
    # check that we have enough points (3 or more)
    if (length(x) > 2) {
      tr <- tryCatch(cor.test(x, y, method="pearson"), error=function(e) return(NULL))
      if (!is.null(tr)) {
        o$pvalue <- tr$p.value
        o$n <- length(x)
        o$r <- tr$estimate[[1]]
        o$r2 <- tr$estimate[[1]]**2
        o$status <- "Test ok."
      }
      else {
        o$status <- "Test failed."
      }
    }
    else {
      o$status <- "Insufficient number of data points."
    }
  }
  else {
    o$status <- "Non-numerical data found."
  }
  return(o)
}

# function to run post hoc ANOVA t tests corrected by bonferroni
runAnovaPostHocTest <- function(x, y) {
  o <- list(test="ANOVA post hoc t test", status="")
  y <- as.character(y)
  # remove the NA
  selected <- !is.na(x) & !is.na(y)
  x <- x[selected]
  y <- y[selected]
  # remove insufficient levels
  selected <-  y %in% unique(y)[sapply(unique(y), function(x1) sum(y==x1)) >= 2]
  x <- x[selected]
  y <- y[selected]
  # check that we have enough levels
  if (length(unique(y))>1) {
    # get the combinations for the post hoc t tests
    tests <- t(combn(unique(y), 2))
    colnames(tests) <- c("group1", "group2")
    o <- data.frame()
    for (i in 1:nrow(tests)) {
      fit <- t.test(x[y==tests[i, "group1"]], x[y==tests[i, "group2"]])
      o <- rbind(o, data.frame(
        test="ANOVA post hoc t test",
        status="Test ok.",
        group1=tests[i, "group1"],
        group2=tests[i, "group2"],
        pvalue=fit$p.value,
        group1_mean=fit$estimate[[1]],
        group1_n=length(x[y==tests[i, "group1"]]),
        group2_mean=fit$estimate[[2]],
        group2_n=length(x[y==tests[i, "group2"]]),
        mean_diff=fit$estimate[[1]]-fit$estimate[[2]],
        ci_95_low=fit$conf.int[[1]],
        ci_95_high=fit$conf.int[[2]],
        tvalue=fit$statistic[[1]],
        stringsAsFactors=F
      ))
    }
    o$pvalue_adj <- p.adjust(o$pvalue, method="BH")
    return(o)
  }
  else {
    o$status <- "Insufficient number of data points."
  }
  return(o)
}

# function to run ANOVA
runAnovaTest <- function(x, y) {
  o <- list(test="ANOVA test", status="")
  y <- as.character(y)
  # remove the NA
  selected <- !is.na(x) & !is.na(y)
  x <- x[selected]
  y <- y[selected]
  # remove insufficient levels
  selected <-  y %in% unique(y)[sapply(unique(y), function(x1) sum(y==x1)) >= 2]
  x <- x[selected]
  y <- y[selected]
  # check that we have enough levels
  if (length(unique(y))>1) {
    tr <- summary(aov(x ~ y, data=data.frame(x=x, y=y)))
    o$pvalue <- tr[[1]][[5]][[1]]
    o$n <- length(x)
    o$status <- "Test ok."
    o$df <- tr[[1]][[1]][[1]]
    o$fvalue <- tr[[1]][[4]][[1]]
    # check for perfectly separating pair
    y1 <- y[order(x)]
    if (sum(sapply(2:length(y1), function(x) y1[x]!=y1[x-1]))==(length(unique(y1))-1)) {
      o$perfect_separation <- "yes"
    } else {
      o$perfect_separation <- "no"
    }
  }
  else {
    o$status <- "Insufficient number of data points."
  }
  return(o)
}

# function to run pairwise paired t tests, for y with >= 2 levels, run all pairwise combinations
# y <- c(rep(c("A", "B", "C"), 3), rep(c("A", "B"), 3), rep(c("A", "B", "C", "D"), 2)); x <- runif(length(y)); subject <- paste("D", c(rep(1:3, each=3), rep(4:6, each=2), rep(7:8, each=4)), sep="")
runPairedTTest <- function(x, y, subject) {
  d <- data.frame(x=x, y=as.character(y), subject=as.character(subject), stringsAsFactors=F)
  # remove the NA
  selected <- !is.na(d$x) & !is.na(d$y) & !is.na(d$subject)
  d <- d[selected, ]
  # check for more than one level after filtering
  if (length(unique(y)) < 2) {
    return(list(test="Pairwise paired t test", status="Insufficient levels for categorical variable."))
  }
  # generate the test combinations
  tests <- t(combn(sort(unique(y)), 2))
  # iterate through the tests
  o <- data.frame()
  for (i in 1:nrow(tests)) {
    g1 <- as.character(tests[i, 1])
    g2 <- as.character(tests[i, 2])
    d1 <- dcast(d[d$y %in% c(g1, g2), ], subject ~ y, value.var="x")
    d1 <- d1[!is.na(d1[[g1]]) & !is.na(d1[[g2]]), ]
    if (nrow(d1) >= 2) {
      fit <- t.test(d1[[g1]], d1[[g2]], paired=T)
      o <- rbind(o, data.frame(
        test="Pairwise paired t test",
        status="Test ok.",
        group1=g1,
        group2=g2,
        pvalue=fit$p.value,
        group1_mean=mean(d1[[g1]]),
        group1_n=length(d1[[g1]]),
        group2_mean=mean(d1[[g2]]),
        group2_n=length(d1[[g2]]),
        n=length(d1[[g1]])+length(d1[[g2]]),
        mean_diff=fit$estimate[[1]],
        ci_95_low=fit$conf.int[[1]],
        ci_95_high=fit$conf.int[[2]],
        tvalue=fit$statistic[[1]],
        stringsAsFactors=F
      ))
    }
  }
  if (nrow(o) > 0) {
    return(o)
  } else {
    return(list(test="Pairwise paired t test", status="Insufficient number of data points."))
  }
}

# function to run post hoc Kruskal-Wallis Dunn's test corrected using bonferroni
runKruskalWallisDunnPostHocTest <- function(x, y) {
  o <- list(test="Kruskal-Wallis Dunn's post hoc test", status="")
  y <- as.character(y)
  # remove the NA
  selected <- !is.na(x) & !is.na(y)
  x <- x[selected]
  y <- y[selected]
  # remove insufficient levels
  selected <-  y %in% unique(y)[sapply(unique(y), function(x1) sum(y==x1)) >= 2]
  x <- x[selected]
  y <- y[selected]
  # check that we have enough levels
  if (length(unique(y))>1) {
    fit <- dunn.test(x, factor(y), method="bonferroni", kw=F, list=F, table=F)
    o <- data.frame(
      test="Kruskal-Wallis Dunn's post hoc test",
      status="Test ok.",
      group1=sapply(strsplit(fit$comparisons, " - ", fixed=T), function(x) x[1]),
      group2=sapply(strsplit(fit$comparisons, " - ", fixed=T), function(x) x[2]),
      chisq=fit$chi2,
      pvalue=fit$P * 2,
      zvalue=fit$Z,
      stringsAsFactors=F
    )
    o$pvalue_adj <- p.adjust(o$pvalue, "bonf")
    return(o)
  }
  else {
    o$status <- "Insufficient number of data points."
  }
  return(o)
}

# function to run Kruskal-Wallis tests
runKruskalWallisTest <- function(x, y) {
  o <- list(test="Kruskal-Wallis test", status="")
  y <- as.character(y)
  # remove the NA
  selected <- !is.na(x) & !is.na(y)
  x <- x[selected]
  y <- y[selected]
  # remove insufficient levels
  selected <-  y %in% unique(y)[sapply(unique(y), function(x1) sum(y==x1)) >= 2]
  x <- x[selected]
  y <- y[selected]
  # check that we have enough levels
  if (length(unique(y))>1) {
    tr <- kruskal.test(x, factor(y))
    o$pvalue <- tr$p.value
    o$n <- length(x)
    o$status <- "Test ok."
    o$df <- tr$parameter[[1]]
    o$chisq <- tr$statistic[[1]]
    # check for perfectly separating pair
    y1 <- y[order(x)]
    if (sum(sapply(2:length(y1), function(x) y1[x]!=y1[x-1]))==(length(unique(y1))-1)) {
      o$perfect_separation <- "yes"
    } else {
      o$perfect_separation <- "no"
    }
  }
  else {
    o$status <- "Insufficient number of data points."
  }
  return(o)
}

# run paired wilcoxon signed rank test
# y <- c(rep(c("A", "B", "C"), 3), rep(c("A", "B"), 3), rep(c("A", "B", "C", "D"), 2)); x <- runif(length(y)); subject <- paste("D", c(rep(1:3, each=3), rep(4:6, each=2), rep(7:8, each=4)), sep="")
runPairedWilcoxonTest <- function(x, y, subject) {
  tname <- "Wilcoxon signed rank test"
  d <- data.frame(x=x, y=as.character(y), subject=as.character(subject), stringsAsFactors=F)
  # remove the NA
  selected <- !is.na(d$x) & !is.na(d$y) & !is.na(d$subject)
  d <- d[selected, ]
  # check for more than one level after filtering
  if (length(unique(y)) < 2) {
    return(list(test=tname, status="Insufficient levels for categorical variable."))
  }
  # generate the test combinations
  tests <- t(combn(sort(unique(y)), 2))
  # iterate through the tests
  o <- data.frame()
  for (i in 1:nrow(tests)) {
    g1 <- tests[i, 1]
    g2 <- tests[i, 2]
    d1 <- dcast(d[d$y %in% c(g1, g2), ], subject ~ y, value.var="x")
    d1 <- d1[!is.na(d1[[g1]]) & !is.na(d1[[g2]]), ]
    if (nrow(d1) >= 2) {
      tr <- wilcox.test(d1[[g1]], d1[[g2]], paired=T)
      o <- rbind(o, data.frame(
        test=tname,
        pvalue=tr$p.value,
        n=length(d1[[g1]])+length(d1[[g2]]),
        status="Test ok.",
        w=tr$statistic[[1]],
        median1=median(d1[[g1]]),
        median2=median(d1[[g2]]),
        mean1=mean(d1[[g1]]),
        mean2=mean(d1[[g2]]),
        sd1=sd(d1[[g1]]),
        sd2=sd(d1[[g2]]),
        iqr1=IQR(d1[[g1]]),
        iqr2=IQR(d1[[g2]]),
        first_quartile1=quantile(d1[[g1]], 0.25)[[1]],
        first_quartile2=quantile(d1[[g2]], 0.25)[[1]],
        third_quartile1=quantile(d1[[g1]], 0.75)[[1]],
        third_quartile2=quantile(d1[[g2]], 0.75)[[1]],
        min1=min(d1[[g1]]),
        min2=min(d1[[g2]]),
        max1=max(d1[[g1]]),
        max2=max(d1[[g2]]),
        group1=tests[i, 1],
        group2=tests[i, 2],
        n1=length(d1[[g1]]),
        n2=length(d1[[g2]]),
        perfect_separation=ifelse(min(d1[[g1]]) > max(d1[[g2]]) || min(d1[[g2]]) > max(d1[[g1]]), "yes", "no"),
        stringsAsFactors=F
      ))
    }
  }
  if (nrow(o) > 0) {
    return(o)
  } else {
    return(list(test=tname, status="Insufficient number of data points."))
  }
}

# run wilcoxon rank sum test
runWilcoxonTest <- function(x, y) {
  o <- list(test="Wilcoxon rank sum (Mann-Whitney) test", status="")
  y <- as.character(y)
  # remove the NA
  selected <- !is.na(x) & !is.na(y)
  x <- x[selected]
  y <- y[selected]
  # remove insufficient levels
  selected <-  y %in% unique(y)[sapply(unique(y), function(x1) sum(y==x1)) >= 2]
  x <- x[selected]
  y <- y[selected]
  # check that we have enough levels
  if (length(unique(y))>1) {
    # get the combinations for the tests
    tests <- t(combn(sort(unique(y)), 2))
    colnames(tests) <- c("group1", "group2")
    o <- data.frame()
    for (i in 1:nrow(tests)) {
      g1 <- x[y==tests[i, "group1"]]
      g2 <- x[y==tests[i, "group2"]]
      tr <- wilcox.test(g1, g2)
      o <- rbind(o, data.frame(
        test="Wilcoxon rank sum (Mann-Whitney) test",
        pvalue=tr$p.value,
        n=length(g1)+length(g2),
        status="Test ok.",
        w=tr$statistic[[1]],
        median1=median(g1),
        median2=median(g2),
        mean1=mean(g1),
        mean2=mean(g2),
        sd1=sd(g1),
        sd2=sd(g2),
        iqr1=IQR(g1),
        iqr2=IQR(g2),
        first_quartile1=quantile(g1, 0.25)[[1]],
        first_quartile2=quantile(g2, 0.25)[[1]],
        third_quartile1=quantile(g1, 0.75)[[1]],
        third_quartile2=quantile(g2, 0.75)[[1]],
        min1=min(g1),
        min2=min(g2),
        max1=max(g1),
        max2=max(g2),
        group1=tests[i, "group1"],
        group2=tests[i, "group2"],
        n1=length(g1),
        n2=length(g2),
        perfect_separation=ifelse(min(g1) > max(g2) || min(g2) > max(g1), "yes", "no"),
        stringsAsFactors=F
      ))
    }
  }
  else {
    o$status <- "Insufficient number of data points."
  }
  return(o)
}

# run t test
runTTest <- function(x, y) {
  o <- list(test="t test", status="")
  y <- as.character(y)
  # remove the NA
  selected <- !is.na(x) & !is.na(y)
  x <- x[selected]
  y <- y[selected]
  # remove insufficient levels
  selected <-  y %in% unique(y)[sapply(unique(y), function(x1) sum(y==x1)) >= 2]
  x <- x[selected]
  y <- y[selected]
  # check that we have enough levels
  if (length(unique(y))>1) {
    # get the combinations for the tests
    tests <- t(combn(sort(unique(y)), 2))
    colnames(tests) <- c("group1", "group2")
    o <- data.frame()
    for (i in 1:nrow(tests)) {
      g1 <- x[y==tests[i, "group1"]]
      g2 <- x[y==tests[i, "group2"]]
      fit <- t.test(g1, g2)
      o <- rbind(o, data.frame(
        test="t test",
        status="Test ok.",
        group1=tests[i, "group1"],
        group2=tests[i, "group2"],
        pvalue=fit$p.value,
        group1_mean=fit$estimate[[1]],
        group1_n=length(g1),
        group2_mean=fit$estimate[[2]],
        group2_n=length(g2),
        n=length(g1)+length(g2),
        mean_diff=fit$estimate[[1]]-fit$estimate[[2]],
        ci_95_low=fit$conf.int[[1]],
        ci_95_high=fit$conf.int[[2]],
        tvalue=fit$statistic[[1]],
        perfect_separation=ifelse(min(g1) > max(g2) || min(g2) > max(g1), "yes", "no"),
        stringsAsFactors=F
      ))
    }
    return(o)
  }
  else {
    o$status <- "Insufficient number of data points."
  }
  return(o)
}

# run parallel t tests
runParallelTTest <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runTTest, nodes, jobsPerNode)
  if (nrow(o[status=="Test ok.", ]) > 0) {
    o <- merge(o, o[, .(x, pvalue_adj=p.adjust(pvalue, padjust)), .(y, group1, group2)], by=c("x", "y", "group1", "group2"))
  }
  return(o)
}

# run parallel wilcoxon tests
runParallelWilcoxonTest <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runWilcoxonTest, nodes, jobsPerNode)
  o <- merge(o, o[, .(x, pvalue_adj=p.adjust(pvalue, padjust)), .(y, group1, group2)], by=c("x", "y", "group1", "group2"))
  return(o)
}

# run parallel anova
runParallelAnovaTest <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runAnovaTest, nodes, jobsPerNode)
  o$pvalue_adj <- p.adjust(o$pvalue, padjust)
  o <- o[order(o$pvalue), ]
  return(o)
}

# run parallel anova post hoc tests
runParallelAnovaPostHocTest <- function(d, config, nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runAnovaPostHocTest, nodes, jobsPerNode)
  return(o)
}

# run parallel kruskal-wallis tests
runParallelKruskalWallisTest <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runKruskalWallisTest, nodes, jobsPerNode)
  o$pvalue_adj <- p.adjust(o$pvalue, padjust)
  o <- o[order(pvalue), ]
  return(o)
}

# run parallel kruskal-wallis post hoc tests
runParallelKruskalWallisPostHocTest <- function(d, config, nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runKruskalWallisDunnPostHocTest, nodes, jobsPerNode)
  return(o)
}

# run parallel paired t tests
runParallelPairedTTest <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runPairedTTest, nodes, jobsPerNode)
  o <- merge(o, o[, .(x, pvalue_adj=p.adjust(pvalue, padjust)), .(y, group1, group2)], by=c("x", "y", "group1", "group2"))
  return(o)
}

# run parallel paired wilcoxon tests
runParallelPairedWilcoxonTest <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runPairedWilcoxonTest, nodes, jobsPerNode)
  o <- merge(o, o[, .(x, pvalue_adj=p.adjust(pvalue, padjust)), .(y, group1, group2)], by=c("x", "y", "group1", "group2"))
  return(o)
}

# run parallel pearson correlations
runParallelPearsonCorrelation <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runPearsonCorrelation, nodes, jobsPerNode)
  o$pvalue_adj <- p.adjust(o$pvalue, padjust)
  o <- o[order(o$r2, decreasing=T), ]
  return(o)
}

# run parallel spearman correlations
runParallelSpearmanRankCorrelation <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runSpearmanRankCorrelation, nodes, jobsPerNode)
  o$pvalue_adj <- p.adjust(o$pvalue, padjust)
  o <- o[order(o$rho2, decreasing=T), ]
  return(o)
}

# run parallel fisher correlations
runParallelFisherChisquareTest <- function(d, config, padjust="BH", nodes=0, jobsPerNode=1000) {
  o <- runParallelJobArgMatch(d, config, runFisherChisquareTest, nodes, jobsPerNode)
  o$pvalue_adj <- p.adjust(o$pvalue, padjust)
  o <- o[order(pvalue), ]
  return(o)
}

# run all possible bivariate tests
runAllPossibleBivariateTests <- function(d, numerical_min_unique_number=10, categorical_max_unique_number=10, padjust="BH", nodes=0, jobsPerNode=1000, tests_to_run=c("spearman", "kruskal_wallis", "fisher")) {
  flog.info(sprintf("Running all possible bivariate tests on a table with %d rows and %d columns", nrow(d), ncol(d)))
  # compute the summary first
  flog.info("Running summary on the table")
  d.sum <- computeSummaryStatistics(d, numerical_min_unique_number, categorical_max_unique_number)
  flog.info(sprintf("%d numerical columns detected", nrow(d.sum[d.sum$use_as_numeric=="yes", ])))
  flog.info(sprintf("%d categorical columns detected", nrow(d.sum[d.sum$use_as_categorical=="yes" & d.sum$num_min_count_group>=2, ])))
  # compute the spearman next
  if ("spearman" %in% tests_to_run) {
    dv1s <- d.sum[d.sum$use_as_numeric=="yes", "name"]
    dv2s <- d.sum[d.sum$use_as_numeric=="yes", "name"]
    config <- computeTestCombinations(dv1s, dv2s)
    config <- config[config$x!=config$y & config$x<config$y, ]
    if (nrow(config) > 0) {
      flog.info(sprintf("Running %d Spearman rank correlations", nrow(config)))
      d.spearman <- runParallelSpearmanRankCorrelation(d, config, padjust, nodes, jobsPerNode)
      flog.info(sprintf("Spearman Rank correlations had %d nominal and %d multiple testing corrected results with rho2 >= 0.3", nrow(d.spearman[d.spearman$rho2>=0.3 & d.spearman$pvalue<0.05, ]), nrow(d.spearman[d.spearman$rho2>=0.3 & d.spearman$pvalue_adj<0.05, ])))
    }
    else {
      flog.info("No Spearman Rank correlations to run")
      d.spearman <- NULL
    }
  } else {
    flog.info("Spearman Rank correlations not requested")
    d.spearman <- NULL
  }
  # compute the kruskal wallis next
  if ("kruskal_wallis" %in% tests_to_run) {
    dv1s <- d.sum[d.sum$use_as_numeric=="yes", "name"]
    dv2s <- d.sum[d.sum$use_as_categorical=="yes" & d.sum$num_min_count_group>=2, "name"]
    config <- computeTestCombinations(dv1s, dv2s)
    if (nrow(config) > 0) {
      flog.info(sprintf("Running %d Kruskal-Wallis tests", nrow(config)))
      d.kruskal <- runParallelKruskalWallisTest(d, config, padjust, nodes, jobsPerNode)
      flog.info(sprintf("Kruskal-Wallis tests had %d nominal and %d multiple testing corrected results", nrow(d.kruskal[d.kruskal$pvalue<0.05, ]), nrow(d.kruskal[d.kruskal$pvalue_adj<0.05, ])))
      # run the dunn's post hoc
      d.dunn <- runParallelKruskalWallisPostHocTest(d, config, nodes, jobsPerNode)
      flog.info(sprintf("Kruskal-Wallis Dunn's post hoc tests had %d results", nrow(d.dunn[d.dunn$pvalue<0.05, ])))
    }
    else {
      flog.info("No Kruskal-Wallis tests to run")
      d.kruskal <- NULL
      d.dunn <- NULL
    }
  } else {
    flog.info("Kruskal-Wallis tests not requested")
    d.kruskal <- NULL
    d.dunn <- NULL
  }
  # compute the fisher last
  if ("fisher" %in% tests_to_run) {
    dv1s <- d.sum[d.sum$use_as_categorical=="yes" & d.sum$num_min_count_group>=2, "name"]
    dv2s <- d.sum[d.sum$use_as_categorical=="yes" & d.sum$num_min_count_group>=2, "name"]
    config <- computeTestCombinations(dv1s, dv2s)
    config <- config[config$x!=config$y & config$x<config$y, ]
    if (nrow(config) > 0) {
      flog.info(sprintf("Running %d Fisher's Exact tests", nrow(config)))
      d.fisher <- runParallelFisherChisquareTest(d, config, padjust, nodes, jobsPerNode)
      flog.info(sprintf("Fisher's Exact tests tests had %d nominal and %d multiple testing corrected results", nrow(d.fisher[d.fisher$pvalue<0.05, ]), nrow(d.fisher[d.fisher$pvalue_adj<0.05, ])))
    }
    else {
      flog.info("No Fisher's Exact tests to run")
      d.fisher <- NULL
    }
  } else {
    flog.info("Fisher's Exact tests not requested")
    d.fisher <- NULL
  }
  # return the results
  return(list(summary=d.sum, kruskal_wallis=d.kruskal, spearman=d.spearman, fisher=d.fisher, dunn=d.dunn))
}

# function to run PCA using a numerical matrix
runPCA <- function(x) {
  f <- prcomp(x)
  o <- list(
    components=as.data.frame(f$x),
    variance=data.frame(pc=1:length(f$sdev), variance=(f$sdev^2) / sum(f$sdev^2)),
    loadings=data.frame(f$rotation)
  )
  o$loadings$parameter <- rownames(f$rotation)
  return(o)
}

# results plotting functions

# plot the scatter plot
plotScatterplots <- function(filename, results, data, dv1, dv2, fString, fargs, best_fit_line=TRUE, by_rank=FALSE) {
  pdf(file=filename, width=8, height=6)
  for (i in 1:nrow(results)) {
    x <- data[[results[i, ][[dv1]]]]
    y <- data[[results[i, ][[dv2]]]]
    mlab <- do.call(sprintf, append(fString, lapply(fargs, function(x) results[i, ][[x]])))
    plot(x, y, main=mlab, xlab=results[i, ][[dv1]], ylab=results[i, ][[dv2]])
    if (best_fit_line) {
      abline(lm(y ~ x, data=data.frame(x=x,y=y)))
    }
    if (by_rank) {
      selected <- !is.na(data[[results[i, ][[dv1]]]]) & !is.na(data[[results[i, ][[dv2]]]])
      x <- rank(data[[results[i, ][[dv1]]]][selected])
      y <- rank(data[[results[i, ][[dv2]]]][selected])
      plot(x, y, main=mlab, xlab=sprintf("Rank of %s", results[i, ][[dv1]]), ylab=sprintf("Rank of %s", results[i, ][[dv2]]))
      if (best_fit_line) {
        abline(lm(y ~ x, data=data.frame(x=x,y=y)))
      }
    }
  }
  dev.off()
}

# plot the box plots
panel.mn <- function(x,y,...){
  panel.bwplot(x,y,pch="|",...)
}
plotBoxplots <- function(filename, results, data, dv, f1, fString, fargs, ylab=NULL) {
  pdf(file=filename, width=8, height=6)
  for (i in 1:nrow(results)) {
    fl <- as.formula(paste("`", results[i, ][[dv]], "` ~ `", results[i, ][[f1]], "`", sep=""))
    mlab <- do.call(sprintf, append(fString, lapply(fargs, function(x) results[i, ][[x]])))
    ylab <- ifelse(is.null(ylab), results[i, ][[dv]], ylab)
    p <- bwplot(fl, data=data, main=mlab, ylab=ylab, xlab=results[i, ][[f1]], panel=panel.mn, scales=list(cex=0.5, x=list(rot=45)), par.settings=list(box.rectangle=list(col="black"), box.umbrella=list(col="black"), plot.symbol=list(col="black")))
    print(p)
  }
  dev.off()
}

# plot the strip plots
plotStripplots <- function(filename, results, data, dv, f1, fString, fargs) {
  pdf(file=filename, width=8, height=6)
  for (i in 1:nrow(results)) {
    fl <- as.formula(paste("jitter(`", results[i, ][[dv]], "`) ~ factor(`", results[i, ][[f1]], "`)", sep=""))
    mlab <- do.call(sprintf, append(fString, lapply(fargs, function(x) results[i, ][[x]])))
    p <- lattice::dotplot(fl, data=data, main=mlab, ylab=results[i, ][[dv]], xlab=results[i, ][[f1]], jitter.x=T, groups=data[[results[i, ][[f1]]]], scales=list(cex=1, x=list(rot=45)))
    print(p)
  }
  dev.off()
}

# plot a dendrogram using ggdendro with a hclust object
plotDendrogram <- function(hdata, groups=NULL) {
  ddata <- dendro_data(hdata)
  if (!is.null(groups)) {
    # check that the groups is correct
    if (is.data.frame(groups) && "label" %in% colnames(groups) && "group" %in% colnames(groups)) {
      # check for all matches
      if (all(as.character(label(ddata)$label) %in% groups$label)) {
        lcolors <- as.character(groups$group)[match(as.character(label(ddata)$label), as.character(groups$label))]
        lmapping <- rainbow(length(unique(lcolors)))
        names(lmapping) <- sample(unique(lcolors), length(unique(lcolors)))
      } else {
        flog.error("Unable to locate all label values in groups")
        stop()
      }
    } else {
      flog.error("groups is not a data.frame or label and/or group is not found as columns")
      stop()
    }
  } else {
    lcolors <- "black"
    lmapping <- "#000000"
    names(lmapping) <- "black"
  }
  # the basic dendrogram
  p <- ggplot(segment(ddata)) + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
  # add the labels
  p <- p + scale_x_continuous(breaks=seq_along(ddata$labels$label), labels=ddata$labels$label)
  # add the little dot
  p <- p + geom_point(data=label(ddata), aes(x=x, y=y, col=lcolors))
  # set the theme stuff
  p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, colour=lmapping[match(lcolors, names(lmapping))]), panel.background=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.line.y=element_line(colour="black"), axis.ticks.x=element_blank())
  # add the legend
  p <- p + scale_color_manual(values=lmapping, name="colors")
  return(p)
}

# compute overlaps between 2 vectors and return a data.table for use
overlapTwoVectors <- function(x, y) {
  o <- data.table()
  o <- rbind(o, data.table(value=intersect(x, y), state="common"))
  o <- rbind(o, data.table(value=setdiff(x, y), state="unique to x"))
  o <- rbind(o, data.table(value=setdiff(y, x), state="unique to y"))
  return(o)
}

# run the survival log rank test
runSurvivalLogRankTest <- function(st, sc, f1) {
  o <- list(test="Survival log-rank test", status="Test ok.")
  # limit the analysis to the non-na values
  selected <- !is.na(st) & !is.na(sc) & !is.na(f1)
  st <- st[selected]
  sc <- sc[selected]
  f1 <- f1[selected]
  # run the test
  fit <- survdiff(Surv(st, sc) ~ f1, rho=0)
  o$chisq <- fit$chisq
  o$n <- length(st)
  o$pvalue <- pchisq(fit$chisq, length(fit$n)-1, lower.tail=F)
  return(o)
}
