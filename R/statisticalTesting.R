statisticalTesting <- function(df, categorial = FALSE) 
{
    if (!categorical) { # continuous variables
        if (!is.numeric(df$column_one)) {
            stop("Please provide continuous variables in column_one.")
        }

        nlevels_column_two = nlevels(factor(df$column_two))
        if (nlevels_column_two < 2) {
            stop("Column_two must have at least two levels.")
        }

        out_mean   = by(df$column_one, df$column_two, mean  , na.rm = TRUE)
        out_sd     = by(df$column_one, df$column_two, sd    , na.rm = TRUE)
        out_median = by(df$column_one, df$column_two, median, na.rm = TRUE)

        if (nlevels_column_two == 2) {
            zz <- list(mean = out_mean, sd = out_sd, median = out_median,
                t.test.p.value = .ttest(df),
                wilcox.p.value = .wilcox(df))
        } else if (nlevels_column_two > 2) {
            anova.object = .anova(df)
            zz <- list(mean = out_mean, sd = out_sd, median = out_median,
                anova.p = anova.object$p.value,
                tuckey.p = anova.object$tuckey.p.value)
        } 

        class(zz) <- "Continuous variables"
        zz
    } else { # categorical variables
        nlevels_column_one = nlevels(factor(df$column_one))
        nlevels_column_two = nlevels(factor(df$column_two))

        if (nlevels_column_one < 2 || nlevels_column_two < 2 || is.numeric(df$column_one) ||
            is.numeric(df$column_two)) {
                stop("Please provide discrete variables or at least two levels.")
        }

        cochran_armitage_trand.p.value = "Not applicable"
        odds.ratio = "Not applicable"
        odds.ratio.95CI = "Not applicable"

        if (nlevels_column_one * nlevels_column_two == 6) {
            cochran_armitage_trand.p.value = .catt(df)
        }

        if (nlevels_column_one == 2 && nlevels_column_two == 2) {
            odds.ratio.object = .odds.ratio(df)
            odds.ratio = odds.ratio.object$OR
            odds.ratio.95CI = odds.ratio.object$OR_95CI
        }

        zz <- list(table = table(df),
            fisher.p.value = .fisher(df),
            chisq.p.value = .chisq(df),
            cochran.armitage.trend.p.value = cochran_armitage_trand.p.value,
            odds.ratio = odds.ratio,
            odds.ratio.95CI = odds.ratio.95CI)

        class(zz) <- "Categorical vriables"
        zz
    }
}

## Continuous variables
.ttest <- function(df) {
    obj <- try(t.test(column_one ~ column_two, data=df, var.equal=FALSE),
        silent=TRUE)
    if (is(obj, "try-error")) {
        value <- NA
    } else {
        value <- obj$p.value
    }
    return(value)
}

.wilcox <- function(df) {
    obj <- try(wilcox.test(column_one ~ column_two, data=df, var.equal=FALSE),
        silent=TRUE)
    if (is(obj, "try-error")) {
        value <- NA
    } else {
        value <- obj$p.value
    }
    return(value)
}

.anova <- function(df) {
    obj <- try(aov(column_one ~ column_two, data=df), silent=TRUE)
    if(is(obj, "try-error")) {
        value <- NA
    }else{
        value <- summary(obj)[[1]][1,5]
        tuckey <- data.frame(TukeyHSD(obj)$column_two)
        adjusted_p <- tuckey["p.adj"]
    }
    return(list(p.value = value, tuckey.p.value = adjusted_p))
}

## Categorical variables
.fisher <- function(df) {
    obj <- try(fisher.test(table(df)), silent=TRUE)
    if (is(obj, "try-error")) {
        value <- NA
    } else {
        value <- obj$p.value
    }
    return(value)
}

.chisq <- function(df) {
    obj <- try(chisq.test(table(df)), silent=TRUE)
    if (is(obj, "try-error")) {
        value <- NA
    } else {
        value <- obj$p.value
    }
    return(value)
}

# 2 X 2 matrix
.odds.ratio <- function(df, conf.level = 0.95){
    x = as.matrix(table(df))
    rowsums <- rowSums(x)
    p1 <- x[1, 1] / rowsums[1]
    p2 <- x[2, 1] / rowsums[2]
    o1 <- p1 / (1 - p1)
    o2 <- p2 / (1 - p2)
    OR <- o2 / o1
    log.OR <- log(OR)
    SE.log.OR <- sqrt(sum(1/x))
    crit <- qnorm((1 - conf.level)/2, lower.tail = FALSE)
    log.lower <- log.OR - crit * SE.log.OR
    log.upper <- log.OR + crit * SE.log.OR
    lower <- exp(log.lower)
    upper <- exp(log.upper)
    return(list(OR = OR, OR_95CI = paste0(format(round(lower, 2), nsmall = 2), 
        "-", format(round(upper, 2), nsmall = 2))))

}

# 2 X 3 matrix
.catt <- function(df) {
    x = as.matrix(table(df))
    obj <- try(DescTools::CochranArmitageTest(x), silent=TRUE)
    if (is(obj, "try-error")) {
        value <- NA
    } else {
        value <- obj$p.value
    }
    return(value)
}
