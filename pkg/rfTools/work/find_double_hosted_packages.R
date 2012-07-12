
## Code to compute double hosted packages and their respective version numbers

avail_repos <- available.packages(contriburl = contrib.url(c(
                                    cran_url        = "http://CRAN.R-project.org",
                                    bioc_url        = "http://bioconductor.org/packages/release/bioc",
                                    bioc_data       = "http://bioconductor.org/packages/release/data/annotation",
                                    bioc_experiment = "http://bioconductor.org/packages/release/data/experiment",
                                    omega_hat_url   = "http://www.omegahat.org/R")))

avail_rforge <- available.packages(contriburl = contrib.url("http://r-forge.r-project.org"))
                                        #fields = c(tools:::.get_standard_repository_db_fields(), "Repository/R-Forge/Project"))

double_hosted <- rownames(avail_repos)[rownames(avail_repos) %in% rownames(avail_rforge)]

tab <- cbind(avail_repos[double_hosted, "Version"], avail_rforge[double_hosted, "Version"])
colnames(tab) <- c("CRAN et al.", "R-Forge")

## suppose foo depends on truncreg (CRAN: "0.1-1", R-Forge: "0.2-0")
## maintainer declares in DESCRIPTION: Depends truncreg (>= 0.2-0)

## double hosted packages outdated on R-Forge (possibly orphaned by the authors)
orphaned <- rownames(tab)[tab[,1] > tab[,2]]
orphaned

## TODO: find an automatic way to notify project admins about orphaned
## projects. We need to save some space on R-Forge and get computing
## time for free then.
## Notfied on 2012-07-12: project admins of a4, pdminblegen, survival
