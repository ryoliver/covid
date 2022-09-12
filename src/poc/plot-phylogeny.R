if(interactive()) {
  rm(list=ls())
  library(here)
  
  .wd <- '~/Documents/ucsb/repos/covid'
  .test <- TRUE
  rd <- here::here
  
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
  
} else {
  library(docopt)
  library(rprojroot)
  
  .wd <- '/gpfs/ysm/project/jetz/ryo3/projects/covid'
  .script <-  thisfile()
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .datPF <- file.path(.wd,'data/')
  .outPF <- file.path(.wd,'analysis/')
}

source(file.path(.wd,'/src/startup.r'))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)



suppressWarnings(
  suppressPackageStartupMessages({
    library(data.table)
    library(cowplot)
    library(lubridate)
    library(ape)
    library(tidyverse)
    library(ape)
    library(ggtree)
    library(phytools)
  }))



vertnet_tax <- fread("raw_data/vertlife_taxonomies.csv")

size <- fread("out/dbbmm_size.csv") %>% 
  mutate(
    # trt_new = gsub('_.*','',trt),
    year_f = factor(year),
    # trt_new = fct_relevel(trt_new, "pre.ld", "ld", "post.ld")
  )

summary  <- fread(paste0(.outPF, "species-individual-summary.csv")) %>%
  filter(taxon_canonical_name %in% size$species) %>%
  filter(!taxon_canonical_name %in% c("Alces", "Anser"))

# join species in dataset to the taxonomy
foc_sp <- vertnet_tax %>% 
  inner_join(summary, by = c("scientificname" = "taxon_canonical_name")) %>%
  mutate(scientificname = str_replace(scientificname, " ", "_"))

  

#read in trees
birds <- read.nexus("raw_data/tree-pruner-05469463-d432-42ac-a726-58ff39a822b8/output.nex")
birds <- birds$tree_4345
mammals <- read.nexus("raw_data/tree-pruner-baceaa54-30cf-4bdd-a253-a20897aa6496/output.nex")
mammals <- mammals$tree_2272
combphylo <- bind.tree(birds, mammals)


d <- data.frame("scientificname" = c(birds$tip.label, mammals$tip.label)) %>%
  left_join(., foc_sp) %>%
  select(scientificname, n_individuals) %>%
  mutate(n_individuals = as.numeric(n_individuals))

x <- d %>%
  select(n_individuals)
rownames(x) <- d$scientificname
x <- as.matrix(x)


data("mammal.data")
data(mammal.tree)
data(mammal.data)
## log-transform trait data
log.mammal<-log(mammal.data)
## plot dotTree
dotTree(mammal.tree,log.mammal,fsize=0.7,
        standardize=TRUE,length=10)

dotTree(mammal.tree, mammal.data)
mammal.tree$tip.label


dotTree(combphylo, x)

plotTree(combphylo, plot=TRUE)

plotTree(combphylo, plot=TRUE, type = "fan", ftype = "off")

