#summarize_covs.R

library(tidyverse)
library(plotrix)


# read in the covs --------------------------------------------------------
#this'll take some time and ram

#get file names
cov_file_list = list.files('bismark_data', '.cov.gz', full.names = TRUE)

#function to read in 
read_cov = function(cov_file){
  cdat = read_tsv(cov_file,
                  col_names = c('chr', 'pos1', 'pos2', 'pct_meth', 'nM', 'nU'))
  cdat$coverage = cdat$nM + cdat$nU
  cov_name = sub('bismark_data/', '', cov_file)
  cov_name = sub('.trim_bismark_bt2.bismark.cov.gz', '', cov_name)
  cdat$file = cov_name
  return(cdat)
}

#apply read_cov
cov_df_list = map(cov_file_list, read_cov)
map(cov_df_list, head)
adat = purrr::reduce(cov_df_list, rbind)
rm(cov_df_list)

# get summaries -------------------------------------------------------------

#per sample
summary = adat %>% 
  group_by(file) %>% 
  summarize(mn = mean(coverage),
            ste = std.error(coverage)) %>% 
  arrange(mn)

#overall
omn = mean(adat$coverage)
ose = std.error(adat$coverage)

#write out
overall = c('overall', omn, ose)
rbind(summary, overall) %>% 
  write_tsv('bismark_data/coverage_summary.tsv')
