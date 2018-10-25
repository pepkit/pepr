library(GenomicRanges)
library(BiocFileCache)
parseEncodeRegions = function(project) {
  # get the data from the Project config
  url = samples(project)$remote_url[[1]]
  sampleName = samples(project)$sample_name[[1]]
  fileName = samples(project)$file_name[[1]]
  workDir = config(project)$metadata$output_dir
  # create the download dir if it does not exist
  # download the file
  bfc = BiocFileCache(cache = tempdir(),ask = FALSE)
  path = bfcrpath(x = bfc, url)
  # read it in
  df=read.table(path)
  # formatting
  colnames(df) = c('chr', 'start', 'end', 'name')
  # convert to GRanges object
  GenomicRanges::GRanges(df)
}
