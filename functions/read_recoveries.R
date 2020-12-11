# read in recoveries that were downloaded.

read_recoveries <- function(){
  if(!file.exists("GatherData/recoveries.csv")){stop("Recoveries file not found. Please run download_recs() to create recoveries file ")}
  read_csv(
    "GatherData/recoveries.csv",
    col_types = cols(
      .default = col_guess(),
      sampled_run = "c",
      catch_sample_id = "c",
      weight = "d",
      sampled_sex = "c",
      run_year="d",
      sex="c",
      sequential_number="c",
      recovery_id="c",
      weight_code="c",
      sampled_length_range="c",
      sampled_mark="c"),
    progress=FALSE
  )
}
  
