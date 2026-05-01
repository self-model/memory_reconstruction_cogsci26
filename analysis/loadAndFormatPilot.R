pilot_sym.df <- read.csv('../experiments/pilots/symmetric/data/jatos_results_batch2.csv',na.strings=c(""," ","NA")) %>%
  mutate(subj_id = substr(PROLIFIC_PID,1,10)) %>%
  mutate(subj_id = factor(subj_id)) %>%
  mutate(test_part=ifelse(test_part=='non-pretend','game',test_part))

pilot_asym.df <- read.csv('../experiments/pilots/asymmetric/data/jatos_results_batch1.csv',na.strings=c(""," ","NA")) %>%
  mutate(subj_id = substr(PROLIFIC_PID,1,10)) %>%
  mutate(subj_id = factor(subj_id)) %>%
  mutate(test_part=ifelse(test_part=='non-pretend','game',test_part))

pilot.df <- pilot_sym.df %>%
  mutate(condition='sym')%>%
  rbind(pilot_asym.df %>%
          mutate(condition='asym'))

pilot.click_df <- pilot.df %>%
  dplyr::select(condition,
                subj_id,
                test_part,
                grid_number,
                num_clicks,
                click_log) %>%
  mutate(click_log = gsub("\'","\"", click_log)) %>%
  filter(test_part=='game' | test_part=='memory') %>%
  group_by(subj_id) %>%
  mutate(round=1:n())

pilot.click_log <- data.frame(matrix(ncol=11,nrow=0,
                                     dimnames=list(NULL,
                                                   c("condition",
                                                     "subj_id",
                                                     "round",
                                                     "test_part",
                                                     "grid_number",
                                                     "num_clicks",
                                                     "i",
                                                     "j",
                                                     "hit",
                                                     "t",
                                                     "click_number"))))


for (row in 1:nrow(pilot.click_df)) {

  subject_click_log <- data.frame(Filter(function(x) length(x) > 0,fromJSON(pilot.click_df[row, ]$click_log))) %>%
    mutate(
      condition = pilot.click_df[row, ]$condition,
      click_number = 1:n(),
      subj_id = pilot.click_df[row, ]$subj_id,
      test_part = pilot.click_df[row, ]$test_part,
      grid_number = pilot.click_df[row, ]$grid_number,
      num_clicks = pilot.click_df[row, ]$num_clicks,
      round = pilot.click_df[row, ]$round
    )

  pilot.click_log <- rbind(pilot.click_log, subject_click_log);
}


pilot.click_log <- pilot.click_log %>%
  relocate(subj_id, .before = i) %>%
  relocate(round, .before = i) %>%
  relocate(test_part, .before=i) %>%
  relocate(grid_number, .before=i) %>%
  relocate(click_number, .before=i) %>%
  group_by(subj_id,test_part,grid_number) %>%
  mutate(RT=t-lag(t,default=0)) %>%
  mutate(square=i*10+j)
