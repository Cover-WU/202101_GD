# exploratory data insight: ggplot to find hidden rules.
# for different scenario and PRS subscale: facet 4 by 4.
ggplot(data = aggregate_data_screen %>% gather(Escape:Compatibility, key = 'variable', value = 'Score'),
       aes(x = `Interpersonal Trust`, y = Score)) +
  geom_point(aes(color = `Level of Vitality`),  shape = 16, alpha = 0.4) +
  geom_smooth(aes(color = `Level of Vitality`), method = "lm") +
  facet_grid(variable ~ Scenario, scales = "free_y") +
  theme(legend.position = "bottom")

# reverse coding the coherence dimension
ggplot(data = aggregate_data_screen %>% mutate(Coherence = 7 - Coherence) %>% 
         gather(Escape:Compatibility, key = 'variable', value = 'Score'),
       aes(x = `Interpersonal Trust`, y = Score)) +
  geom_point(aes(color = `Level of Vitality`),  shape = 16, alpha = 0.4) +
  geom_smooth(aes(color = `Level of Vitality`), method = "lm") +
  facet_grid(variable ~ Scenario, scales = "free_y") +
  theme(legend.position = "bottom")


ggsave("fig7.svg", width = 20, height = 15, units = "cm")


# for different vitality level and PRS subscale: facet 2 by 4 
ggplot(data = aggregate_data_screen %>% gather(Escape:Compatibility, key = 'variable', value = 'Score'),
       aes(x = `Interpersonal Trust`, y = Score)) +
  geom_point(aes(color = `Scenario`),  alpha = 0.4) +
  geom_smooth(aes(color = `Scenario`), method = "lm") +
  facet_grid(variable ~ `Level of Vitality`, scales = "free_y")

# fixation is significant
ggplot(aggregate_data_screen %>% 
         gather(Average_duration_of_whole_fixations, Average_amplitude_of_saccades, key = 'variable', value = 'Score') %>% 
         mutate(variable = fct_relabel(variable, ~ str_replace_all(., '_', ' ') %>% 
                                         str_remove(., ' whole') %>% paste(c("[°]", "[ms]")))), 
       aes(x = `Interpersonal Trust`, y = Score)) +
  geom_point(aes(color = `Level of Vitality`), shape = 16, alpha = 0.4) +
  geom_smooth(aes(color = `Level of Vitality`), method = "lm") +
  scale_y_continuous(name = "Value") +
  facet_grid(variable ~ Scenario, scales = "free_y") +
  theme(legend.position = "bottom")
ggsave("fig6.svg", width = 16*1.3, height = 12*1.3, units = "cm")

# gender interaction is significant
ggplot(aggregate_data_summary, aes(x = Gender, y = mean, group = `Level of Vitality`)) +
  geom_bar(aes(fill = `Level of Vitality`), stat = "identity", 
           position = "dodge", alpha = 0.60) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), stat = "identity",
                position = position_dodge(0.9), width = .35) +
  scale_y_continuous(name = "Restoration")+
  theme(legend.position = "bottom")

ggsave("fig8.svg", width = 12, height = 12, units = "cm")



# vital_attention_ratio is significant
ggplot(aggregate_data_screen, aes(x = `Interpersonal Trust`, y = Vitality_attention_standardized)) +
  geom_point(aes(color = `Level of Vitality`),  shape = 16, alpha = 0.4) +
  geom_smooth(aes(color = `Level of Vitality`), method = "lm") +
  scale_y_continuous(name = "Standardized attention on ubban human activities") +
  # facet_wrap( ~ Scenario, scales = "free_y", nrow = 2) +
  facet_grid( ~ Scenario, scales = "free_y") +
  theme(legend.position = "bottom")
ggsave("fig9.svg", width = 20, height = 10, units = "cm")
