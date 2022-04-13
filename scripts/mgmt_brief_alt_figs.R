# plot by lot
ggplot(data = visit_est,
       mapping = aes(x = Year, y = Visitors, group = Location, 
                     color = Location)) + 
  geom_point() + 
  geom_line() + 
  xlab('Year') + 
  ylab('Number of visitors') +
  scale_y_continuous(labels = scales::label_number(big.mark = ',')) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c(cal_palette('tidepool')[1],
                                cal_palette('tidepool')[2],
                                cal_palette('tidepool')[5]),
                     labels = c('Entrance station', 
                                'Tidepool Parking', 'Coast View Parking' )) +
  lltheme_light + 
  theme(panel.grid = element_blank(),
        text = element_text(size = 10, color = 'black'),
        axis.text.y = element_text(size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'))


# tile graph instead of "lollipop" visitatio plot
ungroup(holiday_test) %>%
  select(lot, holiday, pval_sig, mean_dif) %>%
  mutate(lot = if_else(lot == 'Lot 1', 'Tidepool', 'Coast View')) %>%
  ggplot(data = .,
         mapping = aes(x = fct_rev(lot), y = fct_rev(holiday),
                       fill = case_when(mean_dif >= 0 & pval_sig == TRUE ~ 'More Visitors',
                                        mean_dif <= 0 & pval_sig == TRUE ~ 'Fewer Visitors',
                                        TRUE ~ 'No significant change'))) + 
  geom_tile(color = 'black') +
  scale_fill_manual(values = c(cal_palette('tidepool')[1], cal_palette('tidepool')[2],'gray20')) +
  labs(fill = 'Change in Visitation') +
  xlab('Parking Lot') + 
  ylab('Holiday') +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        legend.position = 'top',
        legend.direction = 'vertical',
        text = element_text(size = 10, color = 'black'),
        axis.text.y = element_text(size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'))