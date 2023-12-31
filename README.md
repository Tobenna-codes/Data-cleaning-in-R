# Data-cleaning-in-R

## FBI Crime Data
[Download xls File](https://github.com/Tobenna-codes/Data-cleaning-in-R/blob/main/FBI%20Crime%20Data.xls)

### Project Summary

FBI crime dataset was used to demonstrate data cleaning processes and creating data piplines.
Also contains use of ggplot to visualize our now cleaned data at the end.

### Data Cleaning Steps includes:
- Correcting inconsistent data
  - Data formats
  - Data representations
- Transforming data types
- Date and time parsing
- Deriving necessary columns
- Dropping unneeded columns

#### A bar plot of the mean of crimes from 1997-2016

![Crime Plot](https://github.com/Tobenna-codes/Data-cleaning-in-R/assets/135149511/d360541a-3151-45f6-9fca-d8c69dfa1ec6)

#### The R code for the bar plot
```R
(
ggplot(data = mean_crime/1000, aes(x = rownames(mean_crime), y = mean_value)) 
        + geom_bar(stat = 'identity', fill = 'darkorange')
        + xlab('Crimes') + ylab('Mean Value (K)') + labs(title = 'US Crime Reports', subtitle = '1997 - 2016') 
        + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
)
```
