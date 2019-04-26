library(tidyverse)
head(mtcars, 20)

mtcarsDbyC <- mtcars %>%
  group_by(cyl) %>%
  summarise(meandisp = mean(disp))

head(mtcarsDbyC, 20)

mtcarsDHbyC <- mtcars %>%
  group_by(cyl) %>%
  summarise(meandisp = mean(disp),
            meanhp = mean(hp))
head(mtcarsDHbyC, 20)

mtcarsDHbyCG <- mtcars %>%
  group_by(cyl, gear) %>%
  summarise(meandisp = mean(disp),
            meanhp = mean(hp))
head(mtcarsDHbyCG, 20)

mtcarsDHbyCGquant <- mtcars %>%
  group_by(cyl, gear) %>%
  summarise(meandisp = mean(disp),
            meanhp = mean(hp),
            disp75 = quantile(disp, probs=0.75),
            hp75 = quantile(disp, probs=0.7),
            dispSD = sd(disp),
            countsamp = n())
head(mtcarsDHbyCGquant, 20)

mtcarsDHbyCGquant$ratiodisp <- mtcarsDHbyCGquant$meandisp/mtcarsDHbyCGquant$disp75
mtcarsDHbyCGquant$CVdisp <- mtcarsDHbyCGquant$dispSD/mtcarsDHbyCGquant$meandisp

head(mtcarsDHbyCGquant, 20)

boxplot(CVdisp~cyl, data=mtcarsDHbyCGquant)




mtcarsDdifbyCG <- mtcars %>%
  group_by(cyl, gear) %>%
  mutate(meandisp = mean(disp),
         dispdif = disp/meandisp,
         mediandispdif = median(dispdif))

head(mtcarsDdifbyCG, 20)

boxplot(dispdif~cyl, data=mtcarsDdifbyCG)

ggplot(mtcarsDdifbyCG, aes(factor(cyl),y=dispdif)) +
  geom_point() +
  geom_point(data=mtcarsDdifbyCG, aes(x=factor(cyl),y=mediandispdif, shape=factor(gear)), col="red", size=3)
