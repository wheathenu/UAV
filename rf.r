setwd("")


otu <- read.delim('UAV-DRR.txt', row.names = 1)

plant <- read.delim('TKW-DRR.txt', row.names = 1)

otu <- data.frame(t(otu))
otu <- otu[rownames(plant), ]
otu <- cbind(otu, plant)


set.seed(123)
train <- sample(nrow(otu), nrow(otu)*0.8)
otu_train <- otu[train, ]
otu_test <- otu[-train, ]

library(randomForest)

set.seed(123)
otu_train.forest <- randomForest(TKW~., data = otu_train, importance = TRUE, ntree=500)
otu_train.forest


plant_predict <- predict(otu_train.forest, otu_train)
write.table(plant_predict, 'plant_predict.txt', sep = '\t', col.names = NA, quote = FALSE)
plot(otu_train$TKW, plant_predict, main = 'Training set', 
    xlab = 'TKW', ylab = 'Predicted TKW')

plant_predict <- predict(otu_train.forest, otu_test)
write.table(plant_predict, 'otu_train.forest_predict.txt', sep = '\t', col.names = NA, quote = FALSE)
plot(otu_test$TKW, plant_predict, main = 'Test set',
    xlab = 'TKW', ylab = 'Predicted TKW')


plant_predict2 <- predict(otu_train.forest, otu)

plot(otu$TKW, plant_predict2, main = 'All',
     xlab = 'TKW', ylab = 'Predicted TKW')

importance_otu <- otu_train.forest$importance
head(importance_otu)


importance_otu <- data.frame(importance(otu_train.forest), check.names = FALSE)
head(importance_otu)
write.table(importance_otu, 'importance_otu-all.txt', sep = '\t', col.names = NA, quote = FALSE)

varImpPlot(otu_train.forest, n.var = min(30, nrow(otu_train.forest$importance)), 
    main = 'Top 30 - variable importance')


importance_otu <- importance_otu[order(importance_otu$IncNodePurity, decreasing = TRUE), ]
head(importance_otu)

write.table(importance_otu, 'importance_otu.txt', sep = '\t', col.names = NA, quote = FALSE)


set.seed(123)
otu_train.cv <- replicate(5, rfcv(otu_train[-ncol(otu_train)], otu_train$TKW, cv.fold = 10, step = 1.5), simplify = FALSE)
otu_train.cv

otu_train.cv <- data.frame(sapply(otu_train.cv, '[[', 'error.cv'))
otu_train.cv$otus <- rownames(otu_train.cv)
otu_train.cv <- reshape2::melt(otu_train.cv, id = 'otus')
otu_train.cv$otus <- as.numeric(as.character(otu_train.cv$otus))

otu_train.cv.mean <- aggregate(otu_train.cv$value, by = list(otu_train.cv$otus), FUN = mean)
head(otu_train.cv.mean, 10)
write.table(otu_train.cv.mean, 'SNP_train.cv.mean.txt', sep = '\t', col.names = NA, quote = FALSE)

library(ggplot2)

ggplot(otu_train.cv.mean, aes(Group.1, x)) +
geom_line() +
theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent')) +  
labs(title = '',x = 'trait number', y = 'Cross-validation error')


importance_otu <- importance_otu[order(importance_otu$IncNodePurity, decreasing = TRUE), ]


importance_otu.select <- importance_otu[1:17, ]
importance_otu.select


write.table(importance_otu.select, 'importance_otu.select.txt', sep = '\t', col.names = NA, quote = FALSE)


otu_id.select <- rownames(importance_otu.select)
otu.select <- otu[ ,c(otu_id.select, 'TKW')]
otu.select <- reshape2::melt(otu.select, id = 'TKW')

ggplot(otu.select, aes(x = TKW, y = value)) +
geom_point() +
geom_smooth() +
facet_wrap(~variable, ncol = 3, scale = 'free_y') +
labs(title = '',x = 'TKW', y = 'Relative abundance')


otu.select <- otu[ ,c(otu_id.select, 'TKW')]


set.seed(123)
train <- sample(nrow(otu.select), nrow(otu.select)*0.8)
otu_train.select <- otu.select[train, ]
otu_test.select <- otu.select[-train, ]


set.seed(123)
otu_train.select.forest <- randomForest(TKW~., data = otu_train.select, importance = TRUE, ntree=500)
otu_train.select.forest


plant_predict <- predict(otu_train.select.forest, otu_train.select)

plot(otu_train.select$TKW, plant_predict, main = 'Training set', 
    xlab = 'TKW', ylab = 'Predicted TKW')



plant_predict <- predict(otu_train.select.forest, otu_test.select)

plot(otu_test.select$TKW, plant_predict, main = 'Test set',
    xlab = 'TKW', ylab = 'Predicted TKW')
abline(1, 1)


plant_predict4 <- predict(otu_train.select.forest, otu.select)

plot(otu.select$TKW, plant_predict4, main = 'All',
     xlab = 'TKW', ylab = 'Predicted TKW')

otu_id.select <- rownames(importance_otu.select)
otu.select <- otu[, c(otu_id.select, 'TKW')]


cor_matrix <- cor(otu.select)


cor_with_TKW <- cor_matrix['TKW', otu_id.select]
cor_with_TKW

write.table(cor_with_TKW, 'cor_with_TKW.txt', sep = '\t', col.names = NA, quote = FALSE)
