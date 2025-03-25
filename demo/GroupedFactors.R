library(likert)
data(sasr)

head(sasr)

factor.structure <- list(
	'Metacognition' = c(5, 6, 13, 14, 21, 22, 28, 29, 34, 40, 45, 50, 52, 55, 62),
	'Self-Regulation' = c(2, 7, 10, 15, 18, 25, 30, 33, 35, 42, 47, 57, 61),
	'Personal Relevance & Control' = c(8, 16, 23, 31, 37, 41, 46, 51, 56, 63),
	'Intrinsic Motivation' = c(1, 9, 17, 24, 32, 38, 43, 49, 54, 60),
	'Self-Efficacy' = c(4, 12, 20, 27, 36, 39, 44, 53, 58),
	'Extrinsic Motivation' = c(3, 11, 19, 26, 48, 59)
)

tmp <- unlist(factor.structure, use.names = FALSE)
factor.names <- character(length(tmp))
start <- 1
for(i in seq_along(factor.structure)) {
	factor.names[start:(start+length(factor.structure[[i]]))] <- names(factor.structure)[i]
	start <- start + length(factor.structure[[i]])
}
sasr.factors <- factor.names[tmp]

sasr.item.names <- c(
	"I prefer tasks that are more challenging.",
	"I hold myself to the highest learning standards.",
	"It is very important for others to see me as capable.",
	"I know I can learn even the most difficult material.",
	"When I cannot solve a problem, I change my approach to it.",
	"I use most available study aids (e.g., outlines, glossary, etc.).",
	"I know I am able to accomplish most tasks assigned to me.",
	"I place the highest value on my education.",
	"I find learning in college to be very enjoyable.",
	"Once I start a task, I usually find it hard to finish.",
	"I often like to let others see just how smart I am.",
	"I know that I will do well on most of my quizzes or tests.",
	"I review the effectiveness of my approach once I finish a task.",
	"I keep track of my long-term goal progress after each task.",
	"I complete assigned tasks even when they are uninteresting.",
	"I believe what I learn in college has real-world relevancy.",
	"I like to completely master the tasks I am learning.",
	"I often make excuses for not doing my school work.",
	"I act as if a task is easy even when it is not.",
	"I usually do very well on most of my learning tasks.",
	"I know when, how, and why to use a specific learning strategy.",
	"I set personal learning goals before I even begin studying.",
	"I achieve most of the learning goals I set for myself.",
	"I can connect most of what I learn in college to my own life.",
	"I try to study in places where I can easily concentrate.",
	"I enjoy knowing more than others do.",
	"I usually put off studying because I worry about not doing well.",
	"I keep track of how well I do or do not understand material.",
	"I know the studying and learning resources available to me.",
	"I spend too much time socializing when I should be studying.",
	"I am well aware of what my instructors/professors expect of me.",
	"I can think of different ways to make a boring task interesting.",
	"I usually get my studying done first before playing.",
	"I often summarize to myself the things I am learning.",
	"I try very hard to attend all of my classes.",
	"I often worry about not doing as well as others do in college.",
	"I can easily identify the main ideas when learning or studying.",
	"I study because I enjoy learning, not just to get a good grade.",
	"I am not easily distracted from what I am learning or studying.",
	"I often test myself to see how well I understand something.",
	"I am quite sure I am going to succeed in college.",
	"I remind myself how important studying is when I get tired of it.",
	"I usually want to learn more than just what is required.",
	"I am often afraid of looking dumb when I ask a question in class.",
	"I like to reconsider my own view when I hear a different one.",
	"I know I have much control over how much I can learn.",
	"I almost always complete my schoolwork on time.",
	"I am driven to know more than what others do.",
	"I often find learning and studying to be enjoyable.",
	"I approach problems by first considering all of my options.",
	"What I am learning in college will help me realize my life's goals.",
	"I reflect on how well I am managing my learning as it unfolds.",
	"I often cannot concentrate on tests because I get so nervous.",
	"I prefer to analyze the evidence before I accept another's view.",
	"I usually try different approaches rather than give up on a task.",
	"I easily connect what I am learning to what I already know.",
	"My time management skills allow me to get things done.",
	"I get pretty nervous even when I am prepared for a test.",
	"It is important that I do not appear dumb in front of others.",
	"I like to consider several different perspectives on a topic.",
	"I set benchmarks for stopping studying before I start.",
	"I have a repertoire of different test-taking strategies.",
	"If the help is available, I will usually use it when I need to."
)

for(i in seq_along(sasr)) {
	sasr[,i] <- factor(as.character(sasr[,i]), levels=c('Strongly Disagree',
					'Disagree','Neither','Agree','Strongly Agree'), ordered=TRUE)
}
str(sasr)

names(sasr) <- sasr.item.names

l.sasr1 <- likert(items=sasr)
plot(l.sasr1)

l.sasr2 <- likert(items=sasr, factors=sasr.factors)
plots <- list()
for(i in unique(l.sasr2$factors)) {
	l2 <- l.sasr2
	l2$results <- l2$results[l2$results$Group == i,]
	l2$items <- l2$items[,names(l2$items) %in% l2$results$Item]
	l2 <- likert(l2$items)
	plots[[i]] <- plot(l2, legend.position = 'none', wrap=150) + 
		ggtitle(paste('', i))
}
do.call(grid.arrange, plots)
