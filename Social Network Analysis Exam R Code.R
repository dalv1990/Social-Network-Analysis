setwd('C:\\Users\\Halyna\\Google Drive\\KU Leuven\\Network Analysis\\Week 1')
library(sna)

#load data
affective_w1 <- as.matrix(read.csv("RECENS_data/3200_affective_w1.csv",
                                   header=TRUE, row.names=1, sep=","))
affective_w2 <- as.matrix(read.csv("RECENS_data/3200_affective_w2.csv",
                                   header=TRUE, row.names=1, sep=","))
trust_w1 <- as.matrix(read.csv("RECENS_data/3200_trust_w1.csv",
                               header=TRUE, row.names=1, sep=","))
trust_w2 <- as.matrix(read.csv("RECENS_data/3200_trust_w2.csv",
                               header=TRUE, row.names=1, sep=","))
sex <- as.matrix(read.csv("RECENS_data/3200_sex.csv",
                          header=TRUE, row.names=1, sep=","))
drink <- as.matrix(read.csv("RECENS_data/3200_drink.csv",
                            header=TRUE, row.names=1, sep=","))
affective_w1 # -2 to 2
tail(affective_w2)
sex
drink
trust_w1 # binary
trust_w2

#what is the number of ties of different affective types?
enemy_w1 <- length(which(affective_w1 == -2))
enemy_w2 <- length(which(affective_w2 == -2))
dislike_w1 <- length(which(affective_w1 == -1))
dislike_w2 <- length(which(affective_w2 == -1))
neutral_w1 <- length(which(affective_w1 == 0))
neutral_w2 <- length(which(affective_w2 == 0))
like_w1 <- length(which(affective_w1 == 1))
like_w2 <- length(which(affective_w2 == 1))
friend_w1 <- length(which(affective_w1 == 2))
friend_w2 <- length(which(affective_w2 == 2))
enemy_w1
enemy_w2
dislike_w1
dislike_w2
neutral_w1
neutral_w2
like_w1
like_w2
friend_w1
friend_w2 #negatives increase, positives and neutrals decrease

#descriptives
class_size <- nrow(affective_w1)
class_size

gender_vector<- c("boys", "girls", sum(sex==1), sum(sex==2))
gender_comp <- matrix(gender_vector, 2,2, byrow=TRUE)
gender_comp

#visualization
library(igraph)
graph.1 <- graph.adjacency(affective_w1)
graph.2 <- graph.adjacency(affective_w2)
graph.12 <- graph.adjacency(affective_w1 + affective_w2)
myLayout <- layout.fruchterman.reingold(graph.12)

#reformat drinking
drinking.w1<- drink[,1]
drinking.w2<- drink[,2]
head(drink)
head(drinking.w1)
head(drinking.w2)

par(mfrow=c(1,2))
plot(graph.1,
     vertex.color = ifelse(drinking.w1 == 1, "white", ifelse(drinking.w1 == 2, "lightblue", "blue")),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = ifelse(affective_w1== -2, "red", ifelse(affective_w1==-1, "pink",
                                                          ifelse(affective_w1==0, "white", ifelse(affective_w1==1, "lightgreen", "darkgreen")))),
     edge.width = 0.2,
     edge.arrow.size = 0.05,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Affective network - wave 1")
plot(graph.2,
     vertex.color = ifelse(drinking.w2 == 1, "white", ifelse(drinking.w2 == 2, "lightblue", "blue")),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = ifelse(affective_w2== -2, "red", ifelse(affective_w2==-1, "pink",
                                                          ifelse(affective_w2==0, "white", ifelse(affective_w2==1, "lightgreen", "darkgreen")))),
     edge.width = 0.2,
     edge.arrow.size = 0.05,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Affective network - wave 2")
legend('topleft',legend=c("Boys", "Girls", "Low drinking", "Moderate drinking", "High drinking",
                          "Enemy", "Dislike", "Like","Friend"),cex=1, pch=c(15,16,1,16,16,NA,NA,NA,NA), 
       col=c('black', 'black', "black", "lightblue", "blue", "red", "pink", "lightgreen", "darkgreen"),
       lty=c(NA,NA,NA,NA,NA,1,1,1,1), lwd=c(NA,NA,NA,NA,NA,2,2,2,2))

par(mfrow=c(1,1))


#missing data
affective_miss_w1 <- sum(is.na(affective_w1))
affective_miss_w1 #diagonal which is okay
affective_miss_w2 <- sum(is.na(affective_w2))
affective_miss_w2 #students 1,8 & 21 missing plus some random
drink_miss <- sum(is.na(drink))
drink_miss
sex_miss <- sum(is.na(sex))
sex_miss
trust_miss_w1 <- sum(is.na(trust_w1))
trust_miss_w1 #just diagonal
trust_miss_w2 <- sum(is.na(trust_w2))
trust_miss_w2 #students 1,8, & 21, no random

#subtract students 1,8, and 21
affective_w2_full <- affective_w2[-c(1,8,21),]
affective_w2_full
affective_w2_full_miss <- sum(is.na(affective_w2_full))
affective_w2_full_miss

trust_w2_full <- trust_w2[-c(1,8,21),]
trust_w2_full
trust_w2_full_miss <- sum(is.na(trust_w2_full))
trust_w2_full_miss #just diagonal NAs

#check proportion of missing ties
affective_miss_w1 <- affective_miss_w1 / ( nrow(affective_w1) * (ncol(affective_w1) - 1) )
affective_miss_w1
affective_miss_w2 <- affective_miss_w2 / ( nrow(affective_w2) * (ncol(affective_w2) - 1) )
affective_miss_w2
trust_miss_w1<- trust_miss_w1 / ( nrow(trust_w1) * (ncol(trust_w1) - 1))
trust_miss_w1
trust_miss_w2<- trust_miss_w2 / ( nrow(trust_w2) * (ncol(trust_w2) - 1))
trust_miss_w2

#FRIENDSHIP ONLY
#affective => friendship
friendship_w1 <- affective_w1
friendship_w1[friendship_w1==-2] <- 0 
friendship_w1[friendship_w1==-1] <- 0 
friendship_w1[friendship_w1==0] <- 0 
friendship_w1[friendship_w1==1] <- 0 
friendship_w1[friendship_w1==2] <- 1
table(friendship_w1)

friendship_w2 <- affective_w2
friendship_w2[friendship_w2==-2] <- 0 
friendship_w2[friendship_w2==-1] <- 0 
friendship_w2[friendship_w2==0] <- 0 
friendship_w2[friendship_w2==1] <- 0 
friendship_w2[friendship_w2==2] <- 1
table(friendship_w2)

#plots
library(igraph)
graph.3 <- graph.adjacency(friendship_w1)
graph.4 <- graph.adjacency(friendship_w2)
graph.34 <- graph.adjacency(friendship_w1 + friendship_w2)
myLayout2 <- layout.fruchterman.reingold(graph.34)

par(mfrow=c(1,2))
plot(graph.3,
     vertex.color = ifelse(drinking.w1 == 1, "white", ifelse(drinking.w1 == 2, "lightblue", "blue")),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 0.2,
     edge.arrow.size = 0.05,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship network - wave 1")
plot(graph.4,
     vertex.color = ifelse(drinking.w2 == 1, "white", ifelse(drinking.w2 == 2, "lightblue", "blue")),
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     edge.color = "black",
     edge.width = 0.2,
     edge.arrow.size = 0.05,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Friendship network - wave 2")
legend('topleft',legend=c("Boys", "Girls", "Low drinking", "Moderate drinking", "High drinking",
                          "Friend"),cex=1, pch=c(15,16,1,16,16,NA), 
       col=c('black', 'black', "black", "lightblue", "blue", "black"),
       lty=c(NA,NA,NA,NA,NA,1), lwd=c(NA,NA,NA,NA,NA,2))
par(mfrow=c(1,1))

#ties
table(friendship_w1)
table(friendship_w2)
#reciprocity
friend_rec_w1 <- grecip(friendship_w1, measure="dyadic.nonnull")
friend_rec_w2 <- grecip(friendship_w2, measure="dyadic.nonnull")
friend_rec_w1
friend_rec_w2
#density
friend_dens_w1 <- gden(friendship_w1)
friend_dens_w2 <- gden(friendship_w2)
friend_dens_w1
friend_dens_w2
#in- out- degree histograms
detach("package:igraph")
friend_ind_w1 <- degree(friendship_w1, cmode = "indegree")
friend_outd_w1 <- degree(friendship_w1, cmode = "outdegree")
friend_ind_w2 <- degree(friendship_w2, cmode = "indegree") 
friend_outd_w2 <- degree(friendship_w2, cmode = "outdegree")
friend_ind_w1
friend_outd_w1
friend_ind_w2
friend_outd_w2
#12, 22, 13 18
hist(friend_ind_w1, xlim=c(0,14), ylim=c(0,20), breaks=13, main ="Histogram of indegrees", xlab="Indegree distribution", col="blue")
hist(friend_outd_w1, xlim=c(0,22), ylim=c(0,20), breaks=21, main="Histogram of outdegrees", xlab="Outdegree distribution", col="red")
hist(friend_ind_w2, xlim=c(0,14), ylim=c(0,20), breaks=13, main ="Histogram of indegrees", xlab="Indegree distribution", col="blue")
hist(friend_outd_w2, xlim=c(0,22), ylim=c(0,20), breaks=21, main="Histogram of outdegrees", xlab="Outdegree distribution", col="red")

#assortativity
library(igraph)
assortativity.nominal(graph.3, sex)
assortativity.nominal(graph.4, sex)
drinking.w1[is.na(drinking.w1)] <- median(drinking.w1, na.rm=TRUE)
drinking.w2[is.na(drinking.w2)] <- median(drinking.w2, na.rm=TRUE)
assortativity(graph.3, drinking.w1)
assortativity(graph.4, drinking.w2)
assortativity(graph.3, friend_ind_w1)
assortativity(graph.3, friend_outd_w1)
assortativity(graph.4, friend_ind_w2)
assortativity(graph.4, friend_outd_w2)

assortativity.degree(graph.3, directed=TRUE)
assortativity.degree(graph.4, directed=TRUE)

#friendship selection tables
gg.1 <- friendship_w1[sex==2, sex==2]
gb.1 <- friendship_w1[sex==2, sex==1]
bb.1 <- friendship_w1[sex==1, sex==1]
bg.1 <- friendship_w1[sex==1, sex==2]

friend.selection <- matrix(NA, 2, 2)
rownames(friend.selection) <- c("girl", "boy")
colnames(friend.selection) <- c("girl", "boy")
detach(package:igraph)
library(sna)
friend.selection[1,1] <- gden(gg.1, diag=FALSE)
friend.selection[1,2] <- gden(gb.1, diag=TRUE)
friend.selection[2,2] <- gden(bb.1, diag=FALSE)
friend.selection[2,1] <- gden(bg.1, diag=TRUE)

(friend.selection.norm <- friend.selection / gden(friendship_w1))
friend.selection[1,1]/friend.selection[1,2]
friend.selection[2,2]/friend.selection[2,1]

gg.2 <- friendship_w2[sex==2, sex==2]
gb.2 <- friendship_w2[sex==2, sex==1]
bb.2 <- friendship_w2[sex==1, sex==1]
bg.2 <- friendship_w2[sex==1, sex==2]

friend.selection2 <- matrix(NA, 2, 2)
rownames(friend.selection2) <- c("girl", "boy")
colnames(friend.selection2) <- c("girl", "boy")
detach(package:igraph)
library(sna)
friend.selection2[1,1] <- gden(gg.2, diag=FALSE)
friend.selection2[1,2] <- gden(gb.2, diag=TRUE)
friend.selection2[2,2] <- gden(bb.2, diag=FALSE)
friend.selection2[2,1] <- gden(bg.2, diag=TRUE)

(friend.selection.norm2 <- friend.selection2 / gden(friendship_w2))
friend.selection2[1,1]/friend.selection2[1,2]
friend.selection2[2,2]/friend.selection2[2,1]

#comparing the two wave networks
(hamming <- hdist(friendship_w1, friendship_w2))
(hamming.prop <- hamming/nties(friendship_w1))
(matching <- 1 - hamming.prop)
#shows a stable network, but
#because most ties are absent and hamming sensitive to that
#Jaccard index - disregards absent ties
A <- sum((friendship_w1 * friendship_w2)==1, na.rm=TRUE) 
BplusC <- sum((friendship_w1 + friendship_w2)==1, na.rm=TRUE) 
(jaccard <- A/(A+BplusC))

#dyads and triads
dyad.count <- dyad.census(friendship_w1)
triad.count <- triad.census(friendship_w1)
dyad.count2 <- dyad.census(friendship_w2)
triad.count2 <- triad.census(friendship_w2)
dyad.count
triad.count
dyad.count2
triad.count2
barplot(dyad.count, col="aliceblue", border=TRUE, ylim = c(0,400))
barplot(dyad.count2, add=TRUE, col="black", density=20, border = TRUE)
barplot(triad.count, col="aliceblue", border=TRUE, ylim = c(0,2500), 
        names.arg=c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", "030T",
                    "030C", "201", "120D", "120U","120C", "210", "300"))
barplot(triad.count2, add=TRUE, col='black', density=20)

#random graphs
#testing dyad/triad census against random graphs
net.size <- nrow(friendship_w1)
net.dens <- gden(friendship_w1)
random.nets <- rgraph(net.size, 200, net.dens)
dim(random.nets)
random.dens <- gden(random.nets)
hist(random.dens)
mean(random.dens)
net.dens #super similar
random.triad <- triad.census(random.nets)
random.dyad <- dyad.census(random.nets)
library(vioplot)
vioplot(random.triad[,3],random.triad[,4],random.triad[,5],  
        random.triad[,6],random.triad[,7],random.triad[,8],random.triad[,9],random.triad[,10], 
        random.triad[,11], random.triad[,12], random.triad[,13], random.triad[,14], random.triad[,15],  
        random.triad[,16],  
        names=colnames(random.triad)[3:16],  
        col="transparent",                
        ylim=c(0, 1000)) 
points(1:14, triad.count[c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)],
       col="red",                        
       pch=15)  #1 003 above, 2 012 below
vioplot(random.dyad[,1], random.dyad[,2], random.dyad[,3],
        names=colnames(random.dyad), col = "transparent",
        ylim=c(0,400))
points(1:3, dyad.count[c(1,2,3)],
       col="red",
       pch=15)

net.size2 <- nrow(friendship_w2)
net.dens2 <- gden(friendship_w2)
random.nets2 <- rgraph(net.size2, 200, net.dens2)
dim(random.nets)
random.dens2 <- gden(random.nets2)
hist(random.dens2)
mean(random.dens2)
net.dens2 #super similar
random.triad2 <- triad.census(random.nets2)
random.dyad2 <- dyad.census(random.nets2)
library(vioplot)
vioplot(random.triad2[,3],random.triad2[,4],random.triad2[,5],  
        random.triad2[,6],random.triad2[,7],random.triad2[,8],random.triad2[,9],random.triad2[,10], 
        random.triad2[,11], random.triad2[,12], random.triad2[,13], random.triad2[,14], random.triad2[,15],  
        random.triad2[,16],  
        names=colnames(random.triad2)[3:16],  
        col="transparent",                
        ylim=c(0, 1000)) 
points(1:14, triad.count2[c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)],
       col="red",                        
       pch=15)  #1 003 fits, 2 012 way below
vioplot(random.dyad2[,1], random.dyad2[,2], random.dyad2[,3],
        names=colnames(random.dyad2), col = "transparent",
        ylim=c(0,400))
points(1:3, dyad.count2[c(1,2,3)],
       col="red",
       pch=15)

#cluster detection
friend_w1 <- friendship_w1 + t(friendship_w1)
friend_w1[friend_w1==2] <- 1
detach(package:sna)
library(igraph)

friend1 <- graph.adjacency(friend_w1)
friend1 <- as.undirected(friend1)
cliques <- cliques(friend1)
length(cliques)
table(sapply(maximal.cliques(friend1), length))
cores <- graph.coreness(friend1)

detach(package:igraph)
library(sna)
gplot(friend_w1)
equiv.w1 <- equiv.clust(friend_w1, cluster.method="ward.D2", method="hamming")
plot(equiv.w1)
bm.w1 <- blockmodel(friend_w1, equiv.w1, k=5)
plot.sociomatrix(friend_w1, diaglab = FALSE)
plot.sociomatrix(bm.w1$blocked.data, diaglab=FALSE)
block.members <- bm.w1$block.membership[order(bm.w1$order.vector)]
gplot(friend_w1, vertex.col=block.members)

communities <- fastgreedy.community(friend1)
length(communities) 
sizes(communities)
membership(communities)

par(mfrow=c(2,2))
# the original friendship network
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 10,
     vertex.label = "",
     vertex.color="skyblue",
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     layout=myLayout,
     main="original network")
# the blockmodel (from the previous script)
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 10,
     vertex.label = "",
     vertex.color= block.members,
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     layout=myLayout,
     main="blockmodel")
# the k-cores
plot(friend1,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 10,
     vertex.label = "",
     vertex.color= cores,
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     layout=myLayout,
     main="k-cores")
# the fast-greedy communities
plot(communities, friend1,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 10,
     vertex.label = "",
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     layout=myLayout,
     main="fast'n'greedy")
par(mfrow=c(1,1))

friend_w2 <- friendship_w2 + t(friendship_w2)
friend_w2[friend_w1==2] <- 1
detach(package:sna)
library(igraph)

friend2 <- graph.adjacency(friend_w2)
friend2 <- as.undirected(friend2)
cliques2 <- cliques(friend2)
length(cliques2)
table(sapply(maximal.cliques2(friend2), length))
cores2 <- graph.coreness(friend2)

detach(package:igraph)
library(sna)
gplot(friend_w2)
equiv.w2 <- equiv.clust(friend_w2, cluster.method="ward.D2", method="hamming")
plot(equiv.w2)
bm.w2 <- blockmodel(friend_w2, equiv.w2, k=5)
plot.sociomatrix(friend_w2, diaglab = FALSE)
plot.sociomatrix(bm.w2$blocked.data, diaglab=FALSE)
block.members2 <- bm.w2$block.membership[order(bm.w2$order.vector)]
gplot(friend_w2, vertex.col=block.members2)
library(igraph)
communities2 <- fastgreedy.community(friend2)
length(communities2) 
sizes(communities2)
membership(communities2)

par(mfrow=c(2,2))
# the original friendship network
plot(friend2,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 10,
     vertex.label = "",
     vertex.color="skyblue",
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     layout=myLayout,
     main="original network")
# the blockmodel (from the previous script)
plot(friend2,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 10,
     vertex.label = "",
     vertex.color= block.members2,
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     layout=myLayout,
     main="blockmodel")
# the k-cores
plot(friend2,
     edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 10,
     vertex.label = "",
     vertex.color= cores2,
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     layout=myLayout,
     main="k-cores")
# the fast-greedy communities
plot(communities2, friend1,edge.color = "black",
     edge.width = 1.5,
     edge.arrow.size = 0.25,
     vertex.size = 10,
     vertex.label = "",
     layout=myLayout,
     vertex.shape = ifelse(sex == 2, "square", "circle"),
     main="fast'n'greedy")
par(mfrow=c(1,1))

#MRQAP
qap1 <- netlogit(friendship_w2, friendship_w1, nullhyp="qap", reps=100)
qap1

same.sex <- sex %*% t(sex)
same.sex[same.sex==2] <- 0
same.sex[same.sex==4] <- 1

qap2<- netlogit(friendship_w2, list(friendship_w1, same.sex), nullhyp='qap', reps=100)
qap2
summary(qap2)

#SIENA
library(RSiena)
library(igraph)

nActors <- dim(friendship_w1)[1]
friendship.dependent <- sienaDependent(array(c(friendship_w1, friendship_w2),
                                             dim=c(nActors, nActors, 2)))
gender<- unlist(read.csv("RECENS_data/3200_sex.csv",
                         header=TRUE, row.names=1, sep=","))
gender.coCovar <- coCovar(gender)
mySienaData <- sienaDataCreate(friendship.dependent,
                               gender.coCovar)
print01Report(mySienaData,
              modelname="friendship siena")

mySienaEffects <- getEffects(mySienaData)
mySienaEffects <- includeEffects(mySienaEffects, transTrip, cycle3)
mySienaEffects <- includeEffects(mySienaEffects, inPop)
mySienaEffects <- includeEffects(mySienaEffects, egoX, altX, sameX, interaction1="gender.coCovar")
mySienaEffects

mySienaAlgorithm <- sienaAlgorithmCreate(projname="friendship siena",
                                         MaxDegree=c(friendship.dependent=50))
result <- siena07(mySienaAlgorithm,
                  data=mySienaData,
                  effects=mySienaEffects)

result

siena.table(result, type="html", file="results.html")