imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=100;%this is the number of cycles
lam=10000;%this is the penalization factor
st=0.1;%this is the step size of the plain gradient descent
gam=2./((1:numcyc)+100);%define the gamma as proportion in conditional gradient descent
beta=zeros(dim,numcyc);%define constrained beta
betaun=zeros(dim,numcyc);%define unconstrained beta
betapre=zeros(dim,numcyc);%define the unconstrained beta after each pivot
rad=2;%define the radius of the constraining euclidean ball
beta(:,1)=-0.1;%this is the initial value of constrained beta
betaun(:,1)=beta(:,1);%this is the initial value of unconstrained beta
%%%%%%%%%%%%%%%%%%initialize the labels into binomial label%%%%%%%%%%%%%%%%
labelsbi=zeros(numtrain,10);
for j=1:10
    labelsbi(:,j)=2*(labels(:,1)==(j-1))-1;
end
%%%%%%%%%start the conditional gradient descent minimization%%%%%%%%%%%%%%%
for i=2:numcyc
    part1=(-1)*images*((labelsbi(:,1).*(images'*beta(:,(i-1)))<1).*labelsbi(:,1));
    part2=lam*beta(:,(i-1))/sqrt(beta(:,(i-1))'*beta(:,(i-1)));
    betapre(:,i)=(-1)*rad*(part1+part2)/sqrt((part1+part2)'*(part1+part2));%this returns the unconstrained beta after each pivot
    beta(:,i)=gam(1,(i-1))*betapre(:,i)+(1-gam(1,(i-1)))*beta(:,(i-1));
end
%%%%%%%%%%%%%%%%%%%%do the unconstrained minimization%%%%%%%%%%%%%%%%%%%%%%
for i=2:numcyc
    part1=images*((-1)*labelsbi(:,1).*(exp((-1)*labelsbi(:,1).*(images'*betaun(:,(i-1))))./(1+exp((-1)*labelsbi(:,1).*(images'*betaun(:,(i-1)))))));
    part2=lam*betaun(:,(i-1))/sqrt(betaun(:,(i-1))'*betaun(:,(i-1)));
    betaun(:,i)=betaun(:,(i-1))-st*(part1+part2);
end
plot(beta(:,(numcyc-4):(numcyc)));%plot the last 5 set of beta
plot(sqrt(sum(beta.^2)),'r'),xlabel('Number of Steps'),ylabel('l2 norm of beta'),title('Projected Gradient Descent');%plot the sumsquare of constrained coefficients
plot(sqrt(sum(betaun.^2))),xlabel('Number of Steps'),ylabel('l2 norm of beta'),title('Projected Gradient Descent');%also plot the sumsquare of unconstrained coefficients to make comparison
%%%%%%%%%%%%%%%%%%%%%%%%prediction and error rate%%%%%%%%%%%%%%%%%%%%%%%%%%
pr=zeros(numtrain,10);%define the vector for binomial prediction(1 or -1)
prob=zeros(numtrain,10);%this is the probability of being +1
erate=zeros(1,numcyc);%define the vector of error rate, dim=number of cycles
for k=1:numcyc
prob(:,1)=1./(exp(((-1)*ones(numtrain,1).*(beta(:,k)'*images)'))+1);
pr=2*(prob>0.5)-1;%this uses logic calculation to judge whether p>0.5
erate(1,k)=1-sum(pr(:,1)==labelsbi(:,1))/numtrain;
end%calculate the vector of error rate
%%%%%%%%%convergence characteristics---conditional gradient descent%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(beta(:,k)'*images)'))+1))+lam*sqrt(beta(:,k)'*beta(:,k));
end%calculate the vector of objective function
%%%%%%%%%%%convergence characteristics---plain gradient descent%%%%%%%%%%%%
loglossl2un=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2un(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(betaun(:,k)'*images)'))+1))+lam*sqrt(betaun(:,k)'*beta(:,k));
end%calculate the vector of objective function
%%%%%%%%%%%%%%%plot both objective function for comparison%%%%%%%%%%%%%%%%%
plot(1:numcyc,loglossl2),xlabel('Number of Steps'),ylabel('Objective Function'),title('Conditional Gradient Descent for SVM function');
