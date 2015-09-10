imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=200;%this is the number of cycles
lam=1;%this is the penalization factor
beta=zeros(dim,numcyc);%define constrained beta
betaun=zeros(dim,numcyc);%define unconstrained beta
bt=1332150;%define the smoonthness parameter
alpha=2;%define the strongly convexness parameter
Q=bt/alpha;
rad=15;%define the radius of the constraining euclidean ball
betaun(:,1)=-0.1;%this is the initial value of unconstrained beta
%%%%%%%%%%%%%%%%%%initialize the labels into binomial label%%%%%%%%%%%%%%%%
labelsbi=zeros(numtrain,10);
for j=1:10
    labelsbi(:,j)=2*(labels(:,1)==(j-1))-1;
end
%%%%%%%%%%%%%%%%%%%%do the unconstrained minimization%%%%%%%%%%%%%%%%%%%%%%
for i=2:numcyc
    part1=images*((-1)*labelsbi(:,1).*(exp((-1)*labelsbi(:,1).*(images'*betaun(:,(i-1))))./(1+exp((-1)*labelsbi(:,1).*(images'*betaun(:,(i-1)))))));
    part2=lam*betaun(:,(i-1))/sqrt(betaun(:,(i-1))'*betaun(:,(i-1)));
    betaun(:,i)=betaun(:,(i-1))-(1/bt)*(part1+part2);
end
plot(betaun(:,(numcyc-4):(numcyc)),'r');%plot gradient descent as comparison
%%%%%%%%%%%%%%%%%%%%%%%convergence characteristics%%%%%%%%%%%%%%%%%%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(betaun(:,k)'*images)'))+1))+lam*sqrt(betaun(:,k)'*betaun(:,k));
end%calculate the vector of objective function
plot(loglossl2),title('Evolution of loss function','FontSize',15),xlabel('number of iterates','FontSize',10);
%%%%%%%%%%%%%%%%%%%compare with the theoretical upper bound%%%%%%%%%%%%%%%%
compl=loglossl2-minim;
compr=0.5*(bt+alpha)*sum(((beta(:,1)-lim).^2))*exp((1-(1:numcyc))./sqrt(Q));
plot(1:numcyc,compl,1:numcyc,compr),legend('Empirical','Theoretical'),title('Comparison of convergence rate','FontSize',15),xlabel('number of iterates','FontSize',10);