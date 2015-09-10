imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
numtrain=length(imagespre);%this is the size of training data
dim=785;%this is the dimension of each data vector
numcyc=100;%this is the number of cycles
lam=1;%this is the penalization factor
minim=1400;
gam=2./((1:numcyc)+100);%define the gamma as proportion in conditional gradient descent
beta=zeros(dim,numcyc);%define constrained beta
betapre=zeros(dim,numcyc);%define the unconstrained beta after each pivot
rad=15;%define the radius of the constraining euclidean ball
beta(:,1)=-0.1;%this is the initial value of constrained beta
smo=0.25*sum(sum(images.^2))+1570*lam%calculate the smooth factor
%%%%%%%%%%%%%%%%%%initialize the labels into binomial label%%%%%%%%%%%%%%%%
labelsbi=zeros(numtrain,10);
for j=1:10
    labelsbi(:,j)=2*(labels(:,1)==(j-1))-1;
end
%%%%%%%%%start the conditional gradient descent minimization%%%%%%%%%%%%%%%
for i=2:numcyc
    part1=images*((-1)*labelsbi(:,1).*(exp((-1)*labelsbi(:,1).*(images'*beta(:,(i-1))))./(1+exp((-1)*labelsbi(:,1).*(images'*beta(:,(i-1)))))));
    part2=2*lam*beta(:,(i-1));
    betapre(:,i)=(-1)*rad*(part1+part2)/sqrt((part1+part2)'*(part1+part2));%this returns the unconstrained beta after each pivot
    beta(:,i)=gam(1,(i-1))*betapre(:,i)+(1-gam(1,(i-1)))*beta(:,(i-1));
end
%%%%%%%%%convergence characteristics---conditional gradient descent%%%%%%%%
loglossl2=zeros(1,numcyc);%define the vector of objective function, dim=number of cycles, to see its evolution
for k=1:numcyc
    loglossl2(1,k)=sum(log(exp(((-1)*labelsbi(:,1).*(beta(:,k)'*images)'))+1))+lam*beta(:,k)'*beta(:,k);
end%calculate the vector of objective function
plot(loglossl2),title('Evolution of loss function','FontSize',15),xlabel('number of iterates','FontSize',10);
%%%%%%%%%%%%%%%%%%%compare with the theoretical upper bound%%%%%%%%%%%%%%%%
compl=loglossl2-minim;
compr=2*smo*4*(rad^2)./((1:numcyc)+1);
plot(1:numcyc,compl,1:numcyc,compr),legend('Empirical','Theoretical'),title('Comparison of convergence rate','FontSize',15),xlabel('number of iterates','FontSize',10);