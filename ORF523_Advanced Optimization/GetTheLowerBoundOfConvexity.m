imagespre=loadMNISTImages('train-images.idx3-ubyte');
labels=loadMNISTLabels('train-labels.idx1-ubyte');
images=[ones(1,length(imagespre));imagespre];
mat=zeros(795,795);
for i=1:60000
    mat=images(:,i)*images(:,i)';
end
samsize=1000;
convexity=zeros(1,samsize);
for i=1:samsize
    convexity(1,i)=norm(0.25*i/samsize*mat+2*eye(785));
end
plot((1:samsize)*0.25/samsize,convexity);
