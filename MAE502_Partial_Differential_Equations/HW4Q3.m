numcyc=1000;%number of cycles
num=100;%number of grid points
st=1/num;%size of step
epsi=0.05;%equation parameter
xs=[0:st:1];%x series
ys=zeros(num+1,1);%y series
ysper=zeros(num+1,1)%y series, perturbation
for i=1:(num+1)
    ysper(i,1)=tanh(0.5*(xs(1,i)-1)/epsi+atanh(-2))-xs(1,i)+1;
end
ys(1,1)=0;%boundary value
ys(num+1,1)=-2;%boundary value
for k=1:numcyc
for i=2:num
    ys(i,1)=((i-1)*st^3/epsi-ys(i+1,1)-ys(i-1,1))/(0.5*st/epsi*(ys(i+1,1)-ys(i-1,1))-2);
end
end
plot(xs,ys,xs,ysper),xlabel('x'),ylabel('y'),title('Comparison:numerical and perturbation'),grid on,legend('Numerical','Perturbation')