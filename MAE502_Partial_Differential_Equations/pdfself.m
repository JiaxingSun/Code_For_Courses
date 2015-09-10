v=0.3;
ss=0.2;%this is the space step
ts=0.1*ss^2;%this is the time step
cyc=400%this is the number of cycles
sp=30%this is the space span of the x-coordinates
num=sp/ss+1
u=zeros(num,cyc);
st=-5%this is the starting point 
for i=1:num
    u(i,1)=heaviside((i-1)*ss+st)-heaviside((i-1)*ss+st-pi)*sin((i-1)*ss+st);
end
for j=1:cyc
    for i=1:num
        if i==1
            u(i,j+1)=u(i,j)-u(i,j)*ts*(u(i+1,j)-u(i,j))/ss+v*ts*(2*u(i+1,j)-2*u(i,j))/(ss^2);
        elseif i==num
            u(i,j+1)=u(i,j)-u(i,j)*ts*(u(i,j)-u(i,j))/ss+v*ts*(u(i,j)+u(i-1,j)-2*u(i,j))/(ss^2);
        else
            u(i,j+1)=u(i,j)-u(i,j)*ts*(u(i+1,j)-u(i,j))/ss+v*ts*(u(i+1,j)+u(i-1,j)-2*u(i,j))/(ss^2);
        end
    end
end
xco=[st:ss:(st+sp)]%this is the x-coordinates
for p=1:(cyc/40)
    plot(xco,u(:,40*p-1)),xlabel('x'),ylabel('u(x)'),title('Viscous Burger Equation'),grid on
    hold on
end