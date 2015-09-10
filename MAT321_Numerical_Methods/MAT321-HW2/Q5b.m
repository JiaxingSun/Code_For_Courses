x = linspace(-1,1,5);
y = exp(x);
[coef, ma] = interpolate(x,y);
xseries = linspace(-1,1,50);
yseries = exp(xseries);
numys = zeros(1,length(xseries));
for i = 1:length(x)
    numys = numys+coef(i)*xseries.^(i-1);
end
[max_err,max_i] = max(abs(numys-yseries))
plot(xseries,yseries,'bo');
hold on;
grid on;
plot(xseries,numys);
ylabel('exp(x)&polynomial approximation');
xlabel('x');
