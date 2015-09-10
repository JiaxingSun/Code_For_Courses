function [c,A] = interpolate(x,y)%A is the matrix, c is the coefficients
A = ones(1,length(x));
for i = 1:(length(x)-1)
A = [A;x.^i];
end
A = A';
c=A\y';
end
