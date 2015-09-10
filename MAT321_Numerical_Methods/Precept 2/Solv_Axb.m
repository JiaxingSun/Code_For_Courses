m=4;
a = rand(m,m);
A = a'*a;
b = rand(m,1);
x = (LL_decomp(A))'\(LL_decomp(A)\b);