m=4;
a = rand(m,m);
A = a'*a;%genertate a symetric positive definite matrix A
b = rand(m,1);%generate a vector b
x = (LL_decomp(A))'\(LL_decomp(A)\b);%solve the equation Ax=b