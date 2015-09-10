function  A  = matgen( n,condno )
% Generate an n by n random matrix with 2-norm condition number condno.
% Required: condno >=1
    
[U,R] = qr(randn(n,n));
[V,R] = qr(randn(n,n));

sigma = condno.^linspace(-1,0,n);
Sigma = diag(sigma);

A = U*Sigma*V';
end

